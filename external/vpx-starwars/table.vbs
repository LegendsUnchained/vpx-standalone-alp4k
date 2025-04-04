
 ' JPSalas Star Wars - based on Star Wars by Data East 1993
 ' VP9 version 1.0
' Mod permission granted by JPSalas
' Modders so far: Shoopity, Dark
' Shoopity's mod includes adding primitives for R2D2, the Death Star, Pop Bumpers, Death Star Door, and LED lights (and Jabba's Bounty)
' Also hooking up all the code to make all those primitives work
' Code to add functionality for the Shift lever; pushing the launch button would be like pushing the lever up, then down, then pushing launch (useful for opening the DS with 5 moons)
' Shoopity has not gone through and removed ALL non-essential/no-longer-applicable code or images
' Dark has provided an amazing mesh of R2 including a specular shell and internal lighting
 
 Option Explicit
 Randomize 

ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01550000", "DE.VBS", 3.26

if table1.showdt = false then ramp11.visible = 0:ramp12.visible = 0
 
 Dim bsTrough, bsL, bsR, dtBank, bsVuk, mDoor, mR2, x
 Dim R2head, R2Rhead, R2Yhead, R2RYhead, DR2head, DR2Rhead, DR2Yhead, DR2RYhead, R2D2Down, R2OldPos 
 
 Const UseSolenoids = 2
 Const UseLamps = 0
 Const UseGI = 0
 Const UseSync = 0
 Const HandleMech = 0
 
 ' Standard Sounds
 Const SSolenoidOn = "Solenoid"
 Const SSolenoidOff = ""
 Const SFlipperOn = "FlipperUp"
 Const SFlipperOff = "FlipperDown"
 Const SCoin = "Coin"
 
 'Select the ROM
 Const cGameName = "stwr_106" ' version 1.04
 'Const cGameName = "stwr_103" ' version 1.03
 'Const cGameName = "stwr_e12" ' version 1.02 England
 'Const cGameName = "stwr_102" ' version 1.02
 'Const cGameName = "stwr_g11" ' version 1.01 Germany
 
 '************
 ' Table init.
 '************
 
 Sub table1_Init
     vpmInit Me
     With Controller
         .GameName = cGameName
         If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
         .SplashInfoLine = "JPSalas Star Wars for VP9 v1.0" & vbNewLine & "based on Star Wars by Data East 1993"
         .HandleKeyboard = 0
         .ShowTitle = 0
         .ShowDMDOnly = 1
         .ShowFrame = 0
         .HandleMechanics = 0
         .Hidden = 0         
         .Games(cGameName).Settings.Value("rol") = 0 'not rotated
         .Games(cGameName).Settings.Value("sound") = 1         
         '.SetDisplayPosition 0, 0, GetPlayerHWnd 'uncomment this line if you can't see the dmd
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
 
     ' Nudging
     vpmNudge.TiltSwitch = 1
     vpmNudge.Sensitivity = 5
     vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, Bumper4, LeftSlingshot, RightSlingshot)
 
     ' Trough
     Set bsTrough = New cvpmBallStack
     With bsTrough
         .InitSw 10, 13, 12, 11, 0, 0, 0, 0
         .InitKick BallRelease, 120, 4
         .InitEntrySnd "Solenoid", "Solenoid"
         .InitExitSnd "ballrel", "Solenoid"
         .Balls = 3
     End With
 
     ' Left Eject Hole
     Set bsL = New cvpmBallStack
     With bsL
         .InitSw 0, 37, 0, 0, 0, 0, 0, 0
         .InitKick sw37, 161, 18
         '       .KickZ = 1.1344645 '65 Degrees
         .KickForceVar = 2
         .InitExitSnd "Solenoid", "Solenoid"
         .Balls = 0
         .KickBalls = 1
     End With
 
     ' Right Eject Hole
     Set bsR = New cvpmBallStack
     With bsR
         .InitSw 0, 33, 0, 0, 0, 0, 0, 0
         .InitKick sw33, 220, 19
         '       .KickZ = 1.1344645 '65 Degrees
         .KickForceVar = 2
         .InitExitSnd "Solenoid", "Solenoid"
         .Balls = 0
         .KickBalls = 1
     End With
 
     ' Vuk
     Set bsVuk = New cvpmBallStack
     With bsVuk
         bsVUK.InitSw 0, 36, 0, 0, 0, 0, 0, 0
         bsVUK.InitKick sw36, 220, 2
     End With
 
     ' Drop targets
     set dtbank = new cvpmdroptarget
     With dtbank
         .initdrop array(sw32, sw31, sw30), array(32, 31, 30)
         .initsnd "fx_droptarget", "fx_resetdrop"
     End With
 
     ' Main Timer init
     PinMAMETimer.Interval = PinMAMEInterval
     PinMAMETimer.Enabled = 1
 
 
     ' Init Bumper Rings and targets
     sw25a.IsDropped = 1:sw26a.IsDropped = 1:sw27a.IsDropped = 1
     sw28a.IsDropped = 1:sw29a.IsDropped = 1
     Plunger.PullBack:KickBack.PullBack
     'For each x in Bar:x.IsDropped = 1:next
     'sw40.IsDropped = 0
     Llogo2.IsDropped = 1:Rlogo2.IsDropped = 1
     LeftSLing.visible = 1:LeftSLing1.visible = 0:LeftSLing2.visible = 0
     RightSLing.visible = 1:RightSLing1.visible = 0:RightSLing2.visible = 0
 End Sub
 
 Sub table1_Paused:Controller.Pause = 1:End Sub
 Sub table1_unPaused:Controller.Pause = 0:End Sub
 
 '**********
 ' Keys
 '**********
 
 Sub table1_KeyDown(ByVal Keycode)
     If keycode = LeftTiltKey Then LeftNudge 80, 1.2, 20:PlaySound "nudge_left"
     If keycode = RightTiltKey Then RightNudge 280, 1.2, 20:PlaySound "nudge_right"
     If keycode = CenterTiltKey Then CenterNudge 0, 1.6, 25:PlaySound "nudge_forward"
     If keycode = 45 Then LeftNudge 80, 2, 30:PlaySound "nudge_left"
     If keycode = 52 Then RightNudge 280, 2, 30:PlaySound "nudge_right"
     If vpmKeyDown(keycode) Then Exit Sub
     If keycode = PlungerKey Then vpmTimer.PulseSw 51:Controller.Switch(50) = 1
	If keycode = 200 Then Controller.Switch(51) = 0
	If keycode = 208 Then Controller.Switch(51) = 1
 End Sub
 
 Sub table1_KeyUp(ByVal Keycode)
     If vpmKeyUp(keycode) Then Exit Sub
     If keycode = KeyRules Then RulesOff
     If keycode = PlungerKey Then Controller.Switch(50) = 0
 End Sub
 
 '*************************************
 '          Nudge System
 ' based on Noah's nudgetest table
 '*************************************
 
 Dim LeftNudgeEffect, RightNudgeEffect, NudgeEffect
 
 Sub LeftNudge(angle, strength, delay)
     vpmNudge.DoNudge angle, (strength * (delay-LeftNudgeEffect) / delay) + RightNudgeEffect / delay
     LeftNudgeEffect = delay
     RightNudgeEffect = 0
     RightNudgeTimer.Enabled = 0
     LeftNudgeTimer.Interval = delay
     LeftNudgeTimer.Enabled = 1
 End Sub
 
 Sub RightNudge(angle, strength, delay)
     vpmNudge.DoNudge angle, (strength * (delay-RightNudgeEffect) / delay) + LeftNudgeEffect / delay
     RightNudgeEffect = delay
     LeftNudgeEffect = 0
     LeftNudgeTimer.Enabled = 0
     RightNudgeTimer.Interval = delay
     RightNudgeTimer.Enabled = 1
 End Sub
 
 Sub CenterNudge(angle, strength, delay)
     vpmNudge.DoNudge angle, strength * (delay-NudgeEffect) / delay
     NudgeEffect = delay
     NudgeTimer.Interval = delay
     NudgeTimer.Enabled = 1
 End Sub
 
 Sub LeftNudgeTimer_Timer()
     LeftNudgeEffect = LeftNudgeEffect-1
     If LeftNudgeEffect = 0 then LeftNudgeTimer.Enabled = False
 End Sub
 
 Sub RightNudgeTimer_Timer()
     RightNudgeEffect = RightNudgeEffect-1
     If RightNudgeEffect = 0 then RightNudgeTimer.Enabled = False
 End Sub
 
 Sub NudgeTimer_Timer()
     NudgeEffect = NudgeEffect-1
     If NudgeEffect = 0 then NudgeTimer.Enabled = False
 End Sub
 
 '*********
 ' Switches
 '*********
 
 ' Slings
 Dim LStep, RStep
 
 Sub LeftSlingShot_Slingshot:LeftSling.visible = 1:PlaySound "slingshot":vpmTimer.PulseSw 43:LStep = 0:Me.TimerEnabled = 1:End Sub
 
 Sub LeftSlingShot_Timer
     Select Case LStep
         Case 0:LeftSLing.visible = 1
         Case 1: 'pause
         Case 2:LeftSLing.visible = 0:LeftSLing1.visible = 1
         Case 3:LeftSLing1.visible = 0:LeftSLing2.visible = 1
         Case 4:LeftSLing2.visible = 0:LeftSLing.visible = 1:Me.TimerEnabled = 0
     End Select
 
     LStep = LStep + 1
 End Sub
 
 Sub RightSlingShot_Slingshot:RightSling.visible = 1:PlaySound "slingshot":vpmTimer.PulseSw 44:RStep = 0:Me.TimerEnabled = 1:End Sub
 Sub RightSlingShot_Timer
     Select Case RStep
         Case 0:RightSLing.visible = 1
         Case 1: 'pause
         Case 2:RightSLing.visible = 0:RightSLing1.visible = 1
         Case 3:RightSLing1.visible = 0:RightSLing2.visible = 1
         Case 4:RightSLing2.visible = 0:RightSLing.visible = 1:Me.TimerEnabled = 0
     End Select
 
     RStep = RStep + 1
 End Sub
 
 ' Bumpers
 Sub Bumper1_Hit:vpmTimer.PulseSw 45:PlaySound "tall_bumper":bc1.z = -30:vpmTimer.AddTimer 200,"bc1.z = -10'":End Sub 
 Sub Bumper2_Hit:vpmTimer.PulseSw 46:PlaySound "tall_bumper":bc2.z = -30:vpmTimer.AddTimer 200,"bc2.z = -10'":End Sub 
 Sub Bumper3_Hit:vpmTimer.PulseSw 47:PlaySound "tall_bumper":bc3.z = -30:vpmTimer.AddTimer 200,"bc3.z = -10'":End Sub 
 Sub Bumper4_Hit:vpmTimer.PulseSw 48:PlaySound "tall_bumper":bc4.z = -30:vpmTimer.AddTimer 200,"bc4.z = -10'":End Sub 
 ' Holes
 Sub Drain_Hit:Playsound "drain":bsTrough.AddBall Me:End Sub
 Sub Drain1_Hit:Playsound "drain":bsTrough.AddBall Me:End Sub
 Sub Drain2_Hit:Playsound "drain":bsTrough.AddBall Me:End Sub
 Sub Drain3_Hit:Playsound "drain":bsTrough.AddBall Me:End Sub
 Sub Drain4_Hit:Playsound "drain":bsTrough.AddBall Me:End Sub
 
 Sub sw34_Hit:PlaySound "ballhit":sw34.destroyball:vpmTimer.PulseSwitch(34), 100, "VUKout":End Sub
 
 Sub sw35_Hit:PlaySound "ballhit":sw35.destroyball:vpmTimer.PulseSwitch(35), 100, "VUKout":End Sub
 
 Sub sw36a_Hit:sw36a.kick 210,30,1.4:PlaySound "popper":End Sub
 
 Sub VUKout(Switch)
     bsVUK.AddBall 0
 End Sub
 
 ' Holes with animation
 Dim aBall, aZpos
 Dim bBall, bZpos
 
 Sub sw33_Hit
     Set aBall = ActiveBall
     PlaySound "ballhit"
     aZpos = 35
     Me.TimerInterval = 2
     Me.TimerEnabled = 1
 End Sub
 
 Sub sw33_Timer
     aBall.Z = aZpos
     aZpos = aZpos-4
     If aZpos <-30 Then
         Me.TimerEnabled = 0
         Me.DestroyBall
         bsR.AddBall Me
     End If
 End Sub
 
 Sub sw37_Hit
     Set bBall = ActiveBall
     PlaySound "ballhit"
     bZpos = 35
     Me.TimerInterval = 2
     Me.TimerEnabled = 1
 End Sub
 
 Sub sw37_Timer
     bBall.Z = bZpos
     bZpos = bZpos-4
     If bZpos <-30 Then
         Me.TimerEnabled = 0
         Me.DestroyBall
         bsL.AddBall Me
     End If
 End Sub
 
 ' Rollovers
 Sub sw21_Hit:Controller.Switch(21) = 1:PlaySound "sensor":End Sub
 Sub sw21_UnHit:Controller.Switch(21) = 0:End Sub
 
 Sub sw22_Hit:Controller.Switch(22) = 1:PlaySound "sensor":ActiveBall.VelY = ActiveBall.VelY/2:End Sub
 Sub sw22_UnHit:Controller.Switch(22) = 0:End Sub
 
 Sub sw23_Hit:Controller.Switch(23) = 1:PlaySound "sensor":ActiveBall.VelY = ActiveBall.VelY/2:End Sub
 Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
 
 Sub sw24_Hit:Controller.Switch(24) = 1:PlaySound "sensor":End Sub
 Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
 
 Sub sw17_Hit:Controller.Switch(17) = 1:PlaySound "sensor":End Sub
 Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub
 
 Sub sw19_Hit:Controller.Switch(19) = 1:PlaySound "sensor":End Sub
 Sub sw19_UnHit:Controller.Switch(19) = 0:End Sub
 
 Sub sw20_Hit:Controller.Switch(20) = 1:End Sub
 Sub sw20_UnHit:Controller.Switch(20) = 0:End Sub
 
 Sub sw38_Hit:Controller.Switch(38) = 1:sw17.enabled = 1:End Sub
 Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub
 
 Sub sw39_Hit:Controller.Switch(39) = 1:PlaySound "metalrolling":End Sub
 Sub sw39_UnHit:Controller.Switch(39) = 0:End Sub
 
 Sub sw14_Hit:Controller.Switch(14) = 1:sw17.enabled = 0:PlungerLight.state = 2:End Sub
 Sub sw14_UnHit:Controller.Switch(14) = 0:PlungerLight.state = 0:End Sub
 
 ' Droptargets
 Sub sw32_Hit:dtbank.Hit 1:End Sub
 Sub sw31_Hit:dtbank.Hit 2:End Sub
 Sub sw30_Hit:dtbank.Hit 3:End Sub
 
 ' Targets
 Sub sw25_Hit:vpmTimer.PulseSw 25:sw25.IsDropped = 1:sw25a.IsDropped = 0:Me.TimerEnabled = 1:PlaySound "target":End Sub
 Sub sw25_Timer:sw25.IsDropped = 0:sw25a.IsDropped = 1:Me.TimerEnabled = 0:End Sub
 
 Sub sw26_Hit:vpmTimer.PulseSw 26:sw26.IsDropped = 1:sw26a.IsDropped = 0:Me.TimerEnabled = 1:PlaySound "target":End Sub
 Sub sw26_Timer:sw26.IsDropped = 0:sw26a.IsDropped = 1:Me.TimerEnabled = 0:End Sub
 
 Sub sw27_Hit:vpmTimer.PulseSw 27:sw27.IsDropped = 1:sw27a.IsDropped = 0:Me.TimerEnabled = 1:PlaySound "target":End Sub
 Sub sw27_Timer:sw27.IsDropped = 0:sw27a.IsDropped = 1:Me.TimerEnabled = 0:End Sub
 
 Sub sw28_Hit:vpmTimer.PulseSw 28:sw28.IsDropped = 1:sw28a.IsDropped = 0:Me.TimerEnabled = 1:PlaySound "target":End Sub
 Sub sw28_Timer:sw28.IsDropped = 0:sw28a.IsDropped = 1:Me.TimerEnabled = 0:End Sub
 
 Sub sw29_Hit:vpmTimer.PulseSw 29:sw29.IsDropped = 1:sw29a.IsDropped = 0:Me.TimerEnabled = 1:PlaySound "target":End Sub
 Sub sw29_Timer:sw29.IsDropped = 0:sw29a.IsDropped = 1:Me.TimerEnabled = 0:End Sub ' Death Star Door hits
 Sub sw40_Hit:PlaySound "target":vpmTimer.PulseSw 40:End Sub

 Sub bar13_Hit:PlaySound "target":vpmTimer.PulseSw 40:End Sub

 
 '*********
 'Solenoids
 '*********
 
 SolCallback(1) = "bsTrough.SolIn"
 SolCallback(2) = "bsTrough.SolOut"
 Solcallback(3) = "vpmSolAutoPlunger Plunger,15,"
 SolCallback(4) = "bsL.SolOut"
 SolCallback(5) = "bsVUK.SolOut"
 SolCallback(6) = "dtBank.SolDropUp"
 SolCallback(7) = "bsR.SolOut"
 SolCallback(8) = "vpmSolSound ""Knocker"","
 SolCallback(9) = "SolR2D2"
 
 SolCallback(11) = "SolGi"
SolCallBack(12) ="barupdate"
 
 SolCallback(16) = "vpmSolAutoPlunger Kickback,10,"
SolCallBack(15) = "rotatehead"
 
 SolCallback(25) = "SetLamp 66,"
 SolCallBack(26) = "SetLamp 67,"
 SolCallBack(27) = "SetLamp 68,"
 SolCallBack(28) = "SetLamp 69,"
 SolCallBack(29) = "SetLamp 70,"
 SolCallBack(30) = "SetLamp 71,"
 SolCallBack(31) = "SetLamp 72,"
 SolCallback(32) = "SetLamp 73,"
 
 Sub SolR2D2(Enabled)
	R2D2Down = ABS(Enabled)
	If Enabled = True Then
		PrR2D2.z = -10
		PrR2D2Head.z = -10
		R2D2_Specular_bling.z = 180
	End If
	If Enabled = False Then
		PrR2D2.z = 0
		PrR2D2Head.z = 0
		R2D2_Specular_bling.z = 190
	End If
End Sub

Dim headdirection, headspeed, rightstop, leftstop
headdirection = -1
rightstop = -70
leftstop = 70

Sub TiR2Head_Timer()
	headspeed = (leftstop - PrR2D2Head.RotY)/30
	If headspeed > 1 Then headspeed = 1
	PrR2D2Head.RotY = PrR2D2Head.RotY + (headdirection*headspeed)
	If PrR2D2Head.RotY > (leftstop-1) Then headdirection = -1
	If PrR2D2Head.RotY < rightstop Then headdirection = 1
	FlDSGreen.x = dSin(PrDS.RotZ+225)*130 + PrDS.x
	FlDSGreen.y = dCos(PrDS.Rotz+45)*130 + PrDS.y
	FlDSGreen.RotX = (dSin(PrDS.Rotz-45)*50)
	FlDSGreen.RotY = (dCos(PrDS.Rotz-225)*50)
	PrDS.Rotz = PrDS.Rotz + 0.5
	If PrDS.RotZ >= 360 Then PrDS.RotZ = 0
    TopNut.RotY = PrDS.RotZ
End Sub

Sub rotatehead(enabled)
	TiR2Head.enabled = enabled
End Sub

Function Pi():Pi = 4*Atn(1):End Function

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
	if ABS(dSin) < 0.000001 Then dSin = 0
	if ABS(dSin) > 0.999999 Then dSin = 1 * sgn(dSin)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
	if ABS(dCos) < 0.000001 Then dCos = 0
	if ABS(dCos) > 0.999999 Then dCos = 1 * sgn(dCos)
End Function

 Sub SolGi(Enabled):SetLamp 65, ABS(NOT Enabled):End Sub
 
 ' Bar update
 Sub BarUpdate(Enabled)
     bar13.timerenabled = Enabled
 End Sub

Controller.Switch(41) = 1
Controller.Switch(42) = 0
dim bardirection
bardirection = -0.25

Sub bar13_Timer()
	PrBar.z = PrBar.z + bardirection
	If PrBar.z <= -52 Then
		Controller.Switch(42) = 1
		bardirection = 0.25
		bar13.isdropped = 1
	ElseIf PrBar.z >= 0 Then
		Controller.Switch(41) = 1
		bardirection = -0.25
	End If
	If PrBar.z >= -48 AND PrBar.z <=-3 Then
		Controller.Switch(41) = 0
		Controller.Switch(42) = 0
		bar13.isdropped = 0
	End If
End Sub

 '**************
 ' Flipper Subs
 '**************
 
 SolCallback(sLRFlipper) = "SolRFlipper"
 SolCallback(sLLFlipper) = "SolLFlipper"
 
 Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SFlipperOn:LeftFlipper.RotateToEnd
         Llogo.IsDropped = 1:Llogo2.IsDropped = 0
     Else
         PlaySound SFlipperOff:LeftFlipper.RotateToStart
         Llogo2.IsDropped = 1:Llogo.IsDropped = 0
     End If
 End Sub
 
 Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SFlipperOn:RightFlipper.RotateToEnd
         Rlogo.IsDropped = 1:Rlogo2.IsDropped = 0
     Else
         PlaySound SFlipperOff:RightFlipper.RotateToStart
         Rlogo2.IsDropped = 1:Rlogo.IsDropped = 0
     End If
 End Sub 
 
 
'****************************************
'  JP's Fading Lamps 3.6 VP9 Fading only
'      Based on PD's Fading Lights
' SetLamp 0 is Off
' SetLamp 1 is On
' LampState(x) current state
' Includes the flash element (needs own timer)
' Flashers can be used as lights too
'****************************************
 
Dim LampState(200), FadingLevel(200)
Dim FlashState(200), FlashLevel(200)
Dim FlashSpeedUp, FlashSpeedDown
 
 AllLampsOff()
 LampTimer.Interval = 35
 LampTimer.Enabled = 1

FlashInit()
FlasherTimer.Interval = 10 'flash fading speed
FlasherTimer.Enabled = 1
 
 Sub LampTimer_Timer()
     Dim chgLamp, num, chg, ii
     chgLamp = Controller.ChangedLamps
     If Not IsEmpty(chgLamp) Then
         For ii = 0 To UBound(chgLamp)
             LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
			FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
			FlashState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
         Next
     End If
     UpdateLamps
 End Sub
 
 Sub UpdateLamps
     'Flashers
     NFadeLm 66, f1
     FadeDisableLighting 67, f2, 10
     NFadeLm 67, f2a     
     FadeL 68, l24, l24a
     NFadeLm 69, f4c
     NFadeLm 69, l45b
     FadeLm 69, f40, f40a
     FadeLm 69, f41, f41a
     FadeDisableLighting 69, f4, 10
     NFadeLm 69, f4a
	 FadeL 70, l29, l29a
     FadeDisableLighting 71, f21, 10
     NFadeLm 71, f21a
     FadeL 71, f6, f6a
     NFadeL 72, l58
     FadeDisableLighting 73, f8, 10
     NFadeLm 73, f8a
     NFadeLm 73, f8d
     NFadeLm 73, f82
     FadeLm 73, f80, f80a
     FadeL 73, f81, f81a
 
     'Lights
     FadeL 1, l1, l1a
     FadeL 2, l2, l2a
     FadeL 3, l3, l3a
     FadeL 4, l4, l4a
     FadeL 5, l5, l5a
     FadeL 6, l6, l6a
     FadeL 7, l7, l7a
     FadeL 8, l8, l8a
     FadeL 9, l9, l9a
     FadeL 10, l10, l10a
     FadeL 11, l11, l11a
     FadeL 12, l12, l12a
     FadeL 13, l13, l13a
     FadeL 14, l14, l14a
     FadeL 15, l15, l15a
     FadeL 16, l16, l16a
     FadeL 17, l17, l17a
     FadeL 18, l18, l18a
     FadeL 19, l19, l19a
     FadeL 20, l20, l20a
     FadeL 21, l21, l21a
     FadeL 22, l22, l22a
     FadeL 23, l23, l23a
     FadeL 24, l24, l24a
'     NFadeL 25, l25
     FadeL 26, l26, l26a
     FadeL 27, l27, l27a
     FadeL 28, l28, l28a
   	 NFadeL 31, L31
     FadeL 32, l32, l32a
     FadeL 33, l33, l33a
     FadeL 34, l34, l34a
     FadeL 35, l35, l35a
     FadeL 36, l36, l36a
     FadeL 37, l37, l37a     
	FadeDisableLighting 38, l38, 8
	FadeDisableLighting 39, l39, 8
	FadeDisableLighting 40, l40, 8
     FadeL 41, l41, l41a
     FadeL 42, l42, l42a
     FadeL 43, l43, l43a
     FadeL 44, l44, l44a
     FadeL 45, l45, l45a
     FadeL 46, l46, l46a
     NFadeLm 47, l47c     
	FadeDisableLighting 47, l47, 8     
	FadeDisableLighting 48, l48, 8
	FadeDisableLighting 49, l49, 8
	FadeDisableLighting 50, l50, 8
	FadeDisableLighting 51, l51, 8
     FadeL 52, l52, l52a
     FadeL 53, l53, l53a
     FadeL 54, l54, l54a
     FadeL 55, l55, l55a
     FadeL 56, l56, l56a     
	FadePR2 57, PrR2D2Head
     FadeL 59, l59, l59a
     FadeL 60, l60, l60a
     NFadeL 61, BL1
     NFadeL 62, BL2
     NFadeL 63, BL3
     NFadel 64, BL4

     'Gi
     NFadeL 65, L65
Dim xx
if L65.state = 1 Then
for each xx in GI: xx.state = 1:Next
table1.colorgradeimage = "ColorGradeLUT256x16_extraConSat"
Wall119.blenddisablelighting = 0.05
PrDS.blenddisablelighting = 0.45
PrR2D2.blenddisablelighting = 0.25
PrR2D2Head.blenddisablelighting = 0.3
Ramp1.image = "aluminumBright"
Ramp2.image = "aluminumBright"
Ramp9.image = "aluminumBright"
Else
for each xx in GI:xx.state = 0:Next
table1.colorgradeimage = "ColorGrade_4"
Wall119.blenddisablelighting = 0
PrDS.blenddisablelighting = 0.2
PrR2D2.blenddisablelighting = 0.07
PrR2D2Head.blenddisablelighting = 0.1
Ramp1.image = "aluminum"
Ramp2.image = "aluminum"
Ramp9.image = "aluminum"
end if 
 End Sub

Sub FlasherTimer_Timer()
	Flash 57, FlR2Head
	Flash 29, FlDSGreen
	Flash 30, FlDSYellow
End Sub

Sub AllLampsOff()
	For x = 1 to 200
		LampState(x) = 4
	Next
	UpdateLamps
	UpdateLamps
	Updatelamps
End Sub
 
Sub SetLamp(nr, value)
	LampState(nr) = abs(value) + 4
	FadingLevel(nr) = abs(value) + 4
End Sub

Sub FlashInit
	Dim i
	For i = 0 to 200
		FlashState(i) = 0
		FlashLevel(i) = 0
	Next

	FlashSpeedUp = 50   ' fast speed when turning on the flasher
	FlashSpeedDown = 10 ' slow speed when turning off the flasher, gives a smooth fading
	AllFlashOff()
End Sub

Sub AllFlashOff
	Dim i
	For i = 0 to 200
		FlashState(i) = 0
	Next
End Sub

Sub SetFlash(nr, stat)
	FlashState(nr) = ABS(stat)
End Sub

Sub FadePR2(nr, a)
	Select Case LampState(nr)
		Case 1:a.image = "R2D2_HeadMap_ON3":LampState(nr) = 6
		Case 2:a.image = "R2D2_HeadMap_OFF":LampState(nr) = 0
		Case 3:a.image = "R2D2_HeadMap_ON1":LampState(nr) = 2
		Case 4:a.image = "R2D2_HeadMap_ON2":LampState(nr) = 3
		Case 5:a.image = "R2D2_HeadMap_ON2":LampState(nr) = 1
		case 6:a.image = "R2D2_HeadMap_ON2":LampState(nr) = 0
	End Select
End Sub

Sub Flash(nr, object)
	Select Case FlashState(nr)
		Case 0 'off
			FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
			If FlashLevel(nr) < 0 Then
				FlashLevel(nr) = 0
				FlashState(nr) = -1 'completely off
			End if
			Object.opacity = FlashLevel(nr)
		Case 1 ' on
			FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
			If FlashLevel(nr) > 255 Then
				FlashLevel(nr) = 255
				FlashState(nr) = -2 'completely on
			End if
			Object.opacity = FlashLevel(nr)
	End Select
End Sub
 
 Sub FadeL(nr, a, b)
     Select Case LampState(nr)
         Case 2:b.state = 0:LampState(nr) = 0
         Case 3:b.state = 1:LampState(nr) = 2
         Case 4:a.state = 0:LampState(nr) = 3
         Case 5:a.state = 1:LampState(nr) = 1
     End Select
 End Sub
 
 Sub FadeLm(nr, a, b)
     Select Case LampState(nr)
         Case 2:b.state = 0
         Case 3:b.state = 1
         Case 4:a.state = 0
         Case 5:a.state = 1
     End Select
 End Sub
 
 Sub NFadeL(nr, a)
     Select Case LampState(nr)
         Case 4:a.state = 0:LampState(nr) = 0
         Case 5:a.State = 1:LampState(nr) = 1
     End Select
 End Sub
 
 Sub NFadeLm(nr, a)
     Select Case LampState(nr)
         Case 4:a.state = 0
         Case 5:a.State = 1
     End Select
 End Sub 
 
 Sub FlashL(nr, a, b) ' simple light flash, not controlled by the rom
     Select Case LampState(nr)
         Case 2:b.state = 0:LampState(nr) = 0
         Case 3:b.state = 1:LampState(nr) = 2
         Case 4:a.state = 0:LampState(nr) = 3
         Case 5:a.state = 1:LampState(nr) = 4
     End Select
 End Sub
 

 
Sub FadeDisableLighting(nr, a, alvl)
	Select Case FadingLevel(nr)
		Case 4
			a.UserValue = a.UserValue - 0.3
			If a.UserValue < 0 Then 
				a.UserValue = 0
				FadingLevel(nr) = 0
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
		Case 5
			a.UserValue = a.UserValue + 0.50
			If a.UserValue > 1 Then 
				a.UserValue = 1
				FadingLevel(nr) = 1
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
	End Select
End Sub

 '**************
 ' Ramp helpers
 '**************
 
 Sub RHelp1_Hit()
     ActiveBall.VelZ = -1
     ActiveBall.VelY = 0
     ActiveBall.VelX = 0
     StopSound "metalrolling"
     PlaySound "BallHit"
 End Sub
 
 Sub RHelp2_Hit()
     ActiveBall.VelZ = -1
     ActiveBall.VelY = 0
     ActiveBall.VelX = 0
     StopSound "metalrolling"
     PlaySound "BallHit"
 End Sub
 
 Sub RHelp3:PlaySound "metalrolling":End Sub

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
    Vol = Csng(BallVel(ball) ^2 / 400)
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

Const tnob = 6 ' total number of balls
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
      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/9, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/12, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
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

 '**********************
'Flipper Shadows
'***********************
Sub RealTime_Timer
  lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
BallShadowUpdate
End Sub


Sub BallShadowUpdate()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6)
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
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10                       
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 80
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 80
End If
    Next	
End Sub

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
	PlaySound "gate", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub Table1_Exit():Controller.Stop:End Sub
Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
