Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="taxi_l3",UseSolenoids=1,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM "00990300", "S11.VBS", 3.36
Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=1
Else
Ramp16.visible=0
Ramp15.visible=0
Primitive13.visible=0
End if

'**********************************************************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1) = "bsTrough.SolIn"
SolCallback(2) = "bsTrough.SolOut"
SolCallback(3) = "b3.SolOut"
SolCallback(4) = "dtC.SolDropUp"
SolCallback(5) = "bsJoyRide.SolOut"
SolCallback(6) = "dtR.SolDropUp"
SolCallback(7) = "bsSpinOut.SolOut"
SolCallback(8) = "bsLock.SolOut"
SolCallback(9) = "vpmSolWall TopGate, 0, " 'Top ball Gate
SolCallback(13) = "vpmSolSound ""fx_bellring"","
SolCallback(14) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(23) = "vpmNudge.SolGameOn"

'GI
'SolCallback(10) = 'Insert Gen Illumin Relay aka backglass GI
SolCallback(11) = "PFGI" 'PF GI

'Flahsers
SolCallback(15) ="vpmFlasher Flasher15," 'Jackpot Flasher
SolCallback(16) ="vpmFlasher Flasher16," 'Joyride Flasher
SolCallback(25) ="vpmFlasher Flasher25,"
SolCallback(26) ="vpmFlasher Flasher26,"
SolCallback(27) ="vpmFlasher Flasher27,"
SolCallback(28) ="vpmFlasher Flasher28,"
SolCallback(29) ="vpmFlasher Flasher29,"
SolCallback(30) ="vpmFlasher array(Flasher30,Flasher30a)," 'Left Ramp Flasher
SolCallback(31) ="vpmFlasher array(Flasher31,Flasher31a)," 'Right Ramp Flasher
SolCallback(32) ="vpmFlasher array(Flasher32,Flasher32a)," 'Spinout Flasher


SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart
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

'Playfield GI
Sub PFGI(Enabled)
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"
	Else
		For each xx in GI:xx.State = 1: Next
        PlaySound "fx_relay"
	End If
 End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************
dim bsTrough, bsLock, bsJoyRide, b3, bsSpinout, dtR, dtC, Mspin

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Taxi (Williams 1988)"&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    vpmNudge.TiltSwitch = swTilt
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3,LeftSlingShot,RightSlingshot)

    Set bsTrough = New cvpmBallStack
		bsTrough.InitSw 10, 11, 12, 0, 0, 0, 0, 0
		bsTrough.InitKick BallRelease, 60, 6
		bsTrough.Balls = 2
		bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)

    Set bsLock = New cvpmBallStack
		bsLock.InitSaucer RightLock, 36, 180, 20
		bsLock.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

    Set bsJoyRide = New cvpmBallStack
		bsJoyRide.InitSaucer JoyrideEject, 13, 170, 20
		bsJoyRide.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

    Set b3 = New cvpmBallStack
		b3.InitSaucer Catapult, 35, 0, 140
		b3.KickZ = 1
		b3.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

    Set bsSpinout = New cvpmBallStack
		bsSpinout.InitSw 0, 43, 0, 0, 0, 0, 0, 0
		bsSpinout.InitKick SpinoutKicker, 330, 35
		bsSpinout.KickAngleVar = 2
		bsSpinout.KickForceVar = 3
		bsSpinout.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

    Set dtR = New cvpmDropTarget
		dtR.InitDrop Array(sw30,sw31,sw32),Array(30,31,32)
		dtR.InitSnd  SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

    Set dtC = New cvpmDropTarget
		dtC.InitDrop Array(sw27,sw28,sw29),Array(27,28,29)
		dtC.InitSnd  SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
End Sub

'*********************************************************************************************************
'Kickers
Sub Drain_Hit:playsound"drain":bsTrough.addball me:End Sub
Sub RightLock_Hit():bsLock.AddBall 0 : playsound "popper_ball": End Sub
Sub JoyrideEject_Hit():bsJoyRide.AddBall 0 : playsound "popper_ball": End Sub

'Dracula kicker
Sub Catapult_Hit():b3.AddBall 0: playsound "popper_ball": End Sub

Sub DracKickerIn_Hit()
    DracKickerIn.DestroyBall
    vpmCreateBall DracKickerOut
    DracKickerOut.Kick 15, 45
    Prim_Catapult.ObjRotX = 75
    Me.TimerEnabled = 1  
End Sub
Sub DracKickerIn_timer : Prim_Catapult.ObjRotX = 0 : Me.TimerEnabled = 0 : End Sub

'Spinner Ramp
Sub SpinoutKicker1_Hit():SpinoutKicker1.DestroyBall:bsSpinOut.addball Me : StopSound "fx_launch" : PlaySound "fx_balldrop": End Sub
Sub SpinHelp_Hit():StopSound "fx_launch" :ActiveBall.VelY = 1.4 * ActiveBall.VelY:End Sub
Sub SpinoutTrigger_Hit():vpmTimer.pulsesw 44:End Sub
Sub Trigger7_Hit() : PlaySound "fx_turn_SSRamp" :End Sub
Sub Trigger8_Hit() : If ActiveBall.VelY < 0 Then Playsound "fx_launch" : Else StopSound "fx_launch" : End If : End Sub

'Drop Targets
 Sub Sw27_Dropped:dtC.Hit 1 :End Sub  
 Sub Sw28_Dropped:dtC.Hit 2 :End Sub  
 Sub Sw29_Dropped:dtC.Hit 3 :End Sub

 Sub Sw30_Dropped:dtR.Hit 1 :End Sub  
 Sub Sw31_Dropped:dtR.Hit 2 :End Sub  
 Sub Sw32_Dropped:dtR.Hit 3 :End Sub

'Bumpers
Sub Bumper1_Hit():vpmTimer.pulsesw 17 : playsound"fx_bumper1": End Sub
Sub Bumper2_Hit():vpmTimer.pulsesw 19 : playsound"fx_bumper2": End Sub
Sub Bumper3_Hit():vpmTimer.pulsesw 21 : playsound"fx_bumper3": End Sub

'Stand UP Target
Sub sw24_Hit():vpmTimer.PulseSwitch 24, 0, "":End Sub

'Wire Triggers
 Sub sw14_Hit():Controller.Switch(14)=1: playsound"rollover" : End Sub
 Sub sw14_Unhit():Controller.Switch(14)=0:End Sub
 Sub sw15_Hit():Controller.Switch(15)=1 : playsound"rollover" : End Sub
 Sub sw15_Unhit():Controller.Switch(15)=0:End Sub
 Sub sw16_Hit():Controller.Switch(16)=1 : playsound"rollover" : End Sub
 Sub sw16_Unhit():Controller.Switch(16)=0:End Sub
 Sub sw37_Hit():Controller.Switch(37)=1 : playsound"rollover" : End Sub
 Sub sw37_UnHit():Controller.Switch(37)=0:End Sub
 Sub sw38_Hit():Controller.Switch(38)=1 : playsound"rollover" : End Sub
 Sub sw38_UnHit():Controller.Switch(38)=0:End Sub
 Sub sw39_Hit():Controller.Switch(39)=1 : playsound"rollover" : End Sub
 Sub sw39_UnHit():Controller.Switch(39)=0:End Sub
 Sub sw40_Hit():Controller.Switch(40)=1 : playsound"rollover" : End Sub
 Sub sw40_UnHit():Controller.Switch(40)=0:End Sub
'Shooter Lane
 Sub sw22_Hit():Controller.Switch(22)=1 : playsound"rollover" : End Sub
 Sub sw22_Unhit():Controller.Switch(22)=0 : End Sub

'Gate Triggers
'Gorbie Lane Entry
Sub sw23_Hit():vpmTimer.PulseSwitch 23, 0, "":End Sub
'Left Ramp Entry
Sub sw25_Hit():vpmTimer.PulseSwitch 25, 0, "":End Sub
'Right Ramp Entry
Sub sw26_Hit():vpmTimer.PulseSwitch 26, 0, "":End Sub

'Right Ramp exit = enter wire ramp
Sub sw33_Hit():Controller.Switch(33) = 1 : sw33.timerenabled = 1 : playsound"rollover" : End Sub
Sub sw33_UnHit():Controller.Switch(33) = 0:End Sub
dim sw33Dir
sw33Dir = -1
Sub sw33_Timer()
	If sw33P.ObjRotZ = 240 then sw33Dir = 20
	If sw33P.ObjRotZ = 220 then sw33Dir = -20
	sw33P.ObjRotZ = sw33P.ObjRotZ - sw33Dir
	If sw33P.ObjRotZ = 220 then sw33.timerenabled = 0
End Sub

'Left Ramp exit = enter wire ramp
Sub sw34_Hit():Controller.Switch(34) = 1 : sw34.timerenabled = 1 : playsound"rollover" : End Sub
Sub sw34_UnHit():Controller.Switch(34) = 0:End Sub
dim sw34Dir
sw34Dir = -1
Sub sw34_Timer()
	If sw34P.ObjRotZ = 110 then sw34Dir = 20
	If sw34P.ObjRotZ = 130 then sw34Dir = -20
	sw34P.ObjRotZ = sw34P.ObjRotZ + sw34Dir
	If sw34P.ObjRotZ = 130 then sw34.timerenabled = 0
End Sub

'Generic ramp sounds
Sub Trigger1_Hit: playsound"fx_balldrop":End Sub
Sub Trigger2_Hit: playsound"fx_balldrop":End Sub
Sub Trigger3_Hit: playsound"Wire Ramp": End Sub
Sub Trigger4_Hit: playsound"Wire Ramp": End Sub
Sub BalldropSpinramp_Hit: playsound"fx_balldrop":End Sub

Set Lights(9)=l9
Set Lights(10)=l10
Set Lights(11)=l11
Set Lights(12)=l12
Set Lights(13)=l13
Set Lights(14)=l14
Set Lights(15)=l15
Set Lights(16)=l16
Set Lights(17)=l17
Set Lights(18)=l18	
Set Lights(19)=l19
Set Lights(20)=l20
Set Lights(21)=l21
Set Lights(22)=l22
Set Lights(23)=l23
Set Lights(24)=l24
Set Lights(25)=l25
Set Lights(26)=l26
Set Lights(27)=l27
Set Lights(28)=l28
Set Lights(29)=l29
Set Lights(30)=l30
Set Lights(31)=l31
Set Lights(32)=l32	
Set Lights(33)=l33
Set Lights(34)=l34
Set Lights(35)=l35
Set Lights(36)=l36
Set Lights(37)=l37
Set Lights(38)=l38
Set Lights(39)=l39
Set Lights(40)=l40
Set Lights(41)=l41
Set Lights(42)=l42
Set Lights(43)=l43
Set Lights(44)=l44
Set Lights(45)=l45
Set Lights(46)=l46
Set Lights(47)=l47
Set Lights(48)=l48
Set Lights(57)=l57
Set Lights(58)=l58
Set Lights(59)=l59
Set Lights(60)=l60
Set Lights(61)=l61
Set Lights(62)=l62
Set Lights(63)=l63


'Trafic Light
Lights(53)=array(l53,l53a) 'Yellow
Lights(54)=array(l54,l54a) 'Red
Lights(55)=array(l55,l55a) 'Green

'BackGlass
'Set Lights(1)=l1 '10 Thousand
'Set Lights(2)=l2 'Spot
'Set Lights(3)=l3 'Mystery Score
'Set Lights(4)=l4 'Extra Ball
'Set Lights(5)=l5 'Special
'Set Lights(64)=l64



'enable PF Traffic lights for desktop or FS
If DesktopMode = True Then
l53a.visible=1
l54a.visible=1
l55a.visible=1
l53.visible=0
l54.visible=0
l55.visible=0
Else
l53a.visible=0
l54a.visible=0
l55a.visible=0
l53.visible=1
l54.visible=1
l55.visible=1
end If


'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************

 Dim Digits(39)
 Digits(0)=Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
 Digits(1)=Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
 Digits(2)=Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
 Digits(3)=Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
 Digits(4)=Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
 Digits(5)=Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
 Digits(6)=Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
 Digits(7)=Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
 Digits(8)=Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
 Digits(9)=Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
 Digits(10)=Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
 Digits(11)=Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
 Digits(12)=Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
 Digits(13)=Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
 Digits(14) =Array(ac18, ac16, acc1, acd1, ac19, ac17, ac15, acf1, ac11, ac13, ac12, ac14, acb1, aca1, ac10, ace1)
 Digits(15) =Array(ad18, ad16, adc1, add1, ad19, ad19, ad15, adf1, ad11, ad13, ad12, ad14, adb1, ada1, ad10, ade1)

Digits(16) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(17) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(18) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(19) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(20) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(21) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)
Digits(22) = Array(LED70,LED71,LED72,LED73,LED74,LED75,LED76)
Digits(23) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(24) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(25) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(26) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(27) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(28) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)
Digits(29) = Array(LED140,LED141,LED142,LED143,LED144,LED145,LED146)
Digits(30) = Array(LED151,LED149,LED153,LED154,LED152,LED148,LED150)
Digits(31) = Array(LED158,LED156,LED160,LED161,LED159,LED155,LED157)

Digits(32) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(33) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)
Digits(34) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(35) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)
Digits(36) = Array(LED88,LED79,LED97,LED98,LED89,LED78,LED87)
Digits(37) = Array(LED109,LED107,LED118,LED119,LED117,LED99,LED108)
Digits(38) = Array(LED137,LED128,LED139,LED147,LED138,LED127,LED129)


Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		If DesktopMode = True Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 39) then
				For Each obj In Digits(num)
					If chg And 1 Then obj.State = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			else
				'if char(stat) > "" then msg(num) = char(stat)
			end if
		next
		end if
end if
End Sub

'**********************************************************************************************************
'**********************************************************************************************************
'**********************************************************************************************************
'VPX Functions
'**********************************************************************************************************
'**********************************************************************************************************
'**********************************************************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmtimer.pulsesw 20
    PlaySound SoundFX("right_slingshot",DOFContactors), 0, 1, 0.05, 0.05
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
	vpmtimer.pulsesw 18:
    PlaySound SoundFX("left_slingshot",DOFContactors),0,1,-0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub

'****************************
'     Realtime Updates
' called by the MotorCallBack
'****************************
Set MotorCallback=GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    'flippers
    FlipperLP.RotY = LeftFlipper.CurrentAngle
    FlipperRP.RotY = RightFlipper.CurrentAngle
End Sub


' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 800)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
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
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
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

' The sound is played using the VOL, PAN and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the PAN function will change the stereo position according
' to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rubbers_Hit(idx)
	PlaySound "fx_rubber", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

