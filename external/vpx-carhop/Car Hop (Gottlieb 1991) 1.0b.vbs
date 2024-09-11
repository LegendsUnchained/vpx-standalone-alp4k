Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="carhop",UseSolenoids=2,UseLamps=0,UseGI=0, SCoin="coin"

LoadVPM "01120100", "gts3.vbs", 3.26

'*******************************************
'  User Options
'*******************************************

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

'----- General Sound Options -----
Const VolumeDial = 0.8				' Recommended values should be no greater than 1.

'----- Phsyics Mods -----
Const RubberizerEnabled = 1			'0 = normal flip rubber, 1 = more lively rubber for flips
Const FlipperCoilRampupMode = 1   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. 0.0 thru 1.0, higher value is more bounciness (don't go above 1)


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

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
'PINMAME has no support for the Zero ID  add one from the service manual
'SolCallback(1)  = "vpmSolSound ""lSling"","
'SolCallback(2)  = "vpmSolSound ""lSling"","
SolCallback(3)  = "bsRKicker.SolOut"
SolCallback(4)  = "bsLKicker.SolOut"
SolCallback(5)  = "dtLBank.SolDropUp" 'Drop Targets
SolCallback(6)  = "dtRBank.SolDropUp" 'Drop Targets
SolCallback(7)  = "dtTBank.SolDropUp" 'Drop Targets
SolCallback(9)  = "SolDrop1"
SolCallback(10) = "SolDrop2"
SolCallback(11) = "SolDrop3"
SolCallback(12) = "SolDrop4"
SolCallback(13) = "SolDrop5"
SolCallback(14) = "SolTop"
'Aux board lights are mapped to PINMAME IDs  100-120 
SolCallback(15) = "FlashWhite1" 'Bottom Left 67 Flasher
SolCallBack(16) = "FlashWhite2" 'Bottom Right 67 Flasher
SolCallback(17) = "FlashWhite3" 'Top 67 Flasher
SolCallback(18) = "FlashWhite4" 'Bullseye 67 Flasher
SolCallback(19) = "FlashWhite6" 'Right Drop 67 Flasher
SolCallback(20) = "FlashWhite5" 'Left Drop 67 Flasher
'SolCallback(21) = 'Backglass Only
'SolCallback(22) = 'Backglass Only
'SolCallback(23) = 'Backglass Only
'SolCallback(24) = 'Backglass Only
'SolCallback(25) = 'Backglass Only
'SolCallback(26) = "PFGI" 'Backglass GI used as PF GI
'SolCallback(27) = 'Ticket Dispenser
SolCallback(28)=  "vpmSolSound SoundFX(""Bell"",DOFChimes),"
SolCallback(29) = "bsTrough.SolOut" 
SolCallback(30)=  "SolKnocker"
SolCallBack(31)= "PFGI"   'Tilt Realy   aka Gottliebs GI controller
SolCallback(32)= "vpmNudge.SolGameOn"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
         If Enabled Then
                LF.Fire  'leftflipper.rotatetoend
                LeftFlipper.RotateToEnd
                If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
                        RandomSoundReflipUpLeft LeftFlipper
                Else 
                        SoundFlipperUpAttackLeft LeftFlipper
                        RandomSoundFlipperUpLeft LeftFlipper
                End If                
        Else
                LeftFlipper.RotateToStart
                If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
                        RandomSoundFlipperDownLeft LeftFlipper
                End If
                FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub
 
Sub SolRFlipper(Enabled)
        If Enabled Then
                RF.Fire 'rightflipper.rotatetoend
                RightFlipper.RotateToEnd:RightFlipper1.RotateToEnd
                If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
                        RandomSoundReflipUpRight RightFlipper
                Else 
                        SoundFlipperUpAttackRight RightFlipper
                        RandomSoundFlipperUpRight RightFlipper
                End If
        Else
                RightFlipper.RotateToStart:RightFlipper1.RotateToStart
                If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
                        RandomSoundFlipperDownRight RightFlipper
                End If        
                FlipperRightHitParm = FlipperUpSoundLevel
        End If
End Sub


Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
    LeftFlipperCollide parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
    RightFlipperCollide parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub

' iaakki Rubberizer
sub Rubberizer(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * 1.2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * -1.1
		activeball.vely = activeball.vely * 1.4
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub

' apophis rubberizer
sub Rubberizer2(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 0.5
		activeball.vely = activeball.vely * (1.2 + rnd(1)/3 )
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub

'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

	Sub SolTop(Enabled)
		If Enabled Then
			dtTBank.Hit 1
		End If
	End Sub

	Sub SolDrop1(Enabled)
		If Enabled Then
			dtLBank.Hit 1
			dtRBank.Hit 5
		End If
	End Sub

	Sub SolDrop2(Enabled)
		If Enabled Then
			dtLBank.Hit 2
			dtRBank.Hit 4
		End If
	End Sub

	Sub SolDrop3(Enabled)
		If Enabled Then
			dtLBank.Hit 3
			dtRBank.Hit 3
		End If
	End Sub

	Sub SolDrop4(Enabled)
		If Enabled Then
			dtLBank.Hit 4
			dtRBank.Hit 2
		End If
	End Sub

	Sub SolDrop5(Enabled)
		If Enabled Then
			dtLBank.Hit 5
			dtRBank.Hit 1
		End If
	End Sub


Sub SolKnocker(Enabled)
        If enabled Then
                KnockerSolenoid 'Add knocker position object
        End If
End Sub

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

Dim bsTrough, bsLKicker, bsRKicker, dtLBank, dtRBank, dtTBank

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Car Hop Gottlieb/Premier 1990"&chr(13)&"You Suck"
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
	vpmNudge.TiltSwitch = 151
	vpmNudge.Sensitivity = 2
	vpmNudge.TiltObj = Array(RightSlingShot, LeftSlingShot)

     Set bsTrough = New cvpmBallStack
         '.InitSw 0, 0, 0, 0, 0, 0, 0, 0			'set trough switches
         bsTrough.InitNoTrough BallRelease, 25, 80, 6
'         bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
         bsTrough.Balls = 1	

     Set bsRKicker = New cvpmBallStack
         bsRKicker.InitSaucer sw40, 40, 165, 65
'         bsRKicker.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
         bsRKicker.KickAngleVar = 2

     Set bsLKicker = New cvpmBallStack
         bsLKicker.InitSaucer sw41, 41, 165, 85
'         bsLKicker.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
         bsLKicker.KickAngleVar = 2

     set dtLBank = new cvpmdroptarget
         dtLBank.initdrop array(sw16, sw26, sw36, sw46, sw56), array(16, 26, 36, 46, 56)
         dtLBank.initsnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

     set dtRBank = new cvpmdroptarget
         dtRBank.initdrop array(sw17, sw27, sw37, sw47, sw57), array(17, 27, 37, 47, 57)
         dtRBank.initsnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

	 set dtTBank = new cvpmdroptarget
         dtTBank.initdrop sw15, 15
         dtTBank.initsnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal keycode)
    If KeyCode = PlungerKey Then Plunger.Pullback:SoundPlungerPull()
	If keycode= StartGameKey Then Controller.Switch(3)= 1: soundStartButton()
	If KeyCode= LeftFlipperKey Then 
		Controller.Switch(4)=1
		Controller.Switch(6)=1
	End If
	If KeyCode= RightFlipperkey Then
		Controller.Switch(5)=1
		Controller.Switch(7)=1
	End If
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress

	If keycode = LeftMagnaSave Then bLutActive = True
	If keycode = RightMagnaSave Then 
		If bLutActive Then NextLUT: End If
    End If
	If vpmKeyDown(KeyCode) Then Exit Sub
	If keycode = LeftFlipperKey Then lfpress = 1
	If keycode = RightFlipperKey Then rfpress = 1
    If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
    If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
    If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter()
    
    If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
                Select Case Int(rnd*3)
                        Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                        Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                        Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

    End Select

    End If


End Sub


Sub Table1_KeyUp(ByVal keycode)
        If KeyCode = PlungerKey Then
                Plunger.Fire
                If BIPL = 1 Then
                        SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
                Else
                        SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
                End If
        End If
	If keycode= StartGameKey Then Controller.Switch(3)= 0
	If KeyCode= LeftFlipperKey Then 
		Controller.Switch(4)=0
		Controller.Switch(6)=0
	End If
	If KeyCode= RightFlipperkey Then
		Controller.Switch(5)=0
		Controller.Switch(7)=0
	End If
	If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode = LeftMagnaSave Then bLutActive = False
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
End Sub

'**********************************************************************************************************
' Fleep BIPL
'**********************************************************************************************************
Dim BIPL : BIPL=0

Sub sw44_Hit
	BIPL = 1
End Sub

Sub sw44_UnHit
	BIPL = 0
End Sub

'**********************************************************************************************************

 ' Drain hole and kickers

Sub Drain_Hit:bsTrough.addball me:RandomSoundDrain Drain:End Sub
Sub BallRelease_Unhit:RandomSoundBallRelease BallRelease: End Sub

Sub sw40_Hit:SoundSaucerLock:bsRKicker.AddBall 0:End Sub
Sub sw40_UnHit:SoundSaucerKick 1, sw40:End Sub

Sub sw41_Hit:SoundSaucerLock:bsLKicker.AddBall 0:End Sub
Sub sw41_UnHit:SoundSaucerKick 0, sw41:End Sub

'Drop Targets
 Sub Sw16_Dropped:dtLBank.Hit 1
 TargetBouncer Activeball, 1
 End Sub
  
 Sub Sw26_Dropped:dtLBank.Hit 2
 TargetBouncer Activeball, 1
 End Sub

 Sub Sw36_Dropped:dtLBank.Hit 3
 TargetBouncer Activeball, 1
 End Sub

 Sub Sw46_Dropped:dtLBank.Hit 4
 TargetBouncer Activeball, 1
 End Sub

 Sub Sw56_Dropped:dtLBank.Hit 5
 TargetBouncer Activeball, 1
 End Sub


 Sub Sw17_Dropped:dtRBank.Hit 1
 TargetBouncer Activeball, 1
 End Sub
 
 Sub Sw27_Dropped:dtRBank.Hit 2
 TargetBouncer Activeball, 1
 End Sub
 
 Sub Sw37_Dropped:dtRBank.Hit 3
 TargetBouncer Activeball, 1
 End Sub

 Sub Sw47_Dropped:dtRBank.Hit 4
 TargetBouncer Activeball, 1
 End Sub

 Sub Sw57_Dropped:dtRBank.Hit 5
 TargetBouncer Activeball, 1
 End Sub

 Sub Sw15_Dropped:dtTBank.Hit 1:End Sub


 'Stand Up Targets
Sub sw12_hit:vpmTimer.pulseSw 12
TargetBouncer Activeball, 1
End Sub
 
Sub sw22_hit:vpmTimer.pulseSw 22
TargetBouncer Activeball, 1
End Sub

Sub sw32_hit:vpmTimer.pulseSw 32
TargetBouncer Activeball, 1
End Sub

'Bullseye
 Sub sw33_Hit:vpmTimer.PulseSw 33:Me.TimerEnabled = 1:sw33p.Transz = 10 : playsound SoundFX("target",DOFTargets) : End Sub
 Sub sw33_timer:sw33p.Transz = 0:Me.TimerEnabled = 0: End Sub
 Sub sw43_Hit:vpmTimer.PulseSw 43:Me.TimerEnabled = 1:sw33p.Transz = 10 : playsound SoundFX("target",DOFTargets) : End Sub
 Sub sw43_timer:sw33p.Transz = 0:Me.TimerEnabled = 0: End Sub

'Spinners
Sub sw13_Spin:vpmTimer.PulseSw 13 : playsound"fx_spinner" : End Sub

'Wire Triggers
	Sub sw14_Hit:Controller.Switch(14) = 1 : End Sub 
	Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub
	Sub sw30_Hit:Controller.Switch(30) = 1 : End Sub 
	Sub sw30_UnHit:Controller.Switch(30) = 0:End Sub
	Sub sw31_Hit:Controller.Switch(31) = 1 : End Sub 
	Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub
	Sub sw34_Hit:Controller.Switch(34) = 1 : End Sub 
	Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub
	Sub sw42_Hit:Controller.Switch(42) = 1 : End Sub 
	Sub sw42_UnHit:Controller.Switch(42) = 0:End Sub
	Sub sw44_Hit:Controller.Switch(44) = 1 : End Sub 
	Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub
	Sub sw55_Hit:Controller.Switch(55) = 1 : End Sub 
	Sub sw55_UnHit:Controller.Switch(55) = 0:End Sub

'Star Triggers
	Sub sw20_Hit:Controller.Switch(20) = 1 : End Sub 
	Sub sw20_UnHit:Controller.Switch(20) = 0:End Sub
	Sub sw21_Hit:Controller.Switch(21) = 1 : End Sub 
	Sub sw21_UnHit:Controller.Switch(21) = 0:End Sub
	Sub sw23_Hit:Controller.Switch(23) = 1 : End Sub 
	Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
	Sub sw24_Hit:Controller.Switch(24) = 1 : End Sub 
	Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub

 'Scoring Rubber
Sub sw45a_hit:vpmTimer.pulseSw 45 : playsound"flip_hit_3" : End Sub 
Sub sw45b_hit:vpmTimer.pulseSw 45 : playsound"flip_hit_3" : End Sub 

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 11
    RandomSoundSlingshotRight SLING1
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
	vpmTimer.PulseSw 10
    RandomSoundSlingshotLeft SLING2
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
	NFadeL 0, L0
	NFadeL 1, L1
	NFadeL 2, L2
	NFadeL 3, L3
	NFadeL 4, L4
	NFadeL 5, L5
	NFadeL 6, L6
	NFadeL 7, L7
	NFadeL 10, L10
	NFadeL 11, L11
	NFadeL 12, L12
	NFadeL 13, L13
	NFadeL 14, L14
	NFadeL 15, L15
	NFadeL 16, L16
	NFadeL 17, L17
	NFadeL 20, L20
	NFadeL 21, L21
	NFadeL 22, L22
	NFadeL 23, L23
	NFadeL 24, L24
	NFadeL 25, L25
	NFadeL 26, L26
	NFadeL 27, L27
	NFadeL 30, L30
	NFadeL 31, L31
	NFadeL 32, L32
	NFadeL 33, L33
	NFadeL 34, L34
	NFadeL 35, L35
	NFadeL 36, L36
	NFadeL 37, L37
	NFadeL 40, L40
	NFadeL 41, L41
	NFadeL 42, L42
	NFadeL 43, L43
	NFadeL 44, L44
	NFadeL 45, L45
	NFadeL 46, L46
	NFadeL 47, L47
	NFadeL 50, L50
	NFadeL 51, L51
	NFadeL 52, L52
	NFadeL 53, L53
	NFadeL 54, L54
	NFadeL 55, L55
	NFadeL 60, L60
	NFadeL 61, L61
	NFadeL 62, L62
	NFadeL 63, L63
	NFadeL 64, L64
	NFadeL 65, L65
	NFadeL 70, L70
	NFadeL 71, L71
	NFadeL 72, L72
	NFadeL 73, L73
	NFadeL 74, L74
	NFadeL 75, L75
	NFadeL 76, L76
	NFadeL 80, L80
	NFadeL 81, L81
	NFadeL 82, L82
	NFadeL 83, L83
	NFadeL 84, L84
	NFadeL 85, L85
	NFadeL 86, L86
	NFadeL 87, L87

'Solenoid Controlled Lights


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

' *********************************************************************
'Fluppers Domes V2
' *********************************************************************
 
Sub FlashWhite1(flstate) 'White Domes
	If Flstate Then
		Objlevel(1) = 1 : FlasherFlash1_Timer
		PlaySound "fx_relay"
	End If
End Sub

Sub FlashWhite2(flstate) 'White Domes
	If Flstate Then
		Objlevel(2) = 1 : FlasherFlash2_Timer
		PlaySound "fx_relay"
	End If
End Sub

Sub FlashWhite3(flstate) 'White Domes
	If Flstate Then
		Objlevel(3) = 1 : FlasherFlash3_Timer
		PlaySound "fx_relay"
		Objlevel(9) = 1 : FlasherFlash9_Timer
		PlaySound "fx_relay"
	End If
End Sub

Sub FlashWhite4(flstate) 'White Domes
	If Flstate Then
		Objlevel(4) = 1 : FlasherFlash4_Timer
		PlaySound "fx_relay"
	End If
End Sub
 
Sub FlashWhite5(flstate) 'White Domes
	If Flstate Then
		Objlevel(5) = 1 : FlasherFlash5_Timer
		PlaySound "fx_relay"
		Objlevel(7) = 1 : FlasherFlash7_Timer
		PlaySound "fx_relay"
	End If
End Sub

Sub FlashWhite6(flstate) 'White Domes
	If Flstate Then
		Objlevel(6) = 1 : FlasherFlash6_Timer
		PlaySound "fx_relay"
		Objlevel(8) = 1 : FlasherFlash8_Timer
		PlaySound "fx_relay"
	End If
End Sub
  
Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness
 
' *********************************************************************
TestFlashers = 0 ' *** set this to 1 to check position of flasher object ***
Set TableRef = Table1 ' *** change this, if your table has another name ***
FlasherLightIntensity = 0.25 ' *** lower this, if the VPX lights are too bright (i.e. 0.1) ***
FlasherFlareIntensity = 0 ' *** lower this, if the flares are too bright (i.e. 0.1) ***
FlasherOffBrightness = 0.5 ' *** brightness of the flasher dome when switched off (range 0-2) ***
' *********************************************************************
 
 
Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = Table1.width : tableheight = Table1.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "white"
InitFlasher 2, "white"
InitFlasher 3, "white"
InitFlasher 4, "white"
InitFlasher 5, "white"
InitFlasher 6, "white"
InitFlasher 7, "white"
InitFlasher 8, "white"
InitFlasher 9, "white"

 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 11,90
 
Sub InitFlasher(nr, col)

	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)

	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ = atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If

	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness

	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col :
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select

	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If

	select case col
		Case "blue" : objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" : objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" : objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255)
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" : objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select

	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If

End Sub
 
Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub
 
Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 * FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting = FlasherOffBrightness + 10 * ObjLevel(nr)^3
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub
 
Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub
Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub
Sub FlasherFlash8_Timer() : FlashFlasher(8) : End Sub
Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub


'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
 Dim Digits(40)
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
 Digits(14)=Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
 Digits(15)=Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
 Digits(16)=Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
 Digits(17)=Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
 Digits(18)=Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
 Digits(19)=Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)

 Digits(20)=Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
 Digits(21)=Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
 Digits(22)=Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
 Digits(23)=Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
 Digits(24)=Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
 Digits(25)=Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
 Digits(26)=Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
 Digits(27)=Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
 Digits(28)=Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
 Digits(29)=Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
 Digits(30)=Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
 Digits(31)=Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)
 Digits(32)=Array(ac18, ac16, acc1, acd1, ac19, ac17, ac15, acf1, ac11, ac13, ac12, ac14, acb1, aca1, ac10, ace1)
 Digits(33)=Array(ad18, ad16, adc1, add1, ad19, ad17, ad15, adf1, ad11, ad13, ad12, ad14, adb1, ada1, ad10, ade1)
 Digits(34)=Array(ae18, ae16, aec1, aed1, ae19, ae17, ae15, aef1, ae11, ae13, ae12, ae14, aeb1, aea1, ae10, aee1)
 Digits(35)=Array(af18, af16, afc1, afd1, af19, af17, af15, aff1, af11, af13, af12, af14, afb1, afa1, af10, afe1)
 Digits(36)=Array(b9, b7, b0c1, b0d1, b100, b8, b6, b0f1, b2, b4, b3, b5, b0b1, b0a1, b1,b0e1)
 Digits(37)=Array(b109, b107, b1c1, b1d1, b110, b108, b106, b1f1, b102, b104, b103, b105, b1b1, b1a1, b101,b1e1)
 Digits(38)=Array(b119, b117, b2c1, b2d1, b120, b118, b116, b2f1, b112, b114, b113, b115, b2b1, b2a1, b111, b2e1)
 Digits(39)=Array(b129, b127, b3c1, b3d1, b130, b128, b126, b3f1, b122, b3b1, b123, b125, b3b1, b3a1, b121, b3e1)


 Sub DisplayTimer_Timer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
		If DesktopMode = True Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			if (num < 40) then
              For Each obj In Digits(num)
                   If chg And 1 Then obj.State=stat And 1
                   chg=chg\2 : stat=stat\2
                  Next
			Else
			       end if
        Next
	   end if
    End If
 End Sub
'**********************************************************************************************************

'******************************************************
'                BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

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
                StopSound("BallRoll_" & b)
        Next

        ' exit the sub if no balls on the table
        If UBound(BOT) = -1 Then Exit Sub

        ' play the rolling sound for each ball

        For b = 0 to UBound(BOT)
                If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
                        rolling(b) = True
                        PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

                Else
                        If rolling(b) = True Then
                                StopSound("BallRoll_" & b)
                                rolling(b) = False
                        End If
                End If

                '***Ball Drop Sounds***
                If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
                        If DropCount(b) >= 5 Then
                                DropCount(b) = 0
                                If BOT(b).velz > -7 Then
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

 
'*************
'   JP'S LUT
'*************
 
Dim bLutActive, LUTImage
Sub LoadLUT
Dim x
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub
 
Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub
 
Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 10: UpdateLUT: SaveLUT: End Sub
 
Sub UpdateLUT
Select Case LutImage
Case 0: table1.ColorGradeImage = "LUT0"
Case 1: table1.ColorGradeImage = "LUT1"
Case 2: table1.ColorGradeImage = "LUT2"
Case 3: table1.ColorGradeImage = "LUT3"
Case 4: table1.ColorGradeImage = "LUT4"
Case 5: table1.ColorGradeImage = "LUT5"
Case 6: table1.ColorGradeImage = "LUT6"
Case 7: table1.ColorGradeImage = "LUT7"
Case 8: table1.ColorGradeImage = "LUT8"
Case 9: table1.ColorGradeImage = "LUT9"
 
End Select
End Sub
 
 
'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************
 
sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperRSh1.RotZ = RightFlipper1.currentangle

	FlipperT3L.Roty = LeftFlipper.currentangle +240
	FlipperT3R.Roty = RightFlipper.currentangle +120
	FlipperT3R1.Roty = RightFlipper1.currentangle +145
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
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/21)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/21)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 4 
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub
 
 
 
'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'                  Start nFOZZY FLIPPERS'
'******************************************************
 
'Flipper Correction Initialization late 80’s to early 90’s
dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
 
InitPolarity
 
Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
        Next
 
        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5
        AddPt "Polarity", 2, 0.4, -5
        AddPt "Polarity", 3, 0.6, -4.5
        AddPt "Polarity", 4, 0.65, -4.0
        AddPt "Polarity", 5, 0.7, -3.5
        AddPt "Polarity", 6, 0.75, -3.0
        AddPt "Polarity", 7, 0.8, -2.5
        AddPt "Polarity", 8, 0.85, -2.0
        AddPt "Polarity", 9, 0.9,-1.5
        AddPt "Polarity", 10, 0.95, -1.0
        AddPt "Polarity", 11, 1, -0.5
        AddPt "Polarity", 12, 1.1, 0
        AddPt "Polarity", 13, 1.3, 0
 
        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945
 
        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub
 
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub
 
'******************************************************
'			FLIPPER CORRECTION FUNCTIONS
'******************************************************
 
Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub
 
Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
 
	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub
 
	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property	
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property
 
	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 
 
	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
			case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub
 
	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub
 
	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
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
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next		
	End Property
 
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
 
	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
 
			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if
 
			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next
 
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)						'find safety coefficient 'ycoef' data
			End If
 
			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
	 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
 
				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
 
				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If
 
			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
 
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'playsound "fx_knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class
 
'******************************************************
'		FLIPPER POLARITY AND RUBBER DAMPENER
'			SUPPORTING FUNCTIONS 
'******************************************************
 
' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
		if IsObject(a(x)) then 
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
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function
 
' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function
 
' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class
 
' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
 
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper
 
	LinearEnvelope = Y
End Function
 
' Used for drop targets and stand up targets
Function Atn2(dy, dx)
	dim pi
	pi = 4*Atn(1)
 
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function
 
' Used for drop targets and flipper tricks
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function
 
'******************************************************
'			FLIPPER TRICKS
'******************************************************
 
RightFlipper.timerinterval=1
Rightflipper.timerenabled=True
 
sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub
 
Dim LFEOSNudge, RFEOSNudge
 
Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim BOT, b
 
	If abs(Flipper1.currentangle) < abs(Endangle1) + 3 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		If Flipper2.currentangle = EndAngle2 Then 
			BOT = GetBalls
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
 					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					'debug.print "flippernudge!!"
					BOT(b).velx = BOT(b).velx /1.5
					BOT(b).vely = BOT(b).vely - 1
				end If
			Next
		End If
	Else 
		If abs(Flipper1.currentangle) > abs(Endangle1) + 30 then EOSNudge1 = 0
	End If
End Sub
 
'*****************
' Maths
'*****************
Const Pi = 3.1415927
 
Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function
 
Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function
 
 
'*************************************************
' Check ball distance from Flipper for Rem
'*************************************************
 
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function
 
Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function
 
Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function
 
Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function
 
Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function
 
Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
 
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If	
End Function
 
'*************************************************
' End - Check ball distance from Flipper for Rem
'*************************************************
 
 
 
dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle
 
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 1 '0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 4.5
	Case 2:
		SOSRampup = 8.5
End Select
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.035 '0.025
 
 
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
	Flipper.eostorque = EOST*EOSReturn/FReturn
 
 
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim BOT, b
		BOT = GetBalls
 
		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub
 
Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)	'-1 for Right Flipper
 
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime
 
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup			
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST	
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
 
	End If
End Sub
 
Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 117  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
 
Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce															'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount
 
	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.8 Then						'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)	'Partial catch when catch happens a bit late
		end If
 
		'debug.print "Live catch! Bounce: " & LiveCatchBounce
 
		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (16 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	End If
End Sub
 
'*****************************************************************************************************
'END nFOZZY FLIPPERS'
 
 
'PHYSICS DAMPENERS
 
'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR
 
Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub
 
Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub
 
 
dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least
 
dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85
 
Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 
 
	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub
 
	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
 
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub
 
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub
 
 
	Public Sub Report() 	'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub
 
 
End Class
 
'******************************************************
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker

	public ballvel, ballvelx, ballvely
 
	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

 
Sub RDampen_Timer()
Cor.Update
End Sub
 
'******************************************************
' TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled <> 0 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
    If aball.velx=0 then vratio=1 else vratio= aball.vely/aball.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.1*defvalue
			Case 2: zMultiplier = 0.2*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.5*defvalue
            Case 6: zMultiplier = 0.6*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
    end if
end sub

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1                                                                                                                'volume level; range [0, 1]
NudgeLeftSoundLevel = 1                                                                                                        'volume level; range [0, 1]
NudgeRightSoundLevel = 1                                                                                                'volume level; range [0, 1]
NudgeCenterSoundLevel = 1                                                                                                'volume level; range [0, 1]
StartButtonSoundLevel = 0.1                                                                                                'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr                                                                                        'volume level; range [0, 1]
PlungerPullSoundLevel = 1                                                                                                'volume level; range [0, 1]
RollingSoundFactor = 1.1/5                

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010                                                           'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635                                                                'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                                                                        'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                                                                      'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
SlingshotSoundLevel = 0.95                                                                                                'volume level; range [0, 1]
BumperSoundFactor = 4.25                                                                                                'volume multiplier; must not be zero
KnockerSoundLevel = 1                                                                                                         'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2                                                                        'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5                                                                                        'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5                                                                                        'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5                                                                                'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025                                                                        'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025                                                                        'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8                                                                        'volume level; range [0, 1]
WallImpactSoundFactor = 0.075                                                                                        'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5                                                                                                        'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10                                                                                        'volume multiplier; must not be zero
DTSoundLevel = 0.25                                                                                                                'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                                                                      'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8                                                                                                                'volume level; range [0, 1]
BallReleaseSoundLevel = 1                                                                                                'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2                                                                        'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015                                                                                'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5                                                                                                        'volume multiplier; must not be zero


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
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
    PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
        Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
        Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
        PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


' *********************************************************************
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************

'Dim tablewidth, tableheight : tablewidth = table1.width : tableheight = table1.height

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
        Dim tmp
    tmp = tableobj.y * 2 / tableheight-1
    If tmp > 0 Then
                AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
        Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
        Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
        VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
    PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
    RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
    RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
        PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
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
        PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
        PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
        PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
        PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub


'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
        PlaySoundAtLevelStatic SoundFXDOF("Bumpers_Middle_" & Int(Rnd*5)+1, 111, DOFPulse, DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
        PlaySoundAtLevelStatic SoundFXDOF("Bumpers_Bottom_" & Int(Rnd*5)+1, 112, DOFPulse, DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub


'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
        FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
        PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
        FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
                PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
        FlipperLeftHitParm = parm/10
        If FlipperLeftHitParm > 1 Then
                FlipperLeftHitParm = 1
        End If
        FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
        RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
        FlipperRightHitParm = parm/10
        If FlipperRightHitParm > 1 Then
                FlipperRightHitParm = 1
        End If
        FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
         RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
        PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
        PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
        RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 5 then                
                 RandomSoundRubberStrong 1
        End if
        If finalspeed <= 5 then
                 RandomSoundRubberWeak()
         End If        
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
        Select Case Int(Rnd*10)+1
                Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
        End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
        PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 5 then
                 RandomSoundRubberStrong 1 
        End if
        If finalspeed <= 5 then
                 RandomSoundRubberWeak()
         End If        
End Sub

Sub RandomSoundWall()
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 16 then 
                Select Case Int(Rnd*5)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                Select Case Int(Rnd*4)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
         End If
        If finalspeed < 6 Then
                Select Case Int(Rnd*3)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
        End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
        PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
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
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 16 then 
                PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                 Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                End Select
         End If
        If finalspeed < 6 Then
                 Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                End Select
        End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
        PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
        If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 16 then 
                 Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
                End Select
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
         End If
        If finalspeed < 6 Then
                PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
        End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
        PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()                
        PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 10 then
                 RandomSoundTargetHitStrong()
                RandomSoundBallBouncePlayfieldSoft Activeball
        Else 
                 RandomSoundTargetHitWeak()
         End If        
End Sub

Sub Targets_Hit (idx)
        PlayTargetSound        
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
        Select Case Int(Rnd*9)+1
                Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
                Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
                Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
                Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
                Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
                Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
        End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
        PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
        Select Case Int(Rnd*5)+1
                Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
        End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()                        
        PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
        PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
        SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)        
        SoundPlayfieldGate        
End Sub        

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
        PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
        PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
        If Activeball.velx > 1 Then SoundPlayfieldGate
        StopSound "Arch_L1"
        StopSound "Arch_L2"
        StopSound "Arch_L3"
        StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
        If activeball.velx < -8 Then
                RandomSoundRightArch
        End If
End Sub

Sub Arch2_hit()
        If Activeball.velx < 1 Then SoundPlayfieldGate
        StopSound "Arch_R1"
        StopSound "Arch_R2"
        StopSound "Arch_R3"
        StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
        If activeball.velx > 10 Then
                RandomSoundLeftArch
        End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
        PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
        Select Case scenario
                Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
                Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
        End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
        Dim snd
        Select Case Int(Rnd*7)+1
                Case 1 : snd = "Ball_Collide_1"
                Case 2 : snd = "Ball_Collide_2"
                Case 3 : snd = "Ball_Collide_3"
                Case 4 : snd = "Ball_Collide_4"
                Case 5 : snd = "Ball_Collide_5"
                Case 6 : snd = "Ball_Collide_6"
                Case 7 : snd = "Ball_Collide_7"
        End Select

        PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'/////////////////////////////////////////////////////////////////
'                                        End Mechanical Sounds
'/////////////////////////////////////////////////////////////////
 
' Thalamus : Exit in a clean and proper way
Sub Table1_exit()
  If B2SOn Then 
    Controller.Pause = False
    Controller.Stop
  End If
End Sub
