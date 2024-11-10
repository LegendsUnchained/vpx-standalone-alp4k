Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="solar_l2",UseSolenoids=2,UseLamps=0,UseGI=0, SCoin="coin3"


LoadVPM "01560000","S7.VBS",3.2


Const cCredits="Solar Fire - Williams 1981"

Dim DesktopMode: DesktopMode = Table1.ShowDT
If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Primitive007.visible=1
Else
Ramp16.visible=0
Ramp15.visible=0
Primitive007.visible=0
End if

'-----------------------------------
'------  Solenoid Assignment  ------
'-----------------------------------

SolCallback(1) = "bsTopEjectHole.Solout"				'Top Eject Hole
SolCallback(2) = "bsRightEjectHole.Solout"				'Bottom Right Eject Hole
SolCallback(3) = "bsLeftEjectHole.Solout"				'Bottom Left Eject Hole
SolCallback(4) = "DropTargetBank_BottomLeft.SolDropUp" 	'Bottom Left Drop Target Bank
SolCallback(5) = "DropTargetBank_BottomRight.SolDropUp"	'Bottom Right Drop Target Bank
SolCallback(6) = "DropTargetBank_TopRight.SolDropUp"	'Top Right Drop Target Bank
SolCallback(7) = "DropTarget4Bank_Left.SolDropUp" 		'Top 4 Bank Drop Target Bank - Left Targets
SolCallback(8) = "DropTarget4Bank_Right.SolDropUp" 		'Top 4 Bank Drop Target Bank - Right Targets
SolCallback(9) = "SolSolarGunLamps"						'Solar Gun Lamps
SolCallback(10) = "bsTrough.SolIn"						'Outhole
SolCallback(11) = "SolGI"        						'General Illumination
SolCallback(12) = "bsTrough.SolOut"						'BallRelease
'Sol13 Backbox Flasher
'Sol14 Backbox Flasher
SolCallback(15)  = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"	'Credit Knocker
'Sol16 Coin Lockout
'Sol17 Left Slingshot, Sound handled elsewhere
'Sol18 Right Slingshot, Sound handled elsewhere
SolCallback(19) = "SolSolarGun" 						'Sol19 Solar Gun
'Sol20 Left Magnet Relay, handled elsewhere
'Sol21 Right Magnet Relay, handled elsewhere
'SolCallback(25) = "vpmNudge.SolGameOn"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("flipperup",DOFFlippers):LeftFlipper.RotateToEnd:ULeftFlipper.RotateToEnd
     Else
         PlaySound SoundFX("flipperdown",DOFFlippers):LeftFlipper.RotateToStart:ULeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("flipperup",DOFFlippers):RightFlipper.RotateToEnd:URightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("flipperdown",DOFFlippers):RightFlipper.RotateToStart:URightFlipper.RotateToStart
     End If
End Sub 

'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************


Sub SolSolarGun(enabled)
	SolarGunHelper.isdropped = not enabled
End Sub

Sub SolSolarGunLamps(enabled)
	SetLamp 119, enabled
End Sub


Sub SolGI(enabled)
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"
	Else
		For each xx in GI:xx.State = 1: Next
        PlaySound "fx_relay"
	End If
End Sub

'--------------------------
'------  Table Init  ------
'--------------------------
Dim bsTrough,obj,bsTopEjectHole,bsRightEjectHole,bsLeftEjectHole,DropTargetBank_BottomLeft,DropTargetBank_BottomRight,DropTargetBank_TopRight,DropTarget4Bank_Left,DropTarget4Bank_Right
Dim mGunMagnet,mLeftMagnet,mRightMagnet,cCaptive

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Laser Cue Williams 1984"&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 1
		.Games("solar_l2").Settings.Value("sound") = 0
		.Games("solar_l2").Settings.Value("samples") = 0
        '.Games(cGameName).Settings.Value("sound")=1
		'.PuPHide = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

    PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled = true

    vpmNudge.TiltSwitch=1
    vpmNudge.Sensitivity=5
	vpmNudge.TiltObj = Array(LeftFlipper,RightFlipper,ULeftFlipper,URightFlipper) 
    

    Set bsTrough=New cvpmBallStack
        bsTrough.InitSw 20,21,22,23,0,0,0,0
        bsTrough.InitKick BallRelease,90,5
        bsTrough.InitExitSnd SoundFX("BallRel",DOFContactors),SoundFX("Solenoid",DOFContactors)
        bsTrough.Balls=3

    Set bsTopEjectHole=New cvpmBallStack       
        bsTopEjectHole.InitSaucer TopEjectHole,17,160,5
        bsTopEjectHole.InitExitSnd SoundFX("popper",DOFContactors),SoundFX("solon",DOFContactors)

	Set bsLeftEjectHole=New cvpmBallStack       
        bsLeftEjectHole.InitSaucer LeftEjectHole,19,180,5
        bsLeftEjectHole.InitExitSnd SoundFX("popper",DOFContactors),SoundFX("solon",DOFContactors)

    Set bsRightEjectHole=New cvpmBallStack       
        bsRightEjectHole.InitSaucer RightEjectHole,18,260,5
        bsRightEjectHole.InitExitSnd SoundFX("popper",DOFContactors),SoundFX("solon",DOFContactors)

	set DropTarget4Bank_Left = new cvpmDropTarget
		DropTarget4Bank_Left.InitDrop Array(Target36,Target37), Array(36,37)
		DropTarget4Bank_Left.InitSnd SoundFX("Targetdrop1",DOFDropTargets),SoundFX("TargetBankreset1",DOFContactors)
		DropTarget4Bank_Left.CreateEvents "DropTarget4Bank_Left"

	set DropTarget4Bank_Right = new cvpmDropTarget
		DropTarget4Bank_Right.InitDrop Array(Target38,Target39), Array(38,39)
		DropTarget4Bank_Right.InitSnd SoundFX("Targetdrop1",DOFDropTargets),SoundFX("TargetBankreset1",DOFContactors)
		DropTarget4Bank_Right.CreateEvents "DropTarget4Bank_Right"

	set DropTargetBank_BottomRight = new cvpmDropTarget
		DropTargetBank_BottomRight.InitDrop Array(Target30,Target31,Target32), Array(30,31,32)
		DropTargetBank_BottomRight.InitSnd SoundFX("Targetdrop1",DOFDropTargets),SoundFX("TargetBankreset1",DOFContactors)
		DropTargetBank_BottomRight.CreateEvents "DropTargetBank_BottomRight"

	set DropTargetBank_BottomLeft = new cvpmDropTarget
		DropTargetBank_BottomLeft.InitDrop Array(Target27,Target28,Target29), Array(27,28,29)
		DropTargetBank_BottomLeft.InitSnd SoundFX("Targetdrop1",DOFDropTargets),SoundFX("TargetBankreset1",DOFContactors)
		DropTargetBank_BottomLeft.CreateEvents "DropTargetBank_BottomLeft"

	set DropTargetBank_TopRight = new cvpmDropTarget
		DropTargetBank_TopRight.InitDrop Array(Target33,Target34,Target35), Array(33,34,35)
		DropTargetBank_TopRight.InitSnd SoundFX("Targetdrop1",DOFDropTargets),SoundFX("TargetBankreset1",DOFContactors)
		DropTargetBank_TopRight.CreateEvents "DropTargetBank_TopRight"

	Set mLeftMagnet=New cvpmMagnet
		mLeftMagnet.InitMagnet LeftMagnet,20
		mLeftMagnet.Solenoid=20
		mLeftMagnet.GrabCenter=False
		mLeftMagnet.CreateEvents "mLeftMagnet"

	Set mRightMagnet=New cvpmMagnet
		mRightMagnet.InitMagnet RightMagnet,20
		mRightMagnet.Solenoid=21
		mRightMagnet.GrabCenter=False
		mRightMagnet.CreateEvents "mRightMagnet"

	Set mGunMagnet=New cvpmMagnet
		mGunMagnet.InitMagnet GunMagnet,10
		mGunMagnet.Solenoid=19
		mGunMagnet.GrabCenter=False

	Set cCaptive=New cvpmCaptiveBall
		cCaptive.NailedBalls = 1
		cCaptive.InitCaptive SolarGunTrigger,SolarCannonWall,Array(CaptiveBallKicker1,CaptiveBallKicker2),10	
		CaptiveBallKicker1.createBall
		cCaptive.Start
		cCaptive.ForceTrans = 0.5
		cCaptive.MinForce = 3.5
		cCaptive.CreateEvents "cCaptive"
	


End Sub
'**********
' Music
'**********


'Start the music

DIM music

music = "pota.mp3"
PlayMusic "pota.mp3" 

Sub Table1_MusicDone()
PlayMusic "pota.mp3"
End Sub 

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************


Sub Table1_KeyDown(ByVal keycode)
	If keycode = LeftMagnaSave Then bLutActive = True
	If keycode = RightMagnaSave Then 
		If bLutActive Then NextLUT: End If
    End If
	If keycode = PlungerKey Then Plunger.PullBack:playsound"plungerpull"
	If keycode = LeftMagnaSave then Controller.Switch(9) = 1
	If keycode = RightMagnaSave then Controller.Switch(10) = 1
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If keycode = LeftMagnaSave Then bLutActive = False
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
	If keycode = LeftMagnaSave then Controller.Switch(9) = 0
	If keycode = RightMagnaSave then Controller.Switch(10) = 0
	If KeyUpHandler(keycode) Then Exit Sub
End Sub


'------------------------------
'------  Solar Gun       ------
'------------------------------

Sub GunMagnet_Hit:mGunMagnet.AddBall ActiveBall:End Sub
Sub GunMagnet_unHit:mGunMagnet.RemoveBall ActiveBall:End Sub

Sub Trigger42_Hit:Controller.Switch(42)=1:End Sub		'42 Gun Magnet
Sub Trigger42_unHit:Controller.Switch(42)=0:End Sub

' Button movement
Sub TriggerA_Hit
	TriggerA.Timerenabled = True
End Sub
Sub TriggerA_Timer
	TriggerA.Timerenabled = False
End Sub

Sub TriggerB_Hit
	if TriggerA.Timerenabled then
		MoveSolarGunTarget
	end if
End Sub

 '**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 12
    PlaySound SoundFX("RSling",DOFContactors), 0,1,0.13,0.1 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
'	gi1.State = 0:Gi2.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 11
    PlaySound SoundFX("LSling",DOFContactors), 0,1,-0.14,0.1 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
'	gi3.State = 0:Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub


'------------------------------
'------  Switch Handler  ------
'------------------------------


 ' Drain hole and kickers

Sub Drain_Hit:bsTrough.addball me : playsound"drain5" : End Sub


sub sw13_hit:Controller.Switch(13)=1:PlaySound "Gate2",0,0.1,-0.18,0.1:End Sub		'13
sub sw13_unhit:Controller.Switch(13)=0:End Sub
sub sw14_hit:Controller.Switch(14)=1:PlaySound "Gate2",0,0.1,0.17,0.1:End Sub		'14
sub sw14_unhit:Controller.Switch(14)=0:End Sub
sub sw15_hit:Controller.Switch(15)=1:PlaySound "Gate2",0,0.1,-0.16,0.1:End Sub		'15
sub sw15_unhit:Controller.Switch(15)=0:End Sub
sub sw16_hit:Controller.Switch(16)=1:PlaySound "Gate2",0,0.1,0.15,0.1:End Sub		'16
sub sw16_unhit:Controller.Switch(16)=0:End Sub
sub sw24_hit:Controller.Switch(24)=1:PlaySound "Gate2",0,0.1,0.18,0.1:End Sub		'24
sub sw24_unhit:Controller.Switch(24)=0:End Sub



Sub TopEjectHole_Hit:bsTopEjectHole.addball me:end Sub			'17
Sub RightEjectHole_Hit:bsRightEjectHole.addball me:end Sub		'18
Sub LeftEjectHole_Hit:bsLeftEjectHole.addball me:end Sub		'19

Sub Target25_Hit:vpmTimer.PulseSw 25:PlaySound SoundFX("Target",DOFTargets),0,0.1,0.15,0.1:End Sub '25

Sub Target27_Hit:DropTargetBank_BottomLeft.Hit 1:End Sub		'27
Sub Target28_Hit:DropTargetBank_BottomLeft.Hit 2:End Sub		'28
Sub Target29_Hit:DropTargetBank_BottomLeft.Hit 3:End Sub		'29

Sub Target30_Hit:DropTargetBank_BottomRight.Hit 1:End Sub		'30
Sub Target31_Hit:DropTargetBank_BottomRight.Hit 2:End Sub		'31
Sub Target32_Hit:DropTargetBank_BottomRight.Hit 3:End Sub		'32

Sub Target33_Hit:DropTargetBank_TopRight.Hit 1:End Sub		'33
Sub Target34_Hit:DropTargetBank_TopRight.Hit 2:End Sub		'34
Sub Target35_Hit:DropTargetBank_TopRight.Hit 3:End Sub		'35

Sub Target36_Hit:DropTarget4Bank_Left.Hit 1:End Sub		'36
Sub Target37_Hit:DropTarget4Bank_Left.Hit 2:End Sub		'37

Sub Target38_Hit:DropTarget4Bank_Right.Hit 1:End Sub		'38
Sub Target39_Hit:DropTarget4Bank_Right.Hit 2:End Sub		'39

sub Trigger40_hit:Controller.Switch(40)=1:PlaySound "Gate2",0,0.1,-0.18,0.1:End Sub		'40
sub Trigger40_unhit:Controller.Switch(40)=0:End Sub
Sub Target41_Hit:vpmTimer.PulseSw 41:PlaySound "Target",0,0.1,-0.15,0.1:End Sub


'--------------------------------
'------  Helper Functions  ------
'--------------------------------

Sub Gate2_Hit:vpmTimer.pulseSw 26 :PlaySound "Gate5",0,1,0.13,0.1:End Sub

Sub Gate3_Hit:PlaySound "Gate5",0,1,0.13,0.1:End Sub


Dim ScoopVel
Sub ShooterLaneTop_Hit
	ScoopVel = Activeball.vely
	if ScoopVel < -5 then
		ShooterLaneTop.destroyball
		UpperPFEnter.createBall
		UpperPFEnter.kick 180,-ScoopVel*0.25
	end if
End Sub


'Round Targets

Sub MoveSolarGunTarget
	P_SolarGunTarget.Transy = 5
	SolarGunTrigger.Timerenabled = False
	SolarGunTrigger.Timerenabled = True
End Sub
Sub SolarGunTrigger_Timer
	SolarGunTrigger.Timerenabled = False
	P_SolarGunTarget.Transy = 0
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

Sub UpdateLamps()

    NFadeL 2, l2  'Ball in Play Apron
    NFadeL 7, l7
    NFadeL 8, l8
    NFadeL 9, l9
    NFadeL 10, l10
    NFadeL 11, l11
    NFadeL 12, l12
    NFadeL 13, l13
    NFadeL 14, l14
    NFadeL 15, l15
'    NFadeL 16, l16  'Not used
    NFadeL 17, l17
    NFadeL 18, l18
    NFadeL 19, l19
    NFadeL 20, l20
    NFadeL 21, l21
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
    NFadeL 33, l33
    NFadeL 34, l34
    NFadeL 35, l35
    NFadeL 36, l36
    NFadeL 37, l37
    NFadeL 38, l38
    NFadeL 39, l39
    NFadeL 40, l40
    NFadeL 41, l41
    NFadeL 42, l42
    NFadeL 43, l43
    NFadeL 44, l44
    NFadeL 45, l45
    NFadeL 46, l46
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
    NFadeL 60, l60
    NFadeL 61, l61
    NFadeL 62, l62
    NFadeL 63, l63

'Solenid Controlled Lamps

   NFadeLm 119, S119
   NFadeL 119, S119a
   NFadeL 119, S119b
   NFadeL 119, S119c

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

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************

Dim Digits(32)
' 1st Player
Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)
Digits(6) = Array(LED70,LED71,LED72,LED73,LED74,LED75,LED76)

' 2nd Player
Digits(7) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(8) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(9) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(10) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(11) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(12) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)
Digits(13) = Array(LED140,LED141,LED142,LED143,LED144,LED145,LED146)

' 3rd Player
Digits(14) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(15) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(16) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(17) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(18) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(19) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)
Digits(20) = Array(LED210,LED211,LED212,LED213,LED214,LED215,LED216)

' 4th Player
Digits(21) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(22) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(23) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(24) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(25) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(26) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)
Digits(27) = Array(LED280,LED281,LED282,LED283,LED284,LED285,LED286)

' Credits
Digits(28) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(29) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)
' Balls
Digits(30) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(31) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)

Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		If DesktopMode = True Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 32) then
				For Each obj In Digits(num)
					If chg And 1 Then obj.State = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			else
			end if
		next
		end if
end if
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
        StopSound "fx_ballrolling" & b
        StopSound "fx_plasticrolling" & b
        StopSound "fx_metalrolling" & b
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
		If BallVel(BOT(b) ) > 1 Then
            rolling(b) = True
			If BOT(b).Z > 30 Then
				' ball on plastic ramp
				StopSound "fx_ballrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_plasticrolling" & b, -1, Vol(BOT(b)) / 2, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			Else
				' ball on playfield
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_ballrolling" & b, -1, Vol(BOT(b)) / 2, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			End If
		Else
			If rolling(b) Then
                StopSound "fx_ballrolling" & b
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
                rolling(b) = False
            End If
		End If
		
		'   ball drop sounds matching the adjusted height params but not the way down the ramps
		If BOT(b).VelZ < -1 And BOT(b).Z < 55 And BOT(b).Z > 27 then 'And Not InRect(BOT(b).X, BOT(b).Y, 610,320, 740,320, 740,550, 610,550) And Not InRect(BOT(b).X, BOT(b).Y, 180,400, 230,400, 230, 550, 180,550) Then
			PlaySound "fx_ball_drop" & Int(Rnd()*3), 0, ABS(BOT(b).VelZ)/17*5, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
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
	FlipperLSh1.RotZ = ULeftFlipper.currentangle
	FlipperRSh1.RotZ = URightFlipper.currentangle

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
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
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

Sub Table1_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub
'******************
' Rom sound ON wwhen close table
'******************
Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub