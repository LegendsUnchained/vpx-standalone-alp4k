Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="SteveMiller",UseSolenoids=2,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_Flipperup",SFlipperOff="fx_Flipperdown"
Const SCoin="coin"

'Dim PlayMusicOnStartup
'Dim MusicOption1
'Dim MusicOption2


'+++++++++++++++++++++++
'+    TABLE OPTIONS    +
'+++++++++++++++++++++++
'-------------------------------------------------------------------------------------------------------------
'MusicOption1 = 1		'1 = Gameplay Music ON   0 = Gameplay Music OFF
'MusicOption2 = 1		'1 = Random Gameplay Music ON   0 = Random Gameplay Music OFF

'MusicOption1 MUST BE SET TO 1 in order to also use the MusicOption2 Random feature
'-------------------------------------------------------------------------------------------------------------
'PlayMusicOnStartup = 0      '1 = Start Music on Table Startup    0 = Start Music Manually
'-------------------------------------------------------------------------------------------------------------
'************************
'Glowball
'*************************
Dim GlowAura,GlowIntensity
'Dim musicEnd

Const ChooseBall 			= 0	' *** Ball Settings **********
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

GlowAura=125 'GlowBlob Auroa radius
GlowIntensity=3 'Glowblob intensity

'*************************************************************

Dim cNewController

'*********************
'Shadows Layes
'*********************

Const showShadow1 = 1		' 0=Off, 1=ON - Show Shadow1
Const showShadow2 = 0		' 0=Off, 1=ON - Show Shadow2

'///////////////////////-----VR Options-----///////////////////////
Const VRRoomChoice = 1 '1 = Mega Room, 0 = Minimal Room

'----- VR Room Auto-Detect -----
Dim VRRoom, VR_Obj, VRMode
Const VRTest = 0				' 1 = Testing VR in Live View, 0 = Do not force VR mode.

If RenderingMode = 2 or VRTest = 1 Then
	ramp001.visible = 0
	ramp002.visible = 0
	ramp15.visible = 0
	ramp16.visible = 0
	BLADL.visible = 0
	BLADL001.visible = 0
	Primitive13.visible = 0
	Wall007.visible = 0
	Wall007.sidevisible = 0
	VRMode = True
	For Each VR_Obj in VRStuff : VR_Obj.Visible = 1 : Next
Else
	VRMode = False
	For Each VR_Obj in VRStuff : VR_Obj.Visible = 0 : Next
End If

LoadVPM "01130100", "Bally.VBS", 3.21

'******************************************************
'  ZTIM: Timers
'******************************************************

'The FrameTimer interval should be -1, so executes at the display frame rate
'The frame timer should be used to update anything visual, like some animations, shadows, etc.
'However, a lot of animations will be handled in their respective _animate subroutines.

Dim FrameTime, InitFrameTime
InitFrameTime = 0

FrameTimer.Interval = -1
Sub FrameTimer_Timer() 
	FrameTime = gametime - InitFrameTime 'Calculate FrameTime as some animuations could use this
	InitFrameTime = gametime	'Count frametime
	'Add animation stuff here


    If VRMode = False Then
        DisplayTimer
    End If

	If VRMode = True Then
		VRDisplayTimer
	End If

End Sub


'Flipper Rampup mode: 0 = fast, 1 = medium, 2 = slow (tap passes should work)
dim FlipperCoilRampupMode : FlipperCoilRampupMode = 1

Dim LUTimage

Sub Table1_OptionEvent(ByVal eventId)
    ' -- pause processes not needed to run --
    If eventId = 1 Then DisableStaticPreRendering = True
    Controller.Pause = True

    ' --- Table LUT --- 
    LUTimage = Table1.Option("Table Color Setting (LUT)", 0, 15, 1, 0, 0, Array("New ColorLUT","Normal","Warm Dark","Warm Bright","Warm Vivid Soft","Warm Vivid Hard","Natural Balanced","Natural High Contrast","THX Standard","Punchy","Desaturated","Washed Out","Natural Dark","Bassgeige","Blacklight","B&W Comic Book"))
    Table1.ColorGradeImage = "LUT" & LUTimage

    ' -- start paused processes --
    Controller.Pause = False
    If eventId = 3 Then DisableStaticPreRendering = False
End Sub

Dim DesktopMode: DesktopMode = Table1.ShowDT
If DesktopMode = True Then 'Show Desktop components
  Ramp16.visible=1
  Ramp15.visible=1
  Primitive13.visible=1
Else
  Ramp16.visible=0
  Ramp15.visible=0
  Primitive13.visible=1
  Primitive13.size_y=250
End if

dim countrSL
countrSL = 1
pegtimer.enabled = 1
pegtimer.enabled = False
pegTimerstop.enabled = False
'Solenoid Call backs
'**********************************************************************************************************
 SolCallback(6)		= "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
 SolCallback(7)     = "bsTrough.SolOut"			
 SolCallback(13)    = "UpKicker"
 SolCallback(14)    = "KickAndDown"
 
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFx("fx_Flipperup",DOFFlippers)
        LF.FIRE
     Else
         PlaySound SoundFx("fx_Flipperdown",DOFFlippers):LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFx("fx_Flipperup",DOFFlippers)
        RF.FIRE
     Else
         PlaySound SoundFx("fx_Flipperdown",DOFFlippers):RightFlipper.RotateToStart
     End If
End Sub

'fantasy bumper flashers
' SolCallback(8) ="vpmFlasher array(Flasher8,Flasher8a),"
' SolCallback(9) ="vpmFlasher array(Flasher9,Flasher9a),"
' SolCallback(10)="vpmFlasher array(Flasher10,Flasher10a),"
'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************
 dim NewlySet : NewlySet = False
 Sub KickAndDown(enabled)
 	If enabled Then
 		If NewlySet = True Then : KickIM.AutoFire : End If
 		KickPost.IsDropped = True
 		NewlySet = False
 	End If
 End Sub
 
 Sub UpKicker(enabled)
 	If enabled Then
 		KickPost.IsDropped = False
 		NewlySet = True
 	End If
 End Sub

'*****GI Lights On
dim xx

For each xx in GI:xx.State = 1: Next

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************
Dim bsTrough, KickIM

Sub Table1_Init
	vpmInit Me
    MusicOn
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Pharaoh (Williams)"&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
        .Games("slbmanib").Settings.Value("sound") = 0
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
		Controller.SolMask(0)=0
      vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
		Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

	vpmNudge.TiltSwitch  = 7
	vpmNudge.Sensitivity = 5
	vpmNudge.Tiltobj = Array(LeftSlingshot,RightSlingshot,Bumper1,Bumper2,Bumper3)

	Set bsTrough = New cvpmBallStack
	bsTrough.Initsw 0,8,0,0,0,0,0,0
	bsTrough.InitKick BallRelease, 55, 4
	bsTrough.InitExitSnd SoundFX("BallRelease",DOFContactors),SoundFX("solenoid",DOFContactors)
	bsTrough.Balls = 1

	Set KickIM = New cvpmImpulseP
	KickIM.InitImpulseP KickIt, 30, 0
	KickIM.Random 1
 	KickIM.Switch 1
	KickIM.InitExitSnd SoundFX("BallRelease",DOFContactors),SoundFX("solenoid",DOFContactors)
	KickIM.CreateEvents "KickIM"

    'If PlayMusicOnStartup = 1 then
      'NextTrack
	'End If
	
  Glowball_Init 'Start Glowballs
' Choose Shadow 

	if showShadow1=0 Then
		Shadow1.visible = False 
	End if 

	if showShadow2=0 Then
		Shadow2.visible = False 
	End if 

 	if VRMode = True Then
		setup_backglass()
	End If

 End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************
'*********************
' VR Plunger code
'*********************

Sub TimerVRPlunger_Timer
  If VR_Shooter.Y < 86 then
  		VR_Shooter.Y = VR_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
	VR_Shooter.Y = -4 + (5* Plunger.Position) -20
End Sub

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyCode = PlungerKey Then
		TimerVRPlunger.Enabled = True
		TimerVRPlunger1.Enabled = False
		VR_Shooter.Y = -4
	End If

	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.X = 2112 + 6
	End If
	If keycode = RightFlipperKey Then
		VRFlipperButtonRight.X = 2094 - 6
	End If

	If keycode = StartGameKey Then
		StartButton.y = 1916 - 5
	End If
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, lfpress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, rfpress
    If keycode = LeftMagnaSave Then EndMusic
	If keycode = RightMagnaSave Then MusicOn()
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
End Sub

'******

Sub Table1_KeyUp(ByVal KeyCode)

	If keycode = LeftFlipperKey Then FlipperDeactivate LeftFlipper, lfpress
	If keycode = RightFlipperKey Then FlipperDeactivate RightFlipper, rfpress

	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then 
		Plunger.Fire:PlaySound"plunger"
		TimerVRPlunger.Enabled = False
		TimerVRPlunger1.Enabled = True
		VR_Shooter.Y = -4
	End If

	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.X = 2112
	End If

	If keycode = RightFlipperKey Then
		VRFlipperButtonRight.X = 2094
	End If

	If keycode = StartGameKey Then
		StartButton.y = 1916
	End If
End Sub


'**********************************************************************************************************
' Switch handling
'**********************************************************************************************************
'**********************************************************************************************************

 ' Drain hole
Sub Drain_Hit
Playsound"drain_1"
bsTrough.addball me
StopSpots
End Sub

'Wire Triggers
Sub sw3_Hit:Controller.Switch(3)=1 : playsound"rollover" : End Sub 
Sub sw3_unHit:Controller.Switch(3)=0:End Sub
Sub sw4_Hit:Controller.Switch(4)=1 : playsound"rollover" : End Sub 
Sub sw4_unHit:Controller.Switch(4)=0:End Sub
Sub sw5_Hit:Controller.Switch(5)=1 : playsound"rollover" : End Sub 
Sub sw5_unHit:Controller.Switch(5)=0:End Sub
Sub sw26_Hit:Controller.Switch(26)=1 : playsound"rollover" : End Sub 
Sub sw26_unHit:Controller.Switch(26)=0:End Sub
Sub sw27_Hit:Controller.Switch(27)=1 : playsound"rollover" : End Sub 
Sub sw27_unHit:Controller.Switch(27)=0:End Sub
Sub sw29_Hit:Controller.Switch(29)=1 : playsound"rollover" : End Sub 
Sub sw29_unHit:Controller.Switch(29)=0:End Sub
Sub sw30_Hit:Controller.Switch(30)=1 : playsound"rollover" : End Sub 
Sub sw30_unHit:Controller.Switch(30)=0:End Sub

'Star Triggers
Sub sw2a_Hit:Controller.Switch(2)=1 : playsound"rollover" : End Sub 
Sub sw2a_unHit:Controller.Switch(2)=0:End Sub
Sub sw2b_Hit:Controller.Switch(2)=1 : playsound"rollover" : End Sub 
Sub sw2b_unHit:Controller.Switch(2)=0:End Sub

'scoring rubbers
Sub sw34e_Hit():vpmtimer.pulsesw 34 :  End Sub
Sub sw34f_Hit():vpmtimer.pulsesw 34 :  End Sub
Sub sw34g_Hit():vpmtimer.pulsesw 34 :  End Sub
Sub sw34h_Hit():vpmtimer.pulsesw 34 :  End Sub

'Spinners
Sub sw25_Spin:vpmTimer.PulseSw 25 : playsound"fx_spinner" : End Sub
Sub sw33_Spin:vpmTimer.PulseSw 33 : playsound"fx_spinner" : End Sub

'Stand Up Targets
Sub sw17_Hit():vpmtimer.pulsesw 17: PlaySound SoundFX("Target",DOFTargets),0,1,-0.15,0.1:End Sub
Sub sw18_Hit():vpmtimer.PulseSw 18: PlaySound SoundFX("Target",DOFTargets),0,1,-0.15,0.1:End Sub 
Sub sw19_Hit():vpmtimer.PulseSw 19: PlaySound SoundFX("Target",DOFTargets),0,1,-0.15,0.1:End Sub
Sub sw20_Hit():vpmtimer.PulseSw 20: PlaySound SoundFX("Target",DOFTargets),0,1,-0.15,0.1:End Sub
Sub sw21_Hit():vpmtimer.PulseSw 21: PlaySound SoundFX("Target",DOFTargets),0,1,-0.15,0.1:End Sub
Sub sw22_Hit():vpmtimer.PulseSw 22: PlaySound SoundFX("Target",DOFTargets),0,1,-0.15,0.1:End Sub 
Sub sw23_Hit():vpmtimer.PulseSw 23: PlaySound SoundFX("Target",DOFTargets),0,1,-0.15,0.1:End Sub 
Sub sw24_Hit():vpmtimer.PulseSw 24: PlaySound SoundFX("Target",DOFTargets),0,1,-0.15,0.1:End Sub 
Sub sw31_Hit():vpmtimer.PulseSw 31: PlaySound SoundFX("Target",DOFTargets),0,1,-0.15,0.1:End Sub 
Sub sw32_Hit():vpmtimer.PulseSw 32: PlaySound SoundFX("Target",DOFTargets),0,1,-0.15,0.1:End Sub 

Sub sw28_Hit():vpmtimer.PulseSw 28
 StartSpots
 PlaySound SoundFX("Target",DOFTargets),0,1,-0.15,0.1 
 If   Pegtimer.enabled = false Then
       Pegtimer.enabled = True
       Pegtimerstop.enabled = True
	End If
End Sub



'Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw(38) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw(39) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw(40) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub

Sub sw34a_Hit:vpmtimer.pulsesw(34) : End Sub
Sub sw34b_Hit:vpmtimer.pulsesw(34) : End Sub
Sub sw34c_Hit:vpmtimer.pulsesw(34) : End Sub
Sub sw34d_Hit:vpmtimer.pulsesw(34) : End Sub

'Gate 

Sub Gate1_Hit():PlaySound "Gate5":End Sub
Sub Gate2_Hit():PlaySound "Gate5":End Sub
Sub Gate3_Hit():PlaySound "Gate5":End Sub


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 36
    If Spots.Enabled = 0 Then FlashForms spot1, 1000, 50, 0:FlashForms spot2, 1000, 50, 0
    PlaySound SoundFX("left_slingshot",DOFContactors), 0, 1, 0.05, 0.05
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
	vpmTimer.PulseSw 37
    If Spots.Enabled = 0 Then FlashForms spot1, 1000, 50, 0:FlashForms spot2, 1000, 50, 0
    PlaySound SoundFX("right_slingshot",DOFContactors),0,1,-0.05,0.05
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

'Lights

Set Lights(1)=Light1
Set Lights(2)=Light2
Set Lights(3)=Light3
Set Lights(4)=Light4
Set Lights(5)=Light5
Set Lights(6)=Light6
Set Lights(7)=Light7
Set Lights(8)=Light8
Set Lights(9)=Light9
Set Lights(10)=Light10
Set Lights(11)=Light11
Set Lights(17)=Light17
Set Lights(18)=Light18
Set Lights(19)=Light19
Set Lights(20)=Light20
Set Lights(21)=Light21
Set Lights(22)=Light22
Set Lights(23)=Light23
Set Lights(24)=Light24
Lights(25)=array(Light25,Light25a)
Set Lights(26)=Light26
Set Lights(33)=Light33
Set Lights(34)=Light34
Set Lights(35)=Light35
Set Lights(36)=Light36
Set Lights(37)=Light37
Set Lights(38)=Light38
Set Lights(39)=Light39
Set Lights(40)=Light40
Set Lights(41)=Light41
Set Lights(42)=Light42
Set Lights(43)=Light43
Set Lights(44)=Light44
Set Lights(49)=Light49
Set Lights(50)=Light50
Set Lights(51)=Light51
Lights(52)=array(Light52,Light52a)
Set Lights(53)=Light53
Set Lights(54)=Light54
Set Lights(55)=Light55
Set Lights(56)=Light56
Lights(57)=array(Light57,Light57a)
Set Lights(58)=Light58
Set Lights(59)=Light59
Set Lights(60)=Light60
'Backglass
'Set Lights(11)=	' Shoot Again
'Set Lights(13)=	' Ball In Play
'Set Lights(27)=	' Match
'Set Lights(29)=	' High Score To Date
'Set Lights(45)=	' Game Over
'Set Lights(61)=	' Tilt
 'Silverball on Backglass
'Set Lights(14)=	' "S"
'Set Lights(30)=	' "I"
'Set Lights(46)=	' "L"
'Set Lights(62)=	' "V"
'Set Lights(15)=	' "E"
'Set Lights(31)=	' "R"
'Set Lights(47)=	' "B"
'Set Lights(63)=	' "A"
'Set Lights(12)=	' "L"
'Set Lights(28)=	' "L"

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

Sub DisplayTimer
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

'******************************************************
' VR Backglass Digital Display
'******************************************************

Dim xoff,yoff1, yoff2, yoff3, yoff4, yoff5, zoff,xrot,zscale, xcen,ycen

Sub Setup_Backglass()

	xoff = -20
	yoff1 = -65 ' this is where you adjust the forward/backward position for player 1 score
	yoff2 = -65 ' this is where you adjust the forward/backward position for player 2 score
	yoff3 = -65 ' this is where you adjust the forward/backward position for player 3 score
	yoff4 = -65 ' this is where you adjust the forward/backward position for player 4 score
	yoff5 = -65 ' this is where you adjust the forward/backward position for credits and ball in play
	zoff = 638
	xrot = -90

	CenterVRDigits()

End Sub

Sub CenterVRDigits()
	Dim ix, xx, yy, yfact, xfact, xobj

	zscale = 0.0000001

	xcen = (130 /2) - (92 / 2)
	ycen = (780 /2 ) + (203 /2)

	for ix = 0 to 6
		For Each xobj In VRDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff1 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 7 to 13
		For Each xobj In VRDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff2 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 14 to 20
		For Each xobj In VRDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff3 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 21 to 27
		For Each xobj In VRDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff4 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 28 to 31
		For Each xobj In VRDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff5 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

End Sub

Dim VRDigits(32)
VRDigits(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6,led1x7)
VRDigits(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6)
VRDigits(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6)
VRDigits(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6,led4x7)
VRDigits(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6)
VRDigits(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6)
VRDigits(6) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6)

VRDigits(7) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6,led8x7)
VRDigits(8) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6)
VRDigits(9) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6)
VRDigits(10) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6,led11x7)
VRDigits(11) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6)
VRDigits(12) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6)
VRDigits(13) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6)

VRDigits(14) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006,LED1x007)
VRDigits(15) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106)
VRDigits(16) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206)
VRDigits(17) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306,LED1x307)
VRDigits(18) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406)
VRDigits(19) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506)
VRDigits(20) = Array(LED1x600,LED1x601,LED1x602,LED1x603,LED1x604,LED1x605,LED1x606)

VRDigits(21) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006,led2x007)
VRDigits(22) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106)
VRDigits(23) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206)
VRDigits(24) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306,led2x307)
VRDigits(25) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406)
VRDigits(26) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506)
VRDigits(27) = Array(LED2x600,LED2x601,LED2x602,LED2x603,LED2x604,LED2x605,LED2x606)

VRDigits(28) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306)
VRDigits(29) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406)
VRDigits(30) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506)
VRDigits(31) = Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606)

dim DisplayColor

DisplayColor =  RGB(255,40,1)

Sub VRDisplayTimer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			if (num < 34) then
              For Each obj In VRDigits(num)
'                   If chg And 1 Then obj.visible=stat And 1
				   If chg And 1 Then FadeDisplay obj, stat And 1	
                   chg=chg\2 : stat=stat\2
              Next
			Else
			       end if
        Next
    End If
 End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 12
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 8
	End If
End Sub

Sub InitVRDigits()
	dim tmp, x, obj
	for x = 0 to uBound(VRDigits)
		if IsArray(VRDigits(x) ) then
			For each obj in VRDigits(x)
				obj.height = obj.height + 18
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

InitVRDigits
'**********************************************************************************************************
'**********************************************************************************************************

'Bally Silverball Mania 7 digits
'Added by Inkochnito
Sub editDips
	Dim vpmDips:Set vpmDips=New cvpmDips
	With vpmDips
		.AddForm 415,400,"Silverball Mania 7 digits - DIP switches"
		.AddFrame 2,0,190,"Maximum credits",&H03000000,Array("10 credits",0,"15 credits",&H01000000,"25 credits",&H02000000,"free play (40 credits)",&H03000000)'dip 25&26
		.AddFrame 2,76,190,"Sound features",&H30000000,Array("chime effects",0,"no background noises",&H10000000,"noise effects",&H20000000,"background noises",&H30000000)'dip 29&30
		.AddFrame 2,152,190,"High score to date",&H00200000,Array("no award",0,"3 credits",&H00200000)'dip 22
		.AddFrame 2,200,190,"Score version",&H00100000,Array("6 digit scoring",0,"7 digit scoring",&H00100000)'dip 21
		.AddFrame 2,248,190,"Special/extra ball modes",&H00000060,Array("points",0,"extra ball",&H00000040,"replay/extra ball",&H00000060)'dip6&7
		.AddFrame 210,0,190,"Balls per game",&H40000000,Array("3 balls",0,"5 balls",&H40000000)'dip 31
		.AddFrame 210,46,190,"Carryover award",&H00000080,Array("1 credit",0,"3 credits",&H00000080)'dip 8
		.AddFrame 210,92,190,"Carryover advance",&H00004000,Array("advance on SBM special",&H00004000,"advance on kicker special",0)'dip 15
		.AddFrame 210,138,190,"Extra ball lites",&H80000000,Array("together with 5X bonus",&H80000000,"after 5X bonus",0)'dip32
		.AddFrame 210,184,190,"Kicker special lites",32768,Array("together with SBM special",32768,"after awarding SBM special",0)'dip 16
		.AddFrame 210,230,190,"Center hoop advances",&H00800000,Array("1 letter",0,"2 letters",&H00800000)'dip24
		.AddChk 210,280,190,Array("Match feature",&H08000000)'dip 28
		.AddChk 210,295,190,Array("Credits displayed",&H04000000)'dip 27
		.AddChk 210,310,200,Array("Silverball (backglass) carryover feature",&H00400000)'dip 23
		.AddLabel 50,330,340,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub
Set vpmShowDips=GetRef("editDips")


'**********************************************************************************************************
'**********************************************************************************************************


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




'**************************************
'*         Music Mod: HiRez00         *
'**************************************

'Sub NextTrack
    'If MusicOption1 = 0 Then Exit Sub 
    'If MusicOption1 = 1 and MusicOption2 = 0 then OrderTrack: End If
    'If MusicOption2 = 1 and MusicOption1 = 1 then RandomTrack: End If
'End Sub

'***********************************************
'*         Playlist Music Mod: HiRez00         *
'***********************************************

'Dim musicNum
'Sub NextTrack

'If musicNum = 0 Then PlayMusic "STMILLERB/Space Intro.mp3" End If
'If musicNum = 1 Then PlayMusic "STMILLERB/Abracadabra.mp3" End If
'If musicNum = 2 Then PlayMusic "STMILLERB/Swingtown.mp3" End If
'If musicNum = 3 Then PlayMusic "STMILLERB/The Joker.mp3" End If
'If musicNum = 4 Then PlayMusic "STMILLERB/Fly Like An Eagle.mp3" End If
'If musicNum = 5 Then PlayMusic "STMILLERB/Winter Time.mp3" End If
'If musicNum = 6 Then PlayMusic "STMILLERB/Jungle Love.mp3" End If
'If musicNum = 7 Then PlayMusic "STMILLERB/Jet Airliner.mp3" End If
'If musicNum = 8 Then PlayMusic "STMILLERB/Rock_n Me.mp3" End If
'If musicNum = 9 Then PlayMusic "STMILLERB/Take The Money and Run.mp3" End If
'If musicNum = 10 Then PlayMusic "STMILLERB/True Fine Love.mp3" End If
'If musicNum = 11 Then PlayMusic "STMILLERB/I Love You.mp3" End If
'If musicNum = 12 Then PlayMusic "STMILLERB/Jackson-Kent Blues.mp3" End If
'If musicNum = 13 Then PlayMusic "STMILLERB/Kow Kow Calqulator.mp3" End If
'If musicNum = 14 Then PlayMusic "STMILLERB/Little Girl.mp3" End If
'If musicNum = 15 Then PlayMusic "STMILLERB/Living in the U.S.A..mp3" End If
'If musicNum = 16 Then PlayMusic "STMILLERB/Mercury Blues.mp3" End If
'If musicNum = 17 Then PlayMusic "STMILLERB/My Dark Hour.mp3" End If
'If musicNum = 18 Then PlayMusic "STMILLERB/Quicksilver Girl.mp3" End If
'If musicNum = 19 Then PlayMusic "STMILLERB/Gangster Of Love.mp3" End If
'If musicNum = 20 Then PlayMusic "STMILLERB/Seasons.mp3" End If
'If musicNum = 21 Then PlayMusic "STMILLERB/Serenade.mp3" End If
'If musicNum = 22 Then PlayMusic "STMILLERB/Shu Ba Da Du Ma Ma Ma Ma.mp3" End If
'If musicNum = 23 Then PlayMusic "STMILLERB/Song for Our Ancestors.mp3" End If
'If musicNum = 24 Then PlayMusic "STMILLERB/Space Cowboy.mp3" End If
'If musicNum = 25 Then PlayMusic "STMILLERB/Sweet Maree.mp3" End If
'If musicNum = 26 Then PlayMusic "STMILLERB/Going To Mexico.mp3" End If
'If musicNum = 27 Then PlayMusic "STMILLERB/Come On in My Kitchen.mp3" End If
'If musicNum = 28 Then PlayMusic "STMILLERB/The Window.mp3" End If
'If musicNum = 29 Then PlayMusic "STMILLERB/Wild Mountain Honey.mp3" End If
'If musicNum = 30 Then PlayMusic "STMILLERB/Your Saving Grace.mp3" End If
'If musicNum = 31 Then PlayMusic "STMILLERB/Blue Odyssey.mp3" End If
'If musicNum = 32 Then PlayMusic "STMILLERB/Celebration Song.mp3" End If
'If musicNum = 33 Then PlayMusic "STMILLERB/Don_t Let Nobody Turn You Around.mp3" End If
'If musicNum = 34 Then PlayMusic "STMILLERB/Evil.mp3" End If
'If musicNum = 35 Then PlayMusic "STMILLERB/The Stake.mp3" End If
'If musicNum = 36 Then PlayMusic "STMILLERB/Dance Dance Dance.mp3" End If
'If musicNum = 37 Then PlayMusic "STMILLERB/Threshold.mp3" End If
'If musicNum = 38 Then PlayMusic "STMILLERB/Going to the Country.mp3" End If
'musicNum = (musicNum + 1) mod 39

'End Sub

'*********************************************
'*         Random Music Mod: MachWon         *
'*********************************************

Sub MusicOn
   'Dim musicEnd
   Dim m
   m = INT(39 * RND(1) )
Select Case m
Case 0:PlayMusic "STMILLERB/Space Intro.mp3"
Case 1:PlayMusic "STMILLERB/Abracadabra.mp3"
Case 2:PlayMusic "STMILLERB/Swingtown.mp3"
Case 3:PlayMusic "STMILLERB/The Joker.mp3"
Case 4:PlayMusic "STMILLERB/Fly Like An Eagle.mp3"
Case 5:PlayMusic "STMILLERB/Winter Time.mp3"
Case 6:PlayMusic "STMILLERB/Jungle Love.mp3"
Case 7:PlayMusic "STMILLERB/Jet Airliner.mp3"
Case 8:PlayMusic "STMILLERB/Rock_n Me.mp3"
Case 9:PlayMusic "STMILLERB/Take The Money and Run.mp3"
Case 10:PlayMusic "STMILLERB/True Fine Love.mp3"
Case 11:PlayMusic "STMILLERB/I Love You.mp3"
Case 12:PlayMusic "STMILLERB/Jackson-Kent Blues.mp3"
Case 13:PlayMusic "STMILLERB/Kow Kow Calqulator.mp3"
Case 14:PlayMusic "STMILLERB/Little Girl.mp3"
Case 15:PlayMusic "STMILLERB/Living in the U.S.A..mp3"
Case 16:PlayMusic "STMILLERB/Mercury Blues.mp3"
Case 17:PlayMusic "STMILLERB/My Dark Hour.mp3"
Case 18:PlayMusic "STMILLERB/Quicksilver Girl.mp3"
Case 19:PlayMusic "STMILLERB/Gangster Of Love.mp3"
Case 20:PlayMusic "STMILLERB/Seasons.mp3"
Case 21:PlayMusic "STMILLERB/Serenade.mp3"
Case 22:PlayMusic "STMILLERB/Shu Ba Da Du Ma Ma Ma Ma.mp3"
Case 23:PlayMusic "STMILLERB/Song for Our Ancestors.mp3"
Case 24:PlayMusic "STMILLERB/Space Cowboy.mp3"
Case 25:PlayMusic "STMILLERB/Sweet Maree.mp3"
Case 26:PlayMusic "STMILLERB/Going To Mexico.mp3"
Case 27:PlayMusic "STMILLERB/Come On in My Kitchen.mp3"
Case 28:PlayMusic "STMILLERB/The Window.mp3"
Case 29:PlayMusic "STMILLERB/Wild Mountain Honey.mp3"
Case 30:PlayMusic "STMILLERB/Your Saving Grace.mp3"
Case 31:PlayMusic "STMILLERB/Blue Odyssey.mp3"
Case 32:PlayMusic "STMILLERB/Celebration Song.mp3"
Case 33:PlayMusic "STMILLERB/Don_t Let Nobody Turn You Around.mp3"
Case 34:PlayMusic "STMILLERB/Evil.mp3"
Case 35:PlayMusic "STMILLERB/The Stake.mp3"
Case 36:PlayMusic "STMILLERB/Dance Dance Dance.mp3"
Case 37:PlayMusic "STMILLERB/Threshold.mp3"
Case 38:PlayMusic "STMILLERB/Going to the Country.mp3"
	End Select	 
End Sub

Sub Table1_MusicDone()
    MusicOn
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

'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
    LeftFlipperTop.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
    RightFlipperTop.RotZ = RightFlipper.currentangle
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
	PlaySound "Gate5", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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

'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'                  Start nFOZZY FLIPPERS'
'******************************************************

'Flipper Correction Initialization late 80s to early 90s
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
 
'Tracks ball velocity for judging bounce calculations & angle
'apologies to JimmyFingers is this is what his script does. I know his tracks ball velocity too but idk how it works in particular
dim cor : set cor = New CoRTracker
cor.debugOn = False
'cor.update() - put this on a low interval timer
Class CoRTracker
	public DebugOn 'tbpIn.text
	public ballvel
 
	Private Sub Class_Initialize : redim ballvel(0) : End Sub 
	'TODO this would be better if it didn't do the sorting every ms, but instead every time it's pulled for COR stuff
	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs
		if uBound(allballs) < 0 then if DebugOn then str = "no balls" : TBPin.text = str : exit Sub else exit sub end if: end if
		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next
 
		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
 
		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			if DebugOn then 
				dim s, bs 'debug spacer, ballspeed
				bs = round(BallSpeed(b),1)
				if bs < 10 then s = " " else s = "" end if
				str = str & b.id & ": " & s & bs & vbnewline 
				'str = str & b.id & ": " & s & bs & "z:" & b.z & vbnewline 
			end if
		Next
		if DebugOn then str = "ubound ballvels: " & ubound(ballvel) & vbnewline & str : if TBPin.text <> str then TBPin.text = str : end if
	End Sub
End Class
 
Sub RDampen_Timer()
Cor.Update
End Sub


' Thalamus : Exit in a clean and proper way
Sub Table1_exit()
  Controller.Games("slbmanib").Settings.Value("sound") = 1  
  Controller.Pause = False 
  Controller.Stop
  
End Sub

'***Peg****

Sub pegtimer_Timer
countrSL = countrSL + 1 : If CountrSL > 4 then CountrSL = 1 : end If
select case countrSL
case 1 : peg001.z=70:peg002.z=-350:peg003.z=-350:peg004.z=-350
case 2 : peg001.z=-350:peg002.z=70:peg003.z=-350:peg004.z=-350
case 3 : peg001.z=-350:peg002.z=-350:peg003.z=70:peg004.z=-350
case 4 : peg001.z=-350:peg002.z=-350:peg003.z=-350:peg004.z=70
end Select
End Sub 

Sub pegtimerstop_Timer()
    if countrSL = 1 then
   pegtimer.enabled = False
   pegTimerstop.enabled = False
    end if
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
CustomBallImage(1) = 		"glowball purple"
CustomBallLogoMode(1) = 	True
CustomBallDecal(1) = 		""
CustomBulbIntensity(1) = 	0
GBred(1) = 255 : GBgreen(1)	= 0 : GBblue(1) = 255


' green GlowBall
CustomBallGlow(2) = 		True
CustomBallImage(6) = 		"glowball green"
CustomBallLogoMode(6) = 	True
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
GBred(2) = 100 : GBgreen(2)	= 255 : GBblue(2) = 100

' blue GlowBall
CustomBallGlow(3) = 		True
CustomBallImage(3) = 		"glowball blue"
CustomBallLogoMode(3) = 	True
CustomBallDecal(3) = 		""
CustomBulbIntensity(3) = 	0
GBred(3) = 50 : GBgreen(3)	= 50 : GBblue(3) = 255


' Orange GlowBall
CustomBallGlow(4) = 		True
CustomBallImage(4) = 		"glowball orange"
CustomBallLogoMode(4) = 	True
CustomBallDecal(4) = 		""
CustomBulbIntensity(4) = 	0
GBred(4) = 255 : GBgreen(4)	= 165 : GBblue(4) = 000


' red GlowBall
CustomBallGlow(5) = 		True
CustomBallImage(5) = 		"glowball red"
CustomBallLogoMode(5) = 	True
CustomBallDecal(5) = 		""
CustomBulbIntensity(5) = 	0
GBred(5) = 255 : GBgreen(5)	= 99 : GBblue(5) = 71

' white GlowBall
CustomBallGlow(6) = 		True
CustomBallImage(6) = 		"glowball white"
CustomBallLogoMode(6) = 	True
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
GBred(6) = 255 : GBgreen(6)	= 255 : GBblue(6) = 255

' yellow GlowBall
CustomBallGlow(7) = 		True
CustomBallImage(7) = 		"glowball yellow"
CustomBallLogoMode(7) = 	True
CustomBallDecal(7) = 		""
CustomBulbIntensity(7) = 	0
GBred(7) = 255 : GBgreen(7)	= 255 : GBblue(7) = 000

' gold GlowBall
CustomBallGlow(8) = 		True
CustomBallImage(8) = 		"glowball gold"
CustomBallLogoMode(8) = 	True
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

'***********************
' spotflasher animation
'***********************

Dim MyPii, SpotStep, SpotDir
Dim sRGBStep, sRGBFactor, sRed, sGreen, sBlue

Sub StartSpots
    Spot1.visible = 1
    Spot2.visible = 1
    MyPii = Round(4 * Atn(1), 6) / 90
    SpotStep = 0
    sRGBStep = 0
    sRGBFactor = 5
    sRed = 255
    sGreen = 0
    sBlue = 0
    Spots.Enabled = 1
End Sub

Sub StopSpots
    Spot1.visible = 0
    Spot2.visible = 0
    Spots.Enabled = 0
    Spot1.RotZ = 210
    Camera1.RotZ = 210
    Spot2.RotZ = 170
    Camera2.RotZ = 170
    Spot1.color = RGB(255, 252, 224)
    Spot2.color = RGB(255, 252, 224)
End Sub

Sub Spots_Timer()
    Spot1.visible = 1
    Spot2.visible = 1
    'rotate spots
    SpotDir = SIN(SpotStep * MyPii) * 50
    SpotStep = (SpotStep + 1)MOD 360
    Spot1.RotZ = 210 - SpotDir
    Camera1.RotZ = 210 - SpotDir
    Spot2.RotZ = 170 + SpotDir
    Camera2.RotZ = 170 + SpotDir
    ' color the spotlights
    Select Case sRGBStep
        Case 0 'Green
            sGreen = sGreen + sRGBFactor
            If sGreen > 255 then
                sGreen = 255
                sRGBStep = 1
            End If
        Case 1 'Red
            sRed = sRed - sRGBFactor
            If sRed < 0 then
                sRed = 0
                sRGBStep = 2
            End If
        Case 2 'Blue
            sBlue = sBlue + sRGBFactor
            If sBlue > 255 then
                sBlue = 255
                sRGBStep = 3
            End If
        Case 3 'Green
            sGreen = sGreen - sRGBFactor
            If sGreen < 0 then
                sGreen = 0
                sRGBStep = 4
            End If
        Case 4 'Red
            sRed = sRed + sRGBFactor
            If sRed > 255 then
                sRed = 255
                sRGBStep = 5
            End If
        Case 5 'Blue
            sBlue = sBlue - sRGBFactor
            If sBlue < 0 then
                sBlue = 0
                sRGBStep = 0
            End If
    End Select
    Spot1.color = RGB(sRed, sGreen, sBlue)
    Spot2.color = RGB(sRed, sGreen, sBlue)
End Sub

'******************************************************
' LAMP CALLBACK for the bckglass flasher lamps
'******************************************************

if VRMode = True Then
	Set LampCallback = GetRef("UpdateVRLamps")
End If


Sub UpdateVRLamps()
	If Controller.Lamp(11) = 0 Then: VR_ShootAgain.visible=0: 		Else: VR_ShootAgain.visible=1 			'Shoot again
	If Controller.Lamp(13) = 0 Then: VR_BIP.visible=0: 				Else: VR_BIP.visible=1 					'Ball in  play
	If Controller.Lamp(61) = 0 Then: VR_Tilt.visible=0: 			Else: VR_Tilt.visible=1 				'Tilt
	If Controller.Lamp(45) = 0 Then: VR_GameOver.visible=0: 		Else: VR_GameOver.visible=1 			'Game Over
	If Controller.Lamp(27) = 0 Then: VR_Match.visible=0: 			Else: VR_Match.visible=1 				'Match
	If Controller.Lamp(29) = 0 Then: VR_HighScore.visible=0: 		Else: VR_HighScore.visible=1 			'High Score
	If Controller.Lamp(1)  = 0 Then: VRS.visible = 0:				Else: VRS.visible = 1					'S
	If Controller.Lamp(17) = 0 Then: VRI.visible = 0:				Else: VRI.visible = 1					'I
	If Controller.Lamp(33) = 0 Then: VRL1.visible = 0:				Else: VRL1.visible = 1					'L
	If Controller.Lamp(49) = 0 Then: VRV.visible = 0:				Else: VRV.visible = 1					'V
	If Controller.Lamp(2)  = 0 Then: VRE.visible = 0:				Else: VRE.visible = 1					'E
	If Controller.Lamp(18) = 0 Then: VRR.visible = 0:				Else: VRR.visible = 1					'R
	If Controller.Lamp(34) = 0 Then: VRB.visible = 0:				Else: VRB.visible = 1					'B
	If Controller.Lamp(50) = 0 Then: VRA.visible = 0:				Else: VRA.visible = 1					'A
	If Controller.Lamp(3)  = 0 Then: VRL2.visible = 0:				Else: VRL2.visible = 1					'L
	If Controller.Lamp(19) = 0 Then: VRL3.visible = 0:				Else: VRL3.visible = 1					'L

End Sub
