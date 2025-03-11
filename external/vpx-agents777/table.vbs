'
' AAAAAAAA GGGGGGGG EEEEEEEE NN    NN TTTTTTTT SSSSSSSS 77777777 77777777 77777777
' AAAAAAAA GGGGGGGG EEEEEEEE NNN   NN TTTTTTTT SSSSSSSS 77777777 77777777 77777777
' AA    AA GG       EE       NNNN  NN    TT    SS       77    77 77    77 77    77
' AA    AA GG GGGGG EEEEE    NN NN NN    TT    SSSSSSSS       77       77       77
' AAAAAAAA GG GGGGG EEEEE    NN  NNNN    TT    SSSSSSSS       77       77       77
' AA    AA GG    GG EE       NN   NNN    TT          SS       77       77       77
' AA    AA GGGGGGGG EEEEEEEE NN    NN    TT    SSSSSSSS       77       77       77
' AA    AA GGGGGGGG EEEEEEEE NN    NN    TT    SSSSSSSS       77       77       77
'
'
' Agents 777 / IPD No. 26 / November, 1984 / 4 Players
' http://www.ipdb.org/machine.cgi?id=26
' version 1.0 created in VP10beta by MaX
'
' \_  _/  /| |\/
'  \\//  / | |/\
'         /
'
'v1.0rc - 2015, April, 4th
'
' Thanks to (in alphabetical order):
' Eala Dubh Sidhe and Destruk for the VP8 table
' jpsalas for allowing me to use his scripts and resources
' Unclewilly for allowing me to use his scripts and resources

Option Explicit

'**************************************
'**************************************
'Choose Controller: 1-VPM, 2-UVP, 3-B2S        
'**************************************
'**************************************

Const cController = 3

'**************************************
'**************************************
'**************************************
'**************************************

LoadVPM "01200000","GamePlan.vbs",3.1

Dim cCredits
cCredits="Agents 7-7-7, GamePlan 1984"
Const cGameName="agent777",UseSolenoids=1,UseLamps=1,UseGI=0,UseSync=1,SFlipperOn="fx_FlipperUp",SFlipperOff="fx_FlipperDown",SCoin="coin3",HandleMech=0

Sub LoadVPM(VPMver, VBSfile, VBSver)
	On Error goto 0
		If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
		ExecuteGlobal GetTextFile(VBSfile)
		If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description : Err.Clear
        Select Case cController
			Case 1:
				Set Controller = CreateObject("VPinMAME.Controller")
				If Err Then MsgBox "Can't Load VPinMAME." & vbNewLine & Err.Description
				If VPMver>"" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
				If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
            Case 2:
				Set Controller = CreateObject("UltraVP.BackglassServ")
'				Leds.enabled=1
            Case 3:
                Set Controller = CreateObject("B2S.Server")
         End Select
	On Error Goto 0
End Sub

SolCallback(15)="bsTrough.SolOut"		'Sol 1
SolCallback(7)="bsSaucer.SolOut"		'Sol 2
SolCallback(8)="bsSaucer2.SolOut"		'Sol 3
'SolCallback(4)="vpmSolSound ""jet3"","	'Sol 4
'SolCallback(2)="vpmSolSound ""jet3"","	'Sol 5
'SolCallback(9)="vpmSolSound ""jet3"","	'Sol 6
'SolCallback(1)="dtL.SolDropUp"			'Sol 7
SolCallback(1)="SoldtL"			'Sol 7
'SolCallback(5)="vpmSolSound ""sling"","	'Sol 8
'SolCallback(6)="vpmSolSound ""sling"","	'Sol 9
''SolCallback(3)="vpmSolSound ""Knocker"","'Sol 10
SolCallback(16)="vpmNudge.SolGameOn"	'Sol 14
SolCallback(sLLFlipper)="vpmSolFlipper LeftFlipper, Nothing,"
SolCallback(sLRFlipper)="vpmSolFlipper RightFlipper, Rightflipper1,"

Dim bsTrough,bsSaucer,bsSaucer2,dtL

If ShowDT=false Then
dim objekt : for each objekt in backdropobjs : objekt.visible = 0 : next
End If

Sub Agents777_Init
	On Error Resume Next
	With Controller
		.GameName=cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine=cCredits
'      .Games(cGameName).Settings.Value("dmd_pos_x")=0
'      .Games(cGameName).Settings.Value("dmd_pos_y")=0
'		.HandleMechanics=1
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden=1
		.Run
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0

	Controller.Dip(0) = (0*1 + 1*2 + 0*4 + 0*8 + 0*16 + 0*32 + 0*64 + 0*128) '01-08
	Controller.Dip(1) = (0*1 + 0*2 + 0*4 + 1*8 + 1*16 + 1*32 + 1*64 + 1*128) '09-16
	Controller.Dip(2) = (0*1 + 1*2 + 0*4 + 0*8 + 0*16 + 0*32 + 0*64 + 1*128) '17-24
	Controller.Dip(3) = (1*1 + 1*2 + 1*4 + 0*8 + 0*16 + 1*32 + 1*64 + 0*128) '25-32

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=True

	vpmNudge.TiltSwitch=swTilt
	vpmNudge.Sensitivity=5
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingShot,RightSlingShot,Slingshot3,Slingshot4)

	Set bsTrough=New cvpmBallStack
	bsTrough.InitSw 0,11,0,0,0,0,0,0
	bsTrough.InitKick BallRelease,90,8 
	bsTrough.InitExitSnd "ballrelease",""
	bsTrough.Balls=1
	
	Set bsSaucer=New cvpmBallStack
	bsSaucer.InitSaucer Kicker1,10,167.5,5
	bsSaucer.KickAngleVar=2.5
	bsSaucer.InitExitSnd "popper_ball","fx_flipperdown"

	Set bsSaucer2=New cvpmBallStack
	bsSaucer2.InitSaucer Kicker2,9,192.5,5
	bsSaucer2.KickAngleVar=2.5
	bsSaucer2.InitExitSnd "popper_ball","fx_flipperdown"

	Set dtL=New cvpmDropTarget
	dtL.InitDrop Array(droptarget1a,droptarget2a,droptarget3a),Array(18,19,20)
	dtL.InitSnd "droptargetdown","bankresetA 100P"
	dtL.CreateEvents dtL

	vpmMapLights AllLights

End Sub 

Dim droptarget1pos,droptarget2pos,droptarget3pos
droptarget1pos=0:droptarget2pos=0:droptarget3pos=0

Sub SoldtL(enabled)
	if enabled then
		dtL.DropSol_On
		if droptarget1pos=48 then rdt1.enabled=1
		if droptarget2pos=48 then rdt2.enabled=1
		if droptarget3pos=48 then rdt3.enabled=1
	end if
End Sub

Sub droptarget1a_Hit:dtL.Hit 1:me.timerenabled=1:End Sub
Sub droptarget2a_Hit:dtL.Hit 2:me.timerenabled=1:End Sub
Sub droptarget3a_Hit:dtL.Hit 3:me.timerenabled=1:End Sub

Sub droptarget1a_timer()
	droptarget1pos=droptarget1pos+4
	droptarget1b.TransZ=0-droptarget1pos
	if droptarget1pos=48 then me.timerenabled=0
End Sub
Sub rdt1_timer()
	droptarget1pos=droptarget1pos-12
	droptarget1b.TransZ=0-droptarget1pos
	if droptarget1pos=0 then me.enabled=0
End Sub

Sub droptarget2a_timer()
	droptarget2pos=droptarget2pos+4
	droptarget2b.TransZ=0-droptarget2pos
	if droptarget2pos=48 then me.timerenabled=0
End Sub
Sub rdt2_timer()
	droptarget2pos=droptarget2pos-12
	droptarget2b.TransZ=0-droptarget2pos
	if droptarget2pos=0 then me.enabled=0
End Sub

Sub droptarget3a_timer()
	droptarget3pos=droptarget3pos+4
	droptarget3b.TransZ=0-droptarget3pos
	if droptarget3pos=48 then me.timerenabled=0
End Sub
Sub rdt3_timer()
	droptarget3pos=droptarget3pos-12
	droptarget3b.TransZ=0-droptarget3pos
	if droptarget3pos=0 then me.enabled=0
End Sub


'Sub BallRelease_Hit:Me.Kick 90,8:End Sub
Sub Kicker1_Hit:PlaySound "kicker_enter_center",0,0.5,0.5,0.25:bsSaucer.AddBall 0:End Sub
Sub Kicker2_Hit:PlaySound "kicker_enter_center",0,0.5,0.5,0.25:bsSaucer2.AddBall 0:End Sub
Sub Drain_Hit:bsTrough.AddBall Me:End Sub
Sub RubberRebound_Hit:vpmTimer.PulseSw 17:End Sub
Sub Rubber18_Hit:vpmTimer.PulseSw 17:End Sub
Sub Rubber28_Hit:vpmTimer.PulseSw 17:End Sub

Sub SpinnerSW4_Spin
	vpmTimer.PulseSw 4
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub Agents777_Paused:Controller.Pause=True:End Sub 
Sub Agents777_UnPaused:Controller.Pause=False:End Sub
Sub Agents777_Exit:Controller.Stop:End Sub

'Switches and Rollovers

Sub sw18_Hit:vpmTimer.PulseSw 18:End Sub
Sub sw19_Hit:vpmTimer.PulseSw 19:End Sub
Sub sw20_Hit:vpmTimer.PulseSw 20:End Sub

Sub sw22_Hit:vpmTimer.PulseSw 22:End Sub
Sub sw23_Hit:vpmTimer.PulseSw 23:End Sub
Sub sw24_Hit:vpmTimer.PulseSw 24:End Sub
Sub sw25_Hit:vpmTimer.PulseSw 25:End Sub

Sub sw21_Hit:Controller.Switch(21) = 1:End Sub
Sub sw21_unHit:Controller.Switch(21) = 0:End Sub

Sub sw27_Hit:Controller.Switch(27) = 1:End Sub
Sub sw27_unHit:Controller.Switch(27) = 0:End Sub
Sub sw28_Hit:Controller.Switch(28) = 1:End Sub
Sub sw28_unHit:Controller.Switch(28) = 0:End Sub
Sub sw29_Hit:Controller.Switch(29) = 1:End Sub
Sub sw29_unHit:Controller.Switch(29) = 0:End Sub
Sub sw30_Hit:Controller.Switch(30) = 1:End Sub
Sub sw30_unHit:Controller.Switch(30) = 0:End Sub
Sub sw31_Hit:Controller.Switch(31) = 1:End Sub
Sub sw31_unHit:Controller.Switch(31) = 0:End Sub
Sub sw32_Hit:Controller.Switch(32) = 1:End Sub
Sub sw32_unHit:Controller.Switch(32) = 0:End Sub

Sub sw34_Hit:Controller.Switch(34) = 1:End Sub
Sub sw34_unHit:Controller.Switch(34) = 0:End Sub
Sub sw35_Hit:Controller.Switch(35) = 1:End Sub
Sub sw35_unHit:Controller.Switch(35) = 0:End Sub
Sub sw36_Hit:Controller.Switch(36) = 1:End Sub
Sub sw36_unHit:Controller.Switch(36) = 0:End Sub

Sub Agents777_KeyDown(ByVal keycode)
    If vpmKeyDown(KeyCode) Then Exit Sub 
	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySound "plungerpull",0,1,0.25,0.25
	End If

'	If keycode = LeftFlipperKey Then
'		LeftFlipper.RotateToEnd
'		PlaySound "fx_flipperup", 0, .67, -0.05, 0.05
'	End If
'    
'	If keycode = RightFlipperKey Then
'		RightFlipper.RotateToEnd
'		RightFlipper1.RotateToEnd
'
'		PlaySound "fx_flipperup", 0, .67, 0.05, 0.05
'	End If
    
	If keycode = LeftTiltKey Then
		Nudge 90, 2
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
	End If
    
End Sub

Sub Agents777_KeyUp(ByVal keycode)
If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySound "plunger",0,1,0.25,0.25
	End If
    
'	If keycode = LeftFlipperKey Then
'		LeftFlipper.RotateToStart
'		PlaySound "fx_flipperdown", 0, 1, -0.05, 0.05
'	End If
'    
'	If keycode = RightFlipperKey Then
'		RightFlipper.RotateToStart
'		Rightflipper1.rotatetostart
'		PlaySound "fx_flipperdown", 0, 1, 0.05, 0.05
'	End If

End Sub

Sub Drain_Hit()
	PlaySound "drain",0,1,0,0.25
	bsTrough.AddBall Me
End Sub

Sub LeftSlingshot_Slingshot()
	PlaySound "fx_bumper4",0,1,-0.15,0.25
End Sub

Sub Bumper1_Hit
	PlaySound "fx_bumper4"
	vpmTimer.PulseSw 14
'	B1L1.State = 1
'	Me.TimerEnabled = 1
End Sub

Sub Bumper1_Timer
	B1L1.State = 0
	Me.Timerenabled = 0
End Sub

Sub Bumper2_Hit
	PlaySound "fx_bumper4"
	vpmTimer.PulseSw 16
'	B2L1.State = 1
'	Me.TimerEnabled = 1
End Sub

Sub Bumper2_Timer
	B2L1.State = 0
	Me.Timerenabled = 0
End Sub	

Sub Bumper3_Hit
	PlaySound "fx_bumper4"
	vpmTimer.PulseSw 15
'	B3L1.State = 1
'	Me.TimerEnabled = 1
End Sub

Sub Bumper3_Timer
	B3L1.State = 0
	Me.Timerenabled = 0
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
LampTimer.Interval = 10 'lamp fading speed
LampTimer.Enabled = 0

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

NFadeL 2,l2
NFadeL 3,l3
NFadeL 4,l4
NFadeL 5,l5
NFadeL 6,l6
NFadeL 7,l7
NFadeL 8,l8
NFadeL 9,l9
NFadeL 10,l10
NFadeL 11,l11
NFadeL 12,l12
NFadeL 13,l13
NFadeL 14,l14
NFadeL 15,l15
NFadeL 16,l16
NFadeL 17,l17
NFadeL 18,l18
NFadeL 19,l19
NFadeL 20,l20
NFadeL 21,l21
NFadeL 22,l22
NFadeL 23,l23
NFadeL 24,l24
NFadeL 25,l25
NFadeL 26,l26
NFadeL 27,l27

NFadeL 29,l29
NFadeL 30,l30
NFadeL 31,l31

NFadeL 33,l33
NFadeL 34,l34
NFadeL 35,l35
NFadeL 36,l36
NFadeL 37,l37

NFadeL 39,l39

NFadeL 42,l42
NFadeL 43,l43

NFadeL 46,l46
NFadeL 47,l47
NFadeL 48,l48

NFadeL 52,l52
NFadeL 53,l53
NFadeL 54,l54
NFadeL 55,l55
NFadeL 56,l56
NFadeL 57,l57
NFadeL 58,l58
NFadeL 59,l59
NFadeL 60,l60
NFadeL 61,l61
NFadeL 62,l62
NFadeL 63,l63
NFadeL 64,l64


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

' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub FadeR(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.SetValue 0:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1              'wait
        Case 9:object.SetValue 2:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1          'wait
        Case 13:object.SetValue 3:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeRm(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1
        Case 5:object.SetValue 0
        Case 9:object.SetValue 2
        Case 3:object.SetValue 3
    End Select
End Sub

'Texts

Sub NFadeT(nr, object, message)
    Select Case FadingLevel(nr)
        Case 4:object.Text = "":FadingLevel(nr) = 0
        Case 5:object.Text = message:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, object, b)
    Select Case FadingLevel(nr)
        Case 4:object.Text = ""
        Case 5:object.Text = message
    End Select
End Sub



'*****GI Lights On
dim xx

For each xx in GI:xx.State = 1: Next

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub SlingShot3_Slingshot
    PlaySound "left_slingshot", 0, 1, 0.05, 0.05
	vpmTimer.PulseSw 17
End Sub
Sub SlingShot4_Slingshot
    PlaySound "right_slingshot", 0, 1, 0.05, 0.05
	vpmTimer.PulseSw 17
End Sub

Sub RightSlingShot_Slingshot
    PlaySound "left_slingshot", 0, 1, 0.05, 0.05
	vpmTimer.PulseSw 12
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	gi1.State = 0:Gi2.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound "right_slingshot",0,1,-0.05,0.05
	vpmTimer.PulseSw 13
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	gi3.State = 0:Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
End Sub



' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Vol2(ball1, ball2) ' Calculates the Volume of the sound based on the speed of two balls
    Vol2 = (Vol(ball1) + Vol(ball2) ) / 2
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "Agents777" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / Agents777.width-1
    If tmp> 0 Then
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
'    JP's VP10 Collision & Rolling Sounds
'*****************************************

Const tnob = 8 ' total number of balls
ReDim rolling(tnob)
ReDim collision(tnob)
Initcollision

Sub Initcollision
    Dim i
    For i = 0 to tnob
        collision(i) = -1
        rolling(i) = False
    Next
End Sub

Sub CollisionTimer_Timer()
    Dim BOT, B, B1, B2, dx, dy, dz, distance, radii
    BOT = GetBalls

    ' rolling
	
	For B = UBound(BOT) +1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
	Next

    If UBound(BOT) = -1 Then Exit Sub

    For B = 0 to UBound(BOT)
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

    'collision

    If UBound(BOT) < 1 Then Exit Sub

    For B1 = 0 to UBound(BOT)
        For B2 = B1 + 1 to UBound(BOT)
            dz = INT(ABS((BOT(b1).z - BOT(b2).z) ) )
            radii = BOT(b1).radius + BOT(b2).radius
			If dz <= radii Then

            dx = INT(ABS((BOT(b1).x - BOT(b2).x) ) )
            dy = INT(ABS((BOT(b1).y - BOT(b2).y) ) )
            distance = INT(SQR(dx ^2 + dy ^2) )

            If distance <= radii AND (collision(b1) = -1 OR collision(b2) = -1) Then
                collision(b1) = b2
                collision(b2) = b1
                PlaySound("fx_collide"), 0, Vol2(BOT(b1), BOT(b2)), Pan(BOT(b1)), 0, Pitch(BOT(b1)), 0, 0
            Else
                If distance > (radii + 10)  Then
                    If collision(b1) = b2 Then collision(b1) = -1
                    If collision(b2) = b1 Then collision(b2) = -1
                End If
            End If
			End If
        Next
    Next
End Sub


'************************************
' What you need to add to your table
'************************************

' a timer called CollisionTimer. With a fast interval, like from 1 to 10
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

' The Double For loop: This is a double cycle used to check the collision between a ball and the other ones.
' If you look at the parameters of both cycles, you�ll notice they are designed to avoid checking 
' collision twice. For example, I will never check collision between ball 2 and ball 1, 
' because I already checked collision between ball 1 and 2. So, if we have 4 balls, 
' the collision checks will be: ball 1 with 2, 1 with 3, 1 with 4, 2 with 3, 2 with 4 and 3 with 4.

' Sum first the radius of both balls, and if the height between them is higher then do not calculate anything more,
' since the balls are on different heights so they can't collide.

' The next 3 lines calculates distance between xth and yth balls with the Pytagorean theorem,

' The first "If": Checking if the distance between the two balls is less than the sum of the radius of both balls, 
' and both balls are not already colliding.

' Why are we checking if balls are already in collision? 
' Because we do not want the sound repeting when two balls are resting closed to each other.

' Set the collision property of both balls to True, and we assign the number of the ball colliding

' Play the collide sound of your choice using the VOL, PAN and PITCH functions

' Last lines: If the distance between 2 balls is more than the radius of a ball,
' then there is no collision and then set the collision property of the ball to False (-1).


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

' Backglass Display
'leds.enabled=false
Dim Digits(40),chg2LED2, num2, ii2, jj2, chg2, stat2

Set Digits(0) = a0
Set Digits(1) = a1
Set Digits(2) = a2
Set Digits(3) = a3
Set Digits(4) = a4
Set Digits(5) = a5
Set Digits(6) = a6
Set Digits(7) = a7
Set Digits(8) = b0
Set Digits(9) = b1xx
Set Digits(10) = b2xx
Set Digits(11) = b3
Set Digits(12) = b4
Set Digits(13) = b5
Set Digits(14) = b6
Set Digits(15) = b7
Set Digits(16) = c0
Set Digits(17) = c1
Set Digits(18) = c2
Set Digits(19) = c3
Set Digits(20) = c4
Set Digits(21) = c5
Set Digits(22) = c6
Set Digits(23) = c7
Set Digits(24) = d0
Set Digits(25) = d1
Set Digits(26) = d2
Set Digits(27) = d3
Set Digits(28) = d4
Set Digits(29) = d5
Set Digits(30) = d6
Set Digits(31) = d7

Sub Leds_timer()
     chg2led2 = Controller.ChangedLEDs(&HFF, &HFFFF)
     If Not IsEmpty(chg2led2) Then
         For ii2 = 0 To UBound(chg2led2)
             num2 = chg2led2(ii2, 0):chg2 = chg2led2(ii2, 1):stat2 = chg2led2(ii2, 2)
              Select Case stat2
                 Case 0:Digits(num2).SetValue 0    'empty
                 Case 63:Digits(num2).SetValue 1   '0
                 Case 6:Digits(num2).SetValue 2    '1
                 Case 91:Digits(num2).SetValue 3   '2
                 Case 79:Digits(num2).SetValue 4   '3
                 Case 102:Digits(num2).SetValue 5  '4
                 Case 109:Digits(num2).SetValue 6  '5
                 Case 124:Digits(num2).SetValue 7  '6
                 Case 125:Digits(num2).SetValue 7  '6
                 Case 252:Digits(num2).SetValue 7  '6
                 Case 7:Digits(num2).SetValue 8    '7
                 Case 127:Digits(num2).SetValue 9  '8
                 Case 103:Digits(num2).SetValue 10 '9
                 Case 111:Digits(num2).SetValue 10 '9
                 Case 231:Digits(num2).SetValue 10 '9
                 Case 128:Digits(num2).SetValue 0  'empty
                 Case 191:Digits(num2).SetValue 1  '0
                 Case 832:Digits(num2).SetValue 2  '1
                 Case 896:Digits(num2).SetValue 2  '1
                 Case 768:Digits(num2).SetValue 2  '1
                 Case 134:Digits(num2).SetValue 2  '1
                 Case 219:Digits(num2).SetValue 3  '2
                 Case 207:Digits(num2).SetValue 4  '3
                 Case 230:Digits(num2).SetValue 5  '4
                 Case 237:Digits(num2).SetValue 6  '5
                 Case 253:Digits(num2).SetValue 7  '6
                 Case 135:Digits(num2).SetValue 8  '7
                 Case 255:Digits(num2).SetValue 9  '8
                 Case 239:Digits(num2).SetValue 10 '9
             End Select
         Next
     End IF
 End Sub
 
