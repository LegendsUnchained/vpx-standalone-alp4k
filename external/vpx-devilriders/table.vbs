option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cgamename = "dvlrider", UseSolenoids=1, UseLamps=0,UseGI=0, SCoin="DR Coin"

LoadVPM "01110000","zac2.vbs",2.0


Const cCredits="Devil Riders - Zaccaria 1984"

Dim DesktopMode: DesktopMode = Table1.ShowDT
If DesktopMode = True Then 'Show Desktop components
  Ramp16.visible=1
  Ramp15.visible=1
  Primitive13.visible=1
  cycle.visible=1
Else
  Ramp16.visible=0
  Ramp15.visible=0
  Primitive13.visible=0
  cycle.visible=0
End if

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

SolCallback(1)=		"LeftKick"
SolCallback(2)=		"RightKick"
SolCallback(4)=  	"vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(6)=		"dtR.SolDropUp"  'Bottom Right Drop Target Bank
SolCallback(7)=		"dtL.SolDropUp"  'Bottom Left Drop Target Bank
SolCallback(14)=	"DoLeftMovePost"
SolCallback(15)=	"DoRightMovePost"
SolCallback(16)=	"dtL.SolHit 1,"
SolCallback(18)=	"dtL.SolHit 2,"
SolCallback(19)=	"dtL.SolHit 3,"
SolCallback(20)=	"vpmSolSound SoundFX(""bell"",DOFChime),"
SolCallback(21)=	"dtR.SolHit 1,"
SolCallback(22)=	"dtR.SolHit 2,"
SolCallback(23)=	"dtR.SolHit 3,"
SolCallback(24)=    "bsTrough.SolOut"						'BallRelease

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"


Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("DR FlipperUp",DOFFlippers), LeftFlipper, 1:LeftFlipper.RotateToEnd
		if OnUpperPF then LeftUpperFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFX("DR FlipperDown",DOFFlippers), LeftFlipper, 1:LeftFlipper.RotateToStart
		if OnUpperPF then LeftUpperFlipper.RotateToStart
     End If
  End Sub

Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("DR FlipperUp",DOFFlippers), RightFlipper, 1:RightFlipper.RotateToEnd
		if OnUpperPF then RightUpperFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFX("DR FlipperDown",DOFFlippers), RightFlipper, 1:RightFlipper.RotateToStart
		if OnUpperPF then RightUpperFlipper.RotateToStart
     End If
End Sub

Sub LeftKick(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("fx_Flipperup",DOFContactors), LeftFlipper2, 1:LeftFlipper2.RotateToEnd
     Else
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFContactors), LeftFlipper2, 1:LeftFlipper2.RotateToStart
     End If
  End Sub

Sub RightKick(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("fx_Flipperup",DOFContactors), RightFlipper2, 1:RightFlipper2.RotateToEnd
     Else
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFContactors), RightFlipper2, 1:RightFlipper2.RotateToStart
     End If
End Sub


'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

'Playfield GI
'*****GI Lights On
dim xx
For each xx in GI:xx.State = 1: Next

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough,dtL, dtR

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Devil Riders by Sindbad"&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 1
        '.Games(cGameName).Settings.Value("sound")=1
		'.PuPHide = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled= 1

	vpmNudge.TiltSwitch = 10
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, Bumper4, LeftSlingshot, RightSlingshot)

    Set bsTrough=New cvpmBallStack
        bsTrough.InitSw 0, 16, 0, 0, 0, 0, 0, 0
        bsTrough.InitKick BallRelease,90,12
        bsTrough.InitExitSnd SoundFX("DR BallRelease",DOFContactors),SoundFX("Solenoid",DOFContactors)
        bsTrough.Balls=1

	Set dtL=New cvpmDropTarget
		dtL.InitDrop Array(sw31,sw30,sw29),Array(31,30,29)
		dtL.InitSnd SoundFX("DR DropTargetHit",DOFDropTargets),SoundFX("DR BankReset",DOFContactors)

	Set dtR=New cvpmDropTarget
		dtR.InitDrop Array(sw27,sw26,sw25),Array(27,26,25)
		dtR.InitSnd SoundFX("DR DropTargetHit",DOFDropTargets),SoundFX("DR BankReset",DOFContactors)

	MovePosts_Init
	UpperPF_Off
    bumpn1=1

End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If keycode = LeftMagnaSave Then bLutActive = True
	If keycode = RightMagnaSave Then
		If bLutActive Then NextLUT: End If
    End If
	If keycode = PlungerKey Then Plunger.Pullback:playsoundAtVol "PlungerPull", Plunger, 1
	If KeyCode = LeftFlipperKey Then Controller.Switch(17) = 1
	If KeyCode = RightFlipperKey Then Controller.Switch(23) = 1
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
    If keycode = LeftMagnaSave Then bLutActive = False
	If keycode = PlungerKey Then Plunger.Fire:PlaySoundAtVol "plunger", Plunger, 1
	If KeyCode = LeftFlipperKey Then Controller.Switch(17) = 0
	If KeyCode = RightFlipperKey Then Controller.Switch(23) = 0
	If KeyUpHandler(keycode) Then Exit Sub
End Sub


'***********************************************************************************
'****						  Drains and Kickers           						****
'***********************************************************************************

Sub Drain_Hit:bsTrough.addball me : playsoundAtVol "DR Drain" , Drain, 1: End Sub



'***********************************************************************************
'****								Bumpers		          						****
'***********************************************************************************

Sub Bumper1_Hit : vpmTimer.PulseSw(40) : playsoundAtVol SoundFX("DR BumperL",DOFContactors), ActiveBall, 1: End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(41) : playsoundAtVol SoundFX("DR BumperL",DOFContactors), ActiveBall, 1: End Sub
Sub Bumper3_Hit : vpmTimer.PulseSw(42) : playsoundAtVol SoundFX("DR BumperR",DOFContactors), ActiveBall, 1: End Sub
Sub Bumper4_Hit : vpmTimer.PulseSw(43) : playsoundAtVol SoundFX("DR BumperR",DOFContactors), ActiveBall, 1: End Sub

'***********************************************************************************
'****								  Drop Targets								****
'***********************************************************************************


Sub sw29_Hit:dtL.Hit 3:End Sub		'29
Sub sw30_Hit:dtL.Hit 2:End Sub		'30
Sub sw31_Hit:dtL.Hit 1:End Sub		'31

Sub sw25_Hit:dtR.Hit 3:End Sub		'25
Sub sw26_Hit:dtR.Hit 2:End Sub		'26
Sub sw27_Hit:dtR.Hit 1:End Sub		'27

'***********************************************************************************
'****						   		Targets		 	     						****
'***********************************************************************************

Sub sw37_Hit:vpmTimer.PulseSw 37: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub
Sub sw38_Hit:vpmTimer.PulseSw 38: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub
Sub sw39_Hit:vpmTimer.PulseSw 39: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub
Sub sw49_Hit:vpmTimer.PulseSw 49: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub
Sub sw50_Hit:vpmTimer.PulseSw 50: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub
Sub sw51_Hit:vpmTimer.PulseSw 51: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub
Sub sw52_Hit:vpmTimer.PulseSw 52: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub
Sub sw53_Hit:vpmTimer.PulseSw 53: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub
Sub sw54_Hit:vpmTimer.PulseSw 54: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub
Sub sw55_Hit:vpmTimer.PulseSw 55: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub
Sub sw56_Hit:vpmTimer.PulseSw 56: PlaysoundAtVol SoundFX("DR Target",DOFTargets), ActiveBall, 1:End Sub




'***********************************************************************************
'****						   Rollovers, Triggers, Gates  						****
'***********************************************************************************


Sub sw18_Hit:Controller.Switch(18) = 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub
Sub sw19_Hit:Controller.Switch(19) = 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub sw19_UnHit:Controller.Switch(19) = 0:End Sub
Sub sw22_Hit:Controller.Switch(22) = 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub sw22_UnHit:Controller.Switch(22) = 0:End Sub
Sub sw24_Hit:Controller.Switch(24) = 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub

Sub sw33_Hit:Controller.Switch(33) = 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub
Sub sw34_Hit:Controller.Switch(34) = 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub
Sub sw35_Hit:Controller.Switch(35) = 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub
Sub sw45_Hit:Controller.Switch(45) = 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub
Sub sw46_Hit:Controller.Switch(46) = 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub sw46_UnHit:Controller.Switch(46) = 0:End Sub
Sub sw47_Hit:Controller.Switch(47) = 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub

'Upper Level Gate Triggers
Sub sw36_Hit:vpmTimer.PulseSw 36:UpperPF_On:End Sub
Sub sw44_Hit:vpmTimer.PulseSw 44:UpperPF_On:End Sub

Sub sw36_Hit:vpmTimer.PulseSw(36):UpperPF_On:PlaySoundAtVol "DR Gate", ActiveBall, 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub sw44_Hit:vpmTimer.PulseSw(44):UpperPF_On:PlaySoundAtVol "DR Gate", ActiveBall, 1:PlaySoundAtVol "DR Sensor", ActiveBall, 1:End Sub
Sub UpperPFExits_Hit(idx):UpperPF_Off:End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 21
    PlaySoundAtVol SoundFX("DR Sling",DOFContactors), sling1, 1
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
	vpmTimer.PulseSw 20
    PlaySoundAtVol SoundFX("DR Sling",DOFContactors), sling2, 1
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


'***********************************************************************************
'****							Upper PF Handling          						****
'***********************************************************************************
Dim PFExitBall, PFExitDir, i, OnUpperPF , bumpn1
Sub UpperPF_On
	For each i in UpperPFExits: i.Enabled = 1:Next
	CycleTimer.Enabled = 1
	OnUpperPF = True
End Sub

Sub UpperPF_Off
	CycleTimer.Enabled = 0
	For each i in UpperPFExits: i.Enabled = 0:Next
	OnUpperPF = False
    LeftUpperFlipper.RotateToStart
    RightUpperFlipper.RotateToStart
End Sub

Sub UpperPFExitCenter_Hit
	Set PFExitBall = ActiveBall
	UPFExitRamp.Collidable=1
	PFExitDir= -1:UPFExitTimer.Enabled = 1
	PlayLoopSoundAtVol "DR MetalNoise", UpperPFExitCenter, 1
End Sub

Sub UPFExitTimer_Timer
	Select Case PFExitDir
		Case 0:
			Me.Enabled = 0
		Case -1:
			If UPFExitWire.RotX < 31 Then UPFExitWire.RotX = UPFExitWire.RotX + 2
			If PFExitBall.Z < 27 Then
				UPFExitRamp.Collidable=0
				UpperPF_Off
				PFExitDir = 1
'				PlayLoopSoundAtVol "DR MetalNoise", UpperPFExitCenter, 1
				StopSound "DR MetalNoise"
			End If
		Case 1:
			UPFExitWire.RotX = UPFExitWire.RotX - 2
			If UPFExitWire.RotX = -2 Then Me.Enabled = 0
		End Select
End Sub

Sub CycleTimer_Timer
dim emv ' a local variable
	vpmtimer.pulsesw 11
	'start the motorcycle engine
     emv=bumpn1
     ' if past frame 35 display the previous frames in reverse order
     If emv>35 then	emv=36-emv
     ' draw the reel frame
     cycle.setvalue(emv)
     ' animation sequence 1-36
     bumpn1=bumpn1+1
End Sub


'***********************************************************************************
'****							MovePost Handling          						****
'***********************************************************************************
Dim mpr, mpl
Dim mppos: mppos = Array(-57, -45, -34, -22, -11, 0, 0, 0)
Dim mrpos: mrpos = Array(90, 93, 96, 99, 102, 104, 107, 104)

Sub DoLeftMovePost(Enabled)
	If ((Enabled) AND (mpl = 0)) Then
		mpl = NOT LeftMovePostW.IsDropped
		LeftMovePostW.TimerEnabled = 1
		playsoundAtVol SoundFX("DR MovePost",DOFContactors), LeftMovePostP, 1
	End If
End Sub

Sub DoRightMovePost(Enabled)
	If ((Enabled) AND (mpr = 0)) Then
		mpr = NOT RightMovePostW.IsDropped
		RightMovePostW.TimerEnabled = 1
		playsoundAtVol SoundFX("DR MovePost",DOFContactors), RightMovePostP, 1
	End If
End Sub

Sub LeftMovePostW_Timer
	If mpl < 0 Then
		LeftMovePostP.Z = mppos(mpl + 5)
		LeftMoveRamp.ObjRotZ = mrpos(mpl + 5)
		mpl = mpl - 1
		If mpl = -6 Then
			Controller.Switch(32) = 1
			LeftMovePostW.IsDropped = 1
			RampLeftUp.Collidable=0:RampLeftDown.Collidable=1
			Me.TimerEnabled = 0: mpl = 0
		End If
	Else
		LeftMovePostP.Z = mppos(mpl)
		LeftMoveRamp.ObjRotZ = mrpos(mpl)
		mpl = mpl + 1
		If mpl = 8 Then
			Controller.Switch(32) = 0
			LeftMovePostW.IsDropped = 0
			RampLeftUp.Collidable=1:RampLeftDown.Collidable=0
			Me.TimerEnabled = 0: mpl = 0
		End If
	End If
End Sub

Sub RightMovePostW_Timer
	If mpr < 0 Then
		RightMovePostP.Z = mppos(mpr + 5)
		RightMoveRamp.ObjRotZ = mrpos(mpr + 5)
		mpr = mpr - 1
		If mpr = -6 Then
			Controller.Switch(28) = 1
			RightMovePostW.IsDropped = 1
			RampRightUp.Collidable=0:RampRightDown.Collidable=1
			Me.TimerEnabled = 0: mpr = 0
		End If
	Else
		RightMovePostP.Z = mppos(mpr)
		RightMoveRamp.ObjRotZ = mrpos(mpr)
		mpr = mpr + 1
		If mpr = 8 Then
			Controller.Switch(28) = 0
			RightMovePostW.IsDropped = 0
			RampRightUp.Collidable=1:RampRightDown.Collidable=0
			Me.TimerEnabled = 0: mpr = 0
		End If
	End If
End Sub

Sub MovePosts_Init
	RightMovePostP.Z = 0: RightMovePostW.IsDropped = 1: Controller.Switch(28) = 1: RampRightUp.Collidable=0: RampRightDown.Collidable=1: mpr = 0
	LeftMovePostP.Z = 0: LeftMovePostW.IsDropped = 1: Controller.Switch(32) = 1: RampLeftUp.Collidable=0: RampLeftDown.Collidable=1: mpl = 0
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

    NFadeLm 1, l1
    NFadeL 1, l1a
    NFadeLm 2, l2
    NFadeL 2, l2a
    NFadeLm 3, l3
    NFadeL 3, l3a
    NFadeLm 4, l4
    NFadeL 4, l4a
    NFadeLm 5, l5
    NFadeL 5, l5a
    NFadeLm 8, l8
    NFadeL 8, l8a
    NFadeLm 9, l9
    NFadeL 9, l9a
    NFadeLm 11, l11
    NFadeL 11, l11a
    NFadeLm 12, l12
    NFadeL 12, l12a

    NFadeLm 14, l14    'Bumper
    NFadeL 14, l14a    'Bumper
    NFadeL 14, l14b    'Bumper
    NFadeL 14, l14c    'Bumper
    NFadeLm 15, l15    'Bumper
    NFadeL 15, l15a    'Bumper
    NFadeL 15, l15b    'Bumper
    NFadeL 15, l15c    'Bumper

    NFadeL 18, l18
    NFadeL 19, l19
    NFadeL 21, l21
    NFadeL 22, l22
    NFadeL 23, l23
    NFadeL 24, l24
    NFadeL 25, l25
    NFadeL 26, l26
    NFadeL 28, l28
    NFadeL 29, l29
    NFadeL 30, l30
    NFadeL 32, l32
    NFadeL 34, l34
    NFadeL 35, l35
    NFadeL 36, l36
    NFadeL 37, l37

    NFadeL 38, l38 'Upper Playfield
    NFadeL 39, l39 'Upper Playfield
    NFadeL 40, l40 'Upper Playfield
    NFadeL 41, l41 'Upper Playfield
    NFadeL 42, l42 'Upper Playfield
    NFadeL 43, l43 'Upper Playfield

    NFadeL 44, l44
    NFadeL 45, l45
    NFadeL 47, l47
    NFadeL 48, l48
    NFadeL 49, l49
    NFadeL 50, l50
    NFadeL 51, l51
    NFadeL 53, l53
    NFadeL 55, l55
    NFadeL 57, l57
    NFadeL 58, l58
    NFadeL 59, l59

    NFadeLm 61, l61 'Top Playfield
    NFadeLm 63, l63 'Top Playfield
    NFadeLm 64, l64 'Top Playfield
    NFadeLm 65, l65 'Top Playfield
    NFadeLm 68, l68 'Top Playfield

    NFadeLm 69, l69
    NFadeL 69, l69a
    NFadeL 70, l70
    NFadeL 71, l71
    NFadeL 74, l74
    NFadeL 75, l75


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
Dim Digits(35)

Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)
Digits(6) = Array(LED70,LED71,LED72,LED73,LED74,LED75,LED76)

Digits(7) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(8) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(9) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(10) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(11) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(12) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)
Digits(13) = Array(LED140,LED141,LED142,LED143,LED144,LED145,LED146)

Digits(14) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(15) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(16) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(17) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(18) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(19) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)
Digits(20) = Array(LED210,LED211,LED212,LED213,LED214,LED215,LED216)

Digits(21) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(22) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(23) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(24) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(25) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(26) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)
Digits(27) = Array(LED280,LED281,LED282,LED283,LED284,LED285,LED286)

Digits(28) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(29) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)
Digits(30) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(31) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)
Digits(32) = Array(LED88,LED79,LED97,LED98,LED89,LED78,LED87)
Digits(33) = Array(LED109,LED107,LED118,LED119,LED117,LED99,LED108)
Digits(34) = Array(LED137,LED128,LED139,LED147,LED138,LED127,LED129)


Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		If DesktopMode = True Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 35) then
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

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX, Rothbauerw, Thalamus and Herweh
' PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

' set position as table object and Vol + RndPitch manually

Sub PlaySoundAtVolPitch(sound, tableobj, Vol, RndPitch)
  PlaySound sound, 1, Vol, AudioPan(tableobj), RndPitch, 0, 0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallAbsVol(sound, VolMult)
  PlaySound sound, 0, VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

' requires rampbump1 to 7 in Sound Manager

Sub RandomBump(voladj, freq)
  Dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
  PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' set position as bumperX and Vol manually. Allows rapid repetition/overlaying sound

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
  PlaySound sound, 0, ABS(BOT.velz)/17, Pan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub

' play a looping sound at a location with volume
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
  PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function RndNum(min, max)
  RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = ball.x * 2 / table1.width-1
  If tmp > 0 Then
    Pan = Csng(tmp ^10)
  Else
    Pan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function VolMulti(ball,Multiplier) ' Calculates the Volume of the sound based on the ball speed
  VolMulti = Csng(BallVel(ball) ^2 / 150 ) * Multiplier
End Function

Function DVolMulti(ball,Multiplier) ' Calculates the Volume of the sound based on the ball speed
  DVolMulti = Csng(BallVel(ball) ^2 / 150 ) * Multiplier
  ' debug.print DVolMulti
End Function

Function BallRollVol(ball) ' Calculates the Volume of the sound based on the ball speed
  BallRollVol = Csng(BallVel(ball) ^2 / (80000 - (79900 * Log(RollVol) / Log(100))))
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
  BallVelZ = INT((ball.VelZ) * -1 )
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
  VolZ = Csng(BallVelZ(ball) ^2 / 200)*1.2
End Function

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order

Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
  Dim AB, BC, CD, DA
  AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
  BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
  CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
  DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)

  If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
    InRect = True
  Else
    InRect = False
  End If
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


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperLSh1.RotZ = LeftUpperFlipper.currentangle
	FlipperRSh1.RotZ = RightUpperFlipper.currentangle
	FlipperLSh2.RotZ = LeftFlipper2.currentangle
	FlipperRSh2.RotZ = RightFlipper2.currentangle

	RightUpperFlipperP.roty = RightUpperFlipper.CurrentAngle
	LeftUpperFlipperP.roty = LeftUpperFlipper.CurrentAngle
	RightFlipperP.roty = RightFlipper.CurrentAngle
	LeftFlipperP.roty = LeftFlipper.CurrentAngle
	RightOuterFlipperP.roty = RightFlipper2.CurrentAngle
	LeftOuterFlipperP.roty = LeftFlipper2.CurrentAngle

	GateShooterLaneP.RotZ = -(GateShooterLane.currentangle)
	sw36GateP.RotZ = -(sw36.currentangle)
	sw36support.RotZ = sw36.currentangle
	sw36support2.Z = ABS(sw36.currentangle / 9) + 105
	sw44GateP.RotZ = -(sw44.currentangle)
	sw44support.RotZ = -(sw44.currentangle)
	sw44support2.Z = ABS(sw44.currentangle / 9) + 105

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


'***********************************************************************************
'****							DIP switch routines 							****
'***********************************************************************************
Dim TableOptions, TableName

Sub CustomizeTable
	OptionsEdit
End Sub

Sub OptionsEdit
	Dim vpmDips: Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 530, 350, "Truck Stop - Table Options"
		If DesktopMode = False Then .AddFrameExtra 0,0,105,"Controller Selection*",&H00000030, Array("Visual PinMame", 0, "B2S Server", &H00000020)
		.AddChkExtra 175,0,135, Array("Disable Mech Sounds*", &H00000040)
		.AddLabel 185,15,120,15,"(For use with DOF)"
		.AddChkExtra 0,65,135, Array("Enable Night Mode", &H00000002)
		.AddChkExtra 0,85,135, Array("Enable Halo Effect", &H00000004)
		.AddChkExtra 175,35,135, Array("Enable dull flippers", &H00000008)
		.AddChkExtra 0,165,300, Array("Enable automatic outer flippers", &H00000080)
		.AddChkExtra 100,185,135, Array("Disable Menu Next Start", &H00000001)
	End With
	TableOptions = vpmDips.ViewDipsExtra(TableOptions)
	SaveValue TableName,"Options",TableOptions
	OptionsToVariables
End Sub

Sub OptionsLoad
	TableName="Devil Riders"
	Set vpmShowDips = GetRef("CustomizeTable")
 	TableOptions = LoadValue(TableName,"Options")
	Set Controller = CreateObject("VPinMAME.Controller")
	If TableOptions = "" Or OptionReset Then
		TableOptions = 7
		OptionsEdit
	Else
		If TableOptions And 1 = 0 Then
			TableOptions = TableOptions OR 1
			OptionsEdit
		Else
			OptionsToVariables
		End If
	End If
	Set Controller = Nothing
End Sub

Sub OptionsToVariables
	Dim nm
	DOFSound = (TableOptions AND &H00000040) / &H00000040
	If DesktopMode = False Then cController = ((TableOptions AND &H00000030) / &H00000010) + 1
	nm = (TableOptions AND &H00000002) / &H00000002
	UseHalo = (TableOptions AND &H00000004) / &H00000004
	DullFlippers = (TableOptions AND &H00000008) / &H00000008
	AutoFlipper = (TableOptions AND &H00000080) / &H00000080
	If nm <> NightMode Then
		NightMode = nm
		If GameOn = True Then Illumination_Init
	End If
End Sub

Sub editDips
   Dim vpmDips : Set vpmDips = New cvpmDips
   With vpmDips

  .ViewDips
  End With
 End Sub

'***********************************************************************************
'****				        	Sound Triggers    				  		  	****
'***********************************************************************************

Sub Trigger1_Hit:PlaySoundAtVol "DR ShooterLane", ActiveBall, 1:End Sub
Sub DR_Gates_Hit:PlaySoundAtVol "DR Gate", ActiveBall, 1:End Sub
Sub LeftRampTrigger_Hit(): PlaySoundAtVol "DR BallRollingMetalL", ActiveBall, 1:End Sub
Sub LeftRampTrigger_UnHit(): StopSound "DR BallRollingMetalL":End Sub
Sub RightRampTrigger_Hit(): PlaySoundAtVol "DR BallRollingMetalR", ActiveBall, 1:End Sub
Sub RightRampTrigger_UnHit(): StopSound "DR BallRollingMetalR":End Sub

Sub Table1_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub