'MAd MAx Table Build By Balutito  with help of Daeron61 for 3d
' based on:
'  Sega Table build by 32assassin and francisco666 with code rebuild by 32assassin and arngrim scripted DOF and test
'Twister Sega by xio, TheWool and JimmyFingers on VP 
'Twister Sega by francisco666 on FP
'Contains zany models of bumper caps
'Thanks Richard Bousquet for his Youtube tutorials about Converting Future Pinball tables to Visual Pinball X



' Primitive67 is the plastic under the truck, for some reason no vicible in editor


Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="twst_405",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM "01120100", "SEGA.VBS", 3.02    


Dim TargetsdeVPX
Dim GILightsColor
Dim decalonflippers 
Dim Primitivetransplastics
Dim FanSound
Dim RollVol 

'**********************************************************************************************************
'  *****  Options  ****
'         =======

' Round targets can be choosen between the VPX ones and primirives animated, Set 0 to VPX ones or set 1 to primitives
TargetsdeVPX = 0

'Gi lights can be choosen between normal lights, blue or violet, Set the next option to 0 for normal, 1 to blue ot 2 for violet
GILightsColor = 1

'Flippers can have or not a decal image on it, select 0 for standard flippers or 1 to flippers with decal
decalonflippers = 1

'set 0 for primitives non visibles and 1 to make them visibles instead of VPX surfaces
Primitivetransplastics=1

'set 0 to turn off fan sound if you have a real fan (automatic if you also have contactors)
FanSound = 0

' Ball rolling volune.  100 default, 200 louder, 50 quieter..
RollVol = 20

'**********************************************************************************************************




Dim DesktopMode: DesktopMode = Table1.ShowDT
If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Primitive74.visible=1
Else
Ramp16.visible=0
Ramp15.visible=0
Primitive74.visible=0
End if

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallBack(1) = "bsTrough2.SolOut"
SolCallback(2) = "Auto_Plunger"
SolCallback(3) = "DropTarget.SolDropUp"
SolCallback(4) = "DropTarget.SolHit 1,"
SolCallback(5) = "SolFan"'Fan Motor Relay
SolCallback(6) = "SolSpinWheelsMotor"
SolCallback(8) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallBack(14)= "bsLock.SolOut"
SolCallBack(17)= "bsTrough.SolOut"
SolCallBack(22)= "SetLamp 122,"
SolCallBack(23)= "SetLamp 123,"
SolCallBack(25)= "SetLamp 125," 'F1
SolCallBack(26)= "SetLamp 126," 'F2
SolCallBack(27)= "SetLamp 127," 'F3
SolCallBack(28)= "SetLamp 128," 'F4
SolCallBack(29)= "SetLamp 129," 'F5
SolCallBack(30)= "SetLamp 130," 'F6
SolCallBack(31)= "SetLamp 131," 'F7
SolCallBack(32)= "SetLamp 132," 'F8
SolCallback(33)= "MagnetDiverter.MagnetOn="
SolCallback(34)= "SolDiscMagnet"


SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolDiscMagnet(Enabled)
	'debug.print "Magnet: " & Enabled
	magnetdisco.MagnetOn=Enabled
	if not Enabled Then	
		magnetdisco.GrabCenter = False
	end if
End Sub

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySoundAt SoundFX("fx_Flipperup",DOFFlippers), gi5:LeftFlipper.RotateToEnd
     Else
         PlaySoundAt SoundFX("fx_Flipperdown",DOFFlippers), gi5:LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySoundAt SoundFX("fx_Flipperup",DOFFlippers), gi7:RightFlipper.RotateToEnd
     Else
         PlaySoundAt SoundFX("fx_Flipperdown",DOFFlippers), gi7:RightFlipper.RotateToStart
     End If
End Sub
'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

Sub Auto_Plunger(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
    End If
End Sub   

Sub SolFan(Enabled)
If enabled and FanSound=1 Then
		playsound SoundFX("Topper_Fan",DOFContactors)
	Else 
		stopsound "Topper_Fan"
	end If
End Sub

set GICallback = GetRef("UpdateGI")
Sub UpdateGI(no, Enabled)
	'	SetLamp 134, Enabled	'Backwall bulbs
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 1:	Next
        PlaySound "fx_relay"
		DOF 101, DOFOn
	Else For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"
		DOF 101, DOFOff
	End If
End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsTrough2, bsLock, DropTarget, TSPINA, magnetdisco, MagnetDiverter
NoUpperLeftFlipper
NoUpperRightFlipper

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Sega Twister"&chr(13)&"We all sucks"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 0
		.Games("twst_405").Settings.Value("sound") = 0
		.Games("twst_405").Settings.Value("samples") = 0

         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0


'****  Execute options here  *****
'If chose 0 VPX must be 1 and if choose 1 VPX must be 0, the primitive stay the same
		dim y
		For each y in targetscristal:y.visible = (- TargetsdeVPX + 1):	Next
'		For each y in targetcristalprimitive:y.visible = TargetsdeVPX:	Next Primitives in collection refuse to be animated :(
		psw33a.visible = TargetsdeVPX
		psw34a.visible = TargetsdeVPX
		psw35a.visible = TargetsdeVPX
		psw36a.visible = TargetsdeVPX
		psw37a.visible = TargetsdeVPX
		psw38a.visible = TargetsdeVPX
		psw39a.visible = TargetsdeVPX
'Set the lights color depending on choose
	select case GILightsColor 
		case 0 'caso 0 normales
			changetonormal
		case 1 'caso 1 Azul
			changetoblue
		case 2 'caso 2 Violeta
			changetoviolet
	end Select
	
'Flippers
	if decalonflippers = 0 Then
		RightFlipper.image = "flipper_w_red_Right"
		LeftFlipper.image = "flipper_w_red_Right"
	end if

'Surfaces
	Primitive22.visible = Primitivetransplastics

	dim plast
	For each plast in transparentsurfaces : plast.visible = (-Primitivetransplastics + 1 ) : Next


'******  end of options *****



	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled  = True
	vpmNudge.TiltSwitch=56
	vpmNudge.Sensitivity=2
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)

    Set bsTrough=New cvpmBallStack
		bsTrough.InitSw 0,14,13,12,11,10,0,0
		bsTrough.InitKick BallRelease,90,5
		bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsTrough.Balls=5

    Set bsTrough2=New cvpmBallStack
		bsTrough2.InitSaucer BallRelease2,15,90,5

    Set bsLock=New cvpmBallStack
		bsLock.InitSw 0,45,41,42,43,44,0,0
		bsLock.InitKick Kicker2,110,3
		bsLock.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

	set DropTarget=new cvpmDropTarget
		DropTarget.initdrop sw9,9
		DropTarget.InitSnd SoundFX("DTDrop",DOFDropTargets),SoundFX("DTReset",DOFContactors)

	Set TSPINA = New cvpmTurntable
		TSPINA.InitTurntable SpinTrigger, 25
		TSPINA.SpinDown = 10
		TSPINA.CreateEvents "TSPINA"

	Set magnetdisco = New cvpmMagnet
		magnetdisco.InitMagnet iman, 15
		magnetdisco.strength = 8
		magnetdisco.GrabCenter = false
		magnetdisco.MagnetOn = False
		magnetdisco.CreateEvents "magnetdisco"


	Set MagnetDiverter=new cvpmMagnet
		MagnetDiverter.initMagnet TrMagDiv,20

Const MusicOn = True 'Change it to False if you do not want the extra music

    If MusicOn Then PLaySound "MdMx", -1

end sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)

	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)

	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
	If KeyUpHandler(keycode) Then Exit Sub
End Sub

	Dim plungerIM
    Const IMPowerSetting = 24 'Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .switch 16
        .InitExitSnd  SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .CreateEvents "plungerIM"
    End With

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsoundat "drain", drain : End Sub
Sub BallRelease2_Hit:bsTrough2.AddBall 0 : playsoundat "popper_ball", BallRelease2: End Sub
Sub Lock1_Hit:bsLock.AddBall Me : playsoundat "popper_ball", lock1: End Sub

'Drop Targets
 Sub Sw9_Dropped:DropTarget.Hit 1 :End Sub  

'Wire Triggers
Sub sw16_Hit: : PlaySoundAtBall "rollover" : light003.state=2:End Sub 
Sub sw16_UnHit:light003.state=0:End Sub
Sub sw17_Hit:Controller.Switch(17) = 1 : PlaySoundAtBallVol "rollover",1 : End Sub 
Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub
Sub sw18_Hit:Controller.Switch(18) = 1 : PlaySoundAtBallVol "rollover",1 : End Sub 
Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub
Sub sw19_Hit:Controller.Switch(19) = 1 : PlaySoundAtBallVol "rollover",1 : End Sub 
Sub sw19_UnHit:Controller.Switch(19) = 0:End Sub
Sub sw21_Hit:Controller.Switch(21) = 1 : PlaySoundAtBallVol "rollover",1 : End Sub 
Sub sw21_UnHit:Controller.Switch(21) = 0:End Sub
Sub sw23_Hit:Controller.Switch(23) = 1 : PlaySoundAtBallVol "rollover",1: End Sub 
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
Sub sw25_Hit:Controller.Switch(25) = 1 : End Sub 
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
Sub sw57_Hit:Controller.Switch(57) = 1 : PlaySoundAtBallVol "rollover", 1: End Sub 
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
Sub sw58_Hit:Controller.Switch(58) = 1 : PlaySoundAtBallVol "rollover",1 : End Sub 
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
Sub sw61_Hit:Controller.Switch(61) = 1 : PlaySoundAtBallVol "rollover",1 : End Sub 
Sub sw61_UnHit:Controller.Switch(61) = 0:End Sub
Sub sw62_Hit:Controller.Switch(62) = 1 : PlaySoundAtBallVol "rollover", 1: End Sub 
Sub sw62_UnHit:Controller.Switch(62) = 0:End Sub


'Spinners
Sub sw20_Spin:vpmTimer.PulseSw 20 : playsoundat "fx_spinner", sw20 : End Sub
Sub sw22_Spin:vpmTimer.PulseSw 22 : playsoundat "fx_spinner", sw22 : End Sub

'Ramp Triggers sound by collection
Sub sw24_Hit:vpmTimer.PulseSw 24:End Sub 
Sub sw40_Hit:vpmTimer.PulseSw 40:End Sub 

'Stand Up Targets sound by collection cardinal points
Sub sw26_Hit:vpmTimer.PulseSw 26:End Sub
Sub sw27_Hit:vpmTimer.PulseSw 27:End Sub
Sub sw30_Hit:vpmTimer.PulseSw 30:End Sub
Sub sw31_Hit:vpmTimer.PulseSw 31:End Sub

'Mini Targets
Sub sw28_Hit:vpmTimer.PulseSw 28:End Sub
Sub sw29_Hit:vpmTimer.PulseSw 29:End Sub


'Sub sw33_Hit:vpmTimer.PulseSw 33:End Sub
'Sub sw34_Hit:vpmTimer.PulseSw 34:End Sub
'Sub sw35_Hit:vpmTimer.PulseSw 35:End Sub
'Sub sw36_Hit:vpmTimer.PulseSw 36:End Sub
'Sub sw37_Hit:vpmTimer.PulseSw 37:End Sub
'Sub sw38_Hit:vpmTimer.PulseSw 38:End Sub
'Sub sw39_Hit:vpmTimer.PulseSw 39:End Sub


'***********************'Banco de Hits 1*****************

Sub sw33_Hit:vpmTimer.PulseSw 33:psw33a.RotX = -7:Me.TimerEnabled = 1:End Sub
Sub sw33_Timer:psw33a.RotX = 0:Me.TimerEnabled = 0:End Sub 
Sub sw34_Hit:vpmTimer.PulseSw 34:psw34a.RotX = -7:Me.TimerEnabled = 1:End Sub
Sub sw34_Timer:psw34a.RotX = 0:Me.TimerEnabled = 0:End Sub 
Sub sw35_Hit:vpmTimer.PulseSw 35:psw35a.RotX = -7:Me.TimerEnabled = 1:End Sub
Sub sw35_Timer:psw35a.RotX = 0:Me.TimerEnabled = 0:End Sub 

Sub sw36_Hit:vpmTimer.PulseSw 36:psw36a.RotX = -1:Me.TimerEnabled = 1:End Sub
Sub sw36_Timer:psw36a.RotX = 0:Me.TimerEnabled = 0:End Sub 

'***********************'Banco de Hits 2*****************

Sub sw37_Hit:vpmTimer.PulseSw 37:psw37a.RotX = -10:Me.TimerEnabled = 1:End Sub
Sub sw37_Timer:psw37a.RotX = 0:Me.TimerEnabled = 0:End Sub 
Sub sw38_Hit:vpmTimer.PulseSw 38:psw38a.RotX = -7:Me.TimerEnabled = 1:End Sub
Sub sw38_Timer:psw38a.RotX = 0:Me.TimerEnabled = 0:End Sub 
Sub sw39_Hit:vpmTimer.PulseSw 39:psw39a.RotX = -7:Me.TimerEnabled = 1:End Sub
Sub sw39_Timer:psw39a.RotX = 0:Me.TimerEnabled = 0:End Sub







'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw(49) : PlaySoundAt SoundFX("fx_bumper1",DOFContactors), Bumper1: End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(50) : PlaySoundAt SoundFX("fx_bumper1",DOFContactors), Bumper2: End Sub
Sub Bumper3_Hit : vpmTimer.PulseSw(51) : PlaySoundAt SoundFX("fx_bumper1",DOFContactors), Bumper3: End Sub

'********************  Spinning Discs Animation Timer ****************************
Dim SpinnerMotorOff, SpinnerStep, ss

Sub SolSpinWheelsMotor(enabled)
	If enabled Then
		TSPINA.MotorOn = True
		SpinnerStep = 10
		SpinnerMotorOff = False
		SpinnerTimer.Interval = 10
		SpinnerTimer.enabled = True
	Else
		SpinnerMotorOff = True
		TSPINA.MotorOn = False
	end If
End Sub

Sub SpinnerTimer_Timer()
	If Not(SpinnerMotorOff) Then
		spina.ObjRotZ  = ss
		ss = ss + SpinnerStep
	Else
		if SpinnerStep < 0 Then
			SpinnerTimer.enabled = False
		Else
		'slow the rate of spin by decreasing rotation step
			SpinnerStep = SpinnerStep - 0.05
			
			spina.ObjRotZ  = ss
			ss = ss + SpinnerStep
		End If
	End If
	if ss > 360 then ss = ss - 360
End Sub


'MAGNET : DIVERTER (AT THE TOP OF THE PF)
Sub TrMagDiv_Hit
	Controller.Switch(32)=1
	ActiveBall.AngMomZ=0
	ActiveBall.AngMomY=0
	ActiveBall.AngMomX=0

	MagnetDiverter.AddBall ActiveBall
	MagnetDiverter.AttractBall ActiveBall
End Sub	

Sub TrMagDiv_unHit
	Controller.Switch(32)=0
	MagnetDiverter.RemoveBall ActiveBall
End Sub

'*************************************************************************
'*************************************************************************

'Generic Sounds
sub Trigger1_hit:stopSound "Wire Ramp":magnetdisco.GrabCenter = True:PlaySoundAt "fx_ballrampdrop", Trigger1:End Sub

sub Trigger2_hit:PlaySoundAt "Wire Ramp", Trigger2:End Sub

'pared mball
sub Trigger4_hit:PlaySoundAt "Wire Ramp", Trigger4:End Sub

sub Trigger5_hit:PlaySoundAt "Wire Ramp", Trigger5:End Sub

sub Trigger6_hit:stopSound "Wire Ramp":PlaySoundAt "fx_ballrampdrop", Trigger6:End Sub

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
    NFadeL 1, l1
    NFadeL 2, l2
    NFadeL 3, l3
    NFadeL 4, l4
    NFadeL 5, l5
    NFadeL 6, l6
    NFadeL 9, l9
    NFadeL 17, l17b 'Dorthy LED
    NFadeL 18, l18b 'Dorthy LED
    NFadeL 19, l19b 'Dorthy LED
    NFadeL 20, l20b 'Dorthy LED
    NFadeL 21, l21b 'Dorthy LED
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
	NFadeL 50, L50
	NFadeL 51, L51
	NFadeLm 57, l57a
	NFadeL 57, l57 'Bumper
	NFadeLm 58, l58a
	NFadeL 58, l58 'Bumper
	NFadeLm 59, l59a
	NFadeL 59, l59 'Bumper
	NFadeL 60, L60
    NFadeL 61, l61
	NFadeL 62, l62
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


'Solenoid Controlled Lights

NFadeLm 122, S122a 'Dome
NFadeLm 122, S122b 'Dome
NFadeLm 122, S122c
NFadeL 122, S122d

NFadeLm 123, S123a 'Dome
NFadeL 123, S123a1

NFadeLm 125, S125a
NFadeLm 125, F125b1
NFadeLm 125, F125b2
NFadeL 125, S125b

NFadeLm 126, S126a 'Dome
NFadeLm 126, S126b
NFadeL 126, S126c

NFadeLm 127, S127a
NFadeL 127, S127b

NFadeLm 128, S128a
NFadeLm 128, S128b
NFadeLm 128, S128a1
NFadeLm 128, S128b1
NFadeLm 128, S128c1
NFadeL 128, S128c

NFadeLm 129, S129 'Dome
NFadeLm 129, S129b 'Dome
NFadeL 129, S129a


'Flashm 130, Flasher130
NFadeLm 130, S130a 'Dome
NFadeLm 130, S130a1
NFadeL 130, S130b



NFadeLm 131, S131a 'Dome
NFadeLm 131, S131a1 'Dome
NFadeL 131, S131b

NFadeLm 132, S132a 'Dome
NFadeL 132, S132a1 'Dome


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





'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 62
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05,0,0,1,AudioFade(gi1)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 59
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05,0,0,1,AudioFade(gi3)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0
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
	'debug.print now &" " & soundname 
    PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
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

Function BallRollVol(ball) ' Calculates the Volume of the sound based on the ball speed
   BallRollVol = Csng(BallVel(ball) ^2 / (80000 - (79900 * Log(RollVol) / Log(100))))
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
		If BallVel(BOT(b) ) > 1 Then
			rolling(b) = True
			if BOT(b).z < 30 Then ' Ball on playfield
						PlaySound("fx_ballrolling" & b), -1, BallRollVol(BOT(b) )*.8, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else ' Ball on raised ramp
						PlaySound("fx_ballrolling" & b), -1, BallRollVol(BOT(b) )*.2, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
				End If
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
    dim mball
    for each mball in magnetdisco.Balls
        if mball.ID = ball1.ID OR mball.ID = ball2.ID then exit sub
    next
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
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

Dim NextOrbitHit:NextOrbitHit = 0 


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	debug.print "Gate"
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



'Sub WireRampBumps_Hit(idx)
'	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
'		RandomBump3 .5, Pitch(ActiveBall)+5
'		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
'		' Lowering these numbers allow more closely-spaced clunks.
'		NextOrbitHit = Timer + .2 + (Rnd * .2)
'	end if 
'End Sub

'Sub MetalGuideBumps_Hit(idx)
'	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
'		RandomBump2 2, Pitch(ActiveBall)
'		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
'		' Lowering these numbers allow more closely-spaced clunks.
'		NextOrbitHit = Timer + .2 + (Rnd * .2)
'	end if 
'End Sub

Sub PlasticBump1_Hit
	RandomBump 1, Pitch(ActiveBall)
End Sub

Sub PlasticBump2_Hit
	RandomBump 1, Pitch(ActiveBall)
End Sub


Sub PlasticRamps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, Pitch(ActiveBall)
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .1 + (Rnd * .2)
	end if 
End Sub

Sub Metals_Thin_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, 20000 'Increased pitch to simulate metal wall
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub

'' Requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)+voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

'luz el color se puede dar por nn.color=RGB(rojo ,verde ,azul)


sub changetonormal
	dim luz
	For each luz in GI:luz.color = RGB(255,155,64):	Next
end sub 

sub changetoblue
	dim luz
	For each luz in GI:luz.color = RGB(0,12,255):	Next
end sub 

sub changetoviolet
	dim luz
	For each luz in GI:luz.color = RGB(189,108,255):	Next
end sub 




' Timer for added effects based on lights states
sub effectcheckT_Timer

	if S130a.state=1 then:Primitive82.visible=0:Primitive70.visible=1:end If
	if S130a.state=0 then:Primitive82.visible=1:Primitive70.visible=0:end If


	if S129b.state=1 then:Primitive58.visible=0:Primitive72.visible=1:end If
	if S129b.state=0 then:Primitive58.visible=1:Primitive72.visible=0:end If

end Sub

'******************
' Rom sound ON wwhen close table
'******************
Sub Table1_Exit()
    Controller.Pause = False
    Controller.Stop
    Controller.Games("twst_405").Settings.Value("sound") = 1
    Controller.Games("twst_405").Settings.Value("samples") = 1
End Sub
