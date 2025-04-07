'***************************************************************************************************************************************************************
'***************************************************************************************************************************************************************
'       _     ____  ____   __        __            _      _   _____                   
'      / \   / ___|| __ )  \ \      / /___   _ __ | |  __| | |_   _|___   _   _  _ __ 
'     / _ \ | |  _ |  _ \   \ \ /\ / // _ \ | '__|| | / _` |   | | / _ \ | | | || '__|
'    / ___ \| |_| || |_) |   \ V  V /| (_) || |   | || (_| |   | || (_) || |_| || |   
'   /_/   \_\\____||____/     \_/\_/  \___/ |_|   |_| \__,_|   |_| \___/  \__,_||_|   
'                                                                                      
'***************************************************************************************************************************************************************
'***************************************************************************************************************************************************************                                                                      
                                                                       

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'Const cGameName="wrldtou3"
Const cGameName="wrldtou2"
Const UseSolenoids=2
Const UseLamps=0
Const UseGI=0
Const SCoin="coin"

LoadVPM "01300000","alvinG.VBS",3.10

Dim CDMotor,Perf,FRubber,CState

'***************************************************************************************************************************************************************
'***************************************************************************************************************************************************************

'    _____       _      _          ___          _    _                    
'   |_   _|__ _ | |__  | |  ___   / _ \  _ __  | |_ (_)  ___   _ __   ___ 
'     | | / _` || '_ \ | | / _ \ | | | || '_ \ | __|| | / _ \ | '_ \ / __|
'     | || (_| || |_) || ||  __/ | |_| || |_) || |_ | || (_) || | | |\__ \
'     |_| \__,_||_.__/ |_| \___|  \___/ | .__/  \__||_| \___/ |_| |_||___/
'                                       |_|                               

'CD Disk Motor Noise (as it can be a bit annoying)
'   Off =0
'   On = 1
'Change the value below to set option
CDMotor = 0

'Cabinet Clean Or Dirty
'   Clean = 0
'   Dirty = 1 
'Change the value below to set option
CState = 1

'Flipper Rubber Colour
'   Red    		= 0
'   Blue   		= 1
'   Yellow		= 2
'   Blue Clean	= 3
'Change the value below to set option
FRubber = 0

'Stupid over the top details, Disable if you have performance issues
'   Off =0
'   On = 1 
'Change the value below to set option
Perf = 0

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1)=		"SolKickback"
'SolCallback(2)=	"vpmSolSound ""Jet3"","
'SolCallback(3)=	"vpmSolSound ""Jet3"","
'SolCallback(4)=	"vpmSolSound ""Sling"","
'SolCallback(5)=	"vpmSolSound ""Sling"","
'SolCallback(6)=	"vpmSolSound ""Sling"","
SolCallback(9)=		"solAutofire"
SolCallback(11)=	"bsVUK.SolOut"
'SolCallback(12)=	"vpmSolSound ""Jet3"","
SolCallback(13)=	"bsTrough.SolIn"
SolCallback(14)=	"vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
'SolCallback(15)='Flasher Relay
SolCallback(16)=	"PFGI"
SolCallback(17)=	"SetLamp 117,"
SolCallback(18)=	"SetLamp 118,"
SolCallback(19)=	"SetLamp 119,"	'Top Ramp
SolCallback(20)=	"SetLamp 120,"	'Side Ramp
SolCallback(21)=	"SetLamp 121,"	'Left Side
SolCallback(22)=	"SetLamp 122,"	'Left Top
SolCallback(23)=	"SetLamp 123,"	'Right Top
SolCallback(24)=	"SetLamp 124,"	'Right Side
SolCallback(26)=	"solDisk"	'Motor Relay
SolCallback(27)=	"vpmNudge.SolGameOn"
'SolCallback(29)=	'Backbox GI
SolCallback(30)=	"bsTrough.SolOut"
SolCallback(31)=	"solback"	'top blast
SolCallback(32)=	"VukTopPop"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then		
		LF.fire 'LeftFlipper.RotateToEnd	
		LeftFlipper.RotateToEnd
		PlaySound SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), 0, 0.1
     Else		
		LeftFlipper.RotateToStart
		PlaySound SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), 0, 0.1
     End If
  End Sub
  
Sub SolRFlipper(Enabled)		
     If Enabled Then	
		RF.fire 'RightFlipper.RotateToEnd		
		RightFlipper.RotateToEnd
		PlaySound SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), 0, 0.1
     Else
		RightFlipper.RotateToStart
		PlaySound SoundFX("Flipper_Right_Down_" & Int(Rnd*7)+1,DOFFlippers), 0, 0.1
		
     End If
End Sub


'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

Sub SolKickback(Enabled)
     If Enabled Then
		KickBack.Fire
        PlaySound SoundFX("Popper",DOFContactors)
     Else
        KickBack.Pullback
     End If
End Sub

Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
End Sub


'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsVUK, turntable, TSpina, Bump1, Bump2, Bump3

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Anti"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=1
		.ShowTitle=0
		.hidden = 0
        .Games(cGameName).Settings.Value("sound")=1
		'.PuPHide = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

PinMAMETimer.Interval=PinMAMEInterval
PinMAMETimer.Enabled=1

vpmNudge.TiltSwitch=6
vpmNudge.Sensitivity=2
vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot,Bumper1,Bumper2,Bumper3)

Set bsTrough=New cvpmBallStack
	bsTrough.InitSw 10,13,12,11,0,0,0,0
	bsTrough.InitKick BallRelease,120,8
	bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
	bsTrough.Balls=2
	bsTrough.AddBall 0

Set bsVUK=New cvpmBallStack
	bsVUK.InitSaucer sw40,40,-90,25
 	bsVUK.KickZ= 1.4 '3.1415926/2 '90 degrees in radians
	bsVUK.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

Set TSPINA = New cvpmTurntable
	TSPINA.InitTurntable SpinTrigger, 80
	TSPINA.SpinDown = 10
	TSPINA.CreateEvents "TSPINA"

kickback.PullBack

sw23w.IsDropped = 1
sw39w.IsDropped = 1
sw40w.IsDropped = 1
Wall066.IsDropped=1
Wall065.IsDropped=0
Wall070.IsDropped=0

Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components
	gi026.intensity=16
	gi011.intensity=30
	gi010.intensity=30
	gi009.intensity=30
	gi027.intensity=8
	gi007.intensity=40
	gi006.intensity=40
	gi014.intensity=40
	gi020.intensity=14
Else
	gi026.intensity=0
	gi011.intensity=50
	gi010.intensity=50
	gi009.intensity=50
	gi027.intensity=3
	gi007.intensity=8
	gi006.intensity=8
	gi014.intensity=8
	gi020.intensity=8
End if

'Load LUT
LoadLUT

Select Case FRubber

	Case 0:
	LeftFlipperPrim.image = "FlipperL"
	RightFlipperPrim.image = "FlipperR"

	Case 1:

	LeftFlipperPrim.image = "FlipperL-Blue"
	RightFlipperPrim.image = "FlipperR-Blue"

	Case 2:

	LeftFlipperPrim.image = "FlipperL-Yellow"
	RightFlipperPrim.image = "FlipperR-Yellow"

	Case 3:

	LeftFlipperPrim.image = "FlipperL-Blue-Clean"
	RightFlipperPrim.image = "FlipperR-Blue-Clean"


End Select

Select Case CState

	Case 0:

	Primcab.image = "Cabinet-Clean"

	Case 1:

	Primcab.image = "Cabinet"


End Select

End Sub



'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress
	If keycode = LeftMagnaSave Then bLutActive = True
	If keycode = RightMagnaSave Then 
		If bLutActive Then NextLUT: End If
	End If
	If KeyCode=PlungerKey Then Controller.Switch(37)=1
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If keycode = LeftMagnaSave Then bLutActive = False
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
	If KeyCode=PlungerKey Then Controller.Switch(37)=0
	If KeyUpHandler(keycode) Then Exit Sub
End Sub

	' Impulse Plunger
	dim plungerIM

    ' Impulse Plunger
    Const IMPowerSetting = 60 'Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.2
        .switch 14
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .CreateEvents "plungerIM"
    End With

'*********
'   LUT
'*********

Dim bLutActive, LUTImage
Sub LoadLUT
	dim x
	bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 12: UpdateLUT: SaveLUT: End Sub

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
Case 10: table1.ColorGradeImage = "LUT10"
Case 11: table1.ColorGradeImage = "LUT11"
End Select
End Sub

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub

 '***********************************
 'Vertical Kick to Guitar sw38
 '***********************************
 'Variables used for VUK 
 Dim raiseballsw, raiseball

 Sub TopVUK_Hit() 
 	TopVUK.Enabled=FALSE
	Controller.switch (38) = True
	playsound "popper_ball"
 End Sub
 
 Sub VukTopPop(enabled)
	if(enabled and Controller.switch (38)) then
		TopVUK.DestroyBall
 		Set raiseball = TopVUK.CreateBall
 		playsound SoundFX("Popper",DOFContactors)
 		raiseballsw = True
 		TopVukraiseballtimer.Enabled = True 'Added by Rascal
		TopVUK.Enabled=TRUE	
 		Controller.switch (38) = False
	else

	end if
End Sub
 
 Sub TopVukraiseballtimer_Timer()
 	If raiseballsw = True then
 		raiseball.z = raiseball.z + 8
		raiseball.x = raiseball.x + 0.2
 		If raiseball.z > 142 then
			playsound "kicker_enter", 0 ,1
 			TopVUK.Kick 300, 18
 			Set raiseball = Nothing
			playsound "fx_ball_drop4", 0 ,1
 			TopVukraiseballtimer.Enabled = False
 			raiseballsw = False
 		End If
 	End If
 End Sub

'Wire Triggers
Sub sw14_Hit:Controller.Switch(14)=1 : PlaySoundAt"rollover",sw14 : End Sub 
Sub sw14_unHit:Controller.Switch(14)=0:End Sub
Sub sw30_Hit:Controller.Switch(30)=1 : playsoundAt"rollover",sw30 : End Sub 
Sub sw30_unHit:Controller.Switch(30)=0:End Sub
Sub sw31_Hit:Controller.Switch(31)=1 : playsoundAt"rollover",sw31 : End Sub 
Sub sw31_unHit:Controller.Switch(31)=0:End Sub
Sub sw32_Hit:Controller.Switch(32)=1 : playsoundAt"rollover",sw32 : End Sub 
Sub sw32_unHit:Controller.Switch(32)=0:End Sub
Sub sw33_Hit:Controller.Switch(33)=1 : playsoundAt"rollover",sw33 : End Sub 
Sub sw33_unHit:Controller.Switch(33)=0:End Sub
Sub sw34_Hit:Controller.Switch(34)=1 : playsoundAt"rollover",sw34 : End Sub 
Sub sw34_unHit:Controller.Switch(34)=0:End Sub
Sub sw35_Hit:Controller.Switch(35)=1 : playsoundAt"rollover",sw35 : End Sub 
Sub sw35_unHit:Controller.Switch(35)=0:End Sub
Sub sw36_Hit:Controller.Switch(36)=1 : playsoundAt"rollover",sw36 : End Sub 
Sub sw36_unHit:Controller.Switch(36)=0:End Sub
Sub sw42_Hit:Controller.Switch(42)=1 : playsoundAt"rollover",sw42 : End Sub 
Sub sw42_unHit:Controller.Switch(42)=0:End Sub
Sub sw43_Hit:Controller.Switch(43)=1 : playsoundAt"rollover",sw43 : End Sub 
Sub sw43_unHit:Controller.Switch(43)=0:End Sub
Sub sw44_Hit:Controller.Switch(44)=1 : playsoundAt"rollover",sw44 :	Wall066.IsDropped=0 : End Sub 
Sub sw44_unHit:Controller.Switch(44)=0:End Sub

'Bumpers
Sub Bumper3_Hit : vpmTimer.PulseSw(19) : playsoundAt SoundFX("fx_bumper1",DOFContactors),Bumper3: End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(18) : playsoundAt SoundFX("fx_bumper1",DOFContactors),Bumper2: End Sub
Sub Bumper1_Hit : vpmTimer.PulseSw(17) : playsoundAt SoundFX("fx_bumper1",DOFContactors),Bumper1: End Sub


'Lock Ball
Sub sw23_Hit:sw23w.IsDropped=0:Controller.Switch(23)=1 : playsound"rollover" : End Sub 
Sub sw23_UnHit:sw23w.IsDropped=1:Controller.Switch(23)=0:End Sub
Sub sw39_Hit:sw39w.IsDropped=0:Controller.Switch(39)=1 : playsound"rollover" : End Sub 
Sub sw39_UnHit:sw39w.IsDropped=1:Controller.Switch(39)=0:End Sub
Sub sw40_Hit:bsVUK.AddBall 0:sw40w.IsDropped=0 : PlaySoundAt "popper_ball",sw40: End Sub
Sub sw40_UnHit:sw40w.IsDropped=1:End Sub

'Stand Up Targets
Sub sw25_Hit:vpmTimer.PulseSw 25:PlaySoundAt SoundFX("target",DOFTargets),sw25:End Sub
Sub sw26_Hit:vpmTimer.PulseSw 26:PlaySoundAt SoundFX("target",DOFTargets),sw26:End Sub
Sub sw27_Hit:vpmTimer.PulseSw 27:PlaySoundAt SoundFX("target",DOFTargets),sw27:End Sub
Sub sw28_Hit:vpmTimer.PulseSw 28:PlaySoundAt SoundFX("target",DOFTargets),sw28:End Sub
Sub sw29_Hit:vpmTimer.PulseSw 29:PlaySoundAt SoundFX("target",DOFTargets),sw29:End Sub
Sub sw46_Hit:vpmTimer.PulseSw 46:PlaySoundAt SoundFX("target",DOFTargets),sw46:End Sub
Sub sw47_Hit:vpmTimer.PulseSw 47:PlaySoundAt SoundFX("target",DOFTargets),sw47:End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:PlaySoundAt SoundFX("target",DOFTargets),sw48:End Sub
Sub sw49_Hit:vpmTimer.PulseSw 49:PlaySoundAt SoundFX("target",DOFTargets),sw49:End Sub
Sub sw50_Hit:vpmTimer.PulseSw 50:PlaySoundAt SoundFX("target",DOFTargets),sw50:End Sub
Sub sw51_Hit:vpmTimer.PulseSw 51:PlaySoundAt SoundFX("target",DOFTargets),sw51:End Sub
Sub sw52_Hit:vpmTimer.PulseSw 52:PlaySoundAt SoundFX("target",DOFTargets),sw52:End Sub

'Ramp Triggers
Sub sw41_Hit:vpmTimer.PulseSw 41:End Sub
Sub sw45_Hit:vpmTimer.PulseSw 45:End Sub

'Top Blast
dim Tonce ' trigger once
Tonce = 0

Sub solback(Enabled)
	Wall065.IsDropped=1 'Plunger
	Wall070.IsDropped=1	'Top protector
End Sub

Sub Trigger002_Hit()
	Wall066.IsDropped=1 'Box escape
	Wall070.IsDropped=0	'Top protector
	Wall065.IsDropped=0 'Plunger
	Tonce = 0 'Reset trigger once
End Sub

Sub Trigger003_Hit()
	if Tonce = 0 then
		PlaySoundAt "Kicker_Release", Trigger003
		Wall066.IsDropped=0 'Box escape
		Tonce = 1
	end if
End Sub



'**************************************************************************************************************************************************
'    Spinning Disc Subs
'**************************************************************************************************************************************************


Dim SpinnerMotorOff, SpinnerStep, ss

Sub SolDisk(enabled)
	If enabled Then
		TSPINA.MotorOn = True
		SpinnerStep = 10
		SpinnerMotorOff = False
		SpinnerTimer.Interval = 10
		SpinnerTimer.enabled = True
		PlaySound "fx_relay",0,1
	Else
		SpinnerMotorOff = True
		TSPINA.MotorOn = False
	end If
End Sub

Sub SpinnerTimer_Timer()
	If Not(SpinnerMotorOff) Then
		If CDMotor = 1 then
			PlaySound "Motor",0,1,0.0,0.0,-40,1,1
        End If
		spina.RotY  = ss
		ss = ss + SpinnerStep
	Else
		if SpinnerStep < 0 Then
			SpinnerTimer.enabled = False
			stopSound "Motor"
		Else
		'slow the rate of spin by decreasing rotation step
			SpinnerStep = SpinnerStep - 0.05		
			spina.RotY  = ss
			ss = ss + SpinnerStep
		End If
	End If
	if ss > 360 then ss = ss - 360
End Sub




'***************************
'   Gi & GI Effects
'***************************

dim fon,foff

GiON
GiEffects.Enabled=1
GiMaskOff.Enabled=1:foff=1

'Playfield GI
Sub PFGI(Enabled)
	If Enabled Then
		PlaySound "fx_relay", 0,0.15	
		GiOFF
		PrimitiveRampSides.image = "Ramp-Left"
		PrimWireRamps.image = "wires-ramps-OFF"
		'PrimitiveLightRamp.image = "Ramp-Left"
		MaterialColor "Rampshine",RGB(255,255,255)
		MaterialColor "Plastic Guard",RGB(255,255,255)
		MaterialColor "fIDDLY bITS",RGB(255,255,255)
		GiMaskOn.Enabled=1:fon=1
	Else
		PlaySound "fx_relay", 0,0.15	
		GiON
		PrimitiveRampSides.image = "Ramp-Left-on"
		PrimWireRamps.image = "wires-ramps"
		'PrimitiveLightRamp.image = "Ramp-Left-on"
		MaterialColor "Rampshine",RGB(255,229,174)
		MaterialColor "fIDDLY bITS",RGB(255,200,174)
		MaterialColor "Plastic Guard",RGB(255,216,117)
		GiMaskOff.Enabled=1:foff=1
	End If
End Sub

Sub GiON
	dim xx
	For each xx in GI
		xx.State = 1
	Next
End Sub

Sub GiOFF
	dim xx
    For each xx in GI
        xx.State = 0
    Next
End Sub


Sub GiEffects_timer()
'debug.print gi001.State
		FadeDisableLighting Primitive002, 0.3, 0, 0.1, 0.15 'Guitar Fence
		FadeDisableLighting Primitive003, 0.2, 0, 0.066, 0.066 'Horseshoe Plastic Ramp
		FadeDisableLighting Primitive054, 0.8, 0.2, 0.2, 0.2 'Mirror
		FadeDisableLighting Primcab, 0.05, 0, 0.01, 0.01 'Cabinet
		FadeDisableLighting Primitive013, 0.1, 0, 0.025, 0.025 'CD Box
		FadeDisableLighting LeftFlipperPrim, 0.15, 0, 0.05, 0.05 'Left Flipper
		FadeDisableLighting RightFlipperPrim, 0.15, 0, 0.05, 0.05 'Right Flipper
		FadeDisableLighting Wall005, 0.2, 0, 0.066, 0.066 'Ramp Bits
		FadeDisableLighting Primitive055, 0.1, 0, 0.033, 0.033 'Metal guides
		FadeDisableLighting PrimWireRamps001, 0.4, 0, 0.133, 0.133 'Wire ramp
		FadeDisableLighting PrimWireRamps002, 0.5, 0, 0.166, 0.166 'Wire ramp
		FadeDisableLighting PrimWireRamps003, 0.7, 0, 0.233, 0.233 'Wire ramp
		FadeDisableLighting PrimWireRamps, 0.5, 0, 0.166, 0.166 'Wire ramp (Bottom Right)
		FadeDisableLighting Primitive008, 0.4, 0.2, 0.066, 0.066 'Plastic Guard
		FadeDisableLighting Primitive077, 0.2, 0, 0.066, 0.066 'Drummer
		FadeDisableLighting Primitive050, 0.3, 0.14, 0.053, 0.053 'SignPost
		FadeDisableLighting Primitive045, 0.8, 0, 0.1, 0.15 'Ramp Left Lid
		FadeDisableLighting PrimitiveRampSides, 0.4, 0, 0.1, 0.2 'Left Ramp Sides
		FadeDisableLighting PrimitiveLightRamp, 0.4, 0, 0.133, 0.133 'Left Ramp Bottom

		'Plastics	
		FadeDisableLighting Primiplas1, 0.2, 0, 0.066, 0.066 'Plastic 1-1
		FadeDisableLighting Primiplas2, 0.2, 0, 0.066, 0.066 'Plastic 1-2
		FadeDisableLighting Primiplas3, 0.2, 0, 0.066, 0.066 'Plastic 2-1
		FadeDisableLighting Primiplas4, 0.2, 0, 0.066, 0.066 'Plastic 2-2
		FadeDisableLighting gwall, 0.3, 0, 0.1, 0.1 'Guitar Playfield

		If Perf = 1 then
			FadeDisableLighting Primitive004, 0.1, 0, 0.025, 0.025 'Guitar Scoop		
			FadeDisableLighting Primtargets, 0.2, 0, 0.66, 0.66 'Cd Targets
			FadeDisableLighting lockPin010, 1, 0.4, 0.2, 0.2 'Lockpin010
			'Pegs
			FadeDisableLighting PegPlasticT007, 0.4, 0, 0.133, 0.133
			FadeDisableLighting PegPlasticT008, 0.4, 0, 0.133, 0.133
		End if	
End Sub


Sub GiMaskOn_timer()

	Select Case fon 
		Case 1:Flasher004.IntensityScale = 0.33	:FlasherGI1.IntensityScale = 0.66	:fon=2
		Case 2:Flasher004.IntensityScale = 0.66	:FlasherGI1.IntensityScale = 0.33	:fon=3
		Case 3:Flasher004.IntensityScale = 1	:FlasherGI1.IntensityScale = 0		:fon=4
		Case 4:GiMaskOn.Enabled=0
	End Select

End Sub

Sub GiMaskOff_timer()
	
	Select Case foff
		Case 1:Flasher004.IntensityScale = 0.66	:FlasherGI1.IntensityScale = 0.33	:foff=2
		Case 2:Flasher004.IntensityScale = 0.33	:FlasherGI1.IntensityScale = 0.66	:foff=3
		Case 3:Flasher004.IntensityScale = 0	:FlasherGI1.IntensityScale = 1		:foff=4
		Case 4:GiMaskOff.Enabled=0
	End Select

End Sub


'Fade DisableLighting (object, starting brightness, ending brightness, speed on, speed off)

Sub FadeDisableLighting(a, alvlu, alvld, spdu, spdd)

	Select Case gi001.State

		Case 1: 
			a.UserValue = a.UserValue + spdu
				If a.UserValue > alvlu Then 
					a.UserValue = alvlu
				end If
			a.BlendDisableLighting = a.UserValue 'On

		Case 0:
			a.UserValue = a.UserValue - spdd
				If a.UserValue < alvld Then 
					a.UserValue = alvld
				end If
			a.BlendDisableLighting = a.UserValue 'Off

		End Select

End Sub
 
'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
'       Plus Anti fading disabled lighting
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingState(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

'Fire once var
Dim plo119,plo124,plo120
plo119 = 0:plo124 = 0:plo120 = 0

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1)       'keep the real state in an array
            FadingState(chgLamp(ii, 0)) = chgLamp(ii, 1) + 3 'fading step
        Next
    End If
    'UpdateLeds
    UpdateLamps
End Sub

Sub UpdateLamps

  	Lamp 1, l1
  	Lamp 2, l2
  	Lamp 3, l3
  	Lamp 4, l4
  	Lamp 5, l5
  	Lamp 6, l6
  	Lamp 7, l7
  	Lamp 8, l8
  	Lamp 9, l9
  	Lamp 10, l10
  	Lamp 11, l11
  	Lamp 12, l12
  	Lamp 13, l13
  	Lamp 14, l14
  	Lamp 15, l15
  	Lamp 16, l16
  	Lamp 17, l17
  	Lamp 18, l18
  	Lamp 19, l19
  	Lamp 20, l20
  	Lamp 21, l21
  	Lamp 22, l22
  	Lamp 23, l23
  	Lamp 24, l24
  	Lamp 25, l25
  	Lamp 26, l26
  	Lamp 27, l27
  	Lamp 28, l28
  	Lamp 29, l29
  	Lamp 30, l30
  	Lamp 31, l31
  	Lamp 32, l32
  	Lamp 33, l33
  	Lamp 34, l34
  	Lamp 35, l35
  	Lamp 36, l36

'******************
'Sign post red lamp

  	Lampm 37, l37	
	NFadeObjm 37, P52, "LED-Red_a", "LED-Red"
	FDLm 37, p52, 1, 0.2
	FadeObjm 37, Primitive050, "Ramp-Sign_a", "Ramp-Sign_b", "Ramp-Sign_c", "Ramp-Sign"
	If Perf = 1 then
		Flashm 37, F37a
	Else
		F37a.visible = 0
	End if
	Flash 37, F37

'******************

  	Lamp 38, l38
  	Lamp 41, l41
  	Lamp 42, l42
  	Lamp 43, l43
  	Lamp 44, l44
  	Lamp 45, l45
  	Lamp 46, l46
  	Lamp 47, l47
  	Lamp 48, l48
  	Lamp 49, l49
  	Lamp 50, l50
  	Lamp 51, l52
  	Lamp 52, l51

  	Lampm 53, l53
	Flash 53, F53 'Mirror reflection

	Lampm 54, l54
	Flash 54, F54 'Mirror reflection
  	
  	Lamp 55, l55
  	Lamp 56, l56

'***********
'Bumper bass

	FadeObjm 65, Primitive007, "Bumper-Plastic-1_a", "Bumper-Plastic-1_b", "Bumper-Plastic-1_b", "Bumper-Plastic-1"	
	Lampm 65, l65a   ' Bumper
    Lampm 65, l65   ' Bumper
	If Perf = 1 then
		NFadeObjm 67, Wall94, "Bumpers_on", "Bumpers-Top"
	End If
	Flashm 65, F65
	FDL 65, Primitive007, 0.3, 0

'*************
'Bumper guitar

	FadeObjm 66, Flasherbase002, "Dome-Red_a","Dome-Red_b","Dome-Red_c", "Dome-Red"
	FadeObjm 66, Primitive005, "Bumper-Plastic-2_a", "Bumper-Plastic-2_b", "Bumper-Plastic-2_b", "Bumper-Plastic-2"
	If Perf = 1 then
		NFadeObjm 66, Wall156, "Bumpers_on", "Bumpers-Top"
	End If
    Lampm 66, l66b
    Lampm 66, l66
	Flashm 66, F66a
	Flashm 66, F66
	FDLm 66, Flasherbase002, 1, 0
	FDL 66, Primitive005, 0.3, 0

'*************
'Bumper singer

	FadeObjm 67, Flasherbase5, "Dome-Red_a","Dome-Red_b","Dome-Red_c", "Dome-Red"
	FadeObjm 67, Primitive006, "Bumper-Plastic-3_a", "Bumper-Plastic-3_b", "Bumper-Plastic-3_b", "Bumper-Plastic-3"
	If Perf = 1 then
		NFadeObjm 67, Wall002, "Bumpers_on", "Bumpers-Top"
	End If
    Lampm 67, l67b
    Lampm 67, l67
	Flashm 67, F67
	FDLm 67, Flasherbase5, 1, 0
	FDL 67, Primitive006, 0.3, 0

'*************

  	Lamp 62, l62
  	Lamp 63, l63
  	Lamp 64, l64
  	Lamp 170, l170
  	Lamp 171, l171
  	Lamp 172, l172
  	Lamp 173, l173

'*******************
'Solenoid Controlled
'*******************

	Lampm 117, S117a
	Lamp 117, S117 'Near Bumpers

	Lampm 118, S118a
	Lamp 118, S118 'Near Bumpers

'****
'F119 BHFlashers

	FadeObjm 119, Flasherbase001, "dome2basewhite_on", "dome2basewhite_on", "dome2basewhite", "dome2basewhite"
	FadeObjm 119, Flasherbase3, "dome2basewhite_on", "dome2basewhite_on", "dome2basewhite", "dome2basewhite"
	FDLm 119, Primitive013, 0.4, 0
	FDLm 119, Primitive054, 1, 0.8
	FDLm 119, Flasherbase001, 1, 0
	FDLm 119, Flasherbase3, 1, 0
	FDLm 119, spina, 0.65, 0
	Flashm 119, F119a
	Flashm 119, F119b
	Flashm 119, F119c
	Flashm 119, F119d
	Flash 119, F119

'****
'F120 Plastic ramp

	'Lampm 120, S120a
	FDLm 120, Primitive003, 1, 0.2
	FDLm 120, Wall005, 1, 0.2
	Flashm 120, F120a
	Flash 120, F120

'****
'F121 LeftRamp
	
	Lampm 121, S121
	Lampm 121, S121a
	FDLm 121, Primitive045, 1, 0.8
	Flashm 121, F121a
	Flash 121, F121

'**********
'F122 F123 Drummer,Speaker

	Lampm 122, S122a
	Flash 122, F122 'Drummer


	Lampm 123, S123a
	Flash 123, F123 'Speaker

'****
'F124 RightMiddle Flasher

	FadeObjm 124, Flasherbase1, "dome3_white_on","dome3_white_on","dome3_white_on","dome3_white"
	Lampm 124, S124b
	Lampm 124, S124a	
	Flashm 124, F124a
	FDLm 124, Flasherbase1, 1, 0
	Flash 124, F124



'*********************************************************************
'F124 Right Middle Flasher

If S124a.state = 1  and plo124 = 1  then

	PlaySound "fx_relay", 0,0.2	
	plo124 = 0

end if

If F124.IntensityScale = 0 and plo124 = 0  then

	plo124 = 1

end if

'*********************************************************************
'F119 Backbox Flashers

If F119.IntensityScale > 0.33 and plo119 = 1 then

	PlaySound "fx_relay", 0,0.1
	Primtargets.BlendDisableLighting = 0.4
	Primitive014.BlendDisableLighting = 0.4
	Primitive015.BlendDisableLighting = 0.2
	plo119 = 0

End If


If F119.IntensityScale = 0 and plo119 = 0 then

	Primtargets.BlendDisableLighting = 0.2
	Primitive014.BlendDisableLighting = 0
	Primitive015.BlendDisableLighting = 0
	plo119 = 1

End If


'*********************************************************************
'F120 Plastic ramp

If F120.IntensityScale > 0 and plo120 = 1 then

	MaterialColor "Rampshine",RGB(178,255,255)
	MaterialColor "fIDDLY bITS",RGB(178,255,255)
	plo120 = 0

End If


If F120.IntensityScale = 0 and plo120 = 0 then

	MaterialColor "Rampshine",RGB(255,229,174)
	MaterialColor "fIDDLY bITS",RGB(255,229,174)
	plo120 = 1

End If		


End Sub



'*********************************************************************

' Lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
    LampTimer.Interval = 30 ' flasher fading speed
    LampTimer.Enabled = 1
    For x = 0 to 200
        LampState(x) = 0
        FadingState(x) = 3 ' used to track the fading state
        FlashLevel(x) = 0
    Next
End Sub

Sub SetLamp(nr, value) ' 0 is off, 1 is on
    FadingState(nr) = abs(value) + 3
End Sub

' Lights: used for VPX standard lights, the fading is handled by VPX itself, they are here to be able to make them work together with the flashers

Sub Lamp(nr, object)
    Select Case FadingState(nr)
        Case 4:object.state = 1:FadingState(nr) = 0
        Case 3:object.state = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Lampm(nr, object) ' used for multiple lights, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:object.state = 1
        Case 3:object.state = 0
    End Select
End Sub


' Flashers: 4 is on,3,2,1 fade steps. 0 is off

Sub Flash(nr, object)
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1:FadingState(nr) = 0
        Case 3:Object.IntensityScale = 0.66:FadingState(nr) = 2
        Case 2:Object.IntensityScale = 0.33:FadingState(nr) = 1
        Case 1:Object.IntensityScale = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1
        Case 3:Object.IntensityScale = 0.66
        Case 2:Object.IntensityScale = 0.33
        Case 1:Object.IntensityScale = 0
    End Select
End Sub


'Fade DisableLighting (object, start state, end state)


Sub FDL(nr, object, ss, es)

	Select Case FadingState(nr)

		Case 4:Object.BlendDisableLighting = ss:FadingState(nr) = 0
        Case 3:Object.BlendDisableLighting = ss / 1.4:FadingState(nr) = 2
        Case 2:Object.BlendDisableLighting = ss / 2.5:FadingState(nr) = 1
        Case 1:Object.BlendDisableLighting = es:FadingState(nr) = 0

	End Select

End Sub


Sub FDLm(nr, object, ss, es)

	Select Case FadingState(nr)

		Case 4:Object.BlendDisableLighting = ss
        Case 3:Object.BlendDisableLighting = ss / 1.4
        Case 2:Object.BlendDisableLighting = ss / 2.5
        Case 1:Object.BlendDisableLighting = es

	End Select

End Sub


'Fade DisableLighting two state (object, start state, end state)

Sub NFDLm(nr, object, starts, ends)

	Select Case FadingState(nr)

		Case 4:Object.BlendDisableLighting = starts
        Case 3:Object.BlendDisableLighting = ends


	End Select

End Sub


' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub Reel(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1:FadingState(nr) = 0
        Case 3:object.SetValue 2:FadingState(nr) = 2
        Case 2:object.SetValue 3:FadingState(nr) = 1
        Case 1:object.SetValue 0:FadingState(nr) = 0
    End Select
End Sub

Sub Reelm(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1
        Case 3:object.SetValue 2
        Case 2:object.SetValue 3
        Case 1:object.SetValue 0
    End Select
End Sub

Sub NFadeReel(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1:FadingState(nr) = 1
        Case 3:object.SetValue 0:FadingState(nr) = 0
    End Select
End Sub

Sub NFadeReelm(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1
        Case 3:object.SetValue 0
    End Select
End Sub

'Texts

Sub Text(nr, object, message)
    Select Case FadingState(nr)
        Case 4:object.Text = message:FadingState(nr) = 0
        Case 3:object.Text = "":FadingState(nr) = 0
    End Select
End Sub

Sub Textm(nr, object, message)
    Select Case FadingState(nr)
        Case 4:object.Text = message
        Case 3:object.Text = ""
    End Select
End Sub

' Modulated Subs for the WPC tables

Sub SetModLamp(nr, level)
    FlashLevel(nr) = level / 150 'lights & flashers
End Sub

Sub LampMod(nr, object)          ' modulated lights used as flashers
    Object.IntensityScale = FlashLevel(nr)
    Object.State = 1             'in case it was off
End Sub

Sub FlashMod(nr, object)         'sets the flashlevel from the SolModCallback
    Object.IntensityScale = FlashLevel(nr)
End Sub

'Walls and mostly Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingState(nr)
        Case 4:object.image = a:FadingState(nr) = 0 'fading to off...
        Case 3:object.image = b:FadingState(nr) = 2
        Case 2:object.image = c:FadingState(nr) = 1
        Case 1:object.image = d:FadingState(nr) = 0
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingState(nr)
        Case 4:object.image = a
        Case 3:object.image = b
        Case 2:object.image = c
        Case 1:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingState(nr)
        Case 4:object.image = a:FadingState(nr) = 0 'off
        Case 3:object.image = b:FadingState(nr) = 0 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingState(nr)
        Case 4:object.image = a
        Case 3:object.image = b
    End Select
End Sub


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep, Lstep1

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 21
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
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
	vpmTimer.PulseSw 20
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub


Sub LeftSlingShot1_Slingshot
	vpmTimer.PulseSw 22
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    L1Sling.Visible = 0
    L1Sling1.Visible = 1
    sling3.rotx = 20
    LStep1 = 0
    LeftSlingShot1.TimerEnabled = 1
End Sub

Sub LeftSlingShot1_Timer
    Select Case LStep1
        Case 3:L1SLing1.Visible = 0:L1SLing2.Visible = 1:sling3.rotx = 10
        Case 4:L1SLing2.Visible = 0:L1SLing.Visible = 1:sling3.rotx = 0:LeftSlingShot1.TimerEnabled = 0:
    End Select
    LStep1 = LStep1 + 1
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
    PlaySound soundname, 0, 1, AudioPan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySound soundname, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0.4, 0, 0, 0, AudioFade(ActiveBall)
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

Const tnob = 19 ' total number of balls
Const lob = 0   'number of locked balls
Const maxvel = 54 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)



    ' render the shadow for each ball

        If BOT(b).X < Table1.Width/2 Then
            aBallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            aBallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        aBallShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            aBallShadow(b).visible = 1
        Else
            aBallShadow(b).visible = 0
        End If




        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) ) * 2
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, AudioPan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
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

'Rotate Flipper prims

	LeftFlipperPrim.RotZ = LeftFlipper.CurrentAngle
	RightFlipperPrim.RotZ = RightFlipper.CurrentAngle

End Sub



'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.



Sub Plastic_Hit(idx)
	PlaySound "ballhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit(idx)
	PlaySound SoundFX("target",DOFTargets), 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit(idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit(idx)
	PlaySound "metalhit_medium", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit(idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit(idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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

Sub Wall025_Slingshot
	PlaySound "Popper", 0, 1
	PlaySound "Solenoid", 0, 1
	PlaySoundAt "rail",Trigger002
End Sub

' Ramps sounds
Sub T4_hit():PlaySoundAt "ballhit3",T4: End Sub
Sub T5_hit():PlaySoundAt "ballhit3",T5: End Sub

' Ramps sounds
Sub RampSound1_Hit: PlaySoundAt "rail",RampSound1: End Sub
Sub RampSound2_Hit:PlaySoundAt"rail",RampSound7: End Sub
Sub RampSound3_Hit: PlaySoundAt "fx_metalrolling",sw40: End Sub
Sub RampSound7_Hit: PlaySoundAt "fx_metalrolling",RampSound7: End Sub
Sub RampSound9_Hit: PlaySoundAt "fx_ball_drop4",RampSound9: End Sub
Sub RampSound11_Hit: PlaySoundAt "fx_ball_drop4",RampSound11: End Sub
Sub RampSound12_Hit: PlaySoundAt "ballhit",RampSound12: End Sub
Sub RampSound10_Hit: PlaySoundAt "fx_ball_drop4",RampSound10: End Sub
Sub Gate2_Hit()
PlaySoundAt "fx_ball_drop2",Gate2
End Sub


' Stop Ramps Sounds
Sub RampSound4_Hit: StopSound "rail": StopSound "WireRamp":PlaySound "fx_ball_drop4": End Sub
Sub RampSound5_Hit: StopSound "rail": End Sub
Sub RampSound6_Hit: StopSound "fx_metalrolling": End Sub
Sub RampSound8_Hit: StopSound "fx_metalrolling": End Sub

Sub Table1_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub



'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
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

	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

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
				'playsound "knocker"
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
end sub


dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025

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
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce															'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then						'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)	'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	End If
End Sub

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	'LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	'RightFlipperCollide parm
End Sub

'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx)
	PlaySound "fx_rubber", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	PlaySound "fx_rubber", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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
'Shake 
'******************************************************
Dim Primitive004Step,shake

Sub Trigger001_Hit

	Dim max,min
	Primitive004Step = 0	
	max=2
	min=0.001
	shake = Int((max-min+1)*Rnd+min)
	Timer8.Enabled = True

End Sub


Sub Timer8_Timer()

	Primitive004Step = Primitive004Step + 1

	Select Case Primitive004Step
		Case 1:
			Primitive004.RotX = shake/2:Primitive004.RotY = shake/2
		Case 2:
			Primitive004.RotX = shake:Primitive004.RotY= shake
		Case 3:
			Primitive004.RotX = shake/1.5:Primitive004.RotY = shake/1.5
		Case 4:
			Primitive004.RotX = shake/2:Primitive004.RotY = shake/2
		Case 5:
			Primitive004.RotX  = shake/3:Primitive004.RotY = shake/3
		Case 6:
			Primitive004.RotX = 0:Primitive004.RotY = 0
		Case 7:
			Primitive004.RotX = -shake/15:Primitive004.RotY= -shake/15
		Case 9:
			Primitive004.RotX = -shake/2:Primitive004.RotY = -shake/2
		Case 10:
			Primitive004.RotX  = -shake/3:Primitive004.RotY = -shake/3
		Case 11:
			Primitive004.RotX  = 0:Primitive004.RotY = 0.1
		Case 12:
			Primitive004.RotX = -0.15:Primitive004.RotY = 0.4
		Case 13:
			Primitive004.RotX  =-0.2:Primitive004.RotY = 0.2
		Case 14:
			Primitive004.RotX  = 0.1:Primitive004.RotY = 0.1
		Case 15:
			Primitive004.RotX = 0:Primitive004.RotY = 0
			Timer8.Enabled = False
			Primitive004Step = 0
	End Select
End Sub
