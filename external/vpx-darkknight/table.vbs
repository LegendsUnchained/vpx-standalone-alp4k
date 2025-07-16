'                                                                                                                                                      
'                                                                                                                                                      
'        .                                                                                                                                     .       
'        :2B@B@B@B@@@B@B@@@@@@@B@B@@@B@B@B@B@B@B@@@@@B@                                           PB@B@@@B@B@B@B@@@B@@@@@B@B@B@B@@@B@B@B@B@B@Bki.      
'            :2@B@B@B@B@B@B@B@@@@@B@B@@@B@B@B@B@B@B@B@Bi                v:     :v                 B@B@@@B@B@@@B@B@B@B@B@@@B@B@B@B@B@B@B@B@qi           
'                iMB@B@B@B@B@@@B@B@B@B@B@B@B@B@B@B@B@B@B                @B     B@                8@@@@@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@v               
'                   Y@B@B@@@B@BBBBB@BBBBBBBBB@B@B@B@B@@@Z               M@:::::@M               7@@@B@B@B@BBBBBBB@B@B@B@B@B@B@B@@@B@F                  
'                     i@@B@B@B@B@B@B@B@BBB@BBBBBBB@B@B@@@M.             @@@B@B@B@              5@B@BBBBB@BBBBBBB@B@B@B@B@B@B@B@@@@Y                    
'                       r@B@@@B@B@BBB@B@B@B@BBB@B@B@BBB@B@B@M5;,       .B@B@B@B@Br       .:JE@B@B@B@B@B@B@B@B@B@B@B@B@B@BBB@B@B@u                      
'                         MB@B@B@BBB@BBB@B@B@BBB@B@B@BBB@B@B@B@@@B@B@B@B@B@B@B@B@B@B@B@B@@@B@B@@@B@B@B@BBBBBBBBB@BBB@B@B@B@B@B@                        
'                          FB@B@B@BBB@B@B@B@BBB@B@B@B@BBB@B@B@B@B@B@B@@@B@B@B@B@B@@@B@@@B@B@B@B@B@B@B@B@B@B@B@B@B@B@BBB@B@B@BM                         
'                           SB@B@B@B@BBB@B@BBB@B@B@B@B@B@B@B@B@B@B@B@B@@@B@B@BBB@B@@@B@B@B@B@B@B@B@B@BBB@B@B@BBB@B@B@B@B@B@@B                          
'                            @B@B@B@B@B@BBB@B@B@B@B@BBBBB@B@B@B@B@B@B@B@BBB@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@BBB@BBBBB@B@B@B@B@                           
'                            ,@@@B@BBB@BBB@B@BBB@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@BBB@B@BBB@B@B@B@BBB@B@B@B@BBB@B@B@B@B@v                           
'                             B@BBB@B@BBB@B@B@B@B@B@B@B@B@B@B@BBB@BBB@B@B@B@BBB@B@BBB@BBB@B@B@BBB@BBB@B@B@BBB@B@B@B@B@B@B@@                            
'                             @B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@                            
'                             B@B@B@@@B@B@B@B@@@B@@@B@B@B@BBB@B@BBB@B@B@B@BBBBB@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@B@@@B@B@B@B                            
'                            :@B@B@@@@@B@B@B@B@B@@@B@@@@@@@B@B@BBB@B@B@B@BBB@B@B@B@B@B@B@B@B@@@B@B@B@B@B@@@B@B@B@@@B@B@@@B@Y                           
'                            @B@B@@@@@B@B@B@B@B@@B@@@B@B@B@B@B@B@B@B@B@B@BBBBB@B@B@BBB@B@B@B@B@B@B@B@B@M@B@B@@@@@B@B@B@B@B@@                          
'                                                     ,r18@B@B@@@B@B@B@B@B@B@B@BBB@B@B@B@B@B@B@Mk7:                                                    
'                                                           ,uB@B@B@B@B@B@B@BBB@B@B@B@B@B@BS:                                                          
'                                                               iO@B@B@B@B@B@B@B@B@B@@@Bv                                                              
'                                                                  JB@B@B@B@B@B@B@B@Bk                                                                 
'                                                                    Y@B@B@B@B@B@B@F                                                                   
'                                                                      OB@B@B@B@@@                                                                     
'                                                                       JB@@@@@BP                                                                      
'                                                                        iB@@@Bu                                                                       
'                                                                         rB@B1                                                                        
'                                                                          P@@                                                                         
'                                                                           @                                                                          
'                                                                                                                                                      
'                                                                                                                                                      
'
'
'Batman - The Dark Knight - IPDB No. 5307
'© Stern 2008
'VPX recreation by tom tower & ninuzzu
'Thanks to Lord Hiryu for the playfield texture
'Thanks to DJRobX for coding the Joker
'Thanks to the VPDev Team for the amazing VPX!

Option Explicit
Randomize

Const UseVPMModSol = True

Const cSingleLFlip = 0
Const cSingleRFlip = 0

'******************************************************************************************
'* CUSTOMIZABLE OPTIONS 			
'******************************************************************************************

'***********	Flippers Type 	('0=normal, 1=custom)
Const FlippersType = 0

'***********	Disable Cabinet Side Reflection	('0=no, 1=yes)
Const DisableCabSides = 0

'***********	Enable Custom Lighting  ('0=no, 1=yes)
Const LightingMod = 0

'************************************************************************
'						 INIT VPM
'************************************************************************

Const BallSize = 50
Const BallMass = 1.25

' Load controller.vbs to handle VPinMAME.Controller and B2S.Server
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

' Internal DMD in Desktop Mode, using a textbox (must be called before LoadVPM)
Dim DesktopMode:DesktopMode = Table1.ShowDT
'Dim UseVPMDMD:UseVPMDMD = DesktopMode

' Standard Options
Const UseSolenoids = 1, UseLamps = 0, UseSync = 0, HandleMech = 1, SSolenoidOn = "fx_solenoid", SSolenoidOff = "", SCoin = "fx_Coin"

' Rom Name
Const cGameName = "bdk_294"

LoadVPM "03000000", "SAM.VBS", 3.50

'************************************************************************
'						 INIT TABLE
'************************************************************************

Dim bsTrough, bsTEject, bsJokerEject, bsVUK, dtJoker, plungerIM, mechJoker, mechCrane

Sub Table1_Init
	vpmInit Me
	With Controller      
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Batman - The Dark Knight (Stern 2008)"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = 0   'DesktopMode	'Hide VPM DMD in Desktop Mode
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Nudging
    vpmNudge.TiltSwitch = -7
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(sw26,sw27,sw30,sw31,sw32)

	'Trough
    Set bsTrough = New cvpmTrough
    With bsTrough
		.Size = 4
		.InitSwitches Array(21, 20, 19, 18)
		.InitExit BallRelease, 90, 10
		.InitEntrySounds "fx_drain", SoundFX(SSolenoidOn,DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
		.InitExitSounds  SoundFX(SSolenoidOn,DOFContactors), SoundFX("Ballrelease",DOFContactors)
		.Balls = 4
		.CreateEvents "bsTrough", Drain
    End With

	'Top Eject
    Set bsTEject = new cvpmSaucer
    With bsTEject
		.InitKicker Sw44, 44, -90, 8, 0
		.InitExitVariance 0, 2
        .InitSounds "fx_kicker_enter", SoundFX(SSolenoidOn,DOFContactors), SoundFX("fx_saucer_exit",DOFContactors)
        .CreateEvents "bsTEject", Sw44
    End With

	'Joker Eject
	Set bsJokerEject = New cvpmSaucer
	With bsJokerEject
		.InitKicker JokerLock, 46, -12, 65, 0
        .InitExitVariance 0, 5
		.InitSounds "fx_metal_Hit_1", SoundFX("AutoPlunger",DOFContactors), SoundFX("AutoPlunger",DOFContactors)
		.CreateEvents "bsJokerEject", JokerLock
	End With

	'Joker Drop Target
    Set dtJoker = new cvpmDropTarget
	With dtJoker
		.InitDrop sw45, 45
		.Initsnd SoundFX("fx_DropTargetDown",DOFDropTargets), SoundFX("fx_DropTargetUp",DOFDropTargets)
	End With

	'Scarecrow VUK
	Set bsVUK = New cvpmSaucer
	With bsVUK
		.InitKicker sw12, 12, 0, 35, 1.56
        .InitExitVariance 0, 5
		.InitSounds "fx_kicker_catch", SoundFX(SSolenoidOn,DOFContactors), SoundFX("kicker_out",DOFContactors)
		.CreateEvents "bsVUK", sw12
	End With

	' Auto Plunger
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP sw23, 65, 0.55
		.Switch 23
		.Random 0.05
		.InitExitSnd SoundFX("AutoPlunger",DOFContactors), SoundFX("AutoPlunger",DOFContactors)
		.CreateEvents "plungerIM"
	End With

	' Joker Motor
	Set mechJoker = new cvpmMech
	With mechJoker
		.Mtype = vpmMechLinear + vpmMechOneDirSol + vpmMechFast
		.Sol1 = 26
		.Sol2 = 30
		.Length = 1500
		.Steps = 360
		.AddSw 52, 0, 0
		.AddSw 51, 178, 182
		.AddSw 50, 359, 359
		.Callback = GetRef("UpdateJokerMech")
		.Start
     End With

	' Crane Motor
	Set mechCrane = new cvpmMech
	With mechCrane
		.Mtype = vpmMechLinear + vpmMechOneDirSol
		.Sol1 = 31
		.Sol2 = 28
		.Length = 110
		.Steps = 90
		.AddSw 56, 0, 1
		.AddSw 57, 30, 33
		.AddSw 58, 47, 50
		.AddSw 59, 59, 62
		.AddSw 60, 71, 74
		.AddSw 61, 89, 89
		.Callback = GetRef("UpdateCraneMech")
		.Start
     End With

	'Init Other Stuff
	InitLights:InitOptions
	Rails.visible = DesktopMode
    WaitHere.isdropped=1
	Dim i: For i=1 to 5:CraneHit(i).IsDropped = 1:Next
	

	' Fast Flips
	On Error Resume Next 
	InitVpmFFlipsSAM
	If Err Then MsgBox "You need the latest sam.vbs in order to run this table, available with vp10.5 rev3434"
	On Error Goto 0
End Sub

'************************************************************************
'							KEYS
'************************************************************************

Sub Table1_KeyDown(ByVal Keycode)
	If keycode = keyFront Then Controller.Switch(15) = 1		'tournament
	If KeyCode = PlungerKey Then Plunger.Pullback: PlaysoundAt "PlungerPull", Plunger
	If keycode = LeftTiltKey Then Nudge 90, 5:PlaySoundAt SoundFX("fx_nudge",0),sw24
	If keycode = RightTiltKey Then Nudge 270, 5:PlaySoundAt SoundFX("fx_nudge",0),Drain
	If keycode = CenterTiltKey Then Nudge 0, 6:PlaySoundAt SoundFX("fx_nudge",0),sw29
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal Keycode)
	If KeyCode = PlungerKey Then Plunger.Fire: StopSound "PlungerPull":PlaysoundAt "fx_Plunger", Plunger
	If keycode = keyFront Then Controller.Switch(15) = 0		'tournament
	If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub Table1_Paused:Controller.Pause = True:End Sub
Sub Table1_unPaused:Controller.Pause = False:End Sub
Sub Table1_exit()
	Controller.Pause = False:Controller.Stop
End Sub

'*****************************************
'      		General Illumination
'*****************************************
Set GiCallBack = GetRef("UpdateGi")

Sub UpdateGi(nr,enabled)
	Dim ii
	Select Case nr
	Case 0
		If enabled Then
			DOF 103, DOFOn
			If LightingMod=1 Then
				Table1.ColorGradeImage = "ColorGradeBlue_on"
				LeftFlipperP.BlendDisableLighting = 1 : RightFlipperP.BlendDisableLighting = 1
				LeftFlipperLight.state = 1 : RightFlipperLight.state = 1
			Else
				Table1.ColorGradeImage = "ColorGradeEx_8"
			End If
			For each ii in GI:ii.state=1:Next
			For each ii in BWLamps:ii.IntensityScale=1:Next
			Reflect1.IntensityScale=1
			Reflect2.IntensityScale=1
			bulbBW.BlendDisableLighting = 8
		Else
			DOF 103, DOFOff
			If LightingMod=1 Then
				Table1.ColorGradeImage = "ColorGradeBlue_off"
				LeftFlipperP.BlendDisableLighting = 0 : RightFlipperP.BlendDisableLighting = 0
				LeftFlipperLight.state = 0 : RightFlipperLight.state = 0
			Else
				Table1.ColorGradeImage = "ColorGradeEx_1"
			End If
			For each ii in GI:ii.state=0:Next
			For each ii in BWLamps:ii.IntensityScale=0:Next
			Reflect1.IntensityScale=0
			Reflect2.IntensityScale=0
			bulbBW.BlendDisableLighting = 0
		End If
	End Select
End Sub

'*****************************************
' 			Lights Mapping
'*****************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

Sub InitLights
    Dim x
    For x = 0 to 200
        LampState(x) = 0       	 ' current light state
        FadingLevel(x) = 0       ' current light fading level
        FlashSpeedUp(x) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.35 ' slower speed when turning off the flasher
        FlashMax(x) = 1          ' the maximum value when on, usually 1
        FlashMin(x) = 0          ' the minimum value when off, usually 0
        FlashLevel(x) = 0        ' the intensity of the flashers, usually from 0 to 1
    Next
	LampTimer.Interval = -1
	LampTimer.Enabled = 1
End Sub

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
        Next
    End If
    UpdateLamps
	UpdateJokerLighting
End Sub

Sub UpdateJokerLighting
	If LampState(79) Then	
		cilin.image = "JokerMotor_ON"
		Joker.image = "joker_tex_ON"
		Joker.DisableLighting = 0.05	
	Else	
		cilin.image = "JokerMotor"
		Joker.image = "joker_tex"
		Joker.DisableLighting = 0	
	End If
End Sub

Sub UpdateLamps
	'Inserts
	FadeLamp 3,  l3
	FadeLamp 4,  l4
	FadeLamp 5,  l5
	FadeLamp 6,  l6
	FadeLamp 7,  l7
	FadeLamp 8,  l8
	FadeLamp 9,  l9
	FadeLamp 10,  l10
	FadeLamp 11,  l11
	FadeLamp 12,  l12
	FadeLamp 13,  l13
	FadeLamp 14,  l14
	FadeLamp 15,  l15
	FadeLamp 16,  l16
	FadeLamp 17,  l17
	FadeLamp 18,  l18
	FadeLamp 19,  l19
	FadeLamp 20,  l20
	FadeLamp 21,  l21
	FadeLamp 22,  l22
	FadeLamp 23,  l23
	FadeLamp 24,  l24
	FadeLamp 25,  l25
	FadeLamp 26,  l26
	FadeLamp 27,  l27
	FadeLamp 28,  l28
	FadeLamp 29,  l29
	FadeLamp 31,  l31
	FadeLamp 32,  l32
	FadeLamp 33,  l33
	FadeLamp 34,  l34
	FadeLamp 35,  l35
	FadeLamp 36,  l36
	FadeLamp 37,  l37
	FadeLamp 38,  l38
	FadeLamp 39,  l39
	FadeLamp 40,  l40
	FadeLamp 41,  l41
	FadeLamp 42,  l42
	FadeLamp 43,  l43
	FadeLamp 44,  l44
	FadeLamp 45,  l45
	FadeLamp 47,  l47
	FadeLamp 48,  l48
	FadeLamp 49,  l49
	FadeLamp 50,  l50
	FadeLamp 51,  l51
	FadeLamp 52,  l52
	FadeLamp 53,  l53
	FadeLamp 54,  l54
	FadeLamp 55,  l55
	FadeLamp 56,  l56
	FadeLamp 57,  l57
	FadeLamp 58,  l58
	MiniPFLamps 59,  l59		'minipf BL
	FadePrim 59,  bulbBL, 8		'minipf BL
	FadeLamp 60,  l60			'left bumper
	FadePrim 60,  bulbl60,20	'left bumper
	FadeLamp 61,  l61			'right bumper
	FadePrim 61,  bulbl61,20	'right bumper
	MiniPFLamps 62,  l62		'minipf TL
	FadePrim 62,  bulbTL, 8		'minipf TL
	FadeLamp 63,  l63
	FadeLamp 64,  l64
	FadeLamp 65,  l65
	FadeLamp 66,  l66
	FadeLamp 67,  l67
	MiniPFLamps 68,  l68		'minipf BR
	FadePrim 68,  bulbBR, 8		'minipf BR
	FadeLamp 69,  l69
	FadeLamp 70,  l70
	FadeLamp 71,  l71
	FadeLamp 72,  l72
	FadeLamp 73,  l73
	FadeLamp 74,  l74
	MiniPFLamps 75,  l75		'minipf TR
	FadePrim 75,  bulbTR, 8		'minipf TR
	FadeLamp 76,  l76
	FadeLamp 77,  l77
	FadeLamp 78,  l78a			'batmobile
	FadeLamp 78,  l78b			'batmobile
	FadeLamp 79,  l79a			'joker
	FadeLamp 79,  l79b			'joker
	FadeLamp 80,  l80a			'scarecrow
	FadeLamp 80,  l80b			'scarecrow
	FadeLamp 86,  l86
	FadeLamp 87,  l87 
End Sub

	'Flashers
Sub	SetLamp179(m):m = m/255:F19a.state = m:F19b.state = m:End Sub
Sub	SetLamp181(m):m = m/255:F21.state = m:End Sub
Sub	SetLamp182(m):m = m/255:F22.state = m:F22a.state = m:F22b.state = m:F22c.state = m: End Sub
Sub	SetLamp183(m):m = m/255:F23.state = m:End Sub
Sub	SetLamp185(m):m = m/255:F25a.state = m:F25b.state = m:End Sub  
Sub	SetLamp187(m):m = m/255:F27.state = m:F27a.state = m:LDome.BlendDisableLighting = m*3:End Sub
Sub	SetLamp189(m):m = m/255:F29.state = m:F29a.state = m:RDome.BlendDisableLighting = m*3:End Sub
Sub	SetLamp192(m):m = m/255:F32.state = m:F32a.state = m:End Sub  


' Not Modulated lights and flashers
Sub FadeLamp(nr, object)
	If TypeName(object) = "Light" Then
		Object.State = LampState(nr)
	End If
	If TypeName(object) = "Flasher" Then
		Object.IntensityScale = LampState(nr)
	End If
End Sub

Sub FadePrim(nr, object, factor)
	Object.BlendDisableLighting = factor * LampState(nr)
End Sub

Sub SetLamp(nr, enabled)
    If enabled Then
		LampState(nr) = 1
	Else
		LampState(nr) = 0
	End If
End Sub

Sub MiniPFLamps(nr, object)
	object.IntensityScale = LampState(nr)/(LampState(59) + LampState(62) + LampState(68) + LampState(75) + 0.05)
End Sub

' Modulated lights and flashers
Sub FadeModLamp(nr, object)
	Object.IntensityScale = FadingLevel(nr)/255
End Sub 

Sub FadeModPrim(nr, object, factor)
	Object.BlendDisableLighting = factor * FadingLevel(nr)/255
End Sub

Sub SetModLamp(nr, value)
	FadingLevel(nr) = value
End Sub

'************************************************************************
'						SOLENOIDS MAP
'************************************************************************

SolCallBack(1) = "SolTrough"						'Trough-Up Kicker
SolCallBack(2) = "SolAutoPlungerIM"					'AutoLaunch
SolCallback(3) = "bsTEject.SolOut"					'Top Right Eject
SolCallBack(4) = "bsJokerEject.SolOut"				'Joker Lockup
SolCallback(5) = "dtJoker.SolDropUp"				'Joker Drop Target Up
SolCallback(6) = "SolVUK"							'Scarecrow VUK
SolCallback(7) = "dtJoker.SolDropDown"				'Joker Drop Target Down
SolCallback(8) = "SolShaker"						'Shaker Motor
'SolCallback(9) = ""							'Left Bumper
'SolCallback(10)= ""							'Right Bumper			
'SolCallback(11)= ""							'Bottom Bumper
SolCallBack(12)= "vpmSolGate LGate, SoundFX(""ElGate"",DOFContactors),"	'Left Control Gate
SolCallBack(13)= "SolBatRamp"						'Batmobile Ramp Down
SolCallBack(14)= "vpmSolGate RGate, SoundFX(""ElGate"",DOFContactors),"	'Right Control Gate
SolCallback(15)= "SolLFlipper"						'Left Flipper
SolCallback(16)= "SolRFlipper"						'Right Flipper
'SolCallback(17)= ""							'Left Sling
'SolCallback(18)= ""							'Right Sling
SolModCallBack(19)= "SetLamp179"						'Flasher:ScareCrow Home Insert
SolModCallBack(21)= "SetLamp181"						'Flasher:BackPanel
SolModCallBack(22)= "SetLamp182"						'Flasher:Joker (x3)
SolModCallBack(23)= "SetLamp183"						'Flasher:Scarecrow
'SolCallBack(24)= "vpmSolSound SoundFX(""fx_knocker"",DOFKnocker),"					'Knocker
SolModCallBack(25)= "SetLamp185"						'Flasher:Pop Bumpers  (x3)
'SolCallBack(26)= ""							'Joker Motor
SolModCallBack(27)= "SetLamp187"						'Flasher:Left SlingShot
'SolCallBack(28)= ""							'ScareCrow Motor Relay
SolModCallBack(29)= "SetLamp189"						'Flasher:Right SlingShot
'SolCallBack(30)= ""							'Joker Motor Relay
'SolCallBack(31)= ""							'ScareCrow Motor
SolModCallback(32)= "SetLamp192"						'Flasher:BatMobile Crash (x2)

'************************************************************************
'						FLIPPERS
'************************************************************************

Sub SolLFlipper(Enabled)
	If Enabled Then		 
		PlaySoundAt SoundFX("fx_FlipperUp",DOFFlippers), LeftFlipper
		LeftFlipper.RotateToEnd
	Else
        PlaySoundAt SoundFX("fx_FlipperDown",DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
	End If
 End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_FlipperUp",DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_FlipperDown",DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
	End If
 End Sub

'************************************************************************
'						BALL TROUGH
'************************************************************************

Sub SolTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		If BsTrough.Balls Then vpmTimer.PulseSw 22
	End If
End Sub

'************************************************************************
'						 AUTOPLUNGER 
'************************************************************************

Sub SolAutoPlungerIM(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
End Sub

'************************************************************************
'						 SCARECROW VUK
'************************************************************************

Sub SolVUK(enabled)
	If Enabled Then
		bsVUK.ExitSol_On
		ScareLockFront.Enabled=0
		vpmtimer.addtimer 400, "ScareLockFront.Enabled=1'"
	End If
End Sub

'************************************************************************
'						 SHAKER MOTOR
'************************************************************************
Dim bou,brake,perc
Dim bou2,brake2,perc2

Sub SolShaker(enabled)
	If Enabled Then
		ShakeJoker : ShakeBall
		PlaySoundAt SoundFX("ShakerPulse",DOFShaker), Primitive12
	End If
End Sub

Sub ShakeJoker
	perc=3:ShakeJokerTimer.enabled=1
End Sub

Sub ShakeBall
	perc2=3:ShakeTimer.enabled=1
End Sub

Sub ShakeJokerTimer_timer()
	bou=bou+0.4:brake=brake+0.02 
	Joker.rotx=sin(bou)*(perc-(brake*(perc/6)))
	If (perc-(brake*(perc/6)))<0 Then Me.Enabled = 0 :bou=0 :brake=0 :perc=0
End Sub

Sub ShakeTimer_timer()
	bou2=bou2+0.4:brake2=brake2+0.02 
	CraneS.transX=sin(bou2)*(perc2-(brake2*(perc2/6)))
	If (perc2-(brake2*(perc2/6)))<0 Then Me.Enabled = 0 :bou2=0 :brake2=0 :perc2=0
End Sub

'************************************************************************
'						BATMOBILE RAMP
'************************************************************************

Const Pi=3.1415926535

Dim BatRampRadius:BatRampRadius = SQR((LowerTrack.X - Sw64.X)^2 + (LowerTrack.Y - Sw64.Y)^2)

Dim myBall, fakeBall, RampDir, RampPos

Sub sw64_Hit: If ActiveBall.VelY<0 Then Controller.Switch(64) = 1 :End If: End Sub

Sub Trigger1_hit : Set myBall= ActiveBall:PlaysoundAt "fx_rrturn", Trigger1:If ActiveBall.VelX<0 And ActiveBall.VelX>-5 Then ActiveBall.VelX = -10 :End If : End Sub

Sub SolBatRamp(enabled)
	If Enabled Then
		RampPos=10:RampDir=-1: MoveBatRamp.Enabled=1 
		PlaysoundAt SoundFX("DiverterOn", DOFContactors), BatMobile
	Else
		If NOT IsEmpty(myBall) Then myBall= Empty
		RampPos=0:RampDir=1: MoveBatRamp.Enabled=1
		PlaysoundAt SoundFX("DiverterOff", DOFContactors), BatMobile        
	End If
End Sub

Sub MoveBatRamp_timer
	RampPos = RampPos + RampDir
	If RampDir = -1 And RampPos<0 Then RampPos=0  : Me.Enabled=0 : DropTheBall 1
	If RampDir = 1 And RampPos>10 Then RampPos=10 : Me.Enabled=0 : DropTheBall 0
	UpperTrack.RotX = RampPos
	LowerTrack.RotX = RampPos
	DecalDropRamp.RotX = RampPos
	DecalSide.RotX = RampPos
	BatMobile.RotX = RampPos
	If NOT IsEmpty(myBall) Then
		myBall.Y = LowerTrack.Y - BatRampRadius * cos (RampPos * Pi/180)
		myBall.Z = 135 + BatRampRadius * sin (RampPos * Pi/180) + 25
	End If
End Sub

Sub DropTheBall(n)
	Select Case n
		Case 0
			RampUp.collidable=1
			RampDown.collidable=0
			Set fakeBall = BallDestroy.createball : fakeBall.visible = 0 : BallDestroy.kick 0,20
			MoveBatMobile.enabled= 1
		Case 1
			If NOT IsEmpty(myBall) Then
				myBall.VelZ=5
				Controller.Switch(64) = 0
			End If
			RampUp.collidable=0
			RampDown.collidable=1
			Set fakeBall = BallMake.createball : fakeBall.visible = 0 : BallMake.kick 180,1
			MoveBatMobile.enabled= 1
	End Select
End Sub

Sub MoveBatMobile_timer
	If Not(IsEmpty(fakeBall)) Then
BatMobile. TransY = (-BallMake.Y + fakeBall.Y)-145
Else
BatMobile. TransY = -BallMake.Y
End If
End Sub

Sub BallMake_hit : MoveBatMobile.Enabled=0 :  Me.DestroyBall : fakeBall = Empty : End Sub
Sub BallDestroy_Hit : MoveBatMobile.Enabled=0 :  Me.DestroyBall : fakeBall = Empty : vpmtimer.pulsesw 47: End Sub

'************************************************************************
' 						JOKER MOTOR
'************************************************************************

Dim InPos, OutPos, CurMechPos: InPos = 0 : OutPos = 0
Dim JokerRadius:JokerRadius = SQR((cilin.X - joker.X)^2 + (cilin.Y - joker.Y)^2)

Sub UpdateJoker_timer
	if InPos < CurMechPos then
		InPos = InPos + 1
		if InPos > CurMechPos then InPos = CurMechPos
	elseif InPos > CurMechPos Then
		InPos = InPos - 1
		if InPos < CurMechPos then InPos = CurMechPos		
	else	
		me.Enabled=false
	end if 
	if InPos <=0 then outpos = InPos
	UpdateObjects
End Sub

Sub UpdateJokerMech(aCurrPos,aSpeed,aLastPos)
	CurMechPos = aCurrPos -180
	UpdateJoker.Enabled = 1
End Sub

Sub UpdateObjects
	cilin.RotZ = InPos
	cilout.RotZ = OutPos
	Joker.X = cilin.X - JokerRadius * sin (InPos * Pi/180)
	Joker.Y = cilin.Y + JokerRadius * cos (InPos * Pi/180)
	Joker.RotZ = InPos
End Sub

'************************************************************************
' 						CRANE MOTOR
'************************************************************************

Dim CranePos, MotorSnd, CurMech1Pos : MotorSnd = 0 : CranePos = 160
Dim CraneRadius:CraneRadius = SQR((crane.X - joker.X)^2 + (crane.Y - joker.Y)^2)

Sub UpdateCrane_timer
	if CranePos < CurMech1Pos then
		CranePos = CranePos + 0.5
		if CranePos > CurMech1Pos then CranePos = CurMech1Pos
	elseif CranePos > CurMech1Pos Then
		CranePos = CranePos - 0.5
		if CranePos < CurMech1Pos then CranePos = CurMech1Pos		
	else	
		me.Enabled=false : ShakeBall : StopSound "IdolMotor"
	end if
	UpdateParts
End Sub

Sub UpdateCraneMech(aCurrPos,aSpeed,aLastPos)
	CurMech1Pos = 250 - aCurrPos
	UpdateCrane.Enabled = 1
	If aSpeed=0 Then
		StopSound "Motor":MotorSnd=0
	Else
		If MotorSnd=0 Then PlayLoopSoundAtVol SoundFX("Motor", DOFGear), crane, 1: MotorSnd=1
	End If
End Sub

Sub UpdateParts
	If CranePos > (250 -2) Then
		CraneHit(5).IsDropped = 0
	ElseIf CranePos<(250 -28) AND CranePos > (250 -35) Then
		CraneHit(4).IsDropped = 0
	ElseIf CranePos<(250 -45) AND CranePos > (250 -52) Then
		CraneHit(3).IsDropped = 0
	ElseIf CranePos<(250 -57) AND CranePos > (250 -64) Then
		CraneHit(2).IsDropped = 0
	ElseIf CranePos<(250 -70) AND CranePos > (250 -77) Then
		CraneHit(1).IsDropped = 0
	ElseIf CranePos<(250 -86) Then
		CraneHit(0).IsDropped = 0
	Else
		Dim i: For each i in CraneHit:i.IsDropped=1 : Next
	End If
    crane.rotz=CranePos
    craneB.rotz=CranePos
    craneM.rotz=CranePos
    craneS.rotz=CranePos
End Sub

Sub CraneHit_Hit(idx)
	vpmtimer.Pulsesw 85
	PlaySound("fx_collide"), 0, Csng(BallVel(ActiveBall)) ^2 / 500, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	ShakeBall
End Sub

'************************************************************************
'      					SWITCHES
'************************************************************************

'StandUp Targets
Sub sw1_hit:vpmtimer.PulseSw 1: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw2_hit:vpmtimer.PulseSw 2: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw3_hit:vpmtimer.PulseSw 3: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw4_hit:vpmtimer.PulseSw 4: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw5_hit:vpmtimer.PulseSw 5: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw6_hit:vpmtimer.PulseSw 6: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw11_hit:vpmtimer.PulseSw 11: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw14_hit:vpmtimer.PulseSw 14: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw36_hit:vpmtimer.PulseSw 36: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw37_hit:vpmtimer.PulseSw 37: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw38_hit:vpmtimer.PulseSw 38: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw48_hit:vpmtimer.PulseSw 48: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw49_hit:vpmtimer.PulseSw 49: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub

'Center Ramp Enter
Sub sw13_hit:vpmtimer.pulsesw 13: PlaySoundAt "fx_gate4",ActiveBall:End Sub

'Rollovers
Sub sw7_hit:Controller.switch(7) = 1: PlaySoundAt "fx_sensor",ActiveBall:End Sub
Sub sw7_unhit:Controller.switch(7) = 0: End Sub

Sub sw8_hit:Controller.switch(8) = 1: PlaySoundAt "fx_sensor",ActiveBall:End Sub
Sub sw8_unhit:Controller.switch(8) = 0: End Sub

Sub sw9_hit:Controller.switch(9) = 1: PlaySoundAt "fx_sensor",ActiveBall:End Sub
Sub sw9_unhit:Controller.switch(9) = 0: End Sub

Sub sw24_hit:Controller.switch(24) = 1: PlaySoundAt "fx_sensor",ActiveBall:End Sub
Sub sw24_unhit:Controller.switch(24) = 0: End Sub

Sub sw25_hit:Controller.switch(25) = 1: PlaySoundAt "fx_sensor",ActiveBall:End Sub
Sub sw25_unhit:Controller.switch(25) = 0: End Sub

Sub sw28_hit:Controller.switch(28) = 1: PlaySoundAt "fx_sensor",ActiveBall:End Sub
Sub sw28_unhit:Controller.switch(28) = 0: End Sub

Sub sw29_hit:Controller.switch(29) = 1: PlaySoundAt "fx_sensor",ActiveBall:End Sub
Sub sw29_unhit:Controller.switch(29) = 0: End Sub

Sub sw33_hit:Controller.switch(33) = 1: PlaySoundAt "fx_sensor",ActiveBall:End Sub
Sub sw33_unhit:Controller.switch(33) = 0: End Sub

'Bumpers
Sub sw30_hit:vpmtimer.pulsesw 30:PlaySoundAt SoundFX("LeftJet",DOFContactors), ActiveBall:End Sub
Sub sw31_hit:vpmtimer.pulsesw 31:PlaySoundAt SoundFX("RightJet",DOFContactors), ActiveBall:End Sub
Sub sw32_hit:vpmtimer.pulsesw 32:PlaySoundAt SoundFX("BottomJet",DOFContactors), ActiveBall:End Sub

'Spinners
Sub sw34_spin:vpmtimer.pulsesw 34: PlaySoundAt "fx_spinner", sw34:End Sub
Sub sw39_spin:vpmtimer.pulsesw 39: PlaySoundAt "fx_spinner", sw39:End Sub

'Slingshots
Dim LStep, RStep

Sub sw26_slingshot
	vpmTimer.PulseSw 26
	PlaySoundAt SoundFX("LeftSlingShot",DOFContactors), ActiveBall
	LSling.Visible = 0: LSling2.Visible = 1: sling1.TransZ = -20: LStep = 0
	Me.TimerInterval = 50: Me.TimerEnabled = 1
End Sub

Sub sw27_slingshot
	vpmTimer.PulseSw 27
	PlaySoundAt SoundFX("RightSlingshot",DOFContactors), ActiveBall
	RSling.Visible = 0: RSling2.Visible = 1: sling2.TransZ = -20: RStep = 0
	Me.TimerInterval = 50: Me.TimerEnabled = 1
End Sub

Sub sw26_Timer
sw26.Timerinterval = 20
	Select Case LStep
		Case 3:LSLing2.Visible = 0:LSLing1.Visible = 1:sling1.TransZ = -10
		Case 4:LSLing1.Visible = 0:LSLing.Visible = 1:sling1.TransZ = 0:Me.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub sw27_Timer
sw27.Timerinterval = 20
	Select Case RStep
		Case 3:RSLing2.Visible = 0:RSLing1.Visible = 1:sling2.TransZ = -10
		Case 4:RSLing1.Visible = 0:RSLing.Visible = 1:sling2.TransZ = 0:Me.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

'Left Loop
Sub sw35_hit
vpmtimer.PulseSw 35: PlaySoundAt "fx_gate4",ActiveBall
If ActiveBall.VelY>20 Then ActiveBall.VelY = 18
End Sub

'Right Loop
Sub sw41_hit
vpmtimer.PulseSw 41: PlaySoundAt "fx_gate4",ActiveBall
If ActiveBall.VelY>20 Then ActiveBall.VelY = 18
End Sub

'VUK Exit
Sub sw42_hit:vpmtimer.pulsesw 42: PlaySoundAt "fx_gate4",ActiveBall:End Sub

'Drop Target
Sub sw45_dropped:dtJoker.Hit 1:End Sub

'Mini Pf Rollovers
Sub sw53_hit:Controller.switch(53) = 1: PlaySoundAt "fx_sensor",ActiveBall: sw53p.RotX = -15 : End Sub
Sub sw53_unhit:Controller.switch(53) = 0: sw53p.RotX = 0 : End Sub

Sub sw54_hit:Controller.switch(54) = 1: PlaySoundAt "fx_sensor",ActiveBall: sw54p.RotX = -15 : End Sub
Sub sw54_unhit:Controller.switch(54) = 0: sw54p.RotX = 0 : End Sub

Sub sw55_hit:Controller.switch(55) = 1: PlaySoundAt "fx_sensor",ActiveBall: sw55p.RotX = -15 : End Sub
Sub sw55_unhit:Controller.switch(55) = 0: sw55p.RotX = 0 : End Sub

Sub sw63_hit:Controller.switch(63) = 1: PlaySoundAt "fx_sensor",ActiveBall: sw63p.RotX = -15 : End Sub
Sub sw63_unhit:Controller.switch(63) = 0: sw63p.RotX = 0 : End Sub

' *********************************************************************
'					Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
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

Function AudioFade(ball)
	Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function RndNum(min,max)
 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min and max
End Function

'Set position as table object (Use object or light but NOT wall) and Volume to 1
Sub PlaySoundAt(sound, tableobj)
	PlaySound sound, 1, 1, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Volume Multiplier may be used eg; PlaySoundAtBallVol "sound",3
Sub PlaySoundAtBallVol(sound, VolMult)
	PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as table object and Volume manually.
Sub PlaySoundAtVol(sound, tableobj, Volume)
	PlaySound sound, 1, Volume, Pan(tableobj), 0, 0, 0, 1, AudioFade(tableobj)
End Sub

Sub PlayLoopSoundAtVol(sound, tableobj, Volume)
	PlaySound sound, -1, Volume, Pan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

' *********************************************************************
' 						Other Sound FX
' *********************************************************************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 500, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub LeftFlipper_Collide(parm) : RandomSoundFlipper : End Sub
Sub Rightflipper_Collide(parm) : RandomSoundFlipper : End Sub
Sub Posts_Hit(idx) : RandomSoundRubber : End Sub
Sub Rubbers_Hit(idx): RandomSoundRubber: End Sub
Sub RubberPost_Hit(idx): RandomSoundRubber: End Sub
Sub Gates_Hit(idx): PlaySoundAtBallVol "fx_gate4",1: End Sub

Sub RandomSoundRubber()
	Select Case RndNum(1,3)
		Case 1 : PlaySoundAtBallVol "fx_rubber_hit_1",20
		Case 2 : PlaySoundAtBallVol "fx_rubber_hit_2",20
		Case 3 : PlaySoundAtBallVol "fx_rubber_hit_3",20
	End Select
End Sub

Sub RandomSoundFlipper()
	Select Case RndNum(1,3)
		Case 1 : PlaySoundAtBallVol "fx_flip_hit_1", 20
		Case 2 : PlaySoundAtBallVol "fx_flip_hit_2", 20
		Case 3 : PlaySoundAtBallVol "fx_flip_hit_3", 20
	End Select
End Sub

Sub ScareLockFront_Hit : PlaysoundAt "RightHole", Activeball : End Sub
Sub ScareLock_Hit : PlaysoundAt "RightHole", Activeball : End Sub

Sub ShooterEnd_Hit:If ActiveBall.Z > 30  Then Me.TimerInterval=100:Me.TimerEnabled=1:End If:End Sub			'ball is flying
Sub ShooterEnd_Timer(): Me.TimerEnabled=0 : PlaySoundAt "fx_BallDrop", ShooterEnd : End Sub

Sub LWREnter_Hit: PlaySoundAt "fx_wireramp_enter",LWREnter:End Sub
Sub LWRExit_hit:StopSound "fx_wireramp_enter":PlaysoundAt "fx_wireramp_exit", ActiveBall:Me.TimerInterval=200:Me.TimerEnabled=1:ActiveBall.VelY=1:WaitHere.isdropped=1:End Sub
Sub LWRExit_timer:Me.TimerEnabled=0:PlaysoundAt "fx_BallDrop",LWRExit:End Sub

Sub RWREnter_Hit(): PlaySoundAt "fx_wireramp_enter",RWREnter:	End Sub
Sub RWREnter1_Hit():PlaySoundAt "WireRampBump1",	RWREnter1:	End Sub
Sub RWREnter2_Hit():PlaySoundAt "WireRampBump2",	RWREnter2:	End Sub
Sub RWRExit_hit:StopSound"fx_wireramp_enter":PlaysoundAt "fx_wireramp_exit", ActiveBall:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub RWRExit_timer:Me.TimerEnabled=0:PlaysoundAt "fx_BallDrop",RWRExit:End Sub

Sub RREnter_Hit():If ActiveBall.VelY < 0 Then PlaySoundAt "fx_rrenter", ActiveBall:End If:End Sub			'ball is going up
Sub RREnter_UnHit():If ActiveBall.VelY > 0 Then StopSound "fx_rrenter":End If:End Sub						'ball is going down

Sub BREnter_Hit():PlaySoundAt "fx_brenter",	BREnter:End Sub
Sub BRREnter1_Hit():PlaySoundAt "fx_brbump", BREnter1:End Sub
Sub WHtr_Hit:WaitHere.isdropped=0:End Sub

' *********************************************************************
' 						Real Time Updates
' *********************************************************************

Sub GameTimer_timer
	RollingSoundUpdate
	BallShadowUpdate
	LeftFlipperP.ObjRotZ = LeftFlipper.currentangle
	RightFlipperP.ObjRotZ = RightFlipper.currentangle
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	sw13p.rotX = -sw13.currentangle
	sw34p.rotX = -sw34.currentangle
	sw35p.rotX = sw35.currentangle
	sw39p.rotX = -sw39.currentangle
	sw41p.rotY = sw41.currentangle
	sw42p.rotX = sw42.currentangle
	Gate1P.rotY = -Gate1.currentangle
	Gate2P.rotY = -Gate2.currentangle
	Gate3P.rotY = Gate3.currentangle
	LGateP.rotY = Lgate.currentangle
	RGateP.rotY = Rgate.currentangle *2/3
End Sub

Const tnob = 5						' total number of balls : 4 (trough) + 1(FakeBall)
Const fakeballs = 0					' number of balls created on table start (rolling sound and ballshadow will be skipped)
ReDim rolling(tnob-fakeballs-1)
ReDim BallShadow(tnob-fakeballs-1)
InitRolling:InitBallShadow

Sub InitRolling
	Dim i
	For i=0 to tnob-fakeballs-1
		rolling(i) = False
	Next
End Sub

Sub InitBallShadow
	Dim i
	For i=0 to tnob-fakeballs-1
		ExecuteGlobal "Set BallShadow(" & i & ") = BallShadow" & (i+1) & ":"
	Next
End Sub

' ************************ROLLING SOUND************************
Sub RollingSoundUpdate()
    Dim BOT, b
    BOT = GetBalls
	' stop the sound of deleted balls
	If UBound(BOT)<(tnob - 1) Then
		For b = (UBound(BOT) + 1) to (tnob-1)
			rolling(b-fakeballs) = False
			StopSound("fx_ballrolling" & b-fakeballs)
		Next
	End If
	' exit the Sub if no balls on the table
    If UBound(BOT) = fakeballs-1 Then Exit Sub
       ' play the rolling sound for each ball
    For b = fakeballs to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
			rolling(b-fakeballs) = True
			PlaySound("fx_ballrolling" & b-fakeballs), -1, Vol(BOT(b) )*2, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
		Else
            If rolling(b-fakeballs) = True Then
                StopSound("fx_ballrolling" & b-fakeballs)
                rolling(b-fakeballs) = False
            End If
        End If
    Next
End Sub

' **********************BALL SHADOW**********************
Sub BallShadowUpdate()
    Dim BOT, b
    BOT = GetBalls
	' hide shadow of deleted balls
	If UBound(BOT)<(tnob-1) Then
		For b = (UBound(BOT) + 1) to (tnob-1)
		BallShadow(b-fakeballs).visible = 0
		Next
	End If
	' exit the Sub if no balls on the table
    If UBound(BOT) = fakeballs-1 Then Exit Sub

	' render the shadow for each ball
    For b = fakeballs to UBound(BOT)		
			BallShadow(b-fakeballs).X = BOT(b).X		
	    BallShadow(b-fakeballs).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
		If BOT(b).Z > 30 Then
			BallShadow(b-fakeballs).height = BOT(b).Z - 20
            BallShadow(b-fakeballs).opacity = 90
		Else
			BallShadow(b-fakeballs).height = BOT(b).Z - 24
            BallShadow(b-fakeballs).opacity = 80
		End If
	Next
End Sub

' *********************************************************************
' 						Table Options
' *********************************************************************

Sub InitOptions
'flippers
	Select Case FlippersType
		Case 0
		LeftFlipperP.image = "flippers" : RightFlipperP.image = "flippers"
		Case 1
		LeftFlipperP.image = "BDK flippers" : RightFlipperP.image = "BDK flippers"
	End Select
'side reflections
	Select Case DisableCabSides
		Case 0
		Reflect1.visible = 1 : Reflect2.visible = 1
		Case 1
		Reflect1.visible = 0 : Reflect2.visible = 0
	End Select
'custom lighting
	Select Case LightingMod
		Case 0
		Table1.ColorGradeImage = "ColorGradeEx_8"
		LDome.image = "Dome blue" : RDome.image = "Dome blue"
		LeftFlipperP.BlendDisableLighting = 0 : RightFlipperP.BlendDisableLighting = 0
		LeftFlipperLight.state = 0 : RightFlipperLight.state = 0
		Reflect1.imageA = "CabL" : Reflect1.imageB = "CabL"
		Reflect2.imageA = "CabR" : Reflect2.imageB = "CabR"
		Case 1
		Dim ii: For each ii in GI : ii.color = RGB(55,0,255): ii.colorfull = RGB(120,220,255) : Next
		Table1.ColorGradeImage = "ColorGradeBlue_on"
		LDome.image = "Dome purple" : RDome.image = "Dome purple"
		LeftFlipperP.BlendDisableLighting = 1 : RightFlipperP.BlendDisableLighting = 1
		LeftFlipperLight.state = 1 : RightFlipperLight.state = 1
		Reflect1.imageA = "CabLmod" : Reflect1.imageB = "CabLmod"
		Reflect2.imageA = "CabRmod" : Reflect2.imageB = "CabRmod"
		For each ii in BWLamps : ii.color = RGB(55,0,255) : Next
		L59.color = RGB(155,0,255):L62.color = RGB(155,0,255):L68.color = RGB(155,0,255):L75.color = RGB(155,0,255)
		F27.color = RGB(155,0,255): F27.colorfull = RGB(120,220,255)
		F29.color = RGB(155,0,255): F29.colorfull = RGB(120,220,255)
		F22b.color = RGB(55,255,55): F22b.colorfull = RGB(180,255,180)
		F22c.color = RGB(55,255,55): F22c.colorfull = RGB(180,255,180)
		F23.color = RGB(255,55,55): F23.colorfull = RGB(255,180,180)
		F32.color = RGB(255,55,55): F32.colorfull = RGB(255,180,180)
	End Select
End Sub
