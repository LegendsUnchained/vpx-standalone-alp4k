'============================================================================
'     	                          Jurassic  Park 
'                                Data East 1993
'=============================================================================
'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$**""""`` ````"""#*R$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$$$$$$$$$$*""      ..........      `"#$$$$$$$$$$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$$$$$$$#"    .ue@$$$********$$$$Weu.   `"*$$$$$$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$$$$#"   ue$$*#""              `""*$$No.   "R$$$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$P"   u@$*"`                         "#$$o.  ^*$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$P"  .o$R"               . .WN.           "#$Nu  `#$$$$$$$$$$$$$$
'$$$$$$$$$$$$$"  .@$#`       'ou  .oeW$$$$$$$$W            "$$u  "$$$$$$$$$$$$$
'$$$$$$$$$$$#   o$#`      ueL  $$$$$$$$$$$$$$$$ku.           "$$u  "$$$$$$$$$$$
'$$$$$$$$$$"  x$P`        `"$$u$$$$$$$$$$$$$$"#$$$L            "$o   *$$$$$$$$$
'$$$$$$$$$"  d$"        #$u.2$$$$$$$$$$$$$$$$  #$$$Nu            $$.  #$$$$$$$$
'$$$$$$$$"  @$"          $$$$$$$$$$$$$$$$$$$$k  $$#*$$u           #$L  #$$$$$$$
'$$$$$$$"  d$         #Nu@$$$$$$$$$$$$$$$$$$"  x$$L #$$$o.         #$c  #$$$$$$
'$$$$$$F  d$          .$$$$$$$$$$$$$$$$$$$$N  d$$$$  "$$$$$u        #$L  #$$$$$
'$$$$$$  :$F        ..`$$$$$$$$$$$$$$$$$$$$$$$$$$$`    R$$$$$eu.     $$   $$$$$
'$$$$$!  $$        . R$$$$$$$$$$$$$$$$$$$$$$$$$$$$$.   @$$$$$$$$Nu   '$N  `$$$$
'$$$$$  x$"        Re$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$uu@"``"$$$$$$$i   #$:  $$$$
'$$$$E  $$       c 8$$$$$$$$$$$$$$$$$$$$$G(   ``^*$$$$$$$WW$$$$$$$$N   $$  4$$$
'$$$$~ :$$N. tL i)$$$$$$$$$$$$$$$$$$$$$$$$$N       ^#R$$$$$$$$$$$$$$$  9$  '$$$
'$$$$  t$$$$u$$W$$$$$$$$$$$$$$!$$$$$$$$$$$$$&       . c?"*$$$R$$$$$$$  '$k  $$$
'$$$$  @$$$$$$$$$$$$$$$$$$$$"E F!$$$$$$$$$$."        +."@\* x .""*$$"   $B  $$$
'$$$$  $$$$$$$$$$$$$$$$"$)#F     $$$$$$$$$$$           `  -d>x"*=."`    $$  $$$
'$$$$  $$$$$$$$$$?$$R'$ `#d$""    #$$$$$$$$$ > .                "       $$  $$$
'$$$$  $$$$$$$($$@$"` P *@$.@#"!    "*$$$$$$$L!.                        $$  $$$
'$$$$  9$$$$$$$L#$L  ! " <$$`          "*$$$$$NL:"z  f                  $E  $$$
'$$$$> ?$$$$ $$$b$^      .$c .ueu.        `"$$$$b"x"#  "               x$!  $$$
'$$$$k  $$$$N$ "$$L:$oud$$$` d$ .u.         "$$$$$o." #f.              $$   $$$
'$$$$$  R$""$$o.$"$$$$""" ue$$$P"`"c          "$$$$$$Wo'              :$F  t$$$
'$$$$$: '$&  $*$$u$$$$u.ud$R" `    ^            "#*****               @$   $$$$
'$$$$$N  #$: E 3$$$$$$$$$"                                           d$"  x$$$$
'$$$$$$k  $$   F *$$$$*"                                            :$P   $$$$$
'$$$$$$$  '$b                                                      .$P   $$$$$$
'$$$$$$$b  `$b                                                    .$$   @$$$$$$
'$$$$$$$$N  "$N                                                  .$P   @$$$$$$$
'$$$$$$$$$N  '*$c                                               u$#  .$$$$$$$$$
'$$$$$$$$$$$.  "$N.                                           .@$"  x$$$$$$$$$$
'$$$$$$$$$$$$o   #$N.                                       .@$#  .@$$$$$$$$$$$
'$$$$$$$$$$$$$$u  `#$Nu                                   u@$#   u$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$u   "R$o.                             ue$R"   u$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$$$o.  ^#$$bu.                     .uW$P"`  .u$$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$$$$$$u   `"#R$$Wou..... ....uueW$$*#"   .u@$$$$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$$$$$$$$Nu.    `""#***$$$$$***"""`    .o$$$$$$$$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$$$$$$$$$$$$$eu..               ...ed$$$$$$$$$$$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$NWWeeeeedW@$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

Option Explicit
Randomize

Dim VolumeDial, CollectionVolume, enableBallControl, BallReflection
Dim ContrastSetting, GlowAmountDay, InsertBrightnessDay 
Dim GlowAmountNight, InsertBrightnessNight
Dim GIShadows, FlasherShadows 
Dim GlowBall, ChooseBall, CustomBulbIntensity(10), red(10), green(10), Blue(10)
Dim CustomBallImage(10), CustomBallLogoMode(10), CustomBallDecal(10), CustomBallGlow(10)
Dim ReflectLights
Dim SidewallReflect

'********************
'Options
'********************

VolumeDial = 0.5				'Added Sound Volume Dial (ramps, balldrop, kickers, etc)
CollectionVolume = 20 			'Standard Sound Amplifier (targets, gates, rubbers, metals, etc) use 1 for standard setup

' *** Contrast level, possible values are 0 - 7,                                         **
' *** 0: bright, good for desktop view, daytime settings in insert lighting below *********
' *** 1: same as 0, but with nighttime settings in insert lighting below ******************
' *** 2: darker, better contrast, daytime settings in insert lighting below ***************
' *** 3: same as 2, but with nighttime settings in insert lighting below ******************
' *** etc for 4-7; default is 3 ***********************************************************
ContrastSetting = 0

' *** Insert Lighting settings ************************************************************
' *** The settings below together with ContrastSetting determines how the lighting looks **
' *** for all values: 1.0 = default, useful range 0.1 - 5 *********************************
GlowAmountDay = 0.03
InsertBrightnessDay = 0.8 
GlowAmountNight = 0.5
InsertBrightnessNight = 0.6

enableBallControl = 0

' *** Ball settings ***********************************************************************
' *** 0 = normal ball
' *** 1 = white GlowBall
' *** 2 = magma GlowBall
' *** 3 = blue GlowBall
' *** 4 = Jurassic Park Logo
' *** 5 = earth
' *** 6 = green glowball (matching green GlowBat)
' *** 7 = blue glowball (matching blue GlowBat)
' *** 8 = orange glowball (matching orange GlowBat)
' *** 9 = HDR Ball

ChooseBall = 0

'Sling Plastic Mod ***********************************************************************
SlingPlastics = 0 			' 0=Blue 1=Red   *chooses default color of slings* Press 'i' to invert in game.

'Shadow  Mods ****************************************************************************
GIShadows = 0 			' 0=Off 1=On
FlasherShadows = 0		' 0=Off 1=On

'Light Reflections ****************************************************************************
ReflectLights = 0 			' 0=Off 1=On   *Light reflections on wire ramp*

'SideWallReflect ****************************************************************************
SideWallReflect= 0 			' 0=Off 1=On

'End Options ****************************************************************************
'****************************************************************************************

If SideWallReflect = 0 Then
Primitive_SideWallReflect1.visible = 0
Primitive_SideWallReflect.visible = 0
Else
Primitive_SideWallReflect1.visible = 1
Primitive_SideWallReflect.visible = 1
End If

'End Options ****************************************************************************
'****************************************************************************************

If ReflectLights = 0 Then
	L55ab3.visible = false
	L55ab2.visible = false
	L55ab5.visible = false
	L55ab6.visible = false
	L55ab8.visible = false
	L55ab7.visible = false
	L55ab4.visible = false
Else
	L55ab3.visible = true
	L55ab2.visible = true
	L55ab5.visible = true
	L55ab6.visible = true
	L55ab8.visible = true
	L55ab7.visible = true
	L55ab4.visible = true
End If
If GIShadows = 0 Then
	Flasher_PFShadow.visible = false
	Flasher_PFShadow1.visible = false
	Flasher_Plasticshadow.visible = false
	Flasher_Plasticshadow1.visible = false
End If
If FlasherShadows = 0 Then
	Shadow_LN_Flasher_On.visible = false	
	Shadow_RW_PF_Flasher.visible = false
	Shadow_LN_Flasher_Off.visible = false
	Shadow_RW_PL_Flasher.visible = false
	Shadow_LW_PF_Flasher.visible = false
	Shadow_LW_PL_Flasher.visible = false
	Shadow_RR_PF_Flasher.visible = false
	Shadow_RR_PL_Flasher.visible = false
	Shadow_LR_PF_Flasher.visible = false
	Shadow_LR_PL_Flasher.visible = false
End If

' default Ball
CustomBallGlow(0) = 		False
CustomBallImage(0) = 		"shiney_ball_HDR"
CustomBallLogoMode(0) = 	False
CustomBallDecal(0) = 		"scratches"
CustomBulbIntensity(0) = 	2
Red(0) = 0 : Green(0)	= 0 : Blue(0) = 0

' white GlowBall
CustomBallGlow(1) = 		True
CustomBallImage(1) = 		"white"
CustomBallLogoMode(1) = 	True
CustomBallDecal(1) = 		""
CustomBulbIntensity(1) = 	0
Red(1) = 255 : Green(1)	= 255 : Blue(1) = 255

' Magma GlowBall
CustomBallGlow(2) = 		True
CustomBallImage(2) = 		"ballblack"
CustomBallLogoMode(2) = 	True
CustomBallDecal(2) = 		"ballmagma"
CustomBulbIntensity(2) = 	0
Red(2) = 255 : Green(2)	= 180 : Blue(2) = 100

' Blue ball
CustomBallGlow(3) = 		True
CustomBallImage(3) = 		"blueball2"
CustomBallLogoMode(3) = 	False
CustomBallDecal(3) = 		""
CustomBulbIntensity(3) = 	0
Red(3) = 30 : Green(3)	= 40 : Blue(3) = 200

' Jurassic Park Logo
CustomBallGlow(4) = 		False
CustomBallImage(4) = 		"ball_HDR"
CustomBallLogoMode(4) = 	True
CustomBallDecal(4) = 		"JPLogo1"
CustomBulbIntensity(4) = 	0.01
Red(4) = 0 : Green(4)	= 0 : Blue(4) = 0

' Earth
CustomBallGlow(5) = 		True
CustomBallImage(5) = 		"ballblack"
CustomBallLogoMode(5) = 	True
CustomBallDecal(5) = 		"earth"
CustomBulbIntensity(5) = 	0
Red(5) = 100 : Green(5)	= 100 : Blue(5) = 100

' green GlowBall
CustomBallGlow(6) = 		True
CustomBallImage(6) = 		"glowball green"
CustomBallLogoMode(6) = 	True
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
Red(6) = 100 : Green(6)	= 255 : Blue(6) = 100

' blue GlowBall
CustomBallGlow(7) = 		True
CustomBallImage(7) = 		"glowball blue"
CustomBallLogoMode(7) = 	True
CustomBallDecal(7) = 		""
CustomBulbIntensity(7) = 	0
Red(7) = 100 : Green(7)	= 100 : Blue(7) = 255

' orange GlowBall
CustomBallGlow(8) = 		True
CustomBallImage(8) = 		"glowball orange"
CustomBallLogoMode(8) = 	True
CustomBallDecal(8) = 		""
CustomBulbIntensity(8) = 	0
Red(8) = 255 : Green(8)	= 255 : Blue(8) = 100

' HDR ball
CustomBallGlow(9) = 		False
CustomBallImage(9) = 		"ball_HDR"
CustomBallLogoMode(9) = 	False
CustomBallDecal(9) = 		"JPBall-Scratches"
CustomBulbIntensity(9) = 	0.01
Red(9) = 0 : Green(9)	= 0 : Blue(9) = 0

'********************
'End Options
'********************

Dim MaxBalls
MaxBalls = 8

Const BallSize = 50
Const BallMass = 1.65

' *** prepare the variable with references to three lights for glow ball ***
Dim Glowing(10)
Set Glowing(0) = Glowball0
Set Glowing(1) = Glowball1
Set Glowing(2) = Glowball2
Set Glowing(3) = Glowball3
Set Glowing(4) = Glowball4
Set Glowing(5) = Glowball5
Set Glowing(6) = Glowball6
Set Glowing(7) = Glowball7
Set Glowing(8) = Glowball8
Set Glowing(9) = Glowball9

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01120100", "de.vbs", 3.23

Dim DesktopMode:DesktopMode = Table1.ShowDT

'********************
'Standard definitions
'********************
   
Const UseSolenoids = 2
Const UseLamps = 0
'Const UseGI = 0
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = "solenoid"
Const SSolenoidOff = "soloff"
Const SFlipperOn = "fx_flipperup"
Const SFlipperOff = "fx_flipperdown"
Const SCoin = "fx_coin"

'Table Specific Sounds
Const SfxOuthole    = "Outhole"    	' Ball hits outhole
Const SfxTroughRel  = "Release"    	' Ball kicked into plunger lane
Const sfxTroughKick = "Kick"       	' Ball kicked from outhole into trough
Const SfxSling      = "Sling"      	' Slingshot
Const SfxKnocker    = "Knocker"    	' Knocker
Const SfxPopper     = "Popper"     	' Popper
Const SfxJet		= "Jet1"	    ' Jet Bumper Sound
Const SfxHole		= "Hole"	    ' Ball falls into hole
Const SfxBallDrop	= "BallDrop"   	' Ball falling off ramp end and hitting playfield
Const SfxPlunger    = "Plunger2"   	' Plunger Hitting Ball Sound
Const SfxPlun	    = "Plunger"    	' Plunger Missing Ball Sound
Const SfxGate		= "Gate"	    ' Ball Hitting Gate Sound
Const SfxSTarg		= "STarg"      	' Ball hitting standup target
Const SfxKick		= "Kick"	    ' Ball kicked from scoop
Const SfxSaucer     = "Saucer"     	' Ball kicked out of saucer
Const SfxMotor      = "Motor"      	' Shaker Motor
Const SfxMotorR     = "MotorR"     	' TRex Motor
Const SfxSubway     = "Subway"     	' Ball Moving Under Playfield
Const SfxJaw		= "Jaw"			' TRex Jaw sound
Const SfxHoleEnter  = "hole_enter"  'Hole Enter

'******************************************************
' 					TABLE INIT
'******************************************************
' Initialize table
Dim bsTrough, bsTroughE, bsLScoop, bsBoatDock, CaptiveBall
Dim cbLeft, bsRTPop, TRexxMotor, obj
Dim mPad

Const cGameName = "jupk_600"

Sub Table1_Init
 	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "JurassicPark (Data East 1993) By Dark"
		.Games(cGameName).Settings.Value("rol") = 0
		.HandleMechanics = 0
		.HandleKeyboard = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.ShowTitle = 0
		.Hidden = 0
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With

    '************  Main Timer init  ********************

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

    '*****************  Nudging  ***********************

 	vpmNudge.TiltSwitch  = 1
 	vpmNudge.Sensitivity = .5
 	vpmNudge.Tiltobj = Array(LeftSlingShot,RightSlingShot,sw45,sw46,sw47) ' Slings and Pop Bumpers

	ChangeBall(ChooseBall)
	SetSlingPlastics

	' Trough
 	Set bsTrough = New cvpmBallStack : With bsTrough
 		.Initsw 0,14,13,12,11,10,9,0
 		.InitAddSnd SfxOuthole 
 		.InitEntrySnd SfxTroughKick, SSolenoidOn
 		.CreateEvents "bsTrough", Drain
 		.IsTrough = True 
 		.Balls = 6 
 	End With
 
	' Trough Ejector
  	Set bsTroughE = New cvpmBallStack : With bsTroughE
 		.Initsw 0,15,0,0,0,0,0,0
 		.InitKick BallRelease,150,7
		.InitExitSnd SfxTroughRel,SSolenoidOn
  	End With
 
	'Turn GI ON
 	GIRelay 0
		
	Init_Trex

	'*****************  Captive Ball  ***********************
	Set CaptiveBall = Captive.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Captive.kick 180,1
	Captive.enabled = false

	If Table1.ShowDT = False then
       leftrail.visible = 0
       rightrail.visible = 0
       sidewalls.visible = 0
       Ramp17.visible = 0
     End If
	If Table1.ShowDT = True then
		Primitive_SideWallReflect.visible = 0
		Primitive_SideWallReflect1.visible = 0
	End If
End Sub

Dim SlingPlastics
'************  Sling Plastic option  ********************
Sub SetSlingPlastics()

	If SlingPlastics = 1 Then
		Primitive_Slings.image = "RedSlings"
	Else
		Primitive_Slings.image = "BlueSlings"
	End If

End Sub

'******************************************************
'					SOLENOIDS
'******************************************************

SolCallback(1)    	= "BoatDockSaucer"				' Boat Dock Eject 
SolCallback(2)		= "TroughRelease"				' Trough Release
SolCallback(3)		= "Autofire"					' AutoLaunch
SolCallback(4)    	= "LeftScoopEject"				' Left Scoop Eject 
SolCallback(5)    	= "VukKick"						' Right VUK 
SolCallback(6)    	= "Divert"						' Ramp Diverter 
SolCallback(7)    	= "RexSaucer"					' TRex Saucer Eject 
SolCallback(8)    	= "vpmSolSound SfxKnocker,"		' Knocker 
SolCallback(9)    	= "RaptorKick"					' Raptor Pit 
'SolCallback(10)   	= ""							' L/R Relay (internal solenoid select for same numbered functions)
SolCallback(11)   	= "GIRelay"						' GI
SolCallback(12)   	= "RexLeftRight"				' TRex Left/Right Select
SolCallback(13)   	= "RexMouth"					' TRex Mouth
SolCallback(14)   	= "RexUpDown"					' TRex Up/Down Motor
SolCallback(15)   	= "RexMotor"					' TRex Left/Right Motor On/Off
SolCallback(16)   	= "TroughLockout"				' Trough Lockout
SolCallback(17)		= "TJet"						' Top   Jet
SolCallback(18)		= "LJet"						' Left  Jet
SolCallback(19)		= "RJet"						' Right Jet
SolCallback(20)		= "vpmSolSound SfxSling,"		' Left  Slingshot
SolCallback(21)		= "vpmSolSound SfxSling,"		' Right Slingshot
SolCallback(22)   	= "ShakerMotor"					' Cabinet Shaker
SolCallback(23)   	= "RelayAC"				    	' AC Relay

'******************************************************
'						FLIPPERS
'******************************************************

SolCallback(sLRFlipper)     = "SolRFlipper" 
SolCallback(sLLFlipper)     = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFContactors), 0, 2*VolumeDial, -0.1, 0.25
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown",DOFContactors), 0, 2*VolumeDial, -0.1, 0.25
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFContactors), 0, 2*VolumeDial, 0.1, 0.25
        RightFlipper.RotateToEnd
		UpperRightFlipper.RotateToEnd	
    Else
        PlaySound SoundFX("fx_flipperdown",DOFContactors), 0, 2*VolumeDial, 0.1, 0.25
        RightFlipper.RotateToStart
		UpperRightFlipper.RotateToStart 
    End If
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub UpperRightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub


Sub TroughRelease(enabled) : if enabled then : bstroughE.solout 1 : end if : End Sub 
 Sub TroughLockout(enabled) : if enabled and bstrough.balls > 0 then : bstrough.balls = bstrough.balls - 1 : bstroughE.addball 1 : end if : End Sub


'******************************************************
'					Left Scoop
'******************************************************

Sub LeftScoopEject(enabled)
 	If enabled Then
		If sw35.ballCntOver then
			Playsound SoundFX(sfxkick,DOFContactors), 0, 2*VolumeDial
		Else
			Playsound SoundFX(SSolenoidOn,DOFContactors), 0, 2*VolumeDial
			End If
		sw35.kick 0,30,1.5
		sw35wall.collidable = False
		Controller.Switch(35) = False
 	End If
End Sub

sub sw35_hit
	sw35wall.collidable = True
	PlaySound sfxHole, 0, 1
	Controller.Switch(35) = True
End sub

sub Trigger2_hit
	sw35wall.collidable = False
	Playsound "Hit3", 0, 2*VolumeDial
End sub
'******************************************************
'					Middle Scoop Trough
'******************************************************

sub sw37_hit
	activeball.velx = 0
	vpmTimer.PulseSw 37
    Playsound "Hit1", 0, 2*VolumeDial
End sub

'******************************************************
'					Right Scoop Trough
'******************************************************

sub sw60_hit
	vpmTimer.PulseSw 60
End sub

'******************************************************
'					Upper Right Scoop Trough
'******************************************************

sub Trigger1_hit
	Playsound "Hit3", 0, 2*VolumeDial
End sub

'******************************************************
'					Trex Trough
'******************************************************

sub sw59_hit
	vpmTimer.PulseSw 59
End sub

'******************************************************
'						VUK
'******************************************************

Sub VukKick(enabled)
 	If enabled Then
		If vuk.ballCntOver then
			Playsound SoundFX(sfxkick,DOFContactors), 0, 2*VolumeDial
			PlaySound "wireramp_right", 0, 0.3*volumedial, Csng((900 * 2 / table1.width-1) ^10), 0, 100, 1, 0
		Else
			Playsound SoundFX(SSolenoidOn,DOFContactors), 0, 2*VolumeDial
		End If
		vuk.kick 90,60,1.5
		vukwall.collidable = False
		Controller.Switch(61) = False
 	End If
End Sub

sub vuk_hit
	vukwall.collidable = True
	PlaySound sfxHole, 0, 2*VolumeDial
	Controller.Switch(61) = True
End sub

'******************************************************
'					Boat Dock Saucer
'******************************************************

Sub BoatDockSaucer(enabled)
	If enabled Then
		If Controller.Switch(56) = True then
			Playsound SoundFX(SfxSaucer,DOFContactors), 0, 2*VolumeDial
		Else
			Playsound SoundFX(SSolenoidOn,DOFContactors), 0, 2*VolumeDial
		End If
		Controller.Switch(56) = False
		sw56.Kick 160,10
		KickerArm2.TransY = 10
		vpmTimer.AddTimer 150, "KickerArm2.TransY = 0'"
	End If
	'sw56.enabled = False
End Sub

sub sw56_hit()
	PlaySound "Kicker_enter_center", 0, 2*VolumeDial
	Controller.Switch(56) = True
End sub

'******************************************************
'					Raptor Pit
'******************************************************
  
Sub RaptorKick(enabled)
	If enabled Then
		PlaySound SfxPlunger, 0, 2*VolumeDial
		sw29.Kick 180, 41
		controller.switch (29) = false
		Primitive16.transZ = 0
	End If
End Sub

'******************************************************
'					Shaker Motor
'******************************************************
 
 Sub ShakerMotor(enabled)
	If enabled Then
		ShakeTimer.Enabled = 1
		PlaySound SoundFX(SfxMotor,DOFShaker), -1, 2*VolumeDial
	Else
		ShakeTimer.Enabled = 0
		StopSound SfxMotor
	End If
End Sub

Sub ShakeTimer_Timer()
	'Nudge 0,.2	
	'Nudge 90,.2
	'Nudge 180,.1
	'Nudge 270,.1
End Sub   

'******************************************************
'					Diverter
'******************************************************

Dim Div_Dir

Sub Divert(enabled)
	Diverter_On.timerinterval = 1
	If enabled Then
		Diverter_On.IsDropped = True
		Div_Dir = 1
		Diverter_On.timerenabled = True
	Else
		Diverter_On.IsDropped = False
		Div_Dir = -1
		Diverter_On.timerenabled = True
	End If
End Sub

Sub Diverter_On_timer()
	Primitive_Diverter.ObjRotZ = Primitive_Diverter.ObjRotZ + Div_Dir
	If Primitive_Diverter.ObjRotZ > 110 Then
		Primitive_Diverter.ObjRotZ = 110
		me.timerenabled = False
	elseif Primitive_Diverter.ObjRotZ < 90 Then
		Primitive_Diverter.ObjRotZ = 90
		me.timerenabled = False
	End If
End Sub

Sub TJet(Enabled)
 vpmSolSound SfxJet,Enabled
   If Enabled Then
        SetLamp 31, 1 = abs(enabled)
        SetLamp 131, 1 = abs(enabled)
     else
        SetLamp 31, 0
        SetLamp 131, 0
   End If
 End Sub

 Sub RJet(enabled)
  vpmSolSound SfxJet,Enabled
   If Enabled Then
        SetLamp 32, 1 = abs(enabled)
        SetLamp 132, 1 = abs(enabled)
     else
        SetLamp 32, 0
        SetLamp 132, 0
   End If
 End Sub

 Sub LJet(enabled)
  vpmSolSound SfxJet,Enabled
   If Enabled Then
        SetLamp 62, 1 = abs(enabled)
        SetLamp 162, 1 = abs(enabled)
     else
        SetLamp 62, 0
        SetLamp 162, 0
   End If
 End Sub

'*****************  Plunger  ***********************

Sub Autofire(enabled)
	If enabled Then Plunger1.fire
End Sub

'*****************  GI Stuff  ***********************
 
Sub GIRelay(enabled)
	If enabled Then
		GiOFF
		Playsound "fx_relay_off", 0, 2*VolumeDial
		setlamp 140, 1
		setlamp 141, 1
		setlamp 142, 1
		setlamp 143, 1
		setlamp 144, 0
		setlamp 145, 0
		setlamp 146, 1
		setlamp 147, 0
		setlamp 149, 1
		Primitive_SideWallReflect.visible = 0
		Primitive_SideWallReflect1.visible = 1
	If Table1.ShowDT = True then
		Primitive_SideWallReflect.visible = 0
		Primitive_SideWallReflect1.visible = 0
	End if	
	If SideWallReflect = 0 Then
		Primitive_SideWallReflect1.visible = 0
		Primitive_SideWallReflect.visible = 0
	End If
		
	Else
		GiON
		Playsound "fx_relay_on", 0, 2*VolumeDial
		setlamp 140, 0
		setlamp 141, 0
		setlamp 142, 0
		setlamp 143, 0
		setlamp 144, 1
		setlamp 145, 1
		setlamp 146, 0
		setlamp 147, 1
		setlamp 149, 0
		Primitive_SideWallReflect.visible = 1
		Primitive_SideWallReflect1.visible = 1
	If Table1.ShowDT = True then
		Primitive_SideWallReflect.visible = 0
		Primitive_SideWallReflect1.visible = 0
		End if	
	If SideWallReflect = 0 Then
		Primitive_SideWallReflect1.visible = 0
		Primitive_SideWallReflect.visible = 0
	End If
		
	End If
	ColorGrade
End Sub

Sub GiON
	Dim x
	For each x in Gi:x.State = 1:Next
End Sub

Sub GiOFF
	Dim x
	For each x in Gi:x.State = 0:Next
End Sub

Sub RelayAC(enabled)  
	vpmNudge.SolGameOn enabled 	' Tie In Nudge to AC Relay
End Sub	
 

'******************************************************
' 						KEYS
'******************************************************

Dim change:change=1

Sub Table1_KeyDown(ByVal keycode)

 	If keycode = PlungerKey Then  'Launch Trigger
		controller.switch(41) = True
	End If 

	If keycode = RightMagnaSave or keycode = LeftMagnaSave or keycode = 3 Then  'Smart Bomb Button
		controller.switch(42) = True 
	End If 

If keycode = LeftTiltKey Then vpmNudge.DoNudge 5, 3.5 
If keycode = RightTiltKey Then vpmNudge.DoNudge 355, 3.5 
If keycode = CenterTiltKey Then vpmNudge.DoNudge 0, 3.5

	'*************Sling Plastic invert**************
	If keycode = 23 Then
		If change=0 then 
			Primitive_Slings.image = "BlueSlings"
			change=1
		ElseIf change=1 Then 
			Primitive_Slings.image = "redslings"
			change=0 
		End If
	End If

	'*************   Ball Control 1/3   **************
	if keycode = 46 then	 			' C Key
		If contball = 1 Then
			contball = 0
		Else
			contball = 1
		End If
	End If
	if keycode = 31 then 				'S Key
		If bcboost = 1 Then
			bcboost = bcboostmulti
		Else
			bcboost = 1
		End If
	End If
	if keycode = 203 then bcleft = 1		' Left Arrow
	if keycode = 200 then bcup = 1			' Up Arrow
	if keycode = 208 then bcdown = 1		' Down Arrow
	if keycode = 205 then bcright = 1		' Right Arrow
	'*************   Ball Control 1/3   **************

    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then   'Launch Trigger
		controller.switch(41) = False
	End If
	
	If keycode = RightMagnaSave or keycode = LeftMagnaSave Then  'Smart Bomb Button
		controller.switch(42) = False
	End If 

	'*************   Ball Control 2/3   **************
	if keycode = 203 then bcleft = 0		' Left Arrow
	if keycode = 200 then bcup = 0			' Up Arrow
	if keycode = 208 then bcdown = 0		' Down Arrow
	if keycode = 205 then bcright = 0		' Right Arrow
	'*************   Ball Control 2/3   **************

	If vpmKeyUp(keycode) Then Exit Sub
End Sub

'*************   Ball Control 3/3   **************

Sub StartControl_Hit()
	Set ControlBall = ActiveBall
	contballinplay = true
End Sub

Sub StopControl_Hit()
	contballinplay = false
End Sub	

Dim bcup, bcdown, bcleft, bcright, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti

bcboost = 1		'Do Not Change - default setting
bcvel = 4		'Controls the speed of the ball movement
bcyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3	'Boost multiplier to ball veloctiy (toggled with the B key) 

Sub BallControl_Timer()
	If Contball and ContBallInPlay and enableBallControl then
		If bcright = 1 Then
			ControlBall.velx = bcvel*bcboost
		ElseIf bcleft = 1 Then
			ControlBall.velx = - bcvel*bcboost
		Else
			ControlBall.velx=0
		End If

		If bcup = 1 Then
			ControlBall.vely = -bcvel*bcboost
		ElseIf bcdown = 1 Then
			ControlBall.vely = bcvel*bcboost
		Else
			ControlBall.vely= bcyveloffset
		End If
	End If
End Sub

If enableBallControl Then
	BallControl.interval = 1
	BallControl.enabled = True
Else
	BallControl.enabled = False
End If

'*************   Ball Control 3/3   **************

'******************************************************
'				SWITCHES
'******************************************************

'Rollover and Optos

Sub Slowdown_Hit():activeball.velx=activeball.velx*2/3:end sub ' Outer Loop Slow Down Top Center
	

Sub Sw17_Hit() ' Outer Loop Low
	Controller.Switch(17) = True
	PlaySound "sensor"
End Sub 
Sub Sw17_UnHit()
	Controller.Switch(17) = False
End Sub
Sub Sw18_Hit() ' Outer Loop Top
	Controller.Switch(18) = True
	PlaySound "sensor"
End Sub 
Sub Sw18_UnHit()
	Controller.Switch(18) = False
End Sub
Sub Sw19_Hit() ' Inner Loop Low
	Controller.Switch(19) = True
	PlaySound "sensor"
End Sub 
Sub Sw19_UnHit()
	Controller.Switch(19) = False
End Sub
Sub Sw20_Hit() ' Inner Loop Top
	Controller.Switch(20) = True
	PlaySound "sensor"
End Sub 
Sub Sw20_UnHit()
	Controller.Switch(20) = False
End Sub
Sub Sw21_Hit() ' Right Outlane
	Controller.Switch(21) = True
	PlaySound "sensor"
End Sub 
Sub Sw21_UnHit()
	Controller.Switch(21) = False
End Sub
Sub Sw22_Hit() ' Right Return
	Controller.Switch(22) = True
	PlaySound "sensor"
End Sub 
Sub Sw22_UnHit()
	Controller.Switch(22) = False
End Sub
Sub Sw23_Hit() ' Left Return
	Controller.Switch(23) = True
	PlaySound "sensor"
End Sub 
Sub Sw23_UnHit()
	Controller.Switch(23) = False
End Sub
Sub Sw24_Hit() ' Left Outlane
	Controller.Switch(24) = True
	PlaySound "sensor"
End Sub 
Sub Sw24_UnHit()
	Controller.Switch(24) = False
End Sub


Sub swPLS_Hit() ' Left Outlane
	Controller.Switch(16) = True
	PlaySound "sensor"
End Sub 
Sub swPLS_UnHit()
	Controller.Switch(16) = False
End Sub

Sub lsw29_Hit() ' Center Outlane
	PlaySound "sensor"
End Sub 
Sub lsw29_UnHit()
End Sub
 Sub sw33_Hit()  	  : controller.switch (33) = true  : End Sub	' Right Ramp Enter
 Sub sw33_Unhit()  	  : controller.switch (33) = false : End Sub
 Sub sw34_Hit()  	  : controller.switch (34) = true  : End Sub	' Right Ramp Exit
 Sub sw34_Unhit()  	  : controller.switch (34) = false : End Sub


'Stand up Targets

  Sub sw25_Hit:sw25p.transZ = 5:Me.TimerEnabled = 1:vpmTimer.PulseSw 25:End Sub ' Spitter Target #1 Bottom
  Sub sw25_Timer:sw25p.transZ = 0:Me.TimerEnabled = 0:End Sub
  Sub sw26_Hit:sw26p.transZ = 5:Me.TimerEnabled = 1:vpmTimer.PulseSw 26:End Sub ' Spitter Target #2 Middle
  Sub sw26_Timer:sw26p.transZ = 0:Me.TimerEnabled = 0:End Sub
  Sub sw27_Hit:sw27p.transZ = 5:Me.TimerEnabled = 1:vpmTimer.PulseSw 27:End Sub '' Spitter Target #3 Top
  Sub sw27_Timer:sw27p.transZ = 0:Me.TimerEnabled = 0:End Sub
  Sub sw38_Hit:sw38p.transZ = 5:Me.TimerEnabled = 1:vpmTimer.PulseSw 38:End Sub ' Herrerasaurus Low
  Sub sw38_Timer:sw38p.transZ = 0:Me.TimerEnabled = 0:End Sub
  Sub sw39_Hit:sw39p.transZ = 5:Me.TimerEnabled = 1:vpmTimer.PulseSw 39:End Sub ' Herrerasaurus Top
  Sub sw39_Timer:sw39p.transZ = 0:Me.TimerEnabled = 0:End Sub
  Sub sw40_Hit:sw40p.transZ = 5:Me.TimerEnabled = 1:vpmTimer.PulseSw 40:End Sub ' Brachiasaurus Low
  Sub sw40_Timer:sw40p.transZ = 0:Me.TimerEnabled = 0:End Sub
  Sub sw53_Hit:sw53p.transZ = 5:Me.TimerEnabled = 1:vpmTimer.PulseSw 53:End Sub ' Brachiasaurus Top
  Sub sw53_Timer:sw53p.transZ = 0:Me.TimerEnabled = 0:End Sub
  Sub sw49_Hit:sw49p.transZ = 5:Me.TimerEnabled = 1:vpmTimer.PulseSw 49:End Sub ' Baryonyx Target
  Sub sw49_Timer:sw49p.transZ = 0:Me.TimerEnabled = 0:End Sub
  Sub sw50_Hit:sw50p.transZ = 5:Me.TimerEnabled = 1:vpmTimer.PulseSw 50:End Sub ' Gallimimus Target
  Sub sw50_Timer:sw50p.transZ = 0:Me.TimerEnabled = 0:End Sub
  Sub sw52_Hit:sw52p.transZ = 5:Me.TimerEnabled = 1:vpmTimer.PulseSw 52:End Sub ' Gallimimus Target
  Sub sw52_Timer:sw52p.transZ = 0:Me.TimerEnabled = 0:End Sub
  Sub sw48_Hit:sw48p.transZ = 5:Me.TimerEnabled = 1:vpmTimer.PulseSw 48:End Sub ' Mostquito Captive Ball
  Sub sw48_Timer:sw48p.transZ = 0:Me.TimerEnabled = 0:End Sub
		
  'Pop Bumpers

  Sub sw45_Hit() :PlaySound SoundFX("fx_bumper1",DOFContactors),0,1*VolumeDial: vpmtimer.pulsesw 45: End Sub	' Jet Bumper (Top)
  Sub sw46_Hit() :PlaySound SoundFX("fx_bumper2",DOFContactors),0,1*VolumeDial: vpmtimer.pulsesw 46: End Sub	' Jet Bumper (Left)
  Sub sw47_Hit() :PlaySound SoundFX("fx_bumper3",DOFContactors),0,1*VolumeDial: vpmtimer.pulsesw 47: End Sub	' Jet Bumper (Right)
 

'Raptor Pit
   Sub sw29_Hit() : Primitive16.transZ = -30 : controller.switch (29) = true : End Sub

Sub RRail_Hit()
	PlaySound "wireramp_right", 0, 0.3*volumedial, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub RRHelp_Hit()
	StopSound "wireramp_right"
	PlaySound "wireramp_stop", 0, 0.3*volumedial, 0.2
	PlaySound "ball_bounce"
End Sub

Sub LRHelp_Hit()
	PlaySound "ball_bounce"
End Sub

Dim CBRest
CBRest = 1
Sub CBResting_Hit()
	CBRest = 1
end sub

Sub CBResting_UnHit()
	CBRest = 0
end sub

'******************************************************
'					TREX Saucer
'******************************************************

Dim TrexBall

Sub RexSaucer(enabled)
	If enabled Then
		If Controller.Switch(55) = True then
			Playsound SoundFX(SfxSaucer,DOFContactors), 0, 2*VolumeDial
		Else
			Playsound SoundFX(SSolenoidOn,DOFContactors), 0, 2*VolumeDial
		End If
		Controller.Switch(55) = False
		sw55.Kick 150,12
		KickerArm1.TransY = 10
		vpmTimer.AddTimer 150, "KickerArm1.TransY = 0'"
	End If
	sw55.enabled = false
End Sub

sub sw55_hit()
	PlaySound "Kicker_enter_center", 0, 2*VolumeDial
	Controller.Switch(55) = True
	Set Trexball = Activeball
End sub


'*********************************************************
'					TRex Toy Handling
'*********************************************************

dim trrightangle, trleftangle, trforwardangle, trcenterangle 
dim RexBendDir, RexRotSpeed, RexLift, RexRand 

'**************  Start TRex Control Variables ******************

trrightangle = -23				'Max angle to rotate right
trleftangle = -3				'Max angle to rotate left
trforwardangle = -80			'Max angle to bend forward
trcenterangle = -13				'Angle when centered
RexBendDir = -.25 				'Controls speed and smoothness of bend animation
RexRotSpeed = 0.2				'Controls speed and smoothness of rotation animation
RexLift = 0.75					'Amount Trex head lifts when jaw closes
RexRand = 0.5					'Randomness of Rexlift: 0 is no randomness, 1 is full randomness

'**************  End TRex Control Variables ******************

'If DesktopMode = 0 Then RexLift = Rexlift/2 

Sub Init_Trex()
	controller.switch(31) = False	'TRex Right
	controller.switch(32) = False	'TRex Left
	controller.switch(36) = True	'TRex Center
 	controller.switch(57) = True	'TRex Top/Up
	controller.switch(58) = False	'TRex Forward/Down
End Sub

dim trexcenterangle, trexpivotx, trexpivoty, trexmainx, trexmainy, trexmainz, trexjawx, trexjawy, trexjawz
dim trexballanglez, trexballanglexy, trexballradius

trexcenterangle = 90 - trcenterangle  '103 degrees
trexpivotx = TrexPlastic.x
trexpivoty = TrexPlastic.y

trexmainx = TrexMain.x
trexmainy = TrexMain.y
trexmainz = TrexMain.z

trexjawx = TrexJaw.x
trexjawy = TrexJaw.y
trexjawz = TrexJaw.z

dim trexjawradiusxy, trexjawradiusz, trexjawanglez, trexmainradius

trexjawradiusxy = Distance(trexpivotx,trexpivoty,0,trexjawx,trexjawy,0)
trexjawradiusz = Distance(trexmainx,trexmainy,trexmainz,trexjawx,trexjawy,trexjawz)
trexjawanglez = Degrees(acos((trexjawz-trexmainz)/trexjawradiusz))

trexmainradius = Distance(trexpivotx,trexpivoty,0,trexmainx,trexmainy,0)

'********** TRex Up/Down Control **********
Sub RexUpDown(enabled)
	If enabled then			
		UpDownTimer.enabled = 1
TrexMain.blenddisablelighting = 0.3
TrexJaw.blenddisablelighting = 0.3
	else
		UpDownTimer.Enabled = 0
TrexMain.blenddisablelighting = 0.1
TrexJaw.blenddisablelighting = 0.1
	end if
End Sub

dim ballinmouth : ballinmouth = 0 
dim RexBendRot : RexBendRot = 0
dim xd, yd, zd

Sub UpDownTimer_Timer()
	If Controller.Switch(36) = True then 'if trex is centered

		PlaySound SfxMotorR, 20
		'RexBendRot = RexBendRot + RexBendDir

If RexBendRot < trforwardangle + 1 Then RexBendRot = RexBendRot + RexBendDir/10 Else RexBendRot = RexBendRot + RexBendDir End If

		TrexBend()

		If ballinmouth then
			trexballanglez = trexballanglez + RexBendDir
			trexball.x = trexmain.x + trexballradius*cos(radians(trexballanglexy))*Sin(Radians(trexballanglez))
			trexball.y = trexmain.y + trexballradius*sin(radians(trexballanglexy))*Sin(Radians(trexballanglez))
			trexball.z = trexmain.z - trexballradius*cos(radians(trexballanglez))
		End If

		If RexBendRot <= trforwardangle then
			RexBendRot = trforwardangle
			RexBendDir = -RexBendDir
			if sw55.ballCntOver then 
				Playsound "1-Pickup"
				Controller.Switch(55) = False
				ballinmouth = 1
				
				trexballradius = Distance(trexball.x,trexball.y,trexball.z,trexmain.x,trexmain.y,trexmain.z)
				trexballanglez = Degrees(acos((trexball.z-trexmainz)/trexballradius))
				trexballanglexy = Degrees(acos((trexball.x-trexmainx)/trexballradius))
			end if
		end if
		If RexBendRot <= trforwardangle+2 Then 
			Controller.Switch(58) = True
		Else 
			Controller.Switch(58) = False
		End If

		If RexBendRot > 0 Then RexBendRot = 0: RexBendDir = -RexBendDir
		If RexBendRot >= 0-1.5 Then			
			Controller.Switch(57) = True
			if ballinmouth = 1 then
				PlaySound "2-Swallow"
				sw55.Kick -13,2	
				ballinmouth = 0
			end if
		Else 
			Controller.Switch(57) = False 
		end if
	end if
End Sub

Sub TrexBend()
	TrexMain.rotx = RexBendRot
	TrexJaw.rotx = RexBendRot

	trexjaw.x = trexmain.x - trexjawradiusz*cos(radians(90-RexRot))*Sin(Radians(-trexjawanglez - RexBendRot)) - 4  ' -4 to correct for a slight error in calculations used
	trexjaw.y = trexmain.y + trexjawradiusz*sin(radians(90-RexRot))*Sin(Radians(-trexjawanglez - RexBendRot))
	trexjaw.z = trexmain.z + trexjawradiusz*cos(radians(-trexjawanglez - RexBendRot)) 
End Sub

'TRex Mouth Handling
Sub RexMouth(enabled)
	If enabled Then	
		MouthDir = 1
		MouthOpener.Enabled = True
		Playsound "3-Chew"
TrexMain.blenddisablelighting = 0.3
TrexJaw.blenddisablelighting = 0.3
	Else 
		MouthDir = -1
		MouthOpener.Enabled = True
TrexMain.blenddisablelighting = 0.1
TrexJaw.blenddisablelighting = 0.1
	end if
End Sub

Dim MouthPos, MouthDir, RandLift

MouthPos = 0

Sub MouthOpener_Timer()
	MouthPos = MouthPos + MouthDir
	If MouthPos => 6 then MouthPos = 6
	If MouthPos <= 0 Then MouthPos = 0: RandLift = Rnd * RexRand
	If MouthPos = 0 Or MouthPos = 6 then MouthOpener.Enabled = 0

	trexjaw.objrotx = Mouthpos*2
	
	If RexBendRot >= -1.5 Then
		RexBendRot = MouthPos * RexLift * (RandLift + (1 - RexRand))
		TrexBend()
	End If
End Sub

'TRex Select Left/Right Direction
dim goleft : goleft = 1

Sub RexLeftRight(enabled) 
	If enabled then
 		goleft = -1
 	else
 		goleft = 1
 	end if
End Sub
 
'TRex Left/Right Motor Control 
Sub RexMotor(enabled)
    if enabled then
	MotorTimer.enabled = abs(enabled)
'TrexMain.blenddisablelighting = 0.3
'TrexJaw.blenddisablelighting = 0.3
    else
MotorTimer.enabled = abs(enabled)
'TrexMain.blenddisablelighting = 0.1
'TrexJaw.blenddisablelighting = 0.1
end if
End Sub

'TRex Timers
Dim RexRot: RexRot = trcenterangle

Sub MotorTimer_Timer()
	if Controller.Switch(57) = True then ' if trex is up
		PlaySound SfxMotorR, 20
		RexRot = RexRot + goleft*RexRotSpeed
		
		TrexMain.ObjRotZ = RexRot
		TrexJaw.ObjRotZ = RexRot
		TrexPlastic.ObjRotZ = RexRot 

		trexjaw.x = trexjawx + trexjawradiusxy*cos(radians(RexRot+90)) - trexjawradiusxy*cos(radians(180-trexcenterangle))
		trexjaw.y = trexjawy + trexjawradiusxy*sin(radians(RexRot+90)) - trexjawradiusxy*sin(radians(180-trexcenterangle))
		trexmain.x = trexmainx + trexmainradius*cos(radians(RexRot+90)) - trexmainradius*cos(radians(180-trexcenterangle))
		trexmain.y = trexmainy + trexmainradius*sin(radians(RexRot+90)) - trexmainradius*sin(radians(180-trexcenterangle))

		If RexRot <= trrightangle then 
			controller.switch(31) = true
		else
			controller.switch(31) = False
		end If
		If RexRot >= trleftangle then 
			controller.switch(32) = true
		else
			controller.switch(32) = False
		end If
		If RexRot > trcenterangle - 0.6 and RexRot < trcenterangle + 0.6 then 
			controller.switch(36) = true
		else
			controller.switch(36) = False
		end If
 	End If
End Sub

'*********************************************************
'					End TRex Toy Handling
'*********************************************************

'*********************************************************
'					Dino shake
'*********************************************************


Dim mMagnet, cBall, pMod, rmmod

 Set mMagnet = new cvpmMagnet
 With mMagnet
	.InitMagnet WobbleMagnet, 1.5
	.Size = 100
	.CreateEvents mMagnet
	.MagnetOn = True
 End With
 WobbleInit

 Sub RMShake
	cball.velx = cball.velx + rmball.velx*pMod
	cball.vely = cball.vely + rmball.vely*pMod
 End Sub

'Includes stripped down version of my reverse slope scripting for a single ball
 Dim ngrav, ngravmod, pslope, nslope, slopemod
 Sub WobbleInit
	pslope = Table1.SlopeMin +((Table1.SlopeMax - Table1.SlopeMin) * Table1.GlobalDifficulty)
	nslope = pslope
	slopemod = pslope + nslope
	ngravmod = 60/aWobbleTimer.interval
	ngrav = slopemod * .0905 * Table1.Gravity / ngravmod
	pMod = .15					'percentage of hit power transfered to captive wobble ball
	Set cBall = ckicker.createball:cball.image = "blank":ckicker.Kick 0,0:mMagnet.addball cball
	aWobbleTimer.enabled = 1
 End Sub

 Sub aWobbleTimer_Timer
'	BallShake.Enabled = RMBallInMagnet
	cBall.Vely = cBall.VelY-ngrav					'modifier for slope reversal/cancellation
'	rmmod = (ringmaster.z+265.5)/265*.4				'.4 is a 40% modifier for ratio of ball movement to head movement
	dino.objrotx = (ckicker.y - cball.y)'*rmmod
	dino.objroty = (cball.x - ckicker.x)'*rmmod
	dino1.objrotx = -(ckicker.y - cball.y)'*rmmod
	dino1.objroty = -(cball.x - ckicker.x)'*rmmod
 End Sub

'*********************************************************
'					End Dino shake
'*********************************************************


'*********************************************************
'						Functions
'*********************************************************

Function PI()
	PI = 4*Atn(1)
End Function

Function Radians(angle)
	Radians = PI * angle / 180
End Function

Function Degrees(radians)
	Degrees = 180 * radians / PI
End Function

Function ASin(val)
    ASin = 2 * Atn(val / (1 + Sqr(1 - (val * val))))
End Function
 
Function ACos(val)
    ACos = PI / 2 - ASin(val)
End Function

Function Distance(ax,ay,az,bx,by,bz)
	Distance = SQR((ax - bx)^2 + (ay - by)^2 + (az - bz)^2)
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


'-----------------------------------
' Ramp Diverter
'-----------------------------------

'Sub Divert(enabled) : If enabled Then : Diverter_Off.IsDropped = False : Diverter_On.IsDropped = True : Primitive_Diverter.ObjRotZ = 110 : Else : Diverter_Off.IsDropped = True : Diverter_On.IsDropped = False : Primitive_Diverter.ObjRotZ = 90 :End If : End Sub


'-----------------------------------
' Flipper Primitives 
'-----------------------------------
sub FlipperTimer_Timer()
	LFLogo.objRotZ = LeftFlipper.CurrentAngle
    RFlogo.objRotZ = RightFlipper.CurrentAngle
	RFlogo1.objRotZ = UpperRightFlipper.CurrentAngle
	pleftFlipper.rotz=leftFlipper.CurrentAngle
	prightFlipper.rotz=rightFlipper.CurrentAngle
	PRightFlipper1.rotz=UpperRightFlipper.CurrentAngle
	TrexShadow.objrotz=RexRot
	Trexplate.objrotz=RexRot
	TrexPlasticRivets.objrotz=RexRot

end sub

'*********************************************************
'					GATES
'*********************************************************
Sub timergate_Timer
	Dim RampSwitch

'Bumber Entrance Gate
	If Gate2Open Then
		If Gate2Angle < Gate2.currentangle Then:Gate2Angle=Gate2.currentangle:End If
		If Gate2Angle > 5 and Gate2.currentangle < 5 Then:Gate2Open=0:End If
		If Gate2Angle > 45 Then
			Primitive_GatePivot.rotz=-45
		Else
			Primitive_GatePivot.rotz=-Gate2Angle
			Gate2Angle=Gate2Angle - GateSpeed
		End If
	Else
		if Gate2Angle > 0 Then
			Gate2Angle = Gate2Angle - GateSpeed
		Else 
			Gate2Angle = 0
		End If
		Primitive_GatePivot.rotz=-Gate2Angle
	End If

'Shooter lane gate
If Gate3Open Then
		If Gate3Angle < Gate3.currentangle Then:Gate3Angle=Gate3.currentangle:End If
		If Gate3Angle > 5 and Gate3.currentangle < 5 Then:Gate3Open=0:End If
		If Gate3Angle > 45 Then
			Primitive_GatePivot1.rotz=-45
		Else
			Primitive_GatePivot1.rotz=-Gate3Angle
			Gate3Angle=Gate2Angle - GateSpeed
		End If
	Else
		if Gate3Angle > 0 Then
			Gate3Angle = Gate3Angle - GateSpeed
		Else 
			Gate3Angle = 0
		End If
		Primitive_GatePivot1.rotz=-Gate3Angle
	End If

	'Ramp gate behind trex
	RampSwitch = abs((rswitch.CurrentAngle + -0))
		If RampSwitch > 80 then
			rswitchP.ObjRotY = 80
		else
			rswitchP.ObjRotY = RampSwitch
		end if
End Sub       

Dim GateSpeed
GateSpeed = 1

Dim Gate2Open,Gate2Angle:Gate2Open=0:Gate2Angle=0
Dim Gate3Open,Gate3Angle:Gate3Open=0:Gate3Angle=0

Sub Gate2_Hit():Gate2Open=1:Gate2Angle=0:PlaySound "Gate2":End Sub
Sub Gate3_Hit():Gate3Open=1:Gate3Angle=0:PlaySound "Gate3":End Sub
Sub Gate1_Hit():PlaySound "BallRelease":End Sub



'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmtimer.pulsesw(44)
    PlaySound "right_slingshot", 0, 2*VolumeDial, 0.05, 0.05
	RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	'gi1.State = 0:Gi2.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0':gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmtimer.pulsesw(43)
    PlaySound "left_slingshot",0,2*VolumeDial,-0.05,0.05
	LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	'gi3.State = 0:Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0':gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
End Sub

'Flashers
 SolCallBack(25) = "Flash190" 		'1R Top middle Flashers
 SolCallBack(26) = "Flash191"		'2R Middle Right Flasher
 SolCallBack(27) = "Flash192"		'3R
 SolCallBack(28) = "Flash193"		'4R Right Flasher
 SolCallBack(29) = "Flash194" 		'5R Left Flasher
 SolCallBack(30) = "Flash195" 		'6R Middle Left Flasher
 SolCallBack(31) = "Flash196"		 	'7R
 SolCallBack(32) = "Flash197"		 	'8R
 

Sub Flash190(Enabled)
	l1r.state = enabled 	
end Sub

  
Sub Flash191(Enabled)
	'l2r.state = enabled 
	L2r1.state = enabled
	l2ra.state = enabled
	l2rab.state = enabled
	l2rb.state = enabled
	l2rbb.state = enabled
	if enabled Then
		Flasherdome1.disablelighting =  True
		Flasherdome1.material = "flasherlit"
		Flasherdome1.image = "dome jurassic on white"
		setlamp 148, 1
		setlamp 150, 1
		'setlamp 157, 1
fwl.state=1
	Else
		Flasherdome1.disablelighting =  False
		Flasherdome1.material = "flasherunlit"
		Flasherdome1.image = "dome jurassic off white"
		setlamp 148, 0
		setlamp 150, 0
		'setlamp 157, 0
fwl.state=0
	end if
end Sub

Sub Flash192(Enabled)
frl.state=1
	l3r.state = enabled 
	l3ra.state = enabled
	l3rb.state = enabled
	l3rc.state = enabled
	l3rd.state = enabled
	l3r1b.state = enabled 
	l3rab.state = enabled
	l3rbb.state = enabled
	l3rdb.state = enabled
	if enabled Then
		Flasherdome3.disablelighting =  True
		Flasherdome3.material = "flasherlit"
		Flasherdome3.image = "dome jurassic on"
		setlamp 155, 1
		setlamp 156, 1

	Else
		Flasherdome3.disablelighting =  False
		Flasherdome3.material = "flasherunlit"
		Flasherdome3.image = "dome jurassic off"
		setlamp 155, 0
		setlamp 156, 0
frl.state=0
	end if
end Sub

Sub Flash193(Enabled)
frr.state=1
    l4r.state = enabled 
	l4ra.state = enabled
	l4rb.state = enabled
	l4rc.state = enabled
    l4r1b.state = enabled 
	l4rab.state = enabled
	l4rbb.state = enabled
	l4rc.state = enabled
	if enabled Then
		Flasherdome4.disablelighting =  True
		Flasherdome4.material = "flasherlit"
		Flasherdome4.image = "dome jurassic on"
		setlamp 154, 1
		setlamp 153, 1

	Else
		Flasherdome4.disablelighting =  False
		Flasherdome4.material = "flasherunlit"
		Flasherdome4.image = "dome jurassic off"
		setlamp 154, 0
		setlamp 153, 0
frr.state=0
	end if
end Sub
  

Sub Flash194(Enabled)
	l5r.state = enabled 
	l5ra.state = enabled
	l5rb.state = enabled
	l5r1b.state = enabled 
	l5rab.state = enabled
	l5rbb.state = enabled
End Sub 

Sub Flash195(Enabled)
	l6r.state = enabled 
	l6ra.state = enabled
	l6rab.state = enabled
	l6rb.state = enabled
	l6rbb.state = enabled
	if enabled Then
		Flasherdome2.disablelighting =  True
		Flasherdome2.material = "flasherlit"
		Flasherdome2.image = "dome jurassic on white"
		setlamp 151, 1
		setlamp 152, 1
fwr.state=1
	Else
		Flasherdome2.disablelighting =  False
		Flasherdome2.material = "flasherunlit"
		Flasherdome2.image = "dome jurassic off white"
		setlamp 151, 0
		setlamp 152, 0
fwr.state=0
	end if
End Sub 

Sub Flash196(Enabled)
	l7ra.state = enabled
	l7rb.state = enabled
	l7rc.state = enabled
	l7rd.state = enabled
	l7rbb.state = enabled
	l7rcb.state = enabled
	l7rdb.state = enabled
End Sub

Sub Flash197(Enabled)
	l8r.state = enabled 
	l8r1b.state = enabled
	l8rb.state = enabled
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

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0         ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4       ' used to track the fading state
        FlashSpeedUp(x) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.35 ' slower speed when turning off the flasher
        FlashMax(x) = 1          ' the maximum value when on, usually 1
        FlashMin(x) = 0          ' the minimum value when off, usually 0
        FlashLevel(x) = 0        ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub UpdateLamps
	nFadeLm 1, l1b
	nFadeLm 1, l1
	nFadeLm 1, l1ab
	nFadeL 1, l1a
	NFadeLm 2, l2
	NFadeLm 2, l2a1
	NFadeLm 2, l2a2
	nFadeL 2, l2a
	nFadeLm 3, l3b
	If ReflectLights = 1 then
		nFadeLm 3, l3a
	End If
    nFadeL 3, l3
	'nFadeL 3, l3b1
	nFadeLm 4, l4b
	nFadeL 4, l4
	'nFadeL 4, l4b1
	nFadeLm 5, l5b
	If ReflectLights = 1 then
		nFadeLm 5, l5b1	
	End If
    nFadeL 5, l5
	nFadeLm 6, l6b
	If ReflectLights = 1 then
		nFadeLm 6, L6b1
	End If
	If ReflectLights = 1 then
		nFadeLm 6, L6b2
	End If
    nFadeL 6, l6
	'nFadeL 6, l6b1
	nFadeLm 7, l7b
    nFadeL 7, l7
	nFadeLm 8, l8b
    nFadeL 8, l8
    'nFadeL 9, l9 		'Credit Button
    NFadeLm 10, l10b 	' Mosquito 2 bulbs
	NFadeLm 10, l10 		' Mosquito 2 bulbs
	NFadeLm 10, l10a		' Mosquito 2 bulbs
    FadeDisableLighting 10, mosq, 0.1
	nFadeLm 11, l11b
    nFadeL 11, l11
	nFadeLm 12, l12b
    nFadeL 12, l12
	nFadeLm 13, l13b 
	nFadeL 13, l13
	nFadeLm 14, l14b
    NFadeL 14, l14
	nFadeLm 15, l15b
    nFadeL 15, l15
	nFadeLm 16, l16b
    nFadeL 16, l16
	nFadeLm 17, l17b
	nFadeLm 17, l17
'    Flashm 17, f17a
'	Flash 17, f17 'Left scoop Bottom
	nFadeLm 18, l18b
	nFadeLm 18, l18
'    Flashm 18, f18a
'	Flash 18, f18 'Left scoop top
	nFadeLm 19, l19b
	nFadeLm 19, l19a
	nFadeLm 19, l19ab
    NFadeL 19, l19
	nFadeLm 20, l20b
    nFadeL 20, l20
 
	nFadeLm 21, l21b
    nFadeL 21, l21
	nFadeLm 22, l22b
    nFadeL 22, l22
	nFadeLm 23, l23b
    NFadeL 23, l23
	nFadeLm 24, l24b
    nFadeL 24, l24
	nFadeLm 25, l25b
	nFadeL 25, L25
	nFadeLm 26, l26b
	nFadeL 26, l26
	nFadeLm 27, l27b
	nFadeL 27, l27
	nFadeLm 28, l28a
	nFadeLm 28, l28ab
	nFadeLm 28, l28b
	nFadeL 28, l28
	nFadeLm 29, l29b
	nFadeL 29, l29
	nFadeLm 30, l30b
    nFadeL 30, l30
	nFadeLm 31, l31b
	nFadeLm 31, L31a
	nFadeLm 31, L31a2
    nFadeL 31, l31
   	nFadeLm 32, l32b
	If ReflectLights = 1 then
		nFadeLm 32, l32b1
	End If
	nFadeLm 32, l32a
	nFadeL 32, l32
	NFadeLm 33, l33b
	NFadeLm 33, l33
'	Flashm 33, f33a
'    Flash 33, f33 ' Center scoop bottom
	NFadeLm 34, l34b
	NFadeLm 34, l34
	If ReflectLights = 1 then
		NFadeLm 34, l34b1
	End If
'	Flashm 34, f34a
'    Flash 34, f34 ' Center scoop top
	nFadeLm 35, l35b
	nFadeL 35, l35
	nFadeLm 36, l36b
	If ReflectLights = 1 then
		nFadeLm 36, l36a
	End If
	nFadeL 36, l36
	'FadeDisableLighting 37, Primitive18
	nFadeLm 37, l37
	nFadeLm 37, l37b
	If ReflectLights = 1 then
		nFadeLm 37, l37b1
	End If
	If ReflectLights = 1 then
		nFadeLm 37, l37b2
	End If
	nFadeLm 37, l37bb
	Flash 37, f37
	nFadeLm 38, l38b
	If ReflectLights = 1 then
		nFadeLm 38, l38b1
	End If
	'nFadeLm 38, l38a
	nFadeL 38, l38
	nFadeLm 39, l39b
    nFadeL 39, l39
	nFadeLm 40, l40b
    nFadeL 40, l40
	nFadeLm 41, l41b
    nFadeL 41, l41
	nFadeLm 42, l42b
    nFadeL 42, l42
	nFadeLm 43, l43b
    nFadeL 43, l43
	nFadeLm 44, l44b
	nFadeL 44, l44
	nFadeLm 45, l45b
	nFadeL 45, l45
	'FadeDisableLighting 46, Primitive_JPGate
	NFadeLm 46, l46
	NFadeLm 46, l46a
	Flashm  46, f46a
	Flash 46, f46b
	nFadeLm 47, l47
	nFadeLm 47, l47a1
	nFadeLm 47, l47a2
	nFadeL 47, l47a
    nFadeLm 48, l48
	nFadeLm 48, l48a1
	nFadeLm 48, l48a2
	nFadeL 48, l48a
	nFadeLm 49, L49b
	If ReflectLights = 1 then
		nFadeLm 49, L49b1
	End If
	nFadeL 49, l49
	nFadeLm 50, L50b
	If ReflectLights = 1 then
		nFadeLm 50, L50b1
	End If
	If ReflectLights = 1 then
		nFadeLm 50, L50b2
	End If
	nFadeL 50, l50
	nFadeLm 51, L51b
	nFadeL 51, l51
	nFadeLm 52, L52b
	nFadeL 52, l52
	nFadeLm 53, L53b
	nFadeL 53, l53
	nFadeLm 54, L54b
	nFadeL 54, l54
	nFadeLm 55, L55b
	nFadeLm 55, L55ab
	'nFadeLm 55, L55ab2
	nFadeLm 55, L55a
	nFadeL 55, L55
	nFadeLm 56, l56b
    nFadeL 56, l56
	NFadeLm 57, l57b
    Flashm 57, f57a   
	Flash 57, f57 ' Right Scoop bottom
	NFadeLm 58, l58b
	Flashm 58, f58a   
	Flash 58, f58 ' Right Scoop top
	nFadeLm 59, L59b
    nFadeL 59, l59
    nFadeLm 60, l60
	nFadeLm 60, l60a1
	nFadeLm 60, l60a2
	nFadeL 60, l60a
	nFadeLm 61, L61b
    nFadeL 61, l61
	nFadeLm 62, L62c
	nFadeLm 62, l62b
	If ReflectLights = 1 then
		nFadeLm 62, l62b1
	End If
	nFadeLm 62, L62a
    nFadeL 62, l62
	nFadeLm 63, L63b
    NFadeL 63, l63
	nFadeLm 64, l64b
    nFadeL 64, l64
    FadePrim 140, TrexMain, "Trex-Main-Map", "Trex-Main-Map-ON1", "Trex-Main-Map-ON1", "Trex-Main-Map-ON2"
	FadePrim 141, TrexMain, "Trex-Main-Map", "Trex-Main-Map-ON1", "Trex-Main-Map-ON1", "Trex-Main-Map-ON2"
	FadePrim 142, TrexJaw, "Trex-Main-Map", "Trex-Main-Map-ON1", "Trex-Main-Map-ON1", "Trex-Main-Map-ON2"
    FadeDisableLighting 140, TrexMain, 0.3
	FadeDisableLighting 141, TrexMain, 0.3
	FadeDisableLighting 142, TrexJaw, 0.3
	If GIShadows = 1 Then
		Flash 143, Flasher_PFShadow
		Flash 144, Flasher_PFShadow1
		Flash 145, Flasher_Plasticshadow
		Flash 146, Flasher_Plasticshadow1
	End If
	If FlasherShadows = 1 Then
		Flash 147, Shadow_LN_Flasher_On	
		Flash 148, Shadow_RW_PF_Flasher
		Flash 149, Shadow_LN_Flasher_Off
		Flash 150, Shadow_RW_PL_Flasher
		Flash 151, Shadow_LW_PF_Flasher
		Flash 152, Shadow_LW_PL_Flasher
		Flash 153, Shadow_RR_PF_Flasher
		Flash 154, Shadow_RR_PL_Flasher
		Flash 155, Shadow_LR_PF_Flasher
		Flash 156, Shadow_LR_PL_Flasher
		'Flash 157, Shadow_RW_PF_Flasher1
	End If
End Sub

Sub FadePrim(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:FadingLevel(nr) = 0
        Case 3:pri.image = c:FadingLevel(nr) = 2
        Case 4:pri.image = b:FadingLevel(nr) = 3
        Case 5:pri.image = a:FadingLevel(nr) = 1
    End Select
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
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1            'wait
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

Sub FadePrim(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:FadingLevel(nr) = 0
        Case 3:pri.image = c:FadingLevel(nr) = 2
        Case 4:pri.image = b:FadingLevel(nr) = 3
        Case 5:pri.image = a:FadingLevel(nr) = 1
    End Select
End Sub

Sub FadeMaterial(nr, a, mon, moff)
    Select Case FadingLevel(nr)
        Case 4:a.material = moff
        Case 5:a.material = mon
    End Select
End Sub

Sub FadeDisableLighting(nr, a, alvl)
	Select Case FadingLevel(nr)
		Case 4
			a.UserValue = a.UserValue - 0.1
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
' *********************************************************************
'                      Fluppers Light Routines
' *********************************************************************

Sub ChangeGlow(day)
	Dim Light
	If day Then
		For Each Light in GlowLights : Light.IntensityScale = GlowAmountDay: Light.FadeSpeedUp = Light.Intensity * GlowAmountDay / 2: Light.FadeSpeedDown = Light.FadeSpeedUp / 5: Next 'fadespeeddown = fadespeedup / 25
		For Each Light in InsertLights : Light.IntensityScale = InsertBrightnessDay : Light.FadeSpeedUp = Light.Intensity * InsertBrightnessDay / 2 : Light.FadeSpeedDown = Light.FadeSpeedUp / 5 : Next 'fadespeeddown = fadespeedup / 25
	Else
		For Each Light in GlowLights : Light.IntensityScale = GlowAmountNight: Light.FadeSpeedUp = Light.Intensity * GlowAmountNight / 2: Light.FadeSpeedDown = Light.FadeSpeedUp / 5: Next 'fadespeeddown = fadespeedup / 25
		For Each Light in InsertLights : Light.IntensityScale = InsertBrightnessNight : Light.FadeSpeedUp = Light.Intensity * InsertBrightnessNight / 2 : Light.FadeSpeedDown = Light.FadeSpeedUp / 5 : Next 'fadespeeddown = fadespeedup / 25
	End If
End Sub

Sub ColorGrade()
	Dim lutlevel, ContrastLut
	Lutlevel = 0
	If ContrastSetting=0 or ContrastSetting=2 or ContrastSetting=4 or ContrastSetting=6 Then
		ChangeGlow(True)
		ContrastLut = ContrastSetting / 2
	Else
		ChangeGlow(False)
		ContrastLut = ContrastSetting / 2 - 0.5
	End If
	table1.ColorGradeImage = "LUT" & ContrastLut & "_" & lutlevel
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

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 8 ' total number of balls
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
        if BOT(b).z <130 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
 ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z <155 and BOT(b).z >127 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If   


		' *********	Upper Right Kicker Code
		If InRect(BOT(b).x,BOT(b).y,824,72,874,72,874,122,824,122) Then
			If ABS(BOT(b).velx) < 0.05 and ABS(BOT(b).vely) < 0.05 Then
				sw56.enabled = True
			End If
		End If
		' *********	End Upper Right Kicker Code

		' *********	TREX Saucer Code
		If InRect(BOT(b).x,BOT(b).y,360,540,410,540,410,600,360,600) Then
			If ABS(BOT(b).velx) < 0.05 and ABS(BOT(b).vely) < 0.05 Then
				sw55.enabled = True
			End If
		End If
		' *********	End TREX Saucer Code
    Next

End Sub


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub
'*********** BALL SHADOW and GLOW BALL *********************************
Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6, BallShadow7, BallShadow8)
Const anglecompensate = 15

Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls

	IF GlowBall Then
		For b = UBound(BOT) to 9
			If GlowBall and Glowing(b).state = 1 Then Glowing(b).state = 0 End If
		Next
	End If

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
        If BOT(b).Z > 100 and BOT(b).Z < 220 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
		If GlowBall and b <10 Then
			If Glowing(b).state = 0 Then Glowing(b).state = 1 end if
			Glowing(b).BulbHaloHeight = BOT(b).z + 26
			Glowing(b).x = BOT(b).x : Glowing(b).y = BOT(b).y + anglecompensate
		End If

    Next
End Sub

'*** change ball appearance ***

Sub ChangeBall(ballnr)
	Dim BOT, ii, col
	Table1.BallDecalMode = CustomBallLogoMode(ballnr)
	Table1.BallFrontDecal = CustomBallDecal(ballnr)
	Table1.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
	Table1.BallImage = CustomBallImage(ballnr)
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 9
		col = RGB(red(ballnr), green(ballnr), Blue(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
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


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Woods_Hit (idx)
	PlaySound "woodhit", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub


'Sub Gates_Hit (idx)
	'PlaySound "gate", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : 	PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : 	PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : 	PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub Table1_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub













