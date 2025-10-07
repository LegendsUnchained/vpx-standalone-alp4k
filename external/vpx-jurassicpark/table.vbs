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

Const UseVpmModSol = True
'********************
'Options
'********************
Dim VolumeDial, CollectionVolume
VolumeDial = 0.5				'Added Sound Volume Dial (ramps, balldrop, kickers, etc)
CollectionVolume = 20 			'Standard Sound Amplifier (targets, gates, rubbers, metals, etc) use 1 for standard setup

'********************
'End Options
'********************

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01120100", "DE.VBS", 3.36

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

	' Trough
 	Set bsTrough = New cvpmBallStack : With bsTrough
 		.Initsw 0,14,13,12,11,10,9,0
 		.CreateEvents "bsTrough", Drain
 		.IsTrough = True
 		.Balls = 6
 	End With

	' Trough Ejector
  	Set bsTroughE = New cvpmBallStack : With bsTroughE
 		.Initsw 0,15,0,0,0,0,0,0
 		.InitKick BallRelease,150,7
  	End With

	'Turn GI ON
 	GIRelay 0

	Init_Trex

	'*****************  Captive Ball  ***********************
	Set CaptiveBall = Captive.Createball
	Captive.kick 180,1
	Captive.enabled = false

	If Table1.ShowDT = False then
       leftrail.visible = 0
       rightrail.visible = 0
       sidewalls.visible = 0
       Ramp17.visible = 0
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
'SolCallback(8)    	= "vpmSolSound SfxKnocker,"		' Knocker
SolCallback(9)    	= "RaptorKick"					' Raptor Pit
'SolCallback(10)   	= ""							' L/R Relay (internal solenoid select for same numbered functions)
SolCallback(11)   	= "GIRelay"						' GI
SolCallback(12)   	= "RexLeftRight"				' TRex Left/Right Select
SolCallback(13)   	= "RexMouth"					' TRex Mouth
SolCallback(14)   	= "RexUpDown"					' TRex Up/Down Motor
SolCallback(15)   	= "RexMotor"					' TRex Left/Right Motor On/Off
SolCallback(16)   	= "TroughLockout"				' Trough Lockout
'SolCallback(17)		= "TJet"						' Top   Jet
'SolCallback(18)		= "LJet"						' Left  Jet
'SolCallback(19)		= "RJet"						' Right Jet
'SolCallback(20)		= "vpmSolSound SfxSling,"		' Left  Slingshot
'SolCallback(21)		= "vpmSolSound SfxSling,"		' Right Slingshot
'SolCallback(22)   	= "ShakerMotor"					' Cabinet Shaker
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

Sub TroughRelease(enabled) : if enabled then : bstroughE.solout 1 : end if : End Sub
 Sub TroughLockout(enabled) : if enabled and bstrough.balls > 0 then : bstrough.balls = bstrough.balls - 1 : bstroughE.addball 1 : end if : End Sub


'******************************************************
'					Left Scoop
'******************************************************

Sub LeftScoopEject(enabled)
 	If enabled Then
		Playsound SoundFX(SSolenoidOn,DOFContactors), 0, 2*VolumeDial
		sw35.kick 0,30,1.5
		sw35wall.collidable = False
		Controller.Switch(35) = False
 	End If
End Sub

sub sw35_hit
	sw35wall.collidable = True
	Controller.Switch(35) = True
End sub

sub Trigger2_Unhit
	sw35wall.collidable = False
	Playsound "Hit3", 0, 2*VolumeDial
End sub
'******************************************************
'					Middle Scoop Trough
'******************************************************

sub sw37_hit
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
			PlaySound "wireramp_right", 0, 0.3*volumedial, Csng((900 * 2 / table1.width-1) ^10), 0, 100, 1, 0
		Else
			Playsound SoundFX(SSolenoidOn,DOFContactors), 0, 2*VolumeDial
		End If
		vuk.kick 90,60,1.25
		vukwall.collidable = False
		Controller.Switch(61) = False
 	End If
End Sub

sub vuk_hit
	vukwall.collidable = True
	Controller.Switch(61) = True
End sub

'******************************************************
'					Boat Dock Saucer
'******************************************************

Sub BoatDockSaucer(enabled)
	If enabled Then
		Playsound SoundFX(SSolenoidOn,DOFContactors), 0, 2*VolumeDial
		sw56.Kick 160,10
        Controller.Switch(56) = False
		KickerArm2.TransY = 10
		vpmTimer.AddTimer 150, "KickerArm2.TransY = 0'"
	End If
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
		sw29.Kick 180, 41
		controller.switch (29) = false
		Primitive16.transZ = 0
	End If
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
   If Enabled Then
        SetLamp 31, 1 = abs(enabled)
        SetLamp 131, 1 = abs(enabled)
     else
        SetLamp 31, 0
        SetLamp 131, 0
   End If
 End Sub

 Sub RJet(enabled)
   If Enabled Then
        SetLamp 32, 1 = abs(enabled)
        SetLamp 132, 1 = abs(enabled)
     else
        SetLamp 32, 0
        SetLamp 132, 0
   End If
 End Sub

 Sub LJet(enabled)
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
Table1.colorgradeimage = "ColorGrade_4"
		Playsound "fx_relay_off", 0, 2*VolumeDial
		setlamp 140, 1
		setlamp 141, 1
		setlamp 142, 1
        dino.blenddisablelighting = 0.1
		dino1.blenddisablelighting = 0.1
	Else
		GiON
Table1.colorgradeimage = "ColorGradeLUT256x16_ConSat"
		Playsound "fx_relay_on", 0, 2*VolumeDial
		setlamp 140, 0
		setlamp 141, 0
		setlamp 142, 0
	    dino.blenddisablelighting = 0.3
		dino1.blenddisablelighting = 0.3
End If
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
If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then   'Launch Trigger
		controller.switch(41) = False
	End If

	If keycode = RightMagnaSave or keycode = LeftMagnaSave Then  'Smart Bomb Button
		controller.switch(42) = False
	End If
If vpmKeyUp(keycode) Then Exit Sub
End Sub

'******************************************************
'				SWITCHES
'******************************************************

'Rollover and Optos


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
		Playsound SoundFX(SSolenoidOn,DOFContactors), 0, 2*VolumeDial
		sw55.Kick 170,15
        Controller.Switch(55) = False
        TrexBall = Empty
		KickerArm1.TransY = 10
		vpmTimer.AddTimer 150, "KickerArm1.TransY = 0'"
	End If
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
trcenterangle = -14				'Angle when centered
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

If RexBendRot < trforwardangle + 1 Then RexBendRot = RexBendRot + RexBendDir/10 Else RexBendRot = RexBendRot + RexBendDir End If

		TrexBend()

		If ballinmouth = 1 then
			trexballanglez = trexballanglez + RexBendDir
			trexball.x = trexmain.x + trexballradius*cos(radians(trexballanglexy))*Sin(Radians(trexballanglez))
			trexball.y = trexmain.y + trexballradius*sin(radians(trexballanglexy))*Sin(Radians(trexballanglez))
			trexball.z = trexmain.z - trexballradius*cos(radians(trexballanglez))
		End If

		If RexBendRot <= trforwardangle then
			RexBendRot = trforwardangle
			RexBendDir = -RexBendDir
			if  Controller.Switch(55) = True then
				Playsound "1-Pickup"
				ballinmouth = 1
				trexballradius = Distance(trexball.x,trexball.y,trexball.z,trexmain.x,trexmain.y,trexmain.z)
				trexballanglez = Degrees(acos((trexball.z-trexmainz)/trexballradius))
				trexballanglexy = Degrees(acos((trexball.x-trexmainx)/trexballradius))
			end if
		end if
		If RexBendRot <= trforwardangle+1 Then
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
             	Controller.Switch(55) = False
				ballinmouth = 0
                TrexBall = Empty
                UpDownTimer.Enabled = 0
			end if
		Else
			Controller.Switch(57) = False
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
	MotorTimer.enabled = ABS(enabled)
    else
    MotorTimer.enabled = ABS(enabled)
end if
End Sub

'TRex Timers
Dim RexRot: RexRot = trcenterangle

Sub MotorTimer_Timer()
	if Controller.Switch(57) = True then ' if trex is up
		RexRot = RexRot + goleft*RexRotSpeed

		TrexMain.ObjRotZ = RexRot
		TrexJaw.ObjRotZ = RexRot
		TrexPlastic.ObjRotZ = RexRot
        TrexShadow.objrotz=RexRot
	    Trexplate.objrotz=RexRot
	    TrexPlasticRivets.objrotz=RexRot

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
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
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
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

'Flashers
 SolModCallBack(25) = "Flash190" 		'1R Top middle Flashers
 SolModCallBack(26) = "Flash191"		'2R Middle Right Flasher
 SolModCallBack(27) = "Flash192"		'3R
 SolModCallBack(28) = "Flash193"		'4R Right Flasher
 SolModCallBack(29) = "Flash194" 		'5R Left Flasher
 SolModCallBack(30) = "Flash195" 		'6R Middle Left Flasher
 SolModCallBack(31) = "Flash196"		 	'7R
 SolModCallBack(32) = "Flash197"		 	'8R


Sub Flash190(m)
m=m/64
	l1r.state = m
end Sub


Sub Flash191(m)
m=m/64
	L2r1.state = m
	l2ra.state = m
	l2rb.state = m
	Flasherdome1.blenddisablelighting = m+0.2
    fwl.state=m
end Sub

Sub Flash192(m)
m=m/64
frl.state=m
	l3r.state = m
	l3ra.state = m
	l3rb.state = m
	l3rc.state = m
	l3rd.state = m
	Flasherdome3.blenddisablelighting = m+0.2
end Sub

Sub Flash193(m)
m=m/64
frr.state=m
    l4r.state = m
	l4ra.state = m
	l4rb.state = m
	l4rc.state = m
	Flasherdome4.blenddisablelighting = m+0.2
end Sub


Sub Flash194(m)
m=m/64
	l5r.state = m
	l5ra.state = m
	l5rb.state = m
End Sub

Sub Flash195(m)
m=m/64
	l6r.state = m
	l6ra.state = m
	l6rb.state = m
	Flasherdome2.blenddisablelighting = m+0.2
    fwr.state=m
End Sub

Sub Flash196(m)
m=m/64
	l7ra.state = m
	l7rb.state = m
	l7rc.state = m
	l7rd.state = m
End Sub

Sub Flash197(m)
m=m/64
	l8r.state = m
	l8rb.state = m
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
	nFadeLm 1, l1
	nFadeL 1, l1a
	NFadeLm 2, l2
	NFadeLm 2, l2a1
	NFadeLm 2, l2a2
	nFadeL 2, l2a
    nFadeL 3, l3
	nFadeL 4, l4
    nFadeL 5, l5
    nFadeL 6, l6
    nFadeL 7, l7
    nFadeL 8, l8
    'nFadeL 9, l9 		'Credit Button
	NFadeLm 10, l10 		' Mosquito 2 bulbs
	NFadeLm 10, l10a		' Mosquito 2 bulbs
    FadeDisableLighting 10, mosq, 0.1
    nFadeL 11, l11
    nFadeL 12, l12
	nFadeL 13, l13
    NFadeL 14, l14
    nFadeL 15, l15
    nFadeL 16, l16
	nFadeLm 17, l17b
	nFadeLm 17, l17
'    Flashm 17, f17a
'	Flash 17, f17 'Left scoop Bottom
	nFadeLm 18, l18b
	nFadeLm 18, l18
'    Flashm 18, f18a
'	Flash 18, f18 'Left scoop top
	nFadeLm 19, l19a
    NFadeL 19, l19
    nFadeL 20, l20
    nFadeL 21, l21
    nFadeL 22, l22
    NFadeL 23, l23
    nFadeL 24, l24
	nFadeL 25, L25
	nFadeL 26, l26
	nFadeL 27, l27
	nFadeLm 28, l28a
	nFadeL 28, l28
	nFadeL 29, l29
    nFadeL 30, l30
	nFadeLm 31, l31b
	nFadeLm 31, L31a
	nFadeLm 31, L31a2
    nFadeL 31, l31
   	nFadeLm 32, l32b
	nFadeLm 32, l32a
	nFadeL 32, l32
	NFadeLm 33, l33b
	NFadeLm 33, l33
'	Flashm 33, f33a
'    Flash 33, f33 ' Center scoop bottom
	NFadeLm 34, l34b
	NFadeLm 34, l34
'	Flashm 34, f34a
'    Flash 34, f34 ' Center scoop top
	nFadeL 35, l35
	nFadeL 36, l36
	nFadeLm 37, l37
	nFadeLm 37, l37bb
	Flash 37, f37
	nFadeL 38, l38
    nFadeL 39, l39
    nFadeL 40, l40
    nFadeL 41, l41
    nFadeL 42, l42
    nFadeL 43, l43
	nFadeL 44, l44
	nFadeL 45, l45
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
	nFadeL 49, l49
	nFadeL 50, l50
	nFadeL 51, l51
	nFadeL 52, l52
	nFadeL 53, l53
	nFadeL 54, l54
	nFadeLm 55, L55a
	nFadeL 55, L55
    nFadeL 56, l56
	NFadeLm 57, l57b
    Flashm 57, f57a
	Flash 57, f57 ' Right Scoop bottom
	Flashm 58, f58a
	Flash 58, f58 ' Right Scoop top
    nFadeL 59, l59
    nFadeLm 60, l60
	nFadeLm 60, l60a1
	nFadeLm 60, l60a2
	nFadeL 60, l60a
    nFadeL 61, l61
	nFadeLm 62, l62b
	nFadeLm 62, L62a
    nFadeL 62, l62
    NFadeL 63, l63
    nFadeL 64, l64
    FadePrim 140, TrexMain, "Trex-Main-Map", "Trex-Main-Map-ON1", "Trex-Main-Map-ON1", "Trex-Main-Map-ON2"
	FadePrim 141, TrexMain, "Trex-Main-Map", "Trex-Main-Map-ON1", "Trex-Main-Map-ON1", "Trex-Main-Map-ON2"
	FadePrim 142, TrexJaw, "Trex-Main-Map", "Trex-Main-Map-ON1", "Trex-Main-Map-ON1", "Trex-Main-Map-ON2"
    FadeDisableLighting 140, TrexMain, 0.3
	FadeDisableLighting 141, TrexMain, 0.3
	FadeDisableLighting 142, TrexJaw, 0.3
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
        if BOT(b).z <30 Then ' Ball on playfield
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
        If BOT(b).VelZ < -1 and BOT(b).z <55 and BOT(b).z >27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
Next
End Sub



'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub
'*********** BALL SHADOW *********************************
Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6, BallShadow7, BallShadow8, BallShadow9)
Const anglecompensate = 15

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
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 80
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next
End Sub



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

Sub Table1_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub
