'*************************************
'Tee'd Off Premier (Gottlieb) 1993 - IPDB No. 2508
'VPX by rothbauerw
'Script by rothbauerw/jpsalas (VP9)
'Primitives by Dark
'************************************
 
Option Explicit
Randomize

'********************
'Options
'********************

Dim VolumeDial, CollectionVolume, VolcanoLighting, FlipperTricks
Dim FastFlips, enableFastFlips, enableDoubleLeaf

VolcanoLighting = 1			'"0" Turns off texture swaps for the volcano (will help performance on low end machines)
VolumeDial = 0.5			'Added Sound Volume Dial (ramps, balldrop, kickers, etc)
CollectionVolume = 20 		'Standard Sound Amplifier (targets, gates, rubbers, metals, etc) use 1 for standard setup
FlipperTricks = 0    		'NFozzy/JF flipper script 1 - enabled; 0 - disabled
enableFastFlips = 1			'NFozzy's fastflips 1 - enabled; 0 - disabled
enableDoubleLeaf = 0		'Top flipper controlled by double leaf switch 1- enabled; 0 - disabled (Only works with FastFlips)

'********************
'End Options
'********************

Dim Ballsize,BallMass
BallSize = 50
BallMass = 1

Dim DesktopMode, NightDay:DesktopMode = Table1.ShowDT:NightDay = Table1.NightDay
Dim UseVPMDMD:UseVPMDMD = DesktopMode

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01560000", "GTS3.VBS", 3.26

'********************
'Standard definitions
'********************
   
Const UseSolenoids = 2
Const UseLamps = 1
Const UseSync = 0
 
'Standard Sounds
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SCoin = "fx_coin"

'******************************************************
' 					TABLE INIT
'******************************************************

Dim dtDrop, ii
Dim ttGopherWheel
Const cGameName = "teedoff3"

 Sub Table1_Init
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
 		.SplashInfoLine = "Tee'd Off (Gottlieb 1993)"
		.Games(cGameName).Settings.Value("rol") = 0 'rotated
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = 0
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
     End With
 
    '************  Main Timer init  ********************

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

    '************  Nudging   **************************

	vpmNudge.TiltSwitch = 151
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(Bumper1, LeftSlingShot, RightSlingShot, TopSlingShot)
 
    '************  Trough	**************************
	Slot1.CreateSizedballWithMass Ballsize/2,Ballmass
	Slot2.CreateSizedballWithMass Ballsize/2,Ballmass
	Slot3.CreateSizedballWithMass Ballsize/2,Ballmass
	Drain.CreateSizedballWithMass Ballsize/2,Ballmass
	Controller.Switch(14) = 1
	Controller.Switch(24) = 1

    '************  Droptarget
	Set dtDrop = new cvpmDropTarget
	With dtDrop
		.InitDrop Array(dt1, dt4, dt2, dt3), Array(7,17,27,37)
		.Initsnd  SoundFX("droptarget",DOFContactors),  SoundFX("resetdrop",DOFContactors)
	End With
	
	Set ttGopherWheel = New cvpmTurntable
		ttGopherWheel.InitTurntable ttGopherWheelTrig, 30 '20
		ttGopherWheel.spinup=100
		ttGopherWheel.spindown=100
		ttGopherWheel.CreateEvents "ttGopherWheel"

	'************  Misc Stuff  ******************
	arm3.isdropped=1
	arm4.isdropped=1
	arm5.isdropped=1
	F13c.intensityscale=0:F13d.intensityscale=0:F13e.intensityscale=0
	Init_Roulette

	If DesktopMode Then
		For each xx in GI_Upper:xx.y=xx.y+12:Next
		For each xx in LED:xx.y=xx.y+10:Next
		For each xx in Flashers_Upper:xx.y=xx.y+12:Next
	Else
		leftrail.visible=0
		rightrail.visible=0
	End If

	If enableFastFlips = 1 Then
		Set FastFlips = new cFastFlips
		with FastFlips
			.CallBackL = "SolLflipper"	'Point these to flipper subs
			.CallBackR = "SolRflipper"	'...
		'	.CallBackUL = "SolULflipper"'...(upper flippers, if needed)
			.CallBackUR = "SolURflipper"
			.TiltObjects = True 'Optional, if True calls vpmnudge.solgameon automatically. IF YOU GET A LINE 1 ERROR, DISABLE THIS! (or setup vpmNudge.TiltObj!)
		'	.InitDelay "FastFlips", 100			'Optional, if > 0 adds some compensation for solenoid jitter (occasional problem on Bram Stoker's Dracula)
		'	.DebugOn = False		'Debug, always-on flippers. Call FastFlips.DebugOn True or False in debugger to enable/disable.
		end with
	End If

	'************  Adjust GI based on NightDay  ******************
	Dim xx

	If NightDay <= 75 And NightDay > 50 Then
		For each xx in GI:xx.Intensity = xx.intensity*1.1:Next
		For each xx in GI_Upper:xx.intensity=xx.intensity*1.1:Next
		For each xx in GWLights:xx.intensity=xx.intensity*1.1:Next
		'For each xx in insertlights:xx.intensity=xx.intensity*0.5:Next
		'For each xx in LED:xx.intensity=xx.intensity*1.1:Next
		'RoomLight.state = 1
		'RoomLight.Intensity = 0.25
		Primitive_PlasticRamp.material="Plastic Ramp(Red)1"
	ElseIf NightDay <= 50 And NightDay > 25 Then
		For each xx in GI:xx.Intensity = xx.intensity*1.3:Next
		For each xx in GI_Upper:xx.intensity=xx.intensity*1.3:Next
		For each xx in GWLights:xx.intensity=xx.intensity*1.3:Next
		'For each xx in insertlights:xx.intensity=xx.intensity*0.35:Next
		'For each xx in LED:xx.intensity=xx.intensity*1.3:Next
		'RoomLight.state = 1
		'RoomLight.Intensity = 0.5		
		Primitive_PlasticRamp.material="Plastic Ramp(Red)2"
	ElseIf NightDay <= 25 And NightDay > 5 Then
		For each xx in GI:xx.Intensity = xx.intensity*1.5:Next
		For each xx in GI_Upper:xx.intensity=xx.intensity*1.5:Next
		For each xx in GWLights:xx.intensity=xx.intensity*1.5:Next
		'For each xx in LED:xx.intensity=xx.intensity*1.5:Next
		'For each xx in insertlights:xx.intensity=xx.intensity*0.25:Next
		'RoomLight.state = 1
		'RoomLight.Intensity = 0.5
		Primitive_PlasticRamp.material="Plastic Ramp(Red)3"
		Bulb_Glass.material="Lamps Glass 4"
	ElseIf NightDay <= 5 Then
		For each xx in GI:xx.Intensity = xx.intensity*1.7:Next
		For each xx in GI_Upper:xx.intensity=xx.intensity*1.7:Next
		For each xx in GWLights:xx.intensity=xx.intensity*1.7:Next
		'For each xx in insertlights:xx.intensity=xx.intensity*0.15:Next
		'For each xx in LED:xx.intensity=xx.intensity*1.7:Next
		'RoomLight.state = 1
		'RoomLight.Intensity = 0.5
		Primitive_PlasticRamp.material="Plastic Ramp(Red)4"
		Bulb_Glass.material="Lamps Glass 4"
	End If

End Sub


'******************************************************
' 						KEYS
'******************************************************

Sub Table1_KeyDown(ByVal keycode)
	If keycode = StartGameKey Then Controller.Switch(4)= 1

	If enableFastFlips = 1 Then
		If KeyCode = LeftFlipperKey then FastFlips.FlipL True
		If KeyCode = RightFlipperKey then FastFlips.FlipR True
		If enableDoubleLeaf = 1 Then
			If KeyCode = KeyUpperRight then FastFlips.FlipUR True 
		End If
	End If

	If keycode = LeftTiltKey Then Nudge 90, 3:PlaySound SoundFX("nudge_left", 0)
    If keycode = RightTiltKey Then Nudge 270, 3:PlaySound SoundFX("nudge_right", 0)
    If keycode = CenterTiltKey Then Nudge 0, 3:PlaySound SoundFX("nudge_forward", 0)
 	If keycode = Plungerkey then plunger.PullBack:PlaySound "plungerpull", 0, 2*VolumeDial
	If vpmKeyDown(keycode) Then Exit Sub

'	if keycode=30 then controller.switch(5)=Not Controller.Switch(5) 'tournament switch "A"
	if keycode=31 then controller.switch(6)=Not Controller.Switch(6) 'door switch "S"
	if keycode=33 then EngageRoulette true  ' F test roulette wheel

'	if keycode=34 then		' G test TiltRelay
'		TiltRelay True
'		Flasher11 True:Flasher12 True:Flasher13 True:Flasher14 True:Flasher15 True:Flasher16 True:Flasher17 True:Flasher18 True:Flasher19 True
'	End If  

	'* Test Kicker
	'If keycode = 37 Then TestKick ' K
	If keycode = 205 Then TKickAngle = TKickAngle + 3:KickDirection.Visible=1:KickDirection.RotZ=TKickAngle+90 ' right arrow
	If keycode = 203 Then TKickAngle = TKickAngle - 3:KickDirection.Visible=1:KickDirection.RotZ=TKickAngle+90 'left arrow
	If keycode = 200 Then TKickPower = TKickPower + 2 ' up arrow
	If keycode = 208 Then TKickPower = TKickPower - 2 ' down arrow
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = StartGameKey Then Controller.Switch(4) = 0

	If enableFastFlips = 1 Then
		If KeyCode = LeftFlipperKey then FastFlips.FlipL False 
		If KeyCode = RightFlipperKey then FastFlips.FlipR False
		If enableDoubleLeaf = 1 Then
			If KeyCode = KeyUpperRight then FastFlips.FlipUR False
		End If
	End If

	If keycode = plungerkey then plunger.Fire:PlaySound "plunger", 0, 2*VolumeDial
	If vpmKeyUp(keycode) Then Exit Sub

'	if keycode=34 then 	' G test TiltRelay
'		TiltRelay False
'		Flasher11 False:Flasher12 False:Flasher13 False:Flasher14 False:Flasher15 False:Flasher16 False:Flasher17 False:Flasher18 False:Flasher19 False
'	End If  

	If keycode = 205 Then KickDirection.Visible=0 ' right arrow
	If keycode = 203 Then KickDirection.Visible=0 'left arrow


End Sub

Sub Table1_Paused: Controller.Pause = 1:End Sub
Sub Table1_UnPaused: Controller.Pause = 0:End Sub
Sub Table1_Exit(): Controller.Stop:End Sub

'******************************************************
'					Test Kicker
'******************************************************

Dim TKickAngle, TKickPower, TKickBall
TKickAngle = 0
TKickPower = 10

Sub testkick()
	test.kick TKickAngle,TKickPower
	test.timerenabled = 1
End Sub

Sub test_hit():Set TKickBall=ActiveBall:End Sub
Sub test_timer():TKickBall.velx=0:TKickBall.vely=0:TKickBall.x=test.x:TKickBall.y=test.y-50:test.timerenabled=0:End Sub

'******************************************************
'					SOLENOIDS
'******************************************************

'SolCallback(1) = "" 				           		'1 - Pop Bumper
'SolCallback(2) = ""                           		'2 - Left Sling
'SolCallback(3) = ""                           		'3 - Right Sling
'SolCallback(4) = ""								'4 - Top Kicking Rubber
SolCallback(5) = "MHGKicker"						'5 - Tope Hole
SolCallback(6) = "PlungerGate"						'6 - Plunger Gate
SolCallback(7) = "dtDrop.SolDropUp"					'7 - 5 Bank Drop Target
SolCallback(8) = "TopUpKicker"						'8 - Top Up Kicker
SolCallback(9) = "CenterUpKicker"					'9 - Center Up Kicker	
SolCallback(10) = "RightUpKicker"					'10 - Bottom Up Kicker
SolCallback(11) = "Flasher11"						'11 - Left Flipper Flash
SolCallback(12) = "Flasher12"						'12	- Left Center Flash
SolCallback(13) = "Flasher13"						'13 - Top Left Flash
SolCallback(14) = "Flasher14"						'14 - Center Middle Flash
SolCallback(15) = "Flasher15"						'15 - Center Top Flash 	
SolCallback(16) = "Flasher16"						'16 - Top Right Flash
SolCallback(17) = "Flasher17"						'17 - Top Right Flash #2
SolCallback(18) = "Flasher18"						'18 - Right Center Flash
SolCallback(19) = "Flasher19"						'19 - Right Flipper Flash
'SolCallback(20) = ""								'20 - Light Box #1 Flash
'SolCallback(21) = ""								'21 - Light Box #2 Flash
'SolCallback(22) = ""								'22 - Light Box #3 Flash
'SolCallback(23) = ""								'23 - Light Box #4 Flash
'SolCallback(24) = ""								'24 - Light Box Gopher Motor Relay
SolCallback(25) = "EngageRoulette"					'25 - Playfield Gopher Wheel Relay
SolCallback(26) = "GIRelay"							'26 - LightBox GI Relay
SolCallback(28) = "ReleaseBall"						'28 - Ball Release
SolCallback(29) = "SolOuthole"						'29 - Outhole
SolCallback(30) = "vpmSolSound ""knocker"","		'30 - Knocker
SolCallback(31) = "TiltRelay"						'31 - Tilt
If enableFastFlips = 1 Then
	SolCallBack(32) = "FastFlips.TiltSol"
Else
	SolCallback(32) = "vpmNudge.SolGameOn"				'32 - Game Over Relay
	SolCallback(sLRFlipper) = "SolRFlipper"
	SolCallback(sLLFlipper) = "SolLFlipper"
End If



'******************************************************
'			TROUGH BASED ON NFOZZY'S
'******************************************************

Sub Slot3_Hit():Controller.Switch(14) = 1:UpdateTrough:End Sub
Sub Slot3_UnHit():Controller.Switch(14) = 0:UpdateTrough:End Sub
Sub Slot2_UnHit():UpdateTrough:End Sub
Sub Slot1_UnHit():UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If Slot1.BallCntOver = 0 Then Slot2.kick 60, 9
	If Slot2.BallCntOver = 0 Then Slot3.kick 60, 9
	Me.Enabled = 0
End Sub

'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub Drain_Hit()
	PlaySound "fx_drain", 0 , 0.25 * volumedial
	UpdateTrough
	Controller.Switch(24) = 1
End Sub

Sub Drain_UnHit()
	Controller.Switch(24) = 0
End Sub

Sub SolOuthole(enabled)
	If enabled Then 
		Drain.kick 60,20
		PlaySound SoundFX(SSolenoidOn,DOFContactors), 0, 2*VolumeDial
	End If
End Sub

Sub ReleaseBall(enabled)
	If enabled Then 
		PlaySound SoundFX("fx_ballrel",DOFContactors), 0, 0.25 * volumedial
		Slot1.kick 60, 7
		UpdateTrough
	End If
End Sub

'******************************************************
'				RIGHT UP KICKER (RUK)
'******************************************************

Sub RightUpKicker(enabled)
	If enabled Then
		RUKa.kick 0,40,1.56
		Controller.Switch(36) = 0
		Playsound SoundFX("scoopexit",DOFContactors), 0, 2*VolumeDial
		arm4.isdropped=0
		vpmTimer.AddTimer 150, "arm4.isdropped=1'"
	End If
End Sub

sub RUKa_hit
	PlaySound "kicker_enter_center", 0, 2*VolumeDial
	Controller.Switch(36) = 1
End sub


'******************************************************
'				Mean Hole Green (MHG)
'******************************************************

Sub MHGKicker(enabled)
	If enabled Then
		MHG.kickZ 145,25,0,5
		Controller.Switch(23) = 0
		Playsound SoundFX("solenoid",DOFContactors), 0, 2*VolumeDial
		twkicker1.Z = -12
		MHG.enabled = false
		vpmTimer.AddTimer 150, "twkicker1.Z = -22'"
	End If
End Sub

Sub MHG_hit()
	PlaySound "metalhit2", 0, 2*VolumeDial
	Controller.Switch(23) = 1
End sub


'******************************************************
'				Center Up Kicker (CUK)
'******************************************************

Sub CenterUpKicker(enabled)
	If enabled Then
		CUK.kick 0,35,1.56
		Controller.Switch(26) = 0
		Playsound SoundFX("solenoid",DOFContactors), 0, 2*VolumeDial
		arm5.isdropped=0
		CUK.enabled = false
		vpmTimer.AddTimer 150, "arm5.isdropped=1'"
	End If
End Sub

Sub CUK_hit()
	PlaySound "kicker_enter_center", 0, 2*VolumeDial
	Controller.Switch(26) = 1
End sub

'******************************************************
'				TOP UP KICKER (TUK)
'******************************************************

Sub TopUpKicker(enabled)
	If enabled Then
		TUKa.kick 0,60,1.56
		Controller.Switch(16) = 0
		Playsound SoundFX("vuk_exit",DOFContactors), 0, 2*VolumeDial
		arm3.isdropped=0
		vpmTimer.AddTimer 150, "arm3.isdropped=1'"
	End If
End Sub

sub TUKa_hit
	PlaySound "kicker_enter_center", 0, 2*VolumeDial
	Controller.Switch(16) = 1
End sub

'******************************************************
'					Plunger Gate
'******************************************************


Sub PlungerGate(enabled)
	If enabled Then
		BallLockPrim.Z = 26
		BallLockWall.collidable = 0
		Rubber13.collidable = 0 
		Playsound SoundFX("solenoid",DOFContactors), 0, 0.25*VolumeDial
	Else
		vpmTimer.AddTimer 175, "RaiseGate'" 
	End If
End Sub

Sub RaiseGate()
	BallLockPrim.Z = 70
	BallLockWall.collidable = 1
	Rubber13.collidable = 1
	Playsound SoundFX("solenoid",DOFContactors), 0, 0.25*VolumeDial
End Sub

'******************************************************
'				NFOZZY'S/JF's FLIPPERS
'******************************************************

Dim returnspeed, lfstep, rfstep
returnspeed = LeftFlipper.return
lfstep = 1
rfstep = 1

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFContactors), 0, 2*VolumeDial, -0.1, 0.25
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown",DOFContactors), 0, 2*VolumeDial, -0.1, 0.25
        LeftFlipper.RotateToStart
		If FlipperTricks = 1 Then
			LeftFlipper.TimerEnabled = 1
			LeftFlipper.TimerInterval = 16
			LeftFlipper.return = returnspeed * 0.5
		End if
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFContactors), 0, 2*VolumeDial, 0.1, 0.25
        RightFlipper.RotateToEnd
		If enableDoubleLeaf = 0 Or enableFastFlips = 0 Then
			RightFlipper2.RotateToEnd	
		End If
    Else
        PlaySound SoundFX("fx_flipperdown",DOFContactors), 0, 2*VolumeDial, 0.1, 0.25
        RightFlipper.RotateToStart
		If enableDoubleLeaf = 0  Or enableFastFlips = 0 Then
			RightFlipper2.RotateToStart	
		End If

		If FlipperTricks = 1 Then
			RightFlipper.TimerEnabled = 1
			RightFlipper.TimerInterval = 16
			RightFlipper.return = returnspeed * 0.5
		End If
    End If
End Sub

Sub SolURFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFContactors), 0, 2*VolumeDial, 0.1, 0.25
		RightFlipper2.RotateToEnd	
    Else
        PlaySound SoundFX("fx_flipperdown",DOFContactors), 0, 2*VolumeDial, 0.1, 0.25
		RightFlipper2.RotateToStart	
    End If
End Sub



Sub LeftFlipper_timer()
	select case lfstep
		Case 1: LeftFlipper.return = returnspeed * 0.6 :lfstep = lfstep + 1
		Case 2: LeftFlipper.return = returnspeed * 0.7 :lfstep = lfstep + 1
		Case 3: LeftFlipper.return = returnspeed * 0.8 :lfstep = lfstep + 1
		Case 4: LeftFlipper.return = returnspeed * 0.9 :lfstep = lfstep + 1
		Case 5: LeftFlipper.return = returnspeed * 1 :lfstep = lfstep + 1
		Case 6: LeftFlipper.timerenabled = 0 : lfstep = 1
	end select
end sub

Sub RightFlipper_timer()
	select case rfstep
		Case 1: RightFlipper.return = returnspeed * 0.6 :rfstep = rfstep + 1
		Case 2: RightFlipper.return = returnspeed * 0.7 :rfstep = rfstep + 1
		Case 3: RightFlipper.return = returnspeed * 0.8 :rfstep = rfstep + 1
		Case 4: RightFlipper.return = returnspeed * 0.9 :rfstep = rfstep + 1
		Case 5: RightFlipper.return = returnspeed * 1 :rfstep = rfstep + 1
		Case 6: RightFlipper.timerenabled = 0 : rfstep = 1
	end select
end sub

'******************************************************
'					BUMPERS
'******************************************************

Sub Bumper1_Hit()
BL1.duration 1, 150, 0
	PlaySound SoundFX("fx_bumper1",DOFContactors), 0, 2*VolumeDial
	vpmTimer.PulseSw 10
End Sub


'******************************************************
'				SLINGSHOTS
'******************************************************

Dim RStep, Lstep, Tstep

Sub LeftSlingShot_Slingshot
	PlaySound SoundFX("SlingshotLeft",DOFContactors), 0, 2*VolumeDial, -0.05, 0.05
	vpmTimer.PulseSw 11
    LeftSling.Visible = 0
    LeftSling1.Visible = 1
    sling1.TransZ = -20
    LStep = 0
    Me.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LeftSLing1.Visible = 0:LeftSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:LeftSLing2.Visible = 0:LeftSLing.Visible = 1:sling1.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	PlaySound SoundFX("SlingshotRight",DOFContactors), 0, 2*VolumeDial, 0.05, 0.05
	vpmTimer.PulseSw 12
    RightSling.Visible = 0
    RightSling1.Visible = 10
    sling2.TransZ = -20
    RStep = 0
    Me.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RightSLing1.Visible = 0:RightSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:RightSLing2.Visible = 0:RightSLing.Visible = 1:sling2.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub TopSlingShot_Slingshot
	PlaySound SoundFX("right_slingshot",DOFContactors), 0, 2*VolumeDial, 0.05, 0.05
	vpmTimer.PulseSw 13
    TopSling.Visible = 0
    TopSling1.Visible = 10
    sling3.TransZ = -20
    TStep = 0
    Me.TimerEnabled = 1
End Sub

Sub TopSlingShot_Timer
    Select Case TStep
        Case 3:TopSLing1.Visible = 0:TopSLing2.Visible = 1:sling3.TransZ = -10
        Case 4:TopSLing2.Visible = 0:TopSLing.Visible = 1:sling3.TransZ = 0:Me.TimerEnabled = 0
    End Select
    TStep = TStep + 1
End Sub


'******************************************************
'				Animated Rubbers
'******************************************************

Sub RubberMHG_hit()
	If InRect(ActiveBall.x,ActiveBall.y,176,56,221,56,221,208,176,208) Then
		RubberMHG.visible=0:RubberMHG1.visible=1
		vpmTimer.AddTimer 100, "RubberMHG.visible=1:RubberMHG1.visible=0'"
	End If
End Sub

Sub RubberMHGLU_hit()
	If InRect(ActiveBall.x,ActiveBall.y,296,233,346,207,366,250,315,273) Then
		RubberMHGLU.visible=0:RubberMHGLU1.visible=1
		vpmTimer.AddTimer 100, "RubberMHGLU.visible=1:RubberMHGLU1.visible=0'"
	End If
End Sub

Sub RubberMHGLL_hit()
	If InRect(ActiveBall.x,ActiveBall.y,318,321,369,298,396,368,347,387) Then
		RubberMHGLL.visible=0:RubberMHGLL1.visible=1
		vpmTimer.AddTimer 100, "RubberMHGLL.visible=1:RubberMHGLL1.visible=0'"
	End If
End Sub

Sub RubberMHGPNP_hit()
	If InRect(ActiveBall.x,ActiveBall.y,258,479,314,475,326,673,272,679) Then
		RubberMHGPNP.visible=0:RubberMHGPNP1.visible=1
		vpmTimer.AddTimer 100, "RubberMHGPNP.visible=1:RubberMHGPNP1.visible=0'"
	End If
End Sub

Sub RubberDT_hit()
	If InRect(ActiveBall.x,ActiveBall.y,472,883,514,857,537,896,495,921) Then
		RubberDT.visible=0:RubberDT1.visible=1
		vpmTimer.AddTimer 100, "RubberDT.visible=1:RubberDT1.visible=0'"
	End If
End Sub

Sub RubberCapture_hit()
	If InRect(ActiveBall.x,ActiveBall.y,600,559,642,570,625,636,581,623) Then
		RubberCapture.visible=0:RubberCapture1.visible=1
		vpmTimer.AddTimer 100, "RubberCapture.visible=1:RubberCapture1.visible=0'"
	End If
End Sub

'******************************************************
'				SWITCHES
'******************************************************

'*******	Targets		******************
Sub dt1_dropped():DTdrop.Hit 1:End Sub
Sub dt2_dropped():DTdrop.Hit 3:End Sub
Sub dt3_dropped():DTdrop.Hit 4:End Sub
Sub dt4_dropped():DTdrop.Hit 2:End Sub

'*******	Targets		******************
Sub sw91_Hit:vpmTimer.PulseSw 91:End Sub
Sub sw101_Hit:vpmTimer.PulseSw 101:End Sub
Sub sw111_Hit:vpmTimer.PulseSw 111:End Sub

Sub sw92_Hit:vpmTimer.PulseSw 92:End Sub
Sub sw102_Hit:vpmTimer.PulseSw 102:End Sub
Sub sw112_Hit:vpmTimer.PulseSw 112:End Sub

Sub sw83_Hit:vpmTimer.PulseSw 83:End Sub
Sub sw93_Hit:vpmTimer.PulseSw 93:End Sub
Sub sw103_Hit:vpmTimer.PulseSw 103:End Sub

Sub sw21_Hit:vpmTimer.PulseSw 21:End Sub
Sub sw22_Hit:vpmTimer.PulseSw 22:End Sub

Sub sw31_Hit:vpmTimer.PulseSw 31:End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:End Sub

'*******	Opto Switches		******************
Sub sw80_Hit:Controller.Switch(80) = 1:End Sub
Sub sw80_UnHit:Controller.Switch(80) = 0:End Sub

Sub sw90_Hit:Controller.Switch(90) = 1:End Sub
Sub sw90_UnHit:Controller.Switch(90) = 0:End Sub


'*******	Rollover Switches	******************

Sub sw33_Hit:Controller.Switch(33) = 1:PlaySound "sensor",0,2*VolumeDial:End Sub
Sub sw33_UnHit
	Dim BOT, b, sw33off
	BOT = GetBalls
	sw33off = true

    For b = 0 to UBound(BOT)
	    if inRect(BOT(b).x, BOT(b).y, 374.14, 573, 434.5, 585.5, 421.8, 625, 361, 625) Then sw33off = false
	Next

    if sw33off then Controller.Switch(33) = 0
End Sub

Sub sw34_Hit:Controller.Switch(34) = 1:PlaySound "sensor",0,2*VolumeDial:End Sub
Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub
Sub sw84_Hit:Controller.Switch(84) = 1:PlaySound "sensor",0,2*VolumeDial:End Sub
Sub sw84_UnHit:Controller.Switch(84) = 0:End Sub
Sub sw94_Hit:Controller.Switch(94) = 1:PlaySound "sensor",0,2*VolumeDial:End Sub
Sub sw94_UnHit:Controller.Switch(94) = 0:End Sub
Sub sw104_Hit:Controller.Switch(104) = 1:PlaySound "sensor",0,2*VolumeDial:End Sub
Sub sw104_UnHit:Controller.Switch(104) = 0:End Sub
Sub sw113_Hit:Controller.Switch(113) = 1:PlaySound "sensor",0,2*VolumeDial:End Sub
Sub sw113_UnHit:Controller.Switch(113) = 0:End Sub
Sub sw114_Hit:Controller.Switch(114) = 1:PlaySound "sensor",0,2*VolumeDial:End Sub
Sub sw114_UnHit:Controller.Switch(114) = 0:End Sub


'******************************************************
'				Lights & Flashers
'******************************************************


Set Lights(0) = l0 		'Golf Again

'* Skins!
Set Lights(2) = l2
Set Lights(3) = l3
Set Lights(4) = l4
Set Lights(5) = l5
Set Lights(6) = l6
Set Lights(7) = l7

Set Lights(10) = l10
Set Lights(11) = l11

'* GOPHER
Set Lights(12) = l12
Set Lights(13) = l13
Set Lights(14) = l14
Set Lights(15) = l15
Set Lights(16) = l16
Set Lights(17) = l17

Set Lights(21) = l21
Set Lights(22) = l22
Set Lights(23) = l23
Set Lights(24) = l24
Set Lights(25) = l25
Set Lights(26) = l26
Set Lights(27) = l27
Set Lights(30) = l30
Set Lights(31) = l31
Set Lights(32) = l32
Set Lights(33) = l33
Set Lights(34) = l34
Set Lights(35) = l35
Set Lights(36) = l36
Set Lights(37) = l37
Set Lights(40) = l40
Set Lights(41) = l41
Set Lights(42) = l42
Set Lights(43) = l43
Set Lights(44) = l44
Set Lights(45) = l45
Set Lights(46) = l46
Set Lights(47) = l47
Set Lights(50) = l50
Set Lights(51) = l51
Set Lights(52) = l52
Set Lights(53) = l53
Set Lights(54) = l54
Set Lights(55) = l55
Set Lights(56) = l56
Set Lights(57) = l57
Set Lights(60) = l60
Set Lights(61) = l61
Set Lights(62) = l62
Set Lights(63) = l63
Set Lights(64) = l64
Set Lights(65) = l65
Set Lights(66) = l66
Set Lights(67) = l67
Set Lights(70) = l70
Set Lights(71) = l71
Set Lights(72) = l72		'Pop Bumper 
Set Lights(73) = l73
Set Lights(74) = l74
Set Lights(75) = l75
Set Lights(76) = l76
Set Lights(77) = l77
Set Lights(80) = l80
Set Lights(81) = l81
Set Lights(82) = l82
Set Lights(83) = l83
Set Lights(84) = l84
Set Lights(85) = l85
Set Lights(86) = l86
Set Lights(87) = l87

'* Gopher Wheel Illumination
Lights(90) = Array(l90,l90b,l90c)
Lights(91) = Array(l91,l91b,l91c)
Lights(92) = Array(l92,l92b,l92c)
Lights(93) = Array(l93,l93b,l93c)


Lights(100) = Array(l100,l100a)
Lights(101) = Array(l101,l101a)
Lights(102) = Array(l102,l102a)
Lights(103) = Array(l103,l103a)
Lights(104) = Array(l104,l104a)

'*  Flashers

Sub Flasher11(enabled):F11.state = enabled:F11b.state = enabled:End Sub
Sub Flasher12(enabled):F12.state = enabled:F12b.state = enabled:End Sub
Sub Flasher13(enabled)
	F13.state = enabled
	F13b.state = enabled
	If enabled then
		F13On = 1
		F13.timerenabled = true
Primitive_Volcano.blenddisablelighting = 0.5
	Else
		F13On = -1
		F13.timerenabled = true
Primitive_Volcano.blenddisablelighting = 0
	End If
End Sub
Sub Flasher14(enabled):F14.state = enabled:F14b.state = enabled:End Sub 
Sub Flasher15(enabled):F15.state = enabled:F15b.state = enabled:End Sub 
Sub Flasher16(enabled):F16.state = enabled:F16b.state = enabled:
	If enabled then
		F16On = 1
		F16.timerenabled = true
	Else
		F16On = -1
		F16.timerenabled = true
	End If
End Sub
Sub Flasher17(enabled):F17.state = enabled:F17b.state = enabled:End Sub
Sub Flasher18(enabled):F18.state = enabled:F18b.state = enabled:End Sub
Sub Flasher19(enabled):F19.state = enabled:F19b.state = enabled:End Sub


Dim F13On, F13Count
F13Count = 0

Sub F13_Timer()
	F13Count = F13Count + F13On
	If F13Count < 0 Then:F13Count = 0
	If F13Count > 2 Then:F13Count = 2
	
	If VolcanoLighting Then
		Select Case F13Count
			Case 0: Primitive_Volcano.image="VolcanoMap_GION5"
			Case 1: Primitive_Volcano.image="VolcanoMap_FLSHON0007"
			Case 2: Primitive_Volcano.image="VolcanoMap_FLSHON0010"
		End Select
	End If

	Select Case F13Count
		Case 0: Prim_Dome1.image = "dome3_clear_bright_off":F13c.intensityscale=0:F13d.intensityscale=0:F13e.intensityscale=0
		Case 1: Prim_Dome1.image = "dome3_clear_bright":F13c.intensityscale=1:F13d.intensityscale=1:F13e.intensityscale=0.5
		Case 2: Prim_Dome1.image = "dome3_clear_bright_on":F13c.intensityscale=2:F13d.intensityscale=2:F13e.intensityscale=1
	End Select


	If F13Count = 0 And F13On = -1 Then:F13.timerenabled = false 
	If F13Count = 2 And F13On = 1 Then:F13.timerenabled = false
End Sub


Dim F16On, F16Count
F16Count = 0

Sub F16_Timer()
	F16Count = F16Count + F16On
	If F16Count < 0 Then:F16Count = 0
	If F16Count > 2 Then:F16Count = 2
	
	Select Case F16Count
		Case 0: Prim_Dome2.image = "dome3_clear_bright_off"
		Case 1: Prim_Dome2.image = "dome3_clear_bright"
		Case 2: Prim_Dome2.image = "dome3_clear_bright_on"
	End Select

	If F16Count = 0 And F16On = -1 Then:F16.timerenabled = false 
	If F16Count = 2 And F16On = 1 Then:F16.timerenabled = false
End Sub


'******************************************************
' 			GENERAL ILLUMINATION
'******************************************************

Dim xx

Sub GIRelay(enabled)
	If Enabled Then
 		Playsound "fx_relay_off",0,1*VolumeDial
	Else
 		Playsound "fx_relay_on",0,1*VolumeDial
	End If
End Sub

Sub TiltRelay(enabled)
	If Enabled Then
 		for each xx in GI:xx.State=0:Next
		for each xx in GI_Upper:xx.State=0:Next
 		Table1.ColorGradeImage = "ColorGrade_4"
		Playsound "fx_relay_off",0,1*VolumeDial
		If VolcanoLighting Then
			vpmTimer.AddTimer 10, "Primitive_Volcano.image=""VolcanoMap_GION3""'"
			vpmTimer.AddTimer 20, "Primitive_Volcano.image=""VolcanoMap_GION1""'"
			vpmTimer.AddTimer 30, "Primitive_Volcano.image=""VolcanoMap_OFF""'"
		End If
	Else
 		for each xx in GI:xx.State=1:Next
		for each xx in GI_Upper:xx.State=1:Next
 		Table1.ColorGradeImage = "ColorGradeLUT256x16_extraConSat"
		Playsound "fx_relay_on",0,1*VolumeDial
		If VolcanoLighting Then
			vpmTimer.AddTimer 10, "Primitive_Volcano.image=""VolcanoMap_GION1""'"
			vpmTimer.AddTimer 20, "Primitive_Volcano.image=""VolcanoMap_GION3""'"
			vpmTimer.AddTimer 30, "Primitive_Volcano.image=""VolcanoMap_GION5""'"
		End If
	End If
End Sub


'******************************************************
'				Gopher Wheel
'******************************************************

Dim roulette_step, roulette_time, wheelangle, ball, isOn, isOff, holeangle, holeanglemod, prevholeangle, whxx
roulette_step = 0
roulette_time = 0
wheelangle = 0 
isOn = True
isOff = True
holeangle = 0 

Dim RouletteBall, CoconutBall

Dim GWHoles
GWHoles = Array (GW0,GW1,GW2,GW3,GW4,GW5,GW6,GW7,GW8,GW9,GW10,GW11)


Sub Init_Roulette()
	GWRamp.collidable=0
	Set RouletteBall = GW0.CreateSizedBallWithMass(18,20) '23,1.0*((23*2)^3)/125000
'	RouletteBall.image = "Ball_HDR"
RouletteBall.bulbintensityscale = 0.3
	RouletteSw.enabled = 1

	Set CoconutBall = Kicker2.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Kicker2.kick 180,1
	Kicker2.enabled = false
End Sub

Sub EngageRoulette(enabled):If enabled then:isOn=True:isOff=true:roulette.enabled = 1:End If:End Sub

Sub Roulette_Timer()
	GopherWheel.RotY =  180 + wheelangle
	GopherCone.RotY =  210 + wheelangle
	GopherKicker.RotY = 180 + wheelangle
	CapGW.RotY = 96 + wheelangle

	wheelangle = wheelangle + roulette_step

	holeangle = wheelangle mod 30
	prevholeangle = holeangle
	
	if roulette_time < 800 Then '1500
		if isOn = True And isOff = True Then
			For each ii in Rswitches:Controller.switch(ii)=0:next
			roulettesw.enabled = false
			For ii = 0 to 11
				GWHoles(ii).kick ((ii+1)*30+60), (Int(Rnd * 4) + 12)
				GWHoles(ii).enabled = false
			Next
			GWRamp.collidable = 1
			ttGopherWheel.MotorOn = True
			isOn = False
			Playsound "fx_motor",0,2*VolumeDial
		End If
	    If wheelangle > 360 Then wheelangle = wheelangle - 360
		roulette_step = roulette_step + 0.05
		If roulette_step > 0.75 Then roulette_step = 0.75
	Else
		if isOff = True Then
			ttGopherWheel.MotorOn = False
			isOff = False
			StopSound "fx_motor"
		End If
		roulette_step = roulette_step - 0.0005
		If roulette_step < 0.15 and holeangle = 0 Then
			GWRamp.collidable = 0
			roulette_step=0
			roulette.enabled = false
			roulettesw.enabled = true
			roulette_time = 0
		End If			
	End If

	roulette_time = roulette_time + 1
End Sub


Dim RouletteAtRest, RouletteBallAngle, RouletteX, RouletteY, RouletteAtn2, Rswitches, Rletters(12)
Const RouletteCenterX = 427.8
Const RouletteCenterY = 1324.3
RouletteAtRest = 0

Rswitches = array(40,41,42,43,44,45,50,51,52,53,54,55)

Sub GetRouletteSw()
	RouletteBallAngle = CalcAngle(RouletteBall.X,RouletteBall.Y,RouletteCenterX,RouletteCenterY)
	Rletters(0) = wheelangle + 38
	for ii = 0 to 11
		Rletters(ii) = (Rletters(0) + (ii*30)) mod 360
		If AngleIsNear(RouletteBallAngle,Rletters(ii),15) Then
			Controller.Switch(Rswitches(ii)) = 1
			Exit For
		End If
	next

End Sub


Sub RouletteSw_Timer()
	'debug.print BallVel(RouletteBall)
	If BallVel(RouletteBall) < 2 Then
		For each ii in GWKickers
			ii.enabled = true
		next
	End If
End Sub


Sub GW0_hit():GetRouletteSw:End Sub
Sub GW1_hit():GetRouletteSw:End Sub
Sub GW2_hit():GetRouletteSw:End Sub
Sub GW3_hit():GetRouletteSw:End Sub
Sub GW4_hit():GetRouletteSw:End Sub
Sub GW5_hit():GetRouletteSw:End Sub
Sub GW6_hit():GetRouletteSw:End Sub
Sub GW7_hit():GetRouletteSw:End Sub
Sub GW8_hit():GetRouletteSw:End Sub
Sub GW9_hit():GetRouletteSw:End Sub
Sub GW10_hit():GetRouletteSw:End Sub
Sub GW11_hit():GetRouletteSw:End Sub

 '*****************************************************************
 'Functions
 '*****************************************************************

'*** AngleIsNear returns true if the testangle is within range degrees of the target angle for 0 to 360 degree scale

Function AngleIsNear(testangle, target, range)
	If target <= range Then
		AngleIsNear = ( (testangle + (range*2) ) mod 360 >= target + range ) AND ( (testangle + (range*2) ) mod 360 <= target + (range*3) )
	ElseIf target >= 360 - range Then
		AngleIsNear = ( (testangle - (range*2) ) mod 360 >= target - (range*3) ) AND ( (testangle - (range*2) ) mod 360 <= target - range )
	Else
		AngleIsNear = (testangle >= target - range) AND (testangle <= target + range)
	End If
End Function

'*** CalcAngle returns the angle of a point (x,y) on a circle with center (centerx, centery) with angle 0 at 3 o'clock

Function CalcAngle(x,y,centerX,centerY)
	
	Dim tmpAngle
	
	tmpAngle = Atan2(x - centerX, y - centerY) * 180 / (4*Atn(1))

	If tmpAngle < 0 Then	
		CalcAngle = tmpAngle + 360
	Else	
		CalcAngle = tmpAngle
	End If

End Function

'*** Atan2 returns the Atan2 for a point on a circle (x,y)

Function Atan2(x,y)

	If x > 0 Then
		Atan2 = Atn(y/x)
	ElseIf x < 0 Then
		Atan2 = Sgn(y) * ((4*Atn(1)) - Atn(Abs(y/x)))
	ElseIf y = 0 Then
		Atan2 = 0
	Else
		Atan2 = Sgn(y) * (4*Atn(1)) / 2
	End If

End Function

'*** PI returns the value for PI

Function PI()

	PI = 4*Atn(1)

End Function

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
	Dim AB, BC, CD, DA
	AB = (ax*by - ay*bx) + (bx*py - by*px) + (px*ay - py*ax)
	BC = (bx*cy - by*cx) + (cx*py - cy*px) + (px*by - py*bx)
	CD = (cx*dy - cy*dx) + (dx*py - dy*px) + (px*cy - py*cx)
	DA = (dx*ay - dy*ax) + (ax*py - ay*px) + (px*dy - py*dx)
 
	If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
		InRect = True
	Else
		InRect = False       
	End If
End Function


'******************************************************
'       		RealTime Updates
'******************************************************

Set MotorCallback = GetRef("GameTimer")

Sub GameTimer()
    UpdateMechs
	RollingSoundUpdate
End Sub

Sub UpdateMechs
	Prim_LeftFlipper.RotY=LeftFlipper.currentangle-90
	Prim_RightFlipper.RotY=RightFlipper.currentangle-90
	FlipperUR.RotY = RightFlipper2.currentangle
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperR2Sh.RotZ = RightFlipper2.currentangle
End Sub

'*****************************************
'	Ramp and Drop Sounds
'*****************************************

'PlaySound "name",loopcount,volume,pan,randompitch,pitch,useexisting,restart
Sub TUKRampStart_Hit()
	PlaySound "wireramp_right", 1, 0.3*volumedial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub TUKRampStop_Hit()
	StopSound "wireramp_right"
End Sub

Sub RightRampStart_Hit()
	PlaySound "wireramp_stop", 0, 0.3*volumedial, 0.2
	PlaySound "wireramp_right2", -1, 0.3*volumedial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Dim RightRampBall

Sub RightRampStop_Hit()
	StopSound "wireramp_right2"
	PlaySound "wireramp_stop", 0, 0.3*volumedial, 0.2
	Set RightRampBall = ActiveBall
	RightRampStop.timerenabled = true
End Sub

Sub RightRampStop_Timer()
	If RightRampBall.Z < 140 then
		BallDropSound(1)	
		me.timerenabled = False
	End If
End Sub

Sub CUKRampStart_Hit()
	PlaySound "wireramp_right3", 1, 0.3*volumedial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Dim CUKRampBall

Sub CUKRampStop_Hit()
	StopSound "wireramp_right3"
	PlaySound "wireramp_stop", 0, 0.3*volumedial, 0.2
	Set CUKRampBall = ActiveBall
	CUKRampStop.timerenabled = True
End Sub

Sub CUKRampStop_Timer()
	If CUKRampBall.Z < 140 then
		BallDropSound(1)	
		me.timerenabled = False
	End If
End Sub

Sub RedRampStart_Hit()
	PlaySound "plasticroll", 1, Vol(ActiveBall)*volumedial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub RedRampStop_Hit()
	StopSound "plasticroll"
End Sub

Dim RedRampBall

Sub RedRampStop1_Hit()
	StopSound "plasticroll"
	PlaySound "wireramp_stop", 0, 0.3*volumedial, 0.2
	Set RedRampBall = ActiveBall
	RedRampStop1.timerenabled = true
End Sub

Sub RedRampStop1_Timer()
	If RedRampBall.Z < 140 then
		BallDropSound(1)	
		me.timerenabled = False
	End If
End Sub

Sub PNPStart_Hit()
	If Activeball.vely < 0 Then
		PlaySound "plasticroll1", 1, Vol(ActiveBall)*5*volumedial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End If
End Sub

Sub PNPStart_Unhit()
	If Activeball.vely > 0 Then
		StopSound "plasticroll1"
		BallDropSound(0.3)
	End If
End Sub

Sub PNPStop_UnHit()
	If Activeball.vely < 0 Then
		StopSound "plasticroll1"
		vpmTimer.AddTimer 200, "BallDropSound(1)'"
	End If
End Sub

Sub SLStart_Hit()
	If Activeball.vely < 0 Then
		PlaySound "plasticroll2", 1, Vol(ActiveBall)*5*volumedial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End If
End Sub

Sub SLStart_Unhit()
	If Activeball.vely > 0 Then
		StopSound "plasticroll2"
		BallDropSound(0.05)
	End If
End Sub

Sub SLStop_UnHit()
	If Activeball.vely < 0 Then
		StopSound "plasticroll2"
		vpmTimer.AddTimer 100, "BallDropSound(0.5)'"
	End If
End Sub


Sub BallDropSound(vol)
	PlaySound "BallDrop", 0, vol*volumedial
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
    Vol = Csng(BallVel(ball) ^2 / 400)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'******************************************************
'      		JP's VP10 Rolling Sounds
'******************************************************

Const tnob = 8 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingSoundUpdate()
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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/7, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/12, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
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

	' *********	Kicker Code
	If InRect(BOT(b).x,BOT(b).y,222,118,272,118,272,168,222,168) Then
		If ABS(BOT(b).velx) < 0.05 and ABS(BOT(b).vely) < 0.05 Then
			MHG.enabled = True
		End If
	End If

	If InRect(BOT(b).x,BOT(b).y,467,630,517,630,517,680,467,680) Then
		If ABS(BOT(b).velx) < 0.05 and ABS(BOT(b).vely) < 0.05 Then
			CUK.enabled = True
		End If
	End If
	' *********	End Kicker Code	
    Next
End Sub

'******************************************************
' 			JP's VP10 Ball Collision Sound
'******************************************************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, (Csng(velocity) ^2 / 2000)*2*VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'*****************************************
'	Ball Shadow
'*****************************************

Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6, BallShadow7, BallShadow8)

Sub BallShadow_Timer()
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
ballShadow(b).opacity = 80
End If
    Next	
End Sub
'******************************************************
' 				JP's Sound Routines
'******************************************************

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*CollectionVolume, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound SoundFX("target",DOFContactors),0,CollectionVolume/10
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*CollectionVolume/5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*CollectionVolume, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate", 0, Vol(ActiveBall)*CollectionVolume/10, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall)*CollectionVolume/10, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*CollectionVolume/5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*CollectionVolume/5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*CollectionVolume/5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub Woods_Hit (idx)
	PlaySound "woodhit", 0, Vol(ActiveBall)*CollectionVolume, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper2_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*CollectionVolume, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*CollectionVolume, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*CollectionVolume, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub




'cFastFlips by nFozzy
'Bypasses pinmame callback for faster and more responsive flippers
'Version 1.1 beta2 (More proper behaviour, extra safety against script errors)

'Flipper / game-on Solenoid # reference
'Atari: Sol16
'Astro:  ?
'Bally Early 80's: Sol19
'Bally late 80's (Blackwater 100, etc): Sol19
'Game Plan: Sol16
'Gottlieb System 1: Sol17
'Gottlieb System 80: No dedicated flipper solenoid? GI circuit Sol10?
'Gottlieb System 3: Sol32
'Playmatic: Sol8
'Spinball: Sol25
'Stern (80's): Sol19
'Taito: ?
'Williams System 3, 4, 6: Sol23
'Williams System 7: Sol25
'Williams System 9: Sol23
'Williams System 11: Sol23
'Bally / Williams WPC 90', 92', WPC Security: Sol31
'Data East (and Sega pre-whitestar): Sol23
'Zaccaria: ???

'********************Setup*******************:

'....somewhere outside of any subs....
'dim FastFlips

'....table init....
'Set FastFlips = new cFastFlips
'with FastFlips
'	.CallBackL = "SolLflipper"	'Point these to flipper subs
'	.CallBackR = "SolRflipper"	'...
''	.CallBackUL = "SolULflipper"'...(upper flippers, if needed)
''	.CallBackUR = "SolURflipper"'...
'	.TiltObjects = True 'Optional, if True calls vpmnudge.solgameon automatically. IF YOU GET A LINE 1 ERROR, DISABLE THIS! (or setup vpmNudge.TiltObj!)
''	.InitDelay "FastFlips", 100			'Optional, if > 0 adds some compensation for solenoid jitter (occasional problem on Bram Stoker's Dracula)
''	.DebugOn = False		'Debug, always-on flippers. Call FastFlips.DebugOn True or False in debugger to enable/disable.
'end with

'...keydown section... commenting out upper flippers is not necessary as of 1.1
'If KeyCode = LeftFlipperKey then FastFlips.FlipL True :  FastFlips.FlipUL True
'If KeyCode = RightFlipperKey then FastFlips.FlipR True :  FastFlips.FlipUR True
'(Do not use Exit Sub, this script does not handle switch handling at all!)

'...keyUp section...
'If KeyCode = LeftFlipperKey then FastFlips.FlipL False :  FastFlips.FlipUL False
'If KeyCode = RightFlipperKey then FastFlips.FlipR False :  FastFlips.FlipUR False

'...Solenoid...
'SolCallBack(31) = "FastFlips.TiltSol"
'//////for a reference of solenoid numbers, see top /////


'One last note - Because this script is super simple it will call flipper return a lot.
'It might be a good idea to add extra conditional logic to your flipper return sounds so they don't play every time the game on solenoid turns off
'Example:
'Instead of
		'LeftFlipper.RotateToStart
		'playsound SoundFX("FlipperDown",DOFFlippers), 0, 1, 0.01	'return
'Add Extra conditional logic:
		'LeftFlipper.RotateToStart
		'if LeftFlipper.CurrentAngle = LeftFlipper.StartAngle then
		'	playsound SoundFX("FlipperDown",DOFFlippers), 0, 1, 0.01	'return
		'end if
'That's it]
'*************************************************
Function NullFunction(aEnabled):End Function	'1 argument null function placeholder
Class cFastFlips
	Public TiltObjects, DebugOn, hi
	Private SubL, SubUL, SubR, SubUR, FlippersEnabled, Delay, LagCompensation, Name, FlipState(3)
	
	Private Sub Class_Initialize()
		Delay = 0 : FlippersEnabled = False : DebugOn = False : LagCompensation = False
		Set SubL = GetRef("NullFunction"): Set SubR = GetRef("NullFunction") : Set SubUL = GetRef("NullFunction"): Set SubUR = GetRef("NullFunction")
	End Sub
	
	'set callbacks
	Public Property Let CallBackL(aInput)  : Set SubL  = GetRef(aInput) : Decouple sLLFlipper, aInput: End Property
	Public Property Let CallBackUL(aInput) : Set SubUL = GetRef(aInput) : End Property
	Public Property Let CallBackR(aInput)  : Set SubR  = GetRef(aInput) : Decouple sLRFlipper, aInput:  End Property
	Public Property Let CallBackUR(aInput) : Set SubUR = GetRef(aInput) : End Property
	Public Sub InitDelay(aName, aDelay) : Name = aName : delay = aDelay : End Sub	'Create Delay
	'Automatically decouple flipper solcallback script lines (only if both are pointing to the same sub) thanks gtxjoe
	Private Sub Decouple(aSolType, aInput)  : If StrComp(SolCallback(aSolType),aInput,1) = 0 then SolCallback(aSolType) = Empty End If : End Sub

	'call callbacks
	Public Sub FlipL(aEnabled)
		FlipState(0) = aEnabled	'track flipper button states: the game-on sol flips immediately if the button is held down (1.1)
		If not FlippersEnabled and not DebugOn then Exit Sub
		subL aEnabled
	End Sub

	Public Sub FlipR(aEnabled)
		FlipState(1) = aEnabled
		If not FlippersEnabled and not DebugOn then Exit Sub
		subR aEnabled
	End Sub

	Public Sub FlipUL(aEnabled)
		FlipState(2) = aEnabled
		If not FlippersEnabled and not DebugOn then Exit Sub
		subUL aEnabled
	End Sub	

	Public Sub FlipUR(aEnabled)
		FlipState(3) = aEnabled
		If not FlippersEnabled and not DebugOn then Exit Sub
		subUR aEnabled
	End Sub	
	
	Public Sub TiltSol(aEnabled)	'Handle solenoid / Delay (if delayinit)
		If delay > 0 and not aEnabled then 	'handle delay
			vpmtimer.addtimer Delay, Name & ".FireDelay" & "'"
			LagCompensation = True
		else
			If Delay > 0 then LagCompensation = False
			EnableFlippers(aEnabled)
		end If
	End Sub
	
	Sub FireDelay() : If LagCompensation then EnableFlippers False End If : End Sub
	
	Private Sub EnableFlippers(aEnabled)
		If aEnabled then SubL FlipState(0) : SubR FlipState(1) : subUL FlipState(2) : subUR FlipState(3)
		FlippersEnabled = aEnabled
		If TiltObjects then vpmnudge.solgameon aEnabled
		If Not aEnabled then
			subL False
			subR False
			If not IsEmpty(subUL) then subUL False
			If not IsEmpty(subUR) then subUR False
		End If		
	End Sub
	
End Class