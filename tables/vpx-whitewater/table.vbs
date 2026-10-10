' __    __  __ __  ____  ______    ___      __    __   ____  ______    ___  ____  
'|  |__|  ||  |  ||    ||      |  /  _]    |  |__|  | /    ||      |  /  _]|    \ 
'|  |  |  ||  |  | |  | |      | /  [_     |  |  |  ||  o  ||      | /  [_ |  D  )
'|  |  |  ||  _  | |  | |_|  |_||    _]    |  |  |  ||     ||_|  |_||    _]|    / 
'|  '  '  ||  |  | |  |   |  |  |   [_     |  '  '  ||  _  |  |  |  |   [_ |    \ 
' \      / |  |  | |  |   |  |  |     |     \      / |  |  |  |  |  |     ||  .  |
'  \_/\_/  |__|__||____|  |__|  |_____|      \_/\_/  |__|__|  |__|  |_____||__|\_|
     
                                                                            
' Williams White Water / IPD No. 2768 / January, 1993 / 4 Players
' made for VPX by Flupper
' http://www.ipdb.org/machine.cgi?id=2768
' Thanks to JPSalas and PacDude's earlier versions
' Many thanks also to:
' rock primitives & base textures, bigfoot, transparent targets by Dark
' playfield texture and plastic images by Clark Kent
' texture redraws by Lobotomy
' Physics tuning by wrd1972 and Clark Kent
' reference images of darkened table by darquayle
' many mechanical sounds by Knorr
' sound tuning by DjRobX
' 
' Notes (from experience during development): 
' If Bigfoot head gets out of sync or VPM crashes, delete NVRAM

' Release notes:
' version 1.0    - Initial version
' version 1.1    - Physics update by Clark Kent
'				 - Changed sidewalls without reflection to be more subtle
'				 - 3 ball and ball decal textures added
'				 - Several visual fixes (a.o. left upper flasher visiblity, wireramp AO added)
' version 1.2	 - nFozzy/RothbauerW physics and Fleep sounds added by Robby King Pin
'                - VR stuff from Sixtoe / Tastywasp of the table to make the table hybrid by Robby King Pin.
' 				 - Corrected flippers and the area around by Clark Kent.
'                - Flipperbats added from Flupper's Whirlwind by Robby King Pin
'                - New wire ramp at the plunger lane created by Tomate
'                - Added codes for Dynamic Ball Shadows by Robby King Pin
'                - Fixed all VR "Laser lights" by leojreimroc
'                - Readjusted all side blade flashers to fit table by leojreimroc
'                - Changed Plunger release speed to 100 by leojreimroc
'                - Big Foot motorsounds added by Robby King Pin
' 				 - Added VR plunger animation and Added VR flipper button animations. by TastyWasps
'                - VR Backglass redone by leojreimroc
'                - Apophis Reworked GI fading code. Brightened bats
'                - Using light _animate sub in GI fading now by Apophis. Tweaked DN slider slightly again.
'                - Lowered flipper strength and slingshot force by recommendations from Clark Kent and Apophis
'                - Staged Flipper codes added by Primetime5k
'                - Edited cabinet texture by Robby King Pin with new PS images from HauntFreaks
'                - Added VPW Options UI and disabled DesktopVPXDMD to stop interfearing with the menu by Robby King Pin
'				 - Redid all the ramp textures with refraction, new texture and normal map (Flupper)
'				 - Redid all the insert lighting and some of the flashers (Flupper)
'				 - Retextured the VR room (Flupper)
'				 - Tweaked the playfield image (Flupper)
'				 - Tweaked the lost mine kickout (Flupper)
'				 - changed environment image (Flupper)
'				 - VR cabinet Topper by Retro27

Option Explicit : Randomize
Setlocale 1033
Dim ForceSiderailsFS, HitTheGlass, DisableGI', DesktopVPXDMD
Dim FlasherTimerInterval, debugballs, NoSideWallRelfections
Dim Scoop_Difficulty

' *** synchronize flasher frequency with screen refresh rate (ms) *************************
' *** 17 for 60 fps, 20 for 100 fps, 13 for 75 fps ****************************************
' *** you can use -1 for a fps locked 60 fps **********************************************
FlasherTimerInterval = 30

' *** Show siderails in fullscreen mode: True = show siderails, False = do not show *******
ForceSiderailsFS = False

' *** Special effect: let the ball hit the glass at the top of the waveramp ***************
HitTheGlass = True

' *** Special effect: Play with the global illumination off *******************************
DisableGI = False

'*** Dynamic Ball Shadows: 1 = On, 0 = Off ************************************************
DynamicBallShadowsOn = 0

' **** enables manual ball control with C key (enable/disable control) ********************
' **** and B key (speed boost) and arrow keys *********************************************
debugballs = False

' **** Use built-in VPX DMD instead of relying on VPM one *********************************
' **** looks nicer, required for exclusive fullscreen *************************************
'DesktopVPXDMD = True

' *** different ball brightness options ***************************************************
' change this yourself in Ball settings:
' ballHDR3bright and scratches4bright are when playing with low settings of day night slider
' ballHDR3dark and scratches4dark are when playing with high settings of day night slider

'*******************************************
'  ZCON: Constants and Global Variables
'*******************************************

Const BallSize = 50				 'Ball diameter in VPX units; must be 50
Const BallMass = 1				  'Ball mass must be 1
Const tnob = 3					  'Total number of balls the table can hold
Const lob = 0					   'Locked balls
Const cGameName = "ww_lh6"		 'The unique alphanumeric name for this table
Const VRRoom = 0

Dim tablewidth
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height
Dim BIP		' Balls in play
BIP = 0
Dim BIPL	' Ball in plunger lane
BIPL = False

'VRRoom Initialize *******************

'dim VRRoom, UseVPMDMD
'If RenderingMode = 2 Then VRRoom = 1 Else VRRoom = 0

'If VRRoom = 1 Then
'	Dim VR_Obj
'	Table1.PlayfieldReflectionStrength = 10
'	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 1 : Next
'	For Each VR_Obj in VR_Room : VR_Obj.Visible = 1 : Next
'	lockbar.Visible = 0
'	leftrail.Visible = 0
'	rightrail.Visible = 0
'	Primitive111.Visible = 0
'	Primitive112.Visible = 0
'	Primitive114.Visible = 0
'	' Right Side
'	Primitive108.Visible = 0 
'	Rock5_bigfoot_cave1.Visible = 0
'	Primitive24.Visible = 0
'	Primitive110.Visible = 0
'	Primitive106.Visible = 0
'	Primitive109.Visible = 0
'	' Left Side
'	Primitive5temp10.Visible = 0
'	Primitive10.Visible = 0
'	Primitive9.Visible = 0
'	UseVPMDMD = true
'	SetBackGlass
'Else
'	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 0 : Next
'	For Each VR_Obj in VR_Room : VR_Obj.Visible = 0 : Next
'	If Table1.ShowDT Then UseVPMDMD = true
'End If

' *** (not working anymore) disable side wall reflections *********************************
NoSideWallRelfections = True


'******************************************************************************************
'* END OF TABLE OPTIONS *******************************************************************
'******************************************************************************************

' explanation of standard constants (info by Jpsalas and others on VpForums): 
' These constants are for the vpinmame emulation, and they tell vpinmame what it is supposed 
' to emulate.
' UseSolenoids=1 means the vpinmame will use the solenoids, so in the script there are calls
' 				 for a solenoid to do different things (like reset droptargets, kick a ball, etc)
' UseLamps=0 	 means the vpinmame won't bother updating the lights, but done in script
' UseSync=0      (unclear) but probably is to enable or disable the sync in the vpinmame window 
' 				 (or dmd). So it syncs with the screen or not.
' HandleMech=0   means vpinmame won't handle  special animations, they will have to be done 
'				 manually in Scripts
' UseGI=1        If 1 and used together with "Set GiCallback2 = GetRef("UpdateGI")" where 
'				 UpdateGI is the sub routine that sets the Global Illumination lights 
' 				 only the Williams wpc tables have Gi circuitry support (so you can use GICallback)
'				 for other tables a solenoid has to be used
' SFlipperOn     - Flipper activate sound
' SFlipperOff    - Flipper deactivate sound
' SSolenoidOn    - Solenoid activate sound
' SSolenoidOff   - Solenoid deactivate sound
' SCoin          - Coin Sound 
' UseVPMModSol   When True this allows the ROM to control the intensity level of modulated solenoids
'				 instead of just on/off.
' UseVPMDMD      - Enable VPX rendering of DMD

' *** Global constants ***
Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0
Const UseGI = 1
Const cCredits = "White Water, Williams 1993"
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = "SolenoidOff"
Const SFlipperOn = "FlipperUp"
Const SFlipperOff = "FlipperDown"
Const SCoin = "coin"
Const UseVPMModSol = 1

' *** Global variables ***
Dim relaylevel : relaylevel = 0.5 * GlobalSoundLevel ' Sound level of relay clicking
Dim metalvolume : metalvolume = 0 ' is zero until balls have been created
Dim FlippersEnabled	 ' Used to enable/disable flippers based on tilt status
' UseVPMDMD = true '(Table1.ShowDT AND DesktopVPXDMD)

'----- Desktop Visible Elements -----
Dim VarHidden', UseVPMDMD

' *** Start VPM ***

if Version < 10400 then msgbox "This table requires Visual Pinball 10.4 beta or newer!" & vbnewline & "Your version: " & Version/1000

On Error Resume Next
	ExecuteGlobal GetTextFile("controller.vbs")
	If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01560000", "WPC.VBS", 3.50
Set GiCallback2 = GetRef("UpdateGI")

'*******************************************
'	ZTIM:Timers
'*******************************************

Sub GameTimer_Timer() 'The game timer interval; should be 10 ms
	Cor.Update	  'update ball tracking (this sometimes goes in the RDampen_Timer sub)
	RollingUpdate   'update rolling sounds
	'DoDTAnim		'handle drop target animations
	DoSTAnim		'handle stand up target animations
	'queue.Tick	  'handle the queue system
End Sub

Dim FrameTime, InitFrameTime
InitFrameTime = 0
Sub FrameTimer_Timer() 'The frame timer interval should be -1, so executes at the display frame rate
	FrameTime = GameTime - InitFrameTime
	InitFrameTime = GameTime	'Count frametime
	FlipperVisualUpdate		 'update flipper shadows and primitives
	Options_UpdateDMD
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

Dim WWBall1, WWBall2, WWBall3, gBOT

Sub Table1_Init
	Dim Light, Prim, obj
	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Whitewater - Williams 1993"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		'.hidden = UseVPMDMD
		.hidden = VarHidden
		'.hidden = not whitewater.showdt
		 On Error Resume Next
		 .Run GetPlayerHWnd
		 If Err Then MsgBox Err.Description
		 On Error Goto 0
	 End With

	' old script from vp9: What's this for?
	'Controller.SolMask(0) = 0
	'vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
	'Controller.DIP(0) = &H00
	'Controller.Run GetPlayerHWnd
	'Controller.Switch(22) = 1 'close coin door
	'Controller.Switch(24) = 1 'and keep it close
		
	' Nudging
	vpmNudge.TiltSwitch = 14
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(LeftSlingshot, RightSlingshot, Bumper1, Bumper2, Bumper3)

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1
	LampTimer.Enabled = 1

	Set WWBall3 = sw78.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set WWBall2 = sw77.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set WWBall1 = sw76.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(WWBall1,WWBall2,WWBall3)
	
	Controller.Switch(76) = 1
	Controller.Switch(77) = 1
	Controller.Switch(78) = 1

	multiballwall65.isDropped = true
	multiballwall64.isDropped = true

	GILowerFade(8) : GIMiddleFade(8) : GIUpperFade(8)
	'CheckMaxBalls 'Allow balls to be created at table start up
	BigFoot_Init
	InitLights(Insertlights)

	If VRRoom <> 1 Then
		If Table1.ShowDT or ForceSiderailsFS then 
			If NoSideWallRelfections Then
				Primitive65.visible = 0 : Primitive61.visible = 0 : Primitive111.visible = 1 : Primitive112.visible = 1 : Primitive113.visible = 1 : Primitive114.visible = 0
			Else
				Primitive65.visible = 1 : Primitive61.visible = 0 : Primitive111.visible = 1 : Primitive112.visible = 1 : Primitive113.visible = 0 : Primitive114.visible = 0
			End If
		Else
			If NoSideWallRelfections Then
				Primitive65.visible = 0 : Primitive61.visible = 0 : Primitive111.visible = 0 : Primitive112.visible = 0 : Primitive113.visible = 1 : Primitive114.visible = 1
			Else
				Primitive65.visible = 0 : Primitive61.visible = 1 : Primitive111.visible = 0 : Primitive112.visible = 0 : Primitive113.visible = 0 : Primitive114.visible = 0
			End If
		End If
	End If

	For Each obj In IndirectLights
		obj.FalloffPower = obj.FalloffPower * 2
		obj.IntensityScale = 3
		obj.FadeSpeedUp = obj.FadeSpeedUp * 3
		obj.FadeSpeedDown = obj.FadeSpeedDown * 3
	Next

	' adjust timerinterval to user settting
	Flasherlight24.TimerInterval = FlasherTimerInterval
	Flasherlight23.TimerInterval = FlasherTimerInterval
	FlasherFlash22.TimerInterval = FlasherTimerInterval
	Flasherlight21.TimerInterval = FlasherTimerInterval
	Flasherlight20.TimerInterval = FlasherTimerInterval
	FlasherFlash19.TimerInterval = FlasherTimerInterval
	FlasherFlash18.TimerInterval = FlasherTimerInterval
	Flasherlight17.TimerInterval = FlasherTimerInterval

	Options_Load

End Sub

'**********
' Keys
'**********

' PlaySound "name",loopcount,volume,pan,randompitch,pitch,useexisting,restart,fade  - Y position added in VPX 10.4.
' pitch can be positive or negative and directly adds onto the standard sample frequency

Sub Table1_KeyDown(ByVal Keycode)
	If keycode = StartGameKey Then
		SoundStartButton
	End If
	If keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
		PinCab_LeftFlipperButton.X = PinCab_LeftFlipperButton.X + 10
	End If
	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		PinCab_RightFlipperButton.X = PinCab_RightFlipperButton.X - 10
		FlipperActivate RightFlipper1, RFPress1
	End If
	If keycode = PlungerKey Then
		Plunger.Pullback
		SoundPlungerPull
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If
	If keycode = LeftTiltKey Then
		Nudge 90, 1
		SoundNudgeLeft
	End If
	If keycode = RightTiltKey Then
		Nudge 270, 1
		SoundNudgeRight
	End If
	If keycode = CenterTiltKey Then
		Nudge 0, 1
		SoundNudgeCenter
	End If
	If keycode = MechanicalTilt Then
		SoundNudgeCenter() 'Send the Tilting command to the ROM (usually by pulsing a Switch), or run the tilting code for an orginal table
	End If
	If keycode = AddCreditKey Or keycode = AddCreditKey2 Then
		Select Case Int(Rnd * 3)
			Case 0
				PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1
				PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2
				PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
	If keycode = KeyRules Then Rules
	' *** manual Ball Control ***
	If debugballs Then
		if keycode = 46 then If contball = 1 Then contball = 0 : BallControl.Enabled = False Else contball = 1 : BallControl.Enabled = True: End If : End If ' C Key
		if keycode = 48 then If bcboost = 1 Then bcboost = bcboostmulti Else bcboost = 1 : End If : End If 'B Key
		if keycode = 203 then bcleft = 1        ' Left Arrow
		if keycode = 200 then bcup = 1          ' Up Arrow
		if keycode = 208 then bcdown = 1        ' Down Arrow
		if keycode = 205 then bcright = 1       ' Right Arrow
	End If
	If bInOptions Then
		Options_KeyDown keycode
		Exit Sub
	End If
	If keycode = LeftMagnaSave Then
		If bOptionsMagna Then Options_Open() Else bOptionsMagna = True
    ElseIf keycode = RightMagnaSave Then
		If bOptionsMagna Then Options_Open() Else bOptionsMagna = True
	End If
	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal Keycode)
	If keycode = LeftFlipperKey Then
		FlipperDeActivate LeftFlipper, LFPress
		PinCab_LeftFlipperButton.X = PinCab_LeftFlipperButton.X - 10
	End If
	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		PinCab_RightFlipperButton.X = PinCab_RightFlipperButton.X + 10
		FlipperDeActivate RightFlipper1, RFPress1
	End If
	If KeyCode = PlungerKey Then
		Plunger.Fire
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		VR_Primary_plunger.Y = -65
		If BIPL = 1 Then
			SoundPlungerReleaseBall()   'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall() 'Plunger release sound when there is no ball in shooter lane
		End If
	End If
	' *** manual Ball Control ***
	If debugballs Then
		if keycode = 203 then bcleft = 0        ' Left Arrow
		if keycode = 200 then bcup = 0          ' Up Arrow
		if keycode = 208 then bcdown = 0        ' Down Arrow
		if keycode = 205 then bcright = 0       ' Right Arrow
	End If
	If keycode = LeftMagnaSave And Not bInOptions Then bOptionsMagna = False
    If keycode = RightMagnaSave And Not bInOptions Then bOptionsMagna = False
	If vpmKeyUp(keycode) Then Exit Sub
End Sub

' *** Manual Ball Control ***
Sub StartControl_Hit() : WireRampOn False : Set ControlBall = ActiveBall : contballinplay = true : End Sub
Sub StopControl_Hit() : contballinplay = false : End Sub
 
Dim bcup, bcdown, bcleft, bcright, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti
 
bcboost = 1     		'Do Not Change - default setting
bcvel = 4       		'Controls the speed of the ball movement
bcyveloffset = -0.01    'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3    	'Boost multiplier to ball veloctiy (toggled with the B key)
 
Sub BallControl_Timer()
    If Contball and ContBallInPlay then
        If bcright = 1 Then ControlBall.velx = bcvel*bcboost Else If bcleft = 1 Then ControlBall.velx = - bcvel*bcboost Else ControlBall.velx=0 : End If : End If
        If bcup = 1 Then ControlBall.vely = -bcvel*bcboost Else If bcdown = 1 Then ControlBall.vely = bcvel*bcboost Else ControlBall.vely= bcyveloffset : End If : End If
    End If
End Sub

'*******	Set Up Backglass	***********************
Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglass
		obj.x = obj.x
		obj.height = - obj.y + 547
		obj.y = -20
		obj.rotX = -86.5
	Next
End Sub

Sub glasshit_Hit: If HitTheGlass and ActiveBall.VelY > 0 Then PlaySoundAt "glass_hit", l21b5 : Playsound "quake" : End If : End Sub

Sub Bumper1_Hit: vpmTimer.PulseSw 16 : RandomSoundBumperTop Bumper1: End Sub
Sub Bumper2_Hit: vpmTimer.PulseSw 17 : RandomSoundBumperMiddle Bumper2: End Sub
Sub Bumper3_Hit: vpmTimer.PulseSw 18 : RandomSoundBumperBottom Bumper3: End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:End Sub
Sub sw25_Unhit:Controller.Switch(25) = 0:End Sub

Sub sw26_Hit:Controller.Switch(26) = 1:End Sub
Sub sw26_Unhit:Controller.Switch(26) = 0:End Sub

Sub sw27_Hit:Controller.Switch(27) = 1:End Sub
Sub sw27_Unhit:Controller.Switch(27) = 0:End Sub

Sub sw28_Hit:Controller.Switch(28) = 1:End Sub
Sub sw28_Unhit:Controller.Switch(28) = 0:End Sub

Sub sw29_Hit:Controller.Switch(29) = 1:End Sub
Sub sw29_Unhit:Controller.Switch(29) = 0:End Sub

Sub SW43_Hit:Controller.Switch(43) = 1:End Sub
Sub SW43_Unhit:Controller.Switch(43) = 0:End Sub

Sub sw44_Hit:Controller.Switch(44) = 1:End Sub
Sub sw44_Unhit:Controller.Switch(44) = 0:End Sub

Sub sw45_Hit:Controller.Switch(45) = 1:End Sub
Sub sw45_Unhit:Controller.Switch(45) = 0:End Sub

Sub sw46_Hit:Controller.Switch(46) = 1:End Sub 
Sub sw46_Unhit:Controller.Switch(46) = 0:End Sub

Sub sw47_Hit:Controller.Switch(47) = 1:End Sub 
Sub sw47_Unhit:Controller.Switch(47) = 0:End Sub
 
Sub sw48_Hit:Controller.Switch(48) = 1  End Sub 
Sub sw48_Unhit:Controller.Switch(48) = 0 : End Sub

Sub swPlunger_Hit:Controller.Switch(53) = 1 : BIPL = True : End Sub   
Sub swPlunger_UnHit:Controller.Switch(53) = 0 : BIPL = False : End Sub 

Sub sw54_Hit:vpmTimer.PulseSw 54:RubberBand2.visible = 0::RubberBand2a.visible = 1:sw54.timerenabled = 1:End Sub
Sub sw54_timer:RubberBand2.visible = 1::RubberBand2a.visible = 0: sw54.timerenabled= 0:End Sub

Sub sw55_Hit:vpmTimer.PulseSw 55:End Sub

Sub sw57_Hit:Controller.Switch(57) = 1:End Sub
Sub sw57_Unhit:Controller.Switch(57) = 0:End Sub

Sub sw58_Hit:Controller.Switch(58) = 1:End Sub
Sub sw58_Unhit:Controller.Switch(58) = 0:End Sub

Dim popped : popped = true
Sub sw61_hit() : Controller.Switch(61) = 1 : popped = true : PlaySoundAt "BallFallInGun", ActiveBall : End Sub
Sub KickPopper(Enabled)
'	If Scoop_Difficulty = 1 Then
'		'sw61.Kick 305,50 + rnd * 3,0
'	End If
'	If Scoop_Difficulty = 2 Then
'		'sw61.Kick 305,50 + rnd * 6,0
'	End If
'	If Scoop_Difficulty = 3 Then
		sw61.Kick 295,60.5 + rnd * 2,0
	'End If
	Controller.Switch(61) = 0
	If popped Then Playsound "vuk" : popped = false : End If
End Sub

Sub FallThrough2_Hit: vpmTimer.PulseSw 62 : RandomSoundDelayedBallDropOnPlayfield ActiveBall : End Sub

' *** three ball lock ***
Dim vukked : vukked = true
Sub sw65_Hit() : Controller.Switch(65) = 1 : ActiveBall.VelX = 2 : PlaySoundAt "kicker_enter", ActiveBall : End Sub
Sub sw65_unHit() : Controller.Switch(65) = 0 : End Sub
Sub sw64_Hit() : Controller.Switch(64) = 1 : multiballwall65.isDropped = false : ActiveBall.VelX = 2 : End Sub
Sub sw64_unHit() : Controller.Switch(64) = 0 : vpmTimer.AddTimer 500, "multiballwall65.isDropped = true'" : End Sub
Sub sw63_hit() : Controller.Switch(63) = 1 : multiballwall64.isDropped = false : vukked = true : End Sub

Sub KickBallUp(Enabled) : sw63.Kick 0,100,1.50 : If vukked Then PlaySoundAt "vuk", sw63 : vpmTimer.AddTimer 300, "ballrampdropsound sw63'" : vukked = false : end if : Controller.Switch(63) = 0 : vpmTimer.AddTimer 500, "multiballwall64.isDropped = true'" : End Sub
Sub ballrampdropsound(tableobj): PlaySoundAt "fx_ballrampdrop", tableobj : End Sub

Sub sw66_Hit : Controller.Switch(66) = 1 : WireRampOff : End Sub
Sub sw66_Unhit:Controller.Switch(66) = 0 : WireRampOn True : End Sub

Sub sw68_Hit:Controller.Switch(68) = 1:End Sub
Sub sw68_Unhit:Controller.Switch(68) = 0:End Sub

Sub sw71_Hit:Controller.Switch(71) = 1:End Sub 
Sub sw71_Unhit:Controller.Switch(71) = 0: end sub

Sub sw75_Hit:Controller.Switch(75) = 1 : End Sub
Sub sw75_Unhit:Controller.Switch(75) = 0 : End Sub

Sub plungerfallback_hit : If ActiveBall.VelY > 0 Then PlaySoundAtVol "rubber",ActiveBall,ActiveBall.VelY / 50 : WireRampOff : End If : End Sub
Sub wireguidedrop_Hit : RandomSoundDelayedBallDropOnPlayfield ActiveBall:  end sub

Sub TiltSol(Enabled) : FlippersEnabled = Enabled : SolLFlipper(False) : SolRFlipper(False) : End Sub

Sub ramptrigger01_hit()
	WireRampOn True	 'Play Plastic Ramp Sound
	'bsRampOnClear	   'Shadow on ramp and pf below
End Sub

Sub ramptrigger01_unhit()
	If ActiveBall.VelY > 0 Then WireRampOff : End If
End Sub

Sub ramptrigger02_unhit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
	'bsRampOnWire	'Shadow only on pf
End Sub

Sub ramptrigger03_hit()
	WireRampOn True	 'Play Plastic Ramp Sound
	'bsRampOnClear	   'Shadow on ramp and pf below
End Sub

Sub ramptrigger03_unhit()
	If ActiveBall.VelY > 0 Then WireRampOff : End If
End Sub

Sub ramptrigger04_unhit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
	'bsRampOnWire	'Shadow only on pf
End Sub

Sub ramptrigger05_hit()
	WireRampOn True	 'Play Plastic Ramp Sound
	'bsRampOnClear	   'Shadow on ramp and pf below
End Sub

Sub ramptrigger05_unhit()
	If ActiveBall.VelY > 0 Then WireRampOff : End If
End Sub

Sub ramptrigger06_unhit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
	'bsRampOnWire	'Shadow only on pf
End Sub

Sub ramptrigger07_hit()
	WireRampOn True	 'Play Plastic Ramp Sound
	'bsRampOnClear	   'Shadow on ramp and pf below
End Sub

Sub ramptrigger07_unhit()
	If ActiveBall.VelY > 0 Then WireRampOff : End If
End Sub

Sub ramptrigger08_unhit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
	'bsRampOnWire	'Shadow only on pf
End Sub

Sub ramptrigger09_hit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

'Sub ramptrigger09_unhit()
'	If ActiveBall.VelY > 0 Then WireRampOff : End If
'End Sub

Sub ramptrigger010_hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
End Sub

Sub ramptrigger011_hit()
	WireRampOn True	 'Play Plastic Ramp Sound
	'bsRampOnClear	   'Shadow on ramp and pf below
End Sub

Sub ramptrigger012_unhit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
	'bsRampOnWire	'Shadow only on pf
End Sub

'******************************************************
'	ZRST: STAND-UP TARGETS by Rothbauerw
'******************************************************

'Sub switch31_Hit : If Primitive_Target8.ObjRotY = 0 Then TargetBouncer ActiveBall, 1 : Controller.Switch(31) = 1 : Primitive_TargetParts2.ObjRotY = -1 : Primitive_Target8.ObjRotY = -1 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch31_timer : Me.TimerEnabled = 0 : Primitive_Target8.ObjRotY = 0 : Primitive_TargetParts2.ObjRotY = 0 : Controller.Switch(31) = 0 : end sub
'
'Sub switch32_Hit : If Primitive_Target2.ObjRotY = 0 Then TargetBouncer ActiveBall, 1 : Controller.Switch(32) = 1 : Primitive_TargetParts1.ObjRotY = -1 : Primitive_Target2.ObjRotY = -1 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch32_timer : Me.TimerEnabled = 0 : Primitive_Target2.ObjRotY = 0 : Primitive_TargetParts1.ObjRotY = 0 : Controller.Switch(32) = 0 : end sub
'
'Sub switch33_Hit : If Primitive_Target3.ObjRotY = 0 Then TargetBouncer ActiveBall, 1 : Controller.Switch(33) = 1 : Primitive_TargetParts3.ObjRotY = -1 : Primitive_Target3.ObjRotY = -1 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch33_timer : Me.TimerEnabled = 0 : Primitive_Target3.ObjRotY = 0 : Primitive_TargetParts3.ObjRotY = 0 : Controller.Switch(33) = 0 : end sub
'
'Sub switch34_Hit : If Primitive_Target4.ObjRotY = 0 Then TargetBouncer ActiveBall, 1 : Controller.Switch(34) = 1 : Primitive_TargetParts4.ObjRotY = -1 : Primitive_Target4.ObjRotY = -1 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch34_timer : Me.TimerEnabled = 0 : Primitive_Target4.ObjRotY = 0 : Primitive_TargetParts4.ObjRotY = 0 : Controller.Switch(34) = 0 : end sub
'
'Sub switch35_Hit : If Primitive_Target5.ObjRotY = 0 Then TargetBouncer ActiveBall, 1 : Controller.Switch(35) = 1 : Primitive_TargetParts5.ObjRotY = -1 : Primitive_Target5.ObjRotY = -1 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch35_timer : Me.TimerEnabled = 0 : Primitive_Target5.ObjRotY = 0 : Primitive_TargetParts5.ObjRotY = 0 : Controller.Switch(35) = 0 : end sub
'
'Sub switch36_Hit : If Primitive13.RotZ = 0 Then TargetBouncer ActiveBall, 0.7 : Controller.Switch(36) = 1 : Primitive13.RotZ = -4 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch36_timer : Me.TimerEnabled = 0 : Primitive13.RotZ = 0 : Controller.Switch(36) = 0 : end sub
'
'Sub switch37_Hit : If Primitive12.RotZ = 0 Then TargetBouncer ActiveBall, 0.7 : Controller.Switch(37) = 1 : Primitive12.RotZ = -4 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch37_timer : Me.TimerEnabled = 0 : Primitive12.RotZ = 0 : Controller.Switch(37) = 0 : end sub
'
'Sub switch38_Hit : If Primitive11.RotZ = 0 Then TargetBouncer ActiveBall, 0.7 : Controller.Switch(38) = 1 : Primitive11.RotZ = -4 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch38_timer : Me.TimerEnabled = 0 : Primitive11.RotZ = 0 : Controller.Switch(38) = 0 : end sub
'
'Sub switch41_Hit : If Primitive_Target7.ObjRotX = 0 Then TargetBouncer ActiveBall, 1 : Controller.Switch(41) = 1 : Primitive_TargetParts8.ObjRotX = 1 : Primitive_Target7.ObjRotX = 1 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch41_timer : Me.TimerEnabled = 0 : Primitive_Target7.ObjRotX = 0 : Primitive_TargetParts8.ObjRotX = 0 : Controller.Switch(41) = 0 : end sub
'
'Sub switch42_Hit : If Primitive_Target6.ObjRotX = 0 Then TargetBouncer ActiveBall, 1 : Controller.Switch(42) = 1 : Primitive_TargetParts7.ObjRotX = 1 : Primitive_Target6.ObjRotX = 1 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch42_timer : Me.TimerEnabled = 0 : Primitive_Target6.ObjRotX = 0 : Primitive_TargetParts7.ObjRotX = 0 : Controller.Switch(42) = 0 : end sub
'
'Sub switch56_Hit : If Primitive_Target1.ObjRotY = 0 Then Controller.Switch(56) = 1 : Primitive_TargetParts6.ObjRotY = 1 : Primitive_TargetParts6.ObjRotX = 1 : Primitive_Target1.ObjRotX = 1: Primitive_Target1.ObjRotY = 1 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch56_timer : Me.TimerEnabled = 0 : Primitive_Target1.ObjRotY = 0 : Primitive_TargetParts6.ObjRotX = 0 : Primitive_Target1.ObjRotX = 0 : Primitive_TargetParts6.ObjRotY = 0 : Controller.Switch(56) = 0 : end sub
'
'Sub switch73_Hit : If Primitive15.RotZ = 0 Then Controller.Switch(73) = 1 : Primitive15.RotZ = -4 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch73_timer : Me.TimerEnabled = 0 : Primitive15.RotZ = 0 : Controller.Switch(73) = 0 : end sub
'
'Sub switch74_Hit : If Primitive14.RotZ = 0 Then Controller.Switch(74) = 1 : Primitive14.RotZ = -4 : Me.TimerEnabled = 1 : End If : End Sub
'Sub switch74_timer : Me.TimerEnabled = 0 : Primitive14.RotZ = 0 : Controller.Switch(74) = 0 : end sub

Sub sw31_hit:STHit 31:End Sub
Sub sw32_hit:STHit 32:End Sub
Sub sw33_hit:STHit 33:End Sub
Sub sw34_hit:STHit 34:End Sub
Sub sw35_hit:STHit 35:End Sub
Sub sw36_hit:STHit 36:End Sub
Sub sw37_hit:STHit 37:End Sub
Sub sw38_hit:STHit 38:End Sub
Sub sw41_hit:STHit 41:End Sub
Sub sw42_hit:STHit 42:End Sub
Sub sw56_hit:STHit 56:End Sub
Sub sw73_hit:STHit 73:End Sub
Sub sw74_hit:STHit 74:End Sub


Class StandupTarget
  Private m_primary, m_prim, m_sw, m_animate

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public default Function init(primary, prim, sw, animate)
    Set m_primary = primary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate

    Set Init = Me
  End Function
End Class

'Define a variable for each stand-up target
Dim ST31, ST32, ST33, ST34, ST35, ST36, ST37, ST38, ST41, ST42, ST56, ST73, ST74

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
'   primary:	vp target to determine target hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!!
'				   transy must be used to offset the target animation
'   switch:	 ROM switch number
'   animate:	Arrary slot for handling the animation instrucitons, set to 0
'
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts

Set ST31 = (new StandupTarget)(sw31, sw31p,31, 0)
Set ST32 = (new StandupTarget)(sw32, sw32p,32, 0)
Set ST33 = (new StandupTarget)(sw33, sw33p,33, 0)
Set ST34 = (new StandupTarget)(sw34, sw34p,34, 0)
Set ST35 = (new StandupTarget)(sw35, sw35p,35, 0)
Set ST36 = (new StandupTarget)(sw36, sw36p,36, 0)
Set ST37 = (new StandupTarget)(sw37, sw37p,37, 0)
Set ST38 = (new StandupTarget)(sw38, sw38p,38, 0)
Set ST41 = (new StandupTarget)(sw41, sw41p,41, 0)
Set ST42 = (new StandupTarget)(sw42, sw42p,42, 0)
Set ST56 = (new StandupTarget)(sw56, sw56p,56, 0)
Set ST73 = (new StandupTarget)(sw73, sw73p,73, 0)
Set ST74 = (new StandupTarget)(sw74, sw74p,74, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
'   STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST31, ST32, ST33, ST34, ST35, ST36, ST37, ST38, ST41, ST42, ST56, ST73, ST74)

'Configure the behavior of Stand-up Targets
Const STAnimStep = 1.5  'vpunits per animation step (control return to Start)
Const STMaxOffset = 9   'max vp units target moves when hit

Const STMass = 0.2	  'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)
	
	PlayTargetSound
	STArray(i).animate = STCheckHit(ActiveBall,STArray(i).primary)
	
	If STArray(i).animate <> 0 Then
		DTBallPhysics ActiveBall, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 To UBound(STArray)
		If STArray(i).sw = switch Then
			STArrayID = i
			Exit Function
		End If
	Next
End Function

Function STCheckHit(aBall, target) 'Check if target is hit on it's face
	Dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		STCheckHit = 1
	Else
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i, anyAnimating
	anyAnimating = False
	For i = 0 To UBound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
		If STArray(i).animate > 0 Then anyAnimating = True
	Next

	If Not anyAnimating Then Exit Sub

	sw31pp.transz = -sw31p.transy
	sw31pt.transz = -sw31p.transy
	sw32pp.transz = -sw32p.transy
	sw32pt.transz = -sw32p.transy
	sw33pp.transz = -sw33p.transy
	sw33pt.transz = -sw33p.transy
	sw34pp.transz = -sw34p.transy
	sw34pt.transz = -sw34p.transy
	sw35pp.transz = -sw35p.transy
	sw35pt.transz = -sw35p.transy

	sw36pt.transx = -sw36p.transy
	sw37pt.transx = -sw37p.transy
	sw38pt.transx = -sw38p.transy

	sw41pp.transz = -sw41p.transy
	sw41pt.transz = -sw41p.transy
	sw42pp.transz = -sw42p.transy
	sw42pt.transz = -sw42p.transy

	sw56pp.transz = -sw56p.transy
	sw56pt.transz = -sw56p.transy

	sw73pt.transx = -sw73p.transy
	sw74pt.transx = -sw74p.transy
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime
	
	STAnimate = animate
	
	If animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If animate = 1 Then
		primary.collidable = 0
		prim.transy =  - STMaxOffset
		vpmTimer.PulseSw switch
		STAnimate = 2
		Exit Function
	ElseIf animate = 2 Then
		prim.transy = prim.transy + STAnimStep
		If prim.transy >= 0 Then
			prim.transy = 0
			primary.collidable = 1
			STAnimate = 0
			Exit Function
		Else
			STAnimate = 2
		End If
	End If
End Function

Sub DTBallPhysics(aBall, angle, mass)
	Dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	
	calc1 = cor.BallVel(aball.id) * Cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Cos(rangle + 4 * Atn(1) / 2)
	calc3 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Sin(rangle + 4 * Atn(1) / 2)
	
	aBall.velx = calc1 * Cos(rangle) + calc2
	aBall.vely = calc1 * Sin(rangle) + calc3
End Sub


'******************************************************
'****   END STAND-UP TARGETS
'******************************************************

' *****************************************
'********** Sling Shot Animations *********
' *****************************************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 52
	RandomSoundSlingshotRight Sling1
    RSling.Visible = 0 : RSling1.Visible = 1 : sling1.TransZ = -20 : RStep = 0 : RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 0:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 2:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 51
	RandomSoundSlingshotLeft Sling2
    LSling.Visible = 0 : LSling1.Visible = 1 : sling2.TransZ = -20 : LStep = 0 : LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 0:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 2:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub

'*******************************  Solenoids **************************
'*** flipper solenoids are disabled for faster response in KeyDown ***
'*********************************************************************

'SolCallback(1) = "kisort"    				' Outhole
SolCallback(2) = "SolRelease"   		' Ball Release
SolCallback(3) = "KickPopper"  				' Whirlpool Popper
SolCallback(4) = "KickBallUp"    			' Lockup Popper
SolCallback(5) = "SolKick"          		' Kickback
SolCallback(6) = "SolDiv"           		' Ramp Diverter
SolCallback(7) = "vpmSolSound ""Knocker"","
SolCallback(25) = "SolBigFootDrive"  		' Bigfoot Drive
SolCallback(26) = "SolBigFootEnable" 		' Bigfoot Enable
SolCallback(31)="TiltSol" 					' 31 for WPC
SolModCallback(8) = "Flasherset8" 			' BG - All Riders
SolModCallback(9) = "Flasherset9" 			' BG - Willy
SolModCallback(15) = "Flasherset15" 		' BG FrontRaft
SolModCallback(16) = "Flasherset16" 		' BG Riders
SolModCallback(17) = "Flasherset17" 		' Bigfoot Body
SolModCallback(18) = "Flasherset18" 		' Right Mountains
SolModCallback(19) = "Flasherset19" 		' Left Mountains
SolModCallback(20) = "Flasherset20" 		' Upper Left Playfield
SolModCallback(21) = "Flasherset21" 		' Insanity Falls
SolModCallback(22) = "Flasherset22" 		' Whirlpool Popper
SolModCallback(23) = "Flasherset23" 		' Whirlpool Enter
SolModCallback(24) = "Flasherset24" 		' Bigfoot cave

'fliptronic board
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sURFlipper) = "SolURFlipper"

'******************************************************
'						TROUGH 
'******************************************************

Sub sw76_Hit():Controller.Switch(76) = 1:UpdateTrough:End Sub
Sub sw76_UnHit():Controller.Switch(76) = 0:UpdateTrough:End Sub
Sub sw77_Hit():Controller.Switch(77) = 1:UpdateTrough:End Sub
Sub sw77_UnHit():Controller.Switch(77) = 0:UpdateTrough:End Sub
Sub sw78_Hit():Controller.Switch(78) = 1:UpdateTrough:End Sub
Sub sw78_UnHit():Controller.Switch(78) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw76.BallCntOver = 0 Then sw77.kick 60, 9
	If sw77.BallCntOver = 0 Then sw78.kick 60, 9
	Me.Enabled = 0
End Sub

'******************************************************
'					DRAIN & RELEASE
'******************************************************

Sub Drain_Hit() 
	RandomSoundDrain Drain
	UpdateTrough
	vpmTimer.AddTimer 500, "Drain.kick 70, 30'"
	controller.switch(15) = 1
	BIP = BIP - 1
End Sub

Sub SolRelease(enabled)
	If enabled Then 
		vpmTimer.PulseSw 15
		sw76.kick 60, 10		
		RandomSoundBallRelease sw76
		Controller.Switch(76) = 0
		BIP = BIP + 1
	End If
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''  Ball Trough system ''''''''''''''''''''''''''
'''''''''''''''''''''by cyberpez''''''''''''''''''''''''''''''''
''''''''''''''''based off of EalaDubhSidhe's''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'Dim BallCount, cBall1, cBall2, cBall3, MaxBalls : MaxBalls = 3

'CheckMaxBalls() : BallCount = MaxBalls : TroughWall1.isDropped = true : TroughWall2.isDropped = true : multiballwall65.isDropped = true : multiballwall64.isDropped = true : End Sub

'Sub CreatBalls_timer()
'	If BallCount > 0 then
'		If BallCount = 3 Then Set cBall1 = drain.CreateSizedBallWithMass(BallSize / 2, BallMass) : End If
'		If BallCount = 2 Then Set cBall2 = drain.CreateSizedBallWithMass(BallSize / 2, BallMass) : End If
'		If BallCount = 1 Then Set cBall3 = drain.CreateSizedBallWithMass(BallSize / 2, BallMass) : End If		
'		Drain.kick 70,30
'		BallCount = BallCount - 1
'	Else CreatBalls.enabled = false : metalvolume = 1 : End If
'End Sub	
	
'Sub ballrelease_hit() : Controller.Switch(76)=1 : TroughWall1.isDropped = false : End Sub
'Sub sw77_Hit() : Controller.Switch(77)=1 : TroughWall2.isDropped = false : End Sub
'Sub sw77_unHit() : Controller.Switch(77)=0 : TroughWall2.isDropped = true : End Sub
'Sub sw78_Hit() : Controller.Switch(78)=1 : End Sub
'Sub sw78_unHit() : Controller.Switch(78)=0 : End Sub
'sub kisort(enabled) : Drain.Kick 70,30 : controller.switch(15) = 0 : end sub
'Sub Drain_hit()
'	RandomSoundDrain Drain
'	controller.switch(15) = 1
'	BIP = BIP - 1
'End Sub

'Dim DontKickAnyMoreBalls,DKTMstep

'Sub KickBallToLane(Enabled)
'	If DontKickAnyMoreBalls = 0 then
'		BIP = BIP + 1
'		RandomSoundBallRelease BallRelease
'		ballrelease.Kick 60,10
'		TroughWall1.isDropped = true
'		Controller.Switch(76)=0
'		DontKickAnyMoreBalls = 1
'		DKTMstep = 1
'		DontKickToMany.enabled = true
'	End If
'End Sub

'Sub DontKickToMany_timer()
'	Select Case DKTMstep
'		Case 1:
'		Case 2:
'		Case 3: DontKickAnyMoreBalls = 0:DontKickToMany.Enabled = False: DontKickAnyMoreBalls = 0
'	End Select
'	DKTMstep = DKTMstep + 1
'End Sub

Sub SolKick(Enabled) : Kickback.Enabled = Enabled : End Sub
Sub Kickback_Hit() : RandomSoundBallRelease Kickback :  : Kickback.kick -0, 50 + (rnd * 8) : Kickback.Enabled = False : End Sub

'******************
'Big Foot Animation
'******************

Sub SolDiv(Enabled)
	Diverter.IsDropped = Not Enabled
	If Enabled Then up = 1 : DiverterAnimationTimer.enabled = True : Playsound "TopDiverterOn" Else  up = -1 : DiverterAnimationTimer.enabled = True : Playsound "TopDiverterOn" : End If
End Sub

dim frameCount,up : frameCount=1 : up=0

sub DiverterAnimationTimer_Timer
	frameCount = frameCount + up
	If framecount > 4 or framecount < 1 Then up = 0 : me.enabled = False : end if
	Primitive_BigFootArmFur.ShowFrame frameCount : Primitive_BigFootDiverter.ShowFrame frameCount
end sub

Dim BigDir, BigCount, BigNewPos, BigOldPos : BigDir = 1:BigOldPos = 0:BigNewPos = 0

Sub BigFoot_Init : BigOldPos = 8 : controller.switch(86) = 1 : controller.switch(87) = 0 : End Sub

Sub SolBigFootDrive(Enabled)
	If enabled Then
		PlaySoundAt SoundFX("BigFootMotor",DOFGear),Primitive_BigFoot
	Else
		StopSound "BigfootMotor"
	End If
	BigTimer.Enabled = enabled
End Sub

Sub SolBigFootEnable(Enabled) : BigDir = ABS(Enabled) : End Sub

Sub BigTimer_Timer()
	If BigDir = 1 Then BigNewPos = BigNewPos + 1 Else BigNewPos = BigNewPos - 1 End If
	If BigNewPos <0 Then BigNewPos = 95
	If BigNewPos> 95 Then BigNewPos = 0
	Primitive_BigFoot.ObjRotZ = BigNewPos * 3.75 'value is due to 96 steps for 360 degrees rotation
	Primitive_BigFootWig.ObjRotZ = Primitive_BigFoot.ObjRotZ : Primitive_BigFootWig2.ObjRotZ = Primitive_BigFoot.ObjRotZ
	If BigNewPos = 24 Then:controller.switch(86) = 1:controller.switch(87) = 0:End If  ' Left  (Diverter)
	If BigNewPos = 45 Then:controller.switch(86) = 0:controller.switch(87) = 1:End If ' Up    (Up)
	If BigNewPos = 72 Then:controller.switch(86) = 0:controller.switch(87) = 0:End If ' Right (Unknown)
	If BigNewPos = 7 Then:controller.switch(86) = 1:controller.switch(87) = 1:End If  ' Down  (Player)
	BigOldPos = BigNewPos
End Sub

'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper
		Else
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
	Else
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled) 'Right flipper solenoid callback
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolURFlipper(Enabled)
	If Enabled Then
		RightFlipper1.RotateToEnd	
		If rightflipper1.currentangle > rightflipper1.endangle - ReflipAngle Then
				RandomSoundReflipUpRight RightFlipper1
		Else
			SoundFlipperUpAttackRight RightFlipper1
			RandomSoundFlipperUpRight RightFlipper1
		End If
	Else
		RightFlipper1.RotateToStart
		If RightFlipper1.currentangle > RightFlipper1.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper1
			End If
			FlipperRightHitParm = FlipperUpSoundLevel
		End If
End Sub

' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

Sub RightFlipper1_Collide(parm)
	'CheckLiveCatch ActiveBall, RightFlipper1, RFCount1, parm
	RightFlipperCollide parm
End Sub

Sub FlipperVisualUpdate 'This subroutine updates the flipper shadows and visual primitives
	batleft.objrotz = LeftFlipper.CurrentAngle + 1 : batleftshadow.objrotz = batleft.objrotz
	batright.objrotz = RightFlipper.CurrentAngle - 1 : batrightshadow.objrotz  = batright.objrotz
	batrightupper.objrotz = RightFlipper1.CurrentAngle - 1 : batrightshadowupper.objrotz  = batrightupper.objrotz
End Sub

' *****************************************
' *** insert lights, whirlpool lights *****
' *****************************************

Dim PFLights(200,3), PFLightsCount(200)

Sub InitLights(aColl)
	Dim obj, idx
	For Each obj In aColl
		idx = obj.TimerInterval
		Set PFLights(idx, PFLightsCount(idx)) = obj
		PFLightsCount(idx) = PFLightsCount(idx) + 1
	Next
End Sub

Sub LampTimer_Timer()
	Dim chgLamp, num, chg, ii, nr
    chgLamp = Controller.ChangedLamps
	If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
			select case chglamp(ii,0)
				case 12 : PFLights(chglamp(ii,0),0).state = chglamp(ii,1) : PFLights(chglamp(ii,0),1).state = chglamp(ii,1) : Flasher12.visible = chglamp(ii,1)
				case 13 : PFLights(chglamp(ii,0),0).state = chglamp(ii,1) : PFLights(chglamp(ii,0),1).state = chglamp(ii,1) : Flasher13.visible = chglamp(ii,1)
				case 17 : PFLights(chglamp(ii,0),0).state = chglamp(ii,1) : If chglamp(ii,1) = 1 Then upf_yellow_light = 8	Else upf_yellow_light = 7 End If
				case 52 : PFLights(chglamp(ii,0),0).state = chglamp(ii,1) : PFLights(chglamp(ii,0),1).state = chglamp(ii,1) : Flasher52.visible = chglamp(ii,1)
				case 55 : PFLights(chglamp(ii,0),0).state = chglamp(ii,1) : PFLights(chglamp(ii,0),1).state = chglamp(ii,1) : If chglamp(ii,1) = 1 Then upf_red_light = 8	Else upf_red_light = 7 End If
				case 71,72,73,74,75,76 : PFLights(chglamp(ii,0),0).state = chglamp(ii,1) : If chglamp(ii,1) = 1 Then whirlpoollight(chglamp(ii,0) - 71) = 8 Else whirlpoollight(chglamp(ii,0) - 71) = 7 : End If
				case else For nr = 1 to PFLightsCount(chglamp(ii,0)) : PFLights(chglamp(ii,0),nr - 1).state = chglamp(ii,1) : Next
			end select
        Next
	End If
	Rollingupdate()
End Sub

dim whirlpoollight(6), upf_red_light, upf_yellow_light

Sub ImageLights_Timer()
	dim obj, idx
	For Each obj in whirlpoolbulb
		idx = obj.DepthBias - 71
		If whirlpoollight(idx) > 0 Then
			If whirlpoollight(idx) = 8 Then 
				obj.blenddisablelighting = 2
			Else 
				whirlpoollight(idx) = whirlpoollight(idx) - 1 : obj.blenddisablelighting = whirlpoollight(idx) / 4 - 0.2
			End If
		End If
	Next
	If upf_red_light > 0 Then
		If upf_red_light = 8 Then 
			Primitive100.blenddisablelighting = 1
		Else 
			upf_red_light = upf_red_light - 1 : Primitive100.blenddisablelighting = upf_red_light / 7 + 0.3
		End If
	End If
	If upf_yellow_light > 0 Then
		If upf_yellow_light = 8 Then 
			Primitive99.blenddisablelighting = 2
		Else 
			upf_yellow_light = upf_yellow_light - 1 : Primitive99.blenddisablelighting = upf_yellow_light / 20
		End If
	End If
End Sub

' ***************************************************************************************************************************
' ***             										Update GI           											*****
' ***     	Only WPC games have special GI circuit, Callback function must take two arguments							*****
' *** 		StringNo: GIString that has changed state (0-4) 															*****
' ***		for White Water: 0 = upper pf, 1 = middle pf, 2 = lower pf, 3 = backbox sky, 4 = back boat					*****
' *** 		Status: New status of GI string (0-8)																		*****
' ***************************************************************************************************************************

Sub UpdateGI(no, step)
	If DisableGI Then step = 0
	Select Case no
		case 0 : GIUpperFade(step) 
		case 1 : GIMiddleFade(step)
		case 2 : GILowerFade(step)
		case 3 : BGBoatFlash(step) 
		case 4 : BGSkyFlash(Step)
	End Select
End Sub

Sub BGSkyFlash(step)
	Dim obj
	For Each obj In VRBackglassSky : Obj.IntensityScale = step / 16 : Next
	If step = 0 Then
		BGBright.opacity = 0
	Else
		BGBright.opacity = 25
	End If
End Sub

Sub BGBoatFlash(step)
	Dim obj
	For Each obj In VRBackglassBoat : Obj.IntensityScale = step / 16 : Next
	If step = 0 Then
		BGBright.opacity = 0
	Else
		BGBright.opacity = 25
	End If
End Sub

Primitive5temp.material = "rampsGILower"

Sub GILowerFade(step)
	Dim obj
	For Each obj In GILower : Obj.state = step/8 : Next
End Sub

Sub GILowerLight_Animate
	Dim s
	s = GILowerLight.GetInPlayIntensity / GILowerLight.Intensity
'	debug.print "GILowerFade step="&step&" s="&s
	Primitive39.blenddisablelighting = s/2
	Primitive40.blenddisablelighting = s/2
'	Primitive5temp.material = "rampsGI" & step
'	SetMaterialGlossCoatGreyscale "rampsGILower",s*0.5+0.5
	Flasherbase22.BlendDisableLighting =  s
End Sub

Primitive5temp7.material = "rampsGIMiddle"
Primitive5temp2.material = "rampsGIMiddle"
Rock2_Boulder_garden.material = "rockGI"
Rock1_Lower_popbumper.material = "rockGI"
Rock3_Rightpopbumper.material = "rockGI"

Sub GIMiddleFade(step)
	Dim obj
	For Each obj In GImiddle : Obj.State = step/8 : Next
	Bulb17.image = "simplelightwhite" & step
End Sub

Sub GIMiddleLight_Animate
	Dim s
	s = GIMiddleLight.GetInPlayIntensity / GIMiddleLight.Intensity
	Flasher7.opacity = s*8 * 25 : Flasher8.opacity = s*8 * 25 : Flasher9.opacity = s*8 * 10 : Flasher10.opacity = s*8 * 500

	sw33pt.blenddisablelighting = s*4
	sw34pt.blenddisablelighting = s*4 : sw35pt.blenddisablelighting = s*4 : sw31pt.blenddisablelighting = s*4
	sw32pt.blenddisablelighting = s*4
	Primitive57.blenddisablelighting = 0.2 + 0.2 * s
End Sub

Dim GIUp ' needed for using a 2 VPX flasher objects for GI as well as an actual flasher
Primitive5temp5.material = "rampsGIUpper"
Primitive5temp3.material = "rampsGIUpper"
Primitive5temp8.material = "rampsGIUpper"

Sub GIUpperFade(step)
	Dim obj
	GIUp = step
	For Each obj In GIupper : Obj.state = step/8 : Next
	For Each obj In GIupperbulbs : Obj.image = "simplelightwhite" & step : Next
End Sub

Sub GIUpperLight_Animate
	Dim s
	s = GIupper(0).GetInPlayIntensity / GIupper(0).Intensity
	If Flashlevel20 < 0.01 Then Flasher5.opacity = s*8 * 300 : Flasher6.opacity = s*8 * 300 : end if
	Flasher1.opacity = s*8 * 50 : Flasher2.opacity = s*8 * 20
'	Primitive5temp5.material = "rampsGI" & step : Primitive5temp3.material = "rampsGI" & step : Primitive5temp8.material = "rampsGI" & step 
'	SetMaterialGlossCoatGreyscale "rampsGIUpper",s*0.5+0.5
End Sub

Sub SetMaterialBaseGreyscale(name, val)
	if val < 0 then val = 0
	if val > 1 then val = 1
	'First get the existing material properties
    Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	'Create new grayscale color 
	Dim new_base: new_base = RGB(255*val, 255*val, 255*val)
    UpdateMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, new_base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub

Sub SetMaterialGlossCoatGreyscale(name, val)
	if val < 0 then val = 0
	if val > 1 then val = 1
	'First get the existing material properties
    Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	'Create new grayscale color 
	Dim new_glossy: new_glossy = RGB(255*val, 255*val, 255*val)
	Dim new_clearcoat: new_clearcoat = RGB(255*val, 255*val, 255*val)
    UpdateMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, new_glossy, new_clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub

'UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
'               OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
'               float elasticity, float elasticityFalloff, float friction, float scatterAngle)


' *****************************************
' ***           Flasher subs          *****
' *****************************************

' Flasher state tracking variables (Prevents GPU driver pipeline stalls)
Dim last_matdim8, last_matdim9, last_matdim15, last_matdim16
Dim last_matdim17, last_matdim18, last_matdim19, last_matdim20
Dim last_matdim21, last_matdim22, last_matdim23, last_matdim24

last_matdim8 = -1  : last_matdim9 = -1  : last_matdim15 = -1 : last_matdim16 = -1
last_matdim17 = -1 : last_matdim18 = -1 : last_matdim19 = -1 : last_matdim20 = -1
last_matdim21 = -1 : last_matdim22 = -1 : last_matdim23 = -1 : last_matdim24 = -1

Dim FlashLevel8, Flashlevel9, Flashlevel15, Flashlevel16
Dim FlashLevel17, FlashLevel18, FlashLevel19, FlashLevel20, FlashLevel21, FlashLevel22, FlashLevel23, FlashLevel24
FlasherLight18.state = 0 : FlasherLight19.state = 0 : FlasherLight22.state = 0 : FlasherLight17.state = 0 : FlasherLight17b.state = 0
FlasherLight20.state = 0 : FlasherLight21.state = 0 : FlasherLight23.state = 0 : FlasherLight24.state = 0

Sub FlasherClick(oldvalue, newvalue) : If oldvalue < 0 and newvalue > 0.2 Then PlaySound "fx_relay_on",0,0.1: End If : End Sub

' ********* Bigfoot body flasher **********
Sub Flasherset17(value) : FlasherClick FlashLevel17, value : If value < 160 Then value = 160 : End If : If value > Flashlevel17 * 255 Then FlashLevel17 = value / 255 : Flasherlight17_Timer : End If : End Sub 
Sub Flasherlight17_Timer()
	If not Flasherlight17.TimerEnabled Then
		Flasherlight17.state = 1 : Flasherlight17b.state = 1 : FlasherFlash17.visible = 1
		Flasherlight17.TimerEnabled = True
	End If
	dim flashx3 : flashx3 = FlashLevel17^2
	Flasherlight17.IntensityScale = 50 * flashx3 : Flasherlight17b.IntensityScale = 10 * flashx3 : FlasherFlash17.opacity = 5000 * flashx3
	FlashLevel17 = FlashLevel17 * 0.8 - 0.01
	If FlashLevel17 < 0 Then
		FlasherFlash17.visible = 0 : Flasherlight17.state = 0 : Flasherlight17b.state = 0
		Flasherlight17.TimerEnabled = False
	End If
End Sub

' ********* Right Mountain flasher ********
Sub FlasherFlash18_Timer()
	Dim flashx3, matdim : flashx3 = FlashLevel18^3
	If Not Flasherflash18.TimerEnabled Then 
		Flasherflash18.visible = 1 : Flasher18.visible = 1 : Flasher19.visible = 1 : 
		Flasherlit18.visible = 1 : Rock4_Back_wall2.visible = 1 : FlasherLight18.state = 1
		Flasherflash18.TimerEnabled = True
	End If
	Flasherflash18.opacity = 250 * flashx3^0.8
	Flasherlit18.BlendDisableLighting = 4 * FlashLevel18^0.5
	Flasherbase18.BlendDisableLighting = flashx3
	Flasherlight18.IntensityScale = 2 * flashx3
	Flasher19.opacity = 300000 * Flashx3
	Flasher18.opacity = 100000 * Flashx3

	' State-tracked material swap
	matdim = Round(10 * FlashLevel18)
'	If matdim <> last_matdim18 Then
'		Flasherlit18.material = "domelit" & matdim
'		Rock4_Back_wall2.material = "domelit" & matdim
'		last_matdim18 = matdim
'	End If

	FlashLevel18 = FlashLevel18 * 0.8 - 0.01
	If FlashLevel18 < 0 Then 
		Flasherflash18.visible = 0 : Flasher18.visible = 0 : Flasher19.visible = 0
		Flasherlit18.visible = 0 : Rock4_Back_wall2.visible = 0 : FlasherLight18.state = 0
		last_matdim18 = -1
		Flasherflash18.TimerEnabled = False
	End If
End Sub

' ******* Left Mountain flasher ************
Sub Flasherset19(value) : FlasherClick FlashLevel19, value : If value < 160 Then value = 160 : End If : If value > Flashlevel19 * 255 Then FlashLevel19 = value / 255 : FlasherFlash19_Timer : End If : End Sub
Sub FlasherFlash19_Timer()
	dim flashx3, matdim
	flashx3 = FlashLevel19^3
	If not Flasherflash19.TimerEnabled Then
		Flasherflash19.visible = 1 : Flasher15.visible = 1 : Flasher17.visible = 1
		Flasherlit19.visible = 1 : Rock4_Back_wall4.visible = 1 : FlasherLight19.state = 1
		Flasherflash19.TimerEnabled = True
	End If
	Flasherflash19.opacity = 250 * flashx3^0.8
	Flasherlit19.BlendDisableLighting = 4 * FlashLevel19^0.5
	Flasherbase19.BlendDisableLighting = flashx3
	Flasherlight19.IntensityScale = 2 * flashx3
	Flasher15.opacity = 100000 * Flashx3 : Flasher17.opacity = 40000 * Flashx3
	
	' State-tracked material swap
	matdim = Round(10 * FlashLevel19)
'	If matdim <> last_matdim19 Then
'		Flasherlit19.material = "domelit" & matdim
'		Rock4_Back_wall4.material = "domelit" & matdim
'		last_matdim19 = matdim
'	End If

	FlashLevel19 = FlashLevel19 * 0.8 - 0.01
	If FlashLevel19 < 0 Then
		Flasherflash19.visible = 0 : Flasher15.visible = 0 : Flasher17.visible = 0
		Flasherlit19.visible = 0 : Rock4_Back_wall4.visible = 0 : FlasherLight19.state = 0
		last_matdim19 = -1
		Flasherflash19.TimerEnabled = False
	End If
End Sub

' ****** Upper left playfield flasher ******
Sub Flasherset20(value) : FlasherClick FlashLevel20, value : If value < 160 Then value = 160 : End If : If value > Flashlevel20 * 255 Then FlashLevel20 = value / 255 : Flasherlight20_Timer : End If : End Sub 
Sub Flasherlight20_Timer()
	dim flashx3 : flashx3 = FlashLevel20^3
	If not Flasherlight20.TimerEnabled Then
		Flasherlight20.state = 1
		Flasherlight20.TimerEnabled = True
	End If
	Flasherlight20.IntensityScale = 2* flashx3: Flasher5.opacity = flashx3 * 2000 + GIUp * 300 : Flasher6.opacity = flashx3 * 2000 + GIUp * 300
	FlashLevel20 = FlashLevel20 * 0.8 - 0.01
	If FlashLevel20 < 0 Then
		Flasherlight20.state = 0
		Flasherlight20.TimerEnabled = False
	End If
End Sub

' ******** Insanity Falls flasher **********
Sub Flasherset21(value) : FlasherClick FlashLevel21, value : If value < 160 Then value = 160 : End If : If value > Flashlevel21 * 255 Then FlashLevel21 = value / 255 : Flasherlight21_Timer : End If : End Sub 
Sub Flasherlight21_Timer()
	dim flashx3 : flashx3 = FlashLevel21^3
	FlasherFlash21.opacity = 10000 * flashx3
	Flasherlight21.IntensityScale = 10 * flashx3
	Flasher16.opacity = 2000 * FlashLevel21
	FlashLevel21 = FlashLevel21 * 0.8 - 0.01
	If not Flasherlight21.TimerEnabled Then FlasherFlash21.visible = 1 : Flasher16.visible = 1 : Flasherlight21.state = 1 : Flasherlight21.TimerEnabled = True : End If
	If FlashLevel21 < 0 Then FlasherFlash21.visible = 0 : Flasher16.visible = 0 : Flasherlight21.state = 0 : Flasherlight21.TimerEnabled = False : End If
End Sub

' ******* Whirlpool popper flasher *********
Sub Flasherset22(value) : FlasherClick FlashLevel22, value : If value < 160 Then value = 160 : End If : If value > Flashlevel22 * 255 Then FlashLevel22 = value / 255 : FlasherFlash22_Timer : End If : End Sub 
Sub FlasherFlash22_Timer()
	dim flashx3, matdim : flashx3 = FlashLevel22^3
	Flasherflash22.opacity = 550 * flashx3^0.6
	Flasherlit22.BlendDisableLighting = 2 * FlashLevel22^0.5
	Flasherlight22.IntensityScale = flashx3
	Flasher11.opacity = 70000 * Flashx3
	Flasher14.opacity = 7000 * Flashx3
	Flasher20.opacity = 350 * FlashLevel22

	' State-tracked material swap
	matdim = Round(10 * FlashLevel22)
'	If matdim <> last_matdim22 Then
'		Flasherlit22.material = "domelit" & matdim
'		Rock6_lost_mine_flash.material = "domelit" & matdim
'		last_matdim22 = matdim
'	End If

	FlashLevel22 = FlashLevel22 * 0.85 - 0.01
	If not Flasherflash22.TimerEnabled Then Flasherflash22.visible = 1 : Flasher11.visible = 1 : Flasher14.visible = 1 : Flasher20.visible = 1 : Flasherlit22.visible = 1 : Rock6_lost_mine_flash.visible = 1 : Flasherlight22.state = 1 : Flasherflash22.TimerEnabled = True : End If
	If FlashLevel22 < 0 Then 
		Flasherlit22.visible = 0 
		Flasherflash22.TimerEnabled = False 
		Flasherflash22.visible = 0 
		Rock6_lost_mine_flash.visible = 0 
		Flasher20.visible = 0 
		Flasher11.visible = 0 
		Flasher14.visible = 0 
		Flasherlight22.state = 0 
		last_matdim22 = -1
	End If
End Sub

' ******* Enter Whirlpool flasher ***********
Sub Flasherset23(value) : FlasherClick FlashLevel23, value : If value < 160 Then value = 160 : End If : If value > Flashlevel23 * 255 Then FlashLevel23 = value / 255 : Flasherlight23_Timer : End If : End Sub 
Sub Flasherlight23_Timer()
	Flasherlight23.IntensityScale = FlashLevel23^3 : Primitive77.BlendDisableLighting = 2500 * FlashLevel23^3 : FlashLevel23 = FlashLevel23 * 0.8 - 0.01
	If not Flasherlight23.TimerEnabled Then Flasherlight23.state = 1 : Flasherlight23.TimerEnabled = True : End If
	If FlashLevel23 < 0 Then Flasherlight23.state = 0 : Flasherlight23.TimerEnabled = False : End If
End Sub

' ********** Bigfoot cave flasher ***********
Sub Flasherset24(value) : FlasherClick FlashLevel24, value : If value > Flashlevel24 * 255 Then FlashLevel24 = value / 255 : Flasherlight24_Timer : End If : End Sub 
Sub Flasherlight24_Timer()
	dim flashx3 : flashx3 = FlashLevel24^3^0.8
	If not Flasherlight24.TimerEnabled Then
		FlasherFlash24.visible = 1 : Flasherlight24.state = 1
		Flasherlight24.TimerEnabled = True
	End If
	Flasherlight24.IntensityScale = 2 * flashx3
	FlasherFlash24.opacity = 10000 * flashx3
	FlashLevel24 = FlashLevel24 * 0.8 - 0.01
	If FlashLevel24 < 0 Then
		FlasherFlash24.visible = 0 : Flasherlight24.state = 0
		Flasherlight24.TimerEnabled = False
	End If
End Sub

' ********** Backglass Flasher 8 ***********
Sub Flasherset8(value) : FlasherClick FlashLevel8, value : If value < 160 Then value = 160 : End If : If value > Flashlevel8 * 255 Then FlashLevel8 = value / 255 : VRBGFL8_1_Timer : End If : End Sub 
Sub VRBGFL8_1_Timer()
'	If VRRoom = 1 Then
'		If not VRBGFL8_1.TimerEnabled Then 
'			VRBGFL8_1.visible = 1: VRBGFL8_2.visible = 1: VRBGFL8_3.visible = 1: VRBGFL8_4.visible = 1: VRBGFL8_5.visible = 1
'			VRBGFL8_6.visible = 1: VRBGFL8_7.visible = 1: VRBGFL8_8.visible = 1: VRBGFL8_9.visible = 1: VRBGFL8_10.visible = 1
'			VRBGFL8_1.TimerEnabled = True
'		End If
'		VRBGFL8_1.IntensityScale = 5 * FlashLevel8^2
'		VRBGFL8_2.IntensityScale = 5 * FlashLevel8^2
'		VRBGFL8_3.IntensityScale = 5 * FlashLevel8^2
'		VRBGFL8_4.IntensityScale = 5 * FlashLevel8^2.5
'		VRBGFL8_5.IntensityScale = 5 * FlashLevel8^3
'		VRBGFL8_6.IntensityScale = 5 * FlashLevel8^2
'		VRBGFL8_7.IntensityScale = 5 * FlashLevel8^2
'		VRBGFL8_8.IntensityScale = 5 * FlashLevel8^2
'		VRBGFL8_9.IntensityScale = 5 * FlashLevel8^2.5
'		VRBGFL8_10.IntensityScale = 5 * FlashLevel8^3
'		FlashLevel8 = FlashLevel8 * 0.8 - 0.01
'		If FlashLevel8 < 0 Then	
'			VRBGFL8_1.visible = 1: VRBGFL8_2.visible = 1: VRBGFL8_3.visible = 1: VRBGFL8_4.visible = 0: VRBGFL8_5.visible = 0
'			VRBGFL8_6.visible = 1: VRBGFL8_7.visible = 1: VRBGFL8_8.visible = 1: VRBGFL8_9.visible = 0: VRBGFL8_10.visible = 0
'			VRBGFL8_1.TimerEnabled = False
'		End If
'	End If
End Sub

' ********** Backglass Flasher 9 ***********
Sub Flasherset9(value) : FlasherClick FlashLevel9, value : If value < 160 Then value = 160 : End If : If value > Flashlevel9 * 255 Then FlashLevel9 = value / 255 : VRBGFL9_1_Timer : End If : End Sub 
Sub VRBGFL9_1_Timer()
'	If VRRoom = 1 Then
'		If not VRBGFL9_1.TimerEnabled Then
'			VRBGFL9_1.visible = 1: VRBGFL9_2.visible = 1: VRBGFL9_3.visible = 1: VRBGFL9_4.visible = 1: VRBGFL9_5.visible = 1
'			VRBGFL9_1.TimerEnabled = True
'		End If
'		VRBGFL9_1.IntensityScale = 5 * Flashlevel9^2
'		VRBGFL9_2.IntensityScale = 5 * Flashlevel9^2
'		VRBGFL9_3.IntensityScale = 5 * Flashlevel9^2
'		VRBGFL9_4.IntensityScale = 5 * Flashlevel9^2.5
'		VRBGFL9_5.IntensityScale = 5 * Flashlevel9^3
'		FlashLevel9 = FlashLevel9 * 0.8 - 0.01
'
'		If FlashLevel9 < 0 Then 
'			VRBGFL9_1.visible = 0: VRBGFL9_2.visible = 0: VRBGFL9_3.visible = 0: VRBGFL9_4.visible = 0: VRBGFL9_5.visible = 0
'			VRBGFL9_1.TimerEnabled = False
'		End If
'	End If
End Sub

' ********** Backglass Flasher 15 ***********
Sub Flasherset15(value) : FlasherClick FlashLevel15, value : If value < 160 Then value = 160 : End If : If value > Flashlevel15 * 255 Then FlashLevel15 = value / 255 : VRBGFL15_1_Timer : End If : End Sub 
Sub VRBGFL15_1_Timer()
'	If VRRoom = 1 Then
'		If not VRBGFL15_1.TimerEnabled Then 
'			VRBGFL15_1.visible = 1: VRBGFL15_2.visible = 1: VRBGFL15_3.visible = 1: VRBGFL15_4.visible = 1: VRBGFL15_5.visible = 1
'			VRBGFL15_6.visible = 1: VRBGFL15_7.visible = 1: VRBGFL15_8.visible = 1: VRBGFL15_9.visible = 1: VRBGFL15_10.visible = 1
'			VRBGFL15_1.TimerEnabled = True
'		End If
'		VRBGFL15_1.IntensityScale = 5 * Flashlevel15^2
'		VRBGFL15_2.IntensityScale = 5 * Flashlevel15^2
'		VRBGFL15_3.IntensityScale = 5 * Flashlevel15^2
'		VRBGFL15_4.IntensityScale = 5 * Flashlevel15^2.5
'		VRBGFL15_5.IntensityScale = 5 * Flashlevel15^3
'		VRBGFL15_6.IntensityScale = 5 * Flashlevel15^2
'		VRBGFL15_7.IntensityScale = 5 * Flashlevel15^2
'		VRBGFL15_8.IntensityScale = 5 * Flashlevel15^2
'		VRBGFL15_9.IntensityScale = 5 * Flashlevel15^2.5
'		VRBGFL15_10.IntensityScale = 5 * Flashlevel15^3
'		FlashLevel15 = FlashLevel15 * 0.8 - 0.01
'		If FlashLevel15 < 0 Then 
'			VRBGFL15_1.visible = 0: VRBGFL15_2.visible = 0: VRBGFL15_3.visible = 0: VRBGFL15_4.visible = 0: VRBGFL15_5.visible = 0
'			VRBGFL15_6.visible = 0: VRBGFL15_7.visible = 0: VRBGFL15_8.visible = 0: VRBGFL15_9.visible = 0: VRBGFL15_10.visible = 0
'			VRBGFL15_1.TimerEnabled = False
'		End If
'	End If
End Sub

' ********** Backglass Flasher 16 ***********
Sub Flasherset16(value) : FlasherClick FlashLevel16, value : If value < 160 Then value = 160 : End If : If value > Flashlevel16 * 255 Then FlashLevel16 = value / 255 : VRBGFL16_1_Timer : End If : End Sub 
Sub VRBGFL16_1_Timer()
'	If VRRoom = 1 Then
'		If not VRBGFL16_1.TimerEnabled Then 
'			VRBGFL16_1.visible = 1: VRBGFL16_2.visible = 1: VRBGFL16_3.visible = 1: VRBGFL16_4.visible = 1: VRBGFL16_5.visible = 1
'			VRBGFL16_6.visible = 1: VRBGFL16_7.visible = 1: VRBGFL16_8.visible = 1: VRBGFL16_9.visible = 1: VRBGFL16_10.visible = 1
'			VRBGFL16_1.TimerEnabled = True
'		End If
'		VRBGFL16_1.IntensityScale = 5 * FlashLevel16^2
'		VRBGFL16_2.IntensityScale = 5 * FlashLevel16^2
'		VRBGFL16_3.IntensityScale = 5 * FlashLevel16^2
'		VRBGFL16_4.IntensityScale = 5 * FlashLevel16^2.5
'		VRBGFL16_5.IntensityScale = 5 * FlashLevel16^3
'		VRBGFL16_6.IntensityScale = 5 * FlashLevel16^2
'		VRBGFL16_7.IntensityScale = 5 * FlashLevel16^2
'		VRBGFL16_8.IntensityScale = 5 * FlashLevel16^2
'		VRBGFL16_9.IntensityScale = 5 * FlashLevel16^2.5
'		VRBGFL16_10.IntensityScale = 5 * FlashLevel16^3
'		FlashLevel16 = FlashLevel16 * 0.8 - 0.01

'		If FlashLevel16 < 0 Then 
'			VRBGFL16_1.visible = 0: VRBGFL16_2.visible = 0: VRBGFL16_3.visible = 0: VRBGFL16_4.visible = 0: VRBGFL16_5.visible = 0
'			VRBGFL16_6.visible = 0: VRBGFL16_7.visible = 0: VRBGFL16_8.visible = 0: VRBGFL16_9.visible = 0: VRBGFL16_10.visible = 0
'			VRBGFL16_1.TimerEnabled = False
'		End If
'	End If
End Sub

Sub GI4_23_Init()
	
End Sub

'****************************************************************
'	ZGII: GI
'****************************************************************

Dim gilvl   'General Illumination light state tracked for Dynamic Ball Shadows
gilvl = 1

Sub ToggleGI(Enabled)
	Dim xx
	If enabled Then
		For Each xx In GI
			xx.state = 1
		Next
		'PFShadowsGION.visible = 1
		gilvl = 1
	Else
		For Each xx In GI
			xx.state = 0
		Next
		'PFShadowsGION.visible = 0
		GITimer.enabled = True
		gilvl = 0
	End If
	Sound_GI_Relay enabled, bumper1
End Sub

Sub GITimer_Timer()
	Me.enabled = False
	ToggleGI 1
End Sub

' *****************************************************
' *** Old Ball Shadow code                        *****
' *****************************************************

'Dim BallShadowArray : BallShadowArray = Array (BallShadow1Old, BallShadow2Old, BallShadow3Old)

'Sub GraphicsTimer_Timer()
' *** move ball shadows ***
'	If BallShadow then 
'		Dim BOT, b : BOT = GetBalls
'		For b = 0 to UBound(BOT)
'			BallShadowArray(b).X = BOT(b).X + (BOT(b).X - Table1.Width/2)/8 : BallShadowArray(b).Y = BOT(b).Y + 30 : BallShadowArray(b).Z = BOT(b).Z - 24
'		Next
'	End If
'End Sub

' *** Enhance the ball shine for balls on upper deck or plastic ramps ***

'Dim LightArray : LightArray = Array (Light16, Light18, Light21)

'Sub BallReflections_timer()
'	Dim BOT, vx, vy, mx, b : BOT = Getballs
'	For b = 0 to UBound(BOT)
'		vx =BOT(b).VelX: vy = BOT(b).VelY
'		If abs(vx) + abs(vy) > 8 and BOT(b).Z > 40 Then
'			mx = (abs(vx) + abs(vy)) / 8 : If mx > 2 Then mx = 2 : End If
'			LightArray(b).intensityscale = mx : LightArray(b).BulbHaloHeight = BOT(b).Z + 26
'			LightArray(b).X = BOT(b).X + rnd * 100 + 20 * vx: LightArray(b).Y = BOT(b).Y + rnd * 100 + 20 * vy 
'		else
'			LightArray(b).intensityscale = 0.03
'		end if
'	Next
'End Sub

'***************************************************************
'	ZSHA: VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx8" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* Count from 0 up, with at least as many objects each as there can be balls, including locked balls.  You'll get an "eval" warning if tnob is higher
'	* Warning:  If merging with another system (JP's ballrolling), you may need to check tnob math and add an extra BallShadowA# flasher (out of range error)
' Ensure you have a timer with a -1 interval that is always running
' Set plastic ramps DB to *less* than the ambient shadows (-11000) if you want to see the pf shadow through the ramp
' Place triggers at the start of each ramp *type* (solid, clear, wire) and one at the end if it doesn't return to the base pf
'	* These can share duties as triggers for RampRolling sounds

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
' It's recommended that you be selective in which lights go in this collection, as there are limitations:
' 1. The shadows can "pass through" solid objects and other light sources, so be mindful of where the lights would actually able to cast shadows
' 2. If there are more than two equidistant sources, the shadows can suddenly switch on and off, so places like top and bottom lanes need attention
' 3. At this time the shadows get the light on/off from tracking gilvl, so if you have lights you want shadows for that are on at different times you will need to either:
'	a) remove this restriction (shadows think lights are always On)
'	b) come up with a custom solution (see TZ example in script)
' After confirming the shadows work in general, use ball control to move around and look for any weird behavior

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
' Example timer sub:

'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top for easy access by players
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

' *** The following segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
' ** Change gBOT to BOT if using existing getballs code
' ** Double commented lines commonly found there included for reference:

''	' stop the sound of deleted balls
''	For b = UBound(gBOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
''		...rolling(b) = False
''		...StopSound("BallRoll_" & b)
''	Next
''
'' ...rolling and drop sounds...
''
''		If DropCount(b) < 5 Then
''			DropCount(b) = DropCount(b) + 1
''		End If
''
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			BallShadowA(b).visible = 1
'			BallShadowA(b).X = gBOT(b).X + offsetX
'			If gBOT(b).Z > 30 Then
'				BallShadowA(b).height=gBOT(b).z - BallSize/4 + b/1000	'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
'				BallShadowA(b).Y = gBOT(b).Y + offsetY + BallSize/10
'			Else
'				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 1.04 + b/1000
'				BallShadowA(b).Y = gBOT(b).Y + offsetY
'			End If
'		End If

' *** Place this inside the table init, just after trough balls are added to gBOT
'
' Add balls to shadow dictionary
'	For Each xx in gBOT
'		bsDict.Add xx.ID, bsNone
'	Next

' *** Example RampShadow trigger subs:

'Sub ClearRampStart_hit()
'	bsRampOnClear			'Shadow on ramp and pf below
'End Sub

'Sub SolidRampStart_hit()
'	bsRampOn				'Shadow on ramp only
'End Sub

'Sub WireRampStart_hit()
'	bsRampOnWire			'Shadow only on pf
'End Sub

'Sub RampEnd_hit()
'	bsRampOff ActiveBall.ID	'Back to default shadow behavior
'End Sub


' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

'Function Distance(ax,ay,bx,by)
'	Distance = SQR((ax - bx)^2 + (ay - by)^2)
'End Function

'Dim PI: PI = 4*Atn(1)

'Function Atn2(dy, dx)
'	If dx > 0 Then
'		Atn2 = Atn(dy / dx)
'	ElseIf dx < 0 Then
'		If dy = 0 Then
'			Atn2 = pi
'		Else
'			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
'		end if
'	ElseIf dx = 0 Then
'		if dy = 0 Then
'			Atn2 = 0
'		else
'			Atn2 = Sgn(dy) * pi / 2
'		end if
'	End If
'End Function

'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******

' *** These define the appearance of shadows in your table	***

'Ambient (Room light source)
Const AmbientBSFactor = 0.9	 '0 To 1, higher is darker
Const AmbientMovement = 1	   '1+ higher means more movement as the ball moves left and right
Const offsetX = 0			   'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 5			   'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

'Dynamic (Table light sources)
Const DynamicBSFactor = 0.90	'0 To 1, higher is darker
Const Wideness = 20			 'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness = 5			  'Sets minimum as ball moves away from source

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objrtx1(3), objrtx2(3)
Dim objBallShadow(3)
Dim OnPF(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2)
Dim DSSources(6), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

' *** The Shadow Dictionary
Dim bsDict
Set bsDict = New cvpmDictionary
Const bsNone = "None"
Const bsWire = "Wire"
Const bsRamp = "Ramp"
Const bsRampClear = "Clear"

'Initialization
DynamicBSInit

Sub DynamicBSInit()
	Dim iii, source
	
	'Prepare the shadow objects before play begins
	For iii = 0 To tnob - 1
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii / 1000 + 0.01  'Separate z for layering without clipping
		objrtx1(iii).visible = 0
		
		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii / 1000 + 0.02
		objrtx2(iii).visible = 0
		
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii / 1000 + 0.04
		objBallShadow(iii).visible = 0
		
		BallShadowA(iii).Opacity = 100 * AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next
	
	iii = 0
	
	For Each Source In DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		'   If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
End Sub

Sub BallOnPlayfieldNow(onPlayfield, ballNum)	'Whether a ball is currently on the playfield. Only update certain things once, save some cycles
	'Dim BOT: BOT=getballs	'Uncomment if you're destroying balls - Not recommended! #SaveTheBalls
	If onPlayfield Then
		OnPF(ballNum) = True
		bsRampOff gBOT(ballNum).ID
		'   debug.print "Back on PF"
		UpdateMaterial objBallShadow(ballNum).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(ballNum).size_x = 5
		objBallShadow(ballNum).size_y = 4.5
		objBallShadow(ballNum).visible = 1
		BallShadowA(ballNum).visible = 0
		BallShadowA(ballNum).Opacity = 100 * AmbientBSFactor
	Else
		OnPF(ballNum) = False
		'   debug.print "Leaving PF"
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff 'Max distance to light sources, can be changed dynamically if you have a reason
	falloff = 150
	Dim ShadowOpacity1, ShadowOpacity2
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim bsRampType
	'Dim BOT: BOT=getballs	'Uncomment if you're destroying balls - Not recommended! #SaveTheBalls
	
	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 To tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next
	
	If UBound(gBOT) < lob Then Exit Sub 'No balls in play, exit
	
	'The Magic happens now
	For s = lob To UBound(gBOT)
		' *** Normal "ambient light" ball shadow
		'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your Elseif segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else (under 20)
		
		'Primitive shadow on playfield, flasher shadow in ramps
		If AmbientBallShadowOn = 1 Then
			'** Above the playfield
			If gBOT(s).Z > 30 Then
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update
				bsRampType = getBsRampType(gBOT(s).id)
				'   debug.print bsRampType
				
				If Not bsRampType = bsRamp Then 'Primitive visible on PF
					objBallShadow(s).visible = 1
					objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
					objBallShadow(s).Y = gBOT(s).Y + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z + BallSize) / 80) 'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z + BallSize) / 80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor * (30 / (gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else 'Opaque, no primitive below
					objBallShadow(s).visible = 0
				End If
				
				If bsRampType = bsRampClear Or bsRampType = bsRamp Then 'Flasher visible on opaque ramp
					BallShadowA(s).visible = 1
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize / 10
					BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000 'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
					If bsRampType = bsRampClear Then BallShadowA(s).Opacity = 50 * AmbientBSFactor
				ElseIf bsRampType = bsWire Or bsRampType = bsNone Then 'Turn it off on wires or falling out of a ramp
					BallShadowA(s).visible = 0
				End If
				
				'** On pf, primitive only
			ElseIf gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then
				If Not OnPF(s) Then BallOnPlayfieldNow True, s
				objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
				objBallShadow(s).Y = gBOT(s).Y + offsetY
				'   objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf
				
				'** Under pf, flasher shadow only
			Else
				If OnPF(s) Then BallOnPlayfieldNow False, s
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000
			End If
			
			'Flasher shadow everywhere
		ElseIf AmbientBallShadowOn = 2 Then
			If gBOT(s).Z > 30 Then 'In a ramp
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize / 10
				BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000 'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			ElseIf gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then 'On pf
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height = 1.04 + s / 1000
			Else 'Under pf
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000
			End If
		End If
		
		' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 Then 'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff
				dist2 = falloff
				For iii = 0 To numberofsources - 1 'Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then
						'   If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1
					objrtx1(s).X = gBOT(s).X
					objrtx1(s).Y = gBOT(s).Y
					'   objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1 * DynamicBSFactor ^ 3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1
					objrtx2(s).X = gBOT(s).X
					objrtx2(s).Y = gBOT(s).Y + offsetY
					'   objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2 * DynamicBSFactor ^ 3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0
				objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub

' *** Ramp type definitions

Sub bsRampOnWire()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsWire
	Else
		bsDict.Add ActiveBall.ID, bsWire
	End If
End Sub

Sub bsRampOn()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsRamp
	Else
		bsDict.Add ActiveBall.ID, bsRamp
	End If
End Sub

Sub bsRampOnClear()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsRampClear
	Else
		bsDict.Add ActiveBall.ID, bsRampClear
	End If
End Sub

Sub bsRampOff(idx)
	If bsDict.Exists(idx) Then
		bsDict.Item(idx) = bsNone
	End If
End Sub

Function getBsRampType(id)
	Dim retValue
	If bsDict.Exists(id) Then
		retValue = bsDict.Item(id)
	Else
		retValue = bsNone
	End If
	getBsRampType = retValue
End Function

'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************

'******************************************************
'	ZPHY:  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Adding nFozzy roth physics : pt1 rubber dampeners 				https://youtu.be/AXX3aen06FM
' Adding nFozzy roth physics : pt2 flipper physics 					https://youtu.be/VSBFuK2RCPE
' Adding nFozzy roth physics : pt3 other elements 					https://youtu.be/JN8HEJapCvs
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25)
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 9.5-10.5 |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
'
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 4-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |






'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. an object or point to tell the script where the tip of the flipper is at rest (EndPointLp, EndPointRp)
'	4. and, special scripting
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.3            | 0.3                   | 0.275                  | 0.275              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF
Set LF = New FlipperPolarity
Dim RF
Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 2.7
'		x.AddPt "Polarity", 2, 0.33, - 2.7
'		x.AddPt "Polarity", 3, 0.37, - 2.7
'		x.AddPt "Polarity", 4, 0.41, - 2.7
'		x.AddPt "Polarity", 5, 0.45, - 2.7
'		x.AddPt "Polarity", 6, 0.576, - 2.7
'		x.AddPt "Polarity", 7, 0.66, - 1.8
'		x.AddPt "Polarity", 8, 0.743, - 0.5
'		x.AddPt "Polarity", 9, 0.81, - 0.5
'		x.AddPt "Polarity", 10, 0.88, 0
'
'		x.AddPt "Velocity", 0, 0, 1
'		x.AddPt "Velocity", 1, 0.16, 1.06
'		x.AddPt "Velocity", 2, 0.41, 1.05
'		x.AddPt "Velocity", 3, 0.53, 1 '0.982
'		x.AddPt "Velocity", 4, 0.702, 0.968
'		x.AddPt "Velocity", 5, 0.95,  0.968
'		x.AddPt "Velocity", 6, 1.03, 0.945
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
'
'
''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 3.7
'		x.AddPt "Polarity", 2, 0.33, - 3.7
'		x.AddPt "Polarity", 3, 0.37, - 3.7
'		x.AddPt "Polarity", 4, 0.41, - 3.7
'		x.AddPt "Polarity", 5, 0.45, - 3.7
'		x.AddPt "Polarity", 6, 0.576,- 3.7
'		x.AddPt "Polarity", 7, 0.66, - 2.3
'		x.AddPt "Polarity", 8, 0.743, - 1.5
'		x.AddPt "Polarity", 9, 0.81, - 1
'		x.AddPt "Polarity", 10, 0.88, 0
'
'		x.AddPt "Velocity", 0, 0, 1
'		x.AddPt "Velocity", 1, 0.16, 1.06
'		x.AddPt "Velocity", 2, 0.41, 1.05
'		x.AddPt "Velocity", 3, 0.53, 1 '0.982
'		x.AddPt "Velocity", 4, 0.702, 0.968
'		x.AddPt "Velocity", 5, 0.95,  0.968
'		x.AddPt "Velocity", 6, 1.03, 0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
''*******************************************
''  Late 80's early 90's
'
Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 5
		x.AddPt "Polarity", 2, 0.4, - 5
		x.AddPt "Polarity", 3, 0.6, - 4.5
		x.AddPt "Polarity", 4, 0.65, - 4.0
		x.AddPt "Polarity", 5, 0.7, - 3.5
		x.AddPt "Polarity", 6, 0.75, - 3.0
		x.AddPt "Polarity", 7, 0.8, - 2.5
		x.AddPt "Polarity", 8, 0.85, - 2.0
		x.AddPt "Polarity", 9, 0.9, - 1.5
		x.AddPt "Polarity", 10, 0.95, - 1.0
		x.AddPt "Polarity", 11, 1, - 0.5
		x.AddPt "Polarity", 12, 1.1, 0
		x.AddPt "Polarity", 13, 1.3, 0

		x.AddPt "Velocity", 0, 0, 1
		x.AddPt "Velocity", 1, 0.16, 1.06
		x.AddPt "Velocity", 2, 0.41, 1.05
		x.AddPt "Velocity", 3, 0.53, 1 '0.982
		x.AddPt "Velocity", 4, 0.702, 0.968
		x.AddPt "Velocity", 5, 0.95,  0.968
		x.AddPt "Velocity", 6, 1.03,  0.945
	Next

	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub

'*******************************************
' Early 90's and after
'
'Sub InitPolarity()
'	Dim x, a
'	a = Array(LF, RF)
'	For Each x In a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'		x.DebugOn=False ' prints some info in debugger
'		
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, -5.5
'		x.AddPt "Polarity", 2, 0.4, -5.5
'		x.AddPt "Polarity", 3, 0.6, -5.0
'		x.AddPt "Polarity", 4, 0.65, -4.5
'		x.AddPt "Polarity", 5, 0.7, -4.0
'		x.AddPt "Polarity", 6, 0.75, -3.5
'		x.AddPt "Polarity", 7, 0.8, -3.0
'		x.AddPt "Polarity", 8, 0.85, -2.5
'		x.AddPt "Polarity", 9, 0.9,-2.0
'		x.AddPt "Polarity", 10, 0.95, -1.5
'		x.AddPt "Polarity", 11, 1, -1.0
'		x.AddPt "Polarity", 12, 1.05, -0.5
'		x.AddPt "Polarity", 13, 1.1, 0
'		x.AddPt "Polarity", 14, 1.3, 0
'		
'		x.AddPt "Velocity", 0, 0,	   1
'		x.AddPt "Velocity", 1, 0.160, 1.06
'		x.AddPt "Velocity", 2, 0.410, 1.05
'		x.AddPt "Velocity", 3, 0.530, 1'0.982
'		x.AddPt "Velocity", 4, 0.702, 0.968
'		x.AddPt "Velocity", 5, 0.95,  0.968
'		x.AddPt "Velocity", 6, 1.03,  0.945
'	Next
'	
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'	LF.SetObjects "LF", LeftFlipper, TriggerLF
'	RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
'' Flipper trigger hit subs
'Sub TriggerLF_Hit()
'	LF.Addball activeball
'End Sub
'Sub TriggerLF_UnHit()
'	LF.PolarityCorrect activeball
'End Sub
'Sub TriggerRF_Hit()
'	RF.Addball activeball
'End Sub
'Sub TriggerRF_UnHit()
'	RF.PolarityCorrect activeball
'End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
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
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x) ) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x) ) Then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
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
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperTricks RightFlipper1, RFPress1, RFCount1, RFEndAngle1, RFState1
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
	
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	'Dim BOT
	'BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					gBOT(b).velx = gBOT(b).velx / 1.3
					gBOT(b).vely = gBOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub

Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub

'*****************
' Maths
'*****************

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, RFPress1, LFCount, RFCount, RFCount1
Dim LFState, RFState, RFState1
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle, RFEndAngle1

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
RFState1 = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
'   Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
RFEndAngle1 = RightFlipper1.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity
	
	Flipper.eostorque = EOST
	Flipper.eostorqueangle = EOSA
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b', BOT
		'BOT = GetBalls
		
		For b = 0 To UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= - 0.4 Then gBOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle)	'-1 for Right Flipper
	Dim LiveCatchBounce																														'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime
	CatchTime = GameTime - FCount
	
	If CatchTime <= LiveCatch And parm > 6 And Abs(Flipper.x - ball.x) > LiveDistanceMin And Abs(Flipper.x - ball.x) < LiveDistanceMax Then
		If CatchTime <= LiveCatch * 0.5 Then												'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		Else
			LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)		'Partial catch when catch happens a bit late
		End If
		
		If LiveCatchBounce = 0 And ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx = 0
		ball.angmomy = 0
		ball.angmomz = 0
	Else
		If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
	End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************


'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
'Sub RDampen_Timer
'	Cor.Update
'End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************



'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7	 'Level of bounces. Recommmended value of 0.7

Sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel, vratio
	If TargetBouncerEnabled = 1 And aball.z < 30 Then
		'   debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
		If aBall.velx = 0 Then vratio = 1 Else vratio = aBall.vely / aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1
				zMultiplier = 0.2 * defvalue
			Case 2
				zMultiplier = 0.25 * defvalue
			Case 3
				zMultiplier = 0.3 * defvalue
			Case 4
				zMultiplier = 0.4 * defvalue
			Case 5
				zMultiplier = 0.45 * defvalue
			Case 6
				zMultiplier = 0.5 * defvalue
		End Select
		aBall.velz = Abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((vel ^ 2 - aBall.velz ^ 2) / (1 + vratio ^ 2)))
		aBall.vely = aBall.velx * vratio
		'   debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'   debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer ActiveBall, 1
End Sub



'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS
	
	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
Function RotPoint(x,y,angle)
	dim rx, ry
	rx = x*dCos(angle) - y*dSin(angle)
	ry = x*dSin(angle) + y*dCos(angle)
	RotPoint = Array(rx,ry)
End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			'   debug.print " "
		End If
	End Sub
End Class

'******************************************************
'	ZBRL:  BALL ROLLING AND DROP SOUNDS
'******************************************************

' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 To tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b
	'Dim BOT
	'BOT = GetBalls
	
	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 To tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(gBOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(gBOT)
		If BallVel(gBOT(b)) > 1 And gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If gBOT(b).VelZ <  - 1 And gBOT(b).z < 55 And gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz >  - 7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If
			End If
		End If
		
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
		
		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dynamic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If gBOT(b).Z > 30 Then
				BallShadowA(b).height = gBOT(b).z - BallSize / 4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Else
				BallShadowA(b).height = 0.1
			End If
			BallShadowA(b).Y = gBOT(b).Y + offsetY
			BallShadowA(b).X = gBOT(b).X + offsetX
			BallShadowA(b).visible = 1
		End If
	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************

'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'		  * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'		  * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'		  * Create a Timer called RampRoll, that is enabled, with a interval of 100
'		  * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'		  * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'		  * To stop tracking ball
'				 * call WireRampOff
'				 * Otherwise, the ball will auto remove if it's below 30 vp units
'

Dim RampMinLoops
RampMinLoops = 4

' RampBalls
' Setup:  Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RampBalls(6,2)
Dim RampBalls(6,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(6)

Sub WireRampOn(input)
	Waddball ActiveBall, input
	RampRollUpdate
End Sub

Sub WireRampOff()
	WRemoveBall ActiveBall.ID
End Sub

' WaddBall (Active Ball, Boolean)
Sub Waddball(input, RampInput) 'This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	Dim x
	For x = 1 To UBound(RampBalls)	'Check, don't add balls twice
		If RampBalls(x, 1) = input.id Then
			If Not IsEmpty(RampBalls(x,1) ) Then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next
	
	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 To UBound(RampBalls)
		If IsEmpty(RampBalls(x, 1)) Then
			Set RampBalls(x, 0) = input
			RampBalls(x, 1) = input.ID
			RampType(x) = RampInput
			RampBalls(x, 2) = 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			Exit Sub
		End If
		If x = UBound(RampBalls) Then	 'debug
			Debug.print "WireRampOn error, ball queue Is full: " & vbNewLine & _
			RampBalls(0, 0) & vbNewLine & _
			TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbNewLine & _
			TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbNewLine & _
			TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbNewLine & _
			TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbNewLine & _
			TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbNewLine & _
			" "
		End If
	Next
End Sub

' WRemoveBall (BallId)
Sub WRemoveBall(ID) 'This subroutine is called from the RampRollUpdate subroutine and is used to remove and stop the ball rolling sounds
	'   Debug.Print "In WRemoveBall() + Remove ball from loop array"
	Dim ballcount
	ballcount = 0
	Dim x
	For x = 1 To UBound(RampBalls)
		If ID = RampBalls(x, 1) Then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		If Not IsEmpty(Rampballs(x,1)) Then ballcount = ballcount + 1
	Next
	If BallCount = 0 Then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer()
	RampRollUpdate
End Sub

Sub RampRollUpdate()	'Timer update
	Dim x
	For x = 1 To UBound(RampBalls)
		If Not IsEmpty(RampBalls(x,1) ) Then
			If BallVel(RampBalls(x,0) ) > 1 Then ' if ball is moving, play rolling sound
				If RampType(x) Then
					PlaySound("RampLoop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2) = RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			End If
			If RampBalls(x,0).Z < 30 And RampBalls(x, 2) > RampMinLoops Then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
	Next
	If Not RampBalls(0,0) Then RampRoll.enabled = 0
End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	Me.text = "on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbNewLine & _
	"1 " & TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbNewLine & _
	"2 " & TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbNewLine & _
	"3 " & TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbNewLine & _
	"4 " & TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbNewLine & _
	"5 " & TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbNewLine & _
	"6 " & TypeName(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbNewLine & _
	" "
End Sub

Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
	BallPitch = pSlope(BallVel(ball), 1, - 1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, - 4000, 60, 7000)
End Function

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************

'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.
' Create the following new collections:
'	 Metals (all metal objects, metal walls, metal posts, metal wire guides)
'	 Apron (the apron walls and plunger wall)
'	 Walls (all wood or plastic walls)
'	 Rollovers (wire rollover triggers, star triggers, or button triggers)
'	 Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
'	 Gates (plate gates)
'	 GatesWire (wire gates)
'	 Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate
' how to make these updates. But in summary the following needs to be updated:
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial vides by Apophis
' Audio : Adding Fleep Part 1				https://youtu.be/rG35JVHxtx4
' Audio : Adding Fleep Part 2				https://youtu.be/dk110pWMxGo
' Audio : Adding Fleep Part 3				https://youtu.be/ESXWGJZY_EI


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
RollingSoundFactor = 1.1 / 5

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025 / 5			 'volume multiplier; must not be zero

'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	PlaySound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////

Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////

Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////

Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////

Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////

Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
			PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
			PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
			PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
			PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
			PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
			PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
			PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
			PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
			PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
			PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////

Sub RandomSoundBottomArchBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(ActiveBall.id) < 4) And cor.ballvely(ActiveBall.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft ActiveBall
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, ActiveBall
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, ActiveBall
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If ActiveBall.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If ActiveBall.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If ActiveBall.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If ActiveBall.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, ActiveBall
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
			PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)
	FlipperCradleCollision ball1, ball2, velocity
	Dim snd
	Select Case Int(Rnd * 7) + 1
		Case 1
			snd = "Ball_Collide_1"
		Case 2
			snd = "Ball_Collide_2"
		Case 3
			snd = "Ball_Collide_3"
		Case 4
			snd = "Ball_Collide_4"
		Case 5
			snd = "Ball_Collide_5"
		Case 6
			snd = "Ball_Collide_6"
		Case 7
			snd = "Ball_Collide_7"
	End Select
	
	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

'******************************************************
'****  VPW OPTIONS UI
'******************************************************

' Base options
Const Opt_LUT = 0
Const Opt_Volume = 1
Const Opt_Volume_Ramp = 2
Const Opt_Volume_Ball = 3
' Shadow options
Const Opt_DynBallShadow = 4
Const Opt_AmbientBallShadow = 5
' Difficulty options
'Const Opt_Scoop_Difficulty = 6
' Informations
Const Opt_Info_1 = 6
Const Opt_Info_2 = 7

Const NOptions = 8

'Dim ModSol, ApronMod, DecalMod, InstMod, SideBladeMod, GlassMod, DMDDecalMod, TopperMod, BackglassMod, LogoMod, PosterMod

'************************************************************************
'							OTHER OPTIONS
'************************************************************************

Dim ColorLUT : ColorLUT = 1
Dim VolumeDial : VolumeDial = 0.8			'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5 	'Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 	'Level of ramp rolling volume. Value between 0 and 1

Dim DynamicBallShadowsOn : DynamicBallShadowsOn = 1	  '0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Dim AmbientBallShadowOn : AmbientBallShadowOn = 1	  '0 = Static shadow under ball ("flasher" image, like JP's)
									                  '1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									                  '2 = flasher image shadow, but it moves like ninuzzu's

Dim VRRoomChoice : VRRoomChoice = 0				'0 - Minimal Room

Const TableVersion = "1.2"	'Table version (shown in option UI)

Const FlexDMD_RenderMode_DMD_GRAY_2 = 0
Const FlexDMD_RenderMode_DMD_GRAY_4 = 1
Const FlexDMD_RenderMode_DMD_RGB = 2
Const FlexDMD_RenderMode_SEG_2x16Alpha = 3
Const FlexDMD_RenderMode_SEG_2x20Alpha = 4
Const FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num = 5
Const FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num_4x1Num = 6
Const FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num = 7
Const FlexDMD_RenderMode_SEG_2x7Num_2x7Num_10x1Num = 8
Const FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num_gen7 = 9
Const FlexDMD_RenderMode_SEG_2x7Num10_2x7Num10_4x1Num = 10
Const FlexDMD_RenderMode_SEG_2x6Num_2x6Num_4x1Num = 11
Const FlexDMD_RenderMode_SEG_2x6Num10_2x6Num10_4x1Num = 12
Const FlexDMD_RenderMode_SEG_4x7Num10 = 13
Const FlexDMD_RenderMode_SEG_6x4Num_4x1Num = 14
Const FlexDMD_RenderMode_SEG_2x7Num_4x1Num_1x16Alpha = 15
Const FlexDMD_RenderMode_SEG_1x16Alpha_1x16Num_1x7Num = 16

Const FlexDMD_Align_TopLeft = 0
Const FlexDMD_Align_Top = 1
Const FlexDMD_Align_TopRight = 2
Const FlexDMD_Align_Left = 3
Const FlexDMD_Align_Center = 4
Const FlexDMD_Align_Right = 5
Const FlexDMD_Align_BottomLeft = 6
Const FlexDMD_Align_Bottom = 7
Const FlexDMD_Align_BottomRight = 8

Const FlexDMD_Scaling_Fit = 0
Const FlexDMD_Scaling_Fill = 1
Const FlexDMD_Scaling_FillX = 2
Const FlexDMD_Scaling_FillY = 3
Const FlexDMD_Scaling_Stretch = 4
Const FlexDMD_Scaling_StretchX = 5
Const FlexDMD_Scaling_StretchY = 6
Const FlexDMD_Scaling_None = 7

Const FlexDMD_Interpolation_Linear = 0
Const FlexDMD_Interpolation_ElasticIn = 1
Const FlexDMD_Interpolation_ElasticOut = 2
Const FlexDMD_Interpolation_ElasticInOut = 3
Const FlexDMD_Interpolation_QuadIn = 4
Const FlexDMD_Interpolation_QuadOut = 5
Const FlexDMD_Interpolation_QuadInOut = 6
Const FlexDMD_Interpolation_CubeIn = 7
Const FlexDMD_Interpolation_CubeOut = 8
Const FlexDMD_Interpolation_CubeInOut = 9
Const FlexDMD_Interpolation_QuartIn = 10
Const FlexDMD_Interpolation_QuartOut = 11
Const FlexDMD_Interpolation_QuartInOut = 12
Const FlexDMD_Interpolation_QuintIn = 13
Const FlexDMD_Interpolation_QuintOut = 14
Const FlexDMD_Interpolation_QuintInOut = 15
Const FlexDMD_Interpolation_SineIn = 16
Const FlexDMD_Interpolation_SineOut = 17
Const FlexDMD_Interpolation_SineInOut = 18
Const FlexDMD_Interpolation_BounceIn = 19
Const FlexDMD_Interpolation_BounceOut = 20
Const FlexDMD_Interpolation_BounceInOut = 21
Const FlexDMD_Interpolation_CircIn = 22
Const FlexDMD_Interpolation_CircOut = 23
Const FlexDMD_Interpolation_CircInOut = 24
Const FlexDMD_Interpolation_ExpoIn = 25
Const FlexDMD_Interpolation_ExpoOut = 26
Const FlexDMD_Interpolation_ExpoInOut = 27
Const FlexDMD_Interpolation_BackIn = 28
Const FlexDMD_Interpolation_BackOut = 29
Const FlexDMD_Interpolation_BackInOut = 30

Dim OptionDMD: Set OptionDMD = Nothing
Dim bOptionsMagna, bInOptions : bOptionsMagna = False
Dim OptPos, OptSelected, OptN, OptTop, OptBot, OptSel
Dim OptFontHi, OptFontLo

Sub Options_Open
	bOptionsMagna = False
	On Error Resume Next
	Set OptionDMD = CreateObject("FlexDMD.FlexDMD")
	On Error Goto 0
	If OptionDMD is Nothing Then
		Debug.Print "FlexDMD is not installed"
		Debug.Print "Option UI can not be opened"
		MsgBox "You need to install FlexDMD to access table options"
		Exit Sub
	End If
	If Table1.ShowDT Then OptionDMDFlasher.RotX = -(Table1.Inclination + Table1.Layback)
	bInOptions = True
	OptPos = 0
	OptSelected = False
	OptionDMD.Show = False
	OptionDMD.RenderMode = FlexDMD_RenderMode_DMD_GRAY_4
	OptionDMD.Width = 128
	OptionDMD.Height = 32
	OptionDMD.Clear = True
	OptionDMD.Run = True
	Dim a, scene, font
	Set scene = OptionDMD.NewGroup("Scene")
	Set OptFontHi = OptionDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbWhite, vbWhite, 0)
	Set OptFontLo = OptionDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", RGB(100, 100, 100), RGB(100, 100, 100), 0)
	Set OptSel = OptionDMD.NewGroup("Sel")
	Set a = OptionDMD.NewLabel(">", OptFontLo, ">>>")
	a.SetAlignedPosition 1, 16, FlexDMD_Align_Left
	OptSel.AddActor a
	Set a = OptionDMD.NewLabel(">", OptFontLo, "<<<")
	a.SetAlignedPosition 127, 16, FlexDMD_Align_Right
	OptSel.AddActor a
	scene.AddActor OptSel
	OptSel.SetBounds 0, 0, 128, 32
	OptSel.Visible = False
	
	Set a = OptionDMD.NewLabel("Info1", OptFontLo, "MAGNA EXIT/ENTER")
	a.SetAlignedPosition 1, 32, FlexDMD_Align_BottomLeft
	scene.AddActor a
	Set a = OptionDMD.NewLabel("Info2", OptFontLo, "FLIPPER SELECT")
	a.SetAlignedPosition 127, 32, FlexDMD_Align_BottomRight
	scene.AddActor a
	Set OptN = OptionDMD.NewLabel("Pos", OptFontLo, "LINE 1")
	Set OptTop = OptionDMD.NewLabel("Top", OptFontLo, "LINE 1")
	Set OptBot = OptionDMD.NewLabel("Bottom", OptFontLo, "LINE 2")
	scene.AddActor OptN
	scene.AddActor OptTop
	scene.AddActor OptBot
	Options_OnOptChg
	OptionDMD.LockRenderThread
	OptionDMD.Stage.AddActor scene
	OptionDMD.UnlockRenderThread
	OptionDMDFlasher.Visible = True
	DisableStaticPrerendering = True
End Sub

Sub Options_UpdateDMD
	If OptionDMD is Nothing Then Exit Sub
	Dim DMDp: DMDp = OptionDMD.DmdPixels
	If Not IsEmpty(DMDp) Then
		OptionDMDFlasher.DMDWidth = OptionDMD.Width
		OptionDMDFlasher.DMDHeight = OptionDMD.Height
		OptionDMDFlasher.DMDPixels = DMDp
	End If
End Sub

Sub Options_Close
	bInOptions = False
	OptionDMDFlasher.Visible = False
	If OptionDMD is Nothing Then Exit Sub
	OptionDMD.Run = False
	Set OptionDMD = Nothing
	DisableStaticPrerendering = False
End Sub

Function Options_OnOffText(opt)
	If opt Then
		Options_OnOffText = "ON"
	Else
		Options_OnOffText = "OFF"
	End If
End Function

Sub Options_OnOptChg
	If OptionDMD is Nothing Then Exit Sub
	OptionDMD.LockRenderThread
'	If RenderingMode <> 2 Then
'		If OptPos < Opt_VRRoomChoice Then
			OptN.Text = (OptPos+1) & "/" & (NOptions)
'		Else
'			OptN.Text = (OptPos+1 - 4) & "/" & (NOptions - 4)
'		End If
'	Else
'		OptN.Text = (OptPos+1) & "/" & NOptions
'	End If
	If OptSelected Then
		OptTop.Font = OptFontLo
		OptBot.Font = OptFontHi
		OptSel.Visible = True
	Else
		OptTop.Font = OptFontHi
		OptBot.Font = OptFontLo
		OptSel.Visible = False
	End If
	If OptPos = Opt_LUT Then
		OptTop.Text = "COLOR SATURATION"
'		OptBot.Text = "LUT " & CInt(ColorLUT)
		if ColorLUT = 1 Then OptBot.text = "DISABLED"
		if ColorLUT = 2 Then OptBot.text = "DESATURATED -10%"
		if ColorLUT = 3 Then OptBot.text = "DESATURATED -20%"
		if ColorLUT = 4 Then OptBot.text = "DESATURATED -30%"
		if ColorLUT = 5 Then OptBot.text = "DESATURATED -40%"
		if ColorLUT = 6 Then OptBot.text = "DESATURATED -50%"
		if ColorLUT = 7 Then OptBot.text = "DESATURATED -60%"
		if ColorLUT = 8 Then OptBot.text = "DESATURATED -70%"
		if ColorLUT = 9 Then OptBot.text = "DESATURATED -80%"
		if ColorLUT = 10 Then OptBot.text = "DESATURATED -90%"
		if ColorLUT = 11 Then OptBot.text = "BLACK'N WHITE"
		SaveValue cGameName, "LUT", ColorLUT
	ElseIf OptPos = Opt_Volume Then
		OptTop.Text = "MECH VOLUME"
		OptBot.Text = "LEVEL " & CInt(VolumeDial * 100)
		SaveValue cGameName, "VOLUME", VolumeDial
	ElseIf OptPos = Opt_Volume_Ramp Then
		OptTop.Text = "RAMP VOLUME"
		OptBot.Text = "LEVEL " & CInt(RampRollVolume * 100)
		SaveValue cGameName, "RAMPVOLUME", RampRollVolume
	ElseIf OptPos = Opt_Volume_Ball Then
		OptTop.Text = "BALL VOLUME"
		OptBot.Text = "LEVEL " & CInt(BallRollVolume * 100)
		SaveValue cGameName, "BALLVOLUME", BallRollVolume
	ElseIf OptPos = Opt_DynBallShadow Then
		OptTop.Text = "DYN. BALL SHADOWS"
		OptBot.Text = Options_OnOffText(DynamicBallShadowsOn)
		SaveValue cGameName, "DYNBALLSH", DynamicBallShadowsOn
	ElseIf OptPos = Opt_AmbientBallShadow Then
		OptTop.Text = "AMB. BALL SHADOWS"
		If AmbientBallShadowOn = 0 Then OptBot.Text = "STATIC"
		If AmbientBallShadowOn = 1 Then OptBot.Text = "MOVING"
		If AmbientBallShadowOn = 2 Then OptBot.Text = "FLASHER"
		SaveValue cGameName, "AMBBALLSH", AmbientBallShadowOn
'	ElseIf OptPos = Opt_Scoop_Difficulty Then
'		OptTop.Text = "SCOOP DIFFICULTY"
'		If Scoop_Difficulty = 1 Then OptBot.Text = "EASY"
'		If Scoop_Difficulty = 2 Then OptBot.Text = "MEDIUM (DEFAULT)"
'		If Scoop_Difficulty = 3 Then OptBot.Text = "HARD"
'		SaveValue cGameName, "SCOOPDIFFICULTY", Scoop_Difficulty
	ElseIf OptPos = Opt_Info_1 Then
		OptTop.Text = "VPX " & VersionMajor & "." & VersionMinor & "." & VersionRevision
		OptBot.Text = "WHITE WATER " & TableVersion
	ElseIf OptPos = Opt_Info_2 Then
		OptTop.Text = "RENDER MODE"
		If RenderingMode = 0 Then OptBot.Text = "DEFAULT"
		If RenderingMode = 1 Then OptBot.Text = "STEREO 3D"
		If RenderingMode = 2 Then OptBot.Text = "VR"
	End If
	OptTop.Pack
	OptTop.SetAlignedPosition 127, 1, FlexDMD_Align_TopRight
	'OptTop.SetAlignedPosition 100, 1, FlexDMD_Align_TopRight
	OptBot.SetAlignedPosition 64, 16, FlexDMD_Align_Center
	OptionDMD.UnlockRenderThread
	UpdateMods
End Sub

Sub Options_Toggle(amount)
	If OptionDMD is Nothing Then Exit Sub
	If OptPos = Opt_LUT Then
		ColorLUT = ColorLUT + amount * 1
		If ColorLUT < 1 Then ColorLUT = 11
		If ColorLUT > 11 Then ColorLUT = 1
	ElseIf OptPos = Opt_Volume Then
		VolumeDial = VolumeDial + amount * 0.1
		If VolumeDial < 0 Then VolumeDial = 1
		If VolumeDial > 1 Then VolumeDial = 0
	ElseIf OptPos = Opt_Volume_Ramp Then
		RampRollVolume = RampRollVolume + amount * 0.1
		If RampRollVolume < 0 Then RampRollVolume = 1
		If RampRollVolume > 1 Then RampRollVolume = 0
	ElseIf OptPos = Opt_Volume_Ball Then
		BallRollVolume = BallRollVolume + amount * 0.1
		If BallRollVolume < 0 Then BallRollVolume = 1
		If BallRollVolume > 1 Then BallRollVolume = 0
	ElseIf OptPos = Opt_DynBallShadow Then
		DynamicBallShadowsOn = 1 - DynamicBallShadowsOn
	ElseIf OptPos = Opt_AmbientBallShadow Then
		AmbientBallShadowOn = AmbientBallShadowOn + 1
		If AmbientBallShadowOn > 2 Then AmbientBallShadowOn = 0
'	ElseIf OptPos = Opt_Scoop_Difficulty Then
'		Scoop_Difficulty = Scoop_Difficulty + amount
'		If Scoop_Difficulty < 1 Then Scoop_Difficulty = 3
'		If Scoop_Difficulty > 3 Then Scoop_Difficulty = 1
	End If
End Sub

Sub Options_KeyDown(ByVal keycode)
	If OptSelected Then
		If keycode = LeftMagnaSave Then ' Exit / Cancel
			OptSelected = False
		ElseIf keycode = RightMagnaSave Then ' Enter / Select
			OptSelected = False
		ElseIf keycode = LeftFlipperKey Then ' Next / +
			Options_Toggle	-1
		ElseIf keycode = RightFlipperKey Then ' Prev / -
			Options_Toggle	1
		End If
	Else
		If keycode = LeftMagnaSave Then ' Exit / Cancel
			Options_Close
		ElseIf keycode = RightMagnaSave Then ' Enter / Select
			If OptPos < Opt_Info_1 Then OptSelected = True
		ElseIf keycode = LeftFlipperKey Then ' Next / +
			OptPos = OptPos - 1
			If OptPos < 0 Then OptPos = NOptions - 1
		ElseIf keycode = RightFlipperKey Then ' Prev / -
			OptPos = OptPos + 1
			If OptPos >= NOPtions Then OptPos = 0
		End If
	End If
	Options_OnOptChg
End Sub

Sub Options_Load
	Dim x
    x = LoadValue(cGameName, "LUT") : If x <> "" Then ColorLUT = CInt(x) Else ColorLUT = 1
    x = LoadValue(cGameName, "VOLUME") : If x <> "" Then VolumeDial = CNCDbl(x) Else VolumeDial = 0.8
    x = LoadValue(cGameName, "RAMPVOLUME") : If x <> "" Then RampRollVolume = CNCDbl(x) Else RampRollVolume = 0.5
    x = LoadValue(cGameName, "BALLVOLUME") : If x <> "" Then BallRollVolume = CNCDbl(x) Else BallRollVolume = 0.5
    x = LoadValue(cGameName, "DYNBALLSH") : If x <> "" Then DynamicBallShadowsOn = CInt(x) Else DynamicBallShadowsOn = 1
    x = LoadValue(cGameName, "AMBBALLSH") : If x <> "" Then AmbientBallShadowOn = CInt(x) Else AmbientBallShadowOn = 1
    'x = LoadValue(cGameName, "SCOOPDIFFICULTY") : If x <> "" Then Scoop_Difficulty = CInt(x) Else Scoop_Difficulty = 2
	UpdateMods
End Sub

Sub UpdateMods
	Dim BL, LM, x, y, c, enabled

	'*********************
	'Color LUT
	'*********************

	if ColorLUT = 1 Then Table1.ColorGradeImage = "LUTtotan2022" ' "LUT1_1" '###Flupper
	if ColorLUT = 2 Then Table1.ColorGradeImage = "colorgradelut256x16-10"
	if ColorLUT = 3 Then Table1.ColorGradeImage = "colorgradelut256x16-20"
	if ColorLUT = 4 Then Table1.ColorGradeImage = "colorgradelut256x16-30"
	if ColorLUT = 5 Then Table1.ColorGradeImage = "colorgradelut256x16-40"
	if ColorLUT = 6 Then Table1.ColorGradeImage = "colorgradelut256x16-50"
	if ColorLUT = 7 Then Table1.ColorGradeImage = "colorgradelut256x16-60"
	if ColorLUT = 8 Then Table1.ColorGradeImage = "colorgradelut256x16-70"
	if ColorLUT = 9 Then Table1.ColorGradeImage = "colorgradelut256x16-80"
	if ColorLUT = 10 Then Table1.ColorGradeImage = "colorgradelut256x16-90"
	if ColorLUT = 11 Then Table1.ColorGradeImage = "colorgradelut256x16-100"

End Sub

' Culture neutral string to double conversion (handles situation where you don't know how the string was written)
Function CNCDbl(str)
    Dim strt, Sep, i
    If IsNumeric(str) Then
        CNCDbl = CDbl(str)
    Else
        Sep = Mid(CStr(0.5), 2, 1)
        Select Case Sep
        Case "."
            i = InStr(1, str, ",")
        Case ","
            i = InStr(1, str, ".")
        End Select
        If i = 0 Then     
            CNCDbl = Empty
        Else
            strt = Mid(str, 1, i - 1) & Sep & Mid(str, i + 1)
            If IsNumeric(strt) Then
                CNCDbl = CDbl(strt)
            Else
                CNCDbl = Empty
            End If
        End If
    End If
End Function
'*******************************************
' 	ZVRR: VR Room / VR Cabinet
'*******************************************

'Dim VRThings
'If VRRoom <> 0 Then
'	DMDbackbox.visible = True  ' flasher dmd
'	If VRRoom = 1 Then
'		For Each VRThings In VR_Cab
'			VRThings.visible = 1
'		Next
'	End If
'	If VRRoom = 2 Then
'		For Each VRThings In VR_Cab
'			VRThings.visible = 0
'		Next
'		PinCab_Backglass.visible = 1
'	End If
'Else
'	For Each VRThings In VR_Cab
'		VRThings.visible = 0
'	Next
'End If

''******************************************************
'' 	ZTST:  Debug Shot Tester
''******************************************************
'
''****************************************************************
'' Section; Debug Shot Tester v3.2
''
'' 1.  Raise/Lower outlanes and drain posts by pressing 2 key
'' 2.  Capture and Launch ball, Press and hold one of the buttons (W, E, R, Y, U, I, P, A) below to capture ball by flipper.  Release key to shoot ball
'' 3.  To change the test shot angles, press and hold a key and use Flipper keys to adjust the shot angle.  Shot angles are saved into the User direction as cgamename.txt
'' 4.  Set DebugShotMode = 0 to disable debug shot test code.
''
'' HOW TO INSTALL: Copy all debug* objects from Layer 2 to table and adjust. Copy the Debug Shot Tester code section to the script.
''	Add "DebugShotTableKeyDownCheck keycode" to top of Table1_KeyDown sub and add "DebugShotTableKeyUpCheck keycode" to top of Table1_KeyUp sub
''****************************************************************
'Const DebugShotMode = 1 'Set to 0 to disable.  1 to enable
'Dim DebugKickerForce
'DebugKickerForce = 55
'
'' Enable Disable Outlane and Drain Blocker Wall for debug testing
'Dim DebugBLState
'debug_BLW1.IsDropped = 1
'debug_BLP1.Visible = 0
'debug_BLR1.Visible = 0
'debug_BLW2.IsDropped = 1
'debug_BLP2.Visible = 0
'debug_BLR2.Visible = 0
'debug_BLW3.IsDropped = 1
'debug_BLP3.Visible = 0
'debug_BLR3.Visible = 0
'
'Sub BlockerWalls
'	DebugBLState = (DebugBLState + 1) Mod 4
'	'	debug.print "BlockerWalls"
'	PlaySound ("Start_Button")
'	
'	Select Case DebugBLState
'		Case 0
'			debug_BLW1.IsDropped = 1
'			debug_BLP1.Visible = 0
'			debug_BLR1.Visible = 0
'			debug_BLW2.IsDropped = 1
'			debug_BLP2.Visible = 0
'			debug_BLR2.Visible = 0
'			debug_BLW3.IsDropped = 1
'			debug_BLP3.Visible = 0
'			debug_BLR3.Visible = 0
'			
'		Case 1
'			debug_BLW1.IsDropped = 0
'			debug_BLP1.Visible = 1
'			debug_BLR1.Visible = 1
'			debug_BLW2.IsDropped = 0
'			debug_BLP2.Visible = 1
'			debug_BLR2.Visible = 1
'			debug_BLW3.IsDropped = 0
'			debug_BLP3.Visible = 1
'			debug_BLR3.Visible = 1
'			
'		Case 2
'			debug_BLW1.IsDropped = 0
'			debug_BLP1.Visible = 1
'			debug_BLR1.Visible = 1
'			debug_BLW2.IsDropped = 0
'			debug_BLP2.Visible = 1
'			debug_BLR2.Visible = 1
'			debug_BLW3.IsDropped = 1
'			debug_BLP3.Visible = 0
'			debug_BLR3.Visible = 0
'			
'		Case 3
'			debug_BLW1.IsDropped = 1
'			debug_BLP1.Visible = 0
'			debug_BLR1.Visible = 0
'			debug_BLW2.IsDropped = 1
'			debug_BLP2.Visible = 0
'			debug_BLR2.Visible = 0
'			debug_BLW3.IsDropped = 0
'			debug_BLP3.Visible = 1
'			debug_BLR3.Visible = 1
'	End Select
'End Sub
'
'Sub DebugShotTableKeyDownCheck (Keycode)
'	'Cycle through Outlane/Centerlane blocking posts
'	'-----------------------------------------------
'	If Keycode = 3 Then
'		BlockerWalls
'	End If
'	
'	If DebugShotMode = 1 Then
'		'Capture and launch ball:
'		'	Press and hold one of the buttons (W, E, R, T, Y, U, I, P) below to capture ball by flipper.  Release key to shoot ball
'		'	To change the test shot angles, press and hold a key and use Flipper keys to adjust the shot angle.
'		'--------------------------------------------------------------------------------------------
'		If keycode = 17 Then 'W key
'			debugKicker.enabled = True
'			TestKickerVar = TestKickAngleW
'		End If
'		If keycode = 18 Then 'E key
'			debugKicker.enabled = True
'			TestKickerVar = TestKickAngleE
'		End If
'		If keycode = 19 Then 'R key
'			debugKicker.enabled = True
'			TestKickerVar = TestKickAngleR
'		End If
'		If keycode = 21 Then 'Y key
'			debugKicker.enabled = True
'			TestKickerVar = TestKickAngleY
'		End If
'		If keycode = 22 Then 'U key
'			debugKicker.enabled = True
'			TestKickerVar = TestKickAngleU
'		End If
'		If keycode = 23 Then 'I key
'			debugKicker.enabled = True
'			TestKickerVar = TestKickAngleI
'		End If
'		If keycode = 25 Then 'P key
'			debugKicker.enabled = True
'			TestKickerVar = TestKickAngleP
'		End If
'		If keycode = 30 Then 'A key
'			debugKicker.enabled = True
'			TestKickerVar = TestKickAngleA
'		End If
'		If keycode = 31 Then 'S key
'			debugKicker.enabled = True
'			TestKickerVar = TestKickAngleS
'		End If
'		If keycode = 33 Then 'F key
'			debugKicker.enabled = True
'			TestKickerVar = TestKickAngleF
'		End If
'		If keycode = 34 Then 'G key
'			debugKicker.enabled = True
'			TestKickerVar = TestKickAngleG
'		End If
'		
'		If debugKicker.enabled = True Then		'Use Flippers to adjust angle while holding key
'			If keycode = LeftFlipperKey Then
'				debugKickAim.Visible = True
'				TestKickerVar = TestKickerVar - 1
'				Debug.print TestKickerVar
'			ElseIf keycode = RightFlipperKey Then
'				debugKickAim.Visible = True
'				TestKickerVar = TestKickerVar + 1
'				Debug.print TestKickerVar
'			End If
'			debugKickAim.ObjRotz = TestKickerVar
'		End If
'	End If
'End Sub
'
'
'Sub DebugShotTableKeyUpCheck (Keycode)
'	' Capture and launch ball:
'	' Release to shoot ball. Set up angle and force as needed for each shot.
'	'--------------------------------------------------------------------------------------------
'	If DebugShotMode = 1 Then
'		If keycode = 17 Then 'W key
'			TestKickAngleW = TestKickerVar
'			debugKicker.kick TestKickAngleW, DebugKickerForce
'			debugKicker.enabled = False
'		End If
'		If keycode = 18 Then 'E key
'			TestKickAngleE = TestKickerVar
'			debugKicker.kick TestKickAngleE, DebugKickerForce
'			debugKicker.enabled = False
'		End If
'		If keycode = 19 Then 'R key
'			TestKickAngleR = TestKickerVar
'			debugKicker.kick TestKickAngleR, DebugKickerForce
'			debugKicker.enabled = False
'		End If
'		If keycode = 21 Then 'Y key
'			TestKickAngleY = TestKickerVar
'			debugKicker.kick TestKickAngleY, DebugKickerForce
'			debugKicker.enabled = False
'		End If
'		If keycode = 22 Then 'U key
'			TestKickAngleU = TestKickerVar
'			debugKicker.kick TestKickAngleU, DebugKickerForce
'			debugKicker.enabled = False
'		End If
'		If keycode = 23 Then 'I key
'			TestKickAngleI = TestKickerVar
'			debugKicker.kick TestKickAngleI, DebugKickerForce
'			debugKicker.enabled = False
'		End If
'		If keycode = 25 Then 'P key
'			TestKickAngleP = TestKickerVar
'			debugKicker.kick TestKickAngleP, DebugKickerForce
'			debugKicker.enabled = False
'		End If
'		If keycode = 30 Then 'A key
'			TestKickAngleA = TestKickerVar
'			debugKicker.kick TestKickAngleA, DebugKickerForce
'			debugKicker.enabled = False
'		End If
'		If keycode = 31 Then 'S key
'			TestKickAngleS = TestKickerVar
'			debugKicker.kick TestKickAngleS, DebugKickerForce
'			debugKicker.enabled = False
'		End If
'		If keycode = 33 Then 'F key
'			TestKickAngleF = TestKickerVar
'			debugKicker.kick TestKickAngleF, DebugKickerForce
'			debugKicker.enabled = False
'		End If
'		If keycode = 34 Then 'G key
'			TestKickAngleG = TestKickerVar
'			debugKicker.kick TestKickAngleG, DebugKickerForce
'			debugKicker.enabled = False
'		End If
'		
'		'		EXAMPLE CODE to set up key to cycle through 3 predefined shots
'		'		If keycode = 17 Then	 'Cycle through all left target shots
'		'			If TestKickerAngle = -28 then
'		'				TestKickerAngle = -24
'		'			ElseIf TestKickerAngle = -24 Then
'		'				TestKickerAngle = -19
'		'			Else
'		'				TestKickerAngle = -28
'		'			End If
'		'			debugKicker.kick TestKickerAngle, DebugKickerForce: debugKicker.enabled = false			 'W key
'		'		End If
'		
'	End If
'	
'	If (debugKicker.enabled = False And debugKickAim.Visible = True) Then 'Save Angle changes
'		debugKickAim.Visible = False
'		SaveTestKickAngles
'	End If
'End Sub
'
'Dim TestKickerAngle, TestKickerAngle2, TestKickerVar, TeskKickKey, TestKickForce
'Dim TestKickAngleWDefault, TestKickAngleEDefault, TestKickAngleRDefault, TestKickAngleYDefault, TestKickAngleUDefault, TestKickAngleIDefault
'Dim TestKickAnglePDefault, TestKickAngleADefault, TestKickAngleSDefault, TestKickAngleFDefault, TestKickAngleGDefault
'Dim TestKickAngleW, TestKickAngleE, TestKickAngleR, TestKickAngleY, TestKickAngleU, TestKickAngleI
'Dim TestKickAngleP, TestKickAngleA, TestKickAngleS, TestKickAngleF, TestKickAngleG
'TestKickAngleWDefault =  - 27
'TestKickAngleEDefault =  - 20
'TestKickAngleRDefault =  - 14
'TestKickAngleYDefault =  - 8
'TestKickAngleUDefault =  - 3
'TestKickAngleIDefault = 1
'TestKickAnglePDefault = 5
'TestKickAngleADefault = 11
'TestKickAngleSDefault = 17
'TestKickAngleFDefault = 19
'TestKickAngleGDefault = 5
'If DebugShotMode = 1 Then LoadTestKickAngles
'
'Sub SaveTestKickAngles
'	Dim FileObj, OutFile
'	Set FileObj = CreateObject("Scripting.FileSystemObject")
'	If Not FileObj.FolderExists(UserDirectory) Then Exit Sub
'	Set OutFile = FileObj.CreateTextFile(UserDirectory & cGameName & ".txt", True)
'	
'	OutFile.WriteLine TestKickAngleW
'	OutFile.WriteLine TestKickAngleE
'	OutFile.WriteLine TestKickAngleR
'	OutFile.WriteLine TestKickAngleY
'	OutFile.WriteLine TestKickAngleU
'	OutFile.WriteLine TestKickAngleI
'	OutFile.WriteLine TestKickAngleP
'	OutFile.WriteLine TestKickAngleA
'	OutFile.WriteLine TestKickAngleS
'	OutFile.WriteLine TestKickAngleF
'	OutFile.WriteLine TestKickAngleG
'	OutFile.Close
'	
'	Set OutFile = Nothing
'	Set FileObj = Nothing
'End Sub
'
'Sub LoadTestKickAngles
'	Dim FileObj, OutFile, TextStr
'	
'	Set FileObj = CreateObject("Scripting.FileSystemObject")
'	If Not FileObj.FolderExists(UserDirectory) Then
'		MsgBox "User directory missing"
'		Exit Sub
'	End If
'	
'	If FileObj.FileExists(UserDirectory & cGameName & ".txt") Then
'		Set OutFile = FileObj.GetFile(UserDirectory & cGameName & ".txt")
'		Set TextStr = OutFile.OpenAsTextStream(1,0)
'		If (TextStr.AtEndOfStream = True) Then
'			Exit Sub
'		End If
'		
'		TestKickAngleW = TextStr.ReadLine
'		TestKickAngleE = TextStr.ReadLine
'		TestKickAngleR = TextStr.ReadLine
'		TestKickAngleY = TextStr.ReadLine
'		TestKickAngleU = TextStr.ReadLine
'		TestKickAngleI = TextStr.ReadLine
'		TestKickAngleP = TextStr.ReadLine
'		TestKickAngleA = TextStr.ReadLine
'		TestKickAngleS = TextStr.ReadLine
'		TestKickAngleF = TextStr.ReadLine
'		TestKickAngleG = TextStr.ReadLine
'		TextStr.Close
'	Else
'		'create file
'		TestKickAngleW = TestKickAngleWDefault
'		TestKickAngleE = TestKickAngleEDefault
'		TestKickAngleR = TestKickAngleRDefault
'		TestKickAngleY = TestKickAngleYDefault
'		TestKickAngleU = TestKickAngleUDefault
'		TestKickAngleI = TestKickAngleIDefault
'		TestKickAngleP = TestKickAnglePDefault
'		TestKickAngleA = TestKickAngleADefault
'		TestKickAngleS = TestKickAngleSDefault
'		TestKickAngleF = TestKickAngleFDefault
'		TestKickAngleG = TestKickAngleGDefault
'		SaveTestKickAngles
'	End If
'	
'	Set OutFile = Nothing
'	Set FileObj = Nothing
'	
'End Sub
''****************************************************************
'' End of Section; Debug Shot Tester 3.2
''****************************************************************

' VR PLUNGER ANIMATION
Sub TimerPlunger_Timer
	If VR_Primary_plunger.Y < 35 then
		VR_Primary_plunger.Y = VR_Primary_plunger.Y + 3.5
	End If
End Sub

Sub TimerPlunger2_Timer
	VR_Primary_plunger.Y = -65 + (5* Plunger.Position) -20
End Sub

Sub ramptrigger09_Animate()
	
End Sub