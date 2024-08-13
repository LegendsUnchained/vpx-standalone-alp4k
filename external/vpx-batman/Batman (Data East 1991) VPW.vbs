'##############################################################################
'##############################################################################
'#######                                                               ########
'#######							Batman                             ########
'#######  					   (Data East 1991)                        ########
'#######                                                               ########
'##############################################################################
'##############################################################################
'
'VPW Table Tuneup Mod v1.0
'Based on Javiers VPX version, some assets from Dark and the original playfield by 85vett.
'
'	** VPW Mod V1.0 - CHANGE LOG **
'**************************************
'		VPin Workshop Revisions
'**************************************
' Started from Skitso mod
' 002 cyberpez - All table objects and playfield re-aligned, altered and resized to correct proportions.
' 003 skitso - visual pass
' 003 cyberpez - Added primitive playfield, Reworked Joker Ramp and trough (Primitives - got rid of fall through kickers) also reworked musuim kicker.  Chanaged playfield image to one without transparent holes.
' 006 baldgeek - Added Fleep Sounds
' 007 fluffhead35 - nFozzy physics
' 008 skitso - repositioned and tweaked lower playfield GI lights, remade gangster inserts, batman face and logo inserts and moon insert with skitso style, removed primitive playfield that was of wrong size.
' 009 skitso - repositioned rest of the GI lights, made bat cave toy opaque, made flugelheim toy disable light from behind, fixed ton of depth bias issues around right plastic ramp. Cyberpez added mesh playfield back.
' 010 cyberpez - added nFozzy lighting script. Primitive inserts.  Flupper Flasher Domes, sixtoe-tweaked plastic ramp.  Probably other things.
' 011 skitso - redone joker ramp lighting and flashers, improved left side dome flashers, improved top flashers, added proper bumper lights, tweaked insert primitive material colors, slight tone tweak to PF, improved skitso style inserts (moon, batman face and bat logo), Added shadow to plunger lane and bat mobile.
' 013 skitso - improved backwall flashers (better color, a tad thicker font for better readability), improved left flashers, tweaked gangster inserts, small tweak to top bumper light
' 014 gtxjoe - add debug shot tester (Press 2 for outlane blocker, Press and hold key to test shots: w e r y u i p a s f g h.  While holding a key, use flippers to adjust shot angle )
' 014b cyberpez - reworked museum again.  I think its working.
' 015 skitso - remade bat cave flasher, tweaked blue insert_on material
' 017 iaakki - 3 prim insert rework
' 018 iaakki - 3 prim insert rework continued
' 019 iaakki - 3 prim insert rework continued...
' 020 skitso - tweaked Joker ramp decals to have more pink tone. (warm lighting subdues the change though), improved right hand gangster insert (skitso style) and flasher, tweaked bumper lights more more accuracy, moved Flugelheim walls to more correct position and added shadow.
' 021c tomate - collidable ramp fixed, alternative apron adedd
' 022 skitso - tweaked iaakki's middle PF square 3 prim flashers. Added 2 arrow 3 prim flashers (first try, please be gentle)
' 023 iaakki - apron scaled, rubberizer added, targetbouncer added to posts and sleeves, live catch fixed, rubbers hit treshold fixed, slings tuned, flipper angles and sizes fixed, batcave texture swap disabled
' 024 Sixtoe - Fixed left flasher flare positions, added VR cabinet, room and modes, added blocker wall for left middle plastic as ball can jump over and behind it, aligned a couple of handful of metals, aligned top middle left flasher decal, removed collidable from a handful of things, aligned batmobile in shooter lane, aligned and resized batcave a bit (it was very tall in VR), adjusted triggers (dropped and lengthened), adjusted apron wall and made visible for VR, adjusted apron guard prim, adjusted depth bias of shooter land plastic to -1000
' 025 skitso - added skitso style insert "see through" effect around center playfield square bat tv inserts, 'joker's mouth 4 million' insert, green multiplier inserts and 'shoot again' insert at the bottom of the table.
' 026 skitso - remade the two green lamps on top of the joker ramp, repositioned orange flasher domes.
' 027 cyberpez - started to change out nut bolts and things.  Fixed plunger lane.  Added ramp tweak to cave entrance .  Started changing diverter.
' 028 skitso - set slope to 5.2, bumper force set to 9. Tweaked left dome flashers, moved multiplier insert prims to z-1, fixed million plus and batlogo flashers on top of the batcave, improved bat logo lamp on flugelheim front, improved plunger lane red bulb, resized all GI lights to new table measurements, fixed texture swap for bumpers and Flugelheim flashers, improved Flugelheim flashers, tweaked bumper off texture, made Flugelheim and dome flashers reflect from batcave, small tweaks, iaakki improved batcave fading and fixed flipper nudge values. 
' 029 cyberpez - Changed plunger release speed to 90.  Change slings to 4.  Lowered back wall flashes.  Added rails and primitive flasher bulbs under Joker.
' 030 iaakki - Flugelgugel flashers reworked to have own timer. Made it swap PF flash to different depending to port state. Levels to be adjusted
' 031 skitso - further tweaked Flugelheim flashers and added side blade reflection, tweaked Joker ramp flashers, improved Flugelheim rising wall texture, fixed sticky plungerlane
' 032 cyberpez - Adjusted clear plastics.  More nuts / bolts / screws.  Made top left gate/bracket custom primitive and animated.  Maybe rotated left orage domes to line up with holes. Shifted Museum back and left a bit.  Adjusted Museum trough, hope it fixed 3ball multiball. Adjusted rubber on the small random posts by bumpers.  Adjusted top side of lane guides.  
' 033 skitso - fixed museum flasher locations, fixed top right ramp/protector depth bias issue, removed ball reflection status from few GI bulbs, fixed jackpot insert below Flugelheim. Added a slideblade reflection to left dome flashers (code missing)
' 034 iaakki - flugelheim pTrough primitive adjusted to work better for 3-ball mb, bar hit sound added, left domereflection added to code
' 035 skitso - tweaked left dome reflection
' 036 skitso - fixed minor flasher clipping under Joker ramp, fixed clipping decal on top skill shot flasher dome, improved w/lit insert flasher
' 037 cyberpez - shrunk museum a bit. More nuts and bolts and things.  adjusted switch 28 and 29 posistion, animated primitive gates.  Animated switches for Joker Eye's and Mouth.  Tweaks to diverter.
' 038 skitso - resized and moved museum flashers once more. Removed one stray peg inside museum, made Joker mouth flasher visible through the mouth hole.
' 039 iaakki - rtx ball shadow code included with ramp rolling etc. Some depth bias or z-order issues visible. Shadows are too dark on purpose. Should be reduced once it work
' 040 skitso - resized insert prims to correct size and shape. Removed playfield reflections from insert prims and a bunch of other crap that didn't need it for a hefty performance boost
' 041 iaakki - NormalMapped inserts only in VR mode, right ramp sleeves fixed, shadow depth bias debugged etc
' 042 cyberpez - orginization baby.  Also tweaked bumper "ring drop offset.  changed out "random" posts on either side of bumper area.
' 043 apophis - Fixed the Fleep installation. Fixed some ballshadow issues...shadows are still bugging on insert primitives. Cleaned up the script a bit.
' 044 Wylte - Fixed shadows, with update to latest primitive/image/materials.  Updated RollingUpdate sub with latest shadow code.  Thinned shadows slightly
' 045 fluffhead35 - Added BallPitchV Function and used for Plastic Ramp Sounds.  Changed TargetBouncerFactor to 1.5. Added RubbersD point to make microbounces happen  
' 046 fluffhead35 - Updated PlasticRamp Sound and amplified the volume of the sound. 
' 047 fluffhead35 - Updated BallRoll Sound and amplified the volume of the sound.  Fixed BallPitchV as it was on a BallRoll sound and put it on plastic ramp sound.  Added in ligic by oqqsan to stop ball rolling sound at no balls in play.
' 048 fluffhead35 - Reverted TargetBouncer Logic to iaakki version.  Changed BallRoll Sound to use version with lower amplification. Enabled sw14 for BIPL
' 049 oqqsan - Fix for poltergeist ball at plunger (forgot to save?)
' 050 Wylte - Implemented ".  Commented out dynamic shadow z live updates, left ambient for ramps.  Changed recommended TargetBouncerFactor maximum to 1.5
' 051 fluffhead35 - Implemented option BallRollAmpFactor to set amplification factor for BallRoll Sounds.
' 052 cyberpez -  added a couple stragic walls to help stop stuck balls.  Added wall under Flugelgugel to stop clipping of plastic.  Stop ball collison bellow 0 Z. Started adding options.
' 053 Sixtoe - New playfield mesh, adjusted playfield hole primitives and added new playfield edge texture, fixed lots of VR depth bias issues, hooked up plunger, changed black nut material to black powdercoat to make it slightly mmore visible, adjusted primtives to line up better in 3D, adjusted collidable primtivies so they line up correctly, adjusted sideblade flashers to light orange-ish so it doesn't blow out with pure white anymore, added cut down museum flasher so it doesn't stick out of the cabinet. Split rubber collidables where they have a post in the middle, probably some other stuff I've forgotten...
' 054 cyberpez - copied LUT swapper from tftc, JokerRampFlasher mod.  halfpost mod. rubber color mod. added a couple missing screws and adjusted floating rubbers.  
' 055 tomate - New apron added, cab POV changed
' 056 Skitso - fixed default LUT (vpx original) to what it was earlier, tweaked apron texture and disable lighting. Added subtle shadow to the plunger lane, below bat mobile/apron. Set white rubbers as default. Made final visual pass with tiny tweaks here and there.
' 057/8 Sixtoe - Various tweaks and fixed, desktop pov fixed, script cleaned, rails fixed in desktop, playfield mesh error fixed, lights cut to shape
' 059 Wylte - Gave 4 dome flashers images for 10.7, set TestMode to 0 by default, placed blockerwalls within testmode, testing higher flasher intensity
' 060 Leojreimroc - Implemented VR Backglass Flashers and GI.  Raised apron wall for VR.  Fixed VR plunger.
' 061 Leojreimroc - Backglass Flashers are now a separate option.  LUT Filename changed. Added LUT sounds.
' 062 Sixtoe - Added cabinet mode, other tweaks

Option Explicit
Randomize

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8

'///////////////////////-----Rubberizer-----////////////////////
Const RubberizerEnabled = 1		'0 = normal flip rubber, 1 = more lively rubber for flips

'///////////////////////-----Target Bouncer-----////////////////////
Const TargetBouncerEnabled = 1	'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 1.5	'Level of bounces. 0.2 - 1.5 is probably usable value.

'/////////////////////-----Ball Shadows-----/////////////////////
Const DynamicBallShadowsOn = 1	'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

'/////////////////////-----BallRoll Sound Amplification -----/////////////////////
Const BallRollAmpFactor = 2		'0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)

'///////////////////////-----VR Room-----///////////////////////
Const VRRoom  = 0				'0 - VR Room off, 1 - Minimal Room, 2 - Ultra Minimal

'///////////////////////-----Cabinet Mode-----///////////////////////
Const CabinetMode  = 1			'0 - Rails On, 1 - Rails Off

'///////////////////////-----VR Flashing Backglass-----///////////////////////
Const BackglassFlash = 0		'0 - Static Backglass  1- Flashing Backglass


''''''''''''''''''''''''
' Other Visual Options '
''''''''''''''''''''''''

'Rubber Color
'0=white
'1-black
RubberColor = 0


'Flipper Color
'0=white/black
'1=yellow/black
FlipperColor = 1


'Half Posts
'0=no
'1=yes
HalfPosts = 0


'Joker Ramp Flasher Color
'0=Original
'1=Green
'2=GreenRed
JokerFlasherColor = 0


'***********	Set the default LUT set	*********************************

'You can change LUT option within game with left and right CTRL keys
Dim LUTset, DisableLUTSelector, LutToggleSound
LoadLUT
'LUTset = 11	'override saved LUT for debug
SetLUT

DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1
LutToggleSound = 1		' Enables sound when changing LUT lighting.  Switch to 0 to disable.


'LUTset Types:
'0 = Fleep Natural Dark 1
'1 = Fleep Natural Dark 2
'2 = Fleep Warm Dark
'3 = Fleep Warm Bright
'4 = Fleep Warm Vivid Soft
'5 = Fleep Warm Vivid Hard
'6 = Skitso Natural and Balanced
'7 = Skitso Natural High Contrast
'8 = 3rdaxis Referenced THX Standard
'9 = CalleV Punchy Brightness and Contrast
'10 = TT & Ninuzzu Original
'11 = VPW Orig
'12 = VPW LUT1on1
'13 = VPW LUTbassgeige1
'14 = VPW LUTblacklight

'///////////////////////-----End Of Options-----///////////////////////

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const Ballsize = 50
Const BallMass = 1

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD
If VRRoom <> 1 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode

LoadVPM "01120100", "DE.VBS", 3.36

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0
Const cSingleLFlip = 0
Const cSingleRFlip = 0

' Standard Sounds
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""

'Solenoids
SolCallback(1)  = "bsTrough.SolIn"								'6-ball lockout										(1L)
SolCallback(2)  = "bsTrough.SolOut"								'ball eject											(2L)
SolCallback(3)  = "ScoopKickL"									'Left Scoop                          				(3L)
SolCallback(4)  = "SolAutoPlungerIM"							'Autolaunch                            		 		(4L)
'NOT USED																											(5L)
SolCallback(6)	= "ScoopKickR"									'Right Scoop		                    			(6L)
'NOT USED																											(7L)
SolCallback(8)  = "Solknocker"									'Knocker							  				(8L)
SolCallback(9)  = "Sol9"										'FlashLamp x4 (2 backbox + 2 pf)					(09)
'SolCallback(10) = ""											'L/R Relay 											(10)
SolCallback(11) = "Sol11"										'GI Relay                             				(11)
SolCallback(12) = "Sol12"                          		 		'FlashLamp x4 (1 pf + 3 backbox						(12)
SolCallback(13) = "Sol13"										'FlashLamp x4 (2 pf + 2 backbox)  					(13)
SolCallBack(14) = "Sol14"                          				'FlashLamp x4 (1 pf + 3 backbox)					(14)
'SolCallBack(15) = ""											'Ticket Dispenser									(15)
'SolCallBack(16) = ""											'Bar Motor											(16)
'SolCallBack(17) = ""											'Left Bumper										(17)
'SolCallBack(18) = ""											'Center Bumper										(18)
'SolCallBack(19) = ""			 								'Right Bumper										(19)
'SolCallBack(20) = ""											'Left Slingshot										(20)
'SolCallBack(21) = ""											'Right Slingshot									(21)
SolCallback(22) = "SolDiv"                           			'Ramp Diverter                      				(22)
'NOT USED																											(23)
'NOT USED																											(24)
SolCallback(25) = "Sol25"										'Flashlamp X4 (3 pf + backbox)						(1R)
SolCallback(26) = "Sol26"										'Flashlamp X4 (1 pf + 2 ramp + 1 backbox)			(2R)
SolCallback(27) = "Sol27"										'Flashlamp X4 (2 pf + 2 backbox)					(3R)
SolCallback(28) = "Sol28"										'Flashlamp X4 (2 pf + 2 backbox)					(4R)
SolCallback(29) = "SetLamp 129,"								'Flashlamp X4 (4 pf)								(5R)
SolCallback(30) = "Sol30"										'Flashlamp X4 (3 pf + 1 backbox)					(6R)
SolCallback(31) = "Sol31"										'Flashlamp X4 (3 pf + 1 backbox)					(7R)
SolCallBack(32) = "Flash132"		'"SetLamp 132,"     		'Flashlamp X4 (2 bat + 2 backbox)					(8R)

SolCallback(46) = "SolRFlipper"                         		'Right Flipper
SolCallback(48) = "SolLFlipper"                         		'Left Flipper

' *************************************
' ***** VR Backglass Flasher Objects **
' *************************************
'
'SolCallback(9)  = "Sol9"  'Moon
'SolCallback(11) = "Sol11" 'GI
'SolCallback(12) = "Sol12" '"B","M",City right
'SolCallback(13) = "Sol13" '"A","A"
'SolCallback(14) = "Sol14" '"T","M", Batmobile
'SolCallback(25) = "Sol25" 'City Right
'SolCallback(26) = "Sol26" 'Moon
'SolCallback(27) = "Sol27" 'Vicki,Batman
'SolCallback(28) = "Sol28" 'Batman Chest logo
'SolCallback(30) = "Sol30" 'Joker
'SolCallback(31) = "Sol31" 'City left
'SolCallback(32) = "Sol32" 'Top Right Windows

'************
' Table init.
'************

Const cGameName = "btmn_106"

Dim bsTrough, plungerIM, bsLScoop, bsRScoop, mBar
Dim activeShadowBall

Sub Table1_Init
	vpmInit Me
	With Controller
		.GameName = cGameName
		.Games(cGameName).Settings.Value("sound") = 1
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Batman, Data East 1991" & vbNewLine & "VPW"
        .Games(cGameName).Settings.Value("rol") = 0
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

	' Nudging
	vpmNudge.TiltSwitch = 1
	vpmNudge.Sensitivity = 2 
	vpmNudge.tiltobj = Array(LeftSlingShot,RightSlingShot,Bumper1B,Bumper2B,Bumper3B)

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	' Trough
	Set bsTrough = new cvpmTrough 
	With bsTrough
		.Size = 3
		.InitSwitches Array (13,12,11)
		.EntrySw = 10
		.InitExit BallRelease, 90, 6
		.InitEntrySounds "Drain_1", SoundFX(SSolenoidOn,DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
		'.InitExitSounds  SoundFX(SSolenoidOn,DOFContactors), SoundFX("fx_ballrel",DOFContactors)
		.Balls = 3
		.CreateEvents "bsTrough", Drain
	End With

	' Scoop Left
	'	Set bsLScoop = New cvpmSaucer
	'	With bsLScoop
	'	    .InitKicker Sw39b, 39,50, 30, 1.56
	'		.InitSounds "scoopenter", SoundFX(SSolenoidOn,DOFContactors), SoundFX("salidadebola",DOFContactors)
	'		.CreateEvents "bsLScoop", sw39b
	'    End With

	' Scoop Right
	'	Set bsRScoop = New cvpmTrough
	'	With bsRScoop
	'		.Size = 2
	'		.InitSwitches Array (52,53)
	'		.InitExit Sw52, 192, 25
	'		.InitEntrySounds "fx_chapa", SoundFX(SSolenoidOn,DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
	'		.InitExitSounds  SoundFX(SSolenoidOn,DOFContactors), SoundFX("salidadebola",DOFContactors)
	'		.Balls = 0
	'    End With

	' Impulse Plunger
	Const IMPowerSetting = 55
	Const IMTime = 0.6
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swplunger, IMPowerSetting, IMTime
		.Random 0.3
		.switch 14
		'.InitExitSnd SoundFX("bumper_retro",DOFContactors), SoundFX("fx_target",DOFContactors)
		.CreateEvents "plungerIM"
	End With

	' Bar Motor
	set mBar = new cvpmMech
	with mBar
		.MType = vpmMechOneSol + vpmMechReverse + vpmMechNonLinear 
		.Sol1 = 16
		.Length = 130
		.Steps = 50
		.addsw 50,0,0
		.addsw 51,47,49
		.acc=0
		.ret=0
		.Callback = GetRef("UpdateBar")
		.Start
	End with

	Set activeShadowBall = new cvpmDictionary

	Controller.Switch(50) = 0 
	Controller.Switch(51) = 1

	RampDiverter.Isdropped = 1

	SetOptions

	'If Table1.ShowDT = False then
	'Ramp26.visible = 0
	' Ramp32.visible = 0
	'Ramp33.visible = 0
	'Ramp34.visible = 0
	'Ramp35.visible = 0
	'Ramp36.visible = 0
	'End If

End Sub

'******************
'Keys Up and Down
'*****************

Sub Table1_KeyDown(ByVal Keycode)
	TestTableKeyDownCheck keycode	

	If keycode = PlungerKey Then Plunger.Pullback:SoundPlungerPull()
	If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 6:SoundNudgeCenter()

'	If keycode = LeftMagnaSave Then Flash129 true
'	If keycode = RightMagnaSave Then Flash132 true



	If keycode = RightMagnaSave Then 'AXS 'Fleep
'		Flash132
		if DisableLUTSelector = 0 then
			If LutToggleSound Then
				Playsound "Click"
'				Playsound "LUT_Toggle_Up_Front", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
'				Playsound "LUT_Toggle_Up_Rear", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, -1
			End If
            LUTSet = LUTSet  + 1
			if LutSet > 14 then LUTSet = 0
			SetLUT
			ShowLUT
		end if
	end if
	If keycode = LeftMagnaSave Then
'		Flash129
		if DisableLUTSelector = 0 then
			If LutToggleSound Then
				Playsound "Click"
'				Playsound "LUT_Toggle_Down_Front", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
'				Playsound "LUT_Toggle_Down_Rear", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, -1
			End If
			LUTSet = LUTSet - 1
			if LutSet < 0 then LUTSet = 14
			SetLUT
			ShowLUT
		end if
	end if

	If keycode = PlungerKey Then  
		If VRRoom = 1 Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
	End If


	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

		End Select
	End If

	If keycode=StartGameKey then soundStartButton()

	'** nFozzy - begin
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress
	'** nFozzy - end
	If vpmKeyDown(keycode) Then Exit Sub
End Sub


Sub Table1_KeyUp(ByVal Keycode)
	TestTableKeyUpCheck keycode

	If KeyCode = PlungerKey Then
		Plunger.Fire
		If VRRoom = 1 Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
		End If
		If BallInPlungerLane = 1 Then
			SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
		End If
	End If

	'** nFozzy - begin
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
	'** nFozzy - end

	If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub Table1_Paused : Controller.Pause = True : End Sub
Sub Table1_unPaused : Controller.Pause = False : End Sub
Sub Table1_Exit() : Controller.Pause = False : Controller.Stop() : End Sub







' #####################################
' ###### Flupper Domes            #####
' #####################################


Sub Flash25(Enabled)
	Objlevel(1) = 1 : FlasherFlash1_Timer
End Sub

Sub Flash26(Enabled)
	Objlevel(2) = 1 : FlasherFlash2_Timer
End Sub

Sub Flash27(Enabled)
	Objlevel(3) = 1 : FlasherFlash3_Timer
End Sub

Sub Flash28(Enabled)
	Objlevel(4) = 1 : FlasherFlash4_Timer
End Sub

Sub Flash29(Enabled)
	Objlevel(5) = 1 : FlasherFlash5_Timer
End Sub

Sub Flash129(Enabled)
	Objlevel(6) = 1 : FlasherFlash6_Timer
	Objlevel(7) = 1 : FlasherFlash7_Timer
	Objlevel(8) = 1 : FlasherFlash8_Timer
	Objlevel(9) = 1 : FlasherFlash9_Timer
End Sub



dim Flash132level
sub Flash132(flstate)
	If Flstate Then
		Flash132level = 1
		museumflash_timer
		If BackglassFlash = 1 Then
			BGfl32.visible = 1
			BGFlAreaTopRightL.amount = 250
			BGFlAreaTopRightR.amount = 250
		End If
	else
		Flash132level = Flash132level * 0.7 'minor tweak to force faster fade
		If BackglassFlash = 1 Then
			BGfl32.visible = 0	
			BGFlAreaTopRightL.amount = 100
			BGFlAreaTopRightR.amount = 100
		End If
	End If
end sub

FlasherLight8a.IntensityScale = 0:FlasherLight8b.IntensityScale = 0
Flasher8.opacity = 0:Flasher8a.opacity = 0:museumflash.opacity = 0:museumflash1.opacity = 0:museumflash2.opacity = 0
Primitive_museum.blenddisablelighting = 1
primitive53.blenddisablelighting = 0.77

sub museumflash_timer
	If not museumflash.TimerEnabled Then museumflash.TimerEnabled = True : End If

	FlasherLight8a.IntensityScale = 5 * Flash132level^2
	FlasherLight8b.IntensityScale = 5 * Flash132level^2
	Flasher8.opacity = 100 * Flash132level^1.5
	Flasher8a.opacity = 100 * Flash132level^1.5
	museumflash.opacity = 100 * Flash132level^1.5
	museumflash1.opacity = 100 * Flash132level^1.5
	museumflash2.opacity = 100 * Flash132level^1.5

	if Flash132level < 0.3 then
		Primitive_museum.image="MuseumMap_off"
		primitive53.image = "batcave_completemap"
	elseif Flash132level > 0.3 And Flash132level < 0.55 then 
		Primitive_museum.image="MuseumMap_33"
		primitive53.image = "batcaveright_33"
	elseif Flash132level > 0.55 And Flash132level < 0.85 then 
		Primitive_museum.image="MuseumMap_66"
		primitive53.image = "batcaveright_66"
	else 
		Primitive_museum.image="MuseumMap_on"
		primitive53.image = "batcaveright"
	end if

	Flash132level = Flash132level * 0.95 - 0.01

	If Flash132level < 0 Then museumflash.TimerEnabled = False : End If
end sub







Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = .4		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = .4		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.7		' *** brightness of the flasher dome when switched off (range 0-2)	***
' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "Orange" : InitFlasher 2, "Orange" : InitFlasher 3, "Orange"
InitFlasher 4, "White" : InitFlasher 5, "red" : InitFlasher 6, "Orange"
InitFlasher 7, "Orange" : InitFlasher 8, "Orange"
InitFlasher 9, "Orange" :' InitFlasher 10, "red" : InitFlasher 11, "white" 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : 
RotateFlasher 6,-30:RotateFlasher 7,-30 : RotateFlasher 8,-30 :RotateFlasher 9,-30' : RotateFlasher 10,90 : RotateFlasher 11,90

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
		Flasherflash6.height = 75
		Flasherflash7.height = 75
		Flasherflash8.height = 75
		Flasherflash9.height = 75
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
		Case "orange" :  objlight(nr).color = RGB(255,128,0) : objflasher(nr).color = RGB(255,128,0)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 

Sub FlasherFlash6_Timer()
	FlashFlasher(6)
End Sub

Sub FlasherFlash7_Timer()
	FlashFlasher(7)
End Sub

domereflection.opacity = 0

Sub FlasherFlash8_Timer()
	FlashFlasher(8)
	domereflection.opacity = 100 * ObjLevel(8)^1
	flashBatCave
End Sub

Sub FlasherFlash9_Timer()
	FlashFlasher(9)
End Sub

sub flashBatCave
	if ObjLevel(8) < 0.4 then
		Primitive53.image="BatCave_CompleteMap"
	elseif ObjLevel(8) > 0.4 And ObjLevel(8) < 0.65 then 
		Primitive53.image="Batcaveleft33"
	elseif ObjLevel(8) > 0.65 And ObjLevel(8) < 0.85 then 
		Primitive53.image="Batcaveleft66"
	else 
		Primitive53.image="Batcaveleft"
	end if
End Sub


Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub

' #####################################
' ###### End Flupper Domes        #####
' #####################################




'' set options cp

Dim HalfPosts, FlipperColor, RubberColor, JokerFlasherColor

Dim xxRubberColor
Sub SetOptions ()

'Rubber Color
'0=white
'1-black

	If RubberColor = 1 Then
		for each xxRubberColor in RubbersVisible
			xxRubberColor.material = "Rubber Black"
			next
	Else
		for each xxRubberColor in RubbersVisible
			xxRubberColor.material = "Rubber White"
			next
	End If

'Flipper Color
'0=white/black
'1=yellow/black

	If FlipperColor = 1 Then
		RFLogo.Image = "Yellow_black"
		LFLogo.Image = "Yellow_black"
	Else
		RFLogo.Image = "white_black"
		LFLogo.Image = "white_black"
	End If

'Half Posts
'0=no
'1=yes

	If HalfPosts = 1 Then
		pHalfPost.Visible = true
		pHalfPost001.Visible = true
		pHalfPost002.Visible = true
		pHalfPost003.Visible = true
	Else
		pHalfPost.Visible = false
		pHalfPost001.Visible = false
		pHalfPost002.Visible = false
		pHalfPost003.Visible = false
	End If



	If JokerFlasherColor = 1 Then
		'Left
		L81P005.material = "Metal Green2"
		l36.Color=RGB(0,128,0) 'Green
		l36Fb2.Color=RGB(0,128,0) 'Green
		l36b.Color=RGB(0,128,0) 'Green
		l36b.ColorFull=RGB(0,255,0) 'Green
		l36Fb.Color=RGB(0,128,0) 'Green
		'Right
		L81P001.material = "Metal Green2"
		l37.Color=RGB(0,128,0) 'Green
		l37Fb.Color=RGB(0,128,0) 'Green
		l37b.Color=RGB(0,128,0) 'Green
		l37b.ColorFull=RGB(0,255,0) 'Green
	End If

	If JokerFlasherColor = 2 Then
		'Left
		L81P005.material = "Metal Green2"
		l36.Color=RGB(0,128,0) 'Green
		l36Fb2.Color=RGB(0,128,0) 'Green
		l36b.Color=RGB(0,128,0) 'Green
		l36b.ColorFull=RGB(0,255,0) 'Green
		l36Fb.Color=RGB(0,128,0) 'Green
		'Right
		L81P001.material = "Metal Red"
		l37.color = rgb(255,0,0) ' Red
		l37Fb.color = rgb(255,0,0) ' Red
		l37b.color = rgb(255,0,0) ' Red
		l37b.colorfull = rgb(128,0,0) 'Red
	
	End If

End Sub





'*****************
'Solenoids
'*****************

'AutoPlunger
Sub SolAutoPlungerIM(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
		SoundPlungerReleaseBall()  
	End If
End Sub

'Knocker
Sub SolKnocker(enabled)
	If enabled then 
		KnockerSolenoid 'Added KnockerPosition primitive to table
	End If
End Sub

'CavTargets
Dim CPos
Sub UpdateBar(acurrpos, aspeed, alastpos)
	CPos = acurrpos
	'StopSound"motor": PlaySound"motor"
	Sw49a.TransY = 50 - CPos
	Sw49b.TransY = 50 - CPos
	If CPos => 45 Then Sw49.isdropped = 1
	If CPos =< 25 Then Sw49.isdropped = 0
	If CPos => 35 Then 'open
		FlasherLight8a.visible = 0
		FlasherLight8b.visible = 1
	else
		FlasherLight8a.visible = 1
		FlasherLight8b.visible = 0
	end if     
End Sub

'Ramp Diverter
Sub SolDiv(enabled)
	If enabled Then
		RampDiverter.Isdropped = 0
		'PlaySound "fx_diverter"
		RampDiverter2.Isdropped = 1 
		DivP.RotY = 220
	Else
		RampDiverter.Isdropped = 1 
		'StopSound "fx_Ramp"
		'PlaySound "fx_diverter"
		RampDiverter2.Isdropped = 1
		DivP.RotY = 203
	End If
End Sub

' Drain Sound
Sub Drain_Hit()
	RandomSoundDrain Drain
End Sub

' BallRelease Sound
Sub Ballrelease_UnHit
	RandomSoundBallRelease Ballrelease
End Sub


' Flippers
Const ReflipAngle = 20

Sub SolLFlipper(Enabled)

	If Enabled Then
		'** nFozzy - begin
		LF.Fire  
		'LeftFlipper.rotatetoend
		'** nFozzy - end
		If LeftFlipper.currentangle < LeftFlipper.endangle + ReflipAngle Then 
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


Sub SolRFlipper(Enabled)
	If Enabled Then
		'** nFozzy - begin
		RF.Fire
		'RightFlipper.RotateToEnd
		'** nFozzy - end
		If RightFlipper.currentangle > RightFlipper.endangle - ReflipAngle Then
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


Sub LeftFlipper_Collide(parm)
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	RightFlipperCollide parm
End Sub



'*****************
'VR BackGlass Solenoids
'*****************

Sub Sol9(Enabled)
	If Enabled Then
		SetLamp 109,1
		If BackglassFlash = 1 Then
			BGFlMoon.amount=400
			BGFl9.visible = 1
			BGFlBatman1.visible = 1
			BGFlMoon.visible=1
		End If
	Else
		SetLamp 109,0
		If BackglassFlash = 1 Then
			BGFlMoon.amount=205
			BGFl9.visible = 0	
			BGFlBatman1.visible=0
			BGFlMoon.visible=0
		End If 
	End If
End Sub

Dim aOn

Sub Sol11(Enabled)
	If Enabled Then
		SetLamp 111, 0
		If BackglassFlash = 1 Then
			For each x in BGVRGI: x.visible = 0:Next
			BGFlashers.Enabled = 0
		End If	
	Else
		SetLamp 111, 1
		If BackglassFlash = 1 Then
			For each x in BGVRGI: x.visible = 1:Next
			BGFlashers.Enabled = 1
		End If
	End If
End Sub


Sub Sol12(Enabled)
	If Enabled Then
		SetLamp 112, 1
		If BackglassFlash = 1 Then
			BGFl12.visible = 1
			BGFl121.visible = 1
			BGFlAreaB.amount = 225
			BGFlAreaM.amount = 225
			BGFlBulb122.visible = 1
			BGFlAreaCityRight1.amount = 135
			BGAreaLowRight.amount = 45
		End If
	Else
		SetLamp 112, 0
		If BackglassFlash = 1 Then
			BGFl12.visible = 0
			BGFl121.visible = 0 
			BGFlAreaB.amount = 150
			BGFlAreaM.amount = 150
			BGFlBulb122.visible = 0
			BGFlAreaCityRight1.amount = 60
			BGAreaLowRight.amount = 20
		End If
	End If
End Sub

Sub Sol13(Enabled)
	If Enabled Then
		SetLamp 113, 1
		If BackglassFlash = 1 Then
			BGfl13.visible = 1 
			BGFl131.visible = 1 
			BGFlAreaA1.amount = 225
			BGFlAreaA2.amount = 225
		End If
	Else
		SetLamp 113, 0
		If BackglassFlash = 1 Then
			BGfl13.visible = 0 
			BGFl131.visible = 0
			BGFlAreaA1.amount = 150
			BGFlAreaA2.amount = 150
		End If
	End If
End Sub

Sub Sol14(Enabled)
	If Enabled Then
		SetLamp 114, 1
		If BackglassFlash = 1 Then
			BGfl14.visible = 1 
			BGFl141.visible = 1 
			BGFl142.visible = 1
			BGFlAreaT.amount = 225
			BGFlAreaN.amount = 225
			BGAreaBatmobile.amount = 160
			BGFlAreaCityLeft1.amount = 60
			BGAreaAlfred.amount = 140
			BGFlAreaKnox.amount = 120
			BGAreaLowLeft.amount = 35
		End If
	Else
		SetLamp 114, 0
		If BackglassFlash = 1 Then
			BGfl14.visible = 0
			BGfl141.visible = 0
			BGFl142.visible = 0
			BGFlAreaT.amount = 150
			BGFlAreaN.amount = 150
			BGAreaBatmobile.amount = 110
			BGFlAreaCityLeft1.amount = 40
			BGAreaAlfred.amount = 100
			BGFlAreaKnox.amount = 100
			BGAreaLowLeft.amount = 20
		End If
	End If
End Sub

Sub Sol25(Enabled)
	If Enabled Then
		SetLamp 125, 1
		If BackglassFlash = 1 Then
			BGFl25.visible = 1 
			BGFlAreaCityRight.amount = 60
			BGFlAreaKnox.amount = 120
		End If
	Else
		SetLamp 125, 0
		If BackglassFlash = 1 Then
			BGfl25.visible = 0	
			BGFlAreaCityRight.amount = 50
			BGFlAreaKnox.amount = 100
		End If 
	End If
End Sub

Sub Sol26(Enabled)
	If Enabled Then
		SetLamp 126, 1
		If BackglassFlash = 1 Then
			BGFlMoon.amount= 400
			BGFl26.visible = 1
			BGFlBatman1.visible = 1
			BGFlMoon.visible = 1
		End If
	Else
		SetLamp 126, 0
		If BackglassFlash = 1 Then
			BGFlMoon.amount=205
			BGFl26.visible = 0	
			BGFlBatman1.visible = 0
			BGFlMoon.visible = 0
		End If 
	End If
End Sub

Sub Sol27(Enabled)
	If Enabled Then
		SetLamp 127, 1
		If BackglassFlash = 1 Then
			BGfl27.visible = 1 
			BGfl272.visible = 1 
			BGFlAreaVicki.amount = 150
			BGFlAreaBatman1.amount = 300
			BGFlAreaBatman2.amount = 300
			BGfl27.amount = 135
			BGfl272.amount = 135
			If BGFlAreaVicki.visible = 0 Then
				BGFlAreaVicki1.visible = 1
				BGFlAreaBatman3.visible = 1
				BGFlAreaBatman4.visible = 1
				BGFl27.amount = 250
				BGFl272.amount = 250
			End If
		End If
	Else
		SetLamp 127, 0
		If BackglassFlash = 1 Then
			BGfl27.visible = 0	
			BGFl272.visible = 0
			BGFl27.amount = 135
			BGFl272.amount = 135
			BGFlAreaVicki.amount = 100
			BGFlAreaVicki1.visible = 0	
			BGFlAreaBatman3.visible = 0
			BGFlAreaBatman4.visible = 0	
			BGFlAreaBatman1.amount = 60
			BGFlAreaBatman2.amount = 60
		End If 
	End If
End Sub

Sub Sol28(Enabled)
	If Enabled Then
		SetLamp 128, 1
		If BackglassFlash = 1 Then
			BGfl28.visible = 1 
			BGFlAreaBatmanLogo1.visible = 1
			BGFlAreaBatmanLogo.amount = 190
			BGFlAreaBatman1.amount = 120
			BGFlAreaBatman2.amount = 120
		End If
	Else
		SetLamp 128, 0
		If BackglassFlash = 1 Then
			BGfl28.visible = 0
			BGFlAreaBatmanLogo1.visible = 0
			BGFlAreaBatmanLogo.amount = 110
			BGFlAreaBatman1.amount = 60
			BGFlAreaBatman2.amount = 60
		End If 
	End If
End Sub

Sub Sol30(Enabled)
	If Enabled Then
		SetLamp 130, 1
		If BackglassFlash = 1 Then
			BGFl30.visible = 1 
			BGFlAreaJoker.amount = 160
		End If
	Else
		SetLamp 130, 0
		If BackglassFlash = 1 Then
			BGFl30.visible = 0	
			BGFlAreaJoker.amount = 120
		End If
	End If
End Sub

Sub Sol31(Enabled)
	If Enabled Then
		SetLamp 131, 1
		If BackglassFlash = 1 Then
			BGfl31.visible = 1
			BGFlAreaCityLeft.amount = 135
		End IF
	Else
		SetLamp 131, 0
		If BackglassFlash = 1 Then
			BGfl31.visible = 0
			BGFlAreaCityLeft.amount= 110
		End IF
	End If
End Sub

'Sub Sol32(Enabled)
'	If VRRoom = 1 Then
'		If Enabled Then
'			Flash132
'			BGfl32.visible = 1
'			BGFlAreaTopRightL.amount = 250
'			BGFlAreaTopRightR.amount = 250
'		Else
'			Flash132
'			BGfl32.visible = 0	
'			BGFlAreaTopRightL.amount = 100
'			BGFlAreaTopRightR.amount = 100
'		End If 
'	End If
'End Sub

'*****************
' Switches
'*****************

' Slingshots
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
	RandomSoundSlingshotLeft Lemk
	LeftSling4.Visible = 1
	Lemk.RotX = 26
	LStep = 0
	vpmTimer.PulseSw 47
	LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
		Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
		Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RandomSoundSlingshotRight Remk
	RightSling4.Visible = 1
	Remk.RotX = 26
	RStep = 0
	vpmTimer.PulseSw 48
	RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
		Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
		Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

' Bumpers
Sub Bumper1B_Hit
	RandomSoundBumperTop Bumper1B
	vpmTimer.PulseSw 54
End Sub

Sub Bumper2B_Hit
	RandomSoundBumperMiddle Bumper2B
	vpmTimer.PulseSw 56
End Sub

Sub Bumper3B_Hit
	RandomSoundBumperBottom Bumper3B
	vpmTimer.PulseSw 55
End Sub

' Plunger lane
Dim BallInPlungerLane
Sub Sw14_Hit()
	BallInPlungerLane=1
End Sub

Sub Sw14_UnHit()
	BallInPlungerLane=0
End Sub

'Top Lanes -- Added Sw17, Sw18, Sw19 to Rollovers Collection
Sub Sw17_Hit()
	Controller.Switch(17)=1
End Sub
Sub Sw17_UnHit():Controller.Switch(17)=0: End Sub

Sub Sw18_Hit()
	Controller.Switch(18)=1
End Sub

Sub Sw18_UnHit():Controller.Switch(18)=0: End Sub

Sub Sw19_Hit()
	Controller.Switch(19)=1
End Sub
Sub Sw19_UnHit():Controller.Switch(19)=0: End Sub


Sub Sw21_Hit():Controller.Switch(21)=1: End Sub
Sub Sw21_UnHit():Controller.Switch(21)=0: End Sub

Sub Sw22_Hit():Controller.Switch(22)=1: End Sub
Sub Sw22_UnHit():Controller.Switch(22)=0: End Sub

Sub Sw23_Hit():Controller.Switch(23)=1: End Sub
Sub Sw23_UnHit():Controller.Switch(23)=0: End Sub

Sub Sw24_Hit():Controller.Switch(24)=1: End Sub
Sub Sw24_UnHit():Controller.Switch(24)=0: End Sub

'Center Ramp
Sub Sw28_Hit():Controller.Switch(28)=1: End Sub
Sub Sw28_UnHit():Controller.Switch(28)=0: End Sub

'Center Ramp Exit
Sub Sw29_Hit()
	Controller.Switch(29)=1
End Sub
Sub Sw29_UnHit():Controller.Switch(29)=0: End Sub

' Left Banks Targets
Sub Sw33_Hit:vpmTimer.PulseSw 33 :MoveTarget33 :End Sub
Sub Sw34_Hit:vpmTimer.PulseSw 34 :MoveTarget34 :End Sub
Sub Sw35_Hit:vpmTimer.PulseSw 35 :MoveTarget35 :End Sub

Sub MoveTarget33
	Sw33a.TransZ = 5
	Sw33b.TransZ = 5
	Sw33.Timerenabled = False
	Sw33.Timerenabled = True
End Sub
Sub	Sw33_Timer
	Sw33.Timerenabled = False
	Sw33a.TransZ = 0
	Sw33b.TransZ = 0
End Sub

Sub MoveTarget34
	Sw34a.TransZ = 5
	Sw34b.TransZ = 5
	Sw34.Timerenabled = False
	Sw34.Timerenabled = True
End Sub
Sub	Sw34_Timer
	Sw34.Timerenabled = False
	Sw34a.TransZ = 0
	Sw34b.TransZ = 0
End Sub

Sub MoveTarget35
	Sw35a.TransZ = 5
	Sw35b.TransZ = 5
	Sw35.Timerenabled = False
	Sw35.Timerenabled = True
End Sub
Sub	Sw35_Timer
	Sw35.Timerenabled = False
	Sw35a.TransZ = 0
	Sw35b.TransZ = 0
End Sub

'Joker Eyes and Mouth
'Sub Sw36_Hit: PlaySound "kicker_hit": vpmTimer.PulseSw (36) :  End Sub
'Sub Sw37_Hit: PlaySound "kicker_hit": vpmTimer.PulseSw (37) :  End Sub
'Sub Sw38_Hit: PlaySound "kicker_hit": vpmTimer.PulseSw (38) :  End Sub

''''Left Eye

'sw36
Sub sw36_Hit()
	sw36dir = 1
	sw36Move = 1
	Me.TimerEnabled = true
	Controller.Switch(36) = 1
	'	PlaySoundAt "rollover",KSRswitchArm
End Sub

Sub sw36_unHit()
	sw36dir = -1
	sw36Move = 4
	Me.TimerEnabled = true
	Controller.Switch(36) = 0
End Sub


Dim sw36dir, sw36Move

Sub sw36_timer()
	Select case sw36Move
		Case 0:me.TimerEnabled = false:psw36.RotX = 50
		Case 1:psw36.RotX = 55
		Case 2:psw36.RotX = 60
		Case 3:psw36.RotX = 65
		Case 4:psw36.RotX = 70
		Case 5:me.TimerEnabled = false:psw36.RotX = 75
	End Select

	sw36Move = sw36Move + sw36dir

End Sub


''''Right Eye

'sw37
Sub sw37_Hit()
	sw37dir = 1
	sw37Move = 1
	Me.TimerEnabled = true
	Controller.Switch(37) = 1
	'	PlaySoundAt "rollover",KSRswitchArm
End Sub

Sub sw37_unHit()
	sw37dir = -1
	sw37Move = 4
	Me.TimerEnabled = true
	Controller.Switch(37) = 0
End Sub


Dim sw37dir, sw37Move

Sub sw37_timer()
	Select case sw37Move
		Case 0:me.TimerEnabled = false:psw37.RotX = 50
		Case 1:psw37.RotX = 55
		Case 2:psw37.RotX = 60
		Case 3:psw37.RotX = 65
		Case 4:psw37.RotX = 70
		Case 5:me.TimerEnabled = false:psw37.RotX = 75
	End Select

	sw37Move = sw37Move + sw37dir

End Sub


''''Mouth

'sw38
Sub sw38_Hit()
	sw38dir = 1
	sw38Move = 1
	Me.TimerEnabled = true
	Controller.Switch(38) = 1
	'	PlaySoundAt "rollover",KSRswitchArm
End Sub

Sub sw38_unHit()
	sw38dir = -1
	sw38Move = 4
	Me.TimerEnabled = true
	Controller.Switch(38) = 0
End Sub


Dim sw38dir, sw38Move

Sub sw38_timer()
	Select case sw38Move
		Case 0:me.TimerEnabled = false:psw38.RotX = 50
		Case 1:psw38.RotX = 55
		Case 2:psw38.RotX = 60
		Case 3:psw38.RotX = 65
		Case 4:psw38.RotX = 70
		Case 5:me.TimerEnabled = false:psw38.RotX = 75
	End Select

	sw38Move = sw38Move + sw38dir

End Sub





'Left Scoop
Sub Sw39_hit: Controller.Switch(39)=1 : SoundSaucerLock :  End Sub


Sub ScoopKickL(Enabled)

	'	PlaySoundAt SoundFX("solenoid",DOFContactors),Primitive64
	SoundSaucerKick 1, sw39
	sw39.Kick 0,85,1.56 '50=Strength
	Controller.Switch(39) = 0

End Sub



'Sub Sw39b_UnHit: vpmtimer.addtimer 200, "Sw39.enabled = 1 '" :End Sub

' Right Banks Targets
Sub Sw41_Hit:vpmTimer.PulseSw 41 :MoveTarget41 :End Sub
Sub Sw42_Hit:vpmTimer.PulseSw 42 :MoveTarget42 :End Sub
Sub Sw43_Hit:vpmTimer.PulseSw 43 :MoveTarget43 :End Sub

Sub MoveTarget41
	Sw41a.TransZ = 5
	Sw41b.TransZ = 5
	Sw41.Timerenabled = False
	Sw41.Timerenabled = True
End Sub
Sub	Sw41_Timer
	Sw41.Timerenabled = False
	Sw41a.TransZ = 0
	Sw41b.TransZ = 0
End Sub

Sub MoveTarget42
	Sw42a.TransZ = 5
	Sw42b.TransZ = 5
	Sw42.Timerenabled = False
	Sw42.Timerenabled = True
End Sub
Sub	Sw42_Timer
	Sw42.Timerenabled = False
	Sw42a.TransZ = 0
	Sw42b.TransZ = 0
End Sub

Sub MoveTarget43
	Sw43a.TransZ = 5
	Sw43b.TransZ = 5
	Sw43.Timerenabled = False
	Sw43.Timerenabled = True
End Sub
Sub	Sw43_Timer
	Sw43.Timerenabled = False
	Sw43a.TransZ = 0
	Sw43b.TransZ = 0
End Sub

'Bat Bar StandUp
Sub Sw49_Hit
	vpmTimer.PulseSw 49
	'PlaySound "fx_chapa"
	RandomSoundMetal
End Sub

'Sub Sw52_Hit() : PlaySound "trough": Me.DestroyBall:vpmTimer.Addtimer 1000, "bsRScoop.AddBall" : End Sub



'Right Scoop

Dim BallinSw52


Sub Sw52_hit:Controller.Switch(52) = 1:BallinSw52 = 1:SoundSaucerLock: End Sub


Sub ScoopKickR(Enabled)

	SoundSaucerKick 1, sw52
	sw52.Kick 0,40,1.56 '50=Strength
	Controller.Switch(52) = 0
	BallinSw52 = 0
	Controller.Switch(53) = 0
End Sub



'***************************************************
'	General Illumination
'***************************************************
Dim x

Sub SolGi(enabled)
	If enabled Then
		GiOFF
		Playsound "fx_relay"
		Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSatDark"
	Else
		GiON
		Playsound "fx_relay"
		Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
	End If
End Sub

Sub GiON
	For each x in aGiLights:x.State = 1:Next
	gi29.IntensityScale=1 : gi30.IntensityScale=1
	Primitive_PlasticRamp.Image = "BatRampMapResized"
End Sub

Sub GiOFF
	For each x in aGiLights:x.State = 0:Next
	gi29.IntensityScale=0 : gi30.IntensityScale=0
	Primitive_PlasticRamp.Image = "BatmanRampMap_resizedOFF"
End Sub



'****************************************************************
'					Begin nFozzy lamp handling
'****************************************************************

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
Dim ModLampz : Set ModLampz = New DynamicLamps
InitLampsNF              ' Setup lamp assignments

Sub LampTimer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
	ModLampz.Update1
	'	If F19.IntensityScale > 0 then
	'		fmfl27.visible = False
	'		l27.visible = False
	'	Else
	'		fmfl27.visible = True
	'		l27.visible = True
	'	end if
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
Sub Lampztimer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
	ModLampz.Update
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'Material swap arrays.
Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image trans","Plastic with an image trans","Plastic with an image")
Dim BumperCapArray: BumperCapArray = Array("Batman_Bumpercap_off", "Batman_Bumpercap_33","Batman_Bumpercap_66","Batman_Bumpercap_on") 
Dim BatCaveArray: BatCaveArray = Array("BatCave_bothON", "BatCave_bothON", "BatCave_bothON", "BatCave_CompleteMap")
Dim MuseumArray: MuseumArray = Array("MuseumMap_off", "MuseumMap_off", "MuseumMap_on", "MuseumMap_on")
Dim BatCaveRightArray: BatCaveRightArray = Array("batcave_completemap", "batcave_completemap", "batcaveright", "batcaveright")
Dim FilamentArray: FilamentArray = Array("WireDT_off", "WireDT_33", "WireDT_66", "WireDT_on")
Dim ClearBulbArray: ClearBulbArray = Array("BulbGIoff", "BulbGIoff", "BulbGIOn","BulbGIOn")


Dim DLintensity

'****************************************************************
'					Prim *Image* Swaps
'****************************************************************
Sub ImageSwap(pri, group, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 1:pri.Image = group(0) 'Full
		Case 2:pri.Image = group(1) 'Fading...
		Case 3:pri.Image = group(2) 'Fading...
		Case 4:pri.Image = group(3) 'Off
	End Select
	pri.blenddisablelighting = DLintensity

End Sub

'****************************************************************
'					Prim *Material* Swaps
'****************************************************************
Sub MatSwap(pri, group, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 1:pri.Material = group(0) 'Full
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
		Case 4:pri.Material = group(3) 'Off
	End Select
	pri.blenddisablelighting = aLvl * DLintensity
End Sub


Sub FadeMaterialToys(pri, group, ByVal aLvl)	'cp's script
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 1 'Intensity Adjustment
End Sub


Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity * 0.4
	'	if pri.name = "p37bulb" then debug.print "bulb: " & aLvl * DLintensity * 0.4 & " intensity: " & DLintensity
	'	if pri.name = "p37" then debug.print "on: " & aLvl * DLintensity * 0.4 & " intensity: " & DLintensity
	'	if pri.name = "p56bulb" then debug.print "56 bulb: " & aLvl * DLintensity * 0.4 & " intensity: " & DLintensity
	'	if pri.name = "p56" then debug.print "56 on: " & aLvl * DLintensity * 0.4 & " intensity: " & DLintensity
End Sub

dim apron_group : apron_group = Array("00_apron plastic", "00_apron plastic", "00_apron plastic_ON", "00_apron plastic_ON")

Sub InitLampsNF()
	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating
	ModLampz.Filter = "LampFilter"

	'Adjust fading speeds (1 / full MS fading time)
	dim x
	for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/3 : Lampz.FadeSpeedDown(x) = 1/6 : next
	for x = 0 to 28 : ModLampz.FadeSpeedUp(x) = 1/2 : ModLampz.FadeSpeedDown(x) = 1/30 : Next

	'for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/80 : Lampz.FadeSpeedDown(x) = 1/100 : next
	Lampz.FadeSpeedUp(111) = 1/3 'GI
	Lampz.FadeSpeedDown(111) = 1/15

	'Lamp Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays


	Lampz.MassAssign(1)= l1
	Lampz.Callback(1) = "DisableLighting p1, 40,"
	Lampz.Callback(1) = "DisableLighting p1bulb, 100,"
	Lampz.MassAssign(2)= l2
	Lampz.Callback(2) = "DisableLighting p2, 40,"
	Lampz.Callback(2) = "DisableLighting p2bulb, 100,"
	Lampz.MassAssign(3)= l3
	Lampz.Callback(3) = "DisableLighting p3, 20,"
	Lampz.Callback(3) = "DisableLighting p3bulb, 40,"
	Lampz.MassAssign(4)= l4
	Lampz.Callback(4) = "DisableLighting p4, 40,"
	Lampz.Callback(4) = "DisableLighting p4bulb, 100,"
	Lampz.MassAssign(5)= l5
	Lampz.Callback(5) = "DisableLighting p5, 40,"
	Lampz.Callback(5) = "DisableLighting p5bulb, 100,"
	Lampz.MassAssign(6)= l6
	Lampz.Callback(6) = "DisableLighting p6, 20,"
	Lampz.Callback(6) = "DisableLighting p6bulb, 40,"
	Lampz.MassAssign(7)= l7
	Lampz.Callback(7) = "DisableLighting p7, 40,"
	Lampz.Callback(7) = "DisableLighting p7bulb, 100,"
	Lampz.MassAssign(8)= l8
	Lampz.Callback(8) = "DisableLighting p8, 180,"
	Lampz.Callback(8) = "DisableLighting p8bulb, 200,"
	Lampz.MassAssign(9)= l9
	Lampz.Callback(9) = "DisableLighting p9, 50,"
	Lampz.Callback(9) = "DisableLighting p9bulb, 170,"
	Lampz.MassAssign(10)= l10
	Lampz.Callback(10) = "DisableLighting p10, 50,"
	Lampz.Callback(10) = "DisableLighting p10bulb, 170,"
	Lampz.MassAssign(11)= l11
	Lampz.Callback(11) = "DisableLighting p11, 50,"
	Lampz.Callback(11) = "DisableLighting p11bulb, 170,"
	Lampz.MassAssign(12)= l12
	Lampz.Callback(12) = "DisableLighting p12, 50,"
	Lampz.Callback(12) = "DisableLighting p12bulb, 170,"
	Lampz.MassAssign(13)= l13
	Lampz.Callback(13) = "DisableLighting p13, 50,"
	Lampz.Callback(13) = "DisableLighting p13bulb, 170,"
	Lampz.MassAssign(14)= l14
	Lampz.Callback(14) = "DisableLighting p14, 50,"
	Lampz.Callback(14) = "DisableLighting p14bulb, 170,"

	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(16)= l16

	Lampz.MassAssign(17)= L17f
	Lampz.MassAssign(17)= l17
	Lampz.Callback(17) = "DisableLighting p17, 180,"
	Lampz.Callback(17) = "DisableLighting p17bulb, 200,"
	Lampz.MassAssign(18)= L18f
	Lampz.MassAssign(18)= l18
	Lampz.Callback(18) = "DisableLighting p18, 180,"
	Lampz.Callback(18) = "DisableLighting p18bulb, 200,"
	Lampz.MassAssign(19)= L19f
	Lampz.MassAssign(19)= l19
	Lampz.Callback(19) = "DisableLighting p19, 180,"
	Lampz.Callback(19) = "DisableLighting p19bulb, 200,"

	Lampz.MassAssign(20)= l20

	Lampz.MassAssign(21)= l21
	Lampz.Callback(21) = "DisableLighting p21, 180,"
	Lampz.Callback(21) = "DisableLighting p21bulb, 200,"
	Lampz.MassAssign(22)= l22
	Lampz.Callback(22) = "DisableLighting p22, 180,"
	Lampz.Callback(22) = "DisableLighting p22bulb, 200,"
	Lampz.MassAssign(23)= l23f
	Lampz.MassAssign(23)= l23
	Lampz.Callback(23) = "DisableLighting p23, 180,"
	Lampz.Callback(23) = "DisableLighting p23bulb, 200,"
	Lampz.MassAssign(24)= l24
	Lampz.Callback(24) = "DisableLighting p24, 180,"
	Lampz.Callback(24) = "DisableLighting p24bulb, 200,"


	'	Objlevel(2) = value/255 : FlasherFlash2_Timer
	Lampz.Callback(25)="Flash25"

	'	Lampz.MassAssign(25)= FlasherLight25a
	'	Lampz.MassAssign(25)= FlasherL25
	'	Lampz.MassAssign(25)= FlasherLight25d
	''
	Lampz.Callback(26)="Flash26"
	'	Lampz.MassAssign(26)= FlasherLight26a
	'	Lampz.MassAssign(26)= FlasherL26
	'	Lampz.MassAssign(26)= FlasherLight26d
	''
	Lampz.Callback(27)="Flash27"
	'	Lampz.MassAssign(27)= FlasherLight27a
	'	Lampz.MassAssign(27)= FlasherL27
	'	Lampz.MassAssign(27)= FlasherLight27d
	''
	Lampz.Callback(28)="Flash28"
	'	Lampz.MassAssign(28)= FlasherLight28a
	'	Lampz.MassAssign(28)= FlasherL28
	'	Lampz.MassAssign(28)= FlasherLight28d
	''
	Lampz.Callback(29)="Flash29"
	'	Lampz.MassAssign(29)= FlasherLight29a
	'	Lampz.MassAssign(29)= FlasherL29
	'	Lampz.MassAssign(29)= FlasherLight29d


	Lampz.MassAssign(30)= l30
	Lampz.Callback(30) = "DisableLighting p30, 120,"
	Lampz.Callback(30) = "DisableLighting p30bulb, 500,"
	Lampz.MassAssign(31)= l31
	Lampz.Callback(31) = "DisableLighting p31, 120,"
	Lampz.Callback(31) = "DisableLighting p31bulb, 500,"
	Lampz.MassAssign(32)= l32
	Lampz.Callback(32) = "DisableLighting p32, 180,"
	Lampz.Callback(32) = "DisableLighting p32bulb, 200,"
	Lampz.MassAssign(33)= l33
	Lampz.Callback(33) = "DisableLighting pl33, 300,"
	Lampz.MassAssign(34)= l34
	Lampz.Callback(34) = "DisableLighting pl34, 300,"
	Lampz.MassAssign(35)= l35
	Lampz.Callback(35) = "DisableLighting pl35, 300,"


	Lampz.MassAssign(36)= l36b
	Lampz.Callback(36) = "DisableLighting L81P005, 4,"
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= l36Fb
	Lampz.MassAssign(36)= l36Fb2
	'
	Lampz.MassAssign(37)= l37b
	Lampz.Callback(37) = "DisableLighting L81P001, 4,"
	Lampz.MassAssign(37)= l37
	Lampz.MassAssign(37)= l37Fb
	'
	'	Lampz.MassAssign(38)= L38
	'	Lampz.MassAssign(38)= l38f
	'	Lampz.MassAssign(38)= l382
	'	Lampz.MassAssign(38)= FlasherL38r

	Lampz.MassAssign(38)= l38
	Lampz.Callback(38) = "DisableLighting pl38, 30,"
	Lampz.MassAssign(39)= l39
	Lampz.Callback(39) = "DisableLighting pl39, 15,"
	Lampz.Callback(39) = "DisableLighting pl39bulb, 90,"

	Lampz.MassAssign(40)= l40

	Lampz.MassAssign(41)= l41
	Lampz.Callback(41) = "DisableLighting pl41, 300,"
	Lampz.MassAssign(42)= l42
	Lampz.Callback(42) = "DisableLighting pl42, 300,"
	Lampz.MassAssign(43)= l43
	Lampz.Callback(43) = "DisableLighting pl43, 300,"


	Lampz.Callback(44) = "ImageSwap pBumperCap1, BumperCapArray, 1,"
	Lampz.MassAssign(44)= BumperL_Flasher
	Lampz.MassAssign(44)= BumperL_Flasher_a
	If HalfPosts = 1 Then
		Lampz.MassAssign(44)= Bumberhalolb
	Else
		Lampz.MassAssign(44)= Bumberhalol
	End If
	'
	'
	Lampz.Callback(45) = "ImageSwap pBumperCap3, BumperCapArray, 1,"
	Lampz.MassAssign(45)= BumperB_Flasher
	Lampz.MassAssign(45)= BumperB_Flasher_a
	If HalfPosts = 1 Then
		Lampz.MassAssign(45)= Bumberhalobb
	Else
		Lampz.MassAssign(45)= Bumberhalob
	End If
	'

	Lampz.Callback(46) = "ImageSwap pBumperCap2, BumperCapArray, 1,"	
	'	Lampz.Callback(46) = "DisableLighting pBumperCap2, 1,"
	Lampz.MassAssign(46)= BumperR_Flasher
	If HalfPosts = 1 Then
		Lampz.MassAssign(46)= Bumberhalorb
	Else
		Lampz.MassAssign(46)= Bumberhalor
	End If


	Lampz.MassAssign(47)= l47
	Lampz.Callback(47) = "DisableLighting pl47, 20,"
	Lampz.Callback(47) = "DisableLighting pl47bulb, 70,"

	Lampz.MassAssign(48)= l48


	Lampz.MassAssign(49)= l49
	Lampz.MassAssign(49)= l49f
	Lampz.Callback(49) = "DisableLighting Primitive62, 4,"

	Lampz.MassAssign(55)= L55

	Lampz.MassAssign(56)= l56
	Lampz.Callback(56) = "DisableLighting p56, 10,"
	Lampz.Callback(56) = "DisableLighting p56bulb, 60,"




	Lampz.MassAssign(57)= l57
	Lampz.Callback(57) = "DisableLighting p57, 40,"
	Lampz.Callback(57) = "DisableLighting p57bulb, 100,"
	Lampz.MassAssign(58)= l58
	Lampz.Callback(58) = "DisableLighting p58, 40,"
	Lampz.Callback(58) = "DisableLighting p58bulb, 100,"
	Lampz.MassAssign(59)= l59
	Lampz.Callback(59) = "DisableLighting p59, 40,"
	Lampz.Callback(59) = "DisableLighting p59bulb, 100,"
	Lampz.MassAssign(60)= l60
	Lampz.Callback(60) = "DisableLighting p60, 40,"
	Lampz.Callback(60) = "DisableLighting p60bulb, 100,"
	Lampz.MassAssign(61)= l61
	Lampz.Callback(61) = "DisableLighting p61, 40,"
	Lampz.Callback(61) = "DisableLighting p61bulb, 100,"
	Lampz.MassAssign(62)= l62
	Lampz.Callback(62) = "DisableLighting p62, 40,"
	Lampz.Callback(62) = "DisableLighting p62bulb, 100,"



	'	Lampz.MassAssign(57)= l57
	'	Lampz.Callback(57) = "DisableLighting pl57, 500,"
	'	Lampz.MassAssign(58)= l58
	'	Lampz.Callback(58) = "DisableLighting pl58, 500,"
	'	Lampz.MassAssign(59)= l59
	'	Lampz.Callback(59) = "DisableLighting pl59, 500,"
	'	Lampz.MassAssign(60)= l60
	'	Lampz.Callback(60) = "DisableLighting pl60, 500,"
	'	Lampz.MassAssign(61)= l61
	'	Lampz.Callback(61) = "DisableLighting pl61, 500,"
	'	Lampz.MassAssign(62)= l62
	'	Lampz.Callback(62) = "DisableLighting pl62, 500,"

	Lampz.MassAssign(63)= l63
	Lampz.Callback(63) = "DisableLighting p63, 40,"
	Lampz.Callback(63) = "DisableLighting p63bulb, 120,"
	Lampz.MassAssign(64)= l64
	Lampz.Callback(64) = "DisableLighting p64, 40,"
	Lampz.Callback(64) = "DisableLighting p64bulb, 120,"






	'
	'	'Flashers
	'	Lampz.Callback(109) = "ImageSwap Primitive53, BatCaveArray, 1,"	
	'Lampz.MassAssign(109)= FlasherL9
	Lampz.MassAssign(109)= Flasher9
	Lampz.MassAssign(109)= Flasher9a
	Lampz.MassAssign(112)= Flasher12
	'	Lampz.MassAssign(113)= l382
	Lampz.MassAssign(114)= Flasher14
	'
	Lampz.MassAssign(125)= FlasherL1b
	Lampz.MassAssign(125)= FlasherL2b
	'	
	'
	'	Lampz.Callback(126) = "ImageSwap Primitive53, BatCaveArray, 1,"	
	Lampz.MassAssign(126)= FlasherLight2a
	Lampz.MassAssign(126)= FlasherLight2b
	Lampz.MassAssign(126)= FlasherLight2c
	'
	Lampz.MassAssign(127)= FlasherLight3a
	Lampz.MassAssign(128)= FlasherLight4a
	'

	Lampz.Callback(129)="Flash129"



	'	Lampz.MassAssign(129)= FlasherL1
	'	Lampz.MassAssign(129)= FlasherL1a
	'	Lampz.MassAssign(129)= FlasherL2
	'	Lampz.MassAssign(129)= FlasherL2a
	'	Lampz.MassAssign(129)= FlasherL3
	'	Lampz.MassAssign(129)= FlasherL3a
	'	Lampz.MassAssign(129)= FlasherL4
	'	Lampz.MassAssign(129)= FlasherL4a
	'	Lampz.MassAssign(129)= Flasher1
	'	Lampz.MassAssign(129)= FlasherL1c
	'	Lampz.MassAssign(129)= Flasher2
	'	Lampz.MassAssign(129)= FlasherL2c
	'	Lampz.MassAssign(129)= Flasher3
	'	Lampz.MassAssign(129)= FlasherL3c
	'	Lampz.MassAssign(129)= Flasher4
	'	Lampz.Callback(129) = "DisableLighting Primitive54, 7,"
	'	Lampz.Callback(129) = "DisableLighting Primitive55, 7,"
	'	Lampz.Callback(129) = "DisableLighting Primitive56, 7,"
	'	Lampz.Callback(129) = "DisableLighting Primitive57, 7,"
	'
	'	Lampz.MassAssign(129)= FlasherL4c
	'
	Lampz.MassAssign(130)= Flasher6
	Lampz.MassAssign(130)= Flasher6a
	Lampz.MassAssign(130)= Flasher6c

	Lampz.Callback(130) = "ImageSwap pBulbFilament1, FilamentArray, 5,"
	Lampz.Callback(130) = "MatSwap pBulbFlasher1, ClearBulbArray, .15,"
	Lampz.Callback(130) = "ImageSwap pBulbFilament2, FilamentArray, 5,"
	Lampz.Callback(130) = "MatSwap pBulbFlasher2, ClearBulbArray, .15,"
	Lampz.Callback(130) = "ImageSwap pBulbFilament3, FilamentArray, 5,"
	Lampz.Callback(130) = "MatSwap pBulbFlasher3, ClearBulbArray, .15,"

	'
	Lampz.MassAssign(131)= FlasherLight7a
	Lampz.MassAssign(131)= FlasherLight7b
	Lampz.MassAssign(131)= FlasherLight7c
	Lampz.MassAssign(131)= FlasherLight7d
	Lampz.MassAssign(131)= FlasherLight7e
	'
	'	Lampz.Callback(132) = "ImageSwap Primitive_museum, MuseumArray, 1,"	
	'	Lampz.Callback(132) = "ImageSwap primitive53, BatCaveRightArray, 1,"	
	'
	'	Lampz.MassAssign(132)= FlasherLight8a
	'	Lampz.MassAssign(132)= Flasher8
	'	Lampz.MassAssign(132)= Flasher8a
	'	Lampz.MassAssign(132)= museumflash


	'****************************************************************
	'						GI assignments
	'****************************************************************
	'

	Lampz.obj(111) = ColtoArray(aGiLights)	



	'Turn off all lamps on startup
	lampz.Init	'This just turns state of any lamps to 1
	ModLampz.Init

	'Immediate update to turn on GI, turn off lamps
	lampz.update
	ModLampz.Update

End Sub


'***************************************
'System 11 GI On/Off
'***************************************
Sub GIOn  : SetGI False: End Sub 'These are just debug commands now
Sub GIOff : SetGI True : End Sub


Dim GIoffMult : GIoffMult = 2 'adjust how bright the inserts get when the GI is off
Dim GIoffMultFlashers : GIoffMultFlashers = 2	'adjust how bright the Flashers get when the GI is off

'Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image trans","Plastic with an image trans","Plastic with an image")

Sub GIupdates(ByVal aLvl)	'GI update odds and ends go here
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically



	'	UpdateMaterial "GI_ON_Material",0,0,0,0,0,0,aLvl^5,RGB(255,255,255),0,0,False,True,0,0,0,0


	UpdateMaterial "GI_ON_CAB",0,0,0,0,0,0,aLvl^5,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_ON_Plastic",0,0,0,0,0,0,aLvl^3,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_ON_Metals",0,0,0,0,0,0,aLvl^2,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_ON_Bulbs",0,0,0,0,0,0,aLvl^0.8,RGB(255,255,255),0,0,False,True,0,0,0,0

	'Sideblades: ^5 (fastest to go off)
	'Plastics: ^3 (medium speed)
	'Bulbs: ^0.5 (not sure how this would look. Would be the slowest)
	'metals:^2
	'
	'GI_ON_Bulbs
	'GI_ON_CAB
	'GI_ON_Metals
	'GI_ON_Plastic

	'debug.print aLvl
	'debug.print aLvl^5

	PLAYFIELD_GI1.opacity = 250 * aLvl^2

End Sub

'
'Sub GIupdates(ByVal aLvl)	'GI update odds and ends go here
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
'
''	dim gi0lvl,gi1lvl
''	if Lampz.UseFunction then 	'Callbacks don't get this filter automatically
''		gi0lvl = LampFilter(ModLampz.Lvl(0))
''		gi1lvl = LampFilter(ModLampz.Lvl(1))
''	Else
''		gi0lvl = ModLampz.Lvl(0)
''		gi1lvl = ModLampz.Lvl(1)
''	end if
''
''
''	'DOF
''	if gi0lvl = 0 Then
''		DOF 103, DOFOff
''	else
''		DOF 103, DOFOn
''	end If
''
''	if gi1lvl*7+1.5 >= 8 Then
''		'debug.print gi1lvl & "full"
''		Table1.ColorGradeImage = "ColorGrade_8"
''	else
''		'debug.print gi1lvl & " -> grade" & Int(gi1lvl*7+1.5)
''		Table1.ColorGradeImage = "ColorGrade_" & Int(gi1lvl*7+1.5)
''	end If
'
'
'
'
'	'Fade lamps up when GI is off
''	dim GIscale
''	GiScale = (GIoffMult-1) * (ABS(aLvl-1 )  ) + 1	'invert
''	dim x : for x = 0 to uBound(LightsA) 
''		On Error Resume Next
''		LightsA(x).Opacity = LightsB(x) * GIscale
''		LightsA(x).Intensity = LightsB(x) * GIscale
''		'LightsA(x).FadeSpeedUp = LightsC(x) * GIscale
''		'LightsA(x).FadeSpeedDown = LightsD(x) * GIscale
''		On Error Goto 0
''	Next
'End Sub


'Lamp Filter
Function LampFilter(aLvl)

	LampFilter = aLvl^1.6	'exponential curve?
End Function



'Helper functions

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function

'Set GICallback2 = GetRef("SetGI")

'Sub SetGI(aNr, aValue)
'	msgbox "GI nro: " & aNr & " and step: " & aValue
'	ModLampz.SetGI aNr, aValue 'Redundant. Could reassign GI indexes here
'End Sub


Dim GiOffFOP
Sub SetGI(aOn)
	'	PlayRelay aOn, 13
	Select Case aOn
		Case True  'GI off       
			SetLamp 111, 0	'Inverted, Solenoid cuts GI circuit on this era of game
			'            For each GiOffFOP in LampsInserts 'increases falloff power for lamps in "lampinserts" collection, when GI is turned off
			'				GIOffFOP.falloffpower = 1.25 'Sets falloff power to 1.25 when GI is ON
			'            next
		Case False 
			SetLamp 111, 5
			'            For each GiOffFOP in LampsInserts  'reduces falloff power for lamps in "lampinserts" collection, when GI is turned off
			'				GiOffFOP.falloffpower = 4 'Sets falloff power to 4 when GI is OFF
			'            next
	End Select
End Sub

Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub

Sub SetModLamp(aNr, aInput)
	ModLampz.state(aNr) = abs(aInput)/255
End Sub

'****************************************************************
'					End nFozzy lamp handling
'****

'****************************************************************
'					End nFozzy lamp handling
'****************************************************************




'****************************************************************
'				Class jungle nf (what does this mean?!?)
'****************************************************************

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Public UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		'debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 0.2 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be publicly accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
'Version 0.13a - fixed DynamicLamps hopefully
' Note: if using multiple 'DynamicLamps' objects, change the 'name' variable to avoid conflicts with callbacks

Class DynamicLamps 'Lamps that fade up and down. GI and Flasher handling
	Public Loaded(50), FadeSpeedDown(50), FadeSpeedUp(50)
	Private Lock(50), SolModValue(50)
	Private UseCallback(50), cCallback(50)
	Public Lvl(50)
	Public Obj(50)
	Private UseFunction, cFilter
	private Mult(50)
	Public Name

	Public FrameTime
	Private InitFrame

	Private Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(Obj)
			FadeSpeedup(x) = 0.01
			FadeSpeedDown(x) = 0.01
			lvl(x) = 0.0001 : SolModValue(x) = 0
			Lock(x) = True : Loaded(x) = False
			mult(x) = 1
			Name = "DynamicFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function

	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'debugstr = debugstr & x & "=" & tmp(x) & vbnewline
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property


	Public Property Let State(idx,Value)
		'If Value = SolModValue(idx) Then Exit Property ' Discard redundant updates
		If Value <> SolModValue(idx) Then ' Discard redundant updates
			SolModValue(idx) = Value
			Lock(idx) = False : Loaded(idx) = False
		End If
	End Property
	Public Property Get state(idx) : state = SolModValue(idx) : end Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	'solcallback (solmodcallback) handler
	Sub SetLamp(aIdx, aInput) : state(aIdx) = aInput : End Sub	'0->1 Input
	Sub SetModLamp(aIdx, aInput) : state(aIdx) = aInput/255 : End Sub	'0->255 Input
	'Sub SetGI(aIdx, ByVal aInput) : if aInput = 8 then aInput = 7 end if : state(aIdx) = aInput/7 : End Sub	'0->8 WPC GI input

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline

				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline

			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'just call turnonstates for now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all numeric fading. If done fading, Lock(x) = True
		'dim stringer
		dim x : for x = 0 to uBound(Lvl)
			'stringer = "Locked @ " & SolModValue(x)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					'stringer = "Fading Up " & lvl(x) & " + " & FadeSpeedUp(x)
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					'stringer = "Fading Down " & lvl(x) & " - " & FadeSpeedDown(x)
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		'tbF.text = stringer
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(Lvl)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx
		for x = 0 to uBound(Lvl)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(abs(Lvl(x))*mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(abs(Lvl(x))*mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)*mult(x)
					End If
				end if
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x)*mult(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					Loaded(x) = True
				end if
			end if
		Next
	End Sub
End Class

'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
			AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function




'****************************************************************
'							Timer Code
'****************************************************************

Dim Gate3Angle, Gate5Angle

Sub FrameTimer_Timer()
	LampTimer
	Lampztimer
	RollingUpdate

	LFLogo.RotY = LeftFlipper.CurrentAngle
	RFlogo.RotY = RightFlipper.CurrentAngle

	pGate2.RotX = Gate2.currentAngle * -1 + 90
	pGate3.RotX = Gate3.currentAngle * 1 + 90
	pGate5.RotX = Gate5.currentAngle * 1 + 90

	Gate3Angle = Int(gate3.CurrentAngle)
	If Gate3Angle > 0 then
		pGatesw3.ObjRotY = sin( (Gate3Angle * 1) * (2*PI/180)) * 10
	Else
		pGatesw3.ObjRotY = sin( (Gate3Angle * -1) * (2*PI/180)) * 10
	End If

	Gate5Angle = Int(gate5.CurrentAngle)
	If Gate5Angle > 0 then
		pGatesw5.ObjRotY = sin( (Gate5Angle * -1) * (2*PI/180)) * 10
	Else
		pGatesw5.ObjRotY = sin( (Gate5Angle * 1) * (2*PI/180)) * 10
	End If

'	PinCab_Shooter.Y = -215 + (5* Plunger.Position) -20

End Sub







'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1                                                                                                                'volume level; range [0, 1]
NudgeLeftSoundLevel = 1                                                                                                        'volume level; range [0, 1]
NudgeRightSoundLevel = 1                                                                                                'volume level; range [0, 1]
NudgeCenterSoundLevel = 1                                                                                                'volume level; range [0, 1]
StartButtonSoundLevel = 0.1                                                                                                'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr                                                                                        'volume level; range [0, 1]
PlungerPullSoundLevel = 1                                                                                                'volume level; range [0, 1]
RollingSoundFactor = 1.1/5                

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010                                                           'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635                                                                'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                                                                        'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                                                                      'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
SlingshotSoundLevel = 0.95                                                                                                'volume level; range [0, 1]
BumperSoundFactor = 4.25                                                                                                'volume multiplier; must not be zero
KnockerSoundLevel = 1                                                                                                         'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2                                                                        'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5                                                                                        'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5                                                                                        'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5                                                                                'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025                                                                        'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025                                                                        'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8                                                                        'volume level; range [0, 1]
WallImpactSoundFactor = 0.075                                                                                        'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5                                                                                                        'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10                                                                                        'volume multiplier; must not be zero
DTSoundLevel = 0.25                                                                                                                'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                                                                      'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8                                                                                                                'volume level; range [0, 1]
BallReleaseSoundLevel = 1                                                                                                'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2                                                                        'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015                                                                                'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5                                                                                                        'volume multiplier; must not be zero


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
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
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
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
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
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


' *********************************************************************
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************

'''''''''''''''''''Dim tablewidth, tableheight : tablewidth = table1.width : tableheight = table1.height

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

	If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
	Else
		AudioFade = Csng(-((- tmp) ^10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

	If tmp > 0 Then
		AudioPan = Csng(tmp ^10)
	Else
		AudioPan = Csng(-((- tmp) ^10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
    BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function VolPlasticRampRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlasticRampRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function PitchPlasticRamp(ball) ' Calculates the pitch of the sound based on the ball speed - used for plastic ramps roll sound
	PitchPlasticRamp = BallVel(ball) * 20
End Function

Function RndInt(min, max)
	RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
	RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
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
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	if RubberizerEnabled <> 0 then Rubberizer(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	if RubberizerEnabled <> 0 then Rubberizer(parm)
End Sub


' iaakki Rubberizer
sub Rubberizer(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * 1.2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * -1.1
		activeball.vely = activeball.vely * 1.4
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub


Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then                
		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If        
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	RandomSoundWall()       
End Sub

Sub RandomSoundWall()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
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
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()                
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
		RandomSoundTargetHitWeak()
	End If        
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound        
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()                        
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)        
	SoundPlayfieldGate        
End Sub        

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	If ball1.z < 0 Then

	Else
		PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
	End If

End Sub


'/////////////////////////////////////////////////////////////////
'                                        End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'** nFozzy - begin
'/////////////////////////////////////////////////////////////////
'                 Flipper Correction Init Late 80s to early 90s
'/////////////////////////////////////////////////////////////////


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
'                        FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
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

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
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
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				:                         VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'playsound "fx_knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
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
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
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
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function


'******************************************************
'                        FLIPPER TRICKS
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

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
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
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Flipper1.currentangle <> EndAngle1 then 
			EOSNudge1 = 0
		end if
	End If
End Sub

'*****************
' Maths
'*****************
Const PI = 3.1415927

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
Const EOSTnew = 0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.035

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
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

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
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If
		debug.print "Live catch. Bounce: " & LiveCatchBounce
		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	End If
End Sub


'****************************************************************************
'PHYSICS DAMPENERS
'****************************************************************************

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer activeball, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen Activeball
	TargetBouncer activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1             'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 2, 0.96        
RubbersD.addpoint 2, 3.77, 0.96
RubbersD.addpoint 3, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 4, 15.84, 0.874
RubbersD.addpoint 5, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
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


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


End Class


'******************************************************
'                TRACK ALL BALL VELOCITIES
'                 FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()        'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)        'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)        'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)        'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

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

Sub RDampen_Timer()
	Cor.Update
End Sub


'** nFozzy - end


'iaakki - TargetBouncer for standup targets
Dim zMultiplier
sub TargetBouncer(aBall,defvalue)
	if TargetBouncerEnabled <> 0 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = defvalue+1.1
			Case 2: zMultiplier = defvalue+1.05
			Case 3: zMultiplier = defvalue+0.7
			Case 4: zMultiplier = defvalue+0.3
		End Select
		aBall.velz = aBall.velz * zMultiplier
		'debug.print "----> velz: " & activeball.velz
	end if
end sub

'**************************************
'		BALL ROLLING AND DROP SOUNDS 
'**************************************

Const tnob = 4			'total number of balls = Number of balls installed in machine +1
Const lob = 0			'number of locked balls

ReDim rolling(tnob)
ReDim ramprolling(tnob)
ReDim dwballs(tnob)

InitRolling

Dim DropCount
ReDim DropCount(tnob)

Dim ampFactor

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
		ramprolling(i) = False
	Next
	Select Case BallRollAmpFactor
		Case 0
			ampFactor = "_amp0"
		Case 1
			ampFactor = "_amp2_5"
		Case 2
			ampFactor = "_amp5"
		Case 3
			ampFactor = "_amp7_5"
		Case 4
			ampFactor = "_amp9"
		Case Else
			ampFactor = "_amp0"
	End Select
End Sub

'dim objrtx1(20), objrtx2(20), RtxBScnt
'dim objBallShadow(10)
'RtxInit

dim gBOT
gBOT = GetBalls


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

' The "DynamicBSUpdate" sub should be called with an interval of -1 (framerate)
' Place a toggleable variable (DynamicBallShadowsOn) in user options at the top of the script
' Import the "bsrtx7" and "ballshadow" images
' Copy in the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#, with at least as many objects each as there can be balls
'
' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection
' The easiest way to keep track of this is to start with the group on the left slingshot and move clockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'																E
'	A		 C													B
'	 B		D			your collection should look like		A		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F													C
'																D
'																F
'
'Update shadow options in the code to fit your table and preference

Const fovY					= 0		'Offset y position under ball to account for layback or inclination
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker, 1 will always be maxed even with 2 sources
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const Wideness				= 15	'Sets how wide the shadows can get (20 +5 thinness should be most realistic)
Const Thinness				= 2.5	'Sets minimum as ball moves away from source

Dim sourcenames, currentShadowCount

sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)


dim objrtx1(20), objrtx2(20)
dim objBallShadow(20)
DynamicBSInit

sub DynamicBSInit()
	Dim iii

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.10
		objrtx1(iii).visible = 0
		'objrtx1(iii).uservalue=0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.12
		objrtx2(iii).visible = 0
		'objrtx2(iii).uservalue=0
		currentShadowCount(iii) = 0
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
	Next
end sub


Sub RollingUpdate
	Dim falloff:        falloff 		= 150			'Max distance to light source, can be changed in code if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, b, currentMat, AnotherSource, BOT
	BOT = GetBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
	Next


	If UBound(BOT) = lob - 1 Then 
		For s = 0 to tnob
			StopSound("BallRoll_" & s & ampFactor)
		Next
		Exit Sub		'No balls in play, exit
	End If

	'The Magic happens here

'Ramp Sounds
	For s = lob to UBound(BOT)
		If BallVel(BOT(s) ) > 1 Then
			If ramprolling(s) = True Then
				ramprolling(s) = False
				StopSound ("PlasticRamp_" & s)
				StopSound ("WireRamp_" & s)
			End If
			rolling(s) = True
			PlaySound ("BallRoll_" & s & ampFactor), -1, VolPlayfieldRoll(BOT(s)) * 1.1 * VolumeDial, AudioPan(BOT(s)), 0, PitchPlayfieldRoll(BOT(s)), 1, 0, AudioFade(BOT(s))
		Else
			If rolling(s) = True Then
				StopSound("BallRoll_" & s & ampFactor)
				rolling(s) = False
			End If
		end If

'Normal ambient shadow
		If BOT(s).X < tablewidth/2 Then
			objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/13)) + 5
		Else
			objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/13)) - 5
		End If
		objBallShadow(s).Y = BOT(s).Y + fovY
		objBallShadow(s).Z = s/1000 + 0.04	'make ball shadows to be on different z-order, add ball height if upper pf

		If BOT(s).Z < 30 And BOT(s).Z > 0 Then		'Defining when (height-wise) you want ambient shadows
			objBallShadow(s).visible = 1
		Else
			objBallShadow(s).visible = 0
		end if

'Dynamic shadows
		If DynamicBallShadowsOn = 1 Then
			For Each Source in DynamicSources
				LSd=DistanceFast((BOT(s).x-Source.x),(BOT(s).y-Source.y))	'Calculating the Linear distance to the Source
				If BOT(s).Z < 30 And BOT(s).Z > 0 Then				'Defining when (height-wise) you want dynamic shadows
					If LSd < falloff and Source.state=1 Then	    		'If the ball is within the falloff range of a light and light is on
						currentShadowCount(s) = currentShadowCount(s) + 1	'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then					'1 dynamic shadow source
							sourcenames(s) = source.name
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
'							objrtx1(s).Z = s/1000 + 0.01 '+ BOT(s).Z-25
							objrtx1(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
							'debug.print "update1" & source.name & " at:" & ShadowOpacity

							currentMat = objBallShadow(s).material
							UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0

						Elseif currentShadowCount(s) = 2 Then
																	'Same logic as 1 shadow, but twice
							currentMat = objrtx1(s).material
							set AnotherSource = Eval(sourcenames(s))
							objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
'							objrtx1(s).Z = s/1000 + 0.01 '+ BOT(s).Z-25
							objrtx1(s).rotz = AnglePP(AnotherSource.x, AnotherSource.y, BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-(((BOT(s).x-AnotherSource.x)^2+(BOT(s).y-AnotherSource.y)^2)^0.5))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
'							objrtx2(s).Z = s/1000 + 0.02 '+ BOT(s).Z-25
							objrtx2(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity2 = (falloff-LSd)/falloff
							objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
							'debug.print "update2: " & source.name & " at:" & ShadowOpacity & " and "  & Eval(sourcenames(s)).name & " at:" & ShadowOpacity2

							currentMat = objBallShadow(s).material
							UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
						end if
					Else
						currentShadowCount(s) = 0
					End If
				Else									'Hide dynamic shadows everywhere else
					objrtx2(s).visible = 0 : objrtx1(s).visible = 0
				End If
			Next
		End If

		If BOT(s).z > 40 Then

'Balls Rolling on Plastic Ramps - By knowing the ball is NOT inside co-ordinate defined rectangles covering the wire ramp area
			If InRect(BOT(s).x, BOT(s).y, 0,0,965,0,965,1685,0,427) and Not InRect(BOT(s).x, BOT(s).y,600,430,777,489,656,1308,490,1114) Then
				'debug.print "plastic: " & b & " at z: " & gBOT(s).z
				ramprolling(s) = True
				StopSound ("WireRamp_" & s):PlaySound ("PlasticRamp_" & s), -1, VolPlasticRampRoll(BOT(s)) * 1.1 * VolumeDial, AudioPan(BOT(s)), 0, BallPitchV(BOT(s)), 1, 0, AudioFade(BOT(s))
'				objBallShadow(s).Z = BOT(s).Z - Ballsize/2
'				objBallShadow(s).RotX = -40

'Balls Rolling on Wire Ramps - By knowing the ball is inside co-ordinate defined rectangles covering the wire ramp area.
'				debug.print "wire: " & s & " at z: " & BOT(s).z
			Else
				If ramprolling(s) = True Then
					ramprolling(s) = False
					StopSound ("PlasticRamp_" & s):StopSound ("WireRamp_" & s)
				End If
			End If
		Else
			If ramprolling(s) = True Then
				ramprolling(s) = False
				StopSound ("PlasticRamp_" & s):StopSound ("WireRamp_" & s)
			End If
		End If

'Ball Drop
		If BOT(s).VelZ < -1 and BOT(s).z < 55 and BOT(s).z > 27 Then 'height adjust for ball drop sounds
			'debug.print "ball drop" & BOT(s).velz
			If DropCount(s) >= 2 Then
				DropCount(s) = 0
				If BOT(s).velz > -7 Then
					'debug.print "random sound soft"
					RandomSoundBallBouncePlayfieldSoft BOT(s)
				Else
					'debug.print "random sound hard"
					RandomSoundBallBouncePlayfieldHard BOT(s)
				End If				
			End If
		End If
		If DropCount(s) < 2 Then
			DropCount(s) = DropCount(s) + 1
		End If

		If BallinSw52 = 1 then
			If BOT(s).z < -60 and BOT(s).z > -110 and InRect(BOT(s).x,BOT(s).y,(Primitive012.x-40),(Primitive012.y-40),(Primitive012.x+40),(Primitive012.y-40),(Primitive012.x+40),(Primitive012.y+40),(Primitive012.x-40),(Primitive012.y+40)) Then
				'Right VUK top
				Controller.Switch(53) = 1
				debug.print BOT(s).z
			End If
		End If
	Next
End Sub


Function DistanceFast(x, y)
	dim ratio, ax, ay
	'Get absolute value of each vector
	ax = abs(x)
	ay = abs(y)
	'Create a ratio
	ratio = 1 / max(ax, ay)
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function


'***************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************


Function DistanceFast(x, y)
	dim ratio, ax, ay
	'Get absolute value of each vector
	ax = abs(x)
	ay = abs(y)
	'Create a ratio
	ratio = 1 / max(ax, ay)
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'****************************************************************
' Section; Debug Shot Tester 3.1
' 1.  Raise/Lower outlanes and drain posts by pressing 2 key
' 2.  Capture and Launch ball, Press and hold one of the buttons (W, E, R, Y, U, I, P, A) below to capture ball by flipper.  Release key to shoot ball
' 3.  To change the test shot angles, press and hold a key and use Flipper keys to adjust the shot angle.  Shot angles are saved into the User direction as cgamename.txt
' 4.  Set TestMode = 0 to disable test code.
'
' HOW TO INSTALL: Copy all objects from Layer 2 to table and adjust. Copy the Debug Shot Tester code starting at line 500 to bottom of table script.
'	Add "TestTableKeyDownCheck keycode" to top of Table1_KeyDown sub and add "TestTableKeyUpCheck keycode" to top of Table1_KeyUp sub
'****************************************************************
Const TestMode = 0		'Set to 0 to disable.  1 to enable
Dim KickerDebugForce: KickerDebugForce = 55

' Enable Disable Outlane and Drain Blocker Wall for debug testing
Dim BLState
debug_BLW1.IsDropped=1:debug_BLP1.Visible=0:debug_BLR1.Visible=0: debug_BLW2.IsDropped=1:debug_BLP2.Visible=0:debug_BLR2.Visible=0: debug_BLW3.IsDropped=1:debug_BLP3.Visible=0:debug_BLR3.Visible=0
Sub BlockerWalls

	BLState = (BLState + 1) Mod 4
	'	debug.print "BlockerWalls"
	PlaySound ("Start_Button")
	Select Case BLState:
		Case 0
			debug_BLW1.IsDropped=1:debug_BLP1.Visible=0:debug_BLR1.Visible=0: debug_BLW2.IsDropped=1:debug_BLP2.Visible=0:debug_BLR2.Visible=0: debug_BLW3.IsDropped=1:debug_BLP3.Visible=0:debug_BLR3.Visible=0
		Case 1:
			debug_BLW1.IsDropped=0:debug_BLP1.Visible=1:debug_BLR1.Visible=1: debug_BLW2.IsDropped=0:debug_BLP2.Visible=1:debug_BLR2.Visible=1: debug_BLW3.IsDropped=0:debug_BLP3.Visible=1:debug_BLR3.Visible=1
		Case 2:
			debug_BLW1.IsDropped=0:debug_BLP1.Visible=1:debug_BLR1.Visible=1: debug_BLW2.IsDropped=0:debug_BLP2.Visible=1:debug_BLR2.Visible=1: debug_BLW3.IsDropped=1:debug_BLP3.Visible=0:debug_BLR3.Visible=0
		Case 3:
			debug_BLW1.IsDropped=1:debug_BLP1.Visible=0:debug_BLR1.Visible=0: debug_BLW2.IsDropped=1:debug_BLP2.Visible=0:debug_BLR2.Visible=0: debug_BLW3.IsDropped=0:debug_BLP3.Visible=1:debug_BLR3.Visible=1
	End Select
End Sub


Sub TestTableKeyDownCheck (Keycode)


	If TestMode = 1 Then

		'Cycle through Outlane/Centerlane blocking posts
		'-----------------------------------------------
		If Keycode = 3 Then	
			BlockerWalls
		End If

		'Capture and launch ball:	
		'	Press and hold one of the buttons (W, E, R, T, Y, U, I, P) below to capture ball by flipper.  Release key to shoot ball
		'	In the Keydown sub, set up ball launch angle and force
		'--------------------------------------------------------------------------------------------
		If keycode = 17 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleW	'W key
		If keycode = 18 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleE	'E key
		If keycode = 19 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleR	'R key
		If keycode = 21 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleY	'Y key
		If keycode = 22 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleU	'U key
		If keycode = 23 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleI	'I key
		If keycode = 25 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleP	'P key
		If keycode = 30 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleA	'A key
		If keycode = 31 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleS	'S key
		If keycode = 33 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleF	'F key
		If keycode = 34 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleG	'G key
		If keycode = 35 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleH	'H key

		If KickerDebug.enabled = true Then		'Use Flippers to adjust angle while holding key
			If keycode = leftflipperkey Then
				TestKickAim.Visible = True
				TestKickerVar = TestKickerVar - 1
				debug.print TestKickerVar
			ElseIf keycode = rightflipperkey Then
				TestKickAim.Visible = True
				TestKickerVar = TestKickerVar + 1
				debug.print TestKickerVar
			End If
			TestKickAim.ObjRotz = TestKickerVar 
		End If

	End If
End Sub


Sub TestTableKeyUpCheck (Keycode)

	' Capture and launch ball:
	' Press and hold one of the buttons below to capture ball by flipper.  Release to shoot ball. Set up angle and force as needed for each shot.  
	'--------------------------------------------------------------------------------------------
	If TestMode = 1 Then
		If keycode = 17 Then TestKickAngleW = TestKickerVar : KickerDebug.kick TestKickAngleW, KickerDebugForce: KickerDebug.enabled = false		'W key		
		If keycode = 18 Then TestKickAngleE = TestKickerVar : KickerDebug.kick TestKickAngleE, KickerDebugForce: KickerDebug.enabled = false		'E key		
		If keycode = 19 Then TestKickAngleR = TestKickerVar : KickerDebug.kick TestKickAngleR, KickerDebugForce: KickerDebug.enabled = false: 		'R key		
		If keycode = 21 Then TestKickAngleY = TestKickerVar : KickerDebug.kick TestKickAngleY, KickerDebugForce: KickerDebug.enabled = false		'Y key		
		If keycode = 22 Then TestKickAngleU = TestKickerVar : KickerDebug.kick TestKickAngleU, KickerDebugForce: KickerDebug.enabled = false		'U key		
		If keycode = 23 Then TestKickAngleI = TestKickerVar : KickerDebug.kick TestKickAngleI, KickerDebugForce: KickerDebug.enabled = false		'I key		
		If keycode = 25 Then TestKickAngleP = TestKickerVar : KickerDebug.kick TestKickAngleP, KickerDebugForce: KickerDebug.enabled = false: TestWallRaise		'P key		
		If keycode = 30 Then TestKickAngleA = TestKickerVar : KickerDebug.kick TestKickAngleA, KickerDebugForce+5: KickerDebug.enabled = false		'A key		
		If keycode = 31 Then TestKickAngleS = TestKickerVar : KickerDebug.kick TestKickAngleS, KickerDebugForce: KickerDebug.enabled = false		'S key		
		If keycode = 33 Then TestKickAngleF = TestKickerVar : KickerDebug.kick TestKickAngleF, KickerDebugForce: KickerDebug.enabled = false		'F key		
		If keycode = 34 Then TestKickAngleG = TestKickerVar : KickerDebug.kick TestKickAngleG, KickerDebugForce: KickerDebug.enabled = false		'G key		
		If keycode = 35 Then TestKickAngleH = TestKickerVar : KickerDebug.kick TestKickAngleH, KickerDebugForce: KickerDebug.enabled = false		'H key		

		'		EXAMPLE CODE to set up key to cycle through 3 predefined shots
		'		If keycode = 17 Then 	'Cycle through all left target shots
		'			If TestKickerAngle = -28 then 
		'				TestKickerAngle = -24
		'			ElseIf TestKickerAngle = -24 Then
		'				TestKickerAngle = -19
		'			Else
		'				TestKickerAngle = -28
		'			End If
		'			KickerDebug.kick TestKickerAngle, KickerDebugforce: KickerDebug.enabled = false		 	'W key	
		'		End If

	End If

	If (KickerDebug.enabled = False and TestKickAim.Visible = True) Then 'Save Angle changes
		TestKickAim.Visible = False
		SaveTestKickAngles
	End If

End Sub

TestWall.Isdropped = 1	'Test shot to Joker Mouth	
Sub TestWallRaise
	TestWall.Isdropped = 0
	TestWall.TimerInterval = 1000
	TestWall.TimerEnabled = 1
End Sub

Sub TestWall_Timer
	TestWall.IsDropped = 1
	TestWall.TimerEnabled = 0
End Sub


Dim TestKickerAngle, TestKickerAngle2, TestKickerVar, TeskKickKey, TestKickForce
Dim TestKickAngleWDefault, TestKickAngleEDefault, TestKickAngleRDefault, TestKickAngleYDefault, TestKickAngleUDefault, TestKickAngleIDefault 
Dim TestKickAnglePDefault, TestKickAngleADefault, TestKickAngleSDefault, TestKickAngleFDefault, TestKickAngleGDefault, TestKickAngleHDefault
Dim TestKickAngleW, TestKickAngleE, TestKickAngleR, TestKickAngleY, TestKickAngleU, TestKickAngleI
Dim TestKickAngleP, TestKickAngleA, TestKickAngleS, TestKickAngleF, TestKickAngleG, TestKickAngleH
TestKickAngleWDefault = -30
TestKickAngleEDefault = -27
TestKickAngleRDefault = -25
TestKickAngleYDefault = -20
TestKickAngleUDefault = -15
TestKickAngleIDefault = -14
TestKickAnglePDefault = -15
TestKickAngleADefault = -5
TestKickAngleSDefault = 11
TestKickAngleFDefault = 20
TestKickAngleGDefault = 22
TestKickAngleHDefault = 24

If TestMode = 1 Then LoadTestKickAngles

Sub SaveTestKickAngles
	Dim FileObj, OutFile
	Set FileObj = CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then Exit Sub
	Set OutFile=FileObj.CreateTextFile(UserDirectory & cGameName & ".txt", True)

	OutFile.WriteLine TestKickAngleW
	OutFile.WriteLine TestKickAngleE
	OutFile.WriteLine TestKickAngleR
	OutFile.WriteLine TestKickAngleY
	OutFile.WriteLine TestKickAngleU
	OutFile.WriteLine TestKickAngleI
	OutFile.WriteLine TestKickAngleP
	OutFile.WriteLine TestKickAngleA
	OutFile.WriteLine TestKickAngleS
	OutFile.WriteLine TestKickAngleF
	OutFile.WriteLine TestKickAngleG
	OutFile.WriteLine TestKickAngleH
	OutFile.Close

	Set OutFile = Nothing
	Set FileObj = Nothing
End Sub

Sub LoadTestKickAngles
	Dim FileObj, OutFile, TextStr

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then MsgBox "User directory missing": Exit Sub

	If FileObj.FileExists(UserDirectory & cGameName & ".txt") then 

		Set OutFile = FileObj.GetFile(UserDirectory & cGameName & ".txt")
		Set TextStr = OutFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream = True) then
			Exit Sub
		End if

		TestKickAngleW = TextStr.ReadLine
		TestKickAngleE = TextStr.ReadLine
		TestKickAngleR = TextStr.ReadLine
		TestKickAngleY = TextStr.ReadLine
		TestKickAngleU = TextStr.ReadLine
		TestKickAngleI = TextStr.ReadLine
		TestKickAngleP = TextStr.ReadLine
		TestKickAngleA = TextStr.ReadLine
		TestKickAngleS = TextStr.ReadLine
		TestKickAngleF = TextStr.ReadLine
		TestKickAngleG = TextStr.ReadLine
		TestKickAngleH = TextStr.ReadLine
		TextStr.Close

	Else
		'create file
		TestKickAngleW = TestKickAngleWDefault: 
		TestKickAngleE = TestKickAngleEDefault 
		TestKickAngleR = TestKickAngleRDefault
		TestKickAngleY = TestKickAngleYDefault
		TestKickAngleU = TestKickAngleUDefault
		TestKickAngleI = TestKickAngleIDefault    
		TestKickAngleP = TestKickAnglePDefault
		TestKickAngleA = TestKickAngleADefault
		TestKickAngleS = TestKickAngleSDefault
		TestKickAngleF = TestKickAngleFDefault
		TestKickAngleG = TestKickAngleGDefault
		TestKickAngleH = TestKickAngleHDefault
		SaveTestKickAngles

	End If

	Set OutFile = Nothing
	Set FileObj = Nothing

End Sub
'****************************************************************
' End of Section; Debug Shot Tester 3.1
'****************************************************************

If CabinetMode Then PinCab_Rails.visible = 0 else PinCab_Rails.visible = 1

'*********************
'VR Mode
'*********************
DIM VRThings
If VRRoom > 0 Then
	Scoretext.visible = 0
	museumflash1.visible = 1
	museumflash2.visible = 0
	for each VRThings in VR_Pegs:VRThings.EnableStaticRendering = 0:Next
	If VRRoom = 1 Then
		for each VRThings in VR_Stuff:VRThings.visible = 1:Next
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		If BackglassFlash = 1 Then
			PinCab_Backglass.visible = 0
			BGDark.visible = 1
			SetBackglass
			UpdateMultipleLamps
			BGFlashers.enabled = 1
		End If
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Stuff:VRThings.visible = 0:Next
		for each VRThings in VR_Cab:VRThings.visible = 0:Next
'		for each VRThings in VRBGFlashers:VRThings.visible = 0:Next
		PinCab_Backbox.visible = 1
		PinCab_Backglass.visible = 1
		If BackglassFlash = 1 Then
			PinCab_Backglass.visible = 0
			BGDark.visible = 1
			SetBackglass
			UpdateMultipleLamps
			BGFlashers.enabled = 1
		End If
		DMD.visible = 1
		PinCab_Backbox.image = "PinCab_Backbox_Min"
		BGFlashers.enabled = 0
		TimerVRPlunger.enabled = 0
		TimerVRPlunger2.enabled = 0
	End If
Else
	museumflash1.visible = 0
	museumflash2.visible = 1
	for each VRThings in VR_Stuff:VRThings.visible = 0:Next
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	for each VRThings in VRBGFlashers:VRThings.visible = 0:Next
	for each VRThings in OffInserts:VRThings.NormalMap = "":Next
	BGFlashers.enabled = 0
	TimerVRPlunger.enabled = 0
	TimerVRPlunger2.enabled = 0
End if


dim preloadCounter
sub preloader_timer
	preloadCounter = preloadCounter + 1
	if preloadCounter = 1 then
		'BothPrimsVisible
		'OffPrimSwap 1,1,False
	Elseif preloadCounter = 2 then
		'OffPrimSwap 1,0,False
	Elseif preloadCounter = 3 then
		'OffPrimSwap 2,1,False
	Elseif preloadCounter = 4 then
		'OffPrimSwap 2,0,False
	Elseif preloadCounter = 5 then
		'OffPrimSwap 1,1,True
		'OffPrimsVisible False
	Elseif preloadCounter = 6 then
		'sol4r true
	Elseif preloadCounter = 7 then
		'sol2r true
	Elseif preloadCounter = 8 then
		'lampz.state(111) = 5
	Elseif preloadCounter = 12 then
		me.enabled = false
	end if
	gBOT = GetBalls
	'msgbox preloadCounter
end sub

'******************************************************
' 					LUT
'******************************************************


Sub SetLUT  'AXS
	Table1.ColorGradeImage = "LUT" & LUTset
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	Select Case LUTSet
		Case 0: LUTBox.text = "Fleep Natural Dark 1"
		Case 1: LUTBox.text = "Fleep Natural Dark 2"
		Case 2: LUTBox.text = "Fleep Warm Dark"
		Case 3: LUTBox.text = "Fleep Warm Bright"
		Case 4: LUTBox.text = "Fleep Warm Vivid Soft"
		Case 5: LUTBox.text = "Fleep Warm Vivid Hard"
		Case 6: LUTBox.text = "Skitso Natural and Balanced"
		Case 7: LUTBox.text = "Skitso Natural High Contrast"
		Case 8: LUTBox.text = "3rdaxis Referenced THX Standard"
		Case 9: LUTBox.text = "CalleV Punchy Brightness and Contrast"
		Case 10: LUTBox.text = "TT & Ninuzzu Original"
  		Case 11: LUTBox.text = "*VPin Workshop Original"
        Case 12: LUTBox.text = "1on1"
        Case 13: LUTBox.text = "bassgeige"
        Case 14: LUTBox.text = "blacklight"          
	End Select
	LUTBox.TimerEnabled = 1
End Sub

Sub SaveLUT
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if LUTset = "" then LUTset = 0 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "BATMANDELUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub
Sub LoadLUT
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=11
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "BATMANDELUT.txt") then
		LUTset=11
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "BATMANDELUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=11
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub

Sub Table1_exit()
	SaveLUT
	Controller.Stop
End sub

'******************************************************
'*******	Set Up Backglass Flashers	*******
'******************************************************

Sub SetBackglass()
	Dim obj

	For Each obj In BackglassLow
		obj.x = obj.x
		obj.height = - obj.y + 241
		obj.y = -145 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
	Next


	For Each obj In BackglassMid
		obj.x = obj.x
		obj.height = - obj.y + 241
		obj.y = -174 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
	Next


	For Each obj In BackglassHigh
		obj.x = obj.x
		obj.height = - obj.y + 241
		obj.y = -200 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
	Next
End Sub


Set LampCallback = GetRef("UpdateMultipleLamps")

Sub UpdateMultipleLamps()
	If BackglassFlash = 1 Then
		If Controller.Lamp(50) = 0 Then: BGFlBatman.visible=0: BGFlMoon.visible=0: else: BGFlBatman.visible=1: BGFlMoon.visible=1
		If Controller.Lamp(51) = 0 Then: BGFlLamp51.visible=0: BGFlLamp51.visible=0: else: BGFlLamp51.visible=1: BGFlLamp51.visible=1
		If Controller.Lamp(52) = 0 Then: BGFlLamp52.visible=0: BGFlLamp52.visible=0: else: BGFlLamp52.visible=1: BGFlLamp52.visible=1
		If Controller.Lamp(53) = 0 Then: BGFlLamp53.visible=0: BGFlLamp53.visible=0: else: BGFlLamp53.visible=1: BGFlLamp53.visible=1
	End If
End Sub


Dim Flasherseq1
Sub BGFlashers_Timer
	If BackglassFlash = 1 Then
		Select Case Flasherseq1 'Int(Rnd*6)+1
			Case 1:BGGIBulb32.visible = 1 : BGFlAreaTopRightL.amount = 150
			Case 2:BGGIBulb32.visible = 0 : BGFlAreaTopRightL.amount = 100
			Case 3:BGGIBulb32.visible = 0 : BGFlAreaTopRightL.amount = 100
			Case 4:BGGIBulb32.visible = 0 : BGFlAreaTopRightL.amount = 100
			Case 5:BGGIBulb33.visible = 1 : BGFlAreaTopRightR.amount = 150
			Case 6:BGGIBulb33.visible = 0 : BGFlAreaTopRightR.amount = 100
			Case 7:BGGIBulb33.visible = 0 : BGFlAreaTopRightR.amount = 100 
			Case 8:BGGIBulb33.visible = 0 : BGFlAreaTopRightR.amount = 100 
		End Select	
		Flasherseq1 = Flasherseq1 + 1
		If Flasherseq1 > 8 Then
		Flasherseq1 = 1
		End if
	End If
End Sub

'******************* VR Plunger **********************

Sub TimerVRPlunger_Timer
  If PinCab_Shooter.Y < -99 then
       PinCab_Shooter.Y = PinCab_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger2_Timer
	PinCab_Shooter.Y = -215 + (5* Plunger.Position) -20
End Sub

