'   _____  _             ___  ___              _      _
'  |_   _|| |            |  \/  |             | |    (_)
'    | |  | |__    ___   | .  . |  __ _   ___ | |__   _  _ __    ___
'    | |  | '_ \  / _ \  | |\/| | / _` | / __|| '_ \ | || '_ \  / _ \ 
'    | |  | | | ||  __/  | |  | || (_| || (__ | | | || || | | ||  __/
'    \_/  |_| |_| \___|  \_|  |_/ \__,_| \___||_| |_||_||_| |_| \___|
'
'
'______        _      _                  __   ______  _         _             _
'| ___ \      (_)    | |                / _|  | ___ \(_)       | |           | |
'| |_/ / _ __  _   __| |  ___     ___  | |_   | |_/ / _  _ __  | |__    ___  | |_
'| ___ \| '__|| | / _` | / _ \   / _ \ |  _|  |  __/ | || '_ \ | '_ \  / _ \ | __|
'| |_/ /| |   | || (_| ||  __/  | (_) || |    | |    | || | | || |_) || (_) || |_
'\____/ |_|   |_| \__,_| \___|   \___/ |_|    \_|    |_||_| |_||_.__/  \___/  \__|
'
'
'  _    _  _  _  _  _                           __   _____  _____  __
' | |  | |(_)| || |(_)                         /  | |  _  ||  _  |/  |
' | |  | | _ | || | _   __ _  _ __ ___   ___   `| | | |_| || |_| |`| |
' | |/\| || || || || | / _` || '_ ` _ \ / __|   | | \____ |\____ | | |
' \  /\  /| || || || || (_| || | | | | |\__ \  _| |_.___/ /.___/ /_| |_
'  \/  \/ |_||_||_||_| \__,_||_| |_| |_||___/  \___/\____/ \____/ \___/


'The Machine (Bride of Pinbot) Williams 1991 for VP10 (Requires VP10.7 final release or greater to play)

'This project was started originally by wrd1972 and iaakki in VPW started to rebuild it to match the correct dimensions.
'This is almost a full rebuild as everything had to move and various things were fixed so that this can support PROC games too.
'
'Other VPW members who helped making this: 
' fluffhead, Leojreimroc, Fleep, Sixtoe, ClarkKent, rothbauerw, Nestorgian, Rawd, Cyberpez, Bord, Apophis, Wylte, Niwak, RIK, PinStartsDan
' 
'Most notable changes by VPW:
' - New table dimensions and new PF scan from ClarkKent
' - Totally new SSF sounds for flips, bumpers and slings made by Fleep. Recorded from Pinbot by major_drain_pinball
' - Baked lighting for GI and flashers
' - 3D inserts from Flupper and vectorized insert texts
' - New plastics and metal walls primitives
' - Support for Proc gamecode like BOP2.0
' - VRRoom with full backglass
' - Updated NF/Roth physics
' - Full change notes at the end of the file
'
' Original credits from the version we received from Wrd:
'***The very talented BOP development team.***
'Original VP10 beta by "Unclewilly and completed by wrd1972"
'Additional scripting assistance by "cyberpez", "Rothbauerw".
'Clear ramps, wire ramp primitives, pop-bumper caps and flasher domes, shadows and flipper prims by "Flupper"
'Additional 3D work by "Dark" and "Cyberpez"
'Space shuttle toy and sidewall graphics by "Cyberpez"
'Face rotation scripting by "KieferSkunk/Dorsola & "Rothbauerw""
'High poly helmet and re-texture by "Dark"
'Original bride helmet, plastics and playfield redraw by "Jc144"
'Desktop view, scoring dials aand background by "32Assassin"
'Full function ball trough by "cyberpez"
'Lighting by wrd1972
'Table physics by "wrd1972"
'Modulated GI lighting by "nFozzy"
'LED color GI lighting by "cyberpez"
'DOF and controller by "Arngrim"
'***Many...many thanks to you all for the tremenedous efforts and hard work for making this awesome table a reality. I just cant thank you all enough.***

'NOTE: On rare occasions and based on the performance of the PC being used, the BOP face rotation can become un-syncronized.
'When this happens, the incorrect face will be shown at table start-up. To fix this, delete the "BOP_L*.nv" file located in the NVRAM folder, inside your VpinMAME directory.


'1.0.0	iaakki 	- initial
'1.0.1	iaakki 	- tweak to make ramp exits to be slightly more random, Flasher Dome color script option, Display timer tweaks, default GI tuned slightly more warm
'1.0.2	iaakki	- fix to PROC lampz assignment, ramp7 to non-collidable, fix PROC dof calls for headmech, fix ball sink to center lock post ramp, 
'				backhand to main ramp made harder as it should be.
'1.0.3	iaakki	- SS lights adjusted, top Sling DOF tested and it works, major_drain_pinball added to SSF sound credits, eyes-mouth lights adjusted, 
'				head2 objects adjusted, BL timing adjusted, PROC head orientation retained on load, added a small ramp at the end of SSlane to prevent upward rolls.
'1.0.4	iaakki	- Cabinetmode side bake color fix, helmet reflection color fix, SS Ramp drop sounds increased, new flipper bats with colors, new instructions card,
'				blocker walls added under the table to prevent backdrop show up through helmet mech PF holes, VR plunger checked to work correct.


Option Explicit
Randomize
Dim OptReset


''***************************************************************************************************************************************************************
' _____     _     _        ___________ _   _                   _   _
'|_   _|   | |   | |      |  _  | ___ \ | (_)                 | | | |
'  | | __ _| |__ | | ___  | | | | |_/ / |_ _  ___  _ __  ___  | |_| | ___ _ __ ___
'  | |/ _` | '_ \| |/ _ \ | | | |  __/| __| |/ _ \| '_ \/ __| |  _  |/ _ \ '__/ _ \ 
'  | | (_| | |_) | |  __/ \ \_/ / |   | |_| | (_) | | | \__ \ | | | |  __/ | |  __/
'  \_/\__,_|_.__/|_|\___|  \___/\_|    \__|_|\___/|_| |_|___/ \_| |_/\___|_|  \___|
'

'VR
'VR Room Off = 0
'VR Room BaSti/Rawd = 1
'VR Room Minimal = 2
'VR Room Ultra Minimal = 3
Const VRRoom = 0

'Flasher Fading Performance
'Flasher Fading Performance
'High perf system	= 0 --> smoother fading ~58 fading steps on drain
'Low perf system 	= 1 --> more coarse fading ~38 fading steps on drain (recommended for VR)
Const FlasherFadingPerf = 1

'UsePinmameModulatedSolenoids
'True	= Uses modulated flasher events from pinmame
'False	= Uses binary on-off flasher events from pinmame
Const UsePinmameModulatedSolenoids = True

'Flasher Fading Performance
'High perf system	= 0 --> adjustments made to inserts and flashers based on GI state
'Low perf system 	= 1 --> no adjustments made to inserts and flashers based on GI state (recommended for VR)
Const GIPerf = 1

Const CabinetMode = 0

'GI Color Mod
	'0 	= Randomly sellected color
	'1 	= Incandescent bulbs (slower fade than leds)
	'2 	= Warm White LEDs
	'3 	= Cool white LEDs
	'4 	= Red LEDs
	'5 	= Orange LEDs
	'6 	= Yellow LEDs
	'7 	= Green LEDs
	'8 	= Blue LEDs
	'9 	= Indigo LEDs
	'10 = Violet LEDs
	'11 = Choose your own color (RBG)
	'12 = Automatic color changing bulbs
		'Change the value below to set option
		GIColorMod = 1

			'Enter RGB values below for "choose your own" GI LED color. Remember to set colormod option to 11.
			GIColorRed       =  255	
			GIColorGreen     =  197
			GIColorBlue      =  143

				'Adjust automatic color change rate
				'Lower number = faster
				'Higher number = slower
				CCRate = 50

'Flasher Dome color Mod.
	'0	= Red
	'1	= Green
	'2	= Blue
	'3	= Yellow
	'4	= Purple
	'5	= White
	'6	= Random
DomeColor = 0

'SIDEWALLS MOD
'Black Wood = 0
'Starfield =1
'BOP Blades = 2
'Random = 3
Sidewalls = 0


'SIDE RAILS
'   Show Side Rails = 0
'   Hide Side Rails = 1
'Change the value below to set option
Rails = 0


'CHEATER POST MOD
'Hide Cheater Post = 0
'Show Cheater Post = 1
cheaterpost = 0





'HELMET LIGHTS COLOR MOD
'0 = Random
'1 = White lights
'2 = BOP 2.0 Lights
'3 = Custom
'4 = ColorChanging
'5 = ColorChanging Rainbow
HLcolor = 1


			'Enter RGB values below for "choose your own" Helmet Light color
			HLColorRed       =  249	
			HLColorGreen     =  15
			HLColorBlue      =  255


'SKILL SHOT RAMP COLOR MOD
'Skill Shot Ramp Normal = 0
'Skill Shot Ramp Colored = 1
'Random = 2
SSRampColorMod = 0


'RUBBERS MOD
'Black Rubbers = 0
'White Rubbers = 1
'Random = 2
RubberColor = 1


'Flippers Rubbers Mod
'Black Rubbers 		= 0	'hard / comp
'Red Rubbers 		= 1	'softer / casual
'Blue Rubbers 		= 2	'bouncy / casual
'Purple Rubbers 	= 3	'bouncy / casual
'Green Rubbers 		= 4	'bouncy / casual
'Orange Rubbers 	= 5	'bouncy / casual
'BrightRed Rubbers 	= 6	'bouncy / casual
'Yellow Rubbers 	= 7	'bouncy / casual
'Random 			= 8
FlipperRubbers = 0


'SHUTTLE TOY MOD
'Hide Shuttle =0
'Show Shuttle =1
ShipMod = 0


'Hottie Face Mod
'Normal = 0
'Ashley = 1
'Brooke = 2
'Harley = 3
'Kelly = 4
'Lisa = 5
'Meagan = 6
'Shelly = 7
'Rachel = 8
'Valerie = 9
'Random = 10
HottieMod = 0



bPROC = false


dim DomeColor
'PLAYFIELD SHADOW INTENSITY (adds additional visual depth)
'Usable range is 0 (lighter) - 100 (darker)
'shadowopacity = 80


'////////////////////////////////////////////////////////////////////////////////
'////          Mechanical Sounds Options, by Fleep                           ////
'////////////////////////////////////////////////////////////////////////////////
'
'//////////////////////////  MECHANICAL SOUNDS OPTIONS  /////////////////////////
'//  This section allows to set various general sound options for the mechanical sounds.
'//  For the entire sound system scripts see mechanical sounds block down below in the project.
'
'////////////////////////////  GENERAL SOUND OPTIONS  ///////////////////////////
'
'//  PositionalSoundPlaybackConfiguration:
'//  Specifies the sound playback configuration. Options:
'//  1 = Mono
'//  2 = Stereo (Only L+R channels)
'//  3 = Surround (Full surround sound support for SSF - Front L + Front R + Rear L + Rear R channels)
Const PositionalSoundPlaybackConfiguration = 3
'
'
'//  RelaysPosition:
'//  1 = Relays positioned with power board (Provides sound spread from the left and right back channels)
'//  2 = Relays positioned with GI strings (Provides sound spread from left/right/front/rear surround channels)
Const RelaysPosition = 2
'
'
''//  SpinWheelsMotorConfiguration:
''//  1 = Spin Wheels Motor Sound disabled, DOF disabled
''//  2 = Spin Wheels Motor Sound disabled, DOF enabled
''//  3 = Spin Wheels Motor Sound enabled, DOF disabled
''//  4 = Spin Wheels Motor Sound enabled, DOF enabled
'Const SpinWheelsMotorConfiguration = 4
''
''
''//  BackwallFanBlowerConfiguration:
''//  1 = FanBlower Sound disabled, DOF disabled
''//  2 = FanBlower Sound disabled, DOF enabled
''//  3 = FanBlower Sound enabled, DOF disabled
''//  4 = FanBlower Sound enabled, DOF enabled
'Const BackwallFanBlowerConfiguration = 1
'
'
'//  VolumeDial:
'//  VolumeDial is the actual global volume multiplier for the mechanical sounds.
'//  Values smaller than 1 will decrease mechanical sounds volume.
'//  Recommended values should be no greater than 1. 
'//  Default value is 0.8 and is optimal.
Const VolumeDial = 0.8
'
'
'//  PlayfieldRollVolumeDial:
'//  PlayfieldRollVolumeDial is a constant volume multiplier for the playfield rolling sound.
'//  Default value should be 1 which will guarantee a proper carefully calculated dynamic volume changes profile.
'//  Any values different than the default will impact the volume level and the dynamic voume changes profile.
Const PlayfieldRollVolumeDial = 1
'
'
'//  RampsRollVolumeDial:
'//  RampsRollVolumeDial is a constant volume multiplier for all ramps rolling sounds. 
'//  This includes all types of plastic and metal ramps.
'//  Default value should be 1 which will guarantee a proper carefully calculated dynamic volume changes profile.
'//  Any values different than the default will impact the volume level and the dynamic voume changes profile.
'//  For a specific ramp volume change please refer to the corresponding volume variables in the script sound paramter section.
Const RampsRollVolumeDial = 1
'
'
'////////////////////////////////////////////////////////////////////////////////
'////          End of Fleep Mechanical Sounds Options                        ////
'////////////////////////////////////////////////////////////////////////////////





' ********************************************************************************
' Sound Options
'

'Const VolDiv = 500    ' Smaller value - louder sound.
''
'
'Const VolRol    = 1    ' Rollovers volume multiplier.
''Const VolRub    = 3    ' Rubbers volume multiplier.
'
'
'
''Const VolCol    = 3    ' Ball Collition volume.
'
''

'***************************************************************************************************************************************************************


'----- Phsyics Mods -----
' Enhance micro bounces on flippers
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 1.2 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

'RomName
cGameName = "bop_l7"

Const BallSize = 50  'Ball radius
Const ballmass = 1  'Ball mass
Const UseVPMModSol = true



On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim cGameName, ShipMod, cheaterpost, sidewalls, Prevgameover, bPROC


Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components

Else

End if

dim UseSolenoids

'Const UseVPMColoredDMD = false

SetLocale(1033)

'Variables
Dim bsSaucer, bsLEye, bsREye, bsSS, plungerIM, mFace, xx, bump1, bump2, bump3
Dim mechHead, headAngle, prevHeadAngle, currentFace

Const UseSync = 1
Const UseLamps = 0
Const HandleMech = 0

If Not bPROC then
	LoadVPM "01560000", "WPC.VBS", 3.26
	UseSolenoids = 2
else
	LoadPROC "01560000", "WPC.VBS", 3.26
	UseSolenoids = 39
	UseSolenoids = 39
end if

'Standard Sounds
'Const SSolenoidOn = "Solenoid"
'Const SSolenoidOff = ""
'Const SCoin = "fx_Coin"

if CabinetMode = 1 Then
	Rails = 1
	pSidewall.size_y = 1.6
	pSidewall.rotx = 89
	pSidewall.z = -66
Else
	pSidewall.size_z = 1
	pSidewall.rotx = 91.4
	pSidewall.z = -82

end if

'******************************************************
' 				COLLECTIONS TO ARRAYS
'******************************************************
Dim BluePegsArr, GIBulbsFrontArr, GIBulbsRearArr, GIFrontArr, GIRearArr, GIRearColorArr, HelmetLightReflArr, HelmetLightsArr

BluePegsArr = ColtoArray(BluePegs)
GIBulbsFrontArr = ColtoArray(GIBulbsFront)
GIBulbsRearArr = ColtoArray(GIBulbsRear)

GIFrontArr = ColtoArray(GIFront)
GIRearArr = ColtoArray(GIRear)
GIRearColorArr = ColtoArray(GIRearColor)

HelmetLightReflArr = ColtoArray(HelmetLightRefl)
HelmetLightsArr = ColtoArray(HelmetLights)

'******************************************************
' 				TABLE INIT
'******************************************************

Dim BPBall1, BPBall2, BPBall3, BOT

Sub Table1_Init
    vpmInit Me
	On Error Resume Next
    With Controller
        .GameName =  cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "The Machine BOP, Williams 1991"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
		 if .Version >= "03020000" Then
			.HandleMechanics = -1   ' -1: Reset internal head position, 
		 Else
			.HandleMechanics = 0
		 end if 
        .Hidden = 0
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With


'	ChangeBats(ChooseBats)

    'Nudging
    vpmNudge.TiltSwitch=14
    vpmNudge.Sensitivity=5
    vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot,UpperSlingShot)


    ' Impulse Plunger
    Const IMPowerSetting = 35 'Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP SSLaunch, IMPowerSetting, IMTime
        .Random 0.3
        .switch 31
        .InitExitSnd SoundFX("fx_KickerRelease",DOFContactors), SoundFX("SY_TNA_REV01_Auto_Launch_Coil_4",DOFContactors)	'todo
        .CreateEvents "plungerIM"
    End With


	if Not bPROC then
      Set mechHead = New cvpmMyMech
      With mechHead
'         .MType = vpmMechOneDirSol + vpmMechCircle + vpmMechLinear + vpmMechFast
         .MType = vpmMechOneDirSol + vpmMechCircle + vpmMechLinear + vpmMechSlow
         .Sol1 = 28
         .Sol2 = 27
'         .Length = 60 * 10 * 6	 ' 10 seconds to cycle through all faces.
         .Length = 60 * 10	 ' 10 seconds to cycle through all faces.
         .Steps = 360 * 4    ' 4 half wheel rotations for one full head rotation
	     .InitialPos = 0
'		 .AddSw 67, 0, 150
'		 .AddSw 67, 210, 510
'         .AddSw 67, 570, 870
'		 .AddSw 67, 1290, 1440
         .Callback = GetRef("HeadMechCallback")
         .Start
      End With
	end if

    '**Main Timer init
    PinMAMETimer.Enabled = 1

    PrevGameOver = 0

    SetOptions

	'************  Trough	**************************
	Set BPBall1 = sw27.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set BPBall2 = sw26.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set BPBall3 = ballrelease.CreateSizedballWithMass(Ballsize/2,Ballmass)	
	Controller.Switch(27) = 1
	Controller.Switch(26) = 1
	Controller.Switch(25) = 1
	
	BOT = Array(bpball1, bpball2, bpball3)

	Face_eyes.collidable = False
	Face_flat.collidable = False

	if Not bPROC then
		controller.Switch(67)=1
	else
'		visible init rotation to face 3
'		vpmtimer.addtimer 10, "motor_dir true '"
'		vpmtimer.addtimer 200, "motor_sol true '"
'		vpmtimer.addtimer 3200, "motor_sol false '"
'		vpmtimer.addtimer 3300, "motor_sol true '"
'		vpmtimer.addtimer 6500, "motor_sol false '"

		'do we really need to save the heads previous orientation on table exit and then reset to that once game is started with proc? This is prone to errors.
		'yes
		LoadHEAD
'		currentface = 3 : headCurrAngle = 804
		if currentFace = 1 then headCurrAngle = 84
		if currentFace = 2 then headCurrAngle = 446
		if currentFace = 3 then headCurrAngle = 804
		if currentFace = 4 then headCurrAngle = 1152
		HeadMechCallback headCurrAngle, 0, headCurrAngle
	end if

	SetGiColor
	tableStarted=true
End Sub

Sub SaveHEAD
    Dim FileObj
    Dim ScoreFile
    Set FileObj=CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory) then
      Exit Sub
    End if
    Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "BopPROCHEAD.txt",True)
      ScoreFile.WriteLine currentface
    Set ScoreFile=Nothing
    Set FileObj=Nothing
End Sub
Sub LoadHEAD
  Dim FileObj, ScoreFile, TextStr
   Set FileObj=CreateObject("Scripting.FileSystemObject")
  If Not FileObj.FolderExists(UserDirectory) then
    currentface=3
    Exit Sub
  End if
  If Not FileObj.FileExists(UserDirectory & "BopPROCHEAD.txt") then
    currentface=3
    Exit Sub
  End if
  Set ScoreFile=FileObj.GetFile(UserDirectory & "BopPROCHEAD.txt")
  Set TextStr=ScoreFile.OpenAsTextStream(1,0)
    If (TextStr.AtEndOfStream=True) then
      Exit Sub
    End if
    currentface = int (TextStr.ReadLine)
    Set ScoreFile = Nothing
      Set FileObj = Nothing
End Sub



dim tableStarted:tableStarted=False 

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
'Sub Table1_Exit:Controller.Stop:End Sub
Sub Table1_Exit()
'	msgbox "exit"
 if bProc then SaveHEAD
 Controller.Stop
End sub

'dim chgsol, xxx
Sub FrameTimer_Timer()
	Cor.Update
'	RollingTimer
	RollingSoundUpdate
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'	BallControl
	GraphicsTimer
    UpdateGatesSpinners

'	chgsol = controller.changedsolenoids
'	If Not IsEmpty(chgsol) Then
'		For xxx = 0 To UBound(chgsol)
'			'debug.print chgsol(xxx, 0) &" -> "& chgsol(xxx, 1)
'		next
'	End If

End Sub

'*****Keys
Sub Table1_KeyDown(ByVal keycode)
     If keycode = plungerkey then plunger.PullBack:SoundPlungerPull()

	If keycode = StartGameKey Then 
		soundStartButton()
'		If VRroom >0 then VR_StartButton.y = VR_StartButton.y -5  'VRADDED StartButton				
	End if	

'     If Keycode = RightFlipperKey then SolRFlipper 1
'     If Keycode = LeftFlipperKey then SolLFlipper 1

	If keycode = LeftFlipperKey Then
		if bFlipsEnabled then
'			FlipperActivate LeftFlipper, LFPress
			if bProc then SolLFlipper 1
		end if
		bLeftFlipButton = true
		Controller.Switch(12) = 1
	End If

	If keycode = RightFlipperKey Then
		if bFlipsEnabled then
'			FlipperActivate RightFlipper, RFPress
			if bProc then SolRFlipper 1
		end if
		bRightFlipButton = true
		Controller.Switch(11) = 1
	End If

	If keycode = LeftTiltKey Then 
		Nudge 90, 1 
		SoundNudgeLeft() 			
	End if
	If keycode = RightTiltKey Then 
		Nudge 270, 1  
		SoundNudgeRight()			
	End if
	If keycode = CenterTiltKey Then 
		Nudge 0, 1  
		SoundNudgeCenter()				
	End if

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3  Then   
		Select Case Int(rnd*3)
			Case 0: PlaySoundAtLevelStatic ("Coin_In_1"), CoinSoundLevel, Drain
			Case 1: PlaySoundAtLevelStatic ("Coin_In_2"), CoinSoundLevel, Drain
			Case 2: PlaySoundAtLevelStatic ("Coin_In_3"), CoinSoundLevel, Drain
		End Select
	End If

If VRRoom = 1 or VRRoom = 2 Then
		If keycode = LeftFlipperKey Then
			VR_FB_Left.x = VR_FB_Left.x + 5
		End If
		If keycode = RightFlipperKey Then
			VR_FB_Right.x = VR_FB_Right.x - 5
		End If
		If keycode = StartGameKey Then
			VR_StartButton.y = VR_StartButton.y - 2
		End If
		If keycode = PlungerKey Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
	End If

    '************************   Start Ball Control 1/3
'        if keycode = 46 then                ' C Key
'            If contball = 1 Then
'                contball = 0
'            Else
'                contball = 1
'            End If
'        End If
'        if keycode = 48 then                'B Key
'            If bcboost = 1 Then
'                bcboost = bcboostmulti
'            Else
'                bcboost = 1
'            End If
'        End If
'        if keycode = 203 then bcleft = 1        ' Left Arrow
'        if keycode = 200 then bcup = 1          ' Up Arrow
'        if keycode = 208 then bcdown = 1        ' Down Arrow
'        if keycode = 205 then bcright = 1       ' Right Arrow
'    '************************   End Ball Control 1/3
    If vpmKeyDown(keycode) Then Exit Sub
End Sub


Sub Table1_KeyUp(ByVal keycode)

    'If keycode = plungerkey then plunger.Fire:PlaySoundAt "fx_Plunger", plunger

	If keycode = PlungerKey Then 
		Plunger.Fire
		If bibl Then						 	'If true then ball in shooter lane, else no ball is shooter lane
			SoundPlungerPullStop()
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerPullStop()
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
	End If

'     If Keycode = RightFlipperKey then SolRFlipper 0
'     If Keycode = LeftFlipperKey then SolLFlipper 0
	If keycode = LeftFlipperKey Then
		if bFlipsEnabled then
'			FlipperDeActivate LeftFlipper, LFPress
			if bProc then SolLFlipper 0
		end if
		bLeftFlipButton = false
		Controller.Switch(12) = 0
	End If

	If keycode = RightFlipperKey Then
		if bFlipsEnabled then
'			FlipperDeActivate RightFlipper, RFPress
			if bProc then SolRFlipper 0
		end if
		bRightFlipButton = false
		Controller.Switch(11) = 0
	End If

	If VRRoom = 1 or VRRoom = 2 Then
		If keycode = LeftFlipperKey Then
			VR_FB_Left.x = VR_FB_Left.x - 5
		End If
		If keycode = RightFlipperKey Then
			VR_FB_Right.x = VR_FB_Right.x + 5
		End If
		If keycode = StartGameKey Then
			VR_StartButton.y = VR_StartButton.y + 2
		End If
		If keycode = PlungerKey Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
		End If
		If VRRoom = 1 Then
			If keycode = leftmagnasave Then
				VRFlyerPrim.image = "Flyer1"
			End If
		End If
		If VRRoom = 1 Then
			If keycode = rightmagnasave Then
				VRFlyerPrim.image = "Flyer2"
			End If
		End If			
	End If

'    '************************   Start Ball Control 2/3
'    if keycode = 203 then bcleft = 0        ' Left Arrow
'    if keycode = 200 then bcup = 0          ' Up Arrow
'    if keycode = 208 then bcdown = 0        ' Down Arrow
'    if keycode = 205 then bcright = 0       ' Right Arrow
'    '************************   End Ball Control 2/3
     If vpmKeyUp(keycode) Then Exit Sub
End sub


 '***Solenoids***
SolCallback(1) = "kisort"
SolCallback(2) = "KickBallToLane"
SolCallback(3) = "SolKickout"
'SolCallback(4) = "vpmSolGate Gate3,0,"
SolCallback(4) = "SolGate3"
SolCallback(5) = "Solss"
SolCallback(6) = "solBallLockPost"
SolCallback(7) = "KnockerSolenoid"
SolCallback(8) = "KickMouth"
SolCallback(15) = "KickLeftEye"
SolCallback(16) = "KickRightEye"


sub SolGate3(Enabled)

	vpmSolGate Gate3,0,Enabled
	if Enabled Then
		RandomSoundGateDivert
	Else
		RandomSoundGateBack
	end if

end sub



if Not bPROC And UsePinmameModulatedSolenoids then

'	msgbox "modulated"
	SolModCallBack(17) = "ModLampz.SetModLamp 17, " 'Billions
	SolModCallBack(18) = "ModFlashLR" 				'Left ramp				Backglass Left SpaceShip
	SolModCallBack(19) = "ModFlashJP" 				'jackpot				Backglass Jackpot	
	SolModCallBack(20) = "ModFlashSK"				'SkillShot				Backglass Heart


	SolModCallBack(21) = "SetRedDome1" 				'Left Helmet Dome		Backglass Face
	SolModCallBack(22) = "SetRedDome2"				'Right Helmet Dome		Backglass Bottom Right
	SolModCallBack(23) = "SetRedDome4"				'Jets Enter Dome		Backglass Title left
	SolModCallBack(24) = "SetRedDome3"  			'Left Loop Dome			Backglass Title right

Else
'	msgbox "normal"
	SolCallback(17) = "SetLamp 117, " 	'Billions
	SolCallback(18) = "SolFlashLR" 		'Left ramp
	SolCallback(19) = "SetLamp 119, "	'jackpot
	SolCallback(20) = "SolFlashSK"		'SkillShot

	SolCallback(21) = "SetProcRedDome1" 	'Left Helmet Dome
	SolCallback(22) = "SetProcRedDome2"	'Right Helmet Dome
	SolCallback(23) = "SetProcRedDome4"	'Jets Enter Dome
	SolCallback(24) = "SetProcRedDome3"  'Left Loop Dome
end If

if bPROC then
	SolCallback(27) = "motor_dir"
	SolCallback(28) = "motor_sol"
	SolCallback(40) = "FlipperRelay"
end if

'''**************************************************
'''			Mod/SolFlashers (iaakki)
'''**************************************************

'Left ramp flasher
dim FlashTargetLevel18, FlashLevel18

Sub SolFlashLR(enabled)
	if Enabled then
		FlashTargetLevel18 = 1
	Else
		FlashTargetLevel18 = 0
	end if
	F118_SL_Timer
End Sub

Sub ModFlashLR(aLvl)
	FlashTargetLevel18 = aLvl/255
	F118_SL_Timer
End Sub

F118_SL.visible=0
F118_SL_CM.visible=0
f118.IntensityScale = 0

sub F118_SL_Timer()
	If not F118_SL.TimerEnabled Then 
		F118_SL.TimerEnabled = True
		if CabinetMode = 0 then 
			F118_SL.visible=1
		Else
			F118_SL_CM.visible = 1
		end If
		If VRRoom > 0 Then
			VRBGFL18_1.visible = true
			VRBGFL18_2.visible = true
			VRBGFL18_3.visible = true
			VRBGFL18_4.visible = true
		End If
	End If

	if CabinetMode = 0 then 
		F118_SL.opacity = 100 * FlashLevel18^2 
	Else
		F118_SL_CM.opacity = 100 * FlashLevel18^2 
	end if
	f118.IntensityScale = 0.5 * FlashLevel18
	If VRRoom > 0 Then
		VRBGFL18_1.opacity = 100 * FlashLevel18^2
		VRBGFL18_2.opacity = 100 * FlashLevel18^2
		VRBGFL18_3.opacity = 100 * FlashLevel18^2
		VRBGFL18_4.opacity = 100 * FlashLevel18^3
	End If
	
	if round(FlashTargetLevel18,1) > round(FlashLevel18,1) Then
		FlashLevel18 = FlashLevel18 + 0.3
		if FlashLevel18 > 1 then FlashLevel18 = 1
	Elseif round(FlashTargetLevel18,1) < round(FlashLevel18,1) Then
		FlashLevel18 = FlashLevel18 * 0.9 - 0.01
		if FlashLevel18 < 0 then FlashLevel18 = 0
	Else
		FlashLevel18 = round(FlashTargetLevel18,1)
'		debug.print "stop timer"
		F118_SL.TimerEnabled = False
	end if

	If FlashLevel18 <= 0 Then
		F118_SL.TimerEnabled = False
		F120_pf.visible = 0
		if CabinetMode = 0 then 
			F118_SL.visible=0
		Else
			F118_SL_CM.visible=0
		end if
		f118.IntensityScale = 0
		If VRRoom > 0 Then
			VRBGFL18_1.visible = False
			VRBGFL18_2.visible = False
			VRBGFL18_3.visible = False
			VRBGFL18_4.visible = False
		End If
	End If
end sub

dim FlashTargetLevel19, FlashLevel19

Sub SolFlashJP(enabled)
	if Enabled then
		FlashTargetLevel19 = 1
	Else
		FlashTargetLevel19 = 0
	end if
	F119_Timer
End Sub

Sub ModFlashJP(aLvl)
	FlashTargetLevel19 = aLvl/255
	F119_Timer
End Sub

f119.IntensityScale = 0
LB119.IntensityScale = 0

sub F119_Timer()
	If not f119.TimerEnabled Then 
		f119.TimerEnabled = True
		f119.visible=1
		If VRRoom > 0 Then
			VRBGFL19_1.visible = true
			VRBGFL19_2.visible = true
			VRBGFL19_3.visible = true
			VRBGFL19_4.visible = true
		End If
	End If

	f119.IntensityScale = 0.5 * FlashLevel19
	LB119.IntensityScale = 0.5 * FlashLevel19
	p119on.blenddisablelighting = 10 * FlashLevel19
	If VRRoom > 0 Then
		VRBGFL19_1.opacity = 100 * FlashLevel19^2
		VRBGFL19_2.opacity = 100 * FlashLevel19^2
		VRBGFL19_3.opacity = 100 * FlashLevel19^2
		VRBGFL19_4.opacity = 100 * FlashLevel19^3
	End If
	
	if round(FlashTargetLevel19,1) > round(FlashLevel19,1) Then
		FlashLevel19 = FlashLevel19 + 0.3
		if FlashLevel19 > 1 then FlashLevel19 = 1
	Elseif round(FlashTargetLevel19,1) < round(FlashLevel19,1) Then
		FlashLevel19 = FlashLevel19 * 0.85 - 0.01
		if FlashLevel19 < 0 then FlashLevel19 = 0
	Else
		FlashLevel19 = round(FlashTargetLevel19,1)
'		debug.print "stop timer"
		F119.TimerEnabled = False
	end if

	If FlashLevel19 <= 0 Then
		F119.TimerEnabled = False
		f119.IntensityScale = 0
		LB119.IntensityScale = 0
		If VRRoom > 0 Then
			VRBGFL19_1.visible = False
			VRBGFL19_2.visible = False
			VRBGFL19_3.visible = False
			VRBGFL19_4.visible = False
		End If
	End If
end sub


'SK flasher
dim FlashTargetLevel20, FlashLevel20

Sub SolFlashSK(enabled)
	if Enabled then
		FlashTargetLevel20 = 1
	Else
		FlashTargetLevel20 = 0
	end if
	F120_SR_Timer
End Sub

Sub ModFlashSK(aLvl)
	FlashTargetLevel20 = aLvl/255
	F120_SR_Timer
End Sub

F120_pf.visible = 0
F120_SR.visible=0
F120_SR_CM.visible=0
f120.IntensityScale = 0

sub F120_SR_Timer()
	If not F120_SR.TimerEnabled Then 
		F120_SR.TimerEnabled = True
		F120_pf.visible = 1
		if CabinetMode = 0 then
			F120_SR.visible=1
		Else
			F120_SR_CM.visible=1
		end if
		If VRRoom > 0 Then
			VRBGFL20_1.visible = 1
			VRBGFL20_2.visible = 1
		End If
	End If

	F120_pf.opacity = 80 * FlashLevel20^3
	if CabinetMode = 0 then
		F120_SR.opacity = 80 * FlashLevel20^2 
	Else
		F120_SR_CM.opacity = 80 * FlashLevel20^2 
	end if
	f120.IntensityScale = 0.5 * FlashLevel20
	If VRRoom > 0 Then
		VRBGFL20_1.opacity = 100 * FlashLevel20
		VRBGFL20_2.opacity = 75 * FlashLevel20^2
	End If

	if round(FlashTargetLevel20,1) > round(FlashLevel20,1) Then
		FlashLevel20 = FlashLevel20 + 0.3
		if FlashLevel20 > 1 then FlashLevel20 = 1
	Elseif round(FlashTargetLevel20,1) < round(FlashLevel20,1) Then
		FlashLevel20 = FlashLevel20 * 0.9 - 0.01
		if FlashLevel20 < 0 then FlashLevel20 = 0
	Else
		FlashLevel20 = round(FlashTargetLevel20,1)
'		debug.print "stop timer"
		F120_SR.TimerEnabled = False
	end if

	If FlashLevel20 <= 0 Then
		F120_SR.TimerEnabled = False
		F120_pf.visible = 0
		if CabinetMode = 0 then
			F120_SR.visible=0
		Else
			F120_SR_CM.visible=0
		end if
		f120.IntensityScale = 0
		If VRRoom > 0 Then
			VRBGFL20_1.visible = 0
			VRBGFL20_2.visible = 0
		End If
	End If
end sub


'''**************************************************
'''			P-ROC Flipper Relay (iaakki)
'''**************************************************

dim bFlipsEnabled, bLeftFlipButton, bRightFlipButton 

if bPROC Then
	bFlipsEnabled = False
Else
	bFlipsEnabled = True
end if

sub FlipperRelay(enabled)
'	msgbox "flippersSol40: " & enabled
	if Enabled Then
		bFlipsEnabled = true
		if bLeftFlipButton Then
			FlipperActivate LeftFlipper, LFPress
			SolLFlipper 1
		end If
		if bRightFlipButton Then
			FlipperActivate RightFlipper, RFPress
			SolRFlipper 1
		end If
	Else
		bFlipsEnabled = false	'and drop flips right away
		FlipperDeActivate RightFlipper, RFPress
		SolRFlipper 0
		FlipperDeActivate LeftFlipper, LFPress
		SolLFlipper 0
	end if
end sub



'''**************************************************
'''			P-ROC Head motor (iaakki)
'''**************************************************

dim motorDir : motorDir = -2

sub motor_dir(value)
	If value then
'		debug.print "motor dir left"
		motorDir = -2
	Else
'		debug.print "motor dir right"
		motorDir = 2
	end if
end Sub



'vpmtimer.addtimer 500, "motor_sol true '":	vpmtimer.addtimer 2000, "motor_sol false '"
'vpmtimer.addtimer 500, "motor_sol true '":	vpmtimer.addtimer 4000, "motor_sol false '"
'vpmtimer.addtimer 1000, "motor_sol true '":	vpmtimer.addtimer 4200, "motor_sol false '"
'motordir=2:vpmtimer.addtimer 1000, "motor_sol true '":	vpmtimer.addtimer 4200, "motor_sol false '"

HeadMechTmr.interval = 18


dim delayHeadStop:delayHeadStop = false
sub motor_sol(value)
'	debug.print "   motor sol2: " & value
	if value Then
		HeadMechTmr.enabled = true
'		debug.print "   motor drive ON. CurrentFace: " & currentFace
		
	Else
'		debug.print "   motor drive OFF. CurrentFace: " & currentFace
		if currentFace <> 0 then
			delayHeadStop = false
			HeadMechTmr.enabled = false
			if currentFace = 1 then headCurrAngle = 84
			if currentFace = 2 then headCurrAngle = 446
			if currentFace = 3 then headCurrAngle = 804
			if currentFace = 4 then headCurrAngle = 1152
			HeadMechCallback headCurrAngle, 0, headCurrAngle
		Else
			delayHeadStop = true
		end if
'		stopsound "head_motor"
	End if
end Sub

dim headCurrAngle, headPrevAngle
headCurrAngle = 84

sub HeadMechTmr_timer
	headPrevAngle = headCurrAngle
	headCurrAngle = headCurrAngle + motorDir

	if headCurrAngle > 1436 then headCurrAngle = 0
	if headCurrAngle < 0 then headCurrAngle = 1436

'	debug.print "   motor angle: " & headCurrAngle

	if delayHeadStop Then
		if currentFace <> 0 Then
'			debug.print "delayed head stop"
			delayHeadStop = false
			HeadMechTmr.enabled = false
			if currentFace = 1 then headCurrAngle = 84
			if currentFace = 2 then headCurrAngle = 446
			if currentFace = 3 then headCurrAngle = 804
			if currentFace = 4 then headCurrAngle = 1152
			HeadMechCallback headCurrAngle, 0, headCurrAngle
		end if
	end if
	HeadMechCallback headCurrAngle, 1, headPrevAngle
end sub


'''**************************************************
'''			Flashers
'''**************************************************

Sub Flash17(value)
	SetLamp 117, value
End Sub


Sub SetRedDome1(flLvl)
'	debug.print "1:" & flLvl

	ObjTargetLevel(1) = flLvl / 255
	FlasherFlash1_Timer
	FlasherSolenoidSound 1
End Sub

Sub SetRedDome2(flLvl)
'	debug.print "2:" & flLvl
	ObjTargetLevel(2) = round((flLvl / 255),2)
	FlasherFlash2_Timer
	FlasherSolenoidSound 2
End Sub

Sub SetRedDome3(flLvl)
'	debug.print "3:" & flLvl
	ObjTargetLevel(3) = round((flLvl / 255),2)
	FlasherFlash3_Timer
	FlasherSolenoidSound 3
End Sub

Sub SetRedDome4(flLvl)
'	debug.print "4:" & flLvl
	ObjTargetLevel(4) = round((flLvl / 255),2)
	FlasherFlash4_Timer
	FlasherSolenoidSound 4
End Sub


Sub SetProcRedDome1(enabled)
'	debug.print "1:" & flLvl
	if enabled then
		ObjTargetLevel(1) = 1
'		Call Sound_Flasher_Relay(SoundOn, FlasherPosition)
	else
		ObjTargetLevel(1) = 0
'		Call Sound_Flasher_Relay(SoundOff, FlasherPosition)
	end if
	FlasherFlash1_Timer
End Sub

Sub SetProcRedDome2(enabled)
'	debug.print "2:" & flLvl
	if enabled then
		ObjTargetLevel(2) = 1
'		Call Sound_Flasher_Relay(SoundOn, FlasherPosition)
	else
		ObjTargetLevel(2) = 0
'		Call Sound_Flasher_Relay(SoundOff, FlasherPosition)
	end if
	FlasherFlash2_Timer
End Sub

Sub SetProcRedDome3(enabled)
'	debug.print "3:" & flLvl
	if enabled then
		ObjTargetLevel(3) = 1
'		Call Sound_Flasher_Relay(SoundOn, FlasherPosition)
	else
		ObjTargetLevel(3) = 0
'		Call Sound_Flasher_Relay(SoundOff, FlasherPosition)
	end if
	FlasherFlash3_Timer
End Sub

Sub SetProcRedDome4(enabled)
'	debug.print "4:" & flLvl
	if enabled then
		ObjTargetLevel(4) = 1
'		Call Sound_Flasher_Relay(SoundOn, FlasherPosition)
	else
		ObjTargetLevel(4) = 0
'		Call Sound_Flasher_Relay(SoundOff, FlasherPosition)
	end if
	FlasherFlash4_Timer
End Sub


dim FLRelay(10)
sub FlasherSolenoidSound(nr)
	' No flasher relay sounds for this table
'	if ObjTargetLevel(nr) > 0.4 and Not FLRelay(nr) Then
'		Call Sound_Flasher_Relay(SoundOn, FlasherPosition)
''		debug.print nr & " - ON"
'		FLRelay(nr) = true
'	end if
'	if ObjTargetLevel(nr) < 0.4 and FLRelay(nr) Then
'		Call Sound_Flasher_Relay(SoundOff, FlasherPosition)
''		debug.print nr & " - OFF"
'		FLRelay(nr) = false
'	end if
end sub




Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.03	' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.05		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objactive(20), ObjTargetLevel(20), objfade(20)  'flasher numbers should fall within the 0 - 20 range.
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height

'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"


dim bakedFlasherColor

if DomeColor = 6 then DomeColor = RndInt(0,5)


if DomeColor <> 0 then
	'if not default red, made the script to swap dome colors
	Flasherbase1.image = "domeearbasewhite"
	Flasherbase2.image = "domeearbasewhite"
	Flasherbase3.image = "domeearbasewhite"
	Flasherbase4.image = "domeearbasewhite"
end if


select case DomeColor
	case 0:
		'default red
		bakedFlasherColor = rgb(255,20,2)
		InitFlasher 1, "red"
		InitFlasher 2, "red"
		InitFlasher 3, "red"
		InitFlasher 4, "red"
	case 1:
		'green
		bakedFlasherColor = rgb(20,255,2)
		InitFlasher 1, "green"
		InitFlasher 2, "green"
		InitFlasher 3, "green"
		InitFlasher 4, "green"
	case 2:
		'blue
		bakedFlasherColor = rgb(20,2,255)
		InitFlasher 1, "blue"
		InitFlasher 2, "blue"
		InitFlasher 3, "blue"
		InitFlasher 4, "blue"
	case 3:
		'Yellow
		bakedFlasherColor = RGB(255,200,50)
		InitFlasher 1, "yellow"
		InitFlasher 2, "yellow"
		InitFlasher 3, "yellow"
		InitFlasher 4, "yellow"
	case 4:
		'purple
		bakedFlasherColor = RGB(255,64,255)
		InitFlasher 1, "purple"
		InitFlasher 2, "purple"
		InitFlasher 3, "purple"
		InitFlasher 4, "purple"
	case 5:
		'white
		bakedFlasherColor = rgb(255,210,163)
		InitFlasher 1, "white"
		InitFlasher 2, "white"
		InitFlasher 3, "white"
		InitFlasher 4, "white"
end Select

if domecolor <> 0 Then
	F121Blooma.color = bakedFlasherColor
	F121Blooma.colorfull = bakedFlasherColor
	F122Blooma.color = bakedFlasherColor
	F122Blooma.colorfull = bakedFlasherColor
	F123Blooma.color = bakedFlasherColor
	F123Blooma.colorfull = bakedFlasherColor
	F124Blooma.color = bakedFlasherColor
	F124Blooma.colorfull = bakedFlasherColor
	FlaBloom.color = bakedFlasherColor
end if



' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr)
	Set objfade(nr) = Eval("Flasherfade" & nr)

	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlit(nr).visible = 0 : objfade(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr

	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z

	objfade(nr).RotX = objbase(nr).RotX : objfade(nr).RotY = objbase(nr).RotY : objfade(nr).RotZ = objbase(nr).RotZ
	objfade(nr).ObjRotX = objbase(nr).ObjRotX : objfade(nr).ObjRotY = objbase(nr).ObjRotY : objfade(nr).ObjRotZ = objbase(nr).ObjRotZ
	objfade(nr).x = objbase(nr).x : objfade(nr).y = objbase(nr).y : objfade(nr).z = objbase(nr).z

	objbase(nr).BlendDisableLighting = FlasherOffBrightness

	if FlasherFadingPerf = 0 Then	
		objflasher(nr).timerinterval = 25
	Else
		objflasher(nr).timerinterval = 35
	end If

	' set the texture and color of all objects
	select case objbase(nr).image
'		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
'		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objfade(nr).image = "dome2litear" & col & "fade":objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col : objbase(nr).material = "domebasemat"
	end select

	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" : 	objflasher(nr).color = RGB(200,255,255)
		Case "green" : 	objflasher(nr).color = RGB(12,255,4)
		Case "red" : 	objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objflasher(nr).color = RGB(100,86,59)
	end select
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If

	Flasher_PF1.color = bakedFlasherColor
	Flasher_SL1.color = bakedFlasherColor
	Flasher_SR1.color = bakedFlasherColor
	Flasher_PF2.color = bakedFlasherColor
	Flasher_SL2.color = bakedFlasherColor
	Flasher_SR2.color = bakedFlasherColor
	Flasher_PF3.color = bakedFlasherColor
	Flasher_SL3.color = bakedFlasherColor
	Flasher_SR3.color = bakedFlasherColor
	Flasher_PF4.color = bakedFlasherColor
	Flasher_SL4.color = bakedFlasherColor
	Flasher_SR4.color = bakedFlasherColor

	Flasher_SR1_CM.color = bakedFlasherColor
	Flasher_SL1_CM.color = bakedFlasherColor
	Flasher_SR2_CM.color = bakedFlasherColor
	Flasher_SL2_CM.color = bakedFlasherColor
	Flasher_SR3_CM.color = bakedFlasherColor
	Flasher_SL3_CM.color = bakedFlasherColor
	Flasher_SR4_CM.color = bakedFlasherColor
	Flasher_SL4_CM.color = bakedFlasherColor

End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub


dim FlaTotalIntensity
FlaBloom.opacity = 0 : FlaBloom.visible = true


F121Blooma.IntensityScale = 0
F122Blooma.IntensityScale = 0
F123Blooma.IntensityScale = 0
F124Blooma.IntensityScale = 0

Flasher_SL1.visible = False
Flasher_SL2.visible = False
Flasher_SL3.visible = False
Flasher_SL4.visible = False

Flasher_SR1.visible = False
Flasher_SR2.visible = False
Flasher_SR3.visible = False
Flasher_SR4.visible = False

Flasher_SL1_CM.visible = False
Flasher_SL2_CM.visible = False
Flasher_SL3_CM.visible = False
Flasher_SL4_CM.visible = False

Flasher_SR1_CM.visible = False
Flasher_SR2_CM.visible = False
Flasher_SR3_CM.visible = False
Flasher_SR4_CM.visible = False

dim FlashingFadingSpeedDown, FlashingFadingSpeedUp
if FlasherFadingPerf = 0 Then
	FlashingFadingSpeedDown = 0.9
	FlashingFadingSpeedUp = 0.34
	
Else
	FlashingFadingSpeedDown = 0.75
	FlashingFadingSpeedUp = 0.5
end if


Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then 
		objflasher(nr).TimerEnabled = True
		objflasher(nr).visible = 1 : objlit(nr).visible = 1 : objfade(nr).visible = 1
		select case nr
			case 1: 
				Flasher_PF1.visible = true
				if CabinetMode = 0 then 
					Flasher_SL1.visible = true
					Flasher_SR1.visible = true
				else
					Flasher_SL1_CM.visible = true
					Flasher_SR1_CM.visible = true
				end if
				If VRRoom > 0 Then
					VRBGFL21_1.visible = True
					VRBGFL21_2.visible = True
					VRBGFL21_3.visible = True
					VRBGFL21_4.visible = True
				End If
			case 2: 
				Flasher_PF2.visible = true
				if CabinetMode = 0 then 
					Flasher_SL2.visible = true
					Flasher_SR2.visible = true
				else
					Flasher_SL2_CM.visible = true
					Flasher_SR2_CM.visible = true
				end if
				If VRRoom > 0 Then
					VRBGFL22_1.visible = True
					VRBGFL22_2.visible = True
					VRBGFL22_3.visible = True
					VRBGFL22_4.visible = True
				End If
			case 3: 
				Flasher_PF3.visible = true
				if CabinetMode = 0 then 
					Flasher_SL3.visible = true
					Flasher_SR3.visible = true
				else
					Flasher_SL3_CM.visible = true
					Flasher_SR3_CM.visible = true
				end if
				If VRRoom > 0 Then
					VRBGFL24_1.visible = True
					VRBGFL24_2.visible = True
					VRBGFL24_3.visible = True
					VRBGFL24_4.visible = True
				End If
			case 4: 
				Flasher_PF4.visible = true
				if CabinetMode = 0 then 
					Flasher_SL4.visible = true
					Flasher_SR4.visible = true
				else
					Flasher_SL4_CM.visible = true
					Flasher_SR4_CM.visible = true
				end if
				If VRRoom > 0 Then
					VRBGFL23_1.visible = True
					VRBGFL23_2.visible = True
					VRBGFL23_3.visible = True
					VRBGFL23_4.visible = True
				End If
		end select
	End If

	FlaTotalIntensity = (ObjLevel(1) + ObjLevel(2) + ObjLevel(3) + ObjLevel(4)) / 4

'	debug.print round(FlaTotalIntensity,2)

	FlaBloom.opacity = 55 * FlaTotalIntensity

	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 25 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 15 * ObjLevel(nr)^2
	objfade(nr).BlendDisableLighting = 100 * ObjLevel(nr)^0.6
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 

	select case nr
		case 1 : 
'			debug.print nr & " -> " & ObjLevel(nr)
			Flasher_pf1.opacity = 600 * ObjLevel(nr)
			if CabinetMode = 0 then 
				Flasher_SL1.opacity = 300 * ObjLevel(nr)
				Flasher_SR1.opacity = 300 * ObjLevel(nr)
			Else
				Flasher_SL1_CM.opacity = 300 * ObjLevel(nr)
				Flasher_SR1_CM.opacity = 300 * ObjLevel(nr)
			END IF
			F121Blooma.IntensityScale = 1 * ObjLevel(nr)
			If VRRoom > 0 Then
				VRBGFL21_1.opacity = 100 * ObjLevel(nr)^2
				VRBGFL21_2.opacity = 100 * ObjLevel(nr)^2
				VRBGFL21_3.opacity = 100 * ObjLevel(nr)^2
				VRBGFL21_4.opacity = 100 * ObjLevel(nr)^3
			End If
		case 2 : 
'			debug.print nr & " -> " & ObjLevel(nr)
			Flasher_pf2.opacity = 600 * ObjLevel(nr)
			if CabinetMode = 0 then 
				Flasher_SL2.opacity = 300 * ObjLevel(nr)
				Flasher_SR2.opacity = 300 * ObjLevel(nr)
			Else
				Flasher_SL2_CM.opacity = 300 * ObjLevel(nr)
				Flasher_SR2_CM.opacity = 300 * ObjLevel(nr)
			END IF
			F122Blooma.IntensityScale = 1 * ObjLevel(nr)
			If VRRoom > 0 Then
				VRBGFL22_1.opacity = 100 * ObjLevel(nr)^2
				VRBGFL22_2.opacity = 100 * ObjLevel(nr)^2
				VRBGFL22_3.opacity = 100 * ObjLevel(nr)^2
				VRBGFL22_4.opacity = 100 * ObjLevel(nr)^3
			End If
		case 3 : 
'			debug.print nr & " -> " & ObjLevel(nr)
			Flasher_pf3.opacity = 200 * ObjLevel(nr)
			if CabinetMode = 0 then 
				Flasher_SL3.opacity = 300 * ObjLevel(nr)
				Flasher_SR3.opacity = 300 * ObjLevel(nr)
			Else
				Flasher_SL3_CM.opacity = 300 * ObjLevel(nr)
				Flasher_SR3_CM.opacity = 300 * ObjLevel(nr)
			END IF
			F123Blooma.IntensityScale = 1 * ObjLevel(nr)
'			F123Bloom.opacity = 35 * ObjLevel(nr)
			If VRRoom > 0 Then
				VRBGFL24_1.opacity = 100 * ObjLevel(nr)^2
				VRBGFL24_2.opacity = 100 * ObjLevel(nr)^2
				VRBGFL24_3.opacity = 100 * ObjLevel(nr)^2
				VRBGFL24_4.opacity = 100 * ObjLevel(nr)^3
			End If
		case 4 : 
'			debug.print nr & " -> " & ObjLevel(nr)
			Flasher_pf4.opacity = 200 * ObjLevel(nr)
			if CabinetMode = 0 then 
				Flasher_SL4.opacity = 300 * ObjLevel(nr)
				Flasher_SR4.opacity = 300 * ObjLevel(nr)
			Else
				Flasher_SL4_CM.opacity = 300 * ObjLevel(nr)
				Flasher_SR4_CM.opacity = 300 * ObjLevel(nr)
			END IF
			F124Blooma.IntensityScale = 1 * ObjLevel(nr)
'			F124Bloom.opacity = 35 * ObjLevel(nr)
			If VRRoom > 0 Then
				VRBGFL23_1.opacity = 100 * ObjLevel(nr)^2
				VRBGFL23_2.opacity = 100 * ObjLevel(nr)^2
				VRBGFL23_3.opacity = 100 * ObjLevel(nr)^2
				VRBGFL23_4.opacity = 100 * ObjLevel(nr)^3
			End If
	end select

'	if nr = 2 then debug.print nr & " -> " & ObjLevel(nr)

	if round(ObjTargetLevel(nr),1) > round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) + FlashingFadingSpeedUp
		if ObjLevel(nr) > 1 then ObjLevel(nr) = 1
	Elseif round(ObjTargetLevel(nr),1) < round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) * FlashingFadingSpeedDown - 0.01
		if ObjLevel(nr) < 0 then ObjLevel(nr) = 0
	Else
		ObjLevel(nr) = round(ObjTargetLevel(nr),1)
'		debug.print "stop timer"
		objflasher(nr).TimerEnabled = False
	end if
	'ObjLevel(nr) = ObjLevel(nr) * 0.7 - 0.01



	If ObjLevel(nr) <= 0 Then 
		objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : objfade(nr).visible = 0
		select case nr
			case 1: 
				Flasher_PF1.visible = false
				if CabinetMode = 0 then 
					Flasher_SL1.visible = false
					Flasher_SR1.visible = false
				Else
					Flasher_SL1_CM.visible = false
					Flasher_SR1_CM.visible = false
				END If
				F121Blooma.IntensityScale = 0
				If VRRoom > 0 Then
					VRBGFL21_1.visible = False
					VRBGFL21_2.visible = False
					VRBGFL21_3.visible = False
					VRBGFL21_4.visible = False
				End If
			case 2: 
				Flasher_PF2.visible = false
				if CabinetMode = 0 then 
					Flasher_SL2.visible = false
					Flasher_SR2.visible = false
				Else
					Flasher_SL2_CM.visible = false
					Flasher_SR2_CM.visible = false
				END If
				F122Blooma.IntensityScale = 0
				If VRRoom > 0 Then
					VRBGFL22_1.visible = False
					VRBGFL22_2.visible = False
					VRBGFL22_3.visible = False
					VRBGFL22_4.visible = False
				End If
			case 3: 
				Flasher_PF3.visible = false
				if CabinetMode = 0 then 
					Flasher_SL3.visible = false
					Flasher_SR3.visible = false
				Else
					Flasher_SL3_CM.visible = false
					Flasher_SR3_CM.visible = false
				END If
				F123Blooma.IntensityScale = 0
				If VRRoom > 0 Then
					VRBGFL24_1.visible = False
					VRBGFL24_2.visible = False
					VRBGFL24_3.visible = False
					VRBGFL24_4.visible = False
				End If
			case 4: 
				Flasher_PF4.visible = false
				if CabinetMode = 0 then 
					Flasher_SL4.visible = false
					Flasher_SR4.visible = false
				Else
					Flasher_SL4_CM.visible = false
					Flasher_SR4_CM.visible = false
				END If
				F124Blooma.IntensityScale = 0
				If VRRoom > 0 Then
					VRBGFL23_1.visible = False
					VRBGFL23_2.visible = False
					VRBGFL23_3.visible = False
					VRBGFL23_4.visible = False
				End If
		end select
	end if
	
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub

Sub Flash121(value)
	SetLamp 121, value
End Sub

Sub Flash122(value)
	SetLamp 122, value
End Sub

Sub Flash123(value)
	SetLamp 123, value
End Sub

Sub Flash124(value)
	SetLamp 124, value
End Sub


Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub

Sub SolKickout(enabled)
    If Enabled then
        sw46k.kick 22.5,70   '70 = power
'        sw46.enabled= 0
'        vpmtimer.addtimer 600, "sw46.enabled= 1'"
        'PlaysoundAt SoundFx("fx_KickerRelease",DOFContactors), sw46k
		RandomSoundEjectHoleSolenoid()

    End If
End Sub

sub BallScoopEnter_hit
'	activeball.vely = activeball.vely / 4
'	debug.print "VELy: " & activeball.vely
	if activeball.vely < 0 then
'		debug.print "going down: " & activeball.vely
		RandomSoundEjectHoleEnter()
	Else
'		debug.print "going up: " & activeball.vely
		RandomSoundEjectBallBump()
	end if
end sub

Sub sw46k_Hit()
    'PlaysoundAt "fx_ScoopEnter", sw46k
'	RandomSoundEjectHoleEnter()
    Controller.Switch(46) = 1
End Sub

Sub sw46k_UnHit()
    Controller.Switch(46) = 0
End Sub


Sub SolMotor(Enabled)
    Light1.State = enabled
End Sub

Sub SolRelay(Enabled)
    Light2.State = enabled
End Sub


 '**************
 ' Solenoid Subs
 '**************


Sub SolSS(enabled)
	If Enabled then
		plungerIM.AutoFire:SSKick.RotX = 120:EMPos = 120:SSLaunch1.Enabled = True
	end if
End Sub


dim BLdir
Sub solBallLockPost(enabled)
    If Enabled then 
		BL.IsDropped=1	'drop fast
		'BLP.transY = -50
		BLdir = -1
		BL.Timerinterval = 20
		BL.TimerEnabled = 1
		'PlaySound SoundFX("fx_Solenoid",DOFContactors)
		RandomSoundRampDiverterDivert()
		RandomSoundRampDiverterHold(SoundOn)
	Else
		'this needs some delay on proc
'		if Not bPROC then BL.IsDropped=0
		'BLP.transY = 0
		BLdir = 1
		if bPROC then
			BL.Timerinterval = 30			'higher value as proc had much faster timing
		else
			BL.Timerinterval = 10 '15 '25
		end if
		BL.TimerEnabled = 1
		RandomSoundRampDiverterBack()
		RandomSoundRampDiverterHold(SoundOff)
		if motorSoundStarted = 1 then
			stopsound "head_motor"
'			debug.print "stopsound on force"
			motorSoundStarted = 0
		end if
	end if
End Sub

Sub BL_Timer()
	if BLdir = -1 Then
		if BLP.transY = -50 Then
			BL.TimerEnabled = 0
		Else
			BLP.transY = BLP.transY - 10
		end if
	Else
		if BLP.transY = 0 Then
'			if bPROC then BL.IsDropped=0			'need to test can this be done same for proc and mame
			BL.IsDropped=0					'yes it can with 10ms timer and slippery surface
			BL.TimerEnabled = 0
		Else
'			if Not bPROC then BL.IsDropped=0
			BLP.transY = BLP.transY + 10
		end if
	end if
End Sub


Dim EMPos
EMPos = 0
Sub SSLaunch1_timer()
	EmPos = EmPos - 10
	SSKick.transZ = EmPos
	If EmPos < 0 then SSLaunch1.Enabled = 0
End Sub



'**************
'ConstantUpdates
'**************


Dim GateSpeed
GateSpeed = 0.5

'SPinner Brake
sub SpinnerBrake_Hit
	dim amount: amount = .5
	ActiveBall.velx = ActiveBall.velx * amount
	ActiveBall.vely = ActiveBall.vely * amount
end sub

Dim Gate2Open,Gate2Angle:Gate2Open=0:Gate2Angle=0
Sub Gate2_Hit()
	Gate2Open=1:Gate2Angle=0
	SoundBallGate2()
End Sub

Dim Gate4Open,Gate4Angle:Gate4Open=0:Gate4Angle=0
Sub Gate4_Hit():Gate4Open=1:Gate4Angle=0:End Sub

Dim Gate5Open,Gate5Angle:Gate5Open=0:Gate5Angle=0
Sub Gate5_Hit()
	Gate5Open=1:Gate5Angle=0
End Sub

sub left_gate_hit
	If ActiveBall.VelX > 0 Then SoundBallGate5(SoundOn) : Else SoundBallGate5(SoundOff)
end Sub

Dim Gate6Open,Gate6Angle:Gate6Open=0:Gate6Angle=0
Sub Gate6_Hit():Gate6Open=1:Gate6Angle=0:End Sub

Dim Gate8Open,Gate8Angle:Gate8Open=0:Gate8Angle=0
Sub Gate8_Hit()
	Gate8Open=1:Gate8Angle=0

End Sub

sub tSmallGate_hit
	If ActiveBall.VelY < 0 Then SoundBallGate8(SoundOn) : Else SoundBallGate8(SoundOff)
end sub


Sub right_gate_Hit():if gate3.currentangle < 90 then:SoundBallGate3():end if:End Sub

'**************************************
'Spinner
'**************************************
Sub sw51_Spin:vpmTimer.PulseSw 51:SoundSpinner():End Sub

Sub UpdateGatesSpinners
    SpinnerT1.RotX = -(sw51.currentangle)
	'90 -> 6, 0 -> 0, 180 -> 0, 270 -> 6
	SpinnerTShadow.size_y = abs(sin( (sw51.CurrentAngle+180) * (2*PI/360)) * 6)

'	debug.print sw51.currentangle & " -> " & SpinnerTShadow.size_y

    pSpinnerRod.TransX = sin( (sw51.CurrentAngle+180) * (2*PI/360)) * 6
    pSpinnerRod.TransY = sin( (sw51.CurrentAngle- 90) * (2*PI/360)) * 6

    If Gate3.currentangle > 70 Then
        Gate3P.RotZ = -90
    Else
        Gate3P.RotZ = -(Gate3.currentangle+40)
    End If

	Gate0P.Rotx = Gate0.currentangle
	Gate1P.Rotx = 90 + Gate1.currentangle
		Gate2P.Rotz = 90 - Gate2.currentangle + 270 - 20	'todo
	Gate4P.Roty = 0 - Gate4.currentangle
	Gate5P.Rotz = -Gate5.currentangle + 35
		Gate6P.RotX = 0 - Gate6.currentangle				'todo
	Gate8P.Rotz = 90 + Gate8.currentangle + 270

    pFaceDiverter.RotX = div.currentangle / 2
End Sub


'***********************************************
'**************
' Flipper Subs
'**************

if Not bPROC then
	SolCallback(sLRFlipper) = "SolRFlipper"
	SolCallback(sLLFlipper) = "SolLFlipper"
end if

Const ReflipAngle = 20
Const QuickFlipAngle = 20


Sub SolLFlipper(Enabled)
'	debug.print "left flipper"
     If Enabled Then
         'PlaySoundAtVol SoundFx("fx_FlipperupLeft",DOFFlippers), LeftFlipper, VolFlippers
         LF.Fire 'LeftFlipper.RotateToEnd
		FlipperActivate LeftFlipper, LFPress
		 Controller.Switch(12) = 1

		If LeftFlipper.currentangle < LeftFlipper.endangle + ReflipAngle Then 
			'Play partial flip sound and stop any flip down sound
			'Debug.print "Flip Reflip"
			StopAnyFlipperLowerLeftDown()
'			RandomSoundLowerLeftReflip()
			RandomSoundFlipperLowerLeftReflip LeftFlipper
		Else
			'Debug.print "LeftFlipper.currentangle = " &LeftFlipper.currentangle
			'Debug.print "LeftFlipper.startangle = " &LeftFlipper.startangle
			'Debug.print "LeftFlipper.endangle = " &LeftFlipper.endangle
			'LeftFlipper.RotateToEnd
			'Play full flip sound
			'Debug.print "Flip Up"
			If BallNearLF = 0 Then 
				RandomSoundFlipperLowerLeftUpFullStroke LeftFlipper
			End If
			If BallNearLF = 1 Then 
				Select Case Int(Rnd*2)+1
					Case 1 : RandomSoundFlipperLowerLeftUpDampenedStroke LeftFlipper
					Case 2 : RandomSoundFlipperLowerLeftUpFullStroke LeftFlipper
				End Select		
			End If
		End If
     Else
         'PlaySoundAtVol SoundFx("fx_flipperdown",DOFFlippers), LeftFlipper, VolFlippers
		LeftFlipper.RotateToStart
		FlipperDeActivate LeftFlipper, LFPress
		Controller.Switch(12) = 0

		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			'Play flip down sound
			'Debug.print "Flip Down"
			RandomSoundFlipperLowerLeftDown LeftFlipper
		End If
		If LeftFlipper.currentangle < LeftFlipper.startAngle + QuickFlipAngle and LeftFlipper.currentangle <> LeftFlipper.endangle Then
			'Play quick flip sound and stop any flip up sound
'			Debug.print "Flip Quick"
			StopAnyFlipperLowerLeftUp()
			RandomSoundLowerLeftQuickFlipUp()
		Else
			FlipperLeftLowerHitParm = FlipperUpSoundLevel
		End If
     End If
 End Sub

Sub SolRFlipper(Enabled)
'	debug.print "right flipper"
    If enabled then
        'PlaySoundAtVol SoundFx("fx_FlipperUpRight",DOFFlippers), RightFlipper, VolFlippers
		FlipperActivate RightFlipper, RFPress
        RF.Fire 'RightFlipper.RotateToEnd
		Controller.Switch(11) = 1

		If RightFlipper.currentangle > RightFlipper.endangle - ReflipAngle Then
			'Play partial flip sound and stop any flip down sound
			StopAnyFlipperLowerRightDown()
			RandomSoundFlipperLowerRightReflip RightFlipper
		Else 
			'Play full flip sound
			'RightFlipper.RotateToEnd
			If BallNearRF = 0 Then 
				RandomSoundFlipperLowerRightUpFullStroke RightFlipper
			End If
			
			If BallNearRF = 1 Then 
				Select Case Int(Rnd*2)+1
					Case 1 : RandomSoundFlipperLowerRightUpDampenedStroke RightFlipper
					Case 2 : RandomSoundFlipperLowerRightUpFullStroke RightFlipper
				End Select		
			End If
		End If
     Else
        'PlaySoundAtVol SoundFx("fx_flipperdown",DOFFlippers), RightFlipper, VolFlippers
        RightFlipper.RotateToStart
		FlipperDeActivate RightFlipper, RFPress
		Controller.Switch(11) = 0

		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			'Play flip down sound
			RandomSoundFlipperLowerRightDown RightFlipper
		End If
		If RightFlipper.currentangle < RightFlipper.startAngle + QuickFlipAngle and RightFlipper.currentangle <> RightFlipper.endangle Then
			'Play quick flip sound and stop any flip up sound
			StopAnyFlipperLowerRightUp()
			RandomSoundLowerRightQuickFlipUp()
		Else
			FlipperRightLowerHitParm = FlipperUpSoundLevel
		End If
     End If
 End Sub





'************************************************************************************************************

 '-----------------
' Head/Face rotation script
' cvpmMech-based script by RothbauerW, DJRobX
'-----------------
'
' MOTOR ASSEMBLY (notes from BoP manual)
'
' Motor has a single peg on it at the outside of a disk, which intersects
' with a plus-shaped guide on the head box.  The peg pushes into a slot on
' the guide and rotates the box 90 degrees - during that time, the disk has
' also rotated 90 degrees.  The remaining 270 degrees, the wheel just free-
' spins.  This type of motion causes the face to rotate slowest when the pin
' is entering or exiting the slot (0/90 degrees), and fastest when the peg is
' halfway through the rotation (45 degrees).  A cosine function will simulate
' this speed curve quite nicely.
'
' The mech handler will do 4 full 360-degree rotations to simulate the drive
' wheel through an entire head-box rotation.  The callback will then determine
' the head-box's current position by simulating the peg.
' ---
'
' FACE SWITCH ASSEMBLY:(notes from BoP manual)
'
' Switch 67 is the head position switch.  The switch should be closed when in
' an indentation on the head box bottom plate, and open at all other times.
' Each indent is a concave surface to allow the switch to travel smoothly, so
' we can assume that the switch is closed when at least halfway into the maximum
' depth of an indent.  There are indents on only three sides (same sides as
' Faces 1, 2 and 4) - the last side (Face 3) doesn't have an indent, so the
' switch stays open on this side.  When looking at the head top-down (on playfield),
' this switch is on the right side, which means Face 4 is facing upward when the
' switch is stuck open.  The size of the indents and an assumption about their
' concavity leads me to believe the switch should be closed for about 10 degrees
' either side of center.

headAngle = 0
prevHeadAngle = 0
currentFace = 1
Dim AngleMult, prevPos


Function headIsNear(target)
    headIsNear = (headAngle >= target - 10) AND (headAngle <= target + 10)
End Function

dim motorSoundStarted
dim motor_Soundlevel : motor_Soundlevel = 0.1

Sub HeadMechCallback(aNewPos, aSpeed, aLastPos)

'	debug.print "head: " & aNewPos

	if motorSoundStarted = 0 And tableStarted then
'		debug.print "motor start"
		PlaySoundAtLevelStaticLoop SoundFX("head_motor",DOFShaker), motor_Soundlevel, Primitive035
		motorSoundStarted = 1
'		PlaySoundAtLevelExistingStaticLoop SoundFX("head_motor",DOFShaker), motor_Soundlevel, Primitive035
	end if

	if prevPos = aNewPos Then
		stopsound "head_motor"
'		debug.print "stopsound"
		motorSoundStarted = 0
	end if
'	debug.print prevPos & " <> " & aNewPos

	if aSpeed = 0 And motorSoundStarted = 1 then
'		debug.print "speed zero"
		stopsound "head_motor"
'		debug.print "stopsound"
		motorSoundStarted = 0
	end if
	prevPos = aNewPos

	Controller.HandleMechanics = 0
    headAngle = Fix(aNewPos / 360) * 90        ' Get integer position for current face.
   Dim wheelPos : wheelPos = aNewPos - (headAngle * 4)   ' What position is the wheel in?
   ' Wheel position > 270 = head is 90 degrees further along than original base calc.
   If (wheelPos >= 270) Then
      headAngle = headAngle + 90
    ElseIf (wheelPos >= 180) Then
      ' Calculate how far along into rotation the head is.
     ' Since our goal is to get slow movement at both ends of the arc, we need a
     ' Cosine over 180 degrees (Pi radians), so this formula takes care of that.
     ' Also, Cosine varies between -1 and 1, so add 1 to the value (range 0-2) and
     ' cut it in half to get correct partial angle with correct accel curve.
     Dim wheelRads : wheelRads = ((wheelPos - 180) * 2 + 180) * 0.0174532925
      headAngle = headAngle + 45 * (1 + Cos(wheelRads))
    End If
 
    ' No more to do if the head hasn't changed position.
   If headAngle = prevHeadAngle Then
'		debug.print "exit"
        Exit Sub
    End If
 
    ' Head has moved.
   prevHeadAngle = headAngle

'	if aNewPos <= 360 Then
'		HeadAngle =  getAngle(aNewPos)
'	elseif aNewPos <= 720 Then
'		HeadAngle =  getAngle(aNewPos - 360) + 90
'	elseif aNewPos <= 1080 Then
'		HeadAngle =  getAngle(aNewPos - 720) + 180
'	elseif aNewPos <= 1440 Then
'		HeadAngle =  getAngle(aNewPos - 1080) + 270
'	end if
'
'	debug.print "Head: " & aNewPos &" " & Controller.Switch(67)&" " & HeadAngle


	' Position primitives
	Face.ObjRotY = headAngle
	Face1.ObjRotY = headAngle
	FaceGuides.ObjRotY = headAngle
	pFaceDiverter.ObjRotY = headAngle
	pFaceDiverterPegs.ObjRotY = headAngle


	' Determine which face is up, if any
	'Face1
	If headIsNear(0) OR headIsNear(360) Then
		currentFace = 1
		If HottieModType = 1 Then
		Else
			'FaceGuides.Visible = true
		End If
		if Not bPROC then DOF 102, DOFOff
	'Face2
    ElseIf headIsNear(90) Then
		currentFace = 2
		If HottieModType = 1 Then
		Else
			FaceGuides.Visible = true
		End If

		if Not bPROC then DOF 102, DOFoff
	'Face3
     ElseIf headIsNear(180) Then
		currentFace = 3 
		If HottieModType = 1 Then
		Else
			FaceGuides.Visible = true
		End If
		if Not bPROC then DOF 102, DOFOff
	'Face4
	ElseIf headIsNear(270) Then
		currentFace = 4 
		If HottieModType = 1 Then
		Else
			FaceGuides.Visible = True
		End If
		if Not bPROC then DOF 102, DOFOff
    Else
		currentFace = 0
		if Not bPROC then DOF 102, DOFOn
    End If

    ' Head box position switch
	Controller.Switch(67) = (currentFace > 0 AND currentFace < 4) ' Faces 1, 2 and 3 close the switch.


    F1Guide.IsDropped = NOT (currentFace = 1)
    F1Guide2.IsDropped = NOT (currentFace = 1)
	sw65x.Enabled = (currentFace = 1)

    ' Face 2 parts
	sw63x.Enabled = (currentFace = 2)
    sw64x.Enabled = (currentFace = 2)
    div.Enabled = (currentFace = 2)
    sw63div.Enabled = (currentFace = 2)
    sw64div.Enabled = (currentFace = 2)

	Face_Eyes.Collidable = (currentFace = 2)
	Face_Eyes.visible = (currentFace = 2)

	Select Case currentFace
		Case 1:Face_Mouth.Collidable = True:Face_Eyes.Collidable = False:Face_Flat.Collidable = False:Face_Mouth.visible = False:Face_Eyes.visible = False:Face_Flat.visible = False:ColFaceGuideL3.Collidable = False 
		Case 2:Face_Mouth.Collidable = False:Face_Eyes.Collidable = True:Face_Flat.Collidable = False:Face_Mouth.visible = False:Face_Eyes.visible = False:Face_Flat.visible = False:ColFaceGuideL3.Collidable = True
		Case 3:Face_Mouth.Collidable = False:Face_Eyes.Collidable = False:Face_Flat.Collidable = True:Face_Mouth.visible = False:Face_Eyes.visible = False:Face_Flat.visible = False :ColFaceGuideL3.Collidable = False
		Case 4:Face_Mouth.Collidable = False:Face_Eyes.Collidable = False:Face_Flat.Collidable = True:Face_Mouth.visible = False:Face_Eyes.visible = False:Face_Flat.visible = False :ColFaceGuideL3.Collidable = False
	End Select
End Sub


Function getAngle(step)
	Dim radAngle
	radAngle = Round(4*Atn(1),6) * step / 180 / 2
	getAngle = 45 - 45 * Cos(radAngle)
End Function


'-----------------
' End Head/Face rotation script
'-----------------


'MouthAndEyeKicks
Dim BallLeftEye, BallRightEye, BallMouth

 'Kickers, poppers

Sub sw63div_hit():activeball.vely = 0.7 * activeball.vely :div.RotateToStart:End Sub
Sub sw64div_Hit():activeball.vely = 0.7 * activeball.vely :div.RotateToEnd:End Sub


Sub sw63x_Hit()
	RandomSoundHeadSaucer()
	Controller.Switch(63) = 1

	Set BallLeftEye = ActiveBall
End Sub

Sub sw63x_unHit()
	BallLeftEye = Empty
End Sub

Sub KickLeftEye(Enabled)
	if Enabled then
		Controller.Switch(63) = 0
		'PlaySound SoundFX("fx_Solenoid",DOFContactors)
		RandomSoundEjectHoleSolenoid()
		On Error Resume Next
		BallLeftEye.velz = 15:BallLeftEye.vely = 5:
		On Error Goto 0
		sw63x.timerenabled = 1
	end if
End Sub

Dim SW63xStep
Sub SW63x_Timer()
	Select Case SW63xStep
		Case 0:	TWKicker2.transy = 7
		Case 1: TWKicker2.transy = 14
		Case 2: TWKicker2.transy = 21
		Case 3: TWKicker2.transy = 14
		Case 4:	TWKicker2.transy = 0
		Case 5:
	End Select
	SW63xStep = SW63xStep + 1
End Sub


Sub sw64x_Hit()
	RandomSoundHeadSaucer()
	Controller.Switch(64) = 1:
	Set BallRightEye = ActiveBall
End Sub

Sub sw64x_unHit():
	BallRightEye = Empty
End Sub


Sub KickRightEye(Enabled)
	if Enabled then
		Controller.Switch(64) = 0
		'PlaySound SoundFX("fx_Solenoid",DOFContactors)
		RandomSoundEjectHoleSolenoid()
		On Error Resume Next
		BallRightEye.velz = 15:BallRightEye.vely = 5:
		 'vpmtimer.addtimer 100, "BallRightEye.velz = 10:BallRightEye.vely = 4'"
		On Error Goto 0
		sw64x.timerenabled = 1
	end if
End Sub


Dim SW64xStep
Sub SW64x_Timer()
	Select Case SW64xStep
		Case 0:	TWKicker3.transy = 7
		Case 1: TWKicker3.transy = 14
		Case 2: TWKicker3.transy = 21
		Case 3: TWKicker3.transy = 14
		Case 4:	TWKicker3.transy = 0
		Case 5:
	End Select
	SW64xStep = SW64xStep + 1
End Sub


Sub sw65x_Hit()
	RandomSoundHeadSaucer()
	Controller.Switch(65) = 1
	Set BallMouth = ActiveBall
	'sw65x.timerenabled = 1
End Sub

Sub sw65x_UnHit()
	BallMouth = Empty	
end Sub

Sub KickMouth(Enabled)
	if Enabled then
		Controller.Switch(65) = 0
		'PlaySound SoundFX("fx_Solenoid",DOFContactors)
		RandomSoundEjectHoleSolenoid()
		on error resume next
		BallMouth.velz = 15:BallMouth.vely = 5:
		on error goto 0
		sw65x.timerenabled = 1
	end if
End Sub


Dim SW65xStep
Sub SW65x_Timer()
	Select Case SW65xStep
		Case 0:	TWKicker1.transy = 7
		Case 1: TWKicker1.transy = 14
		Case 2: TWKicker1.transy = 21
		Case 3: TWKicker1.transy = 14
		Case 4:	TWKicker1.transy = 0
		Case 5:
	End Select
	SW65xStep = SW65xStep + 1
End Sub
'*******************************************************************************************************************************



   'Bumpers
     Sub Bumper1_Hit:vpmTimer.PulseSw 53:RandomSoundBumperLeft():End Sub 'bump1 = 1:Me.TimerEnabled = 1

       Sub Bumper1_Timer()  '53
          Select Case bump1
               Case 1:BR1.z = 0:bump1 = 2
               Case 2:BR1.z = -10:bump1 = 3
               Case 3:BR1.z = -20:bump1 = 4
               Case 4:BR1.z = -20:bump1 = 5
               Case 5:BR1.z = -10:bump1 = 6
               Case 6:BR1.z = 0:bump1 = 7
               Case 7:BR1.z = 10:Me.TimerEnabled = 0
           End Select
       End Sub

      Sub Bumper2_Hit:vpmTimer.PulseSw 55:RandomSoundBumperUp():End Sub 'bump2 = 1:Me.TimerEnabled = 1

       Sub Bumper2_Timer()   '55
          Select Case bump2
               Case 1:BR2.z = 0:bump2 = 2
               Case 2:BR2.z = -10:bump2 = 3
               Case 3:BR2.z = -20:bump2 = 4
               Case 4:BR2.z = -20:bump2 = 5
               Case 5:BR2.z = -10:bump2 = 6
               Case 6:BR2.z = 0:bump2 = 7
               Case 7:BR2.z = 10:Me.TimerEnabled = 0
           End Select
       End Sub

      Sub Bumper3_Hit:vpmTimer.PulseSw 54:RandomSoundBumperLow():End Sub 'bump3 = 1:Me.TimerEnabled = 1

       Sub Bumper3_Timer()  '54
          Select Case bump3
               Case 1:BR3.z = 0:bump3 = 2
               Case 2:BR3.z = -10:bump3 = 3
               Case 3:BR3.z = -20:bump3 = 4
               Case 4:BR3.z = -20:bump3 = 5
               Case 5:BR3.z = -10:bump3 = 6
               Case 6:BR3.z = 0:bump3 = 7
               Case 7:BR3.z = 10:Me.TimerEnabled = 0
           End Select
       End Sub


dim bibl : bibl = false

 'FlipperLanes and Plunger
   Sub sw15_Hit:Controller.Switch(15) = 1:RandomSoundRollover() : RandomSoundOutlaneRollover():End Sub
   Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub
   Sub sw16_Hit:Controller.Switch(16) = 1:RandomSoundRollover() : RandomSoundOutlaneRollover():End Sub
   Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub
   Sub sw17_Hit:Controller.Switch(17) = 1:RandomSoundRollover() : RandomSoundOutlaneRollover():End Sub
   Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub
   Sub sw18_Hit:Controller.Switch(18) = 1:RandomSoundRollover() : RandomSoundOutlaneRollover():End Sub
   Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub
   Sub sw52_Hit
		Controller.Switch(52) = 1
		activeball.mass = 1
		Stopsound "zintro"
		RandomSoundRollover()
		RandomSoundOutlaneRollover()
		bibl = True
	End Sub
   Sub sw52_UnHit
		bibl = false
		Controller.Switch(52) = 0
	End Sub

   'SS Lane
  Sub sw31_Hit:Controller.Switch(31) = 1:RandomSoundRollover():End Sub
   Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub
   Sub sw32_Hit:Controller.Switch(32) = 1:RandomSoundRollover():End Sub
   Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub
   Sub sw33_Hit:Controller.Switch(33) = 1:RandomSoundRollover():End Sub
   Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub
   Sub sw34_Hit:Controller.Switch(34) = 1:RandomSoundRollover():End Sub
   Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub
   Sub sw35_Hit:Controller.Switch(35) = 1:RandomSoundRollover():End Sub

   Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub
   'Under Right Ramp
   Sub sw44_Hit:Controller.Switch(44) = 1:RandomSoundRollover():End Sub
   Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub
   Sub sw45_Hit:Controller.Switch(45) = 1:RandomSoundRollover():End Sub
   Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub

   'Left Loop
  Sub sw43_Hit:Controller.Switch(43) = 1:RandomSoundRollover():End Sub
   Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub

   'Gate Switches
	Sub sw47_Hit
		if activeball.vely > 4 then
			activeball.vely = activeball.vely * 0.85
			if activeball.vely > 6 then activeball.vely = 6
		end if
'		debug.print "helmet speed: " & activeball.vely
		vpmTimer.PulseSw 47
	End Sub

   'Ball lock Switches
    'LockInit
  LockWall.IsDropped = 1
   Sub sw72_Hit:Controller.Switch(72) = 1:RandomSoundRollover():LockWall.IsDropped = 0:Switch72dir=-2:sw72.TimerEnabled = True:End Sub
   Sub sw72_UnHit:Controller.Switch(72) = 0:Switch72dir=4:sw72.TimerEnabled = True::LockWall.IsDropped = 1:RandomSoundMetal():End Sub
   Sub sw71_Hit:Controller.Switch(71) = 1:RandomSoundRollover():Switch71dir=-2:sw71.TimerEnabled = True:End Sub
   Sub sw71_UnHit:Controller.Switch(71) = 0:Switch71dir=4:sw71.TimerEnabled = True:End Sub

Sub sw41_Hit
'	debug.print "ramp speed: " & activeball.vely
	if activeball.vely > 6 then
		activeball.vely = activeball.vely * 0.85
		if activeball.vely > 10 then 
			activeball.vely = activeball.vely * 0.7
			if activeball.vely > 12 then activeball.vely = activeball.vely * 0.7
		end if
	end if
'	debug.print "--> ramp speed: " & activeball.vely
	vpmTimer.PulseSw(41):sw41.TimerEnabled = True
End Sub
Sub sw73_Hit:vpmTimer.PulseSw(73):sw73.TimerEnabled = True:SoundBallGate4:End Sub
Sub sw74_Hit
	SoundBallGate6
	vpmTimer.PulseSw(74):sw74.TimerEnabled = True
End Sub
Sub sw75_Hit
	vpmTimer.PulseSw(75):sw75.TimerEnabled = True
End Sub
Sub sw76_Hit:vpmTimer.PulseSw(76):sw76.TimerEnabled = True:SoundBallGate1:End Sub
Sub sw77_Hit
'	if activeball.vely < -20 then activeball.vely = activeball.vely * 0.7
	SoundBallGate0
	vpmTimer.PulseSw(77):sw77.TimerEnabled = True
End Sub

Const Switch41min = 0
Const Switch41max = -20
Dim Switch41dir

    'switch 41 animation
Switch41dir = -2

Sub sw41_timer()
 pRampSwitch1B.RotY = pRampSwitch1B.RotY + Switch41dir
    If pRampSwitch1B.RotY >= Switch41min Then
        sw41.timerenabled = False
        pRampSwitch1B.RotY = Switch41min
        Switch41dir = -2
    End If
    If pRampSwitch1B.RotY <= Switch41max Then
        Switch41dir = 4
    End If
End Sub

    'switch 71 animation
Const Switch71min = 0
Const Switch71max = -20
Dim Switch71dir
Switch71dir = -2

Sub sw71_timer()
 pRampSwitch7B.RotY = pRampSwitch7B.RotY + Switch71dir
    If Switch71dir = 4 Then
        If pRampSwitch7B.RotY >= Switch71min Then
            sw71.timerenabled = False
            pRampSwitch7B.RotY = Switch71min
        End If
    End If
    If Switch71dir = -2 Then
        If pRampSwitch7B.RotY <= Switch71max Then
            sw71.timerenabled = False
            pRampSwitch7B.RotY = Switch71max
        End If
    End If
End Sub

    'switch 72 animation
Const Switch72min = 0
Const Switch72max = -20
Dim Switch72dir
Switch72dir = -2

Sub sw72_timer()
 pRampSwitch8B.RotY = pRampSwitch8B.RotY + Switch72dir
    If pRampSwitch8B.RotY >= Switch72min Then
        sw72.timerenabled = False
        pRampSwitch8B.RotY = Switch72min
    End If
    If pRampSwitch8B.RotY <= Switch72max Then
        sw72.timerenabled = False
        pRampSwitch8B.RotY = Switch72max
    End If
End Sub

    'switch 73 animation
Const Switch73min = 0
Const Switch73max = -20
Dim Switch73dir
Switch73dir = -2

Sub sw73_timer()
 pRampSwitch6B.RotX = pRampSwitch6B.RotX + Switch73dir
    If pRampSwitch6B.RotX >= Switch73min Then
        sw73.timerenabled = False
        pRampSwitch6B.RotX = Switch73min
        Switch73dir = -2
    End If
    If pRampSwitch6B.RotX <= Switch73max Then
        Switch73dir = 4
    End If
End Sub

    'switch 74 animation
Const Switch74min = 0
Const Switch74max = -20
Dim Switch74dir
Switch74dir = -2

Sub sw74_timer()
 pRampSwitch3B.RotX = pRampSwitch3B.RotX + Switch74dir
    If pRampSwitch3B.RotX >= Switch74min Then
        sw74.timerenabled = False
        pRampSwitch3B.RotX = Switch74min
        Switch74dir = -2
    End If
    If pRampSwitch3B.RotX <= Switch74max Then
        Switch74dir = 4
    End If
End Sub

    'switch 75 animation
Const Switch75min = 0
Const Switch75max = -20
Dim Switch75dir
Switch75dir = -2

Sub sw75_timer()
 pRampSwitch2B.RotY = pRampSwitch2B.RotY + Switch75dir
    If pRampSwitch2B.RotY >= Switch75min Then
        sw75.timerenabled = False
        pRampSwitch2B.RotY = Switch75min
        Switch75dir = -2
    End If
    If pRampSwitch2B.RotY <= Switch75max Then
        Switch75dir = 4
    End If
End Sub


    'switch 76 animation
Const Switch76min = 0
Const Switch76max = -20
Dim Switch76dir
Switch76dir = -2

Sub sw76_timer()
 pRampSwitch5B.RotX = pRampSwitch5B.RotX + Switch76dir
    If pRampSwitch5B.RotX >= Switch76min Then
        sw76.timerenabled = False
        pRampSwitch5B.RotX = Switch76min
        Switch76dir = -2
    End If
    If pRampSwitch5B.RotX <= Switch76max Then
        Switch76dir = 4
    End If
End Sub


    'switch 77 animation
Const Switch77min = 0
Const Switch77max = -20
Dim Switch77dir
Switch77dir = -2

Sub sw77_timer()
 pRampSwitch4B.Rotx = pRampSwitch4B.Rotx + Switch77dir
    If pRampSwitch4B.Rotx >= Switch77min Then
        sw77.timerenabled = False
        pRampSwitch4B.Rotx = Switch77min
        Switch77dir = -2
    End If
    If pRampSwitch4B.Rotx <= Switch77max Then
        Switch77dir = 4
    End If
End Sub


'***Spot targets***

Sub DoubleTarget1_hit
vpmTimer.PulseSw 36:pSW36.TransY = -4:Target36Step = 1:sw36.TimerEnabled = 1
vpmTimer.PulseSw 37:pSW37.TransY = -4:Target37Step = 1:sw37.TimerEnabled = 1
End Sub



DIM target28step
Sub sw28_Hit:vpmTimer.PulseSw 28:pSW28.TransY = -3:Target28Step = 1:Me.TimerEnabled = 1:End Sub			
Sub sw28_timer()
	Select Case Target28Step

		Case 1:pSW28.TransY = -1
		Case 2:pSW28.TransY = 2
        Case 3:pSW28.TransY = 0:Me.TimerEnabled = 0
     End Select
	Target28Step = Target28Step + 1
End Sub	


DIM target36step
Sub sw36_Hit:vpmTimer.PulseSw 36:pSW36.TransY = -3:Target36Step = 1:Me.TimerEnabled = 1:End Sub			
Sub sw36_timer()
	Select Case Target36Step

		Case 1:pSW36.TransY = -1
		Case 2:pSW36.TransY = 2
        Case 3:pSW36.TransY = 0:Me.TimerEnabled = 0
     End Select
	Target36Step = Target36Step + 1
End Sub	


DIM target37step
Sub sw37_Hit:vpmTimer.PulseSw 37:pSW37.TransY = -3:Target37Step = 1:Me.TimerEnabled = 1:End Sub			
Sub sw37_timer()
	Select Case Target37Step

		Case 1:pSW37.TransY = -1
		Case 2:pSW37.TransY = 2
        Case 3:pSW37.TransY = 0:Me.TimerEnabled = 0
     End Select
	Target37Step = Target37Step + 1
End Sub	



'*****************
'Animated rubbers
'*****************
Sub wall69_Hit:vpmTimer.PulseSw 100:rubber25.visible = 0::rubber25a.visible = 1:wall69.timerenabled = 1:End Sub
Sub wall69_timer:rubber25.visible = 1::rubber25a.visible = 0: wall69.timerenabled= 0:End Sub

Sub wall75_Hit:vpmTimer.PulseSw 100:rubber24.visible = 0::rubber24a.visible = 1:wall75.timerenabled = 1:End Sub
Sub wall75_timer:rubber24.visible = 1::rubber24a.visible = 0: wall75.timerenabled= 0:End Sub

Sub wall77_Hit:vpmTimer.PulseSw 100:rubber23.visible = 0::rubber23a.visible = 1:wall77.timerenabled = 1:End Sub
Sub wall77_timer:rubber23.visible = 1::rubber23a.visible = 0: wall77.timerenabled= 0:End Sub

Sub wall79_Hit:vpmTimer.PulseSw 100:rubber22.visible = 0::rubber22a.visible = 1:wall79.timerenabled = 1:End Sub
Sub wall79_timer:rubber22.visible = 1::rubber22a.visible = 0: wall79.timerenabled= 0:End Sub

Sub wall80_Hit:vpmTimer.PulseSw 100:rubber21.visible = 0::rubber21a.visible = 1:wall80.timerenabled = 1:End Sub
Sub wall80_timer:rubber21.visible = 1::rubber21a.visible = 0: wall80.timerenabled= 0:End Sub

Sub wall81_Hit:vpmTimer.PulseSw 100:rubber20.visible = 0::rubber20a.visible = 1:wall81.timerenabled = 1:End Sub
Sub wall81_timer:rubber20.visible = 1::rubber20a.visible = 0: wall81.timerenabled= 0:End Sub

Sub wall82_Hit:vpmTimer.PulseSw 100:rubber3.visible = 0::rubber3a.visible = 1:wall82.timerenabled = 1:End Sub
Sub wall82_timer:rubber3.visible = 1::rubber3a.visible = 0: wall82.timerenabled= 0:End Sub

Sub wall83_Hit:vpmTimer.PulseSw 100:rubber14.visible = 0::rubber14a.visible = 1:wall83.timerenabled = 1:End Sub
Sub wall83_timer:rubber14.visible = 1::rubber14a.visible = 0: wall83.timerenabled= 0:End Sub

Sub wall84_Hit:vpmTimer.PulseSw 100:rubber19.visible = 0::rubber19a.visible = 1:wall84.timerenabled = 1:End Sub
Sub wall84_timer:rubber19.visible = 1::rubber19a.visible = 0: wall84.timerenabled= 0:End Sub

Sub LockWall_Hit():RandomSoundMetal:End Sub


'Material swap arrays.
Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image trans","Plastic with an image trans","Plastic with an image")


Dim DLintensity
'***************************************
'***Prim Image Swaps***
'***************************************
Sub ImageSwap(pri, group, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 1:pri.Image = group(0) 'Full
		Case 2:pri.Image = group(1) 'Fading...
		Case 3:pri.Image = group(2) 'Fading...
        Case 4:pri.Image = group(3) 'Off
    End Select
pri.blenddisablelighting = aLvl * DLintensity
End Sub


'***************************************
'***Prim Material Swaps***
'***************************************
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
	pri.blenddisablelighting = aLvl * DLintensity
End Sub


'***************************************
'***Begin nFozzy lamp handling***
'***************************************

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
Dim ModLampz : Set ModLampz = New DynamicLamps
InitLampsNF              ' Setup lamp assignments
LampTimer.Interval = -1
LampTimer.Enabled = 1



Sub LampTimer_Timer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
	ModLampz.Update1
'	If F119.IntensityScale > 0 then
'		fmfl27.visible = False
'		l27.visible = False
'	Else
'		fmfl27.visible = True
'		l27.visible = True
'	end if
'	debug.print modlampz.state(20)
End Sub


dim FrameTime, InitFrameTime : InitFrameTime = 0
Wall9.TimerInterval = 16
Wall9.TimerEnabled = True

Sub Wall9_Timer()	'Stealing this random wall's timer for -1 updates
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
	ModLampz.Update
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

Sub InitLampsNF()
	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating
	ModLampz.Filter = "LampFilter"

	'Adjust fading speeds (1 / full MS fading time)
	dim x
	for x = 0 to 110 : Lampz.FadeSpeedUp(x) = 1/5 : Lampz.FadeSpeedDown(x) = 1/20 : next
	for x = 111 to 140 : Lampz.FadeSpeedUp(x) = 1/10 : Lampz.FadeSpeedDown(x) = 1/50 : next
	for x = 0 to 28 : ModLampz.FadeSpeedUp(x) = 1/5 : ModLampz.FadeSpeedDown(x) = 1/30 : Next

	if GIColorMod <> 1 Then	'if LED GI, make them fade faster
		'modlampz 2 and 4. lampz 112 and 114
		Lampz.FadeSpeedUp(112) = 1/3 : Lampz.FadeSpeedDown(112) = 1/10
		Lampz.FadeSpeedUp(114) = 1/3 : Lampz.FadeSpeedDown(114) = 1/10
		ModLampz.FadeSpeedUp(2) = 1/2 : ModLampz.FadeSpeedDown(2) = 1/5
		ModLampz.FadeSpeedUp(4) = 1/2 : ModLampz.FadeSpeedDown(4) = 1/5
	end if

	ModLampz.FadeSpeedUp(2) = 1/15 : ModLampz.FadeSpeedDown(2) = 1/20
	ModLampz.FadeSpeedUp(4) = 1/15 : ModLampz.FadeSpeedDown(4) = 1/20

	'Lamp Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(11)= l11
	Lampz.MassAssign(11)= lb11
	Lampz.Callback(11) = "DisableLighting p11on, 10,"
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(12)= lb12
	Lampz.Callback(12) = "DisableLighting p12on, 10,"
	Lampz.MassAssign(13)= l13
	Lampz.MassAssign(13)= lb13
	Lampz.Callback(13) = "DisableLighting p13on, 10,"
	Lampz.MassAssign(14)= l14
	Lampz.MassAssign(14)= lb14
	Lampz.Callback(14) = "DisableLighting p14on, 10,"
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= lb15
	Lampz.Callback(15) = "DisableLighting p15on, 2,"
	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(16)= lb16
	Lampz.Callback(16) = "DisableLighting p16on, 2,"
	Lampz.MassAssign(17)= l17
	Lampz.MassAssign(17)= lb17
	Lampz.Callback(17) = "DisableLighting p17on, 2,"
	Lampz.MassAssign(18)= l18
	Lampz.MassAssign(18)= lb18
	Lampz.Callback(18) = "DisableLighting p18on, 10,"
	Lampz.MassAssign(21)= l21
	Lampz.MassAssign(21)= lb21
	Lampz.Callback(21) = "DisableLighting p21on, 10,"
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= lb22
	Lampz.Callback(22) = "DisableLighting p22on, 10,"
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(23)= lb23
	Lampz.Callback(23) = "DisableLighting p23on, 10,"
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= lb24
	Lampz.Callback(24) = "DisableLighting p24on, 10,"
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(25)= lb25
	Lampz.Callback(25) = "DisableLighting p25on, 10,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= lb26
	Lampz.Callback(26) = "DisableLighting p26on, 10,"
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= LB27
	Lampz.Callback(27) = "DisableLighting p27on, 10,"
	Lampz.MassAssign(28)= l28
	Lampz.MassAssign(28)= lb28
	Lampz.Callback(28) = "DisableLighting p28on, 10,"
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(31)= lb31
	Lampz.Callback(31) = "DisableLighting p31on, 10,"
	Lampz.MassAssign(32)= l32
	Lampz.MassAssign(32)= lb32
	Lampz.Callback(32) = "DisableLighting p32on, 10,"
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign(33)= lb33
	Lampz.Callback(33) = "DisableLighting p33on, 10,"
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= lb34
	Lampz.Callback(34) = "DisableLighting p34on, 10,"
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(35)= lb35
	Lampz.Callback(35) = "DisableLighting p35on, 10,"
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= lb36
	Lampz.Callback(36) = "DisableLighting p36on, 10,"
	Lampz.MassAssign(37)= l37
	Lampz.MassAssign(37)= lb37
	Lampz.Callback(37) = "DisableLighting p37on, 10,"
	Lampz.MassAssign(37)= l37a
	Lampz.Callback(37) = "DisableLighting p37a, .1,"
	Lampz.Callback(37) = "DisableLighting p37aa, 50,"
	Lampz.MassAssign(38)= l38
	Lampz.MassAssign(38)= l38a
	Lampz.MassAssign(38)= l38b
	Lampz.MassAssign(38)= l38c
	Lampz.MassAssign(38)= l38d
	Lampz.MassAssign(38)= l38e
'	Lampz.MassAssign(38)= l38f
	Lampz.MassAssign(38)= l38g
	Lampz.MassAssign(38)= l38h
	Lampz.MassAssign(38)= l38i
	Lampz.MassAssign(38)= l38j
	Lampz.MassAssign(38)= l38k
	Lampz.MassAssign(38)= l38l
	Lampz.MassAssign(38)= l38m
	Lampz.MassAssign(38)= lb38
'	Lampz.MassAssign(38)= l38n
	Lampz.Callback(38) = "DisableLighting p38on, 10,"


'	If HLColorType = 0 then
'		Lampz.Callback(37) =         "FadeMaterialHelmetBulb pBulb37, MaterialWhiteArray, "
'		Lampz.Callback(37) = "FadeMaterialHelmetFiliment pFiliment37, MaterialWhiteArray, "
'	End if
'	If HLColorType = 1 then
'		Lampz.Callback(37) =         "FadeMaterialHelmetBulb pBulb37, MaterialLTBlueArray, "
'		Lampz.Callback(37) = "FadeMaterialHelmetFiliment pFiliment37, MaterialLTBlueArray, "
'	End if
'	If HLColorType = 2 then
'		Lampz.Callback(37) =         "FadeMaterialEyesFrostedBulb pBulb37, MaterialFrostedLTBlueArray, "
'		Lampz.Callback(37) = "FadeMaterialHelmetFiliment pFiliment37, MaterialFrostedLTBlueArray, "
'	End if
'	If HLColorType = 3 then
'		'Lampz.Callback(37) =         "FadeMaterialHelmetBulb pBulb37, MaterialWhiteArray, "
'		Lampz.Callback(37) = "FadeMaterialHelmetFiliment pFiliment37, MaterialWhiteArray, "
'	End if


	Lampz.MassAssign(41)= L41
	Lampz.MassAssign(41)= L41a
	Lampz.MassAssign(41)= L41b
	Lampz.Callback(41) = "DisableLighting p41, .05,"
	Lampz.Callback(41) = "DisableLighting p41a, 50,"
	Lampz.Callback(41) = "DisableLighting pSS50k, 0.2,"	
	Lampz.MassAssign(42)= L42
	Lampz.MassAssign(42)= L42a
	Lampz.MassAssign(42)= L42b
	Lampz.Callback(42) = "DisableLighting p42, .05,"
	Lampz.Callback(42) = "DisableLighting p42a, 50,"
	Lampz.Callback(42) = "DisableLighting pSS75k, 0.2,"
	Lampz.MassAssign(43)= L43
	Lampz.MassAssign(43)= L43a
	Lampz.MassAssign(43)= L43b
	Lampz.Callback(43) = "DisableLighting p43, .05,"
	Lampz.Callback(43) = "DisableLighting p43a, 50,"
	Lampz.Callback(43) = "DisableLighting pSS100k, 0.2,"
	Lampz.MassAssign(44)= L44
	Lampz.MassAssign(44)= L44a
	Lampz.MassAssign(44)= L44b
	Lampz.Callback(44) = "DisableLighting p44, .05,"
	Lampz.Callback(44) = "DisableLighting p44a, 50,"
	Lampz.Callback(44) = "DisableLighting pSS200k, 0.2,"
	Lampz.MassAssign(45)= L45
	Lampz.MassAssign(45)= L45a
	Lampz.MassAssign(45)= L45b
	Lampz.Callback(45) = "DisableLighting p45, .05,"
	Lampz.Callback(45) = "DisableLighting p45a, 50,"
	Lampz.Callback(45) = "DisableLighting pSS25k, 0.2,"
	Lampz.MassAssign(46)= l46
	Lampz.MassAssign(46)= l46a
	Lampz.Callback(46) = "DisableLighting p46, .05,"
	Lampz.Callback(46) = "DisableLighting p46a, 25,"
	Lampz.MassAssign(47)= l47
	Lampz.MassAssign(47)= l47a
	Lampz.Callback(47) = "DisableLighting p47, .05,"
	Lampz.Callback(47) = "DisableLighting p47a, 25,"

	Lampz.MassAssign(48)= l48
	Lampz.MassAssign(48)= l48a
	Lampz.Callback(48) = "DisableLighting p48, .05,"
	Lampz.Callback(48) = "DisableLighting p48a, 25,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= lb51
	Lampz.Callback(51) = "DisableLighting p51on, 10,"
	Lampz.MassAssign(52)= l52
	Lampz.MassAssign(52)= lb52
	Lampz.Callback(52) = "DisableLighting p52on, 10,"
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= lb53
	Lampz.Callback(53) = "DisableLighting p53on, 10,"
	Lampz.MassAssign(54)= l54
	Lampz.MassAssign(54)= lb54
	Lampz.Callback(54) = "DisableLighting p54on, 10,"
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(55)= lb55
	Lampz.MassAssign(55)= l55a
	Lampz.Callback(55) = "DisableLighting p55on, 10,"
	Lampz.MassAssign(56)= l56
	Lampz.MassAssign(56)= lb56
	Lampz.Callback(56) = "DisableLighting p56on, 10,"
	Lampz.MassAssign(57)= l57
	Lampz.MassAssign(57)= lb57
	Lampz.Callback(57) = "DisableLighting p57on, 10,"
	Lampz.MassAssign(58)= l58
	Lampz.MassAssign(58)= lb58
	Lampz.Callback(58) = "DisableLighting p58on, 10,"
	Lampz.MassAssign(61)= l61
	Lampz.MassAssign(61)= lb61
	Lampz.Callback(61) = "DisableLighting p61on, 10,"
	Lampz.MassAssign(62)= l62
	Lampz.MassAssign(62)= lb62
	Lampz.Callback(62) = "DisableLighting p62on, 10,"
	Lampz.MassAssign(63)= l63
	Lampz.MassAssign(63)= lb63
	Lampz.Callback(63) = "DisableLighting p63on, 10,"
	Lampz.MassAssign(64)= l64
	Lampz.MassAssign(64)= l64a
	Lampz.MassAssign(64)= Fl64
	Lampz.Callback(64) = "DisableLighting BallLockLightSticker, 0.01,"
	Lampz.Callback(64) = "DisableLighting p64, .05,"
	Lampz.Callback(64) = "DisableLighting p64a, 25,"
	Lampz.MassAssign(86)= f86
	Lampz.MassAssign(87)= f87
	Lampz.MassAssign(88)= f88
	Lampz.MassAssign(65)= l65
	Lampz.MassAssign(65)= lb65
	Lampz.Callback(65) = "DisableLighting p65on, 10,"
	Lampz.MassAssign(66)= l66
	Lampz.MassAssign(66)= lb66
	Lampz.Callback(66) = "DisableLighting p66on, 10,"
	Lampz.MassAssign(67)= l67
	Lampz.MassAssign(67)= lb67
	Lampz.Callback(67) = "DisableLighting p67on, 10,"
	Lampz.MassAssign(68)= l68
	Lampz.MassAssign(68)= lb68
	Lampz.Callback(68) = "DisableLighting p68on, 10,"

	If VRRoom > 0 Then
		Lampz.MassAssign(46)= VRBGFL46LEye
		Lampz.MassAssign(46)= VRBGFL46LEye2
		Lampz.MassAssign(47)= VRBGFL47REye
		Lampz.MassAssign(47)= VRBGFL47REye2
		Lampz.MassAssign(48)= VRBGFL48Mouth
		Lampz.MassAssign(48)= VRBGFL48Mouth2
		Lampz.MassAssign(71)= BG8M
		Lampz.MassAssign(72)= BG7M
		Lampz.MassAssign(73)= BG6M
		Lampz.MassAssign(74)= BG5M
		Lampz.MassAssign(75)= BG4M
		Lampz.MassAssign(76)= BG3M
		Lampz.MassAssign(77)= BG2M
		Lampz.MassAssign(78)= BG1M
		Lampz.MassAssign(81)= VRBGFL81Hip
		Lampz.MassAssign(81)= VRBGFL81Hip2
		Lampz.MassAssign(82)= VRBGFL82MLeg
		Lampz.MassAssign(82)= VRBGFL82MLeg2
		Lampz.MassAssign(83)= VRBGFL83Knee
		Lampz.MassAssign(83)= VRBGFL83Knee2
		Lampz.MassAssign(84)= VRBGFL84Foot
		Lampz.MassAssign(84)= VRBGFL84Foot2
		Lampz.MassAssign(85)= VRBGFL85Shoulder
		Lampz.MassAssign(85)= VRBGFL85Shoulder2
	End If




''***Helmet Lights***

	Lampz.MassAssign(91)= l91
	Lampz.MassAssign(92)= l92
	Lampz.MassAssign(93)= l93
	Lampz.MassAssign(94)= l94
	Lampz.MassAssign(95)= l95
	Lampz.MassAssign(96)= l96
	Lampz.MassAssign(97)= l97
	Lampz.MassAssign(98)= l98
	Lampz.MassAssign(101)= l101
	Lampz.MassAssign(102)= l102
	Lampz.MassAssign(103)= l103
	Lampz.MassAssign(104)= l104
	Lampz.MassAssign(105)= l105
	Lampz.MassAssign(106)= l106
	Lampz.MassAssign(107)= l107
	Lampz.MassAssign(108)= l108

	'HelmetLightRefl
	Lampz.MassAssign(91)= l91_refl
'	Lampz.MassAssign(92)= l92_refl
	Lampz.MassAssign(93)= l93_refl
'	Lampz.MassAssign(94)= l94_refl
	Lampz.MassAssign(95)= L95_refl
'	Lampz.MassAssign(96)= l96_refl
	Lampz.MassAssign(97)= l97_refl
'	Lampz.MassAssign(98)= l98_refl
'	Lampz.MassAssign(101)= l101_refl
	Lampz.MassAssign(102)= l102_refl
'	Lampz.MassAssign(103)= l103_refl
	Lampz.MassAssign(104)= l104_refl
'	Lampz.MassAssign(105)= l105_refl
	Lampz.MassAssign(106)= l106_refl
'	Lampz.MassAssign(107)= l107_refl
	Lampz.MassAssign(108)= l108_refl


'	'Flashers
	if Not bPROC then
		ModLampz.MassAssign(17)= f117   'Billions
		ModLampz.MassAssign(17)= LB117   'Billions
		ModLampz.Callback(17) = "DisableLighting p117on, 50,"
'		ModLampz.MassAssign(18)= f118   'Left ramp
'		ModLampz.MassAssign(18)= F118_SL
'		ModLampz.MassAssign(19)= f119   'jackpot
'		ModLampz.MassAssign(19)= VRBGFL19_1
'		ModLampz.MassAssign(19)= VRBGFL19_2
'		ModLampz.MassAssign(19)= VRBGFL19_3
'		ModLampz.MassAssign(19)= VRBGFL19_4
'		ModLampz.MassAssign(20)= f120  'SkillShot
'		ModLampz.MassAssign(20)= F120_SR  'SkillShot
'		ModLampz.MassAssign(20)= F120_pf  'SkillShot
	Else
		Lampz.MassAssign(117)= f117   'Billions
		Lampz.MassAssign(117)= lb117   'Billions
		Lampz.Callback(117) = "DisableLighting p117on, 50,"
'		Lampz.MassAssign(118)= f118   'Left ramp
'		Lampz.MassAssign(118)= F118_SL
		Lampz.MassAssign(119)= f119   'jackpot
		Lampz.MassAssign(119)= LB119   'jackpot
		Lampz.Callback(119) = "DisableLighting p119on, 10,"
'		Lampz.MassAssign(120)= f120  'SkillShot
'		Lampz.MassAssign(120)= f120_sr  'SkillShot
'		Lampz.MassAssign(120)= f120_pf  'SkillShot
	end if


	if Not bPROC Then

		' 2 Rear Playfield
		ModLampz.MassAssign(2)= ColToArray(GiRear)
		ModLampz.Callback(2) = "FadeMaterialColor DecalHeart1, 180, 80, "
'		Lampz.state(2) = 1		'Turn on GI to Start
		ModLampz.Callback(2) = "GIUpdates 2,"

		' 4 Front Playfield
		ModLampz.MassAssign(4)= ColToArray(GiFront)
		ModLampz.MassAssign(4)= AmbientOverhead
		ModLampz.Callback(4) = "GIUpdates 4,"
		for x = 0 to 4 : ModLampz.State(x) = 1 : Next

		If VRRoom > 0 Then
			ModLampz.MassAssign(2)= ColToArray(VRSpeakerFlashers)
			' 0 Backglass (Body)
			ModLampz.MassAssign(0)=ColToArray(VRGiBody)
			Lampz.state(0) = 1
			'ModLampz.Callback(0) = "GIUpdates 0,"

			' 3 BackGlass (Non-body)
			ModLampz.MassAssign(3)=ColToArray(VRGiNoBody)
			Lampz.state(3) = 1
			'ModLampz.Callback(3) = "GIUpdates 3,"
		End If
	Else

		Lampz.MassAssign(112)= ColToArray(GiRear)
		Lampz.Callback(112) = "FadeMaterialColor DecalHeart1, 180, 80, "
'		Lampz.state(112) = 1		'Turn on GI to Start
		Lampz.Callback(112) = "GIUpdates 2,"

		' 3 BackGlass (Non-body)

		' 4 Front Playfield
		Lampz.MassAssign(114)= ColToArray(GiFront)
		Lampz.MassAssign(114)= AmbientOverhead
		Lampz.Callback(114) = "GIUpdates 4,"
		for x = 110 to 114 : Lampz.State(x) = 1 : Next

		If VRRoom > 0 Then
			Lampz.MassAssign(112)= ColToArray(VRSpeakerFlashers)
			' 0 Backglass (Body)
			Lampz.MassAssign(110)=ColToArray(VRGiBody)
			Lampz.state(110) = 1
			'ALampz.Callback(110) = "GIUpdates 0,"

			' 3 BackGlass (Non-body)
			Lampz.MassAssign(113)=ColToArray(VRGiNoBody)
			Lampz.state(113) = 1
			'Lampz.Callback(113) = "GIUpdates 3,"
		End If

	end if


	'Turn off all lamps on startup
	lampz.Init	'This just turns state of any lamps to 1
	ModLampz.Init

	'Immediate update to turn on GI, turn off lamps
	lampz.update
	ModLampz.Update

End Sub



Sub FadeMaterialColor(PrimName, LevelMax, LevelMin, aLvl)
	dim rgbLevel
	rgbLevel = ((LevelMax - LevelMin) * aLvl) + LevelMin

    Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    GetMaterial PrimName.material, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    UpdateMaterial PrimName.material, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, rgb(rgbLevel,rgbLevel,rgbLevel), glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub


'Lamp Filter
Function LampFilter(aLvl)
	if aLvl < 0 then aLvl = 0
	LampFilter = aLvl^1.6	'exponential curve?
End Function


Dim GIoffMult : GIoffMult = 2 'adjust how bright the inserts get when the GI is off
Dim GIoffMultFlashers : GIoffMultFlashers = 2	'adjust how bright the Flashers get when the GI is off

FGIFront.ModulateVsAdd = 0.999
FGIRear.ModulateVsAdd = 0.999

if CabinetMode = 0 Then
	FGIFront_SR.visible = true
	FGIFront_SL.visible = true
	FGIFront_SR_CM.visible = False
	FGIFront_SL_CM.visible = False
	FGIRear_SR.visible = true
	FGIRear_SL.visible = true
	FGIRear_SR_CM.visible = False
	FGIRear_SL_CM.visible = False
Else
	FGIFront_SR.visible = false
	FGIFront_SL.visible = False
	FGIFront_SR_CM.visible = true
	FGIFront_SL_CM.visible = true
	FGIRear_SR.visible = false
	FGIRear_SL.visible = False
	FGIRear_SR_CM.visible = true
	FGIRear_SL_CM.visible = true
end if

'pGiBulb001.blenddisablelighting
'modlampz.state(2)=0
'modlampz.state(4)=0
'GI callback

Sub GIUpdates(Circuit, aLvl)	'aLvl argument is unused - Circuit 4 = front; 2 = rear; 0 = bg body; 3 = bg no body
	'2 and 4 are the major PF gi circuits, averaging them together...
	dim giAvg, GIFrontLvl, GIRearLvl

	if Not bPROC then
		if Lampz.UseFunction then 	'Callbacks don't get this filter automatically
			giAvg = (LampFilter(ModLampz.Lvl(2)) + LampFilter(ModLampz.Lvl(4)) )/2
			GIFrontLvl = LampFilter(ModLampz.Lvl(4))
			GIRearLvl = LampFilter(ModLampz.Lvl(2))
		Else
			giAvg = (ModLampz.Lvl(2) + ModLampz.Lvl(4) )/2
			GIFrontLvl = ModLampz.Lvl(4)
			GIRearLvl = ModLampz.Lvl(2)
		end if
	else
		if Lampz.UseFunction then 	'Callbacks don't get this filter automatically
			giAvg = (LampFilter(Lampz.Lvl(112)) + LampFilter(Lampz.Lvl(114)) )/2
			GIFrontLvl = LampFilter(Lampz.Lvl(114))
			GIRearLvl = LampFilter(Lampz.Lvl(112))
		Else
			giAvg = (Lampz.Lvl(112) + Lampz.Lvl(114) )/2
			GIFrontLvl = Lampz.Lvl(114)
			GIRearLvl = Lampz.Lvl(112)
		end if
	end if

	If Circuit = 4 Then
		FGIFront.opacity = 1700 * GIFrontLvl
		If CabinetMode = 0 then 
			FGIFront_SR.opacity = 200 * GIFrontLvl
			FGIFront_SL.opacity = 200 * GIFrontLvl
		Else
			FGIFront_SR_CM.opacity = 200 * GIFrontLvl
			FGIFront_SL_CM.opacity = 200 * GIFrontLvl
		end if

		for each x in GIBulbsFrontArr
			if GIColorMod = 1 then
				x.blenddisablelighting = 0.5 * (GIFrontLvl^2)
			else
				x.blenddisablelighting = 2 * (GIFrontLvl^2)
			end if
		Next

		for each x in BluePegsArr
			x.blenddisablelighting = 0.4 * (GIFrontLvl^2)
		Next

		'heartramp
		primitive47.blenddisablelighting = 0.03 * (GIFrontLvl^2) - 0.015
	End If


	If Circuit = 2 Then
		FGIRear.opacity = 2200 * GIRearLvl
		FGIRear_SR.opacity = 200 * GIRearLvl
		FGIRear_SL.opacity = 200 * GIRearLvl
		for each x in GIBulbsRearArr
			if GIColorMod = 1 then
				x.blenddisablelighting = 0.5 * (GIRearLvl^2)
			else
				x.blenddisablelighting = 2 * (GIRearLvl^2)
			end if
		Next

		'leftramp
		Primitive098.blenddisablelighting = 0.025 * (GIRearLvl^2) - 0.01
		

		pBump1.blenddisablelighting = 0.7 * GIRearLvl
		pBump2.blenddisablelighting = 0.7 * GIRearLvl
		pBump3.blenddisablelighting = 0.7 * GIRearLvl


		pBump4.blenddisablelighting = 125 * GIRearLvl
		pBump5.blenddisablelighting = 125 * GIRearLvl
		pBump6.blenddisablelighting = 125 * GIRearLvl
		BumperCap1.blenddisablelighting = 1 * GIRearLvl
		BumperCap2.blenddisablelighting = 1 * GIRearLvl
		BumperCap3.blenddisablelighting = 1 * GIRearLvl


		SetMaterialOpacity "MetalWallsPrimON", 10 * GIRearLvl
		metallwalls_off.blenddisablelighting = 0.3 * GIRearLvl
	End If

	'Brighten inserts when GI is Low

	If GIPerf = 0 Then
		dim GIscale
		GiScale = (GIoffMult-1) * (ABS(giAvg-1 )  ) + 1	'invert
		dim x : for x = 0 to 100
			lampz.Modulate(x) = GiScale
		Next

		'Brighten Flashers when GI is low
		GiScale = (GIoffMultFlashers-1) * (ABS(giAvg-1 )  ) + 1	'invert
		for x = 5 to 28
			modlampz.modulate(x) = GiScale
		Next
		'tb.text = giscale
	End If
End Sub

dim girearprev

sub GIoff
	if Not bPROC then
		modlampz.state(4) = 0
		modlampz.state(2) = 0
	Else
		lampz.state(112) = 0
		lampz.state(114) = 0
	end if
end sub

sub GIon
	if Not bPROC then
		modlampz.state(4) = 1
		modlampz.state(2) = 1
	Else
		lampz.state(112) = 1
		lampz.state(114) = 1
	end if
end sub

'Helper functions

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function


'*******************************
'Intermediate Solenoid Procedures (Setlamp, etc)
'********************************
'Solenoid pipeline looks like this:
'Pinmame Controller -> UseSolenoids -> Solcallback -> intermediate subs (here) -> ModLampz dynamiclamps object -> object updates / more callbacks

'GI
'Pinmame Controller -> core.vbs PinMameTimer Loop -> GIcallback2 ->  ModLampz dynamiclamps object -> object updates / more callbacks
'(Can't even disable core.vbs's GI handling unless you deliberately set GIcallback & GIcallback2 to Empty)

'Lamps, for reference:
'Pinmame Controller -> LampTimer -> Lampz Fading Object -> Object Updates / callbacks

'iaakki - bPROC doesn't seem to give modulated values for GI. So had to revert this to use GICallback instead of GICallback2.
if Not bPROC then
	Set GICallback2 = GetRef("SetGI")
Else
	Set GICallback = GetRef("SetGIProc")
end if
'    GI lights controlled by Strings
' 01 Upper BackGlass		'Case 0
' 02 Rudy					'Case 1
' 03 Upper Playfield		'Case 2
' 04 Center BackGlass		'Case 3
' 05 Lower Playfield		'Case 4

Sub SetGI(aNr, aValue)
	ModLampz.SetGI aNr, aValue 'Redundant. Could reassign GI indexes here
End Sub

Sub SetGIProc(aNr, aValue)
'	msgbox "nr: " & aNr & " value: " & aValue
	SetLamp 110 + aNr, aValue 'Redundant. Could reassign GI indexes here
End Sub


'***************************************
'***End nFozzy lamp handling***
'***************************************



' ******************************************************************************************************************************************
'**********Sling Shot Animations
'****************
Dim Ustep

Sub UpperSlingShot_Slingshot
    RandomSoundSlingshotTopRight() 'todo
    USling.Visible = 0
    USling1.Visible = 1
    slingu.TransZ = -28
    UStep = 0
    UpperSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 56
    'gi1.State = 0:Gi2.State = 0
End Sub

Sub UpperSlingShot_Timer
    Select Case UStep
        Case 1:USLing1.Visible = 0:USLing2.Visible = 1:slingu.transZ = -10
        Case 2:USLing2.Visible = 0:USLing.Visible = 1:slingu.transZ = 0:UpperSlingShot.TimerEnabled = 0:'gi1.State = 1:Gi2.State = 1
   End Select
    UStep = UStep + 1
End Sub

Dim LStep, RStep



Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotLeft()
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -28
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 57
    'gi1.State = 0:Gi2.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 2:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:'gi1.State = 1:Gi2.State = 1
   End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight()
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -28
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 58
    'gi1.State = 0:Gi2.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 2:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:'gi1.State = 1:Gi2.State = 1
   End Select
    RStep = RStep + 1
End Sub

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
if bPROC then DisplayTimer.enabled = false	'disabling for PROC for now

Dim Digits(32)
 Digits(0)=Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
 Digits(1)=Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
 Digits(2)=Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
 Digits(3)=Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
 Digits(4)=Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
 Digits(5)=Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
 Digits(6)=Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
 Digits(7)=Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
 Digits(8)=Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
 Digits(9)=Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
 Digits(10)=Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
 Digits(11)=Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
 Digits(12)=Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
 Digits(13)=Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
 Digits(14)=Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
 Digits(15)=Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
 Digits(16)=Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
 Digits(17)=Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
 Digits(18)=Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
 Digits(19)=Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
 Digits(20)=Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
 Digits(21)=Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
 Digits(22)=Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
 Digits(23)=Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
 Digits(24)=Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
 Digits(25)=Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
 Digits(26)=Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
 Digits(27)=Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
 Digits(28)=Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
 Digits(29)=Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
 Digits(30)=Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
 Digits(31)=Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)

 Sub DisplayTimer_Timer
	If VRRoom = 0 Then
		Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
		ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED)Then
			If DesktopMode = True Then
		   For ii=0 To UBound(chgLED)
			  num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				if (num < 32) then
				  For Each obj In Digits(num)
					   If chg And 1 Then obj.State=stat And 1
					   chg=chg\2 : stat=stat\2
					  Next
				Else
					   end if
			Next
		   end if
		End If
	End If
 End Sub
'**********************************************************************************************************
'**********************************************************************************************************


'******************************************************
'			TROUGH BASED ON NFOZZY'S
'******************************************************

Sub sw27_Hit():Controller.Switch(27) = 1:UpdateTrough:End Sub
Sub sw27_UnHit():Controller.Switch(27) = 0:UpdateTrough:End Sub
Sub sw26_Hit():Controller.Switch(26) = 1:UpdateTrough:End Sub
Sub sw26_UnHit():Controller.Switch(26) = 0:UpdateTrough:End Sub
Sub ballrelease_Hit():Controller.Switch(25) = 1:UpdateTrough:End Sub
Sub ballrelease_UnHit():Controller.Switch(25) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 150
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If ballrelease.BallCntOver = 0 Then sw26.kick 60, 5
	If sw26.BallCntOver = 0 Then sw27.kick 60, 5	
	Me.Enabled = 0
End Sub

'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub KickBallToLane(Enabled)
	if enabled then
		RandomSoundShooterFeeder()
		ballrelease.Kick 60,12
		UpdateTrough
	end if
End Sub

sub kisort(enabled)
	if enabled then
		Drain.Kick 70,20
		controller.switch(38) = false
		RandomSoundOutholeKicker()
	end if
end sub

Sub Drain_hit()
    RandomSoundOutholeHit()
    controller.switch(38) = true
End Sub


'***OPTIONS***

Dim TableOptions, TableName, Rubbercolor,xxrubbercolor, RubberColorType, GIColorMod, GIColorModType, xxSSRampColor, SSRampColorMod, SSRampColorModType, HLColorType, HottieMod, HottieModType, FlipperRubbersType, FlipperRubbers
Dim DivPOS, SideWallType, HLcolor, rails


	Dim HLColorRed
	Dim HLColorGreen
	Dim HLColorBlue

Sub SetOptions()

'ShipMod
    If ShipMod = 1 Then
        pShipToy.Visible = True
        F124c.visible = true
        F124d.visible = true
        F124e.visible = true
        F124f.visible = true
        F124g.visible = true
        F124h.visible = true
    Else
        pShipToy.Visible = False
        F124c.visible = False
        F124d.visible = False
        F124e.visible = False
        F124f.visible = False
        F124g.visible = False
        F124h.visible = False
    End if



'Cheater Post
    If cheaterpost = 1 then
        rubber38.collidable = True
        cpost.visible = True
        Primitive69.visible = True
    Else
        rubber38.collidable = false
        cpost.visible = false
        Primitive69.visible = false
    End If



' Sidewall switching
    If sidewalls = 3 then
        SideWallType = Int(Rnd*3)
    Else
        SideWallType = sidewalls
    End If


    select case SideWallType
        case 2: pSidewall.image = "sidewalls_texture":pSidewall.visible = true
        case 1: pSidewall.image = "sidewalls_texture2":pSidewall.visible = true
        case 0: pSidewall.image = "sidewalls_texture3":pSidewall.visible = true
    end select

'Random Diverter POS at startup
    DivPOS = Int(Rnd*2)+1
    If DivPOS = 1 Then
        div.RotateToStart
    Else
        div.RotateToEnd
    End If




	dim x

    for each x in HelmetLights
        x.Falloff=5
        x.FalloffPower=3
        x.TransmissionScale=0.6
        x.BulbModulateVsAdd=0.2
        x.intensity=200
        x.depthbias=1500 'not sure why it looks better with this. Correct value would be 1100.
    next




	'Helmet Lamp Color
	Dim BlueFull, Blue, BlueI, BlueFlasher, YellowFull, Yellow, YellowI, YellowFlasher, xxHLColorL, xxHLColorF, xxHLRefl, BulbBlueOff, BulbGIOff, xxHLBulb, BulbFrostedLTBlueOff, CCHLI, SSBulb, xxSSBulb


	
	YellowFull = rgb (255,197,143)
	Yellow = rgb (255,197,143)
	YellowFlasher = rgb (255,197,143)
	YellowI = 125
	
	BlueFull = rgb(50,50,255)
	Blue = rgb(50,50,255)
	BlueFlasher = rgb(150,150,255)
	BlueI = 125
	
	CCHLI = 125
	
	If HLColor = 0 Then
		HLColorType = Int(Rnd*5)+1
	Else
		HLColorType = HLColor
	End If
	'
	
	If HLColorType = 1 Then
		for each xxHLColorL in HelmetLights
			xxHLColorL.Color=Yellow
			xxHLColorL.ColorFull=YellowFull
		next

		for each xxHLColorL in HelmetLightReflArr
			xxHLColorL.Color = Yellow
			xxHLColorL.ScaleBulbMesh = 1
			xxHLColorL.intensity = 1
		next

		CCHL.enabled = False
	End If
	
	If HLColorType = 2 Then
		for each xxHLColorL in HelmetLights
			xxHLColorL.Color= rgb(50,50,255)
			xxHLColorL.ColorFull= rgb(50,50,255)
		next

		for each xxHLColorL in HelmetLightReflArr
			xxHLColorL.Color = rgb(50,50,255)
			xxHLColorL.ScaleBulbMesh = 1
			xxHLColorL.intensity = 1
		next

		CCHL.enabled = False
	End If
	
	If HLColorType = 3 Then
		for each xxHLColorL in HelmetLights
			xxHLColorL.Color=rgb(HLColorRed,HLColorGreen,HLColorBlue)
			xxHLColorL.ColorFull=rgb(HLColorRed,HLColorGreen,HLColorBlue)
		next

		for each xxHLColorL in HelmetLightReflArr
			xxHLColorL.Color = rgb(HLColorRed,HLColorGreen,HLColorBlue)
			xxHLColorL.ScaleBulbMesh = 1
			xxHLColorL.intensity = 1
		next

		CCHL.enabled = False
	End If	

	If HLColorType = 4 Then
		for each xxHLColorL in HelmetLights
			xxHLColorL.Intensity = CCHLI 
		next
		CCHL.enabled = true
	End If

	If HLColorType = 5 Then
		for each xxHLColorL in HelmetLights
			xxHLColorL.Intensity = CCHLI 
		next
		CCHL.enabled = true
	End If



If Rails =1 Then
		Leftrail.visible = 0
		Rightrail.visible = 0
Else
	If VRRoom = 0 Then
		Leftrail.visible = 1
		Rightrail.visible = 1
	Else
		Leftrail.visible = 0
		Rightrail.visible = 0	
	End If
End if


If RubberColor = 2 Then
	RubberColorType = Int(Rnd*2)
Else
	RubberColorType = RubberColor
End If


If RubberColorType = 0 Then
	for each xxRubberColor in RubbersColorMod
		xxRubberColor.Material = "RubberBlack"
		next
End If


If RubberColorType = 1 Then
	for each xxRubberColor in RubbersColorMod
		xxRubberColor.Material = "RubberWhite"
		next
End If


If SSRampColorMod = 2 Then
	SSRampColorModType = Int(Rnd*2)
Else
	SSRampColorModType = SSRampColorMod
End If

if vrroom = 0 Then
	Const tValue = 0.7

	'lower
	l41a.TransmissionScale = tValue
	l42a.TransmissionScale = tValue
	l43a.TransmissionScale = tValue
	l44a.TransmissionScale = tValue * 0.5
	l45a.TransmissionScale = tValue * 1.2

	'top
	l41b.TransmissionScale = tValue * 1.5
	l42b.TransmissionScale = tValue * 1.5
	l43b.TransmissionScale = tValue * 1.5
	l44b.TransmissionScale = tValue * 0.9
	l45b.TransmissionScale = tValue * 1.7

	l41b.BulbModulateVsAdd = 0.99
	l42b.BulbModulateVsAdd = 0.99
	l43b.BulbModulateVsAdd = 0.99
	l44b.BulbModulateVsAdd = 0.99
	l45b.BulbModulateVsAdd = 0.99
end if

If SSRampColorModType = 0 Then
		l41.Color = rgb (255,197,143)
		l41.ColorFull = rgb (255,197,143)
		l41a.Color = rgb (255,197,143)
		l41a.ColorFull = rgb (255,197,143)
		l41b.Color = rgb (255,197,143)
		l41a.ColorFull = rgb (255,197,143)
		p41.Material = "44BulbWhiteOff1"
		l42.Color = rgb (255,197,143)
		l42.ColorFull = rgb (255,197,143)
		l42a.Color = rgb (255,197,143)
		l42a.ColorFull = rgb (255,197,143)
		l42b.Color = rgb (255,197,143)
		l42a.ColorFull = rgb (255,197,143)
		p42.Material = "44BulbWhiteOff1"
		l43.Color = rgb (255,197,143)
		l43.ColorFull = rgb (255,197,143)
		l43a.Color = rgb (255,197,143)
		l43a.ColorFull = rgb (255,197,143)
		l43b.Color = rgb (255,197,143)
		l43a.ColorFull = rgb (255,197,143)
		p43.Material = "44BulbWhiteOff1"
		l44.Color = rgb (255,197,143)
		l44.ColorFull = rgb (255,197,143)
		l44a.Color = rgb (255,197,143)
		l44a.ColorFull = rgb (255,197,143)
		l44b.Color = rgb (255,197,143)
		l44a.ColorFull = rgb (255,197,143)
		p44.Material = "44BulbWhiteOff1"
		l45.Color = rgb (255,197,143)
		l45.ColorFull = rgb (255,197,143)
		l45a.Color = rgb (255,197,143)
		l45a.ColorFull = rgb (255,197,143)
		l45b.Color = rgb (255,197,143)
		l45a.ColorFull = rgb (255,197,143)
		p45.Material = "44BulbWhiteOff1"
End if

If SSRampColorModType = 1 Then
		l41.Color = rgb (255,197,143)
		l41.ColorFull = rgb (255,197,143)
		l41a.Color = rgb (255,197,143)
		l41a.ColorFull = rgb (255,197,143)
		l41b.Color = rgb (255,197,143)
		l41a.ColorFull = rgb (255,197,143)
		p41.Material = "44BulbWhiteOff1"
		l42.Color = rgb (255,15,15)
		l42.ColorFull = rgb (255,15,15)
		l42a.Color = rgb (255,15,15)
		l42a.ColorFull = rgb (255,15,15)
		l42b.Color = rgb (255,15,15)
		l42b.ColorFull = rgb (255,15,15)
		p42.Material = "44BulbRedOff1"
		pSS75k.material = "PlasticsSS42"
		l43.Color = rgb (255,128,15)
		l43.ColorFull = rgb (255,128,15)
		l43a.Color = rgb (255,128,15)
		l43a.ColorFull = rgb (255,128,15)
		l43b.Color = rgb (255,128,15)
		l43b.ColorFull = rgb (255,128,15)
		p43.Material = "44BulbOrangeOff1"
		pSS100k.material = "PlasticsSS43"
		l44.Color = rgb (255,255,15)
		l44.ColorFull = rgb (255,255,15)
		l44a.Color = rgb (255,255,15)
		l44a.ColorFull = rgb (255,255,15)
		l44b.Color = rgb (255,255,15)
		l44b.ColorFull = rgb (255,255,15)
		p44.Material = "44BulbYellowOff1"
		pSS200k.material = "PlasticsSS44"
		l45.Color = rgb (15,15,255)
		l45.ColorFull = rgb (15,15,255)
		l45a.Color = rgb (15,15,255)
		l45a.ColorFull = rgb (15,15,255)
		l45b.Color = rgb (15,15,255)
		l45b.ColorFull = rgb (15,15,255)
		p45.Material = "44BulbBlueOff1"
		pSS25k.material = "PlasticsSS45"
End if



'<<<Hottie Mod>>>
If HottieMod = 10 Then
	HottieModType = Int(Rnd*10)
Else
	HottieModType = HottieMod
End If

If HottieModType = 0 Then
	Face.image = "BOPHead_newimageB"
End If

If HottieModType = 1 Then
	Face.image = "Ashley"
End If

If HottieModType = 2 Then
	Face.image = "Brooke"
End If

If HottieModType = 3 Then
	Face.image = "Kelly"
End If

If HottieModType = 4 Then
	Face.image = "Brunette"
End If

If HottieModType = 5 Then
	Face.image = "Meagan"
End If

If HottieModType = 6 Then
	Face.image = "Shelly"
End If

If HottieModType = 7 Then
	Face.image = "Rachel"
End If

If HottieModType = 8 Then
	Face.image = "Valerie"
End If

If HottieModType = 9 Then
	Face.image = "Harley"
End If

End Sub


'Flipper Rubbers Color Mod
If FlipperRubbers = 8 Then
	FlipperRubbersType = RndInt(0,7)
Else
	FlipperRubbersType = FlipperRubbers
End If

If FlipperRubbersType = 0 Then
	batleft.image = "williamsbatwhiteblack" : batright.image = "williamsbatwhiteblack"
	LeftFlipper.elasticity=0.88
	RightFlipper.elasticity=0.88	
End If

If FlipperRubbersType = 1 Then
	batleft.image = "williamsbatwhitered" : batright.image = "williamsbatwhitered"
	LeftFlipper.elasticity=0.92
	RightFlipper.elasticity=0.92	
End If

If FlipperRubbersType = 2 Then
	batleft.image = "williamsbatwhiteblue" : batright.image = "williamsbatwhiteblue"
	LeftFlipper.elasticity=0.94
	RightFlipper.elasticity=0.94	
End If


If FlipperRubbersType = 3 Then
	batleft.image = "williamsbatwhitepurple" : batright.image = "williamsbatwhitepurple"
	LeftFlipper.elasticity=0.94
	RightFlipper.elasticity=0.94	
End If

If FlipperRubbersType = 4 Then
	batleft.image = "williamsbatwhitegreen" : batright.image = "williamsbatwhitegreen"
	LeftFlipper.elasticity=0.94
	RightFlipper.elasticity=0.94	
End If

If FlipperRubbersType = 5 Then
	batleft.image = "williamsbatwhiteorange" : batright.image = "williamsbatwhiteorange"
	LeftFlipper.elasticity=0.94
	RightFlipper.elasticity=0.94	
End If


If FlipperRubbersType = 6 Then
	batleft.image = "williamsbatwhiteBred" : batright.image = "williamsbatwhiteBred"
	LeftFlipper.elasticity=0.94
	RightFlipper.elasticity=0.94	
End If


If FlipperRubbersType = 7 Then
	batleft.image = "williamsbatwhiteyellow" : batright.image = "williamsbatwhiteyellow"
	LeftFlipper.elasticity=0.94
	RightFlipper.elasticity=0.94	
End If





Sub GraphicsTimer()
		'If FlipperRubbers > 0 Then
		' *** move primitive bats ***
		batleft.objrotz = LeftFlipper.CurrentAngle + 1
		batleftshadow.objrotz = batleft.objrotz
		batright.objrotz = RightFlipper.CurrentAngle - 1
		batrightshadow.objrotz  = batright.objrotz
	'End If
End Sub




'Example Usage (Needs to be piggybacked off of a sub that handles the fading, such as nModFlash)
'            Lamp Number     Object      Image Prefix    Number of images (in this case 0 is full off, 13 is full on)
'ModFlashObjm 420,           F20P,       "DomeYellow_",  13

Sub ModFlashObjm(nr, object, imgseq, steps)    'Primitive texture image sequence    'Modified for SolModCallbacks
   dim x, x2, fadex
    Select Case FadingLevel(nr)
        Case 3, 4, 5, 6 'off
           FadeX = ScaleLights(FlashLevel(nr),0 )    'mod for solmodcallbacks
           for x = 0 to steps-1
                if FadeX <= ((x/steps) + ((1/steps)/2 )) then
                    x2 = x
'                    tb3.text = "on " & x & vbnewline & ((x/steps) + ((1/steps)/2 )) & " =? " & x2
                   exit for
                end if
            next
            if    FadeX >= 1-(1/steps) then x2 = steps ': tb3.text = "fullon"
       '    if    FlashLevel(nr) <= (1/steps) then x2 = 0 : tb3.text = "fulloff"
'            tb.text = "flashlevel: " & FlashLevel(nr) & vbnewline & "stepper: " & x2 & vbnewline & "flasherimg: " & x2
'            tb.text = FlashLevel(nr) & vbnewline & imgseq & x2
           object.image = imgseq & x2
    End Select
End Sub




'Dim RedBumper, RedBumperFull, RedBumperI
'RedBumperFull = rgb(255,0,0)
'RedBumper = rgb(255,0,0)
'RedBumperI = 20

Dim BlueBumper, BlueBumperFull, BlueBumperI
BlueBumperFull = rgb(10,10,255)
BlueBumper = rgb(10,10,255)
BlueBumperI = 60

Dim PurpleBumper, PurpleBumperFull, PurpleBumperI
PurpleBumperFull = rgb(150,0,255)
PurpleBumper = rgb(150,0,255)
PurpleBumperI = 60


'<<<Skill Shot Lamp RGB>>>
Dim SSBlueFull, SSBlue, SSBlueI
SSBlueFull = rgb(25,25,255)
SSBlue = rgb(25,25,255)
SSBlueI = 10000

Dim SSYellowFull, SSYellow, SSYellowI
SSYellowFull = rgb(255,255,0)
SSYellow = rgb(255,255,0)
SSYellowI = 10000

Dim SSOrangeFull, SSOrange, SSOrangeI
SSOrangeFull = rgb(255,168,0)
SSOrange = rgb(255,168,0)
SSOrangeI = 10000

Dim SSRedFull, SSRed, SSRedI
SSRedFull = rgb(255,25,25)
SSRed = rgb(255,25,25)
SSRedI = 10000

Dim SSWhiteFull, SSWhite, SSWhiteI
SSWhiteFull = rgb(225,255,220)
SSWhite = rgb(255,255,220)
SSWhiteI = 1000



' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
'(_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_)

''''''''''''Color Changing Helmet Lights
'
'
'
'
''Dim R, G, B
Dim CCGIStep, CCGIStep2, xxGiCC,CCGI2Step
Dim Red1RGB, Green1RGB, Blue1RGB, Red2RGB, Green2RGB, Blue2RGB, Red3RGB, Green3RGB, Blue3RGB
Dim Red4RGB, Green4RGB, Blue4RGB, Red5RGB, Green5RGB, Blue5RGB, Red6RGB, Green6RGB, Blue6RGB
Dim Red7RGB, Green7RGB, Blue7RGB, Red8RGB, Green8RGB, Blue8RGB, Red9RGB, Green9RGB, Blue9RGB
Dim Red1RGBDir, Green1RGBDir, Blue1RGBDir
Dim Red2RGBDir, Green2RGBDir, Blue2RGBDir
Dim Red3RGBDir, Green3RGBDir, Blue3RGBDir
Dim Red4RGBDir, Green4RGBDir, Blue4RGBDir
Dim Red5RGBDir, Green5RGBDir, Blue5RGBDir
Dim Red6RGBDir, Green6RGBDir, Blue6RGBDir
Dim Red7RGBDir, Green7RGBDir, Blue7RGBDir
Dim Red8RGBDir, Green8RGBDir, Blue8RGBDir
Dim Red9RGBDir, Green9RGBDir, Blue9RGBDir



Red1RGB = 255
Green1RGB = 0
Blue1RGB = 0
Red1RGBDir = -1
Green1RGBDir = 1
Blue1RGBDir = 1

Red2RGB = 255
Green2RGB = 127
Blue2RGB = 0
Red2RGBDir = -1
Green2RGBDir = 1
Blue2RGBDir = 1

Red3RGB = 255
Green3RGB = 255
Blue3RGB = 0
Red3RGBDir = -1
Green3RGBDir = -1
Blue3RGBDir = 1

Red4RGB = 0
Green4RGB = 255
Blue4RGB = 0
Red4RGBDir = 1
Green4RGBDir = -1
Blue4RGBDir = 1

Red5RGB = 255
Green5RGB = 255
Blue5RGB = 0
Red5RGBDir = -1
Green5RGBDir = -1
Blue5RGBDir = 1

Red6RGB = 0
Green6RGB = 0
Blue6RGB = 255
Red6RGBDir = 1
Green6RGBDir = 1
Blue6RGBDir = -1

Red7RGB = 127
Green7RGB = 0
Blue7RGB = 255
Red7RGBDir = -1
Green7RGBDir = 1
Blue7RGBDir = -1

Red8RGB = 255
Green8RGB = 0
Blue8RGB = 255
Red8RGBDir = -1
Green8RGBDir = 1
Blue8RGBDir = -1

Red9RGB = 0
Green9RGB = 0
Blue9RGB = 255
Red9RGBDir = 1
Green9RGBDir = 1
Blue9RGBDir = -1






Sub CCHL_timer ()



	If Red1RGB < 1 then Red1RGB = 1:Red1RGBDir = 1 end If
	If Red2RGB < 1 then Red2RGB = 1:Red2RGBDir = 1 end If
	If Red3RGB < 1 then Red3RGB = 1:Red3RGBDir = 1 end If
	If Red4RGB < 1 then Red4RGB = 1:Red4RGBDir = 1 end If
	If Red5RGB < 1 then Red5RGB = 1:Red5RGBDir = 1 end If
	If Red6RGB < 1 then Red6RGB = 1:Red6RGBDir = 1 end If
	If Red7RGB < 1 then Red7RGB = 1:Red7RGBDir = 1 end If
	If Red8RGB < 1 then Red8RGB = 1:Red8RGBDir = 1 end If
	If Red9RGB < 1 then Red9RGB = 1:Red9RGBDir = 1 end If
	If Green1RGB < 1 then Green1RGB = 1:Green1RGBDir = 1 End If
	If Green2RGB < 1 then Green2RGB = 1:Green2RGBDir = 1 End If
	If Green3RGB < 1 then Green3RGB = 1:Green3RGBDir = 1 End If
	If Green4RGB < 1 then Green4RGB = 1:Green4RGBDir = 1 End If
	If Green5RGB < 1 then Green5RGB = 1:Green5RGBDir = 1 End If
	If Green6RGB < 1 then Green6RGB = 1:Green6RGBDir = 1 End If
	If Green7RGB < 1 then Green7RGB = 1:Green7RGBDir = 1 End If
	If Green8RGB < 1 then Green8RGB = 1:Green8RGBDir = 1 End If
	If Green9RGB < 1 then Green9RGB = 1:Green9RGBDir = 1 End If
	If Blue1RGB < 1 then Blue1RGB = 1:Blue1RGBDir = 1 End If
	If Blue2RGB < 1 then Blue2RGB = 1:Blue2RGBDir = 1 End If
	If Blue3RGB < 1 then Blue3RGB = 1:Blue3RGBDir = 1 End If
	If Blue4RGB < 1 then Blue4RGB = 1:Blue4RGBDir = 1 End If
	If Blue5RGB < 1 then Blue5RGB = 1:Blue5RGBDir = 1 End If
	If Blue6RGB < 1 then Blue6RGB = 1:Blue6RGBDir = 1 End If
	If Blue7RGB < 1 then Blue7RGB = 1:Blue7RGBDir = 1 End If
	If Blue8RGB < 1 then Blue8RGB = 1:Blue8RGBDir = 1 End If
	If Blue9RGB < 1 then Blue9RGB = 1:Blue9RGBDir = 1 End If
	If Red1RGB > 254 then Red1RGB = 254:Red1RGBDir = -1 End If
	If Red2RGB > 254 then Red2RGB = 254:Red2RGBDir = -1 End If
	If Red3RGB > 254 then Red3RGB = 254:Red3RGBDir = -1 End If
	If Red4RGB > 254 then Red4RGB = 254:Red4RGBDir = -1 End If
	If Red5RGB > 254 then Red5RGB = 254:Red5RGBDir = -1 End If
	If Red6RGB > 254 then Red6RGB = 254:Red6RGBDir = -1 End If
	If Red7RGB > 254 then Red7RGB = 254:Red7RGBDir = -1 End If
	If Red8RGB > 254 then Red8RGB = 254:Red8RGBDir = -1 End If
	If Red9RGB > 254 then Red9RGB = 254:Red9RGBDir = -1 End If
	If Green1RGB > 254 then Green1RGB = 254:Green1RGBDir = -1 End If
	If Green2RGB > 254 then Green2RGB = 254:Green2RGBDir = -1 End If
	If Green3RGB > 254 then Green3RGB = 254:Green3RGBDir = -1 End If
	If Green4RGB > 254 then Green4RGB = 254:Green4RGBDir = -1 End If
	If Green5RGB > 254 then Green5RGB = 254:Green5RGBDir = -1 End If
	If Green6RGB > 254 then Green6RGB = 254:Green6RGBDir = -1 End If
	If Green7RGB > 254 then Green7RGB = 254:Green7RGBDir = -1 End If
	If Green8RGB > 254 then Green8RGB = 254:Green8RGBDir = -1 End If
	If Green9RGB > 254 then Green9RGB = 254:Green9RGBDir = -1 End If
	If Blue1RGB > 254 then Blue1RGB = 254:Blue1RGBDir = -1 End If
	If Blue2RGB > 254 then Blue2RGB = 254:Blue2RGBDir = -1 End If
	If Blue3RGB > 254 then Blue3RGB = 254:Blue3RGBDir = -1 End If
	If Blue4RGB > 254 then Blue4RGB = 254:Blue4RGBDir = -1 End If
	If Blue5RGB > 254 then Blue5RGB = 254:Blue5RGBDir = -1 End If
	If Blue6RGB > 254 then Blue6RGB = 254:Blue6RGBDir = -1 End If
	If Blue7RGB > 254 then Blue7RGB = 254:Blue7RGBDir = -1 End If
	If Blue8RGB > 254 then Blue8RGB = 254:Blue8RGBDir = -1 End If
	If Blue9RGB > 254 then Blue9RGB = 254:Blue9RGBDir = -1 End If



Red1RGB = Red1RGB + Red1RGBDir
Red2RGB = Red2RGB + Red2RGBDir
Red3RGB = Red3RGB + Red3RGBDir
Red4RGB = Red4RGB + Red4RGBDir
Red5RGB = Red5RGB + Red5RGBDir
Red6RGB = Red6RGB + Red6RGBDir
Red7RGB = Red7RGB + Red7RGBDir
Red8RGB = Red8RGB + Red8RGBDir
Red9RGB = Red9RGB + Red9RGBDir
Green1RGB = Green1RGB + Green1RGBDir
Green2RGB = Green2RGB + Green2RGBDir
Green3RGB = Green3RGB + Green3RGBDir
Green4RGB = Green4RGB + Green4RGBDir
Green5RGB = Green5RGB + Green5RGBDir
Green6RGB = Green6RGB + Green6RGBDir
Green7RGB = Green7RGB + Green7RGBDir
Green8RGB = Green8RGB + Green8RGBDir
Green9RGB = Green9RGB + Green9RGBDir
Blue1RGB = Blue1RGB + Blue1RGBDir
Blue2RGB = Blue2RGB + Blue2RGBDir
Blue3RGB = Blue3RGB + Blue3RGBDir
Blue4RGB = Blue4RGB + Blue4RGBDir
Blue5RGB = Blue5RGB + Blue5RGBDir
Blue6RGB = Blue6RGB + Blue6RGBDir
Blue7RGB = Blue7RGB + Blue7RGBDir
Blue8RGB = Blue8RGB + Blue8RGBDir
Blue9RGB = Blue9RGB + Blue9RGBDir



If HLColorType = 4 Then
	for each xxGiCC in HelmetLightsArr
		xxGiCC.Color = rgb(Red1RGB,Green1RGB,Blue1RGB)
		xxGiCC.ColorFull = rgb(Red1RGB,Green1RGB,Blue1RGB)
	next

	for each xxGiCC in HelmetLightReflArr
		xxGiCC.Color = rgb(Red1RGB,Green1RGB,Blue1RGB)
		xxGiCC.ScaleBulbMesh = 1
		xxGiCC.intensity = 1
	next

End If


for each xxGiCC in HelmetLightReflArr
'	xxGiCC.Color = rgb(0,0,0)
	xxGiCC.ScaleBulbMesh = 1
	xxGiCC.intensity = 1
next

If HLColorType = 5 Then

'			l98.Color = rgb(Red1RGB,Green1RGB,Blue1RGB)
'			l98.ColorFull = rgb(Red1RGB,Green1RGB,Blue1RGB)
'			l98_refl.Color = rgb(Red1RGB,Green1RGB,Blue1RGB)

			l97.Color = rgb(Red2RGB,Green2RGB,Blue2RGB)
			l97.ColorFull = rgb(Red2RGB,Green2RGB,Blue2RGB)
			l97_refl.Color = rgb(Red2RGB,Green2RGB,Blue2RGB)

'			l96.Color = rgb(Red3RGB,Green3RGB,Blue3RGB)
'			l96.ColorFull = rgb(Red3RGB,Green3RGB,Blue3RGB)
'			l96_refl.Color = rgb(Red3RGB,Green3RGB,Blue3RGB)

			l95.Color = rgb(Red4RGB,Green4RGB,Blue4RGB)
			l95_refl.Color = rgb(Red4RGB,Green4RGB,Blue4RGB)
			l95.ColorFull = rgb(Red4RGB,Green4RGB,Blue4RGB)

'			l94.Color = rgb(Red5RGB,Green5RGB,Blue5RGB)
'			l94.ColorFull = rgb(Red5RGB,Green5RGB,Blue5RGB)
'			l94_refl.Color = rgb(Red5RGB,Green5RGB,Blue5RGB)

			l93.Color = rgb(Red6RGB,Green6RGB,Blue6RGB)
			l93.ColorFull = rgb(Red6RGB,Green6RGB,Blue6RGB)
			l93_refl.Color = rgb(Red6RGB,Green6RGB,Blue6RGB)

'			l92.Color = rgb(Red7RGB,Green7RGB,Blue7RGB)
'			l92.ColorFull = rgb(Red7RGB,Green7RGB,Blue7RGB)
'			l92_refl.Color = rgb(Red7RGB,Green7RGB,Blue7RGB)

			l91.Color = rgb(Red8RGB,Green8RGB,Blue8RGB)
			l91.ColorFull = rgb(Red8RGB,Green8RGB,Blue8RGB)
			l91_refl.Color = rgb(Red8RGB,Green8RGB,Blue8RGB)


'			l101.Color = rgb(Red1RGB,Green1RGB,Blue1RGB)
'			l101.ColorFull = rgb(Red1RGB,Green1RGB,Blue1RGB)
'			l101_refl.Color = rgb(Red1RGB,Green1RGB,Blue1RGB)

			l102.Color = rgb(Red2RGB,Green2RGB,Blue2RGB)
			l102.ColorFull = rgb(Red2RGB,Green2RGB,Blue2RGB)
			l102_refl.Color = rgb(Red2RGB,Green2RGB,Blue2RGB)

'			l103.Color = rgb(Red3RGB,Green3RGB,Blue3RGB)
'			l103.ColorFull = rgb(Red3RGB,Green3RGB,Blue3RGB)
'			l103_refl.Color = rgb(Red3RGB,Green3RGB,Blue3RGB)

			l104.Color = rgb(Red4RGB,Green4RGB,Blue4RGB)
			l104.ColorFull = rgb(Red4RGB,Green4RGB,Blue4RGB)
			l104_refl.Color = rgb(Red4RGB,Green4RGB,Blue4RGB)
'
'			l105.Color = rgb(Red5RGB,Green5RGB,Blue5RGB)
'			l105.ColorFull = rgb(Red5RGB,Green5RGB,Blue5RGB)
'			l105_refl.Color = rgb(Red5RGB,Green5RGB,Blue5RGB)

			l106.Color = rgb(Red6RGB,Green6RGB,Blue6RGB)
			l106.ColorFull = rgb(Red6RGB,Green6RGB,Blue6RGB)
			l106_refl.Color = rgb(Red6RGB,Green6RGB,Blue6RGB)

'			l107.Color = rgb(Red7RGB,Green7RGB,Blue7RGB)
'			l107.ColorFull = rgb(Red7RGB,Green7RGB,Blue7RGB)
'			l107_refl.Color = rgb(Red7RGB,Green7RGB,Blue7RGB)

			l108.Color = rgb(Red8RGB,Green8RGB,Blue8RGB)
			l108.ColorFull = rgb(Red8RGB,Green8RGB,Blue8RGB)
			l108_refl.Color = rgb(Red8RGB,Green8RGB,Blue8RGB)
End If

End Sub



''''''''''''' End Color Changing Helmet Lights

'====================
'Class jungle nf
'=============

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
	Sub SetGI(aIdx, ByVal aInput) : if aInput = 8 then aInput = 7 end if : state(aIdx) = aInput/7 : End Sub	'0->8 WPC GI input

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


'Dim shadowopacity
'Shadow.opacity = shadowopacity


Class cvpmMyMech
	Public Sol1, Sol2, MType, Length, Steps, Acc, Ret, InitialPos
	Private mMechNo, mNextSw, mSw(), mLastPos, mLastSpeed, mCallback

	Private Sub Class_Initialize
		ReDim mSw(10)
		gNextMechNo = gNextMechNo + 1 : mMechNo = gNextMechNo : mNextSw = 0 : mLastPos = 0 : mLastSpeed = 0
		MType = 0 : Length = 0 : Steps = 0 : Acc = 0 : Ret = 0 : InitialPos = -1 : vpmTimer.addResetObj Me
	End Sub

	Public Sub AddSw(aSwNo, aStart, aEnd)
		mSw(mNextSw) = Array(aSwNo, aStart, aEnd, 0)
		mNextSw = mNextSw + 1
	End Sub

	Public Sub AddPulseSwNew(aSwNo, aInterval, aStart, aEnd)
		If Controller.Version >= "01200000" Then
			mSw(mNextSw) = Array(aSwNo, aStart, aEnd, aInterval)
		Else
			mSw(mNextSw) = Array(aSwNo, -aInterval, aEnd - aStart + 1, 0)
		End If
		mNextSw = mNextSw + 1
	End Sub

	Public Sub Start
		Dim sw, ii
		With Controller
			.Mech(1) = Sol1 : .Mech(2) = Sol2 : .Mech(3) = Length
			.Mech(4) = Steps : .Mech(5) = MType : .Mech(6) = Acc : .Mech(7) = Ret
			ii = 10
			For Each sw In mSw
				If IsArray(sw) Then
					.Mech(ii) = sw(0) : .Mech(ii+1) = sw(1)
					.Mech(ii+2) = sw(2) : .Mech(ii+3) = sw(3)
					ii = ii + 10
				End If
			Next
			if InitialPos >= 0 then .Mech(8) = InitialPos
			.Mech(0) = mMechNo
		End With
		If IsObject(mCallback) Then mCallBack 0, 0, 0 : mLastPos = 0 : vpmTimer.EnableUpdate Me, True, True  ' <------- Enhances smoothness
	End Sub

	Public Property Get Position : Position = Controller.GetMech(mMechNo) : End Property
	Public Property Get Speed    : Speed = Controller.GetMech(-mMechNo)   : End Property
	Public Property Let Callback(aCallBack) : Set mCallback = aCallBack : End Property

	Public Sub Update
		Dim currPos, speed
		currPos = Controller.GetMech(mMechNo)
		speed = Controller.GetMech(-mMechNo)
		If currPos < 0 Or (mLastPos = currPos And mLastSpeed = speed) Then Exit Sub
		mCallBack currPos, speed, mLastPos : mLastPos = currPos : mLastSpeed = speed
	End Sub

	Public Sub Reset : Start : End Sub
	' Obsolete
	Public Sub AddPulseSw(aSwNo, aInterval, aLength) : AddSw aSwNo, -aInterval, aLength : End Sub
End Class






''''''''''''''''''' Start Color Changing GI

Dim CCRate

CCGI.Interval = CCRate

Dim CustomColorR, CustomColorG, CustomColorB
Dim GIColorRed, GIColorGreen, GIColorBlue, GIColorFullRed, GIColorFullGreen, GIColorFullBlue


CustomColorR = GIColorRed
CustomColorG = GIColorGreen
CustomColorB = GIColorBlue


Dim xxGI

'Add SetGiColor to the init sub
Sub SetGIColor ()

	dim ChosenColor, x

	If GIColorMod = 0 Then
		GIColorModType = Int(Rnd*10)+1
	Else
		GIColorModType = GIColorMod
	End If


	If GIColorModType = 1 then 'Incandescent bulbs
		'AmbientOverhead.color = rgb(255,197,143)
		AmbientOverhead.color = rgb(255,174,102)
	End If

	If GIColorModType = 2 then 'Warm white LEDs
		AmbientOverhead.color = rgb(255,238,142)
	End If

	If GIColorModType = 3 then 'Cool white LEDs
		AmbientOverhead.color = rgb(212,235,255)
	End If

	If GIColorModType = 4 then 'Red LEDs
		AmbientOverhead.color = rgb(255,50,50)
	End If

	If GIColorModType = 5 then 'Orange LEDs
		AmbientOverhead.color = rgb(255,128,0)
	End If

	If GIColorModType = 6 then 'Yellow LEDs
		AmbientOverhead.color = rgb(255,255,50)
	End If

	If GIColorModType = 7 then 'Green LEDs
		AmbientOverhead.color = rgb(50,255,50)
	End If

	If GIColorModType = 8 then 'Blue LEDs
		AmbientOverhead.color = rgb(50,50,255)
	End If


	If GIColorModType = 9 then 'Indigo LEDs
		AmbientOverhead.color = rgb(93,111,211)
	End If

	If GIColorModType = 10 then 'Violet LEDs
		AmbientOverhead.color = rgb(128,50,128)
	End If

	If GIColorModType = 11 then
		AmbientOverhead.color = rgb(CustomColorR,CustomColorG,CustomColorB) 
	End If


	If GIColorModType = 12 then
		CCGI.enabled = true
	End If

	ChosenColor = AmbientOverhead.color

	for each xxGI in GIRearColor
		xxGI.Color = ChosenColor
		xxGI.ColorFull = ChosenColor
		next
	for each xxGI in GiFront
		xxGI.Color = ChosenColor
		xxGI.ColorFull = ChosenColor
	next

	FGIFront.color = ChosenColor
	FGIFront_SR.color = ChosenColor
	FGIFront_SL.color = ChosenColor
	FGIFront_SR_CM.color = ChosenColor
	FGIFront_SL_CM.color = ChosenColor
	FGIRear.color = ChosenColor
	FGIRear_SR.color = ChosenColor
	FGIRear_SL.color = ChosenColor
	FGIRear_SR_CM.color = ChosenColor
	FGIRear_SL_CM.color = ChosenColor

	SetMaterialColor "MetalWallsPrimON", ChosenColor

'	for each x in GIBulbsFront
'		x.materialcolor = ChosenColor
'	Next
'
'	for each x in GIBulbsRear
'		x.materialcolor = ChosenColor
'	Next
'UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
'               OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
'               float elasticity, float elasticityFalloff, float friction, float scatterAngle)

	'UpdateMaterial "44BulbWhiteOff",0,0,0,1,1,1,0.7,ChosenColor,ChosenColor,ChosenColor,False,True,0,0,0,0
	if GIColorMod = 1 Then
		SetMaterialColor "44BulbWhiteOff", rgb(255,177,103)
	else
		SetMaterialColor "44BulbWhiteOff", ChosenColor
	end if
	

End Sub


Sub SetMaterialOpacity(name, new_opacity)
    Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    opacity = new_opacity
    UpdateMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub


Sub SetMaterialColor(name, new_color)
    Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    base = new_color
    UpdateMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub

'Dim R, G, B
'Dim CCGIStep, xxGiCC
Dim RedRGB, GreenRGB, BlueRGB

RedRGB = 255
GreenRGB = 0
BlueRGB = 0




Sub CCGI_timer ()
	If RedRGB < 0 then RedRGB = 0 end If
	If GreenRGB < 0 then GreenRGB = 0 End If
	If BlueRGB < 0 then BlueRGB = 0 End If
	If RedRGB > 255 then RedRGB = 255 End If
	If GreenRGB > 255 then GreenRGB = 255 End If
	If BlueRGB > 255 then BlueRGB = 255 End If
	
	If CCGIStep > 0 and CCGIStep < 255 Then
		GreenRGB = GreenRGB + 1
	End If
		If CCGIStep > 255 and CCGIStep < 510 Then
		RedRGB = RedRGB - 1

	End If
	If CCGIStep > 510 and CCGIStep < 765 Then
		BlueRGB = BlueRGB + 1

	End If
	If CCGIStep > 765 and CCGIStep < 1020 Then
		GreenRGB = GreenRGB - 1

	End If
	If CCGIStep > 1020 and CCGIStep < 1275 Then
		RedRGB = RedRGB + 1

	End If
	If CCGIStep > 1275 and CCGIStep < 1530 Then
		BlueRGB = BlueRGB - 1
	End If

If CCGIStep = 1530 then CCGIStep = 0 End If

CCGIStep = CCGIStep + 1

	for each xxGiCC in GIRearColorArr
			xxGiCC.Color = rgb(RedRGB,GreenRGB,BlueRGB)
			xxGiCC.ColorFull = rgb(RedRGB,GreenRGB,BlueRGB)
			next
	for each xxGiCC in GiFrontArr
			xxGiCC.Color = rgb(RedRGB,GreenRGB,BlueRGB)
			xxGiCC.ColorFull = rgb(RedRGB,GreenRGB,BlueRGB)
			next
			AmbientOverhead.color = rgb(RedRGB,GreenRGB,BlueRGB)
End Sub

''''''''''''''''''' End Color Changing GI




'****************************
'Ball Darkening
'****************************
Sub DarkenBall1_Hit 'Darkens ball in the shooter lane
	activeball.color = RGB(75,75,75)
End Sub
Sub DarkenBall1_Unhit 
	activeball.color = RGB(200,200,200)
End Sub


Sub DarkenBall2_Hit 'Darkens ball in the shooter lane
	activeball.color = RGB(100,100,100)
End Sub
Sub DarkenBall2_Unhit 
	activeball.color = RGB(200,200,200)
End Sub


Sub DarkenBall3_Hit 'Darkens ball in the shooter lane
	activeball.color = RGB(75,75,75)
End Sub
Sub DarkenBall3_Unhit 
	activeball.color = RGB(200,200,200)
End Sub
'
'
Sub DarkenBall4_Hit 'Darkens ball in the shooter lane
	activeball.color = RGB(125,125,125)
End Sub
Sub DarkenBall4_Unhit 
	activeball.color = RGB(200,200,200)
End Sub
'
'
'Sub DarkenBall5_Hit 'Darkens ball in the shooter lane
'	activeball.color = RGB(45,45,45)
'End Sub
'Sub DarkenBall5_Unhit 
'	activeball.color = RGB(200,200,200)
'End Sub 

Sub DarkenBall6_Hit 'Darkens ball in the shooter lane
	activeball.color = RGB(80,80,80)
End Sub
Sub DarkenBall6_Unhit 
	activeball.color = RGB(200,200,200)
End Sub 





'////////////////////////////////////////////////////////////////////////////////
'////          Mechanical Sounds, by Fleep                                   ////
'////                     Last Updated: January, 2022                        ////
'////////////////////////////////////////////////////////////////////////////////
'
'/////////////////////////////////  CARTRIDGES  /////////////////////////////////
'
'//  Specify which mechanical sound cartridge to use for each group of elements.
'//  Mechanical sounds naming convention: <CARTRIDGE>_<Soundset_Name>
'//
'//  Cartridge name is composed using the following convention:
'//  <TABLE MANUFACTURER ABBREVIATION>_<TABLE NAME ABBREVIATION>_<SOUNDSET REVISION NUMBER>
'//  
'//  General Mechanical Sounds Cartridges:
Const Cartridge_Bumpers					= "WS_PBT_REV01" 'Williams Pinbot Cartridge REV01
Const Cartridge_Slingshots				= "WS_PBT_REV01" 'Williams Pinbot Cartridge REV01
Const Cartridge_Flippers				= "WS_PBT_REV01" 'Williams Pinbot Cartridge REV01
Const Cartridge_Kickers					= "WS_WHD_REV01"
Const Cartridge_Diverters				= "WS_DNR_REV01" 'Williams Diner Cartridge REV01
Const Cartridge_Knocker					= "WS_WHD_REV02" 'Williams Whirlwind Cartridge REV02
Const Cartridge_Relays					= "WS_WHD_REV01"
Const Cartridge_Trough					= "WS_WHD_REV01"
Const Cartridge_Rollovers				= "WS_WHD_REV01"
Const Cartridge_Targets					= "WS_WHD_REV01"
Const Cartridge_Gates					= "WS_WHD_REV01"
Const Cartridge_Spinner					= "SY_TNA_REV01" 'Spooky Total Nuclear Annihilation Cartridge REV01
Const Cartridge_Rubber_Hits				= "WS_WHD_REV01"
Const Cartridge_Metal_Hits				= "WS_WHD_REV01"
Const Cartridge_Plastic_Hits			= "WS_WHD_REV01"
Const Cartridge_Wood_Hits				= "WS_WHD_REV01"
Const Cartridge_Cabinet_Sounds			= "WS_WHD_REV01"
Const Cartridge_Drain					= "WS_WHD_REV01"
Const Cartridge_Apron					= "WS_WHD_REV01"
Const Cartridge_Ball_Roll				= "BY_TOM_REV01" 'Bally Theatre of Magic Cartridge REV01
Const Cartridge_BallBallCollision		= "BY_WDT_REV01" 'Bally WHO Dunnit Cartridge REV01
Const Cartridge_Ball_Drop_Bump			= "WS_WHD_REV01"
Const Cartridge_Plastic_Ramps			= "WS_WHD_REV01"
Const Cartridge_Metal_Ramps				= "WS_WHD_REV01"
Const Cartridge_Ball_Guides				= "WS_WHD_REV01"
Const Cartridge_Table_Specifics			= "WS_WHD_REV01"


'////////////////////////////  SOUND SOURCE CREDITS  ////////////////////////////
'//  Special thanks go to the following contributors who have provided audio 
'//  footage recordings:
'//  
'//  Williams Whirlwind - Blackmoor, wrd1972
'//  Williams Diner - Nick Rusis
'//  Spooky Total Nuclear Annihilation - WildDogArcade, Ed and Gary
'//  Bally Theatre of Magic - CalleV, nickbuol
'//  Bally WHO Dunnit - Amazaley1
'//  Williams Pinbot - major_drain_pinball


'///////////////////////////////  USER PARAMETERS  //////////////////////////////
'
'//  Sounds Parameter with suffix "SoundLevel" can have any value in range [0..1]
'//  Sounds Parameter with suffix "SoundMultiplier" can have any value


'///////////////////////////  SOLENOIDS (COILS) CONFIG  /////////////////////////

'//  FLIPPER COILS:
'//  Flippers in this table: Lower Left Flipper, Lower Right Flipper, Upper Right Fliiper
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel
Dim FlipperLeftLowerHitParm, FlipperRightUpperHitParm, FlipperRightLowerHitParm

'//  Flipper Up Attacks initialize during playsound subs
Dim FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel


FlipperUpSoundLevel = 1
FlipperDownSoundLevel = 0.65
FlipperUpAttackMinimumSoundLevel = 0.010
FlipperUpAttackMaximumSoundLevel = 0.435


'//  Flipper Hit Param initialize with FlipperUpSoundLevel
'//  and dynamically modified calculated by ball flipper collision
FlipperLeftLowerHitParm = FlipperUpSoundLevel
FlipperRightLowerHitParm = FlipperUpSoundLevel
FlipperRightUpperHitParm = FlipperUpSoundLevel


'//  CONTROLLED / SWITCHED COILS:
'//  Solenoid 01A = Outhole Kicker
'//  Solenoid 02A = Shooter Feeder
'//  Solenoid 03A = Right Ramp Lifter
'//  Solenoid 04A = Left Locking Kickback
'//  Solenoid 05A = Top Eject
'//  Solenoid 06A = Knocker
'//  Solenoid 07A,08A = 3-Bank Drop Target Reset, 1-Bank Drop Target Reset
'//  Solenoid 13 = Diverter
'//  Solenoid 14 = Under Playfield Kickbig
'//  Solenoid 09,10,15,17,19,21 = 3 Lower Bumpers, 3 Upper Bumpers
'//  Solenoid 18,20 = Left Kicker (Slingshot), Right Kicker (Slingshot)
'//  Solenoid 22 = Right Ramp Down

Dim Solenoid_OutholeKicker_SoundLevel, Solenoid_ShooterFeeder_SoundLevel
Dim Solenoid_RightRampLifter_SoundLevel, Solenoid_LeftLockingKickback_SoundLevel
Dim Solenoid_TopEject_SoundLevel, Solenoid_Knocker_SoundLevel, Solenoid_DropTargetReset_SoundLevel
Dim Solenoid_Diverter_Enabled_SoundLevel, Solenoid_Diverter_Hold_SoundLevel, Solenoid_Diverter_Disabled_SoundLevel
Dim Solenoid_UnderPlayfieldKickbig_SoundLevel, Solenoid_Bumper_SoundMultiplier
Dim Solenoid_Slingshot_SoundLevel, Solenoid_RightRampDown_SoundLevel, AutoPlungerSoundLevel

AutoPlungerSoundLevel = 1												'volume level; range [0, 1]
Solenoid_OutholeKicker_SoundLevel = 1
Solenoid_ShooterFeeder_SoundLevel = 1
Solenoid_RightRampLifter_SoundLevel = 0.3
Solenoid_RightRampDown_SoundLevel = 0.3
Solenoid_LeftLockingKickback_SoundLevel = 1
Solenoid_TopEject_SoundLevel = 1
Solenoid_Knocker_SoundLevel = 1
Solenoid_DropTargetReset_SoundLevel = 1
Solenoid_Diverter_Enabled_SoundLevel = 1
Solenoid_Diverter_Hold_SoundLevel = 0.7
Solenoid_Diverter_Disabled_SoundLevel = 0.4
Solenoid_UnderPlayfieldKickbig_SoundLevel = 1
Solenoid_Bumper_SoundMultiplier = 0.004 '8
Solenoid_Slingshot_SoundLevel = 1


'//  RELAYS:
'//  Solenoid 16 = Lower Playfield Relay GI Relay (P/N 5580-12145-00) / Backbox GI Relay (P/N 5580-09555-01)
'//  Solenoid 11 = Upper Playfield Relay GI Relay (P/N 5580-12145-00)
'//  Solenoid 12 = Solenoid A/C Select Relay (5580-09555-01)
'//  Fake Solenoid = Flahser Relay

Dim RelayLowerGISoundLevel, RelayUpperGISoundLevel, RelaySolenoidACSelectSoundLevel, RelayFlasherSoundLevel
RelayLowerGISoundLevel = 0.45
RelayUpperGISoundLevel = 0.45
RelaySolenoidACSelectSoundLevel = 0.3
RelayFlasherSoundLevel = 0.015


'//  EXTRA SOLENOIDS:
'//  Solenoid 24 = Blower Motor (Ontop Backbox)
'//  Solenoid 27 = Spin Wheels Motor
Dim Solenoid_BlowerMotor_SoundLevel, Solenoid_SpinWheelsMotor_SoundLevel
Solenoid_BlowerMotor_SoundLevel = 0.2
Solenoid_SpinWheelsMotor_SoundLevel = 0.2


'////////////////////////////  SWITCHES SOUND CONFIG  ///////////////////////////
Dim Switch_Gate_SoundLevel, SpinnerSoundLevel, RolloverSoundLevel, OutLaneRolloverSoundLevel, TargetSoundFactor

Switch_Gate_SoundLevel = 1	
SpinnerSoundLevel = 0.1
RolloverSoundLevel = 0.55
OutLaneRolloverSoundLevel = 0.8
TargetSoundFactor = 0.8


'////////////////////  BALL HITS, BUMPS, DROPS SOUND CONFIG  ////////////////////
Dim BallWithBallCollisionSoundFactor, BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor
Dim WallImpactSoundFactor, MetalImpactSoundFactor, WireformAntiRebountRailSoundFactor
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor
Dim BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor, OutlaneWallsSoundFactor
Dim EjectBallBumpSoundLevel, HeadSaucerSoundLevel, EjectHoleEnterSoundLevel
Dim RightRampMetalWireDropToPlayfieldSoundLevel, LeftPlasticRampDropToLockSoundLevel, LeftPlasticRampDropToPlayfieldSoundLevel
Dim CellarLeftEnterSoundLevel, CellarRightEnterSoundLevel, CellerKickouBallDroptSoundLevel


BallWithBallCollisionSoundFactor = 3.2
BallBouncePlayfieldSoftFactor = 0.0015
BallBouncePlayfieldHardFactor = 0.0075
WallImpactSoundFactor = 0.075
MetalImpactSoundFactor = 0.075
RubberStrongSoundFactor = 0.045
RubberWeakSoundFactor = 0.055
RubberFlipperSoundFactor = 0.65
BottomArchBallGuideSoundFactor = 0.2
FlipperBallGuideSoundFactor = 0.015
WireformAntiRebountRailSoundFactor = 0.04
OutlaneWallsSoundFactor = 1
EjectBallBumpSoundLevel = 1
RightRampMetalWireDropToPlayfieldSoundLevel = 1
LeftPlasticRampDropToLockSoundLevel = 1
LeftPlasticRampDropToPlayfieldSoundLevel = 1
EjectHoleEnterSoundLevel = 0.75
HeadSaucerSoundLevel = 0.15
CellerKickouBallDroptSoundLevel = 1
CellarLeftEnterSoundLevel = 0.85
CellarRightEnterSoundLevel = 0.85


'///////////////////////  OTHER PLAYFIELD ELEMENTS CONFIG  //////////////////////
Dim RollingSoundFactor, RollingOnDiscSoundFactor, BallReleaseShooterLaneSoundLevel
Dim LeftPlasticRampEnteranceSoundLevel, RightPlasticRampEnteranceSoundLevel
Dim LeftPlasticRampRollSoundFactor, RightPlasticRampRollSoundFactor
Dim LeftMetalWireRampRollSoundFactor, RightPlasticRampHitsSoundLevel, LeftPlasticRampHitsSoundLevel
Dim SpinningDiscRolloverSoundFactor, SpinningDiscRolloverBumpSoundLevel
Dim LaneSoundFactor, LaneEnterSoundFactor, InnerLaneSoundFactor
Dim LaneLoudImpactMinimumSoundLevel, LaneLoudImpactMaximumSoundLevel

RollingSoundFactor = 50
RollingOnDiscSoundFactor = 1.5
BallReleaseShooterLaneSoundLevel = 1
LeftPlasticRampEnteranceSoundLevel = 0.1
RightPlasticRampEnteranceSoundLevel = 0.1
LeftPlasticRampRollSoundFactor = 0.2
RightPlasticRampRollSoundFactor = 0.2
LeftMetalWireRampRollSoundFactor = 1
RightPlasticRampHitsSoundLevel = 1
LeftPlasticRampHitsSoundLevel = 1
SpinningDiscRolloverSoundFactor = 0.05
SpinningDiscRolloverBumpSoundLevel = 0.3
LaneEnterSoundFactor = 0.9
InnerLaneSoundFactor = 0.0005
LaneSoundFactor = 0.0004
LaneLoudImpactMinimumSoundLevel = 0
LaneLoudImpactMaximumSoundLevel = 0.4


'///////////////////////////  CABINET SOUND PARAMETERS  /////////////////////////
Dim NudgeLeftSoundLevel, NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel
Dim PlungerReleaseSoundLevel, PlungerPullSoundLevel, CoinSoundLevel

NudgeLeftSoundLevel = 1
NudgeRightSoundLevel = 1
NudgeCenterSoundLevel = 1
StartButtonSoundLevel = 0.1
PlungerReleaseSoundLevel = 1
PlungerPullSoundLevel = 1
CoinSoundLevel = 1


'///////////////////////////  MISC SOUND PARAMETERS  ////////////////////////////
Dim LutToggleSoundLevel : 
LutToggleSoundLevel = 0.5


'////////////////////////////////  SOUND HELPERS  ///////////////////////////////
Dim SoundOn : SoundOn = 1
Dim SoundOff : SoundOff = 0
Dim Up : Up = 0
Dim Down : Down = 1
Dim RampUp : RampUp = 1
Dim RampDown : RampDown = 0
Dim RampDownSlow : RampDownSlow = 1
Dim RampDownFast : RampDownFast = 2
Dim CircuitA : CircuitA = 0
Dim CircuitC : CircuitC = 1

'//  Helper for Main (Lower) flippers dampened stroke
Dim BallNearLF : BallNearLF = 0
Dim BallNearRF : BallNearRF = 0

Sub TriggerBallNearLF_Hit()
	'Debug.Print "BallNearLF = 1"
	BallNearLF = 1
End Sub

Sub TriggerBallNearLF_UnHit()
	'Debug.Print "BallNearLF = 0"
	BallNearLF = 0
End Sub

Sub TriggerBallNearRF_Hit()
	'Debug.Print "BallNearRF = 1"
	BallNearRF = 1
End Sub

Sub TriggerBallNearRF_UnHit()
	'Debug.Print "BallNearLF = 0"
	BallNearRF = 0
End Sub


'///////////////////////  SOUND PLAYBACK SUBS / FUNCTIONS  //////////////////////
'//////////////////////  POSITIONAL SOUND PLAYBACK METHODS  /////////////////////
'//  Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
'//  These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
'//  For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
'//  For stereo setup - positional sound playback functions will only pan between left and right channels
'//  For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels
'//  
'//  PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
'//  Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStaticLoop(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
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


'//////////////////////  SUPPORTING BALL & SOUND FUNCTIONS  /////////////////////
'//  Fades between front and back of the table 
'//  (for surround systems or 2x2 speakers, etc), depending on the Y position 
'//  on the table.
Function AudioFade(tableobj)
	Dim tmp
	Select Case PositionalSoundPlaybackConfiguration
		Case 1
			AudioFade = 0
		Case 2
			AudioFade = 0
		Case 3
			tmp = tableobj.y * 2 / tableheight-1
			If tmp > 0 Then
				AudioFade = Csng(tmp ^10)
			Else
				AudioFade = Csng(-((- tmp) ^10) )
			End If
	End Select
End Function

'//  Calculates the pan for a tableobj based on the X position on the table.
Function AudioPan(tableobj)
	Dim tmp
	Select Case PositionalSoundPlaybackConfiguration
		Case 1
			AudioPan = 0
		Case 2
			tmp = tableobj.x * 2 / tablewidth-1
			If tmp > 0 Then
				AudioPan = Csng(tmp ^10)
			Else
				AudioPan = Csng(-((- tmp) ^10) )
			End If
		Case 3
			tmp = tableobj.x * 2 / tablewidth-1

			If tmp > 0 Then
				AudioPan = Csng(tmp ^10)
			Else
				AudioPan = Csng(-((- tmp) ^10) )
			End If
	End Select
End Function

'//  Calculates the volume of the sound based on the ball speed
Function Vol(ball)
	Vol = Csng(BallVel(ball) ^2)
End Function

'//  Calculates the pitch of the sound based on the ball speed
Function Pitch(ball)
    Pitch = BallVel(ball) * 20
End Function

'//  Calculates the ball speed
Function BallVel(ball)
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'//  Calculates the roll volume of the sound based on the ball speed
Dim TempBallVel
Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	TempBallVel = Csng((INT(SQR((ball.VelX^2)+(ball.VelY^2))))/RollingSoundFactor)
	If TempBallVel = 1 Then TempBallVel = 0.999
	If TempBallVel = 0 Then TempBallVel = 0.001
	'debug.print TempBallVel
	TempBallVel = Csng(1/(1+(0.275*(((0.75*TempBallVel)/(1-TempBallVel))^(-2)))))
	VolPlayfieldRoll = TempBallVel
End Function

'//  Calculates the roll volume of the sound based on the ball speed
Function VolSpinningDiscRoll(ball)
	VolSpinningDiscRoll = RollingOnDiscSoundFactor * 0.1 * Csng(BallVel(ball) ^3)
End Function

'//  Calculates the roll volume of the sound based on the ball speed
Dim TempBallVelPlastic
Function VolPlasticMetalRampRoll(ball)
	'VolPlasticMetalRampRoll = RollingOnDiscSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
	TempBallVelPlastic = Csng((INT(SQR((ball.VelX^2)+(ball.VelY^2))))/RollingSoundFactor)
	If TempBallVelPlastic = 1 Then TempBallVelPlastic = 0.999
	If TempBallVelPlastic = 0 Then TempBallVelPlastic = 0.001
	'debug.print TempBallVel
	TempBallVelPlastic = Csng(1/(1+(0.275*(((0.75*TempBallVelPlastic)/(1-TempBallVelPlastic))^(-2)))))
	VolPlasticMetalRampRoll = TempBallVelPlastic



End Function

'//  Calculates the roll pitch of the sound based on the ball speed
Dim TempPitchBallVel
Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	'PitchPlayfieldRoll = BallVel(ball) ^2 * 15
	'PitchPlayfieldRoll = Csng(BallVel(ball))/50 * 10000
	'PitchPlayfieldRoll = (1-((Csng(BallVel(ball))/50)^0.2)) * 20000

	'PitchPlayfieldRoll = (2*((Csng(BallVel(ball)))^0.7))/(2+(Csng(BallVel(ball)))) * 16000
	TempPitchBallVel = Csng((INT(SQR((ball.VelX^2)+(ball.VelY^2))))/50)
	If TempPitchBallVel = 1 Then TempPitchBallVel = 0.999
	If TempPitchBallVel = 0 Then TempPitchBallVel = 0.001
	TempPitchBallVel = Csng(1/(1+(0.275*(((0.75*TempPitchBallVel)/(1-TempPitchBallVel))^(-2))))) * 10000
	PitchPlayfieldRoll = TempPitchBallVel
End Function

'//  Calculates the pitch of the sound based on the ball speed.
'//  Used for plastic ramps roll sound
Function PitchPlasticRamp(ball)
    PitchPlasticRamp = BallVel(ball) * 20
End Function

'//  Determines if a Points (px,py) is inside a 4 point polygon A-D
'//  in Clockwise/CCW order
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

'//  Determines if a point (px,py) in inside a circle with a center of 
'//  (cx,cy) coordinates and circleradius
Function InCircle(px,py,cx,cy,circleradius)
	Dim distance
	distance = SQR(((px-cx)^2) + ((py-cy)^2))
	
	If (distance < circleradius) Then
		InCircle = True
	Else
		InCircle = False
	End If
End Function

'///////////////////////////  PLAY SOUNDS SUBROUTINES  //////////////////////////
'//  
'//  These Subroutines implement all mechanical playsounds including timers
'//  
'//////////////////////////  GENERAL SOUND SUBROUTINES  /////////////////////////
Sub SoundStartButton()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Start_Button"), StartButtonSoundLevel, StartButtonPosition
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Plunger_Pull_Slow"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerPullStop()
	StopSound Cartridge_Cabinet_Sounds & "_Plunger_Pull_Slow"
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Plunger_Release_Ball_" & Int(Rnd*3)+1), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Plunger_Release_Empty"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*3)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*3)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySoundAtLevelStatic ("Nudge_" & Int(Rnd*3)+1), NudgeCenterSoundLevel * VolumeDial, drain
End Sub



'//////  JP'S VP10 ROLLING SOUNDS & FLEEP RAMPS / SPINNING DISC ROLLOVER  ///////
Const tnob = 4 ' total number of balls
ReDim rolling(tnob)
ReDim ramprolling(tnob)
Dim DropCount
ReDim DropCount(tnob)


InitRolling




Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
		ramprolling(i) = False
		DropCount(i) = 0
    Next
End Sub

Sub RollingSoundUpdate()
	Dim b
	
	For b = 0 to UBound(BOT)
			If BOT(b).z < 27 and BOT(b).z > 23 Then
				' play the rolling sound for each ball
				If BallVel(BOT(b) ) > 0.01 Then
					rolling(b) = True
					PlaySound (Cartridge_Ball_Roll & "_Ball_Roll_" & b), -1, VolPlayfieldRoll(BOT(b)) * PlayfieldRollVolumeDial * VolumeDial * 1.25, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
					
				End If
			Else
				If BOT(b).z > 46 Then
					'desided to swap left and right, as whirlwind has similar shapes on ramps but different sides
					'left sound is played on right and viceversa...
					' Play the Right plastic ramp rolling sound for each ball
					' LEFT RAMP inrect LeftRampSound
					If BallVel(BOT(b) ) > 1 and InRect(BOT(b).x, BOT(b).y, 0,75,350,75,250,870,0,1100) Then
						ramprolling(b) = True
						PlaySound (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_BallRoll_" & b), 0, (VolPlasticMetalRampRoll(BOT(b))) * RightPlasticRampRollSoundFactor * RampsRollVolumeDial * VolumeDial * 1.25, AudioPan(BOT(b)), 0, 0, 1, 0, AudioFade(BOT(b))
						'Debug.Print Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_BallRoll_" & b		
					
					End If

					' Play the left ramp - plastic part sound for each ball
					' RIGHT RAMP inrect RightRampSound
					If BallVel(BOT(b) ) > 1 and InRect(BOT(b).x, BOT(b).y, 440,650,850,600,850,1700,680,1700) Then
						ramprolling(b) = True
						StopSound (Cartridge_Ball_Roll & "_Ball_Roll_" & b)
						PlaySound (Cartridge_Plastic_Ramps & "_Ramp_Left_Plastic_BallRoll_" & b), 0, (VolPlasticMetalRampRoll(BOT(b))) * LeftPlasticRampRollSoundFactor * RampsRollVolumeDial * VolumeDial * 1.25, AudioPan(BOT(b)), 0, 0, 1, 0, AudioFade(BOT(b))
						'Debug.Print Cartridge_Plastic_Ramps & "_Ramp_Left_Plastic_BallRoll_" & b		
					End If

					' Play the wire ramp - wire part sound for each ball
					' WireRampSound and SkillRampSound and not in WireRampSoundOff
					If BallVel(BOT(b) ) > 1 and (InRect(BOT(b).x, BOT(b).y, 100,1100,430,650,420,1030,100,1600) Or InRect(BOT(b).x, BOT(b).y, 860,1180,940,1180,940,1750,860,1750)) Then
						ramprolling(b) = True
						'StopSound (Cartridge_Metal_Ramps & "_Ramp_Left_Plastic_BallRoll_" & b)
						PlaySound (Cartridge_Metal_Ramps & "_Ramp_Left_Metal_Wire_BallRoll_" & b), 0, (VolPlasticMetalRampRoll(BOT(b))) * LeftMetalWireRampRollSoundFactor * RampsRollVolumeDial * VolumeDial * 1.25, AudioPan(BOT(b)), 0, 0, 1, 0, AudioFade(BOT(b))
						'Debug.Print Cartridge_Metal_Ramps & "_Ramp_Left_Metal_Wire_BallRoll_" & b
					End If

					' Upper PF - UpperPFSound - using PF rolling sound for now..
					If BallVel(BOT(b) ) > 0.01 and InRect(BOT(b).x, BOT(b).y, 250,75,940,75,940,600,250,600) Then
						ramprolling(b) = True
						StopSound (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_BallRoll_" & b)
						PlaySound (Cartridge_Ball_Roll & "_Ball_Roll_" & b), -1, VolPlayfieldRoll(BOT(b)) * PlayfieldRollVolumeDial * VolumeDial * 1.25, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
					End If

					'BallLeftEye, BallRightEye, BallMouth
					if Not IsEmpty(BallMouth) Or Not IsEmpty(BallLeftEye) Or Not IsEmpty(BallRightEye) Or InRect(BOT(b).x, BOT(b).y, 360,730,420,730,420,875,360,875) then
						ramprolling(b) = False
						StopSound (Cartridge_Plastic_Ramps & "_Ramp_Left_Plastic_BallRoll_" & b)
						StopSound (Cartridge_Metal_Ramps & "_Ramp_Left_Metal_Wire_BallRoll_" & b)
						StopSound (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_BallRoll_" & b)
						StopSound (Cartridge_Ball_Roll & "_Ball_Roll_" & b)
					end if
				Else
					If ramprolling(b) = True Then
						ramprolling(b) = False
						StopSound (Cartridge_Plastic_Ramps & "_Ramp_Left_Plastic_BallRoll_" & b)
						StopSound (Cartridge_Metal_Ramps & "_Ramp_Left_Metal_Wire_BallRoll_" & b)
						StopSound (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_BallRoll_" & b)
						StopSound (Cartridge_Ball_Roll & "_Ball_Roll_" & b)
					End If
				End If

				'***Ball Drop Sounds***
				If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
'					debug.print "ball drop velz: " & BOT(b).velz
					If DropCount(b) >= 5 Then
						If BOT(b).velz > -5 Then
							If BOT(b).z < 35 Then
'								debug.print "--> random sound soft: " & BOT(b).velz
								DropCount(b) = 0
								RandomSoundBallBouncePlayfieldSoft BOT(b)
							end if
						Else
'							debug.print "--> random sound hard: " & BOT(b).velz
							DropCount(b) = 0
							RandomSoundBallBouncePlayfieldHard BOT(b)
						End If				
					End If

				End If

				If DropCount(b) < 5 Then
					DropCount(b) = DropCount(b) + 1
				End If
			End If
			
			If rolling(b) = True and (BallVel(BOT(b) ) <= 1 or BOT(B).z < 23 or BOT(b).z > 27) Then			
				StopSound (Cartridge_Ball_Roll & "_Ball_Roll_" & b)
				rolling(b) = False
'				If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0 'todo what is this??
			End If
			
			
			If ramprolling(b) = True and BOT(b).z <= 46 Then
				ramprolling(b) = False
'				debug.print "stop sounds"
				StopSound (Cartridge_Plastic_Ramps & "_Ramp_Left_Plastic_BallRoll_" & b)
				StopSound (Cartridge_Metal_Ramps & "_Ramp_Left_Metal_Wire_BallRoll_" & b)
				StopSound (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_BallRoll_" & b)
				StopSound (Cartridge_Ball_Roll & "_Ball_Roll_" & b)	'this may break something...
			End If

			' "Static" Ball Shadows
			' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
'			If AmbientBallShadowOn = 0 Then
'				If BOT(b).Z > 30 Then
'					BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
'				Else
'					BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
'				End If
'				BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
'				BallShadowA(b).X = BOT(b).X
'				BallShadowA(b).visible = 1
'			End If

	Next
End Sub



'///////////////////////  JP'S VP10 BALL COLLISION SOUND  ///////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	if abs(ball1.vely) < 1 And abs(ball2.vely) < 1 And InRect(ball1.x, ball1.y, 360,730,420,730,420,875,360,875) then
		exit sub	'don't rattle the locked balls
	end if
	PlaySound (Cartridge_BallBallCollision & "_BallBall_Collide_" & Int(Rnd*7)+1), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub



'///////////////////////////////  CELLAR SOUNDS  ///////////////////////////////
Sub SoundCellarLeftEnter()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Scoop_Left_Cellar_Enter_" & Int(Rnd*4)+1), CellarLeftEnterSoundLevel * finalspeed/40, CellarLeftTrigger
End Sub

Sub SoundCellarRightEnter()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Scoop_Right_Cellar_Enter_" & Int(Rnd*5)+1), CellarRightEnterSoundLevel * finalspeed/40, CellarRightTrigger
End Sub


Sub SoundCellerKickout()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Kickers & "_Scoop_Kickout_Cellar_" & Int(Rnd*3)+1,DOFContactors), Solenoid_UnderPlayfieldKickbig_SoundLevel, ScoopKickerOverflow
End Sub

Sub SoundCellarKickoutBallDrop()
	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Scoop_BallDrop_After_Kickout_" & Int(Rnd*2)+1), CellerKickouBallDroptSoundLevel, ScoopKickerOverflow
End Sub



'///////////////////////////  GENERAL ROLLOVER SOUNDS  //////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall (Cartridge_Rollovers & "_Rollover_All_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub



'///////////////////////////  OUTLANE ROLLOVER SOUNDS  //////////////////////////
Sub RandomSoundOutlaneRollover()
	PlaySoundAtLevelActiveBall (Cartridge_Rollovers & "_Rollover_Outlane_" & Int(Rnd*4)+1), OutLaneRolloverSoundLevel
End Sub



'////////////////////  BALL GATES AND BRACKET GATES SOUNDS  /////////////////////
'Sub SoundRampBrktGate1(toggle)
'	If toggle = SoundOn Then
'		PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_1"), Switch_Gate_SoundLevel * 0.005 , sw33g
'	End If
'	If toggle = SoundOff Then
'		Stopsound Cartridge_Gates & "_Bracket_Gate_1"
'	End If
'End Sub
'
'Sub SoundRampBrktGate2(toggle)
'	If toggle = SoundOn Then
'		PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_2"), Switch_Gate_SoundLevel * 0.005, sw40g
'	End If
'	If toggle = SoundOff Then
'		Stopsound Cartridge_Gates & "_Bracket_Gate_2"
'	End If
'End Sub


Sub SoundBallGate0()
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_1"), Switch_Gate_SoundLevel * 0.005, Gate0
End Sub

Sub SoundBallGate1()
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_2"), Switch_Gate_SoundLevel * 0.005, Gate1
End Sub

Sub SoundBallGate2()
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_2"), Switch_Gate_SoundLevel * 0.005, Gate2
End Sub

Sub SoundBallGate3()	'sound of blocked gate
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Oneway_Ball_Gate_" & Int(Rnd*3)+1), Switch_Gate_SoundLevel, Gate3
End Sub

Sub SoundBallGate4()
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_1"), Switch_Gate_SoundLevel * 0.005, Gate4
End Sub

Sub SoundBallGate5(toggle)
	If toggle = SoundOn Then
		PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_2"), Switch_Gate_SoundLevel * 0.005, Gate5
	End If
	If toggle = SoundOff Then
		Stopsound Cartridge_Gates & "_Bracket_Gate_2"
		PlaySoundAtLevelStatic (Cartridge_Gates & "_Oneway_Ball_Gate_" & Int(Rnd*3)+1), Switch_Gate_SoundLevel * 0.005, Gate5
	End If
End Sub

Sub SoundBallGate6()
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_1"), Switch_Gate_SoundLevel * 0.005, Gate6
End Sub

Sub SoundBallGate8(toggle)
	If toggle = SoundOn Then
		PlaySoundAtLevelStatic ("TOM_Small_Gate_2"), Switch_Gate_SoundLevel * 0.02, Gate8
	End If
	If toggle = SoundOff Then
		Stopsound "TOM_Small_Gate_2"
		PlaySoundAtLevelStatic ("WS_WHD_REV01_Oneway_Ball_Gate_3"), Switch_Gate_SoundLevel * 0.002, Gate8
	End If
End Sub



Sub SoundBallReleaseGate()
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Oneway_Ball_Gate_" & Int(Rnd*3)+1), Switch_Gate_SoundLevel * 0.5, BallReleaseGate
End Sub



'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////
Sub SoundDropTargetsw26()
	PlaySoundAtLevelStatic (Cartridge_Targets & "_Drop_Target_1Bank_Release_Down_" & Int(Rnd*2)+1), Vol(ActiveBall) * TargetSoundFactor, sw26
End Sub

Sub SoundDropTargetsw27()
	PlaySoundAtLevelStatic (Cartridge_Targets & "_Drop_Target_3Bank_Release_Down_" & Int(Rnd*6)+1), Vol(ActiveBall) * TargetSoundFactor, sw27
End Sub

Sub SoundDropTargetsw28()
	PlaySoundAtLevelStatic (Cartridge_Targets & "_Drop_Target_3Bank_Release_Down_" & Int(Rnd*6)+1), Vol(ActiveBall) * TargetSoundFactor, sw28
End Sub

Sub SoundDropTargetsw29()
	PlaySoundAtLevelStatic (Cartridge_Targets & "_Drop_Target_3Bank_Release_Down_" & Int(Rnd*6)+1), Vol(ActiveBall) * TargetSoundFactor, sw29
End Sub



'/////////////////////  DROP TARGET RESET SOLENOID SOUNDS  //////////////////////
Sub RandomSoundDropTargetTop()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Targets & "_Drop_Target_1Bank_Reset_Up_" & Int(Rnd*8)+1,DOFContactors), Solenoid_DropTargetReset_SoundLevel, sw26
End Sub

Sub RandomSoundDropTargetLeft()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Targets & "_Drop_Target_3Bank_Reset_Up_" & Int(Rnd*6)+1,DOFContactors), Solenoid_DropTargetReset_SoundLevel, sw28
End Sub



'///////////////////////////////////  SPINNER  //////////////////////////////////
Sub SoundSpinner()
	PlaySoundAtLevelStatic (Cartridge_Spinner & "_Spinner_Spin_Loop"), SpinnerSoundLevel, sw41
End Sub


'//////////////////////////  STADNING TARGET HIT SOUNDS  ////////////////////////
Sub RandomSoundTargetHitStrong()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_5",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_6",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_7",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_8",DOFTargets), Vol(ActiveBall) * TargetSoundFactor		
	End Select
End Sub

Sub RandomSoundTargetHitWeak()
	Select Case Int(Rnd*4)+1		
		Case 1 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_1",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_2",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_3",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_4",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
	End Select
End Sub



'/////////////////////////////  BALL BOUNCE SOUNDS  /////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_2"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_12"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 3 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_14"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 4 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_18"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 5 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_19"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_20"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 7 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_21"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	Select Case Int(Rnd*12)+1
		Case 1 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_1"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 2 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_3"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 3 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_7"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 4 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_8"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 5 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_9"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 6 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_11"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 7 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_13"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 8 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_15"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 9 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_16"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 10 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_17"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 11 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_22"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 12 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_23"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
	End Select
End Sub

'/////////////////////  RAMPS BALL DROP TO PLAYFIELD SOUNDS  ////////////////////
'/////////  METAL WIRE - RIGHT RAMP - EXIT HOLE TO PLAYFIELD - SOUNDS  //////////
Sub RandomSoundLeftRampDropToPlayfield()
	PlaySoundAtLevelStatic (Cartridge_Metal_Ramps & "_Ramp_Right_Metal_Wire_Drop_to_Playfield_" & Int(Rnd*2)+1), RightRampMetalWireDropToPlayfieldSoundLevel, RHelper3
End Sub


''//////////////  PLASTIC - LEFT RAMP - EXIT HOLE TO LOCK - SOUND  ///////////////
'Sub RandomSoundRightRampLeftExitDropToLock()
'	PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Drop_to_Lock_" & Int(Rnd*4)+1), LeftPlasticRampDropToLockSoundLevel, RHelper2
'End Sub


'////////////  PLASTIC - LEFT RAMP - EXIT HOLE TO PLAYFIELD - SOUND  ////////////
Sub RandomSoundRightRampRightExitDropToPlayfield(prim)
	PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Drop_to_Playfield_" & Int(Rnd*2)+1), LeftPlasticRampDropToPlayfieldSoundLevel, prim
End Sub


'//////////////////  METAL WIRE RIGHT RAMP - 2ND PART RAMP SUB  /////////////////
'Sub SoundRightPlasticRampPart2(toggle, ballvariablePlasticRampTimer1)
'	Set ballvariablePlasticRampTimer1 = ActiveBall
'	Select Case toggle
'		Case SoundOn
'			PlasticRampTimer1.Interval = 10
'			PlasticRampTimer1.Enabled = 1
'		Case SoundOff
'			PlasticRampTimer1.Enabled = 0
'			StopSound Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_BallRoll_" & b
'	End Select
'End Sub


'////////////////////////////  RAMP ENTRANCE EVENTS  ////////////////////////////
'/////////////////////////  RIGHT RAMP ENTRANCE SOUNDS  /////////////////////////

Sub LRAMPEnter_Hit()
	' Play the right plastic ramp entrance lifter/down sound for each ball
	If BallVel(ActiveBall) > 1 and ActiveBall.VelY > 0 Then
'		debug.print "Ball rolls downwards"
		PlaySoundAtLevelTimerExistingActiveBall (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Ramp_RollBack_" & Int(Rnd*2)+1), RightPlasticRampEnteranceSoundLevel, ActiveBall
	End If
	If BallVel(ActiveBall) > 1 and ActiveBall.VelY < 0 Then
'		debug.print "Ball rolls upwards"
		PlaySoundAtLevelTimerExistingActiveBall (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Ramp_Enter_" & Int(Rnd*4)+1), RightPlasticRampEnteranceSoundLevel, ActiveBall
	End If
End Sub


'//////////////////////////  LEFT RAMP ENTRANCE SOUNDS  /////////////////////////

Sub RRAMPEnter_Hit()
	' Play the left ramp plastic ramp entrance for each ball
	If BallVel(ActiveBall) > 1 and ActiveBall.VelY > 0 Then
'		debug.print "Ball rolls downwards"
		PlaySoundAtLevelTimerExistingActiveBall (Cartridge_Plastic_Ramps & "_Ramp_Left_Plastic_RollBack_" & Int(Rnd*2)+1), LeftPlasticRampEnteranceSoundLevel, ActiveBall	
	End If
	If BallVel(ActiveBall) > 1 and ActiveBall.VelY < 0 Then
'		debug.print "Ball rolls upwards"
		PlaySoundAtLevelTimerExistingActiveBall (Cartridge_Plastic_Ramps & "_Ramp_Left_Plastic_Enter_" & Int(Rnd*4)+1), LeftPlasticRampEnteranceSoundLevel, ActiveBall	
	End If
End Sub


'////////////////////////////////////  DRAIN  ///////////////////////////////////
'///////////////////////////////  OUTHOLE SOUNDS  ///////////////////////////////
Sub RandomSoundOutholeHit()
	PlaySoundAtLevelStatic (Cartridge_Trough & "_Outhole_Drain_Hit_" & Int(Rnd*4)+1), Solenoid_OutholeKicker_SoundLevel, Drain
End Sub

Sub RandomSoundOutholeKicker()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Trough & "_Outhole_Kicker_" & Int(Rnd*4)+1,DOFContactors), Solenoid_OutholeKicker_SoundLevel, Drain
End Sub

'/////////////////////  BALL SHOOTER FEEDER SOLENOID SOUNDS  ////////////////////
Sub RandomSoundShooterFeeder()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Trough & "_Shooter_Feeder_" & Int(Rnd*6)+1,DOFContactors), Solenoid_ShooterFeeder_SoundLevel, ballrelease
End Sub


'///////  SHOOTER LANE - BALL RELEASE ROLL IN SHOOTER LANE SOUND - SOUND  ///////
Sub SoundBallReleaseShooterLane(toggle)
	Select Case toggle
		Case SoundOn
			PlaySoundAtLevelActiveBall (Cartridge_Table_Specifics & "_Ball_Launch_from_Shooter_Lane"), BallReleaseShooterLaneSoundLevel
		Case SoundOff
			StopSound Cartridge_Table_Specifics & "_Ball_Launch_from_Shooter_Lane"
	End Select
End Sub

'////////////////////////////  AUTO-PLUNGER SOUNDS  /////////////////////////////
Sub RandomSoundAutoPlunger()
		PlaySoundAtLevelStatic SoundFX("SY_TNA_REV01_Auto_Launch_Coil_" & Int(Rnd*5)+1, DOFContactors), AutoPlungerSoundLevel, TriggerAutoPlunger
End Sub

'//////////////////////////////  KNOCKER SOLENOID  //////////////////////////////
Sub KnockerSolenoid(enabled)
	'PlaySoundAtLevelStatic SoundFX(Cartridge_Knocker & "_Knocker_Coil",DOFKnocker), Solenoid_Knocker_SoundLevel, KnockerPosition
	if Enabled then
		PlaySound SoundFX(Cartridge_Knocker & "_Knocker_Coil",DOFKnocker), 0, Solenoid_Knocker_SoundLevel
	end if
End Sub


'/////////////////////////////  EJECT HOLD SOLENOID  ////////////////////////////
Sub RandomSoundEjectHoleSolenoid()
	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Eject_Kickout_" & Int(Rnd*8)+1), Solenoid_TopEject_SoundLevel, sw46k
End Sub


'///////////////////////////////  EJECT BALL BUMP  //////////////////////////////
Sub RandomSoundEjectBallBump()
	PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_After_Eject_" & Int(Rnd*3)+1), EjectBallBumpSoundLevel, EjectBallPosition
End Sub


'///////////////////////////  HEAD SAUCER BALL ENTER  ////////////////////////////
Sub RandomSoundHeadSaucer()
	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Eject_Enter_" & Int(Rnd*2)+1), HeadSaucerSoundLevel, sw65x
End Sub

'///////////////////////////  EJECT HOLD BALL ENTER  ////////////////////////////
Sub RandomSoundEjectHoleEnter()
	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Eject_Enter_" & Int(Rnd*2)+1), EjectHoleEnterSoundLevel, BallScoopEnter
End Sub


'///////////////////////////  LOCKING KICKER SOLENOID  //////////////////////////
Sub RandomSoundLockingKickerSolenoid()
	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Locking_Kickback_" & Int(Rnd*4)+1), Solenoid_LeftLockingKickback_SoundLevel, LockingPosition
End Sub


'//////////////////////////  SLINGSHOT SOLENOID SOUNDS  /////////////////////////
Sub RandomSoundSlingshotLeft()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Slingshots & "_Slingshot_Left_" & Int(Rnd*26)+1,DOFContactors), Solenoid_Slingshot_SoundLevel, LeftSlingshotPosition
End Sub

Sub RandomSoundSlingshotRight()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Slingshots & "_Slingshot_Right_" & Int(Rnd*25)+1,DOFContactors), Solenoid_Slingshot_SoundLevel, RightSlingshotPosition
End Sub

Sub RandomSoundSlingshotTopRight() 'todo
	PlaySoundAtLevelStatic SoundFX(Cartridge_Slingshots & "_Slingshot_Right_" & Int(Rnd*25)+1,DOFContactors), Solenoid_Slingshot_SoundLevel, SLINGU
End Sub

'///////////////////////////  BUMPER SOLENOID SOUNDS  ///////////////////////////
'////////////////////////////////  BUMPERS - TOP  ///////////////////////////////
Sub RandomSoundBumperLeft()
'	Debug.Print Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier
	PlaySoundAtLevelStatic SoundFX(Cartridge_Bumpers & "_Jet_Bumper_Left_" & Int(Rnd*22)+1,DOFContactors), Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier, Bumper1
End Sub

Sub RandomSoundBumperUp()
	'Debug.Print Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier
	PlaySoundAtLevelStatic SoundFX(Cartridge_Bumpers & "_Jet_Bumper_Up_" & Int(Rnd*25)+1,DOFContactors), Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier, Bumper2
End Sub

Sub RandomSoundBumperLow()
	'Debug.Print Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier
	PlaySoundAtLevelStatic SoundFX(Cartridge_Bumpers & "_Jet_Bumper_Low_" & Int(Rnd*28)+1,DOFContactors), Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier, Bumper3
End Sub



'///////////////////////  FLIPPER BATS SOUND SUBROUTINES  ///////////////////////
'//////////////////////  FLIPPER BATS SOLENOID CORE SOUND  //////////////////////
Sub RandomSoundFlipperLowerLeftUpFullStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Full_Stroke_" & RndInt(1,10),DOFFlippers), FlipperLeftLowerHitParm, Flipper
End Sub
'
'Sub RandomSoundFlipperUpperLeftUpFullStroke(flipper)
'	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Upper_Left_Up_Full_Stroke_" & Int(Rnd*5)+1,DOFFlippers), FlipperLeftUpperHitParm, Flipper
'End Sub

Sub RandomSoundFlipperLowerLeftUpDampenedStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Dampened_Stroke_" & RndInt(1,23),DOFFlippers), FlipperLeftLowerHitParm * 1.2, Flipper
End Sub

Sub RandomSoundFlipperLowerRightUpFullStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Full_Stroke_" & RndInt(1,11),DOFFlippers), FlipperRightLowerHitParm, Flipper
End Sub

Sub RandomSoundFlipperLowerRightUpDampenedStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Dampened_Stroke_" & RndInt(1,23),DOFFlippers), FlipperLeftLowerHitParm * 1.2, Flipper
End Sub

Sub RandomSoundFlipperLowerLeftReflip(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Reflip_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

'Sub RandomSoundFlipperUpperLeftReflip(flipper)
'	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Upper_Left_Reflip",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
'End Sub

Sub RandomSoundFlipperLowerRightReflip(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Reflip_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperLowerLeftDown(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Down_" & RndInt(1,10),DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub
'
'Sub RandomSoundFlipperUpperLeftDown(flipper)
'	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Upper_Left_Down_" & Int(Rnd*6)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
'End Sub

Sub RandomSoundFlipperLowerRightDown(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Down_" & RndInt(1,11),DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundLowerLeftQuickFlipUp()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_QuickFlip_Up_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, LeftFlipper
End Sub

Sub RandomSoundLowerRightQuickFlipUp()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_QuickFlip_Up_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, RightFlipper
End Sub

Sub StopAnyFlipperLowerLeftUp()
	Dim anyFullStrokeSound
	Dim anyDampenedStrokeSound
	For anyFullStrokeSound = 1 to 10
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Full_Stroke_" & anyFullStrokeSound)
	Next
	For anyDampenedStrokeSound = 1 to 23
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Dampened_Stroke_" & anyDampenedStrokeSound)
	Next
End Sub

Sub StopAnyFlipperLowerRightUp()
	Dim anyFullStrokeSound
	Dim anyDampenedStrokeSound
	For anyFullStrokeSound = 1 to 11
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Full_Stroke_" & anyFullStrokeSound)
	Next
	For anyDampenedStrokeSound = 1 to 23
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Dampened_Stroke_" & anyDampenedStrokeSound)
	Next
End Sub

Sub StopAnyFlipperLowerLeftDown()
	Dim anyFullDownSound
	For anyFullDownSound = 1 to 10
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Down_" & anyFullDownSound)
	Next
End Sub

Sub StopAnyFlipperLowerRightDown()
	Dim anyFullDownSound
	For anyFullDownSound = 1 to 10
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Right_Down_" & anyFullDownSound)
	Next
End Sub


Sub FlipperHoldCoilLeft(toggle, flipper)
	Select Case toggle
		Case SoundOn
			PlaySoundAtLevelExistingStaticLoop (Cartridge_Flippers & "_Flipper_Hold_Coil_Low_Frequencies_Loop_Lower_Left"), FlipperHoldSoundLevel, flipper
			PlaySoundAtLevelExistingStaticLoop (Cartridge_Flippers & "_Flipper_Hold_Coil_Med_Frequencies_Loop_Lower_Left"), FlipperHoldSoundLevel, flipper
		Case SoundOff
			StopSound Cartridge_Flippers & "_Flipper_Hold_Coil_Low_Frequencies_Loop_Lower_Left"
			StopSound Cartridge_Flippers & "_Flipper_Hold_Coil_Med_Frequencies_Loop_Lower_Left"
	End Select
End Sub

Sub FlipperHoldCoilRight(toggle, flipper)
	Select Case toggle
		Case SoundOn
			PlaySoundAtLevelExistingStaticLoop (Cartridge_Flippers & "_Flipper_Hold_Coil_Low_Frequencies_Loop_Lower_Right"), FlipperHoldSoundLevel, flipper
			PlaySoundAtLevelExistingStaticLoop (Cartridge_Flippers & "_Flipper_Hold_Coil_Med_Frequencies_Loop_Lower_Right"), FlipperHoldSoundLevel, flipper
		Case SoundOff
			StopSound Cartridge_Flippers & "_Flipper_Hold_Coil_Low_Frequencies_Loop_Lower_Right"
			StopSound Cartridge_Flippers & "_Flipper_Hold_Coil_Med_Frequencies_Loop_Lower_Right"
	End Select
End Sub

'
'
'Sub RandomSoundFlipperLowerLeftUpFullStrokeDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Full_Stroke_" & Int(Rnd*6)+1, DOFevent, DOFstate, DOFFlippers), FlipperLeftLowerHitParm, Flipper
'End Sub
'
'Sub RandomSoundFlipperUpperLeftUpFullStrokeDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Upper_Left_Up_Full_Stroke_" & Int(Rnd*5)+1, DOFevent, DOFstate, DOFFlippers), FlipperLeftUpperHitParm, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerLeftUpDampenedStrokeDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Dampened_Stroke_" & Int(Rnd*8)+1, DOFevent, DOFstate, DOFFlippers), FlipperLeftLowerHitParm * 1.2, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerRightUpFullStrokeDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Full_Stroke_" & Int(Rnd*5)+1, DOFevent, DOFstate, DOFFlippers), FlipperRightLowerHitParm, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerRightUpDampenedStrokeDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Dampened_Stroke_" & Int(Rnd*8)+1, DOFevent, DOFstate, DOFFlippers), FlipperRightLowerHitParm * 1.2, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerLeftReflipDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Left_Reflip", DOFevent, DOFstate, DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
'End Sub
'
'Sub RandomSoundFlipperUpperLeftReflipDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Upper_Left_Reflip", DOFevent, DOFstate, DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerRightReflipDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Right_Reflip", DOFevent, DOFstate, DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerLeftDownDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Left_Down_" & Int(Rnd*7)+1, DOFevent, DOFstate, DOFFlippers), FlipperDownSoundLevel, Flipper
'End Sub
'
'Sub RandomSoundFlipperUpperLeftDownDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Upper_Left_Down_" & Int(Rnd*6)+1, DOFevent, DOFstate, DOFFlippers), FlipperDownSoundLevel, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerRightDownDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Right_Down_" & Int(Rnd*7)+1, DOFevent, DOFstate, DOFFlippers), FlipperDownSoundLevel, Flipper
'End Sub


'///////////////////////  FLIPPER BATS BALL COLLIDE SOUND  //////////////////////
dim angdamp, veldamp
angdamp = 0.2
veldamp = 0.8

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm

	If parm => 22 Then
		' Strong hit safe values boundary
		' Flipper stroke dampened
		FlipperLeftLowerHitParm = FlipperUpSoundLevel * 0.1
	Else
		If parm =< 1 Then 
			' Weak hit safe values boundary
			' Flipper stroke full
			FlipperLeftLowerHitParm = FlipperUpSoundLevel
		Else
			' Fully modulated hit
			FlipperLeftLowerHitParm = FlipperUpSoundLevel * (1-(parm/25))
		End If
	End If

	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm

	If parm => 22 Then
		' Strong hit safe values boundary
		' Flipper stroke dampened
		FlipperRightLowerHitParm = FlipperUpSoundLevel * 0.1
	Else
		If parm =< 1 Then 
			' Weak hit safe values boundary
			' Flipper stroke full
			FlipperRightLowerHitParm = FlipperUpSoundLevel
		Else
			' Fully modulated hit
			FlipperRightLowerHitParm = FlipperUpSoundLevel * (1-(parm/25))
		End If
	End If

 	RandomSoundRubberFlipper(parm)
End Sub


Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Flipper_Hit_" & Int(Rnd*7)+1), parm / 25 * RubberFlipperSoundFactor
End Sub



'///////////////////////  RAMP DIVERTER SOLENOID - DIVERT  //////////////////////
Sub RandomSoundRampDiverterDivert()
	PlaySoundAtLevelStatic (Cartridge_Diverters & "_Diverter_Divert_" & Int(Rnd*4)+1), Solenoid_Diverter_Enabled_SoundLevel, DiverterPosition
End Sub

'////////////////////////  RAMP DIVERTER SOLENOID - BACK  ///////////////////////
Sub RandomSoundRampDiverterBack()
	PlaySoundAtLevelStatic (Cartridge_Diverters & "_Diverter_Back_" & Int(Rnd*4)+1), Solenoid_Diverter_Disabled_SoundLevel, DiverterPosition
End Sub

'///////////////////////  RAMP DIVERTER SOLENOID - DIVERT  //////////////////////
Sub RandomSoundGateDivert()
	PlaySoundAtLevelStatic (Cartridge_Diverters & "_Diverter_Divert_" & Int(Rnd*4)+1), Solenoid_Diverter_Enabled_SoundLevel * 0.05, Gate5
End Sub

'////////////////////////  RAMP DIVERTER SOLENOID - BACK  ///////////////////////
Sub RandomSoundGateBack()
	PlaySoundAtLevelStatic (Cartridge_Diverters & "_Diverter_Back_" & Int(Rnd*4)+1), Solenoid_Diverter_Disabled_SoundLevel * 0.05, Gate5
End Sub

'////////////////////  RAMP DIVERTER SOLENOID - MAGNET SOUND  ///////////////////
Sub RandomSoundRampDiverterHold(toggle)
	Select Case toggle	
		Case SoundOn
			PlaySoundAtLevelStaticLoop SoundFX(Cartridge_Diverters & "_Diverter_Hold_Loop",DOFShaker), Solenoid_Diverter_Hold_SoundLevel, DiverterPosition
		Case SoundOff
			StopSound Cartridge_Diverters & "_Diverter_Hold_Loop"
	End Select
End Sub



'//////////////////////////  SOLENOID A/C SELECT RELAY  /////////////////////////
Sub Sound_Solenoid_AC(toggle)
	Select Case toggle
		Case CircuitA
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_A"), RelaySolenoidACSelectSoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_A"), RelaySolenoidACSelectSoundLevel, ACSelectPosition
		Case CircuitC
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_C"), RelaySolenoidACSelectSoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_C"), RelaySolenoidACSelectSoundLevel, ACSelectPosition
	End Select
End Sub


'//////////////////////////  GENERAL ILLUMINATION RELAYS  ///////////////////////
Sub Sound_LowerGI_Relay(toggle)
	Select Case toggle
		Case SoundOn
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_On"), RelayLowerGISoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_On"), RelayLowerGISoundLevel, GIUpperPosition
		Case SoundOff
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_Off"), RelayLowerGISoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_Off"), RelayLowerGISoundLevel, GIUpperPosition
	End Select
End Sub

Sub Sound_UpperGI_Relay(toggle)
	Select Case toggle
		Case SoundOn
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_On"), RelayUpperGISoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_On"), RelayUpperGISoundLevel, GILowerPosition
		Case SoundOff
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_Off"), RelayUpperGISoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_Off"), RelayUpperGISoundLevel, GILowerPosition
	End Select
End Sub


'///////////////////////////////  FLASHERS RELAY  ///////////////////////////////
Sub Sound_Flasher_Relay(toggle, tableobj)
	Select Case toggle
		Case SoundOn
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_On"), RelayFlasherSoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_On"), RelayFlasherSoundLevel, tableobj
		Case SoundOff
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_Off"), RelayFlasherSoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_Off"), RelayFlasherSoundLevel, tableobj
	End Select
End Sub



'////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  /////////////////////
'/////////////////////////////  RUBBERS AND POSTS  //////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ///////////////////////////////
Sub HitsRubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
 		RandomSoundRubberStrong() 
	End if
	If finalspeed <= 10 then
 		RandomSoundRubberWeak()
 	End If	
End Sub


'/////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  /////////////////////
Sub RandomSoundRubberStrong()
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_10"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub


'///////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  /////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_" & Int(Rnd*8)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub


'///////////////////////////////  WALL IMPACTS  /////////////////////////////////
Sub RandomSoundWall()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor * 0.05
			Case 4 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub


'/////////////////////////////  WALL IMPACTS EVENTS  ////////////////////////////
'Sub Wall27_Hit()
'	RandomSoundMetal()
'End Sub
'
'Sub Wall13_Hit()
'	RandomSoundMetal()
'End Sub
'
'Sub Wall30_Hit()
'	RandomSoundMetal()
'End Sub
'
'Sub Wall18_Hit()
'	RandomSoundMetal()
'End Sub
'
'Sub Wall14_Hit()
'	RandomSoundMetal()
'End Sub
'
'Sub Wall19_Hit()
'	RandomSoundMetal()
'End Sub
'
'Sub Wall16_Hit()
'	RandomSoundMetal()
'End Sub
'
'Sub Wall17_Hit()
'	RandomSoundMetal()
'End Sub

Sub Wall46_Hit()
	RandomSoundMetal()
End Sub

sub HitsMetal_Hit(IDX)
'	debug.print "metal hit"
	RandomSoundMetal()
end sub


'RandomSoundBottomArchBallGuideSoftHit - Soft Bounces
Sub Wall78_Hit() : RandomSoundBottomArchBallGuideSoftHit() : End Sub
Sub Wall26_Hit() : RandomSoundBottomArchBallGuideSoftHit() : End Sub

'RandomSoundBottomArchBallGuideHardHit - Hard Hit
Sub Wall062_Hit() : RandomSoundBottomArchBallGuideHardHit() : End Sub
Sub Wall063_Hit() : RandomSoundBottomArchBallGuideHardHit() : End Sub

'RandomSoundFlipperBallGuide
Sub Wall49_Hit() : RandomSoundFlipperBallGuide() : End Sub
Sub Wall48_Hit() : RandomSoundFlipperBallGuide() : End Sub

'Outlane - Walls & Primitives
Sub Wall70_Hit() : RandomSoundOutlaneWalls() : End Sub
Sub Wall5_Hit() : RandomSoundOutlaneWalls() : End Sub
Sub Wall73_Hit() : RandomSoundOutlaneWalls() : End Sub
Sub Wall24_Hit() : RandomSoundOutlaneWalls() : End Sub


'////////////////////////////  INNER LEFT LANE WALLS  ///////////////////////////
Sub Wall226_Hit()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

Sub Wall225_Hit()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub


'right arch
Sub Wall141_Hit()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

Sub Wall132_Hit()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'right arch wire
'Sub Wall254_Hit()
'	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
'End Sub

Sub Wall224_Hit()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub


'/////////////////////////////  METAL TOUCH SOUNDS  /////////////////////////////
Sub RandomSoundMetal()
	Select Case Int(Rnd*20)+1
		Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_1"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_2"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_3"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_4"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_5"), Vol(ActiveBall) * 0.02 * MetalImpactSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_6"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_7"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_8"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_9"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_10"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 11 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_11"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 12 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_12"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 13 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_13"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 14 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_14"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 15 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_15"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 16 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_16"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 17 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_17"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 18 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_18"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 19 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_19"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 20 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_20"), Vol(ActiveBall) * MetalImpactSoundFactor
	End Select
End Sub


'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub HitsWoods_Hit(idx)

'	debug.print "wood hit"
	RandomSoundWood()      
End Sub

Sub RandomSoundWood()
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

'///////////////////////////////  OUTLANES WALLS  ///////////////////////////////
Sub RandomSoundOutlaneWalls()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Outlane_Wall_" & Int(Rnd*9)+1), OutlaneWallsSoundFactor
End Sub


'///////////////////////////  BOTTOM ARCH BALL GUIDE  ///////////////////////////
'///////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////
Sub RandomSoundBottomArchBallGuideSoftHit()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Arch_Ball_Guide_Hit_Soft_" & Int(Rnd*4)+1), BottomArchBallGuideSoundFactor
End Sub


'//////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Arch_Ball_Guide_Hit_Hard_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 3
End Sub


'//////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
 	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End if
End Sub



'/////////////////////////  WIREFORM ANTI-REBOUNT RAILS  ////////////////////////
Sub RandomSoundWireformAntiRebountRail()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed >= 10 then 
 		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_3"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_4"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_5"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 4 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_6"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 5 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_7"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
		End Select
	End if
	If finalspeed < 10 Then
 		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_1"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_2"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_8"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
		End Select
	End if
End Sub


'////////////////////////////  LANES AND INNER LOOPS  ///////////////////////////
'////////////////////  INNER LOOPS - LEFT ENTRANCE - EVENTS  ////////////////////
Sub LeftInnerLaneTriggerUp_Hit()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
		If ActiveBall.VelY < 0 Then RandomSoundInnerLaneEnter()
	End If
End Sub

Sub LeftInnerLaneTriggerDown_Hit()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 7 then
		If ActiveBall.VelY > 0 Then RandomSoundInnerLaneEnter()
	End If
End Sub


'/////////////////////  INNER LOOPS - LEFT ENTRANCE - SOUNDS  ///////////////////
Sub RandomSoundInnerLaneEnter()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Ball_Guide_Hit_" & Int(Rnd*20)+1), Vol(ActiveBall) * InnerLaneSoundFactor
End Sub


'//////////////////////////////  LEFT LANE ENTRANCE  ////////////////////////////
'/////////////////////////  LEFT LANE ENTRANCE - EVENTS  ////////////////////////
Sub LeftLaneTrigger_Hit()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
		If ActiveBall.VelY < 0 Then PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Lane_Left_Ball_Enter_Hit"), Vol(ActiveBall) * LaneEnterSoundFactor : RandomSoundLaneLeftEnter()
	End If
End Sub


'/////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////
Sub RandomSoundLaneLeftEnter()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Lane_Left_Ball_Roll_" & Int(Rnd*2)+1), Vol(ActiveBall) * LaneSoundFactor
End Sub


'/////////////////////////////  RIGHT LANE ENTRANCE  ////////////////////////////
'////////////////////////  RIGHT LANE ENTRANCE - EVENTS  ////////////////////////
Sub RightLaneTrigger_Hit()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
		If ActiveBall.VelY < 0 Then PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Lane_Right_Ball_Enter_Hit"), Vol(ActiveBall) * LaneEnterSoundFactor : RandomSoundLaneRightEnter()
	End If
End Sub


'/////////////////  RIGHT LANE ENTRANCE (RIGHT ORBIT) - SOUNDS  /////////////////
Sub RandomSoundLaneRightEnter()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Lane_Right_Ball_Roll_" & Int(Rnd*3)+1), Vol(ActiveBall) * LaneSoundFactor
End Sub


'/////////////////////////  RAMP HELPERS BALL VARIABLES  ////////////////////////
'Dim ballvariablePlasticRampTimer1


'/////////  PLASTIC LEFT RAMP - RIGHT EXIT HOLE - TO PLAYFIELD - EVENT  /////////

sub BallDrop1_hit	'sometimes drop sound cannot be heard here.
'	debug.print activeball.velz
	RandomSoundRightRampRightExitDropToPlayfield(BallDrop1)
end sub


Sub BallDrop2_Hit()
	if abs(activeball.AngMomZ) > 70 then activeball.AngMomZ = 70
	activeball.AngMomZ = abs(activeball.AngMomZ) * 3
	RandomSoundRightRampRightExitDropToPlayfield(BallDrop2)
'	Call SoundRightPlasticRampPart2(SoundOff, ballvariablePlasticRampTimer1)
End Sub

'ss ramp
sub BallDrop3_hit
'	debug.print "SS drop"
	RandomSoundRightRampRightExitDropToPlayfield(activeball)
end sub


'/////////  METAL WIRE RIGHT RAMP - EXIT HOLE - TO PLAYFIELD - EVENT  //////////
Sub RHelper3_Hit()
	if abs(activeball.AngMomZ) > 70 then activeball.AngMomZ = 70
	activeball.AngMomZ = -abs(activeball.AngMomZ) * 3
	RandomSoundLeftRampDropToPlayfield()
End Sub


'////////////////////////////////////////////////////////////////////////////////
'////          End of Fleep Mechanical Sounds Section                        ////
'////////////////////////////////////////////////////////////////////////////////

'
''***************************************************************
''Table MISC VP sounds
''***************************************************************


Sub Trigger1_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), RightPlasticRampHitsSoundLevel, Trigger1 : End Sub
Sub Trigger2_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), RightPlasticRampHitsSoundLevel, Trigger2 : End Sub
Sub Trigger3_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_5"), RightPlasticRampHitsSoundLevel, Trigger3 : End Sub
Sub Trigger4_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), RightPlasticRampHitsSoundLevel, Trigger4 : End Sub
Sub Trigger5_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), RightPlasticRampHitsSoundLevel, Trigger5 : End Sub
Sub Trigger6_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_5"), RightPlasticRampHitsSoundLevel, Trigger6 : End Sub
Sub Trigger7_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), RightPlasticRampHitsSoundLevel, Trigger7 : End Sub
Sub Trigger8_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), RightPlasticRampHitsSoundLevel, Trigger8 : End Sub
Sub Trigger9_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_5"), RightPlasticRampHitsSoundLevel, Trigger9 : End Sub
Sub Trigger10_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), RightPlasticRampHitsSoundLevel, Trigger10 : End Sub
Sub Trigger11_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), RightPlasticRampHitsSoundLevel, Trigger11 : End Sub
Sub Trigger13_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_5"), RightPlasticRampHitsSoundLevel, Trigger13 : End Sub
Sub Trigger14_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), RightPlasticRampHitsSoundLevel, Trigger14 : End Sub
Sub Trigger15_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), RightPlasticRampHitsSoundLevel, Trigger15 : End Sub

dim FaceGuideHitsSoundLevel : FaceGuideHitsSoundLevel = 0.002 * RightPlasticRampHitsSoundLevel
Sub ColFaceGuideL1_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_2"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub F1Guide_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_1"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub F1Guide2_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_1"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub ColFaceGuideL2_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_2"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub ColFaceGuideL3_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), FaceGuideHitsSoundLevel, activeball : End Sub


sub pBlockBackhand_hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), FaceGuideHitsSoundLevel, activeball : End Sub
sub pBlockBackhand001_hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_5"), FaceGuideHitsSoundLevel, activeball : End Sub

'******************************************************
' VPW Start
'******************************************************

'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
'        debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
'        debug.print "conservation check: " & BallSpeed(aBall)/vel
    elseif TargetBouncerEnabled = 2 and aball.z < 30 then
		'debug.print "velz: " & activeball.velz
		if aball.vely > 3 then	'only hard hits
			Select Case Int(Rnd * 4) + 1
				Case 1: zMultiplier = defvalue+1.1
				Case 2: zMultiplier = defvalue+1.05
				Case 3: zMultiplier = defvalue+0.7
				Case 4: zMultiplier = defvalue+0.3
			End Select
			aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
			'debug.print "----> velz: " & activeball.velz
			'debug.print "conservation check: " & BallSpeed(aBall)/vel
		End If
	end if
end sub


'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity


'*******************************************
'  Late 80's early 90's

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



'
''*******************************************
'' Early 90's and after
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 60
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -5.5
'        AddPt "Polarity", 2, 0.4, -5.5
'        AddPt "Polarity", 3, 0.6, -5.0
'        AddPt "Polarity", 4, 0.65, -4.5
'        AddPt "Polarity", 5, 0.7, -4.0
'        AddPt "Polarity", 6, 0.75, -3.5
'        AddPt "Polarity", 7, 0.8, -3.0
'        AddPt "Polarity", 8, 0.85, -2.5
'        AddPt "Polarity", 9, 0.9,-2.0
'        AddPt "Polarity", 10, 0.95, -1.5
'        AddPt "Polarity", 11, 1, -1.0
'        AddPt "Polarity", 12, 1.05, -0.5
'        AddPt "Polarity", 13, 1.1, 0
'        AddPt "Polarity", 14, 1.3, 0
'
'        addpt "Velocity", 0, 0,         1
'        addpt "Velocity", 1, 0.16, 1.06
'        addpt "Velocity", 2, 0.41,         1.05
'        addpt "Velocity", 3, 0.53,         1'0.982
'        addpt "Velocity", 4, 0.702, 0.968
'        addpt "Velocity", 5, 0.95,  0.968
'        addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper        
'        LF.EndPoint = EndPointLp
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'End Sub


' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub




'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
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
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
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
'  FLIPPER TRICKS 
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
        Dim b

        If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
                EOSNudge1 = 1
                'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
                If Flipper2.currentangle = EndAngle2 Then                        
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
			If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then 
				EOSNudge1 = 0
			end if
        End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
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

Function dAtn(degrees)
	dAtn = atn(degrees * Pi/180)
End Function

Function RndInt(min, max)
    RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
    RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
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


'*************************************************
'  End - Check ball distance from Flipper for Rem
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
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 4
	Case 2:
		SOSRampup = 6
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
'Const EOSReturn = 0.025  'mid 90's and later

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
		Dim b

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

            If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx= 0
            ball.angmomy= 0
            ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
    End If
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************

'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx) 
'	debug.print "posts"
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

'no bouncer for sling bottoms
Sub Primitive62_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub Primitive16_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
'	debug.print "sleeve"
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

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

	public sub Dampenf(aBall, parm) 'Rubberizer is handle here
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
'			debug.print "dampen f"
		End If
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
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
    public ballvel, ballvelx, ballvely, ballvelz, ballangmomx, ballangmomy, ballangmomz

    Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : redim ballvelz(0) : redim ballangmomx(0) : redim ballangmomy(0): redim ballangmomz(0): End Sub 

    Public Sub Update()    'tracks in-ball-velocity
        dim str, b, AllBalls, highestID : allBalls = getballs

        for each b in allballs
            if b.id >= HighestID then highestID = b.id
        Next

        if uBound(ballvel) < highestID then redim ballvel(highestID)    'set bounds
        if uBound(ballvelx) < highestID then redim ballvelx(highestID)    'set bounds
        if uBound(ballvely) < highestID then redim ballvely(highestID)    'set bounds
        if uBound(ballvelz) < highestID then redim ballvelz(highestID)    'set bounds
        if uBound(ballangmomx) < highestID then redim ballangmomx(highestID)    'set bounds
        if uBound(ballangmomy) < highestID then redim ballangmomy(highestID)    'set bounds
        if uBound(ballangmomz) < highestID then redim ballangmomz(highestID)    'set bounds

        for each b in allballs
            ballvel(b.id) = BallSpeed(b)
            ballvelx(b.id) = b.velx
            ballvely(b.id) = b.vely
            ballvelz(b.id) = b.velz
            ballangmomx(b.id) = b.angmomx
            ballangmomy(b.id) = b.angmomy
            ballangmomz(b.id) = b.angmomz
        Next
    End Sub
End Class



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* with at least as many objects each as there can be balls, including locked balls
' Ensure you have a timer with a -1 interval that is always running

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
'***If there are more than 3 lights that overlap in a playable area, exclude the less important lights***
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection, and more than 3 will cause "jumping" between which shadows are drawn
' The easiest way to keep track of this is to start with the group on the right slingshot and move anticlockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'G				H											^	E
'															^	B
'	A		 C												^	A
'	 B		D			your collection should look like	^	G		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F												^	H
'															^	C
'															^	D
'															^	F
'		When selecting them, you'd shift+click in this order^^^^^

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.6	'0 to 1, higher is darker
Const AmbientMovement		= 1		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
'	' stop the sound of deleted balls
'	For b = UBound(BOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		...rolling(b) = False
'		...StopSound("BallRoll_" & b)
'	Next
'
'...rolling and drop sounds...

'		If DropCount(b) < 5 Then
'			DropCount(b) = DropCount(b) + 1
'		End If
'
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If BOT(b).Z > 30 Then
'				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
'			Else
'				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
'			BallShadowA(b).X = BOT(b).X
'			BallShadowA(b).visible = 1
'		End If

' *** Required Functions, enable these if they are not already present elswhere in your table
Function DistanceFast(x, y)
	dim ratio, ax, ay
	ax = abs(x)					'Get absolute value of each vector
	ay = abs(y)
	ratio = 1 / max(ax, ay)		'Create a ratio
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then			'Quickly determine if it's worth using
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
'
'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******
Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10,BallShadowA11)

DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		iii = iii + 1
	Next
	numberofsources = iii
	numberofsources_hold = iii
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, currentMat, AnotherSource, iii

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(BOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible


		If AmbientBallShadowOn = 1 And Not InRect(BOT(s).x, BOT(s).y, 665,1921,842,1808,853,1917,707,2000) Then			'Primitive shadow on playfield, flasher shadow in ramps
			If BOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				if (BOT(s).Z > 60 And InRect(BOT(s).x, BOT(s).y, 100,1100,430,650,420,1030,100,1600) Or InRect(BOT(s).x, BOT(s).y, 860,1180,940,1180,940,1750,860,1750)) Then
'					debug.print "drop pf"
					If BOT(s).X < tablewidth/2 Then
						objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
					Else
						objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
					End If
					objBallShadow(s).Y = BOT(s).Y + BallSize/10 + fovY
					objBallShadow(s).size_x = 6.5 * ((BOT(s).Z-(ballsize/2))/60)
					objBallShadow(s).size_y = 5.0 * ((BOT(s).Z-(ballsize/2))/60)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*0.8,RGB(0,0,0),0,0,False,True,0,0,0,0
					objBallShadow(s).visible = 1
					BallShadowA(s).visible = 0
				else
'					debug.print "climb ramp"
					objBallShadow(s).visible = 0
					BallShadowA(s).X = BOT(s).X
					BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
					BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
					BallShadowA(s).visible = 1
				end if
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf, primitive only
'				debug.print "normal pf"
				objBallShadow(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + fovY
				UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
				objBallShadow(s).size_x = 6.5
				objBallShadow(s).size_y = 5.0
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if
'
'		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
'			If BOT(s).Z > 30 Then							'In a ramp
'				BallShadowA(s).X = BOT(s).X
'				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
'				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
'				BallShadowA(s).visible = 1
'			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf
'				BallShadowA(s).visible = 1
'				If BOT(s).X < tablewidth/2 Then
'					BallShadowA(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
'				Else
'					BallShadowA(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
'				End If
'				BallShadowA(s).Y = BOT(s).Y + Ballsize/10 + fovY
'				BallShadowA(s).height=BOT(s).z - BallSize/2 + 5
'			Else											'Under pf
'				BallShadowA(s).visible = 0
'			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 Then 'And BOT(s).Y < (TableHeight - 200) Then 'Or BOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				For iii = 0 to numberofsources - 1 
					LSd=DistanceFast((BOT(s).x-DSSources(iii)(0)),(BOT(s).y-DSSources(iii)(1)))	'Calculating the Linear distance to the Source
					If LSd < falloff then 'And gilvl > 0 Then						    'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = iii
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-ShadowOpacity)
							End If

						Elseif currentShadowCount(s) = 2 Then
																	'Same logic as 1 shadow, but twice
							currentMat = objrtx1(s).material
							AnotherSource = sourcenames(s)
							objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-DistanceFast((BOT(s).x-DSSources(AnotherSource)(0)),(BOT(s).y-DSSources(AnotherSource)(1))))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
	'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx2(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity2 = (falloff-LSd)/falloff
							objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2))
							End If
						end if
					Else
						currentShadowCount(s) = 0
						BallShadowA(s).Opacity = 100*AmbientBSFactor
					End If
				Next
			Else									'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************

'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection

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
	AddSlingsPt 0, 0.00,	-4
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
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
	private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2

	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): Enabled = True : End Sub 

	Public Property let Object(aInput) : Set Slingshot = aInput : End Property
	Public Property Let EndPoint1(aInput) : SlingX1 = aInput.x: SlingY1 = aInput.y: End Property
	Public Property Let EndPoint2(aInput) : SlingX2 = aInput.x: SlingY2 = aInput.y: End Property

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 then Report
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		If not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


	Public Sub VelocityCorrect(aBall)
		dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then 
			XL = SlingX1 : YL = SlingY1 : XR = SlingX2 : YR = SlingY2
		Else
			XL = SlingX2 : YL = SlingY2 : XR = SlingX1 : YR = SlingY1
		End If

		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then 
			If ABS(XR-XL) > ABS(YR-YL) Then 
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If

		'Velocity angle correction
		If not IsEmpty(ModIn(0) ) then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
			'debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'debug.print " " 
		End If
	End Sub

End Class

'****************************************************************
'		VR Mode
'****************************************************************
DIM VRThings
If VRRoom > 0 Then
	SetBackGlass	'sets also visibility
	BGDark.visible = 1
	BGSpeakers.visible = 1
	For each VRThings in VRGIBody:VRThings.visible = 1: Next
	For each VRThings in VRGINoBody:VRThings.visible = 1: Next
	For each VRThings in VRSpeakerFlashers:VRThings.visible = 1: Next
	If VRRoom = 1 Then
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VR_BaSti:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		BeerTimer.enabled = 1
		ClockTimer.enabled = 1
	End If
	If VRRoom = 2 Then
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VR_BaSti:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
		BeerTimer.enabled = 0
		ClockTimer.enabled = 0
	End If
	If VRRoom = 3 Then
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VR_BaSti:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next	
		BeerTimer.enabled = 0
		ClockTimer.enabled = 0
	End If
Else
	for each VRThings in VRCab:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	for each VRThings in VR_BaSti:VRThings.visible = 0:Next
	For each VRThings in VRDigitsTop:VRThings.visible = 0: Next
	For each VRThings in VRDigitsBottom:VRThings.visible = 0: Next
	For each VRThings in VRGIBody:VRThings.visible = 0: Next
	For each VRThings in VRGINoBody:VRThings.visible = 0: Next
	For each VRThings in VRSpeakerFlashers:VRThings.visible = 0: Next
End if


'**********************
'	Set Up Backglass	
'**********************


	Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen

Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglass
		obj.x = obj.x
		obj.height = - obj.y + 250
		obj.y = 25 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
		obj.visible = true
	Next

	For Each obj In VRBackglassHigh
		obj.x = obj.x
		obj.height = - obj.y + 250
		obj.y = 5 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
		obj.visible = true
	Next

	For Each obj In VRBackglassMidHigh
		obj.x = obj.x
		obj.height = - obj.y + 250
		obj.y = 20 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
		obj.visible = true
	Next

	For Each obj In VRBackglassMidLow
		obj.x = obj.x
		obj.height = - obj.y + 250
		obj.y = 27 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
		obj.visible = true
	Next

	For Each obj In VRBackglassLow
		obj.x = obj.x
		obj.height = - obj.y + 250
		obj.y = 45 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
		obj.visible = true
	Next

	For Each obj In VRDigitsTop
		obj.x = obj.x
		obj.height = - obj.y + 234
		obj.y = 60 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
	Next

	For Each obj In VRDigitsBottom
		obj.x = obj.x
		obj.height = - obj.y + 230
		obj.y = 68 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
	Next


	For Each obj In VRBackglassSpeaker
		obj.x = obj.x
		obj.height = - obj.y + 210
		obj.y = 75 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
	Next

	For Each obj In VRSpeakerflashers
		obj.x = obj.x
		obj.height = - obj.y + 210
		obj.y = 85 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
	Next

	For Each obj In VRBackglassSolFla
		obj.visible = False
	Next


End Sub


' ***************************************************************************
'Alphanumeric Setup Code
' ****************************************************************************


Dim DigitsVR(32)
DigitsVR(0)=Array(ax00, ax05, ax0c, ax0d, ax08, ax01, ax06, ax0f, ax02, ax03, ax04, ax07, ax0b, ax0a, ax09, ax0e)
DigitsVR(1)=Array(ax10, ax15, ax1c, ax1d, ax18, ax11, ax16, ax1f, ax12, ax13, ax14, ax17, ax1b, ax1a, ax19, ax1e)
DigitsVR(2)=Array(ax20, ax25, ax2c, ax2d, ax28, ax21, ax26, ax2f, ax22, ax23, ax24, ax27, ax2b, ax2a, ax29, ax2e)
DigitsVR(3)=Array(ax30, ax35, ax3c, ax3d, ax38, ax31, ax36, ax3f, ax32, ax33, ax34, ax37, ax3b, ax3a, ax39, ax3e)
DigitsVR(4)=Array(ax40, ax45, ax4c, ax4d, ax48, ax41, ax46, ax4f, ax42, ax43, ax44, ax47, ax4b, ax4a, ax49, ax4e)
DigitsVR(5)=Array(ax50, ax55, ax5c, ax5d, ax58, ax51, ax56, ax5f, ax52, ax53, ax54, ax57, ax5b, ax5a, ax59, ax5e)
DigitsVR(6)=Array(ax60, ax65, ax6c, ax6d, ax68, ax61, ax66, ax6f, ax62, ax63, ax64, ax67, ax6b, ax6a, ax69, ax6e)
DigitsVR(7)=Array(ax70, ax75, ax7c, ax7d, ax78, ax71, ax76, ax7f, ax72, ax73, ax74, ax77, ax7b, ax7a, ax79, ax7e)
DigitsVR(8)=Array(ax80, ax85, ax8c, ax8d, ax88, ax81, ax86, ax8f, ax82, ax83, ax84, ax87, ax8b, ax8a, ax89, ax8e)
DigitsVR(9)=Array(ax90, ax95, ax9c, ax9d, ax98, ax91, ax96, ax9f, ax92, ax93, ax94, ax97, ax9b, ax9a, ax99, ax9e)
DigitsVR(10)=Array(axa0, axa5, axac, axad, axa8, axa1, axa6, axaf, axa2, axa3, axa4, axa7, axab, axaa, axa9, axae)
DigitsVR(11)=Array(axb0, axb5, axbc, axbd, axb8, axb1, axb6, axbf, axb2, axb3, axb4, axb7, axbb, axba, axb9, axbe)
DigitsVR(12)=Array(axc0, axc5, axcc, axcd, axc8, axc1, axc6, axcf, axc2, axc3, axc4, axc7, axcb, axca, axc9, axce)
DigitsVR(13)=Array(axd0, axd5, axdc, axdd, axd8, axd1, axd6, axdf, axd2, axd3, axd4, axd7, axdb, axda, axd9, axde)
DigitsVR(14)=Array(axe0, axe5, axec, axed, axe8, axe1, axe6, axef, axe2, axe3, axe4, axe7, axeb, axea, axe9, axee)
DigitsVR(15)=Array(axf0, axf5, axfc, axfd, axf8, axf1, axf6, axff, axf2, axf3, axf4, axf7, axfb, axfa, axf9, axfe)

DigitsVR(16)=Array(a2ax00, a2ax05, a2ax0c, a2ax0d, a2ax08, a2ax01, a2ax06, a2ax0f, a2ax02, a2ax03, a2ax04, a2ax07, a2ax0b, a2ax0a, a2ax09, a2ax0e)
DigitsVR(17)=Array(a2ax10, a2ax15, a2ax1c, a2ax1d, a2ax18, a2ax11, a2ax16, a2ax1f, a2ax12, a2ax13, a2ax14, a2ax17, a2ax1b, a2ax1a, a2ax19, a2ax1e)
DigitsVR(18)=Array(a2ax20, a2ax25, a2ax2c, a2ax2d, a2ax28, a2ax21, a2ax26, a2ax2f, a2ax22, a2ax23, a2ax24, a2ax27, a2ax2b, a2ax2a, a2ax29, a2ax2e)
DigitsVR(19)=Array(a2ax30, a2ax35, a2ax3c, a2ax3d, a2ax38, a2ax31, a2ax36, a2ax3f, a2ax32, a2ax33, a2ax34, a2ax37, a2ax3b, a2ax3a, a2ax39, a2ax3e)
DigitsVR(20)=Array(a2ax40, a2ax45, a2ax4c, a2ax4d, a2ax48, a2ax41, a2ax46, a2ax4f, a2ax42, a2ax43, a2ax44, a2ax47, a2ax4b, a2ax4a, a2ax49, a2ax4e)
DigitsVR(21)=Array(a2ax50, a2ax55, a2ax5c, a2ax5d, a2ax58, a2ax51, a2ax56, a2ax5f, a2ax52, a2ax53, a2ax54, a2ax57, a2ax5b, a2ax5a, a2ax59, a2ax5e)
DigitsVR(22)=Array(a2ax60, a2ax65, a2ax6c, a2ax6d, a2ax68, a2ax61, a2ax66, a2ax6f, a2ax62, a2ax63, a2ax64, a2ax67, a2ax6b, a2ax6a, a2ax69, a2ax6e)
DigitsVR(23)=Array(a2ax70, a2ax75, a2ax7c, a2ax7d, a2ax78, a2ax71, a2ax76, a2ax7f, a2ax72, a2ax73, a2ax74, a2ax77, a2ax7b, a2ax7a, a2ax79, a2ax7e)
DigitsVR(24)=Array(a2ax80, a2ax85, a2ax8c, a2ax8d, a2ax88, a2ax81, a2ax86, a2ax8f, a2ax82, a2ax83, a2ax84, a2ax87, a2ax8b, a2ax8a, a2ax89, a2ax8e)
DigitsVR(25)=Array(a2ax90, a2ax95, a2ax9c, a2ax9d, a2ax98, a2ax91, a2ax96, a2ax9f, a2ax92, a2ax93, a2ax94, a2ax97, a2ax9b, a2ax9a, a2ax99, a2ax9e)
DigitsVR(26)=Array(a2axa0, a2axa5, a2axac, a2axad, a2axa8, a2axa1, a2axa6, a2axaf, a2axa2, a2axa3, a2axa4, a2axa7, a2axab, a2axaa, a2axa9, a2axae)
DigitsVR(27)=Array(a2axb0, a2axb5, a2axbc, a2axbd, a2axb8, a2axb1, a2axb6, a2axbf, a2axb2, a2axb3, a2axb4, a2axb7, a2axbb, a2axba, a2axb9, a2axbe)
DigitsVR(28)=Array(a2axc0, a2axc5, a2axcc, a2axcd, a2axc8, a2axc1, a2axc6, a2axcf, a2axc2, a2axc3, a2axc4, a2axc7, a2axcb, a2axca, a2axc9, a2axce)
DigitsVR(29)=Array(a2axd0, a2axd5, a2axdc, a2axdd, a2axd8, a2axd1, a2axd6, a2axdf, a2axd2, a2axd3, a2axd4, a2axd7, a2axdb, a2axda, a2axd9, a2axde)
DigitsVR(30)=Array(a2axe0, a2axe5, a2axec, a2axed, a2axe8, a2axe1, a2axe6, a2axef, a2axe2, a2axe3, a2axe4, a2axe7, a2axeb, a2axea, a2axe9, a2axee)
DigitsVR(31)=Array(a2axf0, a2axf5, a2axfc, a2axfd, a2axf8, a2axf1, a2axf6, a2axff, a2axf2, a2axf3, a2axf4, a2axf7, a2axfb, a2axfa, a2axf9, a2axfe)



'Sub DisplayTimerVR_Timer
'		Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
'		ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
'		If Not IsEmpty(ChgLED)Then
'
'		   For ii=0 To UBound(chgLED)
'			  num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
'				'if (num < 32) then
'				if (num < 32) then
'				  For Each obj In DigitsVR(num)
'					   If chg And 1 Then obj.visible=stat And 1
'					   chg=chg\2 : stat=stat\2
'					  Next
'				Else
'				end if
'			Next
'		end if
' End Sub

Sub DisplayTimerVR_Timer
	If VRRoom <> 0 Then
        Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
        ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
        If Not IsEmpty(ChgLED)Then
           For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				if (num < 32) then
					For Each obj In DigitsVR(num)
						If chg And 1 Then obj.visible=stat And 1
							chg=chg\2 : stat=stat\2
					Next
				Else
                end if
            Next
        end if
	End If
End Sub


'******************* VR Plunger **********************

Sub TimerVRPlunger_Timer
  If TAFPlungerNew.Y < 1227 then
		TAFPlungerNew.Y = TAFPlungerNew.Y + 5
		TAFPlungerNew1.Y = TAFPlungerNew1.Y + 5
  End If
End Sub

Sub TimerVRPlunger2_Timer
	TAFPlungerNew.Y = 1097.224 + (5* Plunger.Position) -20
	TAFPlungerNew1.Y = 1097.224 + (5* Plunger.Position) -20
	timervrplunger2.enabled = 0
End Sub

' ***** Beer Bubble Code - Rawd *****
Sub BeerTimer_Timer()

Randomize(21)
	BeerBubble1.z = BeerBubble1.z + Rnd(1)*0.5
	if BeerBubble1.z > -771 then BeerBubble1.z = -955
	BeerBubble2.z = BeerBubble2.z + Rnd(1)*1
	if BeerBubble2.z > -768 then BeerBubble2.z = -955
	BeerBubble3.z = BeerBubble3.z + Rnd(1)*1
	if BeerBubble3.z > -768 then BeerBubble3.z = -955
	BeerBubble4.z = BeerBubble4.z + Rnd(1)*0.75
	if BeerBubble4.z > -774 then BeerBubble4.z = -955
	BeerBubble5.z = BeerBubble5.z + Rnd(1)*1
	if BeerBubble5.z > -771 then BeerBubble5.z = -955
	BeerBubble6.z = BeerBubble6.z + Rnd(1)*1
	if BeerBubble6.z > -774 then BeerBubble6.z = -955
	BeerBubble7.z = BeerBubble7.z + Rnd(1)*0.8
	if BeerBubble7.z > -768 then BeerBubble7.z = -955
	BeerBubble8.z = BeerBubble8.z + Rnd(1)*1
	if BeerBubble8.z > -771 then BeerBubble8.z = -955
End Sub



' ***************** VR Clock code below - THANKS RASCAL ******************
Dim CurrentMinute ' for VR clock 
' VR Clock code below....
Sub ClockTimer_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub
 ' ********************** END CLOCK CODE   *********************************



' 003 - iaakki - Set non-visible: F23SB1, F23SB2, F24SB1, F24SB2, Dome21Bloom-Dome24Bloom, UpperLeftWallGlare, LowerLeftWallGlare, UpperRightWallGlare, LowerRightWallGlare
'				 as they are probably duplicates to new flasher objects. Fixed the flashers and various other script issues. Find changes with tag 'iaakki'
' 009 - iaakki - pf and inserts text images adjusted
' 048 - iaakki - removed some flashers again

' 061_3 - iaakki		- one more time...
' 061_4 - iaakki		- realignment continues..
' 061_5 - iaakki		- realignment continues....
' 061_6 - iaakki		- most of the realignment done
' 061_7 - iaakki		- upper pf ball reflection reworked
' 062_1 - fluffhead35   - Added Materials.  Added Flipper Physics.  Changed Rubbers, Pegs, Posts, Slings to correct materials and added to dPosts and dPegs collections. Changed Fliper Default Physics set from 4 to None
' 062_2 - fluffhead35   - Updating Table Slope, Difficulty, and Table Physics.  Change table default physic set from 4 to none.
' 062_3 - fluffhead35   - Changing materials for all collidables.  Adding Physics for slings and bumpers.  Seperated the slings from the rubbers and then made it so slings did not go all the way to the post.
' 062_4 - fluffhead35   - Changed the Gates Physics

' iaakki - new playfield, new inserts, lighting tweaks, some new brackets

'066 - fiddle
'067 - merge
'074 - upper pf collidables built, trough fixed, front GI reduced and rear boosted, plastic lights redone, layers reorganiced
'075 - pf holes improved, material and collection fixes
'076 - fleep new sound engine included (lots to fix still, may crash on some events..), fixed merge issue with motor control
'077 - various audio fixes, center lock diverter rework(feels too fast now)
'078 - headmechtimer fix
'079 - LockPost animation and some delay added, headMech tweaks and failsafe
'080 - Head speed adjusted, initial state fixed, FlipperRelay sol40 added, minor other script improvements
'081 - Fluffhead - Adding in dynamic shadows and new slingshot corrections.
'082 - iaakki - PROC motor directions swapped, shadows adjusted, PROC flipperrelay control on button hold, apron collider height increased
'083 - iaakki - merged some lighting work, ramp rolling sounds added,
'084 - Leojreimroc - VR Room and backglass added.  Updated Flasher code for solenoid 19 (Jackpot)
'086 - iaakki - GI adjustment, metal material fixes
'087 - iaakki - New upscaled plastics, primitives, bevels. New nuts. Flip shadows fixed.
'088 - iaakki - PBT sound cartridges added for flips, jet bumpers and slings.
'089 - iaakki - Insert blooms and improved ball reflections, various SSF sounds fixes as some collections got corrupted, head motor sounds
'090 - Fleep - Adding missing TriggerBallNearLF and TriggerBallNearRF triggers used in flipper sound scripts, Fixed double up flipper sounds bug
'091 - iaakki - New left ramp primitive, some merged fixes, ball drop sounds, saucer sounds, face saucer visuals fixed, various sound tweaks, 
'				flips reworked to use solenoids with pinmame and different code for Proc, motor sound fixes...
'092 - Sixtoe - Fixed shuttle
'093 - iaakki - fixed ball drop sounds, balanced ssf levels, fixed left ramp gate, default cabinet pov restored, LED GI fades fast, Reduced flasher dome effect on pf and sides
'095 - ClarkKent - reposition some of the collidable walls
'096 - iaakki - merging and fixing script options, cabinetmode, flasher balancing, plastic lights fixed, ball reflections for GI added, New SSF sounds reimported as 24bit, so it works also on 10.7.
'				added roths fix to drop sounds, tied blue plastic pegs to GI lights
'097 - iaakki - lights and reflections adjustments, testing different Balls, helmet lights reworked
'098 - iaakki - red plastic peg position changed, left scoop redone, PF mesh by cyberpez added, shield plastic db fix, ball lock solenoid Speed adjust, helmet speed limit, zigzag ramp speed and output fix
'				ramps tied to gi, flasher blooms combined, reduced insert blooms, helmet bulb reflections fixed
'099 - rothbauerw - converted collections to arrays where needed, adjusted tilt sensitivity and digital nudge, add physical trough (changed ballsize to 50), created a BOT array and eliminated getballs calls, 
'               added playfield wall under flippers and shooter lane to prevent ball from falling through/into playfield, 
'100 - iaakki - FlasherFadingPerf script option added, "VR right sidewall lights appear above the rails" fixed one more time, VR backglass lights 71-85 fixed
'101 - rothbauerw - a couple of timers had the intervals set to '0', set them to 100 instead.  Fixed sw72 to insure it activates in all circumstances, adjusted ball drop sound code
'102 - iaakki - baked metal walls and tied them to different GI colors
'103 - rothbauerw - adjusted sslaunch animation and timer, added efficiencies to GIUpdate (removed calls for bg gi updates and only update the GI circuit being called)
'104 - iaakki - added UsePinmameModulatedSolenoids script Option
'105 - iaakki - new screws here and there, flipper parameter fix, flipper rubber option now affects to elasticity, fixed some miss alignments
'106 - iaakki - cleanup and reorganicing layers, side bakes positioned better to match the rail in VR, slight modification to dynamicsources
'108 - iaakki - Spinner shadow, gate orientation and sound fixes
'RC1 - iaakki - Gate rework finished, head saucer sounds fixed one more time, heart ramp decal redone, white rubber material adjusted, skillshot lights reworked
'RC2 - iaakki - EjectHoleEnterSound sub fixed, info sections updated
