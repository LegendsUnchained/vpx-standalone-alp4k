'              ______           _
'              |  ___|         | |
'              | |_ _   _ _ __ | |__   ___  _   _ ___  ___
'              |  _| | | | '_ \| '_ \ / _ \| | | / __|/ _ \ 
'              | | | |_| | | | | | | | (_) | |_| \__ \  __/
'              \_|  \__,_|_| |_|_| |_|\___/ \__,_|___/\___|
'
'
' _    _ _ _ _ _                        __  __   _____  _____  _____  __
'| |  | (_) | (_)                      / / /  | |  _  ||  _  ||  _  | \ \ 
'| |  | |_| | |_  __ _ _ __ ___  ___  | |  `| | | |_| || |_| || |/' |  | |
'| |/\| | | | | |/ _` | '_ ` _ \/ __| | |   | | \____ |\____ ||  /| |  | |
'\  /\  / | | | | (_| | | | | | \__ \ | |  _| |_.___/ /.___/ /\ |_/ /  | |
' \/  \/|_|_|_|_|\__,_|_| |_| |_|___/ | |  \___/\____/ \____/  \___/   | |
'                                      \_\                            /_/

'Funhouse (Williams 1990) for VP10

'***The very talented FH development team.***
'Original VP10 beta by "Shoopity" and completed by "wrd1972"
'Original content borrowed from "JPsalas" VP9 table
'Additional scripting by "cyberpez", "rothbauerw", "32assassin"
'Plastics prims by "cyberpez"
'Popcorn, balloons and hotdog cart, marble ball and faceless Rudy mods by "Cyberpez"
'Creepy Rudy artwork by "Rothbauerw"
'Clear ramps by "dark"
'Wire ramps by "ninuzzu"
'Subway by "ninuzzu"
'Rudy prims by "vanlion"
'Mystery mirror by "cyberpez"
'Flasher domes and pop-bumper domes by "Flupper"
'PF lighting by "wrd1972"
'Flashers and bulbs by "wrd1972"
'Physics by "wrd1972"
'PF image refresh by "clarkkent"
'DT view scoring reels and background by "32assassin"
'DOF by "arngrim"
'Playfield insert prims and additional 3D work by "Schreibi34"
'***An extra special thanks to "cyberpez" and "Rothbauerw" for the hard work and countless hours of development on this table.***

Option Explicit
Randomize
SetLocale(1033)
Dim RudyMod, LazyEyeMod, BallTypeMod, FlipperRubberColor, ClockMod, BalloonMod, PopCornMod, HotDogCartMod, MouthHitSounds, FaceTwitchMod, LevelMod, DrainPostMod, MirrorLetteringMod, Musicsnippet, Prevgameover
Dim SubwayColorMod, ApronWallsMod, InstructionCardsMod, MirrorLightsMod, GIColorMod, GIColorModType, SpecialBellMod
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'***************************************************************************************************************************************************************
'***************************************************************************************************************************************************************
' _____     _     _        ___________ _   _                   _   _
'|_   _|   | |   | |      |  _  | ___ \ | (_)                 | | | |
'  | | __ _| |__ | | ___  | | | | |_/ / |_ _  ___  _ __  ___  | |_| | ___ _ __ ___
'  | |/ _` | '_ \| |/ _ \ | | | |  __/| __| |/ _ \| '_ \/ __| |  _  |/ _ \ '__/ _ \ 
'  | | (_| | |_) | |  __/ \ \_/ / |   | |_| | (_) | | | \__ \ | | | |  __/ | |  __/
'  \_/\__,_|_.__/|_|\___|  \___/\_|    \__|_|\___/|_| |_|___/ \_| |_/\___|_|  \___|
'
'

'CABMODE
'	Desktop = 0
'	Cab = 1
Const CabMode = 1

'BALL BRIGHT
'	Dark = 0
'	Bright = 1
Const BallBright = 0	

'INTRO MUSIC
'   Add Intro Music Snippet =0
'   No Intro Music Snippet = 1
'Change the value below to set option
MusicSnippet = 1


'Custom Apron & Walls Mod
'	Normal Apron & Walls = 0
'	Custom Apron & Walls = 1
'Change the value below to set option
ApronWallsMod = 0


'GI Color Mod
'	Normal Lighting = 0
'	Blue Lighting = 1
'	Purple Lighting = 2
'	Random = 3
'Change the value below to set option
GIColorMod = 0


'Subway Color Mod
'	No Lights = 0
'	Blue Lights = 1
'   Red Lights = 2
'Change the value below to set option
SubwayColorMod = 0


'Mirror Lights Color Mod
'	Normal Lights = 0
'	Red, White, Blue Lights = 1
'Change the value below to set option
MirrorLightsMod = 0


'Mirror Lettering Mod
'	Normal Lettering =0
'	Backgrounded Lettering =1
'Change the value below to set option
MirrorLetteringMod = 0


'Ball Type Mod
'   Normal Ball = 0
'	Marbled Ball = 1
'Change the value below to set option
BallTypeMod = 0

'Drain Post Mod
'	No Drain Post = 0
'	Add Drain Post = 1
'Change the value below to set option
DrainPostMod = 0


'Flipper Rubbers Color Mod
'	Red Rubbers = 0
'	Blue Rubbers = 1
'Change the value below to set option
FlipperRubberColor = 0


'Instruction Cards Mod
' 	Normal Cards = 0
'	Random Cards = 1
'Change the value below to set option
InstructionCardsMod = 0


'Rudy Lazy Eye Mod
'	Normal Eye = 0
'	Lazy Eye = 1
'Change the value below to set option
LazyEyeMod = 1


'Rudy Face Twitch Mod
'	No Twitch = 0
'	Add Face Twitch Eye = 1
'Change the value below to set option
FaceTwitchMod = 0


'Rudy Mouth Hit Sound Effects
'	No Sound effect = 0
'	Random Sound Effects = 1
'Change the value below to set option
MouthHitSounds = 0


'Clock Toy Mod
'	No Clock = 0
'	Show Clock = 1
'Change the value below to set option
ClockMod = 1


'Balloons Toy Mod
'	No BallonsMod = 0
'	Show Balloons = 1
'Change the value below to set option
BalloonMod = 1


'Popcorn BucketToy Mod
'	No Popcorn Bucket = 0
'	Show Popcorn Bucket = 1
'Change the value below to set option
PopcornMod = 1


'Popcorn Cart Toy Mod
'	No Hotdog cart = 0
'	Show Hotdog Cart = 1
'Change the value below to set option
HotDogCartMod = 0


'Level Mod
'	No Level = 0
'	Show Level = 1
'Change the value below to set option
LevelMod = 1


'BALL SHADOW MOD
'	No Ball Shadow = 0
'	Add Ball Shadow = 1
Ballshadow = 1


'RINGING BELL SPECIAL MOD
'	No Bell = 0
'	Add Bell = 1
SpecialBellMod = 0


'PLAYFIELD SHADOW INTENSITY (adds additional visual depth)
'Usable range is 0 (lighter) - 100 (darker)
shadowopacity = 80



'Left Magna-Save Button toggles "Arcade Ambiant Sounds"
'Right Magna-Save Button toggles "Rudy Face Mod".

Const VolumeDial = 0.8
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5 			'Level of ramp rolling volume. Value between 0 and 1

'***************************************************************************************************************************************************************

' Sound Options
'

'Const VolDiv = 500    ' Smaller value - louder sound.

'Const VolBump   = 2    ' Bumpers multiplier.
'Const VolRol    = 1    ' Rollovers volume multiplier.
'Const VolRub    = 3    ' Rubbers volume multiplier.
'Const VolGates  = 1    ' Gates volume multiplier.
Const VolTarg   = 1    ' Targets multiplier.
'Const VolSpin   = 1.5  ' Spinners volume.
'Const VolFlip   = 1    ' Flipper volume.
'Const VolCol    = 3    ' Ball Collition volume.

'

'***************************************************************************************************************************************************************



Const DMDRotation= 0          '0= normal,  1= rotated by 90?
'Const cGameName="fh_L9"
'Const cGameName="fh_905"
Const cGameName="fh_905h"
'Const cGameName="fh_906h"
Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI=0
Const UseSync = 0
Const HandleMech = 0
Const SSolenoidOn="fx_solon"
Const SSolenoidOff=""
Const SCoin=""

Const tnob = 9

Const AmbientBallShadowOn = 1
Const DynamicBallShadowsOn = 1
Const RubberizerEnabled = 1
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Const ballsize = 25  'radius
Const ballmass = 1

Const UseVPMModSol = 1

LoadVPM "02060000", "WPC.VBS", 3.50
Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components

	Else

End if

'**************************************************
'			Solenoid callbacks
'**************************************************

SolCallback(1) = "SolOuthole"											'Out hole
SolCallback(2) = "SolRampDiverter"										'Ramp Diverter
SolCallback(3) = "bsHideout.SolOut"										'Rudy's Hideout kicker
SolCallback(4) = "SolKickout"											'Main kickout
SolCallback(5) = "SolTrapDoorO"											'Open Trap Door
SolCallback(6) = "SolTrapDoorC"											'Close Trap Door
SolCallback(7) = "knocker"												'Knocker
SolCallback(8) = "MBRelease"											'Multi-ball release
'SolCallback(9) =														'Left Bumper - Red
'SolCallback(10) =														'Right Bumper - White
'SolCallback(11) =														'Bottom Bumper - Blue
'SolCallback(12) =														'Left Sling
'SolCallback(13) =														'Right Sling
SolCallback(14) = "SolFlipperDiverter"									'Steps shooter lane diverter
SolCallback(15) = "ReleaseBall"								     		'Main trough kickout
SolCallback(16) = "bsRudySaucer.SolOut"									'Rudy's mouth kickout
'SolCallback(21) = "SolMouthMotor"										'Rudy Mouth On/Off
'SolCallback(22) = "SolMouthUpDown"										'Rudy Mouth Up/Down
SolCallback(25) = "SolEyesRight"										'Rudy eyes right
SolCallback(26) = "SolEyesOpen"											'Rudy lids open
SolCallback(27) = "SolEyesClosed"										'Rudy lids closed
SolCallback(28) = "SolEyesLeft"											'Rudy eyes left

'***Flashers***
'SolCallback(17) = "SetBlueDome"											'Blue Dome Flasher and X2 PF lights
'SolCallback(18) = "setlamp 118,"										'Flasher in front Rudy
'SolCallback(19) = "setlamp 119,"										'Center clock flasher
'SolCallback(20) = "setlamp 120,"										'Hot Dog Flasher
'SolCallback(23) = "SetRedDome"											'Red Dome Flasher and X2 PF lights
'SolCallback(24) = "SetWhiteDome" 										'White Dome Flasher and X2 PF lights

SolCallback(sLRFlipper) = "SolRFlipper"									'Right Flipper
SolCallback(sLLFlipper) = "SolLFlipper"									'Left Flipper


'**************************************************
'			Initiate Table
'**************************************************

Dim bsHideout, rudyjawmech, bsRudySaucer, cBall1, cBall2, cBall3


Sub Table1_Init()
LoadLUT
	PlayMusic()
	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Funhouse - Williams 1990"
		.Games(cGameName).Settings.Value("rol") = DMDRotation
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

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	vpmNudge.TiltSwitch = 14
	vpmNudge.Sensitivity = 1
	vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot,Bumper1,Bumper2,Bumper3)


	Set bsHideout = New cvpmBallStack
		bsHideout.InitSaucer sw46,46, 290, 25
		bsHideout.InitExitSnd SoundFX("solenoid",DOFContactors), SoundFX("none",DOFContactors)

	Set bsRudySaucer = New cvpmBallStack
		bsRudySaucer.InitSaucer sw65,65, 190, 9
		bsRudySaucer.InitExitSnd SoundFX("solenoid",DOFContactors), SoundFX("none",DOFContactors)

	WaSw28.IsDropped = 1
	TrapWall.IsDropped = 1

'	CheckMaxBalls 'Allow balls to be created at table start up

    Set RudyJawMech = New cvpmMech
	With RudyJawMech
		.MType = vpmMechOneDirSol + vpmMechStopEnd + vpmMechNonLinear + vpmMechFast
		.Sol1 = 21
		.Sol2 = 22
		.length = 95
		.steps = 24
		.callback = getRef("UpdateJawRudy")
		.start
	End With

    PrevGameOver = 0
	SetOptions
	SetGIColor
	InitLampsNF





    '************  Trough	**************************
	set cball1 = sw72.CreateSizedballWithMass(Ballsize,Ballmass)
	set cball2 = sw74.CreateSizedballWithMass(Ballsize,Ballmass)
	set cball3 = sw63.CreateSizedballWithMass(Ballsize,Ballmass)

	SetBallMod

	Controller.Switch(72) = 1
	Controller.Switch(74) = 1
	Controller.Switch(63) = 1


End Sub

'//////////////////////////////////////////////////////////////////////
'// Ball
'//////////////////////////////////////////////////////////////////////

If BallBright Then
	table1.BallImage = "ball_HDR_brighter"
Else
	table1.BallImage = "ball_HDR3bdark"
End If

'//////////////////////////////////////////////////////////////////////
'// LUT
'//////////////////////////////////////////////////////////////////////

'//////////////---- LUT (Colour Look Up Table) ----//////////////
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
'10 = HauntFreaks Desaturated
'11 = Tomate Washed Out 
'12 = VPW Original 1 to 1
'13 = Bassgeige
'14 = Blacklight
'15 = B&W Comic Book

Dim LUTset, DisableLUTSelector, LutToggleSound, bLutActive
LutToggleSound = True
LoadLUT
'LUTset = 0			' Override saved LUT for debug
SetLUT
DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1

Sub Table1_exit:SaveLUT:Controller.Stop:End Sub

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
		Case 10: LUTBox.text = "HauntFreaks Desaturated"
  		Case 11: LUTBox.text = "Tomate washed out"
        Case 12: LUTBox.text = "VPW original 1on1"
        Case 13: LUTBox.text = "bassgeige"
        Case 14: LUTBox.text = "blacklight"
        Case 15: LUTBox.text = "B&W Comic Book"
		Case 16: LUTBox.text = "Skitso New ColorLut"
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

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "FunhouseLUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub
Sub LoadLUT
    bLutActive = False
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=0
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "FunhouseLUT.txt") then
		LUTset=0
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "FunhouseLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=0
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub

'******************************
' 	Keys
'******************************

Dim BGSounds


Sub Table1_KeyDown(ByVal keycode)
'LUT controls
If keycode = LeftMagnaSave Then bLutActive = True
    If keycode = RightMagnaSave Then
            If bLutActive Then
                    if DisableLUTSelector = 0 then
                LUTSet = LUTSet  - 1
                if LutSet < 0 then LUTSet = 16
                SetLUT
                ShowLUT
            End If
        End If
        End If

If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
                Select Case Int(rnd*3)
                        Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                        Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                        Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
End Select
        End If
	If Keycode = KeyFront Then Controller.Switch(23) = 1
	If keycode = PlungerKey Then : Plunger1.Pullback :Plunger2.Pullback :PlaySoundAt"fx_plungerpull",Plunger1:PlaySoundAt"fx_plungerpull",Plunger2 'PlaySound "fx_plungerpull"
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress End If
If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress End If
If keycode = LeftTiltKey Then Nudge 90, 2:PlaySound SoundFX("fx_nudge",0)
	If keycode = RightTiltKey Then Nudge 270, 2:PlaySound SoundFX("fx_nudge",0)
	If keycode = CenterTiltKey Then Nudge 0, 3:PlaySound SoundFX("fx_nudge",0)



	If vpmKeyDown(keycode) Then Exit Sub

'   '************************   Start Ball Control 1/3
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










End Sub

Sub Table1_KeyUp(ByVal keycode)
'LUT controls
If keycode = LeftMagnaSave Then bLutActive = False
If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress End If
If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress End If
	If Keycode = KeyFront Then Controller.Switch(23) = 0
	If keycode = PlungerKey Then : Plunger1.Fire :Plunger2.Fire :PlaySoundAt"plunger2",Plunger1:PlaySoundAt"plunger2",Plunger2' PlaySound "plunger2"
	If vpmKeyUp(keycode) Then Exit Sub


'    '************************   Start Ball Control 2/3
'    if keycode = 203 then bcleft = 0        ' Left Arrow
'    if keycode = 200 then bcup = 0          ' Up Arrow
'    if keycode = 208 then bcdown = 0        ' Down Arrow
'    if keycode = 205 then bcright = 0       ' Right Arrow
'    '************************   End Ball Control 2/3
End Sub

''************************   Start Ball Control 3/3
'Sub StartControl_Hit()
'    Set ControlBall = ActiveBall
'    contballinplay = true
'End Sub
'
'Sub StopControl_Hit()
'    contballinplay = false
'End Sub
'
'Dim bcup, bcdown, bcleft, bcright, contball, contballinplay, ControlBall, bcboost
'Dim bcvel, bcyveloffset, bcboostmulti
'
'bcboost = 1     'Do Not Change - default setting
'bcvel = 4       'Controls the speed of the ball movement
'bcyveloffset = -0.01    'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
'bcboostmulti = 3    'Boost multiplier to ball veloctiy (toggled with the B key)
'
'Sub BallControl_Timer()
'    If Contball and ContBallInPlay then
'        If bcright = 1 Then
'            ControlBall.velx = bcvel*bcboost
'        ElseIf bcleft = 1 Then
'            ControlBall.velx = - bcvel*bcboost
'        Else
'            ControlBall.velx=0
'        End If
'
'        If bcup = 1 Then
'            ControlBall.vely = -bcvel*bcboost
'        ElseIf bcdown = 1 Then
'            ControlBall.vely = bcvel*bcboost
'        Else
'            ControlBall.vely= bcyveloffset
'        End If
'    End If
'End Sub
''************************   End Ball Control 3/3

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub

'************************
'      RealTime Updates
'************************

Dim Gate4Angle,GateSpeed,Gate1Open,Gate1Angle,OldGameTime
'Gate1Open=0:Gate1Angle=0:GateSpeed = 5

'Sub Gate1_Hit():Gate1Open=1:Gate1Angle=0:PlaySound "fx_gate":End Sub

'Set MotorCallback = GetRef("GameTimer")

Sub FlippersTimer_Timer()

	PrStepGate.ObjRotZ = StepGate2.CurrentAngle + 90
	Prim_Diverter.RotZ = RampDiv.CurrentAngle
'	FlipperL.RotZ = LeftFlipper.CurrentAngle
'	FlipperR.RotZ = RightFlipper.CurrentAngle
'	FlipperUL.RotZ = LeftFlipper1.CurrentAngle


	Gate4Angle = Int(Gate4.CurrentAngle)
	If Gate4Angle > 0 then
		pGate4_switch.ObjRotY = sin( (Gate4Angle * -1) * (2*PI/180)) * 5
	Else
		pGate4_switch.ObjRotY = sin( (Gate4Angle * 1) * (2*PI/180)) * 5
	End If

    pGate4.Rotx = Gate4.CurrentAngle' + 90
    p_gate1.RotZ = Gate1.CurrentAngle' +90
	batleftshadow1.objrotz = leftFlipper1.CurrentAngle
	batleftshadow.objrotz = LeftFlipper.CurrentAngle
	batrightshadow.objrotz = RightFlipper.CurrentAngle
End Sub



'**********************************
'  Flippers
'**********************************


'//////////////////////////////////////////////////////////////////////
'// FLIPPERS 
'//////////////////////////////////////////////////////////////////////

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
        LeftFlipper1.RotateToEnd
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		LeftFlipper.RotateToStart
        LeftFlipper1.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
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


' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
End Sub

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -2.7        
'        AddPt "Polarity", 2, 0.33, -2.7
'        AddPt "Polarity", 3, 0.37, -2.7        
'        AddPt "Polarity", 4, 0.41, -2.7
'        AddPt "Polarity", 5, 0.45, -2.7
'        AddPt "Polarity", 6, 0.576,-2.7
'        AddPt "Polarity", 7, 0.66, -1.8
'        AddPt "Polarity", 8, 0.743, -0.5
'        AddPt "Polarity", 9, 0.81, -0.5
'        AddPt "Polarity", 10, 0.88, 0
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
'
'
'
''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -3.7        
'        AddPt "Polarity", 2, 0.33, -3.7
'        AddPt "Polarity", 3, 0.37, -3.7
'        AddPt "Polarity", 4, 0.41, -3.7
'        AddPt "Polarity", 5, 0.45, -3.7 
'        AddPt "Polarity", 6, 0.576,-3.7
'        AddPt "Polarity", 7, 0.66, -2.3
'        AddPt "Polarity", 8, 0.743, -1.5
'        AddPt "Polarity", 9, 0.81, -1
'        AddPt "Polarity", 10, 0.88, 0
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
'
'


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
	Dim b, BOT
	BOT = GetBalls

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
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
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

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
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
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
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
		Dim b, BOT
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

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub

' **********************************
'  			Holes & Subway
' **********************************

 Dim aBall, aZpos
 Dim bBall, bZpos

'***Drain, VUKs & Saucers***
Sub sw46_Hit:bsHideout.addball 0 : End Sub
Sub sw65_Hit:bsRudySaucer.addball 0 : playsoundatball "Saucer_Enter_2" : End Sub
Sub sw67_Hit()
PlaySoundAtBallVol "fx_hole",1
'	Playsound "fx_hole"
	vpmTimer.PulseSw 67
End Sub

'***Wind Tunnel Hole***
Sub subwayenter_hit()
PlaySoundAtBallVol "VUKEnter",1
	activeball.color = RGB(10,10,10)
'	PlaySound "Fx_subwayenter"
End Sub

'***Trap Door Hole***
Sub subwayenter1_hit()
PlaySoundAtBallVol "VUKEnter",1
	activeball.color = RGB(10,10,10)
'	PlaySound "Fx_subwayenter"
End Sub

Sub sw44_hit()
	activeball.color = RGB(255,255,255)
'	PlaySound "kicker_enter_center"
	vpmTimer.PulseSw 44
End Sub

'****Trap Door***
Sub TrigSub1_hit
PlaySoundAtBallVol "fx_subway",1
End Sub

Sub TrigSub2_hit
PlaySoundAtBallVol "fx_subway",1
End Sub


'**************************************
'		Tunnel Kickout
'**************************************
Sub Destroyer_hit():me.destroyball:end sub	'debug
Sw58k1.enabled = 0
Sw58k.enabled = 1

Sub SolKickout(enabled)
	If Enabled then
		if sw58k1.ballcntover = 1 then
			sw58k1.kick 22,55	'22.5, 55 '55 = strength
		Else
			sw58k1.enabled = 0	'disable second chute kicker
			sw58k.kick 20,55	'55 = strength
		End If
'		bsChute.SolExit true
		sw58.enabled= 0
		vpmtimer.addtimer 600, "sw58.enabled= 1'"
		PlaySoundAt SoundFx("Popper",DOFContactors),Primitive90
	End If
End Sub

Sub sw58_Hit()
PlaySoundAtBallVol "VUKEnter",1
'	Playsound "fx_vuk_enter"
End Sub

Sub sw58k_Hit()
	PlaySoundAtBallVol "VUKEnter",1
'	Stopsound "fx_subway"
	'Playsound "fx_kicker_catch"
	Controller.Switch(58) = 1
	Sw58k1.enabled = 1	'enable second chute kicker
End Sub

Sub sw58k_UnHit()
	Controller.Switch(58) = 0
End Sub



'**************************************
'		Lock Mech
'**************************************

Dim lockdir

Sub MBRelease(enabled)
	If enabled then
		PlaySoundAt SoundFx("fx_lock_exit",DOFContactors),LockFlipper
		waSw28.IsDropped = 1
		LockFlipper.rotatetoend
		MoveLock.enabled = 1
		lockdir=-30
	End If
End Sub

Sub MoveLock_Timer()
	Lock_Release_Prim.objRotZ = Lock_Release_Prim.objRotZ + lockdir
	If Lock_Release_Prim.objRotZ <=-210 Then:lockdir = 30:LockFlipper.rotatetostart::end if
	If Lock_Release_Prim.objRotZ >=0 Then Lock_Release_Prim.objRotZ=0: Me.Enabled = 0
End Sub


Sub Sw25_Hit:Controller.Switch(25) = 1:WaSW28.IsDropped = 0 :End Sub
Sub Sw25_UnHit:Controller.Switch(25) = 0:End Sub

Sub Sw27_hit:Controller.Switch(27) = 1:WaSW28.IsDropped = 0 :End Sub
Sub Sw27_unhit:Controller.Switch(27) = 0:End Sub

Sub SW28_Hit:Controller.Switch(28) = 1 :End Sub
Sub SW28_UnHit:Controller.Switch(28) = 0:End Sub


'**************************************
'				Diverters
'**************************************

'******** Step Gate Diverter
Sub SolFlipperDiverter(enabled)
     If Enabled Then
		PlaySoundAt SoundFx("fx_divRR",DOFContactors),StepGate2
    '     PlaySound SoundFX("fx_divRR",DOFContactors)
		StepGate2.RotateToEnd
     Else
		StepGate2.RotateToStart
     End If
End Sub

'******** Ramp Diverter
Sub SolRampDiverter(enabled)
	If Enabled Then
		PlaySoundAt SoundFx("fx_divLR",DOFContactors),RampDiv
	'	PlaySound SoundFX("fx_divLR",DOFContactors)
		RampDiv.RotateToEnd
     Else
		RampDiv.RotateToStart
     End If
End Sub

' **********************************
'		 Bumpers
' **********************************

Sub Bumper1_Hit: vpmTimer.PulseSw(18) : RandomSoundBumperTop Bumper1:End Sub
Sub Bumper2_Hit: vpmTimer.PulseSw(77) : RandomSoundBumperMiddle Bumper2:End Sub
Sub Bumper3_Hit: vpmTimer.PulseSw(68) : RandomSoundBumperBottom Bumper3:End Sub

' **********************************
'		SlingShots Animation
' **********************************
Dim YMultiplierSling
YMultiplierSling = 10

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 41
		RandomSoundSlingshotLeft sling2
 '   PlaySound SoundFX("fx_slingshotL",DOFContactors), 0, 1, 0.05, 0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    If activeball.vely > 0 then activeball.vely = activeball.vely - abs(activeball.vely/YMultiplierSling) 
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -30
        Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 53
		RandomSoundSlingshotRight sling1
 '   PlaySound SoundFX("fx_slingshotR",DOFContactors),0,1,-0.05,0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	If activeball.vely > 0 then activeball.vely = activeball.vely - abs(activeball.vely/YMultiplierSling) 
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 2:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -30
        Case 3:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub


Dim YMultiplier
YMultiplier = .50

Sub Trubber_hit
If activeball.vely > 0 then activeball.vely = activeball.vely - abs(activeball.velyymultiplier) 
End Sub

Sub wall65_hit
If activeball.vely > 0 then activeball.vely = activeball.vely - abs(activeball.vely/ymultiplier) 
End Sub

Sub wall20_hit
If activeball.vely > 0 then activeball.vely = activeball.vely - abs(activeball.vely/ymultiplier) 
End Sub

Sub wall5_hit
If activeball.vely > 0 then activeball.vely = activeball.vely - abs(activeball.vely/ymultiplier) 
End Sub

Sub wall54_hit
If activeball.vely > 0 then activeball.vely = activeball.vely - abs(activeball.vely/ymultiplier) 
End Sub

Sub wall41_hit
If activeball.vely > 0 then activeball.vely = activeball.vely - abs(activeball.vely/ymultiplier) 
End Sub

Sub wall58_hit
If activeball.vely > 0 then activeball.vely = activeball.vely - abs(activeball.vely/ymultiplier) 
End Sub

' **********************************
'  Trap Door Animation -Shoopity-
' **********************************

Dim TrapDir
Dim TrapSpeed : TrapSpeed = 2

Sub SolTrapDoorO(enabled)
		TrapDoorRampUp.Collidable = 1
		Trapwall.isdropped=0
		TrapDir = 1
		TrapMover.Enabled = 1
		Controller.Switch(76) = 0
		If enabled Then PlaySoundAt SoundFx("fx_solon",DOFContactors),PrTrap
End Sub

Sub SolTrapDoorC(enabled)
	TrapDir = -1
	TrapMover.Enabled = 1
	If enabled Then PlaySoundAt SoundFx("fx_solon",DOFContactors),PrTrap
End Sub

Sub TrapMover_Timer()
	PrTrap.RotX = PrTrap.RotX + TrapSpeed*TrapDir
	If PrTrap.RotX >= 120 Then
		PrTrap.RotX = 120
		me.enabled = 0
	End If
	If PrTrap.RotX >= 102 Then
		TrapDrop.enabled = 1
		trapwall.isdropped = 1
	Else
		TrapDrop.enabled = 0
		TrapDoorRampDown.Collidable = 1

		trapwall.isdropped = 0
	End If
	If PrTrap.RotX <= 90 Then
		PrTrap.RotX = 90
		trapwall.isdropped = 1
		TrapDoorRampUp.Collidable = 0
		Controller.Switch(76) = 1
		me.enabled = 0
	End If
End Sub

'Sub TrapDrop_Hit():TrapDrop.timerenabled=1:debug.print "hit":End Sub
Sub TrapDrop_unHit()
	TrapDrop.timerenabled=1
End Sub

Sub TrapDrop_Timer()
	TrapDoorRampDown.Collidable = 0

	TrapDrop.timerenabled = 0
End Sub



Sub TrapDoorEnter1_Hit():TrapDoorHit:End Sub
Sub TrapDoorEnter2_Hit():TrapDoorHit:End Sub

Sub TrapDoorHit()
	PlaySoundAtBallVol "kicker_enter_center",1
End Sub


' **********************************
'  Rudy's Mouth Animation
' **********************************

Sub UpdateJawRudy(aNewPos,aSpeed,aLastPos)
	MoveMouth.Enabled = 1
End Sub

Sub MoveMouth_Timer()
	If RudyJawMech.position > PrMouth.RotX then:PrMouth.RotX = PrMouth.RotX +(0.5):End if
	If RudyJawMech.position < PrMouth.RotX then:PrMouth.Rotx = PrMouth.RotX -(0.5):End if
	If PrMouth.RotX < 15 then WaMouth.isDropped = 1:Else WaMouth.isDropped = 0
	prMouthb.Rotx = prMouth.Rotx
	prMouthSpringA.Size_Z = 29 + (PrMouth.RotX / 10)
	prMouthSpringB.Size_Z = 29 + (PrMouth.RotX / 10)
End Sub

' **********************************
'  Rudy's Eyes Animation -Shoopity-
' **********************************

Dim EyeDest
Dim EyeSpeed : EyeSpeed = 10

Sub SolEyesRight(enabled)
	PlaySoundAt "FX_Rudysol",PrEyeR
	'Playsound "FX_Rudysol"
	MoveEyes.Enabled = 1
	If enabled Then EyeDest = 1 Else EyeDest = 0
End Sub
Sub SolEyesLeft(enabled)
	PlaySoundAt "FX_Rudysol1",PrEyeL
'	Playsound "FX_Rudysol1"
	MoveEyes.Enabled = 1
	If enabled Then EyeDest = -1 Else EyeDest = 0
End Sub

Sub MoveEyes_Timer()
	Select Case EyeDest
	Case -1:
		PrEyeL.RotZ = PrEyeL.RotZ + EyeSpeed
'		PrEyeR.RotZ = PrEyeR.RotZ + EyeSpeed
		If PrEyeL.RotZ >= 22 Then
			PrEyeL.RotZ = 22
'			PrEyeR.RotZ = 22
			me.enabled = 0
		End If
	Case 0:
		If PrEyeL.RotZ <= 2 Then
			PrEyeL.RotZ = PrEyeL.RotZ + EyeSpeed
'			PrEyeR.RotZ = PrEyeR.RotZ + EyeSpeed
		ElseIf PrEyeL.RotZ >= 2 Then
			PrEyeL.RotZ = PrEyeL.RotZ - EyeSpeed
'			PrEyeR.RotZ = PrEyeR.RotZ - EyeSpeed
		End If
		If PrEyeL.RotZ <= 2+(EyeSpeed+1) AND PrEyeL.RotZ >= 2-(EyeSpeed+1) Then
			PrEyeL.RotZ = 2
'			PrEyeR.RotZ = 2
			me.enabled = 0
		End If
	Case 1:
		PrEyeL.RotZ = PrEyeL.RotZ - EyeSpeed
'		PrEyeR.RotZ = PrEyeR.RotZ - EyeSpeed
		If PrEyeL.RotZ <= -22 Then
			PrEyeL.RotZ = -22
'			PrEyeR.RotZ = -22
			me.enabled = 0
		End If
	End Select
	If LazyEyeMod = 1 Then
		PrEyeR.RotZ = PrEyeL.RotZ / 8
	Else
		PrEyeR.RotZ = PrEyeL.RotZ
	End If
	PrEye_Slider.RotY = PrEyeL.RotZ / 3
End Sub

' **********************************
'  Rudy's Lids Animation -Shoopity-
' **********************************

Dim LidSpeed : LidSpeed = 10
Dim LidDest


Sub SolEyesOpen(enabled)
	MoveLids.Enabled = 1
	PlaySoundAt "FX_Rudysol",prLidsThingerA
'	Playsound "FX_Rudysol"
	If enabled Then
		LidDest = 1
	Else
		LidDest = 0
		prLidsThingerA.TransZ = -20
		prLidsThingerB.RotX = 95
	End If
End Sub

Sub SolEyesClosed(enabled)
	MoveLids.Enabled = 1
	PlaySoundAt "FX_Rudysol1",prLidsThingerB
'	Playsound "FX_Rudysol1"
	If enabled Then
		LidDest = -1
		prLidsThingerA.TransZ = 0
		prLidsThingerB.RotX = 90
	Else
'		LidDest = 0
	End If
End Sub

Sub MoveLids_Timer()
	Select Case LidDest
''''Lids Raised
	Case 1:
		PrLids.RotX = PrLids.RotX + LidSpeed
		If PrLids.RotX >= 5 Then  'was 55
			PrLids.RotX = 5	 	'was 55
			me.enabled = 0
		End If
		prLidsThingerB.TransY = (PrLids.RotX / 8) * -1
		prLidsThingerC.TransY = (PrLids.RotX / 8) * -1
		pSmallSpring.Size_Y = (24 - (prLidsThingerC.TransY * -2)) *.75

''''Lids Normal -midpoint
	Case 0:
		If PrLids.RotX <= -30 Then
			PrLids.RotX = PrLids.RotX + LidSpeed
		ElseIf PrLids.RotX >= -30 Then
			PrLids.RotX = PrLids.RotX - LidSpeed
		End If
		If PrLids.RotX <= (LidSpeed+1) AND PrLids.RotX >= -(LidSpeed+1) Then
			PrLids.RotX = -30
			me.enabled = 0
		End If
		prLidsThingerB.TransY = (PrLids.RotX / 8) * -1
		prLidsThingerC.TransY = (PrLids.RotX / 8) * -1
		pSmallSpring.Size_Y = (24 - (prLidsThingerC.TransY * -2)) *.75

''''Lids Lowered
	Case -1:
		PrLids.RotX = PrLids.RotX - LidSpeed
		If PrLids.RotX <= -75 Then  'was 115
			PrLids.RotX = -75		'was 115
			me.enabled = 0
		End If
	End Select
'	prLidsThingerB.TransY = (PrLids.RotX / 8) * -1
'	prLidsThingerC.TransY = (PrLids.RotX / 8) * -1
'	pSmallSpring.Size_Y = (24 - (prLidsThingerC.TransY * -2)) *.75
'	PrLids1.RotX = PrLids.RotX
End Sub

Sub knocker(Enabled)
	if enabled then
		 PlaySound SoundFX("fx_knocker",DOFKnocker)
		if SpecialBellMod = 1 Then
		Playsound "Bell"
		end If
	end if
End Sub



' **********************************
'  Rudy Twitch and Punch Sounds
' **********************************

Dim punchtype,TwitchCounter

Sub WaMouth_hit()

	If FaceTwitchMod = 1 Then:TwitchTimer.enabled=1:TwitchCounter=0

    Select Case MouthHitSounds
        Case 0:	PlaySoundAt "fx_flip_hit_2",PrMouth:'Playsound "fx_Flipperdown" 'normal sound
		Case 1 'random sounds
			    Select Case Int(Rnd * 13) + 1
					Case 1:Playsound "punch"
					Case 2:Playsound "toasty"
					Case 3:Playsound "finish"
					Case 4:Playsound "Coocoo"
					Case 5:Playsound "glass"
					Case 6:Playsound "ricochet1"
					Case 7:Playsound "ricochet2"
					Case 8:Playsound "doink"
					Case 9:Playsound "drama"
					Case 10:Playsound "Cry"
					Case 11:Playsound "Excellent"
					Case 12:Playsound "Silly"
					Case 13:Playsound "Boing"
				End Select
		Case 2:Playsound "punch"
		Case 3:Playsound "toasty"
		Case 4:Playsound "finish"
		Case 5:Playsound "Coocoo"
		Case 6:Playsound "glass"
		Case 7:Playsound "ricochet1"
		Case 8:Playsound "ricochet2"
		Case 9:Playsound "doink"
		Case 10:Playsound "drama"
		Case 11:Playsound "Cry"
		Case 12:Playsound "Excellent"
		Case 13:Playsound "Silly"
		Case 14:Playsound "Boing"

    End Select

End Sub

Sub TwitchTimer_Timer()
	Dim TwitchMove
	Select Case TwitchCounter
		Case 0:TwitchMove=1:if RudyType=3 Then:PrLids.visible=false
		Case 1:TwitchMove=2
		Case 2:TwitchMove=3
		Case 3:TwitchMove=4
		Case 4:TwitchMove=5
		Case 5:TwitchMove=5
		Case 6:TwitchMove=4
		Case 7:TwitchMove=3
		Case 8:TwitchMove=2
		Case 9:TwitchMove=1
		Case 10:TwitchMove=0:if RudyType=3 Then:PrLids.visible=true:Me.Enabled = 0
	End Select

	PrRudy.TransX=TwitchMove/2:PrRudy.TransY=-TwitchMove:PrRudy.TransZ=TwitchMove
	PrRudy1.TransX=TwitchMove/2:PrRudy1.TransY=-TwitchMove:PrRudy1.TransZ=TwitchMove

	PrMouth.TransX=TwitchMove/2:PrMouth.TransY=-TwitchMove:PrMouth.TransZ=TwitchMove
	PrMouthb.TransX=TwitchMove/2:PrMouthb.TransY=-TwitchMove:PrMouthb.TransZ=TwitchMove
	PrMouthSpringA.TransX=TwitchMove/2:PrMouthSpringA.TransY=-TwitchMove:PrMouthSpringA.TransZ=TwitchMove
	PrMouthSpringB.TransX=TwitchMove/2:PrMouthSpringB.TransY=-TwitchMove:PrMouthSpringB.TransZ=TwitchMove

	If RudyType=3 Then
		PrEyeL.TransX=-TwitchMove:PrEyeL.TransY=TwitchMove*2:PrEyeL.TransZ=TwitchMove
		PrEyeR.TransX=-TwitchMove:PrEyeR.TransY=TwitchMove*2:PrEyeR.TransZ=TwitchMove
	End If

	TwitchCounter = TwitchCounter + 1
End Sub


' **********************************
'  				Switches
' **********************************

'***Wire Triggers***
Sub Sw42_Hit:Controller.Switch(42) = 1 : End Sub
Sub Sw42_UnHit:Controller.Switch(42) = 0:End Sub
Sub Sw43_Hit:Controller.Switch(43) = 1 : End Sub
Sub Sw43_UnHit:Controller.Switch(43) = 0:End Sub
Sub Sw47_Hit:Controller.Switch(47) = 1 : End Sub
Sub Sw47_UnHit:Controller.Switch(47) = 0:End Sub
Sub Sw52_Hit:Controller.Switch(52) = 1 : End Sub
Sub Sw52_UnHit:Controller.Switch(52) = 0:End Sub
Sub Sw57_Hit:Controller.Switch(57) = 1 : End Sub
Sub Sw57_UnHit:Controller.Switch(57) = 0:End Sub
Sub Sw61_Hit:Controller.Switch(61) = 1 : End Sub
Sub Sw61_UnHit:Controller.Switch(61) = 0:End Sub
Sub Sw62_Hit:Controller.Switch(62) = 1 : Stopsound "intro":End Sub
Sub Sw62_UnHit:Controller.Switch(62) = 0:End Sub
Sub Sw66_Hit:Controller.Switch(66) = 1 : End Sub
Sub Sw66_UnHit:Controller.Switch(66) = 0:End Sub
Sub Sw71_Hit:Controller.Switch(71) = 1 : End Sub
Sub Sw71_UnHit:Controller.Switch(71) = 0:End Sub
Sub sw75_Hit:Controller.Switch(75) = 1 : End Sub
Sub sw75_UnHit:Controller.Switch(75) = 0:End Sub


' **********************************
'  				Targets
' **********************************

Sub DoubleTarget1_hit
vpmTimer.PulseSw 32:pSW32.TransY = -4:Target32Step = 1:sw32.TimerEnabled = 1 
vpmTimer.PulseSw 37:pSW37.TransY = -4:Target37Step = 1:sw37.TimerEnabled = 1
End Sub

Sub DoubleTarget2_hit
vpmTimer.PulseSw 34:pSW34.TransY = -4:Target34Step = 1:sw34.TimerEnabled = 1
vpmTimer.PulseSw 37:pSW37.TransY = -4:Target37Step = 1:sw37.TimerEnabled = 1
End Sub

Dim zMultiplier

Sub sw17_Hit:t:pSW17.TransY = -3:End Sub

DIM target17step
Sub sw17_Hit
    Select Case Int(Rnd * 4) + 1
        Case 1: zMultiplier = 1.55
        Case 2: zMultiplier = 1.2
        Case 3: zMultiplier = .9
        Case 4: zMultiplier = .7
    End Select
    activeball.velz = activeball.velz*zMultiplier:vpmTimer.PulseSw 17:pSW17.TransY = -3:Target17Step = 1:sw17a.Enabled = 1

End Sub

Sub sw17a_timer()
	Select Case Target17Step

		Case 1:pSW17.TransY = -1
		Case 2:pSW17.TransY = 2
        Case 3:pSW17.TransY = 0:Me.Enabled = 0
     End Select
	Target17Step = Target17Step + 1
End Sub


DIM target31step
Sub sw31_Hit
    Select Case Int(Rnd * 4) + 1
        Case 1: zMultiplier = 1.55
        Case 2: zMultiplier = 1.2
        Case 3: zMultiplier = .9
        Case 4: zMultiplier = .7
    End Select
    activeball.velz = activeball.velz*zMultiplier:vpmTimer.PulseSw 31:pSW31.TransY = -3:Target31Step = 1:sw31a.Enabled = 1

End Sub

Sub sw31a_timer()
	Select Case Target31Step

		Case 1:pSW31.TransY = -1
		Case 2:pSW31.TransY = 2
        Case 3:pSW31.TransY = 0:Me.Enabled = 0
     End Select
	Target31Step = Target31Step + 1
End Sub


DIM target32step
Sub sw32_Hit:vpmTimer.PulseSw 32:pSW32.TransY = -3:Target32Step = 1:Me.TimerEnabled = 1:End Sub

Sub sw32_timer()
	Select Case Target32Step

		Case 1:pSW32.TransY = -1
		Case 2:pSW32.TransY = 2
        Case 3:pSW32.TransY = 0:Me.TimerEnabled = 0
     End Select
	Target32Step = Target32Step + 1
End Sub


DIM target34step
Sub sw34_Hit:vpmTimer.PulseSw 34:pSW34.TransY = -3:Target34Step = 1:Me.TimerEnabled = 1:End Sub

Sub sw34_timer()
	Select Case Target34Step

		Case 1:pSW34.TransY = -1
		Case 2:pSW34.TransY = 2
        Case 3:pSW34.TransY = 0:Me.TimerEnabled = 0
     End Select
	Target34Step = Target34Step + 1
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


DIM target54step
'Sub sw31_Hit:activeball.velz = activeball.velz*zMultiplier:vpmTimer.PulseSw 31:pSW31.TransY = -3:Target31Step = 1:sw31a.Enabled = 1:PlaySoundAtVol SoundFx("fx_target",DOFTargets),Primitive98, VolTarg:End Sub



Sub sw54_Hit
    Select Case Int(Rnd * 4) + 1
        Case 1: zMultiplier = 1.55
        Case 2: zMultiplier = 1.2
        Case 3: zMultiplier = .9
        Case 4: zMultiplier = .7
   End Select
   activeball.velz = activeball.velz*zMultiplier:vpmTimer.PulseSw 54:pSW54.TransY = -3:Target54Step = 1:sw54a.Enabled = 1
End Sub

Sub sw54a_timer()
	Select Case Target54Step

		Case 1:pSW54.TransY = -1
		Case 2:pSW54.TransY = 2
        Case 3:pSW54.TransY = 0:Me.Enabled = 0
     End Select
	Target54Step = Target54Step + 1
End Sub


DIM target64step
Sub sw64_Hit
    Select Case Int(Rnd * 4) + 1
        Case 1: zMultiplier = 1.55
        Case 2: zMultiplier = 1.2
        Case 3: zMultiplier = .9
        Case 4: zMultiplier = .7
    End Select
    activeball.velz = activeball.velz*zMultiplier:vpmTimer.PulseSw 64:pSW64.TransY = -3:Target64Step = 1:sw64a.Enabled = 1
End Sub

Sub sw64a_timer()
	Select Case Target64Step

		Case 1:pSW64.TransY = -1
		Case 2:pSW64.TransY = 2
        Case 3:pSW64.TransY = 0:Me.Enabled = 0
     End Select
	Target64Step = Target64Step + 1
End Sub


'***Ramp Triggers***
Sub sw15_Hit:vpmTimer.PulseSw(15):End Sub 'Step Lights Frenczy
Sub sw16_Hit:vpmTimer.PulseSw(16):End Sub 'Upper Ramp Switch
Sub sw26_Hit:vpmTimer.PulseSw(26):End Sub 'Step Light Exit Ball
Sub sw36_Hit:vpmTimer.PulseSw(36):End Sub 'Step 500,000
Sub sw35_Hit:vpmTimer.PulseSw(35):End Sub 'Step Tracker lower
Sub sw38_Hit:vpmTimer.PulseSw(38):End Sub 'Step Tracker upper
Sub sw48_Hit:vpmTimer.PulseSw(48):End Sub 'Ramp Exit Track
Sub sw45_Hit:vpmTimer.PulseSw(45):activeball.color = RGB(255,255,255):End Sub 'Trap door score
Sub sw55_Hit:vpmTimer.PulseSw(55):End Sub 'Steps Superdog

'***Gate Triggers***
Sub sw33_Hit:vpmTimer.PulseSw(33):End Sub 'Upper left gangway roll under
Sub sw56_Hit:vpmTimer.PulseSw(56):End Sub 'Main Ramp Entrance
'Sub sw56_Hit:vpmTimer.PulseSw 78:sw56.timerenabled = 1:End Sub
'Sub sw56_timer:sw56.timerenabled= 0:End Sub


'***Rudy's mouth kicker animation***
Dim RKStep

Sub sw65_timer()
	Select Case RKStep
		Case 0:pRudyKick.TransY = 35
		Case 1:pRudyKick.TransY = 18
		Case 2:pRudyKick.TransY = 0:me.TimerEnabled = false:RKStep = 0
	End Select
	RKStep = RKStep + 1
End Sub

'******** Hidden switches
Sub sw51_Hit:vpmTimer.PulseSw(51):End Sub 'Dummy Jaw (opto)


'******************
'Switch animations
'*******************

Sub sw35_Hit:Controller.Switch(35) = 1:sw35.timerenabled = true:End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub

'***switch 35 animation***

Const Switch35min = 0
Const Switch35max = -20
Dim Switch35dir
Switch35dir = -2

Sub sw35_timer()
 pRampSwitch1B.RotY = pRampSwitch1B.RotY + Switch35dir
	If pRampSwitch1B.RotY >= Switch35min Then
		sw35.timerenabled = False
		pRampSwitch1B.RotY = Switch35min
		Switch35dir = -2
	End If
	If pRampSwitch1B.RotY <= Switch35max Then
		Switch35dir = 4
	End If
End Sub


Sub sw48_Hit:Controller.Switch(48) = 1:sw48.timerenabled = true:End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub

'***switch 48 animation***

Const Switch48min = 0
Const Switch48max = -20
Dim Switch48dir
Switch48dir = -2

Sub sw48_timer()
	pRampSwitch3B.RotY = pRampSwitch3B.RotY + Switch48dir

	If pRampSwitch3B.RotY >= Switch48min Then
		sw48.timerenabled = False
		pRampSwitch3B.RotY = Switch48min
		Switch48dir = -2
	End If

	If pRampSwitch3B.RotY <= Switch48max Then
		Switch48dir = 4
	End If
End Sub

Sub sw38_Hit:Controller.Switch(38) = 1:sw38.timerenabled = true:End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub

'***switch 38 animation***

Const Switch38min = 0
Const Switch38max = -20
Dim Switch38dir
Switch38dir = -2

Sub sw38_timer()
	pRampSwitch2B.RotY = pRampSwitch2B.RotY + Switch38dir
	If pRampSwitch2B.RotY >= Switch38min Then
		sw38.timerenabled = False
		pRampSwitch2B.RotY = Switch38min
		Switch38dir = -2
	End If
	If pRampSwitch2B.RotY <= Switch38max Then
		Switch38dir = 4
	End If
End Sub


Sub sw15_Hit:Controller.Switch(15) = 1:sw15.timerenabled = true:End Sub
Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub

'***switch 15 animation***

Const Switch15min = 0
Const Switch15max = -20
Dim Switch15dir
Switch15dir = -2

Sub sw15_timer()
	pRampSwitch4B.RotX = pRampSwitch4B.RotX + Switch15dir
	If pRampSwitch4B.RotX >= Switch15min Then
		sw15.timerenabled = False
		pRampSwitch4B.RotX = Switch15min
		Switch15dir = -2
	End If
	If pRampSwitch4B.RotX <= Switch15max Then
		Switch15dir = 4
	End If
End Sub


Sub sw26_Hit:Controller.Switch(26) = 1:sw26.timerenabled = true:End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub

'***switch 26 animation***

Const Switch26min = 0
Const Switch26max = -20
Dim Switch26dir
Switch26dir = -2

Sub sw26_timer()
	pRampSwitch5B.RotX = pRampSwitch5B.RotX + Switch26dir
	If pRampSwitch5B.RotX >= Switch26min Then
		sw26.timerenabled = False
		pRampSwitch5B.RotX = Switch26min
		Switch26dir = -2
	End If
	If pRampSwitch5B.RotX <= Switch26max Then
		Switch26dir = 4
	End If
End Sub


Sub sw36_Hit:Controller.Switch(36) = 1:sw36.timerenabled = true:End Sub
Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub

'***switch 36 animation***

Const Switch36min = 0
Const Switch36max = -20
Dim Switch36dir
Switch36dir = -2

Sub sw36_timer()
	pRampSwitch6B.RotX = pRampSwitch6B.RotX + Switch36dir
	If pRampSwitch6B.RotX >= Switch36min Then
		sw36.timerenabled = False
		pRampSwitch6B.RotX = Switch36min
		Switch36dir = -2
	End If
	If pRampSwitch6B.RotX <= Switch36max Then
		Switch36dir = 4
	End If
End Sub


'*************
'Lamp Handling
'*************
'Material swap arrays.

'GI Color Mod  Arrays
Dim MaterialWhiteArray: MaterialWhiteArray = Array("BulbGIOff", "BulbGIOff","BulbGIOff","BulbGIOn")
Dim MaterialBlueArray: MaterialBlueArray = Array("BulbBlueOff", "BulbBlueOff","BulbBlueOff","BulbBlueOn")
Dim MaterialPurpleArray: MaterialPurpleArray = Array("BulbPurpleOff", "BulbPurpleOff","BulbPurpleOff","BulbPurpleOn")
Dim MaterialGreenArray: MaterialGreenArray = Array("BulbGreenOff", "BulbGreenOff","BulbGreenOff","BulbGreenOn")
Dim MaterialRedArray: MaterialRedArray = Array("BulbRedOff", "BulbRedOff","BulbRedOff","BulbRedOn")
Dim MaterialYellowArray: MaterialYellowArray = Array("BulbYellowOff", "BulbYellowOff","BulbYellowOff","BulbYellowOn")
Dim MaterialClearBumperArray: MaterialClearBumperArray = Array("BumperClearOff", "BumperClearOff","BumperClearOff","BumperClearOn")
Dim MaterialBlueBumperArray: MaterialBlueBumperArray = Array("BumperBlueOff", "BumperBlueOff","BumperBlueOff","BumperBlueOn")
Dim MaterialRedBumperArray: MaterialRedBumperArray = Array("BumperRedOff", "BumperRedOff","BumperRedOff","BumperRedOn")
Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image trans","Plastic with an image trans","Plastic with an image")


Dim TransRedArray: TransRedArray = Array("DomeOffRed", "DomeOnRed","DomeOnRed","DomeOnRed")
Dim TransBlueArray: TransBlueArray = Array("DomeOffBlue","DomeOnBlue","DomeOnBlue","DomeOnBlue")
Dim TransWhiteArray: TransWhiteArray = Array("DomeOffWhite","DomeOnWhite","DomeOnWhite","DomeOnWhite")
Dim TransRedArrowArray: TransRedArrowArray = Array("ArrowInsert", "DomeOnRed","DomeOnRed","DomeOnRed")

Dim LargeRedInsertArray: LargeRedInsertArray = Array("DomeOffRed1", "DomeOnRed","DomeOnRed","DomeOnRed")

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
	If F19.IntensityScale > 0 then
		fmfl27.visible = False
		l27.visible = False
	Else
		fmfl27.visible = True
		l27.visible = True
	end if
End Sub


dim FrameTime, InitFrameTime : InitFrameTime = 0
Wall9.TimerInterval = -1
Wall9.TimerEnabled = True
Sub Wall9_Timer()	'Stealing this random wall's timer for -1 updates
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
	ModLampz.Update
End Sub


Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function


'Image swap arrays...
'Dim DomeArray: DomeArray = Array("Dome0", "Dome1", "Dome2", "Dome3")
'Dim BulbWhiteArray: BulbWhiteArray = Array("BulbWhite0", "BulbWhite2", "BulbWhite4", "BulbWhite6")



'***Material Swap***
'Fade material for green, red, yellow colored Bulb prims
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

'***Material Swap***
'Fade material for green, red, yellow colored Bulb prims
Sub FadeMaterialColoredBulb(pri, group, ByVal aLvl)	'cp's script
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


'Fade material for red, yellow colored bulb Filiment prims
Sub FadeMaterialColoredFiliment(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * 50  'Intensity Adjustment
End Sub


'Fade material for GI bulb prims
Sub FadeMaterialGIBulb(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl,3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * .03  'Intensity Adjustment
End Sub


'Fade material for GI Filiment prims
Sub FadeMaterialGIFiliment(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * 5  'Intensity Adjustment
End Sub


'Flasher Inserts Primitive fading script
Sub FadeMaterialInserts(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full

    End Select
if pri.name = "pInsert19On" then debug.print FlashLevelToIndex(aLvl, 4) & " " & aLvl

pri.blenddisablelighting = aLvl * 10  'Intensity Adjustment
End Sub


'Flasher Domes Primitive fading script
Sub FadeMaterialDome(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full

    End Select
pri.blenddisablelighting = aLvl * 10  'Intensity Adjustment
End Sub










Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating
	ModLampz.Filter = "LampFilter"

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 3/10 : Lampz.FadeSpeedDown(x) = 3/10 : next
	for x = 0 to 28 : ModLampz.FadeSpeedUp(x) = 3/10 : ModLampz.FadeSpeedDown(x) = 3/10 : Next

	'Lamp Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(11)= l11
	Lampz.MassAssign (11)= fmfl11
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign (12)= fmfl12
	Lampz.MassAssign(13)= l13
	Lampz.MassAssign (13)= fmfl13
	Lampz.MassAssign(14)= l14
	Lampz.MassAssign (14)= fmfl14
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(17)= l17
	Lampz.MassAssign(18)= l18
	Lampz.MassAssign(21)= l21
	Lampz.MassAssign (21)= fmfl21
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign (22)= fmfl22
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign (23)= fmfl23
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign (24)= fmfl24
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign (27)= fmfl27
	Lampz.MassAssign(28)= l28
	Lampz.MassAssign (28)= fmfl28
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign (31)= fmfl31
	Lampz.MassAssign(32)= l32
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign (33)= fmfl33
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(37)= l37
	Lampz.MassAssign (37)= fmfl37
	Lampz.MassAssign(38)= l38
	Lampz.MassAssign (38)= fmfl38
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign (41)= fmfl41
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign (42)= fmfl42
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(44)= l44
	Lampz.MassAssign(45)= l45
	Lampz.MassAssign(46)= l46
	Lampz.MassAssign(47)= l47
	Lampz.MassAssign (47)= fmfl47
	Lampz.MassAssign(48)= l48
	Lampz.MassAssign (48)= fmfl48
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= l51a
	Lampz.Callback(51) =         "FadeMaterialGIBulb pBulb51, MaterialBlueArray,"
	Lampz.Callback(51) = "FadeMaterialColoredFiliment pFiliment51, MaterialBlueArray,"
	Lampz.MassAssign(52)= l52
	Lampz.MassAssign(52)= l52a
	Lampz.Callback(52) =         "FadeMaterialGIBulb pBulb52, MaterialRedArray,"
	Lampz.Callback(52) = "FadeMaterialColoredFiliment pFiliment52, MaterialRedArray,"
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= l53a
	Lampz.MassAssign(54)= l54
	Lampz.MassAssign(54)= l54a
	Lampz.Callback(54) =         "FadeMaterialGIBulb pBulb54, MaterialWhiteArray,"
	Lampz.Callback(54) = "FadeMaterialGIFiliment pFiliment54, MaterialWhiteArray,"
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(55)= l55a
	Lampz.Callback(55) =         "FadeMaterialGIBulb pBulb55, MaterialWhiteArray,"
	Lampz.Callback(55) = "FadeMaterialGIFiliment pFiliment55, MaterialWhiteArray,"
	Lampz.MassAssign(56)= l56
	Lampz.MassAssign(56)= l56a
	Lampz.Callback(56) =         "FadeMaterialGIBulb pBulb56, MaterialWhiteArray,"
	Lampz.Callback(56) = "FadeMaterialGIFiliment pFiliment56, MaterialWhiteArray,"
	Lampz.MassAssign(57)= l57
    Lampz.Callback(57) = "FadeMaterialColoredBulb p57, MaterialRedArray, "
    Lampz.Callback(57) = "FadeMaterialColoredFiliment pFiliment57, MaterialRedArray, "
	Lampz.MassAssign(58)= l58
    Lampz.Callback(58) = "FadeMaterialColoredBulb p58, MaterialYellowArray, "
    Lampz.Callback(58) = "FadeMaterialColoredFiliment pFiliment58, MaterialYellowArray, "
	Lampz.MassAssign(61)= l61
	Lampz.MassAssign(61)= l61c
	Lampz.MassAssign(62)= l62
	Lampz.MassAssign(63)= l63
	Lampz.MassAssign (63)= fmfl63
	Lampz.MassAssign(64)= l64
	Lampz.MassAssign(65)= l65
	Lampz.MassAssign(66)= l66
	Lampz.MassAssign(67)= l67
	Lampz.MassAssign(68)= l68

If MirrorLightsMod = 1 then

	Lampz.MassAssign(71)= l71
    Lampz.Callback(71) =         "FadeMaterialColoredBulb pBulb71, MaterialRedArray, "
    Lampz.Callback(71) = "FadeMaterialColoredFiliment pFiliment71, MaterialRedArray, "
	Lampz.MassAssign(72)= l72
	Lampz.MassAssign(72)= l72a
	Lampz.Callback(72) =         "FadeMaterialGIBulb pBulb72, MaterialWhiteArray,"
	Lampz.Callback(72) = "FadeMaterialGIFiliment pFiliment72, MaterialWhiteArray,"
	Lampz.MassAssign(73)= l73
	Lampz.MassAssign(74)= l74
    Lampz.Callback(74) =         "FadeMaterialColoredBulb pBulb74, MaterialWhiteArray,"
    Lampz.Callback(74) = "FadeMaterialColoredFiliment pFiliment74, MaterialWhiteArray,"
	Lampz.MassAssign(75)= l75
    Lampz.Callback(75) =         "FadeMaterialColoredBulb pBulb75, MaterialWhiteArray,"
    Lampz.Callback(75) = "FadeMaterialColoredFiliment pFiliment75, MaterialWhiteArray,"
	Lampz.MassAssign(76)= l76
    Lampz.Callback(76) =         "FadeMaterialColoredBulb pBulb76, MaterialWhiteArray,"
    Lampz.Callback(76) = "FadeMaterialColoredFiliment pFiliment76, MaterialWhiteArray, "
	Lampz.MassAssign(77)= l77
    Lampz.Callback(77) =         "FadeMaterialColoredBulb pBulb77, MaterialWhiteArray,"
    Lampz.Callback(77) = "FadeMaterialColoredFiliment pFiliment77, MaterialWhiteArray,"
	Lampz.MassAssign(78)= l78
    Lampz.Callback(78) =         "FadeMaterialColoredBulb pBulb78, MaterialBlueArray, "
    Lampz.Callback(78) = "FadeMaterialColoredFiliment pFiliment78, MaterialBlueArray, "

Else

	Lampz.MassAssign(71)= l71
    Lampz.Callback(71) =         "FadeMaterialColoredBulb pBulb71, MaterialRedArray, "
    Lampz.Callback(71) = "FadeMaterialColoredFiliment pFiliment71, MaterialRedArray, "
	Lampz.MassAssign(72)= l72
	Lampz.MassAssign(72)= l72a
	Lampz.Callback(72) =         "FadeMaterialGIBulb pBulb72, MaterialWhiteArray,"
	Lampz.Callback(72) = "FadeMaterialGIFiliment pFiliment72, MaterialWhiteArray,"
	Lampz.MassAssign(73)= l73
	Lampz.MassAssign(74)= l74
    Lampz.Callback(74) =         "FadeMaterialColoredBulb pBulb74, MaterialYellowArray, "
    Lampz.Callback(74) = "FadeMaterialColoredFiliment pFiliment74, MaterialYellowArray, "
	Lampz.MassAssign(75)= l75
    Lampz.Callback(75) =         "FadeMaterialColoredBulb pBulb75, MaterialYellowArray, "
    Lampz.Callback(75) = "FadeMaterialColoredFiliment pFiliment75, MaterialYellowArray, "
	Lampz.MassAssign(76)= l76
    Lampz.Callback(76) =         "FadeMaterialColoredBulb pBulb76, MaterialYellowArray, "
    Lampz.Callback(76) = "FadeMaterialColoredFiliment pFiliment76, MaterialYellowArray, "
	Lampz.MassAssign(77)= l77
    Lampz.Callback(77) =         "FadeMaterialColoredBulb pBulb77, MaterialYellowArray, "
    Lampz.Callback(77) = "FadeMaterialColoredFiliment pFiliment77, MaterialYellowArray, "
	Lampz.MassAssign(78)= l78
    Lampz.Callback(78) =         "FadeMaterialColoredBulb pBulb78, MaterialGreenArray, "
    Lampz.Callback(78) = "FadeMaterialColoredFiliment pFiliment78, MaterialGreenArray, "

End If

	Lampz.MassAssign(81)= l81
	Lampz.MassAssign(82)= l82
	Lampz.MassAssign(82)= l82a
	Lampz.MassAssign(83)= l83
	Lampz.MassAssign (83)= fmfl83
	Lampz.MassAssign(84)= l84
	Lampz.MassAssign(85)= l85
	Lampz.MassAssign(86)= l86
	Lampz.MassAssign(87)= l87

	If ClockMod = 1 then
		Lampz.MassAssign(21)= cfs45
		Lampz.MassAssign(22)= cfh8
		Lampz.MassAssign(23)= cfh6
		Lampz.MassAssign(24)= cfs25
		Lampz.MassAssign(25)= cfs15
		Lampz.MassAssign(26)= cfs10
		Lampz.MassAssign(27)= cfh12
		Lampz.MassAssign(28)= l28b
		Lampz.MassAssign(31)= cfs40
		Lampz.MassAssign(32)= cfs35
		Lampz.MassAssign(33)= cfs30
		Lampz.MassAssign(34)= cfs20
		Lampz.MassAssign(35)= cfh3
		Lampz.MassAssign(36)= cfh1
		Lampz.MassAssign(37)= cfh11
		Lampz.MassAssign(38)= cfs50
		Lampz.MassAssign(41)= cfh9
		Lampz.MassAssign(42)= cfh7
		Lampz.MassAssign(43)= cfh5
		Lampz.MassAssign(44)= cfh4
		Lampz.MassAssign(45)= cfh2
		Lampz.MassAssign(46)= cfs5
		Lampz.MassAssign(47)= cfs55
		Lampz.MassAssign(48)= cfh10
	End If


	If BalloonMod = 1 Then
		Lampz.MassAssign  (51)= LBballoon
		Lampz.Callback(51) = "FadeMaterialToys prballoon_Blue, TextureArray1, " 'FadeMaterialP 51, prballoon_Blue, TextureArray1

		Lampz.MassAssign  (52)= LRballoon
		Lampz.Callback(52) = "FadeMaterialToys prballoon_Red, TextureArray1, " 'FadeMaterialP 52, prballoon_Red, TextureArray1

		Lampz.MassAssign  (72)= LYballoon
		Lampz.Callback(72) = "FadeMaterialToys prballoon_Yellow, TextureArray1, " 'FadeMaterialP 72, prballoon_Yellow, TextureArray1
	End If

	If HotDogCartMod = 1 then
		Lampz.Callback(72) = "FadeMaterialToys prHotDogCartC, TextureArray1, " 'FadeMaterialP 53, prHotDogCartC, TextureArray1
		Lampz.MassAssign  (53)= lHotDogCartB
	End If


	'Flasher Assignments
	ModLampz.MassAssign(17)= GlobalBloomB
	ModLampz.MassAssign(17)= F17sb
	ModLampz.MassAssign(17)= F17w
	ModLampz.MassAssign(17)= F17w1
	ModLampz.MassAssign(17)= f17sb1
	ModLampz.MassAssign(17)= f17sb2
	ModLampz.Callback(17) = "FadeMaterialDome pDome17On, TransBlueArray,"
	ModLampz.Callback(17) = "FadeMaterialInserts pInsert17On, TransBlueArray,"
	ModLampz.Callback(17) = "FadeMaterialInserts pInserta17On, TransBlueArray,"

	ModLampz.MassAssign(18)= GlobalBloomW
	ModLampz.MassAssign(18)= F18
	ModLampz.MassAssign(18)= F18a
	ModLampz.MassAssign(18)= F18b

	ModLampz.MassAssign (19)= GlobalBloomR
	ModLampz.MassAssign(19)= f19
	ModLampz.MassAssign (19)= fmfl19
	ModLampz.Callback(19) = "FadeMaterialInserts pInsert19On, LargeRedInsertArray,"
	ModLampz.Callback(19) = "FadeMaterialInserts pInsert19aOn, TransRedArrowArray,"

	If ClockMod = 1 then
		ModLampz.MassAssign(19)= cfcenter
	End If


	'sol 20 hotdog flasher (These assignments weren't set up before)
	ModLampz.obj(20) = Array(f20, F20a, f20b)

	ModLampz.MassAssign(23)= GlobalBloomR
	ModLampz.MassAssign(23)= F23sb
	ModLampz.MassAssign(23)= F23w
	ModLampz.MassAssign(23)= F23w1
	ModLampz.MassAssign(23)= f23sb1
	ModLampz.MassAssign(23)= f23sb2
	ModLampz.Callback(23) = "FadeMaterialDome pDome23On, TransRedArray,"
	ModLampz.Callback(23) = "FadeMaterialInserts pInsert23On, TransRedArray,"
	ModLampz.Callback(23) = "FadeMaterialInserts pInserta23On, TransRedArray,"

	ModLampz.MassAssign(24)= GlobalBloomW
	ModLampz.MassAssign(24)= F24sb
	ModLampz.MassAssign(24)= F24w
	ModLampz.MassAssign(24)= F24w1
	ModLampz.MassAssign(24)= F24w2
	ModLampz.MassAssign(24)= f24sb1
	ModLampz.MassAssign(24)= f24sb2
	ModLampz.Callback(24) = "FadeMaterialDome pDome24On, TransWhiteArray,"
	ModLampz.Callback(24) = "FadeMaterialInserts pInsert24On, TransWhiteArray,"
	ModLampz.Callback(24) = "FadeMaterialInserts pInserta24On, TransWhiteArray,"


	'GI assignments

	' 0 Upper BackGlass

	' 1 Rudy
	ModLampz.Obj(1) = ColToArray(GI_Rudy)
	ModLampz.MassAssign(1)= Array(RudyShade, RudySign1, RudySign2)
	'ModLampz.Callback(1) = "GIUpdates"


	' 2 Upper Playfield
	ModLampz.MassAssign(2)= ColToArray(GI_UpperMain)
	ModLampz.MassAssign(2)= ColToArray(GI_UpperPlastics)
	ModLampz.MassAssign(2)= ColToArray(GI_UpperBulbs)
	ModLampz.MassAssign(2)= Array(RudyShade, RudySign1, RudySign2)

	If GIColorModType = 0 then
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb1, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment1, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb2, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment2, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb3, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment3, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb4, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment4, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb5, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment5, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb6, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment6, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb7, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment7, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb8, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment8, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb9, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment9, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb10, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment10, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb11, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment11, MaterialWhiteArray, "
		ModLampz.MassAssign(4)= GIWall
		ModLampz.MassAssign(4)= GIWall1
		ModLampz.MassAssign(4)= GIWall2
		ModLampz.MassAssign(4)= GIWall3
		ModLampz.MassAssign(4)= GIWall4
		ModLampz.MassAssign(4)= GIWall5
		ModLampz.MassAssign(4)= GIWall6
		ModLampz.MassAssign(4)= GIWall7
		ModLampz.MassAssign(4)= GIWall8
		ModLampz.MassAssign(4)= GIWall9
		ModLampz.MassAssign(4)= GIWall10
		ModLampz.MassAssign(4)= GIWall11
		ModLampz.MassAssign(4)= GIWall12
		ModLampz.MassAssign(4)= GIWall13
		ModLampz.MassAssign(4)= GIWall14
		ModLampz.MassAssign(4)= GIWall15
		ModLampz.MassAssign(4)= GIWall16
		ModLampz.MassAssign(4)= GIWall17
		ModLampz.MassAssign(4)= GIWall18
		ModLampz.MassAssign(4)= GIWall20
		ModLampz.MassAssign(4)= GIWall19

	End If

	If GIColorModType = 1 then
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb1, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment1, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb2, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment2, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb3, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment3, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb4, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment4, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb5, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment5, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb6, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment6, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb7, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment7, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb8, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment8, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb9, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment9, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb10, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment10, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb11, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment11, MaterialBlueArray, "




	End If

	If GIColorModType = 2 then
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb1, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment1, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb2, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment2, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb3, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment3, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb4, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment4, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb5, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment5, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb6, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment6, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb7, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment7, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb8, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment8, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb9, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment9, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb10, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment10, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb11, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment11, MaterialPurpleArray, "



end if
	ModLampz.Callback(2) = "GIUpdates"

	If PopCornMod then
		ModLampz.MassAssign(2)= lPopcornLight
	end if


	' 3 Center BackGlass


	' 4 Lower Playfield
	ModLampz.MassAssign(4)= ColToArray(GI_LowerMain)
	ModLampz.MassAssign(4)= ColToArray(GI_LowerPlastics)
	ModLampz.MassAssign(4)= ColToArray(GI_LowerBulbs)

	If GIColorModType = 0 then
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb1, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment1, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb2, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment2, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb3, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment3, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb4, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment4, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb5, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment5, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb6, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment6, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb7, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment7, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb8, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment8, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb9, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment9, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb10, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment10, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb11, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment11, MaterialWhiteArray, "
	End If

	If GIColorModType = 1 then
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb1, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment1, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb2, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment2, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb3, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment3, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb4, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment4, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb5, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment5, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb6, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment6, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb7, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment7, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb8, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment8, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb9, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment9, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb10, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment10, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb11, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment11, MaterialBlueArray, "
	End If

	If GIColorModType = 2 then
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb1, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment1, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb2, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment2, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb3, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment3, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb4, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment4, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb5, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment5, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb6, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment6, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb7, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment7, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb8, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment8, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb9, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment9, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb10, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment10, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb11, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment11, MaterialPurpleArray, "




	End If

	ModLampz.Callback(4) = "GIUpdates"

	'Turn on GI to Start
	for x = 0 to 4 : ModLampz.State(x) = 1 : Next

	'Turn off all lamps on startup
	lampz.Init	'This just turns state of any lamps to 1
	ModLampz.Init

	'Immediate update to turn on GI, turn off lamps
	lampz.update
	ModLampz.Update

End Sub

'Lamp Filter
Function LampFilter(aLvl)

	LampFilter = aLvl^1.6	'exponential curve?
End Function


Dim GIoffMult : GIoffMult = 2 'adjust how bright the inserts get when the GI is off
Dim GIoffMultFlashers : GIoffMultFlashers = 2	'adjust how bright the Flashers get when the GI is off

'GI callback
Sub GIUpdates(aLvl)	'argument is unused
	'2 and 4 are the major PF gi circuits, averaging them together...
	dim giAvg
	if Lampz.UseFunction then 	'Callbacks don't get this filter automatically
		giAvg = (LampFilter(ModLampz.Lvl(2)) + LampFilter(ModLampz.Lvl(4)) )/2
	Else
		giAvg = (ModLampz.Lvl(2) + ModLampz.Lvl(4) )/2
	end if

	'Brighten inserts when GI is Low
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

End Sub




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


Set GICallback2 = GetRef("SetGI")

'    GI lights controlled by Strings
' 01 Upper BackGlass		'Case 0
' 02 Rudy					'Case 1
' 03 Upper Playfield		'Case 2
' 04 Center BackGlass		'Case 3
' 05 Lower Playfield		'Case 4

Sub SetGI(aNr, aValue)
	ModLampz.SetGI aNr, aValue 'Redundant. Could reassign GI indexes here
End Sub

'Flashers via SolModCallBacks

'DynamicLamps's Setmodlamp handles input automatically (input / 255)
SolModCallback(17) = "ModLampz.SetModLamp 17, "							'Blue Dome Flasher and X2 PF lights
SolModCallback(18) = "ModLampz.SetModLamp 18, "							'Flasher in front Rudy
SolModCallback(19) = "ModLampz.SetModLamp 19, "							'Center clock flasher
SolModCallback(20) = "ModLampz.SetModLamp 20, "							'Hot Dog Flasher
SolModCallback(23) = "ModLampz.SetModLamp 23, "							'Red Dome Flasher and X2 PF lights
SolModCallback(24) = "ModLampz.SetModLamp 24, " 						'White Dome Flasher and X2 PF lights

'End lamps

Sub PlayMusic()
    'Intro Snippet
	If PrevGameOver = 0 Then
		If MusicSnippet = 0 Then
			PlaySound "intro"
			PrevGameOver = 1
		End If
	else
'		PrevGameOver = 0
	End If
End Sub

Sub SetBallMod ()

	If BallTypeMod = 1 Then
		cBall1.Image = "Chrome_Ball_29"
		cBall1.FrontDecal = "FunhouseBall1"
		cBall2.Image = "Chrome_Ball_29"
		cBall2.FrontDecal = "FunhouseBall2"
		cBall3.Image = "Chrome_Ball_29"
		cBall3.FrontDecal = "FunhouseBall3"
	Else
		cBall1.Image = "MRBallDark"
		cBall1.FrontDecal = "Swirl"
		cBall2.Image = "MRBallDark"
		cBall2.FrontDecal = "Swirl"
		cBall3.Image = "MRBallDark"
		cBall3.FrontDecal = "Swirl"
	End If

End Sub
'
'
'Dim DRSstep
'
'Sub DelayRollingStart_timer()
'	Select Case DRSstep
'		Case 5: RollingSoundTimer.enabled = true
'	End Select
'	DRSstep = DRSstep + 1
'End Sub
'
'Sub sw63_hit()
''	Kicker1active = 1
'	Controller.Switch(63)=1
'	TroughWall1.isDropped = false
'
'End Sub
'
'
'
'Dim DontKickAnyMoreBalls,DKTMstep
'
'Sub KickBallToLane(Enabled)
'	If DontKickAnyMoreBalls = 0 then
'		PlaySound SoundFX("fx_ballrel",DOFContactors)
'		PlaySound SoundFX("Solenoid",DOFContactors)
'		sw63.Kick 60,10
'		Controller.Switch(63)=0
'		DontKickAnyMoreBalls = 1
'		DKTMstep = 1
'		DontKickToMany.enabled = true
'	End If
'End Sub
'
'Sub DontKickToMany_timer()
'	Select Case DKTMstep
'		Case 1:
'		Case 2:
'		Case 3: DontKickAnyMoreBalls = 0:DontKickToMany.Enabled = False: DontKickAnyMoreBalls = 0
'	End Select
'	DKTMstep = DKTMstep + 1
'End Sub
'
'sub kisort(enabled)
'	sw73.Kick 70,30
'	controller.switch(73) = false
'end sub
'
'Sub sw73_hit()
'	PlaySound "drain"
'	controller.switch(73) = true
'End Sub

'***Ball brakes***
Sub ramp_brake1_Hit()
    ActiveBall.vely = Activeball.vely *.5
End Sub

Sub ramp_brake2_Hit()
    ActiveBall.vely = Activeball.vely/5
End Sub

Sub ramp_brake3_Hit()
    ActiveBall.velx = Activeball.velx/2
End Sub

Sub ramp_brake4_Hit()
    ActiveBall.vely = Activeball.vely/2
End Sub

Sub ramp_brake5_Hit()
    ActiveBall.velx = Activeball.velx/10
End Sub

Sub ramp_brake6_Hit()
    ActiveBall.vely = Activeball.vely/5
End Sub

Sub ramp_brake7_Hit()
    ActiveBall.vely = Activeball.vely *.5
End Sub


'***Rubber animations***
Sub wall41_Hit:rubber16.visible = 0::rubber16a.visible = 1:rubber18.visible = 0::rubber18a.visible = 1:wall41.timerenabled = 1:End Sub
Sub wall41_timer:rubber16.visible = 1::rubber16a.visible = 0:rubber18.visible = 1::rubber18a.visible = 0: wall41.timerenabled= 0:End Sub
Sub wall58_Hit:rubber31.visible = 0::rubber31a.visible = 1::rubber5.visible = 0:rubber5a.visible = 1:wall58.timerenabled = 1:End Sub
Sub wall58_timer:rubber31.visible = 1::rubber31a.visible = 0:rubber5.visible = 1:rubber5a.visible = 0:wall58.timerenabled= 0:End Sub

Sub wall5_Hit:rubber13.visible = 0::rubber13a.visible = 1:rubber14.visible = 0::rubber14a.visible = 1:wall5.timerenabled = 1:End Sub
Sub wall5_timer:rubber13.visible = 1::rubber13a.visible = 0:rubber14.visible = 1::rubber14a.visible = 0: wall5.timerenabled= 0:End Sub

Sub wall71_Hit:rubber1.visible = 0::rubber1a.visible = 1:rubber12.visible = 0::rubber12a.visible = 1:wall71.timerenabled = 1:End Sub
Sub wall71_timer:rubber1.visible = 1::rubber1a.visible = 0:rubber12.visible = 1::rubber12a.visible = 0: wall71.timerenabled= 0:End Sub

Sub wall65_Hit:rubber23.visible = 0::rubber23a.visible = 1:rubber51.visible = 0::rubber51a.visible = 1:wall65.timerenabled = 1:End Sub
Sub wall65_timer:rubber23.visible = 1::rubber23a.visible = 0:rubber51.visible = 1::rubber51a.visible = 0: wall65.timerenabled= 0:End Sub



'*************************
'* Alpha-numeric display
'*************************

Dim Digits(32)
Digits(0) =  Array(LiScore1, LiScore2, LiScore3, LiScore4, LiScore5, LiScore6, LiScore7, LiScore8, LiScore9, LiScore10, LiScore11, LiScore12, LiScore13, LiScore14, LiScore15, LiScore16)
Digits(1) =  Array(LiScore17, LiScore18, LiScore19, LiScore20, LiScore21, LiScore22, LiScore23, LiScore24, LiScore25, LiScore26, LiScore27, LiScore28, LiScore29, LiScore30, LiScore31, LiScore32)
Digits(2) =  Array(LiScore33, LiScore34, LiScore35, LiScore36, LiScore37, LiScore38, LiScore39, LiScore40, LiScore41, LiScore42, LiScore43, LiScore44, LiScore45, LiScore46, LiScore47, LiScore48)
Digits(3) =  Array(LiScore49, LiScore50, LiScore52, LiScore53, LiScore54, LiScore55, LiScore56, LiScore63, LiScore57, LiScore58, LiScore59, LiScore51, LiScore60, LiScore61, LiScore62, LiScore64)
Digits(4) =  Array(LiScore65, LiScore66, LiScore68, LiScore69, LiScore70, LiScore71, LiScore72, LiScore79, LiScore73, LiScore74, LiScore75, LiScore67, LiScore76, LiScore77, LiScore78, LiScore80)
Digits(5) =  Array(LiScore81, LiScore82, LiScore84, LiScore85, LiScore86, LiScore87, LiScore88, LiScore95, LiScore89, LiScore90, LiScore91, LiScore83, LiScore92, LiScore93, LiScore94, LiScore96)
Digits(6) =  Array(LiScore97, LiScore98, LiScore100, LiScore101, LiScore102, LiScore103, LiScore104, LiScore111, LiScore105, LiScore106, LiScore107, LiScore99, LiScore108, LiScore109, LiScore110, LiScore112)
Digits(7) =  Array(LiScore113, LiScore114, LiScore116, LiScore117, LiScore118, LiScore119, LiScore120, LiScore127, LiScore121, LiScore122, LiScore123, LiScore115, LiScore124, LiScore125, LiScore126, LiScore128)
Digits(8) =  Array(LiScore129, LiScore130, LiScore132, LiScore133, LiScore134, LiScore135, LiScore136, LiScore143, LiScore137, LiScore138, LiScore139, LiScore131, LiScore140, LiScore141, LiScore142, LiScore144)
Digits(9) =  Array(LiScore145, LiScore146, LiScore148, LiScore149, LiScore150, LiScore151, LiScore152, LiScore159, LiScore153, LiScore154, LiScore155, LiScore147, LiScore156, LiScore157, LiScore158, LiScore160)
Digits(10) = Array(LiScore161, LiScore162, LiScore164, LiScore165, LiScore166, LiScore167, LiScore168, LiScore175, LiScore169, LiScore170, LiScore171, LiScore163, LiScore172, LiScore173, LiScore174, LiScore176)
Digits(11) = Array(LiScore177, LiScore178, LiScore180, LiScore181, LiScore182, LiScore183, LiScore184, LiScore191, LiScore185, LiScore186, LiScore187, LiScore179, LiScore188, LiScore189, LiScore190, LiScore192)
Digits(12) = Array(LiScore193, LiScore194, LiScore196, LiScore197, LiScore198, LiScore199, LiScore200, LiScore207, LiScore201, LiScore202, LiScore203, LiScore195, LiScore204, LiScore205, LiScore206, LiScore208)
Digits(13) = Array(LiScore209, LiScore210, LiScore212, LiScore213, LiScore214, LiScore215, LiScore216, LiScore223, LiScore217, LiScore218, LiScore219, LiScore211, LiScore220, LiScore221, LiScore222, LiScore224)
Digits(14) = Array(LiScore225, LiScore226, LiScore228, LiScore229, LiScore230, LiScore231, LiScore232, LiScore239, LiScore233, LiScore234, LiScore235, LiScore227, LiScore236, LiScore237, LiScore238, LiScore240)
Digits(15) = Array(LiScore241, LiScore242, LiScore244, LiScore245, LiScore246, LiScore247, LiScore248, LiScore255, LiScore249, LiScore250, LiScore251, LiScore243, LiScore252, LiScore253, LiScore254, LiScore256)
Digits(16) = Array(LiScore257, LiScore258, LiScore260, LiScore261, LiScore262, LiScore263, LiScore264, LiScore271, LiScore265, LiScore266, LiScore267, LiScore259, LiScore268, LiScore269, LiScore270, LiScore272)
Digits(17) = Array(LiScore273, LiScore274, LiScore276, LiScore277, LiScore278, LiScore279, LiScore280, LiScore287, LiScore281, LiScore282, LiScore283, LiScore275, LiScore284, LiScore285, LiScore286, LiScore288)
Digits(18) = Array(LiScore289, LiScore290, LiScore292, LiScore293, LiScore294, LiScore295, LiScore296, LiScore303, LiScore297, LiScore298, LiScore299, LiScore291, LiScore300, LiScore301, LiScore302, LiScore304)
Digits(19) = Array(LiScore305, LiScore306, LiScore308, LiScore309, LiScore310, LiScore311, LiScore312, LiScore319, LiScore313, LiScore314, LiScore315, LiScore307, LiScore316, LiScore317, LiScore318, LiScore320)
Digits(20) = Array(LiScore321, LiScore322, LiScore324, LiScore325, LiScore326, LiScore327, LiScore328, LiScore335, LiScore329, LiScore330, LiScore331, LiScore323, LiScore332, LiScore333, LiScore334, LiScore336)
Digits(21) = Array(LiScore337, LiScore338, LiScore340, LiScore341, LiScore342, LiScore343, LiScore344, LiScore351, LiScore345, LiScore346, LiScore347, LiScore339, LiScore348, LiScore349, LiScore350, LiScore352)
Digits(22) = Array(LiScore353, LiScore354, LiScore356, LiScore357, LiScore358, LiScore359, LiScore360, LiScore367, LiScore361, LiScore362, LiScore363, LiScore355, LiScore364, LiScore365, LiScore366, LiScore368)
Digits(23) = Array(LiScore369, LiScore370, LiScore372, LiScore373, LiScore374, LiScore375, LiScore376, LiScore383, LiScore377, LiScore378, LiScore379, LiScore371, LiScore380, LiScore381, LiScore382, LiScore384)
Digits(24) = Array(LiScore385, LiScore386, LiScore388, LiScore389, LiScore390, LiScore391, LiScore392, LiScore399, LiScore393, LiScore394, LiScore395, LiScore387, LiScore396, LiScore397, LiScore398, LiScore400)
Digits(25) = Array(LiScore401, LiScore402, LiScore404, LiScore405, LiScore406, LiScore407, LiScore408, LiScore415, LiScore409, LiScore410, LiScore411, LiScore403, LiScore412, LiScore413, LiScore414, LiScore416)
Digits(26) = Array(LiScore417, LiScore418, LiScore420, LiScore421, LiScore422, LiScore423, LiScore424, LiScore431, LiScore425, LiScore426, LiScore427, LiScore419, LiScore428, LiScore429, LiScore430, LiScore432)
Digits(27) = Array(LiScore433, LiScore434, LiScore436, LiScore437, LiScore438, LiScore439, LiScore440, LiScore447, LiScore441, LiScore442, LiScore443, LiScore435, LiScore444, LiScore445, LiScore446, LiScore448)
Digits(28) = Array(LiScore449, LiScore450, LiScore452, LiScore453, LiScore454, LiScore455, LiScore456, LiScore463, LiScore457, LiScore458, LiScore459, LiScore451, LiScore460, LiScore461, LiScore462, LiScore464)
Digits(29) = Array(LiScore465, LiScore466, LiScore468, LiScore469, LiScore470, LiScore471, LiScore472, LiScore479, LiScore473, LiScore474, LiScore475, LiScore467, LiScore476, LiScore477, LiScore478, LiScore480)
Digits(30) = Array(LiScore481, LiScore482, LiScore484, LiScore485, LiScore486, LiScore487, LiScore488, LiScore495, LiScore489, LiScore490, LiScore491, LiScore483, LiScore492, LiScore493, LiScore494, LiScore496)
Digits(31) = Array(LiScore497, LiScore498, LiScore500, LiScore501, LiScore502, LiScore503, LiScore504, LiScore511, LiScore505, LiScore506, LiScore507, LiScore499, LiScore508, LiScore509, LiScore510, LiScore512)


Sub TiDisplay_Timer()
	Dim ChgLED, ii, num, chg, stat, obj
	ChgLED=Controller.ChangedLEDs(&H00000000, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		If DesktopMode = True Then
		For ii=0 To UBound(chgLED)
			num=chgLED(ii,0)
			chg=chgLED(ii,1)
			stat=chgLED(ii,2)
			For Each obj In Digits(num)
				If chg And 1 Then obj.State=stat And 1
				chg=chg\2
				stat=stat\2
			Next
		Next
	   end if
	End If
End Sub


Dim TableOptions, TableName



'''''''Set Options
Dim RudyType, cheaterpost, CardType

Sub SetOptions()
	If MirrorLetteringMod = 1 Then
		pMirrorFrontB.image = "Mirror_FrontB_texture2"
	Else
		pMirrorFrontB.image = "Mirror_FrontB_texture"
	End If

	If ApronWallsMod = 1 Then
		pApronOverlay.visible = 1
		If DesktopMode = True Then 'Show Desktop components

			pSidewallCustom.visible = 1
		Else
			pSidewallCustom.visible = 1

		End If
	Else
		pApronOverlay.visible = 0

		pSidewallCustom.visible = 0
	End If

	If DrainPostMod = 1 Then
		cpost.visible = 1
		crubber.collidable = 1
        DPostM.collidable = 1
		crubber.visible = 1
	Else
		cpost.visible = 0
		crubber.collidable = 0
        DPostM.collidable = 0
		crubber.visible = 0
	End If

'	If RudyMod = 0 then
'		RudyType = Int(Rnd*3)+1
'	Else
'		RudyType = RudyMod
'	End If

'	If RudyType = 1 Then
'		PrRudy.Visible = True
'		PrLids.Image = "Rudy eyelid1"
'		PrEyeL.Image = "eye_texture"
'		PrEyeR.Image = "eye_texture"
'		PrRudy.Image = "Rudy_Face_Off_2"
'		PrRudy1.Image = "Rudy_Back_Off_2"
'		PrMouth.Image = "Rudy mouth baked off"
'		prRIWCage.Material = "RudyFrame"
'		prRudyScoop.Material = "RudyFrame"
'		PrMouthb.Material = "RudyFrame"
'	End If

'	If RudyType = 2 Then
'		PrRudy.Visible = False
'		prRIWCage.Material = "Metal with an image"
'		prRudyScoop.Material = "Metal with an image"
'		PrMouthb.Material = "Metal with an image"
'	End If

'	If RudyType = 3 Then
'		PrRudy.Visible = True
'		PrLids.Image = "Rudy eyelid1c"
'		PrEyeL.Image = "eye_texture2"
'		PrEyeR.Image = "eye_texture2"
'		PrRudy.Image = "Rudy_Face_Off_2c"
'		PrRudy1.Image = "Rudy_Back_Off_2c"
'		PrMouth.Image = "Rudy mouth baked off c"
'		prRIWCage.Material = "RudyFrame"
'		prRudyScoop.Material = "RudyFrame"
'		PrMouthb.Material = "RudyFrame"
'	End If

If FlipperRubberColor = 1 Then

	LeftFlipper.Material = "Plastic Yellow"
	LeftFlipper.RubberMaterial = "Blue Rubber"

	RightFlipper.Material = "Plastic Yellow"
	RightFlipper.RubberMaterial = "Blue Rubber"

	LeftFlipper1.Material = "Plastic Yellow"
	LeftFlipper1.RubberMaterial = "Blue Rubber"



Else

	LeftFlipper.Material = "Plastic Yellow"
	LeftFlipper.RubberMaterial = "Red Rubber"

	RightFlipper.Material = "Plastic Yellow"
	RightFlipper.RubberMaterial = "Red Rubber"

	LeftFlipper1.Material = "Plastic Yellow"
	LeftFlipper1.RubberMaterial = "Red Rubber"



End If

	If ClockMod = 1 Then
		pClock.Visible = True
	Else
		pClock.Visible = False

		cfs55.Opacity = 0
		cfs50.Opacity = 0
		cfs45.Opacity = 0
		cfs40.Opacity = 0
		cfs35.Opacity = 0
		cfs30.Opacity = 0
		cfs25.Opacity = 0
		cfs20.Opacity = 0
		cfs15.Opacity = 0
		cfs10.Opacity = 0
		cfs5.Opacity = 0

		cfh12.Opacity = 0
		cfh11.Opacity = 0
		cfh10.Opacity = 0
		cfh9.Opacity = 0
		cfh8.Opacity = 0
		cfh7.Opacity = 0
		cfh6.Opacity = 0
		cfh5.Opacity = 0
		cfh4.Opacity = 0
		cfh3.Opacity = 0
		cfh2.Opacity = 0
		cfh1.Opacity = 0

		l28b.state = 0
		cfcenter.Opacity = 0
        l28b.ShowBulbMesh = True
        l28b.ShowBulbMesh = False

	End If

	If BalloonMod = 1 Then
		prballoon_yellow.Visible = True
		prballoon_Red.Visible = True
		prballoon_Blue.Visible = True
		prballoon_strings.Visible = True
		'debug.print "On"
	Else
		prballoon_yellow.Visible = False
		prballoon_blue.Visible = False
		prballoon_red.Visible = False
		prballoon_strings.Visible = False
		lbballoon.state=0
		lrballoon.state=0
		lyballoon.state=0
		'debug.print "Off"
	End If

	If PopCornMod = 1 Then
		prPopCornA.Visible = True
		prPopCornB.Visible = True
		lPopcornLight.state = 1
	Else
		prPopCornA.Visible = False
		prPopCornB.Visible = False
		lPopcornLight.state = 0
	End If

	If HotDogCartMod = 1 Then
		prHotDogCartA.Visible = True
		prHotDogCartB.Visible = True
		prHotDogCartC.Visible = True
		lHotDogCartA.state = 1
	Else
		prHotDogCartA.Visible = False
		prHotDogCartB.Visible = False
		prHotDogCartC.Visible = False
		lHotDogCartA.state = 0
		lHotDogCartB.state = 0
	End If

	If LevelMod = 1 Then

		wall31.Visible = True
		wall29.Visible = True
		wall11.Visible = True
		level.Visible = True

	Else

		wall31.Visible = false
		wall29.Visible = false
		wall11.Visible = false
		level.Visible = false
	End If

'	If BallTypeMod = 1 Then
'		cBall1.Image = "Chrome_Ball_29"
'		cBall1.FrontDecal = "FunhouseBall1"
'		cBall2.Image = "Chrome_Ball_29"
'		cBall2.FrontDecal = "FunhouseBall2"
'		cBall3.Image = "Chrome_Ball_29"
'		cBall3.FrontDecal = "FunhouseBall3"
'	Else
'		cBall1.Image = "Pinball"
'		cBall1.FrontDecal = "Scratches"
'		cBall2.Image = "Pinball"
'		cBall2.FrontDecal = "Scratches"
'		cBall3.Image = "Pinball"
'		cBall3.FrontDecal = "Scratches"
'	End If

	If SubwayColorMod = 0 Then 'No color
		Up_subway_red.state=0
		Low_subway_red.state=0
		Up_subway_blue.state=0
		Low_subway_blue.state=0
		TD_subway_blue.state=0
		TD_subway_red.state=0
	ElseIf SubwayColorMod = 1 Then 'Blue color
		Up_subway_red.state = 0
		Low_subway_red.state = 0
		Up_subway_blue.state = 1
		Low_subway_blue.state = 1
		TD_subway_blue.state=1
		TD_subway_red.state=0
	ElseIf SubwayColorMod = 2 Then 'Red color
		Up_subway_red.state = 1
		Low_subway_red.state=1
		Up_subway_blue.state = 0
		Low_subway_blue.state = 0
		TD_subway_blue.state=0
		TD_subway_red.state=1
	End if

	If InstructionCardsMod = 0 then
		CardType = InstructionCardsMod
	Else
		CardType = Int(Rnd*6)
	End If

	If CardType = 0 Then
		pIC_Right.image="FH_IC1-R"
		pIC_Left.image="FH_IC1-L"
	End If

	If CardType = 1 Then
		pIC_Right.image="FH_IC2-R"
		pIC_Left.image="FH_IC2-L"
	End If

	If CardType = 2 Then
		pIC_Right.image="FH_IC3-R"
		pIC_Left.image="FH_IC3-L"
	End If

	If CardType = 3 Then
		pIC_Right.image="FH_IC4-R"
		pIC_Left.image="FH_IC4-L"
	End If

	If CardType = 4 Then
		pIC_Right.image="FH_IC5-R"
		pIC_Left.image="FH_IC5-L"
	End If

	If CardType = 5 Then
		pIC_Right.image="FH_IC6-R"
		pIC_Left.image="FH_IC6-L"
	End If

	If MirrorLightsMod = 1 Then
		pBulb71.Material = "BulbRedOff"
'		F71a.Color = RGB(255,0,0)
'		F71b.Color = RGB(255,0,0)
'		F71c.Color = RGB(255,0,0)
'		F71d.Color = RGB(255,0,0)
'		F71e.Color = RGB(255,0,0)
'		F71f.Color = RGB(255,0,0)
'		F71g.Color = RGB(255,0,0)
'
		pBulb74.Material = "BulbGIOff"
'		F74a.Color = RGB(255,255,255)
'		F74b.Color = RGB(255,255,255)
'		F74c.Color = RGB(255,255,255)
'		F74d.Color = RGB(255,255,255)
'		F74e.Color = RGB(255,255,255)
'		F74f.Color = RGB(255,255,255)
'		F74g.Color = RGB(255,255,255)
'
		pBulb75.Material = "BulbGIOff"
'		F75a.Color = RGB(255,255,255)
'		F75b.Color = RGB(255,255,255)
'		F75c.Color = RGB(255,255,255)
'		F75d.Color = RGB(255,255,255)
'		F75e.Color = RGB(255,255,255)
'		F75f.Color = RGB(255,255,255)
'		F75g.Color = RGB(255,255,255)
'
		pBulb76.Material = "BulbGIOff"
'		F76a.Color = RGB(255,255,255)
'		F76b.Color = RGB(255,255,255)
'		F76c.Color = RGB(255,255,255)
'		F76d.Color = RGB(255,255,255)
'		F76e.Color = RGB(255,255,255)
'		F76f.Color = RGB(255,255,255)
'		F76g.Color = RGB(255,255,255)
'
		pBulb77.Material = "BulbGIOff"
'		F77a.Color = RGB(255,255,255)
'		F77b.Color = RGB(255,255,255)
'		F77c.Color = RGB(255,255,255)
'		F77d.Color = RGB(255,255,255)
'		F77e.Color = RGB(255,255,255)
'		F77f.Color = RGB(255,255,255)
'		F77g.Color = RGB(255,255,255)
'
		pBulb78.Material = "BulbBlueOff"
'		F78a.Color = RGB(0,150,255)
'		F78b.Color = RGB(0,150,255)
'		F78c.Color = RGB(0,150,255)
'		F78d.Color = RGB(0,150,255)
'		F78e.Color = RGB(0,150,255)
'		F78f.Color = RGB(0,150,255)
'		F78g.Color = RGB(0,150,255)
	Else
		pBulb71.Material = "BulbRedOff"
'		F71a.Color = RGB(255,0,0)
'
		pBulb74.Material = "BulbYellowOff"
'		F74a.Color = RGB(255,255,0)
'
		pBulb75.Material = "BulbYellowOff"
'		F75a.Color = RGB(255,255,0)
'
		pBulb76.Material = "BulbYellowOff"
'		F76a.Color = RGB(255,255,0)
'
		pBulb77.Material = "BulbYellowOff"
'		F77a.Color = RGB(255,255,0)
'
		pBulb78.Material = "BulbGreenOff"
'		F78a.Color = RGB(0,128,0)
	End If

End Sub

' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
'(_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_)

Sub CheckRudyType()
	If RudyType = 1 Then
		PrRudy.Visible = True
		PrLids.Image = "Rudy eyelid1"
		PrEyeL.Image = "eye_texture"
		PrEyeR.Image = "eye_texture"
		PrRudy.Image = "Rudy_Face_Off_2"
		PrRudy1.Image = "Rudy_Back_Off_2"
		PrMouth.Image = "Rudy mouth baked off"
		prRIWCage.Material = "RudyFrame"
		prRudyScoop.Material = "RudyFrame"
		PrMouthb.Material = "MRudyFrame"
		pPennywiseHair.Visible = False
	End If

	If RudyType = 2 Then
		PrRudy.Visible = False
		prRIWCage.Material = "RudyFrame"
		prRudyScoop.Material = "RudyFrame"
		PrMouthb.Material = "RudyFrame"
		pPennywiseHair.Visible = False
       PrMouth.Image = "Rudy mouth No face"

	End If

	If RudyType = 3 Then
		PrRudy.Visible = True
		PrLids.Image = "Rudy eyelid1c"
		PrEyeL.Image = "eye_texture2"
		PrEyeR.Image = "eye_texture2"
		PrRudy.Image = "Rudy_Face_Off_2c"
		PrRudy1.Image = "Rudy_Back_Off_2c"
		PrMouth.Image = "Rudy mouth baked off c"
		prRIWCage.Material = "RudyFrame"
		prRudyScoop.Material = "RudyFrame"
		PrMouthb.Material = "RudyFrame"
		pPennywiseHair.Visible = False
	End If

	If RudyType = 4 Then
		PrRudy.Visible = True
		PrLids.Image = "Pennywise_eyelid"
		PrEyeL.Image = "Pennywise_eye"
		PrEyeR.Image = "Pennywise_eye"
		PrRudy.Image = "Pennywise_Face"
		PrRudy1.Image = "Pennywise_Back"
		PrMouth.Image = "Pennywise_Mouth"
		prRIWCage.Material = "RudyFrame"
		prRudyScoop.Material = "RudyFrame"
		PrMouthb.Material = "RudyFrame"
		pPennywiseHair.Visible = true
	End If
End Sub

' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
'(_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_)
Dim GIxx
Sub SetGIColor ()

If GIColorMod = 3 Then
	GIColorModType = Int(Rnd*3)+1
Else
	GIColorModType = GIColorMod
End If


If GIColorModType = 0 Then

	for each GIxx in GI_LowerMain
	GIxx.Color=White
	GIxx.ColorFull=WhiteFull
	GIxx.Intensity=WhiteI
	next

	for each GIxx in GI_LowerBulbs
	GIxx.Color=WhiteBulbs
	GIxx.ColorFull=WhiteBulbsFull
	GIxx.Intensity=WhiteBulbsI
	next

	for each GIxx in GI_LowerPlastics
	GIxx.Color=Whiteplastics
	GIxx.ColorFull=WhitePlasticsFull
	GIxx.Intensity=WhitePlasticsI
	next


	for each GIxx in GI_UpperMain
	GIxx.Color=White
	GIxx.ColorFull=WhiteFull
	GIxx.Intensity=WhiteI
	next

	for each GIxx in GI_UpperBulbs
	GIxx.Color=WhiteBulbs
	GIxx.ColorFull=WhiteBulbsFull
	GIxx.Intensity=WhiteBulbsI
	next

	for each GIxx in GI_UpperPlastics
	GIxx.Color=Whiteplastics
	GIxx.ColorFull=WhitePlasticsFull
	GIxx.Intensity=WhitePlasticsI
	next

	for each GIxx in GI_Overhead
	GIxx.Color=WhiteOverhead
	GIxx.ColorFull=WhiteOverheadFull
	GIxx.Intensity=WhiteOverheadI
	next

	End If

If GIColorModType = 2 Then

	for each GIxx in GI_LowerMain
	GIxx.Color=Purple
	GIxx.ColorFull=PurpleFull
	GIxx.Intensity=PurpleI
	next

	for each GIxx in GI_LowerBulbs
	GIxx.Color=PurpleBulbs
	GIxx.ColorFull=PurpleBulbsFull
	GIxx.Intensity=PurpleBulbsI
	next

	for each GIxx in GI_LowerPlastics
	GIxx.Color=Purpleplastics
	GIxx.ColorFull=PurplePlasticsFull
	GIxx.Intensity=PurplePlasticsI
	next


	for each GIxx in GI_UpperMain
	GIxx.Color=Purple
	GIxx.ColorFull=PurpleFull
	GIxx.Intensity=PurpleI
	next

	for each GIxx in GI_UpperBulbs
	GIxx.Color=PurpleBulbs
	GIxx.ColorFull=PurpleBulbsFull
	GIxx.Intensity=PurpleBulbsI
	next

	for each GIxx in GI_UpperPlastics
	GIxx.Color=Purpleplastics
	GIxx.ColorFull=PurplePlasticsFull
	GIxx.Intensity=PurplePlasticsI
	next

	for each GIxx in GI_UpperPlastics
	GIxx.Color=PurpleOverhead
	GIxx.ColorFull=PurpleOverheadFull
	GIxx.Intensity=PurpleOverheadI
	next


	for each GIxx in GI_Overhead
	GIxx.Color=PurpleOverhead
	GIxx.ColorFull=PurpleOverheadFull
	GIxx.Intensity=PurpleOverheadI
	next

End If



If GIColorModType = 1 Then

	for each GIxx in GI_LowerMain
	GIxx.Color=Blue
	GIxx.ColorFull=BlueFull
	GIxx.Intensity=BlueI
	next

	for each GIxx in GI_LowerBulbs
	GIxx.Color=BlueBulbs
	GIxx.ColorFull=BlueBulbsFull
	GIxx.Intensity=BlueBulbsI
	next

	for each GIxx in GI_LowerPlastics
	GIxx.Color=Blueplastics
	GIxx.ColorFull=BluePlasticsFull
	GIxx.Intensity=BluePlasticsI
	next


	for each GIxx in GI_UpperMain
	GIxx.Color=Blue
	GIxx.ColorFull=BlueFull
	GIxx.Intensity=BlueI
	next

	for each GIxx in GI_UpperBulbs
	GIxx.Color=BlueBulbs
	GIxx.ColorFull=BlueBulbsFull
	GIxx.Intensity=BlueBulbsI
	next

	for each GIxx in GI_UpperPlastics
	GIxx.Color=Blueplastics
	GIxx.ColorFull=BluePlasticsFull
	GIxx.Intensity=BluePlasticsI
	next


	for each GIxx in GI_Overhead
	GIxx.Color=BlueOverhead
	GIxx.ColorFull=BlueOverheadFull
	GIxx.Intensity=BlueOverheadI
	next
End If

End Sub



'**********************************************************************************************************
'**********************************************************************************************************
'***RGB OPTIONS COLOR ADJUST***
'**********************************************************************************************************
'**********************************************************************************************************

Dim White, WhiteFull, WhiteI, WhitePlastics, WhitePlasticsFull, WhitePlasticsI, WhiteBulbs, WhiteBulbsFull, WhiteBulbsI, WhiteOverheadFull, WhiteOverhead, WhiteOverheadI
WhiteFull = rgb(255,255,180)
White = rgb(255,255,180)
WhiteI = 2
WhitePlasticsFull = rgb(255,255,180)
WhitePlastics = rgb(255,255,180)
WhitePlasticsI = 10
WhiteBulbsFull = rgb(255,255,180)
WhiteBulbs = rgb(255,255,180)
WhiteBulbsI = 750
WhiteOverheadFull = rgb(255,255,180)
WhiteOverhead = rgb(255,255,180)
WhiteOverheadI = 2

Dim Blue, BlueFull, BlueI, BluePlastics, BluePlasticsFull, BluePlasticsI, BlueBumper, BlueBumperFull, BlueBumperI, BlueBulbs, BlueBulbsFull, BlueBulbsI, BlueOverheadFull, BlueOverhead, BlueOverheadI
BlueFull = rgb(10,10,255)
Blue = rgb(10,10,255)
BlueI = 2
BluePlasticsFull = rgb(10,10,255)
BluePlastics = rgb(10,10,255)
BluePlasticsI = 15
BlueBulbsFull = rgb(10,10,255)
BlueBulbs = rgb(10,10,255)
BlueBulbsI = 3000
BlueOverheadFull = rgb(10,10,255)
BlueOverhead = rgb(10,10,255)
BlueOverheadI = 2

Dim Purple, PurpleFull, PurpleI, PurplePlastics, PurplePlasticsFull, PurplePlasticsI, PurpleBumper, PurpleBumperFull, PurpleBumperI, PurpleBulbs, PurpleBulbsFull, PurpleBulbsI,PurpleOverheadFull,PurpleOverhead,PurpleOverheadI
Purple = rgb(180,0,180)
PurpleI = 6
PurplePlasticsFull = rgb(125,50,125)
PurplePlastics = rgb(125,0,125)
PurplePlasticsI = 30
PurpleBulbsFull = rgb(125,50,125)
PurpleBulbs = rgb(125,0,125)
PurpleBulbsI = 2000
PurpleOverheadFull = rgb(125,0,125)
PurpleOverhead = rgb(125,0,125)
PurpleOverheadI = 3



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
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

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





'Helper function
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


'Sub Manhole_Hit
'    PlaySound "FX_metalhit2"
'End Sub


Sub trigger1_Hit
    PlaySound "manhole"
End Sub


Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8,BallShadow9,BallShadow10)

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
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/21)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/21)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 4
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub





'******************************************************
'						TROUGH
'******************************************************

Sub sw72_Hit():Controller.Switch(72) = 1:UpdateTrough:End Sub
Sub sw72_UnHit():Controller.Switch(72) = 0:UpdateTrough:End Sub
Sub sw74_Hit():Controller.Switch(74) = 1:UpdateTrough:End Sub
Sub sw74_UnHit():Controller.Switch(74) = 0:UpdateTrough:End Sub
Sub sw63_Hit():Controller.Switch(63) = 1:UpdateTrough:End Sub
Sub sw63_UnHit():Controller.Switch(63) = 0:UpdateTrough:End Sub



Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw63.BallCntOver = 0 Then sw74.kick 60, 9
	If sw74.BallCntOver = 0 Then sw72.kick 60, 9
	Me.Enabled = 0
End Sub


'******************************************************
'					DRAIN & RELEASE
'******************************************************

Sub sw73_Hit() 'Drain
	UpdateTrough
	Controller.Switch(73) = 1
	RandomSoundDrain sw73
End Sub

Sub sw73_UnHit()  'Drain
	Controller.Switch(73) = 0
End Sub

Sub SolOuthole(enabled)
	If enabled Then
		sw73.kick 60,20
		PlaySoundAt SoundFX("solenoid",DOFContactors), sw73
	End If
End Sub

Sub ReleaseBall(enabled)
	If enabled Then
		RandomSoundBallRelease sw63
		sw63.kick 60, 12
		UpdateTrough
	End If
End Sub

'************************************************************

 Sub Glass_Hit
   Select Case Int(Rnd()*4)
     Case 0
       Playsound "AXSGlassHit6a"
       Playsound "AXSGlassHit6a"
     Case 1
       Playsound "AXSGlassHit3a"
       Playsound "AXSGlassHit3a"
     Case 2
       Playsound "AXSGlassHit3a"
     Case 3
       Playsound "AXSGlassHit6a"
     End Select
   End Sub

'*************************************************************
Dim shadowopacity
Shadow.opacity = shadowopacity

'//////////////////////////////////////////////////////////////////////
'// TIMERS
'//////////////////////////////////////////////////////////////////////


' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
End Sub


' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
End Sub

'//////////////////////////////////////////////////////////////////////
'// TargetBounce
'//////////////////////////////////////////////////////////////////////

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
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

' Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub

'//////////////////////////////////////////////////////////////////////
'// PHYSICS DAMPENERS
'//////////////////////////////////////////////////////////////////////

' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR



Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
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
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

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
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
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
		End If
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

'//////////////////////////////////////////////////////////////////////
'// TRACK ALL BALL VELOCITIES FOR RUBBER DAMPENER AND DROP TARGETS
'//////////////////////////////////////////////////////////////////////

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

'//////////////////////////////////////////////////////////////////////
'// RAMP ROLLING SFX
'//////////////////////////////////////////////////////////////////////

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(6)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub


Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function

'//////////////////////////////////////////////////////////////////////
'// END RAMP ROLLING SFX
'//////////////////////////////////////////////////////////////////////

'//////////////////////////////////////////////////////////////////////
'// RAMP TRIGGERS
'//////////////////////////////////////////////////////////////////////

Sub ramptrigger01_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger02_hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub ramptrigger02_unhit()
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
End Sub

Sub ramptrigger03_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub ramptrigger03_unhit()
	PlaySoundAt "WireRamp_Stop", ramptrigger03
End Sub

Sub ramptrigger04_hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub ramptrigger04_unhit()
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
End Sub

Sub ramptrigger05_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub ramptrigger05_unhit()
	PlaySoundAt "WireRamp_Stop", ramptrigger05
End Sub

Sub ramptrigger001_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger0001_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

'//////////////////////////////////////////////////////////////////////
'// Ball Rolling
'//////////////////////////////////////////////////////////////////////

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If

		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = BOT(b).X
			BallShadowA(b).visible = 1
		End If
	Next
End Sub

'//////////////////////////////////////////////////////////////////////
'// Mechanic Sounds
'//////////////////////////////////////////////////////////////////////

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
' 	Metals (all metal objects, metal walls, metal posts, metal wire guides)
' 	Apron (the apron walls and plunger wall)
' 	Walls (all wood or plastic walls)
' 	Rollovers (wire rollover triggers, star triggers, or button triggers)
' 	Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
' 	Gates (plate gates)
' 	GatesWire (wire gates)
' 	Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.  
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).  
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Tutorial vides by Apophis
' Part 1: 	https://youtu.be/PbE2kNiam3g
' Part 2: 	https://youtu.be/B5cm1Y8wQsk
' Part 3: 	https://youtu.be/eLhWyuYOyGg


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


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


'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

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

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^2 * 15
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

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
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
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

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

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'/////////////////////////////////////////////////////////////////
'					CAB MODE
'/////////////////////////////////////////////////////////////////

If Cabmode = 1 Then

rightrail.visible = 0
leftrail.visible = 0
pSidewallCustom.size_y = 1.5
pSidewall.size_y = 1.5
Else 

rightrail.visible = 1
leftrail.visible = 1
pSidewallCustom.size_y = 1.02
pSidewall.size_y = 0.94
End if
