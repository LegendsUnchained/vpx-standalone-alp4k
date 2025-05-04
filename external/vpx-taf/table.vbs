'TAF for VPX by Sliderpoint

 Option explicit
 Randomize

' Thalamus 2018-07-24
' Added/Updated "Positional Sound Playback Functions" and "Supporting Ball & Sound Functions"
' Thalamus 2018-08-18 : Improved directional sounds

Const VolDiv = 400

Const VolBump   = .2    ' Bumpers multiplier.
Const VolGates  = 1    ' Gates volume multiplier.
Const VolMetals = 1    ' Metals volume multiplier.
Const VolRH     = 1    ' Rubber hits multiplier.
Const VolRPo    = 1    ' Rubber posts multiplier.
Const VolRPi    = 1    ' Rubber pins multiplier.
Const VolTarg   = 1    ' Targets multiplier.

Const VolSpin   = 1.5  ' Spinners volume.
Const VolFlip   = .3    ' Flipper volume.


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim OptReset
'OptReset = 1  'Uncomment to reset to default options in case of error OR keep all changes temporary
Dim DefaultOptions
'DefaultOptions = 1*optBear+1*optFester+1*optBox+1*optCousinItt+1*optBooks+1*optBetas+1*optSwLight+1*optCVLight
Dim ThingTrainer

ThingTrainer = 0 ' set to 1 if you want to run the auto-play thing trainer

Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim UseVPMDMD
Dim Incline
Const UseVPMModSol = 1

UseVPMDMD = DesktopMode

' Const BallMass = 1.7

LoadVPM "01120100", "WPC.VBS", 3.26

Sub LoadCoreVBS
     On Error Resume Next
     ExecuteGlobal GetTextFile("core.vbs")
     If Err Then MsgBox "Can't open core.vbs"
     On Error Goto 0
End Sub

'Standard Definitions
Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
Const SCoin="Coin3"

Set GICallback2 = GetRef("UpdateGI")

Dim bsTrough, bsThingSaucer, BookMech, ThingMech
Dim UpperMagnet, RightMagnet, LeftMagnet
Dim LeftFlipperButton
Dim PrimCase1, PrimCase2, PrimCase3, PrimCase4, PrimCase6, PrimCase7, PrimCase8
Dim DayNight

'table init
Const cGameName = "TAF_L7"

Sub Table1_Init()
	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine="The Addams Family by Bally from 1992"
		.Games(cGameName).Settings.Value("rol") = 0
		.HandleKeyboard=0
		.ShowTitle=0
		.ShowDMDOnly=1
		.HandleMechanics=0
		.DIP(0)=&H00
		.ShowFrame=0
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
		If DesktopMode = True THEN
'			If cNewController = 1 Then
'				Controller.Hidden = 0
'			Else
'				Controller.Hidden = 0
'			End If
			L81.Visible = True
            L81F.Visible = True
			L82.Visible = True
            L82F.Visible = True
			L83.Visible = True
            L83F.Visible = True
			L84.Visible = True
            L84F.Visible = True
			L85.Visible = True
            L85F.Visible = True
			L86.Visible = True
            L86F.Visible = True
			L87.Visible = True
            L87F.Visible = True
            ThingLights.Visible = True
		  Else
			Controller.Hidden = 0
			L81.Visible = False
            L81F.Opacity = 0
			L82.Visible = False
            L82F.Opacity = 0
			L83.Visible = False
            L83F.Opacity = 0
			L84.Visible = False
            L84F.Opacity = 0
			L85.Visible = False
            L85F.Opacity = 0
			L86.Visible = False
            L86F.Opacity = 0
			L87.Visible = False
            L87F.Opacity = 0
		    ThingLights.Visible = False
			RailRight.Visible = False
			RailLeft.Visible = False
		end If
	End With

	Set bsTrough = New cvpmTrough
	With bsTrough
	.size = 3
	.entrySw = 18
	.initSwitches Array(17, 16, 15)
	.Initexit BallRelease, 90, 8
	.InitExitSounds SoundFX("BallRelease",DOFContactors), SoundFX("ballrelease",DOFContactors)
	.Balls = 3
	End With

	  Set BookMech = New cvpmMech
 	  With BookMech
		.Sol1 = 27
		.Length = 100
		.Steps = 90
		.MType = vpmMechOneSol + vpmMechReverse + vpmMechNonLinear
	  	.AddSw 81, 89, 90 					' Bookcase Open
	  	.AddSw 82, 0, 1 					' Bookcase Close
		.Callback = GetRef("BookCaseMotor")
		.Start
      End With

	  Set ThingMech = New cvpmMech
 	  With ThingMech
		.Sol1 = 25
		.Length = 175
		.Steps = 60
		.MType = vpmMechOneSol + vpmMechReverse + vpmMechNonLinear
	  	.AddSw 85, 59, 60 					' Thing Up Opto
	  	.AddSw 84, 0, 1 					' Thing Down Opto
		.Callback = GetRef("ThingMotor")
		.Start
      End With

	Set UpperMagnet = New cvpmMagnet
 	With UpperMagnet
		.InitMagnet UMagnet, 8
		.GrabCenter = 0
 		.solenoid = 23						'Upper Magnet
		.CreateEvents "UpperMagnet"
	'	.magnetOn
	End With

 	Set RightMagnet = New cvpmMagnet
 	With RightMagnet
		.InitMagnet RMagnet, 8
		.GrabCenter = 0
		.solenoid = 24						'Right Magnet
		.CreateEvents "RightMagnet"
	'	.MagnetOn
	End With

 	Set LeftMagnet = New cvpmMagnet
 	With LeftMagnet
		.InitMagnet LMagnet, 8
		.GrabCenter = 0
		.solenoid = 16						'Left Magnet
		.CreateEvents "LeftMagnet"
	'	.MagnetOn
	End With

End Sub

Sub TAF_Paused:Controller.Pause = 1:End Sub
Sub TAF_unPaused:Controller.Pause = 0:End Sub
Sub TAF_Exit:Controller.Stop:End Sub

'Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 2

'Solonoids
SolCallback(1)="Chair" 														'Chair Kickout
SolCallback(2)="Knocker" 													'Thing Knocker
SolCallback(3)="vpmSolDiverter Diverter,""MetalHit_medium"","				'Ramp Diverter
SolCallback(4)="bsTrough.SolOut"  											'Ball Release
SolCallBack(5)="bsTrough.SolIn" 											'Outhole
SolCallback(6)="ThingMagnet" 												'Thing Magnet
SolCallback(7)="ThingKickout" 												'Thing Kickout
SolCallback(8)="LockupKickout"												'Lockup Kickout
'SolCallback(9)= 															'Upper Left Jet
'SolCallback(10)= 															'Upper Right Jet
'SolCallback(11)=															'Center Left Jet
'SolCallBack(12)=			  												'Center Right Jet
'SolCallback(13)= 															'Lower Jet
'SolCallback(14)= 															'Left Slingshot
'SolCallback(15)= 															'Right Slingshot
'SolCallback(16)=		 													'Left Magnet
SolModCallback(17)="TelephoneFlasher"								'Telephone/Upper Right Ramp - flasher
SolModCallback(18)="TrainFlasher" 									'Train/Upper Left Ramp - flasher
SolModCallback(19)="LowerRampFlasher" 								'Lower Ramp/Jet Bumpers (2) - flasher
SolModCallback(20)="LlightningBoltFlasher" 						'Left Lighting bolt/Mini Flipper - flasher
SolModCallback(21)="RlightningBoltFlasher"							'Right Lightning bolt/Swampt - flasher
SolModCallback(22)="ThePowerFlasher"											'The Power/Backbox Cloud(3) - flasher
'SolCallback(23)=															'Upper Magnet
'SolCallback(24)=															'Right Magnet
'SolCallback(25)=															'Thing Motor
SolCallback(26)="ThingEjectHole"											'Thing Eject Hole - flasher
'SolCallback(27)=															'Bookcase Motor
SolCallback(28)="SwampRelease"												'Swamp Release

'Flippers
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sURFlipper) = "SolURFlipper"
SolCallback(sULFlipper) = "SolULFlipper"

'Switch Information
''Controller.Switch(13) 'Plumb Bob Tilt
''Controller.Switch(15) 'Left Trough
''Controller.Switch(16) 'Center Trough
''Controller.Switch(17) 'Right Trough
''Controller.Switch(18) 'outhole
'
''Controller.Switch(21) 'Slam Tilt
''Controller.Switch(22) 'Coin Door Closed
'
''Controller.Switch(24) 'always Closed
''Controller.Switch(25) 'Right Flipper Lane
''Controller.Switch(26) 'Right Outlane
''Controller.Switch(27) 'Ball Shooter
'
''Controller.Switch(31) 'Upper Left Jet
''Controller.Switch(32) 'Upper Right Jet
''Controller.Switch(33) 'Center Left Jet
''Controller.Switch(34) 'Center Right Jet
''Controller.Switch(35) 'Lower jet
''Controller.Switch(36) 'Left Slingshot
''Controller.Switch(37) 'Right Slingshot
''Controller.Switch(38) 'Upper Left Loop
'
''Controller.Switch(41) 'Grave "G"
''Controller.Switch(42) 'Grave "R"
''Controller.Switch(43) 'Chair Kickout
''Controller.Switch(44) 'Cousin It (a and b)
''Controller.Switch(45) 'Lower Swamp Million
'
''Controller.Switch(47) 'Center Swamp Million
''Controller.Switch(48) 'Upper Swamp Million
'
''Controller.Switch(51) 'Shooter Lane
'
''Controller.Switch(53) 'Bookcase Opto 1
''Controller.Switch(54) 'Bookcase Opto 2
''Controller.Switch(55) 'Bookcase Opto 3
''Controller.Switch(56) 'Bookcase Opto 4
''Controller.Switch(57) 'Bumper Lane Opto
''Controller.Switch(58) 'Right Ramp Exit
'
''Controller.Switch(61) 'Left Ramp Enter
''Controller.Switch(62) 'Train Wreck
''Controller.Switch(63) 'Thing Eject Lane
''Controller.Switch(64) 'Right Ramp Enter
''Controller.Switch(65) 'Right Ramp Top
''Controller.Switch(66) 'Left Ramp Top
''Controller.Switch(67) 'Upper Right Loop
''Controller.Switch(68) 'Vault
'
''Controller.Switch(71) 'Swamp Lock upper
''Controller.Switch(72) 'Swamp Lock Center
''Controller.Switch(73) 'Swamp Lock Lower
''Controller.Switch(74) 'Lockup Kickout
''Controller.Switch(75) 'Left Outlane
''Controller.Switch(76) 'Left Flipper Lane 2
''Controller.Switch(77) 'Thing Kickout
''Controller.Switch(78) 'Left Flipper Lane 1
'
''Controller.Switch(81) 'BookCase Open
''Controller.Switch(82) 'BookCase Close
'
''Controller.Switch(84) 'Thing Down Opto
''Controller.Switch(85) 'Thing Up Opto
''Controller.Switch(86) 'Grave "A"
''Controller.Switch(87) 'Thing Eject Hole

Sub GameTimer1_Timer
	flipperL.RotY = LeftFlipper.CurrentAngle
    flipperR.RotY = RightFlipper.CurrentAngle
    lfs.RotZ = LeftFlipper.CurrentAngle
    rfs.RotZ = RightFlipper.CurrentAngle
'	flipperL1.RotY = LeftminiFlipper.CurrentAngle
    flipperR1.RotY = Flipper1.CurrentAngle
	DiverterPrim.ObjRotZ = Diverter.CurrentAngle - 137
 End Sub


'Flippers
Sub SolRFlipper(Enabled)
	If Enabled Then
		RightFlipper.RotateToEnd
		PlaySoundAtVol SoundFX("fx_flipperup",DOFFlippers), RightFlipper, VolFlip
	Else
		RightFlipper.RotateToStart
		PlaySoundAtVol SoundFX("fx_flipperdown",DOFFlippers),RightFlipper, VolFlip
	End If
End Sub

Sub SolURFlipper(Enabled)
	If Enabled Then
		Flipper1.RotateToEnd
'		PlaySoundAtVol SoundFX("fx_flipperup"), 0, 1, 0.05, 0.05
	Else
		Flipper1.RotateToStart
'		PlaySoundAtVol SoundFX("fx_flipperdown"), 0, 1, 0.05, 0.05
	End If
End Sub

Sub SolLFlipper(Enabled)
	If Enabled Then
		LeftFlipper.RotateToEnd
		PlaySoundAtVol SoundFX("fx_flipperup",DOFFlippers),LeftFlipper, VolFlip
	Else
       	LeftFlipper.RotateToStart
		PlaySoundAtVol SoundFX("fx_flipperdown",DOFFlippers),LeftFlipper, VolFlip
	End If
End Sub


Sub SolULFlipper(Enabled)
	If Enabled Then
		Flipper2.RotateToEnd
		If 	LeftFlipperButton = 0 Then PlaySoundAtVol SoundFX("fx_flipperup",DOFFlippers), Flipper2, VolFlip
	Else
		PlaySoundAtVol SoundFX("fx_flipperdown",DOFFlippers), Flipper2, VolFlip
		Flipper2.RotateToStart
	End if
End Sub

DayNight = table1.NightDay


Sub Table1_KeyDown(ByVal keycode)

	If vpmKeyDown(keycode) Then Exit Sub

	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySoundAt "plungerpull", Plunger
	End If

	If keycode = LeftTiltKey Then
		Nudge 90, 2
	End If

	If keycode = RightTiltKey Then
		Nudge 270, 2
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 2
	End If

	If Keycode = LeftFlipperKey Then
		LeftFlipperButton = 1
	End if

	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySoundAt "plungerpull", Plunger
	End If

End Sub

Sub Table1_KeyUp(ByVal keycode)

     If vpmKeyUp(keycode) Then Exit Sub

	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySoundAt "plunger", Plunger
	End If

	If Keycode = LeftFlipperKey Then
		LeftFlipperButton = 0
	End if


End Sub

Sub Chair (enabled)
	If Enabled Then
		ChairKicker.TimerEnabled = 1
	End If
End Sub

Sub ThingMagnet(Enabled)
	If Enabled = true and  Position> 25 Then
		ThingBall = true
		ThingSaucer.destroyball
		Controller.Switch(87) = 0
		Playsound "MetalHit_Medium"
	End If
	If enabled = false and position< -85 and ThingBall=true then
 	   ThingBall=false
	   ThingKickOutKicker.CreateBall
	   Controller.Switch(77) = 1   	'Thing KickOut
 	End If
 	If enabled = false and position> 25 then
       ThingBall=false
	   ThingSaucer.createball
		Controller.switch(87) = 1
 	End If
End Sub

Sub ThingKickOut(Enabled)
	If Enabled Then
		ThingKickOutKicker.TimerEnabled = 1
	End If
End Sub

Sub ThingKickOutKicker_Timer
	ThingKickOutKicker.Kick 180, 6, 5   'thinkkickoutkicker
    Controller.switch(77) = 0	'Thing KickOut
	Playsound "SubWay"
	ThingKickOutKicker.TimerEnabled = 0
End Sub

Sub LockupKickout(Enabled)
	If Enabled Then
		SwampLockUp.TimerEnabled = 1
	End If
End Sub

Sub	 LlightningBoltFlasher(Level)
	 If Level > 0 Then
		LLightning.intensityScale = (Level / 2.55)/100
		Light13.intensityScale = (Level / 2.55)/100
		Light13d.intensityScale = (Level / 2.55)/100
		Light15.intensityScale = (Level / 2.55)/100
		Light25.intensityScale = (Level / 2.55)/100
		LLightning.State = 1
		Light13.state = 1
		Light13d.state = 1
		Light15.state = 1
		Light25.state = 1
		PrimCase7 = 1
		Prim7.Enabled = 1
	Else
		LLightning.State = 0
		Light13.state = 0
		Light13d.state = 0
		Light15.state = 0
		Light25.state = 0
		PrimCase7 = 4
		Prim7.Enabled = 1
	End If
End Sub

Sub	 RlightningBoltFlasher(Level)
	 If Level > 0 Then
		RLightning.intensityScale = (Level / 2.55)/100
		Light4.intensityScale = (Level / 2.55)/100
		Light4b.intensityScale = (Level / 2.55)/100
		Light6.intensityScale = (Level / 2.55)/100
		Light23.intensityScale = (Level / 2.55)/100
		RLightning.State = 1
		Light4.state = 1
		Light4b.state = 1
		Light6.state = 1
		Light23.state = 1
		PrimCase3 = 1
		Prim3.Enabled = 1
	Else
		RLightning.State = 0
		Light4.state = 0
		Light4b.state = 0
		Light6.state = 0
		Light23.state = 0
		PrimCase3 = 4
		Prim3.Enabled = 1
	End If
End Sub

Sub LowerRampFlasher(level)
	If Level > 0 Then
	Light10.intensityScale = (Level / 2.55)/100
'	Light10b.intensityScale = (Level / 2.55)/100
	Light12.intensityScale = (Level / 2.55)/100
	Light24.intensityScale = (Level / 2.55)/100
	Light20.intensityScale = (Level / 2.55)/100
	Light10.State = 1
'	Light10b.State = 1
	Light12.State = 1
	Light24.State = 1
	Light20.State = 1
	Else
	Light10.State = 0
'	Light10b.State = 0
	Light12.State = 0
	Light24.State = 0
	Light20.State = 0
	End If
End Sub

Sub ThePowerFlasher(Level)
	If Level > 0 Then
		ThePower.intensityScale = (Level / 2.55)/100
		ThePower.State = 1
	Else
		ThePower.State = 0
	End If
End Sub

Sub TrainFlasher(level)
	If Level > 0 Then
		Light1.intensityScale = (Level / 2.55)/100
'		Light1b.intensityScale = (Level / 2.55)/100
		Light2.intensityScale = (Level / 2.55)/100
'		Light2b.intensityScale = (Level / 2.55)/100
		Light3.intensityScale = (Level / 2.55)/100
		Light35.intensityScale = (Level / 2.55)/100
		Light19.intensityScale = (Level / 2.55)/100
		Light1.State = 1
'		Light1b.State = 1
		Light2.State = 1
'		Light2b.State = 1
		Light3.State = 1
		Light35.State = 1
		Light19.State = 1
	Else
		Light1.State = 0
'		Light1b.State = 0
		Light2.State = 0
'		Light2b.State = 0
		Light3.State = 0
		Light35.State = 0
		Light19.State = 0
	End If
End Sub

Sub TelephoneFlasher(level)
	If Level > 0 Then
		Light7.intensityScale = (Level / 2.55)/100
		Light7b.intensityScale = (Level / 2.55)/100
		Light16.intensityScale = (Level / 2.55)/100
'		Light16b.intensityScale = (Level / 2.55)/100
		Light18.intensityScale = (Level / 2.55)/100
		Light21.intensityScale = (Level / 2.55)/100
		Light22.intensityScale = (Level / 2.55)/100
		Light7.State = 1
		Light7b.State = 1
		Light16.State = 1
'		Light16b.State = 1
		Light18.State = 1
		Light21.State = 1
		Light22.State = 1
	Else
		Light7.State = 0
		Light7b.State = 0
		Light16.State = 0
'		Light16b.State = 0
		Light18.State = 0
		Light21.State = 0
		Light22.State = 0
	End if
End Sub

Sub OutHole_Hit()
	PlaySoundAt "drain", OutHole
	bsTrough.addBall me
End Sub

Sub Knocker(Enabled)
	If Enabled Then
	PlaySound SoundFX("knocker",DOFKnocker)
	End If
End Sub

Sub ChairKicker_Hit
	PlaySound "kicker_enter_center", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Controller.Switch(43) = 1   'Chair KickOut
End Sub

Sub ChairKicker_unHit
		Controller.Switch(43) = 0   'Chair KickOut
End Sub

Sub ChairKicker_Timer
	ChairKicker.kick 0, 37, 70		'chair kicker
	PlaySound SoundFX("popper_ball",DOFContactors),0,.75,0,0.25
	ChairKicker.TimerEnabled = 0
End Sub

Sub SwampRelease(Enabled)
	If Enabled Then
	SwampReleaseKicker.TimerEnabled = 1
	End If
End Sub

Sub SwampLockUp_Hit
	PlaySound "kicker_enter_center", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Controller.Switch(74) = 1     'LockUp Kickout
End Sub

Sub SwampLockUp_UnHit
	Controller.Switch(74) = 0     'LockUp Kickout
End Sub

Sub SwampLockUp_Timer
	SwampLockUp.kick 0, 38, 120				'swamp kicker
	PlaySound SoundFX("popper_ball",DOFContactors),0,.25,0,0.25
	SwampLockUp.TimerEnabled = 0
End Sub

Sub SwampReleaseKicker_Hit
		Controller.Switch(73) = 1    'Swamp Lock Lower
'		Wall69.isdropped = 0
End Sub

Sub SwampReleaseKicker_Timer
	SwampReleaseKicker.kick 300, 15, 5
	Controller.Switch(73) = 0     'Swamp Lock Lower
'	Wall69.isDropped = 1
	SwampReleaseKicker.TimerEnabled = 0
End Sub

Sub SW71_Hit
	Controller.Switch(71) = 1   'Swamp Lock Upper
End Sub

Sub SW71_UnHit
	Controller.Switch(71) = 0   'Swamp Lock Upper
End Sub

Sub SW72_Hit
	Controller.Switch(72) = 1    'Swamp Lock Middle
'	Wall70.isdropped = 0
End Sub

Sub SW72_UnHit
	Controller.Switch(72) = 0    'Swamp Lock Middle
'	Wall70.isDropped = 1
End Sub

Sub ThingEjectHole(Enabled)
	If Enabled Then
	ThingSaucer.TimerEnabled = 1
	End If
End Sub

Sub ThingSaucer_Hit
		PlaySound "ThingSaucerHit", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Controller.Switch(87) = 1
End Sub

Sub ThingSaucer_Timer
	ThingSaucer.Kickz 90, 12, .6, 15   'ThingSaucer
	PlaySound SoundFX("SaucerKick",DOFContactors), 0, .3,.6,0
	Controller.Switch(87) = 0
	ThingSaucer.TimerEnabled = 0
End Sub

Sub Bumper1_Hit
	PlaySoundAtVol SoundFX("bumper",DOFContactors), Bumper1, VolBump
	vpmTimer.PulseSw 31 ''Upper Left Jet
	Me.TimerEnabled = 1
End Sub

Sub Bumper1_Timer
	Me.Timerenabled = 0
End Sub

Sub Bumper2_Hit
	PlaySoundAtVol SoundFX("bumper",DOFContactors), Bumper2, VolBump
	vpmTimer.PulseSw 32  'Upper Right Jet
	Me.TimerEnabled = 1
End Sub

Sub Bumper2_Timer
	Me.Timerenabled = 0
End Sub

Sub Bumper3_Hit
	PlaySoundAtVol SoundFX("bumper",DOFContactors), Bumper3, VolBump
	vpmTimer.PulseSw 33 'Center Left Jet
	Me.TimerEnabled = 1
End Sub

Sub Bumper3_Timer
	Me.Timerenabled = 0
End Sub

Sub Bumper4_Hit
	PlaySoundAtVol SoundFX("bumper",DOFContactors), Bumper4, VolBump
	vpmTimer.PulseSw 34 'Center Right Jet
	Me.TimerEnabled = 1
End Sub

Sub Bumper4_Timer
	Me.Timerenabled = 0
End Sub

Sub Bumper5_Hit
	PlaySoundAtVol SoundFX("bumper",DOFContactors), Bumper5, VolBump
	vpmTimer.PulseSw 35 'Lower Jet
	Me.TimerEnabled = 1
End Sub

Sub Bumper5_Timer
	Me.Timerenabled = 0
End Sub

Sub SW27_Hit
	Controller.Switch(27) = 1   'Ball Shooter
End Sub
Sub SW27_UnHit
	Controller.Switch(27) = 0	'Ball Shooter
End  Sub

Sub SW26_hit
	Controller.Switch(26) = 1	'Right Outlane
End Sub
Sub SW26_unHit
	Controller.Switch(26) = 0	'Right Outlane
End Sub

Sub SW25_Hit
	Controller.Switch(25) = 1	'Right Flipper Lane
End Sub
Sub SW25_unHit
	Controller.Switch(25) = 0  	'Right Flipper Lane
End Sub

Sub SW38_Hit
	Controller.Switch(38) = 1	'Upper Left Loop
End Sub
Sub SW38_UnHit
	Controller.Switch(38) = 0	'Upper Left Loop
End Sub

 Sub BookCaseMotor(aNewPos,aSpeed,aLastPos)
	DIM OBJ
 	PlaySoundAt SoundFX("Motor2",DOFGear), Bookcase
	Bookcase.rotZ = aNewPos
	BCPlastic.rotZ = aNewPos
	BCScrews.rotZ = aNewPos
	BCRubbers.rotZ = aNewPos
	BCPegs.rotZ = aNewPos
	if aNewPos >60 then
		for each obj in BookcaseOpen
			obj.collidable = true
		next
		for each obj in BookcaseClosed
			obj.collidable = false
		next
	else
		for each obj in BookcaseOpen
			obj.collidable = false
		next
		for each obj in BookcaseClosed
			obj.collidable = true
		next
	end if
 End Sub

Sub TestTimer_Timer

End Sub


Dim Position, HandPosition, Thingball

  Sub ThingMotor(aNewPos,aSpeed,aLastPos)
	dim BoxPosition
 	PlaySoundAt SoundFX("Motor",DOFGear), Thing
	position = aNewPos * 2 - 90
	if aNewPos < 3 then
		BoxPosition = aNewPos
	elseif aNewPos < 33 then
		BoxPosition = aNewPos * 1.25
	elseif aNewPos =< 34 then
		BoxPosition = aNewPos * 1.2
	elseif aNewPos =< 35 then
		BoxPosition = aNewPos * 1.14
	elseif aNewPos =< 36 then
		BoxPosition = aNewPos * 1.1
	elseif aNewPos =< 37 then
		BoxPosition = aNewPos * 1.055
	elseif aNewPos =< 38 then
		BoxPosition = aNewPos * 1
	elseif aNewPos =< 39 then
		BoxPosition = aNewPos * .965
	elseif aNewPos =< 40 then
		BoxPosition = aNewPos * .925
	elseif aNewPos < 50 then
		BoxPosition = 34
	elseif aNewPos =< 51 then
		BoxPosition = aNewPos * .685
	elseif aNewPos =< 52 then
		BoxPosition = aNewPos * .67
	elseif aNewPos =< 53 then
		BoxPosition = aNewPos * .655
	elseif aNewPos =< 54 then
		BoxPosition = aNewPos * .64
	elseif aNewPos =< 55 then
		BoxPosition = aNewPos * .625
	elseif aNewPos =< 56 then
		BoxPosition = aNewPos * .61
	elseif aNewPos =< 57 then
		BoxPosition = aNewPos * .59
	elseif aNewPos =< 58 then
		BoxPosition = aNewPos * .56
	elseif aNewPos =< 59 then
		BoxPosition = aNewPos * .55
	elseif aNewPos =< 60 then
		BoxPosition = aNewPos * .53
	else
		BoxPosition = 32
	end if

	Thing.RotY = position
	ThingMag.RotY = position
	ThingMagNut.RotY = position
	if ThingBall then
		ThingBallprim.RotY = -1*(position + 120)
	else
		ThingBallprim.RotY = 0

	end if
	thingBox.Rotx = BoxPosition '-90
 End Sub

'Targets
Sub SW41_Hit
	vpmTimer.PulseSw 41			'Grave "G"
End Sub

Sub SW42_Hit
	vpmTimer.PulseSw 42			'Grave "R"
End Sub

Sub SW44a1_Hit
	vpmTimer.PulseSw 44			''Cousin It (a1)
End Sub

Sub SW44a2_Hit
	vpmTimer.PulseSw 44			'Cousin It (a1)
End Sub

Sub SW44b1_Hit
	vpmTimer.PulseSw 44			'Cousin It (b1)
End Sub

Sub SW44b2_Hit
	vpmTimer.PulseSw 44			'Cousin It (b2)
End Sub

Sub SW45_Hit
	vpmTimer.PulseSw 45			'Lower Swamp Million
End Sub

Sub SW47_Hit
	vpmTimer.PulseSw 47			'Center Swamp Million
End Sub

Sub SW48_Hit
	vpmTimer.PulseSw 48			'Upper Swamp Million
End Sub



Sub SW51_Hit
	VPMTimer.PulseSw 51			'Shooter Lane
End Sub

Sub SW53_Hit
	vpmTimer.PulseSw 53			'Bookcase Opto 1
End Sub
Sub SW54_Hit
	vpmTimer.PulseSw 54			'Bookcase Opto 2
End Sub
Sub SW55_Hit
	vpmTimer.PulseSw 55			'Bookcase Opto 3
End Sub
Sub SW56_Hit
	vpmTimer.PulseSw 56			'Bookcase Opto 4
End Sub

Sub SW57_Hit
	Controller.Switch(57) = 1'vpmTimer.PulseSw 57			'Bumper Lane Opto
End Sub
Sub SW57_unHit
	Controller.Switch(57) = 0'vpmTimer.PulseSw 57			'Bumper Lane Opto
End Sub

Sub SW58_Hit
	vpmTimer.PulseSw 58			'Right Ramp Exit
	StopSound "Rail"
'	SwitchLever2.RotZ = 80
	me.timerenabled = 1
End Sub

Sub SW58_timer
'	SwitchLever2.Rotz = 95
	me.timerenabled = 0
End Sub

Sub SW61_Hit
	vpmTimer.PulseSw 61			'Left Ramp Enter
End Sub

Sub SW62_Hit
	vpmTimer.PulseSw 62			'Train Wreck
End Sub

Sub SW63_hit
	Controller.Switch(63) = 1	'Thing Eject Lane
End Sub
Sub SW63_unHit
	Controller.Switch(63) = 0	'Thing Eject Lane
End Sub

Sub SW64_Hit
	vpmTimer.PulseSw 64			'Right Ramp Enter
End Sub

Sub SW65_Hit
	vpmTimer.PulseSw 65			'Right Ramp Top
'	SwitchLever1.ObjRotx = 10:SwitchLever1.ObjRotZ = 10
	Me.TimerEnabled = 1
End Sub

Sub SW65_Timer
'	SwitchLever1.ObjRotx = 0:SwitchLever1.ObjRotz = 0
	me.timerEnabled = 0
End Sub

Sub SW66_Hit
	vpmTimer.PulseSw 66			'Left Ramp Top
End Sub

Sub SW67_hit
	Controller.Switch(67) = 1	'Upper Right Loop
End Sub
Sub SW67_unHit
	Controller.Switch(67) = 0	'Upper Right Loop
End Sub

Sub SW68_Hit
	vpmTimer.PulseSw 68			'Vault
End Sub

Sub SW75_hit
	Controller.Switch(75) = 1	'Left Outlane
End Sub
Sub SW75_unHit
	Controller.Switch(75) = 0	'Left Outlane
End Sub

Sub SW76_hit
	Controller.Switch(76) = 1	'Left Flipper Lane 2
End Sub
Sub SW76_unHit
	Controller.Switch(76) = 0	'Left Flipper Lane 2
End Sub

Sub SW78_hit
	Controller.Switch(78) = 1	'Left Flipper Lane 1
End Sub
Sub SW78_unHit
	Controller.Switch(78) = 0	'Left Flipper Lane 1
End Sub

Sub SW86_Hit
	vpmTimer.PulseSw 86			'Grave "A"
End Sub

'Flasher prim. image changes
       Sub Prim3_Timer()
           Select Case PrimCase3
               Case 1:ChairPrim2.Image = "TAFChairMappingB2":FesterPrim1.Image = "echairNewB2":PrimCase3 = 2
               Case 2:ChairPrim2.Image = "TAFChairMappingA2":FesterPrim1.Image = "echairNewB2":PrimCase3 = 3
               Case 3:ChairPrim2.Image = "TAFChairMappingON2":FesterPrim1.Image = "echairNewON2":Me.Enabled = 0
               Case 4:ChairPrim2.Image = "TAFChairMappingA2":FesterPrim1.Image = "echairNewA2":PrimCase3 = 5
               Case 5:ChairPrim2.Image = "TAFChairMappingB2":FesterPrim1.Image = "echairNewB2":PrimCase3 = 6
               Case 6:ChairPrim2.Image = "TAFChairMappingOFF2":FesterPrim1.Image = "echairNewOFF2":Me.Enabled = 0
           End Select
       End Sub

       Sub Prim7_Timer()
           Select Case PrimCase7
               Case 1:ChairScoop.image = "ScoopMapB": ChairPrim.Image = "TAFChairMappingB":FesterPrim.Image = "eChairNewB":PrimCase7 = 2
               Case 2:ChairScoop.image = "ScoopMapA": ChairPrim.Image = "TAFChairMappingA":FesterPrim.Image = "eChairNewA":PrimCase7 = 3
               Case 3:ChairScoop.image = "ScoopMapON": ChairPrim.Image = "TAFChairMappingON":FesterPrim.Image = "eChairNewON":Me.Enabled = 0
               Case 4:ChairScoop.image = "ScoopMapA": ChairPrim.Image = "TAFChairMappingA":FesterPrim.Image = "eChairNewA":PrimCase7 = 5
               Case 5:ChairScoop.image = "ScoopMapB": ChairPrim.Image = "TAFChairMappingB":FesterPrim.Image = "eChairNewB":PrimCase7 = 6
               Case 6:ChairScoop.image = "ScoopMap":ChairPrim.Image = "TAFChairMappingOFF":FesterPrim.Image = "eChairNew":Me.Enabled = 0
           End Select
       End Sub

Set LampCallback = GetRef("Lamps")
	Sub Lamps
		L11.State = Controller.Lamp(11)
		L12.State = Controller.Lamp(12)
		L13.State = Controller.Lamp(13)
		L14.State = Controller.Lamp(14)
		L15.State = Controller.Lamp(15)
		L16.State = Controller.Lamp(16)
		L17.State = Controller.Lamp(17)
		L18.State = Controller.Lamp(18)
'		L19.State = Controller.Lamp(19)
'		L20.State = Controller.Lamp(20)
		L21.State = Controller.Lamp(21)
L21a.State = Controller.Lamp(21)
		L22.State = Controller.Lamp(22)
L22a.State = Controller.Lamp(22)
		L23.State = Controller.Lamp(23)
L23a.State = Controller.Lamp(23)
		L24.State = Controller.Lamp(24)
L24a.State = Controller.Lamp(24)
		L25.State = Controller.Lamp(25)
L25a.State = Controller.Lamp(25)
		L26.State = Controller.Lamp(26)
		L27.State = Controller.Lamp(27)
		L28.State = Controller.Lamp(28)
'		L29.State = Controller.Lamp(29)
'		L30.State = Controller.Lamp(30)
		L31.State = Controller.Lamp(31)
		L32.State = Controller.Lamp(32)
		L33.State = Controller.Lamp(33)
		L34.State = Controller.Lamp(34)
		L35.State = Controller.Lamp(35)
		L36.State = Controller.Lamp(36)
		L37.State = Controller.Lamp(37)
		L38.State = Controller.Lamp(38)
'		L39.State = Controller.Lamp(39)
'		L40.State = Controller.Lamp(40)
'		L41.State = Controller.Lamp(41)
		L42.State = Controller.Lamp(42)
		L43.State = Controller.Lamp(43)
		L44.State = Controller.Lamp(44)
		L45.State = Controller.Lamp(45)
		L46.State = Controller.Lamp(46)
		L47.State = Controller.Lamp(47)
		L47b.State = Controller.Lamp(47)
		L48.State = Controller.Lamp(48)
'		L49.State = Controller.Lamp(49)
'		L50.State = Controller.Lamp(50)
		L51.State = Controller.Lamp(51)
		L52.State = Controller.Lamp(52)
		L53.State = Controller.Lamp(53)
		L54.State = Controller.Lamp(54)
		L55.State = Controller.Lamp(55)
		L56.State = Controller.Lamp(56)
		L57.State = Controller.Lamp(57)
		L58.State = Controller.Lamp(58)
'		L59.State = Controller.Lamp(59)
'		L60.State = Controller.Lamp(60)
		L61.State = Controller.Lamp(61)
		L62.State = Controller.Lamp(62)
		L63.State = Controller.Lamp(63)
		L64.State = Controller.Lamp(64)
		L64b.State = Controller.Lamp(64)
		L65.State = Controller.Lamp(65)
		L66.State = Controller.Lamp(66)
		L67.State = Controller.Lamp(67)
		L68.State = Controller.Lamp(68)
'		L69.State = Controller.Lamp(69)
'		L70.State = Controller.Lamp(70)
		L71.State = Controller.Lamp(71)
		L72.State = Controller.Lamp(72)
		L73.State = Controller.Lamp(73)
		L74.State = Controller.Lamp(74)
		L74b.State = Controller.Lamp(74)
		L75.State = Controller.Lamp(75)
		L75b.State = Controller.Lamp(75)
	'	L76.State = Controller.Lamp(76)
		L77.State = Controller.Lamp(77)
		L77b.State = Controller.Lamp(77)
		L78.State = Controller.Lamp(78)
		L78b.State = Controller.Lamp(78)
'		L79.State = Controller.Lamp(79)
'		L80.State = Controller.Lamp(80)
		L81.State = Controller.Lamp(81)
		L82.State = Controller.Lamp(82)
		L83.State = Controller.Lamp(83)
		L84.State = Controller.Lamp(84)
		L85.State = Controller.Lamp(85)
		L86.State = Controller.Lamp(86)
		L87.State = Controller.Lamp(87)
		L81F.visible = L81.state
		L82F.visible = L82.state
		L83F.visible = L83.state
		L84F.visible = L84.state
		L85F.visible = L85.state
		L86F.visible = L86.state
		L87F.visible = L87.state
if l74.state = True then L74Prim.blenddisablelighting = 10 
if l74.state = False then L74Prim.blenddisablelighting = 0
if l75.state = True then l75prim.blenddisablelighting = 10
if l75.state = False then l75prim.blenddisablelighting = 0
if l77.state = True then L74Prim2.blenddisablelighting = 10 
if l77.state = False then L74Prim2.blenddisablelighting = 0
if l78.state = True then l74prim1.blenddisablelighting = 10
if l78.state = False then l74prim1.blenddisablelighting = 0
End Sub


'GI Strings
Sub UpdateGI(giNo, status)
Dim ii
   Select Case giNo
      Case 0  'Left String
		GI1.State = Abs(status)
		Intensity GI1.State, GI1
		GI2.State = Abs(status)
		Intensity GI2.State, GI2
		GI3.State = Abs(status)
		Intensity GI3.State, GI3
		GI4.State = Abs(status)
		Intensity GI4.State, GI4
'		GI10.State = Abs(status)
'		Intensity GI10.State, GI10
		GI11.State = Abs(status)
		Intensity GI11.State, GI11
		GI12.State = Abs(status)
		Intensity GI12.State, GI12
'		GI13.State = Abs(status)
'		Intensity GI13.State, GI13
		GI14.State = Abs(status)
		Intensity GI14.State, GI14
		GI15.State = Abs(status)
		Intensity GI15.State, GI15
		GI28.State = Abs(status)
		Intensity GI28.State, GI28
		GI17.State = Abs(status)
		Intensity GI17.State, GI17
		GI18.State = Abs(status)
		Intensity GI18.State, GI18
		GI19.State = Abs(status)
		Intensity GI19.State, GI19
		GI20.State = Abs(status)
		Intensity GI20.State, GI20
		GI21.State = Abs(status)
		Intensity GI21.State, GI21
		GI22.State = Abs(status)
		Intensity GI22.State, GI22
		GI24.State = Abs(status)
		Intensity GI24.State, GI24
		GI25.State = Abs(status)
		Intensity GI25.State, GI25
		GI27.State = Abs(status)
		Intensity GI27.State, GI27
		GI26.State = Abs(status)
		Intensity GI26.State, GI26
		GI30.State = Abs(status)
		Intensity GI30.State, GI30
		GI35.State = Abs(status)
		Intensity GI35.State, GI35
		GI10.State = Abs(status)		'Brighter
		Intensity2 GI10.State, GI10
		GI13.State = Abs(status)		'Brighter
		Intensity2 GI13.State, GI13
		GI38.State = Abs(status)		'Brighter
		Intensity2 GI38.State, GI38
		GI_Light1.State = Abs(status)
		Intensity GI_Light1.State, GI_Light1
		GI_Light.State = Abs(status)
		Intensity GI_Light.State, GI_Light
		LLightning1.State = Abs(status)
		Intensity LLightning1.State, LLightning1     'Chair Light Mod
 '     Case 1   'Insert House String
 '     Case 2   'Insert People String
      Case 4    'Right String
		GI5.State = Abs(status)
		Intensity GI5.State, GI5
		GI6.State = Abs(status)
		Intensity GI6.State, GI6
		GI7.State = Abs(status)
		Intensity GI7.State, GI7
		GI8.State = Abs(status)
		Intensity GI8.State, GI8
		GI9.State = Abs(status)
		Intensity GI9.State, GI9
		GI16.State = Abs(status)
		Intensity GI16.State, GI16
		GI23.State = Abs(status)
		Intensity GI23.State, GI23
		GI32.State = Abs(status)
		Intensity GI32.State, GI32
		GI29.State = Abs(status)
		Intensity GI29.State, GI29
		GI33.State = Abs(status)
		Intensity GI33.State, GI33
		GI34.State = Abs(status)
		Intensity GI34.State, GI34
		GI36.State = Abs(status)
		Intensity GI36.State, GI36
		GI37.State = Abs(status)		'Brighter
		Intensity2 GI37.State, GI37
		GI39.State = Abs(status)		'Brighter
		Intensity2 GI39.State, GI39
		GI40.State = Abs(status)		'Brighter
		Intensity2 GI40.State, GI40
		GI41.State = Abs(status)		'Brighter
		Intensity2 GI41.State, GI41
		GI42.State = Abs(status)		'Brighter
		Intensity2 GI42.State, GI42
		GI_Light2.State = Abs(status)
		Intensity GI_Light2.State, GI_Light2
		GI_Light3.State = Abs(status)
		Intensity GI_Light3.State, GI_Light3
		SwampLight.State = Abs(status)
		Intensity SwampLight.State, SwampLight			'Swamp Light Mod
		VaultLight.State = Abs(Status)
		Intensity VaultLight.State, VaultLight			'Vault Light Mod
	End Select

End Sub

Dim GILevel

	If DayNight <= 20 Then
			GILevel = 5
	ElseIf DayNight <= 40 Then
			GILevel = 4.16
	ElseIf DayNight <= 60 Then
			GILevel = 3.25
	ElseIf DayNight <= 80 Then
			GILevel = 2.4
	Elseif DayNight <= 100  Then
			GILevel = 1.5
	End If

Sub Intensity(nr, Object)
		If nr = 2 Then
			object.State = 1
		End If
		Object.Intensity = nr * GILevel
End Sub

Sub Intensity2(nr, Object)
		If nr = 2 Then
			object.State = 1
		End If
		Object.Intensity = nr * 4
End Sub

sub FlasherTimer_Timer()
		FesterLightR.visible = L47.State
		FesterLightY.visible = L64.State
		BearRugLight1.State = L63.State
		BearRugLight2.State = L63.State
		BearHeadLight1.State = L63.State
		BearHeadLight2.State = L63.State
		CousinIttLight.State = L26.State
FesterLightR.blenddisablelighting = 30
FesterLightY.blenddisablelighting = 30
End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("RightSlingshot",DOFContactors), sling1
    RSling.Visible = 0
    RSling1.Visible = 1
	sling1.TransZ = -30
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	vpmTimer.pulsesw 36					'RightSlingShot
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -15
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("LeftSlingshot",DOFContactors), sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -30
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	vpmTimer.pulsesw 37					'LeftSlingShot
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -15
		Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


'REGISTERED LOCATIONS ***************************************************************************************************************************************

 Const optOpenAtStart	= 1
 Const optBear			= 6
 Const optFester		= 8
 Const optBox			= 3088
 Const optCousinItt		= 32
 Const optBooks			= 64
 Const optBetas			= 128
 Const optSwLight		= 256
 Const optCVLight		= 512

'OPTIONS MENU *********************************************************************************************************************************************

 Dim TableOptions, TableOptions2, TableName
 Private vpmShowDips1, vpmDips1, vpmDips2

 Sub InitializeOptions
	TableName="TAF_VPX"	'Replace with your descriptive table name, it will be used to save settings in VPReg.stg file
	Set vpmShowDips1 = vpmShowDips								'Reassigns vpmShowDips to vpmShowDips1 to allow usage of default dips menu
	Set vpmShowDips = GetRef("TableShowDips")					'Assigns new sub to vmpShowDips
 	TableOptions = LoadValue(TableName,"Options")				'Load saved table options
' 	TableOptions2 = LoadValue(TableName,"Options2")				'Load saved table options
	Set Controller = CreateObject("VPinMAME.Controller")		'Load vpm controller temporarily so options menu can be loaded if needed
'	If TableOptions2 = "" Then TableOptions2 = 0
	If TableOptions = "" Or optReset Then						'If no existing options, reset to default through optReset, then open Options menu
		TableOptions = DefaultOptions							'clear any existing settings and set table options to default options
		TableShowOptions
	ElseIf (TableOptions And optOpenAtStart) Then				'If Enable Next Start was selected then
		TableOptions = TableOptions - optOpenAtStart			'clear setting to avoid future executions
		TableShowOptions
	Else
		TableSetOptions
	End If
'	TableSetOptions2
	Set Controller = Nothing									'Unload vpm controller so selected controller can be loaded
 End Sub

 Private Sub TableShowDips
	vpmShowDips1												'Show original Dips menu
	TableShowOptions											'Show new options menu
	'TableShowOptions2											'Add more options menus...
 End Sub

 Private Sub TableShowOptions					'New options menu, additional menus can be added as well, just follow similar format and add call to TableShowDips
   Dim oldOptions : oldOptions = TableOptions
	If Not IsObject(vpmDips1) Then				'If creating an additional menus, need to declare additional vpmDips variables above (ex. vpmDips2 and TableOptions2, etc.)
		Set vpmDips1 = New cvpmDips
		With vpmDips1
			.AddForm 700, 500, "TABLE OPTIONS MENU"
			.AddFrameExtra 0,0,155,"Bear Toy",optBear, Array("Rug", 0*2, "Head", 1*2, "None", 2*2)
			.AddFrameExtra 0,65,155,"Thing Box",optBox, Array("Black Vinyl", 1*16, "Red Leather - Dark", 1*1024, "Red Leather", 2*1024, "Default", 0*16)
			.AddFrameExtra 0,140,155,"Chair Options",optFester, Array("with Uncle Fester", 0, "No Fester", 8)
			.AddFrameExtra 0,185,155,"Swamp Lighting", optSwLight, Array("Enable Green Light", 0, "Disabled", 256)
			.AddFrameExtra 175,0,155,"Coustin Itt",optCousinItt, Array("Enabled", 0, "Disabled", 32)
			.AddFrameExtra 175,65,155,"Bookshelf",optBooks, Array("Bookcase Mod", 0, "Default", 64)
			.AddFrameExtra 175,120,155,"Beta Plastics", optBetas, Array("Show Beta Plastics", 0, "No Beta Plastics", 128)
			.AddFrameExtra 175,185,155,"Chair/Vault Lighting", optCVLight, Array("Enabled", 0, "Disabled", 512)
			.AddLabel 0,240,175,30,"* Restart To Apply Settings"
			.AddChkExtra 180,240,135, Array("Enable Menu Next Start", optOpenAtStart)
		End With
	End If
	TableOptions = vpmDips1.ViewDipsExtra(TableOptions)
	SaveValue TableName,"Options",TableOptions
	TableSetOptions
 End Sub

 Dim Bear, Fester, Box, CousinItt, Books, Betas, CVLight, SwLight



 Sub TableSetOptions		'defines required settings before table is run
	Bear = (TableOptions And optBear)
	Fester = (TableOptions And optFester)
	Box = (TableOptions And optBox)
	CousinItt = (TableOptions And optCousinItt)
	Books = (TableOptions And optBooks)
	Betas = (TableOptions And optBetas)
	CVLight = (TableOptions And optCVLight)
	SwLight = (TableOptions And optSwLight)
	SaveValue TableName,"Options",TableOptions
 End Sub

	If Bear = 2 Then
		BearRug.Visible = 0
		BearRugLight1.Visible = 0
		BearRugLight2.Visible = 0
		BearRugEyes.Visible = 0
		BearRugEyes.sideVisible = 0


		BearHead.Visible = 1
		BearHeadLight1.Visible = 1
		BearHeadLight2.Visible = 1
		BearHeadEyes.Visible = 1
		BearHeadEyes.sideVisible = 1
	End If
	If Bear = 4 Then
		BearRug.Visible = 0
		BearRugLight1.Visible = 0
		BearRugLight2.Visible = 0
		BearRugEyes.Visible = 0
		BearRugEyes.sideVisible = 0

		BearHead.Visible = 0
		BearHeadLight1.Visible = 0
		BearHeadLight2.Visible = 0
		BearHeadEyes.Visible = 0
		BearHeadEyes.sideVisible = 0
	End If
	If Bear = 0 Then
		BearRug.Visible = 1
		BearRugLight1.Visible = 1
		BearRugLight2.Visible = 1
		BearRugEyes.Visible = 1
		BearRugEyes.sideVisible = 1

		BearHead.Visible = 0
		BearHeadLight1.Visible = 0
		BearHeadLight2.Visible = 0
		BearHeadEyes.Visible = 0
		BearHeadEyes.sideVisible = 0
	End If

	If Fester = 8 Then
		FesterPrim.Visible = 0
		FesterPrim1.Visible = 0
		FesterBulb.Visible = 0
		FesterLightR.z = -20
		FesterLightY.z = -20
	Else
		FesterPrim.Visible = 1
		FesterPrim1.Visible = 1
		FesterBulb.Visible = 1
'		FesterLightR.Visible = 1
'		FesterLightY.Visible = 1
	End If

	If Box = 0 Then
		ThingBox.image = "thingboxMod3"
		ThingBox.material = "Plastic"
	Elseif Box = 16 Then
		ThingBox.image = "thingboxMod1"
		ThingBox.material = "Plastic"
	Elseif Box = 1024 Then
		ThingBox.image = "thingboxMod2"
		ThingBox.material = "Plastic"
	Elseif Box = 2048 Then
		ThingBox.image = "thingboxMod3"
		ThingBox.material = "Plastic"
	End If

	If CousinItt = 32 Then
		CousinIttPrim.Visible = 0
		CousinIttLight.Visible = 0
	Else
		CousinIttPrim.Visible = 1
		CousinIttLight.Visible = 1
	End If

	If Books = 64 Then
		Bookcase.image = "Bookcase1"
	Else
		Bookcase.image = "Bookcase1-mod"
	End If

	If Betas = 128 Then
		Arch.Visible = 0
		Beta1.Visible = 0
		Beta1.sideVisible = 0
		Beta1_1.Visible = 0
		Beta1_1.sideVisible = 0
		Beta1_2.Visible = 0
		Beta2.Visible = 0
		Beta2.sideVisible = 0
		Beta2_1.Visible = 0
		Beta2_1.sideVisible = 0
		Beta2_2.Visible = 0
		Beta3.Visible = 0
		Beta3.sideVisible = 0
		Beta4.Visible = 0
		Beta4.sideVisible = 0
		Beta4_1.Visible = 0
		Beta4_1.sideVisible = 0
		Beta4_2.Visible = 0
	Else
		Arch.Visible = 1
		Beta1.Visible = 1
		Beta1.sideVisible = 1
		Beta1_1.Visible = 1
		Beta1_1.sideVisible = 1
		Beta1_2.Visible = 1
		Beta2.Visible = 1
		Beta2.sideVisible = 1
		Beta2_1.Visible = 1
		Beta2_1.sideVisible = 1
		Beta2_2.Visible = 1
		Beta3.Visible = 1
		Beta3.sideVisible = 1
		Beta4.Visible = 1
		Beta4.sideVisible = 1
		Beta4_1.Visible = 1
		Beta4_1.sideVisible = 1
		Beta4_2.Visible = 1
	End If

	If CVLight = 512 Then
		VaultLight.Visible = 0
		LLightning1.Visible = 0
	Else
		VaultLight.Visible = 1
		LLightning1.Visible = 1
	End If


	If SwLight = 256 Then
		SwampLight.Visible = 0
	Else
		SwampLight.Visible = 1
	End If



'******ThingTrainer******
Dim Shot
Shot = 0
If ThingTrainer = 1 Then
'	Thing1.enabled = 1
	Thing2.enabled = 1
	Thing3.enabled = 1
	TrainOn.Visible = 1
	Thing4.Collidable = true
	Thing5.Collidable = true
End If

Sub Thing3_hit
	Shot = Shot +1
	If Shot < 20 Then
	Thing3.destroyball
	Thing1.createball
	Thing1.kick 180, 1
	Else
	ThingTrainer = 0
	Thing3.kick 180, 1
	TrainOn.Visible = 0
	End If
End Sub

Sub Thing2_Hit
	Thing2.Kick 10, 60
End Sub


Sub Table1_Exit()
	If B2SOn Then Controller.Stop
End Sub

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
' PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
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

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioFade(ball) ' Can this be together with the above function ?
  Dim tmp
  tmp = ball.y * 2 / Table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'***********************************
'   JP's VP10 Rolling Sounds v4.0
'   JP's Ball Shadows
'   JP's Ball Speed Control
'   Rothbauer's dropping sounds
'***********************************

Const tnob = 19   'total number of balls
Const lob = 2     'number of locked balls
Const maxvel = 45 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate() 'call this routine from any realtime timer you may have, running at an interval of 10 is good.

    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 2100 'under the apron 'may differ from table to table
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' draw the ball shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z - Ballsize / 2 + 1

    'play the rolling sound for each ball
        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 5
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory < 1 Then
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
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*VolRPi, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound SoundFX("target",DOFTargets), 0, Vol(ActiveBall)*VolTarg, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall)*VolMetals, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall)*VolMetals, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall)*VolMetals, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall)*VolGates, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySoundAtVol "fx_spinner", Spinner, VolSpin
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
		PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolRPo, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, (Vol(ActiveBall)*4)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, (Vol(ActiveBall)*4)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, (Vol(ActiveBall)*4)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, (Vol(ActiveBall)*4)*VolFlip, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, (Vol(ActiveBall)*4)*VolFlip, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, (Vol(ActiveBall)*4)*VolFlip, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub Trigger1_hit
	PlaysoundAt "ThingRampHit", ActiveBall
End Sub

Sub SwampEnter_hit
	Playsound "SwampHit", 0, Vol(ActiveBall)*3, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub VaultTrigger_hit
	Playsound "VaultHit", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

' Thalamus : Exit in a clean and proper way
Sub Table1_exit()
  Controller.Pause = False
  Controller.Stop
End Sub