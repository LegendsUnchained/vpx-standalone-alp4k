'############################################################################################
'#######                                                                             ########
'#######          Popeye Saves The Earth                                             ########
'#######          (Bally 1994)                                                       ########
'#######                                                                             ########
'############################################################################################

'***************************************************
'* VP9 - mfuegemann 2014     
'* VPX - JPJ Team PP 
'* VR Room - Psiomicron
'*
'* VPX 10.7x - TastyWasps - Sept. 2023
'*
'* nFozzy Physics, Fleep Sounds, TargetBouncer, VR Room
'* Correct Geometry for Table Sizing, Lighting Tweaks
'* Dynamic Ball Shadows, Multi-Ball Cradle Physics
'* Script Organization, Graphic Improvements 
'* Dimmer Level (LUT) Selector, Gameplay Balance 
'* Playfield Element Upscales - MovieGuru
'* VR Upscales & Assets - Hauntfreaks, DGrimmReaper & Psiomicron
'* Incandescent Lighting Option - Hauntfreaks
'* Testing - VPW Team
'****************************************************

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const BallSize = 54

'*************************************************************
' VR Room Auto-Detect
'*************************************************************
Dim VR_Obj, UseVPMDMD

Dim DesktopMode: DesktopMode = Table1.ShowDT

If RenderingMode = 2 Then
	Pincab_Rails.Visible = True
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 1 : Next
	For Each VR_Obj in VRRoom : VR_Obj.Visible = 1 : Next
	UseVPMDMD = True
Else
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRRoom : VR_Obj.Visible = 0 : Next
	UseVPMDMD = DesktopMode
End If

If Not DesktopMode Then 
	Sides.visible = True ' Put True or False if you want to see or not the wood side in fullscreen mod
Else
	Pincab_Rails.Visible = True
End If 

LoadVPM "01560000","WPC.VBS",3.1

'*************************************************************
' TABLE OPTIONS
'*************************************************************
' Table Dimmer is set by using the Left and Right Magna keys.
' Black & White desktop background available in "Table > Image Manager" under "desktop_bw_upscaled".

'----- LED Lighting -----
Const LEDLighting = 1             ' 1 = LED lighting for brighter, cooler colors - 2 = Incandescent lighting for warmer colors

'----- Black & White MOD ----- 
Const BlackAndWhite = 0             ' 0 - Regular Color Popeye, 1 = Black & White Old School Popeye
									' Note: Changing Dimmer Levels moves back to color if Black & White enabled.

'----- Target Bouncer Levels -----
Const TargetBouncerEnabled = 1	  ' 0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7	  ' Level of bounces. Recommmended value of 0.7

'----- General Sound Options -----
Const VolumeDial = 0.8			  ' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.5		  ' Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5		  ' Level of ramp rolling volume. Value between 0 and 1

'----- Ball Shadow Options -----
Const DynamicBallShadowsOn = 1		' 0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		' 0 = Static shadow under ball ("flasher" image, like JP's)
'									' 1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									' 2 = flasher image shadow, but it moves like ninuzzu's

'------  Global Variables ------
Dim tablewidth:tablewidth = Table1.width
Dim tableheight:tableheight = Table1.height

Const tnob = 6
Const lob = 0

Const cgamename = "pop_lx5"
Const UseSolenoids = 2
Const UseLamps = 1
Const UseSync = 1
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""
Const keyBuyInButton = 3

Dim gilvl:gilvl = 1	

Dim bsTrough,bsEscalatorPopper,bsRightPopper,bsLeftPopper,bsLockup,bsLockKickout,mWheelMotor, shadows, onUpperPF

shadows = 1

ExtraKeyHelp = KeyName(keyBuyInButton) & vbTab & "Buy-In Button"

'-----------------------------------
'------  Solenoid Assignment  ------
'-----------------------------------
SolCallback(1)  = "bsRightPopper.SolOut"			'Right Popper
SolCallback(2)  = "bsLeftPopper.SolOut"				'Left Popper
SolCallback(3) = "AutoPlunger"						'Ball Shooter						'AutoPlunger
SolCallback(4)  = "SolAnimalDiverter"				'Animal Diverter
SolCallback(5)  = "bsTrough.SolOut"					'Trough Coil 
SolCallback(6)  = "bsLockKickout.SolOut"			'Lockup Kicker
SolCallback(7) = "Solknocker"						'Knocker
SolCallback(8)  = "bsEscalatorPopper.SolOut"		'Escalator Popper
'SolCallback(9)  = "vpmSolSound ""Bumper"","			'Left Jet'
'SolCallback(10) = "vpmSolSound ""Bumper"","			'Right Jet
'SolCallback(11) = "vpmSolSound ""Bumper"","			'Center Jet 
'SolCallback(12) = "vpmSolSound ""lSling"","			'Left Slingshot 
'SolCallback(13) = "vpmSolSound ""rSling"","			'Right Slingshot
SolCallback(14)	= "LeftGate.open = "				'Left Gate
SolCallback(15)	= "RightGate.open = "				    'Right Gate
SolCallback(16) = "SolLockupRelease"				'Lockup Release
'SolCallback(17) = 'Wheel Motor - handled elsewhere
SolCallback(18) = "SolLeftPopperArrowFlasher"		'Upper Playfield left - Flasher LeftPopperArrow
SolCallback(19) = "SolRightLoopFlasher"				'Right Loop Backbox
SolCallback(20) = "SolRightPopperArrowFlasher"		'Fight Bluto - Flasher RightPopperArrow
SolCallback(21) = "SolLeftLoopFlasher"				'Left Loop Backbox
SolCallback(22) = "SolAnimalRampFlasher"			'Animal Ramp - Flasher - Flupper #1
SolCallback(23) = "SolSkillWheelFlasher"			'Skill Wheel - Flasher
SolCallback(24) = "SolRPopperEBFlasher"				'R. Popper Backbox EB
'SolCallback(25) = 'not used
SolCallback(26) = "SolRampJackpotFlasher"			'Ramp Jackpot - Flasher
SolCallback(27) = "SolLockjawArrowFlasher"			'Lockjaw Arrow - Flasher CenterBlutoArrow
SolCallback(28) = "SolEscalatorFlasher"				'Escalator Backbox Turtle - Flasher

Sub AutoPlunger(Enabled)
    If Enabled Then
       Plunger.Fire
       SoundPlungerReleaseBall()
	End If
End Sub

' Lower Flippers
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sLRFlipper) = "SolRFlipper"

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire
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

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire
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

' Upper Flippers - Sound routines are different to give a smaller sound from the top tiny flippers.
SolCallback(sULFlipper) = "SolULFlipper"
SolCallback(sURFlipper) = "SolURFlipper"

Sub SolULFlipper(Enabled)
	If onUpperPF = True Then
		If Enabled Then
			PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, 1, -0.1, 0.25
			UpperLeftFlipper.RotateToEnd
		Else
			PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, -0.1, 0.25
			UpperLeftFlipper.RotateToStart
		End If
	End If
End Sub

Sub SolURFlipper(Enabled)
	If onUpperPF = True Then
		If Enabled Then
			PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, 1, 0.1, 0.25
			UpperRightFlipper.RotateToEnd
		Else
			PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, 0.1, 0.25
			UpperRightFlipper.RotateToStart
		End If
	End If
End Sub

' Knocker
Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid
	End If
End Sub

Set GICallBack = GetRef("UpdateGI")

dim ttcentre
Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5
FlasherLight1.IntensityScale = 0
Flasher24.IntensityScale = 0
Flasher22b.IntensityScale = 0
Flasher26.IntensityScale = 0
Flasher1.IntensityScale = 0
Flasher28.IntensityScale = 0

RainbowTimer.enabled = 1

If LEDLighting = 1 Then
	LEDLightingOn
End If

Sub Table1_Init

	realtime.enabled = 1
	Primitive148.image = "blue0"
	Primitive149.image = "red01"
	Primitive153.image = "jaune0"
	Primitive159.image = "blueh0"
	Flasher18.state = 0
	Flasher18b.state = 0
	Flasher20.state = 0
	Flasher20b.state = 0

'	vpminit me

	With Controller
		.GameName=cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description : Exit Sub
		Controller.SplashInfoLine="Popeye Saves the Earth" & vbNewLine & "Created by mfuegemann - Remastered by Team JPJ, TastyWasps"
		.HandleMechanics = 0
		.HandleKeyboard = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.ShowTitle = 0
	End With
	Controller.Hidden = 0
	On Error Resume Next
  
	Controller.Run

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled = true

	vpmNudge.TiltSwitch = 14                  
	vpmNudge.Sensitivity = 4
	vpmNudge.TiltObj = Array(Bumper16, Bumper17, Bumper18, LeftSlingshot, RightSlingshot)

	vpmMapLights AllLights
	vpmMapLights AllLightsz                  

	' Disc
 	Set ttcentre = new cvpmTurnTable
	ttcentre.InitTurnTable trMixMaster,240
	ttcentre.CreateEvents "ttcentre"
	ttcentre.SpinUp = 0
	ttcentre.SpinDown = 0
	ttcentre.Speed = 5
	ttcentre.MaxSpeed = 5

    Set bsEscalatorPopper=New cvpmBallStack    
   		bsEscalatorPopper.InitSw 0,36,0,0,0,0,0,0
        bsEscalatorPopper.InitKick EscalatorPopper,0,45
        bsEscalatorPopper.InitExitSnd SoundFX("popper",DOFContactors),SoundFX("solon",DOFContactors)
		bsEscalatorPopper.Balls=0

	Set bsRightPopper=New cvpmBallStack    
		bsRightPopper.InitSw 0,32,0,0,0,0,0,0
		bsRightPopper.InitKick RightPopper,135,2
		bsRightPopper.InitExitSnd SoundFX("popper",DOFContactors),SoundFX("solon",DOFContactors)
		bsRightPopper.Balls=0

	Set bsLeftPopper=New cvpmBallStack       
		bsLeftPopper.InitSw 0,31,0,0,0,0,0,0
		bsLeftPopper.InitKick LeftPopper,215,2
		bsLeftPopper.InitExitSnd SoundFX("popper",DOFContactors),SoundFX("solon",DOFContactors)
		bsLeftPopper.Balls=0

	Set bsLockup=New cvpmBallStack       
		bsLockup.InitSw 0,45,44,43,0,0,0,0
		bsLockup.Balls=0

	Set bsLockKickout=New cvpmBallStack       
		bsLockKickout.InitSw 0,87,0,0,0,0,0,0
		bsLockKickout.InitKick Lockup,164,6
		bsLockKickout.InitExitSnd SoundFX("popper",DOFContactors),SoundFX("solon",DOFContactors)
		bsLockKickout.Balls=0

	Set bsTrough=New cvpmBallStack
		bsTrough.InitSw 0,51,52,53,54,55,56,0            	
		bsTrough.InitKick BallRelease,90,5               	
		bsTrough.InitExitSnd SoundFX("BallRel",DOFContactors),SoundFX("Solenoid",DOFContactors)
		bsTrough.Balls=6

	Set mWheelMotor=New cvpmMech
		mWheelMotor.Mtype = vpmMechOneSol + vpmMechCircle + vpmMechLinear
		mWheelMotor.Sol1 = 17
		mWheelMotor.length = 200
		mWheelMotor.Steps = 60
		mWheelMotor.Callback = GetRef("UpdateWheel")
		mWheelMotor.start

	Controller.Switch(22) = True 						' Coin Door closed
	Controller.Switch(24) = True 						' Always closed
	Controller.Switch(57) = False
	Controller.Switch(64) = False						' Animals start Off
	Controller.Switch(65) = False
	Controller.Switch(66) = False
	Controller.Switch(67) = False
	Controller.Switch(68) = False

	Plunger.PullBack
	AnimalDiverter.isdropped = True
	AnimalPush.isdropped = True

	LoadLut

	If BlackAndWhite = 1 Then
		Table1.ColorGradeImage = "LUT_BW"
	End If

End Sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_exit():Controller.Pause = False:Controller.Stop:End Sub

Sub GameTimer_Timer
	RollingUpdate
	UpperPlayfieldScan  ' Checks for Z value for upper PF flippers
End Sub

Sub FrameTimer_Timer()
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate ' Update ball shadows
End Sub

Sub Drain_Hit()
	bsTrough.AddBall Me
	gi_alloff 1500
	RandomSoundDrain Drain
End Sub

Sub EscalatorPopper_Hit()
	bsEscalatorPopper.AddBall Me
	playsound "drain3" 
End Sub

Sub RightPopperHole_Hit()
	playsound "drain4" 
	Wait2.enabled = 1 
End Sub

Sub LeftPopperHole_Hit()
	playsound "drain4"
	Wait.enabled = 1 
End Sub

sub Wait_timer()
	bsLeftPopper.AddBall LeftPopperHole
	Me.enabled = 0
end sub

sub Wait2_timer()
	bsRightPopper.AddBall RightPopperHole
	Me.enabled = 0
end sub

Sub Lockup_Hit()
	bsLockup.AddBall Me
	playsound "drain3" 
End Sub

Sub Kicker81Up_Hit
	Kicker81Up.destroyball
	vpmtimer.pulsesw 81
	Kicker81Ramp.createball
	Kicker81Ramp.kick 180,2
End Sub

dim u:u=0
Sub SolAnimalDiverter(enabled)
	If enabled Then
		Flipper1.RotateToEnd
		Flipper2.RotateToEnd
		Flipper3.RotateToEnd
		Flipper4.RotateToEnd
		Flipper5.RotateToEnd
		maillet.ObjRotY = 8
	Else
		Flipper1.RotateToStart
		Flipper2.RotateToStart
		Flipper3.RotateToStart
		Flipper4.RotateToStart
		Flipper5.RotateToStart
		diverter.enabled=1
'		maillet.ObjRotY = 0:div=0:u=1
	End If
End Sub  
 
dim div:div = 0
Sub diverter_timer()
	select case div
		case 0:maillet.ObjRotY = 7
		case 1:maillet.ObjRotY = 6
		case 2:maillet.ObjRotY = 4
		case 3:maillet.ObjRotY = 2
		case 4:maillet.ObjRotY = 0
	end Select
	div=div+1
	if div = 5 then div=0:u=0:me.enabled = 0
End Sub
   
dim ani:ani = 0
Sub SolLockupRelease(enabled)
	If enabled Then
		If bsLockup.Balls > 0 Then
			bsLockup.exitsol_on
			bsLockKickout.addball 0
			ani = 1
			animation.enabled = 1
		End If
	End If
End Sub 

Sub JunctionHelperStart_Hit
	Ramp4.Collidable = False
End Sub

Sub JunctionHelperEnd_Hit
	Ramp4.Collidable = True
End Sub

Sub TriggerBluto_hit()
	animationIn.enabled = 1
end sub

dim aaa:aaa=0
Sub animation_Timer
    Select Case aaa
        Case 0:Ramp28.image = "anim03"
        Case 1:Ramp28.image = "anim02"
        Case 2:Ramp28.image = "anim00"
        Case 3:Ramp28.image = "anim02"
        Case 4:Ramp28.image = "anim01"
    End Select
    aaa = aaa + 1
	ani = 0
	if aaa = 5 then aaa=0:animation.enabled = 0
End Sub

dim aaaa:aaaa=0
Sub animationIn_Timer
    Select Case aaaa
        Case 0:Ramp28.image = "anim00"
        Case 1:Ramp28.image = "anim02"
        Case 2:Ramp28.image = "anim00"
        Case 3:Ramp28.image = "anim02"
        Case 4:Ramp28.image = "anim03"
        Case 5:Ramp28.image = "anim04"
        Case 6:Ramp28.image = "anim01"
    End Select
    aaaa = aaaa + 1
	ani = 1
	if aaaa = 7 then aaaa=0:animationIn.enabled = 0
End Sub

'------------------
'------  GI  ------
'------------------

dim obj
Sub UpdateGI(gino, status)
	Select Case gino
		case 0 :
			if status then 						
				for each obj in GIString1
					obj.state = 1
				next
				for each obj in GIString1Flasher
					obj.state = 1
				next
			else
				for each obj in GIString1
					obj.state = 0
				next
				for each obj in GIString1Flasher
					obj.state = 0
				next
			end if	
	End Select
End Sub

Sub GI_AllOff (time) ' Turn GI Off
    DOF 101, DOFOff
    'debug.print "GI OFF " & time
    'UpdateGI 0,0
    'RampsOff
    'FlippersOff
    dim xx
    for each xx in GILights
		xx.state = 0
    Next
	if shadows = 1 then PFShadows.image = "pf05offs":end if
	sides.image = "WallSidesOFF"
    If time > 0 Then
        GI_AllOnT.Interval = time
        GI_AllOnT.Enabled = 0
        GI_AllOnT.Enabled = 1
    End If
End Sub
 
Sub GI_AllOn 'Turn GI On
    DOF 101, DOFOn
    'UpdateGI 0,8
    'RampsOn
    'FlippersOn
    dim xx
    for each xx in GILights
    xx.state = 1
    Next
	if shadows = 1 then PFShadows.image = "pf05ons":end If
	sides.image = "WallSidesON"
End Sub
 
Sub GI_AllOnT_Timer 'Turn GI On timer
    'UpdateGI 0,8
    'RampsOn
    'FlippersOn
    dim xx
    for each xx in GILights
    xx.state = 1
    Next
	if shadows = 1 then PFShadows.image = "pf05ons":end If
	sides.image = "WallSidesON"
    GI_AllOnT.Enabled = 0
End Sub

'--------------------------------
'------  Flasher Handling  ------
'--------------------------------

Sub SolLeftPopperArrowFlasher(enabled)				'18 - Halo only
	if enabled then
		Flasher18.state = 1	
		Flasher18b.state = 1	
	else
		Flasher18.state = 0	
		Flasher18b.state = 0	
	end if
End Sub

Sub SolRightLoopFlasher(enabled)					'19 - Halo only
	if enabled then
		Flasher19.state = 1
	else
		Flasher19.state = 0
	end if
End Sub

Sub SolRightPopperArrowFlasher(enabled)				'20 - Halo only
	if enabled then
		Flasher20.state = 1	
		Flasher20b.state = 1	
	else
		Flasher20.state = 0	
		Flasher20b.state = 0			
	end if
End Sub

Sub SolLeftLoopFlasher(enabled)						'21 - Halo only
	if enabled then
		Flasher21.state = 1		
	else
		Flasher21.state = 0					
	end if
End Sub

Sub SolAnimalRampFlasher(flstate)					'22 - Flupper #1
	If flstate Then
		ObjTargetLevel(1) = 1
	Else
		ObjTargetLevel(1) = 0
	End If
	FlasherFlash1_Timer
End Sub

Sub SolSkillWheelFlasher(enabled)					'23 - Halo only
	if enabled then
		FlashLevel4 = .75 : FlasherFlash4_Timer
'		Flasher26.state = 1
		Flasherred.enabled = 1
		Flasher26f1.Amount = 40
		Flasher26f1.IntensityScale = 2	
		Flasher26f2.Amount = 20
		Flasher26f2.IntensityScale = 1		
	else
'		Flasher26.state = 0
		Flasher26f1.Amount = 0
		Flasher26f1.IntensityScale = 0	
		Flasher26f2.Amount = 0
		Flasher26f2.IntensityScale = 0					
	end if
End Sub

Sub SolRPopperEBFlasher(enabled)					'24 - Halo only, below cover
	if enabled then
'		Flasher24.state = 1
		FlashLevel3 = 1 : FlasherFlash3_Timer
		Flasherblueh.enabled = 1
		Flasher24f1.Amount = 60
		Flasher24f1.IntensityScale = 2	
		Flasher24f2.Amount = 40
		Flasher24f2.IntensityScale = 1	
	else
'		Flasher24.state = 0
		Flasher24f1.Amount = 0
		Flasher24f1.IntensityScale = 0	
		Flasher24f2.Amount = 0
		Flasher24f2.IntensityScale = 0	
	end if
End Sub

Sub SolRampJackpotFlasher(enabled)					'26 - Halo only, Ramp issue
	if enabled then
		FlashLevel4 = 1 : FlasherFlash4_Timer
'		Flasher26.state = 1
		Flasherred.enabled = 1
		Flasher26f1.Amount = 60
		Flasher26f1.IntensityScale = 2	
		Flasher26f2.Amount = 40
		Flasher26f2.IntensityScale = 1	
	else
'		Flasher26.state = 0
		Flasher26f1.Amount = 0
		Flasher26f1.IntensityScale = 0	
		Flasher26f2.Amount = 0
		Flasher26f2.IntensityScale = 0		
	end if
End Sub

Sub SolLockjawArrowFlasher(enabled)					'27 - Halo only
	if enabled then	
		Flasher27b.state = 1
		Flasher27b1.state = 1
		bluto.image = "Bluto_On"
	else		
		Flasher27b.state = 0
		Flasher27b1.state = 0
		bluto.image = "Bluto_Off"		
	end if
End Sub

Sub SolEscalatorFlasher(enabled)					'28
	if enabled then
		FlashLevel5 = 1 : FlasherFlash5_Timer
'		Flasher28.state = 1	
		Flasherjaune.enabled = 1
		Flasher28f1.Amount = 60
		Flasher28f1.IntensityScale = 2	
		Flasher28f2.Amount = 40
		Flasher28f2.IntensityScale = 1	
	else
'		Flasher28.state = 0
		Flasher28f1.Amount = 0
		Flasher28f1.IntensityScale = 0	
		Flasher28f2.Amount = 0
		Flasher28f2.IntensityScale = 0	
	end if
End Sub

'-------------------------------
'------  Wheel Animation  ------
'-------------------------------
Sub UpdateWheel(aNewPos, aSpeed, aLastPos)
	Primitive140.roty = aNewPos * 6
	Primitive141.roty = aNewPos * 6
	UpdateWBall
End Sub

Dim WheelPos
Sub UpdateWBall
	if kicker60.ballcntover <> 0 then kicker60.destroyball:kicker1.createball:exit sub:end if
	if kicker59.ballcntover <> 0 then kicker59.destroyball:kicker60.createball:exit sub:end if
	if kicker58.ballcntover <> 0 then kicker58.destroyball:kicker59.createball:exit sub:end if
	if kicker57.ballcntover <> 0 then kicker57.destroyball:kicker58.createball:exit sub:end if
	if kicker56.ballcntover <> 0 then kicker56.destroyball:kicker57.createball:exit sub:end if
	if kicker55.ballcntover <> 0 then kicker55.destroyball:kicker56.createball:exit sub:end if
	if kicker54.ballcntover <> 0 then kicker54.destroyball:kicker55.createball:exit sub:end if
	if kicker53.ballcntover <> 0 then kicker53.destroyball:kicker54.createball:exit sub:end if
	if kicker52.ballcntover <> 0 then kicker52.destroyball:kicker53.createball:exit sub:end if
	if kicker51.ballcntover <> 0 then kicker51.destroyball:kicker52.createball:exit sub:end if
	if kicker50.ballcntover <> 0 then kicker50.destroyball:kicker51.createball:exit sub:end if
	if kicker49.ballcntover <> 0 then kicker49.destroyball:kicker50.createball:exit sub:end if
	if kicker48.ballcntover <> 0 then kicker48.destroyball:kicker49.createball:exit sub:end if
	if kicker47.ballcntover <> 0 then kicker47.destroyball:kicker48.createball:exit sub:end if
	if kicker46.ballcntover <> 0 then kicker46.destroyball:kicker47.createball:exit sub:end if
	if kicker45.ballcntover <> 0 then kicker45.destroyball:kicker46.createball:exit sub:end if
	if kicker44.ballcntover <> 0 then kicker44.destroyball:kicker45.createball:exit sub:end if
	if kicker43.ballcntover <> 0 then kicker43.destroyball:kicker44.createball:exit sub:end if

	if kicker36.ballcntover <> 0 then kicker36.destroyball:kicker37.createball:exit sub:end if
	if kicker35.ballcntover <> 0 then kicker35.destroyball:kicker36.createball:exit sub:end if
	if kicker34.ballcntover <> 0 then kicker34.destroyball:kicker35.createball:exit sub:end if
	if kicker33.ballcntover <> 0 then kicker33.destroyball:kicker34.createball:exit sub:end if
	if kicker32.ballcntover <> 0 then kicker32.destroyball:kicker33.createball:exit sub:end if
	if kicker31.ballcntover <> 0 then kicker31.destroyball:kicker32.createball:exit sub:end if
	if kicker30.ballcntover <> 0 then kicker30.destroyball:kicker31.createball:exit sub:end if
	if kicker29.ballcntover <> 0 then kicker29.destroyball:kicker30.createball:exit sub:end if
	if kicker28.ballcntover <> 0 then kicker28.destroyball:kicker29.createball:exit sub:end if
	if kicker27.ballcntover <> 0 then kicker27.destroyball:kicker28.createball:exit sub:end if
	if kicker26.ballcntover <> 0 then kicker26.destroyball:kicker27.createball:exit sub:end if
	if kicker25.ballcntover <> 0 then kicker25.destroyball:kicker26.createball:exit sub:end if
	if kicker24.ballcntover <> 0 then kicker24.destroyball:kicker25.createball:exit sub:end if
	if kicker23.ballcntover <> 0 then kicker23.destroyball:kicker24.createball:exit sub:end if
	if kicker22.ballcntover <> 0 then kicker22.destroyball:kicker23.createball:exit sub:end if
	if kicker21.ballcntover <> 0 then kicker21.destroyball:kicker22.createball:exit sub:end if
	if kicker20.ballcntover <> 0 then kicker20.destroyball:kicker21.createball:exit sub:end if
	if kicker19.ballcntover <> 0 then kicker19.destroyball:kicker20.createball:exit sub:end if
	if kicker18.ballcntover <> 0 then kicker18.destroyball:kicker19.createball:exit sub:end if
	if kicker17.ballcntover <> 0 then kicker17.destroyball:kicker18.createball:exit sub:end if
	if kicker16.ballcntover <> 0 then kicker16.destroyball:kicker17.createball:exit sub:end if
	if kicker15.ballcntover <> 0 then kicker15.destroyball:kicker16.createball:exit sub:end if
	if kicker14.ballcntover <> 0 then kicker14.destroyball:kicker15.createball:exit sub:end if
	if kicker13.ballcntover <> 0 then kicker13.destroyball:kicker14.createball:exit sub:end if
	if kicker12.ballcntover <> 0 then kicker12.destroyball:kicker13.createball:exit sub:end if
	if kicker11.ballcntover <> 0 then kicker11.destroyball:kicker12.createball:exit sub:end if
	if kicker10.ballcntover <> 0 then kicker10.destroyball:kicker11.createball:exit sub:end if
	if kicker9.ballcntover <> 0 then kicker9.destroyball:kicker10.createball:exit sub:end if
	if kicker8.ballcntover <> 0 then kicker8.destroyball:kicker9.createball:exit sub:end if
	if kicker7.ballcntover <> 0 then kicker7.destroyball:kicker8.createball:exit sub:end if
	if kicker6.ballcntover <> 0 then kicker6.destroyball:kicker7.createball:exit sub:end if
	if kicker5.ballcntover <> 0 then kicker5.destroyball:kicker6.createball:exit sub:end if
	if kicker4.ballcntover <> 0 then kicker4.destroyball:kicker5.createball:exit sub:end if
	if kicker3.ballcntover <> 0 then kicker3.destroyball:kicker4.createball:exit sub:end if
	if kicker2.ballcntover <> 0 then kicker2.destroyball:kicker3.createball:exit sub:end if
	if kicker1.ballcntover <> 0 then kicker1.destroyball:kicker2.createball:exit sub:end if

	if kicker37.ballcntover <> 0 then  	'exit Wheel
		GI2b38.state = 0:ledbb=0
		WheelPos = Primitive140.roty
		kicker37.destroyball
		playsound "Balldrop"

		if WheelPos>=13 and WheelPos<=54 then	'11-54	OYL
			Controller.switch(46)=0
			Controller.switch(47)=1
			Controller.switch(48)=1
		end if
		if WheelPos>=55 and WheelPos<=100 then	'55-100	Spinach Can
			Controller.switch(46)=0
			Controller.switch(47)=0
			Controller.switch(48)=1
		end if
		if WheelPos>=101 and WheelPos<=146 then	'101-146 Lite Lock 
			Controller.switch(46)=0
			Controller.switch(47)=1
			Controller.switch(48)=0
		end if
		if WheelPos>=147 and WheelPos<=190 then	'147-188 10 Mil
			Controller.switch(46)=0
			Controller.switch(47)=0
			Controller.switch(48)=0
		end if
		if WheelPos>=191 and WheelPos<=234 then	'189-234 100 K
			Controller.switch(46)=1
			Controller.switch(47)=1
			Controller.switch(48)=1
		end if
		if WheelPos>=235 and WheelPos<=285 then	'235-285 Spot Item
			Controller.switch(46)=1
			Controller.switch(47)=0
			Controller.switch(48)=1
		end if
		if WheelPos>=286 and WheelPos<=324 then	'286-324 2 Mil
			Controller.switch(46)=1
			Controller.switch(47)=1
			Controller.switch(48)=0
		end if
		if (WheelPos>=325 and WheelPos<=360) or (WheelPos>=0 and WheelPos<=12) then	'325-360-10 Spot Animal
			Controller.switch(46)=1
			Controller.switch(47)=0
			Controller.switch(48)=0
		end if

		vpmtimer.pulsesw 37
		UnderWheelKicker.createball
		UnderWheelKicker.kick 250,3
		vpmtimer.addtimer 500,"Controller.switch(46)=0:Controller.switch(47) =0:Controller.switch(48)=0'"
		exit sub
	end if

	'not needed but here to be complete
	if kicker42.ballcntover <> 0 then kicker42.destroyball:kicker43.createball:exit sub:end if
	if kicker41.ballcntover <> 0 then kicker41.destroyball:kicker42.createball:exit sub:end if
	if kicker40.ballcntover <> 0 then kicker40.destroyball:kicker41.createball:exit sub:end if
	if kicker39.ballcntover <> 0 then kicker39.destroyball:kicker40.createball:exit sub:end if
	if kicker38.ballcntover <> 0 then kicker38.destroyball:kicker39.createball:exit sub:end if
End Sub

Sub EnterWheel1_Hit						'from Ball launch
	EnterWheel1Timer.enabled = True
End Sub

Dim PValue,EnterWheelBool
Sub EnterWheel1Timer_Timer
	'36,42		2 Mil
	'78,84 		Spot Animal
	'126,132	OYL
	'168,174	Spinache Can
	'216,222	Lite Lock
	'258,264	10 Mil
	'306,312	100 K
	'348,354	Spot Item

	EnterWheelBool = False
	PValue = Primitive140.roty

	if  PValue=36 or PValue=78 or PValue=126 or PValue=168 or PValue=216 or PValue=258 or PValue=306 or PValue=348 then
		EnterWheel1Timer.enabled = False
		EnterWheel1.destroyball
		kicker43.createball
	end if

End Sub

Sub EnterWheel2_Hit						'from Upper Ramp
	EnterWheel2Timer.enabled = True
End Sub

Dim PValue2,EnterWheelBool2
Sub EnterWheel2Timer_Timer
	'18,24		OYL
	'60,66		Spinache Can
	'108,114	Lite Lock
	'150,156	10 Mil
	'198,204	100 K
	'240,246	Spot Item
	'288,294	2 Mil
	'330,336	Spot Animal
	EnterWheelBool2 = False
	PValue2 = Primitive140.roty

	if  PValue2=18 or PValue2=60 or PValue2=108 or PValue2=150 or PValue2=198 or PValue2=240 or PValue2=288 or PValue2=330 then
		EnterWheel2Timer.enabled = False
		EnterWheel2.destroyball
		kicker25.createball
	end if
End Sub


' Keys
Sub Table1_KeyDown(ByVal keycode)

    If keycode = LeftMagnaSave Then bLutActive = True: LUTbox.text = "Dimmer Level: " & LUTImage + 1

    If keycode = RightMagnaSave Then
        If bLutActive Then
            NextLUT
        End If
    End If

	If keyCode = PlungerKey Then 
		Controller.Switch(23) = 1
	End If

    If keycode = AddCreditKey Then
        Controller.Switch(12) = 1
	End If

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	If keycode = StartGameKey Then soundStartButton()

	If keycode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 1:SoundNudgeCenter()

	If keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
		Pincab_FlipLeft.X = Pincab_FlipLeft.X + 10 
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		Pincab_FlipRight.X = Pincab_FlipRight.X - 10
	End If

	If KeyDownHandler(keycode) Then Exit Sub

End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = LeftMagnaSave Then bLutActive = False: LUTBox.text = ""

	If keyCode = PlungerKey Then 
		Controller.Switch(23) = 0:Plunger.Fire:SoundPlungerReleaseBall()
	End If

    If keycode = AddCreditKey Then
        Controller.Switch(12) = 0
	End If

	If keycode = LeftFlipperKey Then
		FlipperDeActivate LeftFlipper, LFPress
		Pincab_FlipLeft.X = 2094.795
	End If

	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		Pincab_FlipRight.X = 2356.34
	End If

	If KeyUpHandler(keycode) Then Exit Sub

End Sub

'---------------------------------
'------  Switch Assignment  ------
'---------------------------------

 'Left Targets
Sub SWsqr5_hit():sqr5.transz = 10:sqr6.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 35:End Sub
Sub SWsqr5_Timer():sqr5.transz = 5:sqr6.transz = 5:SWsqr5a:Me.TimerEnabled = 0:End Sub
Sub SWsqr5a:sqr5.transZ = 0:sqr6.transz = 0:End Sub

Sub SWror5_hit():ror5.transz = 10:ror6.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 34:End Sub
Sub SWror5_Timer():ror5.transz = 5:ror6.transz = 5:SWror5a:Me.TimerEnabled = 0:End Sub
Sub SWror5a:ror5.transZ = 0:ror6.transz = 0:End Sub

Sub swtrr5_hit():trr5.transz = 10:trr6.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 33:End Sub
Sub swtrr5_Timer():trr5.transz = 5:trr6.transz = 5:SWtrr5a:Me.TimerEnabled = 0:End Sub
Sub swtrr5a:trr5.transZ = 0:trr6.transz = 0:End Sub 
 
 'Center Targets
Sub SWsqr3_hit():sqr3.transz = 10:sqr4.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 43:End Sub
Sub SWsqr3_Timer():sqr3.transz = 5:sqr4.transz = 5:SWsqr3a:Me.TimerEnabled = 0:End Sub
Sub SWsqr3a:sqr3.transz = 0:sqr4.transz = 0:End Sub

Sub SWror3_hit():ror3.transz = 10:ror4.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 42:End Sub
Sub SWror3_Timer():ror3.transz = 5:ror4.transz = 5:SWror3a:Me.TimerEnabled = 0:End Sub
Sub SWror3a:ror3.transZ = 0:ror4.transz = 0:End Sub

Sub swtrr3_hit():trr3.transz = 10:trr4.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 41:End Sub
Sub swtrr3_Timer():trr3.transz = 5:trr4.transz = 5:SWtrr3a:Me.TimerEnabled = 0:End Sub
Sub swtrr3a:trr3.transZ = 0:trr4.transz = 0:End Sub 

 'Right Targets
Sub SWsqr1_hit():sqr1.transz = 10:sqr2.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 51:End Sub
Sub SWsqr1_Timer():sqr1.transz = 5:sqr2.transz = 5:SWsqr1a:Me.TimerEnabled = 0:End Sub
Sub SWsqr1a:sqr1.transZ = 0:sqr2.transz = 0:End Sub

Sub SWror1_hit():ror1.transz = 10:ror2.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 50:End Sub
Sub SWror1_Timer():ror1.transz = 5:ror2.transz = 5:SWror1a:Me.TimerEnabled = 0:End Sub
Sub SWror1a:ror1.transZ = 0:ror2.transz = 0:End Sub

Sub swtrr1_hit():trr1.transz = 10:trr2.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 49:End Sub
Sub swtrr1_Timer():trr1.transz = 5:trr2.transz = 5:SWtrr1a:Me.TimerEnabled = 0:End Sub
Sub swtrr1a:trr1.transZ = 0:trr2.transz = 0:End Sub

sub trigger11_hit:Controller.Switch(11)=1:End Sub    'left lane
sub trigger11_unhit:Controller.Switch(11)=0:End Sub
'12		Buy in - handled elsewhere
'13		Start Button - handled elsewhere
'14		Tilt - handled elsewhere
sub trigger15_hit:Controller.Switch(15)=1:End Sub    'right lane
sub trigger15_unhit:Controller.Switch(15)=0:End Sub

' Bumpers
Sub Bumper16_hit:vpmtimer.pulsesw 16:RandomSoundBumperTop Bumper16:End Sub    ' Left Bumper
Sub Bumper17_hit:vpmtimer.pulsesw 17:RandomSoundBumperMiddle Bumper17:End Sub ' Right Bumper
Sub Bumper18_hit:vpmtimer.pulsesw 18:RandomSoundBumperBottom Bumper18:End Sub ' Center Bumper

'21		Slam Tilt - handled elsewhere
'22		Coin Door closed - handled elsewhere
'23		Ball Launch - handled elsewhere
'24		Always closed - handled elsewhere
sub trigger25_hit:Controller.Switch(25)=1:End Sub    'left loop
sub trigger25_unhit:Controller.Switch(25)=0:End Sub
       
'Right E Target
Sub Target26_hit():T26.transz = 10:t26t.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 28:End Sub
Sub Target26_Timer():T26.transz = 5:t26t.transz = 5:Tar26:Me.TimerEnabled = 0:End Sub
Sub Tar26:t26.transZ = 0:t26t.transz = 0:End Sub       
'Right Y Target
Sub Target27_hit():T27.transz = 10:t27t.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 27:End Sub
Sub Target27_Timer():T27.transz = 5:t27t.transz = 5:Tar27:Me.TimerEnabled = 0:End Sub
Sub Tar27:t27.transZ = 0:t27t.transz = 0:End Sub
'Right lower E Target - Switch 26 & 28 got reversed somehow previously - TastyWasps
Sub Target28_hit():T28.transz = 10:t28t.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 26:End Sub
Sub Target28_Timer():T28.transz = 5:t28t.transz = 5:Tar28:Me.TimerEnabled = 0:End Sub
Sub Tar28:t28.transZ = 0:t28t.transz = 0:End Sub

'31		left popper - handled elsewhere
'32		right popper - handled elsewhere
sub trigger33_hit:Controller.Switch(33)=1:End Sub    'right loop
sub trigger33_unhit:Controller.Switch(33)=0:End Sub
sub trigger34_hit:Controller.Switch(34)=1:End Sub    'ramp entrance
sub trigger34_unhit:Controller.Switch(34)=0:End Sub
sub trigger35_hit:Controller.Switch(35)=1:End Sub    'ramp completion
sub trigger35_unhit:Controller.Switch(35)=0:End Sub
'36		Escalator Popper - handled elsewhere
'37     Wheel Exit - handled elsewhere       

'Hag Target
Sub Target38_hit():T38.transz = 10:t38t.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 38:End Sub
Sub Target38_Timer():T38.transz = 5:t38t.transz = 5:Tar38:Me.TimerEnabled = 0:End Sub
Sub Tar38:t38.transZ = 0:t38t.transz = 0:End Sub

'Upper PF Two Bank (2)
Sub Target41a_hit():T41a.transz = 10:t41at.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 41:End Sub
Sub Target41a_Timer():T41a.transz = 5:t41at.transz = 5:Tar41a:Me.TimerEnabled = 0:End Sub
Sub Tar41a:t41a.transZ = 0:t41at.transz = 0:End Sub      

'Upper PF Two Bank (2)
Sub Target41b_hit():T41b.transz = 10:t41bt.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 41:End Sub
Sub Target41b_Timer():T41b.transz = 5:t41bt.transz = 5:Tar41b:Me.TimerEnabled = 0:End Sub
Sub Tar41b:t41b.transZ = 0:t41bt.transz = 0:End Sub       

 
sub trigger42_hit:Controller.Switch(42)=1:End Sub    'center lane
sub trigger42_unhit:Controller.Switch(42)=0:End Sub
'43		Lockup Upper - handled elsewhere
'44		Lockup Center - handled elsewhere
'45		Lockup Lower - handled elsewhere
'46		Wheel Opto 1 - handled elsewhere
'47		Wheel Opto 2 - handled elsewhere
'48		Wheel Opto 3 - handled elsewhere

'51		Right Trough - handled elsewhere
'52		Trough 2nd - handled elsewhere
'53		Trough 3rd - handled elsewhere
'54		Trough 4th - handled elsewhere
'55		Trough 5th - handled elsewhere
'56		Left Trough - handled elsewhere

sub trigger57_hit:Controller.Switch(57)=1:GI2b38.state = 1:ledbb = 1:End Sub	'Trough Jam
sub trigger57_unhit:Controller.Switch(57)=0:End Sub		

'Sea Target
Sub Target58_hit():T58.transz = 10:t58t.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 58:End Sub
Sub Target58_Timer():T58.transz = 5:t58t.transz = 5:Tar58:Me.TimerEnabled = 0:End Sub
Sub Tar58:t58.transZ = 0:t58t.transz = 0:End Sub

'Left Cheek
Sub Target61_hit():T61.transz = 10:t61t.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 61:End Sub
Sub Target61_Timer():T61.transz = 5:t61t.transz = 5:Tar61:Me.TimerEnabled = 0:End Sub
Sub Tar61:t61.transZ = 0:t61t.transz = 0:End Sub
'Right Cheek
Sub Target62_hit():T62.transz = 10:t62t.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 62:End Sub
Sub Target62_Timer():T62.transz = 5:t62t.transz = 5:Tar62:Me.TimerEnabled = 0:End Sub
Sub Tar62:t62.transZ = 0:t62t.transz = 0:End Sub

sub trigger63_hit:Controller.Switch(63)=1:End Sub  	'Escalator Exit (upper PF)
sub trigger63_unhit:Controller.Switch(63)=0:End Sub
sub trigger64_hit:Controller.Switch(64)=1:End Sub  	'Animal Dolphin
sub trigger64_unhit:Controller.Switch(64)=0:End Sub
sub trigger65_hit:Controller.Switch(65)=1:End Sub  	'Animal Eagle
sub trigger65_unhit:Controller.Switch(65)=0:End Sub
sub trigger66_hit:Controller.Switch(66)=1:End Sub  	'Animal Tiger
sub trigger66_unhit:Controller.Switch(66)=0:End Sub
sub trigger67_hit:Controller.Switch(67)=1:End Sub  	'Animal Panda
sub trigger67_unhit:Controller.Switch(67)=0:End Sub
sub trigger68_hit:Controller.Switch(68)=1:End Sub  	'Animal Rhino
sub trigger68_unhit:Controller.Switch(68)=0:End Sub

'Right P Target
Sub Target71_hit():T71.transz = 10:t71t.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 71:End Sub
Sub Target71_Timer():T71.transz = 5:t71t.transz = 5:Tar71:Me.TimerEnabled = 0:End Sub
Sub Tar71:t71.transZ = 0:t71t.transz = 0:End Sub
'Right O Target
Sub Target72_hit():T72.transz = 10:t72t.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 72:End Sub
Sub Target72_Timer():T72.transz = 5:t72t.transz = 5:Tar72:Me.TimerEnabled = 0:End Sub
Sub Tar72:t72.transZ = 0:t72t.transz = 0:End Sub
'Right lower P Target
Sub Target73_hit():T73.transz = 10:t73t.transz = 10:Me.TimerEnabled = 1:vpmTimer.PulseSw 73:End Sub
Sub Target73_Timer():T73.transz = 5:t73t.transz = 5:Tar73:Me.TimerEnabled = 0:End Sub
Sub Tar73:t73.transZ = 0:t73t.transz = 0:End Sub

sub LeftOutlane_hit:Controller.Switch(74)=1:End Sub 'Left Outlane
sub LeftOutlane_unhit:Controller.Switch(74)=0:End Sub
sub LeftInlane_hit:Controller.Switch(75)=1:End Sub  'Left Inlane   
sub LeftInlane_unhit:Controller.Switch(75)=0:End Sub

' Slingshots 
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotRight Sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -43
    vpmTimer.PulseSw 77:RStep = 0:RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 2:RSLing3.Visible = 0:RSLing2.Visible = 1:RSLing1.Visible = 0:sling1.TransZ = -33
        Case 3:RSLing3.Visible = 1:RSLing2.Visible = 0:RSLing1.Visible = 0:sling1.TransZ = -20
        Case 4:RSLing3.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotLeft Sling2
    LSling.Visible = 0
    LSling1.Visible = 1
	sling2.TransZ = -43
    vpmTimer.PulseSw 76:LStep = 0:LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 2:LSLing3.Visible = 0:LSLing2.Visible = 1:LSLing1.Visible = 0:sling2.TransZ = -33
        Case 3:LSLing3.Visible = 1:LSLing2.Visible = 0:LSLing1.Visible = 0:sling2.TransZ = -20
        Case 4:LSLing3.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

sub RightInlane_hit:Controller.Switch(78)=1:End Sub      'Right Inlane   
sub RightInlane_unhit:Controller.Switch(78)=0:End Sub

'81		Upper Exit to Wheel - handled elsewhere
sub trigger82_hit:Controller.Switch(82)=1:End Sub  'Upper Ramp Left
sub trigger82_unhit:Controller.Switch(82)=0:End Sub
sub trigger83_hit:Controller.Switch(83)=1:End Sub  'Upper Ramp Right
sub trigger83_unhit:Controller.Switch(83)=0:End Sub
sub trigger84_hit:Controller.Switch(84)=1:End Sub  'Animal Jackpot
sub trigger84_unhit:Controller.Switch(84)=0:End Sub
sub RightOutlane_hit:Controller.Switch(85)=1:End Sub 'Right Outlane
sub RightOutlane_unhit:Controller.Switch(85)=0:End Sub
sub trigger86_hit:Controller.Switch(86)=1:End Sub    'Shooter Lane
sub trigger86_unhit:Controller.Switch(86)=0:End Sub
'87		Lockup Kicker - handled elsewhere
sub trigger88_hit:Controller.Switch(88)=1:End Sub  'Upper Shot Exit
sub trigger88_unhit:Controller.Switch(88)=0:End Sub

Sub SweePeeHelper_Hit
	ActiveBall.velz = 0
End Sub

' Helper / Ramp Sound Functions
Sub Trigger1_Hit
	If ActiveBall.vely < -40 Then
		ActiveBall.vely = -40
	End If
End Sub

Sub LeftRampStart_Hit
	WireRampOn True
End Sub

Sub LeftRampEnd_Hit
	WireRampOff
End Sub

Sub AnimalRampStart_Hit
	WireRampOn True
End Sub

Sub AnimalRampStart2_Hit
	WireRampOn True
End Sub

Sub TopRightRampStart_Hit
	WireRampOn False
End Sub

Sub MiddleWireRampStart_Hit
	WireRampOn False
End Sub

Sub MiddleRightRampStart_Hit
	WireRampOn False
End Sub

'Flipper Primitives and gate primitives and more - reflect lamps
Sub FlipperTimer_Timer
	LFPrim.objRotz = LeftFlipper.CurrentAngle
	RFPrim.objRotz= RightFlipper.CurrentAngle
	ULFPrim.objRotz = UpperLeftFlipper.CurrentAngle
	URFPrim.objRotz= UpperRightFlipper.CurrentAngle
	LeftFlipperSh.RotZ = LeftFlipper.currentangle
	RightFlipperSh.RotZ = RightFlipper.currentangle
	If gate01.CurrentAngle >-1 and gate01.CurrentAngle < 50 Then
		Primitive103.Rotx = gate01.CurrentAngle +90
	end if
	If gate02.CurrentAngle >-1 and gate02.CurrentAngle < 50 Then
		Primitive104.Rotx = gate02.CurrentAngle +90
	end if
	If gate03.CurrentAngle >-1 and gate03.CurrentAngle < 50 Then
		Primitive105.Rotx = gate03.CurrentAngle +90
	end if
	If gate04.CurrentAngle >-1 and gate04.CurrentAngle < 50 Then
		Primitive106.Rotx = gate04.CurrentAngle +90
	end if
	If gate05.CurrentAngle >-1 and gate05.CurrentAngle < 50 Then
		Primitive107.Rotx = gate05.CurrentAngle +90
	end if
	if LSLing1.Visible = 1 then sling2.TransZ = -43:end If
	if LSLing2.Visible = 1 then sling2.TransZ = -33:end If
	if LSLing3.Visible = 1 then sling2.TransZ = -20:end If
	if LSLing.Visible = 1 then sling2.TransZ = 0:end If

	if lamp73.state = 1 then lamp73r.state = 1 else lamp73r.state = 0:end If
	if lamp67.state = 1 then lamp67r.state = 1 else lamp67r.state = 0:end If
	if lamp68.state = 1 then lamp68r.state = 1 else lamp68r.state = 0:end If
	if lamp75.state = 1 then lamp75r.state = 1 else lamp75r.state = 0:end If
	if lamp62.state = 1 then lamp62r.state = 1 else lamp62r.state = 0:end If
	if lamp83.state = 1 then lamp83r.state = 1 else lamp83r.state = 0:end If
	if lamp84.state = 1 then lamp84r.state = 1 else lamp84r.state = 0:end If
End Sub

dim blue
blue = 0
sub Flasherblue_timer()
		select Case Blue
			case 0 :Primitive148.image = "blue2"
			case 1 :Primitive148.image = "blue1"
			case 2 :Primitive148.image = "blue0"
		End Select
	blue = blue + 1
	if blue = 3 then 
		blue = 0
		Primitive148.image = "blue0"
		me.enabled = 0
	end if
end Sub

dim red
red = 0
sub Flasherred_timer()
		select Case red
			case 0 :Primitive149.image = "red2"
			case 1 :Primitive149.image = "red1"
			case 2 :Primitive149.image = "red01"
		End Select
	red = red + 1
	if red = 3 then 
		red = 0
		Primitive149.image = "red01"
		me.enabled = 0
	end if
end Sub

dim blueb
blueb = 0
sub Flasherblueh_timer()
		select Case Blueb
			case 0 :Primitive159.image = "blueh2"
			case 1 :Primitive159.image = "blueh1"
			case 2 :Primitive159.image = "blueh0"
		End Select
	blueb = blueb + 1
	if blueb = 3 then 
		blueb = 0
		Primitive159.image = "blueh0"
		me.enabled = 0
	end if
end Sub

dim jaune
jaune = 0
sub Flasherjaune_timer()
		select Case jaune
			case 0 :Primitive153.image = "jaune2"
			case 1 :Primitive153.image = "jaune1"
			case 2 :Primitive153.image = "jaune0"
		End Select
	jaune = jaune + 1
	if jaune = 3 then 
		jaune = 0
		Primitive153.image = "jaune0"
		me.enabled = 0
	end if
end Sub

sub realtime_timer()
	gate1p.Rotx = gate1.CurrentAngle + 90
end Sub

'********************************************************
'********    Flupper's subs for his Flashers    *********
'********************************************************

'*** Blue flasher 2 ***
Sub FlasherFlash2_Timer()
	dim flashx3, matdim
	If not FlasherFlash2.TimerEnabled Then 
		FlasherFlash2.TimerEnabled = True
		FlasherFlash2.visible = 1
	End If
	flashx3 = FlashLevel2 * FlashLevel2 * FlashLevel2
	Flasherflash2.opacity = 1000 * flashx3
	Flasher22b.IntensityScale = flashx3
	'debug.print flashx3
	matdim = Round(10 * FlashLevel2)
	FlashLevel2 = FlashLevel2 * 0.85 - 0.01
	If FlashLevel2 < 0 Then
		FlasherFlash2.TimerEnabled = False
		FlasherFlash2.visible = 0
	End If
End Sub

'*** Blue flasher 3 upright***
Sub FlasherFlash3_Timer()
	dim flashx3, matdim
	If not FlasherFlash3.TimerEnabled Then 
		FlasherFlash3.TimerEnabled = True
		FlasherFlash3.visible = 1
	End If
	flashx3 = FlashLevel3 * FlashLevel3 * FlashLevel3
	Flasherflash3.opacity = 1000 * flashx3
	Flasher24.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	FlashLevel3 = FlashLevel3 * 0.85 - 0.01
	If FlashLevel3 < 0 Then
		FlasherFlash3.TimerEnabled = False
		FlasherFlash3.visible = 0
	End If
End Sub

'*** Red flasher 4 right***
Sub FlasherFlash4_Timer()
	dim flashx3, matdim
	If not FlasherFlash4.TimerEnabled Then 
		FlasherFlash4.TimerEnabled = True
		FlasherFlash4.visible = 1
	End If
	flashx3 = FlashLevel4 * FlashLevel4 * FlashLevel4
	Flasherflash4.opacity = 1000 * flashx3
	Flasher26.IntensityScale = flashx3
	Flasher1.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel4)
	FlashLevel4 = FlashLevel4 * 0.85 - 0.01
	If FlashLevel4 < 0 Then
		FlasherFlash4.TimerEnabled = False
		FlasherFlash4.visible = 0
	End If
End Sub

'*** Yellow flasher 5 right***
Sub FlasherFlash5_Timer()
	dim flashx3, matdim
	If not FlasherFlash5.TimerEnabled Then 
		FlasherFlash5.TimerEnabled = True
		FlasherFlash5.visible = 1
	End If
	flashx3 = FlashLevel5 * FlashLevel5 * FlashLevel5
	Flasherflash5.opacity = 1000 * flashx3
	Flasher28.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel5)
	FlashLevel5 = FlashLevel5 * 0.85 - 0.01
	If FlashLevel5 < 0 Then
		FlasherFlash5.TimerEnabled = False
		FlasherFlash5.visible = 0
	End If
End Sub

'********************************************
'* Led detection jpj                        *
'********************************************

sub Trigger2_Hit()
	led = led +1
end sub

sub Trigger2_UnHit()
	led = led -1
if led = -1 then led = 0
end sub

Sub Trigger3_Hit()
	playsound "Balldrop"
	Led = led - 1
	if led = -1 then led = 0
End Sub

'********************************************
'*	Led part, adapted from JP Salas script  *
'********************************************

Dim rGreen, rRed, rBlue, RGBFactor, RGBStep, Led, ledbb
ledbb = 0
led = 0

Sub RainbowTimer_Timer 'rainbow led light color changing
	RGBFactor =20 
	Select Case RGBStep
		Case 0 'Green
			rGreen = rGreen + RGBFactor
			If rGreen > 255 then
				rGreen = 255
				RGBStep = 1
			End If
		Case 1 'Red
			rRed = rRed - RGBFactor
			If rRed < 0 then
				rRed = 0
				RGBStep = 2
			End If
		Case 2 'Blue
			rBlue = rBlue + RGBFactor
			If rBlue > 255 then
				rBlue = 255
				RGBStep = 3
			End If
		Case 3 'Green
			rGreen = rGreen - RGBFactor
			If rGreen < 0 then
				rGreen = 0
				RGBStep = 4
			End If
		Case 4 'Red
			rRed = rRed + RGBFactor
			If rRed > 255 then
				rRed = 255
				RGBStep = 5
			End If
		Case 5 'Blue
			rBlue = rBlue - RGBFactor
			If rBlue < 0 then
				rBlue = 0
				RGBStep = 0
			End If
		End Select

		if led > 0 then 
			Primitive5.material = "BumpersBlueLed"
			'Primitive66.material = "BumpersBlue"
			'Primitive124.material = "BumpersBlue"
			For each obj in Led3
				obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
				obj.colorfull = RGB(rRed, rGreen, rBlue)
			Next
			For each obj in LedUp
				obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
				obj.colorfull = RGB(rRed, rGreen, rBlue)
			Next
		end if

		if ledbb = 1 then 
			For each obj in Ledb
				obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
				obj.colorfull = RGB(rRed, rGreen, rBlue)
			Next
		end if

		if ledbb = 0 then 
				GI2b13.color = RGB(254, 109, 67)
				GI2b13.colorfull = RGB(232, 252, 255)
		end if

		if led = 0 then 
			Primitive5.material = "BumpersBlue"
			'Primitive66.material = "BumpersBlue"
			'Primitive124.material = "BumpersBlue"
			For each obj in Led1
				obj.color = RGB(13, 147, 94)
				obj.colorfull = RGB(109, 248, 165)
			Next
			For each obj in Led2
				obj.color = RGB(210, 251, 255)
				obj.colorfull = RGB(199, 250, 254)
			Next
			For each obj in LedUp
				obj.color = RGB(255, 255, 0)
				obj.colorfull = RGB(255, 255, 255)
			Next
		end if
End Sub

'**************
' Flipper Subs
'**************
 
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

' Animal Flippers
Sub Flipper1_Collide(parm)
	PlaySound "MetalJP"
End Sub
Sub Flipper2_Collide(parm)
	PlaySound "MetalJP"
End Sub
Sub Flipper3_Collide(parm)
	PlaySound "MetalJP"
End Sub
Sub Flipper4_Collide(parm)
	PlaySound "MetalJP"
End Sub
Sub Flipper5_Collide(parm)
    PlaySound "MetalJP"
End Sub

'*****************
' Triggers ramps
'*****************
Dim tra, traa, trl, trll, trf, trff
tra = 0:traa = 0:trl = 0:trll = 0:trf=0:trff=0

Sub TRAB_UnHit:
	playsound "WireRamp2jp"
End Sub
Sub TRABS_UnHit:
	stopsound "WireRamp2jp"
	playsound "WireRamp_Stop_balldrop"
End Sub

' Launch Ramp
Sub TriggerRampL2_Hit:
	if TRL=0 and TRLL=0 then playsound "WireRamp2jpShort01":TRA=1:exit sub
	if (TRL=1 and TRLL=1) or (TRL=1 and TRLL=0) then stopsound "WireRamp2jpShort01":TRA=0:TRAA=0
End Sub

Sub TriggerRampLL_Unhit:
	TRLL=1
End Sub

Sub TriggerRampL1_Hit:
	stopsound "WireRamp2jpShort01":TRL=0:TRLL=0
End Sub


Sub TRAF_Hit:
	if trf=1 and trff=0 then playsound "WireRamp2jpShort01":End If
	if trf=0 and trff=0 then stopsound "WireRamp2jpShort01":End If
End Sub
Sub TRAF1_UnHit:
	if trf=0 and trff=0 then playsound "WireRamp2jpShort01":End If
	if trf=1 and trff=0 then stopsound "WireRamp2jpShort01":End If
End Sub
Sub TRAF2_Hit:
	trf=1
	trff=0
End Sub
Sub TRAF3_Hit:
	trf=0
	trff=0
End Sub

Sub UpperPlayfieldScan
	' If any ball is on the upper playfield, allow the upper PF flippers to flip. 
	Dim BOT, b
	BOT = GetBalls
	For b = 0 to UBound(BOT)
		If BOT(b).z > 210 Then
			onUpperPF = True
			Exit Sub
		Else
			onUpperPF = False
		End If
	Next

	' If routine gets this far then nothing is on the upper PF so move the Upper PFs to starting position in case they were rotated up.
	If UpperLeftFlipper.CurrentAngle < 126 Then
		UpperLeftFlipper.RotateToStart
	End If

	If UpperRightFlipper.CurrentAngle > -126 Then
		UpperRightFlipper.RotateToStart
	End If

End Sub

'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF
Set LF = New FlipperPolarity
Dim RF
Set RF = New FlipperPolarity

InitPolarity

'*******************************************
' Early 90's and after

Sub InitPolarity()
	Dim x, a
	a = Array(LF, RF)
	For Each x In a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger
		
		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, -5.5
		x.AddPt "Polarity", 2, 0.4, -5.5
		x.AddPt "Polarity", 3, 0.6, -5.0
		x.AddPt "Polarity", 4, 0.65, -4.5
		x.AddPt "Polarity", 5, 0.7, -4.0
		x.AddPt "Polarity", 6, 0.75, -3.5
		x.AddPt "Polarity", 7, 0.8, -3.0
		x.AddPt "Polarity", 8, 0.85, -2.5
		x.AddPt "Polarity", 9, 0.9,-2.0
		x.AddPt "Polarity", 10, 0.95, -1.5
		x.AddPt "Polarity", 11, 1, -1.0
		x.AddPt "Polarity", 12, 1.05, -0.5
		x.AddPt "Polarity", 13, 1.1, 0
		x.AddPt "Polarity", 14, 1.3, 0
		
		x.AddPt "Velocity", 0, 0,	   1
		x.AddPt "Velocity", 1, 0.160, 1.06
		x.AddPt "Velocity", 2, 0.410, 1.05
		x.AddPt "Velocity", 3, 0.530, 1'0.982
		x.AddPt "Velocity", 4, 0.702, 0.968
		x.AddPt "Velocity", 5, 0.95,  0.968
		x.AddPt "Velocity", 6, 1.03,  0.945
	Next
	
	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub

'' Flipper trigger hit subs
Sub TriggerLF_Hit()
	LF.Addball activeball
End Sub
Sub TriggerLF_UnHit()
	LF.PolarityCorrect activeball
End Sub
Sub TriggerRF_Hit()
	RF.Addball activeball
End Sub
Sub TriggerRF_UnHit()
	RF.PolarityCorrect activeball
End Sub

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
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	Dim BOT
	BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
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

Dim LFPress, RFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

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
		Dim b, BOT
		BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
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
Sub RDampen_Timer
	Cor.Update
End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

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
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

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

	FlipperCradleCollision ball1, ball2, velocity

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
	Dim BOT
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 To tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(BOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(BOT)
		If BallVel(BOT(b)) > 1 And BOT(b).z < 250 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If BOT(b).VelZ <  - 1 And BOT(b).z < 55 And BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz >  - 7 Then
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
		' Comment the next If block, if you are not implementing the Dynamic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height = BOT(b).z - BallSize / 4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Else
				BallShadowA(b).height = 0.1
			End If
			BallShadowA(b).Y = BOT(b).Y + offsetY
			BallShadowA(b).X = BOT(b).X + offsetX
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
Dim RampBalls(7,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(7)

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
' 	ZFLD:  FLUPPER DOMES
'******************************************************
Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object			 ***
Set TableRef = Table1		   ' *** change this, if your table has another name				   ***
FlasherLightIntensity = 0.1	 ' *** lower this, if the VPX lights are too bright (i.e. 0.1)	   ***
FlasherFlareIntensity = 0.3	 ' *** lower this, if the flares are too bright (i.e. 0.1)		   ***
FlasherBloomIntensity = 0.2	 ' *** lower this, if the blooms are too bright (i.e. 0.1)		   ***
FlasherOffBrightness = 0.5	  ' *** brightness of the flasher dome when switched off (range 0-2)  ***
' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20), ObjTargetLevel(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height

'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "red"

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr)
	Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr)
	Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ = Atn( (tablewidth / 2 - objbase(nr).x) / (objbase(nr).y - tableheight * 1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ
		objflasher(nr).height = objbase(nr).z + 40
	End If
	
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0
	objlit(nr).visible = 0
	objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX
	objlit(nr).RotY = objbase(nr).RotY
	objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX
	objlit(nr).ObjRotY = objbase(nr).ObjRotY
	objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x
	objlit(nr).y = objbase(nr).y
	objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	
	'rothbauerw
	'Adjust the position of the flasher object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	If objbase(nr).roty > 135 Then
		objflasher(nr).y = objbase(nr).y + 50
		objflasher(nr).height = objbase(nr).z + 20
	Else
		objflasher(nr).y = objbase(nr).y + 20
		objflasher(nr).height = objbase(nr).z + 50
	End If
	objflasher(nr).x = objbase(nr).x
	
	'rothbauerw
	'Adjust the position of the light object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	objlight(nr).x = objbase(nr).x
	objlight(nr).y = objbase(nr).y
	objlight(nr).bulbhaloheight = objbase(nr).z - 10
	
	'rothbauerw
	'Assign the appropriate bloom image basked on the location of the flasher base
	'Comment out these lines if you want to manually assign the bloom images
	Dim xthird, ythird
	xthird = tablewidth / 3
	ythird = tableheight / 3
	If objbase(nr).x >= xthird And objbase(nr).x <= xthird * 2 Then
		objbloom(nr).imageA = "flasherbloomCenter"
		objbloom(nr).imageB = "flasherbloomCenter"
	ElseIf objbase(nr).x < xthird And objbase(nr).y < ythird Then
		objbloom(nr).imageA = "flasherbloomUpperLeft"
		objbloom(nr).imageB = "flasherbloomUpperLeft"
	ElseIf  objbase(nr).x > xthird * 2 And objbase(nr).y < ythird Then
		objbloom(nr).imageA = "flasherbloomUpperRight"
		objbloom(nr).imageB = "flasherbloomUpperRight"
	ElseIf objbase(nr).x < xthird And objbase(nr).y < ythird * 2 Then
		objbloom(nr).imageA = "flasherbloomCenterLeft"
		objbloom(nr).imageB = "flasherbloomCenterLeft"
	ElseIf  objbase(nr).x > xthird * 2 And objbase(nr).y < ythird * 2 Then
		objbloom(nr).imageA = "flasherbloomCenterRight"
		objbloom(nr).imageB = "flasherbloomCenterRight"
	ElseIf objbase(nr).x < xthird And objbase(nr).y < ythird * 3 Then
		objbloom(nr).imageA = "flasherbloomLowerLeft"
		objbloom(nr).imageB = "flasherbloomLowerLeft"
	ElseIf  objbase(nr).x > xthird * 2 And objbase(nr).y < ythird * 3 Then
		objbloom(nr).imageA = "flasherbloomLowerRight"
		objbloom(nr).imageB = "flasherbloomLowerRight"
	End If
	
	' set the texture and color of all objects
	Select Case objbase(nr).image
		Case "dome2basewhite"
			objbase(nr).image = "dome2base" & col
			objlit(nr).image = "dome2lit" & col
			
		Case "ronddomebasewhite"
			objbase(nr).image = "ronddomebase" & col
			objlit(nr).image = "ronddomelit" & col
			
		Case "domeearbasewhite"
			objbase(nr).image = "domeearbase" & col
			objlit(nr).image = "domeearlit" & col
	End Select
	If TestFlashers = 0 Then
		objflasher(nr).imageA = "domeflashwhite"
		objflasher(nr).visible = 0
	End If
	Select Case col
		Case "blue"
			objlight(nr).color = RGB(4,120,255)
			objflasher(nr).color = RGB(200,255,255)
			objbloom(nr).color = RGB(4,120,255)
			objlight(nr).intensity = 5000
			
		Case "green"
			objlight(nr).color = RGB(12,255,4)
			objflasher(nr).color = RGB(12,255,4)
			objbloom(nr).color = RGB(12,255,4)
			
		Case "red"
			objlight(nr).color = RGB(255,32,4)
			objflasher(nr).color = RGB(255,32,4)
			objbloom(nr).color = RGB(255,32,4)
			
		Case "purple"
			objlight(nr).color = RGB(230,49,255)
			objflasher(nr).color = RGB(255,64,255)
			objbloom(nr).color = RGB(230,49,255)
			
		Case "yellow"
			objlight(nr).color = RGB(200,173,25)
			objflasher(nr).color = RGB(255,200,50)
			objbloom(nr).color = RGB(200,173,25)
			
		Case "white"
			objlight(nr).color = RGB(255,240,150)
			objflasher(nr).color = RGB(100,86,59)
			objbloom(nr).color = RGB(255,240,150)
			
		Case "orange"
			objlight(nr).color = RGB(255,70,0)
			objflasher(nr).color = RGB(255,70,0)
			objbloom(nr).color = RGB(255,70,0)
	End Select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT And ObjFlasher(nr).RotX =  - 45 Then
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle)
	angle = ((angle + 360 - objbase(nr).ObjRotZ) Mod 180) / 30
	objbase(nr).showframe(angle)
	objlit(nr).showframe(angle)
End Sub

Sub FlashFlasher(nr)
	If Not objflasher(nr).TimerEnabled Then
		objflasher(nr).TimerEnabled = True
		objflasher(nr).visible = 1
		objbloom(nr).visible = 1
		objlit(nr).visible = 1
	End If
	objflasher(nr).opacity = 1000 * FlasherFlareIntensity * ObjLevel(nr) ^ 2.5
	objbloom(nr).opacity = 100 * FlasherBloomIntensity * ObjLevel(nr) ^ 2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr) ^ 3
	objbase(nr).BlendDisableLighting = FlasherOffBrightness + 10 * ObjLevel(nr) ^ 3
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr) ^ 2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0
	If Round(ObjTargetLevel(nr),1) > Round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) + 0.3
		If ObjLevel(nr) > 1 Then ObjLevel(nr) = 1
	ElseIf Round(ObjTargetLevel(nr),1) < Round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) * 0.85 - 0.01
		If ObjLevel(nr) < 0 Then ObjLevel(nr) = 0
	Else
		ObjLevel(nr) = Round(ObjTargetLevel(nr),1)
		objflasher(nr).TimerEnabled = False
	End If
	'   ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then
		objflasher(nr).TimerEnabled = False
		objflasher(nr).visible = 0
		objbloom(nr).visible = 0
		objlit(nr).visible = 0
	End If
End Sub

Sub FlasherFlash1_Timer()
	FlashFlasher(1)
End Sub

'******************************************************
'******  END FLUPPER DOMES
'******************************************************

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
Dim objrtx1(6), objrtx2(6)
Dim objBallShadow(6)
Dim OnPF(6)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4, BallShadowA5)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

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

	Dim gBOT: gBOT=getballs	

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
	Dim gBOT: gBOT=getballs	
	
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


'***************************
'   LUT - Darkness control 
'***************************

Dim bLutActive, LUTImage

Sub LoadLUT

	Dim FileObj, ScoreFile, TextStr, rLine
    bLutActive = False

	If DesktopMode = True or RenderingMode = 2 Then
		LUTImage = 0
	Else
		LUTImage = 2 ' Cab Mode a touch darker by default
	End If

	Set FileObj=CreateObject("Scripting.FileSystemObject")

	If Not FileObj.FolderExists(UserDirectory) then 
		UpdateLUT
		Exit Sub
	End If

	If Not FileObj.FileExists(UserDirectory & "PopeyeLUT.txt") then
		UpdateLUT
		Exit Sub
	End If

	Set ScoreFile=FileObj.GetFile(UserDirectory & "PopeyeLUT.txt") 
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)

	If (TextStr.AtEndOfStream=True) then
		Exit Sub
	End If

	LUTImage = TextStr.ReadLine

	If LUTImage = "" then
		UpdateLUT
		Exit Sub
	End If

    UpdateLUT

	Set ScoreFile = Nothing
	Set FileObj = Nothing

End Sub

Sub SaveLUT

	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End If

	If LUTImage = "" Then LUTImage = 4 ' Failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "PopeyeLUT.txt",True)
	ScoreFile.WriteLine LUTImage

	Set ScoreFile=Nothing
	Set FileObj=Nothing

End Sub

Sub NextLUT
	LUTImage = (LUTImage + 1)MOD 15
	If RenderingMode = 2 or DesktopMode = False Then
		VRLutdesc.Visible = 1
	End If
	UpdateLUT
	SaveLUT
	LUTbox.Text = "Dimmer Level: " & LUTImage + 1
End Sub

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0
	If RenderingMode = 2 or DesktopMode = False Then
		VRLutdesc.Visible = 0
	End If
End Sub

Sub UpdateLUT
    Select Case LUTImage
        Case 0:table1.ColorGradeImage = "LUT0":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase0"
        Case 1:table1.ColorGradeImage = "LUT1":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase1"
        Case 2:table1.ColorGradeImage = "LUT2":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase2"
        Case 3:table1.ColorGradeImage = "LUT3":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase3"
        Case 4:table1.ColorGradeImage = "LUT4":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase4"
        Case 5:table1.ColorGradeImage = "LUT5":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase5"
        Case 6:table1.ColorGradeImage = "LUT6":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase6"
        Case 7:table1.ColorGradeImage = "LUT7":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase7"
        Case 8:table1.ColorGradeImage = "LUT8":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase8"
        Case 9:table1.ColorGradeImage = "LUT9":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase9"
        Case 10:table1.ColorGradeImage = "LUT10":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase10"
        Case 11:table1.ColorGradeImage = "LUT11":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase11"
        Case 12:table1.ColorGradeImage = "LUT12":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase12"
        Case 13:table1.ColorGradeImage = "LUT13":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase13"
        Case 14:table1.ColorGradeImage = "LUT14":If RenderingMode = 2 or DesktopMode = False Then VRLUTdesc.imageA = "LUTcase14"
    End Select
	LUTBox.TimerEnabled = 1
End Sub

Sub LEDLightingOn

	Dim Light_Obj

	For Each Light_Obj in GILights
		Light_Obj.Color = RGB(145,224,255)
	Next

End Sub
