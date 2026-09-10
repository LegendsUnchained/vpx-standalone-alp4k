Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="potc_600as"
Const UseSolenoids=1
Const UseLamps=True,UseGI=0,SSolenoidOn="SolOn",SolenoidOff="SolOff", SCoin="coin"
Const ballmass = 1.2
Const UseVPMModSol = 1

Dim FlipperShadows, BallShadows

FlipperShadows = 0 ' set to 0 to disable flipper shadows
BallShadows = 0 ' set to 0 to disable ball shadow

LoadVPM "01560000", "sam.VBS", 3.10
Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=1
Else
Ramp16.visible=0
Ramp15.visible=0
Primitive13.visible=0
End if

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

SolCallBack(1)    = "SolTrough"
SolCallBack(2)    = "SolAutoPlungerIM"
SolCallBack(3)    =	"bsL.SolOut"
SolCallBack(4)	  = "SolChest"'CHEST LID
SolCallBack(5)	  = "SolSailsUp"'RAISE SAILS
SolCallBack(6)	  = "SolSpinner"
SolCallBack(18)   = "bsPOP.SolOut"
SolCallBack(19)	  = "SolChestExit" 'Chest Kicker
SolCallBack(21)	  = "SolShipMotor" 'Ship Motor
SolCallBack(23)   = "SolTortugaPost" 
'SolCallback(24)   = "FastFlips.TiltSol"
SolCallBack(27)	  = "SolMotorDir"'SHIP MOTOR RELAY
SolCallBack(28)   = "SolSailsDown"'LOWER SAILS LATCH
SolCallBack(29)   = "SolSHIPPIN"
SolModCallBack(20)   = "Flash20"
SolModCallBack(22)   = "Flash22"
SolModCallBack(30)   = "Flash30"
SolModCallBack(31)   = "Flash31"
SolModCallBack(32)   = "Flash32"

SolCallBack(15)	  = "SolLFlipper"
SolCallBack(16)	  = "SolRFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0,0,0,1,AudioFade(LeftFlipper)
		If LeftFlipper.CurrentAngle < 79 Then
			LeftFlipper.RampUp = 2.0
		Else
			LeftFlipper.RampUp = 0
		End If
		LeftFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_flipperDown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0,0,0,1,AudioFade(LeftFlipper):LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0,0,0,1,AudioFade(RightFlipper)
		If RightFlipper.CurrentAngle > -79 Then
			RightFlipper.RampUp = 2.0
		Else
			RightFlipper.RampUp = 0
		End If
		RightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_flipperDown",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0,0,0,1,AudioFade(RightFlipper):RightFlipper.RotateToStart
     End If
End Sub
  
'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

'Trough Ball
Sub SolTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		vpmTimer.PulseSw 22
	End If
 End Sub

Sub SolAutoPlungerIM(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
 End Sub

Sub SolSHIPPIN(Enabled)
	If Enabled Then
		ShipPin.IsDropped=0
		playsound SoundFX("Popper",DOFContactors), 0, 1, AudioPan(Gate4), 0,0,0,1,AudioFade(Gate4)
	Else
		ShipPin.IsDropped=1
		playsound SoundFX("Solon",DOFContactors), 0, 1, AudioPan(Gate4), 0,0,0,1,AudioFade(Gate4)
	End If
End Sub

Sub SolTortugaPost(Enabled)
	If Enabled Then
		TortugaPost.IsDropped=0
		playsound SoundFX("Popper",DOFContactors), 0, 1, AudioPan(sw4), 0,0,0,1,AudioFade(sw4)
	Else
		TortugaPost.IsDropped=1
		playsound SoundFX("Solon",DOFContactors), 0, 1, AudioPan(sw4), 0,0,0,1,AudioFade(sw4)
	End If
End Sub

Sub SolSpinner (Enabled)
		mDISC.MotorOn = Enabled
End Sub

Dim BallinSpin:BallinSpin=0

Sub PlaySpinSound_Timer
		If BallinSpin > 0 Then
			Playsound "SpinningDisc", -1, 1, AudioPan(Plunder), 0,0,0,1,AudioFade(Plunder)
		Else
			stopsound "SpinningDisc"
		End If
	me.enabled = 0
End Sub

Sub RotationTimer_Timer
	dim temptimer
	if mDisc.speed > 0 Then
		temptimer = int(((1/mDisc.speed)*20)+0.5)
		if temptimer<1 Then	
				RotationTimer.interval = 1
			elseif temptimer>100 then 
				RotationTimer.interval = 100
			else rotationtimer.interval=temptimer
		end if
		RotatingPlatform.RotZ = (RotatingPlatform.RotZ + 7) MOD 360
	  Else
		RotationTimer.interval = 1
		BallinSpin = 0
	end If
End Sub

'Stern-Sega GI 
set GICallback = GetRef("UpdateGI")
DayNight = Table1.NightDay

Sub UpdateGI(no, Enabled)
	If Enabled Then
		dim xx, xxx
		For each xx in GI:xx.State = 1: Next
		For each xxx in GI2:xxx.visible = 1: Next
        PlaySound "fx_relay"
	Else
		For each xx in GI:xx.State = 0: Next
		For each xxx in GI2:xxx.visible = 0: Next
        PlaySound "fx_relay"
	End If
End Sub

Dim GILevel, DayNight, xx

Sub Intensity
	If DayNight > 10 Then
	GILevel = (Round((1/DayNight),2))*12
	Else
	GILevel = 1
	End If

	For each xx in GI: xx.IntensityScale = xx.IntensityScale * (GILevel): Next
	For each xx in GI2: xx.IntensityScale = xx.IntensityScale * (GILevel): Next
End Sub

'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

Dim bsTROUGH, bsPOP, bsCHEST, bsL, mDISC

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Pirates of the Caribbean (Stern 2006)"&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 0
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

	vpmNudge.TiltSwitch=-7
	vpmNudge.Sensitivity=2
	vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot,Bumper1,Bumper2,Bumper3)

	Set bsTROUGH=New cvpmBallStack
		bsTROUGH.InitSw 0,21,20,19,18,0,0,0
		bsTROUGH.InitKick BallRelease,83,6
		bsTROUGH.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsTROUGH.Balls=4

	Set bsPOP=New cvpmBallStack
		bsPOP.InitSaucer sw56,56,75,8
		bsPOP.InitExitSnd SoundFX("Popper_ball",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsPOP.KickAngleVar = 1
		bsPOP.KickForceVar = 1

    Set bsL = New cvpmBallStack
        bsL.InitSw 0, 9, 0, 0, 0, 0, 0, 0
        bsL.InitKick sw9, 89, 20
        bsL.InitExitSnd SoundFX("Popper_ball",DOFContactors), SoundFX("Solenoid",DOFContactors)
        bsL.KickForceVar = 3

    Set mDISC=New cvpmTurnTable
		mDISC.InitTurnTable Plunder,20
		mDISC.SpinUp=1000
		mDISC.SpinDown=10
		mDISC.CreateEvents"mDISC"

 	ChestOpen.IsDropped=1
 	ShipPin.IsDropped=1
	Sink1.IsDropped=1
 	Sink2.IsDropped=1
 	SailsDown.IsDropped=1
 	TortugaPost.IsDropped=1
 	Controller.Switch(63)=1
	Intensity

	InitVpmFFlipsSAM
 End Sub

vpmMapLights AllLamps

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
    If KeyCode=7 Then vpmTimer.PulseSw 15   'Start Tournament
	If Keycode = StartGameKey Then Controller.Switch(16) = 1
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	If Keycode = StartGameKey Then Controller.Switch(16) = 0
End Sub

     ' Impulse Plunger
	Dim PlungerIM
    Const IMPowerSetting = 60
    Const IMTime = 0.6
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .CreateEvents "plungerIM"
    End With

'**********************************************************************************************************
 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub
Sub sw9a_Hit:bsL.AddBall Me: playsound "Saucerhit", 0, 1, AudioPan(sw9a), 0,0,0,1,AudioFade(sw9a): End Sub
Sub sw56_Hit:bsPOP.AddBall Me: playsound "Saucerhit", 0, 1, AudioPan(sw56), 0,0,0,1,AudioFade(sw56): End Sub

'simulated 180 metal scoop
Sub sw57_Hit:controller.Switch(57) = 1: playsound"Wire Ramp",0,1,AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall) : End Sub
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub			

'Wire Triggers
Sub sw1_Hit:Controller.Switch(1) = 1 : playsound"rollover", 0, 1, AudioPan(sw1), 0,0,0,1,AudioFade(sw1) : End Sub 
Sub sw1_UnHit:Controller.Switch(1) = 0:End Sub
Sub sw2_Hit:Controller.Switch(2) = 1 : playsound"rollover", 0, 1, AudioPan(sw2), 0,0,0,1,AudioFade(sw2) : End Sub 
Sub sw2_UnHit:Controller.Switch(2) = 0:End Sub
Sub sw23_Hit:Controller.Switch(23) = 1 : playsound"rollover", 0, 1, AudioPan(sw23), 0,0,0,1,AudioFade(sw23) : End Sub 
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
Sub sw24_Hit:Controller.Switch(24) = 1 : playsound"rollover", 0, 1, AudioPan(sw24), 0,0,0,1,AudioFade(sw24) : End Sub 
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
Sub sw25_Hit:Controller.Switch(25) = 1 : playsound"rollover", 0, 1, AudioPan(sw25), 0,0,0,1,AudioFade(sw25) : End Sub 
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
Sub sw28_Hit:Controller.Switch(28) = 1 : playsound"rollover", 0, 1, AudioPan(sw28), 0,0,0,1,AudioFade(sw28) : End Sub 
Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub
Sub sw29_Hit:Controller.Switch(29) = 1 : playsound"rollover", 0, 1, AudioPan(sw29), 0,0,0,1,AudioFade(sw29) : End Sub 
Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub

'Hidden wire Trigges
Sub sw3_Hit:Controller.Switch(3)=1 : End Sub 
Sub sw3_unHit:Controller.Switch(3)=0:End Sub
Sub sw4_Hit:Controller.Switch(4)=1:BallinSpin = BallinSpin -1:playsound"fx_ballrampdrop", 0, 1, AudioPan(sw4), 0,0,0,1,AudioFade(sw4):End Sub 			
Sub sw4_unHit:Controller.Switch(4)=0:End Sub
Sub sw6_Hit:Controller.Switch(6) = 1 : End Sub
Sub sw6_unHit:Controller.Switch(6) = 0: End Sub
Sub sw61_Hit:Controller.Switch(61)=1 : End Sub 
Sub sw61_unHit:Controller.Switch(61)=0:End Sub

'Gate Triggers
Sub sw8_Hit:vpmTimer.PulseSw 8:End Sub
Sub sw10_Hit:vpmTimer.PulseSw 10:End Sub
Sub sw11_Hit:vpmTimer.PulseSw 11:BallinSpin = BallinSpin +1:PlaySpinSound.Enabled = 1:playsound"gate", 0, 1, AudioPan(sw11), 0,0,0,1,AudioFade(sw11) : playsound"fx_ballrampdrop", 0, 1, AudioPan(sw11), 0,0,0,1,AudioFade(sw11) : End Sub
Sub sw58_Hit:vpmTimer.PulseSw 58:End Sub

'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw(30) : playsound SoundFX("fx_bumper2",DOFContactors), 0, 1, AudioPan(Bumper1), 0,0,0,1,AudioFade(Bumper1): End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(31) : playsound SoundFX("fx_bumper2",DOFContactors), 0, 1, AudioPan(Bumper2), 0,0,0,1,AudioFade(Bumper2): End Sub
Sub Bumper3_Hit : vpmTimer.PulseSw(32) : playsound SoundFX("fx_bumper2",DOFContactors), 0, 1, AudioPan(Bumper3), 0,0,0,1,AudioFade(Bumper3): End Sub

'Stand Up Targets
Sub sw37_Hit:vpmTimer.PulseSw 37:End Sub
Sub sw39_Hit:vpmTimer.PulseSw 39:End Sub
Sub sw43_Hit:vpmTimer.PulseSw 42:End Sub
Sub sw43_Hit:vpmTimer.PulseSw 43:End Sub
Sub sw44_Hit:vpmTimer.PulseSw 44:End Sub
Sub sw45_Hit:vpmTimer.PulseSw 45:End Sub
Sub sw46_Hit:vpmTimer.PulseSw 46:End Sub
Sub sw47_Hit:vpmTimer.PulseSw 47:End Sub
Sub sw50_Hit:vpmTimer.PulseSw 50:End Sub
Sub sw51_Hit:vpmTimer.PulseSw 51:End Sub
Sub sw52_Hit:vpmTimer.PulseSw 52:End Sub
Sub sw53_Hit:vpmTimer.PulseSw 53:End Sub
Sub sw54_Hit:vpmTimer.PulseSw 54:End Sub
Sub sw55_Hit:vpmTimer.PulseSw 55:End Sub

'Upper PF 
Sub sw12_Hit:Controller.Switch(12)=1 : playsound"rollover", 0, 1, AudioPan(sw12), 0,0,0,1,AudioFade(sw12) : End Sub 
Sub sw12_unHit:Controller.Switch(12)=0:End Sub
Sub sw13_Hit:vpmTimer.PulseSw 13:End Sub
Sub sw14_Hit:Controller.Switch(14)=1 : playsound"rollover", 0, 1, AudioPan(sw14), 0,0,0,1,AudioFade(sw14) : End Sub 
Sub sw14_unHit:Controller.Switch(14)=0:End Sub

'hole
Sub sw60_Hit:Controller.Switch(60)=1 : playsound"fx_ballrampdrop", 0, 1, AudioPan(sw60), 0,0,0,1,AudioFade(sw60) : End Sub
Sub sw60_unHit:Controller.Switch(60)=0:End Sub

'Generic Sounds
Sub Trigger1_Hit: playsound"fx_ballrampdrop", 0, 1, AudioPan(Trigger1), 0,0,0,1,AudioFade(Trigger1) : End Sub
Sub Trigger2_Hit: playsound"fx_ballrampdrop", 0, 1, AudioPan(Trigger2), 0,0,0,1,AudioFade(Trigger2) : End Sub
Sub Trigger3_Hit: playsound"fx_ballrampdrop", 0, 1, AudioPan(Trigger3), 0,0,0,1,AudioFade(Trigger3) : End Sub
Sub Trigger4_Hit: playsound"fx_ballrampdrop", 0, 1, AudioPan(Trigger4), 0,0,0,1,AudioFade(Trigger4) : End Sub

''************************************************************************************
''*****************       Chest                  ****************************
''************************************************************************************
 Sub SolChestExit(Enabled)
 	If Enabled Then
		ChestPin.IsDropped=1
		Playsound SoundFX("Solenoid",DOFContactors), 0, 1, AudioPan(sw6), 0,0,0,1,AudioFade(sw6)
	Else
		ChestPin.Isdropped=0
		Playsound SoundFX("Solon",DOFContactors), 0, 1, AudioPan(sw6), 0,0,0,1,AudioFade(sw6)
 	End If
 End Sub
 
 Sub SolChest(Enabled)
 	If Enabled Then
		Playsound SoundFX("Solenoid",DOFContactors), 0, 1, AudioPan(chest), 0,0,0,1,AudioFade(chest)
 		ChestOpen.IsDropped=0
 		ChestClosed.IsDropped=1
	Else
		Playsound SoundFX("Solon",DOFContactors), 0, 1, AudioPan(chest), 0,0,0,1,AudioFade(chest)
 		ChestOpen.IsDropped=1
 		ChestClosed.IsDropped=0
	 	End If
 End Sub

Dim ChestO

Sub ChestMon_Timer
	if chestopen.IsDropped = True then chesto = False else chesto = True
End Sub

Sub chestMove_Timer()
	If chesto = True and chest.RotX < 66 then chest.RotX = chest.RotX + 3
	If chesto = False and chest.RotX > 0 then chest.RotX = chest.RotX - 3
	If chest.RotX >= 0 then chesto = False
End Sub

''************************************************************************************
''*****************       Ship                 ****************************
''************************************************************************************
Dim ShipDir,ShipPos,MotorDir
ShipDir=1:ShipPos=0:MotorDir=0

 Sub SolMotorDir(Enabled)
 	If Enabled Then
 		MotorDir=1
 	Else
 		MotorDir=-1
 	End If
End Sub

Sub SolShipMotor(enabled)
	If Enabled Then
		Ship.TransZ = -7	
		ShipTimer.Enabled=True
	Else	
		ship.TransZ = 0
 	End If
End Sub

Sub ShipTimer_Timer
 	ShipPos=ShipPos+MotorDir
 	If ShipPos>0 And ShipPos<3 Then
 		Controller.Switch(62)=0
 		Controller.Switch(63)=0
 	End If
 	If ShipPos<0 Then
 		ShipPos=0
 		ShipTimer.Enabled=0
 		Controller.Switch(63)=1
 		Controller.Switch(62)=0
 	End If
 	If ShipPos>3 Then
 		ShipPos=3
 		ShipTimer.Enabled=0
 		Controller.Switch(63)=0
 		Controller.Switch(62)=1
 	End If
 	Select Case ShipPos
		Case 0:SailsUp.IsDropped=1:Sink1.IsDropped=1:Sink2.IsDropped=1:SailsDown.IsDropped=0
 		Case 1:SailsUp.IsDropped=1:SailsDown.IsDropped=1:Sink2.IsDropped=1:Sink1.IsDropped=0
		Case 2:SailsUp.IsDropped=1:SailsDown.IsDropped=1:Sink1.IsDropped=1:Sink2.IsDropped=0
	End Select
End Sub

 Sub SolSailsUp(Enabled)
 	If Enabled Then
 		If ShipPos=0 Then
 		Sink1.IsDropped=1
 		Sink2.IsDropped=1
 		SailsDown.IsDropped=1
 		SailsUp.IsDropped=0
 		End If
 	End If
 End Sub
 
 Sub SolSailsDown(Enabled)
 	If Enabled Then
 		If ShipPos=0 Then
 		Sink1.IsDropped=1
 		Sink2.IsDropped=1
 		SailsUp.IsDropped=1
 		SailsDown.IsDropped=0
 		End If
 	End If
 End Sub

Dim sailsupw, sailsdownw, sink1w, sink2w

Sub PrimMon_Timer
		if sailsup.IsDropped = false then sailsupw = True else sailsupw = False 'everything is upright
		if sailsdown.IsDropped = false then sailsdownw = True else sailsdownw = False 'ship is upright but masts are bent forward
		if sink1.IsDropped = false then sink1w = True else sink1w = False 'ship leaning back but sails facing upright
		if sink2.IsDropped = false then sink2w = True else sink2w = False 'ship is vertical and sails are bent far forward
End Sub

Sub PrimMove_Timer()
	If sailsupw = True and ship.RotX > 90 then ship.RotX = Ship.RotX - 2
	If sailsupw = True and ship.RotX < 90 then ship.RotX = Ship.RotX + 2
	If sailsupw = True and mast1.RotX > 90 then mast1.RotX = mast1.RotX - 2
	If sailsupw = True and mast1.RotX < 90 then mast1.RotX = mast1.RotX + 2
	If sailsupw = True and mast1.TransY < 0 then mast1.TransY = mast1.TransY + 2
	If sailsupw = True and mast1.TransY > 0 then mast1.TransY = mast1.TransY - 2
	If sailsdownw = True and ship.RotX > 90 then ship.RotX = Ship.RotX - 2
	If sailsdownw = True and ship.RotX < 90 then ship.RotX = Ship.RotX + 2
	If sailsdownw = True and mast1.RotX > 46 then mast1.RotX = mast1.RotX - 2
	If sailsdownw = True and mast1.RotX < 46 then mast1.RotX = mast1.RotX + 2
	If sailsdownw = True and mast1.TransY < 0 then mast1.TransY = mast1.TransY + 2
	If sailsdownw = True and mast1.TransY > 0 then mast1.TransY = mast1.TransY - 2
	If sink1w = True and ship.RotX < 134 then ship.RotX = ship.RotX + 2
	If sink1w = True and ship.RotX > 134 then ship.RotX = ship.RotX - 2
	If sink1w = True and mast1.RotX < 90 then mast1.RotX = mast1.RotX + 2
	If sink1w = True and mast1.RotX > 90 then mast1.RotX = mast1.RotX - 2
	If sink1w = True and mast1.TransY < -24 then mast1.TransY = mast1.TransY + 2
	If sink1w = True and mast1.TransY > -24 then mast1.TransY = mast1.TransY - 2
	If sink2w = True and ship.RotX < 180 then ship.RotX = ship.RotX + 2
	If sink2w = True and ship.RotX > 180 then ship.RotX = ship.RotX - 2
	If sink2w = True and mast1.RotX < 90 then mast1.RotX = mast1.RotX + 2
	If sink2w = True and mast1.RotX > 90 then mast1.RotX = mast1.RotX - 2
	If sink2w = True and mast1.TransY < -46 then mast1.TransY = mast1.TransY + 2
	If sink2w = True and mast1.TransY > -46 then mast1.TransY = mast1.TransY - 2
	If ship.RotX = 90 and mast1.RotX = 90 then sailsupw = false
	If ship.RotX = 90 and mast1.RotX = 46 then sailsdownw = false
	If ship.RotX = 135 then sink1w = false
	If ship.RotX = 180 then sink2w = false
	mast1.TransZ = (ship.Rotx / 90 -1)*50
	mast2.RotX = mast1.RotX
	mast2.TransY = mast1.TransY
	mast2.TransZ = mast1.TransZ
End Sub

'****************Flasher Lamps*************
Sub Flash20(level)
	If Level > 0 Then
		F20.IntensityScale = (level/2.55)/100
		F20.State = 1
	Else
		F20.State = 0
	End If
End Sub

Sub Flash22(level)
	If Level > 0 Then
	F22.IntensityScale = (level/2.55)/100
	F22b.IntensityScale = (level/2.55)/100
	F22c.IntensityScale = (level/2.55)/100
	F22.State = 1
	F22b.State = 1
	F22c.State = 1
	Else
	F22.State = 0
	F22b.State = 0
	F22c.State = 0
	End If
End Sub

Sub Flash30(level)
	If Level > 0 Then
	F30.IntensityScale = (level/2.55)/100
	F30b.IntensityScale = (level/2.55)/100
	F30c.IntensityScale = (level/2.55)/100
	F30d.IntensityScale = (level/2.55)/100
	F30e.IntensityScale = (level/2.55)/100
	F30.State = 1
	F30b.State = 1
	F30c.State = 1
	F30d.State = 1
	F30e.State = 1
	Else
	F30.State = 0
	F30b.State = 0
	F30c.State = 0
	F30d.State = 0
	F30e.State = 0
	End If
End Sub

Sub Flash31(level)
	If Level > 0 Then
	F31.IntensityScale = (level/2.55)/100
	F31.State = 1
	Else
	F31.State = 0
	End If
End Sub

Sub Flash32(level)
	If Level > 0 Then
	F32.IntensityScale = (level/2.55)/100
	F32.State = 1
	Else
	F32.State = 0
	End If
End Sub

Sub MiscTimer_Timer
	If FlipperShadows = 1 then
		FlipperLSh.RotZ = LeftFlipper.currentangle
		FlipperRSh.RotZ = RightFlipper.currentangle
	End If
    RampGate3.RotX = -(Gate5.currentangle +38)
    RampGate1.RotX = -(Gate6.currentangle +35)
    RampGate2.RotX = -(Gate4.currentangle +35)
	RampGate4.RotX = -(SW58.currentangle)
    RampGate5.RotX = (sw13.currentangle)
	Rampgate6.RotX = (sw8.currentangle)
	Rampgate7.RotX = (sw10.currentangle)
	Rampgate8.RotX = -(sw11.currentangle +35)
	GISpot1b.visible = GISpot1.State
	GISpot2b.visible = GISpot2.State
	GISpot3b.visible = GISpot3.State
	l24B.visible = L24.state
	L32b.visible = L32.state
	L40b.visible = L40.state
	L48b.visible = L48.state
	L56b.visible = L56.state

		If BallinSpin = 0 Then
			stopsound "SpinningDisc"
		End If
End Sub

If Flippershadows = 0 Then
	FlipperLSh.visible = 0
	FlipperRSh.visible = 0
End If

'**********************************************************************************************************
'**********************************************************************************************************
'	Start of VPX functions
'**********************************************************************************************************
'**********************************************************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    vpmTimer.PulseSw 27
    PlaySound SoundFX("right_slingshot",DOFContactors), 0, 1, AudioPan(SLING1), 0,0,0,1,AudioFade(SLING1)
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
    vpmTimer.PulseSw 26
    PlaySound SoundFX("left_slingshot",DOFContactors), 0, 1, AudioPan(SLING2), 0,0,0,1,AudioFade(SLING2)
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

Sub PlaySoundAtBallVol(sound, VolMult)
		PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
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
Dim tmpVel
Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
	tmpVel = (Round(BallVel(ball)/50,2))
		if tmpVel <.1 Then
			Vol = 0
		Elseif tmpVel >1 Then
			Vol = 1
		Else 
		Vol =  tmpVel
		End if
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

If BallShadows = 1 Then
	BallShadowUpdate.Enabled = 1
Else
	BallShadowUpdate.Enabled = 0
End If

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
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 13
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 13
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
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

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub
