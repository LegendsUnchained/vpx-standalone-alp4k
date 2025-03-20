'############################################################################################
'############################################################################################
'#######                                                                             ########
'#######          Krull          	                                                 ########
'#######          (Gottlieb 1983)                                                    ########
'#######                                                                             ########
'############################################################################################
'############################################################################################
'
' Version 1.3 kevinleedrum, mfuegemann 2014
'
' Thanks to Jean-René Karr and Josh Lehan (Krellan) for documentation scans.
' Thanks to Chris Hutchins for restoration photos.
' Thanks to Inkochnito for the DIP switch settings code
' Thanks to arngrim for integrating the DOF code
'
' =======================================================================================================
' Krull was designed by John Trudeau for Gottlieb in 1983. The cost of the machine's production combined
' with the poor box office performance of the film resulted in only ten prototypes being built.  It was
' also the last game made at Gottlieb's Northlake factory. This System 80A game utilizes three playfields,
' the lower one being viewed through a Fresnel lens. There are a total of seven flippers, four slingshots,
' six pop bumpers, four saucers, and three "shooter" kickers on this massive table.
' =======================================================================================================

Option Explicit

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim VarRol, VarHidden
VarHidden=0
If Krull.ShowDT = true then VarRol=0 Else VarRol=1

If B2SOn = true Then VarHidden=1

'-----------------------------------
'------  Global Configurations ------
'-----------------------------------
Const HideActiveLens=1	'set to 1 to hide lens above the lower playfield while it is active, 0 shows a lit lens 
Const cController = 1 '1=VPinMAME, 2=UVP, 3=B2S, 4=B2S + disable mech sounds



Const UseSolenoids=2,UseLamps=1,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SCoin="coin3"
LoadVPM "01560000","SYS80.VBS",3.1

'Sub LoadVPM(VPMver, VBSfile, VBSver)
'  On Error Resume Next
'  If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
'  ExecuteGlobal GetTextFile(VBSfile)
'  If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description
'  Select Case cController
'  Case 1
'    Set Controller = CreateObject("VPinMAME.Controller")
'	If Err Then MsgBox "Can't Load VPinMAME." & vbNewLine & Err.Description
'	If VPMver>"" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
'	If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
'  Case 2
'	Set Controller = CreateObject("UltraVP.BackglassServ")
'  Case else
'    Set Controller = CreateObject("B2S.Server")  
'  End Select
'  On Error Goto 0
'End Sub

'-----------------------------------
'------  Solenoid Assignment  ------
'-----------------------------------
SolCallback(1) = "bsHoleU.SolOut"			' HOLE (U)
SolCallback(2) = "dtSlayersM.SolDropUp"		' 2 POS RESET BANK
SolCallback(3) = "dtSlayerU.SolHit 1,"		' ROLLOVER DROP TARGET (U)
SolCallback(4) = "dtSlayersM.SolHit 2,"		' RIGHT ROLLOVER DROP TARGET
SolCallback(5) = "dtSlayerU.SolDropUp"		' 1 POS RESET BANK (U)
SolCallback(6) = "dtBankU.SolDropUp"		' 4 POS RESET BANK (U)
SolCallback(7) = "dtSlayersM.SolHit 1,"		' LEFT ROLLOVER DROP TARGET
SolCallback(8) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"	' KNOCKER
SolCallback(9) = "bsTrough.SolOut"			' OUTHOLE
SolCallback(sLLFlipper) = "FlipperLeft"
SolCallback(sLRFlipper) = "FlipperRight"

'Sub DOF(dofevent, dofstate)	
'	If cController>2 Then
'		If dofstate = 2 Then
'			Controller.B2SSetData dofevent, 1:Controller.B2SSetData dofevent, 0
'		Else
'			Controller.B2SSetData dofevent, dofstate
'		End If
'	End If
'End Sub
'********************************************************************

'Function SoundFX (sound)
'    If cController = 4 Then 'Table sounds disabled
'        SoundFX = ""  
'    Else                    'Table sounds enabled
'        SoundFX = sound
'    End If
'End Function

'--------------------------
'------  Table Init  ------
'--------------------------
Dim bsTrough,bsHoleU,dtSlayersM,dtSlayerU,dtBankU,bsRightHoleM,bsLeftHoleM,bsLeftShooterM,bsHoleL,bsLeftShooterL,bsRightShooterL
Dim InitGravity,LPFactive,MBumper1_Dir,MBumper2_Dir,UBumper_Dir,LBumper1_Dir,LBumper2_Dir,LBumper3_Dir
MBumper1_Dir = 0:MBumper2_Dir = 0:UBumper_Dir = 0:LBumper1_Dir = 0:LBumper2_Dir = 0:LBumper3_Dir = 0

Sub Krull_Init
	vpminit me
	Initgravity = Krull.gravity
	LPFactive = False
	On Error Resume Next

	Const cgamename = "krull"
    Controller.GameName=cGameName
    Controller.SplashInfoLine="Krull" & vbNewLine & "Created by kevinleedrum and mfuegemann" & vbNewLine & "VPX conversion by Rascal"
    Controller.HandleKeyboard=False
    Controller.ShowTitle=0
    Controller.ShowFrame=0
    Controller.ShowDMDOnly=0

	Controller.Hidden=1		'activate for B2S backglass use

    'DMD position for 3 Monitor Setup
    'Controller.Games(cGameName).Settings.Value("dmd_pos_x")=3850		'set this to 0 if You cannot find the DMD
    'Controller.Games(cGameName).Settings.Value("dmd_pos_y")=300		'set this to 0 if You cannot find the DMD
    'Controller.Games(cGameName).Settings.Value("dmd_width")=505
    'Controller.Games(cGameName).Settings.Value("dmd_height")=155
    Controller.Games(cGameName).Settings.Value("rol")=VarRol			

	'Controller.Games(cGameName).Settings.Value("ddraw") = 0             'set to 0 if You have problems with DMD showing or table stutter
    
    Controller.HandleMechanics=0
    Controller.Run 
    If Err Then MsgBox Err.Description
    On Error Goto 0

	Controller.SolMask(0) = 0
	vpmTimer.AddTimer 3000, "Controller.SolMask(0) = &Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 3 seconds

    PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled = true

    vpmNudge.TiltSwitch=57
    vpmNudge.Sensitivity=2
	vpmNudge.TiltObj = Array(UBumper,MBumper1,MBumper2,LBumper1,LBumper2,LBumper3,LeftslingShot,RightslingShot)
    
    vpmMapLights AllLights

	' Init Ball Stacks
	Set bsTrough = New cvpmBallStack
		bsTrough.InitNoTrough BallRelease, 67, 45, 10
		bsTrough.InitExitSnd SoundFX("BallRelease",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)

	Set bsHoleU = New cvpmBallStack
		bsHoleU.InitSaucer HoleU, 2, 130, 5
		bsHoleU.InitExitSnd SoundFX("Krull_Hole",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)

	Set bsHoleL = New cvpmBallStack
		bsHoleL.InitSaucer HoleL, 14, -80, 2
		bsHoleL.InitExitSnd SoundFX("Kicker",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)

	Set bsLeftShooterL = New cvpmBallStack
		bsLeftShooterL.InitSaucer LeftShooterL, 24, -15, 11
		bsLeftShooterL.InitExitSnd SoundFX("Kicker",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
		LeftShooterL.Createsizedball(13)
		LeftShooterL.kick 0,1

	Set bsRightShooterL = New cvpmBallStack
		bsRightShooterL.InitSaucer RightShooterL, 34, 15, 11
		bsRightShooterL.InitExitSnd SoundFX("Kicker",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)

	Set bsLeftShooterM = New cvpmBallStack
		bsLeftShooterM.InitSaucer LeftShooterM, 44, 5, 10  
		bsLeftShooterM.InitExitSnd SoundFX("Krull_Hole",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)

	Set bsLeftHoleM = New cvpmBallStack
		bsLeftHoleM.InitSaucer LeftHoleM, 54, 75, 7
		bsLeftHoleM.InitExitSnd SoundFX("Krull_Hole",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)

	Set bsRightHoleM = New cvpmBallStack
		bsRightHoleM.InitSaucer RightHoleM, 64, 70, 10
		bsRightHoleM.InitExitSnd SoundFX("Krull_Hole",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)

'	' Init Drop Targets
	Set dtSlayerU = New cvpmDropTarget
		dtSlayerU.InitDrop SlayerU, 1
		dtSlayerU.InitSnd SoundFX("TargetDrop1",DOFDropTargets), SoundFX("TargetBankReset1",DOFDropTargets)
		dtSlayerU.CreateEvents "dtSlayerU"

	Set dtBankU = New cvpmDropTarget
		dtBankU.InitDrop Array(Drop1, Drop2, Drop3, Drop4), Array(0, 10, 20, 30)
		dtBankU.InitSnd SoundFX("TargetDrop1",DOFDropTargets), SoundFX("TargetBankReset1",DOFDropTargets)
		dtBankU.CreateEvents "dtBankU"

	Set dtSlayersM = New cvpmDropTarget
		dtSlayersM.InitDrop Array(SlayerM1, SlayerM2), Array(65, 63)
		dtSlayersM.InitSnd SoundFX("TargetDrop2",DOFDropTargets), SoundFX("TargetBankReset1",DOFDropTargets)
		dtSlayersM.CreateEvents "dtSlayersM"

End Sub

Sub Krull_exit()
	Controller.Pause = False
	Controller.Stop
End Sub


'------------------------------
'------  Trough Handler  ------
'------------------------------
Sub Drain_Hit()
	bsTrough.AddBall Me
	playsound "Drain5"	
	DOF 111, 2
End Sub

'-------------------------------
'------  Keybord Handler  ------
'-------------------------------
Sub Krull_KeyDown(ByVal keycode)
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.PullBack
End Sub

Sub Krull_KeyUp(ByVal keycode)
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire
End Sub


'-----------------------
'------  Flipper  ------
'-----------------------

Sub FlipperLeft(Enabled)
	If LRelay = 1 Then
	' Lower Left Flipper
		If Enabled Then
			LowerLeftFlipper.RotateToEnd
			PlaySound SoundFX("Krull_Flipper",DOFContactors),0,1,-0.1,0.25	
		Else
			LowerLeftFlipper.RotateToStart
			PlaySound SoundFX("FlipperDown",DOFContactors),0,1,-0.1,0.25
		End If
	Else
	' Main/Upper Left Flippers
		If Enabled Then
			LeftFlipper.RotateToEnd:	ULeftFlipper.RotateToEnd:	UpperLeftFlipper.RotateToEnd
			PlaySound SoundFX("Krull_Flipper",DOFContactors),0,1,-0.1,0.25
		Else
			LeftFlipper.RotateToStart:	ULeftFlipper.RotateToStart:	UpperLeftFlipper.RotateToStart
			PlaySound SoundFX("FlipperDown",DOFContactors),0,1,-0.1,0.25
		End If
	End If
End Sub

Sub FlipperRight(Enabled)
	If LRelay = 1 Then
	' Lower Right Flipper
		If Enabled Then
			LowerRightFlipper.RotateToEnd
			PlaySound SoundFX("Krull_Flipper",DOFContactors),0,1,0.1,0.25
		Else
			LowerRightFlipper.RotateToStart
			PlaySound SoundFX("FlipperDown",DOFContactors),0,1,0.1,0.25
		End If
	' Main/Upper Right Flippers
	Else
		If Enabled Then
			RightFlipper.RotateToEnd:	UpperRightFlipper.RotateToEnd
			PlaySound SoundFX("Krull_Flipper",DOFContactors),0,1,0.1,0.25
		Else
			RightFlipper.RotateToStart:	UpperRightFlipper.RotateToStart
			PlaySound SoundFX("FlipperDown",DOFContactors),0,1,0.1,0.25
		End If
	End If
End Sub


'------------------------------
'------  Switch Handler  ------
'------------------------------

Sub Target3_Hit:vpmTimer.pulseSw 3:End Sub
Sub Trigger11_Hit:Controller.Switch(11) = 1:P_T11.TransZ=-8:PlaySound SoundFX("soloff",DOFContactors),0,0.13,0.1,0.1:End Sub
Sub Trigger11_UnHit:Controller.Switch(11) = 0:Trigger11.timerenabled=True:End Sub
Sub Target13_Hit:vpmTimer.pulseSw 13:End Sub
Sub Trigger21_Hit:Controller.Switch(21) = 1:P_T21.TransZ=-8:PlaySound SoundFX("soloff",DOFContactors),0,0.11,0.1,0.1:End Sub
Sub Trigger21_UnHit:Controller.Switch(21) = 0:Trigger21.timerenabled=True:End Sub
Sub Target23_Hit:vpmTimer.pulseSw 23:End Sub
Sub Trigger31_Hit:Controller.Switch(31) = 1:End Sub
Sub Trigger31_UnHit:Controller.Switch(31) = 0:End Sub
Sub Trigger32_Hit:Controller.Switch(32) = 1:End Sub
Sub Trigger32_UnHit:Controller.Switch(32) = 0:End Sub
Sub Trigger33_Hit:Controller.Switch(33) = 1:End Sub
Sub Trigger33_UnHit:Controller.Switch(33) = 0:End Sub
Sub Target40_Hit:vpmTimer.pulseSw 40:MoveTarget40:End Sub
Sub Target41_Hit:vpmTimer.pulseSw 41:MoveTarget41:End Sub
Sub UBumper_Hit
	vpmTimer.pulseSw 42
	DOF 110, 2
	PlaySound SoundFX("Krull_Bumper",DOFContactors),0,1,0,0.25
	UBumper_Dir = -1
	UBumper.Timerenabled = False
	UBumper.Timerenabled = True
End Sub
Sub MBumper1_Hit
	vpmTimer.pulseSw 42
	DOF 105, 2
	PlaySound SoundFX("Krull_Bumper",DOFContactors),0,1,0.1,0.25
	MBumper1_Dir = -1
	MBumper1.Timerenabled = False
	MBumper1.Timerenabled = True
End Sub
Sub MBumper2_Hit
	vpmTimer.pulseSw 42
	PlaySound SoundFX("Krull_Bumper",DOFContactors),0,1,0.1,0.25
	DOF 106, 2
	MBumper2_Dir = -1
	MBumper2.Timerenabled = False
	MBumper2.Timerenabled = True
End Sub
Sub LBumper1_Hit
	vpmTimer.pulseSw 42
	PlaySound SoundFX("Krull_Bumper",DOFContactors),0,1,0,0.25
	DOF 107, 2
	PLBumper1.image = "Bumper_Yellow"
	LBumper1_Dir = -1
	LBumper1.Timerenabled = False
	LBumper1.Timerenabled = True
End Sub
Sub LBumper2_Hit
	vpmTimer.pulseSw 42
	DOF 108, 2
	PlaySound SoundFX("Krull_Bumper",DOFContactors),0,1,0,0.25	
	PLBumper2.image = "Bumper_Yellow"
	LBumper2_Dir = -1
	LBumper2.Timerenabled = False
	LBumper2.Timerenabled = True
End Sub
Sub LBumper3_Hit
	vpmTimer.pulseSw 42
	DOF 109, 2
	PlaySound SoundFX("Krull_Bumper",DOFContactors),0,1,0.2,0.25
	PLBumper3.image = "Bumper_Yellow"
	LBumper3_Dir = -1
	LBumper3.Timerenabled = False
	LBumper3.Timerenabled = True
End Sub
Sub RightInlane_Hit:Controller.Switch(43) = 1:P_RightInlanea.TransZ=-8:P_RightInlaneb.TransZ=-8:PlaySound "soloff",0,1,0.2,0.1:End Sub
Sub RightInlane_UnHit:Controller.Switch(43) = 0:RightInlane.timerenabled=True:End Sub
Sub LeftInlane_Hit:Controller.Switch(45) = 1:P_LeftInlanea.TransZ=-8:P_LeftInlaneb.TransZ=-8:PlaySound "soloff",0,1,-0.2,0.1:End Sub
Sub LeftInlane_UnHit:Controller.Switch(45) = 0:LeftInlane.timerenabled=True:End Sub
Sub Target50_Hit:vpmTimer.pulseSw 50:MoveTarget50:End Sub
Sub Target51_Hit:vpmTimer.pulseSw 51:MoveTarget51:End Sub
Sub Target52a_Hit:vpmTimer.pulseSw 52:End Sub
Sub Target52b_Hit:vpmTimer.pulseSw 52:End Sub
Sub Target52c_Hit:vpmTimer.pulseSw 52:End Sub
Sub Target52d_Hit:vpmTimer.pulseSw 52:End Sub

Sub RightOutlane_Hit:Controller.Switch(53) = 1:P_RightOutlanea.TransZ=-8:P_RightOutlaneb.TransZ=-8:PlaySound "soloff",0,1,0.18,0.1:End Sub
Sub RightOutlane_UnHit:Controller.Switch(53) = 0:RightOutlane.timerenabled=True:End Sub
Sub LeftOutlane_Hit:Controller.Switch(55) = 1:P_LeftOutlanea.TransZ=-8:P_LeftOutlaneb.TransZ=-8:PlaySound "soloff",0,1,-0.18,0.1:End Sub
Sub LeftOutlane_UnHit:Controller.Switch(55) = 0:LeftOutlane.timerenabled=True:End Sub
Sub Target60_Hit:vpmTimer.pulseSw 60:MoveTarget60:End Sub
Sub Trigger62_Hit:Controller.Switch(62) = 1:P_T62a.TransZ=-8:P_T62b.TransZ=-8:PlaySound "soloff",0,1,-0.2,0.1:End Sub
Sub Trigger62_UnHit:Controller.Switch(62) = 0:Trigger62.timerenabled=True:End Sub

Sub LeftSlingshot_Slingshot:vpmTimer.PulseSw 52:PlaySound SoundFX("Krull_Sling",DOFContactors):DOF 101, 2:End Sub
Sub RearSlingshot_Slingshot:vpmTimer.PulseSw 52:PlaySound SoundFX("Krull_Sling",DOFContactors):DOF 103, 2:End Sub
Sub LowerSlingshot_Slingshot:vpmTimer.PulseSw 52:PlaySound SoundFX("Krull_Sling",DOFContactors):DOF 104, 2:End Sub
Sub RightSlingshot_Slingshot:vpmTimer.PulseSw 52:PlaySound SoundFX("Krull_Sling",DOFContactors):DOF 102, 2:End Sub

Sub SlayerM1_Hit:DropTargetBank.Hit 1:End Sub
Sub SlayerM2_Hit:DropTargetBank.Hit 2:End Sub
Sub SlayerU_Hit:DropTargetBank.Hit 1:End Sub

Sub HoleU_Hit:			bsHoleU.AddBall Me:End Sub
Sub LeftShooterM_Hit:	bsLeftShooterM.AddBall Me:End Sub
Sub LeftHoleM_Hit:		bsLeftHoleM.AddBall Me:End Sub
Sub RightHoleM_Hit:		bsRightHoleM.AddBall Me:End Sub
Sub LeftShooterL_Hit:	bsLeftShooterL.AddBall Me:End Sub
Sub RightShooterL_Hit:	bsRightShooterL.AddBall Me:End Sub
Sub HoleL_Hit:			bsHoleL.AddBall Me:End Sub

Sub MoveTarget40
	PlaySound SoundFX("Target1",DOFDropTargets),0,1,-0.1,0.25
	P_Target40.Transy = -5
	TimerP40.enabled = False
	TimerP40.enabled = True
End Sub
Sub TimerP40_Timer
	TimerP40.enabled = False
	P_Target40.Transy = 0
End Sub

Sub MoveTarget50
	PlaySound SoundFX("Target1",DOFDropTargets),0,1,-0.1,0.25
	P_Target50.Transy = -5
	TimerP50.enabled = False
	TimerP50.enabled = True
End Sub
Sub TimerP50_Timer
	TimerP50.enabled = False
	P_Target50.Transy = 0
End Sub

Sub MoveTarget60
	PlaySound SoundFX("Target1",DOFDropTargets),0,1,-0.1,0.25
	P_Target60.Transy = -5
	TimerP60.enabled = False
	TimerP60.enabled = True
End Sub
Sub TimerP60_Timer
	TimerP60.enabled = False
	P_Target60.Transy = 0
End Sub

Sub MoveTarget41
	PlaySound SoundFX("Target1",DOFDropTargets),0,1,-0.1,0.25
	P_Target41.Transy = -5
	TimerP41.enabled = False
	TimerP41.enabled = True
End Sub
Sub TimerP41_Timer
	TimerP41.enabled = False
	P_Target41.Transy = 0
End Sub

Sub MoveTarget51
	PlaySound SoundFX("Target1",DOFDropTargets),0,1,-0.1,0.25
	P_Target51.Transy = -5
	TimerP51.enabled = False
	TimerP51.enabled = True
End Sub
Sub TimerP51_Timer
	TimerP51.enabled = False
	P_Target51.Transy = 0
End Sub

Sub UBumper_Timer
	PURing.TransZ = PURing.TransZ + UBumper_Dir * 2.5
	if PURing.TransZ <= -30 then UBumper_Dir = 1
	if PURing.TransZ >= 0 then 
		UBumper.Timerenabled = False
		UBumper_Dir = 0		
	end if
End Sub

Sub MBumper1_Timer
	PMRing1.TransZ = PMRing1.TransZ + MBumper1_Dir * 2.5
	if PMRing1.TransZ <= -30 then MBumper1_Dir = 1
	if PMRing1.TransZ >= 0 then 
		MBumper1.Timerenabled = False
		MBumper1_Dir = 0		
	end if
End Sub

Sub MBumper2_Timer
	PMRing2.TransZ = PMRing2.TransZ + MBumper2_Dir * 2.5
	if PMRing2.TransZ <= -30 then MBumper2_Dir = 1
	if PMRing2.TransZ >= 0 then 
		MBumper2.Timerenabled = False
		MBumper2_Dir = 0		
	end if
End Sub

Sub LBumper1_Timer
	PLRing1.TransZ = PLRing1.TransZ + LBumper1_Dir * 1.4
	if PLRing1.TransZ <= -16 then LBumper1_Dir = 1
	if PLRing1.TransZ >= 0 then 
		LBumper1.Timerenabled = False
		LBumper1_Dir = 0		
		PLBumper1.image = "Bumper_Yellow_dark"
	end if
End Sub

Sub LBumper2_Timer
	PLRing2.TransZ = PLRing2.TransZ + LBumper2_Dir * 1.4
	if PLRing2.TransZ <= -16 then LBumper2_Dir = 1
	if PLRing2.TransZ >= 0 then 
		LBumper2.Timerenabled = False
		LBumper2_Dir = 0		
		PLBumper2.image = "Bumper_Yellow_dark"
	end if
End Sub

Sub LBumper3_Timer
	PLRing3.TransZ = PLRing3.TransZ + LBumper3_Dir * 1.4
	if PLRing3.TransZ <= -16 then LBumper3_Dir = 1
	if PLRing3.TransZ >= 0 then 
		LBumper3.Timerenabled = False
		LBumper3_Dir = 0		
		PLBumper3.image = "Bumper_Yellow_dark"
	end if
End Sub



'' Relays, Lamp-Driven Solenoids, Etc.
'' =======================================================================================================
Dim QRelay,TRelay,LRelay,SRelay,Strobes,OldQRelay,OldTRelay,OldLRelay,OldSRelay,OldStrobes,AuxLampStep,obj
OldQRelay = 0:OldTRelay = 0:OldLRelay = 0:OldSRelay = 0:OldStrobes = 0

'Set LampCallback = GetRef("UpdateMultipleLamps")
'Sub UpdateMultipleLamps 

Sub UpdateLampTimer_Timer
	' Hole Solenoids
	If L15x.State = 1 And bsLeftShooterM.Balls	Then bsLeftShooterM.ExitSol_On
	If L16x.State = 1 And bsLeftHoleM.Balls		Then bsLeftHoleM.ExitSol_On
	If L17x.State = 1 And bsHoleL.Balls			Then bsHoleL.ExitSol_On
	If L19x.State = 1 And bsRightHoleM.Balls	Then bsRightHoleM.ExitSol_On
	If L20x.State = 1 And bsLeftShooterL.Balls	Then bsLeftShooterL.ExitSol_On
	If L21x.State = 1 And bsRightShooterL.Balls	Then bsRightShooterL.ExitSol_On

 	QRelay = L0.State
 	TRelay = L1.State
 	LRelay = L13.State
 	SRelay = L14.State
	Strobes = L22.State

	' 'L' Relay
	If LRelay <> OldLRelay Then
 		If LRelay = 1 Then               'Lower Playfield active
			PUBumper.blenddisablelighting = 0
			PMBumper1.blenddisablelighting = 0
			PMBumper2.blenddisablelighting = 0
PLBumper1.blenddisablelighting = 2
PLBumper2.blenddisablelighting = 2
PLBumper3.blenddisablelighting = 2
Light023.state=1
Light024.state=1
Light025.state=1
Light026.state=1
 			L101.state = 0
 			If SRelay = 0 Then L102.state = 1
			DarkRamp.Widthbottom = 0
			DarkRamp.WidthTop = 0
			If HideActiveLens then
				Ramp4.Widthbottom = 0
				Ramp4.WidthTop = 0
			end if
			Krull.gravity = Initgravity * 0.7	'0.82
  			AuxLampStep = 0:AuxLamp.Enabled = 1
			LeftFlipper.rotatetostart
			RightFlipper.rotatetostart
			UpperLeftFlipper.rotatetostart
			UpperRightFlipper.rotatetostart
		Else
			PUBumper.blenddisablelighting = 2
			PMBumper1.blenddisablelighting = 2
			PMBumper2.blenddisablelighting = 2
PLBumper1.blenddisablelighting = 0
PLBumper2.blenddisablelighting = 0
PLBumper3.blenddisablelighting = 0
Light023.state=0
Light024.state=0
Light025.state=0
Light026.state=0
 			L101.state = 1
 			L102.state = 0
			DarkRamp.Widthbottom = 330
			DarkRamp.WidthTop = 330
			If HideActiveLens then
				Ramp4.Widthbottom = 330
				Ramp4.WidthTop = 330
			end if
			Krull.gravity = Initgravity
 			AuxLamp.Enabled = 0:L91.state = 0:L92.state = 0:L93.state = 0:L94.state = 0
			LowerLeftFlipper.rotatetostart
			LowerRightFlipper.rotatetostart
 		End If
 		OldLRelay = LRelay
 	End If
 	
 	' 'S' Relay
	If SRelay <> OldSRelay Then
 		If SRelay = 1 Then
 			L102.state = 0
 		Else
 			If LRelay = 1 Then L102.state = 1
 		End If
 		OldSRelay = SRelay
 	End If

 	' Tilt Relay
	If TRelay <> OldTRelay Then
 		If TRelay = 1 Then
 			L101.state = 0
 			L102.state = 0
 		Else
 			If LRelay = 0 Then L101.state = 1
 			If LRelay = 1 Then L102.state = 1
 		End If
 		OldTRelay = TRelay
 	End If
	
	' Strobes
	If Strobes <> OldStrobes Then
		If Strobes = 1 and LRelay = 0 Then
			For each obj in StrobeLamps
				obj.visible = True
			Next
		Else
			For each obj in StrobeLamps
				obj.visible = False
			Next
		End If
		OldStrobes = Strobes
	End If

	SlayerM1_Helper.isdropped = SlayerM1.isdropped			'avoid Target hits from behind

	'show lights only on active playfield
	if LRelay = 1 then										'lower PF is active
		L46a.state = L46.state
		For each obj in UpperPFGI
			obj.state = 0
		Next
		For each obj in UpperPFLights
			obj.state = 0
		Next
'  	    L30_Flasher.visible = False
'		L31_Flasher.visible = False
'		L32_Flasher.visible = False
		L33_Flasher.visible = False
		L34_Flasher.visible = False
	else													'main and upper PF are active
		L46a.state = Lightstateoff
		For each obj in UpperPFGI
			obj.state = 1
		Next
		For each obj in LowerPFLights
			obj.state = 0
		Next
'		L30_Flasher.visible = L30.state
'		L31_Flasher.visible = L31.state
'		L32_Flasher.visible = L32.state
		L33_Flasher.visible = L33.state
		L34_Flasher.visible = L34.state
	end if	

End Sub

' Auxiliary Lamp Sequence (Lower Playfield Glaives)
' =======================================================================================================
' This emulates the 10-step auxiliary lamp driver board.

Sub AuxLamp_Timer
	AuxLampStep = AuxLampStep + 1
	Select Case AuxLampStep
		Case 1: 				' Off
		Case 2:					' Off
		Case 3:	L91.state = LightstateOn	' #1 On
		Case 4:	L91.state = LightstateOff	' Off
		Case 5:	L92.state = LightstateOn	' #2 On
		Case 6:	L92.state = LightstateOff 	' Off
		Case 7:	L93.state = LightstateOn 	' #3 On
		Case 8:	L93.state = LightstateOff 	' Off
		Case 9:	L94.state = LightstateOn 	' #4 On
		Case 10:L94.state = LightstateOff 	' Off
	End Select
	If AuxLampStep = 10 Then AuxLampStep = 0
End Sub



'##############################################
'#####     Helper Functions     ###############
'##############################################

Dim ShootVel
Sub ShooterLaneLaunch_Hit
	if ActiveBall.vely < -6 then PlaySound "Launch",0,1,0.25,0.25	
	Light022.state = 2
End Sub
Sub ShooterLaneLaunch_unHit:Light022.state = 0:End Sub

Sub ShooterLaneTop_Hit
	ShootVel = SQR((ActiveBall.velx^2) + (ActiveBall.vely^2))
	if ShootVel > 6 then
		ShooterLaneTop.destroyball
		ShooterKicker.Timerenabled = True
	end if
End Sub

Sub ShooterKicker_Timer
	ShooterKicker.Timerenabled = False
	ShooterKicker.createball
	ShooterKicker.kick 350,ShootVel * 1
End Sub	

Sub RampEnd1_Hit
	if ActiveBall.VelZ > 1 then	ActiveBall.VelZ = 0
End Sub

Sub RampEnd2_Hit
	if ActiveBall.VelZ > 1 then ActiveBall.VelZ = 0
End Sub

Sub Gate1_Hit:PlaySound "Gate5",0,1,0.15,0.25:End Sub
Sub Gate4_Hit:PlaySound "Gate1",0,1,0.1,0.25:End Sub

'Flipper Primitives
Sub FlipperTimer_Timer
	LFPrim.ObjRotZ=LeftFlipper.CurrentAngle
	LFPrim1.ObjRotZ=ULeftFlipper.CurrentAngle
	RFPrim.ObjRotZ=RightFlipper.CurrentAngle
	ULFPrim.ObjRotZ=UpperLeftFlipper.CurrentAngle
	URFPrim.ObjRotZ=UpperRightFlipper.CurrentAngle
lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
lfs1.RotZ = ULeftFlipper.CurrentAngle
BallShadowUpdate
RollingUpdate
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

Sub PlaySoundAtExisting(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / Krull.width-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / Krull.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / Krull.width-1
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
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) + (ball.velz^2) ) )
End Function

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 1000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 3 ' total number of balls
ReDim rolling(tnob)
InitRolling

Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3)

Dim BallShadowTop
BallShadowTop = Array (BallshadowTop1,BallshadowTop2,BallshadowTop3)

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Dim SkillVel

Sub RollingUpdate()
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 300 Then
			StopSound("plasticroll" & b)
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        ElseIf BallVel(BOT(b) ) > 1 AND BOT(b).z > 300 Then
			StopSound("fx_ballrolling" & b)
            rolling(b) = True
            PlaySound("plasticroll" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
				StopSound("plasticroll" & b)
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
		    Next
End Sub

Sub BallShadowUpdate()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
            BallShadowTop(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 120 and BOT(b).Z < 130 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next	
For b = 0 to UBound(BOT)
		BallShadowTop(b).X = BOT(b).X
		BallShadowTop(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 130 and BOT(b).Z < 200 Then
            BallShadowTop(b).visible = 1
        Else
            BallShadowTop(b).visible = 0
        End If
    Next	
End Sub



'Rollover Switches
Sub Trigger11_Timer
	Trigger11.timerenabled = False
	P_T11.transz = 0
End Sub
Sub Trigger21_Timer
	Trigger21.timerenabled = False
	P_T21.transz = 0
End Sub
Sub Trigger62_Timer
	Trigger62.timerenabled = False
	P_T62a.transz = 0
	P_T62b.transz = 0
End Sub
Sub LeftOutlane_Timer
	LeftOutlane.Timerenabled = False
	P_LeftOutlanea.transz = 0
	P_LeftOutlaneb.transz = 0
End Sub
Sub LeftInlane_Timer
	LeftInlane.Timerenabled = False
	P_LeftInlanea.transz = 0
	P_LeftInlaneb.transz = 0
End Sub
Sub RightInlane_Timer
	RightInlane.Timerenabled = False
	P_RightInlanea.transz = 0
	P_RightInlaneb.transz = 0
End Sub
Sub RightOutlane_Timer
	RightOutlane.Timerenabled = False
	P_RightOutlanea.transz = 0
	P_RightOutlaneb.transz = 0
End Sub


'Krull dip-setting menu
'added by Inkochnito
Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700,400,"Krull - DIP switches"
		.AddFrame 2,2,190,"Maximum credits",49152,Array("8 credits",0,"10 credits",32768,"15 credits",&H00004000,"20 credits",49152)'dip 15&16
		.AddFrame 2,80,190,"Coin chute 1 and 2 control",&H00002000,Array("seperate",0,"same",&H00002000)'dip 14
		.AddFrame 2,126,190,"Playfield special",&H00200000,Array("replay",0,"extra ball",&H00200000)'dip 22
		.AddFrame 2,172,190,"Game mode",&H10000000,Array("replay",0,"extra ball",&H10000000)'dip 29
		.AddFrame 2,218,190,"B-E-A-S-T replays",&H80000000,Array("1 replay",0,"2 replays",&H40000000)'dip 32
		.AddFrame 205,2,190,"High score to date awards",&H00C00000,Array("not displayed and no award",0,"displayed and no award",&H00800000,"displayed and 2 replays",&H00400000,"displayed and 3 replays",&H00C00000)'dip 23&24
		.AddFrame 205,80,190,"Balls per game",&H01000000,Array("5 balls",0,"3 balls",&H01000000)'dip 25
		.AddFrame 205,126,190,"Replay limit",&H04000000,Array("no limit",0,"one per ball",&H04000000)'dip 27
		.AddFrame 205,172,190,"Novelty",&H08000000,Array("normal",0,"points",&H08000000)'dip 28
		.AddFrame 205,218,190,"3rd coin chute credits control",&H20000000,Array("no effect",0,"add 9",&H20000000)'dip 30
		.AddChk 2,273,190,Array("Match feature",&H02000000)'dip 26
		.AddLabel 50,290,300,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub
Set vpmShowDips = GetRef("editDips")



'#################################################################################

' =======================================================================================================
' KRULL (Gottlieb 1983) 
'
' SWITCHES:
' 0 #1 DROP TARGET (U)
' 1 R.O. DROP TARGET (U)
' 2 HOLE (U)
' 3 #1 SPOT TARGET (L)
' 7 TEST

' 10 #2 DROP TARGET (U)
' 11 LEFT ROLLOVER (U)
' 13 #2 SPOT TARGET (L)
' 14 RIGHT HOLE (L)
' 17 COIN CHUTE 1

' 20 #3 DROP TARGET (U)
' 21 RIGHT ROLLOVER (U)
' 23 #3 SPOT TARGET (L)
' 24 LEFT SHOOTER (L)
' 27 COIN CHUTE 2

' 30 #4 DROP TARGET (U)
' 31 ROLLUNDER (U)
' 32 ROUNDABOUT (L)
' 33 ROLLUNDER (L)
' 34 RIGHT SHOOTER (L)
' 37 COIN CHUTE 3

' 40 #1 SPOT TARGET
' 41 #4 SPOT TARGET
' 42 POP BUMPERS (6)
' 43 RIGHT RETURN ROLLOVER
' 44 LEFT SHOOTER
' 45 LEFT RETURN ROLLOVER
' 47 START

' 50 #2 SPOT TARGET
' 51 #5 SPOT TARGET
' 52 10 POINT (8)
' 53 RIGHT OUTSIDE ROLLOVER
' 54 LEFT HOLE
' 55 LEFT OUTSIDE ROLLOVER
' 57 TILT

' 60 #3 SPOT TARGET
' 62 LEFT SIDE ROLLOVER
' 63 RIGHT ROLLOVER DROP TARGET
' 64 RIGHT HOLE
' 65 LEFT ROLLOVER DROP TARGET
' 67 OUTHOLE

' 112 LEFT FLIPPER
' 114 RIGHT FLIPPER

