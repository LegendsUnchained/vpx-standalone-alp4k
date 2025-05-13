Option Explicit
Randomize

Const BallSize = 51
Const BallMass = 1.3

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0


Dim DesktopMode:DesktopMode = JM.ShowDT
Dim UseVPMDMD:UseVPMDMD = DesktopMode
Const UseVPMModSol = 0

 Const cGameName = "jm_12r" 

LoadVPM "02000000", "WPC.VBS", 3.50

 NoUpperRightFlipper
 NoUpperLeftFlipper
 Const UseSolenoids = 2
 Const UseLamps = True
 Const UseSync = True
 Const HandleMech = False
 Const UseGI = False
 Const SSolenoidOn   = "fx_solenoid"
 Const SSolenoidOff  = ""
 Const SFlipperOn    = ""
 Const SFlipperOff   = ""
 Const SCoin         = "coin"

if JM.ShowDT = false then SideRailL.visible = 0:SideRailR.visible = 0

'************************************************************************
'						 INIT TABLE
'************************************************************************

 Dim bsTrough,bsCrazyBobs,bsKickToGlove,dtDrop,MoveGloveX,MoveGloveY,PlungerIM

 Sub JM_Init()
 	vpmInit Me
		With Controller
			.GameName = cGameName
			If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
			.SplashInfoLine = "Johnny Mnemonic, Williams 1995" & vbNewLine & "Alessio"
			.HandleMechanics = 0
			.HandleKeyboard = False
			.ShowDMDOnly = True 
			.ShowFrame = False 
			.ShowTitle = False
			.Hidden= 0   'DesktopMode
			.Run GetPlayerHWnd
			If Err Then MsgBox Err.Description
			On Error Goto 0
			.Switch(22)=1				'Close coin door
			.Switch(24)=0				'Always Closed
		End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

	' Nudging
 	vpmNudge.TiltSwitch = 14
 	vpmNudge.Sensitivity = 6
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)
	
    ' Trough
    Set bsTrough = New cvpmTrough
    With bsTrough
		.size = 4
		.initSwitches Array(32, 33, 34, 35)
		.Initexit BallRelease, 90, 8
		.InitExitSounds SoundFX(SSolenoidOn,DOFContactors), SoundFX("BallRelease",DOFContactors)
		.Balls = 4
		.CreateEvents "bsTrough", Drain
    End With

	' Crazy Bob's Eject
	Set bsCrazyBobs = New cvpmSaucer
	With bsCrazyBobs
		.InitKicker CrazyBobs, 47, 147.5, 5, 0
        .InitExitVariance 7.5, 2
		.InitSounds "kicker_enter_center", SoundFX(SSolenoidOn,DOFContactors), SoundFX("ExitKicher",DOFContactors)
		.CreateEvents "bsCrazyBobs", CrazyBobs
	End With

	' KickToGlove Popper
	Set bsKickToGlove = New cvpmSaucer
	With bsKickToGlove
		.InitKicker KickToGlove, 36, 0, 35, 1.56
		.InitSounds "hole_enter", SoundFX(SSolenoidOn,DOFContactors), SoundFX("Magnete",DOFContactors)
		.CreateEvents "bsKickToGlove", KickToGlove
	End With

	'Drop Target
	Set dtDrop=New cvpmDropTarget
	With dtDrop
		.InitDrop DropTarget,43
		.Initsnd SoundFX("DropTargetDown",DOFDropTargets), SoundFX("DropTargetUp",DOFDropTargets)
	End With

	'Autoplunger
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swplunger, 55, 0.6
		.Random 0.3
		.InitExitSnd SoundFX("plunger2",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
		.CreateEvents "plungerIM"
	End With

	UpdateGI 0,0:UpdateGI 1,0:UpdateGI 2,0
	InitGlove
	LeftDiverterOpen.IsDropped = True: ScambioSinistroAperto
 	LeftDiverterClosed.IsDropped = False
  	RightDiverterOpen.IsDropped = False
 	RightDiverterClosed.IsDropped = True : ScambioDestroChiuso

'debug!
'	Matrix31.CreateBall
'	Matrix32.CreateBall
'	Matrix33.CreateBall
'	Matrix21.CreateBall
'	Matrix22.CreateBall
'	Matrix13.CreateBall
End Sub

'************************************************************************
'							KEYS
'************************************************************************

Sub JM_KeyDown(ByVal KeyCode)
	If KeyCode=KeyFront Then Controller.Switch(23)=1
	If KeyCode=PlungerKey Then Controller.Switch(11)=1
	If KeyCode=rightmagnasave Then Controller.Switch(67)=1 'pos Y
	If KeyCode=leftmagnasave Then Controller.Switch(68)=1 'neg Y
    If keycode = LeftTiltKey Then Nudge 90, 4:PlaySoundAt SoundFX("fx_nudge",0), sw_15a
	If keycode = RightTiltKey Then Nudge 270, 4:PlaySoundAt SoundFX("fx_nudge",0), sw_18a
    If keycode = CenterTiltKey Then Nudge 0, 5:PlaySoundAt SoundFX("fx_nudge",0), Drain
	If KeyDownHandler(KeyCode) Then Exit Sub
End Sub

Sub JM_KeyUp(ByVal KeyCode)
	If KeyCode=KeyFront Then Controller.Switch(23)=0
	If KeyCode=PlungerKey Then Controller.Switch(11)=0
	If KeyCode=rightmagnasave Then Controller.Switch(67)=0 'pos Y
	If KeyCode=leftmagnasave  Then Controller.Switch(68)=0 'neg Y

	If KeyUpHandler(KeyCode) Then Exit Sub
End Sub

Sub JM_Paused:Controller.Pause = True:End Sub
Sub JM_unPaused:Controller.Pause = False:End Sub
Sub JM_exit():Controller.Pause = False:Controller.Stop:End Sub

'****************************************
'            GENERAL ILLUMINATION
'****************************************

Set GiCallBack2 = GetRef("UpdateGi")

Sub UpdateGi(nr,step)
	Dim ii
If Step>=7 Then JM.ColorGradeImage = "ColorGradeLUT256x16_extraConSat":Else JM.ColorGradeImage = "ColorGrade_" & (step+1):End If
	Select Case nr

	Case 0 		'Left Playfield
		If step=0 Then
			For each ii in GI_Left:ii.state=0:Next
		Else
			For each ii in GI_Left:ii.state=1:Next
		End If
		For each ii in GI_Left:ii.IntensityScale = 0.125 * step:Next

	Case 1		'Right Playfield
		If step=0 Then
			For each ii in GI_Right:ii.state=0:Next
		Else
			For each ii in GI_Right:ii.state=1:Next
		End If
		For each ii in GI_Right:ii.IntensityScale = 0.125 * step:Next

	Case 2		'Top Playfield
		If step=0 Then
			For each ii in GI_Top:ii.state=0:Next
		Else
			For each ii in GI_Top:ii.state=1:Next
		End If
		For each ii in GI_Top:ii.IntensityScale = 0.125 * step:Next
		If step>4 Then DOF 103, DOFOn : Else DOF 103, DOFOff:End If

	Case 3		'Insert 1

	Case 4		'Insert 2

	End Select
End Sub

'****************************************
'             SOLENOIDS MAP
'****************************************

SolCallback(1)	= "SolBallRelease"
SolCallback(2)	= "Auto_Plunger"
SolCallBack(3)	= "bsKickToGlove.SolOut"
SolCallBack(6)	= "DoHandMagnet"
SolCallBack(5)	= "DoClearMatrix"
SolCallback(7)	= "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(14)	= "bsCrazyBobs.SolOut"
SolCallback(15)	= "dtDrop.SolDropUp"
SolCallback(16)	= "dtDrop.SolDropDown"
SolCallback(17) = "setLamp 117,"
SolCallback(18) = "setLamp 118,"
SolCallback(19) = "setLamp 119,"
SolCallback(20) = "setLamp 120,"
SolCallback(25) = "setLamp 125,"
SolCallback(26) = "setLamp 126,"
SolCallback(27) = "setLamp 127,"
SolCallback(28) = "setLamp 128,"
SolCallBack(34)	= "SolLeftDiverterHold"
SolCallBack(36)	= "SolRightDiverterHold"

SolCallback(sLLFlipper)="SolLFlipper"
SolCallback(sLRFlipper)="SolRFlipper"

'******************************************
'				FLIPPERS
'******************************************

Sub SolLFlipper(Enabled)
	If Enabled Then		 
		PlaySoundAt SoundFX ("FlipperL",DOFFlippers), LeftFlipper
		LeftFlipper.RotateToEnd
	Else
        PlaySoundAt SoundFX ("FlipperGiu",DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
     End If
 End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("FlipperR",DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("FlipperGiu",DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
     End If
 End Sub

'************************************************************************
'						 BALL RELEASE
'************************************************************************

Sub SolBallRelease(Enabled)
    If Enabled Then
        If bsTrough.Balls Then vpmTimer.PulseSw 31
        bsTrough.ExitSol_On
    End If
End Sub

'************************************************************************
'						 AUTOPLUNGER 
'************************************************************************

 Sub Auto_Plunger(Enabled)
 If Enabled Then
	PlungerIM.AutoFire
 End If
 End Sub

'************************************************************************
'						 GLOVE POPPER
'************************************************************************

Dim thePopBall
Sub TriggerGloveMag_Hit
	TriggerGloveMag.DestroyBall
	GloveMag.DestroyBall ' Paranoia 
	GloveMag.Enabled = 1
	Set thePopBall = GloveMag.CreateBall
	thePopBall.Y = (450 - MoveGloveY.Position) - 12
	thePopBall.X = MoveGloveX.Position + 308
	Controller.Switch(115)=1
End Sub

'************************************************************************
'						 GLOVE MAGNET
'************************************************************************

Sub CheckPickup(x, y, obj, sw)
	if (x > obj.x - 25 AND x < obj.x + 25 AND y > obj.y - 25 AND y < obj.y +25) then 
		obj.DestroyBall
		GloveMag.DestroyBall ' Paranoia 
		GloveMag.Enabled = 1
		Set thePopBall = GloveMag.CreateBall
		Controller.Switch(sw)=0
		PlaySoundAt SoundFX("Magnete",DOFContactors), obj 
		thePopBall.x = x
		thePopBall.y = y 
		Controller.Switch(115)=1
	Else	
		if Controller.Switch(sw) then 
			Debug.Print "MISS x:" & obj.x & " y:" & obj.y & " objx:" & obj.x & " objy:" & obj.y 
		end if 
	end if 
end sub 

 Sub DoHandMagnet(enabled)
	If Enabled and MoveGloveX.Position > 200 Then 
		' Are we above the matrix?   If yes, find the kicker with the ball..
		' Assert: the ROM will only attempt this when there is a ball sitting in the matrix (switch enabled)
		Dim hx:hx = MoveGloveX.Position + 308
		dim hy:hy = (450 - MoveGloveY.Position) - 12
		'debug.print "X:" & hx & " Y:" & hy & " m33x" & Matrix33.x & " m33y " & Matrix33.y 

		'... These are the drop-at numbers with x = 330 + XPos and y = (450-Pos) 
		'Drop at 797,347
		'Drop at 716,260 - ' ** Center ** want 694, 248 , so adjusted to +308, - 12
		'Drop at 634,180
		'Drop at 787,354  ' Actual 776, 338 ... These are the drop-at numbers with x = 330 + XPos and y = (450-Pos) 
		'Drop at 708,273  ' Actual 694, 248 ... ROM Spacing is X=79, Y=78-ish (81 to 74?! )  
		'Drop at 628,199  ' Actual 611, 165 .... Table spacing 82, 86

		CheckPickup hx, hy, Matrix32, 63
		CheckPickup hx, hy, Matrix33, 73		
		CheckPickup hx, hy, Matrix22, 62
		CheckPickup hx, hy, Matrix23, 72
		CheckPickup hx, hy, Matrix12, 61
		CheckPickup hx, hy, Matrix13, 71
		CheckPickup hx, hy, Matrix31, 53
		CheckPickup hx, hy, Matrix21, 52
		CheckPickup hx, hy, Matrix11, 51

	end if 
		
	If NOT Enabled Then
		If NOT IsEmpty(thePopBall) Then
			 Debug.Print "Drop at " & thePopBall.x & ","  &thePopBall.y
			GloveMag.Kick 0,0
			GloveMag.Enabled = 0
			thePopBall.VelY = 0
			thePopBall=Empty
			Controller.Switch(115)=0
 		End If
 	End if
 End Sub

'************************************************************************
'						 CLEAR MATRIX
'************************************************************************

Sub DoClearMatrix(enabled)
 	If enabled then
  		Matrix31.Enabled = False
 		Matrix32.Enabled = False
 		Matrix33.Enabled = False

		If Controller.Switch(53) then
			Matrix31.Kick 180,6
			Controller.Switch(53)=0
		End If 
		If Controller.Switch(63) then
			Matrix32.Kick 180,6
			Controller.Switch(63)=0	
		End If
		If Controller.Switch(73) then
			Matrix33.Kick 180,6
			Controller.Switch(73)=0
		End If

 		Matrix21.Enabled = False
 		Matrix22.Enabled = False
 		Matrix23.Enabled = False

		If Controller.Switch(52) then
			Matrix21.Kick 180,6
			Controller.Switch(52)=0	
		End If
		If Controller.Switch(62) Then			
			Matrix22.Kick 180,6
			Controller.Switch(62)=0	
		End If
		If Controller.Switch(72) then
			Matrix23.Kick 180,6
			Controller.Switch(72)=0
		End If

  		Matrix11.Enabled = False
 		Matrix12.Enabled = False
 		Matrix13.Enabled = False

		If Controller.Switch(51) then 
			Matrix11.Kick 180,6
			Controller.Switch(51)=0	
		End If
		If Controller.Switch(61) then
			Matrix12.Kick 180,6
			Controller.Switch(61)=0	
		End If
		If Controller.Switch(71) then
			Matrix13.Kick 180,6
			Controller.Switch(71)=0	
		End If

		AlzaMatrix
		ReEnableMatrix.Enabled = True
 	end if
 End Sub
 
 Sub ReEnableMatrix_Timer()
  	Matrix31.Enabled = True
 	Matrix32.Enabled = True
 	Matrix33.Enabled = True
 	Matrix21.Enabled = True
 	Matrix22.Enabled = True
 	Matrix23.Enabled = True
 	Matrix11.Enabled = True
 	Matrix12.Enabled = True
 	Matrix13.Enabled = True
 	ReEnableMatrix.Enabled = False
End Sub

Sub MatrixAbbassato_Timer()
	 AbbassaMatrix
	 Me.Enabled=0
 End Sub

 Sub AlzaMatrix
	 Griglia.TransY = 43
	 Griglia.ObjRotX = -5
	 PianoGriglia.TransY = 43
	 PianoGriglia.ObjRotX = -4
	 BucoAlto1.TransY = 35
	 BucoAlto1.ObjRotX = -5
	 BucoAlto2.TransY = 35
	 BucoAlto2.ObjRotX = -5
	 BucoAlto3.TransY = 35
	 BucoAlto3.ObjRotX = -5
	 BucoMedio1.TransY = 28
	 BucoMedio1.ObjRotX = -5
	 BucoMedio2.TransY = 28
	 BucoMedio2.ObjRotX = -5
	 BucoMedio3.TransY = 28
	 BucoMedio3.ObjRotX = -5
	 BucoBasso1.TransY = 20
	 BucoBasso1.ObjRotX = -5
	 BucoBasso2.TransY = 20
	 BucoBasso2.ObjRotX = -5
	 BucoBasso3.TransY = 20
	 BucoBasso3.ObjRotX = -5
	 MatrixAbbassato.Enabled=True
	 PlaysoundAt SoundFX ("MatrixSound",DOFContactors), Griglia
 End Sub

  Sub AbbassaMatrix
	 Griglia.TransY = 0
	 Griglia.ObjRotX = 0
	 PianoGriglia.TransY = 0
	 PianoGriglia.ObjRotX = 0
	 BucoAlto1.TransY = 0
	 BucoAlto1.ObjRotX = 0
	 BucoAlto2.TransY = 0
	 BucoAlto2.ObjRotX = 0
	 BucoAlto3.TransY = 0
	 BucoAlto3.ObjRotX = 0
	 BucoMedio1.TransY = 0
	 BucoMedio1.ObjRotX = 0
	 BucoMedio2.TransY = 0
	 BucoMedio2.ObjRotX = 0
	 BucoMedio3.TransY = 0
	 BucoMedio3.ObjRotX = 0
	 BucoBasso1.TransY = 0
	 BucoBasso1.ObjRotX = 0
	 BucoBasso2.TransY = 0
	 BucoBasso2.ObjRotX = 0
	 BucoBasso3.TransY = 0
	 BucoBasso3.ObjRotX = 0
	 PlaysoundAt SoundFX ("MatrixSound",DOFContactors), Griglia
 End Sub

'************************************************************************
'						 DIVERTERS
'************************************************************************

 Sub SolLeftDiverterHold(enabled)
 	PlaySoundAt SoundFX ("diverter",DOFContactors), LeftDiverter
 	If enabled then
 		LeftDiverterClosed.IsDropped = False
 		LeftDiverterOpen.IsDropped = True
		ScambioSinistroAperto
 	else
 		LeftDiverterClosed.IsDropped = True
 		LeftDiverterOpen.IsDropped = False
		ScambioSinistroChiuso
 	end if
 End Sub

 Sub ScambioSinistroAperto
	 LeftDiverter.TransX= 0
	 LeftDiverter.TransZ= 0
	 LeftDiverter.ObjRotZ= 0
 End Sub

 Sub ScambioSinistroChiuso
	 LeftDiverter.TransX= 68
	 LeftDiverter.TransZ= 105
	 LeftDiverter.ObjRotZ= 33
 End Sub

 Sub SolRightDiverterHold(enabled)
 	PlaySoundAt SoundFX ("diverter",DOFContactors), RightDiverter
 	If enabled then
 		RightDiverterClosed.IsDropped = False
 		RightDiverterOpen.IsDropped = True
		ScambioDestroAperto
 	else
 		RightDiverterClosed.IsDropped = True
 		RightDiverterOpen.IsDropped = False
		ScambioDestroChiuso
 	end if
 End Sub
 
 Sub ScambioDestroAperto
	 RightDiverter.TransX= -113
	 RightDiverter.TransZ= -168
	 RightDiverter.ObjRotZ= -35
 End Sub

 Sub ScambioDestroChiuso
	 RightDiverter.TransX= 0
	 RightDiverter.TransZ= 0
	 RightDiverter.ObjRotZ= 0
 End Sub

'****************************************
'SWITCHES
'****************************************

'**Drop Target
Sub DropTarget_Hit():PlaySoundAt "Muro", ActiveBall:End Sub
Sub DropTarget_dropped():dtDrop.Hit 1:End Sub

'**SLINGSHOTS

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 25
	PlaySoundAt SoundFX("Sling",DOFContactors), ActiveBall
	LSling.Visible = 0
	LSling1.Visible = 1
	sling1.TransZ = -27
	LStep = 0
	Me.TimerInterval = 50: Me.TimerEnabled = 1
End Sub

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 26
	PlaySoundAt SoundFX("Sling",DOFContactors), ActiveBall
	RSling.Visible = 0
	RSling1.Visible = 1
	sling2.TransZ = -27
	RStep = 0
	Me.TimerInterval = 50: Me.TimerEnabled = 1
End Sub

 Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling1.TransZ = -15
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling1.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
 End Sub

 Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling2.TransZ = -15
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling2.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
 End Sub

Sub sw_38_Hit
	vpmTimer.PulseSw 38
End Sub

'**MATRIX 

Sub Matrix11_Hit()
 	Controller.Switch(51)=1
 	PlaySoundAt "EnterHole", ActiveBall
End Sub

Sub Matrix21_Hit()
 	Controller.Switch(52)=1
 	PlaySoundAt "EnterHole", ActiveBall
End Sub

Sub Matrix31_Hit()
 	Controller.Switch(53)=1
 	PlaySoundAt "EnterHole", ActiveBall
End Sub

Sub Matrix12_Hit()
 	Controller.Switch(61)=1
 	PlaySoundAt "EnterHole", ActiveBall
End Sub

Sub Matrix22_Hit()
 	Controller.Switch(62)=1
 	PlaySoundAt "EnterHole", ActiveBall
End Sub

Sub Matrix32_Hit()
 	Controller.Switch(63)=1
 	PlaySoundAt "EnterHole", ActiveBall
End Sub

Sub Matrix13_Hit()
 	Controller.Switch(71)=1
 	PlaySoundAt "EnterHole", ActiveBall
End Sub

Sub Matrix23_Hit()
 	Controller.Switch(72)=1
 	PlaySoundAt "EnterHole", ActiveBall
End Sub

Sub Matrix33_Hit()
 	Controller.Switch(73)=1
 	PlaySoundAt "EnterHole", ActiveBall
End Sub

'**BUMPERS
Sub Bumper1_Hit:vpmTimer.PulseSw 44:PlaySoundAt SoundFX("BumperSinistro",DOFContactors), ActiveBall:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 45:PlaySoundAt SoundFX("BumperCentrale",DOFContactors), ActiveBall:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 46:PlaySoundAt SoundFX("BumperDestro",DOFContactors), ActiveBall:End Sub

'**Spinner
Sub RetinaSpinner_spin:vpmTimer.PulseSw 48:PlaySoundAt "spinner", RetinaSpinner: End Sub

'**BOTTOM LANE rollovers
Sub Sw_17a_Hit():Controller.Switch(17) = 1:PlaySoundAt "sensor", ActiveBall:End Sub
Sub Sw_17a_UnHit():Controller.Switch(17) = 0:End Sub

Sub Sw_16a_Hit():Controller.Switch(16) = 1:PlaySoundAt "sensor", ActiveBall:End Sub
Sub Sw_16a_UnHit():Controller.Switch(16) = 0:End Sub

Sub Sw_15a_Hit():Controller.Switch(15) = 1:PlaySoundAt "sensor", ActiveBall:End Sub
Sub Sw_15a_UnHit():Controller.Switch(15) = 0:End Sub

Sub Sw_18a_Hit():Controller.Switch(18) = 1:PlaySoundAt "sensor", ActiveBall:End Sub
Sub Sw_18a_UnHit():Controller.Switch(18) = 0:End Sub

Sub sw78_hit:Controller.Switch (78) = 1 : PlaySoundAt "sensor", ActiveBall:End Sub
Sub sw78_Unhit:Controller.Switch (78) = 0 : End Sub

'**JET LANE rollovers
Sub Sw_64a_Hit():Controller.Switch(64) = 1:PlaySoundAt "sensor", ActiveBall:End Sub
Sub Sw_64a_UnHit():Controller.Switch(64) = 0:End Sub

Sub Sw_65a_Hit():Controller.Switch(65) = 1:PlaySoundAt "sensor", ActiveBall:End Sub
Sub Sw_65a_UnHit():Controller.Switch(65) = 0:End Sub

Sub Sw_66a_Hit():Controller.Switch(66) = 1:PlaySoundAt "sensor", ActiveBall:End Sub
Sub Sw_66a_UnHit():Controller.Switch(66) = 0:End Sub

'**Targets
Sub sw_27_Hit:Controller.Switch(27) = 1:PlaySoundAt SoundFX("Target",DOFTargets), ActiveBall:End Sub
Sub sw_27_UnHit():Controller.Switch(27) = 0:End Sub

Sub sw_28_Hit:Controller.Switch(28) = 1:PlaySoundAt SoundFX("Target",DOFTargets), ActiveBall:End Sub
Sub sw_28_UnHit():Controller.Switch(28) = 0:End Sub

'**Left Ramp
Sub sw_41_Hit()
	Controller.Switch(41) = 1
	PlaySoundAt "gate", ActiveBall
	If ActiveBall.velY < 0  Then
	PlaysoundAt "RampaIN", ActiveBall
	Else
	StopSound "RampaIN"
	End If
End Sub

Sub sw_41_UnHit():Controller.Switch(41) = 0:End Sub

Sub sw_42_Hit():Activeball.VelY=0:Controller.Switch(42) = 1:PlaysoundAt "plasticrolling", ActiveBall:End Sub
Sub sw_42_UnHit():Controller.Switch(42) = 0:End Sub

'**Right Ramp
Sub sw_55_Hit():Controller.Switch(55) = 1:PlaysoundAt "plasticrolling", ActiveBall:End Sub
Sub sw_55_UnHit():Controller.Switch(55) = 0:End Sub

Sub sw_54_Hit()
	Controller.Switch(54) = 1
	PlaySoundAt "gate", ActiveBall
	If ActiveBall.velY < 0  Then
	PlaysoundAt "RampaIN", ActiveBall
	Else
	StopSound "RampaIN"
	End If
End Sub

Sub sw_54_UnHit():Controller.Switch(54) = 0:End Sub

'**Left Loop
Sub sw_56_Hit():Controller.Switch(56) = 1:PlaySoundAt "gate", ActiveBall:End Sub
Sub sw_56_UnHit():Controller.Switch(56) = 0:End Sub

'**Right Loop
Sub Sw_57a_Hit():Controller.Switch(57) = 1:PlaySoundAt "sensor", ActiveBall:End Sub 
Sub Sw_57a_UnHit():Controller.Switch(57) = 0:End Sub

'**Inner Loop
Sub Sw_58a_Hit():Controller.Switch(58) = 1:PlaySoundAt "sensor", ActiveBall:End Sub 
Sub Sw_58a_UnHit():Controller.Switch(58) = 0:End Sub

'**BALL STOPPER

Sub DropRampLeft_Hit()
	Playsound "PlasticDrop"
End Sub

Sub DropRampRight_Hit()
	Playsound "PlasticDrop"
End Sub

Sub SX_BallStop_Hit()   
 	 ActiveBall.VelZ = -2
     ActiveBall.VelY = 0
     ActiveBall.VelX = 0
	 StopSound "plasticrolling"
	 CadutaSuTavolo.Enabled=1
End Sub

Sub DX_BallStop_Hit()
 	 ActiveBall.VelZ = -2
	 ActiveBall.VelY = 0
     ActiveBall.VelX = 0
	 StopSound "plasticrolling"
	 CadutaSuTavolo.Enabled=1
End Sub

Sub CadutaSuTavolo_Timer()
	PlaysoundAt "balldrop", DX_BallStop
	Me.Enabled=0
End Sub

Sub CadutaDaMano_Hit()
	PlaysoundAt "BallDropHand", ActiveBall
End Sub

Sub DrainGame_Hit()
	PlaysoundAt "DrainBefore", ActiveBall
	DopoDrain.enabled = True
End Sub

Sub DopoDrain_Timer()
	PlaysoundAt "drain", DrainGame
	DopoDrain.enabled = False
End Sub

Sub RampBlockTrigger_Hit()
'msgbox(activeball.VelX & "$" & activeball.VelY & "$" & activeball.VelZ)
	if activeball.VelY<-70 then
		activeball.VelY=activeball.VelY/2
		activeball.VelX=activeball.VelX/2
		activeball.VelZ=activeball.VelZ/2
	end if	
'msgbox(activeball.VelX & "$" & activeball.VelY & "$" & activeball.VelZ)
End Sub

'********************************
'			GLOVE ANIMATION
'********************************

Sub InitGlove
	Set MoveGloveX = New cvpmMech
	With MoveGloveX 
		.Sol1=22
		.Sol2=-21
		.MType=vpmMechOneDirSol+vpmMechLinear+vpmMechStopEnd+vpmMechFast+vpmMechLengthSw
		.Length=3400:.Steps=480
		.AddSw 12,0,2
		.AddPulseSwNew 75,12,0,5
		.AddPulseSwNew 74,12,4,9
		.Callback=GetRef("DrawHandX")
		.Start
	End With

	Set MoveGloveY = New cvpmMech
	With MoveGloveY
		.Sol1=24
		.Sol2=-23
		.MType=vpmMechOneDirSol+vpmMechLinear+vpmMechStopEnd+vpmMechFast+vpmMechLengthSw
		.Length=3400:.Steps=450
		.AddSw 37,0,1502
		.AddPulseSwNew 77,12,4,9
		.AddPulseSwNew 76,12,0,5
		.Callback=GetRef("DrawHandY")
		.Start
	End With

	'initialize the glove's X coordinates
	Mano.TransX=MoveGloveX.Position
	SupportoMano.TransX= Mano.TransX: Slitta.TransX= Mano.TransX: Magnet.TransX= Mano.TransX

	'initialize the glove's Y coordinates
	Mano.TransY = (450 - MoveGloveY.Position)
	SupportoMano.TransY= Mano.TransY: Magnet.TransY= Mano.TransY

End Sub

Dim MotorSnd:MotorSnd=0
Sub DrawHandX(aCurrPos,aSpeed,aLastPos)
 	If NOT IsEmpty(thePopBall) Then
 		thePopBall.X = aCurrPos + 308
 	End If
	If aSpeed=0 Then
		StopSound "motor1":MotorSnd=0
	Else
		If MotorSnd=0 Then PlayLoopSoundAtVol SoundFX("motor1", DOFGear), Mano, 1: MotorSnd=1
	End If
	Mano.TransX=aCurrPos - 20
	SupportoMano.TransX= Mano.TransX: Slitta.TransX= Mano.TransX: Magnet.TransX= Mano.TransX
End Sub

Sub DrawHandY(aCurrPos,aSpeed,aLastPos)
 	If NOT IsEmpty(thePopBall) Then
		thePopBall.Y = (450 - aCurrPos) - 12
	End If 
	If aSpeed=0 Then
		StopSound "motor1":MotorSnd=0
	Else
		If MotorSnd=0 Then PlayLoopSoundAtVol SoundFX("motor1", DOFGear), Mano, 1: MotorSnd=1
	End If
	Mano.TransY = (450 - aCurrPos)
	SupportoMano.TransY= Mano.TransY: Magnet.TransY= Mano.TransY
End Sub



' *********************************************************************
'						Lighting
' *********************************************************************

Dim LampState(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashLevel(200)
InitLamps
Sub InitLamps
	On Error Resume Next
	Dim i
	For i=0 To 200: Execute "Lights(" & i & ")  = Array (Light" & i & ",Light" & i & "a)": Next
    For i = 0 to 200
        LampState(i) = 0         ' current light state, independent of the fading level. 0 is off and 1 is on
        FlashSpeedUp(i) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(i) = 0.35 ' slower speed when turning off the flasher
        FlashLevel(i) = 0        ' the intensity of the flashers, usually from 0 to 1
    Next
	LampTimer.Interval = 10:LampTimer.Enabled = 1
End Sub

Sub LampTimer_Timer()
	Dim i
	For i=0 to 88:LampState(i) = ABS(Controller.Lamp(i)):Next
 	'Flashers
	AddLamp 117, F117
	AddLamp 118, F118
	AddLamp 119, F119
AddLamp 119, F119a
	AddLamp 120, F120
AddLamp 120, F120a
	AddLamp 125, F125
AddLamp 125, F125a
	AddLamp 126, F126
AddLamp 126, F126a
	AddLamp 127, F127
	AddLamp 128, F128
AddLamp 128, F128a
AddLamp 128, F128b

	'Flashers Luci Matrix
	AddLamp 51, F151
	AddLamp 52, F152
	AddLamp 53, F153
	AddLamp 61, F161
	AddLamp 62, F162
	AddLamp 63, F163
	AddLamp 71, F171
	AddLamp 72, F172
	AddLamp 73, F173
End Sub

Sub SetLamp(nr, enabled)
    If enabled Then
		LampState(nr) = 1
	Else
		LampState(nr) = 0
	End If
End Sub

Sub AddLamp(nr, object)
    Select Case LampState(nr)

        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < 0 Then FlashLevel(nr) = 0
			If TypeName(object) = "Light" Then
				Object.State = 0
			End If
			If TypeName(object) = "Flasher" Then
				Object.IntensityScale = FlashLevel(nr)
			End If
			If TypeName(object) = "Primitive" Then
				Object.DisableLighting = 0
			End If

        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > 1 Then FlashLevel(nr) = 1
			If TypeName(object) = "Light" Then
				Object.State = 1
			End If
			If TypeName(object) = "Flasher" Then
				Object.IntensityScale = FlashLevel(nr)
			End If
			If TypeName(object) = "Primitive" Then
				Object.DisableLighting = 1
			End If

    End Select
End Sub

'****************************************
'            REAL TIME UPDATES
'****************************************
 Set MotorCallback = GetRef("RealTimeUpdate")

Sub RealTimeUpdate
	FlipperL.objrotz = LeftFlipper.CurrentAngle + 1
	FlipperR.objrotz = RightFlipper.CurrentAngle + 1
	PGate1.RotX = Gate1.currentangle +90
    PGate2.RotX = Gate2.currentangle +90
    PGate3.RotX = Gate3.currentangle +90
lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
	RollingSoundUpdate
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

Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

'Set position as table object and Vol manually.
Sub PlaySoundAtVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3
Sub PlaySoundAtBallVol(sound, VolMult)
	PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub
'Set position as bumperX and Vol manually.
Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub
'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / JM.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / JM.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 6000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function RndNum(min,max)
	RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min and max
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
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

' *********************************************************************
' 						Other Sound FX
' *********************************************************************

Sub LeftFlipper_Collide(parm)
    RandomSoundFlipper
End Sub

Sub Rightflipper_Collide(parm)
    RandomSoundFlipper
End Sub

Sub Posts_Hit(idx)
	RandomSoundRubber()
End Sub

Sub Rubbers_Hit(idx)
	RandomSoundRubber()
End Sub

Sub RandomSoundRubber()
	Select Case RndNum(1,3)
		Case 1 : PlaySoundAtBallVol "fx_rubber_hit_1",10
		Case 2 : PlaySoundAtBallVol "fx_rubber_hit_2",10
		Case 3 : PlaySoundAtBallVol "fx_rubber_hit_3",10
	End Select
End Sub

Sub RandomSoundFlipper()
	Select Case RndNum(1,3)
		Case 1 : PlaySoundAtBallVol "flip_hit_1", 20
		Case 2 : PlaySoundAtBallVol "flip_hit_2", 20
		Case 3 : PlaySoundAtBallVol "flip_hit_3", 20
	End Select
End Sub

