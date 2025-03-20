'The Hateful Eight By BALUTITO

' Cactus Canyon - IPDB No. 4445
' © Bally/Midway 1998
' © Eric Priepke 2012 (P-ROC Project)
' VPX recreation by ninuzzu/Tom Tower

' Thalamus 2018-07-23
' This table already has its own 7.1 "Positional Sound Playback Functions" routine
' Changed UseSolenoids=1 to 2 for no-proc
' No special SSF tweaks yet.
' Support CCC, please see P-Roc updates in cactuscanyoncontinued_PROC folder
' FastFlips 2.0 required for CCC drunk flips.

 Option Explicit
 Randomize

'optReset = 1  'Uncomment to reset to default options in case of error or keep all changes temporary. When done recomment again!

Const PROC = 0

Const BallSize = 50
Const BallMass = 1.7

 On Error Resume Next
 ExecuteGlobal GetTextFile("controller.vbs")
 If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
 On Error Goto 0

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD:UseVPMDMD = DesktopMode
Dim UseSolenoids

Const cGameName = "cc_13"
Const B2ScGameName = "Proc_CC"
Const UseVPMModSol = True

' Needed or the bad guy's hat and dof shaker will be triggered.
Const cSingleLFlip = 0
Const cSingleRFlip = 0

' LoadVBSFiles "02800000", "WPC.VBS", 3.52
' Mentioned by Toxie here : https://www.vpforums.org/index.php?showtopic=44135&p=456910

If PROC = 1 Then
    UseSolenoids = 39
	'LoadController("PROC")
  LoadProc "02000000", "WPC.VBS", 3.50
Else
	UseSolenoids = 2
	'LoadController("VPM")
  LoadVPM "02000000", "WPC.VBS", 3.50
End If

' Standard Options
Const UseLamps     = 1
Const UseSync      = 0
Const HandleMech   = 0
Const UseGI = 0

' Standard Sounds
Const SSolenoidOn = "fx_solon"
Const SSolenoidOff = ""
Const SCoin = "fx_coin"

'**************************************************************
'************   Table Init   **********************************
'**************************************************************
Dim TrainMech, MineMech
Dim musicNum
musicNum = int ( rnd * 5)
NextTrack
Sub Table1_Init
	vpmInit Me
	On Error Resume Next
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Cactus Canyon - Midway/Williams 1998"
		.HandleKeyboard = 0
		.HandleMechanics = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.ShowTitle = 0
		If PROC = 0 Then .Hidden = Desktopmode
		.Games("cc_13").Settings.Value("sound") = 0
		.Games("cc_13").Settings.Value("samples") = 0
		On Error Resume Next
		.Run
		If Err Then MsgBox Err.Description
		.Switch(22) = 1 'close coin door
		.Switch(24) = 0 'always closed
	On Error Goto 0
	End With

'************  Main Timer init  ********************

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

'************   Nudging   **************************

	vpmNudge.TiltSwitch  = 14
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, sw51, sw52, sw55)

'**************   Mine Mech  ********************

	If PROC = 0 Then 
	Set MineMech = New cvpmMech 
	With MineMech
		.Sol1 = 17
		.Length = 100
		.Steps = 49
		.MType = vpmMechOneSol + vpmMechReverse + vpmMechLinear
		.AddSw 77, 0, 1 'Home							'Shoopity - Changed the last variable from 48 to 0, it helps with moving the primitive
		.AddPulseSw 78, 8, 2
		.Start
	End With
	End If
 
'**************   Train Mech  ********************

	If PROC = 0 Then
	Set TrainMech = New cvpmMech
	With TrainMech
		.Sol1 = 38 'Forward
		.Sol2 = 37 'Reverse
		.Length = 570
		.Steps = -700
		.MType = vpmMechTwoDirSol + vpmMechStopEnd + vpmMechLinear
		.AddSw 72, 0, 0   ' Home 
		.AddPulseSw 71, 14, 3
		.Start
	End With
	End If

'************  Misc Stuff init  ********************

	block.isdropped=1
	Sw61.IsDropped  = 1: Sw62.IsDropped  = 1 :Sw63.IsDropped  = 1 :Sw64.IsDropped  = 1
	Sw61a.IsDropped  = 1: Sw62a.IsDropped  = 1 :Sw63a.IsDropped  = 1 :Sw64a.IsDropped  = 1 
	LPin.IsDropped  = 1: RPin.IsDropped  = 1
	UpdateGI 0,0: UpdateGI 1,0: UpdateGI 2,0
	InitToys:InitLamps:InitTrough

'************ P-ROC Stuff init  ********************

	If PROC = 1 then 
		Enc_Stop.enabled = 1
		UpdateGI 0,1: UpdateGI 1,1: UpdateGI 2,1
		Card1.image = "card1_CCC"
		Card2.image = "card2_CCC"
		Guns.material = "Guns"
	End If
 End Sub

'********************************************
'**********   Keys   ************************
'********************************************

Sub Table1_KeyDown(ByVal keycode)
    If keycode = LeftTiltKey Then Nudge 90, 3:PlaySoundAt SoundFX("fx_nudge", 0), Card1
    If keycode = RightTiltKey Then Nudge 270, 3:PlaySoundAt SoundFX("fx_nudge", 0), Card2
    If keycode = CenterTiltKey Then Nudge 0, 4:PlaySoundAt SoundFX("fx_nudge", 0), OutHole
    If KeyCode = PlungerKey Then Plunger.Pullback:PlaysoundAt "fx_plungerpull", Plunger
    If vpmKeyDown(KeyCode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
    If KeyCode = PlungerKey Then Plunger.Fire : PlaysoundAt "fx_plunger", Plunger
    If vpmKeyUp(KeyCode) Then Exit Sub
End Sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_exit()
	Controller.Stop
	If B2SOn = True and PROC = 1 Then B2SController.Stop
	SaveValue TableName,"TrainPos",Train.TransX
End sub


'******************************
'*     HiRez00: Music Mod     *
'******************************


Sub NextTrack

If musicNum = 0 Then PlayMusic "django1.mp3" End If
If musicNum = 1 Then PlayMusic "django2.mp3" End If
If musicNum = 2 Then PlayMusic "django3.mp3" End If
If musicNum = 3 Then PlayMusic "django4.mp3" End If
If musicNum = 4 Then PlayMusic "django5.mp3" End If
If musicNum = 4 Then PlayMusic "django6.mp3" End If

musicNum = (musicNum + 1) mod 5
End Sub

Sub Table1_MusicDone
        NextTrack

End Sub
'******************
' Solenoids
'******************

SolCallback(1) = "AutoPlunger"							' Autoplunger
SolCallback(2) = "Drop1"					        	' Left Drop Target
SolCallback(3) = "Drop2"								' Left Center Drop Target
SolCallback(4) = "Drop3"								' Right Center Drop Target
SolCallback(5) = "Drop4"					        	' Right Drop Target
SolCallback(6) = "SolMinePopper"	        			' Mine Popper
SolCallback(7) = "Knocker"								' Knocker
SolCallback(8) = "SolSaloonPopper"	        			' Saloon Popper
SolCallback(9) = "ReleaseBall"					    	' Ball Release
'SolCallback(10) = ""									' Left Slingshot
'SolCallback(11) = ""									' Right Slingshot
'SolCallback(12) = ""									' Left Jet Bumper
'SolCallback(13) = ""									' Right Jet Bumper
SolCallback(14) = "GunPostLeft"				        	' Left  Gunfight Post
SolCallback(15) = "GunPostRight"			        	' Right Gunfight Post
'SolCallback(16) = ""									' Bottom Jet Bumper
SolCallback(21) = "LGate" 	  		    				' Left  Loop Gate
SolCallback(22) = "RGate" 	  		    				' Right Loop Gate

SolCallback(sLRFlipper) = "SolRFlipper"					' Left Flipper
SolCallback(sLLFlipper) = "SolLFlipper"					' Right Flipper
SolCallback(33) = "MoveBart"	    					' Move Bart Toy
SolCallback(36) = "MoveHat" 	    				    ' Bart Toy Hat

If PROC = 1 then
	SolCallBack(17) = "MoveMine_PROC"					' Mine Motor-PROC
	SolCallback(18) = "Sol18" 	  		    			' Mine Flasher-PROC
	SolCallBack(19) = "Sol19"                           ' Front Left Flasher-PROC
	SolCallBack(20) = "Sol20"                           ' Front Right Flasher-PROC
	SolCallback(24) = "Sol24" 	  		    		    ' Beacon Flasher-PROC
	SolCallBack(25) = "Sol25"                           ' Middle Right Flasher-PROC
	SolCallBack(26) = "Sol26"                           ' Saloon Flasher-PROC
	SolCallback(27) = "Sol27" 	  		    		    ' Back Right Flasher-PROC
	SolCallback(28) = "Sol28" 	  		    			' Back Left Flasher-PROC
	SolCallback(37) = "TrainB_PROC"						' Train Reverse-PROC
	SolCallback(38) = "TrainF_PROC"						' Train forward-PROC
Else
	SolCallBack(17) = "MoveMine"						' Mine Motor-VPM
	SolModCallback(18) = "Sol18" 	  		    		' Mine Flasher-VPM
	SolModCallBack(19) = "Sol19"                   		' Front Left Flasher-VPM
	SolModCallBack(20) = "Sol20"                    	' Front Right Flasher-VPM
	SolModCallback(24) = "Sol24" 	  		    		' Beacon Flasher-VPM
	SolModCallBack(25) = "Sol25"                    	' Middle Right Flasher-VPM
	SolModCallBack(26) = "Sol26"                    	' Saloon Flasher-VPM
	SolModCallback(27) = "Sol27" 	  		    		' Back Right Flasher-VPM
	SolModCallback(28) = "Sol28" 	  		    	'	 Back Left Flasher-VPM
	SolCallback(37) = "TrainB"							' Train Reverse-VPM
	SolCallback(38) = "TrainF"							' Train forward-VPM
End If

'**************************************************
'***************  Autoplunger *********************
'**************************************************

Dim plungerIM
Const IMPowerSetting = 65
Const IMTime = 0.25
Set plungerIM = New cvpmImpulseP
With plungerIM
.InitImpulseP swPlunger, IMPowerSetting, IMTime
.InitExitSnd SoundFX("fx_Autoplunger",DOFContactors), SoundFX("fx_solon",DOFContactors)
.CreateEvents "plungerIM"
End With

Sub AutoPlunger(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
		DOFALT 120, DOFPulse
	End If
End Sub

'**************************************************
'***************	Targets Bad Guys	***********
'**************************************************

Dim sw61Dir, sw62Dir, sw63Dir, sw64Dir
' Left Drop Target
 Sub Drop1(enabled)
	If enabled Then
		DOFALT 125, DOFPulse
		Sw61.IsDropped = 0 : Sw61a.IsDropped = 0 : Controller.Switch(61) = 0 : PlaySoundAt SoundFX("fx_DropTargetUp",DOFDropTargets),sw61p: sw61Dir = 1: sw61.TimerEnabled = 1 : light42.state=2 : light43.state = 2
	Else
		DOFALT 125, DOFPulse
		Sw61.IsDropped = 1 : Sw61a.IsDropped = 1 : Controller.Switch(61) = 1 : PlaySoundAt SoundFX("fx_DropTargetDown",DOFDropTargets),sw61p: sw61Dir = -1: sw61.TimerEnabled = 1: light42.state=0 : light43.state = 0
	End If
 End Sub
' Left Center Drop Target
 Sub Drop2(enabled)
	If enabled Then
		DOFALT 126, DOFPulse
		Sw62.IsDropped = 0 : Sw62a.IsDropped = 0 : Controller.Switch(62) = 0 : PlaySoundAt SoundFX("fx_DropTargetUp",DOFDropTargets),sw62p: sw62Dir = 1: sw62.TimerEnabled = 1: light25.state=2 : light26.state = 2
	Else
		DOFALT 126, DOFPulse
		Sw62.IsDropped = 1 : Sw62a.IsDropped = 1 : Controller.Switch(62) = 1 : PlaySoundAt SoundFX("fx_DropTargetDown",DOFDropTargets),sw62p: sw62Dir = -1: sw62.TimerEnabled = 1: light25.state=0 : light26.state = 0
	End If
 End Sub
' Right Center Drop Target
 Sub Drop3(enabled)
	If enabled Then
		DOFALT 127, DOFPulse
		Sw63.IsDropped = 0 : Sw63a.IsDropped = 0 : Controller.Switch(63) = 0 : PlaySoundAt SoundFX("fx_DropTargetUp",DOFDropTargets),sw63p: sw63Dir = 1: sw63.TimerEnabled = 1: light44.state=2 : light45.state = 2
	Else
		DOFALT 127, DOFPulse
		Sw63.IsDropped = 1 : Sw63a.IsDropped = 1 : Controller.Switch(63) = 1 : PlaySoundAt SoundFX("fx_DropTargetDown",DOFDropTargets),sw63p: sw63Dir = -1: sw63.TimerEnabled = 1: light44.state=0 : light45.state = 0
	End If
 End Sub
' Right Drop Target
 Sub Drop4(enabled)
	If enabled Then
		DOFALT 128, DOFPulse
		Sw64.IsDropped = 0 : Sw64a.IsDropped = 0 : Controller.Switch(64) = 0 : PlaySoundAt SoundFX("fx_DropTargetUp",DOFDropTargets),sw64p: sw64Dir = 1: sw64.TimerEnabled = 1: light50.state=2 : light51.state = 2
	Else
		DOFALT 128, DOFPulse
		Sw64.IsDropped = 1 : Sw64a.IsDropped = 1 : Controller.Switch(64) = 1 : PlaySoundAt SoundFX("fx_DropTargetDown",DOFDropTargets),sw64p: sw64Dir = -1: sw64.TimerEnabled = 1: light50.state=0 : light51.state = 0
	End If
 End Sub

Sub sw61_timer()
	sw61p.Z = sw61p.Z + (2 * sw61Dir)
	If sw61p.Z <= -39 Then Me.TimerEnabled = 0 : sw61p.Z = -39
	If sw61p.Z >= 39 Then Me.TimerEnabled = 0 : sw61p.Z = 39
End Sub

Sub sw62_timer()
	sw62p.Z = sw62p.Z + (2 * sw62Dir)
	If sw62p.Z <= -39 Then Me.TimerEnabled = 0 : sw62p.Z = -39
	If sw62p.Z >= 39 Then Me.TimerEnabled = 0 : sw62p.Z = 39
End Sub

Sub sw63_timer()
	sw63p.Z = sw63p.Z + (2 * sw63Dir)
	If sw63p.Z <= -39 Then Me.TimerEnabled = 0 : sw63p.Z = -39
	If sw63p.Z >= 39 Then Me.TimerEnabled = 0 : sw63p.Z = 39
End Sub

Sub sw64_timer()
	sw64p.Z = sw64p.Z + (2 * sw64Dir)
	If sw64p.Z <= -39 Then Me.TimerEnabled = 0 : sw64p.Z = -39
	If sw64p.Z >= 39 Then Me.TimerEnabled = 0 : sw64p.Z = 39
End Sub

'**************************************************
'***************  Mine Popper	 ******************
'**************************************************

 Sub SolMinePopper(enabled)
	If enabled Then
		DOFALT 132, DOFPulse
		MinePopper.kick 0, 45, 1.56
		If BIK = 0 Then
			PlaysoundAt SoundFX(SSolenoidOn,DOFContactors),MinePopper
		Else
			PlaysoundAt SoundFX("fx_mine_popper",DOFContactors),MinePopper
		End If
	End If
End Sub

'**************************************************
'***************  Knocker		 ******************
'**************************************************

Sub Knocker(enabled)
	If enabled Then
		PlaySoundAt SoundFX("fx_knocker",DOFKnocker),l84
		DOFALT 151,DOFPulse
	End If
End Sub

'************************************************
'***************  Saloon Popper	 ****************
'************************************************

 Sub SolSaloonPopper(enabled)
	If enabled Then
		DOFALT 133, DOFPulse
		BartPopper.kick 0, 45, 1.56
		SecretEntrance.enabled = 0
		vpmtimer.addtimer 1000 ,"SecretEntrance.enabled = 1'"
		If BIK = 0 Then
			PlaysoundAt SoundFX(SSolenoidOn,DOFContactors),BartPopper
		Else
			PlaysoundAt SoundFX("fx_saloon_popper",DOFContactors),BartPopper
		End If
	End If
End Sub

'******************************************************************
'*******	Trough based on nFozzy's 	***************************
'******************************************************************

'Load The Trough on Game Launch (order 35, 34, 33, 32)
Sub InitTrough()
'	If PROC = 0 then
'		Controller.Switch(35) = 1
'	Else
'		Controller.Switch(35) = 0
'	End If
	sw32.CreateSizedBallWithMass Ballsize/2, BallMass
	sw33.CreateSizedBallWithMass Ballsize/2, BallMass
	sw34.CreateSizedBallWithMass Ballsize/2, BallMass
	sw35.CreateSizedBallWithMass Ballsize/2, BallMass
	Controller.Switch(35) = 1
	Controller.Switch(34) = 1
	Controller.Switch(33) = 1
	Controller.Switch(32) = 1
End Sub

'Sub sw35_Hit():UpdateTrough: If PROC = 0 then Controller.Switch(35) = 1: End If: End Sub

Sub sw35_Hit():Controller.Switch(35) = 1:UpdateTrough:End Sub
Sub sw35_UnHit():Controller.Switch(35) = 0:UpdateTrough:End Sub

Sub sw34_Hit():Controller.Switch(34) = 1:End Sub
Sub sw34_UnHit():Controller.Switch(34) = 0:UpdateTrough:End Sub

Sub sw33_Hit():Controller.Switch(33) = 1:End Sub
Sub sw33_UnHit():Controller.Switch(33) = 0:UpdateTrough:End Sub

Sub sw32_Hit():Controller.Switch(32) = 1:End Sub
Sub sw32_UnHit():Controller.Switch(32) = 0:UpdateTrough:End Sub

Sub sw31_Hit():vpmTimer.PulseSw 31 : End Sub

Sub UpdateTrough()
	OutHole.TimerInterval = 300
	OutHole.TimerEnabled = 1
End Sub

Sub OutHole_Timer()
	If sw32.BallCntOver = 0 Then sw33.kick 58, 8
	If sw33.BallCntOver = 0 Then sw34.kick 58, 8
	If sw34.BallCntOver = 0 Then sw35.Kick 58, 8
	If sw35.BallCntOver = 0 Then OutHole.kick 58,8
	Me.TimerEnabled = 0
End Sub

'************************************************
'***************  Drain and Release *************
'************************************************

Dim BIP : BIP = 0

Sub OutHole_Hit()
	PlaySoundAtBall "fx_drain"
	UpdateTrough
	BIP = BIP - 1
	If PROC = 1 Then block.isdropped=0 : sw35.TimerEnabled = 1 : sw35.TimerInterval = 2000 ': Controller.Switch(35) = 1 : 
End Sub

Sub sw35_Timer()					'let's enable trough switch 35 for 2 seconds and add a dropping wall to prevent bugs
	'Controller.Switch(35)=0
	block.isdropped=1
	Me.TimerEnabled = 0
End Sub

Sub ReleaseBall(enabled)
	If enabled Then
		DOFALT 121, DOFPulse
		If sw35.BallCntOver = 0 Then
			PlaySoundAt SoundFX(SSolenoidOn,DOFContactors),sw32
		Else
			PlaySoundAt SoundFX("fx_ballrel",DOFContactors),sw32
		End If
		sw32.kick 58, 10
		UpdateTrough
		BIP = BIP + 1
	End If
End Sub

'************************************************
'**************		Slingshots 	*****************
'************************************************

Dim LStep, RStep, TStep

Sub sw51_Slingshot
	DOFALT 122, DOFPulse
	DOFALT 124, DOFPulse
	PlaySoundAtBall SoundFX("fx_slingshotL",DOFContactors)
	vpmTimer.PulseSw 51
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    Me.TimerInterval=20:Me.TimerEnabled=1
End Sub

Sub sw51_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


Sub sw52_Slingshot
	DOFALT 123, DOFPulse
	DOFALT 126, DOFPulse
	PlaySoundAtBall SoundFX("fx_slingshotR",DOFContactors)
	vpmTimer.PulseSw 52
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    Me.TimerInterval=20:Me.TimerEnabled=1
End Sub

Sub sw52_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub


Sub sw55_Slingshot
	DOFALT 134, DOFPulse
	PlaySoundAtBall SoundFX("fx_slingshotL",DOFContactors)
	vpmTimer.PulseSw 55
    TSling.Visible = 0
    TSling1.Visible = 1
    sling3.TransZ = -20
    TStep = 0
    Me.TimerInterval=20:Me.TimerEnabled=1
End Sub

Sub sw55_Timer
    Select Case TStep
        Case 3:TSLing1.Visible = 0:TSLing2.Visible = 1:sling3.TransZ = -10
        Case 4:TSLing2.Visible = 0:TSLing.Visible = 1:sling3.TransZ = 0:Me.TimerEnabled = 0
    End Select
    TStep = TStep + 1
End Sub

'************************************************
'**************		Bumpers 	*****************
'************************************************

Dim dirRing1 : dirRing1 = -1
Dim dirRing2 : dirRing2 = -1

Sub Bumper1_Hit
	vpmTimer.PulseSw 53
	PlaySoundAtBall SoundFX("fx_BumperLeft",DOFContactors)
	Me.TimerInterval=10:Me.TimerEnabled=1
	DOFALT 130, DOFPulse
	DOFALT 135, DOFPulse
End Sub

Sub Bumper2_Hit
	vpmTimer.PulseSw 54
	PlaySoundAtBall SoundFX("fx_BumperRight",DOFContactors)
	Me.TimerInterval=10:Me.TimerEnabled=1
	DOFALT 131, DOFPulse
	DOFALT 136, DOFPulse
End Sub

Sub Bumper1_timer()
	BR1.Z = BR1.Z + (5 * dirRing1)
	If BR1.Z <= 0 Then dirRing1 = 1
	If BR1.Z >= 30 Then
		dirRing1 = -1
		BR1.Z = 30
		Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper2_timer()
	BR2.Z = BR2.Z + (5 * dirRing2)
	If BR2.Z <= 0 Then dirRing2 = 1
	If BR2.Z >= 30 Then
		dirRing2 = -1
		BR2.Z = 30
		Me.TimerEnabled = 0
	End If
End Sub

'************************************************
'**************		Gun Posts 	*****************
'************************************************

Sub GunPostLeft(enabled)
	DOFALT 145, DOFPUlse
	If Enabled Then
		LPin.IsDropped = 0
		PlaySoundAt SoundFX("fx_PostUp",DOFContactors),LeftFlipper
		DOFALT 152, DOFOn
	Else
		LPin.IsDropped = 1
		PlaySoundAt SoundFX("fx_PostDown",0),LeftFlipper
		DOFALT 152, DOFOff
	End If
End Sub

Sub GunPostRight(enabled)
	DOFALT 146, DOFPulse
	If Enabled Then
		RPin.IsDropped = 0
		PlaySoundAt SoundFX("fx_PostUp",DOFContactors),RightFlipper
		DOFALT 152, DOFOn
	Else
		RPin.IsDropped = 1
		PlaySoundAt SoundFX("fx_PostDown",0),RightFlipper
		DOFALT 152, DOFOff
	End If
End Sub

'************************************************
'***************  Loop Gates	 ****************
'************************************************

 Sub LGate(enabled)
	If enabled Then
		PlaySoundAt "fx_gate",LLoopGate
		LLoopGate.Open = 1
		DOFALT 147,DOFPulse
	Else
		DOFALT 147,DOFPulse
		LLoopGate.Open = 0
	End If
 End Sub
 
 Sub RGate(enabled)
	If enabled Then
		PlaySoundAt "fx_gate",RLoopGate
		RLoopGate.Open = 1
		DOFALT 148,DOFPulse
	Else
		DOFALT 148,DOFPulse
		RLoopGate.Open = 0
	End If
 End Sub

'*************************************************
'******		Shoopity's Train Animation		******
'*************************************************

Dim TrainOn,TrainDir, TrainSpeed
TrainSpeed = 1

Sub TrainF(enabled)
	Train.TransX = -TrainMech.Position
	Train1.TransX = -TrainMech.Position
	TrainTimer.Enabled = 1
	TrainOn = enabled
End Sub

Sub TrainB(enabled)
	Train.TransX = -TrainMech.Position
	Train1.TransX = -TrainMech.Position
	TrainTimer.Enabled = 1
	TrainOn = enabled
End Sub

Sub TrainTimer_Timer()
	PlayLoopSoundAt SoundFX("fx_train_motor", DOFGear), Train
	If Train.TransX >= -TrainMech.Position Then
		TrainDir = -1
		Pgear1.RotY = Pgear1.RotY + 1
		Pgear2.RotY = Pgear2.RotY - 1
		Pgear3.RotY = Pgear3.RotY - 1
	Else
		TrainDir = 1
		Pgear1.RotY = Pgear1.RotY - 1
		Pgear2.RotY = Pgear2.RotY + 1
		Pgear3.RotY = Pgear3.RotY + 1
	End If
	Train.TransX = Train.TransX + TrainSpeed * TrainDir
	Train1.TransX = Train.TransX
	If ((Train.TransX >= -TrainMech.Position AND TrainDir = 1) OR (Train.TransX <= -TrainMech.Position AND TrainDir = -1)) Then Train.TransX = -TrainMech.Position
	If ((Train.TransX >= -TrainMech.Position AND TrainDir = 1) OR (Train.TransX <= -TrainMech.Position AND TrainDir = -1))  AND TrainOn = 0 Then
		Me.Enabled = 0
		StopSound "fx_train_motor"
	End If
	If Train.TransX = -611 Then StopSound "fx_train_motor"
End Sub

'***********************************************
'** New Train Mech to work with P-ROC (Dozer) **
'***********************************************

Dim TrainDirPROC

Sub TrainF_PROC(Enabled)
	If Enabled Then
		PlayLoopSoundAt SoundFX("fx_train_motor", DOFGear), Train
		DOFALT 140, DOFOn
		Polly.Enabled = 1
		sw71.enabled = 1
		TrainDirPROC = 1
	Else
		StopSound "fx_train_motor"
		DOFALT 140, DOFOff
		Polly.enabled = 0
		sw71.enabled = 0
	End If
End Sub

Sub TrainB_PROC(Enabled)
	If Enabled Then
		PlayLoopSoundAt SoundFX("fx_train_motor", DOFGear), Train
		DOFALT 140, DOFOn
		Polly.Enabled = 1
		sw71.enabled = 1
		TrainDirPROC = 2
	Else
		StopSound "fx_train_motor"
		DOFALT 140, DOFOff
		Polly.enabled = 0
		sw71.enabled = 0
	End If
End Sub

Sub Polly_Timer()
	Select Case TrainDirPROC
		Case 1:		Train.TransX = Train.TransX - 2
					Train1.TransX = Train.TransX
					Pgear1.RotY = Pgear1.RotY + 1
					Pgear2.RotY = Pgear2.RotY - 1
					Pgear3.RotY = Pgear3.RotY - 1
					If Train.TransX <= -5 Then Controller.Switch(72) = 0 
					If Train.TransX <= -611 Then Controller.Switch(71) = 1

		Case 2:		Train.TransX = Train.TransX + 4
					Train1.TransX = Train.TransX
					Pgear1.RotY = Pgear1.RotY - 1
					Pgear2.RotY = Pgear2.RotY + 1
					Pgear3.RotY = Pgear3.RotY + 1
					If Train.TransX => 0 Then Controller.Switch(72) = 1 : Controller.Switch(71) = 1
	End Select
End Sub

Sub sw71_Timer()					'Dozer - Encoder Pulse which runs with Train Mech.
vpmTimer.PulseSw 71
End Sub

Sub Enc_Stop_Timer()				'Dozer - will disable the pulse the first 5 seconds the table is run
sw71.enabled = 0
me.enabled = 0
End Sub

'*********************************************
'******		Shoopity's Mine Animation	******
'*********************************************

Dim MineOn, Minespeed, MineDir
MineSpeed = 0.25

Sub MoveMine(enabled)
	MineTimer.enabled = 1
	MineOn = enabled
End Sub

Sub MineTimer_Timer()
	If MineSign.Z > MineMech.Position Then
		MineDir = -1
	Else
		MineDir = 1
	End If
	PlayLoopSoundAt SoundFX("fx_mine_motor", DOFGear), Mine
	MineSign.Z = MineSign.Z + Minespeed * MineDir
	If ((MineSign.Z >= MineMech.Position AND MineDir = 1) OR (MineSign.Z <= MineMech.Position AND MineDir = -1)) AND Not(MineOn) Then		'Shoopity- If the primitive catches the mech AND the solenoid is off, then turn off the timer
		Me.Enabled = 0
		StopSound "fx_mine_motor"
	End If
	Mine.RotX = (MineSign.Z * 0.375) - 10
	If MineSign.Z < 24 Then												'Shoopity- I will drop the wall in the function that moves the sign
		sw15.IsDropped = 0
	else
		sw15.IsDropped = 1
	End If
End sub

'**********************************************
'** New Mine Mech to work with P-ROC (Dozer) **
'**********************************************

Dim MineDirPROC:MineDirPROC = 1

Sub MoveMine_PROC(Enabled)
	If Enabled then
		PlayLoopSoundAt SoundFX("fx_mine_motor", DOFGear), Mine
		DOFALT 141, DOFOn
		MineDirPROC=MineDirPROC
		sw15.Timerenabled = 1
		sw15b.Timerenabled = 1
	Else
		sw15.Timerenabled = 0
		sw15b.Timerenabled = 0
		StopSound "fx_mine_motor"
		DOFALT 141, DOFOff
	End If
End Sub

Sub Sw15_Timer()
	Select Case MineDirPROC
		Case 1:		MineSign.Z = MineSign.Z + Minespeed
					Mine.RotX = (MineSign.Z * 0.375) - 10
					If MineSign.Z => 0 Then Controller.Switch(77) = 0
					If MineSign.Z => 24 Then sw15.IsDropped = 1
					If MineSign.Z => 45 Then MineDirPROC = 2

		Case 2:		MineSign.Z = MineSign.Z - Minespeed
					Mine.RotX = (MineSign.Z * 0.375) - 10
					If MineSign.Z <= 0 Then Controller.Switch(77) = 1
					If MineSign.Z <= 24 Then sw15.IsDropped = 0
					If MineSign.Z <= 0 Then MineDirPROC = 1
	End Select
End sub

Sub sw15b_Timer()										'Dozer - Encoder Pulse which runs with Mine Mech.
vpmTimer.PulseSw 78
End Sub

'*********************************************
'******		Shoopity 's Bart Animation	******
'*********************************************
 
Dim bsmash : bsmash = 1
Dim blrnh:blrnh = 1
Dim blrnhud:blrnhud = 1
Dim BartSpeed:Bartspeed = 6
Dim BallHat

Sub InitToys
	Set BallHat = KiHat.CreateBall
	BartOrigX = Bart.X
	BartOrigY = Bart.Y
	Mine.RotX = (MineSign.Z * 0.375) - 10
	KiHat.Kick 0, 0, 0
	Dim TrainPosX:TrainPosX = LoadValue (TableName,"TrainPos")
	If TrainPosX = "" then Train.TransX = 0 Else Train.TransX = TrainPosX End If
End Sub

Sub MoveBart(Enabled)
	If Enabled Then
		DOFALT 149, DOFPulse
		blrnh = -1									'Start him moving left
		BartSwitched = 0							'Reset every time this kicks
		Bart1.Enabled = 1							'Shoopity- 	turn on the animation any time the bart toy is moving							
		PlaySoundAt SoundFX("fx_bart_hit", DOFContactors),Bart
		BallHat.VelX = RndNum (-10,10)
	end If
End Sub	

Sub MoveHat(Enabled)
	If Enabled Then
		DOFALT 150, DOFPulse
		PlaySoundAt SoundFX("fx_bart_move", DOFContactors),Bart
		Bart2.Enabled = 1			'Shoopity- Here, the hat obviously returns to the head, and we'll turn it off in the timer, like it was done prior to me coming here
	End If
End Sub

Sub BHit_Timer()
	Select Case bsmash
	Case 1:
		If Bart.TransY => 20 Then
			bsmash = 2
		End If
		Bart.TransY = Bart.TransY + 2
		Bart_Hat.TransY = Bart_Hat.TransY + 2
		bartsh.Y = bartsh.Y + 2
	Case 2:
		If Bart.TransY <= 0 Then
			me.enabled = 0
			Bart.TransZ= 0
			Bart_Hat.TransY = 70
			bartsh.Y = 224.25
		End If
		Bart.TransY = Bart.TransY - 2
		Bart_Hat.TransY = Bart_Hat.TransY - 2
		bartsh.Y = bartsh.Y - 2
	End Select
End Sub

Dim BartOrigX, BartOrigY, BartSwitched
Sub Bart1_Timer() ' - Left then right with no hat lift.					'Shoopity- I've changed it to just move back and forth as long as the solenoid is on
	Bart.X = Bart.X + BartSpeed * blrnh									'Shoopity- so we move it, but you can change the speed by changing BartSpeed if you want, and blrnh will be used as a direction
	Bart_Hat.X = Bart_Hat.X + BartSpeed * blrnh						'Shoopity- had to change it back since they're not longer in the same location
	bartsh.X = bartsh.X + BartSpeed * blrnh
	Bart_Hat.RotZ = KiHat.X - BallHat.X
	If Bart.X <= 592 Then blrnh = 1:BartSwitched = BartSwitched + 1
	If Bart.X >= 660 Then blrnh = -1:BartSwitched = BartSwitched + 1
	If Bart.X <= BartOrigX AND BartSwitched >=2 Then
		Bart.X = BartOrigX:bartsh.X=624.436
		Me.Enabled = 0
	End If
End Sub

Dim HatHeight, HatHi, HatAdjuster
HatAdjuster = 100
HatHeight = Bart_Hat.Z												'Shoopity- So if you ever change the height of the hat, it'll get remembered here properly
HatHi = HatHeight + 75												'Shoopity- The heighest point the hat will go, so if you want to change how hi the hat goes, change that here
Sub Bart2_Timer() ' - Left then right with no hat lift.				'Shoopity- I'm pretty sure this is supposed to say "Hat hit" or something like that
	Bart_Hat.Z = (dSin(blrnhud) * HatAdjuster) + HatHeight			'Shoopity- This is how I use physics, although it's not going to be variable height like the beer mug is
	blrnhud = blrnhud + SlowMo + 5
	If blrnhud > 180 Then
		blrnhud =1
		Me.Enabled = 0
	End If
End Sub

'********************************************************
' Shoopity's Beer Mug Animation
'********************************************************

Dim MugHit								'Shoopity- Variable to capture the Y velocity of the ball to know how high to kick it 
Sub Sw46_Hit()
	DOFALT 153, DOFPulse
	If BeerMugMod = 1 Then
		BeerMugL1.Duration 2, 1000, 0		'light starts to blink for 1000ms and then switches to OFF
		BeerMugL2.Duration 2, 1000, 0
		BeerMugL3.Duration 2, 1000, 0
	End If
	RandomSoundMetal()
	vpmTimer.PulseSwitch 46,0,0
	MugHit = ActiveBall.VelY
	Beer.Enabled = 1
End Sub

Dim mmmbeer, BeerMaxHeight, SlowMo, BeerAdjuster, mmmswitch
SlowMo = 3																'Shoopity- Just a fun variable if you want to see stuff move in slow motion
mmmbeer = 1
BeerAdjuster = 10														'Shoopity- Adjuster to translate the ball velocity into how high the mug goes
BeerMaxHeight = 150														'Shoopity- if you want to change how high the mug goes, just change this number
Sub Beer_Timer()
	BeerMug.Z = (dSin(mmmBeer) * (ABS(MugHit)*BeerAdjuster)) + 52
	sw46p.RotX = -(dSin(mmmBeer) * MugHit/30)
	If BeerMugMod = 1 Then
		BeerMugL1.BulbHaloHeight = BeerMug.Z
		BeerMugL2.BulbHaloHeight = BeerMug.Z + 56
		BeerMugL3.BulbHaloHeight = BeerMug.Z + 30
	End If
	mmmBeer = mmmBeer + SlowMo
	If BeerMug.Z > BeerMaxHeight Then									'Shoopity- If you hight the max height then...
		BeerMug.Z = BeerMaxHeight										' Hold it there for a bit
		sw46p.RotX = -BeerMaxHeight/3									' Hold it there for a bit
	End If
	If mmmBeer > 180 Then												'Shoopity- If it's traveled all the way up and down then...
		mmmBeer = 1														'Reset so next time it starts over
		Me.Enabled = 0													'Stop the animation
	End If
End Sub

'*********************************************
'**************	Flippers	***************
'*********************************************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup",DOFFlippers), LeftFlipper
		DOFALT 101, DOFOn
        LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown",DOFFlippers), RightFlipper
		DOFALT 101, DOFOff
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup",DOFFlippers), LeftFlipper
		DOFALT 102, DOFOn
        RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown",DOFFlippers), RightFlipper
		DOFALT 102, DOFOff
        RightFlipper.RotateToStart
    End If
End Sub

'**************************************************************
'***********************	Switches   ************************
'**************************************************************

'***************  Gold Mine & Sign   *************

Sub sw15_Hit() : PlaySoundAtBall "fx_mine_hit" : vpmTimer.PulseSwitch 15,0,0 : DOFALT 149, DOFPulse : End Sub
Sub sw15b_Hit() : RandomSoundMetal() : vpmTimer.PulseSwitch 15,0,0 : End Sub

'***************  Bart Toy   *********************

Sub sw75_Hit() : RandomSoundMetal() : vpmTimer.PulseSwitch 75,0,0 : bsmash = 1 : Bhit.enabled = 1 : End Sub

'***************  Saloon   *********************

Sub sw73_Hit() : PlaySoundAtBall "fx_gate4" : vpmTimer.PulseSwitch 73,0,0 : End Sub

'***************  Opto Switches   *********************

Sub sw36_Hit() : vpmTimer.PulseSwitch 36,0,0 : End Sub
Sub sw37_Hit() : vpmTimer.PulseSwitch 37,0,0 : End Sub
Sub sw56_Hit() : vpmTimer.PulseSwitch 56,0,0 : End Sub
Sub sw66_Hit() : vpmTimer.PulseSwitch 66,0,0 : End Sub
Sub sw67_Hit() : vpmTimer.PulseSwitch 67,0,0 : End Sub

'***************  Rollover Switches   *************************

Sub sw16_Hit : Controller.Switch(16) = 1 : PlaySoundAtBall "fx_sensor" : End Sub
Sub sw16_UnHit : Controller.Switch(16) = 0 : End Sub
Sub sw17_Hit : Controller.Switch(17) = 1 : PlaySoundAtBall "fx_sensor" : End Sub
Sub sw17_UnHit : Controller.Switch(17) = 0 : End Sub
Sub sw18_Hit : Controller.Switch(18) = 1 : PlaySoundAtBall "fx_sensor"  : End Sub
Sub sw18_UnHit : Controller.Switch(18) = 0 : End Sub
Sub sw26_Hit : Controller.Switch(26) = 1 : PlaySoundAtBall "fx_sensor" : End Sub
Sub sw26_UnHit : Controller.Switch(26) = 0 : End Sub
Sub sw27_Hit : Controller.Switch(27) = 1 : PlaySoundAtBall "fx_sensor" : End Sub
Sub sw27_UnHit : Controller.Switch(27) = 0 : End Sub
Sub sw47_Hit : Controller.Switch(47) = 1 : PlaySoundAtBall "fx_sensor" : End Sub
Sub sw47_UnHit : Controller.Switch(47) = 0 : End Sub
Sub sw48_Hit : Controller.Switch(48) = 1 : PlaySoundAtBall "fx_sensor" : End Sub
Sub sw48_UnHit : Controller.Switch(48) = 0 : End Sub
Sub sw57_Hit : Controller.Switch(57) = 1 : PlaySoundAtBall "fx_sensor" : End Sub
Sub sw57_UnHit : Controller.Switch(57) = 0 : End Sub
Sub sw58_Hit : Controller.Switch(58) = 1 : PlaySoundAtBall "fx_sensor" : End Sub
Sub sw58_UnHit : Controller.Switch(58) = 0 : End Sub

'***************  Ramps Switches **********************

Sub sw82_Hit() : vpmTimer.PulseSwitch 82,0,0 : If ActiveBall.VelY < 0 Then PlaySoundAtBall "fx_center_ramp_enter": End If : End Sub
Sub sw82_UnHit(): If ActiveBall.VelY > 0 Then StopSound "fx_center_ramp_enter": End If : End Sub
Sub Sw65_Hit():Controller.Switch(65) = 1:sw65flip.rotatetoend: PlaySoundAtBall "fx_gate4": DOFALT 107, DOFPulse : End Sub
Sub Sw65_UnHit():Controller.Switch(65) = 0:sw65flip.rotatetostart: End Sub
Sub sw68_Hit():Controller.Switch(68) = 1:sw68flip.rotatetoend: PlaySoundAtBall "fx_gate4":End Sub
Sub Sw68_UnHit():Controller.Switch(68) = 0:sw68flip.rotatetostart: End Sub
Sub Sw83_Hit():Controller.Switch(83) = 1:sw83flip.rotatetoend: PlaySoundAtBall "fx_gate4": DOFALT 107, DOFPulse : End Sub
Sub Sw83_UnHit():controller.Switch(83) = 0:sw83flip.rotatetostart:End Sub
Sub sw84_Hit():Controller.Switch(84) = 1:sw84flip.rotatetoend: PlaySoundAtBall "fx_gate4": DOFALT 107, DOFPulse : End Sub
Sub Sw84_UnHit():controller.Switch(84) = 0:sw84flip.rotatetostart:End Sub
Sub sw85_Hit():Controller.Switch(85) = 1:sw85flip.rotatetoend: PlaySoundAtBall "fx_gate4":End Sub
Sub Sw85_UnHit():controller.Switch(85) = 0:sw85flip.rotatetostart:End Sub

'***************  Hit Targets **********************

Sub sw28_Hit() : PlaySoundAtBall SoundFX("fx_target",DOFTargets) : vpmTimer.PulseSwitch 28,0,0 : DOFALT 104, DOFPulse : End Sub
Sub sw44_Hit() : PlaySoundAtBall SoundFX("fx_target",DOFTargets) : vpmTimer.PulseSwitch 44,0,0 : DOFALT 104, DOFPulse : End Sub
Sub sw86_Hit() : PlaySoundAtBall SoundFX("fx_target",DOFTargets) : vpmTimer.PulseSwitch 86,0,0 : DOFALT 103, DOFPulse : End Sub
Sub sw87_Hit() : PlaySoundAtBall SoundFX("fx_target",DOFTargets) : vpmTimer.PulseSwitch 87,0,0 : DOFALT 103, DOFPulse : End Sub

'***************  Drop Targets **********************

Sub Sw61_Hit() : PlaySoundAtBall SoundFX("fx_target_drop",DOFDropTargets) : Controller.Switch(61) = 1 : sw61p.transY = -5 : DOFALT 125, DOFPulse : End Sub
Sub Sw61_UnHit() : sw61p.transY = 0 :End Sub
Sub Sw62_Hit() : PlaySoundAtBall SoundFX("fx_target_drop",DOFDropTargets) : Controller.Switch(62) = 1 : sw62p.transY = -5 : DOFALT 126, DOFPulse : End Sub
Sub Sw62_UnHit() : sw62p.transY = 0 :End Sub
Sub Sw63_Hit() : PlaySoundAtBall SoundFX("fx_target_drop",DOFDropTargets) : Controller.Switch(63) = 1 : sw63p.transY = -5 : DOFALT 127, DOFPulse : End Sub
Sub Sw63_UnHit() : sw63p.transY = 0 :End Sub
Sub Sw64_Hit() : PlaySoundAtBall SoundFX("fx_target_drop",DOFDropTargets) : Controller.Switch(64) = 1 : sw64p.transY = -5 : DOFALT 128, DOFPulse : End Sub
Sub Sw64_UnHit() : sw64p.transY = 0 :End Sub

'************************************************************
'**************		Saloon & Gold Mine Subway		*********
'************************************************************

Dim BIK:BIK=0

Sub SecretEntrance_Hit() : PlaySoundAtBall "fx_hole" : End Sub

Sub BartHole_Hit() : PlaySoundAtBall "fx_saloon_enter" :End Sub

Sub BartPopper_Hit
	PlaySoundAtBall "fx_kicker_catch"
	Controller.Switch (42) = 1
	BIK = BIK + 1
End Sub

Sub BartPopper_UnHit()
	Controller.Switch (42) = 0
	BIK = BIK - 1
End Sub

Sub MineHole_Hit() : PlaySoundAtBall "fx_mine_enter" :End Sub

Sub MinePopper_Hit
	PlaySoundAtBall "fx_kicker_catch"
	Controller.Switch (41) = 1
	BIK = BIK + 1
End Sub

Sub MinePopper_UnHit()
	Controller.Switch (41) = 0
	BIK = BIK - 1
End Sub

' *********************************************************************
'						Lighting
' *********************************************************************

Dim LampState(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashLevel(200), FadingLevel(200)
Dim RedGis(50), GreenGis(50), BlueGis(50)

Sub InitLamps
	On Error Resume Next
	Dim i
	For i=0 To 127: Execute "Lights(" & i & ")  = Array (L" & i & ",L" & i & "a)": Next
	If BeerMugMod = 1 Then Lights(85) = Array (BeerMugL1,BeerMugL2,BeerMugL3)
    For i = 0 to 200
        LampState(i) = 0         ' current light state, independent of the fading level. 0 is off and 1 is on
        FlashSpeedUp(i) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(i) = 0.35 ' slower speed when turning off the flasher
        FlashLevel(i) = 0        ' the intensity of the flashers, usually from 0 to 1
		FadingLevel(i) = 0			 ' the modulation using VPM modulated solenoids, usually from 0 to 255
    Next
	LampTimer.Interval = 30:LampTimer.Enabled = 1
	For i=0 to (RedGI.count-1) : RedGis(i) = RedGI(i).intensity : Next			'map gi lamp intensities to an array
	For i=0 to (GreenGI.count-1): GreenGis(i) = GreenGI(i).intensity: Next		'map gi lamp intensities to an array
	For i=0 to (BlueGI.count-1): BlueGis(i) = BlueGI(i).intensity : Next		'map gi lamp intensities to an array
End Sub

Sub LampTimer_Timer()
	LampState(17) = L17.state
	LampState(18) = L18.state
	AddLamp 17, L17R
	AddLamp 18, L18R
	'Flashers
	If PROC = 0 Then
		AddCollMod 148, F18
		AddCollMod 149, F19
		AddCollMod 150, F20
		AddCollMod 154, F24
		AddCollMod 155, F25
		AddCollMod 156, F26
		AddCollMod 157, F27
		AddCollMod 158, F28
		AddLampMod 155, F25R
		AddLampMod 156, F26R
		AddLampMod 157, F27R
		AddLampMod 157, F27R1
		AddLampMod 157, F27R2
		AddLampMod 158, F28R
		AddLampMod 158, F28R1
		AddLampMod 158, F28R2
	Else
		AddColl 148, F18
		AddColl 149, F19
		AddColl 150, F20
		AddColl 154, F24
		AddColl 155, F25
		AddColl 156, F26
		AddColl 157, F27
		AddColl 158, F28
		AddLamp 155, F25R
		AddLamp 156, F26R
		AddLamp 157, F27R
		AddLamp 157, F27R1
		AddLamp 157, F27R2
		AddLamp 158, F28R
		AddLamp 158, F28R1
		AddLamp 158, F28R2
	End If
End Sub

Sub SetLamp(nr, enabled)
	LampState(nr) = ABS(CInt(enabled))
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

' Modulated Flasher and Lights objects

Sub SetLampMod(nr, value)
    If value > 0 Then
		LampState(nr) = 1
	Else
		LampState(nr) = 0
	End If
	FadingLevel(nr) = value
End Sub

Sub AddLampMod(nr, object)
	Object.IntensityScale = FadingLevel(nr)/128
	If TypeName(object) = "Light" Then
		Object.State = LampState(nr)
	End If
	If TypeName(object) = "Flasher" Then
		Object.visible = LampState(nr)
	End If
End Sub

Sub AddCollMod(nr, object)
	Dim i
	for each i in object:i.State = LampState(nr):i.IntensityScale = FadingLevel(nr)/128:next
End Sub

Sub AddColl(nr, object)
	Dim i
	for each i in object:i.State = LampState(nr):next
End Sub

'*********		FLASHERS	****************
Const OnOffDelay = 50					'PROC on-off is too fast, let's reduce it a bit. This is the delay (in milliseconds) between the on and off signals		

Sub Sol18(value)
	If PROC = 0 Then
		SetLampMod 148, value
		If value = 0 Then DOFALT 18, DOFOff Else DOFALT 18, DOFOn End If
		If value > 110 Then
			Mine.Image = "GoldMine_1"
		Else
			Mine.Image = "GoldMine_0"
		End If
	End If
	If PROC = 1 Then
		If value Then
			DOFALT 18, DOFOn
			Mine.Image = "GoldMine_1"
			SetLamp 148, 1
		else
			F18a.TimerInterval=OnOffDelay:F18a.TimerEnabled=1	'PROC on-off is too fast, let's reduce it a bit
		End If
	End If
End Sub

Sub F18a_timer()
	Me.TimerEnabled=0
	DOFALT 18, DOFOff
	Mine.Image = "GoldMine_0"
	SetLamp 148, 0
End Sub

Sub Sol19(value)
	If PROC = 0 Then
		SetLampMod 149, value
		If value = 0 Then DOFALT 19, DOFOff Else DOFALT 19, DOFOn End If
	End If
	If PROC = 1 Then
		If value Then
			DOFALT 19, DOFOn
			SetLamp 149, 1
		else
			light10.TimerInterval=OnOffDelay:light10.TimerEnabled=1	'PROC on-off is too fast, let's reduce it a bit
		End If
	End If
End Sub

Sub light10_timer()
	Me.TimerEnabled=0
	DOFALT 19, DOFOff
	SetLamp 149, 0
End Sub

Sub Sol20(value)
	If PROC = 0 Then
		SetLampMod 150, value
		If value = 0 Then DOFALT 20, DOFOff Else DOFALT 20, DOFOn End If
	End If
	If PROC = 1 Then
		If value Then
			DOFALT 20, DOFOn
			SetLamp 150, 1
		else
			light11.TimerInterval=OnOffDelay:light11.TimerEnabled=1	'PROC on-off is too fast, let's reduce it a bit
		End If
	End If
End Sub

Sub light11_timer()
	Me.TimerEnabled=0
	DOFALT 20, DOFOff
	SetLamp 150, 0
End Sub

Sub Sol24(value)
	If PROC = 0 Then
		SetLampMod 154, value
		If value = 0 Then DOFALT 24, DOFOff Else DOFALT 24, DOFOn End If
	End If
	If PROC = 1 Then
		DOFALT 24, ABS(CInt(value))
		SetLamp 154, value
	End If
End Sub

Sub Sol25(value)
	If PROC = 0 Then
		SetLampMod 155, value
		If value = 0 Then DOFALT 25, DOFOff Else DOFALT 25, DOFOn End If
	End If
	If PROC = 1 Then
		If value Then
			DOFALT 25, DOFOn
			SetLamp 155, 1
		else
			F25R.TimerInterval=OnOffDelay:F25R.TimerEnabled=1	'PROC on-off is too fast, let's reduce it a bit
		End If
	End If
End Sub

Sub F25R_timer()
	Me.TimerEnabled=0
	DOFALT 25, DOFOff
	SetLamp 155, 0
End Sub

Sub Sol26(value)
	If PROC = 0 Then
		SetLampMod 156, value
		If value = 0 Then DOFALT 26, DOFOff Else DOFALT 26, DOFOn End If
	End If
	If PROC = 1 Then
		If value Then
			DOFALT 26, DOFOn
			SetLamp 156, 1
		else
			F26R.TimerInterval=OnOffDelay:F26R.TimerEnabled=1	'PROC on-off is too fast, let's reduce it a bit
		End If
	End If
End Sub

Sub F26R_timer()
	Me.TimerEnabled=0
	DOFALT 26, DOFOff
	SetLamp 156, 0
End Sub

Sub Sol27(value)
	If PROC = 0 Then
		SetLampMod 157, value
		If value = 0 Then DOFALT 27, DOFOff Else DOFALT 27, DOFOn End If
	End If
	If PROC = 1 Then
		If value Then
			DOFALT 27, DOFOn
			SetLamp 157, 1
		else
			F27R.TimerInterval=OnOffDelay:F27R.TimerEnabled=1	'PROC on-off is too fast, let's reduce it a bit
		End If
	End If
End Sub

Sub F27R_timer()
	Me.TimerEnabled=0
	DOFALT 27, DOFOff
	SetLamp 157, 0
End Sub

Sub Sol28(value)
	If PROC = 0 Then
		SetLampMod 158, value
		If value = 0 Then DOFALT 28, DOFOff Else DOFALT 28, DOFOn End If
	End If
	If PROC = 1 Then
		If value Then
			DOFALT 28, DOFOn
			SetLamp 158, 1
		else
			F28R.TimerInterval=OnOffDelay:F28R.TimerEnabled=1	'PROC on-off is too fast, let's reduce it a bit
		End If
	End If
End Sub

Sub F28R_timer()
	Me.TimerEnabled=0
	DOFALT 28, DOFOff
	SetLamp 158, 0
End Sub

'**************		GI		*****************
Set GiCallback2 = GetRef("UpdateGI")

Sub UpdateGI(nr, step)
Dim xxx
	If PROC=0 Then
		Select Case nr
			Case 0
				If step=0 Then
					For each xxx in LeftGI:xxx.state=0:Next
				Else
					For each xxx in LeftGI:xxx.state=1:Next
				End If
				For each xxx in LeftGI:xxx.IntensityScale = 0.125 * step:next
			Case 1
				If step=0 Then
					For each xxx in RightGI:xxx.state=0:Next
				Else
					For each xxx in RightGI:xxx.state=1:Next
				End If
				For each xxx in RightGI:xxx.IntensityScale = 0.125 * step:next
			Case 2
				If step=0 Then
					For each xxx in TopGI:xxx.state=0:Next
				Else
					For each xxx in TopGI:xxx.state=1:Next
				End If
				For each xxx in TopGI:xxx.IntensityScale = 0.125 * step:next
				For each xxx in BWGI:xxx.IntensityScale = 0.125 * step:next
				If Step>=7 Then Table1.ColorGradeImage = "ColorGrade_8":Else Table1.ColorGradeImage = "ColorGrade_" & (step+1):End If
		End Select
	Else
		Select Case nr
			Case 0 For each xxx in LeftGI:xxx.state=step:Next
					If Step>0 Then Table1.ColorGradeImage = "ColorGrade_8":Else Table1.ColorGradeImage = "ColorGrade_1":End If
			Case 1 For each xxx in RightGI:xxx.state=step:Next
			Case 2 For each xxx in TopGI:xxx.state=step:Next:For each xxx in BWGI:xxx.visible = step:next
		End Select
	End If
End Sub

' *********************************************************************
' 			Left and Right Orbits Hack
' *********************************************************************
'slow down the ball when shooting at loops (orbis)
Sub LoopHelpL_Unhit():If ActiveBall.VelY > 20 Then ActiveBall.VelY = RndNum(18,20):End If:End Sub
Sub LoopHelpR_Unhit():If ActiveBall.VelY > 20 Then ActiveBall.VelY = RndNum(18,20):End If:End Sub

' *********************************************************************
'					Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 800)
End Function

Function AudioPan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function AudioFade(ball)
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

'*********************************************************************
'                 Positional Sound Playback Routines
'*********************************************************************

Sub PlaySoundXY(soundname, loopcount, volume, useexisting, tableobj)
	If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
		PlaySound soundname, loopcount, volume, AudioPan(tableobj), 0, Pitch(tableobj), useexisting, 0, AudioFade(tableobj)
	Else
		PlaySound soundname, loopcount, volume, AudioPan(tableobj), 0, Pitch(tableobj), useexisting, 0
	End If
End Sub

Sub PlaySoundAt(soundname, tableobj)
	If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
		PlaySound soundname, 1, 1, AudioPan(tableobj), 0, 0, 0, 1, AudioFade(tableobj)
	Else
		PlaySound soundname, 1, 1, AudioPan(tableobj)
	End If
End Sub

Sub PlaySoundAtBall(soundname)
	If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
		PlaySound soundname, 1, 1, AudioPan(ActiveBall), 0, 0, 0, 1, AudioFade(ActiveBall)
	Else
		PlaySound soundname, 1, 1, AudioPan(ActiveBall)
	End If
End Sub

Sub PlaySoundAtBallLouder(soundname)
	If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
		PlaySound soundname, 0, 10*Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Else
		PlaySound soundname, 0, 10*Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End If
End Sub

Sub PlayLoopSoundAt(soundname, tableobj)
	If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
		PlaySound soundname, -1, 1, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
	Else
		PlaySound soundname, -1, 1, AudioPan(tableobj), 0, 0, 1, 0
	End If
End Sub

'*****************
'* Maths
'*****************

Const Pi = 3.1415927

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
	if ABS(dSin) < 0.000001 Then dSin = 0
	if ABS(dSin) > 0.999999 Then dSin = 1' * sgn(dSin)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
	if ABS(dCos) < 0.000001 Then dCos = 0
	if ABS(dCos) > 0.999999 Then dCos = 1' * sgn(dCos)
End Function

Function RndNum(min, max)
    RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

' *********************************************************************
'       RealTime Updates
' *********************************************************************
Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
sw65p.rotx = sw65flip.currentangle
sw68p.rotx = sw68flip.currentangle
sw83p.rotx = sw83flip.currentangle
sw84p.rotx = sw84flip.currentangle
sw85p.rotx = sw85flip.currentangle
FlipperL.rotz=LeftFlipper.currentangle
FlipperR.rotz=RightFlipper.currentangle
FlipperLsh.rotz=LeftFlipper.currentangle
FlipperRsh.rotz=RightFlipper.currentangle
sw82p.rotx = BartRampG.currentangle
sw73p.rotx = SaloonG.currentangle
LLoopGateSw.roty = LLoopGate.currentangle
RLoopGateSw.roty = RLoopGate.currentangle
RollingSoundUpdate
'BallShadowUpdate
End Sub

'*********** ROLLING SOUND *********************************
Const tnob = 5						' total number of balls : 4 (trough) + 1 (Fake Ball)
Const fakeballs = 1					' number of balls created on table start (rolling sound and ballshadow will be skipped)
ReDim rolling(tnob-fakeballs)

Sub InitRolling:Dim i:For i=0 to (tnob-fakeballs-1):rolling(i) = False:Next:End Sub

Sub RollingSoundUpdate()
    Dim BOT, b
    BOT = GetBalls
	' stop the sound of deleted balls
	If UBound(BOT)<(tnob - 1) Then
		For b = (UBound(BOT)-fakeballs + 2) to (tnob-fakeballs)
			rolling(b-1) = False
			StopSound("fx_ballrolling" & b)
		Next
	End If
	' exit the Sub if no balls on the table
    If UBound(BOT) = fakeballs-1 Then Exit Sub
	' play the rolling sound for each ball
    For b = fakeballs to UBound(BOT)
        If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
            rolling(b-fakeballs) = True
			PlaySoundXY "fx_ballrolling" & (b-fakeballs+1), -1, Vol(BOT(b))/4, 1, BOT(b)
        Else
            If rolling(b-fakeballs) = True Then
                StopSound("fx_ballrolling" & (b-fakeballs+1))
                rolling(b-fakeballs) = False
            End If
        End If
    Next
End Sub

'*********** BALL SHADOW *********************************
Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3,Ballshadow4)

Sub BallShadowUpdate()
    Dim BOT, b
    BOT = GetBalls
	' hide shadow of deleted balls
	If UBound(BOT)<(tnob-1) Then
		For b = (UBound(BOT)-fakeballs + 2) to (tnob-fakeballs)
			BallShadow(b-1).visible = 0
		Next
	End If
	' exit the Sub if no balls on the table
    If UBound(BOT) = fakeballs-1 Then Exit Sub
	' render the shadow for each ball
    For b = fakeballs to UBound(BOT)
		If BOT(b).X < Table1.Width/2 Then
			BallShadow(b-fakeballs).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/10)) + 10
		Else
			BallShadow(b-fakeballs).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/10)) - 10
		End If
	    ballShadow(b-fakeballs).Y = BOT(b).Y + 15
		If BOT(b).Z > 20 Then
			BallShadow(b-fakeballs).visible = 1
		Else
			BallShadow(b-fakeballs).visible = 0
		End If
	Next
End Sub

'**********************
' Balldrop & Ramp Sound
'**********************

Sub Balldrop1_Hit() : StopSound "fx_leftramp_exit" : Me.TimerInterval= 200: Me.TimerEnabled=1: End Sub
Sub Balldrop1_Timer(): Me.TimerEnabled=0: PlaySoundAt "fx_balldrop", Balldrop1: End Sub

Sub Balldrop2_Hit() : StopSound "fx_leftramp_exit" : Me.TimerInterval= 150: Me.TimerEnabled=1: End Sub
Sub Balldrop2_Timer(): Me.TimerEnabled=0: PlaySoundAt "fx_balldrop", Balldrop2: End Sub

Sub LREnter_Hit(): If ActiveBall.VelY < 0 Then PlaySoundAtBall "fx_leftramp_enter": End If: End Sub
Sub LREnter_UnHit(): If ActiveBall.VelY > 0 Then StopSound "fx_leftramp_enter": End If: End Sub

Sub RREnter_Hit(): If ActiveBall.VelY < 0 Then PlaySoundAtBall "fx_leftramp_enter": End If: End Sub
Sub RREnter_UnHit(): If ActiveBall.VelY > 0 Then StopSound "fx_leftramp_enter": End If: End Sub

Sub LREnter1_Hit() : PlaySoundAtBall "fx_leftramp_exit" : End Sub

Sub RPin_Hit() : RandomSoundMetal() : End Sub
Sub LPin_Hit() : RandomSoundMetal() : End Sub
Sub Trigger1_Hit() : StopSound "fx_launch" : PlaySoundAtBall "fx_turn_SSRamp" :End Sub
Sub Trigger2_Hit() : StopSound "fx_launch" : PlaySoundAtBall "fx_balldrop_SSRamp" : End Sub
Sub Trigger3_Hit() : PlaySoundAtBall "fx_wireramp" : End Sub
Sub Trigger4_Hit() : StopSound "fx_wireramp" : End Sub
Sub Trigger5_Hit() : PlaySoundAtBall "fx_wireramp" : End Sub
Sub Trigger6_Hit() : StopSound "fx_wireramp" : Me.TimerInterval= 200: Me.TimerEnabled=1: End Sub
Sub Trigger6_Timer() : Me.TimerEnabled=0: PlaySoundAt "fx_balldrop_station", Trigger6: End Sub
Sub Trigger7_Hit() : If BIP > 0 And ActiveBall.VelY < 0 Then PlaySoundAtBall "fx_launch" : Else StopSound "fx_launch" : End If : End Sub

' *********************************************************************
' 						Other Sound FX
' *********************************************************************

'Ball Collision
Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySoundXY "fx_collide", 0, Csng(velocity) ^2 / 500, 0, ball1
End Sub

'Gates Collision
Sub LLoopGate_Hit() : RandomSoundMetal() : End Sub
Sub RLoopGate_Hit() : RandomSoundMetal() :End Sub

'Flippers Collision
Sub LeftFlipper_Collide(parm):RandomSoundFlipper:End Sub
Sub Rightflipper_Collide(parm):RandomSoundFlipper:End Sub

'Rubber Posts Collision
Sub Posts_Hit()
 	dim finalspeed
  	finalspeed=SQR((activeball.velx ^2) + (activeball.vely ^2))
 	If finalspeed > 20 then 
		PlaySoundAtBallLouder "fx_rubber"
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

'Rubber Collision
Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR((activeball.velx ^2) + (activeball.vely ^2))
 	If finalspeed > 20 then 
		PlaySoundAtBallLouder "fx_rubber"
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case RndNum(1,3)
		Case 1 : PlaySoundAtBallLouder "fx_rubber_hit_1"
		Case 2 : PlaySoundAtBallLouder "fx_rubber_hit_2"
		Case 3 : PlaySoundAtBallLouder "fx_rubber_hit_3"
	End Select
End Sub

Sub RandomSoundFlipper()
	Select Case RndNum(1,3)
		Case 1 : PlaySoundAtBallLouder "fx_flip_hit_1"
		Case 2 : PlaySoundAtBallLouder "fx_flip_hit_2"
		Case 3 : PlaySoundAtBallLouder "fx_flip_hit_3"
	End Select
End Sub

Sub RandomSoundMetal()
	Select Case RndNum(1,3)
		Case 1 : PlaySoundAtBallLouder "fx_metal_hit_1"
		Case 2 : PlaySoundAtBallLouder "fx_metal_hit_2"
		Case 3 : PlaySoundAtBallLouder "fx_metal_hit_3"
	End Select
End Sub

'******************************************************************************************
'* TABLE OPTIONS **************************************************************************
'******************************************************************************************

'**** New Options Selection through F6 menu ***

Dim FlipperType, ScoopLight, ColorMod, BadGuysMod, BeerMugMod

'REGISTRY LOCATIONS ***************************************************************************************************************************************
 
 Const optOpenAtStart	= &H00001
 Const optBeerMug	= &H00010
 Const optFlip		= &H00020
 Const optScoop		= &H00080
 Const optGIMod		= &H00100
 Const optBadGuys	= &H00200

'OPTIONS MENU *********************************************************************************************************************************************

Dim TableOptions, TableName, optReset
Private vpmShowDips1, vpmDips1

 Sub InitializeOptions
	'Replace with your descriptive table name, it will be used to save settings in VPReg.stg file
	If PROC = 1 Then
		TableName="CCC_TTNZ"
	Else
		TableName="CC_TTNZ"
	End If
	Set vpmShowDips1 = vpmShowDips								'Reassigns vpmShowDips to vpmShowDips1 to allow usage of default dips menu
	Set vpmShowDips = GetRef("TableShowDips")					'Assigns new sub to vmpShowDips
 	TableOptions = LoadValue(TableName,"Options")				'Load saved table options
	Set Controller = CreateObject("VPinMAME.Controller")		'Load vpm controller temporarily so options menu can be loaded if needed
	If TableOptions = "" Or optReset Then						'If no existing options, reset to default through optReset, then open Options menu
		TableOptions = optOpenAtStart + 2*optFlip				'clear any existing settings and set table options to default options
		TableShowOptions
	ElseIf (TableOptions And optOpenAtStart) Then				'If Enable Next Start was selected then
		TableOptions = TableOptions - optOpenAtStart			'clear setting to avoid future executions
		TableShowOptions
	Else
		TableSetOptions
	End If
	Set Controller = Nothing									'Unload vpm controller so selected controller can be loaded
 End Sub

Private Sub TableShowDips
	vpmShowDips1												'Show original Dips menu
	TableShowOptions											'Show new options menu
End Sub

 Private Sub TableShowOptions					'New options menu, additional menus can be added as well, just follow similar format and add call to TableShowDips
   Dim oldOptions : oldOptions = TableOptions
	If Not IsObject(vpmDips1) Then				'If creating an additional menus, need to declare additional vpmDips variables above (ex. vpmDips2 and TableOptions2, etc.)
		Set vpmDips1 = New cvpmDips
		With vpmDips1
			.AddForm 630, 250, "TABLE OPTIONS MENU (Press " & vpmKeyName(keyShowDips) & " to open this menu)"
			.AddFrameExtra 0,0,155,"Beer Mug Mod",optBeerMug, Array("No", 0*optBeerMug, "Yes", 1*optBeerMug)
			.AddFrameExtra 0,50,155,"Flippers Type",3*optFlip, Array("White", 0*optFlip, "Yellow", 1*optFlip, "Random", 2*optFlip)
			.Addlabel 0,125,125,41,"TIP: To Reset to defaults,open table script and uncomment line 31"
			.AddChkExtra 0,180,155,Array("Enable Menu Next Start", optOpenAtStart)
			.AddFrameExtra 175,0,155,"Red Light under the scoop",optScoop, Array("No", 0*optScoop, "Yes", 1*optScoop)
			.AddFrameExtra 175,50,155,"G.I Mod",optGIMod, Array("No", 0*optGIMod, "Yes", 1*optGIMod)
			.AddFrameExtra 175,100,155,"Bad Guys Flashing lights", optBadGuys, Array("No", 0*optBadGuys, "Yes", 1*optBadGuys)
		End With
	End If
	TableOptions = vpmDips1.ViewDipsExtra(TableOptions)
	SaveValue TableName,"Options",TableOptions
	TableSetOptions
 End Sub

'MENU ENTRYS *********************************************************************************************************************************************

 Sub TableSetOptions		'defines required settings before table is run
	BeerMugMod = (TableOptions And optBeerMug)\optBeerMug
	FlipperType = (TableOptions And 3*optFlip)\optFlip
	ScoopLight = (TableOptions And optScoop)\optScoop
	ColorMod = (TableOptions And optGIMod)\optGIMod
	BadGuysMod = (TableOptions And optBadGuys)\optBadGuys
	SaveValue TableName,"Options",TableOptions
	GetOptions
 End Sub

Sub GetOptions
	Dim xx
	ScoopL.visible = ScoopLight
	CabRail.visible = DesktopMode
	light58.visible = DesktopMode
	light59.visible = NOT DesktopMode

	If FlipperType = 2 then FlipperType = RndNum(0, 1)

	Select Case FlipperType
		Case 0:	FlipperL.image = "flippers white"
				FlipperR.image = "flippers white"
		Case 1:	FlipperL.image = "flippers"
				FlipperR.image = "flippers"
	End Select

	For Each xx in BadGuysLights : xx.visible = BadGuysMod : Next

	Select Case ColorMod
		Case 0:	For xx=0 to (RedGI.count-1) : RedGI(xx).color = RGB (255, 200, 128) : RedGI(xx).intensity = RedGis(xx) : Next
				For Each xx in GreenGI : xx.color = RGB (255, 200, 128) : Next
				For xx=0 to (BlueGI.count-1) : BlueGI(xx).color = RGB (255, 200, 128) : BlueGI(xx).intensity = BlueGis(xx): Next

		Case 1:	For Each xx in RedGI : xx.color = RGB (255, 0, 0) : xx.intensity = 65 : Next
				For Each xx in GreenGI : xx.color = RGB (0, 255, 0) : Next
				For Each xx in BlueGI : xx.color = RGB (0, 0, 255) : xx.intensity = 65: Next
	End Select

End Sub

'******************
' Rom sound ON wwhen close table
'******************
Sub Table1_Exit()
    Controller.Pause = False
    Controller.Stop
    Controller.Games("cc_13").Settings.Value("sound") = 1
    Controller.Games("cc_13").Settings.Value("samples") = 1
End Sub