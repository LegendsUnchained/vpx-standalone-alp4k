'
'                    **************************************************************************
'                    *                                                                        *
'                    *                          STERN PINBALL, INC.                           *
'                    *             (C) COPYRIGHT 2011 - 2017 STERN PINBALL, INC.              *
'                    *                                                                        *
'                    **************************************************************************
'                    *                                                                        *
'                    *                     		AC/DC Pro Vault Edition		                  *
'                    *                                                                        *
'                    **************************************************************************
'
'             *                      *                                       *********                      *         
'            ***                    ***                                     ***********                    ***        
'           *****                  *****                                   *************                  *****       
'          *******                *******                                 ***************                *******      
'         *********              *********                               *****************              *********     
'        ***********            ***********                             *******************            ***********    
'       *************          *************                            ********************          *************   
'      ***************        ***************                            ********************        ***************  
'     *****************      *****************                            ******** ***********      ***************** 
'    ****** ************    ******* ***********             ********      ********  **********     ******* ***********
'   *******  ***********    *******  **********             ********      ********   *********     *******  **********
'   *******   **********    *******   ********             ********       ********    ********     *******   ******** 
'   *******    *********    *******    ******              ********       ********     *******     *******    ******  
'   *******     ********    *******     ****              *********       ********     *******     *******     ****   
'   *******      *******    *******      **               ********        ********     *******     *******      **    
'   *******      *******    *******                      *********        ********     *******     *******            
'   *******      *******    *******                      *********        ********     *******     *******            
'   *******      *******    *******                      ********         ********     *******     *******            
'   *******      *******    *******                     *********         ********     *******     *******            
'   *******      *******    *******                     ********          ********     *******     *******            
'   *******      *******    *******                    *********          ********     *******     *******            
'   *******      *******    *******                    ***************    ********     *******     *******            
'   ********************    *******                   ***************     ********     *******     *******            
'   ********************    *******                   **************      ********     *******     *******            
'   ********************    *******                   **************      ********     *******     *******            
'   ********************    *******                  **************       ********     *******     *******            
'   ********************    *******                  *************        ********     *******     *******            
'   ********************    *******                 *************         ********     *******     *******            
'   ********************    *******                       *******         ********     *******     *******            
'   ********************    *******       *               ******          ********     *******     *******      **    
'   *******      *******    *******      ***             ******           ********     *******     *******     ****   
'   *******      *******    *******     *****            ******           ********     *******     *******    ******  
'   *******      *******    *******    *******           *****            ********     *******     *******   ******** 
'   *******      *******    ********  *********         *****             ********    ********     *******  **********
'   *******      *******    *******************         ****              ********   *********     *******************
'  ********     ********    ******************          ****              ********  **********     ****************** 
' **********   **********    ****************          ****               ********************      ****************  
'************ ************    **************           ***               ********************        **************   
' *********** ************     ************            ***              ********************          ************    
'  *********   **********       **********            ***               *******************            **********     
'   *******     ********         ********             **                 *****************              ********      
'    *****       ******           ******              *                   ***************                ******       
'     ***         ****             ****              **                    *************                  ****        
'      *           **               **               *                      ***********                    **         
'                                                   *                                                                 
'VPX recreation by ninuzzu
'Thanks to:
'dark for the bell base model;
'knorr for the laser mod;
'RustyCardores for the Surround Sound MOD;
'rysr, Peter J and javier for the help with the pics.
'the VPDevTeam for the freaking amazing VPX

Option explicit
Randomize

'************************************************************************
'							TABLE OPTIONS
'************************************************************************

Const HelenMod = 0					'Enable Helen Mod, with new artwork (0= no, 1= yes)
Const LaserMod = 0					'Shows a laser beam when Cannon is moving (0= no, 1= yes)
Const RampsDecals = 0				'Shows Custom Ramps Decals (0= no, 1= yes)
Const FlippersDecals = 0			'Shows Custom Flippers Decals (0= no, 1= yes)
Const SideCabDecals = 0				'Shows Custom Cabinet Decals (0= no, 1= yes)
Const FIREButtonLight = 0			'Light the apron decal when FIRE button is on, useful for pincabs (0= no, 1= yes)

'************************************************************************
'						END OF TABLE OPTIONS
'************************************************************************

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const Ballsize = 51
Const BallMass = 1.1

Const cGameName = "acd_170"

Const UseVPMModSol = True
Dim UseVPMDMD,CustomDMD,DesktopMode
DesktopMode = ACDC.ShowDT : CustomDMD = False
If Right(cGamename,1)="c" Then CustomDMD=True
If CustomDMD OR (NOT DesktopMode AND NOT CustomDMD) Then UseVPMDMD = False		'hides the internal VPMDMD when using the color ROM or when table is in Full Screen and color ROM is not in use
If DesktopMode AND NOT CustomDMD Then UseVPMDMD = True							'shows the internal VPMDMD when in desktop mode and color ROM is not in use
Scoretext.visible = NOT CustomDMD												'hides the textbox when using the color ROM

LoadVPM "02800000", "Sam.VBS", 3.54

'********************
'Standard definitions
'********************

Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0

'Standard Sounds
Const SSolenoidOn = "fx_Solon"
Const SSolenoidOff = ""
Const SCoin = "fx_coin"

'************************************************************************
'						 INIT TABLE
'************************************************************************

Dim bsTrough, bsTEject, PlungerIM

Sub ACDC_Init
	vpmInit Me
    With Controller
        .GameName = cGameName
        .SplashInfoLine = "AC/DC Pro Vault Edition (Stern 2017)"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        If NOT CustomDMD Then .Hidden = DesktopMode				'hides the external DMD when in desktop mode and color ROM is not in use
        .HandleMechanics = 0
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With

'Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

'Nudging
    vpmNudge.TiltSwitch = -7
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

'Trough
    Set bsTrough = New cvpmTrough
    With bsTrough
		.Size = 4
		.InitSwitches Array(21, 20, 19, 18)
		.InitExit BallRelease, 70, 15
		.InitEntrySounds "fx_drain", SoundFX(SSolenoidOn,DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
		.InitExitSounds  SoundFX(SSolenoidOn,DOFContactors), SoundFX("fx_ballrel",DOFContactors)
		.Balls = 4
		.CreateEvents "bsTrough", Drain
    End With

'Top Eject
    Set bsTEject = new cvpmSaucer
    With bsTEject
		.InitKicker Sw37, 37, 90, 20, 0
		.InitExitVariance 0, 1
        .InitSounds "fx_kicker_enter", SoundFX(SSolenoidOn,DOFContactors), SoundFX("ExitSandman",DOFContactors)
        .CreateEvents "bsTEject", Sw37
    End With

'Impulse Plunger
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP sw23, 65, 0.5
        .Switch 23
        .Random 1.5
        .InitExitSnd SoundFX("fx_AutoPlunger",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
        .CreateEvents "plungerIM"
    End With

'Other Suff
	InitOptions:InitCannon:InitDiverters:InitBell

'Fast Flips
	On Error Resume Next 
	InitVpmFFlipsSAM
	If Err Then MsgBox "You need the latest sam.vbs in order to run this table, available with vp10.5 rev3434"
	On Error Goto 0
End Sub

'************************************************************************
'							KEYS
'************************************************************************

Sub ACDC_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 3:PlaySoundAt SoundFX("fx_nudge", 0), sw24
    If keycode = RightTiltKey Then Nudge 270, 3:PlaySoundAt SoundFX("fx_nudge", 0), sw29
    If keycode = CenterTiltKey Then Nudge 0, 4:PlaySoundAt SoundFX("fx_nudge", 0), Drain
	If KeyCode = PlungerKey Then Plunger.Pullback: PlaysoundAt "fx_PlungerPull",Plunger
	If KeyCode = RightMagnaSave OR KeyCode = LockbarKey Then Controller.Switch(64)=1				'FIRE Button, mapped to the right magna save
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub ACDC_KeyUp(ByVal Keycode)
	If KeyCode = PlungerKey Then Plunger.Fire: StopSound "fx_PlungerPull":PlaySoundAt "fx_Plunger",Plunger
	If KeyCode = RightMagnaSave OR KeyCode = LockbarKey Then Controller.Switch(64)=0				'FIRE Button, mapped to the right magna save
    If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub ACDC_Paused:Controller.Pause = 1:End Sub
Sub ACDC_UnPaused:Controller.Pause = 0:End Sub
Sub ACDC_Exit:Controller.Stop:End Sub

'************************************************************************
'						 SOLENOIDS
'************************************************************************

SolCallBack(1) = "SolTrough"						'Trough-Up Kicker
SolCallBack(2) = "SolAutoPlungerIM"					'AutoLaunch
SolCallback(3) = "SolCannon"						'Cannon Eject
SolCallback(4) = "SolCannonDiv"						'Cannon Diverter
SolCallback(5) = "vpmSolgate Gate, SoundFX(SSolenoidOn,DOFContactors),"			'Right Control Gate
SolCallback(8) = "vpmSolSound SoundFX(""ShakerPulse"",DOFShaker),"				'Shaker Motor
SolCallback(9) = ""									'Left Bumper
SolCallback(10)= ""									'Right Bumper			
SolCallback(11)= ""									'Top Bumper
SolCallback(12)= "SolTopEject"						'Top Eject
SolCallback(13)= ""									'Left Sling
SolCallback(14)= ""									'Right Sling
SolCallback(15)= "SolLFlipper"						'Left Flipper
SolCallback(16)= "SolRFlipper"						'Right Flipper
SolModCallBack(17)= "SetLampMod 177,"				'Flasher: Train
SolModCallBack(20)= "SetLampMod 180,"				'Flasher: Left Ramp
SolModCallBack(21)= "SetLampMod 181,"				'Flasher:Left Side
SolModCallBack(22)= "SetLampMod 182,"				'Flasher:BackPanel
SolModCallBack(23)= "SetLampMod 183,"				'Flasher: Top Eject
SolCallBack(24)= "vpmSolSound SoundFX(""fx_knocker"",DOFKnocker),"				'Knocker
SolModCallBack(25)= "SetLampMod 185,"				'Flasher:Pop Bumpers (x3)
SolModCallBack(26)= "SetLampMod 186,"				'Flasher: Bell Arrow
SolModCallBack(27)= "SetLampMod 187,"				'Flasher:Left Ramp Left
SolModCallBack(28)= "SetLampMod 188,"				'Flasher:Left Ramp Right
SolModCallBack(29)= "SetLampMod 189,"				'Flasher:Right Ramp Right
SolModCallBack(30)= "SetLampMod 190,"				'Flasher:Right Ramp
SolModCallBack(31)= "SetLampMod 191,"				'Flasher:Right Side
SolCallback(32)= "SolCannonMotor"					'Cannon Motor

'************************************************************************
' 							FLIPPERS
'************************************************************************
Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers),LeftFlipper
        LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers),LeftFlipper
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers),RightFlipper
        RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers),RightFlipper
        RightFlipper.RotateToStart
    End If
End Sub

'************************************************************************
'						 BALL TROUGH
'************************************************************************
Sub SolTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		If BsTrough.Balls Then vpmTimer.PulseSw 22
	End If
End Sub

'************************************************************************
'						 AUTOPLUNGER 
'************************************************************************
Sub SolAutoPlungerIM(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
End Sub

'************************************************************************
'						 EXIT JUKEBOX
'************************************************************************
Sub SolTopEject(enabled)
	If Enabled Then
		bsTEject.ExitSol_On
		Gate.elasticity = RndNum(6,10)/10     ' Sets a random elasticity between 0.6 and 1
	End If
End Sub

'************************************************************************
' 						CANNON DIVERTER
'************************************************************************
Dim Div1Hit

Sub InitDiverters
	Div1.Isdropped = 1
	Div1Hit=0
End Sub

Sub SolCannonDiv(Enabled)
    If Enabled Then 
        Div1.Isdropped = 0
        Div2.Isdropped = 1
        DivP.Z = 142
        PlaySoundAt SoundFX("DiverterOn", DOFContactors),DivP
    Else 
        Div1.Isdropped = 1
        Div2.Isdropped = 0
        DivP.Z = 172.5
        PlaySoundAt SoundFX("DiverterOff", DOFContactors),DivP
    End If
End Sub

'************************************************************************
' 						CANNON ENTER
'************************************************************************
Const Pi=3.1415926535
Dim GPos, GDir, BallInGun, BallInGunRadius, LaserON
BallInGunRadius = SQR((Cannon_assyM.X - Sw45.X)^2 + (Cannon_assyM.Y - Sw45.Y)^2)
GPos = 110: GDir = -1

Sub Sw45_hit()
	RandomSoundMetal
	Controller.switch(45) = 1
	Set BallInGun = ActiveBall
End Sub

'************************************************************************
' 						CANNON MOTOR
'************************************************************************
Sub InitCannon
    Controller.switch(61) = 1
    Controller.switch(62) = 0
End Sub

Sub SolCannonMotor(Enabled)
    If Enabled Then
		GDir = -1
		UpdateCannon.Enabled = 1
		PlaySoundAt SoundFX("CannonMotor", DOFGear),Cannon_assyM
		If LaserMod = 1 then
			LaserR.Size_Z = 0.5
			LaserR1.Size_Z = 0.5
			LaserR.visible = 1
			LaserR1.visible = 1
			LaserZTimer.Enabled = 1
			LaserON = 1
		End if
     Else
		UpdateCannon.Enabled = 0
		StopSound "CannonMotor"
		LaserON = 0
  End If
End Sub

 Sub UpdateCannon_Timer()
	GPos = GPos + GDir
	If GPos < 110 AND GDir=-1 Then Controller.switch(61) = 0
	If GPos <= 80 Then Controller.switch(62) = 1
	If GPos <= 20 Then GDir = 1
	If GPos > 80 AND GPos < 110 AND GDir=1 Then Controller.switch(61) = 0: Controller.switch(62) = 0
	If GPos >= 110 Then GPos=110:GDir = -1: Controller.switch(61) = 1: Controller.switch(62) = 0

	Cannon_assyM.RotZ = GPos
	Cannon_decalM.RotZ = GPos
	Cannon_assyP.RotZ = GPos
	Cannon_decalP.RotZ = GPos
	Cannon_Plunger.RotZ = GPos
	Cannon_shadow.RotZ = GPos
	LaserR.objRotZ = -(GPos-90)
	LaserR1.objRotZ = -(Gpos -90)
	If Not IsEmpty(BallInGun) Then
		BallInGun.X = Cannon_assyM.X - BallInGunRadius * Cos(GPos*Pi/180)
		BallInGun.Y = Cannon_assyM.Y - BallInGunRadius * Sin(GPos*Pi/180)
		BallInGun.Z = CannonRamp.HeightTop + Ballsize
	End If
 End Sub

'***LaserZTimer***

Sub LaserZTimer_Timer()
	If GPos >= 102 And GPos <= 110 then LaserR.Size_Z = 0.5: LaserR1.Size_Z = 0.5
	If GPos <= -101.99 And GPos >= 97 then LaserR.Size_Z = 1: LaserR1.Size_Z = 1
	If GPos <= -96.99 And GPos >= 89.01 then LaserR.Size_Z = 0.55: LaserR1.Size_Z = 0.55
	If GPos <= 89 And GPos >= 81.01 then LaserR.Size_Z = 0.48: LaserR1.Size_Z = 0.48
	If GPos <= 81 And GPos >= 73.01 then LaserR.Size_Z = 0.78: LaserR1.Size_Z = 0.78
	If GPos <= 73 And GPos >= 65.01 then LaserR.Size_Z = 0.7: LaserR1.Size_Z = 0.7
	If GPos <= 65 And GPos >= 45.01 then LaserR.Size_Z = 0.6: LaserR1.Size_Z = 0.6
	If GPos <= 45 And GPos >= 42.01 then LaserR.Size_Z = 0.43: LaserR1.Size_Z = 0.43
	If GPos <= 42 And GPos >= 40.01 then LaserR.Size_Z = 0.425: LaserR1.Size_Z = 0.425
	If GPos <= 40 And GPos >= 37.01 then LaserR.Size_Z = 0.41: LaserR1.Size_Z = 0.41
	If GPos <= 37 And GPos >= 30.01 then LaserR.Size_Z = 0.39: LaserR1.Size_Z = 0.39
	If GPos <= 30 And GPos >= 22.01 then LaserR.Size_Z = 0.385: LaserR1.Size_Z = 0.385
	If GPos <= 22  then LaserR.Size_Z = 0.385: LaserR1.Size_Z = 0.375
	If GPos >= 110 And LaserON = 0 Then
		LaserR.Visible = 0
		LaserR1.Visible = 0
		LaserZTimer.Enabled = 0
	End if
End Sub

'************************************************************************
' 						CANNON KICKOUT
'************************************************************************
 Sub SolCannon(Enabled)
	If Enabled Then
		PlaySoundAt SoundFX("CannoShot", DOFContactors),Cannon_assyM
		Cannon_Plunger.PlayAnim 0, 1
		If NOT IsEmpty(BallInGun) AND Gpos < 100 Then
			Sw45.kick GPos-90, 45
			Controller.switch(45) = 0
			BallInGun = Empty
		End If
	End If
End Sub

'************************************************************************
' 					BELL SWINGING ANIMATION
'************************************************************************

Sub InitBell
	sw36.TimerEnabled=0
	sw36.TimerEnabled=1
	sw36.TimerInterval=10
End Sub

Sub sw36_timer
	Bell.RotX = -SpinnerBell.CurrentAngle
	Bell_Support.RotX = Bell.RotX: Bell_Support1.RotX= Bell.RotX
End Sub

Sub sw36_Hit : Controller.Switch(36) = 1 : PlaySoundAtBall "fx_collide": End Sub
Sub sw36_Unhit : Controller.Switch(36) = 0 : End Sub

'************************************************************************
'						SWITCHES
'************************************************************************

'Targets AC/DC
Sub Sw1_Hit():vpmTimer.PulseSw 1: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub
Sub Sw2_Hit():vpmTimer.PulseSw 2: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub
Sub Sw3_Hit():vpmTimer.PulseSw 3: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub
Sub Sw4_Hit():vpmTimer.PulseSw 4: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub
Sub Sw5_Hit():vpmTimer.PulseSw 5: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub

'Targets ROCK
Sub Sw6_Hit():vpmTimer.PulseSw 6: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub
Sub Sw7_Hit():vpmTimer.PulseSw 7: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub
Sub Sw8_Hit():vpmTimer.PulseSw 8: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub
Sub Sw9_Hit():vpmTimer.PulseSw 9: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub

'Targets TNT
Sub Sw10_Hit():vpmTimer.PulseSw 10: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub
Sub Sw11_Hit():vpmTimer.PulseSw 11: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub
Sub Sw12_Hit():vpmTimer.PulseSw 12: PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub

'Left Ramp
Sub Sw13_Hit:Controller.Switch(13)=1: End Sub
Sub Sw13_UnHit:Controller.Switch(13)=0: End Sub

Sub Sw14_Hit:Controller.Switch(14) = 1:End Sub 
Sub Sw14_UnHit():Controller.Switch(14) = 0:End Sub

'Bottom Lane Rollovers
Sub sw24_Hit:Controller.Switch(24) = 1:PlaySoundAt "rollover",Sw24:End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:PlaySoundAt "rollover",Sw25:End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub

Sub sw28_Hit:Controller.Switch(28) = 1:PlaySoundAt "rollover",Sw28:End Sub
Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub

Sub sw29_Hit:Controller.Switch(29) = 1:PlaySoundAt "rollover",Sw29:End Sub
Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub

' Slingshots
Dim LStep, RStep

Sub LeftSlingShot_Slingshot()
    PlaySoundAt SoundFX("LeftSlingShot", DOFContactors),lemk 
    LeftSling4.Visible = 1
    Lemk.TransZ = -20
    LStep = 0
    vpmTimer.PulseSw 26
    Me.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer()
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.TransZ = -12
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.TransZ = -5
        Case 3:LeftSLing2.Visible = 0:Lemk.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot()
    PlaySoundAt SoundFX("RightSlingShot", DOFContactors),remk
    RightSling4.Visible = 1
    Remk.TransZ = -20
    RStep = 0
    vpmTimer.PulseSw 27
    Me.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer()
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.TransZ = -12
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.TransZ = -5
        Case 3:RightSLing2.Visible = 0:Remk.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' Bumpers
Sub Bumper1_Hit():vpmTimer.PulseSw 30:PlaySoundAtBumperVol SoundFX("LeftBumper_Hit", DOFContactors),Bumper1,2:End Sub
Sub Bumper2_Hit():vpmTimer.PulseSw 31:PlaySoundAtBumperVol SoundFX("RightBumper_Hit", DOFContactors),Bumper2,2:End Sub
Sub Bumper3_Hit():vpmTimer.PulseSw 32:PlaySoundAtBumperVol SoundFX("TopBumper_Hit", DOFContactors),Bumper3,2:End Sub

' Spinner
Sub Sw33_Spin():vpmTimer.PulseSw 33:PlaySoundAt "fx_spinner",ActiveBall : End Sub

'Targets Thunder
Sub Sw34_Hit():vpmTimer.PulseSw 34:PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub
Sub Sw35_Hit():vpmTimer.PulseSw 35:PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub
Sub Sw42_Hit():vpmTimer.PulseSw 42:PlaySoundAtBallVol SoundFX("fx_target",DOFTargets),2: End Sub

'Top Lane Rollovers
Sub sw38_Hit:Controller.Switch(38) = 1:PlaySoundAt "rollover",Sw38:End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub
Sub sw39_Hit:Controller.Switch(39) = 1:PlaySoundAt "rollover",Sw39:End Sub
Sub sw39_UnHit:Controller.Switch(39) = 0:End Sub
Sub sw40_Hit:Controller.Switch(40) = 1:PlaySoundAt "rollover",Sw40:End Sub
Sub sw40_UnHit:Controller.Switch(40) = 0:End Sub

'Right Ramp
Sub Sw41_Hit:Controller.Switch(41) = 1:End Sub 
Sub Sw41_UnHit:Controller.Switch(41) = 0:End Sub

Sub sw43_Hit:Controller.Switch(43)=1: End Sub
Sub sw43_UnHit:Controller.Switch(43)=0: End Sub

'Left Orbit
Sub sw44_Hit:Controller.Switch(44) = 1:End Sub
Sub sw44_UnHit:Controller.Switch(44) = 0:If Activeball.velX>20 Then Activeball.velX = Activeball.VelX*0.8:End If:End Sub

'Plunger Lane
Sub sw48_Hit:Controller.Switch(48) = 1:PlaySoundAt "rollover",Sw48:End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub

'Right Orbit
Sub sw59_Hit:Controller.Switch(59) = 1:End Sub
Sub sw59_UnHit:Controller.Switch(59) = 0:End Sub

'************************************************************************
'						GENERAL ILLUMINATION
'************************************************************************
Set GiCallBack = GetRef("UpdateGi")

Sub UpdateGi(nr,enabled)
	Dim ii
	Select Case nr
	Case 0
		If enabled Then
			DOF 103, DOFOn
			ACDC.ColorGradeImage="ColorGrade_on"
			For Each ii in BandMembersPoster: ii.IntensityScale=1:Next
			For each ii in GI_Red: ii.state=1:Next
			For each ii in GI_White: ii.state=1:Next
			For each ii in GI_Blue: ii.state=1:Next
			For Each ii in BLRLights: ii.IntensityScale=1:Next
			For Each bulb in BLBLights: bulb.IntensityScale=1:Next
			bulb1.BlendDisableLighting = 8
			bulb2.BlendDisableLighting = 8
			bulb3.BlendDisableLighting = 8
			bulb4.BlendDisableLighting = 8
			bulb5.BlendDisableLighting = 8
			bulb6.BlendDisableLighting = 8
			bulb7.BlendDisableLighting = 8
		Else
			DOF 103, DOFOff
			ACDC.ColorGradeImage="ColorGrade_off"
			For Each ii in BandMembersPoster: ii.IntensityScale=0:Next
			For each ii in GI_Red: ii.state=0:Next
			For each ii in GI_White: ii.state=0:Next
			For each ii in GI_Blue: ii.state=0:Next
			For Each bulb in BLRLights: bulb.IntensityScale=0:Next
			For Each bulb in BLBLights: bulb.IntensityScale=0:Next
			bulb1.BlendDisableLighting = 0
			bulb2.BlendDisableLighting = 0
			bulb3.BlendDisableLighting = 0
			bulb4.BlendDisableLighting = 0
			bulb5.BlendDisableLighting = 0
			bulb6.BlendDisableLighting = 0
			bulb7.BlendDisableLighting = 0
		End If
	End Select
End Sub

'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' 		With Modulated Flasher Routines (ninuzzu)
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim bulb
Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200), ModulationLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 10 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
	UpdateLamps
End Sub

Sub InitLamps()
    Dim x
    For x = 0 to 200
		LampState(x) = 0         ' current light state, independent of the fading level. 0 is off and 1 is on
		FadingLevel(x) = 4       ' used to track the fading state
		FlashSpeedUp(x) = 0.5    ' faster speed when turning on the flasher
		FlashSpeedDown(x) = 0.35 ' slower speed when turning off the flasher
		FlashMax(x) = 1          ' the maximum value when on, usually 1
		FlashMin(x) = 0          ' the minimum value when off, usually 0
		FlashLevel(x) = 0        ' the intensity of the flashers, usually from 0 to 1
		ModulationLevel(x) = 0		 ' the starting modulation level, from 0 to 255
    Next
End Sub

Sub UpdateLamps
'Inserts (PRO)
    nFadeL 3, l3
    nFadeL 4, l4
    nFadeL 5, l5
    nFadeL 6, l6
    nFadeL 7, l7
    nFadeL 8, l8
    nFadeL 9, l9
    nFadeL 10, l10
    nFadeL 11, l11
    nFadeL 12, l12
    nFadeL 13, l13
'    nFadeL 14, l14		'removed in the Pro Vault Edition
'    nFadeL 15, l15		'removed in the Pro Vault edition
'
'    nFadeL 17, l17		'removed in the Pro Vault edition
    nFadeL 18, l18
    nFadeL 19, l19
    nFadeL 20, l20
    nFadeL 21, l21
    nFadeL 22, l22
    nFadeL 23, l23
    nFadeL 24, l24
    nFadeL 25, l25
    nFadeL 26, l26
    nFadeL 27, l27
    nFadeL 28, l28
    nFadeL 29, l29
    nFadeL 30, l30
    nFadeL 31, l31
    nFadeL 32, l32
    nFadeL 33, l33
    nFadeL 34, l34
    nFadeL 35, l35
    nFadeL 36, l36
    nFadeL 37, l37
    nFadeL 38, l38
    nFadeLm 39, l39
    Flash 39, l39a
    nFadeLm 40, l40
    Flash 40, l40a
    nFadeL 41, l41
    nFadeL 42, l42
    nFadeL 43, l43
    nFadeL 44, l44
    nFadeL 45, l45
    nFadeL 46, l46
    nFadeL 47, l47
    nFadeL 48, l48
    nFadeL 49, l49
    nFadeL 50, l50
    nFadeL 51, l51
    nFadeL 52, l52
'Tracks
    Flash 53, T_YouShookMe				'YouShookMeAllNightLong
    Flash 54, T_HighwaytoHell			'HighwayToHell
    Flash 55, T_RockNRollTrain			'RockNRollTrain
    Flash 56, T_WholeLottaRosie			'WholeLottaRosie
    Flash 65, T_HellsBells				'HellsBells
    Flash 66, T_ThunderStruck			'ThunderStruck
    Flash 67, T_BackInBlack				'BackInBlack
    Flash 68, T_WarMachine				'WarMachine
    Flash 69, T_ForThoseAbout			'ForThoseAboutToRock
    Flash 70, T_TNT						'TNT
    Flash 71, T_HellAintBad				'HellAintBadPlaceToBe
    Flash 72, T_LetThereBeRock			'LetThereBeRock
'hornet left
	NFadeObjm 57, l57p, "Horns_on", "Horns_off"
	Flashm 57, l57
	Flash 57, l57a
'hornet right
	NFadeObjm 58, l58p, "Horns_on", "Horns_off"
	Flashm 58, l58		
	Flash 58, l58a
'Bumpers
    nFadeLm 60, l60
    nFadeLm 60, l60a
    nFadeL 60, l60b
    nFadeLm 61, l61
    nFadeLm 61, l61a
    nFadeL 61, l61b
    nFadeLm 62, l62
    nFadeLm 62, l62a
    nFadeL 62, l62b
'FIRE button
    nFadeLm 63, l63
    nFadeLm 63, l63a
    nFadeLm 63, l63b
    nFadeL 63, l63c
    nFadeL 64, l64

'Flashers
'VPM returns an 0-255 range value
	LampMod 177, f17		'Train
	LampMod 177, f17a		'Train
	LampMod 177, f17b		'Train
	LampMod 177, f17c		'Train
	LampMod 177, f17r		'Train
	LampMod 180, f20		'Left Ramp
	LampMod 181, f21		'Yellow Dome
	LampMod 181, f21a		'Yellow Dome
	LampMod 181, f21c		'Yellow Dome
	LampMod 181, f21b		'Yellow Dome
	LampMod 181, f21bDT		'Yellow Dome
	LampMod 181, f21r		'Yellow Dome
	LampMod 182, f22 		'Backbox
	LampMod 182, f22b 		'Backbox
	LampMod 182, f22c 		'Backbox
	LampMod 182, f22r 		'Backbox
	LampMod 183, f23 		'Top Eject
	LampMod 183, f23a 		'Top Eject
	LampMod 185, f25a 		'Bumper Flash
	LampMod 185, f25 		'Bumper Flash
	LampMod 186, f26 		'Bell Arrow
	LampMod 187, f27 		'Thunder Left
	LampMod 187, f27a		'Thunder Left
	LampMod 187, f27b		'Thunder Left
	LampMod 187, f27c		'Thunder Left
	LampMod 188, f28 		'Thunder Center
	LampMod 188, f28a		'Thunder Center
	LampMod 188, f28b		'Thunder Center
	LampMod 188, f28c		'Thunder Center
	LampMod 189, f29 		'Thunder Right
	LampMod 189, f29a		'Thunder Right
	LampMod 189, f29b		'Thunder Right
	LampMod 189, f29c		'Thunder Right
	LampMod 190, f30		'Right Ramp
	LampMod 191, f31 		'Red Dome
	LampMod 191, f31a		'Red Dome
	LampMod 191, f31b		'Red Dome
	LampMod 191, f31c		'Red Dome
	LampMod 191, f31bDT		'Red Dome
	LampMod 191, f31bDT1	'Red Dome
	LampMod 191, f31d		'Red Dome
	LampMod 191, f31r		'Red Dome
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1            'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0
        Case 5:object.image = a:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

' Flasher objects

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
End Sub

' RGB Leds

Sub RGBLED (object,red,green,blue)
	If TypeName(object) = "Light" Then
		object.color = RGB(0,0,0)
		object.colorfull = RGB(2.5*red,2.5*green,2.5*blue)
		object.state=1
	ElseIf TypeName(object) = "Flasher" Then
		object.color = RGB(2.5*red,2.5*green,2.5*blue)
		object.IntensityScale = 1
	End If
End Sub

' Modulated Flasher and Lights objects

Sub SetLampMod(nr, value)
    If value > 0 Then
		LampState(nr) = 1
	Else
		LampState(nr) = 0
	End If
	ModulationLevel(nr) = value
End Sub

Sub LampMod(nr, object)
	Object.IntensityScale = ModulationLevel(nr)/128
	If TypeName(object) = "Light" Then
		Object.State = LampState(nr)
	End If
	If TypeName(object) = "Flasher" Then
		Object.visible = LampState(nr)
	End If
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table
    Dim tmp
    tmp = ball.x * 2 / ACDC.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

function AudioFade(ball)
    Dim tmp
    tmp = ball.y * 2 / ACDC.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function RndNum(min,max)
	RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min AND max
End Function

' *********************************************************************
' 					Ball Drop & Ramp Sounds
' *********************************************************************

Sub ShooterEnd_Hit:If ActiveBall.Z > 30  Then Me.TimerInterval=100:Me.TimerEnabled=1:End If:End Sub						'ball is flying
Sub ShooterEnd_Timer(): Me.TimerEnabled=0 : PlaySound "fx_balldrop",0,1,.2,0,0,0,0,-.8 : End Sub

Sub LREnter_Hit():If ActiveBall.VelY < 0 Then PlaySoundAtBallVol "fx_ramp_enter1",.2:End If:End Sub			'ball is going up
Sub LREnter_UnHit():If ActiveBall.VelY > 0 Then StopSound "fx_ramp_enter1":End If:End Sub		'ball is going down

Sub RREnter_Hit():If ActiveBall.VelY < 0 Then PlaySoundAtBallVol "fx_ramp_enter1",.2:End If:End Sub			'ball is going up
Sub RREnter_UnHit():If ActiveBall.VelY > 0 Then StopSound "fx_ramp_enter1":End If:End Sub		'ball is going down

Sub LREnter1_Hit():PlaySoundAtBallVol "fx_ramp_enter2",.2:End Sub
Sub RREnter1_Hit():PlaySoundAtBallVol "fx_ramp_enter2",.2:End Sub
Sub LREnter2_Hit():PlaySoundAtBallVol "fx_ramp_enter3",.2:End Sub
Sub RREnter2_Hit():PlaySoundAtBallVol "fx_ramp_enter3",.2:End Sub

Sub LRExit_Hit():ActiveBall.VelY=1:StopSound "fx_ramp_enter3":Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub LRExit_timer():Me.TimerEnabled=0:PlaySound "fx_BallDrop2",0,3,-.4,0,0,0,0,.7:End Sub

Sub RRExit_Hit():StopSound "fx_ramp_enter3":Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub RRExit_timer():Me.TimerEnabled=0:PlaySound "fx_BallDrop2",0,3,.4,0,0,0,0,.7:End Sub

Sub Div1_Hit() : StopSound "fx_ramp_enter3" : If Div1Hit=0 Then PlaySoundAtBall "fx_ramp_turn": End If: Div1Hit=1: Me.TimerInterval=3000: Me.TimerEnabled=1: End Sub
Sub Div1_Timer(): Me.TimerEnabled=0 : Div1Hit = 0 : End Sub

Sub Helper_hit:ActiveBall.VelX=0.9*ActiveBall.VelX:ActiveBall.VelY=0.9*ActiveBall.VelY:End Sub

' *********************************************************************
' 						Other Sound FX
' *********************************************************************

Sub LeftFlipper_Collide(parm)
    PlaySoundAtBallVol "fx_flip_hit_" & Int(Rnd*3)+1, 3
End Sub

Sub RightFlipper_Collide(parm)
    PlaySoundAtBallVol "fx_flip_hit_" & Int(Rnd*3)+1, 3
End Sub

Sub LeftFlipperMini_Collide(parm)
    PlaySoundAtBallVol "fx_flip_hit_" & Int(Rnd*3)+1, 3
End Sub

Sub RightFlipperMini_Collide(parm)
    PlaySoundAtBallVol "fx_flip_hit_" & Int(Rnd*3)+1, 2
End Sub

Sub RandomSoundMetal()
    PlaySoundAtBallVol "fx_metal_hit_" & Int(Rnd*3)+1, 1
End Sub

Sub cRubbers_Hit(idx)
    PlaySoundAtBallVol "fx_rubber_hit_" & Int(Rnd*3)+1, 3
End Sub

Sub aGates_Hit (idx)
	PlaySoundAt "fx_gate4",ActiveBall
End Sub

Dim NextOrbitHit:NextOrbitHit = 0 

Sub PlasticRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, Pitch(ActiveBall)
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .1 + (Rnd * .2)
	end if 
End Sub

Sub MetalWallBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, 20000 'Increased pitch to simulate metal wall
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub

' Requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)+voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' Stop Bump Sounds
Sub BumpSTOPplastic1_Hit ()
dim i:for i=1 to 7:StopSound "rampbump" & i:next
NextOrbitHit = Timer + 1
End Sub

Sub BumpSTOPplastic2_Hit ()
dim i:for i=1 to 7:StopSound "rampbump" & i:next
NextOrbitHit = Timer + 1
End Sub

Sub BumpSTOPplastic3_Hit ()
dim i:for i=1 to 7:StopSound "rampbump" & i:next
NextOrbitHit = Timer + 1
End Sub


' *********************************************************************
' 						RealTime Updates
' *********************************************************************
Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
	RollingSoundUpdate
	BallShadowUpdate
	GateR2.RotX = - Spinner1.currentangle
	GateR.RotX = - Spinner2.currentangle
	GateL.RotX = - Spinner3.currentangle
	GateL2.RotX = - Spinner4.currentangle
	SpinnerT1.RotX = - Sw33.currentangle
	GateR1.RotX = - Spinner6.currentangle
	GateL1.RotX = - Spinner7.currentangle
	RightGate.RotY = Gate.currentangle
	LeftGate.RotX = -Gate2.currentangle
End Sub

'*********** ROLLING SOUND *********************************
Const tnob = 4						' total number of balls : 4 (trough)
Const fakeballs = 0					' number of balls created on table start (rolling sound will be skipped)
ReDim rolling(tnob)
InitRolling

Sub InitRolling:Dim i:For i=0 to (tnob-1):rolling(i) = False:Next:End Sub

Sub RollingSoundUpdate()
    Dim BOT, b
    BOT = GetBalls
	' stop the sound of deleted balls
	If UBound(BOT)<(tnob - 1) Then
		For b = (UBound(BOT) + 1) to (tnob-1)
			rolling(b) = False
			StopSound("fx_ballrolling" & b)
		Next
	End If
	' exit the Sub if no balls on the table
    If UBound(BOT) = fakeballs-1 Then Exit Sub
	
'**********************************************************
' Raised Ramp RollingBall Sounds by RustyCardores & DJRobX 
'**********************************************************

       ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 Then
			rolling(b) = True
			if BOT(b).z < 30 Then ' Ball on playfield
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.8, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else ' Ball on raised ramp
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.4, Pan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
				End If
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
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 200, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'**************************************************************************
'                 Positional Sound Playback Functions by DJRobX
'**************************************************************************

'Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(sound, tableobj)
		PlaySound sound, 1, 1, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'Set all as per ball position & speed.

Sub PlaySoundAtBall(sound)
		PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
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


'*********** BALL SHADOW *********************************
Dim BallShadow:BallShadow = Array (BallShadow1, BallShadow2, BallShadow3,Ballshadow4,Ballshadow5)

Sub BallShadowUpdate()
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
		If BOT(b).X < ACDC.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (ACDC.Width/2))/7)) + 10
		Else
			BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (ACDC.Width/2))/7)) - 10
		End If
	    ballShadow(b).Y = BOT(b).Y + 20
		If BOT(b).Z > 20 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub

Sub InitOptions
	Dim ii
	Armour.visible = DesktopMode
	l63b.visible = DesktopMode:l63c.visible = DesktopMode
	EMReel1.visible = DesktopMode:	EMReel2.visible = DesktopMode
	f21b.visible = NOT DesktopMode : f21bDT.visible = DesktopMode
	f31b.visible = NOT DesktopMode : f31bDT.visible = DesktopMode : f31bDT1.visible = DesktopMode
	If FlippersDecals=0 Then
		LeftFlipper.image = "LeftFlipper" : RightFlipper.image = "RightFlipper"
	Else
		LeftFlipper.image = "LeftFlipper_c" : RightFlipper.image = "RightFlipper_c"
	End If
	If SideCabDecals=0 Then
		LeftCab.image = "sidecabL":RightCab.image = "sidecabR"
	Else
		LeftCab.image = "sidecabL_c":RightCab.image = "sidecabR_c"
	End If
	If HelenMod=1 Then
		HelenDecal.image = "ACDC-Helen-PfDecal"
		ApronStickers.image= "ApronSticker-helen"
		Cards.image= "ACDC-Helen-Card (PRO)"
		SpinnerT1.image= "spinner-helen"
	End If
	LeftRampDecal.visible = RampsDecals
	RightRampDecal.visible = RampsDecals
	l63a.visible = FIREButtonLight
End Sub
