'Family Guy / IPD No. 5219 / 2006 / 4 Players


Option Explicit
Randomize

'******************************************************
'* TABLE OPTIONS **************************************
'******************************************************
' GI color
Const GIcolor = 0    '0= original, '1= white, 2=fantasy

'******************************************************
'* VPM INIT *******************************************
'******************************************************

Const BallSize = 50
Const BallMass = 1.2

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD:UseVPMDMD = DesktopMode
If Table1.showdt = false then ramp12.visible = 0:ramp13.visible = 0:ramp14.visible = 0

LoadVPM "01560000","sam.vbs",3.43

'********************
'Standard definitions
'********************

Const UseSolenoids = 1
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0
Const SSolenoidOff = ""
Const SCoin = ""

'******************************************************
'* ROM VERSION ****************************************
'******************************************************

Const cGameName = "fg_1200af"

'******************************************************
'* VARIABLES ******************************************
'******************************************************

Dim bsTrough, PlungerIM, BIP, TVScoop, DrunkenClam, FARTBank, DeathTarget, CPUPos, CPDPos, MiniPF, MiniRight, MiniLeft
Dim  CastleGatePos, CastleGatePos2, CGUp, StewiePos, StewiePos2, StewiePos3, StewieDir, StewiePinball, MegPos

'******************************************************
'* KEYS ***********************************************
'******************************************************

Sub Table1_KeyDown(ByVal keycode)
If keycode = PlungerKey Then Plunger.PullBack

If Keycode = RightFlipperKey then
Controller.Switch(90)=1
Controller.Switch(82)=1
If MiniPF.Balls=0 Then
PlaySound SoundFX("Stern_MiniFlipperUp2",DOFContactors)
MiniPF_RightFlipper.RotateToEnd
DOF 101, 1
MiniRight=1
End If
Exit Sub
End If

If Keycode = LeftFlipperKey then
Controller.Switch(84)=1
If MiniPF.Balls=0 Then
PlaySound SoundFX("Stern_MiniFlipperUp1",DOFContactors)
MiniPF_LeftFlipper.RotateToEnd
DOF 102, 1
MiniLeft=1
End If
Exit Sub
End If

If keycode = LeftTiltKey Then Nudge 90, 3:End If
If keycode = RightTiltKey Then Nudge 270, 3:End If   
If keycode = CenterTiltKey Then Nudge 0, 3:End If
If vpmKeyDown(keycode) Then Exit Sub
If keycode = PlungerKey Then Plunger.PullBack
End Sub

Sub Table1_KeyUp(ByVal keycode)

If keycode = PlungerKey Then Plunger.Fire:If BIP=1 then Playsound "Stern_Plunge" else If BIP=0 then Playsound "Stern_Hit7":End If

If Keycode = RightFlipperKey then
Controller.Switch(90)=0
Controller.Switch(82)=0 
If MiniRight=1 Then 
PlaySound SoundFX("Stern_MiniFlipperDown2",DOFContactors)
MiniPF_RightFlipper.RotateToStart
DOF 101, 0
MiniRight=0
End If
Exit Sub
End If

If Keycode = LeftFlipperKey then
Controller.Switch(84)=0 
If MiniLeft=1 Then 
PlaySound SoundFX("Stern_MiniFlipperDown1",DOFContactors)
MiniPF_LeftFlipper.RotateToStart
DOF 102, 0
MiniLeft=0
End If
Exit Sub
End If

If vpmKeyUp(keycode) Then Exit Sub
If keycode = PlungerKey Then Plunger.Fire
End Sub

'******************************************************
'******************************************************
'******************************************************
'* TABLE INIT *****************************************
'******************************************************
'******************************************************
'******************************************************

Sub Table1_Init

'* ROM AND DMD ****************************************

With Controller
.GameName = cGameName
.SplashInfoLine = "Family Guy - STERN 2007"
.HandleMechanics = 0
.HandleKeyboard = 0
.ShowDMDOnly = 1
.ShowFrame = 0
.ShowTitle = 0
.Hidden = 0
On Error Resume Next
.Run
If Err Then MsgBox Err.Description
On Error Goto 0
End With

'* PINMAME TIMER **************************************

PinMAMETimer.Interval = PinMAMEInterval
PinMAMETimer.Enabled = 1

'* NUGDE **********************************************

vpmNudge.TiltSwitch=-7
vpmNudge.Sensitivity=3
vpmNudge.TiltObj=Array(bumper1,bumper2,bumper3,LeftSlingshot,RightSlingshot)

'* TROUGH *********************************************

Set bsTrough = New cvpmTrough
With bsTrough
.Size=4
.InitSwitches Array(21, 20, 19, 18)
.InitExit BallRelease, 90, 8
.InitEntrySounds "Stern_Drain", "", ""
.InitExitSounds "",SoundFX("Stern_Release",DOFContactors)
.Balls = 4
.CreateEvents "bsTrough", Drain
End With

'* MINI TROUGH ****************************************

Set MiniPF = New cvpmBallStack
With MiniPF
.InitSaucer sw55,55, 90, 35
.InitExitSnd SoundFX("Popper",DOFContactors), ""
.InitAddSnd "fx_kin" 
.CreateEvents "MiniPF", sw55
End With

'* TV SCOOP *******************************************

Set TVScoop = New cvpmTrough
With TVScoop
.Size=3
.InitSwitches Array(13, 0, 0)
.InitExit sw13, 184, 15
.InitEntrySounds "Stern_Scoopenter", "", ""
.InitExitSounds "",SoundFX("Stern_Scoopexit",DOFContactors)
.Balls = 0
End With

'* DRUNKEN CLAM ***************************************

Set DrunkenClam = New cvpmSaucer
With DrunkenClam
.InitKicker sw64,64, 183, 4, 0
.InitSounds "Stern_Scoopenter", "", SoundFX("Stern_Scoopexit",DOFContactors)
.CreateEvents "DrunkenClam", sw64
End With

'* FART TARGETS 4-BANK DROPTARGETS ********************

Set FARTBank = New cvpmDropTarget
With FARTBank
.InitDrop Array(sw47,sw44,sw46,sw45), Array(44,47,45,46)
.Initsnd SoundFX("Stern_Droptargethit",DOFContactors), SoundFX("Stern_Bankraise",DOFContactors)
End With

'* DEATH DROPTARGET *******************************

Set DeathTarget = New cvpmDropTarget
With DeathTarget
.InitDrop Array(Array(sw9, sw9w)), Array(9)
.Initsnd SoundFX("Stern_Droptargethit",DOFContactors), SoundFX("Stern_DropTargetraise",DOFContactors)
End With

'* IMPULSE PLUNGER ************************************

Const IMPowerSetting = 33
Const IMTime = 0.33
Set PlungerIM = New cvpmImpulseP
With plungerIM
.InitImpulseP BIPREG, IMPowerSetting, IMTime
.InitExitSnd "Stern_Autoplung", ""
.CreateEvents "PlungerIM"
End With

'* STARTUP CALLS **************************************
CPC.isdropped=1
kicker1.CreateSizedBallWithMass BallSize/2,Ballmass
kicker1.kick 180, 1
kicker1.enabled = 0
kicker2.CreateSizedBallWithMass BallSize/2,Ballmass
kicker2.kick 180, 1
kicker2.enabled = 0
kicker3.CreateSizedBallWithMass BallSize/2,Ballmass
kicker3.kick 180, 1
kicker3.enabled = 0
kicker4.CreateSizedBallWithMass BallSize/2,Ballmass
kicker4.kick 180, 1
kicker4.enabled = 0
sw55.createsizedball(14.6875)			'minipinball 5/8" 
MiniPF.AddBall 0
InitGIColor
'******************************************************
'******************************************************
'******************************************************
'* TABLE INIT END *************************************
'******************************************************
'******************************************************
'******************************************************

End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit():Controller.Stop:End sub

'******************************************************
'* SOLENOID CALLS *************************************
'******************************************************

SolCallback(1)  = "solTrough"				 'BallExit
SolCallback(2)  = "AutoLaunch"				 'AutoLaunch
SolCallback(3)  = "BankReset"				 '4-Bank Drop Tragets reset
SolCallback(4)  = "CPDSol"                   'Ball Saver Down
SolCallback(5)  = "DrunkenClam.SolOut"		 'DrunkenClam Exit 
SolCallback(6)  = "DeathReset"				 'Single Drop Targte reset
'SolCallback(7)  = "LeftSlingHit"            'Left Slingshot
'SolCallback(8)  = "RightSlingHit"           'Right Slingshot
'SolCallback(9)  = "Bump1Sol"                'Bottom Bumper
'SolCallback(10) = "Bump2Sol"                'Right Bumper
'SolCallback(11) = "Bump3Sol"                'Top Bumper
SolCallback(12) = "CPUSol"                   'Ball Saver Up
SolCallback(13) = "TVScoop.SolOut"                'Swamp Eject Scoop

SolCallback(15) = "LFlipper"
SolCallback(16) = "RFlipper"
SolCallback(17) = "Miniflipper_Left"
SolCallback(18) = "Miniflipper_Right"
SolCallback(19) = "CastleGuardSol"
SolCallBack(20) = "StewieMove"				'Stewie Motor Drive
SolCallBack(21) = "MiniPFSol"
SolCallBack(22) = "MegMove"					'Meg Move Solenoid
SolCallback(23) = "setlamp 193,"			'Flash Lower Left
SolCallBack(24)= "vpmSolSound SoundFX(""fx_knocker"",DOFKnocker),"					'Knocker
SolCallback(25) = "setlamp 195,"			'Flash Backpanel Left
SolCallback(26) = "setlamp 196,"			'Flash Backpanel Center
SolCallback(27) = "setlamp 197,"			'Flash Backpanel Right
SolCallback(28) = "setlamp 198,"			'Flash BeerCan
SolCallback(29) = "setlamp 199,"			'Flash Fiona
SolCallback(30) = "setlamp 190,"			'Flash RIght Orbit (Spinner)
SolCallback(31) = "setlamp 191,"			'Flash Pop Bumpers
SolCallback(32) = "setlamp 192,"			'Flash Lower Right

'******************************************************
'* CASTLE GUARD GATE **********************************
'******************************************************

Sub CastleGuardsw_Hit()
Controller.Switch(35)=0
CastleGuardUp.enabled=1
CGUp=1
CastleGuardsw.isdropped=1
End Sub

Sub CastleGuardSol (enabled)
If enabled then
Controller.Switch(35)=1
End if
End Sub

Sub CastleGuardUp_Timer()	
Select Case CastleGatePos
Case 0: CastleGuardP.RotX=0:CastleGatePos=1
Case 1: CastleGuardP.RotX=0:
Case 2: CastleGuardP.RotX=-15
Case 3: CastleGuardP.RotX=-30
Case 4: CastleGuardP.RotX=-45
Case 5: CastleGuardP.RotX=-60
Case 6: CastleGuardP.RotX=-75
Case 7: CastleGuardP.RotX=-90:Me.Enabled=0:CastleGatePos=0
End Select
If CastleGatePos>0 then CastleGatePos=CastleGatePos+1
End Sub

Sub CastleGuardDown_Timer()	
Select Case CastleGatePos2
Case 0: CastleGuardP.RotX=-90:CastleGatePos2=1
Case 1: CastleGuardP.RotX=-90:
Case 2: CastleGuardP.RotX=-75
Case 3: CastleGuardP.RotX=-60
Case 4: CastleGuardP.RotX=-45
Case 5: CastleGuardP.RotX=-30
Case 6: CastleGuardP.RotX=-15
Case 7: CastleGuardP.RotX=0:CastleGuardsw.isdropped=0:CGUp=0
Case 8: CastleGuardP.RotX=5
Case 9: CastleGuardP.RotX=10
Case 10: CastleGuardP.RotX=15
Case 11: CastleGuardP.RotX=20
Case 12: CastleGuardP.RotX=15
Case 13: CastleGuardP.RotX=10
Case 14: CastleGuardP.RotX=5
Case 15: CastleGuardP.RotX=0
Case 16: CastleGuardP.RotX=-5
Case 17: CastleGuardP.RotX=-10
Case 18: CastleGuardP.RotX=-15
Case 19: CastleGuardP.RotX=-10
Case 20: CastleGuardP.RotX=-5
Case 21: CastleGuardP.RotX=0
Case 22: CastleGuardP.RotX=5
Case 23: CastleGuardP.RotX=10
Case 24: CastleGuardP.RotX=15
Case 25: CastleGuardP.RotX=10
Case 26: CastleGuardP.RotX=5
Case 27: CastleGuardP.RotX=0
Case 28: CastleGuardP.RotX=-5
Case 29: CastleGuardP.RotX=-10
Case 30: CastleGuardP.RotX=-15
Case 31: CastleGuardP.RotX=-10
Case 32: CastleGuardP.RotX=-5
Case 33: CastleGuardP.RotX=0
Case 34: CastleGuardP.RotX=5
Case 35: CastleGuardP.RotX=10
Case 36: CastleGuardP.RotX=10
Case 37: CastleGuardP.RotX=5
Case 38: CastleGuardP.RotX=0
Case 39: CastleGuardP.RotX=-5
Case 40: CastleGuardP.RotX=-10
Case 41: CastleGuardP.RotX=-10
Case 42: CastleGuardP.RotX=-5
Case 43: CastleGuardP.RotX=0
Case 44: CastleGuardP.RotX=5
Case 45: CastleGuardP.RotX=10
Case 46: CastleGuardP.RotX=5
Case 47: CastleGuardP.RotX=0
Case 48: CastleGuardP.RotX=-5
Case 49: CastleGuardP.RotX=-10
Case 50: CastleGuardP.RotX=-5
Case 51: CastleGuardP.RotX=0
Case 52: CastleGuardP.RotX=5
Case 53: CastleGuardP.RotX=0
Case 54: CastleGuardP.RotX=-5
Case 55: CastleGuardP.RotX=0
Case 56: CastleGuardP.RotX=5
Case 57: CastleGuardP.RotX=0:Me.Enabled=0:CastleGatePos2=0
End Select
If CastleGatePos2>0 then CastleGatePos2=CastleGatePos2+1
End Sub

'******************************************************
'* MINI PLAYFIELD *************************************
'******************************************************

Sub Miniflipper_Right(Enabled)
If Enabled Then
PlaySound SoundFX("Stern_MiniFlipperUp1",DOFContactors):MiniPF_RightFlipper.RotateToEnd
Else
PlaySound SoundFX("Stern_MiniFlipperDown1",DOFContactors):MiniPF_RightFlipper.RotateToStart
End If
End Sub

Sub Miniflipper_Left(Enabled)
If Enabled Then
StewieSWTimer.Enabled=1
PlaySound SoundFX("Stern_MiniFlipperUp2",DOFContactors):MiniPF_LeftFlipper.RotateToEnd
Else
PlaySound SoundFX("Stern_MiniFlipperDown2",DOFContactors):MiniPF_LeftFlipper.RotateToStart
End If
End Sub

Sub MiniPFSol(enabled)
If enabled then
MiniPF.ExitSol_On
end if
End Sub

Sub StewieSWTimer_Timer()
StewieSW.enabled=1
End Sub

Sub StewieSW_hit()
StewiePinball=2
End Sub

Sub StewieTurn_Hit()
If StewiePinball=0 then
StewieTimer2.enabled=1
StewiePinball=3
StewieSWTimer.enabled=1
End If
End Sub

Sub StewieReset_Hit
If StewiePinball >0 then
StewiePinball = 0
End If
End Sub

Sub StewieDelay_Timer()
StewieTimer.Enabled = 1
StewieDelay.Enabled = 0
End Sub

'******************************************************
'* STEWIE MOVEMENT ************************************
'******************************************************

Sub StewieMove (enabled)
If enabled AND StewiePinball=0 then
StewieDelay.Enabled = 1
Else
If enabled AND StewiePinball=2 then
StewieTimer3.Enabled = 1
End If
End If
End Sub

Sub StewieTimer_Timer()	
Select Case StewiePos
Case 0: StewieP.RotZ=0:StewiePos3=0:StewiePos=1
Case 1: StewieP.RotZ=+2
Case 2: StewieP.RotZ=+4
Case 3: StewieP.RotZ=+6
Case 4: StewieP.RotZ=+8
Case 5: StewieP.RotZ=+10
Case 6: StewieP.RotZ=+12
Case 7: StewieP.RotZ=+10
Case 8: StewieP.RotZ=+8
Case 9: StewieP.RotZ=+6
Case 10: StewieP.RotZ=+4
Case 11: StewieP.RotZ=+2
Case 12: StewieP.RotZ=0:StewiePos=1:StewieTimer.Enabled=0:StewiePinball=0
End Select
If StewiePos>0 then StewiePos=StewiePos+1
Sh5.RotZ = - StewieP.RotZ
End Sub

Sub StewieTimer2_Timer()	
Select Case StewiePos2
Case 0: StewieP.RotZ=0:StewiePos2=1
Case 1: StewieP.RotZ=0
Case 2: StewieP.RotZ=-15
Case 3: StewieP.RotZ=-30
Case 4: StewieP.RotZ=-45
Case 5: StewieP.RotZ=-60
Case 6: StewieP.RotZ=-75
Case 7: StewieP.RotZ=-90
Case 8: StewieP.RotZ=-105
Case 9: StewieP.RotZ=-120
Case 10: StewieP.RotZ=-135
Case 11: StewieP.RotZ=-150
Case 12: StewieP.RotZ=-150:StewieTimer2.Enabled=0:StewiePos2=0:StewiePinball=3
End Select
If StewiePos2>0 then StewiePos2=StewiePos2+1
Sh5.RotZ = -StewieP.RotZ
End Sub

Sub StewieTimer3_Timer()	
Select Case StewiePos3
Case 0: StewieP.RotZ=-150:StewiePos3=1
Case 1: StewieP.RotZ=-150
Case 2: StewieP.RotZ=-135
Case 3: StewieP.RotZ=-120
Case 4: StewieP.RotZ=-105
Case 5: StewieP.RotZ=-90
Case 6: StewieP.RotZ=-75
Case 7: StewieP.RotZ=-60
Case 8: StewieP.RotZ=-45
Case 9: StewieP.RotZ=-30
Case 10: StewieP.RotZ=-15
Case 11: StewieP.RotZ=0
Case 12: StewieP.RotZ=0:StewieTimer3.Enabled=0:StewiePos3=0:StewieSWTimer.enabled=0:StewiePinball=0:StewieSW.enabled=0
End Select
If StewiePos3>0 then StewiePos3=StewiePos3+1
Sh5.RotZ = -StewieP.RotZ
End Sub

'******************************************************
'* TROUGH *********************************************
'******************************************************

Sub solTrough(enabled)
If enabled then
bsTrough.ExitSol_On
vpmTimer.PulseSw 22
end if
End Sub

'******************************************************
'* AUTOLAUNCH *****************************************
'******************************************************

Sub AutoLaunch(Enabled)
If Enabled Then
PlungerIM.AutoFire
End If
End Sub

'******************************************************
'* TV SCOOP ****************************************
'******************************************************
Dim bBall, bZpos

Sub sw13_Hit
Set bBall = ActiveBall
bZpos = 50
Me.TimerInterval = 2
Me.TimerEnabled = 1
End Sub

Sub sw13_Timer
bBall.Z = bZpos
bZpos = bZpos-2
If bZpos <40 Then
Me.TimerEnabled = 0
Me.DestroyBall
TVScoop.AddBall Me
End If
End Sub

'******************************************************
'* LOIS SPINNER ***************************************
'******************************************************
Sub sw39_Spin:vpmTimer.PulseSw 39:End Sub

'******************************************************
'* POP BUMPERS ****************************************
'******************************************************

Dim dirRing1 : dirRing1 = -1
Dim dirRing2 : dirRing2 = -1
Dim dirRing3 : dirRing3 = -1

Sub Bumper1_Hit : vpmTimer.PulseSw 32 : PlaySound SoundFX("Stern_Bump1",DOFContactors) : Me.TimerEnabled = 1 : End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw 31 : PlaySound SoundFX("Stern_Bump2",DOFContactors) : Me.TimerEnabled = 1 : End Sub
Sub Bumper3_Hit : vpmTimer.PulseSw 30 : PlaySound SoundFX("Stern_Bump3",DOFContactors) : Me.TimerEnabled = 1 : End Sub

Sub Bumper1_timer()
	BR1.Z = BR1.Z + (5 * dirRing1)
	If BR1.Z <= -35 Then dirRing1 = 1
	If BR1.Z >= 0 Then
		dirRing1 = -1
		BR1.Z = 0
		Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper2_timer()
	BR2.Z = BR2.Z + (5 * dirRing2)
	If BR2.Z <= -35 Then dirRing2 = 1
	If BR2.Z >= 0 Then
		dirRing2 = -1
		BR2.Z = 0
		Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper3_timer()
	BR3.Z = BR3.Z + (5 * dirRing3)
	If BR3.Z <= -35 Then dirRing3 = 1
	If BR3.Z >= 0 Then
		dirRing3 = -1
		BR3.Z = 0
		Me.TimerEnabled = 0
	End If
End Sub

'******************************************************
'* TARGETS ********************************************
'******************************************************

Sub sw3_Hit:vpmTimer.PulseSw 3:PlaySound SoundFX("Stern_Hit1",DOFContactors):If CGUp= 1 then CastleGuardDown.enabled=1:End If:End Sub

Sub sw4_Hit:vpmTimer.PulseSw 4:PlaySound SoundFX("Stern_Hit1",DOFContactors):End Sub

Sub sw5_Hit:vpmTimer.PulseSw 5:PlaySound SoundFX("Stern_Hit1",DOFContactors):End Sub

Sub sw8_Hit:vpmTimer.PulseSw 8:PlaySound SoundFX("Stern_Hit1",DOFContactors):End Sub

Sub sw10_Hit:vpmTimer.PulseSw 10:PlaySound SoundFX("Stern_Hit1",DOFContactors):End Sub

Sub sw41_Hit:vpmTimer.PulseSw 41:PlaySound SoundFX("Stern_Hit1",DOFContactors):End Sub

Sub sw42_Hit:vpmTimer.PulseSw 42:PlaySound SoundFX("Stern_Hit1",DOFContactors):End Sub

Sub sw43_Hit:vpmTimer.PulseSw 43:PlaySound SoundFX("Stern_Hit1",DOFContactors):End Sub


'******************************************************
'* MINIPF TARGETS **************************************
'******************************************************

Sub sw50_Hit:vpmTimer.PulseSw 50:PlaySound SoundFX("Stern_Hit1",DOFContactors):End Sub
Sub sw51_Hit:vpmTimer.PulseSw 51:PlaySound SoundFX("Stern_Hit1",DOFContactors):End Sub

'******************************************************
'* MINI PF SWITCHES ***********************************
'******************************************************

Sub sw52_Hit:Controller.Switch(52) = 1:End Sub
Sub sw52_Unhit:Controller.Switch(52) = 0:End Sub

Sub sw53_Hit:Controller.Switch(53) = 1:End Sub
Sub sw53_Unhit:Controller.Switch(53) = 0:End Sub

Sub sw54_Hit:Controller.Switch(54) = 1:End Sub
Sub sw54_Unhit:Controller.Switch(54) = 0:End Sub

'******************************************************
'* ROLLOVER SWITCHES **********************************
'******************************************************

Sub sw6_Hit:Controller.Switch(6) = 1:sw6p.Z=-2:End Sub
Sub sw6_Unhit:Controller.Switch(6) = 0:sw6p.Z=0:End Sub

Sub sw7_Hit:Controller.Switch(7) = 1:sw7p.Z=-2:End Sub
Sub sw7_Unhit:Controller.Switch(7) = 0:sw7p.Z=0:End Sub

Sub sw23_Hit:Controller.Switch(23) = 1:sw23p.Z=-2:BIP=1:End Sub
Sub sw23_Unhit:Controller.Switch(23) = 0:sw23p.Z=0:BIP=0:End Sub

Sub sw24_Hit:Controller.Switch(24) = 1:sw24p.Z=-2:End Sub
Sub sw24_Unhit:Controller.Switch(24) = 0:sw24p.Z=0:End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:sw25p.Z=-2:End Sub
Sub sw25_Unhit:Controller.Switch(25) = 0:sw25p.Z=0:End Sub

Sub sw28_Hit:Controller.Switch(28) = 1:sw28p.Z=-2:End Sub
Sub sw28_Unhit:Controller.Switch(28) = 0:sw28p.Z=0:End Sub

Sub sw29_Hit:Controller.Switch(29) = 1:sw29p.Z=-2:End Sub
Sub sw29_Unhit:Controller.Switch(29) = 0:sw29p.Z=0:End Sub

Sub sw40_Hit:Controller.Switch(40) = 1:sw40p.Z=-2:End Sub
Sub sw40_Unhit:Controller.Switch(40) = 0:sw40p.Z=0:End Sub

Sub sw48_Hit:Controller.Switch(48) = 1:End Sub
Sub sw48_Unhit:Controller.Switch(48) = 0:End Sub

Sub sw57_Hit:Controller.Switch(57) = 1:End Sub
Sub sw57_Unhit:Controller.Switch(57) = 0:End Sub

'******************************************************
'* SLINGSHOTS *****************************************
'******************************************************
Dim LStep, RStep
Sub LeftSlingShot_Slingshot
    Playsound SoundFX("Stern_LeftSlingshot",DOFContactors),0,1,-0.05,0.05
    vpmTimer.PulseSw 26
    LSling.Visible = 0
    LSling1.Visible = 1
    sling1.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling1.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


Sub RightSlingShot_Slingshot
    Playsound SoundFX("Stern_RightSlingshot",DOFContactors),0,1,0.05,0.05
    vpmTimer.PulseSw 27
    RSling.Visible = 0
    RSling1.Visible = 1
    sling2.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling2.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'******************************************************
'* CENTER POST ****************************************
'******************************************************
'* DJRobX fixed the ball saver emulation error. Game  *
'* seems to fire both up and down to put the saver    *
'* down. Create a "latch" to hold it down'            *
'******************************************************

Dim SaverPos, SaverTime    
SaverPos = 0
SaverTime = Now

Sub CPDSol (enabled)
If enabled then
SaverTime = DateAdd("s",3, Now)
CPUSol (true)
End If
End Sub

Sub CPUSol (enabled)
If enabled then
If SaverTime > Now Then
If SaverPos = 1 Then
' ** Move Saver Down **'
Controller.Switch(2) = 1
Controller.Switch(1) = 0
CenterpostDown.Enabled=1     
SaverPos=0
CPC.isdropped=1
End If
Else
If SaverPos = 0 Then
' ** Move Saver Up ** '
Controller.Switch(2) = 0
Controller.Switch(1) = 1
CenterpostUp.Enabled=1
SaverPos=1
CPC.isdropped=0
End If
End If
End If
End Sub

Sub CenterpostUp_Timer()	
Select Case CPUPos
Case 0: CenterPost.Z=-15
Case 1: CenterPost.Z=-05:Playsound SoundFX("Stern_Centerpost_Up",DOFContactors)
Case 2: CenterPost.Z=0
Case 3: CenterPost.Z=0:CPUPos=0:CenterpostUp.Enabled=0
End Select
If CPUPos=>0 then CPUPos=CPUPos+1
End Sub

Sub CenterpostDown_Timer()	
Select Case CPDPos
Case 0: CenterPost.Z=-05
Case 1: CenterPost.Z=-15:Playsound SoundFX("Stern_Centerpost_Down",DOFContactors)
Case 2: CenterPost.Z=-26
Case 3: CenterPost.Z=-26:CPDPos=0:CenterpostDown.Enabled=0
End Select
If CPDPos=>0 then CPDPos=CPDPos+1
End Sub

'******************************************************
'* MEG MOVEMENT ***************************************
'******************************************************
Sub MegMove (enabled)
if enabled then
MegTimer.Enabled=1
End If
End Sub

Sub MegTimer_Timer()	
Select Case MegPos
Case 0: MegP.Z=180
Case 1: MegP.Z=175:Playsound SoundFX("Stern_Megshake",DOFContactors)
Case 2: MegP.Z=170
Case 3: MegP.Z=165
Case 4: MegP.Z=160
Case 5: MegP.Z=170
Case 6: MegP.Z=180
Case 20: MegP.Z=180:MegPos=0:Me.Enabled=0
End Select
If MegPos=>0 then MegPos=MegPos+1
End Sub

'******************************************************
'* BRIAN BEERCAN TARGET *******************************
'******************************************************
Dim BeerCanPos:BeerCanPos=1
Dim BeerCanDir : BeerCanDir=0
Sub sw49_Hit:vpmTimer.PulseSw 49:Me.TimerEnabled = 1:PlaySound SoundFX("Stern_Beercanhit",DOFContactors):End Sub

Sub sw49_Timer()
sw49P.Rotx = sw49P.Rotx - 2*BeerCanPos
BrianP.RotX= sw49P.RotX
If sw49P.Rotx < - 10 AND BeerCanPos=1 Then BeerCanPos=-1
If sw49P.Rotx > 6 AND BeerCanPos=-1 Then BeerCanDir=1 : BeerCanPos=1
If sw49P.Rotx <0 AND BeerCanDir=1 Then sw49P.Rotx =0 : BeerCanDir=0 : Me.TimerEnabled = 0
End Sub

'******************************************************
'* DEATH TARGET ***************************************
'******************************************************
Dim sw9Dir

Sub Deathreset (enabled)
If enabled then
sw9Dir = 1
sw9.TimerEnabled = 1
sw9w.isdropped=0
sw9.isdropped=0
DeathTarget.DropSol_On
End If
End Sub

Sub sw9_Hit:DeathTarget.Hit 1:sw9Dir=-1:Me.TimerEnabled = 1:End Sub

Sub sw9_Timer()	
Select Case sw9Dir
Case -1
sw9P.z = sw9P.z -5
If sw9p.Z <  -65 Then sw9p.Z = -65 : Me.TimerEnabled = 0
Case 1
sw9P.z = sw9P.z + 5
If sw9p.Z >  0 Then sw9p.Z = 0 : Me.TimerEnabled = 0
End Select
End Sub

'******************************************************
'* FART TARGETBANK ************************************
'******************************************************
Dim sw44Dir, sw45Dir, sw46Dir, sw47Dir

Sub Bankreset (enabled)
If enabled then
sw44Dir = 1
sw44.TimerEnabled = 1
sw45Dir = 1
sw45.TimerEnabled = 1
sw46Dir = 1
sw46.TimerEnabled = 1
sw47Dir = 1
sw47.TimerEnabled = 1
FARTBank.DropSol_On
End If
End Sub

Sub sw44_Hit:FARTBank.Hit 2:sw44Dir = -1:Me.TimerEnabled = 1:End Sub
Sub sw45_Hit:FARTBank.Hit 4:sw45Dir = -1:Me.TimerEnabled = 1:End Sub
Sub sw46_Hit:FARTBank.Hit 3:sw46Dir = -1:Me.TimerEnabled = 1:End Sub
Sub sw47_Hit:FARTBank.Hit 1:sw47Dir = -1:Me.TimerEnabled = 1:End Sub

'* T TARGET *******************************************

Sub sw44_Timer()	
Select Case sw44Dir
Case -1
sw44P.z = sw44P.z -5
If sw44p.Z <  -60 Then sw44p.Z = -60 : Me.TimerEnabled = 0
Case 1
sw44P.z = sw44P.z + 5
If sw44p.Z >  0 Then sw44p.Z = 0 : Me.TimerEnabled = 0
End Select
End Sub

'* R TARGET *******************************************

Sub sw45_Timer()	
Select Case sw45Dir
Case -1
sw45P.z = sw45P.z -5
If sw45p.Z <  -60 Then sw45p.Z = -60 : Me.TimerEnabled = 0
Case 1
sw45P.z = sw45P.z + 5
If sw45p.Z >  0 Then sw45p.Z = 0 : Me.TimerEnabled = 0
End Select
End Sub

'* A TARGET *******************************************
Sub sw46_Timer()	
Select Case sw46Dir
Case -1
sw46P.z = sw46P.z -5
If sw46p.Z <  -60 Then sw46p.Z = -60 : Me.TimerEnabled = 0
Case 1
sw46P.z = sw46P.z + 5
If sw46p.Z >  0 Then sw46p.Z = 0 : Me.TimerEnabled = 0
End Select
End Sub

'* F TARGET *******************************************
Sub sw47_Timer()	
Select Case sw47Dir
Case -1
sw47P.z = sw47P.z -5
If sw47p.Z <  -60 Then sw47p.Z = -60 : Me.TimerEnabled = 0
Case 1
sw47P.z = sw47P.z + 5
If sw47p.Z >  0 Then sw47p.Z = 0 : Me.TimerEnabled = 0
End Select
End Sub

'******************************************************
'* FLIPPER CALLS **************************************
'******************************************************

SolCallback(sLRFlipper) = "RFlipper"
SolCallback(sLLFlipper) = "LFlipper"

Sub LFlipper(Enabled)
If Enabled Then
If LeftFlipper.CurrentAngle > 100 then PlaySound SoundFX("Stern_LeftFlipper_Up",DOFContactors)
If LeftFlipper.CurrentAngle < 100 then PlaySound SoundFX("Stern_LeftFlipper_Up2",DOFContactors)
LeftFlipper.RotateToEnd:LeftFlipperSmall.RotateToEnd
Else
PlaySound SoundFX("Stern_Flipper_Down",DOFContactors):LeftFlipper.RotateToStart:LeftFlipperSmall.RotateToStart
End If
End Sub

Sub RFlipper(Enabled)
If Enabled Then
If RightFlipper.CurrentAngle < -100 then PlaySound SoundFX("Stern_RightFlipper_Up",DOFContactors)
If RightFlipper.CurrentAngle > -100 then PlaySound SoundFX("Stern_RightFlipper_Up2",DOFContactors)
RightFlipper.RotateToEnd
Else
PlaySound SoundFX("Stern_Flipper_Down",DOFContactors):RightFlipper.RotateToStart
End If
End Sub

'******************************************************
'* LEFT GATE ******************************************
'******************************************************

Sub LeftGate_hit:vpmTimer.PulseSw 33:LeftGateFl.RotateToEnd:End Sub
Sub LeftGate_Unhit:LeftGateFl.RotateToStart:End Sub

'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

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
    Next
End Sub

Sub UpdateLamps
NFadeL 3, l3
NFadeL 4, l4
NFadeL 5, l5
NFadeL 6, l6
NFadeL 7, l7
NFadeL 8, l8
NFadeL 9, l9
NFadeL 10, l10
NFadeL 11, l11
NFadeL 12, l12
NFadeL 13, l13
NFadeL 14, l14
NFadeL 15, l15
NFadeL 16, l16
NFadeL 17, l17
NFadeL 18, l18
NFadeL 19, l19
NFadeL 20, l20
NFadeL 21, l21
NFadeL 22, l22
NFadeL 23, l23
NFadeL 24, l24
NFadeL 25, l25
NFadeL 26, l26
NFadeL 27, l27
NFadeL 28, l28
NFadeL 29, l29
NFadeL 30, l30
NFadeL 31, l31
NFadeL 32, l32
NFadeL 33, l33
NFadeL 34, l34
NFadeL 35, l35
NFadeL 36, l36
NFadeL 37, l37
NFadeL 38, l38
NFadeL 39, l39
NFadeL 40, l40
NFadeL 41, l41
NFadeL 42, l42
NFadeL 43, l43
NFadeL 44, l44
NFadeL 45, l45
NFadeL 46, l46
NFadeL 47, l47
NFadeL 48, l48
NFadeL 49, l49
NFadeL 50, l50
NFadeL 51, l51
NFadeL 52, l52
NFadeL 53, l53
NFadeL 54, l54
NFadeL 55, l55
NFadeL 56, l56
NFadeL 57, l57
NFadeL 58, l58

NFadeLm 61, l61
NFadeL 61, l61a
NFadeL 62, l62
NFadeL 63, l63
NFadeL 64, l64
NFadeL 65, l65
NFadeL 66, l66
NFadeL 67, l67
NFadeL 68, l68
NFadeL 69, l69      
NFadeLm 70, l70
Flashm 70, l70a
Flash 70, l70a1


'Mini Playfield LED Inserts
NFadeL 125, LED1       'B
NFadeL 124, LED2       'R
NFadeL 123, LED3       'I
NFadeL 122, LED4       'A
NFadeL 121, LED5       'N

NFadeL 98, LED6        'M
NFadeL 99, LED7        'E
NFadeL 100, LED8       'G

NFadeL 114, LED9       'P
NFadeL 89, LED10       'E
NFadeL 90, LED11       'T
NFadeL 91, LED12       'E
NFadeL 92, LED13       'R

NFadeL 108, LED14      'L
NFadeL 107, LED15      'O
NFadeL 106, LED16      'I
NFadeL 105, LED17      'S

NFadeL 84, LED18       'C
NFadeL 83, LED19       'H
NFadeL 82, LED20       'R
NFadeL 81, LED21       'I
NFadeL 97, LED22       'S

NFadeL 199, F29
NFadeL 190, F30
NFadeL 191, F31

NfadeLm 192, F32
NfadeLm 192, F32a
NfadeLm 192, F32b
Flashm 192, F32c
Flashm 192, F32a1
Flashm 192, F32a2
Flashm 192, F32a3
Flashm 192, F32a4
Flash 192, F32r

NfadeLm 193, F23
NfadeLm 193, F23a
NfadeLm 193, F23b
Flashm 193, F23c
Flashm 193, F23a1
Flashm 193, F23a2
Flashm 193, F23a3
Flash 193, F23r

NFadeLm 195, F25
NFadeL 195, F25a
NFadeLm 196, F26
NFadeL 196, F26a
NFadeLm 197, F27
NFadeL 197, F27a

Flashm 198, F28
Flash 198, F28a
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
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
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

'******************************************************
'* GENERAL ILLUMINATION *******************************
'******************************************************

Set GICallBack = GetRef("UpdateGI")

Sub UpdateGI(nr,enabled)
Dim x
If (Enabled) Then
For each x in GI:x.state=1:Next
For each x in BWGI:x.state=2:Next
For each x in BWGI2:x.state=2:Next
Else
For each x in GI:x.state=0:Next
For each x in BWGI:x.state=0:Next
For each x in BWGI2:x.state=0:Next
End If
End Sub

Sub InitGIColor
dim xx
select case GIcolor
case 0:
For each xx in GI:xx.color = RGB(255,238,215):xx.colorfull = RGB (228,152,58): Next
case 1:
For each xx in GI:xx.color = RGB(255,238,215):xx.colorfull = RGB(255,238,215): Next
case 2:
GI1.color = RGB(255,238,215):GI1.colorfull = RGB(25,255,25)
GI2.color = RGB(255,238,215):GI2.colorfull = RGB(25,255,25)
GI3.color = RGB(255,238,215):GI3.colorfull = RGB(25,255,25)
GI4.color = RGB(255,238,215):GI4.colorfull = RGB(25,255,25)
GI5.color = RGB(255,238,215):GI5.colorfull = RGB(25,255,25)
GI6.color = RGB(255,238,215):GI6.colorfull = RGB(25,255,25)

GI7.color = RGB(255,25,25):GI7.colorfull = RGB(255,25,25)
GI8.color = RGB(255,25,25):GI8.colorfull = RGB(255,25,25)
GI9.color = RGB(255,25,25):GI9.colorfull = RGB(255,25,25)

GI10.color = RGB(25,25,255):GI10.colorfull = RGB(25,25,255)
GI11.color = RGB(25,25,255):GI11.colorfull = RGB(25,25,255)
GI12.color = RGB(25,25,255):GI12.colorfull = RGB(25,25,255)
GI13.color = RGB(25,25,255):GI13.colorfull = RGB(25,25,255)
GI14.color = RGB(25,25,255):GI14.colorfull = RGB(25,25,255)
GI15.color = RGB(25,25,255):GI15.colorfull = RGB(25,25,255)
GI16.color = RGB(25,25,255):GI16.colorfull = RGB(25,25,255)
GI17.color = RGB(25,25,255):GI17.colorfull = RGB(25,25,255)
GI18.color = RGB(25,25,255):GI18.colorfull = RGB(25,25,255)
GI19.color = RGB(25,25,255):GI19.colorfull = RGB(25,25,255)
GI20.color = RGB(25,25,255):GI19.colorfull = RGB(25,25,255)
GI21.color = RGB(25,25,255):GI19.colorfull = RGB(25,25,255)

GI22.color = RGB(255,25,25):GI20.colorfull = RGB(255,25,25)
GI23.color = RGB(255,25,25):GI21.colorfull = RGB(255,25,25)
GI24.color = RGB(255,25,25):GI22.colorfull = RGB(255,25,25)
GI25.color = RGB(255,25,25):GI23.colorfull = RGB(255,25,25)
For each xx in GIMiniPF:xx.color = RGB(25,25,255):xx.colorfull = RGB(25,25,255): Next
end select
End Sub

'******************************************************
'* REAL TIME UPDATES *********************************
'******************************************************

Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
RightFlipperP.Rotz = RightFlipper.CurrentAngle
LeftFlipperP.Rotz = LeftFlipper.CurrentAngle
LeftFlipperSmallP.RotY = LeftFlipperSmall.CurrentAngle
MiniPF_LeftFlipperP.Rotz = MiniPF_LeftFlipper.CurrentAngle
MiniPF_RightFlipperP.Rotz = MiniPF_RightFlipper.CurrentAngle
Gate1p.Roty = Gate1.CurrentAngle
LeftGatep.Rotx = LeftGate.CurrentAngle
RightSpinnerP.Rotx = sw39.CurrentAngle
RightSpinnerP1.Rotx = sw39.CurrentAngle
LeftGateSwitch.Rotx = LeftGateFl.CurrentAngle
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

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 5000)
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

Const tnob = 10 ' total number of balls
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

 '**********************
'Flipper Shadows
'***********************
Sub RealTime_Timer
  lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
End Sub


Sub BallShadow_Timer()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8,BallShadow9,Ballshadow10)
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
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10                       
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 80
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub

Sub Gates_Hit (idx)
	PlaySound "fx_gate", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
Dim Ballspeed
Ballspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
If Ballspeed >5 then Playsound "Stern_Flippercollide1" Else Playsound "Stern_FlipperCollideLow":End If:End Sub

Sub RightFlipper_Collide(parm)
Dim Ballspeed
Ballspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
If Ballspeed >5 then Playsound "Stern_Flippercollide1" Else Playsound "Stern_FlipperCollideLow":End If:End Sub

Sub WirerollSND_Hit:Playsound"Stern_Wireroll":End Sub
Sub BallDropSound_Hit:Stopsound"Stern_Wireroll":Playsound"Stern_Balldrop":End Sub


