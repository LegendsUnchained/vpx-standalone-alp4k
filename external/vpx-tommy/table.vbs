'The Who's Tommy Pinball Wizard - IPDB No. 2579
'© Data East 1994
'Table Recreation by ninuzzu for Visual Pinball 10
'Credits/Thanks (in no particular order):
'Franzleo for the ramps and the airplane models
'Freneticamnesic for the help with the mirror animation
'Dark and Zany for some primitive templates I used and edited
'Javier1515,JPSalas,GTXJoe and Shoopity for some code and sounds effects I borrowed from their tables
'Arngrim for the DOF support
'VPDev Team for the new freaking amazing VPX

Option Explicit
Randomize

Dim Scoop_kick, FlipperColor, PostColor, RubberColor, UnionJackMod, TNTMod

'***********************************************************************
'* TABLE OPTIONS *******************************************************
'***********************************************************************

'***********	Set the Left Scoop kick direction	********************

Scoop_kick = 2				'0 = left side , 1 = right side, 2 = random

'***********	Set the Color of Flippers Rubbers	********************

FlipperColor = 2			'0 = black , 1 = red, 2 = random

'***********	Set the Color of the Rubber Posts	********************

PostColor = 2				'0 = black , 1 = yellow, 2 = random

'***********	Set the Color of Rubbers	****************************

RubberColor = 2				'0 = black , 1 = white, 2 = random

'***********	Enable Union Jack Mod (Targets with UK flag)	********

UnionJackMod = 1			'0 = disabled , 1 = enabled

'***********	Enable TNT Amusements Mod	****************************

TNTMod = 1					'0 = disabled , 1 = enabled

'******************************************************
'* VPM INIT *******************************************
'******************************************************

Const Ballsize = 51
Dim BallMass:BallMass=(BallSize^3)/125000

Dim DesktopMode:DesktopMode = Tommy.ShowDT
Dim UseVPMDMD: UseVPMDMD = DesktopMode

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "02000000", "de.vbs", 3.49

Sub Tommy_Paused:Controller.Pause = 1:End Sub
Sub Tommy_unPaused:Controller.Pause = 0:End Sub
Sub Tommy_exit():Controller.Stop:End Sub

'******************************************************
'* STANDARD DEFINITIONS *******************************
'******************************************************

Const UseSolenoids = 1
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0
Const SSolenoidOn ="fx_solon"
Const SSolenoidOff = ""
Const SFlipperOn = ""
Const SFlipperOff = ""
Const SCoin = "fx_coin"

'******************************************************
'* ROM VERSION ****************************************
'******************************************************

Const cGameName = "tomy_500"

'******************************************************
'* KEYS ***********************************************
'******************************************************

Dim BIP:BIP = 0				'balls in plunger

Sub Tommy_KeyDown(ByVal Keycode)
If keycode = 3 Then Controller.Switch(8)=1                    		'extra ball button
If keycode = LeftTiltKey Then Nudge 90,2:PlaySound SoundFX("fx_nudge",0)
If keycode = RightTiltKey Then Nudge 270,2:PlaySound SoundFX("fx_nudge",0)
If keycode = CenterTiltKey Then Nudge 0,3:PlaySound SoundFX("fx_nudge",0)
If keycode = PlungerKey Then Plunger.Pullback:PlaySound "fx_plungerpull"
If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Tommy_KeyUp(ByVal Keycode)
If keycode = 3 Then Controller.Switch(8)=0
If keycode = PlungerKey Then 
Plunger.Fire
If BIP = 0 then
PlaySound "fx_plunger"
Else
PlaySound "fx_launch"
End If
End If
If vpmKeyUp(keycode) Then Exit Sub
End Sub

'******************************************************
'* TABLE INIT *****************************************
'******************************************************

Dim bsTrough

Sub Tommy_Init

'* ROM AND DMD ****************************************

With Controller
.GameName = cGameName
.SplashInfoLine = "The Who's Tommy the Pinball Wizard - Data East 1994"
.HandleMechanics = 0
.HandleKeyboard = 0
.ShowDMDOnly = 1
.ShowFrame = 0
.ShowTitle = 0
.Hidden = DesktopMode
End With
On Error Resume Next
Controller.Run
If Err Then MsgBox Err.Description
On Error Goto 0

'* PINMAME TIMER **************************************

PinMAMETimer.Interval = PinMAMEInterval
PinMAMETimer.Enabled = 1

'* NUGDE **********************************************

vpmNudge.TiltSwitch=1
vpmNudge.Sensitivity=3
vpmNudge.TiltObj=Array(bumper1,bumper2,bumper3,LeftSlingshot,RightSlingshot)

'* TROUGH *********************************************

Set bsTrough = New cvpmTrough
With bsTrough
.Size = 6
.InitSwitches Array(14, 13, 12, 11, 10, 9)
.Initexit BallTrough, 70, 5
.Balls = 6
.InitExitSounds SoundFX("fx_trough",DOFContactors), SoundFX("fx_trough",DOFContactors)
End With

'* STARTUP CALLS **************************************

GIoff
captiveBall.CreateSizedBallWithMass Ballsize/2,BallMass
captiveball.kick 180,1
captiveball.enabled= 0
TopDiverter.IsDropped = 1

If DesktopMode = true then
l39.visible= 0
l39a.visible= 1
l40.visible= 0
l40a.visible= 1
l56.visible= 0
l56a.visible= 1
l60.visible= 0
l60a.visible= 1
l61.visible= 0
l61a.visible= 1
leftrail.visible= 1
rightrail.visible= 1
Else
l39.visible= 1
l39a.visible= 0
l40.visible= 1
l40a.visible= 0
l56.visible= 1
l56a.visible= 0
l60.visible= 1
l60a.visible= 0
l61.visible= 1
l61a.visible= 0
leftrail.visible= 0
rightrail.visible= 0
End If

Randomize
If FlipperColor = 2 then FlipperColor = RndNum(0,1)
If FlipperColor = 1 then
LeftFlipperP.image = "flippers-red"
LeftFlipper1P.image = "flippers-red"
RightFlipperP.image = "flippers-red"
End If

Randomize
If PostColor = 2 then PostColor = RndNum(0,1)
If PostColor = 1 AND TNTMod = 0 then
RubberPost1.image = "rubber-post-t1-yellow"
RubberPost2.image = "rubber-post-t1-yellow"
RubberPost5.image = "rubber-post-t1-yellow"
RubberPost6.image = "rubber-post-t1-yellow"
RubberPost11.image = "rubber-post-t1-yellow"
End If

Dim x
Randomize
If RubberColor = 2 then RubberColor = RndNum(0,1)
If RubberColor = 1 then
for each x in Rubbers:x.material = "Rubber White":next
End If

If UnionJackMod = 1 then
sw20.image = "targetT1Small_UJ"
sw21.image = "targetT1Small_UJ"
sw22.image = "targetT1Small_UJ"
sw38.image = "targetT1Round_UJ"
sw43.image = "targetT1Round_UJ"
End If

Dim xx
If TNTMod = 1 then
for each xx in GITNTBlue: xx.color = RGB (0,0,255):xx.intensity = 150: next
for each xx in GITNTBlue1: xx.color = RGB (0,0,255): next
for each xx in GITNT:xx.intensity = 50: next
FR9.opacity = 2500
FR10.opacity = 2500
bulb5.visible = 1
bulb6.visible = 1
bulb7.visible = 1
bulb8.visible = 1
RubberPost1.image = "rubber-post-t1-orange"
RubberPost2.image = "rubber-post-t1-orange"
RubberPost5.image = "rubber-post-t1-orange"
RubberPost6.image = "rubber-post-t1-orange"
RubberPost9.image = "rubber-post-t1-orange"
RubberPost10.image = "rubber-post-t1-orange"
RubberPost11.image = "rubber-post-t1-orange"
End If

End Sub

'******************************************************
'* SOLENOIDS ******************************************
'******************************************************

SolCallback(1)	= "SolTrough"                           				'6-ball lockout							(1L)
SolCallback(2)	= "SolRelease"                     						'ball eject								(2L)
SolCallback(3)  = "AutoLaunch"                          				'autolaunch                           	(3L)
SolCallback(4)	= "ExitVUK"                             				'VUK                                  	(4L)
SolCallback(5)	= "ExitScoop"                           				'LeftScoop.                           	(5L)
SolCallback(6)	= "SolEject"                            				'Top Left Eject                       	(6L)
SolCallback(8)	= "vpmSolSound SoundFX(""fx_knocker"",DOFKnocker),"		'Knocker							  	(8L)
'SolCallback(9)                                         				'Shaker motor                         	(09)    
'SolCallback(10)														'L/R Relay                            	(10)
SolCallback(11) = "GIRelay"                             				'GI Relay                             	(11)
SolCallback(12) = "Diverter"                            				'Top Diverter                         	(12)
SolCallback(13) = "PropellerMove"                       				'Airplane propellers                  	(13)
SolCallBack(14) = "MirrorMove"                          				'Mirror Motor                        	(14)
SolCallBack(15) = "setlamp 115,"   										'Flashlamp X1 Mirror                  	(15)
SolCallback(17) = "SolBumper1"                          				'Top Bumper                           	(17)
SolCallback(18) = "SolBumper2"                          				'Center bumper                        	(18)
SolCallback(19) = "SolBumper3"                          				'Right bumper                         	(19)
SolCallback(20) = "SolLSling"                           				'Left Slinghshot                      	(20)
SolCallback(21) = "SolRSling"                           				'Right Slingshot                      	(21)
SolCallBack(25) = "setlamp 125,"					    				'Flashlamp X4 Bottom Arch L/R         	(1R)
SolCallBack(26) = "setlamp 126,"										'Flashlamp X4 Upper Right Corner		(2R)
SolCallBack(27) = "setlamp 127,"  										'Flashlamp X2 Left Scooop             	(3R)
SolCallBack(28) = "setlamp 128,"  										'Flashlamp X2 Top Eject					(4R)
SolCallBack(29) = "setlamp 129,"  										'Flashlamp X4 Bumpers Hot Dog			(5R)
SolCallBack(30) = "setlamp 130," 										'Flashlamp X4 Back Panel				(6R)
SolCallBack(31) = "setlamp 131,"  										'Flashlamp X2 Lower Right Hot Dogs		(7R)
SolCallBack(32) = "setlamp 132,"     									'Flashlamp X4 Top Hot Dogs				(8R)
SolCallback(46) = "SolRFlipper"                         				'Right Flipper
SolCallback(47) = "SolULFlipper"                        				'Upper Left Flipper
SolCallback(48) = "SolLFlipper"                         				'Left Flipper
SolCallback(51) = "BlinderMove"                         				'Blinder Motor

'******************************************************
'* DRAIN **********************************************
'******************************************************

Sub Drain_Hit()
PlaySound "fx_drain"
BsTrough.AddBall Me
End Sub

'******************************************************
'* TROUGH *********************************************
'******************************************************

Sub SolTrough(Enabled)
If enabled Then
bsTrough.ExitSol_On
Controller.Switch(15) = 1
End If
End Sub

Sub SolRelease(Enabled)
If enabled Then
BallRelease.kick 90, 8
Controller.Switch(15) = 0
Playsound SoundFX("fx_ballrel",DOFContactors)
End If
End Sub

'******************************************************
'* AUTOLAUNCH *****************************************
'******************************************************

Dim plungerIM
Const IMPowerSetting = 46
Const IMTime = 0.5
Set plungerIM = New cvpmImpulseP
With plungerIM
.InitImpulseP swPlunger, IMPowerSetting, IMTime
.Switch 16
.Random 1.5
.InitExitSnd "fx_launch", SoundFX("fx_solon",DOFContactors)
.CreateEvents "plungerIM"
End With

Sub AutoLaunch(Enabled)
If Enabled Then
PlungerIM.AutoFire
End If
End Sub

'******************************************************
'* VUK ************************************************
'******************************************************

Dim BIK:BIK = 0				'balls in kicker

Sub exitVUK(enabled)
If enabled then
sw19k.kick 0, 40, 1.56
Controller.Switch(19) = 0
vpmTimer.AddTimer 2000, "sw19.enabled=1 '"
If BIK = 0 then 
Playsound SoundFX("fx_solon",DOFContactors)
Else
Playsound SoundFX("fx_vuk_exit",DOFContactors)
End If
End If
End Sub

Sub sw19k_hit
BIK = BIK + 1
PlaySound "fx_kicker_catch"
StopSound "fx_subway"
Controller.Switch(19) = 1
sw19.enabled= 0
End Sub

Sub sw19k_Unhit
BIK = BIK - 1
End Sub

Sub sw19_hit:PlaySound "fx_vuk_enter":End sub

'******************************************************
'* LEFT SCOOP *****************************************
'******************************************************

Sub ExitScoop(enabled)
If enabled then
sw23k.Kick 0, 28, 1.56
Controller.Switch(23) = 0
vpmTimer.AddTimer 2000, "sw23.enabled=1 '"
Playsound SoundFX("fx_scoop_exit",DOFContactors)
End If
End Sub

sub sw23k_hit
Dim opt
Randomize
opt= RndNum(0,1)

If Scoop_kick = 2 then
ramp12.collidable=opt
ramp13.collidable=opt
If opt=1 then
ramp17.collidable=0
ramp18.collidable=0
Else
ramp17.collidable=1
ramp18.collidable=1
End If
End If
If Scoop_kick = 1 then
ramp12.collidable=0
ramp13.collidable=0
ramp17.collidable=1
ramp18.collidable=1
End If
PlaySound "fx_kicker_catch"
Controller.Switch(23) = 1
sw23.enabled= 0
End sub

Sub sw23_hit:PlaySound "fx_scoop_enter":End sub

'******************************************************
'* MIRROR, SKILLSHOT AND VUK THROUGH HOLES ************
'******************************************************

Sub sw41_Hit
vpmTimer.PulseSw 41
PlaySound "fx_subway"
End Sub

Sub sw42_Hit
vpmTimer.PulseSw 42
PlaySound "fx_subway"
End Sub

Sub swVUKHole_Hit
PlaySound "fx_hole_enter"
End Sub

'******************************************************
'* TOP THROUGH EJECT **********************************
'******************************************************

Sub SolEject(enabled)
If enabled then
sw47.kick 180,8
Playsound SoundFX("fx_saucer_exit",DOFContactors)
Controller.Switch(47) = 0
End If
End Sub

Sub sw47_Hit
PlaySound "fx_saucer_enter"
Controller.Switch(47) = 1
End Sub

'******************************************************
'* TOP DIVERTER ***************************************
'******************************************************

Sub Diverter(enabled)
If enabled then
Playsound SoundFX("fx_diverter",DOFContactors)
TopDiverter.IsDropped = 0
Else
Playsound SoundFX("fx_diverter",DOFContactors)
TopDiverter.IsDropped = 1
End If
End Sub

'******************************************************
'* PROPELLERS ANIMATION *******************************
'******************************************************

Dim discAngle:discAngle=0
Dim stepAngle, stopRotation

Sub PropellerMove(enabled)
	If Enabled Then 
		PropellerTimer.Enabled = 1
		Playsound SoundFX("fx_propellers_on",DOFGear)
		stepAngle=15
		stopRotation=0
	Else
		StopSound "fx_propellers_on"
		PlaySound SoundFX("fx_propellers_off",0)
		stopRotation=1
	End If
End Sub

Sub PropellerTimer_Timer()
	discAngle = discAngle + stepAngle
	If discAngle >= 360 Then
		discAngle = discAngle - 360
	End If
	Propeller1.RotZ = 360 - discAngle
	Propeller2.RotZ = 360 - discAngle
	If stopRotation Then
		stepAngle = stepAngle - 0.2
		If stepAngle <= 0 Then
			PropellerTimer.Enabled 	= 0
		End If
	End If
End Sub

'******************************************************
'* MIRROR ANIMATION ***********************************
'******************************************************

Dim MirrorDown:MirrorDown = 0

Sub sw32_Hit
vpmTimer.PulseSw 32
PlaySound "fx_mirror_hit", 0, Vol(ActiveBall)*4, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
ShakeMirror
End Sub

Sub MirrorMove(Enabled)
If MirrorP.Z <= 3 then
If Enabled then
MirrorDown = False
Playsound SoundFX("fx_motor",DOFGear), 0,1,0,0.25
End If
End If
If MirrorP.Z >= 137 then
If Enabled then
MirrorDown = True
Playsound SoundFX("fx_motor",DOFGear), 0,1,0,0.25
End If
End If
End Sub

Sub MirrorTimer_Timer()	
If MirrorDown = True and MirrorP.Z >= 0 then 
MirrorP.Z = MirrorP.Z - 2	
End If
If MirrorDown = False and MirrorP.Z <= 140 then 
MirrorP.Z = MirrorP.Z + 2
End If
If MirrorP.Z >= 137 then
Controller.Switch(28) = 1:sw32.isdropped=0
Else
Controller.Switch(28) = 0
End If
If MirrorP.Z <= 3 then
Controller.Switch(31) = 1:sw32.isdropped=1
Else
Controller.Switch(31) = 0
End If
End Sub

'******************************************************
'* MIRROR SHAKE CODE BASED ON JP'S ********************
'******************************************************

Dim MirrorPos

Sub ShakeMirror
Dim finalspeed
finalspeed=INT(1 + SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely))
If finalspeed >= 8 then
MirrorPos = 8
Else
MirrorPos = INT(9 - 8 / finalspeed)
End If
MirrorShakeTimer.Enabled = 1
End Sub

Sub MirrorShakeTimer_Timer
    MirrorP.TransX = MirrorPos
    If MirrorPos = 0 Then MirrorShakeTimer.Enabled = 0:Exit Sub
    If MirrorPos < 0 Then
        MirrorPos = ABS(MirrorPos) - 1
    Else
        MirrorPos = - MirrorPos + 1
    End If
End Sub

'******************************************************
'* BLINDER ANIMATION **********************************
'******************************************************

Dim StatusBlinder:StatusBlinder=1      'initial status is 1 and timers are off (no movement)
BlinderForward.enabled=0
BlinderBack.enabled=0

Sub BlinderMove(enabled)
    If enabled then
    If StatusBlinder=1 then   
       BlinderForward.enabled=1
       BlinderBack.enabled=0
       Playsound "fx_blinder", 0,1,0,0.25
    End If
    End If
    If not enabled then
    If StatusBlinder=2 then
       BlinderBack.enabled=1
       BlinderForward.enabled=0
       Playsound "fx_blinder", 0,1,0,0.25
    End If
    End If
End Sub

Sub BlinderForward_timer
BlinderP2.rotY= BlinderP2.rotY - 3
if BlinderP2.rotY <=-52 then blinderP1.rotY= blinderP1.rotY - 3
if BlinderP1.rotY <=-48 then BlinderForward.enabled=0:StatusBlinder=2
End Sub

Sub BlinderBack_timer
BlinderP2.rotY= BlinderP2.rotY + 3
if BlinderP2.rotY >=-52 then blinderP1.rotY= blinderP1.rotY + 3
if BlinderP1.rotY >=0 then BlinderP2.rotY=0:BlinderBack.enabled=0:StatusBlinder=1
End Sub

'******************************************************
'* SLINGSHOTS *****************************************
'******************************************************

Dim Lstep, RStep

Sub LeftSlingShot_Slingshot: vpmTimer.PulseSw 17: End Sub
Sub RightSlingShot_Slingshot: vpmTimer.PulseSw 18: End Sub

Sub solLSling(enabled)
	If enabled then
		PlaySound SoundFX ("fx_slingshot_left",DOFContactors)
		LSling.Visible = 0
		LSling1.Visible = 1
		sling1.TransZ = -23
		LStep = 0
		LeftSlingShot.TimerEnabled = 1
	End If
End Sub

Sub solRSling(enabled)
	If enabled then
		PlaySound SoundFX ("fx_slingshot_right",DOFContactors)
		RSling.Visible = 0
		RSling1.Visible = 1
		sling2.TransZ = -23
		RStep = 0
		RightSlingShot.TimerEnabled = 1
	End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling1.TransZ = -12
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling1.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling2.TransZ = -12
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling2.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'************************************************
' BUMPERS ***************************************
'************************************************

Dim dirRing1:dirRing1 = -1
Dim dirRing2:dirRing2 = -1
Dim dirRing3:dirRing3 = -1

Sub Bumper1_Hit:vpmTimer.PulseSw 49:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 50:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 51:End Sub

Sub solBumper1 (enabled): If enabled then Bumper1.TimerEnabled = 1: PlaySound SoundFX("fx_bumper1",DOFContactors): End If: End Sub
Sub solBumper2 (enabled): If enabled then Bumper2.TimerEnabled = 1: PlaySound SoundFX("fx_bumper2",DOFContactors): End If: End Sub
Sub solBumper3 (enabled): If enabled then Bumper3.TimerEnabled = 1: PlaySound SoundFX("fx_bumper3",DOFContactors): End If: End Sub

Sub Bumper1_timer()
	BR1.Z = BR1.Z + (5 * dirRing1)
	If BR1.Z <= -40 Then dirRing1 = 1
	If BR1.Z >= -5 Then
		dirRing1 = -1
		BR1.Z = -5
		Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper2_timer()
	BR2.Z = BR2.Z + (5 * dirRing2)
	If BR2.Z <= -40 Then dirRing2 = 1
	If BR2.Z >= -5 Then
		dirRing2 = -1
		BR2.Z = -5
		Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper3_timer()
	BR3.Z = BR3.Z + (5 * dirRing3)
	If BR3.Z <= -40 Then dirRing3 = 1
	If BR3.Z >= -5 Then
		dirRing3 = -1
		BR3.Z = -5
		Me.TimerEnabled = 0
	End If
End Sub

'******************************************************
'* STANDUP TARGETS ************************************
'******************************************************

'* Silverball Target **********************************

Sub sw24_Hit
vpmTimer.PulseSw 24
sw24p.transx = -10
PlaySound("fx_collide"), 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
Me.TimerEnabled = 1
End Sub

 Sub sw24_Timer:sw24p.transx = 0:Me.TimerEnabled = 0:End Sub

'* Left Ramp Left Standup Target **********************
 Sub sw20_Hit:vpmTimer.PulseSw 20:PlaySound SoundFX("fx_target",DOFContactors):End Sub

'* Left Ramp Right Standup Target *********************

 Sub sw21_Hit:vpmTimer.PulseSw 21:PlaySound SoundFX("fx_target",DOFContactors):End Sub

'* Right Ramp Standup Target **************************
 Sub sw22_Hit:vpmTimer.PulseSw 22:PlaySound SoundFX("fx_target",DOFContactors):End Sub

'* Middle Standup Target ******************************
 Sub sw38_Hit:vpmTimer.PulseSw 38:PlaySound SoundFX("fx_target",DOFContactors):End Sub

'* Captive Ball Standup Target ************************
 Sub sw43_Hit:vpmTimer.PulseSw 43:PlaySound SoundFX("fx_target",DOFContactors):End Sub

'* Targets Bank Left **********************************

 Sub sw25_Hit:vpmTimer.PulseSw 25:PlaySound SoundFX("fx_target",DOFContactors):End Sub
 Sub sw26_Hit:vpmTimer.PulseSw 26:PlaySound SoundFX("fx_target",DOFContactors):End Sub
 Sub sw27_Hit:vpmTimer.PulseSw 27:PlaySound SoundFX("fx_target",DOFContactors):End Sub

'* Targets Bank Right *********************************

 Sub sw33_Hit:vpmTimer.PulseSw 33:PlaySound SoundFX("fx_target",DOFContactors):End Sub
 Sub sw34_Hit:vpmTimer.PulseSw 34:PlaySound SoundFX("fx_target",DOFContactors):End Sub
 Sub sw35_Hit:vpmTimer.PulseSw 35:PlaySound SoundFX("fx_target",DOFContactors):End Sub

'******************************************************
'* ROLLOVER SWITCHES **********************************
'******************************************************

Sub sw16_Hit():Controller.Switch(16) = 1:PlaySound "fx_sensor":BIP=BIP+1:End Sub
Sub sw16_Unhit:Controller.Switch(16) = 0:BIP=BIP-1:End Sub
Sub sw29_Hit():Controller.Switch(29) = 1:PlaySound "fx_sensor":End Sub
Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub
Sub sw36_Hit():Controller.Switch(36) = 1:PlaySound "fx_sensor":End Sub
Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub
Sub sw37_Hit():Controller.Switch(37) = 1:PlaySound "fx_sensor":End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub
Sub sw48_Hit():Controller.Switch(48) = 1:PlaySound "fx_sensor":End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub
Sub sw55_Hit():Controller.Switch(55) = 1:PlaySound "fx_sensor":End Sub
Sub sw55_UnHit:Controller.Switch(55) = 0:End Sub
Sub sw56_Hit():Controller.Switch(56) = 1:PlaySound "fx_sensor":End Sub
Sub sw56_UnHit:Controller.Switch(56) = 0:End Sub

'******************************************************
'* RAMP SWITCHES **************************************
'******************************************************

 Sub sw30_Hit():Controller.Switch(30) = 1:If ActiveBall.VelX > 0 Then PlaySound "fx_rrenter": End If:End Sub
 Sub sw30_UnHit:Controller.Switch(30) = 0:End Sub
 Sub sw57_Hit():Controller.Switch(57) = 1:If ActiveBall.VelY < 0 Then PlaySound "fx_rrenter": End If:End Sub
 Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
 Sub sw58_Hit():Controller.Switch(58) = 1:flipper1.rotatetoend:PlaySound "fx_sensor":End Sub
 Sub sw58_UnHit:Controller.Switch(58) = 0:flipper1.rotatetostart:End Sub
 Sub sw62_Hit():Controller.Switch(62) = 1:flipper2.rotatetoend:PlaySound "fx_sensor":End Sub
 Sub sw62_UnHit:Controller.Switch(62) = 0:flipper2.rotatetostart:End Sub
 
'******************************************************
'* SPINNERS *******************************************
'******************************************************

 Sub sw39_Spin:vpmTimer.PulseSw 39:PlaySound "fx_spinner":End Sub
 Sub sw40_Spin:vpmTimer.PulseSw 40:PlaySound "fx_spinner":End Sub

'******************************************************
'* FLIPPERS *******************************************
'******************************************************

Sub SolLFlipper(Enabled)
If Enabled Then
LeftFlipper.RotateToEnd:PlaySound SoundFX("fx_flipper1",DOFContactors)
Else
LeftFlipper.RotateToStart:PlaySound SoundFX("fx_flipperdown",DOFContactors)
End If
End Sub

Sub SolRFlipper(Enabled)
If Enabled Then
RightFlipper.RotateToEnd:PlaySound SoundFX("fx_flipper2",DOFContactors)
Else
RightFlipper.RotateToStart:PlaySound SoundFX("fx_flipperdown",DOFContactors)
End If
End Sub

Sub SolULFlipper(Enabled)
If Enabled Then
LeftFlipper1.RotateToEnd:PlaySound SoundFX("fx_flipper2",DOFContactors)
Else
LeftFlipper1.RotateToStart:PlaySound SoundFX("fx_flipperdown",DOFContactors)
End If
End Sub

'******************************************************
'* REALTIME UPDATES ***********************************
'******************************************************

Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
UpdateMechs
RollingSound
BallShadowUpdate
End Sub

Sub UpdateMechs
LeftFlipperP.RotZ = LeftFlipper.CurrentAngle
LeftFlipper1P.RotZ = LeftFlipper1.CurrentAngle
RightFlipperP.RotZ = RightFlipper.CurrentAngle
FlipperLSh.RotZ = LeftFlipper.CurrentAngle
FlipperL1Sh.RotZ = LeftFlipper1.CurrentAngle
FlipperRSh.RotZ = RightFlipper.CurrentAngle
sw57p.RotX = Spinner_LeftRamp.currentangle+90
sw30p.RotX = Spinner_RightRamp.currentangle+90
sw58p.RotZ = -Flipper1.currentangle
sw62p.RotZ = -Flipper2.currentangle
End Sub

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
LampTimer.Interval = 40		'lamp fading speed
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
 
Sub UpdateLamps()

'NFadeL 1, l1				'Tommy T Backbox
NFadeL 2, l2
NFadeL 3, l3
NFadeL 4, l4
NFadeL 5, l5
NFadeL 6, l6
NFadeL 7, l7
NFadeL 8, l8
NFadeL 9, l9
'NFadeL 10, l10				'Tommy O Backbox
NFadeL 11, l11
NFadeL 12, l12
NFadeL 13, l13
NFadeL 14, l14
NFadeL 15, l15
NFadeL 16, l16
NFadeL 17, l17
NFadeL 18, l18
'NFadeL 19, l19				'Tommy M Backbox
NFadeLm 20, l20
Flash 20, TL20
NFadeLm 21, l21
Flash 21, TL21
NFadeLm 22, l22
Flash 22, TL22
'NFadeL 23					'Extra Ball Button Cabinet
NFadeLm 24, l24
Flash 24, TL24
NFadeLm 25, l25
Flash 25, TL25
NFadeLm 26, l26
Flash 26, TL26
NFadeLm 27, l27
Flash 27, TL27
'NFadeL 28,l28				'Tommy M Backbox
NFadeL 29, l29
NFadeL 30, l30
NFadeL 31, l31
NFadeL 32, l32
NFadeLm 33, l33
Flash 33, TL33
NFadeLm 34, l34
Flash 34, TL34
NFadeLm 35, l35
Flash 35, TL35
NFadeL 36, l36
'NFadeL 37,l37				'Tommy Y Backbox
NFadeLm 38, l38
Flash 38, TL38

NFadeLm 39, l39				'red
Flash 39, l39a				'red used in Desktop Mode
NFadeLm 40, l40				'red
Flash 40, l40a				'red used in Desktop Mode

NFadeL 41, l41
NFadeL 42, l42
NFadeL 43, l43
NFadeL 44, l44
NFadeL 45, l45
NFadeLm 46, l46
NFadeLm 46, l46a
Flashm 46, TL46
Flash 46, TL46a
NFadeL 47, l47
NFadeL 48, l48

NFadeLm 49, l49
NFadeLm 49, l49a
NFadeLm 49, l49b
NFadeLm 49, l49c
NFadeL 49, l49d

NFadeLm 50, l50
NFadeLm 50, l50a
NFadeLm 50, l50b
NFadeLm 50, l50c
NFadeL 50, l50d

NFadeLm 51, l51
NFadeLm 51, l51a
NFadeLm 51, l51b
NFadeLm 51, l51c
NFadeL 51, l51d

NFadeL 52, l52
NFadeL 53, l53
NFadeL 54, l54
NFadeLm 55, l55
NFadeL 55, l55a

NFadeLm 56, l56
NFadeL 56, l56a				'used in Desktop Mode

NFadeL 57, l57
NFadeL 58, l58
NFadeL 59, l59

NFadeLm 60, l60				'yellow
Flash 60, l60a				'yellow used in Desktop Mode
NFadeLm 61, l61				'yellow
Flash 61, l61a				'yellow used in Desktop Mode

NFadeL 62, l62
NFadeL 63, l63

NFadeLm 115,F15
NFadeL 115,F15A

NFadeLm 125,F25A
NFadeLm 125,F25B
NFadeLm 125,F25C
NFadeL 125,F25D

NFadeLm 126,F26A
NFadeLm 126,F26A0
NFadeLm 126,F26A1
NFadeLm 126,F26A2
NFadeLm 126,F26A3
NFadeLm 126,F26A4
NFadeLm 126,F26A5
NFadeLm 126,F26A6
NFadeLm 126,F26B
Flashm 126,F26R
Flash 126,F26R1

NFadeLm 127,F27A
NFadeLm 127,F27A0
NFadeLm 127,F27A1
NFadeLm 127,F27A2
NFadeLm 127,F27A3
Flash 127,F27R

NFadeLm 128,F28A
NFadeL 128,F28B

NFadeLm 129,F29A
NFadeL 129,F29B

NFadeLm 130,F3E
NFadeLm 130,F3F
Flashm 130,F3A
Flashm 130,F3B
Flashm 130,F3C
Flashm 130,F3D
Flashm 130,FA
Flash 130,FB

NFadeLm 131,F31A
NFadeL 131,F31B

NFadeLm 132,F32A
NFadeLm 132,F32A0
NFadeLm 132,F32A1
NFadeLm 132,F32A2
NFadeLm 132,F32A3
NFadeLm 132,F32A4
NFadeLm 132,F32A5
NFadeLm 132,F32A6
NFadeLm 132,F32B
NFadeLm 132,F32C
NFadeLm 132,F32D
Flashm 132,F32R
Flash 132,F32R1

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
'* GI *************************************************
'******************************************************
Dim bulb

Sub GIRelay(enabled)
If enabled Then
Gioff
Playsound "fx_relay_off"
Tommy.ColorGradeImage="ColorGrade_off"
Else
Gion
Playsound "fx_relay_on"
Tommy.ColorGradeImage="ColorGrade_on"
End If
End Sub

Sub GIon
F26B.intensity = 20
F27A3.intensity = 8
F26R.opacity = 5000
F27R.opacity = 5000
F32R.opacity = 5000
For each bulb in GI:bulb.state=1: Next
For each bulb in GICab:bulb.IntensityScale=1: Next
If TNTMod = 1 then
for each bulb in GITNTAir:bulb.IntensityScale=1: Next
End If
End Sub

Sub GIoff
F27A3.intensity = 25
F26B.intensity = 35
F26R.opacity = 40000
F27R.opacity = 40000
F32R.opacity = 40000
For each bulb in GI:bulb.state=0: Next
For each bulb in GICab:bulb.IntensityScale=0: Next	
for each bulb in GITNTAir:bulb.IntensityScale=0: Next
End Sub

'******************************************************
'* JP's VP10 Supporting Ball & Sound Functions ********
'******************************************************

Function RndNum(min,max)
	RndNum = Int(Rnd()*(max-min+1))+min  			' Sets a random number between min and max
End Function

Function Vol(ball) 									' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 500)
End Function

Function Pan(ball) 									' Calculates the pan for a ball based on the X position on the table. 
    Dim tmp											' "table1" is the name of the table
    tmp = ball.x * 2 / Tommy.width-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) 								' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 								'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'******************************************************
'* JP's VP10 COLLISION & ROLLING SOUNDS ***************
'******************************************************

Const tnob = 7 										' total number of balls

ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingSound()
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
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ) * 0.6, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 500, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'*****************************************
'	Ball Shadow
'*****************************************

Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7)

Sub BallShadowUpdate()
    Dim BOT, b
    BOT = GetBalls

	' render the shadow for each ball
    For b = 0 to UBound(BOT)
		If BOT(b).X < Tommy.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Tommy.Width/2))/7)) + 10
		Else
			BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Tommy.Width/2))/7)) - 10
		End If
		BallShadow(b).Y = BOT(b).Y + 20
		BallShadow(b).Z = 1
	
		If BOT(b).Z > 20 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub

'******************************************************
'* SOUND EFFECTS **************************************
'******************************************************

Sub RailTrigger_Hit
	If ActiveBall.VelY > -13 Then ActiveBall.VelY = -16
	If ActiveBall.VelY < -19.5 Then ActiveBall.VelY = -19.5		'let's sync the sound with the ball speed
	If ActiveBall.VelY < 0 Then									'ball is travelling up the ramp 
	PlaySound "fx_rampL"
	End if
End Sub

Sub RailTrigger2_Hit
If ActiveBall.VelY > 14.25 Then ActiveBall.VelY = 14.25			'let's sync the sound with the ball speed
PlaySound "fx_rampR"
End Sub

Sub RailEndTrigger2_Hit
vpmTimer.AddTimer 250, "BallHitSound"
End Sub

Sub RailEndTrigger_Hit
vpmTimer.AddTimer 250, "BallHitSound"
End Sub

Sub ShootTrigger_Hit
If ActiveBall.Z > 30  Then			'ball is flying
vpmTimer.AddTimer 150, "BallHitSound"
End If
End Sub

Sub BallHitSound(dummy):PlaySound "fx_balldrop":End Sub

Sub Bottomvuk_Trigger_Hit: PlaySound "fx_metals":End Sub

Sub LRHelp_hit : If activeball.vely > -6.5 then activeball.vely = -12: end if : End Sub

Sub Metals_Hit (idx)
	PlaySound "fx_metals", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "fx_rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "fx_rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "fx_rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub LeftFlipper1_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "fx_flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "fx_flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "fx_flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

'********************************************************************************************************************************
