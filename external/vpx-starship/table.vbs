'Starship Troopers (1997) Sega Pinball by eMBee/Sixtoe for VPX (Version 2.0)

'Changelog:
'V2.1: New plastics from Chucky87, numerous depth bias issues, changed romset to startrp2 and enabled fastflips thanks to TheLoafer, added DOF undercab thanks to Angrim, tweaked plunger position.
'V2.0: Rebuilt from the ground up. Most things replaced or changed. Added loads of stuff (Sixtoe), New toy primitives (eMBee).
'V1.1: Added Global lite to Live fire range, fixed analog/mechanical plunger, Moved couple of walls, changed Arachnid primivtive size and texture, cleanups
'V1.0: 'Script changes, new mechanical sounds, Adjusted ramps, new plastics, new lighting, new playfield, new apron, new materials en textures.

'Special Thanks to:
'32Assassin: For his SST code rebuild Table.
'DJRobX: Warrior bug code and answering Sixtoe's stupid code questions.
'Flupper: Help integrating his v2 flasher code, new ramps & textures, shadow layer.
'Thalamus: SSF sound support and fixes.
'Schreibi34: PF Inserts and for his lighting basics tutorial.
'Knorr: 3D Apron
'Zany/Dark: Domes, screws, nuts and bolts.
'JPSalas: Help with GI, lighting and overall appearance
'Vogliadicane: Upscaled plastics and images.

'Thanks to Destruk, TAB, TheManFromPost for VP8/9 version and DeVaL for the original VPX conversion.

'Thanks to all others who have helped directly or via threads support and tutorials

'All Logos, Copyrights and Trademarks are property of their respective owners.

'Stuff still left to do;
'Stepper motor sounds for warrior bug
'Implement staged flashers when vpinmame supports VPMModSol for Sega tables.

'Not bugs;
'Rear left flasher mount and dropship rear wing sticking through back wall, the real machine does this.

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'Const cGameName="startrp",UseSolenoids=1,UseLamps=0,UseGI=1,SCoin="coin" 'Non-FastFlip, Newer Rom
Const cGameName="startrp2",UseSolenoids=15,UseLamps=0,UseGI=1,SCoin="coin" 'Fastflip, Older Rom

LoadVPM "01530000","sega.vbs",3.1

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


 '******************************************************
 '					Plunger Options
 '******************************************************
 
 Plunger.MechStrength = 500
 Plunger.MomentumXfer = 1.75
 

'******************************************************
'					Sound Options
'******************************************************

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 2000    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 3    ' Bumpers volume.
Const VolRol    = 1    ' Rollovers volume.
Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
Const VolRB     = 1    ' Rubber bands volume.
Const VolRH     = 1    ' Rubber hits volume.
Const VolPo     = 1    ' Rubber posts volume.
Const VolPi     = 1    ' Rubber pins volume.
Const VolPlast  = 1    ' Plastics volume.
Const VolTarg   = 1    ' Targets volume.
Const VolWood   = 1    ' Woods volume.
Const VolKick   = 1    ' Kicker volume.
Const VolSpin   = 1.5  ' Spinners volume.
Const VolFlip   = 3    ' Flipper volume.
Const VolFlipH  = 1    ' Flipper Hit volume.
Const VolDrain  = 1    ' Drain volume.
Const VolGI     = 0.3   ' General Illumination volume.

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallBack(1)= "bsTrough.SolOut"
SolCallBack(2)= "Auto_Plunger"
SolCallBack(3)="bsBottom.SolOut"	'Extra Ball KickOut
SolCallBack(4)="VukTopPop"			'Centre KickOut
'SolCallback(5)="LMag.MagnetOn="	'TopLeft Magnet
'SolCallback(6)="RMag.MagnetOn="	'TopRight Magnet
SolCallBack(7)="SolBrainBug"		'BrainBug
SolCallBack(8)= "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
'SolCallBack(9)="vpmSolSound ""fx_bumper1"","
'SolCallBack(10)="vpmSolSound ""fx_bumper2"","
'SolCallBack(11)="vpmSolSound ""fx_bumper3"","
'SolCallBack(12)="vpmSolSound ""Sling"","
'SolCallBack(13)="vpmSolSound ""Sling"","
SolCallBack(14)="SolFlipperMini"	'MiniFlipper
'SolCallBack(15)='Left Flipper
'SolCallBack(16)='Right Flipper
'SolCallback(17)="SolMotorOn"		'Warrior Bug Stepper Motor #1
'SolCallback(18)="SolMotorOn"		'Warrior Bug Stepper Motor #2
'SolCallback(19)="SolMotorOn"		'Warrior Bug Stepper Motor #3
'SolCallback(20)="SolMotorOn"		'Warrior Bug Stepper Motor #4
SolCallBack(23)="SetLamp 123,"		'BrainBug Toy Flasher X2
'SolCallBack(24)='Optional Coin Meter
SolCallBack(25)="FlashRed"			'Red Flashers x4
SolCallBack(26)="Flashyellow"		'Yellow Flashers x4
SolCallBack(27)="Flashgreen"		'Green Flashers x4
SolCallBack(28)="Flashblue"			'Blue Flashers x4

Sub Flashred(flstate)
	If Flstate  Then
		ObjActive(1) = True : ObjActive(5) = True : ObjActive(9) = True : ObjActive(13) = True
		Flashlevelb(1) = 1 : FlasherFlash1_Timer : Flashlevelb(5) = 1 : FlasherFlash5_Timer : Flashlevelb(9) = 1 : FlasherFlash9_Timer: Flashlevelb(13) = 1 : FlasherFlash13_Timer 
		StopSound "Jon_GI_On"
		PlaySound "Jon_GI_On"
	Else
		ObjActive(1) = False : ObjActive(5) = False: ObjActive(9) = False: ObjActive(13) = False
	End If
End Sub

Sub Flashyellow(flstate)
	If Flstate  Then
		ObjActive(2) = True : ObjActive(6) = True : ObjActive(10) = True : ObjActive(14) = True
		Flashlevelb(2) = 1 : FlasherFlash2_Timer : Flashlevelb(6) = 1 : FlasherFlash6_Timer : Flashlevelb(10) = 1 : FlasherFlash10_Timer: Flashlevelb(14) = 1 : FlasherFlash14_Timer 
		StopSound "Jon_GI_On"
		PlaySound "Jon_GI_On"
	Else
		ObjActive(2) = False: ObjActive(6) = False: ObjActive(10) = False: ObjActive(14) = False
	End If
End Sub

Sub Flashgreen(flstate)
	If Flstate  Then
		ObjActive(3) = True : ObjActive(7) = True : ObjActive(11) = True : ObjActive(15) = True
		Flashlevelb(3) = 1 : FlasherFlash3_Timer : Flashlevelb(7) = 1 : FlasherFlash7_Timer : Flashlevelb(11) = 1 : FlasherFlash11_Timer: Flashlevelb(15) = 1 : FlasherFlash15_Timer 
		StopSound "Jon_GI_On"
		PlaySound "Jon_GI_On"
	Else
		ObjActive(3) = False : ObjActive(7) = False: ObjActive(11) = False: ObjActive(15) = False
	End If
End Sub

Sub Flashblue(flstate)
	If Flstate  Then
		ObjActive(4) = True : ObjActive(8) = True : ObjActive(12) = True : ObjActive(16) = True
		Flashlevelb(4) = 1 : FlasherFlash4_Timer : Flashlevelb(8) = 1 : FlasherFlash8_Timer : Flashlevelb(12) = 1 : FlasherFlash12_Timer: Flashlevelb(16) = 1 : FlasherFlash16_Timer 
		StopSound "Jon_GI_On"
		PlaySound "Jon_GI_On"
	Else
		ObjActive(4) = False : ObjActive(8) = False: ObjActive(12) = False: ObjActive(16) = False
	End If
End Sub

SolCallBack(29)="SetLamp 129," 'F5 WarBugSled Flasher X4
SolCallBack(30)="SetLamp 130," 'F6 Left Ramp Flasher X4
SolCallBack(31)="SetLamp 131," 'F7 Right Ramp Flasher X4
SolCallBack(32)="SetLamp 132," 'F8 Pop Bumpers Flasher X2

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFFlippers):LeftFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFFlippers):LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFFlippers):RightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFFlippers):RightFlipper.RotateToStart
     End If
End Sub

'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

Sub Auto_Plunger(Enabled) 'plunger
	If Enabled Then
		PlungerIM.AutoFire
	End If
End Sub

Sub SolFlipperMini(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFFlippers):RightFlipper1.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFFlippers):RightFlipper1.RotateToStart
     End If
End Sub

'Stern-Sega GI 
set GICallback = GetRef("UpdateGI")
Sub UpdateGI(no, Enabled)
'		SetLamp 134, Enabled	'Backwall bulbs
	If Enabled Then
		Dim xx
		For each xx in GI:xx.State = 1:Next
		DOF 101, DOFOn
		PlaySound "Jon_GI_On"
		FighterLights.blenddisablelighting = 0.5
		DropshipLights.blenddisablelighting = 0.5
	Else
		For each xx in GI:xx.State = 0:Next
		DOF 101, DOFOff
		PlaySound "Jon_GI_On"
		FighterLights.blenddisablelighting = 0
		DropshipLights.blenddisablelighting = 0
	End If
End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsBottom, LMag, RMag, mBug

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Starship Troopers"&chr(13)&"eMBee & Sixtoe"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 0
        .Games(cGameName).Settings.Value("sound")=1
		'.PuPHide = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

	if Controller.Version < "03030000" then MsgBox "This table requires vpinmame 3.3 rev 4895 or later. Yours reports " & Controller.Version & ".  Please get the latest SAMBuild from VPUniverse"

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

	vpmNudge.TiltSwitch=56:
	vpmNudge.Sensitivity=2
	vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot,Bumper1,Bumper2,Bumper3)

	Set bsTrough=New cvpmBallStack
		bsTrough.InitSw 0,15,14,13,12,0,0,0
		bsTrough.InitKick BallRelease,90,6
		bsTrough.InitExitSnd SoundFX("fx_ballRel",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsTrough.Balls=4

	Set bsBottom=New cvpmBallStack
		bsBottom.InitSaucer sw45,45,105,15
		bsBottom.InitExitSnd SoundFX("balleject",DOFContactors), SoundFX("Solenoid",DOFContactors)
'		bsBottom.KickAngleVar=20
'		bsBottom.KickForceVar = 5

	Set LMag=New cvpmMagnet
		LMag.InitMagnet LMagnet,100
		LMag.Solenoid=5
		LMag.GrabCenter=True
		LMag.CreateEvents "LMag"

	Set RMag=New cvpmMagnet
		RMag.InitMagnet RMagnet,100
		RMag.Solenoid=6
		RMag.GrabCenter=True
		RMag.CreateEvents "RMag"

 	sw38.IsDropped=1
 	sw38a.IsDropped=1

	BBUGdir = "down"
	BBUGtimer.enabled = true
 
     Set mBug=New cvpmMyMech
	' In VPX 10.7+ should use "vpmMechFourStepSol" which happens to be the same as setting both StepSol and TwoDirSol
	' mBug.MType=vpmMechFourStepSol+vpmMechStopEnd+vpmMechLinear+vpmMechFast
	mBug.MType=vpmMechStepSol+vpmMechTwoDirSol+vpmMechStopEnd+vpmMechLinear+vpmMechFast
	mBug.Sol1=17
	mBug.Length=600'150
	mBug.Steps=190
	mBug.AddSw 34,0,1
	mBug.AddSw 33,189,190
    mBug.Callback=GetRef("UpdateWarriorBug")
	mBug.Start

End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyCode=RightFlipperKey Then Controller.Switch(88)=1
	If KeyCode=LeftFlipperKey Then Controller.Switch(63)=1 
	If KeyCode=RightFlipperKey Then Controller.Switch(64)=1
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyCode=RightFlipperKey Then Controller.Switch(88)=0
	If KeyCode=LeftFlipperKey Then Controller.Switch(63)=0 
	If KeyCode=RightFlipperKey Then Controller.Switch(64)=0
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
	If KeyUpHandler(keycode) Then Exit Sub
End Sub

	' Impulse Plunger
	dim plungerIM

	Const IMPowerSetting = 60 ' Plunger Power
	Const IMTime = 0.6        ' Time in seconds for Full Plunge
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swPlunger, IMPowerSetting, IMTime
		.Random 0.3
		.Switch 16
		.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		.CreateEvents "plungerIM"
	End With

     plungerIM.Strength = 75

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:RandomSoundDrain:bsTrough.addball me : End Sub
Sub sw45_Hit:bsBottom.AddBall 0 : playsound "popper_ball": End Sub	

 '***********************************
 'Top Raising VUK sw46
 '***********************************
 'Variables used for VUK 
 Dim raiseballsw, raiseball 

 Sub TopVUK_Hit() 
	playsound "popper_ball"
	StopSound "subway"
 	TopVUK.Enabled=FALSE
	Controller.switch (46) = True
 End Sub
 
 Sub VukTopPop(enabled)
	if(enabled and Controller.switch (46)) then
		playsound SoundFX("fx_ScoopExit",DOFContactors)
		TopVUK.DestroyBall
 		Set raiseball = TopVUK.CreateBall
 		raiseballsw = True
 		TopVukraiseballtimer.Enabled = True 'Added by Rascal
		TopVUK.Enabled=TRUE	
 		Controller.switch (46) = False
	else
		'PlaySound "Popper"
	end if
End Sub
 
 Sub TopVukraiseballtimer_Timer()
 	If raiseballsw = True then
 		raiseball.z = raiseball.z + 10
 		If raiseball.z > 50 then
 			'msgbox ("Over")
 			TopVUK.Kick 182, 10
 			Set raiseball = Nothing
 			TopVukraiseballtimer.Enabled = False
 			raiseballsw = False
 		End If
 	End If
 End Sub

'Subway to sw46
Sub sw9_Hit:Controller.Switch(9)=1 : playsound"Subway" : End Sub 
Sub sw9_unHit:Controller.Switch(9)=0:End Sub
Sub sw10_Hit:Controller.Switch(10)=1 : playsound"Subway" : End Sub 
Sub sw10_unHit:Controller.Switch(10)=0:End Sub
Sub sw40_Hit:Controller.Switch(40)=1 : playsound"Subway" : End Sub 
Sub sw40_unHit:Controller.Switch(40)=0:End Sub

'Wire Triggers
Sub sw16_Hit : End Sub 'Coded to impulse plunger
'Sub sw16_unHit : Controller.Switch (16)=0:End Sub
Sub sw41_Hit:Controller.Switch(41)=1:End Sub 
Sub sw41_unHit:Controller.Switch(41)=0:End Sub
Sub sw42_Hit:Controller.Switch(42)=1:End Sub 
Sub sw42_unHit:Controller.Switch(42)=0:End Sub
Sub sw43_Hit:Controller.Switch(43)=1:End Sub 
Sub sw43_unHit:Controller.Switch(43)=0:End Sub
Sub sw57_Hit:Controller.Switch(57)=1:End Sub 
Sub sw57_unHit:Controller.Switch(57)=0:End Sub
Sub sw58_Hit:Controller.Switch(58)=1:End Sub 
Sub sw58_unHit:Controller.Switch(58)=0:End Sub
Sub sw60_Hit:Controller.Switch(60)=1:End Sub 
Sub sw60_unHit:Controller.Switch(60)=0:End Sub
Sub sw61_Hit:Controller.Switch(61)=1:End Sub 
Sub sw61_unHit:Controller.Switch(61)=0:End Sub

 'Stand Up Targets
Sub sw17_Hit:vpmTimer.PulseSw 17:End Sub
Sub sw18_Hit:vpmTimer.PulseSw 18:End Sub
Sub sw19_Hit:vpmTimer.PulseSw 19:End Sub
Sub sw20_Hit:vpmTimer.PulseSw 20:End Sub
Sub sw21_Hit:vpmTimer.PulseSw 21:End Sub
Sub sw22_Hit:vpmTimer.PulseSw 22:End Sub
Sub sw23_Hit:vpmTimer.PulseSw 23:End Sub
Sub sw24_Hit:vpmTimer.PulseSw 24:End Sub
Sub sw27_Hit:vpmTimer.PulseSw 27:End Sub
Sub sw28_Hit:vpmTimer.PulseSw 28:End Sub
Sub sw29_Hit:vpmTimer.PulseSw 29:End Sub
Sub sw30_Hit:vpmTimer.PulseSw 30:End Sub
Sub sw31_Hit:vpmTimer.PulseSw 31:End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:End Sub

'Ramp Triggers
Sub sw25_Hit:vpmTimer.PulseSw 25:End Sub
Sub sw26_Hit:vpmTimer.PulseSw 26:PlaySoundAtBall "Wire Ramp":End Sub
Sub sw52_Hit:vpmTimer.PulseSw 52:End Sub
Sub sw53_Hit:vpmTimer.PulseSw 53:End Sub

'Optical Triggers
Sub sw47_Hit:Controller.Switch(47)=1 : End Sub 
Sub sw47_unHit:Controller.Switch(47)=0:End Sub
Sub sw48_Hit:Controller.Switch(48)=1 : End Sub 
Sub sw48_unHit:Controller.Switch(48)=0:End Sub

'Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 49 : PlaySoundAtVol SoundFX("fx_bumper1",DOFContactors),Bumper1,VolBump: End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 50 : PlaySoundAtVol SoundFX("fx_bumper2",DOFContactors),Bumper2,VolBump: End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 51 : playsoundAtVol SoundFX("fx_bumper3",DOFContactors),Bumper3,VolBump: End Sub

'Generic Sounds
Sub Trigger1_Hit:stopSound "Wire Ramp":PlaySound "fx_ballrampdrop":End Sub
Sub Trigger2_Hit:PlaySound "fx_ballrampdrop":End Sub
Sub Trigger3_Hit:PlaySound "fx_ballrampdrop":End Sub

'***************************************************
'Warrior Bug Animation
'***************************************************

dim BugTargets
BugTargets=Array(BugT1,BugT2,BugT3,BugT4,BugT5,BugT6,BugT7,BugT8,BugT9,BugT10)

Sub UpdateWarriorBug(NewPos, aSpeed, LastPos)
	dim Position, LastPosition
	Position = NewPos * 9 / 190
	LastPosition = LastPos * 9 / 190

	BugTargets(LastPosition).IsDropped=1
'	BugTargets(LastPosition).Visible = False
	BugTargets(Position).IsDropped=0
'	BugTargets(Position).Visible = True

	WarBug.TransY = NewPos-130
End Sub

'Bug Triggers
Sub BugT1_Hit:vpmTimer.PulseSw 35:End Sub
Sub BugT2_Hit:vpmTimer.PulseSw 35:End Sub
Sub BugT3_Hit:vpmTimer.PulseSw 35:End Sub
Sub BugT4_Hit:vpmTimer.PulseSw 35:End Sub
Sub BugT5_Hit:vpmTimer.PulseSw 35:End Sub
Sub BugT6_Hit:vpmTimer.PulseSw 35:End Sub
Sub BugT7_Hit:vpmTimer.PulseSw 35:End Sub
Sub BugT8_Hit:vpmTimer.PulseSw 35:End Sub
Sub BugT9_Hit:vpmTimer.PulseSw 35:End Sub
Sub BugT10_Hit:vpmTimer.PulseSw 35:End Sub

'sw33 Stepper Motor rear
'sw34 Stepper Motor Front
'sw35 Stepper motor Bug

Dim BugLit
BugLit=False

Sub SolWarrior(Enabled)
  If Enabled Then
    BugLit=True
   ' BugWalls(Position).IsDropped=1
    BugTargets(Position).IsDropped=1
    Position=Position+6
   ' BugWalls(Position).IsDropped=0
    BugTargets(Position).IsDropped=0
    WarBug.TransY = -16
'	PlaySoundAtVol "fx_chapa", WarBug, 1
  Else
    BugLit=False
  '  BugWalls(Position).IsDropped=1
    BugTargets(Position).IsDropped=1
    Position=Position-6
  '  BugWalls(Position).IsDropped=0
    BugTargets(Position).IsDropped=0
    WarBug.TransY = +16
  End If
End Sub

'***************************************************
'Brain Bug  animation
'***************************************************

'Brain Bug Trigger
Sub sw38_Hit:vpmTimer.PulseSw 38:End Sub

'sw 37 and sw39 = 1 when toy is up

 Dim BrainE,BrainFrame
 BrainE=False:BrainFrame=0
 
Sub SolBrainBug(Enabled)
	If Enabled Then
		playsound SoundFX("Solenoid",DOFContactors)
		BBUGdir = "up"
		BBUGtimer.enabled = true
		BrainE=True
		sw38.IsDropped=0
		sw38a.IsDropped=0
		Controller.Switch(37)=1
		Controller.Switch(39)=1
	Else
		playsound SoundFX("Solenoid",DOFContactors)
		BBUGdir = "down"
		BBUGtimer.enabled = true
		BrainE=False
		sw38.IsDropped=1
		sw38a.IsDropped=1
		Controller.Switch(37)=0
		Controller.Switch(39)=0
	End If
End Sub

dim bbugNudgePos
bbugNudgePos = 0
dim bbugNudgeAmount 
bbugNudgeAmount = -2

Sub nudgeBrain()
	BBUG.TransY = bbugNudgeAmount
	'BBUGtop.TransY = bbugNudgeAmount
	bbugNudgePos = 1
	brainNudgeTimer.enabled = 1
End Sub

dim BBUGdir
BBUGdir = "down"

Sub BBUGtimer_Timer()	
	select case bbugdir
		case "down":
			if bbug.z > 0 then
				bbug.z = bbug.z-5
				'bbugtop.z = bbug.z-5
			end if
			if bbug.z <= 0 then
				me.enabled = 0
				bbug.z = 0
				'bbugtop.z = -180
			end if
		case "up":
			if bbug.z < 360 then
				bbug.z = bbug.z+5
				'bbugtop.z = bbug.z+5
			end if

			if bbug.z >= 130 then
				me.enabled = 0
				bbug.z = 130
				'bbugtop.z = 2
			end if
	end select
	Debug.Print bbug.z

End Sub

Sub brainNudgeTimer_Timer()
	me.enabled = 0
			BBUG.TransY = -1*bbugNudgeAmount
			'BBUGtop.TransY = -1*bbugNudgeAmount
			bbugNudgePos = 0
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
LampTimer.Interval = 5 'lamp fading speed
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

Sub UpdateLamps()
NFadeL 1, L1
NFadeL 2, L2
NFadeL 3, L3
NFadeL 4, L4
NFadeL 5, L5
NFadeL 6, L6
NFadeL 7, L7
NFadeL 8, L8
NFadeL 9, L9
NFadeL 10, L10
NFadeL 11, L11
NFadeL 12, L12
NFadeL 13, L13
NFadeL 14, L14
NFadeL 15, L15
NFadeL 16, L16
NFadeL 17, L17
NFadeL 18, L18
NFadeL 19, L19
NFadeL 20, L20
NFadeL 21, L21
NFadeL 22, L22
NFadeL 23, L23
NFadeL 24, L24
NFadeL 25, L25
NFadeL 26, L26
NFadeL 27, L27

NFadeObjm 28, L28b, "bulbcover1_redOn", "bulbcover1_red"
NFadeL 28, L28a 'Pop Bumper LED
NFadeObjm 29, L29b, "bulbcover1_redOn", "bulbcover1_red"
NFadeL 29, L29a 'Pop Bumper LED
NFadeObjm 30, L30b, "bulbcover1_redOn", "bulbcover1_red"
NFadeL 30, L30a 'Pop Bumper LED

NFadeL 31, L31
NFadeL 32, L32

FadePrimm 34, WarBugSledSign 'Live Fire Glow
Flashm 34, Flasher34  'Live Fire Flasher
Flash 34, Flasher34b  'Live Fire Flasher

NFadeObjm 35, P35, "bulbcover1_orangeOn", "bulbcover1_orange"
Flash 35, Flasher35 'Nuke LED
NFadeObjm 36, P36, "bulbcover1_redOn", "bulbcover1_red"
Flash 36, Flasher36 'Nuke LED

NFadeL 37, L37
NFadeL 38, L38
NFadeObjm 39, P39, "bulbcover1_blueOn", "bulbcover1_blue"
Flash 39, Flasher39 'Nuke LED
NFadeLm 40, L40a
NFadeL 40, L40b
NFadeL 41, L41
NFadeL 42, L42
NFadeL 43, L43
NFadeL 44, L44
NFadeL 45, L45
NFadeL 46, L46
NFadeObjm 47, P47, "bulbcover1_redOn", "bulbcover1_red"
Flash 47, Flasher47 'L Ramp LED
NFadeObjm 48, P48, "bulbcover1_redOn", "bulbcover1_red"
Flash 48, Flasher48 'R Ramp LED
NFadeL 49, L49
NFadeL 50, L50
NFadeL 51, L51
NFadeL 52, L52
NFadeL 53, L53
NFadeL 54, L54
NFadeL 55, L55
NFadeL 56, L56
NFadeL 57, L57
NFadeL 58, L58
NFadeL 59, L59
NFadeL 60, L60
NFadeL 61, L61
NFadeL 62, L62
NFadeL 63, L63
NFadeL 64, L64
NFadeL 65, L65
NFadeL 66, L66
NFadeL 67, L67
NFadeL 68, L68
NFadeL 69, L69
NFadeL 70, L70
NFadeL 71, L71
NFadeL 72, L72
NFadeL 73, L73
NFadeL 74, L74
NFadeL 75, L75
NFadeL 76, L76
NFadeL 77, L77
NFadeL 78, L78
NFadeL 79, L79
NFadeL 80, L80

'Solenoid Controlled Lamps

NFadeLm 123, S123a
NFadeL 123, S123b

NFadeLm 129, S129a
NFadeLm 129, S129b
NFadeLm 129, S129c
NFadeLm 129, S129d
NFadeLm 129, S129e
NFadeLm 129, S129f
Flash 129, Flasher129

NFadeLm 130, S130a
NFadeLm 130, S130b
NFadeLm 130, S130c
NFadeL 130, S130d

NFadeLm 131, S131a
NFadeLm 131, S131b
NFadeLm 131, S131c
NFadeL 131, S131d

NFadeLm 132, S132a
NFadeL 132, S132b

End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
	if nr > 123 and value then
		PlaySound "Jon_GI_Off"
	end if
End Sub

Sub FadePrimm(nr, Object)
 Object.blenddisablelighting = FlashLevel(nr) * 0.1
end sub 

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
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
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
 
'*****************FLUPPER FLASH****************************

Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5, FlashLevel6, FlasherLevel7, FlasherLevel8, FlasherLevel9, FlasherLevel10

FlasherLight1.IntensityScale = 0
FlasherLight2.IntensityScale = 0
FlasherLight3.IntensityScale = 0
FlasherLight4.IntensityScale = 0

FlasherLight5.IntensityScale = 0
FlasherLight6.IntensityScale = 0
FlasherLight7.IntensityScale = 0
Flasherlight8.IntensityScale = 0

FlasherLight9.IntensityScale = 0
FlasherLight10.IntensityScale = 0
FlasherLight11.IntensityScale = 0
Flasherlight12.IntensityScale = 0

FlasherLight13.IntensityScale = 0
FlasherLight14.IntensityScale = 0
FlasherLight15.IntensityScale = 0
FlasherLight16.IntensityScale = 0

Sub FlupperFlash(nr, FlashObject, LitObject, BaseObject, LightObject)
	FadeEmpty nr
	FlupperFlashm nr, FlashObject, LitObject, BaseObject, LightObject
End Sub

Sub FlupperFlashm(nr, FlashObject, LitObject, BaseObject, LightObject)
	select case nr
		case 1 : Flashlevelb(1) = 1 : FlasherFlash1_Timer
		case 2 : Flashlevelb(2) = 1 : FlasherFlash2_Timer
		case 3 : FlashLevelb(3) = 1 : FlasherFlash3_Timer
		case 4 : FlashLevelb(4) = 1 : FlasherFlash4_Timer
		case 5 : FlashLevelb(5) = 1 : FlasherFlash5_Timer
		case 6 : FlashLevelb(6) = 1 : FlasherFlash6_Timer
		case 7 : FlashLevelb(7) = 1 : FlasherFlash7_Timer
		case 8 : FlashLevelb(8) = 1 : FlasherFlash8_Timer
		case 9 : FlashLevelb(9) = 1 : FlasherFlash9_Timer
		case 10 : FlashLevelb(10) = 1 : FlasherFlash10_Timer
		case 11 : FlashLevelb(11) = 1 : FlasherFlash11_Timer
		case 12 : FlashLevelb(12) = 1 : FlasherFlash12_Timer
		case 13 : FlashLevelb(13) = 1 : FlasherFlash13_Timer
		case 14 : FlashLevelb(14) = 1 : FlasherFlash14_Timer
		case 15 : FlashLevelb(15) = 1 : FlasherFlash15_Timer
		case 16 : FlashLevelb(16) = 1 : FlasherFlash16_Timer
	end Select
End Sub
 
Sub FadeEmpty(nr)	'Fade a lamp number, no object updates
    Select Case FadingLevel(nr)
		Case 3
			FadingLevel(nr) = 0
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 3 'completely off
            End if
            'Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 6 'completely on
            End if
            'Object.IntensityScale = FlashLevel(nr)
		Case 6
			FadingLevel(nr) = 1
    End Select
End Sub

'  ----Fluppers Flashers V2 Script----
' #####################################
' ###### copy script from here on #####
' #####################################
dim TestFlashers, tableref
TestFlashers = 0		' *** set this to 1 to check position of flasher object ***
Set TableRef = Table1   ' *** change this, if your table has another name       ***
Dim FlashLevelb(20), objbase(20), objlit(20), objflasher(20), objlight(20), objactive(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "red" : InitFlasher 2, "yellow" : InitFlasher 3, "green" : InitFlasher 4, "blue"
InitFlasher 5, "red" : InitFlasher 6, "yellow" : InitFlasher 7, "green" : InitFlasher 8, "blue"
InitFlasher 9, "red" : InitFlasher 10, "yellow" : InitFlasher 11, "green" : InitFlasher 12, "blue"
InitFlasher 13, "red" : InitFlasher 14, "yellow" : InitFlasher 15, "green" : InitFlasher 16, "blue"
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 5,180 : RotateFlasher 6,180 : RotateFlasher 7,180 : RotateFlasher 8,180

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objbase(nr).BlendDisableLighting = 0.4
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 15 + 5 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlashLevelb(nr)^2.5
	objlight(nr).IntensityScale = 0.03 * FlashLevelb(nr)^3
	objbase(nr).BlendDisableLighting =  0.4 + 10 * FlashLevelb(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * FlashLevelb(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,FlashLevelb(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	If ObjActive(nr) Then 
		FlashLevelb(nr) = FlashLevelb(nr) * 0.7 - 0.01
	Else
		FlashLevelb(nr) = FlashLevelb(nr) * 0.9 - 0.01
	End If
	If not ObjActive(nr) and FlashLevelb(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0
	If ObjActive(nr) and FlashLevelb(nr) < 0.5 Then FlashLevelb(nr) = 1: End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 
Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub
Sub FlasherFlash8_Timer() : FlashFlasher(8) : End Sub
Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub
Sub FlasherFlash12_Timer() : FlashFlasher(12) : End Sub
Sub FlasherFlash13_Timer() : FlashFlasher(13) : End Sub
Sub FlasherFlash14_Timer() : FlashFlasher(14) : End Sub
Sub FlasherFlash15_Timer() : FlashFlasher(15) : End Sub
Sub FlasherFlash16_Timer() : FlashFlasher(16) : End Sub

' ###################################
' ###### copy script until here #####
' ###################################


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 62
    PlaySound SoundFX("right_slingshot",DOFContactors)', 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 59
    PlaySound SoundFX("left_slingshot",DOFContactors)', 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

'********************************************
'DJRobX's Warrior Bug Stepper Supporting Code
'********************************************

Class cvpmMyMech
	Public Sol1, Sol2, MType, Length, Steps, Acc, Ret
	Private mMechNo, mNextSw, mSw(), mLastPos, mLastSpeed, mCallback

	Private Sub Class_Initialize
		ReDim mSw(10)
		gNextMechNo = gNextMechNo + 1 : mMechNo = gNextMechNo : mNextSw = 0 : mLastPos = 0 : mLastSpeed = 0
		MType = 0 : Length = 0 : Steps = 0 : Acc = 0 : Ret = 0 : vpmTimer.addResetObj Me
	End Sub

	Public Sub AddSw(aSwNo, aStart, aEnd)
		mSw(mNextSw) = Array(aSwNo, aStart, aEnd, 0)
		mNextSw = mNextSw + 1
	End Sub

	Public Sub AddPulseSwNew(aSwNo, aInterval, aStart, aEnd)
		If Controller.Version >= "01200000" Then
			mSw(mNextSw) = Array(aSwNo, aStart, aEnd, aInterval)
		Else
			mSw(mNextSw) = Array(aSwNo, -aInterval, aEnd - aStart + 1, 0)
		End If
		mNextSw = mNextSw + 1
	End Sub

	Public Sub Start
		Dim sw, ii
		With Controller
			.Mech(1) = Sol1 : .Mech(2) = Sol2 : .Mech(3) = Length
			.Mech(4) = Steps : .Mech(5) = MType : .Mech(6) = Acc : .Mech(7) = Ret
			ii = 10
			For Each sw In mSw
				If IsArray(sw) Then
					.Mech(ii) = sw(0) : .Mech(ii+1) = sw(1)
					.Mech(ii+2) = sw(2) : .Mech(ii+3) = sw(3)
					ii = ii + 10
				End If
			Next
			.Mech(0) = mMechNo
		End With
		If IsObject(mCallback) Then mCallBack 0, 0, 0 : mLastPos = 0 : vpmTimer.EnableUpdate Me, True, True  ' <------- All for this.
	End Sub

	Public Property Get Position : Position = Controller.GetMech(mMechNo) : End Property
	Public Property Get Speed    : Speed = Controller.GetMech(-mMechNo)   : End Property
	Public Property Let Callback(aCallBack) : Set mCallback = aCallBack : End Property

	Public Sub Update
		Dim currPos, speed
		currPos = Controller.GetMech(mMechNo)
		speed = Controller.GetMech(-mMechNo)
		If currPos < 0 Or (mLastPos = currPos And mLastSpeed = speed) Then Exit Sub
		mCallBack currPos, speed, mLastPos : mLastPos = currPos : mLastSpeed = speed
	End Sub

	Public Sub Reset : Start : End Sub
	
End Class

'**************************************************************
'	ninuzzu's	FLIPPER SHADOWS (with added primitive flippers)
'**************************************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperRSh1.RotZ = RightFlipper1.currentangle
    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
	MFlogo.RotZ = RightFlipper1.CurrentAngle + 120
End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

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
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'*********************************************************************
'						TABLE AUDIO SECTION
'*********************************************************************

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX, Rothbauerw and Herweh
' PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
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

' set position as table object and Vol + RndPitch manually

Sub PlaySoundAtVolPitch(sound, tableobj, Vol, RndPitch)
  PlaySound sound, 1, Vol, AudioPan(tableobj), RndPitch, 0, 0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallAbsVol(sound, VolMult)
  PlaySound sound, 0, VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

' requires rampbump1 to 7 in Sound Manager

Sub RandomBump(voladj, freq)
  Dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
  PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' set position as bumperX and Vol manually. Allows rapid repetition/overlaying sound

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
  PlaySound sound, 0, ABS(BOT.velz)/17, Pan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub

' play a looping sound at a location with volume
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
  PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function RndNum(min, max)
  RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = ball.x * 2 / table1.width-1
  If tmp > 0 Then
    Pan = Csng(tmp ^10)
  Else
    Pan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
End Function

Function BallRollVol(ball) ' Calculates the Volume of the sound based on the ball speed
  BallRollVol = Csng(BallVel(ball) ^2 / (80000 - (79900 * Log(RollVol) / Log(100))))
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
  BallVelZ = INT((ball.VelZ) * -1 )
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
  VolZ = Csng(BallVelZ(ball) ^2 / 200)*1.2
End Function

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order

Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
  Dim AB, BC, CD, DA
  AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
  BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
  CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
  DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)

  If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
    InRect = True
  Else
    InRect = False
  End If
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
      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
          ' PlaySound("fx_ballrolling" & b), -1, BallRollVol(BOT(b) )*.8, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
          ' PlaySound("fx_ballrolling" & b), -1, BallRollVol(BOT(b) )*.2, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
    If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
      PlaySoundAtBOTBallZ "fx_ball_drop" & b, BOT(b)
    End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'Missing Motor Sound

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

Sub Rollovers_Hit (idx)
	' PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
  RandomSoundRollover()
End Sub

Sub Targets_Hit (idx)
	' PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
  RandomSoundTargetHitStrong()
End Sub

Sub Metals_Thin_Hit (idx)
	' PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  RandomSoundMetal
End Sub

Sub Metals_Medium_Hit (idx)
	' PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  RandomSoundMetal
End Sub

Sub Metals2_Hit (idx)
	' PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  RandomSoundMetal()
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 5 then
    RandomSoundRubberStrong()
  End if
  If finalspeed <= 5 then
    RandomSoundRubberWeak()
  End If
End Sub

Sub Posts_Hit(idx)
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 5 then
    RandomSoundRubberStrong()
  End if
  If finalspeed <= 5 then
    RandomSoundRubberWeak()
  End If
End Sub

Sub RandomSoundRubberStrong()
  Select Case Int(Rnd*10)+1
    Case 1 : PlaySoundAtVol "TOM_Rubber_Flipper_Slingshot_Strong_1", ActiveBall, VolRH
    Case 2 : PlaySoundAtVol "TOM_Rubber_Flipper_Slingshot_Strong_2", ActiveBall, VolRH
    Case 3 : PlaySoundAtVol "TOM_Rubber_Flipper_Slingshot_Strong_3", ActiveBall, VolRH
    Case 4 : PlaySoundAtVol "TOM_Rubber_Flipper_Slingshot_Strong_4", ActiveBall, VolRH
    Case 5 : PlaySoundAtVol "TOM_Rubber_Flipper_Slingshot_Strong_5", ActiveBall, VolRH
    Case 6 : PlaySoundAtVol "TOM_Rubber_Flipper_Slingshot_Strong_6", ActiveBall, VolRH
    Case 7 : PlaySoundAtVol "TOM_Rubber_Flipper_Slingshot_Strong_7", ActiveBall, VolRH
    Case 8 : PlaySoundAtVol "TOM_Rubber_Flipper_Slingshot_Strong_8", ActiveBall, VolRH
    Case 9 : PlaySoundAtVol "TOM_Rubber_Flipper_Slingshot_Strong_9", ActiveBall, VolRH
    Case 10 : PlaySoundAtVol "TOM_Rubber_1_Hard", ActiveBall, VolRH
  End Select
End Sub

Sub RandomSoundRubberWeak()
  Select Case Int(Rnd*9)+1
    Case 1 : PlaySoundAtVol "TOM_Rubber_1", ActiveBall, VolRH * 0.5
    Case 2 : PlaySoundAtVol "TOM_Rubber_2", ActiveBall, VolRH * 0.5
    Case 3 : PlaySoundAtVol "TOM_Rubber_3", ActiveBall, VolRH * 0.5
    Case 4 : PlaySoundAtVol "TOM_Rubber_3", ActiveBall, VolRH * 0.5
    Case 5 : PlaySoundAtVol "TOM_Rubber_5", ActiveBall, VolRH * 0.5
    Case 6 : PlaySoundAtVol "TOM_Rubber_6", ActiveBall, VolRH * 0.5
    Case 7 : PlaySoundAtVol "TOM_Rubber_7", ActiveBall, VolRH * 0.5
    Case 8 : PlaySoundAtVol "TOM_Rubber_8", ActiveBall, VolRH * 0.5
    Case 9 : PlaySoundAtVol "TOM_Rubber_9", ActiveBall, VolRH * 0.5
  End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	'RandomSoundFlipper()
  RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	'RandomSoundFlipper()
  RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub RandomSoundRubberFlipper(parm)
  Select Case Int(Rnd*7)+1
    Case 1 : PlaySoundAtBallVol "TOM_Rubber_Flipper_Normal_1", parm, VolFlipH
    Case 2 : PlaySoundAtBallVol "TOM_Rubber_Flipper_Normal_2", parm, VolFlipH
    Case 3 : PlaySoundAtBallVol "TOM_Rubber_Flipper_Normal_3", parm, VolFlipH
    Case 4 : PlaySoundAtBallVol "TOM_Rubber_Flipper_Normal_4", parm, VolFlipH
    Case 5 : PlaySoundAtBallVol "TOM_Rubber_Flipper_Normal_5", parm, VolFlipH
    Case 6 : PlaySoundAtBallVol "TOM_Rubber_Flipper_Normal_6", parm, VolFlipH
    Case 7 : PlaySoundAtBallVol "TOM_Rubber_Flipper_Normal_7", parm, VolFlipH
  End Select
End Sub

Dim NextOrbitHit:NextOrbitHit = 0
Sub RandomSoundMetal()
 if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
  Select Case Int(Rnd*13)+1
    Case 1 : PlaySoundAtVol "TOM_Metal_Touch_1", ActiveBall, VolMetal
    Case 2 : PlaySoundAtVol "TOM_Metal_Touch_2", ActiveBall, VolMetal
    Case 3 : PlaySoundAtVol "TOM_Metal_Touch_3", ActiveBall, VolMetal
    Case 4 : PlaySoundAtVol "TOM_Metal_Touch_4", ActiveBall, VolMetal
    Case 5 : PlaySoundAtVol "TOM_Metal_Touch_5", ActiveBall, VolMetal
    Case 6 : PlaySoundAtVol "TOM_Metal_Touch_6", ActiveBall, VolMetal
    Case 7 : PlaySoundAtVol "TOM_Metal_Touch_7", ActiveBall, VolMetal
    Case 8 : PlaySoundAtVol "TOM_Metal_Touch_8", ActiveBall, VolMetal
    Case 9 : PlaySoundAtVol "TOM_Metal_Touch_9", ActiveBall, VolMetal
    Case 10 : PlaySoundAtVol "TOM_Metal_Touch_10", ActiveBall, VolMetal
    Case 11 : PlaySoundAtVol "TOM_Metal_Touch_11", ActiveBall, VolMetal
    Case 12 : PlaySoundAtVol "TOM_Metal_Touch_12", ActiveBall, VolMetal
    Case 13 : PlaySoundAtVol "TOM_Metal_Touch_13", ActiveBall, VolMetal
  End Select
  NextOrbitHit = Timer + .1 + (Rnd * .2)
 end if
End Sub

Sub RandomSoundRollover()
  Select Case Int(Rnd*4)+1
    Case 1 : PlaySoundAtVol "TOM_Rollover_1", ActiveBall, VolRol
    Case 2 : PlaySoundAtVol "TOM_Rollover_2", ActiveBall, VolRol
    Case 3 : PlaySoundAtVol "TOM_Rollover_3", ActiveBall, VolRol
    Case 4 : PlaySoundAtVol "TOM_Rollover_4", ActiveBall, VolRol
  End Select
End Sub

Sub RandomSoundTargetHitStrong()
  Select Case Int(Rnd*4)+1
    Case 1 : PlaySoundAtVol SoundFX("TOM_Target_Hit_5",DOFTargets), ActiveBall, VolTarg
    Case 2 : PlaySoundAtVol SoundFX("TOM_Target_Hit_6",DOFTargets), ActiveBall, VolTarg
    Case 3 : PlaySoundAtVol SoundFX("TOM_Target_Hit_7",DOFTargets), ActiveBall, VolTarg
    Case 4 : PlaySoundAtVol SoundFX("TOM_Target_Hit_8",DOFTargets), ActiveBall, VolTarg
  End Select
End Sub

Sub RandomSoundTargetHitWeak()
  Select Case Int(Rnd*4)+1
    Case 1 : PlaySoundAtVol SoundFX("TOM_Target_Hit_1",DOFTargets), ActiveBall, VolTarg
    Case 2 : PlaySoundAtVol SoundFX("TOM_Target_Hit_2",DOFTargets), ActiveBall, VolTarg
    Case 3 : PlaySoundAtVol SoundFX("TOM_Target_Hit_3",DOFTargets), ActiveBall, VolTarg
    Case 4 : PlaySoundAtVol SoundFX("TOM_Target_Hit_4",DOFTargets), ActiveBall, VolTarg
  End Select
End Sub

Sub RandomSoundDrain()
  Select Case Int(Rnd*11)+1
    Case 1 : PlaySoundAtVol ("TOM_Drain_1"), Drain, VolDrain
    Case 2 : PlaySoundAtVol ("TOM_Drain_2"), Drain, VolDrain
    Case 3 : PlaySoundAtVol ("TOM_Drain_3"), Drain, VolDrain
    Case 4 : PlaySoundAtVol ("TOM_Drain_4"), Drain, VolDrain
    Case 5 : PlaySoundAtVol ("TOM_Drain_5"), Drain, VolDrain
    Case 6 : PlaySoundAtVol ("TOM_Drain_6"), Drain, VolDrain
    Case 7 : PlaySoundAtVol ("TOM_Drain_7"), Drain, VolDrain
    Case 8 : PlaySoundAtVol ("TOM_Drain_8"), Drain, VolDrain
    Case 9 : PlaySoundAtVol ("TOM_Drain_9"), Drain, VolDrain
    Case 10 : PlaySoundAtVol ("TOM_Drain_10"), Drain, VolDrain
    Case 11 : PlaySoundAtVol ("TOM_Drain_10"), Drain, VolDrain
  End Select
End Sub

