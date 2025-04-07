
'	 __   __        __   ___       ___      ___ 
'	/ _` /  \ |    |  \ |__  |\ | |__  \ / |__  
'	\__> \__/ |___ |__/ |___ | \| |___  |  |___ 
'
'
' Based on the VPX table by JPJ, Team PP and many more (Pingod, UncleReamus, Destruk, The Trout, 32assassin, JPJ, Dark, Neo, Arngrim, Flupper, Rothbauerw, Chucky, Fleep, Knorr, Jpn Osborne, JPSalas, Neofr45, Chucky87, Rajo Joey, Thalamus, Davidcd13, Toxie)
'
' VPW GoldenEye Team:
' -------------------
'	Project lead: Astronasty
'	Playfield cleanup: Astronasty
'	Physics: Fluffhead35, Apophis
'	Sound effects: Fluffhead35
'	3D Inserts: EBisLit, Oqqsan
'	Dynamic Shadows: Wylte
'	VR Room and Backglass: Leojreimroc, Sixtoe
'	Lighting: Hauntfreaks, Sixtoe
'	GI control update: Iaakki
'	Radar dish updates: Apophis, Oqqsan
'	Flasher dome updates: Apophis, Leojreimroc
'	Script fixes: Oqqsan,Apophis
'	Desktop background: Oqqsan
'	Ramp updates: Tomate
'	Testing: VPW team, 1 day by Rik
'
'
' CHANGE LOG
' 12 - fluffhead35 	- Addeed nfozzy physics, fleep sound, rubberizer, target bouncer, coil rampup
' 13 - fluffhead35 	- Adjusted physics, table slope, ramp material, and adjustments for right ramp
' 14 - fluffehad35 	- Adjusted physics on gates
' 15 - Wylte		- Shadowzzzzzz
' 16 - apophis		- Installed Lampz in prep for 3D inserts. Updated PF, plastics, and insert overlay images. Made all rubber objects non-collidable, Added prototype Enhanced Sounds option (no sounds installed yet). Auto-formatted script.
' 17 - EBisLit		- Added 3D inserts
' 18 - EBisLit		- tweaking insert brightness
' 20 - HauntFreaks	- GI lighting and shadows - small tweaks to PF and plastics
' 21 - oqqsan		- RadarA   removed normalmap + changed material to 3dapron, might need some adjustment
' 22c - fluffhead35 - Updated the physics and ballroll sounds.  Realigned rubbers, posts, and pegs.
' 23 - oqqsan 		- small fixes and, added extra vpxlight on all inserts
' 24 - oqqsan		- fixed ball in radar !
' 25 - Leojreimroc  - Adjusted tabled flashers.  Added fade timers to all flashers.  Many solenoids were pointing to the wrong flashers, fixed.
' 26 - oqqsan 		- fixed on some lights,spotlight flasher, swapped out metal bracket, shrinked flippers to lotr size, added volumedial for some mechsounds 
' 27 - Leojreimroc 	- VR Backglass added.  VR Room Code, slight flasher adjustments.
' 28 - fluffhead35  - Added Materials to all Collidables.  Added Hit Events to metal collidables.
' 29 - Sixtoe		- Fixed plunger lane, fixed ramps, redid gi a bit, hooked up ramps to gi, hooked up ramp lights, despaired, drank.
' 30 - apophis		- Flupperized the flasher domes. Updated dynamic shadows. Updated relay sound effects and added radar motor sound effect. Some code cleanup. 
' 31 - oqqsan 		- hooked up the vrbg flashers to flupperdomes. fixed the wall above DT's one more time ( need testing so it dont just drain :P )
' 32 - oqqsan		- made plunger shoot much harder 60? instead of 45 strength. adjusted flippers, from 70 to max 68degrees. metalic on rampmaterials removed
' 33 - oqqsan		- silly mistakes hunting ! :P
' 35 - oqqsan		- fixed silly timers... DOUBLE FPS after :)   added dof calls for flippers
' 36 - oqqsan		- New super Desktop backscreen made just before release time
' 36_gi - iaakki 	- GI control rework and ramp difference textures added and tied to fading
' 37 - oqqsan		- Not used
' 38 - apophis		- Rebuilt the radar dish mechanic. Fixed a few plunger lane issues. Made LampTimer fixed 16ms interval. Increased GI fade speed a bit.
' 39 - oqqsan		- Updated ballsave kick angle
' 40b - apophis		- Added slingshot correction code. Fixed satellite lock so that ball can be unlocked by ball-ball collisions. Increased insert DL a little.
' 41 - tomate       - Added new wireRamps and plasticRamps textures (OFF plasticRamps prims still has old texture)
' 42 - apophis		- Tuned wire ramp DL (removed wire ramps from DL collection). Updated ramp off primitives, texture, and material. Added ramp off primitives to OFF_Prims collection. Fixed right flipper trigger. Increased playfield and ramp frictions a little.
' v1.0 - apophis	- Release
'

Option Explicit
Randomize


'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8
Const MroVol = 9					'Wire Ramps Rolling
Const ProVol = 0.7					'Plastic Ramps Rolling
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1

'/////////////////////----- VR Room -----/////////////////////
Const VRRoom = 0					'0 = VR Off, 1 = Minimal Room

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'*********************************************************************************************************
'***                                    to show dmd in desktop Mod                                     ***
Dim UseVPMDMD, DesktopMode
DesktopMode = Goldeneye.ShowDT  
'If NOT DesktopMode Then 
'	UseVPMDMD = False		'hides the internal VPMDMD when using the color ROM or when table is in Full Screen and color ROM is not in use
'	SidesMetal.visible = False'put True or False if you want to see or not the wood side in fullscreen mod
'	LockDown.visible = False
'end if 
If DesktopMode Then UseVPMDMD = True							'shows the internal VPMDMD when in desktop mode 
'*********************************************************************************************************


Const BallSize = 50
Const BallMass = 1

LoadVPM "01200100","SEGA2.VBS",3.1

Const cGameName="gldneye",UseLamps=0,UseGI=0 ',UseSolenoids=2 already in vbs file
Const SSolenoidOn="solon",SSolenoidOff="soloff",sCoin="coin3" ',SFlipperOn="FlipperUp",SFlipperOff="FlipperDown" 
   '
Dim tablewidth: tablewidth = goldeneye.width
Dim tableheight: tableheight = goldeneye.height
Dim gilvl : gilvl = 0




'**********************************************************
'*               if you don't have SSF                    *
'*      Put 1 to up the magnet Volumes else put 0         *
'**********************************************************
dim MagnetVolMax, MagnetVol
MagnetVolMax = 0 ' 0 (SSF) or 1 (without SSF)
MagnetVol = 0.5 'level of magnet sounds for non SSF users
'**********************************************************

'********************************************************
'*  LUT Options - Script Taken from The Flintstones     *
'* All lut examples taken from The Flintstones and TOTAN*
'*           Big Thanks for all the samples             *
'********************************************************
Dim LUTmeUP:'LUTMeUp = 1 '0 = No LUT Initialized in the memory procedure
Dim DisableLUTSelector:DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1
Const MaxLut = 1
'********************************************************

'********************************************************
'*                 Fantasy colored magnet               *
'********************************************************
Dim FantasyMagnet
FantasyMagnet = 1 '1 For Fantasy colored magnet, 0 for nothing
'********************************************************


Dim preload
Preload = 0	'disabling this while testing texture swaps

'************************************
'*****             Init			*****
'************************************
NoUpperLeftFlipper
NoUpperRightFlipper


Dim bsTrough,bsScoop,bsTank,bsPlunger,bsLockOut,mFlipperMagnet,RadarMagnet
Dim FL, FR, GO
FL = 0:FR = 0:GO = 1


'*************** LUT Memory by JPJ ****************
dim FileObj, File, LUTFile, Txt, TxtTF 'Dim for LUT Memory Outside Init's SUB
Set FileObj = CreateObject("Scripting.FileSystemObject")
If Not FileObj.FileExists(UserDirectory & "GoldeneyeLUT.txt") then
	LutMeUp = 1:WriteLUT
End if
If FileObj.FileExists(UserDirectory & "GoldeneyeLUT.txt") then
	Set LUTFile=FileObj.GetFile(UserDirectory & "GoldeneyeLUT.txt")
	Set Txt=LUTFile.OpenAsTextStream(1,0) 'Number taken from the file
	TxtTF = cint(txt.readline)
	LUTMeUp = TxtTF
	if LutMeUp >8 or LutMeUp <0 or LutMeUp = Null Then 
		LutMeUp = 1
	End if
End if
'****************************************************


Sub GoldenEye_Init
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1
 	SatTop.IsDropped=1
 	SatTop1.IsDropped=1
 	SatTop2.IsDropped=1
 	On Error Resume Next
	vpmInit Me
 	With Controller
 		.GameName=cGameName
 		.Games(cGameName).Settings.Value("rol")=0
		.Games(cGameName).Settings.Value("sound") = 1
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine="007 Goldeneye - Sega, 1996"&vbNewLine&"Initial Table Releace by Pingod & Kid Charlemagne"&vbNewLine&"Mod by UncleReamus & Destruk"&vbNewLine&"Further Modding by The Trout"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden=DesktopMode
		.Run
	End With

	

	If Err Then MsgBox Err.Description
	On Error Goto 0


	Gate3.collidable = 0

	gilvl = 1
	lightdir = -0.1:Textlight.Enabled = 1
	'For each xx in GI:xx.State = 0: Next
	If VRRoom > 0 Then
		For each xx in VRBGGI:xx.visible = 0: Next
	End If
	Sound_GI_Relay 1,BottomTurboBumper
	DOF 103, DOFOff
	If B2SOn Then
		Controller.B2SSetData 90, 0	
	End If



	'********** Nudge **************
	vpmNudge.TiltSwitch=1
	vpmNudge.Sensitivity=3
	vpmNudge.TiltObj=Array(LeftFlipper,RightFlipper,LeftSlingshot,RightSlingshot,LeftTurboBumper,RightTurboBumper,BottomTurboBumper)
	'*******************************

	Set mFlipperMagnet = New cvpmMagnet
	With mFlipperMagnet
		.InitMagnet FlipperMagnet, 25 '20 '10
		.GrabCenter = True
		.CreateEvents "mFlipperMagnet"
	End With

	Set bsTrough=New cvpmBallStack
	bsTrough.InitSw 0,14,13,12,11,10,0,0
	bsTrough.InitKick BallRelease,40,8
	'bsTrough.InitExitSnd SoundFX("BallRelease1",DOFContactors),SoundFX("Solon",DOFContactors)
	bsTrough.Balls=5

	Set bsScoop=New cvpmBallStack
	bsScoop.InitSw 0,50,0,0,0,0,0,0
	bsScoop.InitKick Scoop,213,10
	bsScoop.InitExitSnd SoundFX("popper",DOFContactors),SoundFX("Solon",DOFContactors)
	bsScoop.KickForceVar=7

	Set bsTank=New cvpmBallStack
	bsTank.InitSw 0,56,0,0,0,0,0,0
	bsTank.InitKick TankKickBig,0,65
	bsTank.InitExitSnd SoundFX("popper",DOFContactors),SoundFX("Solon",DOFContactors)
	bsTank.KickBalls=2
	bsTank.KickForceVar=5

	Set bsPlunger=New cvpmBallStack
	bsPlunger.InitSaucer Plunger,16,0,60
	bsPlunger.InitExitSnd SoundFX("Solon",DOFContactors),SoundFX("SolOn",DOFContactors)
	bsPlunger.KickForceVar=7

	Controller.Switch(63)=0
	Controller.Switch(64)=0

	for each xx in DL:
		xx.blendDisableLighting = 0
		playfieldOff.opacity = 100
	Next

	playfieldOff.opacity = 100
'	SidesBack.image = "Backtest"
	Plastics.blenddisablelighting = 0
	BumperA.blenddisableLighting = 0.2
	BumperA001.blenddisableLighting = 0.2
	BumperA002.blenddisableLighting = 0.2
	Rampe3.blenddisableLighting = 0.5
	Rampe2.blenddisableLighting = 0.3
	Rampe1.blenddisableLighting = 0.1
	VisA.blenddisablelighting = 0
	VisB.blenddisablelighting = 0
	MurMetal.blenddisablelighting = 0
	MetalParts.blenddisablelighting = 0
	spotlight.blenddisablelighting = 0.5
'	spotlightLight.blenddisablelighting = 0 ' 0.5
	PegsBoverSlings.blenddisablelighting = 0
'	Rampe3.image = "rampe3GIOFF"
'	Rampe2.image = "rampe2GIOFF"
'	Rampe1.image = "rampe1GIOFF"
	Apron.blenddisablelighting = 0
	ApronCachePlunger.blenddisablelighting = 0
	MrampB.blenddisablelighting = 0

	Plastics.image = "plasticsOff"
	Ledrouge.Amount = 100:LedRouge.IntensityScale = 0
	LedBleu1.Amount = 100:LedBleu1.IntensityScale = 0
	LedBleu2.Amount = 100:LedBleu2.IntensityScale = 0
	LedFond.Amount = 100:LedFond.IntensityScale = 0


	SetLUT

End Sub

'*************************************************


SolCallback(1)="bsTrough.SolOut"
SolCallback(2)="bsPlunger.SolOut"
SolCallback(4)="bsScoop.SolOut"
SolCallback(8)="vpmSolSound SoundFX(""knocker_1"",DOFKnocker),"
'SolCallback(9)="vpmSolSound ""fx_bumper1"","
'SolCallback(10)="vpmSolSound ""fx_bumper2"","
'SolCallback(11)="vpmSolSound ""fx_bumper3"","
'SolCallback(12)="vpmSolSound ""sling1"","
'SolCallback(13)="vpmSolSound ""sling2"","
SolCallback(14)="bsTank.SolOut"

SolCallback(17)="SolLockOut"
SolCallback(18)="SolUpDownRamp"
SolCallback(20)="SolSatLaunchRamp"
SolCallback(21)="SolSatMotorRelay"
SolCallback(22)="DropRamp1.Enabled="

SolCallback(25)="Sol25"	
SolCallback(26)="Sol26"
SolCallback(27)="Sol27"
SolCallback(28)="Sol28"
SolCallback(29)="Sol29"
SolCallback(30)="sol30"
SolCallback(31)="Sol31"
SolCallback(32)="Sol32"
SolCallback(33)="SolRadarMagnet"
SolCallback(34)="SolFlipperMagnet"

SolCallback(45)="TiltMod"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"


'*********************** Targets **********************
Sub SS25_hit():SS25c.rotx = 7:SS25c.roty = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 25:End Sub
Sub SS25_Timer():SS25c.rotx = 0:SS25c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS26_hit():SS26c.rotx = 7:SS26c.roty = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 26:End Sub
Sub SS26_Timer():SS26c.rotx = 0:SS26c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS27_hit():SS27c.rotx = 7:SS27c.roty = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 27:End Sub
Sub SS27_Timer():SS27c.rotx = 0:SS27c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS28_hit():SS28c.rotx = 7:SS28c.roty = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 28:End Sub
Sub SS28_Timer():SS28c.rotx = 0:SS28c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS30_hit():SS30c.rotx = 7:SS30c.roty = -2.5:Me.TimerEnabled = 1:vpmTimer.PulseSw 30:End Sub
Sub SS30_Timer():SS30c.rotx = 0:SS30c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS31_hit():SS31c.rotx = 8:SS31c.roty = -1:Me.TimerEnabled = 1:vpmTimer.PulseSw 31:End Sub
Sub SS31_Timer():SS31c.rotx = 0:SS31c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS33_hit():SS33c.rotx = 3:SS33c.roty = 7:Me.TimerEnabled = 1:vpmTimer.PulseSw 33:End Sub
Sub SS33_Timer():SS33c.rotx = 0:SS33c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS34_hit():SS34c.rotx = 3:SS34c.roty = 7:Me.TimerEnabled = 1:vpmTimer.PulseSw 34:End Sub
Sub SS34_Timer():SS34c.rotx = 0:SS34c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS39_hit():SS39c.rotx = 7:SS39c.roty = 0:Me.TimerEnabled = 1:vpmTimer.PulseSw 39:End Sub
Sub SS39_Timer():SS39c.rotx = 0:SS39c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS44_hit():SS44c.rotx = 6:SS44c.roty = 6:Me.TimerEnabled = 1:vpmTimer.PulseSw 44:End Sub
Sub SS44_Timer():SS44c.rotx = 0:SS44c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS45_hit():SS45c.rotx = 6:SS45c.roty = 6:Me.TimerEnabled = 1:vpmTimer.PulseSw 45:End Sub
Sub SS45_Timer():SS45c.rotx = 0:SS45c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS46_hit():SS46c.rotx = 6:SS46c.roty = 6:Me.TimerEnabled = 1:vpmTimer.PulseSw 46:End Sub
Sub SS46_Timer():SS46c.rotx = 0:SS46c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS47_hit():SS47c.rotx = 6:SS47c.roty = 6:Me.TimerEnabled = 1:vpmTimer.PulseSw 47:End Sub
Sub SS47_Timer():SS47c.rotx = 0:SS47c.roty = 0:Me.TimerEnabled = 0:End Sub
Sub SS48_hit():SS48c.rotx = 6:SS48c.roty = 6:Me.TimerEnabled = 1:vpmTimer.PulseSw 48:End Sub
Sub SS48_Timer():SS48c.rotx = 0:SS48c.roty = 0:Me.TimerEnabled = 0:End Sub
'******************************************************


'****************** Flippers Magnet **********************

Dim Dir, speed, ball, BallsOnMagnet, Mdof
mdof = 0

' Magnet power is pulsed so wait before turning power off
Sub SolFlipperMagnet(enabled)
	HideFlipper.TimerEnabled = Not enabled
	If enabled Then
		FM = 1
		Light001.state = 1
		mFlipperMagnet.MagnetOn = True
		if MagnetVolMax = 1 then
			PlaySound "fx_magnet",-1, MagnetVol
		else
			PlaySound "fx_magnet",-1
		end if
	Else
		FM = 0
		Light001.state = 0
		StopSound "fx_magnet"
	End If
End Sub

' Magnet is turned off Ball is ejected
Sub HideFlipper_Timer
	Dim dir, speed, ball
	For Each ball In mFlipperMagnet.Balls
		With ball
			dir = 180:speed = 45 + Rnd * 2
			.VelX = -6 : .VelY = speed * Cos(dir) '4 * Sin(dir)
			PlaySound "fx_balldrop",1,VolumeDial
		End With
	Next
	vpmTimer.PulseSw 24:
	mFlipperMagnet.MagnetOn = False:	mFlipperMagnet.GrabCenter = True:mdof = 0
	Me.TimerEnabled = False
End Sub
'******************************************************

'******************************************************

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire 'LeftFlipper.RotateToEnd
		Controller.Switch(63)=1
		Dof 101,1
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If	
	Else
		Controller.Switch(63)=0
		Dof 101,0
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'RightFlipper.RotateToEnd
		Controller.Switch(64)=1
		Dof 102,1
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		Controller.Switch(64)=0
		Dof 102,0
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub TiltMod(Enabled)
	If true then 
		go = 0
	Else
		go = 1
	end if
End Sub



Sub sw16_Hit:Controller.Switch(16) = 1  : 
	if DesktopMode and vrroom = 0 then
		L007_1.state=2
		L007_2.state=2
		L007_3.state=2
		Timer001.enabled=True
	End If	
End Sub
Sub Timer001_Timer 
	Timer001.enabled=False
	L007_1.state=0
	L007_2.state=0
	L007_3.state=0
End Sub
Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub
Sub sw49_Hit:Controller.Switch(49) = 1 : End Sub 
Sub sw49_UnHit:Controller.Switch(49) = 0:End Sub

Sub sw51_Hit:Controller.Switch(51) = 1 : End Sub 
Sub sw51_UnHit:Controller.Switch(51) = 0:End Sub
Sub sw52_Hit:Controller.Switch(52) = 1 : End Sub 
Sub sw52_UnHit:Controller.Switch(52) = 0:End Sub
Sub sw53_Hit:Controller.Switch(53) = 1 : End Sub 
Sub sw53_UnHit:Controller.Switch(53) = 0:End Sub


Sub sw55_Hit:Controller.Switch(55) = 1 : End Sub 
Sub sw55_UnHit:Controller.Switch(55) = 0:End Sub
Sub sw57_Hit:Controller.Switch(57) = 1 : End Sub 
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
Sub sw58_Hit:Controller.Switch(58) = 1 : End Sub 
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
Sub sw59_Hit:Controller.Switch(59) = 1 : End Sub 
Sub sw59_UnHit:Controller.Switch(59) = 0:End Sub
Sub sw60_Hit:Controller.Switch(60) = 1: End Sub 
Sub sw60_UnHit:Controller.Switch(60) = 0:End Sub

sub GateSw17_Hit:vpmTimer.PulseSw 17: End Sub
sub GateSw18_Hit:vpmTimer.PulseSw 18: End Sub
sub GateSw19_Hit:vpmTimer.PulseSw 19: End Sub
sub GateSw32_Hit:vpmTimer.PulseSw 32: End Sub
sub GateSw40_Hit:vpmTimer.PulseSw 40: End Sub
sub GateSw54_Hit:vpmTimer.PulseSw 54: End Sub



dim sol25lvl, sol26lvl, sol27lvl, sol28lvl, sol29lvl, sol30lvl, sol31lvl, sol32lvl

sub Sol25(enabled)
	If Enabled = True Then
		sol25lvl = 1
		sol25timer_timer
	End If
end sub

flash001.state = 1
flash001.intensityscale = 0
Flash002.state = 1
Flash002.intensityscale = 0

sub sol25timer_timer                            
    if Not sol25timer.enabled then
        If VRRoom > 0 Then
            VRBGFL25_1.visible = true
            VRBGFL25_2.visible = true
            VRBGFL25_3.visible = true
            VRBGFL25_4.visible = true
        End If
        flasha001.visible = true
        flasha001B.visible = true
        sol25timer.enabled = true
    end if
	If VRRoom > 0 Then
		VRBGFL25_1.opacity = 400 * sol25lvl^1.5
		VRBGFL25_2.opacity = 400 * sol25lvl^1.5
		VRBGFL25_3.opacity = 400 * sol25lvl^1.5
		VRBGFL25_4.opacity = 400 * sol25lvl^2
	End If
    Flasha001.opacity = 175 * sol25lvl^2
    Flasha001B.opacity = 175 * sol25lvl^2
    Flash001.IntensityScale = 2 * sol25lvl
    Flash002.IntensityScale = 2 * sol25lvl
    sol25lvl = 0.85 * sol25lvl - 0.01
    if sol25lvl < 0 then sol25lvl = 0
    if sol25lvl =< 0 Then
        If VRRoom > 0 Then
			VRBGFL25_1.visible = false
			VRBGFL25_2.visible = false
			VRBGFL25_3.visible = false
			VRBGFL25_4.visible = false
		End If
		flasha001.visible = False
		flasha001B.visible = False
		sol25timer.enabled = false
    end if

end sub

sub Sol26(enabled)
	If Enabled = True Then
		sol26lvl = 1
		sol26timer_timer
	End If
end sub

sub sol26timer_timer                            
    if Not sol26timer.enabled then
		If VRRoom > 0 Then
            VRBGFL26_1.visible = true
            VRBGFL26_2.visible = true
            VRBGFL26_3.visible = true
            VRBGFL26_4.visible = true
            VRBGFL26_5.visible = true
            VRBGFL26_6.visible = true
            VRBGFL26_7.visible = true
            VRBGFL26_8.visible = true
		End If
		flasha003.visible = true
        sol26timer.enabled = true
    end if
    If VRRoom > 0 Then
		VRBGFL26_1.opacity = 400 * sol26lvl^1.5
		VRBGFL26_2.opacity = 400 * sol26lvl^1.5
		VRBGFL26_3.opacity = 400 * sol26lvl^1.5
		VRBGFL26_4.opacity = 400 * sol26lvl^2
		VRBGFL26_5.opacity = 400 * sol26lvl^1.5
		VRBGFL26_6.opacity = 400 * sol26lvl^1.5
		VRBGFL26_7.opacity = 400 * sol26lvl^1.5
		VRBGFL26_8.opacity = 400 * sol26lvl^2
	End If
    Flasha003.opacity = 1800 * sol26lvl * 2
    LL15.IntensityScale = 1 * sol26lvl^2
    LL15F.IntensityScale = 1 * sol26lvl^2
    LLb015.IntensityScale = 0.75 * Sol26lvl^3
    p15f.blenddisablelighting = 1.5 * sol26lvl^2 + 0.1
    sol26lvl = 0.8 * sol26lvl - 0.01
    if sol26lvl < 0 then sol26lvl = 0
    if sol26lvl =< 0 Then
        flasha003.visible = false
		If VRRoom > 0 Then
			VRBGFL26_1.visible = false
			VRBGFL26_2.visible = false
			VRBGFL26_3.visible = false
			VRBGFL26_4.visible = false
			VRBGFL26_5.visible = false
			VRBGFL26_6.visible = false
			VRBGFL26_7.visible = false
			VRBGFL26_8.visible = false
		End If
        sol26timer.enabled = false
    end if
end sub

sub Sol27(enabled)
	If Enabled = True Then
		sol27lvl = 1
		sol27timer_timer
	End If
end sub

Flash003A.state = 1
Flash003A.intensityscale = 0
Flash003B.state = 1
Flash003B.intensityscale = 0

sub sol27timer_timer                            
    if Not sol27timer.enabled then
        If VRRoom > 0 Then
            VRBGFL27_1.visible = true
            VRBGFL27_2.visible = true
            VRBGFL27_3.visible = true
            VRBGFL27_4.visible = true
        End If
        flasha002.visible = true
        sol27timer.enabled = true
    end if
    If VRRoom > 0 Then
		VRBGFL27_1.opacity = 400 * sol27lvl^1.5
		VRBGFL27_2.opacity = 400 * sol27lvl^1.5
		VRBGFL27_3.opacity = 400 * sol27lvl^1.5
		VRBGFL27_4.opacity = 400 * sol27lvl^2
	End If
    flasha002.opacity = 70 * sol27lvl * 2
    Flash003A.IntensityScale = 1 * sol27lvl
    Flash003B.IntensityScale = 1 * sol27lvl
    sol27lvl = 0.8 * sol27lvl - 0.01
    if sol27lvl < 0 then sol27lvl = 0
    if sol27lvl =< 0 Then
        If VRRoom > 0 Then
			VRBGFL27_1.visible = false
			VRBGFL27_2.visible = false
			VRBGFL27_3.visible = false
			VRBGFL27_4.visible = false
		End If
        flasha002.visible = false
        sol27timer.enabled = false
    end if
end sub

sub Sol28(enabled)
	If enabled = true then
		sol28lvl = 1
		sol28timer_timer
	End If
end sub

Flash004.state = 1
Flash004.intensityscale = 0

sub sol28timer_timer                            
    if Not sol28timer.enabled then
        If VRRoom > 0 Then
            VRBGFL28_1.visible = true
            VRBGFL28_2.visible = true
            VRBGFL28_3.visible = true
            VRBGFL28_4.visible = true
        End If
        flasha0004.visible = true
        sol28timer.enabled = true
    end if
	If VRRoom > 0 Then
		VRBGFL28_1.opacity = 400 * sol28lvl^1.5
		VRBGFL28_2.opacity = 400 * sol28lvl^1.5
		VRBGFL28_3.opacity = 400 * sol28lvl^1.5
		VRBGFL28_4.opacity = 400 * sol28lvl^2
	End If
    flasha0004.opacity = 15 * sol28lvl * 2
    Flash004.IntensityScale = 6 * sol28lvl
    sol28lvl = 0.8 * sol28lvl - 0.01
    if sol28lvl < 0 then sol28lvl = 0
    if sol28lvl =< 0 Then
        If VRRoom > 0 Then
			VRBGFL28_1.visible = false
			VRBGFL28_2.visible = false
			VRBGFL28_3.visible = false
			VRBGFL28_4.visible = false
		End If
        flasha0004.visible = false
        sol28timer.enabled = false
    end if
end sub

Sub Sol29(enabled) ' Formerly 30
	Flash1 enabled
End Sub


sub Sol30(enabled)
	If Enabled = True Then
		sol30lvl = 1
		sol30timer_timer
	End If
end sub

Rampflash4.state = 1
Rampflash4.intensityscale = 0
Flash003.state = 1
Flash003.intensityscale = 0

sub sol30timer_timer					
	if Not sol30timer.enabled then
		If VRRoom > 0 Then
			VRBGFL30_1.visible = true
			VRBGFL30_2.visible = true
			VRBGFL30_3.visible = true
			VRBGFL30_4.visible = true
		End If
		sol30timer.enabled = true
	end if
	If VRRoom > 0 Then
		VRBGFL30_1.opacity = 400 * sol30lvl^1.5
		VRBGFL30_2.opacity = 400 * sol30lvl^1.5
		VRBGFL30_3.opacity = 400 * sol30lvl^1.5
		VRBGFL30_4.opacity = 400 * sol30lvl^2
	End If
	Rampflash4.intensityscale = 2 * sol30lvl
	Flash003.intensityscale = 2 * sol30lvl
	sol30lvl = 0.8 * sol30lvl - 0.01
	if sol30lvl < 0 then sol30lvl = 0
	if sol30lvl =< 0 Then
        If VRRoom > 0 Then
			VRBGFL30_1.visible = false
			VRBGFL30_2.visible = false
			VRBGFL30_3.visible = false
			VRBGFL30_4.visible = false
		End If
		sol30timer.enabled = false
	end if
end sub

Sub Sol31(enabled)
	Flash3 enabled
	Flash2 enabled
End Sub

Sub Sol32(enabled)
	Flash4 enabled
End Sub


'***************************************************
'      Real Time Sub
'***************************************************
Dim FM, FMI, FMIV
FM = 0 'FlashMagnet On or off
FMI = 200 'FlashMagnet Variable Intensity
FMIV = 5 'FlashMagnet Add Variable Intensity

Sub RealTime_timer()
	If FantasyMagnet = 1 then
		If FM = 1 then 
			FMI = FMI + FMIV
			FlashMagnet.IntensityScale = FMI/2
			FlashMagnet.Opacity = 2+(FMI/400)
			FlashMagnet.Amount = (100 / (FMI/6))
			FlashMagnet.rotz = FlashMagnet.rotz + (Int(Rnd*5)+1)
			if FMI > 800 then FMIV = -3:RandomFM:End If
			if FMI < 100 then FMIV = 3:RandomFM:End If
		else 
			FlashMagnet.IntensityScale = 0
		End if
	End if

	if preload = 1 then preloadTimer.enabled = 1:end if
	BallsOnMagnet = Ubound(mFlipperMagnet.Balls) 'Test if there is a ball In Magnet THX To JP Salas !!!
	If BallsOnMagnet = 0 and mdof = 0 then DOF 104, DOFOn:mdof = 1:end If
	If BallsOnMagnet = -1 and mdof = 1 then DOF 104, DOFOff:mdof = 0:end If

	LFlipper.ObjRotZ = LeftFlipper.CurrentAngle -121
	RFlipper.ObjRotZ = RightFlipper.CurrentAngle +121
	LeftFlipperSh.RotZ = LeftFlipper.currentangle
	RightFlipperSh.RotZ = RightFlipper.currentangle
	GateA.Rotx = GateSw17.CurrentAngle
	GateB.Rotx = GateSw40.CurrentAngle
	GateC.Rotx = GateSw54.CurrentAngle
	GateD.Rotx = GateSw32.CurrentAngle
	GateE.Rotx = GateSw19.CurrentAngle
	GateF.Rotx = GateSw18.CurrentAngle
	If ll58.state = 1 then 
		RedLightC.blenddisableLighting = 60
	Else
		RedLightC.blenddisableLighting = 0.5
	End If
	If ll50.state = 1 then 
'		RedLightA.blenddisableLighting = 40
		RedFA.Amount = 40
		RedFA.IntensityScale = 2
	Else
'		RedLightA.blenddisableLighting = 0.3
		RedFA.Amount = 0
		RedFA.IntensityScale = 0
	End If
	If ll51.state = 1 then 
'		RedLightB.blenddisableLighting = 40
		RedFB.Amount = 30
		RedFB.IntensityScale = 2
	Else
'		RedLightB.blenddisableLighting = 0.3
		RedFB.Amount = 0
		RedFB.IntensityScale = 0
	End If
	If ll34.state = 1 then 
'		GreenLight.blenddisableLighting = 20
		GreenF.Amount = 30
		GreenF.IntensityScale = 2
	Else
'		GreenLight.blenddisableLighting = 0.3
		GreenF.Amount = 0
		GreenF.IntensityScale = 0
	End If
	If ll49.state = 1 then 

		StopR = 0:rotorspeed = 10:Rotor.enabled = 1
	end if
	If ll69.state = 1 Then
'		spotlightLight.blenddisablelighting = 0
		Flash69.Amount = 150
		flash69.IntensityScale = 50
	Else 
'		spotlightLight.blenddisablelighting = 0 ' 40
		Flash69.Amount = 0
		flash69.IntensityScale = 0
	End If
End Sub


Sub RandomFM()
	Select Case Int(Rnd*4)+1
		Case 1 : FlashMagnet.imageA = "FlashTest03":FlashMagnet.imageB = "FlashTest01b"
		Case 2 : FlashMagnet.imageA = "FlashTest01":FlashMagnet.imageB = "FlashTest03b"
		Case 3 : FlashMagnet.imageA = "FlashTest03b":FlashMagnet.imageB = "FlashTest01"
		Case 4 : FlashMagnet.imageA = "FlashTest01b":FlashMagnet.imageB = "FlashTest03"
	End Select
End Sub


'***************************************************
'       GI ON OFF
'***************************************************

dim xx
dim lightdir, Light, pflight
pflight = 0
Light=0
lightdir = 0.1

set GICallback = GetRef("UpdateGI")
Sub UpdateGI(no, Enabled)
	If Enabled Then
'		lightdir = 0.1:Textlight.Enabled = 1
'		ShadowGI.visible=1
''		Apron.image= "apron_on"
'		RWires.blenddisablelighting = 1
'		LWires.blenddisablelighting = 1

'		gilvl = 1
'		For each xx in GI:xx.State = 1:	Next

		SetLamp 0,1
		If VRRoom > 0 Then
			For each xx in VRBGGI:xx.Visible = 1: Next
		End If

		Sound_GI_Relay 1,BottomTurboBumper

		DOF 103, DOFOn
'		PinCab_Backglass.blenddisablelighting = 3
		If B2SOn Then
			Controller.B2SSetData 90, 1	
		End If

	Else
'		lightdir = -0.1:Textlight.Enabled = 1
'		ShadowGI.visible=0
''		Apron.image= "apron_off"
'		RWires.blenddisablelighting = 0.1
'		LWires.blenddisablelighting = 0.1

'		gilvl = 0
'		For each xx in GI:xx.State = 0: Next

		SetLamp 0,0
		If VRRoom > 0 Then
			For each xx in VRBGGI:xx.Visible = 0: Next
		End If

		Sound_GI_Relay 0,BottomTurboBumper

		DOF 103, DOFOff
'		PinCab_Backglass.blenddisablelighting = 0.2
		If B2SOn Then
			Controller.B2SSetData 90, 0	
		End If
	End If
End Sub

'Sub UpdateGI_backup(no, Enabled)
'	If Enabled Then
'		lightdir = 0.1:Textlight.Enabled = 1
'		ShadowGI.visible=1
''		Apron.image= "apron_on"
'		RWires.blenddisablelighting = 1
'		LWires.blenddisablelighting = 1
'
'		gilvl = 1
'		For each xx in GI:xx.State = 1:	Next
'		If VRRoom > 0 Then
'			For each xx in VRBGGI:xx.Visible = 1: Next
'		End If
'
'		Sound_GI_Relay 1,BottomTurboBumper
'
'		DOF 103, DOFOn
'		PinCab_Backglass.blenddisablelighting = 3
'		If B2SOn Then
'			Controller.B2SSetData 90, 1	
'		End If
'
'	Else
'		lightdir = -0.1:Textlight.Enabled = 1
'		ShadowGI.visible=0
''		Apron.image= "apron_off"
'		RWires.blenddisablelighting = 0.1
'		LWires.blenddisablelighting = 0.1
'
'		gilvl = 0
'		For each xx in GI:xx.State = 0: Next
'		If VRRoom > 0 Then
'			For each xx in VRBGGI:xx.Visible = 0: Next
'		End If
'
'		Sound_GI_Relay 0,BottomTurboBumper
'
'		DOF 103, DOFOff
'		PinCab_Backglass.blenddisablelighting = 0.2
'		If B2SOn Then
'			Controller.B2SSetData 90, 0	
'		End If
'	End If
'End Sub

'
'Sub Textlight_timer
'	light=light+(lightdir) '/2 si 0,5
'	if lightdir = 0.1 then pflight = pflight - 10:end If
'	if lightdir = -0.1 then pflight = pflight + 10:end If
'	if pflight > 105 then pflight = 100
'	if pflight < 0 then pflight = 0
'
'	if lightdir = 0.1 and light > 1 then
'		PlayfieldInsertOutline.material = "Playfield Outlines"
'		playfieldOff.opacity = 0
''		SidesBack.image = "BacktestOn"
'		Plastics.blenddisablelighting = 1
'		MrampB.blenddisablelighting = 0.6
'		BumperA.blenddisableLighting = 1
'		BumperA001.blenddisableLighting = 1
'		BumperA002.blenddisableLighting = 1
'		Rampe3.blenddisableLighting = 2
'		Rampe2.blenddisableLighting = 1.4
'		Rampe1.blenddisableLighting = .8
'		VisA.blenddisablelighting = 4
'		VisB.blenddisablelighting = 4
'		MurMetal.blenddisablelighting = 2
'		MetalParts.blenddisablelighting = 2
'		spotlight.blenddisablelighting = 40
''		spotlightLight.blenddisablelighting = 40
'		PegsBoverSlings.blenddisablelighting = 1
'		Apron.blenddisablelighting = 0.7
'		ApronCachePlunger.blenddisablelighting = 0.7
'		Ledrouge.Amount = 100:LedRouge.IntensityScale = 10
'		LedBleu1.Amount = 100:LedBleu1.IntensityScale = 10
'		LedBleu2.Amount = 100:LedBleu2.IntensityScale = 10
'		LedFond.Amount = 100:LedFond.IntensityScale = 200
'		light = 1
'		me.enabled = 0
'	End If
'
'	if lightdir = -0.1 and light < 0 then 
'PlayfieldInsertOutline.material = "Playfield Outlines1"
'		playfieldOff.opacity = 100
''		SidesBack.image = "Backtest"
'		MrampB.blenddisablelighting = 0
'		Plastics.blenddisablelighting = 0
'		BumperA.blenddisableLighting = 0.2
'		BumperA001.blenddisableLighting = 0.2
'		BumperA002.blenddisableLighting = 0.2
'		Rampe3.blenddisableLighting = 0.5
'		Rampe2.blenddisableLighting = 0.3
'		Rampe1.blenddisableLighting = 0.1
'		VisA.blenddisablelighting = 0
'		VisB.blenddisablelighting = 0
'		MurMetal.blenddisablelighting = 0
'		MetalParts.blenddisablelighting = 0
'		spotlight.blenddisablelighting = 0.5
''		spotlightLight.blenddisablelighting = 0.5
'		PegsBoverSlings.blenddisablelighting = 0
''		Rampe3.image = "rampe3GIOFF"
''		Rampe2.image = "rampe2GIOFF"
''		Rampe1.image = "rampe1GIOFF"
'		Apron.blenddisablelighting = 0
'		ApronCachePlunger.blenddisablelighting = 0
'		Plastics.image = "plasticsOff"
'		Ledrouge.Amount = 100:LedRouge.IntensityScale = 0
'		LedBleu1.Amount = 100:LedBleu1.IntensityScale = 0
'		LedBleu2.Amount = 100:LedBleu2.IntensityScale = 0
'		LedFond.Amount = 100:LedFond.IntensityScale = 0
'		light = 0
'		me.enabled = 0
'	Else
'		Plastics.image = "plasticsOn"
''		Rampe3.image = "rampe3GION"
''		Rampe2.image = "rampe2GION"
''		Rampe1.image = "rampe1GION"
'	End If
'	for each xx in DL:
'		xx.blendDisableLighting = light
'		playfieldOff.opacity = pflight
'	Next
'End Sub

'***************************************************







'******************************************************
'****  LAMPZ by nFozzy
'******************************************************


Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              ' Setup lamp assignments
LampTimer.Enabled = 1
LampTimer.Interval = 16

Sub LampTimer_Timer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps

	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
Sub LampTimer2_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'***Material Swap***
'Fade material for green, red, yellow colored Bulb prims
Sub FadeMaterialColoredBulb(pri, group, ByVal aLvl)	'cp's script
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 1 'Intensity Adjustment
End Sub


'Fade material for red, yellow colored bulb Filiment prims
Sub FadeMaterialColoredFiliment(pri, group, ByVal aLvl)	'cp's script
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 50  'Intensity Adjustment
End Sub


Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub



Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/10 : next
	'gi speed
	Lampz.FadeSpeedUp(0) = 1/2 : Lampz.FadeSpeedDown(0) = 1/10

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	


	Lampz.MassAssign(1)= LL1
	Lampz.MassAssign(1)= LLb001
	Lampz.Callback(1) = "DisableLighting p1, 220,"
	Lampz.MassAssign(2)= LL2
	Lampz.MassAssign(2)= LLb002
	Lampz.Callback(2) = "DisableLighting p2, 220,"
	Lampz.MassAssign(3)= LL3
	Lampz.MassAssign(3)= LLb003
	Lampz.Callback(3) = "DisableLighting p3, 220,"
	Lampz.MassAssign(4)= LL4
	Lampz.MassAssign(4)= LLb004
	Lampz.Callback(4) = "DisableLighting p4, 220,"
	Lampz.MassAssign(5)= LL5
	Lampz.MassAssign(5)= LLb005
	Lampz.Callback(5) = "DisableLighting p5, 220,"
	Lampz.MassAssign(6)= LL6
	Lampz.MassAssign(6)= LLb006
	Lampz.Callback(6) = "DisableLighting p6, 220,"
	Lampz.MassAssign(7)= LL7
	Lampz.MassAssign(7)= LLb007
	Lampz.Callback(7) = "DisableLighting p7, 220,"
	Lampz.MassAssign(8)= LL8
	Lampz.MassAssign(8)= LLb008
	Lampz.Callback(8) = "DisableLighting p8, 220,"
	'Lampz.MassAssign(9)= LL9  'Not in use
	Lampz.MassAssign(10)= LL10
	Lampz.MassAssign(10)= LLb010
	Lampz.Callback(10) = "DisableLighting p10, 220,"
	Lampz.MassAssign(11)= LL11
	Lampz.MassAssign(11)= LLb011
	Lampz.Callback(11) = "DisableLighting p11, 220,"
	Lampz.MassAssign(12)= LL12
	Lampz.MassAssign(12)= LLb012
	Lampz.Callback(12) = "DisableLighting p12, 220,"
	Lampz.MassAssign(13)= LL13
	Lampz.MassAssign(13)= LLb013
	Lampz.Callback(13) = "DisableLighting p13, 220,"
	Lampz.MassAssign(14)= LL14
	Lampz.MassAssign(14)= LLb014
	Lampz.Callback(14) = "DisableLighting p14, 240,"

	Lampz.MassAssign(15)= LL15
	Lampz.MassAssign(15)= LL15F
	Lampz.MassAssign(15)= LLb015
	Lampz.Callback(15) = "DisableLighting p15f, 400,"
	Lampz.MassAssign(16)= LL16
	Lampz.MassAssign(16)= LLb016
	Lampz.Callback(16) = "DisableLighting p16, 220,"
	Lampz.MassAssign(17)= LL17
	Lampz.MassAssign(17)= LLb017
	Lampz.Callback(17) = "DisableLighting p17, 220,"
	Lampz.MassAssign(18)= LL18
	Lampz.MassAssign(18)= LLb018
	Lampz.Callback(18) = "DisableLighting p18, 220,"
	Lampz.MassAssign(19)= LL19 	'Bumper
	Lampz.MassAssign(19)= LL19A
	Lampz.MassAssign(19)= LL19B
	Lampz.MassAssign(20)= LL20
	Lampz.MassAssign(20)= LLb020
	Lampz.Callback(20) = "DisableLighting p20, 220,"
	Lampz.MassAssign(21)= LL21
	Lampz.MassAssign(21)= LLb021
	Lampz.Callback(21) = "DisableLighting p21, 220,"
	Lampz.MassAssign(22)= LL22
	Lampz.MassAssign(22)= LLb022
	Lampz.Callback(22) = "DisableLighting p22, 220,"
	Lampz.MassAssign(23)= LL23
	Lampz.MassAssign(23)= LLb023
	Lampz.Callback(23) = "DisableLighting p23, 220,"
	Lampz.MassAssign(24)= LL24
	Lampz.MassAssign(24)= LLb024
	Lampz.Callback(24) = "DisableLighting p24, 220,"
	Lampz.MassAssign(25)= LL25
	Lampz.MassAssign(25)= LLb025
	Lampz.Callback(25) = "DisableLighting p25, 220,"
	Lampz.MassAssign(26)= LL26
	Lampz.MassAssign(26)= LLb026
	Lampz.Callback(26) = "DisableLighting p26, 220,"
	Lampz.MassAssign(27)= LL27 	'Bumper
	Lampz.MassAssign(27)= LL27A
	Lampz.MassAssign(27)= LL27B
	Lampz.MassAssign(28)= LL28
	Lampz.MassAssign(28)= LLb028
	Lampz.Callback(28) = "DisableLighting p28, 220,"
	Lampz.MassAssign(29)= LL29
	Lampz.MassAssign(29)= LLb029
	Lampz.Callback(29) = "DisableLighting p29, 220,"
	Lampz.MassAssign(30)= LL30
	Lampz.MassAssign(30)= LLb030
	Lampz.Callback(30) = "DisableLighting p30, 220,"
	Lampz.MassAssign(31)= LL31
	Lampz.MassAssign(31)= LLb031
	Lampz.Callback(31) = "DisableLighting p31, 220,"
	Lampz.MassAssign(32)= LL32
	Lampz.MassAssign(32)= LLb032
	Lampz.Callback(32) = "DisableLighting p32, 220,"
	Lampz.MassAssign(33)= LL33
	Lampz.MassAssign(33)= LLb033
	Lampz.Callback(33) = "DisableLighting p33, 220,"
	Lampz.MassAssign(34)= LL34	'LED (need to change to prim)
	Lampz.Callback(34) = "DisableLighting GreenLight, 100,"
	Lampz.MassAssign(35)= LL35
	Lampz.MassAssign(35)= LLb035
	Lampz.Callback(35) = "DisableLighting p35, 220,"
	Lampz.MassAssign(36)= LL36
	Lampz.MassAssign(36)= LLb036
	Lampz.Callback(36) = "DisableLighting p36, 220,"
	Lampz.MassAssign(37)= LL37
	Lampz.MassAssign(37)= LLb037
	Lampz.Callback(37) = "DisableLighting p37, 220,"
	Lampz.MassAssign(38)= LL38
	Lampz.MassAssign(38)= LLb038
	Lampz.Callback(38) = "DisableLighting p38, 220,"
	Lampz.MassAssign(39)= LL39
	Lampz.MassAssign(39)= LLb039
	Lampz.Callback(39) = "DisableLighting p39, 220,"
	'Lampz.MassAssign(40)= LL40  'Not in use
	Lampz.MassAssign(41)= LL41 	'Bumper
	Lampz.MassAssign(41)= LL41A
	Lampz.MassAssign(41)= LL41B
	Lampz.MassAssign(42)= LL42
	Lampz.MassAssign(42)= LLb042
	Lampz.Callback(42) = "DisableLighting p42, 220,"
	Lampz.MassAssign(43)= LL43
	Lampz.MassAssign(43)= LLb043
	Lampz.Callback(43) = "DisableLighting p43, 220,"
	Lampz.MassAssign(44)= LL44
	Lampz.MassAssign(44)= LLb044
	Lampz.Callback(44) = "DisableLighting p44, 220,"
	Lampz.MassAssign(45)= LL45
	Lampz.MassAssign(45)= LLb045
	Lampz.Callback(45) = "DisableLighting p45, 220,"
	Lampz.MassAssign(46)= LL46
	Lampz.MassAssign(46)= LLb046
	Lampz.Callback(46) = "DisableLighting p46, 220,"
	Lampz.MassAssign(47)= LL47
	Lampz.MassAssign(47)= LLb047
	Lampz.Callback(47) = "DisableLighting p47, 220,"
	Lampz.MassAssign(48)= LL48
	Lampz.MassAssign(48)= LLb048
	Lampz.Callback(48) = "DisableLighting p48, 220,"
	Lampz.MassAssign(49)= LL49	'Copter (need to change to prim)
	Lampz.MassAssign(50)= LL50	'LED (need to change to prim)
	Lampz.Callback(50) = "DisableLighting RedLightA, 150,"
	Lampz.MassAssign(51)= LL51	'LED (need to change to prim)
	Lampz.Callback(51) = "DisableLighting RedLightB, 150,"
	Lampz.MassAssign(52)= LL52
	Lampz.Callback(52) = "DisableLighting p52, 220,"
	Lampz.MassAssign(53)= LL53
	Lampz.MassAssign(53)= LLb053
	Lampz.Callback(53) = "DisableLighting p53, 220,"
	Lampz.MassAssign(54)= LL54
	Lampz.MassAssign(54)= LLb054
	Lampz.Callback(54) = "DisableLighting p54, 220,"
	Lampz.MassAssign(55)= LL55
	Lampz.MassAssign(55)= LLb055
	Lampz.Callback(55) = "DisableLighting p55, 220,"
	Lampz.MassAssign(56)= LL56
	Lampz.MassAssign(56)= LLb056
	Lampz.Callback(56) = "DisableLighting p56, 220,"
	'Lampz.MassAssign(57)= LL57  'Start button. Not in use
	Lampz.MassAssign(58)= LL58
	Lampz.MassAssign(58)= LL58bis
	Lampz.MassAssign(59)= LL59
	Lampz.MassAssign(60)= LL60
	Lampz.MassAssign(60)= LLb060
	Lampz.Callback(60) = "DisableLighting p60, 220,"
	Lampz.MassAssign(61)= LL61
	Lampz.MassAssign(61)= LLb061
	Lampz.Callback(61) = "DisableLighting p61, 220,"
	Lampz.MassAssign(62)= LL62
	Lampz.MassAssign(62)= LLb062
	Lampz.Callback(62) = "DisableLighting p62, 220,"
	Lampz.MassAssign(63)= LL63
	Lampz.MassAssign(63)= LLb063
	Lampz.Callback(63) = "DisableLighting p63, 220,"
	'Lampz.MassAssign(64)= LL64  'Not in use
	Lampz.MassAssign(65)= LL65
	Lampz.MassAssign(65)= LLb065
	Lampz.Callback(65) = "DisableLighting p65, 220,"
	Lampz.MassAssign(66)= LL66
	Lampz.MassAssign(66)= LLb066
	Lampz.Callback(66) = "DisableLighting p66, 220,"
	Lampz.MassAssign(67)= LL67
	Lampz.MassAssign(67)= LLb067
	Lampz.Callback(67) = "DisableLighting p67, 220,"
	Lampz.MassAssign(68)= LL68
	Lampz.MassAssign(68)= LLb068
	Lampz.Callback(68) = "DisableLighting p68, 220,"
	Lampz.MassAssign(69)= LL69 	'Copter Spotlight
	Lampz.Callback(69) = "DisableLighting spotlightLight, 100,"

	'Lampz.MassAssign(70)= LL70  'Not in use
	'Lampz.MassAssign(71)= LL71  'Not in use

	if DesktopMode and vrroom = 0 then
		Lampz.MassAssign(73) = L73
		Lampz.MassAssign(74) = L74
		Lampz.MassAssign(75) = L75
		Lampz.MassAssign(76) = L76
		Lampz.MassAssign(77) = L77
		Lampz.MassAssign(78) = L78
		Lampz.MassAssign(79) = L79
		Lampz.MassAssign(80) = L80
		Lampz.MassAssign(72) = L72
	end If

	Lampz.obj(0) = ColtoArray(GI)
	Lampz.Callback(0) = "GIUpdates"
	Lampz.state(0) = 1

	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub

sub OnPrimsVisible(aValue)
	dim kk
	If aValue then
		For each kk in ON_Prims:kk.visible = 1:next
	Else
		For each kk in ON_Prims:kk.visible = 0:next
	end If
end Sub

sub OffPrimsVisible(aValue)
	dim kk
	If aValue then
		For each kk in OFF_Prims:kk.visible = 1:next
	Else
		For each kk in OFF_Prims:kk.visible = 0:next
	end If
end Sub

'GI callback
Const PFGIOFFOpacity = 100

Sub GIUpdates(ByVal aLvl)	'argument is unused
	dim x

	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	'debug.print "aLvl=" & aLvl & " giprevalvl=" & giprevalvl

	if aLvl = 0 then										'GI OFF, let's hide ON prims
		'debug.print "aLvl = 0. OnPrimsVisible False"
		OnPrimsVisible False
'		Rampe2.image = "rampe2GIOFF"
'		Rampe1.image = "rampe1GIOFF"
'		Flasherlight1.intensity = 7
		If gilvl = 1 Then OffPrimsVisible true
'		if ballbrightness <> -1 then ballbrightness = ballbrightMin
	Elseif aLvl = 1 then									'GI ON, let's hide OFF prims
		'debug.print "aLvl = 1. OffPrimsVisible False"
		OffPrimsVisible False
'		Flasherlight1.intensity = 2
		If gilvl = 0 Then OnPrimsVisible True
'		if ballbrightness <> -1 then ballbrightness = ballbrightMax
	Else
		if gilvl = 0 Then								'GI has just changed from OFF to fading, let's show ON
			'debug.print "giprevalvl = 0. OnPrimsVisible True"
			OnPrimsVisible True
'			Rampe2.image = "rampe2GION"
'			Rampe1.image = "rampe1GION"
'			ballbrightness = ballbrightMin + 1
		elseif gilvl = 1 Then							'GI has just changed from ON to fading, let's show OFF
			'debug.print "giprevalvl = 1. OffPrimsVisible true"
			OffPrimsVisible true
'			ballbrightness = ballbrightMax - 1
		Else
			'no change
		end if
	end if

	'UpdateMaterial "OpaqueON",			0,0,0,0,0,0,((aLvl*0.75)+0.25)^1,RGB(255,255,255),0,0,False,True,0,0,0,0		'let transparency be only 0.25 and 1.
'	UpdateMaterial "OpaqueON",			0,0,0,0,0,0,aLvl,RGB(255,255,255),0,0,False,True,0,0,0,0		'let transparency be only 0.25 and 1.
    'UpdateMaterial "PlasticTransON",	0,0,0,0,0,0,aLvl^1,RGB(255,255,255),0,0,False,True,0,0,0,0

'	Playfield_OFF.opacity = PFGIOFFOpacity - (PFGIOFFOpacity * alvl^3)
'	plasticRamps.blenddisablelighting = 1.5 * alvl + 0.5
'
'	FlashOffDL = FlasherOffBrightness*(4/5*aLvl + 1/5)
'	Flasherbase1.blenddisablelighting = FlashOffDL
'	Flasherbase2.blenddisablelighting = FlashOffDL
'	Flasherbase3.blenddisablelighting = FlashOffDL
'	Flasherbase4.blenddisablelighting = FlashOffDL
'	Flasherbase5.blenddisablelighting = FlashOffDL
'
'	'ball
'	if ballbrightness <> ballbrightMax Or ballbrightness <> ballbrightMin Or ballbrightness <> -1 then ballbrightness = INT(alvl * (ballbrightMax - ballbrightMin) + ballbrightMin)


	ShadowGI.opacity = 70 * aLvl^2
'		Apron.image= "apron_on"
	RWires.blenddisablelighting = 0.4 * aLvl^2 + 0.05
	LWires.blenddisablelighting = 0.4 * aLvl^2 + 0.05


	MrampB.blenddisablelighting = 0.6 * alvl^3
	Plastics.blenddisablelighting = alvl
	BumperA.blenddisableLighting = 0.8 * alvl + 0.2
	BumperA001.blenddisableLighting = 0.8 * alvl + 0.2
	BumperA002.blenddisableLighting = 0.8 * alvl + 0.2
	Rampe3.blenddisableLighting = 3.5 * alvl + 0.5		'gi fading material
	Rampe2.blenddisableLighting = 1.1 * alvl + 0.3
	Rampe1.blenddisableLighting = 3.7 * alvl + 0.1
	VisA.blenddisablelighting = 4 * alvl
	VisB.blenddisablelighting = 4 * alvl
	MurMetal.blenddisablelighting = 2 * alvl^3
	MetalParts.blenddisablelighting = 2 * alvl^3
	spotlight.blenddisablelighting = 39.5 * alvl + 0.5
'		spotlightLight.blenddisablelighting = 0.5
	PegsBoverSlings.blenddisablelighting = alvl
	Apron.blenddisablelighting = 0.7 * alvl
	ApronCachePlunger.blenddisablelighting = 0.7 * alvl^2

	Ledrouge.Amount = 100:LedRouge.IntensityScale = 10 * aLvl^2
	LedBleu1.Amount = 100:LedBleu1.IntensityScale = 10 * aLvl^2
	LedBleu2.Amount = 100:LedBleu2.IntensityScale = 10 * aLvl^2
	LedFond.Amount = 100:LedFond.IntensityScale = 200 * aLvl^2
'UpdateMaterial(string, 			float wrapLighting, float roughness, 	float glossyImageLerp, 		float thickness, float edge, float edgeAlpha, float opacity, OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive, float elasticity, float elasticityFalloff, float friction, float scatterAngle)
	UpdateMaterial "PlasticsFade",	0,					1,					0,							0,				1,			0,					aLvl^2,		RGB(192,192,192),RGB(248,248,248),0,False,True,0,0,0,0
	UpdateMaterial "RampFade",		0,					0,					1,							0,				1,			0,					0.3 * aLvl^1,	RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "SidesFade",		0,					0,					1,							0,				1,			0,					aLvl^1, RGB(33,33,33),0,0,False,True,0,0,0,0

	for each xx in DL:
		xx.blendDisableLighting = aLvl*0.85 + 0.15
	Next
	playfieldOff.opacity = 100 * (1-alvl)
	gilvl = alvl
    
End Sub
ShadowGI.visible=1

'====================
'Class jungle nf
'====================

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Public UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		''debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next
		'msgbox "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		''debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class


'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function


Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function


Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub

Sub SetModLamp(aNr, aInput)
	ModLampz.state(aNr) = abs(aInput)/255
End Sub



'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
			AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function

'******************************************************
'****  END LAMPZ
'******************************************************







'
''***************************************************
''       JP's VP10 Fading Lamps & Flashers
''       Based on PD's Fading Light System
'' SetLamp 0 is Off
'' SetLamp 1 is On
'' fading for non opacity objects is 4 steps
''***************************************************
'
'Dim LampState(200), FadingLevel(200)
'Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)
'
'InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
'LampTimer.Interval = 20 'lamp fading speed
'LampTimer.Enabled = 1
'
'' Lamp & Flasher Timers
'
'Sub LampTimer_Timer()
'	Dim chgLamp, num, chg, ii
'	chgLamp = Controller.ChangedLamps
'	If Not IsEmpty(chgLamp) Then
'		For ii = 0 To UBound(chgLamp)
'			LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
'			FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
'
'		Next
'	End If
'	UpdateLamps
'End Sub
'
'Sub UpdateLamps
'	NFadeL 1, LL1
'	NFadeL 2, LL2
'	NFadeL 3, LL3
'	NFadeL 4, LL4
'	NFadeL 5, LL5
'	NFadeL 6, LL6
'	NFadeL 7, LL7
'	NFadeL 8, LL8
'	'  NFadeL 9, LL9 Not in use
'	NFadeL 10, LL10
'	NFadeL 11, LL11
'	NFadeL 12, LL12
'	NFadeL 13, LL13
'	NFadeL 14, LL14
'	NFadeL 15, LL15
'	NFadeL 16, LL16
'	NFadeL 17, LL17
'	NFadeL 18, LL18
'	NFadeLm 19, LL19A
'	NFadeLm 19, LL19B
'	NFadeL 19, LL19 'Bumper
'	NFadeL 20, LL20
'	NFadeL 21, LL21
'	NFadeL 22, LL22
'	NFadeL 23, LL23
'	NFadeL 24, LL24
'	NFadeL 25, LL25
'	NFadeL 26, LL26
'	NFadeLm 27, LL27A
'	NFadeLm 27, LL27B
'	NFadeL 27, LL27 'Bumper
'	NFadeL 28, LL28
'	NFadeL 29, LL29
'	NFadeL 30, LL30
'	NFadeL 31, LL31
'	NFadeL 32, LL32
'	NFadeL 33, LL33
'	NFadeL 34, LL34
'	NFadeL 35, LL35
'	NFadeL 36, LL36
'	NFadeL 37, LL37
'	NFadeL 38, LL38
'	NFadeL 39, LL39
'	'	NFadeL 40, LL40 'Not in use 
'	NFadeLm 41, LL41A
'	NFadeLm 41, LL41B
'	NFadeL 41, LL41 'Bumper
'	NFadeL 42, LL42
'	NFadeL 43, LL43
'	NFadeL 44, LL44
'	NFadeL 45, LL45
'	NFadeL 46, LL46
'	NFadeL 47, LL47
'	NFadeL 48, LL48
'	NFadeL 49, LL49 'Copter
'	NFadeL 50, LL50
'	NFadeL 51, LL51
'	NFadeL 52, LL52
'	NFadeL 53, LL53
'	NFadeL 54, LL54
'	NFadeL 55, LL55
'	NFadeL 56, LL56
'	'	NFadeL 57, LL57 Start button
'	NFadeLm 58, LL58
'	NFadeL 58, LL58bis
'	NFadeL 59, LL59
'	NFadeL 60, LL60
'	NFadeL 61, LL61
'	NFadeL 62, LL62
'	NFadeL 63, LL63
'	'	NFadeL 64, LL64
'	NFadeL 65, LL65
'	NFadeL 66, LL66
'	NFadeL 67, LL67
'	NFadeL 68, LL68
'	NFadeL 69, LL69 'Copter Spotlight
'	'	NFadeL 70, LL70 Not in use
'	'	NFadeL 71, LL71 Not in use
'
'End Sub
'
'
'' div lamp subs
'
'Sub InitLamps()
'	Dim x
'	For x = 0 to 200
'		LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
'		FadingLevel(x) = 4      ' used to track the fading state
'		FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
'		FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
'		FlashMax(x) = 1         ' the maximum value when on, usually 1
'		FlashMin(x) = 0         ' the minimum value when off, usually 0
'		FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
'	Next
'End Sub
'
'Sub AllLampsOff
'	Dim x
'	For x = 0 to 200
'		SetLamp x, 0
'	Next
'End Sub
'
'Sub SetLamp(nr, value)
'	If value <> LampState(nr) Then
'		LampState(nr) = abs(value)
'		FadingLevel(nr) = abs(value) + 4
'	End If
'End Sub
'
'' Lights: used for VP10 standard lights, the fading is handled by VP itself
'
'Sub NFadeL(nr, object)
'	Select Case FadingLevel(nr)
'		Case 4:object.state = 0:FadingLevel(nr) = 0
'		Case 5:object.state = 1:FadingLevel(nr) = 1
'	End Select
'End Sub
'
'Sub NFadeLm(nr, object) ' used for multiple lights
'	Select Case FadingLevel(nr)
'		Case 4:object.state = 0
'		Case 5:object.state = 1
'	End Select
'End Sub
'
''Lights, Ramps & Primitives used as 4 step fading lights
''a,b,c,d are the images used from on to off
'
'Sub FadeObj(nr, object, a, b, c, d)
'	Select Case FadingLevel(nr)
'		Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
'		Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
'		Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
'		Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
'		Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
'		Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
'	End Select
'End Sub
'
'Sub FadeObjm(nr, object, a, b, c, d)
'	Select Case FadingLevel(nr)
'		Case 4:object.image = b
'		Case 5:object.image = a
'		Case 9:object.image = c
'		Case 13:object.image = d
'	End Select
'End Sub
'
'Sub NFadeObj(nr, object, a, b)
'	Select Case FadingLevel(nr)
'		Case 4:object.image = b:FadingLevel(nr) = 0 'off
'		Case 5:object.image = a:FadingLevel(nr) = 1 'on
'	End Select
'End Sub
'
'Sub NFadeObjm(nr, object, a, b)
'	Select Case FadingLevel(nr)
'		Case 4:object.image = b
'		Case 5:object.image = a
'	End Select
'End Sub
'
'' Flasher objects
'
'Sub Flash(nr, object)
'	Select Case FadingLevel(nr)
'		Case 4 'off
'			FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
'			If FlashLevel(nr) < FlashMin(nr) Then
'				FlashLevel(nr) = FlashMin(nr)
'				FadingLevel(nr) = 0 'completely off
'			End if
'			Object.IntensityScale = FlashLevel(nr)
'		Case 5 ' on
'			FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
'			If FlashLevel(nr) > FlashMax(nr) Then
'				FlashLevel(nr) = FlashMax(nr)
'				FadingLevel(nr) = 1 'completely on
'			End if
'			Object.IntensityScale = FlashLevel(nr)
'	End Select
'End Sub
'
'Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
'	Object.IntensityScale = FlashLevel(nr)
'End Sub
'
'
''**********************************************************************



Sub SolLockOut(Enabled)
	If Enabled Then vpmTimer.PulseSw 15
End Sub

'********** Sling Shot Animations *******************************
' Rstep and Lstep  are the variables that increment the animation
'****************************************************************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 62
	RandomSoundSlingshotRight sling1
'	RightSlingFlash.state =0
	RSling.Visible = 0
	RSling1.Visible = 1
	sling1.TransZ = -26
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	'PlaySoundAtVol SoundFX("RightSlingSub",DOFContactors), sling1, 1
	'PlaySoundAtVol SoundFX("RightSlingSub2",DOFContactors), sling1, 1
	'    PlaySoundAtVol "RightSlingSub", SLING1, 1
	'    PlaySoundAtVol "RightSlingSub2", SLING1, 1
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -16
		Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
'	RightSlingFlash.state =1
'	if RSLing1.Visible = 0 then RightSlingFlash.state =0:end If
End Sub

Sub LeftSlingShot_Slingshot
    LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 61
	RandomSoundSlingshotLeft sling2
'	LeftSlingFlash.state = 0
	LSling.Visible = 0
	LSling1.Visible = 1
	sling2.TransZ = -26
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	'PlaySoundAtVol SoundFX("LeftSlingSub",DOFContactors), sling2, 1
	'PlaySoundAtVol SoundFX("LeftSlingSub2",DOFContactors), sling2, 1
	'	PlaySoundAtVol "LeftSlingSub", sling2, 1
	'	PlaySoundAtVol "LeftSlingSub2", sling2, 1
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -16
		Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
'	LeftSlingFlash.state = 1
'	if LSLing1.Visible = 0 then LeftSlingFlash.state = 0:end If
End Sub



Sub GoldenEye_Exit  '  in some tables this needs to be Table1_Exit
	Controller.Games(cGameName).Settings.Value("sound") = 1
	Controller.Stop
End Sub


'**************** KEYS ***********************

Sub GoldenEye_KeyDown(ByVal Keycode)
	If KeyCode=StartGameKey Then
		Controller.Switch(3)=1
		soundStartButton()
		Exit Sub
	End If
	If KeyCode=KeySlamDoorHit Then
		Controller.Switch(7)=1
		Exit Sub
	End If

	If keycode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 1:SoundNudgeCenter()

	If KeyCode=PlungerKey Then Controller.Switch(9)=1 ': SoundPlungerPull()

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

		End Select
	End If


	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress

	If vpmKeyDown(KeyCode) Then Exit Sub
End Sub

Sub GoldenEye_KeyUp(ByVal KeyCode)
	If KeyCode=StartGameKey Then
		Controller.Switch(3)=0
		Exit Sub
	End If
	If KeyCode=KeySlamDoorHit Then
		Controller.Switch(7)=0
		Exit Sub
	End If
	If KeyCode=PlungerKey Then Controller.Switch(9)=0 ': SoundPlungerReleaseBall()

	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress

	If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode = RightMagnaSave Then
		if DisableLUTSelector = 0 then
			LUTmeUP = LUTMeUp + 1
			if LutMeUp > MaxLut then LUTmeUP = 0
			SetLUT
		end if
	end if
	If keycode = LeftMagnaSave Then
		if DisableLUTSelector = 0 then
			LUTmeUP = LUTMeUp - 1
			if LutMeUp < 0 then LUTmeUP = MaxLut
			SetLUT
		end if
	end if
End Sub


'***********************************************************************************
'*************                 Bumpers                               ***************
'***********************************************************************************
Sub LeftTurboBumper_Hit
	vpmTimer.PulseSw 41
	'PlaySoundAtVol SoundFX("fx_bumper1",DOFContactors), ActiveBall, 1
	'PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1
	RandomSoundBumperTop LeftTurboBumper
	Me.TimerEnabled = 1
	'	PlaySoundAtVol "bumper", LeftTurboBumper, 1
End Sub

Sub LeftTurboBumper_Timer
	Me.Timerenabled = 0
End Sub

Sub BottomTurboBumper_Hit
	vpmTimer.PulseSw 42
	'PlaySoundAtVol SoundFX("fx_bumper2",DOFContactors), ActiveBall, 1
	'PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1
	RandomSoundBumperBottom BottomTurboBumper
	Me.TimerEnabled = 1
	'	PlaySoundAtVol "bumper", BottomTurboBumper, 1
End Sub

Sub BottomTurboBumper_Timer
	Me.Timerenabled = 0
End Sub

Sub RightTurboBumper_Hit
	vpmTimer.PulseSw 43
	'PlaySoundAtVol SoundFX("fx_bumper3",DOFContactors), ActiveBall, 1
	'PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1
	RandomSoundBumperMiddle RightTurboBumper
	Me.TimerEnabled = 1
	'	PlaySoundAtVol "bumper", RightTurboBumper, 1
End Sub

Sub RightTurboBumper_Timer
	Me.Timerenabled = 0
End Sub
'**********************************************************************************


'************************** Left Ramp Helper ****************************************
Sub Trigger1_Hit
	If ActiveBall.VelY<9 and activeBall.velY>0 Then ActiveBall.VelY=ActiveBall.VelY+9
End Sub

'************************************************************************************



Sub Drain_Hit:ClearBallID : bsTrough.AddBall Me:	RandomSoundDrain Drain:End Sub
Sub Scoop_Hit:ClearBallID :bsScoop.AddBall Me:PlaySound "Scoopenter",1,VolumeDial:End Sub
Sub TankKickBig_Hit:ClearBallID :bsTank.AddBall Me:End Sub
Sub DropRamp1_Hit:ClearBallID :bsTank.AddBall Me
	PlaySound "Kicker_enter",1,VolumeDial
End Sub
Sub Plunger_Hit:bsPlunger.Addball 0:End Sub
'Sub Kicker1_Hit:bsLockOut.AddBall 0:End Sub


'***************** Radar ********************

Sub SolSatMotorRelay(enabled)		'rotate satelite
	if enabled then
		RotRadar.enabled = 1
		PlaySoundAtVolLoops "Motor", RadarA, 0.5 , -1
	else
		RotRadar.enabled = 0
		StopSound "Motor"
		'		radinit = 1
		Controller.Switch(20) = false
		Controller.Switch(23) = false
	End If
End Sub

dim RadarDirection, RadarC, First, radinit
RadarC = 0:First = 0:RadarDirection = 3:radinit = 0

sub RotRadar_Timer()
	RadarA.objrotz = RadarA.objrotz + RadarDirection:RadarB.objrotz = RadarB.objrotz + RadarDirection
	RadarC = RadarC + 1
	if RadarC > 10 and RadarDirection = 3 and First = 0 then RadarDirection=-3:RadarC = 0:First = 1:end If
	if RadarC > 21 and RadarDirection = -3 then RadarDirection=3:RadarC = 0:First = 1:end If
	if RadarC > 21 and RadarDirection = 3 then RadarDirection=-3:RadarC = 0:First = 1:end If
End Sub


'***************** Radar Ramp ********************
Dim RampC,RampDirection
RampC=0 '3 time to move up
RampDirection=-15'Ramp Will Move UP positive value it will move down


Sub SolSatLaunchRamp(Enabled)
	If Enabled Then
		RampDirection=-15:RadarRampAnim.enabled = 1
		RampRadar.collidable=1
		SatTop.IsDropped=0
		SatTop1.IsDropped=0
		SatTop2.IsDropped=0
		PlaySoundAtVol "RadarRampOn", RampSat3D, 1
	Else
		RampDirection=15:RadarRampAnim.enabled = 1
		RampRadar.collidable=0
		SatTop.IsDropped=1
		SatTop1.IsDropped=1
		SatTop2.IsDropped=1
		PlaySoundAtVol "RadarRampOff", RampSat3D, 1
	End If
End Sub

sub RadarRampAnim_Timer()
	RampSat3D.objrotx = RampSat3D.objrotx + RampDirection
	RampSat3Dv.objrotx =  RampSat3Dv.objrotx+ RampDirection
	RampC = RampC + 1
	if RampC = 3 and RampDirection=-15 then RampSat3D.objrotx = -45:RampSat3Dv.objrotx = -45:RampC = 0:me.enabled = 0:LL40.State = 1:end If
	if RampC = 3 and RampDirection=15 then RampSat3D.objrotx = 0:RampSat3Dv.objrotx = 0:RampC = 0:me.enabled = 0:LL40.State = 0:end If
End Sub

'****************** Radar Magnet **********************
dim RadarBall 
dim BallInRadarLock : BallInRadarLock = False

Sub SolRadarMagnet(Enabled)
	If Not Enabled Then
		BallInRadarLock = False
		RadarKicker.Kick 0,1
		Controller.Switch(23)=0
		StopSound "fx_magnetR"
	End If
End Sub

Sub RMagnet_Hit
	if Not BallInRadarLock Then
		if MagnetVolMax = 1 then
			PlaySound "fx_magnetR",-1,MagnetVol
		else
			PlaySound "fx_magnetR",-1
		end if
		ActiveBall.velX=0
		ActiveBall.velY=0
		ActiveBall.velZ=0
		ActiveBall.X=637
		ActiveBall.Y=768
		ActiveBall.Z=152
		PlaySoundAtVol "magnethit", ActiveBall, VolumeDial
	End If
End Sub	

Sub RadarKicker_hit
	Set RadarBall = Activeball
	BallInRadarLock = True
	Controller.Switch(23)=1
End Sub


'********************** Copter ********************

dim rotorspeed, StopR
rotorspeed = 10 
StopR = 0
sub Rotor_timer()
	CopterC.roty = CopterC.roty + rotorspeed
	CopterD.rotx = CopterD.rotx + rotorspeed
	Copter001.image = "CopterBOn"
	if ll49.state = 0 then StopR = 1:Copter001.image = "CopterB":end if
	if StopR = 1 then rotorspeed=rotorspeed -0.1:end If
	if rotorspeed < 0 then me.enabled = 0:StopR = 0:rotorspeed = 10:end If
End Sub

'**************************************************

'*********Moving Ramp Solenoid 18 *****************

Sub SolUpDownRamp(Enabled)
	If Enabled Then
		Ramp005.collidable = 0
		Mramp3.collidable = 0
		Mramp.collidable = 1
		Mramp2.collidable = 1
		MrampB.objrotx = 17
		MrampA.objrotx = 17
		PlaySoundAtVol "RampDown", MrampB, 1
	Else
		Mramp.collidable = 0
		Mramp2.collidable = 0
		MrampB.objrotx = 1
		MrampA.objrotx = 1
		Ramp005.collidable = 1
		Mramp3.collidable = 1
		PlaySoundAtVol "RampUp", MrampB, 1
	End If
End Sub



'****************************************
' B2B Collision by Steely & Pinball Ken
' jpsalas: added destruk's changes
'  & ball height check
'****************************************


Sub ClearBallID
	On Error Resume Next
	iball = ActiveBall.uservalue
	currentball(iball).UserValue = 0
	If Err Then Msgbox Err.description & vbCrLf & iball
	ballStatus(iBall) = 0
	ballStatus(0) = ballStatus(0) -1
	On Error Goto 0
End Sub


' Get angle
'
'Dim Xin, Yin, rAngle, Radit, wAngle
'
'Sub GetAngle(Xin, Yin, wAngle)
'    If Sgn(Xin) = 0 Then
'        If Sgn(Yin) = 1 Then rAngle = 3 * Pi / 2 Else rAngle = Pi / 2
'        If Sgn(Yin) = 0 Then rAngle = 0
'        Else
'            rAngle = atn(- Yin / Xin)
'    End If
'    If sgn(Xin) = -1 Then Radit = Pi Else Radit = 0
'    If sgn(Xin) = 1 and sgn(Yin) = 1 Then Radit = 2 * Pi
'    wAngle = round((Radit + rAngle), 4)
'End Sub

'Dim iball, cnt, coff, errMessage
Dim cnt, coff

Sub NewBallID 						' Assign new ball object and give it ID for tracking
	For cnt = 1 to ubound(ballStatus)		' Loop through all possible ball IDs
		If ballStatus(cnt) = 0 Then			' If ball ID is available...
			Set currentball(cnt) = ActiveBall			' Set ball object with the first available ID
			currentball(cnt).uservalue = cnt			' Assign the ball's uservalue to it's new ID
			ballStatus(cnt) = 1				' Mark this ball status active
			ballStatus(0) = ballStatus(0)+1 		' Increment ballStatus(0), the number of active balls
			' 	If coff = False Then				' If collision off, overrides auto-turn on collision detection
			' 							' If more than one ball active, start collision detection process
			''		If ballStatus(0) > 1 and XYdata.enabled = False Then 
			''			XYdata.enabled = True
			''		end if
			'	End If
			Exit For					' New ball ID assigned, exit loop
		End If
	Next 
	'  	Debugger 					' For demo only, display stats
	'	If ballStatus(0)=0 then textbox1.text="0"
	'	If ballStatus(0)=1 then textbox1.text="1"
End Sub

Sub BallRelease_Unhit()
	NewBallID
	RandomSoundBallRelease BallRelease
End Sub

Sub Scoop_Unhit()
	NewBallID
End Sub

Sub TankKickBig_Unhit()
	NewBallID
	TankAnim.enabled = 1
End Sub

dim TKA
TKA = 1
'Sub TankAnim_timer()
'	select case TKA
'		case 1:
'			TankCTourelle.y = TankCTourelle.y + 3
'		case 2:
'			TankCTourelle.y = TankCTourelle.y - 15
'		case 3:
'			TankCTourelle.y = TankCTourelle.y - 5
'			TankA.y = TankA.y - 10
'			TankB.y = TankB.y - 10
'		case 4:
'			TankCTourelle.y = TankCTourelle.y - 3
'			TankA.y = TankA.y + 3
'			TankB.y = TankB.y + 3
'		case 5:
'			TankCTourelle.y = TankCTourelle.y + 5
'			TankA.y = TankA.y + 1
'			TankB.y = TankB.y + 1
'		case 6:
'			TankCTourelle.y = TankCTourelle.y + 5
'			TankA.y = TankA.y + 2
'			TankB.y = TankB.y + 2
'		case 7:
'			TankCTourelle.y = TankCTourelle.y + 5
'			TankA.y = TankA.y + 2
'			TankB.y = TankB.y + 2
'		case 7:
'			TankCTourelle.y = TankCTourelle.y + 3
'			TankA.y = TankA.y + 1
'			TankB.y = TankB.y + 1
'		case 7:
'			TankCTourelle.y = TankCTourelle.y + 2
'			TankA.y = TankA.y + 1
'			TankB.y = TankB.y + 1
'	end select
'
'	if TKA = 20 then TKA = 1:TankAnim.enabled = 0:exit sub:end if
'	TKA = TKA + 1
'End Sub

Sub TankAnim_timer()
	select case TKA
		case 1:
			TankCTourelle.y = TankCTourelle.y - 4
		case 2:
			TankCTourelle.y = TankCTourelle.y + 29
		case 3:
			TankCTourelle.y = TankCTourelle.y + 12
			TankA.y = TankA.y + 20
			TankB.y = TankB.y + 20
		case 4:
			TankCTourelle.y = TankCTourelle.y + 3
			TankA.y = TankA.y - 6
			TankB.y = TankB.y - 6
		case 5:
			TankCTourelle.y = TankCTourelle.y - 10
			TankA.y = TankA.y - 2
			TankB.y = TankB.y - 2
		case 6:
			TankCTourelle.y = TankCTourelle.y - 8
			TankA.y = TankA.y - 4
			TankB.y = TankB.y - 4
		case 7:
			TankCTourelle.y = TankCTourelle.y - 8
			TankA.y = TankA.y - 4
			TankB.y = TankB.y - 4
		case 7:
			TankCTourelle.y = TankCTourelle.y - 6
			TankA.y = TankA.y - 4
			TankB.y = TankB.y - 4
		case 7:
			TankCTourelle.y = TankCTourelle.y - 4
			TankA.y = TankA.y - 2
			TankB.y = TankB.y - 2
		case 8:
			TankCTourelle.y = TankCTourelle.y - 4
			TankA.y = TankA.y + 2
			TankB.y = TankB.y + 2
	end select

	if TKA = 10 then TKA = 1:TankAnim.enabled = 0:exit sub:end if
	TKA = TKA + 1
End Sub



'*****************************************
'    JP's VP10 Collision & Rolling Sounds
'*****************************************

Const tnob = 7 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
ReDim rolling(tnob)
ReDim collision(tnob)
Initcollision
Dim ballStatus(7), currentball(7)


Dim DropCount
ReDim DropCount(tnob)

Sub Initcollision
	Dim i
	For i = 0 to tnob
		collision(i) = -1
		rolling(i) = False
	Next
End Sub


Sub CollisionTimer_Timer()
	Dim BOT, B, B1, B2, dx, dy, dz, radii
	BOT = GetBalls
	'	TextBox001.Text = (UBound(BOT))+1
	' rolling

	For B = UBound(BOT) +1 to tnob
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		StopSound("BallRoll_" & b)
		StopSound("fx_plasticrolling" & b)
		StopSound("fx_metalrollingA" & b)
		StopSound("fx_metalrollingB" & b)
		rolling(b) = False
		'		StopSound("fx_Rolling_MetalC" & b)
	Next

	If UBound(BOT) = -1 Then 
		GO = 1:Exit Sub
	Else
		GO = 0
	End if

	For b = 0 to UBound(BOT)

		'debug.print "botZ :"& BOT(b).z

		If BallVel(BOT(b) ) > 1 Then
			rolling(b) = True
			if BOT(b).z < 27 Then ' Ball on playfield
				StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b):StopSound("fx_plasticrolling" & b)
				PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
			Else 
				'Left Ramp
				If InRect(BOT(b).x, BOT(b).y, 48,214,321,105,200,805,101,840) And BOT(b).z < 24+27 And BOT(b).z > 27 Then
					StopSound("BallRoll_" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
					PlaySound("fx_plasticrolling" & b), -1, VolPlayfieldRoll(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				ElseIf InRect(BOT(b).x, BOT(b).y, 317,104,440,146,351,482,280,494) And BOT(b).z < 72+27 And BOT(b).z > 24+27 Then
					StopSound("BallRoll_" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
					PlaySound("fx_plasticrolling" & b), -1, VolPlayfieldRoll(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				ElseIf InRect(BOT(b).x, BOT(b).y, 283,472,480,435,180,1422,25,1352) And BOT(b).z < 72+27 And BOT(b).z > 64+27 Then
					StopSound("BallRoll_" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingB" & b)
					PlaySound("fx_metalrollingA" & b), -1, VolPlayfieldRoll(BOT(b) )*3*MroVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				ElseIf InRect(BOT(b).x, BOT(b).y, 5,1343,189,1425,141,1625,17,1737) And BOT(b).z < 72+27 And BOT(b).z > 64+27 Then
					StopSound("BallRoll_" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
					PlaySound("fx_metalrollingB" & b), -1, VolPlayfieldRoll(BOT(b) )*3*MroVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
					'Right Ramp
				ElseIf InRect(BOT(b).x, BOT(b).y, 710,426,870,338,845,1096,662,1000) And BOT(b).z < 150+27 And BOT(b).z > 27 Then
					StopSound("BallRoll_" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
					PlaySound("fx_plasticrolling" & b), -1, VolPlayfieldRoll(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				ElseIf InRect(BOT(b).x, BOT(b).y, 240,155,852,343,709,427,287,342) And BOT(b).z < 192+27 And BOT(b).z > 147+27 Then
					StopSound("BallRoll_" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
					PlaySound("fx_plasticrolling" & b), -1, VolPlayfieldRoll(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				ElseIf InRect(BOT(b).x, BOT(b).y, 5,437,265,254,286,343,109,492) And BOT(b).z < 171+27 And BOT(b).z > 137+27 Then
					StopSound("BallRoll_" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
					PlaySound("fx_plasticrolling" & b), -1, VolPlayfieldRoll(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				ElseIf InRect(BOT(b).x, BOT(b).y, 4,436,109,493,76,1220,5,1353) And BOT(b).z < 145+27 And BOT(b).z > 80+27 Then
					StopSound("BallRoll_" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
					PlaySound("fx_plasticrolling" & b), -1, VolPlayfieldRoll(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
					'Center Ramp
				ElseIf InRect(BOT(b).x, BOT(b).y, 154,294,245,327,321,805,217,823) And BOT(b).z < 72+27 And BOT(b).z > 27 Then
					StopSound("BallRoll_" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
					PlaySound("fx_plasticrolling" & b), -1, VolPlayfieldRoll(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				ElseIf InRect(BOT(b).x, BOT(b).y, 436,1,458,87,244,326,131,286) And BOT(b).z < 120+27 And BOT(b).z > 147+27 Then
					StopSound("BallRoll_" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
					PlaySound("fx_plasticrolling" & b), -1, VolPlayfieldRoll(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				ElseIf InRect(BOT(b).x, BOT(b).y, 436,1,944,38,786,156,457,87) And BOT(b).z < 119+27 And BOT(b).z > 82+27 Then
					StopSound("BallRoll_" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
					PlaySound("fx_plasticrolling" & b), -1, VolPlayfieldRoll(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				ElseIf InRect(BOT(b).x, BOT(b).y, 785,156,841,115,913,244,850,271) And BOT(b).z < 87+27 And BOT(b).z > 68+27 Then
					StopSound("BallRoll_" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
					PlaySound("fx_plasticrolling" & b), -1, VolPlayfieldRoll(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				ElseIf InRect(BOT(b).x, BOT(b).y, 850,271,935,234,940,1437,871,1427) And BOT(b).z < 72+27 And BOT(b).z > 66+27 Then
					StopSound("BallRoll_" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
					PlaySound("fx_metalrollingB" & b), -1, VolPlayfieldRoll(BOT(b) )*3*MroVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				ElseIf InRect(BOT(b).x, BOT(b).y, 870,1426,958,1440,779,1739,708,1635) And BOT(b).z < 72+27 And BOT(b).z > 66+27 Then
					StopSound("BallRoll_" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
					PlaySound("fx_metalrollingB" & b), -1, VolPlayfieldRoll(BOT(b) )*3*MroVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
				end if
			End If

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				StopSound("fx_plasticrolling" & b)
				StopSound("fx_metalrollingA" & b)
				StopSound("fx_metalrollingB" & b)
				rolling(b) = False
			End If
		End If

		'***Ball Drop Sounds***
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
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
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = BOT(b).X
			BallShadowA(b).visible = 1
		End If

	Next
End Sub


Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub





'******************************************************
'*****   FLUPPER DOMES 
'******************************************************


 Sub Flash1(Enabled)
	If Enabled Then		
		Objlevel(1) = 1 : FlasherFlash1_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase1
 End Sub

 Sub Flash2(Enabled)
	If Enabled Then
		Objlevel(2) = 1 : FlasherFlash2_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase2
 End Sub

 Sub Flash3(Enabled)
	If Enabled Then		
		Objlevel(3) = 1 : FlasherFlash3_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase3
 End Sub

 Sub Flash4(Enabled)
	If Enabled Then
		Objlevel(4) = 1 : FlasherFlash4_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase4
 End Sub




Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = goldeneye   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.3		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.1  	' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.2		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.3		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"

InitFlasher 1, "yellow"
InitFlasher 2, "red"
InitFlasher 3, "red"
InitFlasher 4, "yellow"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 


Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness

	'rothbauerw
	'Adjust the position of the flasher object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	If objbase(nr).roty > 135 then
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
	objlight(nr).bulbhaloheight = objbase(nr).z +50

	'rothbauerw
	'Assign the appropriate bloom image basked on the location of the flasher base
	'Comment out these lines if you want to manually assign the bloom images
	dim xthird, ythird
	xthird = tablewidth/3
	ythird = tableheight/3

	If objbase(nr).x >= xthird and objbase(nr).x <= xthird*2 then
		objbloom(nr).imageA = "flasherbloomCenter"
		objbloom(nr).imageB = "flasherbloomCenter"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird then
		objbloom(nr).imageA = "flasherbloomUpperLeft"
		objbloom(nr).imageB = "flasherbloomUpperLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird then
		objbloom(nr).imageA = "flasherbloomUpperRight"
		objbloom(nr).imageB = "flasherbloomUpperRight"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird*2 then
		objbloom(nr).imageA = "flasherbloomCenterLeft"
		objbloom(nr).imageB = "flasherbloomCenterLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird*2 then
		objbloom(nr).imageA = "flasherbloomCenterRight"
		objbloom(nr).imageB = "flasherbloomCenterRight"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird*3 then
		objbloom(nr).imageA = "flasherbloomLowerLeft"
		objbloom(nr).imageB = "flasherbloomLowerLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird*3 then
		objbloom(nr).imageA = "flasherbloomLowerRight"
		objbloom(nr).imageB = "flasherbloomLowerRight"
	end if

	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objbloom(nr).color = RGB(4,120,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4) : objbloom(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4) : objbloom(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) : objbloom(nr).color = RGB(230,49,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50) : objbloom(nr).color = RGB(200,173,25)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objbloom(nr).color = RGB(255,240,150)
		Case "orange" :  objlight(nr).color = RGB(255,70,0) : objflasher(nr).color = RGB(255,70,0) : objbloom(nr).color = RGB(255,70,0)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	Dim flashx3
	If not objflasher(nr).TimerEnabled Then
		objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1 
		If VRRoom > 0 Then
			select case nr
				case 1
				VRBGFL29_1.visible = 1
				VRBGFL29_2.visible = 1
				VRBGFL29_3.visible = 1
				VRBGFL29_4.visible = 1
				VRBGFL29_5.visible = 1
				VRBGFL29_6.visible = 1
				VRBGFL29_7.visible = 1
				VRBGFL29_8.visible = 1
				Case 2
				VRBGFL31_1.visible = 1
				VRBGFL31_2.visible = 1
				VRBGFL31_3.visible = 1
				VRBGFL31_4.visible = 1
				Case 4
				VRBGFL32_1.visible = 1
				VRBGFL32_2.visible = 1
				VRBGFL32_3.visible = 1
				VRBGFL32_4.visible = 1
			End Select
		End If


	End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5

		If VRRoom > 0 Then
			flashx3 = objlevel(nr)^3 
			select case nr
				case 1
				VRBGFL29_1.opacity = 50 * flashx3^1.5
				VRBGFL29_2.opacity = 50 * flashx3^1.5
				VRBGFL29_3.opacity = 50 * flashx3^1.5
				VRBGFL29_4.opacity = 50 * flashx3^2
				VRBGFL29_5.opacity = 50 * flashx3^1.5
				VRBGFL29_6.opacity = 50 * flashx3^1.5
				VRBGFL29_7.opacity = 50 * flashx3^1.5
				VRBGFL29_8.opacity = 50 * flashx3^2
				Case 2
				VRBGFL31_1.opacity = 200 * flashx3^1.5
				VRBGFL31_2.opacity = 200 * flashx3^1.5
				VRBGFL31_3.opacity = 200 * flashx3^1.5
				VRBGFL31_4.opacity = 200 * flashx3^2
				Case 4
				VRBGFL32_1.opacity = 100 * flashx3^1.5
				VRBGFL32_2.opacity = 100 * flashx3^1.5
				VRBGFL32_3.opacity = 100 * flashx3^1.5
				VRBGFL32_4.opacity = 100 * flashx3^2
			End Select
		End If

	

	If nr=1 Then 
		objlight(nr).IntensityScale = 0.5/3 * FlasherLightIntensity * ObjLevel(nr)^3
	Else
		objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	End If
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 12 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 35 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then
		objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0
			If VRRoom > 0 Then
			select case nr
				case 1
				VRBGFL29_1.visible = 0
				VRBGFL29_2.visible = 0	
				VRBGFL29_3.visible = 0
				VRBGFL29_4.visible = 0
				VRBGFL29_5.visible = 0
				VRBGFL29_6.visible = 0
				VRBGFL29_7.visible = 0
				VRBGFL29_8.visible = 0
				Case 2
				VRBGFL31_1.visible = 0
				VRBGFL31_2.visible = 0
				VRBGFL31_3.visible = 0
				VRBGFL31_4.visible = 0
				Case 4
				VRBGFL32_1.visible = 0
				VRBGFL32_2.visible = 0
				VRBGFL32_3.visible = 0
				VRBGFL32_4.visible = 0
			End Select
		End If
	End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 



'******************************************************
'******  END FLUPPER DOMES
'******************************************************



'
'' what you need in your table to use these flashers:
'' copy the objects flasherbase, flasherlit, flasherlight and flasherflash from layer 1,2,3 and 4 to your table
'' export the materials domebase, domelit0 - domelit9 and import them in your table
'' copy the script below "Dim ... flasherlightx.IntensityScale = 0" and the sub for your flasher to your table
'' for flashing the flasher use in the script: "FlashLevel1 = 1 : FlasherFlash1_Timer"
'' this should also work for flashers with different levels from the rom, just use FlashLevel1 = xx from the rom (in the range 0-1)
''
'' notes:
'' - due to how the texture is made, you need to turn the flasher to face the player: 
''	The red flasher for instance has ObjRotZ = -10 for flasherbase and flasherlit
''	and the flasherflash has RotZ = -10
'' - for the vertical flasher the flasherflash has RotZ = 180
'' - the flasherbase and flasherlit primitive must be on the exact same x,y,z
'' - the flasherflash could have a slightly different position for desktop vs FS view
'' - for the colors yellow/purple/green, please replace the textures and light color on the four objects for the white flasher
'
'
'Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4
'FlasherLight1.IntensityScale = 0
'Flasherlight2.IntensityScale = 0
'Flasherlight3.IntensityScale = 0
'Flasherlight4.IntensityScale = 0
'Flasherlight001.IntensityScale = 0
'''*** Yellow flasher 1***
'Dim matdim1
'Sub FlasherFlash1_Timer()
'	dim flashx3
'	If not FlasherFlash1.TimerEnabled Then 
'		FlasherFlash1.TimerEnabled = True
'		FlasherFlash1.visible = 1
'		Flasher001.visible = 1
'		flasha3.visible = 1
'		flasha4.visible = 1
'		If VRRoom > 0 Then
'			VRBGFL29_1.visible = 1
'			VRBGFL29_2.visible = 1
'			VRBGFL29_3.visible = 1
'			VRBGFL29_4.visible = 1
'			VRBGFL29_5.visible = 1
'			VRBGFL29_6.visible = 1
'			VRBGFL29_7.visible = 1
'			VRBGFL29_8.visible = 1
'		End If
'	End If
'	flashx3 = (FlashLevel1 * FlashLevel1 * FlashLevel1) *2 
'	Flasherflash1.opacity = 800 * flashx3
'	Flasher001.opacity = 250 * flashx3
'	FlasherBase1.BlendDisableLighting = 15 * matdim1
'	FlasherLight1.IntensityScale = flashx3
'	Flasherlight001.IntensityScale = flashx3
'	flasha4.IntensityScale = flashx3^2
'	flasha3.IntensityScale = flashx3^2
'	If VRRoom > 0 Then
'		VRBGFL29_1.opacity = 50 * flashx3^1.5
'		VRBGFL29_2.opacity = 50 * flashx3^1.5
'		VRBGFL29_3.opacity = 50 * flashx3^1.5
'		VRBGFL29_4.opacity = 50 * flashx3^2
'		VRBGFL29_5.opacity = 50 * flashx3^1.5
'		VRBGFL29_6.opacity = 50 * flashx3^1.5
'		VRBGFL29_7.opacity = 50 * flashx3^1.5
'		VRBGFL29_8.opacity = 50 * flashx3^2
'	End If
'	matdim1 = matdim1 - 1
'	FlashLevel1 = FlashLevel1 * 0.85 - 0.01
'	If matdim1 = 0 Then
'		FlasherBase1.BlendDisableLighting = 1
'		Flasherlight1.IntensityScale = 0
'		Flasherlight001.IntensityScale = 0
'		matdim1 = 9
'		FlasherFlash1.visible = 0
'		Flasher001.visible = 0
'		flasha3.visible = 0
'		flasha4.visible = 0
'		If VRRoom > 0 Then
'			VRBGFL29_1.visible = 0
'			VRBGFL29_2.visible = 0
'			VRBGFL29_3.visible = 0
'			VRBGFL29_4.visible = 0
'			VRBGFL29_5.visible = 0
'			VRBGFL29_6.visible = 0
'			VRBGFL29_7.visible = 0
'			VRBGFL29_8.visible = 0
'		End If
'		FlasherFlash1.TimerEnabled = False
'	End If
'End Sub
''
''*** Red flasher ***
'Dim matdim2
'Sub FlasherFlash2_Timer()
'	dim flashx3
'	If not FlasherFlash2.TimerEnabled Then 
'		FlasherFlash2.TimerEnabled = True
'		FlasherFlash2.visible = 1
'		flasha5.visible = 1
'		flasha6.visible = 1
'		flasha7.visible = 1
'		flasha8.visible = 1
'		flasha9.visible = 1
'		If VRRoom > 0 Then
'			VRBGFL31_1.visible = 1
'			VRBGFL31_2.visible = 1
'			VRBGFL31_3.visible = 1
'			VRBGFL31_4.visible = 1
'		End If
'	End If
'	flashx3 = (FlashLevel2 * FlashLevel2 * FlashLevel2)
'	Flasherflash2.opacity = 800 * flashx3^2
'	FlasherBase2.BlendDisableLighting = 10 * matdim2^2
'	FlasherLight2.IntensityScale = 0.5 * flashx3^2
'	flasha5.IntensityScale = 2 * flashx3^3
'	flasha6.IntensityScale = 2 * flashx3^3
'	flasha7.IntensityScale = 4 * flashx3^3
'	flasha8.IntensityScale = 1.5 * flashx3^3
'	flasha9.IntensityScale = 2 * flashx3^3
'	If VRRoom > 0 Then
'		VRBGFL31_1.opacity = 200 * flashx3^1.5
'		VRBGFL31_2.opacity = 200 * flashx3^1.5
'		VRBGFL31_3.opacity = 200 * flashx3^1.5
'		VRBGFL31_4.opacity = 200 * flashx3^2
'	End If	
'	matdim2 = matdim2 - 1	
'	FlashLevel2 = FlashLevel2 * 0.80 - 0.01
'	If matdim2 = 0 Then
'		FlasherBase2.BlendDisableLighting = 1
'		Flasherlight2.IntensityScale = 0
'		matdim2 = 9
'		FlasherFlash2.visible = 0
'		flasha5.visible = 0
'		flasha6.visible = 0
'		flasha7.visible = 0
'		flasha8.visible = 0
'		flasha9.visible = 0
'		If VRRoom > 0 Then
'			VRBGFL31_1.visible = 0
'			VRBGFL31_2.visible = 0
'			VRBGFL31_3.visible = 0
'			VRBGFL31_4.visible = 0
'		End If
'		FlasherFlash2.TimerEnabled = False
'	End If
'End Sub
''
''*** Red flasher 2 ***
'Dim matdim3
'Sub FlasherFlash3_Timer()
'	dim flashx3
'	If not FlasherFlash3.TimerEnabled Then 
'		FlasherFlash3.TimerEnabled = True
'		FlasherFlash3.visible = 1
'	End If
'	flashx3 = (FlashLevel3 * FlashLevel3 * FlashLevel3)
'	Flasherflash3.opacity = 800 * flashx3^2
'	FlasherBase3.BlendDisableLighting = 10 * matdim3^2
'	FlasherLight3.IntensityScale = flashx3^2
'	matdim3 = matdim3 - 1
'	FlashLevel3 = FlashLevel3 * 0.80 - 0.01
'	If matdim3 = 0 Then
'		FlasherBase3.BlendDisableLighting = 1
'		Flasherlight3.IntensityScale = 0
'		matdim3 = 9
'		FlasherFlash3.visible = 0
'		FlasherFlash3.TimerEnabled = False
'	End If
'End Sub
'
''*** Yellow flasher vertical (script is the same as for blue Flasher) ***
'Dim matdim4
'Sub FlasherFlash4_Timer()
'	dim flashx3
'	If not FlasherFlash4.TimerEnabled Then 
'		FlasherFlash4.TimerEnabled = True
'		FlasherFlash4.visible = 1
'		Flasha1.visible = 1
'		Flasha2.visible = 1
'		If VRRoom > 0 Then
'			VRBGFL32_1.visible = 1
'			VRBGFL32_2.visible = 1
'			VRBGFL32_3.visible = 1
'			VRBGFL32_4.visible = 1
'		End If
'	End If
'	flashx3 = (FlashLevel4 * FlashLevel4 * FlashLevel4)
'	Flasherflash4.opacity = 800 * flashx3
'	FlasherBase4.BlendDisableLighting = 10 * matdim4^2
'	FlasherLight4.IntensityScale = flashx3^2
'	flasha1.IntensityScale = 2 * flashx3^2
'	flasha2.IntensityScale = 1 * flashx3^2
'	matdim4 = matdim4 - 1
'	If VRRoom > 0 Then
'		VRBGFL32_1.opacity = 100 * flashx3^1.5
'		VRBGFL32_2.opacity = 100 * flashx3^1.5
'		VRBGFL32_3.opacity = 100 * flashx3^1.5
'		VRBGFL32_4.opacity = 100 * flashx3^2
'	End If	
'	FlashLevel4 = FlashLevel4 * 0.85 - 0.01
'	If matdim4 = 0 Then
'		FlasherBase4.BlendDisableLighting = 1
'		Flasherlight4.IntensityScale = 0
'		matdim4 = 9
'		FlasherFlash4.visible = 0
'		Flasha1.visible = 0
'		Flasha2.visible = 0
'		If VRRoom > 0 Then
'			VRBGFL32_1.visible = 0
'			VRBGFL32_2.visible = 0
'			VRBGFL32_3.visible = 0
'			VRBGFL32_4.visible = 0
'		End If
'		FlasherFlash4.TimerEnabled = False
'	End If
'End Sub



'******************* Preload **********************

'Dim GIInit: GIInit=10 * 4
'sub preloadTimer_Timer
'	If Preload = 1 and GIInit > 0 Then
'		GIInit = GIInit -1
'		select case (GIInit \ 4) ' Divide by 4, this is not a frame timer, so we want to be sure frame is visible 
'			case 0:
'				playfieldOff.opacity = 100
'				SidesBack.image = "Backtest"
'				Rampe3.image = "rampe3GIOFF"
'				Rampe2.image = "rampe2GIOFF"
'				Rampe1.image = "rampe1GIOFF"
'				Plastics.image = "plasticsOff"
'				Rplast01.image = "Rplastic01off"
'				preload = 0:preloadTimer.enabled = 0
'			case 1:
'				SidesBack.image = "Backtest"
'				Rampe3.image = "rampe3GIOFF"
'				Rampe2.image = "rampe2GIOFF"
'				Rampe1.image = "rampe1GIOFF"
'				Plastics.image = "plasticsOff"
'				Rplast01.image = "Rplastic01off"
'				PlayfieldOff.opacity = 100
'
'			case 2:
'				SidesBack.image = "Backtest"
'
'				Rampe3.image = "rampe3GIOFF"
'				Rampe2.image = "rampe2GIOFF"
'				Rampe1.image = "rampe1GIOFF"
'				Plastics.image = "plasticsOff"
'				Rplast01.image = "Rplastic01off"
'				PlayfieldOff.opacity = 100
'			case 3:
'				SidesBack.image = "Backtest"
'				Rampe3.image = "rampe3GIOFF"
'				Rampe2.image = "rampe2GIOFF"
'				Rampe1.image = "rampe1GIOFF"
'				Plastics.image = "plasticsOff"
'				Rplast01.image = "Rplastic01off"
'				PlayfieldOff.opacity = 100
'			case 4:
'				SidesBack.image = "Backtest"
'				Rampe3.image = "rampe3GIOFF"
'				Rampe2.image = "rampe2GIOFF"
'				Rampe1.image = "rampe1GIOFF"
'				Plastics.image = "plasticsOff"
'				Rplast01.image = "Rplastic01off"
'				PlayfieldOff.opacity = 100
'			case 5:
'				SidesBack.image = "BacktestOn"
'				SidesBack.image = "Backtest"
'				Rampe3.image = "rampe3GION"
'				Rampe2.image = "rampe2GION"
'				Rampe1.image = "rampe1GION"
'				Plastics.image = "plasticsON"
'				Rplast01.image = "Rplastic01on"
'				PlayfieldOff.opacity = 100
'			case 6:
'				SidesBack.image = "BacktestOn"
'				SidesBack.image = "Backtest"
'				Rampe3.image = "rampe3GION"
'				Rampe2.image = "rampe2GION"
'				Rampe1.image = "rampe1GION"
'				Plastics.image = "plasticsON"
'				Rplast01.image = "Rplastic01on"
'				PlayfieldOff.opacity = 100
'			case 7:
'				SidesBack.image = "BacktestOn"
'				SidesBack.image = "Backtest"
'				Rampe3.image = "rampe3GION"
'				Rampe2.image = "rampe2GION"
'				Rampe1.image = "rampe1GION"
'				Plastics.image = "plasticsON"
'				Rplast01.image = "Rplastic01on"
'				playfieldOff.opacity = 80
'			case 8:
'				SidesBack.image = "BacktestOn"
'				SidesBack.image = "Backtest"
'				Rampe3.image = "rampe3GION"
'				Rampe2.image = "rampe2GION"
'				Rampe1.image = "rampe1GION"
'				Plastics.image = "plasticsON"
'				Rplast01.image = "Rplastic01on"
'				PlayfieldOff.opacity = 40
'			case 9:
'				SidesBack.image = "Backtest"
'				Plastics.blenddisablelighting = 0
'				BumperA.blenddisableLighting = 0.5
'				BumperA001.blenddisableLighting = 0.5
'				BumperA002.blenddisableLighting = 0.5
'				Rampe3.blenddisableLighting = 0.5
'				Rampe2.blenddisableLighting = 0.3
'				Rampe1.blenddisableLighting = 0.1
'				VisA.blenddisablelighting = 0
'				VisB.blenddisablelighting = 0
'				MurMetal.blenddisablelighting = 0
'				MetalParts.blenddisablelighting = 0
'				spotlight.blenddisablelighting = 0.5
''				spotlightLight.blenddisablelighting = 0.5
'				PegsBoverSlings.blenddisablelighting = 0
'				Rampe3.image = "rampe3GIOFF"
'				Rampe2.image = "rampe2GIOFF"
'				Rampe1.image = "rampe1GIOFF"
'				Apron.blenddisablelighting = 0
'				ApronCachePlunger.blenddisablelighting = 0
'				Plastics.image = "plasticsOff"
'				Rplast01.image = "Rplastic01off"
'		end select 
'	End If
'End Sub


'******************* LUT CHoice **************************

Sub SetLUT
	Select Case LUTmeUP
		Case 0:goldeneye.ColorGradeImage = 0
		Case 1:goldeneye.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
		Case 2:goldeneye.ColorGradeImage = "LUT1_1_09"
		Case 3:goldeneye.ColorGradeImage = "LUTbassgeige1"
		Case 4:goldeneye.ColorGradeImage = "LUTbassgeige2"
		Case 5:goldeneye.ColorGradeImage = "LUTbassgeigemeddark"
		Case 6:goldeneye.ColorGradeImage = "LUTfleep"
		Case 7:goldeneye.ColorGradeImage = "LUTmandolin"
		Case 8:goldeneye.ColorGradeImage = "LUTVogliadicane70"
		Case 9:goldeneye.ColorGradeImage = "LUTchucky4"
	end Select
	WriteLUT 'Write LUT each time you change it
end sub 

'***************** Writing file for LUT's memory JPJ **********
sub WriteLUT
	Set File = FileObj.createTextFile(UserDirectory & "GoldeneyeLUT.txt",True) 'write new file, and delete one if it exists
	File.writeline(LUTmeUP)
	File.close
End sub
'**********************************************************






'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.99	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


Sub FrameTimer_Timer()
	CollisionTimer_Timer
	RealTime_Timer
	LampTimer2_Timer
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows	'update ball shadows
End Sub


Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10,BallShadowA11)

DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		iii = iii + 1
	Next
	numberofsources = iii
	numberofsources_hold = iii
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, currentMat, AnotherSource, BOT, iii
	BOT = GetBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(BOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If BOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + BallSize/10 + fovY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + fovY
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If BOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(s).Y = BOT(s).Y + Ballsize/10 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 Then 'And BOT(s).Y < (TableHeight - 200) Then 'Or BOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				For iii = 0 to numberofsources - 1 
					LSd=DistanceFast((BOT(s).x-DSSources(iii)(0)),(BOT(s).y-DSSources(iii)(1)))	'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then						    'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = iii
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-ShadowOpacity)
							End If

						Elseif currentShadowCount(s) = 2 Then
																	'Same logic as 1 shadow, but twice
							currentMat = objrtx1(s).material
							AnotherSource = sourcenames(s)
							objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-DistanceFast((BOT(s).x-DSSources(AnotherSource)(0)),(BOT(s).y-DSSources(AnotherSource)(1))))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
	'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx2(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity2 = (falloff-LSd)/falloff
							objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2))
							End If
						end if
					Else
						currentShadowCount(s) = 0
						BallShadowA(s).Opacity = 100*AmbientBSFactor
					End If
				Next
			Else									'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************



'*********************** Ramps Sounds *************************
sub Wall018_hit()
	Stopsound "fx_metalrolling0"
	'playsound "MetalRampHit1", 0, (VolPlayfieldRoll(ActiveBall)*10), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

sub Wall017_hit()
	Stopsound "fx_metalrolling2"
	'playsound "MetalRampHit2", 0, (VolPlayfieldRoll(ActiveBall)*10), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

'********************** NFozzy Flipper ************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
	Next

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -5.5
	AddPt "Polarity", 2, 0.4, -5.5
	AddPt "Polarity", 3, 0.6, -5.0
	AddPt "Polarity", 4, 0.65, -4.5
	AddPt "Polarity", 5, 0.7, -4.0
	AddPt "Polarity", 6, 0.75, -3.5
	AddPt "Polarity", 7, 0.8, -3.0
	AddPt "Polarity", 8, 0.85, -2.5
	AddPt "Polarity", 9, 0.9,-2.0
	AddPt "Polarity", 10, 0.95, -1.5
	AddPt "Polarity", 11, 1, -1.0
	AddPt "Polarity", 12, 1.05, -0.5
	AddPt "Polarity", 13, 1.1, 0
	AddPt "Polarity", 14, 1.3, 0

	addpt "Velocity", 0, 0,         1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41,         1.05
	addpt "Velocity", 3, 0.53,         1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03,         0.945

	LF.Object = LeftFlipper        
	LF.EndPoint = EndPointLp
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub



'******************************************************
'                        FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
				case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
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
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				:                         VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'playsound "fx_knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
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
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function


'******************************************************
'                        FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim BOT, b

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			BOT = GetBalls
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.7
					BOT(b).vely = BOT(b).vely - 1
				end If
			Next
		End If
	Else 
		If Flipper1.currentangle <> EndAngle1 then 
			EOSNudge1 = 0
		end if
	End If
End Sub

'*****************
' Maths
'*****************
Const PI = 3.1415927

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

'*************************************************
' Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function DistanceFast(x, y)
	dim ratio, ax, ay
	ax = abs(x)					'Get absolute value of each vector
	ay = abs(y)
	ratio = 1 / max(ax, ay)		'Create a ratio
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then			'Quickly determine if it's worth using
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function

' Used for drop targets and stand up targets
Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

'*************************************************
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025

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
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim BOT, b
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST        
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub


'****************************************************************************
'PHYSICS DAMPENERS
'****************************************************************************

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handle here
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class



'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************

dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	AddSlingsPt 0, 0.00,	-7
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	7

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub


'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************

Class SlingshotCorrection
	Public DebugOn, Enabled
	private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2

	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): Enabled = True : End Sub 

	Public Property let Object(aInput) : Set Slingshot = aInput : End Property
	Public Property Let EndPoint1(aInput) : SlingX1 = aInput.x: SlingY1 = aInput.y: End Property
	Public Property Let EndPoint2(aInput) : SlingX2 = aInput.x: SlingY2 = aInput.y: End Property

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 then Report
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		If not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


	Public Sub VelocityCorrect(aBall)
		dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then 
			XL = SlingX1 : YL = SlingY1 : XR = SlingX2 : YR = SlingY2
		Else
			XL = SlingX2 : YL = SlingY2 : XR = SlingX1 : YR = SlingY1
		End If

		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then 
			If ABS(XR-XL) > ABS(YR-YL) Then 
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If

		'Velocity angle correction
		If not IsEmpty(ModIn(0) ) then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
'			debug.print " BallPos=" & BallPos &" Angle=" & Angle 
'			debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
'			debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
'			debug.print " " 
		End If
	End Sub

End Class


'******************************************************
'                TRACK ALL BALL VELOCITIES
'                 FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()        'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)        'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)        'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)        'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

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


Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function


'Sub RDampen_Timer()
'	Cor.Update
'End Sub

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


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
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
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
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
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
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


' *********************************************************************
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

	If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
	Else
		AudioFade = Csng(-((- tmp) ^10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

	If tmp > 0 Then
		AudioPan = Csng(tmp ^10)
	Else
		AudioPan = Csng(-((- tmp) ^10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
	RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
	RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
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
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then		
		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If	
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then
		RandomSoundRubberStrong 1 
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If	
End Sub

Sub RandomSoundWall()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
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
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()		
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
		RandomSoundTargetHitWeak()
	End If	
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound	
	TargetBouncer Activeball, 1
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()			
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)	
	SoundPlayfieldGate	
End Sub	

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)

	'Release ball in radar if there is a collision with another ball
	If BallInRadarLock Then
		If ball1.id = RadarBall.id or ball2.id = RadarBall.id Then SolRadarMagnet False
	End If

End Sub


'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315									'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05									'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025*RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025*RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025*RelayFlashSoundLevel, obj			
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025*RelayFlashSoundLevel, obj		
	End Select
End Sub


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////


'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************


Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

sub TargetBouncer(aBall,defvalue)
	dim zMultiplier, vel, vratio
	if TargetBouncerEnabled = 1 and aball.z < 30 then
		'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
	if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
			Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
			Case 5: zMultiplier = 0.45*defvalue
			Case 6: zMultiplier = 0.5*defvalue
		End Select
		aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
		aBall.vely = aBall.velx * vratio
		'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
		elseif TargetBouncerEnabled = 2 and aball.z < 30 then
		if aball.vely > 3 then	'only hard hits
			'debug.print "velz: " & activeball.velz
			Select Case Int(Rnd * 4) + 1
				Case 1: zMultiplier = defvalue+1.1
				Case 2: zMultiplier = defvalue+1.05
				Case 3: zMultiplier = defvalue+0.7
				Case 4: zMultiplier = defvalue+0.3
			End Select
			aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
			'debug.print "----> velz: " & activeball.velz
			'debug.print "conservation check: " & BallSpeed(aBall)/vel
		End If
	end if
end sub



'******************************************************
'*******	VR stuff                      	*******
'******************************************************

DIM VRThings
If VRRoom > 0 Then
	SetBackglass
	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
	End If
'	If VRRoom = 2 Then
'		for each VRThings in VR_Cab:VRThings.visible = 1:Next
'		for each VRThings in VR_Min:VRThings.visible = 1:Next
'	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		PinCab_Backbox.visible = 1
		VRBGSpeaker.visible = 1
		bgdark.visible = 1
		DMD.visible = 1
	End If
Else
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
'	for each VRThings in VR_360:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
End If
If desktopmode Then PinCab_Metal_LRF.visible=1

'******************************************************
'*******	Set Up VR Backglass Flashers	*******
'******************************************************

Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglass
		obj.x = obj.x + 3
		obj.height = - obj.y + 300
		obj.y = -35 'adjusts the distance from the backglass towards the user
		obj.rotx=-86.5
	Next

	For Each obj In VRSpeaker
		obj.x = obj.x + 3
		obj.height = - obj.y + 280
		obj.y = 38 'adjusts the distance from the backglass towards the user
		obj.rotx=-86.5
	Next

	For Each obj In VRSpeakerLights
		obj.x = obj.x + 3
		obj.height = - obj.y + 280
		obj.y = 46 'adjusts the distance from the backglass towards the user
		obj.rotx=-86.5
	Next
End Sub

' *****************************************************
'      LAMP CALLBACK
' *****************************************************

Set LampCallback = GetRef("UpdateMultipleLamps")

Sub UpdateMultipleLamps()
  IF VRRoom > 0 Then
	If Controller.Lamp(73) = 0 Then: SpeakerG.visible=0: else: SpeakerG.visible=1
	If Controller.Lamp(74) = 0 Then: SpeakerO.visible=0: else: SpeakerO.visible=1
	If Controller.Lamp(75) = 0 Then: SpeakerL.visible=0: else: SpeakerL.visible=1
	If Controller.Lamp(76) = 0 Then: SpeakerD.visible=0: else: SpeakerD.visible=1
	If Controller.Lamp(77) = 0 Then: SpeakerE1.visible=0: else: SpeakerE1.visible=1
	If Controller.Lamp(78) = 0 Then: SpeakerN.visible=0: else: SpeakerN.visible=1
	If Controller.Lamp(79) = 0 Then: SpeakerE2.visible=0: else: SpeakerE2.visible=1
	If Controller.Lamp(80) = 0 Then: SpeakerY.visible=0: else: SpeakerY.visible=1
	If Controller.Lamp(72) = 0 Then: SpeakerE3.visible=0: else: SpeakerE3.visible=1
  End If
End Sub

