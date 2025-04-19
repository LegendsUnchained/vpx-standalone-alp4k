Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const BallSize = 51.8
Const BallMass = 1.20

'*********************************************************************************************************
'***                                    to show dmd in desktop Mod                                     ***
Dim UseVPMDMD, DesktopMode

UseVPMDMD = False
DesktopMode = True

'DesktopMode = Goldeneye.ShowDT  
'If NOT DesktopMode Then 
'	UseVPMDMD = False		'hides the internal VPMDMD when using the color ROM or when table is in Full Screen and color ROM is not in use
'	SidesMetal.visible = False'put True or False if you want to see or not the wood side in fullscreen mod
'	LockDown.visible = False
'end if 
'If DesktopMode Then UseVPMDMD = True							'shows the internal VPMDMD when in desktop mode 
'*********************************************************************************************************

 
LoadVPM "01200100","SEGA2.VBS",3.1

Const cGameName="gldneye",UseLamps=0,UseGI=0 ',UseSolenoids=2 already in vbs file
Const SSolenoidOn="solon",SSolenoidOff="soloff",sCoin="coin3" ',SFlipperOn="FlipperUp",SFlipperOff="FlipperDown"    '


' ***************** Some volumes *******************

Const VolDiv = 4000	'Lower number, louder ballrolling/collition sound
Const VolCol = 10 	'Ball collition divider ( voldiv/volcol )
Const RolVol = 2		'Ball Rolling
Const MroVol = 9		'Wire Ramps Rolling
Const ProVol = 0.7		'Plastic Ramps Rolling



'**********************************************************
'*               if you don't have SSF                    *
'*      Put 1 to up the magnet Volumes else put 0         *
'**********************************************************
dim MagnetVolMax, MagnetVol
MagnetVolMax = 1 ' 0 (SSF) or 1 (without SSF)
MagnetVol = 1 'level of magnet sounds for non SSF users
'**********************************************************

'********************************************************
'*  LUT Options - Script Taken from The Flintstones     *
'* All lut examples taken from The Flintstones and TOTAN*
'*           Big Thanks for all the samples             *
'********************************************************
Dim LUTmeUP:'LUTMeUp = 1 '0 = No LUT Initialized in the memory procedure
Dim DisableLUTSelector:DisableLUTSelector = 1  ' Disables the ability to change LUT option with magna saves in game when set to 1
Const MaxLut = 9
'********************************************************

'********************************************************
'*                 Fantasy colored magnet               *
'********************************************************
Dim FantasyMagnet
FantasyMagnet = 0 '1 For Fantasy colored magnet, 0 for nothing
'********************************************************


Dim preload
Preload = 1

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
    vpminit me
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1
	SatTop.IsDropped=1
	SatTop1.IsDropped=1
	SatTop2.IsDropped=1
	On Error Resume Next
		With Controller
			.GameName=cGameName
             .Games(cGameName).Settings.Value("rol")=0
			 .Games(cGameName).Settings.Value("sound")=1
			If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
			.SplashInfoLine="007 Goldeneye - Sega, 1996"&vbNewLine&"Initial Table Release by Pingod & Kid Charlemagne"&vbNewLine&"Mod by UncleReamus & Destruk"&vbNewLine&"Further Modding by The Trout"
			.HandleMechanics=0
			.HandleKeyboard=0
			.ShowDMDOnly=1
			.ShowFrame=0
			.ShowTitle=0
			.Run
			End With
			If Err Then MsgBox Err.Description
	On Error Goto 0
	

	Gate3.collidable = 0


		lightdir = -0.1:Textlight.Enabled = 1
		For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"
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
		.InitMagnet FlipperMagnet, 20 '10
		.GrabCenter = True
		.CreateEvents "mFlipperMagnet"
	End With

	Set RadarMagnet=New cvpmMagnet
		RadarMagnet.initMagnet RMagnet,40 '20
		'RadarMagnet.GrabCenter = True

	Set bsTrough=New cvpmBallStack
		bsTrough.InitSw 0,14,13,12,11,10,0,0
		bsTrough.InitKick BallRelease,40,8
		bsTrough.InitExitSnd SoundFX("BallRelease",DOFContactors),SoundFX("Solon",DOFContactors)
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
		bsPlunger.InitSaucer Plunger,16,0,45
		bsPlunger.InitExitSnd SoundFX("Solon",DOFContactors),SoundFX("SolOn",DOFContactors)
		bsPlunger.KickForceVar=10
		
		 Controller.Switch(63)=0
		 Controller.Switch(64)=0

	for each xx in DL:
		xx.blendDisableLighting = 0
		playfieldOff.opacity = 100
	Next

		playfieldOff.opacity = 100
		SidesBack.image = "Backtest"
		Plastics.blenddisablelighting = 0
		BumperA.blenddisableLighting = 0.1
		BumperA001.blenddisableLighting = 0.1
		BumperA002.blenddisableLighting = 0.1
		Rampe3.blenddisableLighting = 0.2
		Rampe2.blenddisableLighting = 0.2
		Rampe1.blenddisableLighting = 0.1
		VisA.blenddisablelighting = 0
		VisB.blenddisablelighting = 0
		MurMetal.blenddisablelighting = 0
		MetalParts.blenddisablelighting = 0
		spotlight.blenddisablelighting = 0.5
		spotlightLight.blenddisablelighting = 0
		PegsBoverSlings.blenddisablelighting = 0
		Rampe3.image = "rampe3GIOFF"
		Rampe2.image = "rampe2GIOFF"
		Rampe1.image = "rampe1GIOFF"
		Apron.blenddisablelighting = 0
		ApronCachePlunger.blenddisablelighting = 0
		MrampB.blenddisablelighting = 0
		Plastics.image = "plasticsOff"
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
	Ledrouge.Amount = 100:LedRouge.IntensityScale = 0
	LedBleu1.Amount = 100:LedBleu1.IntensityScale = 0
	LedBleu2.Amount = 100:LedBleu2.IntensityScale = 0
	LedFond.Amount = 100:LedFond.IntensityScale = 0


	SetLUT
Wall020.isdropped = 1
End Sub

'*************************************************


SolCallback(1)="bsTrough.SolOut"
SolCallback(2)="bsPlunger.SolOut"
SolCallback(4)="bsScoop.SolOut"
SolCallback(8)="vpmSolSound SoundFX(""knocker"",DOFKnocker),"
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

SolCallback(25)="flashtest"	
SolCallback(26)="flashtest2"
SolCallback(27)="flashtest3"
SolCallback(28)="flashtest4"
SolCallback(29)="flashtest8"
SolCallback(30)="flashtest5"
SolCallback(31)="flashtest6"
SolCallback(32)="flashtest7 "
SolCallback(33)="RadarMagnet.MagnetOn="
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
		Light001.state = 2
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
				.VelX = 4 * Sin(dir): .VelY = speed * Cos(dir)
			PlaySound "fx_balldrop"
		End With
	Next
	vpmTimer.PulseSw 24:
	mFlipperMagnet.MagnetOn = False:	mFlipperMagnet.GrabCenter = True:mdof = 0
	Me.TimerEnabled = False
End Sub
'******************************************************

'****************** Radar Magnet **********************
dim balltrap


Sub RMagnet_Hit
	Controller.Switch(23)=1
Wall020.isdropped = 0
		if MagnetVolMax = 1 then
				PlaySound "fx_magnetR",-1,MagnetVol
		else
			PlaySound "fx_magnetR",-1
		end if
		ActiveBall.velX=0
		ActiveBall.velY=0
		ActiveBall.velZ=0
		ActiveBall.X=633
		ActiveBall.Y=785
		ActiveBall.Z=155
	PlaySoundAtVol "magnethit", ActiveBall, 1
		set balltrap = Activeball
		Balltrap.visible = 0
		BallInSat.visible = 1
		BallinRadar.enabled = 1
	RadarMagnet.AddBall ActiveBall
SatBallLight.state = 2
End Sub	

Sub RMagnet_unHit
	Controller.Switch(23)=0
Wall020.isdropped = 1
	RadarMagnet.RemoveBall ActiveBall
		BallInSat.visible = 0
		BallinRadar.enabled = 0
		Balltrap.visible = 1
		ActiveBall.velZ=-6:ActiveBall.VelX = -(RadarB.objrotz):ActiveBall.VelY = 1 
	StopSound "fx_magnetR"
SatBallLight.state = 0
End Sub

dim rottz, rotty, rottx
	
Sub BallInRadar_Timer()
	BallInSat.RotX = BallInSat.RotX +3
	BallInSat.RotY = BallInSat.RotY +5
	BallInSat.RotZ = BallInSat.RotZ -13
End Sub
'******************************************************


Sub SolLFlipper(Enabled)
     If Enabled Then
'		If FL = 1 then
			Controller.Switch(63)=1
			PlaySound SoundFXDOF("FlipperUpL",101,DOFOn,DOFFlippers)
			LeftFlipper.EOSTorque = 0.75
			LeftFlipper.RotateToEnd
'			PlaySoundAtVol "FlipperUpL2", LeftFlipper, 1 
			PlaySoundAtVol SoundFX("FlipperUpL2",DOFContactors), LeftFlipper, 1
'		End if
	Else
			Controller.Switch(63)=0
			PlaySound SoundFXDOF("flipperdown",101,DOFOff,DOFFlippers)
			LeftFlipper.EOSTorque = 0.15
			LeftFlipper.RotateToStart
     End If
End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
'		If FR = 1 then
			Controller.Switch(64)=1
			PlaySound SoundFXDOF("FlipperUpR",102,DOFOn,DOFFlippers)
			RightFlipper.EOSTorque = 0.70
			RightFlipper.RotateToEnd
'			PlaySoundAtVol "FlipperUpR2", RightFlipper, 1
			PlaySoundAtVol SoundFX("FlipperUpR2",DOFContactors), RightFlipper, 1
'		End if
     Else
			Controller.Switch(64)=0
			PlaySound SoundFXDOF("flipperdown",102,DOFOff,DOFFlippers)
		 	RightFlipper.EOSTorque = 0.15
			RightFlipper.RotateToStart
     End If
End Sub

Sub TiltMod(Enabled)
	If true then 
		go = 0
	Else
		go = 1
	end if
End Sub



Sub sw16_Hit:Controller.Switch(16) = 1 : PlaySoundAtVol "rollover", ActiveBall, 1 : End Sub 
Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub
Sub sw49_Hit:Controller.Switch(49) = 1 : PlaySoundAtVol "rollover", ActiveBall, 1 : End Sub 
Sub sw49_UnHit:Controller.Switch(49) = 0:End Sub

Sub sw51_Hit:Controller.Switch(51) = 1 : PlaySoundAtVol "rollover", ActiveBall, 1 : End Sub 
Sub sw51_UnHit:Controller.Switch(51) = 0:End Sub
Sub sw52_Hit:Controller.Switch(52) = 1 : PlaySoundAtVol "rollover", ActiveBall, 1 : End Sub 
Sub sw52_UnHit:Controller.Switch(52) = 0:End Sub
Sub sw53_Hit:Controller.Switch(53) = 1 : PlaySoundAtVol "rollover", ActiveBall, 1 : End Sub 
Sub sw53_UnHit:Controller.Switch(53) = 0:End Sub


Sub sw55_Hit:Controller.Switch(55) = 1 : PlaySoundAtVol "rollover", ActiveBall, 1 : End Sub 
Sub sw55_UnHit:Controller.Switch(55) = 0:End Sub
Sub sw57_Hit:Controller.Switch(57) = 1 : PlaySoundAtVol "rollover", ActiveBall, 1 : End Sub 
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
Sub sw58_Hit:Controller.Switch(58) = 1 : PlaySoundAtVol "rollover", ActiveBall, 1 : End Sub 
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
Sub sw59_Hit:Controller.Switch(59) = 1 : PlaySoundAtVol "rollover", ActiveBall, 1 : End Sub 
Sub sw59_UnHit:Controller.Switch(59) = 0:End Sub
Sub sw60_Hit:Controller.Switch(60) = 1 : PlaySoundAtVol "rollover", ActiveBall, 1 : End Sub 
Sub sw60_UnHit:Controller.Switch(60) = 0:End Sub

sub GateSw17_Hit:vpmTimer.PulseSw 17: End Sub
sub GateSw18_Hit:vpmTimer.PulseSw 18: End Sub
sub GateSw19_Hit:vpmTimer.PulseSw 19: End Sub
sub GateSw32_Hit:vpmTimer.PulseSw 32: End Sub
sub GateSw40_Hit:vpmTimer.PulseSw 40: End Sub
sub GateSw54_Hit:vpmTimer.PulseSw 54: End Sub


Sub flashtest(enabled)
	if enabled = true then
		Flash002.state = 1
		Flash001.state = 1
		flasha001.Amount = 100
		flasha001.IntensityScale = 8
		flasha001B.Amount = 100
		flasha001B.IntensityScale = 8
	else
		Flash002.state = 0
		Flash001.state = 0
		flasha001.Amount = 0
		flasha001.IntensityScale = 0
		flasha001B.Amount = 0
		flasha001B.IntensityScale = 0
	end if
End Sub

Sub flashtest2(enabled)
	if enabled = true then
		LL15F.state = 1
		flasha003.Amount = 50
		flasha003.IntensityScale = 50
	else
		LL15F.state = 0
		flasha003.Amount = 0
		flasha003.IntensityScale = 0
	end if
End Sub

Sub flashtest3(enabled)
	if enabled = true then
		Flash003A.state = 1
		Flash003B.state = 1
		flasha002.Amount = 100
		flasha002.IntensityScale = 10
	else
		Flash003A.state = 0
		Flash003B.state = 0
		flasha002.Amount = 0
		flasha002.IntensityScale = 0
	end if
End Sub

Sub flashtest4(enabled)
if enabled = true then
		Flash004.state = 1
		flasha0004.Amount = 100
		flasha0004.IntensityScale = 10
	else
		Flash004.state = 0
		flasha0004.Amount = 0
		flasha0004.IntensityScale = 0
	end if
End Sub

Sub flashtest5(enabled)
	if enabled = true then
		matdim1 = 9
		FlashLevel1 = 1:FlasherFlash1_Timer
			flasha3.Amount = 100
			flasha3.IntensityScale = 10
			flasha4.Amount = 60
			flasha4.IntensityScale = 6
	else
			flasha3.Amount = 0
			flasha3.IntensityScale = 0
			flasha4.Amount = 0
			flasha4.IntensityScale = 0
		FlashLevel1 = 0:FlasherFlash1_Timer
	end if

End Sub

Sub flashtest6(enabled)
if enabled = true then
		rampflash4.state = 1
		Flash003.state = 1
	else
		rampflash4.state = 0
		Flash003.state = 0
	end if
End Sub

Sub flashtest7(enabled)
	if enabled = true then
		matdim3 = 9:matdim2 = 9
		FlashLevel3 = 1:FlasherFlash3_Timer
		FlashLevel2 = 1:FlasherFlash2_Timer
			flasha7.Amount = 50
			flasha7.IntensityScale = 5
			flasha8.Amount = 50
			flasha8.IntensityScale = 3
			flasha9.Amount = 60
			flasha9.IntensityScale = 4
			flasha5.Amount = 40
			flasha5.IntensityScale = 4
			flasha6.Amount = 60
			flasha6.IntensityScale = 4
	else
		FlashLevel3 = 0:FlasherFlash3_Timer
		FlashLevel2 = 1:FlasherFlash2_Timer
			flasha7.Amount = 0
			flasha7.IntensityScale = 0
			flasha8.Amount = 0
			flasha8.IntensityScale = 0
			flasha9.Amount = 0
			flasha9.IntensityScale = 0
			flasha5.Amount = 0
			flasha5.IntensityScale = 0
			flasha6.Amount = 0
			flasha6.IntensityScale = 0
	end if
End Sub

Sub flashtest8(enabled)
	if enabled = true then
		matdim4 = 9
		FlashLevel4 = 1:FlasherFlash4_Timer
			flasha1.Amount = 100
			flasha1.IntensityScale = 10
			flasha2.Amount = 60
			flasha2.IntensityScale = 6
	else
			flasha1.Amount = 0
			flasha1.IntensityScale = 0
			flasha2.Amount = 0
			flasha2.IntensityScale = 0
		FlashLevel4 = 0:FlasherFlash4_Timer
	end if
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
		RedLightA.blenddisableLighting = 5
		RedFA.Amount = 20
		RedFA.IntensityScale = 1
	Else
		RedLightA.blenddisableLighting = 0
		RedFA.Amount = 0
		RedFA.IntensityScale = 0
	End If
	If ll51.state = 1 then 
		RedLightB.blenddisableLighting = 5
		RedFB.Amount = 20
		RedFB.IntensityScale = 1
	Else
		RedLightB.blenddisableLighting = 0
		RedFB.Amount = 0
		RedFB.IntensityScale = 0
	End If
	If ll34.state = 1 then 
		GreenLight.blenddisableLighting = 20
		GreenF.Amount = 30
		GreenF.IntensityScale = 2
	Else
		GreenLight.blenddisableLighting = 0.3
		GreenF.Amount = 0
		GreenF.IntensityScale = 0
	End If
	If ll49.state = 1 then 
	
		StopR = 0:rotorspeed = 10:Rotor.enabled = 1
	end if
	If ll69.state = 1 Then
		spotlightLight.blenddisablelighting = 3
		Flash69.Amount = 80
		flash69.IntensityScale = 30
	Else 
		spotlightLight.blenddisablelighting = 0
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
		lightdir = 0.1:Textlight.Enabled = 1
		For each xx in GI:xx.State = 1:	Next
        PlaySound "fx_relay"
		DOF 103, DOFOn
		If B2SOn Then
			Controller.B2SSetData 90, 1	
		End If
	Else
		lightdir = -0.1:Textlight.Enabled = 1
		For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"
		DOF 103, DOFOff
		If B2SOn Then
			Controller.B2SSetData 90, 0	
		End If
	End If
End Sub


Sub Textlight_timer
	light=light+(lightdir) '/2 si 0,5
	if lightdir = 0.1 then pflight = pflight - 10:end If
	if lightdir = -0.1 then pflight = pflight + 10:end If
	if pflight > 105 then pflight = 100
	if pflight < 0 then pflight = 0

	if lightdir = 0.1 and light > 1 then
				playfieldOff.opacity = 0
				SidesBack.image = "BacktestOn"
				Plastics.blenddisablelighting = 1
				MrampB.blenddisablelighting = 0.6
				BumperA.blenddisableLighting = 0.3
				BumperA001.blenddisableLighting = 0.3
				BumperA002.blenddisableLighting = 0.3
				Rampe3.blenddisableLighting = 9
				Rampe2.blenddisableLighting = 3
				Rampe1.blenddisableLighting = 1
				VisA.blenddisablelighting = 4
				VisB.blenddisablelighting = 4
				MurMetal.blenddisablelighting = 2
				MetalParts.blenddisablelighting = 2
				spotlight.blenddisablelighting = 1
				spotlightLight.blenddisablelighting = 3
				PegsBoverSlings.blenddisablelighting = 1
                SidesMetal.blenddisablelighting = 0.2
				Apron.blenddisablelighting = 0.4
				ApronCachePlunger.blenddisablelighting = 0.7
                BlackWoodWalls.image = "BlackWoodWalls"
				Ledrouge.Amount = 100:LedRouge.IntensityScale = 10
				LedBleu1.Amount = 100:LedBleu1.IntensityScale = 10
				LedBleu2.Amount = 100:LedBleu2.IntensityScale = 10
				LedFond.Amount = 100:LedFond.IntensityScale = 200
RadarA.blenddisablelighting = 2
LPannel.blenddisablelighting = 1
MPannel.blenddisablelighting = 1
RPannel.blenddisablelighting = 1.2
TankA.blenddisablelighting = 1.2
TankB.blenddisablelighting = 1.2
TankCTourelle.blenddisablelighting = 1.2
				light = 1
				me.enabled = 0
	End If
			
	if lightdir = -0.1 and light < 0 then 
				playfieldOff.opacity = 100
				SidesBack.image = "Backtest"
				MrampB.blenddisablelighting = 0
				Plastics.blenddisablelighting = 0
				BumperA.blenddisableLighting = 0.1
				BumperA001.blenddisableLighting = 0.1
				BumperA002.blenddisableLighting = 0.1
				Rampe3.blenddisableLighting = 0.2
				Rampe2.blenddisableLighting = 0.2
				Rampe1.blenddisableLighting = 0.1
				VisA.blenddisablelighting = 0
				VisB.blenddisablelighting = 0
				MurMetal.blenddisablelighting = 0
				MetalParts.blenddisablelighting = 0
				spotlight.blenddisablelighting = 0.5
				spotlightLight.blenddisablelighting = 0
				PegsBoverSlings.blenddisablelighting = 0
				Rampe3.image = "rampe3GIOFF"
				Rampe2.image = "rampe2GIOFF"
				Rampe1.image = "rampe1GIOFF"
                SidesMetal.blenddisablelighting = 0
				Apron.blenddisablelighting = 0
				ApronCachePlunger.blenddisablelighting = 0
				Plastics.image = "plasticsOff"
                BlackWoodWalls.image = "BlackWoodWallsoff"
				Ledrouge.Amount = 100:LedRouge.IntensityScale = 0
				LedBleu1.Amount = 100:LedBleu1.IntensityScale = 0
				LedBleu2.Amount = 100:LedBleu2.IntensityScale = 0
				LedFond.Amount = 100:LedFond.IntensityScale = 0
RadarA.blenddisablelighting = 0.3
LPannel.blenddisablelighting = 0.1
MPannel.blenddisablelighting = 0.1
RPannel.blenddisablelighting = 0.2
TankA.blenddisablelighting = 0.3
TankB.blenddisablelighting = 0.3
TankCTourelle.blenddisablelighting = 0.3
				light = 0
				me.enabled = 0
		Else
				Plastics.image = "plasticsOn"
				Rampe3.image = "rampe3GION"
				Rampe2.image = "rampe2GION"
				Rampe1.image = "rampe1GION"
	End If
	for each xx in DL:
		xx.blendDisableLighting = light 
		playfieldOff.opacity = pflight
	Next
End Sub

'***************************************************



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
LampTimer.Interval = 20 'lamp fading speed
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

Sub UpdateLamps
	NFadeL 1, LL1
	NFadeL 2, LL2
	NFadeL 3, LL3
	NFadeL 4, LL4
	NFadeL 5, LL5
	NFadeL 6, LL6
	NFadeL 7, LL7
	NFadeL 8, LL8
 '  NFadeL 9, LL9 Not in use
	NFadeL 10, LL10
	NFadeL 11, LL11
	NFadeL 12, LL12
	NFadeL 13, LL13
	NFadeL 14, LL14
	NFadeL 15, LL15
	NFadeL 16, LL16
	NFadeL 17, LL17
	NFadeL 18, LL18
	NFadeLm 19, LL19A
	NFadeLm 19, LL19B
	NFadeL 19, LL19 'Bumper
	NFadeL 20, LL20
	NFadeL 21, LL21
	NFadeL 22, LL22
	NFadeL 23, LL23
	NFadeL 24, LL24
	NFadeL 25, LL25
	NFadeL 26, LL26
	NFadeLm 27, LL27A
	NFadeLm 27, LL27B
	NFadeL 27, LL27 'Bumper
	NFadeL 28, LL28
	NFadeL 29, LL29
	NFadeL 30, LL30
	NFadeL 31, LL31
	NFadeL 32, LL32
	NFadeL 33, LL33
	NFadeL 34, LL34
	NFadeL 35, LL35
	NFadeL 36, LL36
	NFadeL 37, LL37
	NFadeL 38, LL38
	NFadeL 39, LL39
'	NFadeL 40, LL40 'Not in use 
	NFadeLm 41, LL41A
	NFadeLm 41, LL41B
	NFadeL 41, LL41 'Bumper
	NFadeL 42, LL42
	NFadeL 43, LL43
	NFadeL 44, LL44
	NFadeL 45, LL45
	NFadeL 46, LL46
	NFadeL 47, LL47
	NFadeL 48, LL48
	NFadeL 49, LL49 'Copter
	NFadeL 50, LL50
	NFadeL 51, LL51
	NFadeL 52, LL52
	NFadeL 53, LL53
	NFadeL 54, LL54
	NFadeL 55, LL55
	NFadeL 56, LL56
'	NFadeL 57, LL57 Start button
	NFadeLm 58, LL58
	NFadeL 58, LL58bis
	NFadeL 59, LL59
	NFadeL 60, LL60
	NFadeL 61, LL61
	NFadeL 62, LL62
	NFadeL 63, LL63
'	NFadeL 64, LL64
	NFadeL 65, LL65
	NFadeL 66, LL66
	NFadeL 67, LL67
	NFadeL 68, LL68
	NFadeL 69, LL69 'Copter Spotlight
'	NFadeL 70, LL70 Not in use
'	NFadeL 71, LL71 Not in use
	
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


'**********************************************************************



Sub SolLockOut(Enabled)
	If Enabled Then vpmTimer.PulseSw 15
End Sub

'********** Sling Shot Animations *******************************
' Rstep and Lstep  are the variables that increment the animation
'****************************************************************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    vpmTimer.PulseSw 62
    PlaySoundAtVol SoundFX("Rightsling",DOFContactors), sling1, 1
	RightSlingFlash.state =0
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -26
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    PlaySoundAtVol SoundFX("RightSlingSub",DOFContactors), sling1, 1
    PlaySoundAtVol SoundFX("RightSlingSub2",DOFContactors), sling1, 1
'    PlaySoundAtVol "RightSlingSub", SLING1, 1
'    PlaySoundAtVol "RightSlingSub2", SLING1, 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -16
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
	RightSlingFlash.state =1
	if RSLing1.Visible = 0 then RightSlingFlash.state =0:end If
End Sub

Sub LeftSlingShot_Slingshot
    vpmTimer.PulseSw 61
	PlaySoundAtVol SoundFX("Leftsling",DOFContactors), sling2, 1
	LeftSlingFlash.state = 0
	LSling.Visible = 0
	LSling1.Visible = 1
	sling2.TransZ = -26
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	PlaySoundAtVol SoundFX("LeftSlingSub",DOFContactors), sling2, 1
	PlaySoundAtVol SoundFX("LeftSlingSub2",DOFContactors), sling2, 1
'	PlaySoundAtVol "LeftSlingSub", sling2, 1
'	PlaySoundAtVol "LeftSlingSub2", sling2, 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -16
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
	LeftSlingFlash.state = 1
	if LSLing1.Visible = 0 then LeftSlingFlash.state = 0:end If
End Sub



Sub GoldenEye_Exit  '  in some tables this needs to be Table1_Exit
    Controller.Stop
End Sub


'**************** KEYS ***********************

Sub GoldenEye_KeyDown(ByVal Keycode)
	If KeyCode=StartGameKey Then
		Controller.Switch(3)=1
		Exit Sub
	End If
	If KeyCode=KeySlamDoorHit Then
		Controller.Switch(7)=1
		Exit Sub
	End If


	If KeyCode=PlungerKey Then Controller.Switch(9)=1
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
	If KeyCode=PlungerKey Then Controller.Switch(9)=0
	If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode = RightMagnaSave Then
			if DisableLUTSelector = 0 then
				LUTmeUP = LUTMeUp + 1
				if LutMeUp > MaxLut then LUTmeUP = 0
				SetLUT
				ShowLUT
			end if
		end if
		If keycode = LeftMagnaSave Then
			  if DisableLUTSelector = 0 then
				LUTmeUP = LUTMeUp - 1
				if LutMeUp < 0 then LUTmeUP = MaxLut
				SetLUT
				ShowLUT
			end if
	end if
End Sub


'***********************************************************************************
'*************                 Bumpers                               ***************
'***********************************************************************************
Sub LeftTurboBumper_Hit
    vpmTimer.PulseSw 41
	PlaySoundAtVol SoundFX("fx_bumper1",DOFContactors), ActiveBall, 1
	PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1
	Me.TimerEnabled = 1
'	PlaySoundAtVol "bumper", LeftTurboBumper, 1
End Sub

Sub LeftTurboBumper_Timer
	Me.Timerenabled = 0
End Sub

Sub BottomTurboBumper_Hit
    vpmTimer.PulseSw 42
	PlaySoundAtVol SoundFX("fx_bumper2",DOFContactors), ActiveBall, 1
	PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1
	Me.TimerEnabled = 1
'	PlaySoundAtVol "bumper", BottomTurboBumper, 1
End Sub

Sub BottomTurboBumper_Timer
	Me.Timerenabled = 0
End Sub

Sub RightTurboBumper_Hit
    vpmTimer.PulseSw 43
	PlaySoundAtVol SoundFX("fx_bumper3",DOFContactors), ActiveBall, 1
	PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1
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



Sub Drain_Hit:ClearBallID : bsTrough.AddBall Me:	PlaySoundAtVol "fx_drain", Drain, 1:End Sub
Sub Scoop_Hit:ClearBallID :bsScoop.AddBall Me:PlaySound "Scoopenter":End Sub
Sub TankKickBig_Hit:ClearBallID :bsTank.AddBall Me:End Sub
Sub DropRamp1_Hit:ClearBallID :bsTank.AddBall Me
	PlaySound "Kicker_enter"
End Sub
Sub Plunger_Hit:bsPlunger.Addball 0:PlungerLight.state = 2:End Sub
Sub Plunger_unHit:PlungerLight.state = 0:End Sub
'Sub Kicker1_Hit:bsLockOut.AddBall 0:End Sub


'***************** Radar ********************

Sub SolSatMotorRelay(enabled)		'rotate satelite
	if enabled then
		RotRadar.enabled = 1
	else
		RotRadar.enabled = 0
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
'	if radinit = 1 Then 
'		if RadarA.objrotz = 0 and RadarB.objrotz = 0 then 
'			radinit = 0
'			RotRadar.enabled = 0
'		End If
'	End If
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
RampSat3D.blenddisablelighting = 2
	Else
		RampDirection=15:RadarRampAnim.enabled = 1
		RampRadar.collidable=0
		SatTop.IsDropped=1
		SatTop1.IsDropped=1
		SatTop2.IsDropped=1
		PlaySoundAtVol "RadarRampOff", RampSat3D, 1
RampSat3D.blenddisablelighting = 1
	End If
End Sub

sub RadarRampAnim_Timer()
	RampSat3D.objrotx = RampSat3D.objrotx + RampDirection
	RampSat3Dv.objrotx =  RampSat3Dv.objrotx+ RampDirection
	RampC = RampC + 1
	if RampC = 3 and RampDirection=-15 then RampSat3D.objrotx = -45:RampSat3Dv.objrotx = -45:RampC = 0:me.enabled = 0:LL40.State = 1:end If
	if RampC = 3 and RampDirection=15 then RampSat3D.objrotx = 0:RampSat3Dv.objrotx = 0:RampC = 0:me.enabled = 0:LL40.State = 0:end If
End Sub
'**************************************************

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

Dim Xin, Yin, rAngle, Radit, wAngle, Pi
Pi = Round(4 * Atn(1), 6) '3.1415926535897932384626433832795

Sub GetAngle(Xin, Yin, wAngle)
    If Sgn(Xin) = 0 Then
        If Sgn(Yin) = 1 Then rAngle = 3 * Pi / 2 Else rAngle = Pi / 2
        If Sgn(Yin) = 0 Then rAngle = 0
        Else
            rAngle = atn(- Yin / Xin)
    End If
    If sgn(Xin) = -1 Then Radit = Pi Else Radit = 0
    If sgn(Xin) = 1 and sgn(Yin) = 1 Then Radit = 2 * Pi
    wAngle = round((Radit + rAngle), 4)
End Sub

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




' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX and Rothbauerw
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

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
    PlaySound sound, 0, ABS(BOT.velz)/17, AudioPan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
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
  tmp = tableobj.y * 2 / goldeneye.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.x * 2 / goldeneye.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
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
'    JP's VP10 Collision & Rolling Sounds
'*****************************************

Const tnob = 7 ' total number of balls
ReDim rolling(tnob)
ReDim collision(tnob)
Initcollision
Dim ballStatus(7), currentball(7)

Sub Initcollision
    Dim i
    For i = 0 to tnob
        collision(i) = -1
        rolling(i) = False
    Next
End Sub

Sub CollisionTimer_Timer()
    Dim BOT, B, B1, B2, dx, dy, dz, distance, radii
    BOT = GetBalls
'	TextBox001.Text = (UBound(BOT))+1
    ' rolling

	For B = UBound(BOT) +1 to tnob
        rolling(b) = False
		StopSound("fx_ballrolling" & b)
		StopSound("fx_plasticrolling" & b)
		StopSound("fx_metalrollingA" & b)
		StopSound("fx_metalrollingB" & b)
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
			PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*RolVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
		Else 
'Left Ramp
		If InRect(BOT(b).x, BOT(b).y, 48,214,321,105,200,805,101,840) And BOT(b).z < 24+27 And BOT(b).z > 27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 317,104,440,146,351,482,280,494) And BOT(b).z < 72+27 And BOT(b).z > 24+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 283,472,480,435,180,1422,25,1352) And BOT(b).z < 72+27 And BOT(b).z > 64+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_metalrollingA" & b), -1, Vol(BOT(b) )*3*MroVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 5,1343,189,1425,141,1625,17,1737) And BOT(b).z < 72+27 And BOT(b).z > 64+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
				PlaySound("fx_metalrollingB" & b), -1, Vol(BOT(b) )*3*MroVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
'Right Ramp
			ElseIf InRect(BOT(b).x, BOT(b).y, 710,426,870,338,845,1096,662,1000) And BOT(b).z < 150+27 And BOT(b).z > 27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 240,155,852,343,709,427,287,342) And BOT(b).z < 192+27 And BOT(b).z > 147+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 5,437,265,254,286,343,109,492) And BOT(b).z < 171+27 And BOT(b).z > 137+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 4,436,109,493,76,1220,5,1353) And BOT(b).z < 145+27 And BOT(b).z > 80+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
'Center Ramp
			ElseIf InRect(BOT(b).x, BOT(b).y, 154,294,245,327,321,805,217,823) And BOT(b).z < 72+27 And BOT(b).z > 27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 436,1,458,87,244,326,131,286) And BOT(b).z < 120+27 And BOT(b).z > 147+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 436,1,944,38,786,156,457,87) And BOT(b).z < 119+27 And BOT(b).z > 82+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 785,156,841,115,913,244,850,271) And BOT(b).z < 87+27 And BOT(b).z > 68+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*3*ProVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 850,271,935,234,940,1437,871,1427) And BOT(b).z < 72+27 And BOT(b).z > 66+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
				PlaySound("fx_metalrollingB" & b), -1, Vol(BOT(b) )*3*MroVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 870,1426,958,1440,779,1739,708,1635) And BOT(b).z < 72+27 And BOT(b).z > 66+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
				PlaySound("fx_metalrollingB" & b), -1, Vol(BOT(b) )*3*MroVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			end if
		End If

        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
				StopSound("fx_plasticrolling" & b)
				StopSound("fx_metalrollingA" & b)
				StopSound("fx_metalrollingB" & b)
                rolling(b) = False
            End If
        End If

'********************************************
'*** Ball Drop Sounds - Thx to RothBauerw ***
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 30 Then 'height adjust for ball drop sounds
			PlaySound "ball_bounce" & b, 0, ABS(BOT(b).velz)/10, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If
'********************************************

	Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySoundAtVol "target", ActiveBall, 1
End Sub

Sub Wood_Hit (idx)
	PlaySound "wood", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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

Sub Metal_Hit (idx)
	RandomSoundMetal()
End Sub

Sub RandomSoundMetal()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "MetalHit1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "MetalHit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "MetalHit3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub Gates_Hit (idx)
	RandomSoundGates()
End Sub

Sub RandomSoundGates()
	Select Case Int(Rnd*2)+1
		Case 1 : PlaySound "Gate", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "Gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub Gates2_Hit(idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Plastic_Hit(idx)
	PlaySound "fx_plastichit", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rollovers_Hit (idx)
	PlaySound "rollover", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Bumper_Hit (idx)
	RandomSoundBumper()
End Sub

Sub RandomSoundBumper()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtVol SoundFX("fx_bumper1",DOFContactors), ActiveBall, 1
		Case 2 : PlaySoundAtVol SoundFX("fx_bumper2",DOFContactors), ActiveBall, 1
		Case 3 : PlaySoundAtVol SoundFX("fx_bumper3",DOFContactors), ActiveBall, 1
		Case 4 : PlaySoundAtVol SoundFX("fx_bumper4",DOFContactors), ActiveBall, 1
	End Select
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 15 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 15 then
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


Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper(parm)
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper(parm)
End Sub

Sub RandomSoundFlipper(parm)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, parm / 10, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, parm / 10, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, parm / 10, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

' what you need in your table to use these flashers:
' copy the objects flasherbase, flasherlit, flasherlight and flasherflash from layer 1,2,3 and 4 to your table
' export the materials domebase, domelit0 - domelit9 and import them in your table
' copy the script below "Dim ... flasherlightx.IntensityScale = 0" and the sub for your flasher to your table
' for flashing the flasher use in the script: "FlashLevel1 = 1 : FlasherFlash1_Timer"
' this should also work for flashers with different levels from the rom, just use FlashLevel1 = xx from the rom (in the range 0-1)
'
' notes:
' - due to how the texture is made, you need to turn the flasher to face the player: 
'	The red flasher for instance has ObjRotZ = -10 for flasherbase and flasherlit
'	and the flasherflash has RotZ = -10
' - for the vertical flasher the flasherflash has RotZ = 180
' - the flasherbase and flasherlit primitive must be on the exact same x,y,z
' - the flasherflash could have a slightly different position for desktop vs FS view
' - for the colors yellow/purple/green, please replace the textures and light color on the four objects for the white flasher


Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4
FlasherLight1.IntensityScale = 0
Flasherlight2.IntensityScale = 0
Flasherlight3.IntensityScale = 0
Flasherlight4.IntensityScale = 0

''*** Yellow flasher 1***
Dim matdim1
Sub FlasherFlash1_Timer()
	dim flashx3
	If not FlasherFlash1.TimerEnabled Then 
		FlasherFlash1.TimerEnabled = True
		FlasherFlash1.visible = 1
	End If
	flashx3 = (FlashLevel1 * FlashLevel1 * FlashLevel1)
	Flasherflash1.opacity = 800 * flashx3
	FlasherBase1.BlendDisableLighting = 10 * matdim1
	FlasherLight1.IntensityScale = flashx3
	matdim1 = matdim1 - 1
	FlashLevel1 = FlashLevel1 * 0.85 - 0.01
	If matdim1 = 0 Then
		FlasherBase1.BlendDisableLighting = 2
		Flasherlight1.IntensityScale = 0
		matdim1 = 9
		FlasherFlash1.visible = 0
		FlasherFlash1.TimerEnabled = False
	End If
End Sub
'
'*** Red flasher ***
Dim matdim2
Sub FlasherFlash2_Timer()
	dim flashx3
	If not FlasherFlash2.TimerEnabled Then 
		FlasherFlash2.TimerEnabled = True
		FlasherFlash2.visible = 1
	End If
	flashx3 = (FlashLevel2 * FlashLevel2 * FlashLevel2)
	Flasherflash2.opacity = 800 * flashx3
	FlasherBase2.BlendDisableLighting = 10 * matdim2
	FlasherLight2.IntensityScale = flashx3
	matdim2 = matdim2 - 1
	FlashLevel2 = FlashLevel2 * 0.85 - 0.01
	If matdim2 = 0 Then
		FlasherBase2.BlendDisableLighting = 1
		Flasherlight2.IntensityScale = 0
		matdim2 = 9
		FlasherFlash2.visible = 0
		FlasherFlash2.TimerEnabled = False
	End If
End Sub
'
'*** Red flasher 2 ***
Dim matdim3
Sub FlasherFlash3_Timer()
	dim flashx3
	If not FlasherFlash3.TimerEnabled Then 
		FlasherFlash3.TimerEnabled = True
		FlasherFlash3.visible = 1
	End If
	flashx3 = (FlashLevel3 * FlashLevel3 * FlashLevel3)
	Flasherflash3.opacity = 900 * flashx3
	FlasherBase3.BlendDisableLighting = 10 * matdim3
	FlasherLight3.IntensityScale = flashx3
	matdim3 = matdim3 - 1
	FlashLevel3 = FlashLevel3 * 0.85 - 0.01
	If matdim3 = 0 Then
		FlasherBase3.BlendDisableLighting = 1
		Flasherlight3.IntensityScale = 0
		matdim3 = 9
		FlasherFlash3.visible = 0
		FlasherFlash3.TimerEnabled = False
	End If
End Sub

'*** Yellow flasher vertical (script is the same as for blue Flasher) ***
Dim matdim4
Sub FlasherFlash4_Timer()
	dim flashx3
	If not FlasherFlash4.TimerEnabled Then 
		FlasherFlash4.TimerEnabled = True
		FlasherFlash4.visible = 1
	End If
	flashx3 = (FlashLevel4 * FlashLevel4 * FlashLevel4)
	Flasherflash4.opacity = 800 * flashx3
	FlasherBase4.BlendDisableLighting = 10 * matdim4
	FlasherLight4.IntensityScale = flashx3
	matdim4 = matdim4 - 1
	FlashLevel4 = FlashLevel4 * 0.85 - 0.01
	If matdim4 = 0 Then
		FlasherBase4.BlendDisableLighting = 2
		Flasherlight4.IntensityScale = 0
		matdim4 = 9
		FlasherFlash4.visible = 0
		FlasherFlash4.TimerEnabled = False
	End If
End Sub



'******************* Preload **********************

Dim GIInit: GIInit=10 * 4
sub preloadTimer_Timer
	If Preload = 1 and GIInit > 0 Then
		GIInit = GIInit -1
		select case (GIInit \ 4) ' Divide by 4, this is not a frame timer, so we want to be sure frame is visible 
		case 0:
				playfieldOff.opacity = 100
				SidesBack.image = "Backtest"
				Rampe3.image = "rampe3GIOFF"
				Rampe2.image = "rampe2GIOFF"
				Rampe1.image = "rampe1GIOFF"
				Plastics.image = "plasticsOff"
				preload = 0:preloadTimer.enabled = 0
		case 1:
				SidesBack.image = "Backtest"
				Rampe3.image = "rampe3GIOFF"
				Rampe2.image = "rampe2GIOFF"
				Rampe1.image = "rampe1GIOFF"
				Plastics.image = "plasticsOff"
				PlayfieldOff.opacity = 100

		case 2:
				SidesBack.image = "Backtest"

				Rampe3.image = "rampe3GIOFF"
				Rampe2.image = "rampe2GIOFF"
				Rampe1.image = "rampe1GIOFF"
				Plastics.image = "plasticsOff"
				PlayfieldOff.opacity = 100
		case 3:
				SidesBack.image = "Backtest"
				Rampe3.image = "rampe3GIOFF"
				Rampe2.image = "rampe2GIOFF"
				Rampe1.image = "rampe1GIOFF"
				Plastics.image = "plasticsOff"
				PlayfieldOff.opacity = 100
		case 4:
				SidesBack.image = "Backtest"
				Rampe3.image = "rampe3GIOFF"
				Rampe2.image = "rampe2GIOFF"
				Rampe1.image = "rampe1GIOFF"
				Plastics.image = "plasticsOff"
				PlayfieldOff.opacity = 100
		case 5:
				SidesBack.image = "BacktestOn"
				SidesBack.image = "Backtest"
				Rampe3.image = "rampe3GION"
				Rampe2.image = "rampe2GION"
				Rampe1.image = "rampe1GION"
				Plastics.image = "plasticsON"
				PlayfieldOff.opacity = 100
		case 6:
				SidesBack.image = "BacktestOn"
				SidesBack.image = "Backtest"
				Rampe3.image = "rampe3GION"
				Rampe2.image = "rampe2GION"
				Rampe1.image = "rampe1GION"
				Plastics.image = "plasticsON"
				PlayfieldOff.opacity = 100
		case 7:
				SidesBack.image = "BacktestOn"
				SidesBack.image = "Backtest"
				Rampe3.image = "rampe3GION"
				Rampe2.image = "rampe2GION"
				Rampe1.image = "rampe1GION"
				Plastics.image = "plasticsON"
				playfieldOff.opacity = 80
		case 8:
				SidesBack.image = "BacktestOn"
				SidesBack.image = "Backtest"
				Rampe3.image = "rampe3GION"
				Rampe2.image = "rampe2GION"
				Rampe1.image = "rampe1GION"
				Plastics.image = "plasticsON"
				PlayfieldOff.opacity = 40
		case 9:
				SidesBack.image = "Backtest"
				Plastics.blenddisablelighting = 0
				BumperA.blenddisableLighting = 0.2
				BumperA001.blenddisableLighting = 0.2
				BumperA002.blenddisableLighting = 0.2
				Rampe3.blenddisableLighting = 0.2
				Rampe2.blenddisableLighting = 0.2
				Rampe1.blenddisableLighting = 0.1
				VisA.blenddisablelighting = 0
				VisB.blenddisablelighting = 0
				MurMetal.blenddisablelighting = 0
				MetalParts.blenddisablelighting = 0
				spotlight.blenddisablelighting = 0.5
				spotlightLight.blenddisablelighting = 0
				PegsBoverSlings.blenddisablelighting = 0
				Rampe3.image = "rampe3GIOFF"
				Rampe2.image = "rampe2GIOFF"
				Rampe1.image = "rampe1GIOFF"
				Apron.blenddisablelighting = 0
				ApronCachePlunger.blenddisablelighting = 0
				Plastics.image = "plasticsOff"
		end select 
	End If
End Sub


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

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	LUTBox.text = "LUTmeUP: " & CStr(LUTmeUP)
	LUTBox.TimerEnabled = 1
End Sub



'*********************** Ramps Sounds *************************
sub Wall018_hit()
	Stopsound "fx_metalrolling0"
	playsound "MetalRampHit1", 0, (Vol(ActiveBall)*10), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

sub Wall017_hit()
	Stopsound "fx_metalrolling2"
	playsound "MetalRampHit2", 0, (Vol(ActiveBall)*10), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub