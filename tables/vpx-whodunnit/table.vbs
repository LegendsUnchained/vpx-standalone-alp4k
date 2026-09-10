'Who Dunnit - IPDB No. 3685
'© Bally(Midway) 1995
'VPX recreation by ninuzzu
'thanks to DJRobX for the help and to the VPDev Team for the amazing VPX!

Option Explicit
Randomize

'Constantes
'///////////////

Const VolumeDial = 0.8				'fleep
Const BallRollVolume = 0.5 			'fleep'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5 			'fleep'Level of ramp rolling volume. Value between 0 and 1
Dim tablewidth: tablewidth = table1.width 'fleep
Dim tableheight: tableheight = table1.height 'fleep
Dim LUTset, LutToggleSound 'LUT
'Dim Ballsize,BallMass
'BallSize = 50
'BallMass = 1 '(Ballsize^3)/125000

'----- Phsyics Mods -----
Const RubberizerEnabled = 1			'0 = normal flip rubber, 1 = more lively rubber for flips
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

' ****** LUT overall contrast & brightness setting **************************************************************************************
Dim luts, lutpos
luts = array("LUTVogliadicane80", "LUTVogliadicane70", "1to1", "Fleep Natural Dark 1", "Fleep Natural Dark 2", "Fleep Warm Bright", "Fleep Warm Dark", "3rdaxis Referenced THX Standard", "CalleV Punchy Brightness and Contrast", "Skitso Natural and Balanced", "Skitso Natural High Contrast", "LUTbassgeige1", "LUTbassgeige2", "LUTbassgeigemeddark", "LUTbassgeigemeddarkwhite", "LUTbassgeigeultrdark", "LUTbassgeigeultrdarkwhite", "LUTblacklight", "LUTfleep", "LUTmandolin", "LUTmlager8", "LUTmlager8night", "LUTrobertmstotan0_darker", "luttotan4mhcontrastfilmic2", "LUTtotan1", "LUTtotan2", "LUTtotan4", "LUTtotan5", "LUTtotan6")
'lutpos = 0						'  set the nr of the LUT you want to use (0 = first in the list above, 1 = second, etc); 0 is the default
'table1.ColorGradeImage = luts(lutpos)
Const EnableMagnasave = 1		' 1 - on; 0 - off; if on then the magnasave button let's you rotate all LUT's
LoadLUT 'LUT
SetLUT 'LUT
'****************************************************************************************************************************************

Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
Const DynamicBallShadowsOn = 0		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 0		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's


Const FlasherIntensityGIOn = .75
Const FlasherIntensityGIOff = 1 

Const BallSize = 51
Const BallMass = 1.3

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD:UseVPMDMD = DesktopMode
Const UseVPMModSol = 0

' Rom Name
Const cGameName = "wd_12"

LoadVPM "02000000", "WPC.VBS", 3.50

' Standard Options
Const UseSolenoids = 2
Const UseLamps = 1
Const UseSync = 1
Const HandleMech = 0 

' Standard Sounds
Const SSolenoidOn="",SSolenoidOff="",SFlipperOn="",SFlipperOff="",SCoin="coin_in_1" 'fleep

'************************************************************************
'						 INIT TABLE
'************************************************************************

Dim bsTrough, bsLSaucer, bsRBPopper, bsRFPopper, plungerIM

Sub Table1_Init
	vpmInit Me 
	With Controller      
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Who Dunnit (Bally 1995)"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = DesktopMode
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
		.Switch(22) = 1 'close coin door
		.Switch(24) = 0 'always closed
	End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(sw61, sw62, sw63, sw64, sw65)

    ' Trough
    Set bsTrough = New cvpmTrough
    With bsTrough
		.size = 4
		.initSwitches Array(32, 33, 34, 35)
		.Initexit BallRelease, 90, 6
		.InitEntrySounds "drain", "", ""
		.InitExitSounds SoundFX(SSolenoidOn,DOFContactors), SoundFX("BallRelease1",DOFContactors)
		.Balls = 4
		.CreateEvents "bsTrough", Drain
    End With

	' Left Lock Up
	Set bsLSaucer = New cvpmSaucer
	With bsLSaucer
		.InitKicker sw51, 51, 180, 10, 1
        .InitExitVariance 1, 1
		.InitSounds "fx_saucerHit", SoundFX(SSolenoidOn,DOFContactors), SoundFX("saucer_Kick",DOFContactors)
		.CreateEvents "bsLSaucer", sw51
	End With

	' Right Back Popper
	Set bsRBPopper = New cvpmSaucer
	With bsRBPopper
		.InitKicker sw43, 43, 0, 35, 1.56
        .InitExitVariance 1, 1
		.InitSounds "fx_saucerHit", SoundFX(SSolenoidOn,DOFContactors), SoundFX("saucer_kick",DOFContactors)
		.CreateEvents "bsRBPopper", sw43
	End With

    ' Right Front Popper
    Set bsRFPopper = New cvpmTrough
	With bsRFPopper
		.size = 2
		.balls = 0
		.InitSwitches Array(44,57)
		.Initexit sw44, 210, 10
		.InitEntrySounds "fx_kicker_catch", "", ""
		.InitExitSounds SoundFX(SSolenoidOn,DOFContactors),SoundFX("saucer_kick",DOFContactors)
		.CreateEvents "bsRFPopper", sw57
	End With

	'Impulse Plunger
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP sw15, 45, 0.7
		.Switch 15
		.Random 0.05
		.InitExitSnd SoundFX("AutoPlunger",DOFContactors), SoundFX("AutoPlunger",DOFContactors)
		.CreateEvents "plungerIM"
	End With

	SlotInit:TargetsInit:RampInit:mapLights
	UpdateGI 0,0:UpdateGI 1,0:UpdateGI 2,0

	If DesktopMode Then
		CabRails.visible = 1
		'lower right flasher position
		flasherflash4.Y=1443.816
		flasherflash4.RotX=-65
		flasherflash4.Height=185
		'spinner flasher position
		flasherflash5.Height=210
	Else
		CabRails.visible = 0
		flasherflash4.Y=1439
		flasherflash4.RotX=-45
		flasherflash4.Height=195
		Flasherflash5.Height=245
		GI_SideR.opacity=150
		GI_SideTR.opacity=150
	End If
End Sub

'************************************************************************
'							KEYS
'************************************************************************

Sub Table1_KeyDown(ByVal Keycode)
	If keycode = keyFront Then Controller.Switch(23) = 1		'buy-in
		If keycode = PlungerKey Then Plunger.PullBack:SoundPlungerPull() 'test fleep
   
		If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
                Select Case Int(rnd*3)
                        Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                        Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                        Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

                End Select
        End If

	If keycode = LeftTiltKey Then Nudge 90, 5:PlaySoundAt SoundFX("fx_nudge",0),sw27
	If keycode = RightTiltKey Then Nudge 270, 5:PlaySoundAt SoundFX("fx_nudge",0),Drain
	If keycode = CenterTiltKey Then Nudge 0, 6:PlaySoundAt SoundFX("fx_nudge",0),sw16

' ****** LUT Keydown ************************

	If keycode = RightMagnaSave then
		lutpos = lutpos + 1 : If lutpos > ubound(luts) Then lutpos = 0 : end if
        call myChangeLut
		playsound "Lut_Toggle"
	End if

	If keycode = LeftMagnaSave then
		lutpos = lutpos - 1 : If lutpos < 0 Then lutpos = ubound(luts) : end if 
        call myChangeLut
		playsound "LUT_Toggle"
    	end if

' ****** LUT Keydown End ********************

'Flipper nFozzy
If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress : SolLFlipper True 'nfozzy
If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress : SolRFlipper True 'nfozzy

    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal Keycode)
	If keycode = keyFront Then Controller.Switch(23) = 0		'buy-in
		If KeyCode = PlungerKey Then
                Plunger.Fire
                SoundPlungerReleaseBall()                        
        End If

If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress : SolLFlipper False 'nfozzy
If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress : SolRFlipper False 'nfozzy

	If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub Table1_Paused:Controller.Pause = True:End Sub
Sub Table1_unPaused:Controller.Pause = False:End Sub
'Sub Table1_exit():Controller.Pause = False:Controller.Stop:End Sub

'*****************************************
'      		General Illumination
'*****************************************
Set GiCallBack2 = GetRef("UpdateGi")

Sub UpdateGi(nr,step)
	Dim ii
	Select Case nr

	Case 0 		'Left Playfield
		If step=0 Then
			For each ii in GI_Left:ii.state=0:Next
		Else
			For each ii in GI_Left:ii.state=1:Next
		End If
		For each ii in GI_Left:ii.IntensityScale = 0.125 * step:Next
		GI_SideL.IntensityScale = 0.125 * step
		GI_SideL1.IntensityScale = 0.125 * step

	Case 1		'Right Playfield
		If step=0 Then
			For each ii in GI_Right:ii.state=0:Next
		Else
			For each ii in GI_Right:ii.state=1:Next
		End If
		For each ii in GI_Right:ii.IntensityScale = 0.125 * step:Next
		GI_SideR.IntensityScale = 0.125 * step

	Case 2		'Top Playfield
		If step=0 Then
			For each ii in GI_Top:ii.state=0:Next
		Else
			For each ii in GI_Top:ii.state=1:Next
		End If
		BWR.IntensityScale = 0.125 * step
		BWL.IntensityScale = 0.125 * step
		GI_SideTL.IntensityScale = 0.125 * step
		GI_SideTR.IntensityScale = 0.125 * step
		For each ii in GI_Top:ii.IntensityScale = 0.125 * step:Next
		If Step>=7 Then Table1.ColorGradeImage = luts(lutpos):Else Table1.ColorGradeImage = "LUTbassgeigeultrdark":End If
		If step>4 Then DOF 103, DOFOn : Else DOF 103, DOFOff:End If

	Case 3		'Insert 1

	Case 4		'Insert 2

	End Select
End Sub

'*****************************************
' 			Lights Mapping
'*****************************************
Sub mapLights
	Dim obj
	For Each obj In Lamps
		set Lights(CInt(mid(obj.Name,2))) = obj
	Next
	Lights(16)= Array(l16,l16a,l16b,l16c)
	Lights(17)= Array(l17,l17a,l17b,l17c)
	Lights(18)= Array(l18,l18a,l18b)
End Sub

Sub SpotLightsUpdate
	L83.state = Controller.Lamp(83)
	L83a.Visible = Controller.Lamp(83)
	L84.state = Controller.Lamp(84)
	L84a.Visible = Controller.Lamp(84)
End Sub

'*****************************************
'      		Solenoids Mapping
'*****************************************
SolCallback(1)= "SolTrough"					'1-Trough
SolCallback(2)= "SolAutoPlungerIM"			'2-AutoPlunger
SolCallback(3)= "bsLSaucer.SolOut"			'3-Left Lock Up
SolCallback(4)= "bsRBPopper.SolOut"			'4-Right Back Popper
SolCallback(5)= "SolRampDown"				'5-Ramp Down
'SolCallback(6)= ""							'6-N.U.
SolCallback(7)= "vpmSolSound SoundFX(""fx_knocker"",DOFKnocker),"		'7-Knocker
SolCallback(8)= "bsRFPopper.Solout" 		'8-Right Front Popper
'SolCallback(9)= ""							'9-Left Sling
'SolCallback(10)= ""						'10-Right Sling
'SolCallback(11)= ""						'11-Left Jet
'SolCallback(12)= ""						'12-Bottom Jet
'SolCallback(13)= ""						'13-Right Jet
SolCallback(14)= "SolPhoneFlasher"			'14-Flasher: Phone
'SolCallback(15)= ""						'15-N.U.
SolCallback(16)= "SolRampUp"				'16-Ramp Up
SolCallback(17)= "SolBackFlashers"			'17-Flasher: Back (x2)
SolCallback(18)= "FL18.State="				'18-Flasher: AutoFire
SolCallback(19)= "SolLowerLeftFlasher"		'19-Flasher: Lower Left
SolCallback(20)= "SolSpinnerFlasher"		'20-Flasher: Spinner
SolCallback(21)= "SolLowerRightFlasher"		'21-Flasher: Lower Right
SolCallback(22)= "SolBank"					'22-Motor: 3-Bank
'SolCallback(23)= ""						'23-Motor: Left Slot B
'SolCallback(24)= ""						'24-Motor: Left Slot A
'SolCallback(25)= ""						'25-Motor: Center Slot B
'SolCallback(26)= ""						'26-Motor: Center Slot A
'SolCallback(27)= ""						'27-Motor: Right Slot B
'SolCallback(28)= ""						'28-Motor: Right Slot A
'
SolCallback(36)= "SolPost"					'36-Up-Down Post

'******************************************
'				FLIPPERS
'******************************************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Const ReflipAngle = 20
Sub SolLFlipper(Enabled)
        If Enabled Then
		LF.Fire
		'LF2.Fire
		'LF3.Fire 
        
                If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
                        RandomSoundReflipUpLeft LeftFlipper
                Else 
                        SoundFlipperUpAttackLeft LeftFlipper
                        RandomSoundFlipperUpLeft LeftFlipper
                End If                
        Else
                LeftFlipper.RotateToStart
				'LeftFlipper2.RotateToStart  'voir ATTENTION
				'LeftFlipper3.RotateToStart
                If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
                        RandomSoundFlipperDownLeft LeftFlipper
                End If
                FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper(Enabled)
        If Enabled Then
                RF.Fire
				'RF2.Fire
				'RF3.Fire

                If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
                        RandomSoundReflipUpRight RightFlipper
                Else 
                        SoundFlipperUpAttackRight RightFlipper
                        RandomSoundFlipperUpRight RightFlipper
                End If
        Else
                RightFlipper.RotateToStart
				'RightFlipper2.RotateToStart
				'RightFlipper3.RotateToStart
                If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
                        RandomSoundFlipperDownRight RightFlipper
                End If        
                FlipperRightHitParm = FlipperUpSoundLevel
        End If
End Sub

Sub LeftFlipper_Collide(parm)
		CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
        LeftFlipperCollide parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)

End Sub

Sub RightFlipper_Collide(parm)
		CheckLiveCatch Activeball, RightFlipper, RFCount, parm
        RightFlipperCollide parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)

End Sub

'************************************************************************
'						 BALL TROUGH
'************************************************************************
Sub SolTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		If BsTrough.Balls Then vpmTimer.PulseSw 31
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
'						3-BANK TARGETS
'************************************************************************
dim tbrake,tshake,tdir
Const targvel= 0.5			'the up-down speed

Sub TargetsInit
	'starts the bank in up position
    TargetBank.z=0
	sw66.Isdropped = 0:sw67.Isdropped = 0:sw68.Isdropped = 0:sw6X.IsDropped = 0
	Controller.Switch(11) = 0			
	Controller.Switch(73) = 1
	tdir=-1:tbrake=0:tshake=0
End Sub

Dim BankDisableTime: BankDisableTime=0

Sub SolBank(Enabled)
	If Enabled then
		PlayLoopSoundAtVol SoundFX("fx_mine_motor", DOFGear), TargetBank, 0.5
	else 
		StopSound "fx_mine_motor"
		BankDisableTime = 0
	End If
	BankMove.Enabled=Enabled
End Sub

Sub BankMove_timer()
	' Are we waiting for the ROM to disable the movement?  If yes, exit 
	if BankDisableTime > 0 and BankDisableTime > Timer Then Exit Sub
	TargetBank.z=TargetBank.z+targvel*tdir
	If TargetBank.z<-60 AND tdir=-1 then
		Controller.Switch(11) = 1:Controller.Switch(73) = 0:TargetBank.z=-60
		sw66.Isdropped = 1:sw67.Isdropped = 1:sw68.Isdropped = 1:sw6X.IsDropped = 1:tdir=1
		BankDisableTime = Timer + 1
	End If
	If TargetBank.z>0 AND tdir=1 then
		Controller.Switch(73) = 1:Controller.Switch(11) = 0:TargetBank.z=0
		sw66.Isdropped = 0:sw67.Isdropped = 0:sw68.Isdropped = 0:sw6X.IsDropped = 0:tdir=-1
		BankDisableTime = Timer + 1
	End if
End Sub

sub bankshake_timer()
	if tbrake>0 then			
		tshake=tshake+30
		tbrake=tbrake-0.08
		TargetBank.transy =((sin(tshake)))*tbrake
	else
		me.Enabled=0:tbrake=0:tshake=0
	End if
End Sub

'************************************************************************
'						UP-DOWN RAMP
'************************************************************************
Dim rdir
Const rampvel= 5			'the up-down speed

Sub RampInit
	'starts the ramp in up position
	UpDownRamp.rotX=0:UpDownRamp1.rotX=0
	UpRamp.collidable=0
	Controller.Switch(74) = 1
End Sub

Sub SolRampDown(Enabled)
	If Enabled then
		PlaySoundAt SoundFX("Taxi Ramp Down",DOFContactors), UpDownRamp
		rdir=-1:RampMove.Enabled=1:UpRamp.collidable=1
	End If
End Sub

Sub SolRampUp(Enabled)
	If Enabled then
		PlaySoundAt SoundFX("Taxi Ramp Up",DOFContactors), UpDownRamp
		rdir=1:RampMove.Enabled=1:UpRamp.collidable=0
	End If
End Sub

Sub RampMove_timer()
	UpDownRamp.rotX=UpDownRamp.rotX+rampvel*rdir
	UpDownRamp1.rotX=UpDownRamp.rotX
	If UpDownRamp.rotX<-15 and rdir=-1 then Controller.Switch(74) = 0:Me.Enabled=0:UpDownRamp.rotX=-15:UpDownRamp1.rotX=-15
	If UpDownRamp.rotX>0 and rdir=1 then Controller.Switch(74) = 1:Me.Enabled=0:UpDownRamp.rotX=0:UpDownRamp1.rotX=0
End Sub

'************************************************************************
'						UP-DOWN POST
'************************************************************************

Sub diverter_Init
	diverter.IsDropped = 1							'up-down post
End Sub

Sub SolPost(Enabled)
	if Enabled then
		'diverter.isdropped=0:PlaySoundAt SoundFX("fx_postup",DOFContactors),diverterP:diverterP.transZ = 70
		diverter.isdropped=0:PlaySoundAt SoundFX("",DOFContactors),diverterP:diverterP.transZ = 70
	else
		'diverter.isdropped=1:PlaySoundAt SoundFX("fx_postdown",DOFContactors),diverterP:diverterP.transZ = 0
		diverter.isdropped=1:PlaySoundAt SoundFX("",DOFContactors),diverterP:diverterP.transZ = 0
	End if
End Sub

'************************************************************************
'						SLOT REEL
'************************************************************************
dim MechReelL, MechReelM, MechReelR, ReelSnd, Reel1Speed, Reel2speed, Reel3Speed
Reel1Speed=0:Reel2Speed=0:Reel3Speed=0:ReelSnd = 0

Sub UpdateReelSound
	If Reel1Speed + Reel2Speed + Reel3Speed > 0 Then
		If ReelSnd=0 Then PlayLoopSoundAtVol SoundFX("Reel Motor",DOFGear), Reel1, 0.25
		ReelSnd=Timer
	Else
		if ReelSnd > 0 and Timer > ReelSnd + .5 then StopSound "Reel Motor":ReelSnd=0 
	End If
End Sub

Sub ReelLPos(aCurrPos, aSpeed, aLastPos)
	Reel1Speed = abs(aSpeed)
	Reel1.RotX = (-aCurrPos) + 275
End Sub

Sub ReelMPos(aCurrPos, aSpeed, aLastPos)
	Reel2Speed = abs(aSpeed)
	Reel2.RotX = (-aCurrPos) + 275
End Sub

Sub ReelRPos(aCurrPos, aSpeed, aLastPos)
	Reel3Speed = abs(aSpeed)
	Reel3.RotX = (-aCurrPos) + 275
End Sub

Sub SlotInit
	Set MechReelL = New cvpmMyMech : With mechReelL
		.Sol1 = 23 : .Sol2 = 24
		.MType = vpmMechLinear + vpmMechCircle + vpmMechStepSol + vpmMechFast
		.Length = 200 : .Steps = 360
		.AddSw 12, 0, 8
		.CallBack = GetRef("ReelLPos")
		.Start
	End With

	Set MechReelM = New cvpmMyMech : With mechReelM
		.Sol1 = 25 : .Sol2 = 26
		.MType = vpmMechLinear + vpmMechCircle + vpmMechStepSol + vpmMechFast
		.Length = 200 : .Steps = 360
		.AddSw 25, 0, 8
		.CallBack = GetRef("ReelMPos")
		.Start
	End With

	Set MechReelR = New cvpmMyMech : With mechReelR
		.Sol1 = 27 : .Sol2 = 28
		.MType = vpmMechLinear + vpmMechCircle + vpmMechStepSol + vpmMechFast
		.Length = 200 : .Steps = 360
		.AddSw 48, 0, 8
		.CallBack = GetRef("ReelRPos")
		.Start
	End With
End Sub

'*****************************************
'      			Switches
'*****************************************

'rollover
Sub sw16_hit:Controller.switch(16) = 1: PlaySoundAt "",ActiveBall:End Sub
Sub sw16_unhit:Controller.switch(16) = 0: End Sub
Sub sw17_hit:Controller.switch(17) = 1: PlaySoundAt "",ActiveBall:End Sub
Sub sw17_unhit:Controller.switch(17) = 0: End Sub
Sub sw18_hit:Controller.switch(18) = 1: PlaySoundAt "",ActiveBall:End Sub
Sub sw18_unhit:Controller.switch(18) = 0: End Sub
Sub sw26_hit:Controller.switch(26) = 1: PlaySoundAt "",ActiveBall:End Sub
Sub sw26_unhit:Controller.switch(26) = 0: End Sub
Sub sw27_hit:Controller.switch(27) = 1: PlaySoundAt "",ActiveBall:End Sub
Sub sw27_unhit:Controller.switch(27) = 0: End Sub
Sub sw28_hit:Controller.switch(28) = 1: PlaySoundAt "",ActiveBall:End Sub
Sub sw28_unhit:Controller.switch(28) = 0: End Sub

'ramp optos
Sub sw36_hit:vpmtimer.pulsesw 36: End Sub
Sub sw37_hit:vpmtimer.pulsesw 37: End Sub

'subway optos
Dim SubwaySnd:SubwaySnd=0
Sub sw41_hit:controller.Switch(41)=1:If SubwaySnd=0 Then SubwaySnd=1:PlaySoundAt "fx_subway",ActiveBall:End If: End Sub
Sub sw41_unhit:controller.Switch(41)=0:End Sub	
Sub sw42_hit:controller.Switch(42)=1:If SubwaySnd=0 Then SubwaySnd=1:PlaySoundAt "fx_subway",ActiveBall:End If: End Sub
Sub sw42_unhit:controller.Switch(42)=0:End Sub
Sub sw47_hit:controller.Switch(47)=1:If SubwaySnd=0 Then SubwaySnd=1:PlaySoundAt "fx_subway",ActiveBall:End If: End Sub
Sub sw47_unhit:controller.Switch(47)=0:End Sub
Sub SubwayStop_Hit():StopSound "fx_subway":SubwaySnd=0:End Sub

'4-bank targets
Sub sw52_hit:vpmtimer.pulsesw 52:PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub
Sub sw53_hit:vpmtimer.pulsesw 53:PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub
Sub sw54_hit:vpmtimer.pulsesw 54:PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub
Sub sw55_hit:vpmtimer.pulsesw 55:PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub

'Mystery target
Sub sw56_hit:vpmtimer.pulsesw 56:PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub

'Red target
Sub sw58_hit:vpmtimer.pulsesw 58: PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub

'Slingshots
Dim LStep, RStep

Sub sw61_slingshot
	vpmTimer.PulseSw 61
	RandomSoundSlingshotLeft SLING2
	LSling.Visible = 0: LSling2.Visible = 1: sling1.TransZ = -20: LStep = 0
	Me.TimerInterval = 50: Me.TimerEnabled = 1
End Sub

Sub sw62_slingshot
	vpmTimer.PulseSw 62
	RandomSoundSlingshotRight SLING1
	RSling.Visible = 0: RSling2.Visible = 1: sling2.TransZ = -20: RStep = 0
	Me.TimerInterval = 50: Me.TimerEnabled = 1
End Sub

Sub sw61_Timer
	Select Case LStep
		Case 3:LSLing2.Visible = 0:LSLing1.Visible = 1:sling1.TransZ = -10
		Case 4:LSLing1.Visible = 0:LSLing.Visible = 1:sling1.TransZ = 0:Me.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub sw62_Timer
	Select Case RStep
		Case 3:RSLing2.Visible = 0:RSLing1.Visible = 1:sling2.TransZ = -10
		Case 4:RSLing1.Visible = 0:RSLing.Visible = 1:sling2.TransZ = 0:Me.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

'Bumpers
Sub sw63_hit:vpmtimer.pulsesw 63:RandomSoundBumperTop sw63:End Sub
Sub sw64_hit:vpmtimer.pulsesw 64:RandomSoundBumperMiddle sw64:End Sub
Sub sw65_hit:vpmtimer.pulsesw 65:RandomSoundBumperBottom sw65:End Sub

'3-bank drop targets
Sub sw66_hit:vpmtimer.pulsesw 66:bankshake.enabled=1:tbrake=5:PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub
Sub sw67_hit:vpmtimer.pulsesw 67:bankshake.enabled=1:tbrake=5:PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub
Sub sw68_hit:vpmtimer.pulsesw 68:bankshake.enabled=1:tbrake=5:PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub

'2-bank targets
Sub sw71_hit:vpmtimer.pulsesw 71:PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub
Sub sw72_hit:vpmtimer.pulsesw 72:PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub

'Scoop Roof
Sub sw75_hit:Controller.switch(75) = 1: PlaySoundAt "",ActiveBall: sw75p.RotZ=-35: Me.TimerInterval=100: Me.TimerEnabled=1: End Sub
Sub sw75_timer():Me.TimerEnabled=0:sw75p.RotZ=-70:End Sub
Sub sw75_unhit:Controller.switch(75) = 0: End Sub
Sub sw76_hit:Controller.switch(76) = 1: PlaySoundAt "",ActiveBall: sw76p.RotZ=25: Me.TimerInterval=100: Me.TimerEnabled=1: End Sub
Sub sw76_timer():Me.TimerEnabled=0:sw76p.RotZ=-10:End Sub
Sub sw76_unhit:Controller.switch(76) = 0: End Sub
Sub sw77_hit:Controller.switch(77) = 1: PlaySoundAt "",ActiveBall: sw77p.RotZ=35: Me.TimerInterval=100: Me.TimerEnabled=1: End Sub
Sub sw77_timer():Me.TimerEnabled=0:sw77p.RotZ=0:End Sub
Sub sw77_unhit:Controller.switch(77) = 0: End Sub

'Black target
Sub sw78_hit:vpmtimer.pulsesw 78: PlaySoundAt SoundFX("",DOFTargets), ActiveBall:End Sub

'Spinner
Sub sw115_spin:vpmtimer.pulsesw 115: SoundSpinner sw115:End Sub

'******************************************************
'       	Flupper Flashers
'******************************************************

Dim FlashLevel1, Flashlevel2, FlashLevel3, FlashLevel4, FlashLevel5, FlashLevel6

FlasherLight1.IntensityScale = 0
Flasherlight2.IntensityScale = 0
Flasherlight3.IntensityScale = 0
Flasherlight4.IntensityScale = 0
Flasherlight5.IntensityScale = 0
Flasherlight6.IntensityScale = 0

Sub SolBackFlashers(enabled)
	If Enabled then
		Dim Intensity : If Light15.state = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
		FlashLevel1 = Intensity
		FlasherFlash1.TimerEnabled=0
		Roulette.BlendDisableLightingFromBelow = 0
		FlasherFlash1.TimerEnabled=1
	end if
End Sub

Sub SolLowerLeftFlasher(enabled)
	if Enabled then 
		Dim Intensity : If Light15.state = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
		FlashLevel3 = Intensity
		FlasherFlash3_Timer
	end if
End Sub

Sub SolLowerRightFlasher(enabled)
	if Enabled then 
		Dim Intensity : If Light15.state = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
		FlashLevel4 = Intensity
		FlasherFlash4_Timer
	end if
End Sub

Sub SolSpinnerFlasher(enabled)
	if Enabled then 
		Dim Intensity : If Light15.state = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
		FlashLevel5 = Intensity
		FlasherFlash5_Timer
	end if
End Sub

Sub SolPhoneFlasher(enabled)
	If enabled Then
		FlashLevel6=1
		FlasherFlash6_Timer
	End If
End Sub

Sub FlasherFlash1_Timer()
	dim flashx3, matdim
	FlasherFlash1.visible = 1
	FlasherFlash2.visible = 1
	FlasherFlash1a.visible = 1:FlasherFlash2a.visible = 1
	FlasherLit1.visible = 1
	FlasherLit2.visible = 1
	flashx3 = FlashLevel1 * FlashLevel1 * FlashLevel1
	Flasherflash1.opacity = 1000 * flashx3
	Flasherflash2.opacity = 1000 * flashx3
	Flasherflash1a.opacity = 500 * flashx3:Flasherflash2a.opacity = 500 * flashx3
	FlasherLit1.BlendDisableLighting = 10 * flashx3
	FlasherLit2.BlendDisableLighting = 10 * flashx3
	If Flasherbase1.BlendDisableLighting > 0.4 Then Flasherbase1.BlendDisableLighting =  flashx3
	If Flasherbase2.BlendDisableLighting > 0.4 Then Flasherbase2.BlendDisableLighting =  flashx3
	FlasherLight1.IntensityScale = flashx3
	FlasherLight2.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel1)
	FlasherLit1.material = "domelit" & matdim
	FlasherLit2.material = "domelit" & matdim
	FlashLevel1 = FlashLevel1 * 0.85 - 0.01
	If FlashLevel1 < 0.8 Then Roulette.BlendDisableLightingFromBelow = 1
	If FlashLevel1 < 0.15 Then
		FlasherLit1.visible = 0
		FlasherLit2.visible = 0
	Else
		FlasherLit1.visible = 1
		FlasherLit2.visible = 1
	end If
	If FlashLevel1 < 0 Then
		FlasherFlash1.TimerEnabled = False
		FlasherFlash1.visible = 0
		FlasherFlash2.visible = 0
		FlasherFlash1a.visible = 0:FlasherFlash2a.visible = 0
		Roulette.BlendDisableLightingFromBelow=1
	End If
End Sub

Sub FlasherFlash3_Timer()
	dim flashx3, matdim
	If not Flasherflash3.TimerEnabled Then 
		Flasherflash3.TimerEnabled = True
		Flasherflash3.visible = 1
		Flasherlit3.visible = 1
	End If
	flashx3 = FlashLevel3 * FlashLevel3 * FlashLevel3
	Flasherflash3.opacity = 8000 * flashx3
	Flasherlit3.BlendDisableLighting = 10 * flashx3
	Flasherbase3.BlendDisableLighting =  flashx3
	Flasherlight3.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	Flasherlit3.material = "domelit" & matdim
	FlashLevel3 = FlashLevel3 * 0.85 - 0.01
	If FlashLevel3 < 0.15 Then
		Flasherlit3.visible = 0
	Else
		Flasherlit3.visible = 1
	end If
	If FlashLevel3 < 0 Then
		Flasherflash3.TimerEnabled = False
		Flasherflash3.visible = 0
	End If
End Sub

Sub FlasherFlash4_Timer()
	dim flashx3, matdim
	If not Flasherflash4.TimerEnabled Then 
		Flasherflash4.TimerEnabled = True
		Flasherflash4.visible = 1
		Flasherlit4.visible = 1
	End If
	flashx3 = FlashLevel4 * FlashLevel4 * FlashLevel4
	Flasherflash4.opacity = 4000 * flashx3
	Flasherlit4.BlendDisableLighting = 10 * flashx3
	Flasherbase4.BlendDisableLighting =  flashx3
	Flasherlight4.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	Flasherlit4.material = "domelit" & matdim
	FlashLevel4 = FlashLevel4 * 0.85 - 0.01
	If FlashLevel4 < 0.15 Then
		Flasherlit4.visible = 0
	Else
		Flasherlit4.visible = 1
	end If
	If FlashLevel4 < 0 Then
		Flasherflash4.TimerEnabled = False
		Flasherflash4.visible = 0
	End If
End Sub

Sub FlasherFlash5_Timer()
	dim flashx3, matdim
	If not Flasherflash5.TimerEnabled Then 
		Flasherflash5.TimerEnabled = True
		Flasherflash5.visible = 1
		Flasherlit5.visible = 1
	End If
	flashx3 = FlashLevel5 * FlashLevel5 * FlashLevel5
	Flasherflash5.opacity = 8000 * flashx3
	Flasherlit5.BlendDisableLighting = 10 * flashx3
	Flasherbase5.BlendDisableLighting =  flashx3
	Flasherlight5.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	Flasherlit5.material = "domelit" & matdim
	FlashLevel5 = FlashLevel5 * 0.85 - 0.01
	If FlashLevel5 < 0.15 Then
		Flasherlit5.visible = 0
	Else
		Flasherlit5.visible = 1
	end If
	If FlashLevel5 < 0 Then
		Flasherflash5.TimerEnabled = False
		Flasherflash5.visible = 0
	End If
End Sub

Sub FlasherFlash6_Timer()
	dim flashx3
	If not FlasherFlash6.TimerEnabled Then 
		FlasherFlash6.TimerEnabled = True
		FlasherFlash6.visible = 1
	End If
	flashx3 = FlashLevel6 * FlashLevel6 * FlashLevel6
	FlasherFlash6.opacity = 6000 * flashx3
	Flasherlight6.IntensityScale = flashx3
	FlashLevel6 = FlashLevel6 * 0.85 - 0.01
	If FlashLevel6 < 0 Then
		FlasherFlash6.TimerEnabled = False
		FlasherFlash6.visible = 0
	End If
End Sub

'******************************************************
'       	RealTime Updates
'******************************************************
Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates()
	LeftBat.ObjRotZ = LeftFlipper.CurrentAngle
	RightBat.ObjRotZ = RightFlipper.CurrentAngle
	LeftFlipperSh.RotZ = LeftFlipper.currentangle
	RightFlipperSh.RotZ = RightFlipper.currentangle
	ScoopGate.RotX = Gate4.currentangle/2
	sw115p.RotX = -15*ABS(cos (sw115.currentangle*3.14/360))
'	RollingSoundUpdate
'	BallShadowUpdate
RollingTimer_Timer
	SpotLightsUpdate
	UpdateReelSound
End Sub

' *********************************************************************
'					Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

function AudioFade(ball)
	Dim tmp
    tmp = ball.y * 2 / Table1.height-1
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
 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min and max
End Function

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
	PlaySound sound, 1, Vol, Pan(tableobj), 0, 0, 0, 1, AudioFade(tableobj)
End Sub

Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, Pan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3
Sub PlaySoundAtBallVol(sound, VolMult)
	PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.
Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Dim NextOrbitHit:NextOrbitHit = 0
Sub PlasticRampBumps_Hit(idx)
Walls_hit (idx)
'	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
'		RandomBump 10, -20000
'		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
'		' Lowering these numbers allow more closely-spaced clunks.
'		NextOrbitHit = Timer + .1 + (Rnd * .2)
'	end if 
End Sub

' Requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
	PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' *********************************************************************
' 						Other Sound FX
' *********************************************************************

Sub Pins_Hit(idx)
	Rubbers_Hit(idx)
End Sub

Sub Posts_Hit(idx)
	Rubbers_Hit(idx)
End Sub

Sub CenterHole_hit:PlaySoundAt "fx_hole1", ActiveBall:End Sub
Sub Trigger1_hit:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub Trigger1_timer:Me.TimerEnabled=0:PlaysoundAt "Ball_Bounce_Playfield_Soft_1",Trigger1:End Sub
Sub Trigger2_hit:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub Trigger2_timer:Me.TimerEnabled=0:PlaysoundAt "Ball_Bounce_Playfield_Soft_2",Trigger2:End Sub
Sub Trigger3_hit:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub Trigger3_timer:Me.TimerEnabled=0:PlaysoundAt "Ball_Bounce_Playfield_Soft_3",Trigger3:End Sub
Sub Trigger4_hit:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub Trigger4_timer:Me.TimerEnabled=0:PlaysoundAt "Ball_Bounce_Playfield_Soft_4",Trigger4:End Sub

' *********************************************************************
'						ROLLING SOUND
' *********************************************************************
Const tnob = 10 ' total number of balls

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

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
			If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0 'ambient
                rolling(b) = False
				StopSound("RampLoop" & b)
                StopSound("BallRoll_" & b)
        Next

        ' exit the sub if no balls on the table
        If UBound(BOT) = -1 Then Exit Sub

        ' play the rolling sound for each ball

        For b = 0 to UBound(BOT)
                If BallVel(BOT(b)) > 1 Then
					rolling(b) = True
					If BOT(b).z < 30 Then
                        						StopSound("RampLoop" & b)
                        PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
					Else
						StopSound "BallRoll_" & b
						PlaySound ("RampLoop" & b), -1, VolPlayfieldRoll(BOT(b)) * 2.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
					End If
                Else
                        If rolling(b) = True Then
                                StopSound("BallRoll_" & b)
                                StopSound("RampLoop" & b)
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


' *****  cvpmMech replacement, all because the "Fast" parameter is not set on timer update!!   We can't get smooth reels without this.   Until this is addressed in Core.vbs, replacing here.

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

'******************************************************
'****  GENEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these 
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Flippers: 	https://www.youtube.com/watch?v=FWvM9_CdVHw
' Dampeners: 	https://www.youtube.com/watch?v=tqsxx48C6Pg
' Physics: 		https://www.youtube.com/watch?v=UcRMG-2svvE
'
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25) 
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 9.5-10.5 |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
' 
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 4-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |



'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level weÂll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. an object or point to tell the script where the tip of the flipper is at rest (EndPointLp, EndPointRp)
'	4. and, special scripting
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.  
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end 
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.3            | 0.3                   | 0.275                  | 0.275              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'


'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
dim RF1: Set RF1 = New FlipperPolarity
dim LF1: Set LF1 = New FlipperPolarity
dim LF2 : Set LF2 = New FlipperPolarity
dim RF2 : Set RF2 = New FlipperPolarity
dim LF3 : Set LF3 = New FlipperPolarity
dim RF3 : Set RF3 = New FlipperPolarity


InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF, LF2, RF2, LF3, RF3)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80  '*****Important, this variable is an offset for the speed that the ball travels down the table to determine if the flippers have been fired 
'							'This is needed because the corrections to ball trajectory should only applied if the flippers have been fired and the ball is in the trigger zones.
'							'FlipAT is set to GameTime when the ball enters the flipper trigger zones and if GameTime is less than FlipAT + this time delay then changes to velocity
'							'and trajectory are applied.  If the flipper is fired before the ball enters the trigger zone then with this delay added to FlipAT the changes
'							'to tragectory and velocity will not be applied.  Also if the flipper is in the final 20 degrees changes to ball values will also not be applied.
'							'"Faster" tables will need a smaller value while "slower" tables will need a larger value to give the ball more time to get to the flipper. 		
'							'If this value is not set high enough the Flipper Velocity and Polarity corrections will NEVER be applied.
'
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -2.7        
'        AddPt "Polarity", 2, 0.33, -2.7
'        AddPt "Polarity", 3, 0.37, -2.7        
'        AddPt "Polarity", 4, 0.41, -2.7
'        AddPt "Polarity", 5, 0.45, -2.7
'        AddPt "Polarity", 6, 0.576,-2.7
'        AddPt "Polarity", 7, 0.66, -1.8
'        AddPt "Polarity", 8, 0.743, -0.5
'        AddPt "Polarity", 9, 0.81, -0.5
'        AddPt "Polarity", 10, 0.88, 0
'
'        addpt "Velocity", 0, 0,         1
'        addpt "Velocity", 1, 0.16, 1.06
'        addpt "Velocity", 2, 0.41,         1.05
'        addpt "Velocity", 3, 0.53,         1'0.982
'        addpt "Velocity", 4, 0.702, 0.968
'        addpt "Velocity", 5, 0.95,  0.968
'        addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper 
'        LF.EndPoint = EndPointLp  'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'		'LF2.Object = LeftFlipper2        
'        'LF2.EndPoint = EndPointLp2
'        'RF2.Object = RightFlipper2
'        'RF2.EndPoint = EndPointRp2
'		'LF3.Object = LeftFlipper3        
'        'LF3.EndPoint = EndPointLp3
'        'RF3.Object = RightFlipper3
'        'RF3.EndPoint = EndPointRp3
'
'End Sub



''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF, LF2, RF2, LF3, RF3)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -3.7        
'        AddPt "Polarity", 2, 0.33, -3.7
'        AddPt "Polarity", 3, 0.37, -3.7
'        AddPt "Polarity", 4, 0.41, -3.7
'        AddPt "Polarity", 5, 0.45, -3.7 
'        AddPt "Polarity", 6, 0.576,-3.7
'        AddPt "Polarity", 7, 0.66, -2.3
'        AddPt "Polarity", 8, 0.743, -1.5
'        AddPt "Polarity", 9, 0.81, -1
'        AddPt "Polarity", 10, 0.88, 0
'
'        addpt "Velocity", 0, 0,         1
'        addpt "Velocity", 1, 0.16, 1.06
'        addpt "Velocity", 2, 0.41,         1.05
'        addpt "Velocity", 3, 0.53,         1'0.982
'        addpt "Velocity", 4, 0.702, 0.968
'        addpt "Velocity", 5, 0.95,  0.968
'        addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper        
'        LF.EndPoint = EndPointLp
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'		'LF2.Object = LeftFlipper2        
'       'LF2.EndPoint = EndPointLp2
'       'RF2.Object = RightFlipper2
'       'RF2.EndPoint = EndPointRp2
'		'LF3.Object = LeftFlipper3        
'       'LF3.EndPoint = EndPointLp3
'       'RF3.Object = RightFlipper3
'       'RF3.EndPoint = EndPointRp3
'End Sub
'
'


'*******************************************
'  Late 80's early 90's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF, LF2, RF2, LF3, RF3)
'	for each x in a
'		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'	Next
'
'	AddPt "Polarity", 0, 0, 0
'	AddPt "Polarity", 1, 0.05, -5
'	AddPt "Polarity", 2, 0.4, -5
'	AddPt "Polarity", 3, 0.6, -4.5
'	AddPt "Polarity", 4, 0.65, -4.0
'	AddPt "Polarity", 5, 0.7, -3.5
'	AddPt "Polarity", 6, 0.75, -3.0
'	AddPt "Polarity", 7, 0.8, -2.5
'	AddPt "Polarity", 8, 0.85, -2.0
'	AddPt "Polarity", 9, 0.9,-1.5
'	AddPt "Polarity", 10, 0.95, -1.0
'	AddPt "Polarity", 11, 1, -0.5
'	AddPt "Polarity", 12, 1.1, 0
'	AddPt "Polarity", 13, 1.3, 0
'
'	addpt "Velocity", 0, 0,         1
'	addpt "Velocity", 1, 0.16, 1.06
'	addpt "Velocity", 2, 0.41,         1.05
'	addpt "Velocity", 3, 0.53,         1'0.982
'	addpt "Velocity", 4, 0.702, 0.968
'	addpt "Velocity", 5, 0.95,  0.968
'	addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper        
'        LF.EndPoint = EndPointLp
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'		'LF2.Object = LeftFlipper2        
'       'LF2.EndPoint = EndPointLp2
'       'RF2.Object = RightFlipper2
'       'RF2.EndPoint = EndPointRp2
'		'LF3.Object = LeftFlipper3        
'       'LF3.EndPoint = EndPointLp3
'       'RF3.Object = RightFlipper3
'       'RF3.EndPoint = EndPointRp3
'End Sub
'
'
'
'
'*******************************************
'' Early 90's and after
'
Sub InitPolarity()
        dim x, a : a = Array(LF, RF, LF2, RF2, LF3, RF3)
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
		'LF2.Object = LeftFlipper2        
       'LF2.EndPoint = EndPointLp2
       'RF2.Object = RightFlipper2
       'RF2.EndPoint = EndPointRp2
		'LF3.Object = LeftFlipper3        
       'LF3.EndPoint = EndPointLp3
       'RF3.Object = RightFlipper3
       'RF3.EndPoint = EndPointRp3
End Sub


' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub
'Sub TriggerLF2_Hit() : LF.Addball activeball : End Sub
'Sub TriggerLF2_UnHit() : LF.PolarityCorrect activeball : End Sub
'Sub TriggerRF2_Hit() : RF.Addball activeball : End Sub
'Sub TriggerRF2_UnHit() : RF.PolarityCorrect activeball : End Sub
'Sub TriggerLF3_Hit() : LF.Addball activeball : End Sub
'Sub TriggerLF3_UnHit() : LF.PolarityCorrect activeball : End Sub
'Sub TriggerRF3_Hit() : RF.Addball activeball : End Sub
'Sub TriggerRF3_UnHit() : RF.PolarityCorrect activeball : End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.


'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF, LF1, RF1, LF2, RF2)
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

'********Triggered by a ball hitting the flipper trigger area
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

'*********Used to rotate flipper since this is removed from the key down for the flippers
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
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))   '% of flipper swing
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

'***********gameTime is a global variable of how long the game has progressed in ms
'***********This function lets the table know if the flipper has been fired
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

'***********This is turned on when a ball leaves the flipper trigger area
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
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)   'Set creates an object in VB
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
'**********Takes in more than one array and passes them to ShuffleArray
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
'**********Calculate ball speed as hypotenuse of velX/velY triangle
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
'**********Calculates the value of Y for an input x using the slope intercept equation
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
'********Interpolates the value for areas between the low and upper bounds sent to it
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
'  FLIPPER TRICKS 
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
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************
'Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

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
'  Check ball distance from Flipper for Rem
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


'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle, LF1EndAngle, RF1EndAngle

'Const FlipperCoilRampupMode = 1   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque			'End of Swing Torque
EOSA = leftflipper.eostorqueangle		'End of Swing Torque Angle
Frampup = LeftFlipper.rampup			'Flipper Stregth Ramp Up
FElasticity = LeftFlipper.elasticity	'Flipper Elasticity
FReturn = LeftFlipper.return			'Flipper Return Strength
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 		'determines strength of coil field at start of swing
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16					'variable to check elapsed time
Const LiveElasticity = 0.45
Const SOSEM = 0.815						
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
Const EOSReturn = 0.025  'mid 90's and later

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
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	'What this code does is swing the flipper fast and make the flipper soft near its EOS to enable live catches.  It resets back to the base Table
	'settings once the flipper reaches the end of swing.  The code also makes the flipper starting ramp up high to simulate the stronger starting
	'coil strength and weaker at its EOS to simulate the weaker hold coil.

	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then  'If the flipper has started its swing, make it swing fast to nearly the end...
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 									'set flipper Ramp Up high
			Flipper.endangle = FEndAngle - 3*Dir						'swing to within 3 degrees of EOS
			Flipper.Elasticity = FElasticity * SOSEM					'Set the elasticity to the base table elasticity
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then   'If the flipper is fully swung and the flipper button is pressed then
		if FCount = 0 Then FCount = GameTime				'notes the Game Time to see if a live catch is possible

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew				'sets flipper EOS Torque Angle to .2 
			Flipper.eostorque = EOSTnew						'sets flipper EOS Torque to 1
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 'If the flipper has swung past it's end of swing then..
		If FState <> 3 Then													
			Flipper.eostorque = EOST        								'set the flipper EOS Torque back to the base table setting
			Flipper.eostorqueangle = EOSA									'set the flipper EOS Torque Angle back to the base table setting
			Flipper.rampup = Frampup										'set the flipper Ramp Up back to the base table setting
			Flipper.Elasticity = FElasticity								'set the flipper Elasticity back to the base table setting
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

'**************************************************
'        Flipper Collision Subs
'NOTE: COpy and overwrite collision sound from original collision subs over
'RandomSoundFlipper()' below
'**************************************************'

Sub LeftFlipper_Collide(parm)
    CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
    'RandomSoundFlipper() 'Remove this line if Fleep is integrated
    LeftFlipperCollide parm   'This is the Fleep code
End Sub

Sub RightFlipper_Collide(parm)
    CheckLiveCatch Activeball, RightFlipper, RFCount, parm
    'RandomSoundFlipper() 'Remove this line if Fleep is integrated
    RightFlipperCollide parm  'This is the Fleep code
End Sub

' iaakki Rubberizer
sub Rubberizer(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * 1.2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * -1.1
		activeball.vely = activeball.vely * 1.4
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub

' apophis rubberizer
sub Rubberizer2(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 0.5
		activeball.vely = activeball.vely * (1.2 + rnd(1)/3 )
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************

'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR



Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub

'*********This sets up the rubbers:
dim RubbersD : Set RubbersD = new Dampener     'Makes a Dampener Class Object 
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

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
'       		 Uses the LinearEnvelope function to calculate the correction based upon where it's value sits in relation
'       		 to the addpoint parameters set above.  Basically interpolates values between set points in a linear fashion
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
'       		 Uses the function BallSpeed's value at the point of impact/the active ball's velocity which is constantly being updated	
'				 RealCor is always less than 1 
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
'                Divides the desired CoR by the real COR to make a multiplier to correct velocity in x and y
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
'             	  Applies the coef to x and y velocities
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handled here
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
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************
'*********CoR is Coefficient of Restitution defined as "how much of the kinetic energy remains for the objects 
'to rebound from one another vs. how much is lost as heat, or work done deforming the objects 
dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
Sub RDampen_Timer
	Cor.Update
End Sub

'******************************************************
'  END NFOZZY PHYSICS
'******************************************************
'//////////////////////////////////////////////////////////////////////
'// Mechanic Sounds
'//////////////////////////////////////////////////////////////////////

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
' 	Metals (all metal objects, metal walls, metal posts, metal wire guides)
' 	Apron (the apron walls and plunger wall)
' 	Walls (all wood or plastic walls)
' 	Rollovers (wire rollover triggers, star triggers, or button triggers)
' 	Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
' 	Gates (plate gates)
' 	GatesWire (wire gates)
' 	Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.  
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).  
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Tutorial vides by Apophis
' Part 1: 	https://youtu.be/PbE2kNiam3g
' Part 2: 	https://youtu.be/B5cm1Y8wQsk
' Part 3: 	https://youtu.be/eLhWyuYOyGg


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
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

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


'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

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

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
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
	RandomSoundWall()      
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
End Sub


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////
'********************************************
'*   LUT Selector
'********************************************																																				"

dim textindex : textindex = 1
dim charobj(55), glyph(201)
InitDisplayText

Sub myChangeLut
	table1.ColorGradeImage = luts(lutpos)
	DisplayText lutpos, luts(lutpos)
	vpmTimer.AddTimer 2000, "If lutpos = " & lutpos & " then for anr = 10 to 54 : charobj(anr).visible = 0 : next'"
End Sub

Sub InitDisplayText
	Dim anr
	For anr = 10 to 54 : set charobj(anr) = eval("text0" & anr) : charobj(anr).visible = 0 : Next
	For anr = 32 to 96 : glyph(anr) = anr : next
	For anr = 0 to 31 : glyph(anr) = 32 : next
	for anr = 97 to 122 : glyph(anr)  = anr - 32 : next
	for anr = 123 to 200 : glyph(anr) = 32 : next
End Sub

Sub DisplayText(nr, luttext)
	dim tekst, anr
	for anr = 10 to 54 : charobj(anr).imageA = 32 : charobj(anr).visible = 1 : next
	If nr > -1 then
		tekst = "lutpos:" & nr
		For anr = 1 to len(tekst) : charobj(43 + anr).imageA = glyph(asc(mid(tekst, anr, 1))) : Next
	End If
	For anr = 1 to len(luttext)
		charobj(9 + anr).imageA = glyph(asc(mid(luttext, anr, 1)))
		If nr = -1 Then
			charobj(9 + anr).y = 1500 + sin(((textindex * 4 + anr)/20)*3.14) * 100
			charobj(9 + anr).height = 150 + cos(((textindex * 4 + anr)/20)*3.14) * 100
		End If
	Next
End Sub

Sub SetLUT
	'Table1.ColorGradeImage = "LUT" & LUTset
	table1.ColorGradeImage = luts(lutpos)
end sub 

Sub SaveLUT

	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if lutpos = "" then lutpos = 0 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "SLUT_" & cGameName & ".txt",True)
	ScoreFile.WriteLine lutpos 'la réf dans le txt
	Set ScoreFile=Nothing
	Set FileObj=Nothing

End Sub

Sub LoadLUT

	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		lutpos=0
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "SLUT_" & cGameName & ".txt") then
		lutpos=0
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "SLUT_" & cGameName & ".txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			lutpos=0
			Exit Sub
		End if
		lutpos = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub
'********************************************	
'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* with at least as many objects each as there can be balls, including locked balls
' Ensure you have a timer with a -1 interval that is always running

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
'***If there are more than 3 lights that overlap in a playable area, exclude the less important lights***
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection, and more than 3 will cause "jumping" between which shadows are drawn
' The easiest way to keep track of this is to start with the group on the right slingshot and move anticlockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'G				H											^	E
'															^	B
'	A		 C												^	A
'	 B		D			your collection should look like	^	G		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F												^	H
'															^	C
'															^	D
'															^	F
'		When selecting them, you'd shift+click in this order^^^^^

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
Sub FrameTimer2_Timer()
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.97	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
'	' stop the sound of deleted balls
'	For b = UBound(BOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		...rolling(b) = False
'		...StopSound("BallRoll_" & b)
'	Next
'
'...rolling and drop sounds...

'		If DropCount(b) < 5 Then
'			DropCount(b) = DropCount(b) + 1
'		End If
'
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If BOT(b).Z > 30 Then
'				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
'			Else
'				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
'			BallShadowA(b).X = BOT(b).X
'			BallShadowA(b).visible = 1
'		End If

' *** Required Functions, enable these if they are not already present elswhere in your table
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

Dim PI: PI = 4*Atn(1)

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

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******
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
'****************************************************************
'  GI
'****************************************************************
dim gilvl:gilvl = 1
Sub ToggleGI(Enabled)
	dim xx
	If enabled Then
		for each xx in GI:xx.state = 1:Next
		gilvl = 1
	Else
		for each xx in GI:xx.state = 0:Next	
		GITimer.enabled = True
		gilvl = 0
	End If
	Sound_GI_Relay enabled, bumper1
End Sub

Sub GITimer_Timer()
	me.enabled = False
	ToggleGI 1
End Sub
'****************************************************************
'  END GI
'****************************************************************

Sub Table1_exit()
	SaveLUT
	Controller.Stop
End Sub
