' Robo-War (Gottlieb 1988)
' =======================================================
' VP9 Version by Kevin Lee Drum
' B2B Collision thanks to jimmyfingers
' Thanks to the VP8 authors TAB, Destruk, MNPG
' Much was also inspired by JPSalas and unclewilly

' VPX Version By Dozer - January 2017.

Option Explicit

'Constantes
'///////////////

Const VolumeDial = 0.8				'fleep
Const BallRollVolume = 0.5 			'fleep'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5 			'fleep'Level of ramp rolling volume. Value between 0 and 1
Dim tablewidth: tablewidth = table1.width 'fleep
Dim tableheight: tableheight = table1.height 'fleep
Dim LUTset, LutToggleSound 'LUT
Dim Ballsize,BallMass
BallSize = 50
BallMass = 1 '(Ballsize^3)/125000

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
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim DesktopMode:DesktopMode = Table1.ShowDT

'GRAPHICS OPTIONS
'Const BallFlare =1 'Render a lense flare effect above the ball when certain flashers are active.
Const Playfield_Dimples = 0 ' Render a texture over the playfield to give it some depth.
'------------------------------------

LoadVPM "01210000", "sys80.vbs", 3.1

' Variables
' ==================================================================
Const cGameName = "robowars"
Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0
Dim bsTop, bsTrough, dtAlpha1, dtAlpha2, dtAlpha3, dtBeta,gion
Dim QRelay, OldQRelay, TRelay, OldTRelay, SRelay,OldSRelay, ARelay, OldARelay, BallRel, OldBallRel, BallLaunched, StargateRelay, OldStargateRelay
Dim AuxLightsStep, BlinkGIStep, LeftSlingStep, RightSlingStep, UpOrDown, PlungeWidth, PlungeStep, PlungeRamps, PDirection
' Standard Sounds
' ==================================================================
' All the sounds in this table are tweaked versions of sounds from the VPForums sound library.
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SFlipperOn = "FlipperUp"
Const SFlipperOff = "FlipperDown"
Const SCoin = "coin_in_1"

' Table Init
' ==================================================================
Sub Table1_Init
	vpmInit Me 'new
	With Controller
		.GameName = cGameName
		.Games(cGameName).Settings.Value("rol") = 0
		.Games(cGameName).Settings.Value("dmd_red") = 0
		.Games(cGameName).Settings.Value("dmd_green") = 223
		.Games(cGameName).Settings.Value("dmd_blue") = 223		
		.SplashInfoLine = "Robo-War (Gottlieb 1988)" & vbNewLine & "VPM table by Kevin Lee Drum"
		.HandleMechanics = 0
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		If DesktopMode AND B2SOn then
			.Hidden = 0 
		Else
			If B2SOn then
				.Hidden = 0
			Else
				.Hidden = 0
			End If
        End If
		If Err Then MsgBox Err.Description
		On Error Resume Next
	End With
	
	On Error Goto 0
	Controller.SolMask(0) = 0
	vpmTimer.AddTimer 2000, "Controller.SolMask(0) = &Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
	Controller.Run GetPlayerHWnd

	' Timers
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	' Nudging
	vpmNudge.TiltSwitch = 57
	vpmNudge.Sensitivity = 1
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)

	' Ball Stacks
	Set bsTop = New cvpmBallStack
	With bsTop
		.InitSaucer Kicker,76,140,5
		.InitExitSnd SoundFX("Popper_Ball",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
	End With

	Set bsTrough = New cvpmBallStack
	With bsTrough
		.InitNoTrough BallRelease,66,45,10
		.InitSw 66,0,56,0,0,0,0,0
		.InitKick BallRelease,45,10
		.InitExitSnd SoundFX("BallRelease1",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
		'.BallImage = "BallDark"
		.Balls = 2
	End With
	
	' Drop Targets
	Set dtAlpha1 = New cvpmDropTarget
	dtAlpha1.InitDrop Alpha1,43
	dtAlpha1.InitSnd SoundFX("drop_target_down_1",DOFDropTargets), SoundFX("drop_target_reset_1",DOFDropTargets)
	dtAlpha1.CreateEvents "dtAlpha1"

	Set dtAlpha2 = New cvpmDropTarget
	dtAlpha2.InitDrop Array(Alpha2A,Alpha2B), Array(42,52)
	dtAlpha2.InitSnd SoundFX("drop_target_down_2",DOFDropTargets), SoundFX("drop_target_reset_2",DOFDropTargets)
	dtAlpha2.CreateEvents "dtAlpha2"

	Set dtAlpha3 = New cvpmDropTarget
	dtAlpha3.InitDrop Array(Alpha3A,Alpha3B,Alpha3C), Array(41,51,61)
	dtAlpha3.InitSnd SoundFX("drop_target_down_3",DOFDropTargets), SoundFX("drop_target_reset_3",DOFDropTargets)
	dtAlpha3.CreateEvents "dtAlpha3"

	Set dtBeta = New cvpmDropTarget
	dtBeta.InitDrop Array(BetaB,BetaE,BetaT,BetaA), Array(40,50,60,70)
	dtBeta.InitSnd SoundFX("drop_target_down_4",DOFDropTargets), SoundFX("drop_target_reset_4",DOFDropTargets)
	dtBeta.CreateEvents "dtBeta"
	
	' GI Option
	' DOF 150,1
twos = 0
gion = 1
End Sub

If Table1.ShowDT = true then

Ramp16.visible = 1
Ramp15.visible = 1
 else

Ramp16.visible = 0
Ramp15.visible = 0
 End If

If Playfield_Dimples = 1 Then
LTFDim.Visible = 1
RTFDim.Visible = 1
PFDim.Visible = 1
else
LTFDim.Visible = 0
RTFDim.Visible = 0
PFDim.Visible = 0
End If

' Keys, etc.
' ==================================================================
Sub Table1_Paused:Controller.Pause = True:End Sub
Sub Table1_unPaused:Controller.Pause = False:End Sub

Sub Table1_KeyDown(ByVal keycode)
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
If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress : SolRFlipper True : Controller.Switch(46)=1 'nfozzy

    'If keycode = 3 Then Msgbox Activeball.Y
		If keycode = PlungerKey Then Plunger.PullBack:SoundPlungerPull() 'test fleep
   
		If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
                Select Case Int(rnd*3)
                        Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                        Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                        Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

                End Select
        End If
	If vpmKeyDown(KeyCode) Then Exit Sub
	If KeyCode = KeyRules then Rules
End Sub

Sub Table1_KeyUp(ByVal keycode)
If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress : SolLFlipper False 'nfozzy
If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress : SolRFlipper False : Controller.Switch(46)=0 'nfozzy

		If KeyCode = PlungerKey Then
                Plunger.Fire
                SoundPlungerReleaseBall()                        
        End If
	If vpmKeyUp(KeyCode) Then Exit Sub
End Sub

' Switches
' ==================================================================
' Slingshots

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    RandomSoundSlingshotRight SLING1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    RightSlingShot.TimerInterval = 10
	vpmTimer.PulseSw 33
    DOF 111,DOFPulse
    RSS.opacity = 60
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
    RSS.opacity = 0
End Sub

Sub LeftSlingShot_Slingshot
    RandomSoundSlingshotLeft SLING2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    LeftSlingShot.TimerInterval = 10
	vpmTimer.PulseSw 33
    LSS.opacity = 60
    DOF 110, DOFPulse
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
    LSS.opacity = 0
End Sub

' Bumpers
Sub Bumper1_Hit:RandomSoundBumperTop Bumper1:vpmTimer.PulseSw 53:End Sub
Sub Bumper2_Hit:RandomSoundBumperMiddle Bumper2:vpmTimer.PulseSw 63:End Sub
Sub Bumper3_Hit:RandomSoundBumperBottom Bumper3:vpmTimer.PulseSw 73:End Sub
' Drain
Sub Drain_Hit:RandomSoundDrain Drain:bsTrough.AddBall Me:End Sub
' Rollovers
Sub LaneR_Hit:Controller.Switch(44) = 1:				End Sub
Sub LaneR_UnHit:Controller.Switch(44) = 0:				End Sub
Sub LaneO_Hit:Controller.Switch(54) = 1:				End Sub
Sub LaneO_UnHit:Controller.Switch(54) = 0:				End Sub
Sub LaneB_Hit:Controller.Switch(64) = 1:				End Sub
Sub LaneB_UnHit:Controller.Switch(64) = 0:				End Sub
Sub LaneO2_Hit:Controller.Switch(74) = 1:				End Sub
Sub LaneO2_UnHit:Controller.Switch(74) = 0:				End Sub
Sub LeftOutlane_Hit:Controller.Switch(45) = 1:			End Sub
Sub LeftOutlane_UnHit:Controller.Switch(45) = 0:		End Sub
Sub LeftInlane_Hit:Controller.Switch(55) = 1:			End Sub
Sub LeftInlane_UnHit:Controller.Switch(55) = 0:			End Sub
Sub RightInlane_Hit:Controller.Switch(65) = 1:			End Sub
Sub RightInlane_UnHit:Controller.Switch(65) = 0:		End Sub
Sub RightOutlane_Hit:Controller.Switch(75) = 1:			End Sub
Sub RightOutlane_UnHit:Controller.Switch(75) = 0:		End Sub
Sub RightLane_Hit:Controller.Switch(35) = 1:			End Sub
Sub RightLane_UnHit:Controller.Switch(35) = 0:			End Sub
Sub ShooterLane_Hit:Controller.Switch(36) = 1:			End Sub
Sub ShooterLane_UnHit:Controller.Switch(36) = 0:    	End Sub
Sub TopRightRollover_Hit:Controller.Switch(31) = 1:		End Sub
Sub TopRightRollover_UnHit:Controller.Switch(31) = 0:	End Sub
Sub StargateRollover_Hit:Controller.Switch(71) = 1:		End Sub
Sub StargateRollover_UnHit:Controller.Switch(71) = 0:	End Sub
' Spinner
Sub Spinner_Spin:vpmTimer.PulseSw 30:SoundSpinner Spinner:End Sub
' Drop Targets
Sub Alpha1_Hit:dtAlpha1.hit 1:End Sub
Sub Alpha2A_Hit:dtAlpha2.hit 1:End Sub
Sub Alpha2B_Hit:dtAlpha2.hit 2:End Sub
Sub Alpha3A_Hit:dtAlpha3.hit 1:End Sub
Sub Alpha3B_Hit:dtAlpha3.hit 2:End Sub
Sub Alpha3C_Hit:dtAlpha3.hit 3:End Sub
Sub BetaB_Hit:dtBeta.hit 1:End Sub
Sub BetaE_Hit:dtBeta.hit 2:End Sub
Sub BetaT_Hit:dtBeta.hit 3:End Sub
Sub BetaA_Hit:dtBeta.hit 4:End Sub
' Top Saucer
Sub Kicker_Hit:bsTop.AddBall 0:PlaySound "saucer_enter_1":End Sub
Sub Kicker_Unhit:DOF 113, DOFPulse:End Sub
Sub Kicker1_Hit():PlaySound "Ball_Bounce_Playfield_Soft_1":End Sub
Sub Trigger1_hit():PlaySound "saucer_kick":End Sub
' Targets
Sub StargateTarget_Hit:vpmTimer.PulseSw 72:PlaySound "Ball_Bounce_Playfield_Soft_2":End Sub
Sub LeftTarget_Hit:vpmTimer.PulseSw 62:PlaySound "Ball_Bounce_Playfield_Soft_3":End Sub
' Trapped Ball Untrapper
'Sub Untrapper1_Hit:Me.DestroyBall:Untrapper2.Enabled=1:Untrapper2.CreateBall:Untrapper2.Kick 90,10:Controller.B2SSetData 114,1:Controller.B2SSetData 114,0:End Sub
' Ball Image Changers
'Sub BallChanger_Hit:ActiveBall.Image = "Ball":End Sub
'Sub BallChangerRed_Hit:ActiveBall.Image = "BallRed":End Sub
'Sub BallChangerRed_UnHit:ActiveBall.Image = "Ball":End Sub
' Sound Effects Only
'Sub Bounce_Hit(parm):PlaySound "Bounce":End Sub
'Sub LeftFlipper_Collide(parm):PlaySound "Bounce":End Sub
'Sub RightFlipper_Collide(parm):PlaySound "Bounce":End Sub
'Sub GateTopLeft_Hit:PlaySound "Gate":End Sub
'Sub GateTopRight_Hit:PlaySound "Gate":End Sub
'Sub GateRightFlipper_Hit:PlaySound "Gate":End Sub
'Sub BallRollSound_Hit:PlaySound "BallRoll3":End Sub
'Sub DrainSound_Hit:PlaySound "BallRoll1":End Sub
' Ball Launched (Used for GI Control)
'Sub GateLane_Hit:BallLaunched = 1:End Sub

' Solenoids
' ==================================================================
SolCallback(1) = "SolOne"										' 1 Reset Alpha I / Top Left Flasher 3
SolCallback(2) = "SolTwo"										' 2 Reset Alpha II / Top Left Flasher 2
SolCallback(3) = "SolThree"										' 3 Top Kicker / Stargate Flashers
SolCallback(4) = "SolFour"		' 4 Right Under-Plastic Flasher
SolCallback(5) = "SolFive"										' 5 Reset Alpha III / Top Left Flasher 1
SolCallback(6) = "SolSix"										' 6 Reset Beta / Top Right Flasher
SolCallback(7) = "SolSeven"		' 7 Left Under-Plastic Flasher
SolCallback(8) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(9) = "bsTrough.SolIn"
'SolCallback(sLLFlipper) = "vpmSolFlipper LeftFlipper,nothing,"
'SolCallback(sLRFlipper) = "vpmSolFlipper RightFlipper,nothing,"

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

Dim ltxx
Sub SolOne(Enabled)
	If Enabled Then
		If SRelay = 0 Then
			dtAlpha1.DropSol_On
            DOF 115, DOFPulse
		Else
            For each ltxx in LTFlash:ltxx.state=1:Next
			SetLamp 163,1
		End If
	Else
		'If SRelay = 1 Then
        SetLamp 163,0
        For each ltxx in LTFlash:ltxx.state=0:Next
	'End If
End If
End Sub

Dim lmxx, twos
Sub SolTwo(Enabled)
	If Enabled Then
		If SRelay = 0 Then
			dtAlpha2.DropSol_On
            DOF 116, DOFPulse
		Else
			SetLamp 162,1:twos = 1
            For each lmxx in LMFlash:lmxx.state=1:Next
            If LampState(1) = 1 Then
            LSL.state = 1
            End If
		End If
	Else
		'If SRelay = 1 Then
        SetLamp 162,0:twos = twos = 0
        For each lmxx in LMFlash:lmxx.state=0:Next
        LSL.State = 0
	    'End If
End If
End Sub

Sub SolThree(Enabled)
	If Enabled Then
		If SRelay = 0 Then
			bsTop.ExitSol_On
		Else
			SGP.state = 1
            SGP1.state = 1
            SGP2.state = 1
		End If
	Else
            SGP.state = 0
            SGP1.state = 0
            SGP2.state = 0
		If SRelay = 0 Then	
		End If
	End If
End Sub

Sub SolFour(Enabled)
	If Enabled Then
			RPF.state = 1
            RPF1.state = 1
            RPF2.state = 1
		Else
            RPF.state = 0
            RPF1.state = 0
            RPF2.state = 0
	End If
End Sub

Sub SolSeven(Enabled)
	If Enabled Then
			LPF.state = 1
            LPF1.state = 1
            LPF2.state = 1
		Else
            LPF.state = 0
            LPF1.state = 0
            LPF2.state = 0
End If
End Sub

Dim lbxx

Sub SolFive(Enabled)
	If Enabled Then
		If SRelay = 0 Then
			dtAlpha3.DropSol_On
            DOF 117, DOFPulse
		Else
             For each lbxx in LBFlash:lbxx.state=1:Next 
			 SetLamp 161,1
		End If
	Else
		'If SRelay = 1 Then
        SetLamp 161,0
        For each lbxx in LBFlash:lbxx.state=0:Next 
	'End If
End If
End Sub

Dim rtxx

Sub SolSix(Enabled)
	If Enabled Then
		If SRelay = 0 Then
			dtBeta.DropSol_On
            DOF 118, DOFPulse
		Else
			SetLamp 164,1:twos = 1
            For each rtxx in RTFlash:rtxx.state=1:Next
            If LampState(1) = 1 Then
            RSL.state = 1
            End If
		End If
	Else
		'If SRelay = 1 Then 
        SetLamp 164,0:twos = 0
        For each rtxx in RTFlash:rtxx.state=0:Next
        RSL.State = 0
	End If
End Sub

' Relays/Lamp Events
' ==================================================================
OldQRelay = 0:OldTRelay = 0:OldARelay = 0:OldBallRel = 0:OldSRelay = 1:OldStargateRelay = 1:UpOrDown = 1
Set LampCallback = GetRef("UpdateRelays")
Sub UpdateRelays
	SRelay = LampState(12) ' Used for solenoid multiplexing.
	If SRelay <> OldSRelay Then OldSRelay = SRelay
	
	'If GIAlwaysOn = 0 Then
		QRelay = LampState(0)
		If QRelay <> OldQRelay Then 
			If QRelay = 0 Then
				'SetLamp 101, 0 ' Turn off the GI when not playing ('Q' Relay is off).
			End If
			'BallLaunched = 0
			OldQRelay = QRelay
		'End If

		TRelay = LampState(1)
		If TRelay <> OldTRelay Then
			If QRelay = 1 Then
				If TRelay = 0 Then
					If SRelay = 1 Then
						SetLamp 101, 1 ' Turn on the GI when the 'T' Relay is off during play.
					End If
				Else	
                        SetLamp 101, 0
				End If
			End If
			OldTRelay = TRelay
		End If
	End If
	
	ARelay = LampState(13)
	If ARelay <> OldARelay Then
		If ARelay = 1 Then
			AuxLights.Enabled = 1 ' Turn on aux light sequence when 'A' Relay is on.
		Else
			AuxLights.Enabled = 0
			Dim x, y
			For x = 71 To 100 ' Turn off all aux lights when 'A' Relay is off.
				SetLamp x, 0
			Next
		End If
	End If
	OldARelay = ARelay
	
	BallRel = LampState(2)
	If BallRel <> OldBallRel Then
		If BallRel = 0 Then
			If bsTrough.Balls Then bsTrough.ExitSol_On ' Release ball when relay 2 is on.
		End If
	End If
	OldBallRel = BallRel

	StargateRelay = LampState(14)
	If StargateRelay <> OldStargateRelay Then
		If StargateRelay = 0 Then ' Ramp is down when relay 14 is off.
			UpOrDown = -1
			If StargateRamp.HeightBottom <> 0 Then RampMove.Enabled = 1
'			StargateInvisible.Collidable = 1
			StargateRamp.Collidable = 1

			Untrapper1.Enabled = 1
		End If
		If StargateRelay = 1 Then ' Ramp is up when relay 14 is on.
			UpOrDown = 1
			If StargateRamp.HeightBottom <> 60 Then PlaySound SoundFX("RampUp",DOFcontactors)
			If StargateRamp.HeightBottom <> 60 Then RampMove.Enabled = 1
'			StargateInvisible.Collidable = 0
			StargateRamp.Collidable = 0

		End If
	OldStargateRelay = StargateRelay
	End If
	
End Sub

' Auxiliary Lights Timer (Playfield "Robo-Units" and Rear Chaser Lights)
' ==================================================================
' Thanks to Destruk for the lamp schematic!
AuxLightsStep = 0
Sub AuxLights_Timer
	AuxLightsStep = AuxLightsStep + 1
	Select Case AuxLightsStep
		Case 1
			SetLamp 100,0:SetLamp 89,0:SetLamp 90,0
			SetLamp 91,1:SetLamp 71,1:SetLamp 72,1
DOF 100,0
DOF 89,0
DOF 90,0
DOF 91,2
DOF 71,2
DOF 72,2
		Case 2
			SetLamp 91,0:SetLamp 71,0:SetLamp 72,0
			SetLamp 92,1:SetLamp 73,1:SetLamp 74,1
DOF 91,0
DOF 71,0
DOF 72,0
DOF 92,2
DOF 73,2
DOF 74,2
		Case 3
			SetLamp 92,0:SetLamp 73,0:SetLamp 74,0
			SetLamp 93,1:SetLamp 75,1:SetLamp 76,1
DOF 92,0
DOF 73,0
DOF 74,0
DOF 93,2
DOF 75,2
DOF 76,2
		Case 4
			SetLamp 93,0:SetLamp 75,0:SetLamp 76,0
			SetLamp 94,1:SetLamp 77,1:SetLamp 78,1
DOF 93,0
DOF 75,0
DOF 76,0
DOF 94,2
DOF 77,2
DOF 78,2
		Case 5
			SetLamp 94,0:SetLamp 77,0:SetLamp 78,0
			SetLamp 95,1:SetLamp 79,1:SetLamp 80,1
DOF 94,0
DOF 77,0
DOF 78,0
DOF 95,2
DOF 79,2
DOF 80,2
		Case 6
			SetLamp 95,0:SetLamp 79,0:SetLamp 80,0
			SetLamp 96,1:SetLamp 81,1:SetLamp 82,1
DOF 95,0
DOF 79,0
DOF 80,0
DOF 96,2
DOF 81,2
DOF 82,2
		Case 7
			SetLamp 96,0:SetLamp 81,0:SetLamp 82,0
			SetLamp 97,1:SetLamp 83,1:SetLamp 84,1
DOF 96,0
DOF 81,0
DOF 82,0
DOF 97,2
DOF 83,2
DOF 84,2
		Case 8
			SetLamp 97,0:SetLamp 83,0:SetLamp 84,0
			SetLamp 98,1:SetLamp 85,1:SetLamp 86,1
DOF 97,0
DOF 83,0
DOF 84,0
DOF 98,2
DOF 85,2
DOF 86,2
		Case 9
			SetLamp 98,0:SetLamp 85,0:SetLamp 86,0
			SetLamp 99,1:SetLamp 87,1:SetLamp 88,1
DOF 98,0
DOF 85,0
DOF 86,0
DOF 99,2
DOF 87,2
DOF 88,2
		Case 10
			SetLamp 99,0:SetLamp 87,0:SetLamp 88,0
			SetLamp 100,1:SetLamp 89,1:SetLamp 90,1
DOF 99,0
DOF 87,0
DOF 88,0
DOF 100,2
DOF 89,2
DOF 90,2
	End Select
	If AuxLightsStep = 10 Then AuxLightsStep = 0
'DOF 93,0
End Sub

Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
    'RollingSound
End Sub

''*****************************************
''	Ball Shadow
''*****************************************
'
'Dim BallShadow
'BallShadow = Array (BallShadow1, BallShadow2)
'
'Sub BallShadowUpdate()
'    Dim BOT, b, shadowZ
'    BOT = GetBalls
'
'	' render the shadow for each ball
'    For b = 0 to UBound(BOT)
'		BallShadow(b).X = BOT(b).X
'		BallShadow(b).Y = BOT(b).Y + 20
'		'If BOT(b).Z > 90 and BOT(b).Z < 120 Then
'		'	BallShadow(b).visible = 1
'		'Else
'		'	BallShadow(b).visible = 0
'		'End If
'	Next
'End Sub
'
'Dim XBallShadow
'XBallShadow = Array (XBallShadow1, XBallShadow2)
'
'
'
'Sub XBallShadowUpdate()
'    Dim XBOT, c, XshadowZ
'    XBOT = GetBalls
'
'	' render the shadow for each ball
'    For c = 0 to UBound(XBOT)
'		XBallShadow(c).X = XBOT(c).X
'		XBallShadow(c).Y = XBOT(c).Y - 10
'        If ballflare = 1 AND twos = 1 AND XBOT(c).VelY > 0 AND XBOT(c).Y < 1800 Then 
'        XBallShadow(c).visible = 1
'		Else
'		XBallShadow(c).visible = 0
'		End If      
'        Shad_Rot.Interval = NOT XBOT(c).VelY
'	Next
'End Sub

' Ramp Movement Timer
' ==================================================================
Sub RampMove_Timer
	Dim x:x = 20 * UpOrDown
	StargateRamp.HeightBottom = StargateRamp.HeightBottom + x
	StargateHelper.HeightBottom = StargateHelper.HeightBottom + x
	StargateHelper.HeightTop = StargateHelper.HeightTop + x
	RampRefresh.State = 1:RampRefresh.State = 0
	If StargateRamp.HeightBottom = 0 Or StargateRamp.HeightBottom = 60 Then RampMove.Enabled = 0
End Sub

' Rules
' ==================================================================
' Based on Inkochnito's Reproduction Card and JP's script
Dim Msg(20)
Sub Rules()
	Msg(0) = "HOW TO PLAY" &Chr(10)
	Msg(1) = "ROBO-WAR" &Chr(10) &Chr(10)
	Msg(2) = ""
	Msg(3) = "SPECIAL: ADD A LETTER TO R-O-B-O-W-A-R BY COMPLETING EITHER THE"
	Msg(4) = "TOP ROLLOVERS (R-O-B-O), OR BY HITTING THE STROBING"
	Msg(5) = "DROP TARGETS (B-E-T-A). COMPLETING (R-O-B-O-W-A-R)"
	Msg(6) = "LIGHTS A SPECIAL."
	Msg(7) = ""
	Msg(8) = "EXTRA BALL: COMPLETING THE ALPHA DROP TARGET SEQUENCE LIGHTS"
	Msg(9) = "AN EXTRA BALL."
	Msg(10) = ""
	Msg(11) = "MULTIPLIER: ADVANCE MULTIPLIER ON VARIOUS PLAYFIELD TARGETS"
	Msg(12) = "WHEN LIT. SCORE 10,000 TIMES MULTIPLIER FOR EACH"
	Msg(13) = "LETTER AWARDED IN (R-O-B-O-W-A-R) AT THE END OF A BALL"
	Msg(14) = "IN PLAY."
	Msg(15) = ""
	Msg(16) = "MULTI-BONUS: SCORE 5000 TIMES MULTIPLIER FOR EACH DROP TARGET"
	Msg(17) = "HIT DURING MULTI-BALL PLAY. SCORE MULTI-BONUS VALUE IN"
	Msg(18) = "HOLE DURING MULTI-BALL PLAY AND AFTER LAST BALL"
	Msg(19) = "IN PLAY."
	Msg(20) = ""
	Dim x
	For x = 1 To 20
		Msg(0) = Msg(0) + Msg(X) &Chr(13)
    Next
	MsgBox Msg(0), , "         Instructions and Rule Card"
End Sub

' Dip Switches
' ==================================================================
' Gottlieb Robo-War
' originally added by Inkochnito
' Updated Switches 8, 31, and 32
Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm  700,400,"Robo-War - DIP switches"
		.AddFrame 2,4,190,"Maximum Credits",49152,Array("8",0,"10",32768,"15",&H00004000,"20",49152)'dip 15&16
		.AddFrame 2,80,190,"Coin Chute Left and Right Control",&H00002000,Array("Separate",0,"Same",&H00002000)'dip 14
		.AddFrame 2,126,190,"Playfield Special",&H00200000,Array("Special",0,"Extra Ball",&H00200000)'dip 22
		.AddFrame 2,172,190,"Highest Games to Date Control",&H00000020,Array("No Effect",0,"Reset High Games #2-#5 on Power Off",&H00000020)'dip 6
		.AddFrame 2, 218, 190, "Auto-Percentage Control", &H00000080, Array("Disabled (Normal High Score Mode)", 0, "Enabled", &H00000080)'dip 8
        .AddFrame 2, 264, 190, "Alpha Drop Bank Sequence", &H40000000, Array("Also Award ROBOWAR Letter", 0, "Light Extra Ball Only", &H40000000)'dip 31
        .AddFrame 2, 310, 190, "Number of Active ADV X Targets", &H80000000, Array("More", 0, "Less", &H80000000)'dip 32		
		.AddFrame 205,4,190,"Highest Game to Date Awards",&H00C00000,Array("None (Not Displayed)",0,"None",&H00800000,"2 Replay",&H00400000,"3 Replay",&H00C00000)'dip 23&24
		.AddFrame 205,80,190,"Balls/Game",&H01000000,Array("5",0,"3",&H01000000)'dip 25
		.AddFrame 205,126,190,"Replay Limit",&H04000000,Array("No Limit",0,"1",&H04000000)'dip 27
		.AddFrame 205,172,190,"Novelty",&H08000000,Array("Normal",0,"Score 500,000 in Place of Extra Ball and Special",&H08000000)'dip 28
		.AddFrame 205,218,190,"Game Mode",&H10000000,Array("Replay",0,"Extra Ball",&H10000000)'dip 29
		.AddFrame 205,264,190,"3rd Coin Chute Credit Control",&H20000000,Array("No Effect",0,"Add 9",&H20000000)'dip 30
		.AddChk 205,316,180,Array("Match",&H02000000)'dip 26
		.AddChk 205,331,190,Array("Attract Mode Sound",&H00000040)'dip 7
		.AddLabel 50,360,300,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub
Set vpmShowDips = GetRef("editDips")

Sub Primflip_Timer()
	FlipperL.rotz = LeftFlipper.currentangle  + 240
	FlipperR.rotz = RightFlipper.currentangle + 120
	batleftshadow.ObjRotZ = LeftFlipper.CurrentAngle
	batrightshadow.ObjRotZ = RightFlipper.CurrentAngle
'MiddleWheel.ObjRotz = LeftFlipper.currentangle - 90
'MiddleWheel1.ObjRotz = RightFlipper.currentangle - 100
'BallShadowUpdate
'XBallShadowUpdate
End Sub

'****************************************
'  JP's Fading Lamps v5 VP9 Fading only
'      Based on PD's Fading Lights
' SetLamp 0 is Off
' SetLamp 1 is On
' LampState(x) current state
'****************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps() 
LampTimer.Interval = 10
LampTimer.Enabled = 1


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
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.2   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.1 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0 '0.05         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub UpdateLamps

	' I had to create lights for each of the relays, or they wouldn't be 100% reliable:
	XNFadeL 0, l0 ' ['Q' Relay] Game Over Relay
	XNFadeL 1, l1 ' ['T' Relay] Tilt Relay
	XNFadeL 2, l2 ' Ball Release
	' NFadeL 4, l4 ' Sound 16
	'XNFadeL 12, l12 ' ['S' Relay] Solenoid Multiplexer
    L12.State = LampState(12)
	XNFadeL 13, l13 ' ['A' Relay] Robo-Unit and Rear Chaser Light Sequences
	XNFadeL 14, l14 ' Stargate Ramp / Launch Attack

	' These lamps will fade slower during gameplay than they do during the attract sequence.
	If LampState(0) = 1 Then
		XNFadeL 3, l3 ' Fight Again
		XNFadeL 15, l15 ' 1x
		XNFadeL 16, l16 ' 2x
		XNFadeL 17, l17 ' 4x
       XNFadeL 18, l18
		XNFadeL 31, l31 ' Base
		XNFadeL 32, l32 ' III (Alpha)
		XNFadeL 33, l33 ' II (Alpha)
		XNFadeL 34, l34 ' I (Alpha)
		XNFadeL 35, l35 ' Special
		XNFadeL 36, l36 ' Special
		XNFadeL 37, l37 ' Special
		XNFadeL 38, l38 ' Special
		XNFadeL 39, l39 ' Adv X
		XNFadeL 40, l40 ' Adv X
		XNFadeL 41, l41 ' Adv X
		XNFadeL 42, l42 ' Adv X
		XNFadeL 43, l43 ' Battle Station
	Else
		XNFadeL 3, l3' Fight Again
		XNFadeL 15, l15 ' 1x
		XNFadeL 16, l16 ' 2x
		XNFadeL 17, l17 ' 4x
		XNFadeL 18, l18
		XNFadeL 31, l31 ' Base
		XNFadeLm 32, l32 ' III (Alpha)
        XNFadeL 32, l32sc ' III (Alpha) 
		XNFadeL 33, l33 ' II (Alpha)
		XNFadeL 34, l34 ' I (Alpha)
		XNFadeLm 35, l35 ' Special
        XNFadeL 35, l35sc ' Special
		XNFadeL 36, l36 ' Special
		XNFadeLm 37, l37 ' Special
        XNFadeL 37, l37sc ' Special
		XNFadeLm 38, l38 ' Special
        XNFadeL 38, L38sc
		XNFadeL 39, l39 ' Adv X
		XNFadeL 40, l40 ' Adv X
		XNFadeL 41, l41 ' Adv X
		XNFadeL 42, l42 ' Adv X
		XNFadeL 43, l43 ' Battle Station
	End If

	' The rest are always the same speed.
    XNFadeLm 5, l5
    XNFadeL 5, l5a
    XNFadeLm 6, l6
    XNFadeL 6, l6a
	XNFadeLm 7, l7	' (ro)B(owar)
    XNFadeL 7, l7a
	XNFadeLm 8, l8	' (rob)O(war)
    XNFadeL 8, l8a
	XNFadeLm 9, l9	' (robo)W(ar)
	XNFadeL 9, l9a
    XNFadeLm 10, l10	' (robow)A(r)
    XNFadeL 10, l10a
	XNFadeLm 11, l11	' (robowa)R
    XNFadeL 11, l11a
	XNFadeLm 19, l19	' Extra Ball 1
    XNFadeL 19, l19x	' Extra Ball 1
	XNFadeLm 20, l20	' Extra Ball 2
    XNFadeL 20, l20x	' Extra Ball 2
	XNFadeLm 21, l21	' Extra Ball 3
	XNFadeL 21, l21x	' Extra Ball 2
    XNFadeLm 22, l22	' Extra Ball 4
    XNFadeL 22, l22x	' Extra Ball 2
	XNFadeL 23, l23	' R(obo)
	XNFadeL 24, l24	' (r)O(bo)
	XNFadeL 25, l25	' (ro)B(o)
	XNFadeL 26, l26	' (rob)O
	XNFadeL 27, l27	' B(eta)
	XNFadeL 28, l28	' (b)E(ta)
	XNFadeL 29, l29	' (be)T(a)
	XNFadeL 30, l30	' (bet)A
	XNFadeL 44, l44	' Power Surge 
	
	XNFadeLm 45, X45
    XNFadeLm 45, X45a
    XNFadeLm 45, X45b
    XNFadeLm 45, X45c
    XNFadeLm 45, X45d
    XNFadeLm 45, X45r
    XNFadeLm 45, X45sc
    'XNFadeL 45, X45sc1

    XNFadeLm 46, X46
    XNFadeLm 46, X46a
    XNFadeLm 46, X46b
    XNFadeLm 46, X46c
    XNFadeLm 46, X46d
    XNFadeLm 46, X46e
    XNFadeLm 46, X46r
    XNFadeLm 46, X46sc
    'XNFadeL 46, X46sc1
    
    XNFadeLm 47, X47
    XNFadeLm 47, X47a
    XNFadeLm 47, X47b
	XNFadeLm 47, X47c
    XNFadeL 47, X47d

    SetLamp 147,LampState(47)
    Flashm 147, f47
    Flash 147, f47a

    SetLamp 146,LampState(46)
    Flashm 146, f46
    Flashm 146, f46a
    Flash 146, f46b

    SetLamp 145,LampState(45)
    Flash 145, f45

    Flashm 164, RTF
    Flashm 164, RTF1
    Flash 164, RTFDim

    Flash 161, LF61
    
    Flashm 162, LF62
    Flash 162, LTFDim
    Flash 163, LF63

    	' Flashers
'	FadeL 161, l61, l61z ' Top Left Flasher 1
'	FadeL 162, l62, l62z ' Top Left Flasher 2
'	FadeL 163, l63, l63z ' Top Left Flasher 3
'	FadeL 164, l64, l64z ' Top Right Flasher
'	FlashAR 61, l61a, l61b, l61c, FRefresh1, 200, 200 ' Top Left Flasher 1
'	FlashAR 62, l62a, l62b, l62c, FRefresh1, 200, 200 ' Top Left Flasher 2
'	FlashAR 63, l63a, l63b, l63c, FRefresh1, 200, 200 ' Top Left Flasher 3
'	FlashAR 64, l64a, l64b, l64c, FRefresh2, 200, 200 ' Top Right Flasher
'	FlashAR 65, l65a, l65b, l65c, FRefresh3, 200, 200 ' Right Plastic Flasher
'	FlashAR 66, l66a, l66b, l66c, FRefresh4, 200, 200  ' Left Plastic Flasher
	' FlashAR 67 ' Stargate Flasher 1 (Didn't Use)
	' FlashAR 68 ' Stargate Flasher 2 (Didn't Use)
	
	' Auxiliary Lights
	XNFadeL 71, l71
	XNFadeL 72, l72
	XNFadeL 73, l73
	XNFadeL 74, l74
	XNFadeL 75, l75
	XNFadeL 76, l76
	XNFadeL 77, l77
	XNFadeL 78, l78
	XNFadeL 79, l79
	XNFadeL 80, l80
	XNFadeL 81, l81
	XNFadeL 82, l82
	XNFadeL 83, l83
	XNFadeL 84, l84
	XNFadeL 85, l85
	XNFadeL 86, l86
	XNFadeL 87, l87
	XNFadeL 88, l88
	XNFadeL 89, l89
	XNFadeL 90, l90
	
XNFadeLm 91, l91a ' Rear Chaser Light
XNFadeLm 91, l91b ' Rear Chaser Light
XNFadeL 91, l91c ' Rear Chaser Light

SetLamp 191,LampState(91)
Flash 191, F91
	
XNFadeLm 100, l100a ' Rear Chaser Light
XNFadeLm 100, l100b ' Rear Chaser Light
XNFadeL 100, l100c ' Rear Chaser Light

SetLamp 200,LampState(100)
Flash 200, F100

XNFadeLm 92, l92a ' Rear Chaser Light
XNFadeLm 92, l92b ' Rear Chaser Light
XNFadeL 92, l92c ' Rear Chaser Light

SetLamp 192,LampState(92)
Flash 192, F92

XNFadeLm 99, l99a ' Rear Chaser Light
XNFadeLm 99, l99b ' Rear Chaser Light
XNFadeL 99, l99c ' Rear Chaser Light

SetLamp 199,LampState(99)
Flash 199, F99

XNFadeLm 93, l93a ' Rear Chaser Light
XNFadeLm 93, l93b ' Rear Chaser Light
XNFadeL 93, l93c ' Rear Chaser Light

SetLamp 193,LampState(93)
Flash 193, F93

XNFadeLm 98, l98a ' Rear Chaser Light
XNFadeLm 98, l98b ' Rear Chaser Light
XNFadeL 98, l98c ' Rear Chaser Light

SetLamp 198,LampState(98)
Flash 198, F98

XNFadeLm 94, l94a ' Rear Chaser Light
XNFadeLm 94, l94b ' Rear Chaser Light
XNFadeL 94, l94c ' Rear Chaser Light

SetLamp 194,LampState(94)
Flash 194, F94

XNFadeLm 97, l97a ' Rear Chaser Light
XNFadeLm 97, l97b ' Rear Chaser Light
XNFadeL 97, l97c ' Rear Chaser Light

SetLamp 197,LampState(97)
Flash 197, F97

XNFadeLm 95, l95a ' Rear Chaser Light
XNFadeLm 95, l95b ' Rear Chaser Light
XNFadeL 95, l95c ' Rear Chaser Light

SetLamp 195, LampState(95)
Flash 195, F95

XNFadeLm 96, l96a ' Rear Chaser Light
XNFadeLm 96, l96b ' Rear Chaser Light
XNFadeL 96, l96c ' Rear Chaser Light

SetLamp 196,LampState(96)
Flash 196, F96
    
Dim gixx

If LampState(1) = 1 Then
For each gixx in GI:gixx.state = 0:next
'DOF 150,0
else
For each gixx in GI:gixx.state = 1:next
'DOF 150,1
End If

If LampState(1) = 1 AND gion = 1 Then
PlaySound "fx_relay"
DOF 103,0
gion = 0
End If

If LampState(1) = 0 AND gion = 0 Then
PlaySound "fx_relay"
DOF 103,1
gion = 1
End If

If LampState(1) = 1 Then
Table1.ColorGradeImage = "ColorGrade_off"
else
Table1.ColorGradeImage = luts(lutpos)
End If

if Alpha1.isdropped AND LampState(1) < 1 Then
FL1.opacity = 0
else 
FL1.opacity = 55
End If

if Alpha2A.isdropped AND LampState(1) < 1 Then
FL2.opacity = 0
else 
FL2.opacity = 55
End If

if Alpha2B.isdropped AND LampState(1) < 1 Then
FL3.opacity = 0
else 
FL3.opacity = 55
End If

if Alpha3A.isdropped AND LampState(1) < 1 Then
FL4.opacity = 0
else 
FL4.opacity = 55
End If

if Alpha3B.isdropped AND LampState(1) < 1 Then
FL5.opacity = 0
else 
FL5.opacity = 55
End If

if Alpha3C.isdropped AND LampState(1) < 1 Then
FL6.opacity = 0
else 
FL6.opacity = 55
End If

if betab.isdropped AND LampState(1) < 1 Then
FLb.opacity = 0
else 
FLb.opacity = 55
End If

if betae.isdropped AND LampState(1) < 1 Then
FLe.opacity = 0
else 
FLe.opacity = 55
End If

if betat.isdropped AND LampState(1) < 1 Then
FLt.opacity = 0
else 
FLt.opacity = 55
End If

if betaa.isdropped AND LampState(1) < 1 Then
FLa.opacity = 0
else 
FLa.opacity = 55
End If

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

'Walls

Sub FadeW(nr, a, b, c)
    Select Case LampState(nr)
        Case 2:c.IsDropped = 1:LampState(nr) = 0                 'Off
        Case 3:b.IsDropped = 1:c.IsDropped = 0:LampState(nr) = 2 'fading...
        Case 4:a.IsDropped = 1:b.IsDropped = 0:LampState(nr) = 3 'fading...
        Case 5:c.IsDropped = 1:b.IsDropped = 0:LampState(nr) = 6 'ON
        Case 6:b.IsDropped = 1:a.IsDropped = 0:LampState(nr) = 1 'ON
    End Select
End Sub

Sub FadeWm(nr, a, b, c)
    Select Case LampState(nr)
        Case 2:c.IsDropped = 1
        Case 3:b.IsDropped = 1:c.IsDropped = 0
        Case 4:a.IsDropped = 1:b.IsDropped = 0
        Case 5:c.IsDropped = 1:b.IsDropped = 0
        Case 6:b.IsDropped = 1:a.IsDropped = 0
    End Select
End Sub

Sub NFadeW(nr, a)
    Select Case LampState(nr)
        Case 4:a.IsDropped = 1:LampState(nr) = 0
        Case 5:a.IsDropped = 0:LampState(nr) = 1
    End Select
End Sub

Sub NFadeWm(nr, a)
    Select Case LampState(nr)
        Case 4:a.IsDropped = 1
        Case 5:a.IsDropped = 0
    End Select
End Sub

Sub NFadeWi(nr, a)
    Select Case LampState(nr)
        Case 4:a.IsDropped = 0:LampState(nr) = 0
        Case 5:a.IsDropped = 1:LampState(nr) = 1
    End Select
End Sub

Sub NFadeWim(nr, a)
    Select Case LampState(nr)
        Case 4:a.IsDropped = 0
        Case 5:a.IsDropped = 1
    End Select
End Sub

'Lights

Sub XNFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub XNFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

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
        '   Object.IntensityScale = 1
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
         ' Object.IntensityScale = 1
End Sub

Sub FadeL(nr, a, b)
    Select Case LampState(nr)
        Case 2:b.state = 0:LampState(nr) = 0
        Case 3:b.state = 1:LampState(nr) = 2
        Case 4:a.state = 0:LampState(nr) = 3
        Case 5:b.state = 1:LampState(nr) = 6
        Case 6:a.state = 1:LampState(nr) = 1
    End Select
End Sub

Sub FastL(nr, a, b)
    Select Case LampState(nr)
        Case 2:b.state = 0:LampState(nr) = 0
        Case 3:b.state = 1:LampState(nr) = 2
        Case 4:a.state = 0:LampState(nr) = 3
        Case 5:a.state = 1:LampState(nr) = 1
    End Select
End Sub

Sub FadeLm(nr, a, b)
    Select Case LampState(nr)
        Case 2:b.state = 0
        Case 3:b.state = 1
        Case 4:a.state = 0
        Case 5:b.state = 1
        Case 6:a.state = 1
    End Select
End Sub

Sub NFadeL(nr, a)
    Select Case LampState(nr)
        Case 4:a.state = 0:LampState(nr) = 0
        Case 5:a.State = 1:LampState(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, a)
    Select Case LampState(nr)
        Case 4:a.state = 0
        Case 5:a.State = 1
    End Select
End Sub

Sub FadeOldL(nr, a, b, c)
    Select Case LampState(nr)
        Case 2:c.state = 0:LampState(nr) = 0
        Case 3:b.state = 0:c.state = 1:LampState(nr) = 2
        Case 4:a.state = 0:b.state = 1:LampState(nr) = 3
        Case 5:a.state = 0:c.state = 0:b.state = 1:LampState(nr) = 6
        Case 6:b.state = 0:a.state = 1:LampState(nr) = 1
    End Select
End Sub

Sub FadeOldLm(nr, a, b, c)
    Select Case LampState(nr)
        Case 2:c.state = 0
        Case 3:b.state = 0:c.state = 1
        Case 4:a.state = 0:b.state = 1
        Case 5:a.state = 0:c.state = 0:b.state = 1
        Case 6:b.state = 0:a.state = 1
    End Select
End Sub

'Reels

Sub FadeR(nr, a)
    Select Case LampState(nr)
        Case 2:a.SetValue 3:LampState(nr) = 0
        Case 3:a.SetValue 2:LampState(nr) = 2
        Case 4:a.SetValue 1:LampState(nr) = 3
        Case 5:a.SetValue 1:LampState(nr) = 6
        Case 6:a.SetValue 0:LampState(nr) = 1
    End Select
End Sub

Sub FadeRm(nr, a)
    Select Case LampState(nr)
        Case 2:a.SetValue 3
        Case 3:a.SetValue 2
        Case 4:a.SetValue 1
        Case 5:a.SetValue 1
        Case 6:a.SetValue 0
    End Select
End Sub

'Texts

Sub NFadeT(nr, a, b)
    Select Case LampState(nr)
        Case 4:a.Text = "":LampState(nr) = 0
        Case 5:a.Text = b:LampState(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, a, b)
    Select Case LampState(nr)
        Case 4:a.Text = ""
        Case 5:a.Text = b
    End Select
End Sub

' Flash a light, not controlled by the rom

Sub FlashL(nr, a, b)
    Select Case LampState(nr)
        Case 1:b.state = 0:LampState(nr) = 0
        Case 2:b.state = 1:LampState(nr) = 1
        Case 3:a.state = 0:LampState(nr) = 2
        Case 4:a.state = 1:LampState(nr) = 3
        Case 5:b.state = 1:LampState(nr) = 4
    End Select
End Sub

' Light acting as a flash. C is the light number to be restored

Sub MFadeL(nr, a, b, c)
    Select Case LampState(nr)
        Case 2:b.state = 0:LampState(nr) = 0
            If LampState(c) = 1 Then SetLamp c, 1
        Case 3:b.state = 1:LampState(nr) = 2
        Case 4:a.state = 0:LampState(nr) = 3
        Case 5:a.state = 1:LampState(nr) = 1
    End Select
End Sub

' Added in version 5 : lights made with alpha ramps
' a, b, c and d are the ramps from on to off
' r is the refresh light
' wt is the top width of the ramp
' wb is the bottom width of the ramp

Sub FadeAR(nr, a, b, c, d, r, wt, wb)
    Select Case LampState(nr)
        Case 2:c.WidthBottom = 0:c.WidthTop = 0
            d.WidthBottom = wb:d.WidthTop = wt
            r.State = ABS(r.state -1)
            LampState(nr) = 0 'Off
        Case 3:b.WidthBottom = 0:b.WidthTop = 0
            c.WidthBottom = wb:c.WidthTop = wt
            r.State = ABS(r.state -1)
            LampState(nr) = 2 'fading...
        Case 4:a.WidthBottom = 0:a.WidthTop = 0
            b.WidthBottom = wb:b.WidthTop = wt
            r.State = ABS(r.state -1)
            LampState(nr) = 3 'fading...
        Case 5:d.WidthBottom = 0:d.WidthTop = 0
            b.WidthBottom = wb:b.WidthTop = wt
            r.State = ABS(r.state -1)
            LampState(nr) = 6 ' 1/2 ON
        Case 6:b.WidthBottom = 0:b.WidthTop = 0
            a.WidthBottom = wb:a.WidthTop = wt
            r.State = ABS(r.state -1)
            LampState(nr) = 1 'ON
    End Select
End Sub

Sub FadeARm(nr, a, b, c, d, r, wt, wb)
    Select Case LampState(nr)
        Case 2:c.WidthBottom = 0:c.WidthTop = 0
            d.WidthBottom = wb:d.WidthTop = wt
            r.State = ABS(r.state -1)
        Case 3:b.WidthBottom = 0:b.WidthTop = 0
            c.WidthBottom = wb:c.WidthTop = wt
            r.State = ABS(r.state -1)
        Case 4:a.WidthBottom = 0:a.WidthTop = 0
            b.WidthBottom = wb:b.WidthTop = wt
            r.State = ABS(r.state -1)
        Case 5:d.WidthBottom = 0:d.WidthTop = 0
            b.WidthBottom = wb:b.WidthTop = wt
            r.State = ABS(r.state -1)
        Case 6:b.WidthBottom = 0:b.WidthTop = 0
            a.WidthBottom = wb:a.WidthTop = wt
            r.State = ABS(r.state -1)
    End Select
End Sub

Sub FlashAR(nr, a, b, c, r, wt, wb) 'used for reflections when the off is transparent - no ramp
    Select Case LampState(nr)
        Case 2:c.WidthBottom = 0:c.WidthTop = 0
            r.State = ABS(r.state -1)
            LampState(nr) = 0 'Off
        Case 3:b.WidthBottom = 0:b.WidthTop = 0
            c.WidthBottom = wb:c.WidthTop = wt
            r.State = ABS(r.state -1)
            LampState(nr) = 2 'fading...
        Case 4:a.WidthBottom = 0:a.WidthTop = 0
            b.WidthBottom = wb:b.WidthTop = wt
            r.State = ABS(r.state -1)
            LampState(nr) = 3 'fading...
        Case 5:b.WidthBottom = wb:b.WidthTop = wt
            r.State = ABS(r.state -1)
            LampState(nr) = 6 ' 1/2 ON
        Case 6:b.WidthBottom = 0:b.WidthTop = 0
            a.WidthBottom = wb:a.WidthTop = wt
            r.State = ABS(r.state -1)
            LampState(nr) = 1 'ON
    End Select
End Sub

Sub FlashARm(nr, a, b, c, r, wt, wb) 'used for reflections when the off is transparent - no ramp
    Select Case LampState(nr)
        Case 2:c.WidthBottom = 0:c.WidthTop = 0
            r.State = ABS(r.state -1)
        Case 3:b.WidthBottom = 0:b.WidthTop = 0
            c.WidthBottom = wb:c.WidthTop = wt
            r.State = ABS(r.state -1)
        Case 4:a.WidthBottom = 0:a.WidthTop = 0
            b.WidthBottom = wb:b.WidthTop = wt
            r.State = ABS(r.state -1)
        Case 5:b.WidthBottom = wb:b.WidthTop = wt
            r.State = ABS(r.state -1)
        Case 6:b.WidthBottom = 0:b.WidthTop = 0
            a.WidthBottom = wb:a.WidthTop = wt
            r.State = ABS(r.state -1)
    End Select
End Sub

Sub NFadeAR(nr, a, b, r, wt, wb) ' a is the ramp on, b if the ramp off
    Select Case LampState(nr)
        Case 4:a.WidthBottom = 0:a.WidthTop = 0
            b.WidthBottom = wb:b.WidthTop = wt
            r.State = ABS(r.state -1)
            LampState(nr) = 0 'off
        Case 5:a.WidthBottom = wb:a.WidthTop = wt
            b.WidthBottom = 0:b.WidthTop = 0
            r.State = ABS(r.state -1)
            LampState(nr) = 1 'on
    End Select
End Sub

Sub NFadeARm(nr, a, b, r, wt, wb) ' a is the ramp on, b if the ramp off
    Select Case LampState(nr)
        Case 4:a.WidthBottom = 0:a.WidthTop = 0
            b.WidthBottom = wb:b.WidthTop = wt
            r.State = ABS(r.state -1)
        Case 5:a.WidthBottom = wb:a.WidthTop = wt
            b.WidthBottom = 0:b.WidthTop = 0
            r.State = ABS(r.state -1)
    End Select
End Sub

'--------- ADDED by JF
Sub BallRelease_UnHit(): DOF 112,2:End Sub
Sub Untrapper2_UnHit(): Untrapper2.Enabled = 0 : End Sub
'--------- END ADDED by JF

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

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
                StopSound("BallRoll_" & b)
				StopSound("Wireloop" & b)
        Next

        ' exit the sub if no balls on the table
        If UBound(BOT) = -1 Then Exit Sub

        ' play the rolling sound for each ball

        For b = 0 to UBound(BOT)
				If BallVel(BOT(b) ) > 1 Then
					rolling(b) = True
						if BOT(b).z < 40 Then ' Ball on playfield
							PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
							StopSound("Wireloop" & b)
						Else ' Ball on raised ramp
							PlaySound ("Wireloop" & b), -1, VolPlayfieldRoll(BOT(b)) * 2.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
							StopSound("BallRoll_" & b)
						End If
                Else
                        If rolling(b) = True Then
                                StopSound("BallRoll_" & b)
								StopSound("Wireloop" & b)
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
' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************


Sub Metals_Thin_Hit (idx)
	Metals_Hit(idx)
End Sub


Sub Shad_Rot_Timer()
'XBallShadow1.ObjRotZ = XBallShadow1.ObjRotZ + 1
'XBallShadow2.ObjRotZ = XBallShadow2.ObjRotZ + 1
End Sub

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
