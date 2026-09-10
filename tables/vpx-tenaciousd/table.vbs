Option Explicit
Randomize
 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
 

'Const BallsSize = 50

Const cGameName="fpwr2_l2",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM "01560000", "S7.VBS", 3.26

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8

'Flipper Rampup mode: 0 = fast, 1 = medium, 2 = slow (tap passes should work)
dim FlipperCoilRampupMode : FlipperCoilRampupMode = 1

'Flipper Rubberizer: 0 = normal flip rubber, 1 = more lively rubber for flips, 2 = different rubberizer
Const RubberizerEnabled = 1 		

Dim DesktopMode: DesktopMode = Table1.ShowDT	

If DesktopMode = True Then 'Show Desktop components
lrail.visible=1
rrail.visible=1

Else
lrail.visible=1
rrail.visible=1

End if

'///////////////////////-----VR Options-----///////////////////////
Const VRRoomChoice = 1 '1 = Mega Room, 0 = Minimal Room

'----- VR Room Auto-Detect -----
Dim VRRoom, VR_Obj, VRMode
Const VRTest = 0				' 1 = Testing VR in Live View, 0 = Do not force VR mode.

If RenderingMode = 2 or VRTest = 1 Then
	VRMode = True
	primitive060.visible=0
	lrail.visible=0
	rrail.visible=0
	Wall001.sidevisible=0
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 1 : Next
	For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 1 : Next
	If VRRoomChoice = 1 Then
		For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 1 : Next
	Else
		For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 1 : Next
		For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 0 : Next
	End If
Else
	VRMode = False
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 0 : Next
End If

leftflipper.visible=0
rightflipper.visible=0
'************************
'Glowball
'*************************
Dim GlowAura,GlowIntensity

Const ChooseBall 			= 7	' *** Ball Settings **********
									' *** 0 = Normal Ball	
									' *** 1 = Purple GlowBall
									' *** 2 = Green GlowBall																		
									' *** 3 = Blue Glowball
									' *** 4 = Orange Glowball 
									' *** 5 = Red Glowball
									' *** 6 = White Glowball
									' *** 7 = Yellow Glowball
									' *** 8 = Gold Glowball
									

'******************
'Additional Ball Settings
'******************

GlowAura=155 'GlowBlob Auroa radius
GlowIntensity=4 'Glowblob intensity


'****Model animation timeres******
dim countr
countr = 0
boxingtimer.enabled = 1
dim countrr
countr = 0
baxingtimer.enabled = 1
dim countr4
countr = 0
buxingtimer.enabled = 1
dim countr3
countr3 = 0
BEZtimer.enabled = 0
dim countr5
countr5 = 0
bixingtimer.enabled = 0

'==================================================================
'Setup Solenoids
'==================================================================

SolCallback(1) = "bsTrough.SolIn"
SolCallback(2) = "bsTrough.SolOut"
SolCallback(3) = "bsSaucer.SolOut"
SolCallback(4) = "FLOrbitFlasher"
SolCallback(15) = "SolBell"
SolCallback(11) = "GILights"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
 
Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
         If Enabled Then
                LF.Fire  'leftflipper.rotatetoend
                If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
                        RandomSoundReflipUpLeft LeftFlipper
                Else 
                        SoundFlipperUpAttackLeft LeftFlipper
                        RandomSoundFlipperUpLeft LeftFlipper
                End If                
        Else
                LeftFlipper.RotateToStart
                If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
                        RandomSoundFlipperDownLeft LeftFlipper
                End If
                FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub
 
Sub SolRFlipper(Enabled)
        If Enabled Then
                RF.Fire 'rightflipper.rotatetoend
                If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
                        RandomSoundReflipUpRight RightFlipper
                Else 
                        SoundFlipperUpAttackRight RightFlipper
                        RandomSoundFlipperUpRight RightFlipper
                End If
        Else
                RightFlipper.RotateToStart
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


'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

Sub SolBell(enabled)
	If enabled Then
		Playsound "Ding_01", 0, 1, -0.05, 0.05
	End If
End Sub

Sub GILights(Enabled)
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 0: Next
		playsound"Fx_Relay"
		If VRMode = True Then
			VR_BG_ON.visible=0
		End if
	Else
		For each xx in GI:xx.State = 1: Next
		playsound"Fx_Relay"
		If VRMode = True Then
			VR_BG_ON.visible=1
		End if
	End If
End Sub

 Sub sGameOn(Enabled)
	If Enabled Then

	Else

	End If
End Sub

sub FLOrbitFlasher(enabled)
	if enabled Then
	light62.state = LightStateOn
	Else
	light62.state = lightstateoff	
	End If
End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, BsSaucer

Sub Table1_Init
    Playsound "vo_Welcome"
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
         If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Firepower II (Williams 1983)" & vbNewLine & "Created for VPX by Walamab"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
        .Games("fpwr2_l2").Settings.Value("sound") = 0
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0
	PinMAMETimer.Interval	= PinMAMEInterval
	PinMAMETimer.Enabled	= True

     vpmNudge.TiltSwitch = 7
     vpmNudge.Sensitivity = 5
     vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, Bumper4, LeftSlingshot, RightSlingshot)


     Set bsTrough = New cvpmBallStack
         bsTrough.InitSw 9, 11, 10, 0, 0, 0, 0, 0
         bsTrough.InitKick BallRelease, 90, 10
'         bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
         bsTrough.Balls = 2


     Set bsSaucer = New cvpmBallStack
         bsSaucer.InitSaucer sw23, 23, -90, 15
'         bsSaucer.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
         bsSaucer.KickAngleVar = 1.5
		 bsSaucer.KickForceVar = 4

   Glowball_Init 'Start Glowballs

	if VRMode = True Then
		setup_backglass()
	End If
End Sub

	

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************
Sub table1_KeyDown(ByVal Keycode)
	If keycode = LeftFlipperKey Then
		VR_CabFlipperLeft.X = VR_CabFlipperLeft.X +10
	End If

	If keycode = RightFlipperKey Then
		VR_CabFlipperRight.X = VR_CabFlipperRight.X - 10
	End If

	If keycode = PlungerKey Then
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If

	If Keycode = StartGameKey Then	
		VR_Cab_StartButton.y = VR_Cab_StartButton.y -5
	End If

    If KeyCode = RightMagnaSave Then NextTrack
    If KeyCode = LeftMagnaSave Then EndMusic
    If keycode = StartGameKey Then EndMusic
	If keycode = StartGameKey Then NextTrack
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = LeftMagnaSave Then bLutActive = True
    If keycode = RightMagnaSave Then
        If bLutActive Then NextLUT:End If
    End If
    If vpmKeyDown(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_PlungerPull", Plunger:Plunger.Pullback
End Sub


'*********
 
Sub Table1_KeyUp(ByVal keycode)
	If vpmKeyUp(KeyCode) Then Exit Sub

	If keycode = PlungerKey Then
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		VR_Primary_plunger.Y = -350		
	end if

	If keycode = LeftFlipperKey Then
		VR_CabFlipperLeft.X = VR_CabFlipperLeft.X -10
	End If

	If keycode = RightFlipperKey Then
		VR_CabFlipperRight.X = VR_CabFlipperRight.X +10
	End If

	If Keycode = StartGameKey Then	
		VR_Cab_StartButton.y = VR_Cab_StartButton.y +5
	End If

	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
 
    If keycode = LeftMagnaSave Then bLutActive = False
	If keycode=RightFlipperKey then Controller.Switch(57)=0
	If keycode=LeftFlipperKey then Controller.Switch(57)=0
        If KeyCode = PlungerKey Then
                Plunger.Fire
                If BIPL = 1 Then
                        SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
                Else
                        SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
                End If
        End If


End Sub 

Sub TimerPlunger_Timer

  If VR_Primary_plunger.Y < -230 then
      VR_Primary_plunger.Y = VR_Primary_plunger.Y + 5
  End If
End Sub

Sub TimerPlunger2_Timer
	VR_Primary_plunger.Y = -350 + (5* Plunger.Position) -20
End Sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()

	If VRMode = False Then
		DisplayTimer
	End If

	If VRMode = True Then
		VRDisplayTimer
	End If

End Sub

'**Ramp Teleport**

dim teleportball:teleportball=1

Sub sw51_Hit()
    PlaySoundAt "fx_kicker_enter", sw51
    PlaySound "portal"
    if teleportball = 1 then
    sw51.DestroyBall
    sw52.Createball
    kickoutsw52
    exit sub
    end if
    kickoutsw51
End Sub

sub kickoutsw51
PlaySoundAt "fx_kicker", sw51
sw51.Kick 180, 20
end sub

sub kickoutsw52
PlaySoundAt "fx_kicker", sw52
sw52.Kick 0, 40, 1.5
end sub

'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

    If TypeName(MyLight) = "Light" Then

        If FinalState = 2 Then
            FinalState = MyLight.State 'Keep the current light state
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then

        Dim steps

        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
        If FinalState = 2 Then                      'Keep the current flasher state
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

        ' Start blink timer and create timer subroutine
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub

'**********************************************************************************************************
' Fleep BIPL
'**********************************************************************************************************
Dim BIPL : BIPL=0

Sub sw12_Hit:Controller.Switch(12) = 1:BIPL = 1:End Sub
Sub sw12_Unhit:Controller.Switch(12) = 0:BIPL = 0:End Sub

' Drain hole and kickers

Sub Drain_Hit:bsTrough.addball me:RandomSoundDrain Drain:End Sub
Sub BallRelease_Unhit:RandomSoundBallRelease BallRelease: End Sub

Sub sw23_Hit:SoundSaucerLock:bsSaucer.AddBall 0
    ShakeBez1
    ShakeBez2
    ShakeBez3
    SwapBEZ1
    FlashForMs plasflash, 1000, 50, 0
    FlashForMs pentagram001, 1000, 50, 0
    FlashForMs pentagram002, 1000, 50, 0
if BEZtimer.enabled = false Then
   BEZtimer.enabled = True
   Beztimerstop.enabled = True
	end if
End Sub
Sub sw23_UnHit:SoundSaucerKick 1, sw23
 ShakeBez1
 ShakeBez2
 ShakeBez3
 SwapBackBEZ1
 FlashForMs plasflash, 1000, 50, 0
 FlashForMs pentagram001, 1000, 50, 0
 FlashForMs pentagram002, 1000, 50, 0
if BEZtimer.enabled = false Then
   BEZtimer.enabled = True
   Beztimerstop.enabled = True
	end if
End Sub

'Rollovers
Sub sw34_Hit:Controller.Switch(34) = 1:End Sub
Sub sw34_Unhit:Controller.Switch(34) = 0:End Sub
Sub sw35_Hit:Controller.Switch(35) = 1:End Sub
Sub sw35_Unhit:Controller.Switch(35) = 0:End Sub
Sub sw36_Hit:Controller.Switch(36) = 1:End Sub
Sub sw36_Unhit:Controller.Switch(36) = 0:End Sub
Sub sw37_Hit:Controller.Switch(37) = 1:End Sub
Sub sw37_Unhit:Controller.Switch(37) = 0:End Sub
Sub sw38_Hit:Controller.Switch(38) = 1:End Sub
Sub sw38_Unhit:Controller.Switch(38) = 0:End Sub
Sub sw39_Hit:Controller.Switch(39) = 1:End Sub
Sub sw39_Unhit:Controller.Switch(39) = 0:End Sub
Sub sw45_Hit:Controller.Switch(45) = 1:End Sub
Sub sw45_Unhit:Controller.Switch(45) = 0:End Sub
Sub sw46_Hit:Controller.Switch(46) = 1:End Sub
Sub sw46_Unhit:Controller.Switch(46) = 0:End Sub

'Targets
Sub sw25_Hit:vpmTimer.PulseSw(25):End Sub
Sub sw26_Hit:vpmTimer.PulseSw(26):End Sub
Sub sw27_Hit:vpmTimer.PulseSw(27):End Sub
Sub sw28_Hit:vpmTimer.PulseSw(28):End Sub
Sub sw29_Hit:vpmTimer.PulseSw(29):End Sub
Sub sw30_Hit:vpmTimer.PulseSw(30):End Sub
Sub sw31_Hit:vpmTimer.PulseSw(31):End Sub
Sub sw32_Hit:vpmTimer.PulseSw(32):End Sub
Sub sw33_Hit:vpmTimer.PulseSw(33):End Sub
Sub sw24_Hit:vpmTimer.PulseSw(24):End Sub

'Gates

Sub Gate2_Hit():vpmTimer.PulseSw 14 : End Sub
Sub Gate3_Hit():vpmtimer.PulseSw 15 : End Sub
Sub Gate4_Hit():vpmTimer.PulseSw 19 : End Sub

'Spinner
Sub sw18_Spin():vpmTimer.PulseSw 18 : Playsound "fx_spinner" 
if bixingtimer.enabled = false Then
		bixingtimer.enabled = True
		bixingtimerstop.enabled = True
End If
 End Sub

'Rubbers
Sub sw20_Hit():vpmTimer.PulseSw 20:End Sub
Sub sw22_Hit():vpmTimer.PulseSw 22:End Sub
Sub sw16_Hit():vpmTimer.PulseSw 16:End Sub
Sub sw17_Hit():vpmTimer.PulseSw 17:End Sub


'**********Bumpers***********

Sub Bumper1_Hit
vpmTimer.PulseSw(43) 
RandomSoundBumperTop Bumper1 
FlBumperFadeTarget(1) = 1 'Flupper bumper demo
Bumper1.timerenabled = True
FlashForMs bumpersmalllight1, 1000, 50, 0
End Sub

Sub Bumper1_Timer
	FlBumperFadeTarget(1) = 0
End Sub

Sub Bumper2_Hit
vpmTimer.PulseSw(42)
RandomSoundBumperMiddle Bumper2 
FlBumperFadeTarget(2) = 1 'Flupper bumper demo
Bumper2.timerenabled = True
FlashForMs bumpersmalllight2, 1000, 50, 0
End Sub

Sub Bumper2_Timer
	FlBumperFadeTarget(2) = 0
End Sub

Sub Bumper3_Hit 
vpmtimer.pulsesw(44) 
RandomSoundBumperBottom Bumper3
FlBumperFadeTarget(3) = 1 'Flupper bumper demo
Bumper3.timerenabled = True
FlashForMs bumpersmalllight3, 1000, 50, 0
End Sub

Sub Bumper3_Timer
	FlBumperFadeTarget(3) = 0
End Sub
Sub Bumper4_Hit
vpmTimer.PulseSw(41)
RandomSoundBumperBottom Bumper4
FlBumperFadeTarget(4) = 1 'Flupper bumper demo
Bumper4.timerenabled = True
FlashForMs bumpersmalllight4, 1000, 50, 0
End Sub

Sub Bumper4_Timer
	FlBumperFadeTarget(4) = 0
End Sub


'******************************************************
' 	ZFLB:  FLUPPER BUMPERS
'******************************************************
' Based on FlupperBumpers 0.145 final

' Explanation of how these bumpers work:
' There are 10 elements involved per bumper:
' - the shadow of the bumper ( a vpx flasher object)
' - the bumper skirt (primitive)
' - the bumperbase (primitive)
' - a vpx light which colors everything you can see through the bumpertop
' - the bulb (primitive)
' - another vpx light which lights up everything around the bumper
' - the bumpertop (primitive)
' - the VPX bumper object
' - the bumper screws (primitive)
' - the bulb highlight VPX flasher object
' All elements have a special name with the number of the bumper at the end, this is necessary for the fading routine and the initialisation.
' For the bulb and the bumpertop there is a unique material as well per bumpertop.
' To use these bumpers you have to first copy all 10 elements to your table.
' Also export the textures (images) with names that start with "Flbumper" and "Flhighlight" and materials with names that start with "bumper".
' Make sure that all the ten objects are aligned on center, if possible with the exact same x,y coordinates
' After that copy the script (below); also copy the BumperTimer vpx object to your table
' Every bumper needs to be initialised with the FlInitBumper command, see example below;
' Colors available are red, white, blue, orange, yellow, green, purple and blacklight.
' In a GI subroutine you can then call set the bumperlight intensity with the "FlBumperFadeTarget(nr) = value" command
' where nr is the number of the bumper, value is between 0 (off) and 1 (full on) (so you can also use 0.3 0.4 etc).

' Notes:
' - There is only one color for the disk; you can photoshop it to a different color
' - The bumpertops are angle independent up to a degree; my estimate is -45 to + 45 degrees horizontally, 0 (topview) to 70-80 degrees (frontview)
' - I built in correction for the day-night slider; this might not work perfectly, depending on your table lighting
' - These elements, textures and materials do NOT integrate with any of the lighting routines I have seen in use in many VPX tables
'   (just find the GI handling routine and insert the FlBumperFadeTarget statement)
' - If you want to use VPX native bumperdisks just copy my bumperdisk but make it invisible

' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0
	DNA45 = (NightDay - 10) / 20
	DNA90 = 0
	DayNightAdjust = 0.4
Else
	DNA30 = (NightDay - 10) / 30
	DNA45 = (NightDay - 10) / 45
	DNA90 = (NightDay - 10) / 90
	DayNightAdjust = NightDay / 25
End If

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6)
Dim cnt
For cnt = 1 To 6
	FlBumperActive(cnt) = False
Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight
FlInitBumper 1, "blacklight"
FlInitBumper 2, "green"
FlInitBumper 3, "red"
FlInitBumper 4, "orange"

' ### uncomment the statement below to change the color for all bumpers ###
'   Dim ind
'   For ind = 1 To 5
'	   FlInitBumper ind, "green"
'   Next

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1
	FlBumperFadeTarget(nr) = 1.1
	FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr)
	FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr)
	Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr)
	Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr)
	FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr)
	FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	
	' set the color for the two VPX lights
	Select Case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0)
			FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0)
			FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
			
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255)
			FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255)
			FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8)
			FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32)
			FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255)
			MaterialColor "bumpertopmat" & nr, RGB(16,255,16)
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0)
			FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8)
			FlBumperBigLight(nr).colorfull = RGB(255,190,8)
			
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190)
			FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
			
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255)
			FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4)
			FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50)
			FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			FlBumperSmallLight(nr).TransmissionScale = 0
			
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255)
			FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255)
			FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	End Select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
	'   UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
	'			   OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
	'			   float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 ) * DayNightAdjust
	
	Select Case FlBumperColor(nr)
		Case "blue"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(38 - 24 * Z,130 - 98 * Z,255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z + 0.97 * Z ^ 3)
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z ^ 3) / (0.5 + DNA90)
			
		Case "green"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(16 + 16 * Sin(Z * 3.14),255,16 + 16 * Sin(Z * 3.14)), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z ^ 3) / (1 + DNA90)
			
		Case "red"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 16 - 11 * Z + 16 * Sin(Z * 3.14),0), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z * 4,8 - Z * 8)
			
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 100 - 22 * z + 16 * Sin(Z * 3.14),Z * 32), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z * 50, 0)
			
		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 8 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z ^ 3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20 * Z,255 - 65 * Z)
			FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20 * Z,255 - 65 * Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z * 36,220 - Z * 90)
			
		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 1, RGB(30 - 27 * Z ^ 0.03,30 - 28 * Z ^ 0.01, 255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z ^ 3
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z ^ 3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255 - 240 * (Z ^ 0.1),255 - 240 * (Z ^ 0.1),255)
			FlBumperSmallLight(nr).colorfull = RGB(255 - 200 * z,255 - 200 * Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255 - 190 * Z,235 - z * 180,220 + 35 * Z)
			
		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 180 + 40 * z, 48 * Z), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)
			
		Case "purple"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(128 - 118 * Z - 32 * Sin(Z * 3.14), 32 - 26 * Z ,255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15 + 200 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z + 0.97 * Z ^ 3)
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z ^ 3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128 - 60 * Z,32,255)
	End Select
End Sub

Sub BumperTimer_Timer
	Dim nr
	For nr = 1 To 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) And FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) And FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	Next
End Sub
'*******Tfiggers*********


Sub Trigger1_Hit()
	Wall32.IsDropped = 1
	OrbitGate.RotY = 10
	vpmTimer.PulseSw 40
End Sub

Sub Trigger2_Hit()
	if Wall32.IsDropped Then
		Wall32.IsDropped = False
		OrbitGate.Roty = 0
	End If
End Sub

'*******************************************
'  Ramp Triggers
'*******************************************
Sub ramptrigger01_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger02_hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub ramptrigger02_unhit()
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
End Sub

Sub ramptrigger03_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub ramptrigger03_unhit()
	PlaySoundAt "WireRamp_Stop", ramptrigger03
End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    ShakeRightCat
    FlashForMs Flasherspaulding2, 1000, 50, 0
	vpmTimer.PulseSw 48
    RandomSoundSlingshotRight SLING1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    ShakeLeftCat
    FlashForMs Flasherspaulding1, 1000, 50, 0
	vpmTimer.PulseSw 47
    RandomSoundSlingshotLeft SLING2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub

'======================================================================================
' Mesh gate animations based on overlated VPX gates with visible = 0
'======================================================================================
dim gate2angle
Sub Gate2_Timer():gate2angle = 180 - gate2.CurrentAngle:LeftRampGate.RotX = -gate2angle:End Sub

dim gate3angle
Sub Gate3_Timer():gate3angle = 180 - gate3.CurrentAngle:RightRampGate.RotX = -gate3angle:End Sub

dim gate4angle
Sub Gate4_Timer():gate4angle = 180 - gate4.CurrentAngle:SpinnerGate.RotX = gate4angle:End Sub

dim gate1angle
Sub Gate1_Timer():gate1angle = 180 - gate1.CurrentAngle:ShooterLaneGate.RotX = gate1angle:End Sub
'======================================================================================

'******************************
'*     HiRez00: Music Mod     *
'******************************

Dim musicNum
Sub NextTrack

If musicNum = 0 Then PlayMusic "TEND/The Metal.mp3" End If
If musicNum = 1 Then PlayMusic "TEND/Tribute.mp3" End If
If musicNum = 2 Then PlayMusic "TEND/Explosivo.mp3" End If
If musicNum = 3 Then PlayMusic "TEND/Master Exploder.mp3" End If
If musicNum = 4 Then PlayMusic "TEND/Kickapoo.mp3" End If
If musicNum = 5 Then PlayMusic "TEND/Classico.mp3" End If
If musicNum = 6 Then PlayMusic "TEND/Fuck Her Gently.mp3" End If
If musicNum = 7 Then PlayMusic "TEND/The Who Medley.mp3" End If
If musicNum = 8 Then PlayMusic "TEND/Beelzeboss.mp3" End If
If musicNum = 9 Then PlayMusic "TEND/History.mp3" End If
If musicNum = 10 Then PlayMusic "TEND/The Government Totally Sucks.mp3" End If
If musicNum = 11 Then PlayMusic "TEND/The Divide.mp3" End If
If musicNum = 12 Then PlayMusic "TEND/Papagenu.mp3" End If
If musicNum = 13 Then PlayMusic "TEND/Dude.mp3" End If
If musicNum = 14 Then PlayMusic "TEND/Break In-City.mp3" End If
If musicNum = 15 Then PlayMusic "TEND/Car Chase City.mp3" End If
If musicNum = 16 Then PlayMusic "TEND/Destiny.mp3" End If
If musicNum = 17 Then PlayMusic "TEND/POD.mp3" End If
If musicNum = 18 Then PlayMusic "TEND/Wicked Game.mp3" End If
If musicNum = 19 Then PlayMusic "TEND/Cave Intro.mp3" End If
If musicNum = 20 Then PlayMusic "TEND/Jesus Ranch.mp3" End If
If musicNum = 21 Then PlayMusic "TEND/Rize of The Fenix.mp3" End If
If musicNum = 22 Then PlayMusic "TEND/Low Hangin' Fruit.mp3" End If
If musicNum = 23 Then PlayMusic "TEND/Classical Teacher.mp3" End If
If musicNum = 24 Then PlayMusic "TEND/Senorita.mp3" End If
If musicNum = 25 Then PlayMusic "TEND/Deth Starr.mp3" End If
If musicNum = 26 Then PlayMusic "TEND/Roadie.mp3" End If
If musicNum = 27 Then PlayMusic "TEND/The Ballad of Hollywood Jack and the Rage Kage.mp3" End If
If musicNum = 28 Then PlayMusic "TEND/Throwdown.mp3" End If
If musicNum = 29 Then PlayMusic "TEND/Rock Is Dead.mp3" End If
If musicNum = 30 Then PlayMusic "TEND/They Fucked Our Asses.mp3" End If
If musicNum = 31 Then PlayMusic "TEND/To Be The Best.mp3" End If
If musicNum = 32 Then PlayMusic "TEND/39.mp3" End If
If musicNum = 33 Then PlayMusic "TEND/Kielbasa.mp3" End If
If musicNum = 34 Then PlayMusic "TEND/One Note Song.mp3" End If
If musicNum = 35 Then PlayMusic "TEND/Wonderboy.mp3" End If
If musicNum = 36 Then PlayMusic "TEND/Hard Fucking.mp3" End If
If musicNum = 37 Then PlayMusic "TEND/Dio.mp3" End If
If musicNum = 38 Then PlayMusic "TEND/Inward Singing.mp3" End If
If musicNum = 39 Then PlayMusic "TEND/Kyle Quit The Band.mp3" End If
If musicNum = 40 Then PlayMusic "TEND/The Road.mp3" End If
If musicNum = 41 Then PlayMusic "TEND/Cock Pushups.mp3" End If
If musicNum = 42 Then PlayMusic "TEND/Lee.mp3" End If
If musicNum = 43 Then PlayMusic "TEND/Friendship.mp3" End If
If musicNum = 44 Then PlayMusic "TEND/Karate.mp3" End If
If musicNum = 45 Then PlayMusic "TEND/Rock Your Socks.mp3" End If
If musicNum = 46 Then PlayMusic "TEND/Drive-Thru.mp3" End If
If musicNum = 47 Then PlayMusic "TEND/Double Team.mp3" End If
If musicNum = 48 Then PlayMusic "TEND/City Hall.mp3" End If
If musicNum = 49 Then PlayMusic "TEND/Baby.mp3" End If
musicNum = (musicNum + 1) mod 50

End Sub

Sub Table1_MusicDone
        NextTrack

End Sub

'*****************************
'*       End Music Mod       *
'*****************************
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
 
Sub UpdateLamps
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
     NFadeLm 41, l41a
    NFadeL 41, l41
    NFadeLm 42, l42a
    NFadeL 42, l42
    NFadeLm 43, l43a
    NFadeL 43, l43
    NFadeLm 44, L44a
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
    NFadeL 59, l59
    NFadeL 60, l60

 
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
 
 'Reels
Sub FadeReel(nr, reel)
    Select Case FadingLevel(nr)
        Case 2:FadingLevel(nr) = 0
        Case 3:FadingLevel(nr) = 2
        Case 4:reel.Visible = 0:FadingLevel(nr) = 3
        Case 5:reel.Visible = 1:FadingLevel(nr) = 1
    End Select
End Sub
 
 'Inverted Reels
Sub FadeIReel(nr, reel)
    Select Case FadingLevel(nr)
        Case 2:FadingLevel(nr) = 0
        Case 3:FadingLevel(nr) = 2
        Case 4:reel.Visible = 1:FadingLevel(nr) = 3
        Case 5:reel.Visible = 0:FadingLevel(nr) = 1
    End Select
End Sub

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
 
Dim Digits(32)
' 1st Player
Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)
Digits(6) = Array(LED70,LED71,LED72,LED73,LED74,LED75,LED76)
 
' 2nd Player
Digits(7) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(8) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(9) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(10) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(11) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(12) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)
Digits(13) = Array(LED140,LED141,LED142,LED143,LED144,LED145,LED146)
 
' 3rd Player
Digits(14) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(15) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(16) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(17) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(18) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(19) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)
Digits(20) = Array(LED210,LED211,LED212,LED213,LED214,LED215,LED216)
 
' 4th Player
Digits(21) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(22) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(23) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(24) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(25) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(26) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)
Digits(27) = Array(LED280,LED281,LED282,LED283,LED284,LED285,LED286)
 
' Credits
Digits(28) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(29) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)
' Balls
Digits(30) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(31) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)
 
Sub DisplayTimer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		If DesktopMode = True and VRMode = False Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 32) then
				For Each obj In Digits(num)
					If chg And 1 Then obj.State = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			else
			end if
		next
		end if
end if
End Sub


'******************************************************
'                BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 5 ' total number of balls
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
                rolling(b) = False
                StopSound("BallRoll_" & b)
        Next

        ' exit the sub if no balls on the table
           If UBound(BOT) =  - 1 Then Exit Sub

'Rotate the idols
idol2.Rotz = 90 - (BOT(0).Y)\15
idol3.Rotz = 90 - (BOT(0).Y)\15
idol1.Rotz = -90 + (BOT(0).Y)\15
idol4.Rotz = -90 + (BOT(0).Y)\15
Bez001.Rotz = 90 - (BOT(0).Y)\15
Bez002.Rotz = 90 - (BOT(0).Y)\15

        ' play the rolling sound for each ball

        For b = 0 to UBound(BOT)
                If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
                        rolling(b) = True
                        PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

                Else
                        If rolling(b) = True Then
                                StopSound("BallRoll_" & b)
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
        Next
End Sub


'******************************************************
'**** RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(6)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub


Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function



'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************
 
'*************
'   JP'S LUT
'*************
 
Dim bLutActive, LUTImage
Sub LoadLUT
Dim x
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub
 
Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub
 
Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 11: UpdateLUT: SaveLUT: End Sub
 
Sub UpdateLUT
Select Case LutImage
Case 0: table1.ColorGradeImage = "LUT0"
Case 1: table1.ColorGradeImage = "LUT1"
Case 2: table1.ColorGradeImage = "LUT2"
Case 3: table1.ColorGradeImage = "LUT3"
Case 4: table1.ColorGradeImage = "LUT4"
Case 5: table1.ColorGradeImage = "LUT5"
Case 6: table1.ColorGradeImage = "LUT6"
Case 7: table1.ColorGradeImage = "LUT7"
Case 8: table1.ColorGradeImage = "LUT8"
Case 9: table1.ColorGradeImage = "LUT9"
Case 10: table1.ColorGradeImage = "LUT10"
 
End Select
End Sub
 
 
'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************
 
sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

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
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/21)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/21)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 4 
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub
 
 
 
'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'                  Start nFOZZY FLIPPERS'
'******************************************************
 
'Flipper Correction Initialization late 80s to early 90s
dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
 
InitPolarity
 
Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 80
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -3.7        
        AddPt "Polarity", 2, 0.33, -3.7
        AddPt "Polarity", 3, 0.37, -3.7
        AddPt "Polarity", 4, 0.41, -3.7
        AddPt "Polarity", 5, 0.45, -3.7 
        AddPt "Polarity", 6, 0.576,-3.7
        AddPt "Polarity", 7, 0.66, -2.3
        AddPt "Polarity", 8, 0.743, -1.5
        AddPt "Polarity", 9, 0.81, -1
        AddPt "Polarity", 10, 0.88, 0

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
'			FLIPPER CORRECTION FUNCTIONS
'******************************************************
 
Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub
 
Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
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
 
	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
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
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
 
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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next
 
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)						'find safety coefficient 'ycoef' data
			End If
 
			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
	 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
 
				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
 
				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If
 
			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
 
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'playsound "fx_knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class
 
'******************************************************
'		FLIPPER POLARITY AND RUBBER DAMPENER
'			SUPPORTING FUNCTIONS 
'******************************************************
 
' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
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
Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
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
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
 
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper
 
	LinearEnvelope = Y
End Function
 
' Used for drop targets and stand up targets
Function Atn2(dy, dx)
	dim pi
	pi = 4*Atn(1)
 
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
 
' Used for drop targets and flipper tricks
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function
 
'******************************************************
'			FLIPPER TRICKS
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
 
	If abs(Flipper1.currentangle) < abs(Endangle1) + 3 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
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
					'debug.print "flippernudge!!"
					BOT(b).velx = BOT(b).velx /1.5
					BOT(b).vely = BOT(b).vely - 1
				end If
			Next
		End If
	Else 
		If abs(Flipper1.currentangle) > abs(Endangle1) + 30 then EOSNudge1 = 0
	End If
End Sub
 
'*****************
' Maths
'*****************
Const Pi = 3.1415927
 
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
Const EOSTnew = 1 '0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 4.5
	Case 2:
		SOSRampup = 8.5
End Select
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.035 '0.025
 
 
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
	Dir = Flipper.startangle/Abs(Flipper.startangle)	'-1 for Right Flipper
 
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
Const LiveDistanceMax = 117  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
 
Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce															'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount
 
	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.8 Then						'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)	'Partial catch when catch happens a bit late
		end If
 
		'debug.print "Live catch! Bounce: " & LiveCatchBounce
 
		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (16 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	End If
End Sub
 
'*****************************************************************************************************
'END nFOZZY FLIPPERS'
 
 
'PHYSICS DAMPENERS
 
'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR
 
Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub
 
Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub
 
 
dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least
 
dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85
 
Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
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
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
 
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub
 
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub
 
 
	Public Sub Report() 	'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub
 
 
End Class
 
'******************************************************
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

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

 
Sub RDampen_Timer()
Cor.Update
End Sub
 
 

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1                                                                                                                'volume level; range [0, 1]
NudgeLeftSoundLevel = 1                                                                                                        'volume level; range [0, 1]
NudgeRightSoundLevel = 1                                                                                                'volume level; range [0, 1]
NudgeCenterSoundLevel = 1                                                                                                'volume level; range [0, 1]
StartButtonSoundLevel = 0.1                                                                                                'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr                                                                                        'volume level; range [0, 1]
PlungerPullSoundLevel = 1                                                                                                'volume level; range [0, 1]
RollingSoundFactor = 1.1/5                

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010                                                           'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635                                                                'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                                                                        'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                                                                      'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
SlingshotSoundLevel = 0.95                                                                                                'volume level; range [0, 1]
BumperSoundFactor = 4.25                                                                                                'volume multiplier; must not be zero
KnockerSoundLevel = 1                                                                                                         'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2                                                                        'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5                                                                                        'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5                                                                                        'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5                                                                                'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025                                                                        'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025                                                                        'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8                                                                        'volume level; range [0, 1]
WallImpactSoundFactor = 0.075                                                                                        'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5                                                                                                        'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10                                                                                        'volume multiplier; must not be zero
DTSoundLevel = 0.25                                                                                                                'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                                                                      'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8                                                                                                                'volume level; range [0, 1]
BallReleaseSoundLevel = 1                                                                                                'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2                                                                        'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015                                                                                'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5                                                                                                        'volume multiplier; must not be zero


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

Dim tablewidth, tableheight : tablewidth = table1.width : tableheight = table1.height

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
        Dim tmp
    tmp = tableobj.y * 2 / tableheight-1
    If tmp > 0 Then
                AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1
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
        PlaySoundAtLevelStatic SoundFXDOF("Bumpers_Middle_" & Int(Rnd*5)+1, 111, DOFPulse, DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
        PlaySoundAtLevelStatic SoundFXDOF("Bumpers_Bottom_" & Int(Rnd*5)+1, 112, DOFPulse, DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
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
'                                        End Mechanical Sounds
'/////////////////////////////////////////////////////////////////
 
' Thalamus : Exit in a clean and proper way
Sub Table1_exit()
  If B2SOn Then 
    Controller.Pause = False
    Controller.Stop
  Controller.Games("fpwr2_l2").Settings.Value("sound") = 1
  End If
End Sub


'================================================================
' Define Easy Names for Switch Numbers
'================================================================
'Const sPlumbTilt = 1
'Const sBallRollTilt = 2
'Const sCreditButton = 3
'Const sRightCoin = 4
'Const sCenterCoin = 5
'Const sLeftCoin = 6
'Const sSlamTilt = 7
'Const sHighScoreReset = 8
'Const sDrain = 9
'Const sRightBallRamp = 10
'Const sLeftBallRamp = 11
'Const sBallShooter = 12
'Const sLaneChange = 13
'Const sRampInRollUnder = 14
'Const sRampOutRollUnder = 15
'Const sCenterLeftStandup = 16
'Const sStandup = 17
'Const sSpinner = 18
'Const sOrbitInRollUnder = 19
'Const sUpperLeftStandup = 20
'Const sUpperRightStandup = 22
'Const sEjectHole = 23
'Const sReleaseTarget = 24
'Const sFtgt = 25
'Const sItgt = 26
'Const sRtgt = 27
'Const sEtgt = 28
'Const sPtgt = 29
'Const sOtgt = 30
'Const sWtgt = 31
'Const sEtgt2 = 32
'Const sRtgt2 = 33
'Const sA = 34
'Const sB = 35
'Const sC = 36
'Const sD = 37
'Const sLeftOutlane = 38
'Const sRightOutlane = 39
'Const sOrbitOutGate = 40
'Const sLowerLeftBumper = 41
'Const sUpperRightBumper = 42
'Const sUpperLeftBumper = 43
'Const sLowerRightBumper = 44
'Const sLeftReturnLane = 45
'Const sRightReturnLane = 46
'Const sLeftSlingshot = 47
'Const sRightSlingshot = 48
'Const sPlayfieldTilt = 49


'================================================================
' Define Easy Names for Solenoid Numbers
'================================================================
'Const cOutHole = 1
'Const cBallRampThrower = 2
'Const cEjectHole = 3
'Const cOrbitFlasher = 4
'Const cGIRelay = 11
'Const cBell = 15
'Const cCoinLockout = 16
'Const cLeftSlingShot = 17
'Const cRightSlingShot = 18
'Const cLowerLeftBumper = 19
'Const cUpperRightBumper = 20
'Const cUpperLeftBumper = 21
'Const cLowerRightBumper = 22
'********************
' Real Time updatess
'********************
'used for all the real time updates


Sub RealTime_Timer
    RollingTimer_Timer()
    pick.RotX = Sw18.CurrentAngle
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
'****inserts******

pon25.blenddisablelighting = L25.getinplayintensity * 40
poff25.blenddisablelighting = L25.getinplayintensity + 1
pon26.blenddisablelighting = L26.getinplayintensity * 40
poff26.blenddisablelighting = L26.getinplayintensity + 1
pon27.blenddisablelighting = L27.getinplayintensity * 40
poff27.blenddisablelighting = L27.getinplayintensity + 1
pon28.blenddisablelighting = L28.getinplayintensity * 40
poff28.blenddisablelighting = L28.getinplayintensity + 1
pon29.blenddisablelighting = L29.getinplayintensity * 40
poff29.blenddisablelighting = L29.getinplayintensity + 1
pon30.blenddisablelighting = L30.getinplayintensity * 40
poff30.blenddisablelighting = L30.getinplayintensity + 1
pon31.blenddisablelighting = L31.getinplayintensity * 40
poff31.blenddisablelighting = L31.getinplayintensity + 1
pon32.blenddisablelighting = L32.getinplayintensity * 40
poff32.blenddisablelighting = L32.getinplayintensity + 1
pon33.blenddisablelighting = L33.getinplayintensity * 40
poff33.blenddisablelighting = L33.getinplayintensity + 1
pon56.blenddisablelighting = L56.getinplayintensity * 40
poff56.blenddisablelighting = L56.getinplayintensity + 1
pon57.blenddisablelighting = L57.getinplayintensity * 40
poff57.blenddisablelighting = L57.getinplayintensity + 1
pon58.blenddisablelighting = L58.getinplayintensity * 40
poff58.blenddisablelighting = L58.getinplayintensity + 1
pon8.blenddisablelighting = L8.getinplayintensity * 40
poff8.blenddisablelighting = L8.getinplayintensity + 1
pon60.blenddisablelighting = L60.getinplayintensity * 40
poff60.blenddisablelighting = L60.getinplayintensity + 1
pon59.blenddisablelighting = L59.getinplayintensity * 40
poff59.blenddisablelighting = L59.getinplayintensity + 1
pon34.blenddisablelighting = L34.getinplayintensity * 40
poff34.blenddisablelighting = L34.getinplayintensity + 1
pon35.blenddisablelighting = L35.getinplayintensity * 40
poff35.blenddisablelighting = L35.getinplayintensity + 1
pon36.blenddisablelighting = L36.getinplayintensity * 40
poff36.blenddisablelighting = L36.getinplayintensity + 1
pon37.blenddisablelighting = L37.getinplayintensity * 40
poff37.blenddisablelighting = L37.getinplayintensity + 1
pon55.blenddisablelighting = L55.getinplayintensity * 40
poff55.blenddisablelighting = L55.getinplayintensity + 1
pon62.blenddisablelighting = Light62.getinplayintensity * 40
poff62.blenddisablelighting = Light62.getinplayintensity + 1
pon52.blenddisablelighting = L52.getinplayintensity * 40
poff52.blenddisablelighting = L52.getinplayintensity + 1
pon53.blenddisablelighting = L53.getinplayintensity * 40
poff53.blenddisablelighting = L53.getinplayintensity + 1
pon54.blenddisablelighting = L54.getinplayintensity * 40
poff54.blenddisablelighting = L54.getinplayintensity + 1
pon19.blenddisablelighting = L19.getinplayintensity * 40
poff19.blenddisablelighting = L19.getinplayintensity + 1
pon20.blenddisablelighting = L20.getinplayintensity * 40
poff20.blenddisablelighting = L20.getinplayintensity + 1
pon21.blenddisablelighting = L21.getinplayintensity * 40
poff21.blenddisablelighting = L21.getinplayintensity + 1
pon22.blenddisablelighting = L22.getinplayintensity * 40
poff22.blenddisablelighting = L22.getinplayintensity + 1
pon48.blenddisablelighting = L48.getinplayintensity * 40
poff48.blenddisablelighting = L48.getinplayintensity + 1
pon47.blenddisablelighting = L47.getinplayintensity * 40
poff47.blenddisablelighting = L47.getinplayintensity + 1
pon38.blenddisablelighting = L38.getinplayintensity * 40
poff38.blenddisablelighting = L38.getinplayintensity + 1
pon39.blenddisablelighting = L39.getinplayintensity * 40
poff39.blenddisablelighting = L39.getinplayintensity + 1
pon46.blenddisablelighting = L46.getinplayintensity * 40
poff46.blenddisablelighting = L46.getinplayintensity + 1
pon45.blenddisablelighting = L45.getinplayintensity * 40
poff45.blenddisablelighting = L45.getinplayintensity + 1
pon49.blenddisablelighting = L49.getinplayintensity * 40
poff49.blenddisablelighting = L49.getinplayintensity + 1
pon50.blenddisablelighting = L50.getinplayintensity * 40
poff50.blenddisablelighting = L50.getinplayintensity + 1
pon51.blenddisablelighting = L51.getinplayintensity * 40
poff51.blenddisablelighting = L51.getinplayintensity + 1
pon24.blenddisablelighting = L24.getinplayintensity * 40
poff24.blenddisablelighting = L24.getinplayintensity + 1
pon23.blenddisablelighting = L23.getinplayintensity * 40
poff23.blenddisablelighting = L23.getinplayintensity + 1
pon7.blenddisablelighting = L7.getinplayintensity * 40
poff7.blenddisablelighting = L7.getinplayintensity + 1
pon6.blenddisablelighting = L6.getinplayintensity * 40
poff6.blenddisablelighting = L6.getinplayintensity + 1
pon9.blenddisablelighting = L9.getinplayintensity * 40
poff9.blenddisablelighting = L9.getinplayintensity + 1
pon10.blenddisablelighting = L10.getinplayintensity * 40
poff10.blenddisablelighting = L10.getinplayintensity + 1
pon11.blenddisablelighting = L11.getinplayintensity * 40
poff11.blenddisablelighting = L11.getinplayintensity + 1
pon12.blenddisablelighting = L12.getinplayintensity * 40
poff12.blenddisablelighting = L12.getinplayintensity + 1
pon13.blenddisablelighting = L13.getinplayintensity * 40
poff13.blenddisablelighting = L13.getinplayintensity + 1
pon14.blenddisablelighting = L14.getinplayintensity * 40
poff14.blenddisablelighting = L14.getinplayintensity + 1
pon15.blenddisablelighting = L15.getinplayintensity * 40
poff15.blenddisablelighting = L15.getinplayintensity + 1
pon16.blenddisablelighting = L16.getinplayintensity * 40
poff16.blenddisablelighting = L16.getinplayintensity + 1
pon17.blenddisablelighting = L17.getinplayintensity * 40
poff17.blenddisablelighting = L17.getinplayintensity + 1
pon18.blenddisablelighting = L18.getinplayintensity * 40
poff18.blenddisablelighting = L18.getinplayintensity + 1

End Sub

'*******Animations******

Sub boxingtimer_Timer
countr = countr + 1 : If Countr > 2 then Countr = 1 : end If
select case countr
case 1 : idol2.z=55:idol3.z=-200
case 2 : idol2.z=-200:idol3.z=55

end Select
End Sub 

Sub baxingtimer_Timer
countrr = countrr + 1 : If Countrr > 2 then Countrr = 1 : end If
select case countrr
case 1 : idol1.z=55:idol4.z=-200
case 2 : idol1.z=-200:idol4.z=55

end Select
End Sub 

Sub buxingtimer_Timer
countr4 = countr4 + 1 : If Countr4 > 2 then Countr4 = 1 : end If
select case countr4
case 1 : tog001.z=55:tog002.z=-250
case 2 : tog001.z=-250:tog002.z=55

end Select
End Sub 

Sub BEZtimer_Timer
countr3 = countr3 + 1 : If Countr3 > 2 then Countr3 = 1 : end If
select case countr
case 1 : bez001.z=56:bez002.z=-260
case 2 : bez001.z=-260:bez002.z=56

end Select
End Sub 

Sub BEZtimerstop_Timer()
	BEZtimer.enabled = False
	BEZtimerstop.enabled = False
End Sub

Sub bixingtimer_Timer
countr5 = countr5 + 1 : If Countr5 > 2 then Countr5 = 1 : end If
select case countr5
case 1 : piz001.z=56:piz002.z=-260
case 2 : piz001.z=-260:piz002.z=56

end Select
End Sub 

Sub bixingtimerstop_Timer()
	bixingtimer.enabled = False
	bixingtimerstop.enabled = False
End Sub



'**************************
' PORTAL 
'**************************

Dim PORT1Pos, PORTFLOW
PORTFLOW = Array("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", _
    "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24")

Sub StartPORT
    PORT1Pos = 0
   
    PORTTimer.Enabled = 1
End Sub

Sub PORTTimer_Timer
    'debug.print fire1pos
    PORT1.ImageA = PORTFLOW(PORT1Pos)
    
    PORT1Pos = (PORT1Pos + 1) MOD 24    
    
End Sub


'**************************
' ELECTRICITY
'**************************

Dim PORT2Pos, PORTFLOW2
PORTFLOW2 = Array("L_1", "L_2", "L_3", "L_4", "L_5", "L_6", "L_7", "L_8", "L_9", _
    "L_10", "L_11", "L_12", "L_13", "L_14", "L_15", "L_16", "L_17", "L_18", "L_19", "L_20", "L_21", "L_22", "L_23", "L_24")

Sub StartPORT2
    PORT2Pos = 0
   
    PORT2Timer.Enabled = 1
End Sub

Sub PORT2Timer_Timer
    'debug.print fire1pos
    PORT2.ImageA = PORTFLOW2(PORT2Pos)
    
    PORT2Pos = (PORT2Pos + 1) MOD 24    
    
End Sub

'*******Beez*********

' Shake Bez
Dim Bez1Pos, Bez2Pos, Bez3Pos

Sub ShakeBez1
    Bez1Pos = 8
    Bez1Timer.Enabled = 1
End Sub

Sub Bez1Timer_Timer
    Bez001.TransY = Bez1Pos
    If Bez1Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Bez1Pos < 0 Then
        Bez1Pos = ABS(Bez1Pos)- 1
    Else
        Bez1Pos = - Bez1Pos + 1
    End If
End Sub

Sub ShakeBez2
    Bez2Pos = 8
    Bez2Timer.Enabled = 1
End Sub

Sub Bez2Timer_Timer
    Bez002.TransY = Bez2Pos
    If Bez2Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Bez2Pos < 0 Then
        Bez2Pos = ABS(Bez2Pos)- 1
    Else
        Bez2Pos = - Bez2Pos + 1
    End If
End Sub

Sub ShakeBez3
    Bez3Pos = 8
    Bez3Timer.Enabled = 1
End Sub

Sub Bez3Timer_Timer
    Bez003.TransY = Bez3Pos
    If Bez3Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Bez3Pos < 0 Then
        Bez3Pos = ABS(Bez3Pos)- 1
    Else
        Bez3Pos = - Bez3Pos + 1
    End If
End Sub

'***********SWAPS***************

Sub SwapBEZ1
	BEZ001.visible = False
	BEZ002.visible = False
    BEZ003.visible = True
    Port2.visible = True
    tog001.visible = True
    tog002.visible = True
End Sub

Sub SwapBackBEZ1
	BEZ001.visible = True
	BEZ002.visible = True
    BEZ003.visible = False
	Port2.visible = False
    tog001.visible = False
    tog002.visible = False
End Sub

'**********************************************************************************************************
'*********** Glowball Section *****************************************************************************
Dim GlowBall, CustomBulbIntensity(10)
Dim  GBred(10)
Dim GBgreen(10)
Dim GBblue(10)
Dim CustomBallImage(10), CustomBallLogoMode(10), CustomBallDecal(10), CustomBallGlow(10)


' default Ball
CustomBallGlow(0) = 		False
CustomBallImage(0) = 		"ball"
CustomBallLogoMode(0) = 	False
CustomBallDecal(0) = 		""
CustomBulbIntensity(0) = 	0.01
GBred(0) = 0 : GBgreen(0)	= 0 : GBblue(0) = 0

' Purple GlowBall
CustomBallGlow(1) = 		True
CustomBallImage(1) = 		"glowball purple"
CustomBallLogoMode(1) = 	True
CustomBallDecal(1) = 		""
CustomBulbIntensity(1) = 	0
GBred(1) = 255 : GBgreen(1)	= 0 : GBblue(1) = 255


' green GlowBall
CustomBallGlow(2) = 		True
CustomBallImage(6) = 		"glowball green"
CustomBallLogoMode(6) = 	True
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
GBred(2) = 100 : GBgreen(2)	= 255 : GBblue(2) = 100

' blue GlowBall
CustomBallGlow(3) = 		True
CustomBallImage(3) = 		"glowball blue"
CustomBallLogoMode(3) = 	True
CustomBallDecal(3) = 		""
CustomBulbIntensity(3) = 	0
GBred(3) = 50 : GBgreen(3)	= 50 : GBblue(3) = 255


' Orange GlowBall
CustomBallGlow(4) = 		True
CustomBallImage(4) = 		"glowball orange"
CustomBallLogoMode(4) = 	True
CustomBallDecal(4) = 		""
CustomBulbIntensity(4) = 	0
GBred(4) = 255 : GBgreen(4)	= 165 : GBblue(4) = 000


' red GlowBall
CustomBallGlow(5) = 		True
CustomBallImage(5) = 		"glowball red"
CustomBallLogoMode(5) = 	True
CustomBallDecal(5) = 		""
CustomBulbIntensity(5) = 	0
GBred(5) = 255 : GBgreen(5)	= 99 : GBblue(5) = 71

' white GlowBall
CustomBallGlow(6) = 		True
CustomBallImage(6) = 		"glowball white"
CustomBallLogoMode(6) = 	True
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
GBred(6) = 255 : GBgreen(6)	= 255 : GBblue(6) = 255

' yellow GlowBall
CustomBallGlow(7) = 		True
CustomBallImage(7) = 		"glowball yellow"
CustomBallLogoMode(7) = 	True
CustomBallDecal(7) = 		""
CustomBulbIntensity(7) = 	0
GBred(7) = 255 : GBgreen(7)	= 255 : GBblue(7) = 000

' gold GlowBall
CustomBallGlow(8) = 		True
CustomBallImage(8) = 		"glowball gold"
CustomBallLogoMode(8) = 	True
CustomBallDecal(8) = 		""
CustomBulbIntensity(8) = 	0
GBred(8) = 255 : GBgreen(8)	= 215 : GBblue(8) = 000



' *** prepare the variable with references to three lights for glow ball ***
Dim Glowing(10)
Set Glowing(0) = Glowball1 : Set Glowing(1) = Glowball2 : Set Glowing(2) = Glowball3 : Set Glowing(3) = Glowball4


'*** change ball appearance ***

Sub ChangeBall(ballnr)
	Dim BOT, ii, col
	table1.BallDecalMode = CustomBallLogoMode(ballnr)
	table1.BallFrontDecal = CustomBallDecal(ballnr)
	table1.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
	table1.BallImage = CustomBallImage(ballnr)
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 3
		col = RGB(GBred(ballnr), GBgreen(ballnr), GBblue(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
	Next
End Sub

' *** Ball Shadow code / Glow Ball code / Primitive Flipper Update ***

Dim BallShadowArray
BallShadowArray = Array (BallShadow1, BallShadow2, BallShadow3,BallShadow004,BallShadow005)
Const anglecompensate = 15

Sub GraphicsTimer_Timer()
	Dim BOT, b
    BOT = GetBalls

	' switch off glowlight for removed Balls
	IF GlowBall Then
		For b = UBound(BOT) + 1 to 3
			If GlowBall and Glowing(b).state = 1 Then Glowing(b).state = 0 End If
		Next
	End If

    For b = 0 to UBound(BOT)
		' *** move ball shadow for max 3 balls ***
'		If BallShadow and b < 3 Then
'			If BOT(b).X < table1.Width/2 Then
'				BallShadowArray(b).X = ((BOT(b).X) - (50/6) + ((BOT(b).X - (table1.Width/2))/7)) + 10
'			Else
'				BallShadowArray(b).X = ((BOT(b).X) + (50/6) + ((BOT(b).X - (table1.Width/2))/7)) - 10
'			End If
'			BallShadowArray(b).Y = BOT(b).Y + 20 : BallShadowArray(b).Z = 1
'			If BOT(b).Z > 20 Then BallShadowArray(b).visible = 1 Else BallShadowArray(b).visible = 0 End If
'		End If
		' *** move glowball light for max 3 balls ***
		If GlowBall and b < 4 Then
			If Glowing(b).state = 0 Then Glowing(b).state = 1 end if
			Glowing(b).BulbHaloHeight = BOT(b).z + 25
			Glowing(b).x = BOT(b).x : Glowing(b).y = BOT(b).y + anglecompensate
			Glowing(b).falloff=GlowAura 'GlowBlob Auroa radius
			Glowing(b).intensity=GlowIntensity 'Glowblob intensity
		End If
	Next
End Sub




Sub Glowball_Init
	ChangeBall(ChooseBall)
	If GlowBall Then GraphicsTimer.enabled = True End If
End Sub
 
' Shake Cats
Dim CatLPos, CatRPos

Sub ShakeLeftCat
    CatLPos = 8
    CatLTimer.Enabled = 1
End Sub

Sub CatLTimer_Timer
    CatL.TransY = CatLPos
    If CatLPos = 0 Then Me.Enabled = 0:Exit Sub
    If CatLPos < 0 Then
        CatLPos = ABS(CatLPos)- 1
    Else
        CatLPos = - CatLPos + 1
    End If
End Sub

Sub ShakeRightCat
    CatRPos = 8
    CatRTimer.Enabled = 1
End Sub

Sub CatRTimer_Timer
    CatR.TransY = CatRPos
    If CatRPos = 0 Then Me.Enabled = 0:Exit Sub
    If CatRPos < 0 Then
        CatRPos = ABS(CatRPos)- 1
    Else
        CatRPos = - CatRPos + 1
    End If
End Sub


'*******************************************
' Setup Backglass
'*******************************************

Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen, ix, xx, yy, xobj

Sub setup_backglass()

	xoff = -20
	yoff = 77
	zoff = 699
	xrot = -90
	zscale = 0.0000001

	xcen = 0  '(130 /2) - (92 / 2)
	ycen = (780 /2 ) + (203 /2)

	for ix = 0 to 31
		For Each xobj In VRDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next
end sub


Dim VRDigits(32)
VRDigits(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6)
VRDigits(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6)
VRDigits(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6)
VRDigits(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6)
VRDigits(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6)
VRDigits(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6)
VRDigits(6) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6)

VRDigits(7) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6)
VRDigits(8) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6)
VRDigits(9) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6)
VRDigits(10) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6)
VRDigits(11) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6)
VRDigits(12) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6)
VRDigits(13) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6)

VRDigits(14) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006)
VRDigits(15) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106)
VRDigits(16) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206)
VRDigits(17) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306)
VRDigits(18) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406)
VRDigits(19) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506)
VRDigits(20) = Array(LED1x600,LED1x601,LED1x602,LED1x603,LED1x604,LED1x605,LED1x606)

VRDigits(21) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006)
VRDigits(22) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106)
VRDigits(23) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206)
VRDigits(24) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306)
VRDigits(25) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406)
VRDigits(26) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506)
VRDigits(27) = Array(LED2x600,LED2x601,LED2x602,LED2x603,LED2x604,LED2x605,LED2x606)

VRDigits(28) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306)
VRDigits(29) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406)

VRDigits(30) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506)
VRDigits(31) = Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606)


dim DisplayColor
DisplayColor =  RGB(255,40,1)

Sub VRDisplayTimer
	Dim ii, jj, obj, b, x
	Dim ChgLED,num, chg, stat
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				For Each obj In VRDigits(num)
 '                  If chg And 1 Then obj.visible=stat And 1    'if you use the object color for off; turn the display object visible to not visible on the playfield, and uncomment this line out.
					If chg And 1 Then FadeDisplay obj, stat And 1	
					chg=chg\2 : stat=stat\2
				Next
			Next
		End If

End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 20
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 8
	End If
End Sub

Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(VRDigits)
		if IsArray(VRDigits(x) ) then
			For each obj in VRDigits(x)
				obj.height = obj.height + 0
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

If VRMode = True Then
	InitDigits
End If

'*******************************************
' LAMP CALLBACK for the backglass flasher lamps (not the solenoid conrolled ones)
'*******************************************



if VRMode = True Then
	Set LampCallback = GetRef("UpdateVRLamps")
End If


Sub UpdateVRLamps()
	If Controller.Lamp(1) = 0 Then: VR_BG_Fl_GO.visible=0: 		else: VR_BG_Fl_GO.visible=1 		'Game Over
	If Controller.Lamp(3) = 0 Then: VR_BG_Fl_Tilt.visible=0: 		else: VR_BG_Fl_Tilt.visible=1 	'Tilt
	If Controller.Lamp(2) = 0 Then: VR_BG_Fl_Match.visible=0: 		else: VR_BG_Fl_Match.visible=1 	'Match
	If Controller.Lamp(5) = 0 Then: VR_BG_Fl_SA.visible=0: 		else: VR_BG_Fl_SA.visible=1 		'Shoot again
	If Controller.Lamp(4) = 0 Then: VR_BG_Fl_HS.visible=0: 		else: VR_BG_Fl_HS.visible=1 		'Highest Score
	If Controller.Lamp(40) = 0 Then: VR_BG_Fl_BIP.visible=0: 		else: VR_BG_Fl_BIP.visible=1 	'Ball In Play
End Sub
