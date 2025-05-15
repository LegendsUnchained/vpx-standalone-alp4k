'*******************************************************************************************************
'
'					   	    	 	 Baywatch Sega 1995 VPX v1.0.5
'								http://www.ipdb.org/machine.cgi?id=2848
'
'											Created by Kiwi
'
'*******************************************************************************************************

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'********************************************** OPTIONS ************************************************

'****************************************** Volume Settings **********************************

Const RolVol = 1	'Ball Rolling
Const MroVol = 1	'Wire Ramps Rolling
Const ProVol = 1	'Plastic Ramps Rolling
Const ColVol = 1	'Ball Collision
Const RubVol = 1	'Rubbers Collision
Const MhiVol = 1	'Metals Hit
Const DroVol = 1	'Ball drop (4)
Const NudVol = 1	'Nudge
Const TuiVol = 0.5	'TroughLock on
Const TuoVol = 0.5	'TroughLock off
Const TufVol = 0.3	'TroughUp fire
Const ApfVol = 1	'AutoPlungers fire (3)
Const SliVol = 1	'Slingshots (2)
Const BumVol = 1	'Bumpers (3)
Const SwiVol = 1	'Rollovers (17)
Const TarVol = 1	'Targets (12)
Const DtaVol = 1	'Droptargets (3)
Const DtrVol = 1	'Droptarget reset
Const GaoVol = 0.4	'Control Gates & Trapdoor open (3)
Const GacVol = 0.4	'Control Gates & Trapdoor close (3)
Const GatVol = 1	'Gates (2)
Const SpiVol = 1	'Spinner
Const VukVol = 1	'VUK catch
Const VufVol = 1	'VUK & Shark Super Scoop & Lower Super Vuk fire (3)
Const SssVol = 1	'Shark Super Scoop catch
Const KidVol = 1	'Kicker Drain
Const FluVol = 0.6	'Flippers up
Const FldVol = 0.6	'Flippers down
Const KnoVol = 1	'Knocker
Const GirVol = 0.5	'General Illumination Relay click

'************************ ROM

Const cGameName = "baywatch"

'************************ Ball: 50 unit is standard ball size ** Mass=(50^3)/125000 ,(BallSize^3)/125000

Const BallSize = 50

Const BallMass = 1.07

'************************ Ball momentum limiter: 0 off, 1 on ** Max ball momentum.

Const MomOn = 0

Const Maxmom = 400

'************************ Ball Shadow: 0 hidden , 1 visible

Const BallSHW = 1

'************************ CabRails and rail lights Hidden/Visible in FS mode: 0 hidden , 1 visible

Const RailsLights = 0

'************************ Flashers Intensity

Const Lumens = 10

'************************ Color Grading LUT: 1 = Active, any other value = disabled

Const LutEnabled = 1

'************************ DMD: 0 for VPinMAME DMD visible in DT mode, 1 for Text DMD visible in DT mode

Const VPMorTextDMD = 1

'************************ 0 Backglass on in FSS, 1 Backglass on and Backdrop off in DT, 2 Backglass on in FS

Const BackG = 0

'************************ Slingshot mode: 0 Walls, 1 Flippers

Const SlingM = 0

'************************ Slingshot hit threshold, with flippers (parm)

Const ThSling = 3

'************************ Holes shadow playfield

Dim y
For each y in aOmbre
	y.IntensityScale = y.IntensityScale * 0.95
	y.Height = -24
Next

'******************************************** OPTIONS END **********************************************

Dim ModePlay, VarHidden, UseVPMColoredDMD
ModePlay = Table1.ShowDT + Table1.ShowFSS - BackG

'******************************************** DMD

 If ModePlay < 0 Then
    UseVPMColoredDMD = VPMorTextDMD
    VarHidden = VPMorTextDMD
Else
    UseVPMColoredDMD = 0
    VarHidden = 0		'Put 1 if you whant DMD Hidden in FS mode
    TextBoxDMD.Visible = 0
End If

' If B2SOn = True Then VarHidden = 1

'*******************************************************************************************************

LoadVPM "01560000", "de2.VBS", 3.26

'Set Controller = CreateObject("b2s.server")

Dim bsTrough, bsTroughUp, bsUpVUK, bsSSScoop, bsLowSVuk, cdtBank, PinPlay

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
'Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SFlipperOn = ""
Const SFlipperOff = ""
Const SCoin = "coin3"

'************
' Table init.
'************

Sub Table1_Init
	vpmInit me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Baywatch (Sega 1995)" & vbNewLine & "VPX table by Kiwi 1.0.5"
		.HandleMechanics = 0
		.HandleKeyboard = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.ShowTitle = 0
		.Hidden = VarHidden
'		.DoubleSize = 1
'		.Games(cGameName).Settings.Value("dmd_pos_x")=0
'		.Games(cGameName).Settings.Value("dmd_pos_y")=0
'		.Games(cGameName).Settings.Value("dmd_width")=768
'		.Games(cGameName).Settings.Value("dmd_height")=256
'		.Games(cGameName).Settings.Value("rol") = 0
'		.Games(cGameName).Settings.Value("ddraw") = 0
'		.Games(cGameName).Settings.Value("sound") = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With

	' Nudging
	vpmNudge.TiltSwitch = swTilt
	vpmNudge.Sensitivity = 2
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingShot, RightSlingShot)

	' Trough
	Set bsTrough = New cvpmBallStack
	With bsTrough
		.InitSw 0, 14, 13, 12, 11, 10, 0, 0
'		.InitKick BallRelease, 80, 27
'		.InitEntrySnd "Solenoid", "Solenoid"
'		.InitExitSnd SoundFX("solenoid",DOFContactors), SoundFX("popper",DOFContactors)
		.Balls = 5
	End With

	Set bsTroughUp = New cvpmBallStack
	With bsTroughUp
		.initSaucer BallRelease, 15, 0, 36
		.KickZ = 1.56
'		.InitExitSnd SoundFX("popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
	End With

	' Upper VUK
	Set bsUpVUK = New cvpmBallStack
	With bsUpVUK
		.InitSaucer sw44, 44, 0, 36
		.KickZ = 1.56
'		.InitExitSnd SoundFX("popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
	End With

	' Shark Super Scoop
    Set bsSSScoop = New cvpmBallStack
    With bsSSScoop
		.InitSaucer sw56, 56, 0, 45
		.KickForceVar = 4
'		.KickAngleVar = 1
		.KickZ = 1.56
'		.InitExitSnd SoundFX("popper",DOFContactors), SoundFX("vuk_enter",DOFContactors)
    End With

    ' Lower Super Vuk (Subway)
    Set bsLowSVuk = New cvpmBallStack
    With bsLowSVuk
		.InitSaucer sw46, 46, 0, 30
		.KickZ = 1.56
'		.InitExitSnd SoundFX("popper",DOFContactors), SoundFX("vuk_enter",DOFContactors)
    End With

	' Drop targets
	set cdtBank = new cvpmdroptarget
	With cdtBank
		.initdrop array(sw22, sw23, sw24), array(22, 23, 24)
'		.initsnd SoundFX("DROPTARG",DOFContactors), SoundFX("DTResetB",DOFContactors)
	End With

	' Main Timer init
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	Plunger1.Pullback
	LaserKick.Pullback
	UpBLaunch.Pullback
	RampLG1.Collidable=1
	RampLG2.Collidable=0
	RampLG3.Collidable=0
	Botola.Open=0
	RampLG6.Collidable=0

	GIBulb13gt22.Visible=0
	GIBulb13gt23.Visible=0
	GIBulb13gt24.Visible=0

	SlingFSx1.Enabled = SlingM
	SlingFSx2.Enabled = SlingM
	SlingFDx1.Enabled = SlingM
	SlingFDx2.Enabled = SlingM

	RightSlingshot.IsDropped = SlingM
	LeftSlingshot.IsDropped = SlingM

'************ Rails and rail lights

 If ModePlay = 0 And RailsLights = 0 Then
	CabRailSx.Visible = 0
	CabRailDx.Visible = 0
	f194b.Visible = 0
	f194c.Visible = 0
	f193a.Visible = 0
	f196b.Visible = 0
	f198a.Visible = 0
	f194.ImageA = "fro2"
	f194.Imageb = "fro2"
	f194.Height = 182
	f194a.ImageA = "fro2"
	f194a.Imageb = "fro2"
	f194a.Height = 182
	f193.ImageA = "fro2"
	f193.Imageb = "fro2"
	f193.Height = 168
	f196a.ImageA = "fro2"
	f196a.Imageb = "fro2"
	f196a.Height = 178
	f198.Height = 200
End If

'************ FSS Init

 If ModePlay = -2 Then
	TextBoxDMD.Visible = False
End If

 If ModePlay > -2 Then
	FlasherDMD.Visible = False
f1r.Visible=0:f1ra.Visible=0:f2r.Visible=0:f2ra.Visible=0:f3r.Visible=0:f3ra.Visible=0:f4r.Visible=0
f5r.Visible=0:f5ra.Visible=0:f6r.Visible=0:f6ra.Visible=0:f7r.Visible=0:f8r.Visible=0:f8ra.Visible=0
fbb1.Visible=0:fbb2.Visible=0:fbb3.Visible=0:fbb4.Visible=0:fbb5.Visible=0:fbb6.Visible=0:fbb7.Visible=0:fbb8.Visible=0:fbb9.Visible=0:fbb10.Visible=0
fbb11.Visible=0:fbb12.Visible=0:fbb13.Visible=0:fbb14.Visible=0:fbb15.Visible=0:fbb16.Visible=0:fbb17.Visible=0:fbb18.Visible=0:fbb19.Visible=0:fbb20.Visible=0
fbb21.Visible=0:fbb22.Visible=0:fbb23.Visible=0:fbb24.Visible=0:fbb25.Visible=0:fbb26.Visible=0:fbb27.Visible=0:fbb28.Visible=0:fbb29.Visible=0:fbb30.Visible=0
fbb31.Visible=0:fbb32.Visible=0:fbb33.Visible=0
End If

f1r.Y=151:f1ra.Y=151:f2r.Y=151:f2ra.Y=151:f3r.Y=151:f3ra.Y=151:f4r.Y=151:f5r.Y=151:f5ra.Y=151:f6r.Y=151:f6ra.Y=151:f7r.Y=151:f8r.Y=151:f8ra.Y=151
fbb1.Y=151:fbb2.Y=151:fbb3.Y=151:fbb4.Y=151:fbb5.Y=151:fbb6.Y=151:fbb7.Y=151:fbb8.Y=151:fbb9.Y=151:fbb10.Y=151
fbb11.Y=151:fbb12.Y=151:fbb13.Y=151:fbb14.Y=151:fbb15.Y=151:fbb16.Y=151:fbb17.Y=151:fbb18.Y=151:fbb19.Y=151:fbb20.Y=151
fbb21.Y=151:fbb22.Y=151:fbb23.Y=151:fbb24.Y=151:fbb25.Y=151:fbb26.Y=151:fbb27.Y=151:fbb28.Y=151:fbb29.Y=151:fbb30.Y=151
fbb31.Y=151:fbb32.Y=151:fbb33.Y=151

	TextLUT.Visible = 0
	TextLUT.Text = Table1.ColorGradeImage
	LoadLut
	TextBoxDMD.Width=Int(TextBoxDMD.Width * (4/WindowWidth)*(WindowHeight/3))
'	TextBoxDMD.Width=300	' 192 x 64 = 3 x 1    300 x 100
'	TextBoxDMD.Width=250	' DMD 16-10 X * 0,83  250 x 100
'	TextBoxDMD.Width=225	' DMD 16-9  X * 0,75  225 x 100

End Sub

Sub Table1_Exit():Controller.Stop:End Sub
Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub

'**********
' Keys
'**********

Sub Table1_KeyDown(ByVal keycode)

	If KeyCode = LeftFlipperKey Then Controller.Switch(63) = 1
	If KeyCode = RightFlipperKey Then Controller.Switch(64) = 1
	If keycode = PlungerKey Then Controller.Switch(50) = 1
	If keycode = LockBarKey Then Controller.Switch(50) = 1
	If KeyCode = KeyFront Then Controller.Switch(8) = 1
	If keycode = LeftMagnaSave Then bLutActive = True:TextLUT.Visible = 1
	If keycode = RightMagnaSave Then
	If bLutActive And LutEnabled = 1 Then NextLUT: End If
	End If
'	If KeyCode = LeftFlipperKey And PinPlay=1 Then LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd:Controller.Switch(63) = 1
'	If KeyCode = RightFlipperKey And PinPlay=1 Then RightFlipper.RotateToEnd:RightFlipper1.RotateToEnd:Controller.Switch(64) = 1
	If keycode = LeftTiltKey Then Nudge 90, 4:PlaySound SoundFX("fx_nudge",0), 0, NudVol, -0.1, 0.25
	If keycode = RightTiltKey Then Nudge 270, 4:PlaySound SoundFX("fx_nudge",0), 0, NudVol, 0.1, 0.25
	If keycode = CenterTiltKey Then Nudge 0, 5:PlaySound SoundFX("fx_nudge",0), 0, NudVol, 0, 0.25
	If vpmKeyDown(keycode) Then Exit Sub

    'debug key (M)
'	If keycode = "50" Then
'        setlamp 191, 1
'        setlamp 192, 1
'        setlamp 193, 1
'        setlamp 194, 1
'        setlamp 195, 1
'        setlamp 196, 1
'        setlamp 197, 1
'        setlamp 198, 1
'  Stop1.IsDropped=ABS(Stop1.IsDropped+1)
'End If

End Sub

Sub Table1_KeyUp(ByVal keycode)

	If KeyCode = LeftFlipperKey Then Controller.Switch(63) = 0
	If KeyCode = RightFlipperKey Then Controller.Switch(64) = 0
	If keycode = PlungerKey Then Controller.Switch(50) = 0
	If keycode = LockBarKey Then Controller.Switch(50) = 0
	If KeyCode = KeyFront Then Controller.Switch(8) = 0
    If keycode = LeftMagnaSave Then bLutActive = False:TextLUT.Visible = 0
'	If KeyCode = LeftFlipperKey And PinPlay=1 Then LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart:Controller.Switch(63) = 0
'	If KeyCode = RightFlipperKey And PinPlay=1 Then RightFlipper.RotateToStart:RightFlipper1.RotateToStart:Controller.Switch(64) = 0
	If vpmKeyUp(keycode) Then Exit Sub

    'debug key (M)
'	If keycode = "50" Then
'        setlamp 191, 0
'        setlamp 192, 0
'        setlamp 193, 0
'        setlamp 194, 0
'        setlamp 195, 0
'        setlamp 196, 0
'        setlamp 197, 0
'        setlamp 198, 0
'End If

End Sub

Plunger1.TimerEnabled = 1
Plunger1.TimerInterval = 1

Sub Plunger1_Timer
    
    'Debug.Print Plunger1.position
    
    if Plunger1.position > 5 Then Controller.Switch(50) = 1
    if Plunger1.position < 5 Then Controller.Switch(50) = 0
    
End Sub


'*********
'   LUT
'*********

Dim bLutActive, LUTImage, x

Sub LoadLUT
	bLutActive = False
	x = LoadValue(cGameName, "LUTImage")
	If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
	SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage +1) MOD 10:UpdateLUT:SaveLUT:End Sub

Sub UpdateLUT
Select Case LutImage
	Case 0:Table1.ColorGradeImage = "LUT0"
	Case 1:Table1.ColorGradeImage = "LUT1"
	Case 2:Table1.ColorGradeImage = "LUT2"
	Case 3:Table1.ColorGradeImage = "LUT3"
	Case 4:Table1.ColorGradeImage = "LUT4"
	Case 5:Table1.ColorGradeImage = "LUT5"
	Case 6:Table1.ColorGradeImage = "LUT6"
	Case 7:Table1.ColorGradeImage = "LUT7"
	Case 8:Table1.ColorGradeImage = "LUT8"
	Case 9:Table1.ColorGradeImage = "LUT9"
End Select
TextLUT.Text = Table1.ColorGradeImage
End Sub

'*********
' Switches
'*********

' Slings

Dim LStep, RStep

Sub LeftSlingshot_Slingshot:vpmTimer.PulseSw 61:PlaySoundAtVol SoundFX("Slingshot",DOFContactors), SxEmKickerT1, SliVol:LeftSling.Visible=1:SxEmKickerT1.TransX=-25:LStep=0:Me.TimerEnabled=1:End Sub
Sub RightSlingshot_Slingshot:vpmTimer.PulseSw 62:PlaySoundAtVol SoundFX("Slingshot",DOFContactors), DxEmKickerT1, SliVol:RightSling.Visible=1:DxEmKickerT1.TransX=-25:RStep=0:Me.TimerEnabled=1:End Sub

Sub SlingFSx1_Collide(parm)
 If SxEmKickerT1.TransX=0 And PinPlay=1 And parm > ThSling Then
	vpmTimer.PulseSw 61:PlaySoundAtVol SoundFX("Slingshot",DOFContactors), SxEmKickerT1, SliVol:LStep=0
	LeftSling.Visible=1:SxEmKickerT1.TransX=-25:LeftSlingshot.TimerEnabled=1:SlingFSx1.RotateToEnd:SlingFSx2.RotateToEnd
End If
End Sub
Sub SlingFSx2_Collide(parm)
 If SxEmKickerT1.TransX=0 And PinPlay=1 And parm > ThSling Then
	vpmTimer.PulseSw 61:PlaySoundAtVol SoundFX("Slingshot",DOFContactors), SxEmKickerT1, SliVol:LStep=0
	LeftSling.Visible=1:SxEmKickerT1.TransX=-25:LeftSlingshot.TimerEnabled=1:SlingFSx1.RotateToEnd:SlingFSx2.RotateToEnd
End If
End Sub

Sub LeftSlingshot_Timer
	Select Case LStep
		Case 0:LeftSling.Visible = 1
		Case 1: 'pause
		Case 2:LeftSling.Visible = 0 :LeftSling1.Visible = 1:SxEmKickerT1.TransX=-21
		Case 3:LeftSling1.Visible = 0:LeftSling2.Visible = 1:SxEmKickerT1.TransX=-17:SlingFSx1.RotateToStart:SlingFSx2.RotateToStart
		Case 4:LeftSling2.Visible = 0:Me.TimerEnabled = 0:SxEmKickerT1.TransX=0
	End Select
	LStep = LStep + 1
End Sub

Sub SlingFDx1_Collide(parm)
 If DxEmKickerT1.TransX=0 And PinPlay=1 And parm > ThSling Then
	vpmTimer.PulseSw 62:PlaySoundAtVol SoundFX("Slingshot",DOFContactors), DxEmKickerT1, SliVol:RStep=0
	RightSling.Visible=1:DxEmKickerT1.TransX=-25:RightSlingshot.TimerEnabled=1:SlingFDx1.RotateToEnd:SlingFDx2.RotateToEnd
End If
End Sub
Sub SlingFDx2_Collide(parm)
 If DxEmKickerT1.TransX=0 And PinPlay=1 And parm > ThSling Then
	vpmTimer.PulseSw 62:PlaySoundAtVol SoundFX("Slingshot",DOFContactors), DxEmKickerT1, SliVol:RStep=0
	RightSling.Visible=1:DxEmKickerT1.TransX=-25:RightSlingshot.TimerEnabled=1:SlingFDx1.RotateToEnd:SlingFDx2.RotateToEnd
End If
End Sub

Sub RightSlingshot_Timer
	Select Case RStep
		Case 0:RightSling.Visible = 1
		Case 1: 'pause
		Case 2:RightSling.Visible = 0 :RightSling1.Visible = 1:DxEmKickerT1.TransX=-21
		Case 3:RightSling1.Visible = 0:RightSling2.Visible = 1:DxEmKickerT1.TransX=-17:SlingFDx1.RotateToStart:SlingFDx2.RotateToStart
		Case 4:RightSling2.Visible = 0:Me.TimerEnabled = 0:DxEmKickerT1.TransX=0
	End Select
	RStep = RStep + 1
End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 41:PlaySoundAtVol SoundFX("jet1",DOFContactors), Bumper1, BumVol:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 42:PlaySoundAtVol SoundFX("jet1",DOFContactors), Bumper2, BumVol:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 43:PlaySoundAtVol SoundFX("jet1",DOFContactors), Bumper3, BumVol:End Sub

' Eject holes
Sub Drain_Hit:bsTrough.AddBall Me:PlaysoundAtVol "drain1a", Drain, KidVol:End Sub
Sub sw44_Hit:PlaysoundAtVol "fx_kicker_enter1", sw44, VuKVol:bsUpVUK.AddBall 0:End Sub
Sub sw56_Hit:PlaysoundAtVol "vuk_enter", sw56, SssVol:bsSSScoop.AddBall 0:End Sub
Sub sw46_Hit:PlaysoundAtVol "fx_kicker_enter1", sw46, SssVol:bsLowSVuk.AddBall 0:End Sub

' Spinner
Sub sw52_Spin:vpmTimer.PulseSw 52:PlaySoundAtVol "spinner", sw52, SpiVol:End Sub

' Rollovers
Sub sw16_Hit:  Controller.Switch(16) = 1:PlaySoundAtVol "sensor", sw16, SwiVol:End Sub
Sub sw16_UnHit:vpmtimer.addtimer 250, "Controller.Switch(16) = 0 '":End Sub
Sub sw26_Hit:  Controller.Switch(26) = 1:PlaySoundAtVol "sensor", sw26, SwiVol:End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub
'Sub sw28_Hit:  Controller.Switch(28) = 1:PlaySoundAtVol "sensor", sw28, SwiVol:End Sub
'Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub
Sub sw30_Hit:  Controller.Switch(30) = 1:PlaySoundAtVol "sensor", sw30, SwiVol:End Sub
Sub sw30_UnHit:Controller.Switch(30) = 0:End Sub
Sub sw33_Hit:  Controller.Switch(33) = 1:PlaySoundAtVol "sensor", sw33, SwiVol:End Sub
Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub

Sub sw38_Hit:  Controller.Switch(38) = 1:PlaySoundAtVol "sensor", sw38, SwiVol:End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub
Sub sw39_Hit:  Controller.Switch(39) = 1:PlaySoundAtVol "sensor", sw39, SwiVol:End Sub
Sub sw39_UnHit:Controller.Switch(39) = 0:End Sub
Sub sw40_Hit:  Controller.Switch(40) = 1:PlaySoundAtVol "sensor", sw40, SwiVol:End Sub
Sub sw40_UnHit:Controller.Switch(40) = 0:End Sub
Sub sw45_Hit:  Controller.Switch(45) = 1:PlaySoundAtVol "sensor", sw45, SwiVol:End Sub
Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub

Sub sw47_Hit:  Controller.Switch(47) = 1:PlaySoundAtVol "sensor", sw47, SwiVol:End Sub
Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub
Sub sw49_Hit:  Controller.Switch(49) = 1:PlaySoundAtVol "sensor", sw49, SwiVol:End Sub
Sub sw49_UnHit:Controller.Switch(49) = 0:End Sub

Sub sw54_Hit:  Controller.Switch(54) = 1:PlaySoundAtVol "sensor", sw54, SwiVol:End Sub
Sub sw54_UnHit:Controller.Switch(54) = 0:End Sub
Sub sw55_Hit:  Controller.Switch(55) = 1:PlaySoundAtVol "sensor", sw55, SwiVol:End Sub
Sub sw55_UnHit:Controller.Switch(55) = 0:End Sub
Sub sw57_Hit:  Controller.Switch(57) = 1:PlaySoundAtVol "sensor", sw57, SwiVol:End Sub
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
Sub sw58_Hit:  Controller.Switch(58) = 1:PlaySoundAtVol "sensor", sw58, SwiVol:End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
Sub sw59_Hit:  Controller.Switch(59) = 1:PlaySoundAtVol "sensor", sw59, SwiVol:End Sub
Sub sw59_UnHit:Controller.Switch(59) = 0:End Sub
Sub sw60_Hit:  Controller.Switch(60) = 1:PlaySoundAtVol "sensor", sw60, SwiVol:End Sub
Sub sw60_UnHit:Controller.Switch(60) = 0:End Sub

' Targets
Sub sw9_Hit:vpmTimer.PulseSw 9:Psw9.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw9_Timer:Psw9.TransX=0:Me.TimerEnabled=0:End Sub
Sub sw53_Hit:vpmTimer.PulseSw 53:Psw53.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw53_Timer:Psw53.TransX=0:Me.TimerEnabled=0:End Sub

Sub sw17_Hit:vpmTimer.PulseSw 17:Psw17.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw17_Timer:Psw17.TransX=0:Me.TimerEnabled=0:End Sub
Sub sw18_Hit:vpmTimer.PulseSw 18:Psw18.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw18_Timer:Psw18.TransX=0:Me.TimerEnabled=0:End Sub
Sub sw19_Hit:vpmTimer.PulseSw 19:Psw19.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw19_Timer:Psw19.TransX=0:Me.TimerEnabled=0:End Sub
Sub sw20_Hit:vpmTimer.PulseSw 20:Psw20.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw20_Timer:Psw20.TransX=0:Me.TimerEnabled=0:End Sub
Sub sw21_Hit:vpmTimer.PulseSw 21:Psw21.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw21_Timer:Psw21.TransX=0:Me.TimerEnabled=0:End Sub
Sub sw31_Hit:vpmTimer.PulseSw 31:Psw31.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw31_Timer:Psw31.TransX=0:Me.TimerEnabled=0:End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:Psw32.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw32_Timer:Psw32.TransX=0:Me.TimerEnabled=0:End Sub
Sub sw36_Hit:vpmTimer.PulseSw 36:Psw36.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw36_Timer:Psw36.TransX=0:Me.TimerEnabled=0:End Sub
Sub sw37_Hit:vpmTimer.PulseSw 37:Psw37.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw37_Timer:Psw37.TransX=0:Me.TimerEnabled=0:End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:Psw48.TransX=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw48_Timer:Psw48.TransX=0:Me.TimerEnabled=0:End Sub

' Droptargets
Sub sw22_Hit():PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw22, DtaVol:End Sub
Sub sw22_Dropped:cdtbank.Hit 1:GIBulb13gt22.Visible=1:End Sub
Sub sw23_Hit():PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw23, DtaVol:End Sub
Sub sw23_Dropped:cdtbank.Hit 2:GIBulb13gt23.Visible=1:End Sub
Sub sw24_Hit():PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw24, DtaVol:End Sub
Sub sw24_Dropped:cdtbank.Hit 3:GIBulb13gt24.Visible=1:End Sub

' Gates
Sub LowerGate_Hit():PlaySoundAtBallVol "Gate51", GatVol:End Sub	'Necessari ?
Sub UpperGate_Hit():PlaySoundAtBallVol "Gate51", GatVol:End Sub	'Necessari ?
Sub Gate25_Hit():vpmTimer.PulseSw 25:PlaySoundAtBallVol "Gate51", GatVol:End Sub
Sub Gate28_Hit():vpmTimer.PulseSw 28:PlaySoundAtBallVol "Gate51", GatVol:End Sub

' Fx Sounds
Sub swfx1_Hit:PlaySoundAtVol "fx_InMetalrolling", swfx1, MhiVol*0.2:End Sub
Sub swfx2_Hit:PlaySoundAtBallVol "fx_InMetalrolling", MhiVol:End Sub
Sub swfx3_Hit:PlaySoundAtBallVol "fx_InMetalrolling", MhiVol:End Sub
Sub swfx4_Hit:PlaySoundAtBallVol "fx_InMetalrolling", MhiVol:End Sub

Sub TriggerOSxR_Hit:PlaySoundAtBallVol "fx_ballrampendhit", MhiVol*3:End Sub
Sub EndWY_Hit:PlaySoundAtBallVol "fx_metalhit", MhiVol*3:End Sub

Sub TriggerOLT_Hit:PlaySoundAtBallVol "fx_ballrampendhit", MhiVol*2:End Sub
Sub TriggerOVUK_Hit:PlaySoundAtBallVol "fx_ballrampendhit", MhiVol*2:End Sub

Sub THelper1_Hit:ActiveBall.VelZ=0:End Sub

'*********
'Solenoids
'*********

SolCallback(1) = "TroughLock"
SolCallback(2) = "bsTroughUpSolOut"
SolCallback(3) = "AutoBallLaunch"
SolCallback(4) = "UpBallLaunch"
SolCallback(5) = "bsUpVUKSolOut"
SolCallback(6) = "bsLowSVukSolOut"
SolCallback(7) = "bsSSScoopSolOut"
SolCallback(8) = "KnockerSound"
SolCallback(9) = "UpperG"
'SolCallback(10) = "LRABRelay"
SolCallback(11) = "GIRelay"
SolCallback(12) = "LockTrap"
SolCallback(13) = "AutoFlipper"
SolCallback(14) = "LowerG"
SolCallback(16) = "dtcbank"
SolCallback(22) = "LaserkickBack"
SolCallback(23) = "SolRun"

SolCallback(25) = "setlamp 191,"
SolCallback(26) = "setlamp 192,"
SolCallback(27) = "setlamp 193,"
SolCallback(28) = "setlamp 194,"
SolCallback(29) = "setlamp 195,"
SolCallback(30) = "setlamp 196,"
SolCallback(31) = "setlamp 197,"
SolCallback(32) = "setlamp 198,"

Sub TroughLock(Enabled)
 If Enabled Then
	bsTrough.ExitSol_On
	vpmCreateBall BallRelease 
	bsTroughUp.AddBall 0
	PlaySoundAtVol SoundFX("metalhit",DOFContactors), BallRelease, TuiVol
Else
	PlaySoundAtVol SoundFX("fx_solenoidoff",DOFContactors), BallRelease, TuoVol
End If
End Sub

Sub bsTroughUpSolOut(Enabled)
 If Enabled Then
	vpmtimer.addtimer 250, "bsTroughUp.ExitSol_On '"
	vpmtimer.addtimer 250, "TVUKSound '"
'	bsTroughUp.ExitSol_On
'	PlaySoundAtVol SoundFX("TVUKOut",DOFContactors), BallRelease, TufVol
End If
End Sub

Sub TVUKSound
	PlaySoundAtVol SoundFX("TVUKOut",DOFContactors), BallRelease, TufVol
End Sub

Sub AutoBallLaunch(Enabled)
 If Enabled Then
	Plunger1.Fire
	PlaySoundAtVol SoundFX("plunger",DOFContactors), Plunger1, ApfVol
	Else
	Plunger1.Pullback
End If
End Sub

Sub UpBallLaunch(Enabled)
 If Enabled Then
	UpBLaunch.Fire
	PlaySoundAtVol SoundFX("plunger",DOFContactors), UpBLaunch, ApfVol
	Else
	UpBLaunch.Pullback
End If
End Sub

Sub bsUpVUKSolOut(Enabled)
 If Enabled Then
	bsUpVUK.ExitSol_On
	VUKArm.Z = -20
	VUKArmTimer.Enabled = 1
	PlaySoundAtVol SoundFX("popper",DOFContactors), sw44, VufVol
End If
End Sub

Sub VUKArmTimer_Timer()
	VUKArm.Z = -40
	VUKArmTimer.Enabled = 0
End Sub

Sub bsLowSVukSolOut(Enabled)
 If Enabled Then
	bsLowSVuk.ExitSol_On
	PlaySoundAtVol SoundFX("popper",DOFContactors), sw46, VufVol
End If
End Sub

Sub bsSSScoopSolOut(Enabled)
 If Enabled Then
	bsSSScoop.ExitSol_On
	PlaySoundAtVol SoundFX("popper",DOFContactors), sw56, VufVol
End If
End Sub

Sub KnockerSound(Enabled)
 If Enabled Then
	PlaySoundAtVol SoundFX("Knocker",DOFKnocker), l27, KnoVol
End If
End Sub

Sub UpperG(Enabled)
 If Enabled Then
	UpperGate.Open=1
	PlaySoundAtVol SoundFX("fx_solenoidon",DOFContactors), UpperGate, GaoVol
Else
	UpperGate.Open=0
	PlaySoundAtVol SoundFX("fx_solenoidoff",DOFContactors), UpperGate, GacVol
End If
End Sub

Sub GIRelay(Enabled)
	Dim GIoffon
	GIoffon = ABS(ABS(Enabled) -1)
	SetLamp 200, GIoffon
	PlaySoundAtVol "sensor", ViteEsa13, GirVol
End Sub

Sub LockTrap(Enabled)
 If Enabled Then
	RampLG1.Collidable=0
	RampLG2.Collidable=1
	RampLG3.Collidable=1
	RampLG6.Collidable=1
	Botola.Open=1
	PlaySoundAtVol SoundFX("fx_solenoidon",DOFContactors), Trapdoor, GaoVol
Else
	RampLG1.Collidable=1
	RampLG2.Collidable=0
	RampLG3.Collidable=0
	RampLG6.Collidable=0
	Botola.Open=0
	PlaySoundAtVol SoundFX("fx_solenoidoff",DOFContactors), Trapdoor, GacVol
End If
End Sub

Sub AutoFlipper(Enabled)
 If Enabled Then
	LeftFlipper1.RotateToEnd:PlaySoundAtVolPitch SoundFX("flipperup1",DOFFlippers), LeftFlipper1, FluVol, 3500
	RightFlipper1.RotateToEnd:PlaySoundAtVolPitch SoundFX("flipperup1",DOFFlippers), RightFlipper1, FluVol*0.9, 5000
Else
	LeftFlipper1.RotateToStart:PlaySoundAtVolPitch SoundFX("flipperdown1",DOFFlippers), LeftFlipper1, FldVol, 3500
	RightFlipper1.RotateToStart:PlaySoundAtVolPitch SoundFX("flipperdown1",DOFFlippers), RightFlipper1, FldVol*0.9, 5000
End If
End Sub

Sub LowerG(Enabled)
 If Enabled Then
	LowerGate.Open=1
	PlaySoundAtVol SoundFX("fx_solenoidon",DOFContactors), LowerGate, GaoVol
Else
	LowerGate.Open=0
	PlaySoundAtVol SoundFX("fx_solenoidoff",DOFContactors), LowerGate, GacVol
End If
End Sub

Sub dtcbank(Enabled)
 If Enabled Then
	cdtbank.DropSol_On
	GIBulb13gt22.Visible=0
	GIBulb13gt23.Visible=0
	GIBulb13gt24.Visible=0
End If
	PlaySoundAtVol SoundFX("DTResetB",DOFContactors), sw23, DtrVol
End Sub

Sub LaserKickBack(Enabled)
 If Enabled Then
	LaserKick.Fire
	LaserKick.FireSpeed=130+(20*RND)
	PlaySoundAtVol SoundFX("plunger",DOFContactors), LaserKick, ApfVol
	Else
	LaserKick.Pullback
End If
End Sub

Sub SolRun(Enabled)
	vpmNudge.SolGameOn Enabled
 If Enabled Then
	PinPlay=1
'	LeftSlingShot.Disabled=0
'	RightSlingShot.Disabled=0
Else
	PinPlay=0
	SlingFSx1.RotateToStart:SlingFSx2.RotateToStart
	SlingFDx1.RotateToStart:SlingFDx2.RotateToStart
'	LeftFlipper.RotateToStart
'	RightFlipper.RotateToStart
'	LeftFlipper1.RotateToStart
'	RightFlipper1.RotateToStart
'	LeftSlingShot.Disabled=1
'	RightSlingShot.Disabled=1
End If
End Sub

'**************
' Flipper Subs
'**************

'SolCallback(sULFlipper)
'SolCallback(sURFlipper)
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
	If Enabled Then
		PlaySoundAtVol SoundFX("flipperup1",DOFFlippers), LeftFlipper, FluVol:LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
		PlaySoundAtVolPitch SoundFX("flipperup1",DOFFlippers), LeftFlipper1, FluVol, 3500
	Else
		If LeftFlipper.CurrentAngle < LeftFlipper.StartAngle - 5 Then
			PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), LeftFlipper, FldVol
			PlaySoundAtVolPitch SoundFX("flipperdown1",DOFFlippers), LeftFlipper1, FldVol, 3500
		End If	
        LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		PlaySoundAtVol SoundFX("flipperup1",DOFFlippers), RightFlipper, FluVol:RightFlipper.RotateToEnd:RightFlipper1.RotateToEnd
		PlaySoundAtVolPitch SoundFX("flipperup1",DOFFlippers), RightFlipper1, FluVol*0.9, 5000
	Else
		If RightFlipper.CurrentAngle > RightFlipper.StartAngle + 5 Then
			PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), RightFlipper, FldVol
			PlaySoundAtVolPitch SoundFX("flipperdown1",DOFFlippers), RightFlipper1, FldVol*0.9, 5000
		End If
		RightFlipper.RotateToStart:RightFlipper1.RotateToStart
	End If
End Sub

Sub LeftFlipper_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper1_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper1_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'******************
' RealTime Updates
'******************
Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
	UpdateFlipperLogo
End Sub

Sub UpdateFlipperLogo
	LogoSx.RotZ = LeftFlipper.CurrentAngle - 90
	LogoSx1.RotZ = LeftFlipper1.CurrentAngle - 90
	LogoDx.RotZ = RightFlipper.CurrentAngle + 90
	Pinna.RotZ = RightFlipper1.CurrentAngle
	Trapdoor.RotX = Botola.CurrentAngle
	LowerGatePrim.RotX = LowerGate.CurrentAngle / 2
	UpperGatePrim.RotX = UpperGate.CurrentAngle / 2
	UGateArm.RotX = UpperGate.CurrentAngle / 10 - 8
	WireGLG.RotX = Gate25.CurrentAngle
	WireGTS.RotX = Gate28.CurrentAngle
End Sub

'**********************************
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

Sub UpdateLamps
	NFadeL 1, l1
	NFadeL 2, l2
	NFadeL 3, l3
	NFadeL 4, l4
	NFadeL 5, l5
	NFadeL 9, l9
	NFadeL 10, l10
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
	NFadeLm 25, lL25
	NFadeL 25, lR25
	NFadeL 26, l26
	NFadeL 27, l27
	NFadeL 28, l28
	NFadeL 29, l29
	NFadeL 30, l30
	NFadeL 31, l31
	NFadeL 32, l32
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
	NFadeL 59, l59
	NFadeL 60, l60
	NFadeL 61, l61
	NFadeL 62, l62
	NFadeL 63, l63

	NFadeLm 200, lgi1
	NFadeLm 200, lgi2
	NFadeLm 200, GIBulb1
	NFadeLm 200, GIBulb1g
	NFadeLm 200, GIBulb2
	NFadeLm 200, GIBulb2g
	NFadeLm 200, GIBulb3
	NFadeLm 200, GIBulb3g
	NFadeLm 200, GIBulb4
	NFadeLm 200, GIBulb4g
	NFadeLm 200, GIBulb5
	NFadeLm 200, GIBulb5g
	NFadeLm 200, GIBulb6
	NFadeLm 200, GIBulb6g
	NFadeLm 200, GIBulb7
	NFadeLm 200, GIBulb7g
	NFadeLm 200, GIBulb8
	NFadeLm 200, GIBulb8g
	NFadeLm 200, GIBulb9
	NFadeLm 200, GIBulb9g
	NFadeLm 200, GIBulb10
	NFadeLm 200, GIBulb10g
	NFadeLm 200, GIBulb11
	NFadeLm 200, GIBulb11g
	NFadeLm 200, GIBulb12
	NFadeLm 200, GIBulb12g
	NFadeLm 200, GIBulb13
	NFadeLm 200, GIBulb13g
	NFadeLm 200, GIBulb13gt22
	NFadeLm 200, GIBulb13gt23
	NFadeLm 200, GIBulb13gt24
	NFadeLm 200, GIBulb14
	NFadeLm 200, GIBulb15
	NFadeLm 200, GIBulb15g
	NFadeLm 200, GIBulb16
	NFadeLm 200, GIBulb16g
	NFadeLm 200, GIBulb17
	NFadeLm 200, GIBulb17g
	NFadeLm 200, GIBulb18
	NFadeLm 200, GIBulb18g
	NFadeLm 200, GIBulb19
	NFadeLm 200, GIBulb19g
	NFadeLm 200, GIBulb20
	NFadeLm 200, GIBulb20g
	NFadeLm 200, GIBulb21
	NFadeLm 200, GIBulb21g
	NFadeLm 200, GIBulb22
	NFadeLm 200, GIBulb22g
	NFadeLm 200, GIBulb23
	NFadeLm 200, GIBulb23g
	NFadeLm 200, GIBulb24
	NFadeLm 200, GIBulb24g
	NFadeLm 200, GIBulb25
	NFadeLm 200, GIBulb25g
	NFadeLm 200, GIBulb26
	NFadeLm 200, GIBulb26g
	NFadeLm 200, GIBulb27
	NFadeLm 200, GIBulb27g
	NFadeLm 200, GIBulb28
	NFadeLm 200, GIBulb28g
	NFadeLm 200, GIBulb29
	NFadeLm 200, GIBulb29g
	NFadeLm 200, GIBulb30
	NFadeLm 200, GIBulb30g
	NFadeLm 200, GIBulb31
	NFadeLm 200, GIBulb31g
	NFadeLm 200, GIBulb32
	NFadeLm 200, GIBulb32a
	NFadeLm 200, GIBulb33
	NFadeLm 200, GIBulb33a
	NFadeLm 200, GIBumb2
	NFadeLm 200, GIBumb3

	Flashm 200, fgit1
	Flashm 200, fgit2
	Flashm 200, fgit3
	Flashm 200, fgit4
	Flashm 200, fgit5
	Flashm 200, fgit6
	Flashm 200, fgit8
	Flashm 200, fgit17
	Flashm 200, fgit26

	Flashm 200, fbb1
	Flashm 200, fbb2
	Flashm 200, fbb3
	Flashm 200, fbb4
	Flashm 200, fbb5
	Flashm 200, fbb6
	Flashm 200, fbb7
	Flashm 200, fbb8
	Flashm 200, fbb9
	Flashm 200, fbb10
	Flashm 200, fbb11
	Flashm 200, fbb12
	Flashm 200, fbb13
	Flashm 200, fbb14
	Flashm 200, fbb15
	Flashm 200, fbb16
	Flashm 200, fbb17
	Flashm 200, fbb18
	Flashm 200, fbb19
	Flashm 200, fbb20
	Flashm 200, fbb21
	Flashm 200, fbb22
	Flashm 200, fbb23
	Flashm 200, fbb24
	Flashm 200, fbb25
	Flashm 200, fbb26
	Flashm 200, fbb27
	Flashm 200, fbb28
	Flashm 200, fbb29
	Flashm 200, fbb30
	Flashm 200, fbb31
	Flashm 200, fbb32
	Flash 200, fbb33

	Flashm 191, f191
	Flashm 191, f191a
	Flashm 191, f1r
	Flash 191, f1ra

	NFadeLm 192, l192
	NFadeLm 192, l192a
	NFadeLm 192, l192b
	Flashm 192, f2r
	Flash 192, f2ra

	NFadeLm 193, l193
	NFadeLm 193, l193a
	Flashm 193, f193
	Flashm 193, f193a
	Flashm 193, f3r
	Flashm 193, f3ra
	FlashDL 193, 0.1, DomeBW193

	NFadeLm 194, l194
	Flashm 194, f194
	Flashm 194, f194a
	Flashm 194, f194b
	Flashm 194, f194c
	Flashm 194, f4r
	FlashDLm 194, 0.1, DomeBW194
	FlashDL 194, 0.25, DomeBW194a

	Flashm 195, f195a
	Flashm 195, f195b
	Flashm 195, f5r
	Flash 195, f5ra

	Flashm 196, f196
	Flashm 196, f196a
	Flashm 196, f196b
	Flashm 196, f6r
	Flashm 196, f6ra
	FlashDL 196, 0.1, DomeBW196

	NFadeLm 197, l197a
	NFadeLm 197, l197b
	NFadeLm 197, l197c
	Flash 197, f7r

	NFadeLm 198, l198
	Flashm 198, f198
	Flashm 198, f198a
	Flashm 198, f8r
	Flashm 198, f8ra
	FlashDL 198, 0.1, DomeBW198

	FlashDLm 6, 1, BulboF6
	Flash 6, f6
	FlashDLm 11, 1, BulboF11
	Flash 11, f11
	FlashDLm 12, 1, BulboF12
	Flash 12, f12
	FlashDLm 13, 1, BulboF13
	Flash 13, f13
	FlashDLm 33, 1, BulboF33
	Flash 33, f33
	FlashDLm 34, 1, BulboF34
	Flash 34, f34
	FlashDLm 35, 1, BulboF35
	Flash 35, f35

End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.8   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.2	' slower speed when turning off the flasher
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

' Lights: old method, using 4 images

Sub FadeL(nr, light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:light.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:light.image = a:light.State = 1:FadingLevel(nr) = 1   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1           'wait
        Case 9:light.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1        'wait
        Case 13:light.image = d:Light.State = 0:FadingLevel(nr) = 0  'Off
    End Select
End Sub

Sub FadeLm(nr, light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:light.image = b
        Case 5:light.image = a
        Case 9:light.image = c
        Case 13:light.image = d
    End Select
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

' Ramps & Primitives used as 4 step fading lights
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
            Object.IntensityScale = Lumens*FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = Lumens*FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = Lumens*FlashLevel(nr)
End Sub

' Objects DisableLighting

Sub FlashDL(nr, Limite, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.BlendDisableLighting = Limite*FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.BlendDisableLighting = Limite*FlashLevel(nr)
    End Select
End Sub

Sub FlashDLm(nr, Limite, object) 'multiple objects, it just sets the flashlevel
    Object.BlendDisableLighting = Limite*FlashLevel(nr)
End Sub

' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub FadeR(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.SetValue 0:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.SetValue 2:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1          'wait
        Case 13:object.SetValue 3:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeRm(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1
        Case 5:object.SetValue 0
        Case 9:object.SetValue 2
        Case 3:object.SetValue 3
    End Select
End Sub

'Texts

Sub NFadeT(nr, object, message)
    Select Case FadingLevel(nr)
        Case 4:object.Text = "":FadingLevel(nr) = 0
        Case 5:object.Text = message:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, object, b)
    Select Case FadingLevel(nr)
        Case 4:object.Text = ""
        Case 5:object.Text = message
    End Select
End Sub

' *********************************************************************
' 					Wall, rubber and metal hit sounds
' *********************************************************************

Sub Rubbers_Hit(idx):PlaySoundAtBallVol "rubber1", RubVol:End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
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
'PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, Vol) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, Vol, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVolPitch(soundname, tableobj, Vol, Pitch) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, Vol, Pan(tableobj), 0, Pitch, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVol(soundname, VolMult) ' play a sound at the ball position, like rubbers, targets
    PlaySound soundname, 0, Vol(ActiveBall) * VolMult, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*******************************************
'   JP's VP10 Rolling Sounds + Ballshadow
' uses a collection of shadows, aBallShadow
'*******************************************

Const tnob = 19 ' total number of balls
Const lob = 0  'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b, momfactorx, momfactory, momfactorz
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		aBallShadow(b).Visible = 0
		StopSound("fx_ballrolling" & b)
		StopSound("fx_Rolling_Plastic" & b)
		StopSound("fx_Rolling_Metal" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)

		aBallShadow(b).X = BOT(b).X
		aBallShadow(b).Y = BOT(b).Y
		aBallShadow(b).Height = BOT(b).Z - (BallSize / 2)+1

        If BallVel(BOT(b)) > 1 Then
            rolling(b) = True

'Playfield
			If BOT(b).z < 30 Then
					StopSound("fx_Rolling_Metal" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*RolVol, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else 
'Wire Ramps
'55-70 Uscita U 80 95
				If InRect(BOT(b).x, BOT(b).y, 16,266,62,266,135,1509,16,1509) And BOT(b).z < 100 And BOT(b).z > 79 Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
'90-96 Uscita Y 115 121
			ElseIf InRect(BOT(b).x, BOT(b).y, 704,159,1144,159,78,883,78,822) And BOT(b).z < 122 And BOT(b).z > 114 Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
'62-150 Lancio
			ElseIf InRect(BOT(b).x, BOT(b).y, 810,1350,952,1350,1160,1930,883,1930) And BOT(b).z > 85 Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 0,450,70,450,952,1350,810,1350) And BOT(b).z > 134 Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 5,125,70,125,70,450,5,450) And BOT(b).z > 149 Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 5,7,534,7,534,125,5,125) And BOT(b).z < 155 And BOT(b).z > 95 Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
'215-110 VUK 135
			ElseIf InRect(BOT(b).x, BOT(b).y, 360,226,470,226,945,955,600,955) And BOT(b).z > 135 Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
'171-110 Uscita Lifeguard
			ElseIf InRect(BOT(b).x, BOT(b).y, 865,307,936,307,936,728,892,728) And BOT(b).z > 135 Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 362,0,1100,307,865,307,342,64) And BOT(b).z > 170 Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )

'Plastic Ramps
			Else 
					StopSound("fx_Rolling_Metal" & b):StopSound("fx_ballrolling" & b)
					PlaySound("fx_Rolling_Plastic" & b), -1, Vol(BOT(b) )*3*ProVol, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) ) 
			End If
			End If

        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
				StopSound("fx_Rolling_Plastic" & b)
				StopSound("fx_Rolling_Metal" & b)
                rolling(b) = False
            End If
        End If

		' play ball drop sounds
		If BOT(b).VelZ < -8 and BOT(b).VelY < 11 And BOT(b).z < 50 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			PlaySound ("fx_ballhit" & b), 0, (ABS(BOT(b).velz)/17)*DroVol, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If

		' Ball Shadow
		If BallSHW = 1 Then
			aBallShadow(b).Visible = 1
		Else
			aBallShadow(b).Visible = 0
		End If

        ' ball momentum control
        If BOT(b).AngMomX AND BOT(b).AngMomY AND BOT(b).AngMomZ <> 0 And MomOn = 1 Then
            momfactorx = ABS(Maxmom / BOT(b).AngMomX)
            momfactory = ABS(Maxmom / BOT(b).AngMomY)
            momfactorz = ABS(Maxmom / BOT(b).AngMomZ)
            If momfactorx < 1 And MomOn = 1 Then
                BOT(b).AngMomX = BOT(b).AngMomX * momfactorx
                BOT(b).AngMomY = BOT(b).AngMomY * momfactorx
                BOT(b).AngMomZ = BOT(b).AngMomZ * momfactorx
            End If
            If momfactory < 1 And MomOn = 1 Then
                BOT(b).AngMomX = BOT(b).AngMomX * momfactory
                BOT(b).AngMomY = BOT(b).AngMomY * momfactory
                BOT(b).AngMomZ = BOT(b).AngMomZ * momfactory
            End If
            If momfactorz < 1 And MomOn = 1 Then
                BOT(b).AngMomX = BOT(b).AngMomX * momfactorz
                BOT(b).AngMomY = BOT(b).AngMomY * momfactorz
                BOT(b).AngMomZ = BOT(b).AngMomZ * momfactorz
            End If
        End If

    Next
End Sub

'0-180 Lifeguard

'0-65 Lifeguard U
'0-103 Y Sx
'0-115 Y Dx

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(ColVol*((velocity) ^2 / 200)), Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'16/9  FSS X Y Scale 1.82
'16/10 FSS X Y Scale 1.59