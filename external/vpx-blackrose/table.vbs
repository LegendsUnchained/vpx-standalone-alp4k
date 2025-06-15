'************************************************************************
'					Black Rose VPX
'************************************************************************

'Special Thanks to:
'gtxjoe for a working script and playfield resource
'JayFoxRox for plastic scans
'Xagesz for additional photos
'HauntFreaks for his majic touch and lighting

'This is my first table that was not a conversion, so please take it for what it is.
'I'm not an artist or very good with blender but I hope you enjoy until someone with a little more artistic ability decides to build it.



Option Explicit
Randomize

Const UseVPMModSol = 1
Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD:UseVPMDMD = DesktopMode

Dim Ballsize,BallMass
BallSize = 50
BallMass = (Ballsize^3)/125000

'////////////////////////////////

'OPTIONS GRAPHICS / GAMEPLAY

Const flasher_brightness = 15 '< This is a divisor so a higher number = Duller Flashers.

Const Cannon_Walls = 0 'On the real machine, the ball sometimes runs along the edges of the cannon toy in
                       'the middle of the playfield resulting in unpredictable behaviour. Set this to 1 to
                       'attempt to simulate that effect.  

Const Flipper_Color = 1 '1 Black and Red Flippers (Authentic) - 2 White and Red Flippers. 

Const Plastic_Ramp_Sounds = 1 'Play plastic rolling sounds when ball hits plastic ramps.
'////////////////////////////////

'//Don't Touch :)
Const Dozer_Cab = 0
'////////////////

Const cGameName = "br_l4" ' Black Rose ROM L4  

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

LoadVPM "01120100", "wpc.VBS", 3.26


'************************************************
'************************************************
'************************************************
'************************************************
'************************************************
Const UseSolenoids = 2
Const UseLamps = False
Const UseSync = False
Const UseGI		= 1
Set GiCallback2 = GetRef("GIUpdate")

' Standard Sounds
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = "fx_coin"

If DesktopMode = 1 Then
CabinetRailRight.visible = 1
CabinetRailLeft.visible = 1
Else
CabinetRailRight.visible = 1
CabinetRailLeft.visible = 1
End If

Dim bsTrough

Sub Table1_Init
	vpminit Me
     With Controller
         .GameName = cGameName
         If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
         .SplashInfoLine = "Black Rose (Bally 1992)"
         .HandleKeyboard = 0
         .ShowTitle = 0
         .ShowDMDOnly = 1
         .ShowFrame = 0
         .HandleMechanics = False
         .Hidden = 0
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
	
	'GI_Init

	Controller.Switch(63) = 0
	Controller.Switch(64) = 0


   ' Trough
    Set bsTrough = New cvpmTrough
    With bsTrough
        .size = 3
        .initSwitches Array(16, 17, 18)
        .Initexit BallRelease, 55, 8
        .InitEntrySounds "Solenoid", SoundFX("fx_Solenoid",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .InitExitSounds SoundFX("fx_Solenoid",DOFContactors), SoundFX("fx_ballrel",DOFContactors)
        .Balls = 3
        .EntrySw = 15
    End With

	' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

	' Nudging
	vpmNudge.TiltSwitch = 14
	vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(bumpersw46, bumpersw47, bumpersw48, LeftSlingShot, RightSlingShot)
End Sub

Sub Table1_KeyDown(ByVal keycode)
	If vpmKeyDown(keycode) Then Exit Sub
	If keycode = PlungerKey Then Controller.Switch(34)=1:Plunger.PullBack: PlaySound "fx_plungerpull",0,1,0.25,0.25
	If keycode = LeftFlipperKey Then Controller.Switch(12)=1
	If keycode = RightFlipperKey Then Controller.Switch(11)=1
	If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge",0)
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge",0)
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge",0)
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then Controller.Switch(34)=0: Plunger.Fire: PlaySound "fx_plunger",0,1,0.25,0.25
	If keycode = LeftFlipperKey Then Controller.Switch(12)=0
	If keycode = RightFlipperKey Then Controller.Switch(11)=0
	If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub ShooterLane_Hit
	Controller.Switch(25) = 1
End Sub

Sub ShooterLane_UnHit
	Controller.Switch(25) = 0
End Sub


Sub Drain_Hit()
    bsTrough.AddBall Me
	PlaySoundAtVol "fx2_drain2",drain, 0.1
End Sub

If Flipper_color = 2 Then
LeftFlipper.material = "Plastic White"
RightFlipper.material = "Plastic White"
RightFlipper2.material = "Plastic White"
LFA.state = 0:RFA.state = 0
Else
LeftFlipper.material = "Rubber Black"
RightFlipper.material = "Rubber Black"
RightFlipper2.material = "Rubber Black"
LFA.state = 1:RFA.state = 1
End If

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw(38)
    PlaySoundat SoundFX("FX_Slingshot",DOFContactors),sling2
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    RightSlingShot.TimerInterval = 10
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw(28)
    PlaySoundat SoundFX("FX_Slingshot",DOFContactors),sling1
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval  = 10
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

'************************************************
' Solenoids
'************************************************
SolCallback(1) = 	"RampSwordKicker"
SolCallback(2) =	"bsTrough.SolIn"
SolCallback(3) = 	"CannonMotor"
SolCallback(4) =	"bsTrough.SolOut"
SolCallback(5) = 	""
SolCallback(6) = 	""
SolCallback(7) = 	"vpmSolSound SoundFX(""fx2_Knocker"",DOFKnocker),"
SolCallback(8) = 	"FireCannon"
SolCallback(9) = 	"PiratesCoveKick"
SolCallback(10) =	"RampUp"
SolCallback(11) =	"RampDown"
SolCallback(12) = 	""
SolCallback(13) = 	""
SolCallback(14) = 	""
SolCallback(15) = 	""
SolCallback(16) = 	""
SolModCallback(17) = 	"Flashers_17"
SolModCallback(18) = 	"Flashers_18"
SolModCallback(19) = 	"Flashers_19"
SolModCallback(20) = 	"Flashers_20"
SolModCallback(21) = 	"Flashers_21"
SolModCallback(22) = 	"Flashers_22"
SolModCallback(23) = 	"Flashers_23"
SolModCallback(25) = 	"Flashers_25"
SolModCallback(24) = 	"Flashers_24"
SolModCallback(28) = 	"Flashers_28"
SolModCallback(27) = 	"Flashers_Fire"
SolModCallback(26) = 	"CannonFlashers"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"


' Flashers

Dim xxx

Sub Flashers_23(enabled)
F23.Intensity = Enabled / flasher_brightness
F23l.Intensity = Enabled / flasher_brightness
End Sub

Sub Flashers_24(enabled)
F24.Intensity = Enabled / flasher_brightness
F24l.Intensity = Enabled / flasher_brightness
End Sub

Sub Flashers_28(enabled)
F28.Intensity = Enabled / flasher_brightness
F28l.Intensity = Enabled / flasher_brightness
End Sub

Sub Flashers_Fire(enabled)
For each xxx in Fire_Flashers:xxx.Intensity = Enabled / 10:next
Light74.Intensity = Enabled / 100
End Sub

Sub Flashers_19(enabled)
For each xxx in F19_Flashers:xxx.Intensity = Enabled / flasher_brightness:next
Light73.Intensity = Enabled / 200
End Sub

Sub Flashers_17(enabled)
For each xxx in F17_Flashers:xxx.Intensity = Enabled / flasher_brightness:next
End Sub

Sub Flashers_18(enabled)
For each xxx in F18_Flashers:xxx.Intensity = Enabled / flasher_brightness:next
End Sub

Sub Flashers_20(enabled)
For each xxx in F20_Flashers:xxx.Intensity = Enabled / flasher_brightness:next
End Sub

Sub Flashers_22(enabled)
For each xxx in F22_Flashers:xxx.Intensity = Enabled / flasher_brightness:next
Light72.Intensity = Enabled / 200
End Sub

Sub Flashers_21(enabled)
For each xxx in F21_Flashers:xxx.Intensity = Enabled / flasher_brightness:next
For each xxx in F21A_Flashers:xxx.Intensity = Enabled / 30:next
End Sub

Sub Flashers_25(enabled)
For each xxx in F25_Flashers:xxx.Intensity = Enabled / flasher_brightness:next
'For each xxx in F21A_Flashers:xxx.Intensity = Enabled / 30:next
End Sub

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundat SoundFX("fx_flipperup",DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
    Else
        PlaySoundat SoundFX("fx_flipperdown",DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundat SoundFX("fx_flipperup",DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipper2.RotateToEnd
   Else
        PlaySoundat SoundFX("fx_flipperdown",DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipper2.RotateToStart
    End If
End Sub

Sub SolKnocker(Enabled)
	If Enabled Then PlaySound SoundFX("Knocker",DOFKnocker)
End Sub

Sub RampDown(Enabled)
    If Enabled Then
debug.print "rampdown"
        Controller.Switch(54) = 1
        DaveyRampDown.Collidable = 1
        DaveyRampUp.Collidable = 0 
		DaveyRampTimer.Enabled = 1 'DaveyRamp.HeightBottom = 0
        playsoundat "fx_diverter", sw61_sub
    End If
End sub

Sub RampUp(Enabled)
    If Enabled Then
debug.print "rampup"
        Controller.Switch(54) = 0
        DaveyRampDown.Collidable = 0
        DaveyRampUp.Collidable = 1
		DaveyRampTimer.Enabled = 1 'DaveyRamp.HeightBottom = 80
        playsoundat "fx_diverter", sw61_sub
    End If
End Sub

Const RampInc = 5
DaveyRampTimer.Interval = 20
Sub DaveyRampTimer_Timer
dim temp

	If Controller.Switch(54) = True Then 'Down
		Temp = DaveyRamp.HeightBottom - RampInc
		If Temp  <= 0 then 
			DaveyRamp.HeightBottom = 0
			DaveyRampTimer.Enabled = 0
		Else
			DaveyRamp.HeightBottom = Temp
		End If
	Else	'Up
		Temp = DaveyRamp.HeightBottom  + RampInc
		If Temp >= 80 then 
			DaveyRamp.HeightBottom = 80
			DaveyRampTimer.Enabled = 0
		Else
			DaveyRamp.HeightBottom = Temp
		End If
	End If
debug.print daveyramp.heightbottom
End Sub


Sub CannonFlashers (Enabled)
		'Flasher26a.Intensity = enabled 
		'Flasher26b.Intensity = enabled 
		'Flasher26a2.Intensity = enabled 
		'Flasher26b2.Intensity = enabled 
        'Light75.Intensity = enabled / 25
End Sub

		
Sub RampSwordKicker (Enabled)
	If (Enabled and Controller.Switch(55) = True) Then
		KickerSw55.destroyball
		vpmCreateBall KickerSW55Upper
        PlaySoundat "fx_solenoid", Kickersw55
		PlaySound "fx_metalrolling"
		KickerSW55Upper.kick 180,5
		Controller.Switch(55)=0
	End If
End Sub

'Cannon	
Sub FireCannon(Enabled)
	if Enabled and Controller.Switch(35) = True then
'debug.print discangle
        PlaySoundat "gunshot", DiscCannonOnly
		DiscCannonOnly.visible=1
		vpmCreateBall CannonKicker
		cannonKicker.kick -1*discangle, 52
		Controller.Switch(35) = 0
	else
		DiscCannonOnly.visible=0		
	end if
end sub

Sub CannonMotor(Enabled)
	If Enabled Then
		Playsoundat "Fx_Motor",DiscCannonOnly
		DiscTimer.Enabled = 1
	Else
		DiscTimer.Enabled = 0
	End If
End Sub

If cannon_walls = 1 Then
C1.isdropped = 0
C2.isdropped = 0
C3.isdropped = 0
C4.isdropped = 0
Else
C1.isdropped = 1
C2.isdropped = 1
C3.isdropped = 1
C4.isdropped = 1
End If

Dim DiscAngle, Dir
Dir = 1
Sub DiscTimer_Timer
	DiscAngle = DiscAngle + Dir
	Disc.Rotz = DiscAngle
	DiscCannonOnly.Rotz = DiscAngle
'debug.print disc.rotz & ":" & Timer
   If DiscAngle > -2 AND DiscAngle < 2 AND cannon_walls = 1 Then
C1.isdropped = 0
C2.isdropped = 0
C3.isdropped = 0
C4.isdropped = 0
Else
C1.isdropped = 1
C2.isdropped = 1
C3.isdropped = 1
C4.isdropped = 1
   End If

	If DiscAngle => 45 then Dir = -1
	If DiscAngle =< -45 then Dir = 1
End Sub
	


'************************************************
' Switches
'************************************************

'Rollovers (Add to collection for sound)
Sub Sw26_Hit:	Controller.Switch(26)=1: End Sub
Sub Sw26_UnHit:	Controller.Switch(26)=0: End Sub
Sub Sw27_Hit:	Controller.Switch(27)=1: End Sub
Sub Sw27_UnHit:	Controller.Switch(27)=0: End Sub
Sub Sw36_Hit:	Controller.Switch(36)=1: End Sub
Sub Sw36_UnHit:	Controller.Switch(36)=0: End Sub
Sub Sw37_Hit:	Controller.Switch(37)=1: End Sub
Sub Sw37_UnHit:	Controller.Switch(37)=0: End Sub
Sub Sw45_Hit:	Controller.Switch(45)=1: End Sub
Sub Sw45_UnHit:	Controller.Switch(45)=0: End Sub
Sub Sw56_Hit:	Controller.Switch(56)=1: StopSound "subway" : PlaySound "fx_metalrolling": End Sub
Sub Sw56_UnHit:	Controller.Switch(56)=0: End Sub
Sub Sw57_Hit:	Controller.Switch(57)=1: End Sub
Sub Sw57_UnHit:	Controller.Switch(57)=0: End Sub
Sub Sw58_Hit:	Controller.Switch(58)=1: End Sub
Sub Sw58_UnHit:	Controller.Switch(58)=0: End Sub
Sub Sw62_Hit:	Controller.Switch(62)=1: End Sub
Sub Sw62_UnHit:	Controller.Switch(62)=0: End Sub

'Bumpers (Add to collection for sound)
Sub BumperSw46_Hit:	vpmTimer.PulseSw(46): Dir=-1: BumperSw46.TimerEnabled=1: BumperSw46.TimerInterval=10: End Sub
Sub BumperSw46_Timer
	BumperCap46.TransY = BumperCap46.TransY + Dir*5
	If BumperCap46.TransY < -20 Then Dir=1
	If BumperCap46.TransY > 0 Then BumperCap46.TransY=0: BumperSw46.TimerEnabled=0
End Sub
Sub BumperSw47_Hit:	vpmTimer.PulseSw(47): Dir=-1: BumperSw47.TimerEnabled=1: BumperSw47.TimerInterval=10: End Sub
Sub BumperSw47_Timer
	BumperCap47.TransY = BumperCap47.TransY + Dir*5
	If BumperCap47.TransY < -20 Then Dir=1
	If BumperCap47.TransY > 0 Then BumperCap47.TransY=0: BumperSw47.TimerEnabled=0
End Sub
Sub BumperSw48_Hit:	vpmTimer.PulseSw(48): Dir=-1: BumperSw48.TimerEnabled=1: BumperSw48.TimerInterval=10: End Sub
Sub BumperSw48_Timer
	BumperCap48.TransY = BumperCap48.TransY + Dir*5
	If BumperCap48.TransY < -20 Then Dir=1
	If BumperCap48.TransY > 0 Then BumperCap48.TransY=0: BumperSw48.TimerEnabled=0
End Sub

'Targets (Add to collection for sound)
Sub Sw31_Hit:	vpmTimer.PulseSw(31): End Sub
Sub Sw32_Hit:	vpmTimer.PulseSw(32): End Sub
Sub Sw33_Hit:	vpmTimer.PulseSw(33): End Sub
Sub Sw41_Hit:	vpmTimer.PulseSw(41): End Sub
Sub Sw42_Hit:	vpmTimer.PulseSw(42): End Sub
Sub Sw43_Hit:	vpmTimer.PulseSw(43): End Sub
Sub Sw51_Hit:	vpmTimer.PulseSw(51): End Sub
Sub Sw52_Hit:	vpmTimer.PulseSw(52): End Sub
Sub Sw53_Hit:	vpmTimer.PulseSw(53): End Sub
Sub Sw65_Hit:	vpmTimer.PulseSw(65): End Sub

'Gates (Add to collection for sound)
Sub GateSw44_Hit:	vpmTimer.PulseSw(44): End Sub
Sub GateSw71_Hit:	vpmTimer.PulseSw(71): End Sub
Sub GateSw72_Hit:	vpmTimer.PulseSw(72): End Sub
Sub GateSw76_Hit:	vpmTimer.PulseSw(76): End Sub

Sub WP_Hit()
If Plastic_Ramp_Sounds = 1 Then
If ActiveBall.VelY < -30 Then
PlaySoundat "subway_short" ,Peg20
Else
PlaySoundatvol "subway_short", peg20, 0.01
End If
End If
End Sub

Sub LR_Hit()
If Plastic_Ramp_Sounds = 1 Then
If ActiveBall.VelY < -30 Then
PlaySoundat "subway_short" ,BulbFil13
Else
PlaySoundatvol "subway_short", BulbFil13, 0.01
End If
End If
End Sub

Sub BR_Hit()
If Plastic_Ramp_Sounds = 1 Then
PlaySoundatvol "subway", KickerSW55, 0.03
End If
End Sub

Sub DRD_Hit()
PlaySoundatvol "fx_metalclank", sw61_sub, 0.02
End Sub

Sub br_drop_hit()
StopSound "Subway"
PlaySoundat "fx_ballrampdrop",BulbFil26
End Sub

Sub dr_drop_hit()
PlaySoundat "fx_ballrampdrop",Kickersw63
End Sub

Sub Kicker1_Hit: 
	StopSound "fx_metalrolling"
	Playsoundat "fx_balldrop", WireGuide1
	me.destroyball
	vpmCreateBall Kicker1
	Kicker1.Kick 180, 1, 4.71
End Sub
'Pirates Cove kickers
Sub KickerSw63_Hit: Controller.Switch(63) = 1: KickerSw64.Enabled=1: End Sub
Sub KickerSw64_Hit: Controller.Switch(64) = 1: KickerSw64.Enabled=0: End Sub

Sub PiratesCoveKick (Enabled)
	Enabled = abs(Enabled)
	If (Enabled) Then
        ukick = 1
        Upper_Kicker.enabled = 1
		If (Controller.Switch(64) = True) Then 
			KickerSw64.Enabled = 0
			KickerSW64.kick 345, 15: PlaySoundAt SoundFX("FX_Kicker",DOFContactors),Kickersw64
			Controller.Switch(64) = 0
		End If
		If (Controller.Switch (63) = True) Then
			KickerSw64.Enabled = 0
			KickerSW63.kick 345, 15: PlaySoundAt SoundFX("FX_Kicker",DOFContactors),Kickersw64
			Controller.Switch(63) = 0
		End If
	End If
End Sub

Dim ukick

Sub Upper_Kicker_Timer()
Select Case ukick
Case 1:
If Hammer.RotZ => 45 Then
ukick = 2
End If
Hammer.RotZ = Hammer.RotZ + 1
Case 2:
If Hammer.RotZ <= 0 Then
Hammer.RotZ = 0
me.enabled = 0
End If
Hammer.RotZ = Hammer.RotZ - 1
End Select
End Sub

Sub sw61_Sub_Hit()
PlaySoundat "headquarterhit", sw61_sub
PlaySound "subway"
vpmTimer.PulseSwitch 61, 0, ""
End Sub

Sub sw66_Sub_Hit()
vpmTimer.PulseSwitch 66, 0, ""
Controller.Switch(35) = 1
StopSound "subway"
me.destroyball
PlaySoundat "fx_kicker_enter", sw66_sub
End Sub

'Ramp Sword
Sub KickerSW55_Hit
	Controller.Switch(55) = 1
	playsoundat "fx_kicker_enter", Primitive7
End Sub

'GI =========================================

Sub GIUpdate(nr, level)
	Dim ii

	Select Case nr
        Case 0	'Bumpers (Blue)
			'nr0 = level
			For Each ii in Bump_GI
				ii.intensity = level / 3
			Next
        Light3.Intensity = Level * 1.5
        Light56.Intensity = Level * 1.5
If Dozer_Cab = 1 Then
If level = 1 OR level = 0 then Controller.B2SSetData 111,0:Controller.B2SSetData 112,0:Controller.B2SSetData 113,0:Controller.B2SSetData 114,0:Controller.B2SSetData 115,0:Controller.B2SSetData 116,0
If level = 2 then Controller.B2SSetData 111,1:Controller.B2SSetData 112,0:Controller.B2SSetData 113,0:Controller.B2SSetData 114,0:Controller.B2SSetData 115,0:Controller.B2SSetData 116,0
If level = 3 then Controller.B2SSetData 112,1:Controller.B2SSetData 111,0:Controller.B2SSetData 113,0:Controller.B2SSetData 114,0:Controller.B2SSetData 115,0:Controller.B2SSetData 116,0
If level = 4 then Controller.B2SSetData 113,1:Controller.B2SSetData 112,0:Controller.B2SSetData 111,0:Controller.B2SSetData 114,0:Controller.B2SSetData 115,0:Controller.B2SSetData 116,0
If level = 5 then Controller.B2SSetData 114,1:Controller.B2SSetData 112,0:Controller.B2SSetData 113,0:Controller.B2SSetData 111,0:Controller.B2SSetData 115,0:Controller.B2SSetData 116,0
If level = 6 then Controller.B2SSetData 115,1:Controller.B2SSetData 112,0:Controller.B2SSetData 113,0:Controller.B2SSetData 114,0:Controller.B2SSetData 111,0:Controller.B2SSetData 116,0
If level = 8 then Controller.B2SSetData 116,1:Controller.B2SSetData 112,0:Controller.B2SSetData 113,0:Controller.B2SSetData 114,0:Controller.B2SSetData 115,0:Controller.B2SSetData 111,0
End If		
		Case 1	'Upper 
			if level > 4 Then
			DOF 201, DOFOn
			Else
			DOF 201, DOFOff
			End If
			For Each ii in Top_GI
				ii.intensity = level / 1
			Next
		    Light37.Intensity = Level / 5

If Level>=7 Then Table1.ColorGradeImage = "ColorGradeEX_7":Else Table1.ColorGradeImage = "ColorGradeEX_" & (level+1):End If

If Level < 2 Then 
Light61.State = 1
Light70.State = 1
Light71.State = 1
Else
Light61.State = 0
Light70.State = 0
Light71.State = 0
End If

If Dozer_Cab = 1 Then
If level = 1 OR level = 0 then Controller.B2SSetData 121,0:Controller.B2SSetData 122,0:Controller.B2SSetData 123,0:Controller.B2SSetData 124,0:Controller.B2SSetData 125,0:Controller.B2SSetData 126,0
If level = 2 then Controller.B2SSetData 121,1:Controller.B2SSetData 122,0:Controller.B2SSetData 123,0:Controller.B2SSetData 124,0:Controller.B2SSetData 125,0:Controller.B2SSetData 126,0
If level = 3 then Controller.B2SSetData 122,1:Controller.B2SSetData 121,0:Controller.B2SSetData 123,0:Controller.B2SSetData 124,0:Controller.B2SSetData 125,0:Controller.B2SSetData 126,0
If level = 4 then Controller.B2SSetData 123,1:Controller.B2SSetData 122,0:Controller.B2SSetData 121,0:Controller.B2SSetData 124,0:Controller.B2SSetData 125,0:Controller.B2SSetData 126,0
If level = 5 then Controller.B2SSetData 124,1:Controller.B2SSetData 122,0:Controller.B2SSetData 123,0:Controller.B2SSetData 121,0:Controller.B2SSetData 125,0:Controller.B2SSetData 126,0
If level = 6 then Controller.B2SSetData 125,1:Controller.B2SSetData 122,0:Controller.B2SSetData 123,0:Controller.B2SSetData 124,0:Controller.B2SSetData 121,0:Controller.B2SSetData 126,0
If level = 8 then Controller.B2SSetData 126,1:Controller.B2SSetData 122,0:Controller.B2SSetData 123,0:Controller.B2SSetData 124,0:Controller.B2SSetData 125,0:Controller.B2SSetData 121,0
End If	
		Case 2	'Bottom (White ?)
			For Each ii in Bottom_GI
				ii.intensity = level / 1
			Next
        Flasher26a1.Intensity = Level * 15
        Flasher26b1.Intensity = Level * 15
LFA.Intensity = Level / 3
RFA.Intensity = Level / 3

If Dozer_Cab = 1 Then
If level = 1 OR level = 0 then Controller.B2SSetData 131,0:Controller.B2SSetData 132,0:Controller.B2SSetData 133,0:Controller.B2SSetData 134,0:Controller.B2SSetData 135,0:Controller.B2SSetData 136,0
If level = 2 then Controller.B2SSetData 131,1:Controller.B2SSetData 132,0:Controller.B2SSetData 133,0:Controller.B2SSetData 134,0:Controller.B2SSetData 135,0:Controller.B2SSetData 136,0
If level = 3 then Controller.B2SSetData 132,1:Controller.B2SSetData 131,0:Controller.B2SSetData 133,0:Controller.B2SSetData 134,0:Controller.B2SSetData 135,0:Controller.B2SSetData 136,0
If level = 4 then Controller.B2SSetData 133,1:Controller.B2SSetData 132,0:Controller.B2SSetData 131,0:Controller.B2SSetData 134,0:Controller.B2SSetData 135,0:Controller.B2SSetData 136,0
If level = 5 then Controller.B2SSetData 134,1:Controller.B2SSetData 132,0:Controller.B2SSetData 133,0:Controller.B2SSetData 131,0:Controller.B2SSetData 135,0:Controller.B2SSetData 136,0
If level = 6 then Controller.B2SSetData 135,1:Controller.B2SSetData 132,0:Controller.B2SSetData 133,0:Controller.B2SSetData 134,0:Controller.B2SSetData 131,0:Controller.B2SSetData 136,0
If level = 8 then Controller.B2SSetData 136,1:Controller.B2SSetData 132,0:Controller.B2SSetData 133,0:Controller.B2SSetData 134,0:Controller.B2SSetData 135,0:Controller.B2SSetData 131,0
End If		
End Select
End Sub



' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************
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
    Vol = Csng(BallVel(ball) ^2 / 500)
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

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

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

Sub PlaySoundAtVol(soundname, tableobj, vol)
    PlaySound soundname, 1, (vol), AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

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

Sub RollingSoundUpdate()
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
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
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
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "fx2_flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "fx2_flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "fx2_flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub aTargets_Hit(idx):PlaySoundatBall "fx_target":End Sub
Sub aBumpers_Hit (idx): PlaySoundatball SoundFX("fx_bumper", DOFContactors) : End Sub 
Sub aRollovers_Hit(idx):PlaySoundatball "fx_sensor":End Sub
Sub aGates_Hit(idx):PlaySoundatball "fx_Gate":End Sub
Sub aMetals_Hit(idx):PlaySoundatball "fx_MetalHit2":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundatball "fx2_hit_rubber":End Sub
Sub aRubbers_Hit(idx):PlaySoundatball "fx2_hit_rubber":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundatball "fx_postrubber":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundatball "fx_postrubber":End Sub
Sub aYellowPins_Hit(idx):PlaySoundatball "fx_postrubber":End Sub
Sub aPlastics_Hit(idx):PlaySoundatball "fx_PlasticHit":End Sub
Sub aWoods_Hit(idx):PlaySoundatball "fx_Woodhit":End Sub

' Ramp Soundss
Sub RHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop", 0, 1, pan(ActiveBall)
End Sub

Sub RHelp2_UnHit()
    StopSound "fx_metalrolling"
    'PlaySound "fx_balldrop", 0, 1, pan(ActiveBall)
    PlaySoundAt "fx_balldrop",sw66_sub
End Sub

'*************************************************************
' Lamp routines
' SetLamp xx, 0 is Off. SetLamp xx,1 is On
'*************************************************************
Dim LampState(200), FadingLevel(200), FadingState(200)
Dim FlashState(200), FlashLevel(200)
Dim FlashSpeedUp, FlashSpeedDown
Dim x


AllLampsOff()
LampTimer.Interval = 30
LampTimer.Enabled = 1

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
			FlashState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
        Next
    End If
    UpdateLamps

End Sub
 
Sub UpdateLamps
'	nFadeL 1, l1
'	nFadeL 2, l2
'	nFadeL 3, l3
'	nFadeL 4, l4
'	nFadeL 5, l5
'	nFadeL 6, l6
'	nFadeL 7, l7
'	nFadeL 8, l8
'	nFadeL 9, l9
'	nFadeL 10, l10
	NFadeLm 11, l11
    NfadeL 11, l11a
	nFade 12, l12, h12
	nFade 13, l13, h13
	nFade 14, l14, h14
	nFade 15, l15, h15
	nFade 16, l16, h16
	nFade 17, l17, h17
	nFade 18, l18, h18
'	nFadeL 19, l19
'	nFadeL 20, l20
	nFade 21, l21, h21
	nFade 22, l22, h22
	nFade 23, l23, h23
	nFade 24, l24, h24
	nFade 25, l25, h25
	nFade 26, l26, h26
	nFade 27, l27, h27
	nFade 28, l28, h28
'	nFade 29, l29
'	nFade 30, l30
	nFade 31, l31, h31
	nFade 32, l32, h32
	nFade 33, l33, h33
	nFade 34, l34, h34
	nFade 35, l35, h35
	nFade 36, l36, h36
	nFade 37, l37, h37
	nFade 38, l38, h38
'	nFade 39, l39
'	nFade 40, l40
	nFade 41, l41, h41
	nFade 42, l42, h42
	nFade 43, l43, h43
	nFade 44, l44, h44
	nFade 45, l45, h45
	nFade 46, l46, h46
	nFade 47, l47, h47
	nFade 48, l48, h48
'	nFade 49, l49
'	nFade 50, l50
	nFade 51, l51, h51
	nFade 52, l52, h52
	nFade 53, l53, h53
	nFade 54, l54, h54
	nFade 55, l55, h55
	nFade 56, l56, h56
	nFade 57, l57, h57
	nFade 58, l58, h58
'	nFade 59, l59
'	nFade 60, l60
	nFade 61, l61, h61		
	nFade 62, l62, h62
	nFade 63, l63, h63
	nFade 64, l64, h64
	nFade 65, l65, h65
	nFade 66, l66, h66
	nFadeL 67, l67
	nFade 68, l68, h68
	nFade 71, l71, h71
	nFade 72, l72, h72
	nFade 73, l73, h73
	nFade 74, l74, h74
	nFade 75, l75, h75
	nFade 76, l76, h76
	nFade 77, l77, h77
	nFade 81, l81, h81
	nFade 82, l82, h82
	nFade 83, l83, h83
	nFade 84, l84, h84
	nFade 85, l85, h85
	nFade2 86, l86, h86, l86b, h86b
End Sub

Sub AllLampsOff():For x = 1 to 200:LampState(x) = 4:Next:UpdateLamps:UpdateLamps:Updatelamps:End Sub
 
Sub SetLamp(nr, value)
	LampState(nr) = abs(value) + 4
	FadingLevel(nr) = abs(value) + 4
	FlashState(nr) = abs(value)
End Sub

Sub NFade(nr, a, b)
	Select Case LampState(nr)
		 Case 4:a.state = 0:b.state = 0:LampState(nr) = 0
		 Case 5:a.State = 1:b.state = 1:LampState(nr) = 1
	End Select
End Sub

Sub NFade2(nr, a, b, c, d)
	Select Case LampState(nr)
		 Case 4:a.state = 0:b.state = 0:c.state = 0:d.state = 0:LampState(nr) = 0
		 Case 5:a.State = 1:b.state = 1:c.state = 1:d.state = 1:LampState(nr) = 1
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


'*****************************************
'	Ball Shadow
'*****************************************

Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5)


Sub BallShadowUpdate()
    Dim BOT, b
    BOT = GetBalls

	' render the shadow for each ball
    For b = 0 to UBound(BOT)
		If BOT(b).X < Table1.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 10
		Else
			BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 10
		End If

			BallShadow(b).Y = BOT(b).Y + 10
			BallShadow(b).Z = 1
		If BOT(b).Z > 20 AND shad_off = 0 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub

Dim shad_off

Sub Shadow_Off_Hit()
shad_off = 1
End Sub

Sub Shadow_Off_Unhit()
shad_off = 0
End Sub

' ===============================================================================================
' realtime updates
' ===============================================================================================

'Set MotorCallback = GetRef("RealTimeUpdates")

	Sub Update_Stuff_Timer()
		BallShadowUpdate
        RollingSoundUpdate
        FlipperLSh.RotZ = LeftFlipper.currentangle
		FlipperRSh.RotZ = RightFlipper.currentangle
		FlipperRTop.RotZ = RightFlipper2.currentangle
	End Sub

'Flipper EOS
Dim EOSAngle,EOSTorque
EOSAngle = 3
EOSTorque = .9
'FlipperTorque_Init

Sub FlipperTorque_Init
	LeftFlipper.UserValue = LeftFlipper.EOSTorque
	RightFlipper.UserValue = RightFlipper.EOSTorque
End Sub

FlipperTorqueTimer.Interval = 10
FlipperTorqueTimer.Enabled = 0
Sub FlipperTorqueTimer_Timer
	FlipperTorque LeftFlipper
	FlipperTorque RightFlipper
End Sub

Sub FlipperTorque (FlipperName)
	If FlipperName.StartAngle > FlipperName.EndAngle Then
		If FlipperName.CurrentAngle < FlipperName.EndAngle + EOSAngle Then
			FlipperName.EOSTorque = EOSTorque
		Else
			FlipperName.EOSTorque = FlipperName.UserValue
		End If
	Else ' If flipper start angle is < flipper end angle
		If FlipperName.CurrentAngle > FlipperName.EndAngle - EOSAngle Then
			FlipperName.EOSTorque = EOSTorque
		Else
			FlipperName.EOSTorque = FlipperName.UserValue
		End If
	End If

End Sub

Sub Table1_Exit
Controller.Stop
End Sub