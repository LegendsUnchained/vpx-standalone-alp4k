Option Explicit

' Thalamus 2019 May : Improved directional sounds

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 2500    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 1    ' Bumpers volume.
Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
Const VolRH     = 1    ' Rubber hits volume.
Const VolPi     = 1    ' Rubber pins volume.
Const VolTarg   = 1    ' Targets volume.
Const VolSpin   = 1    ' Spinners volume.

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim VarRol,VarHidden, loopvar
If Table1.ShowDT = true then
	VarRol=0
	VarHidden=1
	For Each loopvar in dtdisplay
		loopvar.Visible = True
	Next
	DisplayTimer.Enabled=True
Else
	VarRol=1
	VarHidden=0
	For Each loopvar in dtdisplay
		loopvar.Visible = False
	Next
	DisplayTimer.Enabled=False
End If

If B2SOn = true Then
	VarHidden=1
End If

Const cGameName="ravena",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_Flipperup",SFlipperOff="fx_Flipperdown"
Const SCoin="coin3",cCredits=""

LoadVPM "01120100","sys80.vbs",3.02

'Sub LoadVPM(VPMver, VBSfile, VBSver)
'	On Error Resume Next
'		If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
'		ExecuteGlobal GetTextFile(VBSFile)
'		If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description : Err.Clear
'		Set Controller = CreateObject("VPinMAME.Controller")
'		If Err Then MsgBox "Can't Load VPinMAME." & vbNewLine & Err.Description
'		If VPMver>"" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required." : Err.Clear
'		If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
'	On Error Goto 0
'End Sub

Sub Table1_KeyDown(ByVal keycode)
	if keycode=leftflipperkey then controller.switch(6)=1
    if keycode=rightflipperkey then
    	Controller.Switch(16)=1
    	controller.switch(75)=1
    End If
	If vpmKeyDown(KeyCode) Then Exit Sub
	If keycode=PlungerKey Then Plunger.Pullback:playsoundAtVol "plungerpull", Plunger, 1
End Sub

Sub Table1_KeyUp(ByVal keycode)
	if keycode=leftflipperkey then controller.switch(6)=0
    if keycode=rightflipperkey then
    	Controller.Switch(16)=0
    	controller.switch(75)=0
    End If
	If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode=PlungerKey Then Plunger.Fire:playsoundAtVol "plunger", Plunger, 1
End Sub

PlaySound "Intro",-0 

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1)="TLB.SolDropUp" 'Drop Targets
SolCallback(2)="TRB.SolDropUp" 'Drop Targets
Solcallback(4) = "SetLamp 94,"
SolCallback(5)="BLB.SolDropUp" 'Drop Targets
SolCallback(6)="BRB.SolDropUp" 'Drop Targets
Solcallback(7) = "SetLamp 97,"

SolCallback(8)="vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(9)="bsTrough.SolOut"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("fx_Flipperup",DOFContactors), LeftFlipper, 1:LeftFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFContactors), LeftFlipper, 1:LeftFlipper.RotateToStart
     End If
  End Sub

Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("fx_Flipperup",DOFContactors), RightFlipper, 1:RightFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFContactors), RightFlipper, 1:RightFlipper.RotateToStart
     End If
End Sub

'Playfield GI
Sub PFGI(Enabled)
    If Enabled Then
        dim xx
        For each xx in GI:xx.State = 1: Next
        PlaySound "fx_relay"
        Timer1.Enabled = 0 'Aux Board 1
    Else
        For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"
        Timer1.Enabled = 1 'Aux Board 1
    End If
End Sub

Dim bsTrough,TLB,BLB,TRB,BRB

Sub Table1_Init
    vpmInit Me
Controller.Games(cGameName).Settings.Value("dmd_red")=7
Controller.Games(cGameName).Settings.Value("dmd_green")=194
Controller.Games(cGameName).Settings.Value("dmd_blue")=78
	On Error Resume Next
	With Controller 
		.GameName=cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine="Rambo First Blood Part II (TBA 2020)" & vbnewline & "Table by IVANTBA "
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.Hidden=0
		.Games(cGameName).Settings.Value("sound") = 0 '1= rotated display, 0= normal
		.ShowTitle=0
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
		Controller.SolMask(0)=0
      vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
		Controller.Run

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

	vpmNudge.TiltSwitch=57
	vpmNudge.Sensitivity=5
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,Bumper4,LeftSlingshot,RightSlingshot)
	
	Set bsTrough=New cvpmBallstack
	bsTrough.InitSw 0,67,0,0,0,0,0,0
	bsTrough.InitKick BallRelease,120,3
	bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors),SoundFX("solon",DOFContactors)
	bsTrough.Balls=1

	Set TLB=New cvpmDropTarget
	TLB.InitDrop Target11, 44
	TLB.InitSnd SoundFX("DTDrop",DOFDropTargets),SoundFX("DTReset",DOFContactors)

	Set BLB=New cvpmDropTarget
	BLB.InitDrop Target15, 64
	BLB.InitSnd SoundFX("DTDrop",DOFDropTargets),SoundFX("DTReset",DOFContactors)

	Set TRB=New cvpmDropTarget
	TRB.InitDrop Target13, 54
	TRB.InitSnd SoundFX("DTDrop",DOFDropTargets),SoundFX("DTReset",DOFContactors)

	Set BRB=New cvpmDropTarget
	BRB.InitDrop Target16, 74
	BRB.InitSnd SoundFX("DTDrop",DOFDropTargets),SoundFX("DTReset",DOFContactors)
End Sub

Sub Target18_Hit:VpmTimer.PulseSw 40:End Sub										'40
Sub Target22_Hit:VpmTimer.PulseSw 41:End Sub										'41
Sub Target24_Hit:VpmTimer.PulseSw 42:End Sub										'42
Sub Trigger1_Hit:Controller.Switch(43)=1:PlaySound"soundfx9":End Sub								'43
Sub Trigger1_unHit:Controller.switch(43)=0:PlaySound"soundfx10":End Sub
Sub TDrop4_dropped:TLB.Hit 1:End Sub												'44
Sub Bumper1_Hit:VpmTimer.PulseSw 46:PlaySoundAtVol SoundFX("bumper",DOFContactors),Bumper1, VolBump :DOF 101, DOFPulse:End Sub					'46
Sub Bumper2_Hit:VpmTimer.PulseSw 46:PlaySoundAtVol SoundFX("bumper2",DOFContactors),Bumper2, VolBump :DOF 103, DOFPulse:End Sub
Sub Bumper3_Hit:VpmTimer.PulseSw 46:PlaySoundAtVol SoundFX("bumper3",DOFContactors),Bumper3, VolBump :DOF 102, DOFPulse:End Sub	
Sub Bumper4_Hit:VpmTimer.PulseSw 46:PlaySoundAtVOl SoundFX("bumper4",DOFContactors),Bumper4, VolBump :DOF 104, DOFPulse:End Sub	
Sub Target19_Hit:VpmTimer.PulseSw 50:End Sub										'50
Sub Target27_Hit:VpmTimer.PulseSw 51:End Sub										'51
Sub Target25_Hit:VpmTimer.PulseSw 52:End Sub										'52
Sub Trigger2_Hit:Controller.Switch(53)=1:PlaySound"soundfx11":End Sub								'53,
Sub Trigger2_unHit:Controller.switch(53)=0:PlaySound"soundfx12":End Sub
Sub TDrop3_dropped:TRB.Hit 1:End Sub												'54
Sub LeftInlane_Hit:Controller.Switch(55)=1:PlaySound"soundfx":End Sub							'55
Sub LeftInlane_unHit:Controller.Switch(55)=0:Playsound"soundfx2":End Sub
Sub RightInlane_Hit:Controller.Switch(55)=1:PlaySound"soundfx3":End Sub
Sub RightInlane_UnHit:Controller.Switch(55)=0:PlaySound"soundfx4":End Sub 
Sub Target20_Hit:VpmTimer.PulseSw 60:End Sub										'60
Sub Target23_Hit:VpmTimer.PulseSw 61:End Sub										'61
Sub Target26_Hit:VpmTimer.PulseSw 62:End Sub										'62
Sub Trigger3_Hit:Controller.Switch(63)=1:PlaySound"soundfx13":End Sub								'63
Sub Trigger3_unHit:Controller.switch(63)=0:PlaySound"soundfx14":End Sub
Sub TDrop1_dropped:BLB.Hit 1:End Sub												'64
Sub LeftOutlane_Hit:Controller.Switch(65)=1:PlaySound"soundfx5":End Sub								'65
Sub LeftOutlane_unHit:Controller.Switch(65)=0:PlaySound"soundfx6":End Sub
Sub RightOutlane_Hit:Controller.Switch(65)=1:PlaySound"soundfx7":End Sub
Sub RightOutlane_UnHit:Controller.Switch(65)=0:PlaySound"soundfx8":End Sub
Sub Drain_Hit:PlaySound "drain":bsTrough.AddBall Me:End Sub 										'67
Sub Target21_Hit:VpmTimer.PulseSw 70:End Sub										'70
Sub Trigger10_Hit:Controller.Switch(71)=1:PlaySound"soundfx15":End Sub								'71
Sub Trigger10_unHit:Controller.switch(71)=0:PlaySound"soundfx16":End Sub
Sub Trigger11_Hit:Controller.Switch(72)=1:PlaySound"soundfx17":End Sub								'72
Sub Trigger11_unHit:Controller.switch(72)=0:PlaySound"soundfx18":End Sub
Sub Trigger4_Hit:Controller.Switch(73)=1:PlaySound"soundfx19":End Sub								'73
Sub Trigger4_unHit:Controller.switch(73)=0:PlaySound"soundfx20":End Sub
Sub Trigger5_Hit:Controller.Switch(73)=1:RLight.State=0:Playsound"soundfx21":End Sub
Sub Trigger5_unHit:Controller.switch(73)=0:RLight.State=1:PlaySound"soundfx22":End Sub
Sub TDrop2_dropped:BRB.Hit 1:End Sub												'74
Sub sw45b_Hit:vpmtimer.pulsesw 45:End Sub
Sub sw45c_Hit:vpmtimer.pulsesw 45:End Sub

'***************************************************
'***************************************************
' ramp animation
'***************************************************
'***************************************************

Ramp_Init
Sub Ramp_Init
    Ramp_Move 0
End Sub

Sub Ramp_Move (Enabled)
    If Enabled = 1 Then 'Up
        ramp12.Collidable = False
        PlaySound SoundFX("flapopen",DOFContactors)
        AuxRampLamps = 0
		rampmove.rotx=12
    Else                'Down
        ramp12.Collidable = True
        PlaySound SoundFX("flapclose",DOFContactors)
        AuxRampLamps = 1
		rampmove.rotx=0
    End If
    Timer2.Enabled = 0 'Aux Board 2
End Sub


'***************************************************
'       Aux Board Logic
'***************************************************

Dim AuxRampLamps1, AuxLampCount1

Sub Timer1_Timer
    AuxLampCount1 = (AuxLampCount1 + 1) Mod 10
    Select Case AuxLampCount1
        Case 0:
            SetLamp 101, 1: SetLamp 102,0:SetLamp 103,0:SetLamp 104, 0: SetLamp 105,0:SetLamp 106, 0: SetLamp 107,0:SetLamp 108, 0: SetLamp 109,0: SetLamp 110,0
        Case 1:
            SetLamp 101, 0: SetLamp 102,1:SetLamp 103,0:SetLamp 104, 0: SetLamp 105,0:SetLamp 106, 0: SetLamp 107,0:SetLamp 108, 0: SetLamp 109,0: SetLamp 110,0
        Case 2:
            SetLamp 101, 0: SetLamp 102,0:SetLamp 103,1:SetLamp 104, 0: SetLamp 105,0:SetLamp 106, 0: SetLamp 107,0:SetLamp 108, 0: SetLamp 109,0: SetLamp 110,0
        Case 3:
            SetLamp 101, 0: SetLamp 102,0:SetLamp 103,0:SetLamp 104, 1: SetLamp 105,0:SetLamp 106, 0: SetLamp 107,0:SetLamp 108, 0: SetLamp 109,0: SetLamp 110,0
        Case 4:
            SetLamp 101, 0: SetLamp 102,0:SetLamp 103,0:SetLamp 104, 0: SetLamp 105,1:SetLamp 106, 0: SetLamp 107,0:SetLamp 108, 0: SetLamp 109,0: SetLamp 110,0
        Case 5:
            SetLamp 101, 0: SetLamp 102,0:SetLamp 103,0:SetLamp 104, 0: SetLamp 105,0:SetLamp 106, 1: SetLamp 107,0:SetLamp 108, 0: SetLamp 109,0: SetLamp 110,0
        Case 6:
            SetLamp 101, 0: SetLamp 102,0:SetLamp 103,0:SetLamp 104, 0: SetLamp 105,0:SetLamp 106, 0: SetLamp 107,1:SetLamp 108, 0: SetLamp 109,0: SetLamp 110,0
        Case 7:
            SetLamp 101, 0: SetLamp 102,0:SetLamp 103,0:SetLamp 104, 0: SetLamp 105,0:SetLamp 106, 0: SetLamp 107,0:SetLamp 108, 1: SetLamp 109,0: SetLamp 110,0
        Case 8:
            SetLamp 101, 0: SetLamp 102,0:SetLamp 103,0:SetLamp 104, 0: SetLamp 105,0:SetLamp 106, 0: SetLamp 107,0:SetLamp 108, 0: SetLamp 109,1: SetLamp 110,0
        Case 9:
            SetLamp 101, 0: SetLamp 102,0:SetLamp 103,0:SetLamp 104, 0: SetLamp 105,0:SetLamp 106, 0: SetLamp 107,0:SetLamp 108, 0: SetLamp 109,0: SetLamp 110,1
    End Select
End Sub


Dim AuxRampLamps, AuxLampCount

Sub Timer2_Timer
    AuxLampCount = (AuxLampCount + 1) Mod 11
    Select Case AuxLampCount
        Case 0:
             SetLamp 111, 1: SetLamp 112, 0: SetLamp 113, 0 : SetLamp 114, 0 : SetLamp 115, 0 : SetLamp 116, 0 : SetLamp 117, 0 : SetLamp 118, 0 : SetLamp 119, 0 : SetLamp 120, 0
        Case 1:
             SetLamp 111, 0: SetLamp 112, 1: SetLamp 113, 0 : SetLamp 114, 0 : SetLamp 115, 0 : SetLamp 116, 0 : SetLamp 117, 0 : SetLamp 118, 0 : SetLamp 119, 0 : SetLamp 120, 0
        Case 3:
             SetLamp 111, 0: SetLamp 112, 0: SetLamp 113, 1 : SetLamp 114, 0 : SetLamp 115, 0 : SetLamp 116, 0 : SetLamp 117, 0 : SetLamp 118, 0 : SetLamp 119, 0 : SetLamp 120, 0
        Case 4:
             SetLamp 111, 0: SetLamp 112, 0: SetLamp 113, 0 : SetLamp 114, 1 : SetLamp 115, 0 : SetLamp 116, 0 : SetLamp 117, 0 : SetLamp 118, 0 : SetLamp 119, 0 : SetLamp 120, 0
        Case 5:
             SetLamp 111, 0: SetLamp 112, 0: SetLamp 113, 0 : SetLamp 114, 0 : SetLamp 115, 1 : SetLamp 116, 0 : SetLamp 117, 0 : SetLamp 118, 0 : SetLamp 119, 0 : SetLamp 120, 0
        Case 6:
             SetLamp 111, 0: SetLamp 112, 0: SetLamp 113, 0 : SetLamp 114, 0 : SetLamp 115, 0 : SetLamp 116, 1 : SetLamp 117, 0 : SetLamp 118, 0 : SetLamp 119, 0 : SetLamp 120, 0
        Case 7:
             SetLamp 111, 0: SetLamp 112, 0: SetLamp 113, 0 : SetLamp 114, 0 : SetLamp 115, 0 : SetLamp 116, 0 : SetLamp 117, 1 : SetLamp 118, 0 : SetLamp 119, 0 : SetLamp 120, 0
        Case 8:
             SetLamp 111, 0: SetLamp 112, 0: SetLamp 113, 0 : SetLamp 114, 0 : SetLamp 115, 0 : SetLamp 116, 0 : SetLamp 117, 0 : SetLamp 118, 1 : SetLamp 119, 0 : SetLamp 120, 0
        Case 9:
             SetLamp 111, 0: SetLamp 112, 0: SetLamp 113, 0 : SetLamp 114, 0 : SetLamp 115, 0 : SetLamp 116, 0 : SetLamp 117, 0 : SetLamp 118, 0 : SetLamp 119, 1 : SetLamp 120, 0
        Case 10:
             SetLamp 111, 0: SetLamp 112, 0: SetLamp 113, 0 : SetLamp 114, 0 : SetLamp 115, 0 : SetLamp 116, 0 : SetLamp 117, 0 : SetLamp 118, 0 : SetLamp 119, 0 : SetLamp 120, 1
    End Select
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

            'Special Handling
            If chgLamp(ii,0) = 2 Then MotorChooper chgLamp(ii,1)
            If chgLamp(ii,0) = 4 Then PFGI chgLamp(ii,1)
            If (chgLamp(ii,0) = 12 And chgLamp(ii,1) = 1) Then TLB.Hit 1
            If (chgLamp(ii,0) = 13 And chgLamp(ii,1) = 1) Then TRB.Hit 1
            If (chgLamp(ii,0) = 14 And chgLamp(ii,1) = 1) Then BLB.Hit 1
            If (chgLamp(ii,0) = 15 And chgLamp(ii,1) = 1) Then BRB.Hit 1
            If chgLamp(ii,0) = 16 Then Ramp_Move chgLamp(ii,1)

        Next
    End If
    UpdateLamps
End Sub



Sub UpdateLamps
    'NFadeL  0,  l0 'Game Over Relay
    'NFadeL  1,  l1 'Tilt Relay
    'NFadeL  2,  Light2 'Chopper Motor Relay
    NFadeL  3,  l3
    'NFadeL  4,  Light4 'GI Relay
	Flash 4, gion
	SetLamp 166, 1-LampState(4)
	Flash 166, gioff
    NFadeL  5,  l5
    NFadeL  6,  l6
    NFadeL  7,  l7
    NFadeL  8,  l8
    NFadeLm  9,  l9
	Flash 9, F9
    NFadeLm 10, l10
	Flash 10, F10
    NFadeLm 11, l11
	Flash 11, F11
'   NFadeL 12, l12 'Drop Target hit when lit
'   NFadeL 13, l13 'Drop Target hit when lit
'   NFadeL 14, l14 'Drop Target hit when lit
'   NFadeL 15, l15 'Drop Target hit when lit
'   NFadeL 16, l16 'Drop Target hit when lit
    NFadeLm 17, l17
	Flash 17, F17
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
    NFadeL 51, l51

'Solenoid Controlled Flashers
    Flash 94, fl3
	Flashm 94, Fl1
	Flashm 94, Fl2
    Flash 97, fr3
	Flashm 97, fr1
	Flashm 97, fr2

'   Aux lights
    NFadeL 101, AM1
    NFadeL 102, AM2
    NFadeL 103, AM3
    NFadeL 104, AM4
    NFadeL 105, AM5
    NFadeL 106, AM6
    NFadeL 107, AM7
    NFadeL 108, AM8
    NFadeL 109, AM9
    NFadeL 110, AM10

    NFadeL 111, AL1
    NFadeL 112, AL2
    NFadeL 113, AL3
    NFadeL 114, AL4
    NFadeL 115, AL5
    NFadeL 116, AL6
    NFadeL 117, AL7
    NFadeL 118, AL8
    NFadeL 119, AL9
    NFadeL 120, AL10
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

'***************************************************
'***************************************************
' Blade animation based on spinner
'***************************************************
'***************************************************

Sub sw76_Spin()
    vpmTimer.PulseSw 76
     playsoundatvol "fx_spinner", sw76, VolSpin
End Sub

'Const bladeStepMax = 40
'Const bladeRevsMax = 300
'Dim bladeRevs
'
'Randomize
'rotor.Rotz = rnd() * 360
'
''Sub spinnervelocity_hit()
''Dim vtemp
''    vtemp = (int)(abs(activeball.vely))
''    If vtemp > 8 Then
''        bladeRevs = bladeRevsMax
''        bladetimer.interval = 20
''        bladetimer.enabled = 1
''    End If
''End Sub
'
Sub MotorChooper(Enabled)
    If Enabled Then 'Blade On when Light 2 is on
'        bladeRevs = bladeRevsMax
'        bladetimer.interval = 20
        bladeTimer.Enabled = 1
    Else
        largetimer.enabled=1
    End if
End Sub
'
'Sub bladetimer_timer
'    rotor.Rotz = (rotor.Rotz - bladeStepMax *(bladeRevs/bladeRevsMax)) MOD 360
'    bladeRevs = bladeRevs - 1
'    if bladeRevs = 0 Then bladeTimer.Enabled = 0
'End Sub

Sub LargeTimer_Timer
	LargeTimer.Enabled=0
	bladeTimer.Enabled=0
End Sub

Sub bladeTimer_Timer
	rotornew.RotY=rotornew.RotY-1
	

End Sub

'Gottlieb Raven
'added by Inkochnito
'Added Coins chute by Mike da Spike
Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700,400,"Raven - DIP switches"
        .AddFrame 2,4,190,"Left Coin Chute (Coins/Credit)",&H0000001F,Array("4/1",&H0000000D,"2/1",&H0000000A,"1/1",&H00000000,"1/2",&H00000010) 'Dip 1-5
        .AddFrame 2,80,190,"Right Coin Chute (Coins/Credit)",&H00001F00,Array("4/1",&H00000D00,"2/1",&H00000A00,"1/1",&H00000000,"1/2",&H00001000) 'Dip 9-13
        .AddFrame 2,160,190,"Center Coin Chute (Coins/Credit)",&H001F0000,Array("4/1",&H000D0000,"2/1",&H000A0000,"1/1",&H00000000,"1/2",&H00010000) 'Dip 17-21
        .AddFrame 2,240,190,"3rd coin chute credits control",&H20000000,Array("no effect",0,"add 9",&H20000000)'dip 30        

		.AddFrame 207,4,190,"Maximum credits",49152,Array("8 credits",0,"10 credits",32768,"15 credits",&H00004000,"20 credits",49152)'dip 15&16
		.AddFrame 207,80,190,"Coin chute 1 and 2 control",&H00002000,Array("seperate",0,"same",&H00002000)'dip 14
		.AddFrame 207,126,190,"Playfield special",&H00200000,Array("replay",0,"extra ball",&H00200000)'dip 22
		.AddFrame 207,172,190,"High games to date control",&H00000020,Array("no effect",0,"reset high games 2-5 on power off",&H00000020)'dip 6
		.AddFrame 207,218,190,"Enable Snipers on completing",&H40000000,Array("all 6 bottom targets",0,"either 3 left or 3 right bottom targets",&H40000000)'dip 31
		.AddFrame 207,264,190,"Sniper special control",&H80000000,Array("special on all 4 Snipers hit",0,"Special on any 3 Sinpers hit",&H80000000)'dip 32

		.AddFrame 412,4,190,"High game to date awards",&H00C00000,Array("not displayed and no award",0,"displayed and no award",&H00800000,"displayed and 2 replays",&H00400000,"displayed and 3 replays",&H00C00000)'dip 23&24
		.AddFrame 412,80,190,"Balls per game",&H01000000,Array("5 balls",0,"3 balls",&H01000000)'dip 25
		.AddFrame 412,126,190,"Replay limit",&H04000000,Array("no limit",0,"one per game",&H04000000)'dip 27
		.AddFrame 412,172,190,"Novelty",&H08000000,Array("normal",0,"extra ball and replay scores points",&H08000000)'dip 28
		.AddFrame 412,218,190,"Game mode",&H10000000,Array("replay",0,"extra ball",&H10000000)'dip 29

		.AddChk 2,316,120,Array("Match feature",&H02000000)'dip 26
		.AddChk 205,316,120,Array("Attract sound",&H00000040)'dip 7
		.AddLabel 50,335,300,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub
Set vpmShowDips = GetRef("editDips")



'LED taken from Victory Table (Gottlieb1987) by Sindbad
'https://vpinball.com/VPBdownloads/victory-gottlieb-1987-2-0-1/

Dim Digits(40)
'Digits(0) = Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
'Digits(1) = Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
'Digits(2) = Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
'Digits(3) = Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
'Digits(4) = Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
'Digits(5) = Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
'Digits(6) = Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
'Digits(7) = Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
'Digits(8) = Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
'Digits(9) = Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
'Digits(10) = Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
'Digits(11) = Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
'Digits(12) = Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
'Digits(13) = Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
'Digits(14) = Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
'Digits(15) = Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
'Digits(16) = Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
'Digits(17) = Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
'Digits(18) = Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
'Digits(19) = Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
'Digits(20) = Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
'Digits(21) = Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
'Digits(22) = Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
'Digits(23) = Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
'Digits(24) = Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
'Digits(25) = Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
'Digits(26) = Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
'Digits(27) = Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
'Digits(28) = Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
'Digits(29) = Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
'Digits(30) = Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
'Digits(31) = Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)
'Digits(32) = Array(c00, c05, c0c, c0d, c08, c01, c06, c0f, c02, c03, c04, c07, c0b, c0a, c09, c0e)
'Digits(33) = Array(c10, c15, c1c, c1d, c18, c11, c16, c1f, c12, c13, c14, c17, c1b, c1a, c19, c1e)
'Digits(34) = Array(c20, c25, c2c, c2d, c28, c21, c26, c2f, c22, c23, c24, c27, c2b, c2a, c29, c2e)
'Digits(35) = Array(c30, c35, c3c, c3d, c38, c31, c36, c3f, c32, c33, c34, c37, c3b, c3a, c39, c3e)
'Digits(36) = Array(c40, c45, c4c, c4d, c48, c41, c46, c4f, c42, c43, c44, c47, c4b, c4a, c49, c4e)
'Digits(37) = Array(c50, c55, c5c, c5d, c58, c51, c56, c5f, c52, c53, c54, c57, c5b, c5a, c59, c5e)
'Digits(38) = Array(c60, c65, c6c, c6d, c68, c61, c66, c6f, c62, c63, c64, c67, c6b, c6a, c69, c6e)
'Digits(39) = Array(c70, c75, c7c, c7d, c78, c71, c76, c7f, c72, c73, c74, c77, c7b, c7a, c79, c7e)

Sub DisplayTimer_Timer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			if (num < 40) then
              'For Each obj In Digits(num)
                   'If chg And 1 Then obj.State=stat And 1
                   chg=chg\2 : stat=stat\2
                  'Next
			Else
			       end if
        Next
    End If
 End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
  ' PlaySound SoundFX("slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
	RandomSoundSlings
	DOF 106, DOFPulse
	vpmtimer.pulsesw 45
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
  ' PlaySound SoundFX("slingshot2",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
	RandomSoundSlings
	DOF 105, DOFPulse
	vpmtimer.pulsesw 45
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
    PlaySound sound, 0, ABS(BOT.velz)/17, Pan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
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
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
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
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

End Sub

'*****************************************
'   ninuzzu's   BALL SHADOW
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

'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*VolPi, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, .1, AudioPan(ActiveBall)*VolTarg, 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall)*VolGates, 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundBumper()
	Select Case Int(Rnd*2)+1
		Case 1 : PlaySound "bumper", 0, VolBump, AudioPan(ActiveBall), 0, 1, 0, 1, AudioFade(ActiveBall)
		Case 2 : PlaySound "bumper2", 0, VolBump, AudioPan(ActiveBall), 0, 1, 0, 1, AudioFade(ActiveBall)
	End Select
End Sub

Sub RandomSoundSlings()
	Select Case Int(Rnd*2)+1
		Case 1 : PlaySound SoundFX("slingshot",DOFContactors), 0, 1, AudioPan(ActiveBall), 0, 1, 0, 1, AudioFade(ActiveBall)
		Case 2 : PlaySound SoundFX("slingshot",DOFContactors), 0, 1, AudioPan(ActiveBall), 0, 1, 0, 1, AudioFade(ActiveBall)
	End Select
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub Trigger6_Hit()
	PlaySoundAtVol "fx_ballrampdrop", ActiveBall, 1
End Sub

Sub Trigger7_Hit()
	PlaySoundAtVol "fx_ballrampdrop", ActiveBall, 1
End Sub

Sub AUX2A_Init()

End Sub

' Start Rambo First Blood Part II music 
    MusicOn

Sub MusicOn
    Dim x
    x = INT(12 * RND(12) )
    Select Case x
       Case 0:PlayMusic "Rambo II OST 01 - Main Title.mp3"       
       Case 1:PlayMusic "Rambo II OST 03 - Preparations.mp3"
       Case 2:PlayMusic "Rambo II OST 04 - The Jump.mp3"
       Case 3:PlayMusic "Rambo II OST 05 - The Snake.mp3"
	   Case 4:PlayMusic "Rambo II OST 10 - River Cash,The Gunboat.mp3"
	   Case 5:PlayMusic "Rambo II OST 11 - Betrayed.mp3"
	   Case 6:PlayMusic "Rambo II OST 13 - Escape From Torture.mp3"
	   Case 7:PlayMusic "Rambo II OST 15 - Revenge.mp3"
	   Case 8:PlayMusic "Rambo II OST 16 - Bowed Down.mp3"
	   Case 9:PlayMusic "Rambo II OST 17 - Pilot Over.mp3"
	   Case 10:PlayMusic "Rambo II OST 18 - Village Raid,Helicopter Flight.mp3"
	   Case 11:PlayMusic "Rambo II OST 19 - Home Flight.mp3"

    End Select
 End Sub
 
 Sub Table1_MusicDone()
    MusicOn
 End Sub

Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub