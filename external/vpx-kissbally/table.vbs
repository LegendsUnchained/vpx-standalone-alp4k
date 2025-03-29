''''''Kiss (Bally 1979) Tom 1.3'''''

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'********************* Rom Selection ***************************
Const SCoin="coin"
 'Const cGameName = "kiss"  ,UseSolenoids=2,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_Flipperup",SFlipperOff="fx_Flipperdown"
 Const cGameName = "kissb" ,UseSolenoids=2,UseLamps=1,UseGI=1,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_Flipperup",SFlipperOff="fx_Flipperdown"
 'Const cGameName = "kissc" ,UseSolenoids=2,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_Flipperup",SFlipperOff="fx_Flipperdown"

'******************************************************************
Const Ballmass = 1.7
Const Ballsize = 50

LoadVPM "01550000", "Bally.vbs", 3.26

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


'Solenoid Call backs
'**********************************************************************************************************
SolCallback(6) = "vpmSolSound SoundFX(""knocker"",DOFKnocker),"
SolCallback(7) = "bsTrough.SolOut"
SolCallback(14) = "DTraised" '"dtbank1.SolDropUp"
SolCallback(17) = "SolGateDiverter"
SolCallback(19) = "vpmNudge.SolGameOn"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFx("LeftFlipper",DOFContactors),LeftFlipper,LF.fire'LeftFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFx("LeftFlipperdown",DOFContactors),LeftFlipper,.1:LeftFlipper.RotateToStart
     End If
  End Sub

Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFx("RightFlipper",DOFContactors),RightFlipper,RF.fire'RightFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFx("RightFlipperdown",DOFContactors),RightFlipper,.1:RightFlipper.RotateToStart
     End If
End Sub



'**********************************************************************************************************

 'Solenoid Controlled toys
'**********************************************************************************************************

Sub SolGateDiverter(enabled)
	If Enabled Then
        gate.RotateToStart:
		PlaySoundAtVol "metalhit_medium", gate, 2
     Else
        gate.RotateToEnd:
		PlaySoundAtVol "metalhit_medium", gate, 2
     End If
End Sub

Sub DTraised(enabled)
	if enabled then DTreset.enabled=True
End Sub

Sub DTreset_timer
	dtbank1.DropSol_On
	dt1 = 0
	dt2 = 0
	dt3 = 0
	dt4 = 0
	DTreset.enabled=False
End Sub


'*****GI Lights On
dim xx
For each xx in GI:xx.State = 1: Next


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

Sub FlipperTimer_Timer
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	PrimitiveGate.roty = gate.currentangle
    
End Sub


'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************
  Dim bsTrough, dtBank1


Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Miss World (Geiger)"&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0


     PinMAMETimer.Interval = PinMAMEInterval
     PinMAMETimer.Enabled = 1

     vpmNudge.TiltSwitch = swTilt
     vpmNudge.Sensitivity = .2
     vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, Bumper4, LeftSlingshot, RightSlingshot)

     Set bsTrough = New cvpmBallStack
         bsTrough.InitSw 0, 8, 0, 0, 0, 0, 0, 0
         bsTrough.InitKick BallRelease, 80, 6
         bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
         bsTrough.Balls = 1

     set dtbank1 = new cvpmdroptarget
         dtbank1.initdrop array(sw1, sw2, sw3, sw4), array(1, 2, 3, 4)
         dtbank1.initsnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

 End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)

	If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound "nudge"
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound "nudge"
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound "nudge"
	If keycode = LeftFlipperKey Then LFPress = 1
	If keycode = RightFlipperKey Then rfpress = 1
	If keycode = PlungerKey Then Plunger.Pullback:PlaySoundAt "plungerpull",Plunger
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If keycode = LeftFlipperKey Then 
		lfpress = 0
		leftflipper.eostorqueangle = EOSA
		leftflipper.eostorque = EOST
	End If
	If keycode = RightFlipperKey Then 
		rfpress = 0
		rightflipper.eostorqueangle = EOSA
		rightflipper.eostorque = EOST
	End If
	If keycode = PlungerKey Then Plunger.Fire:PlaySoundAt "plunger",Plunger
	If KeyUpHandler(keycode) Then Exit Sub
End Sub

'**********************************************************************************************************
RightFlipper.timerinterval=1
rightflipper.timerenabled=True

sub RightFlipper_timer()

	If leftflipper.currentangle = leftflipper.endangle and LFPress = 1 then 
		leftflipper.eostorqueangle = EOSAnew
		leftflipper.eostorque = EOSTnew
		LeftFlipper.rampup = EOSRampup
		if LFCount < LiveCatch Then
			LFCount = LFCount + 1
			leftflipper.Elasticity = 0.1
			If LeftFlipper.endangle <> LFEndAngle Then leftflipper.endangle = LFEndAngle
		Else	
			leftflipper.Elasticity = FElasticity
		end if
	elseif leftflipper.currentangle > leftflipper.startangle - 0.05  Then
		leftflipper.rampup = SOSRampup
		leftflipper.endangle = LFEndAngle - 3
		leftflipper.Elasticity = FElasticity
		LFCount = 0
	elseif leftflipper.currentangle > leftflipper.endangle + 0.01 Then 
		leftflipper.eostorque = EOST
		leftflipper.eostorqueangle = EOSA
		LeftFlipper.rampup = Frampup
		leftflipper.Elasticity = FElasticity
	end if

	If rightflipper.currentangle = rightflipper.endangle and RFPress = 1 then
		rightflipper.eostorqueangle = EOSAnew
		rightflipper.eostorque = EOSTnew
		RightFlipper.rampup = EOSRampup
		if RFCount < LiveCatch Then
			RFCount = RFCount + 1
			rightflipper.Elasticity = 0.1
			If RightFlipper.endangle <> RFEndAngle Then rightflipper.endangle = RFEndAngle
		Else
			rightflipper.Elasticity = FElasticity
		end if
	elseif rightflipper.currentangle < rightflipper.startangle + 0.05 Then
		rightflipper.rampup = SOSRampup 
		rightflipper.endangle = RFEndAngle + 3
		rightflipper.Elasticity = FElasticity
		RFCount = 0 
	elseif rightflipper.currentangle < rightflipper.endangle - 0.01 Then 
		rightflipper.eostorque = EOST
		rightflipper.eostorqueangle = EOSA
		RightFlipper.rampup = Frampup
		rightflipper.Elasticity = FElasticity
	end if

end sub

dim LFPress, RFPress, EOST, EOSA, EOSTnew, EOSAnew
dim FStrength, Frampup, FElasticity, EOSRampup, SOSRampup
dim RFEndAngle, LFEndAngle, LFCount, RFCount, LiveCatch

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
FStrength = LeftFlipper.strength
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
EOSTnew = 1.0 'FEOST
EOSAnew = 0.2
EOSRampup = 1.5 
SOSRampup = 8.5 
LiveCatch = 12

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
'******************************************************

' Drain holes
 Sub Drain_Hit:PlaySoundAtVol "drain",drain,.5:bsTrough.AddBall Me:End Sub

'scoring rubbers
 Sub Sling3_Hit:vpmTimer.PulseSw 25:PlaySoundAt "Rubber",ActiveBall:End Sub
 Sub Sling4_Hit:vpmTimer.PulseSw 25:PlaySoundAt "Rubber",ActiveBall:End Sub
 Sub Sling5_Hit:vpmTimer.PulseSw 25:PlaySoundAt "Rubber",ActiveBall:End Sub

 'Bumpers
 Sub Bumper1_Hit:vpmTimer.PulseSw 38: PlaySoundAt SoundFX("fx_bumper1",DOFContactors),Bumper1: End Sub
 Sub Bumper2_Hit:vpmTimer.PulseSw 40: PlaySoundAt SoundFX("fx_bumper2",DOFContactors),Bumper2: End Sub
 Sub Bumper3_Hit:vpmTimer.PulseSw 37: PlaySoundAt SoundFX("fx_bumper3",DOFContactors),Bumper3: End Sub
 Sub Bumper4_Hit:vpmTimer.PulseSw 39: PlaySoundAt SoundFX("fx_bumper4",DOFContactors),Bumper4: End Sub

 'Rollovers
 Sub sw34_Hit:Controller.Switch(34) = 1 : PlaySoundAtVol "rollover",sw34,.5: End Sub
 Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub
 Sub sw27_Hit:Controller.Switch(27) = 1 : PlaySoundAtVol "rollover",sw27,.5: End Sub
 Sub sw27_UnHit:Controller.Switch(27) = 0:End Sub
 Sub sw26_Hit:Controller.Switch(26) = 1 : PlaySoundAtVol "rollover",sw26,.5: End Sub
 Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub
 Sub sw33_Hit:Controller.Switch(33) = 1 : PlaySoundAtVol "rollover",sw33,.5: End Sub
 Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub
 Sub sw24_Hit:Controller.Switch(24) = 1 : PlaySoundAtVol "rollover",sw24,.5: End Sub
 Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
 Sub sw23_Hit:Controller.Switch(23) = 1 : PlaySoundAtVol "rollover",sw23,.5: End Sub
 Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
 Sub sw5_Hit:Controller.Switch(5) = 1 : PlaySoundAtVol "rollover",sw5,.5: End Sub
 Sub sw5_UnHit:Controller.Switch(5) = 0:End Sub
 Sub sw22_Hit:Controller.Switch(22) = 1 : PlaySoundAtVol "rollover",sw22,.5: End Sub
 Sub sw22_UnHit:Controller.Switch(22) = 0:End Sub
 Sub sw21_Hit:Controller.Switch(21) = 1 : PlaySoundAtVol "rollover",sw21,.5: End Sub
 Sub sw21_UnHit:Controller.Switch(21) = 0:End Sub

 'Spinners
 Sub SPinner1_Spin():vpmTimer.PulseSw 31 : PlaySoundAtVol "fx_spinner",Spinner1,2: End Sub
 Sub SPinner2_Spin():vpmTimer.PulseSw 30 : PlaySoundAtVol "fx_spinner2",Spinner2,2 : End Sub

Dim dt1, dt2, dt3, dt4

 ' Droptargets
 Sub sw1_Hit:dtbank1.Hit 1:dt1 = 1:End Sub
 Sub sw2_Hit:dtbank1.Hit 2:dt2 = 1:End Sub
 Sub sw3_Hit:dtbank1.Hit 3:dt3 = 1:End Sub
 Sub sw4_Hit:dtbank1.Hit 4:dt4 = 1:End Sub


Sub CheckDropLights ()

	If dt1 = 1 Then
		lightdt1.state = 1
	Else
		lightdt1.state = 0
	End If

	If dt2 = 1 Then
		lightdt2.state = 1
		lightdt5.state = 1
	Else
		lightdt2.state = 0
		lightdt5.state = 0
	End If

	If dt3 = 1 Then
		lightdt3.state = 1
		lightdt6.state = 1
	Else
		lightdt3.state = 0
		lightdt6.state = 0
	End If

	If dt4 = 1 Then
		lightdt4.state = 1
	Else
		lightdt4.state = 0
	End If

End Sub


 ' Targets
 Sub sw12_Hit:vpmTimer.PulseSw 12:End Sub
 Sub sw13_Hit:vpmTimer.PulseSw 13:End Sub
 Sub sw14_Hit:vpmTimer.PulseSw 14:End Sub
 Sub sw15_Hit:vpmTimer.PulseSw 15:End Sub
 Sub sw17_Hit:vpmTimer.PulseSw 17:End Sub
 Sub sw18_Hit:vpmTimer.PulseSw 18:End Sub
 Sub sw19_Hit:vpmTimer.PulseSw 19:End Sub
 Sub sw20_Hit:vpmTimer.PulseSw 20:End Sub

'Lights mapped to an array
Set Lights(1)= l1
Set Lights(2)= l2
Set Lights(3)= l3
Set Lights(4)= l4
Set Lights(5)= l5
Set Lights(6)= l6
Set Lights(7)= l7
Set Lights(8)= l8
Lights(9)= array(l9,l9a)
Set Lights(10)= l10
Set Lights(12)= l12
Set Lights(15)= l15
Set Lights(17)= l17
Set Lights(18)= l18
Set Lights(19)= l19
Set Lights(20)= l20
Set Lights(21)= l21
Set Lights(22)= l22
Set Lights(23)= l23
Set Lights(24)= l24
Lights(25)= array(l25,l25a)
Lights(26)= array(Light12,Light21,light12a,Light21a) 'Bumper Lights
Set Lights(28)= l28
Set Lights(30)= l30
Set Lights(31)= l31
Set Lights(33)= l33
Set Lights(34)= l34
Set Lights(35)= l35
Set Lights(36)= l36
Set Lights(37)= l37
Set Lights(38)= l38
Set Lights(39)= l39
Set Lights(40)= l40
Lights(41)= array(l41,l41a)
Lights(42)= array(Light15,Light16,Light15a,Light16a) 'Bumper Lights
Set Lights(43)= l43
Set Lights(44)= l44
Set Lights(46)= l46
Set Lights(47)= l47
Set Lights(49)= l49
Set Lights(50)= l50
Set Lights(51)= l51
Set Lights(52)= l52
Set Lights(53)= l53
Set Lights(54)= l54
Set Lights(55)= l55
Set Lights(56)= l56
Lights(57)= array(l57,l57a)
Set Lights(58)= l58
Set Lights(59)= l59
Set Lights(60)= l60
Set Lights(62)= l62
Set Lights(63)= l63

 '************************************
 '          LEDs Display
 'Based on Scapino's 7 digit Reel LEDs
 '************************************


 LampTimer.Interval = 35
 LampTimer.Enabled = 1

 Sub LampTimer_Timer()
    UpdateLeds
	CheckDropLights
 End Sub


 Dim Digits(32)
 Dim Patterns(11)
 Dim Patterns2(11)

 Patterns(0) = 0     'empty
 Patterns(1) = 63    '0
 Patterns(2) = 6     '1
 Patterns(3) = 91    '2
 Patterns(4) = 79    '3
 Patterns(5) = 102   '4
 Patterns(6) = 109   '5
 Patterns(7) = 125   '6
 Patterns(8) = 7     '7
 Patterns(9) = 127   '8
 Patterns(10) = 111  '9

 Patterns2(0) = 128  'empty
 Patterns2(1) = 191  '0
 Patterns2(2) = 134  '1
 Patterns2(3) = 219  '2
 Patterns2(4) = 207  '3
 Patterns2(5) = 230  '4
 Patterns2(6) = 237  '5
 Patterns2(7) = 253  '6
 Patterns2(8) = 135  '7
 Patterns2(9) = 255  '8
 Patterns2(10) = 239 '9

 Set Digits(0) = a0
 Set Digits(1) = a1
 Set Digits(2) = a2
 Set Digits(3) = a3
 Set Digits(4) = a4
 Set Digits(5) = a5
 Set Digits(6) = a6

 Set Digits(7) = b0
 Set Digits(8) = b1
 Set Digits(9) = b2
 Set Digits(10) = b3
 Set Digits(11) = b4
 Set Digits(12) = b5
 Set Digits(13) = b6

 Set Digits(14) = c0
 Set Digits(15) = c1
 Set Digits(16) = c2
 Set Digits(17) = c3
 Set Digits(18) = c4
 Set Digits(19) = c5
 Set Digits(20) = c6

 Set Digits(21) = d0
 Set Digits(22) = d1
 Set Digits(23) = d2
 Set Digits(24) = d3
 Set Digits(25) = d4
 Set Digits(26) = d5
 Set Digits(27) = d6

 Set Digits(28) = e0
 Set Digits(29) = e1
 Set Digits(30) = e2
 Set Digits(31) = e3

 Sub UpdateLeds
     On Error Resume Next
     Dim ChgLED, ii, jj, chg, stat
     ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
     If Not IsEmpty(ChgLED)Then
         For ii = 0 To UBound(ChgLED)
             chg = chgLED(ii, 1):stat = chgLED(ii, 2)

             For jj = 0 to 10
                 If stat = Patterns(jj)OR stat = Patterns2(jj)then Digits(chgLED(ii, 0)).SetValue jj
             Next
         Next
     End IF
 End Sub
  Sub NFadeT(nr, a, b)
     Select Case controller.lamp(nr)
         Case False:a.Text = ""
         Case True:a.Text = b
     End Select
End Sub

dim zz
If Table1.ShowDT = false then
    For each zz in DT:zz.Visible = false: Next
else
    For each zz in DT:zz.Visible = true: Next
End If


'**********************************************************************************************************
'**********************************************************************************************************

 'Bally Kiss
 'added by Inkochnito
 Sub editDips
     Dim vpmDips:Set vpmDips = New cvpmDips
     With vpmDips
         .AddForm 700, 400, "Kiss - DIP switches"
         .AddChk 2, 10, 180, Array("Match feature", &H00100000)                                                                                                                                                                                                           'dip 21
         .AddChk 205, 10, 115, Array("Credits display", &H00080000)                                                                                                                                                                                                       'dip 20
         .AddFrame 2, 30, 190, "Maximum credits", &H00070000, Array("5 credits", 0, "10 credits", &H00010000, "15 credits", &H00020000, "20 credits", &H00030000, "25 credits", &H00040000, "30 credits", &H00050000, "35 credits", &H00060000, "40 credits", &H00070000) 'dip 17&18&19
         .AddFrame 2, 160, 190, "Sound features", &H80000080, Array("chime effects", &H80000000, "chime and tunes", 0, "noise", &H00000080, "noises and tunes", &H80000080)                                                                                               'dip 8&32
         .AddFrame 2, 235, 190, "High score to date", &H00000060, Array("no award", 0, "1 credit", &H00000020, "2 credits", &H00000040, "3 credits", &H00000060)                                                                                                          'dip 6&7
         .AddFrame 2, 310, 190, "High score feature", &H00006000, Array("no award", 0, "extra ball", &H00004000, "replay", &H00006000)                                                                                                                                    'dip 14&15
         .AddFrame 205, 30, 190, "Balls per game", 32768, Array("3 balls", 0, "5 balls", 32768)                                                                                                                                                                           'dip 16
         .AddFrame 205, 76, 190, "After completing KISS card 3 times", &H00200000, Array("any letter made is not held over", 0, "any letter made is held over", &H00200000)                                                                                               'dip 22
         .AddFrame 205, 122, 190, "Light-a-line lite", &H00400000, Array("goes on and off", 0, "stays lit", &H00400000)                                                                                                                                                   'dip 23
         .AddFrame 205, 168, 190, "KISS special lites", &H00800000, Array("after 'colossal' lite", 0, "with 'colossal' lite", &H00800000)                                                                                                                                 'dip 24
         .AddFrame 205, 214, 190, "'Opens gate when lit' lite", &H10000000, Array("remains lit", 0, "lites 1 in 3", &H10000000)                                                                                                                                           'dip 29
         .AddFrame 205, 260, 190, "Light-a-line lite", &H20000000, Array("lites for next ball", 0, "comes up same as last ball", &H20000000)                                                                                                                              'dip 30
         .AddFrame 205, 306, 190, "Any A-B-C-D made is", &H40000000, Array("not held in memory", 0, "held in memory for next ball", &H40000000)                                                                                                                           'dip 31
         .AddLabel 50, 382, 300, 20, "After hitting OK, press F3 to reset game with new settings."
         .ViewDips
     End With
 End Sub
 Set vpmShowDips = GetRef("editDips")

'**********************************************************************************************************
'**********************************************************************************************************
'Start of VPX call Backs

'**********************************************************************************************************
'**********************************************************************************************************


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 35
   PlaySoundAt SoundFX("left_slingshot",DOFContactors),SLING1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -15
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
	vpmTimer.PulseSw 36
    PlaySoundAt SoundFX("right_slingshot",DOFContactors),SLING2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -15
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
  Vol = Csng(BallVel(ball) ^2 / 2600)
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

Const tnob = 1 ' total number of balls
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
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else ' Ball on raised ramp
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, Pan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
				End If
		Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
      PlaySoundAtBOTBallZ "fx_ball_drop" & b, BOT(b)
      'debug.print BOT(b).velz
    End If
    Next
End Sub


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 200, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

Sub Pins_Hit (idx):PlaySoundAtBall "pinhit_low":End Sub
Sub Targets_Hit (idx):PlaySoundAt "target",ActiveBall:End Sub
Sub Metals_Thin_Hit (idx):PlaySoundAtBall "metalhit_thin":End Sub
Sub Metals_Medium_Hit (idx):PlaySoundAtBall "metalhit_medium":End Sub
Sub Metals2_Hit (idx):PlaySoundAtBall "metalhit2":End Sub

Sub Gates_Hit (idx)
	PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub



'***********************
' Random Rubber Bumps
'***********************


Sub Rubbers_Hit(idx)
    PlaySoundAtBallVol "rubber_hit_" & Int(Rnd*3)+1, .1
End Sub

Sub Posts_Hit(idx)
    PlaySoundAtBallVol "rubber_hit_" & Int(Rnd*3)+1, .1
End Sub


'***********************
' Random Flipper Hits
'***********************

Sub LeftFlipper_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 1
End Sub

Sub RightFlipper_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 1
End Sub

'*********** BALL SHADOW *********************************t
Dim BallShadow
BallShadow = Array (BallShadow1)

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
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 10
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 10
        End If
        ballShadow(b).Y = BOT(b).Y + 20
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

Dim NextOrbitHit:NextOrbitHit = 0

Sub Wall40_Hit()
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump .2, 50000
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much.
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .3 + (Rnd * .2)
	end if
End Sub

Sub Metals_Thin_Hit(idx)
	if BallVel(ActiveBall) > .05 and Timer > NextOrbitHit then
		RandomBumpMetals_Thin 1, 2000
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much.
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .1 + (Rnd * .2)
	end if
End Sub

Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "fx_rampbump" & CStr(Int(Rnd*7)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

Sub RandomBumpMetals_Thin(voladj, freq)
	dim BumpSnd:BumpSnd= "fx_sensor" & CStr(Int(Rnd*3)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

Sub Table1_Exit()

End Sub

'******************************************************
'			STEPS 2-4 (FLIPPER POLARITY SETUP
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		x.TimeDelay = 44
	Next

	
	'rf.report "Polarity"
	AddPt "Polarity", 0, 0, -2.7
	AddPt "Polarity", 1, 0.16, -2.7	
	AddPt "Polarity", 2, 0.33, -2.7
	AddPt "Polarity", 3, 0.37, -2.7	'4.2
	AddPt "Polarity", 4, 0.41, -2.7
	AddPt "Polarity", 5, 0.45, -2.7 '4.2
	AddPt "Polarity", 6, 0.576,-2.7
	AddPt "Polarity", 7, 0.66, -1.8'-2.1896
	AddPt "Polarity", 8, 0.743, -0.5
	AddPt "Polarity", 9, 0.81, -0.5
	AddPt "Polarity", 10, 0.88, 0

	'"Velocity" Profile
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp

End Sub

'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'******************************************************
'		FLIPPER CORRECTION SUPPORTING FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper -
'ProcessBalls - catches ball data.
' - OR -
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
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
	Public Property Let EndPoint(aInput) : if IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end if : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property

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
				if DebugOn then StickL.visible = True : StickL.x = balldata(x).x		'debug TODO
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
		if abs(Flipper.currentAngle - Flipper.EndAngle) < 30 Then
			PartialFlipCoef = 0
		End If
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			if tmp < 0.1 then 'if real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
				if DebugOn then TestStr = "real pos < 0.1 ( " & round(tmp,2) & ")" : tbpl.text = Teststr
				'RemoveBall aBall
				'Exit Sub
			end if

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				if DebugOn then teststr = "y velocity: " & round(aBall.vely, 3) & "exit sub" : tbpl.text = teststr
				RemoveBall aBall
				exit Sub
			end if
			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					'TB.TEXT = balldata(x).id & " " & BALLDATA(X).X & VBNEWLINE & FLIPPERSTART & " " & FLIPPEREND
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				if DebugOn then set tmp = new spoofball : tmp.data = aBall : End If
				if IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'if tip hit with no collected data, do vel correction anyway
					if PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						if Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						if Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
						if DebugOn then teststr = "tip protection" & vbnewline & "velcoef: " & round(velcoef,3) & vbnewline & round(PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1),3) & vbnewline
						'debug.print teststr
					end if
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					if Enabled then aBall.Velx = aBall.Velx*VelCoef
					if Enabled then aBall.Vely = aBall.Vely*VelCoef
				end if
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			'debug
			if DebugOn then
				TestStr = teststr & "%pos:" & round(BallPos,2)
				if IsEmpty(PolarityOut(0) ) then
					teststr = teststr & vbnewline & "(Polarity Disabled)" & vbnewline
				else
					teststr = teststr & "+" & round(1 *(AddX*ycoef*PartialFlipcoef),3)
					if BallPos >= PolarityOut(uBound(PolarityOut) ) then teststr = teststr & "(MAX)" & vbnewline else teststr = teststr & vbnewline end if
					if Ycoef < 1 then teststr = teststr &  "ycoef: " & ycoef & vbnewline
					if PartialFlipcoef < 1 then teststr = teststr & "PartialFlipcoef: " & round(PartialFlipcoef,4) & vbnewline
				end if

				teststr = teststr & vbnewline & "Vel: " & round(BallSpeed(tmp),2) & " -> " & round(ballspeed(aBall),2) & vbnewline
				teststr = teststr & "%" & round(ballspeed(aBall) / BallSpeed(tmp),2)
				tbpl.text = TestSTR
			end if
		Else
			'if DebugOn then tbpl.text = "td" & timedelay
		End If
		RemoveBall aBall
	End Sub
End Class

'================================
'Helper Functions


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

Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub


Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

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


Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp if on the boundry lines
	'if L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'if L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function


