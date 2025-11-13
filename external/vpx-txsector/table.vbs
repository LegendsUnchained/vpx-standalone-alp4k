Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

' Thalamus 2020 January : Improved directional sounds
' !! NOTE : Table not verified yet !!

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 20    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 1  ' Bumpers volume.
Const VolRol    = 0.1    ' Rollovers volume.
Const VolGates  = 0.1  ' Gates volume.
Const VolMetal  = 0.1    ' Metals volume.
Const VolRB     = 0.1   ' Rubber bands volume.
Const VolRH     = 0.1    ' Rubber hits volume.
Const VolPo     = 0.1   ' Rubber posts volume.
Const VolPi     = 0.1    ' Rubber pins volume.
Const VolPlast  = 0.1    ' Plastics volume.
Const VolTarg   = 0.1    ' Targets volume.
Const VolWood   = 0.1   ' Woods volume.
Const VolKick   = 0.1    ' Kicker volume.
Const VolSpin   = 0.15  ' Spinners volume.
Const VolFlip   = 0.1    ' Flipper volume.

'----- FlexDMD Options -----
Dim UseFlexDMD:UseFlexDMD = 1		' 1 = on, 0 = off (Non VRRoom only. Intended for Real DMD users but will work on LCD)
Const FlexColour = 0				' 0 = default, 1 = cyan, 2 = red, 3 = yellow, 4 = Green, 5 = blue, 6 = white

Const cGameName = "txsector",UseSolenoids=2,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="Flipper2",SFlipperOff="Flipper"
Const SCoin="coin3",cCredits=""

Const Ballsize = 50

If Table1.ShowDT = True Then 'Show Desktop components
ramp15.visible=1
Ramp16.visible=1
DisplayTimer.Enabled = 1
Else
ramp15.visible=0
Ramp16.visible=0
DisplayTimer.Enabled = 0
End if

LoadVPM "01110000", "sys80.vbs", 3.1

' Thalamus - for Fast Flip v2
NoUpperRightFlipper

Set LampCallback=GetRef("UpdateMultipleLamps")

gion()

sub gion()
Table1.colorgradeimage = "ColorGradeLUT256x16_ConSat"
gi_1.state=1
gi_2.state=1
GI_3.state=1
GI_4.state=1
gi_5.state=1
GI_6.state=1
GI_7.state=1
gi_8.state=1
gi_9.state=1
GI_10.state=1
GI_12.state=1
gi_13.state=1
gi_14.state=1
gi_20.state=1
GI_28.state=1
GI_29.state=1
gi_31.state=1
GI_34.state=1
GI_43.state=1
GI_53.state=1
End Sub

sub gioff()
Table1.colorgradeimage = "ColorGrade_4"
gi_1.state=0
gi_2.state=0
gi_3.state=0
gi_4.state=0
gi_5.state=0
GI_6.state=0
GI_7.state=0
gi_8.state=0
gi_9.state=0
GI_10.state=0
GI_12.state=0
gi_13.state=0
gi_14.state=0
gi_20.state=0
GI_28.state=0
GI_29.state=0
gi_31.state=0
GI_34.state=0
GI_43.state=0
GI_53.state=0
End Sub
if rnd*2 > 1 Then
ball2.CreateSizedBall(25)
	ball2.Kick 180,1
	ball2.enabled=false
Else
ball3.CreateSizedBall(25)
	ball3.Kick 180,1
	ball3.enabled=false
end If
ball4.CreateSizedBall(25)
	ball4.Kick 180,1
	ball4.enabled=false
ball5.CreateSizedBall(25)
	ball5.Kick 180,1
	ball5.enabled=false

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(1)="sol1"
SolCallback(2)="sol2"
SolCallback(5)="sol5"
SolCallback(6)="sol6"
SolCallback(8)="vpmSolSound SoundFX(""knocker"",DOFKnocker)," 'knocker
SolCallback(9)="k4" 'outhole
SolCallback(3) = "dtr4.SolDropUp"
SolCallback(4) = "DTt4.SolDropUp"
SolCallback(7) = "DTl1.SolDropUp"




sub sol1 (enabled)
	if enabled Then
			if light12.state =1 Then
			lightsol1.state=1
			lightsol1a.state=1
            lightsol1b.state=1
		Else
			Kicker5.kick 135,2
			lightsol1.state=0
			lightsol1a.state=0
            lightsol1b.state=0
		end If
	Else
			lightsol1.state=0
			lightsol1a.state=0
            lightsol1b.state=0
	end If
end Sub

sub sol2 (enabled)
	if enabled Then
		if light12.state =1Then
			lightsol2.state=1
			lightsol2a.state=1
            lightsol2b.state=1
		Else
			Kicker5.kick 135,2
			Kicker2.kick RnDNum(198,202),2
			lightsol2.state=0
			lightsol2a.state=0
            lightsol2b.state=0
		end If
	Else
			lightsol2.state=0
			lightsol2a.state=0
            lightsol2b.state=0
	end If
end Sub

sub sol5 (enabled)
	if enabled Then
		if light12.state =1 Then
			lightsol5.state=1
			lightsol5a.state=1
		Else
			Kicker3.kick 270,2
			lightsol5.state=0
			lightsol5a.state=0
		end If
	Else
			lightsol5.state=0
			lightsol5a.state=0
	end If
end Sub

sub sol6 (enabled)
	if enabled Then
		if light12.state =1 Then
			lightsol6a.state=1
			lightsol6.state=1
		Else
			Kicker1.kick 135,2
			lightsol6.state=0
			lightsol6a.state=0
		end If
	Else
			lightsol6.state=0
			lightsol6a.state=0
	end If
end Sub

Sub sollFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("fx_Flipperup",DOFContactors), LeftFlipper, 1:LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
         PlaySoundAtVol "fx_Flipperup", LeftFlipper1, 1
     Else
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFContactors), LeftFlipper, 1:LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
         PlaySoundAtVol "fx_Flipperdown", LeftFlipper1, 1
     End If
  End Sub

Sub solrflipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("fx_Flipperup",DOFContactors), RightFlipper, 1:RightFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFContactors), RightFlipper, 1:RightFlipper.RotateToStart
     End If
End Sub

'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

Dim FlipperActive




'Primitive Flipper
Sub FlipperTimer_Timer
	FlipperT1.roty = LeftFlipper.currentangle  + 237
	FlipperT2.roty = LeftFlipper1.currentangle  + 200
	FlipperT5.roty = RightFlipper.currentangle + 120
rfs.rotz = RightFlipper.currentangle
lfs.rotz = LeftFlipper.currentangle
End Sub

Sub UpdateMultipleLamps

End Sub



'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsKicker, bsHole, DTl1, DTt4, dtr4, bssaucer,k1,k2

Sub Table1_Init

	FlexDMD_Init

	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "TX-Sector"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
		If Err Then MsgBox Err.Description
	End With
	If UseFlexDMD Then ExternalEnabled = Controller.Games(cGameName).Settings.Value("showpindmd")
	If UseFlexDMD Then Controller.Games(cGameName).Settings.Value("showpindmd") = 0
	On Error Goto 0
		Controller.SolMask(0)=0
      vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
		Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

    vpmNudge.TiltSwitch=57
    vpmNudge.Sensitivity=5
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3)

	Set dtt4=New cvpmDropTarget
   dtt4.InitDrop Array(Targt4a,Targt4b,Targt4c,Targt4d),Array(40,50,60,70)
   dtt4.InitSnd SoundFX("flapclos",DOFContactors),SoundFX("flapopen",DOFContactors)

	Set dtr4=New cvpmDropTarget
   dtr4.InitDrop Array(Targr4a,Targr4b,Targr4c,targr4d),Array(41,51,61,71)
   dtr4.InitSnd SoundFX("flapclos",DOFContactors),SoundFX("flapopen",DOFContactors)


   Set dtl1=New cvpmDropTarget
   dtl1.InitDrop Array(Targl1a),Array(42)
   dtl1.InitSnd SoundFX("flapclos",DOFContactors),SoundFX("flapopen",DOFContactors)

End Sub


'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
If KeyCode=LeftFlipperKey Then Controller.Switch(6)=1
If KeyCode=RightFlipperKey Then Controller.Switch(16)=1

If keycode = PlungerKey Then Plunger.Pullback:playsoundAtVol "plungerpull", Plunger, 1
If vpmKeyDown(KeyCode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)

If KeyCode=RightFlipperKey Then
	Controller.Switch(46)=0
	Controller.Switch(16)=0
end If
If KeyCode=LeftFlipperKey Then Controller.Switch(6)=0
If keycode = PlungerKey Then Plunger.Fire:PlaySoundAtVol "plunger", Plunger, 1
If vpmKeyUp(KeyCode) Then Exit Sub
End Sub

'**********************************************************************************************************

'Arngrim offers
'DOF 104 : Left oultane switch
'DOF 105 : Right outlane switch
'DOF 121 : Right slingshot
'DOF 122 : Left slingshot
'DOF 123 : Left lower trigger switch
'DOF 124 : Right lower trigger switch
'Add by Thalamus 2020 January

Sub Targt4a_dropped:DTt4.Hit 1:End Sub
Sub Targt4b_dropped:DTt4.Hit 2:End Sub
Sub Targt4c_dropped:DTt4.Hit 3:End Sub
Sub Targt4d_dropped:DTt4.Hit 4:End Sub
Sub Targr4a_dropped:dtr4.Hit 1:End Sub
Sub Targr4b_dropped:dtr4.Hit 2:End Sub
Sub Targr4c_dropped:dtr4.Hit 3:End Sub
Sub Targr4d_dropped:dtr4.Hit 4:End Sub
Sub Targl1a_dropped:dtl1.Hit 1:End Sub

Sub sw35_Hit:Controller.Switch(35)=1:End Sub	'switch 35
Sub sw35_unHit:Controller.Switch(35)=0:End Sub	'switch 35
Sub sw36_Hit:Controller.Switch(36)=1:End Sub	'switch 36
Sub sw36_unHit:Controller.Switch(36)=0:End Sub	'switch 36
Sub sw46l_Hit:Controller.Switch(46)=1:DOF 104, DOFOn:End Sub	'switch 46
Sub sw46l_unHit:Controller.Switch(46)=0:DOF 104, DOFOff:End Sub	'switch 46
Sub sw46r_Hit:Controller.Switch(46)=1:DOF 105, DOFOn:End Sub	'switch 46
Sub sw46r_unHit:Controller.Switch(46)=0:DOF 105, DOFOff:End Sub	'switch 46

Sub sw44_Hit:Controller.Switch(44)=1:End Sub	'switch 44
Sub sw44_unHit:Controller.Switch(44)=0:End Sub	'switch 44
Sub sw43_Hit:Controller.Switch(43)=1:End Sub	'switch 43
Sub sw43_unHit:Controller.Switch(43)=0:End Sub	'switch 43

Sub sw45_Hit:Controller.Switch(45)=1:End Sub'switch 45
Sub sw45_unHit:Controller.Switch(45)=0:End Sub'switch 45
Sub sw55_Hit:Controller.Switch(55)=1:End Sub'switch 55
Sub sw55_unHit:Controller.Switch(55)=0:End Sub'switch 55
Sub sw65_Hit:Controller.Switch(65)=1:End Sub'switch 65
Sub sw65_unHit:Controller.Switch(65)=0:End Sub'switch 65
Sub sw75_Hit:Controller.Switch(75)=1:End Sub'switch 75
Sub sw75_unHit:Controller.Switch(75)=0:End Sub'switch 75
Sub sw66_Hit:Controller.Switch(66)=1:PlaySoundAt "Drain", sw66:End Sub'switch 66
Sub sw66_unHit:Controller.Switch(66)=0:End Sub'switch 66
Sub sw63_Hit:Controller.Switch(63)=1:End Sub'switch 63
Sub sw63_unHit:Controller.Switch(63)=0:End Sub'switch 63

Sub sw53_Hit:vpmTimer.PulseSw(53):End Sub	'switch 53
Sub sw54_Hit:vpmTimer.PulseSw(54):End Sub	'switch 54
Sub sw64_Hit:vpmTimer.PulseSw(64):End Sub	'switch 64

Sub sw76l_SlingShot:vpmTimer.PulseSw(76): PlaySoundAtVol "undef_hit2", Primitive101, 1 :PlaySound SoundFXDOF("",123,DOFPulse,DOFContactors) :End Sub	'switch 76
Sub sw76r_SlingShot:vpmTimer.PulseSw(76): PlaySoundAtVol "undef_hit2", Primitive8, 1   :PlaySound SoundFXDOF("",124,DOFPulse,DOFContactors)  :End Sub	'switch 76
Sub sw76lt_Hit:vpmTimer.PulseSw(76):End Sub	'switch 76
Sub sw76t_Hit:vpmTimer.PulseSw(76):End Sub	'switch 76
Sub sw76rt_Hit:vpmTimer.PulseSw(76):End Sub	'switch 76

Sub sw73_Spin:vpmTimer.PulseSw(73):End Sub	'switch 73
Sub sw74_Spin:vpmTimer.PulseSw(74):End Sub	'switch 74											'switch 57 Tilt
Sub tr1_hit:tl1.state=2:End Sub
Sub tr1_unhit:tl1.state=0:End Sub


Sub sw56_Hit:Controller.Switch(56)=1:End Sub'switch 56
Sub sw56_unHit:Controller.Switch(56)=0:End Sub'switch 56

Sub Bumper1_Hit:vpmTimer.PulseSw(72):RandomSoundbumper:End Sub	'switch 72
Sub Bumper2_Hit:vpmTimer.PulseSw(52):RandomSoundbumper:End Sub	'switch 52
Sub Bumper3_Hit:vpmTimer.PulseSw(62):RandomSoundbumper:End Sub	'switch 62


Sub sw43b_Hit
if k2=1 then
dtl1.hit 1
k2=0
Else
k2=1
end If
End Sub
Sub sw43t_Hit:k2=0:End Sub

Sub sw451_Hit
 Kicker5.kick 135,2
 Kicker3.kick 270,2
End Sub
Sub sw551_Hit
 Kicker2.kick 200,2
End Sub
Sub sw651_Hit
 Kicker3.kick 270,2
Kicker5.kick 135,2
end Sub
Sub sw751_Hit
Kicker1.kick RnDNum(132,138),2
End Sub
sub k4(Enabled)
 Kicker4.kick 70,20
End Sub

'

Dim BallsInPlay



'**********************************************************************************************************
' Map Lights to Array
'**********************************************************************************************************
set Lights(13)=light13
set lights(12)=light12
set Lights(2)=light2
 lights(3)=array(Light3,light3a)
 lights(5)=array(Light5,light5a)
set lights(4)=light4
 lights(6)=array(Light6,light6a)
 lights(7)=array(Light7,light7a)
set lights(8)=light8
set lights(9)=light9
set lights(10)=light10
 lights(11)=array(light11,Light11a)
set lights(1)=light1
 lights(15)=array(Light15,Light15a,light15b,Light15h,Light15ah,Light15bh)
 lights(16)=array(Light16,Light16a,light16b,Light16h,Light16ah,Light16bh)
 lights(17)=array(Light17,Light17a,light17b,light17h,Light17ah,Light17bh)
 lights(18)=array(Light18,Light18a,light18b,Light18h,Light18ah,Light18bh)
 lights(19)=array(Light19,Light19a,light19b,Light19h,Light19ah,Light19bh)
set lights(20)=Light20
set lights(21)=Light21
set lights(22)=light22
set lights(23)=light23
set lights(24)=light24
set lights(25)=light25
set lights(26)=light26
set lights(27)=light27
set lights(28)=light28
set lights(29)=light29

set lights(30)=light30
set lights(31)=light31
set lights(32)=light32
set lights(33)=light33
set lights(34)=light34
set lights(35)=light35
set lights(36)=light36
set lights(37)=light37
set lights(38)=light38
set lights(39)=light39
set lights(40)=light40
set lights(41)=light41
set lights(42)=light42
set lights(43)=light43
set lights(44)=light44
set lights(45)=light45
set lights(46)=light46
lights(47)=array(light47,light47a)
set lights(50)=light50
lights(51)=array(light51,light51a)
set lights(49)=light48

'**********************************************************************************************************
' Backglass Light Displays
'**********************************************************************************************************


Sub LampTimer_Timer()
	if light1.state=0 Then
		gion()
	Else
		gioff()
	end If

	If light2.State=1 Then
		releasein.collidable=1
		releaseout.collidable=0
		kicker001.kick 100,5
	Else
		releaseout.collidable=1
		releasein.collidable=0
	End If
	if k1<>light13.state Then
		if light13.state then
			dtl1.hit 1
			k1=light13.state
		end If
	end If
if light15.state then
 bulb005.blenddisablelighting = 8
bulb010.blenddisablelighting = 8
bulb015.blenddisablelighting = 8
 else 
bulb005.blenddisablelighting = 0
bulb010.blenddisablelighting = 0
bulb015.blenddisablelighting = 0
end If
if light16.state then
 bulb001.blenddisablelighting = 8
bulb009.blenddisablelighting = 8
bulb014.blenddisablelighting = 8
 else 
bulb001.blenddisablelighting = 0
bulb009.blenddisablelighting = 0
bulb014.blenddisablelighting = 0
end If
if light17.state then
 bulb002.blenddisablelighting = 8
bulb007.blenddisablelighting = 8
bulb013.blenddisablelighting = 8
 else 
bulb002.blenddisablelighting = 0
bulb007.blenddisablelighting = 0
bulb013.blenddisablelighting = 0
end If
if light18.state then
 bulb003.blenddisablelighting = 8
bulb008.blenddisablelighting = 8
bulb012.blenddisablelighting = 8
 else 
bulb003.blenddisablelighting = 0
bulb008.blenddisablelighting = 0
bulb012.blenddisablelighting = 0
end If
if light19.state then
 bulb004.blenddisablelighting = 8
bulb006.blenddisablelighting = 8
bulb011.blenddisablelighting = 8
 else 
bulb004.blenddisablelighting = 0
bulb006.blenddisablelighting = 0
bulb011.blenddisablelighting = 0
end If
End Sub



'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
 Dim Digits(40)
 Digits(0)=Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
 Digits(1)=Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
 Digits(2)=Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
 Digits(3)=Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
 Digits(4)=Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
 Digits(5)=Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
 Digits(6)=Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
 Digits(7)=Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
 Digits(8)=Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
 Digits(9)=Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
 Digits(10)=Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
 Digits(11)=Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
 Digits(12)=Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
 Digits(13)=Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
 Digits(14)=Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
 Digits(15)=Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
 Digits(16)=Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
 Digits(17)=Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
 Digits(18)=Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
 Digits(19)=Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)

 Digits(20)=Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
 Digits(21)=Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
 Digits(22)=Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
 Digits(23)=Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
 Digits(24)=Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
 Digits(25)=Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
 Digits(26)=Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
 Digits(27)=Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
 Digits(28)=Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
 Digits(29)=Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
 Digits(30)=Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
 Digits(31)=Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)
 Digits(32)=Array(ac18, ac16, acc1, acd1, ac19, ac17, ac15, acf1, ac11, ac13, ac12, ac14, acb1, aca1, ac10, ace1)
 Digits(33)=Array(ad18, ad16, adc1, add1, ad19, ad17, ad15, adf1, ad11, ad13, ad12, ad14, adb1, ada1, ad10, ade1)
 Digits(34)=Array(ae18, ae16, aec1, aed1, ae19, ae17, ae15, aef1, ae11, ae13, ae12, ae14, aeb1, aea1, ae10, aee1)
 Digits(35)=Array(af18, af16, afc1, afd1, af19, af17, af15, aff1, af11, af13, af12, af14, afb1, afa1, af10, afe1)
 Digits(36)=Array(b9, b7, b0c1, b0d1, b100, b8, b6, b0f1, b2, b4, b3, b5, b0b1, b0a1, b1,b0e1)
 Digits(37)=Array(b109, b107, b1c1, b1d1, b110, b108, b106, b1f1, b102, b104, b103, b105, b1b1, b1a1, b101,b1e1)
 Digits(38)=Array(b119, b117, b2c1, b2d1, b120, b118, b116, b2f1, b112, b114, b113, b115, b2b1, b2a1, b111, b2e1)
 Digits(39)=Array(b129, b127, b3c1, b3d1, b130, b128, b126, b3f1, b122, b3b1, b123, b125, b3b1, b3a1, b121, b3e1)


 Sub DisplayTimer_Timer
    Dim ChgLED, ii, num, chg, stat, obj
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
		If Not FlexDMD is Nothing Then FlexDMD.LockRenderThread
		For ii=0 To UBound(chgLED)
			num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			If UseFlexDMD then UpdateFlexChar num, stat
			If Table1.ShowDT = True Then
				if (num < 40) then
					For Each obj In Digits(num)
						If chg And 1 Then obj.State=stat And 1
						chg=chg\2 : stat=stat\2
					Next
				Else
				End If
			End If
		Next
		If Not FlexDMD is Nothing Then FlexDMD.UnlockRenderThread
    End If
 End Sub
'**********************************************************************************************************
'**********************************************************************************************************

' Dip Switches
' ==================================================================
' Gottlieb TX-Section
' originally added by Inkochnito
' Added Coins chute by Mike da Spike
Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
        .AddForm  700,400,"TX-Section - DIP switches"
	    .AddFrame 2,4,190,"Left Coin Chute (Coins/Credit)",&H0000001F,Array("1/4",&H00000018,"1/2",&H00000010,"1/1",&H00000000,"2/1",&H0000000A) 'Dip 1-5
	    .AddFrame 2,80,190,"Right Coin Chute (Coins/Credit)",&H00001F00,Array("1/4",&H00001800,"1/2",&H00001000,"1/1",&H00000000,"2/1",&H00000A00) 'Dip 9-13
	    .AddFrame 2,160,190,"Center Coin Chute (Coins/Credit)",&H001F0000,Array("1/4",&H00180000,"1/2",&H00100000,"1/1",&H00000000,"2/1",&H000A0000) 'Dip 17-21
		.AddFrame 2,240,190,"3rd coin chute credits control",&H20000000,Array("no effect",0,"add 9",&H20000000)'dip 30
		.AddFrame 207,4,190,"Maximum credits",49152,Array("8 credits",0,"10 credits",32768,"15 credits",&H00004000,"20 credits",49152)'dip 15&16
		.AddFrame 207,80,190,"Coin chute 1 and 2 control",&H00002000,Array("seperate",0,"same",&H00002000)'dip 14
		.AddFrame 207,126,190,"Playfield special",&H00200000,Array("replay",0,"extra ball",&H00200000)'dip 22
		.AddFrame 207,172,190,"High games to date control",&H00000020,Array("no effect",0,"reset high games 2-5 on power off",&H00000020)'dip 6
		.AddFrame 207,218,190,"Auto-percentage control",&H00000080,Array("disabled (normal high score mode)",0,"enabled",&H00000080)'dip 8
		.AddFrame 207,264,190,"Playfield special control",&H40000000,Array("special lit after banks compl. 3 times",0,"special lit after banks compl. 2 times",&H40000000)'dip 31
		.AddFrame 412,4,190,"High game to date awards",&H00C00000,Array("not displayed and no award",0,"displayed and no award",&H00800000,"displayed and 2 replays",&H00400000,"displayed and 3 replays",&H00C00000)'dip 23&24
		.AddFrame 412,80,190,"Balls per game",&H01000000,Array("5 balls",0,"3 balls",&H01000000)'dip 25
		.AddFrame 412,126,190,"Replay limit",&H04000000,Array("no limit",0,"one per game",&H04000000)'dip 27
		.AddFrame 412,172,190,"Novelty",&H08000000,Array("normal",0,"extra ball and replay scores 500K",&H08000000)'dip 28
		.AddFrame 412,218,190,"Game mode",&H10000000,Array("replay",0,"extra ball",&H10000000)'dip 29
		.AddFrame 412,264,190,"Extra bal control",&H80000000,Array("Reset energy level",0,"Memorize energy level",&H80000000)'dip 32
		.AddChk 412,316,180,Array("Match feature",&H02000000)'dip 26
		.AddChk 412,331,190,Array("Attract sound",&H00000040)'dip 7
		.AddLabel 50,360,300,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub
Set vpmShowDips = GetRef("editDips")

 Sub Table1_Exit ' in some tables this needs to be Table1_Exit
    Controller.Pause = False
	Controller.Stop
	If UseFlexDMD then
		If Not FlexDMD is Nothing Then 
			FlexDMD.Show = False
			FlexDMD.Run = False
			FlexDMD = NULL
		End if
		Controller.Games(cGameName).Settings.Value("showpindmd") = ExternalEnabled
	End if
 End Sub

'**********************************************************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep,LStep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw(76)
    PlaySoundAtVol SoundFX("right_slingshot",DOFContactors), sling1, 1
    DOF 121, DOFPulse
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


Sub leftSlingShot_Slingshot
	vpmTimer.PulseSw(76)
    PlaySoundAtVol SoundFX("left_slingshot",DOFContactors), sling2, 1
	DOF 122, DOFPulse
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    leftSlingShot.TimerEnabled = 1
End Sub

Sub leftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:leftSlingShot.TimerEnabled = 0:
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
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
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
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub BallShadow_Timer() 
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6)
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
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10                       
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 90
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
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

' The sound is played using the VOL, PAN and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the PAN function will change the stereo position according
' to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.

Sub rollingmetal_Hit (idx)
'PlaySound SoundFX("Wire ramp",DOFContactors), 0, 1, 0.05, 0.05
PlaySound "Wire Ramp", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub


Sub Switches_Hit (idx)
	PlaySound "switch_hit1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub



Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub


Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub



Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub



Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub RandomSoundbumper()
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtBallVol SoundFX("fx_Bumper",DOFContactors), 1
		Case 2 : PlaySoundAtBallVol SoundFX("fx_Bumper1",DOFContactors), 1
		Case 3 : PlaySoundAtBallVol SoundFX("fx_Bumper2",DOFContactors), 1
		Case 4 : PlaySoundAtBallVol SoundFX("fx_bumper3",DOFContactors), 1
		Case 5 : PlaySoundAtBallVol SoundFX("fx_bumper4",DOFContactors), 1
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub
Sub LeftFlipper1_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub
Sub RightFlipper1_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

'**********************************************************************************************************
' FlexDMD code - scutters
'**********************************************************************************************************
Dim FlexDMD
DIm FlexDMDDict
Dim FlexDMDScene
Dim ExternalEnabled

Sub FlexDMD_Init() 'default/startup values

	'setup flex dmd

	If UseFlexDMD = 0 then 
		Set FlexDMD = nothing 
		UseFlexDMD =0 
		exit sub
	end if
	
	Dim i

	' populate the lookup dictionary for mapping display characters
	FlexDictionary_Init

	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")

	If Not FlexDMD is Nothing Then
	
		FlexDMD.GameName = cGameName
 		FlexDMD.TableFile = Table1.Filename & ".vpx"
		FlexDMD.RenderMode = 2
		FlexDMD.Width = 128
		FlexDMD.Height = 32
		FlexDMD.Clear = True
		FlexDMD.Run = True

		Set FlexDMDScene = FlexDMD.NewGroup("Scene")
		
		With FlexDMDScene
			'populate blank display
			.AddActor FlexDMD.NewImage("BackG", "FlexDMD.Resources.dmds.black.png")
			
			.AddActor FlexDMD.NewFrame("Frame")
			.GetFrame("Frame").Visible = True
			Select Case FlexColour
			Case 1
				.GetFrame("Frame").FillColor = vbYellow
				.GetFrame("Frame").BorderColor = vbYellow
			Case 2
				.GetFrame("Frame").FillColor = vbBlue
				.GetFrame("Frame").BorderColor = vbBlue
			Case 3
				.GetFrame("Frame").FillColor = vbCyan
				.GetFrame("Frame").BorderColor = vbCyan
			Case 4
				.GetFrame("Frame").FillColor = vbGreen
				.GetFrame("Frame").BorderColor = vbGreen
			Case 5
				.GetFrame("Frame").FillColor = vbRed
				.GetFrame("Frame").BorderColor = vbRed
			Case 6
				.GetFrame("Frame").FillColor = vbWhite
				.GetFrame("Frame").BorderColor = vbWhite
			Case Else
				.GetFrame("Frame").FillColor = RGB(255,128,0)
				.GetFrame("Frame").BorderColor = RGB(255,128,0)
			End Select
			.GetFrame("Frame").Height = 32
			.GetFrame("Frame").Width= 128
			.GetFrame("Frame").Fill= True
			.GetFrame("Frame").Thickness= 1

			.AddActor FlexDMD.NewImage("Back", "VPX.DMD_Background")

			'40 segment display holders
			for i = 0 to 19 
				'first line
				.AddActor FlexDMD.NewImage("Seg" & i, "VPX.DMD_Space")
				.GetImage("Seg" & i).SetAlignedPosition 4 + i * 6,0,0
				'second line
				.AddActor FlexDMD.NewImage("Seg" & i+20, "VPX.DMD_Space")
				.GetImage("Seg" & i+20).SetAlignedPosition 4 + i * 6,16,0
			next
	
		End With

		FlexDMD.LockRenderThread
		FlexDMD.Stage.AddActor FlexDMDScene
		
		FlexDMD.Show = True
		FlexDMD.UnlockRenderThread

		DisplayTimer.Enabled = 1

	Else
		
		UseFlexDMD = 0

	End If

End Sub

Sub FlexDictionary_Init

	Set FlexDMDDict = CreateObject("Scripting.Dictionary")

	FlexDMDDict.Add 0, "VPX.DMD_Space"
	FlexDMDDict.Add 63, "VPX.DMD_O"
	FlexDMDDict.Add 8704, "VPX.DMD_1"
	FlexDMDDict.Add 2139, "VPX.DMD_2"
	FlexDMDDict.Add 2127, "VPX.DMD_3"
	FlexDMDDict.Add 2150, "VPX.DMD_4"
	FlexDMDDict.Add 2157, "VPX.DMD_S"
	FlexDMDDict.Add 2173, "VPX.DMD_6"
	FlexDMDDict.Add 7, "VPX.DMD_7"
	FlexDMDDict.Add 2175,"VPX.DMD_8"
	FlexDMDDict.Add 2159,"VPX.DMD_9"
	
	FlexDMDDict.Add 191,"VPX.DMD_Odot"
	FlexDMDDict.Add 8832, "VPX.DMD_1dot"
	FlexDMDDict.Add 2267, "VPX.DMD_2dot"
	FlexDMDDict.Add 2255, "VPX.DMD_3dot"
	FlexDMDDict.Add 2278, "VPX.DMD_4dot"
	FlexDMDDict.Add 2285, "VPX.DMD_Sdot"
	FlexDMDDict.Add 2301, "VPX.DMD_6dot"
	FlexDMDDict.Add 135, "VPX.DMD_7dot"
	FlexDMDDict.Add 2303, "VPX.DMD_8dot"
	FlexDMDDict.Add 2287, "VPX.DMD_9dot"
	
	FlexDMDDict.Add 2167, "VPX.DMD_A"
	FlexDMDDict.Add 10767, "VPX.DMD_B"
	FlexDMDDict.Add 57, "VPX.DMD_C"
	FlexDMDDict.Add 8719, "VPX.DMD_D"
	FlexDMDDict.Add 121, "VPX.DMD_E"
	FlexDMDDict.Add 113, "VPX.DMD_F"
	FlexDMDDict.Add 2109, "VPX.DMD_G"
	FlexDMDDict.Add 2166, "VPX.DMD_H"
	FlexDMDDict.Add 8713, "VPX.DMD_I"
	FlexDMDDict.Add 30, "VPX.DMD_J"
	FlexDMDDict.Add 5232, "VPX.DMD_K"
	FlexDMDDict.Add 56, "VPX.DMD_L"
	FlexDMDDict.Add 1334, "VPX.DMD_M"
	FlexDMDDict.Add 4406, "VPX.DMD_N"
	' "O" = 0
	FlexDMDDict.Add 2163, "VPX.DMD_P"
	FlexDMDDict.Add 4159, "VPX.DMD_Q"
	FlexDMDDict.Add 6259, "VPX.DMD_R"
	 ' "S" = 5
	FlexDMDDict.Add 8705, "VPX.DMD_T"
	FlexDMDDict.Add 62, "VPX.DMD_U"
	FlexDMDDict.Add 17456, "VPX.DMD_V"
	FlexDMDDict.Add 20534, "VPX.DMD_W"
	FlexDMDDict.Add 21760, "VPX.DMD_X"
	FlexDMDDict.Add 9472, "VPX.DMD_Y"
	FlexDMDDict.Add 17417, "VPX.DMD_Z"
	
	FlexDMDDict.Add &h400,"VPX.DMD_SingleQuote"
	FlexDMDDict.Add 16640, "VPX.DMD_CloseBracket"
	FlexDMDDict.Add 5120, "VPX.DMD_OpenBracket"
	FlexDMDDict.Add 2120, "VPX.DMD_Equals"
	FlexDMDDict.Add 10275, "VPX.DMD_Question"
	FlexDMDDict.Add 2112, "VPX.DMD_Minus"
	FlexDMDDict.Add 10861, "VPX.DMD_Dollar"
	FlexDMDDict.Add 6144, "VPX.DMD_GreaterThan"
	FlexDMDDict.Add 65535, "VPX.DMD_Hash"
	FlexDMDDict.Add 32576, "VPX.DMD_Asterick"
	FlexDMDDict.Add 10816, "VPX.DMD_Plus"
	
End sub

Sub UpdateFlexChar(id, value)
	
	If id < 40 Then
		if FlexDMDDict.Exists (value) then
			FlexDMDScene.GetImage("Seg" & id).Bitmap = FlexDMD.NewImage("", FlexDMDDict.Item (value)).Bitmap
		Else
			FlexDMDScene.GetImage("Seg" & id).Bitmap = FlexDMD.NewImage("", "VPX.DMD_Space").Bitmap
		end if
	End If

End Sub

'**********************************************************************************************************
'**********************************************************************************************************




