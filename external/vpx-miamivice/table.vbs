Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="comet_l5",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM "01560000", "S11.VBS", 3.26
Dim DesktopMode: DesktopMode = Table1.ShowDT

dim xx, hiddenvalue
If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=15
hiddenvalue=0
Else
Ramp16.visible=0
Ramp15.visible=0
hiddenvalue=1
	End if

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1) = "bsTrough.SolOut"
SolCallback(2) = "dtDTBank.SolDropUp"
SolCallback(3) = "bsSaucer.SolOut"
SolCallback(4) = "Setlamp 104,"	'Corkscrew Flash
SolCallback(5) = "Setlamp 105,"	'Cycle Flash
SolCallback(6) = "bsCycleSaucer.SolOut"
'SolCallback(7) = ""	'Player3 Flasher
'SolCallback(8) = ""	'Player1 Flasher
'SolCallback(9) = ""	'Player4 Flasher
'SolCallback(10) = ""	'Player2 Flasher
SolCallback(11) = "PFGI"
SolCallback(15) =  "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(23) = "vpmNudge.SolGameOn"
  
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart
     End If
End Sub

Sub FlipperTimer_Timer
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	lflip.rotz = LeftFlipper.currentangle
	rflip.rotz = RightFlipper.currentangle
End Sub

'******************
' RealTime Updates
'******************

Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
UpdateGates
BallShadowUpdate
End Sub

Sub UpdateGates
	rampgate_prim.RotX = GateSw50.CurrentAngle
	rampexitgate_prim.RotX = Gatesw28.CurrentAngle+90
	corkscrewgate_prim.RotX = GateSw19.CurrentAngle
	plungegate_prim.RotX = Gate4.CurrentAngle + 90
	If sw29.isdropped=1 then dropshadow.image="blank" end If
	If sw29.isdropped=0 then dropshadow.image="dropshadow" end if
End Sub


'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

dim GIIsOff

'Playfield GI
Sub PFGI(Enabled)
	If Enabled Then
		GiOFF
		dim xx
'		For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"
		Table1.ColorGradeImage = "ColorGradeLUT256x16_shadowcrush"
		GIIsOff=true
		
	Else
		GiON
'		For each xx in GI:xx.State = 1: Next
        PlaySound "fx_relay"
		Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1"
		GIIsOff=false
		
end if 
End Sub

Sub GiON
	Dim x
	For each x in Gi:x.State = 1:Next
End Sub

Sub GiOFF
	Dim x
	For each x in Gi:x.State = 0:Next
End Sub


'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsSaucer, bsCycleSaucer, dtDTBank

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Miami Vice (TBA 2020)"&chr(13)&"by IVANTBA"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=0
		.ShowFrame=0
		.Games(cGameName).Settings.Value("sound")=0    'TURNS OFF ROM SOUNDS.
		.ShowTitle=0
        	.hidden = hiddenvalue
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0
 
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

  	vpmNudge.TiltSwitch=1
  	vpmNudge.Sensitivity=3
  	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)
 
   Set bsTrough = New cvpmBallStack
       bsTrough.InitSw 0,45,0,0,0,0,0,0
       bsTrough.InitKick BallRelease, 90, 8
       bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
       bsTrough.Balls = 1
 
   Set bsSaucer = New cvpmBallStack
       bsSaucer.InitSaucer sw24,24, 175, 8
       bsSaucer.KickForceVar = 2
       bsSaucer.KickAngleVar = 2
       bsSaucer.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
 
  Set bsCycleSaucer = New cvpmBallStack
      bsCycleSaucer.InitSaucer sw25,25, 260, 8
      bsCycleSaucer.KickForceVar = 2
      bsCycleSaucer.KickAngleVar = 2
      bsCycleSaucer.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
 
  Set dtDTBank = new cvpmdroptarget
      dtDTBank.InitDrop sw29,29
      dtDTBank.Initsnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

  End Sub

PlaySound "Intro",-0 

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
End Sub

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub
Sub sw24_Hit:bsSaucer.AddBall 0 : playsound "popper_ball": End Sub
Sub sw25_Hit:bsCycleSaucer.AddBall 0 : playsound "popper_ball": End Sub

'Star Rollovers
Sub sw9_Hit:Controller.Switch(9) = 1:PlaySound"rollover":End Sub
Sub sw9_UnHit:Controller.Switch(9) = 0:End Sub

 'Stand Up Targets
Sub sw10_Hit:vpmTimer.PulseSw 10:playsound"target":End Sub
Sub sw11_Hit:vpmTimer.PulseSw 11:playsound"target2":End Sub
Sub sw12_Hit:vpmTimer.PulseSw 12:playsound"target3":End Sub
Sub sw13_Hit:vpmTimer.PulseSw 13:playsound"target4":End Sub
Sub sw14_Hit:vpmTimer.PulseSw 14:playsound"target5":End Sub
Sub sw15_Hit:vpmTimer.PulseSw 15:playsound"target6":End Sub
Sub sw16_Hit:vpmTimer.PulseSw 16:playsound"target7":End Sub
Sub sw17_Hit:vpmTimer.PulseSw 17:playsound"target8":End Sub
Sub sw18_Hit:vpmTimer.PulseSw 18:playsound"target9":End Sub

'Gate Triggers
Sub sw19_Hit:vpmTimer.PulseSw 19:End Sub
Sub sw28_Hit:vpmTimer.PulseSw 28:End Sub
Sub sw50_Hit:vpmTimer.PulseSw 50:End Sub

'Drop Targets
Sub Sw29_Dropped:dtDTBank.Hit 1 :playsound"plastichit":End Sub 

 'Scoring Rubber
Sub sw33_Hit:vpmTimer.PulseSw 33 : playsound"flip_hit_3" : End Sub 
Sub sw34_Hit:vpmTimer.PulseSw 34 : playsound"flip_hit_3" : End Sub 
Sub sw35_Hit:vpmTimer.PulseSw 35 : playsound"flip_hit_3" : End Sub 
Sub sw36_Hit:vpmTimer.PulseSw 36 : playsound"flip_hit_3" : End Sub 
Sub sw37_Hit:vpmTimer.PulseSw 37 : playsound"flip_hit_3" : End Sub 
Sub sw38_Hit:vpmTimer.PulseSw 38 : playsound"flip_hit_3" : End Sub 
Sub sw39_Hit:vpmTimer.PulseSw 39 : playsound"flip_hit_3" : End Sub 

'Wire Triggers
Sub sw20_Hit:Controller.Switch(20) = 1:PlaySound"rollover":End Sub
Sub sw20_UnHit:Controller.Switch(20) = 0:End Sub
Sub sw21_Hit:Controller.Switch(21) = 1:PlaySound"rollover2":End Sub
Sub sw21_UnHit:Controller.Switch(21) = 0:End Sub
Sub sw22_Hit:Controller.Switch(22) = 1:PlaySound"rollover3":End Sub
Sub sw22_UnHit:Controller.Switch(22) = 0:End Sub
Sub sw23_Hit:Controller.Switch(23) = 1:PlaySound"rollover4":End Sub
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
Sub sw26_Hit:Controller.Switch(26) = 1:PlaySound"rollover5":End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub
Sub sw27_Hit:Controller.Switch(27) = 1:PlaySound"rollover6":End Sub
Sub sw27_UnHit:Controller.Switch(27) = 0:End Sub
Sub sw31_Hit:Controller.Switch(31) = 1:PlaySound"rollover7":End Sub
Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub
Sub sw32_Hit:Controller.Switch(32) = 1:PlaySound"rollover8":End Sub
Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub
Sub sw43_Hit:Controller.Switch(43) = 1:PlaySound"rollover9":End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub
Sub sw44_Hit:Controller.Switch(44) = 1:PlaySound"rollover10":End Sub
Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub
Sub sw46_Hit:Controller.Switch(46) = 1:PlaySound"rollover11":End Sub
Sub sw46_UnHit:Controller.Switch(46) = 0:End Sub

'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw(40) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(41) : playsound SoundFX("fx_bumper2",DOFContactors): End Sub
Sub Bumper3_Hit : vpmTimer.PulseSw(42) : playsound SoundFX("fx_bumper3",DOFContactors): End Sub

'Generic Sounds
Sub Trigger1_Hit: playsound"fx_ballrampdrop" : End Sub 
Sub Trigger2_Hit: playsound"fx_ballrampdrop2" : End Sub
Sub Trigger3_Hit: playsound"fx_ballrampdrop3" : End Sub
Sub Trigger4_Hit: stopsound"plasticroll" :playsound"fx_ballrampdrop" : End Sub
Sub Trigger5_Hit: playsound"wireramp" : End Sub
Sub Trigger6_Hit: playsound"plasticroll" : End Sub
Sub Trigger7_Hit: playsound"plastichit" : End Sub


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
  	'FadeR 1, 'GameOver
   	'FadeR 2, 'Match
   	'FadeR 3, 'Tilt
   	'FadeR 4, 'HS
   	'FadeR 5, 'Ride Again 2x
   	'FadeR 6, 'BIP
   	'FadeR 7, 'Comet Eyes
   	'FadeR 8, 'Comet Eyes
 
   	NFadeLm 9, l9
   	NFadeLm 9, l9a
   	NFadeLm 9, l9z
   	NFadeLm 10, l10
   	NFadeLm 10, l10a
   	NFadeLm 10, l10z
   	NFadeLm 11, l11
   	NFadeLm 11, l11a
   	NFadeLm 11, l11z
   	NFadeLm 12, l12
   	NFadeLm 12, l12a
   	NFadeLm 12, l12z
   	NFadeLm 13, l13
   	NFadeLm 13, l13a
   	NFadeLm 13, l13z
   	NFadeLm 14, l14
   	NFadeLm 14, l14a
   	NFadeLm 14, l14z
   	NFadeLm 15, l15
   	NFadeLm 15, l15a
   	NFadeLm 15, l15z
   	NFadeLm 16, l16
   	NFadeLm 16, l16a
    NFadeLm 16, l16z
   	NFadeLm 17, l17
   	NFadeLm 17, l17a
   	NFadeLm 17, l17z
   	NFadeLm 18, l18
   	NFadeLm 18, l18a
   	NFadeLm 18, l18z
   	NFadeLm 19, l19
   	NFadeLm 19, l19a
   	NFadeLm 19, l19z
   	NFadeLm 20, l20
   	NFadeLm 20, l20a
   	NFadeLm 20, l20z
	Flash 20, f20
   	NFadeLm 21, l21
   	NFadeLm 21, l21a
   	NFadeLm 21, l21z
	Flash 21, f21
   	NFadeLm 22, l22
   	NFadeLm 22, l22a
   	NFadeLm 22, l22z
	Flash 22, f22
   	NFadeLm 23, l23
   	NFadeLm 23, l23a
   	NFadeLm 23, l23z
	Flash 23, f23
   	NFadeLm 24, l24
   	NFadeLm 24, l24a
   	NFadeLm 24, l24z 
   	NFadeLm 25, l25
   	NFadeLm 25, l25a
   	NFadeLm 25, l25z
   	NFadeLm 26, l26
   	NFadeLm 26, l26a
   	NFadeLm 26, l26z
   	NFadeLm 27, l27
   	NFadeLm 27, l27a
   	NFadeLm 27, l27z
   	NFadeLm 28, l28
   	NFadeLm 28, l28a
   	NFadeLm 28, l28z
   	NFadeLm 29, l29
   	NFadeLm 29, l29a
   	NFadeLm 29, l29z
   	NFadeLm 30, l30
   	NFadeLm 30, l30a
   	NFadeLm 30, l30z
   	NFadeLm 31, l31
   	NFadeLm 31, l31a
   	NFadeLm 31, l31z
   	NFadeLm 32, l32
   	NFadeLm 32, l32a
    NFadeLm 32, l32z
   	NFadeLm 33, l33
   	NFadeLm 33, l33a
   	NFadeLm 33, l33z
   	NFadeLm 34, l34
   	NFadeLm 34, l34a
   	NFadeLm 34, l34z
   	NFadeLm 35, l35
   	NFadeLm 35, l35a
   	NFadeLm 35, l35z
   	NFadeLm 36, l36
   	NFadeLm 36, l36a
   	NFadeLm 36, l36z
   	NFadeLm 37, l37
   	NFadeLm 37, l37a
   	NFadeLm 37, l37z
   	NFadeLm 38, l38
   	NFadeLm 38, l38a
   	NFadeLm 38, l38z
   	NFadeLm 39, l39
   	NFadeLm 39, l39a
   	NFadeLm 39, l39z
   	NFadeLm 40, l40
   	NFadeLm 40, l40a
   	NFadeLm 40, l40z
   	NFadeLm 41, l41
   	NFadeLm 41, l41a
   	NFadeLm 41, l41z
   	NFadeLm 42, l42
   	NFadeLm 42, l42a
   	NFadeLm 42, l42z
   	NFadeLm 43, l43
   	NFadeLm 43, l43a
   	NFadeLm 43, l43z
   	NFadeLm 44, l44
   	NFadeLm 44, l44a
   	NFadeLm 44, l44z
   	NFadeLm 45, l45
   	NFadeLm 45, l45a
   	NFadeLm 45, l45z
   	NFadeLm 46, l46
   	NFadeLm 46, l46a
   	NFadeLm 46, l46z
   	NFadeLm 47, l47
   	NFadeLm 47, l47a
   	NFadeLm 47, l47z
   	NFadeLm 48, l48
   	NFadeLm 48, l48a
    NFadeLm 48, l48z
   	NFadeLm 49, l49
   	NFadeLm 49, l49a
   	NFadeLm 49, l49z
   	NFadeLm 50, l50
   	NFadeLm 50, l50a
   	NFadeLm 50, l50z
   	NFadeLm 51, l51
   	NFadeLm 51, l51a
   	NFadeLm 51, l51z
   	NFadeLm 52, l52
   	NFadeLm 52, l52a
   	NFadeLm 52, l52z
   	NFadeLm 53, l53
   	NFadeLm 53, l53a
   	NFadeLm 53, l53z
   	NFadeLm 54, l54
   	NFadeLm 54, l54a
   	NFadeLm 54, l54z
   	NFadeLm 55, l55
   	NFadeLm 55, l55a
   	NFadeLm 55, l55z
   	NFadeLm 56, l56
   	NFadeLm 56, l56a
    NFadeLm 56, l56z
	NFadeLm 57, l57
	
   	NFadeLm 60, l60
   	NFadeLm 60, l60a
   	NFadeLm 60, l60z
   	NFadeLm 61, l61
   	NFadeLm 61, l61a
   	NFadeLm 61, l61z
   	NFadeLm 62, l62
   	NFadeLm 62, l62a
   	NFadeLm 62, l62z
   	NFadeLm 63, l63
   	NFadeLm 63, l63a
   	NFadeLm 63, l63z
   	NFadeLm 64, l64
   	NFadeLm 64, l64a
   	NFadeLm 64, l64z

 	'Solenoid Controlled Flashers
	
	Flashm 104, f104
	NFadeLm 104, f104c
	NFadeLm 104, f104d
	Nfadelm 104, i104dummy

 	Flashm 105, f105
 	Flashm 105, f105a
	NFadeLm 105, f105c
	NFadeLm 105, f105d
	Nfadelm 105, i105dummy

	If GIIsOff=false Then
		corkscrew_prim.image = "corkscrew_gi"
		CorkscrewTube_prim.image = "Flashtube_gi"
		woodguides_prim.image = "woodguides_gi"
		cometbrackets_prim.image = "brackets_gi"
		plasticsedges_prim.image="plasticedgesGIOn"
		millionbackglass_prim.image = "millionbackglass_gi"
		metalrails_prim.image = "metals_gi"
		millionplastic_prim.image = "millionplastic_GI"
		millionbackbox_prim.image = "millionbackbox_gi"
		outerwalls_prim.image = "outer_gi"
		Flasher1.visible=1
		Flasher2.visible=0
		Flasher3.visible=0
		Flasher4.visible=0
		If l57.state = 1 Then
'			FadeObjm 150, millionbackglass_prim, "millionbackglass_rightflash", "millionbackglass_rightflash", "millionbackglass_rightflash", "millionbackglass_gi"
			FadeObjm 150, millionbackglass_prim, "millionbackglass_flash", "millionbackglass_flash", "millionbackglass_flash", "millionbackglass_gi"
			FadeObjm 151, metalrails_prim, "metals_gi_flash", "metals_gi_flash", "metals_gi_flash", "metals_gi"
			FadeObjm 152, millionplastic_prim, "millionplastic_gi_flash", "millionplastic_gi_flash", "millionplastic_gi_flash", "millionplastic_GI"
			FadeObjm 153, millionbackbox_prim, "millionbackbox_gi_flash", "millionbackbox_gi_flash", "millionbackbox_gi_flash", "millionbackbox_gi"
			FadeObjm 154, metalrails_prim, "metals_gi_flash", "metals_gi_flash", "metals_gi_flash", "metals_gi"
			FadeObjm 155, outerwalls_prim, "outer_gi_flash", "outer_gi_flash", "outer_gi_flash", "outer_gi"
			Flasher1.visible=1
			Flasher2.visible=0
			Flasher3.visible=0
			Flasher4.visible=0
		End If
	Else
		corkscrew_prim.image = "corkscrew_off"
		CorkscrewTube_prim.image = "flashtube_off"
		woodguides_prim.image =  "woodguides_gioff"
		cometbrackets_prim.image = "brackets_gioff"
		plasticsedges_prim.image="plasticedgesGIOff"
		millionbackglass_prim.image = "millionbackglass_gioff"
		metalrails_prim.image = "metals_gioff"
		millionplastic_prim.image = "millionplastic_GIoff"
		millionbackbox_prim.image = "millionbackbox_gi"
		outerwalls_prim.image = "outer_gioff"
		Flasher1.visible=0
		Flasher2.visible=0
		Flasher3.visible=1
		Flasher4.visible=0
		end if

		If GIIsOff=false and i105dummy.state = 1 and l57.state = 1 Then
			FadeObjm 161, millionbackglass_prim, "millionbackglass_flash", "millionbackglass_flash", "millionbackglass_flash", "millionbackglass_gi"
			FadeObjm 162, metalrails_prim, "metals_gi_flash", "metals_gi_flash", "metals_gi_flash","metals_gi"
			FadeObjm 163, millionplastic_prim, "millionplastic_GI_flash", "millionplastic_GI_flash", "millionplastic_GI_flash", "millionplastic_GI"
			FadeObjm 164, millionbackbox_prim, "millionbackbox_gi_flash", "millionbackbox_gi_flash", "millionbackbox_gi_flash", "millionbackbox_gi"
			Flasher1.visible=1
			Flasher2.visible=0
			Flasher3.visible=0
			Flasher4.visible=0
		End If

		If GIIsOff=false and i105dummy.state = 1 and l57.state = 0 Then
			FadeObjm 165, millionbackglass_prim, "millionbackglass_rightflash", "millionbackglass_rightflash", "millionbackglass_rightflash", "millionbackglass_gi"
			FadeObjm 166, metalrails_prim, "metals_gi_flashright", "metals_gi_flashright", "metals_gi_flashright", "metals_gi"
			FadeObjm 167, millionplastic_prim, "millionplastic_GI_flashright", "millionplastic_GI_flashright", "millionplastic_GI_flashright", "millionplastic_GI"
			FadeObjm 168, millionbackbox_prim, "millionbackbox_gioff_flashright", "millionbackbox_gioff_flashright", "millionbackbox_gioff_flashright", "millionbackbox_gi"
			Flasher1.visible=1
			Flasher2.visible=0
			Flasher3.visible=0
			Flasher4.visible=0
		End If

		If GIIsoff = false and i105dummy.state = 1 Then	
			FadeObjm 169, outerwalls_prim, "outer_gi_flashright", "outer_gi_flashright", "outer_gi_flashright", "outer_gi"
			FadeObjm 170, cometbrackets_prim, "brackets_gi_flashright", "brackets_gi_flashright", "brackets_gi_flashright", "brackets_gi"
			FadeObjm 188, plasticsedges_prim, "plasticedgesRFlash", "plasticedgesRFlash", "plasticedgesRFlash", "plasticedgesGIOn"
			Flasher1.visible=1
			Flasher2.visible=0
			Flasher3.visible=0
			Flasher4.visible=0
		End If

		If GIIsoff = True and i105dummy.state = 1 and l57.state = 0 Then
			FadeObjm 171, millionbackglass_prim, "millionbackglass_rightflash", "millionbackglass_rightflash", "millionbackglass_rightflash", "millionbackglass_gioff"
			FadeObjm 172, metalrails_prim, "metals_gioff_flashright", "metals_gioff_flashright", "metals_gioff_flashright", "metals_gioff"
			FadeObjm 173, millionplastic_prim, "millionplastic_GIoff_flashright", "millionplastic_GIoff_flashright", "millionplastic_GIoff_flashright", "millionplastic_GIoff"
			FadeObjm 174, millionbackbox_prim, "millionbackbox_gioff_flashright", "millionbackbox_gioff_flashright", "millionbackbox_gioff_flashright", "millionbackbox_gi"
		Flasher1.visible=0
		Flasher2.visible=0
		Flasher3.visible=0
		Flasher4.visible=1
		End If

		If GIIsoff = True and i105dummy.state = 1 Then	
			FadeObjm 175, outerwalls_prim, "outer_gioff_flashright", "outer_gioff_flashright", "outer_gioff_flashright", "outer_gioff"
			FadeObjm 176, cometbrackets_prim, "brackets_gioff_flashright", "brackets_gioff_flashright", "brackets_gioff_flashright", "brackets_gioff"
			FadeObjm 187, plasticsedges_prim, "plasticedgesGIOFFRFlash", "plasticedgesGIOFFRFlash", "plasticedgesGIOFFRFlash", "plasticedgesGIOFF"
		Flasher1.visible=0
		Flasher2.visible=0
		Flasher3.visible=0
		Flasher4.visible=1
		End If

		If GIIsOff=false and i105dummy.state = 1 and l57.state = 1 Then
			FadeObjm 177, millionbackglass_prim, "millionbackglass_flash", "millionbackglass_flash", "millionbackglass_flash", "millionbackglass_gioff"
			FadeObjm 178, metalrails_prim, "metals_gioff_flash", "metals_gioff_flash", "metals_gioff_flash", "metals_gioff"
			FadeObjm 179, millionplastic_prim, "millionplastic_GIoff_flash", "millionplastic_GIoff_flash", "millionplastic_GIoff_flash", "millionplastic_GIoff"
			FadeObjm 180, millionbackbox_prim, "millionbackbox_gioff_flash", "millionbackbox_gioff_flash", "millionbackbox_gioff_flash", "millionbackbox_gi"
			Flasher1.visible=1
			Flasher2.visible=0
			Flasher3.visible=0
			Flasher4.visible=0
		End If

		If GIIsOff=false and i104dummy.state = 1 Then
			FadeObjm 181, corkscrew_prim, "corkscrew_flash", "corkscrew_flash", "corkscrew_flash", "corkscrew_gi"
			FadeObjm 182, CorkscrewTube_prim, "flashtube_flash", "flashtube_flash", "flashtube_flash", "Flashtube_gi"
			FadeObjm 183, outerwalls_prim, "outer_gi_flashleft", "outer_gi_flashleft", "outer_gi_flashleft", "outer_gi"
			FadeObjm 184, metalrails_prim, "metals_gi_flashleft", "metals_gi_flashleft", "metals_gi_flashleft", "metals_gi"
			FadeObjm 185, cometbrackets_prim, "brackets_gi_flashleft", "brackets_gi_flashleft", "brackets_gi_flashleft", "brackets_gi"
			FadeObjm 186, plasticsedges_prim, "plasticedgesLFlash", "plasticedgesLFlash", "plasticedgesLFlash", "plasticedgesGIOn"
		Flasher1.visible=0
		Flasher2.visible=1
		Flasher3.visible=0
		Flasher4.visible=0
		End If
	
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

Sub FadePrim(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:FadingLevel(nr) = 0
        Case 3:pri.image = c:FadingLevel(nr) = 2
        Case 4:pri.image = b:FadingLevel(nr) = 3
        Case 5:pri.image = a:FadingLevel(nr) = 1
    End Select
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
'**********************************************************************************************************



' *********************************************************************
' *********************************************************************

					'Start of VPX call back Functions

' *********************************************************************
' *********************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 48
    PlaySound SoundFX("right_slingshot",DOFContactors), 0, 1, 0.05, 0.05
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
	vpmTimer.PulseSw 47
    PlaySound SoundFX("left_slingshot",DOFContactors),0,1,-0.05,0.05
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



' *********************************************************************
'                      Supporting Ball & Sound Functions
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

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 10 ' total number of balls
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

'*****************************************
'	Ball Shadow
'*****************************************

Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5,BallShadow6, BallShadow7, BallShadow8, BallShadow9, BallShadow10)


Sub BallShadowUpdate()
    Dim BOT, b, shadowZ
    BOT = GetBalls

	' render the shadow for each ball
    For b = 0 to UBound(BOT)
		If BOT(b).X < Table1.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 10
		Else
			BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 10
		End If

		If BOT(b).X > 875 AND BOT(b).Y > 935 Then shadowZ = BOT(b).Z : BallShadow(b).X = BOT(b).X Else shadowZ = 1

			BallShadow(b).Y = BOT(b).Y + 20
			BallShadow(b).Z = shadowZ
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


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

' Start Miami Vice music 
    MusicOn

Sub MusicOn
    Dim x
    x = INT(8 * RND(8) )
    Select Case x
       Case 0:PlayMusic "Miami Vice Theme.mp3"
       Case 1:PlayMusic "Miami Vice Original Theme.mp3"
       Case 2:PlayMusic "Miami Vice Crocketts Theme.mp3"
       Case 3:PlayMusic "Miami Vice Chase.mp3"
       Case 4:PlayMusic "Miami Vice Evan.mp3"
       Case 5:PlayMusic "Miami Vice Last Flight.mp3"
	   Case 6:PlayMusic "Miami Vice One Way Out.mp3"
	   Case 7:PlayMusic "Miami Vice Payback.mp3"
    End Select
 End Sub
 
 Sub Table1_MusicDone()
    MusicOn
 End Sub


Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub