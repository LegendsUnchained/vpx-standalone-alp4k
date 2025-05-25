Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01560000", "Sega.VBS", 3.02

Const cGameName="simpprty",UseSolenoids=2,UseLamps=0,UseGI=0,UseSync=1,HandleMech=1, SCoin="coin"

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

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
 SolCallBack(1) = "SolRelease"
 SolCallBack(2) = "Auto_Plunger"
 SolCallback(3) = "CouchExit"'"vlLock.SolExit"
 SolCallback(4) = "dtDrop.SolDropUp"
 SolCallback(5) = "bsBR.SolOut"
 SolCallback(6) = "bsVUK.SolOut"
 SolCallback(7) = "SolTVRelease"
 SolCallback(8) = "SolHomer"

 SolCallback(12) = "SolUPFLeftFlipper"
 SolCallback(13) = "SolUPFRightFlipper"
 SolCallback(14) = "SolTopRightFlipper"

 SolCallBack(19) = "bsTR.SolOut"
 SolCallBack(20) =  "GarageUp"'"dtGarage.SolDropUp"
 SolCallBack(21) = "SetLamp 121," 'Dome Flasher
 SolCallBack(22) = "SetLamp 122," 'Dome Flasher
 SolCallBack(23) = "SetLamp 123," 'Dome Flasher

 SolCallBack(25) = "SetLamp 125," 'Right Middle plastic
 SolCallBack(26) = "SetLamp 126," 'Right Middle plastic
 SolCallBack(27) = "SetLamp 127," 'Homer Head
 SolCallBack(28) = "SetLamp 128," 'Dome Flasher
 SolCallBack(29) = "SetLamp 129,"  'Comic Book Guy
 SolCallBack(30) = "SolDropBankTrips"
 SolCallBack(31) = "SetLamp 131," 'Dome Flasher
 SolCallBack(32) = "SetLamp 132," 'Dome Flasher


'Flippers
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd:TopLeftFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart:TopLeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd:RightFlipper2.RotateToEnd:TopRightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart:RightFlipper2.RotateToStart:TopRightFlipper.RotateToStart
     End If
End Sub

Sub SolTopRightFlipper(Enabled)
	Dim tmp
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors)
		RightFlipper2.RotateToEnd
	 Else
		'PlaySound SoundFX("fx_Flipperdown",DOFContactors)
		RightFlipper2.RotateToStart
	End If
End Sub

Sub SolUPFLeftFlipper(enabled)
	Dim tmp
	 If Enabled Then
         'PlaySound SoundFX("fx_Flipperup",DOFContactors)
		TopLeftFlipper.RotateToEnd
	 Else
		'PlaySound SoundFX("fx_Flipperdown",DOFContactors)
		TopLeftFlipper.RotateToStart
	 End If
End Sub

Sub SolUPFRightFlipper(enabled)
	Dim tmp
	 If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors)
		TopRightFlipper.RotateToEnd
	 Else
		'PlaySound SoundFX("fx_Flipperdown",DOFContactors)
		TopRightFlipper.RotateToStart
	 End If
End Sub

'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

Sub SolRelease(Enabled)
	If Enabled Then
	bsTrough.ExitSol_On
	vpmTimer.PulseSw 15
	End If
End Sub
 
Sub Auto_Plunger(Enabled)
	If Enabled Then
	PlungerIM.AutoFire
	End If
End Sub

set GICallback = GetRef("UpdateGI")
Sub UpdateGI(no, Enabled)
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 1:	Next
Table1.colorgradeimage = "ColorGradeLUT256x16_extraConSat"
        Shadows.opacity = 20
        Backwall.opacity = 50
        PlaySound "fx_relay"
		DOF 101, DOFOn
	Else For each xx in GI:xx.State = 0: Next
Table1.colorgradeimage = "ColorGrade_4"
        PlaySound "fx_relay"
		DOF 101, DOFOff
        Shadows.opacity = 50
        Backwall.opacity = 20
	End If
End Sub

 'DROPBANK HANDLE
 Sub SolDropBankTrips(Enabled)
	If Enabled Then
		dtDrop.Hit 1
		dtDrop.Hit 2
		dtDrop.Hit 3
	End If
 End Sub

 'TV LOCK HANDLE
 Sub SolTVRelease(Enabled):
	If Enabled Then
		TopPost.IsDropped = 1:
		playsound SoundFX("Solenoid",DOFContactors)
	Else
		TopPost.IsDropped = 0
	End If
 End Sub


'GARAGE DOOR
Dim DoorStatus

Sub GarageUp(Enabled)
	If Enabled Then
		DoorStatus = 1
		GDoorT.enabled = 1
		playsound "Diverter" 
	Else
		DoorStatus = 0
		GDoorT.enabled = 1
	End If
End Sub

Sub GdoorT_Timer
	If DoorStatus = 1 Then
		If Gdoor.RotX < 60 Then
			Gdoor.RotX = Gdoor.RotX +4
		Else
			sw48.isdropped = 1
			GDoorT.Enabled = 0
		End If
	End If
	If DoorStatus = 0 Then
		If Gdoor.RotX > 0 Then
			Gdoor.RotX = Gdoor.RotX -4
		Else
			sw48.isdropped = 0
			GDoorT.Enabled = 0
		End If
	End If

End Sub
	
  'COUCH LOCK	
Sub CouchExit(enabled)
	If Enabled Then
		CouchDrop.Enabled = 1
	Else
		couchdrop.Enabled = 0
		DropCheck.Enabled = 1
	End If
End Sub

Sub DropCheck_Timer
	Drop1.isdropped = 1
	DropCheck.enabled = 0
End Sub

Sub CouchDrop_Hit:Controller.Switch(38) = 0:End Sub

'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsBR, bsTR, bsVuk, dtDrop, capBall

Set LampCallback = GetRef("UpdateLeds") 'Color TV

Sub Table1_Init
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "The Simpsons Pinball Party (Stern 2003)"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.Hidden = 0
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

    vpmNudge.TiltSwitch=swTilt
    vpmNudge.Sensitivity=3
    vpmNudge.TiltObj=Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

 Set bsTrough = New cvpmBallStack
	 bsTrough.InitSw 0, 14, 13, 12, 11, 10, 0, 0
	 bsTrough.InitKick BallRelease, 45, 9
	 bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
	 bsTrough.Balls = 5

 Set bsBR = New cvpmBallStack
	 bsBR.InitSaucer sw20, 20, 232, 16
	 bsBR.KickForceVar = 2.5
	 bsBR.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

 Set bsTR = New cvpmBallStack
	 bsTR.InitSaucer sw24, 24, 100, 1
	 bsTR.KickForceVar = 2
	 bsTR.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

 Set bsVuk = New cvpmBallStack
	 bsVuk.InitSw 0, 55, 0, 0, 0, 0, 0, 0
	 bsVuk.InitKick VukOut, 180, 0
	 bsVuk.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

 Set dtDrop = New cvpmDropTarget
	 dtDrop.InitDrop Array(sw17, sw18, sw19), Array(17, 18, 19)
	 dtDrop.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

 set capBall = CapKicker.CreateBall
	CapKicker.kick 0,0

	Drop1.isdropped = 1
	Drop2.isdropped = 1

 End sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
sub Table1_Exit:Controller.Stop:end sub

 
'*****************************************************************
'***********   TV LED Init ***************************************
'*****************************************************************
Dim x
		For each x in LEDR:x.visible = 0:next
		For each x in LEDG:x.visible = 0:next
		For each x in LEDY:x.visible = 0:next

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************
Sub Table1_KeyDown(ByVal KeyCode)
	
	If keycode = StartGameKey Then 
	Controller.Switch(54) = 1
	End If
	If Keycode = AddCreditKey Then 
	Controller.Switch(6) = 1
	End If	
	If KeyCode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
	If KeyDownHandler(KeyCode) Then Exit Sub

End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	
	If keycode = StartGameKey Then 
	Controller.Switch(54) = 0
	Exit Sub
End If
	If Keycode = AddCreditKey Then 
	Controller.Switch(6) = 0
	Exit Sub
End If	
	If KeyCode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
	If KeyUpHandler(KeyCode) Then Exit Sub
	End Sub
 
' IMPULSE PLUNGER
Dim plungerIM
 Const IMPowerSetting = 75 ' Plunger Power
 Const IMTime = 0.7        ' Time in seconds for Full Plunge
Set plungerIM = New cvpmImpulseP
	plungerIM.InitImpulseP swPlunger, IMPowerSetting, IMTime
	plungerIM.Random 0.3
	plungerIM.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
	plungerIM.CreateEvents "plungerIM"

'**********************************************************************************************************

'DRAIN and Kickers
 Sub Drain_Hit:bsTrough.AddBall Me : Playsound "drain" : End Sub
 Sub sw20_Hit:bsBR.AddBall 0 : playsound "popper_ball" : End Sub
 Sub sw24_Hit:bsTR.AddBall 0 : playsound "popper_ball" : End Sub
 Sub sw55_Hit:bsVuk.AddBall Me : playsound "popper_ball" : End Sub
 
'Drop Targets
 Sub sw17_Dropped:dtDrop.Hit 1 :End Sub  
 Sub sw18_Dropped:dtDrop.Hit 2 :End Sub  
 Sub sw19_Dropped:dtDrop.Hit 3 :End Sub

'Wire Triggers
 Sub sw16_Hit:Controller.Switch(16) = 1 : playsound"rollover" : End Sub
 Sub sw16_Unhit:Controller.Switch(16) = 0:End Sub
 Sub sw25_Hit:Controller.Switch(25) = 1 : playsound"rollover" : End Sub
 Sub sw25_Unhit:Controller.Switch(25) = 0:End Sub
 Sub sw26_Hit:Controller.Switch(26) = 1 : playsound"rollover" : End Sub
 Sub sw26_Unhit:Controller.Switch(26) = 0:End Sub
 Sub sw29_Hit:Controller.Switch(29) = 1 : playsound"rollover" : End Sub
 Sub sw29_Unhit:Controller.Switch(29) = 0:End Sub
 Sub sw37_Hit:Controller.Switch(37) = 1 : playsound"rollover" : End Sub
 Sub sw37_Unhit:Controller.Switch(37) = 0::End Sub
 Sub sw44_Hit:Controller.Switch(44) = 1 : playsound"rollover" : End Sub
 Sub sw44_Unhit:Controller.Switch(44) = 0:End Sub
 Sub sw57_Hit:Controller.Switch(57) = 1 : playsound"rollover" : End Sub
 Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
 Sub sw58_Hit:Controller.Switch(58) = 1 : playsound"rollover" : End Sub
 Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
 Sub sw61_Hit:Controller.Switch(61) = 1 : playsound"rollover" : End Sub
 Sub sw61_UnHit:Controller.Switch(61) = 0:End Sub
 Sub sw60_Hit:Controller.Switch(60) = 1 : playsound"rollover" : End Sub
 Sub sw60_UnHit:Controller.Switch(60) = 0:End Sub
 Sub sw63_Hit:Controller.Switch(63) = 1 : playsound"rollover" : End Sub
 Sub sw63_Unhit:Controller.Switch(63) = 0:End Sub
 Sub sw64_Hit:Controller.Switch(64) = 1 : playsound"rollover" : End Sub
 Sub sw64_Unhit:Controller.Switch(64) = 0:End Sub
 
'Gate Triggers
 Sub sw36_Hit:vpmTimer.PulseSw 36:End Sub
 Sub sw45_Hit:vpmTimer.PulseSw 45:End Sub
 Sub sw46_Hit:vpmTimer.PulseSw 46:End Sub
 Sub sw47_Hit:vpmTimer.PulseSw 47:End Sub

'Garage Door
 Sub sw48_Hit:vpmTimer.PulseSW 48 : Playsound "metalhit_thin" : End Sub'

'Stand Up Targets
 Sub sw9_Hit:vpmTimer.PulseSw 9:End Sub
 Sub sw31_Hit:vpmTimer.PulseSw 31:End Sub
 Sub sw32_Hit:vpmTimer.PulseSw 32:End Sub
 Sub sw30_Hit:vpmTimer.PulseSw 30:End Sub
 Sub sw33_Hit:vpmTimer.PulseSw 33:End Sub
 Sub sw34_Hit:vpmTimer.PulseSw 34:End Sub
 Sub sw35_Hit:vpmTimer.PulseSw 35:End Sub
 Sub sw38_Hit:Controller.Switch(38) = 1:Drop1.isDropped = 0 : End Sub
 Sub sw38_Unhit:Controller.Switch(38) = 0:End Sub
 Sub sw39_Hit:Controller.Switch(39) = 1:Drop2.isdropped = 0 : End Sub
 Sub sw39_Unhit:Controller.Switch(39) = 0:Drop2.isDropped = 1:End Sub
 Sub sw40_Hit:Controller.Switch(40) = 1:End Sub
 Sub sw40_Unhit:Controller.Switch(40) = 0:End Sub 

 Sub sw41_Hit:vpmTimer.PulseSw 41:End Sub
 Sub sw42_Hit:vpmTimer.PulseSw 42:End Sub
 Sub sw43_Hit:vpmTimer.PulseSw 43:End Sub
 Sub sw52_Hit:vpmTimer.PulseSw 52:End Sub
 
'SPINNERS
 Sub sw21_Spin():vpmTimer.PulseSw 21 : playsound"fx_spinner" : End Sub

'BART
 Sub sw22_Hit:Controller.Switch(22) = 1 : playsound"rollover" : End Sub
 Sub sw22_unHit: Controller.Switch(22) = 0: End Sub
 Sub sw23_Hit:Controller.Switch(23) = 1:End Sub
 Sub sw23_unHit: Controller.Switch(23) = 0: Bart.enabled = True : End Sub

 Sub Bart_Timer
bart1.y=bart1.y-1.5
bart1.x=bart1.x+0.5
board1.y=board1.y-1.5
board1.x=board1.x+0.5
If board1.y < 620 Then
bart1.y=729
bart1.x=748
board1.y=711
board1.x=752
Bart.enabled = False
End If
 End Sub

'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw 50 : BR1.transz = -20 : BCap1.transz = -20  : Me.TimerEnabled = 1 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper1_Timer : BR1.transz = 0 : BCap1.transz = 0 : Me.TimerEnabled = 0 : End Sub

Sub Bumper2_Hit : vpmTimer.PulseSw 51 : BR2.transz = -20 : BCap2.transz = -20  : Me.TimerEnabled = 1 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper2_Timer : BR2.transz = 0 : BCap2.transz = 0 : Me.TimerEnabled = 0 : End Sub

Sub Bumper3_Hit:vpmTimer.PulseSw 49 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub

'Generic Ramp Sounds
Sub Trigger1_Hit : PlaySound "fx_ballrampdrop":End Sub
Sub Trigger3_Hit : PlaySound "fx_ballrampdrop":End Sub


Sub Trigger2_Hit : PlaySound "Wire Ramp":End Sub
Sub Trigger4_Hit : PlaySound "Wire Ramp":End Sub
Sub Trigger5_Hit : PlaySound "Wire Ramp":End Sub
Sub Trigger6_Hit : PlaySound "Wire Ramp":End Sub

'************************************************************
'******************** HOMERs HEAD ***************************
'************************************************************
  Dim HeadPos, HeadDir,HeadState, HeadRef, Homeractive
	HeadPos = 0:HeadDir = 5:HeadState = 0:HeadRef = 0

  Sub CheckLamps()
	Dim RR, RO, LO
	'HeadRef determines which image is displayed
	If F22.State = 1 then: RO = 1:else: RO = 0:end if
	If F23.State = 1 then: RR = 1:else: RR = 0:end if
	If F32.State = 1 then: LO = 1:else: LO = 0:end if
	If RR = 1 then HeadRef = 1
	If RO = 1 then HeadRef = 2
	If LO = 1 then HeadRef = 3
	If RO = 1 and LO = 1 then HeadRef = 4
	If RO = 1 and RR = 1 then HeadRef = 5
	If LO = 1 and RR = 1 then HeadRef = 5
	If RO = 1 and RR = 1 and LO = 1 then HeadRef = 5
	If RO = 0 and RR = 0 and LO = 0 then HeadRef = 0
  End Sub

  Sub SolHomer(Enabled)
 If Enabled Then
 HomerActive = 1:HeadDir = 5:HeadTimer.Enabled = 1
 Else
 HomerActive = 0:HeadDir = -5:HeadTimer.Enabled = 1
 End If
 End Sub

 Sub HeadTimer_Timer()

	If HeadPos > 39 or HeadPos < 6 Then
		HeadPos = HeadPos + HeadDir/5
	Else
		HeadPos = HeadPos + HeadDir
	End If

	If HeadPos > 45 then 
		HeadPos = 45:HeadTimer.Enabled = 0
	end if
	If HeadPos < 0 then
		HeadPos = 0:HeadTimer.Enabled = 0
	end if
	UpdateHead
  End Sub

Sub UpdateHead()
	CheckLamps
	Select case HeadRef
		case 0 : 'no flasher reflections
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hh"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
		case 1 : 'RRFlasher
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hh"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
		case 2 : 'RO Flasher
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hh"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
		case 3 : 'LO flasher
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hh"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
		case 4 : 'Both Orange
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hh"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
		case 5 : 'Orange and Red
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hh"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
	end Select
End Sub
 
 Sub Homer_Hit
 If HomerActive Then
 If ActiveBall.VelX <0 Then
 HomerOff.Enabled = 0:HomerOn.Enabled = 1
 Else
 HomerOn.Enabled = 0:HomerOff.Enabled = 1
 End If
 End If
 End Sub
 
 Sub Homer3_Hit
 If HomerActive Then
 HomerOn.Enabled = 0:HomerOff.Enabled = 1
 End If
 End Sub
 
 Sub Homer2_Hit
 If HomerActive Then
 HomerOff.Enabled = 0:HomerOn.Enabled = 1
 End If
 End Sub
 
 Sub HomerOn_Timer
' Select Case HomerState
' Case 0:HomerState = 1:HomeRm 127, homerhead, HomerState
' Case 1:HomerState = 2:HomeRm 127, homerhead, HomerState
' Case 2:HomerState = 3:HomeRm 127, homerhead, HomerState:Me.Enabled = 0
' End Select
 End Sub
 
 Sub HomerOff_Timer
' Select Case HomerState
' Case 3:HomerState = 2:HomeRm 127, homerhead, HomerState
' Case 2:HomerState = 1:HomeRm 127, homerhead, HomerState
' Case 1:HomerState = 0:HomeRm 127, homerhead, HomerState:Me.Enabled = 0
' End Select
 End Sub
 
 Sub homer4_Hit
' HomerReset:HomerHead(0).Visible = 1
 End sub



 
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
 NFadeL 1, l1
 NFadeL 2, l2
 NFadeL 3, l3
 NFadeL 4, l4
 NFadeL 5, l5
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
 NFadeL 17, l17 'Left Bumper
 NFadeL 18, l18a 'Right Bumper Tower
 NFadeL 19, l19a 'Lower Bumper Tower
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
' NFadeL 31, l31 'start button light

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
 NFadeL 64, l64
 NFadeL 65, l65
 NFadeL 66, l66
 NFadeL 67, l67
 NFadeL 68, l68
 NFadeL 69, l69
 NFadeL 70, l70

 NFadeObj 73, l73, "MoesGreen_OFF", "MoesGreen_ON"  'Moes Sign
 NFadeObj 74, l74, "MoesGreen_OFF", "MoesGreen_ON"  'Moes Sign
 NFadeObj 75, l75, "MoesGreen_OFF", "MoesGreen_ON"  'Moes Sign
 NFadeObj 76, l76, "MoesGreen_OFF", "MoesGreen_ON"  'Moes Sign
 NFadeObj 77, l77, "MoesGreen_OFF", "MoesGreen_ON"  'Moes Sign
 NFadeObj 78, l78, "MoesGreen_OFF", "MoesGreen_ON"  'Moes Sign
 NFadeObj 79, l79, "MoesGreen_OFF", "MoesGreen_ON"  'Moes Sign
 NFadeObj 80, l80, "MoesRed_OFF", "MoesRed_ON"  'Moes Sign

'Solenoid Controlled
' NFadeObjm 121, f121, "dome2_0_orangeON", "dome2_0_orange"
 NFadeLm 121, f21
NFadeL 121, f21a
' NFadeObjm 122, f122, "dome2_0_redON", "dome2_0_red"
 NFadeLm 122, f22
 NFadeL 122, f22a
' NFadeObjm 123, f123, "dome2_0_orangeON", "dome2_0_orange"
 NFadeLm 123, f23
NFadeL 123, f23a
 NFadeObjm 125, f125, "dome2_0_clearON", "dome2_0_clear"
 NFadeLm 125, f25
NFadeL 125, f25a
 NFadeL 126, f26

 NFadeLm 127, l127
 NFadeL 127, l127a
if l127.state = 1 then HHead.blenddisablelighting = 0.3
if l127.state = 0 then HHead.blenddisablelighting = 0.1

' NFadeObjm 128, f128, "dome2_0_yellowON", "dome2_0_yellow"
 NFadeLm 128, f28
NFadeL 128, f28a
 NFadeLm 129, f29
 NFadeL 129, f29a
if f29.state = 1 then Primitive111.blenddisablelighting = 0.6
if f29.state = 0 then Primitive111.blenddisablelighting = 0.3
' NFadeObjm 131, f131, "dome2_0_orangeON", "dome2_0_orange"
 NFadeLm 131, f31
NFadeL 131, f31a
' NFadeObjm 132, f132, "dome2_0_redON", "dome2_0_red"
 NFadeLm 132, f32
 NFadeL 132, f32a
 UpdateHead

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

Sub FadeDisableLighting(nr, a, alvl)
	Select Case FadingLevel(nr)
		Case 4
			a.UserValue = a.UserValue - 0.1
			If a.UserValue < 0 Then 
				a.UserValue = 0
				FadingLevel(nr) = 0
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
		Case 5
			a.UserValue = a.UserValue + 0.50
			If a.UserValue > 1 Then 
				a.UserValue = 1
				FadingLevel(nr) = 1
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
	End Select
End Sub

'3 COLOR LED TV-DISPLAY ***********************************************************************************************************************************
 Dim REDL

 REDL = Array(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14,_
    r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29,_
    r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44,_
    r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59,_
    r61, r62, r63, r64, r65, r66, r67, r68, r69, r70, r71, r72, r73, r74,_
    r76, r77, r78, r79, r80, r81, r82, r83, r84, r85, r86, r87, r88, r89,_
    r91, r92, r93, r94, r95, r96, r97, r98, r99, r100, r101, r102, r103, r104,_
    r106, r107, r108, r109, r110, r111, r112, r113, r114, r115, r116, r117, r118, r119,_
    r121, r122, r123, r124, r125, r126, r127, r128, r129, r130, r131, r132, r133, r134,_
    r136, r137, r138, r139, r140, r141, r142, r143, r144, r145, r146, r147, r148, r149)
 
 
Sub UpdateLeds
	Dim ChgLED, ii, jj, num, stat, s, x
		ChgLED = Controller.ChangedLEDs(0, &HFFFFF)
			If Not IsEmpty(ChgLED) Then
				For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0):stat = chgLED(ii, 2)
				for jj = 0 to 6
				x = num * 7 + 6-jj
				s = stat And 3
		Select Case S
			Case 3:
				REDL(x).material = "EVYellow":REDL(x).visible = 1				
			Case 2:
				REDL(x).material = "EVGreen":REDL(x).visible = 1
			Case Else:
				REDL(x).material = "EVRed":REDL(x).visible = 1
			If S = 0 Then REDL(x).visible = 0
		End Select
			stat = (stat And &H3FFC) \ 4
			next
			Next
		End If
 End Sub
'RED LED TV-DISPLAY ***************************************************************************************************************************************

 Dim RLED(20)
 
 RLED(0) = Array(r7, r6, r5, r4, r3, r2, r1)
 RLED(1) = Array(r14, r13, r12, r11, r10, r9, r8)
 RLED(2) = Array(r22, r21, r20, r19, r18, r17, r16)
 RLED(3) = Array(r29, r28, r27, r26, r25, r24, r23)
 RLED(4) = Array(r37, r36, r35, r34, r33, r32, r31)
 RLED(5) = Array(r44, r43, r42, r41, r40, r39, r38)
 RLED(6) = Array(r52, r51, r50, r49, r48, r47, r46)
 RLED(7) = Array(r59, r58, r57, r56, r55, r54, r53)
 RLED(8) = Array(r67, r66, r65, r64, r63, r62, r61)
 RLED(9) = Array(r74, r73, r72, r71, r70, r69, r68)
 RLED(10) = Array(r82, r81, r80, r79, r78, r77, r76)
 RLED(11) = Array(r89, r88, r87, r86, r85, r84, r83)
 RLED(12) = Array(r97, r96, r95, r94, r93, r92, r91)
 RLED(13) = Array(r104, r103, r102, r101, r100, r99, r98)
 RLED(14) = Array(r112, r111, r110, r109, r108, r107, r106)
 RLED(15) = Array(r119, r118, r117, r116, r115, r114, r113)
 RLED(16) = Array(r127, r126, r125, r124, r123, r122, r121)
 RLED(17) = Array(r134, r133, r132, r131, r130, r129, r128)
 RLED(18) = Array(r142, r141, r140, r139, r138, r137, r136)
 RLED(19) = Array(r149, r148, r147, r146, r145, r144, r143)
 
 Sub UpdateRedLeds
Dim ChgLED, ii, jj, chg, num, stat, obj, x
	ChgLED = Controller.ChangedLEDs(0, &HFFFFF)
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(chgLED)
		num = chgLED(ii, 0):chg = chgLED(ii, 1):stat = chgLED(ii, 2)
		For Each obj in RLED(num)
		If chg And 3 Then obj.visible = ABS((stat And 3)> 0)
		chg = chg \ 4:stat = stat \ 4
		next
		Next
		End If
 End Sub
 

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
	vpmTimer.PulseSw 62
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
	vpmTimer.PulseSw 59
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

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

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
    Vol = Csng(BallVel(ball) ^2 / 800)
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

Const tnob = 6 ' total number of balls
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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/7, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/10, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
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
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

'sub FlipperTimer_Timer()
   ' lfs.RotZ = LeftFlipper.CurrentAngle
 ' rfs.RotZ = RightFlipper.CurrentAngle
  '  FlipperRSh1.RotZ = RightFlipper2.currentangle
'End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
'Dim BallShadow
'BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6)

'Sub BallShadowUpdate_timer()
   ' Dim BOT, b
   ' BOT = GetBalls
    ' hide shadow of deleted balls
   ' If UBound(BOT)<(tnob-1) Then
     '   For b = (UBound(BOT) + 1) to (tnob-1)
       '   '  BallShadow(b).visible = 0
       ' Next
    'End If
    ' exit the Sub if no balls on the table
   ' If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
   ' For b = 0 to UBound(BOT)
	'	BallShadow(b).X = BOT(b).X
	'	ballShadow(b).Y = BOT(b).Y + 10                       
      '  If BOT(b).Z > 20 and BOT(b).Z < 200 Then
      '      BallShadow(b).visible = 1
      '  Else
      '      BallShadow(b).visible = 0
     '   End If
'if BOT(b).z > 30 Then 
'ballShadow(b).height = BOT(b).Z - 20
'ballShadow(b).opacity = 80
'Else
'ballShadow(b).height = BOT(b).Z - 24
'ballShadow(b).opacity = 80
'End If
  '  Next	
'End Sub



Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
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

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub
