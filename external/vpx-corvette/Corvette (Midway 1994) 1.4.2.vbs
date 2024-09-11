Option Explicit
Randomize
 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
 
Const BallMass=1.3
Const cGameName="corv_21",UseSolenoids=2,UseLamps=0, SCoin="coin"
Const cSingleLFlip=0
Const cSingleRFlip=0

'Apron.image = "apron chrome"		'enable the apron version, You want
Apron.image = "apron main"
'Apron.image = "apron gold"



' Thalamus - volume of the cars, and engine - higher value equals louder sound - 1 is maximum
Const CarVolume = 0.05
Const EngineVol = 0.1
 
Const UseGI = 1  'Enable Bally Williams GI String Calls
 
LoadVPM "01560000", "WPC.VBS", 3.26
 
Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim EngineTol
 
If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=1
Else
Ramp16.visible=0
Ramp15.visible=0
Primitive13.visible=0
End if
 
'********************
'Solenoids Call backs
'********************
SolCallback(1) = "solTrough"
SolCallback(2) = "SolLGate"
SolCallback(3) = "SolKickback"
SolCallback(4) = "bsL.SolOut"
SolCallback(5) = "SolGateU"
'6 Not Used
SolCallback(8) = "bsR.SolOut"
'SolCallback(9)="vpmSolSound ""Sling"","
'SolCallback(10)="vpmSolSound ""Sling"","
'SolCallback(11)="vpmSolSound ""Jet3"","
'SolCallback(12)="vpmSolSound ""Jet3"","
'SolCallback(13)="vpmSolSound ""Jet3"","
'14 Not Used
SolCallback(15) ="bsLock.SolOut"
SolCallback(16) ="SolTGate"
SolCallback(17) ="SetLamp 117," 'motor controller and Flasher
SolCallback(18) ="SetLamp 118," 'motor controller
SolCallback(19) ="SetLamp 119," 'motor controller
SolCallback(20) ="SetLamp 120," 'Backglass Only
SolCallback(21) ="SetLamp 121,"
SolCallback(22) ="SetLamp 122,"
SolCallback(23) ="SetLamp 123,"
SolCallback(24) ="SetLamp 124,"
SolCallback(25) ="SetLamp 125,"
SolCallback(26) ="SolEngine" 'motor controller and flasher
SolCallback(27) ="SetLamp 127,"
SolCallback(28) ="SetLamp 128,"
 
'SolCallback(33) = "" 'Diverter Power
SolCallback(34) = "SolDiv"
 
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
 
 
Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFFlippers):LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFFlippers):LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
     End If
  End Sub
 
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFFlippers):RightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFFlippers):RightFlipper.RotateToStart
     End If
End Sub 
 
'**********************************************************************************************************
 
'Solenoid Controlled toys
'**********************************************************************************************************
 
Sub solTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		vpmTimer.PulseSw 37
	End If
 End Sub
 
Sub SolLGate(Enabled) 'Lock Gate 1
	If Enabled Then
		Lgate.open = 0
		Wall12.IsDropped=0
        PlaySound SoundFX("Popper",DOFContactors)
	Else
		Lgate.Open = 1
		Wall12.IsDropped=1
     End If
 End Sub
 
Sub SolKickBack(Enabled)
	If Enabled Then
		KickBack.Fire
		PlaySound SoundFX("Popper",DOFContactors)
	Else
		KickBack.PullBack
	End if
End Sub
 
Sub SolGateU(Enabled) 'Lock Gate 2
	If Enabled Then
		GateU.Open = 1
		PlaySound SoundFX("Popper",DOFContactors)
 
	Else
		GateU.Open = 0
	End If
End Sub
 
Sub SolTGate(Enabled)
	If Enabled Then
		TGate.Open = 1
		PlaySound SoundFX("Popper",DOFContactors)
	Else
		TGate.Open = 0
	End if
End Sub
 
Sub SolEngine(Enabled)
		SetLamp 126, Enabled

If EngineTol<100 Then
	Magnet1.Enabled=1
	Magnet2.Enabled=1
	Magnet3.Enabled=1
	Magnet4.Enabled=1
Else
	Magnet1.Enabled=0
	Magnet2.Enabled=0
	Magnet3.Enabled=0
	Magnet4.Enabled=0
End If

    If Enabled then
        EngineP.RotY = EngineP.RotY + 5
' Thalamus
		PlaySoundAtVol "fx_rampbump1", EngineP, EngineVol
    Else
       EngineP.RotY = EngineP.RotY - 5
' Thalamus
		PlaySoundAtVol "fx_rampbump1", EngineP, EngineVol
	End If
End Sub

 
Sub SolDiv(Enabled)
    If Enabled Then
		PlaySound SoundFX("Diverter",DOFContactors)
		DiverterOFF.IsDropped = 1
		DiverterON.IsDropped = 0
		DivFlipper.RotateToEnd
    Else
		PlaySound SoundFX("Diverter",DOFContactors)
		DiverterOFF.IsDropped = 0
		DiverterON.IsDropped = 1
		DivFlipper.RotateToStart
    End If
End Sub
 
 
'*****************************************
'      		General Illumination  for Bally/Williams String GI
'*****************************************
 
Set GiCallBack2 = GetRef("UpdateGi")
 
Sub UpdateGi(nr,step)
	Dim xx
 
	If step > 0 Then
		DOF 101, DOFOn
	Else
		DOF 101, DOFOff
	End If
 
	Select Case nr
	Case 0
		If step=0 Then
			'GI1 OFF
			For each xx in GI1:xx.State = 0: Next
			PlaySound "fx_relay"
		Else
			'GI1 ON
			For each xx in GI1:xx.State = 1: Next
			PlaySound "fx_relay"
		End If
	Case 1
		If step=0 Then
			'GI2 OFF
			For each xx in GI2:xx.State = 0: Next
			PlaySound "fx_relay"
		Else
			'GI2 ON
			For each xx in GI2:xx.State = 1: Next
			PlaySound "fx_relay"
		End If
 
	Case 2
		If step=0 Then
			'GI3 OFF
			For each xx in GI3:xx.State = 0: Next
			PlaySound "fx_relay"
		Else
			'GI3 ON
			For each xx in GI3:xx.State = 1: Next
			PlaySound "fx_relay"
		End If
 
	Case 3
		If step=0 Then
			'GI4 OFF
			For each xx in GI4:xx.State = 0: Next
			PlaySound "fx_relay"
		Else
			'GI4 ON
			For each xx in GI4:xx.State = 1: Next
			PlaySound "fx_relay"
		End If
 
	'Case 4		'GI5  is Backglass Only
 
	End Select
 
End Sub
 
 
 
 
 '************
' Table init.
 '************
Dim bsTrough, bsR, bsL, bsLock, Mag1, Mag2, Mag3, Mag4, ApronImg

ApronImg=1
Wall12.IsDropped=1
 
Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Corvette Bally 1994"&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 0
        .HandleMechanics = 7
        .Games(cGameName).Settings.Value("sound")=1
		'.PuPHide = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
        .Switch(22) = 1 
        .Switch(24) = 1 
     End With
     On Error Goto 0
 
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1
 
    	vpmNudge.TiltSwitch=14
    	vpmNudge.Sensitivity=1
    	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)
 
'**Trough
    Set bsTrough = New cvpmBallStack
    bsTrough.InitSw 0,31,32,33,34,0,0,0
    bsTrough.InitKick BallRelease, 90, 4
    bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
    bsTrough.Balls = 4
 
'**Route 66 Kicker
	Set bsR = New cvpmBallStack
        bsR.InitSw 0, 57, 0, 0, 0, 0, 0, 0
        bsR.InitKick sw57, 270, 24
        bsR.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        bsR.KickForceVar = 3
 
'**Pit Stop
	Set bsL = New cvpmBallStack
        bsL.InitSw 0, 36, 0, 0, 0, 0, 0, 0
        bsL.InitKick sw36, 235, 5
        bsL.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        bsL.KickForceVar = 3
 
'**ZR1 Lock
	Set bsLock=New cvpmBallStack
		bsLock.InitSw 0,76,77,78,0,0,0,0
		bsLock.InitKick Kicker3,235,1
		bsLock.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsLock.Balls=0
 
	Set mag1= New cvpmMagnet
		mag1.InitMagnet Magnet1, 60 
		mag1.GrabCenter = False
 		mag1.solenoid=26
		mag1.CreateEvents "mag1"
 
	Set mag2= New cvpmMagnet
 	With mag2
		.InitMagnet Magnet2, 60 
		.GrabCenter = False
 		.solenoid=26
		.CreateEvents "mag2"
	End With
 
	Set mag3= New cvpmMagnet
 	With mag3
	.InitMagnet Magnet3, 60 
		.GrabCenter = False
 		.solenoid=26
		.CreateEvents "mag3"
	End With
 
	Set mag4= New cvpmMagnet
 	With mag4
		.InitMagnet Magnet4, 60 
		.GrabCenter = False
 		.solenoid=26
		.CreateEvents "mag4"
	End With

 
	DiverterON.IsDropped = 1
    KickBack.PullBack
 
  End Sub

Sub EngTol_Timer
If EngineTol>100 Then
mag1.solenoid=0
mag2.solenoid=0
mag3.solenoid=0
mag4.solenoid=0
else
mag1.solenoid=26
mag2.solenoid=26
mag3.solenoid=26
mag4.solenoid=26
End If
End Sub










 
'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************
 


Sub Table1_KeyDown(ByVal KeyCode)
	If Keycode = StartGameKey Then Controller.Switch(13) = 1 ' Start Button
	If KeyCode = KeyFront Then Controller.Switch(23) = 1 ' Buy In Button
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = RightMagnaSave Then ApronImg=ApronImg+1
	If keycode = LeftMagnaSave Then ApronImg=ApronImg-1
	If keycode = LeftFlipperKey Then EngineTol=0
	If keycode = RightFlipperKey Then EngineTol=0

End Sub
 
Sub Table1_KeyUp(ByVal KeyCode)
 
	If Keycode = StartGameKey Then Controller.Switch(13) = 0
	If KeyCode = KeyFront Then Controller.Switch(23) = 0
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
	If KeyUpHandler(keycode) Then Exit Sub
End Sub
 
'**********************************************************************************************************
 
 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub
 
Sub sw57a_Hit::PlaySound "kicker_enter",0,1,0.1,0.25:bsR.AddBall Me:End Sub
Sub sw57_Hit()::PlaySound "kicker_enter",0,1,0.1,0.25:bsL.AddBall Me:End Sub   
Sub sw36a_Hit: :PlaySound "kicker_enter",0,1,0.1,0.25:bsL.AddBall Me:End Sub  
                                                  '   



Sub Kicker3_Hit():::PlaySound "kicker_enter",0,1,0.1,0.25:bsR.AddBall Me:End Sub
Sub Kicker1_Hit::PlaySound "kicker_enter",0,1,0.1,0.25:bsLock.AddBall Me:End Sub
 
 

Sub ApronTimer_Timer
If ApronImg=1 then Apron.Image="Apron Main"
If ApronImg=2 then Apron.Image="Apron Main-Custom"
If ApronImg=3 then Apron.Image="Apron Chrome"
If ApronImg=4 then Apron.Image="Apron Gold"
If ApronImg>4 then Apron.Image="Apron Main":ApronImg=1
If ApronImg<1 then Apron.Image="Apron Gold":ApronImg=4
End Sub

 
'**********************************
'Switches
'**********************************
'Wire Triggers
  Sub sw11_Hit:Controller.Switch(11) = 1:PlaySound "rollover":End Sub
  Sub sw11_UnHit:Controller.Switch(11) = 0:End Sub
  Sub sw12_Hit:Controller.Switch(12) = 1:PlaySound "rollover":End Sub
  Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub
  Sub sw15_Hit:Controller.Switch(15) = 1:PlaySound "rollover":End Sub
  Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub
  Sub sw16_Hit:Controller.Switch(16) = 1:PlaySound "rollover":End Sub
  Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub
  Sub sw17_Hit:Controller.Switch(17) = 1:PlaySound "rollover":End Sub
  Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub
  Sub sw38_Hit:Controller.Switch(38) = 1:PlaySound "rollover":End Sub
  Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub
  Sub sw66_Hit:Controller.Switch(66) = 1:PlaySound "rollover":End Sub
  Sub sw66_UnHit:Controller.Switch(66) = 0:End Sub
  Sub sw67_Hit:Controller.Switch(67) = 1:PlaySound "rollover":End Sub
  Sub sw67_UnHit:Controller.Switch(67) = 0:End Sub
  Sub sw68_Hit:Controller.Switch(68) = 1:PlaySound "rollover":End Sub
  Sub sw68_UnHit:Controller.Switch(68) = 0:End Sub
  Sub sw87_Hit:Controller.Switch(87) = 1:PlaySound "rollover":End Sub
  Sub sw87_UnHit:Controller.Switch(87) = 0:End Sub
  Sub sw88_Hit:Controller.Switch(88) = 1:PlaySound "rollover":End Sub
  Sub sw88_UnHit:Controller.Switch(88) = 0:End Sub
  Sub sw71_Hit:Controller.Switch(71) = 1:PlaySound "fx_rampbump1":EngineTol=EngineTol+1:End Sub
  Sub sw71_UnHit:Controller.Switch(71) = 0:End Sub
  Sub sw72_Hit:Controller.Switch(72) = 1:PlaySound "fx_rampbump1":EngineTol=EngineTol+1:End Sub
  Sub sw72_UnHit:Controller.Switch(72) = 0:End Sub
 
'Spinners
Sub sw18_Spin:vpmTimer.PulseSw 18 : playsound"fx_spinner" : End Sub
 
'********* Stand Up Target ***********
Sub sw46_Hit:vpmTimer.PulseSw 46:PlaySound SoundFX("target",DOFTargets):End Sub
Sub sw47_Hit:vpmTimer.PulseSw 47:PlaySound SoundFX("target",DOFTargets):End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:PlaySound SoundFX("target",DOFTargets):End Sub
 
Sub sw83_Hit:vpmTimer.PulseSw 83:PlaySound SoundFX("target",DOFTargets):End Sub
Sub sw81_Hit:vpmTimer.PulseSw 81:PlaySound SoundFX("target",DOFTargets):End Sub
Sub sw82_Hit:vpmTimer.PulseSw 82:PlaySound SoundFX("target",DOFTargets):End Sub
 
'RAMP SWITCHES *********************************
 Sub sw35_Hit:Controller.Switch(35) = 1:End Sub			
 Sub sw35_unHit:Controller.Switch(35)=0:End Sub
 Sub sw43_Hit:Controller.Switch(43) = 1:End Sub			
 Sub sw43_unHit:Controller.Switch(43)=0:End Sub
 Sub sw44_Hit:vpmTimer.PulseSw 44:PlaySound "fx_metalrolling":End Sub
 Sub sw45_Hit:vpmTimer.PulseSw 45:PlaySound "fx_metalrolling":End Sub
 'Sub sw58_Hit:vpmTimer.PulseSw 58:PlaySound "fx_metalrolling":End Sub
 Sub sw75_Hit:vpmTimer.PulseSw 75:PlaySound "fx_metalrolling":End Sub
 
'Optical Triggers
 Sub sw41_Hit
Controller.Switch(41) = 1
	EngineTol=0	
	Magnet1.Enabled=1
	Magnet2.Enabled=1
	Magnet3.Enabled=1
	Magnet4.Enabled=1
	Magnet5.Enabled=1
End Sub		
 Sub sw41_unHit:Controller.Switch(41)=0:End Sub
 Sub sw42_Hit:Controller.Switch(42) = 1:End Sub			
 Sub sw42_unHit:Controller.Switch(42)=0:End Sub
 Sub sw58_Hit:Controller.Switch(58) = 1:End Sub			
 Sub sw58_unHit:Controller.Switch(58)=0:End Sub
'51
'52
'55
'56
'71
'72
 
'Scoring rubber
Sub sw84_Hit:vpmTimer.PulseSw 84:End Sub
Sub sw86_Hit:vpmTimer.PulseSw 86:End Sub
 
'**********************************
			'Bumpers
'**********************************
Sub Bumper1_Hit: vpmTimer.PulseSw 64 : PlayBumperSound : End Sub
Sub Bumper2_Hit: vpmTimer.PulseSw 63 : PlayBumperSound : End Sub
Sub Bumper3_Hit: vpmTimer.PulseSw 65 : PlayBumperSound : End Sub
 
Sub PlayBumperSound()
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySound SoundFX("fx_bumper1",DOFContactors)
			Case 2 : PlaySound SoundFX("fx_bumper2",DOFContactors)
			Case 3 : PlaySound SoundFX("fx_bumper3",DOFContactors)
		End Select
End Sub
 
'********************
'Generic Sounds
'********************
 
 
'********************
'Cars
'********************
 
 
Dim EP,NewEP,RedCarPos,BlueCarPos,NewRedCarPos,NewBlueCarPos,RedPercentDownTrack,BluePercentDownTrack,MechNoMovement
Dim BlueCarXStart, BlueCarYStart, RedCarXStart, RedCarYStart
Const MechCarStart 		= 0
Const MechCarFinish 	= 675
Const MechTimerFast		= 15
Const MechTimerSlow		= 100
 
Const BlueCarXFinish 	= 878
Const BlueCarYFinish 	= 117
Const RedCarXFinish 	= 936
Const RedCarYFinish 	= 117
 
Dim bluecarlast, redcarlast
 
BlueCarXStart = BlueCar.X
BlueCarYStart = BlueCar.Y
RedCarXStart = RedCar.X
RedCarYStart = RedCar.Y
 
 
Sub Mech_Timer
    NewBlueCarPos=Controller.GetMech(0)
    NewRedCarPos=Controller.GetMech(1)
 
	'If Cars not moving slow down timer 
	If (NewBlueCarPos = BlueCarPos) and (NewRedCarPos = RedCarPos) and (Mech.Interval <> MechTimerSlow) Then 
		If MechNoMovement > 10 Then
			Mech.Interval = MechTimerSlow
		Else 
			MechNoMovement = MechNoMovement + 1
		End If
 
	Else
		If NewBlueCarPos <> BlueCarPos Then
			BlueCarPos = NewBlueCarPos
			BluePercentDownTrack = NewBlueCarPos/(MechCarFinish - MechCarStart)
			BlueCar.Y = BlueCarYStart - (BlueCarYStart - BlueCarYFinish) * BluePercentDownTrack
			BlueCar.X = BlueCarXStart - (BlueCarXStart - BlueCarXFinish) * BluePercentDownTrack
 
 ' Thalamus
 		If ( ( bluecar.y > 117 ) and ( bluecar.y < 1421 ) and ( bluecarlast <> bluecar.y ) ) Then
			PlaySoundAtVol "electric_motor", BlueCar, CarVolume
			bluecarlast = bluecar.y
		End if
		debug.print bluecar.y
 
			'Speed Up Timer when car in motion
			If Mech.Interval <>  MechTimerFast Then Mech.Interval = MechTimerFast
		End If	
 
		If NewRedCarPos <> RedCarPos Then
			RedCarPos = NewRedCarPos
			RedPercentDownTrack = NewRedCarPos/(MechCarFinish - MechCarStart)
			RedCar.Y = RedCarYStart - (RedCarYStart - RedCarYFinish) * RedPercentDownTrack
			RedCar.X = RedCarXStart - (RedCarXStart - RedCarXFinish) * RedPercentDownTrack
 
 ' Thalamus
  		if ( ( redcar.y > 117 ) and ( redcar.y < 1422 ) and ( redcarlast <> redcar.y ) ) Then
			PlaySoundAtVol "electric_motor", RedCar, CarVolume
			redcarlast = redcar.y
		end if
 
			'Speed Up Timer when car in motion
			If Mech.Interval <>  MechTimerFast Then Mech.Interval = MechTimerFast
		End If
		MechNoMovement = 0
	End If
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

	   'If chgLamp(ii,0) = 4 Then PFGI chgLamp(ii,1)
 
        Next
    End If
    UpdateLamps
End Sub
 
Dim engnew, difeng, enghold

Sub UpdateLamps()
    LFLogo.RotY = LeftFlipper.CurrentAngle-210
    LFLogo1.RotY = LeftFlipper1.CurrentAngle-210
    RFlogo.RotY = RightFlipper.CurrentAngle+30



 	engnew=Controller.getmech(2)
	difeng=abs(enghold-engnew)
	if difeng < 30 then
		EngineP.objRoty=(engnew-127)/7
		enghold=engnew
	end If


    NFadeL 1, L1    'Bumper1
    NFadeL 2, L2    'Bumper2
    NFadeL 3, L3    'Bumper3
 
    NFadeL 11, l11
    NFadeL 12, l12
    NFadeL 13, l13
    NFadeL 14, l14
    NFadeL 15, l15
    NFadeL 16, l16
    NFadeL 17, l17
    NFadeL 18, l18
 
 
    NFadeL 21, l21
    NFadeL 22, l22
    NFadeL 23, l23
    NFadeL 24, l24
    NFadeL 25, l25
    NFadeL 26, l26
    NFadeL 27, l27
    NFadeL 28, l28
 
 
    NFadeL 31, l31
    NFadeL 32, l32
    NFadeL 33, l33
    NFadeL 34, l34
    NFadeL 35, l35
    NFadeL 36, l36
    NFadeL 37, l37
    NFadeL 38, l38
 
 
    NFadeL 41, l41
    NFadeL 42, l42
    NFadeL 43, l43
    NFadeL 44, l44
    NFadeL 45, l45
    NFadeL 46, l46
    NFadeL 47, l47
    NFadeL 48, l48
 
 
    NFadeL 51, l51
    NFadeL 52, l52
    Flash 53, f53
    NFadeL 54, l54
    NFadeL 55, l55
    NFadeL 56, l56
    NFadeL 57, l57
    NFadeL 58, l58
 
 
    NFadeL 61, l61
    NFadeL 62, l62
    NFadeL 63, l63
    NFadeL 64, l64
    NFadeL 65, l65
    NFadeL 66, l66
    NFadeL 67, l67
    Flash 68, f68
 
 
    NFadeL 71, l71
    NFadeL 72, l72
    NFadeL 73, l73
    NFadeL 74, l74
    NFadeL 75, l75
    Flash 76, f76
    Flash 77, f77
    Flash 78, f78
 
 
    Flash 81, f81
    Flash 82, f82
    Flashm 83, f83a
    Flash 83, f83
    Flashm 84, f84a
    Flash 84, f84
    Flash 85, f85
    Flash 86, f86
 
'Solenid Controlled Lamps
'NFadeL 117,  L117  'Not Shown in the Service Manual No clue where it goes
Flashm 118, Flasher118
Flash 118, Flasher118a
Flashm 119, Flasher119
Flash 119, Flasher119a
'NFadeL 120,  L120 'Backglass Only
 
NFadeL 121,  L121
NFadeL 122,  L122
NFadeL 123,  L123
NFadeL 124,  L124
NFadeL 125,  L125
Flash 126, Flasher126
NFadeL 127,  L127
NFadeLm 128,  L128
NFadeL 128,  L128b
 
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
 
 
 
'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep
 
Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 62
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
'	gi1.State = 0:Gi2.State = 0
End Sub
 
Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub
 
Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 61
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
'	gi3.State = 0:Gi4.State = 0
End Sub
 
Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub
 
' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX, Rothbauerw, Thalamus and Herweh
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

' set position as table object and Vol + RndPitch manually

Sub PlaySoundAtVolPitch(sound, tableobj, Vol, RndPitch)
  PlaySound sound, 1, Vol, AudioPan(tableobj), RndPitch, 0, 0, 1, AudioFade(tableobj)
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

Sub PlaySoundAtBallAbsVol(sound, VolMult)
  PlaySound sound, 0, VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

' requires rampbump1 to 7 in Sound Manager

Sub RandomBump(voladj, freq)
  Dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
  PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' set position as bumperX and Vol manually. Allows rapid repetition/overlaying sound

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
  Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function VolMulti(ball,Multiplier) ' Calculates the Volume of the sound based on the ball speed
  VolMulti = Csng(BallVel(ball) ^2 / 150 ) * Multiplier
End Function

Function DVolMulti(ball,Multiplier) ' Calculates the Volume of the sound based on the ball speed
  DVolMulti = Csng(BallVel(ball) ^2 / 150 ) * Multiplier
  debug.print DVolMulti
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


 
'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************
 
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
        StopSound "fx_ballrolling" & b
        StopSound "fx_plasticrolling" & b
        StopSound "fx_metalrolling" & b
    Next
 
	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
 
	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
		If BallVel(BOT(b) ) > 1 Then
            rolling(b) = True
			If BOT(b).Z > 30 Then
				' ball on plastic ramp
				StopSound "fx_ballrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_plasticrolling" & b, -1, Vol(BOT(b)) / 2, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			Else
				' ball on playfield
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_ballrolling" & b, -1, Vol(BOT(b)) / 2, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			End If
		Else
			If rolling(b) Then
                StopSound "fx_ballrolling" & b
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
                rolling(b) = False
            End If
	    End If
        ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
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
 
sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperLSh1.RotZ = LeftFlipper1.currentangle
 
    GateUp.RotZ = -(GateU.currentangle)
    DivPrim.RotZ = DivFlipper.currentangle
 
End Sub
 
'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)
 
'Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
'    If UBound(BOT) = -1 Then Exit Sub
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
'End Sub
 
 
 
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

' Thalamus : Exit in a clean and proper way
Sub Table1_exit()
  Controller.Pause = False
  Controller.Stop
End Sub

