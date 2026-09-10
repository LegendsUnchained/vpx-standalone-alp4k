Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="roadrunr",UseSolenoids=1,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM "01510000", "Atari2.VBS", 3.1
Dim DesktopMode: DesktopMode = Table1.ShowDT


'************************************************************************************************
'Carl Stalling's Original Music Stems for, "There They Go Go Go (1956)" & "Zoom and Bored (1957)"
'************************************************************************************************

Const TurnMusicOn = 0

' Start the random music 
    MusicOn

Sub MusicOn
	if TurnMusicOn = 1 Then
		Dim x
		x = INT(10 * RND(1) )
		Select Case x
		   Case 0:PlayMusic "RoadRunner/0RR1.ogg"
		   Case 1:PlayMusic "RoadRunner/0RR1a.ogg"
		   Case 2:PlayMusic "RoadRunner/0RR2.ogg"
		   Case 3:PlayMusic "RoadRunner/0RR2a.ogg"
		   Case 4:PlayMusic "RoadRunner/0RR3.ogg"
		   Case 5:PlayMusic "RoadRunner/0RR3a.ogg"
		   Case 6:PlayMusic "RoadRunner/0RR4.ogg"
		   Case 7:PlayMusic "RoadRunner/0RR4a.ogg"
		   Case 8:PlayMusic "RoadRunner/0RR5.ogg"
		   Case 9:PlayMusic "RoadRunner/0RR5a.ogg"

		   
		   End Select
	end if
 End Sub
 
 Sub Table1_MusicDone()
    MusicOn
 End Sub

'OPTION TO SHUT OFF SOUND MOD SOUNDS - MOD SOUNDS ARE ON BY DEFAULT
'*****************************************************************************************************
    SoundMod = 0          'THIS TURNS MOD SOUNDS ON OR OFF    0 turns mod sounds off   1 turns them on
'*****************************************************************************************************




If DesktopMode = True Then 'Show Desktop components
Ramp1.visible=1
Ramp2.visible=1
Ramp3.visible=1
Ramp4.visible=1
MarcoMesa.visible=1
Flasherflash1.visible=0
Flasherflash2.visible=0
Flasherflash3.visible=0
Flasherflash4.visible=0
Flasherflash5.visible=0
Flasherflash6.visible=0
Else
Ramp1.visible=0
Ramp2.visible=0
Ramp3.visible=0
Ramp4.visible=0
MarcoMesa.visible=0
Wall16.visible=0
Wall17.visible=0
Wall18.visible=0
End if

'**********************************************
'STAT's Modified Change Team script

Dim LBBImage8, LBBImage9
Dim xd, xe

Sub LoadLBB8
    xd = LoadValue(cGameName, "LBBImage8")
    If(xd <> "") Then LBBImage8 = xd Else LBBImage8 = 0
    UpdateLBB8
End Sub

Sub SaveLBB8
    SaveValue cGameName, "LBBImage8", LBBImage8
End Sub

Sub NextLBB8: LBBImage8 = (LBBImage8 +1 ) MOD 14: UpdateLBB8: SaveLBB8: End Sub

Sub UpdateLBB8
    If LBBImage8=0 Then Hongo1.image = "RRBumper":Hongo2.image = "RRBumper":End If
    If LBBImage8=1 Then Hongo1.image = "RRBumper-2":Hongo2.image = "RRBumper-2":End If
    If LBBImage8=2 Then Hongo1.image = "RRBumper-3":Hongo2.image = "RRBumper-3":End If
    If LBBImage8=3 Then Hongo1.image = "RRBumper-4":Hongo2.image = "RRBumper-4":End If
    If LBBImage8=4 Then Hongo1.image = "RRBumper-5":Hongo2.image = "RRBumper-5":End If
    If LBBImage8=5 Then Hongo1.image = "RRBumper-6":Hongo2.image = "RRBumper-6":End If
    If LBBImage8=6 Then Hongo1.image = "RRBumper-7":Hongo2.image = "RRBumper-7":End If
    If LBBImage8=7 Then Hongo1.image = "RRBumper-8":Hongo2.image = "RRBumper-8":End If
    If LBBImage8=8 Then Hongo1.image = "RRBumper-9":Hongo2.image = "RRBumper-9":End If
    If LBBImage8=9 Then Hongo1.image = "RRBumper-10":Hongo2.image = "RRBumper-10":End If
    If LBBImage8=10 Then Hongo1.image = "RRBumper-11":Hongo2.image = "RRBumper-11":End If
    If LBBImage8=11 Then Hongo1.image = "RRBumper-12":Hongo2.image = "RRBumper-12":End If
    If LBBImage8=12 Then Hongo1.image = "RRBumper-13":Hongo2.image = "RRBumper-13":End If
    If LBBImage8=13 Then Hongo1.image = "RRBumper-14":Hongo2.image = "RRBumper-14":End If
End Sub

Sub LoadLBB9
    xe = LoadValue(cGameName, "LBBImage9")
    If(xe <> "") Then LBBImage9 = xe Else LBBImage9 = 0
    UpdateLBB9
End Sub

Sub SaveLBB9
    SaveValue cGameName, "LBBImage9", LBBImage9
End Sub

Sub NextLBB9: LBBImage9 = (LBBImage9 +1 ) MOD 6: UpdateLBB9: SaveLBB9: End Sub

Sub UpdateLBB9
    If LBBImage9=0 And DesktopMode = False Then
       Flasherflash1.visible = 1:Flasherflash2.visible = 0:Flasherflash3.visible = 0:Flasherflash4.visible = 0:Flasherflash5.visible = 0:Flasherflash6.visible = 0 
    End If
    If LBBImage9=1 And DesktopMode = False Then
       Flasherflash1.visible = 0:Flasherflash2.visible = 1:Flasherflash3.visible = 0:Flasherflash4.visible = 0:Flasherflash5.visible = 0:Flasherflash6.visible = 0 
    End If
    If LBBImage9=2 And DesktopMode = False Then
       Flasherflash1.visible = 0:Flasherflash2.visible = 0:Flasherflash3.visible = 1:Flasherflash4.visible = 0:Flasherflash5.visible = 0:Flasherflash6.visible = 0 
    End If
    If LBBImage9=3 And DesktopMode = False Then
       Flasherflash1.visible = 0:Flasherflash2.visible = 0:Flasherflash3.visible = 0:Flasherflash4.visible = 1:Flasherflash5.visible = 0:Flasherflash6.visible = 0 
    End If
    If LBBImage9=4 And DesktopMode = False Then
       Flasherflash1.visible = 0:Flasherflash2.visible = 0:Flasherflash3.visible = 0:Flasherflash4.visible = 0:Flasherflash5.visible = 1:Flasherflash6.visible = 0 
    End If
    If LBBImage9=5 And DesktopMode = False Then
       Flasherflash1.visible = 0:Flasherflash2.visible = 0:Flasherflash3.visible = 0:Flasherflash4.visible = 0:Flasherflash5.visible = 0:Flasherflash6.visible = 1 
    End If
End Sub

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

SolCallback(4)="dtR.SolDropUp"
SolCallback(5)="dtL.SolDropUp"
SolCallback(15)="bsTrough.SolOut"

SolCallBack(16) = "FastFlips.TiltSol"
SolCallback(sLRFlipper) = ""
SolCallback(sLLFlipper) = ""
SolCallback(sURFlipper) = ""
SolCallback(sULFlipper) = ""

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd:RightFlipper1.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart:RightFlipper1.RotateToStart
     End If
End Sub
'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

'*****GI Lights On
sub Encendido_timer
dim xx
playsound "encendido"
For each xx in Ambiente:xx.State = 1: Next		
	me.enabled=false
For each xx in Ambientep:xx.State = 1: Next		
	me.enabled=false
end sub



'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, dtL, dtR
Dim BIP, SoundMod

Sub StopSounds()

   StopSound"0DRR0"
   StopSound"0DRR1"
   StopSound"0RR5a"
   StopSound"0RR5b"
   StopSound"0RR5e"
   StopSound"0RRa"
   StopSound"0RRc"
   StopSound"0RRd"
   StopSound"0RRf"
   StopSound"0RRg"
   StopSound"0RRh"
   StopSound"0RRi"
   StopSound"0RRj"
   StopSound"0RRk"
   StopSound"ThatsAllFolks"
End Sub

Sub Table1_Init
	vpmInit Me:LoadLUT:LoadLBB8:LoadLBB9
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Road Runner (Atari 1979) Remastered v3.0"&chr(13)&"VPX table by Kalavera"&vbNewLine&"Remastered by GaryInMotion (Original SoundMod by Xenonph)"
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

	PinMAMETimer.Interval=PinMAMEInterval
    PinMAMETimer.Enabled=True:Bumper1.timerinterval=3000:Bumper1.timerenabled=1

	vpmNudge.TiltSwitch=48
	vpmNudge.Sensitivity=2
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,LeftSlingshot,RightSlingshot)

	Set bsTrough=New cvpmBallStack
		bsTrough.InitSw 0,1,0,0,0,0,0,0
		bsTrough.InitKick BallRelease,110,6
		bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsTrough.Balls=1
		
	Set dtL=New cvpmDropTarget
		dtL.InitDrop Array(Corre1,Corre2,Corre3,Corre4),Array(8,9,10,11)
		dtL.InitSnd  SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)
		
	Set dtR=New cvpmDropTarget
		dtR.InitDrop Corre5,12
		dtR.InitSnd  SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

Captive1.CreateBall
Captive2.CreateBall
Captive3.CreateBall
Captive1.Kick 180,1
Captive2.Kick 180,1
Captive3.Kick 180,1

End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyCode=LeftMagnaSave Then NextLBB9
	If KeyCode=RightMagnaSave Then NextLBB8
	If KeyCode= 49 Then SoundMod=0:StopSounds:Drain.timerenabled=0:Gate3.timerenabled=0:Gate4.timerenabled=0:Bumper1.timerenabled=0
	If KeyCode= 50 Then SoundMod=1:PlaySound"0RR5b"
	If keycode = 3 Then NextLut
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
      If KeyCode = LeftFlipperKey then FastFlips.FlipL True : ' FastFlips.FlipUL True
      If KeyCode = RightFlipperKey then FastFlips.FlipR True :  'FastFlips.FlipUR True
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
    If KeyCode = 6 and SoundMod=0 Then PlaySound"coin"


    If KeyCode = 6 and SoundMod=1 Then
			Dim s
			s = INT(2 * RND(1) )
			Select Case s
			Case 0:PlaySound"coin":PlaySound("0RR5a")
			Case 1:PlaySound"coin":PlaySound("0RR5b")

			End Select
            End If


    If KeyCode = 4 and SoundMod=0 Then PlaySound"coin"


    If KeyCode = 4 and SoundMod=1 Then
			Dim y
			y = INT(2 * RND(1) )
			Select Case y
			Case 0:PlaySound"coin":PlaySound("0RR5a")
			Case 1:PlaySound"coin":PlaySound("0RR5b")

			End Select
            End If

	If KeyCode = 2 and SoundMod=1 Then Playsound"0DRR0":Gate4.timerenabled=0


	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey and BIP=1 Then Plunger.Fire:PlaySound"plunger"
     If keycode = PlungerKey and BIP=0 Then Plunger.Fire:PlaySound"plungerreleasefree"
     If KeyCode = LeftFlipperKey then FastFlips.FlipL False :  'FastFlips.FlipUL False
     If KeyCode = RightFlipperKey then FastFlips.FlipR False :  'FastFlips.FlipUR False	
End Sub

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me :StopSounds :playsound"drain" 
     If SoundMod=1 Then PlaySound"0DRR1":GameOver1.enabled=true:GameOver1.interval=100:GameOver1off.enabled=true:GameOver1off.interval=9000

End Sub

Sub GameOver1_Timer
If Controller.Lamp(61) Then
EndMusic
Playsound"ThatsAllFolks"
GameOver1.enabled=False
GameOver1off.enabled=False
Attract1.enabled=true:Attract1.interval=4000
End If
End Sub 


Sub GameOver1off_Timer
GameOver1.enabled=False
GameOver1off.enabled=False
End Sub 

Sub Attract1_Timer
MusicOn
Attract1.enabled=False
End Sub 

'Drop Targets
Sub Corre1_Dropped:dtL.Hit 1 
          If SoundMod=1 Then PlaySound"0RRi"
End Sub  
Sub Corre2_Dropped:dtL.Hit 2 
          If SoundMod=1 Then PlaySound"0RRi"
End Sub  
Sub Corre3_Dropped:dtL.Hit 3 
          If SoundMod=1 Then PlaySound"0RRi"
End Sub
Sub Corre4_Dropped:dtL.Hit 4 
          If SoundMod=1 Then PlaySound"0RRi"
End Sub

Sub Corre5_Dropped:dtR.Hit 1 
          If SoundMod=1 Then PlaySound"0RRi"
End Sub

 'Scoring Rubber
Sub Conf13_Hit:vpmTimer.PulseSw 13 : playsound"flip_hit_3" : End Sub 

'Wire Triggers
Sub Conf16_Hit:Controller.Switch(16)=1 : playsound"rollover"
          If SoundMod=1 Then PlaySound"0RR5b"
End Sub
Sub Conf16_unHit:Controller.Switch(16)=0:End Sub
Sub Conf17_Hit:Controller.Switch(17)=1 : playsound"rollover"  
          If SoundMod=1 Then PlaySound"0RR5b"
End Sub
Sub Conf17_unHit:Controller.Switch(17)=0:End Sub
Sub Conf18_Hit:Controller.Switch(18)=1 : playsound"rollover" 
          If SoundMod=1 Then PlaySound"0RRd"
End Sub
Sub Conf18_unHit:Controller.Switch(18)=0:End Sub
Sub Conf19_Hit:Controller.Switch(19)=1 : playsound"rollover" 
          If SoundMod=1 Then PlaySound"0RRc"
End Sub
Sub Conf19_unHit:Controller.Switch(19)=0:End Sub
Sub Conf24_Hit:Controller.Switch(24)=1 : playsound"rollover" 
          If SoundMod=1 Then PlaySound"0RRd"
End Sub
Sub Conf24_unHit:Controller.Switch(24)=0:End Sub

'Star Triggers
Sub Conf27_Hit:Controller.Switch(27)=1 : playsound"rollover"
              If SoundMod=1 Then 
              Dim x
              x = INT(2 * RND(1) )
              Select Case x
              Case 0:PlaySound"0RRf"
              Case 1:PlaySound"0RRg"
              End Select
              End If
End Sub
Sub Conf27_unHit:Controller.Switch(27)=0:End Sub
Sub Conf28_Hit:Controller.Switch(28)=1 : playsound"rollover"
              If SoundMod=1 Then 
              Dim x
              x = INT(2 * RND(1) )
              Select Case x
              Case 0:PlaySound"0RRf"
              Case 1:PlaySound"0RRg"
              End Select
              End If
End Sub
Sub Conf28_unHit:Controller.Switch(28)=0:End Sub

'Spinners
Sub Conf25_Spin:vpmTimer.PulseSw 25 : playsound"fx_spinner" : End Sub
Sub Conf26_Spin:vpmTimer.PulseSw 26 : playsound"fx_spinner" : End Sub

 'Stand Up Targets
Sub Conf36_Hit:vpmTimer.PulseSw 36
          If SoundMod=1 Then Playsound"0RRh"
End Sub
Sub Conf37_Hit:vpmTimer.PulseSw 37
          If SoundMod=1 Then Playsound"0RRh"
End Sub
Sub Conf40_Hit:vpmTimer.PulseSw 40
          If SoundMod=1 Then Playsound"0RRh"
End Sub
Sub Conf41_Hit:vpmTimer.PulseSw 41
          If SoundMod=1 Then PlaySound"0RRh"
End Sub
Sub Conf42_Hit:vpmTimer.PulseSw 42
          If SoundMod=1 Then Playsound"0RRh"
End Sub
Sub Conf43_Hit:vpmTimer.PulseSw 43
          If SoundMod=1 Then Playsound"0RRh"
End Sub
Sub Conf44_Hit:vpmTimer.PulseSw 44
          If SoundMod=1 Then Playsound"0RRh"
End Sub
Sub Conf45_Hit:vpmTimer.PulseSw 45
          If SoundMod=1 Then PlaySound"0RRh"
End Sub
Sub Conf49_Hit:vpmTimer.PulseSw 49
          If SoundMod=1 Then Playsound"0RRh"
End Sub
Sub Conf50_Hit:vpmTimer.PulseSw 50
          If SoundMod=1 Then Playsound"0RRh"
End Sub
Sub Conf51_Hit:vpmTimer.PulseSw 51
          If SoundMod=1 Then Playsound"0RRh"
End Sub
Sub Conf56_Hit:vpmTimer.PulseSw 56
          If SoundMod=1 Then Playsound"0RRh"
End Sub
Sub Conf57_Hit:vpmTimer.PulseSw 57
          If SoundMod=1 Then Playsound"0RRh"
End Sub

'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw(56) : playsound SoundFX("fx_bumper1",DOFContactors) 

End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(57) : playsound SoundFX("fx_bumper1",DOFContactors) 

End Sub


Set Lights(1)=L1
Set Lights(2)=L2
Set Lights(3)=L3
Set Lights(4)=L4
Set Lights(5)=L5
Set Lights(6)=L6
Set Lights(7)=L7
Set Lights(8)=L8
Set Lights(9)=L9
Set Lights(10)=L10
Set Lights(11)=L11
Set Lights(12)=L12
Set Lights(13)=L13
Set Lights(14)=L14
Set Lights(15)=L15
Set Lights(16)=L16
Set Lights(17)=L17
Set Lights(18)=L18
Set Lights(19)=L19
Set Lights(20)=L20
Set Lights(21)=L21
Set Lights(22)=L22
Set Lights(23)=L23
Set Lights(24)=L24
Set Lights(25)=L25
Set Lights(26)=L26
Set Lights(27)=L27
Set Lights(28)=L28
Set Lights(29)=L29
Set Lights(30)=L30
Set Lights(31)=L31
Set Lights(32)=L32 
Set Lights(33)=L33
Set Lights(34)=L34
Set Lights(35)=L35
Set Lights(36)=L36
Set Lights(37)=L37
Set Lights(38)=L38
Set Lights(39)=L39
Set Lights(40)=L40
Set Lights(41)=L41
Set Lights(42)=L42
Set Lights(43)=L43
Lights(44)=Array(L44,GI_Bumper_1)
Lights(45)=Array(L45,GI_Bumper_2)
Set Lights(46)=L46
Set Lights(47)=L47
Set Lights(48)=L48
Set Lights(49)=L49	
Set Lights(50)=L50
Set Lights(51)=L51
'Set Lights(52)=T52
'Set Lights(53)=T53
'Set Lights(54)=L54
'Set Lights(55)=L55' High Score
'Set Lights(56)=L56'Tilt
'Set Lights(57)=T57'High Score P1
'Set Lights(58)=T58'High Score P2
'Set Lights(59)=T59'High Score P3
'Set Lights(60)=T60'High Score P4
Set Lights(61)=L61'Game Over
'Set Lights(62)=BallInPlay1'Ball In Play 1
'Set Lights(63)=BallInPlay2'Ball In Play 2
'Set Lights(64)=Match'Match


'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************

Dim Digits(28)
' 1st Player
Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)

' 2nd Player
Digits(6) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(7) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(8) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(9) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(10) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(11) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)

' 3rd Player
Digits(12) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(13) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(14) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(15) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(16) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(17) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)

' 4th Player
Digits(18) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(19) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(20) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(21) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(22) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(23) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)

' Credits
Digits(24) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(25) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)
' Balls
Digits(26) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(27) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)

Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		If DesktopMode = True Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 28) then
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

'**********************************************************************************************************
'**********************************************************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
vpmTimer.PulseSw 59
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    If SoundMod=1 Then PlaySound"0RRa"

End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
vpmTimer.PulseSw 58
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    If SoundMod=1 Then PlaySound"0RRa"

End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
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
    Vol = Csng(BallVel(ball) ^2 / 2000)
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySound "fx_MetalHit2", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_rubber_post", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_rubber_pin", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub

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
	FlipperRSh1.RotZ = RightFlipper1.currentangle
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


'**********************************************************************************************************
'cFastFlips by nFozzy
'**********************************************************************************************************
dim FastFlips
Set FastFlips = new cFastFlips
with FastFlips
	.CallBackL = "SolLflipper"	'Point these to flipper subs
	.CallBackR = "SolRflipper"	'...
'	.CallBackUL = "SolULflipper"'...(upper flippers, if needed)
'	.CallBackUR = "SolURflipper"'...
	.TiltObjects = True 'Optional, if True calls vpmnudge.solgameon automatically. IF YOU GET A LINE 1 ERROR, DISABLE THIS! (or setup vpmNudge.TiltObj!)
'	.InitDelay "FastFlips", 100			'Optional, if > 0 adds some compensation for solenoid jitter (occasional problem on Bram Stoker's Dracula)
'	.DebugOn = False		'Debug, always-on flippers. Call FastFlips.DebugOn True or False in debugger to enable/disable.
end with

Class cFastFlips
	Public TiltObjects, DebugOn
	Private SubL, SubUL, SubR, SubUR, FlippersEnabled, Delay, LagCompensation, Name
	
	Private Sub Class_Initialize()
		Delay = 0 : FlippersEnabled = False : DebugOn = False : LagCompensation = False
	End Sub
	
	'set callbacks
	Public Property Let CallBackL(aInput)  : Set SubL  = GetRef(aInput) : End Property
	Public Property Let CallBackUL(aInput) : Set SubUL = GetRef(aInput) : End Property
	Public Property Let CallBackR(aInput)  : Set SubR  = GetRef(aInput) : End Property
	Public Property Let CallBackUR(aInput) : Set SubUR = GetRef(aInput) : End Property
	Public Sub InitDelay(aName, aDelay) : Name = aName : delay = aDelay : End Sub	'Create Delay
	
	'call callbacks
	Public Sub FlipL(aEnabled)
		if not FlippersEnabled and not DebugOn then Exit Sub
		subL aEnabled
	End Sub
	
	Public Sub FlipR(aEnabled)
		if not FlippersEnabled and not DebugOn then Exit Sub
		subR aEnabled
	End Sub

	Public Sub FlipUL(aEnabled)
		if not FlippersEnabled and not DebugOn then Exit Sub
		subUL aEnabled
	End Sub	

	Public Sub FlipUR(aEnabled)
		if not FlippersEnabled and not DebugOn then Exit Sub
		subUR aEnabled
	End Sub	
	
	Public Sub TiltSol(aEnabled)	'Handle solenoid / Delay (if delayinit)
		if delay > 0 and not aEnabled then 	'handle delay
			vpmtimer.addtimer Delay, Name & ".FireDelay" & "'"
			LagCompensation = True
		else
			if Delay > 0 then LagCompensation = False
			EnableFlippers(aEnabled)
		end if
	End Sub
	
	Sub FireDelay() : if LagCompensation then EnableFlippers False End If : End Sub
	
	Private Sub EnableFlippers(aEnabled)
		FlippersEnabled = aEnabled
		if TiltObjects then vpmnudge.solgameon aEnabled
		If Not aEnabled then
			subL False
			subR False
			if not IsEmpty(subUL) then subUL False
			if not IsEmpty(subUR) then subUR False
		End If		
	End Sub
	
End Class
'**********************************************************************************************************
'**********************************************************************************************************

'*************
'   JP'S LUT MODIFIED
'*************

Dim LUTImage
Dim xa
Sub LoadLUT
    xa = LoadValue(cGameName, "LUTImage")
    If(xa <> "") Then LUTImage = xa Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 13: UpdateLUT: SaveLUT: End Sub

Sub UpdateLUT
Select Case LutImage
Case 0: Table1.ColorGradeImage = "LUT0"
Case 1: Table1.ColorGradeImage = "LUT1"
Case 2: Table1.ColorGradeImage = "LUT2"
Case 3: Table1.ColorGradeImage = "LUT3"
Case 4: Table1.ColorGradeImage = "LUT4"
Case 5: Table1.ColorGradeImage = "LUT5"
Case 6: Table1.ColorGradeImage = "LUT6"
Case 7: Table1.ColorGradeImage = "LUT7"
Case 8: Table1.ColorGradeImage = "LUT8"
Case 9: Table1.ColorGradeImage = "LUT9"
Case 10: Table1.ColorGradeImage = "LUT10"
Case 11: Table1.ColorGradeImage = "LUT11"
Case 12: Table1.ColorGradeImage = "LUT12"
End Select
End Sub