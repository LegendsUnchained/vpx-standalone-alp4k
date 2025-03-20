Option Explicit
Randomize


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="csmic_l1",UseSolenoids=1,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="fx_coin"'".weeh"
Const Ballsize = 50
Const BallMass = 1.6



LoadVPM "00990300", "S7.VBS", 2.21 
Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Else
Ramp16.visible=0
Ramp15.visible=0
End if

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
	SolCallback(1)			= "bsTrough.SolIn"
	SolCallback(2)			= "bsTrough.SolOut"
	SolCallback(3)			= "bsSaucerT.SolOut"
	SolCallback(4)			= "bsSaucerB.SolOut"
	SolCallback(5)			= "dtL.SolDropUp"
	SolCallback(6)			= "dtC.SolDropUp"
	SolCallback(7)			= "dtR.SolDropUp"
	SolCallback(11)			= "GILights"
	SolCallback(15)			= "vpmSolSound Knocker,"  
	SolCallback(5)			= "dtL.SolDropUp"
	SolCallback(6)			= "dtC.SolDropUp"
	SolCallback(7)			= "dtR.SolDropUp"
	SolCallback(11)			= "GILights"
	SolCallback(15)			= "vpmSolSound Knocker,"  
	'SolCallback(25)			= "sGameOn"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound"fx_Flipperup":LeftFlipper.RotateToEnd
     Else
         PlaySound "fx_Flipperdown":LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound"fx_Flipperup":RightFlipper.RotateToEnd
     Else
         PlaySound "fx_Flipperdown":RightFlipper.RotateToStart
     End If
End Sub

'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

Sub GILights(Enabled)
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 0: Next
	Else
		For each xx in GI:xx.State = 1: Next
	End If
End Sub

 Sub sGameOn(Enabled)
	If Enabled Then

	Else

	End If
End Sub


'**********************************************************************************************************

'MUSIC
'**********************************************************************************************************
Dim musicNum : Dim musicEnd

Sub music_hit()

    If musicNum = 0 then PlayMusic "TW_01.mp3" End If
	If musicNum = 1 then PlayMusic "TW_02.mp3" End If
    If musicNum = 2 then PlayMusic "TW_03.mp3" End If
    If musicNum = 3 then PlayMusic "TW_04.mp3" End If
    If musicNum = 4 then PlayMusic "TW_05.mp3" End If
    If musicNum = 5 then PlayMusic "TW_06.mp3" End If
    If musicNum = 6 then PlayMusic "TW_07.mp3" End If
    If musicNum = 7 then PlayMusic "TW_08.mp3" End If
    If musicNum = 8 then PlayMusic "TW_09.mp3" End If
    If musicNum = 9 then PlayMusic "TW_10.mp3" End If
   
    
    musicNum = (musicNum + 1) mod 10
End Sub

Sub table1_MusicDone
    music_hit
End Sub


'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************
 Dim bsTrough, dtL, dtC, dtR, bsSaucerT, bsSaucerB

PlaySound ".weeh"

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Cosmic GunFight (Williams)"&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
		.Games(cGameName).Settings.Value("sound") = 0
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
		Controller.SolMask(0)=0
      vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
		Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0


	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

	vpmNudge.TiltSwitch=swTilt
	vpmNudge.Sensitivity=5
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3,Bumper4,LeftSlingshot,RightSlingshot)
 
	Set bsTrough=New cvpmBallstack
		bsTrough.InitSw 29,27,28,0,0,0,0,0
		bsTrough.InitKick BallRelease,90,4
		bsTrough.InitExitSnd "ballrelease","solon"
		bsTrough.Balls=2
 
	Set bsSaucerT = New cvpmBallStack
		bsSaucerT.InitSaucer sw25,25,89,10
		bsSaucerT.KickForceVar = 6
		bsSaucerT.InitExitSnd "popper_ball",""

  
	Set bsSaucerB = New cvpmBallStack
		bsSaucerB.InitSaucer sw26,26,90,12
		bsSaucerB.KickForceVar = 4
		bsSaucerB.InitExitSnd "popper_ball",""

   
 	Set dtL=New cvpmDropTarget
		dtL.InitDrop  Array(sw40,sw41,sw42),Array(40,41,42)
		dtL.InitSnd "DTDrop","DTReset"

 	Set dtC=New cvpmDropTarget
		dtC.InitDrop Array(sw43,sw44,sw45),Array(43,44,45)
		dtC.InitSnd "DTDrop","DTReset"

 	Set dtR=New cvpmDropTarget
		dtR.InitDrop Array(sw46,sw47,sw48),Array(46,47,48)
		dtR.InitSnd "DTDrop","DTReset"

End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
	If keycode = LeftFlipperKey then controller.switch(52)=1
	If keycode = RightFlipperKey then controller.switch(49)=1
    If keycode = 5 Then PlaySound ".hey":PlaySound "fx_coin"
    If keycode = 4 Then PlaySound ".hey":PlaySound "fx_coin"
    If keycode = 6 Then PlaySound ".hey":PlaySound "fx_coin"
    If keycode = 2 Then PlaySound ".heykurz"
    If keycode = RightMagnaSave Then music_hit
    If keycode = LeftMagnaSave Then PlaySound ".tw2" :EndMusic
    If vpmKeyDown(keycode)Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
    If vpmKeyUp(keycode)Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
	If keycode = LeftFlipperKey then controller.switch(52)=0
	If keycode = RightFlipperKey  then controller.switch(49)=0

End Sub


'**********************************************************************************************************
 ' Drain hole and kickers


Sub Drain_Hit
     playsound"drain"
     If l15.State Then PlaySound ".acting"
     If l16.State Then PlaySound ".tr-magiclight"
     If l17.State Then PlaySound ".dogslive"
     If l18.State Then PlaySound ".stop"
     If l19.State Then PlaySound ".pissoff"
     bsTrough.addball me
 End Sub

Sub sw25_Hit  
    bsSaucerT.AddBall Me 
    PlaySound "kicker_enter_left" 
    If l25.State Then PlaySound ".locked"
End Sub

Sub sw26_Hit 
    bsSaucerB.AddBall Me 
    PlaySound "kicker_enter_left" 
    If l26.State Then PlaySound ".locked"
End Sub

Sub sw26a_Hit 
    bsSaucerB.AddBall Me 
    PlaySound "kicker_enter_left" 
    If l26.State Then PlaySound ".locked"
End Sub


Sub KickerA_Hit:
    playsound".wusch"
    me.DestroyBall
    Me.TimerEnabled = 1
End Sub

Sub KickerA_Timer
    KickerB.CreateBall :
    KickerB.Kick -90,15
    Me.Timerenabled = 0
End Sub


'Wire Triggers


Sub sw9_Hit:Controller.Switch(9) = 1
     PlaySound "rollover"
     If l9.State = 0 Then PlaySound ".tr-time"
     If l9.State Then PlaySound ".tr-terible"
End Sub
Sub sw9_unHit:Controller.Switch(9)=0:End Sub


Sub sw9a_Hit:Controller.Switch(9)=1
     If l9a.State = 0 Then PlaySound ".shit"
     If l9a.State Then PlaySound ".leave"
End Sub 
Sub sw9a_unHit:Controller.Switch(9)=0:End Sub


Sub sw10_Hit:Controller.Switch(10) = 1
     PlaySound "rollover"
     If l10.State = 0 Then PlaySound ".tr-though"
     If l10.State Then PlaySound ".tr-terible"
End Sub
Sub sw10_unHit:Controller.Switch(10)=0 : End Sub


Sub sw10a_Hit:Controller.Switch(10) = 1
     PlaySound "rollover"
     If l10.State = 0 Then PlaySound ".tr-bedroom"
     If l10.State Then PlaySound ".damnit"
End Sub
Sub sw10a_unHit:Controller.Switch(10)=0:End Sub


Sub sw11_Hit:Controller.Switch(11) = 1
     PlaySound "rollover"
     If l11.State = 0 Then PlaySound ".jov-magic"
     If l11.State Then PlaySound ".inlane"
End Sub
Sub sw11_unHit:Controller.Switch(11)=0:End Sub


Sub sw12_Hit:Controller.Switch(12) = 1
     PlaySound "rollover"
     If l12.State = 0 Then PlaySound ".trinking"
     If l12.State Then PlaySound ".inlane"
End Sub
Sub sw12_unHit:Controller.Switch(12)=0:End Sub


Sub sw13_Hit:Controller.Switch(13) = 1
     PlaySound "rollover"
     If l13.State = 0 Then PlaySound ".jov-well"
     If l13.State Then PlaySound ".inlane"
End Sub
Sub sw13_unHit:Controller.Switch(13)=0:End Sub


Sub sw14_Hit:Controller.Switch(14) = 1
     PlaySound "rollover"
     If l15.State Then PlaySound ".hereagain"
     If l16.State Then PlaySound ".geraldw"
     If l17.State Then PlaySound ".free"
     If l18.State Then PlaySound ".trouble"
     If l19.State Then PlaySound ".broom"
End Sub
Sub sw14_unHit:Controller.Switch(14)=0:End Sub


Sub sw15_Hit:Controller.Switch(34) = 1
     If l14.State Then PlaySound ".achm"
     If l14.State = 0 Then PlaySound ".cut"
End Sub
Sub sw15_unHit:Controller.Switch(34) = 0 :End Sub



Sub sw31_Hit:Controller.Switch(31)=1 : playsound"rollover" : End Sub 
Sub sw31_unHit:Controller.Switch(31)= 0:End Sub


Sub sw32_Hit:Controller.Switch(13) = 1
     PlaySound "rollover"
     If l13a.State = 0 Then PlaySound ".accident"
     If l13a.State Then PlaySound ".woman"
End Sub
Sub sw32_unHit:Controller.Switch(13)= 0:End Sub


Sub sw38_Hit:Controller.Switch(38) = 1
     If l14.State = 0 Then PlaySound ".witcher"
     If l14.State Then PlaySound ".triss"
     If l27.State Then PlaySound ".extraballc"
End Sub
Sub sw38_unHit:Controller.Switch(38)= 0:End Sub


Sub sw39_Hit:Controller.Switch(11) = 1
     PlaySound "rollover"
     If l11.State = 0 Then PlaySound ".jov-book"
     If l11.State Then PlaySound ".inlane"
End Sub
Sub sw39_unHit:Controller.Switch(11)= 0:End Sub


Sub sw51_Hit:Controller.Switch(51)=1  
     If l1.State Then PlaySound ".shootagain"
     If l8.State Then PlaySound ".last"
End Sub 
Sub sw51_unHit:Controller.Switch(51)= 0: End Sub


Sub triss_Hit:
     If l28.State Then PlaySound ".tr-pleasure"
     If l29.State Then PlaySound ".tr-stay"
     If l30.State Then PlaySound ".tr-perhaps"
     If l31.State Then PlaySound ".tr-missed"
     If l32.State Then PlaySound ".tr-bestbed"
End Sub


Sub EB_Hit:
     If l27.State Then PlaySound ".extraballlit"
End Sub


Sub SkillShot_Hit: 
     Controller.Switch(33) = 1
     Controller.Switch(34) = 1
     Controller.Switch(35) = 1
     If l27.State = 0 Then PlaySound ".tr-job"
     If l27.State Then PlaySound ".extraballc"
End Sub


Sub SkillShot_unHit: 
     Controller.Switch(33) = 0
     Controller.Switch(34) = 0
     Controller.Switch(35) = 0
   End Sub

'Scoring Rubbers
'Sub sw15_Hit : vpmTimer.PulseSw(15) : playsound".switch":End Sub
Sub sw18_Hit : vpmTimer.PulseSw(18) : playsound".switch2":End Sub
Sub sw19_Hit : vpmTimer.PulseSw(19) : playsound".switch3":End Sub
Sub sw20_Hit : vpmTimer.PulseSw(20) : playsound".switch3":End Sub

Sub sw50_Hit :  Controller.Switch(50)= 1 : playsound".switch2":End Sub
Sub sw50_unHit: Controller.Switch(50)= 0 : End Sub


'Stand Up Targets
Sub sw33_Hit 
        vpmTimer.PulseSw(33)
        playsound".targethit" 
        If l33.State Then If l36.State = 0 Then PlaySound ".hell"
        If l34.State Then If l39.State = 0 Then PlaySound ".gold"
        If l35.State Then If l42.State = 0 Then PlaySound ".here"
   End Sub


Sub sw34_Hit
        vpmTimer.PulseSw(34)
        playsound".targethit" 
        If l33.State Then If l37.State = 0 Then PlaySound ".hell" 
        If l34.State Then If l40.State = 0 Then PlaySound ".gold"
        If l35.State Then If l43.State = 0 Then PlaySound ".here"
   End Sub

Sub sw35_Hit
        vpmTimer.PulseSw(35)
        playsound".targethit" 
        If l33.State Then If l38.State = 0 Then PlaySound ".hell" 
        If l34.State Then If l41.State = 0 Then PlaySound ".gold"
        If l35.State Then If l44.State = 0 Then PlaySound ".here"
   End Sub


'Drop Targets
 Sub Sw40_Dropped:dtL.Hit 1 : PlaySound ".ww1":End Sub  
 Sub Sw41_Dropped:dtL.Hit 2 : PlaySound ".ww2":End Sub  
 Sub Sw42_Dropped:dtL.Hit 3 : PlaySound ".ww3":End Sub

 Sub Sw43_Dropped:dtC.Hit 1 : PlaySound ".lesh1":End Sub  
 Sub Sw44_Dropped:dtC.Hit 2 : PlaySound ".lesh2":End Sub  
 Sub Sw45_Dropped:dtC.Hit 3 : PlaySound ".lesh3":End Sub

 Sub Sw46_Dropped:dtR.Hit 1 : PlaySound ".striga":End Sub  
 Sub Sw47_Dropped:dtR.Hit 2 : PlaySound ".striga2":End Sub  
 Sub Sw48_Dropped:dtR.Hit 3 : PlaySound ".striga3":End Sub

'Bumpers
 Sub Bumper1_Hit : vpmTimer.PulseSw 21 : playsound"fx_bumper1": playsound".sword2": End Sub
 Sub Bumper2_Hit : vpmTimer.PulseSw 22 : playsound"fx_bumper1": playsound".sword":End Sub
 Sub Bumper3_Hit : vpmTimer.PulseSw 24 : playsound"fx_bumper1": playsound".bumper3":End Sub
 Sub Bumper4_Hit : vpmTimer.PulseSw 23 : playsound"fx_bumper1": playsound".bumper3":End Sub


 Sub MusicStop_Timer()

     If l5.state = 1 Then EndMusic
     If l3.state = 1 Then EndMusic

 End Sub


 Sub Switch_Timer()

     If l14.state = 1 Then l14b.state = 0
     If l14.state = 0 Then l14b.state = 1
     If l64.state = 1 Then l14b.state = 0
 
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
        Next
    End If
    UpdateLamps
End Sub

Sub UpdateLamps

	NFadeL 1, l1
	NFadeL 3, l3
    NFadeL 5, l5
	NFadeL 7, l7
	NFadeL 8, l8
	NFadeLm 9, l9
    NFadeL 9, l9a
	NFadeLm 10, l10
	NFadeL 10, l10a
	NFadeLm 11, l11
	NFadeL 11, l11a
	NFadeL 12, l12
	NFadeLm 13, l13
	NFadeL 13, l13a
	NFadeLm 14, l14
	NFadeL 14, l14a
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
	NFadeLm 26, l26a
	NFadeL 26, l26
	NFadeLm 27, l27
	NFadeL 27, l27a
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

Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		If DesktopMode = True Then
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

' ********************************************************************************************************************************
' ********************************************************************************************************************************
   'Start of VPX  Call Back Functions
' ********************************************************************************************************************************
' ********************************************************************************************************************************



'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 37
    PlaySound "right_slingshot", 0, 1, 0.05, 0.05
    PlaySound ".sword"
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
	vpmTimer.PulseSw 36
    PlaySound "left_slingshot",0,1,-0.05,0.05
    PlaySound ".sword"
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

Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound")=1:Controller.Stop:End Sub