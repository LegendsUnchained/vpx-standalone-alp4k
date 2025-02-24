       '  CYBERNAUT    Bally 1985





' VPX version by Mistermixer - Schreibi34    (2019)

     'script based on the vp9 version from Destruk/Apoc/Skalar/Kristian/TAB


' BIG thanx to Schreibi34 who made almost all the 3D primitives !!!


' Also thanx to all developers involved who made this tablebuild possible !!!

         ' 32assassin - kiwi -  gtxjoe - flupper - arngrim - inkochnito   ..........and all visual pinball developers for their fantastic work

' Table testers :  roccodimarco - naeromagus




Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="cybrnaut",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM "01130100", "Bally.VBS", 3.21
Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components

Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=1
PFrightmid.State=0
Else
Ramp16.visible=0
Ramp15.visible=0
Primitive13.visible=0
End if

Const sSaucer=9'1
Const sLT=10'2              drop target reset
Const sBumper=11'3         
Const sRightSlingshot=12'4
Const sLeftSlingshot=13'5
Const sBallRelease=14'6     outhole
Const sKnocker=15'7
Const sGate=17'8            left gate
Const sCLo=18'9             ?
Const sEnable=19'10         K1 reley ( flipper enable )
Const sRightGate=20'11

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

SolCallback(sBallRelease)="bsTrough.SolOut"
SolCallback(sKnocker)="vpmSolSound SoundFX(""knocker"",DOFKnocker),"
'SolCallback(sRightSlingshot)="vpmSolSound ""slingshot""," 
'SolCallback(sLeftSlingshot)="vpmSolSound ""slingshot""," 
SolCallBack(sSaucer)="bsSaucer.SolOut"
SolCallback(sLT)="dtL.SolDropUp Not"
SolCallBack(sGate)="SolGate"
SolCallback(sRightGate)="SolRightGate"
'SolCallback(sBumper)="vpmSolSound ""jet3"","
SolCallback(sEnable)="vpmNudge.SolGameOn"

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

 Sub FlipperTimer_Timer
	FlipperT1.roty = LeftFlipper1.currentangle  + 0
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperLSh1.RotZ = LeftFlipper1.currentangle
	Prim1.Rotx = Gate3.Currentangle 
	Prim2.Rotx = Gate5.Currentangle
    Prim18.Rotx = Gate10.Currentangle


Primitive7.RotY = Flipper2.currentangle
Primitive6.RotY = Flipper3.currentangle

End Sub

Sub SolGate(Enabled) 

SetLamp 134, Enabled  

If Enabled Then  

     Flipper2.RotateToEnd  
     L135.State=LightStateON 

Else  

  Flipper2.RotateToStart 
  L135.State=LightStateOff 
 

End If 
End Sub



Sub SolRightGate(Enabled) 

SetLamp 136, Enabled  

If Enabled Then  

     Flipper3.RotateToEnd  
     L137.State=LightStateON 
    

Else  

  Flipper3.RotateToStart 
  L137.State=LightStateOff 
 

End If 
End Sub





'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

Dim bsTrough, dtL, bsSaucer

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Cybernaut (Bally)"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 0
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

	vpmNudge.TiltSwitch  = swTiltMe
	vpmNudge.Sensitivity = 5
	vpmNudge.Tiltobj = Array(LeftSlingshot,RightSlingshot,Bumper) ' Slings and Pop Bumpers

 
  
' Trough
	Set bsTrough = New cvpmBallStack
		bsTrough.Initsw 0,8,0,0,0,0,0,0
 		bsTrough.InitKick BallRelease,90,25
 		bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsTrough.Balls = 1
 		bsTrough.IsTrough = True
  
' Saucer (Lower Left)
	Set bsSaucer = New cvpmBallStack
		bsSaucer.InitSaucer Saucer1,5,124,15
 		'bsSaucer1.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)


  	
 	Set dtL=New cvpmDropTarget
		dtL.InitDrop Array(sw24,sw23,sw22,sw21),Array(swDropTargetLeft,swDropTargetMidLeft,swDropTargetMidRight,swDropTargetRight)
		dtL.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

' GI init
GITT.Enabled=1



 End Sub
 
'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
 	If keycode = RightMagnaSave Then : controller.switch(8) = True 
 	If keycode = 38 Then LampTest
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
	If keycode = RightMagnaSave Then : controller.switch(8) = False 
End Sub


'**********************************************************************************************************


'*****GI Lights On
dim xx

Sub GITT_Timer()

GITT.Enabled=0
For each xx in GI:xx.State = 1: Next
End Sub


 ' Drain and Kickers

Sub Gate1_Hit:End Sub
Sub Drain_Hit:playsound"drain":bsTrough.addball me:End Sub
Sub Saucer1_Hit:bsSaucer.addball 0: playsound"popper_ball" :End Sub  

Sub Trigger3_Hit:playsound"goner"End Sub
Sub Trigger4_Hit:playsound"goner"End Sub


  ' Collect bonus switch

Sub sw32_Hit()        : controller.switch (swcollectbonusLeft)=1 : playsound"rollover" : : End Sub
Sub sw32_unHit()      : controller.switch (swcollectbonusLeft)=0 :  : End Sub
Sub sw31_Hit()        : controller.switch (swcollectbonusMidLeft)=1 : playsound"rollover" : End Sub
Sub sw31_unHit()      : controller.switch (swcollectbonusMidLeft)=0 : End Sub
Sub sw30_Hit()        : controller.switch (swcollectbonusMidRight)=1 : playsound"rollover" : End Sub
Sub sw30_unHit()      : controller.switch (swcollectbonusMidRight)=0 : End Sub
Sub sw29_Hit()        : controller.switch (swcollectbonusRight)=1 : playsound"rollover" : End Sub
Sub sw29_unHit()      : controller.switch (swcollectbonusRight)=0 : End Sub

  ' Tube switch

Sub sw12_Hit()        : controller.switch (swTube)=1 :playsound"rail_low_slower":End Sub
Sub sw12_unHit()      : controller.switch (swTube)=0 : End Sub



'Wire Triggers
Sub sw14_Hit()   	  : controller.switch (swShooterLane)=1 : playsound"rollover" : End Sub  
Sub sw14_unHit() 	  : controller.switch (swShooterLane)=0:End Sub

Sub sw4_Hit()   	  : controller.switch (swRightOutLane)=1 : playsound"rollover" : End Sub
Sub sw4_unHit() 	  : controller.switch (swRightOutLane)=0:End Sub

Sub sw37_Hit()   	  : controller.switch (swRightReturnLane)=1 : playsound"rollover" : End Sub 
Sub sw37_unHit() 	  : controller.switch (swRightReturnLane)=0:End Sub

Sub sw13_Hit()   	  : controller.switch (swLeftOutLane)=1 : playsound"rollover" : End Sub 
Sub sw13_unHit() 	  : controller.switch (swLeftOutLane)=0:End Sub

Sub sw33_Hit()   	  : controller.switch (swLeftReturnLane)=1 : playsound"rollover" : End Sub 
Sub sw33_unHit() 	  : controller.switch (swLeftReturnLane)=0:End Sub

 'StarRollover

Sub Trigger2_Hit:Controller.Switch(17)=1:Me.TimerEnabled=1:End Sub			'switch17
Sub Trigger2_unHit:Controller.Switch(17)=0:End Sub			'switch17
Sub Trigger2_Timer:Me.TimerEnabled=0::End Sub

'Bumpers
Sub Bumper_Hit : vpmTimer.PulseSw(1) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub

 'Stand Up Targets
 Sub sw34_Hit : vpmTimer.PulseSw(swLeft) : End Sub 
 Sub sw36_Hit : vpmTimer.PulseSw(swRight1): End Sub 
 Sub sw35_Hit : vpmTimer.PulseSw(swRight2) : End Sub 

 Sub sw18_Hit 

     If Primitive_TargetParts4.ObjRoty = 0 Then 
        vpmTimer.PulseSw 18:Primitive_TargetParts4.ObjRotx = 14:Primitive_TargetParts7.ObjRotx = 14:Me.TimerEnabled=1
     
     End If
End Sub

Sub SW18_Timer:Primitive_TargetParts4.ObjRotx=0:Primitive_TargetParts7.ObjRotx = 0:Me.TimerEnabled=0:End Sub


 Sub sw19_Hit 

    If Primitive_TargetParts3.ObjRoty = 0 Then 
        vpmTimer.PulseSw 19:Primitive_TargetParts3.ObjRotx = 14:Primitive_TargetParts6.ObjRotx = 14:Me.TimerEnabled=1
     
     End If
End Sub

Sub SW19_Timer:Primitive_TargetParts3.ObjRotx=0:Primitive_TargetParts6.ObjRotx = 0:Me.TimerEnabled=0:End Sub



 Sub sw20_Hit 
     
     If Primitive_TargetParts2.ObjRoty = 0 Then 
        vpmTimer.PulseSw 20:Primitive_TargetParts2.ObjRotx = 14:Primitive_TargetParts5.ObjRotx = 14:Me.TimerEnabled=1
     
     End If
End Sub

Sub SW20_Timer:Primitive_TargetParts2.ObjRotx=0:Primitive_TargetParts5.ObjRotx = 0:Me.TimerEnabled=0:End Sub




'Drop Targets
 Sub Sw24_Dropped:dtL.Hit 1 :End Sub  
 Sub Sw23_Dropped:dtL.Hit 2 :End Sub  
 Sub Sw22_Dropped:dtL.Hit 3 :End Sub
 Sub Sw21_Dropped:dtL.Hit 4 :End Sub



'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampTst, LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 5 'lamp fading speed
LampTimer.Enabled = 1
LampTst = 0

' Lamp & Flasher Timers

Sub LampTest
	Dim x
	If LampTst = 1 Then
		LampTst = 0
		LampTimer.Enabled = 1
	Else
		LampTimer.Enabled = 0
		For x = 0 to 200
			LampState(x) = 0       ' current light state, independent of the fading level. 0 is off and 1 is on
			FadingLevel(x) = 5      ' used to track the fading state
			FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
		Next
		LampTst = 1
	End If
End Sub

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
NFadeL 1, l1                     ' arrow  yellow
NFadeL 2, L2                     ' special  midleft  red
NFadeL 3, L3                     ' special  midmid   red
NFadeL 4, L4                     ' 5   white
NFadeL 5, L5                     ' 25  white
NFadeL 6, L6                     ' 45  white
NFadeL 7, L7                     ' 50 midmid   white
NFadeL 8, L8                     ' special ( sling right )  red
NFadeL 9, L9                     ' B left inlane  orange
NFadeL 10, L10                   ' T right inlane  orange
NFadeL 11, L11                   ' shoot again   red
NFadeL 12, L12                   ' green 4 left up (collect bonus )   green

NFadeL 14, L14                   ' 500  midmid  green
NFadeL 15, L15                   ' 10 midleft  white 
NFadeL 17, L17                   ' white left in ion gen   white
NFadeL 18, L18                   ' white mid in ion gen    white
NFadeL 82, L82                   ' 2x   yellow
NFadeL 19, L19                   ' white right ion gen   white
NFadeL 20, L20                   ' 10  white
NFadeL 21, L21                   ' 30  white     
NFadeL 22, L22                   ' 50  white yellow
NFadeL 81, L81                   ' L  orange
NFadeL 23, L23                   ' 100  midmid   white
NFadeL 24, L24                   ' 90  thausends   white
NFadeL 25, L25                   ' L  leftmid  orange
NFadeL 28, L28                   ' green 3 left up  ( collect bonus )   green

NFadeL 30, L30                   ' 1000  midmid   green
NFadeL 31, L31                   ' 20 midleft  white
NFadeL 33, L33                   ' yellow left ion gen   yellow
NFadeL 63, L63                   ' 40 midleft  white
NFadeL 65, l65                   ' B  orange
NFadeL 97, l97                   ' A  orange
NFadeL 34, L34             ' yellow mid ion gen  yellow
NFadeL 98, L98             ' 3X yellow
NFadeL 53, l53             '  40  white
NFadeL 57, l57
NFadeL 35, L35             ' yellow right ion gen   yellow
NFadeL 36, L36             '  15   white
NFadeL 37, L37             '  35   white
NFadeL 38, L38             '  100  white yellow
NFadeL 39, L39             '  150 midmid  white
NFadeL 40, L40             '  extra ball (tube)  orange
NFadeL 41, L41             '  A rightmid  orange
NFadeL 42, L42             ' collect bonus light left up    yellow
NFadeL 43, L43             '  50 midleft  white
NFadeL 44, L44             ' green 2 left up ( ion gen )     green

NFadeL 46, L46             '  1500  midmid  green
NFadeL 47, L47             ' 30 midleft   white
NFadeL 60, l60           ' green left up (collect bonus)    green

NFadeL 83, L83           ' 180 ion generator  white
NFadeL 49, L49           ' red left ion gen   red
NFadeL 50, L50              ' red mid ion gen  red
NFadeL 51, L51              ' red right ion gen   red
NFadeL 52, L52              ' 20   white
NFadeL 54, L54              ' 200  white yellow
NFadeL 55, L55              ' 200  midmid   white
NFadeL 56, L56              '  special tube   red
NFadeL 57, L57              '  S  midright  orange
NFadeL 58, L58              '  special left up ( collect bonus )    red
NFadeL 62, L62              ' 2000   midmid   green
NFadeL 113, l113            ' S  orange
NFadeL 66, l66              ' T  orange
NFadeL 67, L67              ' 90 ion generator   white
NFadeL 114, L114                 ' 4x  yellow
NFadeL 115, L115                ' special  ion generator   red

NFadeL 99, L99

'FlashValCollection 96, aGIP,    255   'general illumination collection
'Backglass Lights
'FadeReel 11, L11					' Shoot Again
'FadeReel 13, L13					' Ball In Play
'FadeReel 27, L27					' Match
'FadeReel 29, L29					' High Score
'FadeReel 45, L45					' Game Over
'FadeReel 61, L61					' Tilt


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
	FlashMax(149) = 40
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

Sub FlashOpacity(nr, object, stepup, stepdown) 
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - stepdown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FadingLevel(nr) = 0 'completely off
            End if
            Object.Opacity = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + stepup
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.Opacity = FlashLevel(nr)
    End Select
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



'Bally Cybernaut dipswitchmenu
'added by Inkochnito
Sub EditDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700,400,"Cybernaut - DIP switches"
		.AddChk 7,10,180,Array("Match feature",&H08000000)'dip 28
		.AddChk 205,10,115,Array("Credits display",&H04000000)'dip 27
		.AddFrame 2,30,190,"Maximum credits",&H03000000,Array("10 credits",0,"15 credits",&H01000000,"25 credits",&H02000000,"40 credits",&H03000000)'dip 25&26
		.AddFrame 2,106,190,"Top Rollover Special adjustment",&H00000020,Array("Only 1 Special per game",0,"More than 1 Special per game",&H00000020)'dip 6
		.AddFrame 2,152,190,"Tube Special at",&H000000C0,Array("60,000",&H000000C0,"80,000",&H00000080,"100,000",&H00000040,"120,000",0)'dip 7&8
		.AddFrame 2,228,190,"Bonus Multiplier advance adjustment",&H00002000,Array("Advance every 3rd Tube Shot",0,"Advance every 2nd Tube Shot",&H00002000)'dip 14
		.AddFrame 2,278,190,"Ramp Ion-Generator special adjustment",&H00004000,Array ("special on with 270,000",0,"special on with 180,000",&H00004000)'dip 15
		.AddFrame 2,328,190,"Guardian Drop Targets",32768,Array ("reset after Extra Ball is earned",0,"reset after Ramp Special is earned",32768)'dip 16
		.AddFrame 205,30,190,"Balls per game",&HC0000000,Array ("2 balls",&HC0000000,"3 balls",0,"4 balls",&H80000000,"5 balls",&H40000000)'dip 31&32
		.AddFrame 205,106,190,"Saucer Bonus Multiplier Adjustment",&H00400000,Array("Saucer collects without Multipliers",0,"Saucer collects with Multipliers",&H00400000)'dip 23
		.AddFrame 205,152,190,"B-L-A-S-T Special Adjustment",&H00300000,Array("Special on with 200,000",0,"Special on with 150,000",&H00100000,"Special on with 100,000",&H00200000,"Special on with 50,000",&H00300000)'dip 21&22
		.AddFrame 205,228,190,"Top Rollover Button Special Active",&H00800000,Array("inactive",0,"active",&H00800000)'dip 24
		.AddFrame 205,278,190,"Dip 29",&H10000000,Array("off",0,"on",&H10000000)'dip 29
		.AddFrame 205,328,190,"Cybernaut Bonus Special Adjustment",&H20000000,Array("Special at 395,000",0,"Special at 300,000",&H20000000)'dip 30
		.AddLabel 50,400,300,20,"Set selftest position 17,18 and 19 to 03 for the best gameplay."
		.AddLabel 50,420,300,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub

Set vpmShowDips = GetRef("EditDips")



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
	vpmtimer.pulsesw 3
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
	vpmtimer.pulsesw 2
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
    Vol = Csng(BallVel(ball) ^2 / 400)
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


Sub BallShadowUpdate_Timer()
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
ballShadow(b).opacity = 80
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Wall_Hit (idx)
	PlaySound "glass_hit" , 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

'***********************************************************************************
'****				        	Switch reference      				  		  	****
'***********************************************************************************

Const swShooterLane					= 14
Const swTube                        = 12
Const swTiltMe						= 15
Const swUpperSaucer1				= 17
Const swTopRight				    = 18
Const swTopMid    		     		= 19
Const swTopLeft   		     		= 20
Const swLeft					    = 34
Const swRight1			     		= 35
Const swRight2		    			= 36
Const swDropTargetLeft				= 24
Const swDropTargetMidRight	        = 22
Const swDropTargetMidLeft	        = 23
Const swDropTargetRight             = 21
Const swcollectbonusRight    		= 29
Const swcollectbonusMidRight	    = 30
Const swcollectbonusMidLeft		    = 31
Const swRightOutLane				= 4
Const swRightReturnLane				= 37
Const swLeftOutLane					= 13
Const swLeftReturnLane				= 33
Const swcollectbonusLeft            = 32


'***********************************************************************************
'****				        		DOF reference      				    		****
'***********************************************************************************
'Const dShooterLane					= 200
'Const dBallRelease					= 201
'Const dUpperSaucer3Up				= 202
'Const dUpperSaucer3Down				= 203
'Const dUpperSaucer2					= 204
'Const dUpperSaucer1					= 205
'Const dDropUpperLeft				= 206
'Const dDropUpperMiddle				= 207
'Const dDropUpperRight				= 208
'Const dDropLowerLeft				= 209
'Const dDropLowerMiddle				= 210
'Const dDropLowerRight				= 211









'if Full Screen turn off Backglas Components but keep PF LED displays on
If DesktopMode = True Then
dim xxx
For each xxx in BG:xxx.Visible = 1: Next
else
For each xxx in BG:xxx.Visible = 0: Next
End if


Set LampCallback=GetRef("UpdateMultipleLamps")
 Dim O27,N27,O61,N61,O45,N45,O29,N29,O13,N13
 O27=0:N27=0:O61=0:N61=0:O45=0:N45=0:O29=0:N29=0:O13=0:N13=0
 
 Sub UpdateMultipleLamps
 	N13=Controller.Lamp(13) 'BALL IN PLAY
 	N27=Controller.Lamp(27) 'MATCH
 	N29=Controller.Lamp(29) 'HIGH SCORE
 	N45=Controller.Lamp(45) 'GAME OVER
 	N61=Controller.Lamp(61) 'TILT
 	If N13<>O13 Then
 		If N13 Then
 			EMReel5.SetValue 1
 		Else
 			EMReel5.SetValue 0
 		End If
 	O13=N13
 	End If
 	If N27<>O27 Then
 		If N27 Then
 			EMReel1.SetValue 1
 		Else
 			EMReel1.SetValue 0
 		End If
 	O27=N27
 	End If
 	If N29<>O29 Then
 		If N29 Then
 			EMReel4.SetValue 1
 		Else
 			EMReel4.SetValue 0
 		End If
 	O29=N29
 	End If
 	If N45<>O45 Then
 		If N45 Then
 			EMReel3.SetValue 1
 		Else
 			EMReel3.SetValue 0
 		End If
 	O45=N45
 	End If
 	If N61<>O61 Then
 		If N61 Then
 			EMReel2.SetValue 1
 		Else
 			EMReel2.SetValue 0
 		End If
 	O61=N61
 	End If
 End Sub