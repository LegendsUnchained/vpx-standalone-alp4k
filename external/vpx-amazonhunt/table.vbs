
' Original version: Fast Draw VP9 by JPSalas October 2009
' This is the VPX version of my table "Amazon Hunt VP9 Batch DT 1.1"
' Script rebuilt by 32assassin
' Timer "RealTime Updates" by JP Salas (GI Light on when inserting coin)
' Uses the ROM from Amazon Hunt, since the original was an Em table.

Option Explicit
Randomize


'Change cGameName to desired rom
'Change LoadVPM to desired system VBS file
'Change bsTrough to desired switch values
'Change FlipperAlwaysOn to 0 to disable test flippers

' Thalamus 2018-07-19
' Added/Updated "Positional Sound Playback Functions" and "Supporting Ball & Sound Functions"
' Changed UseSolenoids=1 to 2
' Fixed unreferenced switches sw50, sw52, sw60 and sw62 - might not be the same in the
' updated version of this table  !!
' Thalamus 2018-11-01 : Improved directional sounds
' !! NOTE : Table not verified yet !!

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 2000    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 2    ' Bumpers volume.
Const VolRol    = 1    ' Rollovers volume.
Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
Const VolRB     = 1    ' Rubber bands volume.
Const VolRH     = 1    ' Rubber hits volume.
Const VolPo     = 1    ' Rubber posts volume.
Const VolPi     = 1    ' Rubber pins volume.
Const VolPlast  = 1    ' Plastics volume.
Const VolTarg   = 1    ' Targets volume.
Const VolWood   = 1    ' Woods volume.
Const VolKick   = 1    ' Kicker volume.
Const VolSpin   = 1.5  ' Spinners volume.
Const VolFlip   = 1    ' Flipper volume.

Const FlippersAlwaysOn = 0 'Enable Flippers for testing

Const Ballsize = 50

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="amazonh",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM "01120100", "sys80.vbs", 3.02
Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components
  Ramp16.visible=1
  Ramp15.visible=1
  PrimWalls.visible=1
Else
  Ramp16.visible=0
  Ramp15.visible=0
  PrimWalls.visible=0
End if

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

 SolCallback(1) = "SolRight"
 SolCallback(2) = "SolLeft"
 SolCallback(5) = "dtLBank.SolDropUp"
 SolCallback(6) = "dtRbank.SolDropUp"
' solcallback(8) =   "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
 SolCallback(8) =    "solknocker" 
 solcallback(9) =   "bsTrough.SolOut"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("fx_Flipperup",DOFContactors),LeftFlipper,VolFlip:LeftFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFContactors),LeftFlipper,VolFlip:LeftFlipper.RotateToStart
     End If
  End Sub

Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("fx_Flipperup",DOFContactors),RightFlipper,VolFlip:RightFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFContactors),RightFlipper,VolFlip:RightFlipper.RotateToStart
     End If
End Sub


Sub UpdateFlipperLogo_Timer
	LFLogo.RotY  = LeftFlipper.CurrentAngle +90
	RFLogo.RotY  = RightFlipper.CurrentAngle +90

End Sub

'**********************************'
' JP's VP10 Timer "RealTime Updates"'
' GI Light on when inserting coin
'*********************************'

Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    GIUpdate
End Sub

Sub GiON
	Dim x
	For each x in aGiLights
		x.State = 1
	Next
End Sub

Sub GiOFF
	Dim x
	For each x in aGiLights
		x.State = 0
	Next
End Sub

Dim OldGiState
OldGiState = -1 'start witht he Gi off'

Sub GIUpdate
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = -1 Then
            GiOff
        Else
            GiOn
        End If
    End If
End Sub


'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

'*****GI Lights On
dim xx
'For each xx in aGiLights:xx.State = 1: Next


 Dim LeftDone, RightDone
 LeftDone=0
 RightDone=0

 Sub SolRight(Enabled)
 LeftDone = ABS(LeftDone -1)
    If Enabled AND LeftDone Then
       dtRBank.Hit 1
       dtRBank.Hit 2
       dtRBank.Hit 4
       dtRBank.Hit 5
    End If
 End Sub

 Sub SolLeft(Enabled)
 RightDone= ABS(RightDone -1)
    If Enabled AND RightDone Then
       dtLBank.Hit 1
       dtLBank.Hit 2
       dtLBank.Hit 4
       dtLBank.Hit 5
    End If
 End Sub

'kickers are controlled by light IDs see line 303

Sub LFKI(Enabled)
     If Enabled Then
		bsLHole.ExitSol_On
     End If
End Sub

Sub RTKI(Enabled)
     If Enabled Then
		bsRHole.ExitSol_On
     End If
End Sub



'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************
 Dim bsTrough, dtRBank, dtLBank, bsLHole, bsRHole

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Amazon Hunt"&chr(13)&"Welcome to the jungle"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
         On Error Resume Next
     End With
     On Error Goto 0
		 Controller.SolMask(0)=0
      vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
		 Controller.Run
	   If Err Then MsgBox Err.Description
	   On Error Goto 0

    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    vpmNudge.TiltSwitch = 57
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3)

    ' Trough
   Set bsTrough = New cvpmBallStack
       bsTrough.InitSw 0, 67, 0, 0, 0, 0, 0, 0
       bsTrough.InitKick BallRelease, 80, 6
       bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
       bsTrough.Balls = 1

    ' Left Drop targets
   set dtLBank = new cvpmdroptarget
       dtLBank.InitDrop Array(sw0, sw10, sw20, sw30, sw40), Array(0, 10, 20, 30, 40)
       dtLBank.Initsnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

    ' Right Drop targets
   set dtRBank = new cvpmdroptarget
       dtRBank.InitDrop Array(sw41, sw31, sw21, sw11, sw1), Array(41, 31, 21, 11, 1)
       dtRBank.Initsnd SoundFX("DTDrop2",DOFContactors),SoundFX("DTReset2",DOFContactors)

    ' Left Eject Hole
   Set bsLHole = New cvpmBallStack
       bsLHole.InitSaucer sw3, 3, 43+int(rnd(1))*4, 16+int(rnd(1))*4
       bsLHole.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

    ' Right Eject Hole
   Set bsRHole = New cvpmBallStack
       bsRHole.InitSaucer sw23, 23, 313+int(rnd(1))*4, 16+int(rnd(1))*4
       bsRHole.InitExitSnd SoundFX("Popper2",DOFContactors), SoundFX("Solenoid2",DOFContactors)

end sub


'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:playsoundat"plungerpull",plunger
	if KeyCode = LeftTiltKey Then Nudge 90, 4
	if KeyCode = RightTiltKey Then Nudge 270, 4
	if KeyCode = CenterTiltKey Then Nudge 0, 4
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySoundAt"plunger",plunger
End Sub

'**********************************************************************************************************

 ' Drain & holes
 Sub Drain_Hit:bsTrough.addball me : PlaySoundAt "drain", Drain : End Sub
 Sub sw3_Hit:bsLHole.AddBall 0 : playsound "popper_ball": End Sub
 Sub sw23_Hit:bsRHole.AddBall 0 : playsound "popper_ball": End Sub

 ' Slings & Rubbers
 Sub sw72a_Hit:vpmTimer.PulseSw 72:PlaySoundAtVol "fx_rubber2",ActiveBall, 1:End Sub
 Sub sw72b_Hit:vpmTimer.PulseSw 72:PlaySoundAtVol "fx_rubber2",ActiveBall, 1:End Sub
 Sub sw70a_Hit:vpmTimer.PulseSw 70:PlaySoundAtVol "fx_rubber2",ActiveBall, 1:End Sub
 Sub sw70b_Hit:vpmTimer.PulseSw 70:PlaySoundAtVol "fx_rubber2",ActiveBall, 1:End Sub

 ' Bumpers
 Sub Bumper1_Hit:vpmTimer.PulseSw 51 : playsound SoundFX("",DOFContactors):RandomSoundBumper: End Sub
 Sub Bumper2_Hit:vpmTimer.PulseSw 61 : playsound SoundFX("",DOFContactors):RandomSoundBumper: End Sub
 Sub Bumper3_Hit:vpmTimer.PulseSw 71 : playsound SoundFX("",DOFContactors):RandomSoundBumper: End Sub

 ' Rollovers
 Sub sw42_Hit:Controller.Switch(42) = 1 : playsoundAtVol"rollover" ,ActiveBall, 1: End Sub
 Sub sw42_UnHit:Controller.Switch(42) = 0:End Sub
 Sub sw2a_Hit:Controller.Switch(2) = 1 : playsoundAtVol"rollover" ,ActiveBall, 1: End Sub
 Sub sw2a_UnHit:Controller.Switch(2) = 0:End Sub
 Sub sw12a_Hit:Controller.Switch(12) = 1 : playsoundAtVol"rollover" ,ActiveBall, 1: End Sub
 Sub sw12a_UnHit:Controller.Switch(12) = 0:End Sub
 Sub sw12b_Hit:Controller.Switch(12) = 1 : playsoundAtVol"rollover" ,ActiveBall, 1: End Sub
 Sub sw12b_UnHit:Controller.Switch(12) = 0:End Sub
 Sub sw22a_Hit:Controller.Switch(22) = 1 : playsoundAtVol"rollover" ,ActiveBall, 1: End Sub
 Sub sw22a_UnHit:Controller.Switch(22) = 0:End Sub
 Sub sw42a_Hit:Controller.Switch(42) = 1 : playsoundAtVol"rollover" ,ActiveBall, 1: End Sub
 Sub sw42a_UnHit:Controller.Switch(42) = 0:End Sub
 Sub sw2b_Hit:Controller.Switch(2) = 1 : playsoundAtVol"rollover" ,ActiveBall, 1: End Sub
 Sub sw2b_UnHit:Controller.Switch(2) = 0:End Sub
 Sub sw32_Hit:Controller.Switch(32) = 1 : playsoundAtVol"rollover" ,ActiveBall, 1: End Sub
 Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub
 Sub sw22b_Hit:Controller.Switch(22) = 1 : playsoundAtVol"rollover" ,ActiveBall, 1: End Sub
 Sub sw22b_UnHit:Controller.Switch(22) = 0:End Sub

 ' Droptargets
 Sub sw0_Dropped:dtLBank.hit 1 :End Sub
 Sub sw10_Dropped:dtLBank.hit 2 :End Sub
 Sub sw20_Dropped:dtLBank.hit 3 :End Sub
 Sub sw30_Dropped:dtLBank.hit 4 :End Sub
 Sub sw40_Dropped:dtLBank.hit 5 :End Sub

 Sub sw1_Dropped:dtRBank.hit 5 :End Sub
 Sub sw11_Dropped:dtRBank.hit 4 :End Sub
 Sub sw21_Dropped:dtRBank.hit 3 :End Sub
 Sub sw31_Dropped:dtRBank.hit 2 :End Sub
 Sub sw41_Dropped:dtRBank.hit 1 :End Sub

 ' Targets
 Sub sw60a_Hit:vpmTimer.PulseSw 60:End Sub
 Sub sw50a_Hit:vpmTimer.PulseSw 50:End Sub
 Sub sw62a_Hit:vpmTimer.PulseSw 62:End Sub
 Sub sw52a_Hit:vpmTimer.PulseSw 52:End Sub


  '************************
 '      Check Bonus
 '************************

 Dim BonusLamps, OldState
 BonusLamps = Array(32,33,34,35,36,37,38,39,40,41) 'bonus lights
 PlayBonus.Enabled = 0

 Sub PlayBonus_Timer()
    Dim lamp, ii, state
    ii = 0
    state = 0
    For each lamp in BonusLamps
       If LampState(lamp) Then state = state + 2 ^ii
       ii = ii + 1
    Next
    If(OldState <> state) Then PlaySound "Bonus"
    OldState = state
    If (state = 1 and LampState(42) = 0) Or LampState(11)=1 Then PlayBonus.Enabled = 0
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
			If chgLamp(ii,0) = 12 Then LFKI chgLamp(ii,1)
			If chgLamp(ii,0) = 13 Then RTKI chgLamp(ii,1)

        Next
    End If
    UpdateLamps
End Sub

 Sub UpdateLamps
    'NFadeT 1, l1 'BG Tilt
    NFadeL 3, l3 ' PF and BG Same Player Shoot Again
    'NFadeT 10, l10 'BG High score
    'NFadeT 11, l11 'BG GAME OVER
    'NFadeL 12, l2   'Left  Kicker controller
   ' NFadeL 13, l3   'Right  Kicker controller
    NFadeL 14, l14b
    NFadeL 15, l15b
    NFadeL 16, l16b
    NFadeL 17, l17b
    NFadeL 18, l18 'Middle Bumper
    NFadeL 19, l19 'Left Bumper
    NFadeL 20, l20 'Right Bumper
    NFadeL 21, l21b
    NFadeL 22, l22b
    NFadeL 23, l23b
    NFadeL 24, l24b
    NFadeL 25, l25b
    NFadeL 26, l26b
    NFadeL 27, l27b
    NFadeL 28, l28b
    NFadeL 29, l29b
    NFadeL 30, l30b
    NFadeL 31, l31b
    NFadeL 32, l32b
    NFadeL 33, l33b
    NFadeL 34, l34b
    NFadeL 35, l35b
    NFadeL 36, l36b
    NFadeL 37, l37b
    NFadeL 38, l38b
    NFadeL 39, l39b
    NFadeL 40, l40b
    NFadeL 41, l41b
    NFadeL 42, l42b
    NFadeL 43, l43b
    NFadeL 44, l44a
    NFadeL 45, l45b
    NFadeL 46, l46b
    NFadeL 47, l47b
    NFadeL 48, l48b
    NFadeL 49, l49b
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

'Gottlieb Amazon Hunt
 Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700,400,"Amazon Hunt - DIP switches"
		.AddFrame 2,5,190,"Maximum credits",49152,Array("8 credits",0,"10 credits",32768,"15 credits",&H00004000,"25 credits",49152)'dip 15&16
		.AddFrame 2,81,190,"Coin chute 1 and 2 control",&H00002000,Array("seperate",0,"same",&H00002000)'dip 14
		.AddFrame 2,127,190,"Playfield special",&H00200000,Array("replay",0,"extra ball",&H00200000)'dip 22
		.AddFrame 2,173,190,"3rd coin chute credits control",&H20000000,Array("no effect",0,"add 9",&H20000000)'dip 30
		.AddFrameExtra 2,219,190,"Attract tune",&H0200,Array("no attract tune",0,"attract tune played every 6 minutes",&H0200)'S-board dip 2
		.AddChk 2,270,190,Array("Match feature",&H02000000)'dip 26
		'.AddChkExtra 2,290,190,Array("Background sound off",&H0100)'S-board dip 1
		.AddFrame 205,5,190,"High game to date awards",&H00C00000,Array("not displayed and no award",0,"displayed and no award",&H00800000,"displayed and 2 credits",&H00400000,"displayed and 3 credits",&H00C00000)'dip 23&24
		.AddFrame 205,81,190,"Balls per game",&H01000000,Array("5 balls",0,"3 balls",&H01000000)'dip 25
		.AddFrame 205,127,190,"Replay limit",&H04000000,Array("no limit",0,"one per game",&H04000000)'dip 27
		.AddFrame 205,173,190,"Novelty mode",&H08000000,Array("normal game mode",0,"50,000 points per special/extra ball",&H08000000)'dip 28
		.AddFrame 205,219,190,"Game mode",&H10000000,Array("replay",0,"extra ball",&H10000000)'dip 29
		.AddChk 205,270,190,Array("Background sound",&H40000000)'dip 31
		.AddLabel 50,300,300,20,"After hitting OK, press F3 to reset game with new settings."
	End With
	Dim extra
	extra = Controller.Dip(4) + Controller.Dip(5)*256
	extra = vpmDips.ViewDipsExtra(extra)
	Controller.Dip(4) = extra And 255
	Controller.Dip(5) = (extra And 65280)\256 And 255
End Sub
Set vpmShowDips = GetRef("editDips")


'**********************************************************************************************************
'**********************************************************************************************************
'	Start of VPX functions
'**********************************************************************************************************
'**********************************************************************************************************



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
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*VolPi, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall)*VolTarg, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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
	PlaySound "gate", 0, Vol(ActiveBall)*VolGates, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySoundAtVol "fx_spinner", Spinner, VolSpin
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolPo, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundBumper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "fx_bumper1", 0, VolBump, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "fx_bumper2", 0, VolBump, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "fx_bumper3", 0, VolBump, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 4 : PlaySound "fx_bumper4", 0, VolBump, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
' PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
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

Sub PlaySoundAtVol(sound, tableobj, Volum)
  PlaySound sound, 1, Volum, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
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

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioFade(ball) ' Can this be together with the above function ?
  Dim tmp
  tmp = ball.y * 2 / Table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
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
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub SolKnocker(Enabled)
    If Enabled Then PlaySoundAt SoundFX("knocker",DOFKnocker), Plunger
End Sub

' Thalamus : Exit in a clean and proper way
Sub Table1_exit()
  Controller.Pause = False
  Controller.Stop
End Sub

