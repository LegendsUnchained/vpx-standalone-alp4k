Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName = "ali",UseSolenoids=1,UseLamps=0,UseGI=0,UseSync=0,HandleMech=0,SSolenoidOn="fx_Solenoid",SSolenoidOff="fx_solenoidoff",SCoin="fx_coin"
 
LoadVPM "01120100", "stern.vbs", 3.02
 
Dim VarHidden
If Table1.ShowDT = true then
    VarHidden = 1
else
    VarHidden = 0
    For each x in aReels
        x.Visible = 0
    Next
    lrail.Visible = 0
    rrail.Visible = 0
end if
 


'*********************
'   VR Room Toggle
'*********************

dim VRRoom
VRRoom  =  0 '0 - VR Room off, 1 - Minimal VR Room


'*********************
'       VR ROOM 
'*********************
DIM VRObjects

If VRRoom = 1 Then
	for each VRObjects in VRStuff:VRObjects.visible = 1:Next
	VarHidden = 0
	For each x in aReels
        x.Visible = 0
    Next
    lrail.Visible = 0
    rrail.Visible = 0

	
	
Else
	for each VRObjects in VRStuff:VRObjects.visible = 0:Next
End if
   
'*************************************************************
'Solenoid Call backs
'*************************************************************
Solcallback(6) = "vpmsolsound SoundFX(""fx_knocker"",DOFKnocker),"
Solcallback(7) = "SolAliKickers"
SolCallback(8) = "dtTbank.SolDropUp"
SolCallback(9) = "dtLBank.SolDropUp"
Solcallback(10) = "SolRHole"
Solcallback(11) = "bsTrough.SolOut"
SolCallback(19) = "vpmNudge.SolGameOn"

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

'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************
 
Sub SolAliKickers(Enabled)
    If Enabled Then
        AliKiOn
        If bsHole1.Balls Then bsHole1.ExitSol_On
        If bsHole2.Balls Then bsHole2.ExitSol_On
        If bsHole3.Balls Then bsHole3.ExitSol_On
        vpmTimer.AddTimer 200, "AliKiOff"
    End If
End Sub
 
Sub AliKiOn():sw30i.IsDropped = 0:sw31i.IsDropped = 0:sw32i.IsDropped = 0:End Sub
Sub AliKiOff(dummy):sw30i.IsDropped = 1:sw31i.IsDropped = 1:sw32i.IsDropped = 1:End Sub
 
Sub SolRHole(Enabled)
    If Enabled Then
        sw38i.IsDropped = 0
        If bsRHole.Balls Then bsRHole.ExitSol_On
        vpmTimer.AddTimer 200, "RHoleOff"
    End If
End Sub
 
Sub RHoleoff(dummy):sw38i.IsDropped = 1:End Sub
 

'*****GI Lights On
dim xx
For each xx in GI:xx.State = 1: Next


 
'************
' Table init.
'************

Dim bsTrough, dtTBank, dtLBank, bsHole1, bsHole2, bsHole3, bsRHole
Dim x, i, j, k 'used in loops
 
Sub Table1_Init
    With Controller
        .GameName = cGameName
        .SplashInfoLine = "Ali, Stern 1980" & vbNewLine & "VPX table by JPSalas v1.0.1"
        .Games(cGameName).Settings.Value("sound")=0
        .HandleMechanics = 0
        .HandleKeyboard = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .ShowTitle = 0
        .Hidden = 1
        If Err Then MsgBox Err.Description
    End With
    On Error Goto 0
    Controller.SolMask(0) = 0
    vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
    Controller.Run
 
    ' Nudging
    vpmNudge.TiltSwitch = 7
    vpmNudge.Sensitivity = 0.1
    vpmNudge.TiltObj = Array(LBumper, BBumper, RBumper, LeftSlingshot, RightSlingshot)
 
    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        bsTrough.InitNoTrough BallRelease, 33, 115, 3
        .InitExitSnd SoundFX("fx_ballrel",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .Balls = 1
    End With
 
    ' Left Drop targets
    set dtLBank = new cvpmdroptarget
    With dtLBank
        .InitDrop Array(sw22, sw23, sw24, sw17, sw18), Array(22, 23, 24, 17, 18)
        .Initsnd SoundFX("1 Bell",DOFDropTargets), SoundFX("fx_resetdrop",DOFContactors)
    End With
 
    ' Top Drop targets
    set dtTBank = new cvpmdroptarget
    With dtTBank
        .InitDrop Array(sw19, sw20, sw21), Array(19, 20, 21)
        .Initsnd SoundFX("3 Bell",DOFDropTargets), SoundFX("fx_resetdrop",DOFContactors)
    End With
 
    ' Top Eject Hole 1
    Set bsHole1 = New cvpmBallStack
    With bsHole1
        .InitSaucer sw30, 30, 180, 10
        .InitExitSnd SoundFX("fx_kicker",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .KickForceVar = 3
        .KickAngleVar = 3
    End With
 
    ' Top Eject Hole 2
    Set bsHole2 = New cvpmBallStack
    With bsHole2
        .InitSaucer sw31, 31, 180, 10
        .InitExitSnd SoundFX("fx_kicker",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .KickForceVar = 3
        .KickAngleVar = 3
    End With
 
    ' Top Eject Hole 3
    Set bsHole3 = New cvpmBallStack
    With bsHole3
        .InitSaucer sw32, 32, 180, 10
        .InitExitSnd SoundFX("fx_kicker",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .KickForceVar = 3
        .KickAngleVar = 3
    End With
 
    ' Right Eject Hole
    Set bsRHole = New cvpmBallStack
    With bsRHole
        .InitSaucer sw38, 38, 180, 10
        .InitExitSnd SoundFX("fx_kicker",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .KickForceVar = 3
        .KickAngleVar = 3
    End With
 
    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
	
 
    ' Init Bumper Rings and targets
    AliKiOff 0:RHoleoff 0
	
PlayMusic "rocky\Intro.mp3"

	setup_backglass

End Sub
 
'*****************************************************************************************************
' VR PLUNGER ANIMATION
''
'*****************************************************************************************************

Sub TimerPlunger_Timer
  If VR_Primary_plunger.Y < 2249 then
  		VR_Primary_plunger.Y = VR_Primary_plunger.Y + 5
  End If
End Sub

Sub TimerPlunger2_Timer
 'debug.print plunger.position
  VR_Primary_plunger.Y = 2114 + (5* Plunger.Position) -20
End Sub


Sub GILights (enabled)
	Dim light
	For each light in GI:light.State = Enabled: Next
End Sub



'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************
 
Sub table1_KeyDown(ByVal Keycode)
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = LeftMagnaSave Then bLutActive = True
'	If keycode = RightMagnaSave Then
'		If bLutActive Then NextLUT
'		If bLutActive = False Then MusicOn
'		End If
	If keycode = PlungerKey Then 
		Plunger.Pullback
		playsound"plungerpull"
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If
End Sub
 
Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
    If keycode = LeftMagnaSave Then bLutActive = False
	If keycode = PlungerKey Then 
		Plunger.Fire
		PlaySound"plunger"
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		VR_Primary_plunger.Y = 2114
	End If
End Sub

'**********************************************************************************************************
'Rocky Tunes
'**********************************************************************************************************

Dim musicNum

Sub PlayRandomMusic()
    ' Initialize random number generator
    Randomize

    ' Select a random number between 0 and 9
    musicNum = Int(10 * Rnd)

    ' Play the selected track based on the random number
    Select Case musicNum
        Case 0: PlayMusic "rocky\1.mp3"
        Case 1: PlayMusic "rocky\2.mp3"
        Case 2: PlayMusic "rocky\3.mp3"
        Case 3: PlayMusic "rocky\4.mp3"
        Case 4: PlayMusic "rocky\5.mp3"
        Case 5: PlayMusic "rocky\6.mp3"
        Case 6: PlayMusic "rocky\7.mp3"
        Case 7: PlayMusic "rocky\8.mp3"
        Case 8: PlayMusic "rocky\9.mp3"

    End Select
End Sub

Sub Table1_MusicDone()
    ' Play a random track when the current track finishes
    PlayRandomMusic
End Sub

'*****************************
'*       End Music Mod       *
'*****************************
 
'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 15
    PlaySound SoundFX("fx_slingshot",DOFContactors), 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    F103b.State = 1
	F102b.State = 1
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
    F103b.State = 0
	F102b.State = 0
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 16
    PlaySound SoundFX("fx_slingshot",DOFContactors),0,1,-0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    F103a.State = 1
	F102a.State = 1
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
    F103a.State = 0
	F102a.State = 0
End Sub


'*********
' Switches
'*********
 
' Bumpers
Sub LBumper_Hit:vpmTimer.PulseSw 13:PlaySound SoundFX("coup_poing",DOFContactors), 0, 1, 0.15, 0.15:End Sub
Sub RBumper_Hit:vpmTimer.PulseSw 12:PlaySound SoundFX("coup_poing",DOFContactors), 0, 1, 0.15, 0.15:End Sub
Sub BBumper_Hit:vpmTimer.PulseSw 14:PlaySound SoundFX("coup_poing",DOFContactors), 0, 1, 0.15, 0.15:End Sub
Sub LBumper1_Hit:vpmTimer.PulseSw 14:PlaySound SoundFX("coup_poing",DOFContactors), 0, 1, 0.15, 0.15:End Sub
Sub BBumper1_Hit:vpmTimer.PulseSw 14:PlaySound SoundFX("coup_poing",DOFContactors), 0, 1, 0.15, 0.15:End Sub
 
' Drain & holes
Sub Drain_Hit:Playsound "drain":bsTrough.AddBall Me:End Sub
Sub sw30_Hit:bsHole1.AddBall 0:PlaySound "fx_kicker_enter":End Sub
Sub sw31_Hit:bsHole2.AddBall 0:PlaySound "fx_kicker_enter":End Sub
Sub sw32_Hit:bsHole3.AddBall 0:PlaySound "fx_kicker_enter":End Sub
Sub sw38_Hit:bsRHole.AddBall 0:PlaySound "rocky-bonus":End Sub

'Drain
Sub Drain_Hit:PlaySound "Drain":bsTrough.AddBall Me:End Sub
'Kickers

 
' Rollovers
Sub sw35_Hit:Controller.Switch(35) = 1:PlaySound "long tambour", 0, 1, Audiopan(ActiveBall):End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub
 
Sub sw37_Hit:Controller.Switch(37) = 1:PlaySound "fx_sensor", 0, 1, Audiopan(ActiveBall):CheckGREATEST:End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub
 
Sub sw36_Hit:Controller.Switch(36) = 1:PlaySound "fx_sensor", 0, 1, Audiopan(ActiveBall):CheckGREATEST:End Sub
Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub
 
Sub sw34_Hit:Controller.Switch(34) = 1:PlaySound "long tambour", 0, 1, Audiopan(ActiveBall):End Sub
Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub
 
Sub sw39_Hit:Controller.Switch(39) = 1:PlaySound "rocky-bonus", 0, 1, Audiopan(ActiveBall):End Sub
Sub sw39_UnHit:Controller.Switch(39) = 0:End Sub
 
Sub sw5_Hit:Controller.Switch(5) = 1:PlaySound "fx_sensor", 0, 1, Audiopan(ActiveBall):CheckGREATEST:End Sub
Sub sw5_UnHit:Controller.Switch(5) = 0:End Sub
 
Sub sw11_Hit:Controller.Switch(11) = 1:sw11l.State = 1:PlaySound "fx_sensor", 0, 1, Audiopan(ActiveBall):End Sub
Sub sw11_UnHit:Controller.Switch(11) = 0:vpmTimer.AddTimer 250, "sw11l.State=":End Sub
 
Sub sw11a_Hit:Controller.Switch(11) = 1:sw11al.State = 1:PlaySound "fx_sensor", 0, 1, Audiopan(ActiveBall):End Sub
Sub sw11a_UnHit:Controller.Switch(11) = 0:vpmTimer.AddTimer 250, "sw11al.State=":End Sub
 
Sub sw9_Hit:Controller.Switch(9) = 1:sw9l.State = 1:PlaySound "tambour", 0, 1, Audiopan(ActiveBall):End Sub
Sub sw9_UnHit:Controller.Switch(9) = 0:vpmTimer.AddTimer 250, "sw9l.State=":End Sub
 
' Droptargets
Sub sw22_Dropped:dtLBank.hit 1:PlaySound SoundFX("1 Bell",DOFDropTargets), 0, 1, Audiopan(ActiveBall):End Sub
Sub sw23_Dropped:dtLBank.hit 2:PlaySound SoundFX("1 Bell",DOFDropTargets), 0, 1, Audiopan(ActiveBall):End Sub
Sub sw24_Dropped:dtLBank.hit 3:PlaySound SoundFX("1 Bell",DOFDropTargets), 0, 1, Audiopan(ActiveBall):End Sub
Sub sw17_Dropped:dtLBank.hit 4:PlaySound SoundFX("1 Bell",DOFDropTargets), 0, 1, Audiopan(ActiveBall):End Sub
Sub sw18_Dropped:dtLBank.hit 5:PlaySound SoundFX("1 Bell",DOFDropTargets), 0, 1, Audiopan(ActiveBall):End Sub
'Top Droptargets
Sub sw19_Dropped:dtTBank.hit 1:PlaySound SoundFX("3 Bell",DOFDropTargets), 0, 1, Audiopan(ActiveBall):End Sub
Sub sw20_Dropped:dtTBank.hit 2:PlaySound SoundFX("3 Bell",DOFDropTargets), 0, 1, Audiopan(ActiveBall):End Sub
Sub sw21_Dropped:dtTBank.hit 3:PlaySound SoundFX("3 Bell",DOFDropTargets), 0, 1, Audiopan(ActiveBall):End Sub
 
' Targets
Sub sw25_Hit:vpmTimer.PulseSw 25:PlaySound SoundFX("1 Bell",DOFTargets), 0, 1, Audiopan(ActiveBall):CheckGREATEST
	F103a.State = 1
	Me.TimerEnabled = 1
End Sub

Sub sw25_Timer
	F103a.State = 0
	Me.Timerenabled = 0
End Sub

 
Sub sw26_Hit:vpmTimer.PulseSw 26:PlaySound SoundFX("1 Bell",DOFTargets), 0, 1, Audiopan(ActiveBall):CheckGREATEST:End Sub
 
Sub sw27_Hit:vpmTimer.PulseSw 27:PlaySound SoundFX("1 Bell",DOFTargets), 0, 1, Audiopan(ActiveBall):CheckGREATEST:
 	F102a.State = 1
	Me.TimerEnabled = 1
End Sub

Sub sw27_Timer
	F102a.State = 0
	Me.Timerenabled = 0
End Sub

 
Sub sw28_Hit:vpmTimer.PulseSw 28:PlaySound SoundFX("1 Bell",DOFTargets), 0, 1, Audiopan(ActiveBall):CheckGREATEST
    F102a.State = 1
	F103b.State = 1
	Me.TimerEnabled = 1
End Sub

Sub sw28_Timer
    F102a.State = 0
	F103b.State = 0
	Me.Timerenabled = 0
End Sub

 
Sub sw29_Hit:vpmTimer.PulseSw 29:PlaySound "1 Bell", 0, 1, Audiopan(ActiveBall):CheckGREATEST
    F103a.State = 1
	F102b.State = 1
	Me.TimerEnabled = 1
End Sub

Sub sw29_Timer
    F103a.State = 0
	F102b.State = 0
	Me.Timerenabled = 0
End Sub

 
Sub CheckGREATEST
If (l4.State + l51.State +l35.State +l19.State +l3.State +l20.State +l36.State) = 7 Then
    GiEffect
End If
End Sub
 
' Gates
Sub Gate1_Hit():PlaySound "Gate5":End Sub
Sub Gate2_Hit():PlaySound "Gate5":End Sub
' Spinner
Sub Spinner1_Spin:vpmTimer.PulseSw 9:PlaySound "fx_spinner", 0, 1, -0.01:End Sub
 

'************************************
'          LEDs Display
'************************************
 
Dim Digits(28)
 
Set Digits(0) = a0
Set Digits(1) = a1
Set Digits(2) = a2
Set Digits(3) = a3
Set Digits(4) = a4
Set Digits(5) = a5
 
Set Digits(6) = b0
Set Digits(7) = b1
Set Digits(8) = b2
Set Digits(9) = b3
Set Digits(10) = b4
Set Digits(11) = b5
 
Set Digits(12) = c0
Set Digits(13) = c1
Set Digits(14) = c2
Set Digits(15) = c3
Set Digits(16) = c4
Set Digits(17) = c5
 
Set Digits(18) = d0
Set Digits(19) = d1
Set Digits(20) = d2
Set Digits(21) = d3
Set Digits(22) = d4
Set Digits(23) = d5
 
Set Digits(24) = e0
Set Digits(25) = e1
Set Digits(26) = e2
Set Digits(27) = e3
 
Sub UpdateLeds
    '    On Error Resume Next
    Dim ChgLED, num, ii, jj, chg, stat
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED) Then
        For ii = 0 To UBound(ChgLED)
            num = chgLED(ii, 0):chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            Select Case stat
                Case 0:Digits(num).SetValue 0    'empty
                Case 63:Digits(num).SetValue 1   '0
                Case 6:Digits(num).SetValue 2    '1
                Case 91:Digits(num).SetValue 3   '2
                Case 79:Digits(num).SetValue 4   '3
                Case 102:Digits(num).SetValue 5  '4
                Case 109:Digits(num).SetValue 6  '5
                Case 124:Digits(num).SetValue 7  '6
                Case 125:Digits(num).SetValue 7  '6
                Case 252:Digits(num).SetValue 7  '6
                Case 7:Digits(num).SetValue 8    '7
                Case 127:Digits(num).SetValue 9  '8
                Case 103:Digits(num).SetValue 10 '9
                Case 111:Digits(num).SetValue 10 '9
                Case 231:Digits(num).SetValue 10 '9
                Case 128:Digits(num).SetValue 0  'empty
                Case 191:Digits(num).SetValue 1  '0
                Case 832:Digits(num).SetValue 2  '1
                Case 896:Digits(num).SetValue 2  '1
                Case 768:Digits(num).SetValue 2  '1
                Case 134:Digits(num).SetValue 2  '1
                Case 219:Digits(num).SetValue 3  '2
                Case 207:Digits(num).SetValue 4  '3
                Case 230:Digits(num).SetValue 5  '4
                Case 237:Digits(num).SetValue 6  '5
                Case 253:Digits(num).SetValue 7  '6
                Case 135:Digits(num).SetValue 8  '7
                Case 255:Digits(num).SetValue 9  '8
                Case 239:Digits(num).SetValue 10 '9
            End Select
        Next
    End IF
End Sub


'******************************
' Setup VR Integrated Backglass
'******************************

Dim xoff,yoff1, yoff2, yoff3, yoff4, yoff5,zoff,xrot,zscale, xcen,ycen

Sub setup_backglass()

	xoff = -20
	yoff1 = 189 ' this is where you adjust the forward/backward position for player 1 score
	yoff2 = 189 ' this is where you adjust the forward/backward position for player 2 score
	yoff3 = 189 ' this is where you adjust the forward/backward position for player 3 score
	yoff4 = 189 ' this is where you adjust the forward/backward position for player 4 score
	yoff5 = 189	' this is where you adjust the forward/backward position for credits and ball in play
	zoff = 699
	xrot = -90

	center_Digits()

end sub


Sub center_Digits()
	Dim ix, xx, yy, yfact, xfact, xobj

	zscale = 0.0000001

	xcen = (130 /2) - (92 / 2)
	ycen = (780 /2 ) + (203 /2)

	for ix = 0 to 5
		For Each xobj In vrDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff1 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 6 to 11
		For Each xobj In vrDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff2 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 12 to 17
		For Each xobj In vrDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff3 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 18 to 23
		For Each xobj In vrDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff4 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 24 to 27
		For Each xobj In vrDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff5 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

end sub

'********************************************
' Setup VR Integrated LEDS
'********************************************


Dim vrDigits(32)
vrDigits(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6)
vrDigits(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6)
vrDigits(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6)
vrDigits(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6)
vrDigits(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6)
vrDigits(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6)

vrDigits(6) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6)
vrDigits(7) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6)
vrDigits(8) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6)
vrDigits(9) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6)
vrDigits(10) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6)
vrDigits(11) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6)

vrDigits(12) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006)
vrDigits(13) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106)
vrDigits(14) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206)
vrDigits(15) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306)
vrDigits(16) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406)
vrDigits(17) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506)

vrDigits(18) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006)
vrDigits(19) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106)
vrDigits(20) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206)
vrDigits(21) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306)
vrDigits(22) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406)
vrDigits(23) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506)

vrDigits(24) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306)
vrDigits(25) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406)
vrDigits(26) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506)
vrDigits(27) = Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606)

dim DisplayColor
DisplayColor =  RGB(255,40,1)

Sub vrDisplayTimer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
              For Each obj In vrDigits(num)
'                   If chg And 1 Then obj.visible=stat And 1    'if you use the object color for off; turn the display object visible to not visible on the playfield, and uncomment this line out.
				   If chg And 1 Then FadeDisplay obj, stat And 1	
                   chg=chg\2 : stat=stat\2
              Next
        Next
    End If
 End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 12
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 6
	End If
End Sub


Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(Digits)
		if IsArray(vrDigits(x) ) then
			For each obj in vrDigits(x)
				obj.height = obj.height + 18
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

InitDigits

' ******************************************************************************************
'      LAMP CALLBACK for the 6 backglass flasher lamps (not the solenoid conrolled ones)
' ******************************************************************************************

Set LampCallback = GetRef("UpdateMultipleLamps")

Sub UpdateMultipleLamps()
	If Controller.Lamp(13) = 0 Then: FlBGL13.visible=0: else: FlBGL13.visible=1 'Ball In Play
	If Controller.Lamp(29) = 0 Then: FlBGL29.visible=0: else: FlBGL29.visible=1 'Ball In Play
	If Controller.Lamp(63) = 0 Then: FlBGL63.visible=0: else: FlBGL63.visible=1 'Match
	If Controller.Lamp(45) = 0 Then: FlBGL45.visible=0: else: FlBGL45.visible=1 'Game Over
	If Controller.Lamp(61) = 0 Then: FlBGL61.visible=0: else: FlBGL61.visible=1 'Tilt
End Sub



 
 
'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************
 
Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200), FlashRepeat(200)
 
InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 10 'lamp fading speed
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
 
    UpdateLeds
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
    '9
    NFadeL 10, l10
    NFadeL 11, l11
    NFadeL 12, l12
    NFadeL 14, l14
    NFadeL 15, l15
    '16
    NFadeL 17, l17
    NFadeL 18, l18
    NFadeL 19, l19
    NFadeL 20, l20
    NFadeL 21, l21
    NFadeL 22, l22
    NFadeL 23, l23
    '24
    '25
    NFadeL 26, l26
    NFadeL 27, l27
    NFadeL 28, l28
    NFadeL 30, l30
    NFadeL 31, l31
    '32
    NFadeL 33, l33
    NFadeL 34, l34
    NFadeL 35, l35
    NFadeL 36, l36
    NFadeL 37, l37
    NFadeL 38, l38
    NFadeL 39, l39
    '40
    NFadeL 41, l41
    NFadeL 42, l42
    NFadeL 43, l43
    NFadeL 44, l44
    NFadeL 46, l46
    NFadeL 47, l47
    '48
    NFadeL 49, l49
    NFadeL 50, l50
    NFadeL 51, l51
    '52
    NFadeL 53, l53
    NFadeL 54, l54
    NFadeL 55, l55
    '56
    NFadeL 57, l57
    NFadeL 58, l58
    NFadeL 59, l59
    NFadeL 60, l60
    NFadeL 62, l62
 
' backdrop lights
If VarHidden Then
    NFadeT 13, l13, "Highscore"
    NFadeT 29, l29, "Ball in Play"
    NFadeT 45, l45, "Game Over"
    NFadeT 61, l61, "Tilt"
    NFadeT 63, l63, "Match"
End If
End Sub
 
' div lamp subs
 
Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.2   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.1 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
        FlashRepeat(x) = 20     ' how many times the flash repeats
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
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1            'wait
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
            If FlashLevel(nr) <FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr)> FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub
 
Sub Flashm(nr, object) 'multiple flashers, it doesn't change anything, it just follows the main flasher
    Select Case FadingLevel(nr)
        Case 4, 5
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub
 
Sub FlashBlink(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) <FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 0 AND FlashRepeat(nr) Then 'repeat the flash
                FlashRepeat(nr) = FlashRepeat(nr) -1
                If FlashRepeat(nr) Then FadingLevel(nr) = 5
            End If
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr)> FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 1 AND FlashRepeat(nr) Then FadingLevel(nr) = 4
    End Select
End Sub
 
' Desktop Objects: Reels & texts (you may also use lights on the desktop)
 
' Reels
 
Sub FadeR(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.SetValue 0:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.SetValue 2:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1          'wait
        Case 13:object.SetValue 3:FadingLevel(nr) = 0                  'Off
    End Select
End Sub
 
Sub FadeRm(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1
        Case 5:object.SetValue 0
        Case 9:object.SetValue 2
        Case 3:object.SetValue 3
    End Select
End Sub
 
'Texts
 
Sub NFadeT(nr, object, message)
    Select Case FadingLevel(nr)
        Case 4:object.Text = "":FadingLevel(nr) = 0
        Case 5:object.Text = message:FadingLevel(nr) = 1
    End Select
End Sub
 
Sub NFadeTm(nr, object, b)
    Select Case FadingLevel(nr)
        Case 4:object.Text = ""
        Case 5:object.Text = message
    End Select
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
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    For b = 0 to UBound(BOT)
        ' play the rolling sound for each ball
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
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


'*************
'   JP'S LUT
'*************

Dim bLutActive, LUTImage
Sub LoadLUT
Dim x
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 10: UpdateLUT: SaveLUT: End Sub

Sub UpdateLUT
Select Case LutImage
Case 0: table1.ColorGradeImage = "LUT0"
Case 1: table1.ColorGradeImage = "LUT1"
Case 2: table1.ColorGradeImage = "LUT2"
Case 3: table1.ColorGradeImage = "LUT3"
Case 4: table1.ColorGradeImage = "LUT4"
Case 5: table1.ColorGradeImage = "LUT5"
Case 6: table1.ColorGradeImage = "LUT6"
Case 7: table1.ColorGradeImage = "LUT7"
Case 8: table1.ColorGradeImage = "LUT8"
Case 9: table1.ColorGradeImage = "LUT9"

End Select
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	If VrRoom=1 Then vrDisplayTimer
	
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
	PlaySound "Gate5", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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


'Sub trigger001_Hit 
'	Pupevent 802
'End Sub



' Thalamus : Exit in a clean and proper way
Sub Table1_exit()
  Controller.Pause = False
  Controller.Stop
End Sub

Sub kicker001_Hit
	 
	Kicker001.Kick 0, 35, 0
End Sub
