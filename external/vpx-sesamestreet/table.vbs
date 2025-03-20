'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'                                                          +
'		            SESAME STREET (Custom)                 +
'				    PLAYBOY (BALLY 1978)                   +
'              First table skin by ninjafu                 +
'              Rebuild by: GoldChicco & HiRez00            +
'                                                          +
'        Original Table Authors: ICPjuggla & gtxjoe        +
'                                                          +
'                                                          +
'      Based on Original VPX table: Gemini by BorgDog      +
'                                                          +
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

'Option Explicit
'Randomize

'+++++++++++++++++++++++
'+     TABLE NOTES     +
'+++++++++++++++++++++++
'
' There and 16 LUT brightness / color levels built into this table.
' Hold down LEFT MAGNASAVE and then press RIGHT MAGNASAVE to adjust / cycle through the different LUT brightness levels.
' The LUT you choose will be automatically saved when you exit the table.

' While table is active during gameplay, you can change the music track by pressing the RIGHT MAGNASAVE.
' You can press the LEFT MAGNASAVE at any time to TURN OFF the current music track completely.
' You can also use the TABLE OPTIONS below to customize when music tracks will play during the game.

'+++++++++++++++++++++++
'+    TABLE OPTIONS    +
'+++++++++++++++++++++++
'-------------------------------------------------------------------------------------------------------------
MusicOption1 = 1	'1 = Gameplay Music ON   0 = Gameplay Music OFF
MusicOption2 = 1	'1 = Random Gameplay Music ON   0 = Random Gameplay Music OFF

'MusicOption1 MUST BE SET TO 1 in order to also use the MusicOption2 Random feature
'-------------------------------------------------------------------------------------------------------------
PlayMusicOnStartup = 1      '1 = Start Music on Table Startup    0 = Start Music Manually
'-------------------------------------------------------------------------------------------------------------
BallApronCard = 3     'Set to Either 3 or 5 depending on which ROM balls setting you are using
'-------------------------------------------------------------------------------------------------------------
InstructionCard = 1    'Set to 1 for Card Rules E   Set to 2 for Card Rules F   Set to 3 for Card Rules G
'-------------------------------------------------------------------------------------------------------------
AlternateFlippers = True	'<--- True enables alternative fippers, False sets fippers back to factory flippers
'--------------------------------------------------------------------------------------------------------------------
AddBallShadow = 1	'1 = Ball Shadows ON  0 = Ball Shawdows OFF - Turn OFF if using Ambient Occlusion
'--------------------------------------------------------------------------------------------------------------------
AddFlipperShadows = 1    '1 = Flipper Shadows ON  0 = Flipper Shawdows OFF - Turn OFF if using Ambient Occlusion
'--------------------------------------------------------------------------------------------------------------------
'+++++++++++++++++++++++
'+  END TABLE OPTIONS  +
'+++++++++++++++++++++++

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="playboyb",UseSolenoids=1,UseLamps=1,UseGI=0,SCoin="coin"

LoadVPM "01000100", "Bally.VBS", 1.2

Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim AlternateFlippers
Dim AddBallShadow
Dim AddFlipperShadows
Dim PlayMusicOnStartup
Dim MusicOption1
Dim MusicOption2
Dim BallApronCard
Dim InstructionCard

Dim DTMode
If DesktopMode = True Then 'Show Desktop components
	For Each DTMode in dt_leds
		DTMode.Visible = True
	Next
    Ramp16.visible=1
    Ramp15.visible=1
    Primitive13.visible=1
Else
	For Each DTMode in dt_leds
        DTMode.Visible = False
    Next
    Ramp16.visible=0
    Ramp15.visible=0
    Primitive13.visible=0
End if

Set LampCallback		= GetRef("UpdateMultipleLamps")


'*************************************************************
'Solenoid Call backs
'*************************************************************

SolCallback(6)  = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(7)  = "bsTrough.SolOut"
SolCallback(8)  = "bsSaucer.SolOut"
SolCallback(9) ="vpmFlasher array(Flasher9,Flasher9a)," 'Light as flasher
SolCallback(10)="vpmFlasher array(Flasher10,Flasher10a)," 'Light as flasher
SolCallback(11)="vpmFlasher array(Flasher11,Flasher11a)," 'Light as flasher
SolCallback(13) = "dtDrop.SolDropUp" 'Drop Targets

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

'Initiate Table
'**********************************************************************************************************
Dim bsTrough, dtDrop, bsSaucer

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Sesame Street (ninjafu custom)"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 1
        .Games(cGameName).Settings.Value("sound")=0
		.PuPHide = 1
         On Error Resume Next
        .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1
	' Nudging
	vpmNudge.TiltSwitch = 7
	vpmNudge.Sensitivity = 2
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingShot)

	Set bsTrough = New cvpmBallStack ' Trough handler
	bsTrough.InitSw 0,8,0,0,0,0,0,0
	bsTrough.InitKick BallRelease,90,5
    bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
	bsTrough.Balls = 1

	Set bsSaucer = New cvpmBallStack
	bsSaucer.InitSaucer sw32, 32, 0, 26
	bsSaucer.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
    bsSaucer.KickAngleVar=30
    bsSaucer.KickForceVar=5

	Set dtDrop = New cvpmDropTarget
	dtDrop.InitDrop Array(sw1,sw2,sw3,sw4,sw5),Array(1,2,3,4,5)
	dtDrop.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

    LoadLUT
    If PlayMusicOnStartup = 1 then
       NextTrack
    End If
End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)

	If keycode = LeftMagnaSave Then bLutActive = True: EndMusic
	If keycode = RightMagnaSave Then 
		If bLutActive Then NextLUT: End If
        If bLutActive = False Then NextTrack: End If
    End If

	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
    If keycode = LeftMagnaSave Then bLutActive = False
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
End Sub

'**********************************************************************************************************

'Drain hole
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub
Sub sw32_Hit : bsSaucer.AddBall 0 : playsound "popper_ball": End Sub

'Drop Targets
Sub sw1_Dropped : dtDrop.Hit 1 : End Sub
Sub sw2_Dropped : dtDrop.Hit 2 : End Sub
Sub sw3_Dropped : dtDrop.Hit 3 : End Sub
Sub sw4_Dropped : dtDrop.Hit 4 : End Sub
Sub sw5_Dropped : dtDrop.Hit 5 : End Sub

'Bumpers
Sub Bumper2_Hit : vpmTimer.PulseSwitch 38, 0, 0 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper1_Hit : vpmTimer.PulseSwitch 39, 0, 0 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper3_Hit : vpmTimer.PulseSwitch 40, 0, 0 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub

'Wire Triggers
Sub sw18_Hit   : Controller.Switch(18) =1 : playsound"rollover" : End Sub 
Sub sw18_UnHit : Controller.Switch(18) =0 : End Sub
Sub sw19_Hit   : Controller.Switch(19) =1 : playsound"rollover" : End Sub 
Sub sw19_UnHit : Controller.Switch(19) =0 : End Sub
Sub sw20_Hit   : Controller.Switch(20) =1 : playsound"rollover" : End Sub 
Sub sw20_UnHit : Controller.Switch(20) =0 : End Sub
Sub sw21_Hit   : Controller.Switch(21) =1 : playsound"rollover" : End Sub 
Sub sw21_UnHit : Controller.Switch(21) =0 : End Sub
Sub sw22_Hit   : Controller.Switch(22) =1 : playsound"rollover" : End Sub 
Sub sw22_UnHit : Controller.Switch(22) =0 : End Sub
Sub sw23_Hit   : Controller.Switch(23) =1 : playsound"rollover" : End Sub 
Sub sw23_UnHit : Controller.Switch(23) =0 : End Sub
Sub sw24_Hit   : Controller.Switch(24) =1 : playsound"rollover" : End Sub 
Sub sw24_UnHit : Controller.Switch(24) =0 : End Sub
Sub sw24a_Hit   : Controller.Switch(24) =1 : playsound"rollover" : End Sub 
Sub sw24a_UnHit : Controller.Switch(24) =0 : End Sub
Sub sw31_Hit   : Controller.Switch(31) =1 : playsound"rollover" : End Sub 
Sub sw31_UnHit : Controller.Switch(31) =0 : End Sub

'StandUp Targets
Sub sw17_Hit:vpmTimer.PulseSw 17:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw25_Hit:vpmTimer.PulseSw 25:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw26_Hit:vpmTimer.PulseSw 26:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw27_Hit:vpmTimer.PulseSw 27:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw28_Hit:vpmTimer.PulseSw 28:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw29_Hit:vpmTimer.PulseSw 29:PlaySound SoundFX("Target",DOFTargets):End Sub


'StarTrigger
Sub sw30_Hit   : Controller.Switch(30) =1 : playsound"rollover" : End Sub 
Sub sw30_UnHit : Controller.Switch(30) =0 : End Sub

'Scoring Rubber
Sub sw33_Slingshot : vpmTimer.pulseSw 33 : End Sub


Set Lights(1)= l1
Set Lights(2)= l2
Set Lights(3)= l3
Set Lights(4)= l4
Set Lights(5)= l5
Set Lights(6)= l6
Set Lights(7)= l7
Set Lights(8)= l8
Set Lights(9)= l9
Set Lights(17)= l17
Set Lights(18)= l18
Set Lights(19)= l19
Set Lights(20)= l20
Set Lights(21)= l21
Set Lights(22)= l22
Set Lights(23)= l23
Set Lights(24)= l24
Set Lights(25)= l25
Set Lights(33)= l33
Set Lights(34)= l34
Set Lights(35)= l35
Set Lights(36)= l36
Set Lights(37)= l37
Set Lights(38)= l38
Set Lights(39)= l39
Set Lights(40)= l40
Set Lights(41)= l41
Set Lights(43)= l43
Set Lights(44)= l44
Set Lights(49)= l49
Set Lights(50)= l50
Set Lights(51)= l51
Set Lights(52)= l52
Set Lights(53)= l53
Set Lights(54)= l54
Set Lights(55)= l55
Set Lights(56)= l56
Set Lights(57)= l57
Set Lights(59)= l59
Set Lights(60)= l60


'************************************
'          LEDs Display
'     Based on Scapino's LEDs
'************************************

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

'Assign 7-digit output to reels
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

Sub LedTimer_Timer()
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

'Desktop Lights / Backdrop + Main GI Lights

Dim DTL
Dim xx

Sub UpdateMultipleLamps

	DTL=Controller.Lamp(13) 'GILights
	If DTL Then
		For each xx in GI:xx.State = 1: Next
        Else
        For each xx in GI:xx.State = 0: Next
	End If

  If DesktopMode = True Then

	DTL=Controller.Lamp(29) 'HS to Date
	If DTL Then
		HighScore_Box.text="HIGH SCORE TO DATE"
	  Else
		HighScore_Box.text=""
	End If

	DTL=Controller.Lamp(61) 'Tilt
	If DTL Then
		Tilt_Box.text="TILT"
	  Else
		Tilt_Box.text=""
	End If

	DTL=Controller.Lamp(11) 'Shoot Again
	If DTL Then
		ShootAgain_Box.text="SHOOT AGAIN"
	  Else
		ShootAgain_Box.text=""
	End If

	DTL=Controller.Lamp(14) '1 Player
	If DTL Then
		OnePlayer_Box.text="1"
	  Else
		OnePlayer_Box.text=""
	End If

	DTL=Controller.Lamp(15) '1 Player Light
	If DTL Then
        pl1.state = 1
	  Else
        pl1.state = 0
	End If


	DTL=Controller.Lamp(30) '2 Player
	If DTL Then
		TwoPlayer_Box.text="2"
	  Else
		TwoPlayer_Box.text=""
	End If

	DTL=Controller.Lamp(31) '2 Player Light
	If DTL Then
        pl2.state = 1
	  Else
        pl2.state = 0
	End If

	DTL=Controller.Lamp(46) '3 Player
	If DTL Then
		ThreePlayer_Box.text="3"
	  Else
		ThreePlayer_Box.text=""
	End If

	DTL=Controller.Lamp(47) '3 Player Light
	If DTL Then
        pl3.state = 1
	  Else
        pl3.state = 0
	End If

	DTL=Controller.Lamp(62) '4 Player
	If DTL Then
		FourPlayer_Box.text="4"
	  Else
		FourPlayer_Box.text=""
	End If

	DTL=Controller.Lamp(63) '4 Player Light
	If DTL Then
        pl4.state = 1
	  Else
        pl4.state = 0
	End If

	DTL=Controller.Lamp(27) 'Match
	If DTL Then
		Match_Box.text="MATCH"
	  Else
		Match_Box.text=""
	End If

	DTL=Controller.Lamp(45) 'Game Over
	If DTL Then
		GameOver_Box.text="GAME OVER"
        BIP_Box.text=""
	  Else
		GameOver_Box.text=""
        BIP_Box.text="BALL IN PLAY"
	End If
   End If
 End Sub


'**************************************
'*         Music Mod: HiRez00         *
'**************************************

Sub NextTrack
    If MusicOption1 = 0 Then Exit Sub 
    If MusicOption1 = 1 and MusicOption2 = 0 then OrderTrack: End If
    If MusicOption2 = 1 and MusicOption1 = 1 then RandomTrack: End If
End Sub


'***********************************************
'*         Playlist Music Mod: HiRez00         *
'***********************************************

Dim musicNum
Sub OrderTrack
If MusicOption1 = 1 then
   If musicNum = 0 Then PlayMusic "SesameStreet/1 Classic Sesame Street Theme Song.mp3" End If
   If musicNum = 1 Then PlayMusic "SesameStreet/2 Sesame Street  Monster in the Mirror original.mp3" End If
   If musicNum = 2 Then PlayMusic "SesameStreet/3 Sesame Street Thats About the Size.mp3" End If
   If musicNum = 3 Then PlayMusic "SesameStreet/4 Sesame Street Cookie Monster Sings C is for Cookie.mp3" End If
   If musicNum = 4 Then PlayMusic "SesameStreet/5 Sesame Street The Lonely N Song.mp3" End If
   If musicNum = 5 Then PlayMusic "SesameStreet/6 Sesame Street Capital I song.mp3" End If
   If musicNum = 6 Then PlayMusic "SesameStreet/7 Sesame Street Ladybugs Picnic.mp3" End If
   If musicNum = 7 Then PlayMusic "SesameStreet/8 Sesame Street  People in Your Neighborhood 77.mp3" End If
   If musicNum = 8 Then PlayMusic "SesameStreet/9 Sesame Street  Pinball Number Count.mp3" End If
   If musicNum = 9 Then PlayMusic "SesameStreet/10 Sesame Street Alligator King.mp3" End If
   If musicNum = 10 Then PlayMusic "SesameStreet/11 Sesame Street  Song Of The Count.mp3" End If
   If musicNum = 11 Then PlayMusic "SesameStreet/12 Sesame Street One Fine Face.mp3" End If
   If musicNum = 12 Then PlayMusic "SesameStreet/13 Big Birds ABC word.mp3" End If
   If musicNum = 13 Then PlayMusic "SesameStreet/14 Sesame Street I Love Trash.mp3" End If
   If musicNum = 14 Then PlayMusic "SesameStreet/15 Sesame Street  Rubber Duckie 1998 version.mp3" End If
   If musicNum = 15 Then PlayMusic "SesameStreet/16 Sesame Street Elmos Song.mp3" End If
   If musicNum = 16 Then PlayMusic "SesameStreet/17 Sesame Street Imagine That With Ernie.mp3" End If
   If musicNum = 17 Then PlayMusic "SesameStreet/18 Sesame Street Batty Bat.mp3" End If
   If musicNum = 18 Then PlayMusic "SesameStreet/19 Sesame Street Bert Dances To Doin The Pigeon.mp3" End If
   If musicNum = 19 Then PlayMusic "SesameStreet/20 I Dont Want to Live on the Moon.mp3" End If
   If musicNum = 20 Then PlayMusic "SesameStreet/21 Kermit  Its not easy being green original.mp3" End If
   If musicNum = 21 Then PlayMusic "SesameStreet/22 L La La La.mp3" End If
   If musicNum = 22 Then PlayMusic "SesameStreet/23 Sesame Street Sing.mp3" End If

    musicNum = (musicNum + 1) mod 23
    
    End If
End Sub

'*********************************************
'*         Random Music Mod: HiRez00         *
'*********************************************

Sub RandomTrack
   Dim m
   m = INT(23 * RND(1) )
   If m = 0 Then PlayMusic "SesameStreet/1 Classic Sesame Street Theme Song.mp3" End If
   If m = 1 Then PlayMusic "SesameStreet/2 Sesame Street  Monster in the Mirror original.mp3" End If
   If m = 2 Then PlayMusic "SesameStreet/3 Sesame Street Thats About the Size.mp3" End If
   If m = 3 Then PlayMusic "SesameStreet/4 Sesame Street Cookie Monster Sings C is for Cookie.mp3" End If
   If m = 4 Then PlayMusic "SesameStreet/5 Sesame Street The Lonely N Song.mp3" End If
   If m = 5 Then PlayMusic "SesameStreet/6 Sesame Street Capital I song.mp3" End If
   If m = 6 Then PlayMusic "SesameStreet/7 Sesame Street Ladybugs Picnic.mp3" End If
   If m = 7 Then PlayMusic "SesameStreet/8 Sesame Street  People in Your Neighborhood 77.mp3" End If
   If m = 8 Then PlayMusic "SesameStreet/9 Sesame Street  Pinball Number Count.mp3" End If
   If m = 9 Then PlayMusic "SesameStreet/10 Sesame Street Alligator King.mp3" End If
   If m = 10 Then PlayMusic "SesameStreet/11 Sesame Street  Song Of The Count.mp3" End If
   If m = 11 Then PlayMusic "SesameStreet/12 Sesame Street One Fine Face.mp3" End If
   If m = 12 Then PlayMusic "SesameStreet/13 Big Birds ABC word.mp3" End If
   If m = 13 Then PlayMusic "SesameStreet/14 Sesame Street I Love Trash.mp3" End If
   If m = 14 Then PlayMusic "SesameStreet/15 Sesame Street  Rubber Duckie 1998 version.mp3" End If
   If m = 15 Then PlayMusic "SesameStreet/16 Sesame Street Elmos Song.mp3" End If
   If m = 16 Then PlayMusic "SesameStreet/17 Sesame Street Imagine That With Ernie.mp3" End If
   If m = 17 Then PlayMusic "SesameStreet/18 Sesame Street Batty Bat.mp3" End If
   If m = 18 Then PlayMusic "SesameStreet/19 Sesame Street Bert Dances To Doin The Pigeon.mp3" End If
   If m = 19 Then PlayMusic "SesameStreet/20 I Dont Want to Live on the Moon.mp3" End If
   If m = 20 Then PlayMusic "SesameStreet/21 Kermit  Its not easy being green original.mp3" End If
   If m = 21 Then PlayMusic "SesameStreet/22 L La La La.mp3" End If
   If m = 22 Then PlayMusic "SesameStreet/23 Sesame Street Sing.mp3" End If
 
   
End Sub

Sub Table1_MusicDone
        NextTrack

End Sub



'**********************************************************************************************************
'**********************************************************************************************************

Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm  370,500,"Playboy - DIP switch settings"
		.AddFrame   2,  5, 115,"Balls per game",32768,Array("3 balls",0,"5 balls",32768)
		.AddFrame   2, 53, 115,"Credits Display",&H00080000,Array("On",&H00080000,"Off",0)
		.AddFrame   2,102, 115,"Match feature",&H00100000,Array("On",&H00100000,"Off",0)
		.AddFrame   2,150, 115,"Drop Target Special",&H00200000,Array("Lit until collected",&H200000,"Lit until ball lost",0)
		.AddFrame   2,198, 115,"5 Keys",&H00400000,Array("Held until made",&H400000,"Not held",0)
		.AddFrame   2,248, 115,"1 && 4 Keys",&H20000000,Array("Tied together",&H20000000,"Not tied",0)
		.AddFrame   2,298, 115,"2 && 3 Keys",&H10000000,Array("Tied together",&H10000000,"Not tied",0)
		.AddFrame   2,348, 115,"Outlanes",&H00800000,Array("Both lit",&H800000,"Alternating",0)
		.AddFrame   2,398, 115,"Sounds-Scoring", &H80, Array("Chime",0,"Noise",&H80)
		.AddFrame   2,448, 115,"Sounds-Coin (no credit)",&H80000000,Array("Chime",0,"Noise",&H80000000)
		.AddFrame 130,  5, 118,"High Score Award",&H00006000,Array("Replay",&H6000,"Extra Ball",&H4000,"No Award",0)
		.AddFrame 130, 75, 118,"Rollover Extra && Special",&H40000000,Array("Hold until made",&H40000000,"Not Held",0)
		.AddFrame 130,130, 118,"High game to date",&H00000060,Array("No award",0,"1 credit",&H20,"2 credits",&H40,"3 credits",&H60)
		.AddFrame 130,211, 118,"Max. credits",&H00070000,Array("5 credits",0,"10 credits",&H10000,"15 credits",&H20000,"20 credits",&H30000,"25 credits",&H40000,"30 credits",&H50000,"35 credits",&H60000,"40 credits",&H70000)
		.AddFrame 130,350, 118,"Coin Slot 2 (Key 3)",&H0F000000,Array("Same as Coin 1",&H0000000,"1/coin",&H1000000,"2/coin",&H2000000,_
							   "3/coin",&H3000000,"4/coin",&H4000000,"5/coin",&H5000000,"6/coin",&H6000000,"7/coin",&H7000000,_
							   "8/coin",&H8000000)
		.AddFrame 260,  5, 90,"Coin Slot 1 (Key 4)",&H000001F,Array("3/2 coins",&H00,"1 coin",&H02,"1/2 coins",&H03,"2/coin",&H04,"2/2 coins",&H05,_
							  "3/coin",&H06,"3/2 coins",&H07,"4/coin",&H08,"4/2 coins",&H09,"5/coin",&H0A,"5/2 coins",&H0B,_
							  "6/coin",&H0C,"6/2 coins",&H0D,"7/coin",&H0E,"7/2 coins",&H0F)
		.AddFrame 260,265, 90,"Coin Slot 3 (Key 5)",&H1F00,Array("3/2 coins",&H0000,"1 coin",&H0200,"1/2 coins",&H0300,"2/coin",&H0400,"2/2 coins",&H05000000,_
							  "3/coin",&H0600,"3/2 coins",&H0700,"4/coin",&H0800,"4/2 coins",&H0900,"5/coin",&H0A00,"5/2 coins",&H0B00,_
							  "6/coin",&H0C00,"6/2 coins",&H0D00,"7/coin",&H0E00,"7/2 coins",&H0F00)
		.ViewDips
	End With
End Sub
Set vpmShowDips = GetRef("editDips")

' *****************************************************************************************************************************
' *****************************************************************************************************************************
 'Start of VPX Callbacks
' *****************************************************************************************************************************
' *****************************************************************************************************************************



'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.pulseSw 36
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
	vpmTimer.pulseSw 37
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


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle


	LFLogo1.RotY = LeftFlipper.CurrentAngle + 180
	LFLogo2.Roty = LeftFlipper.CurrentAngle + 90
	RFLogo1.RotY = RightFlipper.CurrentAngle 
	RFLogo2.RotY = RightFlipper.CurrentAngle +90


End Sub

'*********** FLIPPER SHADOWS OPTION ************

EnableFlipperShadows
Sub EnableFlipperShadows
       if AddFlipperShadows = 1 Then
        FlipperLSh.visible=true
		FLipperRSh.visible=true
       Else
        FlipperLSh.visible=false
		FLipperRSh.visible=false
End If
End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    If AddBallShadow=1 Then
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
    End If
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

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 14: UpdateLUT: SaveLUT: End Sub

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
Case 10: table1.ColorGradeImage = "LUT10"
Case 11: table1.ColorGradeImage = "LUT11"
Case 12: table1.ColorGradeImage = "LUT12"
Case 13: table1.ColorGradeImage = "LUT13"

End Select
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
	PlaySound "gate", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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

Sub Table1_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub



'++++++++++++++++++++++++
'+    OPTION SCRIPTS    +
'++++++++++++++++++++++++

Sub changeballcard
	If BallApronCard = 5 Then
	    BalllCard.Image="5-Balls"
	Else
		apron.Image="3-Balls"
	End If
End Sub

changeinstructcard
Sub changeinstructcard
	If InstructionCard = 1 Then
		InstructCard.Image="Instruct-E"
    End If
	If InstructionCard = 2 Then
	    InstructCard.Image="Instruct-F"
    End If
	If InstructionCard = 3 Then
	    InstructCard.Image="Instruct-G"
    End If
End Sub

changeflippers
Sub changeflippers
	If AlternateFlippers=True Then
		LFLogo1.Image="playboybunny_left"
		RFLogo1.Image="playboybunny_right"
	Else
		LFLogo1.Image="flippers-blank"
		RFLogo1.Image="flippers-blank"
	End If
End Sub
