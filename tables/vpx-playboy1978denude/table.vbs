'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'                                                          +
'		            PLAYBOY (BALLY 1978)                   +
'         New VPX 10.7 Build by: HiRez00 & Apophis         +
'                                                          +
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Option Explicit
Randomize

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

'----- Music Options -----
MusicOption1 = 0				'1 = Gameplay Music ON   0 = Gameplay Music OFF
MusicOption2 = 0				'1 = Random Gameplay Music ON   0 = Random Gameplay Music OFF
MusicVol     = 1				'Level of music volume. Value between 0 and 1

'MusicOption1 MUST BE SET TO 1 in order to also use the MusicOption2 Random feature
PlayMusicOnStartup = 0      	'1 = Start Music on Table Startup    0 = Start Music Manually

'----- GI Color Options ----- 
colornow = 1    				'1 = Incodecent Yellow, 2 = Pink, 3 = White

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1	'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1	'0 = Static shadow under ball ("flasher" image, like JP's), 1 = flasher image shadow, but it moves like ninuzzu's

'----- General Sound Options -----
Const VolumeDial = 0.5			'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.4		'Level of ball rolling volume. Value between 0 and 1

'----- Other Options -----
BallApronCard = 3         		'Set to Either 3 or 5 depending on which ROM balls setting you are using
InstructionCard = 1    			'Set to 1 for Card Rules E   Set to 2 for Card Rules F   Set to 3 for Card Rules G
AlternateFlippers = True		'<--- True enables alternative fippers, False sets fippers back to factory flippers

'----- Cheat Posts -----
CheatPosts = 0					'1 = Cheat Posts in outer lanes ON, 0 = Cheat Posts in outer lanes OFF

'NOTE: BEFORE YOU START A GAME OR PRESS THE START BUTTON - YOU CAN PRESS THE 2 (BUY-IN KEY) TO ACCESS THE SPECIAL DMD OPTIONS MENU.
'THIS MENU WILL ALLOW YOU TO MAKE CHANGES TO MANY OF THE OPTIONS ABOVE ON AFTER THE TABLE HAS LOADED - BUT WILL NOT PERMENANTLY SAVE THEM. 
'YOU CAN ONLY MAKE PERMENANT SETTINGS BY MAKING CHANGES TO THE VALUES ABOVE THAT WILL DETERMINE HOW THE TABLE WILL LOAD ALL THE TIME.

'+++++++++++++++++++++++
'+  END TABLE OPTIONS  +
'+++++++++++++++++++++++


Const BallSize = 50
Const BallMass = 1

Const tnob = 1
Const lob = 0
Dim tablewidth
Dim colornow, CDMD
Dim DMDMenuON
Dim Options
Dim MusicVol
Dim MusicVolume
Dim FirstLook
MusicVolume=10
MusicVolume=10
DMDMenuON=0
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="playboyb",UseSolenoids=2,UseLamps=1,UseGI=0,SCoin=""

LoadVPM "01000100", "Bally.VBS", 1.2

Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim AlternateFlippers
Dim PlayMusicOnStartup
Dim MusicOption1
Dim MusicOption2
Dim BallApronCard
Dim InstructionCard
Dim CheatPosts

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
    For Each DTMode in Desktop
        DTMode.Visible = False
    Next
    Ramp16.visible=0
    Ramp15.visible=0
    Primitive13.visible=0
End if

Set LampCallback		= GetRef("UpdateMultipleLamps")


'*******************************************
'  Timers
'*******************************************

Sub GameTimer_Timer() 'The game timer interval; should be 10 ms
	Cor.Update	  'update ball tracking (this sometimes goes in the RDampen_Timer sub)
	RollingUpdate   'update rolling sounds
	DoDTAnim		'handle drop target animations
	DoSTAnim		'handle stand up target animations
End Sub

Dim FrameTime, InitFrameTime
InitFrameTime = 0
Sub FrameTimer_Timer() 'The frame timer interval should be -1, so executes at the display frame rate
	FrameTime = gametime - InitFrameTime
	InitFrameTime = gametime	'Count frametime
	FlipperVisualUpdate		 'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub



'*************************************************************
'Solenoid Call backs
'*************************************************************

SolCallback(6)  = "vpmSolSound SoundFX(""Knocker_1"",DOFKnocker),"
SolCallback(7)  = "bsTrough.SolOut"
SolCallback(8)  = "bsSaucer.SolOut"
SolCallback(9)  = "" 'Light as flasher
SolCallback(10) = "" 'Light as flasher
SolCallback(11) = "" 'Light as flasher
SolCallback(13) = "dtDropSolDropUp" 'Drop Targets

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"


Sub dtDropSolDropUp(Enabled)
	If Enabled Then
		DTRaise 1
		DTRaise 2
		DTRaise 3
		DTRaise 4
		DTRaise 5
		RandomSoundDropTargetReset sw1p
		RandomSoundDropTargetReset sw5p
		' Make drop target shadows visible
		For Each xx In ShadowDT
			xx.visible = True
		Next
	End If
End Sub

'*******************************************
'  Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
		
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper
		Else
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
	Else
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled) 'Right flipper solenoid callback
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend
		
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub



'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************
Dim bsTrough, bsSaucer

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Playboy Bally 1978"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 1
        .Games(cGameName).Settings.Value("sound")=1
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
    bsTrough.InitExitSnd SoundFX("BallRelease1",DOFContactors), SoundFX("Saucer_Empty",DOFContactors)
	bsTrough.Balls = 1

	Set bsSaucer = New cvpmBallStack
	bsSaucer.InitSaucer sw32, 32, 0, 30
	bsSaucer.InitExitSnd SoundFX("Saucer_Kick",DOFContactors), SoundFX("Saucer_Empty",DOFContactors)
    bsSaucer.KickAngleVar=2
    bsSaucer.KickForceVar=4
    LoadLUT
    CheatAdd
    colorgi
    If PlayMusicOnStartup = 1 then NextTrack: End If
    If CheatPosts Then	
			zCol_Rubber_Cheat1.collidable = True
			Rubber_Cheat1.visible = True
			PegMetal_Cheat1.visible = True
			zCol_Rubber_Cheat2.collidable = True
			Rubber_Cheat2.visible = True
			PegMetal_Cheat2.visible = True
		Else
			zCol_Rubber_Cheat1.collidable = False
			Rubber_Cheat1.visible = False
			PegMetal_Cheat1.visible = False
			zCol_Rubber_Cheat2.collidable = False
			Rubber_Cheat2.visible = False
			PegMetal_Cheat2.visible = False
	 End If

	For Each xx In ShadowDT
		xx.visible = False
	Next

End Sub



Sub Table1_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)

	If keycode = LeftFlipperKey and DMDMenuON=1 then DMDMenuSelect
	If keycode = RightFlipperKey and DMDMenuON=1 then DMDMenu
	If keycode = LeftFlipperKey and DMDMenuON=0 Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey and DMDMenuON=0 Then FlipperActivate RightFlipper, RFPress
	If keycode = LeftMagnaSave Then bLutActive = True: EndMusic
	If keycode = RightMagnaSave Then 
		If bLutActive Then NextLUT: End If
        If bLutActive = False Then NextTrack: End If
    End If
	If keycode = LeftTiltKey Then
		Nudge 90, 1
		SoundNudgeLeft
	End If
	If keycode = RightTiltKey Then
		Nudge 270, 1
		SoundNudgeRight
	End If
	If keycode = CenterTiltKey Then
		Nudge 0, 1
		SoundNudgeCenter
	End If
    If keycode = StartGameKey and DMDMenuON=1 Then Exit Sub: End If
	If keycode = StartGameKey and DMDMenuON=0 Then SoundStartButton: End If
	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then 
		Select Case Int(Rnd * 3)
			Case 0
			PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1
			PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2
			PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
	If keycode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull
	End If
    If keycode = 3 then
         If DMDMenuON=1 then CloseDMDMenu: Exit Sub
         If DMDMenuON=0 and Controller.lamp(45) = True Then Options=0: DMDMenuON=1: DMDMenu: Exit Sub
    End If

	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
    If keycode = LeftMagnaSave Then bLutActive = False
	If KeyCode = PlungerKey Then
		Plunger.Fire
		SoundPlungerReleaseBall()   'Plunger release sound when there is a ball in shooter lane
	End If
	If KeyUpHandler(keycode) Then Exit Sub
End Sub

'**********************************************************************************************************

'Drain hole
Sub Drain_Hit:bsTrough.addball Drain : RandomSoundDrain Drain : End Sub
Sub sw32_Hit : bsSaucer.AddBall 0 : SoundSaucerLock: End Sub

'Drop Targets
Sub sw1_hit : DTHit 1 : End Sub
Sub sw2_hit : DTHit 2 : End Sub
Sub sw3_hit : DTHit 3 : End Sub
Sub sw4_hit : DTHit 4 : End Sub
Sub sw5_hit : DTHit 5 : End Sub

'Bumpers
Sub Bumper2_Hit : vpmTimer.PulseSwitch 38, 0, 0 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper1_Hit : vpmTimer.PulseSwitch 39, 0, 0 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper3_Hit : vpmTimer.PulseSwitch 40, 0, 0 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub

'Wire Triggers
Sub sw18_Hit   : Controller.Switch(18) =1 : End Sub 
Sub sw18_UnHit : Controller.Switch(18) =0 : End Sub
Sub sw19_Hit   : Controller.Switch(19) =1 : End Sub 
Sub sw19_UnHit : Controller.Switch(19) =0 : End Sub
Sub sw20_Hit   : Controller.Switch(20) =1 : End Sub 
Sub sw20_UnHit : Controller.Switch(20) =0 : End Sub
Sub sw21_Hit   : Controller.Switch(21) =1 : End Sub 
Sub sw21_UnHit : Controller.Switch(21) =0 : End Sub
Sub sw22_Hit   : Controller.Switch(22) =1 : End Sub 
Sub sw22_UnHit : Controller.Switch(22) =0 : End Sub
Sub sw23_Hit   : Controller.Switch(23) =1 : End Sub 
Sub sw23_UnHit : Controller.Switch(23) =0 : End Sub
Sub sw24_Hit   : Controller.Switch(24) =1 : End Sub 
Sub sw24_UnHit : Controller.Switch(24) =0 : End Sub
Sub sw24a_Hit   : Controller.Switch(24) =1 : End Sub 
Sub sw24a_UnHit : Controller.Switch(24) =0 : End Sub
Sub sw31_Hit   : Controller.Switch(31) =1 : End Sub 
Sub sw31_UnHit : Controller.Switch(31) =0 : End Sub

'StandUp Targets
Sub sw17_Hit: STHit 17 :End Sub
Sub sw25_Hit: STHit 25 :End Sub
Sub sw26_Hit: STHit 26 :End Sub
Sub sw27_Hit: STHit 27 :End Sub
Sub sw28_Hit: STHit 28 :End Sub
Sub sw29_Hit: STHit 29 :End Sub


'StarTrigger
Sub sw30_Hit   : Controller.Switch(30) =1 : End Sub 
Sub sw30_UnHit : Controller.Switch(30) =0 : End Sub

'Scoring Rubber
Sub sw33_Hit : vpmTimer.pulseSw 33 : End Sub


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
		For each xx in GI_a:xx.State = 1: Flasher_S_ON.visible=1:Flasher_S_OFF.visible=0: Next
        For each xx in GI_b:xx.State = 1: Next
        For each xx in GI_Extra:xx.State = 1: Next
		' Turn on DT shadow if DT is not dropped
		Dim ind : ind=0
		For Each xx In ShadowDT
			If DTArray(ind).isDropped = False Then 
				xx.visible = True
			End If
			ind = ind + 1
		Next
    Else
        For each xx in GI_a:xx.State = 0: Flasher_S_OFF.visible=1:Flasher_S_ON.visible=0: Next
        For each xx in GI_b:xx.State = 0: Next
        For each xx in GI_Extra:xx.State = 0: Next   
		For Each xx In ShadowDT:xx.visible = False: Next    
	End If

  If DesktopMode = True Then

	DTL=Controller.Lamp(29) 'HS to Date
	If DTL Then
		DT_High_Score.visible=True
	  Else
		DT_High_Score.visible=False
	End If

	DTL=Controller.Lamp(61) 'Tilt
	If DTL Then
		DT_Tilt.visible=True
	  Else
		DT_Tilt.visible=False
	End If

	DTL=Controller.Lamp(11) 'Shoot Again
	If DTL Then
		DT_Shoot_Again.visible=True
	  Else
		DT_Shoot_Again.visible=False
	End If

	DTL=Controller.Lamp(14) '1 Player
	If DTL Then
		DT_Can_Play_1.visible=True
	  Else
		DT_Can_Play_1.visible=False
	End If

	DTL=Controller.Lamp(15) '1 Player Light
	If DTL Then
        pl1.state = 1
	  Else
        pl1.state = 0
	End If


	DTL=Controller.Lamp(30) '2 Player
	If DTL Then
		DT_Can_Play_2.visible=True
	  Else
		DT_Can_Play_2.visible=False
	End If

	DTL=Controller.Lamp(31) '2 Player Light
	If DTL Then
        pl2.state = 1
	  Else
        pl2.state = 0
	End If

	DTL=Controller.Lamp(46) '3 Player
	If DTL Then
		DT_Can_Play_3.visible=True
	  Else
		DT_Can_Play_3.visible=False
	End If

	DTL=Controller.Lamp(47) '3 Player Light
	If DTL Then
        pl3.state = 1
	  Else
        pl3.state = 0
	End If

	DTL=Controller.Lamp(62) '4 Player
	If DTL Then
		DT_Can_Play_4.visible=True
	  Else
		DT_Can_Play_4.visible=False
	End If

	DTL=Controller.Lamp(63) '4 Player Light
	If DTL Then
        pl4.state = 1
	  Else
        pl4.state = 0
	End If

	DTL=Controller.Lamp(27) 'Match
	If DTL Then
		DT_Match.visible=True
	  Else
		DT_Match.visible=False
	End If

	DTL=Controller.Lamp(45) 'Game Over
	If DTL Then
        DT_Ball.visible=False
        DT_Game_Over.visible=True

	  Else
		DT_Game_Over.visible=False
        DT_Ball.visible=True
	End If
   End If
 End Sub


'******************************************************
'****  ADD/REMOVE CHEAT POSTS
'******************************************************

Sub CheatAdd
    If CheatPosts > 1 then CheatPosts=0
    If CheatPosts = 1 Then	
			zCol_Rubber_Cheat1.collidable = True
			Rubber_Cheat1.visible = True
			PegMetal_Cheat1.visible = True
			zCol_Rubber_Cheat2.collidable = True
			Rubber_Cheat2.visible = True
			PegMetal_Cheat2.visible = True
     		zCol_Rubber_Cheat3.collidable = True
			Rubber_Cheat3.visible = True
			PegMetal_Cheat3.visible = True
			zCol_Rubber_Cheat4.collidable = True
			Rubber_Cheat4.visible = True
			PegMetal_Cheat4.visible = True
            CheatPosts=1
		Else
			zCol_Rubber_Cheat1.collidable = False
			Rubber_Cheat1.visible = False
			PegMetal_Cheat1.visible = False
			zCol_Rubber_Cheat2.collidable = False
			Rubber_Cheat2.visible = False
			PegMetal_Cheat2.visible = False
     		zCol_Rubber_Cheat3.collidable = False
			Rubber_Cheat3.visible = False
			PegMetal_Cheat3.visible = False
			zCol_Rubber_Cheat4.collidable = False
			Rubber_Cheat4.visible = False
			PegMetal_Cheat4.visible = False
            CheatPosts=0
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
    MPlayer
    musicNum = (musicNum + 1) mod 86
    End If
End Sub


Sub MPlayer
   If musicNum = 0 Then PlayMusic "playboy78/01 - Stayin' Alive.mp3",MusicVol: End If
   If musicNum = 1 Then PlayMusic "playboy78/02 - Lonely Is The Night.mp3",MusicVol: End If
   If musicNum = 2 Then PlayMusic "playboy78/03 - Werewolves Of London.mp3",MusicVol: End If
   If musicNum = 3 Then PlayMusic "playboy78/04 - Moving In Stereo.mp3",MusicVol: End If
   If musicNum = 4 Then PlayMusic "playboy78/05 - Life In the Fast Lane.mp3",MusicVol: End If
   If musicNum = 5 Then PlayMusic "playboy78/06 - Hot Blooded.mp3",MusicVol: End If
   If musicNum = 6 Then PlayMusic "playboy78/07 - Simply Irresistible.mp3",MusicVol: End If
   If musicNum = 7 Then PlayMusic "playboy78/08 - Another One Bites the Dust.mp3",MusicVol: End If
   If musicNum = 8 Then PlayMusic "playboy78/09 - The Stroke.mp3",MusicVol: End If
   If musicNum = 9 Then PlayMusic "playboy78/10 - Shake It Up.mp3",MusicVol: End If
   If musicNum = 10 Then PlayMusic "playboy78/11 - Hold On! I'm Comin'.mp3",MusicVol: End If
   If musicNum = 11 Then PlayMusic "playboy78/12 - Disco Inferno.mp3",MusicVol: End If
   If musicNum = 12 Then PlayMusic "playboy78/13 - More Than A Feeling.mp3",MusicVol: End If
   If musicNum = 13 Then PlayMusic "playboy78/14 - Magic Man.mp3",MusicVol: End If
   If musicNum = 14 Then PlayMusic "playboy78/15 - Old Time Rock And Roll.mp3",MusicVol: End If
   If musicNum = 15 Then PlayMusic "playboy78/16 - All Fired Up.mp3",MusicVol: End If
   If musicNum = 16 Then PlayMusic "playboy78/17 - LA Woman.mp3",MusicVol: End If
   If musicNum = 17 Then PlayMusic "playboy78/18 - Hungry Like the Wolf.mp3",MusicVol: End If
   If musicNum = 18 Then PlayMusic "playboy78/19 - My Best Friend's Girl.mp3",MusicVol: End If
   If musicNum = 19 Then PlayMusic "playboy78/20 - St. Elmos Fire.mp3",MusicVol: End If
   If musicNum = 20 Then PlayMusic "playboy78/21 - Gimme Three Steps.mp3",MusicVol: End If
   If musicNum = 21 Then PlayMusic "playboy78/22 - Any Way You Want It.mp3",MusicVol: End If
   If musicNum = 22 Then PlayMusic "playboy78/23 - Rock N' Me.mp3",MusicVol: End If
   If musicNum = 23 Then PlayMusic "playboy78/24 - More Than A Woman.mp3",MusicVol: End If
   If musicNum = 24 Then PlayMusic "playboy78/25 - Push It To The Limit.mp3",MusicVol: End If
   If musicNum = 25 Then PlayMusic "playboy78/26 - Rocket Man.mp3",MusicVol: End If
   If musicNum = 26 Then PlayMusic "playboy78/27 - Rock In America.mp3",MusicVol: End If
   If musicNum = 27 Then PlayMusic "playboy78/28 - Boogie Shoes.mp3",MusicVol: End If
   If musicNum = 28 Then PlayMusic "playboy78/29 - Rock Me Tonite.mp3",MusicVol: End If
   If musicNum = 29 Then PlayMusic "playboy78/30 - Freeze-Frame.mp3",MusicVol: End If
   If musicNum = 30 Then PlayMusic "playboy78/31 - Break On Through.mp3",MusicVol: End If
   If musicNum = 31 Then PlayMusic "playboy78/32 - Y.M.C.A.mp3",MusicVol: End If
   If musicNum = 32 Then PlayMusic "playboy78/33 - She's On Fire.mp3",MusicVol: End If
   If musicNum = 33 Then PlayMusic "playboy78/34 - Her Strut.mp3",MusicVol: End If
   If musicNum = 34 Then PlayMusic "playboy78/35 - Just What I Needed.mp3",MusicVol: End If
   If musicNum = 35 Then PlayMusic "playboy78/36 - Take Me Home Tonight.mp3",MusicVol: End If
   If musicNum = 36 Then PlayMusic "playboy78/37 - Invincible.mp3",MusicVol: End If
   If musicNum = 37 Then PlayMusic "playboy78/38 - Rush Rush.mp3",MusicVol: End If
   If musicNum = 38 Then PlayMusic "playboy78/39 - Hollywood Nights.mp3",MusicVol: End If
   If musicNum = 39 Then PlayMusic "playboy78/40 - Night Fever.mp3",MusicVol: End If
   If musicNum = 40 Then PlayMusic "playboy78/41 - You Really Got Me.mp3",MusicVol: End If
   If musicNum = 41 Then PlayMusic "playboy78/42 - Only The Young.mp3",MusicVol: End If
   If musicNum = 42 Then PlayMusic "playboy78/43 - Fat Bottomed Girls.mp3",MusicVol: End If
   If musicNum = 43 Then PlayMusic "playboy78/44 - My Kinda Lover.mp3",MusicVol: End If
   If musicNum = 44 Then PlayMusic "playboy78/45 - Start Me Up.mp3",MusicVol: End If
   If musicNum = 45 Then PlayMusic "playboy78/46 - Good Times Roll.mp3",MusicVol: End If
   If musicNum = 46 Then PlayMusic "playboy78/47 - Light My Fire.mp3",MusicVol: End If
   If musicNum = 47 Then PlayMusic "playboy78/48 - Two Tickets To Paradise.mp3",MusicVol: End If
   If musicNum = 48 Then PlayMusic "playboy78/49 - Born To Be Wild.mp3",MusicVol: End If
   If musicNum = 49 Then PlayMusic "playboy78/50 - Mama Told Me Not To Come.mp3",MusicVol: End If
   If musicNum = 50 Then PlayMusic "playboy78/51 - Best Of My Love.mp3",MusicVol: End If
   If musicNum = 51 Then PlayMusic "playboy78/52 - Jungle Fever.mp3",MusicVol: End If
   If musicNum = 52 Then PlayMusic "playboy78/53 - Sex As A Weapon.mp3",MusicVol: End If
   If musicNum = 53 Then PlayMusic "playboy78/54 - Jessie's Girl.mp3",MusicVol: End If
   If musicNum = 54 Then PlayMusic "playboy78/55 - Machine Gun.mp3",MusicVol: End If
   If musicNum = 55 Then PlayMusic "playboy78/56 - Sister Christian.mp3",MusicVol: End If
   If musicNum = 56 Then PlayMusic "playboy78/57 - Barracuda.mp3",MusicVol: End If
   If musicNum = 57 Then PlayMusic "playboy78/58 - Centerfold.mp3",MusicVol: End If
   If musicNum = 58 Then PlayMusic "playboy78/59 - Everybody Wants You.mp3",MusicVol: End If
   If musicNum = 59 Then PlayMusic "playboy78/60 - I Ran.mp3",MusicVol: End If
   If musicNum = 60 Then PlayMusic "playboy78/61 - Naughty Naughty.mp3",MusicVol: End If
   If musicNum = 61 Then PlayMusic "playboy78/62 - Lunatic Fringe.mp3",MusicVol: End If
   If musicNum = 62 Then PlayMusic "playboy78/63 - Addicted To Love.mp3",MusicVol: End If
   If musicNum = 63 Then PlayMusic "playboy78/64 - Cars.mp3",MusicVol: End If
   If musicNum = 64 Then PlayMusic "playboy78/65 - Working for the Weekend (Remastered).mp3",MusicVol: End If
   If musicNum = 65 Then PlayMusic "playboy78/66 - I Wanna Go Back.mp3",MusicVol: End If
   If musicNum = 66 Then PlayMusic "playboy78/67 - Four In The Morning (Album Version).mp3",MusicVol: End If
   If musicNum = 67 Then PlayMusic "playboy78/68 - Back In Black.mp3",MusicVol: End If
   If musicNum = 68 Then PlayMusic "playboy78/69 - A Little Less Conversation.mp3",MusicVol: End If
   If musicNum = 69 Then PlayMusic "playboy78/70 - Crazy On You.mp3",MusicVol: End If
   If musicNum = 70 Then PlayMusic "playboy78/71 - Honky Tonk Women.mp3",MusicVol: End If
   If musicNum = 71 Then PlayMusic "playboy78/72 - I'm Hot Tonight.mp3",MusicVol: End If
   If musicNum = 72 Then PlayMusic "playboy78/73 - Jump Into The Fire.mp3",MusicVol: End If
   If musicNum = 73 Then PlayMusic "playboy78/74 - Jungle Boogie.mp3",MusicVol: End If
   If musicNum = 74 Then PlayMusic "playboy78/75 - Saturday Night's Alright For Fighting.mp3",MusicVol: End If
   If musicNum = 75 Then PlayMusic "playboy78/76 - Runnin' With The Devil.mp3",MusicVol: End If
   If musicNum = 76 Then PlayMusic "playboy78/77 - Heartbreaker.mp3",MusicVol: End If
   If musicNum = 77 Then PlayMusic "playboy78/78 - Don't Say You Love Me.mp3",MusicVol: End If
   If musicNum = 78 Then PlayMusic "playboy78/79 - A 5th Of Beethoven.mp3",MusicVol: End If
   If musicNum = 79 Then PlayMusic "playboy78/80 - Shake It Up.mp3",MusicVol:End If
   If musicNum = 80 Then PlayMusic "playboy78/81 - Macho Man.mp3",MusicVol: End If
   If musicNum = 81 Then PlayMusic "playboy78/82 - Nowhere To Run.mp3",MusicVol: End If
   If musicNum = 82 Then PlayMusic "playboy78/83 - Private Eyes.mp3",MusicVol: End If
   If musicNum = 83 Then PlayMusic "playboy78/84 - In The City.mp3",MusicVol: End If
   If musicNum = 84 Then PlayMusic "playboy78/85 - You Sexy Thing.mp3",MusicVol: End If
   If musicNum = 85 Then PlayMusic "playboy78/86 - Still The Same.mp3",MusicVol: End If
End Sub


'*********************************************
'*         Random Music Mod: HiRez00         *
'*********************************************

Sub RandomTrack
If MusicOption2 = 1 then
   musicNum = INT(86 * RND(1) )
   MPlayer
   End If
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
	RS.VelocityCorrect(activeball)
	vpmTimer.pulseSw 36
    RandomSoundSlingshotLeft sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 2:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 3:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(activeball)
	vpmTimer.pulseSw 37
    RandomSoundSlingshotRight sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub




'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperVisualUpdate()
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
    FlipperLSh.visible=true
	FLipperRSh.visible=true
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




'++++++++++++++++++++++++
'+    OPTION SCRIPTS    +
'++++++++++++++++++++++++

changeballcard
Sub changeballcard
	If BallApronCard = 5 Then
	    BallCard.Image="5-Balls"
	Else
		BallCard.Image="3-Balls"
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



'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7	 'Level of bounces. Recommmended value of 0.7

Sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel, vratio
	If TargetBouncerEnabled = 1 And aball.z < 30 Then
		'   debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
		If aBall.velx = 0 Then vratio = 1 Else vratio = aBall.vely / aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1
			zMultiplier = 0.2 * defvalue
			Case 2
			zMultiplier = 0.25 * defvalue
			Case 3
			zMultiplier = 0.3 * defvalue
			Case 4
			zMultiplier = 0.4 * defvalue
			Case 5
			zMultiplier = 0.45 * defvalue
			Case 6
			zMultiplier = 0.5 * defvalue
		End Select
		aBall.velz = Abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((vel ^ 2 - aBall.velz ^ 2) / (1 + vratio ^ 2)))
		aBall.vely = aBall.velx * vratio
		'   debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'   debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer activeball, 1
End Sub




'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

Dim LF
Set LF = New FlipperPolarity
Dim RF
Set RF = New FlipperPolarity

InitPolarity


'*******************************************
' Late 70's to early 80's

Sub InitPolarity()
   dim x, a : a = Array(LF, RF)
   for each x in a
	   x.AddPoint "Ycoef", 0, RightFlipper.Y - 65, 1
	   x.AddPoint "Ycoef", 1, RightFlipper.Y - 11, 1
	   x.enabled = True
	   x.TimeDelay = 80
   Next

   AddPt "Polarity", 0, 0, 0
   AddPt "Polarity", 1, 0.05, - 2.7		
   AddPt "Polarity", 2, 0.33, - 2.7
   AddPt "Polarity", 3, 0.37, - 2.7		
   AddPt "Polarity", 4, 0.41, - 2.7
   AddPt "Polarity", 5, 0.45, - 2.7
   AddPt "Polarity", 6, 0.576, - 2.7
   AddPt "Polarity", 7, 0.66, - 1.8
   AddPt "Polarity", 8, 0.743, - 0.5
   AddPt "Polarity", 9, 0.81, - 0.5
   AddPt "Polarity", 10, 0.88, 0

   addpt "Velocity", 0, 0, 1
   addpt "Velocity", 1, 0.16, 1.06
   addpt "Velocity", 2, 0.41, 1.05
   addpt "Velocity", 3, 0.53, 1 '0.982
   addpt "Velocity", 4, 0.702, 0.968
   addpt "Velocity", 5, 0.95,  0.968
   addpt "Velocity", 6, 1.03, 0.945

   LF.Object = LeftFlipper		
   LF.EndPoint = EndPointLp
   RF.Object = RightFlipper
   RF.EndPoint = EndPointRp
End Sub



' Flipper trigger hit subs
Sub TriggerLF_Hit()
	LF.Addball activeball
End Sub
Sub TriggerLF_UnHit()
	LF.PolarityCorrect activeball
End Sub
Sub TriggerRF_Hit()
	RF.Addball activeball
End Sub
Sub TriggerRF_UnHit()
	RF.PolarityCorrect activeball
End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		  'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled TODO set time!
	Private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = New SpoofBall
		Next
	End Sub
	
	Public Property Let Object(aInput)
		Set Flipper = aInput
		StartPoint = Flipper.x
	End Property
	
	Public Property Let StartPoint(aInput)
		If IsObject(aInput) Then
			FlipperStart = aInput.x
		Else
			FlipperStart = aInput
		End If
	End Property
	
	Public Property Get StartPoint
		StartPoint = FlipperStart
	End Property
	
	Public Property Let EndPoint(aInput)
		FlipperEnd = aInput.x
		FlipperEndY = aInput.y
	End Property
	
	Public Property Get EndPoint
		EndPoint = FlipperEnd
	End Property
	
	Public Property Get EndPointY
		EndPointY = FlipperEndY
	End Property
	
	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			Case "Polarity"
			ShuffleArrays PolarityIn, PolarityOut, 1
			PolarityIn(aIDX) = aX
			PolarityOut(aIDX) = aY
			ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
			ShuffleArrays VelocityIn, VelocityOut, 1
			VelocityIn(aIDX) = aX
			VelocityOut(aIDX) = aY
			ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
			ShuffleArrays YcoefIn, YcoefOut, 1
			YcoefIn(aIDX) = aX
			YcoefOut(aIDX) = aY
			ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		If gametime > 100 Then Report aChooseArray
	End Sub
	
	Public Sub Report(aChooseArray) 'debug, reports all coords in tbPL.text
		If Not DebugOn Then Exit Sub
		Dim a1, a2
		Select Case aChooseArray
			Case "Polarity"
			a1 = PolarityIn
			a2 = PolarityOut
			Case "Velocity"
			a1 = VelocityIn
			a2 = VelocityOut
			Case "Ycoef"
			a1 = YcoefIn
			a2 = YcoefOut
			Case Else
			tbpl.text = "wrong string"
			Exit Sub
		End Select
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & aChooseArray & " x: " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		tbpl.text = str
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub
	
	Public Sub Fire()
		Flipper.RotateToEnd
		processballs
	End Sub
	
	Public Property Get Pos 'returns % position a ball. For debug stuff.
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x) ) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x) ) Then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = Abs(PartialFlipCoef - 1)
	End Sub
	
	Private Function FlipperOn() 'Timer shutoff for polaritycorrect
		If gameTime < FlipAt + TimeDelay Then FlipperOn = True
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY >  - 8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)	'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)  'find safety coefficient 'ycoef' data
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx * VelCoef
				If Enabled Then aBall.Vely = aBall.Vely * VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				If StartPoint > EndPoint Then LR =  - 1 'Reverse polarity if left flipper
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled Then aBall.VelX = aBall.VelX + 1 * (AddX * ycoef * PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS
	
	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script in-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
'Function RotPoint(x,y,angle)
'	dim rx, ry
'	rx = x*dCos(angle) - y*dSin(angle)
'	ry = x*dSin(angle) + y*dCos(angle)
'	RotPoint = Array(rx,ry)
'End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'   debug.print " " 
		End If
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY. RUBBER DAMPENER, AND SLINGSHOT CORRECTION SUPPORTING FUNCTIONS 
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	Dim a
	a = Array(LF, RF)
	Dim x
	For Each x In a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, ByVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray) 'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount - 1 + offset)   'Resize original array
	For x = 0 To aCount - 1 'set objects back into original array
		If IsObject(a(x)) Then
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = Sqr(ball.VelX ^ 2 + ball.VelY ^ 2 + ball.VelZ ^ 2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)  'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m * X2
	Y = M * x + b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	Dim ii
	For ii = 1 To UBound(xKeyFrame) 'find active line
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L - 1), yLvl(L - 1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )	'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )	'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	Dim BOT
	BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function


'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
	SOSRampup = 2.5
	Case 1
	SOSRampup = 6
	Case 2
	SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
'   Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity
	
	Flipper.eostorque = EOST
	Flipper.eostorqueangle = EOSA
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle)	'-1 for Right Flipper
	Dim LiveCatchBounce																														'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime
	CatchTime = GameTime - FCount
	
	If CatchTime <= LiveCatch And parm > 6 And Abs(Flipper.x - ball.x) > LiveDistanceMin And Abs(Flipper.x - ball.x) < LiveDistanceMax Then
		If CatchTime <= LiveCatch * 0.5 Then												'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		Else
			LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)		'Partial catch when catch happens a bit late
		End If
		
		If LiveCatchBounce = 0 And ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx = 0
		ball.angmomy = 0
		ball.angmomz = 0
	Else
		If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************

'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports in debugger (in vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " in vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = getballs
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class


'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
'****  DROP TARGETS by Rothbauerw
'******************************************************


'******************************************************
'  DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
  Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Secondary(): Set Secondary = m_secondary: End Property
  Public Property Let Secondary(input): Set m_secondary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
  Public Property Let IsDropped(input): m_isDropped = input: End Property

  Public default Function init(primary, secondary, prim, sw, animate, isDropped)
    Set m_primary = primary
    Set m_secondary = secondary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate
    m_isDropped = isDropped

    Set Init = Me
  End Function
End Class

'Define a variable for each drop target
Dim DT1, DT2, DT3, DT4, DT5

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
'   primary:	primary target wall to determine drop
'   secondary:  wall used to simulate the ball striking a bent or offset target after the initial Hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!! 
'				   rotz must be used for orientation
'				   rotx to bend the target back
'				   transz to move it up and down
'				   the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'   switch:	 ROM switch number
'   animate:	Array slot for handling the animation instrucitons, set to 0
'				   Values for animate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 
'   isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.

Set DT1 = (new DropTarget)(sw1, sw1a, sw1p, 1, 0, False)
Set DT2 = (new DropTarget)(sw2, sw2a, sw2p, 2, 0, False)
Set DT3 = (new DropTarget)(sw3, sw3a, sw3p, 3, 0, False)
Set DT4 = (new DropTarget)(sw4, sw4a, sw4p, 4, 0, False)
Set DT5 = (new DropTarget)(sw5, sw5a, sw5p, 5, 0, False)

Dim DTArray
DTArray = Array(DT1, DT2, DT3, DT4, DT5)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 90 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick
Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)
	
	PlayTargetSound
	DTArray(i).animate = DTCheckBrick(Activeball,DTArray(i).prim)
	If DTArray(i).animate = 1 Or DTArray(i).animate = 3 Or DTArray(i).animate = 4 Then
		DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
	End If
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)
	
	DTArray(i).animate =  - 1
	DoDTAnim
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch)
	
	DTArray(i).animate = 1
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 To UBound(DTArray)
		If DTArray(i).sw = switch Then 
			DTArrayID = i
			Exit Function
		End If
	Next
End Function

Sub DTBallPhysics(aBall, angle, mass)
	Dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	
	calc1 = cor.BallVel(aball.id) * Cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Cos(rangle + 4 * Atn(1) / 2)
	calc3 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Sin(rangle + 4 * Atn(1) / 2)
	
	aBall.velx = calc1 * Cos(rangle) + calc2
	aBall.vely = calc1 * Sin(rangle) + calc3
End Sub

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim)
	Dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	Xintersect = (aBall.y - dtprim.y - Tan(bangle) * aball.x + Tan(rangle2) * dtprim.x) / (Tan(rangle2) - Tan(bangle))
	Yintersect = Tan(rangle2) * Xintersect + (dtprim.y - Tan(rangle2) * dtprim.x)
	
	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		If DTEnableBrick = 1 And  perpvel > DTBrickVel And DTBrickVel <> 0 And cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		DTCheckBrick = 4
	Else
		DTCheckBrick = 0
	End If
End Function

Sub DoDTAnim()
	Dim i
	For i = 0 To UBound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch, animate)
	Dim transz, switchid
	Dim animtime, rangle
	
	switchid = switch
	
	Dim ind
	ind = DTArrayID(switchid)
	
	rangle = prim.rotz * PI / 180
	
	DTAnimate = animate
	
	If animate = 0 Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = gametime
	End If
	
	animtime = gametime - primary.uservalue
	
	If (animate = 1 Or animate = 4) And animtime < DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		DTAnimate = animate
		Exit Function
	ElseIf (animate = 1 Or animate = 4) And animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If
	
	If animate = 2 Then
		transz = (animtime - DTDropDelay) / DTDropSpeed * DTDropUnits *  - 1
		If prim.transz >  - DTDropUnits  Then
			prim.transz = transz
		End If
		
		prim.rotx = DTMaxBend * Cos(rangle) / 2
		prim.roty = DTMaxBend * Sin(rangle) / 2
		
		If prim.transz <= - DTDropUnits Then
			prim.transz =  - DTDropUnits
			secondary.collidable = 0
			DTArray(ind).isDropped = True 'Mark target as dropped
			controller.Switch(Switchid) = 1
			DTAction Switchid
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		End If
	End If
	
	If animate = 3 And animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
	ElseIf animate = 3 And animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If
	
	If animate =  - 1 Then
		transz = (1 - (animtime) / DTDropUpSpeed) * DTDropUnits *  - 1
		
		If prim.transz =  - DTDropUnits Then
			Dim b
			Dim BOT
			BOT = GetBalls
			
			For b = 0 To UBound(BOT)
				If InRotRect(BOT(b).x,BOT(b).y,prim.x, prim.y, prim.rotz, - 25, - 10,25, - 10,25,25, - 25,25) And BOT(b).z < prim.z + DTDropUnits + 25 Then
					BOT(b).velz = 20
				End If
			Next
		End If
		
		If prim.transz < 0 Then
			prim.transz = transz
		ElseIf transz > 0 Then
			prim.transz = transz
		End If
		
		If prim.transz > DTDropUpUnits Then
			DTAnimate =  - 2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = gametime
		End If
		primary.collidable = 0
		secondary.collidable = 1
		DTArray(ind).isDropped = False 'Mark target as not dropped
		controller.Switch(Switchid) = 0
	End If
	
	If animate =  - 2 And animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay) / DTDropSpeed * DTDropUnits *  - 1 + DTDropUpUnits
		If prim.transz < 0 Then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0
			
			primary.collidable = 1
			secondary.collidable = 0
		End If
	End If
End Function

Sub DTAction(switchid)
	Select Case switchid
		Case 1: ShadowDT(0).visible = False
		Case 2: ShadowDT(1).visible = False
		Case 3: ShadowDT(2).visible = False
		Case 4: ShadowDT(3).visible = False
		Case 5: ShadowDT(4).visible = False
	End Select
End Sub

'******************************************************
'  DROP TARGET
'  SUPPORTING FUNCTIONS 
'******************************************************

' Used for drop targets
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function

'******************************************************
'****  END DROP TARGETS
'******************************************************

'******************************************************
'		STAND-UP TARGET INITIALIZATION
'******************************************************

Class StandupTarget
  Private m_primary, m_prim, m_sw, m_animate

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public default Function init(primary, prim, sw, animate)
    Set m_primary = primary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate

    Set Init = Me
  End Function
End Class

'Define a variable for each stand-up target
Dim ST17, ST25, ST26, ST27, ST28, ST29

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
'   primary:	vp target to determine target hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!! 
'				   transy must be used to offset the target animation
'   switch:	 ROM switch number
'   animate:	Arrary slot for handling the animation instrucitons, set to 0
' 
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts

Set ST17 = (new StandupTarget)(sw17, psw17, 17, 0)
Set ST25 = (new StandupTarget)(sw25, psw25, 25, 0)
Set ST26 = (new StandupTarget)(sw26, psw26, 26, 0)
Set ST27 = (new StandupTarget)(sw27, psw27, 27, 0)
Set ST28 = (new StandupTarget)(sw28, psw28, 28, 0)
Set ST29 = (new StandupTarget)(sw29, psw29, 29, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
'   STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST17, ST25, ST26, ST27, ST28, ST29)

'Configure the behavior of Stand-up Targets
Const STAnimStep = 1.5  'vpunits per animation step (control return to Start)
Const STMaxOffset = 9   'max vp units target moves when hit

Const STMass = 0.2	  'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)
	
	PlayTargetSound
	STArray(i).animate = STCheckHit(Activeball,STArray(i).primary)
	
	If STArray(i).animate <> 0 Then
		DTBallPhysics Activeball, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 To UBound(STArray)
		If STArray(i).sw = switch Then 
			STArrayID = i
			Exit Function
		End If
	Next
End Function

Function STCheckHit(aBall, target) 'Check if target is hit on it's face
	Dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		STCheckHit = 1
	Else
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i = 0 To UBound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime
	
	STAnimate = animate
	
	If animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = gametime
	End If
	
	animtime = gametime - primary.uservalue
	
	If animate = 1 Then
		primary.collidable = 0
		prim.transy =  - STMaxOffset
		vpmTimer.PulseSw switch
		STAnimate = 2
		Exit Function
	ElseIf animate = 2 Then
		prim.transy = prim.transy + STAnimStep
		If prim.transy >= 0 Then
			prim.transy = 0
			primary.collidable = 1
			STAnimate = 0
			Exit Function
		Else
			STAnimate = 2
		End If
	End If
End Function



'******************************************************
'		END STAND-UP TARGETS
'******************************************************



'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
'	 Metals (all metal objects, metal walls, metal posts, metal wire guides)
'	 Apron (the apron walls and plunger wall)
'	 Walls (all wood or plastic walls)
'	 Rollovers (wire rollover triggers, star triggers, or button triggers)
'	 Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
'	 Gates (plate gates)
'	 GatesWire (wire gates)
'	 Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.  
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).  
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate 
' how to make these updates. But in summary the following needs to be updated:	
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial vides by Apophis
' Part 1:	 https://youtu.be/PbE2kNiam3g
' Part 2:	 https://youtu.be/B5cm1Y8wQsk
' Part 3:	 https://youtu.be/eLhWyuYOyGg


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
RollingSoundFactor = 1.1 / 5

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025 / 5			 'volume multiplier; must not be zero

'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////

Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////

Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////

Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////

Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////

Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
		PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
		PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
		PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
		PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
		PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
		PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
		PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
		PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
		PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
		PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
			PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////

Sub RandomSoundBottomArchBallGuide()
	Dim finalspeed
	finalspeed = Sqr(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
			PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
			PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
			PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
			PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) And cor.ballvely(activeball.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
			PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
			PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
		PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
		PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
		PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
		PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
		PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
		PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
		PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
		PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
		PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
		PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
		PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
		PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
		PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
		PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
		PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
		PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd * 7) + 1
		Case 1
		snd = "Ball_Collide_1"
		Case 2
		snd = "Ball_Collide_2"
		Case 3
		snd = "Ball_Collide_3"
		Case 4
		snd = "Ball_Collide_4"
		Case 5
		snd = "Ball_Collide_5"
		Case 6
		snd = "Ball_Collide_6"
		Case 7
		snd = "Ball_Collide_7"
	End Select
	
	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
		PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
		PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
		PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
		PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************



'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************


' *** These define the appearance of shadows in your table	***

'Ambient (Room light source)
Const AmbientBSFactor = 0.8	 '0 to 1, higher is darker
Const AmbientMovement = 1	   '1+ higher means more movement as the ball moves left and right
Const offsetX = 0			   'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 5			   'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

'Dynamic (Table light sources)
Const DynamicBSFactor = 0.95	'0 to 1, higher is darker
Const Wideness = 20			 'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness = 5			  'Sets minimum as ball moves away from source

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objrtx1(5), objrtx2(5)
Dim objBallShadow(5)
Dim OnPF(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

'Initialization
DynamicBSInit

Sub DynamicBSInit()
	Dim iii, source
	
	'Prepare the shadow objects before play begins
	For iii = 0 To tnob - 1
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii / 1000 + 0.01  'Separate z for layering without clipping
		objrtx1(iii).visible = 0
		
		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii / 1000 + 0.02
		objrtx2(iii).visible = 0
		
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii / 1000 + 0.04
		objBallShadow(iii).visible = 0
		
		BallShadowA(iii).Opacity = 100 * AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next
	
	iii = 0
	
	For Each Source In DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		'   If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
End Sub

'Sub BallOnPlayfieldNow(onPlayfield, ballNum)	'Whether a ball is currently on the playfield. Only update certain things once, save some cycles
'	If onPlayfield Then
'		OnPF(ballNum) = True
'		bsRampOff BOT(ballNum).ID
'		'   debug.print "Back on PF"
'		UpdateMaterial objBallShadow(ballNum).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
'		objBallShadow(ballNum).size_x = 5
'		objBallShadow(ballNum).size_y = 4.5
'		objBallShadow(ballNum).visible = 1
'		BallShadowA(ballNum).visible = 0
'		BallShadowA(ballNum).Opacity = 100 * AmbientBSFactor
'	Else
'		OnPF(ballNum) = False
'		'   debug.print "Leaving PF"
'	End If
'End Sub

Sub DynamicBSUpdate
	Dim falloff 'Max distance to light sources, can be changed dynamically if you have a reason
	falloff = 150
	Dim ShadowOpacity1, ShadowOpacity2
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim bsRampType
	Dim BOT: BOT=getballs	'Uncomment if you're destroying balls - Not recommended! #SaveTheBalls
	
	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 To tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next
	
	If UBound(BOT) < lob Then Exit Sub 'No balls in play, exit
	
	'The Magic happens now
	For s = lob To UBound(BOT)
		' *** Normal "ambient light" ball shadow
		'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your Elseif segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else (under 20)
		
		'Flasher shadow everywhere
		If AmbientBallShadowOn = 1 Then
			If BOT(s).Z > 30 Then 'In a ramp
				BallShadowA(s).X = BOT(s).X + offsetX
				BallShadowA(s).Y = BOT(s).Y + offsetY + BallSize / 10
				BallShadowA(s).height = BOT(s).z - BallSize / 4 + s / 1000 'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			ElseIf BOT(s).Z <= 30 And BOT(s).Z > 20 Then 'On pf
				BallShadowA(s).visible = 1
				BallShadowA(s).X = BOT(s).X + (BOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
				BallShadowA(s).Y = BOT(s).Y + offsetY
				BallShadowA(s).height = 1.04 + s / 1000
			Else 'Under pf
				BallShadowA(s).X = BOT(s).X + offsetX
				BallShadowA(s).Y = BOT(s).Y + offsetY
				BallShadowA(s).height = 0.5 + s / 1000
			End If
		End If
		
		' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 And BOT(s).X < 850 Then 'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff
				dist2 = falloff
				For iii = 0 To numberofsources - 1 'Search the 2 nearest influencing lights
					LSd = Distance(BOT(s).x, BOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff Then 'And gilvl > 0 Then
						'   If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1
					objrtx1(s).X = BOT(s).X
					objrtx1(s).Y = BOT(s).Y
					'   objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), BOT(s).X, BOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1 * DynamicBSFactor ^ 3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1
					objrtx2(s).X = BOT(s).X
					objrtx2(s).Y = BOT(s).Y + offsetY
					'   objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), BOT(s).X, BOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2 * DynamicBSFactor ^ 3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0
				objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub


'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************



'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************

' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 To tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b
	Dim BOT
	BOT = GetBalls
	
	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 To tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		'StopSound("BallRoll_" & b)
		StopSound("fx_ballrolling" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(BOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(BOT)
		If BallVel(BOT(b)) > 1 And BOT(b).z < 30 Then
			rolling(b) = True
			'PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, 2*PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
			PlaySound ("fx_ballrolling" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, 2*PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
		Else
			If rolling(b) = True Then
				'StopSound("BallRoll_" & b)
				StopSound("fx_ballrolling" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If BOT(b).VelZ <  - 1 And BOT(b).z < 55 And BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz >  - 7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If
			End If
		End If
		
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
		
		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dynamic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height = BOT(b).z - BallSize / 4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Else
				BallShadowA(b).height = 0.1
			End If
			BallShadowA(b).Y = BOT(b).Y + offsetY
			BallShadowA(b).X = BOT(b).X + offsetX
			BallShadowA(b).visible = 1
		End If
	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************


'******************************************************
'****  GI COLOR + CHANGER
'******************************************************

Sub colorgi
	If colornow=1 Then Dim x:For each x in GI_a:x.color=RGB(255,204,153):x.intensity=4: Next: For each x in GI_b:x.color=RGB(255,204,153):x.intensity=50: gi032.intensity=10: gi033.intensity=10: gi034.intensity=10: GI_Bumper_1.intensity=40: GI_Bumper_2.intensity=40: GI_Bumper_3.intensity=40: Next: Exit Sub
    If colornow=2 Then Dim z:For each z in GI_a:z.color=RGB(255,102,204):z.intensity=4: Next: For each z in GI_b:z.color=RGB(255,102,204):z.intensity=50: gi032.intensity=10: gi033.intensity=10: gi034.intensity=10: GI_Bumper_1.intensity=40: GI_Bumper_2.intensity=40: GI_Bumper_3.intensity=40: Next: Exit Sub
    If colornow=3 Then Dim y:For each y in GI_a:y.color=RGB(255,255,255):y.intensity=4: Next: For each y in GI_b:y.color=RGB(255,255,255):y.intensity=50: gi032.intensity=10: gi033.intensity=10: gi034.intensity=10: GI_Bumper_1.intensity=40: GI_Bumper_2.intensity=40: GI_Bumper_3.intensity=40: Next
End Sub

'******************************************************
'****  OPTIONS MENU DMD
'******************************************************

Sub DMDMenu
      If DMDMenuON = 1 then
      DMD_Flasher_Back.visible=True
        Options=Options+1
        If Options=10 then Options=2
        Playsound "Beep1"
		Select Case (Options)
			Case 1:
                Option_Box.visible=True
                Option_Box.imageA="DMDOptInst"
                Select_Box.imageA="DMDSelectBlank": Select_Box.visible=True
                FirstLook=1
                DMDMenuSelect
			Case 2:
                Option_Box.imageA="DMDOptCheat"
                FirstLook=1
                DMDMenuSelect
			Case 3:
                Option_Box.imageA="DMDOptGI"
                FirstLook=1
                DMDMenuSelect
			Case 4:
                Option_Box.imageA="DMDOptMusic"
                FirstLook=1
                DMDMenuSelect
			Case 5:
                Option_Box.imageA="DMDOptMusicVol"
                FirstLook=1
                DMDMenuSelect
			Case 6:
                Option_Box.imageA="DMDOptBallCard"
                FirstLook=1
                DMDMenuSelect
			Case 7:
                Option_Box.imageA="DMDOptRulesCard"
                FirstLook=1
                DMDMenuSelect
			Case 8:
                Option_Box.imageA="DMDOptAltFlips"
                FirstLook=1
                DMDMenuSelect
			Case 9:    
                FirstLook=1
                DMDMenuSelect                                                'EXIT
                Option_Box.imageA="DMDSelectExit"
                Select_Box.visible=False
                Select_Box.imageA="DMDSelectBlank"
		End Select
      End If
End Sub

Sub DMDMenuSelect
   If DMDMenuON=1 Then
    Select Case (Options)
        Case 1: 
                Select_Box.imageA="DMDSelectBlank": Select_Box.visible=True
                FirstLook = 0
		Case 2:
                If FirstLook=0 Then
                      Playsound "Beep2"
			          if CheatPosts=0 Then
				         CheatPosts=1
                         CheatAdd
                         Select_Box.imageA="DMDSelectOn"
			          Else
				         CheatPosts=0
                         CheatAdd
                         Select_Box.imageA="DMDSelectOff"
			          end if
                    Else
			          if CheatPosts=1 Then Select_Box.imageA="DMDSelectOn": Select_Box.visible=True
			          if CheatPosts=0 Then Select_Box.imageA="DMDSelectOff": Select_Box.visible=True
                       FirstLook=0
                End If
       Case 3:
               If FirstLook=0 Then
                Playsound "Beep2"
                colornow=colornow+1
			    if colornow>3 then colornow=1: End If
                    if colornow=1 Then Select_Box.imageA="DMDSelectGI1": End If
                    if colornow=2 Then Select_Box.imageA="DMDSelectGI2": End If
                    if colornow=3 Then Select_Box.imageA="DMDSelectGI3": End If
                    colorgi
                Else
                    if colornow=1 Then Select_Box.imageA="DMDSelectGI1": End If
                    if colornow=2 Then Select_Box.imageA="DMDSelectGI2": End If
                    if colornow=3 Then Select_Box.imageA="DMDSelectGI3": End If
                    colorgi
                    FirstLook=0
                End If
       Case 4:
            If FirstLook=0 Then
               Playsound "Beep2"
			  if MusicOption1 = 0 Then
				MusicOption1 = 1
                Select_Box.imageA="DMDSelectOn"
                NextTrack
			  Else
				MusicOption1 = 0
                Select_Box.imageA="DMDSelectOff"
                EndMusic
			   end if
               Else
                     If MusicOption1 = 0 then Select_Box.imageA="DMDSelectOff"
                     If MusicOption1 = 1 then Select_Box.imageA="DMDSelectOn"
                     FirstLook=0
                End If
       Case 5:
              If FirstLook=0 Then
               Playsound "Beep2"
			     MusicVolume = MusicVolume - 1
                 If MusicVolume < 5 then MusicVolume = 10
                 if MusicVolume = 10 Then Select_Box.imageA="DMDSelect100": MusicVol = 1: End If
                 if MusicVolume = 9 Then Select_Box.imageA="DMDSelect90": MusicVol = .9: End If
                 if MusicVolume = 8 Then Select_Box.imageA="DMDSelect80": MusicVol = .8: End If
                 if MusicVolume = 7 Then Select_Box.imageA="DMDSelect70": MusicVol = .7: End If
                 if MusicVolume = 6 Then Select_Box.imageA="DMDSelect60": MusicVol = .6: End If
                 if MusicVolume = 5 Then Select_Box.imageA="DMDSelect50": MusicVol = .5: End If
                     Else
                       if MusicVolume = 10 Then Select_Box.imageA="DMDSelect100": MusicVol = 1: End If
                       if MusicVolume = 9 Then Select_Box.imageA="DMDSelect90": MusicVol = .9: End If
                       if MusicVolume = 8 Then Select_Box.imageA="DMDSelect80": MusicVol = .8: End If
                       if MusicVolume = 7 Then Select_Box.imageA="DMDSelect70": MusicVol = .7: End If
                       if MusicVolume = 6 Then Select_Box.imageA="DMDSelect60": MusicVol = .6: End If
                       if MusicVolume = 5 Then Select_Box.imageA="DMDSelect50": MusicVol = .5: End If
                       FirstLook=0
                End If
       Case 6:
            If FirstLook=0 Then
              Playsound "Beep2"
			  if BallApronCard = 3 Then
				BallApronCard = 5
                Select_Box.imageA="DMDSelect5Ball"
                changeballcard
			  Else
				BallApronCard = 3
                changeballcard
                Select_Box.imageA="DMDSelect3Ball" 
                changeballcard
			   end if
               Else
                     If BallApronCard = 3 then Select_Box.imageA="DMDSelect3Ball": BallCard.Image="3-Balls" 
                     If BallApronCard = 5 then Select_Box.imageA="DMDSelect5Ball": BallCard.Image="5-Balls"
                     changeballcard
                     FirstLook=0
                End If
       Case 7:
               If FirstLook=0 Then
               Playsound "Beep2"
			    InstructionCard=InstructionCard+1
                changeinstructcard
			    if InstructionCard>3 then InstructionCard=1: End If
                    if InstructionCard=1 Then Select_Box.imageA="DMDSelectRulesE": End If
                    if InstructionCard=2 Then Select_Box.imageA="DMDSelectRulesF": End If
                    if InstructionCard=3 Then Select_Box.imageA="DMDSelectRulesG": End If
                    changeinstructcard
                Else
                    if InstructionCard=1 Then Select_Box.imageA="DMDSelectRulesE": End If
                    if InstructionCard=2 Then Select_Box.imageA="DMDSelectRulesF": End If
                    if InstructionCard=3 Then Select_Box.imageA="DMDSelectRulesG": End If
                    changeinstructcard
                    FirstLook=0
                End If

       Case 8:
            If FirstLook=0 Then
            Playsound "Beep2"
			  if AlternateFlippers = True Then
				AlternateFlippers = False
                Select_Box.imageA="DMDSelectOff"
                changeflippers
			  Else
				AlternateFlippers = True
                Select_Box.imageA="DMDSelectOn"
                changeflippers
			   end if
              Else
                If AlternateFlippers = True then Select_Box.imageA="DMDSelectOn"
                If AlternateFlippers = False then Select_Box.imageA="DMDSelectOff"
                changeflippers
                FirstLook=0
              End If

       Case 9: 
                If FirstLook=0 Then
                    CloseDMDMenu  
                  Else
                    FirstLook=0
                End If    
	  End Select
	End If
End Sub

Sub CloseDMDMenu
     Playsound "Beep2"
     DMDMenuON=0
     DMD_Flasher_Back.visible=False
     Select_Box.visible=False
     Option_Box.visible=False
     Option_Box.imageA=""
     Select_Box.imageA=""
End Sub


