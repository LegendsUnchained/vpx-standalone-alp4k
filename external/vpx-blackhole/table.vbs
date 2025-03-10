'==============================================================================================='
'																								' 			  	 
'											BlackHole	     									'
'          	              		         Gottlieb (1981)            	 	                    '
'		  				  	 http://www.ipdb.org/machine.cgi?id=307			                    '
'																								'
' 	  		 	 	  	            Created by: cyberpez		  			                    '    					 
'																								' 			  	 
'==============================================================================================='

Option Explicit
Randomize

Dim LightHalo_ON, BlackLights_on, LowerBlacklights_on, RedGravityTunnel, RomSet, cGameName, UpperFlipperColor, LowerFlipperColor, UpperPeg, LowerPeg, LeftDrain, LouderRoll, GIbuzz, WhooshSound, NightMod, CaptiveLight, GIColorMod, GIColorModLower, BallRadius, BallMass, BlackLightApron, Language, NumOfBalls, ReplayOption, BlackLightMultiball,BallMod, BlackLightPlastics, TubeGlow, WindowFoam, CustomCards, UltraBrightBumpers

'***************************************************************************'
'***************************************************************************'
'								OPTIONS
'***************************************************************************'
'***************************************************************************'

'Lighting

GIColorMod = 0  		' 0=standard GI   1=Blacklight LED's (HauntFreaks special)
GIColorModLower = 0 	' 0=standard GI   1=Blue  2=Blue/Purple
RedGravityTunnel = 0  	' Set to 1 to changes gravity tunnel GI to red
CaptiveLight = 0 		' 0=off 1=Blue 2=Orange 3=Green 4=Purple 5=Red 6=White 7=Yellow
TubeGlow = 0  			' 0=off 1=on  Re-Entry tube glows blue when in use
UltraBrightBumpers = 0	' 0=Normal lit Bumper Caps  1 = Ultra Bright LED caps 2 = Ultra Bright Blue LED caps

'Blacklights, those planets and astroids SCREAM blacklights
BlackLights_on = 0  	' Set to 1 to turn on blacklights for the upper playfield (Stars, Planets, and Astroids don't fade with GI)
LowerBlacklights_on = 0 ' Set to 1 to turn blacklighs on for the lower playfield (Star don't fade with GI)
BlackLightApron = 0		' Set to 1 to add blightlight glow to Apron
BlackLightPlastics = 0	' 0 = Normal Clear plastics  1 = blacklight blue "clear plastics"

'BlackLight Multi-ball  - Overrides other blacklight settings..  Blacklights turn on during multiball
BlackLightMultiball = 0

'Flipper Colors
UpperFlipperColor = 0 	' 0=Red  1=Black 2=Yellow 3=Blue
LowerFlipperColor = 1 	' 0=Red  1=Black 2=Yellow 3=Blue

'BallMod
BallMod = 0				' 0=Normal Balls  1=Marbled balls

'Instruction Cards  (only changes instruction cards, not actual options)
CustomCards = 0 		' 0=normal Cards  1=Custom instuction cards
Language = 1  			' 1=English  2=French  3=German  
NumOfBalls = 0  		' 0=3ball game   1=5 ball game
ReplayOption = 0  		' 0=No replays for beating high score  1=3 replays for beating high score


'Cheaters
UpperPeg = 1 			' Set to 1 to add a post below the upper flippers
LowerPeg = 1 			' Set to 1 to add a post below the lower flippers
LeftDrain = 2 			' 1=Single Post on left drain  2=Double Post on Left Drain  3=Rubber around the whole lot.


'Sounds
GIbuzz = 1  			' Buzz sound when Lower GI turns on.  (Sampled from youtube video)
WhooshSound = 1  		' Wind\Whoosh sound while on lower playfield that is not emulated by rom.  (Sampled from youtube video)


'Ball Rolling sounds	
LouderRoll = 1 			' 0=normal ball rolling sounds 1=a bit louder ball rolling sounds

'Other Mods
WindowFoam = 0 			' 0=black 1=blue

'Ball Size and Weight
BallRadius = 25
BallMass = 1.7

'*****************************
'Rom Version Selector
'*****************************
'1 "blckhole"   ' Gottlieb Rev.1 - blckhole
'2 "blkhole2"   ' Gottlieb Rev.2 - blkhole2
'3 "bhol_ltd"	' Gottlieb Limitied Edition - bhol_ltd  working????? acts CRAZY
'4 "blkholea"   ' Gottlieb Sound Only - blkholea

'5 "blkhole7"   ' Gottlieb 7-digit - blkhole7
'6 "blkhol7s"   ' Gottlieb Sound Only 7-digit - blkhol7s

RomSet = 5


'***************************************************************************'
'***************************************************************************'
'							End Of OPTIONS
'***************************************************************************'
'***************************************************************************'

'options not used anymore, please don't edit.
NightMod = 0 			' 0 = day   1 = night
LightHalo_ON = 1 		' Set to 0 to turn off light halos

On Error Resume Next
	ExecuteGlobal GetTextFile("controller.vbs")
	If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "00990300", "sys80.VBS", 2.33

'****************************************
'Check the selected ROM version
'****************************************

If RomSet = 1 then cGameName="blckhole":DisplayTimer6.Enabled = true  End If
If RomSet = 2 then cGameName="blkhole2":DisplayTimer6.Enabled = true  End If
If RomSet = 3 then cGameName="bhol_ltd":DisplayTimer6.Enabled = true  End If
If RomSet = 4 then cGameName="blkholea":DisplayTimer6.Enabled = true  End If
If RomSet = 5 then cGameName="blkhole7":DisplayTimer7.Enabled = true End If
If RomSet = 6 then cGameName="blkhol7s":DisplayTimer7.Enabled = true End If

'********************
'Standard definitions
'********************

Const cCredits="Black Hole",UseSolenoids=1,UseLamps=0,UseGI=1,UseSync=1
Const SSolenoidOn="solenoid",SSolenoidOff="soloff",SFlipperOn="FlipperUp",SFlipperOff="FlipperDown",sCoin="coin"

'******************************************************
' 					TABLE INIT
'******************************************************

Dim bsTrough,bsSaucer,dtBlack,dtHole,dtLLeft,dtLRight
Dim f,g,h,i,j
Dim cBall1,cBall2,cBall3

Sub BlackHole_Init()
	On Error Resume Next
	With Controller
		.GameName=cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine=cCredits
		.HandleMechanics=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.Run
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With

    '************  Main Timer init  ********************

	PinMAMETimer.Interval=PinMAMEInterval  
	PinMAMETimer.Enabled=1

    '************  Nudging   **************************

	vpmNudge.TiltSwitch=26
	vpmNudge.Sensitivity=3
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3,Bumper4,Bumper5,Bumper6,Sling1,Sling2,Sling3,Sling8)

	CheckSolenoid 16,0			' Initialize Upper Playfield Relay

	controller.switch(25) = true

	StartLampTimer
	CheckBlackLights
	CheckFlipperColors
	CheckCheaters
	CheckDayOrNight
	SetCaptiveLightColor
	CheckGIColorMod
	CheckInstructionCards
	SetWindowsFoam

	Set cBall1 = Kicker1.CreateSizedBallWithMass(BallRadius, BallMass)
	Set cBall2 = Kicker2.CreateSizedBallWithMass(BallRadius, BallMass)
	Set cBall3 = Kicker3.CreateSizedBallWithMass(BallRadius, BallMass)

	If BallMod = 1 Then
		cBall1.Image = "Chrome_Ball_29"	
		cBall1.FrontDecal = "BlackHole_Ball1"
		cBall2.Image = "Chrome_Ball_30"	
		cBall2.FrontDecal = "BlackHole_Ball2"
		cBall3.Image = "Chrome_Ball_29"	
		cBall3.FrontDecal = "BlackHole_Ball3"
	End If

End Sub

'******************************************************
'					SOLENOIDS
'******************************************************

Const sSaucer=12
Const sRT=13
Const sKnocker=8
Const sGate=17
Const sCLo=18
Const sEnable=19
const sdtLeftLower = 5
const sdtRightLower = 6
const sdtRightUpper = 1
const sdtLeftUpper = 2
const sOutHole = 9

SolCallBack(sSaucer)="bsSaucer.SolOut"
SolCallback(sKnocker)="vpmSolSound SoundFX(""knocker"",DOFKnocker),"
SolCallback(sGate)="vpmSolDiverter Flipper1,True,"
SolCallback(sEnable)="vpmNudge.SolGameOn"
SolCallback(42) = "bsLKick.SolOut"
solcallback(sdtLeftUpper)="BlackTargetsUp"
solcallback(sdtRightLower)="WhiteTargetsUp"
solcallback(sdtRightUpper)="HoleTargetsUp"
solcallback(sdtLeftLower)="YellowTargetsUp"
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sOutHole) = "SolOuthole"

'********************
' Black Hole used lamps to activate solenoids, CheckSolenoid is called when lamps are changed and looks for those assigned to solenoid actions
'********************

Sub CheckSolenoid(lnum,lstate)
	Select Case lnum
		Case 8: 
			If lstate = 1 then 								' Lower Playfield Trough Gate
				sw53.kick 60, 5			
				controller.Switch(53) = 0
			End If
		Case 12:											' Lower Playfield kicker
			If lstate = 1 then 
				sw42.kick -30,4
				controller.Switch(42) = 0
				SoundFX "kicker",DOFContactors
				If controller.Lamp(17) Then
					If BlackLightMultiball = 1 then 
						StartBlackLightMultiball 
					End If
				End If
			End If
		Case 13:											' Upper Playfield kicker
			If lstate = 1 then 
				sw05.kickZ 175,10,0,5
				controller.Switch(5) = 0
				SoundFX "kicker",DOFContactors
				sw05.timerenabled = true
			End If
		Case 14: 											' Lower Playfield Tube Kicker
			If lstate = 1 then 
				sw43.kick 180,40,0.5
				controller.Switch(43) = 0
				SoundFX "kicker",DOFContactors
			End If
		Case 15: 											' Shooter Lane Ball Release
			If lstate = 1 then
				ReleaseBall
			End If
		Case 16: 											' Upper Playfield Relay
			if lstate = 0 then 
				SetUpperGI(1)
				Setflash 101, 1
				SetLamp 101, 1
				BlackHole.ColorGradeImage = "ColorGrade_on"	
			Else
				SetUpperGI(0)
				Setflash 101, 0
				SetLamp 101, 0
				BlackHole.ColorGradeImage = "ColorGrade_off"	
			End If
		Case 17: 											' Lower Playfield Relay
			if lstate = 1 then 
				SetLowerGIOn
				LowerGIBuzz.enabled = True
				Woosh.enabled = True
			else
				SetLowerGIOff	
				LowerGIBuzz.enabled = False
				LowerGIBuzzStep = 0
				Woosh.enabled = false
				StopSound "Woosh"
				WooshStep = 0
			end if
		Case 18: 											' Re-Entry Tube Gate
			if lstate = 1 then 
				Flipper1.rotatetoEnd
				SetLamp 138, 1
				SetFlash 138, 1
				SetLamp 139, 0
				SetFlash 139, 0
			else
				Flipper1.rotatetoStart
				SetLamp 138, 0
				SetFlash 138, 0
				SetLamp 139, 1
				SetFlash 139, 1
			end if
	End Select
End Sub


'******************************************************
'			LOWER PLAYFIELD TROUGH & KICKER 
'******************************************************

Sub LowerStage_Hit():If Controller.Switch(53) = 0 Then LowerStage.kick 60, 5:End If:End Sub

Sub sw53_Hit():Controller.Switch(53)=1:End Sub
Sub sw53_UnHit():If LowerStage.BallCntOver = 1 Then LowerStage.kick 60, 5:End If:End Sub

Sub sw42_Hit():playsound "kicker_enter":controller.Switch(42)=1:End Sub
Sub sw43_Hit():playsound "kicker_enter":controller.Switch(43)=1:SetLamp 123, 1:End Sub

Sub ReEnrtyTubeExit_hit():SetLamp 123, 0:End Sub


'******************************************************
'				UPPER PLAYFIELD TROUGH
'******************************************************

Sub Kicker6_Hit():UpdateTrough:End Sub
Sub Kicker5_Hit():UpdateTrough:End Sub
Sub Kicker4_Hit():UpdateTrough:End Sub
Sub Kicker3_Hit():Controller.Switch(25) = 1:UpdateTrough:End Sub
Sub Kicker3_UnHit():Controller.Switch(25) = 0:End Sub
Sub Kicker2_Hit()
	UpdateTrough
	If kicker1.BallCntOver = 1 Then
		If blacklightmultiball = 1 then
			StopBlackLightMultiball 
		End If
	End If
End Sub
Sub Kicker1_Hit():UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If kicker1.BallCntOver = 0 Then kicker2.kick 70,10
	If kicker2.BallCntOver = 0 Then kicker3.kick 70,10
	If kicker3.BallCntOver = 0 Then kicker4.kick 70,10
	If kicker4.BallCntOver = 0 Then kicker5.kick 70,10
	If kicker5.BallCntOver = 0 Then kicker6.kick 70,10
	Me.Enabled = 0
End Sub

'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub Drain_Hit()
	PlaySound "drain"
	Controller.Switch(15) = 1
End Sub

Sub SolOuthole(enabled)
	If enabled Then 
		Drain.kick 70,10
		PlaySound SoundFX(SSolenoidOn,DOFContactors)
		controller.switch(15) = false
	End If
End Sub

Sub ReleaseBall
	PlaySound SoundFX("BallRelease2",DOFContactors)
	Kicker1.Kick 70,5
	UpdateTrough
End Sub


'******************************************************
'			UPPER PLAYFIELD KICKER 
'******************************************************

Sub sw05_Hit():playsound "kicker_enter":controller.Switch(5)=1:End Sub

dim sw05Step

Sub sw05_Timer()
	Select Case sw05Step
		Case 0:	TWKicker.transY = 2
		Case 1: TWKicker.transY = 5
		Case 2: TWKicker.transY = 5
		Case 3: TWKicker.transY = 2
		Case 4:	TWKicker.transY = 0:Me.TimerEnabled = 0:sw05Step = 0
	End Select
	sw05Step = sw05Step + 1
End Sub


'******************************************************
'						FLIPPERS
'******************************************************

Sub SolLFlipper(Enabled)
	If Enabled Then
		PlaySound SoundFx("FlipperUp",DOFFlippers)
		if Controller.Lamp(16) = 0 Then
			LeftFlipper.RotateToEnd
			LeftFlipper2.RotateToEnd
		Else
			LeftFlipper.RotateToStart
			LeftFlipper2.RotateToStart
		End If
		If Controller.Lamp(17) Then
			FlipperLL.RotateToEnd
		Else	
			FlipperLL.RotateToStart
		End If
     Else
		PlaySound SoundFx("FlipperDown",DOFFlippers)
		LeftFlipper.RotateToStart
		LeftFlipper2.RotateToStart
		FlipperLL.RotateToStart
     End If
End Sub
  
Sub SolRFlipper(Enabled)
	If Enabled Then
		PlaySound SoundFx("FlipperUp",DOFFlippers)
		if Controller.Lamp(16) = 0 Then
			RightFlipper.RotateToEnd
			RightFlipper2.RotateToEnd
		Else
			RightFlipper.RotateToStart
			RightFlipper2.RotateToStart
		End If
		If Controller.Lamp(17) Then
			FlipperLR.RotateToEnd
		Else	
			FlipperLR.RotateToStart
		End If
     Else
		PlaySound SoundFx("FlipperDown",DOFFlippers)
		RightFlipper.RotateToStart
		RightFlipper2.RotateToStart
		FlipperLR.RotateToStart
     End If
End Sub

'******************************************************
' 						KEYS
'******************************************************

Sub BlackHole_KeyUp(ByVal KeyCode)
    If vpmKeyUp(KeyCode) Then Exit Sub  
    If KeyCode=PlungerKey Then PlaySound"Plunger":Plunger.Fire
End Sub  

Sub BlackHole_KeyDown(ByVal KeyCode)
    If vpmKeyDown(KeyCode) Then Exit Sub 
    If KeyCode=PlungerKey Then PlaySound"PullbackPlunger":Plunger.Pullback

'''''''''''''''''''''''''''
'' Test Keys
'''''''''''''''''''''''''''

	If keycode = 20 then  ''''''''''''''''''''T Key used for testing

	End If

	If keycode = 21 then  ''''''''''''''''''''Y Key used for testing
'UpperPF.TopMaterial = "PlayfieldInvisible"
'UpperPF2.TopMaterial = "PlayfieldInvisible"
'ShadowB.TopMaterial = "PlayfieldInvisible"
'ShadowL.TopMaterial = "PlayfieldInvisible"
'ShadowA.TopMaterial = "PlayfieldInvisible"
'ShadowC.TopMaterial = "PlayfieldInvisible"
'ShadowK.TopMaterial = "PlayfieldInvisible"
'ShadowH.TopMaterial = "PlayfieldInvisible"
'ShadowO.TopMaterial = "PlayfieldInvisible"
'ShadowL2.TopMaterial = "PlayfieldInvisible"
'ShadowE.TopMaterial = "PlayfieldInvisible"


	End If

	If keycode = 22 then  ''''''''''''''''''''U Key used for testing
'UpperPF.TopMaterial = "Playfield"
'UpperPF2.TopMaterial = "Playfield"
'ShadowB.TopMaterial = "Playfield"
'ShadowL.TopMaterial = "Playfield"
'ShadowA.TopMaterial = "Playfield"
'ShadowC.TopMaterial = "Playfield"
'ShadowK.TopMaterial = "Playfield"
'ShadowH.TopMaterial = "Playfield"
'ShadowO.TopMaterial = "Playfield"
'ShadowL2.TopMaterial = "Playfield"
'ShadowE.TopMaterial = "Playfield"

'		testkick.enabled = true
'		testkick.CreateBall
'		testkick.kick 340,30
''		testkick.kick 15,60
'		testkick.enabled = false
	End If
End Sub  


Sub Gate_TopLeft_Hit:vpmTimer.PulseSw(24):End Sub	'	OK! switch 0 
Sub Spinner1_Spin:vpmTimer.PulseSw (16):End Sub


'Confirmed Lights

Set Lights(3)=L3
Set Lights(4)=L4
Set Lights(5)=L5
Set Lights(7)=L7
Set Lights(19)=L19a	
Set Lights(20)=L20a
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
Set Lights(39)=L39c	
Set Lights(40)=L40	
Set Lights(41)=L41	
Set Lights(42)=L42
Set Lights(43)=L43	
Set Lights(44)=L44	
Set Lights(45)=L45	
Set Lights(46)=L46
Set Lights(48)=L48
Set Lights(49)=L49	
Set Lights(50)=L50
Set Lights(51)=L51	
Set Lights(138)=L39b
Set Lights(139)=L39c

'Turns blacklights on or off

Sub CheckBlackLights()

If BlackLightMultiball = 1 Then
	Blacklights.visible = False
	Blacklights_LP.visible = False
	Blacklight_apron.visible = False
Else

	If BlackLights_on = 1 then
		Blacklights.visible = true
	Else
		Blacklights.visible = false
	End If
	If LowerBlacklights_on = 1 then
		Blacklights_LP.visible = true
	Else
		Blacklights_LP.visible = false
	End If
	If BlackLightApron = 1 Then
		Blacklight_apron.visible = True
	Else
		Blacklight_apron.visible = false
	End If
	If BlackLightPlastics = 1 Then
		pUpperPlasticB.DisableLighting = True
		pUpperPlasticB.material = "AcrylicBLBlue"
		pLeftPlasticsB.DisableLighting = True
		pLeftPlasticsB.material = "AcrylicBLBlue"
		pRightPlasticsB.DisableLighting = True
		pRightPlasticsB.material = "AcrylicBLBlue"
		pLPFplasticsB.DisableLighting = True
		pLPFplasticsB.material = "AcrylicBLBlue"
	Else
		pUpperPlasticB.DisableLighting = False
		pUpperPlasticB.material = "AcrylicClear"
		pLeftPlasticsB.DisableLighting = False
		pLeftPlasticsB.material = "AcrylicClear"
		pRightPlasticsB.DisableLighting = False
		pRightPlasticsB.material = "AcrylicClear"
		pLPFplasticsB.DisableLighting = False
		pLPFplasticsB.material = "AcrylicClear"
	End If


End If

End Sub

Sub StartBlackLightMultiball()

		Blacklights.visible = true
		Blacklights_LP.visible = true
		Blacklight_apron.visible = True

		gi1.color = rgb(0,0,128)
		gi1.colorfull = rgb(181,106,255)
		gi2.color = rgb(0,0,128)
		gi2.colorfull = rgb(181,106,255)
		gi3.color = rgb(0,0,128)
		gi3.colorfull = rgb(181,106,255)
		gi4.color = rgb(0,0,128)
		gi4.colorfull = rgb(181,106,255)
		gi5.color = rgb(0,0,128)
		gi5.colorfull = rgb(181,106,255)
		gi6.color = rgb(0,0,128)
		gi6.colorfull = rgb(181,106,255)
		gi7.color = rgb(0,0,128)
		gi7.colorfull = rgb(181,106,255)
		gi8.color = rgb(0,0,128)
		gi8.colorfull = rgb(181,106,255)
		gi9.color = rgb(0,0,128)
		gi9.colorfull = rgb(181,106,255)
		gi10.color = rgb(0,0,128)
		gi10.colorfull = rgb(181,106,255)
		gi11.color = rgb(0,0,128)
		gi11.colorfull = rgb(181,106,255)

		If RedGravityTunnel = 1 then
			gi12.color = rgb(255,0,0)
			gi13.color = rgb(255,0,0)
			gi12.colorfull = rgb(255,0,0)
			gi13.colorfull = rgb(255,0,0)
		Else
			gi12.color = rgb(0,0,128)
			gi12.colorfull = rgb(181,106,255)
			gi13.color = rgb(0,0,128)
			gi13.colorfull = rgb(181,106,255)
		End If

		gi14.color = rgb(0,0,128)
		gi14.colorfull = rgb(181,106,255)
		gi15.color = rgb(0,0,128)
		gi15.colorfull = rgb(181,106,255)
		gi16.color = rgb(0,0,128)
		gi16.colorfull = rgb(181,106,255)
		gi17.color = rgb(0,0,128)
		gi17.colorfull = rgb(181,106,255)

		lgi1.ImageA = "haloBL"
		lgi2.ImageA = "haloBL"
		lgi3.ImageA = "haloBL"
		lgi4.ImageA = "haloBL"

		Flasher20.ImageA = "haloBL"
		Flasher21.ImageA = "haloBL"
		Flasher22.ImageA = "haloBL"
		Flasher23.ImageA = "haloBL"
		Flasher24.ImageA = "haloBL"
		Flasher25.ImageA = "haloBL"
		Flasher26.ImageA = "haloBL"
		Flasher27.ImageA = "haloBL"

End Sub


Sub StopBlackLightMultiball()

		Blacklights.visible = False
		Blacklights_LP.visible = False
		Blacklight_apron.visible = False

		gi1.color = rgb(255,255,255)
		gi1.colorfull = rgb(255,255,255)
		gi2.color = rgb(255,255,255)
		gi2.colorfull = rgb(255,255,255)
		gi3.color = rgb(255,255,255)
		gi3.colorfull = rgb(255,255,255)
		gi4.color = rgb(255,255,255)
		gi4.colorfull = rgb(255,255,255)
		gi5.color = rgb(255,255,255)
		gi5.colorfull = rgb(255,255,255)
		gi6.color = rgb(255,255,255)
		gi6.colorfull = rgb(255,255,255)
		gi7.color = rgb(255,255,255)
		gi7.colorfull = rgb(255,255,255)
		gi8.color = rgb(255,255,255)
		gi8.colorfull = rgb(255,255,255)
		gi9.color = rgb(255,255,255)
		gi9.colorfull = rgb(255,255,255)
		gi10.color = rgb(255,255,255)
		gi10.colorfull = rgb(255,255,255)
		gi11.color = rgb(255,255,255)
		gi11.colorfull = rgb(255,255,255)

		If RedGravityTunnel = 1 then
			gi12.color = rgb(255,0,0)
			gi13.color = rgb(255,0,0)
			gi12.colorfull = rgb(255,0,0)
			gi13.colorfull = rgb(255,0,0)
		Else
			gi12.color = rgb(255,255,255)
			gi12.colorfull = rgb(255,255,255)
			gi13.color = rgb(255,255,255)
			gi13.colorfull = rgb(255,255,255)
		End If

		gi14.color = rgb(255,255,255)
		gi14.colorfull = rgb(255,255,255)
		gi15.color = rgb(255,255,255)
		gi15.colorfull = rgb(255,255,255)
		gi16.color = rgb(255,255,255)
		gi16.colorfull = rgb(255,255,255)
		gi17.color = rgb(255,255,255)
		gi17.colorfull = rgb(255,255,255)

		lgi1.ImageA = "GIWhite"
		lgi2.ImageA = "GIWhite"
		lgi3.ImageA = "GIWhite"
		lgi4.ImageA = "GIWhite"

		Flasher20.ImageA = "GIWhite"
		Flasher21.ImageA = "GIWhite"
		Flasher22.ImageA = "GIWhite"
		Flasher23.ImageA = "GIWhite"
		Flasher24.ImageA = "GIWhite"
		Flasher25.ImageA = "GIWhite"
		Flasher26.ImageA = "GIWhite"
		Flasher27.ImageA = "GIWhite"
End Sub


'Check Day or Night

Sub CheckDayOrNight()
	If NightMod = 1 then
		Primitive10.image = "metal_rotated_night"
		Primitive8.image = "metal_night"
		Primitive7.image = "metal_night"
		Primitive9.image = "metal_rotated_night"
		Screw7.image = "screw_hex_night"
		Screw6.image = "screw_hex_night"
		Screw3.image = "screw_hex_night"
		Screw9.image = "screw_hex_night"
		Screw10.image = "screw_hex_night"
		Screw4.image = "screw_hex_night"
		Screw8.image = "screw_hex_night"
		Screw5.image = "screw_hex_night"
		Screw1.image = "screw_hex_night"
		Screw2.image = "screw_hex_night"
	Else
		Primitive10.image = "metal_rotated"
		Primitive8.image = "metal"
		Primitive7.image = "metal"
		Primitive9.image = "metal_rotated"
		Screw7.image = "screw_hex"
		Screw6.image = "screw_hex"
		Screw3.image = "screw_hex"
		Screw9.image = "screw_hex"
		Screw10.image = "screw_hex"
		Screw4.image = "screw_hex"
		Screw8.image = "screw_hex"
		Screw5.image = "screw_hex"
		Screw1.image = "screw_hex"
		Screw2.image = "screw_hex"
	End If

End Sub

Sub SetCaptiveLightColor()
	If CaptiveLight = 1 then LCaptiveLight.Color = rgb(0,255,255):LCaptiveLight.Color = rgb(0,255,255):CaptiveLightHalo.imageA = "haloBlue" End If  'blue
	If CaptiveLight = 2 then LCaptiveLight.Color = rgb(255,128,0):LCaptiveLight.Color = rgb(255,128,0):CaptiveLightHalo.imageA = "halo Orange" End If  'orange
	If CaptiveLight = 3 then LCaptiveLight.Color = rgb(0,255,0):LCaptiveLight.Color = rgb(0,255,0):CaptiveLightHalo.imageA = "haloGreen" End If  'green
	If CaptiveLight = 4 then LCaptiveLight.Color = rgb(136,17,255):LCaptiveLight.Color = rgb(136,17,255):CaptiveLightHalo.imageA = "haloPurple" End If  'purple
	If CaptiveLight = 5 then LCaptiveLight.Color = rgb(255,0,0):LCaptiveLight.Color = rgb(255,0,0):CaptiveLightHalo.imageA = "haloRed" End If  'red
	If CaptiveLight = 6 then LCaptiveLight.Color = rgb(255,255,255):LCaptiveLight.Color = rgb(255,255,128):CaptiveLightHalo.imageA = "haloWhite" End If  'white
	If CaptiveLight = 7 then LCaptiveLight.Color = rgb(255,255,0):LCaptiveLight.Color = rgb(255,255,0):CaptiveLightHalo.imageA = "haloYellow" End If  'yellow
End Sub


' Changes Flipper Colors

Sub CheckFlipperColors()
	If UpperFlipperColor = 0 then
			PLeftFlipper.image = "flipper_red_left"
			PLeftFlipper2.image = "flipper_red_left"
			PRightFlipper.image = "flipper_red_right"
			PRightFlipper2.image = "flipper_red_right"
	End If
	If UpperFlipperColor = 1 then
			PLeftFlipper.image = "flipper_black_left"
			PLeftFlipper2.image = "flipper_black_left"
			PRightFlipper.image = "flipper_black_right"
			PRightFlipper2.image = "flipper_black_right"
	End If
	If UpperFlipperColor = 2 then
			PLeftFlipper.image = "flipper_Yellow_left"
			PLeftFlipper2.image = "flipper_Yellow_left"
			PRightFlipper.image = "flipper_Yellow_right"
			PRightFlipper2.image = "flipper_Yellow_right"
	End If

	If UpperFlipperColor = 3 then
			PLeftFlipper.image = "flipper_Blue_left"
			PLeftFlipper2.image = "flipper_Blue_left"
			PRightFlipper.image = "flipper_Blue_right"
			PRightFlipper2.image = "flipper_Blue_right"
	End If

	If LowerFlipperColor = 0 then
			PLeftFlipperLL.image = "flipper_red_left"
			PLeftFlipperLR.image = "flipper_red_right"
	End If
	If LowerFlipperColor = 1 then
			PLeftFlipperLL.image = "flipper_black_left"
			PLeftFlipperLR.image = "flipper_black_right"
	End If
	If LowerFlipperColor = 2 then
			PLeftFlipperLL.image = "flipper_Yellow_left"
			PLeftFlipperLR.image = "flipper_Yellow_right"
	End If
	If LowerFlipperColor = 3 then
			PLeftFlipperLL.image = "flipper_Blue_left"
			PLeftFlipperLR.image = "flipper_Blue_right"
	End If
End Sub



' Cheaters

Sub CheckCheaters()
	If UpperPeg = 1 then
		Primitive41.Visible = true
		Rubber35.Collidable = true
		Rubber35.Visible = True
	Else
		Primitive41.Visible = False
		Rubber35.Collidable = False
		Rubber35.Visible = False
	End If

	If LowerPeg = 1 then
		PegMetalT2.Visible = true
		Rubber36.Collidable = true
		Rubber36.Visible = True
	Else
		PegMetalT2.Visible = False
		Rubber36.Collidable = False
		Rubber36.Visible = False
	End If

	
	If LeftDrain = 1 then
		Rubber41.visible = True
		Rubber40.visible = true
		Rubber42.visible = false
		LeftDrainRubber3a.visible = False
		LeftDrainRubber3a.collidable = False
		wLeftDrainC.isdropped = true
		wLeftDrainB.isdropped = true
		Screw8.visible = false
		PegPlasticT8Twin3.visible = false
		PegPlasticT8.visible = true
	End If
	If LeftDrain = 2 then
		Rubber41.visible = True
		Rubber40.visible = false
		Rubber42.visible = true
		LeftDrainRubber3a.visible = False
		LeftDrainRubber3a.collidable = False
		wLeftDrainC.isdropped = true
		wLeftDrainB.isdropped = true
		Screw8.visible = true
		PegPlasticT8Twin3.visible = true
		PegPlasticT8.visible = false
	End If
	If LeftDrain = 3 then
		Rubber41.visible = false
		Rubber40.visible = false
		Rubber42.visible = false
		LeftDrainRubber3a.visible = true
		LeftDrainRubber3a.collidable = true
		wLeftDrainC.isdropped = false
		wLeftDrainB.isdropped = false
		Screw8.visible = true
		PegPlasticT8Twin3.visible = true
		PegPlasticT8.visible = false
	End If
End Sub

Dim LowerGIBuzzStep

Sub LowerGIBuzz_timer()
	Select Case LowerGIBuzzStep
		Case 1:	If GIbuzz = 1 then PlaySound "LowerGIon" End If
		Case 2: 
		Case 3: 
		Case 4:	LowerGIBuzzStep = 2
	End Select
	LowerGIBuzzStep = LowerGIBuzzStep + 1
End Sub


Dim WooshStep

Sub Woosh_timer()
	Select Case WooshStep
		Case 1:	
		Case 2: 
		Case 3: If WhooshSound = 1 then PlaySound "Woosh" End If
		Case 4:
		Case 5:
		Case 6:
		Case 7:
		Case 8:WooshStep = 0 
	End Select
	WooshStep = WooshStep + 1
End Sub


''''''''''''''''''''''''''''''''''''
'''''''''' GI ColorMod
''''''''''''''''''''''''''''''''''''

Sub CheckGIColorMod()

	If BlackLightMultiball = 0 then
	If GIColorMod = 0 then
		gi1.color = rgb(255,255,255)
		gi1.colorfull = rgb(255,255,255)
		gi2.color = rgb(255,255,255)
		gi2.colorfull = rgb(255,255,255)
		gi3.color = rgb(255,255,255)
		gi3.colorfull = rgb(255,255,255)
		gi4.color = rgb(255,255,255)
		gi4.colorfull = rgb(255,255,255)
		gi5.color = rgb(255,255,255)
		gi5.colorfull = rgb(255,255,255)
		gi6.color = rgb(255,255,255)
		gi6.colorfull = rgb(255,255,255)
		gi7.color = rgb(255,255,255)
		gi7.colorfull = rgb(255,255,255)
		gi8.color = rgb(255,255,255)
		gi8.colorfull = rgb(255,255,255)
		gi9.color = rgb(255,255,255)
		gi9.colorfull = rgb(255,255,255)
		gi10.color = rgb(255,255,255)
		gi10.colorfull = rgb(255,255,255)
		gi11.color = rgb(255,255,255)
		gi11.colorfull = rgb(255,255,255)

		If RedGravityTunnel = 1 then
			gi12.color = rgb(255,0,0)
			gi13.color = rgb(255,0,0)
			gi12.colorfull = rgb(255,0,0)
			gi13.colorfull = rgb(255,0,0)
		Else
			gi12.color = rgb(255,255,255)
			gi12.colorfull = rgb(255,255,255)
			gi13.color = rgb(255,255,255)
			gi13.colorfull = rgb(255,255,255)
		End If

		gi14.color = rgb(255,255,255)
		gi14.colorfull = rgb(255,255,255)
		gi15.color = rgb(255,255,255)
		gi15.colorfull = rgb(255,255,255)
		gi16.color = rgb(255,255,255)
		gi16.colorfull = rgb(255,255,255)
		gi17.color = rgb(255,255,255)
		gi17.colorfull = rgb(255,255,255)
	End If
	If GIColorMod = 1 then
		gi1.color = rgb(0,0,128)
		gi1.colorfull = rgb(181,106,255)
		gi2.color = rgb(0,0,128)
		gi2.colorfull = rgb(181,106,255)
		gi3.color = rgb(0,0,128)
		gi3.colorfull = rgb(181,106,255)
		gi4.color = rgb(0,0,128)
		gi4.colorfull = rgb(181,106,255)
		gi5.color = rgb(0,0,128)
		gi5.colorfull = rgb(181,106,255)
		gi6.color = rgb(0,0,128)
		gi6.colorfull = rgb(181,106,255)
		gi7.color = rgb(0,0,128)
		gi7.colorfull = rgb(181,106,255)
		gi8.color = rgb(0,0,128)
		gi8.colorfull = rgb(181,106,255)
		gi9.color = rgb(0,0,128)
		gi9.colorfull = rgb(181,106,255)
		gi10.color = rgb(0,0,128)
		gi10.colorfull = rgb(181,106,255)
		gi11.color = rgb(0,0,128)
		gi11.colorfull = rgb(181,106,255)

		If RedGravityTunnel = 1 then
			gi12.color = rgb(255,0,0)
			gi13.color = rgb(255,0,0)
			gi12.colorfull = rgb(255,0,0)
			gi13.colorfull = rgb(255,0,0)
		Else
			gi12.color = rgb(0,0,128)
			gi12.colorfull = rgb(181,106,255)
			gi13.color = rgb(0,0,128)
			gi13.colorfull = rgb(181,106,255)
		End If

		gi14.color = rgb(0,0,128)
		gi14.colorfull = rgb(181,106,255)
		gi15.color = rgb(0,0,128)
		gi15.colorfull = rgb(181,106,255)
		gi16.color = rgb(0,0,128)
		gi16.colorfull = rgb(181,106,255)
		gi17.color = rgb(0,0,128)
		gi17.colorfull = rgb(181,106,255)

	End If

	If GIColorModLower = 0 Then
		LPFoverhead.Color = rgb(255,255,128)
		LPFoverhead.Color = rgb(255,255,255)

		lgi1.ImageA = "GIWhite"
		lgi2.ImageA = "GIWhite"
		lgi3.ImageA = "GIWhite"
		lgi4.ImageA = "GIWhite"

		Flasher20.ImageA = "GIWhite"
		Flasher21.ImageA = "GIWhite"
		Flasher22.ImageA = "GIWhite"
		Flasher23.ImageA = "GIWhite"
		Flasher24.ImageA = "GIWhite"
		Flasher25.ImageA = "GIWhite"
		Flasher26.ImageA = "GIWhite"
		Flasher27.ImageA = "GIWhite"

		Flasher20.ImageB = "GIWhite"
		Flasher21.ImageB = "GIWhite"
		Flasher22.ImageB = "GIWhite"
		Flasher23.ImageB = "GIWhite"
		Flasher24.ImageB = "GIWhite"
		Flasher25.ImageB = "GIWhite"
		Flasher26.ImageB = "GIWhite"
		Flasher27.ImageB = "GIWhite"
	End If

	If GIColorModLower = 1 Then
		LPFoverhead.Color = rgb(0,128,255)
		LPFoverhead.Color = rgb(130,192,255)

		lgi1.ImageA = "HaloBlue"
		lgi2.ImageA = "HaloBlue"
		lgi3.ImageA = "HaloBlue"
		lgi4.ImageA = "HaloBlue"

		Flasher20.ImageA = "HaloBlue"
		Flasher21.ImageA = "HaloBlue"
		Flasher22.ImageA = "HaloBlue"
		Flasher23.ImageA = "HaloBlue"
		Flasher24.ImageA = "HaloBlue"
		Flasher25.ImageA = "HaloBlue"
		Flasher26.ImageA = "HaloBlue"
		Flasher27.ImageA = "HaloBlue"

		Flasher20.ImageB = "HaloBlue"
		Flasher21.ImageB = "HaloBlue"
		Flasher22.ImageB = "HaloBlue"
		Flasher23.ImageB = "HaloBlue"
		Flasher24.ImageB = "HaloBlue"
		Flasher25.ImageB = "HaloBlue"
		Flasher26.ImageB = "HaloBlue"
		Flasher27.ImageB = "HaloBlue"
	End If

	If GIColorModLower = 2 Then
		LPFoverhead.Color = rgb(128,0,255)
		LPFoverhead.Color = rgb(179,102,255)

		lgi1.ImageA = "haloBL"
		lgi2.ImageA = "haloBL"
		lgi3.ImageA = "haloBL"
		lgi4.ImageA = "haloBL"

		Flasher20.ImageA = "haloBL"
		Flasher21.ImageA = "haloBL"
		Flasher22.ImageA = "haloBL"
		Flasher23.ImageA = "haloBL"
		Flasher24.ImageA = "haloBL"
		Flasher25.ImageA = "haloBL"
		Flasher26.ImageA = "haloBL"
		Flasher27.ImageA = "haloBL"

		Flasher20.ImageB = "haloBL"
		Flasher21.ImageB = "haloBL"
		Flasher22.ImageB = "haloBL"
		Flasher23.ImageB = "haloBL"
		Flasher24.ImageB = "haloBL"
		Flasher25.ImageB = "haloBL"
		Flasher26.ImageB = "haloBL"
		Flasher27.ImageB = "haloBL"
	End If
	Else
		gi1.color = rgb(255,255,255)
		gi1.colorfull = rgb(255,255,255)
		gi2.color = rgb(255,255,255)
		gi2.colorfull = rgb(255,255,255)
		gi3.color = rgb(255,255,255)
		gi3.colorfull = rgb(255,255,255)
		gi4.color = rgb(255,255,255)
		gi4.colorfull = rgb(255,255,255)
		gi5.color = rgb(255,255,255)
		gi5.colorfull = rgb(255,255,255)
		gi6.color = rgb(255,255,255)
		gi6.colorfull = rgb(255,255,255)
		gi7.color = rgb(255,255,255)
		gi7.colorfull = rgb(255,255,255)
		gi8.color = rgb(255,255,255)
		gi8.colorfull = rgb(255,255,255)
		gi9.color = rgb(255,255,255)
		gi9.colorfull = rgb(255,255,255)
		gi10.color = rgb(255,255,255)
		gi10.colorfull = rgb(255,255,255)
		gi11.color = rgb(255,255,255)
		gi11.colorfull = rgb(255,255,255)
		If RedGravityTunnel = 1 then
			gi12.color = rgb(255,0,0)
			gi13.color = rgb(255,0,0)
			gi12.colorfull = rgb(255,0,0)
			gi13.colorfull = rgb(255,0,0)
		Else
			gi12.color = rgb(255,255,255)
			gi12.colorfull = rgb(255,255,255)
			gi13.color = rgb(255,255,255)
			gi13.colorfull = rgb(255,255,255)
		End If
		gi14.color = rgb(255,255,255)
		gi14.colorfull = rgb(255,255,255)
		gi15.color = rgb(255,255,255)
		gi15.colorfull = rgb(255,255,255)
		gi16.color = rgb(255,255,255)
		gi16.colorfull = rgb(255,255,255)
		gi17.color = rgb(255,255,255)
		gi17.colorfull = rgb(255,255,255)

		lgi1.ImageA = "GIWhite"
		lgi2.ImageA = "GIWhite"
		lgi3.ImageA = "GIWhite"
		lgi4.ImageA = "GIWhite"

		Flasher20.ImageA = "GIWhite"
		Flasher21.ImageA = "GIWhite"
		Flasher22.ImageA = "GIWhite"
		Flasher23.ImageA = "GIWhite"
		Flasher24.ImageA = "GIWhite"
		Flasher25.ImageA = "GIWhite"
		Flasher26.ImageA = "GIWhite"
		Flasher27.ImageA = "GIWhite"
	End If

	If UltraBrightBumpers = 0 Then
		BLight1b.Intensity = 3
		BLight1b.color = rgb(128,201,255)
		BLight1b.colorfull = rgb(215,238,255)
		BLight2b.Intensity = 3
		BLight2b.color = rgb(128,201,255)
		BLight2b.colorfull = rgb(215,238,255)
		BLight3b.Intensity = 3
		BLight3b.color = rgb(128,201,255)
		BLight3b.colorfull = rgb(215,238,255)
		BLight4b.Intensity = 3
		BLight4b.color = rgb(128,201,255)
		BLight4b.colorfull = rgb(215,238,255)
	End If

	If UltraBrightBumpers = 1 Then
		BLight1b.Intensity = 8
		BLight1b.color = rgb(128,201,255)
		BLight1b.colorfull = rgb(215,238,255)
		BLight2b.Intensity = 8
		BLight2b.color = rgb(128,201,255)
		BLight2b.colorfull = rgb(215,238,255)
		BLight3b.Intensity = 8
		BLight3b.color = rgb(128,201,255)
		BLight3b.colorfull = rgb(215,238,255)
		BLight4b.Intensity = 8
		BLight4b.color = rgb(128,201,255)
		BLight4b.colorfull = rgb(215,238,255)
	End If

	If UltraBrightBumpers = 2 Then
		BLight1b.Intensity = 10
		BLight1b.TransmissionScale = .8
		BLight1b.color = rgb(7,91,184)
		BLight1b.colorfull = rgb(5,165,250)
		BLight2b.Intensity = 10
		BLight2b.TransmissionScale = .8
		BLight2b.color = rgb(7,91,184)
		BLight2b.colorfull = rgb(5,165,250)
		BLight3b.Intensity = 10
		BLight3b.TransmissionScale = .8
		BLight3b.color = rgb(7,91,184)
		BLight3b.colorfull = rgb(5,165,250)
		BLight4b.Intensity = 10
		BLight4b.TransmissionScale = .8
		BLight4b.color = rgb(7,91,184)
		BLight4b.colorfull = rgb(5,165,250)
	End If

End Sub

Sub CheckInstructionCards ()

If CustomCards = 1 Then


	If Language = 1 Then
		If NumOfBalls = 0 Then
			LeftInstuctionCard.image = "bh_instuctionLCE_3ball"
		Else
			LeftInstuctionCard.image = "bh_instuctionLCE_3ball"
		End If
	End If

	If Language = 2 Then
		If NumOfBalls = 0 Then
			LeftInstuctionCard.image = "bh_instuctionLCF_3ball"
		Else
			LeftInstuctionCard.image = "bh_instuctionLCF_3ball"
		End If
	End If

	If Language = 3 Then
		If NumOfBalls = 0 Then
			LeftInstuctionCard.image = "bh_instuctionLCG_3ball"
		Else
			LeftInstuctionCard.image = "bh_instuctionLCG_3ball"
		End If
	End If

 ' 1 - English  2 - French  3 - German  
 ' 3or5ball = 1  0 - 3ball game   1 - 5 ball game

	If ReplayOption = 0 then
		If NumOfBalls = 0 Then
			RightInstuctionCard.image = "bh_instuctionRC_3ball"
		Else
			RightInstuctionCard.image = "bh_instuctionRC_5ball"
		End If
	Else
		If NumOfBalls = 0 Then
			RightInstuctionCard.image = "bh_instuctionRCR_3ball"
		Else
			RightInstuctionCard.image = "bh_instuctionRCR_5ball"
		End If
	End If

Else

	If Language = 1 Then
		If NumOfBalls = 0 Then
			LeftInstuctionCard.image = "bh_instuctionLE_3ball"
		Else
			LeftInstuctionCard.image = "bh_instuctionLE_3ball"
		End If
	End If

	If Language = 2 Then
		If NumOfBalls = 0 Then
			LeftInstuctionCard.image = "bh_instuctionLF_3ball"
		Else
			LeftInstuctionCard.image = "bh_instuctionLF_3ball"
		End If
	End If

	If Language = 3 Then
		If NumOfBalls = 0 Then
			LeftInstuctionCard.image = "bh_instuctionLG_3ball"
		Else
			LeftInstuctionCard.image = "bh_instuctionLG_3ball"
		End If
	End If

 ' 1 - English  2 - French  3 - German  
'3or5ball = 1  ' 0 - 3ball game   1 - 5 ball game


	If ReplayOption = 0 then
		If NumOfBalls = 0 Then
			RightInstuctionCard.image = "bh_instuctionR_3ball"
		Else
			RightInstuctionCard.image = "bh_instuctionR_5ball"
		End If
	Else
		If NumOfBalls = 0 Then
			RightInstuctionCard.image = "bh_instuctionRR_3ball"
		Else
			RightInstuctionCard.image = "bh_instuctionRR_5ball"
		End If
	End If

End If

End Sub


Sub SetWindowsFoam()

	If WindowFoam = 1 Then
		pWindowFome.image = "WindowsFome2"
	Else
		pWindowFome.image = "WindowsFome"
	End If

End Sub


''''''''''''''''''''''''''''''''''''''''''''''''''
''''Primitive Flippers, Diverter, and gates
''''''''''''''''''''''''''''''''''''''''''''''''''

Const PI = 3.14
Dim GateSpeed,Gate1Open,Gate1Angle,Gate2Open,Gate2Angle,Gate4Open,Gate4Angle
Gate2Open=0:Gate2Angle=0
Gate1Open=0:Gate1Angle=0:GateSpeed = 5

Sub Gate1_Hit():Gate1Open=1:Gate1Angle=0:PlaySound "fx_gate":End Sub
Sub Gate2_Hit():Gate2Open=1:Gate2Angle=0:PlaySound "fx_gate":End Sub
Sub Gate4_Hit():Gate4Open=1:Gate4Angle=0:PlaySound "fx_gate":End Sub

dim defaultEOS,EOSAngle,EOSTorque
defaulteos = leftflipper.eostorque
EOSAngle = 3
EOSTorque = 0.9


Sub Timer_Timer()
	pFlipper1.Roty = Flipper1.Currentangle
	pLeftFlipper.Roty = LeftFlipper.Currentangle
	pLeftFlipper2.Roty = LeftFlipper2.Currentangle
	pRightFlipper.Roty = RightFlipper.Currentangle
	pRightFlipper2.Roty = RightFlipper2.Currentangle
	PLeftFlipperLL.Roty = FlipperLL.Currentangle
	PLeftFlipperLR.Roty = FlipperLR.Currentangle

	If LeftFlipper.CurrentAngle < LeftFlipper.EndAngle + EOSAngle Then
		LeftFlipper.eostorque = EOSTorque
	Else
		LeftFlipper.eostorque = defaultEOS
	End If

	If RightFlipper.CurrentAngle > RightFlipper.EndAngle - EOSAngle Then
		RightFlipper.eostorque = EOSTorque
	Else
		RightFlipper.eostorque = defaultEOS
	End If

	If LeftFlipper2.CurrentAngle < LeftFlipper2.EndAngle + EOSAngle Then
		LeftFlipper2.eostorque = EOSTorque
	Else
		LeftFlipper2.eostorque = defaultEOS
	End If

	If RightFlipper2.CurrentAngle > RightFlipper2.EndAngle - EOSAngle Then
		RightFlipper2.eostorque = EOSTorque
	Else
		RightFlipper2.eostorque = defaultEOS
	End If

	If FlipperLR.CurrentAngle < FlipperLR.EndAngle + EOSAngle Then
		FlipperLR.eostorque = EOSTorque
	Else
		FlipperLR.eostorque = defaultEOS
	End If

	If FlipperLL.CurrentAngle > FlipperLL.EndAngle - EOSAngle Then
		FlipperLL.eostorque = EOSTorque
	Else
		FlipperLL.eostorque = defaultEOS
	End If

	If Gate1Open Then
		If Gate1Angle < Gate1.currentangle Then:Gate1Angle=Gate1.currentangle:End If
		If Gate1Angle > 5 and Gate1.currentangle < 5 Then:Gate1Open=0:End If

		pV_gate1.RotZ = Gate1Angle - 20
		Gate1Angle=Gate1Angle - GateSpeed
	Else
		if Gate1Angle > 0 Then
			Gate1Angle = Gate1Angle - GateSpeed
		Else 
			Gate1Angle = 0
		End If
		pV_gate1.RotZ = Gate1Angle - 5
	End If

	If Gate2Open Then
		If Gate2Angle < Gate2.currentangle Then:Gate2Angle=Gate2.currentangle:End If
		If Gate2Angle > 5 and Gate2.currentangle < 5 Then:Gate2Open=0:End If

		pV_gate2.RotZ = Gate2Angle - 20
		Gate2Angle=Gate2Angle - GateSpeed
	Else
		if Gate2Angle > 0 Then
			Gate2Angle = Gate2Angle - GateSpeed
		Else 
			Gate2Angle = 0
		End If
		pV_gate2.RotZ = Gate2Angle - 5
	End If

	If Gate4Open Then
		If Gate4Angle < Gate4.currentangle Then:Gate4Angle=Gate4.currentangle:End If
		If Gate4Angle > 5 and Gate4.currentangle < 5 Then:Gate4Open=0:End If

		p_gate4.RotX = Gate4Angle + 100
		Gate4Angle=Gate4Angle - GateSpeed
	Else
		if Gate4Angle > 0 Then
			Gate4Angle = Gate4Angle - GateSpeed
		Else 
			Gate4Angle = 0
		End If
		p_gate4.RotX = Gate4Angle + 100
	End If

    p_gate5.Rotx = gate5.CurrentAngle + 90
	pScoreSpring.TransX = sin( (gate5.CurrentAngle+180) * (2*PI/360)) * 5
	pScoreSpring.TransY = sin( (gate5.CurrentAngle- 90) * (2*PI/360)) * 5

	pSpinner.RotX = -(spinner1.currentangle)

	pSpinnerRod.TransX = sin( (spinner1.CurrentAngle+180) * (2*PI/360)) * 5
	pSpinnerRod.TransY = sin( (spinner1.CurrentAngle- 90) * (2*PI/360)) * 5
End Sub


'*********************************************
'Roll over switches
'*********************************************


'''''Upper

Sub Tri_TopLeft_Hit:Controller.Switch(0)=1:TWTri_TopLeft.transX = -5:End Sub	
Sub Tri_TopLeft_unHit:Controller.Switch(0)=0:TWTri_TopLeft.transX = 0:End Sub


Sub Tri_TopMid_Hit:Controller.Switch(10)=1:TWTri_TopMid.transX = -5:End Sub	 
Sub Tri_TopMid_unHit:Controller.Switch(10)=0:TWTri_TopMid.transX = 0:End Sub


Sub Tri_TopRight_Hit:Controller.Switch(20)=1:TWTri_TopRight.transX = -5:End Sub 
Sub Tri_TopRight_unHit:Controller.Switch(20)=0:TWTri_TopRight.transX = 0:End Sub


Sub Tri_MidRightLane_Hit:Controller.Switch(30)=1:TWTri_MidRightLane.transX = -5:End Sub	 
Sub Tri_MidRightLane_unHit:Controller.Switch(30)=0:TWTri_MidRightLane.transX = 0:End Sub


Sub Tri_BlackHole_Hit:Controller.Switch(33)=1:TWBlackHole.transX = -5:End Sub 

Sub Tri_BlackHole_unHit:Controller.Switch(33)=0:TWBlackHole.transX = 0:End Sub


Sub Tri_LeftInlane_Hit:Controller.Switch(35)=1:TWLeftInlane.transX = -5:End Sub	 
Sub Tri_LeftInlane_unHit:Controller.Switch(35)=0:TWLeftInlane.transX = 0:End Sub


'''''' Lower

Sub Tri_sw62_Hit:Controller.Switch(62)=1:End Sub	 
Sub Tri_sw62_unHit:Controller.Switch(62)=0:End Sub

Sub sw52_Hit:Controller.Switch(52)=1:End Sub	
Sub sw52_unHit:Controller.Switch(52)=0:End Sub


'''''''''''''''''''''''''''''''''
'  Yellow targets
'''''''''''''''''''''''''''''''''
Dim Tsw01Step,Tsw11Step,Tsw21Step,Tsw31Step


Sub Tsw01_Hit:vpmTimer.PulseSw(1):PTarget1.TransY = -5:Tsw01Step = 1:PlaySound SoundFX("fx_target",DOFTargets):Me.TimerEnabled = 1:End Sub			
Sub Tsw01_timer()
	Select Case Tsw01Step
		Case 1:PTarget1.TransY = 3
        Case 2:PTarget1.TransY = -2
        Case 3:PTarget1.TransY = 1
        Case 4:PTarget1.TransY = 0:Me.TimerEnabled = 0
     End Select
	Tsw01Step = Tsw01Step + 1
End Sub


Sub Tsw11_Hit:vpmTimer.PulseSw(11):PTarget2.TransY = -5:Tsw11Step = 1:PlaySound SoundFX("fx_target",DOFTargets):Me.TimerEnabled = 1:End Sub			
Sub Tsw11_timer()
	Select Case Tsw11Step
		Case 1:PTarget2.TransY = 3
        Case 2:PTarget2.TransY = -2
        Case 3:PTarget2.TransY = 1
        Case 4:PTarget2.TransY = 0:Me.TimerEnabled = 0
     End Select
	Tsw11Step = Tsw11Step + 1
End Sub

Sub Tsw21_Hit:vpmTimer.PulseSw(21):PTarget3.TransY = -5:Tsw21Step = 1:PlaySound SoundFX("fx_target",DOFTargets):Me.TimerEnabled = 1:End Sub			
Sub Tsw21_timer()
	Select Case Tsw21Step
		Case 1:PTarget3.TransY = 3
        Case 2:PTarget3.TransY = -2
        Case 3:PTarget3.TransY = 1
        Case 4:PTarget3.TransY = 0:Me.TimerEnabled = 0
     End Select
	Tsw21Step = Tsw21Step + 1
End Sub


Sub Tsw31_Hit:vpmTimer.PulseSw(31):PTarget4.TransY = -5:Tsw31Step = 1:PlaySound SoundFX("fx_target",DOFTargets):Me.TimerEnabled = 1:End Sub			
Sub Tsw31_timer()
	Select Case Tsw31Step
		Case 1:PTarget4.TransY = 3
        Case 2:PTarget4.TransY = -2
        Case 3:PTarget4.TransY = 1
        Case 4:PTarget4.TransY = 0:Me.TimerEnabled = 0
     End Select
	Tsw31Step = Tsw31Step + 1
End Sub


'''''''''''''''''''''''''''''''''
'Drop targets
'''''''''''''''''''''''''''''''''

'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'   Upper  Black
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Sub TargetB_Hit:vpmTimer.PulseSw 3:TargetB.IsDropped = true:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetB_Timer::ShadowB.IsDropped = 1:Me.TimerEnabled = 0 'This Drops The Target
End Sub

Sub TargetL_Hit:vpmTimer.PulseSw 13:TargetL.IsDropped = true:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetL_Timer:ShadowL.IsDropped = 1:Me.TimerEnabled = 0 'This Drops The Target
End Sub

Sub TargetA_Hit:vpmTimer.PulseSw 23:TargetA.IsDropped = true:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetA_Timer:ShadowA.IsDropped = 1:Me.TimerEnabled = 0 'This Drops The Target
End Sub

Sub TargetC_Hit:vpmTimer.PulseSw 4:TargetC.IsDropped = true:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetC_Timer:ShadowC.IsDropped = 1:Me.TimerEnabled = 0 'This Drops The Target
End Sub

Sub TargetK_Hit:vpmTimer.PulseSw 14:TargetK.IsDropped = true:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetK_Timer:ShadowK.IsDropped = 1:Me.TimerEnabled = 0 'This Drops The Target
End Sub


Sub BlackTargetsUp(Enabled)
	PlaySound SoundFX("droptargetreset",DOFContactors)
	TargetB.IsDropped=False:ShadowB.IsDropped = 0
	TargetL.IsDropped=False:ShadowL.IsDropped = 0
	TargetA.IsDropped=False:ShadowA.IsDropped = 0
	TargetC.IsDropped=False:ShadowC.IsDropped = 0
	TargetK.IsDropped=False:ShadowK.IsDropped = 0
End Sub

'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'   Upper  Hole
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Sub TargetH_Hit:vpmTimer.PulseSw 2:TargetH.IsDropped=True:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetH_Timer:ShadowH.IsDropped = 1:Me.TimerEnabled = 0 'This Drops The Target
End Sub

Sub TargetO_Hit:vpmTimer.PulseSw 12:TargetO.IsDropped=True:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetO_Timer:ShadowO.IsDropped = 1:Me.TimerEnabled = 0 'This Drops The Target
End Sub

Sub TargetL2_Hit:vpmTimer.PulseSw 22:TargetL2.IsDropped=True:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetL2_Timer:ShadowL2.IsDropped = 1:Me.TimerEnabled = 0 'This Drops The Target
End Sub

Sub TargetE_Hit:vpmTimer.PulseSw 32:TargetE.IsDropped=True:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetE_Timer:ShadowE.IsDropped = 1:Me.TimerEnabled = 0 'This Drops The Target
End Sub

Sub HoleTargetsUp(Enabled)
	PlaySound SoundFX("droptargetreset",DOFContactors)
	TargetH.IsDropped=False:ShadowH.IsDropped = 0
	TargetO.IsDropped=False:ShadowO.IsDropped = 0
	TargetL2.IsDropped=False:ShadowL2.IsDropped = 0
	TargetE.IsDropped=False:ShadowE.IsDropped = 0
End Sub


'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'   Lower Yellow
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Sub TargetLL40_Hit:vpmTimer.PulseSw 40:pDT_LL40.Z = 220:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetLL40_Timer:pDT_LL40.Z = 195:Me.TimerEnabled = 0:TargetLL40.IsDropped=True 'This Drops The Target
End Sub

Sub TargetLL50_Hit:vpmTimer.PulseSw 50:pDT_LL50.Z = 210:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetLL50_Timer:pDT_LL50.Z = 185:Me.TimerEnabled = 0:TargetLL50.IsDropped=True 'This Drops The Target
End Sub

Sub TargetLL60_Hit:vpmTimer.PulseSw 60:pDT_LL60.Z = 200:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetLL60_Timer:pDT_LL60.Z = 175:Me.TimerEnabled = 0:TargetLL60.IsDropped=True 'This Drops The Target
End Sub

Sub TargetLL70_Hit:vpmTimer.PulseSw 70:pDT_LL70.Z = 190:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetLL70_Timer:pDT_LL70.Z = 165:Me.TimerEnabled = 0:TargetLL70.IsDropped=True 'This Drops The Target
End Sub

Sub YellowTargetsUp(Enabled)
	PlaySound SoundFX("droptargetreset",DOFContactors)
	TargetLL40.IsDropped=False:pDT_LL40.Z = 240
	TargetLL50.IsDropped=False:pDT_LL50.Z = 230
	TargetLL60.IsDropped=False:pDT_LL60.Z = 220
	TargetLL70.IsDropped=False:pDT_LL70.Z = 210
End Sub


'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'   Lower White
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Sub TargetLR41_Hit:vpmTimer.PulseSw 41:pDT_LL41.Z = 175:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetLR41_Timer:pDT_LL41.Z = 155:Me.TimerEnabled = 0:TargetLR41.IsDropped=True 'This Drops The Target
End Sub

Sub TargetLR51_Hit:vpmTimer.PulseSw 51:pDT_LL51.Z = 180:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetLR51_Timer:pDT_LL51.Z = 160:Me.TimerEnabled = 0:TargetLR51.IsDropped=True 'This Drops The Target
End Sub

Sub TargetLR61_Hit:vpmTimer.PulseSw 61:pDT_LL61.Z = 185:Me.TimerEnabled = 1:PlaySound SoundFX("droptarget",DOFDropTargets):End Sub
Sub TargetLR61_Timer:pDT_LL61.Z = 165:Me.TimerEnabled = 0:TargetLR61.IsDropped=True 'This Drops The Target
End Sub

Sub WhiteTargetsUp(Enabled)
	PlaySound SoundFX("droptargetreset",DOFContactors)
	TargetLR41.IsDropped=False:pDT_LL41.Z = 205
	TargetLR51.IsDropped=False:pDT_LL51.Z = 210
	TargetLR61.IsDropped=False:pDT_LL61.Z = 215
End Sub


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''Bumpers
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Dim bump1,bump2,bump3,bump4,bump5,bump6



Sub Bumper1_Hit:vpmTimer.PulseSw(6):bump1 = 1:PlaySound SoundFX("DR BumperL",DOFContactors):DOF 101, DOFPulse:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw(6):bump2 = 1:PlaySound SoundFX("Bumper",DOFContactors):DOF 102, DOFPulse:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw(6):bump3 = 1:PlaySound SoundFX("DR BumperR",DOFContactors):DOF 103, DOFPulse:End Sub
Sub Bumper4_Hit:vpmTimer.PulseSw(6):bump4 = 1:PlaySound SoundFX("DR BumperL",DOFContactors):DOF 104, DOFPulse:End Sub
Sub Bumper5_Hit:vpmTimer.PulseSw(71):bump5 = 1:PlaySound SoundFX("DR BumperL",DOFContactors):DOF 105, DOFPulse:End Sub
Sub Bumper6_Hit:vpmTimer.PulseSw(71):bump6 = 1:PlaySound SoundFX("DR BumperR",DOFContactors):DOF 106, DOFPulse:End Sub

     
       Sub Bumper1_Timer()
           Select Case bump1
               Case 1:BR1.z = 300:
               Case 2:BR1.z = 285:
               Case 3:BR1.z = 270:
               Case 4:BR1.z = 260:				
               Case 5:BR1.z = 270:
               Case 6:BR1.z = 285:
               Case 7:BR1.z = 300:
               Case 8:BR1.z = 310:Me.TimerEnabled = 0
           End Select
		Bump1 = bump1 + 1
       End Sub

       Sub Bumper2_Timer()
           Select Case bump2
               Case 1:BR2.z = 300:
               Case 2:BR2.z = 285:
               Case 3:BR2.z = 270:
               Case 4:BR2.z = 260:
               Case 5:BR2.z = 270:
               Case 6:BR2.z = 285:
               Case 7:BR2.z = 300:
               Case 8:BR2.z = 310:Me.TimerEnabled = 0
           End Select
		Bump2 = bump2 + 1
       End Sub

       Sub Bumper3_Timer()
           Select Case bump3
               Case 1:BR3.z = 300:
               Case 2:BR3.z = 285:
               Case 3:BR3.z = 270:
               Case 4:BR3.z = 260:
               Case 5:BR3.z = 270:
               Case 6:BR3.z = 285:
               Case 7:BR3.z = 300:
               Case 8:BR3.z = 310:Me.TimerEnabled = 0
           End Select
		Bump3 = bump3 + 1
       End Sub

       Sub Bumper4_Timer()
           Select Case bump4
               Case 1:BR4.z = 300:
               Case 2:BR4.z = 285:
               Case 3:BR4.z = 270:
               Case 4:BR4.z = 260:
               Case 5:BR4.z = 270:
               Case 6:BR4.z = 285:
               Case 7:BR4.z = 300:
               Case 8:BR4.z = 310:Me.TimerEnabled = 0
           End Select
		Bump4 = bump4 + 1
       End Sub

       Sub Bumper5_Timer()
           Select Case bump5
               Case 1:BR5.z = 170:
               Case 2:BR5.z = 150:
               Case 3:BR5.z = 140:
               Case 4:BR5.z = 140:
               Case 5:BR5.z = 150:
               Case 6:BR5.z = 170:
               Case 7:BR5.z = 180:Me.TimerEnabled = 0
           End Select
		Bump5 = bump5 + 1
       End Sub

       Sub Bumper6_Timer()
           Select Case bump6
               Case 1:BR6.z = 120:
               Case 2:BR6.z = 110:
               Case 3:BR6.z = 100:
               Case 4:BR6.z = 100:
               Case 5:BR6.z = 110:
               Case 6:BR6.z = 120:
               Case 7:BR6.z = 130:Me.TimerEnabled = 0
           End Select
		Bump6 = bump6 + 1
       End Sub


'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
'				Slings
'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Sub Sling3_Hit:vpmTimer.PulseSw(34) End Sub

Sub Sling4_Hit:vpmTimer.PulseSw(34):Sling4Step = 0:Rubber9.Visible = False:LeafSwitch1b.ObjRotX = -3:Rubber9a.Visible = True:Me.TimerEnabled = true:End Sub
Sub Sling4b_Hit:Sling4bStep = 0:Rubber9.Visible = False:Rubber9b.Visible = True:Me.TimerEnabled = true:End Sub
Sub Sling4c_Hit:Sling4cStep = 0:Rubber9.Visible = False:Rubber9c.Visible = True:Me.TimerEnabled = true:End Sub

Sub Sling5_Hit:vpmTimer.PulseSw(34):Sling5Step = 0:Rubber19.Visible = False:LeafSwitch2b.ObjRotX = -3:Rubber19a.Visible = True:Me.TimerEnabled = true:End Sub
Sub Sling5b_Hit:Sling5bStep = 0:Rubber19.Visible = False:Rubber19b.Visible = True:Me.TimerEnabled = true:End Sub
Sub Sling5c_Hit:Sling5cStep = 0:Rubber19.Visible = False:Rubber19c.Visible = True:Me.TimerEnabled = true:End Sub


Sub Sling6_Hit:vpmTimer.PulseSw(34):Sling6Step = 0:Rubber10.Visible = False:LeafSwitch3b.ObjRotX = -3:Rubber10a.Visible = True:Me.TimerEnabled = true: End Sub

Sub Sling9_Hit:vpmTimer.PulseSw(72) End Sub

Sub Rubber3_hit:Rubber3Step = 0:Rubber3a.Visible = False:Rubber3b.Visible = True:Me.TimerEnabled = true:End Sub

Dim Sling3Step, Sling1Step, Sling2Step, Sling4Step, Sling4bStep, Sling4cStep, Sling5Step, Sling5bStep, Sling5cStep, Sling6Step, Sling7Step, Sling7DIR, Sling7Step2, Sling8Step, Sling9Step, Rubber3Step

Sub Sling1_slingshot:vpmTimer.PulseSw(34):PlaySound SoundFX("slingshot2",DOFContactors):DOF 107, DOFPulse:Sling1a.visible = false:pSling1.TransZ = -8:Sling1b.visible = true:Sling1Step = 0:Me.TimerEnabled = 1:End Sub

Sub Sling1_Timer
    Select Case Sling1Step
        Case 0:Sling1b.visible = false:pSling1.TransZ = -16:Sling1c.visible = true
        Case 1:Sling1c.visible = false:pSling1.TransZ = -24:Sling1d.visible = true
        Case 2:Sling1d.visible = false:pSling1.TransZ = -16:Sling1c.visible = true
        Case 3:Sling1c.visible = false:pSling1.TransZ = -8:Sling1b.visible = true
        Case 4:Sling1b.visible = false:pSling1.TransZ = 0:Sling1a.visible = true:Me.TimerEnabled = 0
    End Select

    Sling1Step = Sling1Step + 1
End Sub


Sub Sling2_slingshot:vpmTimer.PulseSw(34):PlaySound SoundFX("slingshot1",DOFContactors):DOF 108, DOFPulse:Sling2a.visible = false:pSling2.TransZ = -8:Sling2b.visible = true:Sling2Step = 0:Me.TimerEnabled = 1:End Sub

Sub Sling2_Timer
    Select Case Sling2Step
        Case 0:Sling2b.visible = false:pSling2.TransZ = -16:Sling2c.visible = true:
        Case 1:Sling2c.visible = false:pSling2.TransZ = -24:Sling2d.visible = true:
        Case 2:Sling2d.visible = false:pSling2.TransZ = -16:Sling2c.visible = true:
        Case 3:Sling2c.visible = false:pSling2.TransZ = -8:Sling2b.visible = true:
        Case 4:Sling2b.visible = false:pSling2.TransZ = 0:Sling2a.visible = true:Me.TimerEnabled = 0
    End Select

    Sling2Step = Sling2Step + 1
End Sub

Sub Sling4_Timer
    Select Case Sling4Step
        Case 0:Rubber9a.visible = false:LeafSwitch1b.ObjRotX = 0:Rubber9.visible = true:me.TimerEnabled = False:
End Select

    Sling4Step = Sling4Step + 1
End Sub

Sub Sling4b_Timer
    Select Case Sling4Step
        Case 0:Rubber9b.visible = false:Rubber9.visible = true:me.TimerEnabled = False:
End Select

    Sling4bStep = Sling4bStep + 1
End Sub

Sub Sling4c_Timer
    Select Case Sling4cStep
        Case 0:Rubber9c.visible = false:Rubber9.visible = true:me.TimerEnabled = False:
End Select

    Sling4cStep = Sling4cStep + 1
End Sub

Sub Sling5_Timer
    Select Case Sling5Step
        Case 0:Rubber19a.visible = false:LeafSwitch1b.ObjRotX = 0:Rubber19.visible = true:me.TimerEnabled = False:
End Select

    Sling5Step = Sling5Step + 1
End Sub

Sub Sling5b_Timer
    Select Case Sling5Step
        Case 0:Rubber19b.visible = false:Rubber19.visible = true:me.TimerEnabled = False:
End Select

    Sling5bStep = Sling5bStep + 1
End Sub

Sub Sling5c_Timer
    Select Case Sling5Step
        Case 0:Rubber19c.visible = false:Rubber19.visible = true:me.TimerEnabled = False:
End Select

    Sling5cStep = Sling5cStep + 1
End Sub

Sub Sling6_Timer
    Select Case Sling6Step
        Case 0:Rubber10a.visible = false:LeafSwitch3b.ObjRotX = 0:Rubber10.visible = true:me.TimerEnabled = False:
End Select

    Sling6Step = Sling6Step + 1
End Sub


Sub Sling7_slingshot:vpmTimer.PulseSw(72):TSling7.timerEnabled = 0:Sling7Step2 = 0:Me.TimerEnabled = 1:End Sub 

Sub Sling7_Timer
    Select Case Sling7Step
        Case 0:pSling7d.ObjRotX = 2:pSling7d.TransX = -2
        Case 1:pSling7.ObjRotX = 8:pSling7.TransX = -8:pSling7b.ObjRotX = 8:pSling7b.TransX = -8:pSling7d.ObjRotX = 4:pSling7d.TransX = -4
        Case 2:pSling7.ObjRotX = 6:pSling7.TransX = -6:pSling7b.ObjRotX = 6:pSling7b.TransX = -6:pSling7d.ObjRotX = 6:pSling7d.TransX = -6
        Case 3:pSling7.ObjRotX = 4:pSling7.TransX = -4:pSling7b.ObjRotX = 4:pSling7b.TransX = -4:pSling7d.ObjRotX = 8:pSling7d.TransX = -8
		Case 4:pSling7.ObjRotX = 2:pSling7.TransX = -2:pSling7b.ObjRotX = 2:pSling7b.TransX = -2:pSling7d.ObjRotX = 4:pSling7d.TransX = -4
        Case 5:pSling7.ObjRotX = 0:pSling7.TransX = 0:pSling7b.ObjRotX = 0:pSling7b.TransX = 0:pSling7d.ObjRotX = 0:pSling7d.TransX = 0:Me.TimerEnabled = 0:Sling7Step = 0
    End Select

    Sling7Step = Sling7Step + 1
End Sub

Sub TSling7_hit()
	Sling7DIR = 1
	Me.TimerEnabled = 1
End Sub
'
Sub TSling7_unhit()
	Sling7DIR = -1
End Sub

Sub TSling7_timer()
    Select Case Sling7Step2
        Case 0:pSling7.ObjRotX = 0:pSling7.TransX = 0:pSling7b.ObjRotX = 0:pSling7b.TransX = 0:Me.TimerEnabled = 0:
        Case 1:pSling7.ObjRotX = 2:pSling7.TransX = -2:pSling7b.ObjRotX = 2:pSling7b.TransX = -2
        Case 2:pSling7.ObjRotX = 4:pSling7.TransX = -4:pSling7b.ObjRotX = 4:pSling7b.TransX = -4
        Case 3:pSling7.ObjRotX = 6:pSling7.TransX = -6:pSling7b.ObjRotX = 6:pSling7b.TransX = -6
		Case 4:pSling7.ObjRotX = 8:pSling7.TransX = -8:pSling7b.ObjRotX = 8:pSling7b.TransX = -8
        Case 5:pSling7.ObjRotX = 10:pSling7.TransX = -10:pSling7b.ObjRotX = 10:pSling7b.TransX = -10
    End Select

	If Sling7DIR = 1 then
    Sling7Step2 = Sling7Step2 + 1
	Else
    Sling7Step2 = Sling7Step2 - 1
	End If
	If Sling7Step2 = 6 then Sling7Step2 = 5 End If
End Sub

Sub Sling8_slingshot:vpmTimer.PulseSw(72):PlaySound SoundFX("slingshot2",DOFContactors):DOF 111, DOFPulse:Sling8a.visible = false:pSling8.TransZ = -8:Sling8b.visible = true:Sling8Step = 0:Me.TimerEnabled = 1:End Sub

Sub Sling8_Timer
    Select Case Sling8Step
        Case 0:Sling8b.visible = false:pSling8.TransZ = -16:Sling8c.visible = true:
        Case 1:Sling8c.visible = false:pSling8.TransZ = -24:Sling8d.visible = true:
        Case 2:Sling8d.visible = false:pSling8.TransZ = -16:Sling8c.visible = true:
        Case 3:Sling8c.visible = false:pSling8.TransZ = -8:Sling8b.visible = true:
        Case 4:Sling8b.visible = false:pSling8.TransZ = 0:Sling8a.visible = true:Me.TimerEnabled = 0
    End Select

    Sling8Step = Sling8Step + 1
End Sub


Dim wLeftDrainBStep, wLeftDrainCStep

Sub wLeftDrainB_hit()
	LeftDrainRubber3a.visible = false:LeftDrainRubber3b.visible = true:me.timerEnabled = true
End Sub

Sub wLeftDrainB_Timer
    Select Case wLeftDrainBStep
        Case 0:
        Case 1:
        Case 2:LeftDrainRubber3a.visible = true:LeftDrainRubber3b.visible = false:Me.TimerEnabled = 0:wLeftDrainBStep = 0
    End Select

    wLeftDrainBStep = wLeftDrainBStep + 1
End Sub

Sub wLeftDrainc_hit()
	LeftDrainRubber3a.visible = false:LeftDrainRubber3c.visible = true:me.timerEnabled = true
End Sub

Sub wLeftDrainc_Timer
    Select Case wLeftDraincStep
        Case 0:
        Case 1:
        Case 2:LeftDrainRubber3a.visible = true:LeftDrainRubber3c.visible = false:Me.TimerEnabled = 0:wLeftDrainCStep = 0
    End Select

    wLeftDraincStep = wLeftDraincStep + 1
End Sub

Sub Rubber3_Timer
    Select Case Rubber3Step
        Case 0:Rubber3b.visible = false:Rubber3a.visible = true:me.TimerEnabled = False:
	End Select

	Rubber3Step = Rubber3Step + 1
End Sub

'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
'			LL		EEEEEE	DDDD		,,	 SSSSS
'      		LL		EE		DD  DD		,,	SS
'			LL		EE		DD   DD		 ,	 SS
'			LL		EEEE	DD   DD			   SS
'			LL		EE		DD  DD			    SS
'			LLLLLL  EEEEEE	DDDD			SSSSS
'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
'		6 Didget Array
'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Dim LED(33)
LED(0)=Array()
LED(1)=Array()
LED(2)=Array()
LED(3)=Array()
LED(4)=Array()
LED(5)=Array()
LED(6)=Array()
LED(7)=Array()
LED(8)=Array()
LED(9)=Array()
LED(10)=Array()
LED(11)=Array()
LED(12)=Array()
LED(13)=Array()
LED(14)=Array()
LED(15)=Array()
LED(16)=Array()
LED(17)=Array()
LED(18)=Array()
LED(19)=Array()
LED(20)=Array()
LED(21)=Array()
LED(22)=Array()
LED(23)=Array()


LED(26)=Array(d241,d242,d243,d244,d245,d246,d247,LXM,d248)		
LED(27)=Array(d251,d252,d253,d254,d255,d256,d257,LXM,d258)		
LED(24)=Array(d261,d262,d263,d264,d265,d266,d267,LXM,d268)		
LED(25)=Array(d271,d272,d273,d274,d275,d276,d277,LXM,d278)		


LED(28)=Array(D281,D282,D283,D284,D285,D286,D287,LXM,D288)		
LED(29)=Array(D291,D292,D293,D294,D295,D296,D297,LXM,D298)		
LED(30)=Array(D301,D302,D303,D304,D305,D306,D307,LXM,D308)		
LED(31)=Array(D311,D312,D313,D314,D315,D316,D317,LXM,D318)		
LED(32)=Array(D321,D322,D323,D324,D325,D326,D327,LXM,D328)		
LED(33)=Array(D331,D332,D333,D334,D335,D336,D337,LXM,D338)		



Sub DisplayTimer6_Timer
Dim ChgLED, ii, num, chg, stat, obj
ChgLED = Controller.ChangedLEDs (&Hffffffff, &Hffffffff)
If Not IsEmpty (ChgLED) Then
For ii = 0 To UBound (chgLED)
num = chgLED (ii, 0) : chg = chgLED (ii, 1) : stat = chgLED (ii, 2)
For Each obj In LED (num)
If chg And 1 Then obj.State = stat And 1
chg = chg \ 2 : stat = stat \ 2
Next
Next
End If
End Sub


'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
'		7 Didget Array
'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Dim LED7(37)
LED7(0)=Array()
LED7(1)=Array()
LED7(2)=Array()
LED7(3)=Array()
LED7(4)=Array()
LED7(5)=Array()
LED7(6)=Array()
LED7(7)=Array()
LED7(8)=Array()
LED7(9)=Array()
LED7(10)=Array()
LED7(11)=Array()
LED7(12)=Array()
LED7(13)=Array()
LED7(14)=Array()
LED7(15)=Array()
LED7(16)=Array()
LED7(17)=Array()
LED7(18)=Array()
LED7(19)=Array()
LED7(20)=Array()
LED7(21)=Array()
LED7(22)=Array()
LED7(23)=Array()
LED7(24)=Array()
LED7(25)=Array()
LED7(26)=Array()
LED7(27)=Array()

LED7(30)=Array(d241,d242,d243,d244,d245,d246,d247,LXM,d248)		
LED7(31)=Array(d251,d252,d253,d254,d255,d256,d257,LXM,d258)		
LED7(28)=Array(d261,d262,d263,d264,d265,d266,d267,LXM,d268)		
LED7(29)=Array(d271,d272,d273,d274,d275,d276,d277,LXM,d278)		


LED7(32)=Array(D281,D282,D283,D284,D285,D286,D287,LXM,D288)		
LED7(33)=Array(D291,D292,D293,D294,D295,D296,D297,LXM,D298)		
LED7(34)=Array(D301,D302,D303,D304,D305,D306,D307,LXM,D308)		
LED7(35)=Array(D311,D312,D313,D314,D315,D316,D317,LXM,D318)		
LED7(36)=Array(D321,D322,D323,D324,D325,D326,D327,LXM,D328)		
LED7(37)=Array(D331,D332,D333,D334,D335,D336,D337,LXM,D338)		


Sub DisplayTimer7_Timer
	Dim ChgLED, ii, num, chg, stat, obj
	ChgLED = Controller.ChangedLEDs (&Hffffffff, &Hffffffff)
	If Not IsEmpty (ChgLED) Then
		For ii = 0 To UBound (chgLED)
			num = chgLED (ii, 0) : chg = chgLED (ii, 1) : stat = chgLED (ii, 2)
			For Each obj In LED7 (num)
				If chg And 1 Then obj.State = stat And 1
				chg = chg \ 2 : stat = stat \ 2
			Next
		Next
	End If
End Sub

							
'***************************************************
'  JP's Fading Lamps & Flashers version 9 for VP921
'   Based on PD's Fading Lights
' SetLamp 0 is Off
' SetLamp 1 is On
' FadingLevel(x) = fading state
' LampState(x) = light state
' Includes the flash element (needs own timer)
' Flashers can be used as lights too
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashState(200), FlashLevel(200)
Dim FlashSpeedUp, FlashSpeedDown
Dim FadeArray1: FadeArray1 = Array("BlackHole_on", "BlackHole_66", "BlackHole_33", "BlackHole_off")
Dim YellowLight: YellowLight = Array("yellow_on", "yellow_66", "yellow_33", "yellow_off")
Dim OrangeLight: orangeLight = Array("orange_on", "orange_66", "orange_33", "orange_off")
Dim RedLight: RedLight = Array("Red_on", "Red_66", "Red_33", "Red_off")
Dim BlueLightL: BlueLightL = Array("bluearrowL_on", "bluearrowL_66", "bluearrowL_33", "bluearrowL_off")
Dim BlueLightR: BlueLightR = Array("bluearrowR_on", "bluearrowR_66", "bluearrowR_33", "bluearrowR_off")
Dim BArray: BArray = Array("BH_Bumpercap_on", "BH_Bumpercap_66", "BH_Bumpercap_33", "BH_Bumpercap_off")
Dim BArrayDay: BArrayDay = Array("BH_Bumpercap_Day_on", "BH_Bumpercap_Day_66", "BH_Bumpercap_Day_33", "BH_Bumpercap_Day_off")
Dim ReEnetryTubeArray: ReEnetryTubeArray = Array("BH_ReEnetryTube_on", "BH_ReEnetryTube_66", "BH_ReEnetryTube_33", "BH_ReEnetryTube_off")

Const LightHaloBrightness		= 100


FlashInit()
FlasherTimer.Interval = 10 'flash fading speed
FlasherTimer.Enabled = 1

' Lamp & Flasher Timers

Sub StartLampTimer
	AllLampsOff()
	LampTimer.Interval = 30 'lamp fading speed
	LampTimer.Enabled = 1
End Sub

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
			FlashState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
			CheckSolenoid chgLamp(ii, 0),chgLamp(ii, 1)
        Next
    End If	
    UpdateLamps
End Sub
 
 Sub UpdateLamps

	NFadeLm 2, BLight1b
	NFadeLm 2, BLight2b
	NFadeLm 2, BLight3b
	NFadeLm 2, BLight4b
	FadePri4m 2, PBumpCap1, BArrayDay
	FadePri4m 2, PBumpCap2, BArrayDay
	FadePri4m 2, PBumpCap3, BArrayDay
	FadePri4 2, PBumpCap4, BArrayDay
	FadePri4m 17, PBumpCap5, BArrayDay
	FadePri4 17, PBumpCap6, BArrayDay

If TubeGlow = 1 then
	FadeDisableLighting 123, ReEnetryTube
	FadePri4 123, ReEnetryTube, ReEnetryTubeArray
End If

'Upper playfield lights

	NFadeL 3, L3
	NFadeL 7, L7
	NFadeL 21, L21
	NFadeL 22, L22
	NFadeL 23, L23
	NFadeL 24, L24
	NFadeL 25, L25
	NFadeL 26, L26
	NFadeL 27, L27
	NFadeL 28, L28 
	NFadeL 29, L29
	NFadeL 30, L30
	NFadeL 31, L31
	NFadeL 32, L32
	NFadeL 33, L33
	NFadeL 34, L34
	NFadeL 35, L35
	NFadeL 36, L36
	If CaptiveLight = 0 then
	Else 
	NFadeLm 37, LCaptiveLight
	End If
	NFadeL 37, L37
	NFadeL 38, L38
	NFadeL 39, L39c
	NFadeL 40, L40
	NFadeL 41, L41
	NFadeL 42, L42
	NFadeL 43, L43
'
	NFadeL 48, L48
	NFadeL 49, L49
	NFadeL 50, L50
	NFadeL 51, L51
	NFadeL 138, L39a
	NFadeL 139, L39b





' Lower playfield lights

	FadeFlash 4, L4, RedLight
	FadeFlash 5, L5, OrangeLight
	FadeFlash 6, L6, OrangeLight
	FadeFlashm 19, L19c, BlueLightL	
	FadeFlashm 19, L19b, BlueLightL
	FadeFlash 19, L19a, BlueLightL
	FadeFlashm 20, L20c, BlueLightR
	FadeFlashm 20, L20b, BlueLightR
	FadeFlash 20, L20a, BlueLightR
	FadeFlash 47, L47, YellowLight
	FadeFlash 46, L46, YellowLight
	FadeFlash 45, L45, YellowLight
	FadeFlash 44, L44, YellowLight




 End Sub
 
'Sindbad: You can use this instead of FadeLN
' call it this way: FadeLight lampnumber, light, Array
Sub FadeLight(nr, light, group)
	Select Case FadingLevel(nr)
		Case 2:light.State = LightStateOff:light.OffImage = group(3):FadingLevel(nr) = 0
		Case 3:light.State = LightStateOn:light.OnImage = group(2):FadingLevel(nr) = 2
		Case 4:light.State = LightStateOn:light.OnImage = group(1):FadingLevel(nr) = 3
		Case 5:light.State = LightStateOn:light.OnImage = group(0):FadingLevel(nr) = 1
	End Select
End Sub


Sub NFadeL(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0:FadingLevel(nr) = 0
        Case 5:a.State = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0
        Case 5:a.State = 1
    End Select
End Sub

Sub FadeDisableLighting(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.DisableLighting = 0
        Case 5:a.DisableLighting = 1
    End Select
End Sub

'cyberpez FadeFlash can be used to swap images on ramps or flashers


Sub FadeFlash(nr, light, group)
	Select Case FadingLevel(nr)
		Case 2:light.imageA = group(3):FadingLevel(nr) = 0
		Case 3:light.imageA = group(2):FadingLevel(nr) = 2
		Case 4:light.imageA = group(1):FadingLevel(nr) = 3
		Case 5:light.imageA = group(0):FadingLevel(nr) = 1
	End Select
End Sub

Sub FadeFlashm(nr, light, group)
	Select Case FadingLevel(nr)
		Case 2:light.imageA = group(3):
		Case 3:light.imageA = group(2):
		Case 4:light.imageA = group(1):
		Case 5:light.imageA = group(0):
	End Select
End Sub

LightHalos_Init
Sub LightHalos_Init
	If LightHalo_ON = 0 Then 'hide halos
		For each xx in aLightHalos:xx.IsVisible = False:Next
		For each xx in aLightHalos:xx.Alpha = 0:Next
	End If

End Sub


Sub FlasherTimer_Timer()


'***GI
	Flashm 17, lgi1
	Flashm 17, lgi2
	Flashm 17, lgi3
	Flashm 17, lgi4

	Flashm 17, Flasher20
	Flashm 17, Flasher21
	Flashm 17, Flasher22
	Flashm 17, Flasher23
	Flashm 17, Flasher24
	Flashm 17, Flasher25
	Flashm 17, Flasher26

	Flash 17, Flasher27

'
	'Halos
	If LightHalo_ON = 1 Then
'
		FlashVal 4, h4, LightHaloBrightness
		FlashVal 5, h5, LightHaloBrightness
		FlashVal 6, h6, LightHaloBrightness
		FlashValm 19, H19c, LightHaloBrightness
		FlashValm 19, H19b, LightHaloBrightness
		FlashVal 19, H19a, LightHaloBrightness
		FlashValm 20, h20c, LightHaloBrightness
		FlashValm 20, h20b, LightHaloBrightness
		FlashVal 20, h20a, LightHaloBrightness

		If CaptiveLight = 0 then
		Else 
		FlashVal 37, CaptiveLightHalo, LightHaloBrightness 
		End If

		FlashVal 44, h44, LightHaloBrightness
		FlashVal 45, h45, LightHaloBrightness
		FlashVal 46, h46, LightHaloBrightness
		FlashVal 47, h47, LightHaloBrightness
	End If



End Sub



' div lamp subs

Sub AllLampsOff()
    Dim x
    For x = 0 to 200
        LampState(x) = 0
        FadingLevel(x) = 4
    Next

UpdateLamps:UpdateLamps:Updatelamps
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub



Sub SetUpperGI(gistate)
	gi1.state = gistate
	gi2.state = gistate
	gi3.state = gistate
	gi4.state = gistate
	gi5.state = gistate
	gi6.state = gistate
	gi7.state = gistate
	gi8.state = gistate
	gi9.state = gistate
	gi10.state = gistate
	gi11.state = gistate
	gi12.state = gistate
	gi13.state = gistate
	gi14.state = gistate
	gi15.state = gistate
	gi16.state = gistate
	gi17.state = gistate
	gi18.state = gistate
	gi19.state = gistate	
	gi20.state = gistate
	gi21.state = gistate
End Sub

Sub SetLowerGIOn
DOF 110, DOFOff
If NightMod = 1 then
	LowerPlayfield2.imageA = "BlackHole_LPF_night_on"
Else
	LowerPlayfield2.imageA = "BlackHole_LPF_day_on"
End If
	pDT_LL70.image = "droptarget_Yellow"
	pDT_LL60.image = "droptarget_Yellow"
	pDT_LL50.image = "droptarget_Yellow"
	pDT_LL40.image = "droptarget_Yellow"
	pDT_LL61.image = "droptarget_White"
	pDT_LL51.image = "droptarget_White"
	pDT_LL41.image = "droptarget_White"
	LPFoverhead.state = 1
End Sub


Sub SetLowerGIOff
DOF 110, DOFOn
If NightMod = 1 then
	LowerPlayfield2.imageA = "BlackHole_LPF_night_off"
Else
	LowerPlayfield2.imageA = "BlackHole_LPF_day_off"
End If
	pDT_LL70.image = "droptarget_Yellow_off"
	pDT_LL60.image = "droptarget_Yellow_off"
	pDT_LL50.image = "droptarget_Yellow_off"
	pDT_LL40.image = "droptarget_Yellow_off"
	pDT_LL61.image = "droptarget_White_off"
	pDT_LL51.image = "droptarget_White_off"
	pDT_LL41.image = "droptarget_White_off"
	LPFoverhead.state = 0
End Sub


' div flasher subs

Sub FlashInit
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
        FlashLevel(i) = 0
    Next

    FlashSpeedUp = 30   ' fast speed when turning on the flasher
    FlashSpeedDown = 10 ' slow speed when turning off the flasher, gives a smooth fading
    ' you could also change the default images for each flasher or leave it as in the editor
    ' for example
    ' Flasher1.Image = "fr"
    AllFlashOff()
End Sub

Sub AllFlashOff
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
    Next
End Sub

Sub SetFlash(nr, stat)
    FlashState(nr) = ABS(stat)
End Sub


' Flasher objects
' Uses own faster timer

Sub Flash(nr, object)
    Select Case FlashState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FlashState(nr) = -1 'completely off
            End if
            Object.opacity = FlashLevel(nr)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
            If FlashLevel(nr) > 250 Then
                FlashLevel(nr) = 250
                FlashState(nr) = -2 'completely on
            End if
            Object.opacity = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the flashstate
    Select Case FlashState(nr)
        Case 0         'off
            Object.opacity = FlashLevel(nr)
        Case 1         ' on
            Object.opacity = FlashLevel(nr)
    End Select
End Sub

Sub FlashVal(nr, object, value)
    Select Case FlashState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FlashState(nr) = -1 'completely off
            End if
            Object.opacity = FlashLevel(nr)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
            If FlashLevel(nr) > value Then
                FlashLevel(nr) = value
                FlashState(nr) = -2 'completely on
            End if
            Object.opacity = FlashLevel(nr)
    End Select
End Sub

Sub FlashValm(nr, object, value) 'multiple flashers, it doesn't change the flashstate
    Select Case FlashState(nr)
        Case 0 'off
            Object.opacity = FlashLevel(nr)
        Case 1 ' on
            Object.opacity = FlashLevel(nr)
    End Select
End Sub



Sub FadeLn(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.Offimage = d:FadingLevel(nr) = 0 'Off
        Case 3:Light.Offimage = c:FadingLevel(nr) = 2 'fading...
        Case 4:Light.Offimage = b:FadingLevel(nr) = 3 'fading...
        Case 5:Light.Offimage = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadeLnm(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.Offimage = d
        Case 3:Light.Offimage = c
        Case 4:Light.Offimage = b
        Case 5:Light.Offimage = a
    End Select
End Sub

Sub LMapn(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.Onimage = d:FadingLevel(nr) = 0 'Off
        Case 3:Light.ONimage = c:FadingLevel(nr) = 2 'fading...
        Case 4:Light.ONimage = b:FadingLevel(nr) = 3 'fading...
        Case 5:Light.ONimage = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub LMapnm(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.ONimage = d
        Case 3:Light.ONimage = c
        Case 4:Light.ONimage = b
        Case 5:Light.ONimage = a
    End Select
End Sub
' Walls

Sub FadeW(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:c.IsDropped = 1:FadingLevel(nr) = 0                 'Off
        Case 3:b.IsDropped = 1:c.IsDropped = 0:FadingLevel(nr) = 2 'fading...
        Case 4:a.IsDropped = 1:b.IsDropped = 0:FadingLevel(nr) = 3 'fading...
        Case 5:a.IsDropped = 0:FadingLevel(nr) = 1                 'ON
    End Select
End Sub

Sub FadeWm(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:c.IsDropped = 1
        Case 3:b.IsDropped = 1:c.IsDropped = 0
        Case 4:a.IsDropped = 1:b.IsDropped = 0
        Case 5:a.IsDropped = 0
    End Select
End Sub

Sub NFadeW(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 1:FadingLevel(nr) = 0
        Case 5:a.IsDropped = 0:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeWm(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 1
        Case 5:a.IsDropped = 0
    End Select
End Sub

Sub NFadeWi(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 0:FadingLevel(nr) = 0
        Case 5:a.IsDropped = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeWim(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 0
        Case 5:a.IsDropped = 1
    End Select
End Sub

'Lights

Sub FadeL(nr, a, b)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0:FadingLevel(nr) = 0
        Case 3:b.state = 1:FadingLevel(nr) = 2
        Case 4:a.state = 0:FadingLevel(nr) = 3
        Case 5:a.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub FadeLm(nr, a, b)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0
        Case 3:b.state = 1
        Case 4:a.state = 0
        Case 5:a.state = 1
    End Select
End Sub

Sub NFadeL(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0:FadingLevel(nr) = 0
        Case 5:a.State = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0
        Case 5:a.State = 1
    End Select
End Sub

Sub LMap(nr, a, b, c) 'can be used with normal/olod style lights too
    Select Case FadingLevel(nr)
        Case 2:c.state = 0:FadingLevel(nr) = 0
        Case 3:b.state = 0:c.state = 1:FadingLevel(nr) = 2
        Case 4:a.state = 0:b.state = 1:FadingLevel(nr) = 3
        Case 5:b.state = 0:c.state = 0:a.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub LMapm(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:c.state = 0
        Case 3:b.state = 0:c.state = 1
        Case 4:a.state = 0:b.state = 1
        Case 5:b.state = 0:c.state = 0:a.state = 1
    End Select
End Sub

'Reels

Sub FadeR(nr, a)
    Select Case FadingLevel(nr)
        Case 2:a.SetValue 3:FadingLevel(nr) = 0
        Case 3:a.SetValue 2:FadingLevel(nr) = 2
        Case 4:a.SetValue 1:FadingLevel(nr) = 3
        Case 5:a.SetValue 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub FadeRm(nr, a)
    Select Case FadingLevel(nr)
        Case 2:a.SetValue 3
        Case 3:a.SetValue 2
        Case 4:a.SetValue 1
        Case 5:a.SetValue 1
    End Select
End Sub

'Texts

Sub NFadeT(nr, a, b)
    Select Case FadingLevel(nr)
        Case 4:a.Text = "":FadingLevel(nr) = 0
        Case 5:a.Text = b:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, a, b)
    Select Case FadingLevel(nr)
        Case 4:a.Text = ""
        Case 5:a.Text = b
    End Select
End Sub

' Flash a light, not controlled by the rom

Sub FlashL(nr, a, b)
    Select Case FadingLevel(nr)
        Case 1:b.state = 0:FadingLevel(nr) = 0
        Case 2:b.state = 1:FadingLevel(nr) = 1
        Case 3:a.state = 0:FadingLevel(nr) = 2
        Case 4:a.state = 1:FadingLevel(nr) = 3
        Case 5:b.state = 1:FadingLevel(nr) = 4
    End Select
End Sub

' Light acting as a flash. C is the light number to be restored

Sub MFadeL(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0:FadingLevel(nr) = 0:SetLamp c, FadingLevel(c)
        Case 3:b.state = 1:FadingLevel(nr) = 2
        Case 4:a.state = 0:FadingLevel(nr) = 3
        Case 5:a.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub MFadeLm(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0:SetLamp c, FadingLevel(c)
        Case 3:b.state = 1
        Case 4:a.state = 0
        Case 5:a.state = 1
    End Select
End Sub

'Alpha Ramps used as fading lights
'ramp is the name of the ramp
'a,b,c,d are the images used for on...off
'r is the refresh light

Sub FadeAR(nr, ramp, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:ramp.image = d:FadingLevel(nr) = 0 'Off
        Case 3:ramp.image = c:FadingLevel(nr) = 2 'fading...
        Case 4:ramp.image = b:FadingLevel(nr) = 3 'fading...
        Case 5:ramp.image = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadeARm(nr, ramp, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:ramp.image = d
        Case 3:ramp.image = c
        Case 4:ramp.image = b
        Case 5:ramp.image = a
    End Select
End Sub

Sub FlashFO(nr, ramp, a, b, c)                                   'used for reflections when there is no off ramp
    Select Case FadingLevel(nr)
        Case 2:ramp.IsVisible = 0:FadingLevel(nr) = 0                'Off
        Case 3:ramp.image = c:FadingLevel(nr) = 2                'fading...
        Case 4:ramp.image = b:FadingLevel(nr) = 3                'fading...
        Case 5:ramp.image = a:ramp.IsVisible = 1:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FlashAR(nr, ramp, a, b, c)                                   'used for reflections when there is no off ramp
    Select Case FadingLevel(nr)
        Case 2:ramp.alpha = 0:FadingLevel(nr) = 0                'Off
        Case 3:ramp.image = c:FadingLevel(nr) = 2                'fading...
        Case 4:ramp.image = b:FadingLevel(nr) = 3                'fading...
        Case 5:ramp.image = a:ramp.alpha = 1:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FlashARm(nr, ramp, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:ramp.alpha = 0
        Case 3:ramp.image = c
        Case 4:ramp.image = b
        Case 5:ramp.image = a:ramp.alpha = 1
    End Select
End Sub

Sub NFadeAR(nr, ramp, a, b)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b:FadingLevel(nr) = 0 'off
        Case 5:ramp.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeARm(nr, ramp, a, b)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b
        Case 5:ramp.image = a
    End Select
End Sub

Sub MNFadeAR(nr, ramp, a, b, c)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b:FadingLevel(nr) = 0:SetLamp c, FadingLevel(c) 'off
        Case 5:ramp.image = a:FadingLevel(nr) = 1                           'on
    End Select
End Sub

Sub MNFadeARm(nr, ramp, a, b, c)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b:SetLamp c, FadingLevel(c) 'off
        Case 5:ramp.image = a                           'on
    End Select
End Sub

' Flashers using PRIMITIVES
' pri is the name of the primitive
' a,b,c,d are the images used for on...off

Sub FadePri(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:FadingLevel(nr) = 0 'Off
        Case 3:pri.image = c:FadingLevel(nr) = 2 'fading...
        Case 4:pri.image = b:FadingLevel(nr) = 3 'fading...
        Case 5:pri.image = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

'Fades primitives, 3 images

Sub FadePri3m(nr, pri, group)
    Select Case FadingLevel(nr)
        Case 3:pri.image = group(2)
        Case 4:pri.image = group(1)
        Case 5:pri.image = group(0)
    End Select
End Sub

Sub FadePri3(nr, pri, group)
    Select Case FadingLevel(nr)
        Case 3:pri.image = group(2):FadingLevel(nr) = 0 'Off
        Case 4:pri.image = group(1):FadingLevel(nr) = 3 'fading...
        Case 5:pri.image = group(0):FadingLevel(nr) = 1 'ON
    End Select
End Sub


'Fades primitives, 4 images

Sub FadePri4m(nr, pri, group)
    Select Case FadingLevel(nr)
		Case 2:pri.image = group(3)
        Case 3:pri.image = group(2)
        Case 4:pri.image = group(1)
        Case 5:pri.image = group(0)
    End Select
End Sub


Sub FadePri4(nr, pri, group)
    Select Case FadingLevel(nr)
		Case 2:pri.image = group(3):FadingLevel(nr) = 0 'Off
        Case 3:pri.image = group(2):FadingLevel(nr) = 2 'fading...
        Case 4:pri.image = group(1):FadingLevel(nr) = 3 'fading...
        Case 5:pri.image = group(0):FadingLevel(nr) = 1 'ON
    End Select
End Sub


Sub FadePriC(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:For each xx in pri:xx.image = d:Next:FadingLevel(nr) = 0 'Off
        Case 3:For each xx in pri:xx.image = c:Next:FadingLevel(nr) = 2 'fading...
        Case 4:For each xx in pri:xx.image = b:Next:FadingLevel(nr) = 3 'fading...
        Case 5:For each xx in pri:xx.image = a:Next:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadePrih(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:SetFlash nr, 0:FadingLevel(nr) = 0 'Off
        Case 3:pri.image = c:FadingLevel(nr) = 2 'fading...
        Case 4:pri.image = b:FadingLevel(nr) = 3 'fading...
        Case 5:pri.image = a:SetFlash nr, 1:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadePrim(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d
        Case 3:pri.image = c
        Case 4:pri.image = b
        Case 5:pri.image = a
    End Select
End Sub

Sub NFadePri(nr, pri, a, b)
    Select Case FadingLevel(nr)
        Case 4:pri.image = b:FadingLevel(nr) = 0 'off
        Case 5:pri.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadePrim(nr, pri, a, b)
    Select Case FadingLevel(nr)
        Case 4:pri.image = b
        Case 5:pri.image = a
    End Select
End Sub

'Fade a collection of lights

Sub FadeLCo(nr, a, b) 'fading collection of lights
    Dim obj
    Select Case FadingLevel(nr)
        Case 2:vpmSolToggleObj b, Nothing, 0, 0:FadingLevel(nr) = 0
        Case 3:vpmSolToggleObj b, Nothing, 0, 1:FadingLevel(nr) = 2
        Case 4:vpmSolToggleObj a, Nothing, 0, 0:FadingLevel(nr) = 3
        Case 5:vpmSolToggleObj a, Nothing, 0, 1:FadingLevel(nr) = 1
    End Select
End Sub


' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000) * 100
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / BlackHole.width-1
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 500 Then
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


''''''''''''''''''
' Sound Helpers
''''''''''''''''''

Sub tWallHit1_Hit()
	Playsound "metalhit_medium"
End Sub

Sub tWallHit2_Hit()
	Playsound "metalhit_medium"
End Sub

''''''''''''''''''
' Sound Collections
''''''''''''''''''

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub UpperWalls (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub LowerWalls (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rails_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub UpperRubbersBandsLargeRings_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub UpperRubbersSmallRings_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub LowerRubbers_Hit(idx)
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

Sub LeftFlipper2_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper2_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub FlipperLL_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub FlipperLR_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub Flipper1_Collide(parm)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub BlackHole_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub

'-------------------------------
'------ End of Sound code ------
'-------------------------------

'******************************************************
'      			EDIT DIPS BY INKOCHNITO
'******************************************************

Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700,400,"Black Hole - DIP switches"
		.AddFrame 2,5,190,"Maximum credits",49152,Array("8 credits",0,"10 credits",32768,"15 credits",&H00004000,"25 credits",49152)'dip 15&16
		.AddFrame 2,81,190,"Coin chute 1 and 2 control",&H00002000,Array("seperate",0,"same",&H00002000)'dip 14
		.AddFrame 2,127,190,"Playfield special",&H00200000,Array("replay",0,"extra ball",&H00200000)'dip 22
		.AddFrame 2,173,190,"3rd coin chute credits control",&H00001000,Array("no effect",0,"add 9",&H00001000)'dip 13
		.AddFrame 2,219,190,"High score to date awards",&H00C00000,Array("not displayed and no award",0,"displayed and no award",&H00800000,"displayed and 2 credits",&H00400000,"displayed and 3 credits",&H00C00000)'dip 23&24
		.AddChk 2,305,190,Array("Match feature",&H00020000)'dip 18
		.AddChkExtra 2,320,190,Array("Speech",&H0020)'SS-board dip 6
		.AddChkExtra 2,335,190,Array("Background sound",&H0010)'SS-board dip 5
		.AddFrameExtra 205,5,190,"Attract sound",&H000c,Array("off",0,"every 10 seconds",&H0004,"every 2 minutes",&H0008,"every 4 minutes",&H000C)'SS-board dip 3&4
		.AddFrame 205,81,190,"Balls per game",&H00010000,Array("5 balls",0,"3 balls",&H00010000)'dip 17
		.AddFrame 205,127,190,"Replay limit",&H00040000,Array("no limit",0,"one per game",&H00040000)'dip 19
		.AddFrame 205,173,190,"Novelty mode",&H00080000,Array("normal game mode",0,"50,000 points per special/extra ball",&H00080000)'dip 20
		.AddFrame 205,219,190,"Game mode",&H00100000,Array("replay (required for match)",0,"extra ball",&H00100000)'dip 21
		.AddChk 205,275,190,Array("Coin switch tune?",&H04000000)'dip 27
		.AddChk 205,290,190,Array("Credits displayed?",&H08000000)'dip 28
		.AddChk 205,305,190,Array("Attract features",&H20000000)'dip 30
		.AddChk 205,320,190,Array("Must be on",&H01000000)'dip 25
		.AddChk 205,335,190,Array("Must be on",&H02000000)'dip 26
		.AddLabel 50,365,300,20,"After hitting OK, press F3 to reset game with new settings."
	End With
	Dim extra
	extra = Controller.Dip(4) + Controller.Dip(5)*256
	extra = vpmDips.ViewDipsExtra(extra)
	Controller.Dip(4) = extra And 255
	Controller.Dip(5) = (extra And 65280)\256 And 255
End Sub
Set vpmShowDips = GetRef("editDips")

