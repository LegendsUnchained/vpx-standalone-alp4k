'********************************************************* Credits and notes ***************************************************************************
'** NOTE TO ME, redo all SSF sound calls, put proper DOFDevice statesments in IE DOFCONTACTORS etc
'** and find out how to properly do an SSF sound call on a bstrough or bsaucer onbject
'* 										Heavy Metal Rowamet - 1983
'* Inspired by the table made by Destruk for VP9 and the FP table by Cujopb
'* Anything terrible done below is not thier doing
'* I havent a clue what i am doing, we are driving down a cliff in an apple cart here
'*
'*    I chose to remain with the proper ruleset for the table rather than add things like multiball etc
'* I tried it out, it was too much for this table, it is a simple 1983 taito based table and score rolls over at 999,999 points
'* multiball and other stuff just added too much
'* i did steal from 2001 and 1981 in giving the machine display tech that would come later, and giving it content that would have
'* coincided with the 1981 movie release.
'*
'* there are bits and pices of code in here from probably anyone's tables i have downloaded

'* so if you see a line of code that looks familiar, i probably stole it.
'* i kept code comments where practical (or where i remembered)
'* 
'* I stole a model or two out of VPX itself, out of the default new table, they were perfect as is for what i wanted
'* 
'*
'* I redid the artwork for table and plastics using a combination of pictures from ipdb, the art from Destruk's table and Cujopb
'* as guidlines / templates, could be better but i am not terribly gifted in photoshop in that way, magic wand got a lot of work
'* it's now the non magical wand.
'* 
'* The extra non ROM Sounds are made from various things.
'* Some from the movie, some from 1980's video games, some from what ever noise i could make and shove in pro-tools and bastardize
'*
'*
'* To me, the table is old school, simple and fun, and maybe i learned a few things
'* which will all have been forgotten by tomorrow
'* Hope you have fun playing it, feel free to send feedback or suggestions
'*         
'*                             Credits
'* 
'*       Initial inspiration, and the base code of how the table should work
'*       Destruk
'*
'*       Inspiration on a more HiTech twist on the table 
'*       Cujopb
'*           
'*       For the music per ball Idea
'*       dboyrecords
'*
'*       Code copied, stolen, borrowed or mutated
'*       JPSalas, Rothbauerw, Ninuzzu, 32assassin, Arngrim, other people who did not bother to sign their tables or their scripts so i
'*       do not know your names.
'*  
'*       Objects stolen from
'*       VPX: i stole your flipper parts, the little metal kicker pieces, and i stole one of your bumper cap primitives
'*       Cause they were already exactly what i was looking for, and i dont know that i have 3d modeling skills yet.
'*
'*       Thanks to people on VPForums for answering questions and it's all your fault this table now Exists
'*       So if anyone really hates this table, go kill the forum people, they let me do it :)
'* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'* Classic Version 4.0
'* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'* Classic Version 4.0 is the stock table, with stock sounds physics and gameplay
'* No music
'* No DMD
'* No videos
'* No crazy balls or lighting
'* This is the version to use if you want close to a pure stock 1983 table
'* Normalized physics, no warp speed ball
'* This table version uses the stock DOFConfig that is ROM driven
'* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'Version 1.5.1 Notes


'* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'*********** DO NOT EDIT THIS PIECE *****************
		Option Explicit '                           *
			Randomize       '                       *
'**************** NO NO NO **************************

' *****************************************************
' ***************** USER OPTIONS HERE *****************
' ************** STOP, SET OPTIONS BELOW **************
' *****************************************************







' Are you using FSS (full screen, single screen mode?) set this to 1, VPX does not auto detect it well enough to not screw it up.
' If you are using FSS, you probably want to delete the Directb2s file unless you want a backglass popping up on the screen.
' Unless you actually want a backglass running for some reason, the go for it.
FSS = 0



' Do you want to display the LIGHT LEDs?  
' This if for playing in desktop mode when you dont want the pinMAME display for Score
' Set to 1 to enable an LED score board similar to the original Rowamet table
' Set to 1 to show the LEDs, set to 0 to hide them
' If you turn them off, make sure that you are either using a b2s backglass, or have chosen NOT to hide pinmame
LedShow = 0


' Do you want to show the desktop mode backbox Items? (Game Over, High Score, etc, the backglass Lights) 1 for Yes, 0 for No
' If you are playing desktop mode but using a Backglass monitor, you may want these off, and definitely if you are using FSS or FS
' or rotated in a cabinet.
' in Desktop mode they show you the gameover and extra ball and other backglass lights
DesktopBackBox = 0



'********************* REMINDER ***********************
' Lockbar Fire button adjust LUT levels
' *******************************************************************************************
' END OF USER DEFINABLE OPTIONS. PROCEDING BEYOND THIS POINT MAY BE HAZARDOUS TO YOUR SANITY*
' *******************************************************************************************
' MEANING YOU ARE DONE SETTING OPTIONS NOW, DONT CHANGE ANYTHING ELSE*
'*********************************************************************






' A SHADOW SHALL FALL OVER THE UNIVERSE
' AND EVIL SHALL GROW IN IT'S PATH
' AND DEATH
' SHALL COME FROM THE SKIES




dim bb
if DesktopBackBox = 0 Then
for each bb in DTlamps
bb.visible=0
Next
End If
' Declare Constants
Const cGameName="heavymtl",UseSolenoids=1,UseLamps=1
Const SSolenoidOn="solon",SSolenoidOff="soloff",SFlipperOn="metalhit_medium",SFlipperOff="metalhit_medium",SCoin="coinin"
Const TableName = "Heavy_Metal_Classic"
Const myVersion = "1.5.1"
Const BallSize = 50

 ' Declare Global Variables
Dim FSS
Dim UseBackglass
Dim seq
Dim bsTrough,bsSaucer,TrackC
TrackC=False
Dim DesktopBackBox
Dim lightLED, LEDnum
Dim GMS
Dim LedShow
Dim XLED







 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0


' Load the core.vbs for supporting Subs and functions
LoadVPM "01200100","Taito.VBS",3.1




' ************************************************************************

' Solenoids
SolCallback(1)= "Trough"  '"bsTrough.SolOut"			'ok
SolCallback(2)= "Kicker"  '"bsSaucer.SolOut"			'ok
SolCallback(3)="RaiseCenter"				'ok
SolCallback(4)="RaiseTop"					'ok
SolCallback(5)="RaiseLeft"					'ok
SolCallback(18)="vpmNudge.SolGameOn"		'ok
SolCallback(sLRFlipper)="vpmSolFlipper RightFlipper,Nothing,"
SolCallback(sLLFlipper)="vpmSolFlipper LeftFlipper,Nothing,"

Sub Trough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		PlaysoundAT SoundFX("ballrel", DOFContactors), Ballrelease 
		GMS = 0
	End If 
End Sub

Sub Kicker(Enabled)
	If Enabled Then
		bsSaucer.ExitSol_On
		PlaysoundAT SoundFX("the outdoor", DOFContactors), Kicker1
	End If 
End Sub


' ****************** SET UP TABLE CONTROLS *************************
Sub Table1_KeyDown(ByVal KeyCode)
	If Gameover.state = 0 Then
		If Tilted.state = 0 Then
			If KeyCode=RightFlipperKey Then ' JP's Physics 3.0 mods
				Controller.Switch(74)=1
				RightFlipper.TimerEnabled = True
        RightFlipper.RotateToEnd
        RightFlipperOn = 1
				PlaySoundAt SoundFX("fx_flipperup",DOFFlippers), RightFlipper
			End If

		If KeyCode=LeftFlipperKey Then  ' JP's Physics 3.0 mods
				LeftFlipper.TimerEnabled = True
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
				PlaySoundAt SoundFX("fx_flipperup",DOFFlippers), LeftFlipper
		End If
	End If

		If KeyCode=PlungerKey Then 
				Plunger.Pullback
				PlaySoundAt "plungerpull", plunger
		End If


		If KeyCode=PlungerKey Then Plunger.Pullback:PlaySoundAt "plungerpull", plunger
		End If



		if keycode=LockbarKey Then NextLUT


If Keycode = keyrules then 
	If table1.ShowDT = 0 then 
	Rules0.visible = 1
	Else
	Rules1.visible = 1
	End If
End If


		If vpmKeyDown(KeyCode) Then Exit Sub

End Sub



Sub Table1_KeyUp(ByVal KeyCode)
	If Gameover.state = 0 Then
		If Tilted.state = 0 Then
				If KeyCode=RightFlipperKey Then 
						Controller.Switch(74)=0
        RightFlipper.RotateToStart
        RightFlipperOn = 0
						PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), RightFlipper
				End If


				If keycode = LeftFlipperKey Then 
        LeftFlipper.RotateToStart
        LeftFlipperOn = 0
						PlaySoundAt SoundFX("fx_flipperdown",DOFFlippers), LeftFlipper
				End If
		End If
	End If


				If KeyCode=PlungerKey Then 
						Plunger.Fire
						PlaySoundAt "plunger", plunger
				End If


If Keycode = keyrules then 
	If table1.ShowDT = 0 then 
	Rules0.visible = 0
	Else
	Rules1.visible = 0
	End If
End If

	If vpmKeyUp(KeyCode) Then Exit Sub
End Sub

'*********************************************************
' Real Time Flipper adjustments - by JLouLouLou & JPSalas
'        (to enable flipper tricks) 
'*********************************************************

Dim FlipperPower
Dim FlipperElasticity
Dim SOSTorque, SOSAngle
Dim FullStrokeEOS_Torque, LiveStrokeEOS_Torque
Dim LeftFlipperOn
Dim RightFlipperOn

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim LiveCatchSensivity

FlipperPower = 5000
FlipperElasticity = 0.85
FullStrokeEOS_Torque = 0.3 	' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2	' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

LeftFlipper.EOSTorqueAngle = 10
RightFlipper.EOSTorqueAngle = 10

SOSTorque = 0.1
SOSAngle = 6

LiveCatchSensivity = 10

LLiveCatchTimer = 0
RLiveCatchTimer = 0

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's tricks timer
'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
	If LeftFlipperOn = 1 Then
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
			LeftFlipper.EOSTorque = FullStrokeEOS_Torque
			LLiveCatchTimer = LLiveCatchTimer + 1
			If LLiveCatchTimer < LiveCatchSensivity Then
				LeftFlipper.Elasticity = 0
			Else
				LeftFlipper.Elasticity = FlipperElasticity
				LLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		LeftFlipper.Elasticity = FlipperElasticity
		LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
		LLiveCatchTimer = 0
	End If
	

'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
 	If RightFlipperOn = 1 Then
		If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
			RightFlipper.EOSTorque = FullStrokeEOS_Torque
			RLiveCatchTimer = RLiveCatchTimer + 1
			If RLiveCatchTimer < LiveCatchSensivity Then
				RightFlipper.Elasticity = 0
			Else
				RightFlipper.Elasticity = FlipperElasticity
				RLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		RightFlipper.Elasticity = FlipperElasticity
		RightFlipper.EOSTorque = LiveStrokeEOS_Torque
		RLiveCatchTimer = 0
	End If
End Sub






'*********
'   LUT adapted from JP Salas
'*********

Dim LUTImage
Sub LoadLUT
	x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 10: UpdateLUT: SaveLUT: End Sub


'*** Load and Savee BG Volume settings




Sub UpdateLUT
	Select Case LutImage
		Case 0: table1.ColorGradeImage = ""
		Case 1: table1.ColorGradeImage = "colorgradelut256x16_1to1SL10"
		Case 2: table1.ColorGradeImage = "colorgradelut256x16_1to1SL20"
		Case 3: table1.ColorGradeImage = "colorgradelut256x16_1to1SL30"
		Case 4: table1.ColorGradeImage = "colorgradelut256x16_1to1SL40"
		Case 5: table1.ColorGradeImage = "colorgradelut256x16_1to1SL50"
		Case 6: table1.ColorGradeImage = "colorgradelut256x16_1to1SL60"
		Case 7: table1.ColorGradeImage = "colorgradelut256x16_1to1SL70"
		Case 8: table1.ColorGradeImage = "colorgradelut256x16_1to1SL80"
		Case 9: table1.ColorGradeImage = "colorgradelut256x16_1to1SL90"
	End Select
End Sub






' ********************* Start Table *************************************

Sub Table1_Init 
	On Error Resume Next
		With Controller
			.GameName=cGameName
			If Err Then MsgBox"Can't start Game"&cGameName&vbNewLine&Err.Description:Exit Sub
			.SplashInfoLine="Heavy Metal Classic(Rowamet 1983) 1.4" & vbNewLine & "VPX Table By Wiesshund" & vbNewLine & "Based on the concept of Destruct's VP9 table"
			.HandleMechanics=0
			.HandleKeyboard=0
			.ShowDMDOnly=1
			.ShowFrame=0
			.ShowTitle=0
			.Run
			.Hidden = 0
			If Err Then MsgBox Err.Description
		End With
	On Error Goto 0
	
	vpmNudge.TiltSwitch=30
	vpmNudge.Sensitivity=5
	vpmNudge.TiltObj=Array(LeftSlingshot001,RightSlingshot001,Bumper1,Bumper2,Bumper3,MiniSling2,MiniSling001,RightFlipper,LeftFlipper) 

	Set bsTrough=New cvpmBallStack
	bsTrough.InitSw 0,1,0,0,0,0,0,0
	bsTrough.InitKick BallRelease,110,10
	bsTrough.InitExitSnd "ballrel","SolOn"
	bsTrough.Balls=1

	Set bsSaucer=New cvpmBallStack
	bsSaucer.InitSaucer Kicker1,2,220,20
	bsSaucer.InitExitSnd"ballrel", "SolOn"

	vpmMapLights ALights


If FSS = 1 then 
	BACKGLASS()
	Displaytimer.Enabled=1
End If

If FSS = 0 then 
	Displaytimer.Enabled=0
End If





'Hiding LED digits
If LedShow = 0 then LEDHIDE.Enabled=1

LoadLUT ' load saved LUT file
End Sub

Sub LEDHIDE_Timer()
dim iteration,obj
If iteration = 0 then

iteration = (iteration + 1)
	For XLED = 0 To 25
		if XLED < 26 then
				For Each obj In DigitsLED(XLED)
					obj.visible=0
				Next
		end If
	Next
Me.Enabled=0
End If
End Sub


' Table Exit - kill the DMD and stop the controller
Sub table1_Exit
SaveLut
Controller.Stop
End Sub

'********************************* DRAIN **************************************
Sub Drain_Hit 'Switch 1
PlaysoundAt "Drain", Drain
bsTrough.AddBall Me
End Sub

'************************* END DRAIN ********************************************


'*************************** BALL LOCK ****************************************

Sub Kicker1_Hit
		PlaySoundAt "kicker_enter_center", Kicker1
		bsSaucer.AddBall 0
End Sub


'********************** END BALL LOCK ******************************************


'*********************** GATES ***********************************
Sub Gate1_Hit
PlaySoundAt "gate", Gate1
End Sub

Sub Gate2_Hit  'Switch 2
PlaySoundAt "gate", Gate2
End Sub

Sub Gate3_Hit  'Switch 2
PlaySoundAt "gate", Gate3
End Sub

Sub Gate001_Hit
PlaySoundAt "gate", Gate001
End Sub

'**************************** END GATES ****************************
																


'************************ BUMPERS with Stobes ************************
Sub Bumper1_Hit
vpmTimer.PulseSw 13 'Switch 13
PlaySoundAt SoundFX("fx_bumper1", DOFContactors), Bumper1
End Sub			


Sub Bumper2_Hit
vpmTimer.PulseSw 3 'Switch 3
PlaySoundAt SoundFX("fx_bumper2", DOFContactors), Bumper2
End Sub												

Sub Bumper3_Hit
vpmTimer.PulseSw 23 'Switch 23
PlaySoundAt SoundFX("fx_bumper3", DOFContactors), Bumper3
End Sub												
'************************* END BUMPERS **********************************


'******************************* OUT LANE TRIGGERS ************************
Sub M10_Hit ' right outlane, light22 'Switch 24
PlaySoundAt "switch1", M10
Controller.Switch(24)=1
End Sub																

Sub M10_unHit
Controller.Switch(24)=0
End Sub


Sub M7_Hit 'left outlane light23 'Switch 34
PlaySoundAt "switch1", M7
Controller.Switch(34)=1
End Sub

Sub M7_unHit
Controller.Switch(34)=0
End Sub

' ******************************** END OUT LANE TRIGGERS *********************


' **************************** INLANE TRIGGERS ****************************
Sub M8_Hit 'Switch 4, lamp 24
PlaySoundAt "switch1", M8
Controller.Switch(4)=1
End Sub	
															
Sub M8_unHit:Controller.Switch(4)=0:End Sub

Sub M9_Hit 'Switch 14 lamp 25
PlaySoundAt "switch1", M9
Controller.Switch(14)=1
End Sub										

Sub M9_unHit:Controller.Switch(14)=0:End Sub
'********************** END INLANE TRIGGERS ***********************************



' ************************* DROP TARGETS ******************************

'************************* LEFT TARGET BANK *****************************

Sub LB1_Dropped 'Switch 75
Controller.Switch(77)=1
Controller.Switch(75)=1
PlaySoundAt SoundFX("Target",  DOFDropTargets), LB1
End Sub	

Sub LB2_Dropped 'Switch 65
Controller.Switch(77)=1
Controller.Switch(65)=1
PlaySoundAt SoundFX("Target",  DOFDropTargets), LB2
End Sub	

Sub LB3_Dropped 'Switch 55
Controller.Switch(77)=1
Controller.Switch(55)=1
PlaySoundAt SoundFX("Target",  DOFDropTargets), LB3
End Sub	

Sub LB4_Dropped 'Switch 45
Controller.Switch(77)=1
Controller.Switch(45)=1
PlaySoundAt SoundFX("Target",  DOFDropTargets), LB4
End Sub	

Sub LB5_Dropped 'Switch 35
Controller.Switch(77)=1
Controller.Switch(35)=1
PlaySoundAt SoundFX("Target",  DOFDropTargets), LB5
End Sub	

' left bank Reset
Sub RaiseLeft(Enabled)
	If Enabled Then
		LB1.IsDropped=0
		LB2.IsDropped=0
		LB3.IsDropped=0
		LB4.IsDropped=0
		LB5.IsDropped=0
		Controller.Switch(35)=0
		Controller.Switch(45)=0
		Controller.Switch(55)=0
		Controller.Switch(65)=0
		Controller.Switch(75)=0
		Controller.Switch(77)=0
		PlaySoundAt SoundFX("FlapOpen", DOFContactors), LB3
	End If
End Sub

' LEFT BANK BACKING Targets

Sub Alvo5_Hit:vpmTimer.PulseSw 43
PlaysoundAt SoundFX("Targets", DOFTargets), Alvo5
End Sub	
															
Sub Alvo4_Hit:vpmTimer.PulseSw 43
PlaysoundAt SoundFX("Targets", DOFTargets), Alvo4
End Sub

Sub Alvo3_Hit:vpmTimer.PulseSw 43
PlaysoundAt SoundFX("Targets", DOFTargets), Alvo3
End Sub	

Sub Alvo2_Hit:vpmTimer.PulseSw 43
PlaysoundAt SoundFX("Targets", DOFTargets), Alvo2
End Sub	

Sub Alvo1_Hit:vpmTimer.PulseSw 43
PlaysoundAt SoundFX("Targets", DOFTargets), Alvo1
End Sub	



'*********************** CENTER TARGET BANK **********************


Sub C1_Dropped	'Switch 41
Controller.Switch(53)=1
Controller.Switch(41)=1
PlaySoundAt SoundFX("Target",  DOFDropTargets), C1
End Sub

Sub C2_Dropped	'Switch 51
Controller.Switch(51)=1
Controller.Switch(53)=1
PlaySoundAt SoundFX("Target",  DOFDropTargets), C2
End Sub

Sub C3_Dropped'Switch 61
Controller.Switch(61)=1
Controller.Switch(53)=1
PlaySoundAt SoundFX("Target",  DOFDropTargets), C3
End Sub

' center bank Reset
Sub RaiseCenter(Enabled)
	If Enabled Then
		C1.IsDropped=0
		C2.IsDropped=0
		C3.IsDropped=0
		Controller.Switch(41)=0
		Controller.Switch(51)=0
		Controller.Switch(61)=0
		Controller.Switch(53)=0
		PlaySoundAt SoundFX("FlapOpen", DOFContactors), C2
	End If
End Sub

'******************************* TOP TARGET BANK ***************************************
Dim TB
Sub T1_Dropped 'Switch 11
Controller.Switch(11)=1 
Controller.Switch(33)=1
PlaySoundAt SoundFX("Target",  DOFDropTargets), T1
End Sub	

Sub T2_Dropped 'Switch 21
Controller.Switch(21)=1
Controller.Switch(33)=1
PlaySoundAt SoundFX("Target",  DOFDropTargets), T2
End Sub	

Sub T3_Dropped 'Switch 31
Controller.Switch(31)=1
Controller.Switch(33)=1
PlaySoundAt SoundFX("Target",  DOFDropTargets), T3
End Sub	

'reset top Targets
Sub RaiseTop(Enabled)
	If Enabled Then
		T1.IsDropped=0
		T2.IsDropped=0
		T3.IsDropped=0
		Controller.Switch(11)=0
		Controller.Switch(21)=0
		Controller.Switch(31)=0
		Controller.Switch(33)=0
		PlaySoundAt SoundFX("FlapOpen", DOFContactors), T2
	End If
End Sub

'******************************** END TARGETS ****************************************
'***************************************************************************************
'
'
'
'
'***************************SPINNERS***************************************
' spinners with positional audio
Sub M4_Spin
vpmTimer.PulseSw 12
PlaySoundAt "spinnerclicking", M4
End Sub	



Sub M5_Spin 'Switch 22
vpmTimer.PulseSw 22 
PlaySoundAt "spinnerclicking", M5
End Sub		

Sub M6_Spin	'Switch 32
vpmTimer.PulseSw 32
PlaySoundAt "spinnerclicking", M6
End Sub

' *********************** End Spinners ******************************************


' *************** TOP LANES ********************************

Sub M1_Hit
PlaySoundAt "switch1", M1 
Controller.Switch(42)=1
End Sub																'Switch 42 'Light 37

Sub M1_unHit:Controller.Switch(42)=0:End Sub


Sub M2_Hit
PlaySoundAt "switch1", M2
Controller.Switch(52)=1
End Sub																'Switch 52 'Light 38

Sub M2_unHit:Controller.Switch(52)=0:End Sub


Sub M3_Hit
PlaySoundAt "switch1", M3
Controller.Switch(62)=1
End Sub	
															'Switch 62 'Light 39
Sub M3_unHit:Controller.Switch(62)=0:End Sub

' **************** end Lanes *************************************



' classic table slingshots
dim rstep: dim lstep
Sub RightSlingShot001_Slingshot 'Switch 54
	vpmTimer.PulseSw 54
		PlaySoundAt SoundFX("right_slingshot", DOFContactors), rslingaud
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot001.TimerEnabled = 1

End Sub

Sub RightSlingShot001_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot001.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot001_Slingshot
	vpmTimer.PulseSw 44 'Switch 44
    PlaySoundAt SoundFX("left_slingshot", DOFContactors), lslingaud
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot001.TimerEnabled = 1
	
End Sub

Sub LeftSlingShot001_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot001.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


Sub MiniSling001_Slingshot 'Switch 71 need documentation
	vpmTimer.PulseSw 71 
PlaySoundAt SoundFX("right_slingshot" ,DOFContactors), minisling12A
    MSling.Visible = 0
    MSling1.Visible = 1
    MiniSling3.rotx = 20
    MStep = 0
    MiniSling001.TimerEnabled = 1

End Sub

Dim MStep
Sub MiniSling001_Timer
    Select Case MStep
        Case 3:MSLing1.Visible = 0:MSLing2.Visible = 1:MiniSling3.rotx = 10
        Case 4:MSLing2.Visible = 0:MSLing.Visible = 1:MiniSling3.rotx = 0:MiniSling001.TimerEnabled = 0
    End Select
    MStep = MStep + 1
End Sub						

' end slow table slingshots

Sub MiniSling2_Hit:vpmTimer.PulseSw 64 'Switch 64 need documentation

PlaySoundAt SoundFX("right_slingshot" ,DOFContactors), minisling2A
End Sub																


' upper left pocket target 1000 points
Sub M11_Hit 'Switch 72
vpmTimer.PulseSw 72
PlaySoundAt "Targets", M11S
End Sub																	



																										

'**************************************************************************
'****************************SUPPORT ROUTINES******************************
'**************************************************************************
'***********************************
'   JP's VP10 Rolling Sounds v4.0
'   JP's Ball Shadows
'   JP's Ball Speed Control
'   Rothbauer's dropping sounds
'***********************************

Const tnob = 5   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 35 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_timer() 

    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        'aBallShadow(b).Y = 2100 'under the apron 'may differ from table to table
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' draw the ball shadow
    For b = lob to UBound(BOT)
        'aBallShadow(b).X = BOT(b).X
        'aBallShadow(b).Y = BOT(b).Y
        'aBallShadow(b).Height = BOT(b).Z - Ballsize / 2 + 1

    'play the rolling sound for each ball
        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 5
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

'


'***************************************************************
'             Supporting Ball & Sound Functions v4.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'*****************************************
'	ninuzzu's	FLIPPER SHADOWS v2, dont know who he is, but i ganked his stuff
'*****************************************

Sub LeftFlipper_Init()
    LeftFlipper.TimerInterval = 10
End Sub

Sub RightFlipper_Init()
    RightFlipper.TimerInterval = 10
End Sub





'***********************************************************
'********************END SUPPORT ROUTINES*******************
'***********************************************************


' ******************  THE END ********************* (but is it?)



'******************************
' materials Hit Sounds (New)
'******************************

Sub BallGuides_Hit(idx)
PlaySoundAtBall "Metal Hit 2"
End Sub

Sub RubberBands_Hit(idx)
PlaySoundAtBall "rubberband"
End Sub

Sub RubberPins_Hit(idx)
PlaySoundAtBall "rubber_hit_1"
End Sub

Sub Plastics_Hit(idx)
PlaySoundAtBall "plastique"
End Sub

Sub Metals_Hit(idx)
PlaySoundAtBall "Metal Hit 2"
End Sub

Sub Rubbers_Hit(idx)
PlaySoundAtBall "smgrubber"
End Sub


Sub LeftFlipper_Collide(parm)
    PlaySound "flippers", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "flippers", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub





'**********************************************************************************************************************
'************************************* TABLE TIMERS SECTION ***********************************************************
'**********************************************************************************************************************

' **************************** new gate primitives ********************************
Sub GateTimer_Timer()
   Gate2Flap.RotZ = ABS(Gate2.currentangle)
   Gate3Flap.RotZ = ABS(Gate3.currentangle)
End Sub	


'***************** watch for new high score set **************************


Dim IsTilt

' **** idle check, if game in play and nothing to do on DMD, returns ball in play count, as well as some non rom maintenance checks
Sub GILight_Timer()
Dim x



If gameover.state = 0 Then

	For Each x In Pflights:x.state=1
	Next

		Else

	For Each x In Pflights:x.state=0:Next

End If


If tilted.state = 1 then
	If IsTilt=0 then
		IsTilt = 1
	End If
Else
IsTilt=0
End If



		

End Sub

Sub Idle_Timer()

If Gameover.state=0 Then

End If

End Sub









'******************************************************************************************************************************************
'***************************************************** END OF TIMERS **********************************************************************
'******************************************************************************************************************************************


' declaring LED digits for player scores, need global for match counter
Dim P1D1,P1D2,P1D3,P1D4,P1D5,P1D6
Dim P2D1,P2D2,P2D3,P2D4,P2D5,P2D6
Dim P3D1,P3D2,P3D3,P3D4,P3D5,P3D6
Dim P4D1,P4D2,P4D3,P4D4,P4D5,P4D6






'******************************* FSS MODDING ************************
'************ flasher Digits
'******************* I learned this from the king kong table with the single LED on the playfield *****************
Dim Digits(26)

Digits(0)=Array(BALLFLA1,BALLFLA2,BALLFLA3,BALLFLA4,BALLFLA5,BALLFLA6,BALLFLA7) 'BALL COUNTER

Digits(1)=Array(P1D1FLA1,P1D1FLA2,P1D1FLA3,P1D1FLA4,P1D1FLA5,P1D1FLA6,P1D1FLA7) 'PLAYER 1 DIGIT 1
Digits(2)=Array(P1D2FLA1,P1D2FLA2,P1D2FLA3,P1D2FLA4,P1D2FLA5,P1D2FLA6,P1D2FLA7) 'PLAYER 1 DIGIT 2
Digits(3)=Array(P1D3FLA1,P1D3FLA2,P1D3FLA3,P1D3FLA4,P1D3FLA5,P1D3FLA6,P1D3FLA7) 'PLAYER 1 DIGIT 3
Digits(4)=Array(P1D4FLA1,P1D4FLA2,P1D4FLA3,P1D4FLA4,P1D4FLA5,P1D4FLA6,P1D4FLA7) 'PLAYER 1 DIGIT 4
Digits(5)=Array(P1D5FLA1,P1D5FLA2,P1D5FLA3,P1D5FLA4,P1D5FLA5,P1D5FLA6,P1D5FLA7) 'PLAYER 1 DIGIT 5
Digits(6)=Array(P1D6FLA1,P1D6FLA2,P1D6FLA3,P1D6FLA4,P1D6FLA5,P1D6FLA6,P1D6FLA7) 'PLAYER 1 DIGIT 6

Digits(7)=Array(P2D1FLA1,P2D1FLA2,P2D1FLA3,P2D1FLA4,P2D1FLA5,P2D1FLA6,P2D1FLA7) 'PLAYER 2 DIGIT 1
Digits(8)=Array(P2D2FLA1,P2D2FLA2,P2D2FLA3,P2D2FLA4,P2D2FLA5,P2D2FLA6,P2D2FLA7) 'PLAYER 2 DIGIT 2
Digits(9)=Array(P2D3FLA1,P2D3FLA2,P2D3FLA3,P2D3FLA4,P2D3FLA5,P2D3FLA6,P2D3FLA7) 'PLAYER 2 DIGIT 3
Digits(10)=Array(P2D4FLA1,P2D4FLA2,P2D4FLA3,P2D4FLA4,P2D4FLA5,P2D4FLA6,P2D4FLA7) 'PLAYER 2 DIGIT 4
Digits(11)=Array(P2D5FLA1,P2D5FLA2,P2D5FLA3,P2D5FLA4,P2D5FLA5,P2D5FLA6,P2D5FLA7) 'PLAYER 2 DIGIT 5
Digits(12)=Array(P2D6FLA1,P2D6FLA2,P2D6FLA3,P2D6FLA4,P2D6FLA5,P2D6FLA6,P2D6FLA7) 'PLAYER 2 DIGIT 6

Digits(13)=Array(P3D1FLA1,P3D1FLA2,P3D1FLA3,P3D1FLA4,P3D1FLA5,P3D1FLA6,P3D1FLA7) 'PLAYER 3 DIGIT 1
Digits(14)=Array(P3D2FLA1,P3D2FLA2,P3D2FLA3,P3D2FLA4,P3D2FLA5,P3D2FLA6,P3D2FLA7) 'PLAYER 3 DIGIT 2
Digits(15)=Array(P3D3FLA1,P3D3FLA2,P3D3FLA3,P3D3FLA4,P3D3FLA5,P3D3FLA6,P3D3FLA7) 'PLAYER 3 DIGIT 3
Digits(16)=Array(P3D4FLA1,P3D4FLA2,P3D4FLA3,P3D4FLA4,P3D4FLA5,P3D4FLA6,P3D4FLA7) 'PLAYER 3 DIGIT 4
Digits(17)=Array(P3D5FLA1,P3D5FLA2,P3D5FLA3,P3D5FLA4,P3D5FLA5,P3D5FLA6,P3D5FLA7) 'PLAYER 3 DIGIT 5
Digits(18)=Array(P3D6FLA1,P3D6FLA2,P3D6FLA3,P3D6FLA4,P3D6FLA5,P3D6FLA6,P3D6FLA7) 'PLAYER 3 DIGIT 6

Digits(19)=Array(P4D1FLA1,P4D1FLA2,P4D1FLA3,P4D1FLA4,P4D1FLA5,P4D1FLA6,P4D1FLA7) 'PLAYER 4 DIGIT 1
Digits(20)=Array(P4D2FLA1,P4D2FLA2,P4D2FLA3,P4D2FLA4,P4D2FLA5,P4D2FLA6,P4D2FLA7) 'PLAYER 4 DIGIT 2
Digits(21)=Array(P4D3FLA1,P4D3FLA2,P4D3FLA3,P4D3FLA4,P4D3FLA5,P4D3FLA6,P4D3FLA7) 'PLAYER 4 DIGIT 3
Digits(22)=Array(P4D4FLA1,P4D4FLA2,P4D4FLA3,P4D4FLA4,P4D4FLA5,P4D4FLA6,P4D4FLA7) 'PLAYER 4 DIGIT 4
Digits(23)=Array(P4D5FLA1,P4D5FLA2,P4D5FLA3,P4D5FLA4,P4D5FLA5,P4D5FLA6,P4D5FLA7) 'PLAYER 4 DIGIT 5
Digits(24)=Array(P4D6FLA1,P4D6FLA2,P4D6FLA3,P4D6FLA4,P4D6FLA5,P4D6FLA6,P4D6FLA7) 'PLAYER 4 DIGIT 6

Digits(25)=Array(COINFLA1,COINFLA2,COINFLA3,COINFLA4,COINFLA5,COINFLA6,COINFLA7) 'COIN COUNTER





'--------------------------------------
'------  Using Destruk's Display Code  ------ Light base LEDS
'--------------------------------------

Dim DigitsLED(26)
DigitsLED(0)=Array(BALLLED1,BALLLED2,BALLLED3,BALLLED4,BALLLED5,BALLLED6,BALLLED7) 'BALL COUNTER

DigitsLED(1)=Array(P1D1LED1,P1D1LED2,P1D1LED3,P1D1LED4,P1D1LED5,P1D1LED6,P1D1LED7) 'PLAYER 1 DIGIT 1
DigitsLED(2)=Array(P1D2LED1,P1D2LED2,P1D2LED3,P1D2LED4,P1D2LED5,P1D2LED6,P1D2LED7) 'PLAYER 1 DIGIT 2
DigitsLED(3)=Array(P1D3LED1,P1D3LED2,P1D3LED3,P1D3LED4,P1D3LED5,P1D3LED6,P1D3LED7) 'PLAYER 1 DIGIT 3
DigitsLED(4)=Array(P1D4LED1,P1D4LED2,P1D4LED3,P1D4LED4,P1D4LED5,P1D4LED6,P1D4LED7) 'PLAYER 1 DIGIT 4
DigitsLED(5)=Array(P1D5LED1,P1D5LED2,P1D5LED3,P1D5LED4,P1D5LED5,P1D5LED6,P1D5LED7) 'PLAYER 1 DIGIT 5
DigitsLED(6)=Array(P1D6LED1,P1D6LED2,P1D6LED3,P1D6LED4,P1D6LED5,P1D6LED6,P1D6LED7) 'PLAYER 1 DIGIT 6

DigitsLED(7)=Array(P2D1LED1,P2D1LED2,P2D1LED3,P2D1LED4,P2D1LED5,P2D1LED6,P2D1LED7) 'PLAYER 2 DIGIT 1
DigitsLED(8)=Array(P2D2LED1,P2D2LED2,P2D2LED3,P2D2LED4,P2D2LED5,P2D2LED6,P2D2LED7) 'PLAYER 2 DIGIT 2
DigitsLED(9)=Array(P2D3LED1,P2D3LED2,P2D3LED3,P2D3LED4,P2D3LED5,P2D3LED6,P2D3LED7) 'PLAYER 2 DIGIT 3
DigitsLED(10)=Array(P2D4LED1,P2D4LED2,P2D4LED3,P2D4LED4,P2D4LED5,P2D4LED6,P2D4LED7) 'PLAYER 2 DIGIT 4
DigitsLED(11)=Array(P2D5LED1,P2D5LED2,P2D5LED3,P2D5LED4,P2D5LED5,P2D5LED6,P2D5LED7) 'PLAYER 2 DIGIT 5
DigitsLED(12)=Array(P2D6LED1,P2D6LED2,P2D6LED3,P2D6LED4,P2D6LED5,P2D6LED6,P2D6LED7) 'PLAYER 2 DIGIT 6

DigitsLED(13)=Array(P3D1LED1,P3D1LED2,P3D1LED3,P3D1LED4,P3D1LED5,P3D1LED6,P3D1LED7) 'PLAYER 3 DIGIT 1
DigitsLED(14)=Array(P3D2LED1,P3D2LED2,P3D2LED3,P3D2LED4,P3D2LED5,P3D2LED6,P3D2LED7) 'PLAYER 3 DIGIT 2
DigitsLED(15)=Array(P3D3LED1,P3D3LED2,P3D3LED3,P3D3LED4,P3D3LED5,P3D3LED6,P3D3LED7) 'PLAYER 3 DIGIT 3
DigitsLED(16)=Array(P3D4LED1,P3D4LED2,P3D4LED3,P3D4LED4,P3D4LED5,P3D4LED6,P3D4LED7) 'PLAYER 3 DIGIT 4
DigitsLED(17)=Array(P3D5LED1,P3D5LED2,P3D5LED3,P3D5LED4,P3D5LED5,P3D5LED6,P3D5LED7) 'PLAYER 3 DIGIT 5
DigitsLED(18)=Array(P3D6LED1,P3D6LED2,P3D6LED3,P3D6LED4,P3D6LED5,P3D6LED6,P3D6LED7) 'PLAYER 3 DIGIT 6

DigitsLED(19)=Array(P4D1LED1,P4D1LED2,P4D1LED3,P4D1LED4,P4D1LED5,P4D1LED6,P4D1LED7) 'PLAYER 4 DIGIT 1
DigitsLED(20)=Array(P4D2LED1,P4D2LED2,P4D2LED3,P4D2LED4,P4D2LED5,P4D2LED6,P4D2LED7) 'PLAYER 4 DIGIT 2
DigitsLED(21)=Array(P4D3LED1,P4D3LED2,P4D3LED3,P4D3LED4,P4D3LED5,P4D3LED6,P4D3LED7) 'PLAYER 4 DIGIT 3
DigitsLED(22)=Array(P4D4LED1,P4D4LED2,P4D4LED3,P4D4LED4,P4D4LED5,P4D4LED6,P4D4LED7) 'PLAYER 4 DIGIT 4
DigitsLED(23)=Array(P4D5LED1,P4D5LED2,P4D5LED3,P4D5LED4,P4D5LED5,P4D5LED6,P4D5LED7) 'PLAYER 4 DIGIT 5
DigitsLED(24)=Array(P4D6LED1,P4D6LED2,P4D6LED3,P4D6LED4,P4D6LED5,P4D6LED6,P4D6LED7) 'PLAYER 4 DIGIT 6

DigitsLED(25)=Array(COINLED1,COINLED2,COINLED3,COINLED4,COINLED5,COINLED6,COINLED7) 'COIN COUNTER


Sub DisplayTimerLED_Timer
Dim ChgLED,ii,num,chg,stat,obj,obj1
If FSS = 0 then
		ChgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if num < 26 then
				For Each obj In DigitsLED(num)
					obj.visible=LedShow
					If chg And 1 Then obj.State = stat And 1
					chg = chg\2 : stat = stat\2
				Next
			end If
		Next
	End If
End If

If FSS = 1 then
		ChgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if num < 26 then
				For Each obj In DigitsLED(num)
					obj.visible=0
					If chg And 1 Then obj.State = stat And 1
					chg = chg\2 : stat = stat\2
				Next
			end If
		Next
	End If
End If


	If gameover.state=1 then
		if GMS = 0 then
			PlaySound SoundFX("knocker", DOFKnocker) 
			GMS = 1
		End If
	End If
End Sub


Sub DisplayTimer_Timer ' trying to set flahsers to the LED state, since score cant be read from flasher states

BALLFLA1.visible = 	BALLLED1.state
BALLFLA2.visible = 	BALLLED2.state
BALLFLA3.visible = 	BALLLED3.state
BALLFLA4.visible = 	BALLLED4.state
BALLFLA5.visible = 	BALLLED5.state
BALLFLA6.visible = 	BALLLED6.state
BALLFLA7.visible = 	BALLLED7.state
		
P1D1FLA1.visible = 	P1D1LED1.state
P1D1FLA2.visible = 	P1D1LED2.state
P1D1FLA3.visible = 	P1D1LED3.state
P1D1FLA4.visible = 	P1D1LED4.state
P1D1FLA5.visible = 	P1D1LED5.state
P1D1FLA6.visible = 	P1D1LED6.state
P1D1FLA7.visible = 	P1D1LED7.state
P1D2FLA1.visible = 	P1D2LED1.state
P1D2FLA2.visible = 	P1D2LED2.state
P1D2FLA3.visible = 	P1D2LED3.state
P1D2FLA4.visible = 	P1D2LED4.state
P1D2FLA5.visible = 	P1D2LED5.state
P1D2FLA6.visible = 	P1D2LED6.state
P1D2FLA7.visible = 	P1D2LED7.state
P1D3FLA1.visible = 	P1D3LED1.state
P1D3FLA2.visible = 	P1D3LED2.state
P1D3FLA3.visible = 	P1D3LED3.state
P1D3FLA4.visible = 	P1D3LED4.state
P1D3FLA5.visible = 	P1D3LED5.state
P1D3FLA6.visible = 	P1D3LED6.state
P1D3FLA7.visible = 	P1D3LED7.state
P1D4FLA1.visible = 	P1D4LED1.state
P1D4FLA2.visible = 	P1D4LED2.state
P1D4FLA3.visible = 	P1D4LED3.state
P1D4FLA4.visible = 	P1D4LED4.state
P1D4FLA5.visible = 	P1D4LED5.state
P1D4FLA6.visible = 	P1D4LED6.state
P1D4FLA7.visible = 	P1D4LED7.state
P1D5FLA1.visible = 	P1D5LED1.state
P1D5FLA2.visible = 	P1D5LED2.state
P1D5FLA3.visible = 	P1D5LED3.state
P1D5FLA4.visible = 	P1D5LED4.state
P1D5FLA5.visible = 	P1D5LED5.state
P1D5FLA6.visible = 	P1D5LED6.state
P1D5FLA7.visible = 	P1D5LED7.state
P1D6FLA1.visible = 	P1D6LED1.state
P1D6FLA2.visible = 	P1D6LED2.state
P1D6FLA3.visible = 	P1D6LED3.state
P1D6FLA4.visible = 	P1D6LED4.state
P1D6FLA5.visible = 	P1D6LED5.state
P1D6FLA6.visible = 	P1D6LED6.state
P1D6FLA7.visible = 	P1D6LED7.state
		
P2D1FLA1.visible = 	P2D1LED1.state
P2D1FLA2.visible = 	P2D1LED2.state
P2D1FLA3.visible = 	P2D1LED3.state
P2D1FLA4.visible = 	P2D1LED4.state
P2D1FLA5.visible = 	P2D1LED5.state
P2D1FLA6.visible = 	P2D1LED6.state
P2D1FLA7.visible = 	P2D1LED7.state
P2D2FLA1.visible = 	P2D2LED1.state
P2D2FLA2.visible = 	P2D2LED2.state
P2D2FLA3.visible = 	P2D2LED3.state
P2D2FLA4.visible = 	P2D2LED4.state
P2D2FLA5.visible = 	P2D2LED5.state
P2D2FLA6.visible = 	P2D2LED6.state
P2D2FLA7.visible = 	P2D2LED7.state
P2D3FLA1.visible = 	P2D3LED1.state
P2D3FLA2.visible = 	P2D3LED2.state
P2D3FLA3.visible = 	P2D3LED3.state
P2D3FLA4.visible = 	P2D3LED4.state
P2D3FLA5.visible = 	P2D3LED5.state
P2D3FLA6.visible = 	P2D3LED6.state
P2D3FLA7.visible = 	P2D3LED7.state
P2D4FLA1.visible = 	P2D4LED1.state
P2D4FLA2.visible = 	P2D4LED2.state
P2D4FLA3.visible = 	P2D4LED3.state
P2D4FLA4.visible = 	P2D4LED4.state
P2D4FLA5.visible = 	P2D4LED5.state
P2D4FLA6.visible = 	P2D4LED6.state
P2D4FLA7.visible = 	P2D4LED7.state
P2D5FLA1.visible = 	P2D5LED1.state
P2D5FLA2.visible = 	P2D5LED2.state
P2D5FLA3.visible = 	P2D5LED3.state
P2D5FLA4.visible = 	P2D5LED4.state
P2D5FLA5.visible = 	P2D5LED5.state
P2D5FLA6.visible = 	P2D5LED6.state
P2D5FLA7.visible = 	P2D5LED7.state
P2D6FLA1.visible = 	P2D6LED1.state
P2D6FLA2.visible = 	P2D6LED2.state
P2D6FLA3.visible = 	P2D6LED3.state
P2D6FLA4.visible = 	P2D6LED4.state
P2D6FLA5.visible = 	P2D6LED5.state
P2D6FLA6.visible = 	P2D6LED6.state
P2D6FLA7.visible = 	P2D6LED7.state
		
P3D1FLA1.visible = 	P3D1LED1.state
P3D1FLA2.visible = 	P3D1LED2.state
P3D1FLA3.visible = 	P3D1LED3.state
P3D1FLA4.visible = 	P3D1LED4.state
P3D1FLA5.visible = 	P3D1LED5.state
P3D1FLA6.visible = 	P3D1LED6.state
P3D1FLA7.visible = 	P3D1LED7.state
P3D2FLA1.visible = 	P3D2LED1.state
P3D2FLA2.visible = 	P3D2LED2.state
P3D2FLA3.visible = 	P3D2LED3.state
P3D2FLA4.visible = 	P3D2LED4.state
P3D2FLA5.visible = 	P3D2LED5.state
P3D2FLA6.visible = 	P3D2LED6.state
P3D2FLA7.visible = 	P3D2LED7.state
P3D3FLA1.visible = 	P3D3LED1.state
P3D3FLA2.visible = 	P3D3LED2.state
P3D3FLA3.visible = 	P3D3LED3.state
P3D3FLA4.visible = 	P3D3LED4.state
P3D3FLA5.visible = 	P3D3LED5.state
P3D3FLA6.visible = 	P3D3LED6.state
P3D3FLA7.visible = 	P3D3LED7.state
P3D4FLA1.visible = 	P3D4LED1.state
P3D4FLA2.visible = 	P3D4LED2.state
P3D4FLA3.visible = 	P3D4LED3.state
P3D4FLA4.visible = 	P3D4LED4.state
P3D4FLA5.visible = 	P3D4LED5.state
P3D4FLA6.visible = 	P3D4LED6.state
P3D4FLA7.visible = 	P3D4LED7.state
P3D5FLA1.visible = 	P3D5LED1.state
P3D5FLA2.visible = 	P3D5LED2.state
P3D5FLA3.visible = 	P3D5LED3.state
P3D5FLA4.visible = 	P3D5LED4.state
P3D5FLA5.visible = 	P3D5LED5.state
P3D5FLA6.visible = 	P3D5LED6.state
P3D5FLA7.visible = 	P3D5LED7.state
P3D6FLA1.visible = 	P3D6LED1.state
P3D6FLA2.visible = 	P3D6LED2.state
P3D6FLA3.visible = 	P3D6LED3.state
P3D6FLA4.visible = 	P3D6LED4.state
P3D6FLA5.visible = 	P3D6LED5.state
P3D6FLA6.visible = 	P3D6LED6.state
P3D6FLA7.visible = 	P3D6LED7.state
		
P4D1FLA1.visible = 	P4D1LED1.state
P4D1FLA2.visible = 	P4D1LED2.state
P4D1FLA3.visible = 	P4D1LED3.state
P4D1FLA4.visible = 	P4D1LED4.state
P4D1FLA5.visible = 	P4D1LED5.state
P4D1FLA6.visible = 	P4D1LED6.state
P4D1FLA7.visible = 	P4D1LED7.state
P4D2FLA1.visible = 	P4D2LED1.state
P4D2FLA2.visible = 	P4D2LED2.state
P4D2FLA3.visible = 	P4D2LED3.state
P4D2FLA4.visible = 	P4D2LED4.state
P4D2FLA5.visible = 	P4D2LED5.state
P4D2FLA6.visible = 	P4D2LED6.state
P4D2FLA7.visible = 	P4D2LED7.state
P4D3FLA1.visible = 	P4D3LED1.state
P4D3FLA2.visible = 	P4D3LED2.state
P4D3FLA3.visible = 	P4D3LED3.state
P4D3FLA4.visible = 	P4D3LED4.state
P4D3FLA5.visible = 	P4D3LED5.state
P4D3FLA6.visible = 	P4D3LED6.state
P4D3FLA7.visible = 	P4D3LED7.state
P4D4FLA1.visible = 	P4D4LED1.state
P4D4FLA2.visible = 	P4D4LED2.state
P4D4FLA3.visible = 	P4D4LED3.state
P4D4FLA4.visible = 	P4D4LED4.state
P4D4FLA5.visible = 	P4D4LED5.state
P4D4FLA6.visible = 	P4D4LED6.state
P4D4FLA7.visible = 	P4D4LED7.state
P4D5FLA1.visible = 	P4D5LED1.state
P4D5FLA2.visible = 	P4D5LED2.state
P4D5FLA3.visible = 	P4D5LED3.state
P4D5FLA4.visible = 	P4D5LED4.state
P4D5FLA5.visible = 	P4D5LED5.state
P4D5FLA6.visible = 	P4D5LED6.state
P4D5FLA7.visible = 	P4D5LED7.state
P4D6FLA1.visible = 	P4D6LED1.state
P4D6FLA2.visible = 	P4D6LED2.state
P4D6FLA3.visible = 	P4D6LED3.state
P4D6FLA4.visible = 	P4D6LED4.state
P4D6FLA5.visible = 	P4D6LED5.state
P4D6FLA6.visible = 	P4D6LED6.state
P4D6FLA7.visible = 	P4D6LED7.state
		
COINFLA1.visible = 	COINLED1.state
COINFLA2.visible = 	COINLED2.state
COINFLA3.visible = 	COINLED3.state
COINFLA4.visible = 	COINLED4.state
COINFLA5.visible = 	COINLED5.state
COINFLA6.visible = 	COINLED6.state
COINFLA7.visible = 	COINLED7.state

center_digits
End Sub

Dim xoff,yoff,zoff,xrot,zscale, ycen,xcen


SUB BACKGLASS()
xoff=550
yoff=0
zoff=600
xrot= -75

backglasslit.x = xoff
backglasslit.y = yoff
backglasslit.height = zoff
backglasslit.rotx = xrot

backglassframe.x = xoff
backglassframe.y = yoff
backglassframe.height = zoff
backglassframe.rotx = xrot




display_gameover.x = xoff -75
display_gameover.y = yoff
display_gameover.height = zoff + 330
display_gameover.rotx = xrot

display_ExtraBall.x = xoff - 25
display_ExtraBall.y = yoff
display_ExtraBall.height = zoff + 225
display_ExtraBall.rotx = xrot

display_SamePlayer.x = xoff -25
display_SamePlayer.y = yoff
display_SamePlayer.height = zoff + 125
display_SamePlayer.rotx = xrot

display_tilt.x = xoff + 75
display_tilt.y = yoff
display_tilt.height = zoff + 400
display_tilt.rotx = xrot

display_newrecord.x = xoff + 295
display_newrecord.y = yoff
display_newrecord.height = zoff + 100
display_newrecord.rotx = xrot

display_highscore.x = xoff + 295
display_highscore.y = yoff
display_highscore.height = zoff -130
display_highscore.rotx = xrot

END Sub

Dim pp, xx, yy, xfact, yfact, objs, zoff2, xoff2, x
Sub center_digits()
zoff2 = zoff - 500
xoff2 = 30
zscale = 0.0000001

xcen =(1090 /2) - (142 / 2)
ycen = (1083 /2 ) + (167 /2)

yfact =-2
xfact =-5

for pp =0 to 25
	For Each objs In Digits(pp)


xx = objs.x 
		If(xx < 0.) then
		x = x + (100 - x)
		end if

		
	objs.x = xx

	yy = objs.y 

		If(yy < 0.) then
		yy = yy * -1
		end if

	objs.y = yy



	objs.height = zoff2 + yy +140
	
	objs.rotx = xrot
	Next
	Next
end sub


Sub BallInPlay

End Sub






