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
'* Version change to 1.4.0
'* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'* 1.4.0 Notes
'* ADDED: full DOF support added, toys will now go nuts and shake thump flash and vibrate
'* ADDED: Special ball affects
'* ADDED: Physics refinements
'* ADDED: Some new primitives and a few updated textures
'* ADDED: Full SSF collision sounds, but mostly inaudible unless you play the table in mode 0
'* ADDED: Table Play mode. Set to low speed table to play more like the real life table
'*  Or leave it set at 0 to play more akin to "What if the Movie made the Table?"
'*  The table physics, including slingshot and bumper strengths will adjust accordingly
'* ADDED: 1 new ball lost event, you will now avenge on ball 3 and get threatened on ball 4
'*   you will see, it will explain itself.
'* 
'* CHANGED: Moved all music, so as to keep your music folder organized
'*  just take all the music files starting with HM- and the Xtraball.mp3 and move them to a folder inside Music named HeavyMetal
'* CHANGE: Updated some graphics, rotated all POV's for standard cabinet users including FSS POV, updated FSS Backdrop
'*  if you want to play FSS in desktop orientation, you are kind of on your own to adjust that
'* 
'* Special thanks to Conehead for being the DOF guineapig
'* Special thanks to JP Salas for guide to better physics
'* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'Version 1.5.1 Notes
'
'Table moved to use ROM alias for DOF purposes, as it conflicts with an existing DOFconfig
'alias info included in zip file
' List of DOF even ID's and functions

'* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'E101 Left flipper
'E102 Right flipper
'E103 Slingshot left
'E104 Slingshot right
'E105 Bumper back left
'E106 Bumper back center
'E107 Bumper back right
'E108 Bumper Middle Left  -- Left DropTarget Hit
'E109 Bumper Middle Center -- CenterDrop Target Hit
'E110 Bumper Middle Right -- Top DropTarget Reset
'E111 Knocker
'E112 Shaker
'E113 Ballrelease 
'E114 Red flashers
'E115 Green flashers
'E116 Blue flashers
'E117 Beacons 
'E118 
'E119 Strobes
'E121 Right Kicker - Above Lower Right Slingshot
'E120 Red undercab
'E122 Green undercab
'E123 Blue undercab
'E124 Left Target Reset
'E125 Center Target Reset
'E126 Top Target Reset
'E127 Top Slingshot left
'E128 Top Slingshot right
'E129 
'E130 
'E131 

'* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'Attempt to reduce CPU load of timers that is causing some people problems as their pc cant keep up with them for some reason
'reworked and eliminated some timers
'rewrote timer controls
'
'Added a few new DMD scenes
'Rewrote some DMD events
'Eliminated some sub routines
'
'If that does not make it playable for the people having issues all i can suggest is play it in mode 0 or 1 without the DMD
'
'Added a Backglass with the segmented LED's removed as some players asked for one

'Adjust iteration of VPM pulse timer

'Added user option to change resolution of DMD display
'USE AT OWN RISK!
'!!!!!DO NOT USE AT ALL IF YOU HAVE A REAL DMD UNLESS YOU DO NOT LIKE YOUR REAL DMD!!!!
'* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'1.5.1 Notes
'Audio Changes
'Sounds moved to backglass where appropriate
'All sound calls changed to be fully DOF and SFF compliant
'Active players workaround added for machines that update the vpm display slow and cant get the active players fast enough
'causing a divide by zero error on line 751
'Added a sub routine for bstrough and bsSaucer to give DOF and SFF compliant sound playback
'Some additional DOF events added
' Added a volume control for the Backglass sounds, use left and right magnasave keys, there is audio feedback to hear the level
' saves on table exit
' DOF redesigned courtesy of Outhere, who took the time to run the table on his cab and edit the DOF to give better dispersion
' among the contactors, and ensure what if the cab owner sets DOF audio to disabled, the sounds wont play.
' AND Outhere created a config for the table on DOFConfigtool named Heavymetal (Wiesshund)
' If you have a DOF equipped cab, you need to send Outhere a thankyou (No Seriously, i'm not kidding)
'* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'1.5.2
' Small Update
' Added a missing DMD scene that was supposed to play when red bonus lamp was lit for the ball Lock
' Outhere tightened up DOF, beacon support now working

'V4.0 Update
'Updated Table to VPX 10.8 lighting
'Updated table to a modified version of JP's 4.0 physics
'Remove all CLassic Mode play from table, if you want that download the classic version
'You can no longer disable music, get the classic version if you dont want music
'Score is displayed via DMD only now, use classic table if you want LED scoring
'Lockbar Fire changes LUT 
'Magnasaves change Backglass effects volume
'Right flipper during game over cycles DMD colors
'Only fantasy mode brutal physics, get the classic table for normal play 
'Cleaned unneeded objects from table and script 
'New backglasses created. clean less busy, as the original machine never had any backglass lamps anyways
'DMD frames and styles created, read the text file, it explaines how to use the styles
'DMD can display in 128x32 or 256x64 4.1 size, or can display in HD 16.9 full screen ar 128x71 dots or 254x143 dots 
'(Going higher will kill your CPU but if you want to hack it, freel free to try anything up to 720x404, have to make your own style though, you are on your own for this)
'Adjusted various table parts
'Adjusted glowball 
'Removed fixed ball shadows due to 10.8 lighting.



'*********** DO NOT EDIT THIS PIECE *****************
		Option Explicit '                           *
			Randomize       '                       *
'**************** NO NO NO **************************

' *****************************************************
' ***************** USER OPTIONS HERE *****************
' ************** STOP, SET OPTIONS BELOW **************
' *****************************************************





' Did you read the READ ME 1st.txt file that came with the download?
' hopefully you did, or the table wont work

'*************************************
'Player Options (You can and should edit these)
'You have to set these and set UserOptionsSet from 0 to 1
'Or the table is going to keep tossing you out over and over
'Read over each option, each explains what it does
'*************************************

' Set this to 1 to acknowledge you have set your user options
' Sorry, this is to avoid people not setting them and then saying Why dont this work
UserOptionsSet = 1

' Do you want VPX to use B2S server? set 0 for no, 1 for yes
' If you dont want to run a backglass server for a preformance reason, the will stop VPX from loading it, even if the B2S file exists, by setting to 0
' This would be mostly a preformance thing, it will disable B2S server, but it will also disable DOF entirely
' would only be useful on a slow PC, or debugging so the default is 1, i recommend dont change it unless you have good reason
Usebackglass = 1

' Are you using FSS (full screen, single screen mode?) set this to 1, VPX does not auto detect it well enough to not screw it up.
' If you are using FSS, you probably want to delete the Directb2s file unless you want a backglass popping up on the screen.
' Unless you actually want a backglass running for some reason, the go for it.
FSS = 0

' Do you want to hide the PinMAME display? 1 for yes, 0 for no
' Most people will want the VpinMAME display hidden, but you can turn it on here, if you want it to show for any reason
HidePinMAME = 1


' If you only have a single screen desktop then this will show the DMD in table, on the apron, where the instruction cards are.
' if you have a DMD screen leave this off as it increases CPU load 
' 0 for OFF 1 for ON
ApronDMDShow = 0


' If you have only single screen you may wish to turn OFF the external DMD display and just use the apron display, save some CPU cycles
' 1 for Yes, 0 for No
ShowFlexDMD = 1

' Do you want to show the desktop mode backbox Items? (Game Over, High Score, etc, the backglass Lights) 1 for Yes, 0 for No
' If you are playing desktop mode but using a Backglass monitor, you may want these off, and definitely if you are using FSS or FS
' or rotated in a cabinet.
' in Desktop mode they show you the gameover and extra ball and other backglass lights
DesktopBackBox = 0







' *****************************************************************************************
' *****************************    !!!!!WARNING!!!!   *************************************
' *****************************************************************************************
' ***                                                                                 *****
' *** HiRez affects the resolution of the DMD. Not in size, but in LED dots processed   ***
' ***       DO NOT CHANGE THIS IS YOU HAVE A REAL DMD SCREEN IT MAY DAMAGE IT!!!!       ***
' ***  FOR LCD DMD USERS ONLY!!!!!!                                                     ***
' ***  A DMD is normally a grid of 128 long by 32 LEDs tall                             ***
' ***  For an LCD that is rather low detail as the LCD has more pixels than that in     ***
' *** the space of the DMD, this will boost the amount of virtual DMD pixels the LCD    ***
' ***  has to work with.                                                                ***
' ***  0 is the default, 128*32 compantible with PinDMD, Pin2DMD, ColorDMD etc          ***
' ***  1 doubles the pixels to 256x64,                                                  ***
' ***  2 changes to 16.9 aperture for 128x71 pixels                                     ***
' ***  3 changes to 16.9 aperture for 256x143 pixels                                    ***
' ***  For 16.9 DMD i suggest a style with square seamless pixels for freezys           ***
' ***          you will need to edit the Style for optimal appearance                   ***
' *** Again, if you do not use an LCD for a DMD, DONT FUCK WITH THIS, you've been warned***
' *****************************************************************************************
' *****************************    !!!!!WARNING!!!!   *************************************
' *****************************************************************************************
DMDSize=0



'********************* REMINDER ***********************
' magnasave buttons adjust backglass effects volume
' Lockbar Fire button adjust LUT levels
' Right flipper when in gameover changes DMD color

' *******************************************************************************************
' END OF USER DEFINABLE OPTIONS. PROCEDING BEYOND THIS POINT MAY BE HAZARDOUS TO YOUR SANITY*
' *******************************************************************************************
' MEANING YOU ARE DONE SETTING OPTIONS NOW, DONT CHANGE ANYTHING ELSE*
'*********************************************************************






' A SHADOW SHALL FALL OVER THE UNIVERSE
' AND EVIL SHALL GROW IN IT'S PATH
' AND DEATH
' SHALL COME FROM THE SKIES



' quick and dirty table play speed change




	Table1.GlobalDifficulty = 100




' Declare Constants
Const cGameName="HeavyMetal",UseSolenoids=1,UseLamps=1
Const SSolenoidOn="solon",SSolenoidOff="soloff",SFlipperOn="metalhit_medium",SFlipperOff="metalhit_medium",SCoin="coinin"
Const TableName = "heavymtl"
Const myVersion = "1.5.1"
Const UltraDMD_VideoMode_Stretch = 0
Const UltraDMD_VideoMode_Top = 1
Const UltraDMD_VideoMode_Middle = 2
Const UltraDMD_VideoMode_Bottom = 3

' JP's Physics 3.0
Const BallSize = 50
Const BallMass = 1.7

 ' Declare Global Variables
Dim UserOptionsSet
Dim FSS
Dim Players
Dim Player
Dim UseBackglass
Dim musicNum
Dim seq
Dim bsTrough,bsSaucer,TrackC
TrackC=False
Dim HidePinMAME
Dim DesktopBackBox
Dim lightLED, LEDnum
Dim ballcount
Dim Score(4)
Dim ApronDMDShow
Dim ShowFlexDMD
Dim FlexDMD 
Dim UltraDMD
Dim fso
Dim curDir
Dim Sameshot
Dim plungeback
Dim Xtra
Dim GMS
Dim XLED
Dim DMDSize
Dim BgVolume
Dim DMDCOLOR






 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0
If Usebackglass=0 then B2SOff=True 'This will also enable DOF
If UseBackglass=1 then B2SOff=False 'This will Definitely disable DOF

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
		PlaysoundAT SoundFXDOF("ballrel", 113, 2, DOFContactors), Ballrelease 
		DOF 112, 0 ' make sure shaker turns off when ball comes back into play
	End If 
End Sub

Sub Kicker(Enabled)
	If Enabled Then
		bsSaucer.ExitSol_On
		PlaysoundAT SoundFXDOF("the outdoor",121, 2 , DOFContactors), Kicker1
		DOF 117, 0  'Turns Beacons off
	End If 
End Sub


' ****************** SET UP TABLE CONTROLS *************************
' ****************** LANE CHANGE and key shooter, DOF sound on flippers
Sub Table1_KeyDown(ByVal KeyCode) 
	If Gameover.state = 0 Then
		If Tilted.state = 0 Then
			If KeyCode=RightFlipperKey Then 
				Controller.Switch(74)=1
        RightFlipper.RotateToEnd
        RightFlipperOn = 1
				PlaySoundAt SoundFXDOF("fx_flipperup",102, 1,DOFFlippers), RightFlipper
				rflip.duration 1, 40, 0
				rflip2.Duration 1, 20, 0
			End If

		If KeyCode=LeftFlipperKey Then 
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
				PlaySoundAt SoundFXDOF("fx_flipperup",101, 1,DOFFlippers), LeftFlipper
				lflip.duration 1, 40, 0
				lflip2.Duration 1, 20, 0
		End If

		If KeyCode=PlungerKey Then 
				Plunger.Pullback
				PlaySoundAt "plungerpull", plunger
						UltraDMD.CancelRendering():UltraDMD.DisplayScene00Ex "plungerpull.wmv", "", 30, -1, "", -1, -1, 14, 4000, 14
		End If

		if keycode=StartGameKey Then PlaySound "Playerup", 0, BgVolume
		Else 
		If KeyCode=PlungerKey Then Plunger.Pullback:PlaySoundAt "plungerpull", plunger
		End If
	End If 	

		if keycode=StartGameKey Then
			PlaySound "Playerup", 0, BgVolume
		End If

		if keycode=LockbarKey Then NextLUT

	If Keycode = LeftMagnaSave then 
			If BgVolume > 0 Then
				BgVolume = (BgVolume - 0.02)
				Playsound "BG_VolumeDown", 0, BgVolume
			End If
	End If

	If Keycode = RightMagnaSave then 
		If BgVolume < 1 Then
			BgVolume = (BgVolume + 0.02)
			Playsound "BG_VolumeUp", 0, BgVolume
		End If
	End If

If Keycode = keyrules then 
	If table1.ShowDT = 0 then 
	Rules0.visible = 1
	Else
	Rules1.visible = 1
	End If
End If

	If Gameover.state = 1 Then
			If KeyCode=RightFlipperKey Then
					CHGDMDCOLOR
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
						PlaySoundAt SoundFXDOF("fx_flipperdown",102, 0, DOFFlippers), RightFlipper
				End If

				If keycode = LeftFlipperKey Then 
        LeftFlipper.RotateToStart
        LeftFlipperOn = 0
						PlaySoundAt SoundFXDOF("fx_flipperdown", 101, 0 ,DOFFlippers), LeftFlipper
				End If


				If KeyCode=PlungerKey Then 
						Plunger.Fire
						PlaySoundAt "plunger", plunger:shooter.duration 1, 50, 0:shooter001.duration 1, 50, 0
								BallInPlay
					End If
				Else
				If KeyCode=PlungerKey Then Plunger.Fire
			End If
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


Sub LoadBGV
Dim x
	x = LoadValue(cGameName, "BgVolume")
    If(x <> "") Then BgVolume = x Else BgVolume = 1
	End Sub

Sub SaveBGV
    SaveValue cGameName, "BgVolume", BgVolume
End Sub


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


Sub CHGDMDCOLOR
Dim DMDCLRSET
		DMDCOLOR = (DMDCOLOR +1 ) MOD 7
		UpdateDMDColor
				If DMDCOLOR = 0 then DMDCLRSET = "ORANGE"
				If DMDCOLOR = 1 then DMDCLRSET = "GREEN"
				If DMDCOLOR = 2 then DMDCLRSET = "CYAN"
				If DMDCOLOR = 3 then DMDCLRSET = "YELLOW"
				If DMDCOLOR = 4 then DMDCLRSET = "RED"
				If DMDCOLOR = 5 then DMDCLRSET = "PINK"
				If DMDCOLOR = 6 then DMDCLRSET = "PURPLE"
			UltraDMD.CancelRendering
			UltraDMD.DisplayScene00Ex "background.wmv", "DMD Adjustment", 30, -1, Cstr(DMDCLRSET), 30, -1, 14, 10000, 1
		SaveDMD
End Sub

Sub UpdateDMDColor ' RGB(DMDCOLOR)    	FlexDMD.Color = DMDCOLOR
	Select Case DMDCOLOR
		Case 0: FlexDMD.Color = RGB(255,89,8) 'ORANGE
		Case 1: FlexDMD.Color = RGB(31,219,5) 'GREEN
		Case 2: FlexDMD.Color = RGB(61,213,255) 'CYAN
		Case 3: FlexDMD.Color = RGB(255,251,5) 'YELLO
		Case 4: FlexDMD.Color = RGB(255,33,3) 'RED
		Case 5: FlexDMD.Color = RGB(255,35,204) 'PINK
		Case 6: FlexDMD.Color = RGB(131,44,245) 'PURPLE
	End Select
End Sub

Sub SaveDMD
    SaveValue cGameName, "DMDCOLOR", DMDCOLOR
End Sub

Sub LoadDMDCOLOR
Dim x
	x = LoadValue(cGameName, "DMDCOLOR")
    If(x <> "") Then DMDCOLOR = x Else DMDCOLOR = 0
	UpdateDMDColor
End Sub


'**************************** ANALOG PLUNGER ***************************************************
Sub Plunger_Timer()
If Gameover.state = 0 Then
  If Tilted.state = 0 Then
	If Plunger.Position > 4.6 and plungeback = 0 Then
	PlaySoundAt "plungerpull", plunger
	UltraDMD.CancelRendering():UltraDMD.DisplayScene00Ex "plungerpull.wmv", "", 30, -1, "", -1, -1, 14, 4000, 14
	plungeback = 1	
	end If
  End If
End If

If Gameover.state = 0 Then
  If Tilted.state = 0 Then
	If Plunger.position < 4.5 and plungeback = 1 then
	PlaySoundAt "plunger", plunger:shooter.duration 1, 50, 0:shooter001.duration 1, 50, 0
	BallInPlay
	plungeback = 0
	End if
  End If
End If

End Sub
'*************************************** END ANALOG PLUNGER *******************************************


' ********************* Start Table *************************************

Sub Table1_Init 
' NVramPatchLoad
	On Error Resume Next
		With Controller
			.GameName = "heavymtl"
			.Games("heavymtl").Settings.value("sound") = 0 
		    NVOffset (1)
			.GameName=cGameName
			If Err Then MsgBox"Can't start Game"&cGameName&vbNewLine&Err.Description:Exit Sub
			.SplashInfoLine="Heavy Metal (Rowamet 1983) 4.0" & vbNewLine & "VPX Table By Wiesshund" & vbNewLine & "Based on the concept of Destruct's VP9 table" &vbNewLine & "And the FuturePinball table by cujopb"
			.HandleMechanics=0
			.HandleKeyboard=0
			.ShowDMDOnly=1
			.ShowFrame=0
			.ShowTitle=0
			.Run
			.Hidden = HidePinMAME
			If Err Then MsgBox Err.Description
		End With
	On Error Goto 0
	
	vpmNudge.TiltSwitch=30
	vpmNudge.Sensitivity=5
	vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot,Bumper1,Bumper2,Bumper3,MiniSling2,MiniSling1,RightFlipper,LeftFlipper)

	Set bsTrough=New cvpmBallStack
	bsTrough.InitSw 0,1,0,0,0,0,0,0
	bsTrough.InitKick BallRelease,110,10
	bsTrough.InitExitSnd "Shoot","SolOn"
	bsTrough.Balls=1

	Set bsSaucer=New cvpmBallStack
	bsSaucer.InitSaucer Kicker1,2,220,20
	bsSaucer.InitExitSnd"Shoot", "SolOn"

	vpmMapLights ALights

dhit=0

	DisplaytimerLED.Enabled=1

If ApronDMDShow = 1 or FSS = 1 then
FlexDMDTimer.enabled=1
End If

FlexINIT()
plungeback = 0


If UserOptionsSet = 0 Then 
MsgBox"Hello, This table requires VPX 10.8.0 or higher"&vbNewLine&"BS2 Server 2.0.4 and VpinMAME 3.6 to run"
MsgBox"This table uses a VPM ROM Alias"&vbNewLine&"Make sure you added it to VPMAlias.txt See Readme"
MsgBox"PLAYER OPTIONS HAVE NOT BEEN SET"&vbNewLine&"YOU WILL NEED TO SET YOUR USER OPTIONS BEFORE PLAYING"
MsgBox"Please open the script and set your user options before playing"&vbNewLine&"The table will now Halt"&vbNewLine&"Quit to the editor and open the script"&vbNewLine&"Set your options"&vbNewLine&"They are at the very top of the script, you can not miss them"
MsgBox"Now, go and set your options"&vbNewLine&"Or you shall meet the sum of all evils"
Table1_Exit
End If

NewHigh.visible=0
HighScore.visible=0
Activity.visible=0
gameover.visible=0
tilted.visible=0
ingameDMDBack.visible = ApronDMDShow
ingameDMD.visible = ApronDMDShow
'Hiding LED digits


LoadLUT ' load saved LUT file
LoadBGV ' load backglass sound effects volume
LoadDMDCOLOR ' load DMD color
End Sub



' Table Exit - kill the DMD and stop the controller
Sub table1_Exit
SaveBGV
SaveLut
Controller.Games("heavymtl").Settings.Value("sound")= 1
NVramPatchExit
Controller.stop
'  If Not UltraDMD is Nothing Then
'   If UltraDMD.IsRendering Then
'      UltraDMD.CancelRendering
'   End If
'	UltraDMD.Uninit
'   UltraDMD = NULL
'   FlexDMD.Run = False
'  End If

End Sub

' =============================================================================================================
'                 NVram patch for Taito do Brasil tables by Pmax65
'
' NVramPatchExit	' Must be placed before the Controler.Stop statement into the Table1_Exit Sub
' NVramPatchLoad	' Must be placed before the VPinMAME controller initialization
' NVramPatchKeyCheck' Must be placed in the lamptimer timer
' =============================================================================================================

Const GameOverLampID = 149 ' set this constant to the ID number of the game-over lamp

Dim NVramPatchCoinCnt

' Function GetNVramPath()
'     Dim WshShell
'     Set WshShell = CreateObject("WScript.Shell")
'     GetNVramPath = WshShell.RegRead("HKCU\Software\Freeware\Visual PinMame\globals\nvram_directory")
' End function

Function FileExists(FileName)
    DIM FSO
    FileExists = False
    Set FSO = CreateObject("Scripting.FileSystemObject")
    FileExists = FSO.FileExists(FileName)
    Set FSO = Nothing
End Function

Sub Kill(FileName)
    Dim ObjFile, FSO
    On Error Resume Next
    Set FSO = CreateObject("Scripting.FileSystemObject")
    Set ObjFile = FSO.GetFile(FileName)
    ObjFile.Delete
    On Error Goto 0
    Set FSO = Nothing
End Sub

Sub Copy(SourceFileName, DestFileName)
    Dim FSO
    On Error Resume Next
    Set FSO = CreateObject("Scripting.FileSystemObject")
    FSO.CopyFile SourceFileName, DestFileName, True
    On Error Goto 0
    Set FSO = Nothing
End Sub

'Sub NVramPatchLoad
'    NVramPatchCoinCnt = 0
'    If FileExists(GetNVramPath + "\" + cGameName + ".nvb")Then
'        Copy GetNVramPath + "\" + cGameName + ".nvb", GetNVramPath + "\" + cGameName + ".nv"
'    Else
'        Copy GetNVramPath + "\" + cGameName + ".nv", GetNVramPath + "\" + cGameName + ".nvb"
'    End If
'End Sub

Sub NVramPatchExit
    If gameover.state = 1 Then
        Kill GetNVramPath + "\" + cGameName + ".nvb"
 '       Do
 '           LampTimer_Timer          ' This loop is needed to avoid the NVram reset (losing the hi-score and credits)
 '       Loop Until LampState(20) = 1 ' when the game is over but the match procedure isn't still ended
    End If
End Sub

' =============================================================================================================
' To completely erase the NVram file keep the Start Game button pushed while inserting
' two coins into the first coin slit (this resets the high scores too)
' =============================================================================================================

Sub NVramPatchKeyCheck
    If Controller.Switch(swStartButton)then
        If Controller.Switch(swCoin1)then
            If NVramPatchCoinCnt = 2 Then
                Controller.Stop
                Kill GetNVramPath + "\" + cGameName + ".nv"
                Kill GetNVramPath + "\" + cGameName + ".nvb"
                QuitPlayer 2
            Else
                NVramPatchCoinCnt = 1
            End If
        Else
            If NVramPatchCoinCnt = 1 Then
                NVramPatchCoinCnt = 2
            End If
        End If
    Else
        NVramPatchCoinCnt = 0
    End If
End Sub


' RED 255,33,3
' GREEN 31,219,5
' CYAN 61,213,255
' YELLOW 255,251,5
' ORANGE 255,89,8 
' PINK 255,35,204
' PURPLE 255,35,255


' ****************************************************************************************************************************************************
' BELOW HERE SHOULD CHANGE BASED ON TABLE MODES/OPTIONS.
' ****************************************************************************************************************************************************


'******************************* FLEXDMD INITIALIZING **********************************
Sub FlexINIT
    Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
	UseColoredDMD = true
    If FlexDMD is Nothing Then 
        MsgBox "No UltraDMD found.  This table will NOT run without it."
        Exit Sub 
    End If 
    FlexDMD.GameName = cGameName 
    FlexDMD.RenderMode = 2 
	FlexDMD.Show = ShowFlexDMD
	LoadDMDCOLOR

If DMDSize=0 Then	
	FlexDMD.Width = 128
	FlexDMD.Height = 32
End If

If DMDSize=1 Then	
	FlexDMD.Width = 256
	FlexDMD.Height = 64
End If

If DMDSize=2 Then
	FlexDMD.Width = 128
FlexDMD.Height = 71
End If

If DMDSize=3 Then
	FlexDMD.Width = 256
	FlexDMD.Height = 143
End If

    Set UltraDMD = FlexDMD.NewUltraDMD() 
    UltraDMD.Init
    
    If Not UltraDMD.GetMajorVersion = 1 Then
        MsgBox "Incompatible Version of UltraDMD found."
        Exit Sub
    End If

    Set fso = CreateObject("Scripting.FileSystemObject")
    curDir = fso.GetAbsolutePathName(".")
    UltraDMD.SetProjectFolder curDir & "\HvyMtl.DMD"

UltraDMD.DisplayScene00Ex "background.wmv", "Heavy Metal", 30, -1, "Rowamet 1983", 30, -1, 14, 9000, 14

End Sub

Sub FlexDMDTimer_Timer()
	Dim DMDp
	If UseDMD Then
		DMDp = FlexDMD.DmdPixels
		If Not IsEmpty(DMDp) Then
			DMDWidth = FlexDMD.Width
			DMDHeight = FlexDMD.Height
			DMDPixels = DMDp
		End If
	ElseIf UseColoredDMD Then
		DMDp = FlexDMD.DmdColoredPixels
		If Not IsEmpty(DMDp) Then
			DMDWidth = FlexDMD.Width
			DMDHeight = FlexDMD.Height
			DMDColoredPixels = DMDp
		End If
	End If
	
End Sub

'******************************* END FLEXDMD INITIALIZING **********************************






' ********** Jukebox Changer ***************
Sub Jukebox_Hit()
dhit=0
scoreclick=0
PLAYSOUND "BallRelease", 0, BgVolume


If Xtra = 1 then PlayMusic "HeavyMetal\XtraBall.mp3":Xtra = 0:Exit Sub
	

    If musicNum = 0  then
Select Case Int(Rnd*3)+1
		Case 1 : PlayMusic "HeavyMetal\HM-Heavy-Metal.mp3"
		Case 2 : PlayMusic "HeavyMetal\HM-Take-Ride.mp3"
		Case 3 : PlayMusic "HeavyMetal\HM-Coalmine.mp3"
	End Select
  End If
	If musicNum = 1  then 
Select Case Int(Rnd*3)+1
		Case 1 : PlayMusic "HeavyMetal\HM-Psychic-Wars.mp3"
		Case 2 : PlayMusic "HeavyMetal\HM-Prefab.mp3"
		Case 3 : PlayMusic "HeavyMetal\HM-Cool.mp3"
	End Select
End If
    If musicNum = 2  then
Select Case Int(Rnd*3)+1
		Case 1 : PlayMusic "HeavyMetal\HM-Reach-Out.mp3"
		Case 2 : PlayMusic "HeavyMetal\HM-Dreamin.mp3"
		Case 3 : PlayMusic "HeavyMetal\HM-All-You.mp3"
	End Select
End If
    If musicNum = 3  then
Select Case Int(Rnd*3)+1
		Case 1 : PlayMusic "HeavyMetal\HM-Mob-Rules.mp3"
		Case 2 : PlayMusic "HeavyMetal\HM-Crazy.mp3"
		Case 3 : PlayMusic "HeavyMetal\HM-Queenbee.mp3"
	End Select
End If
    If musicNum = 4  then
Select Case Int(Rnd*3)+1
		Case 1 : PlayMusic "HeavyMetal\HM-Radar-Rider.mp3"
		Case 2 : PlayMusic "HeavyMetal\HM-Blue-Lamp.mp3"
		Case 3 : PlayMusic "HeavyMetal\HM-Heartbeat.mp3"
	End Select
End If

musicNum = (musicNum + 1) mod 5
End Sub
' ********** END Jukebox Changer ***************


'**************************************** BALLRELEASE ************************************
sub ballrelease_unhit 




if  SameShot=1 then
			UltraDMD.CancelRendering
			UltraDMD.DisplayScene00Ex "background.wmv", "Player " & Cstr(player+1) & " Is Back!", 30, -1, "GO!", 30, -1, 14, 10000, 1
			UltraDMD.DisplayScene00Ex "background.wmv", "", 30, -1, "", -1, -1, 14, 10000, 1
Xtra=1:sameshot=0:exit sub
End If

if players=0 then players=1
player = (player + 1) mod (players)


If player=0 then 
HvyMtl1.duration 1, 50, 0
end If

If player=1 then 
HvyMtl2.duration 1, 50, 0
end if

If Player=2 then 
HvyMtl3.duration 1, 50, 0
end if

If Player=3 then 
HvyMtl4.duration 1, 50, 0
end if

If Player=4 then 
HvyMtl0.duration 1, 50, 0
end if


			UltraDMD.CancelRendering
			UltraDMD.DisplayScene00Ex "background.wmv", "Player " & Cstr(player+1) & " Up", 30, -1, "DEFEND! ", 30, -1, 14, 10000, 1
			UltraDMD.DisplayScene00Ex "background.wmv", "", 30, -1, "", -1, -1, 14, 10000, 1


end Sub

'**************************************************** END BALLRELEASE ************************************************





' ***** launch1, simply a trigger in the launch lane that triggers a sound as the ball passes, not part of the ROM
Sub TriggerS_Hit() 'This was the VP9 table's ball roll sound timer
Select Case Int(Rnd*2)+1
		Case 1 : PlaySound "launch1", 0, BgVolume
		Case 2 : PlaySound "shooter", 0, BgVolume
	End Select
DOF 112, 2 ' shaker pulse
End Sub
'******************** END LAUNCH 1 **************************************************


'**************************** TIME TUNNEL FLASH ******************************************
sub tunnel1_hit
tunnel1L.duration 1, 50, 0
end Sub

sub tunnel2_hit
tunnel2L.duration 1, 50, 0
end Sub
'***************************************** END TIME TUNNEL *******************************
	


dim dhit 'to prevent target resets from killing off drain video
dim scoreclick
'********************************* DRAIN **************************************
Sub Drain_Hit 'Switch 1
dhit=1
DisplayTimerLED.enabled=0
PlaysoundAt "Drain", Drain
drainL.duration 1, 60, 0
drainL1.duration 1, 120, 0
EndMusic 


'BallDMD

if Light21.state=1 then 
SameShot=1
changeball 1
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "sameplayer.wmv", "Same Player", 30, -1, "Shoots", 30, -1, 14, 10000, 14
vpmTimer.addTimer 10000, "bsTrough.addball Drain:scoreclick=1 '":vpmtimer.addtimer 1, "DisplayTimerLED.enabled = 1 '"
Exit Sub
End If



If BallCount= 4 then 
changeball 3 
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "challenge.wmv", "", 15, "", -1, 14, 31939, 14
UltraDMD.DisplayScene00EX "background.wmv", "Player " & Cstr(player)+1 & " Ball " & Cstr(BallCount), 30, -1, "DIE!", 30, -1, 14, 1, 14
DOF 112, 1 ' Shaker
vpmTimer.addTimer 31939, "bsTrough.addball Drain:scoreclick=1 '":vpmtimer.addtimer 1, "DisplayTimerLED.enabled = 1 '"
DOF 123, 0 ' turn off blue undercab
DOF 122, 1 ' change undercab to green
End If

If BallCount= 3 then 
changeball 2 
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "avenge.wmv", "", 15, "", -1, 14, 15000, 14
UltraDMD.DisplayScene00EX "background.wmv", "Player " & Cstr(player)+1 & " Ball " & Cstr(BallCount), 30, -1, "AVENGE!", 30, -1, 14, 1, 14
vpmTimer.addTimer 15000, "bsTrough.addball Drain:scoreclick=1 '":vpmtimer.addtimer 1, "DisplayTimerLED.enabled = 1 '"
DOF 120, 0 ' turn off red undercab
DOF 123, 1 ' change undercab to blue
End If



If BallCount < 3 then
changeball 0
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "shootagain.wmv", "", 15, "", -1, 14, 7000, 14
vpmTimer.addTimer 9000, "bsTrough.addball Drain:scoreclick=1 '":vpmtimer.addtimer 1, "DisplayTimerLED.enabled = 1 '"
End If



If BallCount = 5 and Light21.state=0 then 
changeball 0
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "gameover.wmv", "Player " & Cstr(player)+1 & " game over", 15, " ", -1, 14, 15882, 14
vpmTimer.addTimer 15882, "bsTrough.addball Drain '":Playsound SoundFXDOF("knocker", 111, 2, DOFKnocker):scoreclick=1:vpmtimer.addtimer 1, "DisplayTimerLED.enabled = 1 '":vpmtimer.addtimer 1, "match.enabled = 1 '"
'7941 
dhit=0
DOF 120, 2 ' under cab red off
DOF 123, 2 ' undercab blue off
DOF 122, 1 ' undercab green on for game over theme
End If


DOF 120, 2 ' under cab red off
DOF 123, 2 ' undercab blue off
DOF 122, 1 ' undercab green on for game over theme


End Sub

'************************* END DRAIN ********************************************


'*************************** BALL LOCK ****************************************

Sub Kicker1_Hit





	If light5.state=1 Then
		PlaySoundAt "kicker_enter_center", Kicker1:DOF 117,1
		kickerL.duration 1, 30, 0
		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "BonusLock.wmv", "Player " & Cstr(player+1) & " Mob Rules", 30, -1, "BONUS!", 30, -1, 14, 16117, 14
		vpmTimer.addTimer 9000, "bsSaucer.addball 0 '":vpmTimer.addTimer 9500, "kickerE.duration 1, 30, 0 '"
		Exit Sub

	ElseIf light4.state=1 Then
		PlaySoundAt "kicker_enter_center", Kicker1:DOF 117,1
		kickerL.duration 1, 30, 0
		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "Xtraball.wmv", "Player " & Cstr(player+1) & " EXTRA BALL!", 30, -1, "CLAIMED", 30, -1, 14, 9000, 14
		vpmTimer.addTimer 6000, "bsSaucer.addball 0 '":vpmTimer.addTimer 6500, "kickerE.duration 1, 30, 0 '"
		Exit Sub

	ElseIF Light1.state = 0 and Light2.state = 0 then  ' Needed when you claim all the ball lock bonuses and reset it
		PlaySoundAt "kicker_enter_center", Kicker1
		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "challenge.wmv", "Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, "Ball Locked", 30, -1, 14, 7000, 1
		ChangeActive 3
		vpmTimer.addTimer 6000, "bsSaucer.addball 0 '":vpmTimer.addTimer 6500, "kickerE.duration 1, 30, 0 '"
		Exit Sub

	ElseIf Light1.state = 1 and Light2.state = 0 then
		PlaySoundAt "kicker_enter_center", Kicker1
		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "landing.wmv", "Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, "Yellow Alert", 30, -1, 14, 7000, 1
		ChangeActive 3
		vpmTimer.addTimer 6000, "bsSaucer.addball 0 '":vpmTimer.addTimer 6500, "kickerE.duration 1, 30, 0 '"
		Exit Sub

	ElseIf Light2.state = 1 and light3.state = 0 then
		PlaySoundAt "kicker_enter_center", Kicker1
		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "landing2.wmv", "Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, "Green Alert", 30, -1, 14, 7000, 1
		ChangeActive 2
		vpmTimer.addTimer 6000, "bsSaucer.addball 0 '":vpmTimer.addTimer 6500, "kickerE.duration 1, 30, 0 '"
		Exit Sub

	ElseIf light3.state = 1 then
		PlaySoundAt "kicker_enter_center", Kicker1
		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "greenalert.wmv", "Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, "Blue Alert", 30, -1, 14, 10000, 1
		ChangeActive 1
	vpmTimer.addTimer 6000, "bsSaucer.addball 0 '":vpmTimer.addTimer 6500, "kickerE.duration 1, 30, 0 '"
		Exit Sub

	End If


End Sub


'********************** END BALL LOCK ******************************************


'*********************** GATES ***********************************
Sub Gate1_Hit
PlaySoundAt "gate", Gate1
End Sub

Sub Gate2_Hit  'Switch 2
PlaySoundAt "gate", Gate2
lgateL.duration 1, 30, 0
End Sub

Sub Gate3_Hit  'Switch 2
PlaySoundAt "gate", Gate3
rgateL.duration 1, 30, 0
End Sub

Sub Gate001_Hit
PlaySoundAt "gate", Gate001
End Sub

'**************************** END GATES ****************************
																


'************************ BUMPERS with Stobes ************************
Sub Bumper1_Hit
vpmTimer.PulseSw 13 'Switch 13
PlaySoundAt SoundFXDOF("fx_bumper1",105, 2 , DOFContactors), Bumper1
'DOF 105, 2 ' Pulse left rear bumper
bump1.duration 1, 50, 0 
bump001.duration 1, 55, 0 
DOF 114, 2 ' pulse red flasher


		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1), 30, -1, " You Heavy", 30, -1, 14, 1600, 14


End Sub			


Sub Bumper2_Hit
vpmTimer.PulseSw 3 'Switch 3
PlaySoundAt SoundFXDOF("fx_bumper2",107, 2, DOFContactors), Bumper2
'DOF 107, 2 ' Pulse right rear bumper
bump2.duration 1, 50, 0 
bump002.duration 1, 55, 0 
DOF 115, 2 ' pulse green flasher


		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1), 30, -1, " You Metal", 30, -1, 14, 1600, 14


End Sub												

Sub Bumper3_Hit
vpmTimer.PulseSw 23 'Switch 23
PlaySoundAt SoundFXDOF("fx_bumper3",106, 2, DOFContactors), Bumper3
'DOF 106, 2 ' Pulse center rear bumper
bump3.duration 1, 50, 0 
bump003.duration 1, 55, 0 
DOF 116, 2 ' pulse blue flasher


		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1), 30, -1, " You Rock", 30, -1, 14, 1600, 14


End Sub												
'************************* END BUMPERS **********************************



' ***************  Launch Lights Tube ****************************
Sub strobe001_hit
strobeL001.duration 1, 60, 0 
strobeL012.duration 1, 50, 0 
DOF 112, 2 ' shaker pulse
End Sub

Sub strobe002_hit
strobeL002.duration 1, 60, 0 
strobeL013.duration 1, 50, 0 
DOF 112, 2 ' shaker pulse
End Sub


Sub strobe003_hit
strobeL003.duration 1, 60, 0 
strobeL014.duration 1, 50, 0 
DOF 112, 2 ' shaker pulse
End Sub


Sub strobe004_hit
strobeL004.duration 1, 60, 0 
strobeL015.duration 1, 50, 0 
DOF 112, 2 ' shaker pulse
End Sub

Sub strobe005_hit
strobeL005.duration 1, 60, 0
strobeL016.duration 1, 50, 0  
DOF 112, 2 ' shaker pulse
End Sub

Sub strobe006_hit
strobeL006.duration 1, 60, 0 
strobeL017.duration 1, 50, 0 
DOF 112, 2 ' shaker pulse
End Sub

Sub strobe007_hit
strobeL007.duration 1, 60, 0
strobeL018.duration 1, 50, 0 
DOF 112, 2 ' shaker pulse 
End Sub

Sub strobe008_hit
strobeL008.duration 1, 60, 0 
strobeL019.duration 1, 50, 0 
DOF 112, 2 ' shaker pulse
End Sub

Sub strobe009_hit
strobeL009.duration 1, 60, 0 
strobeL020.duration 1, 50, 0 
DOF 112, 2 ' shaker pulse
End Sub

Sub strobe010_hit
strobeL010.duration 1, 60, 0
strobeL021.duration 1, 50, 0  
DOF 112, 2 ' shaker pulse
End Sub

Sub strobe011_hit
strobeL011.duration 1, 60, 0 
strobeL022.duration 1, 50, 0 
DOF 112, 2 ' shaker pulse
End Sub
' *************** End Launch Lights Tube ****************************




'******************************* OUT LANE TRIGGERS ************************
Sub M10_Hit ' right outlane, light22 'Switch 24
PlaySoundAt "switch1", M10
Controller.Switch(24)=1

ROutlane

End Sub																

Sub M10_unHit
Controller.Switch(24)=0
End Sub


Sub M7_Hit 'left outlane light23 'Switch 34
PlaySoundAt "switch1", M7
Controller.Switch(34)=1

LOutlane

End Sub

Sub M7_unHit
Controller.Switch(34)=0
End Sub

' ******************************** END OUT LANE TRIGGERS *********************


' **************************** INLANE TRIGGERS ****************************
Sub M8_Hit 'Switch 4, lamp 24
PlaySoundAt "switch1", M8
Controller.Switch(4)=1

LInLane

End Sub	
															
Sub M8_unHit:Controller.Switch(4)=0:End Sub

Sub M9_Hit 'Switch 14 lamp 25
PlaySoundAt "switch1", M9
Controller.Switch(14)=1

RInLane 

End Sub										

Sub M9_unHit:Controller.Switch(14)=0:End Sub
'********************** END INLANE TRIGGERS ***********************************


' ************************* DROP TARGETS ******************************


'************************* LEFT TARGET BANK *****************************
Sub LB1_Dropped 'Switch 75
Controller.Switch(75)=1
Controller.Switch(77)=1
PlaySoundAt SoundFXDOF("Target",108, 2,  DOFDropTargets), LB1
'DOF 108, 2 ' Pulse left bumper


UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "General.png", "Barbarian General", 30, -1, "Killed", 30, -1, 14, 1600, 14
				

End Sub	

Sub LB2_Dropped 'Switch 65
Controller.Switch(65)=1
Controller.Switch(77)=1
PlaySoundAt SoundFXDOF("Target",108, 2,  DOFDropTargets), LB2
'DOF 108, 2 ' Pulse left bumper
		

UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "Zombie.png", "Zombie Pilot", 30, -1, "Eliminated", 30, -1, 14, 1600, 14
			

End Sub	

Sub LB3_Dropped 'Switch 55
Controller.Switch(55)=1
Controller.Switch(77)=1
PlaySoundAt SoundFXDOF("Target",108, 2,  DOFDropTargets), LB3
'DOF 108, 2 ' Pulse left bumper
		

UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "General.png", "Barbarian General", 30, -1, "Killed", 30, -1, 14, 1600, 14

End Sub	

Sub LB4_Dropped 'Switch 45
Controller.Switch(45)=1
Controller.Switch(77)=1
PlaySoundAt SoundFXDOF("Target",108, 2,  DOFDropTargets), LB4
'DOF 108, 2 ' Pulse left bumper
		
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "Zombie.png", "Zombie Pilot", 30, -1, "Eliminated", 30, -1, 14, 1600, 14

End Sub	

Sub LB5_Dropped 'Switch 35
Controller.Switch(35)=1
Controller.Switch(77)=1
PlaySoundAt SoundFXDOF("Target",108, 2,  DOFDropTargets), LB5
'DOF 108, 2 ' Pulse left bumper
		
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "General.png", "Barbarian General", 30, -1, "Killed", 30, -1, 14, 1600, 14

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
		PlaySoundAt SoundFXDOF("FlapOpen",124, 2, DOFContactors), LS3
'DOF 108, 2 'pulse left front bumper Sol

Select Case Int(Rnd*7)+1
		Case 1: call BankReset1
		Case 2: call BankReset2
		Case 3: call BankReset3
		Case 4: call BankReset4
		Case 5: call BankReset5
		Case 6: call BankReset6
		Case 7: call BankReset7
	End Select
	End If
End Sub

' LEFT BANK BACKING Targets

Sub Alvo5_Hit:vpmTimer.PulseSw 43
PlaySound "Alvo", 0, BgVolume
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1), 30, -1, "Kill All", 30, -1, 14, 1600, 14
End Sub	
															'Switch 43
Sub Alvo4_Hit:vpmTimer.PulseSw 43
PlaySound "Alvo", 0, BgVolume
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1), 30, -1, "Try for Bonus", 30, -1, 14, 1600, 14
End Sub

Sub Alvo3_Hit:vpmTimer.PulseSw 43
PlaySound "Alvo", 0, BgVolume
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1), 30, -1, "Shoot the Arrow", 30, -1, 14, 1600, 14
End Sub	

Sub Alvo2_Hit:vpmTimer.PulseSw 43
PlaySound "Alvo", 0, BgVolume
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1), 30, -1, "Engage Targets", 30, -1, 14, 1600, 14
End Sub	

Sub Alvo1_Hit:vpmTimer.PulseSw 43
PlaySound "Alvo", 0, BgVolume
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1), 30, -1, "You Missed", 30, -1, 14, 1600, 14
End Sub	



'*********************** CENTER TARGET BANK **********************


Sub C1_Dropped	'Switch 41
Controller.Switch(41)=1
Controller.Switch(53)=1
PlaySoundAt SoundFXDOF("Target",109, 2,  DOFDropTargets), C1
'DOF 109, 2 ' Pulse center bumper
		
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "Zombie.png", "Zombie Pilot", 30, -1, " Eliminated", 30, -1, 14, 1600, 14

End Sub

Sub C2_Dropped	'Switch 51
Controller.Switch(51)=1
Controller.Switch(53)=1
PlaySoundAt SoundFXDOF("Target",109, 2,  DOFDropTargets), C2
'DOF 109, 2 ' Pulse center bumper
		
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "General.png", "Barbarian General", 30, -1, "Killed", 30, -1, 14, 1600, 14

End Sub

Sub C3_Dropped'Switch 61
Controller.Switch(61)=1
Controller.Switch(53)=1
PlaySoundAt SoundFXDOF("Target",109, 2,  DOFDropTargets), C3
'DOF 109, 2 ' Pulse center bumper
		
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "Nyborg.png", "Hey man, Good", 30, -1, "NYBORG" & Cstr(player+1), 30, -1, 14, 1600, 14

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
		PlaySoundAt SoundFXDOF("FlapOpen",125, 2, DOFContactors), CS2
'DOF 109, 2 'pulse center front bumper Sol
Select Case Int(Rnd*7)+1
		Case 1: call BankReset1
		Case 2: call BankReset2
		Case 3: call BankReset3
		Case 4: call BankReset4
		Case 5: call BankReset5
		Case 6: call BankReset6
		Case 7: call BankReset7
	End Select
	End If
End Sub

'******************************* TOP TARGET BANK ***************************************
Sub T1_Dropped 'Switch 11
Controller.Switch(11)=1 
Controller.Switch(33)=1
PlaySoundAt SoundFXDOF("Target",110, 2,  DOFDropTargets), T1
'DOF 110, 2 ' Pulse right bumper
		
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "Tarna.png", "Player " & Cstr(player+1), 30, -1, "Taarna Claimed", 30, -1, 14, 1600, 14

End Sub	

Sub T2_Dropped 'Switch 21
Controller.Switch(21)=1
Controller.Switch(33)=1
PlaySoundAt SoundFXDOF("Target",110, 2,  DOFDropTargets), T2
'DOF 110, 2 ' Pulse right bumper
		
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "Tarna.png", "Player " & Cstr(player+1), 30, -1, "Taarna Claimed", 30, -1, 14, 1600, 14

End Sub	

Sub T3_Dropped 'Switch 31
Controller.Switch(31)=1
Controller.Switch(33)=1
PlaySoundAt SoundFXDOF("Target",110, 2,  DOFDropTargets), T3
'DOF 110, 2 ' Pulse right bumper
		
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "Tarna.png", "Player " & Cstr(player+1), 30, -1, "Taarna Claimed", 30, -1, 14, 1600, 14

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
		PlaySoundAt SoundFXDOF("FlapOpen",126, 2, DOFContactors), TS2
'DOF 107, 2 'pulse rear right bumper Sol, cause it is closest to targets physically
Select Case Int(Rnd*7)+1
		Case 1: call BankReset1
		Case 2: call BankReset2
		Case 3: call BankReset3
		Case 4: call BankReset4
		Case 5: call BankReset5
		Case 6: call BankReset6
		Case 7: call BankReset7
	End Select
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

Select Case Int(Rnd*5)+1
		Case 1: changeball 1:DOF 114, 2
		Case 2: changeball 2:DOF 115, 2
		Case 3: changeball 3:DOF 116, 2
		Case 4: changeball 4:DOF 114, 2
		Case 5: changeball 5:DOF 115, 2
	End Select

PlaySoundAt "spinnerclicking", M4
PlaySound "Spinner2", 0, BgVolume

eye1.duration 1, 30, 0
eye2.duration 1, 30, 0
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 14



End Sub	



Sub M5_Spin 'Switch 22
vpmTimer.PulseSw 22 
PlaySoundAt "spinnerclicking", M5
PlaySound "Spinner1", 0, BgVolume

M5L1.duration 1, 10, 0
M5L2.duration 1, 50, 0
M5L3.duration 1, 90, 0
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 14	
End Sub		

Sub M6_Spin	'Switch 32
vpmTimer.PulseSw 32
PlaySoundAt "spinnerclicking", M6
PlaySound "Spinner", 0, BgVolume
tunnel1L.duration 1, 50, 0
tunnel2L.duration 1, 50, 0
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 1
UltraDMD.DisplayScene00Ex "score.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 300, 14	
End Sub

' *********************** End Spinners ******************************************


' *************** TOP LANES ********************************

Sub M1_Hit
PlaySoundAt "switch1", M1 
Controller.Switch(42)=1
DOF 119, 2
TopLane1
End Sub																'Switch 42 'Light 37

Sub M1_unHit:Controller.Switch(42)=0:End Sub


Sub M2_Hit
PlaySoundAt "switch1", M2
Controller.Switch(52)=1
DOF 119, 2
TopLane2
End Sub																'Switch 52 'Light 38

Sub M2_unHit:Controller.Switch(52)=0:End Sub


Sub M3_Hit
PlaySoundAt "switch1", M3
Controller.Switch(62)=1
DOF 119, 2
TopLane3
End Sub	
															'Switch 62 'Light 39
Sub M3_unHit:Controller.Switch(62)=0:End Sub

' **************** end Lanes *************************************



'*************** SLINGSHOTS ************************************************************************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot 'Switch 54
	vpmTimer.PulseSw 54
	kick2.duration 1, 50, 0 
		PlaySoundAt SoundFXDOF("right_slingshot",104,2 ,DOFContactors), rslingaud
		'DOF 104, 2 ' pulse right slingshot
		UltraDMD.CancelRendering()	
		UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1), 30, -1, "Boom!", 30, -1, 14, 1600, 14

    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1

End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 44 'Switch 44
	kick1.duration 1, 50, 0 
    PlaySoundAt SoundFXDOF("left_slingshot",103, 2, DOFContactors), lslingaud
		'DOF 103, 2 ' pulse left slingshot

		UltraDMD.CancelRendering()	
		UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1), 30, -1, "Boom!", 30, -1, 14, 1600, 14

    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub
'************************* End Slingshots **************************************************************


Sub MiniSling2_Hit:vpmTimer.PulseSw 64 'Switch 64 need documentation

PlaySoundAt SoundFXDOF("right_slingshot",127,2 ,DOFContactors), minisling2A
'PlaySoundAt SoundFX("Targets", DOFTargets), minisling2A
PlaySound "Multiplier2", 0, BgVolume
'DOF 105, 2 ' pulse back left bumper sol
DOF 116, 2 'pulse blue flasher
		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "score.wmv","Multiplier Advanced", 30, -1,  "Score", 30, -1, 14, 1600, 14
End Sub																


Dim MStep

Sub MiniSling1_Slingshot 'Switch 71 need documentation
	vpmTimer.PulseSw 71 
PlaySoundAt SoundFXDOF("right_slingshot",128,2 ,DOFContactors), minisling12A
'DOF 107, 2 ' pulse back right bumper sol
DOF 114, 2 ' pulse red flasher
minislingl1.duration 1, 40, 0
minislingl2.duration 1, 60, 0

		UltraDMD.CancelRendering()	
		UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1), 30, -1, "Bonus!", 30, -1, 14, 1600, 14

    MSling.Visible = 0
    MSling1.Visible = 1
    MiniSling3.rotx = 20
    MStep = 0
    MiniSling1.TimerEnabled = 1

End Sub

Sub MiniSling1_Timer
    Select Case MStep
        Case 3:MSLing1.Visible = 0:MSLing2.Visible = 1:MiniSling3.rotx = 10
        Case 4:MSLing2.Visible = 0:MSLing.Visible = 1:MiniSling3.rotx = 0:MiniSling1.TimerEnabled = 0
    End Select
    MStep = MStep + 1
End Sub									




' upper left pocket target 1000 points
Sub M11_Hit 'Switch 72
vpmTimer.PulseSw 72
PlaySoundAt "Targets", M11S
PlaySound "multiplier", 0, BgVolume

		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1), 30, -1, "Bonus 1000", 30, -1, 14, 1600, 14

End Sub																	



																										

'**************************************************************************
'****************************SUPPORT ROUTINES******************************
'**************************************************************************
'********************************************************************
'***********************************
'   JP's VP10 Rolling Sounds v4.0
'   JP's Ball Shadows
'   JP's Ball Speed Control
'   Rothbauer's dropping sounds
'***********************************

Const tnob = 5   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 50 'max ball velocity
Const anglecompensate = 15
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer() 'call this routine from any realtime timer you may have, running at an interval of 10 is good.

    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

' glowing ball check
If GlowBall Then
		For b = 0 to 4
			If GlowBall and Glowing(b).state = 1 Then Glowing(b).state = 0 End If
		Next
End If
' End Glow check


    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)

    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' draw the ball shadow
    For b = lob to UBound(BOT)


' glowing balls
          If GlowBall and b <8 Then
			If Glowing(b).state = 0 Then Glowing(b).state = 1 
			end if
			
		  If BOT(b).radius > 23 then
				Glowing(b).BulbHaloHeight = BOT(b).z + 50
				Glowing(b).x = BOT(b).x : Glowing(b).y = BOT(b).y + anglecompensate
			Else
				Glowing(b).BulbHaloHeight = BOT(b).z - 252
				Glowing(b).x = 8000 : Glowing(b).y = 8000
			end if
'End Glowing Ball

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




'***********************************************************
'********************END SUPPORT ROUTINES*******************
'***********************************************************


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************************
' materials Hit Sounds (New)
'******************************

Sub BallGuides_Hit(idx)
Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtBall "Metal Hit 1"
		Case 2 : PlaySoundAtBall "Metal Hit 2"
		Case 3 : PlaySoundAtBall "Metal Hit 3"
		Case 4 : PlaySoundAtBall "Metal Hit 4"
	End Select
DOF 112, 2
End Sub

Sub RubberBands_Hit(idx)
PlaySoundAtBall "rubberband"
End Sub

Sub MetalPins_Hit(idx)
PlaySoundAtBall "hardmetal"
End Sub

Sub RubberPins_Hit(idx)
Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtBall "smrubber"
		Case 2 : PlaySoundAtBall "rubber_hit_1"
		Case 3 : PlaySoundAtBall "rubber_hit_2"
		Case 4 : PlaySoundAtBall "rubber_hit_3"
	End Select
End Sub

Sub Plastics_Hit(idx)
PlaySoundAtBall "plastique"
End Sub

Sub Metals_Hit(idx)
Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtBall "Metal Hit 1"
		Case 2 : PlaySoundAtBall "Metal Hit 2"
		Case 3 : PlaySoundAtBall "Metal Hit 3"
		Case 4 : PlaySoundAtBall "Metal Hit 4"
	End Select
End Sub

Sub Rubbers_Hit(idx)
PlaySoundAtBall "lrgrubber"
End Sub

Sub Target_Hit(idx)
PlaySoundAtBall "Targets"
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "flippers", 0, parm / 60, Pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
DOF 112, 2
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "flippers", 0, parm / 60, Pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
DOF 112, 2
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
dim nhclock
Sub newhighclock_timer()
If nhclock = 0 then
If gameover.state = 0 Then
If NewHigh.State = 1 Then:nhclock = (nhclock+1):UltraDMD.CancelRendering():UltraDMD.DisplayScene00Ex "New Score.png", "Bitchin!", 30, -1, " ", 30, -1, 14, 4000, 1:Me.Enabled = 0
End If
End If
End Sub

Dim IsTilt

' **** idle check, if game in play and nothing to do on DMD, returns ball in play count, as well as some non rom maintenance checks
Sub GILight_Timer()
Dim x



If gameover.state = 0 Then
        HvyMtl.state = 1
		PFShadow.visible = 1
		PFgameover.visible = 0
		plasticsgameover.visible = 0
		GMS = 0
		DOF 120, 1
		DOF 122, 0
	For Each x In Pflights:x.state=1
	Next

		Else

		HvyMtl.state = 0
		PFShadow.visible = 0
		PFgameover.visible = 1
		plasticsgameover.visible = 1
		musicNum = 0 ' resets jukebox
        nhclock=0
		newhighclock.Enabled = 1
		DOF 120, 0
		DOF 122, 1
	For Each x In Pflights:x.state=0:Next
		NVramPatchKeyCheck

End If

If Light20.state = 1 Then
eye1.visible = 1
eye2.visible = 1
Else
eye1.visible = 0
eye2.visible = 0
End If

If tilted.state = 1 then
	If IsTilt=0 then
		IsTilt = 1
		EndMusic
		PlaySound "tilt", 0, BgVolume
		UltraDMD.CancelRendering()
		UltraDMD.DisplayScene00Ex "lostball.wmv", "On No!", 30, -1, "TILT", 30, -1, 14, 6000, 1
	End If
Else
IsTilt=0
End If


' new indicators, lights trigger the flashers
If FSS = 1 then
display_newrecord.visible = NewHigh.state
display_tilt.visible = tilted.state
display_highscore.visible = HighScore.state
display_gameover.visible = gameover.state
display_ExtraBall.visible =  ExtraBall.State
display_SamePlayer.visible = Light21.state
End If

If DesktopBackBox = 1 Then
display_newrecordDT.visible = NewHigh.state
display_tiltDT.visible = tilted.state
display_highscoreDT.visible = HighScore.state
display_gameoverDT.visible = gameover.state
display_ExtraBallDT.visible =  ExtraBall.State
display_SamePlayerDT.visible = Light21.state
End If


		
		

End Sub




' declaring LED digits for player scores, need global for match counter
Dim P1D1,P1D2,P1D3,P1D4,P1D5,P1D6
Dim P2D1,P2D2,P2D3,P2D4,P2D5,P2D6
Dim P3D1,P3D2,P3D3,P3D4,P3D5,P3D6
Dim P4D1,P4D2,P4D3,P4D4,P4D5,P4D6
dim match1, match2, match3, match4, match5, match6, match7, match8 
Dim CRED
DIM score1 
DIM score2 
DIM score3 
DIM score4 
DIM PlayerUp

Sub ballreset_Timer() 

Me.Enabled=0

End Sub


'**** Attract Mode ***** 
Sub attractmode_Timer()
	If gameover.state = 1 Then
		If Not UltraDMD.IsRendering Then
				HMintro
	End If
End If
End Sub


'******************************************************************************************************************************************
'***************************************************** END OF TIMERS **********************************************************************
'******************************************************************************************************************************************

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
		ChgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if num < 26 then
				For Each obj In DigitsLED(num)
					If chg And 1 Then obj.State = stat And 1
					chg = chg\2 : stat = stat\2
						If dhit=1 and scoreclick=1 then playsound "score"
				Next
			end If
		Next
	End If

If gameover.state=0 then 
	If Not IsEmpty(ChgLED) Then

'PlayerUp for SCOREBOARD
If Player=0 then PlayerUp=1
If Player=1 then PlayerUp=2
If Player=2 then PlayerUp=3
If Player=3 then PlayerUp=4

'Send score to strings scorboard function can use as it can not automate
score1=score(1)
score2=score(2)
score3=score(3)
score4=score(4)

' Ball Count

If BALLLED1.state=0 and BALLLED2.state=1 and BALLLED3.state=1 and BALLLED4.state = 0 and BALLLED5.state=0 and BALLLED6.state=0 and BALLLED7.state=0 then ballcount = 1
If BALLLED1.state=1 and BALLLED2.state=1 and BALLLED3.state=0 and BALLLED4.state = 1 and BALLLED5.state=1 and BALLLED6.state=0 and BALLLED7.state=1 then ballcount = 2
If BALLLED1.state=1 and BALLLED2.state=1 and BALLLED3.state=1 and BALLLED4.state = 1 and BALLLED5.state=0 and BALLLED6.state=0 and BALLLED7.state=1 then ballcount = 3
If BALLLED1.state=0 and BALLLED2.state=1 and BALLLED3.state=1 and BALLLED4.state = 0 and BALLLED5.state=0 and BALLLED6.state=1 and BALLLED7.state=1 then ballcount = 4
If BALLLED1.state=1 and BALLLED2.state=0 and BALLLED3.state=1 and BALLLED4.state = 1 and BALLLED5.state=0 and BALLLED6.state=1 and BALLLED7.state=1 then ballcount = 5


'Attempt to tally Players in the game
If P1D1LED1.state=1 or P1D1LED2.state=1 or P1D1LED3.state=1 or P1D1LED4.state=1 or P1D1LED5.state=1 or P1D1LED6.state=1 or P1D1LED7.state=1 then Players = 1 
If P2D1LED1.state=1 or P2D1LED2.state=1 or P2D1LED3.state=1 or P2D1LED4.state=1 or P2D1LED5.state=1 or P2D1LED6.state=1 or P2D1LED7.state=1 then Players = 2
If P3D1LED1.state=1 or P3D1LED2.state=1 or P3D1LED3.state=1 or P3D1LED4.state=1 or P3D1LED5.state=1 or P3D1LED6.state=1 or P3D1LED7.state=1 then Players = 3
If P4D1LED1.state=1 or P4D1LED2.state=1 or P4D1LED3.state=1 or P4D1LED4.state=1 or P4D1LED5.state=1 or P4D1LED6.state=1 or P4D1LED7.state=1 then Players = 4




playcount.text = (Players)

'*********** build score array here *******************


' If sting of digits makeing number then P1D1 = X * 100000, Have to factor for 10 numbers

'********************* PLAYER 1 SCORE*********************
'Player 1 Digit 1
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=1 and P1D1LED6.state=1 and P1D1LED7.state=0 then p1d1=0  
If P1D1LED1.state=0 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=0 and P1D1LED5.state=0 and P1D1LED6.state=0 and P1D1LED7.state=0 then P1D1=1
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=0 and P1D1LED4.state=1 and P1D1LED5.state=1 and P1D1LED6.state=0 and P1D1LED7.state=1 then p1d1=2
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=0 and P1D1LED6.state=0 and P1D1LED7.state=1 then p1d1=3 
If P1D1LED1.state=0 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=0 and P1D1LED5.state=0 and P1D1LED6.state=1 and P1D1LED7.state=1 then p1d1=4
If P1D1LED1.state=1 and P1D1LED2.state=0 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=0 and P1D1LED6.state=1 and P1D1LED7.state=1 then p1d1=5
If P1D1LED1.state=1 and P1D1LED2.state=0 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=1 and P1D1LED6.state=1 and P1D1LED7.state=1 then p1d1=6
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=0 and P1D1LED5.state=0 and P1D1LED6.state=0 and P1D1LED7.state=0 then p1d1=7
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=1 and P1D1LED6.state=1 and P1D1LED7.state=1 then p1d1=8
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=0 and P1D1LED6.state=1 and P1D1LED7.state=1 then p1d1=9
d1.text = (P1D1)

'Player 1 Digit 2
If P1D2LED1.state=1 and P1D2LED2.state=1 and P1D2LED3.state=1 and P1D2LED4.state=1 and P1D2LED5.state=1 and P1D2LED6.state=1 and P1D2LED7.state=0 then P1D2=0
If P1D2LED1.state=0 and P1D2LED2.state=1 and P1D2LED3.state=1 and P1D2LED4.state=0 and P1D2LED5.state=0 and P1D2LED6.state=0 and P1D2LED7.state=0 then P1D2=1
If P1D2LED1.state=1 and P1D2LED2.state=1 and P1D2LED3.state=0 and P1D2LED4.state=1 and P1D2LED5.state=1 and P1D2LED6.state=0 and P1D2LED7.state=1 then P1D2=2
If P1D2LED1.state=1 and P1D2LED2.state=1 and P1D2LED3.state=1 and P1D2LED4.state=1 and P1D2LED5.state=0 and P1D2LED6.state=0 and P1D2LED7.state=1 then P1D2=3
If P1D2LED1.state=0 and P1D2LED2.state=1 and P1D2LED3.state=1 and P1D2LED4.state=0 and P1D2LED5.state=0 and P1D2LED6.state=1 and P1D2LED7.state=1 then P1D2=4
If P1D2LED1.state=1 and P1D2LED2.state=0 and P1D2LED3.state=1 and P1D2LED4.state=1 and P1D2LED5.state=0 and P1D2LED6.state=1 and P1D2LED7.state=1 then P1D2=5
If P1D2LED1.state=1 and P1D2LED2.state=0 and P1D2LED3.state=1 and P1D2LED4.state=1 and P1D2LED5.state=1 and P1D2LED6.state=1 and P1D2LED7.state=1 then P1D2=6
If P1D2LED1.state=1 and P1D2LED2.state=1 and P1D2LED3.state=1 and P1D2LED4.state=0 and P1D2LED5.state=0 and P1D2LED6.state=0 and P1D2LED7.state=0 then P1D2=7
If P1D2LED1.state=1 and P1D2LED2.state=1 and P1D2LED3.state=1 and P1D2LED4.state=1 and P1D2LED5.state=1 and P1D2LED6.state=1 and P1D2LED7.state=1 then P1D2=8
If P1D2LED1.state=1 and P1D2LED2.state=1 and P1D2LED3.state=1 and P1D2LED4.state=1 and P1D2LED5.state=0 and P1D2LED6.state=1 and P1D2LED7.state=1 then P1D2=9
d2.text = (P1D2)

'Player 1 Digit 3
If P1D3LED1.state=1 and P1D3LED2.state=1 and P1D3LED3.state=1 and P1D3LED4.state=1 and P1D3LED5.state=1 and P1D3LED6.state=1 and P1D3LED7.state=0 then P1D3=0
If P1D3LED1.state=0 and P1D3LED2.state=1 and P1D3LED3.state=1 and P1D3LED4.state=0 and P1D3LED5.state=0 and P1D3LED6.state=0 and P1D3LED7.state=0 then P1D3=1
If P1D3LED1.state=1 and P1D3LED2.state=1 and P1D3LED3.state=0 and P1D3LED4.state=1 and P1D3LED5.state=1 and P1D3LED6.state=0 and P1D3LED7.state=1 then P1D3=2
If P1D3LED1.state=1 and P1D3LED2.state=1 and P1D3LED3.state=1 and P1D3LED4.state=1 and P1D3LED5.state=0 and P1D3LED6.state=0 and P1D3LED7.state=1 then P1D3=3
If P1D3LED1.state=0 and P1D3LED2.state=1 and P1D3LED3.state=1 and P1D3LED4.state=0 and P1D3LED5.state=0 and P1D3LED6.state=1 and P1D3LED7.state=1 then P1D3=4
If P1D3LED1.state=1 and P1D3LED2.state=0 and P1D3LED3.state=1 and P1D3LED4.state=1 and P1D3LED5.state=0 and P1D3LED6.state=1 and P1D3LED7.state=1 then P1D3=5
If P1D3LED1.state=1 and P1D3LED2.state=0 and P1D3LED3.state=1 and P1D3LED4.state=1 and P1D3LED5.state=1 and P1D3LED6.state=1 and P1D3LED7.state=1 then P1D3=6
If P1D3LED1.state=1 and P1D3LED2.state=1 and P1D3LED3.state=1 and P1D3LED4.state=0 and P1D3LED5.state=0 and P1D3LED6.state=0 and P1D3LED7.state=0 then P1D3=7
If P1D3LED1.state=1 and P1D3LED2.state=1 and P1D3LED3.state=1 and P1D3LED4.state=1 and P1D3LED5.state=1 and P1D3LED6.state=1 and P1D3LED7.state=1 then P1D3=8
If P1D3LED1.state=1 and P1D3LED2.state=1 and P1D3LED3.state=1 and P1D3LED4.state=1 and P1D3LED5.state=0 and P1D3LED6.state=1 and P1D3LED7.state=1 then P1D3=9
d3.text = (P1D3)

'Player 1 Digit 4
If P1D4LED1.state=1 and P1D4LED2.state=1 and P1D4LED3.state=1 and P1D4LED4.state=1 and P1D4LED5.state=1 and P1D4LED6.state=1 and P1D4LED7.state=0 then P1D4=0
If P1D4LED1.state=0 and P1D4LED2.state=1 and P1D4LED3.state=1 and P1D4LED4.state=0 and P1D4LED5.state=0 and P1D4LED6.state=0 and P1D4LED7.state=0 then P1D4=1
If P1D4LED1.state=1 and P1D4LED2.state=1 and P1D4LED3.state=0 and P1D4LED4.state=1 and P1D4LED5.state=1 and P1D4LED6.state=0 and P1D4LED7.state=1 then P1D4=2
If P1D4LED1.state=1 and P1D4LED2.state=1 and P1D4LED3.state=1 and P1D4LED4.state=1 and P1D4LED5.state=0 and P1D4LED6.state=0 and P1D4LED7.state=1 then P1D4=3
If P1D4LED1.state=0 and P1D4LED2.state=1 and P1D4LED3.state=1 and P1D4LED4.state=0 and P1D4LED5.state=0 and P1D4LED6.state=1 and P1D4LED7.state=1 then P1D4=4
If P1D4LED1.state=1 and P1D4LED2.state=0 and P1D4LED3.state=1 and P1D4LED4.state=1 and P1D4LED5.state=0 and P1D4LED6.state=1 and P1D4LED7.state=1 then P1D4=5
If P1D4LED1.state=1 and P1D4LED2.state=0 and P1D4LED3.state=1 and P1D4LED4.state=1 and P1D4LED5.state=1 and P1D4LED6.state=1 and P1D4LED7.state=1 then P1D4=6
If P1D4LED1.state=1 and P1D4LED2.state=1 and P1D4LED3.state=1 and P1D4LED4.state=0 and P1D4LED5.state=0 and P1D4LED6.state=0 and P1D4LED7.state=0 then P1D4=7
If P1D4LED1.state=1 and P1D4LED2.state=1 and P1D4LED3.state=1 and P1D4LED4.state=1 and P1D4LED5.state=1 and P1D4LED6.state=1 and P1D4LED7.state=1 then P1D4=8
If P1D4LED1.state=1 and P1D4LED2.state=1 and P1D4LED3.state=1 and P1D4LED4.state=1 and P1D4LED5.state=0 and P1D4LED6.state=1 and P1D4LED7.state=1 then P1D4=9
d4.text = (P1D4)

'Player 1 Digit 5
If P1D5LED1.state=1 and P1D5LED2.state=1 and P1D5LED3.state=1 and P1D5LED4.state=1 and P1D5LED5.state=1 and P1D5LED6.state=1 and P1D5LED7.state=0 then P1D5=0
If P1D5LED1.state=0 and P1D5LED2.state=1 and P1D5LED3.state=1 and P1D5LED4.state=0 and P1D5LED5.state=0 and P1D5LED6.state=0 and P1D5LED7.state=0 then P1D5=1
If P1D5LED1.state=1 and P1D5LED2.state=1 and P1D5LED3.state=0 and P1D5LED4.state=1 and P1D5LED5.state=1 and P1D5LED6.state=0 and P1D5LED7.state=1 then P1D5=2
If P1D5LED1.state=1 and P1D5LED2.state=1 and P1D5LED3.state=1 and P1D5LED4.state=1 and P1D5LED5.state=0 and P1D5LED6.state=0 and P1D5LED7.state=1 then P1D5=3
If P1D5LED1.state=0 and P1D5LED2.state=1 and P1D5LED3.state=1 and P1D5LED4.state=0 and P1D5LED5.state=0 and P1D5LED6.state=1 and P1D5LED7.state=1 then P1D5=4
If P1D5LED1.state=1 and P1D5LED2.state=0 and P1D5LED3.state=1 and P1D5LED4.state=1 and P1D5LED5.state=0 and P1D5LED6.state=1 and P1D5LED7.state=1 then P1D5=5
If P1D5LED1.state=1 and P1D5LED2.state=0 and P1D5LED3.state=1 and P1D5LED4.state=1 and P1D5LED5.state=1 and P1D5LED6.state=1 and P1D5LED7.state=1 then P1D5=6
If P1D5LED1.state=1 and P1D5LED2.state=1 and P1D5LED3.state=1 and P1D5LED4.state=0 and P1D5LED5.state=0 and P1D5LED6.state=0 and P1D5LED7.state=0 then P1D5=7
If P1D5LED1.state=1 and P1D5LED2.state=1 and P1D5LED3.state=1 and P1D5LED4.state=1 and P1D5LED5.state=1 and P1D5LED6.state=1 and P1D5LED7.state=1 then P1D5=8
If P1D5LED1.state=1 and P1D5LED2.state=1 and P1D5LED3.state=1 and P1D5LED4.state=1 and P1D5LED5.state=0 and P1D5LED6.state=1 and P1D5LED7.state=1 then P1D5=9
d5.text = (P1D5)

'Player 1 Digit 6
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=1 and P1D6LED6.state=1 and P1D6LED7.state=0 then P1D6=0
If P1D6LED1.state=0 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=0 and P1D6LED5.state=0 and P1D6LED6.state=0 and P1D6LED7.state=0 then P1D6=1
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=0 and P1D6LED4.state=1 and P1D6LED5.state=1 and P1D6LED6.state=0 and P1D6LED7.state=1 then P1D6=2
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=0 and P1D6LED6.state=0 and P1D6LED7.state=1 then P1D6=3
If P1D6LED1.state=0 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=0 and P1D6LED5.state=0 and P1D6LED6.state=1 and P1D6LED7.state=1 then P1D6=4
If P1D6LED1.state=1 and P1D6LED2.state=0 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=0 and P1D6LED6.state=1 and P1D6LED7.state=1 then P1D6=5
If P1D6LED1.state=1 and P1D6LED2.state=0 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=1 and P1D6LED6.state=1 and P1D6LED7.state=1 then P1D6=6
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=0 and P1D6LED5.state=0 and P1D6LED6.state=0 and P1D6LED7.state=0 then P1D6=7
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=1 and P1D6LED6.state=1 and P1D6LED7.state=1 then P1D6=8
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=0 and P1D6LED6.state=1 and P1D6LED7.state=1 then P1D6=9
d6.text = (P1D6)

'********************* PLAYER 2 SCORE*********************
'Player 2 Digit 1
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=1 and P2D1LED6.state=1 and P2D1LED7.state=0 then P2D1=0
If P2D1LED1.state=0 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=0 and P2D1LED5.state=0 and P2D1LED6.state=0 and P2D1LED7.state=0 then P2D1=1
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=0 and P2D1LED4.state=1 and P2D1LED5.state=1 and P2D1LED6.state=0 and P2D1LED7.state=1 then P2D1=2
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=0 and P2D1LED6.state=0 and P2D1LED7.state=1 then P2D1=3
If P2D1LED1.state=0 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=0 and P2D1LED5.state=0 and P2D1LED6.state=1 and P2D1LED7.state=1 then P2D1=4
If P2D1LED1.state=1 and P2D1LED2.state=0 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=0 and P2D1LED6.state=1 and P2D1LED7.state=1 then P2D1=5
If P2D1LED1.state=1 and P2D1LED2.state=0 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=1 and P2D1LED6.state=1 and P2D1LED7.state=1 then P2D1=6
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=0 and P2D1LED5.state=0 and P2D1LED6.state=0 and P2D1LED7.state=0 then P2D1=7
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=1 and P2D1LED6.state=1 and P2D1LED7.state=1 then P2D1=8
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=0 and P2D1LED6.state=1 and P2D1LED7.state=1 then P2D1=9


'Player 2 Digit 2
If P2D2LED1.state=1 and P2D2LED2.state=1 and P2D2LED3.state=1 and P2D2LED4.state=1 and P2D2LED5.state=1 and P2D2LED6.state=1 and P2D2LED7.state=0 then P2D2=0
If P2D2LED1.state=0 and P2D2LED2.state=1 and P2D2LED3.state=1 and P2D2LED4.state=0 and P2D2LED5.state=0 and P2D2LED6.state=0 and P2D2LED7.state=0 then P2D2=1
If P2D2LED1.state=1 and P2D2LED2.state=1 and P2D2LED3.state=0 and P2D2LED4.state=1 and P2D2LED5.state=1 and P2D2LED6.state=0 and P2D2LED7.state=1 then P2D2=2
If P2D2LED1.state=1 and P2D2LED2.state=1 and P2D2LED3.state=1 and P2D2LED4.state=1 and P2D2LED5.state=0 and P2D2LED6.state=0 and P2D2LED7.state=1 then P2D2=3
If P2D2LED1.state=0 and P2D2LED2.state=1 and P2D2LED3.state=1 and P2D2LED4.state=0 and P2D2LED5.state=0 and P2D2LED6.state=1 and P2D2LED7.state=1 then P2D2=4
If P2D2LED1.state=1 and P2D2LED2.state=0 and P2D2LED3.state=1 and P2D2LED4.state=1 and P2D2LED5.state=0 and P2D2LED6.state=1 and P2D2LED7.state=1 then P2D2=5
If P2D2LED1.state=1 and P2D2LED2.state=0 and P2D2LED3.state=1 and P2D2LED4.state=1 and P2D2LED5.state=1 and P2D2LED6.state=1 and P2D2LED7.state=1 then P2D2=6
If P2D2LED1.state=1 and P2D2LED2.state=1 and P2D2LED3.state=1 and P2D2LED4.state=0 and P2D2LED5.state=0 and P2D2LED6.state=0 and P2D2LED7.state=0 then P2D2=7
If P2D2LED1.state=1 and P2D2LED2.state=1 and P2D2LED3.state=1 and P2D2LED4.state=1 and P2D2LED5.state=1 and P2D2LED6.state=1 and P2D2LED7.state=1 then P2D2=8
If P2D2LED1.state=1 and P2D2LED2.state=1 and P2D2LED3.state=1 and P2D2LED4.state=1 and P2D2LED5.state=0 and P2D2LED6.state=1 and P2D2LED7.state=1 then P2D2=9


'Player 2 Digit 3
If P2D3LED1.state=1 and P2D3LED2.state=1 and P2D3LED3.state=1 and P2D3LED4.state=1 and P2D3LED5.state=1 and P2D3LED6.state=1 and P2D3LED7.state=0 then P2D3=0
If P2D3LED1.state=0 and P2D3LED2.state=1 and P2D3LED3.state=1 and P2D3LED4.state=0 and P2D3LED5.state=0 and P2D3LED6.state=0 and P2D3LED7.state=0 then P2D3=1
If P2D3LED1.state=1 and P2D3LED2.state=1 and P2D3LED3.state=0 and P2D3LED4.state=1 and P2D3LED5.state=1 and P2D3LED6.state=0 and P2D3LED7.state=1 then P2D3=2
If P2D3LED1.state=1 and P2D3LED2.state=1 and P2D3LED3.state=1 and P2D3LED4.state=1 and P2D3LED5.state=0 and P2D3LED6.state=0 and P2D3LED7.state=1 then P2D3=3
If P2D3LED1.state=0 and P2D3LED2.state=1 and P2D3LED3.state=1 and P2D3LED4.state=0 and P2D3LED5.state=0 and P2D3LED6.state=1 and P2D3LED7.state=1 then P2D3=4
If P2D3LED1.state=1 and P2D3LED2.state=0 and P2D3LED3.state=1 and P2D3LED4.state=1 and P2D3LED5.state=0 and P2D3LED6.state=1 and P2D3LED7.state=1 then P2D3=5
If P2D3LED1.state=1 and P2D3LED2.state=0 and P2D3LED3.state=1 and P2D3LED4.state=1 and P2D3LED5.state=1 and P2D3LED6.state=1 and P2D3LED7.state=1 then P2D3=6
If P2D3LED1.state=1 and P2D3LED2.state=1 and P2D3LED3.state=1 and P2D3LED4.state=0 and P2D3LED5.state=0 and P2D3LED6.state=0 and P2D3LED7.state=0 then P2D3=7
If P2D3LED1.state=1 and P2D3LED2.state=1 and P2D3LED3.state=1 and P2D3LED4.state=1 and P2D3LED5.state=1 and P2D3LED6.state=1 and P2D3LED7.state=1 then P2D3=8
If P2D3LED1.state=1 and P2D3LED2.state=1 and P2D3LED3.state=1 and P2D3LED4.state=1 and P2D3LED5.state=0 and P2D3LED6.state=1 and P2D3LED7.state=1 then P2D3=9


'Player 2 Digit 4
If P2D4LED1.state=1 and P2D4LED2.state=1 and P2D4LED3.state=1 and P2D4LED4.state=1 and P2D4LED5.state=1 and P2D4LED6.state=1 and P2D4LED7.state=0 then P2D4=0
If P2D4LED1.state=0 and P2D4LED2.state=1 and P2D4LED3.state=1 and P2D4LED4.state=0 and P2D4LED5.state=0 and P2D4LED6.state=0 and P2D4LED7.state=0 then P2D4=1
If P2D4LED1.state=1 and P2D4LED2.state=1 and P2D4LED3.state=0 and P2D4LED4.state=1 and P2D4LED5.state=1 and P2D4LED6.state=0 and P2D4LED7.state=1 then P2D4=2
If P2D4LED1.state=1 and P2D4LED2.state=1 and P2D4LED3.state=1 and P2D4LED4.state=1 and P2D4LED5.state=0 and P2D4LED6.state=0 and P2D4LED7.state=1 then P2D4=3
If P2D4LED1.state=0 and P2D4LED2.state=1 and P2D4LED3.state=1 and P2D4LED4.state=0 and P2D4LED5.state=0 and P2D4LED6.state=1 and P2D4LED7.state=1 then P2D4=4
If P2D4LED1.state=1 and P2D4LED2.state=0 and P2D4LED3.state=1 and P2D4LED4.state=1 and P2D4LED5.state=0 and P2D4LED6.state=1 and P2D4LED7.state=1 then P2D4=5
If P2D4LED1.state=1 and P2D4LED2.state=0 and P2D4LED3.state=1 and P2D4LED4.state=1 and P2D4LED5.state=1 and P2D4LED6.state=1 and P2D4LED7.state=1 then P2D4=6
If P2D4LED1.state=1 and P2D4LED2.state=1 and P2D4LED3.state=1 and P2D4LED4.state=0 and P2D4LED5.state=0 and P2D4LED6.state=0 and P2D4LED7.state=0 then P2D4=7
If P2D4LED1.state=1 and P2D4LED2.state=1 and P2D4LED3.state=1 and P2D4LED4.state=1 and P2D4LED5.state=1 and P2D4LED6.state=1 and P2D4LED7.state=1 then P2D4=8
If P2D4LED1.state=1 and P2D4LED2.state=1 and P2D4LED3.state=1 and P2D4LED4.state=1 and P2D4LED5.state=0 and P2D4LED6.state=1 and P2D4LED7.state=1 then P2D4=9


'Player 2 Digit 5
If P2D5LED1.state=1 and P2D5LED2.state=1 and P2D5LED3.state=1 and P2D5LED4.state=1 and P2D5LED5.state=1 and P2D5LED6.state=1 and P2D5LED7.state=0 then P2D5=0
If P2D5LED1.state=0 and P2D5LED2.state=1 and P2D5LED3.state=1 and P2D5LED4.state=0 and P2D5LED5.state=0 and P2D5LED6.state=0 and P2D5LED7.state=0 then P2D5=1
If P2D5LED1.state=1 and P2D5LED2.state=1 and P2D5LED3.state=0 and P2D5LED4.state=1 and P2D5LED5.state=1 and P2D5LED6.state=0 and P2D5LED7.state=1 then P2D5=2
If P2D5LED1.state=1 and P2D5LED2.state=1 and P2D5LED3.state=1 and P2D5LED4.state=1 and P2D5LED5.state=0 and P2D5LED6.state=0 and P2D5LED7.state=1 then P2D5=3
If P2D5LED1.state=0 and P2D5LED2.state=1 and P2D5LED3.state=1 and P2D5LED4.state=0 and P2D5LED5.state=0 and P2D5LED6.state=1 and P2D5LED7.state=1 then P2D5=4
If P2D5LED1.state=1 and P2D5LED2.state=0 and P2D5LED3.state=1 and P2D5LED4.state=1 and P2D5LED5.state=0 and P2D5LED6.state=1 and P2D5LED7.state=1 then P2D5=5
If P2D5LED1.state=1 and P2D5LED2.state=0 and P2D5LED3.state=1 and P2D5LED4.state=1 and P2D5LED5.state=1 and P2D5LED6.state=1 and P2D5LED7.state=1 then P2D5=6
If P2D5LED1.state=1 and P2D5LED2.state=1 and P2D5LED3.state=1 and P2D5LED4.state=0 and P2D5LED5.state=0 and P2D5LED6.state=0 and P2D5LED7.state=0 then P2D5=7
If P2D5LED1.state=1 and P2D5LED2.state=1 and P2D5LED3.state=1 and P2D5LED4.state=1 and P2D5LED5.state=1 and P2D5LED6.state=1 and P2D5LED7.state=1 then P2D5=8
If P2D5LED1.state=1 and P2D5LED2.state=1 and P2D5LED3.state=1 and P2D5LED4.state=1 and P2D5LED5.state=0 and P2D5LED6.state=1 and P2D5LED7.state=1 then P2D5=9

'Player 2 Digit 6
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=1 and P2D6LED6.state=1 and P2D6LED7.state=0 then P2D6=0
If P2D6LED1.state=0 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=0 and P2D6LED5.state=0 and P2D6LED6.state=0 and P2D6LED7.state=0 then P2D6=1
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=0 and P2D6LED4.state=1 and P2D6LED5.state=1 and P2D6LED6.state=0 and P2D6LED7.state=1 then P2D6=2
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=0 and P2D6LED6.state=0 and P2D6LED7.state=1 then P2D6=3
If P2D6LED1.state=0 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=0 and P2D6LED5.state=0 and P2D6LED6.state=1 and P2D6LED7.state=1 then P2D6=4
If P2D6LED1.state=1 and P2D6LED2.state=0 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=0 and P2D6LED6.state=1 and P2D6LED7.state=1 then P2D6=5
If P2D6LED1.state=1 and P2D6LED2.state=0 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=1 and P2D6LED6.state=1 and P2D6LED7.state=1 then P2D6=6
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=0 and P2D6LED5.state=0 and P2D6LED6.state=0 and P2D6LED7.state=0 then P2D6=7
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=1 and P2D6LED6.state=1 and P2D6LED7.state=1 then P2D6=8
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=0 and P2D6LED6.state=1 and P2D6LED7.state=1 then P2D6=9


'********************* PLAYER 3 SCORE*********************
'Player 3 Digit 1
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=1 and P3D1LED6.state=1 and P3D1LED7.state=0 then P3D1=0
If P3D1LED1.state=0 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=0 and P3D1LED5.state=0 and P3D1LED6.state=0 and P3D1LED7.state=0 then P3D1=1
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=0 and P3D1LED4.state=1 and P3D1LED5.state=1 and P3D1LED6.state=0 and P3D1LED7.state=1 then P3D1=2
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=0 and P3D1LED6.state=0 and P3D1LED7.state=1 then P3D1=3
If P3D1LED1.state=0 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=0 and P3D1LED5.state=0 and P3D1LED6.state=1 and P3D1LED7.state=1 then P3D1=4
If P3D1LED1.state=1 and P3D1LED2.state=0 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=0 and P3D1LED6.state=1 and P3D1LED7.state=1 then P3D1=5
If P3D1LED1.state=1 and P3D1LED2.state=0 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=1 and P3D1LED6.state=1 and P3D1LED7.state=1 then P3D1=6
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=0 and P3D1LED5.state=0 and P3D1LED6.state=0 and P3D1LED7.state=0 then P3D1=7
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=1 and P3D1LED6.state=1 and P3D1LED7.state=1 then P3D1=8
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=0 and P3D1LED6.state=1 and P3D1LED7.state=1 then P3D1=9


'Player 3 Digit 2
If P3D2LED1.state=1 and P3D2LED2.state=1 and P3D2LED3.state=1 and P3D2LED4.state=1 and P3D2LED5.state=1 and P3D2LED6.state=1 and P3D2LED7.state=0 then P3D2=0
If P3D2LED1.state=0 and P3D2LED2.state=1 and P3D2LED3.state=1 and P3D2LED4.state=0 and P3D2LED5.state=0 and P3D2LED6.state=0 and P3D2LED7.state=0 then P3D2=1
If P3D2LED1.state=1 and P3D2LED2.state=1 and P3D2LED3.state=0 and P3D2LED4.state=1 and P3D2LED5.state=1 and P3D2LED6.state=0 and P3D2LED7.state=1 then P3D2=2
If P3D2LED1.state=1 and P3D2LED2.state=1 and P3D2LED3.state=1 and P3D2LED4.state=1 and P3D2LED5.state=0 and P3D2LED6.state=0 and P3D2LED7.state=1 then P3D2=3
If P3D2LED1.state=0 and P3D2LED2.state=1 and P3D2LED3.state=1 and P3D2LED4.state=0 and P3D2LED5.state=0 and P3D2LED6.state=1 and P3D2LED7.state=1 then P3D2=4
If P3D2LED1.state=1 and P3D2LED2.state=0 and P3D2LED3.state=1 and P3D2LED4.state=1 and P3D2LED5.state=0 and P3D2LED6.state=1 and P3D2LED7.state=1 then P3D2=5
If P3D2LED1.state=1 and P3D2LED2.state=0 and P3D2LED3.state=1 and P3D2LED4.state=1 and P3D2LED5.state=1 and P3D2LED6.state=1 and P3D2LED7.state=1 then P3D2=6
If P3D2LED1.state=1 and P3D2LED2.state=1 and P3D2LED3.state=1 and P3D2LED4.state=0 and P3D2LED5.state=0 and P3D2LED6.state=0 and P3D2LED7.state=0 then P3D2=7
If P3D2LED1.state=1 and P3D2LED2.state=1 and P3D2LED3.state=1 and P3D2LED4.state=1 and P3D2LED5.state=1 and P3D2LED6.state=1 and P3D2LED7.state=1 then P3D2=8
If P3D2LED1.state=1 and P3D2LED2.state=1 and P3D2LED3.state=1 and P3D2LED4.state=1 and P3D2LED5.state=0 and P3D2LED6.state=1 and P3D2LED7.state=1 then P3D2=9


'Player 3 Digit 3
If P3D3LED1.state=1 and P3D3LED2.state=1 and P3D3LED3.state=1 and P3D3LED4.state=1 and P3D3LED5.state=1 and P3D3LED6.state=1 and P3D3LED7.state=0 then P3D3=0
If P3D3LED1.state=0 and P3D3LED2.state=1 and P3D3LED3.state=1 and P3D3LED4.state=0 and P3D3LED5.state=0 and P3D3LED6.state=0 and P3D3LED7.state=0 then P3D3=1
If P3D3LED1.state=1 and P3D3LED2.state=1 and P3D3LED3.state=0 and P3D3LED4.state=1 and P3D3LED5.state=1 and P3D3LED6.state=0 and P3D3LED7.state=1 then P3D3=2
If P3D3LED1.state=1 and P3D3LED2.state=1 and P3D3LED3.state=1 and P3D3LED4.state=1 and P3D3LED5.state=0 and P3D3LED6.state=0 and P3D3LED7.state=1 then P3D3=3
If P3D3LED1.state=0 and P3D3LED2.state=1 and P3D3LED3.state=1 and P3D3LED4.state=0 and P3D3LED5.state=0 and P3D3LED6.state=1 and P3D3LED7.state=1 then P3D3=4
If P3D3LED1.state=1 and P3D3LED2.state=0 and P3D3LED3.state=1 and P3D3LED4.state=1 and P3D3LED5.state=0 and P3D3LED6.state=1 and P3D3LED7.state=1 then P3D3=5
If P3D3LED1.state=1 and P3D3LED2.state=0 and P3D3LED3.state=1 and P3D3LED4.state=1 and P3D3LED5.state=1 and P3D3LED6.state=1 and P3D3LED7.state=1 then P3D3=6
If P3D3LED1.state=1 and P3D3LED2.state=1 and P3D3LED3.state=1 and P3D3LED4.state=0 and P3D3LED5.state=0 and P3D3LED6.state=0 and P3D3LED7.state=0 then P3D3=7
If P3D3LED1.state=1 and P3D3LED2.state=1 and P3D3LED3.state=1 and P3D3LED4.state=1 and P3D3LED5.state=1 and P3D3LED6.state=1 and P3D3LED7.state=1 then P3D3=8
If P3D3LED1.state=1 and P3D3LED2.state=1 and P3D3LED3.state=1 and P3D3LED4.state=1 and P3D3LED5.state=0 and P3D3LED6.state=1 and P3D3LED7.state=1 then P3D3=9


'Player 3 Digit 4
If P3D4LED1.state=1 and P3D4LED2.state=1 and P3D4LED3.state=1 and P3D4LED4.state=1 and P3D4LED5.state=1 and P3D4LED6.state=1 and P3D4LED7.state=0 then P3D4=0
If P3D4LED1.state=0 and P3D4LED2.state=1 and P3D4LED3.state=1 and P3D4LED4.state=0 and P3D4LED5.state=0 and P3D4LED6.state=0 and P3D4LED7.state=0 then P3D4=1
If P3D4LED1.state=1 and P3D4LED2.state=1 and P3D4LED3.state=0 and P3D4LED4.state=1 and P3D4LED5.state=1 and P3D4LED6.state=0 and P3D4LED7.state=1 then P3D4=2
If P3D4LED1.state=1 and P3D4LED2.state=1 and P3D4LED3.state=1 and P3D4LED4.state=1 and P3D4LED5.state=0 and P3D4LED6.state=0 and P3D4LED7.state=1 then P3D4=3
If P3D4LED1.state=0 and P3D4LED2.state=1 and P3D4LED3.state=1 and P3D4LED4.state=0 and P3D4LED5.state=0 and P3D4LED6.state=1 and P3D4LED7.state=1 then P3D4=4
If P3D4LED1.state=1 and P3D4LED2.state=0 and P3D4LED3.state=1 and P3D4LED4.state=1 and P3D4LED5.state=0 and P3D4LED6.state=1 and P3D4LED7.state=1 then P3D4=5
If P3D4LED1.state=1 and P3D4LED2.state=0 and P3D4LED3.state=1 and P3D4LED4.state=1 and P3D4LED5.state=1 and P3D4LED6.state=1 and P3D4LED7.state=1 then P3D4=6
If P3D4LED1.state=1 and P3D4LED2.state=1 and P3D4LED3.state=1 and P3D4LED4.state=0 and P3D4LED5.state=0 and P3D4LED6.state=0 and P3D4LED7.state=0 then P3D4=7
If P3D4LED1.state=1 and P3D4LED2.state=1 and P3D4LED3.state=1 and P3D4LED4.state=1 and P3D4LED5.state=1 and P3D4LED6.state=1 and P3D4LED7.state=1 then P3D4=8
If P3D4LED1.state=1 and P3D4LED2.state=1 and P3D4LED3.state=1 and P3D4LED4.state=1 and P3D4LED5.state=0 and P3D4LED6.state=1 and P3D4LED7.state=1 then P3D4=9


'Player 3 Digit 5
If P3D5LED1.state=1 and P3D5LED2.state=1 and P3D5LED3.state=1 and P3D5LED4.state=1 and P3D5LED5.state=1 and P3D5LED6.state=1 and P3D5LED7.state=0 then P3D5=0
If P3D5LED1.state=0 and P3D5LED2.state=1 and P3D5LED3.state=1 and P3D5LED4.state=0 and P3D5LED5.state=0 and P3D5LED6.state=0 and P3D5LED7.state=0 then P3D5=1
If P3D5LED1.state=1 and P3D5LED2.state=1 and P3D5LED3.state=0 and P3D5LED4.state=1 and P3D5LED5.state=1 and P3D5LED6.state=0 and P3D5LED7.state=1 then P3D5=2
If P3D5LED1.state=1 and P3D5LED2.state=1 and P3D5LED3.state=1 and P3D5LED4.state=1 and P3D5LED5.state=0 and P3D5LED6.state=0 and P3D5LED7.state=1 then P3D5=3
If P3D5LED1.state=0 and P3D5LED2.state=1 and P3D5LED3.state=1 and P3D5LED4.state=0 and P3D5LED5.state=0 and P3D5LED6.state=1 and P3D5LED7.state=1 then P3D5=4
If P3D5LED1.state=1 and P3D5LED2.state=0 and P3D5LED3.state=1 and P3D5LED4.state=1 and P3D5LED5.state=0 and P3D5LED6.state=1 and P3D5LED7.state=1 then P3D5=5
If P3D5LED1.state=1 and P3D5LED2.state=0 and P3D5LED3.state=1 and P3D5LED4.state=1 and P3D5LED5.state=1 and P3D5LED6.state=1 and P3D5LED7.state=1 then P3D5=6
If P3D5LED1.state=1 and P3D5LED2.state=1 and P3D5LED3.state=1 and P3D5LED4.state=0 and P3D5LED5.state=0 and P3D5LED6.state=0 and P3D5LED7.state=0 then P3D5=7
If P3D5LED1.state=1 and P3D5LED2.state=1 and P3D5LED3.state=1 and P3D5LED4.state=1 and P3D5LED5.state=1 and P3D5LED6.state=1 and P3D5LED7.state=1 then P3D5=8
If P3D5LED1.state=1 and P3D5LED2.state=1 and P3D5LED3.state=1 and P3D5LED4.state=1 and P3D5LED5.state=0 and P3D5LED6.state=1 and P3D5LED7.state=1 then P3D5=9


'Player 3 Digit 6
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=1 and P3D6LED6.state=1 and P3D6LED7.state=0 then P3D6=0
If P3D6LED1.state=0 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=0 and P3D6LED5.state=0 and P3D6LED6.state=0 and P3D6LED7.state=0 then P3D6=1
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=0 and P3D6LED4.state=1 and P3D6LED5.state=1 and P3D6LED6.state=0 and P3D6LED7.state=1 then P3D6=2
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=0 and P3D6LED6.state=0 and P3D6LED7.state=1 then P3D6=3
If P3D6LED1.state=0 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=0 and P3D6LED5.state=0 and P3D6LED6.state=1 and P3D6LED7.state=1 then P3D6=4
If P3D6LED1.state=1 and P3D6LED2.state=0 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=0 and P3D6LED6.state=1 and P3D6LED7.state=1 then P3D6=5
If P3D6LED1.state=1 and P3D6LED2.state=0 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=1 and P3D6LED6.state=1 and P3D6LED7.state=1 then P3D6=6
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=0 and P3D6LED5.state=0 and P3D6LED6.state=0 and P3D6LED7.state=0 then P3D6=7
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=1 and P3D6LED6.state=1 and P3D6LED7.state=1 then P3D6=8
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=0 and P3D6LED6.state=1 and P3D6LED7.state=1 then P3D6=9


'********************* PLAYER 4 SCORE*********************
'Player 4 Digit 1
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=1 and P4D1LED6.state=1 and P4D1LED7.state=0 then P4D1=0
If P4D1LED1.state=0 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=0 and P4D1LED5.state=0 and P4D1LED6.state=0 and P4D1LED7.state=0 then P4D1=1
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=0 and P4D1LED4.state=1 and P4D1LED5.state=1 and P4D1LED6.state=0 and P4D1LED7.state=1 then P4D1=2
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=0 and P4D1LED6.state=0 and P4D1LED7.state=1 then P4D1=3
If P4D1LED1.state=0 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=0 and P4D1LED5.state=0 and P4D1LED6.state=1 and P4D1LED7.state=1 then P4D1=4
If P4D1LED1.state=1 and P4D1LED2.state=0 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=0 and P4D1LED6.state=1 and P4D1LED7.state=1 then P4D1=5
If P4D1LED1.state=1 and P4D1LED2.state=0 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=1 and P4D1LED6.state=1 and P4D1LED7.state=1 then P4D1=6
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=0 and P4D1LED5.state=0 and P4D1LED6.state=0 and P4D1LED7.state=0 then P4D1=7
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=1 and P4D1LED6.state=1 and P4D1LED7.state=1 then P4D1=8
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=0 and P4D1LED6.state=1 and P4D1LED7.state=1 then P4D1=9


'Player 4 Digit 2
If P4D2LED1.state=1 and P4D2LED2.state=1 and P4D2LED3.state=1 and P4D2LED4.state=1 and P4D2LED5.state=1 and P4D2LED6.state=1 and P4D2LED7.state=0 then P4D2=0
If P4D2LED1.state=0 and P4D2LED2.state=1 and P4D2LED3.state=1 and P4D2LED4.state=0 and P4D2LED5.state=0 and P4D2LED6.state=0 and P4D2LED7.state=0 then P4D2=1
If P4D2LED1.state=1 and P4D2LED2.state=1 and P4D2LED3.state=0 and P4D2LED4.state=1 and P4D2LED5.state=1 and P4D2LED6.state=0 and P4D2LED7.state=1 then P4D2=2
If P4D2LED1.state=1 and P4D2LED2.state=1 and P4D2LED3.state=1 and P4D2LED4.state=1 and P4D2LED5.state=0 and P4D2LED6.state=0 and P4D2LED7.state=1 then P4D2=3
If P4D2LED1.state=0 and P4D2LED2.state=1 and P4D2LED3.state=1 and P4D2LED4.state=0 and P4D2LED5.state=0 and P4D2LED6.state=1 and P4D2LED7.state=1 then P4D2=4
If P4D2LED1.state=1 and P4D2LED2.state=0 and P4D2LED3.state=1 and P4D2LED4.state=1 and P4D2LED5.state=0 and P4D2LED6.state=1 and P4D2LED7.state=1 then P4D2=5
If P4D2LED1.state=1 and P4D2LED2.state=0 and P4D2LED3.state=1 and P4D2LED4.state=1 and P4D2LED5.state=1 and P4D2LED6.state=1 and P4D2LED7.state=1 then P4D2=6
If P4D2LED1.state=1 and P4D2LED2.state=1 and P4D2LED3.state=1 and P4D2LED4.state=0 and P4D2LED5.state=0 and P4D2LED6.state=0 and P4D2LED7.state=0 then P4D2=7
If P4D2LED1.state=1 and P4D2LED2.state=1 and P4D2LED3.state=1 and P4D2LED4.state=1 and P4D2LED5.state=1 and P4D2LED6.state=1 and P4D2LED7.state=1 then P4D2=8
If P4D2LED1.state=1 and P4D2LED2.state=1 and P4D2LED3.state=1 and P4D2LED4.state=1 and P4D2LED5.state=0 and P4D2LED6.state=1 and P4D2LED7.state=1 then P4D2=9


'Player 4 Digit 3
If P4D3LED1.state=1 and P4D3LED2.state=1 and P4D3LED3.state=1 and P4D3LED4.state=1 and P4D3LED5.state=1 and P4D3LED6.state=1 and P4D3LED7.state=0 then P4D3=0
If P4D3LED1.state=0 and P4D3LED2.state=1 and P4D3LED3.state=1 and P4D3LED4.state=0 and P4D3LED5.state=0 and P4D3LED6.state=0 and P4D3LED7.state=0 then P4D3=1
If P4D3LED1.state=1 and P4D3LED2.state=1 and P4D3LED3.state=0 and P4D3LED4.state=1 and P4D3LED5.state=1 and P4D3LED6.state=0 and P4D3LED7.state=1 then P4D3=2
If P4D3LED1.state=1 and P4D3LED2.state=1 and P4D3LED3.state=1 and P4D3LED4.state=1 and P4D3LED5.state=0 and P4D3LED6.state=0 and P4D3LED7.state=1 then P4D3=3
If P4D3LED1.state=0 and P4D3LED2.state=1 and P4D3LED3.state=1 and P4D3LED4.state=0 and P4D3LED5.state=0 and P4D3LED6.state=1 and P4D3LED7.state=1 then P4D3=4
If P4D3LED1.state=1 and P4D3LED2.state=0 and P4D3LED3.state=1 and P4D3LED4.state=1 and P4D3LED5.state=0 and P4D3LED6.state=1 and P4D3LED7.state=1 then P4D3=5
If P4D3LED1.state=1 and P4D3LED2.state=0 and P4D3LED3.state=1 and P4D3LED4.state=1 and P4D3LED5.state=1 and P4D3LED6.state=1 and P4D3LED7.state=1 then P4D3=6
If P4D3LED1.state=1 and P4D3LED2.state=1 and P4D3LED3.state=1 and P4D3LED4.state=0 and P4D3LED5.state=0 and P4D3LED6.state=0 and P4D3LED7.state=0 then P4D3=7
If P4D3LED1.state=1 and P4D3LED2.state=1 and P4D3LED3.state=1 and P4D3LED4.state=1 and P4D3LED5.state=1 and P4D3LED6.state=1 and P4D3LED7.state=1 then P4D3=8
If P4D3LED1.state=1 and P4D3LED2.state=1 and P4D3LED3.state=1 and P4D3LED4.state=1 and P4D3LED5.state=0 and P4D3LED6.state=1 and P4D3LED7.state=1 then P4D3=9


'Player 4 Digit 4
If P4D4LED1.state=1 and P4D4LED2.state=1 and P4D4LED3.state=1 and P4D4LED4.state=1 and P4D4LED5.state=1 and P4D4LED6.state=1 and P4D4LED7.state=0 then P4D4=0
If P4D4LED1.state=0 and P4D4LED2.state=1 and P4D4LED3.state=1 and P4D4LED4.state=0 and P4D4LED5.state=0 and P4D4LED6.state=0 and P4D4LED7.state=0 then P4D4=1
If P4D4LED1.state=1 and P4D4LED2.state=1 and P4D4LED3.state=0 and P4D4LED4.state=1 and P4D4LED5.state=1 and P4D4LED6.state=0 and P4D4LED7.state=1 then P4D4=2
If P4D4LED1.state=1 and P4D4LED2.state=1 and P4D4LED3.state=1 and P4D4LED4.state=1 and P4D4LED5.state=0 and P4D4LED6.state=0 and P4D4LED7.state=1 then P4D4=3
If P4D4LED1.state=0 and P4D4LED2.state=1 and P4D4LED3.state=1 and P4D4LED4.state=0 and P4D4LED5.state=0 and P4D4LED6.state=1 and P4D4LED7.state=1 then P4D4=4
If P4D4LED1.state=1 and P4D4LED2.state=0 and P4D4LED3.state=1 and P4D4LED4.state=1 and P4D4LED5.state=0 and P4D4LED6.state=1 and P4D4LED7.state=1 then P4D4=5
If P4D4LED1.state=1 and P4D4LED2.state=0 and P4D4LED3.state=1 and P4D4LED4.state=1 and P4D4LED5.state=1 and P4D4LED6.state=1 and P4D4LED7.state=1 then P4D4=6
If P4D4LED1.state=1 and P4D4LED2.state=1 and P4D4LED3.state=1 and P4D4LED4.state=0 and P4D4LED5.state=0 and P4D4LED6.state=0 and P4D4LED7.state=0 then P4D4=7
If P4D4LED1.state=1 and P4D4LED2.state=1 and P4D4LED3.state=1 and P4D4LED4.state=1 and P4D4LED5.state=1 and P4D4LED6.state=1 and P4D4LED7.state=1 then P4D4=8
If P4D4LED1.state=1 and P4D4LED2.state=1 and P4D4LED3.state=1 and P4D4LED4.state=1 and P4D4LED5.state=0 and P4D4LED6.state=1 and P4D4LED7.state=1 then P4D4=9


'Player 4 Digit 5
If P4D5LED1.state=1 and P4D5LED2.state=1 and P4D5LED3.state=1 and P4D5LED4.state=1 and P4D5LED5.state=1 and P4D5LED6.state=1 and P4D5LED7.state=0 then P4D5=0
If P4D5LED1.state=0 and P4D5LED2.state=1 and P4D5LED3.state=1 and P4D5LED4.state=0 and P4D5LED5.state=0 and P4D5LED6.state=0 and P4D5LED7.state=0 then P4D5=1
If P4D5LED1.state=1 and P4D5LED2.state=1 and P4D5LED3.state=0 and P4D5LED4.state=1 and P4D5LED5.state=1 and P4D5LED6.state=0 and P4D5LED7.state=1 then P4D5=2
If P4D5LED1.state=1 and P4D5LED2.state=1 and P4D5LED3.state=1 and P4D5LED4.state=1 and P4D5LED5.state=0 and P4D5LED6.state=0 and P4D5LED7.state=1 then P4D5=3
If P4D5LED1.state=0 and P4D5LED2.state=1 and P4D5LED3.state=1 and P4D5LED4.state=0 and P4D5LED5.state=0 and P4D5LED6.state=1 and P4D5LED7.state=1 then P4D5=4
If P4D5LED1.state=1 and P4D5LED2.state=0 and P4D5LED3.state=1 and P4D5LED4.state=1 and P4D5LED5.state=0 and P4D5LED6.state=1 and P4D5LED7.state=1 then P4D5=5
If P4D5LED1.state=1 and P4D5LED2.state=0 and P4D5LED3.state=1 and P4D5LED4.state=1 and P4D5LED5.state=1 and P4D5LED6.state=1 and P4D5LED7.state=1 then P4D5=6
If P4D5LED1.state=1 and P4D5LED2.state=1 and P4D5LED3.state=1 and P4D5LED4.state=0 and P4D5LED5.state=0 and P4D5LED6.state=0 and P4D5LED7.state=0 then P4D5=7
If P4D5LED1.state=1 and P4D5LED2.state=1 and P4D5LED3.state=1 and P4D5LED4.state=1 and P4D5LED5.state=1 and P4D5LED6.state=1 and P4D5LED7.state=1 then P4D5=8
If P4D5LED1.state=1 and P4D5LED2.state=1 and P4D5LED3.state=1 and P4D5LED4.state=1 and P4D5LED5.state=0 and P4D5LED6.state=1 and P4D5LED7.state=1 then P4D5=9


'Player 4 Digit 6
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=1 and P4D6LED6.state=1 and P4D6LED7.state=0 then P4D6=0
If P4D6LED1.state=0 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=0 and P4D6LED5.state=0 and P4D6LED6.state=0 and P4D6LED7.state=0 then P4D6=1
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=0 and P4D6LED4.state=1 and P4D6LED5.state=1 and P4D6LED6.state=0 and P4D6LED7.state=1 then P4D6=2
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=0 and P4D6LED6.state=0 and P4D6LED7.state=1 then P4D6=3
If P4D6LED1.state=0 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=0 and P4D6LED5.state=0 and P4D6LED6.state=1 and P4D6LED7.state=1 then P4D6=4
If P4D6LED1.state=1 and P4D6LED2.state=0 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=0 and P4D6LED6.state=1 and P4D6LED7.state=1 then P4D6=5
If P4D6LED1.state=1 and P4D6LED2.state=0 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=1 and P4D6LED6.state=1 and P4D6LED7.state=1 then P4D6=6
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=0 and P4D6LED5.state=0 and P4D6LED6.state=0 and P4D6LED7.state=0 then P4D6=7
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=1 and P4D6LED6.state=1 and P4D6LED7.state=1 then P4D6=8
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=0 and P4D6LED6.state=1 and P4D6LED7.state=1 then P4D6=9
 
'Credits Counter
If COINLED1.state=1 and COINLED2.state=1 and COINLED3.state=1 and COINLED4.state=1 and COINLED5.state=1 and COINLED6.state=1 and COINLED7.state=0 then CRED=0
If COINLED1.state=0 and COINLED2.state=1 and COINLED3.state=1 and COINLED4.state=0 and COINLED5.state=0 and COINLED6.state=0 and COINLED7.state=0 then CRED=1
If COINLED1.state=1 and COINLED2.state=1 and COINLED3.state=0 and COINLED4.state=1 and COINLED5.state=1 and COINLED6.state=0 and COINLED7.state=1 then CRED=2
If COINLED1.state=1 and COINLED2.state=1 and COINLED3.state=1 and COINLED4.state=1 and COINLED5.state=0 and COINLED6.state=0 and COINLED7.state=1 then CRED=3
If COINLED1.state=0 and COINLED2.state=1 and COINLED3.state=1 and COINLED4.state=0 and COINLED5.state=0 and COINLED6.state=1 and COINLED7.state=1 then CRED=4
If COINLED1.state=1 and COINLED2.state=0 and COINLED3.state=1 and COINLED4.state=1 and COINLED5.state=0 and COINLED6.state=1 and COINLED7.state=1 then CRED=5
If COINLED1.state=1 and COINLED2.state=0 and COINLED3.state=1 and COINLED4.state=1 and COINLED5.state=1 and COINLED6.state=1 and COINLED7.state=1 then CRED=6



Score(1) = ((p1d1)&(P1D2)&(p1d3)&(P1D4)&(P1D5)&(P1D6)) 
Score(2) = ((p2d1)&(P2D2)&(p2d3)&(P2D4)&(P2D5)&(P2D6))
Score(3) = ((p3d1)&(P3D2)&(p3d3)&(P3D4)&(P3D5)&(P3D6))
Score(4) = ((p4d1)&(P4D2)&(p4d3)&(P4D4)&(P4D5)&(P4D6))

		End If
End If




	If gameover.state=1 then
if GMS = 0 then
PlaySound SoundFXDOF("knocker", 111, 2, DOFKnocker) 
Playsound "gameReset", 0, BgVolume
GMS = 1
End If

'Match counter values
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=1 and P1D1LED6.state=1 and P1D1LED7.state=0 then match1=0  
If P1D1LED1.state=0 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=0 and P1D1LED5.state=0 and P1D1LED6.state=0 and P1D1LED7.state=0 then match1=1
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=0 and P1D1LED4.state=1 and P1D1LED5.state=1 and P1D1LED6.state=0 and P1D1LED7.state=1 then match1=2
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=0 and P1D1LED6.state=0 and P1D1LED7.state=1 then match1=3 
If P1D1LED1.state=0 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=0 and P1D1LED5.state=0 and P1D1LED6.state=1 and P1D1LED7.state=1 then match1=4
If P1D1LED1.state=1 and P1D1LED2.state=0 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=0 and P1D1LED6.state=1 and P1D1LED7.state=1 then match1=5
If P1D1LED1.state=1 and P1D1LED2.state=0 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=1 and P1D1LED6.state=1 and P1D1LED7.state=1 then match1=6
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=0 and P1D1LED5.state=0 and P1D1LED6.state=0 and P1D1LED7.state=0 then match1=7
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=1 and P1D1LED6.state=1 and P1D1LED7.state=1 then match1=8
If P1D1LED1.state=1 and P1D1LED2.state=1 and P1D1LED3.state=1 and P1D1LED4.state=1 and P1D1LED5.state=0 and P1D1LED6.state=1 and P1D1LED7.state=1 then match1=9

'Player 1 Digit 6
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=1 and P1D6LED6.state=1 and P1D6LED7.state=0 then match2=0
If P1D6LED1.state=0 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=0 and P1D6LED5.state=0 and P1D6LED6.state=0 and P1D6LED7.state=0 then match2=1
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=0 and P1D6LED4.state=1 and P1D6LED5.state=1 and P1D6LED6.state=0 and P1D6LED7.state=1 then match2=2
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=0 and P1D6LED6.state=0 and P1D6LED7.state=1 then match2=3
If P1D6LED1.state=0 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=0 and P1D6LED5.state=0 and P1D6LED6.state=1 and P1D6LED7.state=1 then match2=4
If P1D6LED1.state=1 and P1D6LED2.state=0 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=0 and P1D6LED6.state=1 and P1D6LED7.state=1 then match2=5
If P1D6LED1.state=1 and P1D6LED2.state=0 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=1 and P1D6LED6.state=1 and P1D6LED7.state=1 then match2=6
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=0 and P1D6LED5.state=0 and P1D6LED6.state=0 and P1D6LED7.state=0 then match2=7
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=1 and P1D6LED6.state=1 and P1D6LED7.state=1 then match2=8
If P1D6LED1.state=1 and P1D6LED2.state=1 and P1D6LED3.state=1 and P1D6LED4.state=1 and P1D6LED5.state=0 and P1D6LED6.state=1 and P1D6LED7.state=1 then match2=9

'Player 2 Digit 1
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=1 and P2D1LED6.state=1 and P2D1LED7.state=0 then match3=0  
If P2D1LED1.state=0 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=0 and P2D1LED5.state=0 and P2D1LED6.state=0 and P2D1LED7.state=0 then match3=1
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=0 and P2D1LED4.state=1 and P2D1LED5.state=1 and P2D1LED6.state=0 and P2D1LED7.state=1 then match3=2
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=0 and P2D1LED6.state=0 and P2D1LED7.state=1 then match3=3 
If P2D1LED1.state=0 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=0 and P2D1LED5.state=0 and P2D1LED6.state=1 and P2D1LED7.state=1 then match3=4
If P2D1LED1.state=1 and P2D1LED2.state=0 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=0 and P2D1LED6.state=1 and P2D1LED7.state=1 then match3=5
If P2D1LED1.state=1 and P2D1LED2.state=0 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=1 and P2D1LED6.state=1 and P2D1LED7.state=1 then match3=6
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=0 and P2D1LED5.state=0 and P2D1LED6.state=0 and P2D1LED7.state=0 then match3=7
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=1 and P2D1LED6.state=1 and P2D1LED7.state=1 then match3=8
If P2D1LED1.state=1 and P2D1LED2.state=1 and P2D1LED3.state=1 and P2D1LED4.state=1 and P2D1LED5.state=0 and P2D1LED6.state=1 and P2D1LED7.state=1 then match3=9

'Player 2 Digit 6
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=1 and P2D6LED6.state=1 and P2D6LED7.state=0 then match4=0
If P2D6LED1.state=0 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=0 and P2D6LED5.state=0 and P2D6LED6.state=0 and P2D6LED7.state=0 then match4=1
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=0 and P2D6LED4.state=1 and P2D6LED5.state=1 and P2D6LED6.state=0 and P2D6LED7.state=1 then match4=2
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=0 and P2D6LED6.state=0 and P2D6LED7.state=1 then match4=3
If P2D6LED1.state=0 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=0 and P2D6LED5.state=0 and P2D6LED6.state=1 and P2D6LED7.state=1 then match4=4
If P2D6LED1.state=1 and P2D6LED2.state=0 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=0 and P2D6LED6.state=1 and P2D6LED7.state=1 then match4=5
If P2D6LED1.state=1 and P2D6LED2.state=0 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=1 and P2D6LED6.state=1 and P2D6LED7.state=1 then match4=6
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=0 and P2D6LED5.state=0 and P2D6LED6.state=0 and P2D6LED7.state=0 then match4=7
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=1 and P2D6LED6.state=1 and P2D6LED7.state=1 then match4=8
If P2D6LED1.state=1 and P2D6LED2.state=1 and P2D6LED3.state=1 and P2D6LED4.state=1 and P2D6LED5.state=0 and P2D6LED6.state=1 and P2D6LED7.state=1 then match4=9

'Player 3 Digit 1
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=1 and P3D1LED6.state=1 and P3D1LED7.state=0 then match5=0  
If P3D1LED1.state=0 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=0 and P3D1LED5.state=0 and P3D1LED6.state=0 and P3D1LED7.state=0 then match5=1
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=0 and P3D1LED4.state=1 and P3D1LED5.state=1 and P3D1LED6.state=0 and P3D1LED7.state=1 then match5=2
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=0 and P3D1LED6.state=0 and P3D1LED7.state=1 then match5=3 
If P3D1LED1.state=0 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=0 and P3D1LED5.state=0 and P3D1LED6.state=1 and P3D1LED7.state=1 then match5=4
If P3D1LED1.state=1 and P3D1LED2.state=0 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=0 and P3D1LED6.state=1 and P3D1LED7.state=1 then match5=5
If P3D1LED1.state=1 and P3D1LED2.state=0 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=1 and P3D1LED6.state=1 and P3D1LED7.state=1 then match5=6
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=0 and P3D1LED5.state=0 and P3D1LED6.state=0 and P3D1LED7.state=0 then match5=7
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=1 and P3D1LED6.state=1 and P3D1LED7.state=1 then match5=8
If P3D1LED1.state=1 and P3D1LED2.state=1 and P3D1LED3.state=1 and P3D1LED4.state=1 and P3D1LED5.state=0 and P3D1LED6.state=1 and P3D1LED7.state=1 then match5=9

'Player 3 Digit 6
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=1 and P3D6LED6.state=1 and P3D6LED7.state=0 then match6=0
If P3D6LED1.state=0 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=0 and P3D6LED5.state=0 and P3D6LED6.state=0 and P3D6LED7.state=0 then match6=1
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=0 and P3D6LED4.state=1 and P3D6LED5.state=1 and P3D6LED6.state=0 and P3D6LED7.state=1 then match6=2
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=0 and P3D6LED6.state=0 and P3D6LED7.state=1 then match6=3
If P3D6LED1.state=0 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=0 and P3D6LED5.state=0 and P3D6LED6.state=1 and P3D6LED7.state=1 then match6=4
If P3D6LED1.state=1 and P3D6LED2.state=0 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=0 and P3D6LED6.state=1 and P3D6LED7.state=1 then match6=5
If P3D6LED1.state=1 and P3D6LED2.state=0 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=1 and P3D6LED6.state=1 and P3D6LED7.state=1 then match6=6
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=0 and P3D6LED5.state=0 and P3D6LED6.state=0 and P3D6LED7.state=0 then match6=7
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=1 and P3D6LED6.state=1 and P3D6LED7.state=1 then match6=8
If P3D6LED1.state=1 and P3D6LED2.state=1 and P3D6LED3.state=1 and P3D6LED4.state=1 and P3D6LED5.state=0 and P3D6LED6.state=1 and P3D6LED7.state=1 then match6=9

'Player 3 Digit 1
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=1 and P4D1LED6.state=1 and P4D1LED7.state=0 then match7=0  
If P4D1LED1.state=0 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=0 and P4D1LED5.state=0 and P4D1LED6.state=0 and P4D1LED7.state=0 then match7=1
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=0 and P4D1LED4.state=1 and P4D1LED5.state=1 and P4D1LED6.state=0 and P4D1LED7.state=1 then match7=2
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=0 and P4D1LED6.state=0 and P4D1LED7.state=1 then match7=3 
If P4D1LED1.state=0 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=0 and P4D1LED5.state=0 and P4D1LED6.state=1 and P4D1LED7.state=1 then match7=4
If P4D1LED1.state=1 and P4D1LED2.state=0 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=0 and P4D1LED6.state=1 and P4D1LED7.state=1 then match7=5
If P4D1LED1.state=1 and P4D1LED2.state=0 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=1 and P4D1LED6.state=1 and P4D1LED7.state=1 then match7=6
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=0 and P4D1LED5.state=0 and P4D1LED6.state=0 and P4D1LED7.state=0 then match7=7
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=1 and P4D1LED6.state=1 and P4D1LED7.state=1 then match7=8
If P4D1LED1.state=1 and P4D1LED2.state=1 and P4D1LED3.state=1 and P4D1LED4.state=1 and P4D1LED5.state=0 and P4D1LED6.state=1 and P4D1LED7.state=1 then match7=9

'Player 3 Digit 6
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=1 and P4D6LED6.state=1 and P4D6LED7.state=0 then match8=0
If P4D6LED1.state=0 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=0 and P4D6LED5.state=0 and P4D6LED6.state=0 and P4D6LED7.state=0 then match8=1
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=0 and P4D6LED4.state=1 and P4D6LED5.state=1 and P4D6LED6.state=0 and P4D6LED7.state=1 then match8=2
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=0 and P4D6LED6.state=0 and P4D6LED7.state=1 then match8=3
If P4D6LED1.state=0 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=0 and P4D6LED5.state=0 and P4D6LED6.state=1 and P4D6LED7.state=1 then match8=4
If P4D6LED1.state=1 and P4D6LED2.state=0 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=0 and P4D6LED6.state=1 and P4D6LED7.state=1 then match8=5
If P4D6LED1.state=1 and P4D6LED2.state=0 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=1 and P4D6LED6.state=1 and P4D6LED7.state=1 then match8=6
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=0 and P4D6LED5.state=0 and P4D6LED6.state=0 and P4D6LED7.state=0 then match8=7
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=1 and P4D6LED6.state=1 and P4D6LED7.state=1 then match8=8
If P4D6LED1.state=1 and P4D6LED2.state=1 and P4D6LED3.state=1 and P4D6LED4.state=1 and P4D6LED5.state=0 and P4D6LED6.state=1 and P4D6LED7.state=1 then match8=9


d6.text = (P1D6)

	End if

'end insert



 

			If gameover.state=0 then
		If  IsEmpty(ChgLED) Then
			If Not UltraDMD.IsRendering Then
			'When the scene finishes rendering
			
if playerup < 1 then playerup=1
			UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, " " &  Cstr(score(player+1)) & " ", 30, -1, 14, 0.5, 14
			End If
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


baclglassDMD.x = xoff
baclglassDMD.y = yoff
baclglassDMD.height = zoff - 325
baclglassDMD.rotx = xrot

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


'***********************************************************************************************************************************************************
'******************************************************************* DMD SUB ROUTINES **********************************************************************
'***********************************************************************************************************************************************************


Sub BallInPlay
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "BallinPlay.wmv", "", 30, -1, "", -1, -1, 14, 4000, 14

End Sub

'************* Idle sequences to play when table is not in play ****************
Sub HMintro
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "HM Intro.wmv", "", 15, "", -1, 14, 35000, 14
UltraDMD.DisplayScene00Ex "welcome.wmv", "Insert", 30, -1, "Coin", 30, -1, 14, 5000, 14
UltraDMD.DisplayScene00 "promo1.wmv", "", 15, "", -1, 14, 59000, 14
UltraDMD.DisplayScene00Ex "idle.wmv", "Heavy Metal", 30, -1, "1983 Rowamet", 30, -1, 14, 20000, 14

UltraDMD.DisplayScene00 "attract.wmv", "", 15, "", -1, 14, 15000, 14
UltraDMD.DisplayScene00Ex "welcome.wmv", "Insert", 30, -1, "Coin", 30, -1, 14, 5000, 14
UltraDMD.DisplayScene00 "promo2.wmv", "", 15, "", -1, 14, 30000, 14
UltraDMD.DisplayScene00Ex "idle.wmv", "Heavy Metal", 30, -1, "1983 Rowamet", 30, -1, 14, 20000, 14

UltraDMD.DisplayScene00 "attract2.wmv", "", 15, "", -1, 14, 31000, 14
UltraDMD.DisplayScene00Ex "welcome.wmv", "Insert", 30, -1, "Coin", 30, -1, 14, 5000, 14
UltraDMD.DisplayScene00 "promo3.wmv", "", 15, "", -1, 14, 67000, 14
UltraDMD.DisplayScene00Ex "idle.wmv", "Heavy Metal", 30, -1, "1983 Rowamet", 30, -1, 14, 20000, 14

UltraDMD.DisplayScene00 "challenge.wmv", "", 15, "", -1, 14, 32000, 14
UltraDMD.DisplayScene00Ex "welcome.wmv", "Insert", 30, -1, "Coin", 30, -1, 14, 9000, 14
UltraDMD.DisplayScene00Ex "background.wmv", "Heavy Metal", 30, -1, "Rowamet 1983", 30, -1, 14, 9000, 14
End Sub

'************ Per Ball DMD Scenes when ball hits drain


dim counter
sub match_timer
If gameover.state = 1 Then
if counter < (13300 + Players * 400) then
counter = (counter+40)

If gameover.state=1 then 

UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "background.wmv", cstr (match1) &"-" &cstr (match2) &"  " &cstr (match3) &"-" &cstr (match4) &"  " &cstr (match5) &"-" &cstr (match6) &"  " &cstr (match7) &"-" & cstr (match8), 30, -1, "Wait for " &Cstr(Players) & " Match", 30, -1, 14, 1, 14


end if 

Exit Sub

Else

counter = 0:me.enabled = 0::scoreclick=0
exit sub
End If
End If

End Sub



'********************************** TOP lane DMD Events **********************************************
Sub TopLane1

If light37.state = 1  and light38.state = 0 and light39.state = 0 Then
		PlaySound "LaneBonus", 0, BgVolume
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "Lane Bonus.wmv", "Lanes Complete", 30, -1, " BONUS ", 30, -1, 14, 1000, 1
DOF 112, 2 ' pulse shaker
DOF 114, 2 ' pulse red flasher

Else 

UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(Player+1) &" Ball " & Cstr(ballcount), 30, -1, "  ", 30, -1, 14, 1000, 1
End if
End Sub

Sub TopLane2

If light37.state = 0  and light38.state = 1 and light39.state = 0 Then
		PlaySound "LaneBonus", 0, BgVolume
DOF 112, 2 ' pulse shaker
DOF 114, 2 ' pulse red flasher
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "Lane Bonus.wmv", "Lanes Complete", 30, -1, " BONUS ", 30, -1, 14, 1000, 1

Else 


UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(Player+1) &" Ball " & Cstr(ballcount), 30, -1, "  ", 30, -1, 14, 1000, 1
End If
End Sub

Sub TopLane3

If light37.state = 0  and light38.state = 0 and light39.state = 1 Then
		PlaySound "LaneBonus", 0, BgVolume
DOF 112, 2 ' pulse shaker
DOF 114, 2 ' pulse red flasher
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "Lane Bonus.wmv", "Lanes Complete", 30, -1, " BONUS ", 30, -1, 14, 1000, 1

Else 

UltraDMD.CancelRendering()
UltraDMD.DisplayScene00Ex "Lane Bonus.wmv", "Player " & Cstr(Player+1) &" Ball " & Cstr(ballcount), 30, -1, "  ", 30, -1, 14, 1000, 1
End If
End Sub
'**********************************End Toplane *******************************************
'*
'*
'********************************* Outlane DMD Events ************************************

Sub LOutlane
		UltraDMD.CancelRendering()
If Light23.state = 1 then
		UltraDMD.DisplayScene00Ex "outlane bonus.wmv", "Player " & Cstr(player+1), 30, -1,  " Bonus!", 30, -1, 14, 1600, 14
Else
		UltraDMD.DisplayScene00Ex "lostball.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, "Gone", 30, -1, 14, 1600, 14
    end if

End Sub 




Sub RInlane
		UltraDMD.CancelRendering()
If Light25.state = 1 then
		UltraDMD.DisplayScene00Ex "outlane bonus.wmv", "Player " & Cstr(player+1), 30, -1,  " Bonus!", 30, -1, 14, 1600, 14
Else
		UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1), 30, -1,  " Attack!", 30, -1, 14, 1600, 14
    end if

End Sub 

Sub LInlane
UltraDMD.CancelRendering()
	If Light24.state = 1 then
		UltraDMD.DisplayScene00Ex "outlane bonus.wmv", "Player " & Cstr(player+1), 30, -1,  " Bonus!", 30, -1, 14, 1600, 14
Else
		UltraDMD.DisplayScene00Ex "score.wmv", "Player " & Cstr(player+1), 30, -1,  " Attack!", 30, -1, 14, 1600, 14
    end if

End Sub 


Sub ROutlane
		UltraDMD.CancelRendering()
If Light22.state = 1 then
	UltraDMD.DisplayScene00Ex "outlane bonus.wmv", "Player " & Cstr(player+1), 30, -1,  " Bonus!", 30, -1, 14, 1600, 14
Else
		UltraDMD.DisplayScene00Ex "lostball.wmv","Player " & Cstr(player+1) & " Ball " & Cstr(BallCount), 30, -1, "Gone", 30, -1, 14, 1600, 14
    end if

End Sub 
'***************************************** End In/OutLanes ********************************
'*




'********************************************* END BALL LOCK *************************************************************************************************
'*
'*
'************************************ TARGET BANK RESETS
Sub BankReset1
If dhit=0 then
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "leftbank.wmv", "", 15, "", -1, 14, 4000, 14
End If
End Sub

Sub BankReset2
If dhit=0 then
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "sameplayer.wmv", "", 15, "", -1, 14, 3000, 14
End if
End Sub

Sub BankReset3
If dhit=0 then
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "topreset.wmv", "", 15, "", -1, 14, 3000, 14
end if
End Sub

Sub BankReset4
If dhit=0 then
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "bankreset4.wmv", "", 15, "", -1, 14, 3000, 14
end if
End Sub

Sub BankReset5
If dhit=0 then
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "bankreset5.wmv", "", 15, "", -1, 14, 2000, 14
end if
End Sub

Sub BankReset6
If dhit=0 then
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "bankreset6.wmv", "", 15, "", -1, 14, 3100, 14
end if
End Sub

Sub BankReset7
If dhit=0 then
UltraDMD.CancelRendering()
UltraDMD.DisplayScene00 "bankreset7.wmv", "", 15, "", -1, 14, 7000, 14
end if
End Sub

'******************** THIS IS THE END *****************************
'If you have read this far, your brain probably hurts



' The GLowing Ball Experiment


Dim GlowBall, ChooseBall
Dim CustomBulbIntensity(10)
Dim red3(10)
Dim green3(10)
Dim Blue3(10)
Dim CustomBallImage(10)
Dim CustomBallLogoMode(10)
Dim CustomBallDecal(10)
Dim CustomBallGlow(10)
ChooseBall = 0

' *** prepare the variable with references to three lights for glow ball ***
Dim Glowing(8)
Set Glowing(0) = Glowball0
Set Glowing(1) = Glowball1
Set Glowing(2) = Glowball2 
Set Glowing(3) = Glowball3
Set Glowing(4) = Glowball4
Set Glowing(5) = Glowball5
Set Glowing(6) = Glowball6
Set Glowing(7) = Glowball7
Set Glowing(8) = Glowball8

' default Ball
CustomBallGlow(0) = 		False
CustomBallImage(0) = 		"locnarlogo"
CustomBallLogoMode(0) = 	True
CustomBallDecal(0) = 		"locnarball"
CustomBulbIntensity(0) = 	2.4
Red3(0) = 0 : Green3(0)	= 0 : Blue3(0) = 0

' Magma Red GlowBall
CustomBallGlow(1) = 		True
CustomBallImage(1) = 		"magmalogo"
CustomBallLogoMode(1) = 	True
CustomBallDecal(1) = 		"magmaball"
CustomBulbIntensity(1) = 	2.4
red3(1) = 100 : Green3(1)	= 10 : Blue3(1) = 10

' Ice Blue GlowBall
CustomBallGlow(2) = 		True
CustomBallImage(2) = 		"icelogo"
CustomBallLogoMode(2) = 	true
CustomBallDecal(2) = 		"iceball"
CustomBulbIntensity(2) = 	2.4
red3(2) = 10 : Green3(2)	= 10 : Blue3(2) = 100

' Loc'Nar GlowBall
CustomBallGlow(3) = 		True
CustomBallImage(3) = 		"locnarlogo"
CustomBallLogoMode(3) = 	True
CustomBallDecal(3) = 		"locnarball"
CustomBulbIntensity(3) = 	2.4
red3(3) = 10 : Green3(3)	= 100 : Blue3(3) = 10

' Ice Purple GlowBall
CustomBallGlow(4) = 		True
CustomBallImage(4) = 		"icelogo"
CustomBallLogoMode(4) = 	true
CustomBallDecal(4) = 		"iceball"
CustomBulbIntensity(4) = 	2.4
red3(4) = 113 : Green3(4)	= 26 : Blue3(4) = 104

' Magma Yellow GlowBall
CustomBallGlow(5) = 		True
CustomBallImage(5) = 		"magmalogo"
CustomBallLogoMode(5) = 	True
CustomBallDecal(5) = 		"magmaball"
CustomBulbIntensity(5) = 	2.4
red3(5) = 100 : Green3(5)	= 100 : Blue3(5) = 0

'*** change ball appearance ***

Sub ChangeBall(ballnr)
 Dim BOT, ii, col
	debug.print "ChangeBall -> " & cstr(ballnr)
	Table1.BallDecalMode = CustomBallLogoMode(ballnr)
	Table1.BallFrontDecal = CustomBallDecal(ballnr)
	Table1.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
	Table1.BallImage = CustomBallImage(ballnr)
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 8
		col = RGB(red3(ballnr), green3(ballnr), Blue3(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
	Next
End Sub

Sub ChangeActive(ballnr)
 Dim BOT, ii, col
	debug.print "ChangeBall -> " & cstr(ballnr)
	ActiveBall.DecalMode = CustomBallLogoMode(ballnr)
	ActiveBall.FrontDecal = CustomBallDecal(ballnr)
	ActiveBall.BulbIntensityScale = CustomBulbIntensity(ballnr)
	ActiveBall.Image = CustomBallImage(ballnr)
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 8
		col = RGB(red3(ballnr), green3(ballnr), Blue3(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
	Next
End Sub
