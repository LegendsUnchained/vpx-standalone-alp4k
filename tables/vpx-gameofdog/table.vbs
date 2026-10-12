' ****************************************************************
'                       VISUAL PINBALL X
'       Game FrameWork JP Salas using Darks DMD Flashes
'		Scripting by RusstyT 
'		VPX script using core.vbs for supporting functions
'            			Version GameOfDog 1.0
' ****************************************************************


'********************************************************************************************************************************

'''Game Of Dog''''DOF by Outhere

'101 - Flipper Left
'102 - Flipper Right
'103 - Slingshot Left
'104 - Slingshot Right
'107 - Bumper Bottom (Right)
'109 - Drop Target Reset (Sub ResetVetTargets)
'110 - Put Ball in Plunger Line ()
'111 - Drop Target Reset (Sub ResetDigTargets)
'112 - Auto Fire
'113 - UpKickerCatch- to Left return ramp(Kicker)
'114 - 
'115 - Sub TriggerFiFi_Hit  (Varitarget)
'116 - Sub UpkickerRelease2
'117 - Sub KickerDogLockKick
'118 - LeftKickBack (Kicker11)
'119 - Shaker (Sub Spinning_Timer)CatChaseSpinner
'120 -
'121 - High Score 3
'122 - Knocker (AwardExtraBall, AwardSuperJackPot & AwardSuperJackpot2, Award Special)
'123 - Sub CreateNewBallUpKickerRelease  ((((CreateNew Ball Release Left Ramp))))
'124 - Beacon (Sub Spinning_Timer)CatChaseSpinner
'125 - (Turns Something on and Off Lighting?)See script 
'126 - Fan (Sub Spinning_Timer)CatChaseSpinner
'127 - SkillShot (Sub AwardSkillshot)
'128 - KickerCanonLoadUKRelease
'129 - Blower 
'130 - 
'131 - 
'132 - Sub KickerCatapultKick (KittyCanonBall SkillShot)
'133 - Sub KingDogUp
'134 - Sub ReleaseBallKickerModes
'135 - Sub KickerDogLockKick
'136 - Sub KickerBooster_Hit(Booster Kicker into top Cat chase spinner)
'137 - Sub KickerDigit_Hit (Subway Magic release to top Left Ramp)
'138 - 
'151 - Lighting 
'152 - See script


Option Explicit
Randomize
Dim VRRoom
Dim Stuff
Dim UseFlexDMD
Dim GlassScratchesOn

'*********************************************************************************************
'  Player Options
'************************************************************************************************

Const FlexDMDHighQuality =	True
Const ForceBackglass = 0		' 1 = on ( for dt if you want the backglass on it )
Const AmbientBallShadowOn =	1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const DynamicBallShadowsOn=	1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that behaves like a true shadow!
									'2 = flasher image shadow, but it moves like ninuzzu's
Dim eyeFollowS: eyeFollowS= 8	'ms timer interval of eye follow speed try 5-20 range
Const BallBright = 1			'0 - Normal, 1 - Bright

Dim SongVolume			'1 is full volume. Value is from 0 to 1, change in F12 menu
SongVolume=0.4
Dim VolumeDial		'Overall Mechanical sound effect volume. Recommended values should be no greater than 1, change in F12 menu
Dim BallRollVolume		'Level of ball rolling volume. Value between 0 and 1, change in F12 menu
Dim RampRollVolume		'Level of ramp rolling volume. Value between 0 and 1, change in F12 menu

'VR OPTIONS....
 
VRRoom = 0 ' 0 - Desktop/FS   1 - VRRoom Note VR Room set to 1 if RenderingMode=2    
GlassScratchesOn = 0     ' 0 - Scratches OFF  1 - Scratches ON
Const VRTest = 0		 'Test VR in DesktopMode
'End VR Options
'*********************************************************************************************

'*********************************************************************************************
'  End Player Options
'************************************************************************************************

Dim mMagnaSave1
Dim SpikeJPCount
Dim SledgeSelect
Dim KenelSledgeSelect
Dim ThroughSledgeSelect
Dim VetChat
Dim DogCatChase
Dim CatsCaught(4)
Dim HolesComplete(4)
Dim DigBonusLevel(4)
Dim ChasesComplete(4)
Dim ChaseBonusLevel(4)
Dim FiFiLevel(4)
Dim FiFiActive(4)
Dim FiFiMultiballReady(4)
Dim GreyKangarooCount(4)
Dim WhizzerCount(4)
Dim CatsLossCount(4)
Dim SuperSheepCount(4)
Dim KingDogCount(4)
Dim KingDogRamps(4)
Dim KingGAchieved(4)
Dim KingNAchieved(4)
Dim KingIAchieved(4)
Dim KingKAchieved(4)
Dim HolesAreDug(4)
Dim CatCount(4)
Dim CatsSorted(4)
Dim CatMultiballReady(4)
Dim SheepCount(4)
Dim SheepSorted(4)
Dim SheepMultiballReady(4)
Dim SpikeReady(4)
Dim CatsLooseActive
Dim SuperSheepActive
Dim MysteryActive(4)
Dim BonoAdvance(4)
Dim BonoMetersCompleted(4)
Dim KingDogActive(4)
Dim FastScoreKing(4)
Dim FastScoreActive
Dim FastScoreCat(4)
Dim FastScoreFiFi(4)
Dim FastScoreDoctorDog(4)
Dim FastScoreSheep(4)
Dim AllBetterActive(4)
Dim VetCount(4)
Dim SpikeMultiballActive
Dim KingDogMultiballReady(4)
Dim KingDogMultiballActive
Dim CatMultiballActive
Dim RightSideDiverterUsed
Dim SelectMode
Dim NextMode1(4)
Dim NextMode2(4)
Dim NextMode3(4)
Dim NextMode4(4)


'  End Player Options
'************************************************************************************************

'//////////////---- LUT (Colour Look Up Table) ----//////////////
'0 = Natural 1
'1 = Natural 2
'2 = Natural 3
'3 = Natural 4
'4 = Natural 5
'5 = Natural 6
'6 = Natural 7
'7 = Natural 8
'8 = Natural 9
'9 = Natural 10
'10 = Warm 1
'11 = Warm 2
'12 = Warm 3
'13 = Warm 4
'14 = Warm 5
'15 = Warm 6
'16 = Warm 7
'17 = Warm 8
'18 = Warm 9
'19 = Warm 10

' Use FlexDMD if in FS mode
Dim CabinetMode
If RenderingMode=2 or VRTest = 1 Then VRRoom=1

If Table1.ShowDT = True And VRRoom=0 then
	UseFlexDMD = True'Dont use Flex in desktop
	for each Stuff in JPDMDAll: Stuff.visible = False: next  'make JP's flashers invvisible for desktop
'		digitgrid.Opacity=60
	Flasher001.visible=True
	Flasher002.visible=True
	lrail.visible=True
	rrail.visible=True
End If

If Table1.ShowDT = True And RenderingMode=2 then
	UseFlexDMD = False'Dont use Flex in desktop
	for each Stuff in JPDMDAll: Stuff.visible = true: next  'make JP's flashers visible for desktop users
		digitgrid.Opacity=60
	rrail.visible = false 
	lrail.visible = false
	Flasher001.visible = false 
	Flasher002.visible = false 
	PinCab_Blades.visible=True

	Else
	UseFlexDMD = true ' Use FlexDMD in FS mode for cabinets
	CabinetMode=1
digitgrid.visible=False
'	digitgrid.Opacity=60
	Flasher001.visible=0
	Flasher002.visible=0
	
End If


If VRRoom =0 then 
for each Stuff in VRRoomCOL: Stuff.visible = False: next  'make room stuff nonvisible
'for each Stuff in VRClock: Stuff.visible = False: next 'make clock stuff nonvisible

ClockTimer.enabled = false
BeerTimer.enabled = false
end if

' Load VRRoom
If VRroom =1 Then
TimerPlunger2.enabled = true 
for each Stuff in VRRoomCOL: Stuff.visible = true: next  'make room stuff visible

'move the DMD into place..
for each Stuff in JPDMDAll: Stuff.x = Stuff.x +1118: next
for each Stuff in JPDMDAll: Stuff.y = Stuff.y -700: next
for each Stuff in JPDMDAll: Stuff.height = Stuff.height +300: next
for each Stuff in JPDMDAll: Stuff.rotx = 274: next

for each Stuff in VRDMDTop: Stuff.y = Stuff.y +22: next
for each Stuff in VRDMDBottom: Stuff.y = Stuff.y -20: next
'DMD done..

for each Stuff in VRCab: Stuff.visible = true: next
for each Stuff in VRNotNeeded: Stuff.visible = False: next


rrail.visible = false 
lrail.visible = false

Flasher001.visible = false 
Flasher002.visible = false  


'VR_Backbox_Backglass.blenddisablelighting = 4
if GlassScratchesOn = 1 then GlassImpurities.visible = true

end if

'******************* VR Plunger **********************
Sub TimerPlunger_Timer
  If VR_Primary_plunger.Y < -171 then
      VR_Primary_plunger.Y = VR_Primary_plunger.Y + 3
  End If
End Sub

Sub TimerPlunger2_Timer
	VR_Primary_plunger.Y = -303.488 + (5* Plunger.Position) -20
End Sub

' End VR Init.....
'*****************************************************************************************************************************
Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1

' Load the core.vbs for supporting Subs and functions
LoadCoreFiles

Sub LoadCoreFiles
	On Error Resume Next
	ExecuteGlobal GetTextFile("core.vbs")
	If Err Then MsgBox "Can't open core.vbs"
	ExecuteGlobal GetTextFile("controller.vbs")
	If Err Then MsgBox "Can't open controller.vbs"
	On Error Goto 0
End Sub

Const cGameName = "GameOfDog1.0"
Const TableName = "GameOfDog1.0"
Const myVersion = "GameOfDog1.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 5  ' limit to 5x in this game, both bonus multiplier and playfield multiplier
Dim BallsPerGame: BallsPerGame = 3   ' usually 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiball
Const tnob = 9
Const lob = 2
Const RubberizerEnabled = 1
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7
Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

'**************************
'Variables
'**************************

Dim ballrolleron
Dim turnonultradmd
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim NewSong
Dim NewSong2
Dim ChooseSongSet1
Dim ChooseSongSet2
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim BonusMultiplierActive(4)
Dim BonusCounter(4)
Dim PlayfieldMultiplier(4)

Dim ModesCompleted(4)

Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot
Dim Jackpot1
Dim Jackpot2
Dim SuperJackpot
Dim SuperJackpot1
Dim SuperJackpot2
Dim SuperJackpot3
Dim Tilt
Dim MechTilt
Dim TiltSensitivity
Dim Tilted
Dim bMechTiltJustHit
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(4)
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim bFlippersEnabled
Dim cFlippersEnabled
Dim cFlipperPressed
Dim AwardSelection
Dim SelectAward
'define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInHole
Dim BallsInLock(4)

'Define Game Flags
Dim bGameFinished
Dim bFreePlay
Dim bGameInPlay
Dim bGameEnded(4)
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim CalloutActive
' core.vbs variables
Dim plungerIM 
Dim cbRight
Dim cbLock1
Dim cbLock2
Dim cbLock3

Dim objShell




' *********************************************************************
'                Visual Pinball Defined Script Events
' ********************************************************************* 

Sub Table1_Init()

	'		resetbackglass
	LoadEM
	Dim i
	Randomize
  LoadDogPicture

	startcontroller

	'AutoPlunger 

	Const IMPowerSetting = 10 ' Plunger Power
	Const IMTime = 1.1        ' Time in seconds for Full Plunge
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swPlunger, IMPowerSetting, IMTime
		.Random 1.5
		.InitExitSnd SoundFX("Popper", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
		.CreateEvents "plungerIM"
	End With



	' Misc. VP table objects Initialisation, droptargets, animations...
'	VPObjects_Init

	' load saved values, highscore, names, jackpot
	Loadhs

	' Initalise the DMD display
	DMD_Init

	' freeplay or coins
	bFreePlay = False 'we want coins
	if bFreePlay Then DOF 125, DOFOn

	'		Loadhs
	bAttractMode = False
	bOnTheFirstBall = False
	bBallInPlungerLane = False
	bBallSaverActive = False
	bBallSaverReady = False
	bMultiBallMode = False
	bGameInPlay = False
	bAutoPlunger = False
	bMusicOn = True
	BallsOnPlayfield = 0
	BallsInHole = 0
	BallsInLock(1) = 0
	BallsInLock(2) = 0
	LastSwitchHit = ""
	Tilt = 0
	MechTilt = 0
	bGameFinished=0
	CatsLoose
	TiltSensitivity = 6
	Tilted = False
	bBonusHeld = False
	bJustStarted = True
	cFlipperPressed=False
	GiOff:PlaySoundAt "fx_relay", KickerModes
	DMDFlush
	StartAttractMode
	Cat1Wall.IsDropped=True:Cat2Wall.IsDropped=True:Cat3Wall.IsDropped=True
	DigTargetsReset
	'LoadLUT
	SheepBoardLightTimer.Enabled=1
	SquirrelSpinnerWall.IsDropped=True
	KingDogUp
	SheepUp
	BoneOMeterAttractTimer.Enabled=1
'	ChooseSongSet1=True:ChooseSongSet2=False

End Sub

'**************************************
'Object Initiation   See Serious Sam
'**************************************
' droptargets, animations, etc

'Sub VPObjects_Init 'init objects
'Dim x
 '   TurnOffPlayfieldLights()
'    For each x in ent1: x.Isdropped = 1: next
'	en1.Visible = 1
'End Sub

'********************
' MATHS
'********************

Function RndNum(min,max)
	RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min AND max
End Function

'*****RotateSheepBoardLight*****

Dim LightRotationCount
Sub SheepBoardLightTimer_Timer
	LightRotationCount=LightRotationCount+1
Select Case LightRotationCount
		Case 1 : LightSB1.State=1:LightSB2.State=0:LightSB3.State=0:LightSB4.State=0:LightSB5.State=0:LightSB6.State=0:LightSB7.State=0:LightSB8.State=0
		Case 4 : LightSB1.State=0:LightSB2.State=1
		Case 7 : LightSB2.State=0:LightSB3.State=1
		Case 10 : LightSB3.State=0:LightSB4.State=1
		Case 13 : LightSB4.State=0:LightSB8.State=1
		Case 16 : LightSB8.State=0:LightSB7.State=1
		Case 19 : LightSB7.State=0:LightSB6.State=1
		Case 22 : LightSB5.State=0:LightSB5.State=1:LightRotationCount=0

End Select

End Sub


'**************************
'   KEYS
'**************************


Sub Table1_KeyDown(ByVal Keycode)
'added by jpsalas

If keycode = LeftFlipperKey then 
	FlipperActivate LeftFlipper, LFPress
	VRFlipperLeft.x = VRFlipperLeft.x + 8
End If
If keycode = RightFlipperKey then 
	FlipperActivate RightFlipper, RFPress 
	VRFlipperRight.x = VRFlipperRight.x - 8
End If
 '   If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
 '   If keycode = RightMagnaSave AND bLutActive Then NextLUT

If  bGameInPlay and cFlippersEnabled=True Then
'	If keycode = RightMagnaSave and cFlipperPressed=True Then SelectAwardTimer.Enabled=True:cFlipperPressed=False
'	If keycode = RightMagnaSave and cFlipperPressed=False Then  DMD CL(0, "FLIPPERS FIRST" ), CL(1, "BOOFHEAD"), "", eNone, eNone, eNone, 3000, True, ""
End If


	If Keycode = AddCreditKey Or Keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
		If Credits < 15 Then: Credits = Credits + 1
		if bFreePlay = False Then DOF 125, DOFOn
		If(Tilted = False) Then
			DMDFlush
			DMD "_", CL(1, "   CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True,""

			If NOT bGameInPlay Then ShowTableInfo
		End If
	End If 

	If keycode = PlungerKey Then 
		Plunger.PullBack:SoundPlungerPull()
		If VRroom > 0 then
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
		End if
	End If

	If bGameInPlay Then
			If keycode = LeftTiltKey Then Nudge 90, 2.5:SoundNudgeLeft():CheckTilt
			If keycode = RightTiltKey Then Nudge 270, 2.5:SoundNudgeRight():CheckTilt
			If keycode = CenterTiltKey Then Nudge 0, 1.5:SoundNudgeCenter():CheckTilt
			If keycode = MechanicalTilt Then SoundNudgeCenter:CheckMechTilt
			'********************************************************************************************************************
			If keycode = LeftFlipperKey and bFlippersEnabled Then InstantInfoTimer.Enabled = True:SolLFlipper 1
			If keycode = RightFlipperKey and bFlippersEnabled Then InstantInfoTimer.Enabled = True:SolRFlipper 1
			'**********************************************************************************************************************
			If hsbModeActive Then
				EnterHighScoreKey(keycode)
				Exit Sub
			End If

			If keycode = StartGameKey Then
				If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

					If(bFreePlay = True)Then
						PlayersPlayingGame = PlayersPlayingGame + 1
						TotalGamesPlayed = TotalGamesPlayed + 1

						DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eNone, eNone, 500, True, "fx_fanfare1"

					Else
						If(Credits > 0)then
							PlayersPlayingGame = PlayersPlayingGame + 1
							TotalGamesPlayed = TotalGamesPlayed + 1
							Credits = Credits - 1
							DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eNone, eNone, 500, True, "fx_fanfare2"

						Else
							' Not Enough Credits to start a game.
							DOF 140, DOFOff
							DMD CL(0, "  CREDITS   " & Credits), CL(1, "INSERT COIN      "), "", eNone, eBlink, eNone, 500, True, ""

						End If
					End If
				End If
			End If
	Else ' If (GameInPlay)

	If keycode = LeftFlipperKey Then DMDFlush:ShowTableInfo
	If keycode = RightFlipperKey Then DMDFlush:ShowTableInfo

		If keycode = StartGameKey Then
			If(bFreePlay = True)Then
				If(BallsOnPlayfield = 0)Then
					ResetForNewGame()

				End If
			Else
				If(Credits > 0)Then
					If(BallsOnPlayfield = 0)Then
						Credits = Credits - 1
						ResetForNewGame()

					End If
				Else
					' Not Enough Credits to start a game.
					DOF 140, DOFOff
					DMDFlush
					DMD CL(0, "    CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, ""

				End If
			End If
		End If

	End If ' If (GameInPlay)

End Sub



Sub PlayersCall
	DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eNone, eNone, 500, True, "fx_fanfare2"
End Sub




 
'********************************************************************************************************

Sub Table1_KeyUp(ByVal keycode)
If keycode = LeftFlipperKey then
	FlipperDeActivate LeftFlipper, LFPress
	VRFlipperLeft.x = VRFlipperLeft.x - 8
End If
If keycode = RightFlipperKey then 
	FlipperDeActivate RightFlipper, RFPress
	VRFlipperRight.x = VRFlipperRight.x + 8
End If
 '   If keycode = LeftMagnaSave Then bLutActive = False: HideLUT

	If KeyCode = PlungerKey Then 
		Plunger.Fire : SoundPlungerReleaseBall()   
			If VRRoom > 0 then
			TimerPlunger.Enabled = False
			TimerPlunger2.Enabled = True
			VR_Primary_plunger.Y = -303.488
			End if  
	End If
	If hsbModeActive Then
		InstantInfoTimer.Enabled = False
		bInstantInfo = False
		Exit Sub
	End If

	' Table specific

	If bGameInPLay AND NOT Tilted Then
		If keycode = LeftFlipperKey Then
			SolLFlipper 0
			InstantInfoTimer.Enabled = False
			If bInstantInfo Then
				bInstantInfo = False
				DMDScoreNow
			End If
		End If
		If keycode = RightFlipperKey Then

			SolRFlipper 0
			InstantInfoTimer.Enabled = False
			If bInstantInfo Then
				bInstantInfo = False
				DMDScoreNow
			End If
		End If
	End If

	If  Tilted Then
		If keycode = LeftFlipperKey Then
			SolLFlipper 0
		End If
		If keycode = RightFlipperKey Then
			'RightFlipper1.rotatetostart
			SolRFlipper 0
		End If
	End If
End Sub

Sub InstantInfo
	DMD CL(0, "INSTANT INFO"), "", "", eNone, eNone, eNone, 800, False, ""

        DMD CL(0, "CATS CAUGHT " & CatsSorted(CurrentPlayer)), CL(1, "HOLES DUG " &KingDogCount(CurrentPlayer)), "", eNone, eNone, eNone, 800, True, ""

        DMD CL(0, "SHEEP COUNT " & SheepSorted(CurrentPlayer)), CL(1, "RALPHS A DAD " & FiFiLevel(CurrentPlayer)), "", eNone, eNone, eNone, 800, True, ""

        DMD CL(0, "STINKERS " & GreyKangarooCount(CurrentPlayer)), CL(1, "WHIZZERS " & WhizzerCount(CurrentPlayer)), "", eNone, eNone, eNone, 800, True, ""


        DMD CL(0, "BONOMETERS " & BonoMetersCompleted(CurrentPlayer)),  CL(1, "" ), "", eNone, eNone, eNone, 800, True, ""

        ' calculate the totalbonus
       DMD CL(0, FormatScore(TotalBonus)), CL(1, "TOTAL BONUS " & " X" & BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 1500, True, ""


 '   DMD CL(0, "HIGHEST SCORE"), CL(1, HighScoreName(0) & " " & HighScore(0) ), "", eNone, eNone, eNone, 800, False, ""
End Sub

Sub EndFlipperStatus
	If bInstantInfo Then
		bInstantInfo = False
'		DMDScoreNow
	End If
End Sub


'************************************
'       LUT - Darkness control
' 10 normal level & 10 warmer levels 
'************************************

Dim bLutActive, LUTImage
Dim x
Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 22:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0"
        Case 1:table1.ColorGradeImage = "LUT1"
        Case 2:table1.ColorGradeImage = "LUT2"
        Case 3:table1.ColorGradeImage = "LUT3"
        Case 4:table1.ColorGradeImage = "LUT4"
        Case 5:table1.ColorGradeImage = "LUT5"
        Case 6:table1.ColorGradeImage = "LUT6"
        Case 7:table1.ColorGradeImage = "LUT7"
        Case 8:table1.ColorGradeImage = "LUT8"
        Case 9:table1.ColorGradeImage = "LUT9"
        Case 10:table1.ColorGradeImage = "LUT10"
        Case 11:table1.ColorGradeImage = "LUT Warm 0"
        Case 12:table1.ColorGradeImage = "LUT Warm 1"
        Case 13:table1.ColorGradeImage = "LUT Warm 2"
        Case 14:table1.ColorGradeImage = "LUT Warm 3"
        Case 15:table1.ColorGradeImage = "LUT Warm 4"
        Case 16:table1.ColorGradeImage = "LUT Warm 5"
        Case 17:table1.ColorGradeImage = "LUT Warm 6"
        Case 18:table1.ColorGradeImage = "LUT Warm 7"
        Case 19:table1.ColorGradeImage = "LUT Warm 8"
        Case 20:table1.ColorGradeImage = "LUT Warm 9"
        Case 21:table1.ColorGradeImage = "LUT Warm 10"
    End Select
End Sub

Dim GiIntensity
GiIntensity = 1   'can be used by the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

' New LUT postit
Function GetHSChar(String, Index)
    Dim ThisChar
    Dim FileName
    ThisChar = Mid(String, Index, 1)
    FileName = "PostIt"
    If ThisChar = " " or ThisChar = "" then
        FileName = FileName & "BL"
    ElseIf ThisChar = "<" then
        FileName = FileName & "LT"
    ElseIf ThisChar = "_" then
        FileName = FileName & "SP"
    Else
        FileName = FileName & ThisChar
    End If
    GetHSChar = FileName
End Function

Sub SetLUTLine(String)
    Dim Index
    Dim xFor
    Index = 1
    LUBack.imagea="PostItNote"
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
    SetLUTLine ""
    LUBack.imagea="PostitBL"
End Sub




'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub Table1_Exit
'	SaveLUT
	Savehs
	If UseFlexDMD Then FlexDMD.Run = False
	If B2SOn = true Then Controller.Stop
End Sub


Sub CalloutTimer_Timer()
	CalloutActive=False
	debug.print "Callout Timer Activated" 
	CalloutTimer.Enabled=False
End Sub

'//////////////////////////////////////////////////////////////////////
'// FLIPPERS 
'//////////////////////////////////////////////////////////////////////

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
		DOF  101, DOFOn

		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		DOF  101, DOFOff
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend

		DOF  102, DOFOn
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		DOF  102, DOFOff
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

'Sub RightFlipper1_Collide(parm)
''	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
'	RightFlipperCollide parm
'End Sub

' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
'	FlipperR1Sh.RotZ = RightFlipper1.CurrentAngle
End Sub

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
'dim RF1 : Set RF1 = New FlipperPolarity

InitPolarity

''*******************************************
'' Early 90's and after
'
Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 20
	Next

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -5.5
	AddPt "Polarity", 2, 0.4, -5.5
	AddPt "Polarity", 3, 0.6, -5.0
	AddPt "Polarity", 4, 0.65, -4.5
	AddPt "Polarity", 5, 0.7, -4.0
	AddPt "Polarity", 6, 0.75, -3.5
	AddPt "Polarity", 7, 0.8, -3.0
	AddPt "Polarity", 8, 0.85, -2.5
	AddPt "Polarity", 9, 0.9,-2.0
	AddPt "Polarity", 10, 0.95, -1.5
	AddPt "Polarity", 11, 1, -1.0
	AddPt "Polarity", 12, 1.05, -0.5
	AddPt "Polarity", 13, 1.1, 0
	AddPt "Polarity", 14, 1.3, 0

	addpt "Velocity", 0, 0,         1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41,         1.05
	addpt "Velocity", 3, 0.53,         1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03,         0.945

	LF.Object = LeftFlipper        
	LF.EndPoint = EndPointLp
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub


' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub




'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
				case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
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
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
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
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function


'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState

	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function


'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress,RF1Press, LFCount, RFCount
dim LFState, RFState, RF1State
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle,LFEndAngle

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
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
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
Const EOSReturn = 0.025  'mid 90's and later

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
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
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
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	Else
		If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub


'************************************************************************
'   TILT
'************************************************************************


'
Sub CheckTilt                                    'Called when table is nudged
	If NOT bGameInPlay Then Exit Sub
	Tilt = Tilt + TiltSensitivity                'Add to tilt count
	TiltDecreaseTimer.Enabled = True
	If(Tilt > TiltSensitivity) AND (Tilt <= 15) Then ShowTiltWarning  'show a warning
	If Tilt > 15 Then TiltMachine                'If more that 15 then TILT the table
End Sub

Sub CheckMechTilt                                	'Called when mechanical tilt bob switch closed
	If Not bGameInPlay Then Exit Sub
	If Not bMechTiltJustHit Then
		MechTilt = MechTilt + 1               		'Add to tilt count
		If(MechTilt > 0) AND (MechTilt <= 2) Then ShowTiltWarning 'show a warning
		If MechTilt > 2 Then TiltMachine  			'If more than 2 then TILT the table
		bMechTiltJustHit = True
		TiltDebounceTimer.Enabled = True
	End If
End Sub

Sub ShowTiltWarning
	PlaySound "danger"
	DMD " WARNING", "TILT DANGER", "", eBlink, eBlink, eNone, 2500, True, "" 'Light 
End Sub

Sub TiltMachine
	Tilted = True
	'display Tilt
	PlaySound "danger"
	DMD "   TILT", "YA BOOFHEAD", "", eNone, eNone, eNone, 2500, False, "" 'Light 
	DisableTable True
	TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
End Sub

Sub TiltDecreaseTimer_Timer
	' DecreaseTilt
	If Tilt > 0 Then
		Tilt = Tilt - 0.1
	Else
		TiltDecreaseTimer.Enabled = False
	End If
End Sub

Sub TiltDebounceTimer_Timer
	bMechTiltJustHit = False
	TiltDebounceTimer.Enabled = False
End Sub


Sub DisableTable(Enabled)
	If Enabled Then
		'turn off GI and turn off all the lights
		GiOff
		PlaySoundAt "Relay_GI_Off" , TargetMystery
		LightSeqTilt.Play SeqAllOff
		'Disable slings, bumpers etc
		LeftFlipper.RotateToStart
		RightFlipper.RotateToStart
		Bumper3.Force = 0
		LeftSlingshot.Disabled = 1
		LeftSlingShot4.Disabled = 1
		RightSlingshot.Disabled = 1
		RightSlingShot3.Disabled = 1

		bFlippersEnabled = False
	Else
'		PlaySoundAt "Relay_GI_On" , GISound
		'turn back on GI and the lights
		'GiOn
		LightSeqTilt.StopPlay
		Bumper3.Force = 7
		LeftSlingshot.Disabled = 0
		LeftSlingshot.Disabled = 0
		LeftSlingshot4.Disabled = 0
		RightSlingShot3.Disabled = 0
		bFlippersEnabled = True
		'clean up the buffer display
		DMDFlush
	End If
End Sub

Sub TiltRecoveryTimer_Timer()
	' if all the balls have been drained then..
	If(BallsOnPlayfield = 0)Then
		' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
		EndOfBall()
		TiltRecoveryTimer.Enabled = False
	End If
	' else retry (checks again in another second or so)
End Sub


Dim Song
Song = ""

Sub PlaySong(name)
	If bMusicOn Then
		If Song <> name Then
			StopSound Song
			Song = name
			If Song = "m_End" Then
				PlaySound Song, 0, SongVolume  'this last number is the volume, from 0 to 1
			Else
				PlaySound Song, -1, SongVolume 'this last number is the volume, from 0 to 1
			End If
		End If
	End If
End Sub


Sub ChangeSong
	If ChooseSongset1=True then ChangeSongSet1:NewSong=1:debug.print "NewSong=1"
	If ChooseSongSet2=True then ChangeSongSet2:NewSong2=1:debug.print "NewSong=2"
End Sub

Sub	ChangeSongSet1
debug.print "PlaySongSet1"
		NewSong=NewSong +1
		Select Case NewSong 
			Case 1 PlaySong "m_mrbluesky"	:debug.print "MrBlueSky-Song1"
			Case 2 PlaySong "m_CopyCat"	:debug.print "CopyCat-Song1"
			Case 3 PlaySong "m_TheGrannies"	
			Case 4 PlaySong "m_Omelette"
			Case 5 PlaySong "m_CamillaAndTheChickens":	NewSong=0:	Debug.Print "NewSong=0/MrBlueSky"	
'			Case 6 PlaySong "m_mrbluesky-nointro"
		End Select
End Sub

Sub ChangeSongSet2
debug.print "PlaySongSet2"
		NewSong2=NewSong2 +1
		Select Case NewSong2 
			Case 1 PlaySong "m_Omelette"	
			Case 2 PlaySong "m_CopyCat"	
			Case 3 PlaySong "m_TheGrannies"	
			Case 4 PlaySong "m_CamillaAndTheChickens"
			Case 5 PlaySong "m_CopyCat":	NewSong2=0:	Debug.Print "NewSong2=0/MrBlueSky"
		End Select
End Sub

'================================
'Helper Functions

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

'*******************************************************
'   START GAME, END GAME- User Defined Script Events
'********************************************************

Sub ResetForNewGame()
	Dim i
	bGameInPLay = True
	'resets the score display, and turn off attract mode
	StopAttractMode
	GiOn
	TotalGamesPlayed = TotalGamesPlayed + 1
	CurrentPlayer = 1
	PlayersPlayingGame = 1
	bOnTheFirstBall = True
	For i = 1 To MaxPlayers
		Score(i) = 0
		BonusPoints(i) = 0
		BonusHeldPoints(i) = 0
		BonusMultiplier(i) = 1
		BonusMultiplierActive(i)=0
		BonusCounter(i)= 0
		PlayfieldMultiplier(i) = 1

		BallsRemaining(i) = BallsPerGame
		ExtraBallsAwards(i) = 0

	Next
	' initialise any other flags
	Tilt = 0
	' initialise Game variables
	Game_Init()
	PlaySoundAt "ball_trough", lane4
	' you may wish to start some music, play a sound, do whatever at this point
    vpmtimer.addtimer 1500, "FirstBall '"
End Sub





'***********FIRSTBALL
'FirstBall
'*************************************************

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with
Dim FirstBallStarted ' For DMD Tracking
Sub FirstBall
	FirstBallStarted=True
	' create a new ball in the shooters lane
	vpmtimer.addtimer 1500, " CreateNewBall'"
	NewSong=1: debug.print "SetNewSong=1 at FirstBall"
	NewSong2=1: debug.print "SetNewSong2=1 at FirstBall"
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
	AllBackGlassLightsOn
	' set the current players bonus multiplier back down to 1X
	'    SetBonusMultiplier 1
	' reset any drop targets, lights, game modes etc..

	If (BallsRemaining(CurrentPlayer) =BallsPerGame)  And 	bExtraBallWonThisBall = False Then
		ResetStartofGameVariables
		TurnOnStartOfGameLights 
		ChangeSong

		StartOfGameLightSequence		
	Else
	vpmtimer.addtimer 500, "ResetNewBallVariables '"

	End If
	bExtraBallWonThisBall = False

	'Reset any table specific


	'This is a new ball, so activate the ballsaver
	bBallSaverReady = True
	bBallSaverActive=False
	'and the skillshot
	'    bSkillShotPlayedOnce = False
	bSkillShotReady = True

End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()

	debug.print "CreateNewBall"
	RandomSoundBallRelease Ballrelease
	'PlaySoundAt "ball_trough", lane4 
	DMDScoreNow
	AddScore 0
	' create a ball in the plunger lane kicker.
	BallRelease.CreateSizedball BallSize / 2
	DOF 110 ,DOFPulse

	' There is a (or another) ball on the playfield
	BallsOnPlayfield = BallsOnPlayfield + 1
	' kick it out..
'	RandomSoundBallRelease BallRelease
	BallRelease.Kick 90, 4

	' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
	' set the bAutoPlunger flag to kick the ball in play automatically
	If BallsOnPlayfield > 1 Then
		DOF 129, DOFPulse
		bMultiBallMode = True
		bAutoPlunger = True
	End If
End Sub

Sub BallReleaseSound
'	RandomSoundBallRelease BallRelease
End Sub


' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
 	bAutoPlunger = True
 	debug.print "addmultiball"
 	mBalls2Eject = mBalls2Eject + nballs
'	CreateMultiballTimer.Interval = 1200
 	CreateMultiballTimer.Enabled = True
	'and eject the first ball
'	vpmtimer.addtimer 2000, "DMDScoreNow'"

'	vpmtimer.addtimer 1200, "CreateMultiballTimer_Timer '"
 End Sub
 
' Eject the ball after the delay, AddMultiballDelay
' Eject one queued ball each time the table timer fires.
 Sub CreateMultiballTimer_Timer()
	' wait if there is a ball in the plunger lane
	If bBallInPlungerLane Then
	' Never eject a ball unless one is actually queued.
	If mBalls2Eject <= 0 Then
		mBalls2Eject = 0
		CreateMultiballTimer.Enabled = False
 		Exit Sub
	Else
		If BallsOnPlayfield <MaxMultiballs Then
			CreateNewBall()
			mBalls2Eject = mBalls2Eject -1
			If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
				CreateMultiballTimer.Enabled = False
			End If
		Else 'the max number of multiballs is reached, so stop the timer
			mBalls2Eject = 0
			CreateMultiballTimer.Enabled = False
		End If
	End If
End If

	' Wait while the plunger lane is occupied.
	If bBallInPlungerLane Then Exit Sub

	If BallsOnPlayfield < MaxMultiballs Then
		CreateNewBall()
		mBalls2Eject = mBalls2Eject - 1
	End If

	If mBalls2Eject <= 0 Or BallsOnPlayfield >= MaxMultiballs Then
		mBalls2Eject = 0
		CreateMultiballTimer.Enabled = False
 	End If
 End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded


Sub EndOfBall()
BackGlassEndOfBallTimer.Enabled=1

	PlaySong "m_EndOfBallDrain"
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 10 'yes 10 points :)
	If BonusMultiplier(CurrentPlayer)=0 Then:BonusMultiplier(CurrentPlayer)=1 
	FirstBallStarted=False
	debug.print "EndOfBall"
	'
	' the first ball has been lost. From this point on no new players can join in
	bOnTheFirstBall = False

	' only process any of this if the table is not tilted.  (the tilt recovery
	' mechanism will handle any extra balls or end of game)
'	vpmtimer.addtimer 200, "DMDScoreNow'"
	If NOT Tilted Then

 'Count the bonus. This table uses several bonus
        DMD CL(0, "BONUS"), "", "", eBlink, eNone, eNone, 1000, True, ""


        'CatChaseBonus

       AwardPoints = HolesComplete(CurrentPlayer) * 1000000 * BonusMultiplier(CurrentPlayer)
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "HOLES DUG " &KingDogCount(CurrentPlayer)), "", eBlink, eNone, eNone, 500, True, ""

       AwardPoints = CatsSorted(CurrentPlayer) * 200000 * BonusMultiplier(CurrentPlayer)
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "CATS CAUGHT " & CatsSorted(CurrentPlayer)), "", eBlink, eNone, eNone, 500, True, ""

       AwardPoints =SheepSorted(CurrentPlayer) * 200000 * BonusMultiplier(CurrentPlayer)
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "SHEEP COUNT " & SheepSorted(CurrentPlayer)), "", eBlink, eNone, eNone, 500, True, ""

       AwardPoints = FiFiLevel(CurrentPlayer) * 1000000 * BonusMultiplier(CurrentPlayer)
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "RALPHS A DAD " & FiFiLevel(CurrentPlayer)), "", eBlink, eNone, eNone, 500, True, ""

       AwardPoints = WhizzerCount(CurrentPlayer) * 10000 * BonusMultiplier(CurrentPlayer)
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "WHIZZERS " & WhizzerCount(CurrentPlayer)), "", eBlink, eNone, eNone, 500, True, ""

       AwardPoints = GreyKangarooCount(CurrentPlayer) * 10000 * BonusMultiplier(CurrentPlayer)
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "STINKERS " & GreyKangarooCount(CurrentPlayer)), "", eBlink, eNone, eNone, 500, True, ""

       AwardPoints = BonoMetersCompleted(CurrentPlayer) * 5000000 * BonusMultiplier(CurrentPlayer)
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "BONOMETERS " & BonoMetersCompleted(CurrentPlayer)), "", eBlink, eNone, eNone, 500, True, ""

        ' calculate the totalbonus
       DMD CL(0, FormatScore(TotalBonus)), CL(1, "TOTAL BONUS " & " X" & BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 1500, True, ""
       TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        ' Add the bonus to the score

        AddScore TotalBonus

If PlayersPlayingGame=1 then vpmtimer.addtimer 6500, "EndOfBall2 '"
If PlayersPlayingGame>1 then vpmtimer.addtimer 8500, "EndOfBall2 '"		
		Else 
		vpmtimer.addtimer 100, "EndOfBall2 '" 'If tilted add short delay and move to the second part of end of the ball
	End If
End Sub




' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
	BackGlassEndOfBallTimer.Enabled=0:EOB=0
	AllBackGlassLightsOn
	' if were tilted, reset the internal tilted flag (this will also
	' set TiltWarnings back to zero) which is useful if we are changing player LOL
	Tilted = False
	Tilt = 0
	MechTilt = 0
	DisableTable False 'enable again bumpers and slingshots

	' has the player won an extra-ball ? (might be multiple outstanding)
	If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
		debug.print "Extra Ball"

		' yep got to give it to them
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1


		' if no more EB's then turn off any shoot again light
		If(ExtraBallsAwards(CurrentPlayer) = 0) Then
			LightShootAgain.State = 0
			EBActive=False
			Debug.Print "EBActive=False"
		End If

		' You may wish to do a bit of a song AND dance at this point
		DMD CL(0,"EXTRA BALL"), CL(1,"SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, ""

		' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
		ResetForNewPlayerBall()
		' Create a new ball in the shooters lane
		CreateNewBall
		EBActive=False
		bExtraBallWonThisBall=False
		LightShootAgain.State=0
		debug.print "EndOfBall2"

	Else ' no extra balls

		BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

		' was that the last ball ?
		If(BallsRemaining(CurrentPlayer) <= 0) Then
			'debug.print "No More Balls, High Score Entry"

			' Submit the CurrentPlayers score to the High Score system
		CheckHighScore()
			' you may wish to play some music at this point

		Else

			' not the last ball (for that player)
			' if multiple players are playing then move onto the next one
			EndOfBallComplete()

			debug.print "EndOfBallComplete"

		End If
	End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Dim BallFinished
Sub EndOfBallComplete()
	BackGlassMiniBlinkTimer.Enabled=1
	vpmtimer.addtimer 4000, "AllBackGlassLightsOn'"
	PlaySong "m_EndOfBallDrain"
	Dim NextPlayer
	BallFinished=True
'	ResetAllModes 'Stop current modes

	debug.print "EndOfBall - Complete"

	' are there multiple players playing this game ?
	If(PlayersPlayingGame> 1) Then
		' then move to the next player
		NextPlayer = CurrentPlayer + 1

		' are we going from the last player back to the first
		' (ie say from player 4 back to player 1)
		If(NextPlayer> PlayersPlayingGame) Then
			NextPlayer = 1

		End If
	Else
		NextPlayer = CurrentPlayer

	End If

	'debug.print "Next Player = " & NextPlayer

	' is it the end of the game ? (all balls been lost for all players)
	If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
		' you may wish to do some sort of Point Match free game award here
		' generally only done when not in free play mode

		' set the machine into game over mode
		EndOfGame()

		' you may wish to put a Game Over message on the desktop/backglass

	Else
		' set the next player
		CurrentPlayer = NextPlayer

		' make sure the correct display is up to date
		AddScore 0
		
		' reset the playfield for the new player (or new ball)
		vpmtimer.addtimer 1500, "ResetForNewPlayerBall()'" 

		' AND create a new ball
		vpmtimer.addtimer 2800, "CreateNewBall()'"        

		' play a sound if more than 1 player
		If PlayersPlayingGame> 1 Then
			'         PlaySound "vo_player" &CurrentPlayer
			DMD "_", CL(1, "PLAYER " &CurrentPlayer), "_", eNone, eNone, eNone, 800, True, ""
		End If
	End If

End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
	vpmtimer.addtimer 8000, "EndOfGameCallout'"
	bJustStarted = False
	debug.print "End Of Game"
	bGameInPLay = False
	bGameEnded(CurrentPlayer)=True
	' just ended your game then play the end of game tune


'	bJustStarted = False
	'   ' ensure that the flippers are down
	SolLFlipper 0
	SolRFlipper 0

	' terminate all Mode - eject locked balls
	' most of the Mode/timers terminate at the end of the ball
	'    PlayQuote.Enabled = 0	refer to ghost buster slimer. requires timer and Sub
	' set any lights for the attract mode
	GiOff
	PlaySound "S_Dog17"
	StartAttractMode
	' you may wish to light any Game Over Light you may have
End Sub

Sub EndOfGameCallout
	PlaySound "CO_HeyCommonManThrowMeTheBall"
End Sub

Function Balls
	Dim tmp
	tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
	If tmp> BallsPerGame Then
		Balls = BallsPerGame
	Else
		Balls = tmp
	End If
End Function



Sub 	TurnOffStartOfGameLights

End Sub


' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
' 
Sub Drain_Hit()


	' Destroy the ball
	Drain.DestroyBall
	If Not bGameInPlay Then Exit Sub
	' Exit Sub ' only for debugging - this way you can add balls from the debug window

	BallsOnPlayfield = BallsOnPlayfield - 1

	' pretend to knock the ball into the ball storage mech
	RandomSoundDrain Drain
	'if Tilted the end Ball Mode
	If Tilted Then
		StopEndOfBallMode
	End If

	' if there is a game in progress AND it is not Tilted
	If(bGameInPLay = True) AND(Tilted = False) Then

		' is the ball saver active,
		If(bBallSaverActive = True) Then

			' yep, create a new ball in the shooters lane
			' we use the Addmultiball in case the multiballs are being ejected
			AddMultiball 1
			' we kick the ball with the autoplunger
			bAutoPlunger = True
			' you may wish to put something on a display or play a sound at this point
			DMD "_", CL(1, "BALL SAVED"), "_", eNone, eNone, eNone, 800, True, ""
		Else
			' cancel any multiball if on last ball (ie. lost all other balls)
			If(BallsOnPlayfield = 1) Then
				' AND in a multi-ball??
				If(bMultiBallMode = True) then
					' not in multiball mode any more
					bMultiBallMode = False
					' you may wish to change any music over at this point and
	'				BarbaBlancaMultiballActive=False:debug.print "BarbaBlancaMultiball=False" 'This ensures Golden reef Selection becomes active with 1 ball remaining
	'				DirtyCreatureMuliBallActive=False:debug.print "DirtyCreatureMultiball=False" 'This is a balls on table check. Othermodes are prevented from activated at golden reef when a golden reef mode is active
	'				WildSeasMultiballActive=False:debug.print "WildSeasMultiball=False" 'This is a balls on table check. Othermodes are prevented from activated at golden reef when a golden reef mode is active
				End If
			End If

			' was that the last ball on the playfield
			If(BallsOnPlayfield = 0) Then
				StopEndOfBallMode
 vpmtimer.addtimer 2000, "EndOfBall'"
	'			If ShipSink(CurrentPlayer)=True Then: vpmtimer.addtimer 10000, "EndOfBall'":End If 'ShipSink(CurrentPlayer)=False: End If
'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
			End If
		End If
	End If
End Sub


' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.


Sub TriggerBallSaver_hit
	' if there is a need for a ball saver, then start off a timer
	' only start if it is ready, and it is currently not running, else it will reset the time period
	If SpikeMultiballActive=1 Then Exit Sub

	If(bBallSaverReady = True) AND(20 <> 0) And(bBallSaverActive = False) Then
	EnableBallSaver 25 
	End If

	LastSwitchHit=  "ballsavestarttrigger"	
End Sub

Sub swPlungerRest_Hit()
	debug.print "ball in plunger lane"
	bBallInPlungerLane = True 
	If bMultiBallMode=False And bBallSaverActive = False Then:bSkillshotReady = True
	If RightSideDiverterUsed=1 Then bAutoPlunger=True:debug.print "DiverterAutoPlunge" '****AutoPlunge for side lane diverter*****************
	' turn on Launch light is there is one
	'LaunchLight.State = 2
	' kick the ball in play if the bAutoPlunger flag is on
	If bMultiBallMode=True  Then:bAutoPlunger=True
	If bAutoPlunger=True Then
		vpmtimer.addtimer 1500, "AutoPlungerDelay '"
	End If
	If bMultiBallMode=False Then bSkillShotReady=True
	LastSwitchHit = "swPlungerRest"
End Sub

Sub swPlungerRest_UnHit()
	bBallInPlungerLane = False
	swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
	If bSkillShotReady Then
		ResetSkillShotTimer.Enabled = 1
	End If
End Sub

Sub ResetSkillShotTimer_Timer
	 bSkillShotReady=False:ResetSkillShotTimer.Enabled = 0
End Sub

Sub AutoPlungerDelay
	PlungerIM.Strength = 0.2
	'PlungerIM.AutoFire
	PlungerIM.Strength = Plunger.MechStrength
	Plunger.AutoPlunger = True
	Plunger.Pullback 
	Plunger.Fire
	PlaySoundAt SoundFXDOF("Popper", 112, DOFPulse, DOFContactors), Plunger
	'DOF 125, DOFPulse
	'DOF 112 ,DOFPulse
	bAutoPlunger = False
	Plunger.AutoPlunger = False
End Sub


Sub swPlungerRest_Timer
	swPlungerRest.TimerEnabled = 0
End Sub



Sub EnableBallSaver(seconds)
	debug.print "Ballsaver started"
	' set our game flag
	bBallSaverActive = True
	bBallSaverReady = False
	' start the timer
	BallSaverTimerExpired.Interval = 1000 * seconds
	BallSaverTimerExpired.Enabled = True
	BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
	BallSaverSpeedUpTimer.Enabled = True
	' if you have a ball saver light you might want to turn it on at this point (or make it flash)
	Light_BallSaver.BlinkInterval = 160
	Light_BallSaver.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag

Sub BallSaverTimerExpired_Timer()
	debug.print "Ballsaver ended skillshot not available"
'	BallSaverTimerExpired.Enabled = False
	' clear the flag
	bBallSaverActive = False
	' if you have a ball saver light then turn it off at this point
	'    Light_BallSaver.BlinkInterval = 80

	Light_BallSaver.State = 0
	vpmtimer.addtimer 2000, "GraceTime '"
End Sub


Sub BallSaverSpeedUpTimer_Timer()
	'debug.print "Ballsaver Speed Up Light"
	BallSaverSpeedUpTimer.Enabled = False
	' Speed up the blinking
	Light_BallSaver.BlinkInterval = 80
	Light_BallSaver.State = 2
End Sub

Sub GraceTime
	bBallSaverActive = False
	BallSaverTimerExpired.Enabled = False
End Sub




' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
' In this table we use SecondRound variable to double the score points in the second round after killing Malthael
Sub AddScore(points)
	If(Tilted = False) Then
		' add the points to the current players score variable
		Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
	End if
	' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
	If(Tilted = False) Then
		' add the bonus to the current players bonus variable
		BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
	End if
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
	' Jackpots only generally increment in multiball mode AND not tilted
	' but this doesn't have to be the case
	'    If(Tilted = False) Then

	' If(bMultiBallMode = True) Then
	'       Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
	'       DMD "_", CL(1, "INCREASED JACKPOT"), "_", eNone, eNone, eNone, 800, True, ""
	' you may wish to limit the jackpot to a upper limit, ie..
	'	If (Jackpot >= 6000) Then
	'		Jackpot = 6000
	' 	End if
	'End if
	'   End if
End Sub

Sub AddSuperJackpot(points) 'not used in this table
	'    If(Tilted = False) Then
	'   End if
End Sub

Sub AddBonusMultiplier(n)
	'    Dim NewBonusLevel
	' if not at the maximum bonus level
	'    if(BonusMultiplier(CurrentPlayer) + n <= MaxMultiplier) then
	' then add and set the lights
'	NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
	'       SetBonusMultiplier(NewBonusLevel)
	'       DMD "_", CL(1, "BONUS X " &NewBonusLevel), "_", eNone, eNone, eNone, 2000, True, "fx_bonus"
	'    Else
	'        AddScore 50000
	'        DMD "_", CL(1, "50000"), "_", eNone, eNone, eNone, 800, True, ""
	'   End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly

Sub SetBonusMultiplier(Level)
	'   ' Set the multiplier to the specified level
	'   BonusMultiplier(CurrentPlayer) = Level
	'   UPdateBonusXLights(Level)
End Sub

Sub UpdateBonusXLights(Level)
	' Update the lights
	'    Select Case Level
	'       Case 1:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 0
	'       Case 2:light54.State = 1:light55.State = 0:light56.State = 0:light57.State = 0
	'      Case 3:light54.State = 0:light55.State = 1:light56.State = 0:light57.State = 0
	'      Case 4:light54.State = 0:light55.State = 0:light56.State = 1:light57.State = 0
	''      Case 5:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 1
	'   End Select
End Sub

Sub AddPlayfieldMultiplier(n)
	'    Dim NewPFLevel
	' if not at the maximum level x
	'   if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier) then
	' then add and set the lights
	'        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
	'        SetPlayfieldMultiplier(NewPFLevel)
	'        DMD "_", CL(1, "PLAYFIELD X " &NewPFLevel), "_", eNone, eNone, eNone, 2000, True, "fx_bonus"
	'    Else 'if the 5x is already lit
	'        AddScore 50000
	'        DMD "_", CL(1, "50000"), "_", eNone, eNone, eNone, 2000, True, ""
	'    End if
	'Start the timer to reduce the playfield x every 30 seconds
	'   pfxtimer.Enabled = 0
	'    pfxtimer.Enabled = 1
	'End Sub

	' Set the Playfield Multiplier to the specified level AND set any lights accordingly

	'Sub SetPlayfieldMultiplier(Level)
	' Set the multiplier to the specified level
	'   PlayfieldMultiplier(CurrentPlayer) = Level
	'   UpdatePFXLights(Level)
	'End Sub

	'Sub UpdatePFXLights(Level)
	' Update the lights
	'   Select Case Level
	'       Case 1:light3.State = 0:light2.State = 0:light1.State = 0:light4.State = 0
	'       Case 2:light3.State = 1:light2.State = 0:light1.State = 0:light4.State = 0
	'       Case 3:light3.State = 0:light2.State = 1:light1.State = 0:light4.State = 0
	'       Case 4:light3.State = 0:light2.State = 0:light1.State = 1:light4.State = 0
	'       Case 5:light3.State = 0:light2.State = 0:light1.State = 0:light4.State = 1
	'   End Select
	' show the multiplier in the DMD
	' in this table the multiplier is always shown in the score display sub
End Sub




Sub ExtraBallHurryUp
	debug.print "ExtraBallHurryUpSubRoutine"
	Vpmtimer.addtimer 8000, "ExtraBallLItCallOut'"
	DMD CL(0, "     EXTRA BALL "), CL(1, "  IS LIT RALPH "), "DMD_King2BG", eBlink, eNone, eNone, 4000, True, ""
	If LightShootAgain.State=0 Then LightExtraBall.State=2:LightExtraBall.TimerEnabled=1
End Sub

Sub ExtraBallLitCallOut
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_ExtraBallLit"
	
End Sub


Sub Light_ExtraBall_Timer
	LightExtraBall.State=0
	LightExtraBall.TimerEnabled=0
End Sub

Dim EBActive

Sub AwardExtraBall()
	If LightKingDogReady.State<2 Then vpmtimer.addtimer 2500, "ExtraBallCallout'" 
	If NOT bExtraBallWonThisBall Then
		DOF 122, DOFPulse
		EBActive=True
		PlaySoundAt "fx_knocker",KickerModes
		DogSound
		LightExtraBall.State=0

		vpmtimer.addtimer 8000, "DMDScoreNow'"

		EBActive=1
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
		bExtraBallWonThisBall = True
		LightShootAgain.State = 1 'light the shoot again lamp
		'       GiEffect 2
		'       LightEffect 2
		Debug.print "EBActive=True"
 
	End If
End Sub

Sub ExtraBallCallout
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_ExtraBall"
End Sub

Sub AwardSpecial()
	DMD "_", CL(1, ("REPLAY")), "_", eNone, eBlink, eNone, 2200, True,"" 
	DOF 122, DOFPulse
	vpmTimer.AddTimer 2000, "DMDScoreNow'"
	Credits = Credits + 1
	If bFreePlay = False Then DOF 125, DOFOn
	'    LightEffect 2
	'    FlashEffect 2
End Sub

Sub AwardJackpot 'award a normal jackpot, 
	DMD CL(0, "       WOOFER"), CL(1, "    JACKPOT "), "DMD_Title25", eBlink, eNone, eNone, 3000, True, ""
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_JackPot"
	DogSound
	vpmtimer.addtimer 3200, "JackPotScore'" 
'	DOF 122, DOFPulse

End Sub

Sub JackPotScore
	AddScore 1000000
	DMDScoreNow
End Sub

Sub AwardJackpot2 'award a normal jackpot, 
	DMD CL(0, "       CHEEKY"), CL(1, "    JACKPOT "), "DMD_Title25", eBlink, eNone, eNone, 3000, True, ""
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_DoubleJackPot2"
	vpmtimer.addtimer 3200, "JackPot2Score'" 
	DogSound
'	DOF 122, DOFPulse
End Sub

Sub JackPot2Score
	AddScore 2000000
	DMDScoreNow
End Sub

Sub AwardJackpot3 'award a normal jackpot, 
	DMD CL(0, "      NASTY"), CL(1, "    JACKPOT "), "DMD_Title25", eBlink, eNone, eNone, 3000, True, ""
	DogSound
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_TripleJackPot"
	vpmtimer.addtimer 3500, "JackPot3Score'" 
'	PlaySoundAt "fx_knocker",KickerDogLock
'	DOF 122, DOFPulse
End Sub

Sub JackPot3Score
	AddScore 3000000
	DMDScoreNow
End Sub

Sub AwardSuperJackpot 
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_SuperJackPot"
	DMD CL(0, "        SUPER"), CL(1, "    JACKPOT "), "DMD_Title25", eBlink, eNone, eNone, 3000, True, ""
	PlaySoundAt "fx_knocker",KickerModes
	DogSound

	vpmtimer.addtimer 3500, "SuperJackPotScore'"
	DOF 122, DOFPulse
End Sub

Sub SuperJackPotScore
	AddScore 5000000
	DMDScoreNow
End Sub

Sub AwardSuperJackpot2 
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_SuperDoubleJackPot2"
	DMD CL(0, "     SUPER DUPER"), CL(1, " JACKPOT"), "DMD_Title25", eBlink, eNone, eNone, 3000, True, ""
	PlaySoundAt "fx_knocker",KickerDogLock
	DogSound
	vpmtimer.addtimer 3500, "SuperJackPot2Score'" 
	DOF 122, DOFPulse
End Sub

Sub SuperJackPot2Score
	AddScore 10000000
	DMDScoreNow
End Sub

Sub AwardSkillshot() 'Notused in this game awards at the kickers
	'   Addscore SkillShotValue(CurrentPlayer)
	'    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 250000
	AddSkillScore
	SheepUp
	DogSound
	'   ResetSkillShotTimer_Timer
	'show dmd animation
	DMD CL(0, "THATS SOME SKILL"), CL(1, "RALPH"), "DMD_Title25", eBlink, eNone, eNone, 3000, True, ""
	    DOF 127, DOFPulse
	' increment the skillshot value with 250.000
	'do some light show
	'    GiEffect 2
	'    LightEffect 2
End Sub

Sub AddSkillScore
	AddScore 1000000
DMDScoreNow
End Sub



'*****************************
'    Load / Save / Highscore
'*****************************
'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
	Dim x
	x = LoadValue(TableName, "HighScore1")
	If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 2000000000 :End If
	x = LoadValue(TableName, "HighScore1Name")
	If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" :End If
	x = LoadValue(TableName, "HighScore2")
	If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 1000000000 :End If
	x = LoadValue(TableName, "HighScore2Name")
	If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" :End If
	x = LoadValue(TableName, "HighScore3")
	If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 750000000 :End If
	x = LoadValue(TableName, "HighScore3Name")
	If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" :End If
	x = LoadValue(TableName, "HighScore4")
	If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 500000000 :End If
	x = LoadValue(TableName, "HighScore4Name")
	If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" :End If
	x = LoadValue(TableName, "Credits")
	If(x <> "") then Credits = CInt(x) Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff : End If : End If
	x = LoadValue(TableName, "TotalGamesPlayed")
	If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 :End If
End Sub

Sub Savehs
	SaveValue TableName, "HighScore1", HighScore(0)
	SaveValue TableName, "HighScore1Name", HighScoreName(0)
	SaveValue TableName, "HighScore2", HighScore(1)
	SaveValue TableName, "HighScore2Name", HighScoreName(1)
	SaveValue TableName, "HighScore3", HighScore(2)
	SaveValue TableName, "HighScore3Name", HighScoreName(2)
	SaveValue TableName, "HighScore4", HighScore(3)
	SaveValue TableName, "HighScore4Name", HighScoreName(3)
	SaveValue TableName, "Credits", Credits
	SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
	HighScoreName(0) = "AAA"
	HighScoreName(1) = "BBB"
	HighScoreName(2) = "CCC"
	HighScoreName(3) = "DDD"
	HighScore(0) = 50000000
	HighScore(1) = 80000000
	HighScore(2) = 100000000
	HighScore(3) = 150000000
	Savehs
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
	Dim tmp
	tmp = Score(CurrentPlayer)

	If tmp > HighScore(0)Then 'add 1 credit for beating the highscore
		Credits = Credits + 1
		DOF 125, DOFOn
	End If

	If tmp > HighScore(3)Then
		PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
		DOF 121, DOFPulse
		HighScore(3) = tmp
		'enter player's name
		HighScoreEntryInit()
	Else
		EndOfBallComplete()
	End If
End Sub

Sub HighScoreEntryInit()
	hsbModeActive = True
	hsLetterFlash = 0

	hsEnteredDigits(0) = " "
	hsEnteredDigits(1) = " "
	hsEnteredDigits(2) = " "
	hsCurrentDigit = 0

	hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow
	hsCurrentLetter = 1
	DMDFlush()
	HighScoreDisplayNameNow()

	HighScoreFlashTimer.Interval = 250
	HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
	If keycode = LeftFlipperKey Then
		playsound "fx_Previous"
		hsCurrentLetter = hsCurrentLetter - 1
		if(hsCurrentLetter = 0)then
			hsCurrentLetter = len(hsValidLetters)
		end if
		HighScoreDisplayNameNow()
	End If

	If keycode = RightFlipperKey Then
		playsound "fx_Next"
		hsCurrentLetter = hsCurrentLetter + 1
		if(hsCurrentLetter > len(hsValidLetters))then
			hsCurrentLetter = 1
		end if
		HighScoreDisplayNameNow()
	End If

	If keycode = PlungerKey OR keycode = StartGameKey Then
		if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
			playsound "fx_Enter"
			hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
			hsCurrentDigit = hsCurrentDigit + 1
			if(hsCurrentDigit = 3)then
				HighScoreCommitName()
			else
				HighScoreDisplayNameNow()
			end if
		else
			playsound "fx_Esc"
			hsEnteredDigits(hsCurrentDigit) = " "
			if(hsCurrentDigit > 0)then
				hsCurrentDigit = hsCurrentDigit - 1
			end if
			HighScoreDisplayNameNow()
		end if
	end if
End Sub

Sub HighScoreDisplayNameNow()
	HighScoreFlashTimer.Enabled = False
	hsLetterFlash = 0
	HighScoreDisplayName()
	HighScoreFlashTimer.Enabled = True
End Sub

Dim dLine(2)
Sub HighScoreDisplayName()
	Dim i
	Dim TempTopStr
	Dim TempBotStr

	TempTopStr = "YOUR NAME:"
	dLine(0) = ExpandLine(TempTopStr,0)
	DMDUpdate 0

	TempBotStr = "    > "
	if(hsCurrentDigit > 0)then TempBotStr = TempBotStr & hsEnteredDigits(0)
	if(hsCurrentDigit > 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
	if(hsCurrentDigit > 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

	if(hsCurrentDigit <> 3)then
		if(hsLetterFlash <> 0)then
			TempBotStr = TempBotStr & "_"
		else
			TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
		end if
	end if

	if(hsCurrentDigit < 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
	if(hsCurrentDigit < 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

	TempBotStr = TempBotStr & " <    "
	dLine(1) = ExpandLine(TempBotStr,1)
	DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
	HighScoreFlashTimer.Enabled = False
	hsLetterFlash = hsLetterFlash + 1
	if(hsLetterFlash = 2)then hsLetterFlash = 0
	HighScoreDisplayName()
	HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
	HighScoreFlashTimer.Enabled = False
	hsbModeActive = False

	hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
	if(hsEnteredName = "   ")then
		hsEnteredName = "YOU"
	end if

	HighScoreName(3) = hsEnteredName
	SortHighscore
	EndOfBallComplete()
End Sub

Sub SortHighscore
	Dim tmp, tmp2, i, j
	For i = 0 to 3
		For j = 0 to 2
			If HighScore(j) < HighScore(j + 1)Then
				tmp = HighScore(j + 1)
				tmp2 = HighScoreName(j + 1)
				HighScore(j + 1) = HighScore(j)
				HighScoreName(j + 1) = HighScoreName(j)
				HighScore(j) = tmp
				HighScoreName(j) = tmp2
			End If
		Next
	Next
End Sub



' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dCharsPerLine(2)

Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Dim FlexDMD
Dim DMDScene

Sub DMD_Init() 'default/startup values
	If UseFlexDMD Then
		Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
		If Not FlexDMD is Nothing Then
            If FlexDMDHighQuality Then
				FlexDMD.TableFile = Table1.Filename & ".vpx"
				FlexDMD.RenderMode = 2
				FlexDMD.Width = 256
				FlexDMD.Height = 64
				FlexDMD.Clear = True
				FlexDMD.GameName = cGameName
				FlexDMD.Run = True
				Set DMDScene = FlexDMD.NewGroup("Scene")
				DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
				DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
				For i = 0 to 40
					DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
					Digits(i).Visible = False
				Next
				digitgrid.Visible = False
				For i = 0 to 19 ' Top
					DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 14, 22
				Next
				For i = 20 to 39 ' Bottom
					DMDScene.GetImage("Dig" & i).SetBounds 8 + (i - 20) * 12, 6 + 24 + 4, 14, 22
				Next
				FlexDMD.LockRenderThread
				FlexDMD.Stage.AddActor DMDScene
				FlexDMD.UnlockRenderThread
			Else
				FlexDMD.TableFile = Table1.Filename & ".vpx"
				FlexDMD.RenderMode = 2
				FlexDMD.Width = 128
				FlexDMD.Height = 32
				FlexDMD.Clear = True
				FlexDMD.GameName = cGameName
				FlexDMD.Run = True
				Set DMDScene = FlexDMD.NewGroup("Scene")
				DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
				DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
				For i = 0 to 40
					DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
					Digits(i).Visible = False
				Next
				digitgrid.Visible = False
				For i = 0 to 19 ' Top
					DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 7, 11
				Next
				For i = 20 to 39 ' Bottom
					DMDScene.GetImage("Dig" & i).SetBounds 4 + (i - 20) * 6, 3 + 12 + 2, 7, 11
				Next
				FlexDMD.LockRenderThread
				FlexDMD.Stage.AddActor DMDScene
				FlexDMD.UnlockRenderThread
			End If
		End If
	End If

	Dim i, j
	DMDFlush()
	deSpeed = 20
	deBlinkSlowRate = 5
	deBlinkFastRate = 2
	dCharsPerLine(0) = 18 'characters lower line
	dCharsPerLine(1) = 22 'characters top line
	dCharsPerLine(2) = 1  'characters back line
	For i = 0 to 2
		dLine(i) = Space(dCharsPerLine(i) )
		deCount(i) = 0
		deCountEnd(i) = 0
		deBlinkCycle(i) = 0
		dqTimeOn(i) = 0
		dqbFlush(i) = True
		dqSound(i) = ""
	Next
	For i = 0 to 2
		For j = 0 to 64
			dqText(i, j) = ""
			dqEffect(i, j) = eNone
		Next
	Next
	DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
	Dim i
	DMDTimer.Enabled = False
	DMDEffectTimer.Enabled = False
	dqHead = 0
	dqTail = 0
	For i = 0 to 2
		deCount(i) = 0
		deCountEnd(i) = 0
		deBlinkCycle(i) = 0
	Next
End Sub

Sub DMDScore()
	Dim tmp, tmp1, tmp2
	if(dqHead = dqTail) Then

		tmp = RL(0, FormatScore(Score(Currentplayer) ) )
		'       tmp = CL(0, FormatScore(Score(Currentplayer) ) )
		tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
		tmp2 = ""
		'        tmp2 = "bkborder"
	End If
	DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
	DMDFlush
	DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
	if(dqTail <dqSize) Then
		if(Text0 = "_") Then
			dqEffect(0, dqTail) = eNone
			dqText(0, dqTail) = "_"
		Else
			dqEffect(0, dqTail) = Effect0
			dqText(0, dqTail) = ExpandLine(Text0, 0)
		End If

		if(Text1 = "_") Then
			dqEffect(1, dqTail) = eNone
			dqText(1, dqTail) = "_"
		Else
			dqEffect(1, dqTail) = Effect1
			dqText(1, dqTail) = ExpandLine(Text1, 1)
		End If

		if(Text2 = "_") Then
			dqEffect(2, dqTail) = eNone
			dqText(2, dqTail) = "_"
		Else
			dqEffect(2, dqTail) = Effect2
			dqText(2, dqTail) = Text2 'it is always 1 letter in this table
		End If

		dqTimeOn(dqTail) = TimeOn
		dqbFlush(dqTail) = bFlush
		dqSound(dqTail) = Sound
		dqTail = dqTail + 1
		if(dqTail = 1) Then
			DMDHead()
		End If
	End If
End Sub

Sub DMDHead()
	Dim i
	deCount(0) = 0
	deCount(1) = 0
	deCount(2) = 0
	DMDEffectTimer.Interval = deSpeed

	For i = 0 to 2
		Select Case dqEffect(i, dqHead)
			Case eNone:deCountEnd(i) = 1
			Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead) )
			Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead) )
			Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
				deBlinkCycle(i) = 0
			Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
				deBlinkCycle(i) = 0
		End Select
	Next
	if(dqSound(dqHead) <> "") Then
		PlaySound(dqSound(dqHead) )
	End If
	DMDEffectTimer.Enabled = True
End Sub

Sub DMDEffectTimer_Timer()
	DMDEffectTimer.Enabled = False
	DMDProcessEffectOn()
End Sub

Sub DMDTimer_Timer()
	Dim Head
	DMDTimer.Enabled = False
	Head = dqHead
	dqHead = dqHead + 1
	if(dqHead = dqTail) Then
		if(dqbFlush(Head) = True) Then
			DMDScoreNow()
		Else
			dqHead = 0
			DMDHead()
		End If
	Else
		DMDHead()
	End If
End Sub

Sub DMDProcessEffectOn()
	Dim i
	Dim BlinkEffect
	Dim Temp

	BlinkEffect = False

	For i = 0 to 2
		if(deCount(i) <> deCountEnd(i) ) Then
			deCount(i) = deCount(i) + 1

			select case(dqEffect(i, dqHead) )
				case eNone:
					Temp = dqText(i, dqHead)
				case eScrollLeft:
					Temp = Right(dLine(i), dCharsPerLine(i) - 1)
					Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
				case eScrollRight:
					Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1) - deCount(i), 1)
					Temp = Temp & Left(dLine(i), dCharsPerLine(i) - 1)
				case eBlink:
					BlinkEffect = True
					if((deCount(i) MOD deBlinkSlowRate) = 0) Then
						deBlinkCycle(i) = deBlinkCycle(i) xor 1
					End If

					if(deBlinkCycle(i) = 0) Then
						Temp = dqText(i, dqHead)
					Else
						Temp = Space(dCharsPerLine(i) )
					End If
				case eBlinkFast:
					BlinkEffect = True
					if((deCount(i) MOD deBlinkFastRate) = 0) Then
						deBlinkCycle(i) = deBlinkCycle(i) xor 1
					End If

					if(deBlinkCycle(i) = 0) Then
						Temp = dqText(i, dqHead)
					Else
						Temp = Space(dCharsPerLine(i) )
					End If
			End Select

			if(dqText(i, dqHead) <> "_") Then
				dLine(i) = Temp
				DMDUpdate i
			End If
		End If
	Next

	if(deCount(0) = deCountEnd(0) ) and(deCount(1) = deCountEnd(1) ) and(deCount(2) = deCountEnd(2) ) Then

		if(dqTimeOn(dqHead) = 0) Then
			DMDFlush()
		Else
			if(BlinkEffect = True) Then
				DMDTimer.Interval = 10
			Else
				DMDTimer.Interval = dqTimeOn(dqHead)
			End If

			DMDTimer.Enabled = True
		End If
	Else
		DMDEffectTimer.Enabled = True
	End If
End Sub

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
	If TempStr = "" Then
		TempStr = Space(dCharsPerLine(id) )
	Else
		if(Len(TempStr)> Space(dCharsPerLine(id) ) ) Then
			TempStr = Left(TempStr, Space(dCharsPerLine(id) ) )
		Else
			if(Len(TempStr) <dCharsPerLine(id) ) Then
				TempStr = TempStr & Space(dCharsPerLine(id) - Len(TempStr) )
			End If
		End If
	End If
	ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
	dim i
	dim NumString

	NumString = CStr(abs(Num) )

	For i = Len(NumString) -3 to 1 step -3
		if IsNumeric(mid(NumString, i, 1) ) then
			NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1) ) + 48) & right(NumString, Len(NumString) - i)
		end if
	Next
	FormatScore = NumString
End function

Function CL(id, NumString)
	Dim Temp, TempStr
	Temp = (dCharsPerLine(id) - Len(NumString) ) \ 2
	TempStr = Space(Temp) & NumString & Space(Temp)
	CL = TempStr
End Function

Function RL(id, NumString)
	Dim Temp, TempStr
	Temp = dCharsPerLine(id) - Len(NumString)
	TempStr = Space(Temp) & NumString
	RL = TempStr
End Function

Function FL(id, aString, bString) 'fill line
	Dim tmp, tmpStr
	aString = LEFT(aString, dCharsPerLine(id))
	bString = LEFT(bString, dCharsPerLine(id))
	tmp = dCharsPerLine(id)- Len(aString)- Len(bString)
	If tmp <0 Then tmp = 0
	tmpStr = aString & Space(tmp) & bString
	FL = tmpStr
End Function

'**************
' Update DMD
'**************

Sub DMDUpdate(id)
	Dim digit, value
	If UseFlexDMD Then FlexDMD.LockRenderThread
	Select Case id
		Case 0 'top text line
			For digit = 0 to 19
				DMDDisplayChar mid(dLine(0), digit + 1, 1), digit
			Next
		Case 1 'bottom text line
			For digit = 20 to 39
				DMDDisplayChar mid(dLine(1), digit -19, 1), digit
			Next
		Case 2 ' back image - back animations
			If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "bkempty"
			Digits(40).ImageA = dLine(2)
			If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
	End Select
	If UseFlexDMD Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
	If achar = "" Then achar = " "
	achar = ASC(achar)
	Digits(adigit).ImageA = Chars(achar)
	If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
End Sub

'****************************
' JP's new DMD using flashers
'****************************

Dim Digits, Chars(255), BumpS(30)

DMDInit

Sub DMDInit
	Dim i
	Digits = Array(digit001, digit002, digit003, digit004, digit005, digit006, digit007, digit008, digit009, digit010, _
	digit011, digit012, digit013, digit014, digit015, digit016, digit017, digit018, digit019, digit020, _
	digit021, digit022, digit023, digit024, digit025, digit026, digit027, digit028, digit029, digit030, _
	digit031, digit032, digit033, digit034, digit035, digit036, digit037, digit038, digit039, digit040, _
	digit041)

	For i = 0 to 255:Chars(i) = "dempty":Next

	Chars(32) = "dempty"
	Chars(43) = "dplus"    '+
	Chars(46) = "ddot"     '.
	Chars(48) = "d0"       '0
	Chars(49) = "d1"       '1
	Chars(50) = "d2"       '2
	Chars(51) = "d3"       '3
	Chars(52) = "d4"       '4
	Chars(53) = "d5"       '5
	Chars(54) = "d6"       '6
	Chars(55) = "d7"       '7
	Chars(56) = "d8"       '8
	Chars(57) = "d9"       '9
	Chars(60) = "dless"    '<
	Chars(61) = "dequal"   '=
	Chars(62) = "dmore"    '>
	Chars(64) = "bkempty"  '@
	Chars(65) = "da"       'A
	Chars(66) = "db"       'B
	Chars(67) = "dc"       'C
	Chars(68) = "dd"       'D
	Chars(69) = "de"       'E
	Chars(70) = "df"       'F
	Chars(71) = "dg"       'G
	Chars(72) = "dh"       'H
	Chars(73) = "di"       'I
	Chars(74) = "dj"       'J
	Chars(75) = "dk"       'K
	Chars(76) = "dl"       'L
	Chars(77) = "dm"       'M
	Chars(78) = "dn"       'N
	Chars(79) = "do"       'O
	Chars(80) = "dp"       'P
	Chars(81) = "dq"       'Q
	Chars(82) = "dr"       'R
	Chars(83) = "ds"       'S
	Chars(84) = "dt"       'T
	Chars(85) = "du"       'U
	Chars(86) = "dv"       'V
	Chars(87) = "dw"       'W
	Chars(88) = "dx"       'X
	Chars(89) = "dy"       'Y
	Chars(90) = "dz"       'Z
	Chars(94) = "dup"      '^
	'    Chars(95) = '_
	Chars(96) = "d0a"  '0.
	Chars(97) = "d1a"  '1. 'a
	Chars(98) = "d2a"  '2. 'b
	Chars(99) = "d3a"  '3. 'c
	Chars(100) = "d4a" '4. 'd
	Chars(101) = "d5a" '5. 'e
	Chars(102) = "d6a" '6. 'f
	Chars(103) = "d7a" '7. 'g
	Chars(104) = "d8a" '8. 'h
	Chars(105) = "d9a" '9  'i

End Sub

'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

	If TypeName(MyLight) = "Light" Then

		If FinalState = 2 Then
			FinalState = MyLight.State 'Keep the current light state
		End If
		MyLight.BlinkInterval = BlinkPeriod
		MyLight.Duration 2, TotalPeriod, FinalState
	ElseIf TypeName(MyLight) = "Flasher" Then

		Dim steps

		' Store all blink information
		steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
		If FinalState = 2 Then                      'Keep the current flasher state
			FinalState = ABS(MyLight.Visible)
		End If
		MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

		' Start blink timer and create timer subroutine
		MyLight.TimerInterval = BlinkPeriod
		MyLight.TimerEnabled = 0
		MyLight.TimerEnabled = 1
		ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
	End If
End Sub


'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 10 colors: red, orange, amber, yellow...
'******************************************

Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white, base,lightgrey,midgrey,darkgrey

red = 10
orange = 9
amber = 8
yellow = 7
darkgreen = 6
green = 5
blue = 4
darkblue = 3
purple = 2
white = 1
base = 11
lightgrey=12
midgrey=13
darkgrey=14

Sub SetLightColor(n, col, stat)
	Select Case col
		Case red
			n.color = RGB(18, 0, 0)
			n.colorfull = RGB(255, 0, 0)
		Case orange
			n.color = RGB(18, 3, 0)
			n.colorfull = RGB(255, 64, 64)
		Case amber
			n.color = RGB(193, 49, 0)
			n.colorfull = RGB(255, 153, 0)
		Case yellow
			n.color = RGB(18, 18, 0)
			n.colorfull = RGB(240, 220, 0)
		Case darkgreen
			n.color = RGB(0, 8, 0)
			n.colorfull = RGB(0, 150, 0) '(0, 64, 0)
		Case green
			n.color = RGB(0, 18, 0)
			n.colorfull = RGB(0, 200, 100) '(0, 255, 0)
		Case blue
			n.color = RGB(0, 18, 18)
			n.colorfull = RGB(0, 255, 255) '0, 255, 255
		Case darkblue
			n.color = RGB(0, 8, 8)
			n.colorfull = RGB(0, 20, 230) '(0, 0, 255)
		Case purple
			n.color = RGB(128, 128, 255)	'128.0,128
			n.colorfull = RGB(3, 0, 240)
		Case white
			n.color = RGB(255, 252, 224)
			n.colorfull = RGB(193, 91, 0)
		Case white
			n.color = RGB(255, 252, 224)
			n.colorfull = RGB(193, 91, 0)
		Case base
			n.color = RGB(128, 128, 255)
			n.colorfull = RGB(255, 252, 224)
		Case lightgrey
			n.color = RGB(192, 192, 192)
		Case midgrey
			n.color = RGB(120, 120, 120)
		Case darkgrey
			n.color = RGB(50, 50, 50)
	End Select
	If stat <> -1 Then
		n.State = 0
		n.State = stat
	End If
End Sub

'*************************
' Rainbow Changing Lights
'*************************

Sub ResetAllLightsColor ' Called at a new game
'	SetLightColor LightLeftInlane,blue, -1
'	SetLightColor LightLeftEscape,orange, -1
'	SetLightColor LightRightInlane,blue, -1
'	SetLightColor Lightrightescape,orange, -1
'	SetLightColor LightShootAgain,red, -1
'	SetLightColor Light_ExtraBall,red, -1
'	SetLightColor Light_BallSaver,blue, -1

End sub



'*************************
' Rainbow Changing Lights
'*************************

Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

Sub StartRainbow(n)
'	set RainbowLights = n
'	RGBStep = 0
'	RGBFactor = 5
'	rRed = 255
'	rGreen = 0
'	rBlue = 0
'	RainbowTimer.Enabled = 1
End Sub


Sub StopRainbow(n)
'	Dim obj
'	RainbowTimer.Enabled = 0
'	RainbowTimer.Enabled = 0
'	For each obj in RainbowLights
'		SetLightColor obj, "white", 0
'	Next
End Sub


Sub RainbowTimer_Timer 'rainbow led light color changing
'	Dim obj
'	Select Case RGBStep
'		Case 0 'Green
'			rGreen = rGreen + RGBFactor
'			If rGreen > 255 then
'				rGreen = 255
'				RGBStep = 1
'			End If
'		Case 1 'Red
'			rRed = rRed - RGBFactor
'			If rRed < 0 then
'				rRed = 0
'				RGBStep = 2
'			End If
'		Case 2 'Blue
'			rBlue = rBlue + RGBFactor
'			If rBlue > 255 then
'				rBlue = 255
'				RGBStep = 3
'			End If
'		Case 3 'Green
'			rGreen = rGreen - RGBFactor
'			If rGreen < 0 then
'				rGreen = 0
'				RGBStep = 4
'			End If
'		Case 4 'Red
'			rRed = rRed + RGBFactor
'			If rRed > 255 then
'				rRed = 255
'				RGBStep = 5
'			End If
'		Case 5 'Blue
'			rBlue = rBlue - RGBFactor
'			If rBlue < 0 then
'				rBlue = 0
'				RGBStep = 0
'			End If
'	End Select
'	For each obj in RainbowLights
'		obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
'		obj.colorfull = RGB(rRed, rGreen, rBlue)
'	Next
End Sub

Sub RainbowTimer1_Timer 'rainbow led light color changing
'	Dim obj
'	Select Case RGBStep2
'		Case 0 'Green
'			rGreen2 = rGreen2 + RGBFactor2
'			If rGreen2 > 255 then
'				rGreen2 = 255
'				RGBStep2 = 1
'			End If
'		Case 1 'Red
'			rRed2 = rRed2 - RGBFactor2
'			If rRed2 < 0 then
'				rRed2 = 0
'				RGBStep2 = 2
'			End If
'		Case 2 'Blue
'			rBlue2 = rBlue2 + RGBFactor2
'			If rBlue2 > 255 then
'				rBlue2 = 255
'				RGBStep2 = 3
'			End If
'		Case 3 'Green
'			rGreen2 = rGreen2 - RGBFactor2
'			If rGreen2 < 0 then
'				rGreen2 = 0
'				RGBStep2 = 4
'			End If
'		Case 4 'Red
'			rRed2 = rRed2 + RGBFactor2
'			If rRed2 > 255 then
'				rRed2 = 255
'				RGBStep2 = 5
'			End If
'		Case 5 'Blue
'			rBlue2 = rBlue2 - RGBFactor2
'			If rBlue2 < 0 then
'				rBlue2 = 0
'				RGBStep2 = 0
'			End If
'	End Select
'	For each obj in RainbowLights2
'		obj.color = RGB(rRed2 \ 10, rGreen2 \ 10, rBlue2 \ 10)
'		obj.colorfull = RGB(rRed2, rGreen2, rBlue2)
'	Next
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo

	If bGameInPLay = True Then: vpmtimer.addtimer 2200,  "Player1Now'": Exit Sub


	Dim tmp
	'info goes in a loop only stopped by the credits and the startkey
	If Score(1)Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 10000, False, ""
	End If
	If Score(2)Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 10000, False, ""
	End If
	If Score(3)Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 10000, False, ""
	End If
	If Score(4)Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 10000, False, ""
	End If
	DMD "", CL(1, "  GAME OVER"), "", eNone, eBlink, eNone, 700, False, ""
	If bFreePlay Then
		DMD CL(0, "FREE PLAY"), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
	Else
		If Credits > 0 Then
			DMD CL(0, "      CREDITS " & Credits), CL(1, "   PRESS START"), "", eNone, eBlink, eNone, 400, False, ""
		Else
			DMD CL(0, "      CREDITS " & Credits), CL(1, "   INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
		End If
		If bGameInPlay=True Then:DMDFlush:DMD "", "", "bkborder", eNone, eNone, eNone, 100, False, "":vpmtimer.addtimer 2200,"Player1Now'"
		End If 


	DMD "", "", "DMD_Blank", eNone, eNone, eNone, 100, False, "" 'blank
	DMD CL(0, " "), CL(1, "    HEY RALPH"), "DMD_King2BG", eBlink, eNone, eNone, 3000, True, ""
	DMD CL(0, " "), CL(1, "   OFF THE COUCH"), "DMD_King2BG", eBlink, eNone, eNone, 3000, True, ""
	DMD CL(0, "     WE HAVE A"), CL(1, "  CHICKEN PROBLEM"), "DMD_Title25", eNone, eNone, eNone, 3000, True, ""
	DMD CL(0, "         RUSSTYT "), CL(1, "    PRESENTS"), "DMD_RusstyTBG", eNone, eBlink, eNone, 3000, True, ""
	DMD CL(0, " "), CL(1, "     GAME OF DOG"), "DMD_Ralph5BG", eBlink, eNone, eNone, 3000, True, ""
	DMD CL(0, " "), CL(1, "   THATS RIGHT BUD"), "DMD_King2BG", eBlink, eNone, eNone, 3000, True, ""
	DMD CL(0, " "), CL(1, "   DOG NOT DOGS"), "DMD_King2BG", eBlink, eNone, eNone, 3000, True, ""
	DMD CL(0, "        ITS ABOUT "), CL(1, "    RALPH"), "DMD_Ralph5BG", eNone, eNone, eNone, 3000, True, ""	
	DMD CL(0, "       THE SCRUFFY"), CL(1, "    MUTT"), "DMD_Ralph5BG", eNone, eNone, eNone, 3000, True, ""
	DMD CL(0, "         YOU"), CL(1, "      KNOW HIM"), "DMD_Ralph5BG", eBlink, eNone, eNone, 3000, True, ""	
	DMD CL(0, "         WHEN YOU "), CL(1, "     SEE HIM"), "DMD_Ralph5BG", eBlink, eNone, eNone, 3000, True, ""
	DMD CL(0, "        GIVE HIM"), CL(1, "     A PAT"), "DMD_Ralph5BG", eNone, eNone, eNone, 3000, True, ""
	DMD CL(0, "       THANK YOU"), CL(1, "   JP SALAS"), "DMD_JPBG", eNone, eNone, eNone, 3000, True, ""
	DMD CL(0, " PHYSICS AND FLEEP"), CL(1, "BY BURGER"), "DMD_Burger", eNone, eNone, eNone, 3000, True, ""
	DMD CL(0, "      DOF BY"), CL(1, "   OUTHERE"), "DMD_Outhere", eNone, eNone, eNone, 3000, True, ""
	DMD CL(0, "     THANK YOU"), CL(1, " MARTIN MCKENNA"), "bkempty", eNone, eNone, eNone, 3000, True, ""
	DMD CL(0, "  THE BOY WHO LIVED"), CL(1, "   WITH DOGS   "), "bkempty", eNone, eNone, eNone, 3200, True, ""
	DMD CL(0, ""), CL(1, " "), "DMD_MartinMcKenna2", eBlink, eNone, eNone, 3200, True, ""
	DMD CL(0, " FEATURING KITTY"), CL(1, "CANON BALL"), "DMD_KittyBoom3BG", eNone, eNone, eNone, 3000, True, ""
	DMD CL(0, " "), CL(1, " "), "DMD_KittyBang", eBlink, eNone, eNone, 3000, True, ""

	DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
	DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD Space(dCharsPerLine(0)), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""

End Sub


'**************************************************************************************
'  ATTRACT MODE
'************************************************************************************* 
' 


Sub StartAttractMode()
'	PlasticsOnFlasher.Visible=False
If 	bGameFinished=1 Then 
	PlaySong "m_End":bGameFinished=0
Else
PlaySong "m_TheCreek"
End If

'	vpmTimer.addtimer 1500, " AttractCallout'"
	bAttractMode = True
	StartLightSeq
	ShowTableInfo
AttractBackGlassLightTimer.Enabled=1

End Sub

Sub AttractCallout

End Sub





Sub StopAttractMode()
'	PlasticsOnFlasher.Visible=True
'	SetLightColor Light_TC2,yellow, -1:SetLightColor Light_TC3,yellow, -1:SetLightColor Light_TC4,yellow, -1
	bAttractMode = False

	AttractBackGlassLightTimer.Enabled=0
	AttractCount=0
	AllBackGlassLightsOn

	LightSeqAttract.StopPlay
	LightSeqAttract2.StopPlay
	'-----------------------------------
	StopRainbow alights         
	'--------------------------------
	ResetAllLightsColor
'	SetLightColor Overlay, midgrey, -1
'	SetLightColor PiratesLife, lightgrey, -1
'	OverlayPiratesLife.Visible=False

End Sub




Sub StartLightSeq()

	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqUpOn, 30, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqDownOn, 25, 1

	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqStripe1VertOn, 15, 2
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqStripe2VertOn, 10, 2
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqCircleOutOn, 15, 2

	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 15
	LightSeqAttract.Play SeqCircleOutOn, 15, 3
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqRightOn, 50, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqLeftOn, 35, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqRightOn, 50, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqLeftOn, 40, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqRightOn, 40, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqLeftOn, 20, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqRightOn, 40, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqCircleOutOn, 15, 3
	LightSeqAttract.UpdateInterval = 100
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 100
	LightSeqAttract.Play SeqRightOn, 25, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.Play SeqStripe2VertOn, 50, 2
	LightSeqAttract.UpdateInterval = 20

	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqStripe1VertOn, 50, 2
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqStripe1VertOn, 50, 3
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqRightOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqStripe2VertOn, 50, 3
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqRightOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqStripe1VertOn, 25, 3
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqStripe2VertOn, 25, 3
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqUpOn, 15, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 15, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqRightOn, 15, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 15, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqRightOn, 15, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 15, 1

End Sub

Sub LightSeqAttract_PlayDone()
	StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
	LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqSkillshot_PlayDone()
	LightSeqSkillshot.Play SeqAllOff
End Sub

Sub StartMajorAwardSequence
	GiOff
	vpmtimer.addtimer 3000, "GiOn'"
	MajorAwardSequence.UpdateInterval = 10
	MajorAwardSequence.Play SeqBlinking,, 14,100
End Sub

Sub StartDogSequence
	GiOff
	vpmtimer.addtimer 3000, "GiOn'"
	KingDogMinorSequence.UpdateInterval = 10
	KingDogMinorSequence.Play SeqCircleOutOn, 15, 6
	KingDogMinorSequence.UpdateInterval = 10
	KingDogMinorSequence.Play SeqUpOn, 15, 2
End Sub

Sub StartOfGameLightSequence
	GameStartLightsSequence.UpdateInterval = 10
	GameStartLightsSequence.Play SeqUpOn, 15, 8
End Sub
'***********************************************
'Flashers
'***************************************************
' generic award flasher
dim AwardFlash:AwardFlash=0
Sub AwardFlasherTimer_Timer
	AwardFlash=AwardFlash+1
	Select Case AwardFlash
		case 1: AwardFlasher.visible = 1
		case 3:	AwardFlasher.visible = 0
		case 5:	AwardFlasher.visible = 1
		case 7:	AwardFlasher.visible = 0
		case 9:	AwardFlasher.visible = 1
		case 11:AwardFlasher.visible = 0
		case 13:AwardFlasher.visible = 1
		case 15:AwardFlasher.visible = 0
		case 17:AwardFlasher.visible = 1
		case 19:AwardFlasher.visible = 0
		case 21:AwardFlasher.visible = 1
		case 23:AwardFlasher.visible = 0
		case 25:AwardFlasher.visible = 1
		case 26:AwardFlasher.visible = 0:AwardFlasherTimer.Enabled=0:AwardFlash=0
	End Select
End Sub	

dim SmallAwardFlash:SmallAwardFlash=0
Sub SmallAwardFlasherTimer_Timer
	SmallAwardFlash=SmallAwardFlash+1
	Select Case SmallAwardFlash
		case 1: AwardFlasher.visible = 1
		case 3:	AwardFlasher.visible = 0
		case 5:	AwardFlasher.visible = 1
		case 7:	AwardFlasher.visible = 0
		case 9:	AwardFlasher.visible = 1
		case 11:AwardFlasher.visible = 0
		case 13:AwardFlasher.visible = 1
		case 15: AwardFlasher.visible = 0:SmallAwardFlasherTimer.Enabled=0:SmallAwardFlash=0
	End Select
End Sub	











'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

Sub Player1Now
	DMD "", CL(1, "PLAYER 1 BALL 1 " ), "", eNone, eNone, eNone, 3000, True, ""
End Sub

Sub Game_Init() 'called at the start of a new game
	Dim i, j
	bExtraBallWonThisBall = False
'PlaySong "m_mrbluesky"
ChangeSong : debug.print "ChangeSong GameInitiation 3657"
	'	If CalloutActive=False Then PlaySound"CO_RaiseTheSails":CalloutActive=True: CalloutTimer.Enabled=True:End If
		StartOfGameLightSequence

	For i = 0 to 4
		SkillshotValue(i) = 1000000 ' increases by 1000000 each time it is collected

		ModesCompleted(i)=0
		BonusMultiplier(i)=0

		bGameEnded(i)=0
	Next
	lrflashtime.Enabled = False
	bExtraBallWonThisBall = False
	MechTilt = 0
	bMechTiltJustHit = False
	bFlippersEnabled = True
	cFlipperPressed=False
	StopAttractMode
	ResetStartofGameVariables()
	TurnOnStartOfGameLights
If CabinetMode=1 Then  debug.print "CabinetMode"
If RenderingMode=0 Then 	debug.print "renderingMode=0"
If RenderingMode=1 Then 	debug.print "renderingMode=1"
End Sub

'*****************************************************Not sure how to do this


'***********************************************************************************************************************

Sub StopEndOfBallMode() 'this sub is called after the last ball is drained
'	If bExtraBallWonThisBall = True Then:Exit Sub
	SaveLightStates
	clearlights
End Sub


Sub ResetNewBallVariables()
	ResetNewBallLights
	vpmtimer.addtimer 500, "ResetModesAndAwards'"	
	bFlippersEnabled = True
	ChangeSong:: debug.print "ChangeSongResetNewBallVariables 3698"

End Sub

Sub ResetNewBallLights()                                 'turn on or off the needed lights before a new ball is released
		LoadLightStates'LoadLightStates 'ensure the multiplier is displayed right
End Sub

Sub ResetStartofGameVariables()

	GiOn
'	kickbacklg.open=True
	vpmTimer.AddTimer 1000, "ResetModesAndAwards'"
	BonusMultiplierActive(CurrentPlayer)=0
	bBallSaverReady = True	
	HolesComplete(CurrentPlayer)=0
	ChasesComplete(CurrentPlayer)=0
	FiFiLevel(CurrentPlayer)=0
	FiFiMultiballReady(CurrentPlayer)=0
	CatsLossCount(CurrentPlayer)=0
	SuperSheepCount(CurrentPlayer)=0
	KingDogCount(CurrentPlayer)=0
	CatsSorted(CurrentPlayer)=0
	CatCount(CurrentPlayer)=0
	CatMultiballReady(CurrentPlayer)=0
	SheepCount(CurrentPlayer)=0
	SheepSorted(CurrentPlayer)=0
	SpikeReady(CurrentPlayer)=0
	FastScoreActive=0
	FastScoreKing(CurrentPlayer)=0
	KingDogRamps(CurrentPlayer)=0
	KingDogMultiballReady(CurrentPlayer)=0
	KingGAchieved(CurrentPlayer)=0
	KingNAchieved(CurrentPlayer)=0
	KingIAchieved(CurrentPlayer)=0
	KingKAchieved(CurrentPlayer)=0
	FastScoreFiFi(CurrentPlayer)=0
	FastScoreDoctorDog(CurrentPlayer)=0
	FastScoreSheep(CurrentPlayer)=0
	FastScoreCat(CurrentPlayer)=0
	SheepMultiballReady(CurrentPlayer)=0
	HolesAreDug(CurrentPlayer)=0
	BonoAdvance(CurrentPlayer)=0
	BonoMetersCompleted(CurrentPlayer)=0
	PopValue0=1:PopValue1=0:PopValue2=0:PopValue3=0
	DigBonusLevel(CurrentPlayer)=0
	FiFiActive(CurrentPlayer)=0
	ChaseBonusLevel(CurrentPlayer)=0
	GreyKangarooCount(CurrentPlayer)=0
	WhizzerCount(CurrentPlayer)=0
	AllBetterActive(CurrentPlayer)=0
	VetCount(CurrentPlayer)=0
	MysteryActive(CurrentPlayer)=0
	SelectMode=0
	ModeActive=0
	NewSong=0
	NewSong2=0
	LuckyDogActive=1	
	CatsLooseActive=0:KingDogActive(CurrentPlayer)=0: SuperSheepActive=0
	NextMode1(CurrentPlayer)=1:NextMode2(CurrentPlayer)=0:NextMode3(CurrentPlayer)=0:NextMode4(CurrentPlayer)=0
	StopAngrySheep
	SheepDown
	CatSelect=0
	VetChat=0
	BoneOMeterAttractTimer.Enabled=0
	vpmtimer.addtimer 200, "ResetBonoMeter'"
	BackGlassDogsRotateTimer.Enabled=1
	StopFiFi
	ResetKingLights
	Spike.TransZ=-130
	KenelSledgeSelect=0
	ThroughSledgeSelect=0
	CalloutActive=0
End Sub


Sub ResetModesAndAwards

	SquirrelSpinnerWall.IsDropped=True
	BallFinished=False
	StopCats
	kickbackleftdisabled:kickbackrightdisabled
'	kickbackleftenabled:kickbackrightenabled
	KingDogMultiballActive=0
	CatMultiballActive=0
	CatCount(CurrentPlayer)=0:debug.print "CatCount(CurrentPlayer)=0"
	ResetVetTargets
	RightSideDiverterUsed=0
	StopCats
	StopAngrySheep
	SuperSheepCount(CurrentPlayer)=0
	FastScoreActive=0
	FastScoreKing(CurrentPlayer)=0
	FastScoreFiFi(CurrentPlayer)=0
	FastScoreDoctorDog(CurrentPlayer)=0
	FastScoreSheep(CurrentPlayer)=0
	StopKingDog
	KingDogDown:debug.print "KingDogDown"
	SpikeJPCount=0
	ResetFastScore
	SledgeSelect=0
If 	SpikeMultiballActive=1 Then 
	Spike.TransZ=-130
	SpikeMultiballActive=0
	KingDogMultiballReady(CurrentPlayer)=0:CatMultiballReady(CurrentPlayer)=0:SheepMultiballReady(CurrentPlayer)=0:FiFiMultiballReady(CurrentPlayer)=0
	LightKingMultiballReady.State=0:LightCatMultiballReady.State=0:LightSheepMultiballReady.State=0:LightFiFiMultiballReady.State=0
	KingDogActive(CurrentPlayer)=0
	LightKingMultiballReady.State=0
	KingDogMultiballReady(CurrentPlayer)=0
	ResetMultiballLights
End If
	vpmtimer.addtimer 1000, "CheckDigTargetsNewBall'"
End Sub

Sub TurnOffPlayfieldLights()
	Dim a
	For each a in aLights
		a.State = 0
	Next
End Sub

Sub TurnOnStartOfGameLights
	GiOn:PlaySoundAt "fx_relay", KickerModes
'	LightLeftOL.State=0:LightLeftIL.State=1:LightRightOL.State=0:LightRightIL.State=1
	LightDig1.State=2:LightDig2.State=2:LightDig3.State=2:LightDig4.State=2:LightDig5.State=2
	LightG.State=2:LightBonus2X.State=2:LightLocksActivate.State=2:LightZoomies1.State=2
	LightTarget25.State=1:LightTarget26.State=1
	LightSquirrel1.State=2:LightSquirrel2.State=2:LightSquirrel3.State=2:LightSquirrel4.State=2:LightSquirrel5.State=2:LightSquirrel6.State=2:LightTarget25.State=2:LightTarget26.State=2
	SquirrelLightsActive=True
	Light_BallSaver.State=0:LightShootAgain.State=0
	LightAngrySheep1.State=2
	ResetChaseBonusLights
	ResetDigBonusLights
	LightVari1.State=0:LightVari2.State=0:LightVari3.State=0
	LightMystery.State=0
	LightFastScore1.State=0:LightFastScore2.State=0:LightFastScore3.State=0:LightFastScore4.State=0:LightFastScore5.State=0
	LuckyDogActive=1 :LightAward.State=2
	vpmtimer.addtimer 500, "ResetSquirrelLights'"
	LightKingMultiballReady.State=0:LightFiFiMultiballReady.State=0:LightCatMultiballReady.State=0:LightSheepMultiballReady.State=0
	LightTree1.State=2:LightTree2.State=2:LightTree3.State=2
LightBono8.State=0:LightBono1.State=0:LightBono2.State=0:LightBono3.State=0:LightBono4.State=0:LightBono5.State=0:LightBono6.State=0:LightBono7.State=0
End Sub

Sub UpdateSkillShot() 

End Sub	



'***********************************
'Light Save & restore- Thanks JP
'***********************************
Dim MyLightStates(4,200)

Sub SaveLightStates
	Dim i, tmp
	i = 0
	For each tmp in aLights
		MyLightStates(CurrentPlayer,i) = tmp.State
		i = i + 1
	Next

End Sub

Sub LoadLightStates



	Dim i, tmp
	i = 0
	For each tmp in aLights
		tmp.State = MyLightStates(CurrentPlayer,i)
		i = i + 1
	Next
'	If l38.State=1 Then:StartPlanetChaos : End If

End Sub

Sub clearlights
	dim i
	for each i in aLights
		i.State = 0
	next
End Sub




'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
	Dim bulb
	For each bulb in GI
		SetLightColor bulb, col,1
	Next
End Sub

'*************************************************************
'GIUpdateTimer is used if captive balls are used in the game. 
'This sub is not used in Pirates Life
'*************************************************************


Sub GIUpdateTimer_Timer
	Dim tmp, obj
	tmp = Getballs
	If UBound(tmp) <> OldGiState Then
		OldGiState = Ubound(tmp)
		If UBound(tmp) = 3 Then 'we have 4 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
			'GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
		Else
			'Gion
		End If
	End If
End Sub
'**************************************************************


Sub GiOn
	PlaySoundAt "Relay_GI_On", KickerModes
	TableDarkFlasher.Visible=0
'	PlasticsDark.Visible=0
'	ApronDark.Visible=0
	Dim bulb 
	Dim obj

	PlaySound "Relay_GI_On"

	For each bulb in aGiLights
		bulb.State = 1

	Next
	'Set GI light clours
'	SetLightColor gi1, 	blue, -1
'	gi1.intensity=2
'	SetLightColor gi2, 	white, -1
'	gi2.intensity=2
'	SetLightColor gi3, 	white, -1
'	gi3.intensity=2
'	SetLightColor gi4, 	blue, -1
'	gi4.intensity=6

End Sub

Sub GiOff
	PlaySoundAt "Relay_GI_Off", KickerModes
	TableDarkFlasher.Visible=1
'	PlasticsDark.Visible=1

'	ApronDark.Visible=1
	Dim bulb
	Dim obj
	PlaySound "Relay_GI_Off"

	For each bulb in aGiLights
					bulb.State = 0
	Next
End Sub



' GI & light sequence effects


Sub GiEffect(n)
	Select Case n
		Case 0 all off
			LightSeqGi.Play SeqAlloff
		Case 1 'all blink
			LightSeqGi.UpdateInterval = 4
			LightSeqGi.Play SeqBlinking, , 5, 100
		Case 2 random
			LightSeqGi.UpdateInterval = 10
			LightSeqGi.Play SeqRandom, 5, , 1000
		Case 3 upon
			LightSeqGi.UpdateInterval = 4
			LightSeqGi.Play SeqUpOn, 5, 1
		Case 4  left-right-left
			LightSeqGi.UpdateInterval = 5
			LightSeqGi.Play SeqLeftOn, 10, 1
			LightSeqGi.UpdateInterval = 5
			LightSeqGi.Play SeqRightOn, 10, 1
	End Select
End Sub

Sub LightEffect(n)
	Select Case n
		Case 0  all off
			LightSeqInserts.Play SeqAlloff
		Case 1 all blink
			LightSeqInserts.UpdateInterval = 4
			LightSeqInserts.Play SeqBlinking, , 5, 100
		Case 2 random
			LightSeqInserts.UpdateInterval = 10
			LightSeqInserts.Play SeqRandom, 5, , 1000
		Case 3 upon
			LightSeqInserts.UpdateInterval = 4
			LightSeqInserts.Play SeqUpOn, 10, 1
		Case 4  left-right-left
			LightSeqInserts.UpdateInterval = 5
			LightSeqInserts.Play SeqLeftOn, 10, 1
			LightSeqInserts.UpdateInterval = 5
			LightSeqInserts.Play SeqRightOn, 10, 1
		Case 5 random
			LightSeqbumper.UpdateInterval = 4
			LightSeqbumper.Play SeqBlinking, , 5, 10
		Case 6 random
			LightSeqRSling.UpdateInterval = 4
			LightSeqRSling.Play SeqBlinking, , 5, 6
		Case 7 random
			LightSeqLSling.UpdateInterval = 4
			LightSeqLSling.Play SeqBlinking, , 5, 6
		Case 8 random
			LightSeqBack.UpdateInterval = 4
			LightSeqBack.Play SeqBlinking, , 5, 6
		Case 12 random
			LightSeqlr.UpdateInterval = 4
			LightSeqlr.Play SeqBlinking, , 5, 10
	End Select
End Sub




'****************************
'  SECONDARY HIT EVENTS
'******************************

Dim RStep, R3Step, Lstep,L4Step

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotRight(sling1)
	DOF 104, DOFPulse
	RSling.Visible = 0
	RSling1.Visible = 1
	sling1.rotx = 20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	AddScore 0
End Sub

Sub RightSlingShot_Timer
	RightSlingGiOff
	Select Case RStep
		Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
		Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0: RightSlingShot.TimerEnabled = 0:RightSlingGiOn
	End Select
	RStep = RStep + 1
End Sub

Sub RightSlingGiOff
	gi9.State=0:gi10.State=0:gi44.State=0:gi47.State=0
End Sub

Sub RightSlingGiOn
	gi9.State=1:gi10.State=1:gi44.State=1:gi47.State=1
End Sub

Sub RightSlingShot3_Slingshot
	RS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotRight(sling3)
	DOF 104, DOFPulse
	RSling3.Visible = 0
	RSling3a.Visible = 1
	sling3.rotx = 20
	R3Step = 0
	RightSlingShot3.TimerEnabled = 1
	AddScore 0
End Sub

Sub RightSlingShot3_Timer
	Select Case RStep
		Case 3:RSLing3.Visible = 0:RSling3a.Visible = 1:sling3.rotx = 10
		Case 4:RSling3b.Visible = 0:RSLing3.Visible = 1:sling3.rotx = 0: RightSlingShot3.TimerEnabled = 0
	End Select
	R3Step = R3Step + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotLeft(sling2)
	DOF 103, DOFPulse
	LSling.Visible = 0
	LSling1.Visible = 1
	sling2.rotx = 20
	LStep = 0
	LeftSlingShot.TimerEnabled = 1

	AddScore 0
End Sub

Sub LeftSlingShot_Timer
	LeftSlingGiOff
	Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
		Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:LeftSlingGiOn
	End Select
	LStep = LStep + 1
End Sub

Sub LeftSlingGiOff
	gi1.State=0:gi2.State=0:gi11.State=0:gi12.State=0:gi46.State=0
End Sub

Sub LeftSlingGiOn
	gi1.State=1:gi2.State=1:gi11.State=1:gi12.State=1:gi46.State=1
End Sub

Sub LeftSlingShot4_Slingshot
	LS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotLeft(sling4)
	DOF 103, DOFPulse
	LSling4.Visible = 0
	LSling4a.Visible = 1
	sling4.rotx = 20
	L4Step = 0
	LeftSlingShot4.TimerEnabled = 1

	AddScore 0
End Sub

Sub LeftSlingShot4_Timer
	Select Case LStep
		Case 3:LSLing4.Visible = 0:LSLing4a.Visible = 1:sling4.rotx = 10
		Case 4:LSling4a.Visible = 0:LSling4.Visible = 1:sling4.rotx = 0:LeftSlingShot4.TimerEnabled = 0
	End Select
	L4Step = L4Step + 1
End Sub

'************************************
'  MAIN SHOTS - PRIMARY HIT EVENTS
'************************************

'*********************************
'  BUMPERS
'**********************************


'*************************
'Bumper2
'*************************
Dim Cup3Pos



'*************************
'Bumper3
'*************************

Sub Bumper3_Hit

	If NOT Tilted Then
		DOF 107,DOFPulse
			RandomSoundBumperBottom Bumper3
			LightSeqBumper3.UpdateInterval = 10
			LightSeqBumper3.Play SeqBlinking,  2, 100
			BumperSequence.enabled = 1
			Movecup3

'		If PopValue0=1 Then AddScore 100: End If
'		If PopValue1=1 Then AddScore 1000: End If
'		If PopValue2=1 Then AddScore 5000: End If
'		If PopValue3=1 Then AddScore 10000: End If
	End If
End Sub


Sub Movecup3
    Cup3Pos = 6
    Cup3Timer.Enabled =1
End Sub

Sub Cup3Timer_Timer
    Cup3.TransY = Cup3Pos
    If Cup3Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Cup3Pos < 0 Then
        Cup3Pos = ABS(Cup3Pos)- 1
    Else
        Cup3Pos = - Cup3Pos + 1
    End If
End Sub

'*****BoosterKicker*****

Sub KickerBooster_Hit
	DOF 136,DOFPulse
	PlaySoundAt "fx_kicker" , KickerBooster
	KickerBooster.Kick 125, 30

End Sub
		

'****************************************************************************************

Sub KickerCatch2_hit()
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer
	SoundSaucerLock
	PlaySoundAt "fx_kicker_catch", UpkickerCatch
	vpmtimer.addtimer 1500, "UpkickerRelease2'"
	KickerCatch2GiOff
	If AllBetterActive(CurrentPlayer)=1 Then VetAward:AllBetterActive(CurrentPlayer)=0:ResetVetTargets
End Sub

Sub KickerCatch2GiOff
	gi23.State=0: gi30.State=0:gi40.State=0: gi50.State=0:gi56.State=0
End Sub

Sub UpkickerRelease2
	'DOF 116,DOFPulse
	GiOn
	SoundSaucerKick 1, KickerCatch2
	PlaySoundAt SoundFXDOF("Popper" , 116, DOFPulse, DOFContactors),KickerCatch2
	KickerCatch2.kick   0, 25, 1.56
	gi23.State=1:gi30.State=1:gi40.State=1: gi50.State=1:gi56.State=1
End Sub


Sub TargetVet1_Hit()
	TargetVet1.IsDropped=1
	SoundDropTargetDrop (TargetVet1)
	LightZoomies1.State=1:LightZoomies2.State=2
	If FastScoreActive=0 Then AddScore 10000
	If FastScoreActive=1 Then AddScore 50000
	DoctorDog
End Sub

Sub TargetVet2_Hit()
	TargetVet2.IsDropped=1
	SoundDropTargetDrop (TargetVet2)
	LightZoomies2.State=1:LightZoomies3.State=2
	If FastScoreActive=0 Then AddScore 20000
	If FastScoreActive=1 Then AddScore 100000
	DoctorDog

End Sub


Sub TargetVet3_Hit()
	TargetVet3.IsDropped=1
	SoundDropTargetDrop (TargetVet3)
	LightZoomies1.State=2:LightZoomies2.State=2:LightZoomies3.State=2
	If FastScoreActive=0 Then AddScore 30000
	If FastScoreActive=1 Then AddScore 200000
	DoctorDog
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlayVetCallout
	AllBetterActive(CurrentPlayer)=1
End Sub

Sub ResetVetTargets
	DOF 109,DOFPulse
	TargetVet1.IsDropped=0:TargetVet2.IsDropped=0:TargetVet3.IsDropped=0
	LightZoomies1.State=2:LightZoomies2.State=0:LightZoomies3.State=0
End Sub


Sub DoctorDog
	Debug.print "VetProblem"
	Dim VetProblem
	VetProblem = int(rnd*7)
	Select Case VetProblem
		Case 1: DMD CL(0, "   YOU HAVE"), CL(1, "WORMS RALPH"), "DMD_DogtorBG", eBlink, eNone, eNone, 3000, True, ""
		Case 2:DMD CL(0, "    YOU HAVE "), CL(1, "FLEAS RALPH  "), "DMD_DogtorBG", eBlink, eNone, eNone, 3000, True, ""
		Case 3:DMD CL(0, "   OH NO"), CL(1, " VACCINATIONS"), "DMD_DogtorBG", eBlink, eNone, eNone, 3000, True, ""
		Case 4:DMD CL(0, "    OUCH"), CL(1, "STITCHES"), "DMD_DogtorBG", eBlink, eNone, eNone, 3000, True, ""
		Case 5:DMD CL(0, "ROTTEN RUBBISH"), CL(1, "SORE TUMMY"), "DMD_DogtorBG", eBlink, eNone, eNone, 3000, True, ""
		Case 6:DMD CL(0, " GOT THE RUNS"), CL(1, "BOOFHEAD"), "DMD_DogtorBG", eBlink, eNone, eNone, 3000, True, ""
		Case 6:DMD CL(0, "   SORE PAWS"), CL(1, "SILLY BILLY"), "DMD_DogtorBG", eBlink, eNone, eNone, 3000, True, ""
				
	End Select

End Sub

Sub PlayVetCallout
	VetChat=VetChat+1
	Select Case VetChat
		Case 1:PlaySound "CO_IfYouGoingRoundFighting"
		Case 2:PlaySound "CO_YouveGotWorms"
		Case 3:PlaySound "CO_Vaccinations"
		Case 4:PlaySound "CO_AllStichedUp"
		Case 5:PlaySound "CO_SoreTummy"
		Case 6:PlaySound "CO_YouveGotFleasRalph"
		Case 7:PlaySound "CO_NoKenelCough":VetChat=0
	End Select


End Sub


Sub VetAward
DMD CL(0, "  ALL BETTER"), CL(1, "RALPH"), "DMD_DOG2BG", eBlink, eNone, eNone, 3000, True, ""
	VetCount(CurrentPlayer)=VetCount(CurrentPlayer)+1
	Select Case VetCount(CurrentPlayer)
		Case 1:	If FastScoreDoctorDog(CurrentPlayer)=0 Then AddScore 300000
				If FastScoreDoctorDog(CurrentPlayer)=1 Then AddScore 600000
		Case 2:	If FastScoreDoctorDog(CurrentPlayer)=0 Then AddScore 400000
				If FastScoreDoctorDog(CurrentPlayer)=1 Then AddScore 800000
		Case 3:	If FastScoreDoctorDog(CurrentPlayer)=0 Then AddScore 1000000:AdvanceBonoMeter:VetCount(CurrentPlayer)=0
				If FastScoreDoctorDog(CurrentPlayer)=1 Then AddScore 2000000:AdvanceBonoMeter:VetCount(CurrentPlayer)=0
	End Select		
End Sub



Sub ResetVetLights
	LightZoomies1.State=2:LightZoomies2.State=0:LightZoomies3.State=0
End Sub



Sub SpinnerSpike_Spin
	DOF 120,DOFPulse
	SoundSpinner SpinnerSpike
	If Not Tilted Then
		AddScore 1000
	End If
End Sub


Sub KickerDogLock_Hit()
		GiOff
		ObjLevel(3) = 1 : FlasherFlash3_Timer
		ObjLevel(4) = 1 : FlasherFlash4_Timer
		SoundSaucerLock
		SpikeBark
	If SpikeMultiballActive=1 Then 
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:vpmtimer.addtimer 3000, "RalphKenelSledge'"
		AwardSpikeJackPot
		BackGlassMiniBlinkTimer.Enabled=1
		DMD CL(0, "KICKING BUTT"), CL(1, "GO DOG GO"), "DMD_SpikeBG", eBlink, eNone, eNone, 3000, True, ""
		AddScore 50000
		vpmtimer.addtimer 200, "KickerDogLockKick'":debug.print "200"
	End If

	If SpikeMultiballActive=0 And LightMultiballReady.State=2 Then 
		CheckKickerDogStatus
		vpmtimer.addtimer 2000, "KickerDogLockKick'"
		debug.print "2000"
		BackglassDogsSpinTimer.Enabled=1
	End If
	If SpikeMultiballActive=0 And LightMultiballReady.State<2 Then 
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:vpmtimer.addtimer 3000, "RalphThroughSledge'"
		CheckKickerDogStatus
		vpmtimer.addtimer 1000, "KickerDogLockKick'"
		debug.print "1000"
		BackGlassMiniBlinkTimer.Enabled=1
	End If

End Sub

Sub RalphKenelSledge
	KenelSledgeSelect=KenelSledgeSelect+1
	Select Case KenelSledgeSelect
		Case 1: PlaySound "CO_MrZoomie"
		Case 2: PlaySound "CO_SmackaDoodyDoodle"
		Case 3:PlaySound "CO_StumpyBoy"
		Case 4:PlaySound "CO_SpikeYouGotFleas"
		Case 5: PlaySound "CO_StumpyLegs":KenelSledgeSelect=0
	End Select
End Sub


Sub RalphThroughSledge
	ThroughSledgeSelect=ThroughSledgeSelect+1
	Select Case ThroughSledgeSelect
		Case 1: PlaySound "CO_ZippidyDangaDangDo"
		Case 2: PlaySound "CO_FreeAsABird"
		Case 3:PlaySound "CO_YouveBeenRalphed"
		Case 4:PlaySound "CO_OoAWeeWah"
		Case 5: PlaySound "CO_HereIComeSpikeyBoy":ThroughSledgeSelect=0
	End Select
End Sub
Sub AwardSpikeJackPot
	SpikeJPCount=SpikeJPCount+1
	Select Case SpikeJPCount
		Case 1:AwardJackpot
		Case 2: AwardJackpot2
		Case 3:AwardJackpot3
		Case 4: AwardSuperJackpot:SpikeJPCount=0
	End Select
End Sub

Sub CheckKickerDogStatus
	If LightMultiballReady.State=2 Then 
		AwardMultiballDMD
		LightFastScore4.State=2
		BackGlassDogsBlinkTimer.Enabled=1		
		
	If KingDogMultiballReady(CurrentPlayer)=1 Then AwardKingDMD:FastScoreKing(CurrentPlayer)=1:LightFastScore2.State=2

	If CatMultiballReady(CurrentPlayer)=1 Then AwardCatDMD:FastScoreCat(CurrentPlayer)=1:LightFastScore1.State=2

	If SheepMultiballReady(CurrentPlayer)=1 Then AwardSheepDMD:FastScoreSheep(CurrentPlayer)=1:LightFastScore5.State=2

	If FiFiMultiballReady(CurrentPlayer)=1 Then AwardFiFiDMD:FastScoreFiFi(CurrentPlayer)=1:LightFastScore3.State=2

	End If
	If LightLock1Ready.State=2 Then 
		LightLock1Ready.State=1
		LightLock2Ready.State=2 
		LightMultiballReady.State=2 
		DMD CL(0, " GRRRRRR"), CL(1, " WOOF  "), "DMD_SpikeBG", eBlink, eNone, eNone, 3000, True, ""
		AddScore 50000
		BackGlassDogsBlinkTimer.Enabled=1
	End If
	If LightLocksReady.State=2 Then 
		LightLocksReady.State=1
		LightLock1Ready.State=2
		DMD CL(0, "  GRRRRR"), CL(1, " MORE GRRRR   "), "DMD_SpikeBG", eBlink, eNone, eNone, 3000, True, ""
		AddScore 50000
		BackGlassDogsBlinkTimer.Enabled=1
	End If
	If LightLocksActivate.State=2 Then 
		LightLocksActivate.State=1
		LightLocksReady.State=2 
		DMD CL(0, "YOU WANNA FEEL"), CL(1, "MY OVERBITE"), "DMD_SpikeBG", eNone, eBlink, eNone, 3000, True, ""
		AddScore 50000
		BackGlassDogsBlinkTimer.Enabled=1
	End If
End Sub

Sub SpikeBark
	Debug.print "SpikeSound"
	Dim SpikeSound
	SpikeSound = int(rnd*7)
	Select Case SpikeSound
		Case 1: PlaySound "S_SpikeBark1"
		Case 2:PlaySound "S_SpikeBark2"
		Case 3:PlaySound "S_SpikeBark3"
		Case 4:PlaySound "S_SpikeBark4"
		Case 5:PlaySound "S_SpikeBark5"
		Case 6:PlaySound "S_SpikeBark6"
	End Select

End Sub



Sub ResetMultiballLights
	LightLocksActivate.State=2:LightLocksReady.State=0 :LightLocksReady.State=0:LightLock1Ready.State=0:LightLock2Ready.State=0:LightMultiballReady.State=0	
	LightFastScore1.State=0:LightFastScore2.State=0:LightFastScore3.State=0:LightFastScore4.State=0:LightFastScore5.State=0
End Sub

Sub AwardCatDMD
	CatsLoose
DMD CL(0, ""), CL(1, " BAD CATS  "), "DMD_CatBG3", eBlink, eNone, eNone, 1500, True, ""
End Sub

Sub AwardSheepDMD
	SheepUp
DMD CL(0, ""), CL(1, " CHEEKY SHEEP  "), "DMD_SheepBG", eBlink, eNone, eNone, 1500, True, ""
End Sub

Sub AwardFiFiDMD
DMD CL(0, " "), CL(1, "HELLO FI FI"), "DMD_FiFiBG", eBlink, eNone, eNone, 1500, True, ""
End Sub

Sub AwardKingDMD
DMD CL(0, " " ), CL(1, "KING DOG"), "DMD_King2BG", eBlink, eNone, eNone, 1500, True, ""
End Sub

Sub AwardMultiballDMD

 
DMD CL(0, ""), CL(1, "MULTIBALL"), "DMD_King2BG", eBlink, eNone, eNone, 3000, True, ""
	If LightKingMultiballReady.State + LightCatMultiballReady.State + LightSheepMultiballReady.State +LightFiFiMultiballReady.State=4 Then  vpmtimer.addtimer 5000, "SpikeMultiball'"
	If LightKingMultiballReady.State + LightCatMultiballReady.State + LightSheepMultiballReady.State +LightFiFiMultiballReady.State=3 Then  vpmtimer.addtimer 5000, "SpikeMultiball'"
	If LightKingMultiballReady.State + LightCatMultiballReady.State + LightSheepMultiballReady.State +LightFiFiMultiballReady.State=2 Then  vpmtimer.addtimer 5000, "SpikeMultiball'"
	If LightKingMultiballReady.State + LightCatMultiballReady.State + LightSheepMultiballReady.State +LightFiFiMultiballReady.State=1 Then  vpmtimer.addtimer 5000, "SpikeMultiball'"
	If LightKingMultiballReady.State + LightCatMultiballReady.State + LightSheepMultiballReady.State +LightFiFiMultiballReady.State=0 Then  vpmtimer.addtimer 5000, "SpikeMultiball'"
End Sub


'*****FastScoringActivateForSpike Multiball***
Sub FastScoring
		FastScoreActive=1 
		FastScoreDoctorDog(CurrentPlayer)=1:FastScoreFiFi(CurrentPlayer)=1:FastScoreKing(CurrentPlayer)=1:FastScoreSheep(CurrentPlayer)=1:FastScoreCat(CurrentPlayer)=1	
		LightFastScore1.State=2:LightFastScore2.State=2:LightFastScore3.State=2:LightFastScore4.State=2:LightFastScore5.State=2
		FastScoreStopTimer.Enabled=1
End Sub

Sub FastScoreStopTimer_Timer
	ResetFastScore
End Sub

Sub ResetFastScore
		FastScoreDoctorDog(CurrentPlayer)=0:FastScoreFiFi(CurrentPlayer)=0:FastScoreKing(CurrentPlayer)=0:FastScoreSheep(CurrentPlayer)=0
		LightFastScore1.State=0:LightFastScore2.State=0:LightFastScore3.State=0:LightFastScore4.State=0:LightFastScore5.State=0
		FastScoreStopTimer.Enabled=0
End Sub

Sub SpikeMultiball
	Spike.TransZ=0
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_RalphyWhatHaveYouDone2"
	KickerDogLockKick
	DMD CL(0, "      SPIKE"), CL(1, "MULTIBALL"), "DMD_SpikeBG", eNone, eNone, eNone, 3000, True, ""
	addmultiball 2
	EnableBallSaver 25
	SpikeMultiballActive=1
	bBallSaverReady = True
	PlaySong "m_LetsDance"

End Sub

Sub KickerDogLockKick
	DOF 135,DOFPulse
'	DMDScoreNow
	SoundSaucerKick 1, KickerDogLock
	PlaySoundAt "fx_kicker" , KickerDogLock
	KickerDogLock.Kick 10, 10
End Sub


Dim LuckyDogActive
Dim ModeActive
Sub KickerModes_hit()
	RandomModeGateSelect
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(2) = 1 : FlasherFlash2_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer
	ObjLevel(1) = 1 : FlasherFlash1_Timer
	GiModeKickerOff
	SoundSaucerLock
	PlaySound"S_Splot"
	vpmtimer.addtimer 1000, "ReleaseBallKickerModes'"
	If ModeActive=1 Then Exit Sub
	If LightExtraBall.State=2 Then AwardExtraBall:LightExtraBall.State=0
	If ModeActive=0 And LuckyDogActive=0 Then vpmtimer.addtimer 2000, "StartMode'":Debug.print "StartModeCalled":Exit Sub
	If LuckyDogActive=1 Then DogReward:LightAward.State=1:Debug.print "DogRewardcalled"

End Sub

Sub RandomModeGateSelect
	Dim GateState
		GateState=int(rnd*4)
	Select Case GateState
		Case 0:ModeGateClose
		Case 1:ModeGateOpen
		Case 2:ModeGateClose
		Case 3:ModeGateOpen
	End Select

End Sub


Sub ModeGateOpen
	GateModeKicker.open = True:debug.print "ModeGateOpen"
End Sub

Sub ModeGateClose
	GateModeKicker.open = False:debug.print "ModeGateClosed"
End Sub

Sub TriggerModeGate_Hit()
	ModeGateClose
End Sub


Sub GiModeKickerOff
	gi21.State=0:gi42.State=0:gi54.State=0
	vpmtimer.addtimer 1500, "GiModeKickerOn'"
End Sub

Sub GiModeKickerOn
	gi21.State=1:gi42.State=1:gi54.State=1
End Sub

Sub ReleaseBallKickerModes
	DOF 134,DOFPulse
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(2) = 1 : FlasherFlash2_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer
	ObjLevel(1) = 1 : FlasherFlash1_Timer	'SplashFlasher.Visible=0
	SoundSaucerKick 1, KickerModes
'	PlaySoundAt "popper" , KickerModes
	KickerModes.Kick 60, 10
End Sub

Sub DogReward
	Debug.print "DogReward"
	LuckyDogActive=0
	LightAward.State=1
	Dim RewardSelect
	RewardSelect = int(rnd*5)
	Select Case RewardSelect
		Case 0 : DMD CL(0, "    GOOD BOY"), CL(1, "TREAT"), "DMD_AwardBG", eBlink, eNone, eNone, 3000, True, "": AddScore 100000
		Case 1 : DMD CL(0, "    BURGLAR"), CL(1, "ALERT"), "DMD_AwardBG", eBlink, eNone, eNone, 3000, True, "": PlaySound "GlassBreak":AddScore 200000 
		Case 2 : DMD CL(0, "     HAPPY"), CL(1, "GREETING   "), "DMD_AwardBG", eBlink, eNone, eNone, 3000, True, "": AddScore 100000
		Case 3 : DMD CL(0, "   ENTERTAIN"), CL(1, "BILLYLIDS  "), "DMD_AwardBG", eBlink, eNone, eNone, 3000, True, "": AddScore 100000 
		Case 4 : DMD CL(0, "    SIT"), CL(1, "GOOD BOY   "), "DMD_AwardBG", eBlink, eNone, eNone, 3000, True, "": AddScore 100000
	End Select
	If ModeActive=0 Then ModeSelect:	ModeActive=0

End Sub 


Sub ModeSelect
Debug.print "DogRewardcalled"
	If NextMode1(CurrentPlayer)=1 Then LightCatsLooseReadya.State=2:Exit Sub
	If NextMode2(CurrentPlayer)=1 Then LightSuperSheepReady.State=2:Exit Sub
	If NextMode3(CurrentPlayer)=1 Then LightKingDogReady.State=2:PrepareForKingMultiball:Exit Sub
	StartMode
End Sub


Sub StartMode
	Debug.print "StartMode Subroutine"
	ModeActive=1
	If NextMode1(CurrentPlayer)=1 And LuckyDogActive=0 And CatsLooseActive=0 Then CatsLoose:NextMode2(CurrentPlayer)=1:NextMode1(CurrentPlayer)=0: LuckyDogActive=1 :LightAward.State=2:LightCatsLooseReadya.State=1:Exit Sub
	If NextMode2(CurrentPlayer)=1 And LuckyDogActive=0  And SuperSheepActive=0 Then SuperSheep:NextMode3(CurrentPlayer)=1:NextMode2(CurrentPlayer)=0: LuckyDogActive=1 :LightAward.State=2:LightSuperSheepReady.State=1:Exit Sub
	If NextMode3(CurrentPlayer)=1 And LuckyDogActive=0 Then KingDog:NextMode1(CurrentPlayer)=1:NextMode3(CurrentPlayer)=0:LuckyDogActive=1 :KingDogUp:ResetModeLights:Exit Sub
End Sub

Sub ResetModeLights
 LightAward.State=2:LightCatsLooseReadya.State=0:LightSuperSheepReady.State=0:LightKingDogReady.State=0

End Sub

Sub ActivateLuckyDog
	debug.print "ActivateLuckyDogAward"
	LuckyDogActive=1:LightAward.State=2
End Sub

Sub KickerCatapult_Hit()
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(2) = 1 : FlasherFlash2_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer
	ObjLevel(1) = 1 : FlasherFlash1_Timer
	GiOff
	SoundSaucerLock
'	PlaySoundAt "fx_kicker_catch", KickerCatapult
	DMD CL(0, "  HELMENT ON"), CL(1, "KITTY BANG BANG"), "DMD_KittyBoom2BG", eBlink, eNone, eNone, 3000, True, ""
	vpmtimer.addtimer 3000, "KickerCatapultKick'"
End Sub

Sub KickerCatapultKick
	'DOF 132, DOFPulse
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(2) = 1 : FlasherFlash2_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer
	ObjLevel(1) = 1 : FlasherFlash1_Timer
	Gion
	SoundSaucerKick 1, KickerCatapult
	PlaySoundAt SoundFXDOF("fx_kicker" , 132, DOFPulse, DOFContactors) , KickerCatapult
	PlaySound "CanonFire"
	KickerCatapult.Kick 220, 30
	CanonKittyBlast.Visible=1
	KittyCanonTimer.Enabled=1
	AdvanceBonoMeter
	vpmtimer.addtimer 3500, "AwardSkillshot'"
End Sub

Sub KittyCanonTimer_Timer
	CanonKittyBlast.Visible=0
	KittyCanonTimer.Enabled=0
End Sub

Sub CatsLoose
		If bGameInPlay=True Then LightCat17.State=2:PlaySound "CO_OoCat"
	DMD CL(0, "   CATS LOOSE"), CL(1, ""), "DMD_CatBG3", eBlink, eNone, eNone, 3000, True, ""
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	Cat1MoveUp:Cat2MoveDown:Cat3MoveDown:Cat1Wall.IsDropped=0:Cat2Wall.IsDropped=1:Cat3Wall.IsDropped=1
	CatsLooseActive=True
	CatsChangeTimer.Enabled=1
	vpmtimer.addtimer 60000, "StopCats'"
End Sub

Dim CatSelect
Sub CatsChangeTimer_Timer
	CatSelect = CatSelect+1
	Select Case CatSelect
		Case 1 :  Cat2MoveUp:Cat1MoveDown:Cat3MoveDown
		Case 2 :  Cat3MoveUp:Cat1MoveDown:Cat2MoveDown
		Case 3 :  Cat1MoveUp:Cat2MoveDown:Cat3MoveDown
		Case 4 :  Cat2MoveUp:Cat1MoveDown:Cat3MoveDown
		Case 5 :  Cat3MoveUp:Cat1MoveDown:Cat2MoveDown
		Case 6 :  Cat1MoveUp:Cat2MoveDown:Cat3MoveDown
		Case 7 :  Cat2MoveUp:Cat1MoveDown:Cat3MoveDown
		Case 8 :  Cat3MoveUp:Cat1MoveDown:Cat2MoveDown
		Case 9:   Cat1MoveUp:Cat2MoveDown:Cat3MoveDown
		Case 10 : Cat2MoveUp:Cat1MoveDown:Cat3MoveDown
		Case 11 : Cat3MoveUp:Cat1MoveDown:Cat2MoveDown:CatSelect=0
	End Select
End Sub

Sub StopCats
	LightCat17.State=0
	debug.print "StopCats"
	CatsDown
	CatsLooseActive=0
	CatsChangeTimer.Enabled=0
'	ActivateLuckyDog
	ModeActive=0
End Sub

Sub CatsDown
		Cat1MoveDown:Cat2MoveDown:Cat3MoveDown:Cat1Wall.IsDropped=1:Cat2Wall.IsDropped=1:Cat3Wall.IsDropped=1
End Sub

Sub Cat1Wall_Hit()
	Cat1Wall.IsDropped=1
	Cat1MoveDown:Cat2MoveUp
	Cat2Wall.IsDropped=0
	CatSound
	CheckCatAward
End Sub

Sub Cat2Wall_Hit()
	Cat2Wall.IsDropped=1
	Cat2MoveDown:Cat3MoveUp
	Cat3Wall.IsDropped=0
	CatSound
	CheckCatAward
End Sub

Sub Cat3Wall_Hit()
	Cat3Wall.IsDropped=1
	Cat3MoveDown:Cat1MoveUp
	Cat1Wall.IsDropped=0
	CatSound
	CheckCatAward
End Sub

Sub CheckCatAward
	CatsSorted(CurrentPlayer)=CatsSorted(CurrentPlayer)+1
	CatCount(CurrentPlayer)=CatCount(CurrentPlayer)+1
	If CatCount(CurrentPlayer)=1 Then DMD CL(0, "DANG CAT"), CL(1, ""), "DMD_CatBG3", eNone, eNone, eNone, 3000, True, "": AddScore 200000: debug.print "CatCount 1"
	If CatCount(CurrentPlayer)=2 Then DMD CL(0, "COP THAT"), CL(1, "POOTY"), "DMD_CatBG3", eBlink, eNone, eNone, 3000, True, "": AddScore 400000: debug.print "CatCount 2"
	If CatCount(CurrentPlayer)=3 Then vpmtimer.addtimer 200,"AwardJackpot3'":vpmtimer.addtimer 1500, "AdvanceBonoMeter'": debug.print "CatCount 3"'*********************************
	If CatCount(CurrentPlayer)=4 Then DMD CL(0, "    SNAPPY"), CL(1, "TOM"), "DMD_CatBG3", eNone, eNone, eNone, 3000, True, "": debug.print "CatCount 4"
	If CatCount(CurrentPlayer)=5 Then DMD CL(0, "     NOT HAPPY"), CL(1, "POOTY"), "DMD_CatBG3", eBlink, eNone, eNone, 3000, True, "": debug.print "CatCount 5"
	If CatCount(CurrentPlayer)=6 Then vpmtimer.addtimer 200,"AwardSuperJackpot'":StopCats:CatCount(CurrentPlayer)=0:vpmtimer.addtimer 3200, "AdvanceBonoMeter'":PrepareCatMultiball: debug.print "CatCount 6"'*********************************
End Sub



Sub SuperSheep
	SuperSheepCount(CurrentPlayer)=0
	SheepUp

	DMD CL(0, ""), CL(1, "   SHEEP ARE OUT"), "DMD_King2BG", eNone, eNone, eNone, 3000, True, ""
	AddScore 100000
	vpmtimer.addtimer 60000, "StopAngrySheep'"
End Sub

Sub ActivateFastScoreSheep
	LightFastScore5.State=2:FastScoreSheep(CurrentPlayer)=1
End Sub

Sub StopAngrySheep
	SheepDown
	LightFastScore5.State=0:FastScoreSheep(CurrentPlayer)=0
'	ActivateLuckyDog
	ModeActive=0
End Sub

'*****SheepPrimitive Action*****
'*****SheepUPAnimation*****

Sub SheepUp
	If bGameInPlay=True Then :PlaySound "CO_SheepAreOut"
	SuperSheepActive=1
	Sheep1Up
	SuperSheepCount(CurrentPlayer)=0
	SheepGlowTimer.Enabled=1
	SheepTimer.Enabled=1
End Sub

Sub Sheep1Up
	SheepTim1.TransZ=120:Sheep1SwirlTimer.Enabled=1:SpinDiscSheep1.Visible=1:Sheep1Active=1:Sheep1Wall.IsDropped=0
End Sub

Sub Sheep2Up
	SheepTim2.TransZ=120 :Sheep2SwirlTimer.Enabled=1:SpinDiscSheep2.Visible=1:Sheep2Active=1:Sheep2Wall.IsDropped=0
End Sub

Sub Sheep3Up
	SheepTim3.TransZ=120:Sheep3SwirlTimer.Enabled=1:SpinDiscSheep3.Visible=1:Sheep3Active=1:Sheep3Wall.IsDropped=0
End Sub

'*****SheepDownAnimation*****

Sub SheepDown
	SheepTimer.Enabled=0
	SuperSheepActive=0
	SuperSheepCount(CurrentPlayer)=0
	SheepGlowTimer.Enabled=0
	Sheep1Down
	Sheep2Down
	Sheep3Down
End Sub

Sub Sheep1Down
	SheepTim1.TransZ=-120:Sheep1Active=0:Sheep1SwirlTimer.Enabled=0:SpinDiscSheep1.Visible=0:Sheep1Wall.IsDropped=1
End Sub

Sub Sheep2Down
	SheepTim2.TransZ=-120:Sheep2Active=0:Sheep2SwirlTimer.Enabled=0:SpinDiscSheep2.Visible=0:Sheep2Wall.IsDropped=1
End Sub

Sub Sheep3Down
	SheepTim3.TransZ=-120:Sheep3Active=0:Sheep3SwirlTimer.Enabled=0:SpinDiscSheep3.Visible=0:Sheep3Wall.IsDropped=1
End Sub

'*****SheepHit****

Sub Sheep1Wall_Hit()
	Sheep1Down:Sheep2Up:SheepSound2: CheckSuperSheep
	SheepSorted(CurrentPlayer)=SheepSorted(CurrentPlayer)+1
End Sub

Sub Sheep2Wall_Hit()
	Sheep2Down:Sheep3Up:SheepSound2: CheckSuperSheep
	SheepSorted(CurrentPlayer)=SheepSorted(CurrentPlayer)+1
End Sub

Sub Sheep3Wall_Hit()
	Sheep3Down:Sheep1Up:SheepSound2: CheckSuperSheep	
	SheepSorted(CurrentPlayer)=SheepSorted(CurrentPlayer)+1
End Sub

'*****CheckSuperSheepHits*****
Sub CheckSuperSheep
	If FastScoreSheep(CurrentPlayer)=1 Then CheckSuperSheepFastScore
	If FastScoreSheep(CurrentPlayer)=0 Then CheckSuperSheepNotFastScore
End Sub

Sub CheckSuperSheepNotFastScore
	SheepSorted(CurrentPlayer)=SheepSorted(CurrentPlayer)+1
	SuperSheepCount(CurrentPlayer)=SuperSheepCount(CurrentPlayer)+1
	If SuperSheepCount(CurrentPlayer)=1 Then AddScore 200000:debug.print "SuperSheepCount=1"
	If SuperSheepCount(CurrentPlayer)=2 Then AddScore 300000'":debug.print "SuperSheepCount=2)"
	If SuperSheepCount(CurrentPlayer)=3 Then vpmtimer.addtimer 200, "AwardJackPot2'":vpmtimer.addtimer 3000, "AdvanceBonoMeter'":debug.print "SuperSheepCount=3)"
	If SuperSheepCount(CurrentPlayer)=4 Then AddScore 800000:debug.print "SuperSheepCount=4)"
	If SuperSheepCount(CurrentPlayer)=5 Then  AddScore 1000000:debug.print "SuperSheepCount=5"
	If SuperSheepCount(CurrentPlayer)=6 Then vpmtimer.addtimer 500, "AwardSuperJackPot'":vpmtimer.addtimer 3000, "AdvanceBonoMeter'":StopAngrySheep:SuperSheepCount(CurrentPlayer)=0:PrepareSheepMultiball:debug.print "SuperSheepCount=6)"
End Sub

Sub CheckSuperSheepFastScore
	SheepSorted(CurrentPlayer)=SheepSorted(CurrentPlayer)+1
	SuperSheepCount(CurrentPlayer)=SuperSheepCount(CurrentPlayer)+1
	If SuperSheepCount(CurrentPlayer)=1 Then AddScore 400000:debug.print "SuperSheepCount=1"
	If SuperSheepCount(CurrentPlayer)=2 Then AddScore 600000'":debug.print "SuperSheepCount=2)"
	If SuperSheepCount(CurrentPlayer)=3 Then vpmtimer.addtimer 200, "AwardJackPot3'":vpmtimer.addtimer 3200, "AdvanceBonoMeter'":debug.print "SuperSheepCount=3)"
	If SuperSheepCount(CurrentPlayer)=4 Then AddScore 1600000:debug.print "SuperSheepCount=4)"
	If SuperSheepCount(CurrentPlayer)=5 Then AddScore 2000000:debug.print "SuperSheepCount=5"
	If SuperSheepCount(CurrentPlayer)=6 Then vpmtimer.addtimer 200, "AwardSuperJackPot2'":vpmtimer.addtimer 3000, "AdvanceBonoMeter'":StopAngrySheep:SuperSheepCount(CurrentPlayer)=0:PrepareSheepMultiball:debug.print "SuperSheepCount=6)"
End Sub



'*****SheepSounds*****

Sub SheepSound2
	Debug.print "SheepSound"
	Dim SheepSoundSelect
	SheepSoundSelect = int(rnd*5)
	Select Case SheepSoundSelect
		Case 0 : DMD CL(0, "    GOTCHA"), CL(1, "FLUFFY BUT"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "":  PlaySound "Sheep1":PlaySoundAt "fx_target",KickerModes 
		Case 1 : DMD CL(0, "     BARBA RAN"), CL(1, "HE HEH"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "": PlaySound "Sheep2":PlaySoundAt "fx_target",KickerModes
		Case 2 : DMD CL(0, "     ITS SHEARING"), CL(1, "TIME"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "":PlaySound "Sheep3":PlaySoundAt "fx_target",KickerModes
		Case 3 : DMD CL(0, "  IN THE PEN"), CL(1, "COTTON BUD"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "":PlaySound "Sheep4":PlaySoundAt "fx_target",KickerModes 
		Case 4 : DMD CL(0, "  BOO YEH"),  CL(1, "FLUFFY"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "": PlaySound "Sheep5":PlaySoundAt "fx_target",KickerModes 
		Case 5 : DMD CL(0, "   A LITTLE NIP"), CL(1, "DOES THE JOB"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "":PlaySound "Sheep6":PlaySoundAt "fx_target",KickerModes
		Case 6 : DMD CL(0, "  HELLO"),  CL(1, "LAMB CHOP"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "": PlaySound "Sheep7":PlaySoundAt "fx_target",KickerModes
	End Select
End Sub 

'*****SwirlsAroundTheSheep*****

Sub Sheep1SwirlTimer_Timer
	SpinDiscSheep1.rotz = (SpinDiscSheep1.rotz + 5)mod 360
End Sub

Sub Sheep2SwirlTimer_Timer
	SpinDiscSheep2.rotz = (SpinDiscSheep2.rotz + 5)mod 360
End Sub

Sub Sheep3SwirlTimer_Timer
	SpinDiscSheep3.rotz = (SpinDiscSheep3.rotz + 5)mod 360
End Sub

Dim SheepSelect
Dim Sheep1Active
Dim Sheep2Active
Dim Sheep3Active

Sub SheepTimer_Timer()
	If Sheep1Active=1 Then Sheep1Down:Sheep2Up:Exit Sub
	If Sheep2Active=1 Then Sheep2Down:Sheep3Up:Exit Sub
	If Sheep3Active=1 Then Sheep3Down:Sheep1Up:Exit Sub
End Sub


Dim SheepGlow:SheepGlow=0
Dim yy
Sub SheepGlowTimer_Timer()
	SheepGlow= SheepGlow+1
	If SheepGlow=1 Then	For each yy in Sheep: yy.image = "SheepTimmyTexture":Next
	If SheepGlow=4 Then	For each yy in Sheep: yy.image = "SheepTimmyTexture":Next
	If SheepGlow=5 Then	For each yy in Sheep: yy.image = "SheepTimmyTextureoff":Next	
	If SheepGlow=7 Then	For each yy in Sheep: yy.image = "SheepTimmyTextureoff":SheepGlow=0:Next
End Sub

Sub TriggerSheep1_Hit()

	LastSwitchHit="TriggerSheep1"
debug.print "TriggerSheep1Hit"
End Sub

Sub TriggerSheep2_Hit()
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer
	SheepBlink
debug.print "TriggerSheep2Hit"
	SheepSorted(CurrentPlayer)=SheepSorted(CurrentPlayer)+1
	SheepCount(CurrentPlayer)=SheepCount(CurrentPlayer)+1
	If LightFastScore5.State=2 And LastSwitchHit="TriggerSheep1" Then
		If LightAngrySheep8.State=2 Then vpmtimer.addtimer 32000, "AwardSuperJackPot2'":vpmtimer.addtimer 500, "ResetSheepLights'":debug.print "AngrySheep8Hit"
		If LightAngrySheep7.State=2 Then LightAngrySheep7.State=1:LightAngrySheep8.State=2:PlaySound "Sheep7":vpmtimer.addtimer 3200, "AwardSuperJackPot'":vpmtimer.addtimer 3200, "AdvanceBonoMeter'":debug.print "AngrySheep7Hit"
		If LightAngrySheep6.State=2 Then LightAngrySheep6.State=1:LightAngrySheep7.State=2:SheepSound:vpmtimer.addtimer 3200, "AwardSuperJackPot'"::debug.print "AngrySheep6Hit"
		If LightAngrySheep5.State=2 Then LightAngrySheep5.State=1:LightAngrySheep6.State=2:SheepSound:vpmtimer.addtimer 3200, "AwardJackpot3'":debug.print "AngrySheep5Hit":PrepareSheepMultiball
		If LightAngrySheep4.State=2 Then LightAngrySheep4.State=1:LightAngrySheep5.State=2:PlaySound "Sheep3":vpmtimer.addtimer 3200, "AwardJackPot2'":vpmtimer.addtimer 3200, "AdvanceBonoMeter'":debug.print "AngrySheep4Hit":PrepareSheepMultiball
		If LightAngrySheep3.State=2 Then LightAngrySheep3.State=1:LightAngrySheep4.State=2:SheepSound:vpmtimer.addtimer 3200, "AwardJackPot'":debug.print "AngrySheep3Hit"
		If LightAngrySheep2.State=2 Then LightAngrySheep2.State=1:LightAngrySheep3.State=2:SheepSound:AddScore 400000:debug.print "AngrySheep2Hit"
		If LightAngrySheep1.State=2 Then LightAngrySheep1.State=1:LightAngrySheep2.State=2:SheepSound:AddScore 200000:debug.print "AngrySheep1Hit"
	End If

	If LightFastScore5.State=0 And LastSwitchHit="TriggerSheep1" Then
		If LightAngrySheep8.State=2 Then AwardJackpot3:vpmtimer.addtimer 500, "ResetSheepLights'":debug.print "AngrySheep8Hit"
		If LightAngrySheep7.State=2 Then LightAngrySheep7.State=1:LightAngrySheep8.State=2:PlaySound "Sheep7":AwardJackpot2:vpmtimer.addtimer 3200, "AdvanceBonoMeter'":debug.print "AngrySheep7Hit"
		If LightAngrySheep6.State=2 Then LightAngrySheep6.State=1:LightAngrySheep7.State=2:SheepSound:AwardJackPot:debug.print "AngrySheep6Hit":
		If LightAngrySheep5.State=2 Then LightAngrySheep5.State=1:LightAngrySheep6.State=2:SheepSound:AddScore 500000:debug.print "AngrySheep5Hit":PrepareSheepMultiball
		If LightAngrySheep4.State=2 Then LightAngrySheep4.State=1:LightAngrySheep5.State=2:PlaySound "Sheep3":AddScore 400000:vpmtimer.addtimer 3200, "AdvanceBonoMeter'":debug.print "AngrySheep4Hit"
		If LightAngrySheep3.State=2 Then LightAngrySheep3.State=1:LightAngrySheep4.State=2:SheepSound:AddScore 300000:debug.print "AngrySheep3Hit"
		If LightAngrySheep2.State=2 Then LightAngrySheep2.State=1:LightAngrySheep3.State=2:SheepSound:AddScore 200000:debug.print "AngrySheep2Hit"
		If LightAngrySheep1.State=2 Then LightAngrySheep1.State=1:LightAngrySheep2.State=2:SheepSound:AddScore 100000:debug.print "AngrySheep1Hit"
	End If
End Sub

Sub SheepBlink
	LightPFSheep1.State=2
	LightPFSheep2.State=2
	LightPFSheep3.State=2

	LightPFSheep1.TimerEnabled=1
End Sub

Sub LightPFSheep1_Timer
	LightPFSheep1.State=0
	LightPFSheep2.State=0
	LightPFSheep3.State=0
	LightPFSheep1.TimerEnabled=0

End Sub

Sub SheepSound
	Debug.print "SheepSound"
	Dim SheepSoundSelect
	SheepSoundSelect = int(rnd*5)
	Select Case SheepSoundSelect
		Case 0 : DMD CL(0, "    GOTCHA"), CL(1, "FLUFFY BUT"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "":  PlaySound "Sheep1":PlaySoundAt "fx_target",KickerModes 
		Case 1 : DMD CL(0, "     BARBA RAN"), CL(1, "HE HEH"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "": PlaySound "Sheep2":PlaySoundAt "fx_target",KickerModes
		Case 2 : DMD CL(0, "     ITS SHEARING"), CL(1, "TIME"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "":PlaySound "Sheep3":PlaySoundAt "fx_target",KickerModes
		Case 3 : DMD CL(0, "  IN THE PEN"), CL(1, "COTTON BUD"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "":PlaySound "Sheep4":PlaySoundAt "fx_target",KickerModes 
		Case 4 : DMD CL(0, "  BOO YEH"),  CL(1, "FLUFFY"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "": PlaySound "Sheep5":PlaySoundAt "fx_target",KickerModes 
		Case 5 : DMD CL(0, "A LITTLE NIP"), CL(1, "DOES THE JOB"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "":PlaySound "Sheep6":PlaySoundAt "fx_target",KickerModes
		Case 6 : DMD CL(0, "  HELLO"),  CL(1, "LAMB CHOP"), "DMD_SheepBG", eBlink, eNone, eNone, 2000, True, "": PlaySound "Sheep7":PlaySoundAt "fx_target",KickerModes
	End Select
End Sub 


Sub PrepareSheepMultiball
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_SheepMultiballReady3"
	SheepMultiballReady(CurrentPlayer)=1
	LightSheepMultiballReady.State=1
End Sub

Sub ResetSheepLights
	LightAngrySheep1.State=2:LightAngrySheep2.State=0:LightAngrySheep3.State=0:LightAngrySheep4.State=0
	LightAngrySheep5.State=0:LightAngrySheep6.State=0:LightAngrySheep7.State=0:LightAngrySheep8.State=0
End Sub

Sub KingDog                            '*****Started From ModeKicker *****
	KingDogActive(CurrentPlayer)=1
	FastScoreKing(CurrentPlayer)=1
	LightFastScore2.State=1
	DMD CL(0, "KING DOG"), CL(1, ""), "", eBlink, eNone, eNone, 3000, True, ""
	AddScore 100000
	vpmtimer.addtimer 30000, "StopKingDog'"
End Sub


Sub StopKingDog
	debug.print "StopKingDog"
	KingDogActive(CurrentPlayer)=0
	FastScoreKing(CurrentPlayer)=0

	LightFastScore2.State=0
'	LightG.State=0:LightN1.State=0:LightI.State=0:LightK.State=0
'	ActivateLuckyDog	
	ModeActive=0
End Sub

Sub TriggerKingDog1a_Hit()
	LastSwitchHit="TriggerKingDog1a"
End Sub

Sub TriggerKingDog1b_Hit()
	LastSwitchHit="TriggerKingDog1b"

	If KingDogActive(CurrentPlayer)=0  Then
		If LightK.State=1 Then AddScore 200000:Exit Sub
		If LightN1.State=1 Then AddScore 100000:Exit Sub
		If LightI.State=1 Then AddScore 750000:Exit Sub
		If LightG.State=1 Then AddScore 50000
	End If

	If KingDogActive(CurrentPlayer)=0  Then
		If LightK.State=2 Then AddScore 200000:Exit Sub
		If LightN1.State=2 Then AddScore 100000:Exit Sub
		If LightI.State=2 Then AddScore 750000:Exit Sub
		If LightG.State=2 Then AddScore 50000
	End If

	If KingDogActive(CurrentPlayer)=1 And LightFastScore2.State=2 Then
		LightPFScruffyDog.State=2
		LightPFScruffyDog.TimerEnabled=1
		If LightK.State=1 Then vpmtimer.addtimer 1000, "AwardSuperJackpot'":Exit Sub
		If LightN1.State=1 Then vpmtimer.addtimer 1000, "AwardJackpot3'":Exit Sub
		If LightI.State=1 Then vpmtimer.addtimer 1000, "AwardJackpot2'":Exit Sub
		If LightG.State=1 Then vpmtimer.addtimer 1000, "AwardJackPot'"
	End If

	If FastScoreKing(CurrentPlayer)=1  Then
		LightPFScruffyDog.State=2
		LightPFScruffyDog.TimerEnabled=1
		If LightK.State=1 Then vpmtimer.addtimer 1000, "AwardJackpot3'"	:Exit Sub
		If LightI.State=1 Then vpmtimer.addtimer 1000, "AwardJackpot2'":Exit Sub
		If LightN1.State=1 Then vpmtimer.addtimer 1000, "AwardJackpot'":Exit Sub
		If LightG.State=1 Then AddScore 500000
	End If
End Sub

Sub LightPFScruffyDog_Timer
		LightPFScruffyDog.State=0
		LightPFScruffyDog.TimerEnabled=0
End Sub

Sub AllKingLightsBlink
	LightK.State=2:LightI.State=2:LightN1.State=2:LightG.State=2
End Sub

'*****BonoMeter*****
Sub AdvanceBonoMeter
	BonoAdvance(CurrentPlayer)=BonoAdvance(CurrentPlayer)+1
	Select Case BonoAdvance(CurrentPlayer)
		Case 1 : DMD CL(0, "    DOG LEVEL 1"), CL(1, " LITTLE FLUFFER  "), "DMD_KittyBoom3BG", eBlink, eNone, eNone, 3000, True, "":AddScore 500000 :BonoMeter1.Visible=1:BonoMeter0.Visible=0
					LightBono1.State=1:LightBono2.State=0:LightBono3.State=0:LightBono4.State=0:LightBono5.State=0:LightBono6.State=0:LightBono7.State=0:LightBono8.State=0
		Case 2 : DMD CL(0, "    DOG LEVEL 2"), CL(1, "  YOUNG PUP  "), "DMD_KittyBoom3BG", eBlink, eNone, eNone, 3000, True, "": AddScore 750000 :BonoMeter2.Visible=1:BonoMeter1.Visible=0:LightBono2.State=1
					LightBono1.State=1:LightBono2.State=1:LightBono3.State=0:LightBono4.State=0:LightBono5.State=0:LightBono6.State=0:LightBono7.State=0:LightBono8.State=0
		Case 3 : DMD CL(0, "    DOG LEVEL 3"), CL(1, "LOOSE LEAD  "), "DMD_KittyBoom3BG", eBlink, eNone, eNone, 3000, True, "":AddScore 10000000:BonoMeter3.Visible=1:BonoMeter2.Visible=0:LightBono3.State=1
					LightBono1.State=1:LightBono2.State=1:LightBono3.State=1:LightBono4.State=0:LightBono5.State=0:LightBono6.State=0:LightBono7.State=0:LightBono8.State=0
		Case 4 : DMD CL(0, "    DOG LEVEL 4"), CL(1, "NIPPING HEALS   "), "DMD_KittyBoom3BG", eBlink, eNone, eNone, 3000, True, "": AddScore 1250000 :BonoMeter4.Visible=1:BonoMeter3.Visible=0:LightBono4.State=1
					LightBono1.State=1:LightBono2.State=1:LightBono3.State=1:LightBono4.State=1:LightBono5.State=0:LightBono6.State=0:LightBono7.State=0:LightBono8.State=0
		Case 5 : DMD CL(0, "    DOG LEVEL 5"), CL(1, "READY TO HERD   "), "DMD_KittyBoom3BG", eBlink, eNone, eNone, 3000, True, "": AddScore 1500000:BonoMeter5.Visible=1:BonoMeter4.Visible=0:LightBono5.State=1
					LightBono1.State=1:LightBono2.State=1:LightBono3.State=1:LightBono4.State=1:LightBono5.State=1:LightBono6.State=0:LightBono7.State=0:LightBono8.State=0
		Case 6 : DMD CL(0, "    DOG LEVEL 6"), CL(1, "BOSS OF HERD   "), "DMD_KittyBoom3BG", eBlink, eNone, eNone, 3000, True, "":AddScore 1750000 :BonoMeter6.Visible=1:BonoMeter5.Visible=0:LightBono6.State=1
					LightBono1.State=1:LightBono2.State=1:LightBono3.State=1:LightBono4.State=1:LightBono5.State=1:LightBono6.State=1:LightBono7.State=0:LightBono8.State=0
		Case 7 : DMD CL(0, "    DOG LEVEL 7"), CL(1, "BULL READY   "), "DMD_KittyBoom3BG", eBlink, eNone, eNone, 3000, True, "": AddScore 2000000:BonoMeter7.Visible=1:BonoMeter6.Visible=0:LightBono7.State=1
					LightBono1.State=1:LightBono2.State=1:LightBono3.State=1:LightBono4.State=1:LightBono5.State=1:LightBono6.State=1:LightBono7.State=1:LightBono8.State=0
		Case 8 : DMD CL(0, "    DOG LEVEL 8"), CL(1, "KING DOG   "), "DMD_KittyBoom3BG", eBlink, eNone, eNone, 3000, True, "": AddScore 5000000 :BonoMeter8.Visible=1:BonoMeter7.Visible=0::LightBono8.State=1
					LightBono1.State=1:LightBono2.State=1:LightBono3.State=1:LightBono4.State=1:LightBono5.State=1:LightBono6.State=1:LightBono7.State=1:LightBono8.State=1
				CheckExtraBallHurryUp
				 BonoAdvance(CurrentPlayer)=0:BonoMetersCompleted(CurrentPlayer)=BonoMetersCompleted(CurrentPlayer)+1
				 ResetBonoMeter
	End Select
End Sub

Sub CheckExtraBallHurryUp
	If bExtraBallWonThisBall=0  Then ExtraBallHurryUp:debug.print "CheckExtraBallHurryUpSubroutine"
End Sub

Dim BonoCount
BonoCount=0
Sub BoneOMeterAttractTimer_Timer()
	BonoCount=BonoCount+1
	Select Case BonoCount
		Case 8:BonoMeter0.Visible=0:BonoMeter1.Visible=1:LightBono8.State=0:LightBono1.State=0:LightBono2.State=0:LightBono3.State=0:LightBono4.State=0:LightBono5.State=0:LightBono6.State=0:LightBono7.State=0
		Case 9:BonoMeter1.Visible=0:BonoMeter2.Visible=1:LightBono1.State=1		
		Case 10:BonoMeter2.Visible=0:BonoMeter3.Visible=1:LightBono2.State=1
		Case 11:BonoMeter3.Visible=0:BonoMeter4.Visible=1:LightBono3.State=1
		Case 12:BonoMeter4.Visible=0:BonoMeter5.Visible=1:LightBono4.State=1
		Case 13:BonoMeter5.Visible=0:BonoMeter6.Visible=1:LightBono5.State=1
		Case 14:BonoMeter6.Visible=0:BonoMeter7.Visible=1:LightBono6.State=1
		Case 15:BonoMeter7.Visible=0:BonoMeter8.Visible=1:LightBono7.State=1
		Case 23:BonoMeter8.Visible=0:BonoMeter0.Visible=1:LightBono8.State=1:BonoCount=0	
	
	End Select
End Sub

Sub ResetBonoMeter
	BonoMeter0.Visible=1:BonoMeter1.Visible=0:BonoMeter2.Visible=0:BonoMeter3.Visible=0:BonoMeter4.Visible=0
	BonoMeter5.Visible=0:BonoMeter6.Visible=0:BonoMeter7.Visible=0:BonoMeter8.Visible=0
	LightBono1.State=0:LightBono2.State=0:LightBono3.State=0:LightBono4.State=0:LightBono5.State=0:LightBono6.State=0:LightBono7.State=0:LightBono8.State=0
End Sub


'*****CatsLoose*****

'*************************
'Cat1-UP/DOWN Animation
'*************************
Dim Cat1Pos, Cat1Dir, Cat1ShakePos, Cat1ShakeDir,Cat1Up


Cat1Pos =-80
Cat1ShakePos = 85

Sub Cat1AnimTimer_Timer()

    Cat1ShakeTimer.Enabled = 0
    Cat1Pos = Cat1Pos + Cat1Dir
    'Dragonis moving up
    If Cat1Pos >= -81 Then
        Me.Enabled = 0
        Cat1Pos = -80
        Cat1ShakeDir = 1
       Cat1ShakeTimer.Enabled = 1
    End If

    'Cat1 is moving down
    If Cat1Pos <= -81 Then
        Me.Enabled = 0
        Cat1Pos = -80
    End If

Dim i
For each i in Cat1: i.Transz = Cat1Pos: Next
End Sub

Sub Cat1ShakeTimer_Timer
    Cat1ShakePos = Cat1ShakePos+ Cat1ShakeDir
    If Cat1ShakePos > 85 Then
        Cat1ShakeDir = -2
    End If

    'Cat1is moving down
    If Cat1ShakePos< 67 Then
        Cat1ShakeDir = 2
    End If
Dim i
	For each i in Cat1 :i.Transz = Cat1ShakePos: Next
End Sub

Dim Cat1Down
Sub Cat1MoveUp()
	Cat1SwirlTimer.Enabled=1: SpinDiscCat1.Visible=1 
	Cat1Down=False
	Cat1Wall.IsDropped=False
    'Play a Sound?
    Cat1Dir = 2
    Cat1AnimTimer.Enabled = 1
	Cat1Up=True

End Sub

Sub Cat1MoveDown()
	Cat1SwirlTimer.Enabled=0: SpinDiscCat1.Visible=0
	Cat1Down=True
	Cat1Wall.IsDropped=True
    Cat1Dir = -2
    Cat1AnimTimer.Enabled = 1
	Cat1Up=False

End Sub

Sub Cat1SwirlTimer_Timer
	SpinDiscCat1.rotz = (SpinDiscCat1.rotz + 5)mod 360
End Sub
'*************************
'Cat2-UP/DOWN Animation
'*************************
Dim Cat2Pos, Cat2Dir, Cat2ShakePos, Cat2ShakeDir,Cat2Up


Cat2Pos =-80
Cat2ShakePos = 85

Sub Cat2AnimTimer_Timer()

    Cat2ShakeTimer.Enabled = 0
    Cat2Pos = Cat2Pos + Cat2Dir
    'Dragonis moving up
    If Cat2Pos >= -81 Then
        Me.Enabled = 0
        Cat2Pos = -80
        Cat2ShakeDir = 1
       Cat2ShakeTimer.Enabled = 1
    End If

    'Cat2 is moving down
    If Cat2Pos <= -81 Then
        Me.Enabled = 0
        Cat2Pos = -80
    End If

Dim i
For each i in Cat2: i.Transz = Cat2Pos: Next
End Sub

Sub Cat2ShakeTimer_Timer
    Cat2ShakePos = Cat2ShakePos+ Cat2ShakeDir
    If Cat2ShakePos > 85 Then
        Cat2ShakeDir = -2
    End If

    'Cat1is moving down
    If Cat2ShakePos< 67 Then
        Cat2ShakeDir = 2
    End If
Dim i
	For each i in Cat2 :i.Transz = Cat2ShakePos: Next
End Sub

Dim Cat2Down
Sub Cat2MoveUp()
	Cat2SwirlTimer.Enabled=1: SpinDiscCat2.Visible=1 
	Cat2Wall.IsDropped=False
	Cat2Down=False
    'Play a Sound?
    Cat2Dir = 2
    Cat2AnimTimer.Enabled = 1
	Cat2Up=True
End Sub

Sub Cat2MoveDown()
	Cat2SwirlTimer.Enabled=0: SpinDiscCat2.Visible=0 
	Cat2Wall.IsDropped=True
	Cat2Down=True
    Cat2Dir = -2
    Cat2AnimTimer.Enabled = 1
	Cat2Up=False
End Sub

Sub Cat2SwirlTimer_Timer
	SpinDiscCat2.rotz = (SpinDiscCat2.rotz + 5)mod 360
End Sub

'*************************
'Cat3-UP/DOWN Animation
'*************************
Dim Cat3Pos, Cat3Dir, Cat3ShakePos, Cat3ShakeDir,Cat3Up


Cat3Pos =-80
Cat3ShakePos = 85

Sub Cat3AnimTimer_Timer()

    Cat3ShakeTimer.Enabled = 0
    Cat3Pos = Cat3Pos + Cat3Dir
    'Dragonis moving up
    If Cat3Pos >= -81 Then
        Me.Enabled = 0
        Cat3Pos = -80
        Cat3ShakeDir = 1
       Cat3ShakeTimer.Enabled = 1
    End If

    'Cat3 is moving down
    If Cat3Pos <= -81 Then
        Me.Enabled = 0
        Cat3Pos = -80
    End If

Dim i
For each i in Cat3: i.Transz = Cat3Pos: Next
End Sub

Sub Cat3ShakeTimer_Timer
    Cat3ShakePos = Cat3ShakePos+ Cat3ShakeDir
    If Cat3ShakePos > 85Then
        Cat3ShakeDir = -2
    End If

    'Cat1is moving down
    If Cat3ShakePos< 67 Then
        Cat3ShakeDir = 2
    End If
Dim i
	For each i in Cat3 :i.Transz = Cat3ShakePos: Next
End Sub

Dim Cat3Down
Sub Cat3MoveUp()
	Cat3SwirlTimer.Enabled=1: SpinDiscCat3.Visible=1          ':SpinDiscCat2.Visible=0:SpinDiscCat1.Visible=1
	Cat3Wall.IsDropped=False
	Cat3Down=False
    'Play a Sound?
    Cat3Dir = 2
    Cat3AnimTimer.Enabled = 1
	Cat3Up=True

End Sub

Sub Cat3MoveDown()
	Cat3SwirlTimer.Enabled=0: SpinDiscCat3.Visible=0 
	Cat3Wall.IsDropped=True
	Cat3Down=True
    Cat3Dir = -2
    Cat3AnimTimer.Enabled = 1
	Cat3Up=False
End Sub

Sub Cat3SwirlTimer_Timer
	SpinDiscCat3.rotz = (SpinDiscCat3.rotz + 5)mod 360
End Sub
'************
' Varitarget
'************

Sub StartVariArrowLights
'	Light_VariArrow1.State=1:Light_VariArrow2.State=1:Light_VariArrow3.State=1:Light_VariArrow4.State=1
End Sub

Sub StopVariArrowLights
'	Light_VariArrow1.State=0:Light_VariArrow2.State=0:Light_VariArrow3.State=0:Light_VariArrow4.State=0
End Sub

Dim variawarded, vtpos, varipos
variawarded = False
vtpos= Array(20,18,16,14,12,10,8,6,4,2,0,-2,-4,-6,-8,-10,-12,-14,-16,-18,-20,-22)

Sub vt_Hit(idx)
Dim x
debug.print "VariTargetHit"

'If idx = 21 Then StartTikiShake:StartChickenJump 'the last vt pos

If ActiveBall.VelY <0 Then
varipos = idx
	ActiveBall.VelY = ActiveBall.VelY * 0.915
    PlaySound "fx_solenoidoff"
    varitarget.roty = vtpos(idx)
Else
	If VTTimer.Enabled = False Then 
	 VTTimer.Interval = 300
	 VTTimer.Enabled = True
	End If
End If
End Sub

Sub VTTimer_Timer()
	VTTimer.Interval = 30
varipos = varipos -1
If varipos <0 Then varipos = 0
If varipos > 21 Then varipos = 21
    varitarget.roty = vtpos(varipos)
		If varipos = 19 AND variawarded = False Then 
			AddScore 5000: AddBonus 5:variawarded = True
		End If
		If varipos = 16 AND variawarded = False Then 
			AddScore 4000: variawarded = True
		End If
		If varipos = 13 AND variawarded = False Then 
			AddScore 3000: variawarded = True
		End If
		If varipos = 10 AND variawarded = False Then 
			AddScore 2000: variawarded = True
		End If
		If varipos = 7 AND variawarded = False Then 
			AddScore 1000: variawarded = True
		End If
		If varipos = 4 AND variawarded = False Then 
			AddScore 1000: variawarded = True
		End If
        If varipos = 0 Then
		  variawarded = False
        VTTimer.Enabled = False
		End If
End Sub

Sub TriggerFiFi_Hit 
	ObjLevel(4) = 1 : FlasherFlash4_Timer
	DOF 115,DOFPulse
debug.print "TriggerVariAwardHit"
	PlaySoundAt "fx_resetdrop",TriggerFiFi
	If FiFiActive(CurrentPlayer)=0 Then AddScore 20000:	DMD CL(0, ""), CL(1, " FIFI IS NOT HERE"), "DMD_FiFiBG", eBlink, eNone, eNone, 3000, True, "" :debug.print "FiFiActive(CurrentPlayer)=0":Exit Sub

	If LightVari3.State=2 Then 	DMD CL(0, "RALPHS A DAD"), CL(1, " "), "DMD_FiFiBG", eBlink, eNone, eNone, 3000, True, "":HelloFiFi
	If LightVari2.State=2 Then LightVari2.State=1:LightVari3.State=2:AddScore 100000:DMD CL(0, "YOU NEED A BATH"), CL(1, "    RALPH     "), "DMD_FiFiBG", eBlink, eNone, eNone, 3000, True, ""
	If LightVari1.State=2 Then LightVari1.State=1:LightVari2.State=2:AddScore 500000:	DMD CL(0, "  OO LA LA"), CL(1, "ITS FIFI"), "DMD_FiFiBG", eBlink, eNone, eNone, 3000, True, "":CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_OoLaLaThatsFiFi"
End Sub

Sub HelloFiFi
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_RalphyBoyYourADad"
	BackGlassMiniBlinkTimer.Enabled=1
	vpmtimer.addtimer 300, "ResetFiFi'":debug.print "ResetFiFi'"
	FiFiLevel(CurrentPlayer)=FiFiLevel(CurrentPlayer)+1
	If FiFiLevel(CurrentPlayer)=3 Then PrepareFiFiMultiball
	vpmtimer.addtimer 3200, "AdvanceBonoMeter'" '**************************************
End Sub

Sub PrepareFiFiMultiball
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_FiFiMultiballReady2"
	LightFiFiMultiballReady.State=1
	FiFiMultiballReady(CurrentPlayer)=1
End Sub

Sub StopFiFi
	LightVari3.State=0:LightVari2.State=0:LightVari1.State=0:FiFiActive(CurrentPlayer)=0:debug.print "FiFiStopped"
End Sub

Sub ResetFiFi
	LightVari3.State=0:LightVari2.State=0:LightVari1.State=2
End Sub

'*****Cat Target*****

Sub TargetSquirrel1_Hit()
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer

Targets_Hit(TargetSquirrel1)
	If LightSquirrel1.State=1 Then AddScore 100
	If LightSquirrel1.State=2 Then LightSquirrel1.State=1: CatSound:CheckChaseStatus
	If FastScoreCat(CurrentPlayer)=1 And LightSquirrel1.State=2 Then AddScore 20000
	If FastScoreCat(CurrentPlayer)=0 And LightSquirrel1.State=2 Then AddScore 10000

End Sub

Sub CatSound
	Dim CatSoundSelect
	CatSoundSelect = int(rnd*5)
	Select Case CatSoundSelect
		Case 0 : PlaySound "S_Cat1"
		Case 1 : PlaySound "S_Cat2"
		Case 2 : PlaySound "S_Cat3"
		Case 3 : PlaySound "S_Cat4"
		Case 4 : PlaySound "S_Cat5"
	End Select
End Sub

'*****Bird Target*****

Sub TargetSquirrel2_Hit()
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer
	Targets_Hit(TargetSquirrel2)
	If LightSquirrel2.State=1 Then AddScore 1000
	If LightSquirrel2.State=2 Then LightSquirrel2.State=1: BirdSound: AddScore 10000:CheckChaseStatus
	If FastScoreCat(CurrentPlayer)=1 And LightSquirrel2.State=2 Then AddScore 20000
	If FastScoreCat(CurrentPlayer)=0 And LightSquirrel2.State=2 Then AddScore 10000
End Sub

Sub BirdSound
	Dim BirdSoundSelect
	BirdSoundSelect = int(rnd*3)
	Select Case BirdSoundSelect
		Case 0 : PlaySound "S_Bird1"
		Case 1 : PlaySound "S_Bird2"
		Case 2 : PlaySound "S_BirdDogBark"
	End Select
End Sub


'*****SquirrelTarget*****
Sub TargetSquirrel3_Hit()
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer
	Targets_Hit(TargetSquirrel3)
		If LightSquirrel3.State=1 Then AddScore 1000
		If LightSquirrel3.State=2 Then LightSquirrel3.State=1: SquirrelSound: CheckChaseStatus
		If FastScoreCat(CurrentPlayer)=1 And LightSquirrel3.State=2 Then AddScore 20000
		If FastScoreCat(CurrentPlayer)=0 And LightSquirrel3.State=2 Then AddScore 10000
End Sub

Sub SquirrelSound
	Dim SquirrelSoundSelect
		SquirrelSoundSelect = int(rnd*4)
	Select Case SquirrelSoundSelect
		Case 0 : PlaySound "S_Squirrel10"
		Case 1 : PlaySound "S_Squirrel11"
		Case 2 : PlaySound "S_Squirrel12"
		Case 3 : PlaySound "S_Squirrel13"
	End Select
End Sub

'*****TurkeyTarget*****
Sub TargetSquirrel4_Hit()
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer
	Targets_Hit(TargetSquirrel4)
		If LightSquirrel4.State=1 Then AddScore 1000
		If LightSquirrel4.State=2 Then LightSquirrel4.State=1: TurkeySound: CheckChaseStatus
		If FastScoreCat(CurrentPlayer)=1 And LightSquirrel4.State=2 Then AddScore 20000
		If FastScoreCat(CurrentPlayer)=0 And LightSquirrel4.State=2 Then AddScore 10000
End Sub

Sub TurkeySound
	Dim TurkeySoundSelect
		TurkeySoundSelect = int(rnd*2)
	Select Case TurkeySoundSelect
		Case 0 : PlaySound "S_Turkey10"
		Case 1 : PlaySound "S_Turkey11"
	End Select
End Sub

'*****BallTarget*****
Sub TargetSquirrel5_Hit()
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer
Targets_Hit(TargetSquirrel5)
		If LightSquirrel5.State=1 Then AddScore 1000
		If LightSquirrel5.State=2 Then LightSquirrel5.State=1:DogSound:CheckChaseStatus
		If FastScoreCat(CurrentPlayer)=1 And LightSquirrel5.State=2 Then AddScore 20000
		If FastScoreCat(CurrentPlayer)=0 And LightSquirrel5.State=2 Then AddScore 10000
End Sub

Sub TargetSquirrel6_Hit()
	ObjLevel(3) = 1 : FlasherFlash3_Timer
	ObjLevel(4) = 1 : FlasherFlash4_Timer
	Targets_Hit(TargetSquirrel6)
	If LightSquirrel6.State=1 Then AddScore 1000
	If LightSquirrel6.State=2 Then LightSquirrel6.State=1:DogSound:CheckChaseStatus
		If FastScoreCat(CurrentPlayer)=1 And LightSquirrel6.State=2 Then AddScore 20000
		If FastScoreCat(CurrentPlayer)=0 And LightSquirrel6.State=2 Then AddScore 10000
End Sub



Dim SquirrelLightsActive

Sub CheckChaseStatus
	If SquirrelLightsActive=True And LightSquirrel1.State=1 And LightSquirrel2.State=1 And LightSquirrel3.State=1 And LightSquirrel4.State=1 And LightSquirrel5.State=1 And LightSquirrel6.State=1 Then 
	SquirrelLightsActive=False
	raisewall
	ChaseComplete
	End If
End Sub

Sub ChaseComplete
	AddScore 1000000
	CatsSorted(CurrentPlayer)=CatsSorted(CurrentPlayer)+1
	ChasesComplete(CurrentPlayer)=ChasesComplete(CurrentPlayer)+1
	If ChasesComplete(CurrentPlayer)=1 Then ChaseBonusLevel(CurrentPlayer)=2:LightN.State=1:LightU.State=2:CatsSorted(CurrentPlayer)=CatsSorted(CurrentPlayer)+6:vpmtimer.addtimer 3200, "AdvanceBonoMeter'":CatChaseAward1
	If ChasesComplete(CurrentPlayer)=2 Then ChaseBonusLevel(CurrentPlayer)=3:LightU.State=1:LightT.State=2:CatsSorted(CurrentPlayer)=CatsSorted(CurrentPlayer)+6:vpmtimer.addtimer 3200, "AdvanceBonoMeter'":CatChaseAward2
	If ChasesComplete(CurrentPlayer)=3 Then ChaseBonusLevel(CurrentPlayer)=4:LightT.State=1:LightS1.State=2:PrepareCatMultiball :CatsSorted(CurrentPlayer)=CatsSorted(CurrentPlayer)+6:CatsLoose:vpmtimer.addtimer 3200, "AdvanceBonoMeter'":CatChaseAward3
	If ChasesComplete(CurrentPlayer)=4 Then ChaseBonusLevel(CurrentPlayer)=5:LightS1.State=1:ResetSquirrelLights:ResetChaseBonusLights:CatsSorted(CurrentPlayer)=CatsSorted(CurrentPlayer)+6:vpmtimer.addtimer 3200, "AdvanceBonoMeter'":CatChaseAward4'***************
End Sub

Sub CatChaseAward1
	If FastScoreCat(CurrentPlayer)=0 Then Addscore 500000
	If FastScoreCat(CurrentPlayer)=1 Then AwardJackPot
End Sub

Sub CatChaseAward2
	If FastScoreCat(CurrentPlayer)=0 Then AwardJackPot
	If FastScoreCat(CurrentPlayer)=1 Then AwardJackPot2
End Sub

Sub CatChaseAward3
	If FastScoreCat(CurrentPlayer)=0 Then AwardJackPot2
	If FastScoreCat(CurrentPlayer)=1 Then AwardJackPot3
End Sub

Sub CatChaseAward4
	If FastScoreCat(CurrentPlayer)=0 Then AwardSuperJackpot
	If FastScoreCat(CurrentPlayer)=1 Then AwardSuperJackpot2
End Sub

Sub ActivateFastScoreCat
	LightFastScore1.State=1: FastScoreCat(CurrentPlayer)=1
End Sub

Sub PrepareCatMultiball
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_CatMultiballReady2"
	LightCatMultiballReady.State=1
	CatMultiballReady(CurrentPlayer)=1
End Sub
 
Sub DeactivateSquirrelLights
	SquirrelLightsActive=False
	Vpmtimer.addtimer 2000, "ResetStartofGameVariables'"
End Sub

Sub ResetSquirrelLights
	SquirrelLightsActive=True
	LightSquirrel1.State=2:LightSquirrel2.State=2:LightSquirrel3.State=2:LightSquirrel4.State=2:LightSquirrel5.State=2:LightSquirrel6.State=2

End Sub

Sub ResetChaseBonusLights
ChasesComplete(CurrentPlayer)=0
	LightN.State=2:LightU.State=0:LightT.State=0:LightS1.State=0
End Sub

'***********************
'SquirrelSpinner
'***********************
Sub SquirrelImageSpin_Timer
'	Debug.print "SquirrelSpinDiscEnabledatFishes"
	spindiscimg.rotz = spindiscimg.rotz + 4
	spindiscimg.Visible=True
End Sub


Sub spinning_timer

	spindiscimg.rotz = spindiscimg.rotz + 20
	DOF 124,DOFpulse 'Beacon on -for DogChaseSpinner
	DOF 119,DOFpulse	'Shaker on for dogChase'
	DOF 126,DOFpulse	'Fan on- for dogspinner)
End Sub

dim spinner
Set spinner = New cvpmTurntable
With spinner
	.InitTurntable spindisc, 100
	.SpinDown = 20
	.CreateEvents "spinner"
End With
spinner.MotorOn = false

'****************************************
'Raisewall Sub'
'****************************************
Sub raisewall	
	TriggerTrapped.Enabled=0
	AddScore 1000000
	'		if bMultiBallMode = true then exit Sub
	'------------------------------------------------
	SquirrelSpinnerWall.IsDropped= False
	'------------------------------------------------
	spinner.MotorOn = true
	spinning.enabled = True
	vpmtimer.addtimer 3000, "dropwall '"
End Sub

Sub StopMotorTimer
	MotorSpinnerTimer.Enabled=False
End Sub


'*************************
'LiftWall
'*************************
Sub LiftWall
	SquirrelSpinnerWall.IsDropped= False
	TriggerTrapped.Enabled=0
	debug.print "LiftWall"
End Sub



'*************************************
'dropwall
'************************************
Sub dropwall
	SquirrelSpinnerWall.IsDropped= True
	spinner.MotorOn = false
	spinning.enabled = false
	vpmtimer.addtimer 1000, "ResetSquirrelLights'"
End Sub

'*************************************
'StopMotorSpinner
'**************************************
Sub 	StopSpinnerMotor
	debug.print "StopMotor"
	spinner.MotorOn = false
	spinning.enabled = false
	spindiscimg.rotz = spindiscimg.rotz + 10
End Sub



'****************************************
' Enable Kicker in case a ball gets stuck This is enable for 6 seconds after the ball is spun out

Sub TriggerTrappedStartTimer_Timer()
	EnableTriggerTrapped
	debug.print "TriggerTrappedTimerEnabled"
End Sub


Sub EnableTriggerTrapped
	TriggerTrapped.Enabled=1
	debug.print "TriggerTrappedEnabled"
End Sub

Sub TriggerTrapped_Hit()
	dropwall
'	vpmtimer.addtimer 500, "LiftWall '"
End Sub


Sub DisableTriggerTrapped
	TriggerTrapped.Enabled=0	
End sub

'******* ShortPlungeTargets*****

Sub Target1_Hit()
	ObjLevel(2) = 1 : FlasherFlash2_Timer
	Targets_Hit(Target1)
	If LightTree1.State=2 Then LightTree1.State=1: AddScore 10000:CheckMysteryActivate
End Sub

Sub Target2_Hit()
	ObjLevel(2) = 1 : FlasherFlash2_Timer
	Targets_Hit(Target2)
	If LightTree2.State=2 Then LightTree2.State=1:AddScore 10000: CheckMysteryActivate
End Sub

Sub Target3_Hit()
	ObjLevel(2) = 1 : FlasherFlash2_Timer
	Targets_Hit(Target3)
	If LightTree3.State=2 Then LightTree3.State=1: AddScore 10000:CheckMysteryActivate
End Sub

Sub CheckMysteryActivate
	If LightTree1.State=1 And LightTree2.State=1 And LightTree3.State=1 And MysteryActive(CurrentPlayer)=0 Then MysteryActivate
End Sub


Sub MysteryActivate
	LightMystery.State=2
	MysteryActive(CurrentPlayer)=1
End Sub

Sub ResetMysterLights
	LightTree1.State=2:LightTree2.State=2:LightTree3.State=2
	LightMystery.State=0
	MysteryActive(CurrentPlayer)=0
End Sub

Sub TargetMystery_Hit()
	Targets_Hit(TargetMystery)
	If LightMystery.State=2 Then MysteryAward:ResetMysterLights:ObjLevel(2) = 1:FlasherFlash2_Timer
End Sub



Sub MysteryAward
	Dim MysterySelect
		MysterySelect = int(rnd*10)
	Select Case MysterySelect
		Case 0 : AdvanceBonoMeter:debug.print"Case0"
		Case 1 : ActivateFiFi:DMD CL(0, "    HELLO FIFI"), CL(1, "OO LA LA "), "DMD_FiFiBG", eNone, eNone, eNone, 3000, True, "":debug.print"Case1"
		Case 2:  CatsLoose:debug.print"Case2"
		Case 3 : ActivateFastScoreSheep:DMD CL(0, "FAST SCORE"), CL(1, "SHEEP"), "DMD_SheepBG", eNone, eNone, eNone, 3000, True, "":debug.print"Case3"
		Case 4 : SuperSheep:debug.print"Case4"
		Case 5 : DoctorDog:debug.print"Case5"
		Case 6:  If LightCatMultiballReady.State=0 Then : PrepareCatMultiball:DMD CL(0, "CAT MULTIBALL"), CL(1, "READY"), "DMD_CatBG", eNone, eNone, eNone, 3000, True, "":debug.print"Case6"
		Case 7 : If LightFiFiMultiballReady.State=0 Then PrepareFiFiMultiball:DMD CL(0, "FIFI MULTIBALL"), CL(1, "READY"), "DMD_FiFiBG", eNone, eNone, eNone, 3000, True, "":debug.print"Case7"
		Case 8 : If LightSheepMultiballReady.State=0 Then PrepareSheepMultiball:DMD CL(0, "SHEEP MULTIBALL"), CL(1, "READY"), "DMD_SheepBG", eNone, eNone, eNone, 3000, True, "":debug.print"Case8"
		Case 9 : ActivateFastScoreCat:DMD CL(0, "FAST SCORE"), CL(1, "SHEEP"), "DMD_SheepBG", eNone, eNone, eNone, 3000, True, "":debug.print"Case3"
	End Select
End Sub



Sub TriggerSkillshot1b_Hit()
	TurnoffSkillShotGi
	vpmtimer.addtimer 2000, "TurnOnSkillShotGi'"
End Sub

Sub TurnoffSkillShotGi
	gi13.State=0:gi14.State=0:gi43.State=0:gi44.State=0:gi47.State=0
End Sub

Sub TurnOnSkillShotGi
	gi13.State=1:gi14.State=1:gi43.State=1:gi44.State=1:gi47.State=1
End Sub



'*****WeeTarget*****

Sub target25_Hit()
	LightPFDogWhizzer.State=2
	LightPFDogWhizzer.TimerEnabled=1
	WhizzerCount(CurrentPlayer)=WhizzerCount(CurrentPlayer)+1
	LightTarget25.State=2:WeeSound
	Targets_Hit(target25)
'	PlaySoundAt "fx_target",Target25
End Sub

Sub LightPFDogWhizzer_Timer
		LightPFDogWhizzer.State=0
		LightPFDogWhizzer.TimerEnabled=0
End Sub

Sub WeeSound
	Dim WeeSoundSelect
		WeeSoundSelect = int(rnd*2)
	Select Case WeeSoundSelect
		Case 0 : PlaySound "S_Wee1"
		Case 1 : PlaySound "S_Wee2"
	End Select

End Sub

Sub Target26_Hit()
	LighPFDogGreyRoo.State=2
	LightPFPoo.State=2
	LighPFDogGreyRoo.TimerEnabled=1
	Targets_Hit(Target26)
	FartSound
	LightTarget26.State=2
	GreyKangarooCount(CurrentPlayer)=GreyKangarooCount(CurrentPlayer)+1
End Sub

Sub FartSound
	Debug.print "FartSound"
	Dim FartSoundSelect
	FartSoundSelect = int(rnd*5)
	Select Case FartSoundSelect
		Case 0 : PlaySound "S_Fart10":AddScore 2000 
		Case 1 : PlaySound "S_Fart11":AddScore 2000 
		Case 2 : PlaySound "S_Fart12":AddScore 2000 
		Case 3 : PlaySound "S_Fart13":AddScore 2000 
		Case 4 : PlaySound "S_Fart14":AddScore 2000 
	End Select
End Sub 

Sub LighPFDogGreyRoo_Timer
		LighPFDogGreyRoo.State=0
		LightPFPoo.State=0
		LighPFDogGreyRoo.TimerEnabled=0
End Sub

Sub FartJackpot
	AwardJackpot
End Sub



Sub TargetDig1_hit()
	TargetDig1.IsDropped=1
	SoundDropTargetDrop (TargetDig1)
	LightDig1.State=1: DogSound:CheckDigScore:CheckDigWallStatus
End Sub

Sub TargetDig2_hit()
	TargetDig2.IsDropped=1
	SoundDropTargetDrop (TargetDig2)
	LightDig2.State=1: DogSound:CheckDigScore:CheckDigWallStatus
End Sub

Sub TargetDig3_hit()
	TargetDig3.IsDropped=1
	SoundDropTargetDrop (TargetDig3)
	LightDig3.State=1: DogSound:CheckDigScore:CheckDigWallStatus
End Sub

Sub TargetDig4_hit()
	TargetDig4.IsDropped=1
	SoundDropTargetDrop (TargetDig4)
	LightDig4.State=1: DogSound:CheckDigScore:CheckDigWallStatus
End Sub

Sub TargetDig5_hit()
	TargetDig5.IsDropped=1
	SoundDropTargetDrop (TargetDig5)
	LightDig5.State=1: DogSound:CheckDigScore:CheckDigWallStatus
End Sub

Sub CheckDigScore
	ObjLevel(1) = 1 : FlasherFlash1_Timer
	If FastScoreActive=0 Then AddScore 10000:vpmtimer.addtimer 1000, "DMDScoreNow'"
	If FastScoreActive=1 Then AddScore 20000:vpmtimer.addtimer 1000, "DMDScoreNow'"
End Sub

Sub CheckDigWallStatus
	If TargetDig1.IsDropped=1 And TargetDig2.IsDropped=1 And TargetDig3.IsDropped=1 And TargetDig4.IsDropped=1 And TargetDig5.IsDropped=1 Then
		LightFence.State=2
		HolesAreDug(CurrentPlayer)=1
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_MightFineDigging2"
	End If
End Sub

'*****If Targets are complete then bonus multiplyer is increased*****

Sub DigitKickerWall_Hit

	ObjLevel(1) = 1 : FlasherFlash1_Timer
		
	If HolesAreDug(CurrentPlayer)=0 Then 
		PlaySoundAt "Target_Hit_1", TargetDig3:debug.print "FenceNotReady HolesNotDug"
	End if
	If 	HolesAreDug(CurrentPlayer)=1 Then 
		CheckHoleDigStatus
		PlaySound "CO_GatesOpen"
		LightPFScruffyDog.State=2
		LightPFScruffyDog.TimerEnabled=1
	If FastScoreActive=0 Then AddScore 100000
	If FastScoreActive=1 Then AddScore 200000
		HolesAreDug(CurrentPlayer)=0:debug.print "FenceWallDeactivated"
		DigitKickerWall.IsDropped=True	
		PlaySoundAt "fx_target", TargetDig3
		LightFence.State=1:	LightDigKickerReady.State=2	
'	DMD CL(0, "    FENCE IS DOWN"), CL(1, "    WOO HOO"), "DMD_King2BG", eBlink, eNone, eNone, 3000, True, ""
		AdvanceKing
	End if
End Sub

Sub TriggerGiDig_Hit()
	GiDiggitOff
End Sub

Sub GiDiggitOff
	gi15.State=0:gi16.State=0:gi24.State=0:gi25.State=0:gi31.State=0:gi32.State=0:gi33.State=0
	vpmtimer.addtimer 1200, "GiDiggitOn'"
End Sub

Sub GiDiggitOn
	gi15.State=1:gi16.State=1:gi24.State=1:gi25.State=1:gi31.State=1:gi32.State=1:gi33.State=1
End Sub

Sub KickerDigit_Hit
	DOF 137,DOFPulse
	GiOff
	LightPFScruffyDog.State=2
	LightPFScruffyDog.TimerEnabled=1
	BackGlassDogsBlinkTimer.Enabled=1
	ObjLevel(1) = 1 : FlasherFlash1_Timer
	KingDogActive(CurrentPlayer)=1	
	ActivateFiFi
	KingDogUp
	KingDogReward
'	EnableKingDogFastScoring
	TriggerSpikeStatue.Enabled=1:debug.print "TriggerShakeSpikeStatueEnabled"
	ShakeSpike.Enabled=1
	KingDogFastScoring.Enabled=1:LightFastScore2.State=1:FastScoreKing(CurrentPlayer)=1
	DigTargetsReset
	PlaySoundAt "popper_ball", KickerDigit
	KickerDigit.DestroyBall
	UpkickerRelease.Enabled=1
	vpmtimer.addtimer 800, "UpKickerReleaseSound'"
	vpmtimer.addtimer 1000, "CreateNewBallUpKickerRelease'"
	debug.print "Upkicker1Catch"
	vpmtimer.addtimer 3500, "KingDogRampReadyDMD'"
	vpmtimer.addtimer 7000, "AdvanceBonoMeter '" '*******************
End Sub

Sub KingDogRampReadyDMD
	GiOn
	DMD CL(0, "       KING DOG "), CL(1, "      RAMP READY "), "DMD_King2BG", eBlink, eNone, eNone, 3000, True, ""
End Sub

Sub ActivateFiFi
	FiFiActive(CurrentPlayer)=1:LightVari1.State=2:debug.print "FiFiActive(CurrentPlayer)=1"
End Sub

Sub EnableKingDogFastScoring
	KingDogFastScoring.Enabled=1:LightFastScore2.State=2:FastScoreKing(CurrentPlayer)=1
End Sub



Sub KingDogUp
	DOF 133,DOFPulse
For each xx in RampFlashOn: xx.image = "Ramp Texture Upper Left-RedPaws":Next
	SpikeTheDog.TransZ=210:SpikeGlowTimer.Enabled=1:ShakeSpike.Enabled=1
	debug.print "KingDogUp"
	SpikeReady(CurrentPlayer)=1
End Sub

Dim DogGlow:DogGlow=0
Dim xx
Sub SpikeGlowTimer_Timer()
	DogGlow=DogGlow+1
	If DogGlow=1 Then	For each xx in DogStatue1on: xx.image = "DogTextureon":Next
	If DogGlow=4 Then	For each xx in DogStatue1on: xx.image = "DogTextureon":Next
	If DogGlow=5 Then	For each xx in DogStatue1on: xx.image = "DogTextureoff":Next	
	If DogGlow=7 Then	For each xx in DogStatue1on: xx.image = "DogTextureoff":DogGlow=0:Next
End Sub


Sub KingDogDown
	DOF 133,DOFPulse
For each xx in RampFlashOn: xx.image = "Ramp Texture Upper Left-Cyan":Next
	SpikeTheDog.TransZ=-210:SpikeGlowTimer.Enabled=0
	KingDogFastScoring.Enabled=0:LightFastScore2.State=0:FastScoreKing(CurrentPlayer)=0:TriggerSpikeStatue.Enabled=0:debug.print "ShakeSpikeDeactivated"
	ShakeSpike.Enabled=0
	KingDogRamps(CurrentPlayer)=0
	debug.print "KingDogDown"
End Sub

Sub KingDogFastScoring_Timer()
	KingDogDown
	SpikeReady(CurrentPlayer)=0
	KingDogFastScoring.Enabled=0
	LightFastScore2.State=0
	FastScoreKing(CurrentPlayer)=0
End Sub 

'******When Dig Wall is Hit Prepare next letter light Flashing*******

Sub AdvanceKing
	debug.print "Advance King"
	If LightI.State=1 And LightK.State=0 Then LightK.State=2:CheckExtraBallHurryUp
	If LightN1.State=1 And LightI.State=0 Then LightI.State=2
	If LightG.State=1 And  LightN1.State=0 Then LightN1.State=2
	If LightG.State=0 Then LightG.State=2
End Sub

'******When the kicker saucer is hit The flashing letter is achieved and lightstate is 1*****
'******After KING is achieved player KING lights stay at 1 for rest of game but bonus multiplyer can continue to rise to 20x Each subsequent kicker saucer is a SuperJackPot2

Sub KingDogReward
	StartMajorAwardSequence
	If KingDogCount(CurrentPlayer)=5 Then AwardSuperJackpot2:Exit Sub
	KingDogCount(CurrentPlayer)=KingDogCount(CurrentPlayer)+1
	If KingDogCount(CurrentPlayer)=1 Then AddScore 200000
	If KingDogCount(CurrentPlayer)=2 Then AddScore 300000
	If KingDogCount(CurrentPlayer)=3 Then AddScore 400000:StartDogSequence
	If KingDogCount(CurrentPlayer)=4  Then AddScore 500000:KingDogCount(CurrentPlayer)=5
	If LightK.State=2 Then LightK.State=1:KingDogCount(CurrentPlayer)=0:KingDogActive(CurrentPlayer)=1:PrepareForKingMultiball:KingKAchieved(CurrentPlayer)=1
	If LightI.State=2 Then LightI.State=1:KingIAchieved(CurrentPlayer)=1
	If LightN1.State=2 Then LightN1.State=1:KingNAchieved(CurrentPlayer)=1
	If LightG.State=2 Then LightG.State=1:KingGAchieved(CurrentPlayer)=1
End Sub

Sub PrepareForKingMultiball
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_KingMultiballReady2"
	LightKingMultiballReady.State=1
	KingDogMultiballReady(CurrentPlayer)=1
End Sub


Sub ResetKingLights
	LightK.state=0:LightI.state=0:LightN1.State=0:LightG.State=0
	KingDogActive(CurrentPlayer)=0
	LightKingMultiballReady.State=0
	KingDogMultiballReady(CurrentPlayer)=0
End Sub

Sub DogSound
	Dim SfxSelect
	SfxSelect = int(rnd*8)
	Select Case SfxSelect
		Case 0 : PlaySound "S_Dog10"
		Case 1 : PlaySound "S_Dog11"
		Case 2 : PlaySound "S_Dog12"
		Case 3 : PlaySound "S_Dog13"
		Case 4 : PlaySound "S_Dog14"
		Case 5 : PlaySound "S_Dog15"
		Case 6 : PlaySound "S_Dog16"
		Case 7 : PlaySound "S_Dog17"
	End Select
End Sub

Sub DigTargetsReset
	TargetDig1.IsDropped=False:TargetDig2.IsDropped=False:TargetDig3.IsDropped=False:TargetDig4.IsDropped=False:TargetDig5.IsDropped=False
	DigitKickerWall.IsDropped=False
	LightDig1.State=2:LightDig2.State=2:LightDig3.State=2:LightDig4.State=2:LightDig5.State=2
	LightFence.State=0:LightDigKickerReady.State=0
	PlaySoundAt "fx_target",TargetDig3
End Sub

Sub CheckDigTargetsNewBall
	If LightDig1.State=1 Then TargetDig1.IsDropped=True
	If LightDig2.State=1 Then TargetDig2.IsDropped=True
	If LightDig3.State=1 Then TargetDig3.IsDropped=True
	If LightDig4.State=1 Then TargetDig4.IsDropped=True
	If LightDig5.State=1 Then TargetDig5.IsDropped=True
	If LightDig1.State=2 Then TargetDig1.IsDropped=False
	If LightDig2.State=2 Then TargetDig2.IsDropped=False
	If LightDig3.State=2 Then TargetDig3.IsDropped=False
	If LightDig4.State=2 Then TargetDig4.IsDropped=False
	If LightDig5.State=2 Then TargetDig5.IsDropped=False
	If LightFence.State=1 Then DigitKickerWall.IsDropped=True:LightDigKickerReady.State=2	
	If LightFence.State=2 Then DigitKickerWall.IsDropped=False
End Sub

'Completing Dig Targets increases the Bonus multiplyer
Sub CheckHoleDigStatus
If TargetDig1.IsDropped=1 And TargetDig2.IsDropped=1 And TargetDig3.IsDropped=1 And TargetDig4.IsDropped=1 And TargetDig5.IsDropped=1 Then 
	DMD CL(0, "      GATE IS"), CL(1, "     OPEN"), "DMD_King2BG", eBlink, eNone, eNone, 4000, True, ""
	IncreaseBonusMultiplier
	HolesAreDug(CurrentPlayer)=1:debug.print "HolesAreDug..FenceReady"
	debug.print "HolesComplete"
	LightFence.State=2
	AddScore 1000000
	HolesComplete(CurrentPlayer)=HolesComplete(CurrentPlayer)+1
End If
End Sub

Sub IncreaseBonusMultiplier
	If LightBonus20X.State=2 Then LightBonus20X.State=1:DigBonusLevel(CurrentPlayer)=20:BonusMultiplier(CurrentPlayer)=20:DMD CL(0, "BONUS LEVEL 20"), CL(1, "WOO HOO"), "DMD_Title25", eBlink, eNone, eNone, 3000, True, ""
	If LightBonus10X.State=2 Then LightBonus10X.State=1:LightBonus20X.State=2:DigBonusLevel(CurrentPlayer)=10:BonusMultiplier(CurrentPlayer)=10:DMD CL(0, "BONUS LEVEL 10"), CL(1, "LETS GO RALPH"), "DMD_Title25", eBlink, eNone, eNone, 3000, True, ""
	If LightBonus5X.State=2 Then LightBonus5X.State=1:LightBonus10X.State=2:DigBonusLevel(CurrentPlayer)=5:BonusMultiplier(CurrentPlayer)=5:DMD CL(0, "BONUS LEVEL 5"), CL(1, "TREAT TIME"), "DMD_Title25", eBlink, eNone, eNone, 3000, True, ""
	If LightBonus3X.State=2 Then LightBonus3X.State=1:LightBonus5X.State=2:DigBonusLevel(CurrentPlayer)=3:BonusMultiplier(CurrentPlayer)=3:DMD CL(0, "BONUS LEVEL 3"), CL(1, "GO DOG GO"), "DMD_Title25", eBlink, eNone, eNone, 3000, True, ""
	If LightBonus2X.State=2 Then LightBonus2X.State=1:LightBonus3X.State=2:DigBonusLevel(CurrentPlayer)=2:BonusMultiplier(CurrentPlayer)=2:DMD CL(0, "BONUS LEVEL 2"), CL(1, "GO RALPH"), "DMD_Title25", eBlink, eNone, eNone, 3000, True, ""
End Sub

Sub ResetHoleDigTargets
	LightDig1.State=2:LightDig2.State=2:LightDig3.State=2:LightDig4.State=2:LightDig5.State=2
	LightFence.State=0:LightDigKickerReady.State=0
End Sub

Sub ResetDigBonusLights
	LightBonus2X.State=2:LightBonus3X.State=0:LightBonus5X.State=0:LightBonus10X.State=0:LightBonus20X.State=0
End Sub


Sub TriggerSpikeStatue_Hit()
	KingDogRamps(CurrentPlayer)=KingDogRamps(CurrentPlayer)+1
	debug.print "TriggerSakeSpikeState_Hit"
	StartSpikeShake:debug.print "StartSpikeShake"
'	KingDogAward
End Sub

Sub KingDogAward
	If KingGAchieved(CurrentPlayer)=0 And FastScoreKing(CurrentPlayer)=0 Then AddScore 10000
	If KingGAchieved(CurrentPlayer)=0 And FastScoreKing(CurrentPlayer)=1 Then AddScore 20000
	If KingGAchieved(CurrentPlayer)=1 And KingNAchieved(CurrentPlayer)=0 Then GAward
	If KingNAchieved(CurrentPlayer)=1 And KingIAchieved(CurrentPlayer)=0 Then NAward
	If KingIAchieved(CurrentPlayer)=1 And KingKAchieved(CurrentPlayer)=0 Then IAward
	If KingKAchieved(CurrentPlayer)=1 Then KAward
End Sub

Sub GAward
	debug.print "GAward"
	If FastScoreKing(CurrentPlayer)=0 Then AddScore 40000
	If FastScoreKing(CurrentPlayer)=1 Then AddScore 100000
End Sub

Sub NAward

	debug.print "NAward"	
	If FastScoreKing(CurrentPlayer)=0 Then AddScore 60000
	If FastScoreKing(CurrentPlayer)=1 Then AddScore 200000
End Sub

Sub IAward
	debug.print "IAward"
	If FastScoreKing(CurrentPlayer)=0 Then AddScore 80000
	If FastScoreKing(CurrentPlayer)=1 Then AwardJackpot
End Sub

Sub KAward
	debug.print "KAward"
	If FastScoreKing(CurrentPlayer)=0 Then AddScore 200000
	If FastScoreKing(CurrentPlayer)=1 Then AwardSuperJackpot
End Sub



' SpikeShake

Dim SpikeShake:SpikeShake = 0
Dim SpikeREShake:SpikeREShake=0
Dim SpikeLEShake:SpikeLEShake=0

Sub StartSpikeShake
	debug.print "StartShakeSpike"
    SpikeShake = 1:SpikeREShake=1:SpikeLEShake=1
    ShakeSpike.Enabled = True

End Sub

Sub ShakeSpike_Timer
	debug.print "ShakeSpikeActive"
    SpikeTheDog.Roty = SpikeShake
'	pSpikeRightEye.TransX=SpikeREShake
'	pSpikeLeftEye.TransX=SpikeLEShake
    If SpikeShake = 0 Then ShakeSpike.Enabled = False:Exit Sub
    If SpikeShake <0 Then

        SpikeShake = ABS(SpikeShake)- 0.1
		SpikeREShake= ABS(SpikeREShake)- 0.1
		SpikeLEShake= ABS(SpikeLEShake)-1
    Else
        SpikeShake = - SpikeShake + 0.1
		SpikeREShake= -SpikeREShake + 0.1
		SpikeLEShake= -SpikeREShake -0.1
    End If
End Sub

Sub UpkickerCatch_hit()
	DOF 113 ,DOFPulse
	SoundSaucerLock
	wirerampoff
'	PlaySoundAt "popper_ball", UpkickerCatch
	UpkickerCatch.DestroyBall
	UpkickerRelease.Enabled=1
	vpmtimer.addtimer 800, "UpKickerReleaseSound'"
	vpmtimer.addtimer 1000, "CreateNewBallUpKickerRelease'"
	debug.print "Upkicker1Catch"
End Sub

Sub UpKickerReleaseSound
	PlaySoundAt "popper" , UpkickerRelease
End Sub


Sub CreateNewBallUpKickerRelease   '<<<<  UpKickerRelease
		SoundSaucerKick 1, UpKickerRelease	
		ObjLevel(5) = 1 : Flasherflash5_Timer
		UpkickerRelease.CreateSizedball BallSize / 2	
		UpKickerRelease.Kick 250, 10
		debug.print "CreateBallUpkickerRelease"
		UpkickerRelease.Enabled=0
	If KingDogActive(CurrentPlayer)=1 Then 
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:RalphSledge
	End If
		DOF 123, DOFPulse
End Sub

Sub RalphSledge
		SledgeSelect = SledgeSelect +1
	Select Case SledgeSelect
		Case 1: PlaySound "CO_HereIComeSpikeyBoy"
		Case 2 PlaySound "CO_CopThatOneSpikeyBoy"
		Case 3: PlaySound "CO_FlappyChops"
		Case 4: PlaySound "CO_YouveBeenRalphed"
		Case 5: PlaySound "CO_ImTheKing"
		Case 6: PlaySound "CO_DoublePawBounce":SledgeSelect=0
	End Select

End Sub



'**********************************************************************************
'Skillshot Kicker destroys ball and creates ball at right outlane portal
'This is acombined Openingshot portal that awards a skillhot if v the light is lit

'***********************************************************************************
'********************
'SkillShot1
'********************

'*************
'  KICKBACKS 
'*************
' 

'kickbackl(CurrentPlayer), kickbackr(CurrentPlayer)

Sub closekickbacks
'	kickbacklg.open = False
'	LightLeftescape.State = 0
'	kickbackrg.open = False
'	Lightrightescape.State = 0
'	LightLeftInlane.State = 0
'	LightRightInlane.State = 0
End Sub

Sub kickbackleftenabled
	kickbacklg.open = True
	LightLeftOL.State = 2:LightLeftIL.State=0
	PlaySoundAt "Kickback2",kickbacklg
End Sub

Sub kickbackleftdisabled
	kickbacklg.open = False
	LightLeftIL.State =2
	LightLeftOL.State =0
End Sub



Sub Kicker11_hit
	ChickenSound
	ObjLevel(6) = 1 : FlasherFlash6_Timer	
	AddScore 10
SoundSaucerLock
'	PlaySoundAt SoundFXDOF("Popper", 152, DOFPulse, DOFContactors), Kicker11
	vpmtimer.addtimer 1000, "LeftKickBack'"	
End Sub

'	


Sub ChickenSound
	Debug.print "ChickenSound"
	Dim ChickenSoundSelect
	ChickenSoundSelect = int(rnd*4)
	Select Case ChickenSoundSelect
		Case 0 : DMD CL(0, "  WANNA PLAY"), CL(1, "CHICKEN"), "DMD_Title25", eBlink, eNone, eNone, 3000, True, "":  PlaySound "S_Chicken1":AddScore 20000
		Case 1 : DMD CL(0, " EYES ARE BUGGIN"), CL(1, "CHOOKY"), "DMD_Title25", eBlink, eNone, eNone, 3000, True, "":  PlaySound "S_Chicken1":AddScore 20000
		Case 2 : DMD CL(0, "    GOT YA"), CL(1, "DRUM STICKS"), "DMD_Title25", eBlink, eNone, eNone, 3000, True, "":PlaySound "S_Chicken3":AddScore 2000
		Case 3 : DMD CL(0, "  IN THE PEN"), CL(1, "FEATHERS"), "DMD_Title25", eBlink, eNone, eNone, 3000, True, "":PlaySound "S_Chicken4":AddScore 2000

	End Select
End Sub 

Sub LeftKickBack
	ObjLevel(6) = 1 : FlasherFlash6_Timer	
	SoundSaucerKick 1, Kicker11
	DOF 118 ,DOFPulse
	Kicker11.Kick 0, 35
	vpmtimer.addtimer 300, "kickbackleftdisabled'"
	LastSwitchHit = "Kicker11"
	DMD CL(0, ""), CL(1, "    LUCKY DOG"), "DMD_DOG4BG", eBlink, eNone, eNone, 3000, True, ""
	AddScore 10
End Sub


Dim ROLDiverterOpen
Sub kickbackrightenabled
	LightRightOL.State = 2:LightRightIL.State=1
	Diverter.rotatetoend
	PlaySoundAt "fx_diverter" , Diverter
	ROLDiverterOpen=1
End Sub

Sub kickbackrightdisabled

	LightRightIL.State=2:LightRightOL.State=0
		Diverter.rotatetostart
		PlaySoundAt "fx_diverter" , Diverter
End Sub


'***********************************
'Lanes
'*************************************


Sub lane1_hit
	If 	LightLeftOL.State=0 Then PlaySound "S_Moo":DMD CL(0, "     LEAVE"), CL(1, "THE COW ALONE"), "DMD_CowBG", eBlink, eNone, eNone, 3000, True, ""
	If	LastSwitchHit = "Kicker11" Then:Exit Sub
	If Tilted Then Exit Sub
	LastSwitchHit = "lane1"
'	LightLeftEscape.State=0
'	LightLeftInlane.State = 0
End Sub

Sub TriggerOutlane1KickerCheck_Hit
	LastSwitchHit = "TriggerOutlane1KickerCheck"
End Sub

Sub TriggerOutlane4KickerCheck_Hit
	LastSwitchHit = "TriggerOutlane4KickerCheck"
End Sub


Sub lane2_hit 
	AddScore 50000

	If Tilted Then Exit Sub
	LastSwitchHit = "lane2"
	If 	LightLeftIL.State = 2 Then kickbackleftenabled

End Sub

Sub TriggerLeftSlingGiControl_Hit()
	GiLeftLaneOff
	vpmtimer.addtimer 1000, "GiLeftLaneOn'"
End Sub

Sub lane3_hit
	AddScore 50000

	If Tilted Then Exit Sub 
	LastSwitchHit = "lane3"
	If 	LightRightIL.State = 1 Then LightRightIL.State = 2
	If 	LightRightIL.State = 2 Then LightRightIL.State = 1:LightRightOL.State=2:kickbackleftenabled
	kickbackrightenabled
End Sub

Sub TriggerRightSlingGiControl_Hit()
	GiRightLaneOff
	vpmtimer.addtimer 500, "GiRightLaneOn'"
End Sub

Sub lane4_hit 
	If Tilted Then Exit Sub
	LastSwitchHit = "lane4"
If 	ROLDiverterOpen=1 Then TriggerDiverterJustUsed.Enabled=1:RightSideDiverterUsed=1
If	LightRightOL.State=0 Then PlaySound "S_Moo":DMD CL(0, "GET OUT OF"), CL(1, "THE GARDEN"), "", eBlink, eNone, eNone, 3000, True, ""
If	LightRightOL.State=2 Then Vpmtimer.addtimer 1000, "kickbackrightdisabled'"	:DMD CL(0, "GET OUT OF THERE"), CL(1, "YA MUTT    "), "", eBlink, eNone, eNone, 3000, True, ""
End Sub

Sub GiRightLaneOff
	gi9.State=0:gi10.State=0:gi44.State=0:gi45.State=0
	PlaySoundAt "fx_relay", lane3
End Sub

Sub GiRightLaneOn
	gi9.State=1:gi10.State=1:gi44.State=1:gi45.State=1
	PlaySoundAt "fx_relay", lane3
End Sub

Sub GiLeftLaneOff
	gi1.State=0:gi2.State=0:gi11.State=0:gi12.State=0
	PlaySoundAt "fx_relay", lane2
End Sub

Sub GiLeftLaneOn
	gi1.State=1:gi2.State=1:gi11.State=1:gi12.State=1
	PlaySoundAt "fx_relay", lane2
End Sub


Sub TriggerDiverterJustUsed_Hit()
	TriggerDiverterJustUsed.Enabled=0
	RightSideDiverterUsed=0
End Sub 




'******************************************************************************


'LightsOff
	pSheepBoard1Off.BlendDisableLighting =1
	pSheepBoard2Off.BlendDisableLighting =1
	pSheepBoard3Off.BlendDisableLighting =1
	pSheepBoard4Off.BlendDisableLighting =1
	pSheepBoard5Off.BlendDisableLighting =1
	pSheepBoard6Off.BlendDisableLighting =1
	pSheepBoard7Off.BlendDisableLighting =1
	pSheepBoard8Off.BlendDisableLighting =1
	pCatapault1Off.BlendDisableLighting =1
	pCatapault2Off.BlendDisableLighting =1
	pLight_BallSaveroff.blenddisablelighting = 1
	pSquirrel1off.blenddisablelighting = 1
	pSquirrel2off.blenddisablelighting = 1
	pSquirrel3off.blenddisablelighting = 1
	pSquirrel4off.blenddisablelighting = 1
	pSquirrel5off.blenddisablelighting = 1
	pSquirrel6off.blenddisablelighting = 1
	pFenceoff.blenddisablelighting = 1
	pLightDigKickerReadyon.blenddisablelighting = 1
	pKingMultibalReadyoff.blenddisablelighting = 1
	pCatMultibalReadyoff.blenddisablelighting = 1
	pFiFiMultibalReadyoff.blenddisablelighting = 1
	pSheepMultibalReadyoff.blenddisablelighting = 1
	pBonus2off.blenddisablelighting = 1
	pBonus3off.blenddisablelighting = 1
	pBonus5off.blenddisablelighting = 1
	pBonus10off.blenddisablelighting = 1
	pBonus20off.blenddisablelighting = 1
	pLightShootAgainoff.blenddisablelighting = 1
	pLightDig1off.blenddisablelighting = 1
	pLightDig2off.blenddisablelighting = 1
	pLightDig3off.blenddisablelighting = 1
	pLightDig4off.blenddisablelighting = 1
	pLightDig5off.blenddisablelighting = 1
	pLightTree1off.blenddisablelighting = 1
	pLightTree2off.blenddisablelighting = 1
	pLightTree3off.blenddisablelighting = 1
	pLightRightILoff.blenddisablelighting = 1
	pLightRightOLoff.blenddisablelighting = 1
	pLightLeftILoff.blenddisablelighting = 1
	pLightLeftOLoff.blenddisablelighting = 1
	pLightKingDogReadyoff.blenddisablelighting = 1
	pLightExtraBalloff.blenddisablelighting = 1
	pLightFastScoringActivateoff.blenddisablelighting = 1
	pLightLocksActivateoff.blenddisablelighting = 1
	pLightLocksReadyoff.blenddisablelighting = 1
	pLightMultiballReadyoff.blenddisablelighting = 1
	pLightZoomies1off.blenddisablelighting = 1
	pLightZoomies2off.blenddisablelighting = 1
	pLightZoomies3off.blenddisablelighting = 1
	pLightVari1off.blenddisablelighting = 1
	pLightVari2off.blenddisablelighting = 1
	pLightVari3off.blenddisablelighting = 1
	pLightKoff.blenddisablelighting = 1
	pLightIoff.blenddisablelighting = 1
	pLightN1off.blenddisablelighting = 1
	pLightGoff.blenddisablelighting = 1
	pLightAngrySheep1off.blenddisablelighting = 1
	pLightAngrySheep2off.blenddisablelighting = 1
	pLightAngrySheep3off.blenddisablelighting = 1
	pLightAngrySheep4off.blenddisablelighting = 1
	pLightAngrySheep5off.blenddisablelighting = 1
	pLightAngrySheep6off.blenddisablelighting = 1
	pLightAngrySheep7off.blenddisablelighting = 1
	pLightAngrySheep8off.blenddisablelighting = 1
	pLightNoff.blenddisablelighting = 1
	pLightUoff.blenddisablelighting = 1
	pLightToff.blenddisablelighting = 1
	pLightS1off.blenddisablelighting = 1
	pLightAwardoff.blenddisablelighting = 1
	pLightCatsLooseReadyaoff.blenddisablelighting = 1
	pLightSuperSheepReadyoff.blenddisablelighting = 1
	pLightTarget25off.blenddisablelighting = 1
	pLightTarget26off.blenddisablelighting = 1
	pLightLock2Readyoff.blenddisablelighting = 1
	pLightLock1Readyoff.blenddisablelighting = 1
	'
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'End of scrypt
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 

'//////////////////////////////////////////////////////////////////////
'// TIMERS
'//////////////////////////////////////////////////////////////////////


' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate

End Sub

Sub PrimBlendTimer_timer
'LightsOn

'	dim L,X
'	L = gi_laneguide1.GetInPlayIntensity
'	For each x in Pegs
'		x.blenddisablelighting = 0.067 * L
'	Next

	pSheepBoard1On.blenddisablelighting = 1.7 * LightSB1.GetInPlayIntensity 
	pSheepBoard2On.blenddisablelighting = 1.7 * LightSB2.GetInPlayIntensity 
	pSheepBoard3On.blenddisablelighting = 1.7 * LightSB3.GetInPlayIntensity 
	pSheepBoard4On.blenddisablelighting = 1.7 * LightSB4.GetInPlayIntensity 
	pSheepBoard5On.blenddisablelighting = 1.7 * LightSB5.GetInPlayIntensity 
	pSheepBoard6On.blenddisablelighting = 1.7 * LightSB6.GetInPlayIntensity 
	pSheepBoard7On.blenddisablelighting = 1.7 * LightSB7.GetInPlayIntensity 
	pSheepBoard8On.blenddisablelighting = 1.7 * LightSB8.GetInPlayIntensity 
	pCatapault1On.blenddisablelighting = 1.7 * LightCatBoard1.GetInPlayIntensity 
	pCatapault2On.blenddisablelighting = 1.7 * LightCatBoard2.GetInPlayIntensity 
	pLight_BallSaver.blenddisablelighting = Light_BallSaver.GetInPlayIntensity / 10
	pSquirrel1on.blenddisablelighting = 1.7 * LightSquirrel1.GetInPlayIntensity 
	pSquirrel2on.blenddisablelighting = 1.7 * LightSquirrel2.GetInPlayIntensity 
	pSquirrel3on.blenddisablelighting = 1.7 * LightSquirrel3.GetInPlayIntensity 
	pSquirrel4on.blenddisablelighting = 1.7 * LightSquirrel4.GetInPlayIntensity 
	pSquirrel5on.blenddisablelighting = 1.7 * LightSquirrel5.GetInPlayIntensity 
	pSquirrel6on.blenddisablelighting = 1.7 * LightSquirrel6.GetInPlayIntensity 
	pFenceon.blenddisablelighting = 1.7 * LightFence.GetInPlayIntensity 
	pLightDigKickerReadyon.blenddisablelighting = 1.7 * LightDigKickerReady.GetInPlayIntensity 
	pKingMultibalReadyon.blenddisablelighting = 1.7 * LightKingMultiballReady.GetInPlayIntensity /5
	pFiFiMultibalReadyon.blenddisablelighting = 1.7 * LightFiFiMultiballReady.GetInPlayIntensity /5
	pCatMultibalReadyon.blenddisablelighting = 1.7 * LightCatMultiballReady.GetInPlayIntensity /5
	pSheepMultibalReadyon.blenddisablelighting = 1.7 * LightSheepMultiballReady.GetInPlayIntensity /5
	pBonus2on.blenddisablelighting = 1.7 * LightBonus2X.GetInPlayIntensity/5
	pBonus3on.blenddisablelighting = 1.7 * LightBonus3X.GetInPlayIntensity /5
	pBonus5on.blenddisablelighting = 1.7 * LightBonus5X.GetInPlayIntensity/5
	pBonus10on.blenddisablelighting = 1.7 * LightBonus10X.GetInPlayIntensity /5
	pBonus20on.blenddisablelighting = 1.7 * LightBonus20X.GetInPlayIntensity /5
	pLightShootAgainon.blenddisablelighting = 1.7 * LightShootAgain.GetInPlayIntensity /5
	pLightDig1on.blenddisablelighting = 1.7 * LightDig1.GetInPlayIntensity /5
	pLightDig2on.blenddisablelighting = 1.7 * LightDig2.GetInPlayIntensity /5
	pLightDig3on.blenddisablelighting = 1.7 * LightDig3.GetInPlayIntensity /5
	pLightDig4on.blenddisablelighting = 1.7 * LightDig4.GetInPlayIntensity /5
	pLightDig5on.blenddisablelighting = 1.7 * LightDig5.GetInPlayIntensity /5
	pLightTree1on.blenddisablelighting = LightTree1.GetInPlayIntensity /5
	pLightTree2on.blenddisablelighting =  LightTree2.GetInPlayIntensity /5
	pLightTree3on.blenddisablelighting =  LightTree3.GetInPlayIntensity /5
	pLightRightILon.blenddisablelighting = 1.7 * LightRightIL.GetInPlayIntensity /5
	pLightRightOLon.blenddisablelighting = 1.7 * LightRightOL.GetInPlayIntensity /5
	pLightLeftILon.blenddisablelighting = 1.7 * LightLeftIL.GetInPlayIntensity /5
	pLightLeftOLon.blenddisablelighting = 1.7 * LightLeftOL.GetInPlayIntensity /5
	pLightKingDogReadyon.blenddisablelighting = 1.7 * LightKingDogReady.GetInPlayIntensity 
	pLightExtraBallon.blenddisablelighting = 1.7 * LightExtraBall.GetInPlayIntensity 
	pLightFastScoringActivateon.blenddisablelighting = 1.7 * LightMystery.GetInPlayIntensity 
	pLightLocksActivateon.blenddisablelighting = 1.7 * LightLocksActivate.GetInPlayIntensity 
	pLightMultiballReadyon.blenddisablelighting = 1.7 * LightMultiballReady.GetInPlayIntensity 
	pLightLocksReadyon.blenddisablelighting = 1.7 *LightLocksReady.GetInPlayIntensity 
	pLightZoomies1on.blenddisablelighting = 1.7 * LightZoomies1.GetInPlayIntensity 
	pLightZoomies2on.blenddisablelighting = 1.7 * LightZoomies2.GetInPlayIntensity/3
	pLightZoomies3on.blenddisablelighting = 1.7 * LightZoomies3.GetInPlayIntensity 
	pLightVari1on.blenddisablelighting = 20 * LightVari1.GetInPlayIntensity 
	pLightVari2on.blenddisablelighting = 1.7 * LightVari2.GetInPlayIntensity/3
	pLightVari3on.blenddisablelighting =10 * LightVari3.GetInPlayIntensity 
	pLightKon.blenddisablelighting =  LightK.GetInPlayIntensity/8
	pLightIon.blenddisablelighting =  LightI.GetInPlayIntensity/3
	pLightN1on.blenddisablelighting =  LightN1.GetInPlayIntensity/3
	pLightGon.blenddisablelighting =  LightG.GetInPlayIntensity/8

	pLightAngrySheep1on.blenddisablelighting =  LightAngrySheep1.GetInPlayIntensity/3
	pLightAngrySheep2on.blenddisablelighting =  LightAngrySheep2.GetInPlayIntensity/3
	pLightAngrySheep3on.blenddisablelighting =  LightAngrySheep3.GetInPlayIntensity/3
	pLightAngrySheep4on.blenddisablelighting =  LightAngrySheep4.GetInPlayIntensity/3
	pLightAngrySheep5on.blenddisablelighting =  LightAngrySheep5.GetInPlayIntensity/3
	pLightAngrySheep6on.blenddisablelighting =  LightAngrySheep6.GetInPlayIntensity/3
	pLightAngrySheep7on.blenddisablelighting =  LightAngrySheep7.GetInPlayIntensity/3
	pLightAngrySheep8on.blenddisablelighting =  LightAngrySheep8.GetInPlayIntensity/3
	pLightNon.blenddisablelighting = 1.7 * LightN.GetInPlayIntensity /5
	pLightUon.blenddisablelighting = 1.7 * LightU.GetInPlayIntensity /5
	pLightTon.blenddisablelighting = 1.7 * LightT.GetInPlayIntensity /5
	pLightS1on.blenddisablelighting = 1.7 * LightS1.GetInPlayIntensity /5
	pLightAwardon.blenddisablelighting =  LightAward.GetInPlayIntensity/3
	pLightCatsLooseReadyaon.blenddisablelighting =  LightCatsLooseReadya.GetInPlayIntensity/3
	pLightSuperSheepReadyon.blenddisablelighting =  LightSuperSheepReady.GetInPlayIntensity/3
	pLightTarget26on.blenddisablelighting = 1.7 * LightTarget26.GetInPlayIntensity 
	pLightTarget25on.blenddisablelighting = 1.7 * LightTarget25.GetInPlayIntensity 
	pLightLock1Readyon.blenddisablelighting = 1.7 * LightLock1Ready.GetInPlayIntensity 
	pLightLock2Readyon.blenddisablelighting = 1.7 * LightLock2Ready.GetInPlayIntensity 
End Sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

Dim Showpost 'used by F12 to show/hide post between flippers
Dim SongChoice:SongChoice=True
Dim dspTriggered : dspTriggered = False
Dim ShowMyDog: ShowMyDog = True
Dim LUToption
Sub Table1_OptionEvent(ByVal eventId)
    ' Only run when options are applied/changed
    If eventId = 0 Or eventId = 1 Then

	LUToption = Table1.Option("LUT", 0, 20, 1, 0, 0, Array("LUT0","LUT1","LUT2","LUT3","LUT4","LUT5","LUT6","LUT7","LUT8","LUT9","LUT10","LUT Warm 0","LUT Warm 1","LUT Warm 2","LUT Warm 3","LUT Warm 4","LUT Warm 5","LUT Warm 6","LUT Warm 7","LUT Warm 8","LUT Warm 9"))
	If LUToption <= 10 Then
		Table1.ColorGradeImage = "LUT" & LUToption
	Else
		Table1.ColorGradeImage = "LUT_Warm_" & (LUToption - 11)
	End If
		SongChoice = Table1.Option("MrBlueSky-SongSet", 0, 1, 1, 1, 0, Array("Bluey_SongSet", "Default"))
		SetSongs SongChoice
		ShowMyDog = Table1.Option("DontShowMyDog", 0, 1, 1, 1, 0, Array("ShowMyDog", "Default"))
		SetShowMyDog ShowMyDog	
If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
		ShowPost = Table1.Option("Flipper Post", 0, 1, 1, 1, 1, Array("Off", "On"))
			CheckCheaters
        ' Sound volumes
		SongVolume      = Table1.Option("Song Volume", 0, 1, 0.01, 0.4, 1)
        VolumeDial      = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
        BallRollVolume  = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.8, 1)
        RampRollVolume  = Table1.Option("Ramp Roll Volume", 0, 1, 0.01, 0.15, 1)
    End If
End Sub

Sub SetShowMyDog(Opt)
	Select Case Opt
		Case 0:
				apron3.visible=True:Apron2.Visible=False
				plunger2.visible=True:plunger1.visible=False
		Case 1:
				Apron2.visible=True:Apron3.Visible=False
				plunger2.visible=False:plunger1.visible=True
	End Select
End Sub


Sub CheckCheaters()
	If Showpost = 1 then
		zCol_Rubber_Peg11.collidable=True
		Rubber007.visible=True
		Primitive034.visible=True
	Else
		zCol_Rubber_Peg11.collidable=False
		Rubber007.visible=False
		Primitive034.visible=False
	End If
End Sub
Sub SetSongs(Opt)
	Select Case Opt
		Case 0:
				ChooseSongSet2=True:ChooseSongSet1=False
		Case 1:
				ChooseSongSet1=True:ChooseSongSet2=False
	End Select
End Sub

Sub SetRails(Opt)
	Select Case Opt
		Case 0:
			lrail.Visible = 0
			rrail.Visible = 0
			PinCab_Blades.visible = 1
			Flasher002.visible = 0
			Flasher001.visible = 0
		Case 1:
			lrail.Visible = 1
			rrail.Visible = 1
			PinCab_Blades.visible = 1
			Flasher002.visible = 1
			Flasher001.visible = 1
	End Select
End Sub


'
''**********************************
'' 	F12 Menu For Cabinet
''**********************************
'Dim RailChoice: RailChoice = True
''//////////////F12 Menu//////////////
'' Called when options are tweaked by the player. 
'' - 0: game has started, good time to load options and adjust accordingly
'' - 1: an option has changed
'' - 2: options have been reseted
'' - 3: player closed the tweak UI, good time to update staticly prerendered parts
'' Table1.Option arguments are: 
'' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
'Dim dspTriggered : dspTriggered = False
'Sub Table1_OptionEvent(ByVal eventId)
'	If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If
'
'    	RailChoice = Table1.Option("Rails Visible", 0, 1, 1, 1, 0, Array("Cabinet", "SideRails (Default)"))
'	SetRails RailChoice
'If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
'	End Sub
'
'	
'Sub SetRails(Opt)
'	Select Case Opt
'		Case 0:
'			Ramp15.Visible = 0
'			Ramp16.Visible = 0
'			PinCab_Blades.visible = 1
'		Case 1:
'			Ramp15.Visible = 1
'			Ramp16.Visible = 1
'			PinCab_Blades.visible = 0
'	End Select
'End Sub


'//////////////////////////////////////////////////////////////////////
'// Ball
'//////////////////////////////////////////////////////////////////////

'If BallBright Then
'	table1.BallImage = "ball_HDR_brighter"
'Else
'	table1.BallImage = "MRBallDark2b"
'End If

'//////////////////////////////////////////////////////////////////////
'// Dynamic Ball Shadows
'//////////////////////////////////////////////////////////////////////
' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'Ambient (Room light source)
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 0		'Offset y position under ball	 (for example 5,5 if the light is in the back left corner)
'Dynamic (Table light sources)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

' ***														***

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(9), objrtx2(9)
dim objBallShadow(9)
Dim OnPF(9)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadow5,BallShadow6,BallShadow7,BallShadow8,BallShadow9)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

Dim ClearSurface:ClearSurface = True		'Variable for hiding flasher shadow on wire and clear plastic ramps
									'Intention is to set this either globally or in a similar manner to RampRolling sounds

'Initialization
DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob - 1								'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.01			'Separate z for layering without clipping
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii/1000 + 0.02
		objrtx2(iii).visible = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
'		If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
end sub


Sub BallOnPlayfieldNow(yeh, num)		'Only update certain things once, save some cycles
	If yeh Then
		OnPF(num) = True
'		debug.print "Back on PF"
		UpdateMaterial objBallShadow(num).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(num).size_x = 5
		objBallShadow(num).size_y = 4.5
		objBallShadow(num).visible = 1
		BallShadowA(num).visible = 0
	Else
		OnPF(num) = False
'		debug.print "Leaving PF"
		If Not ClearSurface Then
			BallShadowA(num).visible = 1
			objBallShadow(num).visible = 0
		Else
			objBallShadow(num).visible = 1
		End If
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed dynamically if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim gBOT: gBOT=getballs	'Uncomment if you're deleting balls - Don't do it! #SaveTheBalls

	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 to tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(gBOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update

				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					If gBOT(s).X < tablewidth/2 Then
						objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
					Else
						objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
					End If
					objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z+BallSize)/80)			'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z+BallSize)/80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(30/(gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				End If

			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				If Not OnPF(s) Then BallOnPlayfieldNow True, s

				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + offsetY
'				objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf

			Else												'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + offsetY
				End If
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=0.1
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff:
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff Then
'					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y
					'objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + offsetY
					'objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'//////////////////////////////////////////////////////////////////////
'// TargetBounce
'//////////////////////////////////////////////////////////////////////

sub TargetBouncer(aBall,defvalue)
	dim zMultiplier, vel, vratio
	if TargetBouncerEnabled = 1 and aball.z < 30 then
		'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
	if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
			Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
			Case 5: zMultiplier = 0.45*defvalue
			Case 6: zMultiplier = 0.5*defvalue
		End Select
		aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
		aBall.vely = aBall.velx * vratio
		'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

' Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub

'//////////////////////////////////////////////////////////////////////
'// PHYSICS DAMPENERS
'//////////////////////////////////////////////////////////////////////

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


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handle here
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class

'//////////////////////////////////////////////////////////////////////
'// TRACK ALL BALL VELOCITIES FOR RUBBER DAMPENER AND DROP TARGETS
'//////////////////////////////////////////////////////////////////////

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

'//////////////////////////////////////////////////////////////////////
'// RAMP ROLLING SFX
'//////////////////////////////////////////////////////////////////////

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(6)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub


Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
	BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function

'//////////////////////////////////////////////////////////////////////
'// RAMP TRIGGERS
'//////////////////////////////////////////////////////////////////////

Sub ramptrigger01_hit() 'Turn on ramp roll after going above ramp
	WireRampOn True 'Play Plastic Ramp Sound
	debug.print "Start Ramp Roll on at Centre Ramp"
End Sub

Sub ramptrigger02on_hit() 'Turn on ramp roll after released at walk the plank
	WireRampOn True 'Play Plastic Ramp Sound
	debug.print "Ramp Roll on at Start of Invisible WTP Ramp"
End Sub

Sub ramptrigger03on_hit() 'Turn on ramp roll after going above ramp
	WireRampOn True 'Play Plastic Ramp Sound
	debug.print "Ramp Rollon at Start Upkicker rampramp"
End Sub 

Sub ramptrigger04on_hit() 'Turn on ramp roll for load the canon ramp
	WireRampOn True 'Play Plastic Ramp Sound
	debug.print "Ramp Roll on at Start of Arm the Canon ramp"
End Sub

Sub ramptrigger01off_hit()'turn off ramp roll at end of ramp at right lane
	WireRampOff 'Play Plastic Ramp Sound
	debug.print "Ramp Roll Off at End Of centre ramp at the RightLane"
End Sub


Sub ramptrigger01aoff_hit()'turn off ramp roll at walk the plank upkicker
	WireRampOff  'Play Plastic Ramp Sound
	debug.print "Ramp Roll Off PlankUpkicker"
End Sub

Sub ramptrigger01boff_hit()'turn off ramp roll at daveyJones diverter
	WireRampOff  'Play Plastic Ramp Sound
	debug.print "Ramp Roll Off at davey Jones"
End Sub


Sub ramptrigger02off_hit()'turn off ramp roll at end of invisible plank ramp
	WireRampOff  'Play Plastic Ramp Sound
	debug.print "Ramp Roll Off at End of Invisible Ramp"
End Sub

Sub ramptrigger04off_hit()'turn off ramp roll at end of canon load ramp
	WireRampOff  'Play Plastic Ramp Sound
	debug.print "Ramp Roll Off at End of Arm Canon ramp"
End Sub


Sub Wall017_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub


Sub Wall017_unhit()
	PlaySoundAt "WireRamp_Stop", ramptrigger03
End Sub

'//////////////////////////////////////////////////////////////////////
'// Ball Rolling
'//////////////////////////////////////////////////////////////////////

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)


Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Dim BallLights
BallLights = array(BallLight001,BallLight002,BallLight003,BallLight004,BallLight005,BallLight006,BallLight007,BallLight008,BallLight009,BallLight010,BallLight011,BallLight012,BallLight013,BallLight014,BallLight015,BallLight016)
Dim BallColors
BallColors = array( RGB(255,255,0),RGB(0,255,255),RGB(0,255,255),RGB(0,255,0),RGB(255,0,0),RGB(0,255,0),RGB(0,255,255),RGB(255,0,255) )
Dim ColorBalls
Dim B_Color
Dim B_image

Sub StartColouredBalls
	ColorBalls = True
	B_Color = rnd(Int(1)*8)
	B_image = "ballgreydragon2"
End Sub

Sub StopColouredBalls
	ColorBalls = False
	B_Color = RGB(255,255,255)
	B_image = "ballgreydragon2"
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory,i
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        ' Comment the next line if you are not implementing Dyanmic Ball Shadows
        If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
        rolling(b) = False
        StopSound("BallRoll_" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub


'*****DogsHead follow the ball*****

	For each i in DogHead
          i.RotZ = - BOT(0).Y\12 -10
    Next

	SheepTim1.RotZ=- BOT(0).Y\12 -10
	SheepTim2.RotZ=- BOT(0).Y\12 -10
	SheepTim3.RotZ=- BOT(0).Y\12 -10
    ' play the rolling sound for each ball

    For b = 0 to UBound(BOT)

    If BallVel(BOT(b)) > 1 And BOT(b).Z < 30 Then
        rolling(b) = True
        PlaySound "BallRoll_" & b, -1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial * 0.7, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
    Else
        If rolling(b) = True Then
            StopSound "BallRoll_" & b
            rolling(b) = False
        End If
    End If

		' Ball Drop Sounds
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If


 '       if b > 1 Then
'            If ColorBalls = True Then
 '               BOT(b).image = B_image
 '               If Bot(b).ID < 22222 Then BOT(b).id = BOT(b).id + 22222 : BOT(b).color = BallColors(B_Color) : B_Color = B_Color + 1 : If B_Color > 7 Then B_Color = 0
 '           Else
 '               BOT(b).color = RGB(255,255,255)
 '               BOT(b).image = "ball_HDR_brighter"
 '               If BOT(b).id > 22222 Then BOT(b).id = BOT(b).id - 22222
 '           End If
'		If BOT(b).z > 50 and BallLightActive=True Then 'TurnOnWhiteLight if height is>50
 '           BallLights(b).x = bot(b).x
  '          BallLights(b).y = bot(b).y 
  '          BallLights(b+8).x = bot(b).x 
 '           BallLights(b+8).y = bot(b).y
 '       End If

	Next
End Sub





'//////////////////////////////////////////////////////////////////////
'// Mechanic Sounds
'//////////////////////////////////////////////////////////////////////

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
' 	Metals (all metal objects, metal walls, metal posts, metal wire guides)
' 	Apron (the apron walls and plunger wall)
' 	Walls (all wood or plastic walls)
' 	Rollovers (wire rollover triggers, star triggers, or button triggers)
' 	Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
' 	Gates (plate gates)
' 	GatesWire (wire gates)
' 	Rubbers (all rubbers including posts, sleeves, pegs, and bands)
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
' Part 1: 	https://youtu.be/PbE2kNiam3g
' Part 2: 	https://youtu.be/B5cm1Y8wQsk
' Part 3: 	https://youtu.be/eLhWyuYOyGg


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 0.5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.5           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel =1								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 1.5											'volume multiplier; must not be zero
RubberWeakSoundFactor =1.2											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.8										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.3											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


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
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
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

Sub PlaySoundAtLevelTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

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

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

	If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
	Else
		AudioFade = Csng(-((- tmp) ^10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

	If tmp > 0 Then
		AudioPan = Csng(tmp ^10)
	Else
		AudioPan = Csng(-((- tmp) ^10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
	RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

'Function RndNum(min, max)
'	RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
'End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
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
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
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
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then		
		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If	
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	RandomSoundWall()      
End Sub

Sub RandomSoundWall()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
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
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()		
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 then
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
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()			
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
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
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -8 Then
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
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd*6)+1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315									'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05									'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025*RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025*RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025*RelayFlashSoundLevel, obj			
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025*RelayFlashSoundLevel, obj		
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************

dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection
dim RS3 : Set RS3 = New SlingshotCorrection
dim LS4 : Set LS4 = New SlingshotCorrection
InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

	RS3.Object = RightSlingShot3
	RS3.EndPoint1 = EndPoint3RS
	RS3.EndPoint2 = EndPoint3aRS

	LS4.Object = LeftSlingShot4
	LS4.EndPoint1 = EndPoint4LS
	LS4.EndPoint2 = EndPoint4aLS

	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00,	-4
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
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

Function RotPoint(x,y,angle)
	dim rx, ry
	rx = x*dCos(angle) - y*dSin(angle)
	ry = x*dSin(angle) + y*dCos(angle)
	RotPoint = Array(rx,ry)
End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2

	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): Enabled = True : End Sub 

	Public Property let Object(aInput) : Set Slingshot = aInput : End Property
	Public Property Let EndPoint1(aInput) : SlingX1 = aInput.x: SlingY1 = aInput.y: End Property
	Public Property Let EndPoint2(aInput) : SlingX2 = aInput.x: SlingY2 = aInput.y: End Property

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 then Report
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		If not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


	Public Sub VelocityCorrect(aBall)
		dim BallPos, XL, XR, YL, YR

		'Assign right and left end points
		If SlingX1 < SlingX2 Then 
			XL = SlingX1 : YL = SlingY1 : XR = SlingX2 : YR = SlingY2
		Else
			XL = SlingX2 : YL = SlingY2 : XR = SlingX1 : YR = SlingY1
		End If

		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then 
			If ABS(XR-XL) > ABS(YR-YL) Then 
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If

		'Velocity angle correction
		If not IsEmpty(ModIn(0) ) then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
			'debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'debug.print " " 
		End If
	End Sub

End Class

Dim PopValue0 '100
Dim PopValue1 '1000
Dim PopValue2 '10000
Dim PopValue3 '500000



Dim BG_array(50,2)
' 1 = state   (0,1,2) pff on blinking   2 = current
Sub bgTimer_Timer
'Bonometer
	For x = 1 to 27
		If LightDog1.getinplaystatebool = true Then BG_array(1,1) = 1 Else BG_array(1,1) = 0
		If LightDog2.getinplaystatebool = true Then BG_array(2,1) = 1 Else BG_array(2,1) = 0
		If LightDog3.getinplaystatebool = true Then BG_array(3,1) = 1 Else BG_array(3,1) = 0
		If LightDog4.getinplaystatebool = true Then BG_array(4,1) = 1 Else BG_array(4,1) = 0
		If LightDog5.getinplaystatebool = true Then BG_array(5,1) = 1 Else BG_array(5,1) = 0
		If LightDog6.getinplaystatebool = true Then BG_array(6,1) = 1 Else BG_array(6,1) = 0
		If LightDog7.getinplaystatebool = true Then BG_array(7,1) = 1 Else BG_array(7,1) = 0
		If LightDog8.getinplaystatebool = true Then BG_array(8,1) = 1 Else BG_array(8,1) = 0
		If LightDog9.getinplaystatebool = true Then BG_array(9,1) = 1 Else BG_array(9,1) = 0
		If LightDog10.getinplaystatebool = true Then BG_array(10,1) = 1 Else BG_array(10,1) = 0
		If LightBone.getinplaystatebool = true Then BG_array(11,1) = 1 Else BG_array(11,1) = 0
		If LightTitle.getinplaystatebool = true Then BG_array(12,1) = 1 Else BG_array(12,1) = 0
		If LightBall.getinplaystatebool = true Then BG_array(13,1) = 1 Else BG_array(13,1) = 0
		If LightLeft.getinplaystatebool = true Then BG_array(14,1) = 1 Else BG_array(14,1) = 0
		If LightMiddle.getinplaystatebool = true Then BG_array(15,1) = 1 Else BG_array(15,1) = 0
		If LightRight.getinplaystatebool = true Then BG_array(16,1) = 1 Else BG_array(16,1) = 0
		If LightCat17.getinplaystatebool = true Then BG_array(17,1) = 1 Else BG_array(17,1) = 0
		If LightFloor.getinplaystatebool = true Then BG_array(18,1) = 1 Else BG_array(18,1) = 0
		If LightCardTable.getinplaystatebool = true Then BG_array(19,1) = 1 Else BG_array(19,1) = 0
		If LightBono1.getinplaystatebool = true Then BG_array(20,1) = 1 Else BG_array(20,1) = 0
		If LightBono2.getinplaystatebool = true Then BG_array(21,1) = 1 Else BG_array(21,1) = 0
		If LightBono3.getinplaystatebool = true Then BG_array(22,1) = 1 Else BG_array(22,1) = 0
		If LightBono4.getinplaystatebool = true Then BG_array(23,1) = 1 Else BG_array(23,1) = 0
		If LightBono5.getinplaystatebool = true Then BG_array(24,1) = 1 Else BG_array(24,1) = 0
		If LightBono6.getinplaystatebool = true Then BG_array(25,1) = 1 Else BG_array(25,1) = 0
		If LightBono7.getinplaystatebool = true Then BG_array(26,1) = 1 Else BG_array(26,1) = 0
		If LightBono8.getinplaystatebool = true Then BG_array(27,1) = 1 Else BG_array(27,1) = 0

		If BG_array(x,1) <> BG_array(x,2) Then
			BG_array(x,2) = BG_array(x,1)
			If BG_array(x,2) = 0 Then Controller.B2SSetData x,0 Else Controller.B2SSetData x,1
		End If
	Next
End Sub

Dim Controller
Dim B2SOn

Sub startcontroller
	Set Controller = CreateObject("B2S.Server") 
	Controller.B2SName = "GameOfDOG"
	Controller.Run
	B2SOn = True
	bgTimer.enabled = True

End Sub

Dim AttractCount
Sub AttractBackGlassLightTimer_Timer
		AttractCount=AttractCount+1
	Select Case AttractCount
		Case 1:  AllBackGlassLightsBlinking
		Case 10: AllBackGlassLightsOn
		Case 38: AllBackGlassLightsOff
		Case 40: LightTitle.State=2:LightBone.State=2:LightBall.State=2
		Case 70: LightTitle.State=1:LightBone.State=1:LightBall.State=1
		Case 85: LightCardTable.State=1
		Case 100: LightLeft.State=1
		Case 115: LightMiddle.State=1
		Case 130: LightRight.State=1
		Case 140: LightFloor.State=1
		Case 160: AllDogLightsBlinking
		Case 168 AllDogLightsOn
		Case 205: AllDogLightsOff
		Case 210: LightDog1.State=1:LightBono1.State=1
		Case 220  LightDog2.State=1:LightBono2.State=1
		Case 230: LightDog3.State=1:LightBono3.State=1
		Case 240: LightDog4.State=1:LightBono4.State=1
		Case 250: LightDog5.State=1:LightBono5.State=1:LightCat17.State=2
		Case 260: LightDog6.State=1:LightBono6.State=1
		Case 270: LightDog10.State=1:LightBono7.State=1
		Case 280: LightDog7.State=2:LightBono8.State=1
		Case 290: LightDog8.State=1
		Case 300: LightDog9.State=1:LightDog7.State=1
		Case 315  LightCat17.State=1
		Case 400: LightRight.State=0
		Case 420: LightMiddle.State=0:LightCat17.State=0
		Case 440: LightLeft.State=0:LightFloor.State=0
		Case 460: AllDogLightsOff
		Case 500: AllBackGlassLightsOff
		Case 530: AttractCount=0
	End Select
End Sub 

Dim EOB
EOB=0
Sub BackGlassEndOfBallTimer_Timer
		EOB=EOB+1
	Select Case EOB
		Case 1:  AllBackGlassLightsOff
		Case 5: TitleBlinking
		Case 15: LightTitle.State=1:LightBone.State=1:LightBall.State=1
		Case 20: LightCardTable.State=1
		Case 25: LightLeft.State=1
		Case 30: LightMiddle.State=1
		Case 35: LightRight.State=1
		Case 40: LightFloor.State=1
		Case 45: LightDog1.State=1
		Case 50:  LightDog2.State=1
		Case 55: LightDog3.State=1
		Case 60: LightDog4.State=1
		Case 65: LightDog5.State=1:LightCat17.State=2
		Case 70: LightDog6.State=1
		Case 75: LightDog10.State=1
		Case 80: LightDog7.State=2
		Case 85: LightDog8.State=1
		Case 90: LightDog9.State=1:LightDog7.State=1
		Case 95  LightCat17.State=2
		Case 100: AllDogLightsOff
		Case 105: AllBackGlassLightsOn
		Case 110  LightCat17.State=0	
		Case 110:EOB=0: BackGlassEndOfBallTimer.Enabled=0
	End Select


End Sub

Dim CountBG1:CountBG1=0
Sub BackGlassDogsRotateTimer_Timer
		CountBG1=CountBG1+1
	Select Case CountBG1
		Case 1:   AllBackGlassLightsOff
		Case 10: AllDogLightsOn
		Case 15: AllDogLightsOff
		Case 20: LightCardTable.State=1:LightTitle.State=1:LightBone.State=1:LightBall.State=1
		Case 23: LightDog1.State=1
		Case 26  LightDog2.State=1
		Case 29: LightDog3.State=1
		Case 32: LightDog4.State=1
		Case 35: LightDog5.State=1
		Case 38: LightDog6.State=1
		Case 41: LightDog10.State=1
		Case 44: LightDog7.State=1
		Case 47: LightDog8.State=1
		Case 50: LightDog9.State=1
		Case 53: LightLeft.State=1
		Case 56: LightMiddle.State=1
		Case 59: LightRight.State=1
		Case 62: LightRight.State=1
		Case 65: LightFloor.State=1
		Case 68: LightCardTable.State=1
		Case 71	: TitleBlinking	
		Case 74: AllDogLightsBlinking
		Case 77: AllBackGlassLightsOn
		Case 80: LightCat17.State=0:CountBG1=0:BackGlassDogsRotateTimer.Enabled=0
		
	End Select
End Sub

Dim DogsBlink
DogsBlink=0
Sub BackGlassDogsBlinkTimer_Timer
DogsBlink=DogsBlink+1
	Select Case DogsBlink
		Case 1: AllBackGlassLightsOff
		Case 2: LightCat17.State=1
		Case 4:AllDogLightsBlinking
		Case 40:AllBackGlassLightsOn
		Case 41:DogsBlink=0:BackGlassDogsBlinkTimer.Enabled=0
	End Select
End Sub

Dim DogsSpin
DogsSpin=0
Sub BackglassDogsSpinTimer_Timer
	DogsSpin=DogsSpin+1
	Select Case DogsSpin
		Case 1: AllBackGlassLightsOff
		Case 5 AllDogLightsOn
		Case 8: AllDogLightsOff
		Case 13: LightDog1.State=1
		Case 15  LightDog2.State=1
		Case 17: LightDog3.State=1
		Case 19: LightDog4.State=1
		Case 21: LightDog5.State=1
		Case 23: LightDog6.State=1
		Case 25: LightDog10.State=1
		Case 27: LightDog8.State=1
		Case 29: LightDog1.State=0
		Case 42  LightDog2.State=0
		Case 45: LightDog3.State=0
		Case 48: LightDog4.State=0
		Case 51: LightDog5.State=0
		Case 54: LightDog6.State=0
		Case 57: LightDog8.State=0
		Case 60: AllBackGlassLightsOff
		Case 70: AllDogsBlink:TitleBlinking
		Case 80: TitleOn
		Case 90: AllBackGlassLightsOn:DogsSpin=0: BackglassDogsSpinTimer.Enabled=0
		
	End Select

End Sub

Dim Mini
Mini=0
Sub BackGlassMiniBlinkTimer_Timer
	Mini=Mini+1
	Select Case Mini
		Case 1: AllBackGlassLightsOff
		Case 5 AllDogLightsBlinking
		Case 28: AllDogLightsOff
		Case 30: AllDogLightsOn:Mini=0: BackGlassMiniBlinkTimer.Enabled=0
		
End Select

End Sub

Sub AllDogsOn
	LightDog1.State=1:LightDog2.State=1:LightDog3.State=1:LightDog4.State=1:LightDog5.State=1
	LightDog6.State=1:LightDog7.State=1:LightDog8.State=1:LightDog9.State=1:LightDog10.State=1
End Sub

Sub AllDogsOff
	LightDog1.State=0:LightDog2.State=0:LightDog3.State=0:LightDog4.State=0:LightDog5.State=0
	LightDog6.State=0:LightDog7.State=0:LightDog8.State=0:LightDog9.State=0:LightDog10.State=0
End Sub

Sub AllDogsBlink
	LightDog1.State=2:LightDog2.State=2:LightDog3.State=2:LightDog4.State=2:LightDog5.State=2
	LightDog6.State=2:LightDog7.State=2:LightDog8.State=2:LightDog9.State=2:LightDog10.State=2
End Sub


Sub AllBackGlassLightsBlinking
	LightDog1.State=2:LightDog2.State=2:LightDog3.State=2:LightDog4.State=2:LightDog5.State=2
	LightDog6.State=2:LightDog7.State=2:LightDog8.State=2:LightDog9.State=2:LightDog10.State=2
	LightBone.State=2:LightTitle.State=2:LightBall.State=2
	LightCat17.State=2
	LightLeft.State=2:LightRight.State=2:LightMiddle.State=2:LightFloor.State=2:LightCardTable.State=2
	LightBono1.State=2:LightBono2.State=2:LightBono3.State=2:LightBono4.State=2:LightBono5.State=2
	LightBono6.State=2:LightBono7.State=2:LightBono8.State=2
End Sub

Sub AllBackGlassLightsOn
	LightDog1.State=1:LightDog2.State=1:LightDog3.State=1:LightDog4.State=1:LightDog5.State=1
	LightDog6.State=1:LightDog7.State=1:LightDog8.State=1:LightDog9.State=1:LightDog10.State=1
	LightBone.State=1:LightTitle.State=1:LightBall.State=1
'	LightCat17.State=1
	LightLeft.State=1:LightRight.State=1:LightMiddle.State=1:LightFloor.State=1:LightCardTable.State=1
	LightBono1.State=0:LightBono2.State=0:LightBono3.State=0:LightBono4.State=0:LightBono5.State=0
	LightBono6.State=0:LightBono7.State=0:LightBono8.State=0

End Sub

Sub AllBackGlassLightsOff
	LightDog1.State=0:LightDog2.State=0:LightDog3.State=0:LightDog4.State=0:LightDog5.State=0
	LightDog6.State=0:LightDog7.State=0:LightDog8.State=0:LightDog9.State=0:LightDog10.State=0
	LightBone.State=0:LightTitle.State=0:LightBall.State=0
	LightCat17.State=0
	LightLeft.State=0:LightRight.State=0:LightMiddle.State=0:LightFloor.State=0:LightCardTable.State=0
	LightBono1.State=0:LightBono2.State=0:LightBono3.State=0:LightBono4.State=0:LightBono5.State=0
	LightBono6.State=0:LightBono7.State=0:LightBono8.State=0

End Sub

Sub TitleOn
	LightBone.State=1:LightTitle.State=1:LightBone.State=1
End Sub

Sub TitleOff
	LightBone.State=0:LightTitle.State=0:LightBone.State=0
End Sub

Sub TitleBlinking
	LightBone.State=2:LightTitle.State=2:LightBone.State=2
End Sub

Sub AllRoomsOn
	LightLeft.State=1:LightRight.State=1:LightMiddle.State=1:LightFloor.State=1
End Sub
Sub LeftRoomOn
	LightLeft.State=1
End Sub

Sub MiddleRoomOn
	LightMiddle.State=1
End Sub

Sub RightRoomOn
	LightRight.State=1
End Sub

Sub FloorOn
	LightFloor.State=1
End Sub


Sub CardTableOn
	LightCardTable.State=1
End Sub





Sub AllDogLightsBlinking
	LightDog1.State=2:LightDog2.State=2:LightDog3.State=2:LightDog4.State=2:LightDog5.State=2
	LightDog6.State=2:LightDog7.State=2:LightDog8.State=2:LightDog9.State=2:LightDog10.State=2
End Sub

Sub AllDogLightsOn
	LightDog1.State=1:LightDog2.State=1:LightDog3.State=1:LightDog4.State=1:LightDog5.State=1
	LightDog6.State=1:LightDog7.State=1:LightDog8.State=1:LightDog9.State=1:LightDog10.State=1
End Sub

Sub AllDogLightsOff
	LightDog1.State=0:LightDog2.State=0:LightDog3.State=0:LightDog4.State=0:LightDog5.State=0
	LightDog6.State=0:LightDog7.State=0:LightDog8.State=0:LightDog9.State=0:LightDog10.State=0

End Sub


'******************************************************
'*****   FLUPPER DOMES 
'******************************************************
' Based on FlupperDoms2.2

' What you need in your table to use these flashers:
' Open this table and your table both in VPX
' Export all the materials domebasemat, Flashermaterial0 - 20 and import them in your table
' Export all textures (images) starting with the name "dome" and "ronddome" and import them into your table with the same names
' Export all textures (images) starting with the name "flasherbloom" and import them into your table with the same names
' Copy a set of 4 objects flasherbase, flasherlit, flasherlight and flasherflash from layer 7 to your table
' If you duplicate the four objects for a new flasher dome, be sure that they all end with the same number (in the 0-20 range)
' Copy the flasherbloom flashers from layer 10 to your table. you will need to make one per flasher dome that you plan to make
' Select the correct flasherbloom texture for each flasherbloom flasher, per flasher dome
' Copy the script below 

' Place your flasher base primitive where you want the flasher located on your Table
' Then run InitFlasher in the script with the number of your flasher objects and the color of the flasher.  This will align the flasher object, light object, and 
' flasher lit primitive.  It will also assign the appropriate flasher bloom images to the flasher bloom object.
'
' Example: InitFlasher 1, "green"
'
' Color Options: "blue", "green", "red", "purple", "yellow", "white", and "orange"

' You can use the RotateFlasher call to align the Rotz/ObjRotz of the flasher primitives with "handles".  Don't set those values in the editor,
' call the RotateFlasher sub instead (this call will likely crash VP if it's call for the flasher primitives without "handles")
'
' Example: RotateFlasher 1, 180 		'where 1 is the flasher number and 180 is the angle of Z rotation

' For flashing the flasher use in the script: "ObjLevel(1) = 1 : FlasherFlash1_Timer"
' This should also work for flashers with variable flash levels from the rom, just use ObjLevel(1) = xx from the rom (in the range 0-1)
'
' Notes (please read!!):
' - Setting TestFlashers = 1 (below in the ScriptsDirectory) will allow you to see how the flasher objects are aligned (need the targetflasher image imported to your table)
' - The rotation of the primitives with "handles" is done with a script command, not on the primitive itself (see RotateFlasher below)
' - Color of the objects are set in the script, not on the primitive itself
' - Screws are optional to copy and position manually
' - If your table is not named "Table1" then change the name below in the script
' - Every flasher uses its own material (Flashermaterialxx), do not use it for anything else
' - Lighting > Bloom Strength affects how the flashers look, do not set it too high
' - Change RotY and RotX of flasherbase only when having a flasher something other then parallel to the playfield
' - Leave RotX of the flasherflash object to -45; this makes sure that the flash effect is visible in FS and DT
' - If you want to resize a flasher, be sure to resize flasherbase, flasherlit and flasherflash with the same percentage
' - If you think that the flasher effects are too bright, change flasherlightintensity and/or flasherflareintensity below

' Some more notes for users of the v1 flashers and/or JP's fading lights routines:
' - Delete all textures/primitives/script/materials in your table from the v1 flashers and scripts before you start; they don't mix well with v2
' - Remove flupperflash(m) routines if you have them; they do not work with this new script
' - Do not try to mix this v2 script with the JP fading light routine (that is making it too complicated), just use the example script below

' example script for rom based tables (non modulated):

' SolCallback(25)="FlashRed"
'
' Sub FlashRed(flstate)
'	If Flstate Then
'		ObjTargetLevel(1) = 1
'	Else
'		ObjTargetLevel(1) = 0
'	End If
'   FlasherFlash1_Timer
' End Sub

' example script for rom based tables (modulated):

' SolModCallback(25)="FlashRed"
'
' Sub FlashRed(level)
'	ObjTargetLevel(1) = level/255 : FlasherFlash1_Timer
' End Sub



 Sub Flash1(Enabled)
	If Enabled Then		
		ObjTargetLevel(1) = 1
	Else
		ObjTargetLevel(1) = 0
	End If
	FlasherFlash1_Timer
	Sound_Flash_Relay enabled, Flasherbase1
 End Sub

 Sub Flash2(Enabled)
	If Enabled Then
		ObjTargetLevel(2) = 1
	Else
		ObjTargetLevel(2) = 0
	End If
	FlasherFlash2_Timer
	Sound_Flash_Relay enabled, Flasherbase2
 End Sub

 Sub Flash3(Enabled)
	If Enabled Then		
		ObjTargetLevel(3) = 1
	Else
		ObjTargetLevel(3) = 0
	End If
	FlasherFlash3_Timer
	Sound_Flash_Relay enabled, Flasherbase3
 End Sub

 Sub Flash4(Enabled)
	If Enabled Then
		ObjTargetLevel(4) = 1
	Else
		ObjTargetLevel(4) = 0
	End If
	FlasherFlash4_Timer
	Sound_Flash_Relay enabled, Flasherbase1
 End Sub




Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.3		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.2		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20), ObjTargetLevel(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"

InitFlasher 1, "green"
InitFlasher 2, "green"
InitFlasher 3, "blue"
InitFlasher 4, "White"
InitFlasher 5, "red"
InitFlasher 6, "purple"
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 


Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 40
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness

	'rothbauerw
	'Adjust the position of the flasher object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	If objbase(nr).roty > 135 then
		objflasher(nr).y = objbase(nr).y + 50
		objflasher(nr).height = objbase(nr).z + 20
	Else
		objflasher(nr).y = objbase(nr).y + 20
		objflasher(nr).height = objbase(nr).z + 50
	End If
	objflasher(nr).x = objbase(nr).x

	'rothbauerw
	'Adjust the position of the light object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	objlight(nr).x = objbase(nr).x
	objlight(nr).y = objbase(nr).y
	objlight(nr).bulbhaloheight = objbase(nr).z -10

	'rothbauerw
	'Assign the appropriate bloom image basked on the location of the flasher base
	'Comment out these lines if you want to manually assign the bloom images
	dim xthird, ythird
	xthird = tablewidth/3
	ythird = tableheight/3

	If objbase(nr).x >= xthird and objbase(nr).x <= xthird*2 then
		objbloom(nr).imageA = "flasherbloomCenter"
		objbloom(nr).imageB = "flasherbloomCenter"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird then
		objbloom(nr).imageA = "flasherbloomUpperLeft"
		objbloom(nr).imageB = "flasherbloomUpperLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird then
		objbloom(nr).imageA = "flasherbloomUpperRight"
		objbloom(nr).imageB = "flasherbloomUpperRight"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird*2 then
		objbloom(nr).imageA = "flasherbloomCenterLeft"
		objbloom(nr).imageB = "flasherbloomCenterLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird*2 then
		objbloom(nr).imageA = "flasherbloomCenterRight"
		objbloom(nr).imageB = "flasherbloomCenterRight"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird*3 then
		objbloom(nr).imageA = "flasherbloomLowerLeft"
		objbloom(nr).imageB = "flasherbloomLowerLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird*3 then
		objbloom(nr).imageA = "flasherbloomLowerRight"
		objbloom(nr).imageB = "flasherbloomLowerRight"
	end if

	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objbloom(nr).color = RGB(4,120,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4) : objbloom(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(204,0,0) : objflasher(nr).color = RGB(255,32,4) : objbloom(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) : objbloom(nr).color = RGB(230,49,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50) : objbloom(nr).color = RGB(200,173,25)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objbloom(nr).color = RGB(255,240,150)
		Case "orange" :  objlight(nr).color = RGB(255,70,0) : objflasher(nr).color = RGB(255,70,0) : objbloom(nr).color = RGB(255,70,0)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	if round(ObjTargetLevel(nr),1) > round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) + 0.3
		if ObjLevel(nr) > 1 then ObjLevel(nr) = 1
	Elseif round(ObjTargetLevel(nr),1) < round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) * 0.85 - 0.01
		if ObjLevel(nr) < 0 then ObjLevel(nr) = 0
	Else
		ObjLevel(nr) = round(ObjTargetLevel(nr),1)
		objflasher(nr).TimerEnabled = False
	end if
	'ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 

Sub Gate003_Hit()
  GatesWire_hit(Gate003)
End Sub

Sub Gate004_Hit()
  GatesWire_hit(Gate004)
End Sub

Sub Gate006_Hit()
  GatesWire_hit(Gate006)
End Sub

Sub Gate007_Hit()
    GatesWire_hit(Gate007)
End Sub

Sub Gate008_Hit()
    GatesWire_hit(Gate008)
End Sub

Sub Gate009_Hit()
    GatesWire_hit(Gate009)
End Sub

Sub Gate012_Hit()
    GatesWire_hit(Gate012)
End Sub

Sub GateModeKicker_Hit()
    GatesWire_hit(GateModeKicker)
End Sub

Sub kickbacklg_Hit()
    GatesWire_hit(kickbacklg)
End Sub

Sub LoadDogPicture
    On Error Resume Next
    LoadTexture "DogPic", "dog.png"
    If Err.Number = 0 Then DogPicture.ImageA = "DogPic"
    Err.Clear
    On Error Goto 0
End Sub






