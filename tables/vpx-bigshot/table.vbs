'############################################################################################
'############################################################################################
'#######                                                                             ########
'#######                               Big Shot                                      ########
'#######                           (Gottlieb 1974)                                   ########
'#######							 IPD No. 271									 ########	
'#######                                                                             ########
'############################################################################################
'############################################################################################
'*****************************************************************************************************
' CREDITS
' Blender - bord
' Playfield and Plastics Redraw - Shannon1
' Backglass - rothbauerw
' Script and Physics - rothbauerw
' Mechanical Sounds - Fleep
' Previous Authors - Loserman, Wed21
'*****************************************************************************************************

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

Const ballsize = 50
Const ballmass = 1

Const cGameName = "Big_Shot_1974"

'******************************************************
' 						OPTIONS
'******************************************************

Const BallsPerGame = 3				'3 or 5
Const InBallBonus = False			'True will score ball bonus with special and reset drop targets, False is default and simulates actual game rules
Const TrackHighScore = True			'True to track high scores and show high score tape on Apron
Const ResetHighScore = False		'Change to True to reset the high score on next table load
Const ShowPlayfieldReel = False		'set to True to show Playfield EM-Reel and Post-It for Player Scores (instead of a B2S backglass)

'///////////////////////-----General Sound Options-----///////////////////////
'//  VolumeDial:
'//  VolumeDial is the actual global volume multiplier for the mechanical sounds.
'//  Values smaller than 1 will decrease mechanical sounds volume.
'//  Recommended values should be no greater than 1.
Const VolumeDial = 0.8
Const DisableDOFChimes = 1			'0 for DOF Chimes, 1 to disable

'///////////////////////-----VR Options-----///////////////////////
Const VRRoomOn = 0					'Turn VR Room On/Off: 1 for on (can be toggled in-game with right mangasave), 0 for off
Const GlassOn = 0					'Turn playfield glass On/Off: 1 for on, 0 for off

'******************************************************
' 						VARIABLES
'******************************************************

' Set Replay

Dim Replay1, Replay2, Replay3

If BallsPerGame = 3 Then
	card1.image = "card3ball"
	card2.image  = "card50000"
	Replay1 = 50000
	Replay2 = 64000
	Replay3 = 72000
Else
	card1.image = "card5ball"
	card2.image  = "card65000"
	Replay1 = 65000
	Replay2 = 79000
	Replay3 = 87000
End If

Dim i
Dim obj
Dim InProgress:InProgress = False
Dim BallInPlay
Dim MaxPlayers:MaxPlayers = 2
Dim Players, Player, HighPlayer
Dim PrevScore(4)
Dim Score(4), isScoring: isScoring = False
Dim HighScore
Dim Initials
Dim Credits
Dim Match
Dim TableTilted
Dim GameOn:GameOn = False

Dim ReelP1Value1, ReelP1Value10, ReelP1Value100, ReelP1Value1000, ReelP1Value10000 
Dim ReelP2Value1, ReelP2Value10, ReelP2Value100, ReelP2Value1000, ReelP2Value10000

Dim GameOver: GameOver = False
Dim TableWidth, TableHeight

Dim togglestate, togglebip, togglepup, toggleplayers, toggletilt, togglebonus, toggledrops, togglespots, togglerubbers
Dim bistate, gamestate

Dim xx,LStep,RStep
Dim dooralreadyopen
Dim TargetSpecialLit
Dim ScoreDisplay(4)
Dim BonusMultiplier,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15
Dim AlternatingRelay

Dim DTSound:DTSound = "DTDrop"

Dim LastChime10, LastChime100, LastChime1000
Dim CustomBall, BSBall

Dim WireZ: WireZ = -10
Dim DropZ: DropZ = -50
Dim DropUp: DropUp = 10
Dim DropDown: DropDown = 5
Dim TargetY: TargetY = -8
Dim RubberCount:RubberCount = 0
Dim DropCount: DropCount = 0

'******************************************************
' 						TABLE INIT
'******************************************************

Dim DesktopMode, FSSMode

Sub BigShot_Init
	LoadEM

	If BigShot.ShowDT = False or BigShot.ShowFSS = True then
		for each i in DT : i.visible = 0 : next
		DesktopMode = False
		FSSMode = True		
	Else
		DesktopMode = True
		FSSMode = False
	End If

	loadhs
	If ResetHighScore Then
		HighScore=35000
		HSA1 = ""
		HSA2 = ""
		HSA3 = ""
	End If
	if HSA1="" then HSA1=23
	if HSA2="" then HSA2=10
	if HSA3="" then HSA3=18
	UpdatePostIt

	TableTilted=0
	InProgress = false

	Set BSBall = Drain.CreateSizedBallWithMass (Ballsize/2, BallMass)

	gamestate = 0
 	dooralreadyopen=0
 	TargetSpecialLit = 0

	GIOFF

	' FS Scoreboard
	P_Credits.image = cstr(Credits mod 10)
	if not ShowPlayfieldReel Then
		ReelWall.isdropped = True
		P_Reel0.Transz = -100
		P_Reel1.Transz = -100
		P_Reel2.Transz = -100
		P_Reel3.Transz = -100
		P_Reel4.Transz = -100
		P_Reel5.Transz = -100
		P_ActivePlayer.Transz = -100
		P_Credits.Transz = -100
		P_CreditsText.Transz = -100
		P_BallinPlay.Transz = -100
		P_BallinPlayText.Transz = -100
	end If
	HideScoreboard

	If TrackHighScore = False Then
		For each i in HighScoreTape: i.visible = false: Next
	End If 

	WindowGlass.visible = GlassOn
	GlassImpurities.visible = GlassOn
	If VRRoomOn = 1 Then 
		VRToggle = 1
		VRRoomToggle
	Else
		VRRoomToggle
	End If

	If Credits > 0 Then DOF 125, 1
End Sub

Sub BigShot_Exit()
	savehs
	DOF 101, 0
	DOF 102, 0
	DOF 103, 0
	DOF 104, 0
	DOF 105, 0
	DOF 106, 0
	DOF 107, 0
	DOF 108, 0
	DOF 110, 0
	DOF 111, 0
	DOF 112, 0
	DOF 113, 0
	DOF 114, 0
	DOF 115, 0
	DOF 116, 0
	DOF 117, 0
	DOF 118, 0
	DOF 119, 0
	DOF 120, 0
	DOF 121, 0
	DOF 122, 0
	DOF 123, 0
	DOF 124, 0
	DOF 125, 0
	DOF 126, 0
	DOF 127, 0
	DOF 153, 0
	DOF 154, 0
	DOF 155, 0
	DOF 80, 0
	DOF 25, 0
	DOF 26, 0
	DOF 30, 0
	DOF 31, 0
	DOF 32, 0
	DOF 33, 0
	DOF 34, 0
	DOF 35, 0
	DOF 36, 0

	If B2SOn Then Controller.Stop
End Sub

TableWidth = BigShot.width
TableHeight = BigShot.height

Dim BootCount:BootCount = 0

Sub BootTable_Timer()
	dim xx
	If BootCount = 0 Then
		BootCount = 1
		me.interval = 50
		PlaySoundAt "poweron", Plunger
	Else
		ScoreReel1.setvalue score(1)
		ScoreReel2.setvalue score(2)
		CreditReel.setvalue Credits

		'*****GI Lights On
		GameOver = False

		Light1.state = B1
		Light2.state = B2
		Light3.state = B3
		Light4.state = B4
		Light5.state = B5
		Light6.state = B6
		Light7.state = B7
		Light8.state = B8:
		Light9.state = B9
		Light10.state = B10
		Light11.state = B11
		Light12.state = B12 
		Light13.state = B13
		Light14.state = B14
		Light15.state = B15

		GIOn

		SetReels

		GameOn = True
		me.enabled = False
	End If
End Sub

Sub BootB2S_Timer()
	If B2SOn Then 
		Controller.B2SSetCredits Credits
		Controller.B2SSetScorePlayer 1,Score(1)
		Controller.B2SSetScorePlayer 2,Score(2)
	End If

	me.enabled = False
End Sub

'******************************************************
' 						KEYS
'******************************************************
Const ReflipAngle = 20

Sub BigShot_KeyDown(ByVal keycode)

	If keycode = PlungerKey Then
		Plunger.PullBack
		SoundPlungerPull
    TimerVRPlunger.enabled = true  ' We enable the plunger timer below..  look for the TimerVPlunger Sub.
    TimerVRPlunger2.enabled = False' We disaable the plunger2 timer below..  look for the TimerVPlunger2 Sub.

	End If

	If keycode = LeftFlipperKey Then VRFlipperButtonLeft.transx = 10
	If keycode = RightFlipperKey Then VRFlipperButtonRight.transx = -10
	If keycode = StartGameKey Then VR_StartButton.transy = 60
	If keycode = RightMagnaSave Then VRRoomToggle

	If keycode = LeftFlipperKey and InProgress=true and TableTilted=0 Then
		FlipperActivate LeftFlipper, LFPress
		LeftFlipper.RotateToEnd

		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If

		DOF 101, 1
		PlaySoundAtVolLoops "buzzL",LeftFlipper,0.05,-1
	End If
    
	If keycode = RightFlipperKey  and InProgress=true and TableTilted=0 Then
		FlipperActivate RightFlipper, RFPress
		RightFlipper.RotateToEnd

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If

		DOF 102, 1
		PlaySoundAtVolLoops "buzz",LeftFlipper,0.05,-1	
	End If

	If keycode = LeftMagnaSave Then
		If CustomBall Then 
			CustomBall = 0
			BSBall.DecalMode=FALSE
			BSBall.image="BALL BEST"
			BSBall.FrontDecal="BALL SCRATCHES"
			BSBall.BulbIntensityScale=1
			BSBall.PlayfieldReflectionScale=1
		Else
			CustomBall = 1
			BSBall.DecalMode=True
			BSBall.image="BALL BLANK BLACK"
			BSBall.FrontDecal="BALL RED DOT"
			BSBall.BulbIntensityScale=1
			BSBall.PlayfieldReflectionScale=1
		End If
	End If
    
	If keycode = LeftTiltKey Then
		SoundNudgeLeft()
		Nudge 90, 2
		checktilt
	End If
    
	If keycode = RightTiltKey Then
		SoundNudgeRight()
		Nudge 270, 2
		checktilt
	End If
    
	If keycode = CenterTiltKey Then
		SoundNudgeCenter()
		Nudge 0, 2
		checktilt
	End If

	If keycode = MechanicalTilt Then
		gametilted
	End If

	If keycode = AddCreditKey And GameOn then
		coindelay.enabled=true
	end if

	if keycode=StartGameKey and Credits>0 and InProgress=false And GameOn And Not HSEnterMode=true  and PulseCreditReel.enabled = 0 then
		AddCredit(-1)
		soundStartButton()
		StartNewGame.enabled = True
	elseif keycode=StartGameKey and Credits>0 and InProgress=true And GameOn And Not HSEnterMode=true and BallInPlay = 1 and Players = 1 and PulseCreditReel.enabled = 0 then
		AddCredit(-1)
		soundStartButton()
		playsoundAt "AddPlayer", Motorsounds
		Players = 2
		toggleplayers = 1
	end if

	If HSEnterMode Then HighScoreProcessKey(keycode)
  End Sub

Sub BigShot_KeyUp(ByVal keycode)

	If keycode = PlungerKey Then
		If BSBall.x > 860 and BSBall.y > 1610 Then
			SoundPlungerReleaseBall
		Else
			SoundPlungerReleaseNoBall		
		End If
		Plunger.Fire

   TimerVRPlunger.enabled = false  'Disabling the plungertimer.. this is used to animate the plunger
   TimerVRPlunger2.enabled = true  'enabling the plungertimer.. this is used to animate the plunger


	End If

	If keycode = LeftFlipperKey Then VRFlipperButtonLeft.transx = 0
	If keycode = RightFlipperKey Then VRFlipperButtonRight.transx = 0
	If keycode = StartGameKey Then VR_StartButton.transy = 55
    
	If keycode = LeftFlipperKey  and InProgress=true and TableTilted=0 Then
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart

		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel

		DOF 101, 0
		StopSound "buzzL"
	End If
    
	If keycode = RightFlipperKey  and InProgress=true and TableTilted=0 Then
		FlipperActivate RightFlipper, RFPress
		RightFlipper.RotateToStart

		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel

		DOF 102, 0
		StopSound "buzz"
	End If

End Sub

sub coindelay_timer
	AddCredit(1)
	coindelay.enabled=false
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtLevelStatic ("Coin_In_1"), CoinSoundLevel, drain
		Case 1: PlaySoundAtLevelStatic ("Coin_In_2"), CoinSoundLevel, drain
		Case 2: PlaySoundAtLevelStatic ("Coin_In_3"), CoinSoundLevel, drain
	End Select
end sub

'******************************************************
'					  GAME TIMER
' 	  Ball Rolling and Shadows, Primitive Updates,
'			B2S, Desktop, and FSS Updates
'						Lights
'******************************************************

'Dim BallShadow
'BallShadow = Array (BallShadow1)

Const tnob = 1 ' total number of balls
ReDim rolling(tnob)

Sub GameTimer_Timer()

	'****** Flipper shadows and primitives ******
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	RFPrim.RotY = RightFlipper.currentangle
	LFPrim.RotY = LeftFlipper.currentangle

	'****** Animated gate ******
	Pgate.rotx=-Gate1.currentangle*.5

	'****** Animated Diverter ******
	wirearm.rotz = DiverterFlipper.currentangle

	'****** Insert and Bumper Lights ******
	If togglestate = 1 then
		if bistate = 0 Then
			if gameon = true and TableTilted = 0 then
				bumperbase.blenddisablelighting = 1
				bumpercap.blenddisablelighting = 1
				bumperbase.image = "bumper"
				bumpercap.image = "bumpercap"

				LightRightRollover.state = 0
				LightLeftRollover.state = 0
				LightRightTarget.state = 0
				LightLeftTarget.state = 0
			end if
		Else
			If TableTilted = 0 Then
				if gameon = true then
					bumperbase.blenddisablelighting = 0.95
					bumpercap.blenddisablelighting = 0.95
					bumperbase.image = "bumper_lit"
					bumpercap.image = "bumpercap_lit"
				end if

				if AlternatingRelay=0 or Gamestate = 0 then
					LightLeftRollover.state=1
					LightRightRollover.state=0
					if TargetSpecialLit=1 then
						LightRightTarget.state=1
						LightLeftTarget.state=0
					end if
				else
					LightLeftRollover.state=0
					LightRightRollover.state=1
					if TargetSpecialLit=1 then
						LightRightTarget.state=0
						LightLeftTarget.state=1
					end if
				end if

				If Light8.state = 1 Then
					Top8.state = 0
				Else
					Top8.state = 1
				End If
			End If
		End If

		togglestate = 0
	End If

	'****** 8 Ball Light and Cup ******
	Light16.state = top8.state
	If Light16.state = 1 Then
		layer1cup.blenddisablelighting = 4
	Else
		layer1cup.blenddisablelighting = 0.85
	End If

	If togglebonus = 1 Then
		If BonusMultiplier = 3 Then
			BL1.state = 0
			BL2.state = 0
			BL3.state = 1
		Elseif BonusMultiplier = 2 Then
			BL1.state = 0
			BL2.state = 1
			BL3.state = 0
		Else
			BL1.state = 1
			BL2.state = 0
			BL3.state = 0
		End If	

		togglebonus = 0
	End If


	'B2S Updates ***************************************************************************
	If B2Son and GameOn Then
		If InProgress = False and GameOver = False Then
			If Match = 0 Then
				Controller.B2SSetMatch 100
			Else
				Controller.B2SSetMatch Match
			End If
			Controller.B2SSetGameOver 1
			Controller.B2SSetData 80,1
			Controller.B2SSetBallInPlay 0
			Controller.B2SSetData 30,1
			Controller.B2SSetData 81,1
			Controller.B2SSetData 31, 2
		End If

		If InProgress = True and GameOver = True Then
			Controller.B2SSetMatch 0
			Controller.B2SSetScorePlayer 1,0
			Controller.B2SSetScorePlayer 2,0
			Controller.B2SSetGameOver 0
		End If

		If toggleplayers = 1 Then
			If Players = 1 Then Controller.B2SSetData 31,1
			If Players = 2 Then Controller.B2SSetData 31,2 
		End If

		If togglepup = 1 Then
			If Player = 1 Then Controller.B2SSetData 30,1: Controller.B2SSetData 81,0
			If Player = 2 Then Controller.B2SSetData 81,1: Controller.B2SSetData 30,0		
		End If

		If togglebip = 1 Then
			Controller.B2SSetBallInPlay BallInPlay
		End If

		If Toggletilt = 1 Then
			If TableTilted = 1 Then Controller.B2SSetTilt 1 Else Controller.B2SSetTilt 0
		End If

	End If

	'Desktop Updates ***********************************************************************
	If DesktopMode and GameOn Then

		If InProgress = False and GameOver = False Then
			If Match = 0 Then
				Matchtxt.text = "Match 00"		
			Else
				Matchtxt.text = "Match " & Match
			End If
			gamov.text = "GAME OVER"
			biptext.text= " "
		End If

		If InProgress = True and GameOver = True Then
			Matchtxt.text = ""
			ScoreReel1.resettozero
			ScoreReel2.resettozero
			gamov.text = ""
		End If

		If togglepup = 1 Then
			If Player = 1 Then
				pup1txt.text = "1 Up"
				pup2txt.text = ""
			Elseif Player = 2 Then
				pup1txt.text = ""
				pup2txt.text = "2 Up"
			Else
				pup1txt.text = ""
				pup2txt.text = ""
			End If
		End If

		If togglebip = 1 Then
			If BallInPlay = 0 Then
				biptext.text = ""
			Else
				biptext.text = "Ball In Play " & BallInPlay
			End If
		End If

		If Toggletilt = 1 Then
			If TableTilted = 1 Then Tilttxt.text = "TILT"
			If TableTilted = 0 Then Tilttxt.text = ""
		End If
	End If

	'FS Same Screen Updates ***********************************************************************

		If togglepup = 1 Then
			If Player <> 0 Then P_ActivePlayer.image = "Player" & CStr(Player)
		End If

		If togglebip = 1 Then
			P_BallinPlay.image = CStr(BallinPlay)
		End If


	'FSS Updates ***************************************************************************
	If GameOn Then
		If InProgress = False and GameOver = False Then
			If Match = 0 Then bgmatch00.visible = 1 else bgmatch00.visible = 0
			If Match = 10 Then bgmatch10.visible = 1 else bgmatch10.visible = 0
			If Match = 20 Then bgmatch20.visible = 1 else bgmatch20.visible = 0
			If Match = 30 Then bgmatch30.visible = 1 else bgmatch30.visible = 0
			If Match = 40 Then bgmatch40.visible = 1 else bgmatch40.visible = 0
			If Match = 50 Then bgmatch50.visible = 1 else bgmatch50.visible = 0
			If Match = 60 Then bgmatch60.visible = 1 else bgmatch60.visible = 0
			If Match = 70 Then bgmatch70.visible = 1 else bgmatch70.visible = 0
			If Match = 80 Then bgmatch80.visible = 1 else bgmatch80.visible = 0
			If Match = 90 Then bgmatch90.visible = 1 else bgmatch90.visible = 0

			bggameover.visible=1 'Turn on Game Over

			bgball1.visible = 0
			bgball2.visible = 0
			bgball3.visible = 0
			bgball4.visible = 0
			bgball5.visible = 0	

			ToggleP1 1
			ToggleP2 1

			bg1canplay.visible=0
			bg2canplay.visible=1				
		End If

		If InProgress = True and GameOver = True Then
			bgmatch00.visible = 0
			bgmatch10.visible = 0
			bgmatch20.visible = 0
			bgmatch30.visible = 0
			bgmatch40.visible = 0
			bgmatch50.visible = 0
			bgmatch60.visible = 0
			bgmatch70.visible = 0
			bgmatch80.visible = 0
			bgmatch90.visible = 0

			bggameover.visible=0
		End If

		If toggleplayers = 1 Then
			if players = 1 Then
				bg1canplay.visible=1
				bg2canplay.visible=0				
			end if
			if players = 2 Then
				bg1canplay.visible=0
				bg2canplay.visible=1				
			End if				
		End If

		If togglepup = 1 Then 	'Set Player Up using variable 'Player'
			if player = 1 Then
				ToggleP1 1
				ToggleP2 0
			End if
			if player = 2 Then
				ToggleP1 0
				ToggleP2 1
			End if
			SetScoreReel
		End If

		If togglebip = 1 Then
			If BallInPlay = 1 Then bgball1.visible = 1 else bgball1.visible = 0
			If BallInPlay = 2 Then bgball2.visible = 1 else bgball2.visible = 0
			If BallInPlay = 3 Then bgball3.visible = 1 else bgball3.visible = 0
			If BallInPlay = 4 Then bgball4.visible = 1 else bgball4.visible = 0
			If BallInPlay = 5 Then bgball5.visible = 1 else bgball5.visible = 0			
		End If

		If Toggletilt = 1 Then 	'Set tilt using variable 'TableTilted'
			If Tabletilted = 1 Then BGTilt.visible=1 Else BGTilt.visible=0
		End If
	End If

	'Updates Toggles ***********************************************************************

	If InProgress = False and GameOver = False and GameOn Then
		GameOver = True: gamestate = 0
	End If

	If InProgress = True and GameOver = True and GameOn Then
		GameOver = False
	End If

	If toggleplayers = 1 Then
		toggleplayers = 0
	End If

	If togglepup = 1 Then
		togglepup = 0
	End If

	If togglebip = 1 Then
		togglebip = 0
	End If

	If toggletilt= 1 Then
		toggletilt = 0
	End If

	'Updates Drops ***********************************************************************

	Dim DropsComplete:DropsComplete = 1

	If toggledrops = 1 Then
		If DropBall1.collidable = False Then
			If layer4d1.transz <> DropZ Then
				layer4d1.transz = layer4d1.transz - DropDown
				If layer4d1.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall001a.collidable = False
				End If
			End If
		Else
			If layer4d1.transz <> 0 Then
				layer4d1.transz = layer4d1.transz + DropUp
				If layer4d1.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall2.collidable =False Then
			If layer3d2.transz <> DropZ Then
				layer3d2.transz = layer3d2.transz - DropDown
				If layer3d2.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall002a.collidable = False
				End If
			End If
		Else
			If layer3d2.transz <> 0 Then
				layer3d2.transz = layer3d2.transz + DropUp
				If layer3d2.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall3.collidable = False Then
			If layer4d3.transz <> DropZ Then
				layer4d3.transz = layer4d3.transz - DropDown
				If layer4d3.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall003a.collidable = False
				End If

			End If
		Else
			If layer4d3.transz <> 0 Then
				layer4d3.transz = layer4d3.transz + DropUp
				If layer4d3.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall4.collidable =False Then
			If layer3d4.transz <> DropZ Then
				layer3d4.transz = layer3d4.transz - DropDown
				If layer3d4.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall004a.collidable = False
				End If
			End If
		Else
			If layer3d4.transz <> 0 Then
				layer3d4.transz = layer3d4.transz + DropUp
				If layer3d4.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall5.collidable = False Then
			If layer4d5.transz <> DropZ Then
				layer4d5.transz = layer4d5.transz - DropDown
				If layer4d5.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall005a.collidable = False
				End If
			End If
		Else
			If layer4d5.transz <> 0 Then
				layer4d5.transz = layer4d5.transz + DropUp
				If layer4d5.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall6.collidable =False Then
			If layer3d6.transz <> DropZ Then
				layer3d6.transz = layer3d6.transz - DropDown
				If layer3d6.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall006a.collidable = False
				End If
			End If
		Else
			If layer3d6.transz <> 0 Then
				layer3d6.transz = layer3d6.transz + DropUp
				If layer3d6.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall7.collidable = False Then
			If layer4d7.transz <> DropZ Then
				layer4d7.transz = layer4d7.transz - DropDown
				If layer4d7.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall007a.collidable = False
				End If
			End If
		Else
			If layer4d7.transz <> 0 Then
				layer4d7.transz = layer4d7.transz + DropUp
				If layer4d7.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall9.collidable = False Then
			If layer4d9.transz <> DropZ Then
				layer4d9.transz = layer4d9.transz - DropDown
				If layer4d9.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall009a.collidable = False
				End If
			End If
		Else
			If layer4d9.transz <> 0 Then
				layer4d9.transz = layer4d9.transz + DropUp
				If layer4d9.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall10.collidable =False Then
			If layer3d10.transz <> DropZ Then
				layer3d10.transz = layer3d10.transz - DropDown
				If layer3d10.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall010a.collidable = False
				End If
			End If
		Else
			If layer3d10.transz <> 0 Then
				layer3d10.transz = layer3d10.transz + DropUp
				If layer3d10.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall11.collidable = False Then
			If layer4d11.transz <> DropZ Then
				layer4d11.transz = layer4d11.transz - DropDown
				If layer4d11.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall011a.collidable = False
				End If
			End If
		Else
			If layer4d11.transz <> 0 Then
				layer4d11.transz = layer4d11.transz + DropUp
				If layer4d11.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall12.collidable =False Then
			If layer3d12.transz <> DropZ Then
				layer3d12.transz = layer3d12.transz - DropDown
				If layer3d12.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall012a.collidable = False
				End If
			End If
		Else
			If layer3d12.transz <> 0 Then
				layer3d12.transz = layer3d12.transz + DropUp
				If layer3d12.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall13.collidable = False Then
			If layer4d13.transz <> DropZ Then
				layer4d13.transz = layer4d13.transz - DropDown
				If layer4d13.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall013a.collidable = False
				End If
			End If
		Else
			If layer4d13.transz <> 0 Then
				layer4d13.transz = layer4d13.transz + DropUp
				If layer4d13.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall14.collidable =False Then
			If layer3d14.transz <> DropZ Then
				layer3d14.transz = layer3d14.transz - DropDown
				If layer3d14.transz <> DropZ Then
					DropsComplete = 0
				Else 
					DropBall014a.collidable = False
				End If
			End If
		Else
			If layer3d14.transz <> 0 Then
				layer3d14.transz = layer3d14.transz + DropUp
				If layer3d14.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If DropBall15.collidable = False Then
			If layer4d15.transz <> DropZ Then
				layer4d15.transz = layer4d15.transz - DropDown
				If layer4d15.transz <> DropZ Then 
					DropsComplete = 0
				Else 
					DropBall015a.collidable = False
				End If
			End If
		Else
			If layer4d15.transz <> 0 Then
				layer4d15.transz = layer4d15.transz + DropUp
				If layer4d15.transz <> 0 Then DropsComplete = 0
			End If
		End If

		If TableTilted = 0 Then
			If layer4d1.transz = DropZ Then
				Flasher001.visible = 0
			Else	
				Flasher001.visible = 1
			End If

			If layer3d2.transz = DropZ Then
				Flasher002.visible = 0
			Else	
				Flasher002.visible = 1
			End If

			If layer4d3.transz = DropZ Then
				Flasher003.visible = 0
			Else	
				Flasher003.visible = 1
			End If

			If layer3d4.transz = DropZ Then
				Flasher004.visible = 0
			Else	
				Flasher004.visible = 1
			End If

			If layer4d5.transz = DropZ Then
				Flasher005.visible = 0
			Else	
				Flasher005.visible = 1
			End If

			If layer3d6.transz = DropZ Then
				Flasher006.visible = 0
			Else	
				Flasher006.visible = 1
			End If

			If layer4d7.transz = DropZ Then
				Flasher007.visible = 0
			Else	
				Flasher007.visible = 1
			End If

			If layer4d9.transz = DropZ Then
				Flasher009.visible = 0
			Else	
				Flasher009.visible = 1
			End If

			If layer3d10.transz = DropZ Then
				Flasher010.visible = 0
			Else	
				Flasher010.visible = 1
			End If

			If layer4d11.transz = DropZ Then
				Flasher011.visible = 0
			Else	
				Flasher011.visible = 1
			End If

			If layer3d12.transz = DropZ Then
				Flasher012.visible = 0
			Else	
				Flasher012.visible = 1
			End If

			If layer4d13.transz = DropZ Then
				Flasher013.visible = 0
			Else	
				Flasher013.visible = 1
			End If

			If layer3d14.transz = DropZ Then
				Flasher014.visible = 0
			Else	
				Flasher014.visible = 1
			End If

			If layer4d15.transz = DropZ Then
				Flasher015.visible = 0
			Else	
				Flasher015.visible = 1
			End If
		End If

	End If

	If DropsComplete = 1 Then toggledrops = 0


	'Updates Spots ***********************************************************************

	If togglespots = 1 Then
		If layer4target1.transy <> 0 Then
			layer4target1.transy = layer4target1.transy + 2
		End If

		If layer4target2.transy <> 0 Then
			layer4target2.transy = layer4target2.transy + 2
		End If

		If layer4target1.transy = 0 and layer4target2.transy = 0 Then togglespots = 0
	End If

	'Updates Rubber Animations **************************************************************

	If togglerubbers = 1 Then
		If lrubber001.visible and RubberCount >= 2 Then
			lrubber002.visible = 1
			lrubber001.visible = 0
		Elseif lrubber003.visible and RubberCount >= 2 Then
			lrubber004.visible = 1
			lrubber003.visible = 0
		Elseif (lrubber002.visible or lrubber004.visible) and RubberCount >= 4 Then
			lrubber.visible = 1
			lrubber002.visible = 0
			lrubber004.visible = 0
		End If

		If rrubber001.visible and RubberCount >= 2 Then
			rrubber002.visible = 1
			rrubber001.visible = 0
		Elseif rrubber003.visible and RubberCount >= 2 Then
			rrubber004.visible = 1
			rrubber003.visible =0
		Elseif (rrubber002.visible or rrubber004.visible) and RubberCount >= 4 Then
			rrubber.visible = 1
			rrubber002.visible = 0
			rrubber004.visible = 0
		End If

		RubberCount = RubberCount + 1

		If lrubber.visible and rrubber.visible Then
			togglerubbers = 0
			RubberCount = 0
		End If
	End If

	'Ball Rolling and Ball Shadows **********************************************************

	' play the rolling sound for each ball
	If BallVel(BSBall ) > 1 AND BSBall.z < 30 Then
		rolling(0) = True
		PlaySound ("BallRoll_0"), -1, VolPlayfieldRoll(BSBall) * 1.1 * VolumeDial, AudioPan(BSBall), 0, PitchPlayfieldRoll(BSBall), 1, 0, AudioFade(BSBall)

	Else
		If rolling(0) = True Then
			StopSound("BallRoll_0")
			rolling(0) = False
		End If
	End If
	
	'BallShadow(0).X = BSBall.X - (TableWidth/2 - BSBall.X)/20
	'ballShadow(0).Y = BSBall.Y + 10

	'If BSBall.Z > 22 and BSBall.Z < 35 Then
		'BallShadow(0).visible = 1
	'Else
		'BallShadow(0).visible = 0
	'End If

	If BSBall.VelZ < -1 and BSBall.z < 55 and BSBall.z > 27 Then 'height adjust for ball drop sounds
		If DropCount >= 5 Then
			RandomSoundBallBouncePlayfieldSoft
			DropCount = 0
		End If
	End If
	If DropCount < 5 Then
		DropCount = DropCount + 1
	End If
	
	cor.update

End Sub

Sub ToggleP1(Enabled)
	dim xx
	BGPlayer1.visible = enabled
	for each xx in P1Lights: xx.visible = enabled: Next
End Sub

Sub ToggleP2(Enabled)
	dim xx
	BGPlayer2.visible = enabled
	for each xx in P2Lights: xx.visible = enabled: Next
End Sub

'******************************************************
' 					START NEW GAME
'******************************************************

Dim NGCount

Sub StartNewGame_Timer()
	NGCount = NGCount + 1

	Select Case (NGCount)
		Case 1: 
			Players = 1
			Player = 1
			BallInPlay = 1
			InProgress=true	
			BonusMultiplier=1
			togglebonus = 1
			toggleplayers = 1
			togglebip = 1
			togglepup = 1
			If tabletilted = 1 Then ResetTilt

			PlaySoundAtVol "AddPlayer", MotorSounds, 1
			PlayLoudClick 1
			ResetReels
			bistate = 0
			togglestate = 1
		Case 2,8,14,20,26:
			PlayLoudClick 2
			ResetReels
		Case 3,9,15,21,27:
			PlayLoudClick 3
			ResetReels
		Case 4,10,16,22,28:
			PlayLoudClick 4
			ResetReels
		Case 5,11,17,23,29:
			PlayLoudClick 5
			ResetReels
			bistate = 1
			togglestate = 1
		Case 7,13,19,25:
			PlayLoudClick 1
			ResetReels
			bistate = 0
			togglestate = 1
		Case 30:
			Score(1) = 0
			Score(2) = 0
			SetReels
			SetScoreReel
			If ShowPlayfieldReel Then
				UpdateScoreboard
			End If

			gamestate = 1
			togglestate = 1 
			ResetBalls
	
			me.enabled = false
			NGCount = 0
	End Select	
End Sub

Sub PlayLoudClick(x)
	Select Case  (x)
		Case 1: PlaySoundAtVol "LoudClick1", MotorSounds, 1
		Case 2: PlaySoundAtVol "LoudClick2", MotorSounds, 1
		Case 3: PlaySoundAtVol "LoudClick3", MotorSounds, 1
		Case 4: PlaySoundAtVol "LoudClick4", MotorSounds, 1
		Case 5: PlaySoundAtVol "LoudClickLong", MotorSounds, 1
	End Select
End Sub

Sub PlayLoudClick2()
	PlaySoundAtVol "Motor_Click_Loud_Long", MotorSounds, 0.5
End Sub


Sub PlayQuietClick()
	PlaySoundAtVol "Motor_Click_Quiet_Long2", MotorSounds, 0.05
End Sub

'******************************************************
' 					START NEW BALL
'******************************************************

Sub ResetBalls()
	Dim TempMultiCounter
	TempMultiCounter=BallsPerGame-BallInPlay

	If TempMultiCounter=0 Then
		BonusMultiplier=3
	ElseIf TempMultiCounter=1 Then
		BonusMultiplier=2
	ElseIf TempMultiCounter>1 Then
		BonusMultiplier=1
	end if
	togglebonus = 1

	If TableTilted = 1 Then ResetTilt

	ResetBallDrops

	ReleaseBall
End Sub

Sub ReleaseBall
	If BallInPlay = 1 and Player = 1 Then
		PlaysoundAtVol "StartBall1", Drain, BallReleaseSoundLevel
	Else
		PlaysoundAtVol "StartBall2-5", Drain, BallReleaseSoundLevel
	End If

	Drain.Kick 60, 20
	DOF 107, 2
End Sub

Sub ResetBallDrops()
	Light1.State=0
	Light2.State=0
	Light3.State=0
	Light4.State=0
	Light5.State=0
	Light6.State=0
	Light7.State=0
	Light8.State=0
	Light9.State=0
	Light10.State=0
	Light11.State=0
	Light12.State=0
	Light13.State=0
	Light14.State=0
	Light15.State=0

	Top8.State=1

	DropBall1.collidable = True
	DropBall2.collidable = True
	DropBall3.collidable = True
	DropBall4.collidable = True
	DropBall5.collidable = True
	DropBall6.collidable = True
	DropBall7.collidable = True
	DropBall9.collidable = True
	DropBall10.collidable = True
	DropBall11.collidable = True
	DropBall12.collidable = True
	DropBall13.collidable = True
	DropBall14.collidable = True
	DropBall15.collidable = True

	DropBall001.collidable = True
	DropBall002.collidable = True
	DropBall003.collidable = True
	DropBall004.collidable = True
	DropBall005.collidable = True
	DropBall006.collidable = True
	DropBall007.collidable = True
	DropBall009.collidable = True
	DropBall010.collidable = True
	DropBall011.collidable = True
	DropBall012.collidable = True
	DropBall013.collidable = True
	DropBall014.collidable = True
	DropBall015.collidable = True

	DropBall001a.collidable = False
	DropBall002a.collidable = False
	DropBall003a.collidable = False
	DropBall004a.collidable = False
	DropBall005a.collidable = False
	DropBall006a.collidable = False
	DropBall007a.collidable = False
	DropBall009a.collidable = False
	DropBall010a.collidable = False
	DropBall011a.collidable = False
	DropBall012a.collidable = False
	DropBall013a.collidable = False
	DropBall014a.collidable = False
	DropBall015a.collidable = False

	toggledrops = 1

	PlaySoundat "dropsup", bumper1
	DOF 127, 2

	'playsoundat "KickerKick", KickerBall8

	TargetSpecialLit = 0
	LightLeftTarget.State=0
	LightRightTarget.State=0

	closegate
End Sub

'******************************************************
' 						NEXT BALL
'******************************************************

sub nextball
	If ShowPlayfieldReel Then
		UpdateScoreboard
	End If

	If Player = Players Then
		BallInPlay = BallInPlay + 1
		togglebip = 1
		Player = 1
		togglepup = 1
	Else 
		Player = Player + 1
		togglepup = 1
	End If

	if ballinplay > BallsPerGame then
		PlaySoundat "MotorLeer", MotorSounds

		'LightsOut

		InProgress = False
		BallInPlay = 0
		togglebip = 1
		Player = 0
		togglepup = 1

		If TableTilted = 1 Then ResetTilt
		CheckMatch

		LeftFlipper.RotateToStart
		StopSound "buzzL"
		DOF 101, 0

		RightFlipper.RotateToStart
		StopSound "buzz"
		DOF 102, 0

		If Score(2) > Score(1) Then
			HighPlayer = 2
		Else 
			HighPlayer = 1
		End If

		If Score(HighPlayer) >= HighScore and TrackHighScore Then
			FreeGame
			HighScore = Score(HighPlayer)
			HighScoreEntryInit()
		End If
	Else
		ResetBalls
	End If
End sub

'******************************************************
' 						DRAIN
'******************************************************

Sub Drain_Hit()
	RandomSoundDrain Drain
	
	DOF 106, DOFPulse

	ScoreBonus.interval = 1600
	ScoreBonus.enabled = True

End Sub

Sub ShooterLaneLaunch_Hit()
	RandomSoundRollover
	layer2wire005.transz = WireZ
	closegate
End Sub

Sub ShooterLaneLaunch_UnHit()
	layer2wire005.transz = 0
End Sub

Sub ShooterLane_Hit()
	DOF 123, 1
End Sub

Sub ShooterLane_unHit()
	DOF 123, 0
	DOF 124, 2
End Sub


'******************************************************
' 						BONUS
'******************************************************

Dim BonusCount, skipcount, PlayRelay

sub ScoreBonus_timer
	BonusCount = BonusCount + 1
	ScoreBonus.interval=135	

	Select Case (BonusCount)
		Case 1,7,13,19,25,31,37,43,49,55,61,67,73,79,85,91: 
			bistate = 0
		Case 6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96: 
			bistate = 1	
			If PlayRelay = 1 Then BonusRelay: PlayRelay = 0		
	End Select

	Select Case (BonusCount)
		'1-5
		Case 1: SkipCount = 0:BonusScore Light1, 5 
		Case 7: BonusScore Light2, 5 
		Case 13: BonusScore Light3, 5
		Case 19: BonusScore Light4, 5
		Case 25: BonusScore Light5, skipcount

		'6 - 10
		Case 31: SkipCount = 0:BonusScore Light6, 5
		Case 37: BonusScore Light7, 5
		Case 43: BonusScore Light8, 5
		Case 49: BonusScore Light9, 5
		Case 55: BonusScore Light10, skipcount

		'11-15
		Case 61: SkipCount = 0:BonusScore Light11, 5
		Case 67: BonusScore Light12, 5
		Case 73: BonusScore Light13, 5
		Case 79: BonusScore Light14, 5
		Case 85: BonusScore Light15, skipcount

		Case 91:
			BonusRelay
			Light8.state = 0
		Case 92:
			BonusRelay	
			Light1.state = 0
			Light2.state = 0
			Light3.state = 0
			Light4.state = 0
			Light5.state = 0
			Light6.state = 0
			Light7.state = 0
		Case 93:			
			BonusRelay	
			Light9.state = 0
			Light10.state = 0
			Light11.state = 0
			Light12.state = 0
			Light13.state = 0
			Light14.state = 0
			Light15.state = 0
		Case 96:
			SkipCount = 0
			PlayRelay = 0
			BonusCount = 0 
			me.enabled = false
			nextball
	End Select

	togglestate = 1
end sub

Sub BonusScore(lightobj, skip)
	If lightobj.state = 1 Then
		If SkipCount > 0 Then
			BonusCount = BonusCount - 7 + SkipCount
			Skipcount = 0
			Exit Sub
		End If

		AddBonus.enabled = True
		PlayRelay = 1
	Else
		BonusRelay		
		BonusCount = BonusCount + skip 
		Skipcount = SkipCount + 1
	End If
End Sub

Sub AddBonus_Timer()
	If BonusMultiplier = 1 Then
		AddScore 1000, 0
	Else
		MultiScore BonusMultiplier, 1000
	End If
	me.enabled = False
End Sub

Sub BonusRelay
	PlaySoundat"BonusRelays1", MotorSounds
End Sub

sub AddBonusInBall()
	i=0
	for each obj in BonusLights
		if obj.state=1 then
			i=i+1
		end if
	next
	MultiScore i*BonusMultiplier, 1000
end sub

'******************************************************
' 					RUBBERS/SLINGS
'******************************************************

Sub RightSlingShot_Slingshot
	RandomSoundSlingshotRight 
	DOF 104, 2		
	Addscore 10,0
	RSling.Visible = 0
	RSling1.Visible = 1
	sling1.TransZ = -25
	RStep = 0
	RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -15
		Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = -10:RightSlingShot.TimerEnabled = 0:gi1.State = 1
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	RandomSoundSlingshotLeft
	DOF 103, 2
	Addscore 10,0
	LSling.Visible = 0
	LSling1.Visible = 1
	sling2.TransZ = -25
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -15
		Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = -10:LeftSlingShot.TimerEnabled = 0:Gi4.State = 1
	End Select
	LStep = LStep + 1
End Sub

'******************************************************
' 						SWITCHES
'******************************************************

Sub DropWallLeft1_Hit()
	lrubber.visible = 0
	lrubber003.visible = 1
	togglerubbers = 1

	If TableTilted=false then
		AddScore 10, 0
	End if
End Sub

Sub DropWallLeft2_Hit()
	lrubber.visible = 0
	lrubber001.visible = 1
	togglerubbers = 1

	If TableTilted=false then
		AddScore 10, 0
	End if
End Sub
	
Sub DropWallRight1_Hit
	rrubber.visible = 0
	rrubber003.visible = 1
	togglerubbers = 1

	If TableTilted=false then
		AddScore 10, 0
	End if
End Sub

Sub DropWallRight2_Hit
	rrubber.visible = 0
	rrubber001.visible = 1
	togglerubbers = 1


	If TableTilted=false then
		AddScore 10, 0
	End if
End Sub

Sub RolloverLeftWall_Hit()
	If TableTilted=false then
		AddScore 10, 0
	End if
End Sub

Sub RolloverRightWall_Hit()
	If TableTilted=false then
		AddScore 10, 0
	End if
End Sub

Sub Trigger8Ball_Hit()
	RandomSoundRollover
	layer2wire009.transz = WireZ
	if TableTilted=false then		
		DOF 109, 1
		MultiScore 5, 100
		Light8.State=1
		Top8.State=0
		'Bottom8.State=0
		CheckAllStripes
		CheckAllSolids
	End If
End Sub

Sub Trigger8Ball_UnHit()
	DOF 109, 0
	layer2wire009.transz = 0
end sub

Sub TriggerLeft1000_Hit()
	RandomSoundRollover
	layer2wire008.transz = WireZ
	If TableTilted=false then
		DOF 108, 1
		AddScore 1000, 0
	end if
	End Sub

Sub TriggerLeft1000_UnHit()
	DOF 108, 0
	layer2wire008.transz = 0
end sub

Sub TriggerRight1000_Hit()
	RandomSoundRollover
	layer2wire010.transz = WireZ
	If TableTilted=false then
		DOF 110, 1
		AddScore 1000, 0
	end if
End Sub

Sub TriggerRight1000_UnHit()
	DOF 110, 0
	layer2wire010.transz = 0
end sub

Sub TriggerRightRollover_Hit()
	RandomSoundRollover
	layer2wire007.transz = WireZ
	If TableTilted=false then
		DOF 112, 1
		If LightRightRollover.State=1 then
			AddScore 1000, 0
		Else
			MultiScore 5, 100
		End If
	End if
End Sub

Sub TriggerRightRollover_UnHit()
	DOF 112, 0
	layer2wire007.transz = 0
end sub

Sub TriggerLeftRollover_Hit()
	RandomSoundRollover
	layer2wire006.transz = WireZ
	If TableTilted=false then
		DOF 111, 1
		If LightLeftRollover.State=1 then
			AddScore 1000, 0
		Else
			MultiScore 5, 100
		End If
	End If
End Sub

Sub TriggerLeftRollover_UnHit()
	DOF 111, 0
	layer2wire006.transz = 0
end sub

Sub TriggerLeftInlane_Hit()
	RandomSoundRollover
	layer2wire002.transz = WireZ
	If TableTilted=false then
		DOF 114, 1
		MultiScore 5, 100
	End If
End Sub

Sub TriggerLeftInlane_UnHit()
	DOF 114, 0
	layer2wire002.transz = 0
end sub

Sub TriggerRightInlane_Hit()
	RandomSoundRollover
	layer2wire003.transz = WireZ
	If TableTilted=false then
		DOF 115, 1
		MultiScore 5, 100
	End If
End Sub

Sub TriggerRightInlane_UnHit()
	DOF 115, 0
	layer2wire003.transz = 0
end sub

Sub Trigger_LeftOutlane_Hit()
	RandomSoundRollover
	layer2wire001.transz = WireZ
	If TableTilted=false then	
		DOF 113, 1
		AddScore 1000, 0
	End If
End Sub

Sub Trigger_LeftOutlane_UnHit()
	DOF 113, 0
	layer2wire001.transz = 0
end Sub

Sub Trigger_RightOutlane_Hit()
	RandomSoundRollover
	layer2wire004.transz = WireZ
	If TableTilted=false then
		DOF 116, 1
		AddScore 1000, 0
	End If
End Sub

Sub Trigger_RightOutlane_UnHit()
	DOF 116, 0
	layer2wire004.transz = 0
end Sub

'******************************************************
' 						BUMPER
'******************************************************

Sub Bumper1_Hit()
	If TableTilted=false then
		If BallsPerGame=3 then
			AddScore 1000, 0
		Else
			AddScore 100, 0
		End If
		RandomSoundBumperA		
		DOF 105, 2
	End If
End Sub

'******************************************************
' 						DROP TARGETS
'******************************************************

Sub DropBall1_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,117,DOFPulse,DOFContactors),DropBall1,DTSoundLevel
	DropBall1.collidable = False
	DropBall001.collidable = False
	DropBall001a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light1.State=1
		CheckAllSolids
	End If
End Sub

Sub DropBall2_Hit	
	PlaySoundAtVol SoundFXDOF(DTSound,117,DOFPulse,DOFContactors),DropBall2,DTSoundLevel
	DropBall2.collidable = False
	DropBall002.collidable = False
	DropBall002a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light2.State=1
		CheckAllSolids
	End If
End Sub

Sub DropBall3_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,117,DOFPulse,DOFContactors),DropBall3,DTSoundLevel
	DropBall3.collidable = False
	DropBall003.collidable = False
	DropBall003a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light3.State=1
		CheckAllSolids
	End If
End Sub

Sub DropBall4_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,117,DOFPulse,DOFContactors),DropBall4,DTSoundLevel
	DropBall4.collidable = False
	DropBall004.collidable = False
	DropBall004a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light4.State=1
		CheckAllSolids		
	End If
End Sub

Sub DropBall5_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,118,DOFPulse,DOFContactors),DropBall5,DTSoundLevel
	DropBall5.collidable = False
	DropBall005.collidable = False
	DropBall005a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light5.State=1
		CheckAllSolids
	End If
End Sub

Sub DropBall6_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,118,DOFPulse,DOFContactors),DropBall6,DTSoundLevel
	DropBall6.collidable = False
	DropBall006.collidable = False
	DropBall006a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light6.State=1
		CheckAllSolids
	End If
End Sub

Sub DropBall7_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,118,DOFPulse,DOFContactors),DropBall7,DTSoundLevel
	DropBall7.collidable = False
	DropBall007.collidable = False
	DropBall007a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light7.State=1
		CheckAllSolids
	End If
End Sub

Sub DropBall9_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,120,DOFPulse,DOFContactors),DropBall9,DTSoundLevel
	DropBall9.collidable = False
	DropBall009.collidable = False
	DropBall009a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light9.State=1
		CheckAllStripes
	End If
End Sub

Sub DropBall10_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,120,DOFPulse,DOFContactors),DropBall10,DTSoundLevel
	DropBall10.collidable = False
	DropBall010.collidable = False
	DropBall010a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light10.State=1
		CheckAllStripes
	End If
End Sub

Sub DropBall11_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,120,DOFPulse,DOFContactors),DropBall11,DTSoundLevel
	DropBall11.collidable = False
	DropBall011.collidable = False
	DropBall011a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light11.State=1
		CheckAllStripes
	End If
End Sub

Sub DropBall12_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,119,DOFPulse,DOFContactors),DropBall12,DTSoundLevel
	DropBall12.collidable = False
	DropBall012.collidable = False
	DropBall012a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light12.State=1
		CheckAllStripes
	End If
End Sub

Sub DropBall13_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,119,DOFPulse,DOFContactors),DropBall13,DTSoundLevel
	DropBall13.collidable = False
	DropBall013.collidable = False
	DropBall013a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light13.State=1
		CheckAllStripes
	End If
End Sub

Sub DropBall14_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,119,DOFPulse,DOFContactors),DropBall14,DTSoundLevel
	DropBall14.collidable = False
	DropBall014.collidable = False
	DropBall014a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light14.State=1
		CheckAllStripes
	End If
End Sub

Sub DropBall15_Hit
	PlaySoundAtVol SoundFXDOF(DTSound,119,DOFPulse,DOFContactors),DropBall15,DTSoundLevel
	DropBall15.collidable = False
	DropBall015.collidable = False
	DropBall015a.collidable = True
	toggledrops = 1

	If TableTilted=false then
		MultiScore 5, 100
		Light15.State=1
		CheckAllStripes
	End If
End Sub

Sub CheckAllSolids()
	If TargetSpecialLit=0 then
		If Light1.State=1 and Light2.State=1 and Light3.State=1 and Light4.State=1 and Light5.State=1 and Light6.State=1 and Light7.State=1 and Light8.State=1 then
			TargetSpecialLit=1
			togglestate = 1
		End If
	End If
End Sub

Sub CheckAllStripes()
	If TargetSpecialLit=0 then
		If Light9.State=1 and Light10.State=1 and Light11.State=1 and Light12.State=1 and Light13.State=1 and Light14.State=1 and Light15.State=1 and Light8.State=1 then
			TargetSpecialLit=1
			togglestate = 1
		End If
	End If
End Sub

'******************************************************
' 						SPOT TARGETS
'******************************************************

Sub TargetLeft1_Hit()
	layer4target1.transY = -8
	togglespots = 1
	
	If TableTilted=false then
		DOF 121, 2
		If LightLeftTarget.State=1 Then
			FreeGame
			If InBallBonus Then
				AddBonusInBall
				ResetBallDrops
			End If
		Else
			AddScore 100, 0
		End If
	End If
End Sub

Sub TargetRight1_Hit()
	layer4target2.transY = -8
	togglespots = 1

	If TableTilted=false then
		DOF 122, 2
		If LightRightTarget.State=1 Then
			FreeGame
			If InBallBonus Then
				AddBonusInBall
				ResetBallDrops
			End If
		Else
			AddScore 100, 0
		End If
	End If
End Sub


'******************************************************
' 						CREDITS
'******************************************************

Sub AddCredit(direction)
	If PulseCreditReel.enabled = 0 Then
		if direction > 0 and credits >= 15 Then
			'do Nothing
		Else
			credits = credits + direction

			CreditDir = direction
			PulseCreditReel.enabled = 1

			PlaySoundAtVolLoops "reelclick", MotorSounds, ReelClickVol, 0
		End If

		If Credits > 0 Then 
			DOF 125, 1
		Else 
			DOF 125, 0
		End If

		P_Credits.image = cstr(Credits mod 10)
		CreditReel.setvalue Credits
		If B2SOn Then Controller.B2SSetCredits Credits
	End If
End Sub

Sub FreeGame()
	KnockerSolenoid	
	DOF 126, 2
	AddCredit(1)
End Sub

'******************************************************
' 						SCORING
'******************************************************

Sub AddScore(x,y)
	If TableTilted = 0 and (isScoring = False or y = 1) Then
		PrevScore(Player) = Score(Player)

		If x = 10 Then
			CheckForRoll 10, Player
			Score(Player) = Score(Player) + 10
			AdvMatch
			ToggleAlternatingRelay
			isScoring = True
		ElseIf x = 100 Then
			CheckForRoll 100, Player
			Score(Player) = Score(Player) + 100			
			isScoring = True
		ElseIf x = 1000 Then
			CheckForRoll 1000, Player
			Score(Player) = Score(Player) + 1000			
			isScoring = True
		End If
		CheckFreeGame
	End If

	SetScoreReel
	EVAL("ScoreReel"&player).setvalue score(player)
	If B2SOn Then 
		Controller.B2SSetScorePlayer Player,Score(Player)
	End If
End Sub

Sub CheckForRoll(x, pnum)
	Select Case (x)
		Case 1: 
			If pnum = 1 then
				PulseReelP1_1.enabled = 1
				ReelP1Value1 = (ReelP1Value1 + 1) mod 10
				PlaySoundAtVol "reelclick", emreel1_p1, ReelClickVol
				if emreel1_p1.rotx = 261 then CheckForRoll 10, 1
			Else
				PulseReelP2_1.enabled = 1
				ReelP2Value1 = (ReelP2Value1 + 1) mod 10
				PlaySoundAtVol "reelclick", emreel1_p2, ReelClickVol
				if emreel1_p2.rotx = 261 then CheckForRoll 10, 2
			End If
		Case 10:
			If pnum = 1 then
				PulseReelP1_10.enabled = 1
				ReelP1Value10 = (ReelP1Value10 + 1) mod 10
				PlaySoundAtVol "reelclick", emreel10_p1, ReelClickVol
				if emreel10_p1.rotx = 261 then CheckForRoll 100, 1 Else PlayChime(10)
			Else
				PulseReelP2_10.enabled = 1
				ReelP2Value10 = (ReelP2Value10 + 1) mod 10
				PlaySoundAtVol "reelclick", emreel10_p2, ReelClickVol
				if emreel10_p2.rotx = 261 then CheckForRoll 100, 2  Else PlayChime(10)
			End If
		Case 100:
			If pnum = 1 then
				PulseReelP1_100.enabled = 1
				ReelP1Value100 = (ReelP1Value100 + 1) mod 10
				PlaySoundAtVol "reelclick", emreel100_p1, ReelClickVol
				if emreel100_p1.rotx = 261 then CheckForRoll 1000, 1 Else PlayChime(100)
			Else
				PulseReelP2_100.enabled = 1
				ReelP2Value100 = (ReelP2Value100 + 1) mod 10
				PlaySoundAtVol "reelclick", emreel100_p2, ReelClickVol
				if emreel100_p2.rotx = 261 then CheckForRoll 1000, 2 Else PlayChime(100)
			End If
		Case 1000:
			If pnum = 1 then
				PulseReelP1_1000.enabled = 1
				ReelP1Value1000 = (ReelP1Value1000 + 1) mod 10
				PlaySoundAtVol "reelclick", emreel1000_p1, ReelClickVol
				if emreel1000_p1.rotx = 261 then CheckForRoll 10000, 1
			Else
				PulseReelP2_1000.enabled = 1
				ReelP2Value1000 = (ReelP2Value1000 + 1) mod 10
				PlaySoundAtVol "reelclick", emreel1000_p2, ReelClickVol
				if emreel1000_p2.rotx = 261 then CheckForRoll 10000, 2
			End If
			 PlayChime(1000)
		Case 10000:
			If pnum = 1 then
				PulseReelP1_10000.enabled = 1
				ReelP1Value10000 = (ReelP1Value10000 + 1) mod 10
				PlaySoundAtVol "reelclick", emreel10000_p1, ReelClickVol				
			Else
				PulseReelP2_10000.enabled = 1
				ReelP2Value10000 = (ReelP2Value10000 + 1) mod 10
				PlaySoundAtVol "reelclick", emreel10000_p2, ReelClickVol				
			End If
	End Select
End Sub

Sub CheckFreeGame()
	If PrevScore(Player) < Replay1 And Score(Player) >= Replay1 Then FreeGame
	If PrevScore(Player) < Replay2 And Score(Player) >= Replay2 Then FreeGame
	If PrevScore(Player) < Replay3 And Score(Player) >= Replay3 Then FreeGame	
End Sub

Dim mCountDown, mScore

Sub MultiScore(cycle, Score)
	If isScoring = false Then
		mScore = Score
		mCountDown = cycle - 1
		AddScore mscore, 1
		MultiScoreTimer.enabled = true
		If cycle = 5 then bistate = 0 : togglestate = 1
	End If
End Sub

Sub MultiScoreTimer_Timer
	mCountDown = mCountDown - 1
	AddScore mscore, 1		
	If mCountDown < 1 Then
		bistate = 1: togglestate = 1
		me.enabled = False
	End If
End Sub

'******************************************************
' 						EMREELS
'******************************************************

Dim ReelStep:ReelStep = 6
Dim ReelClickVol:ReelClickVol=0.1

Sub PulseReelP1_1_Timer
	dim done:done=0
	emreel1_p1.rotx = emreel1_p1.rotx + ReelStep
	if emreel1_p1.rotx > 360 then emreel1_p1.rotx = emreel1_p1.rotx - 360
	
	Select Case (emreel1_p1.rotx)
		Case 9: done = 1
		Case 45: done = 1
		Case 81: done = 1
		Case 117: done = 1
		Case 153: done = 1
		Case 189: done = 1
		Case 225: done = 1
		Case 261: done = 1
		Case 297: done = 1
		Case 333: done = 1
	End Select

	if done = 1 then  
		me.enabled = 0
		if MultiScoreTimer.enabled = False Then isScoring = False
	End If
End Sub

Sub PulseReelP1_10_Timer
	dim done:done=0
	emreel10_p1.rotx = emreel10_p1.rotx + ReelStep
	if emreel10_p1.rotx > 360 then emreel10_p1.rotx = emreel10_p1.rotx - 360
	
	Select Case (emreel10_p1.rotx)
		Case 9: done = 1
		Case 45: done = 1
		Case 81: done = 1
		Case 117: done = 1
		Case 153: done = 1
		Case 189: done = 1
		Case 225: done = 1
		Case 261: done = 1
		Case 297: done = 1
		Case 333: done = 1
	End Select

	if done = 1 then  
		me.enabled = 0
		if MultiScoreTimer.enabled = False Then isScoring = False
	End If
End Sub

Sub PulseReelP1_100_Timer
	dim done:done=0
	emreel100_p1.rotx = emreel100_p1.rotx + ReelStep
	if emreel100_p1.rotx > 360 then emreel100_p1.rotx = emreel100_p1.rotx - 360
	
	Select Case (emreel100_p1.rotx)
		Case 9: done = 1
		Case 45: done = 1
		Case 81: done = 1
		Case 117: done = 1
		Case 153: done = 1
		Case 189: done = 1
		Case 225: done = 1
		Case 261: done = 1
		Case 297: done = 1
		Case 333: done = 1
	End Select

	if done = 1 then  
		me.enabled = 0
		if MultiScoreTimer.enabled = False Then isScoring = False
	End If
End Sub

Sub PulseReelP1_1000_Timer
	dim done:done=0
	emreel1000_p1.rotx = emreel1000_p1.rotx + ReelStep
	if emreel1000_p1.rotx > 360 then emreel1000_p1.rotx = emreel1000_p1.rotx - 360
	
	Select Case (emreel1000_p1.rotx)
		Case 9: done = 1
		Case 45: done = 1
		Case 81: done = 1
		Case 117: done = 1
		Case 153: done = 1
		Case 189: done = 1
		Case 225: done = 1
		Case 261: done = 1
		Case 297: done = 1
		Case 333: done = 1
	End Select

	if done = 1 then  
		me.enabled = 0
		if MultiScoreTimer.enabled = False Then isScoring = False
	End If
End Sub

Sub PulseReelP1_10000_Timer
	dim done:done=0
	emreel10000_p1.rotx = emreel10000_p1.rotx + ReelStep
	if emreel10000_p1.rotx > 360 then emreel10000_p1.rotx = emreel10000_p1.rotx - 360
	
	Select Case (emreel10000_p1.rotx)
		Case 9: done = 1
		Case 45: done = 1
		Case 81: done = 1
		Case 117: done = 1
		Case 153: done = 1
		Case 189: done = 1
		Case 225: done = 1
		Case 261: done = 1
		Case 297: done = 1
		Case 333: done = 1
	End Select

	if done = 1 then  
		me.enabled = 0
		if MultiScoreTimer.enabled = False Then isScoring = False
	End If
End Sub

Sub PulseReelP2_1_Timer
	dim done:done=0
	emreel1_p2.rotx = emreel1_p2.rotx + ReelStep
	if emreel1_p2.rotx > 360 then emreel1_p2.rotx = emreel1_p2.rotx - 360
	
	Select Case (emreel1_p2.rotx)
		Case 9: done = 1
		Case 45: done = 1
		Case 81: done = 1
		Case 117: done = 1
		Case 153: done = 1
		Case 189: done = 1
		Case 225: done = 1
		Case 261: done = 1
		Case 297: done = 1
		Case 333: done = 1
	End Select

	if done = 1 then  
		me.enabled = 0
		if MultiScoreTimer.enabled = False Then isScoring = False
	End If
End Sub

Sub PulseReelP2_10_Timer
	dim done:done=0
	emreel10_p2.rotx = emreel10_p2.rotx + ReelStep
	if emreel10_p2.rotx > 360 then emreel10_p2.rotx = emreel10_p2.rotx - 360
	
	Select Case (emreel10_p2.rotx)
		Case 9: done = 1
		Case 45: done = 1
		Case 81: done = 1
		Case 117: done = 1
		Case 153: done = 1
		Case 189: done = 1
		Case 225: done = 1
		Case 261: done = 1
		Case 297: done = 1
		Case 333: done = 1
	End Select

	if done = 1 then  
		me.enabled = 0
		if MultiScoreTimer.enabled = False Then isScoring = False
	End If
End Sub

Sub PulseReelP2_100_Timer
	dim done:done=0
	emreel100_p2.rotx = emreel100_p2.rotx + ReelStep
	if emreel100_p2.rotx > 360 then emreel100_p2.rotx = emreel100_p2.rotx - 360
	
	Select Case (emreel100_p2.rotx)
		Case 9: done = 1
		Case 45: done = 1
		Case 81: done = 1
		Case 117: done = 1
		Case 153: done = 1
		Case 189: done = 1
		Case 225: done = 1
		Case 261: done = 1
		Case 297: done = 1
		Case 333: done = 1
	End Select

	if done = 1 then  
		me.enabled = 0
		if MultiScoreTimer.enabled = False Then isScoring = False
	End If
End Sub

Sub PulseReelP2_1000_Timer
	dim done:done=0
	emreel1000_p2.rotx = emreel1000_p2.rotx + ReelStep
	if emreel1000_p2.rotx > 360 then emreel1000_p2.rotx = emreel1000_p2.rotx - 360
	
	Select Case (emreel1000_p2.rotx)
		Case 9: done = 1
		Case 45: done = 1
		Case 81: done = 1
		Case 117: done = 1
		Case 153: done = 1
		Case 189: done = 1
		Case 225: done = 1
		Case 261: done = 1
		Case 297: done = 1
		Case 333: done = 1
	End Select

	if done = 1 then  
		me.enabled = 0
		if MultiScoreTimer.enabled = False Then isScoring = False
	End If
End Sub

Sub PulseReelP2_10000_Timer
	dim done:done=0
	emreel10000_p2.rotx = emreel10000_p2.rotx + ReelStep
	if emreel10000_p2.rotx > 360 then emreel10000_p2.rotx = emreel10000_p2.rotx - 360
	
	Select Case (emreel10000_p2.rotx)
		Case 9: done = 1
		Case 45: done = 1
		Case 81: done = 1
		Case 117: done = 1
		Case 153: done = 1
		Case 189: done = 1
		Case 225: done = 1
		Case 261: done = 1
		Case 297: done = 1
		Case 333: done = 1
	End Select

	if done = 1 then  
		me.enabled = 0
		if MultiScoreTimer.enabled = False Then isScoring = False
	End If
End Sub


Dim CreditDir

Sub PulseCreditReel_Timer
	dim done:done=0

	emcredits1.rotx = emcredits1.rotx - ReelStep*CreditDir
	if emcredits1.rotx > 360 then emcredits1.rotx = emcredits1.rotx - 360
	if emcredits1.rotx < 0 then emcredits1.rotx = emcredits1.rotx + 360
	
	If CreditDir = 1 and emcredits1.rotx < 333 and emcredits1.rotx >= 297 then
		emcredits10.rotx = emcredits10.rotx - ReelStep*CreditDir
	Elseif CreditDir = -1 and emcredits1.rotx <= 333 and emcredits1.rotx > 297 then
		emcredits10.rotx = emcredits10.rotx - ReelStep*CreditDir
	End If

	Select Case (emcredits1.rotx)
		Case 9: done = 1
		Case 45: done = 1
		Case 81: done = 1
		Case 117: done = 1
		Case 153: done = 1
		Case 189: done = 1
		Case 225: done = 1
		Case 261: done = 1
		Case 297: done = 1
		Case 333: done = 1
	End Select

	if done = 1 then  
		me.enabled = 0
	End If
End Sub

Sub ResetReels()
	If emreel1_p1.rotx <> 297 then PulseReelP1_1.enabled = true:PlaySoundAtVol "reelclick", emreel1_p1, ReelClickVol
	If emreel10_p1.rotx <> 297 then PulseReelP1_10.enabled = true:PlaySoundAtVol "reelclick", emreel10_p1, ReelClickVol
	If emreel100_p1.rotx <> 297 then PulseReelP1_100.enabled = true:PlaySoundAtVol "reelclick", emreel100_p1, ReelClickVol
	If emreel1000_p1.rotx <> 297 then PulseReelP1_1000.enabled = true:PlaySoundAtVol "reelclick", emreel1000_p1, ReelClickVol
	If emreel10000_p1.rotx <> 297 then PulseReelP1_10000.enabled = true:PlaySoundAtVol "reelclick", emreel10000_p1, ReelClickVol

	If emreel1_p2.rotx <> 297 then PulseReelP2_1.enabled = true:PlaySoundAtVol "reelclick", emreel1_p2, ReelClickVol
	If emreel10_p2.rotx <> 297 then PulseReelP2_10.enabled = true:PlaySoundAtVol "reelclick", emreel10_p2, ReelClickVol
	If emreel100_p2.rotx <> 297 then PulseReelP2_100.enabled = true:PlaySoundAtVol "reelclick", emreel100_p2, ReelClickVol
	If emreel1000_p2.rotx <> 297 then PulseReelP2_1000.enabled = true:PlaySoundAtVol "reelclick", emreel1000_p2, ReelClickVol
	If emreel10000_p2.rotx <> 297 then PulseReelP2_10000.enabled = true:PlaySoundAtVol "reelclick", emreel10000_p2, ReelClickVol
End Sub

Sub SetReels()
	dim digangles(10)
	digangles(0) = 297
	digangles(1) = 333
	digangles(2) = 9
	digangles(3) = 45
	digangles(4) = 81
	digangles(5) = 117
	digangles(6) = 153
	digangles(7) = 189
	digangles(8) = 225
	digangles(9) = 261

	If Score(1) > 0 Then emreel1_p1.rotx = digangles(Int(Mid(StrReverse(score(1)),1,1))):ReelP1Value1=Mid(StrReverse(score(1)),1,1)
	If Score(1) > 9 Then emreel10_p1.rotx = digangles(Int(Mid(StrReverse(score(1)),2,1)))::ReelP1Value10=Mid(StrReverse(score(1)),2,1)
	If Score(1) > 99 Then emreel100_p1.rotx = digangles(Int(Mid(StrReverse(score(1)),3,1))):ReelP1Value100=Mid(StrReverse(score(1)),3,1)
	If Score(1) > 999 Then emreel1000_p1.rotx = digangles(Int(Mid(StrReverse(score(1)),4,1))):ReelP1Value1000=Mid(StrReverse(score(1)),4,1)
	If Score(1) > 9999 Then emreel10000_p1.rotx = digangles(Int(Mid(StrReverse(score(1)),5,1))):ReelP1Value10000=Mid(StrReverse(score(1)),5,1)

	If Score(2) > 0 Then emreel1_p2.rotx = digangles(Int(Mid(StrReverse(score(2)),1,1))):ReelP2Value1=Mid(StrReverse(score(2)),1,1)
	If Score(2) > 9 Then emreel10_p2.rotx = digangles(Int(Mid(StrReverse(score(2)),2,1)))::ReelP2Value10=Mid(StrReverse(score(2)),2,1)
	If Score(2) > 99 Then emreel100_p2.rotx = digangles(Int(Mid(StrReverse(score(2)),3,1))):ReelP2Value100=Mid(StrReverse(score(2)),3,1)
	If Score(2) > 999 Then emreel1000_p2.rotx = digangles(Int(Mid(StrReverse(score(2)),4,1))):ReelP2Value1000=Mid(StrReverse(score(2)),4,1)
	If Score(2) > 9999 Then emreel10000_p2.rotx = digangles(Int(Mid(StrReverse(score(2)),5,1))):ReelP2Value10000=Mid(StrReverse(score(2)),5,1)

	digangles(0) = 297
	digangles(9) = 333
	digangles(8) = 9
	digangles(7) = 45
	digangles(6) = 81
	digangles(5) = 117
	digangles(4) = 153
	digangles(3) = 189
	digangles(2) = 225
	digangles(1) = 261

	If Credits > 9 Then emcredits10.rotx = digangles(Int(Mid(StrReverse(Credits),2,1)))
	emcredits1.rotx = digangles(Int(Mid(StrReverse(Credits),1,1)))

End Sub


Sub UpdateScoreboard
	if not ShowPlayfieldReel or (Players < 2) Then		
		HideScoreBoard
	Else
		P_SB_Postit.image = "Postit"
		Select case Players
			Case 2:
				P_SB_Postit.size_y = 50
				P_SB_Postit.Transy = -30
				PScore1_P.image = "Player1"
				PScore2_P.image = "Player2"
				SetScoreBoard(1)
				SetScoreBoard(2)
			Case 3:
				P_SB_Postit.size_y = 75
				P_SB_Postit.Transy = -15
				PScore1_P.image = "Player1"
				PScore2_P.image = "Player2"
				PScore3_P.image = "Player3"
				SetScoreBoard(1)
				SetScoreBoard(2)
				SetScoreBoard(3)
			Case 4:
				P_SB_Postit.size_y = 100
				P_SB_Postit.Transy = 0
				PScore1_P.image = "Player1"
				PScore2_P.image = "Player2"
				PScore3_P.image = "Player3"
				PScore4_P.image = "Player4"
				SetScoreBoard(1)
				SetScoreBoard(2)
				SetScoreBoard(3)
				SetScoreBoard(4)
		end Select
	end If
End Sub

Dim HSArray1
HSArray1 = Array("HS_0","HS_1","HS_2","HS_3","HS_4","HS_5","HS_6","HS_7","HS_8","HS_9","HS_Space","HS_Comma","Tape")

Sub HideScoreboard
	for each obj in C_ScoreBoard
		obj.image = HSArray1(10)
	Next
End Sub

Dim SBScore100k, SBScore10k, SBScoreK, SBScore100, SBScore10, SBScore1, SBTempScore

Sub SetScoreBoard(PlayerPar)
	SBTempScore = Score(PlayerPar)
	SBScore1 = 0
	SBScore10 = 0
	SBScore100 = 0
	SBScoreK = 0
	SBScore10k = 0
	SBScore100k = 0
	if len(SBTempScore) > 0 Then
		SBScore1 = cint(right(SBTempscore,1))
	end If
	if len(SBTempScore) > 1 Then
		SBTempScore = Left(SBTempScore,len(SBTempScore)-1)
		SBScore10 = cint(right(SBTempscore,1))
	end If
	if len(SBTempScore) > 1 Then
		SBTempScore = Left(SBTempScore,len(SBTempScore)-1)
		SBScore100 = cint(right(SBTempscore,1))
	end If
	if len(SBTempScore) > 1 Then
		SBTempScore = Left(SBTempScore,len(SBTempScore)-1)
		SBScoreK = cint(right(SBTempscore,1))
	end If
	if len(SBTempScore) > 1 Then
		SBTempScore = Left(SBTempScore,len(SBTempScore)-1)
		SBScore10k = cint(right(SBTempscore,1))
	end If
	if len(SBTempScore) > 1 Then
		SBTempScore = Left(SBTempScore,len(SBTempScore)-1)
		SBScore100k = cint(right(SBTempscore,1))
	end If
	Select case PlayerPar
		Case 1:
			Pscore6_1.image = HSArray1(SBScore100K):If Score(PlayerPar)<100000 Then Pscore6_1.image = HSArray1(10)
			Pscore5_1.image = HSArray1(SBScore10K):If Score(PlayerPar)<10000 Then Pscore5_1.image = HSArray1(10)
			Pscore4_1.image = HSArray1(SBScoreK):If Score(PlayerPar)<1000 Then Pscore4_1.image = HSArray1(10)
			Pscore3_1.image = HSArray1(SBScore100):If Score(PlayerPar)<100 Then Pscore3_1.image = HSArray1(10)
			Pscore2_1.image = HSArray1(SBScore10):If Score(PlayerPar)<10 Then Pscore2_1.image = HSArray1(10)
			Pscore1_1.image = HSArray1(SBScore1):If Score(PlayerPar)<1 Then Pscore1_1.image = HSArray1(10)
			if Score(PlayerPar)<1000 then
				PscoreComma_1.image = HSArray1(10)
			else
				PscoreComma_1.image = HSArray1(11)
			end if
		Case 2:
			Pscore6_2.image = HSArray1(SBScore100K):If Score(PlayerPar)<100000 Then Pscore6_2.image = HSArray1(10)
			Pscore5_2.image = HSArray1(SBScore10K):If Score(PlayerPar)<10000 Then Pscore5_2.image = HSArray1(10)
			Pscore4_2.image = HSArray1(SBScoreK):If Score(PlayerPar)<1000 Then PScore4_2.image = HSArray1(10)
			Pscore3_2.image = HSArray1(SBScore100):If Score(PlayerPar)<100 Then Pscore3_2.image = HSArray1(10)
			Pscore2_2.image = HSArray1(SBScore10):If Score(PlayerPar)<10 Then Pscore2_2.image = HSArray1(10)
			Pscore1_2.image = HSArray1(SBScore1):If Score(PlayerPar)<1 Then Pscore1_2.image = HSArray1(10)
			if Score(PlayerPar)<1000 then
				PscoreComma_2.image = HSArray1(10)
			else
				PscoreComma_2.image = HSArray1(11)
			end if
		Case 3:
			Pscore6_3.image = HSArray1(SBScore100K):If Score(PlayerPar)<100000 Then Pscore6_3.image = HSArray1(10)
			Pscore5_3.image = HSArray1(SBScore10K):If Score(PlayerPar)<10000 Then Pscore5_3.image = HSArray1(10)
			Pscore4_3.image = HSArray1(SBScoreK):If Score(PlayerPar)<1000 Then Pscore4_3.image = HSArray1(10)
			Pscore3_3.image = HSArray1(SBScore100):If Score(PlayerPar)<100 Then Pscore3_3.image = HSArray1(10)
			Pscore2_3.image = HSArray1(SBScore10):If Score(PlayerPar)<10 Then Pscore2_3.image = HSArray1(10)
			Pscore1_3.image = HSArray1(SBScore1):If Score(PlayerPar)<1 Then Pscore1_3.image = HSArray1(10)
			if Score(PlayerPar)<1000 then
				PscoreComma_3.image = HSArray1(10)
			else
				PscoreComma_3.image = HSArray1(11)
			end if
		Case 4:
			Pscore6_4.image = HSArray1(SBScore100K):If Score(PlayerPar)<100000 Then Pscore6_4.image = HSArray1(10)
			Pscore5_4.image = HSArray1(SBScore10K):If Score(PlayerPar)<10000 Then Pscore5_4.image = HSArray1(10)
			Pscore4_4.image = HSArray1(SBScoreK):If Score(PlayerPar)<1000 Then Pscore4_4.image = HSArray1(10)
			Pscore3_4.image = HSArray1(SBScore100):If Score(PlayerPar)<100 Then Pscore3_4.image = HSArray1(10)
			Pscore2_4.image = HSArray1(SBScore10):If Score(PlayerPar)<10 Then Pscore2_4.image = HSArray1(10)
			Pscore1_4.image = HSArray1(SBScore1):If Score(PlayerPar)<1 Then Pscore1_4.image = HSArray1(10)
			if Score(PlayerPar)<1000 then
				PscoreComma_4.image = HSArray1(10)
			else
				PscoreComma_4.image = HSArray1(11)
			end if
	End Select
End Sub

Sub SetScoreReel
	Dim TempScore, Score10, Score100, Score1000, Score10000, Score100000 
	TempScore = Score(Player)
	Score10 = 0
	Score100 = 0
	Score1000 = 0
	Score10000 = 0
	Score100000 = 0
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		Score10 = cstr(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		Score100 = cstr(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		Score1000 = cstr(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		Score10000 = cstr(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		Score100000 = cstr(right(Tempscore,1))
	end If
	P_Reel1.image = cstr(Score10)
	P_Reel2.image = cstr(Score100)
	P_Reel3.image = cstr(Score1000)
	P_Reel4.image = cstr(Score10000)
	P_Reel5.image = cstr(Score100000)
End Sub

'*****KICKER STUFF*****

Sub KickerBall8_Hit:
	SoundSaucerLock
	KickerBall8.TIMERINTERVAL = 1000
	KickerHold.ENABLED = TRUE
End Sub

Sub KickerHold_timer
	If IsScoring then
		exit sub
	end if
	KickerHold.enabled=false
	KickerBall8.TIMERENABLED = TRUE
	if TableTilted=false then
		MultiScore 5, 100
	end if
end sub

Sub KickerBall8_Timer()
	KickerBall8.TIMERENABLED = FALSE
	KickerBall8.Kick 159+(rnd*2),10+rnd,0.7
	SoundSaucerKick 1, KickerBall8	
	P_Kicker.rotx = -85
	Kickertimer.enabled = False
	Kickertimer.enabled = True
	DOF 127, 2
	if TableTilted=false then
		Light8.State=1
		Top8.State=0
		CheckAllStripes
		CheckAllSolids
		opengate
	end if
End Sub

Sub KickerTimer_Timer
	P_Kicker.rotx = P_Kicker.rotx + 5
	If P_Kicker.rotx >= -10 then
		P_Kicker.rotx = -10
		Kickertimer.enabled = False
	end if	
End Sub

'********************
'*****  Chimes  *****

Sub PlayChime(x)
	Select Case x
		Case 10
			If DisableDOFChimes = 0 Then DOF 153, DOFPULSE
			If LastChime10=1 Then
				PlaySoundAt SoundFX("SJ_Chime_10a",DOFChimes), ChimeSounds
				LastChime10=0
			Else
				PlaySoundAt SoundFX("SJ_Chime_10b",DOFChimes), ChimeSounds
				LastChime10=1
			End If
		Case 100
			If DisableDOFChimes = 0 Then DOF 154, DOFPULSE
			If LastChime100=1 Then
				PlaySoundAt SoundFX("SJ_Chime_100a",DOFChimes), ChimeSounds
				LastChime100=0
			Else
				PlaySoundAt SoundFX("SJ_Chime_100b",DOFChimes), ChimeSounds
				LastChime100=1
			End If
		Case 1000
			If DisableDOFChimes = 0 Then DOF 155, DOFPULSE
			If LastChime1000=1 Then
				PlaySoundAt SoundFX("SJ_Chime_1000a",DOFChimes), ChimeSounds
				LastChime1000=0
			Else
				PlaySoundAt SoundFX("SJ_Chime_1000b",DOFChimes), ChimeSounds
				LastChime1000=1
			End If
	End Select
End Sub


sub closegate()
	If dooralreadyopen = 1 Then
		DiverterFlipper.rotatetostart
		playsoundat "soloff", DiverterFlipper
		SLLR.collidable = 1
		SLRR.collidable = 1
	End If
	dooralreadyopen=0
end sub  

sub opengate()
	If dooralreadyopen = 0 Then
		DiverterFlipper.rotatetoend
		SLLR.collidable = 0
		SLRR.collidable = 0
		playsoundAt "soloff", DiverterFlipper
	End If
	dooralreadyopen=1
end sub  

Sub ToggleAlternatingRelay
	If AlternatingRelay=0 then
		AlternatingRelay=1
	else
		AlternatingRelay=0
	end if
	togglestate = 1
end sub

'******************************************************
' 							MATCH
'******************************************************

Dim MatchStep

Sub CheckMatch()
	MatchStep=0
	MatchTimer.enabled = 1
End Sub

Sub MatchTimer_Timer()
	If MatchStep < Players Then
		MatchStep = MatchStep + 1
		if Match=(Score(MatchStep) mod 100) then
			FreeGame
		end if
	Else 
		me.enabled = false
	End If
End Sub

Sub AdvMatch()
	Select Case (Match)
		Case 30: Match = 80
		Case 80: Match = 20
		Case 20: Match = 50
		Case 50: Match = 90
		Case 90: Match = 40
		Case 40: Match = 0
		Case 0: Match = 60
		Case 60: Match = 10
		Case 10: Match = 70
		Case 70: Match = 30
	End Select
End Sub

'******************************************************
' 							TILT
'******************************************************

Dim TiltSens

Sub CheckTilt
	If Tilttimer.Enabled = True Then 
	 TiltSens = TiltSens + 1
	 if TiltSens = 3 Then GameTilted
	Else
	 TiltSens = 1
	 Tilttimer.Enabled = True
	End If
End Sub

Sub Tilttimer_Timer()
	If TiltSens > 0 Then TiltSens = TiltSens - 1
	If TiltSens = 0 Then Tilttimer.Enabled = False
End Sub

Sub GameTilted
	TableTilted = 1
	toggletilt = 1

	GIOFF

	Bumper1.threshold = 100
	LeftSlingShot.SlingShotThreshold = 100
	RightSlingShot.SlingShotThreshold = 100

	PlayLoudClick2

	LeftFlipper.RotateToStart
	StopSound "buzzL"
	DOF 101, 0

	RightFlipper.RotateToStart
	StopSound "buzz"
	DOF 102, 0
End Sub

Sub ResetTilt
	TableTilted = 0
	toggletilt = 1

	GION

	PlayLoudClick2

	Bumper1.threshold = 1.6
	LeftSlingShot.SlingShotThreshold = 0.5
	RightSlingShot.SlingShotThreshold = 0.5
End Sub

'******************************************************
' 						GI
'******************************************************

Sub GION()
	For each obj in GI
		obj.state=1
	next
	backbox.image = "backbox"
	BGGI.visible = True

	playfield_off.visible=0
	For each obj in layer1col: obj.image = "layer1": Next
	For each obj in layer2col: obj.image = "layer2": Next
	For each obj in layer3col: obj.image = "layer3": Next
	For each obj in layer4col: obj.image = "layer4": Next
	metalback.image = "metalback"
	'rail_l.image = "lrail"
	'rail_r.image = "rrail"
	bumperbase.image = "bumper"
	bumpercap.image = "bumpercap"
	wirearm.image = "bakery"
	brackets.image = "bakery"

	bistate = 1
	togglestate = 1
	togglebonus = 1
	toggledrops = 1
End Sub

Sub GIOFF()
	For each obj in GI
		obj.state=0
	next

	playfield_off.visible=1
	For each obj in layer1col: obj.image = "layer1off": Next
	For each obj in layer2col: obj.image = "layer2off": Next
	For each obj in layer3col: obj.image = "layer3off": Next
	For each obj in layer4col: obj.image = "layer4off": Next	
	metalback.image = "metalback_off"
	'rail_l.image = "lrail_off"
	'rail_r.image = "rrail_off"
	bumperbase.image = "bumper_off"
	bumpercap.image = "bumpercap_off"
	wirearm.image = "bakery_off"
	brackets.image = "bakery_off"

	For each obj in DropFlashers: obj.visible = 0: Next

	LightsOut
End Sub

Sub LightsOut
	TargetSpecialLit = 0

	bistate = 0
	togglestate = 1

	Light1.State=0
	Light2.State=0
	Light3.State=0
	Light4.State=0
	Light5.State=0
	Light6.State=0
	Light7.State=0
	Light8.State=0
	Light9.State=0
	Light10.State=0
	Light11.State=0
	Light12.State=0
	Light13.State=0
	Light14.State=0
	Light15.State=0

	Top8.State=0	
	'Bottom8.State=0

	BL3.state=0
	BL2.state=0
	BL1.state=0
end sub

'******************************************************
' 					LOAD & SAVE TABLE
'******************************************************
sub savehs
	savevalue "BigShot_VPX", "Credits", Credits
	savevalue "BigShot_VPX", "Match", Match
	savevalue "BigShot_VPX", "HighScore", Highscore
	savevalue "BigShot_VPX", "HSA1", HSA1
	savevalue "BigShot_VPX", "HSA2", HSA2
	savevalue "BigShot_VPX", "HSA3", HSA3
	savevalue "BigShot_VPX", "Score", Score(1)
	savevalue "BigShot_VPX", "Score2", Score(2)
	savevalue "BigShot_VPX", "Multiplier", BonusMultiplier
	savevalue "BigShot_VPX", "B1", Light1.state
	savevalue "BigShot_VPX", "B2", Light2.state
	savevalue "BigShot_VPX", "B3", Light3.state
	savevalue "BigShot_VPX", "B4", Light4.state
	savevalue "BigShot_VPX", "B5", Light5.state
	savevalue "BigShot_VPX", "B6", Light6.state
	savevalue "BigShot_VPX", "B7", Light7.state
	savevalue "BigShot_VPX", "B8", Light8.state
	savevalue "BigShot_VPX", "B9", Light9.state
	savevalue "BigShot_VPX", "B10", Light10.state
	savevalue "BigShot_VPX", "B11", Light11.state
	savevalue "BigShot_VPX", "B12", Light12.state
	savevalue "BigShot_VPX", "B13", Light13.state
	savevalue "BigShot_VPX", "B14", Light14.state
	savevalue "BigShot_VPX", "B15", Light15.state	
	savevalue "BigShot_VPX", "AR", AlternatingRelay
end sub

sub loadhs
	HighScore=0
	Credits=0
	Match=0

	dim temp
	temp = LoadValue("BigShot_VPX", "Credits")

	temp = LoadValue("BigShot_VPX", "Credits")
	If (temp <> "") then Credits = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "Match")
	If (temp <> "") then Match = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "HighScore")
	If (temp <> "") then HighScore = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "hsa1")
	If (temp <> "") then HSA1 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "hsa2")
	If (temp <> "") then HSA2 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "hsa3")
	If (temp <> "") then HSA3 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "Score")
	If (temp <> "") then Score(1) = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "Score2")
	If (temp <> "") then Score(2) = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "Multiplier")
	If (temp <> "") then BonusMultiplier = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B1")
	If (temp <> "") then B1 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B2")
	If (temp <> "") then B2 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B3")
	If (temp <> "") then B3 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B4")
	If (temp <> "") then B4 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B5")
	If (temp <> "") then B5 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B6")
	If (temp <> "") then B6 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B7")
	If (temp <> "") then B7 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B8")
	If (temp <> "") then B8 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B9")
	If (temp <> "") then B9 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B10")
	If (temp <> "") then B10 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B11")
	If (temp <> "") then B11 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B12")
	If (temp <> "") then B12 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B13")
	If (temp <> "") then B13 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B14")
	If (temp <> "") then B14 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "B15")
	If (temp <> "") then B15 = CDbl(temp)

	temp = LoadValue("BigShot_VPX", "AR")
	If (temp <> "") then AlternatingRelay = CDbl(temp)

	if HighScore=0 then HighScore=35000

	SetReels
	Player = 1
	SetScoreReel
end sub

'==========================================================================================================================================
'============================================================= START OF HIGH SCORES ROUTINES =============================================================
'==========================================================================================================================================
'
'ADD LINE TO TABLE_KEYDOWN SUB WITH THE FOLLOWING:    If HSEnterMode Then HighScoreProcessKey(keycode) AFTER THE STARTGAME ENTRY
'ADD: And Not HSEnterMode=true TO IF KEYCODE=STARTGAMEKEY
'TO SHOW THE SCORE ON POST-IT ADD LINE AT RELEVENT LOCATION THAT HAS:  UpdatePostIt
'TO INITIATE ADDING INITIALS ADD LINE AT RELEVENT LOCATION THAT HAS:  HighScoreEntryInit()
'ADD THE FOLLOWING LINES TO TABLE_INIT TO SETUP POSTIT
'	if HSA1="" then HSA1=25
'	if HSA2="" then HSA2=25
'	if HSA3="" then HSA3=25
'	UpdatePostIt
'ADD HSA1, HSA2 AND HSA3 TO SAVE AND LOAD VALUES FOR TABLE
'ADD A TIMER NAMED HighScoreFlashTimer WITH INTERVAL 100 TO TABLE
'SET HSSSCOREX BELOW TO WHATEVER VARIABLE YOU USE FOR HIGH SCORE.
'ADD OBJECTS TO PLAYFIELD (EASIEST TO JUST COPY FROM THIS TABLE)
'IMPORT POST-IT IMAGES


Dim HSA1, HSA2, HSA3
Dim HSEnterMode, hsLetterFlash, hsEnteredDigits(3), hsCurrentDigit, hsCurrentLetter
Dim HSArray  
Dim HSScoreM,HSScore100k, HSScore10k, HSScoreK, HSScore100, HSScore10, HSScore1, HSScorex	'Define 6 different score values for each reel to use
HSArray = Array("Postit0","postit1","postit2","postit3","postit4","postit5","postit6","postit7","postit8","postit9","postitBL","postitCM","Tape")
Const hsFlashDelay = 4

' ***********************************************************
'  HiScore DISPLAY 
' ***********************************************************

Sub UpdatePostIt
	dim tempscore
	HSScorex = HighScore
	TempScore = HSScorex
	HSScore1 = 0
	HSScore10 = 0
	HSScore100 = 0
	HSScoreK = 0
	HSScore10k = 0
	HSScore100k = 0
	HSScoreM = 0
	if len(TempScore) > 0 Then
		HSScore1 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore10 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore100 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScoreK = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore10k = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore100k = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScoreM = cint(right(Tempscore,1))
	end If
	Pscore6.image = HSArray(HSScoreM):If HSScorex<1000000 Then PScore6.image = HSArray(10)
	Pscore5.image = HSArray(HSScore100K):If HSScorex<100000 Then PScore5.image = HSArray(10)
	PScore4.image = HSArray(HSScore10K):If HSScorex<10000 Then PScore4.image = HSArray(10)
	PScore3.image = HSArray(HSScoreK):If HSScorex<1000 Then PScore3.image = HSArray(10)
	PScore2.image = HSArray(HSScore100):If HSScorex<100 Then PScore2.image = HSArray(10)
	PScore1.image = HSArray(HSScore10):If HSScorex<10 Then PScore1.image = HSArray(10)
	PScore0.image = HSArray(HSScore1):If HSScorex<1 Then PScore0.image = HSArray(10)
	if HSScorex<1000 then
		PComma.image = HSArray(10)
	else
		PComma.image = HSArray(11)
	end if
	if HSScorex<1000000 then
		PComma2.image = HSArray(10)
	else
		PComma2.image = HSArray(11)
	end if
'	if showhisc=1 and showhiscnames=1 then
'		for each objekt in hiscname:objekt.visible=1:next
		HSName1.image = ImgFromCode(HSA1, 1)
		HSName2.image = ImgFromCode(HSA2, 2)
		HSName3.image = ImgFromCode(HSA3, 3)
'	  else
'		for each objekt in hiscname:objekt.visible=0:next
'	end if
End Sub

Function ImgFromCode(code, digit)
	Dim Image
	if (HighScoreFlashTimer.Enabled = True and hsLetterFlash = 1 and digit = hsCurrentLetter) then
		Image = "postitBL"
	elseif (code + ASC("A") - 1) >= ASC("A") and (code + ASC("A") - 1) <= ASC("Z") then
		Image = "postit" & chr(code + ASC("A") - 1)
	elseif code = 27 Then
		Image = "PostitLT"
    elseif code = 0 Then
		image = "PostitSP"
    Else
      msgbox("Unknown display code: " & code)
	end if
	ImgFromCode = Image
End Function

Sub HighScoreEntryInit()
	HSA1=0:HSA2=0:HSA3=0
	HSEnterMode = True
	hsCurrentDigit = 0
	hsCurrentLetter = 1:HSA1=1
	HighScoreFlashTimer.Interval = 250
	HighScoreFlashTimer.Enabled = True
	hsLetterFlash = hsFlashDelay
End Sub

Sub HighScoreFlashTimer_Timer()
	hsLetterFlash = hsLetterFlash-1
	UpdatePostIt
	If hsLetterFlash=0 then 'switch back
		hsLetterFlash = hsFlashDelay
	end if
End Sub

' ***********************************************************
'  HiScore ENTER INITIALS 
' ***********************************************************

Sub HighScoreProcessKey(keycode)
    If keycode = LeftFlipperKey Then
		hsLetterFlash = hsFlashDelay
		Select Case hsCurrentLetter
			Case 1:
				HSA1=HSA1-1:If HSA1=-1 Then HSA1=26 'no backspace on 1st digit
				UpdatePostIt
			Case 2:
				HSA2=HSA2-1:If HSA2=-1 Then HSA2=27
				UpdatePostIt
			Case 3:
				HSA3=HSA3-1:If HSA3=-1 Then HSA3=27
				UpdatePostIt
		 End Select
    End If

	If keycode = RightFlipperKey Then
		hsLetterFlash = hsFlashDelay
		Select Case hsCurrentLetter
			Case 1:
				HSA1=HSA1+1:If HSA1>26 Then HSA1=0
				UpdatePostIt
			Case 2:
				HSA2=HSA2+1:If HSA2>27 Then HSA2=0
				UpdatePostIt
			Case 3:
				HSA3=HSA3+1:If HSA3>27 Then HSA3=0
				UpdatePostIt
		 End Select
	End If
	
    If keycode = StartGameKey Then
		Select Case hsCurrentLetter
			Case 1:
				hsCurrentLetter=2 'ok to advance
				HSA2=HSA1 'start at same alphabet spot
'				EMReelHSName1.SetValue HSA1:EMReelHSName2.SetValue HSA2
			Case 2:
				If HSA2=27 Then 'bksp
					HSA2=0
					hsCurrentLetter=1
				Else
					hsCurrentLetter=3 'enter it
					HSA3=HSA2 'start at same alphabet spot
				End If
			Case 3:
				If HSA3=27 Then 'bksp
					HSA3=0
					hsCurrentLetter=2
				Else
					savehs 'enter it
					HighScoreFlashTimer.Enabled = False
					HSEnterMode = False
				End If
		End Select
		UpdatePostIt
    End If
End Sub


'******************************************

Sub Gate1_hit:SoundHeavyGate: End Sub

' *********************************************************************
'                      FLIPPERS AND RUBBER DAMPENERS
' *********************************************************************

'******************************************************
'                        FLIPPER TRICKS
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
        Dim BOT, b

        If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
                EOSNudge1 = 1
                
                If Flipper2.currentangle = EndAngle2 Then 
                        BOT = GetBalls
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
                                         exit Sub
                                end If
                        Next
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
                                        BOT(b).velx = BOT(b).velx / 1.1
                                        BOT(b).vely = BOT(b).vely - 0.3
                                end If
                        Next
                End If
        Else 
                If Flipper1.currentangle <> EndAngle1 then 
                        EOSNudge1 = 0
                end if
        End If
End Sub

'*****************
' Maths
'*****************
Const PI = 3.1415927

'*************************************************
' Check ball distance from Flipper for Rem
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

' Used for drop targets and stand up targets
Function Atn2(dy, dx)
        dim pi
        pi = 4*Atn(1)

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
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 1.4 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.055

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
                Dim BOT, b
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
Dim LiveDistanceMax: LiveDistanceMax = leftflipper.length  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

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
		If Flipper.currentangle = Flipper.endangle Then RubbersD.Dampen Activeball
	End If
End Sub

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

'******************************************************
'				FLIPPER AND RUBBER CORRECTION
'******************************************************

'================================
'Helper Functions

Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp if on the boundry lines
	'if L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'if L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function


'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub

dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.11 '0.97'	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.99 '0.97 '0.935 '0.96
RubbersD.addpoint 2, 5.76, 0.942 '0.97 '0.942 '0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.75 '0.85


Sub dDropTargetsL_Hit(idx) 
	DropsL.DTHit Activeball
End Sub

Sub dDropTargetsR_Hit(idx) 
	DropsR.DTHit Activeball
End Sub

dim DropsL: Set DropsL = new DTPhysics	'this is just rubber but negative 85%...
DropsL.angle  = DropBall1.Orientation
DropsL.mass = 0.1

dim DropsR: Set DropsR = new DTPhysics	'this is just rubber but negative 85%...
DropsR.angle  = DropBall15.Orientation
DropsR.mass = 0.1

Class DTPhysics
	public angle, mass

	public sub DTHit(aBall)
		dim DesiredCOR, rangle,bangle,calc1, calc2, calc3
		rangle = (angle - 90) * 3.1416 / 180
		bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	
		calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
		calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
		calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

		aBall.velx = calc1 * cos(rangle) + calc2
		aBall.vely = calc1 * sin(rangle) + calc3
	End Sub
End Class

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
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
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub

	Public Sub Report() 	'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class

'Tracks ball velocity for judging bounce calculations & angle
'apologies to JimmyFingers is this is what his script does. I know his tracks ball velocity too but idk how it works in particular
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


'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

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
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
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
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]

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

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
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


' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / tableheight-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1
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

Function RndNum(min, max)
    RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySoundAtLevelStatic ("Start_Button"), StartButtonSoundLevel, Drain
End Sub

Sub SoundNudgeLeft()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
	End Select
End Sub

Sub SoundNudgeRight()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
	End Select
End Sub

Sub SoundNudgeCenter()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic ("Nudge_1"), NudgeCenterSoundLevel * VolumeDial, Drain
		Case 2 : PlaySoundAtLevelStatic ("Nudge_2"), NudgeCenterSoundLevel * VolumeDial, Drain
		Case 3 : PlaySoundAtLevelStatic ("Nudge_3"), NudgeCenterSoundLevel * VolumeDial, Drain
	End Select
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
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic ("Drain_1"), DrainSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic ("Drain_2"), DrainSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic ("Drain_3"), DrainSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic ("Drain_4"), DrainSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic ("Drain_5"), DrainSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic ("Drain_6"), DrainSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic ("Drain_7"), DrainSoundLevel, drainswitch
		Case 8 : PlaySoundAtLevelStatic ("Drain_8"), DrainSoundLevel, drainswitch
		Case 9 : PlaySoundAtLevelStatic ("Drain_9"), DrainSoundLevel, drainswitch
		Case 10 : PlaySoundAtLevelStatic ("Drain_10"), DrainSoundLevel, drainswitch
		Case 11 : PlaySoundAtLevelStatic ("Drain_11"), DrainSoundLevel, drainswitch
	End Select
End Sub


'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft()
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_L1",DOFContactors), SlingshotSoundLevel, Sling2
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_L2",DOFContactors), SlingshotSoundLevel, Sling2
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_L3",DOFContactors), SlingshotSoundLevel, Sling2
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_L4",DOFContactors), SlingshotSoundLevel, Sling2
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_L5",DOFContactors), SlingshotSoundLevel, Sling2
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_L6",DOFContactors), SlingshotSoundLevel, Sling2
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_L7",DOFContactors), SlingshotSoundLevel, Sling2
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_L8",DOFContactors), SlingshotSoundLevel, Sling2
		Case 9 : PlaySoundAtLevelStatic SoundFX("Sling_L9",DOFContactors), SlingshotSoundLevel, Sling2
		Case 10 : PlaySoundAtLevelStatic SoundFX("Sling_L10",DOFContactors), SlingshotSoundLevel, Sling2
	End Select
End Sub

Sub RandomSoundSlingshotRight()
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_R1",DOFContactors), SlingshotSoundLevel, Sling1
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_R2",DOFContactors), SlingshotSoundLevel, Sling1
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_R3",DOFContactors), SlingshotSoundLevel, Sling1
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_R4",DOFContactors), SlingshotSoundLevel, Sling1
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_R5",DOFContactors), SlingshotSoundLevel, Sling1
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_R6",DOFContactors), SlingshotSoundLevel, Sling1
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_R7",DOFContactors), SlingshotSoundLevel, Sling1
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_R8",DOFContactors), SlingshotSoundLevel, Sling1
	End Select
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperA()
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper1
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper1
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper1
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper1
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper1
	End Select
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
		PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_L01",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_L02",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_L07",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_L08",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_L09",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_L10",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_L12",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_L14",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_L18",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_L20",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_L26",DOFFlippers), FlipperLeftHitParm, Flipper
	End Select
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_R01",DOFFlippers), FlipperRightHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_R02",DOFFlippers), FlipperRightHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_R03",DOFFlippers), FlipperRightHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_R04",DOFFlippers), FlipperRightHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_R05",DOFFlippers), FlipperRightHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_R06",DOFFlippers), FlipperRightHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_R07",DOFFlippers), FlipperRightHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_R08",DOFFlippers), FlipperRightHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_R09",DOFFlippers), FlipperRightHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_R10",DOFFlippers), FlipperRightHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_R11",DOFFlippers), FlipperRightHitParm, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpRight(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_8",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
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
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_1"), parm  * RubberFlipperSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_2"), parm  * RubberFlipperSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_3"), parm  * RubberFlipperSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_4"), parm  * RubberFlipperSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_5"), parm  * RubberFlipperSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_6"), parm  * RubberFlipperSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_7"), parm  * RubberFlipperSoundFactor
	End Select
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rollover_1"), RolloverSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("Rollover_2"), RolloverSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("Rollover_3"), RolloverSoundLevel
		Case 4 : PlaySoundAtLevelActiveBall ("Rollover_4"), RolloverSoundLevel
	End Select
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

Sub RubberWheel_Hit(idx)
 	'RandomSoundRubberWeakWheel
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
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_1"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_2"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_5"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_6"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_7"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_8"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_9"), Vol(ActiveBall) * RubberWeakSoundFactor
	End Select
End Sub

Sub RandomSoundRubberWeakWheel()
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_1"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_2"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_5"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_6"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_7"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_8"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_9"), Vol(ActiveBall) * RubberWeakSoundFactor/10
	End Select
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then
 		RandomSoundRubberStrong 1 
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
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
	Select Case Int(Rnd*13)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Metal_Touch_1"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Metal_Touch_2"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Metal_Touch_3"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Metal_Touch_4"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Metal_Touch_5"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Metal_Touch_6"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Metal_Touch_7"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Metal_Touch_8"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Metal_Touch_9"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall ("Metal_Touch_10"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 11 : PlaySoundAtLevelActiveBall ("Metal_Touch_11"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 12 : PlaySoundAtLevelActiveBall ("Metal_Touch_12"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 13 : PlaySoundAtLevelActiveBall ("Metal_Touch_13"), Vol(ActiveBall) * MetalImpactSoundFactor
	End Select
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub DiverterFlipper_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_2"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
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

Sub Apron_Hit (idx)
	RandomSoundBottomArchBallGuide
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
 		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Medium_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
 		Select Case Int(Rnd*7)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Soft_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Soft_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Soft_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 4 : PlaySoundAtLevelActiveBall ("Apron_Soft_4"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 5 : PlaySoundAtLevelActiveBall ("Apron_Soft_5"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 6 : PlaySoundAtLevelActiveBall ("Apron_Soft_6"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 7 : PlaySoundAtLevelActiveBall ("Apron_Soft_7"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_5",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_6",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_7",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_8",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor		
	End Select
End Sub

Sub RandomSoundTargetHitWeak()
	Select Case Int(Rnd*4)+1		
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_1",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_2",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_3",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_4",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
	End Select
End Sub

Sub PlayTargetSound()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
 		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft()
	Else 
 		RandomSoundTargetHitWeak()
 	End If	
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound	
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft()
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(BSBall) * BallBouncePlayfieldSoftFactor, BSBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(BSBall) * BallBouncePlayfieldSoftFactor * 0.5, BSBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(BSBall) * BallBouncePlayfieldSoftFactor * 0.8, BSBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(BSBall) * BallBouncePlayfieldSoftFactor * 0.5, BSBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(BSBall) * BallBouncePlayfieldSoftFactor, BSBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(BSBall) * BallBouncePlayfieldSoftFactor * 0.2, BSBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(BSBall) * BallBouncePlayfieldSoftFactor * 0.2, BSBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(BSBall) * BallBouncePlayfieldSoftFactor * 0.2, BSBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(BSBall) * BallBouncePlayfieldSoftFactor * 0.3, BSBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard()
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(BSBall) * BallBouncePlayfieldHardFactor, BSBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(BSBall) * BallBouncePlayfieldHardFactor, BSBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_3"), volz(BSBall) * BallBouncePlayfieldHardFactor, BSBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_4"), volz(BSBall) * BallBouncePlayfieldHardFactor, BSBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(BSBall) * BallBouncePlayfieldHardFactor, BSBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_6"), volz(BSBall) * BallBouncePlayfieldHardFactor, BSBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(BSBall) * BallBouncePlayfieldHardFactor, BSBall
	End Select
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield()
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, BSBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, BSBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, BSBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, BSBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, BSBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	Select Case Int(Rnd*2)+1				
		Case 1 : PlaySoundAtLevelStatic ("Gate_FastTrigger_1"), GateSoundLevel, Activeball
		Case 2 : PlaySoundAtLevelStatic ("Gate_FastTrigger_2"), GateSoundLevel, Activeball
	End Select
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub


'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_L1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_L2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_L3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_L4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
End Sub

Sub RandomSoundRightArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_R1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_R2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_R3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_R4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.vely < -7 Then
		RandomSoundRightArch
	End If
	'debug.print activeball.vely
End Sub

Sub Arch2_hit()
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.vely < -7 Then
		RandomSoundLeftArch
	End If
	'debug.print activeball.vely
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	Select Case Int(Rnd*2)+1	
		Case 1: PlaySoundAtLevelStatic ("Saucer_Enter_1"), SaucerLockSoundLevel, Activeball
		Case 2: PlaySoundAtLevelStatic ("Saucer_Enter_2"), SaucerLockSoundLevel, Activeball
	End Select
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub


'******************************************************
'						VR ROOM
'******************************************************


' Below is the code needed to run the plungers.  Both analog and button press...*****************
'************************************************************************************************

TimerVRPlunger2.enabled = true   '  This sits outside of a sub, and tells the timer2 to be enabled at table load

Sub TimerVRPlunger_Timer
if VRPlunger.Y < 2257 then VRPlunger.Y = VRPlunger.y +4  'If the plunger is not fully extend it, then extend it by 5 coordinates in the Y, 
End Sub

Sub TimerVRPlunger2_Timer
VRPlunger.Y = 2145 + (5* Plunger.Position) -20  ' This follows our dummy plunger position for analog plunger hardware users.
end sub




sub PoolLightTimer2_timer()
	VR_PoolTable.Image = "zVR_PoolNew_l"   'change pool table image to the Lit one...
	VR_PoolLight1.visible = true
	VR_PoolShadow.visible = true
	VR_PoolCue1.Image = "zVR_Pool_Cue_v1_Diffuse"
	VR_PoolCue2.Image = "zVR_Pool_Cue_v1_DiffuseDARKHALF"
	Randomize (21)
	PoolLightTimer2.Interval = 200 * ((7 * Rnd) + 1)  ' 20 *  a random number between 1.05 and 7.95
	PoolLightTimer2.enabled = False
	PoolLightTimer1.enabled = True

	VR_Sign4.disablelighting = 1
	VR_BlueLight4.visible = true
	VR_Sign4.Material = "_noXtrashadingLight"
End Sub


sub PoolLightTimer1_timer()
	VR_PoolTable.Image = "zVR_PoolNew_D"
	VR_PoolLight1.visible = false
	VR_PoolShadow.visible = false
	VR_PoolCue1.Image = "zVR_Pool_Cue_v1_DiffuseDARK"
	VR_PoolCue2.Image = "zVR_Pool_Cue_v1_DiffuseDARK"
	Randomize (21)
	PoolLightTimer1.Interval = 600 * ((7 * Rnd) + 1)  ' 600 *  a random number between 1.05 and 7.95
	PoolLightTimer1.enabled = false
	PoolLightTimer2.enabled = true

	VR_Sign4.disablelighting = 0
	VR_BlueLight4.visible = false

	VR_Sign4.Material = "_noXtrashadingVR"
End Sub

sub Fantimer_timer()
	VR_Fan.Objrotz = VR_Fan.Objrotz + 2
	VR_Fan2.Objrotz = VR_Fan2.Objrotz + 2.2
	VR_Fan3.Objrotz = VR_Fan3.Objrotz + 2.3
	VR_Fan4.Objrotz = VR_Fan4.Objrotz + 2.4
End Sub

Dim TVCounter: TVCounter = 0

Sub TimerTV_Timer()
	VR_TV.Image = "zVR_Frame " & TVCounter
	TVCounter = TVCounter + 1
	If TVCounter > 108 Then
		TVCounter = 0
	End If
End Sub

Sub ClockTimer_Timer()
	' ROB AND WALTER'S Digital Clock below**************************************
	dim n

	n=Hour(now) MOD 12

	if n = 0 then n = 12

	VR_hour1.imagea="zVR_digit" & CStr(n \ 10)
	VR_hour2.imagea="zVR_digit" & CStr(n mod 10)

	n=Minute(now)

	VR_minute1.imagea="zVR_digit" & CStr(n \ 10)
	VR_minute2.imagea="zVR_digit" & CStr(n mod 10)
End Sub

'Sub TimerPlunger_Timer
'  VRPlunger.Y = 2145 + (5* Plunger.Position) - 25
'End Sub

Dim VRToggle, vr

Sub VRRoomToggle()
	if VRToggle = 1 and VRRoomOn = 1 then
		ClockTimer.enabled = True
		TimerTV.enabled = True
		FanTimer.enabled = True
		PoolLightTimer1.enabled = True
		VR_Floor.visible = false

		for each vr in VRRoom: vr.visible = 1: next
		VRToggle = 0
		exit sub
	end If

	if VRToggle = 0 then
		ClockTimer.enabled = False
		TimerTV.enabled = False
		FanTimer.enabled = False
		PoolLightTimer1.enabled = False
		PoolLightTimer2.enabled = False
		VR_Floor.visible = True

		for each vr in VRRoom: vr.visible = 0: next
		VRToggle = 1
		Exit sub
	End If
End Sub




