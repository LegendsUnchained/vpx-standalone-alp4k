
Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const BallSize = 50					'Ball size must be 50
Const BallMass = 1					'Ball mass must be 1

On Error Resume Next
ExecuteGlobal GetTextFile("core.vbs")
If Err Then MsgBox "Can't open core.vbs"
On Error Goto 0
Const cGameName = "volkan"
Const TableName = "volkan"


'*** Options ***
Const B2sServer = False  				' change to true to force B2S for DT mode, other modes is not affected

Const Music_Volume = 0.5				' music and background ( 0-1 )
Const Voice_Volume = 0.8				' voice overs
Const VolumeDial = 0.8					' for mech sounds
Const ChangeLut = True					' True = Hold Right Magna + tap Left magna to change ; False = cant change
Const StagedFlippers = 0                ' 0 = No staged flippers  1 = Upper flippers staged

Const TournamentPlay = False			' True = only 3 balls, no EB, low Ballsave
Const Free_Play = True					' True = no coins needed


Const DynamicBallShadowsOn = 1			'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1			'0 = Static shadow under ball ("flasher" image, like JP's)
										'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
										'2 = flasher image shadow, but it moves like ninuzzu's

' ResetHS 	' HowTo reset Highscores :   remove the first "'"  start table -> exit -> comment this line again then start normal




'VRRoom Auto set based on RenderingMode 
Dim VRRoom
If RenderingMode = 2 Then
	VRRoom = 1 : startcontroller
else 
	VRRoom = 0
End If

Dim MaxBalls 	  :		 MaxBalls = 5
Dim MaxExtraBalls : MaxExtraBalls = 4  
Dim MaxBallsave   :   MaxBallsave = 10	


'////////////////////////////////////////////////////////////////////////////////
'////          Mechanical Sounds Options, by Fleep                           ////
'////////////////////////////////////////////////////////////////////////////////
'
'//////////////////////////  MECHANICAL SOUNDS OPTIONS  /////////////////////////
'//  This section allows to set various general sound options for the mechanical sounds.
'//  For the entire sound system scripts see mechanical sounds block down below in the project.
'
'////////////////////////////  GENERAL SOUND OPTIONS  ///////////////////////////
'
'//  PositionalSoundPlaybackConfiguration:
'//  Specifies the sound playback configuration. Options:
'//  1 = Mono
'//  2 = Stereo (Only L+R channels)
'//  3 = Surround (Full surround sound support for SSF - Front L + Front R + Rear L + Rear R channels)
Const PositionalSoundPlaybackConfiguration = 3
'
'
'//  RelaysPosition:
'//  1 = Relays positioned with power board (Provides sound spread from the left and right back channels)
'//  2 = Relays positioned with GI strings (Provides sound spread from left/right/front/rear surround channels)
Const RelaysPosition = 2
'
'
''//  SpinWheelsMotorConfiguration:
''//  1 = Spin Wheels Motor Sound disabled, DOF disabled
''//  2 = Spin Wheels Motor Sound disabled, DOF enabled
''//  3 = Spin Wheels Motor Sound enabled, DOF disabled
''//  4 = Spin Wheels Motor Sound enabled, DOF enabled
'Const SpinWheelsMotorConfiguration = 4
''
''
''//  BackwallFanBlowerConfiguration:
''//  1 = FanBlower Sound disabled, DOF disabled
''//  2 = FanBlower Sound disabled, DOF enabled
''//  3 = FanBlower Sound enabled, DOF disabled
''//  4 = FanBlower Sound enabled, DOF enabled
'Const BackwallFanBlowerConfiguration = 1
'
'
'//  VolumeDial:
'//  VolumeDial is the actual global volume multiplier for the mechanical sounds.
'//  Values smaller than 1 will decrease mechanical sounds volume.
'//  Recommended values should be no greater than 1. 
'//  Default value is 0.8 and is optimal.
' moved to top of script
'
'
'//  PlayfieldRollVolumeDial:
'//  PlayfieldRollVolumeDial is a constant volume multiplier for the playfield rolling sound.
'//  Default value should be 1 which will guarantee a proper carefully calculated dynamic volume changes profile.
'//  Any values different than the default will impact the volume level and the dynamic voume changes profile.
Const PlayfieldRollVolumeDial = 1
'
'
'//  RampsRollVolumeDial:
'//  RampsRollVolumeDial is a constant volume multiplier for all ramps rolling sounds. 
'//  This includes all types of plastic and metal ramps.
'//  Default value should be 1 which will guarantee a proper carefully calculated dynamic volume changes profile.
'//  Any values different than the default will impact the volume level and the dynamic voume changes profile.
'//  For a specific ramp volume change please refer to the corresponding volume variables in the script sound paramter section.
Const RampsRollVolumeDial = 1
'
'
'////////////////////////////////////////////////////////////////////////////////
'////          End of Fleep Mechanical Sounds Options                        ////
'////////////////////////////////////////////////////////////////////////////////

'DOF id
'E101 0/1 LeftFlipper
'E102 0/1 RightFlipper
'E103 2 Leftslingshot
'E104 2 Rightslingshot
'E105 2 bumper001   bottom right
'E106 2 bumper002   bottom left
'E107 2 bumper003   top right
'E108 2 bumper004   top left
'E109 2 Flasher bottom right
'E110 2 Flasher bottom left
'E111 2 Flasher top right
'E112 2 Flasher top left
'E113 0/1 Startgamebutton ready
'E114 0/1 Ball in plunger lane
'E115 2 startgame
'E116 2 drain Hit
'E117 2 ballsaved
'E118 2 Left Spinner 
'E119 2 Right spinner
'E120 2 autolaunch
'E121 2 Droptarget hit
'E122 2 add credits
'E123 2 blastvuk hit (top middle)
'E124 2 stokevuk hit (top left)
'E125 2 coalkicker hit ( middle right)
'E126 2 award extraball
'E127 0/1 Multiball
'E128 2 addaball
'E129 2 level finished
'E130 2 standup top
'E131 2 standup lower
'E132 0/1 Tweed in Control
'E133 0/1 Baron in control
'E134 0/1 not in Control
'E135 0/1 GI on /off
'E136 2 Carbon target hit
'E137 2 Gear motor 2 sec

Const BallDimScale = 0.4

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

Dim BIP : BIP = 0 'Balls in play
Dim BIPL : BIPL = False				'Ball in plunger lane
Dim PlayerScore(2)
Dim CurrentPlayer
Dim Credits
Dim EOB_Bonus
Dim PlayerMode (2)
Dim Order_Value_Counter
Dim SteamCounter
Dim ElectricCounter
Dim Bumper1Counter
Dim Bumper2Counter
Dim Bumper3Counter
Dim Bumper4Counter
Dim RaiseTemp
Dim TotalStokeHits(2)
Dim HurryupStokeDone(2)
Dim TotalBumperHits(2)
Dim HurryupBumperDone(2)
Dim Hurryup_Active
Dim Ballsave
Dim ballsaveActivated
Dim BallInPlay
Dim Tilt
Dim Tilted : Tilted = False
Const TiltSens = 50
Dim HighscoreTweed(2)
Dim HighscoreBaron(2)
Dim HighscoreDuelTweed(2)
Dim HighscoreDuelBaron(2)

Dim scorereeels(22)
Dim scorechange(22)
Dim scoreflag(18)




'*******************************************
'  Timers
'*******************************************


' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update
End Sub


' The frame timer interval is -1, so executes at the display frame rate
dim FrameTime, InitFrameTime : InitFrameTime = 0
Sub FrameTimer_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime
	FlipperLL.RotZ = LeftFlipper.CurrentAngle-120.5
	FlipperLR.RotZ = RightFlipper.CurrentAngle+120.5
	FlipperUL.RotZ = LeftFlipper2.CurrentAngle-120.5
	FlipperUR.RotZ = RightFlipper2.CurrentAngle+120.5 
	blastdoor.rotx = abs ( DoorF.currentangle-90 ) 
	RollingSoundUpdate
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub


dim ii : ii = array (0,EMReel001,EMReel002,EMReel003,EMReel004,EMReel005,EMReel006,EMReel007,EMReel008,EMReel009,EMReel010,EMReel011,EMReel012,EMReel013,EMReel014,EMReel015,EMReel016,EMReel_BIP,EMReel_Player)
Sub Timer001_Timer ' only desktop for now 
	If DesktopMode or b2son then
		dim i,i2
'controller.B2SSetData 40,1
'controller.B2SSetScorePlayer1 P1score
'controller.B2SSetScoreDigit 1,1

		i2 = PlayerScore(1)
		i=Int(i2/10000000) : scorechange(1)=i : i2=i2-(i*10000000)
		If b2son then controller.B2SSetScoreDigit 1,i
		i=Int(i2/1000000) : scorechange(2)=i : i2=i2-(i*1000000)
		If b2son then controller.B2SSetScoreDigit 2,i
		i=Int(i2/100000) : scorechange(3)=i : i2=i2-(i*100000)
		If b2son then controller.B2SSetScoreDigit 3,i
		i=Int(i2/10000) : scorechange(4)=i : i2=i2-(i*10000)
		If b2son then controller.B2SSetScoreDigit 4,i
		i=Int(i2/1000) : scorechange(5)=i : i2=i2-(i*1000)
		If b2son then controller.B2SSetScoreDigit 5,i
		i=Int(i2/100) : scorechange(6)=i : i2=i2-(i*100)
		If b2son then controller.B2SSetScoreDigit 6,i
		i=Int(i2/10) : scorechange(7)=i : i2=i2-(i*10)
		If b2son then controller.B2SSetScoreDigit 7,i
		scorechange(8)=i2
		If b2son then controller.B2SSetScoreDigit 8,i2
		i2 = PlayerScore(2)
		i=Int(i2/10000000) : scorechange(9)=i : i2=i2-(i*10000000)
		If b2son then controller.B2SSetScoreDigit 9,i
		i=Int(i2/1000000) : scorechange(10)=i : i2=i2-(i*1000000)
		If b2son then controller.B2SSetScoreDigit 10,i
		i=Int(i2/100000) : scorechange(11)=i : i2=i2-(i*100000)
		If b2son then controller.B2SSetScoreDigit 11,i
		i=Int(i2/10000) : scorechange(12)=i : i2=i2-(i*10000)
		If b2son then controller.B2SSetScoreDigit 12,i
		i=Int(i2/1000) : scorechange(13)=i : i2=i2-(i*1000)
		If b2son then controller.B2SSetScoreDigit 13,i
		i=Int(i2/100) : scorechange(14)=i : i2=i2-(i*100)
		If b2son then controller.B2SSetScoreDigit 14,i
		i=Int(i2/10) : scorechange(15)=i : i2=i2-(i*10)
		If b2son then controller.B2SSetScoreDigit 15,i
		scorechange(16)=i2
		If b2son then controller.B2SSetScoreDigit 16,i2
		If DesktopMode then
			For i = 9-len(CStr(abs(playerscore(1)))) to 8
'			For i = 1 to 8

				If scorechange(i) <> scorereeels(i) And PlayerScore(1) > 0 then scoreflag(i) = 1 : scorereeels(i) = scorechange(i)
				Select Case scoreflag(i)
				case 1: If scorereeels(i) <> -1 then ii(i).image = "nixies2"
				case 2: If scorereeels(i) <> -1 then ii(i).image = "nixies3"
				case 3: If scorereeels(i) <> -1 then ii(i).image = "nixies4"
						ii(i).setvalue scorereeels(i)
				case 4: 
						ii(i).image = "nixies3"
				case 5: ii(i).image = "nixies2"
				case 6: ii(i).image = "nixies1"
						scoreflag(i) = 0
				End Select
				If scoreflag(i) > 0 then scoreflag(i) = scoreflag(i) + 1
			Next
			For i = 17-len(CStr(abs(playerscore(2)))) to 16
'			For i = 9 to 16
				If scorechange(i) <> scorereeels(i) And PlayerScore(2) > 0 then scoreflag(i) = 1 : scorereeels(i) = scorechange(i)
				Select Case scoreflag(i)
				case 1: If scorereeels(i) <> -1 then ii(i).image = "nixies2"
				case 2: If scorereeels(i) <> -1 then ii(i).image = "nixies3"
				case 3: If scorereeels(i) <> -1 then ii(i).image = "nixies4"
				case 4: ii(i).setvalue scorereeels(i)
						ii(i).image = "nixies3"
				case 5: ii(i).image = "nixies2"
				case 6: ii(i).image = "nixies1"
						scoreflag(i) = 0
				End Select
				If scoreflag(i) > 0 then scoreflag(i) = scoreflag(i) + 1
			Next

			scorechange(17) = BallInPlay
			scorechange(18) = Credits
			For i = 17 to 18
				If scorechange(i)=0 then scorechange(i) = -1
				If scorechange(i) <> scorereeels(i) then scoreflag(i) = 1 : scorereeels(i) = scorechange(i)
				Select Case scoreflag(i)
				case 1: If scorereeels(i) <> -1 then ii(i).image = "nixies2"
				case 2: If scorereeels(i) <> -1 then ii(i).image = "nixies3"
				case 3: If scorereeels(i) <> -1 then ii(i).image = "nixies4"
				case 4: ii(i).setvalue scorereeels(i)
						ii(i).image = "nixies3"
				case 5: ii(i).image = "nixies2"
				case 6: ii(i).image = "nixies1"
						scoreflag(i) = 0
				End Select
				If scoreflag(i) > 0 then scoreflag(i) = scoreflag(i) + 1
			Next
		End If

		If b2son then
			scorechange(20) = BallInPlay
			scorechange(21) = Credits
			scorechange(22) = PlayersPlaying
			If  scorechange(20) <> scorereeels(20) then scorereeels(20) =  scorechange(20) : controller.B2SSetScoreDigit 18,BallInPlay
			If  scorechange(21) <> scorereeels(21) then scorereeels(21) =  scorechange(21) : controller.B2SSetScoreDigit 17,Credits
			If  scorechange(22) <> scorereeels(22) then scorereeels(22) =  scorechange(22) : controller.B2SSetScoreDigit 19,PlayersPlaying
		End If

	End If
End Sub

Sub ResetScoreWheels
	Dim x
	If DesktopMode then
		for x = 1 to 18
			scorereeels(x) = -1
			scorechange(x) = 0
			scoreflag(x) = 0
			ii(x).setvalue  0
			ii(x).image = "nixies4"
		Next
		
	End If
End Sub


Dim Controller
Sub startcontroller
	Set Controller = CreateObject("B2S.Server") 
	If Not Controller Is Nothing Then ' for standalone
		Controller.B2SName = cGameName
		Controller.Run
		B2SOn = True
	End If  
End Sub

'*******************************************
'  Table Initialization and Exiting
'*******************************************

Dim LutValue
Sub LoadLut
	Dim x
    x = LoadValue(TableName, "SavedLut")
    If(x <> "") Then LutValue = CDbl(x) Else LutValue = 1
	If LutValue < 1 Then LutValue = 1
	If LutValue > 13 Then LutValue = 13
	ShowLut
End Sub
Sub NewLut
    SaveValue TableName, "SavedLut" , LutValue
	ShowLut
	TextBox001.text = "LUT = " & LutValue
	TextBox001.visible = True
	TextBox001.timerenabled = True
End Sub
Sub TextBox001_Timer
	TextBox001.visible = False
	TextBox001.timerenabled = False
End Sub
Sub ShowLut
	table1.ColorGradeImage = "Volkan_LUT_V" & LutValue
End Sub


Dim ETBall1, ETBall2, ETBall3, ETBall4, ETBall5, gBOT, BallColors

Sub Table1_Init
	Dim i
	Dim VRFL
	
	TextBox001_Timer

	If B2sServer and vrroom = 0 then	' for forsced b2s on desktopveiw
		startcontroller
	Else
		If not desktopmode and vrroom = 0 Then startcontroller ' for cabinet
	End If
	If TournamentPlay Then
		MaxBalls = 3
		MaxExtraBalls = 0
		MaxBallsave = 0	
	End If
	If VRRoom > 0 Then
		Timer001.enabled = False
		for each VRFL in VRBackglassGIVolkan : VRFL.opacity = 0 : VRFL.visible = true : Next
		for each VRFL in VRBackglassGITweed : VRFL.opacity = 0 : VRFL.visible = true : Next
		for each VRFL in VRBackglassGIBaron : VRFL.opacity = 0 : VRFL.visible = true : Next
		for each VRFL in VRBackglassGIAbigail : VRFL.opacity = 0 : VRFL.visible = true : Next
		VRTweed.opacity    = 0
		VRBaron.opacity    = 0
		VRTilt.opacity    = 0
		VRGameOver.opacity = 0
		VRBallSave.opacity = 0
		VRHS.opacity = 0
		VRHS2.opacity = 0
		VRHS3.opacity = 0
		VRDHS.opacity = 0
		VRDHS2.opacity = 0
		VRDHS3.opacity = 0
		VRTweed.visible = 1
		VRBaron.visible = 1
		VRGameOver.visible = 1
		VRTilt.visible = 1
		VRBallSave.visible = 1
		VRHS.visible = 1
		VRHS2.visible = 1
		VRHS3.visible = 1
		VRDHS.visible = 1
		VRDHS2.visible = 1
		VRDHS3.visible = 1
		table1.BackdropImage_DT = Empty
		DesktopMode = False
	End If

	'Ball initializations need for physical trough
	Set ETBall1 = swTrough1.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set ETBall2 = swTrough2.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set ETBall3 = swTrough3.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set ETBall4 = swTrough4.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set ETBall5 = swTrough5.CreateSizedballWithMass(Ballsize/2,Ballmass)

	'*** Use gBOT in the script wherever BOT is normally used. Then there is no need for GetBalls calls ***
	gBOT = Array(ETBall1, ETBall2, ETBall3, ETBall4, ETBall5)
	ETBall1.color = rgb(240,60,20)
	ETBall2.color = rgb(100,100,100)
	ETBall3.color = rgb(230,90,10)
	ETBall4.color = rgb(160,60,10)

	BallColors = Array(ETBall1.color, ETBall2.color, ETBall3.color, ETBall4.color, ETBall5.color)

	' Add balls to shadow dictionary
	Dim xx: For Each xx in gBOT
		bsDict.Add xx.ID, bsNone
	Next


	If Not DesktopMode then
		For Each i in DesktopStuff
			i.visible = False
		Next
	Else
		PinCab_Rails.visible = 1
		light013.state = 2
		light014.state = 2
		light015.state = 2
		light016.state = 2
	End If



	GiOff
	StartAttractMode

	wall003.collidable = False
	wall004.collidable = False
	ResetScoreWheels
	NeedleLeft.rotz = 42 '  42-144 = 102
	NeedleRight.rotz = 47 '  47-131 = 84

	Loadhs

	LoadLut
	If Free_Play then Credits = 9
	

End Sub

Sub Table1_Exit
	Savehs
End Sub

Dim MultiballReady
If MaxBalls < 4 Then MaxBalls = 3 Else MaxBalls = 5
If MaxExtraBalls < 0 Then MaxExtraBalls = 0 
If MaxExtraBalls > 4 Then MaxExtraBalls = 4
If MaxBallsave < 0 Then MaxBallsave = 0
If MaxBallsave > 10 Then MaxExtraBalls = 10
Dim EB_Levels(2)
Dim EB_Targets(2)
Dim EB_Carbons(2)
Dim EB_Inlanes(2)
Dim EB_Collected(2)
DIM EB_Ready(2)
Dim HurryupLevel2(2)
Dim HurryupLevelC
Dim FirstMultiball(2)
Dim AfterWizard(2)
Dim SkipMBstart5(2)
Dim SkipMBstart10(2)
Dim BadBall(2)
Dim BadBallComplete(2)

Sub	ResetForNewGame
	dim i
	For i = 1 to 2
		PlayerScore(i)		= 0
		PlayerMode(i)		= 0 
		TotalStokeHits(i)	= 0
		TotalBumperHits(i)	= 0
		EB_Levels(i) = 0
		EB_Targets(i) = 0
		EB_Carbons(i) = 0
		EB_Inlanes(i) = 0
		EB_Collected(i) = 0
		EB_Ready(i) = 0
		HurryupLevel2(i) = 0
		FirstMultiball(i)=0
		AfterWizard(i)=0
		SkipMBstart5(i)=0
		SkipMBstart10(i)=0
		BadBall(i)=0
		BadBallComplete(i)=0
	Next
	HurryupLevelC = 0

	ClearVoiceQ

	Order_Value_Counter = 0
	CurrentPlayer = 1
	BallsToLaunch = 1
	drain.timerenabled = False
	drain.timerenabled = True
	bls 114,114,5,6,13,0
	
	GiOn

	MultiballReady = True

	DoorF.rotatetostart
	Blink(121,1)=0
	Blink(122,1)=0
	Blink(123,1)=0
	Blink(124,1)=0

	PlaySound "sfx_furnacedoor",1,VolumeDial
	blastfurnaceReady.enabled = False
	BlastdoorOpen = 0

	Startmode 

	Blink(134,1)=2
	Blink(133,1)=0
	bls 133,133,3,7,7,0
	bls 134,134,1,7,7,15



	Blink(22,1) = 0

	SteamCounter = 0
	ElectricCounter = 0
	Bumper1Counter = 0
	Bumper2Counter = 0
	Bumper3Counter = 0
	Bumper4Counter = 0
	RaiseTemp = 0

	For i = 1 to 140
		Saveplayer(i,2)=Blink(i,1)
	Next ' save player 2 incase it gonna be playd
	Saveplayer(141,2) = 0
	Saveplayer(142,2) = 0
	Saveplayer(143,2) = 0
	Saveplayer(144,2) = 0
	Saveplayer(145,2) = 0
	Saveplayer(146,2) = 0
	Saveplayer(147,2) = 0
	Saveplayer(148,2) = 0
End Sub




Dim TalkingQ(9,3)
Sub ClearVoiceQ
	Dim x
	For x = 0 to 9
		TalkingQ(x,0) = ""
		TalkingQ(x,1) = ""
		TalkingQ(x,2) = ""
		TalkingQ(x,3) = ""
	Next
	timetotalk = 0
	stopsound LastVoice
'	Debug.Print "ClearVoiceQ" 
End Sub



Dim testdelay
Sub PlayVoice(sound,repeat,volume,delay)
	If Tilted Then Exit Sub
'	debug.print "AddVoice " & sound
'	testdelay = gametime
	Dim x , xx
	If bip > 1 Then xx = 1 else xx = 4
	For x = 0 to xx
		If TalkingQ(x,0) = "" then
			TalkingQ(x,0) = sound
			TalkingQ(x,1) = repeat
			TalkingQ(x,2) = volume
			TalkingQ(x,3) = delay
			Exit Sub
		End If
	Next
'	debug.print "NotAdded " & sound 
End Sub




Dim LastVoice 
Sub BallsaverTimer_Timer
	Dim x
	If Timetotalk < 1 and TalkingQ(0,0)<>"" then
		If TalkingQ(0,3) > 0 then
'			debug.print "PlayVoice Waiting " & TalkingQ(0,3)
			TalkingQ(0,3) = TalkingQ(0,3) - 1
		Else
			timetotalk = 4
			LastVoice = TalkingQ(0,0)
			PlaySound TalkingQ(0,0),TalkingQ(0,1),TalkingQ(0,2)
'			debug.print "PlayVoice: " & TalkingQ(0,0) & " Delay= " & gametime - testdelay
			For x = 0 to 8
				TalkingQ(x,0) = TalkingQ(x+1,0)
				TalkingQ(x,1) = TalkingQ(x+1,1)
				TalkingQ(x,2) = TalkingQ(x+1,2)
				TalkingQ(x,3) = TalkingQ(x+1,3)
			Next
			TalkingQ(9,0) = ""
			TalkingQ(9,1) = ""
			TalkingQ(9,2) = ""
			TalkingQ(9,3) = ""
		End If
	Else

		If TimeToTalk < 1 then
			Select Case CurrentPlayer
				case 1:
					If BaronControl > 0 And BaronControl + 12121 < gametime and balltime < 1000 then
						BaronControl = gametime + 15151
						Select Case Int(rnd(1)*7)
							case 0 : PlayVoice "Tweed_RobertIsTakingMyMoney",1,Voice_Volume,0
							case 1 : PlayVoice "Tweed_RobertChallengesMy"	,1,Voice_Volume	,0
							case 2 : PlayVoice "Tweed_MyEveryMove",1,Voice_Volume,0
							case 3 : PlayVoice "Tweed_IMustGetControlBack",1,Voice_Volume,0
							case 4 : PlayVoice "Tweed_ICannotTrustThose",1,Voice_Volume,0
							case 5 : PlayVoice "baron_TweedDoesntSuspect",1,Voice_Volume,0
							case 6 : PlayVoice "vo_stayawayfromblue",1,Voice_Volume,0
						End Select

					End If

				Case 2:
					If TweedControl > 0 And TweedControl + 12121 < gametime and balltime < 1000 then
						TweedControl = gametime + 15151
						Select Case Int(rnd(1)*6)
							case 0 : PlayVoice "baron_HitTheBlueTargets",1,Voice_Volume,0
							case 1 : PlayVoice "baron_IMustHaveControl"	,1,Voice_Volume	,0
							case 2 : PlayVoice "baron_StayAwayFromGreenTargets",1,Voice_Volume,0
							case 3 : PlayVoice "baron_TweedIsHoldingUsBack",1,Voice_Volume,0
							case 4 : PlayVoice "baron_TweedMustBeStopped",1,Voice_Volume,0
							case 6 : PlayVoice "vo_stayawayfromgreen",1,Voice_Volume,0
						End Select
					End If
			End Select
		End If
	End If

	TimeToTalk = TimeToTalk - 1 : If TimeToTalk < 0 then TimeToTalk = 0


	If balltime mod 28 = 24 and balltime < 1000 And BiP = 1 then PlayAbigail 0.6


	balltime = balltime + 1
	if balltime > 50 and Balltimecounter = 0 and balltime < 1000 And BIP = 1 then Balltimecounter = 1  : PlayVoice "sfx_payattention",1,Voice_Volume,0
	if balltime > 80 and Balltimecounter = 1 and balltime < 1000 And BIP = 1  then Balltimecounter = 2  : PlayVoice "sfx_undividedattention",1,Voice_Volume,0
	if balltime > 110 and Balltimecounter = 2 and balltime < 1000 And BIP = 1  then Balltimecounter = 3 : PlayVoice "vo_orderlate",1,Voice_Volume,0
	if balltime > 160 and Balltimecounter = 2 and balltime < 1000 And BIP = 1  then Balltimecounter = 3 : PlayVoice "vo_orderlate",1,Voice_Volume*0.6,0

	If BlastdoorMB = 1 then MBcounter = MBcounter + 1 Else MBcounter = 0
	If timetotalk < 1 And MBcounter > 14 and BlastdoorMB = 1 then
		If CurrentPlayer = 2 then
			Select Case int(rnd(1)*6)
				case 0 : PlayVoice "baron_gravity",1,Voice_Volume * 0.8 ,0
				case 1 : PlayVoice "baron_idemand",1,Voice_Volume * 0.8 ,0
				case 2 : PlayVoice "baron_moreproduction",1,Voice_Volume * 0.8 ,0
				case 3 : PlayVoice "baron_riskreward",1,Voice_Volume * 0.8 ,0
				case 4 : PlayVoice "baron_thisismine",1,Voice_Volume * 0.8 ,0
				case 5 : PlayVoice "baron_wayofprogress",1,Voice_Volume * 0.8 ,0
			End Select	
		Else
			Select Case int(rnd(1)*6)
				case 0 : PlayVoice "tweed_furnaceempty",1,Voice_Volume * 0.8 ,0
				case 1 : PlayVoice "tweed_imperative",1,Voice_Volume * 0.8 ,0
				case 2 : PlayVoice "tweed_keeprunning",1,Voice_Volume * 0.8 ,0
				case 3 : PlayVoice "tweed_keepup",1,Voice_Volume * 0.8 ,0
				case 4 : PlayVoice "tweed_needmore",1,Voice_Volume * 0.8 ,0
				case 5 : PlayVoice "tweed_onlypay",1,Voice_Volume * 0.8 ,0
			End Select	
		End If
		MBcounter = 0
	End If


	If ballsave > 2 Then
'		bls 17,17,3,6,11,11

		If ballsaveactivated = 1 Then
			If VRRoom = 0 Then
				If DesktopMode then
					Light005.state=2
					Light006.state=2 
				End If
			End If
			If b2son then bbs 5,0,5,10
		End If
	Else
		If DesktopMode then
			Light005.state=0
			Light006.state=0 
		End If
		If b2son then bbs 5,0,0,0
	End If

	If ballsaveactivated = 1 Then
		ballsave = Ballsave - 1
		bls 105,105,2,8,3,0
	End If
	If ballsave < - 2 then
'		If ballsaveActivated > 0 Then debug.print "BallSaveDeactivated"
		ballsaveactivated = 0
		If ControlLock = True then
			If int(rnd(1)*2) = 1 then PlayVoice "vo_controlexpired",1,Voice_Volume,0 else PlayVoice "vo_controlreleased",1,Voice_Volume,0
			ControlLock = False 
			Blink(18,1) = 0 
			Blink(19,1) = 0
		End If
	End If
End Sub
Dim MBcounter

Sub PlayAbigail (vol)
		Select Case int(rnd(1)*12)
			case 0 : PlayVoice "vo2_bestinterests",1,Voice_Volume * vol,0
			case 1 : PlayVoice "vo2_edgeofmadness",1,Voice_Volume * vol,0
			case 2 : PlayVoice "vo2_goingtohappen.",1,Voice_Volume * vol,0
			case 3 : PlayVoice "vo2_imafraid",1,Voice_Volume * vol,0
			case 4 : PlayVoice "vo2_moreforVolkan",1,Voice_Volume * vol,0
			case 5 : PlayVoice "vo2_notwell",1,Voice_Volume * vol,0
			case 6 : PlayVoice "vo2_suspiciouseveryone",1,Voice_Volume * vol,0
			case 7 : PlayVoice "vo2_themoney",1,Voice_Volume * vol,0
			case 8 : PlayVoice "vo2_trustrobert",1,Voice_Volume * vol,0
			case 9 : PlayVoice "vo2_unfounded",1,Voice_Volume * vol,0
			case 10 : PlayVoice "vo2_whattodo",1,Voice_Volume * vol,0
			case 11 : PlayVoice "vo2_worriedbrother",1,Voice_Volume * vol,0
		End Select
End Sub



Dim DoubleScoring
'Dim TrippleScoring
Sub Startmode


	dim axe
	Select Case PlayerMode(currentplayer)
		case 0 : axe = array ( 1,11,12,13,30,31,32,40,41,42,61,62,92 )
				DoubleScoring = False
				Blink(29,1) = 0
		case 1 : axe = array ( 2,11,12,13,30,31,32,33,40,41,42,51,52,91 )

				HurryupLevel2(currentplayer) = 1					' start Hurryup
				HurryupLevelC = 0
				DoorF.rotatetoend
				PlaySound "sfx_furnacedoor",1,VolumeDial
				blastfurnaceReady.enabled = True
				Blink(121,1)=2
				Blink(122,1)=2
				Blink(123,1)=2
				Blink(124,1)=2
				BLS 121,124,5,5,5,0

				Blink(125,1)=2
				Blink(126,1)=2
				Blink(127,1)=2
				Blink(128,1)=2
				bls 125,128,5,5,5,0


		case 2 : axe = array ( 3,11,12,30,31,32,33,40,41,42,43,51,52,53,71,72,93 )

				If HurryupLevel2(currentplayer) > 0 Then ' start Hurryup if level 2 failed
					HurryupLevel2(currentplayer) = 1
					HurryupLevelC = 0
					DoorF.rotatetoend
					PlaySound "sfx_furnacedoor",1,VolumeDial
					blastfurnaceReady.enabled = True
					Blink(121,1)=2
					Blink(122,1)=2
					Blink(123,1)=2
					Blink(124,1)=2
					BLS 121,124,5,5,5,0
					Blink(125,1)=2
					Blink(126,1)=2
					Blink(127,1)=2
					Blink(128,1)=2
					bls 125,128,5,5,5,0

				End If


		case 3 : axe = array ( 4,11,12,30,31,32,33,34,40,41,42,43,51,52,53,81,82,93 )

				HurryupLevel2(currentplayer) = 1					' start Hurryup nr 2
				HurryupLevelC = 0
				DoorF.rotatetoend
				PlaySound "sfx_furnacedoor",1,VolumeDial
				blastfurnaceReady.enabled = True
				Blink(121,1)=2
				Blink(122,1)=2
				Blink(123,1)=2
				Blink(124,1)=2
				BLS 121,124,5,5,5,0
				Blink(125,1)=2
				Blink(126,1)=2
				Blink(127,1)=2
				Blink(128,1)=2
				bls 125,128,5,5,5,0

		case 4 : axe = array ( 5,11,12,13,14,30,31,32,33,34,40,41,42,43,44,61,62,63,21,94 )

				ClearVoiceQ
				LockControl
				Blink(18,1)=2
				Blink(19,1)=0

				PlayVoice "vo_multiball",1,Voice_Volume,0 : bbs 17, 1, 14, 13
				PlayVoice "vo_controllocked",1,Voice_Volume,0

				BallsToLaunch =  BallsToLaunch + 2
				drain.timerenabled = False
				drain.timerenabled = True
				AutoPlunger = True
				Doublescoring = True
				stopsound "Volkan_Overture_II"
				PlayVoice "sfx_multiballbeat",-1,Voice_Volume * 0.9,1
				BlastdoorMB = 1 
				blasttriggerON = 0
				MaxMB_JPs = 15
				DoorF.rotatetoend  
				Blink(121,1)=2
				Blink(122,1)=2
				Blink(123,1)=2
				Blink(124,1)=2
				BLS 121,124,5,5,5,0

				PlaySound "sfx_furnacedoor",1,VolumeDial
				blastfurnaceReady.enabled = True
				bls 17,17,15,7,5,0
				Ballsave = 10 + MaxBallsave
'				debug.print "BallSave20sec"
				blink(104,1) = 0
				Blink(29,1) = 1

		case 5 : axe = array ( 6,11,12,13,30,31,32,33,34,35,40,41,42,43,44,45,46,61,62,63,64,92 )
				If EB_Collected(CurrentPlayer) < MaxExtraBalls And EB_Levels(CurrentPlayer) = 0 Then
					EB_Levels(CurrentPlayer) = 1
					Lite_EB_insert
				End If

				HurryupLevel2(currentplayer) = 1					' start Hurryup nr 3
				HurryupLevelC = 0
				DoorF.rotatetoend
				PlaySound "sfx_furnacedoor",1,VolumeDial
				blastfurnaceReady.enabled = True
				Blink(121,1)=2
				Blink(122,1)=2
				Blink(123,1)=2
				Blink(124,1)=2
				BLS 121,124,5,5,5,0
				Blink(125,1)=2
				Blink(126,1)=2
				Blink(127,1)=2
				Blink(128,1)=2
				bls 125,128,5,5,5,0

		case 6 : axe = array ( 7,11,12,13,30,31,32,33,34,35,36,40,41,42,43,44,45,46,47,51,52,53,54,91 )

				If HurryupLevel2(currentplayer) > 0 Then ' start Hurryup if level failed
					HurryupLevel2(currentplayer) = 1
					HurryupLevelC = 0
					DoorF.rotatetoend
					PlaySound "sfx_furnacedoor",1,VolumeDial
					blastfurnaceReady.enabled = True
					Blink(121,1)=2
					Blink(122,1)=2
					Blink(123,1)=2
					Blink(124,1)=2
					BLS 121,124,5,5,5,0
					Blink(125,1)=2
					Blink(126,1)=2
					Blink(127,1)=2
					Blink(128,1)=2
					bls 125,128,5,5,5,0
				End If

		case 7 : axe = array ( 8,11,12,30,31,32,33,34,35,36,37,40,41,42,43,44,45,46,47,48,51,52,53,54,71,72,73,74,93 )

					HurryupLevel2(currentplayer) = 1
					HurryupLevelC = 0
					DoorF.rotatetoend
					PlaySound "sfx_furnacedoor",1,VolumeDial
					blastfurnaceReady.enabled = True
					Blink(121,1)=2
					Blink(122,1)=2
					Blink(123,1)=2
					Blink(124,1)=2
					BLS 121,124,5,5,5,0
					Blink(125,1)=2
					Blink(126,1)=2
					Blink(127,1)=2
					Blink(128,1)=2
					bls 125,128,5,5,5,0

		case 8 : axe = array ( 9,11,12,30,31,32,33,34,35,36,37,38,40,41,42,43,44,45,46,47,48,51,52,53,54,81,82,83,84,93 )

				If HurryupLevel2(currentplayer) > 0 Then ' start Hurryup if level failed
					HurryupLevel2(currentplayer) = 1
					HurryupLevelC = 0
					DoorF.rotatetoend
					PlaySound "sfx_furnacedoor",1,VolumeDial
					blastfurnaceReady.enabled = True
					Blink(121,1)=2
					Blink(122,1)=2
					Blink(123,1)=2
					Blink(124,1)=2
					BLS 121,124,5,5,5,0
					Blink(125,1)=2
					Blink(126,1)=2
					Blink(127,1)=2
					Blink(128,1)=2
					bls 125,128,5,5,5,0
				End If

		case 9 : axe = array (10,11,12,13,14,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,61,62,63,64,16,21,94 )

				ClearVoiceQ
				LockControl
				Blink(18,1)=2
				Blink(19,1)=0

				PlayVoice "vo_multiball",1,Voice_Volume,0 : bbs 17, 1, 14, 13
				PlayVoice "vo_controllocked",1,Voice_Volume,0
				BallsToLaunch =  BallsToLaunch + 2
				drain.timerenabled = False
				drain.timerenabled = True
				AutoPlunger = True
				Doublescoring = True
				stopsound "Volkan_Overture_II"
				PlayVoice "sfx_multiballbeat",-1,Voice_Volume * 0.9,0
				BlastdoorMB = 1 
				MaxMB_JPs = 15
				DoorF.rotatetoend 
				Blink(121,1)=2
				Blink(122,1)=2
				Blink(123,1)=2
				Blink(124,1)=2
				BLS 121,124,5,5,5,0 
				PlaySound "sfx_furnacedoor",1,VolumeDial
				blastfurnaceReady.enabled = True
				blasttriggerON = 0
				bls 17,17,15,7,5,0
				Ballsave =  10 + MaxBallsave
'				debug.print "BallSave20sec"
				blink(104,1) = 0
				Blink(29,1) = 1

		case 10: axe = array (125,126,127,128,1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,21,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,61,62,63,64,71,72,73,74,81,82,83,84,91,92,93,94)
				Doublescoring = True
				Blink(29,1) = 2
				PlayVoice  "vo_doubleproduction",1,Voice_Volume,0
				PlayVoice "vo_shootcarbon",1,Voice_Volume,4
	End Select
	dim x
	For Each x in axe : Blink(x,1)=2 : Next
	If AfterWizard(CurrentPlayer) = 1 Then
		For x = 1 to 10
			If Blink(x,1) = 0 Then Blink(x,1) = 1
		Next
		BLS 1,10,5,5,5,0
	End If
	GaugeLeftPos = 42
	GaugeRightPos = 47
	balltime = 0
End Sub



Sub completetheorder
	ClearVoiceQ
	dim tmp
	Select Case int(rnd(1)*5)
		case 0 : tmp = "vo_completeorder"
		case 1 : tmp = "vo_youhaveeverything"
		case 2 : tmp = "vo_youhaveeverything2"
		case 3 : tmp = "vo_blastready"
		case 4 : tmp = "vo_shootblast2"
	End Select
	PlayVoice tmp,1,Voice_Volume,1
End Sub

Dim BlastdoorOpen
Sub CheckMode
	Select Case PlayerMode(currentplayer)
	Case 0 : If Blink(40,1)=0 And Blink(30,1)=0 And Blink(61,1)=1 And Blink(62,1)=1 And Blink(92,1)=1 And Blink(23,1)=1 then OpenForCompleteorder
	case 1 : If Blink(40,1)=0 And Blink(30,1)=0 And Blink(51,1)=1 And Blink(52,1)=1 And Blink(91,1)=1 And Blink(23,1)=1 then OpenForCompleteorder
	case 2 : If Blink(40,1)=0 And Blink(30,1)=0 And Blink(51,1)=1 And Blink(52,1)=1 And Blink(53,1)=1 And Blink(71,1)=1 And Blink(72,1)=1 And Blink(93,1)=1 And Blink(22,1)=1 then OpenForCompleteorder
	case 3 : If Blink(40,1)=0 And Blink(30,1)=0 And Blink(51,1)=1 And Blink(52,1)=1 And Blink(53,1)=1 And Blink(81,1)=1 And Blink(82,1)=1 And Blink(93,1)=1 And Blink(22,1)=1 then OpenForCompleteorder
	case 4 : If Blink(40,1)=0 And Blink(30,1)=0 And Blink(61,1)=1 And Blink(62,1)=1 And Blink(63,1)=1 And Blink(21,1)=1 And Blink(94,1)=1 And Blink(24,1)=1 then OpenForCompleteorder
	case 5 : If Blink(40,1)=0 And Blink(30,1)=0 And Blink(61,1)=1 And Blink(62,1)=1 And Blink(63,1)=1 And Blink(64,1)=1 And Blink(92,1)=1 And Blink(23,1)=1 then OpenForCompleteorder
	case 6 : If Blink(40,1)=0 And Blink(30,1)=0 And Blink(51,1)=1 And Blink(52,1)=1 And Blink(53,1)=1 And Blink(54,1)=1 And Blink(91,1)=1 And Blink(23,1)=1 then OpenForCompleteorder
	case 7 : If Blink(40,1)=0 And Blink(30,1)=0 And Blink(51,1)=1 And Blink(52,1)=1 And Blink(53,1)=1 And Blink(54,1)=1 And Blink(71,1)=1 And Blink(72,1)=1 And Blink(73,1)=1 And Blink(74,1)=1 And Blink(93,1)=1 And Blink(22,1)=1 then OpenForCompleteorder
	case 8 : If Blink(40,1)=0 And Blink(30,1)=0 And Blink(51,1)=1 And Blink(52,1)=1 And Blink(53,1)=1 And Blink(54,1)=1 And Blink(81,1)=1 And Blink(82,1)=1 And Blink(83,1)=1 And Blink(84,1)=1 And Blink(93,1)=1 And Blink(22,1)=1 then OpenForCompleteorder
	case 9 : If Blink(40,1)=0 And Blink(30,1)=0 And Blink(61,1)=1 And Blink(62,1)=1 And Blink(63,1)=1 And Blink(64,1)=1 And Blink(21,1)=1 And Blink(16,1)=1 And Blink(94,1)=1 And Blink(24,1)=1 then OpenForCompleteorder
	case 10: If Blink(40,1)=0 And Blink(30,1)=0 And Blink(61,1)=1 And Blink(62,1)=1 And Blink(63,1)=1 And Blink(64,1)=1 And Blink(21,1)=1 And Blink(16,1)=1 And Blink(51,1)=1 And Blink(52,1)=1 And Blink(53,1)=1 And Blink(54,1)=1 And Blink(71,1)=1 And Blink(72,1)=1 And Blink(73,1)=1 And Blink(74,1)=1 And Blink(81,1)=1 And Blink(82,1)=1 And Blink(83,1)=1 And Blink(84,1)=1 And Blink(91,1)=1 And Blink(92,1)=1 And Blink(93,1)=1 And Blink(94,1)=1 And Blink(24,1)=1 then OpenForCompleteorder
	End Select
End Sub

Sub OpenForCompleteorder
				DoorF.rotatetoend
				PlaySound "sfx_furnacedoor",1,VolumeDial
				blastfurnaceReady.enabled = True
				BlastdoorOpen = 1
				Blink(121,1)=2
				Blink(122,1)=2
				Blink(123,1)=2
				Blink(124,1)=2
				BLS 121,124,5,5,5,0
				completetheorder
End Sub

Sub blastfurnaceReady_Timer
	bls 136,136,1,5,5,0
	bls 22,24,2,5,8,3
End Sub



Sub add_order_points(pts)
	pts = pts * ( 1 + Blink(102,1) + Blink(103,1))
	bls 102,103,5,10,5,0
	Howmuch = Howmuch + pts
'	debug.Print "howmuch " & howmuch & " added " & pts 
End Sub

Sub ResetForNewMode
	dim axe : axe = array (51,52,53,54,61,62,63,64,71,72,73,74,81,82,83,84,91,92,93,94,11,12,13,14,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,21,22,23,24,110,111,1,2,3,4,5,6,7,8,9,10	)
	dim x
	For Each x in axe : Blink(x,1)=0 : Next
	For x = 1 To PlayerMode(currentplayer)+1 : Blink(x,1)=1 : Next
	If CurrentPlayer = 1 then Playercontrol = "Tweed" Else 	Playercontrol = "Baron" 
	UpdateControl
End Sub


Dim BaronControl
Dim TweedControl
Dim GameMode

Sub StartAttractMode
	ControlLock = False
	balltime = 1000
'controller.B2SSetData 40,1
'controller.B2SSetScorePlayer1 P1score
'controller.B2SSetScoreDigit 1,1

	GiOff
	If DesktopMode then Blink(141,1)=2 : Blink(142,1)=2 : Blink(143,1)=2 : Blink(144,1)=2 : Blink(145,1)=2 : Blink(146,1)=2
	If b2son then
		bbs  8,0,120,40
		bbs  9,0,240,20
		bbs 10,0,240,20
		bbs 11,0,180,30
	End If


	Dim x
	For x = 1 to 140 : Blink(x,1)= 0 : Next
	If VRRoom = 0 Then
		If DesktopMode then
			Light003.state = 0 : Light006.state = 0
			Light005.state = 0 : Light004.state = 0
			Light007.state = 0 : Light008.state = 0
			Light009.state = 2 : Light010.state = 2
			Light011.state = 0 : Light012.state = 0
		End If
	End If
	If b2son then
		bbs 3,0,99999,11
		bbs 4,0,0,0
		bbs 5,0,0,0
		bbs 1,0,0,0
		bbs 2,0,0,0
	End If

	AttractModeTimer.Enabled = True
	attractvoice = 2
	attractmodecounter = 219
	attractScore1 = PlayerScore(1)
	attractScore2 = PlayerScore(2)

	StopBkgd

	If credits > 0 then DOF 113,1
	GameMode = 0 ' attract
	BIP = 0

	StartLightSeq
	BaronControl = 0
	TweedControl = 0
End Sub




Dim attractvoice
Dim attractScore1
Dim attractScore2
Dim attractmodecounter
Dim attractFlashers
AttractModeTimer.interval = 120	
Sub AttractModeTimer_Timer
	attractFlashers = attractFlashers - 1
	if attractFlashers < 0 then
		attractFlashers = int(Rnd(1)*20) : If attractFlashers = 19 Then attractFlashers = 40
		Select Case int(Rnd(1)*6)
			case 0 : bls 135,135,1,4,5,0
			case 1 : bls 136,136,1,4,5,0
			case 2 : bls 137,137,1,4,5,0
			case 3 : bls 138,138,1,4,5,0
			case 4 : bls 135,138,2,4,5,3
			case 5 : bls 138,135,2,4,5,3
		End Select
	End If

	attractmodecounter = attractmodecounter + 1
		Select Case attractmodecounter
'		case  1
'				If DesktopMode and VRRoom = 0 then
'					light003.state = 1 : light004.state = 1
'					light011.state = 1 : light012.state = 1
'					DT_TweedHS.state = 0  : DT_baronHS.state = 0
'				End If
'				If b2son then
'					bbs 1,1,0,0
'					bbs 2,1,0,0
'					bbs 6,0,0,0
'				End If
'				ResetScoreWheels : PlayerScore(1) = attractScore1		  : PlayerScore(2) = attractScore2			: if tournamentplay then BallInPlay = 9 else BallInPlay = 5


		case 25,45,60,75,90,105,120,135,150,165,180,195,210
				If DesktopMode and VRRoom = 0 then
					light003.state = 0 : light004.state = 0
					light011.state = 0 : light012.state = 0
					DT_TweedHS.state = 0  : DT_baronHS.state = 0
					DT_TweedHS2.state = 0 : DT_baronHS2.state = 0
				End If
				If b2son then
					bbs 1,0,0,0
					bbs 2,0,0,0
					bbs 6,0,0,0
					bbs 7,0,0,0
				End If
				ResetScoreWheels : PlayerScore(1) = 0					  : PlayerScore(2) = 0						: BallInPlay = 0

		case 70 : Select Case int(rnd(1) * attractvoice ) 
						case 0 : PlayVoice "vo_premierproducer",1,Voice_Volume * 0.6,0
						case 3 : PlayVoice "vo_takevolkanfromtweed",1,Voice_Volume * 0.6,0
						case 2 : PlayVoice "vo_underattack",1,Voice_Volume * 0.6,0
						case 1 : PlayVoice "vo_volkansmetals",1,Voice_Volume * 0.6	,0
						case 4 : PlayVoice "vo_startthegame",1,Voice_Volume * 0.6,0	
						case 5 : PlayAbigail 0.3			
					End Select
					attractvoice = attractvoice + 5
		Case 30,90
					bls 151,156,11,8,3,2
		case 35,50
				ResetScoreWheels : PlayerScore(1) = HighscoreTweed(2)	  : PlayerScore(2) = Highscorebaron(2)		: BallInPlay = 1
				If DesktopMode  and VRRoom = 0 then DT_TweedHS.state = 2  : DT_baronHS.state = 2
				If b2son then bbs 6,0,10,5


		case 65,80
				ResetScoreWheels : PlayerScore(1) = HighscoreTweed(1)	  : PlayerScore(2) = Highscorebaron(1)		: BallInPlay = 2
				If DesktopMode and VRRoom = 0 then DT_TweedHS.state = 2  : DT_baronHS.state = 2
				If b2son then bbs 6,0,10,5


		case 95,110
				ResetScoreWheels : PlayerScore(1) = HighscoreTweed(0)	  : PlayerScore(2) = Highscorebaron(0)		: BallInPlay = 3 
				If DesktopMode and VRRoom = 0 then DT_TweedHS.state = 2  : DT_baronHS.state = 2
				If b2son then bbs 6,0,10,5


		case 125,140
				ResetScoreWheels : PlayerScore(1) = HighscoreduelTweed(2) : PlayerScore(2) = Highscoreduelbaron(2)	: BallInPlay = 1
				If DesktopMode and VRRoom = 0 then DT_TweedHS2.state = 2 : DT_baronHS2.state = 2 
				If b2son then bbs 7,0,10,5

		case 155,170
				ResetScoreWheels : PlayerScore(1) = HighscoreduelTweed(1) : PlayerScore(2) = Highscoreduelbaron(1)	: BallInPlay = 2
				If DesktopMode and VRRoom = 0 then DT_TweedHS2.state = 2 : DT_baronHS2.state = 2 
				If b2son then bbs 7,0,10,5

		case 185,200
				ResetScoreWheels : PlayerScore(1) = HighscoreduelTweed(0) : PlayerScore(2) = Highscoreduelbaron(0)	: BallInPlay = 3 
				If DesktopMode and VRRoom = 0 then DT_TweedHS2.state = 2 : DT_baronHS2.state = 2 
				If b2son then bbs 7,0,10,5

		case  220
				If DesktopMode and VRRoom = 0 then
					light003.state = 1 : light004.state = 1
					light011.state = 1 : light012.state = 1
					DT_TweedHS.state = 0  : DT_baronHS.state = 0
				End If
				If b2son then
					bbs 1,1,0,0
					bbs 2,1,0,0
					bbs 6,0,0,0
				End If
				ResetScoreWheels : PlayerScore(1) = attractScore1		  : PlayerScore(2) = attractScore2			: if tournamentplay then BallInPlay = 9 else BallInPlay = 5





		case 280
			attractmodecounter = 0
		End Select

End Sub

Sub StopAttractMode

	If DesktopMode and VRRoom = 0 then
		Blink(141,1)=0 : Blink(142,1)=0 : Blink(143,1)=2 : Blink(144,1)=0 : Blink(145,1)=0 : Blink(146,1)=0
		DT_TweedHS.state = 0	: DT_baronHS.state = 0
		DT_TweedHS2.state = 0	: DT_baronHS2.state = 0
		Light009.state = 0 : Light010.state = 0
		Light003.state = 2 : Light004.state = 2	
	End If
	If b2son then
		bbs  8,0,0,0
		bbs  9,0,99999,9
		bbs 10,0,0,0
		bbs  2,0,0,0
		bbs 11,0,0,0
		bbs  6,0,0,0
		bbs  7,0,0,0
		bbs  3,0,0,0		' gameover offf
		bbs  1,0,99999,9	' tweed select blinking
	End If

	Playerselect = True
	PlayerChoosen = "Tweed"

	AttractModeTimer.Enabled = False

	ResetScoreWheels
	PlayerScore(1)=0
	PlayerScore(2)=0

	GameMode = 2 ' start new game   1=startgame no flippers work ...: 2 =? flipper working
	stopsound "attractsong"
	LightSeqAttract.StopPlay 
	LightSeqAttract2.StopPlay
 
	ResetForNewGame
	PlaySound "SubWayRelease" ,1,VolumeDial

	
	Playercontrol = "Tweed" ' Player 1
	UpdateControl
	Blink(104,1) = 1
	ballsave = 5 + MaxBallsave
'	debug.print "BallSave15sec"

	ballsaveActivated = 0

	SteamElct_Vo = 0

	PlaySound "Steampunk_Bkgd1" ,-1,Music_Volume*0.7


End Sub

Sub LRampEnd_hit
	bendLeft = 0
	PlaySoundAt "fx_wirerampexit",LRampEnd
End Sub
Dim balltime 
balltime = 0
Dim Balltimecounter
Balltimecounter = 0

Dim bendLeft
Sub Trigger002_hit
	bendLeft = 1
End Sub

Dim bendRight
Sub Trigger001_hit
	bendRight = 1
End Sub

Sub Trigger003_hit : RampWireRight.x = 0.1 : Light030.state = 0 : End Sub
Sub Trigger004_hit : RampWireRight.x = -.1 : End Sub
Sub Trigger005_hit : RampWireRight.x = 0.1 : End Sub
Sub Trigger006_hit : RampWireRight.x = -.1 : End Sub
Sub Trigger007_hit : RampWireRight.x =   0 : End Sub
Sub Trigger008_hit :  RampWireLeft.x = -.1 : End Sub
Sub Trigger009_hit :  RampWireLeft.x = 0.1 : End Sub
Sub Trigger010_hit :  RampWireLeft.x =   0 : End Sub



Dim LrampPos
Dim RrampPos
Sub AnimRamps
	If bendRight = 1 Then
		RrampPos = RrampPos + 10
		If RrampPos > 100 then RrampPos = 100
	Else
		RrampPos = RrampPos - 25
		If RrampPos < -20 then RrampPos = 0
	End If
'	If RrampPos <> 0 then Debug.print "R " & RrampPos
	RampWireRight001.rotx = -.2 * RrampPos / 100
	RampWireRight001.z = 4 * RrampPos / 100
	RampWireRight001.x = .2  * RrampPos / 100
	RampWireRight001.y = -.2 * RrampPos / 100

	If bendLeft = 1 Then
		LrampPos = LrampPos + 10
		If LrampPos > 100 then LrampPos = 100
	Else
		LrampPos = LrampPos - 25
		If LrampPos < -20 then LrampPos = 0
	End If
'	If LrampPos <> 0 then Debug.print "L " & LrampPos
	RampWireLeft002.rotx = -.2 * LrampPos / 100
	RampWireLeft002.z=4 * LrampPos / 100
'	RampWireLeft002.x=0 * LrampPos / 100
	RampWireLeft002.y=-.3 * LrampPos / 100
End Sub



Sub RRampEnd_Hit
	bendRight = 0
	PlaySoundAt "fx_wirerampexit",RRampEnd

	If Ballsave > -2 then ballsaveActivated = 1

	If Blink(29,1) = 1 Then
		Blink(29,1) = 2
		ClearVoiceQ
		Select Case int(rnd(1)*3)
			Case 0 : PlayVoice  "vo_doublecatchup",1,Voice_Volume,0
			Case 1 : PlayVoice  "vo_doubleproduction",1,Voice_Volume,0
			Case 2 : PlayVoice  "vo_doublecounting2",1,Voice_Volume,0
		End Select
		PlayVoice "vo_shootcarbon",1,Voice_Volume,1
	End If

	If HurryupLevel2(currentplayer) = 1 Then 
		HurryupLevel2(currentplayer) = 2
		LevelHurryupTimer.enabled = True
		ClearVoiceQ
		PlayVoice  "vo_shoottwice2",1,Voice_Volume,0
	End If


	If PlayerMode(currentplayer) = 4 Then 
		If SkipMBstart5(CurrentPlayer) = 0 Then
			SkipMBstart5(CurrentPlayer) = 1
			Blink(104,1) = 0
			Blink(18,1) = 0
			Blink(19,1) = 0
			multiballtimer.enabled = False
			multiballtimer2.enabled = False
		End If
	End If

	If PlayerMode(currentplayer) = 9 Then 
		If SkipMBstart10(CurrentPlayer) = 0 Then
			SkipMBstart10(CurrentPlayer) = 1
			Blink(104,1) = 0
			Blink(18,1) = 0
			Blink(19,1) = 0
			multiballtimer.enabled = False
			multiballtimer2.enabled = False
		End If
	End If

	If Blink(104,1) = 1 Then
		balltime = 0
		Balltimecounter = 0
		Blink(104,1) = 2 'multiball clock light
		Blink(18,1) = 2	 'left PostUp light
		PlaySound "sfx_clocktimer",1,Voice_Volume
		multiballtimer.enabled = False
		multiballtimer.enabled = True

		If FirstMultiball(CurrentPlayer) = 0 Then
			PlayVoice "vo_whitearrows",1,Voice_Volume,0
		Else
			Select Case int(rnd(1)*2)
				Case 0 : PlayVoice "vo_mustarrow",1,Voice_Volume,0
				Case 1 : PlayVoice "vo_shootarrow2",1,Voice_Volume,0
			End Select
		End If
	End If

End Sub		


Sub SUT6_hit	'left PostUp SU
	DOF 131,2
	RandomSoundTargetHit
	If Tilted then Exit Sub

	PlaySound "sfx_postsup",1,VolumeDial
	If blink(18,1) = 2 and blink(104,1) = 2 then
		AddScore 300
		blink(18,1) = 1 
		blink(19,1) = 2		'right PostUp SU
	Elseif blink(18,1) = 2 and ControlLock = true then
		Blink(18,1)=1
		Blink(19,1)=2
		addscore 200
	Else
		AddScore 20
	End If
End Sub


Sub SUT7_hit 	'right PostUp SU_
	DOF 131,2
	RandomSoundTargetHit
	If Tilted then Exit Sub

	PlaySound "sfx_postsup",1,VolumeDial
	If blink(19,1) = 2  and blink(104,1) = 2 then
		AddScore 300
		blink(19,1) = 1
		Blink(104,1) = 0
		multiballtimer.enabled = False
		multiballtimer2.enabled = True
		Blink(17,1) = 2  
		bls 17,17,8,7,7,0

		StopSound "sfx_clocktimer"
		PlaySound "sfx_awardbonus",1,VolumeDial

		If FirstMultiball(CurrentPlayer) = 0 Then
			FirstMultiball(CurrentPlayer) = 1
			PlayVoice "vo_blaststartmb",1,Voice_Volume,1
		Else
			PlayVoice "vo_postsup",1,Voice_Volume,1 
		End If

	Elseif blink(19,1) = 2 and ControlLock = true then
		Blink(19,1)=1
		ClearVoiceQ
		Select Case int(rnd(1)*4)
			Case 0 : PlayVoice "vo_moretime",1,Voice_Volume,0
			Case 1 : PlayVoice "vo_overtime",1,Voice_Volume,0
			Case 2 : PlayVoice "vo_timeextended",1,Voice_Volume,0
			Case 3 : PlayVoice "vo_timeextended",1,Voice_Volume,0
		End Select
		Ballsave = Ballsave + 12
'		debug.print "BallSaveAdded 10 sec"

		addscore 200
	Else
		AddScore 20
	End If
End Sub

Sub multiballtimer2_Timer ' flashing ramp for Multiball
	bls 135,135,1,5,5,0
	bls 17,17,3,4,4,0
End Sub

Sub multiballtimer_Timer
	multiballtimer.enabled = False

	blink(18,1) = 0 
	blink(19,1) = 0
	Blink(104,1) = 0
	Blink(17,1) = 0
	PlaySound "sfx_shutdown",1,VolumeDial
	wall003.collidable = False
	wall004.collidable = False

' fixing start mode1 here
End Sub


Sub ballcolortest

	dim x
'	dim bot
'	bot = getballs
	For x = 0 to 4'UBound(BOT)
		gbot(x).color=rgb(250,40,10)
	Next
End Sub


'Sub Backgroundmusic_Timer 
'    Select Case INT(5 * RND(1) ) 
'		Case 0:PlaySound "Steampunk_Bkgd1",1,Music_Volume*0.4 : Backgroundmusic.enabled=true:Backgroundmusic.interval=239000 
'		Case 1:PlaySound "Steampunk_Bkgd2",1,Music_Volume*0.4 : Backgroundmusic.enabled=true:Backgroundmusic.interval=230700 
'		Case 2:PlaySound "Steampunk_Bkgd3",1,Music_Volume*0.4 : Backgroundmusic.enabled=true:Backgroundmusic.interval=204800 
'		Case 3:PlaySound "Steampunk_Bkgd4",1,Music_Volume*0.4 : Backgroundmusic.enabled=true:Backgroundmusic.interval=223300 
'		Case 4:PlaySound "Steampunk_Bkgd5",1,Music_Volume*0.4 : Backgroundmusic.enabled=true:Backgroundmusic.interval=240200 
'	End Select 
'	PlaySound "sfx_SqueakyGear" ,1,VolumeDial
'End sub 



Sub StopBkgd 
'	Backgroundmusic.enabled=False
	StopSound "Steampunk_Bkgd1" 
	StopSound "Steampunk_Bkgd2" 
	StopSound "Steampunk_Bkgd3" 
	StopSound "Steampunk_Bkgd4" 
	StopSound "Steampunk_Bkgd5" 
End Sub 
	


Sub StartLightSeq
		PlaySound "attractsong",1,Music_Volume*0.2
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqUpOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqDownOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqCircleOutOn, 15, 2 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqUpOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqDownOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqUpOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqDownOn, 25, 1 
		LightSeqAttract.UpdateInterval = 10 
		LightSeqAttract.Play SeqCircleOutOn, 15, 3 
		LightSeqAttract.UpdateInterval = 5 
		LightSeqAttract.Play SeqRightOn, 50, 1 
		LightSeqAttract.UpdateInterval = 5 
		LightSeqAttract.Play SeqLeftOn, 50, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqRightOn, 50, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 50, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqRightOn, 40, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 40, 1 
		LightSeqAttract.UpdateInterval = 10 
		LightSeqAttract.Play SeqRightOn, 30, 1 
		LightSeqAttract.UpdateInterval = 10 
		LightSeqAttract.Play SeqLeftOn, 30, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqRightOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqRightOn, 15, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 15, 1 
		LightSeqAttract.UpdateInterval = 10 
		LightSeqAttract.Play SeqCircleOutOn, 15, 3 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqRightOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqUpOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqDownOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqUpOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqDownOn, 25, 1 
		LightSeqAttract.UpdateInterval = 5 
		LightSeqAttract.Play SeqStripe1VertOn, 50, 2 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqCircleOutOn, 15, 2 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqStripe1VertOn, 50, 3 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqRightOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqUpOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqDownOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqCircleOutOn, 15, 2 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqStripe2VertOn, 50, 3 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqRightOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqUpOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqDownOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqUpOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqDownOn, 25, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqStripe1VertOn, 25, 3 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqStripe2VertOn, 25, 3 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqUpOn, 15, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqDownOn, 15, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqUpOn, 15, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqDownOn, 15, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqUpOn, 15, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqDownOn, 15, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqRightOn, 15, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 15, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqRightOn, 15, 1 
		LightSeqAttract.UpdateInterval = 8 
		LightSeqAttract.Play SeqLeftOn, 15, 1 
End Sub
'' gi 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqUpOn, 50, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqDownOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqCircleOutOn, 15, 2 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqUpOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqDownOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqUpOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqDownOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 10 
'		LightSeqAttract2.Play SeqCircleOutOn, 15, 3 
'		LightSeqAttract2.UpdateInterval = 5 
'		LightSeqAttract2.Play SeqRightOn, 50, 1 
'		LightSeqAttract2.UpdateInterval = 5 
'		LightSeqAttract2.Play SeqLeftOn, 50, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqRightOn, 50, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 50, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqRightOn, 40, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 40, 1 
'		LightSeqAttract2.UpdateInterval = 10 
'		LightSeqAttract2.Play SeqRightOn, 30, 1 
'		LightSeqAttract2.UpdateInterval = 10 
'		LightSeqAttract2.Play SeqLeftOn, 30, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqRightOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqRightOn, 15, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 15, 1 
'		LightSeqAttract2.UpdateInterval = 10 
'		LightSeqAttract2.Play SeqCircleOutOn, 15, 3 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqRightOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqUpOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqDownOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqUpOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqDownOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 5 
'		LightSeqAttract2.Play SeqStripe1VertOn, 50, 2 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqCircleOutOn, 15, 2 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqStripe1VertOn, 50, 3 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqRightOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqUpOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqDownOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqCircleOutOn, 15, 2 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqStripe2VertOn, 50, 3 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqRightOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqUpOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqDownOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqUpOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqDownOn, 25, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqStripe1VertOn, 25, 3 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqStripe2VertOn, 25, 3 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqUpOn, 15, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqDownOn, 15, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqUpOn, 15, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqDownOn, 15, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqUpOn, 15, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqDownOn, 15, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqRightOn, 15, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 15, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqRightOn, 15, 1 
'		LightSeqAttract2.UpdateInterval = 8 
'		LightSeqAttract2.Play SeqLeftOn, 15, 1 
''	End Sub 

Dim lutchanger
Sub Lutflasher_Timer
	lutchanger = lutchanger + 1
	Select Case lutchanger
	case 1,3,5,7,9,11
		table1.ColorGradeImage = "ColorGradeLUT256x16_1t"
	case 2,4,6,8,10,12
		table1.ColorGradeImage = "LUT_Onevox"
	case 13 : lutchanger = 0
		Lutflasher.enabled = False
	End Select
End Sub

Dim blasttriggerON
Sub blasttrigger_Hit
	blasttriggerON = 1
End Sub


Sub LevelHurryupTimer_Timer
	LevelHurryupTimer.enabled = False
	HurryupLevelC = 0 
	HurryupLevel2(currentplayer) = 10 ' 10 = failed

	Blink(125,1)=0
	Blink(126,1)=0
	Blink(127,1)=0
	Blink(128,1)=0

	If BlastdoorMB = 0 Then
		DoorF.rotatetostart
		PlaySound "sfx_furnacedoor",1,VolumeDial
		blastfurnaceReady.enabled = False
		Blink(121,1)=0
		Blink(122,1)=0
		Blink(123,1)=0
		Blink(124,1)=0

	End If
	ClearVoiceQ
	PlayVoice  "vo_blastclosing",1,Voice_Volume,0
End Sub


Dim MaxMB_JPs
BlastVUK.timerinterval = 3000
Sub BlastVUK_hit
	DOF 123,2
	RandomSoundEjectHoleEnter BlastVUK
	PlaySoundat "sfx_furnaceenter",blastvuk

	If Tilted Then BlastVUK.timerenabled = True : Exit Sub
		
	Light030.state = 2	
	If DesktopMode then bls 141,146,2,5,5,2 
	if b2son then
		bbs  9,-1,2,6
		bbs 10,-1,2,7
		bbs 11,-1,2,5
		bbs  8,-1,2,4
	End If
	bls 117,128,4,5,10,1
	LightSeqAttract.UpdateInterval = 5
	LightSeqAttract.Play Seqblinking ,,5, 22


	Blink(134,1)=0
	Blink(133,1)=1
	bls 134,134,1,7,7,0
	bls 133,133,3,7,7,15

	bls 151,156,11,8,3,2

	If HurryupLevel2(currentplayer) = 2 And blasttriggerON = 1 Then
		Addscore 500
	
		HurryupLevelC = HurryupLevelC + 1
'		debug.print "Hurryup BlastHits = " & HurryupLevelC
		BlastVUK.timerinterval = 1000
		If HurryupLevelC = 1 Then PlaySound "sfx_bigsteam",1,Voice_Volume

		If HurryupLevelC = 2 Then

			Blink(125,1)=0
			Blink(126,1)=0
			Blink(127,1)=0
			Blink(128,1)=0
			BLS 125,128,5,7,7,0

			PlayVoice  "vo_goodwork",1,Voice_Volume,0 : PlaySound "sfx_bigsteam2",1,Voice_Volume

'			debug.print "Hurryup Complete"

			HurryupLevelC = 0 
			HurryupLevel2(currentplayer) = 0 ' 0 = complete
			LevelHurryupTimer.enabled = False

			blasttriggerON = 0
			If DesktopMode then Lutflasher.enabled = True
			LightSeqAttract.stopplay
			LightSeqAttract.UpdateInterval = 8
			LightSeqAttract.Play Seqblinking ,,1, 10
			LightSeqAttract.Play Seqblinking ,,1, 15
			LightSeqAttract.Play Seqblinking ,,1, 20
			LightSeqAttract.Play Seqblinking ,,1, 25
			LightSeqAttract.Play Seqblinking ,,1, 30
			LightSeqAttract.Play SeqUpOn, 30, 2 

			If BlastdoorMB = 0 then
				DoorF.rotatetostart
				Blink(121,1)=0
				Blink(122,1)=0
				Blink(123,1)=0
				Blink(124,1)=0
				PlaySound "sfx_furnacedoor",1,VolumeDial
				blastfurnaceReady.enabled = False
			End If
			BlastdoorOpen = 0
			Blink(121,1)=0
			Blink(122,1)=0
			Blink(123,1)=0
			Blink(124,1)=0
			BLS 121,124,5,5,5,0
			Blink(22,1) = 0 
			Blink(23,1) = 0
			Blink(24,1) = 0
			bls 22,24,5,5,7,1
			ResetForNewMode
			DT1.timerenabled = True
			DTShadow001.visible = 0
			DTShadow002.visible = 0
			DTShadow003.visible = 0
			DTShadow004.visible = 0
			dt1.isdropped = true
			dt2.isdropped = true
			dt3.isdropped = true
			dt4.isdropped = true
	'		If DroppedDT(1)=0 then PlaySoundAt "fx_target",DT1
	'		If DroppedDT(2)=0 then PlaySoundAt "fx_target",DT2
	'		If DroppedDT(3)=0 then PlaySoundAt "fx_target",DT3
	'		If DroppedDT(4)=0 then PlaySoundAt "fx_target",DT4

			SteamCounter = 0
			ElectricCounter = 0
			Bumper1Counter = 0
			Bumper2Counter = 0
			Bumper3Counter = 0
			Bumper4Counter = 0
			RaiseTemp = 0

			If CurrentPlayer = 1 then Playercontrol = "Tweed" Else Playercontrol = "Baron"
			UpdateControl
			DOF 129,2
			Select Case PlayerMode(CurrentPlayer)
				Case 0 : bls 1,1,5,10,5,0 :  add_order_points 5000 : PlayerMode(currentplayer) = 1 : Startmode ' onto Next
				Case 1 : bls 2,2,5,10,5,0 :  add_order_points 7500 : PlayerMode(currentplayer) = 2 : Startmode 
				Case 2 : bls 3,3,5,10,5,0 : add_order_points 10000 : PlayerMode(currentplayer) = 3 : Startmode 
				Case 3 : bls 4,4,5,10,5,0 : add_order_points 15000 : PlayerMode(currentplayer) = 4 : Startmode 
				Case 4 : bls 5,5,5,10,5,0 : add_order_points 20000 : PlayerMode(currentplayer) = 5 : Startmode 
				Case 5 : bls 6,6,5,10,5,0 : add_order_points 10000 : PlayerMode(currentplayer) = 6 : Startmode 
				Case 6 : bls 7,7,5,10,5,0 : add_order_points 15000 : PlayerMode(currentplayer) = 7 : Startmode 
				Case 7 : bls 8,8,5,10,5,0 : add_order_points 20000 : PlayerMode(currentplayer) = 8 : Startmode 
				Case 8 : bls 9,9,5,10,5,0 : add_order_points 30000 : PlayerMode(currentplayer) = 9 : Startmode 
				Case 9 :bls 10,10,5,10,5,0: add_order_points 40000 : PlayerMode(currentplayer) = 10: Startmode 
				Case 10:bls 10,10,5,10,5,0: add_order_points 172500 : PlayerMode(currentplayer) = 0: Startmode
			End Select
			ClearVoiceQ



			bbs 17, 1, 20, 5
			bbs 13, 1, 5, 15
			bbs 14, 1, 5, 16
			bbs 15, 1, 5, 17
			bbs 16, 1, 5, 18
			Select Case PlayerMode(currentplayer) = 1
				Case 2,3 : PlayVoice  "vo_brilliant",1,Voice_Volume,0
				Case 4 : PlayVoice  "vo_outstanding",1,Voice_Volume,0

				Case 6,7,8,9 : PlayVoice  "vo_magnificent",1,Voice_Volume,0


			End Select
		End If

		If BlastdoorOpen = 0 And BlastdoorMB = 0 then blasttriggerON = 0
	End If



	If BlastdoorMB = 1 And blasttriggerON = 1 THEN
		BlastVUK.timerinterval = 1000
		Playsoundat "sfx_awardbonus",BlastVUK
		playsound "sfx_jackpot",1,volumedial : bbs 17, 1, 20, 5

		If CurrentPlayer = 1 then Playercontrol = "Tweed" Else Playercontrol = "Baron"
		UpdateControl

		Howmuch = Howmuch + 500 + PlayerMode(currentplayer) * 200 + MaxMB_JPs * 100
		LightSeqAttract.Play SeqUpOn, 30, 2
		MaxMB_JPs = MaxMB_JPs -1 
		If MaxMB_JPs < 0 then
			MaxMB_JPs = 0
'			BlastdoorMB = 0
'			If BlastdoorOpen = 0 then
'				doorf.rotatetostart
'				PlaySound "sfx_furnacedoor",1,VolumeDial
'				blastfurnaceReady.enabled = False
'			End If
		End If
		If BlastdoorOpen = 0 then blasttriggerON = 0
	End If


	If BlastdoorOpen = 1 And blasttriggerON = 1 then
		blasttriggerON = 0
		If DesktopMode then Lutflasher.enabled = True
		LightSeqAttract.stopplay
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play Seqblinking ,,1, 10
		LightSeqAttract.Play Seqblinking ,,1, 15
		LightSeqAttract.Play Seqblinking ,,1, 20
		LightSeqAttract.Play Seqblinking ,,1, 25
		LightSeqAttract.Play Seqblinking ,,1, 30
		LightSeqAttract.Play SeqUpOn, 30, 2 

		If BlastdoorMB = 0 then
			DoorF.rotatetostart
			Blink(121,1)=0
			Blink(122,1)=0
			Blink(123,1)=0
			Blink(124,1)=0


			PlaySound "sfx_furnacedoor",1,VolumeDial
			blastfurnaceReady.enabled = False
		End If
		BlastdoorOpen = 0
		BLS 121,124,5,5,5,0
		Blink(22,1) = 0 
		Blink(23,1) = 0
		Blink(24,1) = 0
		bls 22,24,5,5,7,1
		ResetForNewMode
		DT1.timerenabled = True
		DTShadow001.visible = 0
		DTShadow002.visible = 0
		DTShadow003.visible = 0
		DTShadow004.visible = 0
		dt1.isdropped = true
		dt2.isdropped = true
		dt3.isdropped = true
		dt4.isdropped = true
'		If DroppedDT(1)=0 then PlaySoundAt "fx_target",DT1
'		If DroppedDT(2)=0 then PlaySoundAt "fx_target",DT2
'		If DroppedDT(3)=0 then PlaySoundAt "fx_target",DT3
'		If DroppedDT(4)=0 then PlaySoundAt "fx_target",DT4

		SteamCounter = 0
		ElectricCounter = 0
		Bumper1Counter = 0
		Bumper2Counter = 0
		Bumper3Counter = 0
		Bumper4Counter = 0
		RaiseTemp = 0

		If CurrentPlayer = 1 then Playercontrol = "Tweed" Else Playercontrol = "Baron"
		UpdateControl
		DOF 129,2
		Select Case PlayerMode(CurrentPlayer)
			Case 0 : bls 1,1,5,10,5,0 :  add_order_points 5000 : PlayerMode(currentplayer) = 1 : Startmode ' onto Next
			Case 1 : bls 2,2,5,10,5,0 :  add_order_points 7500 : PlayerMode(currentplayer) = 2 : Startmode 
			Case 2 : bls 3,3,5,10,5,0 : add_order_points 10000 : PlayerMode(currentplayer) = 3 : Startmode 
			Case 3 : bls 4,4,5,10,5,0 : add_order_points 15000 : PlayerMode(currentplayer) = 4 : Startmode 
			Case 4 : bls 5,5,5,10,5,0 : add_order_points 20000 : PlayerMode(currentplayer) = 5 : Startmode 
			Case 5 : bls 6,6,5,10,5,0 : add_order_points 10000 : PlayerMode(currentplayer) = 6 : Startmode 
			Case 6 : bls 7,7,5,10,5,0 : add_order_points 15000 : PlayerMode(currentplayer) = 7 : Startmode 
			Case 7 : bls 8,8,5,10,5,0 : add_order_points 20000 : PlayerMode(currentplayer) = 8 : Startmode 
			Case 8 : bls 9,9,5,10,5,0 : add_order_points 30000 : PlayerMode(currentplayer) = 9 : Startmode 
			Case 9 :bls 10,10,5,10,5,0: add_order_points 40000 : PlayerMode(currentplayer) = 10: Startmode 
			Case 10:bls 10,10,5,10,5,0: add_order_points 172500 : PlayerMode(currentplayer) = 0: AfterWizard(CurrentPlayer)= 1 :  Startmode
		End Select
		PlayVoice  "vo_welldone",1,Voice_Volume,0
		bbs 17, 1, 20, 5
		bbs 13, 1, 5, 15
		bbs 14, 1, 5, 16
		bbs 15, 1, 5, 17
		bbs 16, 1, 5, 18

	Else
		addscore 100	
	End If



	bls 136,136,3,5,7,0
	gioff
	BlastVUK.timerenabled = True
	L150.State=2 
	L150.BlinkPattern =100000		 'pattern for blinking lights on,off,on,on,on,on,off
	L150.BlinkInterval=250		 'Timer for lights on & off 250 = 1/4 of a second.
	L151.State=2 
	L151.BlinkPattern =010000	 'pattern for blinking lights on,off,on,on,on,on,off
	L151.BlinkInterval=200		 'Timer for lights on & off 250 = 1/4 of a second.
	L152.State=2 
	L152.BlinkPattern =001000		 'pattern for blinking lights on,off,on,on,on,on,off
	L152.BlinkInterval=150		 'Timer for lights on & off 250 = 1/4 of a second.
	L153.State=2 
	L153.BlinkPattern =000100	 'pattern for blinking lights on,off,on,on,on,on,off
	L153.BlinkInterval=200		 'Timer for lights on & off 250 = 1/4 of a second.
	L154.State=2 
	L154.BlinkPattern =-000010	 'pattern for blinking lights on,off,on,on,on,on,off
	L154.BlinkInterval=250		 'Timer for lights on & off 250 = 1/4 of a second.
	L155.State=2 
	L155.BlinkPattern =000001		 'pattern for blinking lights on,off,on,on,on,on,off
	L155.BlinkInterval=150		 'Timer for lights on & off 250 = 1/4 of a second.



End Sub


Sub BlastVUK_Timer
	BlastVUK.timerinterval = 3000
	BlastVUK.timerenabled = False
	BlastVUK.kickz 90,50,12,141
	RandomSoundEjectHoleSolenoid BlastVUK
	PlaySound "sfx_ramprightshort",1,volumedial * 0.9
	L155.state=0
	L154.state=0
	L153.state=0
	L152.state=0
	L151.state=0
	L150.state=0
'fixing	RedFlasher Flash
	If Tilted Then Exit Sub

	GiOn
	bls 136,136,2,4,4,10
End Sub



Function ExtractRGB(col)
  Dim red, green, blue
  red = col And &HFF
  green = (col \ &H100) And &HFF
  blue = (col \ &H10000) And &HFF
  ExtractRGB = Array(red, green, blue)
End Function


Dim VRFL
Dim gilvl:gilvl = 0

Sub GiOn
	dim x, b
	gilvl = 1
	DOF 135,1
	If Playercontrol = "Tweed" then
		DOF 132,1
		DOF 133,0
	Else
		DOF 132,0
		DOF 133,1
	End If

	For b = 0 To UBound(gBOT)
		gBOT(b).color = BallColors(b)
	Next

	SteamSpinner.timerenabled = True
	ElectricSpinner.timerenabled = True

	If b2son then bbs 8,1,0,0
	If desktopmode then
		bls 141,141,1,4,7,0
		bls 144,146,3,4,7,2
		Blink(144,1)=1
		Blink(145,1)=1
		Blink(146,1)=1
	End If

	For Each x In GI 
		x.state=1
	Next
	If VRRoom > 0 Then For each VRFL in VRBackglassGI : VRFL.visible = 1 : Next
	Blink(115,1)=1
	Blink(116,1)=1

End Sub


Sub GiOff
	dim x, b, cRGB
	gilvl = 0 
	DOF 135,0
	DOF 132,0
	DOF 133,0

	For b = 0 To UBound(gBOT)
		cRGB = ExtractRGB(CDbl(BallColors(b)))
		cRGB(0) = CInt(cRGB(0)*BallDimScale)
		cRGB(1) = CInt(cRGB(1)*BallDimScale)
		cRGB(2) = CInt(cRGB(2)*BallDimScale)
		gBOT(b).color = rgb(cRGB(0), cRGB(1), cRGB(2))
	Next
	
	SteamSpinner.timerenabled = True
	ElectricSpinner.timerenabled = True

	If b2son then bbs 8,0,0,0
	If desktopmode then
		bls 141,141,4,4,7,0   ' abigail
		Blink(144,1)=0
		Blink(145,1)=0
		Blink(146,1)=0
	End If

	
	For Each x In GI 
		x.state=0
	Next
	If VRRoom > 0 Then For each VRFL in VRBackglassGI : VRFL.visible = 0 : Next
	Blink(115,1)=0
	Blink(116,1)=0
End Sub

Dim b2sblinking(20,5)
Sub Blink_B2s
	dim x,x2
	For x = 1 to 20
		If b2sblinking(x,3) > 0 then
			b2sblinking(x,4) = b2sblinking(x,4) - 1 
			If b2sblinking(x,4) = 0 then
				b2sblinking(x,4) = 	b2sblinking(x,5)
				b2sblinking(x,3) = b2sblinking(x,3) - 1
				If b2sblinking(x,3) = 0 then
					b2sblinking(x,2)=b2sblinking(x,1)' orginal state if needed
				Else
					if b2sblinking(x,2) = 1 then b2sblinking(x,2)=0 else b2sblinking(x,2)=1
				End If
			End If
		End If
		If b2sblinking(x,0) <> b2sblinking(x,2) then
			b2sblinking(x,0) =  b2sblinking(x,2)
			If b2sblinking(x,0) = 1 then x2 = 1 Else x2 = 0
			Select Case x
				case  1 : if VRRoom > 0 Then bbs001.state = x2 Else controller.B2SSetData 54,x2
				case  2 : if VRRoom > 0 Then bbs002.state = x2 Else controller.B2SSetData 55,x2
				case  3 : if VRRoom > 0 Then bbs003.state = x2 Else controller.B2SSetData 35,x2
				case  4 : if VRRoom > 0 then bbs004.state = x2 Else controller.B2SSetData 33,x2
				case  5 : if VRRoom > 0 Then bbs005.state = x2 Else controller.B2SSetData 36,x2
				case  6 : if VRRoom > 0 Then bbs006.state = x2 Else controller.B2SSetData 50,x2 : controller.B2SSetData 53,x2 : End If
				case  7 : if VRRoom > 0 Then bbs007.state = x2 Else controller.B2SSetData 51,x2 : controller.B2SSetData 52,x2 : End If
				case  8 : if vrroom > 0 then bbs008.state = x2 Else controller.B2SSetData 40,x2 : controller.B2SSetData 60,x2 : End If
				case  9 : if vrroom > 0 then bbs009.state = x2 Else controller.B2SSetData 41,x2
				case 10 : if vrroom > 0 then bbs010.state = x2 Else controller.B2SSetData 42,x2
				case 11 : if vrroom > 0 then bbs011.state = x2 Else controller.B2SSetData 43,x2
				case 12 : if x2 = 1 then VRBGFLTweed1 x2
				case 13 : if x2 = 1 then VRBGFLTweed2 x2
				case 14 : if x2 = 1 then VRBGFLBaron1 x2
				case 15 : if x2 = 1 then VRBGFLBaron2 x2
				case 16 : if x2 = 1 then VRBGFLAbigail x2	
				case 17 : if x2 = 1 then VRBGFLVOL x2
			End Select
		End If
	Next
End Sub
Sub BBS(light,state,nr,time)
	if state > -1 then b2sblinking(light,1) = state
	if state > -1 then b2sblinking(light,2) = state
	b2sblinking(light,3) = nr
	b2sblinking(light,4) = time
	b2sblinking(light,5) = time
End Sub


Dim OldGIIntensity
Dim FLst(4)
Sub GIstuff

	dim x,i
	x = FL1+FL2+FL3+FL4
	if x > 1 then x = 1
	overlay002.opacity = x * 350


	If FL1 = 1 And FLst(1) = 0 then FLst(1) = 1 : DOF 112,2 
	If FL1 = 0 Then FLst(1) = 0
	If FL2 = 1 And FLst(2) = 0 then FLst(2) = 1 : DOF 111,2 
	If FL2 = 0 Then FLst(2) = 0
	If FL3 = 1 And FLst(3) = 0 then FLst(3) = 1 : DOF 110,2 
	If FL3 = 0 Then FLst(3) = 0
	If FL4 = 1 And FLst(4) = 0 then FLst(4) = 1 : DOF 109,2 
	If FL4 = 0 Then FLst(4) = 0



	x = GI111.getinplayintensity
	FlasherStokeNEW.blenddisablelighting = FL1 * 2 + 1 + x / 6
	FlasherBlast.blenddisablelighting = FL2 * 7 + 1 + x / 6
	FlasherElectric.blenddisablelighting = FL3 * 2 + 1+ x /6
	FlasherSteam001.blenddisablelighting = FL4 *7 +1 + x / 6
	CoverTest.blenddisablelighting = 0.2 + volkanlight.getinplayintensity / 4

'	If x <> OldGIIntensity Then
'		OldGIIntensity = x
'	Else
'		Exit Sub
'	End If

	' Layer Walls
	OuterWall.blenddisablelighting = (15-x)/23
	LowWalls.blenddisablelighting = (15-x)/133 ' = 2 
	ElectricWall.blenddisablelighting = x/15 ' = 1 
	CoalWall.blenddisablelighting = x/15 ' = 1 
	SteamWall.blenddisablelighting = x/15 + Light038.getinplayintensity
	' pf plastic and Apron
	PlungerCover.blenddisablelighting = x/144 
	pf_holes.blenddisablelighting = x/44 
	Plastics.blenddisablelighting = x/6 + 0.5
	Apron.blenddisablelighting = x/123
	ApronWires.blenddisablelighting = x/150 + 0.1
	Flasher001.color = RGB(23+x,19+x,17+x)
	GaugeRightFlasher.color = RGB(102+x,83+x,62+x)
	GaugeLeftFlasher.color = RGB(102+x,83+x,62+x)
	'insertlights
	WheelRORightOut.blenddisablelighting = x/70 ' = 0.5 
	WheelRORightIn.blenddisablelighting = x/70 ' = 0.5 
	WheelROLeftIn.blenddisablelighting = x/70 ' = 0.5 
	WheelROLeftout.blenddisablelighting = x/70 ' = 0.5 
	Overlay001.opacity =  105 - (x * 16.6 )  ' gioff  
	' RampsRaILSwIRE
	WireGuides.blenddisablelighting = x/100 ' = 0.15 
	RampWireLeft.blenddisablelighting = x/150 ' = 0.1   
	RampWireLeft002.blenddisablelighting = x/150 ' = 0.1  
	RampWireRight.blenddisablelighting = x/150 ' = 0.1  
	RampWireRight001.blenddisablelighting = x/150 ' = 0.1 
	PlungerRails.blenddisablelighting = x/7.5 + 1.5 ' = 2
	GIBulbs.blenddisablelighting = x*4 ' = 30
	Lightbox.blenddisablelighting = x/10 + 1 ' trainsignal

	'UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity, OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive, float elasticity, float elasticityFalloff, float friction, float scatterAngle)
	UpdateMaterial "Rubber White",0.50,0,0,1,1,0,0,rgb(40+x+x,30+x+x,11+x+x),rgb(33,33,33),rgb(0,0,0),False,False,0,0,0,0
	BumperRingCopper.blenddisablelighting = x/9  ' = 0.25  
	BumperRingTin.blenddisablelighting = x/90 ' = 0.25 
	BumperRingZinc.blenddisablelighting = x/90 ' = 0.25 
	BumperRingIron.blenddisablelighting = x/90 ' = 0.25 
	BumperSkirtCopper.blenddisablelighting = x/90
	BumperSkirtTin.blenddisablelighting = x/90
	BumperSkirtZinc.blenddisablelighting = x/90 
	BumperSkirtIron.blenddisablelighting = x/90 



	'Targetsandtriggers
	SpinnerBracketRight.blenddisablelighting = x/150 ' = 0.1  
	SpinnerBracketLeft.blenddisablelighting = x/150 ' = 0.1  
	SUT6.blenddisablelighting = x/100 ' = 0.15
	SUT4.blenddisablelighting = x/100 ' = 0.15
	sut3.blenddisablelighting = x/100 ' = 0.15
	SUT7.blenddisablelighting = x/100 ' = 0.15
	DT001.blenddisablelighting = x/20 ' = 0.15
	DT002.blenddisablelighting = x/20 ' = 0.15
	DT003.blenddisablelighting = x/20 ' = 0.15
	DT004.blenddisablelighting = x/20 ' = 0.15
	'Bumpers
	BumperCapIron.blenddisablelighting = x/15 
	BumperCapZinc.blenddisablelighting = x/15
	BumperCapCopper.blenddisablelighting = x/15
	BumperCapTin.blenddisablelighting = x/15
	FlipperLR.blenddisablelighting = x/12 ' = 1  
	FlipperLL.blenddisablelighting = x/12 ' = 1  
	FlipperUL.blenddisablelighting = x/15 ' = 1 
	FlipperUR.blenddisablelighting = x/15 ' = 1 
	'toys & models
	SlingMechR.blenddisablelighting =  x/4+2
	SllingMechL.blenddisablelighting =  x/4 +2
	BlastDoor.blenddisablelighting = x/30 ' = 0.5 
	SlingPipesR.blenddisablelighting = x/3+2
	SlingPipesL.blenddisablelighting = x/3+2
	SteamPipes.blenddisablelighting = x/3 + 2 + Flasherlight2.getinplayintensity/15 + Light038.getinplayintensity/4
	WaterPipes.blenddisablelighting = x/3 + 2
	WaterTank.blenddisablelighting = x/7.5 ' = 2
	CoalHopper.blenddisablelighting =  x/4 + 3 
	StokeBase.blenddisablelighting =  x/4 + 3
	BlastFurnace.blenddisablelighting =  x/2 + 2 
	DTMech.blenddisablelighting =  x/15 ' = 1
	Tunnel.blenddisablelighting =  x/15 ' = 1
	ElectricTowerL.blenddisablelighting = x/7.5 ' = 2
	ElectricTowerL001.blenddisablelighting = x/7.5 ' = 2
	ElectricTowerL002.blenddisablelighting = x/7.5 ' = 2
	ElectricTowerL003.blenddisablelighting = x/7.5 ' = 2
	ElectricTowerL004.blenddisablelighting = x/7.5 ' = 2
	ElectricTowerL005.blenddisablelighting = x/7.5 ' = 2
	ElectricTowerL006.blenddisablelighting = x/7.5 ' = 2
	ElectricTowerL006.blenddisablelighting = x/7.5 ' = 2
	ElectricTowerL007.blenddisablelighting = x/7.5 ' = 2
	ElectricTowerL008.blenddisablelighting = x/7.5 ' = 2
	ElectricTowerL009.blenddisablelighting = x/7.5 ' = 2
	RubberPosts.blenddisablelighting = x/30 ' = 0.5 
	CopperPosts.blenddisablelighting = x/222 ' = 0.1 
	SideLightsAll.blenddisablelighting = x/33 
	GearNuts.blenddisablelighting = x/133 
	FlipperGuidePosts.blenddisablelighting = x/120 ' = 1
	GuideGears.blenddisablelighting = x/22 ' = 1
	BallGuide.blenddisablelighting = x/15 ' = 1
	wheel1.blenddisablelighting = x / 55
	wheel2.blenddisablelighting = x / 55
	wheel3.blenddisablelighting = x / 55
	wheel4.blenddisablelighting = x / 55
	wheel5.blenddisablelighting = x / 55


	If x > 4 Then
		lightbox.image = "Lightbox_On"
		OuterWall.image = "WallsOutside_on"
		LowWalls.image = "WallsLow_On"
		ElectricWall.image = "Electricwall_on"
		CoalWall.image = "CoalWall_on"
		SteamWall.image = "SteamWall_on"
		Plastics.image = "Plastics_On"
		PlungerRails.image = "PlungerRails2_On"
		BumperCapIron.image = "bumperiron_on"
		BumperCapCopper.image = "bumpercopper_on"
		BumperCapZinc.image = "bumperzinc_on"
		BumperCapTin.image = "bumpertin_on"
		SlingMechR.image = "Mech_slingR_on"
		SllingMechL.image = "Mech_slingL_on"
		SlingPipesR.image = "pipesSlingR_On"
		SlingPipesL.image = "pipesSlingL_On"
'		SteamPipes.image = "pipessteam_on"
		WaterPipes.image = "Pipes_Water_On"
		WaterTank.image = "WaterTank_On2"
		StokeBase.image = "stokefurnace_on"
		BlastFurnace.image = "BlastFurnace_On"
		Tunnel.image = "Tunnel_On"
		ElectricTowerL.image = "TeslaTowerR_on"
		ElectricTowerL001.image = "TeslaTowerR_on"
		ElectricTowerL002.image = "TeslaTowerR_on"
		ElectricTowerL003.image = "TeslaTowerR_on"
		ElectricTowerL004.image = "TeslaTowerR_on"
		ElectricTowerL005.image = "TeslaTowerR_on"
		ElectricTowerL006.image = "TeslaTowerR_on"
		ElectricTowerL006.image = "TeslaTowerR_on"
		ElectricTowerL007.image = "TeslaTowerR_on"
		ElectricTowerL008.image = "TeslaTowerR_on"
		ElectricTowerL009.image = "TeslaTowerR_on"
		RubberPosts.image = "PostsRubber_on"
		GuideGears.image = "Flipperguidegears_On"
'		RightGuideGears.image = "Flipperguidegears_On"
 		BallGuide.image = "flipperguides_on"
		FlipperGuidePosts.image = "FlipperGuidePosts_On"
	Else
	lightbox.image = "Lightbox_Off"
		OuterWall.image = "WallsOutside_off"
		LowWalls.image = "WallsLow_Off"
		ElectricWall.image = "Electricwall_off"
		CoalWall.image = "CoalWall_off"
		SteamWall.image = "SteamWall_off"
		Plastics.image = "Plastics_Off"
		PlungerRails.image = "PlungerRails2_Off"
		BumperCapIron.image = "bumperiron_off"
		BumperCapCopper.image = "bumpercopper_off"
		BumperCapZinc.image = "bumperzinc_off"
		BumperCapTin.image = "bumpertin_off"
		SlingMechR.image = "Mech_slingR_off"
		SllingMechL.image = "Mech_slingL_off"
		SlingPipesR.image = "pipesSlingR_Off"
		SlingPipesL.image = "pipeSlingL_Off"
'		SteamPipes.image = "pipessteam_off"
		WaterPipes.image = "Pipes_Water_Off"
		WaterTank.image = "WaterTank_Off"
'		CoalHopper.image = "coalhopper_off"
		StokeBase.image = "stokefurnace_off"
		BlastFurnace.image = "BlastFurnace_Off"
		Tunnel.image = "Tunnel_Off"
		ElectricTowerL.image = "TeslaTowerR_off"
		ElectricTowerL001.image = "TeslaTowerR_off"
		ElectricTowerL002.image = "TeslaTowerR_off"
		ElectricTowerL003.image = "TeslaTowerR_off"
		ElectricTowerL004.image = "TeslaTowerR_off"
		ElectricTowerL005.image = "TeslaTowerR_off"
		ElectricTowerL006.image = "TeslaTowerR_off"
		ElectricTowerL006.image = "TeslaTowerR_off"
		ElectricTowerL007.image = "TeslaTowerR_off"
		ElectricTowerL008.image = "TeslaTowerR_off"
		ElectricTowerL009.image = "TeslaTowerR_off"
		RubberPosts.image = "PostRubber_off"
		GuideGears.image = "Flipperguidegears_Off"
'		RightGuideGears.image = "Flipperguidegears_Off"
		BallGuide.image = "flipperguides_off"
		FlipperGuidePosts.image = "FlipperGuidePosts_Off"
	End If

	If VRRoom > 0 then
		If Tilted Then
			VRLight1.image = "VR-RedLight"
			VRLight2.image = "VR-RedLight"
			VRLight3.image = "VR-RedLight"
		Else
			IF AttractModeTimer.Enabled = False then
				If Playerselect = false Then
					If Playercontrol = "Tweed" Then
						VRLight1.image = "VR-GreenLight"
						VRLight1.blenddisablelighting = bbs001.getinplayintensity / 5
						VRLight2.image = "VR-GreenLight"
						VRLight2.blenddisablelighting = bbs001.getinplayintensity / 5
						VRLight3.image = "VR-GreenLight"
						VRLight3.blenddisablelighting = bbs001.getinplayintensity / 5
						RWLightBlue.visible = 0
						RWLightBlue2.visible = 0
						RWLightGreen.visible = 1
						RWLightGreen.opacity = bbs001.getinplayintensity * 30
						RWLightGreen2.visible = 1
						RWLightGreen2.opacity = bbs001.getinplayintensity * 35
						RWLightBlue3.visible = 0
						RWLightBlue4.visible = 0
						RWLightGreen3.visible = 1
						RWLightGreen3.opacity = bbs001.getinplayintensity * 30
						RWLightGreen4.visible = 1
						RWLightGreen4.opacity = bbs001.getinplayintensity * 35
						LWLightBlue.visible = 0
						LWLightBlue2.visible = 0
						LWLightGreen.visible = 1
						LWLightGreen.opacity = bbs001.getinplayintensity * 25
						LWLightGreen2.visible = 1
						LWLightGreen2.opacity = bbs001.getinplayintensity * 30
					End If
					If Playercontrol = "Baron" Then
						VRLight1.image = "VR-BlueLight"
						VRLight1.blenddisablelighting = bbs002.getinplayintensity / 5
						VRLight2.image = "VR-BlueLight"
						VRLight2.blenddisablelighting = bbs002.getinplayintensity / 5
						VRLight3.image = "VR-BlueLight"
						VRLight3.blenddisablelighting = bbs002.getinplayintensity / 5
						RWLightGreen.visible = 0
						RWLightGreen2.visible = 0
						RWLightBlue.visible = 1
						RWLightBlue.opacity = bbs002.getinplayintensity * 30
						RWLightBlue2.visible = 1
						RWLightBlue2.opacity = bbs002.getinplayintensity * 35
						RWLightGreen3.visible = 0
						RWLightGreen4.visible = 0
						RWLightBlue3.visible = 1
						RWLightBlue3.opacity = bbs002.getinplayintensity * 30
						RWLightBlue4.visible = 1
						RWLightBlue4.opacity = bbs002.getinplayintensity * 35
						LWLightGreen.visible = 0
						LWLightGreen2.visible = 0
						LWLightBlue.visible = 1
						LWLightBlue.opacity = bbs002.getinplayintensity * 25
						LWLightBlue2.visible = 1
						LWLightBlue2.opacity = bbs002.getinplayintensity * 30
					End If
				Else
					If PlayerChoosen = "Tweed" Then
						VRLight1.image = "VR-GreenLight"
						VRLight1.blenddisablelighting = bbs001.getinplayintensity / 5
						VRLight2.image = "VR-GreenLight"
						VRLight2.blenddisablelighting = bbs001.getinplayintensity / 5
						VRLight3.image = "VR-GreenLight"
						VRLight3.blenddisablelighting = bbs001.getinplayintensity / 5
						RWLightBlue.visible = 0
						RWLightBlue2.visible = 0
						RWLightGreen.visible = 1
						RWLightGreen.opacity = bbs001.getinplayintensity * 30
						RWLightGreen2.visible = 1
						RWLightGreen2.opacity = bbs001.getinplayintensity * 35
						RWLightBlue3.visible = 0
						RWLightBlue4.visible = 0
						RWLightGreen3.visible = 1
						RWLightGreen3.opacity = bbs001.getinplayintensity * 30
						RWLightGreen4.visible = 1
						RWLightGreen4.opacity = bbs001.getinplayintensity * 35
						LWLightBlue.visible = 0
						LWLightBlue2.visible = 0
						LWLightGreen.visible = 1
						LWLightGreen.opacity = bbs001.getinplayintensity * 25
						LWLightGreen2.visible = 1
						LWLightGreen2.opacity = bbs001.getinplayintensity * 30
					End If
					If PlayerChoosen = "Baron" Then
						VRLight1.image = "VR-BlueLight"
						VRLight1.blenddisablelighting = bbs002.getinplayintensity / 5
						VRLight2.image = "VR-BlueLight"
						VRLight2.blenddisablelighting = bbs002.getinplayintensity / 5
						VRLight3.image = "VR-BlueLight"
						VRLight3.blenddisablelighting = bbs002.getinplayintensity / 5
						RWLightGreen.visible = 0
						RWLightGreen2.visible = 0
						RWLightBlue.visible = 1
						RWLightBlue.opacity = bbs002.getinplayintensity * 30
						RWLightBlue2.visible = 1
						RWLightBlue2.opacity = bbs002.getinplayintensity * 35
						RWLightGreen3.visible = 0
						RWLightGreen4.visible = 0
						RWLightBlue3.visible = 1
						RWLightBlue3.opacity = bbs002.getinplayintensity * 30
						RWLightBlue4.visible = 1
						RWLightBlue4.opacity = bbs002.getinplayintensity * 35
						LWLightGreen.visible = 0
						LWLightGreen2.visible = 0
						LWLightBlue.visible = 1
						LWLightBlue.opacity = bbs002.getinplayintensity * 25
						LWLightBlue2.visible = 1
						LWLightBlue2.opacity = bbs002.getinplayintensity * 30
					End If	
				End If
			Else
				VRLight1.image = "VR-LightOff"
				VRLight2.image = "VR-LightOff"
				VRLight3.image = "VR-LightOff"
				RWLightGreen.visible = 0
				RWLightGreen2.visible = 0
				RWLightBlue.visible = 0
				RWLightBlue2.visible = 0
				RWLightGreen3.visible = 0
				RWLightGreen4.visible = 0
				RWLightBlue3.visible = 0
				RWLightBlue4.visible = 0
				LWLightGreen.visible = 0
				LWLightGreen2.visible = 0
				LWLightBlue.visible = 0
				LWLightBlue2.visible = 0
			End If
		End If
		VRTweed.opacity    = bbs001.getinplayintensity * 30
		VRBaron.opacity    = bbs002.getinplayintensity * 30
		VRTilt.opacity = bbs004.getinplayintensity * 20
		VRGameOver.opacity = bbs003.getinplayintensity * 20
		VRBallSave.opacity = bbs005.getinplayintensity * 20
		VRHS.opacity = bbs006.getinplayintensity * 3.5
		VRHS2.opacity = bbs006.getinplayintensity * 3.5
		VRHS3.opacity = bbs006.getinplayintensity * 10
		VRDHS.opacity = bbs007.getinplayintensity * 3.5
		VRDHS2.opacity = bbs007.getinplayintensity * 3.5
		VRDHS3.opacity = bbs007.getinplayintensity * 10
		for each VRFL in VRBackglassGIVolkan : VRFL.opacity = bbs008.getinplayintensity * 2 : Next
		for each VRFL in VRBackglassGITweed : VRFL.opacity = bbs009.getinplayintensity * 2 : Next
		for each VRFL in VRBackglassGIBaron : VRFL.opacity = bbs010.getinplayintensity * 2 : Next
		for each VRFL in VRBackglassGIAbigail : VRFL.opacity = bbs011.getinplayintensity * 2 : Next
	End If
End Sub


'*******************************************
'  Drain, Trough, and Ball Release
'*******************************************
'
'TROUGH 
Sub swTrough1_Hit   : UpdateTrough : RandomSoundEjectHoleEnter swTrough1 : End Sub
Sub swTrough1_UnHit : UpdateTrough : End Sub
Sub swTrough2_Hit   : UpdateTrough : RandomSoundEjectHoleEnter swTrough2 : End Sub
Sub swTrough2_UnHit : UpdateTrough : End Sub
Sub swTrough3_Hit   : UpdateTrough : RandomSoundEjectHoleEnter swTrough3 : End Sub
Sub swTrough3_UnHit : UpdateTrough : End Sub
Sub swTrough4_Hit   : UpdateTrough : RandomSoundEjectHoleEnter swTrough4 : End Sub
Sub swTrough4_UnHit : UpdateTrough : End Sub
Sub swTrough5_Hit   : UpdateTrough
	activeball.image = "ball_HDR"
	DOF 116,2
	RandomSoundEjectHoleEnter swTrough5
	BIP = BIP - 1
'	debug.print "swTrough5  BIP =" & BIP
	If ballsave > -3 then ' ioncludes a grace period after light stop blinking
		DOF 117,2
		If bip = 0 then PlayVoice "vo_tryagain",1,Voice_Volume*0.6,0
		BallsToLaunch = BallsToLaunch + 1
		AutoPlunger = true
		If BallsToLaunch = 1 And BiP = 0 then drain.timerinterval = 333
		drain.timerenabled = False
		drain.timerenabled = True
		bls 114,114,5,6,13,0
			Select Case int(rnd(1)*7)
				case 0 : PlayVoice "sfx_letitbyyou",1,Voice_Volume * 0.4,0
				case 1 : PlayVoice "sfx_getbehind",1,Voice_Volume * 0.4,0
				case 2 : PlayVoice "sfx_getbusy",1,Voice_Volume * 0.4,0
				case 3 : PlayVoice "sfx_shouldrest",1,Voice_Volume * 0.4,0
				case 4 : PlayVoice "vo_strontiumprecise",1,Voice_Volume * 0.4,0
				case 5 : PlayVoice "vo_strontiumnecessary",1,Voice_Volume * 0.4,0
				case 6 : PlayVoice "vo_withoutcoal",1,Voice_Volume * 0.4,0
			End Select
	End If


	If BIP = 1 And BallsToLaunch = 0 then

		If PlayerMode(currentplayer) < 10 and BlastdoorMB > 0 Then
			DoubleScoring = False
			Blink(29,1) = 0
			ClearVoiceQ
			PlayVoice "vo_itsover",1,Voice_Volume*0.7,0
		End If

		BlastdoorMB = 0 

		If BlastdoorOpen = 0 and HurryupLevel2(CurrentPlayer) <> 1 and HurryupLevel2(CurrentPlayer) <> 2 then
			DoorF.rotatetostart 
			Blink(121,1)=0
			Blink(122,1)=0
			Blink(123,1)=0
			Blink(124,1)=0

			PlaySound "sfx_furnacedoor",1,VolumeDial
			blastfurnaceReady.enabled = False
		End If
		DOF 127,0
		multiballtimer.enabled = False  
		playsound "sfx_powerdown",1,VolumeDial
		StopSound "sfx_multiballdrone"
		StopSound "Volkan_Overture_II"
		StopSound "sfx_multiballbeat"

	End If

	If BIP = 0 And BallsToLaunch = 0  then
		if not tilted then
			Tilt = 0
			If vrroom = 0 and DesktopMode Then Light007.state = 0 : Light008.state = 0
			If b2son then bbs 4,0,0,0

			BallOver = True
'			debug.print "Drain last ball:   Ballover = " & BallOver

			LightSeqAttract.stopplay
			LightSeqAttract.UpdateInterval = 5
			LightSeqAttract.Play seqalloff
			ClearVoiceQ
			balltime = 1000
			If int(rnd(1)*2) <> 1 then
				If CurrentPlayer = 1 then
					PlayVoice "Tweed_ThatIsQuiteUnfortunate",1,Voice_Volume,0
				Else
					PlayVoice "baron_YouFool",1,Voice_Volume ,0
				End If
			Else
'				Select Case int(rnd(1)*3)
'					Case 0 : PlayVoice "vo_dontunderestimate",1,Voice_Volume*0.7,0
'					Case 1 : PlayVoice "vo_maybethisballwill",1,Voice_Volume*0.7,0
'					Case 2 : PlayVoice "vo_dontunderestimate",1,Voice_Volume*0.7,0
'				End Select
				PlayVoice "vo_dontunderestimate",1,Voice_Volume*0.7,0
			End If

			bls 128,117,7,4,4,0
			if not tilted then Addscore Howmuch
			Howmuch = 0
			Hurryup_Active = 0
			HurryupTimerBumpers.Enabled = False
			Blink(15,1)=0
			Blink(117,1)=0
			Blink(118,1)=0
			Blink(119,1)=0
			Blink(120,1)=0

			StopSound "sfx_clocktimer"

			EOB_counter = int ( EOB_Bonus / 100 )
			EOB_Ticks = 25

			If HurryupLevel2(CurrentPlayer) < 3 Then
				HurryupLevel2(CurrentPlayer) = 10 ' failed
				HurryupLevelC = 0
				LevelHurryupTimer.enabled = False
				If BlastdoorOpen = 0 then
					DoorF.rotatetostart 
					Blink(121,1)=0
					Blink(122,1)=0
					Blink(123,1)=0
					Blink(124,1)=0
 
					PlaySound "sfx_furnacedoor",1,VolumeDial
				End If
			End If


			Blink(125,1)=0
			Blink(126,1)=0
			Blink(127,1)=0
			Blink(128,1)=0
'			debug.print "badball " & BadBall(CurrentPlayer) & " " & currentplayer & "   complete=" & BadBallComplete(CurrentPlayer)
			If PlayerScore(CurrentPlayer) < badball(CurrentPlayer) + 10000 And BadBallComplete(CurrentPlayer) = 0 Then BadBallComplete(CurrentPlayer) = 1
'			debug.print "badball " & BadBall(CurrentPlayer) & " " & currentplayer & "   complete=" & BadBallComplete(CurrentPlayer)
			DoubleScoring = False
			EndOfBallBonus.enabled = True
			multiballtimer.enabled = False
			multiballtimer2.enabled = False
			blastfurnaceReady.enabled = False

			GaugeLeftPos = 42
			GaugeRightPos = 47
			TweedControl = 0
			BaronControl = 0
			PlaySound "sfx_awardbonus",1,VolumeDial
			If CurrentPlayer = 1 then
				Playercontrol = "Tweed"
			Else
				Playercontrol = "Baron"
			End If
			UpdateControl
			Blink(113,1)=0
		End If
		bls 151,156,5,15,7,0
	Else
		bls 151,156,2,15,7,0
	End If
End Sub
Dim BallOver : BallOver = True

Sub swTrough5_UnHit : UpdateTrough : End Sub


Sub UpdateTrough
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
'	If A_Wheels(1) = 0 then 
	A_Wheels(0) = 8
End Sub

Sub UpdateTroughTimer_Timer
	If swTrough1.BallCntOver = 0 Then If swTrough2.BallCntOver = 1 then swTrough2.kick 57, 10 : RandomSoundEjectHoleSolenoid swTrough2
	If swTrough2.BallCntOver = 0 Then If swTrough3.BallCntOver = 1 then swTrough3.kick 57, 10 : RandomSoundEjectHoleSolenoid swTrough3
	If swTrough3.BallCntOver = 0 Then If swTrough4.BallCntOver = 1 then swTrough4.kick 57, 10 : RandomSoundEjectHoleSolenoid swTrough4
	If swTrough4.BallCntOver = 0 Then If swTrough5.BallCntOver = 1 then swTrough5.kick 57, 10 : RandomSoundEjectHoleSolenoid swTrough5
	Me.Enabled = 0
End Sub

Dim AutoPlunger
Dim BallsToLaunch
' DRAIN & RELEASE
Dim EOB_counter
Sub Drain_Hit 
	swTrough5.timerenabled = True

	RandomSoundOutholeHit
End Sub
Sub swTrough5_Timer
	swTrough5.timerenabled = False
	
	Drain.kick 60, 20
	BallsDrained = BallsDrained +1
End Sub

Dim Saveplayer(152,2)
Dim EOB_Ticks
Sub EndOfBallBonus_Timer
	Dim x
	If EOB_Ticks > 0 then
		EOB_Ticks = EOB_Ticks - 1
'		DEBUG.PRINT EOB_Bonus & "  " & EOB_counter
		Addscore EOB_counter
		playsound "fx_diverter",1,VolumeDial
	Else
		

		badball(CurrentPlayer) = PlayerScore(CurrentPlayer)

		if not tilted then PlaySound "sfx_awardcontract",1,VolumeDial
		EndOfBallBonus.enabled = False


' fixing  Extra bALL's
		If EB_Ready(CurrentPlayer) > 0 Then Start_EB : Exit Sub

		If playersplaying = 2 then
			For x = 1 to 140 
				Saveplayer(x,currentplayer)=blink(x,1)
				Blink(x,1)=0
			Next
			Saveplayer(141,CurrentPlayer) = Bumper1Counter
			Saveplayer(142,CurrentPlayer) = Bumper2Counter
			Saveplayer(143,CurrentPlayer) = Bumper3Counter
			Saveplayer(144,CurrentPlayer) = Bumper4Counter
			Saveplayer(145,CurrentPlayer) = RaiseTemp
			Saveplayer(146,CurrentPlayer) = ElectricCounter
			Saveplayer(147,CurrentPlayer) = SteamCounter
			Saveplayer(148,CurrentPlayer) = BlastdoorOpen
			Saveplayer(149,CurrentPlayer) = blasttriggerON
'			Saveplayer(150,CurrentPlayer) = tripplescoring

			CurrentPlayer = CurrentPlayer + 1
			If CurrentPlayer = 2 And BallInPlay = MaxBalls then PlayVoice "sfx_disappointment",1,Voice_Volume,0
			If CurrentPlayer = 3 then
				CurrentPlayer = 1

				BallInPlay = BallInPlay + 1
				If BallInPlay = MaxBalls + 1 then
					Ballinplay = 0
					Highscorecheck : BallInPlay = 9
					PlayVoice "vo_nolonger2",1,Voice_Volume ,0
					Exit Sub
				Else

					StopBkgd : PlaySound "Steampunk_Bkgd" & ballinplay ,-1,Music_Volume*0.7
				End If
			End If

			BallOver = False
'			debug.print "EOB 2p:   Ballover = " & BallOver


			If CurrentPlayer = 1 then 
				Playercontrol = "Tweed"
				If BallInPlay = 2 then PlayVoice "Tweed_WhatIsGoingOn",1,Voice_Volume,2
				If BallInPlay = 4 then PlayVoice "Tweed_ThereAreSpies",1,Voice_Volume,2
				If BallInPlay = 3 then PlayVoice "Tweed_MyEveryMove",1,Voice_Volume,2
				If BallInPlay = 5 then PlayVoice "Tweed_WeHaveABusinessToRun",1,Voice_Volume,2
			Else
				Playercontrol = "Baron"
				If BallInPlay = 1 then PlayVoice "baron_ImResponsibleForVolkan",1,Voice_Volume,2
				If BallInPlay = 5 then PlayVoice "baron_VolkanWillBeMine",1,Voice_Volume,2
				If BallInPlay = 2 then PlayVoice "baron_VeryGood",1,Voice_Volume,2
				If BallInPlay = 3 then PlayVoice "baron_IWillGetControl",1,Voice_Volume,2
				If BallInPlay = 4 then PlayVoice "baron_DontLetMeDown",1,Voice_Volume,2
			End If

			For x = 1 TO 140
				Blink(x,1) = Saveplayer(X,CurrentPlayer)
			Next
			Bumper1Counter = Saveplayer(141,CurrentPlayer)
			Bumper2Counter = Saveplayer(142,CurrentPlayer)
			Bumper3Counter = Saveplayer(143,CurrentPlayer)
			Bumper4Counter = Saveplayer(144,CurrentPlayer)
				 RaiseTemp = Saveplayer(145,CurrentPlayer)
			ElectricCounter= Saveplayer(146,CurrentPlayer)
			  SteamCounter = Saveplayer(147,CurrentPlayer)
			If Saveplayer(148,CurrentPlayer) = 1 then
				BlastdoorOpen = 1
				DoorF.rotatetoend
				Blink(121,1)=2
				Blink(122,1)=2
				Blink(123,1)=2
				Blink(124,1)=2
				BLS 121,124,5,5,5,0
				blastfurnaceReady.enabled = True
			Else
				DoorF.rotatetostart
				Blink(121,1)=0
				Blink(122,1)=0
				Blink(123,1)=0
				Blink(124,1)=0
				BLS 121,124,5,5,5,0
				blastfurnaceReady.enabled = False

			End If
			blasttriggerON = Saveplayer(149,CurrentPlayer)
'			tripplescoring = Saveplayer(150,CurrentPlayer)

			If DesktopMode and VRRoom = 0 then
				If currentplayer = 1 then
					DT_Volkan1.state = 1
					DT_Volkan2.state = 0
				End If
			End If
			if b2son then bbs 8,-1,1,5
		Else
			BallInPlay = BallInPlay + 1
			If BallInPlay = MaxBalls + 1 then
				Ballinplay = 0 
				Highscorecheck
				If p2win = 0 then
					If blink(1,1)<> 0 then
						PlayVoice "vo_notoneorder",1,Voice_Volume	,0		
					Else
						Select Case int(rnd(1)*6)
							Case  0 : PlayVoice "vo_nolonger2",1,Voice_Volume,0
							Case  1 : PlayVoice "vo_perhapsabeer",1,Voice_Volume,0
							Case  2 : PlayVoice "vo_perhapsbest",1,Voice_Volume,0
							Case  3 : PlayVoice "vo_pinballortiddlywinks",1,Voice_Volume,0
							Case  4 : PlayVoice "vo_failedvolkan",1,Voice_Volume,0
							Case  5 : PlayVoice "vo_thatwaspathetic",1,Voice_Volume,0
						End Select
					End If
				End If
				Exit Sub
			Else

				StopBkgd : PlaySound "Steampunk_Bkgd" & ballinplay ,-1,Music_Volume*0.7
				BallOver = False
'				debug.print "EOB 1p:   Ballover = " & BallOver

			If BlastdoorOpen = 1 then
				DoorF.rotatetoend
				Blink(121,1)=2
				Blink(122,1)=2
				Blink(123,1)=2
				Blink(124,1)=2
				BLS 121,124,5,5,5,0
				blastfurnaceReady.enabled = True
			Else
				DoorF.rotatetostart
				Blink(121,1)=0
				Blink(122,1)=0
				Blink(123,1)=0
				Blink(124,1)=0
				BLS 121,124,5,5,5,0
			End If

			End If
		End If

		If PlayerMode(CurrentPlayer) = 10 Then
			DoubleScoring = True
			blink(29,1) = 2 
		Else
			DoubleScoring = False
			blink(29,1) = 0 

		End If

		If Blink(31,1)=1 then GaugeLeftPos = 60
		If Blink(32,1)=1 then GaugeLeftPos = 70
		If Blink(33,1)=1 then GaugeLeftPos = 80
		If Blink(34,1)=1 then GaugeLeftPos = 90
		If Blink(35,1)=1 then GaugeLeftPos = 100
		If Blink(36,1)=1 then GaugeLeftPos = 110
		If Blink(37,1)=1 then GaugeLeftPos = 120
		If Blink(38,1)=1 then GaugeLeftPos = 130
		If Blink(39,1)=1 then GaugeLeftPos = 140

		If Blink(41,1)=1 then GaugeRightPos = 63
		If Blink(42,1)=1 then GaugeRightPos = 71.5
		If Blink(43,1)=1 then GaugeRightPos = 80
		If Blink(44,1)=1 then GaugeRightPos = 88.5
		If Blink(45,1)=1 then GaugeRightPos = 97
		If Blink(46,1)=1 then GaugeRightPos = 105.5
		If Blink(47,1)=1 then GaugeRightPos = 114
		If Blink(48,1)=1 then GaugeRightPos = 122.5
		If Blink(49,1)=1 then GaugeRightPos = 131
		If Blink(17,1)=2 then multiballtimer2.enabled = True

		UpdateControl
		EOB_Bonus = 0
		drain.timerenabled = False
		drain.timerenabled = True ' next ball out
		bls 114,114,5,6,13,0

		If BadBallComplete(CurrentPlayer) = 1 Then BadBallComplete(CurrentPlayer) = 2 : ClearVoiceQ :  PlayVoice "vo_maybethisballwill",1,Voice_Volume*0.7,0 

		BallsToLaunch = BallsToLaunch + 1
		LightSeqAttract.stopplay
		LightSeqAttract.Play Seqblinking ,,3, 22
		DT1_Timer ' always reset dt between balls
		BaronControl = 0
		TweedControl = 0

		ballsave = 5  + MaxBallsave
'		debug.print "BallSave15sec"

		ballsaveActivated = 0
		If Blink(17,1) <> 2 then ' multiball challenge start
			Blink(104,1) = 1
			blink(18,1) = 0 
			blink(19,1) = 0
		End If
		Order_Value_Counter = 0 	' 2X3X OFF AT NEW balLS
		Blink(102,1) = 0 
		Blink(103,1) = 0

		SteamElct_Vo = 0
		Danger = 0
		BallOver = False

	End If
End Sub



Sub Start_EB

		ClearVoiceQ
		PlayVoice "vo_extraball2",1,Voice_Volume,0

'		debug.print "Start extra ball"
		Blink(105,1)=0
		BLS 105,105,10,10,5,0
		EB_Ready(CurrentPlayer) = EB_Ready(CurrentPlayer) - 1
'		debug.print "EB_left = " & EB_Ready(CurrentPlayer)
		If EB_Ready(CurrentPlayer) > 0 Then Blink(105,1)=1

		EOB_Bonus = 0
		drain.timerenabled = False
		drain.timerenabled = True ' next ball out
		bls 114,114,5,6,13,0
		BallsToLaunch = BallsToLaunch + 1
		LightSeqAttract.stopplay
		LightSeqAttract.Play Seqblinking ,,3, 22
		DT1_Timer ' always reset dt between balls
		BaronControl = 0
		TweedControl = 0

		ballsave = 5 + MaxBallsave
'		debug.print "BallSave15sec"

		Order_Value_Counter = 0 	' 2X3X OFF AT NEW balLS
		Blink(102,1) = 0 
		Blink(103,1) = 0

		SteamElct_Vo = 0
		Danger = 0
		BallOver = False

' fixing a test for this
End Sub




Dim p2win
Sub Highscorecheck
	balltime = 1000
	p2win = 0
	If PlayerScore(2) > PlayerScore(1) then PlayVoice "baron_IHaveFinallyWon",1,Voice_Volume,0 : p2win = 1
	PlaySound "sfx_powerdown",1,VolumeDial
	Select Case PlayerChoosen
		case "Tweed"
			IF PlayerScore(1) > HighscoreTweed(0) then
				p2win = 1
				If Int(rnd(1)*2) = 1 then PlayVoice "vo_congrats",1,Voice_Volume,0 else PlayVoice "vo_congrats",1,Voice_Volume,0
			End If

			If PlayerScore(1) > HighscoreTweed(2) then
				HighscoreTweed(0) = HighscoreTweed(1)
				HighscoreTweed(1) = HighscoreTweed(2)
				HighscoreTweed(2) = PlayerScore(1)
			elseif PlayerScore(1) > HighscoreTweed(1) then
				HighscoreTweed(0) = HighscoreTweed(1)
				HighscoreTweed(1) = PlayerScore(1)
			elseif PlayerScore(1) > HighscoreTweed(0) then
				HighscoreTweed(0) = PlayerScore(1)
			End If

		case "Baron"
			IF PlayerScore(2) > Highscorebaron(0) then
				If Int(rnd(1)*2) = 1 then PlayVoice "vo_congrats",1,Voice_Volume,0 else PlayVoice "vo_congrats",1,Voice_Volume,0
				p2win = 1
			End If

			If PlayerScore(2) > HighscoreBaron(2) then
				HighscoreBaron(0) = HighscoreBaron(1)
				HighscoreBaron(1) = HighscoreBaron(2)
				HighscoreBaron(2) = PlayerScore(2)
			elseif PlayerScore(2) > HighscoreBaron(1) then
				HighscoreBaron(0) = HighscoreBaron(1)
				HighscoreBaron(1) = PlayerScore(2)
			elseif PlayerScore(2) > HighscoreBaron(0) then
				HighscoreBaron(0) = PlayerScore(2)
			End If
		case "None"
			If PlayerScore(2) > HighscoreTweed(0) or PlayerScore(1) > HighscoreTweed(0) then
				p2win = 1
				If Int(rnd(1)*2) = 1 then PlayVoice "vo_congrats",1,Voice_Volume,0 else PlaySound "PlayVoice",1,Voice_Volume,0
			End If
			If PlayerScore(1) > HighscoreDuelTweed(2) then
				HighscoreDuelTweed(0) = HighscoreDuelTweed(1)
				HighscoreDuelTweed(1) = HighscoreDuelTweed(2)
				HighscoreDuelTweed(2) = PlayerScore(1)
			elseif PlayerScore(1) > HighscoreDuelTweed(1) then
				HighscoreDuelTweed(0) = HighscoreDuelTweed(1)
				HighscoreDuelTweed(1) = PlayerScore(1)
			elseif PlayerScore(1) > HighscoreDuelTweed(0) then
				HighscoreDuelTweed(0) = PlayerScore(1)
			End If

			If PlayerScore(2) > HighscoreDuelBaron(2) then
				HighscoreDuelBaron(0) = HighscoreDuelBaron(1)
				HighscoreDuelBaron(1) = HighscoreDuelBaron(2)
				HighscoreDuelBaron(2) = PlayerScore(2)
			elseif PlayerScore(2) > HighscoreDuelBaron(1) then
				HighscoreDuelBaron(0) = HighscoreDuelBaron(1)
				HighscoreDuelBaron(1) = PlayerScore(2)
			elseif PlayerScore(2) > HighscoreDuelBaron(0) then
				HighscoreDuelBaron(0) = PlayerScore(2)
			End If
			If p2win = 0 then
				If PlayerScore(1)>PlayerScore(2) then
					p2win=1
					Select Case int(rnd(1)*2)
						case 0 : PlayVoice "vo_player1wins",1,Voice_Volume,0
						case 1 : PlayVoice "vo_player1wins2",1,Voice_Volume,0
					End Select
				Elseif PlayerScore(2)>PlayerScore(1) then
					p2win=1
					Select Case int(rnd(1)*2)
						case 0 : PlayVoice "vo_player2wins",1,Voice_Volume,0
						case 1 : PlayVoice "vo_player2wins2",1,Voice_Volume,0
					End Select
				End If
			End If

	End Select
	SaveHS
	StartAttractMode
End Sub




Sub Drain_Timer
	drain.timerinterval = 1500
'	debug.print "ballrelease has ball? = " & swTrough1.BallCntOver & " BIP=" & bip
	If swTrough1.BallCntOver = 1 And BiP < 5 then
		If BallsToLaunch > 0 then
			CreateNewBall
'			If BIP > 1 then Playsound "sfx_andanother",1,Voice_Volume * 0.6
			BallsToLaunch = BallsToLaunch - 1
'			debug.print "Drain: createball   BiP="& BIP & "  remain:" & BallsToLaunch
			drain.timerenabled = False
			Blink(134,1)=2' trainlights
			Blink(133,1)=0
			bls 133,133,3,7,7,0
			bls 134,134,1,7,7,15
		End If
	End If
End Sub

Dim BallsDrained
Sub CreateNewBall
	swTrough1.kick 90,10
	PlaySound "sfx_ballrelease",1,VolumeDial
	RandomSoundShooterFeeder swTrough1
	DOF 120,2
	DOF 137,2
	BIP = BIP + 1
End Sub



'*******************************************
'  Bumbers
'*******************************************
Sub check_BumperHurryup

	TotalBumperHits(CurrentPlayer) = TotalBumperHits(CurrentPlayer) + 1
'	debug.print "TotalBumperHits = " & TotalBumperHits(CurrentPlayer) & "  BIP=" & BIP
	If TotalBumperHits(CurrentPlayer) > 9 And HurryupBumperDone(CurrentPlayer) = 0 And Hurryup_Active = 0 then
		HurryupBumperDone(CurrentPlayer) = 1
		Hurryup_Active = 5
		HurryupTimerBumpers.Enabled = true		
		Audiospeed = 0
		Blink(15,1) = 2
		Blink(117,1)=2
		Blink(118,1)=2
		Blink(119,1)=2
		Blink(120,1)=2
		BLS 15,15,5,5,5,0
		voice_addaball
	End If
	If TotalBumperHits(CurrentPlayer) > 24 And HurryupBumperDone(CurrentPlayer) = 1 And Hurryup_Active = 0 then
		HurryupBumperDone(CurrentPlayer) = 2
		Hurryup_Active = 6
		HurryupTimerBumpers.Enabled = true		
		Audiospeed = 0
		Blink(15,1) = 2
		Blink(117,1)=2
		Blink(118,1)=2
		Blink(119,1)=2
		Blink(120,1)=2
		BLS 15,15,5,5,5,0
		voice_addaball
	End If
	If TotalBumperHits(CurrentPlayer) > 44 And HurryupBumperDone(CurrentPlayer) = 2 And Hurryup_Active = 0 then
		HurryupBumperDone(CurrentPlayer) = 3
		Hurryup_Active = 7
		HurryupTimerBumpers.Enabled = true		
		Audiospeed = 0
		Blink(15,1) = 2
		Blink(117,1)=2
		Blink(118,1)=2
		Blink(119,1)=2
		Blink(120,1)=2
		BLS 15,15,5,5,5,0
		voice_addaball
	End If
	If TotalBumperHits(CurrentPlayer) > 69 And HurryupBumperDone(CurrentPlayer) = 3 And Hurryup_Active = 0 then
		TotalBumperHits(CurrentPlayer) = 40
		Hurryup_Active = 8
		HurryupTimerBumpers.Enabled = true		
		Audiospeed = 0
		Blink(15,1) = 2
		Blink(117,1)=2
		Blink(118,1)=2
		Blink(119,1)=2
		Blink(120,1)=2
		BLS 15,15,5,5,5,0
		voice_addaball
	End If
End Sub

Sub voice_addaball
	Select Case int(rnd(1)*4)
		case 0 : PlayVoice "vo_addaballlit",1,Voice_Volume,0
		case 1 : PlayVoice "vo_goaddaball",1,Voice_Volume,0
		case 2,3 : PlayVoice "vo_shootstokeadd",1,Voice_Volume,0
	End Select

End Sub


Dim Audiospeed
Sub HurryupTimerBumpers_Timer
'	debug.print "hurryupspeed = " & Audiospeed
	BLS 135,135,1,5,5,0
	PlaySound "buzz", 1, VolumeDial, 0, 0,audiospeed,0, 1, 0
	PlaySound "buzz", 1, VolumeDial, 0, 0,0,0, 1, 0
	audiospeed = audiospeed + 2500
	If audiospeed > 35000 then
		HurryupTimerBumpers.Enabled = False
		BLS 15,15,5,5,5,0
	End If
End Sub


Sub Bumper001_hit
	DOF 105,2
	UndercabFlicker
	RandomSoundBumperLow Bumper001
	PlaySoundat "sfx_bumper1",Bumper001
	If Tilted Then Exit Sub

	bls 61,64,3,3,3,0
	bls 138,138,1,3,10,0

	Addscore 50

	check_BumperHurryup

	Bumper1Counter = Bumper1Counter + 1
	Select Case Bumper1Counter
		case 1 : If Blink(61,1)=2 Then Blink(61,1)=1 : CheckMode
		case 2 : If Blink(62,1)=2 Then Blink(62,1)=1 : CheckMode
		case 3 : If Blink(63,1)=2 Then Blink(63,1)=1 : CheckMode
		case 4 : If Blink(64,1)=2 Then Blink(64,1)=1 : CheckMode
	End Select

	bls 25,25,2,5,5,0
	Bumper001.timerenabled = True
	Bumper2Pos = 0
	BumperRingIron.z = -10
	ScrewIron1.z = 77
	ScrewIron2.z = 77
	BumperCapIron.transz = 2

End Sub
Dim Bumper1Pos
Sub Bumper001_Timer
	Bumper2Pos = Bumper2Pos + 1
	Select Case Bumper2Pos
		case 1 : BumperRingIron.z = -20 : BumperCapIron.transz=4
		case 2 : BumperRingIron.z = -30 : BumperCapIron.transz=3
		case 3 : BumperRingIron.z = -32 : BumperCapIron.transz=2
		case 4 : BumperRingIron.z = -28 : BumperCapIron.transz=1
		case 5 : BumperRingIron.z = -16 : BumperCapIron.transz=0
		case 6 : BumperRingIron.z = -8
		case 7 : BumperRingIron.z = 0
			Bumper001.timerenabled = False
	End Select
	ScrewIron1.z = BumperCapIron.transz+75
	ScrewIron2.z = BumperCapIron.transz+75
End Sub

Sub Bumper002_hit
	DOF 106,2
	UndercabFlicker
	RandomSoundBumperLow Bumper002
	PlaySoundat "sfx_bumper2",Bumper002

	If Tilted Then Exit Sub
	bls 51,54,3,3,3,0
	bls 137,137,1,3,10,0

	Addscore 50

	check_BumperHurryup
	

	Bumper2Counter = Bumper2Counter + 1
	Select Case Bumper2Counter
		case 1 : If Blink(51,1)=2 Then Blink(51,1)=1 : CheckMode
		case 2 : If Blink(52,1)=2 Then Blink(52,1)=1 : CheckMode
		case 3 : If Blink(53,1)=2 Then Blink(53,1)=1 : CheckMode
		case 4 : If Blink(54,1)=2 Then Blink(54,1)=1 : CheckMode
	End Select

	bls 26,26,2,5,5,0
	Bumper002.timerenabled = True
	Bumper2Pos = 0
	BumperRingCopper.z=-10
	Screwcopper1.z = 77
	Screwcopper2.z = 77
	BumperCapCopper.transz=2
End Sub
Dim Bumper2Pos
Sub Bumper002_Timer
	Bumper2Pos = Bumper2Pos + 1
	Select Case Bumper2Pos
		case 1 : BumperRingCopper.z=-20 : BumperCapCopper.transz=4
		case 2 : BumperRingCopper.z=-30 : BumperCapCopper.transz=3
		case 3 : BumperRingCopper.z=-32 : BumperCapCopper.transz=2
		case 4 : BumperRingCopper.z=-28 : BumperCapCopper.transz=1
		case 5 : BumperRingCopper.z=-16 : BumperCapCopper.transz=0
		case 6 : BumperRingCopper.z=-8
		case 7 : BumperRingCopper.z=0
			Bumper002.timerenabled = False
	End Select
	Screwcopper1.z = BumperCapCopper.transz+75	
	Screwcopper2.z = BumperCapCopper.transz+75
End Sub

Sub Bumper003_hit
	DOF 107,2
	UndercabFlicker
	RandomSoundBumperUp Bumper003
	PlaySoundat "sfx_bumper3",Bumper003
	If Tilted Then Exit Sub


	If BIP = 1 then
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play Seqblinking ,,1, 22
	End If
	bls 81,84,3,3,3,0
	bls 136,136,1,3,10,0

	Addscore 50

	check_BumperHurryup


	Bumper3Counter = Bumper3Counter + 1
	Select Case Bumper3Counter
		case 1 : If Blink(81,1)=2 Then Blink(81,1)=1 : CheckMode
		case 2 : If Blink(82,1)=2 Then Blink(82,1)=1 : CheckMode
		case 3 : If Blink(83,1)=2 Then Blink(83,1)=1 : CheckMode
		case 4 : If Blink(84,1)=2 Then Blink(84,1)=1 : CheckMode
	End Select

	bls 27,27,2,5,5,0
	Bumper003.timerenabled = True
	Bumper3Pos = 0
	BumperRingZinc.z=-10
	Screwiron001.z = 77
	Screwiron002.z = 77
	BumperCapZinc.transz=2
End Sub
Dim Bumper3Pos
Sub Bumper003_Timer
	Bumper3Pos = Bumper3Pos + 1
	Select Case Bumper3Pos
		case 1 : BumperRingZinc.z=-20 : BumperCapZinc.transz=4
		case 2 : BumperRingZinc.z=-30 : BumperCapZinc.transz=3
		case 3 : BumperRingZinc.z=-32 : BumperCapZinc.transz=2
		case 4 : BumperRingZinc.z=-28 : BumperCapZinc.transz=1
		case 5 : BumperRingZinc.z=-16 : BumperCapZinc.transz=0
		case 6 : BumperRingZinc.z=-8
		case 7 : BumperRingZinc.z=0
			Bumper003.timerenabled = False
	End Select
	Screwiron001.z = BumperCapZinc.transz+75	
	Screwiron002.z = BumperCapZinc.transz+75
End Sub

Sub Bumper004_hit
	DOF 108,2
	UndercabFlicker
	RandomSoundBumperLeft Bumper004
	PlaySoundat "sfx_bumper4",Bumper004
	If Tilted Then Exit Sub

	If BIP = 1 then
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play Seqblinking ,,1, 22
	End If
	bls 71,74,3,3,3,0
	bls 135,135,1,3,10,0

	Addscore 50

	check_BumperHurryup


	Bumper4Counter = Bumper4Counter + 1
	Select Case Bumper4Counter
		case 1 : If Blink(71,1)=2 Then Blink(71,1)=1 : CheckMode
		case 2 : If Blink(72,1)=2 Then Blink(72,1)=1 : CheckMode
		case 3 : If Blink(73,1)=2 Then Blink(73,1)=1 : CheckMode
		case 4 : If Blink(74,1)=2 Then Blink(74,1)=1 : CheckMode
	End Select

	bls 28,28,2,5,5,0
	Bumper004.timerenabled = True
	Bumper4Pos = 0
	BumperRingTin.z=-10
	Screwtin2.z=77
	Screwtin1.z=77
	BumperCapTin.transz=2
End Sub
Dim Bumper4Pos
Sub Bumper004_Timer
	Bumper4Pos = Bumper4Pos + 1
	Select Case Bumper4Pos
		case 1 : BumperRingTin.z=-20 : BumperCapTin.transz=4
		case 2 : BumperRingTin.z=-30 : BumperCapTin.transz=3
		case 3 : BumperRingTin.z=-32 : BumperCapTin.transz=2
		case 4 : BumperRingTin.z=-28 : BumperCapTin.transz=1
		case 5 : BumperRingTin.z=-16 : BumperCapTin.transz=0
		case 6 : BumperRingTin.z=-8
		case 7 : BumperRingTin.z=0
			Bumper004.timerenabled = False
	End Select
	Screwtin1.z = BumperCapTin.transz+75	
	Screwtin2.z = BumperCapTin.transz+75
End Sub


'*******************************************
'  Undercab DOF stuff
'*******************************************

Sub UndercabFlicker
	DOF 132,0
	DOF 133,0
	UndercabFlickerTimer.Enabled = True
End Sub

Sub UndercabFlickerTimer_Timer
	If Playercontrol = "Tweed" then
		DOF 132,1
		DOF 133,0
	Else
		DOF 132,0
		DOF 133,1
	End If
	UndercabFlickerTimer.Enabled = False
End Sub

'*******************************************
'  Kickers
'*******************************************
Sub Inv_StokeGuideRight_Hit
	activeball.velx = activeball.velx/2
	activeball.vely = activeball.vely/2
'	debug.print "stokewall hit :  x+y vel = " &  activeball.velx & " " & activeball.vely
End Sub

Sub StokeVUK_hit
	DOF 124,2
	RandomSoundEjectHoleEnter StokeVUK
	PlaySoundat "sfx_stokeenter",StokeVUK

	If Tilted Then 	StokeVUK.timerenabled = true : activeball.z = 25 : Exit Sub


	If DesktopMode then bls 144,146,2,5,5,2
	If b2son then bbs 9,-1,2,5 : bbs 10,-1,2,5 : bbs 11,-1,2,5

	bls 135,135,2,7,15,0
	bls 20,20,2,7,15,0


	AddScore 250

	TotalStokeHits(currentplayer) = TotalStokeHits(currentplayer) + 1
	If TotalStokeHits(CurrentPlayer) > 8 And HurryupStokeDone(CurrentPlayer) = 0 And Hurryup_Active = 0 then
		HurryupStokeDone(CurrentPlayer) = 1
		Hurryup_Active = 1
		HurryupTimerBumpers.Enabled = true		
		Audiospeed = 0
		Blink(15,1) = 2
		Blink(117,1)=2
		Blink(118,1)=2
		Blink(119,1)=2
		Blink(120,1)=2
		BLS 15,15,5,5,5,0
		voice_addaball
	End If



	If TotalStokeHits(CurrentPlayer) > 23 And HurryupStokeDone(CurrentPlayer) = 1 And Hurryup_Active = 0 then
		HurryupStokeDone(CurrentPlayer) = 2
		Hurryup_Active = 2
		HurryupTimerBumpers.Enabled = true		
		Audiospeed = 0
		Blink(15,1) = 2
		Blink(117,1)=2
		Blink(118,1)=2
		Blink(119,1)=2
		Blink(120,1)=2
		BLS 15,15,5,5,5,0
		voice_addaball

	End If

	If TotalStokeHits(CurrentPlayer) > 43 And HurryupStokeDone(CurrentPlayer) = 2 And Hurryup_Active = 0 then
		HurryupStokeDone(CurrentPlayer) = 3
		Hurryup_Active = 3
		HurryupTimerBumpers.Enabled = true		
		Audiospeed = 0
		Blink(15,1) = 2
		Blink(117,1)=2
		Blink(118,1)=2
		Blink(119,1)=2
		Blink(120,1)=2
		BLS 15,15,5,5,5,0
		voice_addaball
	End If

	If TotalStokeHits(CurrentPlayer) > 68 And HurryupStokeDone(CurrentPlayer) = 3 And Hurryup_Active = 0 then
		TotalStokeHits(CurrentPlayer) = 40 ' reset to 30 hits for next
		Hurryup_Active = 4
		HurryupTimerBumpers.Enabled = true		
		Audiospeed = 0
		Blink(15,1) = 2
		Blink(117,1)=2
		Blink(118,1)=2
		Blink(119,1)=2
		Blink(120,1)=2
		BLS 15,15,5,5,5,0
		voice_addaball
	End If


	StokeVUK.timerenabled = true

	activeball.z = 25
	gioff

	If RaiseTemp > 0 And Blink(22,1) = 0 then
		Blink(22,1)=1 : BLS 22,22,5,7,12,0
		If RaiseTemp = 1 then Blink(20,1) = 0 : CheckMode
	Elseif RaiseTemp > 1 And Blink(23,1) = 0 then
		Blink(23,1)=1 : BLS 23,23,5,7,12,0
		If RaiseTemp = 2 then Blink(20,1) = 0 : CheckMode
	Elseif RaiseTemp > 2 And Blink(24,1) = 0 then
		Blink(24,1) = 1 : BLS 24,24,5,7,12,0
		Blink(20,1) = 0 : CheckMode
	End If
'	debug.print "Raisetemp = " & RaiseTemp & "   " & blink(20,1)& blink(22,1) & blink(23,1) & blink(24,1) & "=doorlights"
End Sub


Sub stokeaddaballs
	If BIP = 1 And BallsToLaunch = 0 then Playsound "Volkan_Overture_II",-1,Music_Volume * 0.3
	BallsToLaunch = BallsToLaunch + 1
	drain.timerenabled = False
	drain.timerenabled = True
	bls 114,114,5,6,13,0
	AutoPlunger = True
	PlayVoice "sfx_anotherball",1,Voice_Volume,0
End Sub


Sub StokeVUK_timer
	if tilted Then 
		RandomSoundEjectHoleSolenoid StokeVUK
		PlaySoundAt "sfx_rampleft",StokeVUK
		StokeVUK.kick 277,70
		Exit Sub
	End if

	If Hurryup_Active > 0 then
		DOF 128,2
		bls 117,128,6,5,10,0

		playsound "sfx_newclanging",1,VolumeDial
		HurryupTimerBumpers.Enabled = False
		BLS 15,15,5,5,5,0
		Blink(15,1) = 0
		Blink(117,1)=0
		Blink(118,1)=0
		Blink(119,1)=0
		Blink(120,1)=0

		If Hurryup_Active = 5 then Howmuch = Howmuch + 1000' : debug.Print "howmuch " & howmuch & " hurry" 
		If Hurryup_Active = 6 then Howmuch = Howmuch + 2000' : debug.Print "howmuch " & howmuch & " hurry"  
		If Hurryup_Active = 7 then Howmuch = Howmuch + 3000'  : debug.Print "howmuch " & howmuch & " hurry" 
		If Hurryup_Active = 8 then Howmuch = Howmuch + 4000'  : debug.Print "howmuch " & howmuch & " hurry" 
		If Hurryup_Active = 1 then Howmuch = Howmuch + 1000 ' : debug.Print "howmuch " & howmuch & " hurry" 
		If Hurryup_Active = 2 then Howmuch = Howmuch + 2000'  : debug.Print "howmuch " & howmuch & " hurry" 
		If Hurryup_Active = 3 then Howmuch = Howmuch + 3000 ': debug.Print "howmuch " & howmuch & " hurry" 
		If Hurryup_Active = 4 then Howmuch = Howmuch + 4000': debug.Print "howmuch " & howmuch & " hurry" 

		Hurryup_Active = 0
		stokeaddaballs
	End If


	gion
	bls 135,135,1,4,5,0
	bls 20,20,1,4,5,0
	StokeVUK.timerenabled = False
	'PlaySoundat "sfx_vukexit",StokeVUK
	RandomSoundEjectHoleSolenoid StokeVUK
	PlaySoundAt "sfx_rampleft",StokeVUK
	StokeVUK.kick 277,70
	moveHopper = 1
	LightSeqAttract.UpdateInterval = 5
	LightSeqAttract.Play SeqDownOn, 40, 1 

	l084.timerenabled = True
	StokeKickerArm.roty = - 30

	If Blink(17,1)= 2 then
		DOF 127,1
		multiballtimer2.enabled = False
		UpPostRight.z = -10
		UpPostLeft.z = -10
		wall003.collidable = True
		wall004.collidable = True
	End If

End Sub

Sub l084_Timer
	If StokeKickerArm.roty < 0 then StokeKickerArm.roty = StokeKickerArm.roty + 5 Else L084.timerenabled = 0
End Sub

Dim moveHopper
Sub AnimCoalHopper
	If moveHopper > 0 Then
		moveHopper = moveHopper + 1
		Select Case moveHopper
			case  2 : CoalHopper.objrotx = 0.2
			case  3 : CoalHopper.objrotx = 0.5
			case  4 : CoalHopper.objrotx = 0.7
			case  5 : CoalHopper.objrotx = 0.85
			case  6 : CoalHopper.objrotx = 0.95
			case  7 : CoalHopper.objrotx = 1.0
			case  8 : CoalHopper.objrotx = 0.95
			case  9 : CoalHopper.objrotx = 0.85
			case 10 : CoalHopper.objrotx = 0.7
			case 11 : CoalHopper.objrotx = 0.5
			case 12 : CoalHopper.objrotx = 0.2
			case 13 : CoalHopper.objrotx = -0.2
			case 14 : CoalHopper.objrotx = -0.5
			case 15 : CoalHopper.objrotx = -0.7
			case 16 : CoalHopper.objrotx = -0.85
			case 17 : CoalHopper.objrotx = -0.95
			case 18 : CoalHopper.objrotx = -1.0
			case 19 : CoalHopper.objrotx = -0.95
			case 20 : CoalHopper.objrotx = -0.85
			case 21 : CoalHopper.objrotx = -0.7
			case 22 : CoalHopper.objrotx = -0.5
			case 23 : CoalHopper.objrotx = -0.2
			case 24 : CoalHopper.objrotx = -0.2
					moveHopper = 0
		End Select
	End If
End Sub



Dim CoalCounter
Sub CoalSaucerKick_hit
	DOF 125,2
	RandomSoundEjectHoleEnter CoalSaucerKick
	PlaySoundat "sfx_coalhit",CoalSaucerKick
	AddScore 50
	CoalSaucerKick.timerenabled = True
	CoalCounter = CoalCounter + 1
	If Blink(12,1)=2 Then

		PlayVoice "vo_stokeblast2",1,Voice_Volume,1
		Blink(12,1)=1 : BLS 12,14,5,4,4,1
		CheckMode : RaiseTemp = 1 : Blink(20,1)=2 : BLS 20,20,5,7,15,0
		If Blink(13,1)=0 Then
			Blink(11,1)=0 : BLS 11,11,5,4,4,0
		Else
			PlayVoice "vo_musthavecoal",1,Voice_Volume,0
		End If
	elseIf Blink(13,1)=2 Then
		PlayVoice "vo_stokeblast2",1,Voice_Volume,1
		Blink(13,1)=1 : BLS 12,14,5,4,4,1 
		CheckMode : RaiseTemp = 2 : Blink(20,1)=2 : BLS 20,20,5,7,15,0
		If Blink(14,1)=0 Then
			Blink(11,1)=0 : BLS 11,11,5,4,4,0
		Else
			PlayVoice "vo_musthavecoal",1,Voice_Volume,0
		End If
	elseIf Blink(14,1)=2 Then
		PlayVoice "vo_stokeblast2",1,Voice_Volume,1
		Blink(14,1)=1 : BLS 12,14,5,4,4,1 
		Blink(11,1)=0 : BLS 11,11,5,4,4,0
		CheckMode : RaiseTemp = 3 : Blink(20,1)=2 : BLS 20,20,5,7,15,0

	End If
End Sub
Sub CoalSaucerKick_timer
	CoalSaucerKick.timerenabled = False
	CoalSaucerKick.Kick 295,9
	L014.timerenabled = True
	CoalKickerArm.roty = -30

	PlaySoundat "sfx_coalkick",CoalSaucerKick
	bls 11,14,5,3,3,0
End Sub

Sub L014_Timer
	If CoalKickerArm.roty < 0 then CoalKickerArm.roty = CoalKickerArm.roty + 5 Else L014.timerenabled = 0
End Sub



'*******************************************
'  Spinners
'*******************************************

Dim RSpike
Dim RGugecount
Sub SteamSpinner_timer
	RSpike = int(rnd(1)*(20-RGugecount)) - RGugecount/2
	RGugecount = RGugecount + 1
	If RGugecount > 20 then
		RSpike = 0
		RGugecount = 0
		SteamSpinner.timerenabled = False
	End If
End Sub

Dim SteamElct_Vo
Sub SteamSpinner_Spin
	DOF 119,2
	BLS 57,57,4,8,3,10
	SoundSpinner SteamSpinner
	PlaySoundAt "sfx_steamspinner",SteamSpinner
	if Tilted Then Exit Sub
	SteamSpinner.timerenabled = True
	bls 41,49,2,3,3,1

	bls 138,138,1,4,10,0
	bls 115,115,4,4,8,0
	Addscore 10

	SteamCounter = SteamCounter + 1

	Select Case SteamCounter
		case  5 : If Blink(41,1) = 2 Then Blink(41,1) = 1 : GaugeRightPos =  63.0
		case 10 : If Blink(42,1) = 2 Then Blink(42,1) = 1 : GaugeRightPos =  71.5 : If Blink(43,1)=0 Then Blink(40,1)=0 : bls 40,49,5,4,4,0 : addscore 50 : CheckMode
		case 15 : If Blink(43,1) = 2 Then Blink(43,1) = 1 : GaugeRightPos =  80.0 : If Blink(44,1)=0 Then Blink(40,1)=0 : bls 40,49,5,4,4,0 : addscore 50 : CheckMode
		case 20 : If Blink(44,1) = 2 Then Blink(44,1) = 1 : GaugeRightPos =  88.5 : If Blink(45,1)=0 Then Blink(40,1)=0 : bls 40,49,5,4,4,0 : addscore 50 : CheckMode
		case 25 : If Blink(45,1) = 2 Then Blink(45,1) = 1 : GaugeRightPos =  97.0 : If Blink(46,1)=0 Then Blink(40,1)=0 : bls 40,49,5,4,4,0 : addscore 50 : CheckMode
		case 30 : If Blink(46,1) = 2 Then Blink(46,1) = 1 : GaugeRightPos = 105.5 : If Blink(47,1)=0 Then Blink(40,1)=0 : bls 40,49,5,4,4,0 : addscore 50 : CheckMode
		case 35 : If Blink(47,1) = 2 Then Blink(47,1) = 1 : GaugeRightPos = 114.0 : If Blink(48,1)=0 Then Blink(40,1)=0 : bls 40,49,5,4,4,0 : addscore 50 : CheckMode
		case 40 : If Blink(48,1) = 2 Then Blink(48,1) = 1 : GaugeRightPos = 122.5 : If Blink(49,1)=0 Then Blink(40,1)=0 : bls 40,49,5,4,4,0 : addscore 50 : CheckMode
		case 45 : If Blink(49,1) = 2 Then Blink(49,1) = 1 : GaugeRightPos = 131.0 :					   Blink(40,1)=0 : bls 49,49,5,4,4,0 : addscore 50 : CheckMode
	End Select

	If Blink(40,1)=0 And Blink(30,1)=2 and SteamElct_Vo = 0 then
		SteamElct_Vo = 1
		 Select Case int(rnd(1)*3) 
			case 0 : PlayVoice "vo_generatemoreelectricity",1,Voice_Volume ,0
			case 1 : PlayVoice "vo_moreelectricity",1,Voice_Volume,0
			case 2 : PlayVoice "vo_mustgenerateelectricity",1,Voice_Volume,0
		End Select
	End If
End Sub


Dim LSpike
Dim Lgugecount
Sub ElectricSpinner_timer
	LSpike = int(rnd(1)*(20-Lgugecount))- Lgugecount/2
	Lgugecount = Lgugecount + 1
	If Lgugecount = 20 then
		LSpike = 0
		Lgugecount = 0
		ElectricSpinner.timerenabled = False
	End If
End Sub

Sub ElectricSpinner_spin
	DOF 118,2
	SoundSpinner ElectricSpinner
	if Tilted Then Exit Sub

	ElectricSpinner.timerenabled = True
	bls 31,39,2,3,3,1
	ElTower = ElTower + 1

	

	bls 137,137,1,4,10,0
	bls 116,116,4,4,8,0
	Addscore 10

	ElectricCounter = ElectricCounter + 1

	Select Case ElectricCounter
		case  5 : If Blink(31,1) = 2 Then Blink(31,1) = 1 : GaugeLeftPos =  60 :  
		case 10 : If Blink(32,1) = 2 Then Blink(32,1) = 1 : GaugeLeftPos =  70 :  If Blink(33,1)=0 Then Blink(30,1)=0 : bls 39,39,5,4,4,0 : addscore 50 : CheckMode
		case 15 : If Blink(33,1) = 2 Then Blink(33,1) = 1 : GaugeLeftPos =  80 :  If Blink(34,1)=0 Then Blink(30,1)=0 : bls 39,39,5,4,4,0 : addscore 50 : CheckMode
		case 20 : If Blink(34,1) = 2 Then Blink(34,1) = 1 : GaugeLeftPos =  90 :  If Blink(35,1)=0 Then Blink(30,1)=0 : bls 39,39,5,4,4,0 : addscore 50 : CheckMode
		case 25 : If Blink(35,1) = 2 Then Blink(35,1) = 1 : GaugeLeftPos = 100 :  If Blink(36,1)=0 Then Blink(30,1)=0 : bls 39,39,5,4,4,0 : addscore 50 : CheckMode
		case 30 : If Blink(36,1) = 2 Then Blink(36,1) = 1 : GaugeLeftPos = 110 :  If Blink(37,1)=0 Then Blink(30,1)=0 : bls 39,39,5,4,4,0 : addscore 50 : CheckMode
		case 35 : If Blink(37,1) = 2 Then Blink(37,1) = 1 : GaugeLeftPos = 120 :  If Blink(38,1)=0 Then Blink(30,1)=0 : bls 39,39,5,4,4,0 : addscore 50 : CheckMode
		case 40 : If Blink(38,1) = 2 Then Blink(38,1) = 1 : GaugeLeftPos = 130 :  If Blink(39,1)=0 Then Blink(30,1)=0 : bls 39,39,5,4,4,0 : addscore 50 : CheckMode
		case 45 : If Blink(39,1) = 2 Then Blink(39,1) = 1 : GaugeLeftPos = 140 :						   Blink(30,1)=0 : bls 30,30,5,4,4,0 : addscore 50 : CheckMode
	End Select
	If Blink(30,1)=0 And Blink(40,1)=2 and SteamElct_Vo = 0 then
		SteamElct_Vo = 1
		Select Case int(rnd(1)*3)
			case 0 : PlayVoice "vo_enoughsteam",1,Voice_Volume ,0
			case 1 : PlayVoice "vo_moresteam",1,Voice_Volume,0
			case 2 : PlayVoice "vo_needmoresteam",1,Voice_Volume,0
		End Select
	End If

End Sub

'*******************************************
'  StandUpTargets
'*******************************************

Sub SUT5_Hit
	DOF 136,2
	RandomSoundTargetHit
	If Tilted then Exit Sub

	bls 21,21,3,5,15,0
	bls 16,16,3,5,15,3
	bls 50,50,3,5,15,6

	If Blink(16,1) = 2 Then
		PlaySoundAt "sfx_carbon",sut5
		Blink(16,1) = 1
		Addscore 250
	Elseif Blink(21,1)=2 then
		PlaySoundAt "sfx_carbon",sut5
		Blink(21,1) = 1
		Addscore 250
	Else
		Addscore 50
	End If

	If Blink(50,1)=2 Then RewardExtraBall : DOF 126,2

	If EB_Carbons(CurrentPlayer) < 25 Then
		EB_Carbons(CurrentPlayer) = EB_Carbons(CurrentPlayer) + 1
'		debug.print "Carbons = " & EB_Carbons(CurrentPlayer)
		If EB_Carbons(CurrentPlayer) = 25 and EB_Collected(CurrentPlayer) < MaxExtraBalls Then Lite_EB_insert
	End If

End Sub

Sub RewardExtraBall
	addscore 500
'	debug.print "RewardExtraBall"
	blink(50,1) = 0
	bls 50,50,10,5,5,0

	If  EB_Levels(CurrentPlayer) = 1  Then  
		EB_Levels(CurrentPlayer) = 2
	elseIf EB_Inlanes(CurrentPlayer) = 25 Then 
		EB_Inlanes(CurrentPlayer) = 16
	elseIf EB_Targets(CurrentPlayer) = 5  Then 
		EB_Targets(CurrentPlayer) = 6
	elseIf EB_Carbons(CurrentPlayer) = 25 Then 
		EB_Carbons(CurrentPlayer) = 26
	End If

	EB_Ready(CurrentPlayer) = EB_Ready(CurrentPlayer) + 1
'	debug.print "Extra Balls Ready = " & EB_Ready(CurrentPlayer)
	blink(105,1) = 1
	bls 105,105,12,8,8,0

	EB_Collected(CurrentPlayer) = EB_Collected(CurrentPlayer) + 1

	If EB_Collected(CurrentPlayer) < MaxExtraBalls Then
		If EB_Levels(CurrentPlayer) = 1  Then Blink(50,1) = 2
		If EB_Inlanes(CurrentPlayer) = 25 Then Blink(50,1) = 2
		If EB_Targets(CurrentPlayer) = 5  Then Blink(50,1) = 2
		If EB_Carbons(CurrentPlayer) = 25 Then Blink(50,1) = 2
	End If
	ClearVoiceQ
	If Int(rnd(1)*2) = 1 Then
		PlayVoice "vo_extraballimpressive",1,Voice_Volume,0
	Else
		PlayVoice "vo_extraballproven",1,Voice_Volume,0
	End If
End Sub

Sub Lite_EB_insert
	blink(50,1)=2
	bls 50,50,7,10,5,0

	ClearVoiceQ
	
	If Int(rnd(1)*2) = 1 Then
		PlayVoice "vo_extraball",1,Voice_Volume,0
	Else
		PlayVoice "vo_shootextraball",1,Voice_Volume,0
	End If
End Sub


Sub check_PFmulti
	Order_Value_Counter = Order_Value_Counter + 1
	Select Case Order_Value_Counter
		case 4 : 
			Blink(102,1)=1
			bls 102,102,5,7,7,0
		case 8 :
			Blink(103,1)=1
			bls 103,103,5,7,7,0
	End Select
End Sub





Dim TimeToTalk
Sub UpdateControl


	If DesktopMode and VRRoom = 0 then
		If Playercontrol = "Tweed" then
			Light003.state = 2 : Light004.state = 2
			If CurrentPlayer = 1 then
				Light011.state = 0 : Light012.state = 0
			Else
				Light011.state = 1 : Light012.state = 1
			End If
		Else
			Light011.state = 2 : Light012.state = 2
			If CurrentPlayer = 2 then
				Light003.state = 0 : Light004.state = 0
			Else
				Light003.state = 1 : Light004.state = 1
			End If
		End If
	End If
	If b2son then
		If Playercontrol = "Tweed" then
			bbs 1,0,99999,8
			If CurrentPlayer = 1 then
				bbs  2,0,0,0
			Else
				bbs 2,1,0,0
			End If
		Else
			bbs 2,0,99999,8
			If CurrentPlayer = 2 then
				bbs 1,0,0,0
			Else
				bbs 1,1,0,0
			End If
		End If
	End If

	If CurrentPlayer = 1 Then
		If Playercontrol = "Tweed" Then DOF 134,0 Else DOF 134,1
	Else
		If Playercontrol = "Tweed" Then DOF 134,1 Else DOF 134,0
	End If

	If Playercontrol = "Tweed" then
		DOF 132,1
		DOF 133,0
	Else
		DOF 132,0
		DOF 133,1
	End If


	If CurrentPlayer = 1 then
		If desktopmode then Blink(143,1)=1 : Blink(142,1)=0
		If b2son then bbs 9,1,0,0 : bbs 10,0,0,0
		Select Case Playercontrol
			Case "Tweed" : Blink(110,1) = 1 : Blink(111,1) = 0
						   Blink(113,1) = 1 : bls 113,113,3,3,5,0
							volkanlight.color = rgb(34,222,24)
							volkanlight.colorfull = rgb(141,241,141)
							volkanlight.state = 1
							p31.material = "InsertGreenOn1"
			Case "Baron" : Blink(110,1) = 0 : Blink(111,1) = 2
						   Blink(113,1) = 2 : bls 113,113,7,5,15,0
							volkanlight.color = rgb(34,24,222)
							volkanlight.colorfull = rgb(141,141,251)
							volkanlight.state = 2
							p31.material = "InsertBlueOn1" 

		End Select
	Else
		If desktopmode then Blink(143,1)=0 : Blink(142,1)=1
		If b2son then bbs 9,0,0,0 : bbs 10,1,0,0
		Select Case Playercontrol
			Case "Tweed" : Blink(110,1) = 2 : Blink(111,1) = 0
						   Blink(113,1) = 2 : bls 113,113,7,5,15,0
							volkanlight.color = rgb(34,222,24)
							volkanlight.colorfull = rgb(141,241,141)
							volkanlight.state = 2
							p31.material = "InsertGreenOn1"

			Case "Baron" : Blink(110,1) = 0 : Blink(111,1) = 1
						   Blink(113,1) = 1 : bls 113,113,3,3,5,0
							volkanlight.color = rgb(34,24,222)
							volkanlight.colorfull = rgb(141,141,251)
							volkanlight.state = 1
							p31.material = "InsertBlueOn1"
		End Select
	End If
End Sub
Dim Playercontrol : Playercontrol = "Tweed"

Sub SUT1_Hit
	DOF 130,2
	RandomSoundTargetHit
	If Tilted then Exit Sub

	If ControlLock then Exit Sub
	If CurrentPlayer=1 and BaronControl > 0 And gametime - BaronControl > 8000 then PlayVoice "Tweed_IFinallyHaveControlAgain",1,Voice_Volume,0
	If Not Playercontrol = "Tweed" Then check_PFmulti : PlaySoundAt "sfx_takeback",SUT1 : TweedControl = gametime : BaronControl = 0
	addscore 30
	Playercontrol = "Tweed" 
	UpdateControl
End Sub


Sub SUT2_Hit
	DOF 130,2
	RandomSoundTargetHit
	If Tilted then Exit Sub

	If ControlLock then Exit Sub
	If CurrentPlayer=2 and TweedControl > 0 And gametime - TweedControl > 8000 then PlayVoice "baron_FinallyHaveControl",1,Voice_Volume,0
	If Playercontrol = "Tweed" Then check_PFmulti : PlaySoundAt "sfx_stealcontrol",SUT2 : BaronControl = gametime : TweedControl = 0
	addscore 30
	Playercontrol = "Baron"
	UpdateControl
End sub

Sub SUT3_Hit
	DOF 131,2
	RandomSoundTargetHit
	If Tilted then Exit Sub

	If ControlLock then Exit Sub
	If CurrentPlayer=1 and BaronControl > 0 And gametime - BaronControl > 8000 then PlayVoice "Tweed_IFinallyHaveControlAgain",1,Voice_Volume,0
	If Not Playercontrol = "Tweed" Then check_PFmulti : PlaySoundAt "sfx_takeback",SUT3 : TweedControl = gametime : BaronControl = 0
	addscore 30
	Playercontrol = "Tweed"
	UpdateControl
End sub

Sub SUT4_Hit
	DOF 131,2
	RandomSoundTargetHit
	If Tilted then Exit Sub

	If ControlLock then Exit Sub
	If CurrentPlayer=2 and TweedControl > 0 And gametime - TweedControl > 8000 then PlayVoice "baron_ImInControlNow",1,Voice_Volume,0
	If Playercontrol = "Tweed" Then check_PFmulti : PlaySoundAt "sfx_stealcontrol",SUT4 : BaronControl = gametime : TweedControl = 0
	addscore 30
	Playercontrol = "Baron"
	UpdateControl
End sub


'*******************************************
'  DropTargets
'*******************************************

Dim DroppedDT(4) : DroppedDT(1)=0 : DroppedDT(2)=0 : DroppedDT(3)=0 : DroppedDT(4)=0

Sub DT1_Hit
	DOF 121,2
	UndercabFlicker
	dt1.collidable = 0
	If DroppedDT(1) = 0 Then
		RandomSoundTargetHit
		SoundDropTargetDrop DT1
		DTShadow001.visible = 0
		DroppedDT(1)=1
		DT001.transy = -3
		DT001.objroty = -2
		CheckDT_bank
		addscore 300
		If Blink(91,1)=2 Then Blink(91,1) = 1 : BLS 91,94,3,7,7,0 : AddScore 500 : PlaySoundAt "sfx_DThit", DT1 : CheckMode
		addstront
	End If
End Sub

Sub addstront
	If Blink(91,1)=2 or Blink(92,1)=2 or Blink(93,1)=2 or Blink(94,1)=2 then
		If timetotalk < 1 then 
			timetotalk = 5 
			Select Case int(rnd(1)*2)
				case 0 : playsound "vo_correctstrontium",1,Voice_Volume
				case 1 : playsound "vo_addstrontium",1,Voice_Volume
			End Select
		End If
	End If
End Sub

Sub DT2_Hit
	DOF 121,2
	UndercabFlicker
	dt2.collidable = 0
	If DroppedDT(2) = 0 Then
		RandomSoundTargetHit
		SoundDropTargetDrop DT2
		DTShadow002.visible = 0
		DroppedDT(2)=1
		DT002.transy = -3
		DT002.objroty = -2
		CheckDT_bank
		addscore 300
		If Blink(92,1)=2 Then Blink(92,1) = 1 : BLS 91,94,3,7,7,0 : AddScore 500 : PlaySoundAt "sfx_DThit", DT2 : CheckMode
		addstront
	End If
End Sub
Sub DT3_Hit
	DOF 121,2
	UndercabFlicker
	dt3.collidable = 0
	If DroppedDT(3) = 0 Then
		RandomSoundTargetHit
		SoundDropTargetDrop DT3
		DTShadow003.visible = 0
		DroppedDT(3)=1
		DT003.transy = -3
		DT003.objroty = -2
		CheckDT_bank
		addscore 300
		If Blink(93,1)=2 Then Blink(93,1) = 1 : BLS 91,94,3,7,7,0 : AddScore 500 : PlaySoundAt "sfx_DThit", DT3 : CheckMode
		addstront
	End If
End Sub
Sub DT4_Hit
	DOF 121,2
	UndercabFlicker
	dt4.collidable = 0
	If DroppedDT(4) = 0 Then
		RandomSoundTargetHit
		SoundDropTargetDrop DT4
		DTShadow004.visible = 0
		DroppedDT(4)=1
		DT004.transy = -3
		DT004.objroty = -2
		CheckDT_bank
		addscore 300
		If Blink(94,1)=2 Then Blink(94,1) = 1 : BLS 91,94,3,7,7,0 : AddScore 500 : PlaySoundAt "sfx_DThit", DT4 : CheckMode
		addstront
	End If
End Sub

Sub AnimDT
	If DroppedDT(1) = 1 And DT001.z > -10 Then DT001.z = DT001.z - 5 : DT001.transy = DT001.transy  *0.85 : DT001.objroty = DT001.objroty *0.85
	If DroppedDT(2) = 1 And DT002.z > -10 Then DT002.z = DT002.z - 5 : DT002.transy = DT002.transy  *0.85 : DT002.objroty = DT002.objroty *0.85
	If DroppedDT(3) = 1 And DT003.z > -10 Then DT003.z = DT003.z - 5 : DT003.transy = DT003.transy  *0.85 : DT003.objroty = DT003.objroty *0.85
	If DroppedDT(4) = 1 And DT004.z > -10 Then DT004.z = DT004.z - 5 : DT004.transy = DT004.transy  *0.85 : DT004.objroty = DT004.objroty *0.85



	If DroppedDT(1) = 0 Then 
		If DT001.z < 40 Then DT001.z = DT001.z + 9 Else DT001.z = 40
	End If
	If DroppedDT(2) = 0 Then 
		If DT002.z < 40 Then DT002.z = DT002.z + 9 Else DT002.z = 40
	End If
	If DroppedDT(3) = 0 Then 
		If DT003.z < 40 Then DT003.z = DT003.z + 9 Else DT003.z = 40
	End If
	If DroppedDT(4) = 0 Then 
		If DT004.z < 40 Then DT004.z = DT004.z + 9 Else DT004.z = 40
	End If
End Sub


Sub CheckDT_bank
	If DroppedDT(1)=1 and DroppedDT(2)=1 and DroppedDT(3)=1 and DroppedDT(4)=1 Then
		DT1.timerenabled = True
		Addscore 4000

		If EB_Targets(CurrentPlayer) < 5 Then
			EB_Targets(CurrentPlayer) = EB_Targets(CurrentPlayer) + 1
'			debug.print "EB_Targets = " & EB_Targets(CurrentPlayer)
			If EB_Targets(CurrentPlayer) = 5 and EB_Collected(CurrentPlayer) < MaxExtraBalls Then Lite_EB_insert
		End If


	End If
End Sub






Sub DT1_Timer
	DroppedDT(1)=0 : DroppedDT(2)=0 : DroppedDT(3)=0 : DroppedDT(4)=0
	DT1.timerenabled = False
	DTShadow001.visible = 1
	DTShadow002.visible = 1
	DTShadow003.visible = 1
	DTShadow004.visible = 1
	dt1.isdropped = 0
	dt2.isdropped = 0
	dt3.isdropped = 0
	dt4.isdropped = 0
	dt1.collidable = 1
	dt2.collidable = 1
	dt3.collidable = 1
	dt4.collidable = 1
	RandomSoundDropTargetReset dt2
	RandomSoundDropTargetReset dt4
End Sub


tunnel002.blenddisablelighting = 70
tunnel003.blenddisablelighting = 70
tunnel004.blenddisablelighting = 70
tunnel005.blenddisablelighting = 70
tunnel006.blenddisablelighting = 70




'*******************************************
'  Triggers
'*******************************************
Dim AutoPlungerBall
Sub PlungerTrigger_hit
	activeball.image = "ballLight_HDR"
	DOF 114,1
	BIPL=1
	bls 139,139,9999,4,14,0
	If AutoPlunger = true then set AutoPlungerBall= activeball : PlungerTrigger.timerenabled = True
End Sub

Sub PlungerTrigger_Timer
	PlungerTrigger.timerenabled = False
	RandomSoundAutoPlunger Plunger
	PlaySound "sfx_launch",1,VolumeDial

	AutoPlungerBall.vely = -50
	AutoPlungerBall.velx = 0
	DOF 120,2
	plungerkicker.x = 840
	plungerlight.timerenabled = 1
End Sub
Sub PlungerTrigger_unhit
	DOF 114,0
	BIPL=0
	If Tilted Then Exit Sub
	bls 139,139,5,4,4,0
	LightSeqAttract.UpdateInterval = 5
	LightSeqAttract.Play SeqUpOn, 40, 1 

	If PlayerSelect then
		PlaySound "sfx_stealcontrol",1,VolumeDial
		BaronControl = 0
		TweedControl = 0
		Select Case PlayerChoosen
			Case "Tweed"
				If DesktopMode then Blink(143,1) = 1 : Blink(142,1)=0
				If b2son then bbs 1,1,0,0 : bbs 2,0,0,0
				CurrentPlayer = 1
				Playercontrol = "Tweed"
				PlayVoice "Tweed_WeHaveABusinessToRun",1,Voice_Volume,1 
			Case "Baron"
				If DesktopMode then Blink(143,1) = 0 : Blink(142,1)=1
				If b2son then bbs 1,0,0,0 : bbs 2,1,0,0
				CurrentPlayer = 2
				Playercontrol = "Baron"
				PlayVoice "baron_ImResponsibleForVolkan",1,Voice_Volume,1 
		End Select
		UpdateControl
	End If
	Playerselect = False

End Sub
Sub plungerlight_Timer
	If plungerkicker.x < 870 then plungerkicker.x = plungerkicker.x + 5 : Else plungerlight.timerenabled = 0
End Sub


'*******************************************
'  Ramp Triggers (and helpers)
'*******************************************

Sub TriggerRamp1_Hit
	activeball.angmomx=0
	activeball.angmomy=0
	activeball.angmomz=0
	activeball.velx=0
	activeball.vely=0
End Sub

Sub TriggerRamp2_Hit
	activeball.angmomx=0
	activeball.angmomy=0
	activeball.angmomz=0
	activeball.velx=0
	activeball.vely=0

End Sub



'TUNNEL LIGHT EFFECTS
Sub Tunnel1_hit
	activeball.vely = 3
	gioff
	PlaySound "sfx_tunnel",1,VolumeDial
	Tunnel001.blenddisablelighting = 15
	tunnel006.visible = 1
	RampWireLeft001.visible = 1
End Sub
Sub Tunnel2_hit	
	activeball.vely = 2
	Tunnel001.blenddisablelighting = 18
	tunnel006.visible = 0
	tunnel005.visible = 1
	tunnel004.visible = 1
End Sub
Sub Tunnel3_hit	
	activeball.vely = 1
	Tunnel001.blenddisablelighting = 22
	tunnel005.visible = 0
	tunnel004.visible = 0
	tunnel003.visible = 1
End Sub
Sub Tunnel4_hit
	Light002.State = 1
	Tunnel001.blenddisablelighting = 30
	tunnel003.visible = 0
	tunnel002.visible = 1
End Sub
Sub Tunnel5_unhit
	Tunnel001.blenddisablelighting = 0
	tunnel002.visible = 0
	RampWireLeft001.visible = 0
	Light002.State = 0
End Sub


Dim BlastdoorMB
Sub Tunnel5_Hit
	If Tilted Then Exit Sub
	gion
	If Blink(17,1)=2 then
		BlastdoorMB = 1 
		MaxMB_JPs = 15
		ClearVoiceQ
		Blink(134,1)=2 ' trainlights
		Blink(133,1)=0
		bls 133,133,3,7,7,0
		bls 134,134,1,7,7,15
		DoorF.rotatetoend  
		Blink(121,1)=2
		Blink(122,1)=2
		Blink(123,1)=2
		Blink(124,1)=2

		PlaySound "sfx_furnacedoor",1,VolumeDial
		blastfurnaceReady.enabled = True
		Blink(17,1)=1
		BallsToLaunch =  BallsToLaunch + 2
		drain.timerenabled = False
		drain.timerenabled = True
		AutoPlunger = True
		bls 114,114,5,6,13,0
		stopsound "Volkan_Overture_II"
		PlaySoundAt "sfx_returncontrol", swTrough1
		PlaySound "sfx_multiballbeat",-1,Music_Volume * 0.9
		bbs 12, 1, 12, 30 : bbs 13, 1, 14, 27 : bbs 14, 1, 12, 31 : bbs 15, 1, 13, 28 : bbs 16, 1, 14, 25
		ClearVoiceQ
	End If
End Sub

Sub Tunnel007_hit
	If Tilted Then Exit Sub
'	PlaySoundAt "fx_collide",tunnel007
'	PlaySoundAt "fx_collide",Tunnel005
	If Blink(17,1) = 1 Then
		Blink(104,1) = 0
		Blink(17,1) = 0  
		bls 104,105,5,5,9,0
		Blink(19,1)=0

		Tunnel007.timerenabled = True

		LightSeqAttract.stopplay
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play seqalloff
		blasttriggerON = 0
		ClearVoiceQ
			
		if int(rnd(1)*1) = 1 then 
			PlayVoice "vo_multiballnow",1,Voice_Volume,0 
		Else 
			PlayVoice "vo_timeforMB",1,Voice_Volume,0
		End If
		bbs 17, 1, 14, 13

		PlayVoice "vo_controllocked",1,Voice_Volume,0

		Doublescoring = True
		Blink(29,1) = 2
		PlayVoice  "vo_doubleproduction",1,Voice_Volume,0

		Blink(18,1)=2
		ballsave = 10 + MaxBallsave
'		debug.print "BallSave20sec"
		ballsaveActivated = 0
	End If
End Sub	

Dim ControlLock 
Sub LockControl
	If CurrentPlayer = 1 then Playercontrol = "Tweed" Else Playercontrol = "Baron"
		BaronControl = 0
		TweedControl = 0
	UpdateControl
	ControlLock = True
End Sub
' fixing	reset multilights and release 2 ballsaa
Sub Tunnel007_Timer
		PlaySound "sfx_multiballdrone",1,volumedial * 3
		Tunnel007.timerenabled = False
		RandomSoundDropTargetReset dt2
		RandomSoundDropTargetReset dt4
		AddScore 100

		bls 17,17,8,7,7,0
		multiballtimer.enabled = False
		multiballtimer2.enabled = False

		UpPostRight.z = -40
		UpPostLeft.z = -40
		wall003.collidable = False
		wall004.collidable = False
		ballsave = 10 + MaxBallsave
'		debug.print "BallSave20sec"
		ballsaveActivated = 1
		LockControl
		Blink(18,1)=2
End Sub

Sub Gate001_hit
	SoundBallGate0 Gate001
End Sub
Sub Gate002_hit
	activeball.velx = - 1
	SoundBallGate1 Gate002

	If AutoPlunger = True then 
		AutoPlunger = False 
		If BallsToLaunch > 0 then 
			AutoPlunger = True
			drain.timerenabled = False
			drain.timerenabled = True
			bls 114,114,5,6,13,0

		End If
	End If

End Sub

Sub Star1_Hit
	PlaySoundat "sfx_star1",Star1

	Blink(106,1)=1
	Blink(98,1)=1
	Addscore 50
	If Blink(96,1)=1 And Blink(97,1)=1 And Blink(98,1)=1 And Blink(99,1)=1 Then Addscore 270 else Addscore 10

	bls 106,109,2,5,5,0
	bls 106,106,5,3,3,0

	pstar106.z = - 4
	Star1.timerenabled = True
	Star1D = 0
End Sub
Sub Star1_UnHit
	pstar106.z = - 5
	Star1.timerenabled = True
	Star1D = 1
End Sub
Dim Star1D
Sub Star1_Timer
	If Star1D = 0 Then pstar106.z = - 7 Else pstar106.z = - 2
	Star1.timerenabled = False
End Sub


Sub Star2_Hit
	PlaySoundat "sfx_star2",Star1

	Blink(107,1)=1
	Blink(96,1)=1
	Addscore 50
	If Blink(96,1)=1 And Blink(97,1)=1 And Blink(98,1)=1 And Blink(99,1)=1 Then Addscore 270 else Addscore 10

	bls 106,109,2,5,5,0
	bls 107,107,5,3,3,0
	pstar107.z = - 4
	Star2.timerenabled = True
	Star2D = 0
End Sub
Sub Star2_UnHit
	pstar107.z = - 5
	Star2.timerenabled = True
	Star2D = 1
End Sub
Dim Star2D
Sub Star2_Timer
	If Star2D = 0 Then pstar107.z = - 7 Else pstar107.z = - 2
	Star2.timerenabled = False
End Sub

Sub Star3_Hit
	PlaySoundat "sfx_star3",Star3

	Blink(108,1)=1
	Blink(97,1)=1
	Addscore 50
	If Blink(96,1)=1 And Blink(97,1)=1 And Blink(98,1)=1 And Blink(99,1)=1 Then Addscore 270 else Addscore 10

	bls 106,109,2,5,5,0
	bls 108,108,5,3,3,0
	pstar108.z = - 4
	Star3.timerenabled = True
	Star3D = 0
End Sub
Sub Star3_UnHit
	pstar108.z = - 5
	Star3.timerenabled = True
	Star3D = 1
End Sub
Dim Star3D
Sub Star3_Timer
	If Star3D = 0 Then pstar108.z = - 7 Else pstar108.z = - 2
	Star3.timerenabled = False
End Sub

Sub Star4_Hit
	PlaySoundat "sfx_star4",star4

	Blink(109,1)=1
	Blink(99,1)=1
	Addscore 50
	If Blink(96,1)=1 And Blink(97,1)=1 And Blink(98,1)=1 And Blink(99,1)=1 Then Addscore 270 else Addscore 10

	bls 106,109,2,5,5,0
	bls 109,109,5,3,3,0
	pstar109.z = - 4
	Star4.timerenabled = True
	Star4D = 0
End Sub
Sub Star4_UnHit
	pstar109.z = - 5
	Star4.timerenabled = True
	Star4D = 1
End Sub
Dim Star4D
Sub Star4_Timer
	If Star4D = 0 Then pstar109.z = - 7 Else pstar109.z = - 2
	Star4.timerenabled = False
End Sub



Sub RightInlane_hit
	RandomSoundRollover
	Playsoundat "sfx_inlane2",rightinlane

	If Blink(97,1)=1 then
		Addscore 1000
		BLS 97,97,12,5,5,0

		If EB_Inlanes(CurrentPlayer) < 25 Then
			EB_Inlanes(CurrentPlayer) = EB_Inlanes(CurrentPlayer) + 1
'			debug.print "EB_inlanes = " & EB_Inlanes(CurrentPlayer)
			If EB_Inlanes(CurrentPlayer) = 15 and EB_Collected(CurrentPlayer) < MaxExtraBalls Then Lite_EB_insert
		End If


	Else
		Addscore 100
		BLS 97,97,3,5,5,0
	End If
	Blink(96,1)=0 : Blink(97,1)=0 : Blink(98,1)=0 : Blink(99,1)=0
	Blink(106,1)=0 : Blink(107,1)=0 : Blink(108,1)=0 : Blink(109,1)=0

	T_rightinlaneSpinS = Abs (( activeball.vely * 2 ) + 1 )
	If activeball.vely > 0 then T_rightinlaneSpinD = 1 Else T_rightinlaneSpinD = 0
	RightInlane.timerenabled = True
	WheelRORightIn.z = -25
	T_rightinlanePos = 0
	T_rightinlaneDir = 0
End Sub

Dim T_rightinlaneDir
Dim T_rightinlanePos
Dim T_rightinlaneSpinS
Dim T_rightinlaneSpinD
Dim T_rightinlaneSpinP
Sub RightInlane_unhit
	RightInlane.timerenabled = False
	RightInlane.timerenabled = True
	WheelRORightIn.z = -35
	T_rightinlanePos = 0
	T_rightinlaneDir = 1
	T_rightinlaneSpinS = T_rightinlaneSpinS + 0.3
End Sub
Sub RightInlane_Timer
	T_rightinlanePos = T_rightinlanePos + 1 
	If T_rightinlaneDir = 0 Then
		Select Case T_rightinlanePos
			Case 1
				WheelRORightIn.z = -35
			Case 2
				WheelRORightIn.z = -40
		End Select
	Else
		Select Case T_rightinlanePos
			Case 1
				WheelRORightIn.z = -25
			Case 2
				WheelRORightIn.z = -15
		End Select
	End If
	T_rightinlaneSpinS = T_rightinlaneSpinS - 0.05
	If T_rightinlaneSpinS < 0.1 Then RightInlane.timerenabled = False
	
	If T_rightinlaneSpinD = 0 Then
		T_rightinlaneSpinP = ( T_rightinlaneSpinP + T_rightinlaneSpinS ) Mod 360
	else
		T_rightinlaneSpinP = ( T_rightinlaneSpinP - T_rightinlaneSpinS ) Mod 360
	End If
	WheelRORightIn.rotx = T_rightinlaneSpinP
End Sub

Sub Rightoutlane_hit
	RandomSoundRollover
	RandomSoundOutlaneRollover
	Playsoundat "sfx_outlane",RightInlane


	If Blink(99,1)=1 then
		Addscore 1000
		if Ballsave < - 1 Then addscore 2500 : If BIP = 1 then Addscore 2500
		BLS 99,99,12,5,5,0

'		If EB_Inlanes(CurrentPlayer) < 15 Then
'			EB_Inlanes(CurrentPlayer) = EB_Inlanes(CurrentPlayer) + 1
'			debug.print "EB_inlanes = " & EB_Inlanes(CurrentPlayer)
'			If EB_Inlanes(CurrentPlayer) = 15 and EB_Collected(CurrentPlayer) < MaxExtraBalls Then Lite_EB_insert
'		End If

	Else
		Addscore 100
		BLS 99,99,3,5,5,0
	End If
	Blink(96,1)=0 : Blink(97,1)=0 : Blink(98,1)=0 : Blink(99,1)=0
	Blink(106,1)=0 : Blink(107,1)=0 : Blink(108,1)=0 : Blink(109,1)=0

	T_rightoutlaneSpinS = Abs (( activeball.vely * 2 ) + 1 )
	If activeball.vely > 0 then T_rightoutlaneSpinD = 1 Else T_rightoutlaneSpinD = 0
	Rightoutlane.timerenabled = True
	WheelRORightout.z = -25
	T_rightoutlanePos = 0
	T_rightoutlaneDir = 0
End Sub
Dim T_rightoutlaneDir
Dim T_rightoutlanePos
Dim T_rightoutlaneSpinS
Dim T_rightoutlaneSpinD
Dim T_rightoutlaneSpinP
Sub RightOutlane_unhit
	Rightoutlane.timerenabled = False
	Rightoutlane.timerenabled = True
	WheelRORightout.z = -35
	T_rightoutlanePos = 0
	T_rightoutlaneDir = 1
	T_rightoutlaneSpinS = T_rightoutlaneSpinS + 0.3
End Sub
Sub Rightoutlane_Timer
	T_rightoutlanePos = T_rightoutlanePos + 1 
	If T_rightoutlaneDir = 0 Then
		Select Case T_rightoutlanePos
			Case 1
				WheelRORightout.z = -35
			Case 2
				WheelRORightout.z = -40
		End Select
	Else
		Select Case T_rightoutlanePos
			Case 1
				WheelRORightout.z = -25
			Case 2
				WheelRORightout.z = -15
		End Select
	End If
	T_rightoutlaneSpinS = T_rightoutlaneSpinS - 0.05
	If T_rightoutlaneSpinS < 0.1 Then Rightoutlane.timerenabled = False
	
	If T_rightoutlaneSpinD = 0 Then
		T_rightoutlaneSpinP = ( T_rightoutlaneSpinP + T_rightoutlaneSpinS ) Mod 360
	else
		T_rightoutlaneSpinP = ( T_rightoutlaneSpinP - T_rightoutlaneSpinS ) Mod 360
	End If
	WheelRORightout.rotx = T_rightoutlaneSpinP
End Sub


Sub LeftInlane_hit
	RandomSoundRollover
	Playsoundat "sfx_inlane2",LeftInlane
	BLS 96,96,3,5,5,0

	If Blink(96,1)=1 then
		Addscore 1000
		BLS 96,96,12,5,5,0

		If EB_Inlanes(CurrentPlayer) < 25 Then
			EB_Inlanes(CurrentPlayer) = EB_Inlanes(CurrentPlayer) + 1
'			debug.print "EB_inlanes = " & EB_Inlanes(CurrentPlayer)

			If EB_Inlanes(CurrentPlayer) = 15 and EB_Collected(CurrentPlayer) < MaxExtraBalls Then Lite_EB_insert
		End If

	Else
		Addscore 100
		BLS 96,96,3,5,5,0
	End If
	Blink(96,1)=0 : Blink(97,1)=0 : Blink(98,1)=0 : Blink(99,1)=0
	Blink(106,1)=0 : Blink(107,1)=0 : Blink(108,1)=0 : Blink(109,1)=0


	T_LeftinlaneSpinS = Abs (( activeball.vely * 2 ) + 1 )
	If activeball.vely > 0 then T_LeftinlaneSpinD = 1 Else T_LeftinlaneSpinD = 0
	LeftInlane.timerenabled = True
	WheelROLeftIn.z = -25
	T_LeftinlanePos = 0
	T_LeftinlaneDir = 0
End Sub
Dim T_LeftinlaneDir
Dim T_LeftinlanePos
Dim T_LeftinlaneSpinS
Dim T_LeftinlaneSpinD
Dim T_LeftinlaneSpinP
Sub LeftInlane_unhit
	LeftInlane.timerenabled = False
	LeftInlane.timerenabled = True
	WheelROLeftIn.z = -35
	T_LeftinlanePos = 0
	T_LeftinlaneDir = 1
	T_LeftinlaneSpinS = T_LeftinlaneSpinS + 0.3
End Sub
Sub LeftInlane_Timer
	T_LeftinlanePos = T_LeftinlanePos + 1 
	If T_LeftinlaneDir = 0 Then
		Select Case T_LeftinlanePos
			Case 1
				WheelROLeftIn.z = -35
			Case 2
				WheelROLeftIn.z = -40
		End Select
	Else
		Select Case T_LeftinlanePos
			Case 1
				WheelROLeftIn.z = -25
			Case 2
				WheelROLeftIn.z = -15
		End Select
	End If
	T_LeftinlaneSpinS = T_LeftinlaneSpinS - 0.05
	If T_LeftinlaneSpinS < 0.1 Then LeftInlane.timerenabled = False
	
	If T_LeftinlaneSpinD = 0 Then
		T_LeftinlaneSpinP = ( T_LeftinlaneSpinP + T_LeftinlaneSpinS ) Mod 360
	else
		T_LeftinlaneSpinP = ( T_LeftinlaneSpinP - T_LeftinlaneSpinS ) Mod 360
	End If
	WheelROLeftIn.rotx = T_LeftinlaneSpinP
End Sub

Sub Leftoutlane_hit

	RandomSoundRollover
	RandomSoundOutlaneRollover
	Playsoundat "sfx_outlane",LeftInlane


	If Blink(98,1)=1 then
		Addscore 1000
		if Ballsave < - 1 Then addscore 2500 : If BIP = 1 then Addscore 2500
		BLS 98,98,12,5,5,0

'		If EB_Inlanes(CurrentPlayer) < 15 Then
'			EB_Inlanes(CurrentPlayer) = EB_Inlanes(CurrentPlayer) + 1
'			debug.print "EB_inlanes = " & EB_Inlanes(CurrentPlayer)
'			If EB_Inlanes(CurrentPlayer) = 15 and EB_Collected(CurrentPlayer) < MaxExtraBalls Then Lite_EB_insert
'		End If

	Else
		Addscore 100
		BLS 98,98,3,5,5,0
	End If
	Blink(96,1)=0 : Blink(97,1)=0 : Blink(98,1)=0 : Blink(99,1)=0
	Blink(106,1)=0 : Blink(107,1)=0 : Blink(108,1)=0 : Blink(109,1)=0


	T_leftoutlaneSpinS = Abs (( activeball.vely * 2 ) + 1 )
	If activeball.vely > 0 then T_leftoutlaneSpinD = 1 Else T_leftoutlaneSpinD = 0
	leftoutlane.timerenabled = True
	WheelROleftout.z = -25
	T_leftoutlanePos = 0
	T_leftoutlaneDir = 0
End Sub
Dim T_leftoutlaneDir
Dim T_leftoutlanePos
Dim T_leftoutlaneSpinS
Dim T_leftoutlaneSpinD
Dim T_leftoutlaneSpinP
Sub leftOutlane_unhit
	leftoutlane.timerenabled = False
	leftoutlane.timerenabled = True
	WheelROleftout.z = -35
	T_leftoutlanePos = 0
	T_leftoutlaneDir = 1
	T_leftoutlaneSpinS = T_leftoutlaneSpinS + 0.3
End Sub
Sub leftoutlane_Timer
	T_leftoutlanePos = T_leftoutlanePos + 1 
	If T_leftoutlaneDir = 0 Then
		Select Case T_leftoutlanePos
			Case 1
				WheelROleftout.z = -35
			Case 2
				WheelROleftout.z = -40
		End Select
	Else
		Select Case T_leftoutlanePos
			Case 1
				WheelROleftout.z = -25
			Case 2
				WheelROleftout.z = -15
		End Select
	End If
	T_leftoutlaneSpinS = T_leftoutlaneSpinS - 0.05
	If T_leftoutlaneSpinS < 0.1 Then leftoutlane.timerenabled = False
	
	If T_leftoutlaneSpinD = 0 Then
		T_leftoutlaneSpinP = ( T_leftoutlaneSpinP + T_leftoutlaneSpinS ) Mod 360
	else
		T_leftoutlaneSpinP = ( T_leftoutlaneSpinP - T_leftoutlaneSpinS ) Mod 360
	End If
	WheelROleftout.rotx = T_leftoutlaneSpinP
End Sub




'*******************************************
'  Scoring
'*******************************************


Sub Addscore ( value)
	If Tilted Then Exit Sub
	If DoubleScoring then value=value * 2
'	If TrippleScoring then value=value * 3

	EOB_Bonus = EOB_Bonus + value

'	debug.print "EOB_Bonus = " & EOB_Bonus

	If Playercontrol = "Tweed" Then
		PlayerScore(1)=PlayerScore(1) + value
	Else
		PlayerScore(2)=PlayerScore(2) + value
	End If
	If DesktopMode then
		If PlayerChoosen = "Tweed" or CurrentPlayer = 1 then
			bls 143,143,1,5,5,0
		Else
			bls 142,142,1,5,5,0
		End If
		BLS 144,146,1,5,5,0
	End If
	If b2son then
		If PlayerChoosen = "Tweed" or CurrentPlayer = 1 then
			bbs 9,-1,2,6
		Else
			bbs 10,-1,2,6
		End If
	End If

End Sub

Dim Howmuch
Sub addscoreovertime_Timer
	If Howmuch < 1 then Exit Sub
	dim x
	x = int ( howmuch / 18 )
	If x < 111 then x = 111

	If Howmuch < x then
'		debug.Print "howmuch " & howmuch & " all"
		Addscore Int (Howmuch)
		Howmuch = 0
		PlaySound "sfx_awardcontract",1,VolumeDial
	Else
		Addscore x
		Howmuch = Howmuch - x
		If Howmuch < 1 then
			Howmuch = 0
			PlaySound "sfx_awardcontract",1,VolumeDial
		End If
	End if
	playsound "fx_diverter",1,VolumeDial
End Sub


'*******************************************
'  Key Press Handling
'*******************************************
Dim PlayersPlaying
Dim Playerselect
Dim PlayerChoosen
Dim InstructionPage
InstructionPage = 1 
Sub Table1_KeyDown(ByVal keycode)
	dim x

	If keycode = leftmagnasave then

		If Lutenabled = 1 Then 
			LutValue = LutValue + 1 
			If LutValue > 13 Then LutValue = 1
			NewLut
		Else
			ScoreCard = 1 : CardTimer.enabled=True
		End If
	End If
	If keycode = rightmagnasave then
		If ChangeLut then Lutenabled = 1

		debug.print "Ball1xyz : " & ETBall1.x & " " &  ETBall1.y &  ETBall1.z
		debug.print "Ball2xyz : " & ETBall2.x & " " &  ETBall2.y &  ETBall2.z
		debug.print "Ball3xyz : " & ETBall3.x & " " &  ETBall3.y &  ETBall3.z
		debug.print "Ball4xyz : " & ETBall4.x & " " &  ETBall4.y &  ETBall4.z
		debug.print "Ball5xyz : " & ETBall5.x & " " &  ETBall5.y &  ETBall5.z

		InstructionPage = InstructionPage + 1
		If InstructionPage > 7 then InstructionPage = 1
		InstructionCard.image = "Instructioncard" & InstructionPage
		cardFlasher.imageA = "Instructioncard" & InstructionPage
	End If

if not tilted then

	If not BallOver then
		If keycode = LeftFlipperKey Then
			If PlayerSelect then
'				debug.print "playerselect " & Playerselect
				PlayerChoosen = "Tweed"
				stopsound "sfx_takeback" : PlaySound "sfx_takeback"
				If DesktopMode and VRRoom = 0 then
					Light011.state = 0 : Light012.state = 0
					Light003.state = 2 : Light004.state = 2	
					Blink(143,1) = 2 'DT_Tweed_Light
					Blink(142,1) = 0 'DT_Baron_Light
				End If
				If b2son then
					bbs 2,0,0,0
					bbs 1,0,99999,6
					bbs 9,0,99999,6
					bbs 10,0,0,0
				End If
			End If
			If gamemode > 1 Then
				DOF 101,1
				FlipperActivate LeftFlipper, LFPress : SolLFlipper True	
				If StagedFlippers = 0 then FlipperActivate LeftFlipper2, LFPress2 : SolLFlipper2 True	
				stopsound "sfx_Lflipper" : PlaySound "sfx_Lflipper", 1, 0.3 * VolumeDial, AudioPan(leftflipper), 0,0,0, 1, AudioFade(leftflipper)
'				x=Blink(98,1)
'				Blink(98,1)=Blink(96,1) : Blink(106,1)=Blink(107,1)
'				Blink(96,1)=Blink(97,1) : Blink(107,1)=Blink(108,1)
'				Blink(97,1)=Blink(99,1) : Blink(108,1)=Blink(109,1)
'				Blink(99,1)=x : Blink(109,1)=x
			End If
		End If
		If StagedFlippers = 1 Then
			If keycode = 30 Then
				If GameMode > 1 Then
					FlipperActivate LeftFlipper2, LFPress2 : SolLFlipper2 True
					PlaySound "sfx_Lflipper", 1, 0.12 * VolumeDial, AudioPan(Rightflipper2), 0,0,0, 1, AudioFade(Rightflipper2)
				End If
			End If

			If keycode = 40 Then
				If GameMode > 1 Then
					FlipperActivate RightFlipper2, RFPress2 : SolRFlipper2 True
					PlaySound "sfx_Rflipper", 1, 0.12 * VolumeDial, AudioPan(Rightflipper2), 0,0,0, 1, AudioFade(Rightflipper2)
				End If
			End If
		End If
		If keycode = RightFlipperKey Then
			If PlayerSelect then
				stopsound "sfx_takeback" : PlaySound "sfx_takeback"
				PlayerChoosen = "Baron"
				If DesktopMode and VRRoom = 0 then
					Light011.state = 2 : Light012.state = 2
					Light003.state = 0 : Light004.state = 0	
					Blink(143,1) = 0 'DT_Tweed_Light
					Blink(142,1) = 2 'DT_Baron_Light
				End If
				If b2son then
					bbs 2,0,99999,6
					bbs 1,0,0,0
					bbs 10,0,99999,6
					bbs 9,0,0,0
				End If
			End If
			If gamemode > 1 Then
				DOF 102,1
				FlipperActivate RightFlipper, RFPress : SolRFlipper True	
				If StagedFlippers = 0 then FlipperActivate RightFlipper2, RFPress2 : SolRFlipper2 True	
				stopsound "sfx_Rflipper" : PlaySound "sfx_Rflipper", 1, 0.25 * VolumeDial, AudioPan(Rightflipper), 0,0,0, 1, AudioFade(Rightflipper)
'				x=Blink(99,1)
'				Blink(99,1)=Blink(97,1) : Blink(109,1)=Blink(108,1)
'				Blink(97,1)=Blink(96,1) : Blink(108,1)=Blink(107,1)
'				Blink(96,1)=Blink(98,1) : Blink(107,1)=Blink(106,1)
'				Blink(98,1)=x : Blink(106,1)=x
			End If
		End If
	End If
End If
	If keycode = PlungerKey Then Plunger.Pullback : SoundPlungerPull()


	If keycode = LeftTiltKey   Then Nudge  90, 4 : SoundNudgeLeft()   : Tilting 
	If keycode = RightTiltKey  Then Nudge 270, 4 : SoundNudgeRight()  : Tilting
	If keycode = CenterTiltKey Then Nudge   0, 4 : SoundNudgeCenter() : Tilting
	If keycode = MechanicalTilt Then SoundNudgeCenter() : Tilting


	If keycode = StartGameKey Then 
		soundStartButton()
		If gamemode = 0 Then 
			If Credits > 0 then


				If Not Free_Play then 
					Credits = Credits - 1
					SaveValue TableName, "Credits", Credits
				End If

				StopAttractMode : DOF 113,0 : DOF 115,2

				If int(rnd(1)*2) = 1 then PlayVoice "vo_welcome",1,Voice_Volume,0 else PlayVoice "vo_welcome2",1,Voice_Volume,0
				playersplaying = 1
				scoreflag(8) = 4
				scorereeels(8) = 0
				scoreflag(16) = 4
				scorereeels(16) = 0
				eltower = 2
				BallInPlay = 1
				PlaySound "sfx_echohit",1,VolumeDial
				BallOver = False
'				debug.print "StartGame:   Ballover = " & BallOver

				If tournamentplay then 
					Playerselect = False
					PlayerChoosen = "Tweed"
					If DesktopMode then Blink(143,1) = 1 : Blink(142,1)=0
					If b2son then bbs 1,1,0,0 : bbs 2,0,0,0
					CurrentPlayer = 1
					Playercontrol = "Tweed"
					PlayVoice "Tweed_WeHaveABusinessToRun",1,Voice_Volume,1 
				End If
			Else
				PlaySound "buzz",1,VolumeDial
				eltower = 1
			End If
		Else
			If Credits > 0 And playersplaying = 1 And BallInPlay = 1 Then
				If int(rnd(1)*2) = 1 then PlayVoice "vo_futurevolkan",1,Voice_Volume,0 else PlayVoice "vo_premierproducer",1,Voice_Volume,0
				stopsound "vo_welcome"
				stopsound "vo_welcome2"

				If Not Free_Play then
					Credits = Credits - 1
					SaveValue TableName, "Credits", Credits
				End If

				playersplaying = 2
				PlayerChoosen = "None"
				Playerselect = False
				If DesktopMode then Blink(143,1) = 1 : Blink(142,1)=0
				If b2son then bbs 9,1,0,0 : bbs 10,0,0,0
				CurrentPlayer = 1
				Playercontrol = "Tweed"
				PlaySound "sfx_stealcontrol",1,VolumeDial
				If DesktopMode Then
					Light011.state = 0 : Light012.state = 0
					Light003.state = 2 : Light004.state = 2	
				End If
				scoreflag(16) = 4
				scorereeels(16) = 0
				UpdateControl
				PlaySound "sfx_echohit",1,VolumeDial
			Else
				PlaySound "buzz",1,VolumeDial
				eltower = 1
			End If
		End If
	End If
		

	'If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then 'Use this for ROM based games
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		If Not Free_Play then
			PlaySound "sfx_coinin",1, Voice_Volume
			Select Case Int(rnd*3)
				Case 0: PlaySound "Coin_In_1",1, VolumeDial
				Case 1: PlaySound "Coin_In_2",1, VolumeDial
				Case 2: PlaySound "Coin_In_3",1, VolumeDial
			End Select
			Credits = Credits + 1 
			IF gamemode = 0 then DOF 113,1
			If Credits > 9 then Credits = 9 Else DOF 122,2
		End If
	End If

	If VRRoom > 0 Then
		If keycode = LeftFlipperKey Then
			VR_FB_Left.x = VR_FB_Left.x + 5
		End If
		If keycode = RightFlipperKey Then
			VR_FB_Right.x = VR_FB_Right.x - 5
		End If
		If keycode = PlungerKey Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
	End If

End Sub



Dim LutEnabled
Sub Table1_KeyUp(ByVal keycode)

	if keycode = leftmagnasave then ScoreCard=0
	if keycode = Rightmagnasave Then Lutenabled = 0

	If KeyCode = PlungerKey Then

		If PlayerSelect then
			PlaySound "sfx_stealcontrol",1,VolumeDial

			Select Case PlayerChoosen
				Case "Tweed"
					If DesktopMode then Blink(143,1) = 1 : Blink(142,1)=0
					If b2son then bbs 1,1,0,0 : bbs 2,0,0,0
					CurrentPlayer = 1
					Playercontrol = "Tweed"
					PlayVoice "Tweed_WeHaveABusinessToRun",1,Voice_Volume,1 
				Case "Baron"
					If DesktopMode then Blink(143,1) = 0 : Blink(142,1)=1
					If b2son then bbs 1,0,0,0 : bbs 2,1,0,0
					CurrentPlayer = 2
					Playercontrol = "Baron"
					PlayVoice "baron_ImResponsibleForVolkan",1,Voice_Volume,1 
			End Select
			UpdateControl
		End If
		Playerselect = False


		Plunger.Fire

		If bipl = 1 then						 	'If true then ball in shooter lane, else no ball is shooter lane
			SoundPlungerPullStop()
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerPullStop()
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
	End If

	if not tilted Then
		If keycode = LeftFlipperKey Then
			If gamemode > 1 Then
				DOF 101,0
				FlipperDeActivate LeftFlipper, LFPress : SolLFlipper False	
				FlipperDeActivate LeftFlipper2, LFPress2 : SolLFlipper2 False
			End If
		End If
		If keycode = 30 Then
			If GameMode > 1 Then
				If StagedFlippers = 1 Then FlipperDeActivate LeftFlipper2, LFPress2 : SolLFlipper2 False
			End If
		End If
		If keycode = RightFlipperKey Then
			If gamemode > 1 Then
				DOF 102,0
				FlipperDeActivate RightFlipper, RFPress : SolRFlipper False	
				FlipperDeActivate RightFlipper2, RFPress2 : SolRFlipper2 False	
			End If
		End If
		If keycode = 40 Then
			If GameMode > 1 Then
				If StagedFlippers = 1 Then FlipperDeActivate RightFlipper2, RFPress2 : SolRFlipper2 False
			End If
		End If
	End If

	If VRRoom > 0 Then
		If keycode = LeftFlipperKey Then
			VR_FB_Left.x = VR_FB_Left.x - 5
		End If
		If keycode = RightFlipperKey Then
			VR_FB_Right.x = VR_FB_Right.x + 5
		End If
		If keycode = PlungerKey Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
		End If		
	End If
End Sub

'*******************************************
'  Flippers
'*******************************************

Const ReflipAngle = 20
Const QuickFlipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend

		If LeftFlipper.currentangle < LeftFlipper.endangle + ReflipAngle Then 
			'Play partial flip sound and stop any flip down sound
			'Debug.print "Flip Reflip"
			StopAnyFlipperLowerLeftDown()
'			RandomSoundLowerLeftReflip()
			RandomSoundFlipperLowerLeftReflip LeftFlipper
		Else
			'Debug.print "LeftFlipper.currentangle = " &LeftFlipper.currentangle
			'Debug.print "LeftFlipper.startangle = " &LeftFlipper.startangle
			'Debug.print "LeftFlipper.endangle = " &LeftFlipper.endangle
			'LeftFlipper.RotateToEnd
			'Play full flip sound
			'Debug.print "Flip Up"
			If BallNearLF = 0 Then 
				RandomSoundFlipperLowerLeftUpFullStroke LeftFlipper
			End If
			If BallNearLF = 1 Then 
				Select Case Int(Rnd*2)+1
					Case 1 : RandomSoundFlipperLowerLeftUpDampenedStroke LeftFlipper
					Case 2 : RandomSoundFlipperLowerLeftUpFullStroke LeftFlipper
				End Select		
			End If
		End If	
	Else
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			'Play flip down sound
			'Debug.print "Flip Down"
			RandomSoundFlipperLowerLeftDown LeftFlipper
		End If
		If LeftFlipper.currentangle < LeftFlipper.startAngle + QuickFlipAngle and LeftFlipper.currentangle <> LeftFlipper.endangle Then
			'Play quick flip sound and stop any flip up sound
'			Debug.print "Flip Quick"
			StopAnyFlipperLowerLeftUp()
			RandomSoundLowerLeftQuickFlipUp()
		Else
			FlipperLeftLowerHitParm = FlipperUpSoundLevel
		End If
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend
		If RightFlipper.currentangle > RightFlipper.endangle - ReflipAngle Then
			'Play partial flip sound and stop any flip down sound
			StopAnyFlipperLowerRightDown()
			RandomSoundFlipperLowerRightReflip RightFlipper
		Else 
			'Play full flip sound
			'RightFlipper.RotateToEnd
			If BallNearRF = 0 Then 
				RandomSoundFlipperLowerRightUpFullStroke RightFlipper
			End If
			
			If BallNearRF = 1 Then 
				Select Case Int(Rnd*2)+1
					Case 1 : RandomSoundFlipperLowerRightUpDampenedStroke RightFlipper
					Case 2 : RandomSoundFlipperLowerRightUpFullStroke RightFlipper
				End Select		
			End If
		End If
	Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			'Play flip down sound
			RandomSoundFlipperLowerRightDown RightFlipper
		End If
		If RightFlipper.currentangle < RightFlipper.startAngle + QuickFlipAngle and RightFlipper.currentangle <> RightFlipper.endangle Then
			'Play quick flip sound and stop any flip up sound
			StopAnyFlipperLowerRightUp()
			RandomSoundLowerRightQuickFlipUp()
		Else
			FlipperRightLowerHitParm = FlipperUpSoundLevel
		End If
	End If
End Sub

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper2(Enabled)
	If Enabled Then
		LF2.Fire  'leftflipper.rotatetoend

'		If leftflipper2.currentangle < leftflipper2.endangle + ReflipAngle Then 
'			RandomSoundReflipUpLeft LeftFlipper2
'		Else 
'			SoundFlipperUpAttackLeft LeftFlipper2
'			RandomSoundFlipperUpLeft LeftFlipper2
'		End If		
	Else
		LeftFlipper2.RotateToStart
'		If LeftFlipper2.currentangle < LeftFlipper2.startAngle - 5 Then
'			RandomSoundFlipperDownLeft LeftFlipper2
'		End If
'		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper2(Enabled)
	If Enabled Then
		RF2.Fire 'rightflipper2.rotatetoend

'		If rightflipper2.currentangle > rightflipper2.endangle - ReflipAngle Then
'			RandomSoundReflipUpRight RightFlipper2
'		Else 
'			SoundFlipperUpAttackRight RightFlipper2
'			RandomSoundFlipperUpRight RightFlipper2
'		End If
	Else
		RightFlipper2.RotateToStart
'		If RightFlipper2.currentangle > RightFlipper2.startAngle + 5 Then
'			RandomSoundFlipperDownRight RightFlipper2
'		End If	
'		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub



'****************************************************************
'  Slingshots
'****************************************************************

' RStep and LStep are the variables that increment the animation
Dim RStep, LStep

Sub RightSling_Slingshot
	DOF 104,2
	RS.VelocityCorrect(activeball)
	RandomSoundSlingshotRight Sling1
	bls 55,55,2,4,3,0
	bls 6,10,2,5,6,0
	bls 57,134,1,0,6,0
	PlaySoundat "sfx_slingR",Sling1
	Addscore 10
	RSling1.Visible = 1
	Sling1.TransY = -20			'Sling Metal Bracket
	RStep = 0
	RightSling.TimerEnabled = 1
	RightSling.TimerInterval = 16
End Sub

Sub RightSling_Timer
	Select Case RStep
		Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:Sling1.TransY = -10
		Case 4:RSLing2.Visible = 0:Sling1.TransY = 0:RightSling.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSling_Slingshot
	DOF 103,2
	LS.VelocityCorrect(activeball)
	RandomSoundSlingshotLeft Sling2
	bls 56,56,2,4,3,0
	bls 1,5,2,5,6,0
	bls 57,134,1,0,6,0
	PlaySoundat "sfx_slingL",Sling2
	Addscore 10
	LSling1.Visible = 1
	Sling2.TransY = -20			'Sling Metal Bracket
	LStep = 0
	LeftSling.TimerEnabled = 1
	LeftSling.TimerInterval = 16
End Sub

Sub LeftSling_Timer
	Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:Sling2.TransY = -10
		Case 4:LSLing2.Visible = 0:Sling2.TransY = 0:LeftSling.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub


'**********************************************************************************************************
'* InstructionCard *
'**********************************************************************************************************

Dim CardCounter, ScoreCard
Sub CardTimer_Timer
        If scorecard=1 Then
                CardCounter=CardCounter+2
                If CardCounter>50 Then CardCounter=50
        Else
                CardCounter=CardCounter-4
                If CardCounter<0 Then CardCounter=0
        End If
        InstructionCard.transX = CardCounter*5
        InstructionCard.transY = CardCounter*6

		InstructionCard.transZ = -cardcounter
'		InstructionCard.objRotX = -cardcounter/5
		InstructionCard.objRotZ = cardcounter/7
'	    InstructionCard.objRoty = -cardcounter/2
        InstructionCard.size_x = 1+CardCounter/25
        InstructionCard.size_y = 1+CardCounter/20

        If CardCounter=0 Then 
                CardTimer.Enabled=False
                InstructionCard.visible=0
        Else
                InstructionCard.visible=1
        End If 
End Sub



'******************************************************
'****  ZLMP : LAMPZ by nFozzy
'******************************************************
' 
' Need to add usage instructions here.
'

'  oqqsan: individual light sequencer commands

' examples
'			    lightnr ,state  ,color   ,blinkinterval ,blinksequence  , BLSBlinks+timeon+timeoff,delay
'		ChangeLamp 17	,1		,"green"	, 90		,"1010100"		,7,8,8,1		 '( 7 blinks slow then		state = On
'		ChangeLamp 17	,0		,"red"		, 90		,"1010100"		,7,2,2,1		 '( 7 blinks fast then		state = Off
'		ChangeLamp  7	,2		,"green"	, 75		,"101000"		,2,2,2,10		 '( 2 blinks fast w/delay then state = Blinking  light should go off when delay is active
' 		ChangeLamp  7	,2		,"green"	, 150		,"10"			,0,0,0,0		 '( no lightsequence  state = Blinking slower just normal on/off
'
'	If Lampstate(17) = 1 Then .... same as if Blink(16,1)=1 or the old vpx   if Light016.state=1
'

Sub ChangeLamp(nr,state,interval,sequence ,blsBlinks , blsOnT, blsOffT, blsDelay )
'	SetLightColor nr,col
	SetState nr,state,interval,sequence ' State=-1 = skip setting new value, will do the interval/sequence only
	If blsBlinks > 0 Then
		Blink(nr,2)=blsBlinks
		Blink(nr,3)=0 ' flag
		Blink(nr,4)=blsOnT
		Blink(nr,6)=blsOffT
		Blink(nr,8)=blsDelay
	End If
End Sub

Sub ChangeLampState(nr,state)
	'WriteToLog "ChangeLampState", "nr = " & nr & " state = " & state
	Blink(nr,1) = state
End Sub

Function Lampstate(nr)
	Lampstate = Blink(nr,1)
End Function


'
' copied from 1 switch how it works
'
'Sub swTopLane1_Hit
'	SwitchWasHit "swTopLane1"
'	Blink(1,1) = 1  ' normal set state use this format   Lampz nr 2 saved state = 1
'					' optional: cLight001.blinkinterval=timeon ( need the right name of the clight )
'					' optional: cLight001.blinkpattern = pattern  ( need the right name of the clight )
'	BLS 1,1,8,5,2,1 ' light 1 to light 1 ( can be many and go down aswell ) , 8 blinks , 5 timeon, 2 timeoff, 1 delay for each light before blinking start ) 
'																						all time values is in updates ( currently 16ms timer )
'End Sub 
'
'     old                      new
' light001.state=1 		->> Blink(1,1)=1
' If light001.state=2 	->> If Blink(1,1)=2 
'		the BLS command overrides the state and blinks away until finished and restore the saved value.. 
'  		all this can be overridden by a normal VPX lightsequencer at anytime
' 		no worries at all set and forget, lampz control everything
'		VPX lightsequencer > lightsequencer2 > normal state

' SetState 40,2,100,"01010110010110"   ' Blink(40,1)=2  sets the same state but not interval and pattern
' **** Insert nr ,State(0;1;2), interval = how long each part of patterns is displayd in ms, pattern to blink "01" off/on any length of 0's and 1's

' individual RGB inserts
'SetLightColor  40,"green"   
' **** insert nr , color ( green yellow red purple blue orange white darklblue ) can add any color to this below


Dim Blink(160,11)
Sub BLS(For_nr,Next_nr,blinks,timeon,timeoff,delay)
	Dim i,Bdir,AddedDelay
	AddedDelay = delay
	If For_nr > Next_nr Then Bdir = -1 Else Bdir = 1
	For i = For_nr to Next_nr step Bdir
		Blink(i,2)=blinks
		Blink(i,3)=0 ' flag
		Blink(i,4)=timeon
		Blink(i,6)=timeoff
		Blink(i,8)=AddedDelay
'		If delay>0 Then all_c_lights(i).state = 0 ' turn off at start if there is delay
		AddedDelay = AddedDelay + delay
	Next
End Sub

Sub SetState ( Lightnr,state,interval,pattern) 
	If Not State = -1 Then Blink(Lightnr,1)=State
'	If Lampz.IsLight(Lightnr) then ' double check so it wont fail
		if IsArray(Lampz.obj(Lightnr)) Then
			dim tmp : tmp = Lampz.obj(Lightnr)
			tmp(0).blinkinterval =  interval
			tmp(0).blinkpattern =  pattern
		Else
'			Debug.print "wrong numbers in setstate " & lightnr
' FIX can remove the last double check
			Lampz.obj(Lightnr).blinkinterval= interval
			Lampz.obj(Lightnr).blinkpattern =  pattern
		End If
'	End If
End Sub


Dim GaugeLeft : GaugeLeft=0
Dim GaugeLeftPos : GaugeLeftPos=42
Dim GaugeRight: GaugeRight=0
Dim GaugeRightPos: GaugeRightPos=47

Sub AnimApronGauge

	If GaugeLeft > GaugeLeftPos then GaugeLeft = GaugeLeft - 0.25
	If GaugeLeft < GaugeLeftPos then GaugeLeft = GaugeLeft + 0.25
	If GaugeRight > GaugeRightPos then GaugeRight = GaugeRight - 0.25
	If GaugeRight < GaugeRightPos then GaugeRight = GaugeRight + 0.25

	NeedleLeft.rotz  = Gaugeleft + LSpike		'  42-144 = 102
	NeedleRight.rotz = GaugeRight + RSpike 		'  47-131 = 84


End Sub


Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              ' Setup lamp assignments
LampTimer.Interval = 16
LampTimer.Enabled = 1

Function RndInt(min, max)
	RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Dim A_Wheels(2)

Sub AnimApronWheels
	If A_Wheels(0) > 0 then 'speed Up steps
		A_Wheels(0) = A_Wheels(0) - 1
		A_Wheels(1) = A_Wheels (1) + 0.5 ' speed
		If A_Wheels(1) > 4 then A_Wheels(1) = 4
	Elseif A_Wheels(1) > 0 then
		A_Wheels(1) = A_Wheels (1) - 0.08 ' speed
		If A_Wheels(1) < 0 then A_Wheels(1) = 0
	End If
	A_Wheels(2) = A_Wheels(2) + A_Wheels(1)
	If A_Wheels(2) > 360 then A_Wheels(2) = A_Wheels(2) - 360
	wheel1.objrotz = A_Wheels(2)
	wheel2.objrotz = -A_Wheels(2)
	wheel3.objrotz = A_Wheels(2)
	wheel4.objrotz = -A_Wheels(2)' + 1
	wheel5.objrotz = A_Wheels(2) '+ 1
End Sub

Dim SpikeOn, ElTower
SpikeOn = 0
ElTower = 0
Sub LampTimer_Timer()

	If Tilt > 0 Then
		Tilt = Tilt - 1
		If Tilt < 7 Then
			Tilt = 0
'			Debug.print "Tiltrecovery ="&Tilt
			If vrroom = 0 and DesktopMode Then Light007.state = 0 : Light008.state = 0
			If b2son then bbs 4,0,0,0
		End If
	End If

	GIstuff
	AnimDT
	AnimApronGauge
	AnimCoalHopper
	AnimRamps
	If VRRoom > 0 or b2son then Blink_B2s
	AnimApronWheels

	If SpikeOn > 0 Then
		SpikeOn = SpikeOn + 1
		Select Case SpikeOn
			Case 2 : ElectricTowerL004.blenddisablelighting = 10	: ElectricTowerL008.blenddisablelighting = 10
					 accumulator.visible = False
			Case 3 : ElectricTowerL004.blenddisablelighting = 5		: ElectricTowerL008.blenddisablelighting = 5
						PlaySoundAt "sfx_electricspin",ElectricSpinner
					 ElectricTowerL003.blenddisablelighting = 10	: ElectricTowerL007.blenddisablelighting = 10
			Case 4 : ElectricTowerL004.blenddisablelighting = 2		: ElectricTowerL008.blenddisablelighting = 2
					 ElectricTowerL003.blenddisablelighting = 5 	: ElectricTowerL007.blenddisablelighting = 5
					 ElectricTowerL002.blenddisablelighting = 10	: ElectricTowerL006.blenddisablelighting = 10
			Case 5 : ElectricTowerL004.blenddisablelighting = 0		: ElectricTowerL008.blenddisablelighting = 0.8
					 ElectricTowerL003.blenddisablelighting = 2		: ElectricTowerL007.blenddisablelighting = 2
					 ElectricTowerL002.blenddisablelighting = 5		: ElectricTowerL006.blenddisablelighting = 5
			Case 6 : ElectricTowerL003.blenddisablelighting = 0		: ElectricTowerL007.blenddisablelighting = 0.8
					 ElectricTowerL002.blenddisablelighting = 2		: ElectricTowerL006.blenddisablelighting = 2
			Case 7 : ElectricTowerL002.blenddisablelighting = 0		: ElectricTowerL006.blenddisablelighting = 0.8
					 accumulator.visible = True
			Case 33 : SpikeOn = 0  : accumulator.visible = False
			Case Else
					Accumulator.rotx = RndInt(360,1)
					Accumulator.blenddisablelighting = RndInt(100,0)
					If int(rnd(1)*10)=3 Then
						Accumulator.z = 180-RndInt(100,0)
						ElectricTowerL001.blenddisablelighting = 6
						ElectricTowerL009.blenddisablelighting = 6
					Else
						Accumulator.z = 180 
						ElectricTowerL001.blenddisablelighting = 0.8
						ElectricTowerL009.blenddisablelighting = 0.8
					End If
		End Select
	Else
		If ElTower > 0 Then
			ElTower = ElTower - 1
			SpikeOn = 1
		End If
	End If



	dim idx : for idx = 0 to uBound(Lampz.Obj)
		if Lampz.IsLight(idx) then 
			if IsArray(Lampz.obj(idx)) then
				dim tmp : tmp = Lampz.obj(idx)

'* added to lampz
'				If Blink(idx,9) Then
'					tmp(0).blinkinterval =  Blink(idx,10)
'					tmp(0).blinkpattern =  Blink(idx,11)
'					Blink(idx,9)=False
'				End If
'				If Blink(idx,12) Then
'					tmp(1).ColorFull =  Blink(idx,13)
'					Blink(idx,12) = False
'				End If
				If Blink(idx,2)>0 Then	' Lightsequencer ? + multipleblinks
					If Blink(idx,8)>0 Then ' is there a delay ! ?=
						tmp(0).state = 0
						Blink(idx,8) = Blink(idx,8) - 1
					Else
						Select Case Blink(idx,3)
							Case 0 : tmp(0).state = 1 :  Blink(idx,5)=Blink(idx,4) : Blink(idx,3) = 1 
							Case 1 : Blink(idx,5)=Blink(idx,5)-1 : If Blink(idx,5) < 1 Then Blink(idx,3) = 2
							Case 2 : tmp(0).state = 0   :  Blink(idx,7)=Blink(idx,6) : Blink(idx,3) = 3
							Case 3 : Blink(idx,7)=Blink(idx,7)-1 
									 If Blink(idx,7) < 1 Then
										Blink(idx,2)=Blink(idx,2)-1
										If Blink(idx,2)<1 Then Blink(idx,2)=0
										Blink(idx,3) = 0
									 End If
						End Select
					End If
				Else
					tmp(0).state = Blink(idx,1)
				End If
'* added to lampz until here

				Lampz.state(idx) = tmp(0).GetInPlayStateBool
				'debug.print tmp(0).name & " " &  tmp(0).GetInPlayStateBool & " " & tmp(0).IntensityScale  & vbnewline
			Else
'* added to lampz
				If Blink(idx,2)>0 Then	' Lightsequencer ? + multipleblinks
					If Blink(idx,8)>0 Then ' is there a delay ! ?=
						Lampz.obj(idx).state = 0
						Blink(idx,8) = Blink(idx,8) - 1
					Else
						Select Case Blink(idx,3)
							Case 0 : Lampz.obj(idx).state = 1 :  Blink(idx,5)=Blink(idx,4) : Blink(idx,3) = 1 
							Case 1 : Blink(idx,5)=Blink(idx,5)-1 : If Blink(idx,5) < 1 Then Blink(idx,3) = 2
							Case 2 : Lampz.obj(idx).state = 0   :  Blink(idx,7)=Blink(idx,6) : Blink(idx,3) = 3
							Case 3 : Blink(idx,7)=Blink(idx,7)-1 
									 If Blink(idx,7) < 1 Then
										Blink(idx,2)=Blink(idx,2)-1
										If Blink(idx,2)<1 Then Blink(idx,2)=0
										Blink(idx,3) = 0
									 End If
						End Select
					End If
				Else
					Lampz.obj(idx).state = Blink(idx,1)
				End If
'* added to lampz until here

				Lampz.state(idx) = Lampz.obj(idx).GetInPlayStateBool
				'debug.print Lampz.obj(idx).name & " " &  Lampz.obj(idx).GetInPlayStateBool & " " & Lampz.obj(idx).IntensityScale  & vbnewline
			end if
		end if
	Next
	Lampz.Update1	'update (fading logic only)
End Sub



LampTimer2.Interval = -1
LampTimer2.Enabled = True
Sub LampTimer2_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
End Sub


Sub UpdateLightmap(lightmap, intensity, ByVal aLvl)  ' additiveblend prims
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)    'Callbacks don't get this filter automatically
    lightmap.Opacity = aLvl * intensity
End Sub


Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically

	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub DisableLightingA(pri,DLintensity,ByVal aLvl)
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	FL1=aLvl
'	pri.blenddisablelighting = aLvl * DLintensity + 1+ GI111.getinplayintensity / 7
	If alvl > 0.3 Then FlasherStokeNEW.image = "FlasherOrangeBase_On"  Else FlasherStokeNEW.image = "FlasherOrangeBase_Off"
End Sub
Sub DisableLightingB(pri,DLintensity,ByVal aLvl)
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	FL2=aLvl
'	pri.blenddisablelighting = aLvl * DLintensity + 1+ GI111.getinplayintensity / 7
	If alvl > 0.3 Then FlasherBlast.image = "FlasherRedBase_On" Else FlasherBlast.image = "FlasherRedBase_Off"

	If DoorF.currentangle = 90 then
		blastlight.intensity = 0
	Else
		blastlight.intensity = 2 * aLvl
	End If

End Sub
Dim FL1,FL2,FL3,FL4

Sub DisableLightingC(pri,DLintensity,ByVal aLvl)
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	FL3=aLvl
'	pri.blenddisablelighting = aLvl * DLintensity + 1+ GI111.getinplayintensity / 7
	If alvl > 0.3 Then FlasherElectric.image = "flasheryellowbase_lit2" Else FlasherElectric.image = "FlasherYellowBase_Off2"
End Sub
Sub DisableLightingD(pri,DLintensity,ByVal aLvl)
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	FL4=aLvl
'	pri.blenddisablelighting = aLvl * DLintensity + 1 + GI111.getinplayintensity / 7
	If alvl > 0.3 Then FlasherSteam001.image = "flasherbluebase_on" Else FlasherSteam001.image = "flasherbluebase_off"
End Sub


Sub Flasherflash(pri,DLintensity,ByVal aLvl)
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.opacity = aLvl * DLintensity
	if alvl < 0.2 then pri.rotz = Rnd(1)*360
End Sub
Sub Flasherblue(pri,DLintensity,ByVal aLvl)
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.opacity = aLvl * DLintensity
End Sub



'UpdateMaterial(string,wrapLighting,roughness,glossyImage, thickness, edge,edgeAlpha,opacity,COLOR base, COLOR glossy,    COLOR clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle)
'UpdateMaterial "Ramps"  ,0.1       ,0.8      ,0.7        ,1           1    ,0       0.999999,RampColor ,rgb(250,200,180),rgb(250,200,180),False,    True         ,0,0,0,0

Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 150 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/10 : next

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(1) = L001
	Lampz.MassAssign(1) = LB001
	Lampz.Callback(1) = "DisableLighting pon001, 400,"
	Lampz.Callback(1) = "DisableLighting poff001, 11,"
	Lampz.MassAssign(2) = L002
	Lampz.MassAssign(2) = LB002
	Lampz.Callback(2) = "DisableLighting pon002, 400,"
	Lampz.Callback(2) = "DisableLighting poff002, 11,"
	Lampz.MassAssign(3) = L003							'
	Lampz.MassAssign(3) = LB003
	Lampz.Callback(3) = "DisableLighting pon003, 400,"
	Lampz.Callback(3) = "DisableLighting poff003, 11,"
	Lampz.MassAssign(4) = L004								'
	Lampz.MassAssign(4) = LB004
	Lampz.Callback(4) = "DisableLighting pon004, 400,"
	Lampz.Callback(4) = "DisableLighting poff004, 11,"
	Lampz.MassAssign(5) = L005								'
	Lampz.MassAssign(5) = LB005
	Lampz.Callback(5) = "DisableLighting pon005, 400,"
	Lampz.Callback(5) = "DisableLighting poff005, 11,"
	Lampz.MassAssign(6) = L006								'
	Lampz.MassAssign(6) = LB006
	Lampz.Callback(6) = "DisableLighting pon006, 400,"
	Lampz.Callback(6) = "DisableLighting poff006, 11,"
	Lampz.MassAssign(7) = L007								'
	Lampz.MassAssign(7) = LB007
	Lampz.Callback(7) = "DisableLighting pon007, 400,"
	Lampz.Callback(7) = "DisableLighting poff007, 11,"
	Lampz.MassAssign(8) = L008								'
	Lampz.MassAssign(8) = LB008
	Lampz.Callback(8) = "DisableLighting pon008, 400,"
	Lampz.Callback(8) = "DisableLighting poff008, 11,"
	Lampz.MassAssign(9) = L009								'
	Lampz.MassAssign(9) = LB009
	Lampz.Callback(9) = "DisableLighting pon009, 400,"
	Lampz.Callback(9) = "DisableLighting poff009, 11,"
	Lampz.MassAssign(10) = L010								'
	Lampz.MassAssign(10) = LB010
	Lampz.Callback(10) = "DisableLighting pon010, 400,"
	Lampz.Callback(10) = "DisableLighting poff010, 11,"

	Lampz.MassAssign(11) = L011
	Lampz.MassAssign(11) = Light024
	Lampz.Callback(11) = "DisableLighting pon011, 150,"
	Lampz.MassAssign(12) = L012								'
	Lampz.Callback(12) = "DisableLighting pon012, 250,"
	Lampz.MassAssign(13) = L013							'
	Lampz.Callback(13) = "DisableLighting pon013, 250,"
	Lampz.MassAssign(14) = L014								'
	Lampz.Callback(14) = "DisableLighting pon014, 250,"

	Lampz.MassAssign(15) = L015								'
	Lampz.Callback(15) = "DisableLighting pon015, 250,"
	Lampz.Callback(15) = "DisableLighting poff015, 100,"
	Lampz.MassAssign(16) = L016								'
	Lampz.Callback(16) = "DisableLighting pon016, 250,"
	Lampz.Callback(16) = "DisableLighting poff016, 100,"
	Lampz.MassAssign(17) = L017								'
	Lampz.Callback(17) = "DisableLighting pon017, 250,"
	Lampz.Callback(17) = "DisableLighting poff017, 100,"

	Lampz.MassAssign(18) = L018								'
	Lampz.Callback(18) = "DisableLighting pon018, 330,"
	Lampz.MassAssign(19) = L019							'
	Lampz.Callback(19) = "DisableLighting pon019, 330,"

	Lampz.MassAssign(20) = L020								'
	Lampz.Callback(20) = "DisableLighting pon020, 200,"
	Lampz.Callback(20) = "DisableLighting poff020, 100,"
	Lampz.MassAssign(21) = L021								'
	Lampz.Callback(21) = "DisableLighting pon021, 250,"
	Lampz.Callback(21) = "DisableLighting poff021, 100,"
	Lampz.MassAssign(22) = L022								'
	Lampz.Callback(22) = "DisableLighting pon022, 250,"
	Lampz.MassAssign(23) = L023							'
	Lampz.Callback(23) = "DisableLighting pon023, 250,"
	Lampz.MassAssign(24) = L024							'
	Lampz.Callback(24) = "DisableLighting pon024, 220,"


	Lampz.MassAssign(25) = b2l2							' bumper001
	Lampz.MassAssign(25) = b2l1							'
	Lampz.Callback(25) = "DisableLighting BumperRingIron, 0.3,"
	Lampz.Callback(25) = "DisableLighting BumperCapIron, 3,"
	Lampz.Callback(25) = "DisableLighting BumperBaseIron, 0.2,"
	Lampz.MassAssign(26) = b3l2							' bumper002
	Lampz.MassAssign(26) = b3l1							'
	Lampz.Callback(26) = "DisableLighting BumperRingCopper, 0.3,"
	Lampz.Callback(26) = "DisableLighting BumperCapCopper, 3,"
	Lampz.Callback(26) = "DisableLighting BumperBaseCopper, 0.2,"
	Lampz.MassAssign(27) = b4l2							' bumper003
	Lampz.MassAssign(27) = b4l1							'
	Lampz.Callback(27) = "DisableLighting BumperRingZinc, 0.3,"
	Lampz.Callback(27) = "DisableLighting BumperCapZinc, 3,"
	Lampz.Callback(27) = "DisableLighting BumperBaseZinc, 0.2,"
	Lampz.MassAssign(28) = b5l2							' bumper004
	Lampz.MassAssign(28) = b5l1							'
	Lampz.Callback(28) = "DisableLighting BumperRingTin, 0.3,"
	Lampz.Callback(28) = "DisableLighting BumperCapTin, 3,"
	Lampz.Callback(28) = "DisableLighting BumperBaseTin, 0.2,"
		
	Lampz.MassAssign(29) = L029							'
	Lampz.Callback(29) = "DisableLighting pon029, 400,"


	Lampz.MassAssign(30) = L030	
	Lampz.MassAssign(30) = Light025	
	Lampz.Callback(30) = "DisableLighting pon030, 333,"
	Lampz.MassAssign(31) = L031						'
	Lampz.Callback(31) = "DisableLighting pon031, 250,"
	Lampz.MassAssign(32) = L032							'
	Lampz.Callback(32) = "DisableLighting pon032, 250,"
	Lampz.MassAssign(33) = L033							'
	Lampz.Callback(33) = "DisableLighting pon033, 250,"
	Lampz.MassAssign(34) = L034						'
	Lampz.Callback(34) = "DisableLighting pon034, 250,"
	Lampz.MassAssign(35) = L035							'
	Lampz.Callback(35) = "DisableLighting pon035, 250,"
	Lampz.MassAssign(36) = L036							'
	Lampz.Callback(36) = "DisableLighting pon036, 250,"
	Lampz.MassAssign(37) = L037							'
	Lampz.Callback(37) = "DisableLighting pon037, 250,"
	Lampz.MassAssign(38) = L038							'
	Lampz.Callback(38) = "DisableLighting pon038, 250,"
	Lampz.MassAssign(39) = L039							'
	Lampz.Callback(39) = "DisableLighting pon039, 250,"

	Lampz.MassAssign(40) = L040	
	Lampz.MassAssign(40) = Light023
	Lampz.Callback(40) = "DisableLighting pon040, 333,"
	Lampz.MassAssign(41) = L041							'
	Lampz.Callback(41) = "DisableLighting pon041, 250,"
	Lampz.MassAssign(42) = L042							'
	Lampz.Callback(42) = "DisableLighting pon042, 250,"
	Lampz.MassAssign(43) = L043							'
	Lampz.Callback(43) = "DisableLighting pon043, 250,"
	Lampz.MassAssign(44) = L044							'
	Lampz.Callback(44) = "DisableLighting pon044, 250,"
	Lampz.MassAssign(45) = L045						'
	Lampz.Callback(45) = "DisableLighting pon045, 250,"
	Lampz.MassAssign(46) = L046							'
	Lampz.Callback(46) = "DisableLighting pon046, 250,"
	Lampz.MassAssign(47) = L047							'
	Lampz.Callback(47) = "DisableLighting pon047, 250,"
	Lampz.MassAssign(48) = L048							'
	Lampz.Callback(48) = "DisableLighting pon048, 250,"
	Lampz.MassAssign(49) = L049						'
	Lampz.Callback(49) = "DisableLighting pon049, 250,"

	Lampz.MassAssign(50) = L050							'
	Lampz.Callback(50) = "DisableLighting pon050, 250,"
	Lampz.Callback(50) = "DisableLighting poff050, 100,"

	Lampz.MassAssign(51) = L051							'
	Lampz.Callback(51) = "DisableLighting pon051, 250,"
	Lampz.MassAssign(52) = L052							'
	Lampz.Callback(52) = "DisableLighting pon052, 250,"
	Lampz.MassAssign(53) = L053							'
	Lampz.Callback(53) = "DisableLighting pon053, 250,"
	Lampz.MassAssign(54) = L054							'
	Lampz.Callback(54) = "DisableLighting pon054, 250,"

	Lampz.MassAssign(55) = slinglightR_bulb						'
	Lampz.Callback(55) = "DisableLighting slinglightR, 2888,"
	Lampz.MassAssign(56) = slinglightL_bulb						'
	Lampz.Callback(56) = "DisableLighting slinglightL, 2888,"

	Lampz.MassAssign(57) = Light037						' SPINNER TUNNEL EFFECT
	Lampz.MassAssign(57) = Light038


	Lampz.MassAssign(61) = L061							'
	Lampz.Callback(61) = "DisableLighting pon061, 250,"
	Lampz.MassAssign(62) = L062							'
	Lampz.Callback(62) = "DisableLighting pon062, 250,"
	Lampz.MassAssign(63) = L063							'
	Lampz.Callback(63) = "DisableLighting pon063, 250,"
	Lampz.MassAssign(64) = L064							'
	Lampz.Callback(64) = "DisableLighting pon064, 250,"

	Lampz.MassAssign(71) = L071							'
	Lampz.Callback(71) = "DisableLighting pon071, 250,"
	Lampz.MassAssign(72) = L072							'
	Lampz.Callback(72) = "DisableLighting pon072, 250,"
	Lampz.MassAssign(73) = L073							'
	Lampz.Callback(73) = "DisableLighting pon073, 250,"
	Lampz.MassAssign(74) = L074							'
	Lampz.Callback(74) = "DisableLighting pon074, 250,"

	Lampz.MassAssign(81) = L081							'
	Lampz.Callback(81) = "DisableLighting pon081, 250,"
	Lampz.MassAssign(82) = L082							'
	Lampz.Callback(82) = "DisableLighting pon082, 250,"
	Lampz.MassAssign(83) = L083							'
	Lampz.Callback(83) = "DisableLighting pon083, 250,"
	Lampz.MassAssign(84) = L084							'
	Lampz.Callback(84) = "DisableLighting pon084, 250,"

	Lampz.MassAssign(91) = L091							'
	Lampz.Callback(91) = "DisableLighting pon091, 333,"
	Lampz.MassAssign(92) = L092							'
	Lampz.Callback(92) = "DisableLighting pon092, 333,"
	Lampz.MassAssign(93) = L093							'
	Lampz.Callback(93) = "DisableLighting pon093, 333,"
	Lampz.MassAssign(94) = L094							'
	Lampz.Callback(94) = "DisableLighting pon094, 333,"

	Lampz.MassAssign(96) = L096						'
	Lampz.MassAssign(96) = LB096	
	Lampz.Callback(96) = "DisableLighting pon096, 200,"
	Lampz.MassAssign(97) = L097	
	Lampz.MassAssign(97) = LB097							'
	Lampz.Callback(97) = "DisableLighting pon097, 200,"
	Lampz.MassAssign(98) = L098	
	Lampz.MassAssign(98) = LB098							'
	Lampz.Callback(98) = "DisableLighting pon098, 350,"
	Lampz.MassAssign(99) = L099	
	Lampz.MassAssign(99) = LB099						'
	Lampz.Callback(99) = "DisableLighting pon099, 350,"

	Lampz.MassAssign(102) = L102
	Lampz.MassAssign(102) = LB102					'
	Lampz.Callback(102) = "DisableLighting pon102, 400,"
	Lampz.Callback(102) = "DisableLighting poff102, 77,"
	Lampz.MassAssign(103) = L103							'
	Lampz.MassAssign(103) = LB103
	Lampz.Callback(103) = "DisableLighting pon103, 400,"
'	Lampz.Callback(103) = "DisableLighting poff103, 77,"

	Lampz.MassAssign(104) = L104	
	Lampz.MassAssign(104) = Light018
	Lampz.Callback(104) = "DisableLighting pon104, 222,"
	Lampz.Callback(104) = "DisableLighting poff104, 77,"

	Lampz.MassAssign(105) = L105								' overtime EB
	Lampz.MassAssign(105) = Light022
	Lampz.Callback(105) = "DisableLighting pon105, 188,"
	Lampz.Callback(105) = "DisableLighting poff105, 30,"

	Lampz.MassAssign(106) = L106						'
	Lampz.Callback(106) = "DisableLighting p106, 160,"
	Lampz.Callback(106) = "DisableLighting pstar106, 0.1,"
	Lampz.MassAssign(107) = L107						'
	Lampz.Callback(107) = "DisableLighting p107, 160,"
	Lampz.Callback(107) = "DisableLighting pstar107, 0.1,"
	Lampz.MassAssign(108) = L108						'
	Lampz.Callback(108) = "DisableLighting p108, 160,"
	Lampz.Callback(106) = "DisableLighting pstar108, 0.1,"
	Lampz.MassAssign(109) = L109						'
	Lampz.Callback(109) = "DisableLighting p109, 160,"
	Lampz.Callback(109) = "DisableLighting pstar109, 0.1,"


	Lampz.MassAssign(110) = L110	
	Lampz.MassAssign(110) = Light019
	Lampz.Callback(110) = "DisableLighting pon110, 300,"
	Lampz.Callback(110) = "DisableLighting poff110, 120,"
	Lampz.MassAssign(111) = L111
	Lampz.MassAssign(111) = Light020						'
	Lampz.Callback(111) = "DisableLighting pon111, 300,"
	Lampz.Callback(111) = "DisableLighting poff111, 120,"



	Lampz.MassAssign(113) = volkanlight						'
	Lampz.Callback(113) = "DisableLighting p31, 9,"


	Lampz.MassAssign(114) = ReleaseBulbLight						'
	Lampz.Callback(114) = "DisableLighting ReleaseBulb, 5,"

	Lampz.MassAssign(115) = GaugeRightLight	
	Lampz.MassAssign(116) = GaugeLeftLight	

	Lampz.MassAssign(117) = ButtonLED001
	Lampz.Callback(117) = "DisableLighting ButtonBulb001, 2,"
	Lampz.MassAssign(118) = ButtonLED002
	Lampz.Callback(118) = "DisableLighting ButtonBulb002, 2,"
	Lampz.MassAssign(119) = ButtonLED003
	Lampz.Callback(119) = "DisableLighting ButtonBulb003, 2,"
	Lampz.MassAssign(120) = ButtonLED004
	Lampz.Callback(120) = "DisableLighting ButtonBulb004, 2,"
	Lampz.MassAssign(121) = ButtonLED005
	Lampz.Callback(121) = "DisableLighting ButtonBulb005, 2,"
	Lampz.MassAssign(122) = ButtonLED006
	Lampz.Callback(122) = "DisableLighting ButtonBulb006, 2,"
	Lampz.MassAssign(123) = ButtonLED007
	Lampz.Callback(123) = "DisableLighting ButtonBulb007, 2,"
	Lampz.MassAssign(124) = ButtonLED008
	Lampz.Callback(124) = "DisableLighting ButtonBulb008, 2,"
	Lampz.MassAssign(125) = ButtonLED009
	Lampz.Callback(125) = "DisableLighting ButtonBulb009, 2,"
	Lampz.MassAssign(126) = ButtonLED010
	Lampz.Callback(126) = "DisableLighting ButtonBulb010, 2,"
	Lampz.MassAssign(127) = ButtonLED011
	Lampz.Callback(127) = "DisableLighting ButtonBulb011, 2,"
	Lampz.MassAssign(128) = ButtonLED012
	Lampz.Callback(128) = "DisableLighting ButtonBulb012, 2,"




' trainlights
	Lampz.MassAssign(133) = Trainred
	Lampz.Callback(133) = "DisableLighting primitive008, 50,"
	Lampz.MassAssign(134) = Traingreen
	Lampz.Callback(134) = "DisableLighting primitive012, 50,"
' FlasherS
	Lampz.MassAssign(135) = Flasherlight3
	Lampz.MassAssign(135) = Light001
	Lampz.Callback(135) = "Flasherflash Flasherflash3, 380,"
	Lampz.Callback(135) = "DisableLightingA FlasherStokeNEW, 2,"
	Lampz.Callback(135) = "UpdateLightmap Primitive004, 160,"

	Lampz.MassAssign(136) = Flasherlight4
	Lampz.MassAssign(136) = Flasherlight001
	Lampz.Callback(136) = "Flasherflash Flasherflash4, 444,"
	Lampz.Callback(136) = "DisableLightingB FlasherBlast, 7,"
	Lampz.Callback(136) = "UpdateLightmap Primitive001, 250,"


	Lampz.MassAssign(137) = Flasherlight1
	Lampz.MassAssign(137) = Flasherlight003
	Lampz.Callback(137) = "Flasherflash Flasherflash1, 400,"
	Lampz.Callback(137) = "DisableLightingC FlasherElectric, 2,"


	Lampz.MassAssign(138) = Flasherlight2
	Lampz.MassAssign(138) = Flasherlight002
	Lampz.Callback(138) = "Flasherflash Flasherflash2, 444,"
	Lampz.Callback(138) = "Flasherblue FlasherRightBlue, 60,"
	Lampz.Callback(138) = "DisableLightingD FlasherSteam001, 7,"
		

	Lampz.MassAssign(139) = plungerlight					'
	Lampz.Callback(139) = "DisableLighting plungerbulb, 5,"

	If VRroom = 0 Then
		Lampz.MassAssign(141) = DT_Abigail
		Lampz.MassAssign(142) = DT_Baron_Light
		Lampz.MassAssign(143) = DT_Tweed_Light
		Lampz.MassAssign(144) = DT_Volkan1
		Lampz.MassAssign(145) = DT_Volkan2
		Lampz.MassAssign(146) = DT_WellsVerne
	End If

	Lampz.MassAssign(151) = Light031
	Lampz.MassAssign(152) = Light032
	Lampz.MassAssign(153) = Light033
	Lampz.MassAssign(154) = Light034
	Lampz.MassAssign(155) = Light035
	Lampz.MassAssign(156) = Light036


	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub



'====================
'Class jungle nf
'====================

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
'Version 0.14 - apophis - added IsLight property to the class
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks

Class LampFader
	Public IsLight(160)					'apophis
	Public FadeSpeedDown(160), FadeSpeedUp(160)
	Private Lock(160), Loaded(160), OnOff(160)
	Public UseFunction
	Private cFilter
	Public UseCallback(160), cCallback(160)
	Public Lvl(160), Obj(160)
	Private Mult(160)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
			IsLight(x) = False   		'apophis
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		''debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next
		'msgbox "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
				if typename(aInput) = "Light" then IsLight(aIdx) = True   'apophis - If first object in array is a light, this will be set true
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		''debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class


'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function


'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
			AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function

'******************************************************
'****  END LAMPZ
'******************************************************





'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF  : Set LF  = New FlipperPolarity
dim RF  : Set RF  = New FlipperPolarity
dim LF2 : Set LF2 = New FlipperPolarity
dim RF2 : Set RF2 = New FlipperPolarity

InitPolarity

'*******************************************
' Early 90's and after

Sub InitPolarity()
        dim x, a : a = Array(LF, RF, LF2, RF2)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
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
        LF2.Object = LeftFlipper2      
        LF2.EndPoint = EndPointLp2
        RF2.Object = RightFlipper2
        RF2.EndPoint = EndPointRp2
End Sub


' Flipper trigger hit subs
Sub TriggerLF_Hit() 	: LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() 	: LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() 	: RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() 	: RF.PolarityCorrect activeball : End Sub
Sub TriggerLF2_Hit() 	: LF2.Addball activeball : End Sub
Sub TriggerLF2_UnHit() 	: LF2.PolarityCorrect activeball : End Sub
Sub TriggerRF2_Hit() 	: RF2.Addball activeball : End Sub
Sub TriggerRF2_UnHit() 	: RF2.PolarityCorrect activeball : End Sub



'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

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
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSling
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSling
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

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



'******************************************************
'  FLIPPER POLARITY. RUBBER DAMPENER, AND SLINGSHOT CORRECTION SUPPORTING FUNCTIONS 
'******************************************************


Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub


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
	FlipperTricks LeftFlipper2, LFPress2, LFCount2, LFEndAngle2, LFState2
	FlipperTricks RightFlipper2, RFPress2, RFCount2, RFEndAngle2, RFState2
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
'	Dim BOT
'	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to 4'Ubound(BOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to 4'Ubound(BOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					gBOT(b).velx = gBOT(b).velx / 1.3
					gBOT(b).vely = gBOT(b).vely - 0.5
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

dim LFPress, RFPress, LFCount, RFCount
dim LFPress2, RFPress2, LFCount2, RFCount2
dim LFState, RFState
dim LFState2, RFState2
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle
dim RFEndAngle2, LFEndAngle2

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
LFState2 = 1
RFState2 = 1
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
Const EOSReturn = 0.035  'mid 80's to early 90's
'Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
LFEndAngle2 = Leftflipper2.endangle
RFEndAngle2 = RightFlipper2.endangle

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
		Dim b
'		Dim BOT
'		BOT = GetBalls

		For b = 0 to 4'UBound(BOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= -0.4 Then gBOT(b).vely = -0.4
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


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************




'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7

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



'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR



Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub
Sub zCol_Rubber_Post004_Hit
	RubbersD.dampen Activeball
End Sub

Sub zCol_Rubber_Post006_Hit
	RubbersD.dampen Activeball
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



'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID
		allBalls = getballs

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




'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************


Sub ResetHS
	HighscoreTweed(0) = 20000
	HighscoreTweed(1) = 30000
	HighscoreTweed(2) = 50000
	HighscoreBaron(0) = 20000
	HighscoreBaron(1) = 30000
	HighscoreBaron(2) = 50000
	HighscoreDuelTweed(0) = 20000
	HighscoreDuelTweed(1) = 30000
	HighscoreDuelTweed(2) = 50000
	HighscoreDuelBaron(0) = 20000
	HighscoreDuelBaron(1) = 30000
	HighscoreDuelBaron(2) = 50000
	Savehs
End Sub

Sub Loadhs
    Dim x
'    x = LoadValue(TableName, "HighScore1Name")
'    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If

    x = LoadValue(TableName, "HighScore1")
    If(x <> "") Then HighscoreTweed(0) = CDbl(x) Else HighscoreTweed(0) = 20000 End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "") Then HighscoreTweed(1) = CDbl(x) Else HighscoreTweed(1) = 30000 End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "") Then HighscoreTweed(2) = CDbl(x) Else HighscoreTweed(2) = 50000 End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "") Then HighscoreBaron(0) = CDbl(x) Else HighscoreBaron(0) = 20000 End If
    x = LoadValue(TableName, "HighScore5")
    If(x <> "") Then HighscoreBaron(1) = CDbl(x) Else HighscoreBaron(1) = 30000 End If
    x = LoadValue(TableName, "HighScore6")
    If(x <> "") Then HighscoreBaron(2) = CDbl(x) Else HighscoreBaron(2) = 50000 End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "") Then HighscoreDuelTweed(0) = CDbl(x) Else HighscoreDuelTweed(0) = 20000 End If
    x = LoadValue(TableName, "HighScore5")
    If(x <> "") Then HighscoreDuelTweed(1) = CDbl(x) Else HighscoreDuelTweed(1) = 30000 End If
    x = LoadValue(TableName, "HighScore6")
    If(x <> "") Then HighscoreDuelTweed(2) = CDbl(x) Else HighscoreDuelTweed(2) = 50000 End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "") Then HighscoreDuelBaron(0) = CDbl(x) Else HighscoreDuelBaron(0) = 20000 End If
    x = LoadValue(TableName, "HighScore5")
    If(x <> "") Then HighscoreDuelBaron(1) = CDbl(x) Else HighscoreDuelBaron(1) = 30000 End If
    x = LoadValue(TableName, "HighScore6")
    If(x <> "") Then HighscoreDuelBaron(2) = CDbl(x) Else HighscoreDuelBaron(2) = 50000 End If

    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0 ' fix  dof for "credit button blink"
'    x = LoadValue(TableName, "TotalGamesPlayed")
'    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
'    SaveValue TableName, "HighScore1Name", HighScoreName(0)
    SaveValue TableName, "HighScore1" , HighscoreTweed(0)
    SaveValue TableName, "HighScore2" , HighscoreTweed(1)
    SaveValue TableName, "HighScore3" , HighscoreTweed(2)
    SaveValue TableName, "HighScore4" , HighscoreBaron(0)
    SaveValue TableName, "HighScore5" , HighscoreBaron(1)
    SaveValue TableName, "HighScore6" , HighscoreBaron(2)
    SaveValue TableName, "HighScore7" , HighscoreDuelTweed(0)
    SaveValue TableName, "HighScore8" , HighscoreDuelTweed(1)
    SaveValue TableName, "HighScore9" , HighscoreDuelTweed(2)
    SaveValue TableName, "HighScore10", HighscoreDuelBaron(0)
    SaveValue TableName, "HighScore11", HighscoreDuelBaron(1)
    SaveValue TableName, "HighScore12", HighscoreDuelBaron(2)
    SaveValue TableName, "Credits", Credits
'    SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub


'////////////////////////////////////////////////////////////////////////////////
'////          Mechanical Sounds, by Fleep                                   ////
'////                     Last Updated: January, 2022                        ////
'////////////////////////////////////////////////////////////////////////////////
'
'/////////////////////////////////  CARTRIDGES  /////////////////////////////////
'
'//  Specify which mechanical sound cartridge to use for each group of elements.
'//  Mechanical sounds naming convention: <CARTRIDGE>_<Soundset_Name>
'//
'//  Cartridge name is composed using the following convention:
'//  <TABLE MANUFACTURER ABBREVIATION>_<TABLE NAME ABBREVIATION>_<SOUNDSET REVISION NUMBER>
'//  
'//  General Mechanical Sounds Cartridges:
Const Cartridge_Bumpers					= "WS_PBT_REV01" 'Williams Whirlwind Cartridge REV01
Const Cartridge_Slingshots				= "WS_PBT_REV01"
Const Cartridge_Flippers				= "WS_PBT_REV01"
Const Cartridge_Kickers					= "WS_WHD_REV01"
Const Cartridge_Diverters				= "WS_DNR_REV01" 'Williams Diner Cartridge REV01
Const Cartridge_Knocker					= "WS_WHD_REV02" 'Williams Whirlwind Cartridge REV02
Const Cartridge_Relays					= "WS_WHD_REV01"
Const Cartridge_Trough					= "WS_WHD_REV01"
Const Cartridge_Rollovers				= "WS_WHD_REV01"
Const Cartridge_Targets					= "WS_WHD_REV01"
Const Cartridge_Gates					= "WS_WHD_REV01"
Const Cartridge_Spinner					= "SY_TNA_REV01" 'Spooky Total Nuclear Annihilation Cartridge REV01
Const Cartridge_Rubber_Hits				= "WS_WHD_REV01"
Const Cartridge_Metal_Hits				= "WS_WHD_REV01"
Const Cartridge_Plastic_Hits			= "WS_WHD_REV01"
Const Cartridge_Wood_Hits				= "WS_WHD_REV01"
Const Cartridge_Cabinet_Sounds			= "WS_WHD_REV01"
Const Cartridge_Drain					= "WS_WHD_REV01"
Const Cartridge_Apron					= "WS_WHD_REV01"
Const Cartridge_Ball_Roll				= "BY_TOM_REV01" 'Bally Theatre of Magic Cartridge REV01
Const Cartridge_BallBallCollision		= "BY_WDT_REV01" 'Bally WHO Dunnit Cartridge REV01
Const Cartridge_Ball_Drop_Bump			= "WS_WHD_REV01"
Const Cartridge_Plastic_Ramps			= "WS_WHD_REV01"
Const Cartridge_Metal_Ramps				= "WS_WHD_REV01"
Const Cartridge_Ball_Guides				= "WS_WHD_REV01"
Const Cartridge_Table_Specifics			= "WS_WHD_REV01"




'////////////////////////////  SOUND SOURCE CREDITS  ////////////////////////////
'//  Special thanks go to the following contributors who have provided audio 
'//  footage recordings:
'//  
'//  Williams Whirlwind - Blackmoor, wrd1972
'//  Williams Diner - Nick Rusis
'//  Spooky Total Nuclear Annihilation - WildDogArcade, Ed and Gary
'//  Bally Theatre of Magic - CalleV, nickbuol
'//  Bally WHO Dunnit - Amazaley1


'///////////////////////////////  USER PARAMETERS  //////////////////////////////
'
'//  Sounds Parameter with suffix "SoundLevel" can have any value in range [0..1]
'//  Sounds Parameter with suffix "SoundMultiplier" can have any value


'///////////////////////////  SOLENOIDS (COILS) CONFIG  /////////////////////////

'//  FLIPPER COILS:
'//  Flippers in this table: Lower Left Flipper, Lower Right Flipper, Upper Right Fliiper
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel
Dim FlipperLeftLowerHitParm, FlipperRightUpperHitParm, FlipperRightLowerHitParm

'//  Flipper Up Attacks initialize during playsound subs
Dim FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel


FlipperUpSoundLevel = 1
FlipperDownSoundLevel = 0.65
FlipperUpAttackMinimumSoundLevel = 0.010
FlipperUpAttackMaximumSoundLevel = 0.435


'//  Flipper Hit Param initialize with FlipperUpSoundLevel
'//  and dynamically modified calculated by ball flipper collision
FlipperLeftLowerHitParm = FlipperUpSoundLevel
FlipperRightLowerHitParm = FlipperUpSoundLevel
FlipperRightUpperHitParm = FlipperUpSoundLevel


'//  CONTROLLED / SWITCHED COILS:
'//  Solenoid 01A = Outhole Kicker
'//  Solenoid 02A = Shooter Feeder
'//  Solenoid 03A = Right Ramp Lifter
'//  Solenoid 04A = Left Locking Kickback
'//  Solenoid 05A = Top Eject
'//  Solenoid 06A = Knocker
'//  Solenoid 07A,08A = 3-Bank Drop Target Reset, 1-Bank Drop Target Reset
'//  Solenoid 13 = Diverter
'//  Solenoid 14 = Under Playfield Kickbig
'//  Solenoid 09,10,15,17,19,21 = 3 Lower Bumpers, 3 Upper Bumpers
'//  Solenoid 18,20 = Left Kicker (Slingshot), Right Kicker (Slingshot)
'//  Solenoid 22 = Right Ramp Down

Dim Solenoid_OutholeKicker_SoundLevel, Solenoid_ShooterFeeder_SoundLevel
Dim Solenoid_RightRampLifter_SoundLevel, Solenoid_LeftLockingKickback_SoundLevel
Dim Solenoid_TopEject_SoundLevel, Solenoid_Knocker_SoundLevel, Solenoid_DropTargetReset_SoundLevel
Dim Solenoid_Diverter_Enabled_SoundLevel, Solenoid_Diverter_Hold_SoundLevel, Solenoid_Diverter_Disabled_SoundLevel
Dim Solenoid_UnderPlayfieldKickbig_SoundLevel, Solenoid_Bumper_SoundMultiplier
Dim Solenoid_Slingshot_SoundLevel, Solenoid_RightRampDown_SoundLevel, AutoPlungerSoundLevel

AutoPlungerSoundLevel = 1												'volume level; range [0, 1]
Solenoid_OutholeKicker_SoundLevel = 1
Solenoid_ShooterFeeder_SoundLevel = 1
Solenoid_RightRampLifter_SoundLevel = 0.3
Solenoid_RightRampDown_SoundLevel = 0.3
Solenoid_LeftLockingKickback_SoundLevel = 1
Solenoid_TopEject_SoundLevel = 1
Solenoid_Knocker_SoundLevel = 1
Solenoid_DropTargetReset_SoundLevel = 1
Solenoid_Diverter_Enabled_SoundLevel = 1
Solenoid_Diverter_Hold_SoundLevel = 0.7
Solenoid_Diverter_Disabled_SoundLevel = 0.4
Solenoid_UnderPlayfieldKickbig_SoundLevel = 1
Solenoid_Bumper_SoundMultiplier = 0.004 '8
Solenoid_Slingshot_SoundLevel = 1


'//  RELAYS:
'//  Solenoid 16 = Lower Playfield Relay GI Relay (P/N 5580-12145-00) / Backbox GI Relay (P/N 5580-09555-01)
'//  Solenoid 11 = Upper Playfield Relay GI Relay (P/N 5580-12145-00)
'//  Solenoid 12 = Solenoid A/C Select Relay (5580-09555-01)
'//  Fake Solenoid = Flahser Relay

Dim RelayLowerGISoundLevel, RelayUpperGISoundLevel, RelaySolenoidACSelectSoundLevel, RelayFlasherSoundLevel
RelayLowerGISoundLevel = 0.45
RelayUpperGISoundLevel = 0.45
RelaySolenoidACSelectSoundLevel = 0.3
RelayFlasherSoundLevel = 0.015


'//  EXTRA SOLENOIDS:
'//  Solenoid 24 = Blower Motor (Ontop Backbox)
'//  Solenoid 27 = Spin Wheels Motor
Dim Solenoid_BlowerMotor_SoundLevel, Solenoid_SpinWheelsMotor_SoundLevel
Solenoid_BlowerMotor_SoundLevel = 0.2
Solenoid_SpinWheelsMotor_SoundLevel = 0.2


'////////////////////////////  SWITCHES SOUND CONFIG  ///////////////////////////
Dim Switch_Gate_SoundLevel, SpinnerSoundLevel, RolloverSoundLevel, OutLaneRolloverSoundLevel, TargetSoundFactor

Switch_Gate_SoundLevel = 1	
SpinnerSoundLevel = 0.1
RolloverSoundLevel = 0.55
OutLaneRolloverSoundLevel = 0.8
TargetSoundFactor = 0.8


'////////////////////  BALL HITS, BUMPS, DROPS SOUND CONFIG  ////////////////////
Dim BallWithBallCollisionSoundFactor, BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor
Dim WallImpactSoundFactor, MetalImpactSoundFactor, WireformAntiRebountRailSoundFactor
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor
Dim BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor, OutlaneWallsSoundFactor
Dim EjectBallBumpSoundLevel, HeadSaucerSoundLevel, EjectHoleEnterSoundLevel
Dim RightRampMetalWireDropToPlayfieldSoundLevel, LeftPlasticRampDropToLockSoundLevel, LeftPlasticRampDropToPlayfieldSoundLevel
Dim CellarLeftEnterSoundLevel, CellarRightEnterSoundLevel, CellerKickouBallDroptSoundLevel


BallWithBallCollisionSoundFactor = 3.2
BallBouncePlayfieldSoftFactor = 0.0015
BallBouncePlayfieldHardFactor = 0.0075
WallImpactSoundFactor = 0.075
MetalImpactSoundFactor = 0.075
RubberStrongSoundFactor = 0.045
RubberWeakSoundFactor = 0.055
RubberFlipperSoundFactor = 0.65
BottomArchBallGuideSoundFactor = 0.2
FlipperBallGuideSoundFactor = 0.015
WireformAntiRebountRailSoundFactor = 0.04
OutlaneWallsSoundFactor = 1
EjectBallBumpSoundLevel = 1
RightRampMetalWireDropToPlayfieldSoundLevel = 1
LeftPlasticRampDropToLockSoundLevel = 1
LeftPlasticRampDropToPlayfieldSoundLevel = 1
EjectHoleEnterSoundLevel = 0.75
HeadSaucerSoundLevel = 0.15
CellerKickouBallDroptSoundLevel = 1
CellarLeftEnterSoundLevel = 0.85
CellarRightEnterSoundLevel = 0.85


'///////////////////////  OTHER PLAYFIELD ELEMENTS CONFIG  //////////////////////
Dim RollingSoundFactor, RollingOnDiscSoundFactor, BallReleaseShooterLaneSoundLevel
Dim LeftPlasticRampEnteranceSoundLevel, RightPlasticRampEnteranceSoundLevel
Dim LeftPlasticRampRollSoundFactor, RightPlasticRampRollSoundFactor
Dim LeftMetalWireRampRollSoundFactor, RightPlasticRampHitsSoundLevel, LeftPlasticRampHitsSoundLevel
Dim SpinningDiscRolloverSoundFactor, SpinningDiscRolloverBumpSoundLevel
Dim LaneSoundFactor, LaneEnterSoundFactor, InnerLaneSoundFactor
Dim LaneLoudImpactMinimumSoundLevel, LaneLoudImpactMaximumSoundLevel

RollingSoundFactor = 50
RollingOnDiscSoundFactor = 1.5
BallReleaseShooterLaneSoundLevel = 1
LeftPlasticRampEnteranceSoundLevel = 0.1
RightPlasticRampEnteranceSoundLevel = 0.1
LeftPlasticRampRollSoundFactor = 0.2
RightPlasticRampRollSoundFactor = 0.2
LeftMetalWireRampRollSoundFactor = 1
RightPlasticRampHitsSoundLevel = 1
LeftPlasticRampHitsSoundLevel = 1
SpinningDiscRolloverSoundFactor = 0.05
SpinningDiscRolloverBumpSoundLevel = 0.3
LaneEnterSoundFactor = 0.9
InnerLaneSoundFactor = 0.0005
LaneSoundFactor = 0.0004
LaneLoudImpactMinimumSoundLevel = 0
LaneLoudImpactMaximumSoundLevel = 0.4


'///////////////////////////  CABINET SOUND PARAMETERS  /////////////////////////
Dim NudgeLeftSoundLevel, NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel
Dim PlungerReleaseSoundLevel, PlungerPullSoundLevel, CoinSoundLevel

NudgeLeftSoundLevel = 1
NudgeRightSoundLevel = 1
NudgeCenterSoundLevel = 1
StartButtonSoundLevel = 0.1
PlungerReleaseSoundLevel = 1
PlungerPullSoundLevel = 1
CoinSoundLevel = 1


'///////////////////////////  MISC SOUND PARAMETERS  ////////////////////////////
Dim LutToggleSoundLevel : 
LutToggleSoundLevel = 0.5


'////////////////////////////////  SOUND HELPERS  ///////////////////////////////
Dim SoundOn : SoundOn = 1
Dim SoundOff : SoundOff = 0
Dim Up : Up = 0
Dim Down : Down = 1
Dim RampUp : RampUp = 1
Dim RampDown : RampDown = 0
Dim RampDownSlow : RampDownSlow = 1
Dim RampDownFast : RampDownFast = 2
Dim CircuitA : CircuitA = 0
Dim CircuitC : CircuitC = 1

'//  Helper for Main (Lower) flippers dampened stroke
Dim BallNearLF : BallNearLF = 0
Dim BallNearRF : BallNearRF = 0

Sub TriggerBallNearLF_Hit()
	'Debug.Print "BallNearLF = 1"
	BallNearLF = 1
End Sub

Sub TriggerBallNearLF_UnHit()
	'Debug.Print "BallNearLF = 0"
	BallNearLF = 0
End Sub

Sub TriggerBallNearRF_Hit()
	'Debug.Print "BallNearRF = 1"
	BallNearRF = 1
End Sub

Sub TriggerBallNearRF_UnHit()
	'Debug.Print "BallNearLF = 0"
	BallNearRF = 0
End Sub


'///////////////////////  SOUND PLAYBACK SUBS / FUNCTIONS  //////////////////////
'//////////////////////  POSITIONAL SOUND PLAYBACK METHODS  /////////////////////
'//  Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
'//  These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
'//  For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
'//  For stereo setup - positional sound playback functions will only pan between left and right channels
'//  For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels
'//  
'//  PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
'//  Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStaticLoop(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
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


'//////////////////////  SUPPORTING BALL & SOUND FUNCTIONS  /////////////////////
'//  Fades between front and back of the table 
'//  (for surround systems or 2x2 speakers, etc), depending on the Y position 
'//  on the table.
Function AudioFade(tableobj)
	Dim tmp
	Select Case PositionalSoundPlaybackConfiguration
		Case 1
			AudioFade = 0
		Case 2
			AudioFade = 0
		Case 3
			tmp = tableobj.y * 2 / tableheight-1
			If tmp > 0 Then
				AudioFade = Csng(tmp ^10)
			Else
				AudioFade = Csng(-((- tmp) ^10) )
			End If
	End Select
End Function

'//  Calculates the pan for a tableobj based on the X position on the table.
Function AudioPan(tableobj)
	Dim tmp
	Select Case PositionalSoundPlaybackConfiguration
		Case 1
			AudioPan = 0
		Case 2
			tmp = tableobj.x * 2 / tablewidth-1
			If tmp > 0 Then
				AudioPan = Csng(tmp ^10)
			Else
				AudioPan = Csng(-((- tmp) ^10) )
			End If
		Case 3
			tmp = tableobj.x * 2 / tablewidth-1

			If tmp > 0 Then
				AudioPan = Csng(tmp ^10)
			Else
				AudioPan = Csng(-((- tmp) ^10) )
			End If
	End Select
End Function

'//  Calculates the volume of the sound based on the ball speed
Function Vol(ball)
	Vol = Csng(BallVel(ball) ^2)
End Function

'//  Calculates the pitch of the sound based on the ball speed
Function Pitch(ball)
    Pitch = BallVel(ball) * 20
End Function

'//  Calculates the ball speed
Function BallVel(ball)
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'//  Calculates the roll volume of the sound based on the ball speed
Dim TempBallVel
Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	TempBallVel = Csng((INT(SQR((ball.VelX^2)+(ball.VelY^2))))/RollingSoundFactor)
	If TempBallVel = 1 Then TempBallVel = 0.999
	If TempBallVel = 0 Then TempBallVel = 0.001
	'debug.print TempBallVel
	TempBallVel = Csng(1/(1+(0.275*(((0.75*TempBallVel)/(1-TempBallVel))^(-2)))))
	VolPlayfieldRoll = TempBallVel
End Function

'//  Calculates the roll volume of the sound based on the ball speed
Function VolSpinningDiscRoll(ball)
	VolSpinningDiscRoll = RollingOnDiscSoundFactor * 0.1 * Csng(BallVel(ball) ^3)
End Function

'//  Calculates the roll volume of the sound based on the ball speed
Dim TempBallVelPlastic
Function VolPlasticMetalRampRoll(ball)
	'VolPlasticMetalRampRoll = RollingOnDiscSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
	TempBallVelPlastic = Csng((INT(SQR((ball.VelX^2)+(ball.VelY^2))))/RollingSoundFactor)
	If TempBallVelPlastic = 1 Then TempBallVelPlastic = 0.999
	If TempBallVelPlastic = 0 Then TempBallVelPlastic = 0.001
	'debug.print TempBallVel
	TempBallVelPlastic = Csng(1/(1+(0.275*(((0.75*TempBallVelPlastic)/(1-TempBallVelPlastic))^(-2)))))
	VolPlasticMetalRampRoll = TempBallVelPlastic
End Function

'//  Calculates the roll pitch of the sound based on the ball speed
Dim TempPitchBallVel
Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	'PitchPlayfieldRoll = BallVel(ball) ^2 * 15
	'PitchPlayfieldRoll = Csng(BallVel(ball))/50 * 10000
	'PitchPlayfieldRoll = (1-((Csng(BallVel(ball))/50)^0.2)) * 20000

	'PitchPlayfieldRoll = (2*((Csng(BallVel(ball)))^0.7))/(2+(Csng(BallVel(ball)))) * 16000
	TempPitchBallVel = Csng((INT(SQR((ball.VelX^2)+(ball.VelY^2))))/50)
	If TempPitchBallVel = 1 Then TempPitchBallVel = 0.999
	If TempPitchBallVel = 0 Then TempPitchBallVel = 0.001
	TempPitchBallVel = Csng(1/(1+(0.275*(((0.75*TempPitchBallVel)/(1-TempPitchBallVel))^(-2))))) * 10000
	PitchPlayfieldRoll = TempPitchBallVel
End Function

'//  Calculates the pitch of the sound based on the ball speed.
'//  Used for plastic ramps roll sound
Function PitchPlasticRamp(ball)
    PitchPlasticRamp = BallVel(ball) * 20
End Function

'//  Determines if a Points (px,py) is inside a 4 point polygon A-D
'//  in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
	Dim AB, BC, CD, DA
	AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
	BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
	CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
	DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)
 
	If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
		InRect = True
	Else
		InRect = False       
	End If
End Function

'//  Determines if a point (px,py) in inside a circle with a center of 
'//  (cx,cy) coordinates and circleradius
Function InCircle(px,py,cx,cy,circleradius)
	Dim distance
	distance = SQR(((px-cx)^2) + ((py-cy)^2))
	
	If (distance < circleradius) Then
		InCircle = True
	Else
		InCircle = False
	End If
End Function

'///////////////////////////  PLAY SOUNDS SUBROUTINES  //////////////////////////
'//  
'//  These Subroutines implement all mechanical playsounds including timers
'//  
'//////////////////////////  GENERAL SOUND SUBROUTINES  /////////////////////////
Sub SoundStartButton()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Start_Button"), StartButtonSoundLevel, StartButtonPosition
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Plunger_Pull_Slow"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerPullStop()
	StopSound Cartridge_Cabinet_Sounds & "_Plunger_Pull_Slow"
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Plunger_Release_Ball_" & Int(Rnd*3)+1), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Plunger_Release_Empty"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*3)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*3)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySoundAtLevelStatic ("Nudge_" & Int(Rnd*3)+1), NudgeCenterSoundLevel * VolumeDial, drain
End Sub



'//////  JP'S VP10 ROLLING SOUNDS & FLEEP RAMPS / SPINNING DISC ROLLOVER  ///////
Const tnob = 5 ' total number of balls
Const lob = 0
ReDim rolling(tnob)
ReDim ramprolling(tnob)
Dim DropCount
ReDim DropCount(tnob)


InitRolling




Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
		ramprolling(i) = False
		DropCount(i) = 0
    Next
End Sub

Sub Trigger011_Hit
	Activeball.angmomx=0
	Activeball.angmomy=0
	Activeball.angmomz=0
	Activeball.velx=0
	Activeball.vely=0
	Activeball.velz=0
	Activeball.x = 468
	Activeball.y = 73
	Activeball.z = 27
	debug.print "BallMoved trigger011"
End Sub
Sub Trigger012_Hit
	Activeball.angmomx=0
	Activeball.angmomy=0
	Activeball.angmomz=0
	Activeball.velx=0
	Activeball.vely=0
	Activeball.velz=0
	Activeball.x = 468
	Activeball.y = 73
	Activeball.z = 27
	debug.print "BallMoved trigger012"
End Sub


Sub RollingSoundUpdate()
	Dim b ', bot
'	bot = getballs
	
	For b = 0 to 4 'UBound(BOT)
		If gBOT(b).z < -15 Then
			gBOT(b).angmomx=0
			gBOT(b).angmomy=0
			gBOT(b).angmomz=0
			gBOT(b).velx=0
			gBOT(b).vely=0
			gBOT(b).velz=0
			gBOT(b).x = 468
			gBOT(b).y = 73
			gBOT(b).z = 27
'			debug.print "BallMoved z<-15"
		Elseif gBOT(b).y < 19 Then	
			gBOT(b).angmomx=0
			gBOT(b).angmomy=0
			gBOT(b).angmomz=0
			gBOT(b).velx=0
			gBOT(b).vely=0
			gBOT(b).velz=0
			gBOT(b).x = 468
			gBOT(b).y = 73
			gBOT(b).z = 27
'			debug.print "BallMoved y<19"
'		Elseif gbot(b).y < 170 and gBOT(b).x > 305 and gbot(b).x < 517 Then
'			gBOT(b).angmomx=0
'			gBOT(b).angmomy=0
'			gBOT(b).angmomz=0
'			gBOT(b).velx=0
'			gBOT(b).vely=0
'			gBOT(b).velz=0
'			gBOT(b).x = 468
'			gBOT(b).y = 73
'			gBOT(b).z = 27
'			debug.print "BallMoved aroundBlast"
		End If


		If gBOT(b).z < 27 and gBOT(b).z > 23 Then
			' play the rolling sound for each ball
			If BallVel(gBOT(b) ) > 0.01 Then
				rolling(b) = True
				PlaySound (Cartridge_Ball_Roll & "_Ball_Roll_" & b), -1, VolPlayfieldRoll(gBOT(b)) * PlayfieldRollVolumeDial * VolumeDial * 1.25, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))
				
			End If
		Else

			If gBOT(b).z > 45 Then

				' Play the wire ramp - wire part sound for each ball
				' WireRampSound and SkillRampSound and not in WireRampSoundOff
				If BallVel(gBOT(b) ) > 1 Then
					ramprolling(b) = True
					'StopSound (Cartridge_Metal_Ramps & "_Ramp_Left_Plastic_BallRoll_" & b)
					PlaySound (Cartridge_Metal_Ramps & "_Ramp_Left_Metal_Wire_BallRoll_" & b), 0, (VolPlasticMetalRampRoll(gBOT(b))) * LeftMetalWireRampRollSoundFactor * RampsRollVolumeDial * VolumeDial * 1.25, AudioPan(gBOT(b)), 0, 0, 1, 0, AudioFade(gBOT(b))
					'Debug.Print Cartridge_Metal_Ramps & "_Ramp_Left_Metal_Wire_BallRoll_" & b
				End If

			Else
				If ramprolling(b) = True Then
					ramprolling(b) = False
					StopSound (Cartridge_Plastic_Ramps & "_Ramp_Left_Plastic_BallRoll_" & b)
					StopSound (Cartridge_Metal_Ramps & "_Ramp_Left_Metal_Wire_BallRoll_" & b)
					StopSound (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_BallRoll_" & b)
					StopSound (Cartridge_Ball_Roll & "_Ball_Roll_" & b)
				End If
			End If

			'***Ball Drop Sounds***
			If gBOT(b).VelZ < -1 and gBOT(b).z < 55 and gBOT(b).z > 27 Then 'height adjust for ball drop sounds
'					debug.print "ball drop velz: " & BOT(b).velz
				If DropCount(b) >= 5 Then
					If gBOT(b).velz > -5 Then
						If gBOT(b).z < 35 Then
'								debug.print "--> random sound soft: " & BOT(b).velz
							DropCount(b) = 0
							RandomSoundBallBouncePlayfieldSoft gBOT(b)
						end if
					Else
'							debug.print "--> random sound hard: " & BOT(b).velz
						DropCount(b) = 0
						RandomSoundBallBouncePlayfieldHard gBOT(b)
					End If				
				End If

			End If

			If DropCount(b) < 5 Then
				DropCount(b) = DropCount(b) + 1
			End If
		End If
		
		If rolling(b) = True and (BallVel(gBOT(b) ) <= 1 or gBOT(B).z < 23 or gBOT(b).z > 27) Then			
			StopSound (Cartridge_Ball_Roll & "_Ball_Roll_" & b)
			rolling(b) = False
'				If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0 'todo what is this??
		End If

		If ramprolling(b) = True and gBOT(b).z <= 46 Then
			ramprolling(b) = False
'				debug.print "stop sounds"
			StopSound (Cartridge_Plastic_Ramps & "_Ramp_Left_Plastic_BallRoll_" & b)
			StopSound (Cartridge_Metal_Ramps & "_Ramp_Left_Metal_Wire_BallRoll_" & b)
			StopSound (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_BallRoll_" & b)
			StopSound (Cartridge_Ball_Roll & "_Ball_Roll_" & b)	'this may break something...
		End If
		
		' "Static" Ball Shadows
		If AmbientBallShadowOn = 0 Then
			BallShadowA(b).visible = 1
			BallShadowA(b).X = gBOT(b).X + offsetX
			If gBOT(b).Z > 30 Then
				BallShadowA(b).height=gBOT(b).z - BallSize/4 + b/1000	'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				BallShadowA(b).Y = gBOT(b).Y + offsetY + BallSize/10
			Else
				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 1.04 + b/1000
				BallShadowA(b).Y = gBOT(b).Y + offsetY
			End If
		End If

	Next
End Sub

sub BallDrop1_hit	'sometimes drop sound cannot be heard here.
'	debug.print activeball.velz
	activeball.velz = activeball.velz * 2
end sub
'
'sub Plungerlanedrop_hit
'	debug.print "lane: " & activeball.velz
''	activeball.velz = activeball.velz * 2
'end sub

'///////////////////////  JP'S VP10 BALL COLLISION SOUND  ///////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	if abs(ball1.vely) < 1 And abs(ball2.vely) < 1 then
		exit sub	'don't rattle the locked balls
	end if
	PlaySound ("Ball_Collide_" & Int(Rnd*7)+1), 0, Csng(velocity) ^2 / 200 *  VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'
''///////////////////////////////  CELLAR SOUNDS  ///////////////////////////////
'Sub SoundCellarLeftEnter()
' 	dim finalspeed
'  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
'	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Scoop_Left_Cellar_Enter_" & Int(Rnd*4)+1), CellarLeftEnterSoundLevel * finalspeed/40, CellarLeftTrigger
'End Sub
'
'Sub SoundCellarRightEnter()
' 	dim finalspeed
'  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
'	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Scoop_Right_Cellar_Enter_" & Int(Rnd*5)+1), CellarRightEnterSoundLevel * finalspeed/40, CellarRightTrigger
'End Sub
'
'
'Sub SoundCellerKickout()
'	PlaySoundAtLevelStatic SoundFX(Cartridge_Kickers & "_Scoop_Kickout_Cellar_" & Int(Rnd*3)+1,DOFContactors), volumedial, swTrough1
'End Sub
'
'Sub SoundCellarKickoutBallDrop()
'	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Scoop_BallDrop_After_Kickout_" & Int(Rnd*2)+1), CellerKickouBallDroptSoundLevel, ScoopKickerOverflow
'End Sub



'///////////////////////////  GENERAL ROLLOVER SOUNDS  //////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall (Cartridge_Rollovers & "_Rollover_All_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub



'///////////////////////////  OUTLANE ROLLOVER SOUNDS  //////////////////////////
Sub RandomSoundOutlaneRollover()
	PlaySoundAtLevelActiveBall (Cartridge_Rollovers & "_Rollover_Outlane_" & Int(Rnd*4)+1), OutLaneRolloverSoundLevel
End Sub



'////////////////////  BALL GATES AND BRACKET GATES SOUNDS  /////////////////////
'Sub SoundRampBrktGate1(toggle)
'	If toggle = SoundOn Then
'		PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_1"), Switch_Gate_SoundLevel * 0.005 , sw33g
'	End If
'	If toggle = SoundOff Then
'		Stopsound Cartridge_Gates & "_Bracket_Gate_1"
'	End If
'End Sub
'
'Sub SoundRampBrktGate2(toggle)
'	If toggle = SoundOn Then
'		PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_2"), Switch_Gate_SoundLevel * 0.005, sw40g
'	End If
'	If toggle = SoundOff Then
'		Stopsound Cartridge_Gates & "_Bracket_Gate_2"
'	End If
'End Sub
'
'
Sub SoundBallGate0(object)
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_1"), Switch_Gate_SoundLevel * 0.005, object
End Sub

Sub SoundBallGate1(object)
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_2"), Switch_Gate_SoundLevel * 0.005, object
End Sub

Sub SoundBallGate2(object)
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_2"), Switch_Gate_SoundLevel * 0.005, object
End Sub
'
'Sub SoundBallGate3()	'sound of blocked gate
'	PlaySoundAtLevelStatic (Cartridge_Gates & "_Oneway_Ball_Gate_" & Int(Rnd*3)+1), Switch_Gate_SoundLevel, Gate3
'End Sub
'
'Sub SoundBallGate4()
'	PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_1"), Switch_Gate_SoundLevel * 0.005, Gate4
'End Sub
'
'Sub SoundBallGate5(toggle)
'	If toggle = SoundOn Then
'		PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_2"), Switch_Gate_SoundLevel * 0.005, Gate5
'	End If
'	If toggle = SoundOff Then
'		Stopsound Cartridge_Gates & "_Bracket_Gate_2"
'		PlaySoundAtLevelStatic (Cartridge_Gates & "_Oneway_Ball_Gate_" & Int(Rnd*3)+1), Switch_Gate_SoundLevel * 0.005, Gate5
'	End If
'End Sub
'
'Sub SoundBallGate6()
'	PlaySoundAtLevelStatic (Cartridge_Gates & "_Bracket_Gate_1"), Switch_Gate_SoundLevel * 0.005, Gate6
'End Sub
'
'Sub SoundBallGate8(toggle)
'	If toggle = SoundOn Then
'		PlaySoundAtLevelStatic ("TOM_Small_Gate_2"), Switch_Gate_SoundLevel * 0.02, Gate8
'	End If
'	If toggle = SoundOff Then
'		Stopsound "TOM_Small_Gate_2"
'		PlaySoundAtLevelStatic ("WS_WHD_REV01_Oneway_Ball_Gate_3"), Switch_Gate_SoundLevel * 0.002, Gate8
'	End If
'End Sub
'


Sub SoundBallReleaseGate(object)
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Oneway_Ball_Gate_" & Int(Rnd*3)+1), Switch_Gate_SoundLevel * 0.5, object
End Sub



'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////
Sub SoundDropTargetDT1()
	PlaySoundAtLevelStatic (Cartridge_Targets & "_Drop_Target_1Bank_Release_Down_" & Int(Rnd*2)+1), Vol(ActiveBall) * TargetSoundFactor, DT1
End Sub

Sub SoundDropTargetDT2()
	PlaySoundAtLevelStatic (Cartridge_Targets & "_Drop_Target_3Bank_Release_Down_" & Int(Rnd*6)+1), Vol(ActiveBall) * TargetSoundFactor, DT2
End Sub

Sub SoundDropTargetDT3()
	PlaySoundAtLevelStatic (Cartridge_Targets & "_Drop_Target_3Bank_Release_Down_" & Int(Rnd*6)+1), Vol(ActiveBall) * TargetSoundFactor, DT3
End Sub

Sub SoundDropTargetDT4()
	PlaySoundAtLevelStatic (Cartridge_Targets & "_Drop_Target_3Bank_Release_Down_" & Int(Rnd*6)+1), Vol(ActiveBall) * TargetSoundFactor, DT4
End Sub



'/////////////////////  DROP TARGET RESET SOLENOID SOUNDS  //////////////////////
Sub RandomSoundDropTargetTop(object)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Targets & "_Drop_Target_1Bank_Reset_Up_" & Int(Rnd*8)+1,DOFContactors), Solenoid_DropTargetReset_SoundLevel, object
End Sub

Sub RandomSoundDropTargetLeft(object)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Targets & "_Drop_Target_3Bank_Reset_Up_" & Int(Rnd*6)+1,DOFContactors), Solenoid_DropTargetReset_SoundLevel, object
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub



'///////////////////////////////////  SPINNER  //////////////////////////////////
Sub SoundSpinner(object)
	PlaySoundAtLevelStatic (Cartridge_Spinner & "_Spinner_Spin_Loop"), SpinnerSoundLevel, object
End Sub


'//////////////////////////  STADNING TARGET HIT SOUNDS  ////////////////////////
Sub RandomSoundTargetHit()
	If BallSpeed(ActiveBall) > 10 Then
		RandomSoundTargetHitStrong
	Else
		RandomSoundTargetHitWeak
	End If
End Sub

Sub RandomSoundTargetHitStrong()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_5",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_6",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_7",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_8",DOFTargets), Vol(ActiveBall) * TargetSoundFactor		
	End Select
End Sub

Sub RandomSoundTargetHitWeak()
	Select Case Int(Rnd*4)+1		
		Case 1 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_1",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_2",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_3",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_4",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
	End Select
End Sub



'/////////////////////////////  BALL BOUNCE SOUNDS  /////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_2"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_12"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 3 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_14"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 4 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_18"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 5 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_19"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_20"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 7 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_21"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	Select Case Int(Rnd*12)+1
		Case 1 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_1"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 2 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_3"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 3 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_7"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 4 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_8"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 5 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_9"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 6 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_11"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 7 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_13"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 8 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_15"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 9 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_16"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 10 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_17"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 11 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_22"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 12 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_23"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
	End Select
End Sub

'/////////////////////  RAMPS BALL DROP TO PLAYFIELD SOUNDS  ////////////////////
'/////////  METAL WIRE - RIGHT RAMP - EXIT HOLE TO PLAYFIELD - SOUNDS  //////////
Sub RandomSoundLeftRampDropToPlayfield()
	PlaySoundAtLevelStatic (Cartridge_Metal_Ramps & "_Ramp_Right_Metal_Wire_Drop_to_Playfield_" & Int(Rnd*2)+1), RightRampMetalWireDropToPlayfieldSoundLevel, RHelper3
End Sub


''//////////////  PLASTIC - LEFT RAMP - EXIT HOLE TO LOCK - SOUND  ///////////////
'Sub RandomSoundRightRampLeftExitDropToLock()
'	PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Drop_to_Lock_" & Int(Rnd*4)+1), LeftPlasticRampDropToLockSoundLevel, RHelper2
'End Sub


'////////////  PLASTIC - LEFT RAMP - EXIT HOLE TO PLAYFIELD - SOUND  ////////////
Sub RandomSoundRightRampRightExitDropToPlayfield()
	PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Drop_to_Playfield_" & Int(Rnd*2)+1), LeftPlasticRampDropToPlayfieldSoundLevel, BallDrop2
End Sub


'//////////////////  METAL WIRE RIGHT RAMP - 2ND PART RAMP SUB  /////////////////
'Sub SoundRightPlasticRampPart2(toggle, ballvariablePlasticRampTimer1)
'	Set ballvariablePlasticRampTimer1 = ActiveBall
'	Select Case toggle
'		Case SoundOn
'			PlasticRampTimer1.Interval = 10
'			PlasticRampTimer1.Enabled = 1
'		Case SoundOff
'			PlasticRampTimer1.Enabled = 0
'			StopSound Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_BallRoll_" & b
'	End Select
'End Sub


'////////////////////////////  RAMP ENTRANCE EVENTS  ////////////////////////////
'/////////////////////////  RIGHT RAMP ENTRANCE SOUNDS  /////////////////////////

Sub LRAMPEnter_Hit()
	' Play the right plastic ramp entrance lifter/down sound for each ball
	If BallVel(ActiveBall) > 1 and ActiveBall.VelY > 0 Then
'		debug.print "Ball rolls downwards"
		PlaySoundAtLevelTimerExistingActiveBall (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Ramp_RollBack_" & Int(Rnd*2)+1), RightPlasticRampEnteranceSoundLevel, ActiveBall
	End If
	If BallVel(ActiveBall) > 1 and ActiveBall.VelY < 0 Then
'		debug.print "Ball rolls upwards"
		PlaySoundAtLevelTimerExistingActiveBall (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Ramp_Enter_" & Int(Rnd*4)+1), RightPlasticRampEnteranceSoundLevel, ActiveBall
	End If
End Sub


'//////////////////////////  LEFT RAMP ENTRANCE SOUNDS  /////////////////////////

Sub RRAMPEnter_Hit()
	' Play the left ramp plastic ramp entrance for each ball
	If BallVel(ActiveBall) > 1 and ActiveBall.VelY > 0 Then
'		debug.print "Ball rolls downwards"
		PlaySoundAtLevelTimerExistingActiveBall (Cartridge_Plastic_Ramps & "_Ramp_Left_Plastic_RollBack_" & Int(Rnd*2)+1), LeftPlasticRampEnteranceSoundLevel, ActiveBall	
	End If
	If BallVel(ActiveBall) > 1 and ActiveBall.VelY < 0 Then
'		debug.print "Ball rolls upwards"
		PlaySoundAtLevelTimerExistingActiveBall (Cartridge_Plastic_Ramps & "_Ramp_Left_Plastic_Enter_" & Int(Rnd*4)+1), LeftPlasticRampEnteranceSoundLevel, ActiveBall	
	End If
End Sub


'////////////////////////////////////  DRAIN  ///////////////////////////////////
'///////////////////////////////  OUTHOLE SOUNDS  ///////////////////////////////
Sub RandomSoundOutholeHit()
	PlaySoundAtLevelStatic (Cartridge_Trough & "_Outhole_Drain_Hit_" & Int(Rnd*4)+1), Solenoid_OutholeKicker_SoundLevel, Drain
End Sub

Sub RandomSoundOutholeKicker()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Trough & "_Outhole_Kicker_" & Int(Rnd*4)+1,DOFContactors), Solenoid_OutholeKicker_SoundLevel, Drain
End Sub

'/////////////////////  BALL SHOOTER FEEDER SOLENOID SOUNDS  ////////////////////
Sub RandomSoundShooterFeeder(object)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Trough & "_Shooter_Feeder_" & Int(Rnd*6)+1,DOFContactors), Solenoid_ShooterFeeder_SoundLevel, object
End Sub


'///////  SHOOTER LANE - BALL RELEASE ROLL IN SHOOTER LANE SOUND - SOUND  ///////
Sub SoundBallReleaseShooterLane(toggle)
	Select Case toggle
		Case SoundOn
			PlaySoundAtLevelActiveBall (Cartridge_Table_Specifics & "_Ball_Launch_from_Shooter_Lane"), BallReleaseShooterLaneSoundLevel
		Case SoundOff
			StopSound Cartridge_Table_Specifics & "_Ball_Launch_from_Shooter_Lane"
	End Select
End Sub

'////////////////////////////  AUTO-PLUNGER SOUNDS  /////////////////////////////
Sub RandomSoundAutoPlunger(object)
		PlaySoundAtLevelStatic SoundFX("SY_TNA_REV01_Auto_Launch_Coil_" & Int(Rnd*5)+1, DOFContactors), AutoPlungerSoundLevel, object
End Sub

'//////////////////////////////  KNOCKER SOLENOID  //////////////////////////////
Sub KnockerSolenoid(enabled)
	'PlaySoundAtLevelStatic SoundFX(Cartridge_Knocker & "_Knocker_Coil",DOFKnocker), Solenoid_Knocker_SoundLevel, KnockerPosition
	if Enabled then
		PlaySound SoundFX(Cartridge_Knocker & "_Knocker_Coil",DOFKnocker), 0, Solenoid_Knocker_SoundLevel
	end if
End Sub


'/////////////////////////////  EJECT HOLD SOLENOID  ////////////////////////////
Sub RandomSoundEjectHoleSolenoid(object)
	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Eject_Kickout_" & Int(Rnd*8)+1), Solenoid_TopEject_SoundLevel, object
End Sub


'///////////////////////////////  EJECT BALL BUMP  //////////////////////////////
Sub RandomSoundEjectBallBump(object)
	PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_After_Eject_" & Int(Rnd*3)+1), EjectBallBumpSoundLevel, object
End Sub


'///////////////////////////  HEAD SAUCER BALL ENTER  ////////////////////////////
Sub RandomSoundHeadSaucer(object)
	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Eject_Enter_" & Int(Rnd*2)+1), HeadSaucerSoundLevel, object
End Sub

'///////////////////////////  EJECT HOLD BALL ENTER  ////////////////////////////
Sub RandomSoundEjectHoleEnter(object)
	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Eject_Enter_" & Int(Rnd*2)+1), EjectHoleEnterSoundLevel, object
End Sub


'///////////////////////////  LOCKING KICKER SOLENOID  //////////////////////////
Sub RandomSoundLockingKickerSolenoid(object)
	PlaySoundAtLevelStatic (Cartridge_Kickers & "_Locking_Kickback_" & Int(Rnd*4)+1), Solenoid_LeftLockingKickback_SoundLevel, object
End Sub


'//////////////////////////  SLINGSHOT SOLENOID SOUNDS  /////////////////////////
Sub RandomSoundSlingshotLeft(object)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Slingshots & "_Slingshot_Left_" & Int(Rnd*26)+1,DOFContactors), Solenoid_Slingshot_SoundLevel, object
End Sub

Sub RandomSoundSlingshotRight(object)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Slingshots & "_Slingshot_Right_" & Int(Rnd*25)+1,DOFContactors), Solenoid_Slingshot_SoundLevel, object
End Sub

Sub RandomSoundSlingshotTopRight(object) 'todo
	PlaySoundAtLevelStatic SoundFX(Cartridge_Slingshots & "_Slingshot_Right_" & Int(Rnd*25)+1,DOFContactors), Solenoid_Slingshot_SoundLevel, object
End Sub

'///////////////////////////  BUMPER SOLENOID SOUNDS  ///////////////////////////
'////////////////////////////////  BUMPERS - TOP  ///////////////////////////////
Sub RandomSoundBumperLeft(object)
'	Debug.Print Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier
	PlaySoundAtLevelStatic SoundFX(Cartridge_Bumpers & "_Jet_Bumper_Left_" & Int(Rnd*22)+1,DOFContactors), Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier, object
End Sub

Sub RandomSoundBumperUp(object)
	'Debug.Print Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier
	PlaySoundAtLevelStatic SoundFX(Cartridge_Bumpers & "_Jet_Bumper_Up_" & Int(Rnd*25)+1,DOFContactors), Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier, object
End Sub

Sub RandomSoundBumperLow(object)
	'Debug.Print Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier
	PlaySoundAtLevelStatic SoundFX(Cartridge_Bumpers & "_Jet_Bumper_Low_" & Int(Rnd*28)+1,DOFContactors), Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier, object
End Sub



'///////////////////////  FLIPPER BATS SOUND SUBROUTINES  ///////////////////////
'//////////////////////  FLIPPER BATS SOLENOID CORE SOUND  //////////////////////
Sub RandomSoundFlipperLowerLeftUpFullStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Full_Stroke_" & RndInt(1,10),DOFFlippers), FlipperLeftLowerHitParm, Flipper
End Sub
'
'Sub RandomSoundFlipperUpperLeftUpFullStroke(flipper)
'	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Upper_Left_Up_Full_Stroke_" & Int(Rnd*5)+1,DOFFlippers), FlipperLeftUpperHitParm, Flipper
'End Sub

Sub RandomSoundFlipperLowerLeftUpDampenedStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Dampened_Stroke_" & RndInt(1,23),DOFFlippers), FlipperLeftLowerHitParm * 1.2, Flipper
End Sub

Sub RandomSoundFlipperLowerRightUpFullStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Full_Stroke_" & RndInt(1,11),DOFFlippers), FlipperRightLowerHitParm, Flipper
End Sub

Sub RandomSoundFlipperLowerRightUpDampenedStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Dampened_Stroke_" & RndInt(1,23),DOFFlippers), FlipperLeftLowerHitParm * 1.2, Flipper
End Sub

Sub RandomSoundFlipperLowerLeftReflip(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Reflip_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

'Sub RandomSoundFlipperUpperLeftReflip(flipper)
'	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Upper_Left_Reflip",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
'End Sub

Sub RandomSoundFlipperLowerRightReflip(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Reflip_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperLowerLeftDown(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Down_" & RndInt(1,10),DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub
'
'Sub RandomSoundFlipperUpperLeftDown(flipper)
'	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Upper_Left_Down_" & Int(Rnd*6)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
'End Sub

Sub RandomSoundFlipperLowerRightDown(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Down_" & RndInt(1,11),DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundLowerLeftQuickFlipUp()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_QuickFlip_Up_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, LeftFlipper
End Sub

Sub RandomSoundLowerRightQuickFlipUp()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_QuickFlip_Up_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, RightFlipper
End Sub

Sub StopAnyFlipperLowerLeftUp()
	Dim anyFullStrokeSound
	Dim anyDampenedStrokeSound
	For anyFullStrokeSound = 1 to 10
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Full_Stroke_" & anyFullStrokeSound)
	Next
	For anyDampenedStrokeSound = 1 to 23
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Dampened_Stroke_" & anyDampenedStrokeSound)
	Next
End Sub

Sub StopAnyFlipperLowerRightUp()
	Dim anyFullStrokeSound
	Dim anyDampenedStrokeSound
	For anyFullStrokeSound = 1 to 11
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Full_Stroke_" & anyFullStrokeSound)
	Next
	For anyDampenedStrokeSound = 1 to 23
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Dampened_Stroke_" & anyDampenedStrokeSound)
	Next
End Sub

Sub StopAnyFlipperLowerLeftDown()
	Dim anyFullDownSound
	For anyFullDownSound = 1 to 10
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Down_" & anyFullDownSound)
	Next
End Sub

Sub StopAnyFlipperLowerRightDown()
	Dim anyFullDownSound
	For anyFullDownSound = 1 to 10
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Right_Down_" & anyFullDownSound)
	Next
End Sub


Sub FlipperHoldCoilLeft(toggle, flipper)
	Select Case toggle
		Case SoundOn
			PlaySoundAtLevelExistingStaticLoop (Cartridge_Flippers & "_Flipper_Hold_Coil_Low_Frequencies_Loop_Lower_Left"), FlipperHoldSoundLevel, flipper
			PlaySoundAtLevelExistingStaticLoop (Cartridge_Flippers & "_Flipper_Hold_Coil_Med_Frequencies_Loop_Lower_Left"), FlipperHoldSoundLevel, flipper
		Case SoundOff
			StopSound Cartridge_Flippers & "_Flipper_Hold_Coil_Low_Frequencies_Loop_Lower_Left"
			StopSound Cartridge_Flippers & "_Flipper_Hold_Coil_Med_Frequencies_Loop_Lower_Left"
	End Select
End Sub

Sub FlipperHoldCoilRight(toggle, flipper)
	Select Case toggle
		Case SoundOn
			PlaySoundAtLevelExistingStaticLoop (Cartridge_Flippers & "_Flipper_Hold_Coil_Low_Frequencies_Loop_Lower_Right"), FlipperHoldSoundLevel, flipper
			PlaySoundAtLevelExistingStaticLoop (Cartridge_Flippers & "_Flipper_Hold_Coil_Med_Frequencies_Loop_Lower_Right"), FlipperHoldSoundLevel, flipper
		Case SoundOff
			StopSound Cartridge_Flippers & "_Flipper_Hold_Coil_Low_Frequencies_Loop_Lower_Right"
			StopSound Cartridge_Flippers & "_Flipper_Hold_Coil_Med_Frequencies_Loop_Lower_Right"
	End Select
End Sub

'
'
'Sub RandomSoundFlipperLowerLeftUpFullStrokeDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Full_Stroke_" & Int(Rnd*6)+1, DOFevent, DOFstate, DOFFlippers), FlipperLeftLowerHitParm, Flipper
'End Sub
'
'Sub RandomSoundFlipperUpperLeftUpFullStrokeDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Upper_Left_Up_Full_Stroke_" & Int(Rnd*5)+1, DOFevent, DOFstate, DOFFlippers), FlipperLeftUpperHitParm, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerLeftUpDampenedStrokeDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Dampened_Stroke_" & Int(Rnd*8)+1, DOFevent, DOFstate, DOFFlippers), FlipperLeftLowerHitParm * 1.2, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerRightUpFullStrokeDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Full_Stroke_" & Int(Rnd*5)+1, DOFevent, DOFstate, DOFFlippers), FlipperRightLowerHitParm, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerRightUpDampenedStrokeDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Dampened_Stroke_" & Int(Rnd*8)+1, DOFevent, DOFstate, DOFFlippers), FlipperRightLowerHitParm * 1.2, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerLeftReflipDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Left_Reflip", DOFevent, DOFstate, DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
'End Sub
'
'Sub RandomSoundFlipperUpperLeftReflipDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Upper_Left_Reflip", DOFevent, DOFstate, DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerRightReflipDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Right_Reflip", DOFevent, DOFstate, DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerLeftDownDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Left_Down_" & Int(Rnd*7)+1, DOFevent, DOFstate, DOFFlippers), FlipperDownSoundLevel, Flipper
'End Sub
'
'Sub RandomSoundFlipperUpperLeftDownDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Upper_Left_Down_" & Int(Rnd*6)+1, DOFevent, DOFstate, DOFFlippers), FlipperDownSoundLevel, Flipper
'End Sub
'
'Sub RandomSoundFlipperLowerRightDownDOF(flipper, DOFevent, DOFstate)
'	PlaySoundAtLevelStatic SoundFXDOF(Cartridge_Flippers & "_Flipper_Lower_Right_Down_" & Int(Rnd*7)+1, DOFevent, DOFstate, DOFFlippers), FlipperDownSoundLevel, Flipper
'End Sub


'///////////////////////  FLIPPER BATS BALL COLLIDE SOUND  //////////////////////
dim angdamp, veldamp
angdamp = 0.2
veldamp = 0.8

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm

	If parm => 22 Then
		' Strong hit safe values boundary
		' Flipper stroke dampened
		FlipperLeftLowerHitParm = FlipperUpSoundLevel * 0.1
	Else
		If parm =< 1 Then 
			' Weak hit safe values boundary
			' Flipper stroke full
			FlipperLeftLowerHitParm = FlipperUpSoundLevel
		Else
			' Fully modulated hit
			FlipperLeftLowerHitParm = FlipperUpSoundLevel * (1-(parm/25))
		End If
	End If

	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm

	If parm => 22 Then
		' Strong hit safe values boundary
		' Flipper stroke dampened
		FlipperRightLowerHitParm = FlipperUpSoundLevel * 0.1
	Else
		If parm =< 1 Then 
			' Weak hit safe values boundary
			' Flipper stroke full
			FlipperRightLowerHitParm = FlipperUpSoundLevel
		Else
			' Fully modulated hit
			FlipperRightLowerHitParm = FlipperUpSoundLevel * (1-(parm/25))
		End If
	End If

 	RandomSoundRubberFlipper(parm)
End Sub



Sub LeftFlipper2_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper2, LFCount2, parm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipper2_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper2, RFCount2, parm
	RandomSoundRubberFlipper(parm)
End Sub

Sub DoorF_collide(parm)
	RandomSoundWall

	PlaySound "sfx_doorhit",1,VolumeDial
End Sub


Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Flipper_Hit_" & Int(Rnd*7)+1), parm / 25 * RubberFlipperSoundFactor
End Sub


'
'
''///////////////////////  RAMP DIVERTER SOLENOID - DIVERT  //////////////////////
'Sub RandomSoundRampDiverterDivert()
'	PlaySoundAtLevelStatic (Cartridge_Diverters & "_Diverter_Divert_" & Int(Rnd*4)+1), Solenoid_Diverter_Enabled_SoundLevel, DiverterPosition
'End Sub
'
''////////////////////////  RAMP DIVERTER SOLENOID - BACK  ///////////////////////
'Sub RandomSoundRampDiverterBack()
'	PlaySoundAtLevelStatic (Cartridge_Diverters & "_Diverter_Back_" & Int(Rnd*4)+1), Solenoid_Diverter_Disabled_SoundLevel, DiverterPosition
'End Sub
'
''///////////////////////  RAMP DIVERTER SOLENOID - DIVERT  //////////////////////
'Sub RandomSoundGateDivert()
'	PlaySoundAtLevelStatic (Cartridge_Diverters & "_Diverter_Divert_" & Int(Rnd*4)+1), Solenoid_Diverter_Enabled_SoundLevel * 0.05, Gate5
'End Sub
'
''////////////////////////  RAMP DIVERTER SOLENOID - BACK  ///////////////////////
'Sub RandomSoundGateBack()
'	PlaySoundAtLevelStatic (Cartridge_Diverters & "_Diverter_Back_" & Int(Rnd*4)+1), Solenoid_Diverter_Disabled_SoundLevel * 0.05, Gate5
'End Sub
'
''////////////////////  RAMP DIVERTER SOLENOID - MAGNET SOUND  ///////////////////
'Sub RandomSoundRampDiverterHold(toggle)
'	Select Case toggle	
'		Case SoundOn
'			PlaySoundAtLevelStaticLoop SoundFX(Cartridge_Diverters & "_Diverter_Hold_Loop",DOFShaker), Solenoid_Diverter_Hold_SoundLevel, DiverterPosition
'		Case SoundOff
'			StopSound Cartridge_Diverters & "_Diverter_Hold_Loop"
'	End Select
'End Sub


'
''//////////////////////////  SOLENOID A/C SELECT RELAY  /////////////////////////
'Sub Sound_Solenoid_AC(toggle)
'	Select Case toggle
'		Case CircuitA
'			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_A"), RelaySolenoidACSelectSoundLevel, GIUpperPosition
'			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_A"), RelaySolenoidACSelectSoundLevel, ACSelectPosition
'		Case CircuitC
'			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_C"), RelaySolenoidACSelectSoundLevel, GIUpperPosition
'			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_C"), RelaySolenoidACSelectSoundLevel, ACSelectPosition
'	End Select
'End Sub
'
'
''//////////////////////////  GENERAL ILLUMINATION RELAYS  ///////////////////////
'Sub Sound_LowerGI_Relay(toggle)
'	Select Case toggle
'		Case SoundOn
'			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_On"), RelayLowerGISoundLevel, GIUpperPosition
'			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_On"), RelayLowerGISoundLevel, GIUpperPosition
'		Case SoundOff
'			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_Off"), RelayLowerGISoundLevel, GIUpperPosition
'			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_Off"), RelayLowerGISoundLevel, GIUpperPosition
'	End Select
'End Sub
'
'Sub Sound_UpperGI_Relay(toggle)
'	Select Case toggle
'		Case SoundOn
'			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_On"), RelayUpperGISoundLevel, GIUpperPosition
'			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_On"), RelayUpperGISoundLevel, GILowerPosition
'		Case SoundOff
'			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_Off"), RelayUpperGISoundLevel, GIUpperPosition
'			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_Off"), RelayUpperGISoundLevel, GILowerPosition
'	End Select
'End Sub
'
'
''///////////////////////////////  FLASHERS RELAY  ///////////////////////////////
'Sub Sound_Flasher_Relay(toggle, tableobj)
'	Select Case toggle
'		Case SoundOn
'			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_On"), RelayFlasherSoundLevel, GIUpperPosition
'			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_On"), RelayFlasherSoundLevel, tableobj
'		Case SoundOff
'			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_Off"), RelayFlasherSoundLevel, GIUpperPosition
'			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_Off"), RelayFlasherSoundLevel, tableobj
'	End Select
'End Sub



'////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  /////////////////////
'/////////////////////////////  RUBBERS AND POSTS  //////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ///////////////////////////////
Sub HitsRubbers_Hit(idx)
'	debug.print "rubber hit"
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
 		RandomSoundRubberStrong() 
	End if
	If finalspeed <= 10 then
 		RandomSoundRubberWeak()
 	End If	
End Sub


'/////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  /////////////////////
Sub RandomSoundRubberStrong()
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_10"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub


'///////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  /////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_" & Int(Rnd*8)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub


'///////////////////////////////  WALL IMPACTS  /////////////////////////////////
Sub RandomSoundWall()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor * 0.05
			Case 4 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall (Cartridge_Metal_Hits & "_Metal_Alternative_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub


'/////////////////////////////  WALL IMPACTS EVENTS  ////////////////////////////
'Sub Wall27_Hit()
'	RandomSoundMetal()
'End Sub
'
'Sub Wall13_Hit()
'	RandomSoundMetal()
'End Sub

sub HitsMetal_Hit(IDX)
'	debug.print "metal hit"
	RandomSoundMetal()
end sub

'
''////////////////////////////  INNER LEFT LANE WALLS  ///////////////////////////
'Sub Wall226_Hit()
'	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
'End Sub
'
'Sub Wall225_Hit()
'	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
'End Sub
'
'
''right arch
'Sub Wall141_Hit()
'	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
'End Sub
'
'Sub Wall132_Hit()
'	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
'End Sub
'
''right arch wire
''Sub Wall254_Hit()
''	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
''End Sub
'
'Sub Wall224_Hit()
'	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Left_Wall_" & Int(Rnd*3)+1), Vol(ActiveBall) * MetalImpactSoundFactor
'End Sub


'/////////////////////////////  METAL TOUCH SOUNDS  /////////////////////////////
Sub RandomSoundMetal()
	Select Case Int(Rnd*20)+1
		Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_1"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_2"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_3"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_4"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_5"), Vol(ActiveBall) * 0.02 * MetalImpactSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_6"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_7"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_8"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_9"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_10"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 11 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_11"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 12 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_12"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 13 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_13"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 14 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_14"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 15 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_15"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 16 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_16"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 17 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_17"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 18 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_18"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 19 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_19"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 20 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_20"), Vol(ActiveBall) * MetalImpactSoundFactor
	End Select
End Sub


'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub HitsWoods_Hit(idx)

'	debug.print "wood hit"
	RandomSoundWood()      
End Sub

Sub RandomSoundWood()
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

'///////////////////////////////  OUTLANES WALLS  ///////////////////////////////
Sub RandomSoundOutlaneWalls()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Outlane_Wall_" & Int(Rnd*9)+1), OutlaneWallsSoundFactor
End Sub


'///////////////////////////  BOTTOM ARCH BALL GUIDE  ///////////////////////////
'///////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////
Sub RandomSoundBottomArchBallGuideSoftHit()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Arch_Ball_Guide_Hit_Soft_" & Int(Rnd*4)+1), BottomArchBallGuideSoundFactor
End Sub


'//////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Arch_Ball_Guide_Hit_Hard_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 3
End Sub


'//////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
 	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End if
End Sub

Sub BallGuide_Hit
	RandomSoundFlipperBallGuide()
End Sub



'/////////////////////////  WIREFORM ANTI-REBOUNT RAILS  ////////////////////////
Sub RandomSoundWireformAntiRebountRail()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed >= 10 then 
 		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_3"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_4"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_5"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 4 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_6"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 5 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_7"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
		End Select
	End if
	If finalspeed < 10 Then
 		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_1"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_2"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Wireform_Anti_Rebound_Rail_8"),  Vol(ActiveBall) * WireformAntiRebountRailSoundFactor
		End Select
	End if
End Sub


'////////////////////////////  LANES AND INNER LOOPS  ///////////////////////////
'////////////////////  INNER LOOPS - LEFT ENTRANCE - EVENTS  ////////////////////
Sub LeftInnerLaneTriggerUp_Hit()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
		If ActiveBall.VelY < 0 Then RandomSoundInnerLaneEnter()
	End If
End Sub

Sub LeftInnerLaneTriggerDown_Hit()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 7 then
		If ActiveBall.VelY > 0 Then RandomSoundInnerLaneEnter()
	End If
End Sub


'/////////////////////  INNER LOOPS - LEFT ENTRANCE - SOUNDS  ///////////////////
Sub RandomSoundInnerLaneEnter()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_InnerLane_Ball_Guide_Hit_" & Int(Rnd*20)+1), Vol(ActiveBall) * InnerLaneSoundFactor
End Sub


'//////////////////////////////  LEFT LANE ENTRANCE  ////////////////////////////
'/////////////////////////  LEFT LANE ENTRANCE - EVENTS  ////////////////////////
Sub LeftLaneTrigger_Hit()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
		If ActiveBall.VelY < 0 Then PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Lane_Left_Ball_Enter_Hit"), Vol(ActiveBall) * LaneEnterSoundFactor : RandomSoundLaneLeftEnter()
	End If
End Sub


'/////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////
Sub RandomSoundLaneLeftEnter()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Lane_Left_Ball_Roll_" & Int(Rnd*2)+1), Vol(ActiveBall) * LaneSoundFactor
End Sub


'/////////////////////////////  RIGHT LANE ENTRANCE  ////////////////////////////
'////////////////////////  RIGHT LANE ENTRANCE - EVENTS  ////////////////////////
Sub RightLaneTrigger_Hit()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
		If ActiveBall.VelY < 0 Then PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Lane_Right_Ball_Enter_Hit"), Vol(ActiveBall) * LaneEnterSoundFactor : RandomSoundLaneRightEnter()
	End If
End Sub


'/////////////////  RIGHT LANE ENTRANCE (RIGHT ORBIT) - SOUNDS  /////////////////
Sub RandomSoundLaneRightEnter()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Lane_Right_Ball_Roll_" & Int(Rnd*3)+1), Vol(ActiveBall) * LaneSoundFactor
End Sub


'/////////////////////////  RAMP HELPERS BALL VARIABLES  ////////////////////////
'Dim ballvariablePlasticRampTimer1


'/////////  PLASTIC LEFT RAMP - RIGHT EXIT HOLE - TO PLAYFIELD - EVENT  /////////
Sub BallDrop2_Hit()
	if abs(activeball.AngMomZ) > 70 then activeball.AngMomZ = 70
	activeball.AngMomZ = abs(activeball.AngMomZ) * 3
	RandomSoundRightRampRightExitDropToPlayfield()
'	Call SoundRightPlasticRampPart2(SoundOff, ballvariablePlasticRampTimer1)
End Sub


'/////////  METAL WIRE RIGHT RAMP - EXIT HOLE - TO PLAYFIELD - EVENT  //////////
Sub RHelper3_Hit()
	if abs(activeball.AngMomZ) > 70 then activeball.AngMomZ = 70
	activeball.AngMomZ = -abs(activeball.AngMomZ) * 3
	RandomSoundLeftRampDropToPlayfield()
End Sub


'//////////////////  PLASTIC LEFT RAMP - PART 2 SOUND - EVENT  //////////////////
'Sub RHelper4_Hit()
'	Call SoundRightPlasticRampPart2(SoundOn, ballvariablePlasticRampTimer1)
'End Sub

'////////////////////////////////////////////////////////////////////////////////
'////          End of Fleep Mechanical Sounds Section                        ////
'////////////////////////////////////////////////////////////////////////////////

'
''***************************************************************
''Table MISC VP sounds
''***************************************************************


Sub Trigger1_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), RightPlasticRampHitsSoundLevel, Trigger1 : End Sub
Sub Trigger2_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), RightPlasticRampHitsSoundLevel, Trigger2 : End Sub
Sub Trigger3_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_5"), RightPlasticRampHitsSoundLevel, Trigger3 : End Sub
Sub Trigger4_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), RightPlasticRampHitsSoundLevel, Trigger4 : End Sub
Sub Trigger5_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), RightPlasticRampHitsSoundLevel, Trigger5 : End Sub
Sub Trigger6_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_5"), RightPlasticRampHitsSoundLevel, Trigger6 : End Sub
Sub Trigger7_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), RightPlasticRampHitsSoundLevel, Trigger7 : End Sub
Sub Trigger8_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), RightPlasticRampHitsSoundLevel, Trigger8 : End Sub
Sub Trigger9_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_5"), RightPlasticRampHitsSoundLevel, Trigger9 : End Sub
Sub Trigger10_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), RightPlasticRampHitsSoundLevel, Trigger10 : End Sub
Sub Trigger11_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), RightPlasticRampHitsSoundLevel, Trigger11 : End Sub
Sub Trigger13_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_5"), RightPlasticRampHitsSoundLevel, Trigger13 : End Sub
Sub Trigger14_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), RightPlasticRampHitsSoundLevel, Trigger14 : End Sub
Sub Trigger15_Hit():PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), RightPlasticRampHitsSoundLevel, Trigger15 : End Sub

dim FaceGuideHitsSoundLevel : FaceGuideHitsSoundLevel = 0.002 * RightPlasticRampHitsSoundLevel
Sub ColFaceGuideL1_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_2"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub F1Guide_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_1"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub F1Guide2_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_1"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub ColFaceGuideL2_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_2"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub ColFaceGuideL3_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), FaceGuideHitsSoundLevel, activeball : End Sub



'**********************************************************************************************************
'* TilT *
'**********************************************************************************************************

Dim Danger : Danger = 0
Sub Tilting    'Nudge comes here



	If Tilted Then Exit Sub
	If BallOver Then Exit Sub


		If Tilt > 115 Then 
			TiltGame 
			Exit Sub
		End If

		Tilt = Tilt + TiltSens  

		If vrroom = 0 and DesktopMode Then Light007.state = 2 : Light008.state = 2
		If b2son then bbs 4,0,9999,11

		If Tilt > TiltSens Then 
			Danger = Danger + 1
			If Danger = 3 Then TiltGame : Exit Sub
			ClearVoiceQ
			Select Case Int(rnd(1)*4)
				case 0 : PlayVoice "vo_shakeearthquake",1,Voice_Volume,0
				case 1 : PlayVoice "vo_shakethingsup"	,1,Voice_Volume	,0
				case 2 : PlayVoice "vo_shakeshutdown",1,Voice_Volume,0
				case 3 : PlayVoice "vo_shakeregret",1,Voice_Volume,0
			End Select
			PlaySound "danger", 1, VolumeDial

		End if

End Sub


Sub TiltGame
	stopsound "Volkan_Overture_II"
	stopsound "sfx_multiballbeat"

	If vrroom = 0 and DesktopMode Then Light007.state = 2 : Light008.state = 2
	If b2son then bbs 4,0,9999,11

	'fixing   cab and vr tilt lights
	
	gioff 

	Howmuch = 0
	ballsave = -3


	LightSeqAttract.stopplay
	LightSeqAttract.Play seqalloff

	ClearVoiceQ
	Select Case Int(rnd(1)*4)
		case 0 : PlayVoice "vo_tiltdisgrace",1,Voice_Volume,0
		case 1 : PlayVoice "vo_tiltmakemoney"	,1,Voice_Volume	,0
		case 2 : PlayVoice "vo_tiltfault",1,Voice_Volume,0
		case 3 : PlayVoice "vo_tilthurtprofits",1,Voice_Volume,0
	End Select
	StopSound "danger"
	PlaySound "Tilt",1,VolumeDial

	Tilted = True
'	debug.print"Tiltgame:   Tilted = True"
	BallOver = True
'	debug.print "Tiltgame:   Ballover = " & BallOver

	multiballtimer.enabled = False
	multiballtimer2.enabled = False

	UpPostRight.z = -40
	UpPostLeft.z = -40
	wall003.collidable = False
	wall004.collidable = False


	tiltedwait.enabled=True ' .. wait for no balls in play ( excluded the ones in lockup )
	BallsToLaunch = 0
	Tilt = 333
	Leftflipper.RotateTOStart
	Rightflipper.RotateTOStart

End Sub

Sub TiltedWait_Timer
	If tilt=0 Then
		If BIP = 0 Then
			danger = 0
			LightSeqAttract.stopplay
			If vrroom = 0 and DesktopMode Then Light007.state = 0 : Light008.state = 0
			If b2son then bbs 4,0,0,0
			'fixing vr tilt lights
	
			Tilted = False
'			debug.print"Tilted = False"
			tiltedwait.enabled=False 

			balltime = 1000
			bls 128,117,7,4,4,0
			Howmuch = 0
			Hurryup_Active = 0
			HurryupTimerBumpers.Enabled = False
			Blink(15,1)=0
			Blink(117,1)=0
			Blink(118,1)=0
			Blink(119,1)=0
			Blink(120,1)=0

			StopSound "sfx_clocktimer"
			EOB_Ticks = 0
			EndOfBallBonus.enabled = True
			AutoPlunger = False
			GaugeLeftPos = 42
			GaugeRightPos = 47
			TweedControl = 0
			BaronControl = 0
			Blink(113,1)=0
			If CurrentPlayer = 1 then
				Playercontrol = "Tweed"
			Else
				Playercontrol = "Baron"
			End If
			UpdateControl
			gion
		End If
	End If
End Sub



'*********************
'VR Mode
'*********************
DIM VRThings

If VRRoom > 0 Then
	Setbackglass
	vrbackglasstimer.enabled = true
	ClockTimer.enabled = true
	for each VRThings in VRCab:VRThings.visible = 1:Next
	If VRRoom = 1 Then
		for each VRThings in VRMin:VRThings.visible = 1:Next
	End If
Else
	vrbackglasstimer.enabled = false
	ClockTimer.enabled = false
	for each VRThings in VRMin:VRThings.visible = 0:Next
	for each VRThings in VRCab:VRThings.visible = 0:Next
	VRBackglassDomeRed.visible = 0
	VRBackglassDomeWhite.visible = 0
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
	for each VRThings in VRBackglassDMD:VRThings.visible = 0:Next
	for each VRThings in VRBackglassDMDBottom:VRThings.visible = 0:Next
	for each VRThings in VRBackglassDMDMid:VRThings.visible = 0:Next
	for each VRThings in VRBackglassDMDTop:VRThings.visible = 0:Next
End if


'******************************************************
'  VR Backglass
'******************************************************

Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglass
		obj.x = obj.x -3
		obj.height = - obj.y + 210
		obj.y = -80 'adjusts the distance from the backglass towards the user
		obj.rotx=-86.1
	Next

	For Each obj In VRBackglassDMD
		obj.x = obj.x -3
		obj.height = - obj.y + 210
		obj.y = 17 'adjusts the distance from the backglass towards the user
		obj.rotx=-83.5
	Next
	For Each obj In VRBackglassDMDTop
		obj.x = obj.x -3
		obj.height = - obj.y + 210
		obj.y = 3 'adjusts the distance from the backglass towards the user
		obj.rotx=-83.5
	Next
	For Each obj In VRBackglassDMDMid
		obj.x = obj.x -3
		obj.height = - obj.y + 210
		obj.y = 17 'adjusts the distance from the backglass towards the user
		obj.rotx=-83.5
	Next
	For Each obj In VRBackglassDMDBottom
		obj.x = obj.x -3
		obj.height = - obj.y + 210
		obj.y = 32 'adjusts the distance from the backglass towards the user
		obj.rotx=-83.5
	Next
End Sub



Dim BallSaveFlash, TweedFlash, BaronFlash
BallSaveFlash = 1
Sub vrbackglasstimer_Timer()

	'Player 1 Digits
	VRP1D0.ImageA="D"&Mid(Playerscore(1)+100000000,9,1)
	VRP1D1.ImageA="D"&Mid(Playerscore(1)+100000000,8,1)
	If len(PlayerScore(1)) < 3 then 
		VRP1D2.ImageA = "Blank"
	Else
		VRP1D2.ImageA="D"&Mid(Playerscore(1)+100000000,7,1)
	End If
	If len(PlayerScore(1)) < 4 then 
		VRP1D3.ImageA = "Blank"
	Else
		VRP1D3.ImageA="D"&Mid(Playerscore(1)+100000000,6,1)
	End If
	If len(PlayerScore(1)) < 5 then 
		VRP1D4.ImageA = "Blank"
	Else
		VRP1D4.ImageA="D"&Mid(Playerscore(1)+100000000,5,1)
	End If
	If len(PlayerScore(1)) < 6 then 
		VRP1D5.ImageA = "Blank"
	Else
		VRP1D5.ImageA="D"&Mid(Playerscore(1)+100000000,4,1)
	End If
	If len(PlayerScore(1)) < 7 then 
		VRP1D6.ImageA = "Blank"
	Else
		VRP1D6.ImageA="D"&Mid((Playerscore(1)+100000000),3,1)
	End If
	If len(PlayerScore(1)) < 8 then 
		VRP1D7.ImageA = "Blank"
	Else
		VRP1D7.ImageA="D"&Mid((Playerscore(1)+100000000),2,1)
	End If

	'Player 2 Digits
	VRP2D0.ImageA="D"&Mid(PlayerScore(2)+100000000,9,1)
	VRP2D1.ImageA="D"&Mid(PlayerScore(2)+100000000,8,1)
	If len(PlayerScore(2)) < 3 then 
		VRP2D2.ImageA = "Blank"
	Else
		VRP2D2.ImageA="D"&Mid(PlayerScore(2)+100000000,7,1)
	End If
	If len(PlayerScore(2)) < 4 then 
		VRP2D3.ImageA = "Blank"
	Else
		VRP2D3.ImageA="D"&Mid(PlayerScore(2)+100000000,6,1)
	End If
	If len(PlayerScore(2)) < 5 then 
		VRP2D4.ImageA = "Blank"
	Else
		VRP2D4.ImageA="D"&Mid(PlayerScore(2)+100000000,5,1)
	End If
	If len(PlayerScore(2)) < 6 then 
		VRP2D5.ImageA = "Blank"
	Else
		VRP2D5.ImageA="D"&Mid(PlayerScore(2)+100000000,4,1)
	End If
	If len(PlayerScore(2)) < 7 then 
		VRP2D6.ImageA = "Blank"
	Else
		VRP2D6.ImageA="D"&Mid((PlayerScore(2)+100000000),3,1)
	End If
	If len(PlayerScore(2)) < 8 then 
		VRP2D7.ImageA = "Blank"
	Else
		VRP2D7.ImageA="D"&Mid((PlayerScore(2)+100000000),2,1)
	End If


	VRBallInPlay.ImageA = "D"&BallInPlay
	VRCredits.imageA = "D"&credits
	If PlayersPlaying = 0 Then
		VRCanPlay.imageA = "D0"
	Else
		VRCanPlay.imageA = "D"&PlayersPlaying
	End If
	Dim x

	'Tweed
	If TalkingQ(x,0) = "Tweed_WeHaveABusinessToRun" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "Tweed_RobertIsTakingMyMoney" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "Tweed_RobertChallengesMy" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "Tweed_MyEveryMove" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "Tweed_IMustGetControlBack" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "Tweed_ICannotTrustThose" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "tweed_furnaceempty" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "tweed_imperative" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "tweed_keeprunning" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "tweed_keepup" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "tweed_needmore" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "tweed_onlypay" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "Tweed_ThatIsQuiteUnfortunate" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "Tweed_IFinallyHaveControlAgain" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "Tweed_WhatIsGoingOn" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "Tweed_ThereAreSpies" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "tweed_needmore" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	If TalkingQ(x,0) = "tweed_onlypay" then bbs 12, 1, 8, 5 : bbs 13, 1, 4, 10
	'Baron
	If TalkingQ(x,0) = "baron_TweedDoesntSuspect" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_HitTheBlueTargets" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_IMustHaveControl" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_StayAwayFromGreenTargets" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_TweedIsHoldingUsBack" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_TweedMustBeStopped" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_gravity" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_idemand" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_moreproduction" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_riskreward" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_thisismine" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_wayofprogress" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_YouFool" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_ImResponsibleForVolkan" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_VolkanWillBeMine" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_VeryGood" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_IWillGetControl" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_DontLetMeDown" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10	
	If TalkingQ(x,0) = "baron_IHaveFinallyWon" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	If TalkingQ(x,0) = "baron_FinallyHaveControl" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10	
	If TalkingQ(x,0) = "Baron_ImInControlNow" then bbs 14, 1, 8, 5 : bbs 15, 1, 4, 10
	'Abigail
	If TalkingQ(x,0) = "vo2_bestinterests" then bbs 16, 1, 8, 5
	If TalkingQ(x,0) = "vo2_edgeofmadness" then bbs 16, 1, 8, 5
	If TalkingQ(x,0) = "vo2_goingtohappen." then bbs 16, 1, 8, 5
	If TalkingQ(x,0) = "vo2_imafraid" then bbs 16, 1, 8, 5
	If TalkingQ(x,0) = "vo2_moreforVolkan" then bbs 16, 1, 8, 5
	If TalkingQ(x,0) = "vo2_notwell" then bbs 16, 1, 8, 5
	If TalkingQ(x,0) = "vo2_suspiciouseveryone" then bbs 16, 1, 8, 5
	If TalkingQ(x,0) = "vo2_themoney" then bbs 16, 1, 8, 5
	If TalkingQ(x,0) = "vo2_trustrobert" then bbs 16, 1, 8, 5
	If TalkingQ(x,0) = "vo2_unfounded" then bbs 16, 1, 8, 5
	If TalkingQ(x,0) = "vo2_whattodo" then bbs 16, 1, 8, 5
	If TalkingQ(x,0) = "baron_FinallyHaveControl" then bbs 16, 1, 8, 5	
	If TalkingQ(x,0) = "vo2_worriedbrother" then bbs 16, 1, 8, 5


End Sub


'**************************
'Backglass Lighting
'**************************
'Baron1
dim VRBGFLBaron1lvl
sub VRBGFLBaron1(nr)
	VRBGFLBaron1lvl = nr
	VRBGFLBaron1_1_timer
end sub

sub VRBGFLBaron1_1_timer()						
	if Not VRBGFLBaron1_1.TimerEnabled then
		VRBGFLBaron1_1.visible = true
		VRBGFLBaron1_2.visible = true
		VRBGFLBaron1_3.visible = true
		VRBGFLBaron1_4.visible = true
		VRBGFLBaron1_5.visible = true
		VRBGFLBaron1_1.TimerEnabled = true
	end if
		VRBGFLBaron1_1.opacity = 60 * VRBGFLBaron1lvl^1.5
		VRBGFLBaron1_2.opacity = 60 * VRBGFLBaron1lvl^1.5
		VRBGFLBaron1_3.opacity = 60 * VRBGFLBaron1lvl^1.5
		VRBGFLBaron1_4.opacity = 100 * VRBGFLBaron1lvl^2
		VRBGFLBaron1_5.opacity = 40 * VRBGFLBaron1lvl^3
	VRBGFLBaron1lvl = 0.80 * VRBGFLBaron1lvl - 0.01
	if VRBGFLBaron1lvl < 0 then VRBGFLBaron1lvl = 0
	if VRBGFLBaron1lvl =< 0 Then
		VRBGFLBaron1_1.visible = false
		VRBGFLBaron1_2.visible = false
		VRBGFLBaron1_3.visible = false
		VRBGFLBaron1_4.visible = false
		VRBGFLBaron1_5.visible = false
		VRBGFLBaron1_1.TimerEnabled = false
	end if
end sub

'Baron2
dim VRBGFLBaron2lvl
sub VRBGFLBaron2(nr)
	VRBGFLBaron2lvl = nr
	VRBGFLBaron2_1_timer
end sub

sub VRBGFLBaron2_1_timer()						
	if Not VRBGFLBaron2_1.TimerEnabled then
		VRBGFLBaron2_1.visible = true
		VRBGFLBaron2_2.visible = true
		VRBGFLBaron2_3.visible = true
		VRBGFLBaron2_4.visible = true
		VRBGFLBaron2_5.visible = true
		VRBGFLBaron2_1.TimerEnabled = true
	end if
		VRBGFLBaron2_1.opacity = 75 * VRBGFLBaron2lvl^1.5
		VRBGFLBaron2_2.opacity = 75 * VRBGFLBaron2lvl^1.5
		VRBGFLBaron2_3.opacity = 75 * VRBGFLBaron2lvl^1.5
		VRBGFLBaron2_4.opacity = 175 * VRBGFLBaron2lvl^2
		VRBGFLBaron2_5.opacity = 50 * VRBGFLBaron2lvl^3
	VRBGFLBaron2lvl = 0.80 * VRBGFLBaron2lvl - 0.01
	if VRBGFLBaron2lvl < 0 then VRBGFLBaron2lvl = 0
	if VRBGFLBaron2lvl =< 0 Then
		VRBGFLBaron2_1.visible = false
		VRBGFLBaron2_2.visible = false
		VRBGFLBaron2_3.visible = false
		VRBGFLBaron2_4.visible = false
		VRBGFLBaron2_5.visible = false
		VRBGFLBaron2_1.TimerEnabled = false
	end if
end sub

'Tweed1
dim VRBGFLTweed1lvl
sub VRBGFLTweed1(nr)
	VRBGFLTweed1lvl = nr
	VRBGFLTweed1_1_timer
end sub

sub VRBGFLTweed1_1_timer()						
	if Not VRBGFLTweed1_1.TimerEnabled then
		VRBGFLTweed1_1.visible = true
		VRBGFLTweed1_2.visible = true
		VRBGFLTweed1_3.visible = true
		VRBGFLTweed1_4.visible = true
		VRBGFLTweed1_5.visible = true
		VRBGFLTweed1_1.TimerEnabled = true
	end if
		VRBGFLTweed1_1.opacity = 65 * VRBGFLTweed1lvl^1.5
		VRBGFLTweed1_2.opacity = 65 * VRBGFLTweed1lvl^1.5
		VRBGFLTweed1_3.opacity = 65 * VRBGFLTweed1lvl^1.5
		VRBGFLTweed1_4.opacity = 75 * VRBGFLTweed1lvl^2
		VRBGFLTweed1_5.opacity = 50 * VRBGFLTweed1lvl^3
	VRBGFLTweed1lvl = 0.80 * VRBGFLTweed1lvl - 0.01
	if VRBGFLTweed1lvl < 0 then VRBGFLTweed1lvl = 0
	if VRBGFLTweed1lvl =< 0 Then
		VRBGFLTweed1_1.visible = false
		VRBGFLTweed1_2.visible = false
		VRBGFLTweed1_3.visible = false
		VRBGFLTweed1_4.visible = false
		VRBGFLTweed1_5.visible = false
		VRBGFLTweed1_1.TimerEnabled = false
	end if
end sub

'Tweed2
dim VRBGFLTweed2lvl
sub VRBGFLTweed2(nr)
	VRBGFLTweed2lvl = nr
	VRBGFLTweed2_1_timer
end sub

sub VRBGFLTweed2_1_timer()						
	if Not VRBGFLTweed2_1.TimerEnabled then
		VRBGFLTweed2_1.visible = true
		VRBGFLTweed2_2.visible = true
		VRBGFLTweed2_3.visible = true
		VRBGFLTweed2_4.visible = true
		VRBGFLTweed2_5.visible = true
		VRBGFLTweed2_1.TimerEnabled = true
	end if
		VRBGFLTweed2_1.opacity = 60 * VRBGFLTweed2lvl^1.5
		VRBGFLTweed2_2.opacity = 60 * VRBGFLTweed2lvl^1.5
		VRBGFLTweed2_3.opacity = 60 * VRBGFLTweed2lvl^1.5
		VRBGFLTweed2_4.opacity = 125 * VRBGFLTweed2lvl^2
		VRBGFLTweed2_5.opacity = 75 * VRBGFLTweed2lvl^3
	VRBGFLTweed2lvl = 0.80 * VRBGFLTweed2lvl - 0.01
	if VRBGFLTweed2lvl < 0 then VRBGFLTweed2lvl = 0
	if VRBGFLTweed2lvl =< 0 Then
		VRBGFLTweed2_1.visible = false
		VRBGFLTweed2_2.visible = false
		VRBGFLTweed2_3.visible = false
		VRBGFLTweed2_4.visible = false
		VRBGFLTweed2_5.visible = false
		VRBGFLTweed2_1.TimerEnabled = false
	end if
end sub

'Abigail
dim VRBGFLAbigaillvl
sub VRBGFLAbigail(nr)
	VRBGFLAbigaillvl = nr
	VRBGFLAbigail_1_timer
end sub

sub VRBGFLAbigail_1_timer()						
	if Not VRBGFLAbigail_1.TimerEnabled then
		VRBGFLAbigail_1.visible = true
		VRBGFLAbigail_2.visible = true
		VRBGFLAbigail_3.visible = true
		VRBGFLAbigail_4.visible = true
		VRBGFLAbigail_5.visible = true
		VRBGFLAbigail_1.TimerEnabled = true
	end if
		VRBGFLAbigail_1.opacity = 60 * VRBGFLAbigaillvl^1.5
		VRBGFLAbigail_2.opacity = 60 * VRBGFLAbigaillvl^1.5
		VRBGFLAbigail_3.opacity = 60 * VRBGFLAbigaillvl^1.5
		VRBGFLAbigail_4.opacity = 100 * VRBGFLAbigaillvl^2
		VRBGFLAbigail_5.opacity = 40 * VRBGFLAbigaillvl^3
	VRBGFLAbigaillvl = 0.80 * VRBGFLAbigaillvl - 0.01
	if VRBGFLAbigaillvl < 0 then VRBGFLAbigaillvl = 0
	if VRBGFLAbigaillvl =< 0 Then
		VRBGFLAbigail_1.visible = false
		VRBGFLAbigail_2.visible = false
		VRBGFLAbigail_3.visible = false
		VRBGFLAbigail_4.visible = false
		VRBGFLAbigail_5.visible = false
		VRBGFLAbigail_1.TimerEnabled = false
	end if
end sub

'VOLKAN
dim VRBGFLVOLlvl
sub VRBGFLVOL(nr)
	VRBGFLVOLlvl = nr
	VRBGFLVOL_1_timer
end sub

sub VRBGFLVOL_1_timer()						
	if Not VRBGFLVOL_1.TimerEnabled then
		VRBGFLVOL_1.visible = true
		VRBGFLVOL_2.visible = true
		VRBGFLVOL_3.visible = true
		VRBGFLVOL_4.visible = true
		VRBGFLKAN_1.visible = true
		VRBGFLKAN_2.visible = true
		VRBGFLKAN_3.visible = true
		VRBGFLKAN_4.visible = true
		VRBGFLVOL_1.TimerEnabled = true
	end if
		VRBGFLVOL_1.opacity = 50 * VRBGFLVOLlvl^1.5
		VRBGFLVOL_2.opacity = 50 * VRBGFLVOLlvl^1.5
		VRBGFLVOL_3.opacity = 50 * VRBGFLVOLlvl^1.5
		VRBGFLVOL_4.opacity = 100 * VRBGFLVOLlvl^2
		VRBGFLKAN_1.opacity = 50 * VRBGFLVOLlvl^1.5
		VRBGFLKAN_2.opacity = 50 * VRBGFLVOLlvl^1.5
		VRBGFLKAN_3.opacity = 50 * VRBGFLVOLlvl^1.5
		VRBGFLKAN_4.opacity = 100 * VRBGFLVOLlvl^2
	VRBGFLVOLlvl = 0.80 * VRBGFLVOLlvl - 0.01
	if VRBGFLVOLlvl < 0 then VRBGFLVOLlvl = 0
	if VRBGFLVOLlvl =< 0 Then
		VRBGFLVOL_1.visible = false
		VRBGFLVOL_2.visible = false
		VRBGFLVOL_3.visible = false
		VRBGFLVOL_4.visible = false
		VRBGFLKAN_1.visible = false
		VRBGFLKAN_2.visible = false
		VRBGFLKAN_3.visible = false
		VRBGFLKAN_4.visible = false
		VRBGFLVOL_1.TimerEnabled = false
	end if
end sub

'******************* VR Plunger **********************


Sub TimerVRPlunger_Timer
  If PinCab_Shooter.Y < 130 then
		PinCab_Shooter.Y = PinCab_Shooter.Y + 1.8
  End If
End Sub

Sub TimerVRPlunger2_Timer
	PinCab_Shooter.Y = 1 + (5* Plunger.Position) -20
	timervrplunger2.enabled = 0
End Sub




' ***************** VR Clock code below - THANKS RASCAL ******************
Dim CurrentMinute ' for VR clock 
' VR Clock code below....
Sub ClockTimer_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub
 ' ********************** END CLOCK CODE   *********************************



'***************************************************************
'	ZSHA: VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx8" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* Count from 0 up, with at least as many objects each as there can be balls, including locked balls.  You'll get an "eval" warning if tnob is higher
'	* Warning:  If merging with another system (JP's ballrolling), you may need to check tnob math and add an extra BallShadowA# flasher (out of range error)
' Ensure you have a timer with a -1 interval that is always running
' Set plastic ramps DB to *less* than the ambient shadows (-11000) if you want to see the pf shadow through the ramp
' Place triggers at the start of each ramp *type* (solid, clear, wire) and one at the end if it doesn't return to the base pf
'	* These can share duties as triggers for RampRolling sounds

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
' It's recommended that you be selective in which lights go in this collection, as there are limitations:
' 1. The shadows can "pass through" solid objects and other light sources, so be mindful of where the lights would actually able to cast shadows
' 2. If there are more than two equidistant sources, the shadows can suddenly switch on and off, so places like top and bottom lanes need attention
' 3. At this time the shadows get the light on/off from tracking gilvl, so if you have lights you want shadows for that are on at different times you will need to either:
'	a) remove this restriction (shadows think lights are always On)
'	b) come up with a custom solution (see TZ example in script)
' After confirming the shadows work in general, use ball control to move around and look for any weird behavior

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
' Example timer sub:

'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top for easy access by players
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

' *** The following segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
' ** Change gBOT to BOT if using existing getballs code
' ** Double commented lines commonly found there included for reference:

''	' stop the sound of deleted balls
''	For b = UBound(gBOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
''		...rolling(b) = False
''		...StopSound("BallRoll_" & b)
''	Next
''
'' ...rolling and drop sounds...
''
''		If DropCount(b) < 5 Then
''			DropCount(b) = DropCount(b) + 1
''		End If
''
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			BallShadowA(b).visible = 1
'			BallShadowA(b).X = gBOT(b).X + offsetX
'			If gBOT(b).Z > 30 Then
'				BallShadowA(b).height=gBOT(b).z - BallSize/4 + b/1000	'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
'				BallShadowA(b).Y = gBOT(b).Y + offsetY + BallSize/10
'			Else
'				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 1.04 + b/1000
'				BallShadowA(b).Y = gBOT(b).Y + offsetY
'			End If
'		End If

' *** Place this inside the table init, just after trough balls are added to gBOT
'
' Add balls to shadow dictionary
'	For Each xx in gBOT
'		bsDict.Add xx.ID, bsNone
'	Next

' *** Example RampShadow trigger subs:

'Sub ClearRampStart_hit()
'	bsRampOnClear			'Shadow on ramp and pf below
'End Sub

'Sub SolidRampStart_hit()
'	bsRampOn				'Shadow on ramp only
'End Sub

'Sub WireRampStart_hit()
'	bsRampOnWire			'Shadow only on pf
'End Sub

'Sub RampEnd_hit()
'	bsRampOff ActiveBall.ID	'Back to default shadow behavior
'End Sub


' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

'Function Distance(ax,ay,bx,by)
'	Distance = SQR((ax - bx)^2 + (ay - by)^2)
'End Function

'Dim PI: PI = 4*Atn(1)

'Function Atn2(dy, dx)
'	If dx > 0 Then
'		Atn2 = Atn(dy / dx)
'	ElseIf dx < 0 Then
'		If dy = 0 Then
'			Atn2 = pi
'		Else
'			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
'		end if
'	ElseIf dx = 0 Then
'		if dy = 0 Then
'			Atn2 = 0
'		else
'			Atn2 = Sgn(dy) * pi / 2
'		end if
'	End If
'End Function

'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******

' *** These define the appearance of shadows in your table	***

Const BallBrightness =  1.0         'Ball brightness - Value between 0 and 1 (0=Dark ... 1=Bright)

'Ambient (Room light source)
Const AmbientBSFactor = 0.9	 '0 To 1, higher is darker
Const AmbientMovement = 1	   '1+ higher means more movement as the ball moves left and right
Const offsetX = 0			   'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 5			   'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

'Dynamic (Table light sources)
Const DynamicBSFactor = 0.90	'0 To 1, higher is darker
Const Wideness = 20			 'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness = 5			  'Sets minimum as ball moves away from source

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objrtx1(5), objrtx2(5)
Dim objBallShadow(5)
Dim OnPF(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

' *** The Shadow Dictionary
Dim bsDict
Set bsDict = New cvpmDictionary
Const bsNone = "None"
Const bsWire = "Wire"
Const bsRamp = "Ramp"
Const bsRampClear = "Clear"

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

Sub BallOnPlayfieldNow(onPlayfield, ballNum)	'Whether a ball is currently on the playfield. Only update certain things once, save some cycles
	If onPlayfield Then
		OnPF(ballNum) = True
		bsRampOff gBOT(ballNum).ID
		'   debug.print "Back on PF"
		UpdateMaterial objBallShadow(ballNum).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(ballNum).size_x = 5
		objBallShadow(ballNum).size_y = 4.5
		objBallShadow(ballNum).visible = 1
		BallShadowA(ballNum).visible = 0
		BallShadowA(ballNum).Opacity = 100 * AmbientBSFactor
	Else
		OnPF(ballNum) = False
		'   debug.print "Leaving PF"
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff 'Max distance to light sources, can be changed dynamically if you have a reason
	falloff = 150
	Dim ShadowOpacity1, ShadowOpacity2
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim bsRampType
	'   Dim gBOT: gBOT=getballs	'Uncomment if you're destroying balls - Not recommended! #SaveTheBalls
	
	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 To tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next
	
	If UBound(gBOT) < lob Then Exit Sub 'No balls in play, exit
	
	'The Magic happens now
	For s = lob To UBound(gBOT)
		' *** Normal "ambient light" ball shadow
		'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your Elseif segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else (under 20)
		
		'Primitive shadow on playfield, flasher shadow in ramps
		If AmbientBallShadowOn = 1 Then
			'** Above the playfield
			If gBOT(s).Z > 30 Then
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update
				bsRampType = getBsRampType(gBOT(s).id)
				'   debug.print bsRampType
				
				If Not bsRampType = bsRamp Then 'Primitive visible on PF
					objBallShadow(s).visible = 1
					objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
					objBallShadow(s).Y = gBOT(s).Y + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z + BallSize) / 80) 'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z + BallSize) / 80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor * (30 / (gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else 'Opaque, no primitive below
					objBallShadow(s).visible = 0
				End If
				
				If bsRampType = bsRampClear Or bsRampType = bsRamp Then 'Flasher visible on opaque ramp
					BallShadowA(s).visible = 1
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize / 10
					BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000 'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
					If bsRampType = bsRampClear Then BallShadowA(s).Opacity = 50 * AmbientBSFactor
				ElseIf bsRampType = bsWire Or bsRampType = bsNone Then 'Turn it off on wires or falling out of a ramp
					BallShadowA(s).visible = 0
				End If
				
				'** On pf, primitive only
			ElseIf gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then
				If Not OnPF(s) Then BallOnPlayfieldNow True, s
				objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
				objBallShadow(s).Y = gBOT(s).Y + offsetY
				'   objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf
				
				'** Under pf, flasher shadow only
			Else
				If OnPF(s) Then BallOnPlayfieldNow False, s
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000
			End If
			
			'Flasher shadow everywhere
		ElseIf AmbientBallShadowOn = 2 Then
			If gBOT(s).Z > 30 Then 'In a ramp
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize / 10
				BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000 'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			ElseIf gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then 'On pf
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height = 1.04 + s / 1000
			Else 'Under pf
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000
			End If
		End If
		
		' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 Then 'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff
				dist2 = falloff
				For iii = 0 To numberofsources - 1 'Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then
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
					objrtx1(s).X = gBOT(s).X
					objrtx1(s).Y = gBOT(s).Y
					'   objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1 * DynamicBSFactor ^ 3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1
					objrtx2(s).X = gBOT(s).X
					objrtx2(s).Y = gBOT(s).Y + offsetY
					'   objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), gBOT(s).X, gBOT(s).Y) + 90
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

' *** Ramp type definitions

Sub bsRampOnWire()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsWire
	Else
		bsDict.Add ActiveBall.ID, bsWire
	End If
End Sub

Sub bsRampOn()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsRamp
	Else
		bsDict.Add ActiveBall.ID, bsRamp
	End If
End Sub

Sub bsRampOnClear()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsRampClear
	Else
		bsDict.Add ActiveBall.ID, bsRampClear
	End If
End Sub

Sub bsRampOff(idx)
	If bsDict.Exists(idx) Then
		bsDict.Item(idx) = bsNone
	End If
End Sub

Function getBsRampType(id)
	Dim retValue
	If bsDict.Exists(id) Then
		retValue = bsDict.Item(id)
	Else
		retValue = bsNone
	End If
	getBsRampType = retValue
End Function

'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************






