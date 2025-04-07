'----------- bases on Borgdog's (Eclipse Gottlieb 1982)
'
'  ___    ___ ________  ___  ___  ________   ________                                                                       
' |\  \  /  /|\   __  \|\  \|\  \|\   ___  \|\   ____\                                                                      
' \ \  \/  / | \  \|\  \ \  \\\  \ \  \\ \  \ \  \___|                                                                      
'  \ \    / / \ \  \\\  \ \  \\\  \ \  \\ \  \ \  \  ___                                                                    
'   \/  /  /   \ \  \\\  \ \  \\\  \ \  \\ \  \ \  \|\  \                                                                   
' __/  / /      \ \_______\ \_______\ \__\\ \__\ \_______\                                                                  
'|\___/ /        \|_______|\|_______|\|__| \|__|\|_______|                                                                  
'\|___|/                                                                                                                    
'                                                                                                                           
'                                                                                                                           
' ________ ________  ________  ________   ___  __    _______   ________   ________  _________  ___  _______   ________      
'|\  _____\\   __  \|\   __  \|\   ___  \|\  \|\  \ |\  ___ \ |\   ___  \|\   ____\|\___   ___\\  \|\  ___ \ |\   ___  \    
'\ \  \__/\ \  \|\  \ \  \|\  \ \  \\ \  \ \  \/  /|\ \   __/|\ \  \\ \  \ \  \___|\|___ \  \_\ \  \ \   __/|\ \  \\ \  \   
' \ \   __\\ \   _  _\ \   __  \ \  \\ \  \ \   ___  \ \  \_|/_\ \  \\ \  \ \_____  \   \ \  \ \ \  \ \  \_|/_\ \  \\ \  \  
'  \ \  \_| \ \  \\  \\ \  \ \  \ \  \\ \  \ \  \\ \  \ \  \_|\ \ \  \\ \  \|____|\  \   \ \  \ \ \  \ \  \_|\ \ \  \\ \  \ 
'   \ \__\   \ \__\\ _\\ \__\ \__\ \__\\ \__\ \__\\ \__\ \_______\ \__\\ \__\____\_\  \   \ \__\ \ \__\ \_______\ \__\\ \__\
'    \|__|    \|__|\|__|\|__|\|__|\|__| \|__|\|__| \|__|\|_______|\|__| \|__|\_________\   \|__|  \|__|\|_______|\|__| \|__|
'                                                                           \|_________|                                    
'****************************************************************************************************************************
'
'	CREDITS
'
	'Builders:
'HauntFreaks <-- Concept, Layout, Design, Graphics, teency bit of coding
'BorgDog <-- Original table, rebuild of script
'Rawd  <-- VR room concept and design, tons of scripting help
'fluffhead35 <-- Added nFozzy Flippers, targetbouncer logic, flipper Rubberizer logic, and fleep sounds
'Leojreimroc  <-- VR segmented scoring display
'
'	Testers:
'RIK
'Wylte
'apophis
'Scampa123
'tomate   
'EBisLit                    
'
'	Special Thanks:
'Rawd for "forcing me to code a bunch of shit myself" aka "teaching a man to fish"
'Scampa123 for last minute testing and helping Rawd with the VR!!
'AstroNasty for last minute ramp graphic!!
'the VP Dev's that make this all possible!!
'
'
'****************************************************************************************************************************
 
Option Explicit
Randomize
 
Const cGameName     = "eclipse"  
 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0
 
LoadVPM "01000100", "sys80.vbs", 2.31
Dim Object ' for VR room Toggle


BallSize = 50					'Ball size must be 50
BallMass = 1					'Ball mass must be 1

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 1

'// Volume dial for the BackGround Music.  Value is between 0 and 1
Const BgVolumeDial = .25

'/////////////////////-----BallRoll Sound Amplification -----/////////////////////
Const BallRollAmpFactor = 3 ' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)

'----- Phsyics Mods -----
Const RubberizerEnabled = 2			'0 = normal flip rubber, 1 = more lively rubber for flips, 2 = a different rubberizer
Const FlipperCoilRampupMode = 2   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. 0.0 thru 1.0, higher value is more bounciness (don't go above 1)


'**********************************************************
'********       OPTIONS     *******************************
'**********************************************************

Dim BallShadows: Ballshadows=0          	'******************set to 1 to turn on Ball shadows
Dim FlipperShadows: FlipperShadows=0  '***********set to 1 to turn on Flipper shadows
Dim ROMSounds: ROMSounds=0			'**********set to 0 for no rom sounds, 1 to play rom sounds.. mostly used for testing



Dim b2sstep
b2sstep = 0
'b2sflash.enabled = 0
Dim b2satm

Sub startB2S(aB2S)
	b2sflash.enabled = 0
	b2satm = ab2s
End Sub

Sub b2sflash_timer
    If B2SOn Then
	b2sstep = b2sstep + 1
	Select Case b2sstep
		Case 0
		Controller.B2SSetData b2satm, 0
		Case 1
		Controller.B2SSetData b2satm, 1
		Case 2
		Controller.B2SSetData b2satm, 0
		Case 3
		Controller.B2SSetData b2satm, 1
		Case 4
		Controller.B2SSetData b2satm, 0
		Case 5
		Controller.B2SSetData b2satm, 1
		Case 6
		Controller.B2SSetData b2satm, 0
		Case 7
		Controller.B2SSetData b2satm, 1
		Case 8
		Controller.B2SSetData b2satm, 0
		b2sstep = 0
		b2sflash.enabled = 0
	End Select
    End If
End Sub

 
'************************************************
'*******     END OPTIONS     ********************
'************************************************
'************************************************
'************************************************

Const UseSolenoids  = 2
Const UseLamps      = 1
Const UseGI         = 0
 
' Standard Sounds
Const SSolenoidOn  = ""
Const SSolenoidOff = ""
Const SCoin        = "fx_coin"
 
Dim xx, objekt
Sub YF_Init()
 
 
    On Error Resume Next
      With Controller
        .GameName                               = cGameName
        .SplashInfoLine                         = "YF, Gottlieb 1982"
        .HandleKeyboard                         = 0
        .ShowTitle                              = 0
        .ShowDMDOnly                            = 1
        .ShowFrame                              = 0
        .ShowTitle                              = 0
        .hidden                                 = 1
        .Games(cGameName).Settings.Value("rol") = 0
	.Games(cGameName).Settings.Value("sound") = ROMSounds
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
      End With
    On Error Goto 0
       
' basic pinmame timer
    PinMAMETimer.Interval   = PinMAMEInterval
    PinMAMETimer.Enabled    = 1

	setup_backglass
	SetupVRRoom
 
' Nudging
    vpmNudge.TiltSwitch = 57
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3, Bumper4)
 
'Controlled Lights
 	vpmMapLights CPULights
   
'************************Trough

	Trough2.CreateBall
	Trough1.CreateBall
	sw25.CreateBall

	Controller.Switch(25) = 1
	controller.Switch(15) = 0
 
    if b2son then: for each xx in backdropstuff: xx.visible = 0: next

    startGame.enabled= True

	if ballshadows=1 then
        BallShadowUpdate.enabled=1
      else
        BallShadowUpdate.enabled=0
    end if

    if flippershadows=1 then
        FlipperLSh.visible=1
        FlipperRSh.visible=1
       else
        FlipperLSh.visible=0
        FlipperRSh.visible=0
 
    end if
    FindDips        'find if match enabled, if so turn back on number to match box
 
End Sub
 
Sub YF_Paused:Controller.Pause = 1:End Sub
 
Sub YF_unPaused:Controller.Pause = 0:End Sub
 
Sub YF_Exit
    If b2son then controller.stop
End Sub
 
sub startGame_timer
    dim xx
    playsound "poweron"
    LampTimer.enabled=1
    For each xx in GI:xx.State = 1: Next        '*****GI Lights On
	LFLogo.image="Gottflip_greyL"
	RFLogo.image="Gottflip_greyR"
	LFLogo1.image="Gottflip_greyL"
	RFLogo1.image="Gottflip_greyR"
	BumperBase1.image="YF_Bumper"
	BumperCap1.image="YF_BumperCap"
	BumperBase2.image="YF_Bumper"
	BumperCap2.image="YF_BumperCap"
	BumperBase3.image="YF_Bumper"
	BumperCap3.image="YF_BumperCap"
	BumperBase4.image="YF_Bumper"
	BumperCap4.image="YF_BumperCap"
	plastics_1.image="YF_Plastics"
	plastics_2.image="YF_Plastics"
	plastics_4.image="YF_Plastics"
	plastics_5.image="YF_Plastics"
	plastics_6.image="YF_Plastics"
	plastics_7.image="YF_Plastics"
	plastics_8.image="YF_Plastics"
	VRBackWall.Sideimage= "backwall"
	metalRamp.image="GIramp_2"
	wireramps.image="wire_ramp_1"
	Psw34Shield.image="center_target_brain"
	apron.image ="YF_Apron"
	shadowsGIOFF.visible= 0
	shadowsGION.visible= 1
	BGLit.visible = 1
		Dim a_rubbers_on
	for each a_rubbers_on in a_rubbers: a_rubbers_on.image = "": next
		Dim a_posts_on
	for each a_posts_on in a_posts: a_posts_on.image = "": next
		Dim a_DropTargets_on
	for each a_DropTargets_on in a_DropTargets: a_DropTargets_on.image = "DT_YF_target": next

    Dim a_Nuts_on
	for each a_Nuts_on in Nuts: a_Nuts_on.image = "": next

	MusicOn
    DigitsTimer.enabled = true ' lights up the divertor timer digits in time with the other GI

    me.enabled=false
end sub
 

Dim musicPlaying : musicPlaying = False

sub MusicOn
   musicPlaying = False
   PlayMusic "YF_bg_music.mp3", BgVolumeDial
end Sub

Sub YF_MusicDone()
   MusicOn
End Sub

Sub GameOver_timer()
	If Controller.Lamp(11) Then
		if gameNumber > 0 Then
			EndMusic
			select case Int(rnd*14 + 1)
				case 1: PlaySound "Over01"
				case 2: PlaySound "Over02"
				case 3: PlaySound "Over03"
				case 4: PlaySound "Over04"
				case 5: PlaySound "Over05"
				case 6: PlaySound "Over06"
				case 7: PlaySound "Over07"
				case 8: PlaySound "Over08"
				case 9: PlaySound "Over09"
				case 10: PlaySound "Over10"
				case 11: PlaySound "Over11"
				case 12: PlaySound "Over12"
				case 13: PlaySound "Over13"
				case 14: PlaySound "Over14"
			End Select
			musicPlaying = False
			gameNumber = 0
		End If
			GameOver.Enabled = False
	End If
End Sub

Dim gameNumber : gameNumber = 0

Sub YF_KeyDown(ByVal keycode)
 
    If keycode = PlungerKey Then
        Plunger.PullBack
        'PlaySoundAt "plungerpull", Plunger
		SoundPlungerPull()

		TimerVRPlunger.Enabled = True   'VR Plunger
		TimerVRPlunger2.Enabled = False   'VR Plunger

    End If

	


	If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter()

	
    'If keycode=AddCreditKey then playsound "coin": vpmTimer.pulseSW (swCoin1): end if

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
			vpmTimer.pulseSW (swCoin1)
			Select Case Int(rnd*3)
					Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
					Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
					Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

			End Select
	End If

	If keycode = StartGameKey Then
		Primary_startbuttoninner.y = Primary_startbuttoninner.y  - 7              ' VR Start Button Animation here 
		Primary_startbutton.y = Primary_startbutton.y  - 7 

		soundStartButton()

		gameNumber = gameNumber + 1
	
		if Not musicPlaying Then
			MusicOn
		End If

		select case Int(rnd*13 + 1)
			case 1: PlaySound "Start01"
			case 2: PlaySound "Start02"
			case 3: PlaySound "Start03"
			case 4: PlaySound "Start04"
			case 5: PlaySound "Start05"
			case 6: PlaySound "Start06"
			case 7: PlaySound "Start07"
			case 8: PlaySound "Start08"
			case 9: PlaySound "Start09"
			case 10: PlaySound "Start10"
			case 11: PlaySound "Start11"
			case 12: PlaySound "Start12"
			case 13: PlaySound "Start13"
		End Select
		End If

	If keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
		VRFlipperLeft.X = VRFlipperLeft.X + 6   ' VR Flipper animation
	end if

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		VRFlipperRight.X = VRFlipperRight.X - 6   ' VR Flipper animation
	end if

	If keycode = LeftMagnasave Then

	If CandleResetReady = true Then
	CandleResetReady = false
	BookshelfTimer2.enabled = true

	' reset all candle components here..
	Smoke1.height = 690
	Smoke1.opacity = 20
	Smoke1.visible = false

	Smoke2.height = 745
	Smoke2.opacity = 17
	Smoke2.visible = false

	CandleTop.x = -1470.268
	CandleTop.y =1007.532
	CandleTop.z = 595
	CandleTop.Size_Y = 45

	CandleFlasher.y = CandleFlasher.y - 40.2  ' for some reason I cant set flashers back to its original coordinates.  I had to find this value with trial and error.
	CandleFlasher.height = 1215
	CandleFlasher.opacity = 1500

	CandleSpinnerTimer.enabled = true
	CandleBurnTimer.enabled = true
	VRCandleWoodTimer.enabled = true
	VRCandleTimer.enabled = true
	VRCandleTimer2.enabled = true
	end if
	End If

	 If vpmKeyDown(keycode) Then Exit Sub
End Sub
 

Sub YF_KeyUp(ByVal keycode)
 
    If keycode = 61 then FindDips
 
    If keycode = PlungerKey Then
		Plunger.Fire

		if ballhome.ballcntover>0 then
			'PlaySoundAt "plungerreleaseball", Plunger	'PLAY WHEN THERE IS A BALL TO HIT
			SoundPlungerReleaseBall()
		  else
			'PlaySoundAt "plungerreleasefree", Plunger	'PLAY WHEN NO BALL TO PLUNGE
			SoundPlungerReleaseNoBall()
		end if


		TimerVRPlunger.Enabled = False  'VR Plunger
		TimerVRPlunger2.Enabled = True   ' VR Plunger
		VRPlunger.Y = 2416  ' VR Plunger
	End If
	   
	If keycode = LeftFlipperKey Then
	FlipperDeActivate LeftFlipper, LFPress
	VRFlipperLeft.X = VRFlipperLeft.X - 6   ' VR Flipper animation
	end if

	If keycode = RightFlipperKey Then
	FlipperDeActivate RightFlipper, RFPress
	VRFlipperRight.X = VRFlipperRight.X + 6   ' VR Flipper animation
	end if

	If keycode = StartGameKey Then
		Primary_startbuttoninner.y = Primary_startbuttoninner.y  + 7              ' VR Start Button Animation here 
		Primary_startbutton.y = Primary_startbutton.y  + 7 
		End If

		If vpmKeyUp(keycode) Then Exit Sub 
End Sub

 
'*****************************************
'Solenoids
'*****************************************
SolCallBack(1)  = "Lraised"
SolCallBack(2)  = "Craised"
SolCallback(5)  = "Ukicker"
SolCallback(6)  = "Rraised"    
SolCallback(8)  = "SolKnocker"
SolCallback(9)  = "SolOutHole"
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
 

Sub Rraised(enabled)
    if enabled then Rreset.enabled=True
End Sub
 
Sub Rreset_timer
	PlaySoundAtVol "dropbankreset", sw32, 1
    For each objekt in DTright: objekt.isdropped=0: next
'    For each light in DTRightLights: light.state=0: Next
    Rreset.enabled=false
End Sub
 
Sub Lraised(enabled)
    if enabled then Lreset.enabled=True
End Sub
 
Sub Lreset_timer
	PlaySoundAtVol "dropbankreset", sw53, 1
     For each objekt in DTleft: objekt.isdropped=0: next
'    For each light in DTLeftLights: light.state=0: Next
    Lreset.enabled=false
End Sub
 
Sub Craised(enabled)
    if enabled then Creset.enabled=True
End Sub
 
Sub Creset_timer
	PlaySoundAtVol "dropbankreset", sw4, 1
     For each objekt in DTmid: objekt.isdropped=0: next
'    For each light in DTmidLights: light.state=0: Next
    Creset.enabled=false
End Sub

Const ReflipAngle = 20
 
Sub SolLFlipper(Enabled)
    If Enabled Then
        LF.Fire 'LeftFlipper.RotateToEnd
        LF1.Fire 'LeftFlipper1.RotateToEnd
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
		If leftflipper1.currentangle < leftflipper1.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper1
		Else 
			SoundFlipperUpAttackLeft LeftFlipper1
			RandomSoundFlipperUpLeft LeftFlipper1
		End If
    Else
        LeftFlipper.RotateToStart
        LeftFlipper1.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		If LeftFlipper1.currentangle < LeftFlipper1.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper1
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub
 
Sub SolRFlipper(Enabled)
    If Enabled Then
        RF.Fire 'RightFlipper.RotateToEnd
        RF1.Fire 'RightFlipper1.RotateToEnd
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
		If rightflipper1.currentangle > rightflipper1.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper1
		Else 
			SoundFlipperUpAttackRight RightFlipper1
			RandomSoundFlipperUpRight RightFlipper1
		End If
    Else
        RightFlipper.RotateToStart
        RightFlipper1.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		If RightFlipper1.currentangle > RightFlipper1.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper1
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
    End If
End Sub
 
 
Sub SolKnocker(Enabled)
    If Enabled Then 
		'PlaySoundAtVol SoundFX("Knocker",DOFKnocker), Plunger, 1
		KnockerSolenoid 
	End If
End Sub
 
Sub LSaucer
    sw5.kick 180,6
    'PlaySoundAtVol SoundFX("popper_ball",DOFContactors), sw5, 1
	SoundSaucerKick 0, Sw5
    sw5.uservalue=1
    sw5.timerenabled=1
	PkickarmSW5.rotz=15
End Sub
 
Sub sw5_timer
    select case sw5.uservalue
      case 2:
        PkickarmSW5.rotz=0
        me.timerenabled=0
    end Select
    sw5.uservalue=sw5.uservalue+1
End Sub
 
Sub Ukicker(Enabled)
    If Enabled Then
		sw30.kick 0, 30, 1.56
		controller.switch(30) = 0
	end If
End Sub
 


'*****************************************
' Saucer
'*****************************************

Sub sw5_Hit()
	startB2S(80)
	SoundSaucerLock
    'PlaySoundAt "fx_hole_enter", sw5
    select case Int(rnd*9 + 1)
        case 1: PlaySound "Kicker01"
        case 2: PlaySound "Kicker02"
        case 3: PlaySound "Kicker03"
        case 4: PlaySound "Kicker04"
        case 5: PlaySound "Kicker05"
        case 6: PlaySound "Kicker06"
        case 7: PlaySound "Kicker07"
        case 8: PlaySound "Kicker08"
        case 9: PlaySound "Kicker09"
    End Select
    Controller.Switch(5) = 1
End Sub
  
Sub SW5_unHit:Controller.Switch(5)=0:End Sub
 
'*****************************************
'Wire rollover Switches
'*****************************************

Sub SW0_Hit:Controller.Switch(0)=1:End Sub    
Sub SW0_unHit:Controller.Switch(0)=0:End Sub
Sub SW10_Hit:Controller.Switch(10)=1:End Sub    
Sub SW10_unHit:Controller.Switch(10)=0:End Sub
Sub SW20_Hit:Controller.Switch(20)=1:End Sub    
Sub SW20_unHit:Controller.Switch(20)=0:End Sub

Sub SW30_Hit:Controller.Switch(30)=1:End Sub    

Sub sw52_Hit()
    'PlaySoundAt "fx_hole_enter", sw52
	SoundSaucerLock
    select case Int(rnd*7 + 1)
        case 1: PlaySound "Rside01"
        case 2: PlaySound "Rside02"
        case 3: PlaySound "Rside03"
        case 4: PlaySound "Rside04"
        case 5: PlaySound "Rside05"
        case 6: PlaySound "Rside06"
        case 7: PlaySound "Rside07"
    End Select
	Controller.Switch(52)=1:End Sub   
Sub SW52_unHit:Controller.Switch(52)=0:End Sub

Sub SW54_Hit:Controller.Switch(54)=1:End Sub   
Sub SW54_unHit:Controller.Switch(54)=0:End Sub

Sub Trigger001_Hit: WireRampOn False : End Sub
Sub Trigger002_Hit: WireRampOff : End Sub
Sub Trigger003_Hit: WireRampOn False : End Sub
Sub Trigger004_Hit: WireRampOff : End Sub

'*****************************************
'Rollunder Gates
'*****************************************
Sub sw50a_Hit()
	startB2S(80)
    PlaySoundAt "rollover", sw50a
    select case Int(rnd*8 + 1)
        case 1: PlaySound "Ramp01"
        case 2: PlaySound "Ramp02"
        case 3: PlaySound "Ramp03"
        case 4: PlaySound "Ramp04"
        case 5: PlaySound "Ramp05"
        case 6: PlaySound "Ramp06"
        case 7: PlaySound "Ramp07"
        case 8: PlaySound "Ramp08"
    End Select
'    Controller.Switch(50a) = 1
End Sub

Sub SW50_Hit:vpmTimer.PulseSw (50):End Sub  

'*****************************************
'Spinners
'*****************************************

Sub Sw35_Spin
	vpmTimer.PulseSw (35)
	PlaySoundAt "fx_spinner", sw35
End Sub

'*****************************************
' Pop Bumpers
'*****************************************
 

Sub BumperFlasherTimer_Timer()  ' turns off bumper flasher lights

Bumper1Flasher.visible = false
Bumper2Flasher.visible = false
Bumper3Flasher.visible = false
Bumper4Flasher.visible = false

BumperBase1.image="YF_Bumper2a"
BumperCap1.image="YF_BumperCap2a"
BumperBase2.image="YF_Bumper2a"
BumperCap2.image="YF_BumperCap2a"
BumperBase3.image="YF_Bumper2a"
BumperCap3.image="YF_BumperCap2a"
BumperBase4.image="YF_Bumper2a"
BumperCap4.image="YF_BumperCap2a"

BumperFlasherTimer.enabled = false
End Sub


Sub Bumper1_Hit
	startB2S(80)
    BGLightning.visible = True
    Bumper1Flasher.visible = true

    BumperBase2.image="YF_Bumper2Red"
	BumperCap2.image="YF_BumperCap2Red"

	BumperFlasherTimer.enabled = true
    vpmTimer.PulseSw 51
    'PlaySoundAtVol SoundFX("fx_bumper2",DOFContactors), Bumper1, 1
	RandomSoundBumperTop Bumper1
	PlaySound "thunder01"
    DOF 103,DOFPulse
    
End Sub
 
Sub Bumper2_Hit
	startB2S(80)
    BGLightning.visible = True
    vpmTimer.PulseSw 51
    Bumper2Flasher.visible = true

    BumperBase1.image="YF_Bumper2Red"
	BumperCap1.image="YF_BumperCap2Red"

	BumperFlasherTimer.enabled = true
    'PlaySoundAtVol SoundFX("fx_bumper2",DOFContactors), Bumper2, 1
	RandomSoundBumperBottom Bumper2
	PlaySound "thunder02"
    DOF 102,DOFPulse
    
End Sub
 
Sub Bumper3_Hit
	startB2S(80)
    BGLightning.visible = True
    vpmTimer.PulseSw 51
    Bumper3Flasher.visible = true

    BumperBase3.image="YF_Bumper2Red"
	BumperCap3.image="YF_BumperCap2Red"

	BumperFlasherTimer.enabled = true
    'PlaySoundAtVol SoundFX("fx_bumper2",DOFContactors), Bumper3, 1
	RandomSoundBumperMiddle Bumper3
	PlaySound "thunder03"
    DOF 104,DOFPulse
    
End Sub
 
Sub Bumper4_Hit
	startB2S(80)
    BGLightning.visible = True
    vpmTimer.PulseSw 51
    Bumper4Flasher.visible = true
	BumperFlasherTimer.enabled = true

    BumperBase4.image="YF_Bumper2Red"
	BumperCap4.image="YF_BumperCap2Red"

    'PlaySoundAtVol SoundFX("fx_bumper2",DOFContactors), Bumper4, 1
	RandomSoundBumperTop Bumper4
	PlaySound "thunder04"
    DOF 101,DOFPulse
  
End Sub

'*****************************************
' Targets
'*****************************************

Sub SW6_Hit:vpmTimer.PulseSw 06 :End Sub 			'left bank   
Sub SW16_Hit:vpmTimer.PulseSw 16:End Sub    
Sub SW26_Hit:vpmTimer.PulseSw 26:End Sub    
Sub SW36_Hit:vpmTimer.PulseSw 36:End Sub 

Sub SW1_Hit:vpmTimer.PulseSw 01:End Sub 			'right bank   
Sub SW11_Hit:vpmTimer.PulseSw 11:End Sub    
Sub SW21_Hit:vpmTimer.PulseSw 21:End Sub    
Sub SW31_Hit:vpmTimer.PulseSw 31:End Sub 

Sub SW34_Slingshot									'kicking target right
	vpmTimer.PulseSw 34
	PlaySoundAtVol SoundFXDOF("fx_solenoid",106,DOFPulse,DOFContactors), PkickHole, 1
	Psw34Kicker.rotx=-2
	Psw34Shield.rotx=-2
	me.uservalue=1
	me.timerenabled=1
End Sub 

Sub sw34_timer
    Select Case me.uservalue
'        Case 3:	Psw34Kicker.rotx=11: Psw34Shield.rotx=11
'        Case 4:	Psw34Kicker.rotx=7: Psw34Shield.rotx=7
		Case 1: Psw34Kicker.rotx=1: Psw34Shield.rotx=3: me.TimerEnabled = 0
    End Select
    me.uservalue = me.uservalue + 1
End Sub

Sub Tsw34_hit
	Psw34Kicker.rotx=-2
	Psw34Shield.rotx=-2
        PlaySoundAtVol SoundFX("fx_vukExit_wire",DOFContactors), Tsw34, 1
    select case Int(rnd*10 + 1)
        case 1: PlaySound "center01"
        case 2: PlaySound "center02"
        case 3: PlaySound "center03"
        case 4: PlaySound "center04"
        case 5: PlaySound "center05"
        case 6: PlaySound "center06"
        case 7: PlaySound "center07"
        case 8: PlaySound "center08"
        case 9: PlaySound "center09"
        case 10: PlaySound "center10"
    End Select
End Sub

Sub Tsw34a_hit
	Psw34Kicker.rotx=-5
	Psw34Shield.rotx=-5
End Sub

Sub Tsw34_unhit
	if Not sw34.timerenabled then
		Psw34Kicker.rotx=1
		Psw34Shield.rotx=1
	end if
End Sub

'*****************************************
' Drop Targets
'*****************************************

Sub SW4_Hit:vpmTimer.PulseSw (4):End Sub 			'center bank   
Sub SW14_Hit:vpmTimer.PulseSw (14):End Sub    
Sub SW24_Hit:vpmTimer.PulseSw (24):End Sub  

Sub SW3_Hit:vpmTimer.PulseSw (3):End Sub 			'left bank   
Sub SW13_Hit:vpmTimer.PulseSw (13):End Sub    
Sub SW23_Hit:vpmTimer.PulseSw (23):End Sub    
Sub SW33_Hit:vpmTimer.PulseSw (33):End Sub 
Sub SW53_Hit:vpmTimer.PulseSw (53):End Sub 

Sub SW2_Hit:vpmTimer.PulseSw (2):End Sub 			'right bank   
Sub SW12_Hit:vpmTimer.PulseSw (12):End Sub    
Sub SW22_Hit:vpmTimer.PulseSw (22):End Sub    
Sub SW32_Hit:vpmTimer.PulseSw (32):End Sub 

'****************
' Secret Passage
'****************

Sub sw56_Hit()
	startB2S(80)
    PlaySoundAt "rollover", sw56
    select case Int(rnd*8 + 1)
        case 1: PlaySound "Passage01"
        case 2: PlaySound "Passage02"
        case 3: PlaySound "Passage03"
        case 4: PlaySound "Passage04"
        case 5: PlaySound "Passage05"
        case 6: PlaySound "Passage06"
        case 7: PlaySound "Passage07"
        case 8: PlaySound "Passage08"
    End Select



GiOff  ' Turns off GI  (sub below)
GIStrobeTimer.enabled = true  ' Starts Strobe timer.  The strobe timer runs to Storbe = 51 and counts along the way
HatchPointsTimer.enabled = true
StrobeRunning = true  'This variable was made so that our strobe didnt interfere with Controller10 sub
End Sub


'****************
' Sling Shot and Rubber Animations
'****************
 
'Sub RightSlingShot_Slingshot
'	vpmTimer.PulseSw 56
'	PlaySoundAtVol SoundFXDOF("fx_slingshot",106,DOFPulse,DOFContactors), slingR, 1
'    RSling.Visible = 0
'    RSling1.Visible = 1
'	slingR.rotx = 20	
'    me.uservalue = 1
'    me.TimerEnabled = 1
'End Sub
'
'Sub RightSlingShot_Timer
'    Select Case me.uservalue
'        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:slingR.rotx = 10
'        Case 4:	slingR.rotx = 0:RSLing2.Visible = 0:RSLing.Visible = 1:me.TimerEnabled = 0
'    End Select
'    me.uservalue = me.uservalue + 1
'End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 56
	'PlaySoundAtVol SoundFXDOF("fx_slingshot",104,DOFPulse,DOFContactors), slingL, 1
	RandomSoundSlingshotLeft slingL
    LSling.Visible = 0
    LSling1.Visible = 1
	slingL.rotx = 20	
    me.uservalue= 1
    me.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case me.uservalue
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:slingL.rotx = 10
        Case 4:slingL.rotx = 0:LSLing2.Visible = 0:LSLing.Visible = 1:me.TimerEnabled = 0
    End Select
    me.uservalue = me.uservalue + 1
End Sub

sub Wall_RB005_hit
	vpmTimer.PulseSw 56
'	SlingA.visible=0
'	SlingA1.visible=1
'	me.uservalue=1
'	Me.timerenabled=1
end sub

'sub dingwalla_timer									'default 50 timer
'	select case me.uservalue
'		Case 1: SlingA1.visible=0: SlingA.visible=1
'		case 2:	SlingA.visible=0: SlingA2.visible=1
'		Case 3: SlingA2.visible=0: SlingA.visible=1: Me.timerenabled=0
'	end Select
'	me.uservalue=me.uservalue+1
'end sub

sub Wall_RB013_hit
	vpmTimer.PulseSw 56
'	SlingB.visible=0
'	SlingB1.visible=1
'	me.uservalue=1
'	Me.timerenabled=1
end sub

'
'sub dingwallb_timer									'default 50 timer
'	select case me.uservalue
'		Case 1: Slingb1.visible=0: SlingB.visible=1
'		case 2:	SlingB.visible=0: Slingb2.visible=1
'		Case 3: Slingb2.visible=0: SlingB.visible=1: Me.timerenabled=0
'	end Select
'	me.uservalue=me.uservalue+1
'end sub

'sub dingwallc_hit
'	vpmTimer.PulseSw 56
'	Slingc.visible=0
'	Slingc1.visible=1
'	me.uservalue=1
'	Me.timerenabled=1
'end sub
'
'sub dingwallc_timer									'default 50 timer
'	select case me.uservalue
'		Case 1: Slingc1.visible=0: Slingc.visible=1
'		case 2:	Slingc.visible=0: Slingc2.visible=1
'		Case 3: Slingc2.visible=0: Slingc.visible=1: Me.timerenabled=0
'	end Select
'	me.uservalue=me.uservalue+1
'end sub
'
'sub dingwalld_hit
'	SlingD.visible=0
'	SlingD1.visible=1
'	me.uservalue=1
'	Me.timerenabled=1
'end sub

'sub dingwalld_timer									'default 50 timer
'	select case me.uservalue
'		Case 1: SlingD1.visible=0: SlingD.visible=1
'		case 2:	SlingD.visible=0: SlingD2.visible=1
'		Case 3: SlingD2.visible=0: SlingD.visible=1: Me.timerenabled=0
'	end Select
'	me.uservalue=me.uservalue+1
'end sub
'
'sub dingwalle_hit
'	SlingD.visible=0
'	SlingE1.visible=1
'	me.uservalue=1
'	Me.timerenabled=1
'end sub
'
'sub dingwalle_timer									'default 50 timer
'	select case me.uservalue
'		Case 1: SlingE1.visible=0: SlingD.visible=1
'		case 2:	SlingD.visible=0: SlingE2.visible=1
'		Case 3: SlingE2.visible=0: SlingD.visible=1: Me.timerenabled=0
'	end Select
'	me.uservalue=me.uservalue+1
'end sub

sub Wall23_Hit
	TargetBouncer Activeball, 1
end Sub

sub Wall21_Hit
	TargetBouncer Activeball, 1
end Sub


'****************
' Add some randomness to gate1 bounce
'****************

Sub TriggerAddScatter_unhit
	'debug.print "BEFORE: velx=" & activeball.velx
	activeball.velx = activeball.velx*(rnd*0.5+0.5)
	'debug.print "AFTER: velx=" & activeball.velx 
End Sub



'****************
' Other Stuff :)
'****************
 
Sub FlipperTimer_Timer()
 
    dim PI:PI=3.1415926
    LFLogo.Rotz = LeftFlipper.CurrentAngle
    LFLogo1.Rotz = LeftFlipper1.CurrentAngle
    RFLogo.Rotz = RightFlipper.CurrentAngle
    RFLogo1.Rotz = RightFlipper1.CurrentAngle
    LFLogo2.Rotz = LeftFlipper.CurrentAngle
    LFLogo3.Rotz = LeftFlipper1.CurrentAngle
    RfLogo2.Rotz = RightFlipper.CurrentAngle
    RFLogo3.Rotz = RightFlipper1.CurrentAngle
	Pgate3.rotx = -Gate3.currentangle*0.5
	Pgate1.rotx = -Gate1.currentangle*0.5

	Dim SpinnerRadius: SpinnerRadius=7

	SpinnerRod.TransZ = (cos((sw35.CurrentAngle + 180) * (PI/180))+1) * SpinnerRadius
	SpinnerRod.TransY = sin((sw35.CurrentAngle) * (PI/180)) * -SpinnerRadius
	
	if FlipperShadows=1 then
		FlipperLSh.RotZ = LeftFlipper.currentangle
		FlipperRSh.RotZ = RightFlipper.currentangle
		FlipperLSh1.RotZ = LeftFlipper1.currentangle
		FlipperRSh1.RotZ = RightFlipper1.currentangle
	end if

	'drop target shadows
	if lgi.state=1 and sw2.isdropped=0 then dropplate1.visible=1
	if lgi.state=1 and sw2.isdropped=1 then dropplate1.visible=0
	if lgi.state=1 and sw12.isdropped=0 then dropplate2.visible=1
	if lgi.state=1 and sw12.isdropped=1 then dropplate2.visible=0
	if lgi.state=1 and sw22.isdropped=0 then dropplate3.visible=1
	if lgi.state=1 and sw22.isdropped=1 then dropplate3.visible=0
	if lgi.state=1 and sw32.isdropped=0 then dropplate4.visible=1
	if lgi.state=1 and sw32.isdropped=1 then dropplate4.visible=0
	if lgi.state=1 and sw53.isdropped=0 then dropplate5.visible=1
	if lgi.state=1 and sw53.isdropped=1 then dropplate5.visible=0
	if lgi.state=1 and sw33.isdropped=0 then dropplate6.visible=1
	if lgi.state=1 and sw33.isdropped=1 then dropplate6.visible=0
	if lgi.state=1 and sw24.isdropped=0 then dropplate7.visible=1
	if lgi.state=1 and sw24.isdropped=1 then dropplate7.visible=0
	if lgi.state=1 and sw14.isdropped=0 then dropplate8.visible=1
	if lgi.state=1 and sw14.isdropped=1 then dropplate8.visible=0
	if lgi.state=1 and sw4.isdropped=0 then dropplate9.visible=1
	if lgi.state=1 and sw4.isdropped=1 then dropplate9.visible=0

End Sub

'Finding an individual dip state based on scapino's Strikes and spares dip code - from unclewillys pinball pool, added another section to get high score award to set replay cards
Dim TheDips(32)
Sub FindDips
    Dim DipsNumber
    DipsNumber = Controller.Dip(1)
    TheDips(16) = Int(DipsNumber/128)
    If TheDips(16) = 1 then DipsNumber = DipsNumber - 128 end if
    TheDips(15) = Int(DipsNumber/64)
    If TheDips(15) = 1 then DipsNumber = DipsNumber - 64 end if
    TheDips(14) = Int(DipsNumber/32)
    If TheDips(14) = 1 then DipsNumber = DipsNumber - 32 end if
    TheDips(13) = Int(DipsNumber/16)
    If TheDips(13) = 1 then DipsNumber = DipsNumber - 16 end if
    TheDips(12) = Int(DipsNumber/8)
    If TheDips(12) = 1 then DipsNumber = DipsNumber - 8 end if
    TheDips(11) = Int(DipsNumber/4)
    If TheDips(11) = 1 then DipsNumber = DipsNumber - 4 end if
    TheDips(10) = Int(DipsNumber/2)
    If TheDips(10) = 1 then DipsNumber = DipsNumber - 2 end if
    TheDips(9) = Int(DipsNumber)
    DipsNumber = Controller.Dip(2)
    TheDips(24) = Int(DipsNumber/128)
    If TheDips(24) = 1 then DipsNumber = DipsNumber - 128 end if
    TheDips(23) = Int(DipsNumber/64)
    If TheDips(23) = 1 then DipsNumber = DipsNumber - 64 end if
    TheDips(22) = Int(DipsNumber/32)
    If TheDips(22) = 1 then DipsNumber = DipsNumber - 32 end if
    TheDips(21) = Int(DipsNumber/16)
    If TheDips(21) = 1 then DipsNumber = DipsNumber - 16 end if
    TheDips(20) = Int(DipsNumber/8)
    If TheDips(20) = 1 then DipsNumber = DipsNumber - 8 end if
    TheDips(19) = Int(DipsNumber/4)
    If TheDips(19) = 1 then DipsNumber = DipsNumber - 4 end if
    TheDips(18) = Int(DipsNumber/2)
    If TheDips(18) = 1 then DipsNumber = DipsNumber - 2 end if
    TheDips(17) = Int(DipsNumber)
    DipsTimer.Enabled=1
End Sub
 
 Sub DipsTimer_Timer()
	dim hsaward, BPG

   hsaward = TheDips(23)
   BPG = TheDips(17)

   If BPG = 1 then
       instcard.imageA="InstCard3Balls"
     Else
       instcard.imageA="InstCard5Balls"
   End if
   replaycard.imageA="replaycard"&hsaward
    DipsTimer.enabled=0
 End Sub
 
'Gottlieb YF
'added by Inkochnito
Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700,400,"YF - DIP switches"
		.AddFrame 2,10,190,"Maximum credits",49152,Array("8 credits",0,"10 credits",32768,"15 credits",&H00004000,"25 credits",49152)'dip 15&16
		.AddFrame 2,86,190,"Coin chute 1 and 2 control",&H00002000,Array("seperate",0,"same",&H00002000)'dip 14
		.AddFrame 2,132,190,"Playfield special",&H00200000,Array("replay",0,"extra ball",&H00200000)'dip 22
		.AddFrame 2,178,190,"Tilt penalty",&H10000000,Array("game over",0,"ball in play only",&H10000000)'dip29
		.AddFrame 2,224,190,"Game mode",&H00100000,Array("replay",0,"extra ball",&H00100000)'dip 21
		.AddFrameExtra 2,270,190,"Sound option",&H0100,Array("continuous sound",0,"scoring sounds only",&H0100)'S-board dip 1
		.AddChk 2,320,190,Array("Background tone",&H80000000)'dip 32
		'.AddChk 2,335,400,Array("dip 31 must be off",&H40000000)'dip 31
		.AddChk 2,335,190,Array("Must remain on",&H01000000)'dip 25
		.AddChk 2,350,190,Array("Must remain on",&H02000000)'dip 26
		.AddChk 2,365,190,Array("Coin switch tune",&H04000000)'dip 27
		.AddFrame 205,10,190,"High score to date awards",&H00C00000,Array("not displayed and no award",0,"displayed and no award",&H00800000,"displayed and 2 replays",&H00400000,"displayed and 3 replays",&H00C00000)'dip 23&24
		.AddFrame 205,86,190,"Balls per game",&H00010000,Array("5 balls",0,"3 balls",&H00010000)'dip 17
		.AddFrame 205,132,190,"Replay limit",&H00040000,Array("no limit",0,"one per game",&H00040000)'dip 19
		.AddFrame 205,178,190,"Novelty",&H00080000,Array("normal",0,"extra ball and replay scores 50,000",&H00080000)'dip 20
		.AddFrame 205,224,190,"center coin chute credits control",&H00001000,Array("no effect",0,"add 9",&H00001000)'dip 13
		.AddFrameExtra 205,270,190,"Attract tune",&H0200,Array("no attract tune",0,"attract tune played every 6 minutes",&H0200)'S-board dip 2
		.AddChk 205,320,190,Array("Attract features",&H20000000)'dip 30
		.AddChk 205,335,190,Array("Match feature",&H00020000)'dip 18
		.AddChk 205,350,190,Array("Credits displayed",&H08000000)'dip 28
		.AddLabel 50,380,300,20,"After hitting OK, press F3 to reset game with new settings."
	End With
	Dim extra
	extra = Controller.Dip(4) + Controller.Dip(5)*256
	extra = vpmDips.ViewDipsExtra(extra)
	Controller.Dip(4) = extra And 255
	Controller.Dip(5) = (extra And 65280)\256 And 255
End Sub
Set vpmShowDips = GetRef("editDips")


'******************************************************
'			TROUGH BASED ON cyberpez' Black Hole
'******************************************************

Sub sw25_Hit(): Controller.Switch(25) = 1: UpdateTrough:End Sub
Sub sw25_UnHit(): Controller.Switch(25) = 0: End Sub

Sub Trough2_Hit(): UpdateTrough:End Sub
Sub Trough1_Hit(): UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	if Trough1.ballCntOver = 0 then Trough2.kick 70,5
	if Trough2.ballCntOver = 0 then sw25.kick 70,5
	Me.Enabled = 0
End Sub


'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub sw15_Hit()
    RandomSoundDrain sw15
    select case Int(rnd*17 + 1)
        case 1: PlaySound "Drain01"
        case 2: PlaySound "Drain02"
        case 3: PlaySound "Drain03"
        case 4: PlaySound "Drain04"
        case 5: PlaySound "Drain05"
        case 6: PlaySound "Drain06"
        case 7: PlaySound "Drain07"
        case 8: PlaySound "Drain08"
        case 9: PlaySound "Drain09"
        case 10: PlaySound "Drain10"
        case 11: PlaySound "Drain11"
        case 12: PlaySound "Drain12"
        case 13: PlaySound "Drain13"
        case 14: PlaySound "Drain14"
        case 15: PlaySound "Drain15"
        case 16: PlaySound "Drain16"
        case 17: PlaySound "Drain17"
    End Select
    Controller.Switch(15) = 1

'Divertor stuff..
Divtime = 0
End Sub



Sub SolOutHole(enabled)
	If enabled Then 
		sw15.kick 70,40
		PlaySoundAt SoundFX("fx_Solenoid",DOFContactors), sw15
		Controller.Switch(15) = 0
	End If
End Sub


Set LampCallback=GetRef("UpdateMultipleLamps")

Sub UpdateMultipleLamps
  if startGame.enabled=0 then

    if controller.lamp(12) then
		Trough1.kick 60, 7
		' Hauntfreaks change to this if ball eject is too loud
		'PlaySoundat SoundFX("ballrelease",DOFContactors), Trough1
		'RandomSoundBallRelease Trough1
		UpdateTrough
    end if

    if controller.lamp(13) and sw5.BallCntOver > 0 then
		LSaucer
    end if

	If Controller.Lamp(11) Then
		if not GameOver.Enabled Then
			if gameNumber > 0 Then
				GameOver.Enabled = true
			End If
		End If
	End If

    If Controller.Lamp(11) Then		'Game Over triggers match and BIP
		if not b2son then game_over.visible = 1		'DT
        BGGameOver.visible = 1   'VR BG
'GIOff
      Else
'GIOn
		game_over.visible = 0		'DT
        BGGameOver.visible = 0    'VR BG
    End If
 

    If Controller.Lamp(1)  Then					'Tilt
		if not b2son then tilt.visible = 1		'DT
        BGTilt.visible = 1  'VR BG
      Else
		tilt.visible = 0		'DT
        BGTilt.visible = 0   'VR BG
    End If
 

    If Controller.Lamp(10) Then				'HIGH SCORE TO DATE
		if not b2son then high_score.visible = 1		'DT
        BGHS.visible = 1   ' VR BG


	if StrobeRunning = false then
	GiOff
	end if
      Else
	if StrobeRunning = false then
	GiOn
	end if
		high_score.visible = 0		'DT    
        BGHS.visible = 0		' VR BG
    End If
  end if
End Sub
 
Dim Digits(32)
 
'Score displays
 
Digits(0)=Array(a1,a2,a3,a4,a5,a6,a7,n,a8)
Digits(1)=Array(a9,a10,a11,a12,a13,a14,a15,n,a16)
Digits(2)=Array(a17,a18,a19,a20,a21,a22,a23,n,a24)
Digits(3)=Array(a25,a26,a27,a28,a29,a30,a31,n,a32)
Digits(4)=Array(a33,a34,a35,a36,a37,a38,a39,n,a40)
Digits(5)=Array(a41,a42,a43,a44,a45,a46,a47,n,a48)
Digits(6)=Array(a49,a50,a51,a52,a53,a54,a55,n,a56)
Digits(7)=Array(a57,a58,a59,a60,a61,a62,a63,n,a64)
Digits(8)=Array(a65,a66,a67,a68,a69,a70,a71,n,a72)
Digits(9)=Array(a73,a74,a75,a76,a77,a78,a79,n,a80)
Digits(10)=Array(a81,a82,a83,a84,a85,a86,a87,n,a88)
Digits(11)=Array(a89,a90,a91,a92,a93,a94,a95,n,a96)
Digits(12)=Array(a97,a98,a99,a100,a101,a102,a103,n,a104)
Digits(13)=Array(a105,a106,a107,a108,a109,a110,a111,n,a112)
Digits(14)=Array(a113,a114,a115,a116,a117,a118,a119,n,a120)
Digits(15)=Array(a121,a122,a123,a124,a125,a126,a127,n,a128)
Digits(16)=Array(a129,a130,a131,a132,a133,a134,a135,n,a136)
Digits(17)=Array(a137,a138,a139,a140,a141,a142,a143,n,a144)
Digits(18)=Array(a145,a146,a147,a148,a149,a150,a151,n,a152)
Digits(19)=Array(a153,a154,a155,a156,a157,a158,a159,n,a160)
Digits(20)=Array(a161,a162,a163,a164,a165,a166,a167,n,a168)
Digits(21)=Array(a169,a170,a171,a172,a173,a174,a175,n,a176)
Digits(22)=Array(a177,a178,a179,a180,a181,a182,a183,n,a184)
Digits(23)=Array(a185,a186,a187,a188,a189,a190,a191,n,a192)
 
'Ball in Play and Credit displays
 
Digits(26)=Array(e00,e01,e02,e03,e04,e05,e06,n)
Digits(27)=Array(e10,e11,e12,e13,e14,e15,e16,n)
Digits(24)=Array(f00,f01,f02,f03,f04,f05,f06,n)
Digits(25)=Array(f10,f11,f12,f13,f14,f15,f16,n)


Sub DisplayTimer_Timer
	If VRRoom = 0 Then
		Dim ChgLED,ii,num,chg,stat, obj
		ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
' 	      If not b2son Then
			For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
				if (num < 28 ) then
					For Each obj In Digits(num)
						If chg And 1 Then obj.State = stat And 1
						chg = chg\2 : stat = stat\2
					Next
				else
				end if
			next
		End if
    End if
End Sub



Const tnob = 15 ' total number of balls
ReDim rolling(tnob)

InitRolling

Dim DropCount
ReDim DropCount(tnob)

Dim ampFactor

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
	Select Case BallRollAmpFactor
		Case 0
			ampFactor = "_amp0"
		Case 1
			ampFactor = "_amp2_5"
		Case 2
			ampFactor = "_amp5"
		Case 3
			ampFactor = "_amp7_5"
		Case 4
			ampFactor = "_amp9"
		Case Else
			ampFactor = "_amp0"
	End Select
End Sub

Sub RollingTimer_Timer()
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("BallRoll_" & b & ampFactor)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b & ampFactor), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b & ampFactor)
				rolling(b) = False
			End If
		End If

		'***Ball Drop Sounds***
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
	Next
End Sub


Sub LeftFlipper_Collide(parm)
    CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
    LeftFlipperCollide parm   'This is the Fleep code
End Sub

Sub LeftFlipper1_Collide(parm)
 	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
    CheckLiveCatch Activeball, RightFlipper, RFCount, parm
    RightFlipperCollide parm  'This is the Fleep code
End Sub

Sub RightFlipper1_Collide(parm)
 	LeftFlipperCollide parm
End Sub


'*****************************************
'           BALL SHADOW by ninnuzu
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)
 
Sub BallShadowUpdate_timer()
    Dim BOT, b
	Dim maxXoffset
	maxXoffset=15
    BOT = GetBalls

	' render the shadow for each ball
    For b = 0 to UBound(BOT)
		BallShadow(b).X = BOT(b).X-maxXoffset*(1-(Bot(b).X)/(YF.Width/2))
		BallShadow(b).Y = BOT(b).Y + 10
		If BOT(b).Z > 0 and BOT(b).Z < 30 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub


'******************************
' Setup VR Room
'******************************

Dim VRRoom: VRRoom = 0    ' 0 = Desktop/FS - 1 = VR Mode/FSS
Dim VRGlass: VRGlass = 0   ' 0 = Performance - 1 = shiny glass in VR   
Dim VRGlassScratches: VRGlassScratches = 0  '0 = Performance - 1 = scratched PF glass in VR 

' Load VR timers if VR room is on...
if VRRoom = 1 Then
VRCandleTimer.enabled = true
VRCandleTimer2.enabled = true
VRCandleWoodTimer.enabled = true
CandleSpinnerTimer.enabled = true
CandleBurntimer.enabled = true
IgorTimer.enabled = true
End If

Sub SetupVRRoom()

If VRRoom = 0 Then
BGVRMessage.visible = true  ' diplays message telling people to set VR  (in case they run it without and are confused)
MessageBacking.visible = true ' backing wall for the message so it can be seen porperly in VR (needed because of flasher depth bias on BG)

'turns off timers for VR lightning ball Topper
LightingLampTimer.enabled = false
LightingLampTimer2.enabled = false
LightingLampTimer3.enabled = false
LightingLampTimer4.enabled = false

for each Object in VRRoomStuff: object.visible = 0 : next  ' makes VR room objects unseen for dekstop and FS mode
End if 

If VRRoom = 1 Then  ' sets glass and scratches per user setting if VR room = 1
if VRGlassScratches = 1 Then
GlassImpurities.visible = True  
end If
if VRGlass = 1 Then
WindowGlass.visible = True
end If
end if
End Sub


'******************************
' Setup VR Backglass
'******************************

Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen

Sub setup_backglass()

	xoff = -20
	yoff = 133 ' this is where you adjust the forward/backward position for players scores	
	zoff = 699
	xrot = -90
	center_digits()
end sub

Sub center_digits()
	Dim ix, xx, yy, yfact, xfact, xobj

	zscale = 0.0000001
	xcen = (130 /2) - (92 / 2)
	ycen = (780 /2 ) + (203 /2)

	for ix = 0 to 23
		For Each xobj In DigitsVR(ix)

			xx = xobj.x  				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next
end sub

'**********************************************************
'            VR Display Output - Joel implemented - Thanks
'**********************************************************

dim DisplayColor
DisplayColor =  RGB(255,0,6) ' red

Sub DisplayTimerVR_Timer
	If VRRoom = 1 Then
		Dim ChgLED,ii,num,chg,stat, obj
		ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
'	        If not b2son Then
			For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
				if (num < 28 ) then
					For Each obj In DigitsVR(num)
						If chg And 1 Then FadeDisplay obj, stat And 1
						chg = chg\2 : stat = stat\2
					Next
				else
				end if
			next
        End if
    End if


' This runs the Backglass Lightning by watching light l14a on the playfield.
if l14a.state = 1 Then
BGLightning.visible = True
else
BGLightning.visible = false
end If
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

Dim DigitsVR(28)
DigitsVR(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6,led1x7,led1x8)
DigitsVR(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6,led2x7,led2x8)
DigitsVR(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6,led3x7,led3x8)
DigitsVR(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6,led4x7,led4x8)
DigitsVR(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6,led5x7,led5x8)
DigitsVR(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6,led6x7,led6x8)

DigitsVR(6) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6,led8x7,led8x8)
DigitsVR(7) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6,led9x7,led9x8)
DigitsVR(8) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6,led10x7,led10x8)
DigitsVR(9) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6,led11x7,led11x8)
DigitsVR(10) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6,led12x7,led12x8)
DigitsVR(11) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6,led13x7,led13x8)

DigitsVR(12) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006,LED1x007,LED1x008)
DigitsVR(13) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106,LED1x107,LED1x108)
DigitsVR(14) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206,LED1x207,LED1x208)
DigitsVR(15) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306,LED1x307,LED1x308)
DigitsVR(16) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406,LED1x407,LED1x408)
DigitsVR(17) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506,LED1x507,LED1x508)

DigitsVR(18) = Array(led2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006,led2x007,led2x008)
DigitsVR(19) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106,led2x107,led2x108)
DigitsVR(20) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206,led2x207,led2x208)
DigitsVR(21) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306,led2x307,led2x308)
DigitsVR(22) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406,led2x407,led2x408)
DigitsVR(23) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506,led2x507,led2x508)

'credit -- Ball In Play
DigitsVR(26) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306,LEDax307,LEDax308)
DigitsVR(27) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406,LEDbx407,LEDbx408)
DigitsVR(24) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506,LEDcx507,LEDcx508)
DigitsVR(25)= Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606,LEDdx607,LEDdx608)


Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(DigitsVR)
		if IsArray(DigitsVR(x) ) then
			For each obj in DigitsVR(x)
				obj.height = obj.height + 18
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

InitDigits

Sub StartButtonTimer_Timer()
if Primary_startbuttoninner.disableLighting = 0 then 
Primary_startbuttoninner.disableLighting = 1
else
Primary_startbuttoninner.disableLighting = 0
End if
End Sub


'********* Tesla Lightning Ball Code - Rawd **********
'*****************************************************

sub LightingLampTimer_Timer()  ' spins all 4 lightning bolts
l2.rotx = l2.rotx +1
l2.roty = l2.roty +0.7
l2.rotz = l2.rotz +0.8
l1.rotx = l1.rotx +1
l1.roty = l1.roty +0.6
l1.rotz = l1.rotz +0.9
l3.rotx = l3.rotx +1.5
l3.roty = l3.roty +0.5
l3.rotz = l3.rotz +0.85

l4.rotx = l4.rotx +17.5
l4.roty = l4.roty +15.5
l4.rotz = l4.rotz +16.85
End Sub

sub LightingLampTimer2_Timer()  ' These timers randomly make the lightning bolts visible at random times.
if l1.visible = false Then
l1.visible = True
ssGlassDome.visible = false  
ssGlassDomeBlue.visible = true   ' replaces glass dome with a dif one with dif materail to create glow effect, we have 3 glass domes total
Else
l1.visible = false
ssGlassDomeBlue.visible = false
ssGlassDome.visible = true
end if

Randomize (21): LightingLampTimer2.Interval = 50 + rnd(1)*500 'randomizes lightning timer time between 50ms and 550ms
End Sub

sub LightingLampTimer3_Timer()
if l2.visible = false Then
l2.visible = True
ssGlassDome.visible = false
ssGlassDomePurple.visible = true
Else
l2.visible = false
ssGlassDomePurple.visible = false
ssGlassDome.visible = true
end if
Randomize (6): LightingLampTimer3.Interval = 80 + rnd(1)*600  'randomizes lightning timer time between 80ms and 680ms
End Sub

sub LightingLampTimer4_Timer()
if l3.visible = false Then
l3.visible = True
ssGlassDome.visible = false
ssGlassDomeBlue.visible = true
Else
l3.visible = false
ssGlassDomeBlue.visible = false
ssGlassDome.visible = true
end if
Randomize (9): LightingLampTimer4.Interval = 20 + rnd(1)*700  'randomizes lightning timer time between 20ms and 720ms
End Sub

'******** End Tesla Ball code **********
'***************************************


' VR Plunger stuff below..........
Sub TimerVRPlunger_Timer
  If VRPlunger.Y < 2541 then    ' This is the furthest we want the VR plunger to extend.
       VRPlunger.Y = VRPlunger.Y + 5
  End If
End Sub
Sub TimerVRPlunger2_Timer
  VRPlunger.Y = 2416 + (5* Plunger.Position) -20   '2416 is the plungers Y position sitting.
End Sub

' set this up so that the Lightning on the BG is not on at game start..  timed with GI turning on...
Sub DisplayTimerVRStartTimer_timer()
DisplayTimerVR.enabled = true
DisplayTimerVRStartTimer.enabled = false  ' turns itself off.
end sub


'********  Burning Candle - Rawd ********
'****************************************

Dim VRFPCounter
Dim VRCandleCounter
Dim VRCandlewoodCounter
VRFPCounter = 1
VRCandleCounter = 1
VRCandlewoodCounter = 1

Sub VRCandleTimer_Timer()  ' Animates back wall lighting
	VR_Wall_Left.Image = "NewWD" & VRFPCounter
	VRFPCounter = VRFPCounter + 1
	If VRFPCounter > 7 Then
		VRFPCounter = Int(rnd*7 + 1)
	End If
End Sub

Sub VRCandleTimer2_Timer()  ' Animates the flame
	CandleFlasher.ImageA = "Fire_" & VRCandleCounter
	VRCandleCounter = VRCandleCounter + 1
	If VRCandleCounter > 59 Then
		VRCandleCounter = 1
	End If
End Sub

Sub VRCandleWoodTimer_Timer() ' animates the wood lighting
	CandleWood1.Image = "WLD" & VRCandlewoodCounter
    CandleWood2.Image = "WLD" & VRCandlewoodCounter
	VRCandlewoodCounter = VRCandlewoodCounter + 1
	If VRCandlewoodCounter > 4 Then
		VRCandlewoodCounter = Int(rnd*4 + 1) '1
	End If
End Sub

Sub CandleSpinnerTimer_timer() ' Spins the fire fast and randomizes opacity 
CandleFlasher.roty = CandleFlasher.roty + Int(rnd*100 + 1) '+100
CandleFlasher.opacity = Int(rnd*200 + 1)
end sub

Sub CandleBurntimer_timer()   ' moves the candle and flasher down by sizing it and moving it on its VP slope
if Candletop.z < 727 then

CandleTop.Size_Y = CandleTop.Size_Y -.1
CandleTop.z = CandleTop.z + .3
CandleTop.y = CandleTop.y - 0.03
CandleFlasher.height = CandleFlasher.height - .86
CandleFlasher.y= CandleFlasher.y + 0.092

Else
CandleSpinnerTimer.enabled = False   ' If the candle reaches the bottom, we disable these timers and start fading the fire texture
CandleFlasher.opacity = 200
CandleBurnoutTimer.enabled = true
CandleBurntimer.enabled = False  ' this must be last - turning itself off
end if
End Sub

Sub CandleBurnoutTimer_timer()  'fades the fire slowly while dropping it into the candle and fading it..
CandleFlasher.roty = CandleFlasher.roty + Int(rnd*100 + 1) '+100
CandleFlasher.opacity = CandleFlasher.opacity -0.51
CandleFlasher.height = CandleFlasher.height - 0.3

if CandleFlasher.opacity =<0 Then   ' If the candle has burned out, we weill stop all the lighting animations and start the smoke timer.
VR_Wall_Left.image = "NewLeftWallDark"
VRCandleWoodTimer.enabled = False
VRCandleTimer2.enabled = False
VRCandleTimer.enabled = False
CandleSmokeTimer.enabled = true '  starts the smoke
CandleSmokeTimer2.enabled = true '  starts the smoke
Smoke1.visible = true
Smoke2.visible = true
CandleWood1.image = "WLD1" ' sets the wood to the darkest texture.,
CandleWood2.image = "WLD1" ' sets the wood to the darkest texture.,
CandleBurnoutTimer.enabled = False   ' turns itself off
end if
end sub

Sub CandleSmokeTimer_timer()  ' Raises 2 smoke textures
Smoke2.height = smoke2.height + 1
Smoke1.height = smoke1.height + 0.5
End sub

Sub CandleSmokeTimer2_timer()   ' had to make a seperate timer or the opacity wouldnt work. I have no idea why not.
Smoke2.opacity = Smoke2.opacity - 0.59 ' only 0.6 or higher works now?  so confused.  WHY????
Smoke1.opacity = Smoke1.opacity - 0.51

if Smoke1.opacity =<0  then   ' When the smoke is gone, turn off last timers

CandleWood2.image = "WoodText" ' sets the wood with the magnasave message
BookshelfTimer.enabled = True   ' Starts bookshelf spin animated 540 degrees and stops halfway.
IgorTimer2.enabled = True ' turns on Igor Model animation
BoltTimer.enabled = True ' turns on lightning bolt on VR Jacobs ladder
BrainTimer.enabled = True ' turns on Brain animation
CandleSmokeTimer.enabled = false ' turns off Candlesmoke timer
CandleSmokeTimer2.enabled = false  ' turns itself off
end if
End sub

'********  Burning End - Rawd ********
'****************************************

Dim CandleResetReady
CandleResetReady = false
Dim Degrees
Degrees = 0

sub BookshelfTimer_timer()

If Degrees < 540 then
Books.roty = Books.roty +0.5
Shelf.roty = shelf.roty +0.5
Degrees = Degrees +1
end If

If Degrees = 540 Then
Playsound "Candle"
Degrees = 0   
CandleResetReady = true   ' sets up candle reset
BookshelfTimer.enabled = false ' disable self
end If
End sub


sub BookshelfTimer2_timer()

If Degrees < 180 then
Books.roty = Books.roty +0.5
Shelf.roty = shelf.roty +0.5
Degrees = Degrees +1

If Degrees = 180 Then
Degrees = 0   
IgorTimer2.enabled = false ' turns off Igor Model animation
BoltTimer.enabled = False
BrainTimer.enabled = False
BookshelfTimer2.enabled = false ' disable self
end If
end If
End Sub


Sub IgorTimer_timer()
if Igor.ImageA = "Igor3" Then
Igor.ImageA = "Igor2"
Else
Igor.ImageA = "Igor3"
end if
IgorTimer.Interval = Int(rnd*7000 + 1) 'randomizes time between 0 and 7 seconds
End sub

Dim Armspeed
ArmSpeed = -0.25

Sub IgorTimer2_timer()
IgorModel.roty = IgorModel.roty + ArmSpeed
IgorLight.y = IgorLight.y + Armspeed*8
IgorShadow.rotz = IgorShadow.rotz + ArmSpeed

if IgorModel.roty = 250 then Armspeed = 0.125
if IgorModel.roty = 320 then Armspeed = -0.125
End sub


Sub BoltTimer_timer()
Bolt.height = Bolt. Height +10
BoltGlow.height = BoltGlow.height +10

If Bolt.Height = 1200 then 
Bolt.height = 750
BoltGlow.height = 750
end If

if Bolt.ImageA = "l4a" Then
Bolt.ImageA = "l4b"
Bolt.ImageB = "l4b"
Else
Bolt.ImageA = "l4a"
Bolt.ImageB = "l4a"
end if
BoltTimer.Interval = Int(rnd*75 + 1) 'randomizes time 
End Sub

Dim BrainSpeed
BrainSpeed = 1
Dim BrainSpeed2
BrainSpeed2 = -0.2

Sub BrainTimer_timer()
Brain.Size_Y = Brain.Size_Y + BrainSpeed
Brain.Size_X = Brain.Size_X + BrainSpeed
Brain.Size_Z = Brain.Size_Z + BrainSpeed

If Brain.Size_Y = 385 then BrainSpeed =   1
If Brain.Size_Y = 451 then BrainSpeed = - 1

Brain.Roty = Brain.Roty + 0.2
Brain.z = Brain.z + BrainSpeed2

if Brain.z > 550 then BrainSpeed2 = -0.2
if Brain.z < 519 then BrainSpeed2 = 0.2
End Sub


Dim ClicksoundOn
ClickSoundOn = 0

Sub GIOn()

    dim xx
    LampTimer.enabled=1
    For each xx in GI:xx.State = 1: Next        '*****GI Lights On

	If ClickSoundOn = 0 then Playsound "FX_SolenoidSoft":ClickSoundOn = 1
	LFLogo.image="Gottflip_greyL"
	RFLogo.image="Gottflip_greyR"
	LFLogo1.image="Gottflip_greyL"
	RFLogo1.image="Gottflip_greyR"
	BumperBase1.image="YF_Bumper"
	BumperCap1.image="YF_BumperCap"
	BumperBase2.image="YF_Bumper"
	BumperCap2.image="YF_BumperCap"
	BumperBase3.image="YF_Bumper"
	BumperCap3.image="YF_BumperCap"
	BumperBase4.image="YF_Bumper"
	BumperCap4.image="YF_BumperCap"
	plastics_1.image="YF_Plastics"
	plastics_2.image="YF_Plastics"
	plastics_4.image="YF_Plastics"
	plastics_5.image="YF_Plastics"
	plastics_6.image="YF_Plastics"
	plastics_7.image="YF_Plastics"
	plastics_8.image="YF_Plastics"
	VRBackWall.Sideimage= "backwall"
	metalRamp.image ="GIramp_2"
	wireramps.image ="wire_ramp_1"
	Psw34Shield.image ="center_target_brain"
	apron.image ="YF_Apron"
	shadowsGIOFF.visible =0
	shadowsGION.visible =1
	BGLit.visible = 1
		Dim a_rubbers_on
	for each a_rubbers_on in a_rubbers: a_rubbers_on.image = "": next
		Dim a_posts_on
	for each a_posts_on in a_posts: a_posts_on.image = "": next
		Dim a_DropTargets_on
	for each a_DropTargets_on in a_DropTargets: a_DropTargets_on.image = "DT_YF_target": next

    Dim a_Nuts_on
	for each a_Nuts_on in Nuts: a_Nuts_on.image = "": next
end sub



Sub GIOff()

    dim xx
    'LampTimer.enabled=1
    For each xx in GI:xx.State = 0: Next        '*****GI Lights On

	If ClickSoundOn = 1 then Playsound "FX_SolenoidSoft":ClickSoundOn = 0
	LFLogo.image="Gottflip_greyL2"
	RFLogo.image="Gottflip_greyR2"
	LFLogo1.image="Gottflip_greyL2"
	RFLogo1.image="Gottflip_greyR2"
	BumperBase1.image="YF_Bumper2a"
	BumperCap1.image="YF_BumperCap2a"
	BumperBase2.image="YF_Bumper2a"
	BumperCap2.image="YF_BumperCap2a"
	BumperBase3.image="YF_Bumper2a"
	BumperCap3.image="YF_BumperCap2a"
	BumperBase4.image="YF_Bumper2a"
	BumperCap4.image="YF_BumperCap2a"
	plastics_1.image="Plastics_off"
	plastics_2.image="Plastics_off"
	plastics_4.image="Plastics_off"
	plastics_5.image="Plastics_off"
	plastics_6.image="Plastics_off"
	plastics_7.image="Plastics_off"
	plastics_8.image="Plastics_off"
	VRBackWall.Sideimage = "backwall2"
	metalRamp.image = "GIramp_1"
	wireramps.image = "wire_ramp_0"
	Psw34Shield.image = "center_target_brain2"
	apron.image ="YF_Apron_off"
	shadowsGIOFF.visible = 1
	shadowsGION.visible = 0
	BGLit.visible = 0
		Dim a_rubbers_on
	for each a_rubbers_on in a_rubbers: a_rubbers_on.image = "rubbers_off": next
		Dim a_posts_on
	for each a_posts_on in a_posts: a_posts_on.image = "rubbers_off": next
		Dim a_DropTargets_on
	for each a_DropTargets_on in a_DropTargets: a_DropTargets_on.image = "DT_YF_target2": next

    Dim a_Nuts_on
	for each a_Nuts_on in Nuts: a_Nuts_on.image = "rubbers_off": next

end sub

Dim StrobeRunning 'This is a variable to know when our strobe light is running 
	StrobeRunning = False


Dim Strobe
Strobe = 0
Dim StrobeOn
StrobeOn = False

sub GIStrobeTimer_Timer()
Strobe = Strobe +1
If StrobeOn = False then 
GIOn
StrobeOn = True
Else
GIOff
StrobeOn = False
end if

If Strobe = 51 Then
Strobe = 0
StrobeOn = False
GIStrobetimer.enabled = false
StrobeRunning = false
end if
End Sub

Dim HatchPoints
HatchPoints = 0
Sub HatchPointsTimer_Timer()
Hatchpoints = HatchPoints + 1
vpmTimer.PulseSw 51 

if Hatchpoints = 10 then ' 5,000 points  (500 x 10, and the HatchPoints timer is set to 220ms..  matches up with the Strobe turning off)
HatchPointsTimer.enabled = false 
HatchPoints = 0
end if
End Sub

'********  outlane divertor lightning - Rawd ********
'****************************************
Dim DivTime
DivTime = 0

Sub DivertorTrigger_hit()

DivTime = DivTime + 15   'seconds

Divertor.isDropped = false 
DivertorTimer.enabled = true 

DivGlow.visible = true
DivBolt.visible = true
DivBoltTimer.enabled = true
end sub  

Sub DivertorTimer_timer()
if Divtime >0 then
DivTime = DivTime - 1 ' seconds

end if

if DivTime = 0 then
Divertor.isDropped = true
DivertorTimer.enabled = false
DivGlow.visible = false
DivBolt.visible = false
DivBoltTimer.enabled = false
end If
if Divtime =>60 then Divtime = 59
End Sub


sub DigitsTimer_timer()

if Divtime < 10 then
Digit2.imageA = "D" & DivTime
Digit2.imageB = "D" & DivTime
Digit1.imageA = "D0"
Digit1.imageB = "D0"
end If

if Divtime => 10 then
Digit2.imageA = "D" & DivTime - 10
Digit2.imageB = "D" & DivTime - 10
Digit1.imageA = "D1"
Digit1.imageB = "D1"
end If

if Divtime => 20 then
Digit2.imageA = "D" & DivTime - 20
Digit2.imageB = "D" & DivTime - 20
Digit1.imageA = "D2"
Digit1.imageB = "D2"
end If

if Divtime => 30 then
Digit2.imageA = "D" & DivTime - 30
Digit2.imageB = "D" & DivTime - 30
Digit1.imageA = "D3"
Digit1.imageB = "D3"
end If

if Divtime => 40 then
Digit2.imageA = "D" & DivTime - 40
Digit2.imageB = "D" & DivTime - 40
Digit1.imageA = "D4"
Digit1.imageB = "D4"
end If

if Divtime => 50 then
Digit2.imageA = "D" & DivTime - 50
Digit2.imageB = "D" & DivTime - 50
Digit1.imageA = "D5"
Digit1.imageB = "D5"
end If

if Divtime => 60 then
Digit2.imageA = "D0" 
Digit2.imageB = "D0"
Digit1.imageA = "D6"
Digit1.imageB = "D6"
end If
end Sub

Sub DivBoltTimer_Timer()
DivGlow.opacity = 200
if DivBolt.ImageA = "l4b" Then
DivBolt.ImageA = "l4a"
DivBolt.ImageB = "l4a"
Else
DivBolt.ImageA = "l4b"
DivBolt.ImageB = "l4b"
end if
DivBoltTimer.Interval = Int(rnd*175 + 1) 'randomizes time 
End Sub

Divertor.isdropped = true  'drops outalane diverter on game load.

Sub Divertor_hit()
'PlaySound "ElectricShock"
PlaySoundAt "ElectricShock", DiverterPrim 
DivGlow.opacity = 400
End Sub

sub VUKstrobe_hit()
startB2S(80)
GiOff  ' Turns off GI  (sub below)
GIStrobeTimer.enabled = true  ' Starts Strobe timer.  The strobe timer runs to Storbe = 51 and counts along the way
StrobeRunning = true  'This variable was made so that our strobe didnt interfere with Controller10 sub
        PlaySoundAtVol SoundFX("fx_vukExit_wire",DOFContactors), VUKstrobe, 1
    select case Int(rnd*8 + 1)
        case 1: PlaySound "VUK01"
        case 2: PlaySound "VUK02"
        case 3: PlaySound "VUK03"
        case 4: PlaySound "VUK04"
        case 5: PlaySound "VUK05"
        case 6: PlaySound "VUK06"
        case 7: PlaySound "VUK07"
        case 8: PlaySound "VUK08"
    End Select
End Sub

sub GIOnTrigger_hit()
GiOn
End Sub


dim LF  : Set LF = New FlipperPolarity
dim RF  : Set RF = New FlipperPolarity
dim LF1  : Set LF1 = New FlipperPolarity
dim RF1  : Set RF1 = New FlipperPolarity

InitPolarity

Sub InitPolarity()
        dim x, a : a = Array(LF, LF1, RF, RF1)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 80
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -2.7        
        AddPt "Polarity", 2, 0.33, -2.7
        AddPt "Polarity", 3, 0.37, -2.7        
        AddPt "Polarity", 4, 0.41, -2.7
        AddPt "Polarity", 5, 0.45, -2.7
        AddPt "Polarity", 6, 0.576,-2.7
        AddPt "Polarity", 7, 0.66, -1.8
        AddPt "Polarity", 8, 0.743, -0.5
        AddPt "Polarity", 9, 0.81, -0.5
        AddPt "Polarity", 10, 0.88, 0

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
        LF1.Object = LeftFlipper1
        LF1.EndPoint = EndPointLp1
        RF1.Object = RightFlipper1
        RF1.EndPoint = EndPointRp1
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub
Sub TriggerLF1_Hit() : LF1.Addball activeball : End Sub
Sub TriggerLF1_UnHit() : LF1.PolarityCorrect activeball : End Sub
Sub TriggerRF1_Hit() : RF1.Addball activeball : End Sub
Sub TriggerRF1_UnHit() : RF1.PolarityCorrect activeball : End Sub



'******************************************************
'                        FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
        dim a : a = Array(LF, LF1, RF, RF1)
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
         :                         VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

                                if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

                                if Enabled then aBall.Velx = aBall.Velx*VelCoef
                                if Enabled then aBall.Vely = aBall.Vely*VelCoef
                        End If

                        'Polarity Correction (optional now)
                        if not IsEmpty(PolarityIn(0) ) then
                                If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
                                dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
        
                                if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
                                'playsound "fx_knocker"
                        End If
                End If
                RemoveBall aBall
        End Sub
End Class

'******************************************************
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
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
'                        FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
        FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
        FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
        FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
        FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
        FlipperTricks LeftFlipper1, LF1Press, LF1Count, LF1EndAngle, LF1State
        FlipperTricks RightFlipper1, RF1Press, RF1Count, RF1EndAngle, RF1State
        FlipperNudge RightFlipper1, RF1EndAngle, RF1EOSNudge, LeftFlipper1, LF1EndAngle
        FlipperNudge LeftFlipper1, LF1EndAngle, LF1EOSNudge,  RightFlipper1, RF1EndAngle
end sub

Dim LFEOSNudge, RFEOSNudge
Dim LF1EOSNudge, RF1EOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
        Dim BOT, b

        If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
                EOSNudge1 = 1
                'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
                If Flipper2.currentangle = EndAngle2 Then 
                        BOT = GetBalls
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
                                        'Debug.Print "ball in flip1. exit"
                                         exit Sub
                                end If
                        Next
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
                                        BOT(b).velx = BOT(b).velx / 1.7
                                        BOT(b).vely = BOT(b).vely - 1
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

Function dSin(degrees)
        dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
        dcos = cos(degrees * Pi/180)
End Function

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
dim LF1Press, RF1Press, LF1Count, RF1Count
dim LFState, RFState
dim LF1State, RF1State
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle, RF1EndAngle, LF1EndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 1 
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
Const EOSReturn = 0.045

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
LF1EndAngle = Leftflipper1.endangle
RF1EndAngle = RightFlipper1.endangle

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
        End If
End Sub


'****************************************************************************
'PHYSICS DAMPENERS
'****************************************************************************

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


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
RubbersD.addpoint 0, 0, 0.96        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

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


        Public Sub Report()         'debug, reports all coords in tbPL.text
                if not debugOn then exit sub
                dim a1, a2 : a1 = ModIn : a2 = ModOut
                dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
                TBPout.text = str
        End Sub
        

End Class


'******************************************************
'                TRACK ALL BALL VELOCITIES
'                 FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
        public ballvel, ballvelx, ballvely

        Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

        Public Sub Update()        'tracks in-ball-velocity
                dim str, b, AllBalls, highestID : allBalls = getballs

                for each b in allballs
                        if b.id >= HighestID then highestID = b.id
                Next

                if uBound(ballvel) < highestID then redim ballvel(highestID)        'set bounds
                if uBound(ballvelx) < highestID then redim ballvelx(highestID)        'set bounds
                if uBound(ballvely) < highestID then redim ballvely(highestID)        'set bounds

                for each b in allballs
                        ballvel(b.id) = BallSpeed(b)
                        ballvelx(b.id) = b.velx
                        ballvely(b.id) = b.vely
                Next
        End Sub
End Class

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
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

Sub RDampen_Timer()
Cor.Update
End Sub

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
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
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
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************

Dim tablewidth, tableheight : tablewidth = YF.width : tableheight = YF.height

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

Function RndNum(min, max)
    RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

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
	TargetBouncer Activeball, 1
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


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled <> 0 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.1*defvalue
			Case 2: zMultiplier = 0.2*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.5*defvalue
            Case 6: zMultiplier = 0.6*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
    end if
end sub

'******************************************************
'**** RAMP ROLLING SFX
'******************************************************

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
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
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


Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function


