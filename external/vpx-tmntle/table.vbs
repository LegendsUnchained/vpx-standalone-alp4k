Option Explicit
Randomize

'*****************************************************************************************************
' Teenage Mutant Ninja Turtles REMIX
' IPDB No. 2509 / Data East: May, 1991 / Stern: June 2020 / 4 Players
' Version 2.0 by Bigus / Cheese3075 (Nov 2024)
' Designed for VPX 10.7
' Original table by pattyg234 in 2021
'*****************************************************************************************************
'v2.1: 20241121 redbone added:	new krang model and texture,  1 second pause after ball enters portal via KickerX, upscaled and color halftone to playfield, replaced wood texture on playfield
'							new portal graphic, upscaled bumper cap graphic
'v2.2: 20241122 redbone modified:  took a step back and went over the PF again and replaced many of the insert graphics.  cleaned up edges of pf.  Did the whole upscale again and cleaned what I could.  Porperly scaled PF to the correct dimensions.  It's not smushed anymroe yay!
'v2.3: 20241126 redbone - more playfield work  fixed some inserts.  tweaked some insert intensity.  added wire ramp in plunger lane that was missing.
'v2.4: 20241127 redbone - lots of changes to plastics.  triangle plastic in the middle was clipping into ramps.  fixed sling rubbers to fit the pegs properly.  changed exit on right for left loop shot.  ball now hits right flipper and bounces left and back around.  
'						Don't know how it should be, but the flow is better to me.
'v2.5->2.7 updates by Cheese
'Added new April, Lair and dimension x images to targets.  Added missing target next to pop bumpers.  Replaced spinner and all remaining targets to images used by Stern.  Hand drew billboard, left playfield inside lair loop, area around sling rubbers and wood grain around pegs/targets/rubbers.30+ hours of image cleanup of playfield and plastics.
'v2.7->v2.81  20241203 redbone - moved pegs, realigned rubbers, removed extra pegs, realigned some plastics, raised turtles on slings and the middle face up on risers of sorts - added plastic nuts to the tops, redrew some plastics, trimmed out some plastic graphics, properly aligned slings (they were way off),  playfield/table/bumper/sling/flipper physic adjustments according to generally accepted current recommendations, and several other things I can't remember.
'v2.81 redbone - adjusted plunger and fixed back wall so the ball could make the full look off the plunge.
'v2.83 redbone - adjusted main flashers to incadescent and fade up/down.  recolored flippers to be closert to stern.
'v2.85 redbone - worked on ball transition on left and right lanes to flippers.  sized flippers according to other stern tables I looked at for the time period ie: GB
'v2.86 cheese - Fixed Mystery X Flasher & added alternative location for it in the table script.  You now have the choice of apron (default) or on the desktop backdrop (this location will not work with CABs).  See table options section, at or near line 176.
'				Added two ball variations: ball_shell and ball_shell_brighter.  Select them from table editor > Properties > Ball image.  Default is ball_HDR. 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

If Table1.ShowDT = false then Ramp1.Visible = 0:Ramp2.Visible = 0



Dim EnableBallControl
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

'Const BallSize = 25  'Ball radius
' ===============================================================================================
' some general constants and variables
' ===============================================================================================
	
Const UseSolenoids 		= 2
Const UseLamps 			= False
Const UseGI 			= True
Const UseSync 			= False
Const HandleMech 		= False

Const SSolenoidOn 		= "SOL_on"
Const SSolenoidOff 		= "SOL_off"
Const SCoin 			= "Coin"
Const SKnocker 			= "Knocker"

'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = true         ' set to TRUE to enable Pup.   Download and move to proper folder on PC.
cPuPPack = "tmnt_104"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup Player

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub

Sub pupevent(EventNum)
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "W"&EventNum,1  'send event to Pup-Pack
End Sub

'************ PuP-Pack Startup **************

PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack


'******************************************************
'* ROM VERSION ****************************************
'******************************************************
' ball size
Const BallRadius = 25
Const BallMass = 1.1
Const cGameName = "tmnt_104"
Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim  UseVPMDMD
'If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode
UseVPMDMD= True 


LoadVPM "01560000", "DE.VBS", 3.26



' ===============================================================================================
' solenoids
' ===============================================================================================


SolCallback(sLLFlipper) = "solLFlipper"
SolCallback(sLRFlipper) = "solRFlipper"

Solcallback(1)  ="kisort"
SolCallback(2)	= "KickBallToLane"

SolCallback(4)	= "SolAutofire"			
'SolCallBack(5)	= "SetLamp5,"
'SolCallBack(6)	= "SetLamp6,"
SolCallback(7)	= "SewerUpKick"
'SolCallback(8)	= "vpmSolSound ""knocker"","
'SolCallBack(9)	= "SetLamp 109,"

SolCallback(11) = "SolGi" 'gi
SolCallBack(12)	= "SetLamp 112,"
SolCallBack(13)	= "SetLamp 113,"
SolCallBack(14) = "SetLamp 114,"
'SolCallBack(16)	= "SewerOpen"
'SolCallBack(17)			    = "vpmSolSound ""bumper2"","
'SolCallBack(18)			    = "vpmSolSound ""bumper2"","
'SolCallBack(19)			    = "TurtleB"
'SolCallback(20)             = "SolFlasher11"                 
'SolCallback(21)             = "SolFlasher12"
SolCallBack(22) ="SolPizzaSpin"


SolCallBack(25)  = "SetLamp 125,"
SolCallBack(26)  = "SetLamp 126,"
SolCallBack(27)  = "SetLamp 127,"
SolCallBack(28)  = "SetLamp 128,"
SolCallBack(29)  = "SetLamp 129,"
SolCallBack(30)  = "SetLamp 130,"
SolCallBack(31)  = "SetLamp 131,"
SolCallBack(32)  = "SetLamp 132,"



Dim  SplashInfoLine
Dim BlendDisableLighting
Dim ballmod
Dim Ball(6)
Dim InitTime
Dim TroughTime
Dim EjectTime
Dim MaxBalls
Dim TroughCount
Dim TroughBall(7)
Dim TroughEject
Dim Momentum
Dim UpperGIon
Dim Multiball
Dim BallsInPlay
Dim	iBall
Dim	fgBall
Dim AutoPlunger
Dim ttPizza


' Tabele Options

'///////////////////////-----VR Room-----///////////////////////
'Const VRRoom = 0 ' 0 - VR Room off, 1 - 360 Room, 2 - Minimal Room,3 -  ultra Minimal Room
Dim FlasherLocation

FlasherLocation=0    'Mystery flasher in apron
'FlasherLocation=1    'Mystery flasher in desktop backdrop (will NOT work in CABs, as this is off screen)



'Ball Mod     **Select them from table editor > Properties > Ball image **
'default: ball_HDR
'ball_shell
'ball_shell_brighter

BallMod = 0  'DO NOT CHANGE


'******************************************************
'******************************************************
'******************************************************
'* TABLE INIT *****************************************
'******************************************************
'******************************************************
'******************************************************




Sub InitVPM()
    With Controller
        .GameName = cGameName
'        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "TMNT" & vbNewLine & "Stern ReSkin"
        .Games(cGameName).Settings.Value("rol") = 0 'rotate DMD to the left
'        .HandleKeyboard = 0
'        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
	  If DesktopMode = true then .hidden = 0 Else .hidden = 0 End If
'        .Run GetPlayerHWnd
'       If Err Then MsgBox Err.Description
 '       On Error Goto 0
    End With
     Controller.SolMask(0) = 0
     vpmTimer.AddTimer 4000, "Controller.SolMask(0)=&Hffffffff'" 
     Controller.Run
End Sub



Sub table1_Init
	' table initialization
	InitVPM


'* PINMAME TIMER **************************************

PinMAMETimer.Interval = PinMAMEInterval
PinMAMETimer.Enabled = 1

' basic pinmame timer
	PinMAMETimer.Interval	= PinMAMEInterval
	PinMAMETimer.Enabled	= True

	' nudging
	vpmNudge.TiltSwitch		= 1
	vpmNudge.Sensitivity	= -10
'	vpmNudge.TiltObj 		= Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)
 

    ' Impulse Plunger
    Const IMPowerSetting = 75 'Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set AutoPlunger = New cvpmImpulseP
    With AutoPlunger
        .InitImpulseP AutoPlung, IMPowerSetting, IMTime
        .Random 0
        '.switch 18
        .InitExitSnd "solon", ""
        .CreateEvents "AutoPlunger"
    End With

''''''''''''''''''''''''''''''''''''''
	' spinning Pizza
''''''''''''''''''''''''''''''''''''''
	Set ttPizza = New cvpmTurnTable
	With ttPizza
		.InitTurnTable PizzaTrigger, 150
		.SpinCW = True
		.SpinUp = 200 : .SpinDown = 200
		.CreateEvents "ttPizza"
	End With

	CreatBalls

	vpmInit me


End Sub

'********************************************
'*************Mystery x location*************
'********************************************

If flasherLocation = 0 Then
	l43a.visible=True
	l44a.visible=True
	l45a.visible=True
	l46a.visible=True
	l47a.visible=True
	l48a.visible=True
	l43.visible=False
	l44.visible=False
	l45.visible=False
	l46.visible=False
	l47.visible=False
	l48.visible=False
Else

	l43.visible=True
	l44.visible=True
	l45.visible=True
	l46.visible=True
	l47.visible=True
	l48.visible=True
	l43a.visible=False
	l44a.visible=False
	l45a.visible=False
	l46a.visible=False
	l47a.visible=False
	l48a.visible=False
End If

'********************************************
'*************SHAKE**************************
'********************************************
Dim krangShake 

Sub PikachuShake()
    krangShake = 6
    krangTimer.Enabled = 1
End Sub

Sub krangTimer_Timer()
    krang.Transz = krangShake / 2
    If krangShake = 0 Then Me.Enabled = 0:Exit Sub
    If krangShake <0 Then
        krangShake = ABS(krangShake) - 0.1
    Else
        krangShake = - krangShake + 0.1
    End If
End Sub
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''  Ball Through system''''''''''''''''''''''''''
'''''''''''''''''''''by cyberpez''''''''''''''''''''''''''''''''
''''''''''''''''based off of EalaDubhSidhe's''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Dim BallCount
Dim cBall1, cBall2, cBall3, cBall4

dim bstatus

Sub CreatBalls()
	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1
	Set cBall1 = BallRelease.CreateSizedballWithMass(BallRadius,Ballmass)
	set cBall2 =BallRelease2.CreateSizedballWithMass(BallRadius,Ballmass)
	Set cBall3 = BallRelease3.CreateSizedballWithMass(BallRadius,Ballmass)
	'Set cBall4 = Kicker2.CreateSizedballWithMass(BallRadius,Ballmass)

	'If BallMod = 2 Then                 NOT USED, CODE NOT FINALIZED!  Set manually in table options.
	'	cBall1.Image = "ball_shell"	
	'	cBall2.Image = "ball_shell"	
	'	cBall3.Image = "ball_shell"	
	'	cBall4.Image = "ball_shell"
	'End If
	'Kicker5.Kick 0,1
	'Kicker5.enabled = false
End Sub

Sub BallRelease3_Hit():Controller.Switch(11) = 1:UpdateTrough:End Sub
Sub BallRelease3_UnHit():Controller.Switch(11) = 0:UpdateTrough:End Sub
Sub BallRelease2_Hit():Controller.Switch(12) = 1:UpdateTrough:End Sub
Sub BallRelease2_UnHit():Controller.Switch(12) = 0:UpdateTrough:End Sub
Sub BallRelease_Hit():Controller.Switch(13) = 1:UpdateTrough:End Sub
Sub BallRelease_UnHit():Controller.Switch(13) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	CheckBallStatus.Interval = 300
	CheckBallStatus.Enabled = 1
End Sub

Sub CheckBallStatus_timer()
	If BallRelease.BallCntOver = 0 Then BallRelease2.kick 60, 9
	If BallRelease2.BallCntOver = 0 Then BallRelease3.kick 60, 9
	Me.Enabled = 0


Dim BallReleaseactive, BallReleaseactive2, BallReleaseactive3, Kicker4active

'Dim TurtleBall

'Sub SetBallMod_hit()

	'If BallMod = 1 then

	'Select Case TurtleBall
		'Case 1: ActiveBall.Image = "PinballBlizzardBlue":TurtleBall=2
		'Case 2: ActiveBall.Image = "PinballPurplePizzazz":TurtleBall=3
		'Case 3: ActiveBall.Image = "PinballRadicalRed":TurtleBall=4
		'Case 4: ActiveBall.Image = "PinballOutrageousOrange":TurtleBall=1
	'End' Select
	'End If
End Sub


'******************************************************
'				DRAIN & RELEASE
'******************************************************

'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub Drain_Hit()
	PlaySound "drain"
	UpdateTrough
	Controller.Switch(10) = 1
	fgBall = true
	iBall = iBall + 1
	BallsInPlay = BallsInPlay - 1
End Sub

Sub Drain_UnHit()
	Controller.Switch(10) = 0
End Sub

sub kisort(enabled)
	If enabled then
		if fgBall then
			Drain.Kick 70,20
			iBall = iBall + 1
			fgBall = false
		end if
	end if
end sub

Sub KickBallToLane(Enabled)
	if enabled then
		StopSound "intro"
		PlaySound SoundFX("BallRelease",DOFContactors)
		BallRelease.Kick 70,5
		iBall = iBall - 1
		fgBall = false
		BallsInPlay = BallsInPlay + 1
		UpdateTrough
	end if
End Sub

 sub Kicker1_hit()
	Kicker1.destroyball
	KickerX.CreateBall
    KickerX.TimerInterval=600            'x second timer
    KickerX.TimerEnabled = True           'enables timer
	
end Sub


 Sub KickerX_Timer()
     KickerX.destroyball
	 Kicker2.CreateBall
     Kicker2.kick 190,7
     KickerX.Timerenabled = False     'stops timer

 End Sub


      ' van door animation



Set MotorCallback = GetRef("RealTimeUpdates")
Sub RealTimeUpdates
    'flippers
    door.RotX = ToiletFlipper.CurrentAngle
    End Sub





sub Kicker7_hit()
	Kicker7.destroyball
    Kicker8.CreateBall
	Kicker8.kick 190,7
End Sub





sub Trigger001_hit()
pizzavanshake
ToiletFlipper.RotatetoEnd
Light001. state = 1

End Sub


sub trigger1_hit()
ToiletFlipper.Rotatetostart
Light001. state = 0

End Sub

'********************************************
'*************VAN SHAKE**************************
'********************************************
Dim VanShake 

Sub pizzavanshake()
    VanShake  = 10
    vandoortimer.Enabled = 1
End Sub

Sub vandoortimer_Timer()
    pizzavan.Transz = VanShake  / 2
    If VanShake  = 0 Then Me.Enabled = 0:Exit Sub
    If VanShake  <0 Then
        VanShake  = ABS(VanShake ) - 1
    Else
        VanShake  = - VanShake  + 1
    End If
End Sub




 '''  Ramp portals

  sub portalentre_hit()
      portal1. opacity= 100
End Sub

Sub portalexit_hit()
    portal2. opacity= 100
End Sub

Sub portalsclose_hit()
    portal1. opacity= 0
    portal2. opacity= 0
End Sub

Sub pclose_hit()
    portal1. opacity= 0
    portal2. opacity= 0
End Sub

'Auto Plunger
''''

Sub solAutofire(Enabled)
	If Enabled Then
		AutoPlunger.autoFire
		'Playsound "solon"
	End If
End Sub





Sub table1_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySoundAt"plungerpull",Plunger
' Move VR plunger
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
    End If

	if keycode=StartGameKey then pupevent 14


	If keycode = LeftTiltKey Then
		Nudge 90, 2
	End If

	If keycode = RightTiltKey Then
		Nudge 270, 2
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 2
	End If

    ' Manual Ball Control
	If keycode = 46 Then	 				' C Key
		If EnableBallControl = 1 Then
			EnableBallControl = 0
		Else
			EnableBallControl = 1
		End If
	End If
    If EnableBallControl = 1 Then
		If keycode = 48 Then 				' B Key
			If BCboost = 1 Then
				BCboost = BCboostmulti
			Else
				BCboost = 1
			End If
		End If
		If keycode = 203 Then BCleft = 1	' Left Arrow
		If keycode = 200 Then BCup = 1		' Up Arrow
		If keycode = 208 Then BCdown = 1	' Down Arrow
		If keycode = 205 Then BCright = 1	' Right Arrow
	End If
    If vpmKeyDown(keycode) Then Exit Sub

	If keycode = 21 then  ''''''''''''''''''''y Key used for testing
		Kicker5.enabled = true
'		Kicker5.Kick 180,20
'		Kicker5.enabled = false
	End If

	If keycode = 22 then  ''''''''''''''''''''u Key used for testing
		Kicker5.Kick 165,400
		Kicker5.enabled = false
	End If

End Sub

Sub table1_KeyUp(ByVal keycode)
    If KeyUpHandler(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySoundAt"plunger",Plunger
        TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		Primary_plunger.Y = -250
End if 


    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If
End Sub




'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
		PlaySoundAt SoundFX("fx_Flipperup",DOFFlippers),LeftFlipper:LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
    Else

        PlaySoundAt SoundFX("fx_Flipperdown",DOFFlippers),LeftFlipper:LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_Flipperup",DOFFlippers),RightFlipper:RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_Flipperdown",DOFFlippers),RightFlipper:RightFlipper.RotateToStart
    End If
End Sub
'================Light Handling==================
'       GI, Flashers, and Lamp handling
'Based on JP's VP10 fading Lamp routine, based on PD's Fading Lights
'       Mod FrameTime and GI handling by nFozzy
'================================================
'Short installation
'Keep all non-GI lamps/Flashers in a big collection called aLampsAll
'Initialize SolModCallbacks: Const UseVPMModSol = 1 at the top of the script, before LoadVPM. vpmInit me in table1_Init()
'LUT images (optional)
'Make modifications based on era of game (setlamp / flashc for games without solmodcallback, use bonus GI subs for games with only one GI control)
 
Dim LampState(340), FadingLevel(340), CollapseMe   
Dim FlashSpeedUp(340), FlashSpeedDown(340), FlashMin(340), FlashMax(340), FlashLevel(340)
Dim SolModValue(340)    'holds 0-255 modulated solenoid values
 
'These are used for fading lights and flashers brighter when the GI is darker
Dim LampsOpacity(340, 2) 'Columns: 0 = intensity / opacity, 1 = fadeup, 2 = FadeDown
Dim GIscale(4)  '5 gi strings
Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image")
Dim TextureRedBulbArray: TextureRedBulbArray = Array("Bulb Red trans", "Bulb Red")
Dim TextureBlueBulbArray: TextureBlueBulbArray = Array("Bulb Blue trans", "Bulb Blue")
Dim TextureGreenBulbArray: TextureGreenBulbArray = Array("Bulb Green trans", "Bulb Green")
Dim TextureYellowBulbArray: TextureYellowBulbArray = Array("Bulb Yellow trans", "Bulb Yellow")
Dim BulbArray1: BulbArray1 = Array("Bulb_on_texture", "Bulb_texture")
Dim RedLight: RedLight = Array("Bulb_texture_on", "Bulb_texture_66", "Bulb_texture_33", "Bulb_texture_off")
Dim BlueLight: BlueLight = Array("Bulb_Blue_texture_on", "Bulb_Blue_texture_66", "Bulb_Blue_texture_33", "Bulb_Blue_texture_off")
Dim GreenLight: GreenLight = Array("Bulb_Green_texture_on", "Bulb_Green_texture_66", "Bulb_Green_texture_33", "Bulb_Green_texture_off")
Dim YellowLight: YellowLight = Array("Bulb_Yellow_texture_on", "Bulb_Yellow_texture_66", "Bulb_Yellow_texture_33", "Bulb_Yellow_texture_off")
Dim RedBumperCap: RedBumperCap = Array("RaB_Bumpercap_redon", "RaB_Bumpercap_red66", "RaB_Bumpercap_red33", "RaB_Bumpercap")
Dim YellowDome: YellowDome = Array("domeyellowbase", "domeyellowlit")
Dim RedDome: RedDome = Array("domeRedbase", "domeRedlit")
Dim YellowRoundDome: YellowRoundDome = Array("TopFlasherYellow_on", "TopFlasherYellow_66", "TopFlasherYellow_33", "TopFlasherYellow_off")
Dim GreenRoundDome: GreenRoundDome = Array("TopFlasherGreen_on", "TopFlasherGreen_66", "TopFlasherGreen_33", "TopFlasherGreen_off")
Dim YellowDome4: YellowDome4 = Array("domeyellow_on", "domeyellow_66", "domeyellow_33", "domeyellow_off")


Dim TestLight: TestLight = Array("Bulb_Yellow_texture_on", "Bulb_texture_66", "Bulb_Green_texture_33", "Bulb_Blue_texture_off")

 
InitLamps
 
reDim CollapseMe(1) 'Setlamps and SolModCallBacks   (Click Me to Collapse)
    Sub SetLamp(nr, value)
        If value <> LampState(nr) Then
            LampState(nr) = abs(value)
            FadingLevel(nr) = abs(value) + 4
        End If
    End Sub
 
    Sub SetLampm(nr, nr2, value)    'set 2 lamps
        If value <> LampState(nr) Then
            LampState(nr) = abs(value)
            FadingLevel(nr) = abs(value) + 4
        End If
        If value <> LampState(nr2) Then
            LampState(nr2) = abs(value)
            FadingLevel(nr2) = abs(value) + 4
        End If
    End Sub
 
    Sub SetModLamp(nr, value)
        If value <> SolModValue(nr) Then
            SolModValue(nr) = value
            if value > 0 then LampState(nr) = 1 else LampState(nr) = 0
            FadingLevel(nr) = LampState(nr) + 4
        End If
    End Sub
 
    Sub SetModLampM(nr, nr2, value) 'set 2 modulated lamps
        If value <> SolModValue(nr) Then
            SolModValue(nr) = value
            if value > 0 then LampState(nr) = 1 else LampState(nr) = 0
            FadingLevel(nr) = LampState(nr) + 4
        End If
        If value <> SolModValue(nr2) Then
            SolModValue(nr2) = value
            if value > 0 then LampState(nr2) = 1 else LampState(nr2) = 0
            FadingLevel(nr2) = LampState(nr2) + 4
        End If
    End Sub

 
'#end section
reDim CollapseMe(2) 'InitLamps  (Click Me to Collapse)
    Sub InitLamps() 'set fading speeds and other stuff here
        GetOpacity aLampsAll    'All non-GI lamps and flashers go in this object array for compensation script!
        Dim x
        for x = 0 to uBound(LampState)
            LampState(x) = 0    ' current light state, independent of the fading level. 0 is off and 1 is on
            FadingLevel(x) = 4  ' used to track the fading state
            FlashSpeedUp(x) = 0.1   'Fading speeds in opacity per MS I think (Not used with nFadeL or nFadeLM subs!)
            FlashSpeedDown(x) = 0.1
           
            FlashMin(x) = 0.001         ' the minimum value when off, usually 0
            FlashMax(x) = 1             ' the minimum value when off, usually 1
            FlashLevel(x) = 0.001       ' Raw Flasher opacity value. Start this >0 to avoid initial flasher stuttering.
           
            SolModValue(x) = 0          ' Holds SolModCallback values
           
        Next
       
        for x = 0 to uBound(giscale)
            Giscale(x) = 1.625          ' lamp GI compensation multiplier, eg opacity x 1.625 when gi is fully off
        next
           
        for x = 11 to 110 'insert fading levels (only applicable for lamps that use FlashC sub)
            FlashSpeedUp(x) = 0.015
            FlashSpeedDown(x) = 0.009
        Next       
       
        for x = 111 to 186  'Flasher fading speeds 'intensityscale(%) per 10MS
            FlashSpeedUp(x) = 1.1
            FlashSpeedDown(x) = 0.9
        next
       
        for x = 200 to 203      'GI relay on / off  fading speeds
            FlashSpeedUp(x) = 0.01
            FlashSpeedDown(x) = 0.008
            FlashMin(x) = 0
        Next
        for x = 300 to 303      'GI 8 step modulation fading speeds
            FlashSpeedUp(x) = 0.01
            FlashSpeedDown(x) = 0.008
            FlashMin(x) = 0
        Next      

    End Sub
 
    Sub GetOpacity(a)   'Keep lamp/flasher data in an array
        Dim x
        for x = 0 to (a.Count - 1)
            On Error Resume Next
            if a(x).Opacity > 0 then a(x).Uservalue = a(x).Opacity
            if a(x).Intensity > 0 then a(x).Uservalue = a(x).Intensity
            If a(x).FadeSpeedUp > 0 then LampsOpacity(x, 1) = a(x).FadeSpeedUp : LampsOpacity(x, 2) = a(x).FadeSpeedDown
        Next
        for x = 0 to (a.Count - 1) : LampsOpacity(x, 0) = a(x).UserValue : Next
    End Sub
 
    sub DebugLampsOn(input):Dim x: for x = 10 to 100 : setlamp x, input : next :  end sub
 
'#end section
 
reDim CollapseMe(3) 'LampTimer  (Click Me to Collapse)
    LampTimer.Interval = -1 '-1 is ideal, but it will technically work with any timer interval
    Dim FrameTime, InitFadeTime : FrameTime = 10    'Count Frametime
    Sub LampTimer_Timer()
        FrameTime = gametime - InitFadeTime
        Dim chgLamp, num, chg, ii
        chgLamp = Controller.ChangedLamps
        If Not IsEmpty(chgLamp) Then
            For ii = 0 To UBound(chgLamp)
                LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
                FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
            Next
        End If       

        UpdateLamps
        InitFadeTime = gametime
    End Sub
'#end section
   
    Sub UpdateLamps()

	NFadeL 1, L1
	NFadeL 2, L2
	NFadeL 3, L3
	NFadeL 4, L4
	NFadeL 5, L5
	NFadeL 6, L6
	NFadeL 7, L7
  NFadeLm 7, L7a
	NFadeL 8, L8
	Flashc 9, flasher9
	Flashc 10, flasher10
	Flashc 11, flasherl11 
	Flashc 12, flasherl12
	Flashc 13, flasherl13
	Flashc 14, flasherl14
	Flashc 15, flasherl15
	NFadeL 16, L16
NFadeLm 16, L16a
	NFadeL 17, L17
	NFadeL 18, L18
	NFadeL 19, L19
	NFadeL 20, L20
NFadeLm 20, L20a
	NFadeL 21, L21
	NFadeL 22, L22
NFadeLm 22, L22a
	NFadeL 23, L23
NFadeLm 23, L23a
NFadeLm 23, L23b
	NFadeL 24, L24
    NFadeLm 24, l24a
	NFadeL 25, L25
	NFadeL 26, L26	
	NFadeL 27, L27
	NFadeL 28, L28
	NFadeL 29, L29	
	NFadeL 30, L30
	NFadeL 32, L32
	NFadeLm 32, L32a
	NFadeL 33, L33
	NFadeL 34, L34
	NFadeLm 34, L34a
	NFadeL 35, L35
NFadeLm 35, L35a
	NFadeL 36, L36
	NFadeL 37, L37	
	NFadeL 38, L38
	NFadeL 41, L41
NFadeLm 41, L41a
	NFadeL 42, L42
    Flashc 43, l43
    Flashc 43, l43a
    Flashc 44, l44
    Flashc 44, l44a
    Flashc 45, l45
    Flashc 45, l45a
    Flashc 46, l46
    Flashc 46, l46a
	Flashc 47, l47
    Flashc 47, l47a
    Flashc 48, l48
    Flashc 48, l48a
	NFadeL 49, L49
NFadeLm 49, L49a
	NFadeL 50, L50
NFadeL 51, L51
NFadeL 52, L52
	NFadeL 53, L53
	NFadeL 55, L55
'NFadeLm 55, l55a
	NFadeL 56, L56
NFadeLm 56, L56a
	NFadeL 57, L57
'    NFadeLm 57, L57a 
	NFadeL 58, L58
NFadeLm 58, L58a
	NFadeL 59, L59
NFadeLm 59, L59a
	NFadeL 60, L60
	NFadeL 61, L61
	 'Flash And LampFlasher
    LampFlasher()
   End Sub
   
'#end section


''''Additions by CP


Sub FadeDisableLighting(nr, a)
    Select Case FadingLevel(nr)
		Case 2:a.BlendDisableLighting = .33
		Case 3:a.BlendDisableLighting = .66
		Case 4:a.BlendDisableLighting = 0
		Case 5:a.BlendDisableLighting = 1
    End Select
End Sub

Sub FadeDisableLighting1(nr, a)
    Select Case FadingLevel(nr)
		Case 2:a.BlendDisableLighting = .033
		Case 3:a.BlendDisableLighting = .066
		Case 4:a.BlendDisableLighting = 0
		Case 5:a.BlendDisableLighting = .1
    End Select
End Sub

'trxture swap	
dim itemw, itemp, itemp2

Sub FadeMaterialW(nr, itemw, group)
    Select Case FadingLevel(nr)
        Case 4:itemw.TopMaterial = group(1):itemw.SideMaterial = group(1)
        Case 5:itemw.TopMaterial = group(0):itemw.SideMaterial = group(0)
    End Select
End Sub


Sub FadeMaterialP(nr, itemp, group)
    Select Case FadingLevel(nr)
        Case 4:itemp.Material = group(1)
        Case 5:itemp.Material = group(0)
    End Select
End Sub


Sub FadeMaterial2P(nr, itemp2, group)
    Select Case FadingLevel(nr)
        Case 4:itemp2.Material = group(1)
        Case 5:itemp2.Material = group(0)
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

Sub FadePri4m(nr, pri, group)
    Select Case FadingLevel(nr)
		Case 2:pri.image = group(1) 'Off
        Case 3:pri.image = group(3) 'Fading...
        Case 4:pri.image = group(2) 'Fading...
        Case 5:pri.image = group(0) 'ON
    End Select
End Sub

Sub FadePri4(nr, pri, group)
    Select Case FadingLevel(nr)
        Case 2:pri.image = group(1):FadingLevel(nr) = 0 'Off
        Case 3:pri.image = group(3):FadingLevel(nr) = 2 'Fading...
        Case 4:pri.image = group(2):FadingLevel(nr) = 3 'Fading...
        Case 5:pri.image = group(0):FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadePri2m(nr, pri, group)
    Select Case FadingLevel(nr)
		Case 2:pri.image = group(1) 'Off
'        Case 3:pri.image = group(3) 'Fading...
'        Case 4:pri.image = group(2) 'Fading...
        Case 5:pri.image = group(0) 'ON
    End Select
End Sub

Sub FadePri2(nr, pri, group)
    Select Case FadingLevel(nr)
        Case 2:pri.image = group(1):FadingLevel(nr) = 0 'Off
'        Case 3:pri.image = group(3):FadingLevel(nr) = 2 'Fading...
'        Case 4:pri.image = group(2):FadingLevel(nr) = 3 'Fading...
        Case 5:pri.image = group(0):FadingLevel(nr) = 1 'ON
    End Select
End Sub

 

    Set GICallback = GetRef("UpdateGIon")       'On/Off GI to NRs 200-203
    Sub UpdateGIOn(no, Enabled) : Setlamp no+200, cInt(enabled) : End Sub  

 
    Sub Flashc(nr, object)  'FrameTime Compensated. Can work with Light Objects (make sure state is 1 though)
        Select Case FadingLevel(nr)
            Case 3 : FadingLevel(nr) = 0
            Case 4 'off
                FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * FrameTime)
                If FlashLevel(nr) < FlashMin(nr) Then
                    FlashLevel(nr) = FlashMin(nr)
                   FadingLevel(nr) = 3 'completely off
                End if
                Object.IntensityScale = FlashLevel(nr)
            Case 5 ' on
                FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * FrameTime)
                If FlashLevel(nr) > FlashMax(nr) Then
                    FlashLevel(nr) = FlashMax(nr)
                    FadingLevel(nr) = 6 'completely on
                End if
                Object.IntensityScale = FlashLevel(nr)
            Case 6 : FadingLevel(nr) = 1
        End Select
    End Sub
  

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
		Case 3
			FadingLevel(nr) = 0
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - (1/FlashSpeedDown(nr) * FrameTime)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 3 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + (1/FlashSpeedUp(nr) * FrameTime)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 6 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
		Case 6
			FadingLevel(nr) = 1
    End Select
End Sub

 
    Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
        select case FadingLevel(nr)
            case 3, 4, 5, 6 : Object.IntensityScale = FlashLevel(nr)
        end select
    End Sub
   
    Sub NFadeL(nr, object)  'Simple VPX light fading using State
   Select Case FadingLevel(nr)
        Case 3:object.state = 0:FadingLevel(nr) = 0
        Case 4:object.state = 0:FadingLevel(nr) = 3
        Case 5:object.state = 1:FadingLevel(nr) = 6
        Case 6:object.state = 1:FadingLevel(nr) = 1
    End Select
    End Sub
 
    Sub NFadeLm(nr, object) ' used for multiple lights
        Select Case FadingLevel(nr)
            Case 3:object.state = 0
            Case 4:object.state = 0
            Case 5:object.state = 1
            Case 6:object.state = 1
        End Select
    End Sub
 
Sub LampFlasher()
If LampState (61) = 1 Then
    flasher16a.opacity = 1000
    flasher16.state = 1
    flasher16b.state = 1
   Flasherlit001. blenddisablelighting= 10
   flasher15.state = 1
   flasher15a.opacity = 1000
   Flasherlit2. blenddisablelighting= 10
  Else
    flasher16a.opacity = 0
    flasher16.state = 0
    flasher16b.state = 0
    Flasherlit001. blenddisablelighting= 0
    flasher15.state = 0
   flasher15a.opacity = 0
Flasherlit2. blenddisablelighting= 0
 End If 



If LampState (60) = 1 Then
     flasher15a001.opacity = 1000
    flasher001.state = 1
    Flasherlit002. blenddisablelighting= 10
  Else
    flasher15a001.opacity = 0
    flasher001.state = 0
    Flasherlit002. blenddisablelighting= 0
 End If 

If LampState (59) = 1 Then
    flasher15a001.opacity = 1000
    flasher001.state = 1
    Flasherlit002. blenddisablelighting= 10
  Else
    flasher15a001.opacity = 0
    flasher001.state = 0
    Flasherlit002. blenddisablelighting= 0
     End If


If LampState (58) = 1 Then
    flasher15a001.opacity = 1000
    flasher001.state = 1
    Flasherlit002. blenddisablelighting= 10
  Else
    flasher15a001.opacity = 0
    flasher001.state = 0
    Flasherlit002. blenddisablelighting= 0
     End If



If LampState (7) = 1 Then
  
    Flash7.blenddisablelighting= 1
    Else
    
    flash7.blenddisablelighting= 0
     End If


If LampState (41) = 1 Then
    
    Flash41.blenddisablelighting= 1
    Else
     
    Flash41.blenddisablelighting= 0
     End If

If LampState (42) = 1 Then
    
    Flash42.blenddisablelighting= 1
    Else
  
    Flash42.blenddisablelighting= 0
     End If

If LampState (16) = 1 Then
    
    Flash16.blenddisablelighting= 1
    Else
   
    Flash16.blenddisablelighting= 0
    
     End If

If LampState (24) = 1 Then
    flasher004.state = 1
    Flasherlit005. blenddisablelighting= 3
    Flasherbase005. blenddisablelighting= 3
  Else
     flasher004.state = 0
     Flasherlit005. visible = 0
     Flasherbase005. visible = 0
 End If

If LampState (22) = 1 Or LampState (40) = 1 Then
    flasher19.state = 1
    Flasherlit8. blenddisablelighting= 3
    Flasherbase8. blenddisablelighting= 3
    
  Else
     flasher19.state = 0
     Flasherlit8. blenddisablelighting= 0
     Flasherbase8. blenddisablelighting= 0
 End If

If LampState (39) = 1 Or LampState (24) = 1 Then
    flasher20.state = 1
    Flasherlit9. blenddisablelighting= 3
    Flasherbase9. blenddisablelighting= 3
  Else
     flasher20.state = 0
     Flasherlit9. blenddisablelighting= 0
      Flasherbase9. blenddisablelighting= 0
 End If

If LampState (40) = 1 Then
    flasher003.state = 1
    Flasherlit004. blenddisablelighting= 3
    Flasherbase004. blenddisablelighting= 3
  Else
     flasher003.state = 0
     Flasherlit004. visible = 0
     Flasherbase004.visible = 0
 End If

If LampState (31) = 1 Then
   flasher005.state = 1
    Flasherlit006. blenddisablelighting= 3
    Flasherbase006. blenddisablelighting= 3
  Else
     flasher005.state = 0
     Flasherlit006. visible = 0
     Flasherbase006. visible = 0
 End If 

If LampState (32) = 1 Or LampState (23) = 1 Then
    flasher17.state = 1
   Flasherlit6. blenddisablelighting= 3
   Flasherbase6. blenddisablelighting= 3
   
  Else
     flasher17.state = 0
     Flasherlit6. blenddisablelighting= 0
     Flasherbase6. blenddisablelighting= 0
 End If

If LampState (23) = 1 Then
    flasher002.state = 1
    Flasherlit003. blenddisablelighting= 3
    Flasherbase003. blenddisablelighting= 3
  Else
     flasher002.state = 0
     Flasherlit003. visible = 0
     Flasherbase003. visible = 0
 End If

If LampState (21) = 1 Or LampState (31) = 1 Then
   
     flasher18.state = 1
     Flasherlit7. blenddisablelighting= 3
     Flasherbase7. blenddisablelighting= 3
  Else
     
     flasher18.state = 0
     Flasherlit7. blenddisablelighting= 0
     Flasherbase7. blenddisablelighting= 0
 End If

End Sub

 Sub UpdateGIstuff()  
FadeGI 200
UpdateGIobjectsSingle 200, theGicollection
GiCompensationSingle 200, aLampsAll, GIscale(0)
End Sub
   
'#end section
 
Sub theend() : End Sub
 
 


'**********
' Gi Lights
'**********

Sub SolGi(Enabled)
    Dim obj
    If Enabled Then
For each obj in GI:obj.state = 0:Next
Primitive018. blenddisablelighting= 0
PinCab_Blades. blenddisablelighting= 0
table1.colorgradeimage = "ColorGrade_4"
	playsound "fx_relay_off", 0	
    portal1. opacity= 300
    portal2. opacity= 300
    Else
For each obj in GI:obj.state = 1:Next
Primitive018. blenddisablelighting= 0.3
PinCab_Blades. blenddisablelighting= 0
table1.colorgradeimage = "ColorGradeLUT256x16_ConSat"
 playsound "fx_relay_on", 0
	portal1. opacity= 1000
    portal2. opacity= 1000
    End If
End Sub


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	flasher12a. state = 1
    Flasherlit5. blenddisablelighting= 10
    flasher12.visible = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
    flasher12.visible =  0
    flasher12a. state = 0
    Flasherlit5. blenddisablelighting= 0
End Sub



Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
   
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	flasher11a. state = 1
    Flasherlit1. blenddisablelighting= 10
    flasher11.visible = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
    flasher11.visible =  0
    flasher11a. state = 0
    Flasherlit1. blenddisablelighting= 0
End Sub
 
'sub LeftSlingShot_Slingshot:Flasherflash1 =1 :Flasherflash1a = 1 :End Sub
'Sub LeftSlingShot_Unhit:Flasherflash1 =0:Flasherflash1a = 0:End Sub


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

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
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
    Vol = Csng(BallVel(ball) ^2 / 400)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function


'*****************************************
'   rothbauerw's Manual Ball Control
'*****************************************

Dim BCup, BCdown, BCleft, BCright
Dim ControlBallInPlay, ControlActiveBall
Dim BCvel, BCyveloffset, BCboostmulti, BCboost

BCboost = 1				'Do Not Change - default setting
BCvel = 4				'Controls the speed of the ball movement
BCyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
BCboostmulti = 3		'Boost multiplier to ball veloctiy (toggled with the B key) 

ControlBallInPlay = false

Sub StartBallControl_Hit()
	Set ControlActiveBall = ActiveBall
	ControlBallInPlay = true
End Sub

Sub StopBallControl_Hit()
	ControlBallInPlay = false
End Sub	

Sub BallControlTimer_Timer()
	If EnableBallControl and ControlBallInPlay then
		If BCright = 1 Then
			ControlActiveBall.velx =  BCvel*BCboost
		ElseIf BCleft = 1 Then
			ControlActiveBall.velx = -BCvel*BCboost
		Else
			ControlActiveBall.velx = 0
		End If

		If BCup = 1 Then
			ControlActiveBall.vely = -BCvel*BCboost
		ElseIf BCdown = 1 Then
			ControlActiveBall.vely =  BCvel*BCboost
		Else
			ControlActiveBall.vely = bcyveloffset
		End If
	End If
End Sub


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

	' play the rolling sound for each ball

    For b = 0 to UBound(BOT)
      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/12, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/15, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
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


'RampHelpers

Dim OnWireRamp

Sub tRampHelper1a_hit()
	OnWireRamp = 1
End Sub

Sub tRampHelper1b_hit()
	OnWireRamp = 0
    StopSound("metalrolling")
End Sub

Sub tRampHelper2a_hit()
	OnWireRamp = 1
End Sub

Sub tRampHelper2b_hit()
	OnWireRamp = 0
    StopSound("metalrolling")
End Sub

'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

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
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10                       
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 80
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub



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
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub



' ===============================================================================================
' Spinning Pizza
' ===============================================================================================

Dim discAngle, stepAngle, stopDiscs, discsAreRunning

InitDiscs()

Sub InitDiscs()
	discAngle 			= 0
	discsAreRunning		= False
End Sub

Sub SolPizzaSpin(Enabled)

	ttPizza.MotorOn = Enabled

	If Enabled Then
		stepAngle			= 20.0
		discsAreRunning		= True
		stopDiscs			= False
		DiscsTimer.Interval = 20
		DiscsTimer.Enabled 	= True
	Else
		stopDiscs			= True
		discsAreRunning		= True
	End If
End Sub

Sub DiscsTimer_Timer()
	' calc angle
	discAngle = discAngle + stepAngle
	If discAngle >= 360 Then
		discAngle = discAngle - 360
	End If

	' rotate discs

	pPizza.RotY = discAngle

	If stopDiscs Then
		stepAngle = stepAngle - 0.1
		If stepAngle <= 0 Then
			DiscsTimer.Enabled 	= False
		End If
	End If
End Sub




''''''''''''''''''''''''''''
''' Switches
''''''''''''''''''''''''''''

Sub sw14_Hit()
	Switch14dir = 1
	Sw14Move = 1
flasher11. opacity= 0
flasher12. opacity= 0
'portal1. opacity= 0
'    portal2. opacity= 0
	Me.TimerEnabled = true
	PlaySoundAt "sensor",sw14
	Controller.Switch(14) = 1
End Sub

Sub sw14_unHit()
	Switch14dir = -1
	Sw14Move = 5
	Me.TimerEnabled = true
	Controller.Switch(14) = 0
End Sub




Sub sw17_Hit()
	Switch17dir = 1
	Sw17Move = 1
	Me.TimerEnabled = true
	Controller.Switch(17) = 1
	PlaySoundAt "sensor",sw17
End Sub

Sub sw17_unHit()
	Switch17dir = -1
	Sw17Move = 5
	Me.TimerEnabled = true
	Controller.Switch(17) = 0
End Sub


Sub sw18_Hit()
	Switch18dir = 1
	Sw18Move = 1
	Me.TimerEnabled = true
	Controller.Switch(18) = 1
	PlaySoundAt "sensor",sw18
End Sub

Sub sw18_unHit()
	Switch18dir = -1
	Sw18Move = 5
	Me.TimerEnabled = true
	Controller.Switch(18) = 0
End Sub


Sub sw19_Hit()
	Switch19dir = 1
	Sw19Move = 1
	Me.TimerEnabled = true
	Controller.Switch(19) = 1
	PlaySoundAt "sensor",sw19
End Sub

Sub sw19_unHit()
	Switch19dir = -1
	Sw19Move = 5
	Me.TimerEnabled = true
	Controller.Switch(19) = 0
End Sub


Sub sw20_Hit()
	Switch20dir = 1
	Sw20Move = 1
	Me.TimerEnabled = true
	Controller.Switch(20) = 1
	PlaySoundAt "sensor",sw20
End Sub

Sub sw20_unHit()
	Switch20dir = -1
	Sw20Move = 5
	Me.TimerEnabled = true
	Controller.Switch(20) = 0
End Sub

Sub sw20a_Hit()
	Switch20adir = 1
	Sw20aMove = 1
	Me.TimerEnabled = true
	Controller.Switch(20) = 1
	PlaySoundAt "sensor",sw20a
End Sub

Sub sw20a_unHit()
	Switch20dir = -1
	Sw20Move = 5
	Me.TimerEnabled = true
	Controller.Switch(20) = 0
End Sub

Sub sw31_Hit:vpmTimer.PulseSw 31:End Sub

Sub sw32_Hit:Controller.Switch(32) = 1:PlaySoundAt "sensor",sw32:sw32.timerenabled = true:End Sub
Sub sw32_Unhit:Controller.Switch(32) = 0:End Sub

'Sub triggervan_Hit:pizzavan. blenddisablelighting = 3:End Sub


Sub sw40_Hit:Controller.Switch(40) = 1:PlaySoundAt "sensor",sw40:sw40.timerenabled = true:End Sub
Sub sw40_Unhit:Controller.Switch(40) = 0:End Sub

Sub sw40a_Hit:Controller.Switch(40) = 1:PlaySoundAt "sensor",sw40:sw40.timerenabled = true:End Sub
Sub sw40a_Unhit:Controller.Switch(40) = 0:End Sub

Sub sw50_Hit()
	Controller.Switch(50) = 1
	PlaySoundAt "sensor",sw50
End Sub

Sub sw50_unHit()
	Controller.Switch(50) = 0
End Sub

Sub sw50a_Hit()
	Controller.Switch(50) = 1
	PlaySoundAt "sensor",sw50
End Sub

Sub sw50a_unHit()
	Controller.Switch(50) = 0
End Sub


Sub sw56_Spin():vpmTimer.PulseSw 56:PlaySoundAt "fx_spinner",sw56:End Sub

Sub Kicker4_Hit():PlaySoundAt "fx_subway",Kicker4:End Sub


Dim TempBallImage
Sub sw41_Hit()
     
	TempBallImage = ActiveBall.image
	If TempBallImage = "pinball_ball2" Then
		ActiveBall.image = "pinball_ball2_dim"
	End If
	If TempBallImage = "PinballBlizzardBlue" Then
		ActiveBall.image = "PinballBlizzardBlue_dim"
	End If
	If TempBallImage = "PinballOutrageousOrange" Then
		ActiveBall.image = "PinballOutrageousOrange_dim"
	End If
	If TempBallImage = "PinballPurplePizzazz" Then
		ActiveBall.image = "PinballPurplePizzazz_dim"
	End If
	If TempBallImage = "PinballRadicalRed" Then
		ActiveBall.image = "PinballRadicalRed_dim"
        
	End If
	Controller.Switch(41) = 1
End Sub

Sub sw41_unHit():ActiveBall.image = TempBallImage:End Sub

Sub SewerUpKick(Enabled)
pizzavanshake
	PlaySoundAt "solenoid",sw41
 	sw41.Kick 190,7  'Power = 45
    pizzavan. blenddisablelighting= 0.1
	Controller.Switch(41) = 0
End Sub









Dim Switch14dir, SW14Move

Sub sw14_timer()
Select case Sw14Move

	Case 0:me.TimerEnabled = false:pRollover5.RotX = 90

	Case 1:pRollover5.RotX = 95

	Case 2:pRollover5.RotX = 100

	Case 3:pRollover5.RotX = 105

	Case 4:pRollover5.RotX = 110

	Case 5:pRollover5.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover5.RotX = 120

End Select

SW14Move = SW14Move + Switch14dir

End Sub


Dim Switch17dir, SW17Move
'Switch19dir = -2

Sub sw17_timer()
Select case Sw17Move

	Case 0:me.TimerEnabled = false:pRollover2.RotX = 90

	Case 1:pRollover2.RotX = 95

	Case 2:pRollover2.RotX = 100

	Case 3:pRollover2.RotX = 105

	Case 4:pRollover2.RotX = 110

	Case 5:pRollover2.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover2.RotX = 120

End Select

SW17Move = SW17Move + Switch17dir

End Sub


Dim Switch20adir, SW20aMove

Sub sw20a_timer()
Select case Sw20aMove

	Case 0:me.TimerEnabled = false:pRollover3.RotX = 90

	Case 1:pRollover3.RotX = 95

	Case 2:pRollover3.RotX = 100

	Case 3:pRollover3.RotX = 105

	Case 4:pRollover3.RotX = 110

	Case 5:pRollover3.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover3.RotX = 120

End Select

SW20aMove = SW20aMove + Switch20adir

End Sub

Dim Switch18dir, SW18Move

Sub sw18_timer()
Select case Sw18Move

	Case 0:me.TimerEnabled = false:pRollover1.RotX = 90

	Case 1:pRollover1.RotX = 95

	Case 2:pRollover1.RotX = 100

	Case 3:pRollover1.RotX = 105

	Case 4:pRollover1.RotX = 110

	Case 5:pRollover1.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover1.RotX = 120

End Select

SW18Move = SW18Move + Switch18dir

End Sub

Dim Switch19dir, SW19Move

Sub sw19_timer()
Select case Sw19Move

	Case 0:me.TimerEnabled = false:pRollover5.RotX = 90

	Case 1:pRollover5.RotX = 95

	Case 2:pRollover5.RotX = 100

	Case 3:pRollover5.RotX = 105

	Case 4:pRollover5.RotX = 110

	Case 5:pRollover5.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover5.RotX = 120

End Select

SW19Move = SW19Move + Switch19dir

End Sub


Dim Switch20dir, SW20Move

Sub sw20_timer()
Select case Sw20Move

	Case 0:me.TimerEnabled = false:pRollover4.RotX = 90

	Case 1:pRollover4.RotX = 95

	Case 2:pRollover4.RotX = 100

	Case 3:pRollover4.RotX = 105

	Case 4:pRollover4.RotX = 110

	Case 5:pRollover4.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover4.RotX = 120

End Select

SW20Move = SW20Move + Switch20dir

End Sub


''''''''''''''''''''''''''''
''' Bumpers
''''''''''''''''''''''''''''

Sub Bumper1_Hit:vpmTimer.PulseSw 47:PlaySound SoundFX("fx_bumper2",DOFContactors):light12.duration 1,200,0:pBumpCap002.blenddisablelighting = 3:vpmtimer.addtimer 200, "pBumpCap002.blenddisablelighting = 0.1'":PikachuShake:krang.blenddisablelighting = 1:vpmtimer.addtimer 200, "krang.blenddisablelighting = 0'":End Sub


Sub Bumper2_Hit:vpmTimer.PulseSw 46:PlaySound SoundFX("fx_bumper2",DOFContactors):light12.duration 1,200,0:pBumpCap3.blenddisablelighting = 3:vpmtimer.addtimer 200, "pBumpCap3.blenddisablelighting = 0.1'":End Sub


Sub Bumper3_Hit:vpmTimer.PulseSw 48:PlaySound SoundFX("fx_bumper2",DOFContactors):light12.duration 1,200,0:pBumpCap003.blenddisablelighting = 3:vpmtimer.addtimer 200, "pBumpCap003.blenddisablelighting = 0.1'":End Sub


Dim Target25Step, Target26Step, Target27Step, Target28Step, Target29Step, Target30Step, Target31Step, Target32Step, Target33Step, Target34Step, Target35Step, Target36Step, Target37Step, Target38Step, Target39Step, Target49Step, Target55Step

Sub t24_Hit:vpmTimer.PulseSw(23):End Sub

Sub t25_Hit:vpmTimer.PulseSw(25):End Sub			

Sub t26_Hit:vpmTimer.PulseSw(26):tagert26.Transz = 5:Target26Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t26:End Sub			
Sub t26_timer()
	Select Case Target26Step
		Case 1:tagert26.Transz = 3
        Case 2:tagert26.Transz = -2
        Case 3:tagert26.Transz = 1
        Case 4:tagert26.Transz = 0:Me.TimerEnabled = 0:Target26Step = 0
     End Select
	Target26Step = Target26Step + 1
End Sub

Sub DoubleTarget4_hit:vpmTimer.PulseSw(27):vpmTimer.PulseSw(26):target27.Transz = 5:Target27Step = 1:t27.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t27:tagert26.Transz = 5:Target26Step = 1:t26.TimerEnabled = 1:End Sub

Sub t27_Hit:vpmTimer.PulseSw(27):target27.Transz = 5:Target27Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t27:End Sub			
Sub t27_timer()
	Select Case Target27Step
		Case 1:target27.Transz = 3
        Case 2:target27.Transz = -2
        Case 3:target27.Transz = 1
        Case 4:target27.Transz = 0:Me.TimerEnabled = 0:Target27Step = 0
     End Select
	Target27Step = Target27Step + 1
End Sub
	

Sub DoubleTarget3_hit:vpmTimer.PulseSw(28):vpmTimer.PulseSw(27):target28.Transz = 5:Target28Step = 1:t28.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t28:target27.Transz = 5:Target27Step = 1:t27.TimerEnabled = 1:End Sub

Sub t28_Hit:vpmTimer.PulseSw(28):target28.Transz = 5:Target28Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t28:End Sub			
Sub t28_timer()
	Select Case Target28Step
		Case 1:target28.Transz = 3
        Case 2:target28.Transz = -2
        Case 3:target28.Transz = 1
        Case 4:target28.Transz = 0:Me.TimerEnabled = 0:Target28Step = 0
     End Select
	Target28Step = Target28Step + 1
End Sub 



Sub DoubleTarget2_hit:vpmTimer.PulseSw(29):vpmTimer.PulseSw(28):target29.Transz = 5:Target29Step = 1:t29.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t29:target28.Transz = 5:Target28Step = 1:t28.TimerEnabled = 1:End Sub

Sub t29_Hit:vpmTimer.PulseSw(29):target29.Transz = 5:Target29Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t29:End Sub			
Sub t29_timer()
	Select Case Target29Step
		Case 1:target29.Transz = 3
        Case 2:target29.Transz = -2
        Case 3:target29.Transz = 1
        Case 4:target29.Transz = 0:Me.TimerEnabled = 0:Target29Step = 0
     End Select
	Target29Step = Target29Step + 1
End Sub

Sub DoubleTarget1_hit:vpmTimer.PulseSw(30):vpmTimer.PulseSw(29):Target30Step = 1:t30.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t30:pT29A.Transz = 5:Target29Step = 1:t29.TimerEnabled = 1:End Sub

Sub t30_Hit:vpmTimer.PulseSw(30):Target30Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t30:End Sub			
Sub t30_timer()
	
	Target30Step = Target30Step + 1
End Sub

Sub t32_Hit:vpmTimer.PulseSw(32):End Sub

Sub t33_Hit:vpmTimer.PulseSw(33):Target33Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t33:End Sub			
Sub t33_timer()
	
	Target33Step = Target33Step + 1
End Sub



Sub t34_Hit:vpmTimer.PulseSw(34):target34.Transz = 1.25:Target34Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t34:End Sub			
Sub t34_timer()
	Select Case Target34Step
		Case 1:target34.Transz= 1
        Case 2:target34.Transz = -.75
        Case 3:target34.Transz = .5
        Case 4:target34.Transz = -.25
        Case 5:target34.Transz = .15
        Case 6:target34.Transz = -.1
        Case 7:target34.Transz = .05
        Case 8:target34.Transz = -.02
        Case 9:target34.Transz = 0:Me.TimerEnabled = 0:Target34Step = 0
     End Select
	Target34Step = Target34Step + 1
End Sub



Sub T35_Hit:vpmTimer.PulseSw (35): target35.Transz = 5:Target35Step = 0:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub			
Sub t35_timer()
	Select Case Target35Step
		Case 1:target35.Transz = 3
        Case 2:target35.Transz = -2
        Case 3:target35.Transz = 1
        Case 4:target35.Transz = 0:Me.TimerEnabled = 0:Target35Step = 0
     End Select
	Target35Step = Target35Step + 1
End Sub

Sub t36_Hit:vpmTimer.PulseSw(36):target36.Transz = 5:Target36Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t36:End Sub			
Sub t36_timer()
	Select Case Target36Step
		Case 1:target36.Transz = 3
        Case 2:target36.Transz = -2
        Case 3:target36.Transz = 1
        Case 4:target36.Transz = 0:Me.TimerEnabled = 0:Target36Step = 0
     End Select
	Target36Step = Target36Step + 1
End Sub

Sub DoubleTarget8_hit:vpmTimer.PulseSw(36):vpmTimer.PulseSw(37):target36.Transz = 5:Target36Step = 1:t36.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t36:target37.Transz = 5:Target37Step = 1:t37.TimerEnabled = 1:End Sub

Sub t37_Hit:vpmTimer.PulseSw(37):target37.Transz = 5:Target37Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t37:End Sub			
Sub t37_timer()
	Select Case Target37Step
		Case 1:Target37.Transz = 3
        Case 2:Target37.Transz = -2
        Case 3:target37.Transz = 1
        Case 4:target37.Transz = 0:Me.TimerEnabled = 0:Target37Step = 0
     End Select
	Target37Step = Target37Step + 1
End Sub

Sub DoubleTarget9_hit:vpmTimer.PulseSw(37):vpmTimer.PulseSw(38):target37.Transz = 5:Target37Step = 1:t37.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t37:target38.Transz = 5:Target38Step = 1:t38.TimerEnabled = 1:End Sub

Sub t38_Hit:vpmTimer.PulseSw(38):target38.Transz = 5:Target38Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t38:End Sub			
Sub t38_timer()
	Select Case Target38Step
		Case 1:target38.Transz = 3
        Case 2:target38.Transz = -2
        Case 3:target38.Transz = 1
        Case 4:target38.Transz = 0:Me.TimerEnabled = 0:Target38Step = 0
     End Select
	Target38Step = Target38Step + 1
End Sub

Sub t39_Hit:vpmTimer.PulseSw(39):Target39Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),t39:End Sub			
Sub t39_timer()
	
	Target39Step = Target39Step + 1
End Sub

Sub t49_Hit:vpmTimer.PulseSw(49):End Sub

Sub t49a_Hit:vpmTimer.PulseSw(49):End Sub

Sub t55_Hit:vpmTimer.PulseSw(55):End Sub		

'''''''''''''''




'*********************
'VR Mode
'*********************
'DIM VRThings
'If VRRoom > 0 Then
	'ScoreText001.visible = 0
	'DMD.visible = 1
	'PinCab_Backglass.blenddisablelighting = 3
	'PinCab_Rails_Hinges.visible = 1
	'If VRRoom = 1 Then
		'for each VRThings in BigRoom:VRThings.visible = 1:Next
		'for each VRThings in VRCab:VRThings.visible = 1:Next
		'for each VRThings in MinRoom:VRThings.visible = 0:Next
       ' for each VRThings in turtles:VRThings.visible = 0:Next
	'End If
	'If VRRoom = 2 Then
		'for each VRThings in BigRoom:VRThings.visible = 0:Next
		'for each VRThings in VRCab:VRThings.visible = 1:Next
		'for each VRThings in MinRoom:VRThings.visible = 1:Next
       ' for each VRThings in turtles:VRThings.visible = 0:Next
	'End If
	'If VRRoom = 3 Then
		'for each VRThings in BigRoom:VRThings.visible = 0:Next
		'for each VRThings in VRCab:VRThings.visible = 0:Next
		'for each VRThings in MinRoom:VRThings.visible = 0:Next
       ' for each VRThings in turtles:VRThings.visible = 0:Next
		'PinCab_Backbox.visible = 1
		'PinCab_Backglass.visible = 1
	'End If
'Else
		'for each VRThings in BigRoom:VRThings.visible = 0:Next
		'for each VRThings in VRCab:VRThings.visible = 0:Next
		'for each VRThings in MinRoom:VRThings.visible = 0:Next
	if DesktopMode then
		ScoreText001.visible = 0
		'PinCab_Rails_Hinges.visible = 1
	else
		ScoreText001.visible = 0
		'PinCab_Rails_Hinges.visible = 0
	End if
'End If



'*****************************************************************************************************
' VR PLUNGER ANIMATION
'
' Code needed to animate the plunger. If you pull the plunger it will move in VR.
' IMPORTANT: there are two numeric values in the code that define the postion of the plunger and the 
' range in which it can move. The fists numeric value is the actual y position of the plunger primitive
' and the second is the actual y position + 135 to determine the range in which it can move.
'
' You need to to select the Primary_plunger primitive you copied from the
' template you need to select the Primary_plunger primitive and copy the value of the Y position 
' (e.g. 1269.286) into the code. The value that determines the range of the plunger is always the y 
' position + 135 (e.g. 1404).
'
'*****************************************************************************************************

'Sub TimerPlunger_Timer
 ' If Primary_plunger.Y < -200 Then
  '	Primary_plunger.Y = Primary_plunger.Y + 5
 ' End If
'End Sub

'Sub TimerPlunger2_Timer
 'debug.print plunger.position
 ' Primary_plunger.Y = -30 + (5* Plunger.Position) -350
'End Sub

' VR Coin in slot animation...

'Dim MoveCoin:MoveCoin=AddCreditKey
'Sub VRCoinTimer_timer()
		'Coin.Y=Coin.Y-MoveCoin	  
		'Coin.Z=Coin.Z-MoveCoin/2
		'Coin.RotX=Coin.RotX+MoveCoin
	'If Coin.y<=2250 then 
		'Coin.visible = false
		'Coin.Y=2350
		'Coin.Z = -5	
		'VRCointimer.enabled = false
   ' End if
'End Sub


























