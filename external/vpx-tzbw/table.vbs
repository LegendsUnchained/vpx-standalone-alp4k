'  +              `s
' yNhssyyyydddhhyhhNo
'+/`      oMN      `+:                    ....`                  ```  ::  .//+/:
'         hMm        : `:/++.    ```.`    :mMd`    :ddo-    `/yyooooyhmm   +MM`  -sNNh/ `          `
'         hMN `/odmh:`   :NMy   .Nm:..     hMd`     yM/   `oNd.       -s+  /MM`    NM: `NhhyyhhssshN.
'         dMd    oMm`     hMM-  .N:`:hmh`  hMh      sM+  `dMm.          .  /MM`    NM. ss`  oMh    :+
'         NMd     dMh`   -MMMo `d+   mMs   hMs      sMo  yMM-           ```:MM+//++MM. `    yMy
'         mMd     `mMh   ds+MN`sy    mMy   hMs      oMo `NMm      -.:yNMN:`/MM/-.`.MM.      yMy
'         NMm      `hMy oh  NMdd     hMo   dMo      yM: `MMd         `NMd  /MM.   `MM`      hMs
'         MMd       `hMdN.  +Mm`     dMs   dMo     .ss+` dMN.         NMd  -Mm`   .MM`      mMo
'        `MMo        `dM:    d-      mMs   NMs   `.:/    .dMN/      `+Ns.  :MM`   -MM.      NM+
'        .MMy         `:            :MMs  :yyssyyhdm.      /ymmsooooo-    -dNd:` .+oo+/    `MM+`.
'        oMMy                       ..``                                                  ./+/:--
'      -+oo+/:-     ``
'                  -o.                  -
'                 /Nmdddddysysoooo+yNNNy.   `-+s+:-  .-//+/     -+sss:           +`
'                +s/:-.`         :dMMs.  `+ho:-:/yMNo`  /MMy      mM.   +hdysossyms
'               `              /dMm+`   oMy`      .dMm. `MMMm`    oN    .NM:      .:
'                           `oNMd/     yMh         `MMy `N+hMN:   +N    .MM-       `
'                         `oNMh:      `NM:          mMy  N+ oMMo  :M    `NMdhhhhdmmy
'                       .smMh-        -MM/         `NN.  m+  :NMh`-M`   `NM+``   `:
'                     :hMMy.           yMd-       `hy.   N+   .hMm:M.   `NM.
'                   :dMMs.          .+/ +dMdo:-:/oo-    `M/     oMMM-   `NM:       -
'                 :hMMd///++osyhhdddMy    .:++/-`       oNy+:.   -mM:   `MM- .-/+ym.
'                /o+/::--.`````     o                  -/-.`       s/  -ohysoo+/:/-

' Twilight Zone - IPDB No. 2684
' © Bally/Midway 1993

' Remastered by Skitso, rothbauerw and Bord. Fixed gumball mechanics not eating balls (thanks fFozzy), completely new gameplay and physics,
' mesh playfield, re-modeled ramps and other primitives, all new lighting and flashers, tons of visual polish and detailing.

' VPX recreation by ninuzzu
' This table started as a FP conversion by coindropper. Clark Kent continued improving it at graphics and physics level
' Then he asked me an help with lighting and other things and it evolved into something different.
' We have completely rebuild the table, so I would call it a "from scratch" build, rather than a conversion.
' Flupper and Tom Tower joined the team later and enhanced the visuals with some stunning models. You guys rock!
' This table is maybe the most modded ever, lots of options in the script. Lots of fun!

' Credits/Thanks
' coindropper early development
' nFozzy for scripting the trough, the lock, the magnets and a lot of other things
' JPSalas for the help with the inserts and the script (you know who is THE MAN!)
' Tom Tower for the mini clock, pyramid, piano, camera, gumballmachine models, wich I retextured.
' Hauntfreaks for the Mystic Seer Toy, wich I retextured
' Flupper for retexturing and remeshing the plastic ramp
' Zany for the domes, flippers and bumpers models
' rom for "Robbie the Robot" model, wich I edited and retextured
' knorr and pacdude for some sound effects I borrowed from their tables
'

' Thalamus 2018-07-24
' Added/Updated "Positional Sound Playback Functions" and "Supporting Ball & Sound Functions"
' Changed UseSolenoids=1 to 2
' No special SSF tweaks yet.
' Wob 2018-08-09
' Added vpmInit Me to table init and cSingleLFlip
' DJRobX 2018-08-18
' Remove CSingleLFlip - there is an ULFlip sub
' Add more SSF and VPMModSol code.

' nf 2018-10-09 rescripted gumball machine, slot machine kickout, clock sfx, added ballsearch on hard pinmame reset / find lost balls

Option Explicit
Randomize

Dim Romset, PowerballStart, CabinetSide,CabinetMode, FlipperType, RampShadow, ScoopLight, GumballMod, SlotMachineMod, PyramidMod, BWClockMod, MiniClockMod
Dim InvaderMod, MysticSeerMod, TargetMod, TVMod, LampLightColor, TownSquarePostMod, SpiralMod, ExtraMagnet, BumperPostsMod, StagedFlipperMod, Lockdownbar

'******************************************************************************************
'* TABLE OPTIONS **************************************************************************
'******************************************************************************************

'/////////////////////-----Cabinet Mode-----/////////////////////
CabinetMode = 1	'0 - Off, 1 - Hides rails & scales side panels

'***********	Choose the ROM	  *********************************************************

Romset = 1				'0 =arcade rom with credits,  1 = home rom (free play)

'***********	Show lockdownbar 	  *****************************************************

Lockdownbar = 0			'0 = no, 1 = yes

'***********	Set the Powerball Starting Location		***********************************

PowerBallStart = 7		'1-3 = gumball machine, 4-6 = trough, 0 = random, 7 = random gumball

'***********	Show cabinet sidewalls custom artwork	***********************************

CabinetSide = 0			'0 = no , 1 = yes

'***********	Use staged flippers (dual leaf switches)	*******************************

StagedFlipperMod = 0 	'0 = not staged, 1 - staged (dual leaf switches)

'***********	Set the Flippers Type	***************************************************

FlipperType = 0 		'0 = yellow , 1 = spiral , 2 = random

'***********	Render the Wireramp shadow on the playfield	*******************************

RampShadow = 1			'0 = no , 1 = yes

'***********	Red Light under the scoop 		*******************************************

ScoopLight = 1			'0 = no , 1 = yes

'***********	Render Gumballs in Gumball Machine  ***************************************

GumballMod = 1 			'0 = no , 1 = yes

'***********	Render the Slot Machine Toy		*******************************************

SlotMachineMod = 0 		'0 = no , 1 = yes

'***********	Render the Pyramid Toy	***************************************************

PyramidMod = 0

'***********	Render the Clock with Black and White Face ********************************

BWClockMod = 0			'0 = no , 1 = yes

'***********	Render the Mini Clock at ramps entrance	 **********************************

MiniClockMod = 1 		'0 = no , 1 = yes

'***********	Render the Invader Toy  ***************************************************

InvaderMod = 0 			'0 = no , 1 = yes

'***********	Render the Mystic Seer	Toy 	*******************************************

MysticSeerMod = 0 		'0 = no , 1 = yes

'***********	Enable TargetMod (Targets with themed decals) 	***************************

TargetMod = 1 			'0 = disabled , 1 = enabled

'***********	Enable TV Mod 	***********************************************************

TVMod = 1				'0 = disabled , 1 = enabled

'***********	Enable Town Square Mod	***************************************************

TownSquarePostMod = 0 	'0 = disabled , 1 = enabled

'***********	Enable the Spiral Animated Cover Mod	***********************************

SpiralMod = 1 			'0 = disabled , 1 = enabled

'***********	Enable Bumper Posts Mod	***************************************************

'the game will be harder if you enable this

BumperPostsMod = 0 		'0 = disabled , 1 = enabled

'***********	Enable 3rd magnet	*******************************************************

ExtraMagnet = 0			'0 = disabled , 1 = enabled


Const VolumeDial = 10				'Change volume of hit events
Const RollingSoundFactor = 0.3 		'Change volume of rolling sounds
Const VROn = False					'Set to True to enable backglass and DMD in VR

If CabinetMode = 1 Then
	Primary_SideRailMapLeft.visible = 0
	Primary_SideRailMapRight.visible = 0
	LeftCab.size_z = 3
	RightCab.size_z = 3
Else
	Primary_SideRailMapLeft.visible = 1
	Primary_SideRailMapRight.visible = 1
	LeftCab.size_z = 1
	RightCab.size_z = 1
End If


'******************************************************************************************
'* END OF TABLE OPTIONS *******************************************************************
'******************************************************************************************

Dim Ballsize,BallMass
BallSize = 50
BallMass = 1

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD:UseVPMDMD = 0' DesktopMode
Const UseVPMColoredDMD = true
Dim Size

If Table1.ShowFSS = True or DesktopMode = False Then
	ScoreText.visible = False
End If

	

TextBox.visible = False ' Used for debugging stuck balls

Const UseVPMModSol = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "02000000", "WPC.VBS", 3.49

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 1
Const HandleMech = 0

Const SSolenoidOn = "fx_solon"
Const SSolenoidOff = ""
Const SFlipperOn = ""
Const SFlipperOff = ""
Const SCoin = "fx_coin"

'Set MotorCallback = GetRef("GameTimer")
Set GiCallback2 = GetRef("UpdateGI")


'************************************************************************
'						 INIT TABLE
'************************************************************************

' using table width and height in script slows down the performance
dim tablewidth: tablewidth = Table1.width
dim tableheight: tableheight = Table1.height

Dim TZBall1, TZBall2, TZBall3, TZBall4, TZBall5, TZBall6

Dim bsSlot, bsAutoPlunger, bsRocket, mLeftMini, mRightMini, mslot, mLeftMagnet, mLowerRightMagnet, mUpperRightMagnet

Sub Table1_Init
	vpmInit Me
	With Controller
		.GameName = cGameName
		.Games(cGameName).Settings.Value("dmd_red") = 255
		.Games(cGameName).Settings.Value("dmd_green") = 255
		.Games(cGameName).Settings.Value("dmd_blue") = 255
		.Games(cGameName).Settings.Value("dmd_red66") = 155
		.Games(cGameName).Settings.Value("dmd_green66") = 155
		.Games(cGameName).Settings.Value("dmd_blue66") = 155
		.Games(cGameName).Settings.Value("dmd_red33") = 55    
		.Games(cGameName).Settings.Value("dmd_green33") = 55
		.Games(cGameName).Settings.Value("dmd_blue33") = 55
		.Games(cGameName).Settings.Value("dmd_red0") = 5
		.Games(cGameName).Settings.Value("dmd_green0") = 5
		.Games(cGameName).Settings.Value("dmd_blue0") = 5
		.SplashInfoLine = "Twilight Zone (Bally 1992)"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.Hidden = DesktopMode
		.HandleMechanics = 1' + 2 'Gumball + Clock	'just clock
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With

	' Init switches
	Controller.Switch(22) = 1 'close coin door

    '************  Main Timer init  ********************

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

    '************   Nudging   **************************

	vpmNudge.TiltSwitch = 14
	vpmNudge.Sensitivity = 4
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, Leftslingshot, Rightslingshot)

	'**************** Slot Machine Kickout ****************
	slotkick_vel = 42			'velocity 
	slotkick_vel_variance = 0.5	'velocity variance
	slotkick_angle = Loopy_New.objrotz	'adjust objrotz on loop mesh to adjust kickout direction
	slotkick_angle_variance = 0.0	'Angle variance

	'****************   Magnets   ******************

	Set mLeftMini = New cvpmMagnet
	With mLeftMini
		.InitMagnet TLMiniFlip, 60 		' Left Powerfield Real Magnet Strength (adjust to taste) - 70
		.GrabCenter = False: .Size = 195
		.CreateEvents "mLeftMini"
	End With

	Set mRightMini = New cvpmMagnet
	With mRightMini
		.InitMagnet TRMiniFlip, 60 		' Right Powerfield Real Magnet Strength (adjust to taste) - 70
		.GrabCenter = False: .Size = 195
		.CreateEvents "mRightMini"
	End With

	Set mLeftMagnet = New cvpmMagnet
	With mLeftMagnet
		.InitMagnet LeftMagnet, 67
		.CreateEvents "mLeftMagnet"
		.GrabCenter = True
	End With

	Set mUpperRightMagnet = New cvpmMagnet
	With mUpperRightMagnet
		.InitMagnet UpperRightMagnet, 63
		.CreateEvents "mUpperRightMagnet"
		.GrabCenter = True
	End With

	Set mLowerRightMagnet = New cvpmMagnet
	With mLowerRightMagnet
		.InitMagnet LowerRightMagnet, 63
		.CreateEvents "mLowerRightMagnet"
		.GrabCenter = True
	End With

	'Controller.Switch(55) = 0

	'****************   Init GI   ******************
	dim x

	UpdateGI 0, 1:UpdateGI 1, 1:UpdateGI 2, 1:UpdateGI 3, 1:UpdateGI 4, 1
	For each x in GIClock:x.IntensityScale = 0:next


	'for ballsearch
	 for each x in Array(sw84, sw85, sw88, sw58,sw18,sw25,sw16,sw15,sw17)	
		x.UserValue = cInt(mid(x.name, 3, 2))
	Next
	GumballPopper.uservalue = 74

	'************  Trough	**************************
	Set TZBall6 = sw17.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TZBall5 = sw16.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TZBall4 = sw15.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TZBall3 = FreezeKicker2.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TZBall2 = FreezeKicker1.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TZBall1 = FreezeKicker0.CreateSizedballWithMass(Ballsize/2,Ballmass)

	Controller.Switch(17) = 1
	Controller.Switch(16) = 1
	Controller.Switch(15) = 1
	Controller.Switch(26) = 1

'	Freezekicker2.kick 0,0,0
'	Freezekicker1.kick 0,0,0
'	Freezekicker0.kick 0,0,0

	If PowerballStart = 0 then PowerballStart = RndNum(1, 6) end if	' 0 = completely random powerball start
	If PowerballStart = 7 then PowerballStart = RndNum(1, 3) end if	' 7 = Random start in the gumball machine

	If powerballstart = 1 Then SetPowerBall TZBall1
	If powerballstart = 2 Then SetPowerBall TZBall2
	If powerballstart = 3 Then SetPowerBall TZBall3
	If powerballstart = 4 Then SetPowerBall TZBall4:Controller.switch(26) = 0
	If powerballstart = 5 Then SetPowerBall TZBall5
	If powerballstart = 6 Then SetPowerBall TZBall6
	

	
	'SpawnBalls	'spawn all balls in gumball machine and in trough

	'****************   Init flashers   ******************

	SetModLamp 117, 0
	SetModLamp 118, 0
	SetModLamp 119, 0
	SetModLamp 120, 0
	SetModLamp 128, 0
	SetModLamp 137, 0
	SetModLamp 138, 0
	SetModLamp 139, 0
	SetModLamp 140, 0
	SetModLamp 141, 0

	'************  FSS & VR	Backglass **************************

	set_FSS

	If Table1.ShowFSS = True or VROn Then
		'Do Nothing
		Wall002.visible=False
		Wall002.sidevisible=False
		light8halo.visible=false
		f40d.visible = false
	Else
		for each x in FSS: x.visible = false:next
	End If

End Sub


'******************************************************
' 						KEYS
'******************************************************

Dim BIPL				'balls in plunger

Sub Table1_KeyDown(ByVal keycode)
	If keycode = LeftFlipperKey Then 
		LFPress = 1
		If StagedFlipperMod <> 1 Then LFPress1 = 1
	End If
	If keycode = RightFlipperKey Then 
		RFPress = 1
		If StagedFlipperMod <> 1 Then RFPress1 = 1
	End If
	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperLeft Then LFPress1 = 1
		If keycode = KeyUpperRight Then RFPress1 = 1
	End If
	If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
	If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
	If keycode = CenterTiltKey Then Nudge 0, 3:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
	If Keycode = KeyFront Then Controller.Switch(23) = 1
	If KeyCode = PlungerKey Then Plunger.Pullback:PlaySoundAt "fx_plungerpull", Plunger
	If vpmKeyDown(KeyCode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If keycode = LeftFlipperKey Then 
		lfpress = 0
		leftflipper.eostorqueangle = EOSA
		leftflipper.eostorque = EOST
		If StagedFlipperMod <> 1 Then
			LFPress1 = 0
			leftflipper1.eostorqueangle = EOSA
			leftflipper1.eostorque = EOST
		End If
	End If
	If keycode = RightFlipperKey Then 
		rfpress = 0
		rightflipper.eostorqueangle = EOSA
		rightflipper.eostorque = EOST
		If StagedFlipperMod <> 1 Then
			RFPress1 = 0
			rightflipper1.eostorqueangle = EOSA
			rightflipper1.eostorque = EOST
		End If
	End If

	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperLeft Then 
			LFPress1 = 0
			leftflipper1.eostorqueangle = EOSA
			leftflipper1.eostorque = EOST
		End If
		If keycode = KeyUpperRight Then 
			RFPress1 = 0
			rightflipper1.eostorqueangle = EOSA
			rightflipper1.eostorque = EOST
		End If
	End If

	If KeyCode = KeyFront Then Controller.Switch(23) = 0
	If KeyCode = PlungerKey Then
		Plunger.Fire
		If BIPL = 1 Then
			PlaysoundAt "fx_launch", sw27
		Else
			PlaysoundAt "fx_plunger", sw27
		End If
	End If
	If vpmKeyUp(KeyCode) Then Exit Sub
End Sub

'************************************************
'*********   BallInit ***************************
'************************************************

Dim PowerBall, PowerBallID

Sub BallSearch() 'on hard pinmame reset check all these triggers and kickers
	dim x : for each x in Array(GumballPopper, sw84, sw85, sw88, sw58,sw18,sw25,sw16,sw15,sw17)
		if x.ballcntover then controller.Switch(x.uservalue) = True
	Next
	for each x in getballs : if x.y > 2500 then x.x = 204 : x.y = 519 : x.velx = 0 : x.vely = 0 : x.z = -30 : end if

	if sw15.ballcntover then 	'Fix the trough powerball detecting switch
		if sw15.lastcapturedball.id <> PowerBallID then controller.Switch(26) = 1
	end if

: Next	'reset balls that have fallen off the table
End Sub

Sub SetPowerBall(ball)
'	if IsObject(PowerballLocation) then 
'		Set PowerBall = PowerBallLocation.CreateSizedBall(Ballsize/2)
'	End If

	With ball
		.image = "powerball"
		.color = RGB(255,255,255)
		'.id = 666
		.Mass = 0.8*Ballmass
		.BulbIntensityScale = 0.05
	End With

	PowerBallID = ball.id
	Set PowerBall = ball
End Sub


sub ballupdate_timer()

	textbox.text = TZBall1.x & " " & TZBall1.y & " " & TZBall1.z  & vbnewline & _
		TZBall2.x & " " & TZBall2.y & " " & TZBall2.z  & vbnewline & _
		TZBall3.x & " " & TZBall3.y & " " & TZBall3.z  & vbnewline & _
		TZBall4.x & " " & TZBall4.y & " " & TZBall4.z  & vbnewline & _
		TZBall5.x & " " & TZBall5.y & " " & TZBall5.z  & vbnewline & _
		TZBall6.x & " " & TZBall6.y & " " & TZBall6.z 

end sub

'**************************************************************
' SOLENOIDS
'**************************************************************

'	  (*) - only in prototype, supported by rom 9.4
'	 (**) - the additional GUM and BALL flashers were removed ro reduce cost
'	(***) - Gumball and Clock Mechanics are handled by vpm classes

'standard coils
SolCallback(1) = "SlotMachineKickout"									'(01) Slot Kickout
SolCallback(2) = "SolRocket"												'(02) Rocket Kicker
SolCallback(3) = "SolAutoKicker"											'(03) Auto-Fire Kicker
SolCallback(4) = "SolGumballPopper"										'(04) Gumball Popper
SolCallback(5) = "SolRightRampDiverter"									'(05) Right Ramp Diverter
SolCallback(6) = "SolGumballDiverter"									'(06) Gumball Diverter
SolCallback(7) = "vpmSolSound SoundFX(""fx_knocker"",DOFKnocker),"		'(07) Knocker
SolCallback(8) = "SolOuthole"											'(08) Outhole
SolCallback(9) = "SolBallRelease"										'(09) Ball Release
'SolCallback(10) = "SolRightSling"										'(10) Right Slingshot
'SolCallback(11) = "SolLeftSling"										'(11) Left Slingshot
'SolCallback(12) = "SolLowerBumper"										'(12) Lower Jet Bumper
'SolCallback(13) = "SolLeftBumper"										'(13) Left Jet Bumper
'SolCallback(14) = "SolRightBumper"										'(14) Right Jet Bumper
SolCallback(15) = "LockKickout"											'(15) Lock Release nf
SolCallback(16) = "SolShootDiverter"									'(16) Shooter Diverter
SolModCallback(17) = "SetModLamp 117,"										'(17) Flasher bumpers x2	
SolModCallback(18) = "SetModLamp 118,"										'(18) Flasher Power Payoff x2
SolModCallback(19) = "SetModLamp 119,"										'(19) Flasher Mini-Playfield x2
SolModCallback(20) = "SetModLamp 120,"										'(20) Flasher Upper Left Ramp x2 (**)
SolCallback(21) = "SolLeftMagnet"										'(21) Left Magnet
SolCallback(22) = "SolUpperRightMagnet"									'(22) Upper Right Magnet (*)
SolCallback(23) = "SolLowerRightMagnet"									'(23) Lower Right Magnet
SolCallback(24) = "SolGumballMotor"										'(24) Gumball Motor
SolCallback(25) = "SolMiniMagnet mLeftMini,"							'(25) Left Mini-Playfield Magnet
SolCallback(26) = "SolMiniMagnet mRightMini,"							'(26) Right Mini-Playfield Magnet
SolCallback(27) = "SolLeftRampDiverter"									'(27) Left Ramp Diverter
SolModCallback(28) = "SetModLamp 128,"									'(28) Flasher Inside Ramp
'aux board coils
SolModCallback(51) = "SetModLamp 137,"										'(37) Flasher Upper Right Flipper
SolModCallback(52) = "SetModLamp 138,"										'(38) Flasher Gumball Machine Higher
SolModCallback(53) = "SetModLamp 139,"										'(39) Flasher Gumball Machine Middle
SolModCallback(54) = "SetModLamp 140,"										'(40) Flasher Gumball Machine Lower
SolModCallback(55) = "SetModLamp 141,"										'(41) Flasher Upper Right Ramp x2 (**)
'SolCallback(56) = ""													'(42) Clock Reverse (***)
'SolCallback(57) = ""													'(43) Clock Forward (***)
'SolCallback(58) = ""													'(44) Clock Switch Strobe (***)
'SolCallback(59) = "SolGumRelease"										'(??) Gumball Release (***)	'pinmame hack unreliable with solmodcallbacks
'fliptronic board
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sURFlipper) = "SolURFlipper"
SolCallback(sULFlipper) = "SolULFlipper"


'***********   Rocket   ********************************

Sub RocketKicker_Hit
	PlaysoundAtBallVol "fx_power", 0.5
	Controller.Switch(28) = 1
End Sub

Sub SolRocket(Enabled)
	If enabled Then 
		If RocketKicker.BallCntOver = 0 Then
			PlaySoundAt SoundFX(SSolenoidOn,DOFContactors), RocketKicker
		Else
			PlaySoundAtVol SoundFX("fx_rocket_exit",DOFContactors), 1, RocketKicker
		End If
		RocketKicker.kick 302 + (Rnd*6), 45 + (Rnd* 20)
		Controller.Switch(28) = 0
	End If
End Sub


'***********   Autoplunger   ********************************

Sub AutoPlungerKicker_Hit
	Controller.Switch(72) = 1
	PlaysoundAt "fx_Lock_enter", AutoPlungerKicker
End Sub

Sub SolAutoKicker(Enabled)
	If enabled Then 
		If AutoPlungerKicker.BallCntOver = 0 Then
			PlaySoundAt SoundFX("fx_AutoPlunger",DOFContactors), AutoPlungerKicker
		Else
			PlaySoundAtVol SoundFX("fx_launch",DOFContactors), 0.5, AutoPlungerKicker
		End If
		AutoPlungerKicker.kick 0, 52 + (Rnd* 16)
		Controller.Switch(72) = 0
	End If
End Sub

'***********   Gumball Popper   ******************************

Dim BIK:BIK=0		'ball in kicker

sub GumballPopper_Hit()
	Controller.Switch(74) = 1
	BIK=BIK+1
	PlaySoundAtBall "fx_kicker_catch"
end sub

sub GumballPopper_UnHit()
	Controller.Switch(74) = 0
	BIK=BIK-1
end sub

Sub GumballPopperHole_Hit
	PlaySoundAtVol "fx_Hole",0.5, GumballPopperHole
	vpmTimer.PulseSw 51
End Sub

Sub SolGumballPopper(enabled)	'VUK
    If enabled Then
		BallSearch
		GumballPopper.Kick 0, 65, 1.5
        If BIK = 0 Then
            PlaySoundAt SoundFX(SSolenoidOn,DOFContactors), GumballPopper
        Else
            PlaySoundAt SoundFX("fx_GumPop",DOFContactors), GumballPopper
        End If
    End If
End Sub

'**************   Drain  and Release  ************************************

Dim BIP:BIP = 0				'Balls In Play

sub sw18_hit()
	Controller.Switch(18) = 1:PlaySoundAtBallVol "fx_drain", 0.3
	BIP = BIP - 1
	If TVMod = 1 then TVTimer.enabled = 0:Frame.imageA = "tv_gameover"
end sub

sub sw18_unhit():controller.Switch(18) = 0:end sub

Sub SolOuthole(enabled)
	If Enabled Then
		'BallSearch
		sw18.kick 60, 9
		Updatetrough
		Playsoundat SoundFX(SSolenoidOn,DOFContactors), sw18
	End If
end sub

Sub SolBallrelease(enabled)
	If Enabled Then
		sw15.kick 60, 9
		Updatetrough					'this is important to reset trough intervals
		BIP = BIP + 1
	End If
End sub

'******************************************************
'						TROUGH 
'******************************************************

sub sw25_hit():controller.Switch(25) = 1:updatetrough:end sub
sub sw25_unhit():controller.Switch(25) = 0:updatetrough:end sub

sub sw17_hit():controller.Switch(17) = 1:updatetrough:end sub
sub sw17_unhit():controller.Switch(17) = 0:updatetrough:end sub

sub sw16_hit():controller.Switch(16) = 1:updatetrough:end sub
sub sw16_unhit():controller.Switch(16) = 0:updatetrough:end sub

sub sw15_hit()
	if activeball.id = PowerBallID then 	'opto handler
		controller.Switch(26) = 0	'if powerball
	Else	
		controller.Switch(26) = 1	'if regular ball
	end if
	controller.Switch(15) = 1
	updatetrough:
end sub
sub sw15_unhit()
	controller.Switch(15) = 0
	controller.switch(26) = 0
	playsoundat SoundFX("fx_ballrel",DOFContactors), sw15
	updatetrough
end sub

sub Updatetrough()
	updatetroughTimer.interval = 300
	updatetroughTimer.enabled = 1
end sub

sub updatetroughTimer_timer()
	if sw15.BallCntOver = 0 then sw16.kick 58, 8 end If
	if sw16.BallCntOver = 0 then sw17.kick 58, 8 end If
	if sw17.BallCntOver = 0 then sw25.Kick 58, 8 end If
	me.enabled = 0
end sub

'*******************   Lock   ******************

sub sw85_hit():controller.Switch(85) = 1:PlaysoundAt "fx_Lock_enter", sw85:updatelock:end sub
sub sw85_unhit():controller.Switch(85) = 0:end sub
sub sw84_hit():controller.Switch(84) = 1:updatelock:end sub
sub sw84_unhit():controller.Switch(84) = 0:end sub
sub sw88_hit():controller.Switch(88) = 1:updatelock:end sub
sub sw88_unhit():controller.Switch(88) = 0:end sub

sub lockramp_hit
	PlaySoundAtBallVolME "fx_metal_ramp_hit", 0.5
end sub

sub updatelock
	updatelocktimer.interval = 32
	updatelocktimer.enabled = 1
end sub

sub updatelocktimer_timer()
	if sw88.BallCntOver = 0 then sw84.kick 180, 2 end If
	if sw84.BallCntOver = 0 then sw85.kick 180, 2 end if
	me.enabled = 0
end sub

sub LockKickout(enabled)
	If enabled then
	sw88.kick 88, 10
	Playsoundat SoundFX("fx_Lock_exit",DOFContactors), sw88
	updatelock
	End If
end sub

'******************************************************************
'** SUBWAY, SHOOTER LANE, SLOTMACHINE; CAMERA, PIANO, DEAD END  ***
'******************************************************************

Sub SubwaySound(dummy)
	PlaySoundat "fx_subway", sw57
End sub

'********	Slot Machine	****************************************

Sub SlotMachine_Hit()
    PlaySoundat "fx_SlotM_enter", slotMachine
End Sub

Sub sw57_Hit()											'submarine switch, Tslot proximity
	'debug.print activeball.id  & " " &  powerballid
	if activeball.id <> PowerBallID then
		vpmTimer.PulseSw 57
	end if	
end Sub

Sub Sw58_Hit() 
	Controller.Switch(58) = 1
	SlotKickerOverflow.Enabled = True
	Playsoundat "fx_kicker_catch", sw58
End Sub

Sub Sw58_UnHit() 
	Controller.Switch(58) = 0
	SlotKickerOverflow.Enabled = False
End Sub

dim slotkick_vel, slotkick_vel_variance
dim slotkick_angle, slotkick_angle_variance

sub slotmachinekickout(enabled)
	if enabled Then
		Playsoundat SoundFX("fx_SlotM_exit",DOFContactors), sw58
		'If SlotKickerOverflow.ballcntover > 0 Then
			SlotKickerOverflow.Kick KickoutVariance(slotkick_angle,slotkick_angle_variance), KickoutVariance(slotkick_vel, slotkick_vel_variance)
		'else
			sw58.Kick KickoutVariance(slotkick_angle,slotkick_angle_variance), KickoutVariance(slotkick_vel, slotkick_vel_variance)
		'end if
	end if
end sub

Function KickoutVariance(aNumber, aVariance)	'strength, variance
	KickoutVariance = aNumber + ((Rnd*2)-1)*aVariance
End Function





'********	Shooter Lane	***************************************

Sub ShooterLaneKicker_Hit
    PlaySoundAtVol "fx_hole", 0.5,  ShooterLaneKicker
	vpmtimer.addtimer 100, "SubwaySound"
End Sub

'********   Dead End   ********************************************

Sub DeadEnd_Hit
    vpmTimer.PulseSw 41
	PlaySoundAtVol "fx_DeadEnd", 0.5, DeadEnd
	vpmtimer.addtimer 100, "SubwaySound"
End Sub

'********   Camera   ***********************************************

Sub CameraKicker_Hit
	PlaySoundAtVol "fx_hole", 0.5, CameraKicker
	vpmtimer.addtimer 100, "SubwaySound"
End Sub

Sub sw42_Hit():vpmTimer.PulseSw 42:end Sub		'submarine switch, camera / upper playfield

Sub Hitch001_hit():PlaySoundAtBallVol "fx_lr2", 0.5:End Sub
Sub Hitch002_hit():PlaySoundAtBallVol "fx_lr3", 0.5:End Sub
Sub Hitch003_hit():PlaySoundAtBallVol "fx_lr4", 0.5:End Sub
Sub Hitch004_hit():If activeball.vely < 0 then:PlaySoundAtBallVol "fx_lr5", 0.5:End If:End Sub

'********  Piano   *************************************************

Sub Piano_Hit()
    PlaySoundAtVol "fx_Piano", 0.5, Piano
	vpmtimer.addtimer 100, "SubwaySound"
End Sub

Sub sw43_Hit():vpmTimer.PulseSw 43:end Sub		'submarine switch, piano

''************************************************************************************
''*****************       SLINGSHOTS                      ****************************
''************************************************************************************

Dim RStep, LStep

Sub LeftSlingShot_Slingshot
	PlaySoundat SoundFX("fx_slingshotL",DOFContactors), sling2
	LSling.Visible = 0
	LSling1.Visible = 1
	sling2.TransZ = -20
	LStep = 0
	Me.TimerEnabled = 1
	vpmTimer.PulseSw 34
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	PlaySoundAt SoundFX("fx_slingshotR",DOFContactors), Sling1
	RSling.Visible = 0
	RSling1.Visible = 1
	sling1.TransZ = -20
	RStep = 0
	Me.TimerEnabled = 1
	vpmTimer.PulseSw 35
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

''************************************************************************************
''*****************               Bumpers                 ****************************
''************************************************************************************

Dim bump1, bump2, bump3

Sub Bumper1_Hit
    vpmTimer.PulseSw 31
    PlaySoundAt SoundFX("fx_BumperLeft", DOFContactors), Bumper1
    bump1 = 1:Me.TimerEnabled = 1
End Sub

Sub Bumper1_Timer()
    Select Case bump1
        Case 1:BR1.Z = 15:bump1 = 2
        Case 2:BR1.Z = 25:bump1 = 3
        Case 3:BR1.Z = 35:bump1 = 4
        Case 4:BR1.Z = 45:Me.TimerEnabled = 0
    End Select
End Sub

Sub Bumper2_Hit
    vpmTimer.PulseSw 32
    PlaySoundAt SoundFX("fx_BumperRight", DOFContactors), Bumper2
    bump2 = 1:Me.TimerEnabled = 1
End Sub

Sub Bumper2_Timer()
    Select Case bump2
        Case 1:BR2.Z = 15:bump2 = 2
        Case 2:BR2.Z = 25:bump2 = 3
        Case 3:BR2.Z = 35:bump2 = 4
        Case 4:BR2.Z = 45:Me.TimerEnabled = 0
    End Select
End Sub

Sub Bumper3_Hit
    vpmTimer.PulseSw 33
    PlaySoundAt SoundFX("fx_BumperMiddle", DOFContactors), Bumper3
    bump3 = 1:Me.TimerEnabled = 1
End Sub

Sub Bumper3_Timer()
    Select Case bump3
        Case 1:BR3.Z = 15:bump3 = 2
        Case 2:BR3.Z = 25:bump3 = 3
        Case 3:BR3.Z = 35:bump3 = 4
        Case 4:BR3.Z = 45:Me.TimerEnabled = 0
    End Select
End Sub

'*******************************************************************
'****************   Diverters   ************************************
'*******************************************************************

Sub solShootDiverter(Enabled)
    If Enabled Then
        shooterdiverter.rotatetoend : PlaysoundAt SoundFX("fx_DivSS",DOFContactors), ShooterDiverter
    Else
        shooterdiverter.rotatetostart
	End If
End Sub

Sub SolGumballDiverter (enabled)
    If Enabled Then
		GumballDiverter.rotatetoend : PlaysoundAt SoundFX("fx_DivGM",DOFContactors), GumballDiverter
	Else
		GumballDiverter.rotatetostart
	End If
End Sub

'********  Left Ramp Diverter   **********************

Sub SolLeftRampDiverter(enabled)
	If Enabled Then
		PlaysoundAt SoundFX("fx_DivLR",DOFContactors), Plunger1
		RampDivWall.IsDropped=1
		RampDiverter.RotatetoEnd
		Plunger1.pullback
	Else
		RampDivWall.IsDropped=0
		RampDiverter.Rotatetostart
		Plunger1.fire
	End If
End Sub

'********  Right Ramp Diverter   **********************

Dim divDir, divPos
divPos = 0

Dim KickerBall:Kickerball = Empty

Sub divWall_Hit()
	StopSound "fx_metalrolling"
	PlaySoundAtBallVol "fx_metalHit", 0.1
	If activeball.velx > 6 then activeball.velx = 6
End Sub

Sub divTrig_Hit()
	Set KickerBall = Activeball
End Sub

Sub divTrig_unHit()
	KickerBall = Empty
End Sub

Sub SolRightRampDiverter(enabled)
	If enabled Then
		Playsoundat SoundFX("fx_DivRR",DOFContactors), DivTrig
		if Not IsEmpty(Kickerball) Then
			Kickball Kickerball, -10, 10, 0, 50
		End If
		divDir = 9
		Kickerball = Empty
	Else
		divDir = -9
	End If
	DiverterTimer.Enabled = 1
	DivWall.collidable = not enabled
End Sub

Sub diverterTimer_Timer()
    divPos = divPos + divDir
    If divPos > 90 Then
        divPos = 90
        DiverterTimer.Enabled = 0
    End If
    If divPos < 0 Then
        divPos = 0
        diverterTimer.Enabled = 0
    End If
    RDiv.RotX = divPos
    SpiralToy.RotX = divPos
End Sub

' set KickerBall = ActiveBall
' Kickerball = Empty
' If Not IsEmpty(KickerBall) Then

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180

	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub

'******************************************************
'						FUNCTIONS
'******************************************************

'*** PI returns the value for PI
Function PI()
	PI = 4*Atn(1)
End Function

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


'*******************************************************************
'*************************       Targets        ********************
'*******************************************************************

Sub sw47_Hit:vpmTimer.PulseSw 47:PlaySoundAtBallVol SoundFX("fx_target",DOFContactors), Vol(ActiveBall)*VolumeDial:End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:PlaySoundAtBallVol SoundFX("fx_target",DOFContactors), Vol(ActiveBall)*VolumeDial:End Sub
Sub sw64_Hit:vpmTimer.PulseSw 64:PlaySoundAtBallVol SoundFX("fx_target",DOFContactors), Vol(ActiveBall)*VolumeDial:End Sub
Sub sw65_Hit:vpmTimer.PulseSw 65:PlaySoundAtBallVol SoundFX("fx_target",DOFContactors), Vol(ActiveBall)*VolumeDial:End Sub
Sub sw65a_Hit:vpmTimer.PulseSw 65:PlaySoundAtBallVol SoundFX("fx_target",DOFContactors), Vol(ActiveBall)*VolumeDial:End Sub
Sub sw66_Hit:vpmTimer.PulseSw 66:PlaySoundAtBallVol SoundFX("fx_target",DOFContactors), Vol(ActiveBall)*VolumeDial:End Sub
Sub sw67_Hit:vpmTimer.PulseSw 67:PlaySoundAtBallVol SoundFX("fx_target",DOFContactors), Vol(ActiveBall)*VolumeDial:End Sub
Sub sw68_Hit:vpmTimer.PulseSw 68:PlaySoundAtBallVol SoundFX("fx_target",DOFContactors), Vol(ActiveBall)*VolumeDial:End Sub
Sub sw77_Hit:vpmTimer.PulseSw 77:PlaySoundAtBallVol SoundFX("fx_target",DOFContactors), Vol(ActiveBall)*VolumeDial:End Sub
Sub sw78_Hit:vpmTimer.PulseSw 78:PlaySoundAtBallVol SoundFX("fx_target",DOFContactors), Vol(ActiveBall)*VolumeDial:End Sub

'******************************************************
'***************   Mini PF Switches *******************
'******************************************************

Sub sw44_Hit:vpmTimer.PulseSw 44: End Sub
Sub sw45_Hit:vpmTimer.PulseSw 45 : End Sub
Sub sw45a_Hit:vpmTimer.PulseSw 45 : End Sub
Sub sw46_Hit:vpmTimer.PulseSw 46 : End Sub
Sub sw46a_Hit:vpmTimer.PulseSw 46 : End Sub
Sub sw75_Hit:vpmTimer.PulseSw 75 : End Sub
Sub sw75_UnHit
	if activeball.vely < 0 Then  PlaySoundat "fx_power", sw75
End Sub
Sub sw76_Hit:vpmTimer.PulseSw 76: End Sub


'******************************************************
'***************  Ramps Switches **********************
'******************************************************

Sub sw53_Hit:vpmTimer.PulseSw 53:PlaySoundAt "fx_Gate", sw53 : End Sub
Sub sw54_Hit:vpmTimer.PulseSw 54 : PlaySoundAt "fx_Gate",sw54 : LRampSw.rotatetoend : End Sub
Sub sw54_UnHit: LRampSw.rotatetostart : End Sub
Sub sw73_Hit:vpmTimer.PulseSw 73: Playsoundatball "fx_metalrolling" : End Sub

'**************************************************************
'***************  Rollover Switches   *************************
'**************************************************************

Sub sw11_Hit:vpmTimer.PulseSw 11:PlaySoundAtBall "fx_sensor": End Sub
Sub sw12_Hit:vpmTimer.PulseSw 12:PlaySoundAtBall "fx_sensor" : End Sub
Sub sw36_Hit:vpmTimer.PulseSw 36:PlaySoundAtBall "fx_sensor" : End Sub
Sub sw37_Hit:vpmTimer.PulseSw 37:PlaySoundAtBall "fx_sensor" : End Sub
Sub sw38_Hit:vpmTimer.PulseSw 38:PlaySoundAtBall "fx_sensor" : End Sub
Sub sw52_Hit:vpmTimer.PulseSw 52:PlaySoundAtBall "fx_sensor" : End Sub
Sub sw56_Hit:vpmTimer.PulseSw 56:PlaySoundAtBall "fx_sensor" : End Sub
Sub sw61_Hit:vpmTimer.PulseSw 61:PlaySoundAtBall "fx_sensor" : End Sub
Sub sw62_Hit:vpmTimer.PulseSw 62:PlaySoundAtBall "fx_sensor" : End Sub
Sub sw63_Hit:vpmTimer.PulseSw 63:PlaySoundAtBall "fx_sensor" : End Sub

'**************************************************************
'***************  Opto Switches   *********************
'**************************************************************

Sub sw81_Hit:Controller.Switch(81) = 1:End Sub
Sub sw81_UnHit
	Controller.Switch(81) = 0
	If mLowerRightMagnet.MagnetOn and activeball.id <> PowerBallID then
		activeball.vely = 0
		activeball.velx = 0
	End If
End Sub

Sub sw82_Hit:Controller.Switch(82) = 1:End Sub
Sub sw82_UnHit
	Controller.Switch(82) = 0
	If mUpperRightMagnet.MagnetOn and activeball.id <> PowerBallID then
		activeball.vely = 0
		activeball.velx = 0
	End If
End Sub

Sub sw83_Hit:Controller.Switch(83) = 1:End Sub
Sub sw83_UnHit
	Controller.Switch(83) = 0
	If mLeftMagnet.MagnetOn and activeball.id <> PowerBallID then
		activeball.vely = 0
		activeball.velx = 0
	End If
End Sub

'Clock Pass Opto (only in prototypes,supported by rom version 9.4)
Sub sw86_Hit:Controller.Switch(86) = 1:End Sub
Sub sw86_UnHit:Controller.Switch(86) = 0:End Sub
'Gumball entry opto
sub sw87_hit():controller.switch(87) = 1:end Sub
sub sw87_unhit():controller.switch(87) = 0:end Sub
''Autoplunger 2nd Opto (only in prototypes,supported by rom version 9.4)
'Sub sw71_Hit:Controller.Switch(71) = 1:End Sub
'Sub sw71_UnHit:Controller.Switch(71) = 0:End Sub

'*************************************************************
'***************   Shooting lane   ***************************
'*************************************************************

Sub sw27_Hit():controller.switch(27) = 1:BIPL = 1
If TVMod = 1 then TVTimer.enabled = 1:Frame.imageA = "tv_1"
End Sub

Sub sw27_UnHit():controller.switch(27) = 0:BIPL = 0:End Sub

'******************************************************
'					FLIPPERS
'******************************************************

Sub SolLFlipper(Enabled)
	If Enabled Then
		PlaySoundat SoundFX("fx_flipperUp",DOFContactors), LeftFlipper
		LF.Fire'LeftFlipper.RotateToEnd
	Else
		If leftflipper.currentangle < leftflipper.startangle - 5 then 
			PlaySoundat SoundFX("fx_flipperdown",DOFContactors), LeftFlipper
		End If
		LeftFlipper.RotateToStart
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		PlaySoundat SoundFX("fx_flipperUp",DOFContactors), RightFlipper
		RF.Fire 'RightFlipper.RotateToEnd
	Else
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			PlaySoundAt SoundFX("fx_flipperdown",DOFContactors), RightFlipper
		End If
		RightFlipper.RotateToStart
	End If
End Sub

Sub SolULFlipper(Enabled)
	If Enabled Then
		If StagedFlipperMod = 1 Then PlaySoundAt SoundFX("fx_flipperUp",DOFContactors), LeftFlipper1
		LeftFlipper1.RotateToEnd
	Else
		If StagedFlipperMod = 1 Then 
			If leftflipper1.currentangle < leftflipper1.startangle - 5 then 
				PlaySoundat SoundFX("fx_flipperdown",DOFContactors), LeftFlipper1
			End If
		End If
		LeftFlipper1.RotateToStart
	End If
End Sub

Sub SolURFlipper(Enabled)
	If Enabled Then
		If StagedFlipperMod = 1 Then PlaySoundat SoundFX("fx_flipperUp",DOFContactors), RightFlipper1
		RightFlipper1.RotateToEnd
	Else
		If StagedFlipperMod = 1 Then 
			If RightFlipper1.currentangle > RightFlipper1.startAngle + 5 Then
				PlaySoundat SoundFX("fx_flipperdown",DOFContactors), RightFlipper1
			End If
		End If
		RightFlipper1.RotateToStart
	End If
End Sub

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricksL LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricksL LeftFlipper1, LFPress1, LFCount1, LFEndAngle1, LFState1
	FlipperTricksR RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperTricksR RightFlipper1, RFPress1, RFCount1, RFEndAngle1, RFState1
end sub

dim LFPress, RFPress, LFPress1, RFPress1, LFCount, LFCount1, RFCount, RFCount1
dim LFState, LFState1, RFState, RFState1
dim EOST, EOSA,Frampup, FElasticity
dim RFEndAngle, RFEndAngle1, LFEndAngle, LFEndAngle1

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
Const EOSTnew = 1.0 'FEOST
Const EOSAnew = 0.2
Const EOSRampup = 1.5 
Const SOSRampup = 8.5 
Const LiveCatch = 8
Const LiveElasticity = 0.45
Const SOSEM = 0.815

LFEndAngle = Leftflipper.endangle
LFEndAngle1 = Leftflipper1.endangle
RFEndAngle = RightFlipper.endangle
RFEndAngle1 = RightFlipper1.endangle

Sub FlipperTricksR (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	If Flipper.currentangle < Flipper.startangle + 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle + 3
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Flipper.currentangle >= Flipper.endangle and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		if GameTime - FCount < LiveCatch Then
			Flipper.Elasticity = LiveElasticity
		elseif GameTime - FCount < LiveCatch * 2 Then
			Flipper.Elasticity = 0.1
		Else
			Flipper.Elasticity = FElasticity
		end if

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup			
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Flipper.currentangle < Flipper.endangle - 0.01 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Sub FlipperTricksL (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	If Flipper.currentangle > Flipper.startangle - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle + 3
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Flipper.currentangle <= Flipper.endangle and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		if GameTime - FCount < LiveCatch Then
			Flipper.Elasticity = LiveElasticity
		elseif GameTime - FCount < LiveCatch * 2 Then
			Flipper.Elasticity = 0.1
		Else
			Flipper.Elasticity = FElasticity
		end if

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup			
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Flipper.currentangle > Flipper.endangle + 0.01 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

'*********************************************************************
'           Game Timer, Ball Rolling, Ball Shadows, Ball Drop
'*********************************************************************

dim FrameTime, InitFrameTime

Const tnob = 10 ' total number of balls
ReDim rolling(tnob)
InitRolling

Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8,BallShadow9,BallShadow10,BallShadow11)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = FALSE
	Next
End Sub

Sub GameTimer_timer()

	Dim BOT, b
	BOT = GetBalls

	' play the rolling sound for each ball
	For b = 0 to UBound(BOT)
		If BallSpeed(BOT(b) ) > 1 AND BOT(b).z < 27 and BOT(b).z > 20  Then
			rolling(b) = True
			PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b))*RollingSoundFactor, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			if BOT(b).y < 1000 and (InRect(BOT(b).x, BOT(B).y, 915,358,859,30,1001,101,1004,362) _
				or InRect(BOT(b).x, BOT(B).y,765,103,754,25,864,34,879,125)_
				or InRect(BOT(b).x, BOT(B).y,724,315,358,122,755,23,761,103)_
				or InRect(BOT(b).x, BOT(B).y,343,131,463,223,393,478,270,551)_
				or InRect(BOT(b).x, BOT(B).y,307,1000,89,766,74,593,392,901)_
				or InRect(BOT(b).x, BOT(B).y,72,593,156,314,213,418,142,678)_
				or InRect(BOT(b).x, BOT(B).y,12,964,2,326,149,329,165,914)_
			) Then
					PlaySound("fx_metalwall" & b), -1, Vol(BOT(b))*RollingSoundFactor*VolumeDial/5, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			Else
				StopSound("fx_metalwall" & b)
			End If
		Else
			If rolling(b) = True Then
				StopSound("fx_ballrolling" & b)
				StopSound("fx_metalwall" & b)
				rolling(b) = False
			End If
		End If

		

		'***Ball Shadows***	
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10

		If BOT(b).Z > 24 and BOT(b).Z < 35 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If

		'***Ball Drop Sounds***
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			PlaySound "fx_ball_drop" & b, 0, (ABS(BOT(b).velz)/17)^2, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If
	Next

	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime
	UpdateClock

	FlipperL.RotZ = LeftFlipper.CurrentAngle
	LogoL.RotZ = LeftFlipper.CurrentAngle
	FlipperR.RotZ = RightFlipper.CurrentAngle
	LogoR.RotZ = RightFlipper.CurrentAngle
	FlipperL1.RotY = LeftFlipper1.CurrentAngle
	LogoL1.RotZ = LeftFlipper1.CurrentAngle
	FlipperR1.RotZ = RightFlipper1.CurrentAngle
	LogoR1.RotZ = RightFlipper1.CurrentAngle
	ShooterDiv.RotY = ShooterDiverter.CurrentAngle
	DiverterP.RotZ = RampDiverter.CurrentAngle
	DiverterP1.RotZ = GumballDiverter.CurrentAngle
	sw53p.RotX = LRampG.CurrentAngle
	sw54p.RotX = LRampSw.CurrentAngle

	cor.update

End Sub

'********************************************************************************
'******************  NFOZZY'S GUMBALL MACHINE  2 ********************************
'********************** UPDATED BY ROTHBAUERW ***********************************
'********************************************************************************

Sub GumKickout()	'unfreeze balls in gumball machine trough
	Freezekicker2.enabled = false
	Freezekicker1.enabled = false
	Freezekicker0.enabled = false

	Freezekicker2.kick 0,0,0
	Freezekicker1.kick 0,0,0
	Freezekicker0.kickz 0,0,0,-25

	FreezeKicker0.timerenabled = true
End Sub

FreezeKicker0.TimerInterval= 80	'interval for kickout, how long the dropwall stays down. Adjust me if it kicks out 2, or none.

Sub FreezeKicker0_Timer()	'repop gumball floor after a short delay
	Freezekicker0.enabled = true
	If Not FreezeKicker1.enabled then 
		If CheckGumball(TZBall1) or CheckGumball(TZBall2) or CheckGumball(TZBall3) or CheckGumball(TZBall4) or CheckGumball(TZBall5) or CheckGumball(TZBall6) then 
			Freezekicker1.enabled = true
		End If
	Elseif Not FreezeKicker2.enabled Then
		If CheckGumball2(TZBall1) or CheckGumball2(TZBall2) or CheckGumball2(TZBall3) or CheckGumball2(TZBall4) or CheckGumball2(TZBall5) or CheckGumball2(TZBall6) then 
			Freezekicker2.enabled = true
			me.timerenabled = 0			
		End If
	End If
End Sub

Function CheckGumball(ball)
	If Int(ball.x) = 211 and Int(ball.y) = 276 and Int(ball.z) = 171 Then
		CheckGumball = True
	Else
		CheckGumball = False
	End If
End Function

Function CheckGumball2(ball)
	If Int(ball.x) = 199 and Int(ball.y) = 236 and Int(ball.z) = 198 Then
		CheckGumball2 = True
	Else
		CheckGumball2 = False
	End If
End Function

Sub SolGumRelease(enabled)	'this is a pinmame hack, will not work with solmodcallbacks. Called from motor sol instead.
    If enabled Then
		GumKickout 					'new
		vpmtimer.PulseSw 55	'Geneva switch
    End If
End Sub

Sub SolGumballMotor(aOn)
	if aOn then PlaySoundat SoundFX("fx_GumMachine",DOFGear), FreezeKicker0 : vpmtimer.addtimer 1400, "GumKnobTimer.enabled = 1'" : vpmtimer.addtimer 1700, "SolGumRelease 1'" 				
End Sub

GumKnobTimer.Interval = -1
Sub GumKnobTimer_Timer()	'prior 20ms period
	GumballMachineKnob.RotY= GumballMachineKnob.RotY + 1 * frametime
	If GumballMachineKnob.RotY >  360 then GumKnobTimer.enabled = 0 : GumballMachineKnob.RotY = 0
End Sub

'********************************************************************
'*************************   CLOCK   ********************************
'********************************************************************

Dim LastTime : LastTime = 0
dim LastClockIndex

Sub UpdateClock()
    Dim Time, Min, Hour, temp
    'Time = CInt(Controller.GetMech(0) )
    Time = Controller.GetMech(0)
    If Time <> LastTime Then
		Min = (Time Mod 60)
		Hour = Int(Time / 2)
		ClockShort.RotY = Hour - 45
		ClockLarge.RotY = min * 6
		Clock_mech.RotY = min * 6
		LastTime = Time	'10.4 playsound args - name,loopcount,volume,pan,randompitch,pitch,UseExisting,Restart,Fade
		PlaySound SoundFXDOF("fx_motor",101,DOFPulse,DOFGear), -1, 1, 0.05, 0, 0, 1, 0
		LastClockIndex = 0
	Elseif LastClockIndex <=2 Then	'wait an update before stopping motor sound
		LastClockIndex = LastClockIndex + 1
	Elseif LastClockIndex > 2 then 
		Stopsound "fx_motor"
    End If
End Sub

'**********************************************************************
'**************   POWER FIELD MAGNETS *********************************
'**********************************************************************

Sub SolMiniMagnet(aMag, enabled)
	If enabled Then
		PlaySoundat SoundFX("fx_magnet",DOFShaker), sw76
		With aMag
			.removeball PowerBall
			.MagnetOn = True
			.Update
			.MagnetOn = False
		End With
	End If
End Sub

'**********************************************************************
' SPECIAL CODE BY NFOZZY TO HANDLE MAGNET TRIGGERS
' based on the code by KIEFERSKUNK/DORSOLA
' Method: on extra triggers unhit, kill the velocity of the
' ball if the magnet is on, helping the magnet catch the ball.
'**********************************************************************

Sub sw81_help_unhit
	If activeball.vely > 28 then  activeball.vely = RndNum (26,27)			'-ninuzzu- Let's slow down the ball a bit so the magnets can
	If activeball.vely < - 28 then  activeball.vely = - RndNum (26,27)		'catch the ball
	If mLowerRightMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely * -0.2
		activeball.velx = activeball.velx * -0.2
	End If
End Sub

Sub LowerRightMagnet_hit()
	If mLowerRightMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely/10
		activeball.velx = activeball.velx/10
	End If
End Sub

Sub sw82_help_unhit
	If mUpperRightMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely * -0.2
		activeball.velx = activeball.velx * -0.2
    End If
End Sub

Sub UpperRightMagnet_hit()
	If mUpperRightMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely/10
		activeball.velx = activeball.velx/10
	End If
End Sub

Sub sw83_help_unhit
	If mLeftMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely * -0.2
		activeball.velx = activeball.velx * -0.2
	End If
End Sub

Sub LeftMagnet_hit()
	If mLeftMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely/10
		activeball.velx = activeball.velx/10
	End If
End Sub

Sub SolLeftMagnet(enabled)
	If enabled Then
		mLeftMagnet.MagnetOn = 1
		mleftmagnet.removeball PowerBall
		PlaySoundat SoundFX("fx_magnet_catch",DOFShaker), sw83
	Else
		mLeftMagnet.MagnetOn = 0
	End If
End Sub

Sub SolUpperRightMagnet(enabled)
	If enabled Then
		mUpperRightMagnet.MagnetOn = 1
		mUpperRightMagnet.removeball PowerBall
		PlaySoundat SoundFX("fx_magnet_catch",DOFShaker), sw82
	Else
		mUpperRightMagnet.MagnetOn = 0
	End If
End Sub

Sub SolLowerRightMagnet(enabled)
	If enabled Then
		mLowerRightMagnet.MagnetOn = 1
		mLowerRightMagnet.removeball PowerBall
		PlaySoundat SoundFX("fx_magnet_catch",DOFShaker), sw81
	Else
		mLowerRightMagnet.MagnetOn = 0
	End If
End Sub

'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 20 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
    UpdateLamps
End Sub

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0         ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4       ' used to track the fading state
        FlashSpeedUp(x) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.35 ' slower speed when turning off the flasher
        FlashMax(x) = 1          ' the maximum value when on, usually 1
        FlashMin(x) = 0          ' the minimum value when off, usually 0
        FlashLevel(x) = 0        ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub UpdateLamps
	NFadeLm 11, l11
	Flash 11, l11r
	NFadeLm 12, l12
	Flash 12, l12r
	NFadeLm 13, l13
	Flash 13, l13r
	NFadeLm 14, l14
	Flash 14, l14r
	NFadeLm 15, l15
	Flash 15, l15r
	NFadeLm 16, l16
	Flashm 16, FlL16a '*
	Flashm 16, FlL16b '*
	Flash 16, l16r
	NFadeLm 17, l17
	NFadeLm 17, l17a1 	'TownSquarePost lights
	NFadeLm 17, l17a2 	'TownSquarePost lights
	Flashm 17, l17r		'TownSquarePost lights
	Flash 17, l17r2
	NFadeLm 18, l18
	Flash 18, l18r

	NFadeLm 21, l21
	Flash 21, l21r
	NFadeLm 22, l22
	Flash 22, l22r
	NFadeLm 23, l23
	Flash 23, l23r
	NFadeLm 24, l24
	Flashm 24, FlL24 '*
	Flash 24, l24r
	NFadeLm 25, l25
	Flash 25, l25r
	NFadeLm 26, l26
	Flash 26, l26r
	NFadeLm 27, l27
	NFadeLm 27, l27a	'SlotMachine Lights
	Flashm 27, l27b		'SlotMachine Lights
	Flash 27, l27r
	NFadeLm 28, l28
	Flash 28, l28r

	NFadeLm 31, l31
	Flashm 31, l31r
	Flash 31, l31halo
	NFadeLm 32, l32
	Flash 32, l32r
	NFadeLm 33, l33
	Flash 33, l33r
	NFadeLm 34, l34
	Flash 34, l34r
	NFadeLm 35, l35
	Flash 35, l35r
	NFadeLm 36, l36
	Flash 36, l36r
	NFadeLm 37, l37
	Flash 37, l37r
	NFadeLm 38, l38
	Flash 38, l38r

	NFadeLm 41, l41
	Flash 41, l41r
	NFadeLm 42, l42
	Flash 42, l42r
	NFadeLm 43, l43
	Flash 43, l43r
	NFadeLm 44, l44
	Flashm 44, FlL44a '*
	Flashm 44, FlL44b '*
	Flash 44, l44r
	NFadeLm 45, l45
	Flash 45, l45r
	NFadeLm 46, l46
	Flash 46, l46halo
	NFadeLm 47, l47
	Flash 47, l47r
	NFadeLm 48, l48
	Flash 48, l48r

	NFadeLm 51, l51
	Flash 51, l51r
	NFadeLm 52, l52
	Flash 52, l52r
	NFadeLm 53, l53
	Flashm 53, FlL53a '*
	Flashm 53, FlL53b  '*
	Flashm 53, FlL53c  '*
	Flashm 53, FlL53d  '*
	Flashm 53, FlL53f  '*
	Flash 53, l53r
	NFadeLm 54, l54
	Flashm 54, FlL54  '*
	Flash 54, l54r
	NFadeLm 55, l55
	Flash 55, l55a		'Camera lights
	NFadeLm 56, l56
	Flashm 56, l56a		'Pyramid lights 
	Flash 56, l56r
	NFadeLm	57, l57
	Flash 57, l57r
	NFadeLm 58, l58
	Flash 58, l58r

	NFadeLm 61, l61
	NFadeLm 61, l61a
	NFadeLm 61, l61b
	Flashm 61, FlL61  '*
	Flashm 61, l61c
	Flash 61, l61d
	NFadeLm 62, l62
	NFadeLm 62, l62a
	NFadeLm 62, l62b
	Flash  62, FlL62  '*
	NFadeLm 63, l63
	NFadeLm 63, l63a
	NFadeLm 63, l63b
	Flashm 63, l63c
	Flash 63, FlL63  '*
	NFadeLm 64, l64
	Flash 64, l64r
	NFadeLm 65, l65
	Flash 65, l65r
	NFadeLm 66, l66
	Flashm 66, l66r
	Flash 66, l66halo
	NFadeL 67, l67
	NFadeLm 68, l68
	Flash 68, l68r

	NFadeLm 71, l71
	Flash 71, l71r
	NFadeLm 72, l72
	Flash 72, l72r
	NFadeLm 73, l73
	Flash 73, l73r
	NFadeLm 74, l74
	Flashm 74, l74r
	Flashm 74, FlL74a  '*
	Flash 74, FlL74b  '*
	NFadeLm 75, l75
	Flash 75, l75r
	NFadeLm 76, l76
	Flash 76, l76r
	NFadeLm 77, l77
	Flash 77, l77rr
	NFadeLm 78, l78
	Flash 78, l78r

	NFadeLm 81, l81
	NFadelm 81, l81halo
	FadeDisableLighting 81, bulb3, 0.85
	Nfadel 81, l81ref
	NFadeLm 82, l82
	NFadeLm 82, l82a 	'Clock Toy Mod lights
	Flashm 82, l82r   	'Clock Toy Mod lights
	Flash 82, l82halo
	NFadeLm 83, l83a
	NfadeLm 83, l83halo
	FadeDisableLighting 83, bulb2, 0.85
	NfadeL 83, l83ref
	NFadeLm 84, l84a
	NFadeLm 84, l84b
	FadeDisableLighting 84, bulb1, 0.20
	NfadeL 84, l84ref
	NFadeLm 85, l85
	NFadeLm 85, l85halo
	NFadeLm 85, l85scooplight
	FadeDisableLighting 85, bulb5, 0.85
	Flashm 85, l85a		'SlotMachine Lights
	Flashm 85, l85b		'SlotMachine Lights
	NFadelM 85, l85c		'SlotMachine Lights
	Flash 85, FlL85  '*
	NFadeLm 86, l86
	FadeDisableLighting 86, bulb4, 0.20
    	NFadelm 86, l86halo
	Flash 86, l86r

    'Flashers
	NFadeLmod 117, f17
	NFadeLMod 117, f17a
	NFadeLMod 117, f17b
	NFadeLMod 117, f17c
	NFadeFMod 117, f17d
	NFadeFMod 117, f17e
	NFadeFMod 117, f17f
	NFadeLmod 118, f18
	NFadeLmod 118, f18b
	NFadeLmod 118, f18c
	NFadeLmod 118, f18d
	NFadeFMod 118, f18e
	NFadeFMod 118, f18f
	NFadeFMod 118, f18g
	NFadeFMod 118, f18h
	NFadeLmod 119, f19
	NFadeLmod 119, f19a
	NFadeFmod 119, FlSol19	'*
	NFadeLmod 120, f20
	NFadeLmod 120, f20a
	NFadeLmod 120, f20b
	NFadeLmod 120, f20c
	NFadeFMod 120, f20e
	NFadeFMod 120, f20r
	NFadeLmod 128, f28
	NFadeLmod 128, f28a
	NFadeLmod 128, f28b
	NFadeFMod 128, f28c
	NFadeFMod 128, f28c1	
	NFadeFMod 128, f28r
	NFadeLmod 137, f37
	NFadeLmod 137, f37c
	NFadeLmod 138, f38
	NFadeLmod 138, f38a
	NFadeFMod 138, f38b
	NFadeFMod 138, f38c
	NFadeFMod 138, f38r
	NFadeLmod 139, f39
	NFadeLmod 139, f39a
	NFadeFMod 139, f39b
	NFadeFMod 139, f39c
	NFadeFMod 139, f39r
	NFadeLmod 140, f40
	NFadeLmod 140, f40a
	NFadeFMod 140, f40b
	NFadeFMod 140, f40c
	NFadeFMod 140, f40d
	NFadeFMod 140, f40r
	NFadeLmod 141, f41
	NFadeLmod 141, f41a
	NFadeLmod 141, f41b
	NFadeLmod 141, f41c
	NFadeFMod 141, f41e
	NFadeFMod 141, f41r
	NFadeFMod 141, f41r2

	If ClockStep = ClockFade then 
		ClockDir = 0 
	Else 
		If ClockStep < ClockFade then
			ClockDir = -6
		Else 
			ClockDir = 6
		End if
'		If abs(ClockStep-ClockFade)> 20 Then
'			ClockFade = round(ClockFade + (Clockstep-ClockFade)/4)
'		end if
	
		ClockFade = ClockFade + ClockDir
		dim xx
		For each xx in GIClock:xx.IntensityScale = (ClockFade-10)/36:next
	end if

	If gitime0 <> 0 and  gametime - gitime0 > gidelay Then
		UpdateGI 0, 1
		gitime0 = 0
	end if

	If gitime1 <> 0 and  gametime - gitime1 > gidelay Then
		UpdateGI 1, 1
		gitime1 = 0
	end if

	If gitime2 <> 0 and  (gametime - gitime2 > gidelay or gistep2 = 8) Then
		UpdateGI 2, 1
		gitime2 = 0
	end if

	If gitime3 <> 0 and  gametime - gitime3 > gidelay Then
		UpdateGI 3, 1
		gitime3 = 0
	end if

	If gitime4 <> 0 and  gametime - gitime4 > gidelay Then
		UpdateGI 4, 1
		gitime4 = 0
	end if

	If BIP > 0 AND SpiralMod = 1 Then SpiralMove.enabled = LampState(68)
End Sub


Sub SetModLamp(nr, level)
	'debug.print nr & ": " & level
	FadingLevel(nr) = level
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

Sub NFadeLMod(nr, object)	'3 lights
	object.IntensityScale = FadingLevel(nr) / 255
	object.state = 1
	object.visible = 1
End Sub

Sub NFadeLModm(nr, object) ' used for multiple lights
	NFadeLMod nr, Object
End Sub

Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

Sub FadeDisableLighting(nr, a, blend)
    Select Case FadingLevel(nr)
        Case 4:a.blendDisableLighting = 0
        Case 5:a.blendDisableLighting = blend
    End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1            'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

' Flasher objects

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub NFadeFMod(nr, object)	'Flashers
	object.IntensityScale = FadingLevel(nr) / 255
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
End Sub

'*********
'Update GI
'*********

Dim gistep, gitime0, gitime1, gitime2, gitime3, gitime4, gidelay
Dim gistep0, gistep1, gistep2, gistep3, gistep4
Dim ClockStep, ClockFade, ClockDir

ClockFade=10
gidelay = 250

Sub UpdateGI(no, step)
	Dim xx

	'only values from 1 to 8 are visible and reliable. 0 is not reliable and 7 & 8 are the same so...
	If step = 0 then
		Select Case no
			Case 0
				If gitime0 = 0 then gitime0 = gametime
			Case 1
				If gitime1 = 0 then gitime1 = gametime
			Case 2
				If gitime2 = 0 then gitime2 = gametime
			Case 3
				If gitime3 = 0 then gitime3 = gametime
			Case 4
				If gitime4 = 0 then gitime4 = gametime
		End Select		
		exit sub 
	End If

	gistep = (step-1) / 7
	Select Case no
		Case 0
			gitime0 = 0
			gistep0 = step
			For each xx in GILeft:xx.IntensityScale = gistep:next   'Playfield Left
		Case 1
			gitime1 = 0
			gistep1 = step
			For each xx in GIMinipf:xx.IntensityScale = gistep:next 'Mini Playfield + Insert
		Case 2
			gitime2 = 0
			gistep2 = step
			If step < 2 Then step = 1
			If step > 7 then step = 7
			ClockStep = step * 6 + 4 					
			'For each xx in GIClock:xx.IntensityScale = gistep:next  'Clock + Insert
		Case 3
			gitime3 = 0
			gistep3 = step
			For each xx in GImain:xx.IntensityScale = gistep:next   'Insert Main
		Case 4
			gitime4 = 0
			gistep4 = step
			For each xx in GIRight:xx.IntensityScale = gistep:next  'Playfield Right
	End Select

	' change the intensity of the flasher depending on the gi to compensate for the gi lights being off
	For xx = 0 to 200
		FlashMax(xx) = 6 - gistep * 3 ' the maximum value of the flashers
	Next
End Sub


'******************************************************
'						SOUNDS
'******************************************************

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

Sub PlaySoundAtLoop(soundname, tableobj)
    PlaySound soundname, -1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, aVol, tableobj)
    PlaySound soundname, 1, aVol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


Sub PlaySoundAtExisting(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolME (Soundname, aVol)
	Playsound soundname, 1,aVol, AudioPan(ActiveBall), 0,0,1,0, AudioFade(ActiveBall)
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

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallSpeed(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallSpeed(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / tablewidth-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function RndNum(min, max)
    RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 500, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'*********************************************************************
'                     	Collection Sounds
'*********************************************************************

Sub aApron_Hit(idx):PlaySoundAtBallVolME "fx_apron", Vol(ActiveBall)*VolumeDial/2:End Sub
Sub aRubbers_Hit(idx):PlaySoundAtBallVol "fx_rubber", Vol(ActiveBall)*VolumeDial/10:End Sub
Sub aPostRubbers_Hit(idx):PlaySoundAtBallVol "fx_postrubber", Vol(ActiveBall)*VolumeDial/10:End Sub
Sub aMetals_Hit(idx):PlaySoundAtBallVolME "fx_MetalHit", Vol(ActiveBall)*VolumeDial/20:End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBallVol "fx_PlasticHit", Vol(ActiveBall)*VolumeDial*5:End Sub
Sub aGates_Hit(idx):PlaySoundAtBallVol "fx_Gate", Vol(ActiveBall)*VolumeDial/10:End Sub
Sub aWoods_Hit(idx):PlaySoundAtBallVolME "fx_Woodhit", Vol(ActiveBall)*VolumeDial/2:End Sub

Sub LeftFlipper_Collide(parm)
    RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
    RandomSoundFlipper()
End Sub

Sub LeftFlipper1_Collide(parm)
    RandomSoundFlipper()
End Sub

Sub RightFlipper1_Collide(parm)
    RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySoundAtBallVol "fx_flip_hit_1", 2*Vol(ActiveBall)*VolumeDial
        Case 2:PlaySoundAtBallVol "fx_flip_hit_2", 2*Vol(ActiveBall)*VolumeDial
        Case 3:PlaySoundAtBallVol "fx_flip_hit_3", 2*Vol(ActiveBall)*VolumeDial
    End Select
End Sub

'**********************
' Balldrop & Ramp Sound
'**********************

Sub BallDropSound(dummy)
    PlaySound "fx_ball_drop"
End Sub

Sub Balldrop1_Hit()
    StopSound "fx_metalrolling"
End Sub

Sub Balldrop3_Hit()
    StopSound "fx_metalrolling"
End Sub




Sub WirerampSound1_Hit()
    PlaySoundat "fx_metalrolling", WirerampSound1
End Sub

Sub WirerampSound2_Hit()
    PlaySoundat "fx_metalrolling", WirerampSound2
End Sub

Sub WirerampSoundStop_Hit()
    StopSound "fx_metalrolling"
End Sub

Sub LREnter_Hit()
    If ActiveBall.VelY < 0 Then PlaySoundAtBallVol "fx_rlenter", Vol(Activeball)*VolumeDial
End Sub

Sub RREnter_Hit()
    If ActiveBall.VelY < 0 Then PlaySoundAtBallVol "fx_metal_ramp_hit", Vol(Activeball)
End Sub


'***********************************************************************
'* TABLE OPTIONS *******************************************************
'***********************************************************************

Dim TVPic, SlotPic, cGameName

TVPic = 0
SlotPic = 0

If CabinetSide = 0 Then
	LeftCab.image = "leftcab_plain"
	RightCab.image = "rightcab_plain"
	Backwall.image = "backpanel2"
End If
pfshadow.visible = RampShadow
Scoop1L.visible = ScoopLight
Scoop2L.visible = ScoopLight
Gumballs.visible = GumballMod
Primary_LockDownBar.visible = Lockdownbar
SlotMachineToy.visible = SlotMachineMod
swSlotReel.enabled = SlotMachineMod
SlotReel.visible = SlotMachineMod
SlotReelLight1.visible = SlotMachineMod
SlotReelLight2.visible = SlotMachineMod
l27a.visible = SlotMachineMod
l27b.visible = SlotMachineMod
l85a.visible = SlotMachineMod
l85b.visible = SlotMachineMod
Pyramid.visible = PyramidMod
PyramidCap.visible = PyramidMod
ClockToy.visible = MiniClockMod
MysticSeerToy.visible = MysticSeerMod
InvaderToy.visible = InvaderMod
l00.visible = InvaderMod
led3.visible = InvaderMod
led4.visible = InvaderMod
led5.visible = InvaderMod
l82a.visible = MiniClockMod
l82r.visible = MiniClockMod
TVToy.visible = TVMod
Frame.visible = TVMod
SpiralToy.visible = SpiralMod
URMagnetP.visible = ExtraMagnet
sw82.enabled = ExtraMagnet
sw82_help.enabled = ExtraMagnet
UpperRightMagnet.enabled = ExtraMagnet

If FlipperType = 2 then FlipperType = RndNum(0, 1)

If TargetMod = 1 Then
    sw47.image = "target-clock"
    sw48.image = "target-greed"
    sw64.image = "target-greed"
    sw65.image = "target-power"
    sw65a.image = "target-power"
    sw66.image = "target-greed"
    sw67.image = "target-greed"
    sw68.image = "target-coins"
    sw77.image = "target-greed"
    sw78.image = "target-greed"
End If

If StagedFlipperMod = 1 Then
	keyStagedFlipperL = KeyUpperLeft
	keyStagedFlipperR = KeyUpperRight
End If

If BWClockMod = 1 Then
	wall3.sideimage="clocktexturebw"
End If

If TownSquarePostMod = 1 Then
    Rubber12.visible = 0
    TownSquarePost.visible = 1
    TownSquarePostW.visible = 1
    TownSquarePostBulb.visible = 1
    l17a1.visible = 1
    l17a2.visible = 1
    l17r.visible = 1
Else
    l17a1.visible = 0
    l17a2.visible = 0
    l17r.visible = 0
End If

Select Case Romset
	Case 0:	cGameName = "tz_94ch"
	Case 1:	cGameName = "tz_94h"
End Select

Select Case FlipperType
    Case 0:
	FlipperL.visible = 1
        FlipperR.visible = 1
        FlipperL1.visible = 1
        FlipperR1.visible = 1
    Case 1:
	FlipperL.visible = 0
        FlipperR.visible = 0
        FlipperL1.visible = 0
        FlipperR1.visible = 0
	LeftFlipper.visible = 1
        RightFlipper.visible = 1
        LeftFlipper1.visible = 1
        RightFlipper1.visible = 1
        LogoL.visible = 1
        LogoR.visible = 1
        LogoL1.visible = 1
        LogoR1.visible = 1
End Select

If LampLightColor = 1 then
					light68.color = RGB (0,0,255):light68.colorfull = RGB (85,85,255)
					light19.color = RGB (0,0,255):light19.colorfull = RGB (85,85,255)
					light17.color = RGB (0,0,255):light17.colorfull = RGB (85,85,255)
					light44.color = RGB (0,0,255):light44.colorfull = RGB (85,85,255)
					light45.color = RGB (0,0,255):light45.colorfull = RGB (85,85,255)
					light46.color = RGB (0,0,255)
					light47.color = RGB (0,0,255)
					light48.color = RGB (0,0,255):light45.colorfull = RGB (85,85,255)
End If

Sub swSlotReel_Hit:SlotReelTimer.enabled = 1:End Sub

Sub SlotReelTimer_Timer()
    SlotPic = SlotPic + 1
    if SlotPic > 10 Then SlotReelTimer.enabled = 0:ResetSlot.enabled = 1
    SlotReel.imageA = "slot_" & SlotPic
End Sub

Sub ResetSlot_Timer()
    SlotReel.imageA = "slot_10":vpmtimer.addtimer 150, "SlotReel.imageA = ""slot_0"" '":SlotPic = 0
    If SlotPic = 0 then ResetSlot.enabled = 0
End Sub

Sub TVTimer_Timer()
    TVPic = TVPic + 1
    if TVPic = 34 Then TVPic = 2
    Frame.imageA = "tv_" & TVPic
End Sub

Sub SpiralMove_Timer()
    SpiralToy.rotz = SpiralToy.rotz + 10
End Sub

Select Case BumperPostsMod
		Case 0:		PegPlastic7.visible = 1
					PegPlastic8.visible = 1
					Rubber9.visible = 1
					Rubber29.visible = 1
					Rubber9.collidable = 1
					Rubber29.collidable = 1
		Case 1:		PegPlastic7.visible = 0
					PegPlastic8.visible = 0
					Rubber9.visible = 0
					Rubber29.visible = 0
					Rubber9.collidable = 0
					Rubber29.collidable = 0
End Select


'******************************************************
'				FLIPPER AND RUBBER CORRECTION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		'x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 0	'don't mess with these
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		'x.DebugOn = True : stickL.visible = True : tbpl.visible = True : vpmSolFlipsTEMP.DebugOn = True
		x.TimeDelay = 60
	Next

	'rf.report "Velocity"
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.2, 	1.07
	addpt "Velocity", 2, 0.41, 1.05
	addpt "Velocity", 3, 0.44, 1
	addpt "Velocity", 4, 0.65, 	1.0'0.982
	addpt "Velocity", 5, 0.702, 0.968
	addpt "Velocity", 6, 0.95,  0.968
	addpt "Velocity", 7, 1.03, 	0.945

	'rf.report "Polarity"
'	AddPt "Polarity", 0, 0, -4.7
'	AddPt "Polarity", 1, 0.16, -4.7	
'	AddPt "Polarity", 2, 0.33, -4.7
'	AddPt "Polarity", 3, 0.37, -4.7	'4.2
'	AddPt "Polarity", 4, 0.41, -4.7
'	AddPt "Polarity", 5, 0.45, -4.7 '4.2
'	AddPt "Polarity", 6, 0.576,-4.7
'	AddPt "Polarity", 7, 0.66, -2.8'-2.1896
'	AddPt "Polarity", 8, 0.743, -1.5
'	AddPt "Polarity", 9, 0.81, -1.5
'	AddPt "Polarity", 10, 0.88, 0

	AddPt "Polarity", 0, 0, -7.5
	AddPt "Polarity", 1, 0.3, -7.5	
	AddPt "Polarity", 2, 0.4, -10
	AddPt "Polarity", 3, 0.8, -10	
	AddPt "Polarity", 4, 0.85, -9
	AddPt "Polarity", 5, 0.9, -8 
	AddPt "Polarity", 6, 0.95, -7
	AddPt "Polarity", 7, 1, -6
	AddPt "Polarity", 8, 1.05, -5
	AddPt "Polarity", 9, 1.1, -4 
	AddPt "Polarity", 10, 1.15, -3
	AddPt "Polarity", 11, 1.2, -2
	AddPt "Polarity", 12, 1.25, -1
	AddPt "Polarity", 13, 1.3, 0



	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
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
	Public Property Let EndPoint(aInput) : if IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end if : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property
	
	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
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
				if DebugOn then StickL.visible = True : StickL.x = balldata(x).x		'debug TODO
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
		if abs(Flipper.currentAngle - Flipper.EndAngle) < 20 Then
			PartialFlipCoef = 0
		End If
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			if tmp < 0.1 then 'if real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
				if DebugOn then TestStr = "real pos < 0.1 ( " & round(tmp,2) & ")" : tbpl.text = Teststr 
				'RemoveBall aBall
				'Exit Sub
			end if

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				if DebugOn then teststr = "y velocity: " & round(aBall.vely, 3) & "exit sub" : tbpl.text = teststr
				RemoveBall aBall
				exit Sub
			end if
			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					'TB.TEXT = balldata(x).id & " " & BALLDATA(X).X & VBNEWLINE & FLIPPERSTART & " " & FLIPPEREND
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				if DebugOn then set tmp = new spoofball : tmp.data = aBall : End If
				if IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'if tip hit with no collected data, do vel correction anyway
					if PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						if Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						if Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
						if DebugOn then teststr = "tip protection" & vbnewline & "velcoef: " & round(velcoef,3) & vbnewline & round(PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1),3) & vbnewline
						'debug.print teststr
					end if
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					if Enabled then aBall.Velx = aBall.Velx*VelCoef
					if Enabled then aBall.Vely = aBall.Vely*VelCoef
				end if
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'debug.print BallPos & " " & AddX
				'playsound "fx_knocker"
			End If
			'debug
			if DebugOn then
				TestStr = teststr & "%pos:" & round(BallPos,2)
				if IsEmpty(PolarityOut(0) ) then 
					teststr = teststr & vbnewline & "(Polarity Disabled)" & vbnewline
				else 
					teststr = teststr & "+" & round(1 *(AddX*ycoef*PartialFlipcoef),3)
					if BallPos >= PolarityOut(uBound(PolarityOut) ) then teststr = teststr & "(MAX)" & vbnewline else teststr = teststr & vbnewline end if	
					if Ycoef < 1 then teststr = teststr &  "ycoef: " & ycoef & vbnewline
					if PartialFlipcoef < 1 then teststr = teststr & "PartialFlipcoef: " & round(PartialFlipcoef,4) & vbnewline				
				end if

				teststr = teststr & vbnewline & "Vel: " & round(BallSpeed(tmp),2) & " -> " & round(ballspeed(aBall),2) & vbnewline
				teststr = teststr & "%" & round(ballspeed(aBall) / BallSpeed(tmp),2)
				tbpl.text = TestSTR
			end if
		Else
			'if DebugOn then tbpl.text = "td" & timedelay
		End If
		RemoveBall aBall
	End Sub
End Class

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

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

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
RubbersD.addpoint 0, 0, 0.935 '0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.935 '0.96
RubbersD.addpoint 2, 5.76, 0.942 '0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

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
		'playsound "fx_knocker"
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
cor.debugOn = False
'cor.update() - put this on a low interval timer
Class CoRTracker
	public DebugOn 'tbpIn.text
	public ballvel

	Private Sub Class_Initialize : redim ballvel(0) : End Sub 
	'TODO this would be better if it didn't do the sorting every ms, but instead every time it's pulled for COR stuff
	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs
		'if uBound(allballs) < 0 then if DebugOn then str = "no balls" : TBPin.text = str : exit Sub else exit sub end if: end if
		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
'			if DebugOn then 
'				dim s, bs 'debug spacer, ballspeed
'				bs = round(BallSpeed(b),1)
'				if bs < 10 then s = " " else s = "" end if
'				str = str & b.id & ": " & s & bs & vbnewline 
'				'str = str & b.id & ": " & s & bs & "z:" & b.z & vbnewline 
'			end if
		Next
		'if DebugOn then str = "ubound ballvels: " & ubound(ballvel) & vbnewline & str : if TBPin.text <> str then TBPin.text = str : end if
	End Sub
End Class

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


'******************************************************
' 						FSS
'******************************************************

Dim xoff, yoff, zoff, xrot, xcen, ycen, zscale, topoff, BGArr
BGArr=Array (FlSol19,FlL16a,FlL16b,FlL24,FlL44a,FlL44b,FlL53a,FlL53b,FlL53c,FlL53d,FlL53f, FlL54,FlL61,FlL62,FlL63, FlL74a,FlL74b,FlL85)

Sub set_FSS()

	xoff = 555
	yoff = 0
	zoff = 880
	xrot = -90

	BGDark.x = xoff
	BGDark.y = yoff
	BGDark.height = zoff
	BGDark.rotx = xrot
	
	BGHigh.x = xoff
	BGHigh.y = yoff
	BGHigh.height = zoff
	BGHigh.rotx = xrot

	BGframe.x = xoff+5
	BGframe.y = yoff
	BGframe.height = zoff +5
	BGframe.rotx = xrot

	BGframeMask.x = xoff+5
	BGframeMask.y = yoff
	BGframeMask.height = zoff +5
	BGframeMask.rotx = xrot

	BGFrameMaskFill.x = xoff
	BGFrameMaskFill.y = yoff
	BGFrameMaskFill.height = zoff 
	BGFrameMaskFill.rotx = xrot

	BGHigh1.x = xoff
	BGHigh1.y = yoff
	BGHigh1.height = zoff + 100
	BGHigh1.rotx = xrot

	BGHigh2.x = xoff
	BGHigh2.y = yoff
	BGHigh2.height = zoff
	BGHigh2.rotx = xrot


	' the topper
	topoff = 615

	TopDark.x = xoff
	TopDark.y = yoff - 40
	TopDark.height = zoff +topoff
	TopDark.rotx = xrot

	TopHigh.x = xoff
	TopHigh.y = yoff - 40
	TopHigh.height = zoff +topoff
	TopHigh.rotx = xrot

	TopHigh1.x = xoff
	TopHigh1.y = yoff - 40
	TopHigh1.height = zoff +topoff
	TopHigh1.rotx = xrot


	TopHigh2.x = xoff
	TopHigh2.y = yoff - 40
	TopHigh2.height = zoff +(topoff-100)
	TopHigh2.rotx = xrot

	dmd.x = xoff + 10
	dmd.y = yoff - 30
	dmd.height = zoff -300
	dmd.rotx = xrot

	center_graphix()

End Sub

Sub center_graphix()
	zscale = 0.0000001 ' screen z scale found in backglass currently set to 1.15 (should by default be set to 1)
	xcen =(1167 /2) - (92 / 2)
	ycen = (1167 /2 ) + (290 /2)

	Dim xx
	Dim yy
	Dim yfact
	Dim xfact
	Dim obj
	yfact =10 'y fudge factor (ycen was wrong so fix)
	xfact =0

	For Each obj In BGArr
		xx =obj.x 
			
		obj.x = (xoff -xcen) + xx +xfact
		yy = obj.y ' get the yoffset before it is changed
		obj.y =yoff 

			If(yy < 0.) then
			yy = yy * -1
			end if

		
		obj.height =( zoff - ycen) + yy - (yy * zscale) + yfact
		
		obj.rotx = xrot
	Next
end sub

