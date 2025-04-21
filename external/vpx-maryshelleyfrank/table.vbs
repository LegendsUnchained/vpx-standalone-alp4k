'
' MM     MM    AAAAA    RRRRRRRR   YY      YY       SSSSSSS   HH     HH  EEEEEEEE   LL        LL        EEEEEEEE  YY      YY   ''   SSSSSSS 
' MMM   MMM   AA   AA   RR     RR   YY    YY       SS     SS  HH     HH  EE         LL        LL        EE         YY    YY    ''  SS     SS
' MMMM MMMM  AA     AA  RR     RR    YY  YY        SS         HH     HH  EE         LL        LL        EE          YY  YY    ''   SS
' MM MMM MM  AA     AA  RR     RR     YYYY         SS         HH     HH  EE         LL        LL        EE           YYYY          SS
' MM  M  MM  AAAAAAAAA  RRRRRRRR       YY           SSSSSSS   HHHHHHHHH  EEEEEEE    LL        LL        EEEEEEE       YY            SSSSSSS
' MM     MM  AA     AA  RR  RR         YY                 SS  HH     HH  EE         LL        LL        EE            YY                  SS
' MM     MM  AA     AA  RR   RR        YY                 SS  HH     HH  EE         LL        LL        EE            YY                  SS
' MM     MM  AA     AA  RR    RR       YY          SS     SS  HH     HH  EE         LL        LL        EE            YY           SS     SS
' MM     MM  AA     AA  RR     RR      YY           SSSSSSS   HH     HH  EEEEEEEEE  LLLLLLLL  LLLLLLLL  EEEEEEEEE     YY            SSSSSSS
'
'
' FFFFFFFFF  RRRRRRRR     AAAAA    NN     NN  KK     KK  EEEEEEEE  NN     NN   SSSSSSS   TTTTTTTT  EEEEEEEE  II  NN     NN
' FF         RR     RR   AA   AA   NNN    NN  KK    KK   EE        NNN    NN  SS     SS     TT     EE        II  NNN    NN
' FF         RR     RR  AA     AA  NNNN   NN  KK   KK    EE        NNNN   NN  SS            TT     EE        II  NNNN   NN
' FF         RR     RR  AA     AA  NN NN  NN  KK  KK     EE        NN NN  NN  SS            TT     EE        II  NN NN  NN
' FFFFF      RRRRRRRR   AAAAAAAAA  NN  NN NN  KK KK      EEEEEEE   NN  NN NN   SSSSSSS      TT     EEEEEEE   II  NN  NN NN
' FF         RR  RR     AA     AA  NN   NNNN  KKKK       EE        NN   NNNN         SS     TT     EE        II  NN   NNNN
' FF         RR   RR    AA     AA  NN    NNN  KK  KK     EE        NN    NNN         SS     TT     EE        II  NN    NNN
' FF         RR    RR   AA     AA  NN     NN  KK    KK   EE        NN     NN  SS     SS     TT     EE        II  NN     NN
' FF         RR     RR  AA     AA  NN     NN  KK     KK  EEEEEEEEE NN     NN   SSSSSSS      TT     EEEEEEEEE II  NN     NN     by Sega, 1995
'
'
' VPX version by Schreibi34 and Herweh
'
' Version 1.0: Initial release
' Version 1.0.1: Correct LUT initialization
'				 Added a wall for Frank
'				 Added a playfield wall left to the Geneva scoop
' Version 1.1:	 Some physical improvements to match the real table, i.e. at the Sarcophagus scoop, the kickback and the plastic ramp
'				 Less reflection on the transparent plastic over the bumper area
'				 Correct GI-Off initialisation at table startup
'
' Schreibi34:
' Visuals and gameplay
' "It's simply amazing what you do in Blender. So thrilling, so inspiring, so extraordinary. I'm overwhelmed every time when I see what you have created.
'  You are the man. And a very relaxed, cool guy too. And for sure an awesome drummer ;-). 
'  Once again it was a pleasure and an honor to work with you" (this was written by Herweh)
'
' Herweh:
' Scripting, VP development and gameplay
' "Everything you see below in this script! Without him Mr. Basic v2.0 Schreibi34 would have been totally lost! What he has done on this table goes beyond
'  my imagination. From animating Frank to implementing RothbauerW's physics to the texture swaps to all sorts of extras. Please check out all the stuff
'  that the script Voodoo Master has provided for you below!
'  Thanks for being such a cool guy! It was a pleasure!" (this was written by Schreibi34)
'
' --------------------------------------------------------------------------------------------------------------------------------------------------------
'
' Special thanks to:
'
' - Dark: We all know what he did! Frank is just jawdropping!!
' - Dids666: For starting all of this and his awesome meshes. Thanks for letting us finish this!!
' - Sheltemke: For stitching together the PF image and playtesting
' - Mlager8: For PS help on the sling plastics
' - RothbauerW: For his awesome physics' guide at vpinball.com and some more help on the physics
' - Mark70, Bord, Thalamus: For beta testing and some good hints
' - Batch: For the DT background image (Sorry for making it a bit darker! Batch's original is in the image manager for those who like more pop)
' - Schotty VH: For gameplay hints


Option Explicit
Randomize

Dim EnableGI, EnableFlasher, EnableReflectionsAtBall, EnableLUTChangeWithRightMagnaSave, SelectSidewalls, ShowBallShadow, LetTheBallJump, AnimateFranksHead, RailsVisible

' ****************************************************
' OPTIONS
' ****************************************************

' ENABLE/DISABLE GI (general illumination)
'	0 = GI is off
'	1 = GI is on
EnableGI = 1

' ENABLE/DISABLE flasher
'	0 = Flashers are off
'	1 = Flashers are on
EnableFlasher = 0

' ENABLE/DISABLE insert reflections at the ball
'	0 = reflections are off
'	1 = reflections are on (value can be between 0 and 1 and controls the reflections' intensity)
EnableReflectionsAtBall = 0

' ENABLED/DISABLE THE ABILITY TO CHANGE THE LUT FILE WITH THE RIGHT MAGNASAVE BUTTON
'	0 = disabled
'	1 = enabled (default)
EnableLUTChangeWithRightMagnaSave = 0

' SELECT YOUR PREFERRED SIDEWALLS
'	0 = autoselect (default)
'	1 = use sidewalls rendered for fullscreen view
'	2 = use sidewalls rendered for a medium view
'	3 = use sidewalls rendered for desktop view
'	4 = use sidewalls rendered for general illumination is off
SelectSidewalls = 3

' ANIMATE FRANK'S HEAD MOVING AROUND
'	0 = animation is off or motor is broken
'	1 = Frank's head is randonly moving around
'	2 = Frank's head is following the nearest ball
'	3 = Frank is watching the action (looking to switches or firing solenoids of the table)
'	4 = Frank's head is moving around like in some youtube videos
AnimateFranksHead = 4

' SHOW BALL SHADOWS
'	0 = no ball shadows
'	1 = ball shadows are visible
ShowBallShadow = 0

' LET THE BALL JUMP A BIT
'	0 = off
'	1 to 6 = ball jump intensity (3 = default)
LetTheBallJump = 0

' SIDE RAILS AND LOCKBAR VISIBILITY
'   0 = hide side rails
'   1 = show side rails
'   2 = side rails visible just in desktop mode
RailsVisible = 2

' volume for rolling sound and head motor sound
Const RollingSoundFactor = 0.15
Const HeadMotorSoundFactor = 0.6

' LUT names and inital LUT
Dim luts, lutpos
luts = Array("ColorGradeLUT256x16_1to1", "LUT_MSF_1", "LUT_MSF_2", "LUT_MSF_3", "LUT_MSF_4", "LUT_MSF_5", "LUT_MSF_6")
lutpos = 0


'***************************************************************************************************************************************************************
'***************************************************************************************************************************************************************


' ****************************************************
' standard definitions
' ****************************************************

Const UseSolenoids 	= 2
Const UseLamps 		= 0
Const UseSync 		= 0
Const HandleMech 	= 0
Const UseGI			= 1

'Standard Sounds
Const SSolenoidOn 	= "fx_Solenoid"
Const SSolenoidOff 	= ""
Const SFlipperOn 	= ""
Const SFlipperOff 	= ""
Const SCoin 		= "fx_coin"


'***************************************************************************************************************************************************************
'***************************************************************************************************************************************************************

Const cDMDRotation 	= -1 			'-1 for No change, 0 - DMD rotation of 0?, 1 - DMD rotation of 90?
Const cGameName 	= "frankst"		'ROM name
Const ballsize 		= 50
Const ballmass 		= 1

If Version < 10600 Then
	MsgBox "This table requires Visual Pinball 10.6 or newer!" & vbNewLine & "Your version: " & Replace(Version/1000,",","."), , "Laser War VPX"
End If

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package."
On Error Goto 0

LoadVPM "01550000", "DE.VBS", 3.26

Dim DesktopMode: DesktopMode = Frankenstein.ShowDT
Dim i, bsTrough

If LetTheBallJump > 6 Then LetTheBallJump = 6 : If LetTheBallJump < 0 Then LetTheBallJump = 0
If EnableReflectionsAtBall < 0 Or EnableReflectionsAtBall > 1 Then EnableReflectionsAtBall = 0

LUTBox.Visible = False


' ****************************************************
' table init
' ****************************************************
Sub Frankenstein_Init()
	vpmInit Me 
	With Controller
        .GameName 			= cGameName
        .SplashInfoLine 	= "Frankenstein (Sega 1995)"
		.HandleMechanics 	= False
		.HandleKeyboard 	= False
		.ShowDMDOnly 		= True
		.ShowFrame 			= False
		.ShowTitle 			= False
		.Hidden 			= False
		If cDMDRotation >= 0 Then .Games(cGameName).Settings.Value("rol") = cDMDRotation
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With
	' initialize some table settings
	InitTable
End Sub

Sub Frankenstein_Paused() 	: Controller.Pause = True : End Sub
Sub Frankenstein_UnPaused() : Controller.Pause = False : End Sub
Sub Frankenstein_Exit()		: Controller.Stop : End Sub

Sub InitTable()
	' tilt
	vpmNudge.TiltSwitch		= 1
	vpmNudge.Sensitivity	= 1
	vpmNudge.TiltObj		= Array(Bumper13, Bumper14, Bumper15, Bumper16, LeftSlingshot, RightSlingshot)
	
	' ball trough
	Set bsTrough = New cvpmBallStack
	With bsTrough
		.InitSw 0, 14, 13, 12, 11, 10, 9, 0
		.InitKick BallLockout, 80, 10
		.Balls = 6
	End With

	' init lights, flippers, bumpers, ...
	InitLights InsertLights

	' init timers
	PinMAMETimer.Interval 				= PinMAMEInterval
    PinMAMETimer.Enabled  				= True
	LampTimer.Enabled 					= True
	GraphicsTimer.Interval				= 10
	GraphicsTimer.Enabled				= True
	RollingSoundTimer.Interval			= 10
	RollingSoundTimer.Enabled			= True
	GITimer.Interval					= 15
	FrankHeadTimer.Interval				= 17
	FrankHeadTimer.Enabled				= (AnimateFranksHead = 1 Or AnimateFranksHead = 2 Or AnimateFranksHead = 3)

	pSiderailsLockbar.Visible 			= RailsVisible = 1 Or (RailsVisible = 2 And DesktopMode)
	pSiderailsLockbarGIOff.Visible 		= pSiderailsLockbar.Visible

	Frankenstein.ColorGradeImage 		= luts(lutpos)
End Sub


' ****************************************************
' keys
' ****************************************************
Sub Frankenstein_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then Controller.Switch(62) = True
	If keycode = LeftFlipperKey Then LFPress = 1 ' : Controller.Switch(63) = True
	If keycode = RightFlipperKey Then RFPress = 1 : RUFPress = 1 ' : Controller.Switch(64) = True
	If keycode = LeftTiltKey Then Nudge 90,2: Playsound SoundFX("fx_nudge",0)
	If keycode = RightTiltKey Then Nudge 270,2: Playsound SoundFX("fx_nudge",0)
	If keycode = CenterTiltKey Then Nudge 0,3: Playsound SoundFX("fx_nudge",0)
	If keycode = RightMagnaSave And EnableLUTChangeWithRightMagnaSave = 1 Then
		LUTBox.Visible = True
		lutpos = lutpos + 1 : If lutpos > UBound(luts) Then lutpos = 0 : End If
		Frankenstein.ColorGradeImage = luts(lutpos)
		Dim tekst : tekst = "LUT id: " & lutpos & " " & luts(lutpos)
		LUTBox.Text = tekst
		vpmTimer.AddTimer 3000, "If LUTBox.Text =" + chr(34) + tekst + chr(34) + " Then LUTBox.Visible = False'"
	End If
	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Frankenstein_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then Controller.Switch(62) = False
	If keycode = LeftFlipperKey Then 
		LFPress = 0 
		LeftFlipper.EOSTorqueAngle = EOSA 
		LeftFlipper.EOSTorque = EOST 
		' Controller.Switch(63) = False
	End If
	If keycode = RightFlipperKey Then 
		RFPress = 0
		RightFlipper.EOSTorqueAngle = EOSA 
		RightFlipper.EOSTorque = EOST

		RUFPress = 0 
		RightUpperFlipper.EOSTorqueAngle = EOSA 
		RightUpperFlipper.EOSTorque = EOST
		' Controller.Switch(64) = False
	End If
	If vpmKeyUp(keycode) Then Exit Sub
End Sub


' ****************************************************
' *** solenoids
' ****************************************************
' tools
SolCallback(1)         	= "SolLockout"
SolCallback(2) 			= "SolBallRelease"
SolCallback(3)			= "SolAutoLaunch"
SolCallback(4)			= "SolLowerScoopEject"
SolCallback(5)			= "SolTopScoopEject"
SolCallback(6)			= "SolIceCaveEject" 
SolCallback(7)			= "SolVUKEject"
SolCallback(8)			= "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(9)			= "SolTrapLeftHand"
SolCallback(11)			= "SolGI"
SolCallback(12)			= "SolTrapRightHand"
SolCallback(13)			= "SolFranksArms"
SolCallback(14)			= "SolOrbitDiverter"
SolCallback(16)			= "SolKickback"

' flasher
SolCallback(15)			= "SolFlasher 0, "	' around VUK
SolCallback(25)			= "SolFlasher 1, "	' above ANK
SolCallback(26)			= "SolFlasher 2, "	' above FR
SolCallback(27)			= "SolFlasher 3, "	' above IN
SolCallback(28)			= "SolFlasher 4, "	' at pop bumpers
SolCallback(29)			= "SolFlasher 5, "	' at spinner
SolCallback(30)			= "SolFlasher 6, "	' upper left corner
SolCallback(31)			= "SolFlasher 7, "	' around VUK

' flipper
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sLRFlipper) = "SolRFlipper"


' ******************************************************
' outhole, drain and ball release
' ******************************************************
Sub Drain_Hit()
	BallSearch
	PlaySoundAtVol "drain", Drain, 0.01
	FrankIsWatching ActiveBall
	bsTrough.AddBall Me
	If bsTrough.Balls = 6 Then StopFranksHeadMode4
End Sub

Sub SolLockout(Enabled)
	If Enabled Then
		PlaySoundAt SoundFX(SSolenoidOn, DOFContactors), BallLockout
		If bsTrough.Balls = 6 Then StartFranksHeadMode4
		If bsTrough.Balls > 0 Then bsTrough.SolOut True
	End If
End Sub
Sub BallRelease_Hit()   : Controller.Switch(15) = True  : End Sub
Sub BallRelease_Unhit() : Controller.Switch(15) = False : End Sub
Sub SolBallRelease(Enabled)
	If Enabled Then
		If Controller.Switch(15) Then
			PlaySoundAt SoundFX("fx_ballrel", DOFContactors), BallRelease
			BallRelease.Kick 85, 7
		End If
	End If
End Sub

' find balls that have fallen off the table
Sub BallSearch()
	Dim b
	For Each b In GetBalls
		If b.Y > 2200 Then 
			b.X = 155 : b.Y = 550 : b.VelX = 0 : b.VelY = 0
		End If
	Next 
End Sub


' ****************************************************
' flipper subs
' ****************************************************
Sub SolLFlipper(Enabled)
	If Enabled Then
		PlaySoundAtVol SoundFX("fx_flipperup",DOFContactors), LeftFlipper, 2
		LF.Fire 'LeftFlipper.RotateToEnd
    Else
		PlaySoundAt SoundFX("fx_flipperdown",DOFContactors), LeftFlipper
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		PlaySoundAtVol SoundFX("fx_flipperup",DOFContactors),RightFlipper, 2
		RF.Fire 'RightFlipper.RotateToEnd
		RightUpperFlipper.RotateToEnd
    Else 
		PlaySoundAt SoundFX("fx_flipperdown",DOFContactors),RightFlipper
		RightFlipper.RotateToStart
		RightUpperFlipper.RotateToStart
    End If
End Sub

' flipper hit sounds
Sub LeftFlipper_Collide(parm)
    PlaySoundAtBallVol "fx_flip_hit_" & Int(Rnd*3), 1
End Sub
Sub RightFlipper_Collide(parm)
    PlaySoundAtBallVol "fx_flip_hit_" & Int(Rnd*3), 1
End Sub
Sub RightUpperFlipper_Collide(parm)
    PlaySoundAtBallVol "fx_flip_hit_" & Int(Rnd*3), 1
End Sub


' ****************************************************
' sling shots and animations
' ****************************************************
Dim LeftStep, RightStep

Sub LeftSlingShot_Slingshot()
	vpmTimer.PulseSw 47
	PlaySoundAt SoundFX("fx_left_slingshot", DOFContactors), pLeftSlingHammer
	FrankIsWatching pLeftSlingHammer
    LeftStep = 0
	LeftSlingShot.TimerInterval = 5
	LeftSlingShot_Timer
End Sub
Sub LeftSlingShot_Timer()
    Select Case LeftStep
		Case 0: pRubberLeftSling1.Visible = False : pRubberLeftSling2.Visible = True : pLeftSlingHammer.TransZ = -12 : LeftSlingShot.TimerEnabled = True
        Case 1: pRubberLeftSling2.Visible = False : pRubberLeftSling3.Visible = True : pLeftSlingHammer.TransZ = -24 : LeftSlingShot.TimerInterval = 50
        Case 2: pRubberLeftSling3.Visible = False : pRubberLeftSling2.Visible = True : pLeftSlingHammer.TransZ = -12
        Case 3: pRubberLeftSling2.Visible = False : pRubberLeftSling1.Visible = True : pLeftSlingHammer.TransZ = 0 : LeftSlingShot.TimerEnabled = False
    End Select
    LeftStep = LeftStep + 1
End Sub

Sub RightSlingShot_Slingshot()
	vpmTimer.PulseSw 48
	PlaySoundAt SoundFX("fx_right_slingshot", DOFContactors), pRightSlingHammer
	FrankIsWatching pRightSlingHammer
    RightStep = 0
	RightSlingShot.TimerInterval = 5
	RightSlingShot_Timer
End Sub
Sub RightSlingShot_Timer()
    Select Case RightStep
		Case 0: pRubberRightSling1.Visible = False : pRubberRightSling2.Visible = True : pRightSlingHammer.TransZ = -12 : RightSlingShot.TimerEnabled = True
		Case 1: pRubberRightSling2.Visible = False : pRubberRightSling3.Visible = True : pRightSlingHammer.TransZ = -24 : RightSlingShot.TimerInterval = 50
        Case 2: pRubberRightSling3.Visible = False : pRubberRightSling2.Visible = True : pRightSlingHammer.TransZ = -12
        Case 3: pRubberRightSling2.Visible = False : pRubberRightSling1.Visible = True : pRightSlingHammer.TransZ = 0 : RightSlingShot.TimerEnabled = False
    End Select
    RightStep = RightStep + 1
End Sub


' ****************************************************
' bumpers
' ****************************************************
Sub Bumper13_Hit() : vpmTimer.PulseSw 41 : PlaySoundAt SoundFX("fx_bumper" & Int(Rnd*3),DOFContactors), Bumper13 : FrankIsWatching ActiveBall : End Sub
Sub Bumper14_Hit() : vpmTimer.PulseSw 42 : PlaySoundAt SoundFX("fx_bumper" & Int(Rnd*3),DOFContactors), Bumper14 : FrankIsWatching ActiveBall : End Sub
Sub Bumper15_Hit() : vpmTimer.PulseSw 43 : PlaySoundAt SoundFX("fx_bumper" & Int(Rnd*3),DOFContactors), Bumper15 : FrankIsWatching ActiveBall : End Sub
Sub Bumper16_Hit() : vpmTimer.PulseSw 44 : PlaySoundAt SoundFX("fx_bumper" & Int(Rnd*3),DOFContactors), Bumper16 : FrankIsWatching ActiveBall : End Sub


' ****************************************************
' switches
' ****************************************************
' orbit
Sub sw33_Hit()   : Controller.Switch(33) = True  : RollOverSound : FrankIsWatching ActiveBall : End Sub
Sub sw33_Unhit() : Controller.Switch(33) = False : End Sub
Sub sw34_Hit()   : Controller.Switch(34) = True  : RollOverSound : FrankIsWatching ActiveBall : End Sub
Sub sw34_Unhit() : Controller.Switch(34) = False : End Sub
Sub sw35_Hit()   : Controller.Switch(35) = True  : RollOverSound : FrankIsWatching ActiveBall : End Sub
Sub sw35_Unhit() : Controller.Switch(35) = False : End Sub
Sub sw36_Hit()   : Controller.Switch(36) = True  : RollOverSound : FrankIsWatching ActiveBall : End Sub
Sub sw36_Unhit() : Controller.Switch(36) = False : End Sub
Sub sw56_Hit()   : Controller.Switch(56) = True  : RollOverSound : FrankIsWatching ActiveBall : End Sub
Sub sw56_Unhit() : Controller.Switch(56) = False : End Sub

' kickback at left outlane
Sub sw37_Hit()   : Controller.Switch(37) = True  : RollOverSound : FrankIsWatching ActiveBall : End Sub
Sub sw37_Unhit() : Controller.Switch(37) = False : End Sub

' inlanes and outlanes
Sub sw38_Hit()   : Controller.Switch(38) = True  : RollOverSound : FrankIsWatching ActiveBall : End Sub
Sub sw38_Unhit() : Controller.Switch(38) = False : End Sub
Sub sw39_Hit()   : Controller.Switch(39) = True  : RollOverSound : FrankIsWatching ActiveBall : End Sub
Sub sw39_Unhit() : Controller.Switch(39) = False : End Sub
Sub sw40_Hit()   : Controller.Switch(40) = True  : RollOverSound : FrankIsWatching ActiveBall : End Sub
Sub sw40_Unhit() : Controller.Switch(40) = False : End Sub

' shooter lane
Sub sw16_Hit()   : Controller.Switch(16) = True  : RollOverSound : FrankIsWatching ActiveBall : End Sub   
Sub sw16_UnHit() : Controller.Switch(16) = False : End Sub

' ramp trigger
Sub sw45_Hit()   : Controller.Switch(45) = True  : FrankIsWatching ActiveBall : End Sub
Sub sw45_Unhit() : Controller.Switch(45) = False : End Sub
Sub sw46_Hit()   : Controller.Switch(46) = True  : FrankIsWatching ActiveBall : End Sub
Sub sw46_Unhit() : Controller.Switch(46) = False : End Sub

' spinner
Sub sw54_Spin()  : vpmTimer.PulseSw 54 : PlaySoundAt "fx_spinner", sw54 : End Sub


' ****************************************************
' stand-up targets
' ****************************************************
Dim sw17Dir, sw18Dir, sw19Dir, sw20Dir, sw21Dir, sw22Dir, sw23Dir, sw24Dir, sw25Dir, sw26Dir, sw27Dir, sw28Dir, sw29Dir
Sub sw17_Hit()   : MoveStandUpTarget 17, sw17, sw17P, sw17PGIOff, sw17Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw17_Timer() : MoveStandUpTarget 17, sw17, sw17P, sw17PGIOff, sw17Dir, False : End Sub
Sub sw18_Hit()   : MoveStandUpTarget 18, sw18, sw18P, sw18PGIOff, sw18Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw18_Timer() : MoveStandUpTarget 18, sw18, sw18P, sw18PGIOff, sw18Dir, False : End Sub
Sub sw19_Hit()   : MoveStandUpTarget 19, sw19, sw19P, sw19PGIOff, sw19Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw19_Timer() : MoveStandUpTarget 19, sw19, sw19P, sw19PGIOff, sw19Dir, False : End Sub
Sub sw20_Hit()   : MoveStandUpTarget 20, sw20, sw20P, sw20PGIOff, sw20Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw20_Timer() : MoveStandUpTarget 20, sw20, sw20P, sw20PGIOff, sw20Dir, False : End Sub
Sub sw21_Hit()   : MoveStandUpTarget 21, sw21, sw21P, sw21PGIOff, sw21Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw21_Timer() : MoveStandUpTarget 21, sw21, sw21P, sw21PGIOff, sw21Dir, False : End Sub
Sub sw22_Hit()   : MoveStandUpTarget 22, sw22, sw22P, sw22PGIOff, sw22Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw22_Timer() : MoveStandUpTarget 22, sw22, sw22P, sw22PGIOff, sw22Dir, False : End Sub
Sub sw23_Hit()   : MoveStandUpTarget 23, sw23, sw23P, sw23PGIOff, sw23Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw23_Timer() : MoveStandUpTarget 23, sw23, sw23P, sw23PGIOff, sw23Dir, False : End Sub
Sub sw24_Hit()   : MoveStandUpTarget 24, sw24, sw24P, sw24PGIOff, sw24Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw24_Timer() : MoveStandUpTarget 24, sw24, sw24P, sw24PGIOff, sw24Dir, False : End Sub
Sub sw25_Hit()   : MoveStandUpTarget 25, sw25, sw25P, sw25PGIOff, sw25Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw25_Timer() : MoveStandUpTarget 25, sw25, sw25P, sw25PGIOff, sw25Dir, False : End Sub
Sub sw26_Hit()   : MoveStandUpTarget 26, sw26, sw26P, sw26PGIOff, sw26Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw26_Timer() : MoveStandUpTarget 26, sw26, sw26P, sw26PGIOff, sw26Dir, False : End Sub
Sub sw27_Hit()   : MoveStandUpTarget 27, sw27, sw27P, sw27PGIOff, sw27Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw27_Timer() : MoveStandUpTarget 27, sw27, sw27P, sw27PGIOff, sw27Dir, False : End Sub
Sub sw28_Hit()   : MoveStandUpTarget 28, sw28, sw28P, sw28PGIOff, sw28Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw28_Timer() : MoveStandUpTarget 28, sw28, sw28P, sw28PGIOff, sw28Dir, False : End Sub
Sub sw29_Hit()   : MoveStandUpTarget 29, sw29, sw29P, sw29PGIOff, sw29Dir, True : FrankIsWatching ActiveBall : End Sub
Sub sw29_Timer() : MoveStandUpTarget 29, sw29, sw29P, sw29PGIOff, sw29Dir, False : End Sub

Sub MoveStandUpTarget(id, sw, prim, primGIOff, counter, init)
	If init And prim.RotX < 20 Then
		OnStandupTargetHit
		vpmTimer.PulseSw id
		sw.TimerInterval = 5
		sw.TimerEnabled  = True
		counter = 0
	End If
	prim.TransY = prim.TransY - counter / 4
	prim.RotX = prim.RotX + counter
	primGIOff.TransY = prim.TransY
	primGIOff.RotX = prim.RotX
	If prim.RotX >= 20 Then counter = 0
	If prim.RotX = 0 Then sw.TimerEnabled = False
End Sub


' ****************************************************
' rotating spinner
' ****************************************************
Sub SpinnerTimer_Timer()
	pSpinner54.RotX 		= 360 - sw54.CurrentAngle
	pSpinner54GIOff.RotX 	= pSpinner54.RotX
    pSpinner54Rod.TransZ 	= -Sin(sw54.CurrentAngle * 2 * 3.14 / 360) * 5
    pSpinner54Rod.TransX 	= Sin((sw54.CurrentAngle - 90) * 2 * 3.14 / 360) * -5
End Sub


' ****************************************************
' saucer
' ****************************************************
Dim sw53Step : sw53Step = 0
Dim sw55Step : sw55Step = 0
Dim sw31Step : sw31Step = 0
Dim sw32Step : sw32Step = 0
Dim sw53Ball : Set sw53Ball = Nothing
Dim sw55Ball : Set sw55Ball = Nothing
Dim sw31Ball : Set sw31Ball = Nothing
Dim sw32Ball : Set sw32Ball = Nothing

Sub sw53_Hit()   : PlaySoundAt "fx_saucer_enter", sw53 : End Sub
Sub sw53_Unhit() : Controller.Switch(53) = False : End Sub
Sub sw55_Hit()   : PlaySoundAt "fx_saucer_enter", sw55 : End Sub
Sub sw55_Unhit() : Controller.Switch(55) = False : End Sub
Sub sw31s_Hit()  : PlaySoundAt "fx_balldrop" & Int(Rnd*3), sw31s : End Sub
Sub sw31_Hit()   : PlaySoundAt "fx_loosemetalplate", sw31 : End Sub
Sub sw31_Unhit() : Controller.Switch(31) = False : sw31e.Enabled = True : End Sub
Sub sw31e_Hit()  : KickScoop ActiveBall, 107, 25 : sw31e.Enabled = False : End Sub
Sub sw32s_Hit()  : PlaySoundAt "fx_balldrop" & Int(Rnd*3), sw32s : End Sub
Sub sw32_Hit()   : PlaySoundAt "fx_loosemetalplate", sw32 : End Sub
Sub sw32_Unhit() : Controller.Switch(32) = False : sw32e.Enabled = True : End Sub
Sub sw32e_Hit()  : KickScoop ActiveBall, 169, 16 : sw32e.Enabled = False : End Sub

Sub SolIceCaveEject(Enabled)
	If Enabled Then
		MoveHammer pSaucer55Hammer, 0
		sw55Step 			= 0
		sw55.TimerInterval 	= 11
		sw55.TimerEnabled 	= True
	End If
End Sub
Sub SolVUKEject(Enabled)
	If Enabled Then
		sw53Step 			= 0
		sw53.TimerInterval 	= 11
		sw53.TimerEnabled 	= True
	End If
End Sub
Sub SolTopScoopEject(Enabled)
	If Enabled Then
		sw31Step 			= 0
		sw31.TimerInterval 	= 11
		sw31.TimerEnabled 	= True
	End If
End Sub
Sub SolLowerScoopEject(Enabled)
	If Enabled Then
		sw32Step 			= 0
		sw32.TimerInterval 	= 11
		sw32.TimerEnabled 	= True
	End If
End Sub

Sub sw55_Timer()
	' ice cave
	SaucerAction sw55Step, pSaucer55Hammer, sw55Ball, 55, sw55, 200, 10
	sw55Step = sw55Step + 1
End Sub
Sub sw53_Timer()
	' VUK
	SaucerAction sw53Step, Nothing, sw53Ball, 53, sw53, -1, 50+Rnd()*10
	sw53Step = sw53Step + 1
End Sub
Sub sw31_Timer()
	' top scoop
	SaucerAction sw31Step, Nothing, sw31Ball, 31, sw31, 290, 150+Rnd()*20
	sw31Step = sw31Step + 1
End Sub
Sub sw32_Timer()
	' bottom scoop
	SaucerAction sw32Step, Nothing, sw32Ball, 32, sw32, 345, 110+Rnd()*15
	sw32Step = sw32Step + 1
End Sub

Sub SaucerAction(step, pHammer, kBall, id, switch, kickangle, kickpower)
	Select Case step
		Case 0		: MoveHammer pHammer, -1 : PlaySoundAt SoundFX("fx_vuk_exit", DOFContactors), switch
		Case 1		: MoveHammer pHammer, 25 : If Controller.Switch(id) And Not (kBall Is Nothing) Then KickBall kBall, kickangle, IIF(kickpower>=100,1,10), kickpower, IIF(kickpower>=100,0,5), IIF(kickpower>=100,0,30)
		Case 13		: Controller.Switch(id) = False
		Case 23,24,25,26,27,28,29 : MoveHammer pHammer, -3
		Case 30 	: MoveHammer pHammer,  0 : switch.TimerEnabled = False
		Case Else 	: ' nothing to do
	End Select
End Sub

Sub KickBall(kBall, kAngle, kAngleVar, kVel, kVelZ, kLiftZ)
	If kBall Is Nothing Then Exit Sub
	Dim rAngle
	rAngle 	= 3.14159265 * (kAngle + (kAngleVar / 2 - kAngleVar * Rnd()) - 90) / 180
	kVel	= kVel + (kVel/20 - kVel/10 * Rnd())
	kVelZ	= kVelZ + (kVelZ/20 - kVelZ/10 * Rnd())
	With kBall
		.Z = .Z + kLiftZ
		If kAngle >= 0 Then
			.VelZ = kVelZ
			.VelX = cos(rAngle) * kVel
			.VelY = sin(rAngle) * kVel
		Else
			.VelZ = kVel
			If kAngle = -2 Then
				.VelX = Rnd() * 2
				.VelY = Rnd() * 2
			End If
		End If
	End With
	Set kBall = Nothing
End Sub

Sub MoveHammer(pHammer, rotZ)
	If Not (pHammer Is Nothing) Then 
		If rotZ = 0 Then
			pHammer.RotZ = 0
		Else
			pHammer.RotZ = pHammer.RotZ + rotZ
		End If
	End If
End Sub

Sub KickScoop(actBall, angle, power)
	With actBall
		.VelX = 0 : .VelY = 0 : .VelZ = 0
		KickBall actBall, angle, 5, power, 1, 20
	End With
End Sub


' ****************************************************
' upper ramp traps
' ****************************************************
rUpperRampTrap1.Collidable  = False
rUpperRampTrap2.Collidable  = False
rUpperRamp002.Collidable	= True

Sub SolTrapLeftHand(Enabled)
	If Enabled Then
		OpenTrapDoor trUpperRampTrap1
	End If
End Sub
Sub SolTrapRightHand(Enabled)
	If Enabled Then
		OpenTrapDoor trUpperRampTrap2
	End If
End Sub

Sub trUpperRampTrap1_Hit()
	Set ballFrankLeftHand		  = ActiveBall
	isBallOnWireRamp = False
	rUpperRampTrap1.Collidable    = True
	rUpperRamp002.Collidable	  = False
	PlaySoundAt "ferris_ball_drop", trUpperRampTrap1
End Sub
Sub kFrankLeftHand_Hit()
	rUpperRampTrap1.Collidable  = False
	rUpperRamp002.Collidable	= True
	PlaySoundAt "ferris_hit" & Int(Rnd()*2+1), kFrankLeftHand
'	vpmTimer.AddTimer 15000, "SolFranksArms True'"
End Sub

Sub trUpperRampTrap2_Hit()
	Set ballFrankRightHand		  = ActiveBall
	isBallOnWireRamp = False
	rUpperRampTrap2.Collidable    = True
	rUpperRamp002.Collidable	  = False
	PlaySoundAt "ferris_ball_drop", trUpperRampTrap2
End Sub
Sub kFrankRightHand_Hit()
	rUpperRampTrap2.Collidable  = False
	rUpperRamp002.Collidable	= True
	PlaySoundAt "ferris_hit" & Int(Rnd()*2+1), kFrankRightHand
'	vpmTimer.AddTimer 15000, "SolFranksArms True'"
End Sub

Sub OpenTrapDoor(sw)
	rUpperRamp002.Collidable = True
	sw.Enabled = True
	PlaySoundAt SoundFX(SSolenoidOn, DOFContactors), sw
	sw.TimerInterval = 9 : sw.TimerEnabled = True
End Sub
Sub CloseTrapDoor(sw)
	rUpperRamp002.Collidable = True
	sw.Enabled = False
	PlaySoundAt SoundFX(SSolenoidOn, DOFContactors), sw
	sw.TimerInterval = 11 : sw.TimerEnabled = True
End Sub

Sub trUpperRampTrap1_Timer()
	If trUpperRampTrap1.TimerInterval = 9 Then
		If pTrap1.ObjRotY <= -30 Then 
			trUpperRampTrap1.TimerEnabled = False : PlaySoundAt "fx_metalhit0", trUpperRampTrap1
		Else
			pTrap1.ObjRotY = pTrap1.ObjRotY - 10
		End If
	Else
		If pTrap1.ObjRotY >= 0 Then 
			trUpperRampTrap1.TimerEnabled = False : PlaySoundAt "fx_sensor", trUpperRampTrap1
		Else
			pTrap1.ObjRotY = pTrap1.ObjRotY + 2
		End If
	End If
End Sub
Sub trUpperRampTrap2_Timer()
	If trUpperRampTrap2.TimerInterval = 9 Then
		If pTrap2.ObjRotY <= -30 Then 
			trUpperRampTrap2.TimerEnabled = False : PlaySoundAt "fx_metalhit0", trUpperRampTrap2
		Else
			pTrap2.ObjRotY = pTrap2.ObjRotY - 10
		End If
	Else
		If pTrap2.ObjRotY >= 0 Then 
			trUpperRampTrap2.TimerEnabled = False : PlaySoundAt "fx_sensor", trUpperRampTrap2
		Else
			pTrap2.ObjRotY = pTrap2.ObjRotY + 2
		End If
	End If
End Sub


' ****************************************************
' Frank
' ****************************************************
Dim ballFrankLeftHand  : Set ballFrankLeftHand = Nothing
Dim ballFrankRightHand : Set ballFrankRightHand = Nothing

Sub SolFranksArms(Enabled)
	If Enabled Then
		MoveArms
		rUpperRamp002.Collidable 	= True
		rUpperRampTrap1.Collidable  = False
		rUpperRampTrap2.Collidable  = False
	End If
End Sub

Sub MoveArms()
	FrankArmsTimer.Interval = 3
	FrankArmsTimer.Enabled  = True
	PlaySoundAt SoundFX("ferris_exit", DOFContactors), pFrankHead
End Sub
Sub FrankArmsTimer_Timer()
	If FrankArmsTimer.Interval = 7 Then
		' move the arms back
		pFrankLeftArm.ObjRotX = pFrankLeftArm.ObjRotX + 0.5
		pFrankLeftArmGIOff.ObjRotX = pFrankLeftArm.ObjRotX
		pFrankLeftArmFOnGIOff.ObjRotX = pFrankLeftArm.ObjRotX
		pFrankRightArm.ObjRotX = pFrankRightArm.ObjRotX + 0.5
		pFrankRightArmGIOff.ObjRotX = pFrankRightArm.ObjRotX
		pFrankRightArmFOnGIOff.ObjRotX = pFrankRightArm.ObjRotX
		If pFrankLeftArm.ObjRotX >= 0 Then FrankArmsTimer.Enabled = False
	ElseIf FrankArmsTimer.Interval = 5 Then
		' throw the ball(s)
		ThrowBall ballFrankLeftHand, kFrankLeftHand, trUpperRampTrap1
		ThrowBall ballFrankRightHand, kFrankRightHand, trUpperRampTrap2
		FrankArmsTimer.Interval = 7
	Else
		' move the arms to the front
		pFrankLeftArm.ObjRotX = pFrankLeftArm.ObjRotX - 5
		pFrankLeftArmGIOff.ObjRotX = pFrankLeftArm.ObjRotX
		pFrankLeftArmFOnGIOff.ObjRotX = pFrankLeftArm.ObjRotX
		MoveBall ballFrankLeftHand
		pFrankRightArm.ObjRotX = pFrankRightArm.ObjRotX - 5
		pFrankRightArmGIOff.ObjRotX = pFrankRightArm.ObjRotX
		pFrankRightArmFOnGIOff.ObjRotX = pFrankRightArm.ObjRotX
		MoveBall ballFrankRightHand
		If pFrankLeftArm.ObjRotX <= -25 Then FrankArmsTimer.Interval = 5
	End If
End Sub

Sub MoveBall(ballInFranksHand)
	If Not (ballInFranksHand Is Nothing) Then
		With ballInFranksHand
			.Y = .Y + 8 : .Z = .Z + 9
		End With
	End If
End Sub
Sub ThrowBall(ballInFranksHand, ballKicker, trigger)
	ballKicker.Kick 180, 20
	CloseTrapDoor trigger
	If Not (ballInFranksHand Is Nothing) Then
		isBallOnWireRamp = False
		Set ballInFranksHand = Nothing
	End If
End Sub

Dim newAngleF, directionF, speedF, breakF, headStateF, headStateTimerF
newAngleF = 0
directionF = 0
Sub FrankHeadTimer_Timer()
	If AnimateFranksHead = 1 Then
		FrankHeadTimer.Interval = 55
		Const maxAngle1 = 45
		If directionF = 0 Or (directionF = -1 And newAngleF >= pFrankHead.ObjRotZ) Or (directionF = 1 And newAngleF <= pFrankHead.ObjRotZ) Then
			If breakF > 0 Then
				breakF = breakF - 1
			Else
				speedF = Rnd()*2.5 + 0.5
				breakF = Int(Rnd()*25)
				newAngleF = Int(Rnd()*(2*maxAngle1+1)) - maxAngle1
				If newAngleF < pFrankHead.ObjRotZ Then
					directionF = -1
				Else
					directionF = 1
				End If
			End If
			StopMotorSound
		Else
			' move Frank's head one step
			pFrankHead.ObjRotZ = pFrankHead.ObjRotZ + speedF * directionF
			StartMotorSound
		End If
	ElseIf AnimateFranksHead = 2 Then
		Const maxAngle2 = 45 ' 75
		Const rotStep = 3
		' identify the nearest ball
		Dim balls, b, distance, nearestBall, nearestDistance
		nearestBall 	= -1
		nearestDistance = 0
		balls 			= GetBalls
		For b = 0 to UBound(balls)
			distance = SQR((pFrankHead.X - balls(b).X) ^ 2 + (pFrankHead.Y - balls(b).Y) ^ 2)
			If nearestBall = -1 Or distance < nearestDistance Then
				nearestBall 	= b
				nearestDistance	= distance
			End If
		Next
		' follow that ball with Frank's head
		If nearestBall > -1 Then
			Dim angle, newAngle
			angle = GetFranksHeadAngle(balls(nearestBall))
			newAngle = pFrankHead.ObjRotZ
			If pFrankHead.ObjRotZ < angle Then
				newAngle = pFrankHead.ObjRotZ + rotStep
				If newAngle > angle Then newAngle = angle
				If newAngle > maxAngle2 Then newAngle = maxAngle2
			ElseIf pFrankHead.ObjRotZ > angle Then
				newAngle = pFrankHead.ObjRotZ - rotStep
				If newAngle < angle Then newAngle = angle
				If newAngle < -maxAngle2 Then newAngle = -maxAngle2
			End If
			pFrankHead.ObjRotZ = newAngle : StartMotorSound
		Else
			pFrankHead.ObjRotZ = 0 : StopMotorSound
		End If
	ElseIf AnimateFranksHead = 3 Then
		FrankHeadTimer.Interval = 25
		Const maxAngle3 = 45
		If Abs(newAngleF) > maxAngle3 Then
			If newAngleF < 0 Then newAngleF = -maxAngle3 Else newAngleF = maxAngle3
		End If
		If (directionF = -1 And newAngleF >= pFrankHead.ObjRotZ) Or (directionF = 1 And newAngleF <= pFrankHead.ObjRotZ) Then
			directionF = 0
			newAngleF = pFrankHead.ObjRotZ
		End If
		If directionF = 0 Then
			If newAngleF <> pFrankHead.ObjRotZ Then
				speedF = Rnd()*3.5 + 2.5
				directionF = 99 ' any value but not 1 or -1
			End If
			StopMotorSound
		Else
			If newAngleF < pFrankHead.ObjRotZ Then
				directionF = -1
			Else
				directionF = 1
			End If
			' move Frank's head one step
			pFrankHead.ObjRotZ = pFrankHead.ObjRotZ + speedF * directionF
			StartMotorSound
		End If
	ElseIf AnimateFranksHead = 4 Then
		FrankHeadTimer.Interval = 11
		speedF = 1
		Select Case headStateF
		Case -1
			If pFrankHead.ObjRotZ > 0 Then pFrankHead.ObjRotZ = pFrankHead.ObjRotZ - 1 : StartMotorSound
			If pFrankHead.ObjRotZ < 0 Then pFrankHead.ObjRotZ = pFrankHead.ObjRotZ + 1 : StartMotorSound
			If pFrankHead.ObjRotZ = 0 Then FrankHeadTimer.Enabled = False : StopMotorSound
		Case 1
			If pFrankHead.ObjRotz > -17 Then 
				pFrankHead.ObjRotz = pFrankHead.ObjRotz - speedF : StartMotorSound 
			Else 
				StopMotorSound
			End If
			headStateTimerF = headStateTimerF + 1
			If headStateTimerF >= 200 Then headStateF = 2
		Case 2
			If pFrankHead.ObjRotz > -35 Then 
				pFrankHead.ObjRotz = pFrankHead.ObjRotz - speedF : StartMotorSound
			Else
				StopMotorSound
			End If
			If pFrankHead.ObjRotz = -35 Then headStateF = 3
		Case 3
			If pFrankHead.ObjRotz < 0 Then 
				pFrankHead.ObjRotz = pFrankHead.ObjRotz + speedF : StartMotorSound
			Else 
				StopMotorSound
			End If
			headStateTimerF = headStateTimerF + 1
			If headStateTimerF >= 300 Then headStateF = 4
		Case 4
			If pFrankHead.ObjRotz > -35 Then 
				pFrankHead.ObjRotz = pFrankHead.ObjRotz - speedF : StartMotorSound
			Else 
				StopMotorSound
			End If
			headStateTimerF = headStateTimerF + 1
			If headStateTimerF >= 700 Then headStateF = 5
		Case 5
			If pFrankHead.ObjRotz < 0 Then 
				pFrankHead.ObjRotz = pFrankHead.ObjRotz + speedF : StartMotorSound
			Else 
				StopMotorSound
			End If
			headStateTimerF = headStateTimerF + 1
			If headStateTimerF >= 800 Then headStateF = 6
		Case 6
			If pFrankHead.ObjRotz < 17 Then 
				pFrankHead.ObjRotz = pFrankHead.ObjRotz + speedF : StartMotorSound
			Else 
				StopMotorSound
			End If
			headStateTimerF = headStateTimerF + 1
			If headStateTimerF >= 850 Then headStateF = 7
		Case 7
			If pFrankHead.ObjRotz < 35 Then 
				pFrankHead.ObjRotz = pFrankHead.ObjRotz + speedF : StartMotorSound
			Else 
				StopMotorSound
			End If
			headStateTimerF = headStateTimerF + 1
			If headStateTimerF >= 900 Then headStateF = 8
		Case 8
			If pFrankHead.ObjRotz > 0 Then 
				pFrankHead.ObjRotz = pFrankHead.ObjRotz - speedF : StartMotorSound
			Else 
				StopMotorSound
			End If
			headStateTimerF = headStateTimerF + 1
			If headStateTimerF >= 1100 Then headStateF = 1 : headStateTimerF = 1
		End Select
	End If
	pFrankHeadGIOff.ObjRotZ = pFrankHead.ObjRotZ
	pFrankHeadFOnGIOff.ObjRotZ = pFrankHead.ObjRotZ
End Sub

Sub FrankIsWatching(aObject)
	newAngleF = GetFranksHeadAngle(aObject)
End Sub

Function GetFranksHeadAngle(aObject)
	If aObject Is Nothing Then Exit Function
	Dim aa, bb, cc, xx
	aa = pFrankHead.X - aObject.X
	bb = pFrankHead.Y - aObject.Y
	cc = SQR(aa^2 + bb^2)
	xx = aa / cc
	GetFranksHeadAngle = Atn(xx / Sqr(-xx * xx + 1)) * 180 / CSng(4*Atn(1))
End Function

Sub StartFranksHeadMode4()
	If AnimateFranksHead <> 4 Then Exit Sub
	headStateF = 1 : headStateTimerF = 1
	FrankHeadTimer.Enabled = True
End Sub
Sub StopFranksHeadMode4()
	If AnimateFranksHead <> 4 Then Exit Sub
	headStateF = -1 : headStateTimerF = 1
End Sub

Dim isMotorSOundOn : isMotorSoundOn = False
Sub StartMotorSound()
	If isMotorSoundOn Then Exit Sub
	isMotorSoundOn = True
	PlayLoopSoundAtVol "fx_motor", pFrankBody, HeadMotorSoundFactor
End Sub
Sub StopMotorSound()
	isMotorSoundOn = False
	StopSound "fx_motor"
End Sub


' ****************************************************
' diverter
' ****************************************************
rDiverter.Collidable = False

Sub SolOrbitDiverter(Enabled)
	PlaySoundAt "fx_sensor", sw35
	FrankIsWatching sw35
	rDiverter.Collidable = Enabled
End Sub


' ****************************************************
' auto launch
' ****************************************************
AutoPlunger.PullBack

Sub SolAutoLaunch(Enabled)
	vpmSolAutoPlungeS AutoPlunger, SoundFX(SSolenoidOn, DOFContactors), 5, Enabled
End Sub


' ****************************************************
' kick back
' ****************************************************
Sub SolKickback(Enabled)
   	Kickback.Enabled = Enabled
End Sub 

Dim kickbackBallVel : kickbackBallVel = 1
Dim kickbackBall    : Set kickbackBall = Nothing
Sub Kickback_Hit()
	Kickback.TimerEnabled  = False
	kickbackBallVel  = BallVel(ActiveBall)
	Set kickbackBall = ActiveBall
	PlaySoundAt SoundFX(SSolenoidOn, DOFContactors), Kickback
	Kickback.TimerInterval = 40
   	Kickback.TimerEnabled  = True
End Sub
Sub Kickback_Timer()
	If Kickback.TimerInterval = 40 Then
		Kickback.Kick 0, Int(60 + kickbackBallVel/2 + Rnd()*10)
		kickbackBall.VelX = 0
		PlaySoundAt SoundFX("plunger", DOFContactors), Kickback
		pKickback.TransY = 0
		Kickback.TimerInterval = 20
	ElseIf Kickback.TimerInterval = 20 Then
		pKickback.TransY = pKickback.TransY + 10
		If pKickback.TransY = 50 Then Kickback.TimerInterval = 500
	ElseIf Kickback.TimerInterval = 500 Then
		Kickback.TimerInterval = 50
	ElseIf Kickback.TimerInterval = 50 Then
		pKickback.TransY = pKickback.TransY - 5
		If pKickback.TransY = 0 Then Kickback.TimerInterval = 1000
	ElseIf Kickback.TimerInterval = 1000 Then
		Kickback.TimerEnabled = False
	End If
End Sub


' *********************************************************************
' lamps and illumination
' *********************************************************************
' inserts
Dim PFLights(200,3), PFLightsCount(200), isGIOn, isGIChanged
isGIOn = False
isGIChanged = False

Sub InitLights(aColl)
	' init inserts
	Dim obj, idx
	For Each obj In aColl
		idx = obj.TimerInterval
		Set PFLights(idx, PFLightsCount(idx)) = obj
		PFLightsCount(idx) = PFLightsCount(idx) + 1
		If Right(obj.Name,1) = "a" Then obj.IntensityScale = EnableReflectionsAtBall
	Next
	InitGI
End Sub
Sub LampTimer_Timer()
	Dim chgLamp, num, chg, ii, nr, xxx, obj
	xxx = -1
    chgLamp = Controller.ChangedLamps
	If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
			Select Case chglamp(ii,0)
			' bumper lights
			Case 13,14,15,16
				If chgLamp(ii,1) = 1 Then aBumperValue(chglamp(ii,0)-13) = -99 Else aBumperValue(chglamp(ii,0)-13) = 3
			' tiny plastic lamps
			Case 29
				fPlasticBulb29.Visible 	= (chglamp(ii,1) = 1)
				lPlasticBulb29.State 	= IIF(chglamp(ii,1) = 1, LightStateOn, LightStateOff)
			Case 30
				fPlasticBulb30.Visible 	= (chglamp(ii,1) = 1)
				lPlasticBulb30.State 	= IIF(fPlasticBulb30.Visible Or fPlasticBulb31.Visible Or fPlasticBulb32.Visible, LightStateOn, LightStateOff)
			Case 31
				fPlasticBulb31.Visible 	= (chglamp(ii,1) = 1)
				lPlasticBulb30.State 	= IIF(fPlasticBulb30.Visible Or fPlasticBulb31.Visible Or fPlasticBulb32.Visible, LightStateOn, LightStateOff)
			Case 32
				fPlasticBulb32.Visible 	= (chglamp(ii,1) = 1)
				lPlasticBulb30.State 	= IIF(fPlasticBulb30.Visible Or fPlasticBulb31.Visible Or fPlasticBulb32.Visible, LightStateOn, LightStateOff)
			Case 48
				fPlasticBulb48.Visible 	= (chglamp(ii,1) = 1)
				lPlasticBulb48.State 	= IIF(chglamp(ii,1) = 1, LightStateOn, LightStateOff)
			Case 56
				fPlasticBulb56.Visible 	= (chglamp(ii,1) = 1)
				lPlasticBulb56.State 	= IIF(chglamp(ii,1) = 1, LightStateOn, LightStateOff)
			Case Else
				For nr = 1 to PFLightsCount(chgLamp(ii,0)) : PFLights(chgLamp(ii,0),nr - 1).State = chgLamp(ii,1) : Next
			End Select
        Next
	End If
End Sub

' GI
Dim GILevel : GILevel = 0
Sub SolGI(IsOff)
	SetGI Not IsOff
End Sub

Sub InitGI()
	Dim obj
	pFlasher2Aa.Visible = False
	pFlasher6Aa.Visible = False
	pFlasher7Aa.Visible = False
	pFlasher15Aa.Visible = False
	For Each obj In GIElements : obj.Visible = False : Next
	UseGIMaterial GIWireTrigger, "Metal0.8", 0
		UseGIMaterial sGates, "Metal Chrome S34", 0
		UseGIMaterial Array(kIceCave,kVUK,pSpinner54Rod), "Metal Chrome S34", 0
End Sub
Sub SetGI(IsOn)
	If EnableGI = 0 And Not isGIOn Then Exit Sub
	If isGIOn <> IsOn Then
		isGIOn = IsOn
		isGIChanged = True
		If isGIOn Then
			' GI goes on
			PlaySoundAtVol "fx_relay_on", pFrankHead, 1
			GILevel = 1 ' IIF(FastMode,4,1)
			DOF 101, DOFOn
		Else
			' GI goes off
			PlaySoundAtVol "fx_relay_off", pFrankHead, 1
			GILevel = -4
			DOF 101, DOFOff
		End If
		InvalidateFrank
	End If
End Sub

Sub SolFlasher(id, enabled)
	If enabled Then aFlasherValue(id) = -99 Else aFlasherValue(id) = 3
End Sub

Dim aBumper 		: aBumper 		= Array(pBumperCap13Off, pBumperCap14Off, pBumperCap15Off, pBumperCap16Off)
Dim aBumperOn 		: aBumperOn 	= Array(pBumperCap13, pBumperCap14, pBumperCap15, pBumperCap16)
Dim aBumperLight	: aBumperLight	= Array(lBumper13, lBumper14, lBumper15, lBumper16)
Dim aBumperValue 	: aBumperValue	= Array(-99, -99, -99, -99)
Dim aFlasher		: aFlasher		= Array(pFlasher15AaOff, Nothing, pFlasher2AaOff, Nothing, Nothing, Nothing, pFlasher6AaOff, pFlasher7AaOff)
Dim aFlasherOn		: aFlasherOn	= Array(pFlasher15Aa, Nothing, pFlasher2Aa, Nothing, Nothing, Nothing, pFlasher6Aa, pFlasher7Aa)
Dim aFlasherLight 	: aFlasherLight = Array(Array(lFlasher15Ab,lFlasher15Ac,lFlasher15Ad,lFlasher15Ae,lFlasher15Af,lFlasher15Ag,lFlasher15Az,lFlasher15Ba,lFlasher15Ca,lFlasher15Da,lFlasher15Db), _
											Array(lFlasher1Aa), _
											Array(lFlasher2Ab,lFlasher2Ac,lFlasher2Ad,lFlasher2Ae,lFlasher2Af,lFlasher2Ag,lFlasher2Ah,lFlasher2Az,lFlasher2Ba), _
											Array(lFlasher3Aa), _
											Array(lFlasher4Aa,lFlasher4Ba,lFlasher4Bb,lFlasher4Ca,lFlasher4Cb,lFlasher4Da,lFlasher4Db), _
											Array(lFlasher5Aa,lFlasher5Ba,lFlasher5Bb), _
											Array(lFlasher6Ab,lFlasher6Ac,lFlasher6Ad,lFlasher6Ae,lFlasher6Az,lFlasher6Ba,lFlasher6Ca), _
											Array(lFlasher7Ab,lFlasher7Ac,lFlasher7Ad,lFlasher7Ae,lFlasher7Af,lFlasher7Ag,lFlasher7Az,lFlasher7Ba,lFlasher7Ca))
Dim aFlasherValue 	: aFlasherValue	= Array(0, 0, 0, 0, 0, 0, 0, 0)
Sub LightsTimer_Timer()
	Dim idx, obj, coll
	' set bumper and flasher off images
	If isGIChanged Then
		For idx = 0 To UBound(aBumper)
			If Not aBumper(idx) Is Nothing Then
				aBumper(idx).Image = IIF(isGIOn, "BumperCap Light-Off GI-On", "BumperCap Light-Off GI-Off")
			End If
		Next
		For idx = 0 To UBound(aFlasher)
			If Not aFlasher(idx) Is Nothing Then
				aFlasher(idx).Image = IIF(isGIOn, "Flasher Light-Off GI-On", "Flasher Light-Off GI-Off")
			End If
		Next
		For Each obj In Array(pRubberLeftSling1,pRubberLeftSling2,pRubberLeftSling3,pRubberRightSling1,pRubberRightSling2,pRubberRightSling3)
			obj.Image = IIF(isGIOn, "SP-Wire-GuidesTexComb", "SP-Wire-GuidesTexComb GI-Off")
		Next
		isGIChanged = False
	End If
	' bumper lights
	For idx = 0 To UBound(aBumper)
		If aBumperValue(idx) > -1 Or aBumperValue(idx) = -99 Then
			If aBumperValue(idx) = -99 Then
				aBumperOn(idx).Visible	= True
				aBumper(idx).Material 	= "Prims platt4"
				aBumper(idx).Visible 	= False
				If IsArray(aBumperLight(idx)) Then
					For Each obj In aBumperLight(idx) : obj.State = LightStateOn : obj.IntensityScale = 1 : Next
				Else
					If Not aBumperLight(idx) Is Nothing Then aBumperLight(idx).State = LightStateOn : aBumperLight(idx).IntensityScale = 1 : End If
				End If
			ElseIf aBumperValue(idx) = 0 Then
				aBumper(idx).Visible 	= True
				aBumper(idx).Material 	= "Prims platt"
				aBumperOn(idx).Visible	= False
				If IsArray(aBumperLight(idx)) Then
					For Each obj In aBumperLight(idx) : obj.State = LightStateOn : obj.IntensityScale = 0 : Next
				Else
					If Not aBumperLight(idx) Is Nothing Then aBumperLight(idx).State = LightStateOff : aBumperLight(idx).IntensityScale = 0 : End If
				End If
			ElseIf aBumperValue(idx) <= 8 Then
				aBumperOn(idx).Visible	= True
				aBumper(idx).Visible 	= True
				aBumper(idx).Material 	= "Prims platt" & aBumperValue(idx)
				If IsArray(aBumperLight(idx)) Then
					For Each obj In aBumperLight(idx) : obj.IntensityScale = aBumperValue(idx) / 4 : Next
				Else
					If Not aBumperLight(idx) Is Nothing Then aBumperLight(idx).IntensityScale = aBumperValue(idx) / 4 : End If
				End If
			End If
			aBumperValue(idx) = aBumperValue(idx) - 1
		End If
	Next
	' flasher
	For idx = 0 To UBound(aFlasher)
		If aFlasherValue(idx) > -1 Or aFlasherValue(idx) = -99 Then
			If aFlasherValue(idx) = -99 Then
				' flasher is on
				If Not aFlasher(idx) Is Nothing Then
					aFlasherOn(idx).Visible	= True
					aFlasher(idx).Material 	= "Prims platt4"
					aFlasher(idx).Visible 	= False
					IlluminateFrank idx, aFlasherValue(idx)
				End If
				If IsArray(aFlasherLight(idx)) Then
					For Each obj In aFlasherLight(idx) : obj.State = LightStateOn : obj.IntensityScale = 1 : Next
				Else
					If Not aFlasherLight(idx) Is Nothing Then aFlasherLight(idx).State = LightStateOn : aFlasherLight(idx).IntensityScale = 1
				End If
			ElseIf aFlasherValue(idx) = 0 Then
				' flasher is off
				If Not aFlasher(idx) Is Nothing Then 
					aFlasher(idx).Visible 	= True
					aFlasher(idx).Material 	= "Prims platt"
					aFlasherOn(idx).Visible	= False
					IlluminateFrank idx, aFlasherValue(idx)
				End If
				If IsArray(aFlasherLight(idx)) Then
					For Each obj In aFlasherLight(idx) : obj.State = LightStateOff : obj.IntensityScale = 0 : Next
				Else
					If Not aFlasherLight(idx) Is Nothing Then aFlasherLight(idx).State = LightStateOff : aFlasherLight(idx).IntensityScale = 0
				End If
			ElseIf aFlasherValue(idx) <= 4 Then
				' flasher goes off
				If Not aFlasher(idx) Is Nothing Then 
					aFlasher(idx).Visible 	= True
					aFlasher(idx).Material 	= "Prims platt" & aFlasherValue(idx)
					aFlasherOn(idx).Visible	= False
					IlluminateFrank idx, aFlasherValue(idx)
				End If
				If IsArray(aFlasherLight(idx)) Then
					For Each obj In aFlasherLight(idx) : obj.IntensityScale = aFlasherValue(idx) / 4 : Next
				Else
					If Not aFlasherLight(idx) Is Nothing Then aFlasherLight(idx).IntensityScale = aFlasherValue(idx) / 4
				End If
			End If
			aFlasherValue(idx) = aFlasherValue(idx) - 1
		End If
	Next
'	' GI
	If GILevel > 0 And GILevel < 5 Then
		' GI goes on, from 1 to 4
		For Each obj In GIBulbs
			obj.IntensityScale = GILevel / 4
			If GILevel > 0 Then obj.State = LightStateOn
		Next
		If GILevel = 4 Then
			For Each obj In GIFlasher : obj.Visible = True : Next
		End If
		' playfield and playfield overlay
		wPlayfieldGIOff.TopMaterial 	= "Playfield" & GILevel
		wPlayfieldGIOff.Visible 		= (wPlayfieldGIOff.TopMaterial <> "Playfield4")
		If DesktopMode Then
			wTextlayerGIOff.Visible		= False
			wTextlayer.Image 			= "Playfield Textlayer"
		Else
			wTextlayerGIOff.TopMaterial = "PlayfieldText" & GILevel
			wTextlayerGIOff.Visible		= (wTextlayerGIOff.TopMaterial <> "PlayfieldText4")
			wTextlayer.TopMaterial 		= "PlayfieldText" & IIF((4-GILevel)=0,"",4-GILevel)
			wTextlayer.Visible			= (wTextlayer.TopMaterial <> "PlayfieldText4")
		End If
		' Frank
		pFrankHeadGIOff.Material 		= "Prims platt" & GILevel
		pFrankBodyGIOff.Material 		= "Prims platt" & GILevel
		pFrankLeftArmGIOff.Material 	= "Prims platt" & GILevel
		pFrankRightArmGIOff.Material 	= "Prims platt" & GILevel
		pFrankHeadGIOff.Visible 		= (pFrankHeadGIOff.Material <> "Prims platt4")
		pFrankBodyGIOff.Visible 		= pFrankHeadGIOff.Visible
		pFrankLeftArmGIOff.Visible 		= pFrankHeadGIOff.Visible
		pFrankRightArmGIOff.Visible 	= pFrankHeadGIOff.Visible
		' playfield elements
		For Each obj In GIElementsGIOff
			obj.Material 	= "Prims platt" & GILevel
			obj.Visible 	= pFrankHeadGIOff.Visible
		Next
		pSiderailsLockbarGIOff.Material = "Prims platt" & GILevel
		pSiderailsLockbarGIOff.Visible  = (pFrankHeadGIOff.Visible And (RailsVisible = 1 Or (RailsVisible = 2 And DesktopMode)))
		If GILevel = 1 Then
			For Each obj In GIElements : obj.Visible = True : Next
			SetSidewallsVisibility True
			pSiderailsLockbar.Visible = (RailsVisible = 1 Or (RailsVisible = 2 And DesktopMode))
		End If
		UseGIMaterial GIWireTrigger, "Metal0.8", GILevel
		UseGIMaterial sGates, "Metal Chrome S34", GILevel
		UseGIMaterial Array(kIceCave,kVUK,pSpinner54Rod), "Metal Chrome S34", GILevel
		If GILevel = 4 Then AdjustLights
		GILevel = GILevel + 1
	ElseIf GILevel < 0 And GILevel > -5 Then
		' GI goes off, from -4 to -1
		For Each obj In GIBulbs
			obj.IntensityScale = (Abs(GILevel)-1) / 4
			If GILevel = -1 Then obj.State = LightStateOff
		Next
		If GILevel = -1 Then
			For Each obj In GIFlasher : obj.Visible = False : Next
		End If
		' playfield and playfield overlay
		wPlayfieldGIOff.TopMaterial 	= "Playfield" & IIF(GILevel=-1,"",Abs(GILevel+1))
		wPlayfieldGIOff.Visible 		= (wPlayfieldGIOff.TopMaterial <> "Playfield4")
		If DesktopMode Then
			wTextlayerGIOff.Visible		= False
			wTextlayer.Image 			= "Playfield Textlayer GI-Off"
		Else
			wTextlayerGIOff.TopMaterial = "PlayfieldText" & IIF(GILevel=-1,"",Abs(GILevel+1))
			wTextlayerGIOff.Visible		= (wTextlayerGIOff.TopMaterial <> "PlayfieldText4")
			wTextlayer.TopMaterial 		= "PlayfieldText" & (5+GILevel)
			wTextlayer.Visible			= (wTextlayer.TopMaterial <> "PlayfieldText4")
		End If
		' Frank
		pFrankHeadGIOff.Material 		= "Prims platt" & IIF(GILevel=-1," Frank",Abs(GILevel+1))
		pFrankBodyGIOff.Material 		= "Prims platt" & IIF(GILevel=-1," Frank",Abs(GILevel+1))
		pFrankLeftArmGIOff.Material 	= "Prims platt" & IIF(GILevel=-1," Frank",Abs(GILevel+1))
		pFrankRightArmGIOff.Material 	= "Prims platt" & IIF(GILevel=-1," Frank",Abs(GILevel+1))
		pFrankHeadGIOff.Visible 		= (pFrankHeadGIOff.Material <> "Prims platt4")
		pFrankBodyGIOff.Visible 		= pFrankHeadGIOff.Visible
		pFrankLeftArmGIOff.Visible 		= pFrankHeadGIOff.Visible
		pFrankRightArmGIOff.Visible 	= pFrankHeadGIOff.Visible
		' playfield elements
		For Each obj In GIElementsGIOff
			obj.Material 	= "Prims platt" & IIF(GILevel=-1,IIF((Left(obj.Name,2)="sw")," Frank",""),Abs(GILevel+1))
			obj.Visible 	= pFrankHeadGIOff.Visible
		Next
		pSiderailsLockbarGIOff.Material = "Prims platt" & IIF(GILevel=-1,"",Abs(GILevel+1))
		pSiderailsLockbarGIOff.Visible  = (pFrankHeadGIOff.Visible And (RailsVisible = 1 Or (RailsVisible = 2 And DesktopMode)))
		If GILevel = -1 Then
			For Each obj In GIElements : obj.Visible = False : Next
			SetSidewallsVisibility False
			pSiderailsLockbar.Visible = False
		End If
		UseGIMaterial GIWireTrigger, "Metal0.8", Abs(GILevel+1)
		UseGIMaterial sGates, "Metal Chrome S34", Abs(GILevel+1)
		UseGIMaterial Array(kIceCave,kVUK,pSpinner54Rod), "Metal Chrome S34", Abs(GILevel+1)
		If GILevel = -4 Then AdjustLights
		GILevel = GILevel + 1
	End If
End Sub

Sub InvalidateFrank()
	If aFlasherValue(0) <> -1 Then IlluminateFrank 0, aFlasherValue(0)
	If aFlasherValue(2) <> -1 Then IlluminateFrank 2, aFlasherValue(2)
End Sub
Sub IlluminateFrank(flasherID, flasherVal)
	If flasherID <> 0 And flasherID <> 2 Then Exit Sub
	If flasherVal <= -99 Then
		If flasherID = 0 Then
			pFrankHeadFOnGIOff.Image 		= "FrankHead GI-Off RFlasher-On"
			pFrankBodyFOnGIOff.Image 		= "FrankBody GI-Off RFlasher-On"
			pFrankLeftArmFOnGIOff.Image 	= "FrankLArm GI-Off RFlasher-On"
			pFrankRightArmFOnGIOff.Image 	= "FrankRArm GI-Off RFlasher-On"
		ElseIf flasherID = 2 Then
			pFrankHeadFOnGIOff.Image 		= "FrankHead GI-Off LFlasher-On"
			pFrankBodyFOnGIOff.Image 		= "FrankBody GI-Off LFlasher-On"
			pFrankLeftArmFOnGIOff.Image 	= "FrankLArm GI-Off LFlasher-On"
			pFrankRightArmFOnGIOff.Image 	= "FrankRArm GI-Off LFlasher-On"
		End If
		pFrankHeadFOnGIOff.Material 	= "Prims platt" & IIF(isGIOn, "4", " Frank")
	ElseIf flasherVal > 0 Then
		pFrankHeadFOnGIOff.Material 	= "Prims platt" & IIF(isGIOn, "4", 4-flasherVal)
	Else
		pFrankHeadFOnGIOff.Material 	= "Prims platt4"
	End If
	pFrankBodyFOnGIOff.Material 	= pFrankHeadFOnGIOff.Material
	pFrankLeftArmFOnGIOff.Material 	= pFrankHeadFOnGIOff.Material
	pFrankRightArmFOnGIOff.Material = pFrankHeadFOnGIOff.Material
	pFrankHeadFOnGIOff.Visible 		= (pFrankHeadFOnGIOff.Material <> "Prims platt4")
	pFrankBodyFOnGIOff.Visible 		= pFrankHeadFOnGIOff.Visible
	pFrankLeftArmFOnGIOff.Visible 	= pFrankHeadFOnGIOff.Visible
	pFrankRightArmFOnGIOff.Visible 	= pFrankHeadFOnGIOff.Visible
End Sub

Sub AdjustLights()
	AdjustInserts
	AdjustWhiteFlasher
End Sub
Dim areInsertsHigh : areInsertsHigh = False
Const HighInsertsFactor = 2.7
Dim areWhiteFlasherHigh : areWhiteFlasherHigh = False
Const HighWhiteFlasherFactor = 3
Sub AdjustInserts()
	If (isGIOn And Not areInsertsHigh) Or (Not isGIOn And areInsertsHigh) Then Exit Sub
	Dim obj
	For Each obj In InsertLights
		If areInsertsHigh Then
			obj.IntensityScale = obj.IntensityScale / HighInsertsFactor
		Else
			obj.IntensityScale = obj.IntensityScale * HighInsertsFactor
		End If
	Next
	areInsertsHigh = Not areInsertsHigh
End Sub
Sub AdjustWhiteFlasher()
	If (isGIOn And Not areWhiteFlasherHigh) Or (Not isGIOn And areWhiteFlasherHigh) Then Exit Sub
	Dim obj
	For Each obj In WhiteFlasher
		If areWhiteFlasherHigh Then
			obj.Intensity = obj.Intensity / HighWhiteFlasherFactor
		Else
			obj.Intensity = obj.Intensity * HighWhiteFlasherFactor
		End If
	Next
	areWhiteFlasherHigh = Not areWhiteFlasherHigh
End Sub

Sub UseGIMaterial(coll, matName, currentGILevel)
	Dim obj, mat
	mat = CreateMaterialName(matName, currentGILevel)
	For Each obj In coll : obj.Material = mat : Next
End Sub
Function CreateMaterialName(matName, currentGILevel)
	CreateMaterialName = matName & IIF(currentGILevel=0, " Dark", IIF(currentGILevel<4, " Dark" & currentGILevel,""))
End Function

Dim setImage : setImage = False
Sub SetSidewallsVisibility(mode)
	Select Case SelectSidewalls
	Case 0
		If DesktopMode Then pSidewallsDT.Visible = mode Else pSidewallsFS.Visible = mode
	Case 1
		pSidewallsFS.Visible = mode
	Case 2
		pSidewalls.Visible = mode
	Case 3
		pSidewallsDT.Visible = mode
	Case 4
		' nothing to do
		If Not setImage Then setImage = True : pSidewalls.Image = "SidewPlasTexCombined GI-Off"
		pSidewalls.Visible = mode
	End Select
End Sub


' *********************************************************************
' sound stuff and a bit ball jumping
' *********************************************************************
Sub sGates_Hit(idx) : PlaySoundAtVolPitch SoundFX("fx_gate",DOFContactors), ActiveBall, 0.02, .25 : End Sub
Sub sMetalWalls_Hit(idx) : PlaySoundAtBallAbsVol "fx_metalhit" & Int(Rnd*3), Minimum(Vol(ActiveBall),0.5) : End Sub
Sub sPlastics_Hit(idx) : PlaySoundAtBallAbsVol "fx_ball_hitting_plastic", Minimum(Vol(ActiveBall),0.5) : End Sub
Sub sRubberWalls_Hit(idx) : PlaySoundAtBallVol "fx_rubber_hit_" & Int(Rnd*3)+1, 10 : OnRubberWallHit : End Sub
Sub sRubberPosts_Hit(idx) : PlaySoundAtBallVol "fx_rubber_hit_" & Int(Rnd*3)+1, 10 : OnRubberPostHit : End Sub

Sub OnDropTargetHit() : PlaySoundAtVolPitch SoundFX("fx_droptarget",DOFTargets), ActiveBall, 2, .5 : OnTargetHit : End Sub
Sub OnStandupTargetHit() : PlaySoundAtVolPitch SoundFX("fx_target",DOFTargets), ActiveBall, 2, .5 : OnTargetHit : End Sub
Sub OnTargetHit() : ActiveBall.VelZ = ActiveBall.VelZ * (0.5 + (Rnd()*LetTheBallJump + Rnd()*LetTheBallJump + 1) / 6) : End Sub
Sub OnRubberWallHit() : ActiveBall.VelZ = ActiveBall.VelZ * (0.8 + (Rnd()*(LetTheBallJump-1) + 1) / 6) : End Sub
Sub OnRubberPostHit() : ActiveBall.VelZ = ActiveBall.VelZ * (0.9 + (Rnd()*(LetTheBallJump-1) + Rnd()*(LetTheBallJump-1) + 1) / 6) : End Sub

Sub RollOverSound() : PlaySoundAtVolPitch SoundFX("fx_rollover",DOFContactors), ActiveBall, 2, .25 : End Sub  
Sub SwitchSound(switch) : PlaySoundAt SoundFX("fx_sensor",DOFContactors), switch : End Sub

Sub trUpperRampStart_Hit() : SlowDownBall ActiveBall : End Sub
Sub trUpperRampWireStart_Hit() : isBallOnWireRamp = True : End Sub
Sub trUpperRampWireEnd_Hit() : isBallOnWireRamp = False : End Sub

Sub trLowerRampBeforeStart_Hit() : isBallOnRamp = False : End Sub
Sub trLowerRampStart_Hit() : isBallOnRamp = True : SpeedUpRamp ActiveBall : End Sub
Sub trLowerRampStart2_Hit() : SpeedUpRamp ActiveBall : End Sub
Sub trLowerRampEnd_Hit() : isBallOnRamp = False : End Sub

' ball collision sound
Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound "fx_collide", 0, Csng(velocity) ^ 2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub SlowDownBall(actBall)
	With actBall
		If .VelY < 0 Then .VelY = .VelY / 2 : .VelX = .VelX / 2
	End With
End Sub

Sub SpeedUpRamp(actBall)
	With actBall
		If .VelY < 0 Then .VelY = .VelY * 1
	End With
End Sub


' *********************************************************************
' supporting Surround Sound Feedback (SSF) functions
' *********************************************************************
' set position as table object (Use object or light but NOT wall) and Vol to 1
Sub PlaySoundAt(sound, tableobj)
	PlaySound sound, 1, 1, AudioPan(tableobj), 0, 0, 0, 1, AudioFade(tableobj)
End Sub
' set position as table object and Vol manually.
Sub PlaySoundAtVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, AudioPan(tableobj), 0, 0, 0, 1, AudioFade(tableobj)
End Sub
' set position as table object and Vol + RndPitch manually 
Sub PlaySoundAtVolPitch(sound, tableobj, Vol, RndPitch)
	PlaySound sound, 1, Vol, AudioPan(tableobj), RndPitch, 0, 0, 1, AudioFade(tableobj)
End Sub

' set all as per ball position & speed.
Sub PlaySoundAtBall(sound)
	PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub
' set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3
Sub PlaySoundAtBallVol(sound, VolMult)
	PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub
Sub PlaySoundAtBallAbsVol(sound, VolMult)
	PlaySound sound, 0, VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

' play a looping sound at a location with volume
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

' requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	Dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
	PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' set position as bumperX and Vol manually. Allows rapid repetition/overlaying sound
Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, AudioPan(tableobj), 0, 0, 1, 1, AudioFade(tableobj)
End Sub


' *********************************************************************
' Supporting Ball & Sound Functions
' *********************************************************************
Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 200)
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
    VolZ = Csng(BallVelZ(ball) ^2 / 2000)
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table.
	Dim tmp : tmp = tableobj.x * 2 / Frankenstein.Width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function AudioFade(ball)
    Dim tmp : tmp = ball.y * 2 / Frankenstein.Height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
    BallVelZ = INT((ball.VelZ) * -1 )
End Function


' *************************************************************
' ball shadow and ramp look
' *************************************************************
Const tnob = 6 ' total number of balls

Dim BallShadow : BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6)

Sub GraphicsTimer_Timer()
	Dim ii

	' maybe show ball shadows
	If ShowBallShadow <> 0 Then
		Dim BOT
		BOT = GetBalls
		' hide shadow of deleted balls
		If UBound(BOT) < tnob - 1 Then
			For ii = UBound(BOT) + 1 To tnob - 1
				If BallShadow(ii).Visible Then BallShadow(ii).Visible = False
			Next
		End If
		' render the shadow for each ball
		For ii = 0 to UBound(BOT)
			If BOT(ii).X < Frankenstein.Width/2 Then
				BallShadow(ii).X = ((BOT(ii).X) - (Ballsize/6) + ((BOT(ii).X - (Frankenstein.Width/2))/7)) + 6
			Else
				BallShadow(ii).X = ((BOT(ii).X) + (Ballsize/6) + ((BOT(ii).X - (Frankenstein.Width/2))/7)) - 6
			End If
			BallShadow(ii).Y = BOT(ii).Y + 12
			BallShadow(ii).Visible = (BOT(ii).Z > 20)
		Next
	End If

	' maybe move primitive flippers
	pLeftFlipper.ObjRotZ  = LeftFlipper.CurrentAngle +180	
	pLeftFlipperGIOff.ObjRotZ  = LeftFlipper.CurrentAngle +180	
	pRightFlipper.ObjRotZ = RightFlipper.CurrentAngle +180	
	pRightFlipperGIOff.ObjRotZ = RightFlipper.CurrentAngle +180	
	pRightUpperFlipper.ObjRotZ = RightUpperFlipper.CurrentAngle +180	
	pRightUpperFlipperGIOff.ObjRotZ = RightUpperFlipper.CurrentAngle +180	
	pLeftFlipperShadow.ObjRotZ  = LeftFlipper.CurrentAngle + 1
	pRightFlipperShadow.ObjRotZ = RightFlipper.CurrentAngle + 1
	pRightUpperFlipperShadow.ObjRotZ = RightUpperFlipper.CurrentAngle + 1
End Sub


' *************************************************************
'      rolling and realtime sounds
' *************************************************************
ReDim rolling(tnob)
For i = 0 to tnob : rolling(i) = False : Next
Dim isBallOnRamp : isBallOnRamp = False
Dim isBallOnWireRamp : isBallOnWireRamp = False

Sub RollingSoundTimer_Timer()
	Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound "fx_ballrolling" & b
        StopSound "fx_plasticrolling" & b
        StopSound "fx_metalrolling" & b
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
	Dim bOWR : bOWR = isBallOnWireRamp
    For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 Then
            rolling(b) = True
			If bOWR Then
				' ball on wire ramp
				bOWR = False
				StopSound "fx_ballrolling" & b
				StopSound "fx_plasticrolling" & b
				PlaySound "fx_metalrolling" & b, -1, Vol(BOT(b))*RollingSoundFactor, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			ElseIf BOT(b).Z > 30 Then
				' ball on plastic ramp
				StopSound "fx_ballrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_plasticrolling" & b, -1, Vol(BOT(b))*RollingSoundFactor, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			Else
				' ball on playfield
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_ballrolling" & b, -1, Vol(BOT(b))*RollingSoundFactor, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			End If
		Else
			If rolling(b) Then
                StopSound "fx_ballrolling" & b
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
                rolling(b) = False
            End If
		End If
		
		'   ball drop sounds matching the adjusted height params
		If BOT(b).VelZ < -2 And BOT(b).Z < 55 And BOT(b).Z > 27 And Not isBallOnRamp Then
			PlaySound "fx_balldrop" & Int(Rnd()*3), 0, ABS(BOT(b).VelZ)/17*5, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If

'		' saucers sw53 and sw55 and scoops sw31 and sw32
		If InCircle(BOT(b).X, BOT(b).Y, sw53.X, sw53.Y, 20) And BOT(b).Z < 0 Then
			If Abs(BOT(b).VelX) < 3 And Abs(BOT(b).VelY) < 3 Then
				BOT(b).VelX = BOT(b).VelX / 3
				BOT(b).VelY = BOT(b).VelY / 3
				If Not Controller.Switch(53) Then
					Controller.Switch(53) 	= True
					Set sw53Ball 			= BOT(b)
					FrankIsWatching sw53Ball
				End If
			End If
		End If
		If InCircle(BOT(b).X, BOT(b).Y, sw55.X, sw55.Y, 20) And BOT(b).Z < 0 Then
			If Abs(BOT(b).VelX) < 3 And Abs(BOT(b).VelY) < 3 Then
				BOT(b).VelX = BOT(b).VelX / 3
				BOT(b).VelY = BOT(b).VelY / 3
				If Not Controller.Switch(55) Then
					Controller.Switch(55) 	= True
					Set sw55Ball 			= BOT(b)
					FrankIsWatching sw55Ball
				End If
			End If
		End If
		If InCircle(BOT(b).X, BOT(b).Y, sw31.X, sw31.Y, 20) And BOT(b).Z < 0 Then
			If Abs(BOT(b).VelX) < 3 And Abs(BOT(b).VelY) < 3 Then
				BOT(b).VelX = BOT(b).VelX / 3
				BOT(b).VelY = BOT(b).VelY / 3
				If Not Controller.Switch(31) Then
					Controller.Switch(31) 	= True
					Set sw31Ball 			= BOT(b)
					FrankIsWatching sw31Ball
				End If
			End If
		End If
		If InCircle(BOT(b).X, BOT(b).Y, sw32.X, sw32.Y, 20) And BOT(b).Z < 0 Then
			If Abs(BOT(b).VelX) < 3 And Abs(BOT(b).VelY) < 3 Then
				BOT(b).VelX = BOT(b).VelX / 3
				BOT(b).VelY = BOT(b).VelY / 3
				If Not Controller.Switch(32) Then
					Controller.Switch(32) 	= True
					Set sw32Ball 			= BOT(b)
					FrankIsWatching sw32Ball
				End If
			End If
		End If

    Next
End Sub


' *********************************************************************
' additional physics stuff
' *********************************************************************
Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
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
	AddPt "Polarity", 0, 0, -4.7
	AddPt "Polarity", 1, 0.16, -4.7	
	AddPt "Polarity", 2, 0.33, -4.7
	AddPt "Polarity", 3, 0.37, -4.7
	AddPt "Polarity", 4, 0.41, -4.7
	AddPt "Polarity", 5, 0.45, -4.7 
	AddPt "Polarity", 6, 0.576,-4.7
	AddPt "Polarity", 7, 0.66, -2.8
	AddPt "Polarity", 8, 0.743, -1.5
	AddPt "Polarity", 9, 0.81, -1.5
	AddPt "Polarity", 10, 0.88, 0

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'******************************************************
'		FLIPPER CORRECTION SUPPORTING FUNCTIONS
'******************************************************
Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

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
		if abs(Flipper.currentAngle - Flipper.EndAngle) < 30 Then
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
				'playsound "knocker"
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

RightFlipper.timerinterval=1
RightFlipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricksL LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricksR RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperTricksR RightUpperFlipper, RUFPress, RUFCount, RUFEndAngle, RUFState
end sub

dim LFPress, RFPress, RUFPress
dim LFCount, RFCount, RUFCount
dim LFState, RFState, RUFState
dim EOST, EOSA,Frampup, FElasticity
dim RFEndAngle, LFEndAngle, RUFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
Const EOSTnew = 1.0 'FEOST
Const EOSAnew = 0.2
Const EOSRampup = 0.5 
Const SOSRampup = 2.5 
Const LiveCatch = 8
Const LiveElasticity = 0.45
Const SOSEM = 0.815

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
RUFEndAngle = RightUpperFlipper.endangle

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
			Flipper.endangle = FEndAngle - 3
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

'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR

RDampen.Interval = 10
RDampen.Enabled = True
Sub RDampen_Timer()
	Cor.Update
End Sub

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
RubbersD.addpoint 0, 0, 0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
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
		'playsound "knocker"
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
		if uBound(allballs) < 0 then if DebugOn then str = "no balls" : TBPin.text = str : exit Sub else exit sub end if: end if
		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			if DebugOn then 
				dim s, bs 'debug spacer, ballspeed
				bs = round(BallSpeed(b),1)
				if bs < 10 then s = " " else s = "" end if
				str = str & b.id & ": " & s & bs & vbnewline 
				'str = str & b.id & ": " & s & bs & "z:" & b.z & vbnewline 
			end if
		Next
		if DebugOn then str = "ubound ballvels: " & ubound(ballvel) & vbnewline & str : if TBPin.text <> str then TBPin.text = str : end if
	End Sub
End Class


' *********************************************************************
' some more general methods
' *********************************************************************
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

Function InCircle(pX, pY, centerX, centerY, radius)
	Dim route
	route = Sqr((pX - centerX) ^ 2 + (pY - centerY) ^ 2)
	InCircle = (route < radius)
End Function

Function Minimum(val1, val2)
	Minimum = IIF(val1<val2,val2,val1)
End Function

Const fileName = "C:\Games\Visual Pinball\User\Log.txt"
Sub WriteLine(text)
	Dim fso, fil
	Set fso = CreateObject("Scripting.FileSystemObject")
	Set fil = fso.OpenTextFile(fileName, 8, true)
	fil.WriteLine Date & " " & Time & ": " & text
	fil.Close
	Set fso = Nothing
End Sub

Function IIF(bool, obj1, obj2)
	If bool Then
		IIF = obj1
	Else
		IIF = obj2
	End If
End Function

Function Maximum(obj1, obj2)
	If obj1 > obj2 Then
		Maximum = obj1
	Else
		Maximum = obj2
	End If
End Function

Function GetBallID(actBall)
	Dim b, BOT, ret
	ret = -1
	BOT = GetBalls
	For b = 0 to UBound(BOT)
		If actBall Is BOT(b) Then
			ret = b : Exit For
		End If
	Next
	GetBallID = ret
End Function