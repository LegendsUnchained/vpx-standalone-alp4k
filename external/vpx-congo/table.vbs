'############################################################################################
'############################################################################################
'#######                                                                             ########
'#######									Congo                                    ########
'#######     						   (Williams 1988)                               ########
'#######                                                                             ########
'############################################################################################
'############################################################################################

'VPW Table Tuneup Mod v1.0
'Based on nFozzys VPX version with some assets from Dark and LoadedWeapon, which was based on JPSalas VP9 version.
'
''** VPW Mod V1.0 - CHANGE LOG **
'*****************************************
'			VPin Workshop Revisions
'*****************************************
'Started from Skitso mod
'004 - Rastan350 - new physics and sounds
'005 - Skitso - reworked all GI and flashers, improved insert lighting, new LUT
'006 - iaakki - debugged some flip physics issues and now live catch and nudge works, flip angles changed, removed some lights, flip physics parameters updated
'007 - Skitso - temporary fix for broken flashers. Replaced SHOOT AGAIN and KICKBACK insert textures to a more visually pleasing ones.
'008 - iaakki - merged flashers. Solflash17 created for "modulated" Amy flasher
'009 - Skitso - fixed flashers, fixed ball shadow depth bias issue, added one missing GI light under left ramp, made upper lane guide lamps to show through AMY's hand, new volcano ramp textures, improved Grey playfield texture and shadows, new dark texture for yellow hit targets on Grey playfield, added tiny bit of DL to Grey when GI is lit, fixed right sligshot plastics material and repositioned light beneath
'010 - Skitso - improved rule card texture, backwall texture and left ramp decal. Added backfacing transparent triangles rendering to ramps, improved bumber lighting.
'011 - iaakki - ball drop sounds fixed. It is still bugged occasionally but not sure why..
'012 - Skitso - better upper PF laneguide prim, improved lights for upper lane guides, tweaked flashers.
'015 - iaakki - fixed ramp exits so that ball drop sounds can work properly
'016 - Sixtoe - Re-organised whole table, added VR room and assets, replaced rails and sideblades (switchable for art ones, which I redid a bit), raised some lights and dropped others (to stop it cutting prims in half), fixed z fighting for several walls, removed numerous unused assets, redid sling rubbers and area (including physics objects), cut holes in playfield and made drop holes, turned off backfacing rendering for ramps as it breaks VR, will try and find a workaround, raised DL meanwhile, removed redundant drop code, removed wall that stopped ball dropping off wire ramp, probably some other stuff I forgot
'017 - Benji - New ramps based on flupper's new tutorial
'018 - Flupper - New ramp models, new ramp textures. "plsatic_ramps" texture in image manager is better for VR
'019 - tomate - LowPoly left ramp added.
'020 - tomate - flipperTimer added, flippers prims and shadows, slightly corrected left flipper location, New wireRamps prims, plastic ramps slightly modified to fit wire ramps, DC-3 model added
'021 - tomate - WireRamps textures added, split wire ramps (up and down), down wireRamp reflection disabled, plastic plane recovered from previous version
'022 - iaakki - Gorilla flips physics change, LUT changer added with failsafe improvements, Cabinet mode improved, RampLook option added
'023 - Sixtoe - Complete fixtures and fittings pass, changed most things, tons of small tweaks and adjustments to positions of things, drilled a hole in the plastic ramp, added seperate VR graphics setup for ramps and wire runs, unified timers, probably something else I forgot.
'024 - Sixtoe - Tinkered with the metals and lighting to get the fixings more natural and suited to table.
'026 - tomate - new 3d apron and apron texture added, apron rails and POV fixed
'027 - Sixtoe - Added playfield mesh from Bord, more tinkering and adjusting including apron area for VR and finally punting it out the door
'RC2 - Skitso - Fixed playfield mesh location, made ball shinier, altered few materials, made apron a tad more in shadow.
'RC3 - iaakki - Fixed GI control, added TargetBouncerEnabled and RubberizerEnabled script options to make table feel more real
'RC4 - Skitso - Improved GI, small insert tweaks
'RC4.1 - Skitso - Grouped a few missing GI lights
'RC4.2 - tomate - post-draw textures added, new primitives and textures for VUK exits added
'RC5 - Sixtoe - Sling kickers adjusted, changed back wall layout, adjusted vuk prims and added material, fixed leftrampdrop height and visibility, tweaked metal pole on left orbit, changed env image, altered materials and textures for metals and volcano, cropped new GI lamps.
'RC6 - iaakki - FlipperNudge tuned. Sw55 target had incorrect phys parameters. gi_bulb022/23 adjust
'RC7 - Skitso - tuned satellite inserts and made all purple inserts more natural color, tuned Gi_Bulb014, tuned perimeter defence flasher
'RC8 - iaakki - l82f fixed. ramp tied to gi. RubberBand material adjusted, ramp41 decal adjusted
'RC9 - Sixtoe - Fixed VR script, altered desktop backdrop

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'///////////////////////-----OPTIONS-----///////////////////////

'///////////////////////-----VR Room-----///////////////////////
Const VRRoom = 0		'0 - VR Room off, 1 - 360 Room, 2 - Minimal Room, 3 - Ultra Minimal

'/////////////////////-----Cabinet Mode-----/////////////////////
Const CabinetMode = 0	'0 - Off, 1 - Hides rails & scales side panels, 2 - Hides rails and side panels

'///////////////////-----Plastic Ramp Look-----//////////////////
Const RampLook = 0		'0 - Default (For Desktop and VR), 1 - Flupper version for cabinet mode

'////////////////-----Cabinet Art Side Panels-----///////////////
Const ArtSides = 0		'0 - Off (black wood), 1 - Mountain sides

'///////////////////////-----Inlane Type-----////////////////////
InlaneType 0		'0 = smooth feed to the left flipper, 1 = old sticky inlane

'///////////////////////-----Target Bouncer-----////////////////////
Const TargetBouncerEnabled = 1 '0 = normal standup targets, 1 = bouncy targets

'///////////////////////-----Rubberizer-----////////////////////
const RubberizerEnabled = 1 '0 = normal flip rubber, 1 = more lively rubber for flips


'const HardFlips = 1	'more rigid flippers
const SingleScreenFS = 0	'Single Screen FS support

''ramps texture brightness
'Prim_Ramp1.blenddisablelighting = .035
'Prim_Ramp2.blenddisablelighting = .015
'Prim_Ramp3.blenddisablelighting = .045

'Prim_Ramp1.blenddisablelighting = .135
'Prim_Ramp2.blenddisablelighting = .115
'Prim_Ramp3.blenddisablelighting = .145

'Prim_Ramp1.blenddisablelighting = .7
'Prim_Ramp2.blenddisablelighting = .7
'Prim_Ramp3.blenddisablelighting = 2
'------------

Dim luts, lutpos	
luts = array("ColorGradeLUT256x16_1to1", "ColorGradeLUT256x16_ConSat", "lut_colorful2_contrastup", "lut_fs56_darker2", "lut_inverse", "LUT1_1" )
LoadLUT
Table1.ColorGradeImage = luts(lutpos)


Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim UseVPMDMD
If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode
Dim UseVPMColoredDMD
Const UseVPMModSol = 1

if SingleScreenFS = 1 then UseVPMColoredDMD = True else UseVPMColoredDMD = DesktopMode

LoadVPM "01560000", "WPC.VBS", 3.5

' Thal: because of useSolenoids=2
Const cSingleRFlip = 0

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8
Const VolumeDialRamps = 0.5


'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0

'Const UseGI = 0

' Standard Sounds
'Const SSolenoidOn = "fx_solenoid"
'Const SSolenoidOff = ""
Const SCoin = "fx_Coin"

Const cGameName = "congo_21"

Dim bsTrough, bsAmyVuk, bsVolcano, bsMap, bsMystery, LowerPlayfieldBall

dim bip : bip = 0

'Set GICallback = GetRef("UpdateGIon")
Set GICallback2 = GetRef("UpdateGI")
'Set MotorCallback = GetRef("RollingUpdate") 'realtime updates - rolling sound

Sub InLaneType(i)
	if i = 1 Then
		LeftInLane_Smooth.isdropped = 1
		LeftInLane_Sticky.Isdropped = 0
	Else
		LeftInLane_Smooth.isdropped = 0
		LeftInLane_Sticky.Isdropped = 1
	End If
End Sub


'ignore this

sub ReflectionsModulate(value)
	dim x
	for each x in Co1
		x.modulatevsadd = value
	Next
End Sub

sub ReflectionsOpacity(value)
	dim x
	for each x in Co1
		x.opacity = value
	Next
End Sub

GI28.visible = 1
GI29.visible = 1
Gi30.visible = 1

'lower gorilla GI
giflare4.height = -35
GIflare4.rotx = 5
GIflare4.roty = 60
GIflare4.rotz = -4.5
giflare4.x = 605
giflare4.y = 1340 '1281.39

giflare5.height = -15
giflare5.rotx = 12
giflare5.roty = -60
giflare5.rotz = -15
giflare5.x = 275
giflare5.y = 1345 '1281.39

gorillaleft 1

congoG.height = -142.4
congog.rotx = 12
congoG.opacity = 25000
congoG.modulatevsadd = 1

Gi_flasher.opacity = 5000


'************
' Table init.
'************
Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Congo (Williams 1995)"
        .Games(cGameName).Settings.Value("rol") = 0 'set it to 1 to rotate the DMD to the left
'        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
'        .Hidden = DesktopMode
'       .Hidden = 0
        If DesktopMode then
			.Hidden = 1
		else
			If SingleScreenFS = 1 then
				.Hidden = 1
			else
				.Hidden = 0
			End If
        End If
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
        .Switch(22) = 1 'close coin door
        .Switch(24) = 1 'and keep it close
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 0.25
    vpmNudge.TiltObj = Array(bumper1, bumper2, bumper3, LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmTrough
    With bsTrough
        .size = 4
        .initSwitches Array(32, 33, 34, 35)
        .Initexit BallRelease, 90, 4
        .Balls = 4
    End With

	 '   Volcano
	Set bsVolcano = New cvpmTrough
	With bsVolcano
		.size = 4
        .initSwitches Array(41, 42, 43)
        .Initexit sw36a, 260, 10
		.InitExitVariance 10, 1	'direction, force
		.InitExitVariance 2, 2
'        .MaxBallsPerKick = 2
    End With


	'2-way Popper
	Set bsAmyVuk = New cvpmSaucer
	With bsAmyVuk
		.InitKicker sw53, 53, 330, 30, 65	'up 'switch, direction, force, Zforce
		.InitAltKick 145, 30, 60 'down
	End With

	Set BsMystery = New cvpmSaucer
	With bsMystery
		.InitKicker sw37, 37, 185, 20, 45
		.InitExitVariance 2, 1
	End With

	Set bsMap = New cvpmSaucer
	With bsMap
		.InitKicker sw38, 38, 210, 20, 45
		.InitExitVariance 2, 1
	End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

	' Init Kickback
    KickBack.Pullback
	AutoPlunger.Pullback

    ' Init other dropwalls - animations
	LeftPost.IsDropped = 1:SolTopPost 0
	TopPost.IsDropped = 0
	leftpost_invis.IsDropped = 1

	'Lower Playfield Ball
	CreateLPFball 
	gorillaleft 0

	FlashLevel(0) = 1.1 : 	FlashLevel(1) = 0.98 : 	FlashLevel(2) = 0.98	'boot gi (needs help)
	UpdateGI 0, 8:UpdateGI 1, 8:UpdateGI 2, 8
End Sub

Sub CreateLPFball
	Set LowerPlayfieldBall = kickerLPF.Createball
	with LowerPlayfieldBall
'		.image = "ball_HDR"
		.color = RGB(108,108,108)	'148
		.BulbIntensityScale = 0
	end with
	kickerLPF.Kick 180, 1
End Sub

Sub LPFcatcherTrigger_hit()	'in case the ball bugs
'	tb.text = "hit!"
	me.destroyball
	CreateLPFball
end sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub

'***********
' Update GI
'***********
Dim gistep, Giswitch, xx
Giswitch = 0
'
'Sub UpdateGIOn(no, Enabled)
''	tbs.text = "on:" & no & " " & enabled
'	debug.print "UpdateGIOn: " & no
'	Select Case no
'		Case 0 'Gorilla
'			If Enabled Then
''				SetLamp 190, 1
''				For each xx in GIGorilla:xx.State = 1: Next
'				setmodlamp 0, gistepm
'				FadingLevel(0) = 5
'			Else
''				For each xx in GIGorilla:xx.state = 0: Next
''				SetLamp 190, 0
'				setmodlamp 0, 0
''				FadingLevel(0) = 4
'			End If
'		Case 1
'			If Enabled Then
''				For Each xx in GiTop:xx.state = 1: Next
'''				Gi_flasher.visible = 1	'spotlight
''				SetLamp 191, 1
'				setmodlamp 1, gistepm
'				FadingLevel(1) = 5
'				GI28.state = 1
'				GI29.state = 1
'				Gi30.state = 1
'			else
''				For Each xx in GiTop:xx.state = 0: Next
'''				Gi_flasher.visible = 0
''				SetLamp 191, 0
''				UpdateLightScaling 0, 1
'				setmodlamp 1, 0
''				FadingLevel(1) = 4
'				GI28.state = 0
'				GI29.state = 0
'				Gi30.state = 0
'			End If
'		Case 2
'			If Enabled Then
''				For Each xx in GiBottom:xx.state = 1: Next
''				SetLamp 192, 1
'				setmodlamp 2, gistepm
'				FadingLevel(2) = 5
'			Else
''				For Each xx in GiBottom:xx.state = 0: Next
''				SetLamp 192, 0
''				UpdateLightScaling 0, 2
'				setmodlamp 2, 0
''				FadingLevel(2) = 4
'
'			End If
'	End Select
'End Sub
'cutting down the intensity a bit
'min 50% intensityscale

'x = intensityscale y = gistep
'x1= 0.5 y1= 1
'x2= 1   y2= 7

'solve for slope
''m = (y2 - y1) / (x2 - x1)
'	(7 - 1) / (1 - 0.5)
'	6 / 0.5
'm = 12

'point slope formula
'y - y1 = m(x-x1)
'	y - 1 = 12(x-0.5)

'y = 12x -5
'x = (y+5)/12

dim gistepm
Sub UpdateGI(no, step)
	'debug.print "UpdateGI: " & no &" step: "& step	
    Dim ii, x
    'If step = 0 then exit sub 'only values from 1 to 8 are visible and reliable. 0 is not reliable and 7 & 8 are the same so...

    'gistep = (step-1)' / 7
	gistepm = step / 8
	'gistepm = ScaleGI(step, 25)
'	tbs.text = "mod:" & no & " " & step
    Select Case no
        Case 0
'			textbox1.text = step
			If step > 0 and step < 8 Then
				For each xx in GIGorilla:xx.State = 1: Next
			Else
				For each xx in GIGorilla:xx.state = 0: Next
			End If
				GorillaGI
			For each ii in GiGorilla
				ii.IntensityScale = gistepm
			Next
			GI_gorilla.opacity = GI_gorilla.uservalue * gistepm
			GIflare4.opacity = GIflare4.uservalue * gistepm
			GIflare5.opacity = GIflare5.uservalue * gistepm
			'debug.print "UpdateGI 0: "& gistepm
			setmodlampf 0, gistepm
        Case 1
			'UpdateLightScaling 1, 1
            For each ii in GiTop
                ii.IntensityScale = gistepm
				' back.Image="backwall"&step
            Next
			Gi_flasher.Opacity = Gi_flasher.UserValue * gistepm	'spotlight
			GIflare1.opacity = GIflare1.uservalue * gistepm
			GIflare2.opacity = GIflare2.uservalue * gistepm
			'debug.print "UpdateGI 1: "& gistepm
			setmodlampf 1, gistepm
        Case 2
			'UpdateLightScaling 1, 2
            For each ii in GiBottom
                ii.IntensityScale = gistepm
            Next
			Prim_Ramp3.blenddisablelighting = 1.1 * gistepm + 0.9
			'0.8 - 2
'			GI_AmbientBottom.Opacity = GI_AmbientBottom.UserValue * gistepm
'			GI_PlasticsBottom.Opacity = GI_PlasticsBottom.UserValue * gistepm
'			Gi_PlasticsBottomLvl2.opacity = GI_PlasticsBottomLvl2.UserValue * gistepm
			GIflare3.opacity = GIflare3.uservalue * gistepm
			'debug.print "UpdateGI 2: "& gistepm
			setmodlampf 2, gistepm
    End Select
End Sub


'This sub scales lights / flashers to compensate for the GI
Sub UpdateLightScaling(onoff, gistring)	'onoff: send 0 for GI on/off callback 'gistring: 1 is top, 2 is bottom, 3 is Lpf
	dim x, ii, GIi, temp1, temp2
	dim s, i
	if onoff = 0 then
	'	exit sub
		ii = 0
		GIi = (9/8) - ii/64
	elseif onoff = 1 then
	'	exit sub	'debug
		ii = gistep	'off behaves as if GIstep at 0, for the GI on/off callback
		GIi = (9/8) - ii/64
'	Else	just for testing
'		Giscale = onoff
	end if

	Select Case gistring
		case 1 'Top
			temp1 = giscale(120)
			temp2 = giscale(125)
			for x = 100 to 200
'				if x = 120 then Continue For
'				if x = 125 then Continue For
				GIscale(x) = GIi
			Next
				GIscale(120) = temp1
				GIscale(125) = temp2	'ug

		'	nModLightm 17, f17b, 0
		'	nModLight 17, f17
		'	nModLight 18, F18
		'	nModLight 19, F19

		'	nModLight 21, F21

		'	nModLight 26, f26
		'	nModLightm 27, F27A, 10	'big ambient	'137
		'	nModLightm 27, F27B, 11	'smaller bulb	'138
		'	nModLightm 27, F27C, 12	'wall absorb (TODO replace eventually with a flasher)	'139
		'	nModLight 27, F27	'primary bulb
		'	nModLightm 28, F28B, 1	'ambient '129
		'	nModLight 28, F28B
			for each x in LampsTOP
				s = mid(x.name, 2, 2)			'take L off the lamp's name
				i = cInt(s)						'convert string to integer to get the lampnumber
				x.intensityscale = GIi
				x.fadespeedup = insertfading(i, 1) * x.intensityscale
				x.fadespeeddown = insertfading(i, 2) * x.intensityscale
			Next
			for each x in LampsMIDDLE
				s = mid(x.name, 2, 2)			'take L off the lamp's name
				i = cInt(s)						'convert string to integer to get the lampnumber
				x.intensityscale = (GIi + 1) / 2	'half scaling for middle inserts
				x.fadespeedup = insertfading(i, 1) * x.intensityscale
				x.fadespeeddown = insertfading(i, 2) * x.intensityscale
			Next
		case 2	'bottom
			GIscale(120) = GIi
			GIscale(125) = GIi
		'	nModLight 20, F20
		'	nModLight 25, F25

			for each x in LampsBOTTOM
				s = mid(x.name, 2, 2)			'take L off the lamp's name
				i = cInt(s)						'convert string to integer to get the lampnumber
				x.intensityscale = GIi
				x.fadespeedup = insertfading(i, 1) * x.intensityscale
				x.fadespeeddown = insertfading(i, 2) * x.intensityscale
			Next
			for each x in LampsMIDDLE
				s = mid(x.name, 2, 2)			'take L off the lamp's name
				i = cInt(s)						'convert string to integer to get the lampnumber
				x.intensityscale = (GIi + 1) / 2	'half scaling for middle inserts
				x.fadespeedup = insertfading(i, 1) * x.intensityscale
				x.fadespeeddown = insertfading(i, 2) * x.intensityscale
			Next
	End Select


End Sub

Sub FadeDisableLightingM(nr, a, alvl)
	Select Case FadingLevel(nr)
		Case 4
			a.UserValue = a.UserValue - 0.335
			If a.UserValue < 0 Then 
				a.UserValue = 0
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
		Case 5
			a.UserValue = a.UserValue + 0.335
			If a.UserValue > 1 Then 
				a.UserValue = 1
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
	End Select
End Sub

dim GiElements
GiElements = Array(Gi_Flasher, GI_Gorilla, GIflare1, GIflare2, Giflare3, Giflare4, giflare5)
dim insertfading(150, 2): 	'columns : 0 = name 1 = fadeup 2 = fadedown
'dim FlashersFading(10, 1)	'0 = fadeup 1 = fadedown
'for x = 0 to ubound(insertfading)
'	insertfading(x, 0) = 0
'	insertfading(x, 1) = 0
'next
	initlampsforfading
Sub initlampsforfading
	dim x, s, i, a(1)
	i = 0
	for each x in Linserts	'setup array
		s = mid(x.name, 2, 2)			'take L off the lamp
		i = cInt(s)	'convert string to integer to get the lampnumber
		insertfading(i, 0) = i
		insertfading(i, 1) = x.fadespeedup
		insertfading(i, 2) = x.fadespeeddown
	next
	for x = 0 to UBOUND(insertfading)
		if insertfading(x, 0) <> 0 then
		exit for
		end If
	next

	'Gi InitAddSnd

	for each x in GiElements
'		x.visible = 0
		x.Uservalue = x.opacity
	Next

'	textbox1.text = insertfading(58, 2)
'	textbox1.text = F4L.UserValue' & vbnewline & F4L.UserValue(1)
end sub


'x = intensityscale y = gistep
'x1= 2.5 y1= 0.5
'x2= 1   y2= 1

'solve for slope
''m = (y2 - y1) / (x2 - x1)
'	(1 - 0.5) / (1 - 2.5)
'm = -1/3

'point slope formula
'y - y1 = m(x-x1)
'y - 0.5 = (-1/3)(x-2.5)




Sub GorillaGI
	If Giswitch = 1 then
	Giswitch = 0
	Gion 1
	Else
	Giswitch = 1
	Gion 0
	End If
End Sub

Sub GIon(i)
	For each xx in GIGorilla:xx.State = i: Next
end sub


'**********
' Keys
'**********


dim LeftFlipperOn, RightFlipperOn
Sub table1_KeyDown(ByVal Keycode)
	If keycode = LeftMagnaSave then
		lutpos = lutpos - 1 : If lutpos < 0 Then lutpos = 0 : end if
		Table1.ColorGradeImage = luts(lutpos)
	End if

	If keycode = RightMagnaSave then
		lutpos = lutpos + 1 : If lutpos > 5 Then lutpos = 5: end if
		Table1.ColorGradeImage = luts(lutpos)
	End if
    
	If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter()

	If KeyCode = PlungerKey Then Plunger.Pullback:SoundPlungerPull()

    'If keycode = PlungerKey Then PlaySoundAt SoundFX("plunger3",0), ActiveBall:Plunger.Pullback:end if
	
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper,LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress

If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

		End Select
	End If

if keycode=StartGameKey then soundStartButton()

	If vpmKeyDown(keycode) Then Exit Sub
End Sub
dim d1, d2
d1 = 15 : d2 = 20


Sub table1_KeyUp(ByVal Keycode)
    If keycode = PlungerKey Then
		Plunger.Fire
		if BallInPlunger then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
End If
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress

    If vpmKeyUp(keycode) Then Exit Sub
End Sub

'*********
' Switches
'*********


'sub drain_hit:drain.destroyball:end sub	'debug, infinite balls
sub destroyer_hit
	bip = bip - 1
	if bip < 0 then bip = 0
	me.destroyball
end sub

' Slings & div switches

Dim Lstep, Rstep

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 61
	  'PlaySoundAt SoundFX("LeftSlingshot",DOFContactors), sling2
	RandomSoundSlingshotLeft sling2 
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -25
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -15
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 62
	  'PlaySoundAt SoundFX("RightSlingshot",DOFContactors), sling1
	  RandomSoundSlingshotRight sling1

    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -25
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -15
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' Bumpers
Sub Bumper1_Hit
	vpmTimer.PulseSw 63
	'PlaySoundAt SoundFX("rightbumper_hit",DOFContactors), ActiveBall
	RandomSoundBumperTop Bumper1
End Sub	'clark

Sub Bumper2_Hit
	vpmTimer.PulseSw 64
	'PlaySoundAt SoundFX("topbumper_hit",DOFContactors), ActiveBall
	RandomSoundBumperMiddle Bumper2
End Sub

Sub Bumper3_Hit
	vpmTimer.PulseSw 65
	'PlaySoundAt SoundFX("leftbumper_hit",DOFContactors), ActiveBall
	RandomSoundBumperBottom Bumper3
End Sub

' Right Eject Rubber
Sub sw25_Hit:vpmTimer.PulseSw 25:End Sub 'Right Eject Rubber

'Shooter Lane

Sub sw18_Hit:Controller.Switch(18) = 1:End Sub
Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub

dim BallInPlunger :BallInPlunger = False
sub PlungerLane_hit():ballinplunger = True: End Sub
Sub PlungerLane_unhit():BallInPlunger = False: End Sub
'sub gate5_hit():stopsound "plunger3": end sub

'Inlane/Outlanes
Sub sw16_Hit:Controller.Switch(16) = 1:End Sub	'Kickback
Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub'kickback

Sub sw26_Hit:Controller.Switch(26) = 1:End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub
Sub sw17_Hit:Controller.Switch(17) = 1:End Sub
Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub
Sub sw27_Hit:Controller.Switch(27) = 1:End Sub
Sub sw27_UnHit:Controller.Switch(27) = 0:End Sub

'Playfield Switches & Rollovers

Sub sw15_Hit:Controller.Switch(15) = 1:End Sub
Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub
Sub sw45_Hit:Controller.Switch(45) = 1:End Sub
Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub
Sub sw11_Hit:Controller.Switch(11) = 1:End Sub
Sub sw11_UnHit:Controller.Switch(11) = 0:End Sub
Sub sw44_Hit:Controller.Switch(44) = 1:End Sub
Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub
Sub sw12_Hit:Controller.Switch(12) = 1:End Sub

'Volcano Switch
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub

'Additional diverter Sub
Sub VolcanoTop_Hit()
	bip = bip + 1
End Sub

Sub sw71_Hit:Controller.Switch(71) = 1:End Sub		'AMY Rollovers
Sub sw71_UnHit:Controller.Switch(71) = 0:End Sub
Sub sw72_Hit:Controller.Switch(72) = 1:End Sub
Sub sw72_UnHit:Controller.Switch(72) = 0:End Sub
Sub sw73_Hit:Controller.Switch(73) = 1:End Sub
Sub sw73_UnHit:Controller.Switch(73) = 0:End Sub

'Targets

Sub sw46_Hit:vpmTimer.PulseSw 46:TargetBouncer(activeball):End Sub
Sub sw47_Hit:vpmTimer.PulseSw 47:TargetBouncer(activeball):End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:TargetBouncer(activeball):End Sub
Sub sw56_Hit:vpmTimer.PulseSw 56:TargetBouncer(activeball):End Sub	'Laser Perimeter
Sub sw51_Hit:vpmTimer.PulseSw 51:TargetBouncer(activeball):End Sub
Sub sw52_Hit:vpmTimer.PulseSw 52:TargetBouncer(activeball):End Sub

Sub sw54_Hit:vpmTimer.PulseSw 54:TargetBouncer(activeball):End Sub			'We Are
Sub sw55_Hit:vpmTimer.PulseSw 55:TargetBouncer(activeball):End Sub			'Watching
Sub sw28_Hit:vpmTimer.PulseSw 28:TargetBouncer(activeball):End Sub			'You

Sub sw74_Hit:vpmTimer.PulseSw 74:End Sub
Sub sw75_Hit:vpmTimer.PulseSw 75:End Sub
Sub sw76_Hit:vpmTimer.PulseSw 76:End Sub
Sub sw77_Hit:vpmTimer.PulseSw 77:End Sub
Sub sw78_Hit:vpmTimer.PulseSw 78:End Sub

'Ramp Switches
Sub sw57_Hit:Controller.Switch(57) = 1:End Sub
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
Sub sw58_Hit:Controller.Switch(58) = 1:End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
Sub sw67_Hit:Controller.Switch(67) = 1:End Sub
Sub sw67_UnHit:Controller.Switch(67) = 0:End Sub
Sub sw68_Hit:Controller.Switch(68) = 1:End Sub
Sub sw68_UnHit:Controller.Switch(68) = 0:End Sub


Dim zMultiplier
'iaakki - TargetBouncer for standup targets
sub TargetBouncer(aBall)
	if TargetBouncerEnabled <> 0 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = 1.55
			Case 2: zMultiplier = 1.2
			Case 3: zMultiplier = 2
			Case 4: zMultiplier = 1
		End Select
		aBall.velz = aBall.velz * zMultiplier
		'debug.print "----> velz: " & activeball.velz
	end if
end sub

'***************************
'   Solenoids & Flashers
'some soleoid subs are from
'Aurian/Guitar/Jive's table
'***************************

SolCallBack(1) = "Auto_Plunger"
SolCallBack(2) = "Kick_back"
SolCallBack(3) = "SolPopUp"
SolCallBack(4) = "SolPopDown"

SolCallback(5) = "RampDiverter"
SolCallBack(6) = "VolcanoKickOut"
SolCallBack(7) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallBack(8) = "SolTopPost"
SolCallBack(9) = "SolRelease"
SolCallBack(15) = "GorillaRight"
SolCallBack(16) = "GorillaLeft"
SolCallback(22) = "MapKick"
SolCallBack(23) = "LeftGateOn"
SolModCallback(17) = "SolFlash17" '"SetModLamp 117,"	'Amy Flasher
'SolCallBack(17) = "SolFlash17_2"
SolModCallback(18) = "SetModLampm 118, 138,"	'Left Ramp Flasher
SolModCallback(19) = "SetModLampm 119, 129,"	'2-Way Popper Flasher
SolModCallback(20) = "SetModLampm 120, 130,"	'SkillShot Flasher
SolModCallback(21) = "SetModLamp 121,"	'Gray Gorilla Flasher
SolCallBack(24) = "RightGateOn"
SolModCallback(25) = "SetModLampm 125, 135,"	'Lower Right Flasher
SolModCallback(26) = "SetModLampm 126, 136,"	'Right Ramp Flasher
SolModCallback(27) = "SetModLampM 127, 137,"	'Volcano Flasher
SolModCallback(28) = "Sol28"	'Perimeter Defense Flasher	'old style
SolCallBack(33) = "SolLeftPost"
SolCallback(34) = "MysteryKick"

'********************
' Special JP Flippers
'********************

 SolCallback(sLRFlipper) = "SolRFlipper"
 SolCallback(sLLFlipper) = "SolLFlipper"
 SolCallback(sULFlipper) = "SolULFlipper"

dim FlashLevel17, Flash17Last, Flash17Dir
Flash17Last = 0
Flash17Dir = 0

f17.IntensityScale = 0
f17b.IntensityScale = 0

sub SolFlash17(aLvl)
	'debug.print "Sol17 output level: " & alvl

	if alvl <> 0 then
		If aLvl > Flash17Last Then 
			Flash17Dir = 2				'flasher goes up
			FlashLevel17 = aLvl/255 * 3	'highest value in game was 81.
		Else
			Flash17Dir = aLvl/255*3		'flasher goes down, set new target level
		end if
	Else
		Flash17Dir = 0					'Let flasher go off
	end if
	Flash17Last = alvl
	Flasher17_timer
end sub


sub Flasher17_timer()
    If not Flasher17.TimerEnabled Then Flasher17.TimerEnabled = True

    'Flasher17.opacity 				= 110 * FlashLevel17^5   
    f17.IntensityScale 				= 1 * FlashLevel17^3
    f17b.IntensityScale 			= 1 * FlashLevel17^3
	amy_priv.blenddisablelighting 	= 0.2 * FlashLevel17^2

	if Flash17Dir <> 2 then			'flasher should fade
		FlashLevel17 = FlashLevel17 * 0.95 - 0.01
	end If

    If FlashLevel17 <= Flash17Dir Then 'flashed has turned off or flasher has reached the new level
        Flasher17.TimerEnabled = False
    End If

	'debug.print FlashLevel17

end Sub

	'nModLightm 117, f17b,	1,	25
	'FadeDisableLightingM 117, amy_priv, 0.3
	'nModLight 117, f17	,	0,	25, 1	'amy

'Pop up
Sub SolPopUp(Enabled)
	if bsAmyVuk.hasball then bip = bip + 1
	If Enabled Then
		if bsamyVuk.hasball then
			bsAmyVuk.SolOutAlt 0
			bsAmyVuk.ExitSol_On
			
			SoundSaucerKick 1, KickerUFTEST
			'playsoundAt SoundFX("fx_kicker2",DOFContactors), KickerUFTEST
		Else
			SoundSaucerKick 0, KickerUFTEST
			'playsoundAt SoundFX("fx_solenoidOn",DOFContactors), KickerUFTEST
		end If
	Else
'		bsAmyVuk.SolOutAlt 1	'Down
		
		'playsoundAt SoundFX("fx_solenoidOff",0), KickerUFTEST
	End If
End Sub
'		.InitSounds "scoop_enter", "fx_Solenoidon", SoundFX("fx_kicker2",DOFContactors)

'   (Public) .solOut           - Fire the primary exit kicker.  Ejects ball if one is present.
'   (Public) .solOutAlt        - Fire the secondary exit kicker.  Ejects ball with alternate forces if present.
'Pop down
Sub SolPopDown(Enabled)
	if bsAmyVuk.hasball then bip = bip + 1
	If Enabled Then
		if bsamyVuk.hasball then
			bsAmyVuk.SolOutAlt 1'down
			bsAmyVuk.ExitSol_On

			SoundSaucerKick 1, KickerUFTEST
			'playsoundAt SoundFX("fx_kicker2",DOFContactors), KickerUFTEST
		Else
			
			SoundSaucerKick 0, KickerUFTEST
			'playsoundAt SoundFX("fx_solenoidOn",DOFContactors), KickerUFTEST
		end If
	Else
		SoundSaucerKick 0, KickerUFTEST
		'playsoundAt SoundFX("fx_solenoidOff",0), KickerUFTEST
	End If
End Sub

'sw53 (2 way popper)
sub sw53_hit
controller.Switch(53) = 1
bsAmyVuk.addball me
bip = bip - 1
SoundSaucerLock
'PlaySoundAt "kicker_hit", ActiveBall
End Sub	'clark



Sub VolcanoKickOut(enabled)
	If Enabled Then
		if bsVolcano.balls then
			bsVolcano.ExitSol_On
			
			SoundSaucerKick 1, KickerUFTEST2
			'playsoundat SoundFX("kicker_release",DOFcontactors), KickerUFTEST2
		Else
			SoundSaucerKick 0, KickerUFTEST2
			'playsoundat SoundFX("fx_solenoidOnVar",DOFContactors), KickerUFTEST2
		end if
	Else
		SoundSaucerKick 0, KickerUFTEST2
		'playsoundat SoundFX("fx_solenoidOffVar",0), KickerUFTEST2
	end if
End Sub

Sub Sw36_Hit:vpmTimer.PulseSw(36):bsVolcano.AddBall me:bip = bip - 1:SoundSaucerLock:End Sub	'Clark

Sub SolRelease(Enabled)	'ball tracking
    If Enabled Then
		if bsTrough.Balls > 0 then
			RandomSoundBallRelease BallRelease
			'Playsoundat SoundFX("BallRelease",DOFcontactors), BallRelease	'ClarkKent
			vpmTimer.PulseSw 31
			bsTrough.ExitSol_On
			bip = bip + 1
		Else
			'playsoundat SoundFX("fx_solenoidOnVar",DOFContactors), BallRelease
		end if
	Else
		'playsoundat SoundFX("fx_solenoidOffVar",0), BallRelease
    End If
End Sub

' Drain hole
Sub Drain_Hit
	RandomSoundDrain(Drain)
	bsTrough.AddBall Me
	bip = bip - 1
	if bip < 0 then bip = 0
End Sub





Sub MapKick(enabled)
	if bsMap.hasball then bip = bip + 1
	If Enabled Then
		if bsMap.hasball then
			SoundSaucerKick 1, sw38
			'playsoundat SoundFX("fx_kicker2",DOFContactors), sw38
			bsmap.ExitSol_On
		Else
			SoundSaucerKick 0, sw38
			'playsoundat SoundFX("fx_solenoidOn",DOFContactors), sw38
		end If
	Else
		SoundSaucerKick 0, sw38
		'playsoundat SoundFX("fx_solenoidOff",0), sw38
	End If
end sub
Sub Sw38_Hit:controller.Switch(38) = 1:bsMap.addball me:bip = bip - 1:SoundSaucerLock:End Sub	'clark

Sub MysteryKick(enabled)
	if bsMystery.hasball then bip = bip + 1 end if
	If Enabled Then
		if bsMystery.hasball then
			SoundSaucerKick 1, sw37
			'playsoundat SoundFX("fx_kicker2",DOFContactors), sw37
			bsmystery.ExitSol_On
		Else
			SoundSaucerKick 0, sw37
			'playsoundat SoundFX("fx_solenoidOn",DOFContactors), sw37
		end If
	Else
		SoundSaucerKick 0, sw37
		'playsoundat SoundFX("fx_solenoidOff",0), sw37
	End If
end sub

Sub Sw37_Hit:controller.Switch(37) = 1:bsMystery.addball me:bip = bip - 1:SoundSaucerLock:End Sub	'clark
'
DiverterSwoop.isdropped = 1
Sub RampDiverter(enabled)
	if Enabled Then
		SoundSaucerKick 1, Rubber_Ob15
		'playsoundat SoundFX("fx_solenoidon",DOFcontactors), Rubber_Ob15
		Diverter.rotatetoend
		DiverterSwoop.isdropped = 0
	else
		SoundSaucerKick 0, Rubber_Ob15
		'playsoundat SoundFX("fx_solenoidoff",DOFcontactors), Rubber_Ob15
		Diverter.rotatetostart
		DiverterSwoop.isdropped = 1
	End If
End Sub


'Lampstate = 1 or 0, on or off
'flashlevel = fading step
'SolModValue = input 0-255
'flashmax, FlashMin
dim SolModValue(200)
dim LightFallOff(200, 4)	'2d array to hold alt falloff values in different columns
dim FlashersOpacity(200)
dim FlashersFalloff(200)	'??? (could use multiply? or some other kind of mixing?...)
dim GIscale(200)


Sub SetModLamp(nr, value)
    If value <> SolModValue(nr) Then
		SolModValue(nr) = value

		if value > 0 then LampState(nr) = 1 else LampState(nr) = 0
'        LampState(nr) = abs(cbool(SolModValue) )
        FadingLevel(nr) = LampState(nr) + 4
    End If
End Sub

Sub SetModLampF(nr, value)	'debug
    If value <> SolModValue(nr) Then
		SolModValue(nr) = value

		if value > 0 then LampState(nr) = 1 else LampState(nr) = 0
'        LampState(nr) = abs(cbool(SolModValue) )
        FadingLevel(nr) = LampState(nr) + 4
    End If
End Sub

Sub SetModLampM(nr, nr2, value)	'setlamp NR, but also NR + 50
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

Sub SetModLampMM(nr, nr2, nr3, value)	'setlamp NR, but also NR + 50
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
    If value <> SolModValue(nr3) Then
		SolModValue(nr3) = value
		if value > 0 then LampState(nr3) = 1 else LampState(nr3) = 0
        FadingLevel(nr3) = LampState(nr3) + 4
    End If
End Sub


Sub TB3_timer()	'debug
	me.text = "f20 state:" & f20.state & vbnewline & _
	"solvalue:" & SolModValue(120) & vbnewline & _
	"intens:" & f20.intensity & vbnewline & _
	"intscale:" & f20.intensityscale & vbnewline & _
	"fading:" & FlashLevel(120) & vbnewline & _
	"FallOffconst:" & LightFallOff(120, 0) & vbnewline & _
	"FallOffcurrent:" & f20.falloff & vbnewline & _
	"lampstate:" & LampState(120) & vbnewline & _
	"fadinglvl:" & FadingLevel(120) & vbnewline & vbnewline & _
	"flashlvl:" & FlashLevel(120) & vbnewline & _
	"GIscale" & GIscale(120) & vbnewline & _
	"okay" & vbnewline & _
	"ls18" & LampState(18) & vbnewline & _
	"fl18" & FadingLevel(18) & vbnewline & _
	"st18" & l18.state & vbnewline & _
	"okay"
End Sub



Sub Sol28(value)	'callback method for insert, because it's less timing sensitive
	if Value = 0 Then
		f28.state = 0
		F28B.state = 0
	Else
		f28.state = 1
		F28B.state = 1
	end If
	f28.intensityscale = (value * (1/255)) * giscale(128)
	F28B.intensityscale = (value * (1/255)) * giscale(128)
	f28B.falloff = LightFallOff(128, 0) * ScaleFalloff(value, 75)
End Sub
'128
'129

Sub SetFlashSpeedUp(lwr,uppr,value)		'subs for adjusting flasher speed in the debugger
	dim x
	for x = lwr to uppr	'primarly fading speeds for flashers	'intensityscale per 10MS
		FlashSpeedUp(x) = value
'		FlashSpeedDown(x) = 1
	next
End Sub

Sub SetFlashSpeedDown(lwr,uppr,value)
	dim x
	for x = lwr to uppr	'primarly fading speeds for flashers	'intensityscale per 10MS
'		FlashSpeedUp(x) = 1
		FlashSpeedDown(x) = value
	next
End Sub

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.01 '0.4  ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.008	'0.2 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
	for x = 0 to 200
		SolModValue(x) = 0
		FlashersOpacity(x) = 0
'		FlashersFalloff(x) = 0	'????
		LightFallOff(x, 0) = 0
		LightFallOff(x, 1) = 0
		LightFallOff(x, 2) = 0
		LightFallOff(x, 3) = 0
		Giscale(x) = 1
	next
	for x = 115 to 180
		FlashSpeedUp(x) = 1.1
		FlashSpeedDown(x) = 0.9
	next
	for x = 0 to 10
		FlashSpeedUp(x) = 0.01
		FlashSpeedDown(x) = 0.008
	next

	GI28.state = 1
	GI29.state = 1
	Gi30.state = 1

	'LightFallOff(117, 0) = f17.Falloff	'amy
	'LightFallOff(117, 1) = f17b.Falloff

	LightFallOff(118, 0) = F18.Falloff	'Lramp
	LightFallOff(138, 0) = F18b.Falloff
	FlashSpeedUp(138) = 1.32
	FlashSpeedDown(138) = 0.9

	LightFallOff(119, 0) = F19.Falloff	'Two-way popper
	LightFallOff(119, 1) = F19a.Falloff
	LightFallOff(129, 0) = F19b.Falloff
	FlashSpeedUp(129) = 1.32
	FlashSpeedDown(129) = 0.9

	LightFallOff(120, 1) = F20a.Falloff	'ambient shadowing
	LightFallOff(120, 2) = F20a2.Falloff	'ambient
	FlashSpeedUp(130) = 1.32
	FlashSpeedDown(130) = 0.9

'	LightFallOff(131, 0) = f21B.Falloff	'grey ambient 31
	FlashSpeedUp(131) = 1.32
	FlashSpeedDown(131) = 0.9

	LightFallOff(121, 0) = f21.Falloff	'grey flash

	LightFallOff(125, 1) = f25a.Falloff	'
	LightFallOff(135, 0) = f25b.Falloff	'opposite of the skillshot single flasher
	FlashSpeedUp(135) = 1.32
	FlashSpeedDown(135) = 0.9

	LightFallOff(126, 0) = f26.Falloff	'Right ramp
	LightFallOff(126, 1) = f26a.Falloff	'Right ramp
	LightFallOff(136, 0) = f26b.Falloff	'Right ramp	bulb 136
	FlashSpeedUp(136) = 1.32
	FlashSpeedDown(136) = 0.9

	LightFallOff(137, 0) = f27a.Falloff		'	ambient volcano	137
	FlashSpeedUp(137) = 1.32
	FlashSpeedDown(137) = 0.9

	LightFallOff(127, 0) = f27.Falloff	'volcano '27 and 27b share fading speeds, A and C have their own seperate
	LightFallOff(127, 1) = f27b1.Falloff
	LightFallOff(127, 2) = f27b2.Falloff
	LightFallOff(127, 3) = f27c.Falloff	'wall absorb



'	LightFallOff(128, 0) = F28.Falloff		'insert
'	FlashSpeedUp(128) = 255
'	FlashSpeedDown(128) = 255
	LightFallOff(128, 0) = F28b.Falloff		'128'ambient
'	FlashSpeedUp(129) = 20
'	FlashSpeedDown(129) = 18
	for each x in Fflashers
		x.state = 1
	next
	f28.state = 0
	f28b.state = 0
End Sub


'This timer handles everything. Lamps, Flashers and Gorilla animation. -1 interval, updates every frame.
'NmodLight subs:	'lampnumber, object, falloff column, ScaleType (see Function ScaleLights below)
dim CGT	'compensated game time
Sub GameTimer_Timer()

    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next

    End If

	cgt = gametime - InitFadeTime(0)

    UpdateLamps

	ModGILight 0
	ModGIFlash 0
	nModGI 0, 0.75

	ModGILight 1
	ModGIFlash 1
	nModGI 1, 0.75

	ModGILight 2
	ModGIFlash 2
	nModGI 2, 0.75

	RollingTimer
	BallShadowUpdate
	FlipperTimer

	UpdateGorilla

	Pincab_Shooter.Y = -158.3448 + (5* Plunger.Position) -20

	'nModLightm 117, f17b,	1,	25
	'FadeDisableLightingM 117, amy_priv, 0.3
	'nModLight 117, f17	,	0,	25, 1	'amy

	nModLight 118, F18,		0,	9,	1	'left ramp f
	nModLight 138, F18b,	0,	15,	1	'left ramp f 'bulb 138

	nModLight 119, F19,		0,	9,	1	'2-way popper
	nModLightm 119, f19a,	1,	9
	nModLight 129, F19b,	0,	15,	1	'2-way popper	'bulb 129

	nModLightm 120, f20a2,	2,	9
	nModLight 120, f20a,	1,	9, 1
	'nModLight 120, F20,		0,	9,	1	'skillshot		'4 lights


'	nModLight 131, F21B, 	0,	15,	0.75 'ambient grey LPF Flasher, 131
	nModLight 121, F21,		0,	15,	1	'LPF flasher

	nModLight 125, F25a,	1,	9,1	'Lower Right Flasher
	nModLight 125, F25a2,	1,	9,1	'Lower Right Flasher 2
	nModLight 135, F25b,	0,	15,	1	'Lower Right Flasher	'bulb 135

	nModLightm 126, f26a,	1,	9	'Right Ramp Flasher
	nModLight 126, f26,		0,	9,	1	'Right Ramp Flasher
	nModLight 136, f26b,	0,	9,	1	'Right Ramp Flasher
	nModLight 136, f26b2,	0,	9,	1	'Right Ramp Flasher


	nModLight	137, f27a,	0,	15,	1	'ambient 137
	nModLight	127, F27,	0,	9,	1
	nModLightm	127, F27b1,	1,	9
	nModLightm	127, F27b2,	2,	9
	nModLightm	127, F27c,	3,	15

	InitFadeTime(0) = gametime
End Sub

Function ScaleLights(value, scaletype)	'returns an intensityscale-friendly 0->100% value out of 255
	dim i
	Select Case scaletype	'select case because bad at maths 	'TODO: Simplify these functions. B/c this is absurdly bad.
		case 0
			i = value * (1 / 255)	'0 to 1
		case 6	'0.0625 to 1
			i = (value + 17)/272
		case 9	'0.089 to 1
			i = (value + 25)/280
		case 15
			i = (value / 300) + 0.15
		case 20
			i = (4 * value)/1275 + (1/5)
		case 25
			i = (value + 85) / 340
		case 37	'0.375 to 1
			i = (value+153) / 408
		case 40
			i = (value + 170) / 425
		case 50
			i = (value + 255) / 510	'0.5 to 1
		case 75
			i = (value + 765) / 1020	'0.75 to 1
		case Else
			i = 10
	End Select
	ScaleLights = i
End Function

Function ScaleByte(value, scaletype)	'returns a number between 1 and 255
	dim i
	Select Case scaletype
		case 0
			i = value * 1	'0 to 1
		case 9	'ugh
			i = (5*(200*value + 1887))/1037
		case 15
			i = (16*value)/17 + 15
		case else
			i = (3*(value + 85))/4	'63.75 to 255
	End Select
	ScaleByte = i
End Function

Function ScaleGI(value, scaletype)	'returns an intensityscale-friendly 0->100% value out of 1>8 'it does go to 8
	dim i
	Select Case scaletype	'select case because bad at maths
		case 0
			i = value * (1/8)	'0 to 1
		case 25
			i = (1/28)*(3*value + 4)
		case 50
			i = (value+5)/12
		case else
'			x = (4*value)/3 - 85	'63.75 to 255

	End Select
	ScaleGI = i
End Function


Function ScaleFalloff(value, nr)	'TODO make more options here
	if nr > 128 then 'do not scale special bulb NRs
		ScaleFalloff = 1
	Else
'		ScaleFalloff = (value + 255) / 510	'0.5 to 1
		ScaleFalloff = (value + 765) / 1020	'0.75 to 1
	end if
End Function

dim InitFadeTime(200)


'inputs SolModValue
'Outputs IntensityScale * giscale = FlashLevel
'Outputs falloff = Flashlevel

Sub nModLight(nr, object, offset, scaletype, offscale)	'Fading using intensityscale with modulated callbacks
	dim DesiredFading
	Select Case FadingLevel(nr)
		case 3	'workaround - wait a frame to let M sub finish fading
			FadingLevel(nr) = 0
		Case 4	'off
'			FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)*offscale
			FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * cgt	) * offscale
			If FlashLevel(nr) < 0 then FlashLevel(nr) = 0 : FadingLevel(nr) = 3
			Object.IntensityScale = ScaleLights(FlashLevel(nr),0 ) * GIscale(nr)
			Object.Falloff = LightFallOff(nr, offset) * ScaleFalloff(FlashLevel(nr), nr)
'			InitFadeTime(nr) = gametime
'			tbt.text = (cgt - InitFadeTime(0)	)
		Case 5 ' Fade (Dynamic)
			DesiredFading = ScaleByte(SolModValue(nr), scaletype)

			if FlashLevel(nr) < DesiredFading Then
'				tb5.text = "+"
				'FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
				FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr)	* cgt	)
				If FlashLevel(nr) >= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
			elseif FlashLevel(nr) > DesiredFading Then
'				tb5.text = "-"
'				FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
				FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * cgt	)
				If FlashLevel(nr) <= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
			End If
			Object.Intensityscale = ScaleLights(FlashLevel(nr),0 ) * GIscale(nr)
			Object.Falloff = LightFallOff(nr, offset) * ScaleFalloff(FlashLevel(nr), nr)
'			tbt.text = (cgt - InitFadeTime(0)	)
'			InitFadeTime(nr) = gametime
'			tbt.text = (FlashSpeedDown(nr) * cgt	) & vbnewline & (FlashSpeedup(nr) * cgt	) & "cgt:" & cgt
	End Select
End Sub

Sub nModFlash(nr, object, offset, scaletype, offscale)	'Fading using intensityscale with modulated callbacks	'gametime compensated
	dim DesiredFading
	Select Case FadingLevel(nr)
		case 3
			FadingLevel(nr) = 0
		Case 4	'off
'			FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)*offscale
			FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * cgt	) * offscale
			If FlashLevel(nr) < 0 then FlashLevel(nr) = 0 : FadingLevel(nr) = 3
			Object.IntensityScale = ScaleLights(FlashLevel(nr),0 ) * GIscale(nr)'			Object.Falloff = LightFallOff(nr, offset) * ScaleFalloff(FlashLevel(nr), nr)
'			InitFadeTime(nr) = gametime
'			tbt.text = (cgt - InitFadeTime(0)	)
		Case 5 ' Fade (Dynamic)
			DesiredFading = ScaleByte(SolModValue(nr), scaletype)

			if FlashLevel(nr) < DesiredFading Then
'				tb5.text = "+"
				'FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
				FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr)	* cgt	)
				If FlashLevel(nr) >= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
			elseif FlashLevel(nr) > DesiredFading Then
'				tb5.text = "-"
'				FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
				FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * cgt	)
				If FlashLevel(nr) <= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
			End If
			Object.Intensityscale = ScaleLights(FlashLevel(nr),0 ) * GIscale(nr)
'			Object.Falloff = LightFallOff(nr, offset) * ScaleFalloff(FlashLevel(nr), nr)
'			tbt.text = (cgt - InitFadeTime(0)	)
'			InitFadeTime(nr) = gametime
'			tbt.text = (FlashSpeedDown(nr) * cgt	) & vbnewline & (FlashSpeedup(nr) * cgt	) & "cgt:" & cgt
'			tbt.text = DesiredFading
	End Select
End Sub

Sub nModLightM(nr, Object, offset, scaletype)	'uses offset to store different falloff values in a unused lamp number. default 0
	Select Case FadingLevel(nr)
		Case 3, 4, 5
			Object.Intensityscale = ScaleLights(FlashLevel(nr),0 ) * GIscale(nr)
'			Object.IntensityScale = ScaleLights(FlashLevel(nr),scaletype ) * GIscale(nr)
			Object.Falloff = LightFallOff(nr, offset) * ScaleFalloff(FlashLevel(nr), nr)
	End Select
End Sub

Sub nModFlashM(nr, Object, offset, scaletype)
	Select Case FadingLevel(nr)
		Case 3, 4, 5
			Object.Intensityscale = ScaleLights(FlashLevel(nr),0 ) * GIscale(nr)
	End Select
End Sub




'






Sub nModGI(nr, offscale)
	dim DesiredFading
	Select Case FadingLevel(nr)
		case 3
			FadingLevel(nr) = 0
		Case 4	'off
			FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * cgt	) * offscale
			If FlashLevel(nr) < 0 then FlashLevel(nr) = 0 : FadingLevel(nr) = 3
		Case 5 ' Fade (Dynamic)
			DesiredFading = SolModValue(nr)	'for gi, it's called with scaled value
			if FlashLevel(nr) < DesiredFading Then
'				tb5.text = "+"
				'FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
				FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr)	* cgt	)
				If FlashLevel(nr) >= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
			elseif FlashLevel(nr) > DesiredFading Then
'				tb5.text = "-"
'				FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
				FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * cgt	)
				If FlashLevel(nr) <= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
			End If
	End Select'	tbs1.text = nr & vbnewline & " modvalue:" & SolModValue(nr) & " " & vbnewline & FadingLevel(nr)
End Sub

Sub ModGILight(nr)
'	tb.text = " !!!!"
	Select Case FadingLevel(nr)
		Case 3, 4, 5
			dim x
			if nr = 2 then 'gi bottom	'scaling y = (x + 1)/2 and x!=1		'nah y = 1/4 (3 x + 1) and x!=1
				'nada
			elseif nr = 1 then
				GI28.IntensityScale = 1/4 * (3*FlashLevel(nr) + 1)
				GI29.IntensityScale = 1/4 * (3*FlashLevel(nr) + 1)
				Gi30.IntensityScale = 1/4 * (3*FlashLevel(nr) + 1)
			elseif nr = 0 Then 'gorilla
			End If
	end select
End Sub


Sub ModGIflash(nr)
	Select Case FadingLevel(nr)
		Case 3, 4, 5
			if nr = 2 then 'gi bottom
				GIflare2.IntensityScale = FlashLevel(nr)
			elseif nr = 1 then
				Gi_flasher.IntensityScale = FlashLevel(nr)	'spotlight
				GIflare3.IntensityScale = FlashLevel(nr)
			elseif nr = 0 Then 'gorilla
				Gi_Gorilla.Intensityscale = FlashLevel(nr)
				giflare4.IntensityScale = FlashLevel(nr)
				giflare5.IntensityScale = FlashLevel(nr)
				gorilla.blenddisablelighting 	= 0.035 * FlashLevel(nr)
			End If
	end select
End Sub

Function FunctEvenOut(value, divis)	'IN: 0-255, 'divis', OUT:A number divisible by 'divis' value 'unused
	FunctEvenOut = Round((value+1)/divis)*divis
End Function


sub tBIP_timer()
	me.text = BIP
end sub

Sub RightGateOn(Enabled)
	If Enabled Then
	gate4.open = True
	Else
	gate4.open = False
	End If
End Sub

Sub LeftGateOn(Enabled)
	If Enabled Then
	gate2.open = True
	Else
	gate2.open = False
	End If
End Sub

Sub Auto_Plunger(Enabled)
    If Enabled Then
		AutoPlunger.Fire
		'Not sure if its the right sound?
		RandomSoundBallRelease AutoPlunger
		'playsoundAt SoundFX("Kicker_release",DOFContactors), AutoPlunger
		if BallInPlunger then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
'		if BallInPlunger then
'			PlaySoundAt SoundFX("plunger3",0),AutoPlunger
'		Else
'			PlaySoundAt SoundFX("plunger",0),AutoPlunger
'		end if
    else
       AutoPlunger.Pullback
	End If
End Sub

Sub Kick_Back(Enabled)
    If Enabled Then
       KickBack.Fire
		RandomSoundBallRelease Kickback
       'PlaySoundAt SoundFX("fx_solenoid",DOFContactors), Kickback
    else
       KickBack.Pullback
	End If
End Sub


' Left Post
Sub SolLeftpost(Enabled)
	If Enabled Then
		LeftPost.IsDropped = 0
		LeftPost_invis.IsDropped = 0
		playsound "buzz", -1, 0.002, -0.05	'borgdog
		playsound SoundFX("LockupPin",DOFcontactors), 0, 1, -0.02	'clark
	Else
		LeftPost.IsDropped = 1
		LeftPost_invis.IsDropped = 1
		stopsound "buzz"
		playsound SoundFx("fx_Solenoidoff",0), 0, 0.05, -0.02
	End If
End Sub

Sub AmyRampTrigger_Hit()
	'playsound "drop_mono", 0, 0.2, -0.1
End Sub

' Top Post - Lock
Sub SolTopPost(Enabled)
	If Enabled Then
		TopPost.IsDropped = 1
		'Debug.Print "TopPost Dropped = 1"
		playsound SoundFX("LockupPin",DOFcontactors), 0, 0.2, 0.02	'clark
	Else
		TopPost.IsDropped = 0
		'Debug.Print "TopPost Dropped = 0"
		playsound SoundFx("fx_Solenoidoff",0), 0, 0.01, 0.02
	End If
End Sub

Sub Updategtb
	gtb.text = "GorDest:" & GorDest & vbnewline & _
	"GorStep:" & GorDirection & vbnewline & _
	"Release?: " & GorRelease & vbnewline & _
	"GorAngle" & GorAngle & vbnewline & _
	"blah" & GorVel1 & vbnewline & _
	".."
End Sub

'Grey Gorilla Scripting
'-===================
'RotY, positve values rotate clockwise
'sub GorillaTimerShutoff_Timer()
'	gtb.text = "...shutoff... "
'	GorillaTimer.enabled = 0
'	me.enabled = 0
'end sub
Dim GorAngle, GorDest : GorAngle = gorilla.RotY
Dim GorVel1, GorVel2
GorVel1 = 0.75	'Solenoid powered
GorVel2 = 0.1	'unpowered (bounce back)
Dim GorRelease : GorRelease = 0	'Finds dead solenoid state for bounce-back animation
Dim GorDirection 'Timer Step

Sub GorillaRight(Enabled)	'...Left
	If Enabled Then
		PlaySound SoundFx("fx_Solenoidon",DOFContactors), 0, 0.1, 0.01
		GorDest = -10
		GorDirection = 4
		GorRelease = 0
		GoFlipperRight.Startangle = 76
		GoFlipperLeft.RotateToEnd
		GoFlipperRight.RotateToStart
		GoFlipperLeft.startangle = -100
	Else
		PlaySound SoundFx("fx_Solenoidoff",0), 0, 0.02, 0.01
		GoFlipperLeft.RotateToStart
		if GorDirection = 0 then GorDirection = 10
		GorRelease = 1
	End If
'	Updategtb
End Sub

Sub GorillaLeft(Enabled)	'...Right
	If Enabled Then
		PlaySound SoundFx("fx_Solenoidon",DOFContactors), 0, 0.1, -0.01
		GorDest = 10
		GorDirection = 5
		GorRelease = 0
		GoFlipperRight.RotateToEnd
		GoFlipperLeft.Startangle = -76
		GoFlipperLeft.RotateToStart
		GoFlipperRight.startangle = 100
	Else
		PlaySound SoundFx("fx_Solenoidoff",0), 0, 0.02, -0.01
		GoFlipperRight.RotateToStart
		if GorDirection = 0 then GorDirection = 10
		GorRelease = 2
	End If
'	Updategtb
End Sub



Sub UpdateGorilla
	Select Case GorDirection
		Case 2
			if GorRelease > 0 then
				Select Case GorRelease
					Case 1 	'Left return, settle clockwise
						gordest = -6
						If GorDest > GorAngle then
							GorAngle = GorAngle + (GorVel2 * cgt)
						ElseIf GorDest < GorAngle Then
							GorAngle = GorDest
							GorDirection = 0	'done
						End If
						Gorilla.RotY = GorAngle
					Case 2 'right return, settle counter-clockwise
						gordest = 6
						If GorDest < GorAngle then
							GorAngle = GorAngle - (GorVel2 * cgt)
						ElseIf GorDest > GorAngle Then
							GorAngle = GorDest
							GorDirection = 0	'done
						End If
						Gorilla.RotY = GorAngle
				End Select
			end if
		Case 4	'Kick left flipper, rotate counter-clockwise 			'GorDest = -10
			If GorAngle > GorDest then
				GorAngle = GorAngle - (GorVel1 * cgt)
			Else
				GorAngle = GorDest
				GorDirection = 0
			End If
			Gorilla.RotY = GorAngle
		Case 5	'kick right flipper, rotate clockwise				'		GorDest = 10
			If GorDest > GorAngle then
				GorAngle = GorAngle + (GorVel1 * cgt)
			Else
				GorAngle = GorDest
				GorDirection = 0
			End If
			Gorilla.RotY = GorAngle

		Case 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 : GorDirection = gordirection + 1 'after solenoid fires, wait. (lag compensation) (solenoid bounce-back animation)
		case 20 : gordirection = 2
	End Select
'	Updategtb
End Sub





'******************************************************
'					FLIPPERS
'******************************************************

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
	
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
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

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub


Sub LeftFlipper_Collide(parm)
	LeftFlipperCollide parm
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	if RubberizerEnabled <> 0 then Rubberizer(parm)
End Sub

Sub RightFlipper_Collide(parm)
	RightFlipperCollide parm
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	if RubberizerEnabled <> 0 then Rubberizer(parm)
End Sub

Sub SolULFlipper(Enabled)
	If Enabled Then
'		PlaySound "fx_flipperup", 0, 0.5, -0.06, 0.15
		ULeftFlipper.RotateToEnd
	Else
'		PlaySound "fx_flipperdown", 0, 0.5, -0.06, 0.15
		ULeftFlipper.RotateToStart
	End If
End Sub

sub Rubberizer(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * 2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * 0.5
		activeball.vely = activeball.vely * 1.4
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub



'================VP10 Fading Lamps Script

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()


Sub UpdateLamps
	Flashc 11, congoC
	Flashc 12, congoO
	Flashc 13, congoN
	Flashc 14, congoG
	Flashc 15, congoO2

	NFadeLwF2 16, L16, l16a, L16F	'A	'was bugged, all inserts need to be in Linserts for the GI scaling thing
	NFadeLwF2 17, L17, L17a, L17F	'M
	NFadeLwF2 18, L18, L18a, L18F	'Y

	NFadeL 21, L21
	NFadeL 22, L22
	NFadeLwF 23, l23, l23f
	NFadeL 24, L24
	NFadeL 25, L25
	NFadeL 26, L26
	NFadeL 27, L27
	NFadeLwF 28, L28, l28f

	NFadeLwf 31, L31, l31f
	NFadeLm 32, l32l	'ambient near bumpers
	NFadeL 32, L32
	NFadeLm 33, L33L	'ambient near bumpers
	NFadeL 33, L33


	NFadeL 34, L34
	NFadeL 35, L35
	NFadeL 36, L36
	NFadeL 37, L37
	NFadeLm 38, L38
	NFadeL 38, L38L		'ambient near bumpers

	NFadeL 41, L41
	NFadeL 42, L42

	NFadeLwF 43, L43, L43f
	NFadeLwF 44, L44, L44f

	NFadeLwF 45, L45, l45f
	NFadeL 46, L46
	NFadeLwF 47, L47, l47f
	NFadeLwF 48, L48, l48f

	NFadeL 51, L51
	NFadeL 52, L52
	NFadeL 53, L53
	NFadeL 54, L54
	NFadeL 55, L55
	NFadeLwF 56, L56, L56F
	NFadeL 57, L57
	NFadeLwF 58, L58, L58F

	NFadeLwf 61, L61, l61f
	NFadeL 62, L62
	NFadeL 63, L63
	NFadeL 64, L64
	NFadeL 65, L65
	NFadeL 66, L66
	NFadeL 67, L67
	NFadeLwF 68, L68, l68f

	NFadeLwF 71, L71, l71f
	NFadeLwF 72, L72, l72f
	NFadeLwF 73, L73, l73f
	NFadeLwF 74, L74, l74f
	NFadeL 75, L75
	NFadeL 76, L76
	NFadeL 77, L77
	NFadeLm 78, L78
	NFadeL 78, L78_2

	NFadeLm 81, L81
	NFadeL 81, L81l	'H	'ambient near bumpers, this is a hippo target
	'NFadeL 82, L82
	NFadeLwF 82, L82, L82f
	NFadeLwF 83, L83, L83f
	NFadeLwF 84, L84, L84f
	NFadeLwF 85, L85, L85f
	NFadeL 86, L86	'Shoot Again

End Sub



Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

'Walls

' Lights: used for VP10 standard lights, the fading is handled by VP itself

Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

'Light with flasher, own fading speeds and min/max (1.1c - simplifed)	'Uses CGT
Sub NFadeLwF(nr, object1, object2)
    Select Case FadingLevel(nr)
		Case 4
			object1.state = 0
			FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * CGT)
			if FlashLevel(nr) < 0 then FlashLevel(nr) = 0 : FadingLevel(nr) = 0
			object2.IntensityScale = FlashLevel(nr)
		Case 5
			object1.state = 1
			FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedDown(nr) * CGT)
			if FlashLevel(nr) > 1 then FlashLevel(nr) = 1 : FadingLevel(nr) = 1
			object2.IntensityScale = FlashLevel(nr)
	End Select
End Sub

Sub NFadeLwF2(nr, object1, object2, object3) 'two lamps with two flashers	'uses CGT
    Select Case FadingLevel(nr)
		Case 4
			object1.state = 0
			object2.state = 0
			FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * CGT)
			if FlashLevel(nr) < 0 then FlashLevel(nr) = 0 : FadingLevel(nr) = 0
			object3.IntensityScale = FlashLevel(nr)
		Case 5
			object1.state = 1
			object2.state = 1
			FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedDown(nr) * CGT)
			if FlashLevel(nr) > 1 then FlashLevel(nr) = 1 : FadingLevel(nr) = 1
			object3.IntensityScale = FlashLevel(nr)
	End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

Sub NFadeLmB(nr, object) ' used for multiple lights, Blinks
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 2
		case 6:object.state = 1			'lil extra before fading
    End Select
End Sub

Sub NFadeLmf(nr, object) ' used for multiple lights working off FadeObj or FadePrim
    Select Case FadingLevel(nr)
        Case 1:object.state = 1
		Case 7:object.state = 0
    End Select
End Sub

Sub NFadeLF(nr, object) ' used for multiple lights working off FadeObj or FadePrim
    Select Case FadingLevel(nr)
        Case 1:object.state = 1
		Case 7:object.state = 0
    End Select
End Sub

Sub Flashc(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * CGT)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * CGT)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
	Select Case FadingLevel(nr)
		case 3,4,5
			Object.IntensityScale = FlashLevel(nr)
	End Select
End Sub


' MAY NEED TO COMMENT OUT? -------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub AmyDownRampdrop_Hit():PlaySoundAt "drop_mono", ActiveBall:End Sub 'name,loopcount,volume,pan,randompitch
Sub WireRampTrigger1_Hit
	isBallOnWireRamp = True
	'PlaySoundAt "wireramp1", ActiveBall
end sub

Sub WireRampTriggerend1_Hit()
	isBallOnWireRamp = false
	'stopsound "wireramp1"
end sub

Sub WireRampTrigger2_Hit
	isBallOnWireRamp = True
	'PlaySoundAt "wireramp1", ActiveBall
end sub

Sub WireRampTriggerend2_Hit()
	isBallOnWireRamp = false
	'stopsound "wireramp1"
end sub


' MAY NEED TO COMMENT OUT? -------------------------------------------------------------------------------------------------------------------------------------------------------------


'Sub AmyDownRampTrigger_Hit()
'	AmyDownRampDrop.Enabled=0
'	me.timerenabled=1
'End Sub
'
'Sub AmyDownRampTrigger_Timer()
'	AmyDownRampDrop.Enabled=1
'	me.Timerenabled=0
'End Sub

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub RightRampdrop_Hit():PlaySoundAt "drop_mono", ActiveBall:End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

'Sub RightRampTrigger_Hit()
'	RightRampDrop.Enabled=0
'	me.timerenabled=1
'End Sub
'
'Sub RightRampTrigger_Timer()
'	RightRampDrop.Enabled=1
'	me.Timerenabled=0
'End Sub
sub RightRampEnd_Hit(idx)
	'debug.print "ramp end hit: " & idx
end sub


' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub LeftRampdrop_Hit():isBallOnWireRamp = false:playsound "drop_mono", 0, 0.3, -0.03:stopsound "wireramp1":End Sub
'Sub LeftRampdrop_Hit():isBallOnWireRamp = false:End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub RampEntry1_Hit()	'left ramp entry
'	If activeball.vely < -10 then
'		PlaySound "ramp_hit2", 0, Vol(ActiveBall)/5, Pan(ActiveBall), 0, Pitch(ActiveBall)*10, 1, 0, AudioFade(ActiveBall)
'	Elseif activeball.vely > 3 then
'		'PlaySound "PlayfieldHit", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'	End If
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub RampEntry2_Hit()	'right ramp entry
' 	If activeball.vely < -10 then
'		PlaySound "ramp_hit2", 0, Vol(ActiveBall)/5, Pan(ActiveBall), 0, Pitch(ActiveBall)*10, 1, 0, AudioFade(ActiveBall)
'	Elseif activeball.vely > 3 then
'		PlaySound "PlayfieldHit", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'	End If
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub Diverter_Hit()
'	playsound "metalhit_medium", 0, Vol(ActiveBall)/5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

'Collection Sounds

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub RampSounds_Hit (idx)
'	PlaySound "ramp_hit1", 0, Vol(ActiveBall)/2, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub Pins_Hit (idx)
'	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub Targets_Hit (idx)
'	PlaySound SoundFX("target",DOFTargets), 0, 0.1, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
'	PlaySound SoundFX("targethit",0), 0, Vol(ActiveBall)*9, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub ApronWalls_Hit (idx)
	'PlaySound "woodhitaluminium", 0, (Vol(ActiveBall)^2.5)*10, Pan(ActiveBall)/4, 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub Metals_Thin_Hit (idx)
'	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub Metals_Medium_Hit (idx)
'	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub MetalWalls_Hit (idx)
'	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub Gates_Hit (idx)
'	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub Rubbers_O_Hit (idx)
'	PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub Rubbers_U_Hit (idx)
'	PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub Rubbers_Hit(idx)
' 	dim finalspeed
'  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
' 	If finalspeed > 20 then
		'PlaySound "fx_rubber2", 0, Vol(ActiveBall)*10, Pan(ActiveBall)*3, 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	'End if
	'If finalspeed >= 6 AND finalspeed <= 20 then
 	'	RandomSoundRubber()
 	'End If
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub Posts_Hit(idx)
 '	dim finalspeed
 ' 	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
' 	If finalspeed > 16 then
'		PlaySound "fx_rubber2", 0, Vol(ActiveBall)* 10, Pan(ActiveBall) * 3, 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'	End if
'	If finalspeed >= 6 AND finalspeed <= 16 then
' 		RandomSoundRubber()
' 	End If
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub RandomSoundRubber()
'	Select Case Int(Rnd*3)+1
'		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*5, Pan(ActiveBall)*3, 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*5, Pan(ActiveBall)*3, 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*5, Pan(ActiveBall)*3, 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'	End Select
'End Sub


'Flipper Sounds

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub ULeftFlipper_Collide(parm)
 '	RandomSoundFlipper()
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub LeftFlipper_Collide(parm)
' 	RandomSoundFlipper()
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub RightFlipper_Collide(parm)
' 	RandomSoundFlipper()
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub RandomSoundFlipper()
'	Select Case Int(Rnd*3)+1
'		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*5, Pan(ActiveBall)*6, 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*5, Pan(ActiveBall)*6, 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*5, Pan(ActiveBall)*6, 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'	End Select
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

'*DOF method for rom controller tables by Arngrim********************
'*******Use DOF 1**, 1 to activate a ledwiz output*******************
'*******Use DOF 1**, 0 to deactivate a ledwiz output*****************
'*******Use DOF 1**, 2 to pulse a ledwiz output**********************
Sub DOF(dofevent, dofstate)
	If cController>2 Then
		If dofstate = 2 Then
			Controller.B2SSetData dofevent, 1:Controller.B2SSetData dofevent, 0
		Else
			Controller.B2SSetData dofevent, dofstate
		End If
	End If
End Sub
'********************************************************************

Sub Table1_exit()
	Controller.Pause = False
	Controller.Stop
End Sub

'--Experimental simple light tweaking UI

dim teststatement: teststatement = "buddy! Press enter to start!" '============CLICK ME TO COLLAPSE============
	ltbody.text = " "
	ltTop.text = "Top bar"
	ltSide.text = "0" & vbnewline & "1" & vbnewline & "2" & vbnewline & "3"' & vbnewline & "4" & vbnewline & "5"
	dim testarray(6)
	testarray(0) = "Hi there " & teststatement
	ltbody.text = testarray(0)

	dim LTtypeselect:LTtypeselect = 0
	dim LTpropertyselect: LTpropertyselect = 0

	'InitLT
	'Sub InitLT
	'	LtTypeSelect = 0
	'End Sub

	sub ltdebug2_timer()
		me.text = "Ltpropertyselect var: " & LTpropertyselect & vbnewline &  "Lttypeselect var: " & LTTypeselect
	end sub

	sub LTcontUpDown(updown)
		dim x
		if Updown = 1 Then
			if LtPropertySelect = 0 then
				LtPropertyselect = 8
			else LtpropertySelect = Ltpropertyselect -1
			end if
		elseif Updown = 0 Then
			if LtPropertySelect = 8 then
				LtPropertyselect = 0
			else LtpropertySelect = Ltpropertyselect +1
			end if
		Else
			Ltdebug.text = "Bad argument"
		end if
		dim a, aa
		aa = array("0 ", "1 ", "2 ", "3 ", "4 ", "5 ", "6 ", "7 ", "8")
		a = array("0>", "1>", "2>", "3>", "4>", "5>", "6>", "7>", "8>")
		aa(LtPropertySelect) = a(LTpropertyselect) 'heh
		ltside.text = aa(0) & vbnewline & aa(1) & vbnewline & aa(2) & vbnewline & aa(3) & vbnewline & aa(4) & vbnewline & aa(5) & vbnewline & aa(6) & vbnewline & aa(7) & vbnewline & aa(8)
	End Sub

	Sub LtcontLeftRight(LeftRight)	'1 or -1
		dim a
		a = array(Linserts)
		dim x
		if mid(Lcatalogn(LTtypeselect), 1, 1) = "L" then
			select case	LTpropertyselect
				case 0 'name
					LtTypeSelect = LtTypeSelect + 1 * leftright
					if LtTypeSelect < 0 then LtTypeSelect = lrarraycount
					if LtTypeSelect > lrarraycount then LtTypeSelect = 0
				case 1	'falloff
					for each x in Lcatalog(LTtypeselect)
						x.falloff = x.falloff + 10 *leftright
					next
				case 2	'fallofpower
					for each x in Lcatalog(LTtypeselect)
						x.falloffpower = x.falloffpower + 0.5 *leftright
					next
				case 3	'intensity
					for each x in Lcatalog(LTtypeselect)
						x.intensity = x.intensity + 1 *leftright
					next
				case 4 'empty
			end select
		Elseif mid(Lcatalogn(LTtypeselect), 1, 1) = "F" then
		'1 object '2 opacity '3 modulate
			select case LTpropertyselect
				case 0	'name
					LtTypeSelect = LtTypeSelect + 1 * leftright
					if LtTypeSelect < 0 then LtTypeSelect = lrarraycount
					if LtTypeSelect > lrarraycount then LtTypeSelect = 0
				case 1	'opacity
					for each x in Lcatalog(LTtypeselect)
						x.opacity = x.opacity + 50 *leftright
					next
				case 2	'modulatevsadd
					for each x in Lcatalog(LTtypeselect)
						x.modulatevsadd = x.modulatevsadd + 0.01 *leftright
					next
				case 3	'modulatevsadd gross
					for each x in Lcatalog(LTtypeselect)
						x.modulatevsadd = x.modulatevsadd + 0.1 *leftright
					next
			end select
		Elseif mid(Lcatalogn(LTtypeselect), 1, 1) = "B" then
			select case LTpropertyselect
				case 0 'name
					LtTypeSelect = LtTypeSelect + 1 * leftright
					if LtTypeSelect < 0 then LtTypeSelect = lrarraycount
					if LtTypeSelect > lrarraycount then LtTypeSelect = 0
				case 1	'falloff
					for each x in Lcatalog(LTtypeselect)
						x.falloff = x.falloff + 10 *leftright
					next
				case 2	'fallofpower
					for each x in Lcatalog(LTtypeselect)
						x.falloffpower = x.falloffpower + 0.5 *leftright
					next
				case 3	'intensity
					for each x in Lcatalog(LTtypeselect)
						x.intensity = x.intensity + 0.5 *leftright
					next
				case 4 'transmit
					for each x in Lcatalog(LTtypeselect)
						x.TransmissionScale = x.TransmissionScale + 0.1 *LeftRight
					next
				case 5 'lightscale for everything else
					for each x in a(0)
						x.intensityscale = x.intensityscale + 0.1 *leftright
					next
			end select
		Elseif mid(Lcatalogn(LTtypeselect), 1, 1) = "G" then
			select case LTpropertyselect
				case 0 'name
					LtTypeSelect = LtTypeSelect + 1 * leftright
					if LtTypeSelect < 0 then LtTypeSelect = lrarraycount
					if LtTypeSelect > lrarraycount then LtTypeSelect = 0
				case 1	'falloff	'1 falloff '2 falloff power '3 intensity '4 transmit '5 GI level '6 GI on/off '7 all lights scale
					for each x in Lcatalog(LTtypeselect)
						x.falloff = x.falloff + 10 *leftright
					next
				case 2	'fallofpower
					for each x in Lcatalog(LTtypeselect)
						x.falloffpower = x.falloffpower + 0.5 *leftright
					next
				case 3	'intensity
					for each x in Lcatalog(LTtypeselect)
						x.intensity = x.intensity + 0.5 *leftright
					next
				case 4 'transmit
					for each x in Lcatalog(LTtypeselect)
						x.TransmissionScale = x.TransmissionScale + 0.1 *LeftRight
					next
				case 5 'GI level
					for each x in Lcatalog(LTtypeselect)
						x.intensityscale = x.intensityscale + 0.1 *leftright
					next
				case 6 'GI state
					for each x in Lcatalog(LTtypeselect)
						if x.state = 0 then x.state = 1 else x.state = 0
					next
				case 7 'lightscale for everything else
					for each x in a(0)
						x.intensityscale = x.intensityscale + 0.1 *leftright
					next
				case 8 'modulate
					for each x in Lcatalog(LTtypeselect)
						x.BulbModulateVsAdd = x.BulbModulateVsAdd + 0.01 *leftright
					next
			end select
		end if
		updateLT
		ltdebug.text = "lr:" & ltpropertyselect
	end sub


	sub updateLT
		dim x, s, xx
	'	for each x in Lcatalog(LTtypeselect)
		for each x in Lcatalog(LTtypeselect)
			if mid(Lcatalogn(LTtypeselect), 1, 1) = "L" Then	'if array starts with "L"
				xx = mid(Lcatalogn(LTtypeselect), 1, 1)
				LTdebug.text = "lamp col."
				LTdisplayLampCats
			elseif mid(Lcatalogn(LTtypeselect), 1, 1) = "F" Then
				ltDebug.text = "flash col."
				LTdisplayFlashCats
			elseif mid(Lcatalogn(LTtypeselect), 1, 1) = "B" Then
				ltDebug.text = "bulb col."
				LTdisplayBulbLampCats
			elseif mid(Lcatalogn(LTtypeselect), 1, 1) = "G" Then
				ltDebug.text = "bulb col."
				LTdisplayGICats
			Else
				LTdebug.text = "error: unknown col."			'for Flashers collections later
			end if
	'		x.intensity = 555
			exit for
		next
		s = " "
		ltTop.text = s
	'	xx = cStr(Lcatalog(LTtypeselect) )	'convert to string for the TextBox	'can't figure this out :(
	'	xx = Lcatalog(0)	'convert to string for the TextBox
	''	xx = cstr(xx)
	'	xx = (Lcatalog(0).tostring())
		s = LcatalogN(LTtypeselect)	'fuck it
		ltTop.text = s
	end sub


	'categories
	sub LTdisplayLampCats
		dim xxx, LTfalloff1, LTfalloff2, LTintens, LTname 	'Loptions0, Loptions1, Loptions2 (falloff, falloffpower, intensity)
		for each xxx in Lcatalog(LTtypeselect)
			LTname = xxx.name
			LTfalloff1 = xxx.falloff
			LTfalloff2 = xxx.falloffpower
			LTintens = xxx.intensity
			exit for 'only need one
		next
		Ltbody.text = "obj: " & LTname & vbnewline & " Falloff: " & LTfalloff1 & vbnewline & " Falloff Power: " & LTfalloff2 & vbnewline & " Intensity: " & LTintens
	end sub

	sub LTdisplayBulbLampCats
		dim xxx, LTfalloff1, LTfalloff2, LTintens, LTname, LTtransmit, LTintscale 	'Loptions0, Loptions1, Loptions2 (falloff, falloffpower, intensity)
		for each xxx in Lcatalog(LTtypeselect)
			LTname = xxx.name
			LTfalloff1 = xxx.falloff
			LTfalloff2 = xxx.falloffpower
			LTintens = xxx.intensity
			LTtransmit = xxx.TransmissionScale
			exit for 'only need one
		next
		for each xxx in Collection1
			LTintscale = xxx.intensityscale
			exit for
		next
		Ltbody.text = "obj: " & LTname & vbnewline & " Falloff: " & LTfalloff1 & vbnewline & " Falloff Power: " & LTfalloff2 & vbnewline & " Intensity: " & LTintens & vbnewline & " Transmit: " & LTtransmit & vbnewline & " insertscale: " & LTintscale
	end sub

	'name, Opacity, 'ModulateVsAdd
	sub LTdisplayFlashCats	'1 object '2 opacity '3 modulate
		dim xxx, ltOpacity, LtModulate, LTname 	'ModulateVsAdd
		for each xxx in Lcatalog(LTtypeselect)
			LTname = xxx.name
			LtModulate = xxx.ModulateVsAdd
			LtOpacity = xxx.Opacity
	'		LTintens = xxx.intensity
			exit for 'only need one
		next
		Ltbody.text = "an object: " & LTname & vbnewline & " Opacity: " & LtOpacity & vbnewline & " Modulate: " & LtModulate
	end sub


	sub LTdisplayGICats																					'1 falloff '2 falloff power '3 intensity '4 transmit '5 GI level '6 GI on/off '7 all lights scale
		dim xxx, LTfalloff1, LTfalloff2, LTintens, LTname, LTtransmit, LTintscale, LTgi, LTgibool, LtBulbModulateVsAdd 	'Loptions0, Loptions1, Loptions2 (falloff, falloffpower, intensity)
		for each xxx in Lcatalog(LTtypeselect)
			LTname = xxx.name
			LTfalloff1 = xxx.falloff
			LTfalloff2 = xxx.falloffpower
			LTintens = xxx.intensity
			LTtransmit = xxx.TransmissionScale
			Ltgi = xxx.intensityscale
			LtGIbool = xxx.state
			LtBulbModulateVsAdd = xxx.BulbModulateVsAdd
			exit for 'only need one
		next
		for each xxx in Linserts
			LTintscale = xxx.intensityscale
			exit for
		next
		Ltbody.text = "obj: " & LTname & vbnewline & " Falloff: " & LTfalloff1 & vbnewline & " Falloff Power: " & LTfalloff2 & _
		vbnewline & " Intensity: " & LTintens & vbnewline & " Transmit: " & LTtransmit & vbnewline & "Gi lvl:" & LTgi & vbnewline & "Gi on:" & LTGibool & vbnewline & "insertscale: " & LTintscale & vbnewline & "Modulate:" & LtBulbModulateVsAdd
	end sub
	'categories end




	dim Lcatalog, LcatalogN
	Lcatalog = array(Linserts, GiTop, Gigorilla) '9
	'aka fuck it
	LcatalogN = array("Linserts", "GiTop", "Gigorilla")
	dim LRarraycount : lrarraycount = 2
'#end region


sub TestUpperFlipper
	KickerUFTEST.createball
	KickerUFTEST.kick 0, 0

'	KickerUFTEST1.createball
'	KickerUFTEST1.kick 270, 10
	bip = bip + 1

	T1.enabled = 1
	ULeftFlipper.rotatetostart
End Sub
'	T1.interval = 1200
	T1.interval = 780
Sub KickerUFTEST2_Hit
	me.destroyball
	'Debug.Print "KickerUFTEST2 hit"
	'PlaySoundAt "ramp_hit1", ActiveBall:End Sub
'	KickerUFTEST2.enabled = 1
'	polltimer.enabled = 0 : discoflips = 1

	tb1.text = "interval: " & t1.interval
End Sub
sub t1_timer()
	uleftflipper.rotatetoend
	me.enabled = 0
end sub

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
' PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position


' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
'  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
'End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

'Sub PlaySoundAt(soundname, tableobj)
'  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
'End Sub

'Set all as per ball position & speed.

'Sub PlaySoundAtBall(soundname)
 ' PlaySoundAt soundname, ActiveBall
'End Sub

'Set position as table object and Vol manually.

'Sub PlaySoundAtVol(sound, tableobj, Vol)
 ' PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
'End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

'Sub PlaySoundAtBallVol(sound, VolMult)
'  PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
'End Sub

'Set position as bumperX and Vol manually.

'Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
'  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------



' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
' PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
'  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
'End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

'Sub PlaySoundAt(soundname, tableobj)
'  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
'End Sub

'Set all as per ball position & speed.

'Sub PlaySoundAtBall(soundname)
'  PlaySoundAt soundname, ActiveBall
'End Sub

'Set position as table object and Vol manually.

'Sub PlaySoundAtVol(sound, tableobj, Vol)
'  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
'End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

'Sub PlaySoundAtBallVol(sound, VolMult)
'  PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
'End Sub

'Set position as bumperX and Vol manually.

'Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
'  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
'End Sub
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************


' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
'  Dim tmp
'  tmp = tableobj.y * 2 / table1.height-1
'  If tmp > 0 Then
'    AudioFade = Csng(tmp ^10)
'  Else
'    AudioFade = Csng(-((- tmp) ^10) )
'  End If
'End Function

'Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
'  Dim tmp
'  tmp = tableobj.x * 2 / table1.width-1
'  If tmp > 0 Then
'    AudioPan = Csng(tmp ^10)
'  Else
'    AudioPan = Csng(-((- tmp) ^10) )
'  End If
'End Function

'Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
'    Dim tmp
'    tmp = ball.x * 2 / table1.width-1
'    If tmp > 0 Then
'        Pan = Csng(tmp ^10)
'    Else
'        Pan = Csng(-((- tmp) ^10) )
'    End If
'End Function

'Function AudioFade(ball) ' Can this be together with the above function ?
'  Dim tmp
'  tmp = ball.y * 2 / Table1.height-1
'  If tmp > 0 Then
'    AudioFade = Csng(tmp ^10)
'  Else
'    AudioFade = Csng(-((- tmp) ^10) )
'  End If
'End Function

'Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
'  Vol = Csng(BallVel(ball) ^2 / 2000)
'End Function

'Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
'  Pitch = BallVel(ball) * 20
'End Function

'Function BallVel(ball) 'Calculates the ball speed
'  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
'End Function
' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


'*********** BALL SHADOW *********************************
Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6, BallShadow7, BallShadow8, BallShadow9, BallShadow10)

Sub BallShadowUpdate()
	on error resume Next
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
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 5
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 5
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub


'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Const tnob = 10 ' total number of balls
'ReDim rolling(tnob)
'InitRolling

'Sub InitRolling
'    Dim i
'    For i = 0 to tnob
'        rolling(i) = False
'    Next
'End Sub

'Sub RollingTimer_Timer() :RollingUpdate : End Sub

'Sub RollingUpdate()
'    Dim BOT, b
'    BOT = GetBalls

    ' stop the sound of deleted balls
    'For b = UBound(BOT) + 1 to tnob
    '    rolling(b) = False
    '    StopSound("fx_ballrolling" & b)
    'Next

    ' exit the sub if no balls on the table
    'If UBound(BOT) = -1 Then Exit Sub

    ' play the rolling sound for each ball

    'For b = 0 to UBound(BOT)
    '  If BallVel(BOT(b) ) > 1 Then
    '    rolling(b) = True
    '    if BOT(b).z < 30 Then ' Ball on playfield
    '      PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
    '    Else ' Ball on raised ramp
    '      PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, Pan(BOT(b) )*75, 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
    '    End If
    '  Else
    '    If rolling(b) = True Then
    '      StopSound("fx_ballrolling" & b)
    '      rolling(b) = False
    '    End If
    '  End If
   ' Next
'End Sub

'**********************
' Ball Collision Sound
'**********************

' COMMENTED OUT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
'Sub OnBallBallCollision(ball1, ball2, velocity)
'  If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
'    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 200, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
'  Else
'    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 200, Pan(ball1), 0, Pitch(ball1), 0, 0
'  End if
'End Sub

'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
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

	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball :End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'******************************************************
'			FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
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
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)						'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
	 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
	
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'playsound "fx_knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'		FLIPPER POLARITY AND RUBBER DAMPENER
'			SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
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
Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
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
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

' Used for drop targets and flipper tricks
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

'******************************************************
'			FLIPPER TRICKS
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

	If abs(Flipper1.currentangle) < abs(Endangle1) + 3 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
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
					'debug.print "flippernudge!!"
					BOT(b).velx = BOT(b).velx /1.3
					BOT(b).vely = BOT(b).vely - 0.7
				end If
			Next
		End If
	Else 
		If abs(Flipper1.currentangle) > abs(Endangle1) + 30 then EOSNudge1 = 0
	End If
End Sub

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
Const EOSTnew = 0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5
Const LiveCatch = 32
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025


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
	Dir = Flipper.startangle/Abs(Flipper.startangle)	'-1 for Right Flipper

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
	Dim LiveCatchBounce															'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.8 Then						'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)	'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (16 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	End If
End Sub

'*****************************************************************************************************
'*******************************************************************************************************
'END nFOZZY FLIPPERS'

'*****************
' Maths'*****************
Const PI = 3.1415927
Function dSin(degrees)
dsin = sin(degrees * Pi/180)
End Function
Function dCos(degrees)
dcos = cos(degrees * Pi/180)
End Function

'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
'Debug.Print "dPosts hit"
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
'Debug.Print "dSleeves hit"
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

'******************************************************
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

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
DelayedBallDropOnPlayfieldSoundLevel = 1									'volume level; range [0, 1]
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

Dim tablewidth, tableheight : tablewidth = table1.width : tableheight = table1.height

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

Sub ApronWalls_Hit (idx)
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

Sub SaveLUT
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if lutpos = "" then lutpos = 0 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "CongoLUT.txt",True)
	ScoreFile.WriteLine lutpos
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub
Sub LoadLUT
	Dim FileObj, ScoreFile, TextStr
	dim rLine
	 Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "CongoLUT.txt") then
		lutpos=0
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "CongoLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			lutpos=0
			Exit Sub
		End if
		lutpos = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub
Sub Table1_exit()
	SaveLUT
	Controller.Stop
End sub


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'///////////////

'******************************************************
'		BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 10 ' total number of balls
ReDim rolling(tnob)
Dim isBallOnWireRamp
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub RollingTimer()
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("BallRoll_" & b)
		StopSound "fx_plasticrolling" & b
        StopSound "fx_metalrolling" & b
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	'For b = 0 to UBound(BOT)
	'	If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
	'		rolling(b) = True
	'		PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
	'		StopSound "fx_plasticrolling" & b

	'	ElseIf BOT(b).Z > 30 Then
	'			' ball on plastic ramp
	'			StopSound("BallRoll_" & b)
	'			PlaySound "fx_plasticrolling" & b, -1, Vol(BOT(b)) / 2, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
	'	Else
	'		If rolling(b) = True Then
	'			StopSound("BallRoll_" & b)
	'			StopSound "fx_plasticrolling" & b
	'			rolling(b) = False
	'		End If
	'	End If


For b = 0 to UBound(BOT)
		If BallVel(BOT(b) ) > 1 Then
            rolling(b) = True
			If isBallOnWireRamp Then
				' ball on wire ramp
				'Debug.Print "Ball on Wire Ramp"
				StopSound("BallRoll_" & b)
				StopSound "fx_plasticrolling" & b
				'PlaySound "fx_metalrolling" & b, -1, Vol(BOT(b)) / 4, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))		
				PlaySound ("fx_metalrolling" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDialRamps, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
			ElseIf BOT(b).Z > 30 Then
				' ball on plastic ramp
				StopSound("BallRoll_" & b)
				StopSound "fx_metalrolling" & b
				'PlaySound "fx_plasticrolling" & b, -1, Vol(BOT(b)) / 4, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
				PlaySound ("fx_plasticrolling" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDialRamps, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
			Else
				' ball on playfield
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
			End If
		Else
			If rolling(b) Then
                StopSound("BallRoll_" & b)
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
                rolling(b) = False
            End If
		End If


		'***Ball Drop Sounds***
		If BOT(b).VelZ < -1 and BOT(b).z < 75 and BOT(b).z > 25 Then 'height adjust for ball drop sounds
			'debug.print "bounce: " & BOT(b).velz
'			If DropCount(b) >= 5 Then
'				DropCount(b) = 0
				If BOT(b).velz > -7 Then
					'debug.print "soft: " & BOT(b).velz
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					'debug.print "hard: " & BOT(b).velz
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If
				'debug.print "*"
'			End If
		End If
'		If DropCount(b) < 5 Then
'			DropCount(b) = DropCount(b) + 1
'		End If
	Next
End Sub

'////////////////////// Options //////////////////////


If Artsides = 1 then
		PinCab_Blades.image = "PinCab_Blades_Congo"
		PinCab_Blades.blenddisablelighting = 0
	Else
		PinCab_Blades.image = "PinCab_Blades"
		PinCab_Blades.blenddisablelighting = 1
End if

If RampLook = 0 Then
	Prim_Ramp3.image = "plastic_ramps"
Else
	Prim_Ramp3.image = "plastic_ramps_Alt_FL"
end if


DIM VRThings
If VRRoom > 0 Then
	scoretext.visible = 0
	Pincab_Rails.visible = 1
	Pincab_Backglass.blenddisablelighting = 10
	WireRamps_up.image = "wireRamps_vr"
	WireRamps_down.image = "wireRamps_vr"
	Prim_Ramp3.material = "R1amp ClearWimgVR"
	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_360:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_360:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
	End If
	If VRRoom = 3 Then
		for each VRThings in VR_Cab:VRThings.visible = 0:Next
		for each VRThings in VR_360:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		PinCab_Backglass.visible = 1
		PinCab_Backbox.visible = 1
		DMD.visible = 1
	End If
Else
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	for each VRThings in VR_360:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	If DesktopMode then scoretext.visible = 1 else scoretext.visible = 0 End If
	If DesktopMode then Pincab_Rails.visible = 1 else Pincab_Rails.visible = 0 End If
	Prim_Ramp3.material = "R1amp ClearWimg"
End if

If CabinetMode = 1 Then
	PinCab_Rails.visible = 0
	PinCab_Blades.Size_y = 2000
Elseif CabinetMode = 2 Then
	PinCab_Rails.visible = 0
	PinCab_Blades.visible = 0
Else
	PinCab_Blades.Size_y = 1000
End If



'******************************************************
'		FLIPPERS PRIMS & SHADOWS
'******************************************************
sub FlipperTimer()
	pleftFlipper.rotz=leftFlipper.CurrentAngle
	UPleftFlipper.rotz=ULeftFlipper.CurrentAngle
	prightFlipper.rotz=rightFlipper.CurrentAngle

    LFLogo.RotZ = LeftFlipper.CurrentAngle
	ULFLogo.RotZ = ULeftFlipper.CurrentAngle
    RFLogo.RotZ = RightFlipper.CurrentAngle
end sub
