' Original VP9 Table by JPSalas
' VP10 conversion by nFozzy
'
' Spotlight primitive by Dark
' Flasher images by LoadedWeapon
' Some SFX by Knorr and Clark Kent

'Version 1.2

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0


'Options------
const HardFlips = 1	'more rigid flippers
const SingleScreenFS = 0	'Single Screen FS support
InlaneType 0 '0 = smooth feed to the left flipper, 1 = old sticky inlane
'------------

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMColoredDMD:UseVPMColoredDMD = False
Const UseVPMModSol = 1
If Table1.showdt = false then LeftRail.visible = 0:RightRail.visible = 0

LoadVPM "01560000", "WPC.VBS", 3.5


'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const cSingleRFlip = 0
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

Set GICallback = GetRef("UpdateGIon")
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

Sub BallReflect_hit:table1.BallReflection = false:end Sub
Sub BallReflect_unhit:table1.BallReflection = true:end Sub


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

gi_bulb1.visible = 1
gi_bulb2.visible = 1
GI28.visible = 1
GI29.visible = 1
Gi30.visible = 1

f17fa.rotx = -3
f17fa.roty = 0 '0
f17fa.rotz = -25
f17fa.height =280 	'237

f17fa.x	=270	'259.69
f17fa.y	=450	'266.398

f17fa.opacity = 5000
f17fa.modulatevsadd = 1

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

gi_ambientbottom.modulatevsadd = 1
gi_ambienttop.modulatevsadd = 1

gi_ambienttop.opacity = 4500
gi_ambientbottom.opacity = 4500
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
			.Hidden = 0 
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

	'Sidewall tops
'	borderL.visible = cInt(desktopmode)
'	borderR.visible = cInt(desktopmode)
'	borderL2.visible = cInt(desktopmode)
'	borderR2.visible = cInt(desktopmode)

	'DMD Adjust
	if DesktopMode Then
		flasherDMD.X = -58: flasherDMD.Y = 1580: flasherDMD.rotX = -40: flasherDMD.rotY = 2.5:flasherDMD.height = 830
	elseif SingleScreenFS then
		'/
'		msgbox("SSFS")
	Else
		flasherdmd.visible = 0
	end if

	FlashLevel(0) = 1.1 : 	FlashLevel(1) = 0.98 : 	FlashLevel(2) = 0.98	'boot gi (needs help)
	UpdateGI 0, 8:UpdateGI 1, 8:UpdateGI 2, 8
End Sub

Sub CreateLPFball
	Set LowerPlayfieldBall = kickerLPF.Createball
	with LowerPlayfieldBall
		.image = "ball_HDR3"
'		.color = RGB(108,108,108)	'148
		.BulbIntensityScale = 0.1
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

Sub UpdateGIOn(no, Enabled)
'	tbs.text = "on:" & no & " " & enabled
	Select Case no
		Case 0 'Gorilla
			If Enabled Then
'				SetLamp 190, 1
'				For each xx in GIGorilla:xx.State = 1: Next
				setmodlamp 0, gistepm
				FadingLevel(0) = 5
			Else
'				For each xx in GIGorilla:xx.state = 0: Next
'				SetLamp 190, 0
				setmodlamp 0, 0
'				FadingLevel(0) = 4
			End If
		Case 1
			If Enabled Then
				For Each xx in GiTop:xx.state = 1: Next
''				Gi_flasher.visible = 1	'spotlight
'				SetLamp 191, 1
				setmodlamp 1, gistepm
				FadingLevel(1) = 5
				GI28.state = 1
				GI29.state = 1
				Gi30.state = 1
			else
				For Each xx in GiTop:xx.state = 0: Next
''				Gi_flasher.visible = 0
'				SetLamp 191, 0
'				UpdateLightScaling 0, 1
				setmodlamp 1, 0
'				FadingLevel(1) = 4
				GI28.state = 0
				GI29.state = 0
				Gi30.state = 0
			End If
		Case 2
			If Enabled Then
				For Each xx in GiBottom:xx.state = 1: Next
'				SetLamp 192, 1
				setmodlamp 2, gistepm
				FadingLevel(2) = 5
				gi_bulb1.state = 1
				gi_bulb2.state = 1
			Else
				For Each xx in GiBottom:xx.state = 0: Next
'				SetLamp 192, 0
'				UpdateLightScaling 0, 2
				setmodlamp 2, 0
'				FadingLevel(2) = 4
				gi_bulb1.state = 0
				gi_bulb2.state = 0

			End If
	End Select
End Sub
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
    Dim ii, x
    If step = 0 then exit sub 'only values from 1 to 8 are visible and reliable. 0 is not reliable and 7 & 8 are the same so...
    gistep = (step-1)' / 7
	gistepm = ScaleGI(gistep, 25)
'	tbs.text = "mod:" & no & " " & step
    Select Case no
        Case 0
'			textbox1.text = step
'			If step > 0 and step < 8 Then
'				For each xx in GIGorilla:xx.State = 1: Next
'			Else
'				For each xx in GIGorilla:xx.state = 0: Next
'			End If
'				GorillaGI
'			For each ii in GiGorilla
'				ii.IntensityScale = gistepm
'			Next
'			GI_gorilla.opacity = GI_gorilla.uservalue * gistepm
'			GIflare4.opacity = GIflare4.uservalue * gistepm
'			GIflare5.opacity = GIflare5.uservalue * gistepm
			setmodlampf 0, gistepm
        Case 1
'			UpdateLightScaling 1, 1
'            For each ii in GiTop
'                ii.IntensityScale = gistepm
'				' back.Image="backwall"&step
'            Next
'			GI_AmbientTop.Opacity = GI_AmbientTop.UserValue * gistepm
'			GI_PlasticsTop.Opacity = GI_PlasticsTop.UserValue * gistepm
'			Gi_flasher.Opacity = Gi_flasher.UserValue * gistepm	'spotlight
'			GIflare1.opacity = GIflare1.uservalue * gistepm
'			GIflare2.opacity = GIflare2.uservalue * gistepm
			setmodlampf 1, gistepm
        Case 2
'			UpdateLightScaling 1, 2
'            For each ii in GiBottom
'                ii.IntensityScale = gistepm
'            Next
'			GI_AmbientBottom.Opacity = GI_AmbientBottom.UserValue * gistepm
'			GI_PlasticsBottom.Opacity = GI_PlasticsBottom.UserValue * gistepm
'			Gi_PlasticsBottomLvl2.opacity = GI_PlasticsBottomLvl2.UserValue * gistepm
'			GIflare3.opacity = GIflare3.uservalue * gistepm
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


dim GiElements
GiElements = Array(GI_AmbientTop, GI_PlasticsTop, GI_AmbientBottom, GI_PlasticsBottom, Gi_Flasher, Gi_PlasticsBottomLvl2, _
			GI_Gorilla, GIflare1, GIflare2, Giflare3, Giflare4, giflare5)
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
	If keycode = LeftTiltKey Then vpmNudge.donudge 90, 3.5:PlaySound "fx_nudge", 0, 1, -0.1, 0.25 : exit sub
	If keycode = RightTiltKey Then vpmNudge.donudge 270, 3.5:PlaySound "fx_nudge", 0, 1, 0.1, 0.25 : exit sub
	If keycode = CenterTiltKey Then vpmNudge.doNudge 0, 4:PlaySound "fx_nudge", 0, 1, 0, 0.25 : exit sub
    If keycode = PlungerKey Then PlaySound SoundFx("PlungerPull",0), 0, 0.05, 0.1, 0.25:Plunger.Pullback:end if
'	If keycode = LeftFlipperKey Then
'		SolLFlipper 1
'		SolULFlipper 1
''		GorillaRight 1
''		GorillaLeft 0
''		flipnf 0, 1
''		if FlippersEnabled then LeftFlipperOn = 1 else LeftFlipperOn = 0	'testing
'	end if
'	If keycode = RightFlipperKey Then
'		SolRFlipper 1
''		GorillaLeft 1
''		GorillaRight 0
''		flipnf 1, 1
''		if FlippersEnabled then RightFlipperOn = 1 else RightFlipperOn = 0	'testing
'	end if

'		if keycode = 28 then updateLT

'		if keycode = 200 then LTcontUpDown 1
'		if keycode = 208 then LTcontUpDown 0
'
'		if keycode = 203 then LtcontLeftRight -1
'		if keycode = 205 then LtcontLeftRight 1

'		if keycode = 200 then FlasherDMD1.y = Flasherdmd1.y + 10
'		if keycode = 208 then FlasherDMD1.y = Flasherdmd1.y - 10
'		if keycode = 203 then FlasherDMD1.x = Flasherdmd1.x - 10
'		if keycode = 205 then FlasherDMD1.x = Flasherdmd1.x + 10
'		if keycode = 38 then FlasherDMD1.height = Flasherdmd1.height - 10	'L
'		if keycode = 37 then FlasherDMD1.height = Flasherdmd1.height + 10	'K

'		if keycode = 38 then FlasherDMD.ROTY = Flasherdmd.ROTY - 2.5	'L
'		if keycode = 37 then FlasherDMD.ROTY = Flasherdmd.ROTY + 2.5	'K
''
'		if keycode = 200 then t1.interval = t1.interval - 10 :	tb1.text = "interval: " & t1.interval
'		if keycode = 208 then t1.interval = t1.interval + 10 :	tb1.text = "interval: " & t1.interval

'	If keycode=33 then ' test Right Kicker debug
'''	initlampsforfading
'''	textbox1.text = insertfading(55, 0)
''	TestUpperFlipper
'''	Kicker1.createball
'''	kicker1.kick 22, 1
'''	bip = bip + 1
'''	vpmTimer.PulseSw 71
'''	vpmtimer.pulsesw 11		'Grey test
'''	If keycode=32 then ' test Left Kicker
'''	Kicker2.createball
'''	kicker2.kick d1,d2
'''	End If
'	LeftRampDrop1.createball
'	LeftRampDrop1.kick 0, 0
'    End If
'	If keycode=33 then ' test Right Kicker debug
''	initlampsforfading
''	textbox1.text = insertfading(55, 0)
''	Kickerz.createball
''	kickerz.kick 180, 10
''	bip = bip + 1
''	vpmTimer.PulseSw 71
'	vpmtimer.pulsesw 11		'Grey test
''	If keycode=32 then ' test Left Kicker
''	Kicker2.createball
''	kicker2.kick 1,1
''	End If
'    End If
	If vpmKeyDown(keycode) Then Exit Sub
End Sub
dim d1, d2
d1 = 15 : d2 = 20


Sub table1_KeyUp(ByVal Keycode)
    If keycode = PlungerKey Then 
		Plunger.Fire
		if BallInPlunger then 
			PlaySound SoundFX("plunger3",0),0,0.5,0.05,0.02
		Else
			PlaySound SoundFX("plunger",0),0,0.6,0.05,0.02
		end if
	End If
'	If keycode = LeftFlipperKey Then
'		SolLFlipper 0
'		SolULFlipper 0
''		flipnf 0, 0
''		LeftFlipperOn = 0
'
''		GorillaRight 0
'	end if
'	If keycode = RightFlipperKey Then
'		SolRFlipper 0
''		flipnf 1, 0
''		RightFlipperOn = 0
'
''		GorillaLeft 0
'	end if
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
'    PlaySound SoundFx("fx_slingshot1",DOFContactors),0,0.7,-0.01,0.05
	PlaySound SoundFX("LeftSlingshot",DOFContactors), 0, 0.7, -0.01, 0.2	'clark
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -28
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -16
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 62
 '   PlaySound SoundFx("fx_slingshot2",DOFContactors), 0, 0.7, 0.01, 0.05
	PlaySound SoundFX("RightSlingshot",DOFContactors), 0, 0.7, 0.01, 0.2	'clark
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -28
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -17
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' Bumpers
'Sub Bumper1_Hit:vpmTimer.PulseSw 63:PlaySound SoundFx("fx_bumper3",DOFContactors), 0, 1, 0.01, 0.20:End Sub
'Sub Bumper2_Hit:vpmTimer.PulseSw 64:PlaySound SoundFx("fx_bumper3",DOFContactors), 0, 1, 0.02, 0.20:End Sub
'Sub Bumper3_Hit:vpmTimer.PulseSw 65:PlaySound SoundFx("fx_bumper3",DOFContactors), 0, 1, 0.01, 0.20:End Sub
Sub Bumper1_Hit:vpmTimer.PulseSw 63:PlaySound SoundFx("rightbumper_hit",DOFContactors), 0, 0.5, 0.01, 0.10:End Sub	'clark
Sub Bumper2_Hit:vpmTimer.PulseSw 64:PlaySound SoundFx("topbumper_hit",DOFContactors), 0, 0.5, 0.02, 0.10:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 65:PlaySound SoundFx("leftbumper_hit",DOFContactors), 0, 0.5, 0.01, 0.10:End Sub

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

Sub sw46_Hit:vpmTimer.PulseSw 46:End Sub
Sub sw47_Hit:vpmTimer.PulseSw 47:End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:End Sub
Sub sw56_Hit:vpmTimer.PulseSw 56:End Sub	'Laser Perimeter
Sub sw51_Hit:vpmTimer.PulseSw 51:End Sub
Sub sw52_Hit:vpmTimer.PulseSw 52:End Sub

Sub sw54_Hit:vpmTimer.PulseSw 54:End Sub			'We Are
Sub sw55_Hit:vpmTimer.PulseSw 55:End Sub			'Watching
Sub sw28_Hit:vpmTimer.PulseSw 28:End Sub			'You

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

'***************************
'   Solenoids & Flashers
'some soleoid subs are from
'Aurian/Guitar/Jive's table
'***************************

SolCallBack(1) = "Auto_Plunger"
SolCallBack(2) = "Kick_back"
SolCallBack(3) = "SolPopUp"
SolCallBack(4) = "SolPopDown"

'Pop up
Sub SolPopUp(Enabled)
	if bsAmyVuk.hasball then bip = bip + 1
	If Enabled Then
		if bsamyVuk.hasball then 
			bsAmyVuk.SolOutAlt 0
			bsAmyVuk.ExitSol_On
			playsound SoundFX("fx_kicker2",DOFContactors), 0, 0.5, 0
		Else
			playsound SoundFX("fx_solenoidOn",DOFContactors), 0, 0.5, 0
		end If
	Else
'		bsAmyVuk.SolOutAlt 1	'Down
		playsound SoundFX("fx_solenoidOff",0), 0, 0.03, 0
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
			playsound SoundFX("fx_kicker2",DOFContactors), 0, 0.5, 0
		Else
			playsound SoundFX("fx_solenoidOn",DOFContactors), 0, 0.5, 0
		end If
	Else
		playsound SoundFX("fx_solenoidOff",0), 0, 0.03, 0
	End If
End Sub

'sw53 (2 way popper) 
sub sw53_hit:controller.Switch(53) = 1:bsAmyVuk.addball me:bip = bip - 1:playsound "kicker_hit", 0, 0.5, 0.01:End Sub	'clark


'SolCallBack(5) = "vpmSolDiverter Diverter,true,"
SolCallback(5) = "RampDiverter"
'SolCallBack(6) = "bsVolcano.SolOut"
SolCallBack(6) = "VolcanoKickOut"
Sub VolcanoKickOut(enabled)
	If Enabled Then
		if bsVolcano.balls then
			bsVolcano.ExitSol_On
			playsound SoundFX("kicker_release",DOFcontactors), 0, 1, 0.01
		Else
			playsound SoundFX("fx_solenoidOnVar",DOFContactors), 0, 1, 0.01
		end if
	Else
		playsound SoundFX("fx_solenoidOffVar",0), 0, 0.03, 0.01
	end if
End Sub

'Sub Sw36_Hit:vpmTimer.PulseSw(36):bsVolcano.AddBall me:bip = bip - 1:playsound "fx_drain", 1, 0.1, 0.005:End Sub	'name,loopcount,volume,pan,randompitch
Sub Sw36_Hit:vpmTimer.PulseSw(36):bsVolcano.AddBall me:bip = bip - 1:playsound "Scoop_Enter2", 1, 0.05, 0.002:End Sub	'Clark


SolCallBack(7) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallBack(8) = "SolTopPost"
'SolCallBack(9) = "bsTrough.SolOut"
SolCallBack(9) = "SolRelease"
Sub SolRelease(Enabled)	'ball tracking
    If Enabled Then
		if bsTrough.Balls > 0 then
			Playsound SoundFX("BallRelease",DOFcontactors), 0, 0.25, 0.005	'ClarkKent
			vpmTimer.PulseSw 31
			bsTrough.ExitSol_On
			bip = bip + 1
		Else
			playsound SoundFX("fx_solenoidOnVar",DOFContactors), 0, 1, 0
		end if
	Else
		playsound SoundFX("fx_solenoidOffVar",0), 0, 0.1, 0
    End If
End Sub

' Drain hole
Sub Drain_Hit
	playsound "fx_drain", 0, 0.5, 0
	bsTrough.AddBall Me
	bip = bip - 1
	if bip < 0 then bip = 0 
End Sub

'SolCallBack(10)	' Left SLingshot
'SolCallBack(11)	' Right Slingshot
'SolCallBack(12)	' Left bumper
'SolCallBack(13)	' Right bumper
'SolCallBack(14)	' Bottom bumper
SolCallBack(15) = "GorillaRight"
SolCallBack(16) = "GorillaLeft"
'SolCallBack(17) = "SetLamp 117,"	'Amy Flasher
'SolCallBack(18) = "SetLamp 118,"	'Left Ramp Flasher
'SolCallBack(19) = "SetLamp 119,"	'2-Way Popper Flasher
'SolCallBack(20) = "SetLamp 120,"	'SkillShot Flasher
'SolCallBack(21) = "SetLamp 121,"	'Gray Gorilla Flasher
'SolCallback(22) = "bsMap.SolOut"
SolCallback(22) = "MapKick"
Sub MapKick(enabled)
	if bsMap.hasball then bip = bip + 1
	If Enabled Then
		if bsMap.hasball then 
			playsound SoundFX("fx_kicker2",DOFContactors), 0, 1, 0.012
			bsmap.ExitSol_On
		Else
			playsound SoundFX("fx_solenoidOn",DOFContactors), 0, 1, 0.012
		end If
	Else
		playsound SoundFX("fx_solenoidOff",0), 0, 0.1, 0.012
	End If
end sub
Sub Sw38_Hit:controller.Switch(38) = 1:bsMap.addball me:bip = bip - 1:playsound "kicker_hit", 0, 0.5, 0.08:End Sub	'clark

SolCallBack(23) = "LeftGateOn"
SolCallBack(24) = "RightGateOn"
'SolCallBack(23) = "vpmSolGate Gate4,true,"
'SolCallBack(24) = "vpmSolGate Gate2,true,"	'dese are reversed
'SolCallBack(25) = "SetLamp 125,"	'Lower Right Flasher
'SolCallBack(26) = "SetLamp 126,"	'Right Ramp Flasher
'SolCallBack(27) = "SetLamp 127,"	'Volcano Flasher
'SolCallBack(28) = "SetLamp 128,"	'Perimeter Defense Flasher
SolCallBack(33) = "SolLeftPost"
'SolCallback(34) = "bsMystery.SolOut"
SolCallback(34) = "MysteryKick"
Sub MysteryKick(enabled)
	if bsMystery.hasball then bip = bip + 1 end if
	If Enabled Then
		if bsMystery.hasball then 
			playsound SoundFX("fx_kicker2",DOFContactors), 0, 1, 0.002
			bsmystery.ExitSol_On 
		Else
			playsound SoundFX("fx_solenoidOn",DOFContactors), 0, 1, 0.002
		end If
	Else
		playsound SoundFX("fx_solenoidOff",0), 0, 0.1, 0.002
	End If
end sub

Sub Sw37_Hit:controller.Switch(37) = 1:bsMystery.addball me:bip = bip - 1:playsound "kicker_hit", 0, 0.5, 0.05:End Sub	'clark
'
DiverterSwoop.isdropped = 1
Sub RampDiverter(enabled)
	if Enabled Then
		playsound SoundFX("fx_solenoidon",DOFcontactors), 0, 0.2, 0.01
		Diverter.rotatetoend
		DiverterSwoop.isdropped = 0
	else
		playsound SoundFX("fx_solenoidoff",DOFcontactors), 0, 0.1, 0.01
		Diverter.rotatetostart
		DiverterSwoop.isdropped = 1
	End If
End Sub

SolModCallback(17) = "SetModLamp 117,"	'Amy Flasher
SolModCallback(18) = "SetModLampm 118, 138,"	'Left Ramp Flasher
SolModCallback(19) = "SetModLampm 119, 129,"	'2-Way Popper Flasher
SolModCallback(20) = "SetModLampm 120, 130,"	'SkillShot Flasher
SolModCallback(21) = "SetModLamp 121,"	'Gray Gorilla Flasher
SolModCallback(25) = "SetModLampm 125, 135,"	'Lower Right Flasher
SolModCallback(26) = "SetModLampm 126, 136,"	'Right Ramp Flasher
SolModCallback(27) = "SetModLampM 127, 137,"	'Volcano Flasher
'SolModCallback(28) = "SetModLampm 128, 129,"	'Perimeter Defense Flasher

SolModCallback(28) = "Sol28"	'Perimeter Defense Flasher	'old style

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

	gi_bulb1.state = 1
	gi_bulb2.state = 1
	GI28.state = 1
	GI29.state = 1
	Gi30.state = 1

	LightFallOff(117, 0) = f17.Falloff	'amy
	LightFallOff(117, 1) = f17b.Falloff

	LightFallOff(118, 0) = F18.Falloff	'Lramp
	LightFallOff(138, 0) = F18b.Falloff
	FlashSpeedUp(138) = 1.32
	FlashSpeedDown(138) = 0.9

	LightFallOff(119, 0) = F19.Falloff	'Two-way popper
	LightFallOff(119, 1) = F19a.Falloff
	LightFallOff(129, 0) = F19b.Falloff
	FlashSpeedUp(129) = 1.32
	FlashSpeedDown(129) = 0.9

	LightFallOff(120, 0) = F20.Falloff	'skillshot
	LightFallOff(120, 1) = F20a.Falloff	'ambient shadowing
	LightFallOff(120, 2) = F20a2.Falloff	'ambient
	LightFallOff(130, 0) = F20b.Falloff	' bulb		'using fadinglevel 30
	FlashSpeedUp(130) = 1.32
	FlashSpeedDown(130) = 0.9

'	LightFallOff(131, 0) = f21B.Falloff	'grey ambient 31
	FlashSpeedUp(131) = 1.32
	FlashSpeedDown(131) = 0.9

	LightFallOff(121, 0) = f21.Falloff	'grey flash

	LightFallOff(125, 0) = f25.Falloff	'opposite of the skillshot single flasher
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


	UpdateGorilla

	nmodflashm 117, f17fa, 	0,	15
	nModLightm 117, f17b,	1,	25
	nModLight 117, f17	,	0,	25, 1	'amy

	nModLight 118, F18,		0,	9,	1	'left ramp f
	nModLight 138, F18b,	0,	15,	1	'left ramp f 'bulb 138

	nModLight 119, F19,		0,	9,	1	'2-way popper
	nModLightm 119, f19a,	1,	9
	nModLight 129, F19b,	0,	15,	1	'2-way popper	'bulb 129

	nModLightm 120, f20a2,	2,	9
	nModLightm 120, f20a,	1,	9
	nModLight 120, F20,		0,	9,	1	'skillshot		'4 lights
	nModLight 130, F20b,	0,	15,	1	'skillshot


'	nModLight 131, F21B, 	0,	15,	0.75 'ambient grey LPF Flasher, 131
	nModLight 121, F21,		0,	15,	1	'LPF flasher

	nModLight 125, F25,		0,	9,	1	'Lower Right Flasher
	nModLightm 125, F25a,	1,	9	'Lower Right Flasher
	nModLight 135, F25b,	0,	15,	1	'Lower Right Flasher	'bulb 135

	nModLightm 126, f26a,	1,	9	'Right Ramp Flasher
	nModLight 126, f26,		0,	9,	1	'Right Ramp Flasher
	nModLight 136, f26b,	0,	9,	1	'Right Ramp Flasher

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
				gi_bulb1.IntensityScale = 1/4 * (3*FlashLevel(nr) + 1)
				gi_bulb2.IntensityScale = 1/4 * (3*FlashLevel(nr) + 1)
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
				GI_AmbientBottom.IntensityScale = FlashLevel(nr)
				GI_PlasticsBottom.IntensityScale = FlashLevel(nr)
				Gi_PlasticsBottomLVL2.IntensityScale = FlashLevel(nr)
				GIflare1.IntensityScale = FlashLevel(nr)
				GIflare2.IntensityScale = FlashLevel(nr)
			elseif nr = 1 then 
				GI_AmbientTop.IntensityScale = FlashLevel(nr)
				GI_PlasticsTop.IntensityScale = FlashLevel(nr)
				Gi_flasher.IntensityScale = FlashLevel(nr)	'spotlight
				GIflare3.IntensityScale = FlashLevel(nr)
			elseif nr = 0 Then 'gorilla
				Gi_Gorilla.Intensityscale = FlashLevel(nr)
				giflare4.IntensityScale = FlashLevel(nr)
				giflare5.IntensityScale = FlashLevel(nr)
			End If
	end select
End Sub



'sub tb3x_timer():me.text = FadingLevel(120) & vbnewline & f20.intensityscale & vbnewline & FlashLevel(120) & " < " & SolModValue(120): end sub 


'Sub nModLightInsert(nr, object, offset, scaletype)	'Fading using intensityscale with modulated callbacks	'outdated script, not used on this table
'	dim DesiredFading
'	Select Case FadingLevel(nr)
'		Case 4	'off
'			FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
'			If FlashLevel(nr) < 0 then FlashLevel(nr) = 0 : FadingLevel(nr) = 0
'			Object.IntensityScale = ScaleLights(FlashLevel(nr),scaletype ) * GIscale(nr)
'			Object.Falloff = LightFallOff(nr, offset) * ScaleFalloff(FlashLevel(nr),scaletype )
'			object.state = 0
'		Case 5 ' Fade (Dynamic)
'			object.state = 1
'			DesiredFading = SolModValue(nr)
'
'			if FlashLevel(nr) < DesiredFading Then 
''				tb5.text = "+"
'			object.state = 0
'				FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
'				If FlashLevel(nr) >= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
'			elseif FlashLevel(nr) > DesiredFading Then 
''				tb5.text = "-"
'				FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
'				If FlashLevel(nr) <= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
'			End If
'			Object.Intensityscale = ScaleLights(FlashLevel(nr),scaletype ) * GIscale(nr)
'			Object.Falloff = LightFallOff(nr, offset) * ScaleFalloff(FlashLevel(nr) , nr)
'	End Select
'End Sub
'
'Sub nModLightInsertm(nr, Object, offset, scaletype)	'uses offset to store different falloff values in a unused lamp number. default 0 'special for the insert flasher
'	if FadingLevel(nr) = 4 then object.state = 0
'	if FadingLevel(nr) = 5 then object.state = 1
'    Object.IntensityScale = ScaleLights(FlashLevel(nr),scaletype ) * GIscale(nr)
'	Object.Falloff = LightFallOff(nr + offset) * ScaleFalloff(FlashLevel(nr) )
'End Sub

Function FunctEvenOut(value, divis)	'IN: 0-255, 'divis', OUT:A number divisible by 'divis' value 'unused
	FunctEvenOut = Round((value+1)/divis)*divis
End Function















'Sub VolcanoKick(enabled): bsVolcano.SolOut True	:	if bsVolcano.balls > 0 then bip = bip + 1 end if : end sub

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
		playsound SoundFX("Kicker_release",DOFContactors), 0, 1, 0.02
		if BallInPlunger then 
			PlaySound SoundFX("plunger3",0),0,0.5,0.02,0.02
		Else
			PlaySound SoundFX("plunger",0),0,0.6,0.02,0.02
		end if
    else
       AutoPlunger.Pullback
	End If
End Sub

Sub Kick_Back(Enabled)
    If Enabled Then
       KickBack.Fire
	   PlaySound SoundFX("fx_solenoid",DOFContactors)
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
	playsound "drop_mono", 0, 0.2, -0.1
End Sub

' Top Post - Lock
Sub SolTopPost(Enabled)
	If Enabled Then
		TopPost.IsDropped = 1
		playsound SoundFX("LockupPin",DOFcontactors), 0, 0.2, 0.02	'clark
	Else
		TopPost.IsDropped = 0
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



'====================
'   -NEW     HARD
'       FLIPS
'====================
'just switches EOStorque when hit
'const HardFlips = 1	'move me!
dim defaultEOS, hardEOS
defaultEOS = LeftFlipper.eostorque
hardEOS = 2200 / LeftFlipper.strength	'eos equivalent to 2200 strength
if HardFlips = 0 then TriggerLF.enabled = 0 : TriggerRF.enabled = 0 end if

Sub TriggerLF_Hit()
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 3 then
		LeftFlipper.eostorque = hardEOS
    End if
end sub

Sub TriggerRF_Hit()
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 3 then
		RightFlipper.eostorque = hardEOS
    End if
end sub

'********************
' Special JP Flippers
'********************
 
 SolCallback(sLRFlipper) = "SolRFlipper"
 SolCallback(sLLFlipper) = "SolLFlipper"
 SolCallback(sULFlipper) = "SolULFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
 '       PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, 0.4, -0.05, 0.11
        PlaySound SoundFX("FlipperUpLeft",DOFFlippers), 0, 0.4, -0.05, 0.0	'clark
        LeftFlipper.RotateToEnd
    Else
'        PlaySound SoundFX("fx_flipperdown",DOFContactors), 0, 0.4, -0.05, 0.11
        PlaySound SoundFX("flipper_down",0), 0, 1, -0.05, 0.11	'clark
        LeftFlipper.RotateToStart
		LeftFlipper.EOStorque = DefaultEOS
    End If
End Sub

'SoundFX("fx_flipperup",DOFContactors)
Sub SolRFlipper(Enabled)
    If Enabled Then
'		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, 0.4, 0.05, 0.15
        PlaySound SoundFX("FlipperUpLeft",DOFFlippers), 0, 0.4, 0.05, 0.0	'clark

		RightFlipper.RotateToEnd
    Else
'		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 0.4, 0.05, 0.15
        PlaySound SoundFX("flipper_down",0), 0, 1, 0.05, 0.11	'clark
		RightFlipper.RotateToStart
		RightFlipper.EOStorque = DefaultEOS
    End If
End Sub
'flipnf
Sub SolULFlipper(Enabled)
	If Enabled Then
'		PlaySound "fx_flipperup", 0, 0.5, -0.06, 0.15
		ULeftFlipper.RotateToEnd
	Else
'		PlaySound "fx_flipperdown", 0, 0.5, -0.06, 0.15
		ULeftFlipper.RotateToStart
	End If
End Sub


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
	NFadeL 78, L78	
	
	NFadeLm 81, L81
	NFadeL 81, L81l	'H	'ambient near bumpers, this is a hippo target
	NFadeL 82, L82
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
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 6 ' total number of balls
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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/7, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/12, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
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


Sub BallShadowUpdate_Timer()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6)
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
BallshadowLP.height = LowerPlayfieldBall.Z - 20
BallshadowLP.x = LowerPlayfieldBall.x
BallshadowLP.y = LowerPlayfieldBall.Y + 10 
    Next	
 lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
End Sub

Sub AmyDownRampdrop_Hit():playsound "drop_mono", 0, 0.4, 0.1:End Sub 'name,loopcount,volume,pan,randompitch
'Sub VukRampEnd_Hit():playsound "Ramp_Hit1", 0, 0.2, 0.1:End Sub	'handled by collection instead
Sub WireRampTrigger1_Hit():playsound "wireramp1", 0, 0.2, AudioPan(ActiveBall):end sub
Sub WireRampTriggerend1_Hit():stopsound "wireramp1":end sub
Sub WireRampTrigger2_Hit():playsound "wireramp1", 0, 0.2, AudioPan(ActiveBall):end sub
Sub WireRampTriggerend2_Hit():stopsound "wireramp1":end sub
Sub WireRampTrigger3_Hit():playsound "wireramp1", 0, 0.4, AudioPan(ActiveBall):end sub
	
	
Sub AmyDownRampTrigger_Hit()
	AmyDownRampDrop.Enabled=0
	me.timerenabled=1
End Sub

Sub AmyDownRampTrigger_Timer()
	AmyDownRampDrop.Enabled=1
	me.Timerenabled=0
End Sub

Sub RightRampdrop_Hit():playsound "drop_mono", 0, 0.5, 0.05:End Sub
'Sub RightRampEnd_Hit():playsound "Ramp_Hit1", 0, 0.3, 0.05:End Sub	'handled by collection instead

Sub RightRampTrigger_Hit()
	RightRampDrop.Enabled=0
	me.timerenabled=1
End Sub

Sub RightRampTrigger_Timer()
	RightRampDrop.Enabled=1
	me.Timerenabled=0
End Sub

Sub LeftRampdrop_Hit():playsound "drop_mono", 0, 0.3, -0.03:stopsound "wireramp1":End Sub

Sub RampEntry1_Hit()	'left ramp entry
 	If activeball.vely < -10 then 
		PlaySound "ramp_hit2", 0, Vol(ActiveBall)/5, AudioPan(ActiveBall), 0, Pitch(ActiveBall)*10, 1, 0
	Elseif activeball.vely > 3 then 
		PlaySound "PlayfieldHit", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End If
End Sub

Sub RampEntry2_Hit()	'right ramp entry
 	If activeball.vely < -10 then 
		PlaySound "ramp_hit2", 0, Vol(ActiveBall)/5, AudioPan(ActiveBall), 0, Pitch(ActiveBall)*10, 1, 0
	Elseif activeball.vely > 3 then 
		PlaySound "PlayfieldHit", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End If
End Sub

Sub Diverter_Hit()
	playsound "metalhit_medium", 0, Vol(ActiveBall)/5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub


'Collection Sounds

Sub RampSounds_Hit (idx)
	PlaySound "ramp_hit1", 0, Vol(ActiveBall)/2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub
Sub Targets_Hit (idx)
	PlaySound SoundFX("target",DOFTargets), 0, 0.1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
	PlaySound SoundFX("targethit",0), 0, Vol(ActiveBall)*9, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub
'	PlaySound SoundFx("plastichit",DOFContactors), 0, Vol(ActiveBall) * 0.2, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0

Sub ApronWalls_Hit (idx)
	PlaySound "woodhitaluminium", 0, (Vol(ActiveBall)^2.5)*10, AudioPan(ActiveBall)/4, 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub MetalWalls_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rubbers_O_Hit (idx)
	PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rubbers_U_Hit (idx)
	PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*10, AudioPan(ActiveBall)*3, 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)* 10, AudioPan(ActiveBall) * 3, 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall)*3, 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall)*3, 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall)*3, 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

'Flipper Sounds

Sub ULeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall)*6, 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall)*6, 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall)*6, 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

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





'yonder table test stuff









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
	playsound "ramp_hit1"
End Sub
'	KickerUFTEST2.enabled = 1
'	polltimer.enabled = 0 : discoflips = 1

	tb1.text = "interval: " & t1.interval
sub t1_timer()
	uleftflipper.rotatetoend
	me.enabled = 0
end sub













