Option Explicit
Randomize

DIM DTMODE
DIM B2SSCRIPTS
'************************************************************************************
'**** SET THE LINE BELOW TO DTMODE = 0 to disable all desktop displays for fullscreen 
'cabinet mode. Also press 7 at any time to turn them on or off.
' also set the other line below to B2SSCRIPTS = 0 to disable b2s code if you don't want it on
' or press 8 to toggle it on/off.

DTMODE = 1
B2SSCRIPTS = 0

'*************************************************************************************
' core.vbs constants
Const BallSize = 50  ' 50 is the normal size
Const BallMass = 1   ' 1 is the normal ball mass.
Const cGameName = "SpacePlatform"
Dim Controller
' load extra vbs files
LoadCoreFiles
'LoadControllerSub
'sub B2STOGGLE()
'	if B2SSCRIPTS = 0 then 
'		B2SSCRIPTS = 1
'	Else
'		B2SSCRIPTS = 0
'	end If
'end Sub

'sub DTMODESELECTOR()
'	if DTMODE = 0 then 
'		DTMODE = 1
'	Else
'		DTMODE = 0
'	end If

'end sub


	
Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    On Error Resume Next
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"

End Sub

Sub LoadControllerSub
if B2SSCRIPTS = 1 then
 Set Controller = CreateObject("B2S.Server")
 Controller.B2SName = "SoacePlatform"  ' *****!!!!This must match the name of your directb2s file!!!!
 Controller.Run()
' controller.B2SOn
END If
end Sub

Dim P1Score
Dim P2Score
Dim P3Score
Dim P4Score
dim players
dim mtargetcount
dim gatelast
public shootperm
public rickylockcount
public sasave
public lockcheck
public loop1
public loop1count
public loop2
public loop2count
public loop3
public loop3count
public tilt
public tilted
public loopbreaker
public TableName
public score
public cp
public numplay
public bonuscount
public multiplier
public multiball
public bonuscalc
public bscore
public targetcount
public resetcount
public gameon
public tempcount
public bonuskeepcount

sub setmulti()
'	for each light in multilights
'		light.state = 0
'	next

			x2light.state = 0
			x3light.state = 0
			x4light.state = 0
			x5light.state = 0
			x6light.state = 0
			x7light.state = 0
			x8light.state = 0
	select case multiplier
		case 0
		case 1
		case 2
			x2light.state = 1
		case 3
			x3light.state = 1
		case 4
			x4light.state = 1
		case 5 
			x5light.state = 1
		case 6
			x6light.state = 1
		case 7
			x7light.state = 1
		case 8
			x8light.state = 1
	end select
	if multiplier > 8 then
	'	for each light in multilights
	'		light.state = 2
	'	next
			x2light.state = 2
			x3light.state = 2
			x4light.state = 2
			x5light.state = 2
			x6light.state = 2
			x7light.state = 2
			x8light.state = 2
	end if
end sub

sub setbonuslights()
	
		bl9.state = 0
		bl8.state = 0
		bl7.state = 0
		bl6.state = 0
		bl5.state = 0
		bl4.state = 0
		bl3.state = 0
		bl2.state = 0
		bl1.state = 0
		bl60.state = 0
		bl50.state = 0
		bl40.state = 0
		bl30.state = 0
		bl20.state = 0
		bl10.state = 0
		bl7x.state = 0
		bl6x.state = 0
		bl5x.state = 0
		bl4x.state = 0
		bl3x.state = 0
		bl2x.state = 0
		bl1x.state = 0
		tempcount = bonuscount
	if bonuscount => 480 then
		bl8x.state = 1
		tempcount = tempcount - 480
		if tempcount > 60 then
			tempcount = 60
		end if
	end if
	if bonuscount => 420 and bonuscount < 480 then
		bl7x.state = 1
		tempcount = tempcount - 420
	end if
	if bonuscount => 360 and bonuscount < 420 then
		bl6x.state = 1
		tempcount = tempcount - 360
	end if
	if bonuscount => 300 and bonuscount < 360 then
		bl5x.state = 1
		tempcount = tempcount - 300
	end if
	if bonuscount => 240 and bonuscount < 300 then
		bl4x.state = 1
		tempcount = tempcount - 240
	end if
	if bonuscount => 180 and bonuscount < 240 then
		bl3x.state = 1
		tempcount = tempcount - 180
	end if
	if bonuscount => 120 and bonuscount < 180 then
		bl2x.state = 1
		tempcount = tempcount - 120
	end if
	if bonuscount => 60 and bonuscount < 120 then
		bl1x.state = 1
		tempcount = tempcount - 60
	end if
	if tempcount => 60 then
		bl60.state = 1
		tempcount = tempcount - 60
	end if
	if tempcount => 50 then
		 bl50.state = 1
		 tempcount = tempcount - 50
	end if
	if tempcount => 40 then
		 bl40.state = 1
		 tempcount = tempcount - 40
	end if
	if tempcount => 30 then
		 bl30.state = 1
		 tempcount = tempcount - 30
	end if
	if tempcount => 20 then
		 bl20.state = 1
		 tempcount = tempcount - 20
	end if
	if tempcount => 10 then
		 bl10.state = 1
		 tempcount = tempcount - 10
	end if
	if tempcount = 9 then 
		bl9.state = 1
		bl8.state = 1
		bl7.state = 1
		bl6.state = 1
		bl5.state = 1
		bl4.state = 1
		bl3.state = 1
		bl2.state = 1
		bl1.state = 1

	end if
	if tempcount = 8 then 
		bl8.state = 1
		bl7.state = 1
		bl6.state = 1
		bl5.state = 1
		bl4.state = 1
		bl3.state = 1
		bl2.state = 1
		bl1.state = 1
	end if
	if tempcount = 7 then 
		bl7.state = 1
		bl6.state = 1
		bl5.state = 1
		bl4.state = 1
		bl3.state = 1
		bl2.state = 1
		bl1.state = 1
	end if
	if tempcount = 6 then 
		bl6.state = 1
		bl5.state = 1
		bl4.state = 1
		bl3.state = 1
		bl2.state = 1
		bl1.state = 1
	end if
	if tempcount = 5 then 
		bl5.state = 1
		bl4.state = 1
		bl3.state = 1
		bl2.state = 1
		bl1.state = 1
	end if
	if tempcount = 4 then 
		bl4.state = 1
		bl3.state = 1
		bl2.state = 1
		bl1.state = 1
	end if
	if tempcount = 3 then 
		bl3.state = 1
		bl2.state = 1
		bl1.state = 1
	end if
	if tempcount = 2 then 
		bl2.state = 1
		bl1.state = 1
	end if
	if tempcount = 1 then 
		bl1.state = 1
	end if
end sub									

sub resettargets
lockgate.rotatetostart
LLANEGATE.ROTATETOSTART
RLANEGATE.ROTATETOSTART

	
		trapt1.isdropped = false
		trapt2.isdropped = false	
		trapt3.isdropped = false
	rickytarg1.isdropped = false
	rickytarg2.isdropped = false
	rickytarg3.isdropped = false
	rickytarg4.isdropped = false
	rickytarg5.isdropped = false
end sub

Sub SetScore(points)
	Dim sz
	
	score = points
	
	'if score = 0 then
		'ScoreText.Text = 0
	'else
select case cp
	case 1
		emreel1.addvalue(points)
p1score = p1score + score
if B2SSCRIPTS = 1 then
     Controller.B2SSetScore cp, p1score:Controller.B2SSetScore 5, p1score
end if'		ScoreText.Text = FormatNumber(score, 0, -1, 0, -1)
	case 2
		emreel2.addvalue(points)
p2score = p2score + points
if B2SSCRIPTS = 1 then
     Controller.B2SSetScore cp, p2score:Controller.B2SSetScore 6, p2score
end if'		ScoreText2.Text = FormatNumber(score, 0, -1, 0, -1)
	case 3
		emreel3.addvalue(points)
p3score = p3score + points
if B2SSCRIPTS = 1 then
     Controller.B2SSetScore cp, p3score:Controller.B2SSetScore 7, p3score
end if'		ScoreText3.Text = FormatNumber(score, 0, -1, 0, -1)
	case 4
		emreel4.addvalue(points)
p4score = p4score + points
if B2SSCRIPTS = 1 then
     Controller.B2SSetScore cp, p4score:Controller.B2SSetScore 8, p4score
end if'		ScoreText4.Text = FormatNumber(score, 0, -1, 0, -1)
	'End if
End Select 
	'ScoreText.Text = "564,987"
End Sub
Sub LightReset
'	light6.state = 0
' 	light1.state = 0
' 	light2.state = 0
'	light3.state = 0
' 	light4.state = 0
 
	shootagain.state = 0
	looplight1.state = 0
	looplight2.state = 0
	looplight3.state = 0
'	locklight.state = 0
 '	light5.state = 0
		bl9.state = 0
		bl8.state = 0
		bl7.state = 0
		bl6.state = 0
		bl5.state = 0
		bl4.state = 0
		bl3.state = 0
		bl2.state = 0
		bl1.state = 0
		bl60.state = 0
		bl50.state = 0
		bl40.state = 0
		bl30.state = 0
		bl20.state = 0
		bl10.state = 0
		bl7x.state = 0
		bl6x.state = 0
		bl5x.state = 0
		bl4x.state = 0
		bl3x.state = 0
		bl2x.state = 0
		bl1x.state = 0
			x2light.state = 0
			x3light.state = 0
			x4light.state = 0
			x5light.state = 0
			x6light.state = 0
			x7light.state = 0
			x8light.state = 0
'		Light025.STATE = 0
'		LIGHT022.STATE = 0
'		LIGHT023.STATE = 0
'		LIGHT024.STATE = 0
'		Light025.STATE = 0
'		SPKL.state = 0
'		light009.state = 0
'		light010.state = 0
'		light011.state = 0
'		light012.state = 0
'		light013.state = 0
'LIGHT005.STATE = 0
'LIGHT006.STATE = 0
'LIGHT007.STATE = 0
'LIGHT008.STATE = 0

'		light009.state = 0
'		light010.state = 0
'		light011.state = 0
'		light012.state = 0
'		light013.state = 0
'		light031.state = 0
'		light032.state = 0
'		light033.state = 0

 	Light5001.state = 0
	cslightC.state = 0
	cslighto.state = 0
	cslightl1.state = 0
	cslightd.state = 0
	cslights.state = 0
	cslightl2.state = 0
	cslighte1.state = 0
	cslighte2.state = 0
	cslightp.state = 0

 petelightp.state = 0
 petelighte1.state = 0
 petelightt.state = 0
 petelighte2.state = 0 
	rickylightR.state = 0
	RickyLighti.state = 0
	RickyLightc.state = 0
	RickyLightk.state = 0
	RickyLightY.state = 0

End Sub




Sub table1_init()
TableName = "CatBurglars"
PlaySound "theme"
lockcheck = 0
shootperm = 0
rickylockcount = 0
loop1 = 0
loop1count = 0
loop2 = 0
loop2count = 0
tilt = 0
tilted = 0
tilttext.text = ""
gameon=0
mtargetcount = 0
resetcount = 0
cp = 1
sasave = 0
emreel1.ResetToZero()
p1score = 0
emreel2.ResetToZero()
p2score = 0
emreel3.ResetToZero()
p3score = 0
emreel4.ResetToZero()
p4score = 0
multiball = 0
ballstext.text = 0
p1light.state = 1
resettargets
multiplier = 1
bscore = 0
bonuscount = 0
numplay = 1
resettargets
lightreset
attractmode

If B2SSCRIPTS = 1 Then
Controller.b2ssetscore 1,P1Score
Controller.b2ssetscore 2,P2Score
Controller.b2ssetscore 3,P3Score
Controller.b2ssetscore 4,P4Score
Controller.b2ssetscore 5,P1Score
Controller.b2ssetscore 6,P2Score
Controller.b2ssetscore 7,P3Score
Controller.b2ssetscore 8,P4Score
End If
' ******  SET UP TRAPED BALL CHUTE ******
	
		trapt1.isdropped = true
		trapt2.isdropped = true	
		trapt3.isdropped = true
	trapkicker.createball
	trapkicker.kick 170,5
'	trapreleasetimer.enabled = true
	trapwalls
' *******  END TRAPED BALL CHUTE SETUP ******

End Sub

sub attractmode()'sets lightsto flash sequence
'	for each light in multilights
'		light.state = 2
'	next
'	for each light in petelights
'		light.state = 2
'	next
'	for each light in bonuslights
'		light.state = 2
'	next
'	for each light in coldsleeplights
'		light.state = 2
'	next
'	for each light in rickylights
'		light.state = 2
'	next
'
 stopsound "theme"

stopsound "world"
 PlaySound "theme"
 end sub
 
Sub getplayers()
'if numplay = "" then numplay = 0
numplay = numplay + 1
if numplay > 5 then numplay = 5
'players.text = numplay - 1
End Sub

Sub assigncp()
p1light.state = 0
p2light.state = 0
p3light.state = 0
p4light.state = 0
select case cp
	case 1
		'score = emreel1.value
		p1light.state = 1
	case 2
		'score = emreel2.value
		p2light.state = 1
	case 3
		'score = emreel3.value
		p3light.state = 1
	case 4
		'score = emreel4.value
		p4light.state = 1
end select
End Sub

Sub AddScore(points)
	if multiball > 0 then
		points = points*2
	end if
 '	if light5.state = 2 then
 '		points = points * 2
 '	end if
'	SetScore score + points
	if tilted = 1 then
		points = 0
	end if
	if skilllight.state = 1 then
		skilllight.state = 0
	end if
	setscore points
End Sub



 sub musictimer_timer()
 stopsound "theme"
stopsound "world"
 PlaySound "theme"
 end sub

Sub Drain_Hit()
	Drain.DestroyBall
	bonuscounter.enabled = false
	if multiball = 0 then
	tilt = 0
	tilted = 0
	'kicker1.destroyball
	lockcheck = 0
	sasave = 0
	tilttext.text = ""
	PlaySoundAtBall "plunger"
	resettargets
	lightreset
stopsound "theme"
stopsound "world"
 playsound "world"
 	setmulti
    Select Case ballstext.text
   		Case "5" 						' Else statement gives you a bonus everytime after a extraball
			cp = cp + 1
			assigncp
			if cp = numplay then
				cp = 1
				resetcount=1
				gameon = 0
				numplay = 1
'				players.text = 0
				ballstext.text = 0
				playsound "sound50"
				stopsound "theme"
				stopsound "world"
				PlaySound "theme"
		'		lock1open = 0
				attractmode
			else
				'plunger.createball
 				launch.createball
 				launch.kick 45,8
 				PlaySoundAtBall "plunger"
			end if
   		Case "4"
			cp = cp + 1
			assigncp
			if cp = numplay then
				cp = 1
				ballstext.text = "5"
b1light.state = 0
b2light.state = 0
b3light.state = 0
b4light.state = 0
b5light.state = 1
				assigncp
			end if
				'plunger.createball
 				launch.createball
 				launch.kick 45,8
 				PlaySoundAtBall "plunger"
   		Case "3"
			cp = cp + 1
			assigncp
			if cp = numplay then
				cp = 1
				ballstext.text = "4"
b1light.state = 0
b2light.state = 0
b3light.state = 0
b4light.state = 1
b5light.state = 0
				assigncp
			end if
				'plunger.createball
 				launch.createball
 				launch.kick 45,8
 				PlaySoundAtBall "plunger"
  		Case "2"
			cp = cp + 1
			assigncp
			if cp = numplay then
				cp = 1
				ballstext.text = "3"
b1light.state = 0
b2light.state = 0
b3light.state = 1
b4light.state = 0
b5light.state = 0
				assigncp
			end if
				'plunger.createball
 				launch.createball
 				launch.kick 45,8
 				PlaySoundAtBall "plunger"
   		Case "1"
			cp = cp + 1
			assigncp
			if cp = numplay then
				cp = 1
				ballstext.text = "2"

b1light.state = 0
b2light.state = 1
b3light.state = 0
b4light.state = 0
b5light.state = 0
				assigncp
			end if
				'plunger.createball
 				launch.createball
 				launch.kick 45,8
 				PlaySoundAtBall "plunger"
	   	End Select
	bonuscount = 0
	multiplier = 1
	bonuscount = 0
'	setmulti
	shootperm = 0
	bscore = 0
	playsound "targetup"
'	Gate2.Open = False
	loop1 = 0
	loop1count = 0
	loop2 = 0
	loop2count = 0
	loop3 = 0
	loop3count = 0
	lockcheck = 0
	rickylockcount = 0
	rickylock.destroyball
	skilllight.state = 1
	else
		multiball = multiball - 1
	end if
	shootagain.state = 1
	shootagaintimer.enabled = true
End Sub

sub shootagaintimer_timer()
	shootagaintimer.enabled = false
	shootagain.state = 0
end sub

sub tilttimer_timer()
	if tilt > 0 then
		tilt = tilt - 1
	end if
	if tilt < 3 and tilted = 0 then
		tilttext.text = ""
	end if
end sub


sub bonuskicker_hit()
	if multiball = 0 then
'		loopbreaker = 0
'		if shootagain.state = 1 and multiball > 0 then
'			bonuskicker.destroyball
'			loopbreaker = 1
'		end if
		if shootagain.state = 1 then
			bonuskicker.destroyball
				'plunger.createball
 				launch.createball
 				launch.kick 45,8
 				PlaySoundAtBall "plunger"
			shootagain.state = 0
		else
		bonuskeepcount = bonuscount
		bonuscounter.enabled = true
		end if
	else
		bonuskicker.destroyball
 		multiball = multiball - 1
	end if
end sub

sub bonuscounter_timer()
	loopbreaker = 0
	PlaySound "left_slingshot"
	PlaySound "gobell"
	if bonuscount = 1 and multiplier = 1 then 
		bonuscounter.enabled = false
	end if
	if bonuscount = 0 and multiplier > 1 then
		bonuscount = bonuskeepcount
		multiplier = multiplier - 1
		loopbreaker = 1
		setmulti
		setbonuslights
	end if
	if bonuscount > 0 then
		addscore 10
		bonuscount = bonuscount - 1
		setbonuslights
	end if
	if bonuscount = 0 and multiplier = 1 and loopbreaker = 0 then
		bonuskicker.kick 90, 45
	end if
end sub


'*****************************************************************************************************
' CREDITS
' Initial table created by fuzzel, jimmyfingers, jpsalas, toxie & unclewilly (in alphabetical order)
' Flipper primitives by zany
' Ball rolling sound script by jpsalas
' Ball shadow by ninuzzu
' Ball control & ball dropping sound by rothbauerw
' DOF by arngrim
' Positional sound helper functions by djrobx
' Plus a lot of input from the whole community (sorry if we forgot you :/)
'*****************************************************************************************************


'First, try to load the Controller.vbs (DOF), which helps controlling additional hardware like lights, gears, knockers, bells and chimes (to increase realism)
'This table uses DOF via the 'SoundFX' calls that are inserted in some of the PlaySound commands, which will then fire an additional event, instead of just playing a sample/sound effect
'On Error Resume Next
'ExecuteGlobal GetTextFile("controller.vbs")
'If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
'On Error Goto 0

'If using Visual PinMAME (VPM), place the ROM/game name in the constant below,
'both for VPM, and DOF to load the right DOF config from the Configtool, whether it's a VPM or an Original table

'Const cGameName = ""


Dim EnableRetractPlunger
EnableRetractPlunger = false 'Change to true to enable retracting the plunger at a linear speed; wait for 1 second at the maximum position; move back towards the resting position; nice for button/key plungers


If Table1.ShowDT = false then
emreel1.visible = False
EMReel2.visible = False
EMReel3.visible = False
EMReel4.visible = False

    Scoretext.Visible = false
End If


Dim EnableBallControl
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

'Const BallSize = 25  'Ball radius

Sub Table1_KeyDown(ByVal keycode)

	If keycode = 19 Then
	end if

	if keycode = 6 and gameon = 0 then
		getplayers
		PlaySound "coin3"
	end if
	
	if keycode = 2 and gameon = 0 and numplay > 1 then
		'for each light in vlights
		'	light.state = 0
		'next
		'for each light in CBLights
		'	light.state = 0
		'next
		lockcheck = 0
		gameon = 1
		cp = 1
		emreel1.resettozero()
p1score = 0
		'setscore 0
		cp = 2
		emreel2.resettozero()
p2score = 0
		'setscore 0
		cp = 3
		emreel3.resettozero()
p3score = 0
		'setscore 0
		cp = 4
		emreel4.resettozero()
p4score = 0
		'setscore 0
If B2SSCRIPTS = 1 Then
Controller.b2ssetscore 1,P1Score
Controller.b2ssetscore 2,P2Score
Controller.b2ssetscore 3,P3Score
Controller.b2ssetscore 4,P4Score
Controller.b2ssetscore 5,P1Score
Controller.b2ssetscore 6,P2Score
Controller.b2ssetscore 7,P3Score
Controller.b2ssetscore 8,P4Score
End If
		cp = 1
		multiplier = 1
		rickylockcount = 0
	'rickylock.destroyball
		setmulti
		multiball = 0
		assigncp
		ballstext.text = 1
		resettargets
		lockcheck = 0
		shootperm = 0
		loop1 = 0
		loop1count = 0
		loop2 = 0
		loop2count = 0
		loop3 = 0
		loop3count = 0
		sasave = 0
		lightreset
	'	plunger.createball
stopsound "theme"
stopsound "world"
 PlaySound "world"
  		launch.createball
 		launch.kick 45,15
 		PlaySoundAtBall "plunger"
		shootagain.state = 1
		skilllight.state = 1
		shootagaintimer.enabled = true
b1light.state = 1
b2light.state = 0
b3light.state = 0
b4light.state = 0
b5light.state = 0

	end if

if keycode = 3 Then'test functuins
end If

	If keycode = PlungerKey Then
        If EnableRetractPlunger Then
            Plunger.PullBackandRetract
        Else
		    Plunger.PullBack
        End If
		PlaySoundAt SoundFX("plungerpull", DOFContactors), Plunger
	End If

	If keycode = LeftFlipperKey AND gameon = 1 Then
cHANNELLIGHTCYCLE()
		LeftFlipper.RotateToEnd
		Flipper001.RotateToEnd
		PlaySoundAt SoundFX("fx_flipperup",DOFFlippers), LeftFlipper
	End If

	If keycode = RightFlipperKey AND gameon = 1 Then
LANELIGHTCYCLE()
		RightFlipper.RotateToEnd
		PlaySoundAt SoundFX("fx_flipperup",DOFFlippers), RightFlipper
	End If

	If keycode = LeftTiltKey Then
		Nudge 90, 2
		tilt = tilt + 1
		if tilt = 3 or tilt = 4 then
			tilttext.text = "WARNING!!!"
		end if
		if tilt = 5 then
			tilted = 1
			tilt = 0
			tilttext.text = "TILT!"
		end if
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
		tilt = tilt + 1
		if tilt = 3 or tilt = 4 then
			tilttext.text = "WARNING!!!"
		end if
		if tilt = 5 then
			tilted = 1
			tilt = 0
			tilttext.text = "TILT!"
		end if
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
		tilt = tilt + 1
		if tilt = 3 or tilt = 4 then
			tilttext.text = "WARNING!!!"
		end if
		if tilt = 5 then
			tilted = 1
			tilt = 0
			tilttext.text = "TILT!"
		end if
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
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySoundAtBall "plunger"
	End If

	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToStart
		Flipper001.RotateToStart
		PlaySoundAt SoundFX("fx_flipperdown",DOFFlippers), LeftFlipper
	End If

	If keycode = RightFlipperKey Then
		RightFlipper.RotateToStart
		PlaySoundAt SoundFX("fx_flipperdown",DOFFlippers), RightFlipper
	End If

    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If
End Sub


'Sub Drain_Hit()
'	PlaySound "drain",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
'	Drain.DestroyBall
'	BIP = BIP - 1
'	If BIP = 0 then
'		'Plunger.CreateBall
'		BallRelease.CreateBall
'		BallRelease.Kick 90, 7
'		PlaySound SoundFX("ballrelease",DOFContactors), 0,1,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
'		BIP = BIP + 1
'	End If
'End Sub


Dim BIP
BIP = 0

Sub Plunger_Init()
	PlaySoundAt SoundFX("ballrelease",DOFContactors), Plunger
	'Plunger.CreateBall
'	BallRelease.CreateBall
'	BallRelease.Kick 90, 7
'	BIP = BIP + 1
End Sub


'*****GI Lights On
dim xx

For each xx in GI:xx.State = 1: Next

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("right_slingshot",DOFContactors), RightSlingShot
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	gi1.State = 0:Gi2.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0:gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("left_slingshot",DOFContactors), LeftSlingShot
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	gi3.State = 0:Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
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
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.2, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v4.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 5   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 40 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
    RollingTimer.Enabled = 1
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
'        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
'        aBallShadow(b).X = BOT(b).X
'        aBallShadow(b).Y = BOT(b).Y
'        aBallShadow(b).Height = BOT(b).Z - Ballsize / 2

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 15
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        End If

        ' jps ball speed & spin control
        BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
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

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub
'##################################################
'End of JPSalas Routines
'##################################################

Sub Pins_Hit (idx)
	PlaySoundAtBall "pinhit_low"
End Sub

Sub Targets_Hit (idx)
	PlaySoundAtBall "target"
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySoundAtBall "metalhit_thin"
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySoundAtBall "metalhit_medium"
End Sub

Sub Metals2_Hit (idx)
	PlaySoundAtBall "metalhit2"
End Sub

Sub Gates_Hit (idx)
	PlaySoundAtBall "gate4"
End Sub

Sub Spinner_Spin
	PlaySoundAtBall "fx_spinner"
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySoundAtBall "fx_rubber2"
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySoundAtBall "fx_rubber2"
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtBall "rubber_hit_1"
		Case 2 : PlaySoundAtBall "rubber_hit_2"
		Case 3 : PlaySoundAtBall "rubber_hit_3"
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
		Case 1 : PlaySoundAtBall "flip_hit_1"
		Case 2 : PlaySoundAtBall "flip_hit_2"
		Case 3 : PlaySoundAtBall "flip_hit_3"
	End Select
End Sub

'___________________________________________________________________________
'
'TABLE ELEMENTS
'
'==============================================================================
'



Sub bumper1_hit()
	addscore 50
	bonuscount = bonuscount + 1
	setbonuslights
B1L.state = 0
BLT.enabled = True

lightseq002.Play SeqUpOff, 1,1
	PlaySoundAt SoundFX("FX_BUMPER2",DOFContactors), bumper1
	PlaySoundAtBall "gobell"
end sub

Sub bumper2_hit()
	addscore 50
B2L.state = 0
BLT.enabled = True

lightseq002.Play SeqUpOff, 1,1
	bonuscount = bonuscount + 1
	setbonuslights
	PlaySoundAt SoundFX("FX_BUMPER2",DOFContactors), bumper2
	PlaySoundAtBall "gobell"
end sub

Sub bumper3_hit()
	addscore 50
B3L.state = 0
BLT.enabled = True

lightseq002.Play SeqUpOff, 1,1
	bonuscount = bonuscount + 1
	setbonuslights
	PlaySoundAt SoundFX("FX_BUMPER2",DOFContactors), bumper3
	PlaySoundAtBall "gobell"
end sub

Sub bumper4_hit()
	addscore 50
B4L.state = 0
BLT.enabled = True

lightseq002.Play SeqHatch1HorizOn, 25
	bonuscount = bonuscount + 1
	setbonuslights
	PlaySoundAt SoundFX("FX_BUMPER2",DOFContactors), bumper4
	PlaySoundAtBall "gobell"
end sub
 
Sub BLT_timer()

B1L.state = 1
B2L.state = 1
B3L.state = 1
B4L.state = 1
BLT.enabled = False
end Sub


' DEATH NOTE REIMPORT FOR NEW VPX PORT - This table is built on the VPX DeathNote Port script, based on 
' the same script template used for Meducks, Univorvs, Death Note, and all six Murray Leinster tables.
' Object names in this script reflect items from "The Door Into Summer" which the original vp8 Death Note was based on. 
' Table element scripts below were deleted for the Murray Leinster tables but reimplemented for Death Note and those which followed.


sub Channellightcycle()'to be used for the channels at the top of the table.
	loopbreaker = 0
	if petelightE2.state = 1 and petelightP.state = 0 then
		petelightE2.state = 0
		petelightP.state = 1
		loopbreaker = 1
	end if
	if petelightT.state = 1 and petelightE2.state = 0 and loopbreaker = 0 then
		petelightT.state = 0
		petelightE2.state = 1
		loopbreaker = 1
	end if
	if petelightE1.state = 1 and PeteLightT.state = 0 and loopbreaker = 0 then
		PetelightE1.state = 0
		PetelightT.state = 1
		loopbreaker = 1
	end if
	if PetelightP.state = 1 and PetelightE1.state = 0 and loopbreaker = 0 then
		PetelightP.state = 0
		PetelightE1.state = 1
	end if
	loopbreaker = 0
end sub

 sub lanelightcycle()'to be used for the channels at the top of the table.
	loopbreaker = 0
	if light1.state = 1 and light4.state = 0 then
		light1.state = 0
		light4.state = 1
		loopbreaker = 1
	end if
	if light4.state = 1 and light3.state = 0 and loopbreaker = 0 then
		light4.state = 0
		light3.state = 1
		loopbreaker = 1
	end if
	if light3.state = 1 and Light2.state = 0 and loopbreaker = 0 then
		light3.state = 0
		light2.state = 1
		loopbreaker = 1
	end if
	if light2.state = 1 and light1.state = 0 and loopbreaker = 0 then
		light2.state = 0
		light1.state = 1
	end if
	loopbreaker = 0
end sub
 
sub trapwalls()
	trapreleasetimer.enabled = true
end sub

sub trapreleasetimer_timer()
	trapreleasetimer.enabled = false
		trapt1.isdropped = false
		trapt2.isdropped = false	
		trapt3.isdropped = false
end sub

Sub LeftSlingshot_Slingshot()
	PlaySoundAtBall "Bumper"
	addscore 25
End Sub

Sub RightSlingshot_Slingshot()
	PlaySoundAtBall "Bumper"
	addscore 25
End Sub

sub leftinlane_hit()
	PlaySoundAtBall "swoosh"
	addscore 25
	light2.state = 1
 	checklanes
end sub
sub rightinlane_hit()
	PlaySoundAtBall "swoosh"
	addscore 25
 	light3.state = 1
	checklanes
end sub
sub leftoutlane_hit()
	PlaySoundAtBall "swoosh"
	addscore 25
 	light1.state = 1
	checklanes
end sub
sub rightoutlane_hit()
	PlaySoundAtBall "swoosh"
	addscore 25
 	light4.state = 1
 	checklanes
end sub

sub checklanes
  if light1.state = 1 and light2.state = 1 and light3.state = 1 and light4.state = 1 then
 	PlaySoundAtBall "gobell"
	shootagain.state = 1
 	addscore 500
 	light1.state = 2
 	light2.state = 2
 	light3.state = 2
 	light4.state = 2
 	lanelighttimer.enabled = true
 end if
 end sub
 
 sub lanelighttimer_timer()
 	lanelighttimer.enabled = false
 	light1.state = 0
 	light2.state = 0
 	light3.state = 0
 	light4.state = 0
 end sub
 
Sub bumper5_hit()
	addscore 50
	bonuscount = bonuscount + 1
	setbonuslights
	PlaySoundAtBall "c6"
	PlaySoundAtBall "gobell"
end sub
Sub bumper6_hit()
	addscore 50
	bonuscount = bonuscount + 1
	setbonuslights
	PlaySoundAtBall "c6"
	PlaySoundAtBall "gobell"
end sub
Sub bumper7_hit()
	addscore 50
	bonuscount = bonuscount + 1
	setbonuslights
	PlaySoundAtBall "c6"
	PlaySound "gobell"
end sub
Sub bumper8_hit()
	addscore 50
	bonuscount = bonuscount + 1
	setbonuslights
	PlaySoundAtBall "c6"
	PlaySound "gobell"
end sub
Sub bumper001_hit()
	addscore 50
	bonuscount = bonuscount + 1
	setbonuslights
	PlaySoundAtBall "c6"
	PlaySound "gobell"
end sub
Sub bumper002_hit()
	addscore 50
	bonuscount = bonuscount + 1
	setbonuslights
	PlaySoundAtBall "c6"
	PlaySound "gobell"
end sub

'PETE lights (top channels)

sub trigger1001_hit()
	petelightp.state = 1
	addscore 50
	playsound "swoosh"
	if petelightp.state = 1 and petelighte1.state = 1 and petelightt.state = 1 and petelighte2.state = 1 then
		multiplier = multiplier + 1
		setmulti
peteLightp.state = 2
petelighte1.state = 2
peteLightt.state = 2
peteLighte2.state = 2
		petelighttimer.enabled = true
	end if
end sub

sub trigger2_hit()
	petelightE1.state = 1
	addscore 50
	playsound "swoosh"
	if petelightp.state = 1 and petelighte1.state = 1 and petelightt.state = 1 and petelighte2.state = 1 then
		multiplier = multiplier + 1
		setmulti
peteLightp.state = 2
petelighte1.state = 2
peteLightt.state = 2
peteLighte2.state = 2
		petelighttimer.enabled = true
	end if
end sub

sub trigger3_hit()
	petelightT.state = 1
	addscore 50
	playsound "swoosh"
	if petelightp.state = 1 and petelighte1.state = 1 and petelightt.state = 1 and petelighte2.state = 1 then
		multiplier = multiplier + 1
		setmulti
peteLightp.state = 2
petelighte1.state = 2
peteLightt.state = 2
peteLighte2.state = 2

		petelighttimer.enabled = true
	end if
end sub

sub trigger4_hit()
	petelightE2.state = 1
	if skilllight.state = 1 then
		addscore 950
	end if	
	addscore 50
	playsound "swoosh"

	if petelightp.state = 1 and petelighte1.state = 1 and petelightt.state = 1 and petelighte2.state = 1 then
		multiplier = multiplier + 1
		setmulti

peteLightp.state = 2
petelighte1.state = 2
peteLightt.state = 2
peteLighte2.state = 2
		petelighttimer.enabled = true
	end if
end sub

sub petelighttimer_timer()
	petelighttimer.enabled = false
 petelightp.state = 0
 petelighte1.state = 0
 petelightt.state = 0
 petelighte2.state = 0 
end sub

'end of pete lights

sub shootagaintimer2_timer()
	shootagaintimer.enabled = false
	if shootperm = 0 then 
		shootagain.state = 0
	end if
end sub

' COLDSLEEP target and lights section

'THIS CRIPT IS ADAPTED FROM DEATH NOTE, WHICH WSA ADAPTED FROM DOOR INTO SUMMER (VP8) hence coldsleep targets..
'for this table the 9 coldsleep targets are broken into three banks of 2, 3 and 4.
' LOOT TARGS below!

sub cstargtimer_timer()
	cstargtimer.enabled = false
	PlaySound "targetup"
end sub

sub cstarg1_hit()
	addscore 150
	bonuscount = bonuscount + 1
	setbonuslights
	cstarg1.isdropped = true
	cstargtimer.enabled = true
	PlaySoundAtBall "cashreg"
	PlaySound "targetdropping"
	cslightC.state = 1
	if cslightC.state = 1 and cslightO.state = 1 and cslightL1.state = 1 and cslightD.state = 1 then
		addscore 10000
		if rickylockcount = 1 then
			rickylockrelease
		else
			shootagain.state = 1
			shootagaintimer2.enabled = true
		end if
	cslightC.state = 2
	cslighto.state = 2
	cslightl1.state = 2
	cslightd.state = 2
		cslighttimer.enabled = true
	end if
end sub
sub cstarg2_hit()
	addscore 150
	bonuscount = bonuscount + 1
	setbonuslights
	cstarg2.isdropped = true
	cstargtimer.enabled = true
	PlaySoundAtBall "cashreg"
	PlaySound "targetdropping"
	cslightO.state = 1
	if cslightC.state = 1 and cslightO.state = 1 and cslightL1.state = 1 and cslightD.state = 1 then
		addscore 10000
		if rickylockcount = 1 then
			rickylockrelease
		else
			shootagain.state = 1
			shootagaintimer2.enabled = true
		end if
	cslightC.state = 2
	cslighto.state = 2
	cslightl1.state = 2
	cslightd.state = 2
		cslighttimer.enabled = true
	end if
end sub
sub cstarg3_hit()
	addscore 150
	bonuscount = bonuscount + 1
	setbonuslights
	cstarg3.isdropped = true
	PlaySoundAtBall "cashreg"
	PlaySoundAtBall "targetdropping"
	cstargtimer.enabled = true
	cslightL1.state = 1
	if cslightC.state = 1 and cslightO.state = 1 and cslightL1.state = 1 and cslightD.state = 1 then
		addscore 10000
		if rickylockcount = 1 then
			rickylockrelease
		else
			shootagain.state = 1
			shootagaintimer2.enabled = true
		end if
	cslightC.state = 2
	cslighto.state = 2
	cslightl1.state = 2
	cslightd.state = 2
		cslighttimer.enabled = true
	end if
end sub
sub cstarg4_hit()
	addscore 150
	bonuscount = bonuscount + 1
	setbonuslights
	cstarg4.isdropped = true
	cstargtimer.enabled = true
	PlaySoundAtBall "cashreg"
	PlaySoundAtBall "targetdropping"
	cslightD.state = 1
		if cslightC.state = 1 and cslightO.state = 1 and cslightL1.state = 1 and cslightD.state = 1 then
		addscore 10000
		if rickylockcount = 1 then
			rickylockrelease
		else
			shootagain.state = 1
			shootagaintimer2.enabled = true
		end if
	cslightC.state = 2
	cslighto.state = 2
	cslightl1.state = 2
	cslightd.state = 2
		cslighttimer.enabled = true
	end if
end sub

'THIS CRIPT IS ADAPTED FROM DEATH NOTE, WHICH WSA ADAPTED FROM DOOR INTO SUMMER (VP8) hence coldsleep targets..
'for this table the 9 coldsleep targets are broken into three banks of 2, 3 and 4.
' CB TARGS below!

sub cstarg5_hit()
	addscore 150
	bonuscount = bonuscount + 1
	setbonuslights
	cstarg5.isdropped = true
	cstargtimer.enabled = true
	PlaySoundAtBall "cashreg"
	PlaySound "targetdropping"
	cslightS.state = 1
	if cslightS.state = 1 and cslightL2.state = 1 then
		addscore 5000
	LLANEGATE.ROTATETOEND
	RLANEGATE.ROTATETOEND
	cslights.state = 2
	cslightl2.state = 2
	mdlighttimer.enabled = true
	end if
end sub
sub cstarg6_hit()
	addscore 150
	bonuscount = bonuscount + 1
	setbonuslights
	cstarg6.isdropped = true
	cstargtimer.enabled = true
	PlaySoundAtBall "cashreg"
	PlaySound "targetdropping"
	cslightL2.state = 1
	if cslightS.state = 1 and cslightL2.state = 1 then
		addscore 5000
	LLANEGATE.ROTATETOEND
	RLANEGATE.ROTATETOEND
	cslights.state = 2
	cslightl2.state = 2
	mdlighttimer.enabled = true
	end if
end sub

'THIS CRIPT IS ADAPTED FROM DEATH NOTE, WHICH WSA ADAPTED FROM DOOR INTO SUMMER (VP8) hence coldsleep targets..
'for this table the 9 coldsleep targets are broken into three banks of 2, 3 and 4.
' CAT TARGS below!

sub cstarg7_hit()
	addscore 150
	bonuscount = bonuscount + 1
	setbonuslights
	cstarg7.isdropped = true
	cstargtimer.enabled = true
	PlaySoundAtBall "cashreg"
	PlaySound "targetdropping"
	if cslightE1.state = 0 Then	cslightE1.state = 1
	if cslightE1.state = 1 and cslightE2.state = 1 and cslightP.state = 1 then
		addscore 7500
	cslighte1.state = 2
	cslighte2.state = 2
	cslightp.state = 2
		aelighttimer.enabled = true
	end if
end sub
sub cstarg8_hit()
	addscore 150
	bonuscount = bonuscount + 1
	setbonuslights
	cstarg8.isdropped = true
	cstargtimer.enabled = true
	PlaySoundAtBall "cashreg"
	PlaySoundAtBall "targetdropping"
	if cslightE2.state = 0 Then	cslighte2.state = 1
	if cslightE1.state = 1 and cslightE2.state = 1 and cslightP.state = 1 then
		addscore 7500
	cslighte1.state = 2
	cslighte2.state = 2
	cslightp.state = 2
		aelighttimer.enabled = true
	end if
end sub
sub cstarg9_hit()
	addscore 150
	bonuscount = bonuscount + 1
	setbonuslights
	cstarg9.isdropped = true
	cstargtimer.enabled = true
	PlaySoundAtBall "cashreg"
	PlaySoundAtBall "targetdropping"
	if cslightP.state = 0 Then	cslightp.state = 1
	if cslightE1.state = 1 and cslightE2.state = 1 and cslightP.state = 1 then
		addscore 7500
	cslighte1.state = 2
	cslighte2.state = 2
	cslightp.state = 2
		aelighttimer.enabled = true
	end if
end sub
sub cslighttimer_timer()
	cslighttimer.enabled = false
	cslightC.state = 0
	cslighto.state = 0
	cslightl1.state = 0
	cslightd.state = 0
end sub
sub MDlighttimer_timer()
	MDlighttimer.enabled = false
	cslights.state = 0
	cslightl2.state = 0
	LLANEGATE.ROTATETOSTART
	RLANEGATE.ROTATETOSTART
end sub

sub AElighttimer_timer()
	cslighttimer.enabled = false
	cslighte1.state = 0
	cslighte2.state = 0
	cslightp.state = 0
end sub

'end of COLDSLEEP


' RICKY target and lock codes
'VAULT TARGETS FOR CATBURGLARS USAGE OF ScriptsDirectory

sub rickylockrelease 'note: this is called from the coldsleep section - DUCK TARGETS!
	lockgate.rotatetoend
	rickytarg1.isdropped = true
	rickytarg2.isdropped = true
	rickytarg3.isdropped = true
	rickytarg4.isdropped = true
	rickytarg5.isdropped = true
	rickylock.kick 180,20
	PlaySoundAtBall "kicker"
	rickytargtimer.enabled = true
	multiball = multiball + 1
	rickylockcount = 0
	rickylightR.state = 0
	RickyLighti.state = 0
	RickyLightc.state = 0
	RickyLightk.state = 0
	RickyLightY.state = 0

lightseq001.Play SeqHatch1HorizOn, 25
end sub

sub rickytarg1_hit()
	rickytarg1.isdropped = true
	PlaySoundAtBall "targetdropping"
	PlaySound "cashreg"
	rickylightR.state = 1
	addscore 150
	bonuscount = bonuscount+1
	setbonuslights
	if rickylockcount = 1 then
		rickytargtimer.enabled = true
	end if
lightseq001.Play SeqHatch1HorizOn, 25
if 	rickytarg5.isdropped = true and rickytarg4.isdropped = true and rickytarg3.isdropped = true and rickytarg2.isdropped = true and rickytarg1.isdropped = true Then
lockgate.rotatetoend
	addscore 5000
end If
end sub
sub rickytarg2_hit()
	rickytarg2.isdropped = true
	PlaySoundAtBall "targetdropping"
	PlaySound "cashreg"
	rickylightI.state = 1
	addscore 150
	bonuscount = bonuscount+1
	setbonuslights
lightseq001.Play SeqHatch1HorizOn, 25
if 	rickytarg5.isdropped = true and rickytarg4.isdropped = true and rickytarg3.isdropped = true and rickytarg2.isdropped = true and rickytarg1.isdropped = true Then
lockgate.rotatetoend
	addscore 5000
end If
end sub
sub rickytarg3_hit()
	rickytarg3.isdropped = true
	PlaySoundAtBall "targetdropping"
	PlaySound "cashreg"
	rickylightC.state = 1
	addscore 150
	bonuscount = bonuscount+1
	setbonuslights
lightseq001.Play SeqHatch1HorizOn, 25
if 	rickytarg5.isdropped = true and rickytarg4.isdropped = true and rickytarg3.isdropped = true and rickytarg2.isdropped = true and rickytarg1.isdropped = true Then
lockgate.rotatetoend
	addscore 5000
end If
end sub
sub rickytarg4_hit()
	rickytarg4.isdropped = true
	PlaySoundAtBall "targetdropping"
	PlaySound "cashreg"
	rickylightK.state = 1
	addscore 150
	bonuscount = bonuscount+1
	setbonuslights
lightseq001.Play SeqHatch1HorizOn, 25
if 	rickytarg5.isdropped = true and rickytarg4.isdropped = true and rickytarg3.isdropped = true and rickytarg2.isdropped = true and rickytarg1.isdropped = true Then
lockgate.rotatetoend

	addscore 5000
end If
end sub
sub rickytarg5_hit()
	rickytarg5.isdropped = true
	PlaySoundAtBall "targetdropping"
	PlaySoundAtBall "cashreg"
	rickylightY.state = 1
	addscore 150
	bonuscount = bonuscount+1
	setbonuslights
lightseq001.Play SeqHatch1HorizOn, 25
if 	rickytarg5.isdropped = true and rickytarg4.isdropped = true and rickytarg3.isdropped = true and rickytarg2.isdropped = true and rickytarg1.isdropped = true Then
lockgate.rotatetoend

	addscore 5000
end If
end sub

sub rickylock_hit()
	addscore 2500
lockgate.rotatetostart
	rickytarg1.isdropped = false
	rickytarg2.isdropped = false
	rickytarg3.isdropped = false
	rickytarg4.isdropped = false
	rickytarg5.isdropped = false
		PlaySoundAtBall "targetup"

lightseq001.Play SeqHatch1HorizOn, 25
	rickylockcount = 1
	rickylightR.state = 2
	RickyLighti.state = 2
	RickyLightc.state = 2
	RickyLightk.state = 2
	RickyLightY.state = 2

				'plunger.createball
 				launch.createball
 				launch.kick 45,8
 				PlaySoundAtBall "plunger"
end sub

sub rickytargtimer_timer()
	rickytargtimer.enabled = false
lockgate.rotatetostart
	rickytarg1.isdropped = false
	rickytarg2.isdropped = false
	rickytarg3.isdropped = false
	rickytarg4.isdropped = false
	rickytarg5.isdropped = false
	PlaySound "targetup"
end sub

'end of Ricky

' DR TWITCHELLS CAGE

sub trapt1_hit()
	addscore 250
	PlaySoundAtBall "cashreg"
	PlaySoundAtBall "kicker"
	trapt1.isdropped = true
lightseq001.Play SeqHatch1HorizOn, 25
end sub
sub trapt2_hit()
	addscore 250
	PlaySoundAtBall "cashreg"
	PlaySoundAtBall "kicker"
	trapt2.isdropped = true
lightseq001.Play SeqHatch1HorizOn, 25
end sub
sub trapt3_hit()
	addscore 250
	PlaySoundAtBall "cashreg"
	PlaySoundAtBall "kicker"
	trapt3.isdropped = true
lightseq001.Play SeqHatch1HorizOn, 25
end sub

sub trapkicker_hit()
	addscore 500
	shootagain.state = 1
	shootperm = 1
	trapkicker.kick 180,25
	trapreleasetimer.enabled = true
lightseq001.Play SeqHatch1HorizOn, 25
end sub

sub trapreleasetimer_timer()
	trapreleasetimer.enabled = false
		trapt1.isdropped = false
		trapt2.isdropped = false	
		trapt3.isdropped = false
end sub

'end of Dr Twitchells Cage


'loop codes

sub looptrigger1_hit() 
	PlaySoundAtBall "cashreg"
	if loop1 = 0 then
		addscore 50
		l1timer.enabled = true
		loop1 = 1
		loop1count = 100
		looplight1.state = 1
	else	
		addscore loop1count
		loop1count = loop1count * 2
	end if
lightseq001.Play SeqHatch1HorizOn, 25

'SUPER LOOP MODE - IF A&E TARGETS FLASHING.
IF 	cslightp.state = 2 Then
ADDSCORE 5000
END IF


end sub

sub l1timer_timer()
	l1timer.enabled = false
	looplight1.state = 0
	loop1 = 0
	loop1count = 0
end sub

sub looptrigger2_hit() 
	if skilllight.state = 0 then 'this trigger must be disabled for the skill shot to operate
		PlaySoundAtBall "cashreg"
		if loop2 = 0 then
			addscore 50
			l2timer.enabled = true
			loop2 = 1
			loop2count = 100
			looplight2.state = 1
		else	
			addscore loop2count
			loop2count = loop2count * 2
		end if
	end if
lightseq001.Play SeqHatch1HorizOn, 25
'SUPER LOOP MODE - IF A&E TARGETS FLASHING.
IF 	cslightp.state = 2 Then
ADDSCORE 5000
END IF
end sub

sub l2timer_timer()
	l2timer.enabled = false
	looplight2.state = 0
	loop2 = 0
	loop2count = 0
end sub

sub looptrigger3_hit() 
	PlaySoundAtBall "cashreg"
	if loop3 = 0 then
		addscore 50
		l3timer.enabled = true
		loop3 = 1
		loop3count = 100
		looplight3.state = 1
	else	
		addscore loop1count
		loop3count = loop3count * 2
	end if
lightseq001.Play SeqHatch1HorizOn, 25
'SUPER LOOP MODE - IF A&E TARGETS FLASHING.
IF 	cslightp.state = 2 Then
ADDSCORE 5000
END IF
end sub

sub l3timer_timer()
	l3timer.enabled = false
	looplight3.state = 0
	loop3 = 0
	loop3count = 0
end sub

'end of loop codes
'L trigger lane added for DeathNote
 
 sub trigger5_hit()
 	addscore 50
 	if Light5001.state = 0 then
 	Light5001.state = 1
 	else if Light5001.state = 1 then
 	Light5001.state = 2
 	addscore 450
 	LTimer.enabled = true
 	end if
 	end if
lightseq001.Play SeqHatch1HorizOn, 25
'SUPER LOOP MODE - IF A&E TARGETS FLASHING.
IF 	cslightp.state = 2 Then
ADDSCORE 5000
END IF
 end sub
 
 sub LTimer_timer()
 	LTimer.enabled = false
 	Light5001.state = 0
 end sub

'LANE BUMPERS ADDED FOR CAT BURGLARS


Sub bumper003_hit()
	addscore 500
	bonuscount = bonuscount + 1
	setbonuslights
B1L.state = 0
BLT.enabled = True

lightseq002.Play SeqUpOff, 1,1
	PlaySoundAtBall "FX_BUMPER2"
	PlaySoundAtBall "gobell"
If cslights.state = 2 Then
	addscore 1500
end If
end sub

Sub bumper005_hit()
	addscore 500
	bonuscount = bonuscount + 1
	setbonuslights
B1L.state = 0
BLT.enabled = True

lightseq002.Play SeqUpOff, 1,1
	PlaySoundAtBall "FX_BUMPER2"
	PlaySoundAtBall "gobell"
If cslights.state = 2 Then
	addscore 1500
end If
end sub

Sub bumper004_hit()
	addscore 250
	bonuscount = bonuscount + 1
	setbonuslights
B1L.state = 0
BLT.enabled = True

lightseq002.Play SeqUpOff, 1,1
	PlaySoundAtBall "FX_BUMPER2"
	PlaySoundAtBall "gobell"
If cslights.state = 2 Then
	addscore 750
end If
end sub

Sub bumper002_hit()
	addscore 250
	bonuscount = bonuscount + 1
	setbonuslights
B1L.state = 0
BLT.enabled = True

lightseq002.Play SeqUpOff, 1,1
	PlaySoundAtBall "FX_BUMPER2"
	PlaySoundAtBall "gobell"
If cslights.state = 2 Then
	addscore 750
end If
end sub