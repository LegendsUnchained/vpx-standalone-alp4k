Option Explicit


Dim tablewidth: tablewidth = table1.width
Dim tableheight: tableheight = table1.height

'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = True            ' enable Pinup Player functions for this table
cPuPPack = "HAL9000"    ' name of the PuP-Pack / PuPVideos folder for this table

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
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

' ******* How to use PUPEvent to trigger / control a PuP-Pack *******

' Usage: pupevent(EventNum)

' EventNum = PuP Exxx trigger from the PuP-Pack

' Example: pupevent 102

' This will trigger E102 from the table's PuP-Pack

' DO NOT use any Exxx triggers already used for DOF (if used) to avoid any possible confusion

'************ PuP-Pack Startup **************

PuPStart(cPuPPack) 'Check for PuP - 

DIM B2SSCRIPTS
DIM GameMusicOn
DIM threeballoption
DIM hardwareDOF
B2SSCRIPTS = 1



'******USER SETTINGS ******
ultramode=1 '1 to use UltraDMD, 0 to disable UltraDMD 
GameMusicOn=1 '1 to hear music during the game, 0 to disable music during the game
HardwareDOF=1 '1 to enable hardware DOF, 0 to disable hardware DOF. 
threeballoption=5 '3 for 3 balls, 5 for 5 balls (those are the only two options)
'******END USER SETTINGS ******

'DOF Solenoid Config by Outhere
' 101 Left Flipper
' 102 Right Flipper
' 103 Left Slingshot
' 104 Right Slingshot
' 105 sub LeftSlingShot002_slingshot
' 106 ----
' 107 Bumper Left
' 108 Bumper Right
' 109 Bumper Center
' 110 ----
' 111 drop_target_reset
' 112 drop_target_reset
' 113 drop_target_reset
' 114 Kicker053
' 115 Kicker003
' 116 Kicker026
' 117 Kicker027
' 118 Kicker028
' 119 Kicker029
' 120 RightSlingShot005_Hit
' 121 Sub LeftSlingShot001_Hit
' 122 ----
' 123 BallRelease
' 124 Kicker007
' 125 Kicker007
' 126 Kicker007
' 127 Kicker007
' 128 Kicker007
' 129 Kicker007
' 130 Kicker007
' 131 kicker030
' 132 kicker030
' 133 kicker030
' 134 kicker030
' 135 kicker035
' 136 kicker034
' 137 kicker035
' 138 kicker035
' 139 ----
' 140 kicker001




'*************************************************************************************
' core.vbs constants

Dim B2SBlink
B2SBlink = 1 'enabled flashing lights on b2s
Const BallSize = 50  ' 50 is the normal size
Const BallMass = 1   ' 1 is the normal ball mass.
Const cGameName = "HAL9000"
Dim Controller
' load extra vbs files
LoadCoreFiles
 LoadControllerSub
sub B2STOGGLE()
	if B2SSCRIPTS = 0 then 
		B2SSCRIPTS = 1
	Else
		B2SSCRIPTS = 0
	end If
end Sub

'sub DTMODESELECTOR()
'	if DTMODE = 0 then 
'		DTMODE = 1
'	Else
'		DTMODE = 0
'	end If

'if DTMODE = 0 then

'end If
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
 Controller.B2SName = "HAL9000"  ' 
if HardwareDOF=0 Then
 Controller.Run()
end If
if HardwareDOF=1 Then
   LoadEM
end If
END If
end Sub
Dim X
X=0

Dim Timer032Step
Timer032Step = 0
Dim messageIndex
messageIndex = 0
dim halplay
Dim FirstGame
Dim musicreset
Dim i
Dim q
Dim r
Dim target
Dim Light
Dim P1Score
Dim P2Score
Dim P3Score
Dim P4Score
dim players
Dim BallNum
dim mtargetcount
dim ballsonplayfield
dim ballsinskull
dim ballsinboss
dim allfour
dim kicker023count
dim extraball
dim beatboss
dim multitimes
dim disabled
dim kickback
dim bumps
dim bumpwin
dim hightext
dim HALtext
Dim HighScore1
Dim HighScore(4)
Dim HALScore 'hal score
dim hhhh
dim DMDballnum
Dim ShipStartX, ShipStartY, ShipStartZ
Dim ShipStartRotY
dim flipped
dim firstball
Dim BallSaveQueue
dim kicker039Active
loadhighscores
loadhalscores
hightext = "HIGH SCORE " & txtHigh1.text
HALtext = "HALS HIGH " & txtHALHigh.text
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
'public TableName
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
BallInPlayReel.SetValue(0)
Controller.B2SSetScorePlayer5 "0"

Dim musicVol

if usepup = false then
PlaySound "2001_music", -1
end if

'**************setting defaults for when table loads 
KitOff
primitive011.BlendDisableLighting = 0.3
primitive011.visible = 0
ramp015.collidable=0
ramp016.collidable=0
Ramp017.collidable=0
Ramp019.collidable=0
halplay=0
firstball = 1
disabled=0
extraball=0
allfour = 0
ballsinskull = 0
kicker023count = 0
ballsinboss = 0
ballsonplayfield = 1
musicreset=0
beatboss=0
multitimes=0
kickback=0
bumps=0
bumpwin=0
FirstGame = 1
BallSaveQueue = 0
kicker009.enabled=0
kicker009.timerenabled=0
Kicker010.enabled=0
Kicker010.timerenabled=0
'kicker053.createball
'kicker053.kick 200, 1
Kicker046.enabled=False
target029.isdropped = 1
target030.isdropped = 1
target031.isdropped = 1
target032.isdropped = 1
target044.isdropped = 1
target045.isdropped = 1
target003.isdropped = 1
Target048.isdropped = 1
Trigger016.enabled=0
light008.state=0
light009.state=0
Light068.state=0
Light071.state=0
Light076.state=0
gi8.state=1
gi010.state=0
gi011.state=0
gi012.state=0
gi013.state=0
gi014.state=0
gi015.state=0
gi016.state=0
gi017.state=0
gi020.state=0
gi003.state=0
gi004.state=0
gi005.state=0
Light003.state=0
Timer002.Enabled = False
Timer003.Enabled = False
'Timer004.Enabled = False
'Timer005.Enabled = False
'Timer006.Enabled = False
Timer010.Enabled = False
Timer011.Enabled = False
Timer023.Enabled = False
Timer024.Enabled = False
Timer025.Enabled = False
Timer026.Enabled = False
Timer028.Enabled = False
Timer031.Enabled = False
Timer036.Enabled = False
Timer039.Enabled = False
Timer007.Enabled = False
'Timer040.Enabled = False
Timer041.Enabled = False
Timer042.Enabled = False
Timer043.Enabled = False
Timer044.Enabled = False
timer045.Enabled = False
Timer046.Enabled = False
timer055.enabled = False
Timer001.enabled = False
timer005.enabled = False
Timer004.enabled = False
timer006.enabled = false
fastrelease.enabled=False
ballrelease.timerenabled=False
qtimer.enabled=1

if ultramode=1 then
scoretimer.enabled=1
qtimer.interval=1500
end If
kicker042.timerEnabled=0
Kicker006.timerEnabled=0
Primitive013.visible = False
CheckVisTimer.enabled=1

Light131.state=0
Light132.state=0
Light128.state=0
Light126.state=0
Light125.state=0
Light124.state=0
Light101.state=0
Light077.state=0
Light134.state=0
Light187.state=0
Light188.state=0
Light177.state=0
Light109.state=0
Light174.state=0
Light165.state=0
Light159.state=0
Light158.state=0
Light129.state=0
Light029.state=0

'**************END setting defaults for when table loads 

Sub Table1_Exit()
    On Error Resume Next                ' swallow any DMD plugin edge-cases
    Controller.Stop
    If UltraMode = 1 And IsObject(UltraDMD) Then
        UltraDMD.CancelRendering
        UltraDMD.Uninit
    End If
    On Error GoTo 0
End Sub

Sub kicker019_Hit() 'ball hits a kicker inside the top left skull
consolidated
end Sub


Sub kicker013_Hit() 'ball hits a kicker inside the top left skull
consolidated
end Sub

Sub kicker020_Hit() 'ball hits a kicker inside the top left skull
consolidated
end Sub

Sub kicker021_Hit() 'ball hits a kicker inside the top left skull
consolidated
end Sub


Sub kicker023_Hit() '***the main center kicker that distributes balls to upper left skull
SoundSaucerLock
RecordActivity   
   ResetRolling  
'LightSeq007.UpdateInterval = 10
'    LightSeq007.Play SeqUpOn,5,2
kiton
timer055.enabled=1

addscore 1000
'LightSeq001.UpdateInterval = 10
 'LightSeq001.Play SeqClockRightOn,360,1
'	' as above just faster
 'LightSeq001.UpdateInterval = 2
 'LightSeq001.Play SeqClockRightOn,360,1
'	' turn on all the lights starting in the middle and going anti-clockwise around the table
 'LightSeq001.UpdateInterval = 1
 'LightSeq001.Play SeqClockLeftOn,360,1
'	' as above just faster
 'LightSeq001.UpdateInterval = 10
 'LightSeq001.Play SeqUpOn,5,2
PlaySound SoundFX("pop", DOFContactors)
light003.state=1
kicker023.timerenabled=true
end Sub


Dim EnableRetractPlunger
EnableRetractPlunger = false 'Change to true to enable retracting the plunger at a linear speed; wait for 1 second at the maximum position; move back towards the resting position; nice for button/key plungers


If Table1.ShowDT = false then
    Scoretext.Visible = false
End If


Dim EnableBallControl
EnableBallControl = False 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

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
     Controller.B2SSetScore cp, p1score
     Controller.B2SSetScore 2, dmdballnum
end if'		ScoreText.Text = FormatNumber(score, 0, -1, 0, -1)
	case 2
		emreel2.addvalue(points)
p2score = p2score + points
if B2SSCRIPTS = 1 then
     Controller.B2SSetScore cp, p2score
end if'		ScoreText2.Text = FormatNumber(score, 0, -1, 0, -1)
	case 3
		emreel3.addvalue(points)
p3score = p3score + points
if B2SSCRIPTS = 1 then
     Controller.B2SSetScore cp, p3score
end if'		ScoreText3.Text = FormatNumber(score, 0, -1, 0, -1)
	case 4
		emreel4.addvalue(points)
p4score = p4score + points
if B2SSCRIPTS = 1 then
     Controller.B2SSetScore cp, p4score
end if'		ScoreText4.Text = FormatNumber(score, 0, -1, 0, -1)
	'End if
End Select

End Sub

Sub table1_init()
flipped= 0 
    ShipStartX = Primitive001.TransX
    ShipStartY = Primitive001.TransY
    ShipStartZ = Primitive001.TransZ
ShipStartRotY = Primitive001.RotY
loadhighscores
loadhalscores
hightext = "HIGH SCORE " & txtHALHigh.text 
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
Randomize
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
txtHigh1.text = HighScore1
txtHALHigh.text = HALScore 'hal score
ballstext.text = 0
DMDballnum = 0 
multiplier = 1
bscore = 0
bonuscount = 0
numplay = 2
'lightsall
attractmode
pupevent 902
'light_test


End Sub

Sub getplayers()
'if numplay = "" then numplay = 0
numplay = numplay + 0
if numplay > 5 then numplay = 5
'players.text = numplay - 1
End Sub

Sub assigncp()

select case cp
	case 1
		'score = emreel1.value
	case 2
		'score = emreel2.value
	case 3
		'score = emreel3.value
	case 4
		'score = emreel4.value
end select
End Sub

Sub AddScore(points)
if tilted = 1 Then
else
	setscore points
          'extra ball 
          if p1score > 400000 and extraball=0 then 
Timer001.enabled=True
startb2s(8)   
PicSelect 7
if usePUP=true Then
'POTENTIAL_INSERTPUP - Extra ball awarded here at 400K points
end if
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "EXTRA BALL!", 3000
qtimer.interval=1500
end if
          'playsound "knocker"
          Light014.state=2
          extraball=1
             if ballstext.text = 1 then
             ballstext.text = 999
             else
             if ballstext.text = 2 Then
             ballstext.text = 1
             else
             if ballstext.text = 3 Then
             ballstext.text = 2
             else
             if ballstext.text = 4 Then
             ballstext.text = 3
             else
             if ballstext.text = 5 and threeballoption=5 Then
             ballstext.text = 4
            else
             if ballstext.text = 5 and threeballoption=3 Then
             ballstext.text = 2
            else
          end if
          end if
          end if
          end if
          end if
          end If
          'show message ?
          end if
          'end extra ball
end If
End Sub

Sub Drain_Hit() 'when drain is hit
RandomSoundDrain Drain
addscore 1500
BallsOnPlayfield=BallsOnPlayfield-1
RecordActivity

 If BallsOnPlayfield = 0 Then  'If last ball on playfield drained
StopGameMusic
DropPrimitives
Flasher005.visible=1
Trigger017.enabled=0
stopspinner
StopLightSeq
Light131.state=0
Light132.state=0
Light128.state=0
Light126.state=0
Light125.state=0
Light124.state=0
'Light101.state=0
Light077.state=0
Light134.state=0
Light187.state=0
Light188.state=0
Light177.state=0
Light109.state=0
Light174.state=0
Light165.state=0
Light159.state=0
Light158.state=0
Light129.state=0
Light029.state=0
lightseq010.stopplay
stopsound "spaceflight"
For each target in DropTargets2
         target.isDropped = False
         
Next
 For each target in DropTargets3
         target.isDropped = False
     Next 
 For each target in DropTargets2
         target.isDropped = False
     Next 
 For each target in DropTargets
         target.isDropped = False
     Next 
 For each light in alltargetlights
        light.state = 0
Next
	
Drain.DestroyBall
 '    Dim sounds92, pick92
  '  sounds92 = Array("Drain_1", "Drain_2", "Drain_3", "Drain_4", "Drain_5")
  '  Randomize
  '  pick92 = Int(Rnd * UBound(sounds92) + 1)
  '  PlaySound sounds92(pick92)

	if multiball = 0 then
	tilt = 0
	tilted = 0
	lockcheck = 0
	sasave = 0
	tilttext.text = ""

ballsinskull = 0
ballsinboss = 0
gi004.state=0
gi005.state=0
gi003.state=0
gi010.state=0
gi011.state=0
gi012.state=0
gi013.state=0
gi014.state=0
gi015.state=0
gi016.state=0
gi017.state=0
gi020.state=0
light008.state=0
light009.state=0
light192.state=0
light055.state=0
light058.state=0
light059.state=0
Primitive012.visible = False
Primitive013.visible = False
'Primitive016.visible=0
Kicker046.enabled=False
Kicker043.enabled=False
Kicker051.enabled=False
Kicker004.enabled=False
Kicker052.enabled=False
Target016.IsDropped = 0
Target023.IsDropped = 0
DestroyHalsBalls
flasher007.state=1
flasher004.state=1
target029.isdropped = 1
target030.isdropped = 1
target031.isdropped = 1
target032.isdropped = 1
Trigger016.enabled=0
magnet.visible=0
target044.isdropped = 1
target045.isdropped = 1
gi005.state=0
gi8.state=1
gi003.state=0
gi004.state=0
Trigger004.enabled=1
if halplay=1 Then
kicker025.enabled=1
end If

    Select Case ballstext.text

   		Case "5" 
   'Dim Score : Score = p1score  ' Replace with your actual score variable
if kicker039Active = true then
CanStartGame = False
end if
if kicker039Active = False Then
'Timer040.enabled=True
end if
if kicker039Active = True then
GameDelayTimer.Enabled = True
RightSlingBlocker.collidable = True
end if
if usePUP=true Then
pupevent 804
pupevent 910'POTENTIAL_INSERTPUP - Gave Over, ball 5 lost
end if

FadeOutTimer001.enabled=1 'lowers the boss picture if at boss scene
FirstGame = 0
loadhighscores()
LoadHalScores()
If p1score > HighScore1 and halplay=0 Then
        playsound "knocker"
        SaveHighScores()
        LoadHighScores()
        txtHigh1.text = HighScore1
        Hightext = "HIGH SCORE " & txtHigh1.text
End If
If p1score > HALScore and halplay=1 Then
        playsound "knocker"
        SaveHALScores()
        LoadHalScores()
        txtHALHigh.text = HALScore
        HALtext = "HALS HIGH " & txtHALHigh.text
End If



Timer049.enabled=1 'makes table lighter again 8 second after game ends
attractmode
pupevent 900
if usepup = false then
PlaySound "2001_music", -1
end if
startspinning
DMDballnum = 0
Controller.B2SSetScore 2, dmdballnum
if ultramode=1 then
qtimer.enabled=0
loadhighscores
hightext = "HIGH SCORE " & Highscore1
DMD_DisplaySceneTextWithPause hightext, p1score, 10000
scoretimer.enabled=1
'DMD_DisplaySceneTextWithPause "GAME OVER", p1score, 10000  
'DMD_DisplaySceneTextWithPause "HAL 9000", "FREE PLAY", 10000 
'DMD_DisplaySceneTextWithPause "HAL 9000", HighScore1, 10000 
qtimer.interval=1500 
end if
timer025.enabled=1
'randomize game over sound hals voice soundbite	
if usepup=false Then
randomize
k = int(rnd*6) + 1
select case k
    
case 1 
playsound "end1"
 case 2 
playsound "end2"
case 3
playsound "end3"
case 4
playsound "end4"
case 5 
playsound "end5"
case 6 
playsound "end6"
end select	
end If	
'randomize game over sound hals voice soundbite			
			cp = cp + 1
			assigncp
Controller.B2SSetScorePlayer5 "0"
			if cp = numplay then
				cp = 1
				resetcount=1
				gameon = 0
				numplay = 2
		'		players.text = 0
				ballstext.text = 0
                DMDballnum = 0
				playsound ""
				stopsound ""
				stopsound ""
				'playsound "gameover"
'				lock1open = 0

'add message at end of game that life systems terminated
'add message at end of game that life systems terminated
      'show discovery
        trigger018.enabled=0
        Trigger019.enabled=0
        Trigger020.enabled=0
        Trigger021.enabled=0
        Trigger022.enabled=0
        Trigger023.enabled=0
        Trigger024.enabled=0
        Trigger025.enabled=0
        kicker025.enabled=0
DaveFlasher.visible=0
primitive013.visible = True
Primitive015.visible=0
Primitive014.visible=0
Primitive012.visible=0
Primitive016.visible=1
      'end show discovery
kicker039.enabled=0
kicker040.enabled=0
Kicker041.enabled=0
target043.isdropped=1
kicker039.kick 180, 1
kicker040.kick 180, 1
kicker041.kick 180, 1
Light116.state=0
Light91.state=0
Light120.state=0
Light133.state=0
Kicker040.enabled=false
Kicker041.enabled=false
kicker042.enabled=false
allfour = 0
Kicker009.enabled=0
Kicker010.enabled=0
ramp015.collidable=0
ramp016.collidable=0
Ramp017.collidable=0
Ramp019.collidable=0
light095.state=0
Light096.state=0
Kicker010.enabled=0
Ramp017.collidable=0
Ramp019.collidable=0
light098.state=2
light099.state=2
light100.state=2
light101.state=2
light102.state=2
light103.state=2
light104.state=2
light105.state=2
light106.state=2
			else
				'plunger.createball
if usepup=true Then
ballrelease.timerenabled=1
end if
if usepup=false Then
fastrelease.enabled=1
end if
			end if
halplay=0
   		Case "4"
if usePUP=true Then
pupevent 803'POTENTIAL_INSERTPUP - Ball 4 lost
end if
KitOn
BallInPlayReel.SetValue(5)
Controller.B2SSetScorePlayer5 "5"
			cp = cp + 1
			assigncp
			if cp = numplay then
				cp = 1
				ballstext.text = "5"
                DMDballnum = 5

				assigncp
			end if
				'plunger.createball
if usepup=true Then
ballrelease.timerenabled=1
end if
if usepup=False Then
fastrelease.enabled=1
end If
              'show life systems critical" primitive
                   primitive014.visible = True
                   Primitive015.visible=0
                   Primitive013.visible=0
                   Primitive012.visible=0
                   Primitive016.visible=1
                   Timer025.Enabled = 1
                   playsound "gothim"
					FlashForMs Flasher002, 2000, 100, 0
              'end show life systems critical" primitive
   		Case "3"
 PicSelect 1
if usePUP=true Then
pupevent 802'POTENTIAL_INSERTPUP - Ball 3 lost
end if
KitOn
BallInPlayReel.SetValue(4)
Controller.B2SSetScorePlayer5 "4"
			cp = cp + 1
			assigncp
			if cp = numplay then
				cp = 1
				ballstext.text = "4"
                DMDballnum = 4

				assigncp
			end if
				'plunger.createball
if usepup=true Then
ballrelease.timerenabled=1
end if
if usepup=False Then
fastrelease.enabled=1
end If
  		Case "2"
if usePUP=true Then
pupevent 801'POTENTIAL_INSERTPUP - Ball 2 lost
end if
KitOn
BallInPlayReel.SetValue(3)
Controller.B2SSetScorePlayer5 "3"
			cp = cp + 1
			assigncp
			if cp = numplay then
				cp = 1
if threeballoption=3 then
				ballstext.text = "5"
                DMDballnum = 3
end If
if threeballoption=5 then
				ballstext.text = "3"
                DMDballnum = 3
end If

				assigncp
			end if
				'plunger.createball
if usepup=true Then
ballrelease.timerenabled=1
end if
if usepup=False Then
fastrelease.enabled=1
end If
   		Case "1"
if usePUP=true Then
pupevent 860'POTENTIAL_INSERTPUP - Ball 1 lost
end if
KitOn
if firstgame =1 Then
MooveUpVaisseau
end If
BallInPlayReel.SetValue(2)
Controller.B2SSetScorePlayer5 "2"
			cp = cp + 1
			assigncp
			if cp = numplay then
				cp = 1
				ballstext.text = "2"
                DMDballnum = 2
				assigncp
			end if
				'plunger.createball
BallsOnPlayfield=1
if usepup=true Then
ballrelease.timerenabled=1
end if
if usepup=False Then
fastrelease.enabled=1
end If
   		Case "999" 'added to manage extra ball when achieved on ball one (cant go to ball 0 so used 999 instead)
if usePUP=true Then
pupevent 800'POTENTIAL_INSERTPUP - Ball 1 lost
end if
if firstgame=1 Then
MooveUpVaisseau
end if
BallInPlayReel.SetValue(1)
Controller.B2SSetScorePlayer5 "1"
			cp = cp + 1
			assigncp
			if cp = numplay then
				cp = 1
				ballstext.text = "1"
                DMDballnum = 1
				assigncp
			end if
				'plunger.createball
BallsOnPlayfield=1
if usepup=true Then
ballrelease.timerenabled=1
end if
if usepup=False Then
fastrelease.enabled=1
end If
	   	End Select
	shootperm = 0
	bscore = 0
	playsound "targetup"
	loop1 = 0
	loop1count = 0
	loop2 = 0
	loop2count = 0
	loop3 = 0
	loop3count = 0
	lockcheck = 0
	else
		multiball = multiball - 1
	end if
 Else 
	'There are more balls on the playfield so do nothing
Drain.DestroyBall
    End If
End Sub

sub shootagaintimer_timer()
	shootagaintimer.enabled = false
	shootagain.state = 0
end sub

sub tilttimer_timer()
	if tilt > 0 then
		tilt = tilt - 1
	end if
	
end sub


EnableRetractPlunger = false 'Change to true to enable retracting the plunger at a linear speed; wait for 1 second at the maximum position; move back towards the resting position; nice for button/key plungers


If Table1.ShowDT = false then
emreel1.visible = False
EMReel2.visible = False
EMReel3.visible = False
EMReel4.visible = False
BallInPlayReel.visible = False


    Scoretext.Visible = false
End If


Sub Table1_KeyDown(ByVal keycode)

 If keycode = LeftMagnaSave Then
StopGameMusic
StartGameMusic
   
End If
  
    If keycode = RightMagnaSave And CanStartGame = True and gameon=0 Then
        halgame
        timer067.enabled=1
    End If

	If keycode = PlungerKey Then
        If EnableRetractPlunger Then
            Plunger.PullBackandRetract
        Else
		    Plunger.PullBack
        End If
        SoundPlungerPull
	End If

	If keycode = LeftFlipperKey Then
        FlipperActivate LeftFlipper, LFPress
if gameon = 0 Then
else
   if tilted=1 Then
       else
          if disabled = 1 Then
        else
             SolLFlipper 1      
  end if
   end if
end If
	End If

	If keycode = RightFlipperKey Then
        FlipperActivate RightFlipper, RFPress
if gameon = 0 Then
else
  if tilted=1 Then
else
         if disabled = 1 Then
        else
             SolRFlipper 1
end if
end if
end if
	End If

	if keycode = AddCreditKey and gameon = 0 then
if usePUP=true Then
pupevent 921
'POTENTIAL_INSERTPUP - Coin inserted"
end if
        	getplayers
	    playsound "coin3"
	end if

if keycode = StartGameKey and halplay=1 then	
     if ultramode=1 then
       DMD_DisplaySceneTextWithPause "", "STAND BY...", 3000
       qtimer.enabled=0
     end If
        PicSelect 17
        ballstext.text = "5"
        trigger018.enabled=0
        Trigger019.enabled=0
        Trigger020.enabled=0
        Trigger021.enabled=0
        Trigger022.enabled=0
        Trigger023.enabled=0
        Trigger024.enabled=0
        Trigger025.enabled=0
        kicker025.enabled=0
        tilted = 1
		tilt = 0
		'primitive015.visible=1
        'Primitive016.visible=1
        'Timer025.Enabled = 1
        lightseq010.stopplay
        lightseq010.play SeqAllOff
        timer067.enabled=0
        timer029.enabled=1
end if

	if keycode = StartGameKey and gameon = 0 and numplay > 1 and CanStartGame = True then
      StartGame
	end if

	If keycode = LeftTiltKey Then
		Nudge 90, 2
		tilt = tilt + 1
		if tilt = 3 then
			tilttext.text = "WARNING!!!"
			FlashForMs Flasher002, 2000, 100, 0
		end if
		if tilt = 4 then
if usePUP=true Then
'POTENTIAL_INSERTPUP - Tilt
end if
if halplay=1 then
        ballstext.text = "5"
        trigger018.enabled=0
        Trigger019.enabled=0
        Trigger020.enabled=0
        Trigger021.enabled=0
        Trigger022.enabled=0
        Trigger023.enabled=0
        Trigger024.enabled=0
        Trigger025.enabled=0
        lightseq010.stopplay
        lightseq010.play SeqAllOff
        timer067.enabled=0
end if
			tilted = 1
			tilt = 0
			primitive015.visible=1
            Primitive016.visible=1
            Timer025.Enabled = 1
lightseq010.stopplay
lightseq010.play SeqAllOff
		end if
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
		tilt = tilt + 1
		if tilt = 3 then
			tilttext.text = "WARNING!!!"
			FlashForMs Flasher002, 2000, 100, 0
		end if
		if tilt = 4 then
if usePUP=true Then
'POTENTIAL_INSERTPUP - Tilt
end if
if halplay=1 then
        ballstext.text = "5"
        trigger018.enabled=0
        Trigger019.enabled=0
        Trigger020.enabled=0
        Trigger021.enabled=0
        Trigger022.enabled=0
        Trigger023.enabled=0
        Trigger024.enabled=0
        Trigger025.enabled=0
        lightseq010.stopplay
        lightseq010.play SeqAllOff
        timer067.enabled=0
end if
			tilted = 1
			tilt = 0
			primitive015.visible=1
            Primitive016.visible=1
            Timer025.Enabled = 1
lightseq010.stopplay
lightseq010.play SeqAllOff
		end if
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
		tilt = tilt + 1
		if tilt = 3 then
			tilttext.text = "WARNING!!!"
			FlashForMs Flasher002, 2000, 100, 0
		end if
		if tilt = 4 then
if usePUP=true Then
'POTENTIAL_INSERTPUP - Tilt
end if
if halplay=1 then
        ballstext.text = "5"
        trigger018.enabled=0
        Trigger019.enabled=0
        Trigger020.enabled=0
        Trigger021.enabled=0
        Trigger022.enabled=0
        Trigger023.enabled=0
        Trigger024.enabled=0
        Trigger025.enabled=0
        lightseq010.stopplay
        lightseq010.play SeqAllOff
        timer067.enabled=0
end if
			tilted = 1
			tilt = 0
			primitive015.visible=1
            Primitive016.visible=1
            Timer025.Enabled = 1
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
        SoundPlungerReleaseBall
	End If

	If keycode = LeftFlipperKey Then
        FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
               LeftFlipper002.RotateToStart
		PlaySound SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
	End If

	If keycode = RightFlipperKey Then
        FlipperDeActivate RightFlipper, RFPress
     		RightFlipper.RotateToStart
        RightFlipper001.RotateToStart
		PlaySound SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), 0, 1, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
	End If

    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If
End Sub

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire
        LeftFlipper002.RotateToEnd
        DOF 101, DOFPulse
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			'RandomSoundReflipUpLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper

		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		LeftFlipper.RotateToStart
        LeftFlipper002.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		'FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire
        RightFlipper001.RotateToEnd
        DOF 102, DOFPulse
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			'RandomSoundReflipUpRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		RightFlipper001.RotateToStart
        RightFlipperOn = 0
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		'FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub


Dim BIP
BIP = 0

Sub Plunger_Init()
'SoundPlungerReleaseBall
End Sub


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

sub LeftSlingShot002_slingshot
'PlaySound "left_slingshot"
    ULS.VelocityCorrect(Activeball) 'nFozzy
	RandomSoundSlingshotLeft primitive044
    'PlaySound SoundFXDOF(""), 105, DOFPulse, DOFContactors)
    addscore 100
Light076.State = 1
if not Light131.state=2 then
light146.state=1
light147.state=1
light148.state=1
light149.state=1
light150.state=1
light151.state=1
end if
	Me.TimerEnabled = 1
end Sub

Sub LeftSlingShot002_Timer
 Light076.State = 0
if not Light131.state=2 then
light146.state=0
light147.state=0
light148.state=0
light149.state=0
light150.state=0
light151.state=0
end if
	Me.TimerEnabled = 0
End Sub

Sub RightSlingShot_Slingshot
RecordActivity
    RS.VelocityCorrect(Activeball) 'nFozzy
	RandomSoundSlingshotRight Sling1
    'PlaySound SoundFXDOF("") , 104, DOFPulse, DOFContactors)  
RSling.Visible = 0
    RSling1001.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	gi1.State = 0:Gi2.State = 0
    addscore 100
Light068.State = 1
if not Light131.state=2 then
light129.state=1
light158.state=1
light029.state=1
light159.state=1
light165.state=1
light174.state=1
light177.state=1
light188.state=1
light187.state=1
end If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1001.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0:gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
Light068.State = 0
if not Light131.state=2 then
light129.state=0
light158.state=0
light029.state=0
light159.state=0
light165.state=0
light174.state=0
light177.state=0
light188.state=0
light187.state=0
end if
End Sub

Sub LeftSlingShot_Slingshot
RecordActivity
    LS.VelocityCorrect(Activeball) 'nFozzy
	RandomSoundSlingshotLeft Sling2
    'PlaySound SoundFXDOF(""), 103, DOFPulse, DOFContactors)
    addscore 100    
LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	gi3.State = 0:Gi4.State = 0
    addscore 100
Light071.State = 1
if not Light131.state=2 then
light134.State = 1
light077.State = 1
light109.State = 1
light124.State = 1
light125.State = 1
light126.State = 1
light128.State = 1
light132.State = 1
light131.State = 1
end if
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
Light071.State = 0
if not Light131.state=2 then
light134.State = 0
light077.State = 0
light109.State = 0
light124.State = 0
light125.State = 0
light126.State = 0
light128.State = 0
light132.State = 0
light131.State = 0
end If
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
    Vol = Csng(BallVel(ball) ^2 / 2000)
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
    Controller.B2SSetScore 2, dmdballnum
if musicreset = 1 and beatboss = 0 and firstball = 0 Then
   PlaySound "gyruss_music",-1
    if usepup=false then
       PlaySound "haha"
     end if
LowerPrimitive
StartFadeOut
musicreset = 0
beatboss = 0

Light131.state=0
Light132.state=0
Light128.state=0
Light126.state=0
Light125.state=0
Light124.state=0
Light101.state=0
Light077.state=0
Light134.state=0
Light187.state=0
Light188.state=0
Light177.state=0
Light109.state=0
Light174.state=0
Light165.state=0
Light159.state=0
Light158.state=0
Light129.state=0
Light029.state=0
Else
end if
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

Const tnob = 20 ' total number of balls


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS v2
'*****************************************

'Add TimerEnabled=True to Table1_KeyDown procedure
' Example :
'Sub Table1_KeyDown(ByVal keycode)
'    If keycode = LeftFlipperKey Then
'        LeftFlipper.TimerEnabled = True 'Add this
'        LeftFlipper.RotateToEnd
'    End If
'    If keycode = RightFlipperKey Then
'        RightFlipper.TimerEnabled = True 'And add this
'        RightFlipper.RotateToEnd
'    End If
'End Sub

'Sub LeftFlipper_Init()
'    LeftFlipper.TimerInterval = 10
'End Sub

'Sub RightFlipper_Init()
'    RightFlipper.TimerInterval = 10
'End Sub

'Sub LeftFlipper_Timer()
'    FlipperLSh.RotZ = LeftFlipper.CurrentAngle
'    If LeftFlipper.CurrentAngle = LeftFlipper.StartAngle Then
'        LeftFlipper.TimerEnabled = False
'    End If
'End Sub

'Sub RightFlipper_Timer()
'    FlipperRSh.RotZ = RightFlipper.CurrentAngle
'    If RightFlipper.CurrentAngle = RightFlipper.StartAngle Then
'        RightFlipper.TimerEnabled = False
'    End If
'End Sub

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
        'If BOT(b).X < Table1.Width/2 Then
        '    BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        'Else
        '    BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        'End If
		BallShadow(b).X = BOT(b).X + (BOT(b).X - (Table1.Width/2)) * 1.25 / BallSize
        BallShadow(b).Y = BOT(b).Y + 12
		BallShadow(b).Size_X = 5
		BallShadow(b).Size_Y = 5
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 10 colors: red, orange, amber, yellow...
'******************************************
' in this table this colors are use to keep track of the progress during the acts and battles

'colors
Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white, base, baseLB, baseLH

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
baseLB = 12
baseLH = 13

Sub SetLightColor(n, col, stat)
	Select Case col
		Case red
			n.color = RGB(255, 0, 0)
			n.colorfull = RGB(255, 0, 0)
		Case orange
			n.color = RGB(255, 64, 0)
			n.colorfull = RGB(255, 64, 0)
		Case amber
			n.color = RGB(193, 49, 0)
			n.colorfull = RGB(255, 153, 0)
		Case yellow
			n.color = RGB(18, 18, 0)
			n.colorfull = RGB(255, 255, 0)
		Case darkgreen
			n.color = RGB(0, 8, 0)
			n.colorfull = RGB(0, 64, 0)
		Case green
			n.color = RGB(0, 255, 0)
			n.colorfull = RGB(0, 255, 0)
  		Case blue
			n.color = RGB(0, 18, 18)
			n.colorfull = RGB(0, 255, 255)
		Case darkblue
			n.color = RGB(0, 0, 255)
			n.colorfull = RGB(0, 0, 255)
		Case purple
			n.color = RGB(128, 0, 128)
			n.colorfull = RGB(255, 0, 255)
		Case white
			n.color = RGB(255, 252, 224)
			n.colorfull = RGB(193, 91, 0)
		Case base
			n.color = RGB(255, 197, 143)
			n.colorfull = RGB(255, 255, 236)
		Case baseLB
			n.color = RGB(0, 0, 160)
			n.colorfull = RGB(0, 0, 160)
		Case baseLH
			n.color = RGB(34, 100, 255)
			n.colorfull = RGB(34, 100, 255)
	End Select
	If stat <> -1 Then
		n.State = 0
		n.State = stat
	End If
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

Sub ChangeGi(col) 'changes the gi color (Example "ChangeGi Blue")
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub ChangeThinLight(col) 'changes the ThinLight color (Example "ChangeThinLight Red")
    Dim bulb
    For each bulb in aThinLights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub ChangeBumperLight(col) 'changes the ThinLight color (Example "ChangeBumperLight Red")
    Dim bulb
    For each bulb in aBumperAllLights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GiOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aBumperLights
        bulb.State = 1
    Next
	FlasherHal.visible = 0
'	table1.ColorGradeImage = "ColorGradeLUT256x16_1to2"
End Sub

Sub GiOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aBumperLights
        bulb.State = 0
    Next
	'FlasherHal.visible = 1
'	table1.ColorGradeImage = "ColorGradeLUT256x16_1to3"
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

'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


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
RecordActivity
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
ScrollLightsDiag
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


'kickers

Sub kicker001_Hit() 'the right kicker that sends ball up to top of playfield
SoundSaucerLock
pupevent 920
Light152.state=2
Light153.state=2
Light154.state=2
Light155.state=2
Light156.state=2
Light142.state=2
Light143.state=2
Light144.state=2
Light145.state=2
kicker001.TimerEnabled = 1
PlaySound SoundFX("pop3", DOFDropTargets)
'Light015.state=1
addscore 1000
BallRescueTimer.Enabled = True
End Sub

Sub kicker001_Timer 'the right kicker that sends ball up to top of playfield
Light152.state=0
Light153.state=0
Light154.state=0
Light155.state=0
Light156.state=0
Light142.state=0
Light143.state=0
Light144.state=0
Light145.state=0
'playsound "fx_kicker_low"
PlaySound SoundFXDOF("fx_kicker_low", 140, DOFPulse, DOFContactors)
kicker001.Kick 2, 60
'quickflash
kicker001.TimerEnabled = 0
End Sub

Sub kicker008_Hit() 'kicker in plunger area, i don't recall why it's there or when it get's hit
kicker008.TimerEnabled = 1
End Sub

Sub kicker008_Timer
kicker008.Kick 0, 60
kicker008.TimerEnabled = 0
End Sub

'Gate
Sub Gate_Hit() 
playsound "gate"
addscore 200
End Sub


'*** these are the two small landmine looking slingshots above the main slingshots 

Sub RightSlingShot005_Hit() 
    Dim sounds5, pick5
    sounds5 = Array("sling_r1", "sling_r2", "sling_r3", "sling_r4", "sling_r5", "sling_r6", "sling_r7", "sling_r8")
    Randomize
    pick5 = Int(Rnd * UBound(sounds5) + 1)
    PlaySound SoundFXDOF(sounds5(pick5), 120, DOFPulse, DOFContactors), 0, 0.1  
ShakeRsling
addscore 200
End Sub


Sub LeftSlingShot001_Hit() 
RecordActivity
    Dim sounds5, pick5
    sounds5 = Array("sling_r1", "sling_r2", "sling_r3", "sling_r4", "sling_r5", "sling_r6", "sling_r7", "sling_r8")
    Randomize
    pick5 = Int(Rnd * UBound(sounds5) + 1)
    PlaySound SoundFXDOF(sounds5(pick5), 121, DOFPulse, DOFContactors), 0, 0.1 
ShakeNewLSling
addscore 200
End Sub

'*** END these are the two small landmine looking slingshots above the main slingshots 

' bumpers

Sub bumper006_hit()
RecordActivity
	'playsound "fx_bumper1"
FlashForMs F1A007, 100, 50, 0
RandomSoundBumperTop Bumper006
DOF 108, DOFPulse
addscore 25
	Me.TimerEnabled = 1
BUMP.enabled = True
'try to get 100 bumps
bumps=bumps+1
if ultramode=1 and ballstext.text=1 and bumps = 1 then
DMD_DisplaySceneTextWithPause "TRY FOR 100 BUMPERS", p1score, 10000
qtimer.interval=1500 
else
if bumps > 9 Then
Light098.state=1
if ultramode=1 and bumps = 10 then
DMD_DisplaySceneTextWithPause "10 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 19 Then
Light099.state=1
if ultramode=1 and bumps = 20 then
DMD_DisplaySceneTextWithPause "20 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 29 Then
Light100.state=1
if ultramode=1 and bumps = 30 then
DMD_DisplaySceneTextWithPause "30 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 39 Then
Light101.state=1
if ultramode=1 and bumps = 40 then
DMD_DisplaySceneTextWithPause "40 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 49 Then
Light102.state=1
if ultramode=1 and bumps = 50 then
DMD_DisplaySceneTextWithPause "50 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 59 Then
Light103.state=1
if ultramode=1 and bumps = 60 then
DMD_DisplaySceneTextWithPause "60 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 69 Then
Light104.state=1
if ultramode=1 and bumps = 70 then
DMD_DisplaySceneTextWithPause "70 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 79 Then
Light105.state=1
if ultramode=1 and bumps = 80 then
DMD_DisplaySceneTextWithPause "80 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 89 Then
Light106.state=1
if ultramode=1 and bumps = 90 then
DMD_DisplaySceneTextWithPause "90 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500 
Else
if bumps > 99 and bumpwin=0 Then
playsound "fastping"
startb2s(8)   
bumpwin=1
Light004.state=2  'flash whole table
timer057.enabled=1 'flash whole table
PicSelect 10
'POTENTIAL_INSERTPUP - 100 Bumpers Hit +32000
addscore 32000
Light107.state=1
if bumps > 99 Then
Light107.state=1
if ultramode=1 and bumps = 100 then
DMD_DisplaySceneTextWithPause "100 BUMPERS HIT!!", "+32,000", 10000
qtimer.interval=1500 
Else
end if
end if 
end if 
end if 
end if 
end if 
end if 
end if 
end if 
end if 
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
'end try to get 100 bumps
End Sub

Sub Bumper006_Timer
	Me.Timerenabled = 0
End Sub



Sub bumper004_hit()
RecordActivity
'playsound "fx_bumper1"
FlashForMs F1A005, 100, 50, 0
RandomSoundBumperTop Bumper004
DOF 107, DOFPulse
addscore 25
Light008.State = 1
Me.TimerEnabled = 1
BUMP.enabled = True
'try to get 100 bumps
bumps=bumps+1
if ultramode=1 and ballstext.text=1 and bumps = 1 then
DMD_DisplaySceneTextWithPause "TRY FOR 100 BUMPERS", p1score, 10000
qtimer.interval=1500 
end if 
if bumps > 9 Then
Light098.state=1
if ultramode=1 and bumps = 10 then
DMD_DisplaySceneTextWithPause "10 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 19 Then
Light099.state=1
if ultramode=1 and bumps = 20 then
DMD_DisplaySceneTextWithPause "20 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 29 Then
Light100.state=1
if ultramode=1 and bumps = 30 then
DMD_DisplaySceneTextWithPause "30 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 39 Then
Light101.state=1
if ultramode=1 and bumps = 40 then
DMD_DisplaySceneTextWithPause "40 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 49 Then
Light102.state=1
if ultramode=1 and bumps = 50 then
DMD_DisplaySceneTextWithPause "50 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 59 Then
Light103.state=1
if ultramode=1 and bumps = 60 then
DMD_DisplaySceneTextWithPause "60 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 69 Then
Light104.state=1
if ultramode=1 and bumps = 70 then
DMD_DisplaySceneTextWithPause "70 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 79 Then
Light105.state=1
if ultramode=1 and bumps = 80 then
DMD_DisplaySceneTextWithPause "80 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 89 Then
Light106.state=1
if ultramode=1 and bumps = 90 then
DMD_DisplaySceneTextWithPause "90 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500 
Else
if bumps > 99 and bumpwin=0 Then
playsound "fastping"
startb2s(8)   
bumpwin=1
PicSelect 10
'POTENTIAL_INSERTPUP - 100 Bumpers Hit +32000
addscore 32000
Light107.state=1
if bumps > 99 Then
Light107.state=1
if ultramode=1 and bumps = 100 then
DMD_DisplaySceneTextWithPause "100 BUMPERS HIT!!", "+32,0000", 10000
qtimer.interval=1500 
Else
end if
end if 
end if 
end if 
end if 
end if 
end if 
end if 
end if 
end if 
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
'end try to get 100 bumps

End Sub

Sub Bumper004_Timer
	Light008.State = 0
	Me.Timerenabled = 0
End Sub

Sub bumper002_hit()
RecordActivity
	'playsound "fx_bumper2"
FlashForMs F1A006, 100, 50, 0
RandomSoundBumperBottom Bumper002
DOF 109, DOFPulse 
addscore 25
	Light009.State = 1
	Me.TimerEnabled = 1
BUMP.enabled = True
'try to get 100 bumps
bumps=bumps+1
if ultramode=1 and ballstext.text=1 and bumps = 1 then
DMD_DisplaySceneTextWithPause "TRY FOR 100 BUMPERS", p1score, 10000
qtimer.interval=1500 
else
if bumps > 9 Then
Light098.state=1
if ultramode=1 and bumps = 10 then
DMD_DisplaySceneTextWithPause "10 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 19 Then
Light099.state=1
if ultramode=1 and bumps = 20 then
DMD_DisplaySceneTextWithPause "20 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 29 Then
Light100.state=1
if ultramode=1 and bumps = 30 then
DMD_DisplaySceneTextWithPause "30 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 39 Then
Light101.state=1
if ultramode=1 and bumps = 40 then
DMD_DisplaySceneTextWithPause "40 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 49 Then
Light102.state=1
if ultramode=1 and bumps = 50 then
DMD_DisplaySceneTextWithPause "50 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 59 Then
Light103.state=1
if ultramode=1 and bumps = 60 then
DMD_DisplaySceneTextWithPause "60 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 69 Then
Light104.state=1
if ultramode=1 and bumps = 70 then
DMD_DisplaySceneTextWithPause "70 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 79 Then
Light105.state=1
if ultramode=1 and bumps = 80 then
DMD_DisplaySceneTextWithPause "80 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500
Else 
if bumps > 89 Then
Light106.state=1
if ultramode=1 and bumps = 90 then
DMD_DisplaySceneTextWithPause "90 BUMPERS HIT!", p1score, 10000
qtimer.interval=1500 
Else
if bumps > 99 and bumpwin=0 Then
playsound "fastping"
startb2s(8)   
bumpwin=1
PicSelect 10
'POTENTIAL_INSERTPUP - 100 Bumpers Hit +32000
addscore 32000
Light107.state=1
if bumps > 99 Then
Light107.state=1
if ultramode=1 and bumps = 100 then
DMD_DisplaySceneTextWithPause "100 BUMPERS HIT!!", "+32,000", 10000
qtimer.interval=1500 
Else
end if
end if 
end if 
end if 
end if 
end if 
end if 
end if 
end if 
end if 
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
'end try to get 100 bumps
End Sub

Sub Bumper002_Timer
	Light009.State = 0
	Me.Timerenabled = 0
End Sub

'Spinners


Sub Spinner002_Spin
RecordActivity
PlaySound SoundFX("laser_spin", DOFDropTargets)
AddScore 25
End Sub

Sub Spinner003_Spin
RecordActivity
SoundSpinner Spinner003
AddScore 25
End Sub



' Droptargets

Sub target004_hit 
gi003.state=1
if Light15.state=1 and Light053.state=1 and  Light4.state=1 Then
addscore 1500
SoundDropTargetDrop target004
else
if Light15.state=1 and Light053.state=1 and  Light4.state=0 Then
addscore 1000
SoundDropTargetDrop target004
Light4.state=1
else
if Light15.state=1 and Light053.state=0 then 
addscore 750
SoundDropTargetDrop target004
Light053.state=1
else
if Light15.state=0 then
addscore 500
SoundDropTargetDrop target004
Light15.state=1
end If
end If
end if
end If
end Sub


Sub target013_hit
gi004.state=1
if Light25.state=1 and Light23.state=1 and  Light10.state=1 Then
addscore 1500
SoundDropTargetDrop target013
if Light25.state=1 and Light23.state=1 and  Light10.state=0 Then
addscore 1000
PicSelect 11
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "SHOOT PYRAMID", 3000
qtimer.interval=1500
end if
SoundDropTargetDrop Target013
Light10.state=1
light192.state=2
light018.state=0
else
if Light25.state=1 and Light23.state=0 then 
addscore 750
SoundDropTargetDrop Target013
Light23.state=1
else
if Light25.state=0 then
addscore 500
SoundDropTargetDrop Target013
Light25.state=1
end If
end If
end if
end If
end Sub

Sub Target013_Hit()
    gi004.State = 1

    ' Always play the drop target sound on hit
    SoundDropTargetDrop Target013

    ' Now handle the scoring / light logic
    If Light25.State = 1 And Light23.State = 1 And Light10.State = 1 Then

        ' All three already lit
        AddScore 1500

    ElseIf Light25.State = 1 And Light23.State = 1 And Light10.State = 0 Then

        ' Last one to complete the set
        AddScore 1000
        PicSelect 11

        If UltraMode = 1 Then
            DMD_DisplaySceneTextWithPause "", "SHOOT PYRAMID", 3000
            qtimer.Interval = 1500
        End If

        Light10.State   = 1
        Light192.State  = 2
        Light018.State  = 0

    ElseIf Light25.State = 1 And Light23.State = 0 Then

        ' Light23 next
        AddScore 750
        Light23.State = 1

    ElseIf Light25.State = 0 Then

        ' First one in the sequence
        AddScore 500
        Light25.State = 1

    End If
End Sub


Sub target014_hit
gi005.state=1
if Light26.state=1 and Light12.state=1 and  Light5.state=1 Then
addscore 1500
'PlaySound SoundFX("targ", DOFDropTargets)
SoundDropTargetDrop target014
else
if Light26.state=1 and Light12.state=1 and  Light5.state=0 Then
addscore 1000
SoundDropTargetDrop target014
Light5.state=1
else
if Light26.state=1 and Light12.state=0 then 
addscore 750
SoundDropTargetDrop target014
Light12.state=1
else
if Light26.state=0 then
addscore 500
SoundDropTargetDrop target014
Light26.state=1
end If
end If
end if
end If
end Sub



Sub target005_hit
gi010.state=1
if Light052.state=1 and Light041.state=1 and  Light042.state=1 Then
addscore 1500
SoundDropTargetDrop target005
else
if Light052.state=1 and Light041.state=1 and  Light042.state=0 Then
addscore 1000
SoundDropTargetDrop target005
Light042.state=1
else
if Light052.state=1 and Light041.state=0 then 
addscore 750
SoundDropTargetDrop target005
light041.state=1
else
if Light052.state=0 then
addscore 500
SoundDropTargetDrop target005
Light052.state=1
end If
end If
end if
end If
end Sub


Sub target006_hit
gi011.state=1
if Light043.state=1 and Light044.state=1 and  Light045.state=1 Then
addscore 1500
SoundDropTargetDrop target006
else
if Light043.state=1 and Light044.state=1 and  Light045.state=0 Then
addscore 1000
SoundDropTargetDrop target006
Light045.state=1
else
if Light043.state=1 and Light044.state=0 then 
addscore 750
SoundDropTargetDrop target006
Light044.state=1
else
if Light043.state=0 then
addscore 500
SoundDropTargetDrop target006
Light043.state=1 
end If
end If
end if
end If
end Sub

Sub Target001_Hit()
    gi012.State = 1

    ' Always play the drop target sound
    SoundDropTargetDrop Target001

    If Light046.State = 1 And Light047.State = 1 And Light048.State = 1 Then

        ' All three already lit
        AddScore 1500

    ElseIf Light046.State = 1 And Light047.State = 1 And Light048.State = 0 Then

        ' Light the last one
        AddScore 1000
        Light048.State = 1

    ElseIf Light046.State = 1 And Light047.State = 0 Then

        ' Light the second one + trigger your extra logic
        AddScore 750
        Light047.State = 1

        Light192.State = 2
        Light018.State = 0
        PicSelect 11

        If UltraMode = 1 Then
            DMD_DisplaySceneTextWithPause "", "SHOOT PYRAMID", 3000
            qtimer.Interval = 1500
        End If

    ElseIf Light046.State = 0 Then

        ' First hit: light the first one
        AddScore 500
        Light046.State = 1

    End If
End Sub


Sub target002_hit
gi017.state=1
if Light049.state=1 and Light050.state=1 and  Light051.state=1 Then
addscore 1500
SoundDropTargetDrop target002
else 
if Light049.state=1 and Light050.state=1 and  Light051.state=0 Then
addscore 1000
'PlaySound SoundFX("targ", DOFDropTargets)
SoundDropTargetDrop target002
Light051.state=1
else
if Light049.state=1 and Light050.state=0 then 
addscore 750
SoundDropTargetDrop target002
Light050.state=1
else
if Light049.state=0 then
addscore 500
SoundDropTargetDrop target002
Light049.state=1
end If
end If
end if
end If
end Sub


Sub target016_Hit
SoundDropTargetDrop  target016
if ultramode=1 and DMDBallnum=1 then
DMD_DisplaySceneTextWithPause "", "HIT HAL 4 TIMES", 3000
qtimer.interval=1500
Else 
end if
if ultramode=1 and DMDBallnum=2 then
DMD_DisplaySceneTextWithPause "", "HIT HAL 4 TIMES", 3000
qtimer.interval=1500
Else 
end if
End Sub

Sub target023_Hit
addscore 1000
LightSeq010.Stopplay
LightSeq010.UpdateInterval = 25
	LightSeq010.Play SeqRandom,20,,2000
FlashForMs F1A001, 500, 50, 0
SoundDropTargetDrop  target023
gi020.state=1
End Sub

'Rollovers
Sub RightInlane001_Hit
PlaySound SoundFX("sidelane", DOFDropTargets)
LightsDown
Addscore 100
 LightSeq1.Play SeqBlinking, , 15, 20
End Sub
 
Sub LeftInlane002_Hit
RecordActivity
PlaySound SoundFX("sidelane", DOFDropTargets)
LightsDown
Addscore 100
 LightSeq1.Play SeqBlinking, , 15, 20
End Sub

'Rollovers
Sub RightInlane003_Hit
if halplay=1 Then
trigger021.enabled=0
trigger022.enabled=0
end if
   If ActiveBall.Vely > 0 Then  ' Only slow if moving down
    ActiveBall.Velx = ActiveBall.Velx * 0.6
    ActiveBall.Vely = ActiveBall.Vely * 0.6
end If
PlaySound SoundFX("sound6", DOFDropTargets)
LightsDown
Addscore 100
 LightSeq1.Play SeqBlinking, , 15, 20
End Sub
 
Sub LeftInlane_Hit
   If ActiveBall.Vely > 0 Then  ' Only slow if moving down
    ActiveBall.Velx = ActiveBall.Velx * 0.6
    ActiveBall.Vely = ActiveBall.Vely * 0.6
end if
PlaySound SoundFX("sound6", DOFDropTargets)
LightsDown
Addscore 100
 LightSeq1.Play SeqBlinking, , 15, 20
End Sub



' rotate the tiny red primitives that are present inside the left and center kickers

'****Rsling animation (the two small landmine looking primitives, one on each side, that act act slingshots above main slingshots)

Dim Primitive005Pos

Sub ShakeRsling
    Primitive005Pos = 15
    Timer010.Enabled = 1
	FlashForMs F1A002, 500, 50, 0
End Sub

Sub Timer010_Timer
    Primitive005.TransZ = Primitive005Pos
    If Primitive005Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Primitive005Pos < 0 Then
        Primitive005Pos = ABS(Primitive005Pos) - 1
    Else
        Primitive005Pos = - Primitive005Pos + 1
    End If
End Sub



' **************Lsling animation

Dim Primitive021Pos

Sub ShakeNewLSling
    Primitive021Pos = 15
    Timer011.Enabled = 1
	FlashForMs F1A001, 500, 50, 0
End Sub

Sub Timer011_Timer
    Primitive021.TransZ = Primitive021Pos
    If Primitive021Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Primitive021Pos < 0 Then
        Primitive021Pos = ABS(Primitive021Pos) - 1
    Else
        Primitive021Pos = - Primitive021Pos + 1
    End If
End Sub

' ******skull animation (shakes the top left skull once the fourth ball hits it 

Dim Primitive007Pos

Sub SkullShake
    'Primitive007Pos = 50
    AnimateLights
End Sub


Sub AnimateLights
LightSeq001.UpdateInterval = 10
LightSeq001.Play SeqHatch1HorizOn,50,1
LightSeq001.UpdateInterval = 10
LightSeq001.Play SeqScrewLeftOn,180,1
End Sub

Sub LightsDown
 LightSeq001.UpdateInterval = 10
 LightSeq001.Play SeqDownOn,5,2
End Sub

Sub ScrollLightsRight
    LightSeq001.UpdateInterval = 10
    LightSeq001.Play SeqRightOn,75,2
End Sub

Sub ScrollLightsLeft
    LightSeq001.UpdateInterval = 10
    LightSeq001.Play SeqLeftOn,75,2
End Sub

Sub ScrollLightsDiag
    LightSeq001.UpdateInterval = 5
    LightSeq001.Play SeqDiagDownRightOn,75,1
End Sub

Sub ScrollUp
    LightSeq001.UpdateInterval = 10
    LightSeq001.Play SeqUpOn,75,2
End Sub

sub lightsall
LightSeq007.UpdateInterval = 3
 LightSeq007.Play SeqClockRightOn,360,10
end sub

sub attractmode
pupevent 901
	GiOff
	ResetChangeLight
	' turn on all the lights with a wiper sweep to the right 
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqWiperRightOn,180,1
	' turn on all the lights with a wiper sweep to the left
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqWiperLeftOn,180,1
	' quick right sweep
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqWiperRightOn,20,1
	' back to the left
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqWiperLeftOn,20,1
	' and back to the right again
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqWiperRightOn,20,1

	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqRadarRightOn,20,1
	LightSeq010.UpdateInterval =10
	LightSeq010.Play SeqRadarLeftOn,20,1
	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqRadarRightOn,20,1
	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqRadarLeftOn,20,1
LightSeq010.UpdateInterval = 10
LightSeq010.Play SeqDownOn,5,1
LightSeq010.Play SeqUpOn,5,1
LightSeq010.UpdateInterval = 5
LightSeq010.Play SeqDownOn,5,1
LightSeq010.Play SeqUpOn,5,1
LightSeq010.Play SeqDownOn,5,1
LightSeq010.Play SeqUpOn,5,1
LightSeq010.UpdateInterval = 4
LightSeq010.Play SeqDownOn,5,1
LightSeq010.Play SeqUpOn,5,1
LightSeq010.UpdateInterval = 3
LightSeq010.Play SeqDownOn,5,1
LightSeq010.Play SeqUpOn,5,1
LightSeq010.UpdateInterval = 2
LightSeq010.Play SeqDownOn,5,1
LightSeq010.Play SeqUpOn,5,1
LightSeq010.UpdateInterval = 20
LightSeq010.Play SeqRandom,40,,2000
LightSeq010.UpdateInterval = 5
LightSeq010.Play SeqClockRightOn,360,1
lightseq006.UpdateInterval = 10
lightseq006.Play SeqClockRightOn,360,5
	' blink all the lights 5 times (with 250ms wait between blink)
	LightSeq010.Play SeqBlinking,,5,250
   	' randomly blink 40 lights (per frame) for 4 seconds
	LightSeq010.UpdateInterval = 25
	LightSeq010.Play SeqRandom,40,,4000

    ' scroll up, turning all the lights on with a tail turning them off
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqUpOn,75,2
    ' as about but with a shorter tail (and quicker)
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqUpOn,25,2
    ' scroll down, turning all the lights on with a tail turning them off
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqDownOn,75,2
    ' as about but with a shorter tail (and quicker)
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqDownOn,25,2
    ' scroll right, turning all the lights on with a tail turning them off
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqRightOn,75,2
    ' as above but with a shorter tail (and quicker)
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqRightOn,25,2
    ' scroll left, turning all the lights on with a tail turning them off
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqLeftOn,75,2
    ' as above but with a shorter tail (and quicker)
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqLeftOn,25,2
    ' turn on all the lights starting at the bottom/left and moving diagonaly up
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqDiagUpRightOn,75,1
    ' turn on all the lights starting at the bottom/right and moving diagonaly up
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqDiagUpLeftOn,75,1
    ' turn on all the lights starting at the top/left and moving diagonaly down
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqDiagDownRightOn,75,1
    ' turn on all the lights starting at the top/right and moving diagonaly down
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqDiagDownLeftOn,75,1
    
    ' Turn on all lights starting in the middle and moving outwards to the side edges
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqMiddleOutHorizOn,50,2
    ' Turn on all lights starting on the side edges and moving into the middle
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqMiddleInHorizOn,50,2
    ' Turn on all lights starting in the middle and moving outwards to the side edges
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqMiddleOutVertOn,50,2
	' Turn on all lights starting on the top and bottom edges and moving inwards to the middle
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqMiddleInVertOn,50,2
	' top half of the playfield wipes on to the right while the bottom half wipes on to the left	  
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqStripe1HorizOn,50,1
	' top half of the playfield wipes on to the left while the bottom half wipes on to the right	  
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqStripe2HorizOn,50,1
    ' left side of the playfield wipes on going up while the right side wipes on doing down
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqStripe1VertOn,50,1
	' left side of the playfield wipes on going down while the right side wipes on doing up
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqStripe2VertOn,50,1
	' turn lights on, cross-hatch with even lines going right and odd lines going left
	LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqHatch1HorizOn,50,1
	' turn lights on, cross-hatch with even lines going left and odd lines going right
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqHatch2HorizOn,50,1
	' turn lights on, cross-hatch with even lines going up and odd lines going down
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqHatch1VertOn,50,1
	' turn lights on, cross-hatch with even lines going down and odd lines going up
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqHatch2VertOn,50,1
    
	' turn on all the lights, starting in the table center and circle out
  	LightSeq010.UpdateInterval = 15
    LightSeq010.Play SeqCircleOutOn,50,2
	' turn on all the lights, starting at the table edges and circle in
  	LightSeq010.UpdateInterval = 15
    LightSeq010.Play SeqCircleInOn,50,2
   	' turn on all the lights, starting in the table center and circle out
  	LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqCircleOutOn,5,3
	' turn on all the lights starting in the middle and going clockwise around the table
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqClockRightOn,360,1
	' as above just faster
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqClockRightOn,360,1
	' turn on all the lights starting in the middle and going anti-clockwise around the table
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqClockLeftOn,360,1
	' as above just faster
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqClockLeftOn,360,1
	' turn on all the lights with a radar sweep to the right 
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqRadarRightOn,180,1
	' turn on all the lights with a radar sweep to the left
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqRadarLeftOn,180,1
	' quick right sweep
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqRadarRightOn,20,1
	' back to the left
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqRadarLeftOn,20,1
	' and back to the right again
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqRadarRightOn,20,1

	' turn on all the lights with a wiper sweep to the right 
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqWiperRightOn,180,1
	' turn on all the lights with a wiper sweep to the left
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqWiperLeftOn,180,1
	' quick right sweep
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqWiperRightOn,20,1
	' back to the left
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqWiperLeftOn,20,1
	' and back to the right again
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqWiperRightOn,20,1

	' turn on all the lights with a left fan upwards
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqFanLeftUpOn,180,1
	' turn on all the lights with a left fan downwards
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqFanLeftDownOn,180,1
	' quick right up
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqFanLeftUpOn,20,1
	' back to the down
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqFanLeftDownOn,20,1
	' and back up again
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqFanLeftUpOn,20,1

	' turn on all the lights with a Right fan upwards
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqFanRightUpOn,180,1
	' turn on all the lights with a Right fan downwards
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqFanRightDownOn,180,1
	' quick right up
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqFanRightUpOn,20,1
	' back to the down
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqFanRightDownOn,20,1
	' and back up again
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqFanRightUpOn,20,1

	' turn on all the lights from the bottom left corner and arc up
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqArcBottomLeftUpOn,90,1
	' turn on all the lights from the bottom left corner and arc down
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqArcBottomLeftDownOn,90,1

	' turn on all the lights from the bottom right corner and arc up
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqArcBottomRightUpOn,90,1
	' turn on all the lights from the bottom right corner and arc down
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqArcBottomRightDownOn,90,1

	' turn on all the lights from the top right corner and arc down
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqArcTopRightDownOn,90,1
	' turn on all the lights from the top right corner and arc up
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqArcTopRightUpOn,90,1

	' turn on all the lights from the top left corner and arc down
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqArcTopLeftDownOn,90,1
	' turn on all the lights from the top left corner and arc up
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqArcTopLeftUpOn,90,1

	' turn all the lights on starting in the centre and screwing clockwise
	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqScrewRightOn,180,1

	' turn all the lights on starting in the centre and screwing anti-clockwise
  	LightSeq010.UpdateInterval = 10
	LightSeq010.Play SeqScrewLeftOn,180,1

	' invalid entry test
  	LightSeq010.UpdateInterval = 10

end Sub
 
Sub light_test
'' turn on all the lights starting in the middle and going clockwise around the table
lightseq006.UpdateInterval = 10
lightseq006.Play SeqClockRightOn,360,9



End Sub
 

Sub Target004_Dropped
   CheckDropTargets 'checks to see if entire bank of targets has been dropped, if so they get raised again
   End Sub
Sub Target013_Dropped
   CheckDropTargets
End Sub
Sub Target014_Dropped
   CheckDropTargets
   End Sub
 
Sub CheckDropTargets
i = 0 
   For each target in DropTargets
       i = i + target.isDropped
   Next 
   If i = 3 Then
      AddScore 5000
      For each target in DropTargets
         target.isDropped = False
  Dim sounds12, pick12
    sounds12 = Array("drop_target_reset_1", "drop_target_reset_2", "drop_target_reset_3", "drop_target_reset_4", "drop_target_reset_5", "drop_target_reset_6")
    Randomize
    pick12 = Int(Rnd * UBound(sounds12) + 1)
    'PlaySound sounds12(pick12)
    PlaySound SoundFXDOF(sounds12(pick12), 111, DOFPulse, DOFContactors)
gi003.state=0
gi004.state=0
gi005.state=0
     Next 
    End If
End Sub

Sub Trigger009_Hit() 'the reason for this trigger is to detect activity on table, one of many places, the reason is to lift ramp if the table thinks there is a ball stuck
RecordActivity
end Sub

Sub Trigger004_Hit() 'a new ball was just launched, this trigger was hit so 
startgamemusic
startb2s(8)                  'sets the number of balls on playfield to 1 and turns off shoot again light
RecordActivity
Light131.state=0
Light132.state=0
Light128.state=0
Light126.state=0
Light125.state=0
Light124.state=0
Light101.state=0
Light077.state=0
Light134.state=0
Light187.state=0
Light188.state=0
Light177.state=0
Light109.state=0
Light174.state=0
Light165.state=0
Light159.state=0
Light158.state=0
Light129.state=0
Light029.state=0
Timer001.enabled=False
if usePUP=true and DMDballnum=1 Then
pupevent 811:pupevent 816'POTENTIAL_INSERTPUP - ball 1 launched"
end if
if  usePUP=true and DMDballnum=2 Then
pupevent 812:pupevent 817'POTENTIAL_INSERTPUP - ball 2 launched"
end if
if  usePUP=true and DMDballnum=3 Then
pupevent 813:pupevent 818'POTENTIAL_INSERTPUP - ball 3 launched"
end if
if  usePUP=true and DMDballnum=4 Then
pupevent 814:pupevent 819'POTENTIAL_INSERTPUP - ball 4 launched"
end if
if  usePUP=true and DMDballnum=5 Then
pupevent 815:pupevent 820'POTENTIAL_INSERTPUP - ball 5 launched"
end if
if  DMDballnum=5 Then
PicSelect 4
end If

'if DMDballnum=5 Then
'DaveFlasher.visible=1 'make daves face visible in center of board for ball 5
'end If

addscore 500
MooveUpVaisseau
if ultramode=1 and dmdballnum=1 then
DMD_DisplaySceneTextWithPause "Hello, Dave", p1score, 10000
qtimer.interval=1500 
end if
if dmdballnum=1 then
PicSelect 8
end if
KitOff
 LightSeq001.UpdateInterval = 3
    LightSeq001.Play SeqUpOn,75,2
 ballsonplayfield = 1
shootagain = 1
beatboss = 0 
musicreset = 0
disabled=0
bumps=0
bumpwin=0
firstball = 0
light098.state=0
light099.state=0
light100.state=0
light101.state=0
light102.state=0
light103.state=0
light104.state=0
light105.state=0
light106.state=0
light107.state=0
Ramp006.collidable=1 'enable hidden ramp to bring shoot again ball to the correct kicker below flippers
kicker006.enabled=1 'enable special shoot again Kicker001
light014.state=2 'turn on shoot again Light
timer039.enabled=1 'turns off shot again light and disables shoot again and the makes shoot again ramp non-collidable
BallRescueTimer.Enabled = True
'allfour=1  'only active for testing boss battle
'ballsinskull=3 'only active for testing boss battle
'ballstext.text = 1 'forces ball 5
'primitive011.visible=1
End Sub

Sub kickerskull
kicker019.TimerEnabled = 1
End Sub

Sub kicker019_Timer
DestroyHalsBalls
Target016.IsDropped = 0
Target023.IsDropped = 0
Timer036.enabled = 1
ballsinskull = 0
kicker019.TimerEnabled = 0
End Sub

sub kicker023_timer() '*** this timer sub determines which skull kickere to send the ball to after main center kicker in hit
if kicker023count = 0 Then
light003.state=0
ResetRolling
kicker023.destroyball
ResetRolling
Kicker027.createball
Kicker027.Kick -15, 38
DOF 117, DOFPulse
kicker023count=kicker023count+1
kicker023.timerenabled=False
Else
if kicker023count = 1 Then
light003.state=0
ResetRolling
kicker023.destroyball
ResetRolling
kicker026.createball
Kicker026.Kick -10, 35
DOF 116, DOFPulse
kicker023count=kicker023count+1
kicker023.timerenabled=False
else
if kicker023count = 2 Then
light003.state=0
ResetRolling
kicker023.destroyball
ResetRolling
Kicker028.createball
Kicker028.Kick 10, 38
DOF 118, DOFPulse
kicker023count=kicker023count+1
kicker023.timerenabled=False
Else
if kicker023count = 3 Then
light003.state=0
ResetRolling
kicker023.destroyball
ResetRolling
Kicker029.createball
Kicker029.Kick 10, 50
DOF 119, DOFPulse
kicker023count = 0
kicker023.timerenabled=False
end If
end If
end If
end If
end Sub

sub kicker039_hit()
StartBallControl.enabled=0
startb2s(8)   
PicSelect 9
RecordActivity
if usePUP=true Then
pupevent 821'POTENTIAL_INSERTPUP - Ball 1 locked
end if
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "BALL 1 LOCKED", 3000
qtimer.interval=1500 
end if
qtimer.interval=1500 
LightSeq009.UpdateInterval = 1
LightSeq009.Play SeqClockLeftOn,360,1
LightSeq008.UpdateInterval = 1.2
LightSeq008.Play SeqClockRightOn,360,2
light133.state=1
addscore 1000
kicker039Active = True
kicker040.timerenabled=True
'playsound "balllocked"
    RandomSoundBallRelease BallRelease
FlashForMs Flasher002, 2000, 100, 0
FlashForMs F1A001, 2000, 100, 0
FlashForMs F1A002, 2000, 100, 0
FlashForMs F1A003, 2000, 100, 0
FlashForMs F1A004, 2000, 100, 0
FlashForMs F1A005, 2000, 100, 0
FlashForMs F1A006, 2000, 100, 0
FlashForMs F1A007, 2000, 100, 0
FlashForMs GiFlipLH, 2000, 100, 1
FlashForMs GiFlipRH, 2000, 100, 1
ChangeThinLight green
ChangeGi purple
ChangeBumperLight green
light133.state=1
Timer016.Enabled = 1
End Sub

sub kicker040_timer
kicker040.enabled=True
kicker040.timerenabled=False
end Sub

sub kicker040_hit()
StartBallControl.enabled=0
startb2s(8)   
PicSelect 9
RecordActivity
if usePUP=true Then
pupevent 822'POTENTIAL_INSERTPUP - Ball 2 locked
end if
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "BALL 2 LOCKED", 3000
qtimer.interval=1500 
end if
LightSeq009.UpdateInterval = 1
LightSeq009.Play SeqClockRightOn,360,1
LightSeq008.UpdateInterval = 1.2
LightSeq008.Play SeqClockRightOn,360,2
addscore 1500
light120.state=1
Kicker041.timerenabled=True
'playsound "balllocked"
    RandomSoundBallRelease BallRelease
FlashForMs Flasher002, 2000, 100, 0
FlashForMs F1A001, 2000, 100, 0
FlashForMs F1A002, 2000, 100, 0
FlashForMs F1A003, 2000, 100, 0
FlashForMs F1A004, 2000, 100, 0
FlashForMs F1A005, 2000, 100, 0
FlashForMs F1A006, 2000, 100, 0
FlashForMs F1A007, 2000, 100, 0
FlashForMs GiFlipLH, 2000, 100, 1
FlashForMs GiFlipRH, 2000, 100, 1
ChangeThinLight darkblue
ChangeGi orange
ChangeBumperLight amber
light120.state=1
Timer016.Enabled = 1
end Sub

sub kicker041_timer
Kicker041.enabled=True
Kicker041.timerenabled=False
end Sub


sub kicker041_hit()
StartBallControl.enabled=0
startb2s(8)   
PicSelect 9
RecordActivity
if usePUP=true Then
pupevent 823'POTENTIAL_INSERTPUP - Ball 3 locked
end if
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "BALL 3 LOCKED", 3000
qtimer.interval=1500 
end if
LightSeq009.UpdateInterval = 1
LightSeq009.Play SeqClockLeftOn,360,1
LightSeq008.UpdateInterval = 1.2
LightSeq008.Play SeqClockRightOn,360,2
addscore 2000
light91.state=1
Timer021.enabled=True
'playsound "balllocked"
    RandomSoundBallRelease BallRelease
'playsound "argue"
FlashForMs Flasher002, 2000, 100, 0
FlashForMs F1A001, 2000, 100, 0
FlashForMs F1A002, 2000, 100, 0
FlashForMs F1A003, 2000, 100, 0
FlashForMs F1A004, 2000, 100, 0
FlashForMs F1A005, 2000, 100, 0
FlashForMs F1A006, 2000, 100, 0
FlashForMs F1A007, 2000, 100, 0
FlashForMs GiFlipLH, 2000, 100, 1
FlashForMs GiFlipRH, 2000, 100, 1
ChangeThinLight orange
ChangeGi darkblue
ChangeBumperLight darkblue
light91.state=1
Timer016.Enabled = 1
end Sub

sub timer021_timer
Kicker042.enabled=True
Timer021.enabled=False
end Sub

sub kicker042_hit()
PicSelect 5
startb2s(8)   
Light004.state=2  'flash whole table
timer057.enabled=1 'flash whole table
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "MULTIBALL", 3000
qtimer.interval=1500
end if
LightSeq009.UpdateInterval = 1
LightSeq009.Play SeqClockRightOn,360,1
LightSeq008.UpdateInterval = 1.2
LightSeq008.Play SeqClockRightOn,360,2
light116.state=1
if ballsonplayfield = 1 then
timer048.enabled=0
if usePUP=true Then
pupevent 824'POTENTIAL_INSERTPUP - Begin 5 ball multiball
end if
'playsound "knocker"
playsound "momentdelay"
addscore 2500
LightSeq007.UpdateInterval = 1
LightSeq007.Play SeqBlinking, , 15, 20 
LightSeq007.Play SeqBlinking, , 15, 10
playsound "multi"
FlashForMs Flasher002, 2000, 100, 0
FlashForMs F1A001, 2000, 100, 0
FlashForMs F1A002, 2000, 100, 0
FlashForMs F1A003, 2000, 100, 0
FlashForMs F1A004, 2000, 100, 0
FlashForMs F1A005, 2000, 100, 0
FlashForMs F1A006, 2000, 100, 0
FlashForMs F1A007, 2000, 100, 0
FlashForMs GiFlipLH, 2000, 100, 1
FlashForMs GiFlipRH, 2000, 100, 1
ChangeThinLight red
ChangeGi red
ChangeBumperLight red
light116.state=1
Target043.isdropped=True
kicker039.enabled=False
kicker040.enabled=False
Kicker041.enabled=False
Kicker042.enabled=False
LightSeq1.Play SeqBlinking, , 15, 20
kicker042.timerEnabled=1
timer041.enabled=1
timer042.enabled=1
timer043.enabled=1
timer044.enabled=1
timer045.enabled=1
Else
if usePUP=true Then
pupevent 825'POTENTIAL_INSERTPUP - Begin 4 ball multiball
end if
addscore 2500
LightSeq007.UpdateInterval = 1
LightSeq007.Play SeqBlinking, , 15, 20 
LightSeq007.Play SeqBlinking, , 15, 10
playsound "multi"
FlashForMs Flasher002, 2000, 100, 0
FlashForMs F1A001, 2000, 100, 0
FlashForMs F1A002, 2000, 100, 0
FlashForMs F1A003, 2000, 100, 0
FlashForMs F1A004, 2000, 100, 0
FlashForMs F1A005, 2000, 100, 0
FlashForMs F1A006, 2000, 100, 0
FlashForMs F1A007, 2000, 100, 0
FlashForMs GiFlipLH, 2000, 100, 1
FlashForMs GiFlipRH, 2000, 100, 1
ChangeThinLight red
ChangeGi red
ChangeBumperLight red
light116.state=1
ballsonplayfield = ballsonplayfield+3
Target043.isdropped=True
kicker039.enabled=False
kicker040.enabled=False
Kicker041.enabled=False
Kicker042.enabled=False
kicker039.kick 180, 1
kicker039Active = False
kicker040.kick 180, 1
kicker041.kick 180, 1
kicker042.kick 180, 1
LightSeq1.Play SeqBlinking, , 15, 20
kicker042.timerEnabled=1
multitimes = multitimes + 1
end if
end Sub

sub timer041_timer
Trigger018.enabled=0
trigger019.enabled=0
Trigger020.enabled=0
trigger022.enabled=0
Trigger024.enabled=0
Trigger025.enabled=0
disabled=1
kicker042.destroyball
randomize
k = int(rnd*3) + 1
select case k
    
case 1 
kicker007.createball
Kicker007.kick -5, 33
 case 2
kicker007.createball
Kicker007.kick 4, 33
case 3
kicker007.createball
Kicker007.kick 5, 33
end select
playsound "photon"
DOF 124, DOFPulse
timer041.enabled = 0
timer030.enabled=1
end Sub

sub timer030_timer 'enabled HAL triggers seconds after multiballs launch
if halplay=1 Then
Trigger018.enabled=1
trigger019.enabled=1
Trigger020.enabled=1
trigger022.enabled=1
Trigger024.enabled=1
Trigger025.enabled=1
end if
timer030.enabled=0
end Sub

sub timer042_timer
kicker041.destroyball
randomize
k = int(rnd*3) + 1
select case k
    
case 1 
kicker007.createball
Kicker007.kick -5, 33
 case 2
kicker007.createball
Kicker007.kick 4, 33
case 3
kicker007.createball
Kicker007.kick 5, 33
end select
playsound "photon"
DOF 125, DOFPulse
'timer041.enabled = 0
Timer042.enabled = 0
ballsonplayfield = ballsonplayfield+1
end Sub

sub timer043_timer
kicker040.destroyball
randomize
k = int(rnd*3) + 1
select case k
    
case 1 
kicker007.createball
Kicker007.kick -5, 33
 case 2
kicker007.createball
Kicker007.kick 4, 33
case 3
kicker007.createball
Kicker007.kick 5, 33
end select
playsound "photon"
DOF 126, DOFPulse
'timer041.enabled = 0
Timer043.enabled = 0
ballsonplayfield = ballsonplayfield+1
timer048.enabled=1
end Sub

sub timer044_timer
kicker039.destroyball
randomize
k = int(rnd*3) + 1
select case k
    
case 1 
kicker007.createball
Kicker007.kick -5, 33
 case 2
kicker007.createball
Kicker007.kick 4, 33
case 3
kicker007.createball
Kicker007.kick 5, 33
end select
playsound "photon"
DOF 127, DOFPulse
'timer041.enabled = 0
Timer044.enabled = 0
ballsonplayfield = ballsonplayfield+1
end Sub

sub timer045_timer
disabled=0
randomize
k = int(rnd*3) + 1
select case k
    
case 1 
kicker007.createball
Kicker007.kick -5, 33
 case 2
kicker007.createball
Kicker007.kick 4, 33
case 3
kicker007.createball
Kicker007.kick 5, 33
end select
playsound "photon"
DOF 128, DOFPulse
'timer041.enabled = 0
Timer045.enabled = 0
ballsonplayfield = ballsonplayfield+1
end Sub

sub kicker042_timer()
Target043.isdropped=false
light133.state=0
light120.state=0
light91.state=0
light116.state=0
kicker039.enabled=True
kicker042.timerEnabled=0
light133.state=2 'green
light120.state=2 'green
light91.state=2 'green
light116.state=2 'green

end Sub

sub Kicker043_hit()
kicker043.timerenabled=1
end Sub

sub Kicker043_timer()
kicker043.destroyball
kicker005.createball
kicker005.kick 180, 1
kicker043.timerenabled=0
end Sub

sub Kicker051_hit()
Kicker051.timerenabled=1
end Sub

sub Kicker051_timer()
kicker051.destroyball
kicker022.createball
kicker022.kick 180, 1
Kicker051.timerenabled=0
end Sub

sub Kicker004_hit()
Kicker004.timerenabled=1
end Sub

sub Kicker004_timer()
kicker004.destroyball
kicker045.createball
kicker045.kick 90, 3
Kicker004.timerenabled=0
end Sub

sub Kicker052_hit()
Kicker052.timerenabled=1
end Sub

sub Kicker052_timer()
Kicker052.destroyball
kicker024.createball
kicker024.kick 170, 1
Kicker052.timerenabled=0
end Sub



Sub kicker046_Hit()
LowerBy150()
  Dim sounds865, pick865
    sounds865 = Array("earth1", "earth2", "earth3")
    Randomize
    pick865 = Int(Rnd * UBound(sounds865) + 1)
    PlaySound sounds865(pick865)
'PlaySound SoundFX("kick_ball2", DOFDropTargets)
'PlaySound SoundFX("pop", DOFDropTargets)
kicker046.timerenabled=true
Kicker002.createball 'testing new wall and ball
end Sub

sub kicker046_timer()
if ballsinboss = 0 Then
addscore 5000
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "+5000", 3000
qtimer.interval=1500
end if 
'Kicker002.destroyball 'testing new wall and ball
Kicker046.destroyball
'kicker044.createball
randomkick
ballsinboss=ballsinboss+1
Kicker046.timerenabled=False
'playsound "ouch1"
Else
if ballsinboss = 1 Then
addscore 10000
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "+10,000", 3000
qtimer.interval=1500
end if 
'Kicker002.destroyball 'testing new wall and ball
Kicker046.destroyball
'kicker044.createball
randomkick
ballsinboss=ballsinboss+1
kicker046.timerenabled=False
'playsound "ouch2"
else
if ballsinboss = 2 Then
addscore 20000
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "+20,000", 3000
qtimer.interval=1500
end if 
'Kicker002.destroyball 'testing new wall and ball
Kicker046.destroyball
'kicker044.createball
randomkick
ballsinboss=ballsinboss+1
Kicker046.timerenabled=False
'playsound "ouch3"
Else
if ballsinboss = 3 Then
kicker046.enabled=0
addscore 100000
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "+100,000", 3000
qtimer.interval=1500
end if 
timer027.enabled=1
'Kicker002.destroyball 'testing new wall and ball
'Kicker046.destroyball
ballsinboss=0
kicker046.timerenabled=False
BossShake
exitboss
end If
end If
end If
end If
end Sub

sub exitboss 'the final boss has been defeated, this switches playfield back to normal
Kicker046.enabled=false
Kicker043.enabled=false
Kicker052.enabled=false
Kicker051.enabled=false
Kicker004.enabled=false
DropPrimitives
if usePUP=true Then
pupevent 826'POTENTIAL_INSERTPUP - Monolith defeated, very long delay here before play starts again
end if
if usePUP=false Then
playsound "noooo"
end if
beatboss = 1
'StopSound "fire"
StopSound "spaceflight"
StopSound "2001_music"
StartGameMusic
Trigger017.enabled=0
Light131.state=0
Light132.state=0
Light128.state=0
Light126.state=0
Light125.state=0
Light124.state=0
Light101.state=0
Light077.state=0
Light134.state=0
Light187.state=0
Light188.state=0
Light177.state=0
Light109.state=0
Light174.state=0
Light165.state=0
Light159.state=0
Light158.state=0
Light129.state=0
Light029.state=0
StopLightSeq
Timer052.enabled=True
timer023.enabled = 1
end Sub

sub Timer052_timer()
DestroyHalsBalls
Kicker046.enabled=false
Kicker043.enabled=false
Kicker052.enabled=false
Kicker051.enabled=false
Kicker004.enabled=false
Primitive030.visible = False
Primitive041.visible = False
Ramp009.visible=0
Target016.isdropped = 0
target029.isdropped = 1
target030.isdropped = 1
target031.isdropped = 1
target032.isdropped = 1
Trigger016.enabled=0
Primitive025.visible = 0
LowerPrimitive
StartFadeOut
StartFadeOut
magnet.visible=0
Timer026.Enabled = 1
Timer052.enabled=False
target044.isdropped = 1
target045.isdropped = 1
ballsinskull = 0

End Sub

Dim Primitive025Pos

Sub BossShake 'shakes the boss primitive
    Primitive025Pos = 3
    Timer015.Enabled = 1
    AnimateLights
End Sub

Sub Timer015_Timer
    Primitive025.TransX = Primitive025Pos
    If Primitive025Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Primitive025Pos < 0 Then
        Primitive025Pos = ABS(Primitive025Pos) - 1
    Else
        Primitive025Pos = - Primitive025Pos + 1
    End If
End Sub
'*************************************************************


Sub Target039_Hit()
    RecordActivity
    FlashForMs FlasherHal, 900, 50, 0
    gi013.State = 1

    ' Always play drop target sound on hit
    SoundDropTargetDrop Target039

    If Light006.State = 1 And Light023.State = 1 And Light040.State = 1 Then

        ' All three already lit
        AddScore 1500

    ElseIf Light006.State = 1 And Light023.State = 1 And Light040.State = 0 Then

        ' Light the last one
        AddScore 1000
        Light040.State = 1

    ElseIf Light023.State = 1 And Light006.State = 0 Then

        ' Second light in the sequence
        AddScore 750
        Light006.State = 1

    ElseIf Light023.State = 0 Then

        ' First light in the sequence
        AddScore 500
        Light023.State = 1

    End If
End Sub

'********************************************************

Sub target040_hit
RecordActivity
FlashForMs FlasherHal, 900, 50, 0
gi015.state=1
if Light025.state=1 and Light026.state=1 and  Light027.state=1 Then
addscore 1500
SoundDropTargetDrop target040
else

if light025.state=1 and light026.state=1 and  Light027.state=0 Then
addscore 1000
SoundDropTargetDrop target040
Light027.state=1
else

if Light025.state=1 and Light026.state=0 then 
addscore 750
PicSelect 11
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "SHOOT PYRAMID", 3000
qtimer.interval=1500
end if
SoundDropTargetDrop target040
Light026.state=1
light192.state=2
light018.state=0
else

if Light025.state=0 then
addscore 500
SoundDropTargetDrop target040
Light025.state=1
end If
end If
end if
end If
end Sub

'**************************************************

Sub target041_hit
RecordActivity
FlashForMs FlasherHal, 900, 50, 0
gi016.state=1

if light034.state=1 and light031.state=1 and  light030.state=1 Then
addscore 1500
SoundDropTargetDrop target041
light030.state=1
else

if light034.state=1 and light031.state=1 and  light030.state=0 Then
addscore 1000
SoundDropTargetDrop target041
light030.state=1
else

if light034.state=1 and light031.state=0 then 
addscore 750
SoundDropTargetDrop target041
Light031.state=1
else

if Light034.state=0 then
addscore 500
SoundDropTargetDrop target041
Light034.state=1
end If
end If
end if
end if
end Sub

'****************************************************************************
Sub target042_hit
RecordActivity
FlashForMs FlasherHal, 900, 50, 0
gi014.state=1
if Light021.state=1 and Light022.state=1 and Light024.state=1 Then
addscore 1500
'PlaySound SoundFX("targ", DOFDropTargets)
SoundDropTargetDrop target042
else

if Light021.state=1 and Light022.state=1 and  Light024.state=0 Then
addscore 1000
SoundDropTargetDrop target042
Light024.state=1
else

if Light021.state=1 and Light022.state=0 then 
addscore 750
SoundDropTargetDrop target042
Light022.state=1
else

if Light021.state=0 then
addscore 500
SoundDropTargetDrop target042
Light021.state=1
end If
end If
end if
end If
end Sub


' *************************************************************************
Sub Target039_Dropped
   CheckDropTargets3
End Sub
Sub Target040_Dropped
   CheckDropTargets3
End Sub
Sub Target041_Dropped
   CheckDropTargets3
End Sub
 Sub Target042_Dropped
   CheckDropTargets3
End Sub

Sub CheckDropTargets3
   q = 0 
   For each target in DropTargets3
       q = q + target.isDropped
   Next 
   If q = 4 Then
      AddScore 5000
      For each target in DropTargets3
         target.isDropped = False
  Dim sounds12, pick12
    sounds12 = Array("drop_target_reset_1", "drop_target_reset_2", "drop_target_reset_3", "drop_target_reset_4", "drop_target_reset_5", "drop_target_reset_6")
    Randomize
    pick12 = Int(Rnd * UBound(sounds12) + 1)
   ' PlaySound sounds12(pick12)
    PlaySound SoundFXDOF(sounds12(pick12), 112, DOFPulse, DOFContactors)
gi013.state=0
gi014.state=0
gi015.state=0
gi016.state=0
     Next 
   '  TurnOnTargetLights
   End If
End Sub

Sub Target001_Dropped
   CheckDropTargets2
	FlashForMs F1A004, 500, 50, 0
End Sub
Sub Target002_Dropped
   CheckDropTargets2
	FlashForMs F1A004, 500, 50, 0
End Sub
Sub Target005_Dropped
   CheckDropTargets2
	FlashForMs F1A004, 500, 50, 0
End Sub
 Sub Target006_Dropped
   CheckDropTargets2
	FlashForMs F1A004, 500, 50, 0
	
End Sub

Sub CheckDropTargets2
   r = 0 
   For each target in DropTargets2
       r = r + target.isDropped
   Next 
   If r = 4 Then
      AddScore 5000
		FlashForMs F1A003, 1000, 250, 0
		FlashForMs F1A004, 1000, 250, 0
      For each target in DropTargets2
         target.isDropped = False
  Dim sounds12, pick12
    sounds12 = Array("drop_target_reset_1", "drop_target_reset_2", "drop_target_reset_3", "drop_target_reset_4", "drop_target_reset_5", "drop_target_reset_6")
    Randomize
    pick12 = Int(Rnd * UBound(sounds12) + 1)
   ' PlaySound sounds12(pick12)
    PlaySound SoundFXDOF(sounds12(pick12), 113, DOFPulse, DOFContactors)
gi010.state=0
gi011.state=0
gi012.state=0
gi017.state=0
     Next 
   '  TurnOnTargetLights
   End If
End Sub



sub Target047_hit
LightSeq007.UpdateInterval = 10
LightSeq007.Play SeqBlinking,, 5, 20
addscore 750
end Sub

sub kicker054_hit
light016.state=2
PlaySound "rampsound"
ramp007.visible=1
ramp007.collidable=1
ramp008.visible=0
'light062.state=0
lipass001.state=0
lipass002.state=0
if ballsonplayfield=1 then
startb2s(8)   
PicSelect 6
if usePUP=true Then
pupevent 827'POTENTIAL_INSERTPUP - Begin 2 ball multiball
end if
addscore 1500
PlaySound SoundFX("multiball", DOFDropTargets)
kicker054.destroyball
kicker054.createball
kicker055.createball
kicker054.kick -30, 30
kicker055.kick -30, 30
FlashForMs Flasher002, 2000, 100, 0
FlashForMs F1A001, 2000, 100, 0
FlashForMs F1A002, 2000, 100, 0
FlashForMs F1A003, 2000, 100, 0
FlashForMs F1A004, 2000, 100, 0
FlashForMs F1A005, 2000, 100, 0
FlashForMs F1A006, 2000, 100, 0
FlashForMs F1A007, 2000, 100, 0
FlashForMs GiFlipLH, 2000, 100, 1
FlashForMs GiFlipRH, 2000, 100, 1
ballsonplayfield=ballsonplayfield+1
Else
kicker054.destroyball
kicker054.createball
kicker054.kick -30, 30
end if
end sub

dim k


Sub Kicker035_hit() 'left kicker in "special" room, it randomly does one of four things with the ball once hit
ResetRolling 
addscore 1500
SoundSaucerLock
'randomize noise
randomize
k = int(rnd*4) + 1
select case k
    
case 1 'lowers a droptarget and shoots the ball into it
'light007.state=1
Flasher010.visible = True
PlaySound SoundFX("lower2", DOFDropTargets)
timer009.enabled=1
 case 2 'creates the ball on hidden ramp, launches it, displays a flasher that says computer malfunction
if usePUP=true Then
pupevent 828'POTENTIAL_INSERTPUP - "Computer Malfunction"
end if
addscore 4000
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "+4000", 3000
qtimer.interval=1500
end if 
	' quick right sweep
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqWiperRightOn,20,1
	' back to the left
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqWiperLeftOn,20,1
	' and back to the right again
  	LightSeq010.UpdateInterval = 5
	LightSeq010.Play SeqWiperRightOn,20,1
Primitive012.visible = True
Primitive015.visible=0
Primitive013.visible=0
Primitive014.visible=0
Primitive016.visible=1
Primitive016.visible=1
PlaySound SoundFX("hyperspace", DOFDropTargets)
PlaySound SoundFX("ping", DOFDropTargets)
timer013.enabled=1
case 3 'lowers a droptarget and shoots the ball into it
timer018.enabled=1
Flasher009.visible = True
PlaySound SoundFX("lower2", DOFDropTargets)
case 4 'flasher displays intermisison
Playsound "readyou"
if usePUP=true Then
pupevent 829'POTENTIAL_INSERTPUP - "intermission"
end if
addscore 4000
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "+4000", 3000
qtimer.interval=1500
end if
'if ballsonplayfield=1 then
PicSelect 13
'CheckVisTimer.enabled=1
PlaySound SoundFX("wormhole", DOFDropTargets)
Timer020.enabled=1
end select
end sub

Sub timer009_timer()
Target048.isdropped = 0
Timer012.enabled=1
timer009.enabled=0
end sub

Sub timer012_timer()
    SoundSaucerKick 1, kicker035
DOF 135, DOFPulse
DOF DOFContactors, DOFPulse
'PlaySound SoundFXDOF("fx_kicker", 135, DOFPulse, DOFContactors)
kicker035.kick 141, 50
timer012.enabled=0
end Sub

Sub timer013_timer()
'playsound announcing hyperspace launch
Timer014.enabled=1
timer013.enabled=0
end Sub

Sub timer014_timer()
'PlaySound SoundFXDOF("fx_kicker", 136, DOFPulse, DOFContactors)
    SoundSaucerKick 1, kicker035
DOF 136, DOFPulse
DOF DOFContactors, DOFPulse
kicker035.destroyball
kicker034.createball
kicker034.kick -180, 80
Timer014.enabled=0
end Sub


Sub timer018_timer()
Target049.isdropped = 0
Timer019.enabled=1
timer018.enabled=0
end sub

Sub Timer019_timer()
    SoundSaucerKick 1, kicker035
DOF 137, DOFPulse
DOF DOFContactors, DOFPulse
'PlaySound SoundFXDOF("fx_kicker", 137, DOFPulse, DOFContactors)
kicker035.kick 119, 55
Timer019.enabled=0
end Sub

Sub kicker062_timer()
kicker062.timerenabled=0
end Sub


Sub Timer020_timer()
'PlaySound SoundFXDOF("fx_kicker", 138, DOFPulse, DOFContactors)
    SoundSaucerKick 1, kicker035
DOF 138, DOFPulse
DOF DOFContactors, DOFPulse
kicker035.kick 120, 25
IntermissionFlasher.visible=0
Timer020.enabled=0
end sub


sub trigger011_hit 'located on a hidden ramp, this trigger hides the flasher that says "hyperspace" 
Primitive012.visible = False
CheckVisTimer.enabled=1
end Sub

sub target048_hit
SoundDropTargetDrop target048
Flasher010.visible = False
light007.state=0
end Sub

sub target049_hit
SoundDropTargetDrop target049
Flasher009.visible = False
end Sub

sub trigger001_hit
RecordActivity
addscore 750
PlaySound SoundFX("whir", DOFDropTargets)
LightSeq1.Play SeqBlinking, , 15, 20
end sub

sub trigger002_hit
RecordActivity
if ballsonplayfield=1 Then
addscore 500
PlaySound SoundFX("died", DOFDropTargets)
Else
end If
end sub

sub trigger012_hit 'located on pyramid shaped light on playfield, adds poins when the corresponding light is hit

if light192.state=2 and light055.state=0 Then
if usePUP=true Then
pupevent 829'POTENTIAL_INSERTPUP - Pyramid shaped light hit for 10k
end if
addscore 10000
if ultramode=1 and light192.state=2 then
DMD_DisplaySceneTextWithPause "", "+10,000", 3000
qtimer.interval=1500
end if
PlaySound SoundFX("gothim", DOFDropTargets)
light192.state=0
light018.state=1
light055.state=1
LightSeq1.Play SeqBlinking, , 15, 20
Else

if light192.state=2 and light055.state=1 and light058.state=0 Then
if usePUP=true Then
pupevent 830'POTENTIAL_INSERTPUP - Pyramid shaped light hit for 25k
end if
addscore 25000
if ultramode=1 and light192.state=2 then
DMD_DisplaySceneTextWithPause "", "+25,000", 3000
qtimer.interval=1500
end if
PlaySound SoundFX("gothim", DOFDropTargets)
light192.state=0
light018.state=1
light058.state=1
LightSeq1.Play SeqBlinking, , 15, 20
Else

if light192.state=2 and light055.state=1 and light058.state=1 and light059.state=0 Then
if usePUP=true Then
pupevent 831'POTENTIAL_INSERTPUP - Pyramid shaped light hit for 75k
end if
addscore 75000
if ultramode=1 and light192.state=2 then
DMD_DisplaySceneTextWithPause "", "+75,000", 3000
qtimer.interval=1500
end if
PlaySound SoundFX("gothim", DOFDropTargets)
light192.state=0
light018.state=1
light059.state=1
LightSeq1.Play SeqBlinking, , 15, 20
Else

if light192.state=2 and light055.state=1 and light058.state=1 and light059.state=1 Then
addscore 500
PlaySound SoundFX("gothim", DOFDropTargets)
light192.state=0
light018.state=1
LightSeq1.Play SeqBlinking, , 15, 20
Else
end If
end If
end if
end If
end sub

Sub BUMP_timer()
'Bump3L.state = 1
BUMP.enabled = False
end Sub


sub kicker030_timer() 'when a multiball is locked, or a ball becomes stationary in the skull, this kicker creates and launches that ball's replacement
kicker030.createball
kicker030.kick 80, 15
playsound "photon"
DOF 131, DOFPulse
'playsound "fx_kicker"
kicker030.timerenabled=0
'Light004.state=1
'timer017.enabled= true
end sub

sub Timer026_timer() 'after defeating monolith, pauses before launching ball (or multiball in > 1 ball on playfield) until after bed image disappears
pupevent 832'Kicker006. kick 10, 40 

if ballsonplayfield = 1 Then
' Timer026.Enabled = 1 'redundant?
Kicker007.createball
randomize
k = int(rnd*3) + 1
select case k
    
case 1 
Kicker007.kick -5, 33
 case 2 
Kicker007.kick 4, 33
case 3
Kicker007.kick 5, 33
end select
playsound "photon"
DOF 129, DOFPulse
end If
if ballsonplayfield > 1 Then
timer008.enabled=1
end If
Timer026.enabled=false
end sub

'sub timer017_timer()
'Light004.state=0
'timer017.enabled= False
'end Sub

sub trigger003_hit 'located at the end of the wire ramp, this trigger animates the explosion and the primitive disappears as if it was blown up
'playsound "wireramp_stop1"
addscore 3000
end Sub


'****lift ramp alternates between being lifted to allow for two-ball multiball acess
ramp007.visible=1
ramp008.visible=0
'light062.state=0
lipass001.state=0
lipass002.state=0

sub trigger007_hit 'changes if the ramp is lifted or not (trapdoor)
target003.isdropped = 0
if LiPass001.state=0 Then
LiPass001.state=2
LiPass002.state=2
ramp007.visible=False
ramp007.collidable=0
ramp008.visible=1
'light062.state=1
PlaySound "rampsound"
Else
end if
end Sub

sub Trigger006_hit 'lowers invisible ramp target once ball is over the hump
'Robottaunt
FlashForMs F1A003, 500, 50, 0
light016.state=0
target003.isdropped = 1
ramp007.collidable=0
end sub

sub Trigger005_hit '(per trigger006, ths one makes sure the target is down!
'Robottaunt
target003.isdropped = 1
end sub




sub trigger008_hit
	If activeball.vely < 0 Then
        WireRampOff
		WireRampOn False
	Else
		WireRampOn True
	End If
target003.isdropped = 1
if lipass001.state=2 Then
PlaySound "rampsound"
ramp007.visible=1
ramp007.collidable=1
ramp008.visible=0
lipass001.state=0
lipass002.state=0
Else
end If
end Sub


Sub kicker003_Hit()
SoundSaucerLock
startb2s(8)   
Light146.state=2
Light147.state=2
Light148.state=2
Light149.state=2
Light150.state=2
Light151.state=2
if usePUP=true Then
pupevent 833'POTENTIAL_INSERTPUP - Hit the emergency airlock kicker (this can be set to trigger the even 1 out of 4 times becasue it happens so often)
end if
LightSeq007.UpdateInterval = 1
LightSeq007.Play SeqBlinking, , 15, 20 

PicSelect 12
playsound "fx_enter"
'playsound "kicker_enter_center"
playsound "sound5"
Kicker003.TimerEnabled = 1
'PlaySound SoundFX("pop3", DOFDropTargets)
'Light015.state=1
addscore 1000
kicker003.Enabled=0
End Sub

Sub kicker003_Timer
Light146.state=0
Light147.state=0
Light148.state=0
Light149.state=0
Light150.state=0
Light151.state=0
'PlaySound SoundFXDOF("fx_kicker", 115, DOFPulse, DOFContactors)
    SoundSaucerKick 1, kicker003
DOF 115, DOFPulse
DOF DOFContactors, DOFPulse
'Primitive016.visible=0
CheckVisTimer.enabled=1
Kicker003.Kick 109, 44
'quickflash
Kicker003.timerEnabled = 0
kicker003.enabled=0
Timer006.enabled=1
End Sub

sub Timer023_timer

Light131.state=0
Light132.state=0
Light128.state=0
Light126.state=0
Light125.state=0
Light124.state=0
'Light101.state=0
Light077.state=0
Light134.state=0
Light187.state=0
Light188.state=0
Light177.state=0
Light109.state=0
Light174.state=0
Light165.state=0
Light159.state=0
Light158.state=0
Light129.state=0
Light029.state=0
if usePUP=0 then
primitive011.visible=1
end if
timer024.Enabled = 1
timer023.Enabled = 0
end Sub

sub Timer024_timer
primitive011.visible=0
timer024.Enabled = 0
end Sub

sub Timer025_timer
primitive013.visible = False
primitive014.visible = False
Primitive015.visible = False
CheckVisTimer.enabled=1
Timer025.Enabled = 0
end Sub


'the following 11 timers (28-38)were added to prevent createball 
'kicker in top right from getting overloaded during multiball
'when a ball hits the mutiball and skull at same 

sub Timer028_timer
kicker030.createball
kicker030.kick 80, 15
playsound "photon"
DOF 132, DOFPulse
'playsound "fx_kicker"
'Light004.state=1
'timer017.enabled= true
Timer028.Enabled = 0
end Sub


sub Timer031_timer
kicker030.createball
kicker030.kick 80, 15
playsound "photon"
DOF 133, DOFPulse
'playsound "fx_kicker"
'Light004.state=1
'timer017.enabled= true
Timer031.Enabled = 0
end Sub

sub Timer036_timer
kicker030.createball
kicker030.kick 80, 15
playsound "photon"
DOF 134, DOFPulse
'Light004.state=1
'timer017.enabled= true
Timer036.Enabled = 0
end Sub


'end timers to prevent backup of kicker


'start add 8 second ball save Timer

dim shootagain
shootagain = 0


sub timer039_timer
shootagain = 0
light014.state = 0
Ramp006.collidable=0
kicker006.enabled=0
timer039.enabled = 0
end sub

Sub kicker006_hit
    If tilted = 1 Then
        kicker006.kick 90, 3
    Else
        If ultramode = 1 Then
            DMD_DisplaySceneTextWithPause "", "BALL SAVED", 3000
            qtimer.Interval = 1500
        End If

        If usePUP = True Then
            pupevent 835
        End If

        PlaySound "justamoment"
        kicker006.DestroyBall

        BallSaveQueue = BallSaveQueue + 1

        If Not BallSaveTimer.Enabled Then
            BallSaveTimer.Enabled = True
        End If
    End If
End Sub

Sub BallSaveTimer_Timer
    If BallSaveQueue > 0 Then
        Kicker007.CreateBall
        Randomize
        Select Case Int(Rnd * 3) + 1
            Case 1: Kicker007.Kick -5, 33
            Case 2: Kicker007.Kick 4, 33
            Case 3: Kicker007.Kick 5, 33
        End Select

playsound "photon"
DOF 130, DOFPulse
        BallSaveQueue = BallSaveQueue - 1
    End If

    ' Disable if queue is empty
    If BallSaveQueue <= 0 Then
        BallSaveTimer.Enabled = False
    End If
End Sub



sub Kicker009_hit 'if hal beaten once, you get one outlanes (left Side)
PicSelect 3
if usePUP=true Then
pupevent 836'POTENTIAL_INSERTPUP - Kickback kicker ejects ball back onto playfield, saving the ball from draining (infrequent)
end if
addscore 2000
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "BALL SAVED", 3000
qtimer.interval=1500
end if
PlaySoundAtLevelStatic ("WarehouseKick"), DrainSoundLevel, Kicker009
LightSeq010.UpdateInterval = 10
LightSeq010.Play SeqUpOn,5,1
if kickback=1 Then
zCol_Sleeve_006.collidable=0
ramp015.collidable=1
ramp016.collidable=1
Kicker009.kick 0, 40
Timer046.enabled=1
kickback=0
Kicker009.enabled=0
Kicker010.enabled=0
Else
end if
end Sub

sub timer046_timer()
zCol_Sleeve_006.collidable=1
ramp015.collidable=0
ramp016.collidable=0
Ramp017.collidable=0
Ramp019.collidable=0
light095.state=0
Light096.state=0
Timer046.enabled=0
end Sub

sub Kicker010_hit 'if hal beaten once, you get one outlanes (right Side)
PicSelect 3
if usePUP=true Then
pupevent 836'POTENTIAL_INSERTPUP - Kickback kicker ejects ball back onto playfield, saving the ball from draining (infrequent)
end if
addscore 2000
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "BALL SAVED", 3000
qtimer.interval=1500
end if
PlaySoundAtLevelStatic ("WarehouseKick"), DrainSoundLevel, Kicker010
LightSeq010.UpdateInterval = 10
LightSeq010.Play SeqUpOn,5,1
if kickback=1 Then
Ramp017.collidable=1
Ramp019.collidable=1
Kicker010.kick 0, 40
Timer047.enabled=1
kickback=0
Kicker009.enabled=0
Kicker010.enabled=0
Else
end if
end Sub

sub timer047_timer()
zCol_Sleeve_006.collidable=1
ramp015.collidable=0
ramp016.collidable=0
Ramp017.collidable=0
Ramp019.collidable=0
light095.state=0
Light096.state=0
Timer047.enabled=0
end Sub

'sub Kicker009_hit 'if hal beaten once, you get one outlanes (left Side)
'pin3.collidable=0
'ramp015.collidable=1
'ramp016.collidable=1
'Kicker009.kick 0, 65
'end Sub

sub timer048_timer
if ballsonplayfield > 1 then
Controller.B2SSetData 201, 1 'makes b2s light come on during multiball
Light108.state=2
else 
if usePUP=true Then
pupevent 837'POTENTIAL_INSERTPUP - Multiball ends, stop any multiball video if playing
end if
Controller.B2SSetData 201, 0
Light108.state=0
end If
end Sub

Sub KitOff
Dim a
For each a in kit
     a.state = 0
Next
End sub

Sub KitOn
Dim a
For each a in kit
     a.state = 2
Next
End sub

sub timer055_timer
KitOff
timer055.enabled=0
end Sub


'*************DMD STUFF**************************************************************
'************************************************************************************
Dim UltraDMD
Dim folderPath
folderPath = "\HAL9000.UltraDMD"

Const UltraDMD_VideoMode_Stretch = 0
Const UltraDMD_VideoMode_Top = 1
Const UltraDMD_VideoMode_Middle = 2
Const UltraDMD_VideoMode_Bottom = 3


'Const UltraDMD_Animation_FadeIn = 0
Const UltraDMD_Animation_FadeOut = 1
'Const UltraDMD_Animation_ZoomIn = 2
Const UltraDMD_Animation_ZoomOut = 3
Const UltraDMD_Animation_ScrollOffLeft = 4
Const UltraDMD_Animation_ScrollOffRight = 5
Const UltraDMD_Animation_ScrollOnLeft = 6
Const UltraDMD_Animation_ScrollOnRight = 7
Const UltraDMD_Animation_ScrollOffUp = 8
Const UltraDMD_Animation_ScrollOffDown = 9
Const UltraDMD_Animation_ScrollOnUp = 10
Const UltraDMD_Animation_ScrollOnDown = 11
Const UltraDMD_Animation_None = 14

Sub LoadUltraDMD
    Set UltraDMD = CreateObject("UltraDMD.DMDObject")
    If UltraDMD is Nothing Then
        MsgBox "No UltraDMD found.  This table MAY run without it."
        Exit Sub
    End If

    UltraDMD.Init
    If Not UltraDMD.GetMajorVersion = 1 Then
        MsgBox "Incompatible Version of UltraDMD found."
        Exit Sub
    End If

    If UltraDMD.GetMinorVersion < 1 Then
        MsgBox "Incompatible Version of UltraDMD found. Please update to version 1.1 or newer."
        Exit Sub
    End If

    Dim fso
    Set fso = CreateObject("Scripting.FileSystemObject")
    Dim curDir
    curDir = fso.GetAbsolutePathName(".")
    UltraDMD.SetProjectFolder curDir & folderPath
End Sub


Function GetTrackText(astr, limit)
	If Not IsNull(astr) And LEN(astr) > 0 Then
		Dim text
		text = astr
		If InStr(astr,".mp3") Then
			text = mid(text,1,LEN(text) -4) 'remove the termination .mp3
		End If 
		If InStr(astr,"VH - ") Then
			text = replace(text,"VH - ","") 'remove prefix
		End If
		If limit And Len(text) > 18 Then
			text = rtrim(left(text,16)) + "..."
		End If
		GetTrackText = text
	End If
End Function

Sub DMD_SetScoreboardBackground(imageName)
	If Not UltraDMD is Nothing Then
		UltraDMD.SetScoreboardBackgroundImage imageName, 15, 15
	End If
End Sub

Sub DMD_DisplaySongSelect
    If Not UltraDMD is Nothing Then
		DMD_ClearScene
		If UltraDMD.GetMajorVersion = 1 AND UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard 2, 0, TempSongSelect, SongSelectTimerCount, 0, 0, GetTrackText(TrackFilename(TempSongSelect), false), ""
		Else
			UltraDMD.DisplayScoreboard00 2, 0, TempSongSelect, SongSelectTimerCount, 0, 0, GetTrackText(TrackFilename(TempSongSelect), false), ""
		End If
	End If
End Sub


Sub DMD_DisplayRandomAward
	If Not UltraDMD Is Nothing Then
		DMD_ClearScene
		If UltraDMD.GetMajorVersion = 1 And UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard 0, 0, 0, 0, 0, 0, "      "+AwardNames(AwardCycleValue), ""
		Else
			UltraDMD.DisplayScoreboard00 0, 0, 0, 0, 0, 0, "      "+AwardNames(AwardCycleValue), ""
		End If
	End If
End Sub

Sub DMD_DisplayWorldTour
If Not UltraDMD Is Nothing Then
		DMD_ClearScene
		If UltraDMD.GetMajorVersion = 1 And UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard 0, 0, 0, 0, 0, 0, "      "+Cities(CityNameAttempt), ""
		Else
			UltraDMD.DisplayScoreboard00 0, 0, 0, 0, 0, 0, "      "+Cities(CityNameAttempt), ""
		End If
	End If
End Sub


Sub DMD_DisplayHighScore(initials)
	If Not UltraDMD Is Nothing Then
		DMD_ClearScene

		Dim scorepoints
		If Score(CurrentPlayer)>=gsHighScore(3) Then
			scorepoints = Score(CurrentPlayer)
		ElseIf CombosThisGame(CurrentPlayer)>=gvCombosforComboChamp Then
			scorepoints = CombosThisGame(CurrentPlayer)
		ElseIf CheckTrackScoresVal Then
			scorepoints = MusicScore(CurrentPlayer,Z4)
		End If

		If UltraDMD.GetMajorVersion = 1 And UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard 1, 0, scorepoints, 0, 0, 0, "      "+initials, ""
		Else
			UltraDMD.DisplayScoreboard00 1, 0, scorepoints, 0, 0, 0, "      "+initials, ""
		End If
	End If
End Sub

Sub DMD_DisplayMatch(pos, val)
	If Not UltraDMD Is Nothing Then
		If UltraDMD.GetMajorVersion = 1 And UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard PlayersPlayingGame, 0, Score(0) MOD 100, Score(1) MOD 100, Score(2) MOD 100, Score(3) MOD 100, "   "&Space(pos)&val, ""
		Else
			UltraDMD.DisplayScoreboard00 PlayersPlayingGame, 0, Score(0) MOD 100, Score(1) MOD 100, Score(2) MOD 100, Score(3) MOD 100, "   "&Space(pos)&val, ""
		End If
	End If
End Sub

Sub DMD_DisplayScoreboard
    If Not UltraDMD is Nothing Then
		If UltraDMD.IsRendering Then
			Debug.Print "Still rendering..."
		End If
		If Not UltraDMD.IsRendering And BallsRemaining(CurrentPlayer) > 0 Then
			If UltraDMD.GetMajorVersion = 1 AND UltraDMD.GetMinorVersion < 4 Then
				UltraDMD.DisplayScoreboard PlayersPlayingGame, CurrentPlayer+1, Score(0), Score(1), Score(2), Score(3), GetTrackText(TrackFilename(MusicNumber), true), "ball "+CStr(gsBallsPerGame+1-BallsRemaining(CurrentPlayer))
			Else
				UltraDMD.DisplayScoreboard00 PlayersPlayingGame, CurrentPlayer+1, Score(0), Score(1), Score(2), Score(3), GetTrackText(TrackFilename(MusicNumber), true), "ball "+CStr(gsBallsPerGame+1-BallsRemaining(CurrentPlayer))
			End If
		End If
	End If
End Sub

Sub DMD_ClearScene
	If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
		Do While UltraDMD.IsRendering
			'wait until rendering is actually cancelled
		Loop
		Debug.Print "DMD Cleared"
	End If
End Sub

Sub DMD_DisplayScores
	If Not UltraDMD is Nothing Then
		DMD_ClearScene
		Dim players
		If IsEmpty(PlayersPlayingGame) Then
			players = 1
		Else
			players = PlayersPlayingGame
		End If
		
		If UltraDMD.GetMajorVersion = 1 AND UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard players, 0, Score(0), Score(1), Score(2), Score(3), "", ""
		Else
			UltraDMD.DisplayScoreboard00 players, 0, Score(0), Score(1), Score(2), Score(3), "", ""
		End If
	End If
End Sub

Sub DMD_DisplayCredits(text)
	If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
		UltraDMD.ScrollingCredits "blank.png", text, 15, UltraDMD_Animation_ScrollOnUp, 500, UltraDMD_Animation_None
	End If
End Sub

Sub DMD_DisplaySceneText(toptext, bottomtext)
	DMD_DisplayScene "", toptext, 15, bottomtext, 15, UltraDMD_Animation_None, 10000, UltraDMD_Animation_None
End Sub

Sub DMD_DisplaySceneTextWithPause(toptext, bottomtext, pauseTime)
	DMD_DisplayScene "", toptext, 15, bottomtext, 15, UltraDMD_Animation_None, pauseTime, UltraDMD_Animation_None
End Sub


Sub DMD_ModifyScene(id, toptext, bottomtext)
	If Not UltraDMD is Nothing Then
		UltraDMD.ModifyScene00 id, toptext, bottomtext
	End If
End Sub

Sub DMD_DisplayLogo
	DMD_DisplayScene "logo.png", "", -1, "", -1, UltraDMD_Animation_None, 5000, UltraDMD_Animation_None
End Sub

Sub DMD_DisplayScene(bkgnd, toptext, topBrightness, bottomtext, bottomBrightness, animateIn, pauseTime, animateOut)
    If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
        UltraDMD.DisplayScene00 bkgnd, toptext, topBrightness, bottomtext, bottomBrightness, animateIn, pauseTime, animateOut
        If pauseTime > 0 OR animateIn < 14 OR animateOut < 14 Then
            'Timer1.Enabled = True
        End If
    End If
End Sub

Sub DMD_DisplaySceneExWithId(id, cancelPrev, bkgnd, toptext, topBrightness, topOutlineBrightness, bottomtext, bottomBrightness, bottomOutlineBrightness, animateIn, pauseTime, animateOut)
	If Not UltraDMD is Nothing Then
		UltraDMD.DisplayScene00ExWithId id, cancelPrev, bkgnd, toptext, topBrightness, topOutlineBrightness, bottomtext, bottomBrightness, bottomOutlineBrightness, animateIn, pauseTime, animateOut
		If pauseTime > 0 OR animateIn < 14 OR animateOut < 14 Then
            Timer1.Enabled = True
		End If
    End If
End Sub

'stuff for UltraDMD
'stuff for UltraDMD
dim UltraMode
dim sortnamed
qtimer.enabled=0

if ultramode=1 then
LoadUltraDMD
Else
end If

if ultramode=1 then
'txtHigh1.text = HighScore1
'DMD_DisplaySceneTextWithPause "HAL 9000", "FREE PLAY", 10000
'HighScore1 = GetValue(hsFile, "HighScore1", 0)
hightext = "HIGH SCORE " & Highscore1 'txtHigh1.text
haltext = "HALS HIGH " & HALScore '
DMD_DisplaySceneTextWithPause "2001: A Space Odyssey", hightext, 10000
scoretimer.enabled=1
end if

Sub scoretimer_Timerb()
    If showHAL Then
        DMD_DisplaySceneTextWithPause "2001: A Space Odyssey", HALtext, 10000
    Else
        DMD_DisplaySceneTextWithPause "2001: A Space Odyssey", hightext, 10000
    End If
    showHAL = Not showHAL
End Sub


Sub scoretimer_Timer()
    Select Case messageIndex
        Case 0
            DMD_DisplaySceneTextWithPause "PRESS RIGHT MAGNASAVE", "FOR HAL TO PLAY", 10000
        Case 1            
            DMD_DisplaySceneTextWithPause "2001: A Space Odyssey", HALtext, 10000
        Case 2            
            DMD_DisplaySceneTextWithPause "2001: A Space Odyssey", hightext, 10000
    End Select

    messageIndex = (messageIndex + 1) Mod 3  ' cycle 0,1,2,0,1,2...
End Sub




Sub QTimer_Timer
'Ramp026.collidable=False
dim sortnamed
if ultramode=1 then
DMD_DisplaySceneTextWithPause ultraball, p1score, 10000
qtimer.interval=100
Else
end If
End Sub

dim ultraball

sub timer056_timer
if ultramode=1 then
    ultraball = "BALL " & dmdballnum
    hightext = "HIGH SCORE " & txtHigh1.text
end if
End Sub
'**************END DMD STUFF*********************************************************
'************************************************************************************


'************************************************************************************
'****************************STUFF NEEDED TO RECORD HIGH SCORE***********************

Const TableName = "HAL9000"

Sub LoadHighScores()
    Dim x
    x = LoadValue(TableName, "HighScore1")

    If IsNumeric(x) Then
        HighScore1 = CLng(x)
    Else
        HighScore1 = 250000
    End If
End Sub

Sub SaveHighScores()
    SaveValue TableName, "HighScore1", p1score
    HighScore1 = p1score ' Optional: keep memory variable in sync
End Sub

Sub LoadHalScores()
    Dim x
    x = LoadValue(TableName, "HALScore")

    If IsNumeric(x) Then
        HALScore = CLng(x)
    Else
        HALScore = 250000
    End If
End Sub

Sub SaveHalScores()
    SaveValue TableName, "HALScore", p1score
    HALScore = p1score ' Optional: keep memory variable in sync
End Sub
'************************************************************************************
'************************************************************************************

'sub wall006_hit
'PlaySound SoundFX("targ", DOFDropTargets)
'end sub

Sub wall006_Hit()
    Dim speed, vol
    speed = Sqr(ActiveBall.VelX ^ 2 + ActiveBall.VelY ^ 2)
    
    vol = speed / 30
    If vol > 1 Then vol = 1
    If vol < 0.1 Then vol = 0.1 ' Minimum volume

    ' Try both to be sure
    PlaySound "metalhit_medium", 0, vol
    ' Or with position (comment one or the other)
    'PlaySoundAt "thud", Kicker1, vol
End Sub

Sub wall023_Hit()
    Dim speed, vol
    speed = Sqr(ActiveBall.VelX ^ 2 + ActiveBall.VelY ^ 2)
    
    vol = speed / 30
    If vol > 1 Then vol = 1
    If vol < 0.1 Then vol = 0.1 ' Minimum volume

    ' Try both to be sure
    PlaySound "rubber_hit_2", 0, vol
    ' Or with position (comment one or the other)
    'PlaySoundAt "thud", Kicker1, vol
End Sub

Sub rubber013_Hit()
    Dim speed, vol
    speed = Sqr(ActiveBall.VelX ^ 2 + ActiveBall.VelY ^ 2)
    
    vol = speed / 30
    If vol > 1 Then vol = 1
    If vol < 0.1 Then vol = 0.1 ' Minimum volume

    ' Try both to be sure
    PlaySound "rubber_hit_1", 0, vol
    ' Or with position (comment one or the other)
    'PlaySoundAt "thud", Kicker1, vol
End Sub

Sub wall002_Hit()
    Dim speed, vol
    speed = Sqr(ActiveBall.VelX ^ 2 + ActiveBall.VelY ^ 2)
    
    vol = speed / 30
    If vol > 1 Then vol = 1
    If vol < 0.1 Then vol = 0.1 ' Minimum volume

    ' Try both to be sure
    PlaySound "rubber_hit_3", 0, vol
    ' Or with position (comment one or the other)
    'PlaySoundAt "thud", Kicker1, vol
End Sub

Sub wall053_Hit()
    Dim speed, vol
    speed = Sqr(ActiveBall.VelX ^ 2 + ActiveBall.VelY ^ 2)
    
    vol = speed / 30
    If vol > 1 Then vol = 1
    If vol < 0.1 Then vol = 0.1 ' Minimum volume

    ' Try both to be sure
    PlaySound "metalhit_medium", 0, vol
    ' Or with position (comment one or the other)
    'PlaySoundAt "thud", Kicker1, vol
End Sub

Sub wall031_Hit()
    Dim speed, vol
    speed = Sqr(ActiveBall.VelX ^ 2 + ActiveBall.VelY ^ 2)
    
    vol = speed / 30
    If vol > 1 Then vol = 1
    If vol < 0.1 Then vol = 0.1 ' Minimum volume

    ' Try both to be sure
    PlaySound "metalhit_medium", 0, vol
    ' Or with position (comment one or the other)
    'PlaySoundAt "thud", Kicker1, vol
End Sub

Sub rubber004_Hit()
RecordActivity
    Dim speed, vol
    speed = Sqr(ActiveBall.VelX ^ 2 + ActiveBall.VelY ^ 2)
    
    vol = speed / 30
    If vol > 1 Then vol = 1
    If vol < 0.1 Then vol = 0.1 ' Minimum volume

    ' Try both to be sure
    PlaySound "rubber_hit_3", 0, vol
    ' Or with position (comment one or the other)
    'PlaySoundAt "thud", Kicker1, vol
End Sub

Sub rubberpost8_Hit()
    Dim speed, vol
    speed = Sqr(ActiveBall.VelX ^ 2 + ActiveBall.VelY ^ 2)
    
    vol = speed / 30
    If vol > 1 Then vol = 1
    If vol < 0.1 Then vol = 0.1 ' Minimum volume

    ' Try both to be sure
    PlaySound "rubber_hit_3", 0, vol
    ' Or with position (comment one or the other)
    'PlaySoundAt "thud", Kicker1, vol
End Sub

Sub rubberpost9_Hit()
    Dim speed, vol
    speed = Sqr(ActiveBall.VelX ^ 2 + ActiveBall.VelY ^ 2)
    
    vol = speed / 30
    If vol > 1 Then vol = 1
    If vol < 0.1 Then vol = 0.1 ' Minimum volume

    ' Try both to be sure
    PlaySound "rubber_hit_3", 0, vol
    ' Or with position (comment one or the other)
    'PlaySoundAt "thud", Kicker1, vol
End Sub

Sub ResetChangeLight
	ChangeThinLight White
	ChangeGi blue
	ChangeBumperLight amber
End Sub
'***************
'Mouvement vaisseau => Use "MooveUpVaisseau"
'***************
Dim MooveVaisseauCount
MooveVaisseauCount = 0

Sub MooveUpVaisseau
	If VaisseauUpDownTimer.Enabled = False And VaisseauRot1Timer.Enabled = False Then
		If MooveVaisseauCount = 0 Then pupevent 841: playsound "thrust": timer003.enabled=1 : TriggerScript 800, "MooveVaisseauToy 1"
		If MooveVaisseauCount = 1 Then pupevent 842: playsound "thrust2": FlashForMs F1A012, 1000, 1000, 0 : FlashForMs F1A011, 900, 50, 0 : FlashForMs F1A013, 800, 200, 0 : TriggerScript 800,"Moove2VaisseauToy 1"
		If MooveVaisseauCount = 2 Then pupevent 843: playsound "thrust": timer002.enabled=1 : TriggerScript 800, "Moove2VaisseauToy 1"
		If MooveVaisseauCount = 3 Then pupevent 844: playsound "thrust2": FlashForMs F1A021, 1000, 1000, 0 : FlashForMs F1A020, 900, 50, 0 : FlashForMs F1A022, 800, 200, 0 : TriggerScript 800,"MooveVaisseauToy 1"
	End If
End Sub

Sub MooveVaisseauToy(enabled)
 If enabled Then
'	PlaySound ""
	VaisseauUpDownTimer.enabled=True
End If
End Sub

Dim VaisseauDown
VaisseauDown = True

Sub VaisseauUpDownTimer_Timer()
If Not VaisseauDown Then
		Primitive001.TransZ = Primitive001.TransZ -1 'step 4 moves at this speed in this negative direction

		If Primitive001.TransZ <= 0 Then		
			VaisseauUpDownTimer.Enabled = False	
			VaisseauRot1Timer.Enabled = False
			VaisseauDown = True	
			MooveVaisseauCount = 0				
		End If
	Else	
		If VaisseauDown and MooveVaisseauCount = 0 and flipped = 0 then	
			Primitive001.TransX = ShipStartX
			Primitive001.TransY = ShipStartY
			Primitive001.TransZ = ShipStartZ
			Primitive001.RotY = ShipStartRotY	
			flipped = 1
		end if

            If Primitive001.TransZ <= -720 Then ' was 940 his is step 1 moves it until 720
			VaisseauUpDownTimer.Enabled = False
			VaisseauRot1Timer.Enabled = False
			VaisseauDown = False	
			MooveVaisseauCount = 1
            flipped = 1
		Else 
			Primitive001.TransZ = Primitive001.TransZ - 1 ' was +20  this is step 1 moves at this speed
		End If
	End If
End Sub


Sub Moove2VaisseauToy(enabled)
	if enabled and MooveVaisseauCount = 2 and flipped = 1 then
		Primitive001.RotY = 81
		Primitive001.Transz = Primitive001.Transz + 1760
		Primitive001.Transx = Primitive001.Transx + -2730
		flipped = 0 
	end if
	If enabled Then
		VaisseauRot1Timer.enabled=True
	End If
End Sub

Dim VaisseauRotDownUp
VaisseauRotDownUp = True

Sub VaisseauRot1Timer_Timer() 'rotation and go to end 
	If Not VaisseauRotDownUp Then

		Primitive001.RotY = Primitive001.RotY - 0.1 'angle change
		Primitive001.TransX = Primitive001.TransX + 2 'how far far to near, maybe distance per 1 tick angle
		Primitive001.TransZ = Primitive001.TransZ - .25 ' was +4 'this is step 3 second rotation 



		If Primitive001.RotY <= 12 Then		'this is step 3 second rotation 
			VaisseauRot1Timer.Enabled = False	
			VaisseauRotDownUp = True
			MooveVaisseauCount = 3
		End If
	Else	
		If Primitive001.RotY >= 260 Then  'this is step 2 FirstGame rotation
			VaisseauRot1Timer.Enabled = False
			VaisseauRotDownUp = False	
			MooveVaisseauCount = 2
		Else 
			Primitive001.RotY = Primitive001.RotY + 0.1 'angle change
			Primitive001.TransX = Primitive001.TransX + 2 'how far far to near, maybe distance per 1 tick angle
			Primitive001.TransZ = Primitive001.TransZ - .25 ' was -4 this is step 2 First rotation how far left and right
		End If
	End If
End Sub

'spin ship
Dim spinAngle
spinAngle = 90

Sub StartSpinning()
primitive002.visible=1
primitive001.visible=0
    SpinTimer.Enabled = True
pupevent 838
End Sub

Sub StopSpinning()
primitive002.visible=0
primitive001.visible=1
    SpinTimer.Enabled = False
End Sub

Sub SpinTimerb_Timer()
    spinAngle = (spinAngle + 1) Mod 360  ' Increment 1 degree per tick
    Primitive002.Roty = spinAngle
End Sub

Sub SpinTimer_Timer()
    spinAngle = (spinAngle + 0.05)
    If spinAngle >= 360 Then spinAngle = spinAngle - 360 ' Keep it within 0–359
    Primitive002.RotY = spinAngle
End Sub

StartSpinning

'*******************ball rescue timer for if ball gets stuck under ramp*********************
Dim LastActivityTime
'Dim BallRescueTimer

BallRescueTimer.Enabled = True

Sub RecordActivity()
    LastActivityTime = GameTime
End Sub

Sub BallRescueTimer_Timer()
    If BallsOnPlayfield = 1 Then
        Dim inactivityTime
        inactivityTime = GameTime - LastActivityTime

        ' Original rescue logic after 10 seconds of inactivity
        If inactivityTime > 10000 Then
            Dim balls, b
            balls = GetBalls
            For Each b In balls
                If IsBallNearRescueArea(b) Then
                    RescueStuckBall()
                    pupevent 839
                    Exit For
                End If
            Next
        End If

        ' Autoplay nudge logic after 20 seconds of inactivity
        If halplay = 1 And inactivityTime > 20000 Then
            Nudge 270, 2 ' (Direction in degrees, strength 1–5)
            BallRescueTimer.Enabled = False
        End If
    End If
End Sub

Function IsBallNearRescueArea(ball)
    ' Define the stuck zone around the ramp
    Dim RampX: RampX = Primitive003.X
    Dim RampY: RampY = Primitive003.Y
    Dim Range: Range = 200 ' How big an area around ramp to count as "near" (adjust as needed)

    IsBallNearRescueArea = (Abs(ball.X - RampX) < Range) And (Abs(ball.Y - RampY) < Range)
End Function

Sub RescueStuckBall()
     if LiPass001.state=0 Then
Ramp006.collidable=1 'enable hidden ramp to bring shoot again ball to the correct kicker below flippers
kicker006.enabled=1 'enable special shoot again Kicker001
light014.state=2 'turn on shoot again Light
timer039.enabled=1 'turns off shot again light and disables shoot again and the makes shoot again ramp non-collidable
    LiPass001.state=2
 LiPass002.state=2
    ramp007.visible=False
    ramp007.collidable=0
    ramp008.visible=1
    PlaySound "rampsound"
    LastActivityTime = GameTime
end If
End Sub
'*******************end ball rescue timer for if ball gets stuck under ramp*********************
'***to remove completely find all instances of "recordactivity" and remove them***********

sub timer001_timer 'keeps shoot again light on if shoot again timer expires after extra ball received
Light014.state=2
end sub

sub Rubber014_hit
RecordActivity
end Sub

sub ballrelease_timer
Flasher005.visible=0
BallRelease.createball
BallRelease.kick 45,8
DOF 123, DOFPulse
    RandomSoundBallRelease BallRelease
pupevent 840
ballrelease.timerenabled=0
end Sub

sub fastrelease_timer
Flasher005.visible=0
BallRelease.createball
pupevent 845
BallRelease.kick 45,8
DOF 123, DOFPulse
    RandomSoundBallRelease BallRelease
fastrelease.enabled=0
end Sub
'****************************
'Remplace Fonction vpmTimer.AddTimer
'Use exemple (TriggerScript 800,"MooveVaisseauToy 1") TriggerScript = Appel de la fonction / 800 = Time start / "Action à faire"
'****************************
Dim pReset(9) 
Dim pStatement(9)           'holds future scripts
Dim FX

for fx=0 to 9
    pReset(FX)=0
    pStatement(FX)=""
next

DIM pTriggerCounter:pTriggerCounter=pTriggerScript.interval

Sub pTriggerScript_Timer()
  for fx=0 to 9  
       if pReset(fx)>0 Then	
          pReset(fx)=pReset(fx)-pTriggerCounter 
      	  if pReset(fx)<=0 Then
			pReset(fx)=0
			execute(pStatement(fx))
		  end if 	
       End if
  next
End Sub


Sub TriggerScript(pTimeMS,pScript)
for fx=0 to 9  
  if pReset(fx)=0 Then
    pReset(fx)=pTimeMS
    pStatement(fx)=pScript
    Exit Sub
  End If 
next
end Sub

sub timer002_timer  'delays the firing of the rocket blast
FlashForMs F1A016, 1000, 1000, 0
FlashForMs F1A015, 900, 50, 0
FlashForMs F1A014, 800, 200, 0
timer002.enabled=0
end Sub

sub timer003_timer 'delays the firing of the rocket blast
FlashForMs F1A010, 1000, 1000, 0
FlashForMs F1A009, 900, 50, 0
FlashForMs F1A008, 800, 200, 0 
timer003.enabled=0
end Sub

'***************animate hal lights quickly down during monolith battle
Dim LightSeqRunning

Sub StartLightSeq()
    If Not LightSeqRunning Then
        LightSeqRunning = True
'   LightSeq009.UpdateInterval = 7
'LightSeq009.Play SeqDownOn,5,200
Light146.state=2
Light147.state=2
Light148.state=2
Light149.state=2
Light150.state=2
Light151.state=2
Light152.state=2
Light153.state=2
Light154.state=2
Light155.state=2
Light156.state=2
Light142.state=2
Light143.state=2
Light144.state=2
Light145.state=2

'LightSeq009.Play SeqUpOn,5,1
'LightSeq009.Play SeqClockRightOn,360,200
        LightSeqTimer.Enabled = True
    End If
End Sub

Sub StopLightSeq()
    LightSeqRunning = False
    LightSeq009.StopPlay
Light146.state=0
Light147.state=0
Light148.state=0
Light149.state=0
Light150.state=0
Light151.state=0
Light152.state=0
Light153.state=0
Light154.state=0
Light155.state=0
Light156.state=0
Light142.state=0
Light143.state=0
Light144.state=0
Light145.state=0
Light131.state=0
Light132.state=0
Light128.state=0
Light126.state=0
Light125.state=0
Light124.state=0
'Light101.state=0
Light077.state=0
Light134.state=0
Light187.state=0
Light188.state=0
Light177.state=0
Light174.state=0
Light165.state=0
Light159.state=0
Light158.state=0
Light129.state=0
Light029.state=0
    LightSeqTimer.Enabled = False
End Sub

Sub LightSeqTimer_Timer()
    If LightSeqRunning Then
Light152.state=2 'the group of lights on the mid-roght plastic sometimes turn off during monolith battle, added these to turn them back on
Light153.state=2
Light154.state=2
Light155.state=2
Light156.state=2
Light142.state=2
Light143.state=2
Light144.state=2
Light145.state=2  
    Else
        LightSeqTimer.Enabled = False
    End If
End Sub
'***************animate hal lights quickly down during monolith battle

sub timer004_timer
'Trigger017.enabled=1
beatboss = 0 
StartRise
pupevent 847'POTENTIAL_INSERTPUP - Battle against monolith begins
barrierdelay2.enabled=1
Kicker043.enabled=True
Kicker051.enabled=True
Kicker004.enabled=True
Kicker052.enabled=True
target044.isdropped = 0
target045.isdropped = 0
RaisePrimitive
Primitive001.visible = False
'Ramp009.visible=0
target029.isdropped = 0
target030.isdropped = 0
target031.isdropped = 0
target032.isdropped = 0
Trigger016.enabled=1
StartFadeIn
'PlaySound "fire",-1
StopSound "2001_music"
'kicker044.createball
'kicker044.kick 90, 12
allfour=0
ballsinskull = 0
destroyhalsballs
'PlaySound "ha_skull"
Light131.state=2
Light132.state=2
Light128.state=2
Light126.state=2
Light125.state=2
Light124.state=2
Light101.state=2
Light077.state=2
Light134.state=2
Light187.state=2
Light188.state=2
Light177.state=2
Light109.state=2
Light174.state=2
Light165.state=2
Light159.state=2
Light158.state=2
Light129.state=2
Light029.state=2
StartLightSeq
timer004.enabled=0
end Sub

sub timer005_timer
'Trigger017.enabled=1
beatboss = 0 
StartRise
barrierdelay.enabled=1
Kicker043.enabled=True
Kicker051.enabled=True
Kicker004.enabled=True
Kicker052.enabled=True
target044.isdropped = 0
target045.isdropped = 0
RaisePrimitive
Primitive001.visible = False
target029.isdropped = 0
target030.isdropped = 0
target031.isdropped = 0
target032.isdropped = 0
Trigger016.enabled=1
StartFadeIn
'PlaySound "fire",-1
StopGameMusic
if usepup = false then
PlaySound "spaceflight", -1
end if
StopSound "2001_music"
allfour=0
ballsinskull = 0
DestroyHalsBalls
'PlaySound "ha_skull"
Light131.state=2
Light132.state=2
Light128.state=2
Light126.state=2
Light125.state=2
Light124.state=2
Light101.state=2
Light077.state=2
Light134.state=2
Light187.state=2
Light188.state=2
Light177.state=2
Light109.state=2
Light174.state=2
Light165.state=2
Light159.state=2
Light158.state=2
Light129.state=2
Light029.state=2
StartLightSeq
timer005.enabled=0
end Sub


Dim JumpBall053

Sub Kicker053_Hit()
LightSeq009.Stopplay
LightSeq009.UpdateInterval = 25
	LightSeq009.Play SeqRandom,10,,2000
    Set JumpBall053 = ActiveBall
    AddScore 2000
SoundSaucerLock
playsound "impact1"
FlashForMs F1A005, 2000, 100, 0
FlashForMs F1A006, 2000, 100, 0
FlashForMs F1A007, 2000, 100, 0
FlashForMs GiFlipLH, 2000, 100, 1
FlashForMs GiFlipRH, 2000, 100, 1
ChangeThinLight baseLH
ChangeGi baseLH
ChangeBumperLight green
'Ramp027.Collidable = 1
    BallJumpTimer053.Enabled = True
kicker053.enabled=0
End Sub

Sub BallJumpTimer053_Timer()
    If Not JumpBall053 Is Nothing Then
  PlaySound "warehousekick"
DOF 114, DOFPulse
DOF DOFContactors, DOFPulse
'PlaySound SoundFXDOF("fx_kicker", 114, DOFPulse, DOFContactors)
        Kicker053.Kick 0, 0.1  ' Release from kicker
        JumpBall053.VelZ = 45
        JumpBall053.VelX = 0
        JumpBall053.VelY = 0
    End If
    BallJumpTimer053.Enabled = False
    Set JumpBall053 = Nothing
End Sub

sub kicker053old_hit
addscore 2000
playsound "fx_kicker"
'ramp027.collidable=1
  Kicker053.Kick 90, 0  ' Temporarily kick it to trigger event
   ActiveBall.VelZ = 60   ' Z velocity controls vertical lift; increase for higher jump
    ActiveBall.VelX = 0
   ActiveBall.VelY = 0
end Sub

sub ramp025_hit
'ramp026.collidable=0
end Sub

sub ramp023_hit
kicker053.enabled=1
'ramp027.collidable=0
end Sub

sub Trigger010_hit
kicker053.enabled=1
end Sub

sub Trigger013_hit
kicker003.Enabled=1
end Sub


sub Kicker011_hit
if halplay=1 and ballsonplayfield < 3 then 
Trigger021.enabled=0
trigger022.enabled=0
end if 
End Sub

sub Kicker012_hit
End Sub

sub ramp022_hit
'ramp026.collidable=0
end sub

Dim JumpBall

Sub Kicker014_Hit()
addscore 2000
LightSeq009.Stopplay
LightSeq009.UpdateInterval = 25
	LightSeq009.Play SeqRandom,10,,2000
SoundSaucerLock
playsound "impact1"
FlashForMs F1A005, 2000, 100, 0
FlashForMs F1A006, 2000, 100, 0
FlashForMs F1A007, 2000, 100, 0
FlashForMs GiFlipLH, 2000, 100, 1
FlashForMs GiFlipRH, 2000, 100, 1
ChangeThinLight base
ChangeGi base
ChangeBumperLight base
    Set JumpBall = ActiveBall
      BallJumpTimer.Enabled = True
    Kicker014.Enabled = False
End Sub

Sub BallJumpTimer_Timer()
    If Not JumpBall Is Nothing Then
        ' Step 1: Kick out at neutral direction to free ball
        Kicker014.Kick 0, 0.1  ' Gentle nudge to release ball
        ' Step 2: Apply vertical lift
  PlaySound "warehousekick"
        JumpBall.VelZ = 40
        JumpBall.VelX = 0
        JumpBall.VelY = 0
    End If
    BallJumpTimer.Enabled = False
    Set JumpBall = Nothing
End Sub

sub kicker014_old_hit
'ramp026.collidable=1
playsound "fx_kicker"
    Kicker014.Kick 90, 0  ' Temporarily kick it to trigger event
    ActiveBall.VelZ = 70    ' Z velocity controls vertical lift; increase for higher jump
    ActiveBall.VelX = 0
    ActiveBall.VelY = 0
'kicker003.Enabled=1
kicker014.enabled=0
end sub

sub Timer006_timer
kicker014.enabled=1
timer006.enabled=0
end Sub


Sub Timer007_timer 'create, disable, make it 5000   PUT AT BOTTOM
kicker023.enabled=1
timer007.enabled=0
end sub

Sub BoostBallRight(aBall)
    aBall.VelX = aBall.VelX + 50  ' Adjust value as needed
End Sub

Sub Trigger014_Hit()
    ActiveBall.VelX = ActiveBall.VelX * 0.1
    ActiveBall.VelY = ActiveBall.VelY * 0.1
End Sub

Dim angle
    angle = 0
Primitive006.Rotz = -23.1 'Jupiter’s axial tilt (roughly 3° IRL, exaggerated here for effect)

Sub RotTimer_Timer()
    angle = (angle + 1) Mod 360   ' Adjust 2 for speed
    Primitive006.RotY = angle     ' Use RotX or RotY for other axes
End Sub

sub timer008_timer 'launches multiball after beating monolith
If BallsOnPlayfield >= 7 Then
    Kicker015.CreateBall
    Kicker015.Kick 215, 11
    PlaySound "photon"
    Timer008.Enabled = False
Else
    If BallsOnPlayfield = 5 Then
    End If

    Kicker015.CreateBall
    Kicker015.Kick 215, 11
    PlaySound "photon"
    BallsOnPlayfield = BallsOnPlayfield + 1

    If BallsOnPlayfield = 7 Then
        Timer008.Enabled = False
        BallsOnPlayfield = BallsOnPlayfield - 1 ' Adjust for destroyed "killshot" ball
    End If
End If
end sub

sub gate007_hit
Trigger004.enabled=0
StartBallControl.enabled=1
end Sub

sub Timer016_timer 'locled ball ball launch
Plunger.AutoPlunger = True
SoundPlungerReleaseBall
plunger.fire
Plunger.AutoPlunger = False
Kicker016.createball
Kicker016.kick 0, 53
'playsound "fx_kicker"
'Light004.state=1
'timer017.enabled= true
Timer016.Enabled = 0
end Sub

sub Trigger015_hit 'stops ramp sound if ball doesnt make it over the top
end Sub

Dim RiseSpeed, TargetZ
RiseTimer.Enabled = False

'Sub Table1_Init()
    ' Starting Z position
  Primitive025.Z = -600
  Primitive030.Z = -600
  Primitive041.Z = -600
    ' Rise speed per timer tick
    RiseSpeed = 1
    ' Final Z position (height)
    TargetZ = 0
'End Sub

Sub StartRise() 'lift the monolith
    playsound "raise"
    Primitive025.Z = -600
    Primitive030.Z = -600
    Primitive041.Z = -600
        Primitive025.visible = True
        Primitive030.visible = True
        Primitive041.visible = True
    RiseTimer.Enabled = True
End Sub

Sub RiseTimer_Timer() 'lift the monolith
    If Primitive025.Z < TargetZ Then
        Primitive025.Z = Primitive025.Z + RiseSpeed
        Primitive030.Z = Primitive030.Z + RiseSpeed
        Primitive041.Z = Primitive041.Z + RiseSpeed
    Else
        Primitive025.visible = False
        Primitive030.visible = False
        Primitive041.visible = False
        Primitive025.visible = True
        Primitive030.visible = True
        Primitive041.visible = True
        RiseTimer.Enabled = False
    End If
End Sub


sub barrierdelay_timer  'delays some things when monolith appears
magnet.visible=1
kicker015.createball
kicker015.kick 215, 11
playsound "photon"
Kicker046.enabled=True
barrierdelay.enabled=0
end sub

sub barrierdelay2_timer  'shorter delay for pup version
magnet.visible=1
kicker015.createball
kicker015.kick 215, 11
playsound "photon"
Kicker046.enabled=True
barrierdelay2.enabled=0
end sub


Dim SpinnerAngle
SpinnerAngle = 0

Sub SpinnerTimer_Timer()
    SpinnerAngle = (SpinnerAngle + 5) Mod 360
    SpinnerPrim.RotY = SpinnerAngle
End Sub

Sub StartSpinner()
    SpinnerTrigger1.enabled = True
    SpinnerPrim.Visible = True 
    SpinnerTimer.Enabled = True
End Sub

Sub StopSpinner()
    SpinnerTrigger1.enabled = False
    SpinnerPrim.Visible = False
    SpinnerTimer.Enabled = False
End Sub

Sub SpinnerTrigger1_Hitv '(ByRef hitBall)
playsound "fx_kicker"
    hitBall.VelY = hitBall.VelY - 90  ' Push upward
    hitBall.VelX = hitBall.VelX + 90  ' Random left/right nudge
End Sub

Sub SpinnerTrigger1_Hit()
    'Debug.Print "Trigger hit"
addscore 5000
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "+5000", 3000
qtimer.interval=1500
end if 
    ActiveBall.VelY = ActiveBall.VelY - 3
    ActiveBall.VelX = ActiveBall.VelX + 3
    'PlaySound "fx_kicker"
End Sub

Sub CheckVisTimer_Timer()
    Dim obj
    Dim anyVisible
    anyVisible = False

    For Each obj In GroupPrims
        If obj.Visible = True Then
            anyVisible = True
            Exit For
        End If
    Next

    Primitive016.Visible = anyVisible
CheckVisTimer.enabled=anyVisible
End Sub

'****Make HAL appear and disappear****
Dim HalVisible, HalCounter

Sub HalTimer_Timer()
    If HalVisible Then
        HalCounter = HalCounter + 1
        If HalCounter > 800 Then  ' After 11 seconds
            Flasherpics.visible = False
            HalVisible = False
        End If
    Else
        ' Rare chance to show HAL each tick — tune this to your liking
        If Rnd < 0.0002 Then  '  ~1 every 30 seconds
            Flasherpics.visible = True
            HalVisible = True
            HalCounter = 0
        End If
    End If
End Sub

'****END Make HAL appear and disappear****


'****remove all balls from HAL
Sub DestroyHalsBalls
ramp010.collidable=False
Ramp012.collidable=False
Ramp013.collidable=False
Ramp014.collidable=False
Kicker019.kick 180, 1
Kicker013.kick 180, 1
Kicker021.kick 180, 1
Kicker020.kick 180, 1
Kicker021.enabled=0
'Wall028.collidable=False
timer022.enabled=1
end sub

sub Timer022_timer
Kicker021.enabled=1
ramp010.collidable=True
Ramp012.collidable=True
Ramp013.collidable=True
Ramp014.collidable=True
timer022.enabled=0
end sub

sub Kicker018_hit
ResetRolling 
kicker018.timerenabled=1
end sub

sub Kicker018_timer
Kicker018.destroyball
kicker018.timerenabled=0
end sub

'****END 
'****END remove all balls from HAL

'****begin - move boss down in four stages***************

Dim DropTargetZ150

Sub Drop150Timer_Timer_old()
    If Primitive025.Z > DropTargetZ150 Then
        Primitive025.Z = Primitive025.Z - 3
    Else
        Primitive025.Z = DropTargetZ150
        Drop150Timer.Enabled = False
    End If
End Sub

Sub Drop150Timer_Timer()
    If Primitive025.Z > DropTargetZ150 Then
        ' Apply subtle shake (±1 unit in X and Y)
        Dim shakeX, shakeY
        shakeX = Rnd() * 4 - 2  ' Range: -1 to +1
        shakeY = Rnd() * 4 - 2 

        Primitive025.X = Primitive025.X + shakeX
        Primitive025.Y = Primitive025.Y + shakeY

        ' Move down slightly
        Primitive025.Z = Primitive025.Z - 3
    Else
        ' Snap to final position
        Primitive025.Z = DropTargetZ150

        ' (Optional) Reset X/Y if desired:
        ' Primitive025.X = OriginalX
        ' Primitive025.Y = OriginalY
        ' Reset to original X/Y
        Primitive025.X = OriginalX
        Primitive025.Y = OriginalY

        Drop150Timer.Enabled = False
    End If
End Sub

Dim OriginalX, OriginalY

Sub LowerBy150()
    OriginalX = Primitive025.X
    OriginalY = Primitive025.Y
    DropTargetZ150 = Primitive025.Z - 106
    Drop150Timer.Enabled = True
End Sub


sub randomkick
 Dim r
    r = Int(Rnd() * 19)
  Select Case r
Case 0: Kicker002.Kick 95, 12
Case 1: Kicker002.Kick 120, 15
Case 2: Kicker002.Kick 270, 11
Case 3: Kicker002.Kick 115, 9
Case 4: Kicker002.Kick 105, 6
Case 5: Kicker002.Kick 200, 7
Case 6: Kicker002.Kick 255, 15
Case 7: Kicker002.Kick 135, 14
Case 8: Kicker002.Kick 110, 18
Case 9: Kicker002.Kick 160, 22
Case 10: Kicker002.Kick 165, 15
Case 11: Kicker002.Kick 225, 20
Case 12: Kicker002.Kick 190, 11
Case 13: Kicker002.Kick 160, 16
Case 14: Kicker002.Kick 195, 33
Case 15: Kicker002.Kick 250, 22
Case 16: Kicker002.Kick 230, 33
Case 17: Kicker002.Kick 100, 18
Case 18: Kicker002.Kick 145, 19
    End Select
End Sub

sub Timer027_timer
Kicker002.destroyball
Kicker046.destroyball
timer027.enabled=0
end sub

Dim CanStartGame
CanStartGame = True

Sub GameDelayTimer_Timer()
    'Flasher005.visible=0
    CanStartGame = True
    RightSlingBlocker.collidable = False
    kicker039Active = False
    GameDelayTimer.Enabled = False
End Sub


' Wobble globals
' Violent wobble globals
Dim WobbleBall, WobbleCount
Const WobbleStrength = 8  ' Stronger shake force

Sub Trigger017_Hit()
'    Set WobbleBall = ActiveBall
'   WobbleCount = 12  ' Fewer frames = brief effect
'   WobbleTimer.Enabled = True
End Sub

Sub WobbleTimer_Timer()
    If WobbleCount <= 0 Then
        WobbleTimer.Enabled = False
        Set WobbleBall = Nothing
        Exit Sub
    End If

    If IsObject(WobbleBall) Then
        Dim dx, dy
        dx = (Rnd() * 2 - 1) * WobbleStrength
        dy = (Rnd() * 2 - 1) * WobbleStrength
        WobbleBall.X = WobbleBall.X + dx
        WobbleBall.Y = WobbleBall.Y + dy
    End If

    WobbleCount = WobbleCount - 1
End Sub

sub Timer040_timer
Flasher005.visible=0
Timer040.enabled=false
end Sub

sub Timer049_timer
Flasher005.visible=0
Timer049.enabled=false
end Sub

' === Fade In ===
Dim FadeStep
FadeStep = 0

Sub StartFadeIn()
    Flasher001.Visible = True
    Flasher001.Opacity = 0
    FadeStep = 0
    FadeInTimer.Enabled = True
End Sub

Sub FadeInTimer_Timer()
    If FadeStep < 40 Then
        FadeStep = FadeStep + 1
        Flasher001.Opacity = FadeStep * (100 / 40)
    Else
        Flasher001.Opacity = 100
        FadeInTimer.Enabled = False
    End If
End Sub

' === Fade Out ===
Dim FadeOutStep
FadeOutStep = 0

Sub StartFadeOut()
    FadeOutStep = 0
    FadeOutTimer.Enabled = True
End Sub

Sub FadeOutTimer_Timer()
    If FadeOutStep < 40 Then
        FadeOutStep = FadeOutStep + 1
        Flasher001.Opacity = 100 - (FadeOutStep * (100 / 40))
    Else
        Flasher001.Opacity = 0
        FadeOutTimer.Enabled = False
        Flasher001.Visible = False ' optional: hide when fully transparent
    End If
End Sub

Dim RaiseStep
Dim LowerStep

' === Raise ===
Sub RaisePrimitive()
    Primitive008.visible=1
    Primitive008.Z = -500
    RaiseStep = 0
    RaiseTimer.Enabled = True
End Sub

Sub RaiseTimer_Timer()
    If RaiseStep < 30 Then
        RaiseStep = RaiseStep + 1
        Primitive008.Z = -500 + (RaiseStep * (500 / 30)) ' -500 → 0
    Else
        Primitive008.Z = 0
        RaiseTimer.Enabled = False
    End If
End Sub

' === Lower ===
Sub LowerPrimitive()
    LowerStep = 0
    LowerTimer.Enabled = True
End Sub

Sub LowerTimer_Timer()
    If LowerStep < 30 Then
        LowerStep = LowerStep + 1
        Primitive008.Z = 0 - (LowerStep * (500 / 30)) ' 0 → -500
    Else
        Primitive008.Z = -500
        Primitive008.visible=0
        If not DMDballnum = 0 then
           Primitive001.visible=1
        end If
        LowerTimer.Enabled = False
    End If
End Sub

Dim DropStep
Dim DropStartZ025, DropStartZ030, DropStartZ041

Sub DropPrimitives()  'lowers the monolith and fences 
if flasher001.visible = True then
    ' Capture current Z positions before starting the drop
    DropStartZ025 = Primitive025.Z
    DropStartZ030 = Primitive030.Z
    DropStartZ041 = Primitive041.Z

    DropStep = 0
    DropTimer.Enabled = True
end if
End Sub


Sub DropTimer_Timer() 'lowers the monolith and fences
    If DropStep < 30 Then ' 10 steps × 10ms = 100ms total drop
        DropStep = DropStep + 1
        Dim dz
        dz = DropStep * (500 / 30)  ' 500 total units over 10 steps

        Primitive025.Z = DropStartZ025 - dz
        Primitive030.Z = DropStartZ030 - dz
        Primitive041.Z = DropStartZ041 - dz
    Else
        DropTimer.Enabled = False
    End If
End Sub

dim flicker
flicker=0

sub timer050_timer 
flasher005.visible=0
timer051.enabled=1
timer050.enabled=0
end Sub

sub timer051_timer
if flicker=1 Then
flasher005.visible=0
timer051.enabled=0
end If
if flicker=0 Then
flasher005.visible=1
flicker=1
end If
end sub

dim PicVal

Sub PicSelect(PicVal)
daveflasher.visible=0
Flasherpics.visible=0
DeadFlasher.visible=0
DeadFlasher2.visible=0
MultiFlasher.visible=0
MultiFlasher001.visible=0
ExtraBallFlasher.visible=0
GoodEveninglFlasher.visible=0
BallLockedlFlasher.visible=0
BumperslFlasher.visible=0
PyramidlFlasher.visible=0
AirlockFlasher.visible=0
IntermissionFlasher.visible=0
JupiterFlasher.visible=0
HalAfraidFlasher.visible=0
MindGoingFlasher.visible=0
StandbyFlasher.visible=0
HALmodeFlasher.visible=0
timer053.enabled=0

    Select Case PicVal
        Case 1
            daveflasher.visible=1  'dave  call with "PicSelect 1"
            timer053.enabled=1
        Case 2
            flasherpics.visible=1  'hal  "PicSelect 2"
            timer053.enabled=1
        Case 3
            DeadFlasher2.visible=1  'floating  "PicSelect 3"
            timer053.enabled=1
        Case 4
            deadflasher.visible=1  'flatlining "PicSelect 4"
            timer053.enabled=1
        Case 5
            MultiFlasher.visible=1  'multiball 1 "PicSelect 5"
            timer053.enabled=1
        Case 6
            MultiFlasher001.visible=1  'multiball 2 "PicSelect 6"
            timer053.enabled=1
        Case 7
            ExtraBallFlasher.visible=1  'extra ball "PicSelect 7"
            Timer054.enabled=1  'shorter (1 second)
        Case 8
            GoodEveninglFlasher.visible=1  'good evening dave "PicSelect 8"
            timer053.enabled=1
        Case 9
            BallLockedlFlasher.visible=1  'ball lcoked "PicSelect 9"
            Timer054.enabled=1  'shorter (1 second)
        Case 10
            BumperslFlasher.visible=1  '100 bumpers hit "PicSelect 10"
            timer053.enabled=1
        Case 11
            PyramidlFlasher.visible=1  'pyramid is lit "PicSelect 11"
            timer053.enabled=1
        Case 12
            AirlockFlasher.visible=1  'emergency airlock "PicSelect 12"
            timer053.enabled=1
        Case 13
            IntermissionFlasher.visible=1  'emergency airlock "PicSelect 13"
            'timer053.enabled=1 (not needed here, something else turns off Flasher when kicker is kicked)
        Case 14
            JupiterFlasher.visible=1  'Jupiter Mission "PicSelect 14"
            timer053.enabled=1
        Case 15
            HalAfraidFlasher.visible=1  'HAL Afraid "PicSelect 15"
            timer053.enabled=1
        Case 16
            MindGoingFlasher.visible=1  'HAL Afraid "PicSelect 16"
            timer053.enabled=1
        Case 17
            StandbyFlasher.visible=1  'Stand By "PicSelect 17"
            timer053.enabled=1
        Case 18
            HALmodeFlasher.visible=1  'HAL MODE "PicSelect 18"
            timer053.enabled=1
                Case Else
    End Select
End Sub

Sub Timer032_Timer()
    If Timer032Step = 0 Then
        PicSelect 18
        Timer032.Interval = 3000
        Timer032Step = 1
    ElseIf Timer032Step = 1 Then
        PicSelect 18
        Timer032.Enabled = False
    End If
End Sub



sub timer053_timer
daveflasher.visible=0
Flasherpics.visible=0
DeadFlasher.visible=0
DeadFlasher2.visible=0
MultiFlasher.visible=0
MultiFlasher001.visible=0
ExtraBallFlasher.visible=0
GoodEveninglFlasher.visible=0
BallLockedlFlasher.visible=0
BumperslFlasher.visible=0
PyramidlFlasher.visible=0
AirlockFlasher.visible=0
IntermissionFlasher.visible=0
JupiterFlasher.visible=0
HalAfraidFlasher.visible=0
MindGoingFlasher.visible=0
StandbyFlasher.visible=0
HALmodeFlasher.visible=0
timer053.enabled=0
end sub

sub timer054_timer
daveflasher.visible=0
Flasherpics.visible=0
DeadFlasher.visible=0
DeadFlasher2.visible=0
MultiFlasher.visible=0
MultiFlasher001.visible=0
ExtraBallFlasher.visible=0
GoodEveninglFlasher.visible=0
BallLockedlFlasher.visible=0
BumperslFlasher.visible=0
PyramidlFlasher.visible=0
AirlockFlasher.visible=0
IntermissionFlasher.visible=0
JupiterFlasher.visible=0
HalAfraidFlasher.visible=0
MindGoingFlasher.visible=0
StandbyFlasher.visible=0
HALmodeFlasher.visible=0
Timer054.enabled=0
end sub

sub Kicker017_hit 'for testing
'Plunger.Fire
'FlashForMs Flasher002, 2000, 100, 0
'FlashForMs F1A001, 2000, 100, 0
'FlashForMs GiFlipLH, 2000, 100, 1
'FlashForMs GiFlipRH, 2000, 100, 1
'quickflash
'LightSeq009.UpdateInterval = 25
'	LightSeq009.Play SeqRandom,10,,4000
'PicSelect 7
'kicker017.kick 180, 1
'dropprimitives
'flasher001.visible=1
'LowerPrimitive
'StartFadeOut
'Timer005.enabled=1
'kicker015.createball
'kicker015.kick 215, 11
'kicker017.destroyball
'GameDelayTimer.Enabled = True
'kicker015.createball
'kicker015.kick 215, 11
'bumps=99
'Kicker017.kick 0, 30
'timer008.enabled=1
End sub 

sub Trigger016_hit
 ActiveBall.Visible = False
end Sub


sub FadeOutTimer001_Timer
LowerPrimitive
StartFadeOut
FadeOutTimer001.enabled=0
end Sub


sub timer057_timer  'turn off whole table blinking
Light004.state=0
timer057.enabled=0
end Sub


dim flashstep
flashstep=0

sub quickflash
Flasher005.visible=1
Timer058.enabled=1
end Sub


sub timer058b_timer
if flashstep=0 Then
flasher005.visible=0
flashstep=1
end If
if flashstep=1 Then
'no flasher for the timer duration
flashstep=2
end If
if flashstep=2 Then
Light004.state=1
flashstep=3
end If
if flashstep=3 Then
Light004.state=0
flashstep=0
timer058.enabled=0
end If
end Sub


sub timer058_timer
if flashstep=3 Then
Light004.state=0
flashstep=0
timer058.enabled=0
end If
if flashstep=2 Then
'Light004.state=1
flashstep=3
end If
if flashstep=1 Then
'no flasher for the timer duration
flashstep=2
end If
if flashstep=0 Then
flasher005.visible=0
flashstep=1
end if
end Sub


' B2S Light Show
' cause i mean everyone loves a good light show
'1= the background
'2= left dude
'3= right dude
'4= left chick
'5= right chick
'6= robot
'7= robot eyes
'8= the logo 

' /////////////////////
' example B2S call 
' startB2S(#)   <---- this is the trigger code (if you want to add it to something like a bumper)

Dim b2sstep
b2sstep = 0
b2sflash.enabled = 0
Dim b2satm

Sub startB2S(aB2S)
    b2sflash.enabled = 1
    b2satm = ab2s
End Sub

Sub b2sflash_timer
    dim i
    If B2SBlink Then
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
                for i = 1 to 7
                    Controller.B2SSetData i, 0
                next
        End Select
     b2sstep = b2sstep + 1
    End If
End Sub

Dim toggleState1
toggleState1 = 0 ' Initial state

Sub Timer059_Timer()
    If toggleState1 = 0 Then
       Controller.B2SSetData 1, 0
       toggleState1 = 1
    Else
        Controller.B2SSetData 1, 1
        toggleState1 = 0
    End If
End Sub

Dim toggleState2
toggleState2 = 0 ' Initial state

Sub Timer060_Timer()
    If toggleState2 = 0 Then
       Controller.B2SSetData 2, 0
       toggleState2 = 1
    Else
        Controller.B2SSetData 2, 1
        toggleState2 = 0
    End If
End Sub

Dim toggleState3
toggleState3 = 0 ' Initial state

Sub Timer061_Timer()
    If toggleState3 = 0 Then
       Controller.B2SSetData 3, 0
       toggleState3 = 1
    Else
        Controller.B2SSetData 3, 1
        toggleState3 = 0
    End If
End Sub

Dim toggleState4
toggleState4 = 0 ' Initial state

Sub Timer062_Timer()
    If toggleState4 = 0 Then
       Controller.B2SSetData 4, 0
       Controller.B2SSetData 9, 0
       toggleState4 = 1
    Else
        Controller.B2SSetData 4, 1
        Controller.B2SSetData 9, 1
        toggleState4 = 0
    End If
End Sub

Dim toggleState5
toggleState5 = 0 ' Initial state

Sub Timer063_Timer()
    If toggleState5 = 0 Then
       Controller.B2SSetData 5, 0
       Controller.B2SSetData 10, 0
       toggleState5 = 1
    Else
        Controller.B2SSetData 5, 1
        Controller.B2SSetData 10, 1
        toggleState5 = 0
    End If
End Sub

Dim toggleState6
toggleState6 = 0 ' Initial state

Sub Timer064_Timer()
    If toggleState6 = 0 Then
       Controller.B2SSetData 6, 0
       toggleState6 = 1
    Else
        Controller.B2SSetData 6, 1
        toggleState6 = 0
    End If
End Sub

'*****begin HAL playing against himself************

sub trigger018_hit	
If Activeball.VelY < -5 Then Exit Sub
        Trigger020.Enabled = True
        Trigger023.Enabled = True
LeftFlipper.RotateToEnd
LeftFlipper002.RotateToEnd
' Flipper Sound Trigger 1
Dim flipSounds1, flipPick1
flipSounds1 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick1 = Int(Rnd * (UBound(flipSounds1) + 1))
PlaySound flipSounds1(flipPick1)
timer065.enabled=1
end Sub

sub timer065_timer
LeftFlipper.RotateToStart
LeftFlipper002.RotateToStart
if ballsonplayfield > 1 Then
Timer065.interval=35
end if
if ballsonplayfield = 1 Then
Timer065.interval=100
end if
PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
timer065.enabled=0
end Sub

sub trigger023_hit	
trigger018.enabled=1
Trigger019.enabled=1
Trigger023.enabled=0	
LeftFlipper.RotateToEnd
LeftFlipper002.RotateToEnd
' Flipper Sound Trigger 2
Dim flipSounds2, flipPick2
flipSounds2 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick2 = Int(Rnd * (UBound(flipSounds2) + 1))
PlaySound flipSounds2(flipPick2)

timer065.enabled=1
end Sub

sub trigger020_hit		
trigger018.enabled=1
Trigger019.enabled=1
trigger020.enabled=0
LeftFlipper.RotateToEnd
LeftFlipper002.RotateToEnd
' Flipper Sound Trigger 3
Dim flipSounds3, flipPick3
flipSounds3 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick3 = Int(Rnd * (UBound(flipSounds3) + 1))
PlaySound flipSounds3(flipPick3)

timer065.enabled=1
end Sub

sub trigger024_hit		
LeftFlipper.RotateToEnd
LeftFlipper002.RotateToEnd
' Flipper Sound Trigger 4
Dim flipSounds4, flipPick4
flipSounds4 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick4 = Int(Rnd * (UBound(flipSounds4) + 1))
PlaySound flipSounds4(flipPick4)

timer065.enabled=1
end Sub

sub trigger019_hit	
If Activeball.VelY < -5 Then Exit Sub
        Trigger022.Enabled = True
        Trigger021.Enabled = True
       RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
' Flipper Sound Trigger 5
Dim flipSounds5, flipPick5
flipSounds5 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick5 = Int(Rnd * (UBound(flipSounds5) + 1))
PlaySound flipSounds5(flipPick5)

Timer066.enabled=1
end Sub

sub trigger022_hit	
trigger018.Enabled=1
Trigger019.Enabled=1	
trigger022.enabled=0
RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
' Flipper Sound Trigger 6
Dim flipSounds6, flipPick6
flipSounds6 = Array("flipper_r01", "flipper_r02", "flipper_r03", "flipper_r04", "flipper_r05", "flipper_r06", "flipper_r07", "flipper_r08", "flipper_r09", "flipper_r10", "flipper_r11")
flipPick6 = Int(Rnd * (UBound(flipSounds6) + 1))
PlaySound flipSounds6(flipPick6)

Timer066.enabled=1
end Sub

sub trigger021_hit	
trigger018.enabled=1
Trigger019.enabled=1	
trigger021.enabled=0
RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
' Flipper Sound Trigger 7
Dim flipSounds7, flipPick7
flipSounds7 = Array("flipper_r01", "flipper_r02", "flipper_r03", "flipper_r04", "flipper_r05", "flipper_r06", "flipper_r07", "flipper_r08", "flipper_r09", "flipper_r10", "flipper_r11")
flipPick7 = Int(Rnd * (UBound(flipSounds7) + 1))
PlaySound flipSounds7(flipPick7)

Timer066.enabled=1
end Sub

sub trigger025_hit		
RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
Dim flipSounds8, flipPick8
flipSounds8 = Array("flipper_r01", "flipper_r02", "flipper_r03", "flipper_r04", "flipper_r05", "flipper_r06", "flipper_r07", "flipper_r08", "flipper_r09", "flipper_r10", "flipper_r11")
flipPick8 = Int(Rnd * (UBound(flipSounds8) + 1))
PlaySound flipSounds8(flipPick8)
Timer066.enabled=1
end Sub

sub Timer066_timer
RightFlipper.RotateToStart
RightFlipper001.RotateToStart
if ballsonplayfield > 1 Then
timer066.interval=35
end if
if ballsonplayfield = 1 Then
timer066.interval=100
end if
PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
Timer066.enabled=0
end Sub

sub Kicker025_Hit 'hal kicker for autoplay
Plunger.AutoPlunger = True
kicker025.timerenabled=1
end sub

sub kicker025_timer 'hal kicker for autoplay
kicker025.kick 0, 40
'playsound "fx_kicker"
SoundPlungerreleaseBall
plunger.fire
Plunger.AutoPlunger = False
kicker025.enabled=0
kicker025.timerenabled=0
end Sub

sub halgame
        if gameon=0 and CanStartGame = True Then
        trigger018.enabled=1
        Trigger019.enabled=1
        Trigger020.enabled=1
        Trigger021.enabled=1
        Trigger022.enabled=1
        Trigger023.enabled=1
        Trigger024.enabled=1
        Trigger025.enabled=1
        kicker025.enabled=1
        halplay=1
        StartGame
        end if
end sub

sub timer067_timer
halgame
end Sub


Sub Trigger026_Hit  'when hal is playing and a ball doesnt clear the launch lane, this will shoot it back out
if halplay=1 then
    activeball.VelY = -60
    PlaySound "fx_kicker"
end if
End Sub

'*****end HAL playing against himself************

'****BEGIN startgame. this sub was added so that HAL can start a game without an actual physical press of a button, he can call "startgame"

Sub StartGame()
    If gameon = 0 And numplay > 1 And CanStartGame = True Then
Stopsound "end1"
Stopsound "end2"
Stopsound "end3"
Stopsound "end4"
Stopsound "end5"
Stopsound "end6"
        HALmodeFlasher.visible=0
        timer032.enabled=0
        musicreset=0
		scoretimer.enabled=0
        dmdballnum = 1
        Controller.B2SSetScore 2, dmdballnum
        PicSelect 14
        flasher005.visible = 0
        Kicker039.enabled = 1
        target043.isdropped = 0
        Trigger004.enabled = 1
        kicker039Active = False
        BallSaveQueue = 0
        LastActivityTime = GameTime
        If usePUP = True Then
            ' POTENTIAL_INSERTPUP - Game started
        End If
        StopSpinning
        firstball = 1
        StopSound "2001_music"
         If firstgame = 1 Then
           ' playsound "thrust"
            MooveUpVaisseau
        End If
        GiOn
        qtimer.enabled = True
        DMDballnum = 1
        If ultramode = 1 Or usePUP = True Then
            qtimer.enabled = True
        End If
        KitOn
        lightseq001.StopPlay
        lightseq006.StopPlay
        lightseq007.StopPlay
        lightseq010.StopPlay
        light098.State = 0
        light099.State = 0
        light100.State = 0
        light101.State = 0
        light102.State = 0
        light103.State = 0
        light104.State = 0
        light105.State = 0
        light106.State = 0
        light107.State = 0
        light133.State = 2
        light120.State = 2
        light91.State = 2
        light116.State = 2
        extraball = 0
        multitimes = 0
        If usePUP = False Then
            Randomize
            Dim k: k = Int(Rnd * 9) + 1
            Select Case k
                Case 1: PlaySound "start1"
                Case 2: PlaySound "start2"
                Case 3: PlaySound "start3"
                Case 4: PlaySound "start4"
                Case 5: PlaySound "start5"
                Case 6: PlaySound "start6"
                Case 7: PlaySound "start7"
                Case 8: PlaySound "start8"
                Case 9: PlaySound "start9"
            End Select
        End If

        lockcheck = 0
        gameon = 1
        cp = 1
        tilted = 0
        emreel1.ResetToZero()
        p1score = 0
        cp = 2
        emreel2.ResetToZero()
        p2score = 0
        cp = 3
        emreel3.ResetToZero()
        p3score = 0
        cp = 4
        emreel4.ResetToZero()
        p4score = 0
        Controller.B2SSetScore 1, P1Score
        Controller.B2SSetScore 3, P3Score
        Controller.B2SSetScore 4, P4Score
        cp = 1
        multiplier = 1
        rickylockcount = 0
        multiball = 0
        assigncp
        Controller.B2SSetScorePlayer5 "1"
        BallInPlayReel.SetValue(1)
        ballstext.Text = "1"
        lockcheck = 0
        shootperm = 0
        loop1 = 0 : loop1count = 0
        loop2 = 0 : loop2count = 0
        loop3 = 0 : loop3count = 0
        sasave = 0

        If usePUP = True Then
            ballrelease.TimerEnabled = True
        Else
            fastrelease.Enabled = True
        End If
    End If
End Sub
'****END startgame. this sub was added so that HAL can start a game without an actual physical press of a button, he can call "startgame"

'********BEGIN when a ball hits HAL, each of the four kickers do this, differnet depending on how many balls are already in HAL
Sub consolidated
    RecordActivity

    LightSeq009.StopPlay
    LightSeq009.UpdateInterval = 5
    LightSeq009.Play SeqBlinking, , 1, 400

    If allfour = 1 And ballsinskull = 3 Then ' defeated HAL twice
        StopSpinner
        If usePUP = True Then
            Pupevent 846 ' POTENTIAL_INSERTPUP - battle against monolith begins
        End If
        AddScore 30000
        If ultramode = 1 Then
            DMD_DisplaySceneTextWithPause "", "+30,000", 3000
            qtimer.Interval = 1500
        End If 
        LightSeq005.Play SeqBlinking, , 15, 20 
        musicreset = 1
        kicker023.Enabled = 0
        Timer007.Enabled = 1
        If usePUP = True Then
            Timer004.Enabled = 1
        End If
        If usePUP = False Then
            Timer005.Enabled = 1
        End If

    ElseIf ballsinskull = 3 Then
        FlashForMs Flasher002, 2000, 100, 0
        FlashForMs F1A001, 2000, 100, 0
        FlashForMs F1A002, 2000, 100, 0
        FlashForMs F1A003, 2000, 100, 0
        FlashForMs F1A004, 2000, 100, 0
        FlashForMs F1A005, 2000, 100, 0
        FlashForMs F1A006, 2000, 100, 0
        FlashForMs F1A007, 2000, 100, 0
        FlashForMs GiFlipLH, 2000, 100, 1
        FlashForMs GiFlipRH, 2000, 100, 1

        kicker023.Enabled = 0
        Timer007.Enabled = 1

        If usePUP = True Then
            Pupevent 848 ' POTENTIAL_INSERTPUP - HAL defeated the first time he was hit by four Balls
        End If
        AddScore 15000
        If ultramode = 1 Then
            DMD_DisplaySceneTextWithPause "", "+15,000", 3000
            qtimer.Interval = 1500
        End If 
        LightSeq005.Play SeqBlinking, , 15, 20 
        allfour = allfour + 1
        kickback = 1
        Light095.State = 2
        Light096.State = 2
        Kicker009.Enabled = 1
        Kicker010.Enabled = 1

        ' PlaySound SoundFX("d1", DOFDropTargets)
        Dim sounds3, pick3
        sounds3 = Array("impact1", "impact2", "impact3")
        Randomize
        pick3 = Int(Rnd * UBound(sounds3) + 1)
        PlaySound sounds3(pick3)

        SkullShake
        PlaySound SoundFX("sound5", DOFDropTargets)
        ' PlaySound SoundFX("skullhit", DOFDropTargets)
        PlaySound "humansdefeated"
        SkullShake
        kickerskull

    ElseIf allfour = 1 And ballsinskull = 2 Then
        PicSelect 2
        StartSpinner
        If usePUP = True Then
            ' POTENTIAL_INSERTPUP - HAL says "my mind is going I can feel it" because he was hit by the seventh ball 
        End If
        AddScore 1000
        ballsinskull = ballsinskull + 1
        PlaySound SoundFX("mindgoing", DOFDropTargets)
        PicSelect 16
        Timer028.Enabled = 1

    ElseIf allfour = 1 And ballsinskull = 1 Then
        AddScore 1000
        If ultramode = 1 Then
            DMD_DisplaySceneTextWithPause "2 HITS LEFT", p1score, 10000
            qtimer.Interval = 1500 
        End If
        ballsinskull = ballsinskull + 1
        ' PlaySound SoundFX("d1", DOFDropTargets)
        Dim sounds111, pick111
        sounds111 = Array("impact1", "impact2", "impact3")
        Randomize
        pick111 = Int(Rnd * UBound(sounds111) + 1)
        PlaySound sounds111(pick111)
        Timer028.Enabled = 1

    ElseIf allfour = 1 And ballsinskull = 0 Then
        AddScore 1000
        If ultramode = 1 Then
            DMD_DisplaySceneTextWithPause "3 HITS LEFT", p1score, 10000
            qtimer.Interval = 1500 
        End If
        ballsinskull = ballsinskull + 1
        PlaySound SoundFX("afraid", DOFDropTargets)
        PicSelect 15
        Timer031.Enabled = 1

    ElseIf allfour = 0 And ballsinskull = 2 Then
        AddScore 1000
        If ultramode = 1 Then
            DMD_DisplaySceneTextWithPause "1 HIT LEFT", p1score, 10000
            qtimer.Interval = 1500 
        End If
        ballsinskull = ballsinskull + 1
        ' PlaySound SoundFX("d1", DOFDropTargets)
        Dim sounds222, pick222
        sounds222 = Array("impact1", "impact2", "impact3")
        Randomize
        pick222 = Int(Rnd * UBound(sounds222) + 1)
        PlaySound sounds222(pick222)
        Timer028.Enabled = 1

    ElseIf allfour = 0 And ballsinskull = 1 Then
        AddScore 1000
        If ultramode = 1 Then
            DMD_DisplaySceneTextWithPause "2 HITS LEFT", p1score, 10000
            qtimer.Interval = 1500 
        End If
        ballsinskull = ballsinskull + 1
        ' PlaySound SoundFX("d1", DOFDropTargets)
        Dim sounds333, pick333
        sounds333 = Array("impact1", "impact2", "impact3")
        Randomize
        pick333 = Int(Rnd * UBound(sounds333) + 1)
        PlaySound sounds333(pick333)
        Timer028.Enabled = 1

    ElseIf allfour = 0 And ballsinskull = 0 Then
        PicSelect 2
        AddScore 1000
        If ultramode = 1 Then
            DMD_DisplaySceneTextWithPause "3 HITS LEFT", p1score, 10000
            qtimer.Interval = 1500 
        End If
        ballsinskull = ballsinskull + 1
        PlaySound SoundFX("justwhat", DOFDropTargets)
        Timer028.Enabled = 1
    End If
End Sub

'******** END when a ball hits HAL, each of the four kickers do this, differnet depending on how many balls are already in HAL



sub timer029_timer  'starts the game when you interrupt hals game,wait for the table to be ready
    If gameon = 0 And numplay > 1 And CanStartGame = True Then
StartGame
timer029.enabled=0
end If
end Sub

sub StartGameMusic
if GameMusicOn = 1 and usepup=false then
    Dim songIndex
    songIndex = Int(Rnd * 8) + 1 ' generates a number from 1 to 8
Stopsound "song1"
Stopsound "song2"
Stopsound "song3"
Stopsound "song4"
Stopsound "song5"
Stopsound "song6"
Stopsound "song7"
Stopsound "song8"
StopSound "spaceflight"
StopSound "2001_music"
    Select Case songIndex
        Case 1: PlaySound "song1",-1
        Case 2: PlaySound "song2",-1
        Case 3: PlaySound "song3",-1
        Case 4: PlaySound "song4",-1
        Case 5: PlaySound "song4",-1
        Case 6: PlaySound "song3",-1
        Case 7: PlaySound "song7",-1
        Case 8: PlaySound "song8",-1
    End Select
end if
End Sub

sub StopGameMusic
if GameMusicOn = 1 and usepup=false then
Stopsound "song1"
Stopsound "song2"
Stopsound "song3"
Stopsound "song4"
Stopsound "song5"
Stopsound "song6"
Stopsound "song7"
Stopsound "song8"
StopSound "spaceflight"
StopSound "2001_music"
end if
end sub


'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.
' Create the following new collections:
'	 Metals (all metal objects, metal walls, metal posts, metal wire guides)
'	 Apron (the apron walls and plunger wall)
'	 Walls (all wood or plastic walls)
'	 Rollovers (wire rollover triggers, star triggers, or button triggers)
'	 Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
'	 Gates (plate gates)
'	 GatesWire (wire gates)
'	 Rubbers (all rubbers including posts, sleeves, pegs, and bands)
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
' Tutorial videos by Apophis
' Audio : Adding Fleep Part 1					https://youtu.be/rG35JVHxtx4?si=zdN9W4cZWEyXbOz_
' Audio : Adding Fleep Part 2					https://youtu.be/dk110pWMxGo?si=2iGMImXXZ0SFKVCh
' Audio : Adding Fleep Part 3					https://youtu.be/ESXWGJZY_EI?si=6D20E2nUM-xAw7xy


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
RollingSoundFactor = 1.1 / 5

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 3						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025 / 5			 'volume multiplier; must not be zero

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
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
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
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
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
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
			PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
			PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
			PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
			PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
			PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
			PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
			PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
			PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
			PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
			PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
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
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(ActiveBall.id) < 4) And cor.ballvely(ActiveBall.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft ActiveBall
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, ActiveBall
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, ActiveBall
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If ActiveBall.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If ActiveBall.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If ActiveBall.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If ActiveBall.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, ActiveBall
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
			PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)

	'FlipperCradleCollision ball1, ball2, velocity
'
'	Dim snd
'	Select Case Int(Rnd * 7) + 1
'		Case 1
'			snd = "Ball_Collide_1"
'		Case 2
'			snd = "Ball_Collide_2"
'		Case 3
'			snd = "Ball_Collide_3"
'		Case 4
'			snd = "Ball_Collide_4"
'		Case 5
'			snd = "Ball_Collide_5"
'		Case 6
'			snd = "Ball_Collide_6"
'		Case 7
'			snd = "Ball_Collide_7"
'	End Select
'	
'	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER   ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////


'******************************************************
'****  END FLEEP MECHANICAL SOUNDS
'******************************************************

'**********************************
' 	ZMAT: General Math Functions
'**********************************
' These get used throughout the script.

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function ArcCos(x)
	If x = 1 Then
		ArcCos = 0/180*PI
	ElseIf x = -1 Then
		ArcCos = 180/180*PI
	Else
		ArcCos = Atn(-x/Sqr(-x * x + 1)) + 2 * Atn(1)
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

' Used for drop targets
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function

'*******************************************
'  ZOPT: User Options
'*******************************************

'Dim LightLevel : LightLevel = 0.25				' Level of room lighting (0 to 1), where 0 is dark and 100 is brightest
Dim ColorLUT : ColorLUT = 1						' Color LUTs
Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.9 		' Level of ramp rolling volume. Value between 0 and 1
'Dim StagedFlippers : StagedFlippers = 0         ' Staged Flippers. 0 = Disabled, 1 = Enabled


' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
Dim dspTriggered : dspTriggered = False
Sub Table1_OptionEvent(ByVal eventId)
    If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If

	' Color Saturation

    ' Sound volumes
    VolumeDial = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.8, 1)
    RampRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.9, 1)
	'RampRollVolume = Table1.Option("Ramp Roll Volume", 0, 1, 0.01, 0.5, 1)
	'Room brightness
	'LightLevel = Table1.Option("Table Brightness (Ambient Light Level)", 0, 1, 0.01, .5, 1)
	'LightLevel = NightDay/100
	'SetRoomBrightness LightLevel   'Uncomment this line for lightmapped tables.
    'Staged Flippers
    'StagedFlippers = Table1.Option("Staged Flippers", 0, 1, 1, 0, 0, Array("Disabled", "Enabled"))

    If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub

'******************************************************
'	ZBRL: BALL ROLLING AND DROP SOUNDS
'******************************************************
'
' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

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

Sub RollingUpdate()
	Dim b', BOT
'	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(gBOT)
		If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		If gBOT(b).VelZ < -1 and gBOT(b).z < 55 and gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
	Next
End Sub

Sub ResetRolling()
    Dim i
    For i = 0 To tnob
        On Error Resume Next
        rolling(i) = False
        StopSound "BallRoll_" & i
        On Error GoTo 0
    Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************



'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. and, special scripting
'
' TriggerLF and RF should now be 27 vp units from the flippers. In addition, 3 degrees should be added to the end angle
' when creating these triggers.
'
' RF.ReProcessBalls Activeball and LF.ReProcessBalls Activeball must be added the flipper_collide subs.
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.4            | 0.4                   | 0.375                  | 0.375              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'
'        x.AddPt "Polarity", 0, 0, 0
'        x.AddPt "Polarity", 1, 0.05, - 2.7
'        x.AddPt "Polarity", 2, 0.16, - 2.7
'        x.AddPt "Polarity", 3, 0.22, - 0
'        x.AddPt "Polarity", 4, 0.25, - 0
'        x.AddPt "Polarity", 5, 0.3, - 1
'        x.AddPt "Polarity", 6, 0.4, - 2
'        x.AddPt "Polarity", 7, 0.5, - 2.7
'        x.AddPt "Polarity", 8, 0.65, - 1.8
'        x.AddPt "Polarity", 9, 0.75, - 0.5
'        x.AddPt "Polarity", 10, 0.81, - 0.5
'        x.AddPt "Polarity", 11, 0.88, 0
'        x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
'
'
''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 3.7
'		x.AddPt "Polarity", 2, 0.16, - 3.7
'		x.AddPt "Polarity", 3, 0.22, - 0
'		x.AddPt "Polarity", 4, 0.25, - 0
'		x.AddPt "Polarity", 5, 0.3, - 2
'		x.AddPt "Polarity", 6, 0.4, - 3
'		x.AddPt "Polarity", 7, 0.5, - 3.7
'		x.AddPt "Polarity", 8, 0.65, - 2.3
'		x.AddPt "Polarity", 9, 0.75, - 1.5
'		x.AddPt "Polarity", 10, 0.81, - 1
'		x.AddPt "Polarity", 11, 0.88, 0
'		x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
''*******************************************
''  Late 80's early 90's
'
'Sub InitPolarity()
'	dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 5
'		x.AddPt "Polarity", 2, 0.16, - 5
'		x.AddPt "Polarity", 3, 0.22, - 0
'		x.AddPt "Polarity", 4, 0.25, - 0
'		x.AddPt "Polarity", 5, 0.3, - 2
'		x.AddPt "Polarity", 6, 0.4, - 3
'		x.AddPt "Polarity", 7, 0.5, - 4.0
'		x.AddPt "Polarity", 8, 0.7, - 3.5
'		x.AddPt "Polarity", 9, 0.75, - 3.0
'		x.AddPt "Polarity", 10, 0.8, - 2.5
'		x.AddPt "Polarity", 11, 0.85, - 2.0
'		x.AddPt "Polarity", 12, 0.9, - 1.5
'		x.AddPt "Polarity", 13, 0.95, - 1.0
'		x.AddPt "Polarity", 14, 1, - 0.5
'		x.AddPt "Polarity", 15, 1.1, 0
'		x.AddPt "Polarity", 16, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945

'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'	LF.SetObjects "LF", LeftFlipper, TriggerLF
'	RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub

'*******************************************
' Early 90's and after

Sub InitPolarity()
	Dim x, a
	a = Array(LF, RF)
	For Each x In a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 5.5
		x.AddPt "Polarity", 2, 0.16, - 5.5
		x.AddPt "Polarity", 3, 0.20, - 0.75
		x.AddPt "Polarity", 4, 0.25, - 1.25
		x.AddPt "Polarity", 5, 0.3, - 1.75
		x.AddPt "Polarity", 6, 0.4, - 3.5
		x.AddPt "Polarity", 7, 0.5, - 5.25
		x.AddPt "Polarity", 8, 0.7, - 4.0
		x.AddPt "Polarity", 9, 0.75, - 3.5
		x.AddPt "Polarity", 10, 0.8, - 3.0
		x.AddPt "Polarity", 11, 0.85, - 2.5
		x.AddPt "Polarity", 12, 0.9, - 2.0
		x.AddPt "Polarity", 13, 0.95, - 1.5
		x.AddPt "Polarity", 14, 1, - 1.0
		x.AddPt "Polarity", 15, 1.05, -0.5
		x.AddPt "Polarity", 16, 1.1, 0
		x.AddPt "Polarity", 17, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.23, 0.85
		x.AddPt "Velocity", 2, 0.27, 1
		x.AddPt "Velocity", 3, 0.3, 1
		x.AddPt "Velocity", 4, 0.35, 1
		x.AddPt "Velocity", 5, 0.6, 1 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

	Next
	
	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before\
' modified 2024 by rothbauerw
' Added Reprocessballs for flipper collisions (LF.Reprocessballs Activeball and RF.Reprocessballs Activeball must be added to the flipper collide subs
' Improved handling to remove correction for backhand shots when the flipper is raised

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef, FlipStartAngle
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
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
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				balldata(x).Data = balls(x)
			End If
		Next
		FlipStartAngle = Flipper.currentangle
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

	Public Sub ReProcessBalls(aBall) 'save data of balls in flipper range
		If FlipperOn() Then
			Dim x
			For x = 0 To UBound(balls)
				If Not IsEmpty(balls(x)) Then
					if balls(x).ID = aBall.ID Then
						If isempty(balldata(x).ID) Then
							balldata(x).Data = balls(x)
						End If
					End If
				End If
			Next
		End If
	End Sub

	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef, BalltoFlip, BalltoBase, NoCorrection, checkHit
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					BalltoFlip = DistanceFromFlipperAngle(BallData(x).x, BallData(x).y, Flipper, FlipStartAngle)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
				NoCorrection = 1
			Else
				checkHit = 50 + (20 * BallPos) 

				If BalltoFlip > checkHit or (PartialFlipCoef < 0.5 and BallPos > 0.22) Then
					NoCorrection = 1
				Else
					NoCorrection = 0
				End If
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				'If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled and NoCorrection = 0 Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef*VelCoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
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
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	   Dim BOT
	   BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub


Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub
	


'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
 Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
	Const EOSReturn = 0.035  'mid 80's to early 90's
'   Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity
	
	Flipper.eostorque = EOST
	Flipper.eostorqueangle = EOSA
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
    Dim BOT, b

    FlipperPress = 0
    Flipper.eostorqueangle = EOSA
    Flipper.eostorque = EOST * EOSReturn / FReturn
	
    If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
        BOT = GetBalls   ' fresh list of valid balls on the table

        If IsArray(BOT) Then
            For b = 0 To UBound(BOT)
                If IsObject(BOT(b)) Then
                    ' check for cradle near this flipper
                    If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then
                        ' clamp downward speed a bit
                        If BOT(b).VelY >= -0.4 Then BOT(b).VelY = -0.4
                    End If
                End If
            Next
        End If
    End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 5  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const BaseDampen = 0.55

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
    Dim Dir, LiveDist
    Dir = Flipper.startangle / Abs(Flipper.startangle)    '-1 for Right Flipper
    Dim LiveCatchBounce   'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime
    CatchTime = GameTime - FCount
    LiveDist = Abs(Flipper.x - ball.x)

    If CatchTime <= LiveCatch And parm > 3 And LiveDist > LiveDistanceMin And LiveDist < LiveDistanceMax Then
        If CatchTime <= LiveCatch * 0.5 Then   'Perfect catch only when catch time happens in the beginning of the window
            LiveCatchBounce = 0
        Else
            LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)  'Partial catch when catch happens a bit late
        End If
        
        If LiveCatchBounce = 0 And ball.velx * Dir > 0 And LiveDist > 30 Then ball.velx = 0

        If ball.velx * Dir > 0 And LiveDist < 30 Then
            ball.velx = BaseDampen * ball.velx
            ball.vely = BaseDampen * ball.vely
            ball.angmomx = BaseDampen * ball.angmomx
            ball.angmomy = BaseDampen * ball.angmomy
            ball.angmomz = BaseDampen * ball.angmomz
        Elseif LiveDist > 30 Then
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx = 0
            ball.angmomy = 0
            ball.angmomz = 0
        End If
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
    End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************

'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub


Dim LS: Set LS = New SlingshotCorrection
Dim RS: Set RS = New SlingshotCorrection
Dim ULS: Set ULS = New SlingshotCorrection
Dim URS: Set URS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

	ULS.Object = LeftSlingShot002
	ULS.EndPoint1 = EndPoint1ULS
	ULS.EndPoint2 = EndPoint2ULS




	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS, ULS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

' The following sub are needed, however they exist in the ZMAT maths section of the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
'Function RotPoint(x,y,angle)
'	dim rx, ry
'	rx = x*dCos(angle) - y*dSin(angle)
'	ry = x*dSin(angle) + y*dCos(angle)
'	RotPoint = Array(rx,ry)
'End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			'   debug.print " "
		End If
	End Sub
End Class

'Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	'LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	'RightFlipperCollide parm
End Sub

Sub LeftFlipper001_Collide(parm)
	'LeftFlipperCollide parm
End Sub

Sub RightFlipper002_Collide(parm)
	'RightFlipperCollide parm
End Sub

'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

' Put all the Post and Pin objects in dPosts collection. Make sure dPosts fires hit events.
Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	'TargetBouncer ActiveBall, 1
End Sub

' This collection contains the bottom sling posts. They are not in the dPosts collection so that the TargetBouncer is not applied to them, but they should still have dampening applied
' If you experience airballs with posts or targets, consider adding them to this collection
Sub NoTargetBouncer_Hit
    RubbersD.dampen ActiveBall
End Sub

' Put all the Sleeve objects in dSleeves collection. Make sure dSleeves fires hit events.
Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	'TargetBouncer ActiveBall, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		aBall.velz = aBall.velz * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
			aBall.velz = aBall.velz * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

Dim gBot
Sub GameTimer_Timer()
  gBOT = GetBalls
  RollingUpdate
cor.Update
End Sub



'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9 	'Level of bounces. Recommmended value of 0.7-1.0

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

'****************************************************************
'****  END VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'****************************************************************



'******************************************************
' 	ZRRL: RAMP ROLLING SFX
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
dim RampBalls(5,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(5)	

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


'Ramp triggers
sub StartRamp_Hit()
	If activeball.velx < 0 Then
		WireRampOn True
	Else
		WireRampOff
	End If
End Sub

sub StartRamp2_Hit()
		WireRampOn False
End Sub

sub StartRamp3_Hit()
		WireRampOn False
End Sub

Sub StartRamp4_Hit()
		WireRampOn False
End Sub

Sub WireStart_Hit()
		WireRampOn False
End Sub

sub StartRamp5_Hit()
	If activeball.velx < 0 Then
		WireRampOn True
	Else
		WireRampOff
	End If
End Sub

Sub StartRamp6_Hit()
		WireRampOn False
End Sub

sub StartRampHit_Hit()
if ramp008.visible=0 then
	If activeball.vely < 0 Then
		WireRampOn True
	Else
		WireRampOff
	End If
end if
End Sub

sub StartWireRamp2_Hit()
	If activeball.vely < 0 Then
        WireRampOff
		WireRampOn False
	Else
        WireRampOn True
		WireRampOff
	End If
End Sub

Sub Rampend_hit()
		WireRampOff
End Sub

Sub Rampend3_hit()
		WireRampOff
End Sub

Sub Rampend2_hit()
	If activeball.vely < 0 Then
		WireRampOff
		Else
		WireRampOn True
	End If
End Sub

Sub trigger027_hit()
		WireRampOff
End Sub

Sub Rampend5_hit()
		WireRampOff
End Sub

Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol "wireramp_stop1", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub



'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************


