Option Explicit


'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = False             ' enable Pinup Player functions for this table
cPuPPack = "starwrs"    ' name of the PuP-Pack / PuPVideos folder for this table

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
DIM hardwareDOF
DIM threeballoption
B2SSCRIPTS = 1

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or keep it False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

dim saberenabled
dim showindex

dim PlayersPlayingGame
PlayersPlayingGame=1

dim testrank

'******USER SETTINGS ******
ultramode=1 '1 to use UltraDMD, 0 to disable UltraDMD 
GameMusicOn=1'1 to hear music during the game, 0 to disable music during the game
HardwareDOF=1 '1 to enable hardware DOF, 0 to disable hardware DOF. 
threeballoption=3 '3 for 3 balls, 5 for 5 balls (those are the only two options)
saberenabled=1 ' 1 for lightsaber during multiball, 0 to disable this
'******END USER SETTINGS ******

'******testing options
'only one or the other below can be used at a time
ballcount.enabled=0 '0=no counter shows, 1= counter shown
showindex=0 '1 to show index # of Gi effect
testrank=0 '1 to hear knocker sound when rank achiebed
'****** end testing options

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
' 113 
' 114 Kicker053
' 115 Kicker003
' 116 Kicker017
' 117 Kicker020
' 118 Kicker028
' 119
' 120 RightSlingShot005_Hit
' 121 Sub LeftSlingShot001_Hit
' 122 ----
' 123 BallRelease
' 124 
' 125 
' 126 
' 127 
' 128 
' 129 
' 130 Kicker007
' 131 kicker030
' 132 
' 133 
' 134 
' 135 kicker035
' 136 Kicker009
' 137 kicker035
' 138 kicker035
' 139 Kicker010
' 140 kicker001




'*************************************************************************************
' core.vbs constants

Dim B2SBlink
B2SBlink = 1 'enabled flashing lights on b2s
Const BallSize = 50  ' 50 is the normal size
Const BallMass = 1   ' 1 is the normal ball mass.
Const cGameName = "starwrs "
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
 Controller.B2SName = "starwrs"  ' 
if HardwareDOF=0 Then
 Controller.Run()
end If
END If
end Sub
Dim X
X=0

'left flipper Sound
Dim LF_Sounds
LF_Sounds = Array( _
  "flipper_l01","flipper_l02","flipper_l03","flipper_l04","flipper_l05", _
  "flipper_l06","flipper_l07","flipper_l08","flipper_l09","flipper_l10","flipper_l11" _
)
Function PickRand(arr)
  ' For Array(...) which is 0-based, this returns an index 0..UBound(arr)
  PickRand = arr(Int(Rnd * (UBound(arr) + 1)))
End Function

Sub PlayLeftFlipSound()
  Dim name: name = PickRand(LF_Sounds)
  PlaySound SoundFXDOF(name, 101, DOFOn, DOFContactors)
End Sub
'end left flipper sound

'right flipper sound
Dim RF_Sounds
RF_Sounds = Array( _
  "flipper_r01","flipper_r02","flipper_r03","flipper_r04","flipper_r05", _
  "flipper_r06","flipper_r07","flipper_r08","flipper_r09","flipper_r10","flipper_r11" _
)

Sub PlayRightFlipSound()
  Dim name: name = PickRand(RF_Sounds)
  PlaySound SoundFXDOF(name, 102, DOFOn, DOFContactors)
End Sub
'end right flipper sound

'left slingshot SoundCooldownTimerDim SlingL_Sounds
Dim SlingL_Sounds
SlingL_Sounds = Array( _
  "sling_l1","sling_l2","sling_l3","sling_l4","sling_l5", _
  "sling_l6","sling_l7","sling_l8","sling_l9","sling_l10" _
)
Sub PlaySlingLeftSound()
  Dim name: name = PickRand(SlingL_Sounds)
  PlaySound SoundFXDOF(name, 105, DOFPulse, DOFContactors)
End Sub
'end left Slingshot
' --- Globals ---
Dim SlingR_Sounds
SlingR_Sounds = Array( _
  "sling_r1","sling_r2","sling_r3","sling_r4", _
  "sling_r5","sling_r6","sling_r7","sling_r8" _
)

Sub PlaySlingRightSound()
  Dim name: name = PickRand(SlingR_Sounds)
  PlaySound SoundFXDOF(name, 104, DOFPulse, DOFContactors)
End Sub
'end right Slingshot
' --- Globals ---
Dim SlingL2_Sounds
SlingL2_Sounds = Array( _
  "sling_l1","sling_l2","sling_l3","sling_l4","sling_l5", _
  "sling_l6","sling_l7","sling_l8","sling_l9","sling_l10" _
)

Sub PlaySlingLeft2Sound()
  Dim name: name = PickRand(SlingL2_Sounds)
  PlaySound SoundFXDOF(name, 103, DOFPulse, DOFContactors)
End Sub
' --- Globals ---
Dim SlingR2_Sounds
SlingR2_Sounds = Array( _
  "sling_r1","sling_r2","sling_r3","sling_r4", _
  "sling_r5","sling_r6","sling_r7","sling_r8" _
)

Sub PlaySlingRight2Sound()
  Dim name: name = PickRand(SlingR2_Sounds)
  PlaySound SoundFXDOF(name, 120, DOFPulse, DOFContactors), 0, 0.1
End Sub
' --- Globals ---
Dim SlingR3_Sounds
SlingR3_Sounds = Array( _
  "sling_r1","sling_r2","sling_r3","sling_r4", _
  "sling_r5","sling_r6","sling_r7","sling_r8" _
)

Sub PlaySlingRight3Sound()
  Dim name: name = PickRand(SlingR3_Sounds)
  PlaySound SoundFXDOF(name, 121, DOFPulse, DOFContactors), 0, 0.1
End Sub
' --- Globals ---
Dim BumpersBottom_Sounds
BumpersBottom_Sounds = Array( _
  "bumpers_bottom_1","bumpers_bottom_2","bumpers_bottom_3", _
  "bumpers_bottom_4","bumpers_bottom_5" _
)

Sub PlayBumpersBottomSound()
  Dim name: name = PickRand(BumpersBottom_Sounds)
  PlaySound SoundFXDOF(name, 108, DOFPulse, DOFContactors)
End Sub
' --- Globals ---
Dim BumpersTop_Sounds
BumpersTop_Sounds = Array( _
  "bumpers_top_1","bumpers_top_2","bumpers_top_3", _
  "bumpers_top_4","bumpers_top_5" _
)

Sub PlayBumpersTopSound()
  Dim name: name = PickRand(BumpersTop_Sounds)
  PlaySound SoundFXDOF(name, 107, DOFPulse, DOFContactors)
End Sub
' --- Globals ---
Dim BumpersMiddle_Sounds
BumpersMiddle_Sounds = Array( _
  "bumpers_middle_1","bumpers_middle_2","bumpers_middle_3", _
  "bumpers_middle_4","bumpers_middle_5" _
)

Sub PlayBumpersMiddleSound()
  Dim name: name = PickRand(BumpersMiddle_Sounds)
  PlaySound SoundFXDOF(name, 109, DOFPulse, DOFContactors)
End Sub
' --- Globals ---
Dim DropTargetReset_Sounds
DropTargetReset_Sounds = Array( _
  "drop_target_reset_1","drop_target_reset_2","drop_target_reset_3", _
  "drop_target_reset_4","drop_target_reset_5","drop_target_reset_6" _
)

Sub PlayDropTargetResetSound()
  Dim name: name = PickRand(DropTargetReset_Sounds)
  PlaySound SoundFXDOF(name, 111, DOFPulse, DOFContactors)
End Sub
' --- Globals ---
Dim BallRelease_Sounds
BallRelease_Sounds = Array( _
  "ballrelease1","ballrelease2","ballrelease3","ballrelease4","ballrelease5" _
)

Sub PlayBallReleaseSound()
  Dim name: name = PickRand(BallRelease_Sounds)
  PlaySound name
End Sub
' --- Globals ---
Dim DropTargetDown_Sounds
DropTargetDown_Sounds = Array( _
  "drop_target_down_1","drop_target_down_2","drop_target_down_3", _
  "drop_target_down_4","drop_target_down_5" _
)

Sub PlayDropTargetDownSound()
  Dim name: name = PickRand(DropTargetDown_Sounds)
  PlaySound name
End Sub
' --- Globals ---
Dim DropTargetReset2_Sounds
DropTargetReset2_Sounds = Array( _
  "drop_target_reset_1","drop_target_reset_2","drop_target_reset_3", _
  "drop_target_reset_4","drop_target_reset_5","drop_target_reset_6" _
)

Sub PlayDropTargetReset2Sound()
  Dim name: name = PickRand(DropTargetReset2_Sounds)
  PlaySound SoundFXDOF(name, 112, DOFPulse, DOFContactors)
End Sub
' --- Globals ---
Dim WireLoop_Sounds
WireLoop_Sounds = Array( _
  "wireloop1","wireloop2","wireloop3","wireloop4", _
  "wireloop5","wireloop6","wireloop7","wireloop8" _
)

Sub PlayWireLoopSound()
  Dim name: name = PickRand(WireLoop_Sounds)
  PlaySound name
End Sub


Const VolumeDial = 1
Dim messageIndex
messageIndex = 0
dim halplay
Dim FirstGame
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
dim kicker023count
dim extraball
dim multitimes
dim disabled
dim kickback
dim bumps
dim bumpwin
dim hightext
dim HALtext
Dim HighScore1
Dim HighScore(4)
Dim HALScore 'AI score
dim DMDballnum
dim firstball  '1 means a ball has been played, 0 means a ball has not been played, this is for the startballcontrol trigger to decide what to do when hit
Dim BallSaveQueue
dim kicker039Active
dim kicker040Active
dim kicker041Active
dim showtwins
dim sabermode
dim addanotherball
addanotherball=2
' --- Mini-ball spawn debounce ---
Dim MB_NextAllowedSpawn : MB_NextAllowedSpawn = 0
Const MB_SPAWN_COOLDOWN = 80   ' ms between allowed spawns
showtwins=0
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
PlaySound "main_music", -1
end if

'**************setting defaults for when table loads 
KitOff
light035.state=1
light033.state=1
ramp015.collidable=0
ramp016.collidable=0
Ramp017.collidable=0
Ramp019.collidable=0
halplay=0
firstball = 1
disabled=0
kicker023count = 0
ballsonplayfield = 1
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
light008.state=0
light009.state=0
Timer025.Enabled = False
Timer039.Enabled = False
Timer046.Enabled = False
timer055.enabled = False
Timer001.enabled = False
fastrelease.enabled=False
ballrelease.timerenabled=False
qtimer.enabled=1
if usepup=true then
introtimer.enabled=1
end if

if ultramode=1 then
'scoretimer.enabled=1
qtimer.interval=1500
end If
kicker042.timerEnabled=0
Kicker006.timerEnabled=0
'Primitive013.visible = False

Light101.state=0
Light134.state=0

Light129.state=0


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

Dim l_guide

Sub table1_init()
FlipLights_On
'FlipLights_GoWild
startjohnny_timer 'starfield flasher animation
For Each l_guide In guidelights
    l_guide.State = 1
Next
Anim_Init
UMD_InitBonus
Randomize
'FZ_Init 'shows the number corresponding to gi light event, use fz_step
GIx_Init
LP_Prox_Init
'InitMiniBallCycle
P008_Init
InitP020
ST71_InitSpin
ST11_InitSpin
InitSaber
InitFlipperShadows
Start_Primitive034_Right
P031_Init
InitRollingSound
if HardwareDOF=1 Then
   LoadEM
end If
    ' Initalise the DMD display
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
attractmode
pupevent 902



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
If tilted = 1 Or gameon = 0 Then
else
	setscore points
          'extra ball 
          if p1score > 400000 and extraball=0 and gameon=1 then 
Timer001.enabled=True
startb2s(8)   
if usePUP=true Then
pupevent 878
'POTENTIAL_INSERTPUP - Extra ball awarded here at 400K points
end if
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "EXTRA BALL!", 3000
qtimer.interval=1500
end if
          playsound "knocker"
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
    Dim b : Set b = ActiveBall
    If Not b Is Nothing Then
        If OrbitSeen.Exists(b.ID) Then OrbitSeen.Remove b.ID
        If Orbit2Seen.Exists(b.ID) Then Orbit2Seen.Remove b.ID
    End If
UMD_RecordBallDrain
     Dim sounds92, pick92
    sounds92 = Array("Drain_1", "Drain_2", "Drain_3", "Drain_4", "Drain_5")
    
    pick92 = Int(Rnd * UBound(sounds92) + 1)
    PlaySound sounds92(pick92)
BallsOnPlayfield=BallsOnPlayfield-1


 If BallsOnPlayfield = 0 Then  'If last ball on playfield drained
FlipLights_Off
PlayRandomLost
UMD_OnBallDrain_ShowBonuses
'target007.isdropped=0
stopsaber
pupevent 953
delaymusic.enabled=0

stopsound "scary_mp3"
StopGameMusic
Flasher005.visible=1
StopLightSeq
'Light101.state=0
Light134.state=0
Light129.state=0
lightseq010.stopplay
stopsound "spaceflight"
'Stopsound "littlepigsaxe"
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

	if multiball = 0 then
	tilt = 0
	tilted = 0
	lockcheck = 0
	sasave = 0
	tilttext.text = ""
light008.state=0
light009.state=0
light055.state=0
light058.state=0
light059.state=0
'Primitive013.visible = False
Trigger004.enabled=1
if halplay=1 Then
kicker025.enabled=1
end If

   endballcase.enabled=1

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

sub endballcase_timer
FlipLights_blink
if ScoutRaised  = True Then
Scout_Lower
end If
 Select Case ballstext.text

   		Case "5" 
mynock_out
P008_ForceOff 'turn off lightsaber
   'Dim Score : Score = p1score  ' Replace with your actual score variable
Primitive036_Scheduler.Enabled = False
if usepup=true then
introtimer.enabled=1
end if
if kicker039Active = true then
CanStartGame = False
end if
if kicker039Active = False Then

end if
if kicker039Active = True then
GameDelayTimer.Enabled = True
RightSlingBlocker.collidable = True
end if
if usePUP=true Then
pupevent 804
pupevent 910'POTENTIAL_INSERTPUP - Gave Over, ball 5 lost
pupevent 944
end if
FirstGame = 0
loadhighscores()
LoadHalScores()
If p1score > HighScore1 and halplay=0 Then
        playsound "knocker"
if usePUP=true Then
'POTENTIAL_INSERTPUP - high score received
end if
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
        HALtext = "AI Score " & txtHALHigh.text
End If



Timer049.enabled=1 'makes table lighter again 8 second after game ends
attractmode
light035.state=1
light033.state=1
'pupevent 900
if usepup = false then
PlaySound "main_music", -1
end if
DMDballnum = 0
Controller.B2SSetScore 2, dmdballnum
if ultramode=1 then
'qtimer.enabled=0
loadhighscores
hightext = "HIGH SCORE " & Highscore1
'DMD_DisplaySceneTextWithPause hightext, p1score, 10000
delayscoretimer.enabled=1
'qtimer.interval=1500 
end if
timer025.enabled=1
'randomize game over soundbite	
if usepup=false Then
Timer082.enabled=1

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
'randomize game over soundbite			
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
        trigger018.enabled=0
        Trigger019.enabled=0
        Trigger020.enabled=0
        Trigger021.enabled=0
        Trigger022.enabled=0
        Trigger023.enabled=0
        kicker025.enabled=0
'primitive013.visible = True
startfadein 'show star wars r2
Primitive015.visible=0
kicker039.enabled=0
kicker040.enabled=0
Kicker041.enabled=0
target043.isdropped=1
kicker039.kick 180, 1
kicker040.kick 180, 1
kicker041.kick 180, 1
Kicker040.enabled=false
Kicker041.enabled=false
kicker042.enabled=false
Kicker009.enabled=0
Kicker010.enabled=0
ramp015.collidable=0 'tiny ball save ramps
ramp016.collidable=0 'tiny ball save ramps
Ramp017.collidable=0 'tiny ball save ramps
Ramp019.collidable=0 'tiny ball save ramps
light095.state=0
Light096.state=0
Kicker010.enabled=0
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
 endballcase.enabled=0
   		Case "4"
if usePUP=true Then
pupevent 803'POTENTIAL_INSERTPUP - Ball 4 lost
pupevent 943
end if
impatient.enabled=1
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
For Each l_guide In guidelights
    l_guide.State = 2
Next
if usepup=true Then
ballrelease.timerenabled=1
end if
if usepup=False Then
fastrelease.enabled=1
end If
              'show life systems critical" primitive
                   Primitive015.visible=0
                   'Primitive013.visible=0
                   Timer025.Enabled = 1
                   playsound "gothim"
					'FlashForMs Flasher002, 2000, 100, 0
              'end show life systems critical" primitive
endballcase.enabled=0
   		Case "3"
if usePUP=true Then
pupevent 802'POTENTIAL_INSERTPUP - Ball 3 lost
pupevent 942
end if
impatient.enabled=1
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
For Each l_guide In guidelights
    l_guide.State = 2
Next
if usepup=true Then
ballrelease.timerenabled=1
end if
if usepup=False Then
fastrelease.enabled=1
end If
endballcase.enabled=0
  		Case "2"
if usePUP=true Then
pupevent 801'POTENTIAL_INSERTPUP - Ball 2 lost
pupevent 940
end if
impatient.enabled=1
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
For Each l_guide In guidelights
    l_guide.State = 2
Next
if usepup=true Then
ballrelease.timerenabled=1
end if
if usepup=False Then
fastrelease.enabled=1
end If
endballcase.enabled=0
   		Case "1"
if usePUP=true Then
pupevent 800'POTENTIAL_INSERTPUP - Ball 1 lost
pupevent 940
end if
impatient.enabled=1
KitOn
if firstgame =1 Then
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
'BallsOnPlayfield=1
For Each l_guide In guidelights
    l_guide.State = 2
Next
if usepup=true Then
ballrelease.timerenabled=1
end if
if usepup=False Then
fastrelease.enabled=1
end If
endballcase.enabled=0
   		Case "999" 'added to manage extra ball when achieved on ball one (cant go to ball 0 so used 999 instead)
if usePUP=true Then
pupevent 800'POTENTIAL_INSERTPUP - Ball 1 lost
pupevent 940
end if
impatient.enabled=1
if firstgame=1 Then
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
if usepup=true Then
ballrelease.timerenabled=1
end if
if usepup=False Then
fastrelease.enabled=1
end If
endballcase.enabled=0
	   	End Select
end Sub



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


Dim SoundReady: SoundReady = True

Sub Table1_KeyDown(ByVal keycode)

If keycode = LeftMagnaSave Then
'mynock_in
'Scout_Raise
    ' simple debounce so repeated/dual keycodes can't double-spawn
    If GameTime < MB_NextAllowedSpawn Then Exit Sub
    MB_NextAllowedSpawn = GameTime + MB_SPAWN_COOLDOWN

    SpawnMiniBallAt Kicker002, 20
    If usepup = False Then
    End If
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
		PlaySound "plungerpull",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	If keycode = LeftFlipperKey Then
Primitive005.BlendDisableLighting = 2 'jabbas eyes light up
if gameon = 0 Then
else
   if tilted=1 Then
       else
          if disabled = 1 Then
        else
if sabermode=true then
    If Rnd < 0.5 Then
        PlayRandomSaber
        Else
    End If
SaberRotateCW
end if
        LeftFlipper.TimerEnabled = True 'This line is only for ninuzzu's flipper shadows!
		LF.fire 'LeftFlipper.RotateToEnd
        FlipperActivate LeftFlipper, LFPress
    PlayLeftFlipSound
  
  end if
   end if
end If
	End If

	If keycode = RightFlipperKey Then
Primitive005.BlendDisableLighting = 0 'jabbas eyes turn off
if gameon = 0 Then
else
  if tilted=1 Then
else
         if disabled = 1 Then
        else
if sabermode=true then
    If Rnd < 0.5 Then
        PlayRandomSaber
        Else
    End If
SaberRotateCCW
end if
        'RightFlipper.TimerEnabled = True 'This line is only for ninuzzu's flipper shadows!
		RF.fire 'RightFlipper.RotateToEnd
        FlipperActivate RightFlipper, RFPress
        'RightFlipper001.RotateToEnd
 PlayRightFlipSound

end if
end if
end if
	End If

	if keycode = AddCreditKey and gameon = 0 then
'Scout_lower

if usepup = false then
If SoundReady Then
    SoundReady = False

    ' Randomize and play sound
    PlaySound Array("10", "9", "8")(Int(Rnd * 3))

    ' Start a cooldown timer
    SoundCooldownTimer.Interval = 3000
    SoundCooldownTimer.Enabled = True
End If
end If

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
        ballstext.text = "5"
        trigger018.enabled=0
        Trigger019.enabled=0
        Trigger020.enabled=0
        Trigger021.enabled=0
        Trigger022.enabled=0
        Trigger023.enabled=0
        kicker025.enabled=0
        tilted = 1
		tilt = 0
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
        lightseq010.stopplay
        lightseq010.play SeqAllOff
        timer067.enabled=0
end if
			tilted = 1
			tilt = 0
			primitive015.visible=1
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
			'FlashForMs Flasher002, 2000, 100, 0
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
        lightseq010.stopplay
        lightseq010.play SeqAllOff
        timer067.enabled=0
end if
			tilted = 1
			tilt = 0
			primitive015.visible=1
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
			'FlashForMs Flasher002, 2000, 100, 0
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
        lightseq010.stopplay
        lightseq010.play SeqAllOff
        timer067.enabled=0
end if
			tilted = 1
			tilt = 0
			primitive015.visible=1
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
		PlaySound "plunger",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToStart
        FlipperDeActivate LeftFlipper, LFPress
               'LeftFlipper002.RotateToStart
		PlaySound SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
	End If

	If keycode = RightFlipperKey Then
     		RightFlipper.RotateToStart
FlipperDeActivate RightFlipper, RFPress
        'RightFlipper001.RotateToStart
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



Dim BIP
BIP = 0

Sub Plunger_Init()
	PlaySound SoundFX("ballrelease",DOFContactors), 0,1,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
End Sub


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

sub LeftSlingShot002_slingshot
gi002.state=1
timer008.enabled=1
ToggleBlastB2S
Rubber001.visible=1
timer086.enabled=1 'hides rubber
'PlaySound "left_slingshot"
    PlaySlingLeftSound
    addscore 100

Light134.state=1

	Me.TimerEnabled = 1
end Sub

Sub LeftSlingShot002_Timer

	Me.TimerEnabled = 0
End Sub

Sub RightSlingShot_Slingshot
ToggleBlastB2S
'If Int(Rnd * 20) = 0 Then
'    Flasher026.Visible = True
'    ' Optional: turn it off again after a short time
'    Flasher026.TimerInterval = 500 ' half a second
'    Flasher026.TimerEnabled = True
'End If
    'PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
PlaySlingRightSound 
RSling.Visible = 0
    RSling1001.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    addscore 100

light129.state=1

End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1001.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 0:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1

light129.state=0

End Sub

Sub LeftSlingShot_Slingshot
ToggleBlastB2S
Jabba083StartWobble

    'PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
PlaySlingLeft2Sound
    addscore 100    
LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    addscore 100

light134.State = 1

End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
light134.State = 0

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

'Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
'	Dim tmp
'    tmp = tableobj.y * 2 / table1.height-1
'    If tmp > 0 Then
'		AudioFade = Csng(tmp ^10)
'   Else
'        AudioFade = Csng(-((- tmp) ^10) )
'    End If
'End Function

Function AudioFade(b)
    Dim t: t = (b.Y * 2 / TableHeight) - 1
    If t > 0 Then
        AudioFade = CSng(t ^ 10)
    Else
        AudioFade = CSng(-((-t) ^ 10))
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
	ControlBallInPlay = false
    Controller.B2SSetScore 2, dmdballnum
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





'*****************************************
'	FLIPPER SHADOWS
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

' ---- Flipper shadows via one timer (nFozzy-friendly) ----
Dim FSLeft, FSRight  ' <-- starts with letters (valid)

Sub InitFlipperShadows()
    ' Rename these to your actual shadow object names
    Set FSLeft  = FlipperLSh
    Set FSRight = FlipperRSh
    ' Safety in case the timer wasn't set in the editor
    FlipperFX.Interval = 15
    FlipperFX.Enabled  = True
End Sub

Sub FlipperFX_Timer()
    ' Just mirror angle/position; nFozzy handles the physics
    If Not FSLeft Is Nothing Then
        FSLeft.RotZ   = LeftFlipper.CurrentAngle
        FSLeft.X      = LeftFlipper.X
        FSLeft.Y      = LeftFlipper.Y
        FSLeft.Visible = True
        glowbatleft.objrotz = LeftFlipper.CurrentAngle
        GlowBatLightLeft.y = 1795 - 123 + LeftFlipper.CurrentAngle
    End If

    If Not FSRight Is Nothing Then
        FSRight.RotZ   = RightFlipper.CurrentAngle
        FSRight.X      = RightFlipper.X
        FSRight.Y      = RightFlipper.Y
        FSRight.Visible = True
        glowbatright.objrotz = RightFlipper.CurrentAngle
        GlowBatLightRight.y =1795 - 123 - RightFlipper.CurrentAngle
    End If
End Sub



'*****************************************
'BALL SHADOW
'*****************************************
'==============================
' Ball Shadows (simple & robust)
'==============================
'==============================
' Ball Shadows (simple & robust)
'==============================

Const SHADOW_X_STRETCH_DIV = 7
Const SHADOW_Y_OFFSET      = 12
Const SHADOW_MIN_SIZE_X    = 5
Const SHADOW_SIZE_Y        = 5
Const SHADOW_SHOW_Z_MIN    = 20

Sub BallShadowUpdate_Timer()
    Dim balls, i, b, w

    ' Probe that BallShadow() is wired to real objects
    On Error Resume Next
    Dim probeShadow : Set probeShadow = BallShadow(0)
    If Err.Number <> 0 Then Err.Clear : On Error GoTo 0 : Exit Sub
    On Error GoTo 0

    balls = GetBalls()
    For i = 0 To UBound(balls)
        Set b = balls(i)

        w = 10 - (b.Z / 12)
        If w < SHADOW_MIN_SIZE_X Then w = SHADOW_MIN_SIZE_X

        BallShadow(i).X = b.X + (b.X - (TableWidth / 2)) / SHADOW_X_STRETCH_DIV
        BallShadow(i).Y = b.Y + SHADOW_Y_OFFSET
        BallShadow(i).Size_X  = w
        BallShadow(i).Size_Y  = SHADOW_SIZE_Y
        BallShadow(i).Visible = (b.Z > SHADOW_SHOW_Z_MIN)
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

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


'kickers

Sub kicker001_Hit() 'the right kicker that sends ball up to top of playfield
GIx_BigRampEffect
startjohnny_timer 'starfield flasher animation
playsound "lightspeed"
    LightSeq010.UpdateInterval = 10
    LightSeq010.Play SeqMiddleOutVertOn,50,2
pupevent 920

kicker001.TimerEnabled = 1
'if usepup=false then
If Rnd < 0.5 Then
   ' PlaySound "saucer_enter_1"
Else
    'PlaySound "saucer_enter_2"
End If
'end If

addscore 15000
'BallRescueTimer.Enabled = True
End Sub

Sub kicker001_Timer 'the right kicker that sends ball up to top of playfield

if usepup=True then
PlaySound SoundFXDOF("kickback2", 140, DOFPulse, DOFContactors)
end If
if usepup=False then
PlaySound SoundFXDOF("kickback2", 140, DOFPulse, DOFContactors)
end If
kicker001.Kick -87, 24
If Rnd < 0.5 Then
    kicker001.Kick -86, 24
Else
    kicker001.Kick -86, 28
End If
kicker001.TimerEnabled = 0
End Sub


'Gate
Sub Gate_Hit() 
playsound "gate"
addscore 200
End Sub


'*** these are the two small landmine looking slingshots above the main slingshots 

Sub RightSlingShot005_Hit() 
ToggleBlastB2S
    PlaySlingRight2Sound
addscore 200
End Sub


'*** END these are the two small landmine looking slingshots above the main slingshots 

' bumpers

Sub bumper006_hit()
Primitive009.BlendDisableLighting = 7
Timer007.enabled=1
	'playsound "fx_bumper1"
'FlashForMs F1A007, 100, 50, 0
PlayBumpersBottomSound
addscore 25
Light082.state=1
	Me.TimerEnabled = 1
BUMP.enabled = True
'try to get 100 bumps
bumps=bumps+1
if ultramode=1 and ballstext.text=1 and bumps = 1 then
DMD_DisplaySceneTextWithPause "I'm Your Father!", p1score, 10000
qtimer.interval=1500 
else
if bumps > 9 Then
Light098.state=1
if ultramode=1 and bumps = 10 then
'DMD_DisplaySceneTextWithPause "10 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 19 Then
Light099.state=1
if ultramode=1 and bumps = 20 then
'DMD_DisplaySceneTextWithPause "20 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 29 Then
Light100.state=1
if ultramode=1 and bumps = 30 then
'DMD_DisplaySceneTextWithPause "30 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 39 Then
Light101.state=1
if ultramode=1 and bumps = 40 then
'DMD_DisplaySceneTextWithPause "40 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 49 Then
Light102.state=1
if ultramode=1 and bumps = 50 then
'DMD_DisplaySceneTextWithPause "50 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 59 Then
Light103.state=1
if ultramode=1 and bumps = 60 then
'DMD_DisplaySceneTextWithPause "60 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 69 Then
Light104.state=1
if ultramode=1 and bumps = 70 then
'DMD_DisplaySceneTextWithPause "70 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 79 Then
Light105.state=1
if ultramode=1 and bumps = 80 then
'DMD_DisplaySceneTextWithPause "80 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 89 Then
Light106.state=1
if ultramode=1 and bumps = 90 then
'DMD_DisplaySceneTextWithPause "90 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500 
Else
if bumps > 99 and bumpwin=0 Then
InitTieFlyCircleBack2
playsound "fastping"
startb2s(8)   
bumpwin=1
'Light004.state=2  'flash whole table

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
light082.state=0
	Me.Timerenabled = 0
End Sub

sub Timer005_timer
Primitive007.BlendDisableLighting = 0.5
timer005.enabled=0
end sub

sub Timer006_timer
Primitive061.BlendDisableLighting = 0.5
Timer006.enabled=0
end sub

sub Timer007_timer
Primitive009.BlendDisableLighting = 0.5
Timer007.enabled=0
end sub

Sub bumper004_hit()
'playsound "fx_bumper1"
'FlashForMs F1A005, 100, 50, 0
PlayBumpersTopSound
addscore 25
Primitive007.BlendDisableLighting = 7
timer005.enabled=1
Light008.State = 1
Me.TimerEnabled = 1
BUMP.enabled = True
'try to get 100 bumps
bumps=bumps+1
if ultramode=1 and ballstext.text=1 and bumps = 1 then
'DMD_DisplaySceneTextWithPause "TRY FOR 100 BUMPERS", p1score, 10000
'qtimer.interval=1500 
end if 
if bumps > 9 Then
Light098.state=1
if ultramode=1 and bumps = 10 then
'DMD_DisplaySceneTextWithPause "10 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 19 Then
Light099.state=1
if ultramode=1 and bumps = 20 then
'DMD_DisplaySceneTextWithPause "20 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 29 Then
Light100.state=1
if ultramode=1 and bumps = 30 then
'DMD_DisplaySceneTextWithPause "30 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 39 Then
Light101.state=1
if ultramode=1 and bumps = 40 then
'DMD_DisplaySceneTextWithPause "40 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 49 Then
Light102.state=1
if ultramode=1 and bumps = 50 then
'DMD_DisplaySceneTextWithPause "50 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 59 Then
Light103.state=1
if ultramode=1 and bumps = 60 then
'DMD_DisplaySceneTextWithPause "60 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 69 Then
Light104.state=1
if ultramode=1 and bumps = 70 then
'DMD_DisplaySceneTextWithPause "70 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 79 Then
Light105.state=1
if ultramode=1 and bumps = 80 then
'DMD_DisplaySceneTextWithPause "80 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 89 Then
Light106.state=1
if ultramode=1 and bumps = 90 then
'DMD_DisplaySceneTextWithPause "90 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500 
Else
if bumps > 99 and bumpwin=0 Then
InitTieFlyCircleBack2
playsound "fastping"
startb2s(8)   
bumpwin=1

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
Primitive061.BlendDisableLighting = 7
Timer006.enabled=1
'DSStartWobble
	'playsound "fx_bumper2"
FlashForMs F1A006, 100, 50, 0
    Dim sounds8, pick8
PlayBumpersMiddleSound  
addscore 25
	Light009.State = 1
	Me.TimerEnabled = 1
BUMP.enabled = True
'try to get 100 bumps
bumps=bumps+1
if ultramode=1 and ballstext.text=1 and bumps = 1 then
DMD_DisplaySceneTextWithPause "Join the dark side!", p1score, 10000
qtimer.interval=1500 
else
if bumps > 9 Then
Light098.state=1
if ultramode=1 and bumps = 10 then
'DMD_DisplaySceneTextWithPause "10 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 19 Then
Light099.state=1
if ultramode=1 and bumps = 20 then
'DMD_DisplaySceneTextWithPause "20 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 29 Then
Light100.state=1
if ultramode=1 and bumps = 30 then
'DMD_DisplaySceneTextWithPause "30 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 39 Then
Light101.state=1
if ultramode=1 and bumps = 40 then
'DMD_DisplaySceneTextWithPause "40 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 49 Then
Light102.state=1
if ultramode=1 and bumps = 50 then
'DMD_DisplaySceneTextWithPause "50 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 59 Then
Light103.state=1
if ultramode=1 and bumps = 60 then
'DMD_DisplaySceneTextWithPause "60 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 69 Then
Light104.state=1
if ultramode=1 and bumps = 70 then
'DMD_DisplaySceneTextWithPause "70 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 79 Then
Light105.state=1
if ultramode=1 and bumps = 80 then
'DMD_DisplaySceneTextWithPause "80 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500
Else 
if bumps > 89 Then
Light106.state=1
if ultramode=1 and bumps = 90 then
'DMD_DisplaySceneTextWithPause "90 BUMPERS HIT!", p1score, 10000
'qtimer.interval=1500 
Else
if bumps > 99 and bumpwin=0 Then
InitTieFlyCircleBack2
playsound "fastping"
startb2s(8)   
bumpwin=1

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
PlaySound SoundFX("laser_spin", DOFDropTargets)
AddScore 25
End Sub


'Rollovers
Sub RightInlane001_Hit
GIx_MediumEffect
'PlaySound SoundFX("sidelane", DOFDropTargets)
LightsDown
Addscore 500
 LightSeq1.Play SeqBlinking, , 15, 20
End Sub
 
Sub LeftInlane002_Hit
GIx_MediumEffect
'PlaySound SoundFX("sidelane", DOFDropTargets)
LightsDown
Addscore 500
 LightSeq1.Play SeqBlinking, , 15, 20
End Sub

'Rollovers
Sub RightInlane003_Hit
GIx_MediumEffect
if halplay=1 Then
trigger021.enabled=0
trigger022.enabled=1
Addscore 500
end if

rightInlaneSpeedLimit
PlaySound SoundFX("sound6", DOFDropTargets)
LightsDown
Addscore 100
 LightSeq1.Play SeqBlinking, , 15, 20
End Sub
 
Sub LeftInlane_Hit
GIx_MediumEffect
leftInlaneSpeedLimit
PlaySound SoundFX("sound6", DOFDropTargets)
LightsDown
Addscore 500
 LightSeq1.Play SeqBlinking, , 15, 20
End Sub



'****Rsling animation (the two small landmine looking primitives, one on each side, that act act slingshots above main slingshots)


' ******skull animation (shakes the top left skull once the fourth ball hits it 


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
 
Sub Trigger004_Hit() 'a new ball was just launched, this trigger was hit so 
scoretimer.enabled=0 'prevents bug where dmd info starts cycling immediately after game ends if you played a game after HAL
FlipLights_On
For Each l_guide In guidelights
    l_guide.State = 1
Next
UMD_OnNextBallStart_Reset 'resets dmd bonus counters
Start_Primitive034_Right 'start star destroyer moving if it's not already moving
Trigger004.enabled=0
qtimer.enabled=1
If Rnd < 0.5 Then
    LightSeq010.UpdateInterval = 1
    LightSeq010.Play SeqUpOn,75,1
Else
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqLeftOn,25,1
End If
if dmdballnum=4 then 
end if
if dmdballnum=5 then 
end if

impatient.enabled=0
If Not (dmdballnum = 1 And extraball = 0) Then
    StartGameMusic
End If

startb2s(8)                  'sets the number of balls on playfield to 1 and turns off shoot again light
if dmdballnum=3 Then
if usepup=false Then
playsound "play"
end If
if usepup=false Then
end if
end if
Light101.state=0
Light134.state=0
Light129.state=0
Timer001.enabled=False
if usePUP=true and DMDballnum=1 Then
pupevent 811:pupevent 816:pupevent 950'POTENTIAL_INSERTPUP - ball 1 launched"
end if
if  usePUP=true and DMDballnum=2 Then
pupevent 812:pupevent 817:pupevent 951'POTENTIAL_INSERTPUP - ball 2 launched"
end if
if  usePUP=true and DMDballnum=3 Then
pupevent 813:pupevent 818:pupevent 952'POTENTIAL_INSERTPUP - ball 3 launched"
end if
if  usePUP=true and DMDballnum=4 Then
pupevent 814:pupevent 819:pupevent 954'POTENTIAL_INSERTPUP - ball 4 launched"
end if
if  usePUP=true and DMDballnum=5 Then
pupevent 815:pupevent 820:pupevent 955'POTENTIAL_INSERTPUP - ball 5 launched"
end if
if  DMDballnum=5 Then

end If

addscore 500

if ultramode=1 and dmdballnum=1 then
DMD_DisplaySceneTextWithPause "Use the force!", p1score, 10000
qtimer.interval=1500 
end if
if dmdballnum=1 then

end if
KitOff
 LightSeq001.UpdateInterval = 3
    LightSeq001.Play SeqUpOn,75,2
ballsonplayfield = 1
shootagain = 1
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
        kickback = 1
        Light095.State = 2
        Light096.State = 2
        Kicker009.Enabled = 1
        Kicker010.Enabled = 1
End Sub


sub kicker039_hit()
'Target007.isdropped=0
Kicker017.enabled=1
StartBallControl.enabled=0
startb2s(8)   

if usePUP=true Then
pupevent 821'POTENTIAL_INSERTPUP - Ball 1 locked
end if
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "BALL 1 LOCKED", 3000
qtimer.interval=1500 
end if
qtimer.interval=1500 
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqRightOn,25,2
addscore 1000
kicker039Active = True
kicker040.timerenabled=True
playsound "impact3"
PlayBallReleaseSound
'FlashForMs Flasher002, 2000, 100, 0
FlashForMs F1A001, 1000, 100, 0
FlashForMs F1A002, 1000, 100, 0
FlashForMs F1A003, 1000, 100, 0
FlashForMs F1A004, 1000, 100, 0
'FlashForMs F1A005, 1000, 100, 0
FlashForMs F1A006, 1000, 100, 0
'FlashForMs F1A007, 1000, 100, 0
'FlashForMs GiFlipLH, 1000, 100, 1
'FlashForMs GiFlipRH, 1000, 100, 1
ChangeGi darkblue
ChangeBumperLight darkblue
Timer016.Enabled = 1
End Sub

sub kicker040_timer
kicker040.enabled=True
kicker040.timerenabled=False
end Sub

sub kicker040_hit()
'Target007.isdropped=0
kicker040Active = True
Kicker017.enabled=1
StartBallControl.enabled=0
startb2s(8)   

if usePUP=true Then
pupevent 822'POTENTIAL_INSERTPUP - Ball 2 locked
end if
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "BALL 2 LOCKED", 3000
qtimer.interval=1500 
end if
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqRightOn,25,2
addscore 1500
Kicker041.timerenabled=True
playsound "fx_collide"
PlayBallReleaseSound
'FlashForMs Flasher002, 2000, 100, 0
FlashForMs F1A001, 1000, 100, 0
FlashForMs F1A002, 1000, 100, 0
FlashForMs F1A003, 1000, 100, 0
FlashForMs F1A004, 1000, 100, 0
'FlashForMs F1A005, 1000, 100, 0
FlashForMs F1A006, 1000, 100, 0
'FlashForMs F1A007, 1000, 100, 0
'FlashForMs GiFlipLH, 1000, 100, 1
'FlashForMs GiFlipRH, 1000, 100, 1
ChangeGi red
ChangeBumperLight red
Timer016.Enabled = 1
end Sub

sub kicker041_timer
Kicker041.enabled=True
Kicker041.timerenabled=False
end Sub


sub kicker041_hit()
If Rnd < 0.5 Then
   InitTieFlyCircleBack
Else
   InitTieFlyCircleBack2
End If
'Target007.isdropped=0
closemulti
kicker041Active = True
Kicker017.enabled=1
StartBallControl.enabled=0
startb2s(8)   

if usePUP=true Then
pupevent 823'POTENTIAL_INSERTPUP - Ball 3 locked
end if
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "hit a ramp!", 3000
qtimer.interval=1500 
end if
    LightSeq010.UpdateInterval = 5
    LightSeq010.Play SeqRightOn,25,2
addscore 2000
Timer021.enabled=True
playsound "fx_collide"
PlayBallReleaseSound
'playsound "argue"
'FlashForMs Flasher002, 2000, 100, 0
FlashForMs F1A001, 1000, 100, 0
FlashForMs F1A002, 1000, 100, 0
FlashForMs F1A003, 1000, 100, 0
FlashForMs F1A004, 1000, 100, 0
'FlashForMs F1A005, 1000, 100, 0
FlashForMs F1A006, 1000, 100, 0
'FlashForMs F1A007, 1000, 100, 0
'FlashForMs GiFlipLH, 1000, 100, 1
'FlashForMs GiFlipRH, 1000, 100, 1
ChangeGi darkblue
ChangeBumperLight darkblue
Timer016.Enabled = 1
end Sub

sub timer021_timer
Kicker042.enabled=True
Timer021.enabled=False
end Sub

sub kicker042_hit()
If Rnd < 0.5 Then
   InitTieFlyCircleBack
Else
   InitTieFlyCircleBack2
End If
'Target007.isdropped=0
ST11_StartSpin 1800, 700, True
kicker039Active = False
kicker040Active = False
kicker041Active = False
If Rnd < 0.5 Then
   Start_Primitive033_SpinHalfway
Else
    Start_Primitive033_OneWay
End If
    If Rnd < 0.5 Then
        PlaySound "r2"
R2StartWobble
    Else
        PlaySound "wonderful"
C3StartWobble
    End If

Kicker017.enabled=1

startb2s(8)   
'Light004.state=2  'flash whole table
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "MULTIBALL", 3000
qtimer.interval=1500
end if
LightSeq009.UpdateInterval = 1
LightSeq009.Play SeqClockRightOn,360,1
LightSeq008.UpdateInterval = 1.2
LightSeq008.Play SeqClockRightOn,360,2
if ballsonplayfield = 1 then
If Rnd < 0.5 Then
startsaber
  MultiBallRelease.enabled=1
playsound "drinkup"
StartFade029
'POTENTIAL_INSERTPUP - ghost multiball
Else
startsaber
MultiBallRelease.enabled=1
StartFade029
playsound "drinkup"
'ghostmulti
pupevent 824'POTENTIAL_INSERTPUP - Begin 5 ball multiball (ball shoots up from below flippers)
End If
Else
if usePUP=true Then
pupevent 825'POTENTIAL_INSERTPUP - Begin 4 ball multiball (formerly known as APE MULTIBALL)
end if
addscore 2500
LightSeq007.UpdateInterval = 1
LightSeq007.Play SeqBlinking, , 15, 20 
LightSeq007.Play SeqBlinking, , 15, 10
'playsound "eject1"
'FlashForMs Flasher002, 2000, 100, 0
FlashForMs F1A001, 1000, 100, 0
FlashForMs F1A002, 1000, 100, 0
FlashForMs F1A003, 1000, 100, 0
FlashForMs F1A004, 1000, 100, 0
'FlashForMs F1A005, 1000, 100, 0
FlashForMs F1A006, 1000, 100, 0
'FlashForMs F1A007, 1000, 100, 0
'FlashForMs GiFlipLH, 1000, 100, 1
'FlashForMs GiFlipRH, 1000, 100, 1
ChangeGi red
ChangeBumperLight red
ballsonplayfield = ballsonplayfield+3
Ramp026.collidable=1 'prevents them rolling stright down the Drain
Timer002.enabled=1 'makes ramp026 collidable a few seconds later

Target043.isdropped=True
kicker039.enabled=False
kicker040.enabled=False
Kicker041.enabled=False
Kicker042.enabled=False
kicker039.kick 180, 1
playsound "scary_sound"
kicker040.kick 180, 1
kicker041.kick 180, 1
kicker042.kick 180, 1
LightSeq1.Play SeqBlinking, , 15, 20
kicker042.timerEnabled=1
multitimes = multitimes + 1
end if
end Sub

sub kicker042_timer()
Target043.isdropped=false
'zCol_Rubber_Post012.collidable=1
kicker039.enabled=True
kicker042.timerEnabled=0
end Sub


'*************************************************************

Sub target039_hit
PlayDropTargetDownSound
if light006.state=1 and light023.state=1 and  light040.state=1 Then
addscore 1500
'PlaySound SoundFX("targ", DOFDropTargets)
  else

if light006.state=1 and light023.state=1 and  light040.state=0 Then
addscore 1000
'PlaySound SoundFX("targ", DOFDropTargets)
light040.state=1
else

if light023.state=1 and light006.state=0 then 
addscore 750
'PlaySound SoundFX("targ", DOFDropTargets)
light006.state=1
else

if light023.state=0 then
addscore 500
'PlaySound SoundFX("targ", DOFDropTargets)
light023.state=1
end If
end If
end if
end If
end Sub
'********************************************************

Sub target040_hit
PlayDropTargetDownSound
if Light025.state=1 and Light026.state=1 and  Light027.state=1 Then
addscore 1500
'PlaySound SoundFX("targ", DOFDropTargets)
else
  if light025.state=1 and light026.state=1 and  Light027.state=0 Then
addscore 1000
'PlaySound SoundFX("targ", DOFDropTargets)
Light027.state=1
else

if Light025.state=1 and Light026.state=0 then 
if showtwins=0 Then
'playsound "play"
end if
if usePUP=true Then
'pupevent 'POTENTIAL_INSERTPUP - girls/twins appear
pupevent 935
end if
timer033.enabled=1
InitTieFlyCircleBack
addscore 750

if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "impossible!!", 3000
qtimer.interval=1500
end if
'PlaySound SoundFX("targ", DOFDropTargets)
Light026.state=1
else

if Light025.state=0 then
addscore 500
'PlaySound SoundFX("targ", DOFDropTargets)
Light025.state=1
end If
end If
end if
end If
end Sub

'**************************************************

Sub target041_hit
PlayDropTargetDownSound

if light034.state=1 and light031.state=1 and  light030.state=1 Then
addscore 1500
'PlaySound SoundFX("targ", DOFDropTargets)
  light030.state=1
else

if light034.state=1 and light031.state=1 and  light030.state=0 Then
addscore 1000
'PlaySound SoundFX("targ", DOFDropTargets)
light030.state=1
else

if light034.state=1 and light031.state=0 then 
If Rnd < 0.5 Then
   Start_Primitive037_Run
Else
    Start_Primitive042_Run
End If
addscore 750
'PlaySound SoundFX("targ", DOFDropTargets)
Light031.state=1
else

if Light034.state=0 then
addscore 500
'PlaySound SoundFX("targ", DOFDropTargets)
Light034.state=1
end If
end If
end if
end if
end Sub

'****************************************************************************
Sub target042_hit
PlayDropTargetDownSound
if Light021.state=1 and Light022.state=1 and Light024.state=1 Then
addscore 1500
'PlaySound SoundFX("targ", DOFDropTargets)
  else

if Light021.state=1 and Light022.state=1 and  Light024.state=0 Then
addscore 1000
'PlaySound SoundFX("targ", DOFDropTargets)
Light024.state=1
else

if Light021.state=1 and Light022.state=0 then 
addscore 750
'PlaySound SoundFX("targ", DOFDropTargets)
Light022.state=1
else

if Light021.state=0 then
addscore 500
'PlaySound SoundFX("targ", DOFDropTargets)
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
GIx_SmallEffect
   q = 0 
   For each target in DropTargets3
       q = q + target.isDropped
   Next 
   If q = 4 Then
      Scout_Raise
      AddScore 5000
      For each target in DropTargets3
         target.isDropped = False
  PlayDropTargetReset2Sound
     Next 
   '  TurnOnTargetLights
   End If
End Sub



sub Target047_hit
'LightSeq007.UpdateInterval = 10
'LightSeq007.Play SeqBlinking,, 5, 10
addscore 750
PlaySound SoundFX("targ", DOFDropTargets)
end Sub


sub Target051_hit
If BallsOnPlayfield = 1 Or BallsOnPlayfield = 2 Then
    If Rnd < 0.2 Then mynock_in
End If
addscore 750
PlaySound SoundFX("targ", DOFDropTargets)
end Sub

sub kicker054_hit
PlaySound "rampsound"
ramp007.visible=0
ramp007.collidable=1
primitive038.visible=True
primitive039.visible=False
ramp008.visible=0
'light062.state=0
if ballsonplayfield=1 then
startb2s(8)   

if usePUP=true Then
pupevent 827'POTENTIAL_INSERTPUP - Begin 2 ball multiball
end if
addscore 1500
PlaySound SoundFX("multiball", DOFDropTargets)
LightSeq010.Stopplay
LightSeq010.UpdateInterval = 25
	LightSeq010.Play SeqRandom,20,,2000
kicker054.destroyball
kicker054.createball
kicker055.createball
kicker054.kick -30, 30
kicker055.kick -30, 30
'FlashForMs Flasher002, 2000, 100, 0
FlashForMs F1A001, 1000, 100, 0
FlashForMs F1A002, 1000, 100, 0
FlashForMs F1A003, 1000, 100, 0
FlashForMs F1A004, 1000, 100, 0
'FlashForMs F1A005, 1000, 100, 0
FlashForMs F1A006, 1000, 100, 0
'FlashForMs F1A007, 1000, 100, 0
'FlashForMs GiFlipLH, 1000, 100, 1
'FlashForMs GiFlipRH, 1000, 100, 1
ballsonplayfield=ballsonplayfield+1
Else
kicker054.destroyball
kicker054.createball
kicker054.kick -30, 30
end if
end sub

dim k


Sub Kicker035_hit() 'left kicker in "special" room, it randomly does one of four things with the ball once hit
fz_step
addscore 1500
playsound "saucer_enter_2"
'randomize noise

k = int(rnd*4) + 1
select case k
    
case 1 'lowers a droptarget and shoots the ball into it
If Rnd < 0.5 Then
   Start_Primitive033_SpinHalfway
Else
    Start_Primitive033_OneWay
End If
PlaySound SoundFX("hydro", DOFDropTargets)
timer009.enabled=1
 case 2 'raises a target and fires the ball
timer018.enabled=1
PlaySound SoundFX("meetdestiny", DOFDropTargets)
case 3 'lowers a droptarget and shoots the ball into it
PlayRandomLaugh
timer018.enabled=1
'PlaySound SoundFX("lower2", DOFDropTargets)
case 4 'flasher displays intermisison
'if usepup=false then
Select Case Int(Rnd * 10) + 1
    Case 1: PlaySound "start1"
    Case 2: PlaySound "start2"
    Case 3: PlaySound "start3"
    Case 4: PlaySound "not_hurt"
    Case 5: PlaySound "start5"
    Case 6: PlaySound "start6"
    Case 7: PlaySound "start7"
    Case 8: PlaySound "getout"
    Case 9: PlaySound "readyou"
    Case 10: PlaySound "chewy"
End Select

'Select Case Int(Rnd * 4)
'    Case 0: PlaySound "readyou"
'    Case 1: PlaySound "not_hurt"
'    Case 2: PlaySound "prove_it"
'    Case 3 PlaySound "spermbank"
'End Select
'end If

if usePUP=true Then
pupevent 829'POTENTIAL_INSERTPUP - "intermission" (this is a 5 seoond pause held by kicker I am using for Jack quotes in the nonPUP version)
end if
if ballsonplayfield=1 Then
end if
addscore 4000
if ultramode=1 then
'DMD_DisplaySceneTextWithPause "", "+4000", 3000
qtimer.interval=1500
end if

PlaySound SoundFX("wormhole", DOFDropTargets)
Timer020.enabled=1
end select
end sub

Sub timer009_timer()
Timer012.enabled=1
timer009.enabled=0
end sub

Sub timer012_timer()
PlaySound SoundFXDOF("fx_kicker", 135, DOFPulse, DOFContactors)
kicker035.kick 141, 30
timer012.enabled=0
end Sub

Sub timer018_timer()
Timer019.enabled=1
timer018.enabled=0
end sub

Sub Timer019_timer()
PlaySound SoundFXDOF("fx_kicker", 137, DOFPulse, DOFContactors)
kicker035.kick 141, 30
Timer019.enabled=0
end Sub

Sub kicker062_timer()
kicker062.timerenabled=0
end Sub


Sub Timer020_timer()
LightSeq010.UpdateInterval = 5
LightSeq010.Play SeqWiperRightOn,20,1
PlaySound SoundFXDOF("fx_kicker", 138, DOFPulse, DOFContactors)
kicker035.kick 141, 30
Timer020.enabled=0
end sub



sub trigger001_hit
addscore 750
'PlaySound "whir"
if usepup=false then
If Int(Rnd * 3) = 0 Then PlaySound "whir"
end if
if usepup=true then
'If Int(Rnd * 6) = 0 Then PlaySound "whir"
end if
LightSeq1.Play SeqBlinking, , 15, 20
end sub

sub trigger002_hit
if ballsonplayfield=1 Then
addscore 500
PlaySound SoundFX("died", DOFDropTargets)
Else
end If
end sub



sub kicker030_timer() 'when a multiball is locked, or a ball becomes stationary in the skull, this kicker creates and launches that ball's replacement
kicker030.createball
kicker030.kick 80, 15
If Rnd < 0.5 Then
    PlaySound "fx_kicker"
Else
    PlaySound "fx_kicker"
End If
DOF 131, DOFPulse
'playsound "fx_kicker"
kicker030.timerenabled=0
'Light004.state=1
'timer017.enabled= true
end sub


'sub timer017_timer()
'Light004.state=0
'timer017.enabled= False
'end Sub

sub trigger003_hit 'located at the end of the wire ramp, this trigger animates the explosion and the primitive disappears as if it was blown up
stopsound "wireloop1"
stopsound "wireloop2"
stopsound "wireloop3"
stopsound "wireloop4"
stopsound "wireloop5"
stopsound "wireloop6"
stopsound "wireloop7"
stopsound "wireloop8"
'playsound "wireramp_stop1"
addscore 3000
end Sub


sub Timer025_timer
'primitive013.visible = False
Primitive015.visible = False
Timer025.Enabled = 0
end Sub


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
            DMD_DisplaySceneTextWithPause "", "Keep Shooting", 3000
pupevent 963
            qtimer.Interval = 1500
        End If

        If usePUP = True Then
            pupevent 835
        End If
        'PlaySound "justamoment"
        kicker006.DestroyBall

        BallSaveQueue = BallSaveQueue + 1

        If Not BallSaveTimer.Enabled Then
            BallSaveTimer.Enabled = True
        End If
    End If
End Sub

Sub BallSaveTimer_Timer
    If BallSaveQueue > 0 Then
'disablepeg
        Kicker016.CreateBall
        
        Select Case Int(Rnd * 3) + 1
            Case 1: Kicker016.Kick 0, 50
            Case 2: Kicker016.Kick 0, 52
            Case 3: Kicker016.Kick 0, 55
        End Select

If Rnd < 0.5 Then
    PlaySound "fx_kicker"
Else
    PlaySound "fx_kicker"
End If
DOF 130, DOFPulse
        BallSaveQueue = BallSaveQueue - 1
    End If

    ' Disable if queue is empty
    If BallSaveQueue <= 0 Then
        BallSaveTimer.Enabled = False
    End If
End Sub



sub Kicker009_hit 'if hal beaten once, you get one outlanes (left Side)
if usePUP=true Then
pupevent 836'POTENTIAL_INSERTPUP - Kickback kicker ejects ball back onto playfield, saving the ball from draining (infrequent)
end if
addscore 2000
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "BALL SAVED", 3000
qtimer.interval=1500
end if
PlaySound SoundFXDOF("kickback2", 136, DOFPulse, DOFContactors)
'PlaySound "kickback2"
LightSeq010.UpdateInterval = 10
LightSeq010.Play SeqUpOn,5,1
if kickback=1 Then
pin3.collidable=0
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
pin3.collidable=1
ramp015.collidable=0
ramp016.collidable=0
Ramp017.collidable=0
Ramp019.collidable=0
light095.state=0
Light096.state=0
Timer046.enabled=0
end Sub

sub Kicker010_hit 'if hal beaten once, you get one outlanes (right Side)
if usePUP=true Then
pupevent 836'POTENTIAL_INSERTPUP - Kickback kicker ejects ball back onto playfield, saving the ball from draining (infrequent)
end if
addscore 2000
if ultramode=1 then
DMD_DisplaySceneTextWithPause "", "BALL SAVED", 3000
qtimer.interval=1500
end if
PlaySound SoundFXDOF("kickback2", 139, DOFPulse, DOFContactors)
'playsound "kickback2"
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
pin3.collidable=1
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
Light108.state=2
else 
if usePUP=true Then
pupevent 837'POTENTIAL_INSERTPUP - Multiball ends, stop any multiball video if playing
end if
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
folderPath = "\starwrs.UltraDMD"

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


Sub DMD_DisplaySceneText(toptext, bottomtext)
	DMD_DisplayScene "", toptext, 15, bottomtext, 15, UltraDMD_Animation_None, 10000, UltraDMD_Animation_None
End Sub

Sub DMD_DisplaySceneTextWithPause(toptext, bottomtext, pauseTime)
	DMD_DisplayScene "", toptext, 15, bottomtext, 15, UltraDMD_Animation_None, pauseTime, UltraDMD_Animation_None
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

dim UltraMode
dim sortnamed
qtimer.enabled=0

if ultramode=1 then
LoadUltraDMD
timer080.enabled=1
Else
end If

if ultramode=1 then
'txtHigh1.text = HighScore1
'DMD_DisplaySceneTextWithPause "HAL 9000", "FREE PLAY", 10000
'HighScore1 = GetValue(hsFile, "HighScore1", 0)
hightext = "HIGH SCORE " & Highscore1 'txtHigh1.text
haltext = "AI SCORE " & HALScore '
'DMD_DisplaySceneTextWithPause "Star Wars", hightext, 10000
scoretimer.enabled=1
end if

sub delayscoretimer_timer
scoretimer.enabled=1
delayscoretimer.enabled=0
end Sub

Sub scoretimer_Timer()
If GameOn = 1 Then Exit Sub
    Select Case messageIndex
        Case 0
            DMD_DisplaySceneTextWithPause "PRESS RIGHT MAGNASAVE", "FOR AI DEMO", 10000
        Case 1            
            DMD_DisplaySceneTextWithPause "Star Wars", HALtext, 10000
        Case 2            
            DMD_DisplaySceneTextWithPause "Star Wars", hightext, 10000
        Case 3
            DMD_DisplaySceneTextWithPause "Press LeFT MAGNASAVE", "to shoot lasers", 10000
    End Select

    messageIndex = (messageIndex + 1) Mod 4  ' cycle 0,1,2,0,1,2...
End Sub




Sub QTimer_Timer
    ' If a bonus cycle is running, advance it and EXIT.
    If UMD_BonusActive Then
        UMD_ShowNextBonus          ' This will set QTimer.Interval = BONUS_GAP_MS
        Exit Sub                   ' <-- Do NOT disable QTimer here
    End If

    ' Normal DMD heartbeat (only when UltraMode=1)
    If UltraMode = 1 Then
        DMD_DisplaySceneTextWithPause ultraball, p1score, 10000
        QTimer.Interval = 333
    End If
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

Const TableName = "starwrs"

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




Sub ResetChangeLight
	ChangeGi blue
	ChangeBumperLight amber
End Sub

'spin ship
Dim spinAngle
spinAngle = 90




sub timer001_timer 'keeps shoot again light on if shoot again timer expires after extra ball received
Light014.state=2
end sub


sub ballrelease_timer
Flasher005.visible=0
BallRelease.createball
BallRelease.kick 45,8
DOF 123, DOFPulse
 PlayBallReleaseSound
pupevent 840
ballrelease.timerenabled=0
end Sub

sub ballrelease2_timer
Flasher005.visible=0
BallRelease.createball
BallRelease.kick 45,8
DOF 123, DOFPulse
 PlayBallReleaseSound
pupevent 840
ballrelease2.enabled=0
end Sub

sub fastrelease_timer
Flasher005.visible=0
BallRelease.createball
'pupevent 845
BallRelease.kick 45,8
DOF 123, DOFPulse
 PlayBallReleaseSound
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

'***************animate hal lights quickly down during monolith battle
Dim LightSeqRunning

Sub StartLightSeq()
    If Not LightSeqRunning Then
        LightSeqRunning = True
'   LightSeq009.UpdateInterval = 7
'LightSeq009.Play SeqDownOn,5,200


'LightSeq009.Play SeqUpOn,5,1
'LightSeq009.Play SeqClockRightOn,360,200
        LightSeqTimer.Enabled = True
    End If
End Sub

Sub StopLightSeq()
    LightSeqRunning = False
    LightSeq009.StopPlay

'Light101.state=0
Light134.state=0
Light129.state=0
    LightSeqTimer.Enabled = False
End Sub

Sub LightSeqTimer_Timer()
    If LightSeqRunning Then

    Else
        LightSeqTimer.Enabled = False
    End If
End Sub
'***************animate hal lights quickly down during monolith battle



Dim JumpBall053

Sub Kicker053_Hit()
Startcircle
LightSeq009.Stopplay
LightSeq009.UpdateInterval = 25
	LightSeq009.Play SeqRandom,10,,2000
    Set JumpBall053 = ActiveBall
    AddScore 2000
    PlaySound "enter_jump"
'playsound "impact1"
'FlashForMs F1A005, 1000, 100, 0
FlashForMs F1A006, 1000, 100, 0
'FlashForMs F1A007, 1000, 100, 0
'FlashForMs GiFlipLH, 1000, 100, 1
'FlashForMs GiFlipRH, 1000, 100, 1
ChangeGi base
ChangeBumperLight base
'Ramp027.Collidable = 1
    BallJumpTimer053.Enabled = True
kicker053.enabled=0
End Sub

Sub BallJumpTimer053_Timer()
    If Not JumpBall053 Is Nothing Then
PlaySound SoundFXDOF("fx_kicker_loud", 114, DOFPulse, DOFContactors)
        Kicker053.Kick 0, 0.1  ' Release from kicker
        JumpBall053.VelZ = 65
        JumpBall053.VelX = 0
        JumpBall053.VelY = 0
    End If
    BallJumpTimer053.Enabled = False
    Set JumpBall053 = Nothing
End Sub


Sub gate007_hit
Trigger004.enabled=0
timer070.enabled=1
end Sub

sub Timer016_timer 'locled ball ball launch
Plunger.AutoPlunger = True
playsound "fx_kicker"
plunger.fire
Plunger.AutoPlunger = False
Kicker016.createball
Kicker016.kick 0, 53
'playsound "fx_kicker"
'Light004.state=1
'timer017.enabled= true
Timer016.Enabled = 0
end Sub


'****Make HAL appear and disappear****
Dim HalVisible, HalCounter


'****END Make HAL appear and disappear****


'****END remove all balls from HAL


Dim CanStartGame
CanStartGame = True

Sub GameDelayTimer_Timer()
    'Flasher005.visible=0
    CanStartGame = True
    RightSlingBlocker.collidable = False
    kicker039Active = False
    kicker040Active = False
    kicker041Active = False
    GameDelayTimer.Enabled = False
End Sub


' Wobble globals
' Violent wobble globals


sub Timer049_timer
Flasher005.visible=0
Timer049.enabled=false
end Sub

' === Fade In ===
Dim FadeStep
FadeStep = 0

Sub StartFadeIn()
Primitive084.BlendDisableLighting = 2
    'LeftInlane.visible=0
    'LeftInlane002.visible=0
    'RightInlane003.visible=0
    'RightInlane001.visible=0
    Flasher014.Visible = True
    Flasher014.Opacity = 0
    FadeStep = 0
    FadeInTimer.Enabled = True
End Sub

Sub FadeInTimer_Timer()
    If FadeStep < 40 Then
        FadeStep = FadeStep + 1
        Flasher014.Opacity = FadeStep * (70 / 20)
    Else
        Flasher014.Opacity = 100
        FadeInTimer.Enabled = False
    End If
End Sub

' === Fade Out ===
Dim FadeOutStep
FadeOutStep = 0

Sub StartFadeOut()
Primitive084.BlendDisableLighting = 0.4
    FadeOutStep = 0
    FadeOutTimer.Enabled = True
End Sub

Sub FadeOutTimer_Timer()
    If FadeOutStep < 20 Then
        FadeOutStep = FadeOutStep + 1
        Flasher014.Opacity = 100 - (FadeOutStep * (70 / 20))
    Else
        Flasher014.Opacity = 0
        FadeOutTimer.Enabled = False
        'Flasher001.Visible = False ' optional: hide when fully transparent
        
    End If
End Sub


sub timer070_timer
startballcontrol.enabled=1
timer070.enabled=0
end sub


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


dim flashstep
flashstep=0

sub quickflash
Flasher005.visible=1
Timer058.enabled=1
end Sub


sub timer058_timer
if flashstep=3 Then
'Light004.state=0
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
Primitive005.BlendDisableLighting = 2 'jabbas eyes light up
'If Activeball.VelY < -5 Then Exit Sub
        Trigger020.Enabled = True
        Trigger023.Enabled = True
if sabermode=true then
    If Rnd < 0.5 Then
        PlayRandomSaber
        Else
    End If
SaberRotateCW
end if
        FlipperActivate LeftFlipper, LFPress
		LF.fire 'LeftFlipper.RotateToEnd
        FlipperActivate LeftFlipper, LFPress
' Flipper Sound Trigger 1
PlayLeftFlipSound
timer065.enabled=1
end Sub

sub timer065_timer
LeftFlipper.RotateToStart
FlipperDeActivate LeftFlipper, LFPress
'LeftFlipper002.RotateToStart
if ballsonplayfield > 1 Then
Timer065.interval=35
end if
if ballsonplayfield = 1 Then
Timer065.interval=100
end if
PlaySound SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
timer065.enabled=0
end Sub

sub trigger023_hit	
Primitive005.BlendDisableLighting = 2 'jabbas eyes light up
trigger018.enabled=1
Trigger019.enabled=1
Trigger023.enabled=0
if sabermode=true then
    If Rnd < 0.5 Then
        PlayRandomSaber
        Else
    End If
SaberRotateCW
end if	
		LF.fire 'LeftFlipper.RotateToEnd
        FlipperActivate LeftFlipper, LFPress
' Flipper Sound Trigger 2
PlayLeftFlipSound

timer065.enabled=1
end Sub

sub trigger020_hit		
Primitive005.BlendDisableLighting = 2 'jabbas eyes light up
trigger018.enabled=1
Trigger019.enabled=1
trigger020.enabled=0
if sabermode=true then
    If Rnd < 0.5 Then
        PlayRandomSaber
        Else
    End If
SaberRotateCW
end if
		LF.fire 'LeftFlipper.RotateToEnd
        FlipperActivate LeftFlipper, LFPress
' Flipper Sound Trigger 3
PlayLeftFlipSound
timer065.enabled=1
end Sub


sub trigger019_hit	
Primitive005.BlendDisableLighting = 0 'jabbas eyes light up
if sabermode=true then
    If Rnd < 0.5 Then
        PlayRandomSaber
        Else
    End If
SaberRotateCCW
end if
'If Activeball.VelY < -5 Then Exit Sub
        Trigger022.Enabled = True
        Trigger021.Enabled = True
		RF.fire 'RightFlipper.RotateToEnd
        FlipperActivate RightFlipper, RFPress
' Flipper Sound Trigger 5
PlayRightFlipSound

Timer066.enabled=1
end Sub

sub trigger022_hit	
Primitive005.BlendDisableLighting = 0 'jabbas eyes light up
trigger018.Enabled=1
Trigger019.Enabled=1	
trigger022.enabled=0
if sabermode=true then
    If Rnd < 0.5 Then
        PlayRandomSaber
        Else
    End If
SaberRotateCCW
end if
		RF.fire 'RightFlipper.RotateToEnd
        FlipperActivate RightFlipper, RFPress
' Flipper Sound Trigger 6
PlayRightFlipSound

Timer066.enabled=1
end Sub

sub trigger021_hit	
Primitive005.BlendDisableLighting = 0 'jabbas eyes light up
trigger018.enabled=1
Trigger019.enabled=1	
trigger021.enabled=0
if sabermode=true then
    If Rnd < 0.5 Then
        PlayRandomSaber
        Else
    End If
SaberRotateCCW
end if
		RF.fire 'RightFlipper.RotateToEnd
        FlipperActivate RightFlipper, RFPress
' Flipper Sound Trigger 7
PlayRightFlipSound

Timer066.enabled=1
end Sub

sub Timer066_timer
RightFlipper.RotateToStart
FlipperDeActivate RightFlipper, RFPress
'RightFlipper001.RotateToStart
if ballsonplayfield > 1 Then
timer066.interval=35
end if
if ballsonplayfield = 1 Then
timer066.interval=100
end if
PlaySound SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), 0, 1, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
Timer066.enabled=0
end Sub

sub Kicker025_Hit 'hal kicker for autoplay
Plunger.AutoPlunger = True
kicker025.timerenabled=1
end sub

sub kicker025_timer 'hal kicker for autoplay
kicker025.kick 0, 50
'playsound "fx_kicker"
playsound "Plunger"
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
Rank_ResetAnnounce
FlipLights_blink
ResetRanks
For Each l_guide In guidelights
    l_guide.State = 2
Next
'Target007.isdropped=0
light035.state=2
light033.state=2
delayfadeout.enabled=1 'hide star wars beam
LiPass005.state=0 'reset multiball blocker and lights
LiPass009.state=0
LiPass010.state=0
LiPass003.state=0 'reset multiball blocker  and lights
LiPass004.state=0 'reset multiball blocker and lights
primitive023.Visible = 0 'reset multiball blocker and lights
Ramp049.Collidable = 0 'reset multiball blocker  and lights

P008_StartFlicker
Init_Primitive036_Scheduler
InitTieFlyCircleBack
Init_Trigger045_Progress
Init_Trigger036_Progress
if usepup=true then
introtimer.enabled=0
end if
addanotherball=2
impatient.enabled=1
timer083.enabled=0
Timer082.enabled=0
stopsound "song_psycho"
Stopsound "end1"
Stopsound "end2"
Stopsound "end3"
Stopsound "end4"
Stopsound "end5"
Stopsound "end6"
        showtwins=0
		scoretimer.enabled=0
        dmdballnum = 1
        Controller.B2SSetScore 2, dmdballnum
              flasher005.visible = 0
           Kicker039.enabled = 1
        target043.isdropped = 0
        Trigger004.enabled = 1
            BallSaveQueue = 0
        If usePUP = True Then
        pupevent 961' POTENTIAL_INSERTPUP - Game started
        End If
        firstball = 1
        StopSound "main_music"
         If firstgame = 1 Then
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
        dstar1.state=2
        dstar001.state=2
        dstar002.state=2
        dstar003.state=2
        tlight.state=0
        tlight001.state=0
        tlight002.state=0
        tlight003.state=0
        extraball = 0
        starlight=0
        tiehitlight=0
        mynockshot=0
        multitimes = 0
        If usePUP = False Then
          if dmdballnum=1 then 
           if GameMusicOn=1 then
           playsound "scary_mp3"
           end If
           delaymusic.enabled=1
          end if
            
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
            ballrelease2.Enabled = True
        Else
            fastrelease.Enabled = True
        End If
    End If
End Sub
'****END startgame. this sub was added so that HAL can start a game without an actual physical press of a button, he can call "startgame"

'********BEGIN when a ball hits HAL, each of the four kickers do this, differnet depending on how many balls are already in HAL

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
Stopsound "song_psycho"
StopSound "spaceflight"
Stopsound "littlepigsaxe"
StopSound "main_music"
    Select Case songIndex
        Case 1: PlaySound "song1",-1
        Case 2: PlaySound "song2",-1
        Case 3: PlaySound "song3",-1
        Case 4: PlaySound "song4",-1
        Case 5: PlaySound "song5",-1
        Case 6: PlaySound "song6",-1
        Case 7: PlaySound "song7",-1
        Case 8: PlaySound "song7",-1
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
Stopsound "song_psycho"
StopSound "spaceflight"
Stopsound "littlepigsaxe"
StopSound "main_music"
end if
end sub






sub timer033_timer
if showtwins=0 Then
playsound "forever"
If Rnd < 0.5 Then
   Start_Primitive033_SpinHalfway
Else
    Start_Primitive033_OneWay
End If
end if
showtwins=1
timer033.enabled=0
end sub



'****flicker bottom right target bank Light
Dim FlickerCounter

Sub FlickerTimer2_Timer()
    FlickerCounter = FlickerCounter + 1
    If FlickerCounter Mod 15 = 0 Then
       ' gi008.State = gi008.State Xor 1 ' toggle on/off
    End If
End Sub




sub ballcount_timer
'if dmdballnum < 5 then
if ballsonplayfield=0 then
FlasherZero.visible=1
FlasherZero001.visible=0
FlasherZero002.visible=0
FlasherZero003.visible=0
FlasherZero004.visible=0
FlasherZero005.visible=0
FlasherZero006.visible=0
FlasherZero007.visible=0
FlasherZero008.visible=0
FlasherZero009.visible=0
FlasherZero010.visible=0
FlasherZero011.visible=0
FlasherZero012.visible=0
end if
if ballsonplayfield=1 then
FlasherZero.visible=0
FlasherZero001.visible=1
FlasherZero002.visible=0
FlasherZero003.visible=0
FlasherZero004.visible=0
FlasherZero005.visible=0
FlasherZero006.visible=0
FlasherZero007.visible=0
FlasherZero008.visible=0
FlasherZero009.visible=0
FlasherZero010.visible=0
FlasherZero011.visible=0
FlasherZero012.visible=0
end if
if ballsonplayfield=2 then
FlasherZero.visible=0
FlasherZero001.visible=0
FlasherZero002.visible=1
FlasherZero003.visible=0
FlasherZero004.visible=0
FlasherZero005.visible=0
FlasherZero006.visible=0
FlasherZero007.visible=0
FlasherZero008.visible=0
FlasherZero009.visible=0
FlasherZero010.visible=0
FlasherZero011.visible=0
FlasherZero012.visible=0
end if
if ballsonplayfield=3 then
FlasherZero.visible=0
FlasherZero001.visible=0
FlasherZero002.visible=0
FlasherZero003.visible=1
FlasherZero004.visible=0
FlasherZero005.visible=0
FlasherZero006.visible=0
FlasherZero007.visible=0
FlasherZero008.visible=0
FlasherZero009.visible=0
FlasherZero010.visible=0
FlasherZero011.visible=0
FlasherZero012.visible=0
end if
if ballsonplayfield=4 then
FlasherZero.visible=0
FlasherZero001.visible=0
FlasherZero002.visible=0
FlasherZero003.visible=0
FlasherZero004.visible=1
FlasherZero005.visible=0
FlasherZero006.visible=0
FlasherZero007.visible=0
FlasherZero008.visible=0
FlasherZero009.visible=0
FlasherZero010.visible=0
FlasherZero011.visible=0
FlasherZero012.visible=0
end if
if ballsonplayfield=5 then
FlasherZero.visible=0
FlasherZero001.visible=0
FlasherZero002.visible=0
FlasherZero003.visible=0
FlasherZero004.visible=0
FlasherZero005.visible=1
FlasherZero006.visible=0
FlasherZero007.visible=0
FlasherZero008.visible=0
FlasherZero009.visible=0
FlasherZero010.visible=0
FlasherZero011.visible=0
FlasherZero012.visible=0
end if
if ballsonplayfield=6 then
FlasherZero.visible=0
FlasherZero001.visible=0
FlasherZero002.visible=0
FlasherZero003.visible=0
FlasherZero004.visible=0
FlasherZero005.visible=0
FlasherZero006.visible=1
FlasherZero007.visible=0
FlasherZero008.visible=0
FlasherZero009.visible=0
FlasherZero010.visible=0
FlasherZero011.visible=0
FlasherZero012.visible=0
end if
if ballsonplayfield=7 then
FlasherZero.visible=0
FlasherZero001.visible=0
FlasherZero002.visible=0
FlasherZero003.visible=0
FlasherZero004.visible=0
FlasherZero005.visible=0
FlasherZero006.visible=0
FlasherZero007.visible=1
FlasherZero008.visible=0
FlasherZero009.visible=0
FlasherZero010.visible=0
FlasherZero011.visible=0
FlasherZero012.visible=0
end if
if ballsonplayfield=8 then
FlasherZero.visible=0
FlasherZero001.visible=0
FlasherZero002.visible=0
FlasherZero003.visible=0
FlasherZero004.visible=0
FlasherZero005.visible=0
FlasherZero006.visible=0
FlasherZero007.visible=0
FlasherZero008.visible=1
FlasherZero009.visible=0
FlasherZero010.visible=0
FlasherZero011.visible=0
FlasherZero012.visible=0
end if
if ballsonplayfield=9 then
FlasherZero.visible=0
FlasherZero001.visible=0
FlasherZero002.visible=0
FlasherZero003.visible=0
FlasherZero004.visible=0
FlasherZero005.visible=0
FlasherZero006.visible=0
FlasherZero007.visible=0
FlasherZero008.visible=0
FlasherZero009.visible=1
FlasherZero010.visible=0
FlasherZero011.visible=0
FlasherZero012.visible=0
end if
if ballsonplayfield=10 then
FlasherZero.visible=0
FlasherZero001.visible=0
FlasherZero002.visible=0
FlasherZero003.visible=0
FlasherZero004.visible=0
FlasherZero005.visible=0
FlasherZero006.visible=0
FlasherZero007.visible=0
FlasherZero008.visible=0
FlasherZero009.visible=0
FlasherZero010.visible=1
FlasherZero011.visible=0
FlasherZero012.visible=0
end if
if ballsonplayfield=11 then
FlasherZero.visible=0
FlasherZero001.visible=0
FlasherZero002.visible=0
FlasherZero003.visible=0
FlasherZero004.visible=0
FlasherZero005.visible=0
FlasherZero006.visible=0
FlasherZero007.visible=0
FlasherZero008.visible=0
FlasherZero009.visible=0
FlasherZero010.visible=0
FlasherZero011.visible=1
FlasherZero012.visible=0
end if
if ballsonplayfield>11 then
FlasherZero.visible=0
FlasherZero001.visible=0
FlasherZero002.visible=0
FlasherZero003.visible=0
FlasherZero004.visible=0
FlasherZero005.visible=0
FlasherZero006.visible=0
FlasherZero007.visible=0
FlasherZero008.visible=0
FlasherZero009.visible=0
FlasherZero010.visible=0
FlasherZero011.visible=0
FlasherZero012.visible=1
end if
'end if
end sub




Dim fadeOpacity028

sub impatient_timer
if usepup=false Then
playsound "impatient"
end If
if usePUP = true Then
pupevent 937
'POTENTIAL_INSERTPUP - Player hasn't shot ball#1 after 30 seconds
end if
impatient.enabled=0
end sub

Sub SoundCooldownTimer_Timer()
    SoundReady = True
    SoundCooldownTimer.Enabled = False
End Sub

sub Kicker013_hit
kicker013.kick 0, 30
end Sub



'***fade in and out Flasher029
' === Fade In for Flasher029 ===
'***fade in and out Flasher029 (max opacity 70)
'***fade in and out Flasher029 (max opacity 85)
Dim FadeStep029
Dim FadingOut029
FadeStep029 = 0
FadingOut029 = False

Sub FadeTimer029_Timer()
    If Not FadingOut029 Then
        ' Fading in
        FadeStep029 = FadeStep029 + 1
        Flasher029.Opacity = FadeStep029 * (85 / 40)

        If FadeStep029 >= 40 Then
            FadingOut029 = True
            FadeStep029 = 0
        End If
    Else
        ' Fading out
        FadeStep029 = FadeStep029 + 1
        Flasher029.Opacity = 85 - (FadeStep029 * (85 / 40))

        If FadeStep029 >= 40 Then
            Flasher029.Opacity = 0
            Flasher029.Visible = False
            FadeTimer029.Enabled = False
        End If
    End If
End Sub

Sub StartFade029()
    Flasher029.Visible = True
    Flasher029.Opacity = 0
    FadeStep029 = 0
    FadingOut029 = False
    FadeTimer029.Enabled = True
End Sub



sub introtimer_timer
pupevent 910
end Sub

sub timer080_timer
DMD_DisplaySceneTextWithPause "", "STAR WARS", 10000
scoretimer.enabled=1
timer080.enabled=0
end Sub

Dim flashCount

Sub FlashAllLights()
LightSeq010.Play SeqAllOn
    flashCount = 0
    FlashTimer.Interval = 137 ' 200 ms = 5 flashes in 2 sec
    FlashTimer.Enabled = True
End Sub

Sub FlashTimer_Timer()
    ' Alternate all-off / all-on
    If flashCount Mod 2 = 0 Then
        LightSeq010.Play SeqAllOff
    Else
        LightSeq010.Play SeqAllOn
    End If

    flashCount = flashCount + 1

    ' End after 8 steps (4 full on/off cycles)
    If flashCount >= 8 Then
        LightSeq010.StopPlay   ' release control back to normal lamp logic
        FlashTimer.Enabled = False
    End If
End Sub

sub Startcircle
spinl1.state=2
spinl2.state=2
spinl3.state=2
spinl4.state=2
spinl5.state=2
spinl6.state=2
spinl7.state=2
spinl8.state=2
timer081.enabled=1
end Sub


sub timer081_timer
spinl1.state=0
spinl2.state=0
spinl3.state=0
spinl4.state=0
spinl5.state=0
spinl6.state=0
spinl7.state=0
spinl8.state=0
timer081.enabled=0
end Sub



sub timer082_timer
stopsound "main_music"
playsound "song_psycho"
timer083.enabled=1
Timer082.enabled=0
end Sub

sub timer083_timer
PlaySound "main_music", -1
timer083.enabled=0
end Sub

sub timer084_timer
'Light056.state=0
timer084.enabled=0
end sub


sub leftslingshot003_slingshot
ToggleBlastB2S
PlaySlingLeftSound
end Sub

sub RightSlingShot005_slingshot
gi020.state=1
timer010.enabled=1
'rubber002.visible=1
timer087.enabled=1
PlaySlingRight2Sound
end Sub

sub timer086_timer
Rubber001.visible=0
timer086.enabled=0
end Sub

sub Timer087_timer
'rubber002.visible=0
timer087.enabled=0
end Sub



Dim JumpBall017

Sub Kicker017_Hit()
LiPass005.state=0 'turn off flashing multiball Light
LiPass009.state=0
LiPass010.state=0
        kickback = 1
        Light095.State = 2
        Light096.State = 2
        Kicker009.Enabled = 1
        Kicker010.Enabled = 1
Startcircle
LightSeq009.Stopplay
LightSeq009.UpdateInterval = 25
	LightSeq009.Play SeqRandom,10,,2000
    Set JumpBall017 = ActiveBall
    AddScore 2000
    PlaySound "Saucer_Enter_1"
playsound "impact1"
'FlashForMs F1A005, 1000, 100, 0
FlashForMs F1A006, 1000, 100, 0
'FlashForMs F1A007, 1000, 100, 0
'FlashForMs GiFlipLH, 1000, 100, 1
'FlashForMs GiFlipRH, 1000, 100, 1
ChangeGi base
ChangeBumperLight base
'Ramp027.Collidable = 1
    BallJumpTimer017.Enabled = True
kicker017.enabled=0
End Sub

Sub BallJumpTimer017_Timer()
    If Not JumpBall017 Is Nothing Then
PlaySound SoundFXDOF("fx_kicker", 116, DOFPulse, DOFContactors)
        Kicker017.Kick 0, 0.1  ' Release from kicker
        JumpBall017.VelZ = 35
        JumpBall017.VelX = 0
        JumpBall017.VelY = 0
    End If
    BallJumpTimer017.Enabled = False
    Set JumpBall017 = Nothing
End Sub



Dim JumpBall020

Sub Kicker020_Hit()
Startcircle
LightSeq009.Stopplay
LightSeq009.UpdateInterval = 25
	LightSeq009.Play SeqRandom,10,,2000
    Set JumpBall020 = ActiveBall
    AddScore 2000
    PlaySound "enter_jump"
'playsound "impact1"
'FlashForMs F1A005, 1000, 100, 0
FlashForMs F1A006, 1000, 100, 0
'FlashForMs F1A007, 1000, 100, 0
'FlashForMs GiFlipLH, 1000, 100, 1
'FlashForMs GiFlipRH, 1000, 100, 1
ChangeGi base
ChangeBumperLight base
'Ramp027.Collidable = 1
    BallJumpTimer020.Enabled = True
Kicker020.enabled=0
End Sub

Sub BallJumpTimer020_Timer()
    If Not JumpBall020 Is Nothing Then
PlaySound SoundFXDOF("fx_kicker_loud", 117, DOFPulse, DOFContactors)
        Kicker020.Kick 0, 0.1  ' Release from kicker
        JumpBall020.VelZ = 65
        JumpBall020.VelX = 0
        JumpBall020.VelY = 0
    End If
    BallJumpTimer020.Enabled = False
    Set JumpBall020 = Nothing
End Sub


sub trigger017_hit
'Light056.state=1
timer084.enabled=1
kicker020.enabled=1
end Sub

sub trigger028_hit
If BallsOnPlayfield = 1 Then
light035.state=2
light033.state=2
    addanotherball = addanotherball + 1
    If addanotherball >= 3 Then
light035.state=1
light033.state=1
        addanotherball = 0  ' reset the counter (every 3rd time)

        ' actually spawn the 2nd ball
        PlayBallReleaseSound
        ballsonplayfield=ballsonplayfield+1
        Timer016.Enabled  = True
    End If
End If

If Rnd < 0.5 Then
ramp021.collidable=1
    If OneIn(5) Then
        PlaySound "yeeha"
    End If
 Else
ramp021.collidable=0
playsound "tractor"
End If



ST11_StartSpin 360, 900, True
kicker053.enabled=1
end Sub



sub disablepeg
zCol_Rubber_Post012.collidable=0
Timer085.enabled=1
end Sub

sub Timer085_timer
zCol_Rubber_Post012.collidable=1
Timer085.enabled=0 'check this
end Sub

sub Trigger030_hit
kicker017.enabled=1
end Sub


sub Trigger031
trigger021.enabled=0
trigger022.enabled=1
end Sub


sub trigger029_hit
ST71_StartSpin 720, 540, True
VaderStartWobble
If Rnd < 0.5 Then
    PlaySound "ouch1"
Else
    PlaySound "vaderhurt1"
End If
If BallsOnPlayfield = 1 Then
light035.state=2
light033.state=2
    addanotherball = addanotherball + 1
    If addanotherball >= 3 Then
light035.state=1
light033.state=1
        addanotherball = 0  ' reset the counter (every 3rd time)

        ' actually spawn the 2nd ball
        PlayBallReleaseSound
        ballsonplayfield=ballsonplayfield+1
        Timer016.Enabled  = True
    End If
End If
'Light056.state=1
'timer084.enabled=1
'kicker021.kick -60, 5
end Sub






'sub kicker049_hit
'kicker049.kick -135, 15
'end Sub


sub kicker026_hit
fz_step
'Light056.state=1
timer084.enabled=1
kicker026.kick -5, 30
PlayWireLoopSound
end Sub





sub Trigger032_hit
UMD_OnRampHit 'bonus for ramp hits
ST11_StartSpin 360, 600, False
'Light056.state=1
timer084.enabled=1
            PlayWireLoopSound
end Sub
sub Trigger034_hit
fz_step
            PlayWireLoopSound
end Sub
sub Trigger033_hit '
          stopsound "wireloop1"
     stopsound "wireloop2"
     stopsound "wireloop3"
     stopsound "wireloop4"
     stopsound "wireloop5"
     stopsound "wireloop6"
     stopsound "wireloop7"
     stopsound "wireloop8"
end Sub

sub Trigger035_hit '
'Light056.state=1
timer084.enabled=1
          stopsound "wireloop1"
     stopsound "wireloop2"
     stopsound "wireloop3"
     stopsound "wireloop4"
     stopsound "wireloop5"
     stopsound "wireloop6"
     stopsound "wireloop7"
     stopsound "wireloop8"
end Sub

'--- Add a timer in the editor ---
' Name it: Tie083WobbleTimer
' Interval: 25
' Enabled: False

'--- Add a timer in the editor ---
' Name it: Tie083WobbleTimer
' Interval: 63    ' ~0.75s total
' Enabled: False

'--- Add a timer in the editor ---
' Name it: Tie083WobbleTimer
' Interval: 25
' Enabled: False

Dim tieStepCount, tieTotalMoves

Sub Tie083StartWobble()
    tieStepCount = 0
    tieTotalMoves = 0
    Tie083WobbleTimer.Enabled = True
End Sub

Sub Tie083WobbleTimer_Timer()
    Dim tieAngle
    tieStepCount = tieStepCount + 1
    
    ' Simple back/forth pattern
    Select Case (tieStepCount Mod 4)
        Case 0: tieAngle = 0
        Case 1: tieAngle = 10
        Case 2: tieAngle = 0
        Case 3: tieAngle = -10
    End Select
    
    Primitive083.RotZ = tieAngle
    
    If tieStepCount >= 12 Then  ' about 0.75s at 25ms interval
        Primitive083.RotZ = 0
        Tie083WobbleTimer.Enabled = False
    End If
End Sub

Dim tie001StepCount, tie001TotalMoves

Sub Tie001StartWobble()
    tie001StepCount = 0
    tie001TotalMoves = 0
    Tie001WobbleTimer.Enabled = True
End Sub

Sub Tie001WobbleTimer_Timer()
    Dim tie001Angle
    tie001StepCount = tie001StepCount + 1
    
    ' Simple back/forth pattern
    Select Case (tie001StepCount Mod 4)
        Case 0: tie001Angle = 0
        Case 1: tie001Angle = 10
        Case 2: tie001Angle = 0
        Case 3: tie001Angle = -10
    End Select
    
    Primitive001.RotZ = tie001Angle
    
    If tie001StepCount >= 12 Then  ' about 0.75s at 25ms interval
        Primitive001.RotZ = 0
        Tie001WobbleTimer.Enabled = False
    End If
End Sub

Dim JabbaStepCount, JabbaTotalMoves

Sub Jabba083StartWobbleb()
    JabbaStepCount = 0
    JabbaTotalMoves = 0
    Jabba083WobbleTimer.Enabled = True
End Sub

Sub Jabba083WobbleTimer_Timerc()
    Dim JabbaAngle
    JabbaStepCount = JabbaStepCount + 1
    
    ' Simple back/forth pattern
    Select Case (JabbaStepCount Mod 4)
        Case 0: JabbaAngle = 0
        Case 1: JabbaAngle = 10
        Case 2: JabbaAngle = 0
        Case 3: JabbaAngle = -10
    End Select
    
    ' Apply wobble to both primitives
    Primitive014.RotZ = JabbaAngle
    Primitive005.RotZ = JabbaAngle
    
    ' Stop after 12 steps
    If JabbaStepCount >= 12 Then  ' about 0.75s at 25ms interval
        Primitive014.RotZ = 0
        Primitive005.RotZ = 0
        Jabba083WobbleTimer.Enabled = False
    End If
End Sub


' Optional: centralize the levels
Const P5_LIGHT_OFF = 0   ' fully ignore lighting
Const P5_LIGHT_ON  = 2   ' your desired end level

Sub Jabba083StartWobble()
    JabbaStepCount = 0
    JabbaTotalMoves = 0

    ' Kill lighting on Primitive005 during the wobble
    Primitive005.BlendDisableLighting = P5_LIGHT_OFF

    Jabba083WobbleTimer.Enabled = True
End Sub

Sub Jabba083WobbleTimer_Timer()
    Dim JabbaAngle
    JabbaStepCount = JabbaStepCount + 1

    Select Case (JabbaStepCount Mod 4)
        Case 0: JabbaAngle = 0
        Case 1: JabbaAngle = 10
        Case 2: JabbaAngle = 0
        Case 3: JabbaAngle = -10
    End Select

    Primitive014.RotZ = JabbaAngle

    If JabbaStepCount >= 12 Then  ' ≈0.75s at 25ms interval
        Primitive014.RotZ = 0

        ' Restore lighting on Primitive005 to level 2
        Primitive005.BlendDisableLighting = P5_LIGHT_ON

        Jabba083WobbleTimer.Enabled = False
    End If
End Sub







Dim R2StepCount, R2TotalMoves

Sub R2StartWobble()
    R2StepCount = 0
    R2TotalMoves = 0
    R2WobbleTimer.Enabled = True
End Sub

Sub R2WobbleTimer_Timer()
    Dim R2Angle
    R2StepCount = R2StepCount + 1
    
    ' Simple back/forth pattern
    Select Case (R2StepCount Mod 4)
        Case 0: R2Angle = 0
        Case 1: R2Angle = 5
        Case 2: R2Angle = 0
        Case 3: R2Angle = -5
    End Select
    
    Primitive084.RotZ = R2Angle
    
    If R2StepCount >= 12 Then  ' about 0.75s at 25ms interval
        Primitive084.RotZ = 0
        R2WobbleTimer.Enabled = False
    End If
End Sub


Dim C3StepCount, C3TotalMoves

Sub C3StartWobble()
    C3StepCount = 0
    C3TotalMoves = 0
    C3WobbleTimer.Enabled = True
End Sub

Sub C3WobbleTimer_Timer()
    Dim C3Angle
    C3StepCount = C3StepCount + 1
    
    ' Simple back/forth pattern
    Select Case (C3StepCount Mod 4)
        Case 0: C3Angle = 0
        Case 1: C3Angle = 5
        Case 2: C3Angle = 0
        Case 3: C3Angle = -5
    End Select
    
    Primitive003.RotZ = C3Angle
    
    If C3StepCount >= 12 Then  ' about 0.75s at 25ms interval
        Primitive003.RotZ = 0
        C3WobbleTimer.Enabled = False
    End If
End Sub



Dim DSStepCount, DSTotalMoves

Sub DSStartWobble()
    DSStepCount = 0
    DSTotalMoves = 0
    DSWobbleTimer.Enabled = True
End Sub

Sub DSWobbleTimer_Timer()
    Dim DSAngle
    DSStepCount = DSStepCount + 1
    
    ' Simple back/forth pattern
    Select Case (DSStepCount Mod 4)
        Case 0: DSAngle = 0
        Case 1: DSAngle = 10
        Case 2: DSAngle = 0
        Case 3: DSAngle = -10
    End Select
    
    Primitive071.RotZ = DSAngle
    
    If DSStepCount >= 12 Then  ' about 0.75s at 25ms interval
        Primitive071.RotZ = 0
        DSWobbleTimer.Enabled = False
    End If
End Sub


Dim VaderStepCount, VaderTotalMoves

Sub VaderStartWobble()
    VaderStepCount = 0
    VaderTotalMoves = 0
    VaderWobbleTimer.Enabled = True
End Sub

Sub VaderWobbleTimer_Timer()
    Dim VaderAngle
    VaderStepCount = VaderStepCount + 1
    
    ' Simple back/forth pattern
    Select Case (VaderStepCount Mod 4)
        Case 0: VaderAngle = 0
        Case 1: VaderAngle = 5
        Case 2: VaderAngle = 0
        Case 3: VaderAngle = -5
    End Select
    
    Primitive012.RotZ = VaderAngle
    
    If VaderStepCount >= 12 Then  ' about 0.75s at 25ms interval
        Primitive012.RotZ = 0
        VaderWobbleTimer.Enabled = False
    End If
End Sub

sub Trigger037_hit
   If Not (TIE_IsRunning Or TIE2_IsRunning) Then
Tie083StartWobble
end if
playsound "tie"
addscore 500
end Sub

sub Trigger041_hit
    PlaySound "tie6"
Tie001StartWobble
addscore 500
end Sub


sub Kicker028_hit
Addscore 2000
If Rnd < 0.5 Then
    PlaySound "saucer_enter_1"
Else
    PlaySound "saucer_enter_2"
End If
If Rnd < 0.5 Then
    PlaySound "end4"
Else
    PlaySound "asteroids"
End If
Kicker028.timerenabled=1
end Sub

sub Kicker028_timer
kicker028.kick 180, 40
Kicker028.timerenabled=0
PlaySound SoundFXDOF("fx_kicker_loud", 118, DOFPulse, DOFContactors)
end Sub



'===============================
' ROLLING SOUND (throttled retriggers, VPX 10.7+) + DROP/CLUNK
'===============================

' --- Table size (your values) ---
Const TableWidth  = 952
Const TableHeight = 2162

' --- Rolling tuning (your values) ---
Const RollBaseVol   = 0.95
Const RollStartSPD  = 5
Const RollMaxSPD    = 80
Const RollPitchMin  = 0.90
Const RollPitchMax  = 1.25
Const MaxBalls      = 15

Const RollPrefix    = "BallRoll_"

' --- Retrigger tuning ---
Const RollTimerMS        = 30    ' timer interval
Const RetriggerCooldown  = 4     ' ticks between allowed restarts (4 * 30ms ≈ 120ms)
Const PanEpsilon         = 0.05  ' only restart if pan moves more than this
Const VolEpsilon         = 0.05  ' only restart if volume changes more than this (0..1)
Const PitchEpsilon       = 0.03  ' only restart if pitch changes more than this
Const SmoothAlpha        = 0.35  ' smoothing for pan/pitch (0..1). Higher = more responsive.

' --- DROP / CLUNK tuning (bolt-on) ---
Const DropZMin      = 20
Const DropZMax      = 80
Const DropVelZMin   = -0.8
Const DropSoftCut   = -7
Const DropCooldownT = 8          ' ticks between drop sounds per ball

' --- State arrays ---
Dim rolling(), lastPan(), lastVol(), lastPitch(), coolTicks(), DropTick()
Dim lastLiveIdx: lastLiveIdx = -1

'==================================================
' INIT
'==================================================
Sub InitRollingSound()
    ReDim rolling(MaxBalls - 1)
    ReDim lastPan(MaxBalls - 1)
    ReDim lastVol(MaxBalls - 1)
    ReDim lastPitch(MaxBalls - 1)
    ReDim coolTicks(MaxBalls - 1)
    ReDim DropTick(MaxBalls - 1)

    Dim i
    For i = 0 To MaxBalls - 1
        rolling(i)   = False
        lastPan(i)   = 0
        lastVol(i)   = 0
        lastPitch(i) = 1
        coolTicks(i) = 0
        DropTick(i)  = 0
    Next

    RollingSoundTimer.Interval = RollTimerMS
    RollingSoundTimer.Enabled  = True
End Sub

'==================================================
' TIMER
'==================================================
Sub RollingSoundTimer_Timer()
    Dim gBOT, i, cnt, b, spd, ratio, vol, pitch, pan, chName, needRestart

    gBOT = GetBalls()
    If Not IsArray(gBOT) Then Exit Sub
    cnt  = UBound(gBOT)

    For i = 0 To cnt
        Set b = gBOT(i)
        chName = RollPrefix & i

        ' Per-ball cooldown
        If coolTicks(i) > 0 Then
            coolTicks(i) = coolTicks(i) - 1
        End If

        spd = BallSpeed2D(b)

        ' ----------------
        ' ROLLING PER BALL
        ' ----------------
        If spd >= RollStartSPD Then
            ratio = Clamp01(spd / RollMaxSPD)
            vol   = ratio * RollBaseVol
            pitch = Lerp(RollPitchMin, RollPitchMax, ratio)

            ' Smooth pan & pitch to avoid jitter
            pan   = Smooth(lastPan(i),  AudioPan(b), SmoothAlpha)
            pitch = Smooth(lastPitch(i), pitch,      SmoothAlpha)

            ' Decide if we really need to restart the loop
            needRestart = False
            If Abs(pan - lastPan(i)) > PanEpsilon Then
                needRestart = True
            End If
            If Abs(vol - lastVol(i)) > VolEpsilon Then
                needRestart = True
            End If
            If Abs(pitch - lastPitch(i)) > PitchEpsilon Then
                needRestart = True
            End If

            If Not rolling(i) Then
                ' First start for this ball index
                PlaySound chName, -1, vol, pan, 0, pitch
                rolling(i) = True
                coolTicks(i) = RetriggerCooldown
            Else
                If needRestart And coolTicks(i) = 0 Then
                    ' Throttled restart with new params
                    StopSound chName
                    PlaySound chName, -1, vol, pan, 0, pitch
                    coolTicks(i) = RetriggerCooldown
                End If
            End If

            ' Remember last applied values
            lastPan(i)   = pan
            lastVol(i)   = vol
            lastPitch(i) = pitch
        Else
            ' Too slow -> ensure stopped
            If rolling(i) Then
                StopSound chName
                rolling(i) = False
            End If
            lastVol(i) = 0
        End If

        ' ----------------
        ' DROP / CLUNK PER BALL (bolt-on)
        ' ----------------
        If b.VelZ < DropVelZMin Then
            If b.Z > DropZMin Then
                If b.Z < DropZMax Then
                    If DropTick(i) >= DropCooldownT Then
                        DropTick(i) = 0
                        If b.VelZ > DropSoftCut Then
                            RandomSoundBallBouncePlayfieldSoft b
                        Else
                            RandomSoundBallBouncePlayfieldHard b
                        End If
                    End If
                End If
            End If
        End If

        If DropTick(i) < DropCooldownT Then
            DropTick(i) = DropTick(i) + 1
        End If
    Next

    ' Stop any stray indices beyond last live ball
    Dim j
    For j = cnt + 1 To UBound(rolling)
        If rolling(j) Then
            StopSound RollPrefix & j
            rolling(j) = False
        End If
    Next

    lastLiveIdx = cnt
End Sub

'==================================================
' HELPERS (shared)
'==================================================
Function AudioPan(b)
    Dim t
    t = (b.X * 2 / TableWidth) - 1
    If t > 0 Then
        AudioPan = CSng(t ^ 10)
    Else
        AudioPan = CSng(-((-t) ^ 10))
    End If
End Function

Function BallSpeed2D(b)
    BallSpeed2D = Sqr(b.VelX * b.VelX + b.VelY * b.VelY)
End Function

Function Clamp01(v)
    If v < 0 Then
        Clamp01 = 0
    ElseIf v > 1 Then
        Clamp01 = 1
    Else
        Clamp01 = v
    End If
End Function

'Function Lerp(a, b, t)
'    Lerp = a + (b - a) * t
'End Function

Function Smooth(prev, target, alpha)
    Smooth = prev + (target - prev) * alpha
End Function

' Master volume fallback (won’t redefine VolumeDial if you already have it)
Function VD()
    On Error Resume Next
    VD = VolumeDial
    If Err.Number <> 0 Then
        Err.Clear
        VD = 1
    End If
    On Error GoTo 0
End Function

' Simple vertical-speed -> volume scaler
Function volz(ball)
    volz = Clamp01(Abs(ball.VelZ) / 35)
End Function

' Minimal PlaySoundAtLevelStatic using your pan function
Sub PlaySoundAtLevelStatic(snd, vol, aBall)
    PlaySound snd, 0, Clamp01(vol) * VD(), AudioPan(aBall), 0, 1
End Sub

'==================================================
' DROP SOUND PICKERS (match your asset names)
'  - Soft: Ball_Bounce_Playfield_Soft_1 .. _5
'  - Hard: Ball_Bounce_Playfield_Hard_1 .. _7
'==================================================
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
    Select Case Int(Rnd * 5) + 1   ' 1..5
        Case 1
            PlaySoundAtLevelStatic "Ball_Bounce_Playfield_Soft_1", volz(aBall), aBall
        Case 2
            PlaySoundAtLevelStatic "Ball_Bounce_Playfield_Soft_2", volz(aBall), aBall
        Case 3
            PlaySoundAtLevelStatic "Ball_Bounce_Playfield_Soft_3", volz(aBall), aBall
        Case 4
            PlaySoundAtLevelStatic "Ball_Bounce_Playfield_Soft_4", volz(aBall), aBall
        Case 5
            PlaySoundAtLevelStatic "Ball_Bounce_Playfield_Soft_5", volz(aBall), aBall
    End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
    PlaySoundAtLevelStatic "Ball_Bounce_Playfield_Hard_" & (Int(Rnd * 7) + 1), volz(aBall), aBall
End Sub


' ===============================
' Tiny hits (volume floor + speed scaling)
' Collections:
'   TinyHits_Plastic  -> "Tiny_Plastic_Hit"
'   TinyHits_Metal    -> "Tiny_Metal_Hit"
' Make sure: each collection has "Fire events for this collection" checked,
' and each object inside has "Has Hit Event" checked and is collidable.
' ===============================

' Tuning
' ---- TinyHits_Plastic using Rubber_* and Rubber_Strong_* (randomized) ----
' Uses: AudioPan(b). No other helpers required.

' Tuning
Const TINY_SPD_REF           = 65     ' speed that maps to "max"
Const PL_VOL_MIN             = 0.60   ' doubled floor so it never mutes
Const PL_VOL_MAX             = 1.00   ' doubled ceiling (clamped later)
Const PL_STRONG_THRESHOLD    = 0.2   ' 0..1 ratio; above this use Rubber_Strong_*

Sub TinyHits_Plastic_Hit(idx)
'GIx_SmallEffect
GIx_TinyHit_Trigger
    Dim o, b, spd, ratio, vol, master, snd

    Set o = TinyHits_Plastic(idx)
    Set b = ActiveBall
    If b Is Nothing Then Exit Sub

    ' 2D speed -> 0..1 ratio (clamped)
    spd = Sqr(b.VelX*b.VelX + b.VelY*b.VelY)
    ratio = spd / TINY_SPD_REF
    If ratio < 0 Then
        ratio = 0
    ElseIf ratio > 1 Then
        ratio = 1
    End If

    ' volume = floor + scaled range
    vol = PL_VOL_MIN + ratio * (PL_VOL_MAX - PL_VOL_MIN)

    ' optional master (VolumeDial if present)
    On Error Resume Next
    master = VolumeDial
    If Err.Number <> 0 Then
        Err.Clear
        master = 1
    End If
    On Error GoTo 0
    vol = vol * master

    ' double it again for global "twice as loud" boost
    vol = vol * 2
    If vol > 1 Then vol = 1

    ' pick a sound based on intensity, randomized within set
    If ratio >= PL_STRONG_THRESHOLD Then
        snd = PickRubberStrong()
    Else
        snd = PickRubberSoft()
    End If

    ' play from ball position (uses your AudioPan)
    PlaySound snd, 0, vol, AudioPan(b), 0, 1
End Sub

' --- Random pickers for your sound names ---

Function PickRubberSoft()
    ' Your list: Rubber_2,3,4,5,7,8,9 (6 omitted)
    Select Case Int(Rnd * 7)   ' 0..6
        Case 0: PickRubberSoft = "Rubber_2"
        Case 1: PickRubberSoft = "Rubber_3"
        Case 2: PickRubberSoft = "Rubber_4"
        Case 3: PickRubberSoft = "Rubber_5"
        Case 4: PickRubberSoft = "Rubber_7"
        Case 5: PickRubberSoft = "Rubber_8"
        Case 6: PickRubberSoft = "Rubber_9"
        '   Case 0: PickRubberSoft = "fx_enter"
        'Case 1: PickRubberSoft = "fx_enter"
        'Case 2: PickRubberSoft = "fx_enter"
        'Case 3: PickRubberSoft = "fx_enter"
        'Case 4: PickRubberSoft = "fx_enter"
        'Case 5: PickRubberSoft = "fx_enter"
        'Case 6: PickRubberSoft = "fx_enter"
    End Select
End Function

Function PickRubberStrong()
    ' Your list: Rubber_Strong_1..Rubber_Strong_9
    Select Case Int(Rnd * 9)   ' 0..8
        Case 0: PickRubberStrong = "Rubber_Strong_1"
        Case 1: PickRubberStrong = "Rubber_Strong_2"
        Case 2: PickRubberStrong = "Rubber_Strong_3"
        Case 3: PickRubberStrong = "Rubber_Strong_4"
        Case 4: PickRubberStrong = "Rubber_Strong_5"
        Case 5: PickRubberStrong = "Rubber_Strong_6"
        Case 6: PickRubberStrong = "Rubber_Strong_7"
        Case 7: PickRubberStrong = "Rubber_Strong_8"
        Case 8: PickRubberStrong = "Rubber_Strong_9"
       '     Case 0: PickRubberStrong = "fx_kicker"
       ' Case 1: PickRubberStrong = "fx_kicker"
       ' Case 2: PickRubberStrong = "fx_kicker"
       ' Case 3: PickRubberStrong = "fx_kicker"
       ' Case 4: PickRubberStrong = "fx_kicker"
       ' Case 5: PickRubberStrong = "fx_kicker"
       ' Case 6: PickRubberStrong = "fx_kicker"
        'Case 7: PickRubberStrong = "fx_kicker"
        'Case 8: PickRubberStrong = "fx_kicker"
    End Select
End Function



' ---- TinyHits_Metal ----
' ---- TinyHits_Metal using Metal_Touch_1..13 (randomized) ----
' Tuning
Const MET_SPD_REF        = 25      ' speed that maps to "max"
Const MET_VOL_MIN        = 0.25    ' floor so it never mutes
Const MET_VOL_MAX        = 0.99    ' louder ceiling for your metal touches

Sub TinyHits_Metal_Hit(idx)
    Dim o, b, spd, ratio, vol, master, snd

    Set o = TinyHits_Metal(idx)
    Set b = ActiveBall
    If b Is Nothing Then Exit Sub

    ' 2D speed -> 0..1 ratio (clamped)
    spd = Sqr(b.VelX*b.VelX + b.VelY*b.VelY)
    ratio = spd / MET_SPD_REF
    If ratio < 0 Then
        ratio = 0
    ElseIf ratio > 1 Then
        ratio = 1
    End If

    ' volume = floor + scaled range
    vol = MET_VOL_MIN + ratio * (MET_VOL_MAX - MET_VOL_MIN)

    ' master gain (uses VolumeDial if you have it)
    On Error Resume Next
    master = VolumeDial
    If Err.Number <> 0 Then
        Err.Clear
        master = 1
    End If
    On Error GoTo 0
    vol = vol * master
    If vol > 1 Then vol = 1

    ' pick a random metal touch from your set
    snd = PickMetalTouch()

    ' play from ball position
    PlaySound snd, 0, vol, AudioPan(b), 0, 1
End Sub

Function PickMetalTouch()
    ' Randomly returns "Metal_Touch_1" .. "Metal_Touch_13"
    PickMetalTouch = "Metal_Touch_" & (Int(Rnd * 13) + 1)
End Function


' ---- TinyHits_Rubber ----
' ---- TinyHits_Rubber using Flipper_Rubber_1..7 ----
' ---- TinyHits_Rubber ----
' ---- TinyHits_Rubber using Flipper_Rubber_1..7 ----

Const RUB_SPD_REF          = 65     ' speed that maps to "max"
Const RUB_VOL_MIN          = 0.25   ' floor so it never mutes
Const RUB_VOL_MAX          = 0.85   ' louder ceiling for rubber bounces

Sub TinyHits_Rubber_Hit(idx)
    Dim o, b, spd, ratio, vol, master, snd

    Set o = TinyHits_Rubber(idx)
    Set b = ActiveBall
    If b Is Nothing Then Exit Sub

    ' 2D ball speed
    spd = Sqr(b.VelX*b.VelX + b.VelY*b.VelY)
    ratio = spd / RUB_SPD_REF
    If ratio < 0 Then ratio = 0
    If ratio > 1 Then ratio = 1

    vol = RUB_VOL_MIN + ratio * (RUB_VOL_MAX - RUB_VOL_MIN)

    On Error Resume Next
    master = VolumeDial
    If Err.Number <> 0 Then
        Err.Clear : master = 1
    End If
    On Error GoTo 0
    vol = vol * master
    If vol > 1 Then vol = 1

    ' pick one of your flipper rubber sounds
    snd = PickFlipperRubber()

    PlaySound snd, 0, vol, AudioPan(b), 0, 1
End Sub

Function PickFlipperRubber()
    ' Randomly returns "Flipper_Rubber_1" .. "Flipper_Rubber_7"
    PickFlipperRubber = "Flipper_Rubber_" & (Int(Rnd * 7) + 1)
End Function


' =====================================================================
' Primitive083: Down-left → Circle (≥1 lap, tangent exit) → Straight back
'   • Nose-forward during flight (RotZ steered; RotX/RotY held at 90/39)
'   • No jumps (tangent into & out of the circle)
'   • On finish: RESTORE exact original X/Y/Z and RotX/RotY/RotZ
'   • All names prefixed with TIE_ to avoid collisions
' =====================================================================


Const TIE_PI = 3.1415926535
Dim TIE_IsRunning

' --------- Flight knobs ----------
Dim TIE_SPEED_UPS        : TIE_SPEED_UPS        = 360      ' units/s
Dim TIE_TIMER_MS         : TIE_TIMER_MS         = 20       ' 50 fps
Dim TIE_OUT_HEADING_DEG  : TIE_OUT_HEADING_DEG  = 110      ' ≈ down-left; 90 = straight down
Dim TIE_OUT_DISTANCE     : TIE_OUT_DISTANCE     = 500      ' straight-out distance
Dim TIE_CIRCLE_RADIUS    : TIE_CIRCLE_RADIUS    = 160      ' circle radius
Dim TIE_CIRCLE_CW        : TIE_CIRCLE_CW        = True     ' True = clockwise (VPX Y-down)
Dim TIE_YAW_OFFSET       : TIE_YAW_OFFSET       = 90       ' adjust if model’s nose is offset

' Exit circle only when tangent points to the start (no jump)
Dim TIE_MIN_LAP_DEG      : TIE_MIN_LAP_DEG      = 360
Dim TIE_EXIT_TOL_DEG     : TIE_EXIT_TOL_DEG     = 3

' Snap threshold so straights end exactly at the target
Dim TIE_SNAP_EPS         : TIE_SNAP_EPS         = 2.0

' --------- Internals ----------
' Original state (to restore exactly at the end)
Dim TIE_OrigX, TIE_OrigY, TIE_OrigZ
Dim TIE_OrigRotX, TIE_OrigRotY, TIE_OrigRotZ

' Flight state
Dim TIE_StartX, TIE_StartY, TIE_StartZ
Dim TIE_Phase              ' 0=out, 1=circle, 2=return, 3=done
Dim TIE_TargetX, TIE_TargetY
Dim TIE_Cx, TIE_Cy, TIE_R, TIE_Phi, TIE_PhiStep, TIE_PhiAccum

' ----------------- Init -----------------
Sub InitTieFlyCircleBack()
If TIE_IsRunning Or TIE2_IsRunning Then Exit Sub
playsound "tieflight"
    ' Snapshot EXACT original pose to restore later
    TIE_OrigX    = Primitive083.X
    TIE_OrigY    = Primitive083.Y
    TIE_OrigZ    = Primitive083.Z
    TIE_OrigRotX = Primitive083.RotX
    TIE_OrigRotY = Primitive083.RotY
    TIE_OrigRotZ = Primitive083.RotZ

    TIE_StartX = TIE_OrigX
    TIE_StartY = TIE_OrigY
    TIE_StartZ = TIE_OrigZ

    ' First straight-leg target (down-left)
    Dim th
    th = TIE_OUT_HEADING_DEG * TIE_PI / 180
    TIE_TargetX = TIE_StartX + TIE_OUT_DISTANCE * Cos(th)
    TIE_TargetY = TIE_StartY + TIE_OUT_DISTANCE * Sin(th)

    ' Face outbound heading and keep flat (your baseline: RotX=90, RotY=39)
    Primitive083.RotZ = TIE_OUT_HEADING_DEG + TIE_YAW_OFFSET
    Primitive083.RotX = 90
    Primitive083.RotY = 39

    TIE_Phase = 0
    TIE_Phi = 0 : TIE_PhiAccum = 0

    TieTimer.Interval = TIE_TIMER_MS
TIE_IsRunning = True
TieLightOn
    TieTimer.Enabled  = True
End Sub

' ----------------- Timer -----------------
Sub TieTimer_Timer()
    Dim dt, stepU
    dt = TieTimer.Interval / 1000.0
    stepU = TIE_SPEED_UPS * dt

    Select Case TIE_Phase
        Case 0: TIE_StraightTo TIE_TargetX, TIE_TargetY, stepU, TIE_OUT_HEADING_DEG, True
        Case 1: TIE_StepCircle stepU
        Case 2: TIE_StraightTo TIE_StartX, TIE_StartY, stepU, TIE_NormalizeDeg(TIE_OUT_HEADING_DEG + 180), False
        Case 3:
            ' Already restored; stop timer
            Light001.state=0
            Trigger006.enabled=0
            TieTimer.Enabled = False
    End Select
End Sub

' ----------------- Straight segment -----------------
Sub TIE_StraightTo(ByVal tx, ByVal ty, ByVal stepU, ByVal fixedHeadingDeg, ByVal alignHeading)
    Dim cx, cy, dx, dy, dist, ux, uy, nx, ny, hdg
    cx = Primitive083.X : cy = Primitive083.Y
    dx = tx - cx : dy = ty - cy
    dist = Sqr(dx*dx + dy*dy)

    ' If close enough, snap exactly to target
    If dist <= stepU + TIE_SNAP_EPS Then
        Primitive083.X = tx
        Primitive083.Y = ty

        If alignHeading Then
            hdg = fixedHeadingDeg
        Else
            hdg = TIE_Atn2Deg(ty - cy, tx - cx)
        End If
        Primitive083.RotZ = hdg + TIE_YAW_OFFSET
        Primitive083.RotX = 90
        Primitive083.RotY = 39

        ' Start next phase, or finish and RESTORE
        If TIE_Phase = 0 Then
            TIE_SetupCircle fixedHeadingDeg
            TIE_Phase = 1
        ElseIf TIE_Phase = 2 Then
            TIE_RestoreOriginalPose          ' <<< put it back exactly
            TIE_Phase = 3
        End If
        Exit Sub
    End If

    ' Move along the straight
    ux = dx / dist : uy = dy / dist
    nx = cx + ux * stepU : ny = cy + uy * stepU
    Primitive083.X = nx : Primitive083.Y = ny

    ' Nose-forward + flat (90/39)
    If alignHeading Then
        hdg = fixedHeadingDeg
    Else
        hdg = TIE_Atn2Deg(uy, ux)
    End If
    Primitive083.RotZ = hdg + TIE_YAW_OFFSET
    Primitive083.RotX = 90
    Primitive083.RotY = 39
End Sub

' ----------------- Circle (tangent in, tangent out) -----------------
Sub TIE_SetupCircle(ByVal incomingHeadingDeg)
    ' Center on RIGHT normal to incoming heading → starts tangent (no jump)
    Dim th, nX, nY, dirSign, ticksPerSec
    th = incomingHeadingDeg * TIE_PI / 180

    If TIE_CIRCLE_CW Then
        ' Right normal in VPX (Y-down): nR = (-sinθ, +cosθ)
        nX = -Sin(th) : nY =  Cos(th)
        dirSign = +1  ' + step = CW
    Else
        nX =  Sin(th) : nY = -Cos(th)
        dirSign = -1  ' - step = CCW
    End If

    TIE_R  = TIE_CIRCLE_RADIUS
    TIE_Cx = Primitive083.X + nX * TIE_R
    TIE_Cy = Primitive083.Y + nY * TIE_R

    ' Start exactly where we are on the circle
    TIE_Phi = TIE_Atn2(Primitive083.Y - TIE_Cy, Primitive083.X - TIE_Cx)
    TIE_PhiAccum = 0

    ticksPerSec = 1000.0 / TieTimer.Interval
    If TIE_R < 1 Then TIE_R = 1
    TIE_PhiStep = dirSign * (TIE_SPEED_UPS / TIE_R) / ticksPerSec
End Sub

Sub TIE_StepCircle(ByVal stepU)
    Dim oldPhi, nx, ny, tx, ty, headingDeg
    oldPhi = TIE_Phi
    TIE_Phi  = TIE_Phi + TIE_PhiStep
    TIE_PhiAccum = TIE_PhiAccum + Abs((TIE_Phi - oldPhi) * 180 / TIE_PI)

    ' Follow circle
    nx = TIE_Cx + TIE_R * Cos(TIE_Phi)
    ny = TIE_Cy + TIE_R * Sin(TIE_Phi)
    Primitive083.X = nx
    Primitive083.Y = ny

    ' Tangent → heading; keep flat (90/39)
    If TIE_PhiStep >= 0 Then
        tx = -Sin(TIE_Phi) : ty =  Cos(TIE_Phi)   ' CW
    Else
        tx =  Sin(TIE_Phi) : ty = -Cos(TIE_Phi)   ' CCW
    End If
    headingDeg = TIE_Atn2Deg(ty, tx)
    Primitive083.RotZ = headingDeg + TIE_YAW_OFFSET
    Primitive083.RotX = 90
    Primitive083.RotY = 39

    ' Exit only when tangent points to start
    If TIE_PhiAccum >= TIE_MIN_LAP_DEG Then
        Dim toStartDeg, diff
        toStartDeg = TIE_Atn2Deg(TIE_StartY - ny, TIE_StartX - nx)
        diff = TIE_AngDiffDeg(headingDeg, toStartDeg)
        If Abs(diff) <= TIE_EXIT_TOL_DEG Then
            TIE_Phase = 2
        End If
    End If
End Sub

' ----------------- Restore EXACT original pose -----------------
Sub TIE_RestoreOriginalPose()
    Primitive083.X    = TIE_OrigX
    Primitive083.Y    = TIE_OrigY
    Primitive083.Z    = TIE_OrigZ
    Primitive083.RotX = TIE_OrigRotX
    Primitive083.RotY = TIE_OrigRotY
    Primitive083.RotZ = TIE_OrigRotZ
TIE_IsRunning = False
End Sub

' ----------------- Math helpers -----------------
Function TIE_Atn2(y, x)
    If x = 0 Then
        If y > 0 Then
            TIE_Atn2 = TIE_PI / 2
        ElseIf y < 0 Then
            TIE_Atn2 = -TIE_PI / 2
        Else
            TIE_Atn2 = 0
        End If
    Else
        TIE_Atn2 = Atn(y / x)
        If x < 0 Then
            If y >= 0 Then
                TIE_Atn2 = TIE_Atn2 + TIE_PI
            Else
                TIE_Atn2 = TIE_Atn2 - TIE_PI
            End If
        End If
    End If
End Function

Function TIE_Atn2Deg(y, x)
    TIE_Atn2Deg = (TIE_Atn2(y, x) * 180 / TIE_PI)
End Function

Function TIE_NormalizeDeg(a)
    Do While a < 0: a = a + 360: Loop
    Do While a >= 360: a = a - 360: Loop
    TIE_NormalizeDeg = a
End Function

' Smallest signed angle a→b (degrees), in [-180, +180]
Function TIE_AngDiffDeg(a, b)
    Dim d
    d = (b - a)
    Do While d > 180: d = d - 360: Loop
    Do While d < -180: d = d + 360: Loop
    TIE_AngDiffDeg = d
End Function

' =====================================================================
' ALT PATH: Primitive083 – More-left → CCW Circle → Straight back
'   • Uses its own names (TIE2_*) and its own timer: TieTimer2
'   • Guards against running while the other path is active
'   • Restores exact original X/Y/Z and RotX/RotY/RotZ on finish
'   • Orientation held flat (RotX=90, RotY=39); nose via RotZ
' =====================================================================

Const TIE2_PI = 3.1415926535
Dim TIE2_IsRunning

' --------- Flight knobs (noticeably different from first) ----------
Dim TIE2_SPEED_UPS        : TIE2_SPEED_UPS        = 360     ' units/s
Dim TIE2_TIMER_MS         : TIE2_TIMER_MS         = 20      ' 50 fps
Dim TIE2_OUT_HEADING_DEG  : TIE2_OUT_HEADING_DEG  = 135     ' more LEFT (vs 110)
Dim TIE2_OUT_DISTANCE     : TIE2_OUT_DISTANCE     = 600     ' a bit farther
Dim TIE2_CIRCLE_RADIUS    : TIE2_CIRCLE_RADIUS    = 200     ' larger circle
Dim TIE2_CIRCLE_CW        : TIE2_CIRCLE_CW        = False   ' CCW (vs CW)
Dim TIE2_YAW_OFFSET       : TIE2_YAW_OFFSET       = 90

' Exit circle only when tangent points to the start (no jump)
Dim TIE2_MIN_LAP_DEG      : TIE2_MIN_LAP_DEG      = 360
Dim TIE2_EXIT_TOL_DEG     : TIE2_EXIT_TOL_DEG     = 3

' Snap threshold so straights end exactly at the target
Dim TIE2_SNAP_EPS         : TIE2_SNAP_EPS         = 2.0

' --------- Internals ----------
' Original state (to restore exactly at the end)
Dim TIE2_OrigX, TIE2_OrigY, TIE2_OrigZ
Dim TIE2_OrigRotX, TIE2_OrigRotY, TIE2_OrigRotZ

' Flight state
Dim TIE2_StartX, TIE2_StartY, TIE2_StartZ
Dim TIE2_Phase              ' 0=out, 1=circle, 2=return, 3=done
Dim TIE2_TargetX, TIE2_TargetY
Dim TIE2_Cx, TIE2_Cy, TIE2_R, TIE2_Phi, TIE2_PhiStep, TIE2_PhiAccum

' ----------------- Init (ALT) -----------------
Sub InitTieFlyCircleBack2()
    ' Don’t start if either path is running
    If TIE_IsRunning Or TIE2_IsRunning Then Exit Sub
    playsound "tieflight"

    ' Snapshot EXACT original pose to restore later
    TIE2_OrigX    = Primitive083.X
    TIE2_OrigY    = Primitive083.Y
    TIE2_OrigZ    = Primitive083.Z
    TIE2_OrigRotX = Primitive083.RotX
    TIE2_OrigRotY = Primitive083.RotY
    TIE2_OrigRotZ = Primitive083.RotZ

    TIE2_StartX = TIE2_OrigX
    TIE2_StartY = TIE2_OrigY
    TIE2_StartZ = TIE2_OrigZ

    ' First straight-leg target (more LEFT)
    Dim th
    th = TIE2_OUT_HEADING_DEG * TIE2_PI / 180
    TIE2_TargetX = TIE2_StartX + TIE2_OUT_DISTANCE * Cos(th)
    TIE2_TargetY = TIE2_StartY + TIE2_OUT_DISTANCE * Sin(th)

    ' Face outbound heading and keep flat (RotX=90, RotY=39)
    Primitive083.RotZ = TIE2_OUT_HEADING_DEG + TIE2_YAW_OFFSET
    Primitive083.RotX = 90
    Primitive083.RotY = 39

    TIE2_Phase = 0
    TIE2_Phi = 0 : TIE2_PhiAccum = 0

    TieTimer2.Interval = TIE2_TIMER_MS
    TIE2_IsRunning = True
    TieLightOn
    TieTimer2.Enabled  = True
End Sub

' ----------------- Timer (ALT) -----------------
Sub TieTimer2_Timer()
    Dim dt, stepU
    dt = TieTimer2.Interval / 1000.0
    stepU = TIE2_SPEED_UPS * dt

    Select Case TIE2_Phase
        Case 0: TIE2_StraightTo TIE2_TargetX, TIE2_TargetY, stepU, TIE2_OUT_HEADING_DEG, True
        Case 1: TIE2_StepCircle stepU
        Case 2: TIE2_StraightTo TIE2_StartX, TIE2_StartY, stepU, TIE2_NormalizeDeg(TIE2_OUT_HEADING_DEG + 180), False
        Case 3:
            Light001.state=0
            Trigger006.enabled=0
            TieTimer2.Enabled = False
    End Select
End Sub

' ----------------- Straight segment (ALT) -----------------
Sub TIE2_StraightTo(ByVal tx, ByVal ty, ByVal stepU, ByVal fixedHeadingDeg, ByVal alignHeading)
    Dim cx, cy, dx, dy, dist, ux, uy, nx, ny, hdg
    cx = Primitive083.X : cy = Primitive083.Y
    dx = tx - cx : dy = ty - cy
    dist = Sqr(dx*dx + dy*dy)

    ' If close enough, snap exactly to target
    If dist <= stepU + TIE2_SNAP_EPS Then
        Primitive083.X = tx
        Primitive083.Y = ty

        If alignHeading Then
            hdg = fixedHeadingDeg
        Else
            hdg = TIE2_Atn2Deg(ty - cy, tx - cx)
        End If
        Primitive083.RotZ = hdg + TIE2_YAW_OFFSET
        Primitive083.RotX = 90
        Primitive083.RotY = 39

        ' Start next phase, or finish and RESTORE
        If TIE2_Phase = 0 Then
            TIE2_SetupCircle fixedHeadingDeg
            TIE2_Phase = 1
        ElseIf TIE2_Phase = 2 Then
            TIE2_RestoreOriginalPose
            TIE2_Phase = 3
        End If
        Exit Sub
    End If

    ' Move along the straight
    ux = dx / dist : uy = dy / dist
    nx = cx + ux * stepU : ny = cy + uy * stepU
    Primitive083.X = nx : Primitive083.Y = ny

    ' Nose-forward + flat (90/39)
    If alignHeading Then
        hdg = fixedHeadingDeg
    Else
        hdg = TIE2_Atn2Deg(uy, ux)
    End If
    Primitive083.RotZ = hdg + TIE2_YAW_OFFSET
    Primitive083.RotX = 90
    Primitive083.RotY = 39
End Sub

' ----------------- Circle (ALT: CCW, tangent in/out) -----------------
Sub TIE2_SetupCircle(ByVal incomingHeadingDeg)
    ' Center on LEFT/RIGHT normal so we enter tangent; CCW here
    Dim th, nX, nY, dirSign, ticksPerSec
    th = incomingHeadingDeg * TIE2_PI / 180

    If TIE2_CIRCLE_CW Then
        ' CW: right normal in VPX (Y-down): nR = (-sinθ, +cosθ)
        nX = -Sin(th) : nY =  Cos(th)
        dirSign = +1   ' + step = CW
    Else
        ' CCW: left normal: nL = (sinθ, -cosθ)
        nX =  Sin(th) : nY = -Cos(th)
        dirSign = -1   ' - step = CCW
    End If

    TIE2_R  = TIE2_CIRCLE_RADIUS
    TIE2_Cx = Primitive083.X + nX * TIE2_R
    TIE2_Cy = Primitive083.Y + nY * TIE2_R

    ' Start exactly where we are on the circle
    TIE2_Phi = TIE2_Atn2(Primitive083.Y - TIE2_Cy, Primitive083.X - TIE2_Cx)
    TIE2_PhiAccum = 0

    ticksPerSec = 1000.0 / TieTimer2.Interval
    If TIE2_R < 1 Then TIE2_R = 1
    TIE2_PhiStep = dirSign * (TIE2_SPEED_UPS / TIE2_R) / ticksPerSec
End Sub

Sub TIE2_StepCircle(ByVal stepU)
    Dim oldPhi, nx, ny, tx, ty, headingDeg
    oldPhi = TIE2_Phi
    TIE2_Phi  = TIE2_Phi + TIE2_PhiStep
    TIE2_PhiAccum = TIE2_PhiAccum + Abs((TIE2_Phi - oldPhi) * 180 / TIE2_PI)

    ' Follow circle
    nx = TIE2_Cx + TIE2_R * Cos(TIE2_Phi)
    ny = TIE2_Cy + TIE2_R * Sin(TIE2_Phi)
    Primitive083.X = nx
    Primitive083.Y = ny

    ' Tangent → heading; keep flat (90/39)
    If TIE2_PhiStep >= 0 Then
        tx = -Sin(TIE2_Phi) : ty =  Cos(TIE2_Phi)   ' CW
    Else
        tx =  Sin(TIE2_Phi) : ty = -Cos(TIE2_Phi)   ' CCW
    End If
    headingDeg = TIE2_Atn2Deg(ty, tx)
    Primitive083.RotZ = headingDeg + TIE2_YAW_OFFSET
    Primitive083.RotX = 90
    Primitive083.RotY = 39

    ' Exit only when tangent points to start (no jump)
    If TIE2_PhiAccum >= TIE2_MIN_LAP_DEG Then
        Dim toStartDeg, diff
        toStartDeg = TIE2_Atn2Deg(TIE2_StartY - ny, TIE2_StartX - nx)
        diff = TIE2_AngDiffDeg(headingDeg, toStartDeg)
        If Abs(diff) <= TIE2_EXIT_TOL_DEG Then
            TIE2_Phase = 2
        End If
    End If
End Sub

' ----------------- Restore EXACT original pose (ALT) -----------------
Sub TIE2_RestoreOriginalPose()
    Primitive083.X    = TIE2_OrigX
    Primitive083.Y    = TIE2_OrigY
    Primitive083.Z    = TIE2_OrigZ
    Primitive083.RotX = TIE2_OrigRotX
    Primitive083.RotY = TIE2_OrigRotY
    Primitive083.RotZ = TIE2_OrigRotZ
    TIE2_IsRunning = False
End Sub

' ----------------- Math helpers (ALT) -----------------
Function TIE2_Atn2(y, x)
    If x = 0 Then
        If y > 0 Then
            TIE2_Atn2 = TIE2_PI / 2
        ElseIf y < 0 Then
            TIE2_Atn2 = -TIE2_PI / 2
        Else
            TIE2_Atn2 = 0
        End If
    Else
        TIE2_Atn2 = Atn(y / x)
        If x < 0 Then
            If y >= 0 Then
                TIE2_Atn2 = TIE2_Atn2 + TIE2_PI
            Else
                TIE2_Atn2 = TIE2_Atn2 - TIE2_PI
            End If
        End If
    End If
End Function

Function TIE2_Atn2Deg(y, x)
    TIE2_Atn2Deg = (TIE2_Atn2(y, x) * 180 / TIE2_PI)
End Function

Function TIE2_NormalizeDeg(a)
    Do While a < 0: a = a + 360: Loop
    Do While a >= 360: a = a - 360: Loop
    TIE2_NormalizeDeg = a
End Function

' Smallest signed angle a→b (degrees), in [-180, +180]
Function TIE2_AngDiffDeg(a, b)
    Dim d
    d = (b - a)
    Do While d > 180: d = d - 360: Loop
    Do While d < -180: d = d + 360: Loop
    TIE2_AngDiffDeg = d
End Function


sub trigger042_hit
If Rnd < 0.5 Then
      Start_Primitive033_SpinHalfway
   Else
       Start_Primitive033_OneWay
   End If

end Sub

'=============================
' Primitive031: slow left-right sway + gentle spin (with random micro-pauses)
'=============================

Const P031_SWAY_UNITS     = 25          ' ±25 VP units side-to-side
Const P031_DURATION_MS    = 8000        ' one full L→R→L cycle (slower = larger)
Const P031_TIMER_MS       = 20          ' must match Prim031Timer.Interval

' Spin amount (angle peaks mid-cycle, returns to zero at ends)
Const P031_SPIN_MIN_DEG   = 20
Const P031_SPIN_MAX_DEG   = 45

' --- pause behavior (random micro-pauses) ---
Const P031_PAUSE_PROB     = 0.012        ' 0..1 chance per tick to start a pause (2%)
Const P031_PAUSE_MS_MIN   = 1000          ' min pause length (ms)
Const P031_PAUSE_MS_MAX   = 4000        ' max pause length (ms)

Dim P031_BaseX
Dim P031_BaseRotX, P031_BaseRotY, P031_BaseRotZ
Dim P031_t
Dim P031_spinDeg

' pause state
Dim P031_isPaused, P031_pauseTicks

' Call this once on table init (e.g., from Table1_Init)

Sub P031_Init()
    With Primitive031
        P031_BaseX     = .X
        P031_BaseRotX  = .RotX
        P031_BaseRotY  = .RotY
        P031_BaseRotZ  = .RotZ
    End With

    
    P031_spinDeg = P031_SPIN_MIN_DEG + Rnd() * (P031_SPIN_MAX_DEG - P031_SPIN_MIN_DEG)

    P031_t = 0
    P031_isPaused = False
    P031_pauseTicks = 0

    Prim031Timer.Interval = P031_TIMER_MS
    Prim031Timer.Enabled  = True
End Sub

Sub Prim031Timer_Timer()
    ' --- random pause handling ---
    If P031_isPaused Then
        P031_pauseTicks = P031_pauseTicks - 1
        If P031_pauseTicks <= 0 Then
            P031_isPaused = False
        End If
        ' while paused, do NOT advance time; just reapply current pose
    Else
        ' small chance to start a micro-pause
        If Rnd() < P031_PAUSE_PROB Then
            Dim ms, ticks
            ms = P031_PAUSE_MS_MIN + Rnd() * (P031_PAUSE_MS_MAX - P031_PAUSE_MS_MIN)
            ticks = CInt((ms / P031_TIMER_MS) + 0.5)
            If ticks < 1 Then ticks = 1
            P031_pauseTicks = ticks
            P031_isPaused = True
            ' fall through without advancing time this tick (first pause frame holds)
        Else
            ' advance normalized time 0..1 only when not paused
            P031_t = P031_t + (P031_TIMER_MS / P031_DURATION_MS)
            If P031_t > 1 Then
                P031_t = P031_t - 1
                ' (optional) vary spin slightly each cycle
                ' P031_spinDeg = P031_SPIN_MIN_DEG + Rnd() * (P031_SPIN_MAX_DEG - P031_SPIN_MIN_DEG)
            End If
        End If
    End If

    ' Side-to-side sway: -1..+1 scaled to ±P031_SWAY_UNITS
    Dim offset : offset = P031_SWAY_UNITS * Sin(2 * 3.1415926535 * P031_t)

    ' Spin: 0 -> peak -> 0 over the cycle (eases in/out)
    Dim ang : ang = P031_spinDeg * Sin(3.1415926535 * P031_t)

    With Primitive031
        ' keep only X moving; lock rotations except Y spin
        .X    = P031_BaseX + offset
        .RotX = P031_BaseRotX
        .RotZ = P031_BaseRotZ
        .RotY = P031_BaseRotY + ang
    End With
End Sub


'========================================
' Primitive033: one-way move, ignore duplicates
'========================================
Const P033_INTERVAL = 20        ' ms (≈50 fps)
Const P033_SPEED    = 6         ' units per tick
Const P033_MARGIN   = 50        ' stop this far from left edge

Dim P033_StartX, P033_TargetX, P033_Running

Sub Start_Primitive033_OneWay()

    ' Don’t start again if it’s already moving
    If P033_Running Or P033b_Running Then Exit Sub

    P033_StartX   = Primitive033.X   ' starting X (place primitive on right)
    P033_TargetX  = P033_MARGIN      ' leftmost point
    P033_Running  = True
playsound "tie3"
    Primitive033_Timer.Interval = P033_INTERVAL
    Primitive033_Timer.Enabled  = True
End Sub

Sub Primitive033_Timer_Timer()
  If Not P033_Running Then
    Primitive033_Timer.Enabled = False
    Exit Sub
End If

    ' move left
    Dim nextX: nextX = Primitive033.X - P033_SPEED
    If nextX < P033_TargetX Then nextX = P033_TargetX
    Primitive033.X = nextX

    ' reached the left side?
    If Primitive033.X = P033_TargetX Then
        ' snap back to start instantly
        Primitive033.X = P033_StartX
        P033_Running = False          ' free to trigger again
        Primitive033_Timer.Enabled = False
    End If
End Sub
'===============================================================
' Primitive033: one-way move + lateral roll halfway (restores pose)
'===============================================================
Const P033b_INTERVAL = 20        ' ms (≈50 fps)
Const P033b_SPEED    = 6         ' units per tick
Const P033b_MARGIN   = 50        ' stop this far from left edge
Const P033b_SPINRATE = 5        ' degrees per tick while spinning

Dim P033b_StartX, P033b_TargetX, P033b_MidPoint
Dim P033b_Running, P033b_Spinning
Dim P033b_RotX0, P033b_RotY0, P033b_RotZ0
Dim P033b_SpinAngle

Sub Start_Primitive033_SpinHalfway()
    If P033_Running Or P033b_Running Then Exit Sub

    ' movement setup
    P033b_StartX   = Primitive033.X
    P033b_TargetX  = P033b_MARGIN
    P033b_MidPoint = (P033b_StartX + P033b_TargetX) / 2

    ' snapshot exact original orientation
    P033b_RotX0 = Primitive033.RotX
    P033b_RotY0 = Primitive033.RotY
    P033b_RotZ0 = Primitive033.RotZ

    ' state
    P033b_SpinAngle = 0
    P033b_Running   = True
    P033b_Spinning  = False

    PlaySound "tie4"
    Primitive033b_Timer.Interval = P033b_INTERVAL
    Primitive033b_Timer.Enabled  = True
End Sub

Sub Primitive033b_Timer_Timer()
If Not P033b_Running Then
    Primitive033b_Timer.Enabled = False
    Exit Sub
End If

    ' move left at constant speed
    Dim nextX: nextX = Primitive033.X - P033b_SPEED
    If nextX < P033b_TargetX Then nextX = P033b_TargetX
    Primitive033.X = nextX

    ' start lateral roll when halfway across
    If Not P033b_Spinning And Primitive033.X <= P033b_MidPoint Then
        P033b_Spinning = True
    End If

    If P033b_Spinning Then
        P033b_SpinAngle = P033b_SpinAngle + P033b_SPINRATE

        ' Lateral roll (side-to-side) without changing heading:
        ' Use RotY for a sideways roll. If this looks like pitch on your setup,
        ' comment the next line and use the RotX line instead.
        'Primitive033.RotY = P033_RotY0 + (P033_SpinAngle Mod 360)
        ' Alternative (if your axis mapping differs):
         Primitive033.RotX = P033b_RotX0 + (P033b_SpinAngle Mod 360)
    End If

    ' reached the left side?
    If Primitive033.X = P033b_TargetX Then
        ' snap back to start and fully restore original pose
        Primitive033.X    = P033b_StartX
        Primitive033.RotX = P033b_RotX0
        Primitive033.RotY = P033b_RotY0
        Primitive033.RotZ = P033b_RotZ0

        P033b_Running  = False
        P033b_Spinning = False
        Primitive033b_Timer.Enabled = False
    End If
End Sub


'=============================
' Ramp progress lights (Trigger045)
' - 170/171/172 = per-cycle ladder
' - 169, 181, 182 = stage progress indicators (Yavin, Hoth, Endor)
'=============================

' --- Light state constants (VPX) ---
Const L_OFF   = 0
Const L_ON    = 1
Const L_BLINK = 2

' --- Progress trackers ---
' RampStage: 0=before Yavin complete, 1=after Yavin (now Hoth), 2=after Hoth (now Endor), 3=all done
' RampStep:  0=none yet this cycle, 1=after first hit, 2=after second hit (third hit will advance stage)
Dim RampStage_045, RampStep_045

' Call this once from your Table_Init to set starting states
Sub Init_Trigger045_Progress()
    RampStage_045 = 0
    RampStep_045  = 0
    Light170.State = L_BLINK
    Light171.State = L_OFF
    Light172.State = L_OFF
    Light169.State = L_OFF
    Light181.State = L_OFF
    Light182.State = L_OFF
End Sub

' Utility to set a light safely
Sub SetLightState(lit, st)
    On Error Resume Next
    lit.State = st
    On Error GoTo 0
End Sub

'=============================
' Trigger: three-hit ladder per stage
' Hit #1: 170 solid, 171 blink, 172 off
' Hit #2 (only if 170 is solid): 171 solid, 172 blink
' Hit #3: advance stage -> progress light solid; 170 blink; 171/172 off
' Stages advance progress lights in order: 169, then 181, then 182
'=============================
Sub Trigger045_Hit()
fz_step
if LiPass003.state=2 then 'multiball
   if ultramode=1 then
   DMD_DisplaySceneTextWithPause "shoot for multiball", p1score, 10000
   qtimer.interval=1500 
   end if
LiPass003.state=0 'multiball
LiPass004.state=0 'multiball
LiPass005.state=2
LiPass009.state=2
LiPass010.state=2
primitive023.Visible = 0
Ramp049.Collidable = 0
playsound "clear"
end if
    ' If all three stages are already complete, do nothing
    If RampStage_045 >= 3 Then Exit Sub

    Select Case RampStep_045

        Case 0  ' First hit of the current cycle
            SetLightState Light170, L_ON
            SetLightState Light171, L_BLINK
            SetLightState Light172, L_OFF
            RampStep_045 = 1

        Case 1  ' Second hit (only valid if 170 is solid)
            If Light170.State = L_ON Then
                SetLightState Light171, L_ON
                SetLightState Light172, L_BLINK
                RampStep_045 = 2
            Else
                ' Safety fallback: if state drifted, treat as first hit
                SetLightState Light170, L_ON
                SetLightState Light171, L_BLINK
                SetLightState Light172, L_OFF
                RampStep_045 = 1
            End If

        Case 2  ' Third hit -> advance the story (lock a progress light)
            If Rnd < 0.5 Then
               Start_P020 'mouse droid appears
            end if
            Dim progLight
            If RampStage_045 = 0 Then
                pupevent 871
                Set progLight = Light169      ' Tatooine complete
            ElseIf RampStage_045 = 1 Then
                pupevent 872
                Set progLight = Light181      ' Hoth complete
            Else
                pupevent 873
                Set progLight = Light182      ' Endor complete
            End If

            SetLightState progLight, L_ON     ' lock in the stage completion

            ' Prep for next cycle (or finish if Endor was just completed)
            RampStage_045 = RampStage_045 + 1
            RampStep_045  = 0

            If RampStage_045 < 3 Then
                ' More stages remain: show 170 blinking to indicate "ready to start next cycle"
                SetLightState Light170, L_BLINK
            Else
                ' All stages complete: clean the ladder lights
                SetLightState Light170, L_OFF
            End If

            SetLightState Light171, L_OFF
            SetLightState Light172, L_OFF

    End Select
End Sub


'=============================
' Ramp/Trigger progress lights (Trigger036)
' - 166/167/168 = per-cycle ladder
' - 164, 179, 180 = stage progress indicators (Stage 1, 2, 3)
'=============================

' --- Progress trackers ---
' Stage: 0=before Stage1 complete, 1=after Stage1 (now Stage2), 2=after Stage2 (now Stage3), 3=all done
' Step:  0=none yet this cycle, 1=after first hit, 2=after second hit (third hit will advance stage)
Dim RampStage_036, RampStep_036

' Call once at game start (NOT each ball) if you want progress to persist ball-to-ball
Sub Init_Trigger036_Progress()
    RampStage_036 = 0
    RampStep_036  = 0
    Light166.State = L_BLINK
    Light167.State = L_OFF
    Light168.State = L_OFF
    Light164.State = L_OFF
    Light179.State = L_OFF
    Light180.State = L_OFF
End Sub

'=============================
' Trigger036: three-hit ladder per stage
' Hit #1: 166 solid, 167 blink, 168 off
' Hit #2 (only if 166 is solid): 167 solid, 168 blink
' Hit #3: advance stage -> progress light solid; 166 blink; 167/168 off
' Progress lights in order: 164, then 179, then 180
'=============================
Sub Trigger036_Hit()
PlayWireLoopSound
fz_step
if LiPass004.state=2 then 
   if ultramode=1 then
   DMD_DisplaySceneTextWithPause "shoot for multiball", p1score, 10000
   qtimer.interval=1500 
   end if
LiPass004.state=0
LiPass003.state=0
LiPass005.state=2
LiPass009.state=2
LiPass010.state=2
primitive023.Visible = 0
Ramp049.Collidable = 0
playsound "clear"
end if
ST71_StartSpin 360, 540, True
            PlayWireLoopSound
    ' If all three stages are complete, do nothing
    If RampStage_036 >= 3 Then Exit Sub

    Select Case RampStep_036

        Case 0  ' First hit of the current cycle
            SetLightState Light166, L_ON
            SetLightState Light167, L_BLINK
            SetLightState Light168, L_OFF
            RampStep_036 = 1

        Case 1  ' Second hit (only valid if 166 is solid)
            If Light166.State = L_ON Then
                SetLightState Light167, L_ON
                SetLightState Light168, L_BLINK
                RampStep_036 = 2
            Else
                ' Safety fallback: treat as first hit if state drifted
                SetLightState Light166, L_ON
                SetLightState Light167, L_BLINK
                SetLightState Light168, L_OFF
                RampStep_036 = 1
            End If

        Case 2  ' Third hit -> advance the story (lock a progress light)
            If Rnd < 0.5 Then
                 Start_P020 'mouse droid appears
            end if
            Dim prog036
            If RampStage_036 = 0 Then
                pupevent 874
                Set prog036 = Light164   ' Stage 1 complete
            ElseIf RampStage_036 = 1 Then
                pupevent 875
                Set prog036 = Light179   ' Stage 2 complete
            Else
                pupevent 876
                Set prog036 = Light180   ' Stage 3 complete
            End If

            SetLightState prog036, L_ON  ' lock in the stage completion

            ' Prep for next cycle (or finish if Stage 3 was just completed)
            RampStage_036 = RampStage_036 + 1
            RampStep_036  = 0

            If RampStage_036 < 3 Then
                ' More stages remain: show 166 blinking to indicate "ready to start next cycle"
                SetLightState Light166, L_BLINK
            Else
                ' All stages complete: clean the ladder lights
                SetLightState Light166, L_OFF
            End If

            SetLightState Light167, L_OFF
            SetLightState Light168, L_OFF

    End Select
End Sub

sub StartGameTie_timer
'Start_Primitive034_Right
If Rnd < 0.5 Then
   Start_Primitive033_SpinHalfway
Else
    Start_Primitive033_OneWay
End If
StartGameTie.enabled=0
end Sub



'========================================
' Primitive034: slow rightward one-shot
'========================================
Const P034_INTERVAL = 30        ' ms per tick (≈33 fps)
Const P034_SPEED    = 0.04       ' units per tick
Const P034_MARGIN   = 50        ' stop this far from right edge

Dim P034_Running, P034_StartX

Sub Start_Primitive034_Right()
    ' don’t start again if it’s already moving
    If P034_Running Then Exit Sub

    P034_StartX   = Primitive034.X   ' remember starting position
    P034_Running  = True
    Primitive034_Timer.Interval = P034_INTERVAL
    Primitive034_Timer.Enabled  = True
End Sub

Sub Primitive034_Timer_Timer()
    ' move to the right
    Primitive034.X = Primitive034.X + P034_SPEED

    ' reached end? stop + reset to start
    If Primitive034.X >= (TableWidth - P034_MARGIN) Then
        Primitive034.X = P034_StartX
        Primitive034_Timer.Enabled = False
        P034_Running = False
    End If
End Sub


'========================================
' Primitive036: left->right with late spin
' - Moves right slowly
' - Starts spinning after 25% of path
' - Snaps back to start at end
' - Ignores duplicate starts while running
'========================================
Const P036_INTERVAL     = 10       ' ms per tick (≈33 fps)
Const P036_SPEED        = 4      ' units per tick (move rate)
Const P036_MARGIN       = 50       ' right-edge margin
Const P036_SPIN_DEG     = 4        ' degrees per tick once spin starts
Const P036_SPIN_START   = 0.25     ' start spinning at 25% progress

Dim P036_Running
Dim P036_StartX, P036_EndX, P036_Distance
Dim P036_StartRotZ

Sub Start_Primitive036_Run()
    If P036_Running Then Exit Sub

    Dim rnd
    rnd = Int(Rnd * 3) + 1   ' random 1–3

    Select Case rnd
        Case 1: PlaySound "crash1"
        Case 2: PlaySound "crash2"
        Case 3: PlaySound "crash3"
    End Select


    ' Capture start and end geometry
    P036_StartX    = Primitive036.X
    P036_EndX      = TableWidth - P036_MARGIN
    P036_Distance  = P036_EndX - P036_StartX
    If P036_Distance <= 0 Then
        ' Nothing to do if already past end
        Exit Sub
    End If

    ' Remember starting rotation so we can restore after snap-back
    P036_StartRotZ = Primitive036.RotZ

    P036_Running = True
    Primitive036_Timer.Interval = P036_INTERVAL
    Primitive036_Timer.Enabled  = True
End Sub

Sub Stop_Primitive036_Run()
    Primitive036_Timer.Enabled = False
    P036_Running = False
End Sub

Sub Primitive036_Timer_Timer()
    ' Advance position to the right
    Primitive036.X = Primitive036.X + P036_SPEED

    ' Progress 0..1 across the planned path
    Dim prog
    prog = (Primitive036.X - P036_StartX) / P036_Distance
    If prog >= P036_SPIN_START Then
        ' Begin spin after 25% of the path; spin around Z (top-down spin)
        Primitive036.RotZ = Primitive036.RotZ + P036_SPIN_DEG
    End If

    ' End reached? Snap back and stop
    If Primitive036.X >= P036_EndX Then
        Primitive036.X    = P036_StartX
        Primitive036.RotZ = P036_StartRotZ
        Stop_Primitive036_Run
    End If
End Sub


'========================================
' Random launch timer for Primitive036
' Fires Start_Primitive036_Run randomly
' every 60–120 seconds
'========================================
Sub Init_Primitive036_Scheduler()
    ' kick off the first schedule
    Schedule_Primitive036
End Sub

Sub Schedule_Primitive036()
    Dim delay
    delay = 60000 + Rnd * 60000   ' 60000 ms = 1 min → 120000 ms = 2 min

    Primitive036_Scheduler.Interval = delay
    Primitive036_Scheduler.Enabled  = True
End Sub

Sub Primitive036_Scheduler_Timer()
    Primitive036_Scheduler.Enabled = False

    ' trigger the primitive (only if it’s not already running)
    Start_Primitive036_Run

    ' schedule the next random run
    Schedule_Primitive036
End Sub

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
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
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
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
			End If
		Next
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
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
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

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
'Sub RDampen_Timer
'	Cor.Update
'End Sub

Sub CorTimer_Timer()
	Cor.Update
End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS
	
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
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

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




'**************************************************
'        Flipper Collision Subs
'NOTE: COpy and overwrite collision sound from original collision subs over
'RandomSoundFlipper()' below
'**************************************************'

Sub LeftFlipper_Collide(parm)
    CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
    RandomSoundFlipper() 'Remove this line if Fleep is integrated
    'LeftFlipperCollide parm   'This is the Fleep code
End Sub

Sub RightFlipper_Collide(parm)
    CheckLiveCatch Activeball, RightFlipper, RFCount, parm
    RandomSoundFlipper() 'Remove this line if Fleep is integrated
    'RightFlipperCollide parm  'This is the Fleep code
End Sub


Sub RDampen_Timer()
       Cor.Update
End Sub

sub Kicker050_hit
kicker050.kick -90, 11
'playsound "fx_kicker"
end sub

sub Trigger046_hit
    Dim b: Set b = ActiveBall
    LP_UnregisterLowerBall b
    b.DestroyBall
'activeball.destroyball
'Kicker001.createball
end Sub


'========================================
' Primitive037: diagonal move + spin (X & Y)
'========================================
' Requirements:
'   - Primitive named: Primitive037
'   - Timer object named: P037_Timer
'   - You should already have Const TableHeight set (common in many templates).
'     If not, set: Const TableHeight = 2162    ' <- your table height

' ---- Tunables ----
Const P037_TIMER_MS       = 20          ' ms (≈50 fps)
Const P037_SPEED_UPS      = 80         ' units per second of movement
Const P037_SPIN_X_TOTAL   = 540         ' total degrees RotX over the run
Const P037_SPIN_Y_TOTAL   = 720         ' total degrees RotY over the run
Const P037_RUN_FRACTION   = 0.5         ' run distance = this * TableHeight (half PF)

' ---- State ----
Dim P037_Running
Dim P037_StartX, P037_StartY
Dim P037_StartRotX, P037_StartRotY
Dim P037_TargetX, P037_TargetY
Dim P037_TotalDist, P037_Travelled
Dim P037_UX, P037_UY      ' unit direction

' ---------------------------------------
' Public entry point
' ---------------------------------------
Sub Start_Primitive037_Run()
    If P037_Running Then Exit Sub   ' prevent self-interrupt
    ' Capture starting transform
    P037_StartX     = Primitive037.X
    P037_StartY     = Primitive037.Y
    P037_StartRotX  = Primitive037.RotX
    P037_StartRotY  = Primitive037.RotY

    ' Compute target: go down-right half the playfield length along a 45° path
    Dim runDist, d
    runDist = P037_RUN_FRACTION * TableHeight
    d = runDist / Sqr(2)                 ' same delta on X and Y for 45°
    P037_TargetX = P037_StartX + d
    P037_TargetY = P037_StartY + d

    ' Direction unit vector (toward target)
    P037_TotalDist = Distance2D(P037_StartX, P037_StartY, P037_TargetX, P037_TargetY)
    If P037_TotalDist <= 0.01 Then Exit Sub
    P037_UX = (P037_TargetX - P037_StartX) / P037_TotalDist
    P037_UY = (P037_TargetY - P037_StartY) / P037_TotalDist

    ' Reset progress
    P037_Travelled = 0
    P037_Running   = True

    ' Timer on
    P037_Timer.Interval = P037_TIMER_MS
    P037_Timer.Enabled  = True
End Sub

' ---------------------------------------
' Timer tick
' ---------------------------------------
Sub P037_Timer_Timer()
    If Not P037_Running Then
        P037_Timer.Enabled = False
        Exit Sub
    End If

    Dim stepDist, remaining, dt
    dt = P037_TIMER_MS / 1000
    stepDist  = P037_SPEED_UPS * dt
    remaining = P037_TotalDist - P037_Travelled

    If stepDist >= remaining Then
        Primitive037.X = P037_TargetX
        Primitive037.Y = P037_TargetY
        Primitive037.RotX = P037_StartRotX + P037_SPIN_X_TOTAL
        Primitive037.RotY = P037_StartRotY + P037_SPIN_Y_TOTAL

        ' Teleport back to start
        Primitive037.X     = P037_StartX
        Primitive037.Y     = P037_StartY
        Primitive037.RotX  = P037_StartRotX
        Primitive037.RotY  = P037_StartRotY

        P037_Running       = False
        P037_Timer.Enabled = False
        Exit Sub
    End If

    P037_Travelled = P037_Travelled + stepDist
    Primitive037.X = P037_StartX + P037_UX * P037_Travelled
    Primitive037.Y = P037_StartY + P037_UY * P037_Travelled

    Dim t
    t = P037_Travelled / P037_TotalDist
    Primitive037.RotX = P037_StartRotX + (P037_SPIN_X_TOTAL * t)
    Primitive037.RotY = P037_StartRotY + (P037_SPIN_Y_TOTAL * t)
End Sub


' ---------------------------------------
' Optional hard reset (if you ever need it)
' ---------------------------------------
Sub Reset_Primitive037()
    P037_Running       = False
    P037_Timer.Enabled = False
    Primitive037.X     = P037_StartX
    Primitive037.Y     = P037_StartY
    Primitive037.RotX  = P037_StartRotX
    Primitive037.RotY  = P037_StartRotY
End Sub

' ---------------------------------------
' Small helper
' ---------------------------------------
Function Distance2D(x1, y1, x2, y2)
    Distance2D = Sqr((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
End Function


'========================================
' Primitive042: diagonal move top-right → bottom-left + spin (X & Y)
'========================================
' Requirements:
'   - Primitive named: Primitive042
'   - Timer object named: P042_Timer
'   - You should already have Const TableHeight set.
'     If not, add: Const TableHeight = 2162    ' <- adjust to your table height

' ---- Tunables ----
Const P042_TIMER_MS       = 20          ' ms (≈50 fps)
Const P042_SPEED_UPS      = 380         ' units per second of movement
Const P042_SPIN_X_TOTAL   = 540         ' total degrees RotX over the run
Const P042_SPIN_Y_TOTAL   = 720         ' total degrees RotY over the run
Const P042_RUN_FRACTION   = 0.5         ' run distance = this * TableHeight (half PF)

' ---- State ----
Dim P042_Running
Dim P042_StartX, P042_StartY
Dim P042_StartRotX, P042_StartRotY
Dim P042_TargetX, P042_TargetY
Dim P042_TotalDist, P042_Travelled
Dim P042_UX, P042_UY      ' unit direction

' ---------------------------------------
' Public entry point
' ---------------------------------------
Sub Start_Primitive042_Run()
    If P042_Running Then Exit Sub   ' prevent self-interrupt

    ' Capture starting transform
    P042_StartX     = Primitive042.X
    P042_StartY     = Primitive042.Y
    P042_StartRotX  = Primitive042.RotX
    P042_StartRotY  = Primitive042.RotY

    ' Compute target: go down-left half the playfield length along a 45° path
    Dim runDist, d
    runDist = P042_RUN_FRACTION * TableHeight
    d = runDist / Sqr(2)                 ' same delta on X and Y for 45°
    P042_TargetX = P042_StartX - d       ' ← subtract on X for left
    P042_TargetY = P042_StartY + d       ' ↓ add on Y for down

    ' Direction unit vector
    P042_TotalDist = Distance2D(P042_StartX, P042_StartY, P042_TargetX, P042_TargetY)
    If P042_TotalDist <= 0.01 Then Exit Sub
    P042_UX = (P042_TargetX - P042_StartX) / P042_TotalDist
    P042_UY = (P042_TargetY - P042_StartY) / P042_TotalDist

    ' Reset progress
    P042_Travelled = 0
    P042_Running   = True

    ' Timer on
    P042_Timer.Interval = P042_TIMER_MS
    P042_Timer.Enabled  = True
End Sub

' ---------------------------------------
' Timer tick
' ---------------------------------------
Sub P042_Timer_Timer()
    If Not P042_Running Then
        P042_Timer.Enabled = False
        Exit Sub
    End If

    Dim stepDist, remaining, dt
    dt = P042_TIMER_MS / 1000
    stepDist  = P042_SPEED_UPS * dt
    remaining = P042_TotalDist - P042_Travelled

    If stepDist >= remaining Then
        ' Snap to target for final frame
        Primitive042.X = P042_TargetX
        Primitive042.Y = P042_TargetY
        Primitive042.RotX = P042_StartRotX + P042_SPIN_X_TOTAL
        Primitive042.RotY = P042_StartRotY + P042_SPIN_Y_TOTAL

        ' Immediately return to start (teleport back), restore start rotation
        Primitive042.X     = P042_StartX
        Primitive042.Y     = P042_StartY
        Primitive042.RotX  = P042_StartRotX
        Primitive042.RotY  = P042_StartRotY

        ' Stop
        P042_Running       = False
        P042_Timer.Enabled = False
        Exit Sub
    End If

    ' Advance position
    P042_Travelled = P042_Travelled + stepDist
    Primitive042.X = P042_StartX + P042_UX * P042_Travelled
    Primitive042.Y = P042_StartY + P042_UY * P042_Travelled

    ' Apply progressive spin based on progress 0..1
    Dim t
    t = P042_Travelled / P042_TotalDist
    Primitive042.RotX = P042_StartRotX + (P042_SPIN_X_TOTAL * t)
    Primitive042.RotY = P042_StartRotY + (P042_SPIN_Y_TOTAL * t)
End Sub

' ---------------------------------------
' Optional hard reset
' ---------------------------------------
Sub Reset_Primitive042()
    P042_Running       = False
    P042_Timer.Enabled = False
    Primitive042.X     = P042_StartX
    Primitive042.Y     = P042_StartY
    Primitive042.RotX  = P042_StartRotX
    Primitive042.RotY  = P042_StartRotY
End Sub

' ---------------------------------------
' Helper
' ---------------------------------------

'========================================
' Lock lights: in-game vs. attract sequence
' gameon = 1 -> your existing lock logic
' gameon = 0 -> chase sequence (one light on at a time, looping)
'========================================

Dim LockFlashOn: LockFlashOn = False
Dim Flash046, Flash047, Flash048, Flash049
Dim SeqIdx: SeqIdx = 0   ' 0..3 -> 046,047,048,049

' ---- Helpers ----
Sub SetPrimSteady(p, val)   ' val: 0=off, 5=on
    p.BlendDisableLighting = val
End Sub

Sub SetPrimFlash(p)
    If LockFlashOn Then
        p.BlendDisableLighting = 5
    Else
        p.BlendDisableLighting = 0
    End If
End Sub

Sub ApplyAttractCurrent()
    ' Turn all off, then turn on the current step
    SetPrimSteady primitive046, 0
    SetPrimSteady primitive047, 0
    SetPrimSteady primitive048, 0
    SetPrimSteady primitive049, 0

    Select Case SeqIdx
        Case 0: primitive046.BlendDisableLighting = 0.9
        Case 1: primitive047.BlendDisableLighting = 0.9
        Case 2: primitive048.BlendDisableLighting = 0.9
        Case 3: primitive049.BlendDisableLighting = 0.9
    End Select
End Sub

' ---- 1.5s tick ----
Sub LockFlashTimer_Timer()
    If gameon = 1 Then
        ' In-game: keep your flashing behavior
        LockFlashOn = Not LockFlashOn
        If Flash046 Then SetPrimFlash primitive046
        If Flash047 Then SetPrimFlash primitive047
        If Flash048 Then SetPrimFlash primitive048
        If Flash049 Then SetPrimFlash primitive049
    Else
        ' Attract: advance the chase and show the new frame
        SeqIdx = (SeqIdx + 1) Mod 4
        ApplyAttractCurrent
    End If
End Sub

' ---- 0.5s poll ----
Sub LockCheckTimer_Timer()
    If gameon = 0 Then
        ' Attract: keep the current chase frame applied and bail
        ApplyAttractCurrent
        Exit Sub
    End If

    ' In-game: your simplified four-case logic
    Dim has039, has040, has041
    has039 = kicker039Active
    has040 = kicker040Active
    has041 = kicker041Active

    ' Reset flags and default all OFF
    Flash046 = False : Flash047 = False : Flash048 = False : Flash049 = False
    SetPrimSteady primitive046, 0
    SetPrimSteady primitive047, 0
    SetPrimSteady primitive048, 0
    SetPrimSteady primitive049, 0

    ' 1) 041 = True  -> 1 solid, 2 solid, 3 solid, 4 flashing
    ' 2) 040 = True & 041 = False -> 1 solid, 2 solid, 3 flashing, 4 off
    ' 3) 039 = True & 040 = False -> 1 solid, 2 flashing, 3 off, 4 off
    ' 4) none -> 1 flashing, 2 off, 3 off, 4 off

    If has041 Then
        SetPrimSteady primitive046, 5
        SetPrimSteady primitive047, 5
        SetPrimSteady primitive048, 5
        Flash049 = True

    ElseIf has040 And (Not has041) Then
        SetPrimSteady primitive046, 5
        SetPrimSteady primitive047, 5
        Flash048 = True

    ElseIf has039 And (Not has040) Then
        SetPrimSteady primitive046, 5
        Flash047 = True

    Else
        Flash046 = True
    End If

    ' Apply current flash state to those flagged
    If Flash046 Then SetPrimFlash primitive046
    If Flash047 Then SetPrimFlash primitive047
    If Flash048 Then SetPrimFlash primitive048
    If Flash049 Then SetPrimFlash primitive049
End Sub

sub MultiBallRelease_timer
kicker017.enabled=0
if ballsonplayfield=1 Then
kicker039.destroyball
Kicker040.destroyball
Kicker041.destroyball
kicker042.destroyball
kicker042.timerenabled=1 'disabled other Kickers for a while
end if
if ballsonplayfield<6 then
kicker039.enabled=False
kicker040.enabled=False
Kicker041.enabled=False
Kicker042.enabled=False
Trigger026.enabled=0
BallRelease.createball
BallRelease.kick 45,8
ballsonplayfield=ballsonplayfield+1
Kicker049.enabled=1
DOF 123, DOFPulse
 PlayBallReleaseSound
end if
if ballsonplayfield=6 then
ballsonplayfield=5
kicker017.enabled=1
MultiBallRelease.enabled=0
Trigger026.enabled=1
end if
end Sub

sub Kicker049_hit
'timer048.enabled=0 'turn of rotating kit lights by target
multiballkick.enabled=1 'starts shiooting balls out
end sub

sub MultiballKick_timer
        
        Select Case Int(Rnd * 3) + 1
            Case 1: Kicker049.Kick 0, 55
            Case 2: Kicker049.Kick 0, 53
            Case 3: Kicker049.Kick 0, 54
        End Select

If Rnd < 0.5 Then
    PlaySound "fx_kicker"
Else
    PlaySound "fx_kicker"
End If
Kicker049.enabled=0
multiballkick.enabled=0
end sub


Sub rightInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	If activeball.vely < 0 Then Exit Sub 							'don't affect upwards movement

    activeball.AngMomZ = abs(activeball.AngMomZ) * RndNum(2,4)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

	If abs(activeball.vely) > 5 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 10 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 15 Then activeball.vely = 0.8 * activeball.vely
    If activeball.vely > 16 Then activeball.vely = RndNum(14,16)
    If activeball.vely < -16 Then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

Sub leftInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	If activeball.vely < 0 Then Exit Sub 							'don't affect upwards movement
    activeball.AngMomZ = -abs(activeball.AngMomZ) * RndNum(3,6)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

    If abs(activeball.vely) > 5 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 10 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 15 Then activeball.vely = 0.8 * activeball.vely
    If activeball.vely > 16 Then activeball.vely = RndNum(14,16)
    If activeball.vely < -16 Then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub

' Ball–ball collision sound
Sub OnBallBallCollision(ball1, ball2, velocity)
FlipperCradleCollision ball1, ball2, velocity
    ' ignore very soft taps
    If velocity < 1.2 Then Exit Sub

    ' loudness from impact speed (clamped 0..1)
    Dim vol: vol = CSng((velocity ^ 2) / 40)
    If vol > 1 Then vol = 1

    ' pitch nudged by speed a bit
    Dim pitch: pitch = 0.2 * velocity

    ' play at ball1’s position (good enough for stereo panning)
    ' If you prefer centerpoint panning, swap in whichever pan/fade helper you use.
    PlaySound "fx_collide", 0, vol, AudioPan(ball1), 0, pitch, 0, 1, AudioFade(ball1)
End Sub

'===============================
' LIGHTSABER ROTATION (±45°) — only Primitive002
'===============================

' --- Tuning ---
Const SABER_INTERVAL_MS   = 10         ' timer tick (ms) ~66 fps
Const SABER_DEG_PER_SEC   = 500        ' rotation speed
Const SABER_LIMIT_DEG     = 45         ' clamp at ±45

' --- State ---
Dim SaberCurDeg, SaberTargetDeg, SaberStepDeg
Dim SaberBase002

Sub InitSaber()
    ' Bind only Primitive002
    SaberBase002 = Primitive002.RotZ

    SaberCurDeg    = 0
    SaberTargetDeg = 0
    SaberStepDeg   = SABER_DEG_PER_SEC * SABER_INTERVAL_MS / 1000

    SaberTimer.Interval = SABER_INTERVAL_MS
    SaberTimer.Enabled  = False
End Sub

' --- Controls ---
Sub SaberRotateCW()   ' Right flipper → +45°
    If SaberMode = False Then Exit Sub
    SaberTargetDeg = SABER_LIMIT_DEG
    SaberTimer.Enabled = True
End Sub

Sub SaberRotateCCW()  ' Left flipper → -45°
    If SaberMode = False Then Exit Sub
    SaberTargetDeg = -SABER_LIMIT_DEG
    SaberTimer.Enabled = True
End Sub

' --- Timer ---
Sub SaberTimer_Timer()
    Dim diff, stepDeg

    diff = SaberTargetDeg - SaberCurDeg
    If Abs(diff) <= 0.01 Then
        SaberCurDeg = SaberTargetDeg
        Call SaberApplyAngle(SaberCurDeg)
        SaberTimer.Enabled = False
        Exit Sub
    End If

    stepDeg = Sgn(diff) * SaberStepDeg
    If Abs(stepDeg) > Abs(diff) Then stepDeg = diff

    SaberCurDeg = SaberCurDeg + stepDeg

    ' Clamp
    If SaberCurDeg >  SABER_LIMIT_DEG Then SaberCurDeg =  SABER_LIMIT_DEG
    If SaberCurDeg < -SABER_LIMIT_DEG Then SaberCurDeg = -SABER_LIMIT_DEG

    Call SaberApplyAngle(SaberCurDeg)
End Sub

' --- Apply rotation only to Primitive002 ---
Sub SaberApplyAngle(deg)
    Primitive002.RotZ = SaberBase002 + deg
End Sub


sub startsaber
if saberenabled=1 Then
sabermode=true
playsound "saberon"
playsound "saberlooping", -1
primitive002.visible=1
saberoff_timer.enabled=1
end If
end Sub

sub stopsaber
if sabermode=true then
sabermode=False
playsound "saberoff"
stopsound "saberlooping"
primitive002.visible=0
end If
end Sub

sub saberoff_timer_timer
stopsaber
saberoff_timer.enabled=0
end Sub

Sub PlayRandomSaber()
    Dim idx
    idx = Int(Rnd * 7) + 1   ' gives 1–7
    PlaySound "saber" & idx
End Sub

sub delaymusic_timer
StartGameMusic
delaymusic.enabled=0
end Sub


'========================================
' Stormtrooper Head (Primitive011) — Smooth Spin
'========================================

Const ST11_INTERVAL_MS = 10   ' ~100 Hz for very smooth motion

Dim ST11_Running
Dim ST11_TotalDeg, ST11_DoneDeg, ST11_StepDeg

Sub ST11_InitSpin()
    ST11_Running   = False
    ST11_TotalDeg  = 0
    ST11_DoneDeg   = 0
    ST11_StepDeg   = 0
    ST11_SpinTimer.Interval = ST11_INTERVAL_MS
    ST11_SpinTimer.Enabled  = False
End Sub

' Start a new spin if not already spinning
' totalDeg   : rotation amount (e.g., 360, 720)
' degPerSec  : angular speed (e.g., 540)
' isClockwise: True = CW, False = CCW

' 360° at 540°/s, clockwise
'ST11_StartSpin 360, 540, True

' 720° at 360°/s, counter-clockwise
'ST11_StartSpin 720, 360, False

Sub ST11_StartSpin(ByVal totalDeg, ByVal degPerSec, ByVal isClockwise)
    If ST11_Running Then Exit Sub
    If degPerSec <= 0 Then degPerSec = 360

    ST11_TotalDeg = Abs(CDbl(totalDeg))
    ST11_DoneDeg  = 0

    Dim dir
    If isClockwise Then
        dir = 1
    Else
        dir = -1
    End If

    ' FIX: use 1000.0 instead of 1000#
    ST11_StepDeg = dir * (CDbl(degPerSec) * (CDbl(ST11_INTERVAL_MS) / 1000.0))

    ST11_Running = True
    ST11_SpinTimer.Enabled = True
End Sub

Sub ST11_SpinTimer_Timer()
    If Not ST11_Running Then
        ST11_SpinTimer.Enabled = False
        Exit Sub
    End If

    Dim remaining, stepMag, stepThisTick
    remaining  = ST11_TotalDeg - ST11_DoneDeg
    stepMag    = Abs(ST11_StepDeg)

    If stepMag > remaining Then
        stepThisTick = Sgn(ST11_StepDeg) * remaining
    Else
        stepThisTick = ST11_StepDeg
    End If

    Primitive011.RotY = Primitive011.RotY + stepThisTick
    ST11_DoneDeg = ST11_DoneDeg + Abs(stepThisTick)

    If ST11_DoneDeg >= ST11_TotalDeg - 0.0001 Then
        ST11_Running = False
        ST11_SpinTimer.Enabled = False
    End If
End Sub

Function ST11_IsSpinning()
    ST11_IsSpinning = ST11_Running
End Function


'========================================
' Stormtrooper Head 2 (Primitive071) — Smooth Spin
'========================================

Const ST71_INTERVAL_MS = 10   ' ~100 Hz for very smooth motion

Dim ST71_Running
Dim ST71_TotalDeg, ST71_DoneDeg, ST71_StepDeg

Sub ST71_InitSpin()
    ST71_Running   = False
    ST71_TotalDeg  = 0
    ST71_DoneDeg   = 0
    ST71_StepDeg   = 0
    ST71_SpinTimer.Interval = ST71_INTERVAL_MS
    ST71_SpinTimer.Enabled  = False
End Sub

' Start a new spin if not already spinning
' totalDeg   : rotation amount (e.g., 360, 720)
' degPerSec  : angular speed (e.g., 540)
' isClockwise: True = CW, False = CCW
Sub ST71_StartSpin(ByVal totalDeg, ByVal degPerSec, ByVal isClockwise)
    If ST71_Running Then Exit Sub
    If degPerSec <= 0 Then degPerSec = 360

    ST71_TotalDeg = Abs(CDbl(totalDeg))
    ST71_DoneDeg  = 0

    Dim dir
    If isClockwise Then
        dir = 1
    Else
        dir = -1
    End If

    ST71_StepDeg = dir * (CDbl(degPerSec) * (CDbl(ST71_INTERVAL_MS) / 1000.0))

    ST71_Running = True
    ST71_SpinTimer.Enabled = True
End Sub

Sub ST71_SpinTimer_Timer()
    If Not ST71_Running Then
        ST71_SpinTimer.Enabled = False
        Exit Sub
    End If

    Dim remaining, stepMag, stepThisTick
    remaining  = ST71_TotalDeg - ST71_DoneDeg
    stepMag    = Abs(ST71_StepDeg)

    If stepMag > remaining Then
        stepThisTick = Sgn(ST71_StepDeg) * remaining
    Else
        stepThisTick = ST71_StepDeg
    End If

    Primitive071.RotZ = Primitive071.RotZ + stepThisTick
    ST71_DoneDeg = ST71_DoneDeg + Abs(stepThisTick)

    If ST71_DoneDeg >= ST71_TotalDeg - 0.0001 Then
        ST71_Running = False
        ST71_SpinTimer.Enabled = False
    End If
End Sub

Function ST71_IsSpinning()
    ST71_IsSpinning = ST71_Running
End Function


'=========================================
' Primitive020: move → pause on touch → resume faster
'=========================================
Const P020_INTERVAL   = 20    ' ms (≈50 fps)
Const P020_SPEED      = 2     ' base units per tick
Const P020_HIT_RADIUS = 50    ' proximity trigger size
Const P020_PAUSE_MS   = 1000  ' pause length in ms

Dim P020_StartX, P020_StartY
Dim P020_TargetX, P020_TargetY
Dim P020_IsRunning, P020_CurSpeed, P020_IsPaused

Sub InitP020()
    ' start / end points
    P020_StartX = 885
    P020_StartY = 1085
    P020_TargetX = 229
    P020_TargetY = 719

    ' state
    P020_CurSpeed = P020_SPEED
    P020_IsRunning = False
    P020_IsPaused = False
End Sub

Sub Start_P020()
    If P020_IsRunning Then Exit Sub
playsound "mousehappy"
Primitive020.visible=True
    P020_IsRunning = True
    P020_CurSpeed  = P020_SPEED   ' reset to base speed
    Primitive020.X = P020_StartX
    Primitive020.Y = P020_StartY
    Primitive020_Timer.Interval = P020_INTERVAL
    Primitive020_Timer.Enabled  = True
End Sub

Sub Primitive020_Timer_Timer()
    If Not P020_IsRunning Then Exit Sub
    If P020_IsPaused Then Exit Sub

    ' --- detect proximity to any live ball
    Dim b, dx, dy
    For Each b In GetBalls()
        dx = b.X - Primitive020.X
        dy = b.Y - Primitive020.Y
        If (dx*dx + dy*dy) <= (P020_HIT_RADIUS^2) Then
            Call P020_DoCollision()
            Exit For
        End If
    Next

    ' --- move toward target
    dx = P020_TargetX - Primitive020.X
    dy = P020_TargetY - Primitive020.Y
    Dim dist, step
    dist = Sqr(dx*dx + dy*dy)

    If dist < P020_CurSpeed Then
Primitive020.visible=False
        ' reached target
        Primitive020.X = P020_TargetX
        Primitive020.Y = P020_TargetY
        P020_IsRunning = False
        Primitive020_Timer.Enabled = False
    Else
        step = P020_CurSpeed / dist
        Primitive020.X = Primitive020.X + dx * step
        Primitive020.Y = Primitive020.Y + dy * step
    End If
End Sub

'=========================================
' Collision handler (reaction only)
'=========================================
Sub P020_DoCollision()
    If Not P020_IsRunning Then Exit Sub
    P020_IsPaused = True

    ' --- EFFECT HOOKS ---
    stopsound "mousehappy"
    PlaySound "mousehit"        ' replace with your own sound name
UMD_OnMouseHit
    'Flasher020.Visible = 1    ' show flasher primitive or VP light (optional)
    'Flasher020.State = 1
    ' -------------------

    P020_TimerPause.Interval = P020_PAUSE_MS
    P020_TimerPause.Enabled  = True
End Sub

Sub P020_TimerPause_Timer()
    P020_CurSpeed = P020_SPEED * 3   ' resume at 2× speed
    P020_IsPaused = False
    P020_TimerPause.Enabled = False

    ' --- turn off effect after pause ---
    'Flasher020.Visible = 0
    'Flasher020.State = 0
End Sub


'===============================
' Primitive008 — Lightsaber Flicker → Solid ON
' Uses BlendDisableLighting like Jabba's eyes
'===============================

' --- Tuning ---
Const P008_TIMER_MS   = 20      ' timer tick (~50 fps)
Const P008_BDL_OFF    = 0       ' normal lighting (looks OFF)
Const P008_BDL_ON     = 2       ' fully emissive (looks ON)
Const P008_OPA_OFF    = 0       ' invisible/off
Const P008_OPA_ON     = 100     ' fully visible

'--------------------------------
' Init: call this in Table_Init
'--------------------------------
' --- State ---
Dim P008_Mode, P008_StepIdx, P008_TicksLeft
Dim P008_Steps          ' Variant that will hold an Array(...)
Dim P008_Durations      ' Variant that will hold an Array(...)

Sub P008_Init()
    P008_Mode    = 0
    P008_StepIdx = 0
    P008_TicksLeft = 0
    ' Start OFF
    Primitive008.BlendDisableLighting = P008_BDL_OFF
    Primitive008.Opacity = P008_OPA_OFF

    ' Prepare timer
    P008_Timer.Interval = P008_TIMER_MS
    P008_Timer.Enabled  = False
End Sub

'--------------------------------
' Start the flicker → ON sequence
' (ignores duplicate calls while active or already on)
'--------------------------------
Sub P008_StartFlicker()
    If P008_Mode <> 0 Then Exit Sub   ' only from fully OFF

    P008_Steps = Array(1,0,1,0,1,1)
    Dim ms, dur(), i, t
    ms = Array(90,120,70,130,60,600)

    ReDim dur(UBound(ms))
    For i = 0 To UBound(ms)
        t = CInt(ms(i) \ P008_TIMER_MS)  ' use integer ticks
        If t < 1 Then t = 1
        dur(i) = t
    Next
    P008_Durations = dur

    P008_Mode      = 1
    P008_StepIdx   = 0
    P008_TicksLeft = P008_Durations(0)

    P008_ApplyState CInt(P008_Steps(0))
    P008_Timer.Enabled = True
End Sub

'--------------------------------
' Optional hard OFF (kills timer)
'--------------------------------
Sub P008_ForceOff()
    P008_Mode = 0
    P008_Timer.Enabled = False
    Primitive008.BlendDisableLighting = P008_BDL_OFF
    Primitive008.Opacity = P008_OPA_OFF
End Sub

'--------------------------------
' Optional force SOLID ON (skips flicker)
'--------------------------------
Sub P008_ForceOn()
    P008_Mode = 2
    P008_Timer.Enabled = False
    Primitive008.BlendDisableLighting = P008_BDL_ON
    Primitive008.Opacity = P008_OPA_ON
End Sub

'--------------------------------
' Timer handler
'--------------------------------
Sub P008_Timer_Timer()
    If P008_Mode = 1 Then
        P008_TicksLeft = P008_TicksLeft - 1
        If P008_TicksLeft <= 0 Then
            P008_StepIdx = P008_StepIdx + 1
            If P008_StepIdx > UBound(P008_Steps) Then
                P008_Mode = 2
                P008_ApplyState 1
                P008_Timer.Enabled = False
            Else
                P008_ApplyState CInt(P008_Steps(P008_StepIdx))
                P008_TicksLeft = P008_Durations(P008_StepIdx)
            End If
        End If
    Else
        P008_Timer.Enabled = False
    End If
End Sub

'--------------------------------
' Apply a single state (0=OFF, 1=ON)
' Adds tiny random “buzz” while ON during flicker for realism
'--------------------------------
Sub P008_ApplyState(isOn)
    If isOn = 0 Then
        Primitive008.BlendDisableLighting = P008_BDL_OFF
        Primitive008.Opacity = P008_OPA_OFF
    Else
        ' During flicker mode, add a touch of jitter; when SOLID, pin to max.
        If P008_Mode = 1 Then
            ' small brightness jitter
            Dim op : op = P008_OPA_ON - Int(Rnd * 8) ' 100..92
            Primitive008.Opacity = op
            ' slight BDL wobble between 4 and 5
            Primitive008.BlendDisableLighting = 4 + Int(Rnd * 2)
        Else
            Primitive008.BlendDisableLighting = P008_BDL_ON
            Primitive008.Opacity = P008_OPA_ON
        End If
    End If
End Sub

sub DelaySaberLight_timer
StartFadein
P008_StartFlicker
DelaySaberLight.enabled=0
end Sub

Function OneIn(N)
    OneIn = (Int(Rnd * N) = 0)
End Function

sub trigger005_hit
GIx_BigRampEffect
    If OneIn(15) Then
        PlaySound "cave"
    End If
addscore 1000
end sub



sub closemulti
   if ultramode=1 then
   DMD_DisplaySceneTextWithPause "hit a ramp!", p1score, 10000
   qtimer.interval=1500 
   end if
LiPass003.state=2
LiPass004.state=2
        primitive023.Visible = 1
        Ramp049.Collidable = 1
end Sub

'closemulti

sub Gate003_hit
playsound "gate4"
end sub

sub asteroidTimer_timer
If Rnd < 0.5 Then
   Start_Primitive037_Run
Else
    Start_Primitive042_Run
End If
end Sub

sub delayfadeout_timer
StartFadeOut
delayfadeout.enabled=0
end sub

'=========================================
' LOWER PLAYFIELD MINI-BALLS + PROXIMITY
' (VBScript-safe: no "Continue For")
'=========================================

' --- Angle cycle state ---
Dim MB_Idx, MB_Dir
MB_Idx = 0
MB_Dir = 1

' --- Proximity state ---
Dim LP_Prims(), LP_LastKnockTick, LP_Tick, LP_R2
Dim LP_LowerBallIDs     ' Dictionary of tracked ball IDs


Const LP_PROX_RADIUS    = 25     ' XY distance threshold
Const LP_TIMER_MS       = 20     ' ~50 fps
Const LP_COOLDOWN_MS    = 80    ' ms between knocks

'-------------------------------------------
' INIT (call in Table_Init)
'-------------------------------------------
Sub LP_Prox_Init()
    ' Bind primitives to watch
    ReDim LP_Prims(3)
    Set LP_Prims(0) = Primitive037
    Set LP_Prims(1) = Primitive042
    Set LP_Prims(2) = Primitive033
    Set LP_Prims(3) = Primitive036

    Set LP_LowerBallIDs = CreateObject("Scripting.Dictionary")

    LP_Tick = 0
    LP_LastKnockTick = -999999
    LP_R2 = LP_PROX_RADIUS * LP_PROX_RADIUS

    LP_ProxTimer.Interval = LP_TIMER_MS
    LP_ProxTimer.Enabled  = False
End Sub

'-------------------------------------------
' Register a new lower-playfield ball
'-------------------------------------------

Sub LP_EnsureDict()
    If Not IsObject(LP_LowerBallIDs) Then
        Set LP_LowerBallIDs = CreateObject("Scripting.Dictionary")
    End If
End Sub


' ==== REGISTER / UNREGISTER ====
Sub LP_RegisterLowerBall(b)
    LP_EnsureDict
    LP_LowerBallIDs(CStr(b.ID)) = True
    LP_ProxTimer.Enabled = True
End Sub

Sub LP_UnregisterLowerBall(b)
    If IsObject(LP_LowerBallIDs) Then
        Dim k: k = CStr(b.ID)
        If LP_LowerBallIDs.Exists(k) Then LP_LowerBallIDs.Remove k
        If LP_LowerBallIDs.Count = 0 Then LP_ProxTimer.Enabled = False
    End If
End Sub



'-------------------------------------------
' Angle cycle helper
'-------------------------------------------
Function MiniBall_NextAngle()
    Const steps = 7
    Const amin  = -32
    Const amax  =  32

    MiniBall_NextAngle = amin + (MB_Idx * ((amax - amin) / (steps - 1)))

    MB_Idx = MB_Idx + MB_Dir
    If MB_Idx >= steps - 1 Or MB_Idx <= 0 Then
        MB_Dir = -MB_Dir
    End If
End Function

'-------------------------------------------
' Spawn a mini-ball from a kicker
'-------------------------------------------
Sub SpawnMiniBallAt(k, launchSpeed)
playsound "falconshot2"
    Dim b, angle
    Set b = k.CreateBall()

    ' half size
    b.Radius = b.Radius * 0.5
    b.Mass   = b.Mass * 0.125

    LP_RegisterLowerBall b   ' mark as lower playfield ball

    angle = MiniBall_NextAngle()
    k.Kick angle, launchSpeed
End Sub

'-------------------------------------------
' Proximity timer (checks only registered balls)
'-------------------------------------------
                   

' ==== TIMER ====
Sub LP_ProxTimer_Timer()
    If Not IsObject(LP_LowerBallIDs) Then Exit Sub
    If LP_LowerBallIDs.Count = 0 Then Exit Sub

    LP_Tick = LP_Tick + LP_TIMER_MS

    Dim gBOT, i, b, idstr, j, p, dx, dy, d2
    gBOT = GetBalls()
    If Not IsArray(gBOT) Then Exit Sub

    For i = 0 To UBound(gBOT)
        Set b = gBOT(i)
        idstr = CStr(b.ID)
        If LP_LowerBallIDs.Exists(idstr) Then
            For j = 0 To UBound(LP_Prims)
                Set p = LP_Prims(j)
                dx = b.X - p.X
                dy = b.Y - p.Y
                d2 = dx*dx + dy*dy
                If d2 <= LP_R2 Then
                    If LP_Tick - LP_LastKnockTick >= LP_COOLDOWN_MS Then
                        addscore 10
                        PlaySound "igothim"
                        flasher015.Visible = 1
                        ' re-arm timer so it always hides after a short burst
                        hideblast.Enabled = False
                        hideblast.Interval = 120
                        hideblast.Enabled = True
                        LP_LastKnockTick = LP_Tick
                    End If
                    Exit For    ' only one trigger per ball per tick
                End If
            Next
        End If
    Next
End Sub


sub hideblast_timer
flasher015.Visible = 0
hideblast.enabled=0
end Sub

'==============================
' Primitive004 Hit Reaction
'==============================

Dim P071_Step, P071_Ticks
dim starlight
starlight=0

Sub WALLstarhit_Hit()
starlight=starlight+1
if starlight=1 Then
dstar1.state=1
end If
if starlight=2 Then
dstar001.state=1
end If
if starlight=3 Then
dstar002.state=1
end If
if starlight=4 Then
dstar003.state=1
end If
playsound "deathstar1"
pupevent 877
UMD_OnDeathStarHit
    ' start shake + brighten
    P071_Step = 0
    P071_Ticks = 50   ' ~1 sec at 20ms interval
    Primitive071.BlendDisableLighting = 3   ' brighter
    P071_ShakeTimer.Interval = 20
    P071_ShakeTimer.Enabled = True
End Sub

Sub P071_ShakeTimer_Timer()
    If P071_Ticks > 0 Then
        P071_Ticks = P071_Ticks - 1
        P071_Step = P071_Step + 1

        ' Simple shake: alternate small offsets left/right
        If (P071_Step Mod 2) = 0 Then
            Primitive071.TransX = Primitive071.TransX + 8
        Else
            Primitive071.TransX = Primitive071.TransX - 8
        End If
    Else
        ' Reset to normal
        P071_ShakeTimer.Enabled = False
        Primitive071.TransX = 0
        Primitive071.TransY = 0
        Primitive071.BlendDisableLighting = 0
    End If
End Sub

sub TieLightOn
Light001.state=2
Trigger006.enabled=1
end Sub

dim tiehitlight
tiehitlight=0

sub trigger006_hit
tiehitlight=tiehitlight+1
if tiehitlight=1 Then
tlight.state=1
end If
if tiehitlight=2 Then
tlight001.state=1
end If
if tiehitlight=3 Then
tlight002.state=1
end If
if tiehitlight=4 Then
tlight003.state=1
end If
pupevent 870
UMD_OnTieHit
GIx_Chase RGB(0, 255, 80), 1, 800
Primitive083.BlendDisableLighting = 10 
playsound "falconshot"
addscore 1000
lighttie.enabled=1
end Sub

sub lighttie_timer
Primitive083.BlendDisableLighting = 0
lighttie.enabled=1
end sub

'========================================
' GIx — GI Effects Kit (Editor timers only; VBScript-safe)
' Requires Timers in editor named:
'   GIxPulseTimer, GIxStrobeTimer, GIxSparkleTimer, GIxChaseTimer, GIxWaveTimer, GIxDimTimer
' Set all editor timers Enabled=False; Interval any (script sets them).
' Works with collection: GI_Lights
'========================================

' ---- STATE ----
Dim GIx_Lights(), GIx_BaseColor(), GIx_BaseState()
Dim GIx_Count

' Pulse
Dim GIx_PulseTicks
Dim GIx_PulseHoldTicks
Dim GIx_PulseColor

' Strobe
Dim GIx_StrobeTicks
Dim GIx_StrobeTotal
Dim GIx_StrobeOn
Dim GIx_StrobeColor

' Sparkle
Dim GIx_SparkleTicks
Dim GIx_SparkleTotal

' Chase
Dim GIx_ChaseIdx
Dim GIx_ChaseDir
Dim GIx_ChaseTicks

' Wave
Dim GIx_WavePhase
Dim GIx_WaveTicks

' Dim / Fade
Dim GIx_DimLevel
Dim GIx_DimHoldTicks
Dim GIx_DimTicks
Dim GIx_FadeA
Dim GIx_FadeB
Dim GIx_FadeSteps
Dim GIx_FadeStep
Dim GIx_FadeReturn

'----------------------------------------
' Call this once in Table_Init
'----------------------------------------
Sub GIx_Init()
    Dim l
    Dim i
    ReDim GIx_Lights(GI_Lights.Count - 1)
    ReDim GIx_BaseColor(GI_Lights.Count - 1)
    ReDim GIx_BaseState(GI_Lights.Count - 1)

    i = 0
    For Each l In GI_Lights
        Set GIx_Lights(i) = l
        GIx_BaseColor(i)  = l.Color
        GIx_BaseState(i)  = l.State
        i = i + 1
    Next
    GIx_Count = i
End Sub

' ---- Helpers ----
Private Sub GIx_AllState(s)
    Dim i
    For i = 0 To GIx_Count - 1
        GIx_Lights(i).State = s   ' 0=OFF, 1=ON, 2=BLINK
    Next
End Sub

Private Sub GIx_AllColor(c)
    Dim i
    For i = 0 To GIx_Count - 1
        GIx_Lights(i).Color = c
    Next
End Sub

Private Sub GIx_RestoreAll()
    Dim i
    For i = 0 To GIx_Count - 1
        GIx_Lights(i).Color = GIx_BaseColor(i)
        GIx_Lights(i).State = GIx_BaseState(i)
    Next
End Sub

Private Sub GIx_SetDim(scale01)
    Dim i
    If scale01 < 0 Then scale01 = 0
    If scale01 > 1 Then scale01 = 1
    On Error Resume Next
    For i = 0 To GIx_Count - 1
        GIx_Lights(i).IntensityScale = CSng(scale01) ' 0..1
    Next
    On Error GoTo 0
End Sub

Private Function Lerp(a, b, t)
    Lerp = a + (b - a) * t
End Function

Private Function Max(a, b)
    If a > b Then
        Max = a
    Else
        Max = b
    End If
End Function

Private Function Sgn(v)
    If v < 0 Then
        Sgn = -1
    ElseIf v > 0 Then
        Sgn = 1
    Else
        Sgn = 0
    End If
End Function

'========================================
' EFFECT 1 — Quick Pulse, then restore
' Example: GIx_Pulse RGB(0,255,180), 220
'========================================
Sub GIx_Pulse(colorRGB, holdMs)
    Dim i
    GIx_PulseColor     = colorRGB
    GIx_PulseHoldTicks = CInt(holdMs / 20)
    GIx_PulseTicks     = 0

    For i = 0 To GIx_Count - 1
        GIx_Lights(i).Color = colorRGB
        GIx_Lights(i).State = 1
    Next

    GIxPulseTimer.Interval = 20
    GIxPulseTimer.Enabled = True
End Sub

Sub GIxPulseTimer_Timer()
    GIx_PulseTicks = GIx_PulseTicks + 1
    If GIx_PulseTicks >= GIx_PulseHoldTicks Then
        GIxPulseTimer.Enabled = False
        GIx_RestoreAll
    End If
End Sub

'========================================
' EFFECT 2 — Strobe burst (fast on/off)
' Example: GIx_Strobe RGB(255,60,60), 12, 600
'========================================
Sub GIx_Strobe(colorRGB, rateHz, durationMs)
    Dim i
    GIx_StrobeColor = colorRGB
    GIx_StrobeOn    = False
    GIx_StrobeTotal = CInt(durationMs / 20)

    For i = 0 To GIx_Count - 1
        GIx_Lights(i).Color = colorRGB
    Next

    If rateHz < 1 Then
        rateHz = 1
    End If

    GIxStrobeTimer.Interval = CInt(1000 / (rateHz * 2)) ' toggle period
    GIx_StrobeTicks = 0
    GIxStrobeTimer.Enabled = True
End Sub

Sub GIxStrobeTimer_Timer()
    Dim i
    GIx_StrobeTicks = GIx_StrobeTicks + 1
    GIx_StrobeOn = Not GIx_StrobeOn

    For i = 0 To GIx_Count - 1
        If GIx_StrobeOn Then
            GIx_Lights(i).State = 1
        Else
            GIx_Lights(i).State = 0
        End If
    Next

    If GIx_StrobeTicks >= GIx_StrobeTotal Then
        GIxStrobeTimer.Enabled = False
        GIx_RestoreAll
    End If
End Sub

'========================================
' EFFECT 3 — Random Sparkle (scattered blinks)
' Example: GIx_Sparkle RGB(120,200,255), 600
'========================================
Sub GIx_Sparkle(colorRGB, durationMs)
    GIx_SparkleTotal = CInt(durationMs / 50)
    GIx_SparkleTicks = 0
    GIx_AllColor colorRGB
    GIxSparkleTimer.Interval = 50
    GIxSparkleTimer.Enabled = True
End Sub

Sub GIxSparkleTimer_Timer()
    Dim i
    Dim idx
    For i = 1 To 3
        idx = Int(Rnd * GIx_Count)
        GIx_Lights(idx).State = 1
        idx = Int(Rnd * GIx_Count)
        GIx_Lights(idx).State = 0
    Next

    GIx_SparkleTicks = GIx_SparkleTicks + 1
    If GIx_SparkleTicks >= GIx_SparkleTotal Then
        GIxSparkleTimer.Enabled = False
        GIx_RestoreAll
    End If
End Sub

'========================================
' EFFECT 4 — Chase (one light runs the strip)
' Example: GIx_Chase RGB(0,255,80), +1, 800
'========================================
Sub GIx_Chase(colorRGB, dir, durationMs)
    Dim i
    If dir = 0 Then
        GIx_ChaseDir = 1
    Else
        GIx_ChaseDir = Sgn(dir)
    End If

    GIx_ChaseIdx   = 0
    GIx_ChaseTicks = CInt(durationMs / 60)

    GIx_AllColor colorRGB
    For i = 0 To GIx_Count - 1
        GIx_Lights(i).State = 0
    Next

    GIxChaseTimer.Interval = 60
    GIxChaseTimer.Enabled = True
End Sub

Sub GIxChaseTimer_Timer()
    Dim i
    For i = 0 To GIx_Count - 1
        GIx_Lights(i).State = 0
    Next

    GIx_Lights(GIx_ChaseIdx).State = 1
    GIx_Lights((GIx_ChaseIdx - GIx_ChaseDir + GIx_Count) Mod GIx_Count).State = 2

    GIx_ChaseIdx = (GIx_ChaseIdx + GIx_ChaseDir + GIx_Count) Mod GIx_Count

    GIx_ChaseTicks = GIx_ChaseTicks - 1
    If GIx_ChaseTicks <= 0 Then
        GIxChaseTimer.Enabled = False
        GIx_RestoreAll
    End If
End Sub

'========================================
' EFFECT 5 — 3-Phase Wave (A-B-C groups)
' Example: GIx_Wave RGB(255,180,40), 1200
'========================================
Sub GIx_Wave(colorRGB, durationMs)
    GIx_WavePhase = 0
    GIx_AllColor colorRGB
    GIx_WaveTicks = CInt(durationMs / 60)
    GIxWaveTimer.Interval = 60
    GIxWaveTimer.Enabled = True
End Sub

Sub GIxWaveTimer_Timer()
    Dim i
    Dim g

    For i = 0 To GIx_Count - 1
        g = i Mod 3
        If g = GIx_WavePhase Then
            GIx_Lights(i).State = 1
        ElseIf g = ((GIx_WavePhase + 1) Mod 3) Then
            GIx_Lights(i).State = 2
        Else
            GIx_Lights(i).State = 0
        End If
    Next

    GIx_WavePhase = (GIx_WavePhase + 1) Mod 3
    GIx_WaveTicks = GIx_WaveTicks - 1
    If GIx_WaveTicks <= 0 Then
        GIxWaveTimer.Enabled = False
        GIx_RestoreAll
    End If
End Sub

'========================================
' EFFECT 6 — Dim and auto-restore (non-blocking)
' Example: GIx_DimHit 0.35, 300
'========================================
Sub GIx_DimHit(level01, ms)
    If level01 < 0 Then level01 = 0
    If level01 > 1 Then level01 = 1

    GIx_DimLevel     = CSng(level01)
    GIx_DimHoldTicks = CInt(ms / 20)
    GIx_DimTicks     = 0

    GIx_SetDim GIx_DimLevel

    GIxDimTimer.Interval = 20
    GIxDimTimer.Enabled = True
End Sub

' OPTIONAL — Smooth fade to level and back
' Example: GIx_DimFade 1, 0.3, 250
Sub GIx_DimFade(fromLevel, toLevel, ms)
    If fromLevel < 0 Then fromLevel = 0
    If fromLevel > 1 Then fromLevel = 1
    If toLevel   < 0 Then toLevel   = 0
    If toLevel   > 1 Then toLevel   = 1

    GIx_FadeA = CSng(fromLevel)
    GIx_FadeB = CSng(toLevel)
    GIx_FadeSteps = Max(1, CInt(ms / 20))
    GIx_FadeStep  = 0
    GIx_FadeReturn = False

    GIx_SetDim GIx_FadeA

    GIxDimTimer.Interval = 20
    GIxDimTimer.Enabled  = True
End Sub

' One timer handles BOTH DimHit and DimFade
Sub GIxDimTimer_Timer()
    ' Fade path (if active)
    If GIx_FadeSteps > 0 Then
        GIx_FadeStep = GIx_FadeStep + 1
        Dim t
        t = GIx_FadeStep / GIx_FadeSteps
        If t > 1 Then
            t = 1
        End If

        Dim cur
        cur = Lerp(GIx_FadeA, GIx_FadeB, t)
        GIx_SetDim cur

        If GIx_FadeStep >= GIx_FadeSteps Then
            If Not GIx_FadeReturn Then
                GIx_FadeReturn = True

                Dim tmp
                tmp = GIx_FadeA
                GIx_FadeA = GIx_FadeB
                GIx_FadeB = tmp

                GIx_FadeStep = 0
            Else
                GIx_FadeSteps = 0
                GIxDimTimer.Enabled = False
                GIx_SetDim 1
            End If
        End If
        Exit Sub
    End If

    ' Simple hold/restore path (DimHit)
    GIx_DimTicks = GIx_DimTicks + 1
    If GIx_DimTicks >= GIx_DimHoldTicks Then
        GIxDimTimer.Enabled = False
        GIx_SetDim 1
    End If
End Sub


'========================================
' FlasherZero 0..11 + GIx demo per step
'========================================
Dim FZ_Idx

Sub FZ_Init()
    FZ_Idx = 0
    FZ_ShowIndex FZ_Idx
End Sub

Private Sub FZ_HideAll()
    FlasherZero.Visible     = 0
    FlasherZero001.Visible  = 0
    FlasherZero002.Visible  = 0
    FlasherZero003.Visible  = 0
    FlasherZero004.Visible  = 0
    FlasherZero005.Visible  = 0
    FlasherZero006.Visible  = 0
    FlasherZero007.Visible  = 0
    FlasherZero008.Visible  = 0
    FlasherZero009.Visible  = 0
    FlasherZero010.Visible  = 0
    FlasherZero011.Visible  = 0
End Sub

Private Sub FZ_ShowIndex(n)
if showindex=1 then
    If n < 0 Then n = 0
    If n > 11 Then n = 11

    FZ_HideAll

    Select Case n
        Case 0:  FlasherZero.Visible    = 1
        Case 1:  FlasherZero001.Visible = 1
        Case 2:  FlasherZero002.Visible = 1
        Case 3:  FlasherZero003.Visible = 1
        Case 4:  FlasherZero004.Visible = 1
        Case 5:  FlasherZero005.Visible = 1
        Case 6:  FlasherZero006.Visible = 1
        Case 7:  FlasherZero007.Visible = 1
        Case 8:  FlasherZero008.Visible = 1
        Case 9:  FlasherZero009.Visible = 1
        Case 10: FlasherZero010.Visible = 1
        Case 11: FlasherZero011.Visible = 1
    End Select
end if
End Sub

' Fire one GIx effect for each index (0..11)
Private Sub FZ_RunEffect(n)
    Select Case n
        Case 0
            ' Quick teal pulse
            GIx_Pulse RGB(0, 255, 180), 220

        Case 1
            ' Red strobe @ 12 Hz for 600 ms
            'GIx_Strobe RGB(255, 60, 60), 12, 600
            GIx_Sparkle RGB(120, 200, 255), 600

        Case 2
            ' Light-blue sparkle chaos 600 ms
            GIx_Sparkle RGB(120, 200, 255), 600

        Case 3
            ' Green chase forward 800 ms
            GIx_Chase RGB(0, 255, 80), 1, 800

        Case 4
            ' Blue chase backward 800 ms
            GIx_Chase RGB(80, 160, 255), -1, 800

        Case 5
            ' Amber 3-phase wave 1200 ms
            GIx_Wave RGB(255, 180, 40), 1200

        Case 6
            ' Heavy dim hit to 35% for 300 ms
            GIx_DimHit 0.35, 300

        Case 7
            ' Smooth fade to 30% over 250 ms, then back
            GIx_DimFade 1, 0.3, 250

        Case 8
            ' Purple pulse (different color/duration)
            GIx_Pulse RGB(200, 60, 255), 300

        Case 9
            ' White strobe faster @ 16 Hz for 500 ms
            'GIx_Strobe RGB(255, 255, 255), 16, 500
             GIx_Wave RGB(60, 200, 255), 1500

        Case 10
            ' Cool-cyan wave a bit longer
            GIx_Wave RGB(60, 200, 255), 1500

        Case 11
            ' Hot orange sparkle burst
            GIx_Sparkle RGB(255, 140, 40), 700
    End Select
End Sub

' Advance index and run its effect
Sub FZ_Step()
    FZ_Idx = (FZ_Idx + 1) Mod 12
    FZ_ShowIndex FZ_Idx
    FZ_RunEffect FZ_Idx
End Sub


'=================================================
' GIx Randomized Effects — Big / Medium / Small
' VBScript-safe • Uses your existing GIx kit
' Requires editor timers (Enabled=False) named:
'   GIxWaveTimer, GIxDimTimer, GIxSparkleTimer, GIxStrobeTimer, GIxChaseTimer
' Assumes you've called GIx_Init in Table_Init and GI_Lights has lights.
'=================================================

' ---- RNG & last-pick state ----
Dim GIx_RandInitDone
Dim GIx_LastBig : GIx_LastBig  = -1
Dim GIx_LastMed : GIx_LastMed  = -1
Dim GIx_LastSml : GIx_LastSml  = -1

Private Sub GIx_RandInit()
    If Not GIx_RandInitDone Then
        Randomize Timer
        GIx_RandInitDone = True
    End If
End Sub

' Return random int [0..maxExclusive-1], try to avoid immediate repeat
Private Function GIx_NextIdx(maxExclusive, lastIdx)
    Dim tries, r
    r = Int(Rnd * maxExclusive)
    tries = 0
    Do While (r = lastIdx) And (tries < 5) And (maxExclusive > 1)
        r = Int(Rnd * maxExclusive)
        tries = tries + 1
    Loop
    GIx_NextIdx = r
End Function

' ---- Safe timer presence checks ----
Private Function GIx_HasWave()
    On Error Resume Next
    Dim ok: ok = IsObject(GIxWaveTimer)
    On Error GoTo 0
    GIx_HasWave = ok And (GIx_Count > 0)
End Function

Private Function GIx_HasDim()
    On Error Resume Next
    Dim ok: ok = IsObject(GIxDimTimer)
    On Error GoTo 0
    GIx_HasDim = ok And (GIx_Count > 0)
End Function

Private Function GIx_HasSparkle()
    On Error Resume Next
    Dim ok: ok = IsObject(GIxSparkleTimer)
    On Error GoTo 0
    GIx_HasSparkle = ok And (GIx_Count > 0)
End Function

Private Function GIx_HasStrobe()
    On Error Resume Next
    Dim ok: ok = IsObject(GIxStrobeTimer)
    On Error GoTo 0
    GIx_HasStrobe = ok And (GIx_Count > 0)
End Function

Private Function GIx_HasChase()
    On Error Resume Next
    Dim ok: ok = IsObject(GIxChaseTimer)
    On Error GoTo 0
    GIx_HasChase = ok And (GIx_Count > 0)
End Function

' ---- Readiness guards used by Medium/Small ----
Private Function GIx_IsReady_Med()
    GIx_IsReady_Med = (GIx_Count > 0) And (GIx_HasSparkle() Or GIx_HasStrobe() Or GIx_HasChase())
End Function

Private Function GIx_IsReady_Short()
    ' Short can always do Pulse; DimHit needs Dim timer (handled inline)
    GIx_IsReady_Short = (GIx_Count > 0)
End Function

' ---- Always-visible fallback ----
Private Sub GIx_FallbackPulse()
    If GIx_Count > 0 Then
        GIx_Pulse RGB(255, 255, 255), 220
    End If
End Sub

' ---- Quick prelude so BIG always shows something immediately ----
Private Sub GIx_BigPrelude()
    GIx_Pulse RGB(255, 255, 255), 150
End Sub

'=================================================
' BIG — dramatic (~1.5s): Waves/Fades with prelude
'=================================================
Sub GIx_BigRampEffect()
    GIx_RandInit

    ' Immediate visible cue
    GIx_BigPrelude

    Dim canWave, canFade
    canWave = GIx_HasWave()
    canFade = GIx_HasDim()

    If (Not canWave) And (Not canFade) Then
        Debug.Print "GIx_Big: no Wave/Dim timers → fallback"
        GIx_FallbackPulse
        Exit Sub
    End If

    ' Build available options
    Dim opt(5), n
    n = 0
    If canWave Then
        opt(n) = "WAVE_AMBER_1500":  n = n + 1
        opt(n) = "WAVE_CYAN_1500":   n = n + 1
        opt(n) = "WAVE_PURPLE_1800": n = n + 1
    End If
    If canFade Then
        opt(n) = "FADE_0_3_1500":    n = n + 1
        opt(n) = "FADE_0_5_1200":    n = n + 1
    End If

    ' Pick randomized, avoid last when possible
    Dim pickIdx
    pickIdx = GIx_NextIdx(n, GIx_LastBig)
    GIx_LastBig = pickIdx

    ' Run with stronger visuals so it's obvious
    Select Case opt(pickIdx)
        Case "WAVE_AMBER_1500"
            Debug.Print "GIx_Big: Wave Amber 1500"
            GIx_AllColor RGB(255, 180, 40)
            GIx_Wave RGB(255, 180, 40), 1500

        Case "WAVE_CYAN_1500"
            Debug.Print "GIx_Big: Wave Cyan 1500"
            GIx_AllColor RGB(0, 200, 255)
            GIx_Wave RGB(0, 200, 255), 1500

        Case "WAVE_PURPLE_1800"
            Debug.Print "GIx_Big: Wave Purple 1800"
            GIx_AllColor RGB(200, 60, 255)
            GIx_Wave RGB(200, 60, 255), 1800

        Case "FADE_0_3_1500"
            Debug.Print "GIx_Big: DimFade → 0.3 (1500) + Sparkle overlay"
            If GIx_HasSparkle() Then GIx_Sparkle RGB(255, 255, 255), 800
            GIx_DimFade 1, 0.3, 1500

        Case "FADE_0_5_1200"
            Debug.Print "GIx_Big: DimFade → 0.5 (1200) + Sparkle overlay"
            If GIx_HasSparkle() Then GIx_Sparkle RGB(255, 200, 80), 700
            GIx_DimFade 1, 0.5, 1200
    End Select
End Sub

'=================================================
' MED — sparkle/strobe/chase (~600–900 ms)
'=================================================
Sub GIx_MediumEffect()
    GIx_RandInit

    If Not GIx_IsReady_Med() Then
        Debug.Print "GIx_Medium: NOT READY → fallback"
        GIx_FallbackPulse
        Exit Sub
    End If

    Dim choices(9), c, cnt
    cnt = 0
    If GIx_HasSparkle() Then
        choices(cnt) = "SPARKLE_BLUE_700": cnt = cnt + 1
        choices(cnt) = "SPARKLE_GOLD_900": cnt = cnt + 1
    End If
    If GIx_HasStrobe() Then
        choices(cnt) = "STROBE_RED_12_800":   cnt = cnt + 1
        choices(cnt) = "STROBE_WHITE_14_600": cnt = cnt + 1
    End If
    If GIx_HasChase() Then
        choices(cnt) = "CHASE_GREEN_FWD_800": cnt = cnt + 1
        choices(cnt) = "CHASE_PINK_BACK_900": cnt = cnt + 1
    End If

    If cnt = 0 Then
        GIx_FallbackPulse
        Exit Sub
    End If

    c = GIx_NextIdx(cnt, GIx_LastMed)
    GIx_LastMed = c

    Select Case choices(c)
        Case "SPARKLE_BLUE_700"
            Debug.Print "GIx_Med: Sparkle Blue 700"
            GIx_Sparkle RGB(120, 200, 255), 700

        Case "SPARKLE_GOLD_900"
            Debug.Print "GIx_Med: Sparkle Gold 900"
            GIx_Sparkle RGB(255, 200, 80), 900

        Case "STROBE_RED_12_800"
            Debug.Print "GIx_Med: Strobe Red 12Hz 800"
            'GIx_Strobe RGB(255, 60, 60), 12, 800
             GIx_Sparkle RGB(255, 200, 80), 900

        Case "STROBE_WHITE_14_600"
            Debug.Print "GIx_Med: Strobe White 14Hz 600"
            GIx_Chase RGB(0, 255, 80), 1, 800

        Case "CHASE_GREEN_FWD_800"
            Debug.Print "GIx_Med: Chase Green +1 800"
            GIx_Chase RGB(0, 255, 80), 1, 800

        Case "CHASE_PINK_BACK_900"
            Debug.Print "GIx_Med: Chase Pink -1 900"
            GIx_Chase RGB(255, 0, 180), -1, 900
    End Select
End Sub

'=================================================
' SMALL — quick pulses/dims (~200–300 ms)
' (If Dim timer missing, it substitutes a Pulse.)
'=================================================
Sub GIx_SmallEffect()
    GIx_RandInit

    If Not GIx_IsReady_Short() Then
        Debug.Print "GIx_Small: NOT READY → fallback"
        GIx_FallbackPulse
        Exit Sub
    End If

    Dim r
    r = GIx_NextIdx(6, GIx_LastSml)
    GIx_LastSml = r

    Select Case r
        Case 0
            Debug.Print "GIx_Small: Pulse Teal 220"
            GIx_Pulse RGB(0, 255, 180), 220

        Case 1
            Debug.Print "GIx_Small: Pulse Orange 250"
            GIx_Pulse RGB(255, 100, 0), 250

        Case 2
            Debug.Print "GIx_Small: Pulse Purple 280"
            GIx_Pulse RGB(200, 60, 255), 280

        Case 3
            If GIx_HasDim() Then
                Debug.Print "GIx_Small: DimHit 35% 300"
                GIx_DimHit 0.35, 300
            Else
                Debug.Print "GIx_Small: Dim timer missing → Pulse White 200"
                GIx_Pulse RGB(255, 255, 255), 200
            End If

        Case 4
            If GIx_HasDim() Then
                Debug.Print "GIx_Small: DimHit 20% 250"
                GIx_DimHit 0.2, 250
            Else
                Debug.Print "GIx_Small: Dim timer missing → Pulse Teal 220"
                GIx_Pulse RGB(0, 255, 180), 220
            End If

        Case 5
            Debug.Print "GIx_Small: Pulse White 200"
            GIx_Pulse RGB(255, 255, 255), 200
    End Select
End Sub

'=================================================
' Tiny hits → GIx_SmallEffect with throttle
' - Prevents effect spam if many plastics fire at once
' - Optional probability so not every micro-bump flashes
'=================================================
Dim GIx_TinyNextMs
Const GIx_TinyCooldownMs = 120      ' try 80–150 ms
Const GIx_TinyChance     = 0.65     ' 65% of hits trigger (set to 1 for always)

Private Sub GIx_TinyHit_Trigger()
    ' Seed RNG (safe to call often)
    If Not GIx_RandInitDone Then
        Randomize Timer
        GIx_RandInitDone = True
    End If

    Dim nowMs
    nowMs = CSng(Timer * 1000)

    ' Simple cooldown
    If nowMs < GIx_TinyNextMs Then Exit Sub

    ' (Optional) probability gate
    If Rnd > GIx_TinyChance Then
        ' even when skipping, set a small grace to avoid hammering every frame
        GIx_TinyNextMs = nowMs + 30
        Exit Sub
    End If

    ' Fire the short random effect
    GIx_SmallEffect

    ' Next allowed time
    GIx_TinyNextMs = nowMs + GIx_TinyCooldownMs
End Sub

'===============================
' UltraDMD — End-of-Ball Bonus Cycle
'===============================

' --- Counters you increment during play ---
Dim RampHits, TieHits, MouseHits, DeathStarHits
Dim DrainsThisBall   ' counts how many individual balls drained during this turn (multiballs etc.)

' --- Cycle state ---
Dim UMD_BonusMsgs(), UMD_BonusVals(), UMD_BonusKeys()
Dim UMD_BonusCount, UMD_BonusIdx, UMD_BonusActive
Dim UMD_BonusRunningTotal

' --- Multipliers (edit to taste) ---
Const RAMP_MUL        = 1000
Const TIE_MUL         = 5000
Const MOUSE_MUL       = 3000
Const DEATHSTAR_MUL   = 10000
Const BALLS_MUL       = 1000

' --- DMD timing ---
Const BONUS_SCENE_MS = 600
Const BONUS_GAP_MS   = 700

'--------------------------------
' Init: call once in Table_Init
'--------------------------------
Sub UMD_InitBonus()
    RampHits        = 0
    TieHits         = 0
    MouseHits       = 0
    DeathStarHits   = 0
    DrainsThisBall  = 0

    UMD_BonusActive        = False
    UMD_BonusIdx           = 0
    UMD_BonusCount         = 0
    UMD_BonusRunningTotal  = 0

    QTimer.Enabled  = False
End Sub

'--------------------------------
' Call these from gameplay events
'--------------------------------
Sub UMD_OnRampHit()       : RampHits      = RampHits + 1 : End Sub
Sub UMD_OnTieHit()        : TieHits       = TieHits + 1  : End Sub
Sub UMD_OnMouseHit()      : MouseHits     = MouseHits + 1: End Sub
Sub UMD_OnDeathStarHit()  : DeathStarHits = DeathStarHits + 1 : End Sub

' Call THIS on EVERY physical drain (multiball drains included)
Sub UMD_RecordBallDrain()
    DrainsThisBall = DrainsThisBall + 1
End Sub

' Reset per ball (call when the NEW ball is actually launched)


'--------------------------------
' Trigger this at END-OF-TURN drain
' (only when the player's last ball has left play)
'--------------------------------
Sub UMD_OnBallDrain_ShowBonuses()
    ' Always (re)build the items first
    UMD_BuildBonusList

    If UMD_BonusCount = 0 Then
        If UltraMode = 1 Then
            DMD_DisplaySceneTextWithPause "No bonus", 0, 800
        End If
        Exit Sub
    End If

    If UltraMode = 1 Then
        ' Show DMD cycle
        UMD_BonusActive        = True
        UMD_BonusIdx           = 0
        UMD_BonusRunningTotal  = 0
        UMD_ShowNextBonus
    Else
        ' No DMD: just score instantly
        UMD_AddBonusesImmediate
        QTimer.Enabled = False
    End If
End Sub

'--------------------------------
' Build fresh list from counters (always include all categories)
'--------------------------------
Private Sub UMD_BuildBonusList()
    ReDim UMD_BonusMsgs(-1)
    ReDim UMD_BonusVals(-1)
    ReDim UMD_BonusKeys(-1)
    UMD_BonusCount = 0

    ' Always add every category (0 is fine)
    UMD_AppendBonus "Ramps And Orbits",      CLng(RampHits       * RAMP_MUL),    "ramp"
    UMD_AppendBonus "TIE fighter hits",      CLng(TieHits        * TIE_MUL),     "tie"
    UMD_AppendBonus "Toaster Droid hits",    CLng(MouseHits      * MOUSE_MUL),   "mouse"
    UMD_AppendBonus "Death Star hits",       CLng(DeathStarHits  * DEATHSTAR_MUL), "deathstar"

    ' Extra balls: count beyond the first; never negative
    Dim extraBalls: extraBalls = DrainsThisBall - 1
    If extraBalls < 0 Then extraBalls = 0
    UMD_AppendBonus "Multiballs",     CLng(extraBalls * BALLS_MUL),       "balls"

    ' Always add a TOTAL row at the end
    UMD_AppendBonus "BONUS TOTAL", 0, "total"
End Sub


'--------------------------------
' Headless scoring path (no DMD)
'--------------------------------
Private Sub UMD_AddBonusesImmediate()
    Dim i, amt, key, running
    running = 0
    For i = 0 To UMD_BonusCount - 1
        key = UMD_BonusKeys(i)
        If key <> "total" Then
            amt = UMD_BonusVals(i)
            If amt <> 0 Then
                AddScore amt
                running = running + amt
            End If
        End If
    Next
    ' If you want to add *only* the grand total once instead:
    ' Comment the AddScore in the loop above and just:
    ' AddScore running
End Sub

'--------------------------------
' Append helper
'--------------------------------
Private Sub UMD_AppendBonus(msg, val, key)
    Dim n: n = UMD_BonusCount
    ReDim Preserve UMD_BonusMsgs(n)
    ReDim Preserve UMD_BonusVals(n)
    ReDim Preserve UMD_BonusKeys(n)
    UMD_BonusMsgs(n) = CStr(msg)
    UMD_BonusVals(n) = CLng(val)
    UMD_BonusKeys(n) = CStr(key)
    UMD_BonusCount   = n + 1
End Sub

'--------------------------------
' Show current item (DMD path only)
'--------------------------------
Private Sub UMD_ShowNextBonus()
    If Not UMD_BonusActive Then Exit Sub
    If UMD_BonusIdx >= UMD_BonusCount Then
        UMD_BonusActive = False
        QTimer.Enabled  = False
        Exit Sub
    End If

    Dim title, amount, key
    title  = UMD_BonusMsgs(UMD_BonusIdx)
    amount = UMD_BonusVals(UMD_BonusIdx)
    key    = UMD_BonusKeys(UMD_BonusIdx)

    If key <> "total" Then
        UMD_BonusRunningTotal = UMD_BonusRunningTotal + amount
        If amount <> 0 Then AddScore amount
        DMD_DisplaySceneTextWithPause title, amount, BONUS_SCENE_MS
        UMD_PlayBonusSound key
ElseIf key = "total" Then
    Dim topLine
    ' e.g., "TOTAL BONUS +12345"
    topLine = "TOTAL BONUS +" & CStr(UMD_BonusRunningTotal)
    ' If you prefer separators, use: topLine = "TOTAL BONUS +" & FormatNumber(UMD_BonusRunningTotal, 0)
    DMD_DisplaySceneTextWithPause topLine, p1score, BONUS_SCENE_MS
    UMD_PlayBonusSound key
    ' (then the rest of the sub continues as-is)
End If

    UMD_BonusIdx    = UMD_BonusIdx + 1
    QTimer.Interval = BONUS_GAP_MS
    QTimer.Enabled  = True
End Sub

'--------------------------------
' Timer advances the cycle
'--------------------------------
'Sub QTimer_Timer()
'    If UMD_BonusActive Then
'        QTimer.Enabled = False
'        UMD_ShowNextBonus
'    Else
'        QTimer.Enabled = False
'    End If
'End Sub

'--------------------------------
' Sound map (only fires if UltraMode=1)
'--------------------------------
Private Sub UMD_PlayBonusSound(key)
    Select Case key
        Case "ramp"     : PlaySound "s_rampbonus"
        Case "tie"      : PlaySound "s_tiebonus"
        Case "mouse"    : PlaySound "s_mousebonus"
        Case "deathstar": PlaySound "s_deathstar"
        Case "balls"    : PlaySound "s_ballsdrained"
        Case "total"    : PlaySound "s_bonustotal"
        Case Else       : ' none
    End Select
End Sub


' Call this on the NEW ball launch, before you zero the counters
Sub UMD_OnNextBallStart_Reset()
    ' --- OPTIONAL SAFETY: finish any in-flight bonus cycle ---
    If UMD_BonusActive Then
        UMD_FlushRemainingBonuses   ' award anything not yet added
        UMD_BonusActive = False
        ' If you used a dedicated timer:
        ' UMDTimer.Enabled = False
        ' If you reused your existing QTimer:
        'QTimer.Enabled = False
    End If
    ' ---------------------------------------------------------

    ' Now clear per-ball counters
    RampHits       = 0
    TieHits        = 0
    MouseHits      = 0
    DeathStarHits  = 0
    DrainsThisBall = 0
End Sub

' Awards only the not-yet-shown line items, then we cancel the cycle.
Private Sub UMD_FlushRemainingBonuses()
    Dim i, amt, key
    For i = UMD_BonusIdx To UMD_BonusCount - 1
        key = UMD_BonusKeys(i)
        If key <> "total" Then
            amt = UMD_BonusVals(i)
            If amt <> 0 Then AddScore amt
        End If
    Next
End Sub

sub timer002_timer
ramp026.collidable=0
Timer002.enabled=0
end sub

sub Trigger007_hit
addscore 200
end sub

'===============================
' Single-Flasher animation using: frame_00_delay-0.1s .. frame_24_delay-0.1s
'===============================
Dim ANIM_PREFIX, ANIM_SUFFIX, ANIM_FRAME_COUNT, ANIM_FPS, ANIM_LOOP
Dim animFrame

Sub Anim_Init()
    ANIM_PREFIX      = "frame_"
    ANIM_SUFFIX      = "_delay-0.1s"
    ANIM_FRAME_COUNT = 25          ' frames 0..24
    ANIM_FPS         = 10          ' 0.1s per frame to match your filenames
    ANIM_LOOP        = False       ' True to loop, False to play once

    animFrame = 0
    animTimer.Interval = CInt(1000 \ ANIM_FPS)
    animF0.Visible = False
End Sub

' Start the animation (you can call this directly, or keep using your startjohnny timer)
Sub Start_AnimOnce()
    animFrame = 0
    animF0.Visible = True
    animTimer.Enabled = True
End Sub

' If you want to keep your existing start timer name:
Sub startjohnny_Timer()
    Start_AnimOnce
    startjohnny.Enabled = False
End Sub

Sub animTimer_Timer()
    If animFrame < ANIM_FRAME_COUNT Then
        animF0.ImageA = ANIM_FrameName(animFrame)   ' e.g., "frame_07_delay-0.1s"
        animFrame = animFrame + 1
    Else
        If ANIM_LOOP Then
            animFrame = 0
            animF0.ImageA = ANIM_FrameName(animFrame)
        Else
            animTimer.Enabled = False
            animF0.Visible = False
        End If
    End If
End Sub

Private Function ANIM_FrameName(n)
    ANIM_FrameName = ANIM_PREFIX & Right("00" & CStr(n), 2) & ANIM_SUFFIX
End Function


' Optional hard stop you can call anytime
Sub Anim_Stop()
    animTimer.Enabled = False
    animF0.Visible = False
End Sub


'from other table remove all other flasher, add anim_init to table init, remove johhnystart from table_init, remove other code blocks


'==========================
' Orbit detection (2 triggers)
'==========================

' --- Tuning ---
Const ORBIT_TIMEOUT_MS = 3000   ' max time allowed between start and end (ms)
Const ORBIT_NEEDS_UP    = True  ' require ball moving up-table at START (VelY < 0)
Const ORBIT_NEEDS_DOWN  = True  ' require ball moving down-table at END   (VelY > 0)

' --- State ---
Dim OrbitSeen : Set OrbitSeen = CreateObject("Scripting.Dictionary")
' Stores: OrbitSeen(ballID) = startTick (GameTime when it entered)

' Call this from the START trigger’s Hit event
Sub OrbitStart_Hit()
    Dim b : Set b = ActiveBall
    If b Is Nothing Then Exit Sub

    ' (optional) require up-table motion at the entry
    If ORBIT_NEEDS_UP And b.VelY >= 0 Then Exit Sub

    OrbitSeen(b.ID) = GameTime   ' remember *this* ball by its unique ID
End Sub

' Call this from the END trigger’s Hit event
Sub OrbitEnd_Hit()
    Dim b : Set b = ActiveBall
    If b Is Nothing Then Exit Sub

    ' (optional) require down-table motion at the exit
    If ORBIT_NEEDS_DOWN And b.VelY <= 0 Then Exit Sub

    If OrbitSeen.Exists(b.ID) Then
        Dim dt : dt = GameTime - CLng(OrbitSeen(b.ID))
        OrbitSeen.Remove b.ID  ' consume the entry either way to avoid re-triggers

        If dt <= ORBIT_TIMEOUT_MS Then
            ' ---- ORBIT CONFIRMED ----
            OnOrbitComplete b
        End If
    End If
End Sub

' Your reward logic here (sound, lights, score, etc.)
Sub OnOrbitComplete(b)
    ' Example:
    If Rnd < 0.5 Then
        Playsound "raider1"
        Else
        Playsound "raider2"
    End If
Light016.state=2
timer004.enabled=1
UMD_OnRampHit
    ' AddScore 100000
End Sub

' (nice to have) cleanup if a ball drains while “armed”
'Sub Drain_Hit()
'    Dim b : Set b = ActiveBall
'    If Not b Is Nothing Then
'        If OrbitSeen.Exists(b.ID) Then OrbitSeen.Remove b.ID
'    End If
'End Sub

'==========================
' Orbit #2 detection (2 triggers)
'==========================

' --- Tuning ---
Const ORBIT2_TIMEOUT_MS = 3000   ' max time allowed between start and end (ms)
Const ORBIT2_NEEDS_UP   = True   ' require ball moving up-table at START (VelY < 0)
Const ORBIT2_NEEDS_DOWN = True   ' require ball moving down-table at END   (VelY > 0)

' --- State ---
Dim Orbit2Seen : Set Orbit2Seen = CreateObject("Scripting.Dictionary")
' Stores: Orbit2Seen(ballID) = startTick (GameTime when it entered)

' Call this from the second orbit START trigger’s Hit event
Sub Orbit2Start_Hit()
    Dim b : Set b = ActiveBall
    If b Is Nothing Then Exit Sub

    If ORBIT2_NEEDS_UP And b.VelY >= 0 Then Exit Sub

    Orbit2Seen(b.ID) = GameTime
End Sub

' Call this from the second orbit END trigger’s Hit event
Sub Orbit2End_Hit()
    Dim b : Set b = ActiveBall
    If b Is Nothing Then Exit Sub

    If ORBIT2_NEEDS_DOWN And b.VelY <= 0 Then Exit Sub

    If Orbit2Seen.Exists(b.ID) Then
        Dim dt : dt = GameTime - CLng(Orbit2Seen(b.ID))
        Orbit2Seen.Remove b.ID

        If dt <= ORBIT2_TIMEOUT_MS Then
            ' ---- ORBIT #2 CONFIRMED ----
            OnOrbit2Complete b
        End If
    End If
End Sub

' Reward logic for orbit #2
Sub OnOrbit2Complete(b)
    ' Example reward:
   '  PlaySound "knocker"
    If Rnd < 0.5 Then
        Playsound "xwing1"
        Else
        Playsound "xwing2"
    End If
Light002.state=2
timer003.enabled=1
UMD_OnRampHit
    ' AddScore 200000
End Sub

sub timer003_timer
light002.state=0
timer003.enabled=0
end sub

sub timer004_timer
Light016.state=0
Timer004.enabled=0
end sub

' Cleanup on drain (safety)
'Sub Drain2_Hit()
'    Dim b : Set b = ActiveBall
'    If Not b Is Nothing Then
'        If Orbit2Seen.Exists(b.ID) Then Orbit2Seen.Remove b.ID
'    End If
'End Sub

'-add this code, change sub drain code, change dmd logic to ramps and orbits instead of just ramps

Sub ToggleBlastB2S
       Controller.B2SSetData 201, 1
blastb2stimer.enabled=1
End Sub

sub blastb2stimer_timer
       Controller.B2SSetData 201, 0
blastb2stimer.enabled=0
end Sub

Sub PlayRandomLaugh()
    Dim n
    n = Int(Rnd * 6) + 1   ' random number 1–6
    PlaySound "laugh" & n  ' concatenates laugh1, laugh2 … laugh6
End Sub

Sub PlayRandomLost()
If Ballstext.Text <> 5 Then
    Dim n
    n = Int(Rnd * 7) + 1   ' random number from 1–7
    PlaySound "lost_" & n
End If
End Sub

'======================
' SCOUT + RIDER (Primitive018 + Primitive038)
' - 100u vertical travel, non-interruptible
' - Primitive038 mirrors motion
' - Damped wobble while lowering (both prims)
'======================

' Movement state
Dim ScoutMoving, ScoutStartZ, ScoutTargetZ
Const SCOUT_SPEED = 2   ' units per tick

' Wobble state
Dim ScoutIsLowering, ScoutPhaseStartTick
Dim ScoutBaseX, ScoutBaseY, ScoutBaseRotZ
Dim RiderBaseX, RiderBaseY, RiderBaseRotZ  ' for Primitive038
Dim ScoutZOffset38                          ' keeps 038 aligned in Z

Const WOBBLE_FREQ_HZ = 1    ' cycles per second
Const WOBBLE_PIX     = 1    ' side sway (X)
Const WOBBLE_DEG     = 1    ' twist (RotZ)

'--- Timer (Interval ~20ms, Enabled=False, TimerName=ScoutMove_Timer) ---
Sub ScoutMove_Timer()
    If Not ScoutMoving Then Exit Sub

    Dim dz
    dz = ScoutTargetZ - Primitive018.Z

    ' Reached destination?
    If Abs(dz) <= SCOUT_SPEED Then
        ' snap to final Z
        Primitive018.Z = ScoutTargetZ
        Primitive038.Z = Primitive018.Z + ScoutZOffset38

        ScoutMoving = False
        ScoutRampTrig.Enabled = True
        ScoutMove.Enabled = False

        ' if we just finished lowering: stop wobble, restore transforms, and cleanup
        If ScoutIsLowering Then
            ' restore exact base transforms (no residual wobble)
            Primitive018.X    = ScoutBaseX
            Primitive018.Y    = ScoutBaseY
            Primitive018.RotZ = ScoutBaseRotZ

            Primitive038.X    = RiderBaseX
            Primitive038.Y    = RiderBaseY
            Primitive038.RotZ = RiderBaseRotZ

            ScoutIsLowering = False

            ' --- post-lower cleanup (only once lowering completes) ---
            ScoutRaised  = False
            RampScout0.Collidable = False
            RampScout1.Collidable = False
            RampScout2.Collidable = False
            RampScout3.Collidable = False
            kicker003.enabled=false
        End If

        Exit Sub
    End If

    ' step toward target
    Dim stepZ
    stepZ = Sgn(dz) * SCOUT_SPEED
    Primitive018.Z = Primitive018.Z + stepZ
    Primitive038.Z = Primitive018.Z + ScoutZOffset38   ' lock 038 to 018

    ' Apply wobble only while lowering
    If ScoutIsLowering Then
        ' elapsed time (s) since lower started
        Dim tms, tsec, phase
        tms  = GameTime - ScoutPhaseStartTick
        tsec = tms / 1000.0
        phase = 2 * 3.14159265358979 * WOBBLE_FREQ_HZ * tsec

        ' progress 0..1 (0 = start/top, 1 = bottom)
        Dim prog, amp
        prog = (ScoutStartZ - Primitive018.Z) / 80
        If prog < 0 Then
            prog = 0
        ElseIf prog > 1 Then
            prog = 1
        End If

        ' decay wobble toward bottom
        amp = (1 - prog)

        ' wobble components
        Dim wobX, wobRz
        wobX  = WOBBLE_PIX * amp * Sin(phase)
        wobRz = WOBBLE_DEG * amp * Sin(phase * 1.2 + 0.7)

        ' apply to BOTH primitives from their own bases
        Primitive018.X    = ScoutBaseX + wobX
        Primitive018.RotZ = ScoutBaseRotZ + wobRz

        Primitive038.X    = RiderBaseX + wobX
        Primitive038.RotZ = RiderBaseRotZ + wobRz
    End If
End Sub

dim scoutraised
'--- Raise Scout (no wobble) ---
Sub Scout_Raise()
    If ScoutMoving or scoutraised Then Exit Sub  ' non-interruptible
    playsound "freeze"

    ScoutMoving  = True
    ScoutRaised  = True
    ScoutIsLowering = False
    ScoutStartZ  = Primitive018.Z
    ScoutTargetZ = ScoutStartZ + 80

    ' maintain rider's Z offset relative to scout
    ScoutZOffset38 = Primitive038.Z - Primitive018.Z

    ' (optional) arm geometry as the ramp comes up—do this only once we know we are moving
        RampScout0.Collidable = True
    RampScout1.Collidable = True
    RampScout2.Collidable = True
    RampScout3.Collidable = True
    kicker003.enabled=True
    ScoutMove.Enabled = True
End Sub

sub kicker003_hit
kicker003.destroyball
kicker004.createball
kicker004.kick 10, 20
kicker003.enabled=false
end sub

'--- Lower Scout (with wobble) ---
Sub Scout_Lower()
    If ScoutMoving Then Exit Sub  ' non-interruptible
    ScoutRampTrig.Enabled = False

    ScoutMoving  = True
    ScoutStartZ  = Primitive018.Z
    ScoutTargetZ = ScoutStartZ - 80

    ' capture bases for clean wobble reference
    ScoutBaseX    = Primitive018.X
    ScoutBaseY    = Primitive018.Y
    ScoutBaseRotZ = Primitive018.RotZ

    RiderBaseX    = Primitive038.X
    RiderBaseY    = Primitive038.Y
    RiderBaseRotZ = Primitive038.RotZ

    ' keep rider's Z offset consistent
    ScoutZOffset38 = Primitive038.Z - Primitive018.Z

    ScoutIsLowering     = True
    ScoutPhaseStartTick = GameTime

    ScoutMove.Enabled = True
End Sub


sub ScoutRampTrig_hit
if scoutraised=true Then
addscore 15000
ScoutRampTrig.Enabled = False
playsound "what_the"
Scout_Lower
end If
end sub

'==========================
' Rank progression (auto-reset, no helper)
'==========================


'=========================
' Rank Progression Tracker
'=========================
Dim RankAnnounced
ReDim RankAnnounced(5)



' Resets at new game (call Rank_ResetAnnounce)

Sub Rank_ResetAnnounce()
    Dim i
    For i = 1 To 5
        RankAnnounced(i) = False
    Next
End Sub

'=========================
' Rank Timer
'=========================
Sub RankTimer_Timer()
    Dim r1, r2, r3, r4, r5

    ' --- Conditions ---
    r1 = (Light180.State = 1 Or Light182.State = 1)
    r2 = (Light180.State = 1 And Light182.State = 1)
    r3 = (dstar003.State = 1)
    r4 = (tlight003.State = 1)
    r5 = (p1score > 800000)

    ' --- Rank 1 ---
    If r1 Then
        If Light019.State = 0 Then
            Light019.State = 1
            If Not RankAnnounced(1) Then
                PuPEvent 879  ' <-- Farmer rank pup EventNum
                if testrank=1 then 
                  playsound "knocker"
                end if
                RankAnnounced(1) = True
                addscore 10000
            End If
        End If
    Else
        Light019.State = 0
    End If

    ' --- Rank 2 ---
    If r2 Then
        If Light029.State = 0 Then
            Light029.State = 1
            If Not RankAnnounced(2) Then
                PuPEvent 881 ' <-- Smuggler rank pup EventNum
                if testrank=1 then 
                  playsound "knocker"
                end if
                RankAnnounced(2) = True
                addscore 10000
            End If
        End If
    Else
        Light029.State = 0
    End If

    ' --- Rank 3 ---
    If r3 Then
        If Light020.State = 0 Then
            Light020.State = 1
            If Not RankAnnounced(3) Then
                PuPEvent 880 ' <-- Pilot rank pup EventNum
                if testrank=1 then 
                  playsound "knocker"
                end if
                RankAnnounced(3) = True
                addscore 10000
            End If
        End If
    Else
        Light020.State = 0
    End If

    ' --- Rank 4 ---
    If r4 Then
        If Light032.State = 0 Then
            Light032.State = 1
            If Not RankAnnounced(4) Then
                PuPEvent 882 ' <-- Squadron Leader rank pup EventNum
                if testrank=1 then 
                  playsound "knocker"
                end if
                RankAnnounced(4) = True
                addscore 10000
            End If
        End If
    Else
        Light032.State = 0
    End If

    ' --- Rank 5 ---
    If r5 Then
        If Light028.State = 0 Then
            Light028.State = 1
            If Not RankAnnounced(5) Then
                PuPEvent 883 ' <-- Jedi Knight rank pup EventNum
                if testrank=1 then 
                  playsound "knocker"
                end if
                RankAnnounced(5) = True
                addscore 10000
            End If
        End If
    Else
        Light028.State = 0
    End If
End Sub



'==========================
' Reset all rank lights
'==========================
Sub ResetRanks()
    Light019.State = 0
    Light029.State = 0
    Light020.State = 0
    Light032.State = 0
    Light028.State = 0
End Sub

Sub FlipLights_On()
  Dim l : For Each l In fliplights : l.State = 1 : Next
End Sub

Sub FlipLights_Off()
  Dim l : For Each l In fliplights : l.State = 0 : Next
End Sub

Sub FlipLights_Blink()
  Dim l : For Each l In fliplights : l.State = 2 : Next
End Sub

sub kicker005_hit
kicker005.kick 180, 22
end Sub

dim mynockshot
mynockshot=0

sub mynock_in
Flasher001.visible=1
mynockwait.enabled=1
playsound "mynock"
Flasher_JiggleStart
stopjiggle.enabled=1
end Sub

sub trigger008_hit
Trigger008.enabled=0
Flasher001.visible=0
flasher015.Visible = 1
hideblast.enabled=1
if mynockshot=0 Then
playsound "blastmynock"
end If
if mynockshot=1 Then
playsound "mynockblast_short"
end If
addscore 5000
mynockshot=1
end Sub


sub mynock_out
Trigger008.enabled=0
Flasher001.visible=0
end Sub

sub mynockwait_timer
Trigger008.enabled=1
mynockwait.enabled=0
end Sub

'=========================
' F L A S H E R   J I G G L E
'=========================

' --- Setup ---
Dim JigglePhase, JiggleOrigX, JiggleOrigY



' --- Start jiggle for 1 second ---
Sub Flasher_JiggleStart()
    Dim f : Set f = Flasher001   ' your flasher name
    JiggleOrigX = f.X
    JiggleOrigY = f.Y
    JigglePhase = 0
    FlasherJiggleTimer.Enabled = True
    'vpmtimer.AddTimer 1000, "Flasher_JiggleStop"  ' stop after 1 second
End Sub

Sub Flasher_JiggleStop()
    FlasherJiggleTimer.Enabled = False
    Flasher001.X = JiggleOrigX
    Flasher001.Y = JiggleOrigY
End Sub

' --- Main animation ---
Sub FlasherJiggleTimer_Timer()
    Dim f : Set f = Flasher001
    JigglePhase = JigglePhase + 0.3
    f.X = JiggleOrigX + Sin(JigglePhase * 3) * 3
    f.Y = JiggleOrigY + Cos(JigglePhase * 4) * 3
End Sub

sub stopjiggle_timer
Flasher_JiggleStop
stopjiggle.enabled=0
end Sub

sub timer008_timer
gi002.state=0
timer008.enabled=0
end Sub

sub timer010_timer
gi020.state=0
timer010.enabled=0
end Sub
