' ****************************************************************
'             SCREAM for VISUAL PINBALL X 10.8
'    version 1.0 By Retro Rommy6604 AKA Out of My Mind Arcade
' ****************************************************************

'On the DOF website put Exxx
'DOF commands:
'101 Left Flipper
'102 Right Flipper
'103 left slingshot
'104 right slingshot
'105
'106
'107
'108 RIGHT Bumper
'109
'110
'111 Scoop
'118
'119
'120 AutoFire
'122 knocker
'123 ballrelease
'204 Lamp posts (teenagers)
'205 Blue targets
'206 Ramps, lanes and spinners
'207 In and outlanes
'208 Plunger

Option Explicit
Randomize


' COPY EVERYTHING BELOW TO THE TOP OF YOUR TABLE SCRIPT UNDER OPTION EXPLICIT

'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = true               ' enable Pinup Player functions for this table
cPuPPack = "SCREAM"    ' name of the PuP-Pack / PuPVideos folder for this table

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

PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Player Options
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X 

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 

  

	Rockmusic = 1 'change this to 1 to switch out the background music 
	soundtrackvol = 100 'Set the background audio volume to whatever you'd like out of 100
	videovol = 113 'set the volume you'd like for the videos
    billyvol = 108 'set the volume you'd like for the videos
    stuvol = 100 'set the volume you'd like for the videos
    mickeyvol = 105 'set the volume you'd like for the videos
    mloomisvol = 117 'set the volume you'd like for the videos
    romanvol = 108 'set the volume you'd like for the videos
    jillvol = 108 'set the volume you'd like for the videos
    charlievol = 110 'set the volume you'd like for the videos
    richievol = 103 'set the volume you'd like for the videos
    ambervol = 107 'set the volume you'd like for the videos
    quinnvol = 107 'set the volume you'd like for the videos
    ethanvol = 103 'set the volume you'd like for the videos
    waynevol = 106 'set the volume you'd like for the videos
    deweyvol = 103 'set the volume you'd like for the videos
    galevol = 100 'set the volume you'd like for the videos
    sidvol = 110 'set the volume you'd like for the videos
    taravol = 103 'set the volume you'd like for the videos
    samvol = 108 'set the volume you'd like for the videos
    mblockvol = 116 'set the volume you'd like for the videos
    killedvol = 113 'set the volume you'd like for the videos
    introvol = 113 'set the volume you'd like for the videos
    drainvol = 113 'set the volume you'd like for the videos
    wizardvol = 113 'set the volume you'd like for the videos
    Audiovol = 120 'set the volume you'd like for the audio FX
	calloutvol = 75 ' set this to whatever you're like your callouts to be
    Topper2vol = 0 ' set this to whatever you're like your topper vol to be
	calloutlowermusicvol = 1 'set to 1 if you want music volume lowered during audio callouts



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   PUPDMD - WIP
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

'**************************
'   PinUp Player Config
'   Change HasPuP = True if using PinUp Player Videos
'**************************

	Dim HasPup:HasPuP = True

'	Dim PuPlayer 

    '***********************************
    'PUP PLAYER SCREEN NUMBER OPTIONS - 
    '***********************************
    '********************************************************************************
    'USE THE BELOW OPTIONS TO SET THE PUP-PACK TO THE SCREEN NUMBERS OF YOUR CHOICE
    '********************************************************************************

			Const pTopper2=0        'change this number to the screen of your choosing for Topper
			Const pDMD=1
            Const pBackglass=2		'change this number to the screen of your choosing for backglass		
			Const pPlayfield=3
			Const pMusic=4
'			Const pBackglassB2S = 2			
			Const pAudio=7
			Const pCallouts=8
            Const pFullDMD=5        'change this number to the screen of your choosing for FullDMD
            

	if HasPuP Then
	on error resume next
	Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay") 
	on error goto 0
	if not IsObject(PuPlayer) then HasPuP = False
	end If

	if HasPuP Then

	PuPlayer.Init pBackglass,"SCREAM"
	If useB2SBG = 1 Then
		PuPlayer.Init pBackglass,"SCREAM"		
	End If
    PuPlayer.Init pFullDMD,"SCREAM"
    PuPlayer.Init pTopper2,"SCREAM"
	PuPlayer.Init pMusic,"SCREAM"
	PuPlayer.Init pAudio,"SCREAM"
	PuPlayer.Init pCallouts,"SCREAM"
	If toppervideo = 1 Then
	PuPlayer.Init pTopper,"SCREAM"
	If useB2SBG = 1 Then
		PuPlayer.Init pBackglass,"SCREAM"		
	End If
	End If

    '**************************************
    'PUP PLAYER TURN ON OR OFF OPTIONS - 
    '**************************************
    '**********************************************************************************************************************************************
    'COMMENT OUT THE LINES ABOVE ( PuPlayer.Init pFullDMD) - (PuPlayer.Init pTopper2) IF YOU WANT TO SHUT OFF THE TOPPER SCREEN OR FULLDMD SCREEN
    '**********************************************************************************************************************************************


	PuPlayer.SetScreenex pBackglass,0,0,0,0,0       'Set PuPlayer DMD TO Always ON    <screen number> , xpos, ypos, width, height, POPUP
    PuPlayer.SetScreenex pAudio,0,0,0,0,2
	PuPlayer.hide pAudio
	PuPlayer.SetScreenex pMusic,0,0,0,0,2
	PuPlayer.hide pMusic
	PuPlayer.SetScreenex pCallouts,0,0,0,0,2
	PuPlayer.hide pCallouts
    PuPlayer.SetScreenex pTopper2,0,0,0,0,0	
    PuPlayer.SetScreenex pFullDMD,0,0,0,0,0
    'PuPlayer.SetScreenex pDMD,0,0,0,0,0	
	If useB2SBG = 1 then 
		PuPlayer.SetScreenex pBackglassB2S,0,0,0,0,0
	If useB2SBG = 1 Then
		PuPlayer.SetScreenex pTopper,0,0,0,0,0	
	End If
	End If  

	Sub chilloutthemusic
		If calloutlowermusicvol = 1 Then
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":40 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":40 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 7, ""FN"":11, ""VL"":40 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 8, ""FN"":11, ""VL"":"&(calloutvol)&" }"
			vpmtimer.addtimer 4500, "turnitbackup'"
		End If
	End Sub

	Sub turnitbackup
		If calloutlowermusicvol = 1 Then
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(videovol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(billyvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(stuvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(stuvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(mickeyvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(mloomisvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(romanvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(jillvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(charlievol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(richievol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(ambervol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(quinnvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(ethanvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(waynevol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(deweyvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(galevol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(sidvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(taravol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(samvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(mblockvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(killedvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(introvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(drainvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(wizardvol)&" }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":"&(soundtrackvol)&" }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 7, ""FN"":11, ""VL"":"&(soundtrackvol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 0, ""FN"":11, ""VL"":"&(Topper2vol)&" }"
            PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 7, ""FN"":11, ""VL"":"&(Audiovol)&" }"
		End If
	End Sub


PuPlayer.playlistadd pMusic,"audioattract", 1 , 0
PuPlayer.playlistadd pMusic,"audiobg", 1 , 0
PuPlayer.playlistadd pMusic,"audioclear", 1 , 0
PuPlayer.playlistadd pMusic,"audiohorror", 1 , 0
PuPlayer.playlistadd pAudio,"audioevents", 1 , 0
PuPlayer.playlistadd pAudio,"audiomodes", 1 , 0
PuPlayer.playlistadd pMusic,"audioquotes", 1 , 0
PuPlayer.playlistadd pAudio,"audioFreddyQuotes", 1 , 0
PuPlayer.playlistadd pAudio,"audiomultiballs", 1 , 0
PuPlayer.playlistadd pCallouts,"audiocallouts", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioBallLost", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioJackpots", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioSuperJackpots", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioWizardMode", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioWizardCleared", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioGameLost", 1 , 0
PuPlayer.playlistadd pCallouts,"audiocheers", 1 , 0
PuPlayer.playlistadd pBackglass,"bgs", 1 , 0
PuPlayer.playlistadd pBackglass,"scene", 1 , 0
PuPlayer.playlistadd pBackglass,"videointro", 1 , 0
PuPlayer.playlistadd pBackglass,"videostart", 1 , 0
PuPlayer.playlistadd pBackglass,"videodrain", 1 , 0
PuPlayer.playlistadd pBackglass,"videobase", 1 , 0
PuPlayer.playlistadd pBackglass,"videobilly", 1 , 0
PuPlayer.playlistadd pBackglass,"videostu", 1 , 0
PuPlayer.playlistadd pBackglass,"videotara", 1 , 0
PuPlayer.playlistadd pBackglass,"videosid", 1 , 0
PuPlayer.playlistadd pBackglass,"videorichie", 1 , 0
PuPlayer.playlistadd pBackglass,"videoamber", 1 , 0
PuPlayer.playlistadd pBackglass,"videoquinn", 1 , 0
PuPlayer.playlistadd pBackglass,"videoethan", 1 , 0
PuPlayer.playlistadd pBackglass,"videowayne", 1 , 0
PuPlayer.playlistadd pBackglass,"videosam", 1 , 0
PuPlayer.playlistadd pBackglass,"videojill", 1 , 0
PuPlayer.playlistadd pBackglass,"videocharlie", 1 , 0
PuPlayer.playlistadd pBackglass,"videomickey", 1 , 0
PuPlayer.playlistadd pBackglass,"videoroman", 1 , 0
PuPlayer.playlistadd pBackglass,"videogale", 1 , 0
PuPlayer.playlistadd pBackglass,"videomloomis", 1 , 0
PuPlayer.playlistadd pBackglass,"videodewey", 1 , 0
PuPlayer.playlistadd pBackglass,"videopolicehurry", 1 , 0
PuPlayer.playlistadd pBackglass,"videocore4hurry", 1 , 0
PuPlayer.playlistadd pBackglass,"videopolicekilled", 1 , 0
PuPlayer.playlistadd pBackglass,"videokilled", 1 , 0
PuPlayer.playlistadd pBackglass,"videomblock", 1 , 0
PuPlayer.playlistadd pBackglass,"videowizard", 1 , 0
PuPlayer.playlistadd pBackglass,"EndOfWizardMode", 1 , 0
PuPlayer.playlistadd pBackglass,"LostWizardMode", 1 , 2
PuPlayer.playlistadd pBackglass,"SkillShot", 1 , 0
PuPlayer.playlistadd pBackglass,"Highscore", 1 , 0
PuPlayer.playlistadd pTopper2,"topperintro", 1 , 0
PuPlayer.playlistadd pTopper2,"topperbase", 1 , 0
PuPlayer.playlistadd pTopper2,"topper", 1 , 0
PuPlayer.playlistadd pFullDMD,"fulldmdbase", 1 , 0
PuPlayer.playlistadd pFullDMD,"fulldmd", 1 , 0
End If

'♥♡♥♡♥♡♥♡♥♡♥♡♥♡♥ PLayer choices ♥♡♥♡♥♡♥♡♥♡♥♡♥♡♥

Const SongVolume = 0.3     ' 1 is full volume, but I set it quite low to listen better the other sounds since I use headphones, adjust to your setup :)

Const FlippersBlood = True 'set it to false if you don't like that. In this version 5 I moved the blood to the slingshots

Const FlexDMDHighQuality = True 'FlexDMD in high quality (True = LCD at 256x64) or normal quality (False = Real DMD at 128x32)

'♥♡♥♡♥♡♥♡♥♡♥♡♥ End of pLayer choices ♥♡♥♡♥♡♥♡♥♡♥♡♥


' Load the core.vbs for supporting Subs and functions
Const BallSize = 50        ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1        ' standard ball mass

LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    On Error Goto 0
End Sub

' Define any Constants
Const cGameName = "SCREAM"
Const myVersion = "1.0"
Const MaxPlayers = 4          ' from 1 to 4
Const BallSaverTime = 15     ' in seconds of the first ball
Const MaxMultiplier = 5      ' limit playfield multiplier
Const MaxBonusMultiplier = 5 'limit Bonus multiplier
Const BallsPerGame = 3       ' usually 3 or 5
Const MaxMultiballs = 13     ' max number of balls during multiballs

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = True
    Group1.visible = 0
    Casey1.visible = 0
    Tara1.visible = 0
    SidRandyGale1.visible = 0
    Group2.visible = 1
    Casey2.visible = 1
    Tara2.visible = 1
    SidRandyGale2.visible = 1
    Livingroom1.visible = 1
    Livingroom2.visible = 0
    Tatum1.visible = 1
    Tatum2.visible = 0
Else
    UseFlexDMD = True
     Group1.visible = 1
    Casey1.visible = 1
    Tara1.visible = 1
    SidRandyGale1.visible = 1
    Group2.visible = 0
    Casey2.visible = 0
    Tara2.visible = 0
    SidRandyGale2.visible = 0
    Livingroom1.visible = 0
    Livingroom2.visible = 1
    Tatum1.visible = 0
    Tatum2.visible = 1
End If

' Define Global Variables
Dim Audiovol
Dim calloutlowermusicvol
Dim soundtrackvol
Dim Topper2vol
Dim toppervideo
Dim useB2SBG
Dim videovol
Dim billyvol
Dim stuvol
Dim mickeyvol
Dim mloomisvol
Dim romanvol
Dim jillvol
Dim charlievol
Dim richievol
Dim ambervol
Dim quinnvol
Dim ethanvol
Dim waynevol
Dim deweyvol
Dim galevol
Dim sidvol
Dim taravol
Dim samvol
Dim mblockvol
Dim killedvol
Dim introvol
Dim drainvol
Dim wizardvol
Dim calloutvol
Dim rockmusic
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim PlayfieldMultiplier(4)
Dim PFxSeconds
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot(4)
Dim SuperJackpot(4)
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(4)
Dim SuperSkillshotValue(4)
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim x

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock(4)
Dim BallsInHole

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJackpot

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs
Dim mMagnet
Dim cbLeft    'captive ball at the magnet

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    Randomize
    'OldCellAprom.visible = 1

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 45 ' Plunger Power
    Const IMTime = 0.5        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Magnet
    Set mMagnet = New cvpmMagnet
    With mMagnet
        .InitMagnet Magnet, 40
        .GrabCenter = True
        .CreateEvents "mMagnet"
    End With

    Set cbLeft = New cvpmCaptiveBall
    With cbLeft
        .InitCaptive CapTrigger, MagnetPost, CapKicker, 0
        .ForceTrans = .7
        .MinForce = 3.5
        .CreateEvents "cbLeft"
        .Start
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Credits = 0
    Loadhs

    ' Initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = true 'we want coins

    if bFreePlay or Credits > 1 Then DOF 125, DOFOn

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    PFxSeconds = 0
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJackpot = False
    bInstantInfo = False
    SpecialTableStartUps
    ' set any lights for the attract mode
    GiOff
    StartAttractMode

    ' Start the RealTime timer
    RealTime.Enabled = 1

    ' Load table color
    LoadLut
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound SoundFX("fx_nudge",0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 8:PlaySound SoundFX("fx_nudge",0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound SoundFX("fx_nudge",0), 0, 1, 1, 0.25

    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If

    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        Credits = Credits + 1
        if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False) Then
            DMDFlush
            DMD "", CL("CREDITS " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then CheckTilt 'only check the tilt during game
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt
        If keycode = MechanicalTilt Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 1
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 0

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                Else
                    If(Credits > 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                        If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, "vo_givemeyourmoney"
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits > 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, "vo_givemeyourmoney"
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
    End If
End Sub

Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    If NOT hsbModeActive Then
        bInstantInfo = True
        DMDFlush
        InstantInfo
    End If
End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub Table1_Exit
    Savehs
    If UseFlexDMD Then FlexDMD.Run = False
    If B2SOn = true Then Controller.Stop
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled AND bFlippersEnabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipper001.RotateToEnd
        LeftFlipperOn = 1
    Else
            PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipper001.RotateToStart
        LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled AND bFlippersEnabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipperOn = 1
    Else
            PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'*********************************************************
' Real Time Flipper adjustments - by JLouLouLou & JPSalas
'        (to enable flipper tricks) 
'*********************************************************

Dim FlipperPower
Dim FlipperElasticity
Dim SOSTorque, SOSAngle
Dim FullStrokeEOS_Torque, LiveStrokeEOS_Torque
Dim LeftFlipperOn
Dim RightFlipperOn

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim LiveCatchSensivity

FlipperPower = 5000
FlipperElasticity = 0.8
FullStrokeEOS_Torque = 0.3 	' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2	' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

LeftFlipper.EOSTorqueAngle = 10
RightFlipper.EOSTorqueAngle = 10

SOSTorque = 0.1
SOSAngle = 6

LiveCatchSensivity = 10

LLiveCatchTimer = 0
RLiveCatchTimer = 0

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's tricks timer
'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
	If LeftFlipperOn = 1 Then
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
			LeftFlipper.EOSTorque = FullStrokeEOS_Torque
			LLiveCatchTimer = LLiveCatchTimer + 1
			If LLiveCatchTimer < LiveCatchSensivity Then
				LeftFlipper.Elasticity = 0
			Else
				LeftFlipper.Elasticity = FlipperElasticity
				LLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		LeftFlipper.Elasticity = FlipperElasticity
		LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
		LLiveCatchTimer = 0
	End If
	

'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
 	If RightFlipperOn = 1 Then
		If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
			RightFlipper.EOSTorque = FullStrokeEOS_Torque
			RLiveCatchTimer = RLiveCatchTimer + 1
			If RLiveCatchTimer < LiveCatchSensivity Then
				RightFlipper.Elasticity = 0
			Else
				RightFlipper.Elasticity = FlipperElasticity
				RLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		RightFlipper.Elasticity = FlipperElasticity
		RightFlipper.EOSTorque = LiveStrokeEOS_Torque
		RLiveCatchTimer = 0
	End If
End Sub

Dim RSplat, LSplat

Sub RightSplat
    RSplat = 0
    Rightblood_Timer
End Sub

Sub Rightblood_Timer
    Select Case RSplat
        Case 0:Rightblood.ImageA = "blood1":Rightblood.Visible = 1:Rightblood.TimerEnabled = 1
        Case 1:Rightblood.ImageA = "blood2"
        Case 2:Rightblood.ImageA = "blood3"
        Case 3:Rightblood.ImageA = "blood4"
        Case 4:Rightblood.ImageA = "blood5"
        Case 5:Rightblood.ImageA = "blood6"
        Case 6:Rightblood.Visible = 0:Rightblood.TimerEnabled = 0
    End Select
    RSplat = RSplat + 1
End Sub

Sub LeftSplat
    LSplat = 0
    Leftblood_Timer
End Sub

Sub Leftblood_Timer
    Select Case LSplat
        Case 0:Leftblood.ImageA = "blood1a":Leftblood.Visible = 1:Leftblood.TimerEnabled = 1
        Case 1:Leftblood.ImageA = "blood2a"
        Case 2:Leftblood.ImageA = "blood3a"
        Case 3:Leftblood.ImageA = "blood4a"
        Case 4:Leftblood.ImageA = "blood5a"
        Case 5:Leftblood.ImageA = "blood6a"
        Case 6:Leftblood.Visible = 0:Leftblood.TimerEnabled = 0
    End Select
    LSplat = LSplat + 1
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                       'Called when table is nudged
    Dim BOT
    BOT = GetBalls
    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub
    Tilt = Tilt + TiltSensitivity                   'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity) AND(Tilt <= 15) Then 'show a warning
        DMD "_", CL("CAREFUL"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End if
    If(NOT Tilted) AND Tilt > 15 Then 'If more that 15 then TILT the table
        'display Tilt
        InstantInfoTimer.Enabled = False
        DMDFlush
        DMD CL("YOU"), CL("TILTED"), "", eNone, eNone, eNone, 200, False, ""
        'PlaySound "vo_yousuck" &RndNbr(5)
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
        StopMBmodes
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        Tilted = True
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        LeftFlipper001.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        Tilted = False
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        vpmtimer.Addtimer 2000, "EndOfBall() '"
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'*****************************************
'         Music as wav sounds
' in VPX 10.7 you may use also mp3 or ogg
'*****************************************

Dim Song
Song = ""

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, SongVolume
        End If
    End If
End Sub

Sub ChangeSong
    If bJasonMBStarted Then
        PlaySong "mu_multiball1"
    ElseIf bFreddyMBStarted Then
        PlaySong "mu_multiball3"
    ElseIf bMichaelMBStarted Then
        PlaySong "mu_multiball2"
    ElseIf Mode(CurrentPLayer, 0) Then
        PlaySong "mu_pursuit"
    Else
        PlaySong "mu_main" &Balls
    End If
End Sub

Sub StopSong(name)
    StopSound name
End Sub

'********************
' Play random quotes
'********************

Sub PlayQuote 'Jason's mom
    PlaySound "vo_gf" &RndNbr(28)
    VOLight
End Sub

Sub PlayHighScoreQuote
    Select Case RndNbr(2)
        Case 1:PlaySound "vo_excellentscore"
        Case 2:PlaySound "vo_greatscore"
    End Select
End Sub

Sub PlayNotGoodScore
    Select Case RndNbr(6)
        Case 1:PlaySound "vo_heywhathappened"
        Case 2:PlaySound "vo_didyouscoreanypoints"
        Case 3:PlaySound "vo_youneedflipperskills"
        Case 4:PlaySound "vo_youmissedeverything"
        Case 5:PlaySound "vo_thatwasprettybad"
        Case 6:PlaySound "vo_thatwasprettybad2"
    End Select
End Sub

Sub PlayEndQuote
    Select Case RndNbr(2)
        Case 1:PlaySound "vo_seeyoulater"
        Case 2:PlaySound "vo_youredonebyebye"
    End Select
End Sub

Sub PlayThunder
    PlaySound "sfx_thunder" &RndNbr(7)
    ShowStrikes1
End Sub

Sub PlaySword
    PlaySound "sfx_sword" &RndNbr(5)
End Sub

Sub PlayKill
    PlaySound "sfx_kill" &RndNbr(10)
End Sub

Sub PlayElectro
    PlaySound "sfx_electro" &RndNbr(9)
End Sub


Sub PlayClawSwipe
    PlaySound "sfx_ClawSwipe" &RndNbr(7)
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

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Dim GiIntensity
GiIntensity = 1   'used for the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 0 Then '-1 means no balls, 0 is the first captive ball, 1 is the second captive ball...)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    PlaySoundAt "fx_GiOn", li036 'about the center of the table
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    PlaySoundAt "fx_GiOff", li036 'about the center of the table
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 40
            LightSeqGi.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqGi.UpdateInterval = 25
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqBlinking, , 10, 20
        Case 4 'seq up
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqUpOn, 25, 3
        Case 5 'seq down
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqDownOn, 25, 3
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 40
            LightSeqInserts.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqInserts.UpdateInterval = 25
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 20
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'center - used in the bonus count
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 15, 2
        Case 6 'down to top
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 15, 1
        Case 7 'center from the magnet
            LightSeqMG.UpdateInterval = 4
            LightSeqMG.Play SeqCircleOutOn, 15, 1
    End Select
End Sub

Sub FlashEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqFlashers.Play SeqAlloff
        Case 1 'all blink
            LightSeqFlashers.UpdateInterval = 40
            LightSeqFlashers.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqFlashers.UpdateInterval = 25
            LightSeqFlashers.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqFlashers.UpdateInterval = 20
            LightSeqFlashers.Play SeqBlinking, , 10, 20
        Case 4 'center
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqDownOn, 15, 1
        Case 6 'down to top
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqUpOn, 15, 1
        Case 7 'top flashers left right
            LightSeqTopFlashers.UpdateInterval = 10
            LightSeqTopFlashers.Play SeqRightOn, 50, 10
    End Select
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
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
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

Const tnob = 19   'total number of balls
Const lob = 1     'number of locked balls
Const maxvel = 40 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        ghostfaceBody.Rotz = +100 - (BOT(1).Y)\20
        ghostfaceMask.Rotz = +100 - (BOT(1).Y)\20
        GF001.Roty = -100 + (BOT(b).Y)\20
        GF002.Roty = -100 + (BOT(b).Y)\20
        GF003.Roty = -100 + (BOT(b).Y)\20
        GF004.Roty = +100 - (BOT(b).Y)\20
        GF005.Roty = +100 - (BOT(b).Y)\20
        GFChase.X = BOT(b).X
        GFChase.Y = BOT(b).Y
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z -Ballsize/2

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 3
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed & spin control
            BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory <1 Then
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
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'************************************
' Diverse Collection Hit Sounds v3.0
'************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

'extra collections in this table
Sub aBlueRubbers_Hit(idx)
    Select Case RndNbr(15)
        Case 1:'Playsound "vo_hahaha1"
        Case 2:'Playsound "vo_hahaha2"
        Case 3:'Playsound "vo_hahaha3"
        Case 4:'Playsound "vo_toobusytoaim"
        Case 5:'Playsound "vo_youmissedeverything"
        Case 6:'Playsound "vo_yousuck1"
        Case 7:'Playsound "vo_yousuck2"
    End Select
End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attract mode
    StopAttractMode
    GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        PlayfieldMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' initialise any other flags
    Tilt = 0

    ' initialise specific Game variables
    Game_Init()

    ' you may wish to start some music, play a sound, do whatever at this point

    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    DMDScoreNow

    ' set the current players bonus multiplier back down to 1X
    SetBonusMultiplier 1

    ' reduce the playfield multiplier
    DecreasePlayfieldMultiplier

    ' reset any drop targets, lights, game Mode etc..

    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False

    'Reset any table specific
    ResetNewBallVariables

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    bSkillShotReady = True

'Change the music ?
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield > 1 Then
        DOF 143, DOFPulse
        bMultiBallMode = True
        bAutoPlunger = True
    End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
    'and eject the first ball
    CreateMultiballTimer_Timer
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield < MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                CreateMultiballTimer.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
        End If
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 10 'yes 10 points :)
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.
    '(the tilt recovery mechanism will handle any extra balls or end of game)

    If NOT Tilted Then
        StopSong Song
        PlaySound "sfx_suspense"
        'Count the bonus. This table uses several bonus
        DMDFlush
        DMD CL("BONUS"), "", "", eNone, eNone, eNone, 1000, True, ""

        'Weapons collected X 1.500.000
        AwardPoints = Weapons(CurrentPlayer) * 1500000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("SCREAM FACTOR " & Weapons(CurrentPlayer) ), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, ""

        'Counselors killed X 750.000
        AwardPoints = CounselorsKilled(CurrentPlayer) * 750000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("GHOSTFACE KILLED"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, ""

        'Teenagers killed x 300.000
        AwardPoints = TeensKilled(CurrentPlayer) * 300000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("EVIDENCE FOUND"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, ""

        'Loops X 150.000
        AwardPoints = LoopHits(CurrentPlayer) * 150000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("LOOP COMBOS"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, ""

        'Combos X 150.000
        AwardPoints = ComboHits(CurrentPlayer) * 150000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("RAMP COMBOS"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, ""

        'Bumpers X 50.000
        AwardPoints = BumperHits(CurrentPlayer) * 50000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("BUMPER HITS"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, ""

        If TotalBonus > 5000000 Then
            DMD CL("TOTAL BONUS X MULT"), CL(FormatScore(TotalBonus * BonusMultiplier(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, True, "vo_heynicebonus"
        Else
            DMD CL("TOTAL BONUS X MULT"), CL(FormatScore(TotalBonus * BonusMultiplier(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, True, "vo_welldone"
        End If
        AddScore2 TotalBonus * BonusMultiplier(CurrentPlayer)

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 9000, "EndOfBall2 '"
    Else 'if tilted then only add a short delay and move to the 2nd part of the end of the ball
        vpmtimer.addtimer 100, "EndOfBall2 '"
    End If
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any Extra Ball light if there was any
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            LightShootAgain.State = 0
            LightShootAgain2.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, "vo_replay"

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
            ' debug.print "No More Balls, High Score Entry"
            ' Submit the CurrentPlayers score to the High Score system
            CheckHighScore()
        ' you may wish to play some music at this point

        Else

            ' not the last ball (for that player)
            ' if multiple players are playing then move onto the next one
            EndOfBallComplete()
        End If
    End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame > 1) Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer > PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode

        ' set the machine into game over mode
        EndOfGame()

    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        DMDScoreNow

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame > 1 Then
            Select Case CurrentPlayer
                Case 1:DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "vo_player1"
                Case 2:DMD "", CL("PLAYER 2"), "", eNone, eNone, eNone, 1000, True, "vo_player2"
                Case 3:DMD "", CL("PLAYER 3"), "", eNone, eNone, eNone, 1000, True, "vo_player3"
                Case 4:DMD "", CL("PLAYER 4"), "", eNone, eNone, eNone, 1000, True, "vo_player4"
            End Select
        Else
            DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "vo_youareup"
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    PlaySound "mu_death"
    vpmtimer.AddTimer 2500, "PlayEndQuote '"
    bgMusicMute
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
    StartAttractMode
    SpecialGameRestarts
' you may wish to light any Game Over Light you may have
End Sub

'this calculates the ball number in play
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function



'******************************
' Movie Screens End Animations
'******************************

Dim ScreamOneEX
ScreamOneEX=0

Sub ScreamOneSFX_Timer
    If ScreamOneSFX.enabled=true then ScreamOneEX=ScreamOneEX+1: End If
    If ScreamOneEX > 24 Then
          MovieScreen1Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamOneSFX.enabled=false
          ScreamOneEX=0
    End If
End Sub



Dim ScreamTwoEX
ScreamTwoEX=0

Sub ScreamTwoSFX_Timer
    If ScreamTwoSFX.enabled=true then ScreamTwoEX=ScreamTwoEX+1: End If
    If ScreamTwoEX > 45 Then
          MovieScreen2Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwoSFX.enabled=false
          ScreamTwoEX=0
    End If
End Sub


Dim ScreamThreeEX
ScreamThreeEX=0

Sub ScreamThreeSFX_Timer
    If ScreamThreeSFX.enabled=true then ScreamThreeEX=ScreamThreeEX+1: End If
    If ScreamThreeEX > 24 Then
          MovieScreen3Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamThreeSFX.enabled=false
          ScreamThreeEX=0
    End If
End Sub


Dim ScreamFourEX
ScreamFourEX=0

Sub ScreamFourSFX_Timer
    If ScreamFourSFX.enabled=true then ScreamFourEX=ScreamFourEX+1: End If
    If ScreamFourEX > 45 Then
          MovieScreen4Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamFourSFX.enabled=false
          ScreamFourEX=0
    End If
End Sub


Dim ScreamFiveEX
ScreamFiveEX=0

Sub ScreamFiveSFX_Timer
    If ScreamFiveSFX.enabled=true then ScreamFiveEX=ScreamFiveEX+1: End If
    If ScreamFiveEX > 24 Then
          MovieScreen5Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamFiveSFX.enabled=false
          ScreamFiveEX=0
    End If
End Sub


Dim ScreamSixEX
ScreamSixEX=0

Sub ScreamSixSFX_Timer
    If ScreamSixSFX.enabled=true then ScreamSixEX=ScreamSixEX+1: End If
    If ScreamSixEX > 45 Then
          MovieScreen6Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamSixSFX.enabled=false
          ScreamSixEX=0
    End If
End Sub


Dim ScreamSevenEX
ScreamSevenEX=0

Sub ScreamSevenSFX_Timer
    If ScreamSevenSFX.enabled=true then ScreamSevenEX=ScreamSevenEX+1: End If
    If ScreamSevenEX > 24 Then
          MovieScreen7Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamSevenSFX.enabled=false
          ScreamSevenEX=0
    End If
End Sub


Dim ScreamEightEX
ScreamEightEX=0

Sub ScreamEightSFX_Timer
    If ScreamEightSFX.enabled=true then ScreamEightEX=ScreamEightEX+1: End If
    If ScreamEightEX > 45 Then
          MovieScreen8Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamEightSFX.enabled=false
          ScreamEightEX=0
    End If
End Sub


Dim ScreamNineEX
ScreamNineEX=0

Sub ScreamNineSFX_Timer
    If ScreamNineSFX.enabled=true then ScreamNineEX=ScreamNineEX+1: End If
    If ScreamNineEX > 24 Then
          MovieScreen9Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamNineSFX.enabled=false
          ScreamNineEX=0
    End If
End Sub


Dim ScreamTenEX
ScreamTenEX=0

Sub ScreamTenSFX_Timer
    If ScreamTenSFX.enabled=true then ScreamTenEX=ScreamTenEX+1: End If
    If ScreamTenEX > 45 Then
          MovieScreen10Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTenSFX.enabled=false
          ScreamTenEX=0
    End If
End Sub


Dim ScreamElevenEX
ScreamElevenEX=0

Sub ScreamElevenSFX_Timer
    If ScreamElevenSFX.enabled=true then ScreamElevenEX=ScreamElevenEX+1: End If
    If ScreamElevenEX > 24 Then
          MovieScreen11Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamElevenSFX.enabled=false
          ScreamElevenEX=0
    End If
End Sub


Dim ScreamTwelveEX
ScreamTwelveEX=0

Sub ScreamTwelveSFX_Timer
    If ScreamTwelveSFX.enabled=true then ScreamTwelveEX=ScreamTwelveEX+1: End If
    If ScreamTwelveEX > 45 Then
          MovieScreen12Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwelveSFX.enabled=false
          ScreamTwelveEX=0
    End If
End Sub


Dim ScreamThirteenEX
ScreamThirteenEX=0

Sub ScreamThirtenSFX_Timer
    If ScreamThirteenSFX.enabled=true then ScreamThirteenEX=ScreamThirteenEX+1: End If
    If ScreamThirteenEX > 24 Then
          MovieScreen13Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamThirteenSFX.enabled=false
          ScreamThirteenEX=0
    End If
End Sub


Dim ScreamFourteenEX
ScreamFourteenEX=0

Sub ScreamFourteenSFX_Timer
    If ScreamFourteenSFX.enabled=true then ScreamFourteenEX=ScreamFourteenEX+1: End If
    If ScreamFourteenEX > 45 Then
          MovieScreen14Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamFourteenSFX.enabled=false
          ScreamFourteenEX=0
    End If
End Sub


Dim ScreamFifteenEX
ScreamFifteenEX=0

Sub ScreamFifteenSFX_Timer
    If ScreamFifteenSFX.enabled=true then ScreamFifteenEX=ScreamFifteenEX+1: End If
    If ScreamFifteenEX > 24 Then
          MovieScreen15Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamFifteenSFX.enabled=false
          ScreamFifteenEX=0
    End If
End Sub


Dim ScreamSixteenEX
ScreamSixteenEX=0

Sub ScreamSixteenSFX_Timer
    If ScreamSixteenSFX.enabled=true then ScreamSixteenEX=ScreamSixteenEX+1: End If
    If ScreamSixteenEX > 45 Then
          MovieScreen16Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamSixteenSFX.enabled=false
          ScreamSixteenEX=0
    End If
End Sub


Dim ScreamSeventeenEX
ScreamSeventeenEX=0

Sub ScreamSeventeenSFX_Timer
    If ScreamSeventeenSFX.enabled=true then ScreamSeventeenEX=ScreamSeventeenEX+1: End If
    If ScreamSeventeenEX > 24 Then
          MovieScreen17Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamSeventeenSFX.enabled=false
          ScreamSeventeenEX=0
    End If
End Sub


Dim ScreamEightteenEX
ScreamEightteenEX=0

Sub ScreamEightteenSFX_Timer
    If ScreamEightteenSFX.enabled=true then ScreamEightteenEX=ScreamEightteenEX+1: End If
    If ScreamEightteenEX > 45 Then
          MovieScreen18Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamEightteenSFX.enabled=false
          ScreamEightteenEX=0
    End If
End Sub


Dim ScreamNineteenEX
ScreamNineteenEX=0

Sub ScreamNineteenSFX_Timer
    If ScreamNineteenSFX.enabled=true then ScreamNineteenEX=ScreamNineteenEX+1: End If
    If ScreamNineteenEX > 24 Then
          MovieScreen19Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamNineteenSFX.enabled=false
          ScreamNineteenEX=0
    End If
End Sub


Dim ScreamTwentyEX
ScreamTwentyEX=0

Sub ScreamTwentySFX_Timer
    If ScreamTwentySFX.enabled=true then ScreamTwentyEX=ScreamTwentyEX+1: End If
    If ScreamTwentyEX > 45 Then
          MovieScreen20Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwentySFX.enabled=false
          ScreamTwentyEX=0
    End If
End Sub


Dim ScreamTwentyoneEX
ScreamTwentyoneEX=0

Sub ScreamTwnetyoneSFX_Timer
    If ScreamTwentyoneSFX.enabled=true then ScreamTwentyoneEX=ScreamTwentyoneEX+1: End If
    If ScreamTwentyoneEX > 24 Then
          MovieScreen21Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwentyoneSFX.enabled=false
          ScreamTwentyoneEX=0
    End If
End Sub


Dim ScreamTwentytwoEX
ScreamTwentytwoEX=0

Sub ScreamTwentytwoSFX_Timer
    If ScreamTwentytwoSFX.enabled=true then ScreamTwentytwoEX=ScreamTwentytwoEX+1: End If
    If ScreamTwentytwoEX > 45 Then
          MovieScreen22Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwentytwoSFX.enabled=false
          ScreamTwentytwoEX=0
    End If
End Sub


Dim ScreamTwentyThreeEX
ScreamTwentyThreeEX=0

Sub ScreamTwnetyThreeSFX_Timer
    If ScreamTwentyThreeSFX.enabled=true then ScreamTwentyThreeEX=ScreamTwentyThreeEX+1: End If
    If ScreamTwentyThreeEX > 24 Then
          MovieScreen23Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwentyThreeSFX.enabled=false
          ScreamTwentyThreeEX=0
    End If
End Sub


Dim ScreamTwentyFourEX
ScreamTwentyFourEX=0

Sub ScreamTwentyFourSFX_Timer
    If ScreamTwentyFourSFX.enabled=true then ScreamTwentyFourEX=ScreamTwentyFourEX+1: End If
    If ScreamTwentyFourEX > 45 Then
          MovieScreen24Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwentyFourSFX.enabled=false
          ScreamTwentyFourEX=0
    End If
End Sub


Dim ScreamTwentyFiveEX
ScreamTwentyFiveEX=0

Sub ScreamTwentyFiveSFX_Timer
    If ScreamTwentyFiveSFX.enabled=true then ScreamTwentyFiveEX=ScreamTwentyFiveEX+1: End If
    If ScreamTwentyFiveEX > 24 Then
          MovieScreen25Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwentyFiveSFX.enabled=false
          ScreamTwentyFiveEX=0
    End If
End Sub


Dim ScreamTwentySixEX
ScreamTwentySixEX=0

Sub ScreamTwentySixSFX_Timer
    If ScreamTwentySixSFX.enabled=true then ScreamTwentySixEX=ScreamTwentySixEX+1: End If
    If ScreamTwentySixEX > 45 Then
          MovieScreen26Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwentySixSFX.enabled=false
          ScreamTwentySixEX=0
    End If
End Sub


Dim ScreamTwentySevenEX
ScreamTwentySevenEX=0

Sub ScreamTwentySevenSFX_Timer
    If ScreamTwentySevenSFX.enabled=true then ScreamTwentySevenEX=ScreamTwentySevenEX+1: End If
    If ScreamTwentySevenEX > 24 Then
          MovieScreen27Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwentySevenSFX.enabled=false
          ScreamTwentySevenEX=0
    End If
End Sub


Dim ScreamTwentyEightEX
ScreamTwentyEightEX=0

Sub ScreamTwentyEightSFX_Timer
    If ScreamTwentyEightSFX.enabled=true then ScreamTwentyEightEX=ScreamTwentyEightEX+1: End If
    If ScreamTwentyEightEX > 45 Then
          MovieScreen28Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwentyEightSFX.enabled=false
          ScreamTwentyEightEX=0
    End If
End Sub


Dim ScreamTwentyNineEX
ScreamTwentyNineEX=0

Sub ScreamTwentyNineSFX_Timer
    If ScreamTwentyNineSFX.enabled=true then ScreamTwentyNineEX=ScreamTwentyNineEX+1: End If
    If ScreamTwentyNineEX > 24 Then
          MovieScreen29Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamTwentyNineSFX.enabled=false
          ScreamTwentyNineEX=0
    End If
End Sub


Dim ScreamThirtyEX
ScreamThirtyEX=0

Sub ScreamThirtySFX_Timer
    If ScreamThirtySFX.enabled=true then ScreamThirtyEX=ScreamThirtyEX+1: End If
    If ScreamThirtyEX > 45 Then
          MovieScreen30Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamThirtySFX.enabled=false
          ScreamThirtyEX=0
    End If
End Sub


Dim ScreamThirtyOneEX
ScreamThirtyOneEX=0

Sub ScreamThirtyOneSFX_Timer
    If ScreamThirtyOneSFX.enabled=true then ScreamThirtyOneEX=ScreamThirtyOneEX+1: End If
    If ScreamThirtyOneEX > 24 Then
          MovieScreen31Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamThirtyOneSFX.enabled=false
          ScreamThirtyOneEX=0
    End If
End Sub


Dim ScreamThirtyTwoEX
ScreamThirtyTwoEX=0

Sub ScreamThirtyTwoSFX_Timer
    If ScreamThirtyTwoSFX.enabled=true then ScreamThirtyTwoEX=ScreamThirtyTwoEX+1: End If
    If ScreamThirtyTwoEX > 45 Then
          MovieScreen32Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamThirtyTwoSFX.enabled=false
          ScreamThirtyTwoEX=0
    End If
End Sub


Dim ScreamThirtyThreeEX
ScreamThirtyThreeEX=0

Sub ScreamThirtyThreeSFX_Timer
    If ScreamThirtyThreeSFX.enabled=true then ScreamThirtyThreeEX=ScreamThirtyThreeEX+1: End If
    If ScreamThirtyThreeEX > 24 Then
          MovieScreen33Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamThirtyThreeSFX.enabled=false
          ScreamThirtyThreeEX=0
    End If
End Sub


Dim ScreamThirtyFourEX
ScreamThirtyFourEX=0

Sub ScreamThirtyFourSFX_Timer
    If ScreamThirtyFourSFX.enabled=true then ScreamThirtyFourEX=ScreamThirtyFourEX+1: End If
    If ScreamThirtyFourEX > 45 Then
          MovieScreen34Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamThirtyFourSFX.enabled=false
          ScreamThirtyFourEX=0
    End If
End Sub


Dim ScreamThirtyFiveEX
ScreamThirtyFiveEX=0

Sub ScreamThirtyFiveSFX_Timer
    If ScreamThirtyFiveSFX.enabled=true then ScreamThirtyFiveEX=ScreamThirtyFiveEX+1: End If
    If ScreamThirtyFiveEX > 24 Then
          MovieScreen35Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamThirtyFiveSFX.enabled=false
          ScreamThirtyFiveEX=0
    End If
End Sub


Dim ScreamThirtySixEX
ScreamThirtySixEX=0

Sub ScreamThirtySixSFX_Timer
    If ScreamThirtySixSFX.enabled=true then ScreamThirtySixEX=ScreamThirtySixEX+1: End If
    If ScreamThirtySixEX > 45 Then
          MovieScreen36Animation.Enabled = True
          Movie1Light.Enabled = True
          ScreamThirtySixSFX.enabled=false
          ScreamThirtySixEX=0
    End If
End Sub


' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
    ' Destroy the ball
    Drain.DestroyBall
    GFTiltDrain
    'GFDrain
    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball
    ' Exit Sub ' only for debugging - this way you can add balls from the debug window

    BallsOnPlayfield = BallsOnPlayfield - 1

    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain
    'if Tilted the end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            ' stop the ballsaver timer during the launch ball saver time, but not during multiballs
            If NOT bMultiBallMode Then
                DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 2500, True, "vo_giveballback"
                BallSaverTimerExpired_Timer
            End If
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True) then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    'changesong
                    ' turn off any multiball specific lights
                    ChangeGi white
                    ChangeGIIntensity 1
                    'stop any multiball modes of this game
                    StopMBmodes
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
                ' End Mode and timers
                StopSong Song
                ChangeGi white
                ChangeGIIntensity 1
                GFDrain
                GFTiltDrain
                GFModeTarget001.Isdropped = 1
                GFModeTarget.Isdropped = 1
                GF006.Z =-90
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, if there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
    PlaySoundAt "fx_sensor", swPlungerRest
    DOF 208, DOFOn
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2
    ' be sure to update the Scoreboard after the animations, if any
    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        vpmtimer.addtimer 1500, "PlungerIM.AutoFire:DOF 120, DOFPulse:DOF 124, DOFPulse:PlaySoundAt SoundFX(""fx_kicker"",DOFContactors), swPlungerRest:bAutoPlunger = False '"
    End If
    'Start the skillshot lights & variables if any
    If bSkillShotReady Then
        'PlaySong "mu_wait"
        UpdateSkillshot()
        ' show the message to shoot the ball in case the player has fallen sleep
        swPlungerRest.TimerEnabled = 1
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    lighteffect 6
    bBallInPlungerLane = False
    DOF 208, DOFOff
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        'ChangeSong
        ResetSkillShotTimer.Enabled = 1
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    End If
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds
Sub swPlungerRest_Timer
    IF bOnTheFirstBall Then
        Select Case RndNbr(3)
            Case 1:DMD CL("BACK FOR SOME"), CL("MORE TORTURE"), "_", eNone, eNone, eNone, 2000, True, "vo_backforsomemoretorture"
            Case 2:DMD CL("I KNEW YOU"), CL("YOU WOULD BE BACK"), "_", eNone, eNone, eNone, 2000, True, "vo_Iknewyoullbeback"
            Case 3:DMD CL("HEY YOU"), CL("SHOOT THE BALL"), "_", eNone, eNone, eNone, 2000, True, "vo_shoothereandhere"
        End Select
    Else
        Select Case RndNbr(2)
            Case 1:DMD CL("TIME TO"), CL("WAKE UP"), "_", eNone, eNone, eNone, 2000, True, "vo_timetowakeup"
            Case 2:DMD CL("HEY"), CL("PULL THE PLUNGER"), "_", eNone, eNone, eNone, 2000, True, "vo_heypulltheplunger"
        End Select
    End If
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
    LightShootAgain2.BlinkInterval = 160
    LightShootAgain2.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimerExpired_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimerExpired.Enabled = False
    BallSaverSpeedUpTimer.Enabled = False 'ensure this timer is also stopped
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
    LightShootAgain2.State = 0
    ' if the table uses the same lights for the extra ball or replay then turn them on if needed
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        LightShootAgain.State = 1
        LightShootAgain2.State = 1
    End If
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
    LightShootAgain2.BlinkInterval = 80
    LightShootAgain2.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board

Sub AddScore(points) 'normal score routine
    If Tilted Then Exit Sub
    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
' you may wish to check to see if the player has gotten a replay
End Sub

Sub AddScore2(points) 'used in jackpots, skillshots, combos, and bonus as it does not use the PlayfieldMultiplier
    If Tilted Then Exit Sub
    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If Tilted Then Exit Sub
    ' add the bonus to the current players bonus variable
    BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If Tilted Then Exit Sub

    ' If(bMultiBallMode = True) Then
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
    DMDFlush
    DMD "_", CL("INCREASED JACKPOT"), "_", eNone, eNone, eNone, 1000, True, ""
' you may wish to limit the jackpot to a upper limit, ie..
'	If (Jackpot >= 6000000) Then
'		Jackpot = 6000000
' 	End if
'End if
End Sub

Sub AddSuperJackpot(points) 'not used in this table
    If Tilted Then Exit Sub
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxBonusMultiplier) then
        ' then add and set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
        DMDFlush
        DMD "_", CL("SCREAM X " &NewBonusLevel), "_", eNone, eBlink, eNone, 2000, True, ""
    Else
        AddScore2 500000
        DMDFlush
        DMD "_", CL("500000"), "_", eNone, eNone, eNone, 1000, True, ""
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly

Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level
    UpdateBonusXLights(Level)
End Sub

Sub UpdateBonusXLights(Level) '4 lights in this table, from 2x to 5x
    ' Update the lights
    Select Case Level
        Case 1:li021.State = 0:li022.State = 0:li023.State = 0:li024.State = 0
        Case 2:li021.State = 1:li022.State = 0:li023.State = 0:li024.State = 0
        Case 3:li021.State = 1:li022.State = 1:li023.State = 0:li024.State = 0
        Case 4:li021.State = 1:li022.State = 1:li023.State = 1:li024.State = 0
        Case 5:li021.State = 1:li022.State = 1:li023.State = 1:li024.State = 1
    End Select
End Sub

Sub AddPlayfieldMultiplier(n)
    Dim snd
    Dim NewPFLevel
    ' if not at the maximum level x
    if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier) then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
        SetPlayfieldMultiplier(NewPFLevel)
        PlayThunder
        DMDFlush
        DMD "_", CL("SCREAM X " &NewPFLevel), "_", eNone, eBlink, eNone, 2000, True, snd
        LightEffect 4
        ' Play a voice sound
        Select Case NewPFLevel
            Case 2:PlaySound ""
            Case 3:PlaySound ""
            Case 4:PlaySound ""
            Case 5:PlaySound ""
        End Select
    Else 'if the max is already lit
        AddScore2 500000
        DMDFlush
        DMD "_", CL("500000"), "_", eNone, eNone, eNone, 2000, True, ""
    End if
    ' restart the PlayfieldMultiplier timer to reduce the multiplier
    PFXTimer.Enabled = 0
    PFXTimer.Enabled = 1
End Sub

Sub PFXTimer_Timer
    DecreasePlayfieldMultiplier
End Sub

Sub DecreasePlayfieldMultiplier 'reduces by 1 the playfield multiplier
    Dim NewPFLevel
    ' if not at 1 already
    if(PlayfieldMultiplier(CurrentPlayer) > 1) then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) - 1
        SetPlayfieldMultiplier(NewPFLevel)
    Else
        PFXTimer.Enabled = 0
    End if
End Sub

' Set the Playfield Multiplier to the specified level AND set any lights accordingly

Sub SetPlayfieldMultiplier(Level)
    ' Set the multiplier to the specified level
    PlayfieldMultiplier(CurrentPlayer) = Level
    UpdatePFXLights(Level)
End Sub

Sub UpdatePFXLights(Level) '4 lights in this table, from 2x to 5x
    ' Update the playfield multiplier lights
    Select Case Level
        Case 1:li025.State = 0:li026.State = 0:li027.State = 0:li027.State = 0
        Case 2:li025.State = 1:li026.State = 0:li027.State = 0:li027.State = 0
        Case 3:li025.State = 0:li026.State = 1:li027.State = 0:li027.State = 0
        Case 4:li025.State = 0:li026.State = 0:li027.State = 1:li027.State = 0
        Case 5:li025.State = 0:li026.State = 0:li027.State = 0:li027.State = 1
    End Select
' perhaps show also the multiplier in the DMD?
End Sub

Sub AwardExtraBall()
    '   If NOT bExtraBallWonThisBall Then 'in this table you can win several extra balls
    DMDFlush
    DMD "_", CL("EXTRA BALL WON"), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    DOF 124, DOFPulse
    PLaySound ""
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    'bExtraBallWonThisBall = True
    LightShootAgain.State = 1  'light the shoot again lamp
    LightShootAgain2.State = 1 'light the shoot again lamp
    GiEffect 2
    LightEffect 2
'    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL("EXTRA GAME WON"), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    DOF 124, DOFPulse
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardJackpot() 'only used for the final mode
    DMDFlush
    DMD CL("JACKPOT"), CL(FormatScore(Jackpot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1500, True, "" 
    DOF 126, DOFPulse
    AddScore2 Jackpot(CurrentPlayer)
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + 100000
    LightEffect 2
    GiEffect 2
    FlashEffect 2
End Sub

Sub AwardTargetJackpot() 'award a target jackpot after hitting all targets
    DMDFlush
    DMD CL("FILM JACKPOT"), CL(FormatScore(TargetJackpot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1500, True, "" 
    DOF 126, DOFPulse
    AddScore2 TargetJackpot(CurrentPlayer)
    TargetJackpot(CurrentPlayer) = TargetJackpot(CurrentPlayer) + 150000
    BlockerLightsBlueOff
    li069.State = 0
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardSuperJackpot() 'not used in this table as there are several superjackpots but I keep it as a reference
    DMDFlush
    DMD CL("SUPER JACKPOT"), CL(FormatScore(SuperJackpot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_superjackpot"
    DOF 126, DOFPulse
    AddScore2 SuperJackpot(CurrentPlayer)
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardWeaponsSuperJackpot()
    DMDFlush
    DMD CL("SUPR JACKPOT"), CL(FormatScore(WeaponSJValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_superjackpot"
    DOF 126, DOFPulse
    AddScore2 WeaponSJValue(CurrentPlayer)
    WeaponSJValue(CurrentPlayer) = WeaponSJValue(CurrentPlayer) + ((Score(CurrentPlayer) * 0.2) \ 10) * 10 'increase the weapons score with 20%
    aWeaponSJactive = False
    li060.State = 0
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMDFlush
    DMD CL("SKILLSHOT"), CL(FormatScore(SkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, False, "sfx_scare"
    DMD CL("SKILLSHOT"), CL(FormatScore(SkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, True, "vo_greatshot"
    DOF 127, DOFPulse
    Addscore2 SkillShotValue(CurrentPlayer)
    ' increment the skillshot value with 100.000
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 100000
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub

Sub AwardSuperSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMDFlush
    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, False, "sfx_scare"
    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, True, "vo_excellentshot"
    DOF 127, DOFPulse
    Addscore2 SuperSkillshotValue(CurrentPlayer)
    ' increment the superskillshot value with 1.000.000
    SuperSkillshotValue(CurrentPlayer) = SuperSkillshotValue(CurrentPlayer) + 1000000
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub

Sub aSkillshotTargets_Hit(idx) 'stop the skillshot if any other target is hit
    If bSkillshotReady then ResetSkillShotTimer_Timer
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 100000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 100000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 100000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(cGameName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue cGameName, "HighScore1", HighScore(0)
    SaveValue cGameName, "HighScore1Name", HighScoreName(0)
    SaveValue cGameName, "HighScore2", HighScore(1)
    SaveValue cGameName, "HighScore2Name", HighScoreName(1)
    SaveValue cGameName, "HighScore3", HighScore(2)
    SaveValue cGameName, "HighScore3Name", HighScoreName(2)
    SaveValue cGameName, "HighScore4", HighScore(3)
    SaveValue cGameName, "HighScore4Name", HighScoreName(3)
    SaveValue cGameName, "Credits", Credits
    SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
    HighScoreName(0) = "AAA"
    HighScoreName(1) = "BBB"
    HighScoreName(2) = "CCC"
    HighScoreName(3) = "DDD"
    HighScore(0) = 1500000
    HighScore(1) = 1400000
    HighScore(2) = 1300000
    HighScore(3) = 1200000
    Savehs
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
    Dim tmp
    tmp = Score(CurrentPlayer)

    If tmp > HighScore(0) Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
        DOF 125, DOFOn
    End If

    If tmp > HighScore(3) Then
        PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        HighScore(3) = tmp
        PlayHighScoreQuote
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
        PlayNotGoodScore
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    PlaySound "vo_enterinitials"
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow
    hsCurrentLetter = 1
    DMDFlush()
    HighScoreDisplayNameNow()

    HighScoreFlashTimer.Interval = 250
    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit > 0) then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayNameNow()
        end if
    end if
End Sub

Sub HighScoreDisplayNameNow()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreDisplayName()
    Dim i
    Dim TempTopStr
    Dim TempBotStr

    TempTopStr = "YOUR NAME:"
    dLine(0) = ExpandLine(TempTopStr)
    DMDUpdate 0

    TempBotStr = "    > "
    if(hsCurrentDigit > 0) then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit > 1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit > 2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit < 1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit < 2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr)
    DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2) then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
    EndOfBallComplete()
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) < HighScore(j + 1) Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
End Sub

'************************************
'       LUT - Darkness control
' 10 normal level & 10 warmer levels 
'************************************

Dim bLutActive, LUTImage

Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 22:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0"
        Case 1:table1.ColorGradeImage = "LUT1"
        Case 2:table1.ColorGradeImage = "LUT2"
        Case 3:table1.ColorGradeImage = "LUT3"
        Case 4:table1.ColorGradeImage = "LUT4"
        Case 5:table1.ColorGradeImage = "LUT5"
        Case 6:table1.ColorGradeImage = "LUT6"
        Case 7:table1.ColorGradeImage = "LUT7"
        Case 8:table1.ColorGradeImage = "LUT8"
        Case 9:table1.ColorGradeImage = "LUT9"
        Case 10:table1.ColorGradeImage = "LUT10"
        Case 11:table1.ColorGradeImage = "LUT Warm 0"
        Case 12:table1.ColorGradeImage = "LUT Warm 1"
        Case 13:table1.ColorGradeImage = "LUT Warm 2"
        Case 14:table1.ColorGradeImage = "LUT Warm 3"
        Case 15:table1.ColorGradeImage = "LUT Warm 4"
        Case 16:table1.ColorGradeImage = "LUT Warm 5"
        Case 17:table1.ColorGradeImage = "LUT Warm 6"
        Case 18:table1.ColorGradeImage = "LUT Warm 7"
        Case 19:table1.ColorGradeImage = "LUT Warm 8"
        Case 20:table1.ColorGradeImage = "LUT Warm 9"
        Case 21:table1.ColorGradeImage = "LUT Warm 10"
    End Select
End Sub

' New LUT postit
Function GetHSChar(String, Index)
    Dim ThisChar
    Dim FileName
    ThisChar = Mid(String, Index, 1)
    FileName = "PostIt"
    If ThisChar = " " or ThisChar = "" then
        FileName = FileName & "BL"
    ElseIf ThisChar = "<" then
        FileName = FileName & "LT"
    ElseIf ThisChar = "_" then
        FileName = FileName & "SP"
    Else
        FileName = FileName & ThisChar
    End If
    GetHSChar = FileName
End Function

Sub SetLUTLine(String)
    Dim Index
    Dim xFor
    Index = 1
    LUBack.imagea="PostItNote"
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
SetLUTLine ""
LUBack.imagea="PostitBL"
End Sub

' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text.
' 1st and 2nd lines are 20 characters long
' 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dLine(2)
Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Dim FlexDMD
Dim DMDScene

Sub DMD_Init() 'default/startup values
    If UseFlexDMD Then
        Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
        If Not FlexDMD is Nothing Then
            If FlexDMDHighQuality Then
                FlexDMD.TableFile = Table1.Filename & ".vpx"
                FlexDMD.RenderMode = 2
                FlexDMD.Width = 256
                FlexDMD.Height = 64
                FlexDMD.Clear = True
                FlexDMD.GameName = cGameName
                FlexDMD.Run = True
                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 12, 22
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + (i - 20) * 12, 34, 12, 22
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            Else
                FlexDMD.TableFile = Table1.Filename & ".vpx"
                FlexDMD.RenderMode = 2
                FlexDMD.Width = 128
                FlexDMD.Height = 32
                FlexDMD.Clear = True
                FlexDMD.GameName = cGameName
                FlexDMD.Run = True
                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 6, 11
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + (i - 20) * 6, 17, 6, 11
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            End If
        End If
    End If

    Dim i, j
    DMDFlush()
    deSpeed = 20
    deBlinkSlowRate = 10
    deBlinkFastRate = 5
    For i = 0 to 2
        dLine(i) = Space(20)
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    dLine(2) = " "
    For i = 0 to 2
        For j = 0 to 64
            dqText(i, j) = ""
            dqEffect(i, j) = eNone
        Next
    Next
    DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
    Dim i
    DMDTimer.Enabled = False
    DMDEffectTimer.Enabled = False
    dqHead = 0
    dqTail = 0
    For i = 0 to 2
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
    Next
End Sub

Sub DMDScore()
    Dim tmp, tmp1, tmp1a, tmp1b, tmp2
    if(dqHead = dqTail) Then
        ' default when no modes are active
        tmp = RL(FormatScore(Score(Currentplayer) ) )
        tmp1 = FL("PLAYER " &CurrentPlayer, "BALL " & Balls)
        'back image
        If bJasonMBStarted Then
            tmp2 = "d_jason"
        ElseIf bFreddyMBStarted Then
            tmp2 = "d_freddy"
        ElseIf bMichaelMBStarted Then
            tmp2 = "d_michael"
        Else
            tmp2 = "d_border"
        End If
        'info on the second line
        Select Case Mode(CurrentPlayer, 0)
            Case 0: 'no Mode active
                If bTommyStarted Then
                    tmp1 = "SHOOT THE RIGHT RAMP"
                ElseIf bPoliceStarted Then
                    tmp1 = CL("HIT THE POLICE")
                End If
            Case 1: 'spinners
                If Not ReadyToKill Then
                    tmp1 = FL("SPINNERS LEFT", SpinNeeded-SpinCount)
                Else
                    tmp1 = CL("SHOOT THE SCOOP")
                End If
            Case 2:
                If Not ReadyToKill Then
                    tmp1 = FL("HITS LEFT", 6-TargetModeHits)
                Else
                    tmp1 = CL("SHOOT THE MAGNET")
                End If
            Case 3:
                If Not ReadyToKill Then
                    tmp1 = FL("HITS LEFT", 5-TargetModeHits)
                Else
                    tmp1 = CL("SHOOT THE SCOOP")
                End If
            Case 4:tmp1 = FL("HITS LEFT", 9-TargetModeHits)
            Case 5:
                If Not ReadyToKill Then
                    tmp1 = FL("HITS LEFT", 9-TargetModeHits)
                Else
                    tmp1 = CL("SHOOT THE BASEMENT")
                End If
            Case 6:tmp1 = FL("HITS LEFT", 5-TargetModeHits)
            Case 7:tmp1 = FL("HITS LEFT", 6-TargetModeHits)
            Case 8:tmp1 = FL("HITS LEFT", 5-TargetModeHits)
            Case 9:tmp1 = FL("HITS LEFT", 9-TargetModeHits)
            Case 10:tmp1 = FL("HITS LEFT", 9-TargetModeHits)
            Case 11:tmp1 = FL("HITS LEFT", 6-TargetModeHits)
            Case 12:tmp1 = FL("SPINNERS LEFT", SpinNeeded-SpinCount)
            Case 13:tmp1 = FL("HITS LEFT", 6-TargetModeHits)
            Case 14:tmp1 = FL("HITS LEFT", 6-TargetModeHits)
            Case 15:tmp1 = CL("SHOOT JACKPOTS")
        End Select
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail < dqSize) Then
        if(Text0 = "_") Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0)
        End If

        if(Text1 = "_") Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1)
        End If

        if(Text2 = "_") Then
            dqEffect(2, dqTail) = eNone
            dqText(2, dqTail) = "_"
        Else
            dqEffect(2, dqTail) = Effect2
            dqText(2, dqTail) = Text2 'it is always 1 letter in this table
        End If

        dqTimeOn(dqTail) = TimeOn
        dqbFlush(dqTail) = bFlush
        dqSound(dqTail) = Sound
        dqTail = dqTail + 1
        if(dqTail = 1) Then
            DMDHead()
        End If
    End If
End Sub

Sub DMDHead()
    Dim i
    deCount(0) = 0
    deCount(1) = 0
    deCount(2) = 0
    DMDEffectTimer.Interval = deSpeed

    For i = 0 to 2
        Select Case dqEffect(i, dqHead)
            Case eNone:deCountEnd(i) = 1
            Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead) )
            Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead) )
            Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
            Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
        End Select
    Next
    if(dqSound(dqHead) <> "") Then
        PlaySound(dqSound(dqHead) )
    End If
    DMDEffectTimer.Enabled = True
End Sub

Sub DMDEffectTimer_Timer()
    DMDEffectTimer.Enabled = False
    DMDProcessEffectOn()
End Sub

Sub DMDTimer_Timer()
    Dim Head
    DMDTimer.Enabled = False
    Head = dqHead
    dqHead = dqHead + 1
    if(dqHead = dqTail) Then
        if(dqbFlush(Head) = True) Then
            DMDScoreNow()
        Else
            dqHead = 0
            DMDHead()
        End If
    Else
        DMDHead()
    End If
End Sub

Sub DMDProcessEffectOn()
    Dim i
    Dim BlinkEffect
    Dim Temp

    BlinkEffect = False

    For i = 0 to 2
        if(deCount(i) <> deCountEnd(i) ) Then
            deCount(i) = deCount(i) + 1

            select case(dqEffect(i, dqHead) )
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), 19)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), 21 - deCount(i), 1)
                    Temp = Temp & Left(dLine(i), 19)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkSlowRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkFastRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
                    End If
            End Select

            if(dqText(i, dqHead) <> "_") Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next

    if(deCount(0) = deCountEnd(0) ) and(deCount(1) = deCountEnd(1) ) and(deCount(2) = deCountEnd(2) ) Then

        if(dqTimeOn(dqHead) = 0) Then
            DMDFlush()
        Else
            if(BlinkEffect = True) Then
                DMDTimer.Interval = 10
            Else
                DMDTimer.Interval = dqTimeOn(dqHead)
            End If

            DMDTimer.Enabled = True
        End If
    Else
        DMDEffectTimer.Enabled = True
    End If
End Sub

Function ExpandLine(TempStr) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(20)
    Else
        if Len(TempStr) > Space(20) Then
            TempStr = Left(TempStr, Space(20) )
        Else
            if(Len(TempStr) < 20) Then
                TempStr = TempStr & Space(20 - Len(TempStr) )
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

    NumString = CStr(abs(Num) )

    For i = Len(NumString) -3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1) ) then
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1) ) + 48) & right(NumString, Len(NumString) - i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    If Len(NumString1) + Len(NumString2) <20 Then
    Temp = 20 - Len(NumString1) - Len(NumString2)
    TempStr = NumString1 & Space(Temp) & NumString2
    FL = TempStr
   End If
End Function

Function CL(NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString)> 20 Then NumString = Left(NumString, 20)
    Temp = (20 - Len(NumString) ) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(NumString) 'right line
    Dim Temp, TempStr
    If Len(NumString)> 20 Then NumString = Left(NumString, 20)
    Temp = 20 - Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
End Function

'**************
' Update DMD
'**************

Sub DMDUpdate(id)
    Dim digit, value
    If UseFlexDMD Then FlexDMD.LockRenderThread
    Select Case id
        Case 0 'top text line
            For digit = 0 to 19
                DMDDisplayChar mid(dLine(0), digit + 1, 1), digit
            Next
        Case 1 'bottom text line
            For digit = 20 to 39
                DMDDisplayChar mid(dLine(1), digit -19, 1), digit
            Next
        Case 2 ' back image - back animations
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "d_border"
            Digits(40).ImageA = dLine(2)
            If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
    End Select
    If UseFlexDMD Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
    If achar = "" Then achar = " "
    achar = ASC(achar)
    Digits(adigit).ImageA = Chars(achar)
    If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
End Sub

'****************************
' JP's new DMD using flashers
'****************************

Dim Digits, Chars(255), Images(255)

DMDInit

Sub DMDInit
    Dim i
    Digits = Array(digit001, digit002, digit003, digit004, digit005, digit006, digit007, digit008, digit009, digit010, _
        digit011, digit012, digit013, digit014, digit015, digit016, digit017, digit018, digit019, digit020,            _
        digit021, digit022, digit023, digit024, digit025, digit026, digit027, digit028, digit029, digit030,            _
        digit031, digit032, digit033, digit034, digit035, digit036, digit037, digit038, digit039, digit040,            _
        digit041)
    For i = 0 to 255:Chars(i) = "d_empty":Next

    Chars(32) = "d_empty"
    Chars(33) = ""       '!
    Chars(34) = ""       '"
    Chars(35) = ""       '#
    Chars(36) = ""       '$
    Chars(37) = ""       '%
    Chars(38) = ""       '&
    Chars(39) = ""       ''
    Chars(40) = ""       '(
    Chars(41) = ""       ')
    Chars(42) = ""       '*
    Chars(43) = ""       '+
    Chars(44) = ""       '
    Chars(45) = ""       '-
    Chars(46) = "d_dot"  '.
    Chars(47) = ""       '/
    Chars(48) = "d_0"    '0
    Chars(49) = "d_1"    '1
    Chars(50) = "d_2"    '2
    Chars(51) = "d_3"    '3
    Chars(52) = "d_4"    '4
    Chars(53) = "d_5"    '5
    Chars(54) = "d_6"    '6
    Chars(55) = "d_7"    '7
    Chars(56) = "d_8"    '8
    Chars(57) = "d_9"    '9
    Chars(60) = "d_less" '<
    Chars(61) = ""       '=
    Chars(62) = "d_more" '>
    Chars(64) = ""       '@
    Chars(65) = "d_a"    'A
    Chars(66) = "d_b"    'B
    Chars(67) = "d_c"    'C
    Chars(68) = "d_d"    'D
    Chars(69) = "d_e"    'E
    Chars(70) = "d_f"    'F
    Chars(71) = "d_g"    'G
    Chars(72) = "d_h"    'H
    Chars(73) = "d_i"    'I
    Chars(74) = "d_j"    'J
    Chars(75) = "d_k"    'K
    Chars(76) = "d_l"    'L
    Chars(77) = "d_m"    'M
    Chars(78) = "d_n"    'N
    Chars(79) = "d_o"    'O
    Chars(80) = "d_p"    'P
    Chars(81) = "d_q"    'Q
    Chars(82) = "d_r"    'R
    Chars(83) = "d_s"    'S
    Chars(84) = "d_t"    'T
    Chars(85) = "d_u"    'U
    Chars(86) = "d_v"    'V
    Chars(87) = "d_w"    'W
    Chars(88) = "d_x"    'X
    Chars(89) = "d_y"    'Y
    Chars(90) = "d_z"    'Z
    Chars(94) = "d_up"   '^
    '    Chars(95) = '_
    Chars(96) = "d_0a"  '0.
    Chars(97) = "d_1a"  '1. 'a
    Chars(98) = "d_2a"  '2. 'b
    Chars(99) = "d_3a"  '3. 'c
    Chars(100) = "d_4a" '4. 'd
    Chars(101) = "d_5a" '5. 'e
    Chars(102) = "d_6a" '6. 'f
    Chars(103) = "d_7a" '7. 'g
    Chars(104) = "d_8a" '8. 'h
    Chars(105) = "d_9a" '9. 'i
    Chars(106) = ""     'j
    Chars(107) = ""     'k
    Chars(108) = ""     'l
    Chars(109) = ""     'm
    Chars(110) = ""     'n
    Chars(111) = ""     'o
    Chars(112) = ""     'p
    Chars(113) = ""     'q
    Chars(114) = ""     'r
    Chars(115) = ""     's
    Chars(116) = ""     't
    Chars(117) = ""     'u
    Chars(118) = ""     'v
    Chars(119) = ""     'w
    Chars(120) = ""     'x
    Chars(121) = ""     'y
    Chars(122) = ""     'z
    Chars(123) = ""     '{
    Chars(124) = ""     '|
    Chars(125) = ""     '}
    Chars(126) = ""     '~
End Sub

'********************
' Real Time updates
'********************
'used for all the real time updates

Sub Realtime_Timer
    RollingUpdate
End Sub

' flippers top animations

Sub LeftFlipper_Animate:
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle: 
    LeftFlipperKnife.RotZ = LeftFlipper.CurrentAngle:
 End Sub


Sub LeftFlipper001_Animate:LeftFlipperTop001.RotZ = LeftFlipper001.CurrentAngle: End Sub

Sub RightFlipper_Animate
   :RightFlipperTop.RotZ = RightFlipper.CurrentAngle:
   :RightFlipperknife.RotZ = RightFlipper.CurrentAngle:
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

'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 11 colors: red, orange, amber, yellow...
'******************************************

'colors
Const red = 5
Const orange = 4
Const amber = 6
Const yellow = 3
Const darkgreen = 7
Const green = 2
Const blue = 1
Const darkblue = 8
Const purple = 9
Const white = 11
Const teal = 10

Sub SetLightColor(n, col, stat) 'stat 0 = off, 1 = on, 2 = blink, -1= no change
    Select Case col
        Case red
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case orange
            n.color = RGB(18, 3, 0)
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
            n.color = RGB(0, 16, 0)
            n.colorfull = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 18, 18)
            n.colorfull = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 8, 8)
            n.colorfull = RGB(0, 64, 64)
        Case purple
            n.color = RGB(64, 0, 96)
            n.colorfull = RGB(128, 0, 192)
        Case white
            n.color = RGB(193, 91, 0)
            n.colorfull = RGB(255, 197, 143)
        Case teal
            n.color = RGB(1, 64, 62)
            n.colorfull = RGB(2, 128, 126)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

Sub SetFlashColor(n, col, stat) 'stat 0 = off, 1 = on, -1= no change - no blink for the flashers, use FlashForMs
    Select Case col
        Case red
            n.color = RGB(255, 0, 0)
        Case orange
            n.color = RGB(255, 64, 0)
        Case amber
            n.color = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(255, 255, 0)
        Case darkgreen
            n.color = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 64, 64)
        Case purple
            n.color = RGB(128, 0, 192)
        Case white
            n.color = RGB(255, 197, 143)
        Case teal
            n.color = RGB(2, 128, 126)
    End Select
    If stat <> -1 Then
        n.Visible = stat
    End If
End Sub

'*************************
' Rainbow Changing Lights
'*************************

Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

Sub StartRainbow(n) 'n is a collection
    set RainbowLights = n
    RGBStep = 0
    RGBFactor = 5
    rRed = 255
    rGreen = 0
    rBlue = 0
    RainbowTimer.Enabled = 1
End Sub

Sub StopRainbow()
    RainbowTimer.Enabled = 0
    TurnOffArrows
End Sub

Sub TurnOffArrows() 'during Modes when changing modes
    For each x in aArrows
        SetLightColor x, white, 0
    Next
End Sub

Sub TurnOnArrows(incolor) 'blink during Modes
    For each x in aArrows
        SetLightColor x, incolor, 2
    Next
End Sub

Sub RainbowTimer_Timer 'rainbow led light color changing
    Dim obj
    Select Case RGBStep
        Case 0 'Green
            rGreen = rGreen + RGBFactor
            If rGreen > 255 then
                rGreen = 255
                RGBStep = 1
            End If
        Case 1 'Red
            rRed = rRed - RGBFactor
            If rRed < 0 then
                rRed = 0
                RGBStep = 2
            End If
        Case 2 'Blue
            rBlue = rBlue + RGBFactor
            If rBlue > 255 then
                rBlue = 255
                RGBStep = 3
            End If
        Case 3 'Green
            rGreen = rGreen - RGBFactor
            If rGreen < 0 then
                rGreen = 0
                RGBStep = 4
            End If
        Case 4 'Red
            rRed = rRed + RGBFactor
            If rRed > 255 then
                rRed = 255
                RGBStep = 5
            End If
        Case 5 'Blue
            rBlue = rBlue - RGBFactor
            If rBlue < 0 then
                rBlue = 0
                RGBStep = 0
            End If
    End Select
    For each obj in RainbowLights
        obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
        obj.colorfull = RGB(rRed, rGreen, rBlue)
    Next
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1) Then
        DMD CL("LAST SCORE"), CL("PLAYER 1 " &FormatScore(Score(1) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2) Then
        DMD CL("LAST SCORE"), CL("PLAYER 2 " &FormatScore(Score(2) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3) Then
        DMD CL("LAST SCORE"), CL("PLAYER 3 " &FormatScore(Score(3) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4) Then
        DMD CL("LAST SCORE"), CL("PLAYER 4 " &FormatScore(Score(4) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD "", CL("GAME OVER"), "", eNone, eBlink, eNone, 2000, False, ""
    If bFreePlay Then
        DMD "", CL("FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits > 0 Then
            DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "        RETRO ROMMY", "          PRESENTS", "d_jppresents", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, ""
    DMD "", CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("HIGHSCORES"), Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL("HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL("HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    StartLightSeq
    StartRainbow aArrows
    DMDFlush
    ShowTableInfo
    'PlaySong "mu_gameover"
End Sub

Sub StopAttractMode
    StopRainbow
    DMDScoreNow
    LightSeqAttract.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 10
    'LightSeqAttract.Play SeqAllOff
    LightSeqAttract.Play SeqDiagUpRightOn, 25, 2
    LightSeqAttract.Play SeqStripe1VertOn, 25
    LightSeqAttract.Play SeqClockRightOn, 180, 2
    LightSeqAttract.Play SeqFanLeftUpOn, 50, 2
    LightSeqAttract.Play SeqFanRightUpOn, 50, 2
    LightSeqAttract.Play SeqScrewRightOn, 50, 2

    LightSeqAttract.Play SeqDiagDownLeftOn, 25, 2
    LightSeqAttract.Play SeqStripe2VertOn, 25, 2
    LightSeqAttract.Play SeqFanLeftDownOn, 50, 2
    LightSeqAttract.Play SeqFanRightDownOn, 50, 2
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
End Sub

Sub LightSeqTopFlashers_PlayDone()
    FlashEffect 7
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, timers, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
Dim bRotateLights
Dim Mode(4, 15) 'the first 4 is the current player, contains status of the Mode, 0 not started, 1 won, 2 started
Dim Weapons(4)  ' collected weapons
Dim LoopCount
Dim LoopHits(4)
Dim LoopValue(4)
Dim SlingCount 'used for the db2s animation
Dim ComboCount
Dim ComboHits(4)
Dim ComboValue(4)
Dim Mystery(4, 4) 'inlane lights for each player
Dim BumperHits(4)
Dim BumperNeededHits(4)
Dim TargetHits(4, 7) '6 targets + the bumper -the blue lights
Dim WeaponHits(4, 6) '6 lights, lanes and the magnet post
Dim aWeaponSJactive
Dim WeaponSJValue(4)
Dim bFlippersEnabled
Dim bTommyStarted
Dim TommyCount 'counts the seconds left
Dim TommyValue
Dim bPoliceStarted
Dim PoliceRampHits
Dim PoliceTargetHits
Dim PoliceCount 'counts the seconds, used for the police jackpot
Dim TeensKilled(4)
Dim TeensKilledValue(4)
Dim CounselorsKilled(4)
Dim CenterSpinnerHits(4)
Dim LeftSpinnerHits(4)
Dim RightSpinnerHits(4)
Dim TargetJackpot(4)
Dim bJasonMBStarted
Dim bFreddyMBStarted
Dim bMichaelMBStarted
Dim ArrowMultiPlier(8) 'used for the Jackpot multiplier and the color for the Jason MB arrow lights
Dim FreddySJValue
Dim MichaelSJValue
'variables used only in the modes
Dim NewMode
Dim ReadyToKill ' final shot in a mode
Dim SpinCount
Dim SpinNeeded
Dim TargetModeHits 'mode 2,3 hits
Dim EndModeCountdown
Dim BlueTargetsCount
Dim ArrowsCount
Dim ArrowsCount2

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    SpecialGameStartUps
    'pupbase

    'Init Variables
    bRotateLights = True
    aWeaponSJactive = False
    bFlippersEnabled = True 'only disabled if the police or Tommy catches you
    bTommyStarted = False
    TommyCount = 1          'we set it to 1 because it also acts as a multiplier in the hurry up
    TommyValue = 500000
    bPoliceStarted = False
    PoliceRampHits = 0
    PoliceTargetHits = 0
    bJasonMBStarted = False
    bFreddyMBStarted = False
    bMichaelMBStarted = False
    FreddySJValue = 1000000
    MichaelSJValue = 1000000
    NewMode = 0
    SpinCount = 0
    ReadyToKill = False
    SpinNeeded = 0
    EndModeCountdown = 0
    BlueTargetsCount = 0
    ArrowsCount = 0
    ArrowsCount2 = 0
    For i = 0 to 4
        SkillShotValue(i) = 500000
        SuperSkillShotValue(i) = 5000000
        LoopValue(i) = 500000
        ComboValue(i) = 500000
        BumperHits(i) = 0
        BumperNeededHits(i) = 10
        Weapons(i) = 0
        WeaponSJValue(i) = 3500000
        TeensKilled(i) = 0
        TeensKilledValue(i) = 250000
        CounselorsKilled(i) = 0
        LoopHits(i) = 0
        ComboHits(i) = 0
        CenterSpinnerHits(i) = 0
        LeftSpinnerHits(i) = 0
        RightSpinnerHits(i) = 0
        TargetJackpot(i) = 500000
        Jackpot(i) = 500000 'only used in the last mode
        BallsInLock(i) = 0
        ArrowMultiPlier(i) = 1
    Next
    For i = 0 to 4
        For j = 0 to 4
            Mystery(i, j) = 0
        Next
    Next
    For i = 0 to 4
        For j = 0 to 15
            Mode(i, j) = 0
        Next
    Next
    For i = 0 to 4
        For j = 0 to 7
            TargetHits(i, j) = 1
        Next
    Next
    For i = 0 to 4
        For j = 0 to 6
            WeaponHits(i, j) = 1
        Next
    Next
    LoopCount = 0
    ComboCount = 0
End Sub

Sub InstantInfo
    Dim tmp
    DMD CL("INSTANT INFO"), "", "", eNone, eNone, eNone, 1000, False, ""
    Select Case NewMode
        Case 1 'Billy Loomis = Super Spinners
            DMD CL("CURRENT MODE"), CL("BILLY LOOMIS"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE SPINNERS"), CL("AND SCOOP TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 2 'Stu Macher = 5 Targets at semi random
            DMD CL("CURRENT MODE"), CL("STU MACHER"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("HIT THE LIT TARGETS"), CL("AND MAGNET TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 3 'Mickey = 5 Flashing Shots 90 seconds to complete
            DMD CL("CURRENT MODE"), CL("MICKEY"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE LIGHTS"), CL("YOU HAVE 2 MINUTES"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE SCOOP"), CL("BEFORE TIME IS UP"), "", eNone, eNone, eNone, 2000, False, ""
        Case 4 'Mrs Loomis = 5 Orbits
            DMD CL("CURRENT MODE"), CL("MRS LOOMIS"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 9"), CL("LIT ORBITS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 5 'Roman = Shoot 4 lights 60 seconds
            DMD CL("CURRENT MODE"), CL("ROMAN"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 9 LIGHTS"), CL("YOU HAVE 60 SECONDS"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("AND THE BASEMENT"), CL("TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 6 'Jill = Shoot the ramps
            DMD CL("CURRENT MODE"), CL("JILL"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("ERIC"), CL("SHOOT 5 RAMPS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 7 'Charlie=  Target Frenzy
            DMD CL("CURRENT MODE"), CL("CHARLIE"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 6"), CL("LIT TARGETS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 8 'Richie = 5 Targets in rotation
            DMD CL("CURRENT MODE"), CL("RICHIE"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL(""), CL("SHOOT 5 LIT TARGETS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 9 'Amber = Magnet post
            DMD CL("CURRENT MODE"), CL("AMBER"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE MAGNET"), CL("9 TIMES"), "", eNone, eNone, eNone, 2000, False, ""
        Case 10 'Quinn = Ramps and Orbits
            DMD CL("CURRENT MODE"), CL("QUINN"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 9 RAMPS"), CL("OR ORBITS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 11 'Ethan = 5 Blue Targets at random
            DMD CL("CURRENT MODE"), CL("ETHAN"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 6"), CL("BLUE TARGETS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 12 'Wayne = Super Spinners at random
            DMD CL("CURRENT MODE"), CL("WAYNE"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE SPINNERS"), CL("AT RANDOM"), "", eNone, eNone, eNone, 2000, False, ""
        Case 13 'Dewey = Follow the Lights
            DMD CL("CURRENT MODE"), CL("DEWEY"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 6 LIT"), CL("LIGHTS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 14 'Gale = Follow the Lights random
            DMD CL("CURRENT MODE"), CL("GALE"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 6 LIT"), CL("LIGHTS"), "", eNone, eNone, eNone, 2000, False, ""
    End Select
    DMD CL("YOUR SCORE"), CL(FormatScore(Score(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("EXTRA BALLS"), CL(ExtraBallsAwards(CurrentPlayer) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SCREAM FACTOR X"), CL("X " &PlayfieldMultiplier(CurrentPlayer) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SCREAM MULTIPLIER"), CL("X " &BonusMultiplier(CurrentPlayer) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SKILLSHOT VALUE"), CL(FormatScore(SkillshotValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SUPR SKILLSHOT VALUE"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("RAMP COMBO VALUE"), CL(FormatScore(ComboValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("LOOP COMBO VALUE"), CL(FormatScore(LoopValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SCREAM SUPR JACKPOT"), CL(FormatScore(WeaponSJValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("FILM JACKPOT"), CL(FormatScore(TargetJackpot(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    If Score(1) Then
        DMD CL("PLAYER 1 SCORE"), CL(FormatScore(Score(1) ) ), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(2) Then
        DMD CL("PLAYER 2 SCORE"), CL(FormatScore(Score(2) ) ), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(3) Then
        DMD CL("PLAYER 3 SCORE"), CL(FormatScore(Score(3) ) ), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(4) Then
        DMD CL("PLAYER 4 SCORE"), CL(FormatScore(Score(4) ) ), "", eNone, eNone, eNone, 2000, False, ""
    End If
End Sub

Sub StopMBmodes 'stop multiball modes after loosing the last multibal
    If bJasonMBStarted Then StopJasonMultiball
    If bFreddyMBStarted Then StopFreddyMultiball
    If bMichaelMBStarted Then StopMichaelMultiball
    If NewMode = 15 Then StopMode
End Sub

Sub StopEndOfBallMode()                         'this sub is called after the last ball in play is drained, reset skillshot, modes, timers
    If li048.State then SuperJackpotTimer_Timer 'to turn off the timer
    If bPoliceStarted Then StopPolice
    if bTommyStarted Then StopTommyJarvis
    StopMode
    TeenTimer.Enabled = 0
End Sub

Sub ResetNewBallVariables() 'reset variables and lights for a new ball or player
    'turn on or off the needed lights before a new ball is released
    TurnOffPlayfieldLights
    libumper.State = 0
    'set up the lights according to the player achievments
    BonusMultiplier(CurrentPlayer) = 1 'no need to update light as the 1x light do not exists
    UpdateTargetLights
    UpdateWeaponLights                 ' the W lights
    UpdateWeaponLights2                ' the collected weapons
    aWeaponSJactive = False
    UpdateLockLights                   ' turn on the lock lights for the current player
    UpdateModeLights                   ' show the killed counselors
    TeenTimer.Enabled = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub UpdateTargetLights 'CurrentPlayer
    Li031.State = TargetHits(CurrentPlayer, 1)
    Li049.State = TargetHits(CurrentPlayer, 2)
    Li050.State = TargetHits(CurrentPlayer, 3)
    Li051.State = TargetHits(CurrentPlayer, 4)
    Li058.State = TargetHits(CurrentPlayer, 5)
    Li057.State = TargetHits(CurrentPlayer, 6)
    Li079.State = TargetHits(CurrentPlayer, 7)
End Sub

Sub TurnOffBlueTargets 'Turns off all blue targets at the start of a mode that uses the blue targets
    Li031.State = 0
    Li049.State = 0
    Li050.State = 0
    Li051.State = 0
    Li058.State = 0
    Li057.State = 0
    Li079.State = 0
End Sub

Sub ResetTargetLights 'CurrentPlayer
    Dim j
    For j = 0 to 7
        TargetHits(CurrentPlayer, j) = 1
    Next
    UpdateTargetLights
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    LightSeqSkillshot.Play SeqAllOff
    DMD CL("HIT LIT LIGHT"), CL("FOR SKILLSHOT"), "", eNone, eNone, eNone, 3000, True, ""
    li034.State = 2
    li063.State = 2
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    bRotateLights = True
    LightSeqSkillshot.StopPlay
    Li034.State = 0
    li063.State = 0
    DMDScoreNow
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************

'*********************************************************
' Slingshots has been hit
' In this table the slingshots change the outlanes lights

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    CellLShake
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lemk
    DOF 105, DOFPulse
        If FlippersBlood Then LeftSplat
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    ' add some effect to the table?
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing004.Visible = 0:LeftSLing003.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing003.Visible = 0:LeftSLing002.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing002.Visible = 0:Lemk.RotX = -20:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    CellRShake
    PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFcontactors), Remk
    DOF 106, DOFPulse
        If FlippersBlood Then RightSplat
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    ' add some effect to the table?
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub SlingTimer_Timer
    Select case SlingCount
        Case 0, 2, 4, 6, 8:Controller.B2SSetData 10, 1
        Case 1, 3, 5, 7, 9:Controller.B2SSetData 10, 0
        Case 10:SlingTimer.Enabled = 0
    End Select
    SlingCount = SlingCount + 1
End Sub

'***********************
'        Bumper
'***********************
' Bumper Jackpot is scored when the bumper light is on
' the value is always 200.000 + 20% of the score

Sub Bumper1_Hit ' W6
    If Tilted Then Exit Sub
    Dim tmp
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), Bumper1
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    DOF 138, DOFPulse
    ' add some points
    If libumper.State Then 'the light is on so give the bumper Jackpot
        FlashForms libumper, 1500, 75, 0
        DOF 127, DOFPulse
        tmp = 100000 + INT(Score(CurrentPlayer) * 0.01) * 10 'the bumper jackpot is 100.000 + 10% of the score
        DMD CL("BUMPER JACKPOT"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 1500, True, "" 
        AddScore2 tmp
    Else 'score normal points
        AddScore 1000
    End If
    ' check for modes
    Select Case NewMode
        Case 2, 7, 8, 11
            If li079.State And MBlocklight.State = 1 Then
                TargetModeHits = TargetModeHits + 1
                li079.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 7) = 0
            CheckTargets
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper1"
    ' increase the bumper hit count and increase the bumper value after each 30 hits
    BumperHits(CurrentPlayer) = BumperHits(CurrentPlayer) + 1
    ' Check the bumper hits to lit the bumper to collect the bumper jackpot
    If BumperHits(CurrentPlayer) = BumperNeededHits(CurrentPlayer) Then
        libumper.State = 1
        BumperNeededHits(CurrentPlayer) = BumperNeededHits(CurrentPlayer) + 10 + RndNbr(10)
    End If
End Sub

'*********
' Lanes
'*********
' in and outlanes - mystery ?
Sub Trigger001_Hit
    PLaySoundAt "fx_sensor", Trigger001
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    Mystery(CurrentPlayer, 1) = 1
    CheckMystery
End Sub

Sub Trigger002_Hit
    PLaySoundAt "fx_sensor", Trigger002
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    Mystery(CurrentPlayer, 2) = 1
    CheckMystery
End Sub

Sub Trigger003_Hit
    PLaySoundAt "fx_sensor", Trigger003
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    Mystery(CurrentPlayer, 3) = 1
    CheckMystery
End Sub

Sub Trigger004_Hit
    PLaySoundAt "fx_sensor", Trigger004
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    Mystery(CurrentPlayer, 4) = 1
    CheckMystery
End Sub

Sub UpdateMysteryLights
    'update lane lights
    li017.State = Mystery(CurrentPlayer, 1)
    li018.State = Mystery(CurrentPlayer, 2)
    li019.State = Mystery(CurrentPlayer, 3)
    li020.State = Mystery(CurrentPlayer, 4)
    If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) = 4 Then
        li078.State = 1
    End If
End Sub

Sub RotateLaneLights(n) 'n is the direction, 1 or 0, left or right. They are rotated by the flippers
    Dim tmp
    If bRotateLights Then
        If n = 1 Then
            tmp = Mystery(CurrentPlayer, 1)
            Mystery(CurrentPlayer, 1) = Mystery(CurrentPlayer, 2)
            Mystery(CurrentPlayer, 2) = Mystery(CurrentPlayer, 3)
            Mystery(CurrentPlayer, 3) = Mystery(CurrentPlayer, 4)
            Mystery(CurrentPlayer, 4) = tmp
        Else
            tmp = Mystery(CurrentPlayer, 4)
            Mystery(CurrentPlayer, 4) = Mystery(CurrentPlayer, 3)
            Mystery(CurrentPlayer, 3) = Mystery(CurrentPlayer, 2)
            Mystery(CurrentPlayer, 2) = Mystery(CurrentPlayer, 1)
            Mystery(CurrentPlayer, 1) = tmp
        End If
    End If
    UpdateMysteryLights
End Sub

'table lanes
Sub Trigger005_Hit
    PLaySoundAt "fx_sensor", Trigger005
    If Tilted Then Exit Sub
    Addscore 1000
    If bSkillShotReady Then li034.State = 0
    If bMichaelMBStarted AND li032.State Then 'award the michael super jackpot
        DOF 126, DOFPulse
        DMDFlush
        DMD CL("SUPER JACKPOT"), CL(FormatScore(MichaelSJValue) ), "_", eBlink, eNone, eNone, 1000, True, "vo_superjackpot"
        Addscore2 MichaelSJValue
        MichaelSJValue = 1000000
        li032.State = 0
        LightEffect 2
        GiEffect 2
    End If
End Sub

Sub Trigger006_Hit 'skillshot 1
    PLaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
    Addscore 1000
    If bSkillShotReady AND li034.State Then AwardSkillshot
End Sub

Sub Trigger008_Hit 'end top loop
    PLaySoundAt "fx_sensor", Trigger008
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger008"
    Select Case NewMode
        Case 4:TargetModeHits = TargetModeHits + 1:CheckWinMode
    End Select
End Sub

Sub Trigger009_Hit 'right loop
    PLaySoundAt "fx_sensor", Trigger009
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    PlayThunder
    Addscore 10000
    Flashforms f2A, 800, 50, 0
    Flashforms F2B, 800, 50, 0
    If LastSwitchHit = "Trigger008" Then
        AwardLoop
    Else
        li061.State = 2 'super loops light
        LoopCount = 1
    End If
    If F002.State Then TeenKilled:F002.State = 0
    ' Weapons Super Jackpot
    If aWeaponSJactive AND li060.State Then AwardWeaponsSuperJackpot
    'Jason multiball
    If bJasonMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(7) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(7) * 1000000
        If ArrowMultiPlier(7) < 5 Then
            ArrowMultiPlier(7) = ArrowMultiPlier(7) + 1
            UpdateArrowLights
        End If
    End If
    'Freddy multiball
    If bFreddyMBStarted and li060.State Then
        DOF 126, DOFPulse
        DMDFlush
        DMD CL("SUPER JACKPOT"), CL(FormatScore(FreddySJValue) ), "_", eBlink, eNone, eNone, 1000, True, "vo_superjackpot"
        Addscore2 FreddySJValue
        FreddySJValue = 1000000
        li060.State = 0
        LightEffect 2
        GiEffect 2
    End If
    'Modes
    Select Case NewMode
        Case 5, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger009"
End Sub

'****************************
' extra triggers - no sound
'****************************

Sub Gate001_Hit 'superskillshot
    If Tilted Then Exit Sub
    Addscore 1000
    If bSkillShotReady Then AwardSuperSkillshot
End Sub

Sub Trigger011_Hit 'cabin playfield , only active when the ball move upwards
    If Tilted OR ActiveBall.VelY > 0 Then Exit Sub
    Addscore 5000
    If aWeaponSJactive Then     'the lit Super Jackpot light is lit, so lit the Super Jackpot Light at the right loop
        SuperJackpotTimer_Timer 'call the timer to stop the 30s timer and blinking light at the cabin
        li060.State = 2
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li066.State Then
                TargetModeHits = TargetModeHits + 1
                li066.State = 0
                CheckWinMode
            End If
        Case 5, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
            If ReadyToKill Then 'kill her :)
                WinMode
            End If
        Case 15
            AwardJackpot
    End Select
End Sub

Sub UpdateLockLights
    Select Case BallsInLock(CurrentPlayer)
        Case 0:li071.State = 0:li072.State = 0:li073.State = 0
        Case 1:li073.State = 2                                 'enabled the first lock
        Case 2:li073.State = 1:li072.State = 2                 'lock 1
        Case 3:li072.State = 1:li071.State = 2                 'lock 2
        Case 4:li071.State = 0:li072.State = 0:li073.State = 0 'lock 3
    End Select
End Sub

Sub Trigger012_Hit 'left spinner - W1
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F5, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    'weapon hit
    If WeaponHits(CurrentPlayer, 1) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 1) = 0
        CheckWeapons
    End If
    'teen killed
    If F001.State Then TeenKilled:F001.State = 0
    'Jason multiball
    If bJasonMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(1) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(1) * 1000000
        If ArrowMultiPlier(1) < 5 Then
            ArrowMultiPlier(1) = ArrowMultiPlier(1) + 1
            UpdateArrowLights
        End If
    End If
    'Michael multiball
    If bMichaelMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        Addscore2 1000000
        MichaelSJValue = MichaelSJValue + 2000000
        If MichaelSJValue >= 5000000 Then
            li032.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
                Case Else:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
            End Select
            PlaySound "vo_getthesuperjackpot"
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li062.State Then
                TargetModeHits = TargetModeHits + 1
                li062.State = 0
                CheckWinMode
            End If
        Case 5, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
End Sub

Sub Trigger013_Hit 'behind right spinner
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F4, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    'weapon Hit
    If WeaponHits(CurrentPlayer, 6) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 6) = 0
        CheckWeapons
    End If
    If li077.State Then 'give the special, which in this table is an add-a-ball
        PlaySound "vo_special"
        AwardExtraBall
        Addscore 150000
        li077.State = 0
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(8) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(8) * 1000000
        If ArrowMultiPlier(8) < 5 Then
            ArrowMultiPlier(8) = ArrowMultiPlier(8) + 1
            UpdateArrowLights
        End If
    End If
    'Michael multiball
    If bMichaelMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        Addscore2 1000000
        MichaelSJValue = MichaelSJValue + 2000000
        If MichaelSJValue >= 5000000 Then
            li032.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
                Case Else:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
            End Select
            PlaySound "vo_getthesuperjackpot"
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select case NewMode
        Case 5, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
            'li077.State = 0
    End Select
End Sub

Sub Trigger014_Hit 'center spinner for loop awards - W4
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F3, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    If LastSwitchHit = "Trigger008" Then
        AwardLoop
    Else
        li061.State = 2 'super loops light
        LoopCount = 1
    End If
    If F005.State Then TeenKilled:F005.State = 0
    'weapon hit
    If WeaponHits(CurrentPlayer, 4) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 4) = 0
        CheckWeapons
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(5) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(5) * 1000000
        If ArrowMultiPlier(5) < 5 Then
            ArrowMultiPlier(5) = ArrowMultiPlier(5) + 1
            UpdateArrowLights
        End If
    End If
    'Michael multiball
    If bMichaelMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        Addscore2 1000000
        MichaelSJValue = MichaelSJValue + 2000000
        If MichaelSJValue >= 5000000 Then
            li032.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
                Case Else:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
            End Select
            PlaySound "vo_getthesuperjackpot"
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li067.State Then
                TargetModeHits = TargetModeHits + 1
                li067.State = 0
                CheckWinMode
            End If
        Case 5, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger014"
End Sub

Sub Trigger015_Hit 'right ramp done - W5
    If Tilted Then Exit Sub
    Addscore 5000
    If LastSwitchHit = "Trigger017" Then
        AwardCombo
    Else
        ComboCount = 1
    End If
    If F003.State Then TeenKilled:F003.State = 0
    'weapon hit
    If WeaponHits(CurrentPlayer, 5) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 5) = 0
        CheckWeapons
    End If
    If bTommyStarted Then 'the Tommy Jarvis hurry up is started so award the jackpot
        AwardTommyJackpot
        StopTommyJarvis
    End If
    If bPoliceStarted Then 'the police hurry up is started
        PoliceRampHits = PoliceRampHits + 1
        If PoliceRampHits = 3 Then
            AwardPoliceJackpot
            StopPolice
        End If
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(6) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(6) * 1000000
        If ArrowMultiPlier(6) < 5 Then
            ArrowMultiPlier(6) = ArrowMultiPlier(6) + 1
            UpdateArrowLights
        End If
    End If
    'Freddy multiball
    If bFreddyMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        Addscore2 1000000
        FreddySJValue = FreddySJValue + 2000000
        If FreddySJValue >= 5000000 Then
            li060.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, ""
                Case Else:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, ""
            End Select
            PlaySound "vo_getthesuperjackpot"
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li068.State Then
                TargetModeHits = TargetModeHits + 1
                li068.State = 0
                CheckWinMode
            End If
        Case 5, 6, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger015"
End Sub

Sub Trigger016_Hit 'left ramp done - W2
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    If LastSwitchHit = "Trigger017" Then
        AwardCombo
    Else
        ComboCount = 1
    End If
    If F004.State Then TeenKilled:F004.State = 0
    If li069.State Then AwardTargetJackpot
    'weapon hit
    If WeaponHits(CurrentPlayer, 2) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 2) = 0
        CheckWeapons
    End If
    If bPoliceStarted Then 'the police hurry up is started
        PoliceRampHits = PoliceRampHits + 1
        If PoliceRampHits = 3 Then
            AwardPoliceJackpot
            StopPolice
        End If
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(2) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(2) * 1000000
        If ArrowMultiPlier(2) < 5 Then
            ArrowMultiPlier(2) = ArrowMultiPlier(2) + 1
            UpdateArrowLights
        End If
    End If
    'Freddy multiball
    If bFreddyMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        Addscore2 1000000
        FreddySJValue = FreddySJValue + 2000000
        If FreddySJValue >= 5000000 Then
            li060.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, ""
                Case Else:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, ""
            End Select
            PlaySound "vo_getthesuperjackpot"
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li064.State Then
                TargetModeHits = TargetModeHits + 1
                li064.State = 0
                CheckWinMode
            End If
        Case 5, 6, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger016"
End Sub

Sub Trigger017_Hit 'cabin top
    DOF 204, DOFPulse
    If Tilted Then Exit Sub
    'ChangeGi red
    ChangeGIIntensity 1
    GiEffect 1
    FlashEffect 1
    'PlaySound "mu_kikimama"
    'vpmTimer.AddTimer 2500, "ChangeGi white:ChangeGIIntensity 1 '"
    ' Select Mode
    Select Case Mode(CurrentPlayer, 0)
        Case 0:SelectMode 'no mode is active then activate another mode
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger017"
End Sub

Sub Trigger018_Hit 'left loop - only light effect
    '	DOF 204, DOFPulse
    If Tilted Then Exit Sub
    Flashforms f1A, 800, 50, 0
    Flashforms F1B, 800, 50, 0
End Sub

'************
'  Targets
'************

Sub Target001_Hit 'police
    PLaySoundAtBall SoundFXDOF("fx_Target",205,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    Flashforms f5, 800, 50, 0
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li031.State And MBlocklight.State = 1 Then
                TargetModeHits = TargetModeHits + 1
                li031.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 1) = 0
            CheckTargets
    End Select
    If NOT bPoliceStarted Then
        PoliceTargetHits = PoliceTargetHits + 1
        If PoliceTargetHits = 5 Then
            StartPolice
        End If
    Else 'the police hurry up is started
        PoliceTargetHits = PoliceTargetHits + 1
        If PoliceTargetHits = 3 Then
            AwardPoliceJackpot
            StopPolice
        End If
    End If
End Sub

Sub Target002_Hit 'right target 1
    PLaySoundAtBall SoundFXDOF("fx_Target",205,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li057.State And MBlocklight.State = 1 Then
                TargetModeHits = TargetModeHits + 1
                li057.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 6) = 0
            CheckTargets
    End Select
End Sub

Sub Target003_Hit 'right target 2
    PLaySoundAtBall SoundFXDOF("fx_Target",205,DOFPulse,DOFTargets)
    Addscore 5000
    If Tilted Then Exit Sub
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li058.State And MBlocklight.State = 1 Then
                TargetModeHits = TargetModeHits + 1
                li058.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 5) = 0
            CheckTargets
    End Select
End Sub

Sub Target004_Hit 'cabin target 1
    PLaySoundAtBall SoundFXDOF("fx_Target",109,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li049.State And MBlocklight.State = 1 Then
                TargetModeHits = TargetModeHits + 1
                li049.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 2) = 0
            CheckTargets
    End Select
End Sub

Sub Target005_Hit 'cabin target 2
    PLaySoundAtBall SoundFXDOF("fx_Target",109,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li050.State And MBlocklight.State = 1 Then
                TargetModeHits = TargetModeHits + 1
                li050.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 3) = 0
            CheckTargets
    End Select
End Sub

Sub Target006_Hit 'loop target
    PLaySoundAtBall SoundFXDOF("fx_Target",109,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li051.State And MBlocklight.State = 1 Then
                TargetModeHits = TargetModeHits + 1
                li051.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 4) = 0
            CheckTargets
    End Select
End Sub

Sub Target007_Hit 'magnet target W3
    PLaySoundAtBall SoundFXDOF("fx_Target",107,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    'weapon hit
    If WeaponHits(CurrentPlayer, 3) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 3) = 0
        CheckWeapons
    End If
    'extra ball
    If li070.State Then AwardExtraBall:li070.State = 0
    Select Case NewMode
        Case 2
            If li065.State And MBlocklight.State = 1 Then
                WinMode
            End If
        Case 9
            If li065.State And MBlocklight.State = 1  Then
                TargetModeHits = TargetModeHits + 1
                CheckWinMode
            End If
    End Select
End Sub

Sub CheckTargets
    Dim tmp, i
    tmp = 0
    UpdateTargetLights
    For i = 1 to 7
        tmp = tmp + TargetHits(CurrentPlayer, i)
    Next
    If tmp = 0 then 'all targets are hit so turn on the target Jackpot
        DMDFlush
        DMD "_", CL("TARGET JACKPOT S LIT"), "", eNone, eNone, eNone, 1000, True, "vo_shoottheleftramp"
        BlockerLightsBlueOff
        li069.State = 1
        LightSeqBLueTargets.Play SeqBlinking, , 15, 25
        For i = 1 to 7 'and reset them
            TargetHits(CurrentPlayer, i) = 1
        Next
        UpdateTargetLights
    End If
End Sub

'*************
'  Spinners
'*************

Sub Spinner001_Spin 'right
    PlaySoundAt "fx_spinner", Spinner001
    DOF 200, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    RightSpinnerHits(CurrentPlayer) = RightSpinnerHits(CurrentPlayer) + 1
    ' check for add-a-aball during multiballs or during normal play
    If RightSpinnerHits(CurrentPlayer) >= 100 Then
        LitSpecial
        RightSpinnerHits(CurrentPlayer) = 0
    End If
    CheckSpinners
    'chek modes
    Select Case NewMode
        Case 1
            If li059.State AND SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
        Case 12
            If li059.State AND SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
    End Select
End Sub

Sub LitSpecial
    DMDFlush
    DMD "_", CL("SPECIAL IS LIT"), "", eNone, eNone, eNone, 1000, True, "vo_specialislit"
    li077.State = 1
End Sub

Sub Spinner002_Spin 'center
    PlaySoundAt "fx_spinner", Spinner002
    DOF 201, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    CenterSpinnerHits(CurrentPlayer) = CenterSpinnerHits(CurrentPlayer) + 1
    ' check for Bonus multiplier
    If CenterSpinnerHits(CurrentPlayer) >= 30 Then
        li074.State = 1
    End If
    If CenterSpinnerHits(CurrentPlayer) >= 40 Then
        AddBonusMultiplier 1
        CenterSpinnerHits(CurrentPlayer) = 0
        li074.State = 0
    End If
    CheckSpinners
    'chek modes
    Select Case NewMode
        Case 1
            If li067.State AND SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
        Case 12
            If li067.State AND SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
    End Select
End Sub

Sub Spinner003_Spin 'left
    PlaySoundAt "fx_spinner", Spinner003
    DOF 202, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    LeftSpinnerHits(CurrentPlayer) = LeftSpinnerHits(CurrentPlayer) + 1
    CheckSpinners
    'chek modes
    Select Case NewMode
        Case 1
            If li062.State AND SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
        Case 12
            If li062.State AND SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
    End Select
End Sub

Sub CheckSpinners
End Sub

'*********
' scoop
'*********

Sub scooptrigger_Hit
     PlaySoundAt "fx_hole_enter", scoop
End Sub

Sub scoop_Hit
    BallsinHole = BallsInHole + 1
    scoop.Destroyball
    If Tilted Then kickBallOut: Exit Sub
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    ' kick out the ball during hurry-ups
    If bTommyStarted OR bPoliceStarted Then vpmtimer.addtimer 500, "kickBallOut '"
    ' check for modes
    Addscore 5000
    Flashforms f4, 800, 50, 0
    ' check modes
    If(Mode(CurrentPlayer, NewMode) = 2) AND(Mode(CurrentPlayer, 0) = 0) Then 'the mode is ready, so start it
        vpmtimer.addtimer 2000, "StartMode '"
        Exit Sub
    End If
    If li078.State Then
        AwardMystery 'after the award the ball will be kicked out
        li078.State = 0
    Else
        Select Case NewMode
            Case 1, 3
                If ReadyToKill Then
                    WinMode
                Else
                    vpmtimer.addtimer 1500, "kickBallOut '"
                End If  
            Case Else
                ' Nothing left to do, so kick out the ball
                vpmtimer.addtimer 1500, "kickBallOut '"
        End Select
    End If
End Sub

Sub kickBallOut
    If BallsinHole > 0 Then
        BallsinHole = BallsInHole - 1
        PlaySoundAt SoundFXDOF("fx_popper", 111, DOFPulse, DOFcontactors), scoop
        DOF 124, DOFPulse
        scoopexit.CreateSizedBallWithMass BallSize / 2, BallMass
        scoopexit.kick 235, 22, 1
        Flashforms F4, 500, 50, 0
        vpmtimer.addtimer 400, "kickBallOut '" 'kick out the rest of the balls, if any
    End If
End Sub

'*************
' Magnet
'*************

Sub Trigger007_Hit
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    If ActiveBall.VelY > 10 Then
        ActiveBall.VelY = 10
    End If
    mMagnet.MagnetOn = True
    DOF 112, DOFOn
    Me.TimerEnabled = 1 'to turn off the Magnet
    'Jason multiball
    If bJasonMBStarted Then
        If ActiveBall.VelY < 0 Then 'this means the ball going up
            DMDFlush
            DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(3) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
            LightEffect 2
            GiEffect 2
            Addscore2 ArrowMultiPlier(3) * 1000000
            If ArrowMultiPlier(3) < 5 Then
                ArrowMultiPlier(3) = ArrowMultiPlier(3) + 1
                UpdateArrowLights
            End If
        End If
    End If
    'Modes
    If ActiveBall.VelY < 0 Then 'this means the ball going up
        Select Case NewMode
            Case 5, 13, 14
                TargetModeHits = TargetModeHits + 1
                CheckWinMode
            Case 15
                AwardJackpot
        End Select
    End If
End Sub

Sub Trigger007_Timer
    Me.TimerEnabled = 0
    ReleaseMagnetBalls
End Sub

Sub ReleaseMagnetBalls 'mMagnet off and release the ball if any
    Dim ball
    mMagnet.MagnetOn = False
    DOF 112, DOFOff
    For Each ball In mMagnet.Balls
        With ball
            .VelX = 0
            .VelY = 1
        End With
    Next
End Sub

'*******************
'    RAMP COMBOS
'*******************
' don't time out
' starts at 500K for a 2 way combo and it is doubled on each combo
' shots that count as ramp combos:
' Left Ramp and Right Ramp
' shots to the same ramp also count as loops

Sub AwardCombo
    ComboCount = ComboCount + 1
    Select Case ComboCount
        Case 1: 'just starting
        Case 2:DMD CL("COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_combo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 3:DMD CL("2X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 2) ), "", eNone, eNone, eNone, 1500, True, "vo_2xcombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 4:DMD CL("3X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 3) ), "", eNone, eNone, eNone, 1500, True, "vo_3xcombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 5:DMD CL("4X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 4) ), "", eNone, eNone, eNone, 1500, True, "vo_4xcombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 6:DMD CL("5X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 5) ), "", eNone, eNone, eNone, 1500, True, "vo_5xcombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 7:DMD CL("SUPER COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 7) ), "", eNone, eNone, eNone, 1500, True, "vo_supercombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1:DOF 126, DOFPulse
        Case Else:DMD CL("SUPERDUPER COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 10) ), "", eNone, eNone, eNone, 1500, True, "vo_superdupercombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1:DOF 126, DOFPulse
    End Select
    AddScore2 ComboValue(CurrentPlayer) * ComboCount
    ComboValue(CurrentPlayer) = ComboValue(CurrentPlayer) + 100000
End Sub

Sub aComboTargets_Hit(idx) 'reset the combo count if the ball hits another target/trigger
    ComboCount = 0
End Sub

'*******************
'    LOOP COMBOS
'*******************
' starts at 500K for a 2 way combo and it is doubled on each combo
' shots that count as loop combos:
' Center loop and Right Loop
' shots to the same loop also count as loops

Sub AwardLoop
    LoopCount = LoopCount + 1
    Select Case LoopCount
        Case 1: 'just starting
        Case 2:DMD CL("COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_combo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case 3:DMD CL("2X COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 2) ), "", eNone, eNone, eNone, 1500, True, "vo_2xcombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case 4:DMD CL("3X COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 3) ), "", eNone, eNone, eNone, 1500, True, "vo_3xcombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case 5:DMD CL("4X COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 4) ), "", eNone, eNone, eNone, 1500, True, "vo_4xcombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case 6:DMD CL("5X COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 5) ), "", eNone, eNone, eNone, 1500, True, "vo_5xcombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case 7:DMD CL("SUPER COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 7) ), "", eNone, eNone, eNone, 1500, True, "vo_supercombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case Else:DMD CL("SUPERDUPER COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 10) ), "", eNone, eNone, eNone, 1500, True, "vo_superdupercombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
    End Select
    AddScore2 LoopValue(CurrentPlayer) * LoopCount
    LoopValue(CurrentPlayer) = LoopValue(CurrentPlayer) + 100000
End Sub

Sub aLoopTargets_Hit(idx) 'reset the loop count if the ball hits another target/trigger
    li061.State = 0       'turn off also the super loops light
    LoopCount = 0
End Sub

'*******************
'  Teenager kill
'*******************

' the timer will change the current teenager by lighting the light on top of her

Sub TeenTimer_Timer
    Select Case RndNbr(15)
        Case 1:F001.State = 2:F002.State = 0:F003.State = 0:F004.State = 0:F005.State = 0:PlayQuote:DOF 204, DOFPulse:GF001.visible = 1:GF002.visible = 0:GF003.visible = 0:GF004.visible = 0:GF005.visible = 0
        Case 2:F001.State = 0:F002.State = 2:F003.State = 0:F004.State = 0:F005.State = 0:PlayQuote:DOF 204, DOFPulse:GF001.visible = 0:GF002.visible = 0:GF003.visible = 0:GF004.visible = 0:GF005.visible = 1
        Case 3:F001.State = 0:F002.State = 0:F003.State = 2:F004.State = 0:F005.State = 0:PlayQuote:DOF 204, DOFPulse:GF001.visible = 0:GF002.visible = 0:GF003.visible = 0:GF004.visible = 1:GF005.visible = 0
        Case 4:F001.State = 0:F002.State = 0:F003.State = 0:F004.State = 2:F005.State = 0:PlayQuote:DOF 204, DOFPulse:GF001.visible = 0:GF002.visible = 1:GF003.visible = 0:GF004.visible = 0:GF005.visible = 0
        Case 5:F001.State = 0:F002.State = 0:F003.State = 0:F004.State = 0:F005.State = 2:PlayQuote:DOF 204, DOFPulse:GF001.visible = 0:GF002.visible = 0:GF003.visible = 1:GF004.visible = 0:GF005.visible = 0
        Case Else:F001.State = 0:F002.State = 0:F003.State = 0:F004.State = 0:F005.State = 0:GF001.visible = 0:GF002.visible = 0:GF003.visible = 0:GF004.visible = 0:GF005.visible = 0
    End Select
End Sub

Sub TeenKilled 'a teen has been killed
    'DMD animation
    DMDFlush
    DMD "", "", "d_goa", eNone, eNone, eNone, 50, False, "sfx_kill" &RndNbr(10)
    DMD "", "", "d_gob", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_goc", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_god", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_goe", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gof", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gog", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_goh", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_goi", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_goj", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gok", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gol", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gom", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gon", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_goo", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gop", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_goq", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gor", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gos", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_got", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gou", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gov", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gow", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gox", eNone, eNone, eNone, 50, False, ""
    DMD CL("Y"), "", "d_go1", eNone, eNone, eNone, 100, False, ""
    DMD CL("YO"), "", "d_go2", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU"), "", "d_go1", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU "), "", "d_go2", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU F"), "", "d_go1", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FO"), "", "d_go2", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOU"), "", "d_go1", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUN"), "", "d_go2", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUND"), "", "d_go1", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUND"), CL("E"), "d_go2", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUND"), CL("EV"), "d_go1", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUND"), CL("EVI"), "d_go2", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUND"), CL("EVID"), "d_go1", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUND"), CL("EVIDE"), "d_go2", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUND"), CL("EVIDEN"), "d_go1", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUND"), CL("EVIDENC"), "d_go2", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUND"), CL("EVIDENCE"), "d_go1", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUND"), CL("EVIDENCE"), "d_go2", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU FOUND"), CL("EVIDENCE"), "", eNone, eNone, eNone, 1000, False, ""
    Select Case RndNbr(3)
        Case 1:DMD CL("EVIDENCE FOUND"), CL(FormatScore(TeensKilledValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_excellent"
        Case 2:DMD CL("EVIDENCE FOUND"), CL(FormatScore(TeensKilledValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_greatshot"
        Case 3:DMD CL("EVIDENCE FOUND"), CL(FormatScore(TeensKilledValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_welldone"
    End Select
    TeensKilled(CurrentPlayer) = TeensKilled(CurrentPlayer) + 1
    AddScore2 TeensKilledValue(CurrentPlayer)
    TeensKilledValue(CurrentPlayer) = TeensKilledValue(CurrentPlayer) + 50000
    LightEffect 5
    GiEffect 5
    'check
    If TeensKilled(CurrentPlayer) MOD 6 = 0 Then StartTommyJarvis 'start Tommy Jarvis hurry up after each 6th killed teenager
    If TeensKilled(CurrentPlayer) MOD 4 = 0 Then StartPolice      'start police after each 4th killed teenager
    If TeensKilled(CurrentPlayer) MOD 10 = 0 Then LitExtraBall    'lit the extra ball
End Sub

Sub LitExtraBall
    DMDFlush
    DMD "_", CL("EXTRA BALL IS LIT"), "", eNone, eNone, eNone, 1500, True, ""
    li070.State = 1
End Sub

'********************************
' Weapons - Playfield multiplier
'********************************

Sub CheckWeapons
    Dim a, j
    a = 0
    For j = 1 to 6
        a = a + WeaponHits(CurrentPlayer, j)
    Next
    'debug.print a
    If a = 0 Then
        UpgradeWeapons
    Else
        LightSeqWeaponLights.UpdateInterval = 25
        LightSeqWeaponLights.Play SeqBlinking, , 15, 25
        UpdateWeaponLights
        PlaySword
    End If
End Sub

Sub UpdateWeaponLights 'CurrentPlayer
    Li035.State = WeaponHits(CurrentPlayer, 1)
    li037.State = WeaponHits(CurrentPlayer, 2)
    li038.State = WeaponHits(CurrentPlayer, 3)
    li039.State = WeaponHits(CurrentPlayer, 4)
    li040.State = WeaponHits(CurrentPlayer, 5)
    Li036.State = WeaponHits(CurrentPlayer, 6)
End Sub

Sub ResetWeaponLights 'CurrentPlayer
    Dim j
    For j = 1 to 6
        WeaponHits(CurrentPlayer, j) = 1
    Next
    UpdateWeaponLights
End Sub

Sub UpgradeWeapons 'increases the playfield multiplier
    Weapons(CurrentPlayer) = Weapons(CurrentPlayer) + 1
    UpdateWeaponLights2
    AddPlayfieldMultiplier 1
    ResetWeaponLights
    aWeaponSJactive = True
    StartSuperJackpot   'turn on the lits SJ at the cabin
End Sub

Sub UpdateWeaponLights2 'collected weapons
    Select Case Weapons(CurrentPlayer)
        Case 1:li041.State = 1:li042.State = 0:li043.State = 0:li044.State = 0:li045.State = 0:li046.State = 0:li047.State = 0
        Case 2:li041.State = 1:li042.State = 1:li043.State = 0:li044.State = 0:li045.State = 0:li046.State = 0:li047.State = 0
        Case 3:li041.State = 1:li042.State = 1:li043.State = 1:li044.State = 0:li045.State = 0:li046.State = 0:li047.State = 0
        Case 4:li041.State = 1:li042.State = 1:li043.State = 1:li044.State = 1:li045.State = 0:li046.State = 0:li047.State = 0
        Case 5:li041.State = 1:li042.State = 1:li043.State = 1:li044.State = 1:li045.State = 1:li046.State = 0:li047.State = 0
        Case 6:li041.State = 1:li042.State = 1:li043.State = 1:li044.State = 1:li045.State = 1:li046.State = 1:li047.State = 0
        Case 7:li041.State = 1:li042.State = 1:li043.State = 1:li044.State = 1:li045.State = 1:li046.State = 1:li047.State = 1
    End Select
End Sub

'*******************************************
' Super Jackpot at the cabin and right Loop
'*******************************************

' 30 seconds timer to lit the Super Jackpot at the right loop
' once the Super Jackpot light is lit it will be lit until the end of the ball

Sub StartSuperJackpot 'lits the cabin's red SJ light
    li048.BlinkInterval = 160
    li048.State = 2
    SuperJackpotTimer.Enabled = 1
    SuperJackpotSpeedTimer.Enabled = 1
End Sub

Sub SuperJackpotTimer_Timer            '30 seconds hurry up to turn off the red light at the cabin
    SuperJackpotSpeedTimer.Enabled = 0 'to be sure it is stopped
    SuperJackpotTimer.Enabled = 0
    li048.State = 0
End Sub

Sub SuperJackpotSpeedTimer_Timer 'after 25 seconds speed opp the blinking of the lit SJ red cabin light
    DMDFlush
    DMD "_", CL("HURRY UP"), "_", eNone, eBlink, eNone, 1500, True, "vo_hurryup"
    SuperJackpotSpeedTimer.Enabled = 0
    li048.BlinkInterval = 80
    li048.State = 2
End Sub

'*************************
' Tommy Jarvis - Hurry up
'*************************
' it starts after each 6 teenagers killed
' a 30 seconds hurry up starts at the right ramp
' hit the right ramp to throw a deadly blow to your archie enemy
' fail and your flippers will die for 3 seconds

Sub StartTommyJarvis 'Tommy's hurry up
    DMDFlush
    DMD CL("THE CORE 4"), CL("HURRY UP"), "d_border", eNone, eNone, eNone, 2500, True, "vo_shoottherightramp" &RndNbr(2)
    bTommyStarted = True
    FlashEffect 7
    Li030.BlinkInterval = 160
    Li030.State = 2
    TommyCount = 1
    TommyValue = 500000
    TommyHurryUpTimer.Enabled = 1
End Sub

Sub StopTommyJarvis
    TommyHurryUpTimer.Enabled = 0
    TommyRecoverTimer.Enabled = 0
    bTommyStarted = False
    LightSeqTopFlashers.StopPlay
    Li030.State = 0
    TommyCount = 1
End Sub

Sub TommyHurryUpTimer_Timer '30 seconds, runs once a second
    TommyCount = TommyCount + 1
    If TommyCount = 20 Then 'speed up the Light
        DMDFlush
        DMD "_", CL("HURRY UP"), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_timerunningout"
        Li030.BlinkInterval = 80
        Li030.State = 2
    End If
    If TommyCount = 31 Then 'the 30 seconds are over
        TommyHurryUpTimer.Enabled = 0
        DMDFlush
        DMD CL("THE CORE 4"), CL("FAILED"), "d_border", eNone, eNone, eNone, 2500, True, "vo_timeisup"
        TommyRecoverTimer.Enabled = 1 '... start the 3 seconds timer to enable the table again
    End If
End Sub

Sub TommyRecoverTimer_Timer 'after disabling the table for 3 seconds then enable it
    TommyRecoverTimer.Enabled = 0
    StopTommyJarvis
    'DisableFlippers False
End Sub

Sub AwardTommyJackpot() 'scores the seconds multiplied by 500.000, the longer you wait the higher the score
    Dim a
    a = TommyCount * 500000
    Select Case RndNbr(3)
        Case 1:DMD CL("THE CORE 4 JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, ""
        Case 2:DMD CL("THE CORE 4 JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, ""
        Case 3:DMD CL("THE CORE 4 JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, ""
    End Select
    DOF 126, DOFPulse
    AddScore2 a
    LightEffect 2
    GiEffect 2
End Sub

'***********************
' The Police - Hurry up
'***********************
' similar to Tommy hurry up, but different score
' it starts after 5 police hits, 2 killed counselors or 4 teenagers
' 30 seconds to hit 2 ramps or 3 police targets
' scores 500.000 for each left second
' fail and your flippers will die for 3 seconds

Sub StartThePoliceTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    StartThePoliceTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    StartPolice
End Sub


Sub StartPolice 'police hurry up
    bPoliceStarted = True
    PoliceL1.BlinkInterval = 160
    PoliceL2.BlinkInterval = 160
    Li033.BlinkInterval = 160
    PoliceL1.State = 2
    PoliceL2.State = 2
    Li033.State = 2
    PlaySound "sfx_siren1", -1
    PoliceCount = 30
    PoliceHurryUpTimer.Enabled = 1
    PoliceRampHits = 0   'reset the count
    PoliceTargetHits = 0 'reset the count
End Sub

Sub StopPolice
    PoliceHurryUpTimer.Enabled = 0
    PoliceRecoverTimer.Enabled = 0
    bPoliceStarted = False
    StopSound "sfx_siren1"
    PoliceL1.State = 0
    PoliceL2.State = 0
    Li033.State = 0
    PoliceRampHits = 0       'reset the count
    PoliceTargetHits = 0     'reset the count
    PoliceCount = 0
    'DisableFlippers False    'ensure they are not disabled
End Sub

Sub PoliceHurryUpTimer_Timer '30 seconds, runs once a second
    PoliceCount = PoliceCount - 1
    If PoliceCount = 8 Then  'speed up the Lights
        DMDFlush
        DMD "_", CL("HURRY UP"), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_timerunningout"
        PoliceL1.BlinkInterval = 80
        PoliceL2.BlinkInterval = 80
        Li033.BlinkInterval = 80
        PoliceL1.State = 2
        PoliceL2.State = 2
        Li033.State = 2
    End If
    If PoliceCount = 0 Then
        PoliceHurryUpTimer.Enabled = 0
        DMDFlush
        DMD CL("THE POLICE"), CL("ARE DEAD"), "d_border", eNone, eNone, eNone, 2500, True, "vo_holyshityoushootme"
        PoliceRecoverTimer.Enabled = 1 '... start the 3 seconds timer to enable the table again
    End If
End Sub

Sub PoliceRecoverTimer_Timer 'after disabling the table for 3 seconds then enable it again
    PoliceRecoverTimer.Enabled = 0
    StopPolice
End Sub

Sub AwardPoliceJackpot() 'scores the seconds left multiplied by 500.000
    Dim a
    DOF 126, DOFPulse
    a = PoliceCount * 500000
    Select Case RndNbr(3)
        Case 1:DMD CL("POLICE JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, ""
        Case 2:DMD CL("POLICE JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, ""
        Case 3:DMD CL("POLICE JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, ""
    End Select
    AddScore2 a
    LightEffect 2
    GiEffect 2
End Sub

Sub DisableFlippers(enabled)
    If enabled Then
        SolLFlipper 0
        SolRFlipper 0
        bFlippersEnabled = 0
    Else
        bFlippersEnabled = 1
    End If
End Sub

'***********************************
' Jason Multiball - Main multiball
'***********************************
' shoot the cabin to start locking
' lock 3 balls under the cabin to start
' all main shots are Lit
' 15 seconds ball saver
' each succesive shot will increase the color and score
' Blue shots: 1 million points
' Green Shots: 2 million points
' Yellow shots: 3 million points
' Orange shots: 4 million points
' Red shots: 5 million points

Sub StartJasonMultiball
    Dim i
    If bMultiBallMode Then Exit Sub 'do not start if already in a multiball mode
    bJasonMBStarted = True
    EnableBallSaver 15
    LittleGFEyesGlowingOn
    KillAllMovies
    ColorChangeGreen()
    MBLettersOn
    pupmultiballsid
    Fulldmdsid
    Toppermodesid
    SidT.Enabled = True
    bgMusicMute
    door2triggersoff
    SmallFlimReelTimer.enabled =True
    FilmReelLight.State = 2
    MBlocklight.State = 1
    MBKillSwitchRestart.enabled =False
    Kicker001.enabled =False
    vpmtimer.addtimer 2000, "StartSidReel '"
    DMDFlush
    DMD "_", CL("SIDNEY MULTIBALL"), "_", eNone, eNone, eNone, 2500, True, "vo_multiball1"
    AddMultiball 2
    For i = 1 to 8
        ArrowMultiPlier(i) = 1
    Next
    UpdateArrowLights
    li016.State = 2
End Sub


Sub StopJasonMultiball
    bJasonMBStarted = False
    BallsInLock(CurrentPlayer) = 0
    TurnOffArrows
    ColorChangeBack()
    MBLettersOff
    KillAllMovies
    mickeyNromanOff
    StartItUp()
    ModeLitLightItUp
    LittleGFEyesGlowingOFF
    pupbase
    Fulldmdbase
    Topperbase
    pupBGmusicrestart
    door2triggersbackon
    MBlocklight.State = 0
    SmallFlimReelTimer.enabled =False
    FilmReelLight.State = 0
    GFModeTarget.Isdropped = 1
    GF006.Z =-90
    li016.State = 0
End Sub


Sub StartItUp()
     if NewMode Then
     vpmtimer.addtimer 2000, "StartMode '"
    end if 
End Sub


Sub Kill17Again
    Trigger017.enabled =False
End Sub


Sub StartSidReel
    MovieScreen33Animation.Enabled = True
    SmallFlimReelTimer.enabled =True
    FilmReelLight.State = 2
End Sub
    



Sub UpdateArrowLights 'sets the color of the arrows according to the jackpot multiplier
    SetLightColor li062, ArrowMultiPlier(1), 2
    SetLightColor li064, ArrowMultiPlier(2), 2
    SetLightColor li053, ArrowMultiPlier(3), 2
    SetLightColor li066, ArrowMultiPlier(4), 2
    SetLightColor li067, ArrowMultiPlier(5), 2
    SetLightColor li068, ArrowMultiPlier(6), 2
    SetLightColor li052, ArrowMultiPlier(7), 2
    SetLightColor li059, ArrowMultiPlier(8), 2
End Sub

'***********************************
' Freddy Kruger Multiball - Ramps
'***********************************
' starts after 3 counselors are killed
' shoot the blue arrows at the ramps to collect jackpots
' this will build the super jackpot at the right loop, shoot the cabin for a better shot

Sub StartTaraMBTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    StartTaraMBTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    StartFreddyMultiball
End Sub


Sub StartTara2MBTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    StartTara2MBTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    StartFreddyMultiball
End Sub


Sub StartFreddyMultiball
    Dim i
    If bMultiBallMode Then Exit Sub 'do not start if already in a multiball mode
    bFreddyMBStarted = True
    EnableBallSaver 20
    LittleGFEyesGlowingOn
    ColorChangeGreen()
    MBLettersOn
    KillAllMovies
    KillSwitchRestart
    pupmultiballtara
    Fulldmdtara
    Toppermodetara
    TaraT.Enabled = True
    bgMusicMute
    door2triggersoff
    SidMBTrigger.enabled =False
    MBlocklight.State = 1
    GFModeTarget.Isdropped = 0
    GF006.Z =5
    MBKillSwitchRestart.enabled =False
    Kicker001.enabled =False
    vpmtimer.addtimer 2000, "StartTaraReel '"
    DMDFlush
    DMD "_", CL("TARA MULTIBALL"), "_", eNone, eNone, eNone, 2500, True, "vo_multiball1"
    AddMultiball 2
    SetLightColor li064, blue, 2
    SetLightColor li068, blue, 2
    li015.State = 2
    MBlocklight.State = 1
End Sub

Sub StopFreddyMultiball
    bFreddyMBStarted = False
    ColorChangeBack()
    MBLettersOff
    LittleGFEyesGlowingOFF
    KillAllMovies
    ModeLitLightItUp
    RestartMBKillSwitch
    pupbase
    Fulldmdbase
    Topperbase
    pupBGmusicrestart
    door2triggersbackon
    SidMBTrigger.enabled =True
    MBlocklight.State = 0
    SmallFlimReelTimer.enabled =False
    FilmReelLight.State = 0
    GFModeTarget.Isdropped = 1
    GF006.Z =-90
    li064.State = 0
    li068.State = 0
    li015.State = 0
End Sub


Sub StartTaraReel
    MovieScreen29Animation.Enabled = True
    SmallFlimReelTimer.enabled =True
    FilmReelLight.State = 2
End Sub

'***********************************
' Michael Myers Multiball - Spinners
'***********************************
' starts randomly after 3 counselors are killed
' shoot the blue arrows at the spinners to collect jackpots
' this will build the super jackpot at the lower right loop
' shoot it to collect it

Sub StartSamMBTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    StartSamMBTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    StartMichaelMultiball
End Sub


Sub StartSam2MBTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    StartSam2MBTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    StartMichaelMultiball
End Sub


Sub StartMichaelMultiball
    Dim i
    If bMultiBallMode Then Exit Sub 'do not start if already in a multiball mode
    bMichaelMBStarted = True
    EnableBallSaver 20
    pupmultiballsam
    Fulldmdsam
    Toppermodesam
    SamT.Enabled = True
    bgMusicMute
    LittleGFEyesGlowingOn
    MBLettersOn
    ColorChangeGreen()
    KillAllMovies
    KillSwitchRestart
    door2triggersoff
    SidMBTrigger.enabled =False
    MBlocklight.State = 1
    GFModeTarget.Isdropped = 0
    GF006.Z =5
    MBKillSwitchRestart.enabled =False
    Kicker001.enabled =False
    vpmtimer.addtimer 2000, "StartSamReel '"
    DMDFlush
    DMD "_", CL("SAMANTHA MULTIBALL"), "_", eNone, eNone, eNone, 2500, True, "vo_multiball1"
    AddMultiball 2
    SetLightColor li062, blue, 2
    SetLightColor li067, blue, 2
    SetLightColor li059, blue, 2
    li075.State = 2
End Sub

Sub StopMichaelMultiball
    bMichaelMBStarted = False
    LittleGFEyesGlowingOFF
    MBLettersOff
    ColorChangeBack()
    KillAllMovies
    ModeLitLightItUp
    RestartMBKillSwitch
    pupbase
    Fulldmdbase
    Topperbase
    pupBGmusicrestart
    door2triggersbackon
    SidMBTrigger.enabled =True
    li032.State = 0
    MBlocklight.State = 0
    SmallFlimReelTimer.enabled =False
    FilmReelLight.State = 0
    GFModeTarget.Isdropped = 1
    GF006.Z =-90
    li062.State = 0
    li067.State = 0
    li059.State = 0
    li075.State = 0
End Sub


Sub StartSamReel
    MovieScreen31Animation.Enabled = True
    SmallFlimReelTimer.enabled =True
    FilmReelLight.State = 2
End Sub



'****************************
' Mystery award at the scoop
'****************************
' this is a kind of award after completing the inlane and outlanes

Sub CheckMystery 'if all the inlanes and outlanes are lit then lit the mystery award
    dim i
    If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) = 4 Then
        DMD "_", CL("MYSTERY IS LIT"), "", eNone, eNone, eNone, 1000, True, ""
        li078.State = 1
        ' and reset the lights
        For i = 1 to 4
            Mystery(CurrentPlayer, i) = 0
        Next
    End If
    UpdateMysteryLights
End Sub

Sub AwardMystery 'mostly points but sometimes it will lit the special or the extra ball
    Dim tmp
    FlashEffect 1
    Select Case RndNbr(20)
        Case 1:LitExtraBall                   'lit extra ball
        Case 2:LitSpecial                     'lit special
        Case Else:
            tmp = 250000 + RndNbr(25) * 10000 'add from 250.000 to 500.000
            Select Case RndNbr(4)
                Case 1:DMD CL("MYSTERY SCORE"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 2000, True, "vo_notbad"
                Case 2:DMD CL("MYSTERY SCORE"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 2000, True, "vo_excellentscore"
                Case 3:DMD CL("MYSTERY SCORE"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 2000, True, "vo_nowyouaregettinghot"
                Case 4:DMD CL("MYSTERY SCORE"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 2000, True, "vo_welldone"
            End Select
    End Select
    vpmtimer.addtimer 3500, "kickBallOut '"
End Sub

'**********************************
'   Modes - Hunting the counselors
'**********************************

' This table has 14 modes which will be selected at random
' After killing all counselors you'll a surprise :)

' current active Mode number is stored in Mode(CurrentPlayer,0)
' select a new random Mode if none is active
Sub SelectMode
    Dim i
    If Mode(CurrentPlayer, 0) = 0 Then
        ' reset the Modes that are not finished
        For i = 1 to 14
            If Mode(CurrentPlayer, i) = 2 Then Mode(CurrentPlayer, i) = 0
        Next
        NewMode = RndNbr(14)
        do while Mode(CurrentPlayer, NewMode) <> 0
            NewMode = RndNbr(14)
        loop
        Mode(CurrentPlayer, NewMode) = 2
        Li076.State = 2 'Start hunting light at the scoop
        UpdateModeLights
    'debug.print "newmode " & newmode
    End If
End Sub

' Update the lights according to the mode's state
Sub UpdateModeLights
    li014.State = Mode(CurrentPlayer, 1):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li013.State = Mode(CurrentPlayer, 2):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li012.State = Mode(CurrentPlayer, 3):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li011.State = Mode(CurrentPlayer, 4):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li010.State = Mode(CurrentPlayer, 5):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li009.State = Mode(CurrentPlayer, 6):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li008.State = Mode(CurrentPlayer, 7):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li007.State = Mode(CurrentPlayer, 8):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li006.State = Mode(CurrentPlayer, 9):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li005.State = Mode(CurrentPlayer, 10):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li004.State = Mode(CurrentPlayer, 11):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li001.State = Mode(CurrentPlayer, 12):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li002.State = Mode(CurrentPlayer, 13):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
    li003.State = Mode(CurrentPlayer, 14):SmallFlimReelTimer.enabled =False:FilmReelLight.State = 0
End Sub

' Starting a mode means to setup some lights and variables, maybe timers
' Mode lights will always blink during an active mode
Sub StartMode
    Mode(CurrentPlayer, 0) = NewMode
    Li076.State = 0
    MBlocklight.State = 1
    SidMBTrigger.enabled =False
    bgMusicMute
    openemup
    StopPolice
    EnableBallSaver 20
    'ChangeSong
    PlaySound "Film Projector Start"
    ReadyToKill = False
    Select Case NewMode
        Case 1 'Billy Loomis = Super Spinners
            DMDFlush
            DMD CL("KILL BILLY LOOMIS"), CL("SHOOT THE SPINNERS"), "", eNone, eNone, eNone, 1500, True, "vo_shootthespinners"
            SpinCount = 0
            SetLightColor li062, amber, 2
            SetLightColor li067, amber, 2
            SetLightColor li059, amber, 2
            SpinNeeded = 70
            ColorChange()
            ModeLitTrigger.enabled =False
            Trigger017.enabled =False
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            OldCellRingFlasher.visible = 1
            OldCellRingFlasher.imageA = "CallBilly"
            MovieScreen3Animation.Enabled = True
            OldCellShakeAgain
            KillOldCallFlasher
            BigGFEyesGlowingOn
            ModeRingTriggersOff
            pupmodebilly
            Fulldmdbilly
            Toppermodebilly
            BillyT.Enabled = True
            'KillSwitchRestart
            'SwitchKillReset
        Case 2 'Stu Macher = 5 Targets at semi random
            DMDFlush
            DMD CL("KILL STU MACHER "), CL("HIT THE LIT TARGETS"), "", eNone, eNone, eNone, 1500, True, "vo_shootthetargets"
            TargetModeHits = 0
            'Turn off all blue targets
            Li031.State = 0
            Li049.State = 0
            Li050.State = 0
            Li051.State = 0
            Li058.State = 0
            Li057.State = 0
            Li079.State = 0
            ColorChange()
            Trigger017.enabled =False
            ModeLitTrigger.enabled =False
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            OldCellRingFlasher.visible = 1
            OldCellRingFlasher.imageA = "CallStu"
            MovieScreen5Animation.Enabled = True
            charlietarget5.Enabled = True
            charlietarget6.Enabled = True
            OldCellShakeAgain
            KillOldCallFlasher
            BigGFEyesGlowingOn
            ModeRingTriggersOff
            pupmodestu
            Fulldmdstu
            Toppermodestu
            StuT.Enabled = True
            'KillSwitchRestart
            'SwitchKillReset
            Select Case RndNbr(1) 'select 4 blue targets
                Case 1:Li049.State = 2:Li050.State = 2:li051.State = 2:Li057.State = 2:charlielight5.State = 2:charlielight6.State = 2:charlielight05.State = 2:charlielight06.State = 2
                'Case 2:Li049.State = 2:Li050.State = 2:li058.State = 2:Li057.State = 2
                'Case 3:Li031.State = 2:Li049.State = 2:li051.State = 2:Li057.State = 2
                'Case 4:Li031.State = 2:Li049.State = 2:li050.State = 2:Li051.State = 2
            End Select
        Case 3 'Mickey = 5 Flashing Shots 90 seconds to complete
            DMDFlush
            DMD CL("KILL MICKEY"), CL("SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            DMD CL("KILL MICKEY"), CL("YOU HAVE 2 MINUTES"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li062, amber, 2
            SetLightColor li064, amber, 2
            SetLightColor li066, amber, 2
            SetLightColor li067, amber, 2
            SetLightColor li068, amber, 2
            EndModeCountdown = 126
            EndModeTimer.Enabled = 1
            Trigger017.enabled =False
            ModeLitTrigger.enabled =False
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            OldCellRingFlasher.visible = 1
            OldCellRingFlasher.imageA = "CallMickey"
            MovieScreen11Animation.Enabled = True
            MickeyTimer.Enabled = True
            pupmodemickey
            Fulldmdmickey
            Toppermodemickey
            MickeyT.Enabled = True
            OldCellShakeAgain
            KillOldCallFlasher
            BigGFEyesGlowingOn
            ModeRingTriggersOff
            ColorChange()
            'KillSwitchRestart
            'SwitchKillReset
        Case 4 'MRS Loomis = 5 Orbits 'uses trigger008 to detect the completed orbits
            DMDFlush
            DMD CL("KILL MRS LOOMIS"), CL("SHOOT 9 ORBITS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li067, amber, 2
            SetLightColor li052, amber, 2
            Trigger017.enabled =False
            ModeLitTrigger.enabled =False
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            OldCellRingFlasher.visible = 1
            OldCellRingFlasher.imageA = "CallMrsLoomis"
            MovieScreen13Animation.Enabled = True
            OldCellShakeAgain
            KillOldCallFlasher
            ColorChangePink()
            BigGFEyesGlowingPinkOn
            ModeRingTriggersOff
            pupmodemloomis
            Fulldmdbmloomis
            Toppermodemloomis
            MloomisT.Enabled = True
            'KillSwitchRestart
            'SwitchKillReset
        Case 5 'Roman = Shoot 4 lights 60 seconds, all shots are lit, after 4 shot lit the cabin to kill her & collect jackpot
            DMDFlush
            DMD CL("KILL ROMAN"), CL("SHOOT 9 LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            DMD CL("KILL ROMAN"), CL("YOU HAVE 2 MINUTES"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            TurnonArrows amber
            EndModeCountdown = 126
            EndModeTimer.Enabled = 1
            Trigger017.enabled =False
            ModeLitTrigger.enabled =False
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            OldCellRingFlasher.visible = 1
            OldCellRingFlasher.imageA = "CallRoman"
            MovieScreen15Animation.Enabled = True
            MickeyTimer.Enabled = True
            pupmoderoman
            Fulldmdroman
            Toppermoderoman
            RomanT.Enabled = True
            OldCellShakeAgain
            KillOldCallFlasher
            ColorChange()
            BigGFEyesGlowingOn
            ModeRingTriggersOff
            'KillSwitchRestart
            'SwitchKillReset
        Case 6 'Jill = Shoot the ramps
            DMDFlush
            DMD CL("KILL JILL"), CL("SHOOT 5 RAMPS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li064, amber, 2
            SetLightColor li068, amber, 2
            ColorChangePink()
            Trigger017.enabled =False
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            ModeLitTrigger.enabled =False
            IphoneCallFlasher.visible = 1
            IphoneCallFlasher.imageA = "CallJill"
            MovieScreen7Animation.Enabled = True
            IphoneShakeAgain
            BigGFEyesGlowingPinkOn
            ModeRingTriggersOff
            pupmodejill
            Fulldmdjill
            Toppermodejill
            JillT.Enabled = True
            KillIphoneCallFlasher
            'KillSwitchRestart
            'SwitchKillReset
        Case 7 'Charlie=  Target Frenzy
            DMDFlush
            DMD CL("KILL CHARLIE"), CL("SHOOT 6 LIT TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            'Turn off all blue targets
            Trigger017.enabled =False
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            ModeLitTrigger.enabled =False
            IphoneCallFlasher.visible = 1
            IphoneCallFlasher.imageA = "CallCharlie"
            MovieScreen17Animation.Enabled = True
            charlietarget5.Enabled = True
            charlietarget6.Enabled = True
            pupmodecharlie
            Fulldmdcharlie
            Toppermodecharlie
            CharlieT.Enabled = True
            IphoneShakeAgain
            KillIphoneCallFlasher
            ColorChange()
            BigGFEyesGlowingOn
            ModeRingTriggersOff
            TurnOffBlueTargets
            Select Case RndNbr(1) 'select 4 blue targets
                Case 1:Li049.State = 2:Li050.State = 2:li051.State = 2:Li057.State = 2:charlielight5.State = 2:charlielight6.State = 2:charlielight05.State = 2:charlielight06.State = 2
                'Case 2:Li049.State = 2:Li050.State = 2:li058.State = 2:Li057.State = 2:charlielight5.State = 2:charlielight6.State = 2:charlielight05.State = 2:charlielight06.State = 2
                'Case 3:Li031.State = 2:Li049.State = 2:li051.State = 2:Li057.State = 2:charlielight5.State = 2:charlielight6.State = 2:charlielight05.State = 2:charlielight06.State = 2
                'Case 4:Li031.State = 2:Li049.State = 2:li050.State = 2:Li051.State = 2:charlielight5.State = 2:charlielight6.State = 2:charlielight05.State = 2:charlielight06.State = 2
            End Select
        Case 8 'Richie = 5 Targets in rotation
            DMDFlush
            DMD CL("KILL RICHIE"), CL("SHOOT 5 LIT TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            'Turn off all blue targets
            TurnOffBlueTargets
            'Start timer to rotate through the targets
            BlueTargetsCount = RndNbr(7)
            BlueTargetsTimer_Timer
            ColorChange()
            Trigger017.enabled =False
            ModeLitTrigger.enabled =False
            BlueTargetsTimer.Enabled = 1
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            IphoneCallFlasher.visible = 1
            IphoneCallFlasher.imageA = "CallRichie"
            MovieScreen19Animation.Enabled = True
            pupmoderichie
            Fulldmdrichie
            Toppermoderichie
            RichieT.Enabled = True
            IphoneShakeAgain
            BigGFEyesGlowingOn
            ModeRingTriggersOff
            KillIphoneCallFlasher
            'KillSwitchRestart
            'SwitchKillReset
        Case 9 'Amber = Magnet
            DMDFlush
            DMD CL("KILL AMBER"), CL("SHOOT THE MAGNET"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            li065.State = 2
            ColorChangePink()
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            ModeLitTrigger.enabled =False
            Trigger017.enabled =False
            IphoneCallFlasher.visible = 1
            IphoneCallFlasher.imageA = "CallAmber"
            MovieScreen1Animation.Enabled = True
            pupmodeamber
            Fulldmdamber
            Toppermodeamber
            AmberT.Enabled = True
            IphoneShakeAgain
            BigGFEyesGlowingPinkOn
            ModeRingTriggersOff
            KillIphoneCallFlasher
            'KillSwitchRestart
            'SwitchKillReset
        Case 10 'Quinn = Ramps and Orbits - all lights lit
            DMDFlush
            DMD CL("KILL QUINN"), CL("SHOOT RAMPS ORBITS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li064, amber, 2
            SetLightColor li067, amber, 2
            SetLightColor li068, amber, 2
            SetLightColor li052, amber, 2
            ColorChangePink()
            Trigger017.enabled =False
            ModeLitTrigger.enabled =False
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            IphoneCallFlasher.visible = 1
            IphoneCallFlasher.imageA = "CallQuinn"
            MovieScreen9Animation.Enabled = True
            IphoneShakeAgain
            BigGFEyesGlowingPinkOn
            ModeRingTriggersOff
            pupmodequinn
            Fulldmdquinn
            Toppermodequinn
            QuinnT.Enabled = True
            KillIphoneCallFlasher
            'KillSwitchRestart
            'SwitchKillReset
        Case 11 'Ethan = 5 Blue Targets at random
            DMDFlush
            DMD CL("KILL ETHAN"), CL("SHOOT BLUE TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            'Turn off all blue targets
            TurnOffBlueTargets
            'Start timer to rotate through the targets
            BlueTargetsCount = RndNbr(7)
            BlueTargetsTimer_Timer
            ColorChange()
            Trigger017.enabled =False
            ModeLitTrigger.enabled =False
            BlueTargetsTimer.Enabled = 1
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            IphoneCallFlasher.visible = 1
            IphoneCallFlasher.imageA = "CallEthan"
            MovieScreen21Animation.Enabled = True
            IphoneShakeAgain
            BigGFEyesGlowingOn
            ModeRingTriggersOff
            pupmodeethan
            Fulldmdethan
            Toppermodeethan
            EthanT.Enabled = True
            KillIphoneCallFlasher
            'KillSwitchRestart
            'SwitchKillReset
        Case 12 'Wayne = Super Spinners at random
            DMDFlush
            DMD CL("KILL WAYNE"), CL("SHOOT THE SPINNERS"), "", eNone, eNone, eNone, 1500, True, ""
            SpinCount = 0
            SpinNeeded = 90
            SpinnersTimer_Timer
            ColorChange()
            Trigger017.enabled =False
            ModeLitTrigger.enabled =False
            SpinnersTimer.Enabled = 1
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            IphoneCallFlasher.visible = 1
            IphoneCallFlasher.imageA = "CallWayne"
            MovieScreen23Animation.Enabled = True
            pupmodewayne
            Fulldmdwayne
            Toppermodewayne
            WayneT.Enabled = True
            IphoneShakeAgain
            BigGFEyesGlowingOn
            ModeRingTriggersOff
            KillIphoneCallFlasher
            'KillSwitchRestart
            'SwitchKillReset
        Case 13 'Dewey = Follow the Lights All main Shots 1 at a time in rotation
            DMDFlush
            DMD CL("SAVE DEWEY"), CL("SHOOT LIT LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            ArrowsCount2 = 0
            SetLightColor li062, amber, 0
            SetLightColor li064, amber, 0
            SetLightColor li053, amber, 0
            SetLightColor li066, amber, 0
            SetLightColor li067, amber, 0
            SetLightColor li068, amber, 0
            SetLightColor li052, amber, 0
            SetLightColor li059, amber, 0
            FollowTheLights2_Timer
            ModeThreeAllOn
            ModeRingTriggersOff
            ColorChange()
            Trigger017.enabled =False
            ModeLitTrigger.enabled =False
            FollowTheLights.Enabled = 1
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            OldCellRingFlasher.visible = 1
            OldCellRingFlasher.imageA = "CallDewey"
            MovieScreen25Animation.Enabled = True
            Trigger011.Enabled = False
            pupmodedewey
            Fulldmddewey
            Toppermodedewey
            DeweyT.Enabled = True
            OldCellShakeAgain
            KillOldCallFlasher
            BigGFEyesGlowingOn
            'KillSwitchRestart
            'SwitchKillReset
        Case 14 'Gale = Follow the Lights All main shots at random
            DMDFlush
            DMD CL("SAVE GALE"), CL("SHOOT LIT LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            ArrowsCount2 = 0
            SetLightColor li062, amber, 0
            SetLightColor li064, amber, 0
            SetLightColor li053, amber, 0
            SetLightColor li066, amber, 0
            SetLightColor li067, amber, 0
            SetLightColor li068, amber, 0
            SetLightColor li052, amber, 0
            SetLightColor li059, amber, 0
            FollowTheLights2_Timer
            ModeThreeAllOn
            ColorChangePink()
            Trigger017.enabled =False
            ModeLitTrigger.enabled =False
            FollowTheLights.Enabled = 1
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            OldCellRingFlasher.visible = 1
            OldCellRingFlasher.imageA = "CallGale"
            MovieScreen27Animation.Enabled = True
            Trigger011.Enabled = False
            pupmodegale
            Fulldmdgale
            Toppermodegale
            GaleT.Enabled = True
            OldCellShakeAgain
            KillOldCallFlasher
            BigGFEyesGlowingPinkOn
            ModeRingTriggersOff
            'KillSwitchRestart
            'SwitchKillReset
        Case 15 'the big final mode: 5 ball multiball all jackpots are lit, no timer, score jackpots until the last multiball
            DMDFlush
            DMD CL("THE BIG FINALE"), CL("SHOOT THE JACKPOTS"), "", eNone, eNone, eNone, 1500, True, ""
            'AddMultiball 5
            SetLightColor WizLight001, red, 2
            SetLightColor WizLight002, red, 2
            SetLightColor WizLight003, red, 2
            SetLightColor WizLight004, red, 2
            SetLightColor WizLight005, red, 2
            SetLightColor WizLight006, red, 2
            SetLightColor WizLight007, red, 2
            SetLightColor WizLight008, red, 2
            WizModeTriggersNLightsOn
            WizKillTriggersNLights
            pupmodewizard
            Fulldmdwizard
            Toppermodewizard
            WizT.Enabled = True
            ChangeGi Red
            ChangeGIIntensity 2
            BigGFEyesGlowingOn
            ModeRingTriggersOff
            wizardprekickeron
            StopPolice
            DisableFlippers True
            BigCellRingTrigger.enabled =False
            BigIphoneRingTrigger.enabled =False
            'SidMBSwitchTimer.Enabled = True
            SidMBTrigger.enabled =False
            MovieScreenBlank.visible = True
            MovieScreen35Animation.Enabled = True
            WizardDrainKicker.Enabled = 1
            WizTarget001.Isdropped = 0
            WizTarget002.Isdropped = 0
            WizardMBpreTimer.Enabled = True
            GFShow2Trigger.enabled =False
            ModeLitTrigger.enabled =False
            Trigger017.enabled =False
            Trigger013.enabled =False
            SmallFlimReelTimer.enabled =True
            FilmReelLight.State = 2
            'KillSwitchRestart
            'SwitchKillReset
    End Select
    ' kick out the ball
    If BallsinHole Then
        vpmtimer.addtimer 2500, "kickBallOut '"
    End If
End Sub

Sub CheckWinMode
    DOF 126, DOFPulse
    LightSeqInserts.StopPlay 'stop the light effects before starting again so they don't play too long.
    LightEffect 3
    Select Case NewMode
        Case 1
            Addscore 10000
            If SpinCount = SpinNeeded Then
                DMDFlush
                DMD CL("GET EM GET EM"), CL("SHOOT THE SCOOP"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
                li029.State = 2 'lit the Hunter Jackpot at the scoop
                li062.State = 0 'turn off the spinner lights
                li067.State = 0
                li059.State = 0
                ReadyToKill = True
            End If
        Case 2
            Addscore 150000
            If TargetModeHits = 6 Then
                DMDFlush
                DMD CL("GET EM GET EM"), CL("SHOOT THE MAGNET"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
                li065.State = 2
                ReadyToKill = True
            End If
        Case 3
            Addscore 150000
            If TargetModeHits = 5 Then
                DMDFlush
                DMD CL("GET EM GET EM"), CL("SHOOT THE SCOOP"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
                li029.State = 2
                ReadyToKill = True
            End If
        Case 4
            Addscore 150000
            If TargetModeHits = 9 Then WinMode:End if
        Case 5
            Addscore 150000
            If TargetModeHits = 9 Then      'lit the cabin for the kill & jackpot
                DMDFlush
                DMD CL("GET EM GET EM"), CL("SHOOT THE BASEMENT"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
                SetLightColor li066, red, 2 'change the color to red
                ReadyToKill = True
            End If
        Case 6
            Addscore 150000
            If TargetModeHits = 5 Then WinMode:End if
        Case 7
            Addscore 150000
            If TargetModeHits = 6 Then WinMode:End if
        Case 8
            Addscore 150000
            If TargetModeHits = 5 Then
                WinMode
            Else
                BlueTargetsTimer_Timer 'to lit another target
            End if
        Case 9
            Addscore 150000
            If TargetModeHits = 9 Then WinMode:End if
        Case 10
            Addscore 150000
            If TargetModeHits = 9 Then WinMode:End if
        Case 11
            Addscore 150000
            If TargetModeHits = 6 Then
                WinMode
            Else
                BlueTargetsTimer_Timer 'to lit another target
            End if
        Case 12
            Addscore 10000
            If SpinCount = SpinNeeded Then
                WinMode
                SpinnersTimer.Enabled = 0
            End If
        Case 13
            Addscore 150000
            If TargetModeHits = 6 Then
                WinMode
            Else
                FollowtheLights_Timer 'to lit another target
            End if
        Case 14
            Addscore 150000
            If TargetModeHits = 6 Then
                WinMode
            Else
                FollowtheLights_Timer 'to lit another target
            End if
    End Select
End Sub

'called after completing a mode
Sub WinMode
    CounselorsKilled(CurrentPlayer) = CounselorsKilled(CurrentPlayer) + 1
    Select Case NewMode
        Case 1
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED BILLY"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED BILLY"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            BillyET.Enabled = True
            MainModeEndTimer.Enabled = True
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmodebilly
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 2
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED STU"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED STU"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            CharlieLnTOff
            StuET.Enabled = True
            MainModeEndTimer.Enabled = True
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmodestu
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 3
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED MICKEY"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED MICKEY"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            MickeyET.Enabled = True
            MainModeEndTimer.Enabled = True
            MickeyTimer.Enabled = False
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmodemickey
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 4
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED MRS LOOMIS"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED MRS LOOMIS"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            MloomisET.Enabled = True
            MainModeEndTimer.Enabled = True
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmodemloomis
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 5
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED ROMAN"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED ROMAN"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            RomanET.Enabled = True
            MickeyTimer.Enabled = False
            MainModeEndTimer.Enabled = True
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmoderoman
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 6
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED JILL"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED JILL"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            JillET.Enabled = True
            MainModeEndTimer.Enabled = True
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmodejill
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 7
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED CHARLIE"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED CHARLIE"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            CharlieLnTOff
            CharlieET.Enabled = True
            MainModeEndTimer.Enabled = True
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmodecharlie
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 8
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED RICHIE"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED RICHIE"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            RichieET.Enabled = True
            MainModeEndTimer.Enabled = True
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmoderichie
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
            'GoSamGo
        Case 9
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED AMBER"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED AMBER"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            AmberET.Enabled = True
            MainModeEndTimer.Enabled = True
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmodeamber
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 10
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED QUINN"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED QUINN"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            QuinnET.Enabled = True
            MainModeEndTimer.Enabled = True
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmodequinn
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 11
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED ETHAN"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED ETHAN"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            EthanET.Enabled = True
            MainModeEndTimer.Enabled = True
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmodeethan
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 12
            DMDFlush
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "" 
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill14", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill15", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill16", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill17", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill18", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill19", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill20", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU KILLED WAYNE"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED WAYNE"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            WayneET.Enabled = True
            MainModeEndTimer.Enabled = True
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            EndOfModeKickersActive
            pupendofmodewayne
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 13
            DMDFlush
            DMD CL("YOU SAVED HIM"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU SAVED HIM"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            AwardExtraBall
            MainModeEndTimer.Enabled = True
            ModeThreekillTimer.Enabled = True
            GFModeTarget.Isdropped = 1
            GF006.Z =-90
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            DeweyET.Enabled = True
            EndOfModeKickersActive
            pupendofmodedewey
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
        Case 14
            DMDFlush
            DMD CL("YOU SAVED HER"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU SAVED HER"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Addscore2 3500000
            GFEyesGlowingOFF
            GFEyesGlowingPinkOFF
            KillAllMovies
            ModeRingTriggersKill
            AwardExtraBall
            MainModeEndTimer.Enabled = True
            ModeThreekillTimer.Enabled = True
            GFModeTarget.Isdropped = 1
            GF006.Z =-90
            MBlocklight.State = 0
            SidMBSwitchTimer.Enabled = True
            GaleET.Enabled = True
            EndOfModeKickersActive
            pupendofmodegale
            Fulldmdbase
            Topperbase
            closedoors
            door2triggersbackon
    End Select
    ' eject the ball if it is in the scoop
    If BallsinHole Then
        vpmtimer.addtimer 5000, "kickBallOut '"
    End If
    DOF 139, DOFPulse
    Mode(CurrentPlayer, 0) = 0
    Mode(CurrentPlayer, NewMode) = 1
    UpdateModeLights
    FlashEffect 2
    LightEffect 2
    GiEffect 2
    'ChangeSong
    StopMode2 'to stop specific lights, timers and variables of the mode
    ' start the police hurry up after 2 kills
    IF CounselorsKilled(CurrentPlayer) MOD 2 = 0 Then
        'StartPolice
        StartThePoliceTimer.Enabled = True
    End If
    ' check for extra modes after each 3 completed kills
    IF CounselorsKilled(CurrentPlayer) MOD 6 = 0 Then
        'StartMichaelMultiball
        StartSamMBTimer.Enabled = True
    ElseIF CounselorsKilled(CurrentPlayer) MOD 3 = 0 Then
        'StartFreddyMultiball
        StartTaraMBTimer.Enabled = True
    ' If all counselors er dead then start final mode
    ElseIF CounselorsKilled(CurrentPlayer) = 14 Then
        'NewMode = 15
        'StartMode
        WizKillTriggersNLights
        WizardDrainKicker.Enabled = 1
        WizardModeTimer.Enabled = True
        WizardKillFlipperTimer.Enabled = True
    End If
End Sub

Sub StopMode 'called at the end of a ball
    Dim i
    Mode(CurrentPlayer, 0) = 0
    For i = 0 to 14 'stop any counselor blinking light
        If Mode(CurrentPlayer, i) = 2 Then Mode(CurrentPlayer, i) = 0
    Next
    UpdateModeLights
    StopMode2
End Sub

Sub StopMode2 'stop any mode special lights, timers or variables, called after a win or end of ball
    ' stop some timers or reset Mode variables
    Select Case NewMode
        Case 1:TurnOffArrows:li029.State = 0
        Case 2:UpdateTargetLights:li065.State = 0
        Case 3:li029.State = 0:EndModeTimer.Enabled = 0
        Case 4:li067.State = 0:li052.State = 0
        Case 5:TurnOffArrows:EndModeTimer.Enabled = 0
        Case 6:TurnOffArrows
        Case 7:UpdateTargetLights
        Case 8:BlueTargetsTimer.Enabled = 0:UpdateTargetLights
        Case 9:li065.State = 0
        Case 10:TurnOffArrows
        Case 11:BlueTargetsTimer.Enabled = 0:UpdateTargetLights
        Case 12:TurnOffArrows:SpinnersTimer.Enabled = 0
        Case 13:TurnOffArrows:FollowTheLights.Enabled = 0
        Case 14:TurnOffArrows:FollowTheLights.Enabled = 0
        Case 15:ResetModes
    End Select
    ' restore multiball lights
    If bJasonMBStarted Then UpdateArrowLights:End If
    If bFreddyMBStarted Then SetLightColor li064, blue, 2:SetLightColor li068, blue, 2
    If bMichaelMBStarted Then SetLightColor li062, blue, 2:SetLightColor li067, blue, 2:SetLightColor li059, blue, 2
    NewMode = 0
    ReadyToKill = False
End Sub

Sub ResetModes
    Dim i, j
    For j = 0 to 4
        CounselorsKilled(j) = 0
        For i = 0 to 14
            Mode(CurrentPlayer, i) = 0
        Next
    Next
    NewMode = 0
    'reset Mode variables
    TurnOffArrows
    SpinCount = 0
    ReadyToKill = False
End Sub

Sub EndModeTimer_Timer '1 second timer to count down to end the timed modes
    EndModeCountdown = EndModeCountdown - 1
    Select Case EndModeCountdown
        Case 16:PlaySound "vo_timerunningout"
        Case 10:PlaySound "vo_ten"
        Case 9:PlaySound "vo_nine"
        Case 8:PlaySound "vo_eight"
        Case 7:PlaySound "vo_seven"
        Case 6:PlaySound "vo_six"
        Case 5:PlaySound "vo_five"
        Case 4:PlaySound "vo_four"
        Case 3:PlaySound "vo_three"
        Case 2:PlaySound "vo_two"
        Case 1:PlaySound "vo_one"
        Case 0:PlaySound "vo_timeisup":StopMode
    End Select
End Sub

Sub BlueTargetsTimer_Timer 'rotates though all the targets
    If NewMode = 8 Then
        BlueTargetsCount = (BlueTargetsCount + 1) MOD 7
    Else                                 'this will be mode 11
        BlueTargetsCount = RndNbr(7) - 1 'from 0 to 6
    End If
    Select Case BlueTargetsCount
        Case 0:Li031.State = 2:Li049.State = 0:Li050.State = 0:Li051.State = 0:Li058.State = 0:Li057.State = 0:Li079.State = 0
        Case 1:Li031.State = 0:Li049.State = 2:Li050.State = 0:Li051.State = 0:Li058.State = 0:Li057.State = 0:Li079.State = 0
        Case 2:Li031.State = 0:Li049.State = 0:Li050.State = 2:Li051.State = 0:Li058.State = 0:Li057.State = 0:Li079.State = 0
        Case 3:Li031.State = 0:Li049.State = 0:Li050.State = 0:Li051.State = 2:Li058.State = 0:Li057.State = 0:Li079.State = 0
        Case 4:Li031.State = 0:Li049.State = 0:Li050.State = 0:Li051.State = 0:Li058.State = 2:Li057.State = 0:Li079.State = 0
        Case 5:Li031.State = 0:Li049.State = 0:Li050.State = 0:Li051.State = 0:Li058.State = 0:Li057.State = 2:Li079.State = 0
        Case 6:Li031.State = 0:Li049.State = 0:Li050.State = 0:Li051.State = 0:Li058.State = 0:Li057.State = 0:Li079.State = 2
    End Select
End Sub

Sub SpinnersTimer_Timer
    Select Case RndNbr(3)
        Case 0:SetLightColor li062, amber, 2:SetLightColor li067, amber, 0:SetLightColor li059, amber, 0
        Case 1:SetLightColor li062, amber, 0:SetLightColor li067, amber, 2:SetLightColor li059, amber, 0
        Case 2:SetLightColor li062, amber, 0:SetLightColor li067, amber, 0:SetLightColor li059, amber, 2
    End Select
End Sub

Sub FollowTheLights_Timer 'rotates though all the targets
    If NewMode = 13 Then
        ArrowsCount = (ArrowsCount + 1) MOD 8
    Else                            'this will be mode 14
        ArrowsCount = RndNbr(8) - 1 'from 0 to 8
    End If
    Select Case ArrowsCount
        Case 0:li062.State = 2:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0
        Case 1:li062.State = 0:Li064.State = 2:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0
        Case 2:li062.State = 0:Li064.State = 0:Li053.State = 2:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0
        Case 3:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 2:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0
        Case 4:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 2:Li068.State = 0:Li052.State = 0:Li059.State = 0
        Case 5:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 2:Li052.State = 0:Li059.State = 0
        Case 6:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 2:Li059.State = 0
        Case 7:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 2
    End Select
End Sub



Sub FollowTheLights2_Timer 'rotates though all the targets
    If NewMode = 13 Then
        ArrowsCount2 = (ArrowsCount2 + 1) MOD 8
    Else                            'this will be mode 14
        ArrowsCount2 = RndNbr(8) - 1 'from 0 to 8
    End If
    Select Case ArrowsCount2
        Case 0:li062.State = 2:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0:ArrowCountTrigger001
        Case 1:li062.State = 0:Li064.State = 2:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0:ArrowCountTrigger002
        Case 2:li062.State = 0:Li064.State = 0:Li053.State = 2:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0:ArrowCountTrigger003
        Case 3:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 2:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0:ArrowCountTrigger004
        Case 4:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 2:Li068.State = 0:Li052.State = 0:Li059.State = 0:ArrowCountTrigger005
        Case 5:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 2:Li052.State = 0:Li059.State = 0:ArrowCountTrigger006
        Case 6:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 2:Li059.State = 0:ArrowCountTrigger007
        Case 7:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 2:ArrowCountTrigger008
    End Select
End Sub



Sub ArrowCountTrigger001
    Trigger012.Enabled = True
    Trigger016.Enabled = False
    Trigger007.Enabled = False
    TimedTrigger.Enabled = False
    Trigger011.Enabled = False
    Trigger014.Enabled = False
    Trigger015.Enabled = False
    Trigger009.Enabled = False
    Trigger013.Enabled = False
End Sub


Sub ArrowCountTrigger002
    Trigger012.Enabled = False
    Trigger016.Enabled = True
    Trigger007.Enabled = False
    TimedTrigger.Enabled = False
    Trigger011.Enabled = False
    Trigger014.Enabled = False
    Trigger015.Enabled = False
    Trigger009.Enabled = False
    Trigger013.Enabled = False
End Sub


Sub ArrowCountTrigger003
    Trigger012.Enabled = False
    Trigger016.Enabled = False
    Trigger007.Enabled = True
    TimedTrigger.Enabled = False
    Trigger011.Enabled = False
    Trigger014.Enabled = False
    Trigger015.Enabled = False
    Trigger009.Enabled = False
    Trigger013.Enabled = False
End Sub


Sub ArrowCountTrigger004
    Trigger012.Enabled = False
    Trigger016.Enabled = False
    Trigger007.Enabled = False
    TimedTrigger.Enabled = True
    Trigger011.Enabled = False
    Trigger014.Enabled = False
    Trigger015.Enabled = False
    Trigger009.Enabled = False
    Trigger013.Enabled = False
End Sub


Sub ArrowCountTrigger005
    Trigger012.Enabled = False
    Trigger016.Enabled = False
    Trigger007.Enabled = False
    TimedTrigger.Enabled = False
    Trigger011.Enabled = False
    Trigger014.Enabled = True
    Trigger015.Enabled = False
    Trigger009.Enabled = False
    Trigger013.Enabled = False
End Sub


Sub ArrowCountTrigger006
    Trigger012.Enabled = False
    Trigger016.Enabled = False
    Trigger007.Enabled = False
    TimedTrigger.Enabled = False
    Trigger011.Enabled = False
    Trigger014.Enabled = False
    Trigger015.Enabled = True
    Trigger009.Enabled = False
    Trigger013.Enabled = False
End Sub


Sub ArrowCountTrigger007
    Trigger012.Enabled = False
    Trigger016.Enabled = False
    Trigger007.Enabled = False
    TimedTrigger.Enabled = False
    Trigger011.Enabled = False
    Trigger014.Enabled = False
    Trigger015.Enabled = False
    Trigger009.Enabled = True
    Trigger013.Enabled = False
End Sub


Sub ArrowCountTrigger008
    Trigger012.Enabled = False
    Trigger016.Enabled = False
    Trigger007.Enabled = False
    TimedTrigger.Enabled = False
    Trigger011.Enabled = False
    Trigger014.Enabled = False
    Trigger015.Enabled = False
    Trigger009.Enabled = False
    Trigger013.Enabled = True
End Sub


Sub ArrowCountAllOn
    Trigger012.Enabled = True
    Trigger016.Enabled = True
    Trigger007.Enabled = True
    TimedTrigger.Enabled = True
    Trigger011.Enabled = True
    Trigger014.Enabled = True
    Trigger015.Enabled = True
    Trigger009.Enabled = True
    Trigger013.Enabled = True
End Sub




'***********
'  torch
'***********
Dim i1, i2
i1 = 0
i2 = 4

Sub torchtimer_timer
    torch1.imageA = "torch" &i1
    torch2.imageA = "torch" &i2
    i1 = (i1 + 1) MOD 8
    i2 = (i2 + 1) MOD 8
End Sub



'****************************************************************************************
' Gi Color Change (Color Changes To Red if a Male GhostFace Killer Mode has Started)  &
'****************************************************************************************

'**************************************************************************
' & (Color Changes To Pink if a Female GhostFace Killer Mode has Started)
'**************************************************************************

Sub ColorChange()
    vpmtimer.addtimer 500, "ChangeGi red:ChangeGIIntensity 2 '" 'kick out the rest of the balls, if any
End Sub


Sub ColorChangeGreen()
    vpmtimer.addtimer 500, "ChangeGi green:ChangeGIIntensity 2 '" 'kick out the rest of the balls, if any
End Sub


Sub ColorChangePink()
    vpmtimer.addtimer 500, "ChangeGi purple:ChangeGIIntensity 2 '" 'kick out the rest of the balls, if any
End Sub



Sub ColorChangeBack()
    vpmtimer.addtimer 500, "ChangeGi white:ChangeGIIntensity 1 '" 'kick out the rest of the balls, if any
End Sub


'Sub KillSeventeen()
 '   vpmtimer.addtimer 500, "Kill17 '" 'kick out the rest of the balls, if any
'End Sub

'Sub Kill17
 '   Trigger017.enabled =False
'End Sub



Sub GoTaraGo
    If NewMode = 15 Then Exit Sub
    if BallsOnPlayfield =1 Then
    StartTara2MBTimer.Enabled = True
    End If
End Sub


Sub GoSamGo
    If NewMode = 15 Then Exit Sub
    if BallsOnPlayfield =1 Then
    StartSam2MBTimer.Enabled = True
    End If
End Sub



'***********************************************************************
' Ball Save Kicker (The Kicker Below The Main Flippers Instead of a Peg
'***********************************************************************

Sub magickicker_Hit()
magickicker.TimerEnabled = 1
PlaySound ""
end sub

Sub magickicker_Timer
magickicker.Kick 2, 14
magickicker.TimerEnabled = 0
End Sub


'************************************************************************************************
' Mode Trigger KillSwitch (This Kills All The Secondary Game Triggers Once a Main Mode is Lit) &
'************************************************************************************************

'*******************************************************
' & Drop Targets / Lights & Triggers & Restart Triggers
'*******************************************************

Sub KillSwitch
    Trigger012.enabled =False
    Trigger013.enabled =False
    Trigger009.enabled =False
    Trigger014.enabled =False
    Trigger015.enabled =False
    Trigger016.enabled =False
    Trigger007.enabled =False
    Trigger011.enabled =False
    Trigger008.enabled =False
    Captrigger.enabled =False
    AllUp
    ModeTwoAllOn
 End Sub


Sub KillSwitchRestart
    Trigger012.enabled =True
    Trigger013.enabled =True
    Trigger009.enabled =True
    Trigger014.enabled =True
    Trigger015.enabled =True
    Trigger016.enabled =True
    Trigger007.enabled =True
    Trigger011.enabled =True
    Trigger008.enabled =True
    Captrigger.enabled =True
    AllDropped
    ModeTwoAllOff
End Sub

Sub SwitchKillReset
    vpmtimer.addtimer 5800, "KillElevenAgain '" 
End Sub


Sub KillElevenAgain
    Trigger011.enabled =False
End Sub



Sub MBKillSwitchRestart_Hit
    Trigger012.enabled =True
    Trigger013.enabled =True
    Trigger009.enabled =True
    Trigger014.enabled =True
    Trigger015.enabled =True
    Trigger016.enabled =True
    Trigger007.enabled =True
    Trigger011.enabled =True
    Captrigger.enabled =True
    AllDropped
    ModeTwoAllOff
    ModeThreeAllOff
    vpmtimer.addtimer 700, "KillKicker001 '"
End Sub


Sub RestartMBKillSwitch
    If li071.State =2 Then
    MBKillSwitchRestart.enabled =True
    Kicker001.enabled =True
    End If
End Sub


Sub AllDropped
    BlockerLightsBlueOff
    blocktarget001. isdropped = 1
    blocktrigger001.enabled =False
    blocktarget002. isdropped = 1
    blocktrigger002.enabled =False
    blocktarget003. isdropped = 1
    blocktrigger003.enabled =False
    RMTarget. isdropped = 1
    RTTarget. isdropped = 1
    RMTopTarget. isdropped = 1
    RLTopTarget. isdropped = 1
    LBTarget001. isdropped = 1
    blocktarget004. isdropped = 1
    blocktrigger004.enabled =False
    blocktarget005. isdropped = 1
    blocktrigger005.enabled =False
    blocktarget006. isdropped = 1
    blocktrigger006.enabled =False
    blocktrigger007.enabled =False
    blocktrigger008.enabled =False
    blocktrigger009.enabled =False
    blocktrigger010.enabled =False
End Sub


Sub AllUp
    BlockerLightsBlueOn
    blocktarget001. isdropped = 0
    blocktrigger001.enabled =True
    blocktarget002. isdropped = 0
    blocktrigger002.enabled =True
    blocktarget003. isdropped = 0
    blocktrigger003.enabled =True
    RMTarget. isdropped = 0
    RTTarget. isdropped = 0
    RMTopTarget. isdropped = 0
    RLTopTarget. isdropped = 0
    LBTarget001. isdropped = 0
    blocktarget004. isdropped = 0
    blocktrigger004.enabled =True
    blocktarget005. isdropped = 0
    blocktrigger005.enabled =True
    blocktarget006. isdropped = 0
    blocktrigger006.enabled =True
    blocktrigger007.enabled =True
    blocktrigger008.enabled =True
    blocktrigger009.enabled =True
    blocktrigger010.enabled =True
End Sub


Sub BlockerLightsBlueOn
    if li049.State =0 Then
    BlockerLightBlue001.State =1
    End If
    If li050.State =0 Then
    BlockerLightBlue002.State =1
    End If
    If li051.State =0 Then
    BlockerLightBlue003.State =1
    End If
    If li057.State =0 Then
    BlockerLightBlue004.State =1
    End If
    If li058.State =0 Then
    BlockerLightBlue005.State =1
    End If
    If li031.State =0 Then
    BlockerLightBlue006.State =1
    End If
    If li079.State =0 Then
    BlockerLightBlue008.State =1
    End If
End Sub

Sub BlockerLightsBlueOff
    BlockerLightBlue001.State =0
    BlockerLightBlue002.State =0
    BlockerLightBlue003.State =0
    BlockerLightBlue004.State =0
    BlockerLightBlue005.State =0
    BlockerLightBlue006.State =0
    BlockerLightBlue008.State =0
End Sub





Sub blocktrigger001_Hit
    'if li049.State =0 Then
    'BlockerLightBlue001.State =1
    AddScore 12000
    BlockerLights
    PlayClawSwipe
    'End If
    'If li049.State =1 Then
    'BlockerLightBlue001.State =0
    'AddScore 12000
    'BlockerLights
    'PlayClawSwipe
    'End If
End Sub


Sub blocktrigger002_Hit
    'If li050.State =0 Then
    'BlockerLightBlue002.State =1
    AddScore 12000
    BlockerLights
    PlayClawSwipe
    'End If
    'If li050.State =1 Then
    'BlockerLightBlue002.State =0
    'AddScore 12000
    'BlockerLights
    'PlayClawSwipe
    'End If
End Sub


Sub blocktrigger003_Hit
    'If li051.State =0 Then
    'BlockerLightBlue003.State =1
    AddScore 12000
    BlockerLights
    PlayClawSwipe
    'End If
    'If li051.State =1 Then
    'BlockerLightBlue003.State =0
    'AddScore 12000
    'BlockerLights
    'PlayClawSwipe
    'End If
End Sub


Sub blocktrigger004_Hit
    'If li057.State =0 Then
    'BlockerLightBlue004.State =1
    AddScore 12000
    BlockerLights
    PlayClawSwipe
    'End If
   ' If li057.State =1 Then
   ' BlockerLightBlue004.State =0
    'AddScore 12000
   ' BlockerLights
   ' PlayClawSwipe
   ' end If
End Sub


Sub blocktrigger005_Hit
    'If li058.State =0 Then
    'BlockerLightBlue005.State =1
    AddScore 12000
    BlockerLights
    PlayClawSwipe
    'End If
    'If li058.State =1 Then
    'BlockerLightBlue005.State =0
    'AddScore 12000
    'BlockerLights
    'PlayClawSwipe
    'End If
End Sub


Sub blocktrigger006_Hit
    'If li031.State =0 Then
    'BlockerLightBlue006.State =1
    AddScore 12000
    BlockerLights
    PlayClawSwipe
    'End If
    'If li031.State =1 Then
    'BlockerLightBlue006.State =0
    'AddScore 12000
    'BlockerLights
    'PlayClawSwipe
    'End If
End Sub


Sub blocktrigger007_Hit
    'If li079.State =0 Then
    'BlockerLightBlue008.State =1
    AddScore 12000
    BlockerLights
    PlayClawSwipe
    'End If
   ' If li079.State =1 Then
   ' BlockerLightBlue008.State =0
    'AddScore 12000
    'BlockerLights
    'PlayClawSwipe
    'End If
End Sub


Sub blocktrigger008_Hit
    'If li079.State =0 Then
    'BlockerLightBlue008.State =1
    AddScore 12000
    BlockerLights
    PlayClawSwipe
    'End If
    'If li079.State =1 Then
    'BlockerLightBlue008.State =0
    'AddScore 12000
    'BlockerLights
    'PlayClawSwipe
    'End If
End Sub


Sub blocktrigger009_Hit
    'If li079.State =0 Then
    'BlockerLightBlue008.State =1
    AddScore 12000
    BlockerLights
    PlayClawSwipe
    'End If
    'If li079.State =1 Then
    'BlockerLightBlue008.State =0
    'AddScore 12000
    'BlockerLights
    'PlayClawSwipe
    'End If
End Sub


Sub blocktrigger010_Hit
    'If li079.State =0 Then
    'BlockerLightBlue008.State =1
    AddScore 12000
    BlockerLights
    PlayClawSwipe
   ' End If
    'If li079.State =1 Then
    'BlockerLightBlue008.State =0
    'AddScore 12000
    'BlockerLights
    'PlayClawSwipe
    'End If
End Sub



Sub BlockerLights
    flashforms  BlockerLight001,800, 100, 0
    flashforms  BlockerLight002,800, 100, 0
    flashforms  BlockerLight003,800, 50, 0
    flashforms  BlockerLight004,800, 50, 0
    flashforms  BlockerLight005,800, 50, 0
    flashforms  BlockerLight006,800, 50, 0
    flashforms  BlockerLight008,800, 50, 0
End Sub



Sub ModeLitRestart()
   If NewMode Then
   KillSwitch
   AllUp
   End If
End Sub


Sub ModeLitTrigger_Hit
    KillSwitch
    AllUp
End Sub

'ModeLitTrigger.enabled =True

Sub ModeLitTriggerOn
    ModeLitTrigger.enabled =True
    Trigger017.enabled =True
End Sub
  

Sub ModeLitLightItUp
    vpmtimer.addtimer 1900, "ModeLitTriggerOn '"
End Sub


'******************************
'GhostFace Ramp Chase Triggers
'******************************


Sub GFChaseTrigger_Hit
    if BallsOnPlayfield =7 Then
    GFShow2Trigger.enabled =False
    GFShowTrigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =6 Then
    GFShow2Trigger.enabled =False
    GFShowTrigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =5 Then
    GFShowTrigger.enabled =False
    GFShow2Trigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =4 Then
    GFShowTrigger.enabled =False
    GFShow2Trigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =3 Then
    GFShowTrigger.enabled =False
    GFShow2Trigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =2 Then
    GFShowTrigger.enabled =False
    GFShow2Trigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =1 Then
    GFShowTrigger.enabled =True
    GFShow2Trigger.enabled =True
    End if 
End Sub


Sub GFChaseTrigger2_Hit
    if BallsOnPlayfield =7 Then
    GFShow2Trigger.enabled =False
    GFShowTrigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =6 Then
    GFShow2Trigger.enabled =False
    GFShowTrigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =5 Then
    GFShow2Trigger.enabled =False
    GFShowTrigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =4 Then
    GFShow2Trigger.enabled =False
    GFShowTrigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =3 Then
    GFShow2Trigger.enabled =False
    GFShowTrigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =2 Then
    GFShow2Trigger.enabled =False
    GFShowTrigger.enabled =False
    ModeLitTrigger.enabled =False
    Trigger017.enabled =False
    End If
    if BallsOnPlayfield =1 Then
    GFShow2Trigger.enabled =True
    GFShowTrigger.enabled =True
    End if 
End Sub


Sub GFShow2Trigger_Hit()
    GFChase.visible = 1
    GFChase.Roty = -405
    GFChase.Y = 315.6665
    GFChase.X = 194
    GFChase.Z = 170
End Sub

Sub GFhightTrigger_Hit()
    GFChase.Z = 230
End Sub


Sub GFShowTrigger_Hit()
    GFChase.visible = 1
    GFChase.Roty = 35
    'GFChase.Z = 230
    GFChase.Z = 180
End Sub


Sub GFShowOffTrigger_Hit()
    GFChase.visible = 0
End Sub


Sub GFRrampTurn001_Hit()
    GFChase.Roty = 0
End Sub


Sub GFRrampTurn002_Hit()
    GFChase.Roty = -20
    GFChase.Y = 1023
    GFChase.X = 36
End Sub


Sub GFRrampTurn003_Hit()
    GFChase.Roty = -60
    GFChase.Y = 1137
    GFChase.X = 68
End Sub

Sub GFRrampTurn004_Hit()
    GFChase.Roty = -10
    GFChase.Y = 1391
    GFChase.X = 720
End Sub


Sub GFLrampTurn001_Hit()
    GFChase.Roty = -100
    GFChase.Y = 714.3333
    GFChase.X = 584.6667
End Sub


Sub GFLrampTurn002_Hit()
    GFChase.Roty = -110
    GFChase.Y = 718.3333
    GFChase.X = 671.3334
End Sub


Sub GFLrampTurn003_Hit()
    GFChase.Roty = -135
    GFChase.Y = 700.9999
    GFChase.X = 738.0001
End Sub


Sub GFLrampTurn004_Hit()
    GFChase.Roty = -175
    GFChase.Y = 659.6666
    GFChase.X = 799.3334
End Sub


Sub GFLrampTurn005_Hit()
    GFChase.Roty = -215
    GFChase.Y = 569.0001
    GFChase.X = 820.6667
End Sub


Sub GFLrampTurn006_Hit()
    GFChase.Roty = -250
    GFChase.Y = 569.0001
    GFChase.X = 501.0001
End Sub


Sub GFLrampTurn007_Hit()
    GFChase.Roty = -290
    GFChase.Y = 453
    GFChase.X = 716.6667
End Sub


Sub GFLrampTurn008_Hit()
    GFChase.Roty = -335
    GFChase.Y = 455.6666
    GFChase.X = 620.6667
End Sub


Sub GFLrampTurn009_Hit()
    GFChase.Roty = -370
    GFChase.Y = 512.9999
    GFChase.X = 556.6667
End Sub


Sub GFLrampTurn010_Hit()
    GFChase.Roty = -390
    GFChase.Y = 571.6666
    GFChase.X = 548.6667
End Sub


Sub GFLrampTurn011_Hit()
    GFChase.Roty = -410
    GFChase.Y = 643.6665
    GFChase.X = 570
End Sub


Sub GFLrampTurn012_Hit()
    GFChase.Roty = -390
    GFChase.Y = 678.3332
    GFChase.X = 620.6666
End Sub


Sub GFLrampTurn013_Hit()
    GFChase.Roty = -370
    GFChase.Y = 720.9999
    GFChase.X = 651.3333
End Sub


Sub GFLrampTurn014_Hit()
    GFChase.Roty = -335
    GFChase.Y = 795.6664
    GFChase.X = 682
End Sub


Sub GFLrampTurn015_Hit()
    GFChase.Roty = -325
    GFChase.Y = 863.6664
    GFChase.X = 640.6666
End Sub


Sub GFLrampTurn016_Hit()
    GFChase.Roty = -340
    GFChase.Y = 1314.333
    GFChase.X = 167.3333
End Sub


Sub GFLrampTurn017_Hit()
    GFChase.Roty = -365
    GFChase.Y = 1387.667
    GFChase.X = 121.9999
End Sub


Sub GFLrampTurn018_Hit()
    GFChase.visible = 0
End Sub


'***********************************************************
'  Kicker GF Drop Target and the ShotGun Kicker and Flasher
'***********************************************************





'MBTrigger2.enabled =False
'GFModeTarget.Isdropped = 1


Sub GFScoopTargetTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    GFScoopTargetTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GFModeTarget.Isdropped = 0
    GF006.Z =5
End Sub


Sub GFScoopTargetbillyTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    GFScoopTargetbillyTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GFModeTarget.Isdropped = 0
    GF006.Z =5
End Sub


Sub GFModeTargetActive
    GFModeTarget.Isdropped = 0
    GF006.Z =5
End Sub


Sub GFModeTargetDeactive
    GFModeTarget.Isdropped = 1
    GF006.Z =-90
End Sub


Sub GFDropp_hit
    If MBlocklight.State = 1 Then
    GFModeTarget.Isdropped = 1
    GF006.Z =-90
    End If
End Sub
    
    


Sub ModeLitTargetUp
    ModeLitTarget.Enabled = True
End Sub


Sub ModeLitTargetOff
    ModeLitTarget.Enabled = False
End Sub



Sub ModeLitTarget_Hit
    vpmtimer.addtimer 1200, "ModeLitTargetDown '"
End Sub

Sub ModeLitTargetDown
    'ModeLitTarget.Isdropped = 1
    ModeLitTarget.Kick -150, 14
    'ModeLitTarget.TimerEnabled = 0
    vpmtimer.addtimer 1400, "ModeLitTargetOff '"
End Sub



Sub KickIt
    kicker001.Kick 2, 14
    kicker001.TimerEnabled = 0
    'MBKillSwitchRestart.Enabled = False
End Sub

Sub KillKicker001
    kicker001.Enabled = False
End Sub
    


Sub kicker001_Hit()
kicker001.TimerEnabled = 1
PlaySound ""
end sub

Sub kicker001_Timer
    vpmtimer.addtimer 500, "KickIt '"
End Sub


Sub kicker002_Hit()
kicker002.TimerEnabled = 1
PlaySound ""
end sub

Sub kicker002_Timer
kicker002.Kick 2, 14
kicker002.TimerEnabled = 0
End Sub


Sub ShotGunkicker002_Hit()
vpmtimer.addtimer 1500, "ShotGunkicker002Out '"
PlaySound "shotgun"
end sub


Sub ShotGunkicker002Out
ShotGunkicker002.Kick -135, 25
flashforms  ShotGunLight001,50, 50, 0
GFChase.visible = 0
ShotGunRShake
End Sub


Sub ShotGunkicker001_Hit()
vpmtimer.addtimer 1500, "ShotGunkicker001Out '"
PlaySound "shotgun"
end sub


Sub ShotGunkicker001Out
ShotGunkicker001.Kick 120, 25
flashforms  ShotGunLight002,50, 50, 0
GFChase.visible = 0
ShotGunLShake
End Sub


Dim ShotGunLPos

	Sub ShotGunLShake()
		ShotGunLPos = 4
		ShotGunLShakeTimer.Enabled = True
	End Sub

	Sub ShotGunLShakeTimer_Timer()
		'OldCellLsling.RotZ = ShotGunLPos
        ShotGunL.RotY = ShotGunLPos
        If ShotGunLPos <= 0.1 AND ShotGunLPos >= -0.1 Then Me.Enabled = False:Exit Sub
		If ShotGunLPos < 0 Then
			ShotGunLPos = ABS(ShotGunLPos)- 0.1
		Else
			ShotGunLPos = - ShotGunLPos + 0.1
		End If
	End Sub


Dim ShotGunRPos

	Sub ShotGunRShake()
		ShotGunRPos = 4
		ShotGunRShakeTimer.Enabled = True
	End Sub

	Sub ShotGunRShakeTimer_Timer()
		'OldCellLsling.RotZ = ShotGunLPos
        ShotGunR.RotY = ShotGunRPos
        If ShotGunRPos <= 0.1 AND ShotGunRPos >= -0.1 Then Me.Enabled = False:Exit Sub
		If ShotGunRPos < 0 Then
			ShotGunRPos = ABS(ShotGunRPos)- 0.1
		Else
			ShotGunRPos = - ShotGunRPos + 0.1
		End If
	End Sub



'**********************************
' Cell Phone Triggers & Animations
'**********************************



 'BigCellRingTrigger.enabled =True
 'BigCellRingOffTrigger.enabled =False
 'BigIphoneRingTrigger.enabled =True
 'BigIphoneRingOffTrigger.enabled =False



Dim CellSlingRPos

	Sub CellRShake()
		CellSlingRPos = 4
		CellRShakeTimer.Enabled = True
	End Sub

	Sub CellRShakeTimer_Timer()
		OldCellRsling.RotZ = CellSlingRPos
        IphoneRSling.RotY = CellSlingRPos
        If CellSlingRPos <= 0.1 AND CellSlingRPos >= -0.1 Then Me.Enabled = False:Exit Sub
		If CellSlingRPos < 0 Then
			CellSlingRPos = ABS(CellSlingRPos)- 0.1
		Else
			CellSlingRPos = - CellSlingRPos + 0.1
		End If
	End Sub


Dim CellSlingLPos

	Sub CellLShake()
		CellSlingLPos = 4
		CellLShakeTimer.Enabled = True
	End Sub

	Sub CellLShakeTimer_Timer()
		OldCellLsling.RotZ = CellSlingLPos
        IphoneLSling.RotY = CellSlingLPos
        If CellSlingLPos <= 0.1 AND CellSlingLPos >= -0.1 Then Me.Enabled = False:Exit Sub
		If CellSlingLPos < 0 Then
			CellSlingLPos = ABS(CellSlingLPos)- 0.1
		Else
			CellSlingLPos = - CellSlingLPos + 0.1
		End If
	End Sub


Dim BigCellPos

	Sub BigCellShake()
		BigCellPos = 4
		BigCellShakeTimer.Enabled = True
	End Sub

	Sub BigCellShakeTimer_Timer()
		OldCellAprom.RotZ = BigCellPos
        Iphone.RotY = BigCellPos
        IphoneCallFlasher.RotY = BigCellPos
        OldCellRingFlasher.RotY = BigCellPos
        If BigCellPos <= 0.1 AND BigCellPos >= -0.1 Then Me.Enabled = False:Exit Sub
		If BigCellPos < 0 Then
			BigCellPos = ABS(BigCellPos)- 0.1
		Else
			BigCellPos = - BigCellPos + 0.1
		End If
	End Sub




Sub BigCellRingTrigger_Hit()
    BigCellShake
    BigCellRingOffTrigger.enabled =True
    PlaySound "OldCellRing"
   'OldCellRingFlasher.visible = 1
    KillSwitchRestart
    vpmtimer.addtimer 50, "BigCellRingTriggerOff '" 
End Sub


Sub BigCellRingOffTrigger_Hit()
    BigCellRingTrigger.enabled =False
End Sub


Sub BigCellRingTriggerOff
    BigCellRingTrigger.enabled =False
    BigIphoneRingTrigger.enabled =False
End Sub


'BigCellRingTrigger.enabled =True

'BigCellRingOffTrigger.enabled =True


Sub BigIphoneRingTrigger_Hit()
    BigCellShake
    BigIphoneRingOffTrigger.enabled =True
    PlaySound "IphoneRingTone"
    'IphoneCallFlasher.visible = 1
    KillSwitchRestart
    vpmtimer.addtimer 50, "BigCellRingTriggerOff '"
End Sub


Sub BigIphoneRingOffTrigger_Hit()
    BigIphoneRingTrigger.enabled =False
End Sub



Sub RingRingTriggerOff001_Hit
    BigCellRingTrigger.enabled =False
    BigIphoneRingTrigger.enabled =False
End Sub



Sub RingRingTriggerOff002_Hit
    BigCellRingTrigger.enabled =False
    BigIphoneRingTrigger.enabled =False
End Sub


'RingRingTriggerOff001.enabled =True
'RingRingTriggerOff002.enabled =True


Sub ModeRingTriggersOff
    RingRingTriggerOff001.enabled =True
    RingRingTriggerOff002.enabled =True
End Sub


Sub ModeRingTriggersKill
    RingRingTriggerOff001.enabled =False
    RingRingTriggerOff002.enabled =False
End Sub



Sub IphoneSwitchUp
    Iphone.visible = 1
    IphoneRSling.visible = 1
    IphoneLSling.visible = 1
    OldCellAprom.visible = 0
    OldCellLsling.visible = 0
    OldCellRsling.visible = 0
    BigIphoneRingTrigger.enabled =True
    BigIphoneRingOffTrigger.enabled =False
    BigCellRingTrigger.enabled =False
End Sub


Sub IphoneSwitchUpStart
    Iphone.visible = 1
    IphoneRSling.visible = 1
    IphoneLSling.visible = 1
    OldCellAprom.visible = 0
    OldCellLsling.visible = 0
    OldCellRsling.visible = 0
    
End Sub


Sub IphoneSwitchBack
    Iphone.visible = 0
    IphoneRSling.visible = 0
    IphoneLSling.visible = 0
    OldCellAprom.visible = 1
    OldCellLsling.visible = 1
    OldCellRsling.visible = 1
    BigCellRingTrigger.enabled =True
    BigCellRingOffTrigger.enabled =False
    BigIphoneRingTrigger.enabled =False
End Sub


Sub IphoneSwitchBackAttract
    Iphone.visible = 0
    IphoneRSling.visible = 0
    IphoneLSling.visible = 0
    OldCellAprom.visible = 1
    OldCellLsling.visible = 1
    OldCellRsling.visible = 1
    
End Sub



Sub PhoneSwitchTrigger_Hit()
    If li014.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li013.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li012.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li011.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li010.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li009.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li008.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li007.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li006.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li005.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li004.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li001.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li002.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li003.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
End Sub


Sub PhoneSwitchTrigger001_Hit()
    If li014.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li013.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li012.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li011.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li010.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li009.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li008.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li007.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li006.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li005.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li004.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li001.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchUp
    End If
    If li002.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
    If li003.State = Mode(CurrentPlayer, NewMode) <> 0 Then
    IphoneSwitchBack
    End If
End Sub


Sub KillOldCallFlasher
    vpmtimer.addtimer 8000, "OldCallFlasherKill '" 'kick out the rest of the balls, if any
End Sub

Sub OldCallFlasherKill
    OldCellRingFlasher.visible = 0
End Sub


Sub OldCellShakeAgain
    vpmtimer.addtimer 1000, "OldCellRingTwice '" 'kick out the rest of the balls, if any
End Sub


Sub OldCellRingTwice
    PlaySound "OldCellRing"
    BigCellShake
End Sub



Sub KillIphoneCallFlasher
    vpmtimer.addtimer 8000, "IphoneCallFlasherKill '" 'kick out the rest of the balls, if any
End Sub


Sub IphoneShakeAgain
    vpmtimer.addtimer 1000, "IphoneRingTwice '" 'kick out the rest of the balls, if any
End Sub


Sub IphoneCallFlasherKill
    IphoneCallFlasher.visible = 0
End Sub


Sub IphoneRingTwice
    PlaySound "IphoneRingTone"
    BigCellShake
End Sub



'*************************************
' Projector & Movie Screen Animations
'*************************************

Sub BigFilmReelTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    BigFilmReelTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    BigFlimReelTimer.enabled =False
    'SmallFlimReelTimer.enabled =False
End Sub


Sub BigFilmReelStartTrigger_Hit()
    BigFlimReelTimer.enabled =True
    'SmallFlimReelTimer.enabled =True
    BigFilmReelTimer.Enabled = True
End Sub

Dim BigFlimReelDir2
BigFlimReelDir2 = 5 'this is both the direction, if + goes up, if - goes down, and also the speed

Sub BigFlimReelTimer_Timer
    BigReel.RotY = BigReel.RotY + BigFlimReelDir2
    BigFlim.RotY = BigFlim.RotY + BigFlimReelDir2
End Sub

'BigFlimReelTimer.enabled =True


Dim SmallFlimReelDir2
SmallFlimReelDir2 = 5 'this is both the direction, if + goes up, if - goes down, and also the speed

Sub SmallFlimReelTimer_Timer
    SmallFilm.RotY = SmallFilm.RotY + SmallFlimReelDir2
    SmallReel.RotY = SmallReel.RotY + SmallFlimReelDir2
    ProjectorReel001.RotZ = ProjectorReel001.RotZ - SmallFlimReelDir2
    ProjectorFilm001.RotZ = ProjectorFilm001.RotZ - SmallFlimReelDir2
    ProjectorReel002.RotZ = ProjectorReel002.RotZ - SmallFlimReelDir2
    ProjectorFilm002.RotZ = ProjectorFilm002.RotZ - SmallFlimReelDir2
End Sub

'SmallFlimReelTimer.enabled =True


'AMBER
Dim MovieScreenONECnt
MovieScreenONECnt= 0
MovieScreen1Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen1Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenONECnt = 0
	End If

	'Select the correct frame
	If MovieScreenONECnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenONECnt
	Elseif MovieScreenONECnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenONECnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenONECnt
	End If
	MovieScreenONECnt = MovieScreenONECnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenONECnt> 24 Then    
		MovieScreenONECnt = 0
        MovieScreen1Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        Amber001Timer.Enabled = True
    End If
End Sub


'MovieScreen1Animation.Enabled = True


Dim MovieScreenTwoCnt
MovieScreenTwoCnt= 0
MovieScreen2Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen2Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwoCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwoCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenTwoCnt
	Elseif MovieScreenTwoCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenTwoCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenTwoCnt
	End If
	MovieScreenTwoCnt = MovieScreenTwoCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwoCnt> 45 Then    
		MovieScreenTwoCnt = 0
        MovieScreen2Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        Amber011Timer.Enabled = True
    End If
End Sub


'MovieScreen2Animation.Enabled = True


Sub Amber001
    MovieScreenBlank.imageA = "Amber001"
End Sub


Sub Amber002
    MovieScreenBlank.imageA = "Amber002"
End Sub


Sub Amber003
    MovieScreenBlank.imageA = "Amber003"
End Sub


Sub Amber004
    MovieScreenBlank.imageA = "Amber004"
End Sub


Sub Amber005
    MovieScreenBlank.imageA = "Amber005"
End Sub


Sub Amber006
    MovieScreenBlank.imageA = "Amber006"
End Sub


Sub Amber007
    MovieScreenBlank.imageA = "Amber007"
End Sub


Sub Amber008
    MovieScreenBlank.imageA = "Amber008"
End Sub


Sub Amber001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Amber001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Amber001
    Amber002Timer.Enabled = True
End Sub


Sub Amber002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Amber002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Amber002
    Amber003Timer.Enabled = True
End Sub


Sub Amber003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Amber003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Amber003
    Amber004Timer.Enabled = True
End Sub


Sub Amber004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Amber004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Amber004
    Amber005Timer.Enabled = True
End Sub


Sub Amber005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Amber005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Amber005
    Amber006Timer.Enabled = True
End Sub


Sub Amber006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Amber006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Amber006
    Amber007Timer.Enabled = True
End Sub


Sub Amber007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Amber007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Amber007
    Amber008Timer.Enabled = True
End Sub


Sub Amber008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Amber008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Amber008
    Amber009Timer.Enabled = True
    'MovieScreen1Animation.Enabled = True
End Sub


Sub Amber009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Amber009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'Amber008
    Amber010Timer.Enabled = True
    'MovieScreenBlank.imageA = ""
    'MovieScreen2Animation.Enabled = True
End Sub


Sub Amber010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Amber010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'Amber008
    'Amber001Timer.Enabled = True
    MovieScreen2Animation.Enabled = True
End Sub


Sub Amber011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Amber011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'Amber008
    'Amber001Timer.Enabled = True
    MovieScreen1Animation.Enabled = True
End Sub



'BILLY
Dim MovieScreenThreeCnt
MovieScreenThreeCnt= 0
MovieScreen3Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen3Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenThreeCnt = 0
	End If

	'Select the correct frame
	If MovieScreenThreeCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenThreeCnt
	Elseif MovieScreenThreeCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenThreeCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenThreeCnt
	End If
	MovieScreenThreeCnt = MovieScreenThreeCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenThreeCnt> 24 Then    
		MovieScreenThreeCnt = 0
        MovieScreen3Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Billy001Timer.Enabled = True
    End If
End Sub


'MovieScreen3Animation.Enabled = True


Dim MovieScreenFourCnt
MovieScreenFourCnt= 0
MovieScreen4Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen4Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenFourCnt = 0
	End If

	'Select the correct frame
	If MovieScreenFourCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenFourCnt
	Elseif MovieScreenFourCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenFourCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenFourCnt
	End If
	MovieScreenFourCnt = MovieScreenFourCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenFourCnt> 45 Then    
		MovieScreenFourCnt = 0
        MovieScreen4Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Billy011Timer.Enabled = True
    End If
End Sub


'MovieScreen4Animation.Enabled = True


Sub Billy001
    MovieScreenBlank.imageA = "Billy001"
End Sub


Sub Billy002
    MovieScreenBlank.imageA = "Billy002"
End Sub


Sub Billy003
    MovieScreenBlank.imageA = "Billy003"
End Sub


Sub Billy004
    MovieScreenBlank.imageA = "Billy004"
End Sub


Sub Billy005
    MovieScreenBlank.imageA = "Billy005"
End Sub


Sub Billy006
    MovieScreenBlank.imageA = "Billy006"
End Sub


Sub Billy007
    MovieScreenBlank.imageA = "Billy007"
End Sub


Sub Billy008
    MovieScreenBlank.imageA = "Billy008"
End Sub


Sub Billy001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Billy001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Billy001
    Billy002Timer.Enabled = True
End Sub


Sub Billy002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Billy002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Billy002
    Billy003Timer.Enabled = True
End Sub


Sub Billy003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Billy003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Billy003
    Billy004Timer.Enabled = True
End Sub


Sub Billy004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Billy004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Billy004
    Billy005Timer.Enabled = True
End Sub


Sub Billy005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Billy005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Billy005
    Billy006Timer.Enabled = True
End Sub


Sub Billy006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Billy006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Billy006
    Billy007Timer.Enabled = True
End Sub


Sub Billy007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Billy007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Billy007
    Billy008Timer.Enabled = True
End Sub


Sub Billy008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Billy008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Billy008
    Billy009Timer.Enabled = True
    'MovieScreen1Animation.Enabled = True
End Sub


Sub Billy009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Billy009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Billy010Timer.Enabled = True
End Sub


Sub Billy010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Billy010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen4Animation.Enabled = True
End Sub


Sub Billy011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Billy011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'Amber008
    'Amber001Timer.Enabled = True
    'MovieScreenBlank.visible = False
    MovieScreen3Animation.Enabled = True
End Sub



'Stu
Dim MovieScreenFiveCnt
MovieScreenFiveCnt= 0
MovieScreen5Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen5Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenFiveCnt = 0
	End If

	'Select the correct frame
	If MovieScreenFiveCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenFiveCnt
	Elseif MovieScreenFiveCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenFiveCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenFiveCnt
	End If
	MovieScreenFiveCnt = MovieScreenFiveCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenFiveCnt> 24 Then    
		MovieScreenFiveCnt = 0
        MovieScreen5Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Stu001Timer.Enabled = True
    End If
End Sub


'MovieScreen5Animation.Enabled = True


Dim MovieScreenSixCnt
MovieScreenSixCnt= 0
MovieScreen6Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen6Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenSixCnt = 0
	End If

	'Select the correct frame
	If MovieScreenSixCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenSixCnt
	Elseif MovieScreenSixCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenSixCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenSixCnt
	End If
	MovieScreenSixCnt = MovieScreenSixCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenSixCnt> 45 Then    
		MovieScreenSixCnt = 0
        MovieScreen6Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Stu011Timer.Enabled = True
    End If
End Sub


'MovieScreen6Animation.Enabled = True


Sub Stu001
    MovieScreenBlank.imageA = "Stu001"
End Sub


Sub Stu002
    MovieScreenBlank.imageA = "Stu002"
End Sub


Sub Stu003
    MovieScreenBlank.imageA = "Stu003"
End Sub


Sub Stu004
    MovieScreenBlank.imageA = "Stu004"
End Sub


Sub Stu005
    MovieScreenBlank.imageA = "Stu005"
End Sub


Sub Stu006
    MovieScreenBlank.imageA = "Stu006"
End Sub


Sub Stu007
    MovieScreenBlank.imageA = "Stu007"
End Sub


Sub Stu008
    MovieScreenBlank.imageA = "Stu008"
End Sub


Sub Stu001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Stu001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Stu001
    Stu002Timer.Enabled = True
End Sub


Sub Stu002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Stu002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Stu002
    Stu003Timer.Enabled = True
End Sub


Sub Stu003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Stu003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Stu003
    Stu004Timer.Enabled = True
End Sub


Sub Stu004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Stu004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Stu004
    Stu005Timer.Enabled = True
End Sub


Sub Stu005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Stu005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Stu005
    Stu006Timer.Enabled = True
End Sub


Sub Stu006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Stu006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Stu006
    Stu007Timer.Enabled = True
End Sub


Sub Stu007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Stu007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Stu007
    Stu008Timer.Enabled = True
End Sub


Sub Stu008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Stu008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Stu008
    Stu009Timer.Enabled = True
End Sub


Sub Stu009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Stu009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Stu010Timer.Enabled = True
End Sub


Sub Stu010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Stu010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen6Animation.Enabled = True
End Sub


Sub Stu011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Stu011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen5Animation.Enabled = True
End Sub



'Jill
Dim MovieScreenSevenCnt
MovieScreenSevenCnt= 0
MovieScreen7Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen7Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenSevenCnt = 0
	End If

	'Select the correct frame
	If MovieScreenSevenCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenSevenCnt
	Elseif MovieScreenSevenCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenSevenCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenSevenCnt
	End If
	MovieScreenSevenCnt = MovieScreenSevenCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenSevenCnt> 24 Then    
		MovieScreenSevenCnt = 0
        MovieScreen7Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Jill001Timer.Enabled = True
    End If
End Sub


'MovieScreen7Animation.Enabled = True


Dim MovieScreenEightCnt
MovieScreenEightCnt= 0
MovieScreen8Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen8Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenEightCnt = 0
	End If

	'Select the correct frame
	If MovieScreenEightCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenEightCnt
	Elseif MovieScreenEightCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenEightCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenEightCnt
	End If
	MovieScreenEightCnt = MovieScreenEightCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenEightCnt> 45 Then    
		MovieScreenEightCnt = 0
        MovieScreen8Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Jill011Timer.Enabled = True
    End If
End Sub


'MovieScreen8Animation.Enabled = True


Sub Jill001
    MovieScreenBlank.imageA = "Jill001"
End Sub


Sub Jill002
    MovieScreenBlank.imageA = "Jill002"
End Sub


Sub Jill003
    MovieScreenBlank.imageA = "Jill003"
End Sub


Sub Jill004
    MovieScreenBlank.imageA = "Jill004"
End Sub


Sub Jill005
    MovieScreenBlank.imageA = "Jill005"
End Sub


Sub Jill006
    MovieScreenBlank.imageA = "Jill006"
End Sub


Sub Jill007
    MovieScreenBlank.imageA = "Jill007"
End Sub


Sub Jill008
    MovieScreenBlank.imageA = "Jill008"
End Sub


Sub Jill001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Jill001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Jill001
    Jill002Timer.Enabled = True
End Sub


Sub Jill002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Jill002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Jill002
    Jill003Timer.Enabled = True
End Sub


Sub Jill003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Jill003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Jill003
    Jill004Timer.Enabled = True
End Sub


Sub Jill004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Jill004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Jill004
    Jill005Timer.Enabled = True
End Sub


Sub Jill005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Jill005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Jill005
    Jill006Timer.Enabled = True
End Sub


Sub Jill006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Jill006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Jill006
    Jill007Timer.Enabled = True
End Sub


Sub Jill007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Jill007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Jill007
    Jill008Timer.Enabled = True
End Sub


Sub Jill008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Jill008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Jill008
    Jill009Timer.Enabled = True
End Sub


Sub Jill009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Jill009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Jill010Timer.Enabled = True
End Sub


Sub Jill010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Jill010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen8Animation.Enabled = True
End Sub


Sub Jill011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Jill011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen7Animation.Enabled = True
End Sub


'Quinn
Dim MovieScreenNineCnt
MovieScreenNineCnt= 0
MovieScreen9Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen9Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenNineCnt = 0
	End If

	'Select the correct frame
	If MovieScreenNineCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenNineCnt
	Elseif MovieScreenNineCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenNineCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenNineCnt
	End If
	MovieScreenNineCnt = MovieScreenNineCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenNineCnt> 24 Then    
		MovieScreenNineCnt = 0
        MovieScreen9Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Quinn001Timer.Enabled = True
    End If
End Sub


'MovieScreen9Animation.Enabled = True


Dim MovieScreenTenCnt
MovieScreenTenCnt= 0
MovieScreen10Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen10Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTenCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTenCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenTenCnt
	Elseif MovieScreenTenCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenTenCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenTenCnt
	End If
	MovieScreenTenCnt = MovieScreenTenCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTenCnt> 45 Then    
		MovieScreenTenCnt = 0
        MovieScreen10Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Quinn011Timer.Enabled = True
    End If
End Sub


'MovieScreen10Animation.Enabled = True


Sub Quinn001
    MovieScreenBlank.imageA = "Quinn001"
End Sub


Sub Quinn002
    MovieScreenBlank.imageA = "Quinn002"
End Sub


Sub Quinn003
    MovieScreenBlank.imageA = "Quinn003"
End Sub


Sub Quinn004
    MovieScreenBlank.imageA = "Quinn004"
End Sub


Sub Quinn005
    MovieScreenBlank.imageA = "Quinn005"
End Sub


Sub Quinn006
    MovieScreenBlank.imageA = "Quinn006"
End Sub


Sub Quinn007
    MovieScreenBlank.imageA = "Quinn007"
End Sub


Sub Quinn008
    MovieScreenBlank.imageA = "Quinn008"
End Sub


Sub Quinn001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Quinn001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Quinn001
    Quinn002Timer.Enabled = True
End Sub


Sub Quinn002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Quinn002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Quinn002
    Quinn003Timer.Enabled = True
End Sub


Sub Quinn003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Quinn003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Quinn003
    Quinn004Timer.Enabled = True
End Sub


Sub Quinn004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Quinn004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Quinn004
    Quinn005Timer.Enabled = True
End Sub


Sub Quinn005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Quinn005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Quinn005
    Quinn006Timer.Enabled = True
End Sub


Sub Quinn006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Quinn006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Quinn006
    Quinn007Timer.Enabled = True
End Sub


Sub Quinn007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Quinn007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Quinn007
    Quinn008Timer.Enabled = True
End Sub


Sub Quinn008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Quinn008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Quinn008
    Quinn009Timer.Enabled = True
End Sub


Sub Quinn009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Quinn009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Quinn010Timer.Enabled = True
End Sub


Sub Quinn010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Quinn010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen10Animation.Enabled = True
End Sub


Sub Quinn011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Quinn011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen9Animation.Enabled = True
End Sub


'Mickey
Dim MovieScreenElevenCnt
MovieScreenElevenCnt= 0
MovieScreen11Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen11Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenElevenCnt = 0
	End If

	'Select the correct frame
	If MovieScreenElevenCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenElevenCnt
	Elseif MovieScreenElevenCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenElevenCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenElevenCnt
	End If
	MovieScreenElevenCnt = MovieScreenElevenCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenElevenCnt> 24 Then    
		MovieScreenElevenCnt = 0
        MovieScreen11Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Mickey001Timer.Enabled = True
    End If
End Sub


'MovieScreen11Animation.Enabled = True


Dim MovieScreenTwelveCnt
MovieScreenTwelveCnt= 0
MovieScreen12Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen12Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwelveCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwelveCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenTwelveCnt
	Elseif MovieScreenTwelveCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenTwelveCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenTwelveCnt
	End If
	MovieScreenTwelveCnt = MovieScreenTwelveCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwelveCnt> 45 Then    
		MovieScreenTwelveCnt = 0
        MovieScreen12Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Mickey011Timer.Enabled = True
    End If
End Sub


'MovieScreen12Animation.Enabled = True


Sub Mickey001
    MovieScreenBlank.imageA = "Mickey001"
End Sub


Sub Mickey002
    MovieScreenBlank.imageA = "Mickey002"
End Sub


Sub Mickey003
    MovieScreenBlank.imageA = "Mickey003"
End Sub


Sub Mickey004
    MovieScreenBlank.imageA = "Mickey004"
End Sub


Sub Mickey005
    MovieScreenBlank.imageA = "Mickey005"
End Sub


Sub Mickey006
    MovieScreenBlank.imageA = "Mickey006"
End Sub


Sub Mickey007
    MovieScreenBlank.imageA = "Mickey007"
End Sub


Sub Mickey008
    MovieScreenBlank.imageA = "Mickey008"
End Sub


Sub Mickey001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mickey001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mickey001
    Mickey002Timer.Enabled = True
End Sub


Sub Mickey002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mickey002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mickey002
    Mickey003Timer.Enabled = True
End Sub


Sub Mickey003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mickey003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mickey003
    Mickey004Timer.Enabled = True
End Sub


Sub Mickey004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mickey004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mickey004
    Mickey005Timer.Enabled = True
End Sub


Sub Mickey005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mickey005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mickey005
    Mickey006Timer.Enabled = True
End Sub


Sub Mickey006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mickey006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mickey006
    Mickey007Timer.Enabled = True
End Sub


Sub Mickey007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mickey007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mickey007
    Mickey008Timer.Enabled = True
End Sub


Sub Mickey008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mickey008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mickey008
    Mickey009Timer.Enabled = True
End Sub


Sub Mickey009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mickey009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mickey010Timer.Enabled = True
End Sub


Sub Mickey010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mickey010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen12Animation.Enabled = True
End Sub


Sub Mickey011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mickey011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen11Animation.Enabled = True
End Sub


'Mrs Loomis
Dim MovieScreenThirteenCnt
MovieScreenThirteenCnt= 0
MovieScreen13Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen13Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenThirteenCnt = 0
	End If

	'Select the correct frame
	If MovieScreenThirteenCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenThirteenCnt
	Elseif MovieScreenThirteenCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenThirteenCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenThirteenCnt
	End If
	MovieScreenThirteenCnt = MovieScreenThirteenCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenThirteenCnt> 24 Then    
		MovieScreenThirteenCnt = 0
        MovieScreen13Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Mrsloomis001Timer.Enabled = True
    End If
End Sub


'MovieScreen13Animation.Enabled = True


Dim MovieScreenFourteenCnt
MovieScreenFourteenCnt= 0
MovieScreen14Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen14Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenFourteenCnt = 0
	End If

	'Select the correct frame
	If MovieScreenFourteenCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenFourteenCnt
	Elseif MovieScreenFourteenCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenFourteenCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenFourteenCnt
	End If
	MovieScreenFourteenCnt = MovieScreenFourteenCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenFourteenCnt> 45 Then    
		MovieScreenFourteenCnt = 0
        MovieScreen14Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Mrsloomis011Timer.Enabled = True
    End If
End Sub


'MovieScreen14Animation.Enabled = True


Sub Mrsloomis001
    MovieScreenBlank.imageA = "Mrsloomis001"
End Sub


Sub Mrsloomis002
    MovieScreenBlank.imageA = "Mrsloomis002"
End Sub


Sub Mrsloomis003
    MovieScreenBlank.imageA = "Mrsloomis003"
End Sub


Sub Mrsloomis004
    MovieScreenBlank.imageA = "Mrsloomis004"
End Sub


Sub Mrsloomis005
    MovieScreenBlank.imageA = "Mrsloomis005"
End Sub


Sub Mrsloomis006
    MovieScreenBlank.imageA = "Mrsloomis006"
End Sub


Sub Mrsloomis007
    MovieScreenBlank.imageA = "Mrsloomis007"
End Sub


Sub Mrsloomis008
    MovieScreenBlank.imageA = "Mrsloomis008"
End Sub


Sub Mrsloomis001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mrsloomis001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mrsloomis001
    Mrsloomis002Timer.Enabled = True
End Sub


Sub Mrsloomis002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mrsloomis002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mrsloomis002
    Mrsloomis003Timer.Enabled = True
End Sub


Sub Mrsloomis003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mrsloomis003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mrsloomis003
    Mrsloomis004Timer.Enabled = True
End Sub


Sub Mrsloomis004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mrsloomis004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mrsloomis004
    Mrsloomis005Timer.Enabled = True
End Sub


Sub Mrsloomis005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mrsloomis005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mrsloomis005
    Mrsloomis006Timer.Enabled = True
End Sub


Sub Mrsloomis006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mrsloomis006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mrsloomis006
    Mrsloomis007Timer.Enabled = True
End Sub


Sub Mrsloomis007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mrsloomis007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mrsloomis007
    Mrsloomis008Timer.Enabled = True
End Sub


Sub Mrsloomis008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mrsloomis008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mrsloomis008
    Mrsloomis009Timer.Enabled = True
End Sub


Sub Mrsloomis009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mrsloomis009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Mrsloomis010Timer.Enabled = True
End Sub


Sub Mrsloomis010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mrsloomis010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen14Animation.Enabled = True
End Sub


Sub Mrsloomis011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Mrsloomis011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen13Animation.Enabled = True
End Sub


'Roman
Dim MovieScreenFifteenCnt
MovieScreenFifteenCnt= 0
MovieScreen15Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen15Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenFifteenCnt = 0
	End If

	'Select the correct frame
	If MovieScreenFifteenCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenFifteenCnt
	Elseif MovieScreenFifteenCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenFifteenCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenFifteenCnt
	End If
	MovieScreenFifteenCnt = MovieScreenFifteenCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenFifteenCnt> 24 Then    
		MovieScreenFifteenCnt = 0
        MovieScreen15Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Roman001Timer.Enabled = True
    End If
End Sub


'MovieScreen15Animation.Enabled = True


Dim MovieScreenSixteenCnt
MovieScreenSixteenCnt= 0
MovieScreen16Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen16Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenSixteenCnt = 0
	End If

	'Select the correct frame
	If MovieScreenSixteenCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenSixteenCnt
	Elseif MovieScreenSixteenCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenSixteenCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenSixteenCnt
	End If
	MovieScreenSixteenCnt = MovieScreenSixteenCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenSixteenCnt> 45 Then    
		MovieScreenSixteenCnt = 0
        MovieScreen16Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Roman011Timer.Enabled = True
    End If
End Sub


'MovieScreen16Animation.Enabled = True


Sub Roman001
    MovieScreenBlank.imageA = "Roman001"
End Sub


Sub Roman002
    MovieScreenBlank.imageA = "Roman002"
End Sub


Sub Roman003
    MovieScreenBlank.imageA = "Roman003"
End Sub


Sub Roman004
    MovieScreenBlank.imageA = "Roman004"
End Sub


Sub Roman005
    MovieScreenBlank.imageA = "Roman005"
End Sub


Sub Roman006
    MovieScreenBlank.imageA = "Roman006"
End Sub


Sub Roman007
    MovieScreenBlank.imageA = "Roman007"
End Sub


Sub Roman008
    MovieScreenBlank.imageA = "Roamn008"
End Sub


Sub Roman001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Roman001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Roman001
    Roman002Timer.Enabled = True
End Sub


Sub Roman002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Roman002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Roman002
    Roman003Timer.Enabled = True
End Sub


Sub Roman003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Roman003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Roman003
    Roman004Timer.Enabled = True
End Sub


Sub Roman004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Roman004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Roman004
    Roman005Timer.Enabled = True
End Sub


Sub Roman005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Roman005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Roman005
    Roman006Timer.Enabled = True
End Sub


Sub Roman006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Roman006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Roman006
    Roman007Timer.Enabled = True
End Sub


Sub Roman007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Roman007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Roman007
    Roman008Timer.Enabled = True
End Sub


Sub Roman008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Roman008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Roman008
    Roman009Timer.Enabled = True
End Sub


Sub Roman009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Roman009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Roman010Timer.Enabled = True
End Sub


Sub Roman010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Roman010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen16Animation.Enabled = True
End Sub


Sub Roman011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Roman011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen15Animation.Enabled = True
End Sub


'Charlie
Dim MovieScreenSeventeenCnt
MovieScreenSeventeenCnt= 0
MovieScreen17Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen17Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenSeventeenCnt = 0
	End If

	'Select the correct frame
	If MovieScreenSeventeenCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenSeventeenCnt
	Elseif MovieScreenSeventeenCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenSeventeenCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenSeventeenCnt
	End If
	MovieScreenSeventeenCnt = MovieScreenSeventeenCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenSeventeenCnt> 24 Then    
		MovieScreenSeventeenCnt = 0
        MovieScreen17Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Charlie001Timer.Enabled = True
    End If
End Sub


'MovieScreen17Animation.Enabled = True


Dim MovieScreenEightteenCnt
MovieScreenEightteenCnt= 0
MovieScreen18Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen18Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenEightteenCnt = 0
	End If

	'Select the correct frame
	If MovieScreenEightteenCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenEightteenCnt
	Elseif MovieScreenEightteenCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenEightteenCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenEightteenCnt
	End If
	MovieScreenEightteenCnt = MovieScreenEightteenCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenEightteenCnt> 45 Then    
		MovieScreenEightteenCnt = 0
        MovieScreen18Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Charlie011Timer.Enabled = True
    End If
End Sub


'MovieScreen18Animation.Enabled = True


Sub Charlie001
    MovieScreenBlank.imageA = "Charlie001"
End Sub


Sub Charlie002
    MovieScreenBlank.imageA = "Charlie002"
End Sub


Sub Charlie003
    MovieScreenBlank.imageA = "Charlie003"
End Sub


Sub Charlie004
    MovieScreenBlank.imageA = "Charlie004"
End Sub


Sub Charlie005
    MovieScreenBlank.imageA = "Charlie005"
End Sub


Sub Charlie006
    MovieScreenBlank.imageA = "Charlie006"
End Sub


Sub Charlie007
    MovieScreenBlank.imageA = "Charlie007"
End Sub


Sub Charlie008
    MovieScreenBlank.imageA = "Charlie008"
End Sub


Sub Charlie001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Charlie001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Charlie001
    Charlie002Timer.Enabled = True
End Sub


Sub Charlie002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Charlie002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Charlie002
    Charlie003Timer.Enabled = True
End Sub


Sub Charlie003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Charlie003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Charlie003
    Charlie004Timer.Enabled = True
End Sub


Sub Charlie004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Charlie004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Charlie004
    Charlie005Timer.Enabled = True
End Sub


Sub Charlie005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Charlie005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Charlie005
    Charlie006Timer.Enabled = True
End Sub


Sub Charlie006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Charlie006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Charlie006
    Charlie007Timer.Enabled = True
End Sub


Sub Charlie007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Charlie007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Charlie007
    Charlie008Timer.Enabled = True
End Sub


Sub Charlie008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Charlie008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Charlie008
    Charlie009Timer.Enabled = True
End Sub


Sub Charlie009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Charlie009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Charlie010Timer.Enabled = True
End Sub


Sub Charlie010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Charlie010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen18Animation.Enabled = True
End Sub


Sub Charlie011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Charlie011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen17Animation.Enabled = True
End Sub


'Richie
Dim MovieScreenNineteenCnt
MovieScreenNineteenCnt= 0
MovieScreen19Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen19Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenNineteenCnt = 0
	End If

	'Select the correct frame
	If MovieScreenNineteenCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenNineteenCnt
	Elseif MovieScreenNineteenCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenNineteenCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenNineteenCnt
	End If
	MovieScreenNineteenCnt = MovieScreenNineteenCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenNineteenCnt> 24 Then    
		MovieScreenNineteenCnt = 0
        MovieScreen19Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Richie001Timer.Enabled = True
    End If
End Sub


'MovieScreen19Animation.Enabled = True


Dim MovieScreenTwentyCnt
MovieScreenTwentyCnt= 0
MovieScreen20Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen20Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwentyCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwentyCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenTwentyCnt
	Elseif MovieScreenTwentyCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenTwentyCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenTwentyCnt
	End If
	MovieScreenTwentyCnt = MovieScreenTwentyCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwentyCnt> 45 Then    
		MovieScreenTwentyCnt = 0
        MovieScreen20Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Richie011Timer.Enabled = True
    End If
End Sub


'MovieScreen20Animation.Enabled = True


Sub Richie001
    MovieScreenBlank.imageA = "Richie001"
End Sub


Sub Richie002
    MovieScreenBlank.imageA = "Richie002"
End Sub


Sub Richie003
    MovieScreenBlank.imageA = "Richie003"
End Sub


Sub Richie004
    MovieScreenBlank.imageA = "Richie004"
End Sub


Sub Richie005
    MovieScreenBlank.imageA = "Richie005"
End Sub


Sub Richie006
    MovieScreenBlank.imageA = "Richie006"
End Sub


Sub Richie007
    MovieScreenBlank.imageA = "Richie007"
End Sub


Sub Richie008
    MovieScreenBlank.imageA = "Richie008"
End Sub


Sub Richie001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Richie001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Richie001
    Richie002Timer.Enabled = True
End Sub


Sub Richie002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Richie002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Richie002
    Richie003Timer.Enabled = True
End Sub


Sub Richie003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Richie003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Richie003
    Richie004Timer.Enabled = True
End Sub


Sub Richie004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Richie004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Richie004
    Richie005Timer.Enabled = True
End Sub


Sub Richie005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Richie005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Richie005
    Richie006Timer.Enabled = True
End Sub


Sub Richie006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Richie006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Richie006
    Richie007Timer.Enabled = True
End Sub


Sub Richie007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Richie007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Richie007
    Richie008Timer.Enabled = True
End Sub


Sub Richie008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Richie008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Richie008
    Richie009Timer.Enabled = True
End Sub


Sub Richie009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Richie009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Richie010Timer.Enabled = True
End Sub


Sub Richie010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Richie010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen20Animation.Enabled = True
End Sub


Sub Richie011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Richie011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen19Animation.Enabled = True
End Sub


'Ethan
Dim MovieScreenTwentyoneCnt
MovieScreenTwentyoneCnt= 0
MovieScreen21Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen21Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwentyoneCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwentyoneCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenTwentyoneCnt
	Elseif MovieScreenTwentyoneCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenTwentyoneCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenTwentyoneCnt
	End If
	MovieScreenTwentyoneCnt = MovieScreenTwentyoneCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwentyoneCnt> 24 Then    
		MovieScreenTwentyoneCnt = 0
        MovieScreen21Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Ethan001Timer.Enabled = True
    End If
End Sub


'MovieScreen21Animation.Enabled = True


Dim MovieScreenTwentytwoCnt
MovieScreenTwentytwoCnt= 0
MovieScreen22Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen22Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwentytwoCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwentytwoCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenTwentytwoCnt
	Elseif MovieScreenTwentytwoCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenTwentytwoCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenTwentytwoCnt
	End If
	MovieScreenTwentytwoCnt = MovieScreenTwentytwoCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwentytwoCnt> 45 Then    
		MovieScreenTwentytwoCnt = 0
        MovieScreen22Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Ethan011Timer.Enabled = True
    End If
End Sub


'MovieScreen22Animation.Enabled = True


Sub Ethan001
    MovieScreenBlank.imageA = "Ethan001"
End Sub


Sub Ethan002
    MovieScreenBlank.imageA = "Ethan002"
End Sub


Sub Ethan003
    MovieScreenBlank.imageA = "Ethan003"
End Sub


Sub Ethan004
    MovieScreenBlank.imageA = "Ethan004"
End Sub


Sub Ethan005
    MovieScreenBlank.imageA = "Ethan005"
End Sub


Sub Ethan006
    MovieScreenBlank.imageA = "Ethan006"
End Sub


Sub Ethan007
    MovieScreenBlank.imageA = "Ethan007"
End Sub


Sub Ethan008
    MovieScreenBlank.imageA = "Ethan008"
End Sub


Sub Ethan001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Ethan001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Ethan001
    Ethan002Timer.Enabled = True
End Sub


Sub Ethan002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Ethan002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Ethan002
    Ethan003Timer.Enabled = True
End Sub


Sub Ethan003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Ethan003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Ethan003
    Ethan004Timer.Enabled = True
End Sub


Sub Ethan004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Ethan004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Ethan004
    Ethan005Timer.Enabled = True
End Sub


Sub Ethan005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Ethan005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Ethan005
    Ethan006Timer.Enabled = True
End Sub


Sub Ethan006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Ethan006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Ethan006
    Ethan007Timer.Enabled = True
End Sub


Sub Ethan007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Ethan007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Ethan007
    Ethan008Timer.Enabled = True
End Sub


Sub Ethan008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Ethan008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Ethan008
    Ethan009Timer.Enabled = True
End Sub


Sub Ethan009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Ethan009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Ethan010Timer.Enabled = True
End Sub


Sub Ethan010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Ethan010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen22Animation.Enabled = True
End Sub


Sub Ethan011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Ethan011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen21Animation.Enabled = True
End Sub


'Wayne
Dim MovieScreenTwentThreeCnt
MovieScreenTwentThreeCnt= 0
MovieScreen23Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen23Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwentThreeCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwentThreeCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenTwentThreeCnt
	Elseif MovieScreenTwentThreeCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenTwentThreeCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenTwentThreeCnt
	End If
	MovieScreenTwentThreeCnt = MovieScreenTwentThreeCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwentThreeCnt> 24 Then    
		MovieScreenTwentThreeCnt = 0
        MovieScreen23Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Wayne001Timer.Enabled = True
    End If
End Sub


'MovieScreen23Animation.Enabled = True


Dim MovieScreenTwentyFourCnt
MovieScreenTwentyFourCnt= 0
MovieScreen24Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen24Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwentyFourCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwentyFourCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenTwentyFourCnt
	Elseif MovieScreenTwentyFourCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenTwentyFourCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenTwentyFourCnt
	End If
	MovieScreenTwentyFourCnt = MovieScreenTwentyFourCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwentyFourCnt> 45 Then    
		MovieScreenTwentyFourCnt = 0
        MovieScreen24Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Wayne011Timer.Enabled = True
    End If
End Sub


'MovieScreen24Animation.Enabled = True


Sub Wayne001
    MovieScreenBlank.imageA = "Wayne001"
End Sub


Sub Wayne002
    MovieScreenBlank.imageA = "Wayne002"
End Sub


Sub Wayne003
    MovieScreenBlank.imageA = "Wayne003"
End Sub


Sub Wayne004
    MovieScreenBlank.imageA = "Wayne004"
End Sub


Sub Wayne005
    MovieScreenBlank.imageA = "Wayne005"
End Sub


Sub Wayne006
    MovieScreenBlank.imageA = "Wayne006"
End Sub


Sub Wayne007
    MovieScreenBlank.imageA = "Wayne007"
End Sub


Sub Wayne008
    MovieScreenBlank.imageA = "Wayne008"
End Sub


Sub Wayne001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Wayne001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Wayne001
    Wayne002Timer.Enabled = True
End Sub


Sub Wayne002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Wayne002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Wayne002
    Wayne003Timer.Enabled = True
End Sub


Sub Wayne003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Wayne003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Wayne003
    Wayne004Timer.Enabled = True
End Sub


Sub Wayne004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Wayne004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Wayne004
    Wayne005Timer.Enabled = True
End Sub


Sub Wayne005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Wayne005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Wayne005
    Wayne006Timer.Enabled = True
End Sub


Sub Wayne006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Wayne006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Wayne006
    Wayne007Timer.Enabled = True
End Sub


Sub Wayne007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Wayne007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Wayne007
    Wayne008Timer.Enabled = True
End Sub


Sub Wayne008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Wayne008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Wayne008
    Wayne009Timer.Enabled = True
End Sub


Sub Wayne009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Wayne009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Wayne010Timer.Enabled = True
End Sub


Sub Wayne010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Wayne010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen24Animation.Enabled = True
End Sub


Sub Wayne011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Wayne011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen23Animation.Enabled = True
End Sub


'Dewey
Dim MovieScreenTwentFiveCnt
MovieScreenTwentFiveCnt= 0
MovieScreen25Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen25Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwentFiveCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwentFiveCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenTwentFiveCnt
	Elseif MovieScreenTwentFiveCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenTwentFiveCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenTwentFiveCnt
	End If
	MovieScreenTwentFiveCnt = MovieScreenTwentFiveCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwentFiveCnt> 24 Then    
		MovieScreenTwentFiveCnt = 0
        MovieScreen25Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Dewey001Timer.Enabled = True
    End If
End Sub


'MovieScreen25Animation.Enabled = True


Dim MovieScreenTwentySixCnt
MovieScreenTwentySixCnt= 0
MovieScreen26Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen26Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwentySixCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwentySixCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenTwentySixCnt
	Elseif MovieScreenTwentySixCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenTwentySixCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenTwentySixCnt
	End If
	MovieScreenTwentySixCnt = MovieScreenTwentySixCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwentySixCnt> 45 Then    
		MovieScreenTwentySixCnt = 0
        MovieScreen26Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Dewey011Timer.Enabled = True
    End If
End Sub


'MovieScreen26Animation.Enabled = True


Sub Dewey001
    MovieScreenBlank.imageA = "Dewey001"
End Sub


Sub Dewey002
    MovieScreenBlank.imageA = "Dewey002"
End Sub


Sub Dewey003
    MovieScreenBlank.imageA = "Dewey003"
End Sub


Sub Dewey004
    MovieScreenBlank.imageA = "Dewey004"
End Sub


Sub Dewey005
    MovieScreenBlank.imageA = "Dewey005"
End Sub


Sub Dewey006
    MovieScreenBlank.imageA = "Dewey006"
End Sub


Sub Dewey007
    MovieScreenBlank.imageA = "Dewey007"
End Sub


Sub Dewey008
    MovieScreenBlank.imageA = "Dewey008"
End Sub


Sub Dewey001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Dewey001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Dewey001
    Dewey002Timer.Enabled = True
End Sub


Sub Dewey002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Dewey002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Dewey002
    Dewey003Timer.Enabled = True
End Sub


Sub Dewey003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Dewey003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Dewey003
    Dewey004Timer.Enabled = True
End Sub


Sub Dewey004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Dewey004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Dewey004
    Dewey005Timer.Enabled = True
End Sub


Sub Dewey005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Dewey005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Dewey005
    Dewey006Timer.Enabled = True
End Sub


Sub Dewey006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Dewey006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Dewey006
    Dewey007Timer.Enabled = True
End Sub


Sub Dewey007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Dewey007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Dewey007
    Dewey008Timer.Enabled = True
End Sub


Sub Dewey008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Dewey008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Dewey008
    Dewey009Timer.Enabled = True
End Sub


Sub Dewey009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Dewey009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Dewey010Timer.Enabled = True
End Sub


Sub Dewey010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Dewey010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen26Animation.Enabled = True
End Sub


Sub Dewey011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Dewey011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen25Animation.Enabled = True
End Sub


'Gale
Dim MovieScreenTwentSevenCnt
MovieScreenTwentSevenCnt= 0
MovieScreen27Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen27Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwentSevenCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwentSevenCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenTwentSevenCnt
	Elseif MovieScreenTwentSevenCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenTwentSevenCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenTwentSevenCnt
	End If
	MovieScreenTwentSevenCnt = MovieScreenTwentSevenCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwentSevenCnt> 24 Then    
		MovieScreenTwentSevenCnt = 0
        MovieScreen27Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Gale001Timer.Enabled = True
    End If
End Sub


'MovieScreen27Animation.Enabled = True


Dim MovieScreenTwentyEightCnt
MovieScreenTwentyEightCnt= 0
MovieScreen28Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen28Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwentyEightCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwentyEightCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenTwentyEightCnt
	Elseif MovieScreenTwentyEightCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenTwentyEightCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenTwentyEightCnt
	End If
	MovieScreenTwentyEightCnt = MovieScreenTwentyEightCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwentyEightCnt> 45 Then    
		MovieScreenTwentyEightCnt = 0
        MovieScreen28Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Gale011Timer.Enabled = True
    End If
End Sub


'MovieScreen28Animation.Enabled = True


Sub Gale001
    MovieScreenBlank.imageA = "Gale001"
End Sub


Sub Gale002
    MovieScreenBlank.imageA = "Gale002"
End Sub


Sub Gale003
    MovieScreenBlank.imageA = "Gale003"
End Sub


Sub Gale004
    MovieScreenBlank.imageA = "Gale004"
End Sub


Sub Gale005
    MovieScreenBlank.imageA = "Gale005"
End Sub


Sub Gale006
    MovieScreenBlank.imageA = "Gale006"
End Sub


Sub Gale007
    MovieScreenBlank.imageA = "Gale007"
End Sub


Sub Gale008
    MovieScreenBlank.imageA = "Gale008"
End Sub


Sub Gale001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Gale001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Gale001
    Gale002Timer.Enabled = True
End Sub


Sub Gale002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Gale002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Gale002
    Gale003Timer.Enabled = True
End Sub


Sub Gale003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Gale003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Gale003
    Gale004Timer.Enabled = True
End Sub


Sub Gale004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Gale004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Gale004
    Gale005Timer.Enabled = True
End Sub


Sub Gale005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Gale005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Gale005
    Gale006Timer.Enabled = True
End Sub


Sub Gale006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Gale006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Gale006
    Gale007Timer.Enabled = True
End Sub


Sub Gale007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Gale007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Gale007
    Gale008Timer.Enabled = True
End Sub


Sub Gale008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Gale008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Gale008
    Gale009Timer.Enabled = True
End Sub


Sub Gale009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Gale009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Gale010Timer.Enabled = True
End Sub


Sub Gale010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Gale010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen28Animation.Enabled = True
End Sub


Sub Gale011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Gale011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen27Animation.Enabled = True
End Sub



'Tara
Dim MovieScreenTwentyNineCnt
MovieScreenTwentyNineCnt= 0
MovieScreen29Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen29Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenTwentyNineCnt = 0
	End If

	'Select the correct frame
	If MovieScreenTwentyNineCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenTwentyNineCnt
	Elseif MovieScreenTwentyNineCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenTwentyNineCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenTwentyNineCnt
	End If
	MovieScreenTwentyNineCnt = MovieScreenTwentyNineCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenTwentyNineCnt> 24 Then    
		MovieScreenTwentyNineCnt = 0
        MovieScreen29Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Tara001Timer.Enabled = True
    End If
End Sub


'MovieScreen29Animation.Enabled = True


Dim MovieScreenThirtyCnt
MovieScreenThirtyCnt= 0
MovieScreen30Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen30Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenThirtyCnt = 0
	End If

	'Select the correct frame
	If MovieScreenThirtyCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenThirtyCnt
	Elseif MovieScreenThirtyCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenThirtyCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenThirtyCnt
	End If
	MovieScreenThirtyCnt = MovieScreenThirtyCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenThirtyCnt> 45 Then    
		MovieScreenThirtyCnt = 0
        MovieScreen30Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Tara011Timer.Enabled = True
    End If
End Sub


'MovieScreen30Animation.Enabled = True


Sub Tara001
    MovieScreenBlank.imageA = "Tara001"
End Sub


Sub Tara002
    MovieScreenBlank.imageA = "Tara002"
End Sub


Sub Tara003
    MovieScreenBlank.imageA = "Tara003"
End Sub


Sub Tara004
    MovieScreenBlank.imageA = "Tara004"
End Sub


Sub Tara005
    MovieScreenBlank.imageA = "Tara005"
End Sub


Sub Tara006
    MovieScreenBlank.imageA = "Tara006"
End Sub


Sub Tara007
    MovieScreenBlank.imageA = "Tara007"
End Sub


Sub Tara008
    MovieScreenBlank.imageA = "Tara008"
End Sub


Sub Tara001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Tara001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Tara001
    Tara002Timer.Enabled = True
End Sub


Sub Tara002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Tara002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Tara002
    Tara003Timer.Enabled = True
End Sub


Sub Tara003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Tara003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Tara003
    Tara004Timer.Enabled = True
End Sub


Sub Tara004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Tara004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Tara004
    Tara005Timer.Enabled = True
End Sub


Sub Tara005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Tara005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Tara005
    Tara006Timer.Enabled = True
End Sub


Sub Tara006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Tara006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Tara006
    Tara007Timer.Enabled = True
End Sub


Sub Tara007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Tara007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Tara007
    Tara008Timer.Enabled = True
End Sub


Sub Tara008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Tara008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Tara008
    Tara009Timer.Enabled = True
End Sub


Sub Tara009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Tara009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Tara010Timer.Enabled = True
End Sub


Sub Tara010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Tara010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen30Animation.Enabled = True
End Sub


Sub Tara011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Tara011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen29Animation.Enabled = True
End Sub


'Sam
Dim MovieScreenThirtyOneCnt
MovieScreenThirtyOneCnt= 0
MovieScreen31Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen31Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenThirtyOneCnt = 0
	End If

	'Select the correct frame
	If MovieScreenThirtyOneCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenThirtyOneCnt
	Elseif MovieScreenThirtyOneCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenThirtyOneCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenThirtyOneCnt
	End If
	MovieScreenThirtyOneCnt = MovieScreenThirtyOneCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenThirtyOneCnt> 24 Then    
		MovieScreenThirtyOneCnt = 0
        MovieScreen31Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Sam001Timer.Enabled = True
    End If
End Sub


'MovieScreen31Animation.Enabled = True


Dim MovieScreenThirtyTwoCnt
MovieScreenThirtyTwoCnt= 0
MovieScreen32Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen32Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenThirtyTwoCnt = 0
	End If

	'Select the correct frame
	If MovieScreenThirtyTwoCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenThirtyTwoCnt
	Elseif MovieScreenThirtyTwoCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenThirtyTwoCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenThirtyTwoCnt
	End If
	MovieScreenThirtyTwoCnt = MovieScreenThirtyTwoCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenThirtyTwoCnt> 45 Then    
		MovieScreenThirtyTwoCnt = 0
        MovieScreen32Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Sam011Timer.Enabled = True
    End If
End Sub


'MovieScreen32Animation.Enabled = True


Sub Sam001
    MovieScreenBlank.imageA = "Sam001"
End Sub


Sub Sam002
    MovieScreenBlank.imageA = "Sam002"
End Sub


Sub Sam003
    MovieScreenBlank.imageA = "Sam003"
End Sub


Sub Sam004
    MovieScreenBlank.imageA = "Sam004"
End Sub


Sub Sam005
    MovieScreenBlank.imageA = "Sam005"
End Sub


Sub Sam006
    MovieScreenBlank.imageA = "Sam006"
End Sub


Sub Sam007
    MovieScreenBlank.imageA = "Sam007"
End Sub


Sub Sam008
    MovieScreenBlank.imageA = "Sam008"
End Sub


Sub Sam001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sam001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sam001
    Sam002Timer.Enabled = True
End Sub


Sub Sam002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sam002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sam002
    Sam003Timer.Enabled = True
End Sub


Sub Sam003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sam003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sam003
    Sam004Timer.Enabled = True
End Sub


Sub Sam004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sam004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sam004
    Sam005Timer.Enabled = True
End Sub


Sub Sam005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sam005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sam005
    Sam006Timer.Enabled = True
End Sub


Sub Sam006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sam006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sam006
    Sam007Timer.Enabled = True
End Sub


Sub Sam007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sam007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sam007
    Sam008Timer.Enabled = True
End Sub


Sub Sam008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sam008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sam008
    Sam009Timer.Enabled = True
End Sub


Sub Sam009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sam009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sam010Timer.Enabled = True
End Sub


Sub Sam010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sam010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen32Animation.Enabled = True
End Sub


Sub Sam011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sam011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen31Animation.Enabled = True
End Sub


'Sid
Dim MovieScreenThirtyThreeCnt
MovieScreenThirtyThreeCnt= 0
MovieScreen33Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen33Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenThirtyThreeCnt = 0
	End If

	'Select the correct frame
	If MovieScreenThirtyThreeCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenThirtyThreeCnt
	Elseif MovieScreenThirtyThreeCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenThirtyThreeCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenThirtyThreeCnt
	End If
	MovieScreenThirtyThreeCnt = MovieScreenThirtyThreeCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenThirtyThreeCnt> 24 Then    
		MovieScreenThirtyThreeCnt = 0
        MovieScreen33Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Sid001Timer.Enabled = True
    End If
End Sub


'MovieScreen33Animation.Enabled = True


Dim MovieScreenThirtyFourCnt
MovieScreenThirtyFourCnt= 0
MovieScreen34Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen34Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenThirtyFourCnt = 0
	End If

	'Select the correct frame
	If MovieScreenThirtyFourCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenThirtyFourCnt
	Elseif MovieScreenThirtyFourCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenThirtyFourCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenThirtyFourCnt
	End If
	MovieScreenThirtyFourCnt = MovieScreenThirtyFourCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenThirtyFourCnt> 45 Then    
		MovieScreenThirtyFourCnt = 0
        MovieScreen34Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        Sid011Timer.Enabled = True
    End If
End Sub


'MovieScreen34Animation.Enabled = True


Sub Sid001
    MovieScreenBlank.imageA = "Sid001"
End Sub


Sub Sid002
    MovieScreenBlank.imageA = "Sid002"
End Sub


Sub Sid003
    MovieScreenBlank.imageA = "Sid003"
End Sub


Sub Sid004
    MovieScreenBlank.imageA = "Sid004"
End Sub


Sub Sid005
    MovieScreenBlank.imageA = "Sid005"
End Sub


Sub Sid006
    MovieScreenBlank.imageA = "Sid006"
End Sub


Sub Sid007
    MovieScreenBlank.imageA = "Sid007"
End Sub


Sub Sid008
    MovieScreenBlank.imageA = "Sid008"
End Sub


Sub Sid001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sid001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sid001
    Sid002Timer.Enabled = True
End Sub


Sub Sid002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sid002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sid002
    Sid003Timer.Enabled = True
End Sub


Sub Sid003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sid003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sid003
    Sid004Timer.Enabled = True
End Sub


Sub Sid004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sid004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sid004
    Sid005Timer.Enabled = True
End Sub


Sub Sid005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sid005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sid005
    Sid006Timer.Enabled = True
End Sub


Sub Sid006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sid006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sid006
    Sid007Timer.Enabled = True
End Sub


Sub Sid007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sid007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sid007
    Sid008Timer.Enabled = True
End Sub


Sub Sid008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sid008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sid008
    Sid009Timer.Enabled = True
End Sub


Sub Sid009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sid009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Sid010Timer.Enabled = True
End Sub


Sub Sid010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sid010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen34Animation.Enabled = True
End Sub


Sub Sid011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    Sid011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen33Animation.Enabled = True
End Sub


'Scream Attract and Wizard
Dim MovieScreenThirtyFiveCnt
MovieScreenThirtyFiveCnt= 0
MovieScreen35Animation.Interval=17*4 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen35Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenThirtyFiveCnt = 0
	End If

	'Select the correct frame
	If MovieScreenThirtyFiveCnt > 99 Then
		MovieScreen.imageA = "ScreamOne-" & MovieScreenThirtyFiveCnt
	Elseif MovieScreenThirtyFiveCnt > 9 Then
		MovieScreen.imageA = "ScreamOne-0" & MovieScreenThirtyFiveCnt
	Else
		MovieScreen.imageA = "ScreamOne-00" & MovieScreenThirtyFiveCnt
	End If
	MovieScreenThirtyFiveCnt = MovieScreenThirtyFiveCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenThirtyFiveCnt> 24 Then    
		MovieScreenThirtyFiveCnt = 0
        MovieScreen35Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        SM001Timer.Enabled = True
    End If
End Sub


'MovieScreen35Animation.Enabled = True


Dim MovieScreenThirtySixCnt
MovieScreenThirtySixCnt= 0
MovieScreen36Animation.Interval=17*5 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MovieScreen36Animation_Timer
	'Initialize the animation
	If MovieScreen.visible = False Then 
		MovieScreen.visible = True
        MovieScreenBlank.visible = False
		MovieScreenThirtySixCnt = 0
	End If

	'Select the correct frame
	If MovieScreenThirtySixCnt > 99 Then
		MovieScreen.imageA = "ScreamTwo-" & MovieScreenThirtySixCnt
	Elseif MovieScreenThirtySixCnt > 9 Then
		MovieScreen.imageA = "ScreamTwo-0" & MovieScreenThirtySixCnt
	Else
		MovieScreen.imageA = "ScreamTwo-00" & MovieScreenThirtySixCnt
	End If
	MovieScreenThirtySixCnt = MovieScreenThirtySixCnt + 1
    Movie1Light.State=1

	'Finish animation
	If MovieScreenThirtySixCnt> 45 Then    
		MovieScreenThirtySixCnt = 0
        MovieScreen36Animation.Enabled = False
        'Poster001
		MovieScreen.visible = False
        MovieScreenBlank.visible = True
        MovieScreenBlank.imageA = ""
        SM011Timer.Enabled = True
    End If
End Sub


'MovieScreen36Animation.Enabled = True


Sub SM001
    MovieScreenBlank.imageA = "SM001"
End Sub


Sub SM002
    MovieScreenBlank.imageA = "SM002"
End Sub


Sub SM003
    MovieScreenBlank.imageA = "SM003"
End Sub


Sub SM004
    MovieScreenBlank.imageA = "SM004"
End Sub


Sub SM005
    MovieScreenBlank.imageA = "SM005"
End Sub


Sub SM006
    MovieScreenBlank.imageA = "SM006"
End Sub


Sub SM007
    MovieScreenBlank.imageA = "SM007"
End Sub


Sub SM008
    MovieScreenBlank.imageA = "SM008"
End Sub


Sub SM001Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SM001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SM001
    SM002Timer.Enabled = True
End Sub


Sub SM002Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SM002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SM002
    SM003Timer.Enabled = True
End Sub


Sub SM003Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SM003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SM003
    SM004Timer.Enabled = True
End Sub


Sub SM004Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SM004Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SM004
    SM005Timer.Enabled = True
End Sub


Sub SM005Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SM005Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SM005
    SM006Timer.Enabled = True
End Sub


Sub SM006Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SM006Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SM006
    SM007Timer.Enabled = True
End Sub


Sub SM007Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SM007Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SM007
    SM008Timer.Enabled = True
End Sub


Sub SM008Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SM008Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SM008
    SM009Timer.Enabled = True
End Sub


Sub SM009Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SM009Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SM010Timer.Enabled = True
End Sub


Sub SM010Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SM010Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreenBlank.visible = False
    MovieScreen36Animation.Enabled = True
End Sub


Sub SM011Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SM011Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    MovieScreen35Animation.Enabled = True
End Sub



Sub AmberKillTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    AmberKillTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    KillAllMovies
    AmberKill2Timer.Enabled = True 
End Sub


Sub AmberKill2Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    AmberKill2Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    KillAllMovies
    AmberKill3Timer.Enabled = True 
End Sub


Sub AmberKill3Timer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    AmberKill3Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    KillAllMovies
End Sub


Sub KillAllMovies
    Amber001Timer.Enabled = False
    Amber002Timer.Enabled = False
    Amber003Timer.Enabled = False
    Amber004Timer.Enabled = False
    Amber005Timer.Enabled = False
    Amber006Timer.Enabled = False
    Amber007Timer.Enabled = False
    Amber008Timer.Enabled = False
    Amber009Timer.Enabled = False
    Amber010Timer.Enabled = False
    Amber011Timer.Enabled = False
    Billy001Timer.Enabled = False
    Billy002Timer.Enabled = False
    Billy003Timer.Enabled = False
    Billy004Timer.Enabled = False
    Billy005Timer.Enabled = False
    Billy006Timer.Enabled = False
    Billy007Timer.Enabled = False
    Billy008Timer.Enabled = False
    Billy009Timer.Enabled = False
    Billy010Timer.Enabled = False
    Billy011Timer.Enabled = False
    Stu001Timer.Enabled = False
    Stu002Timer.Enabled = False
    Stu003Timer.Enabled = False
    Stu004Timer.Enabled = False
    Stu005Timer.Enabled = False
    Stu006Timer.Enabled = False
    Stu007Timer.Enabled = False
    Stu008Timer.Enabled = False
    Stu009Timer.Enabled = False
    Stu010Timer.Enabled = False
    Stu011Timer.Enabled = False
    MovieScreen.visible = False
    Jill001Timer.Enabled = False
    Jill002Timer.Enabled = False
    Jill003Timer.Enabled = False
    Jill004Timer.Enabled = False
    Jill005Timer.Enabled = False
    Jill006Timer.Enabled = False
    Jill007Timer.Enabled = False
    Jill008Timer.Enabled = False
    Jill009Timer.Enabled = False
    Jill010Timer.Enabled = False
    Jill011Timer.Enabled = False
    Quinn001Timer.Enabled = False
    Quinn002Timer.Enabled = False
    Quinn003Timer.Enabled = False
    Quinn004Timer.Enabled = False
    Quinn005Timer.Enabled = False
    Quinn006Timer.Enabled = False
    Quinn007Timer.Enabled = False
    Quinn008Timer.Enabled = False
    Quinn009Timer.Enabled = False
    Quinn010Timer.Enabled = False
    Quinn011Timer.Enabled = False
    Mickey001Timer.Enabled = False
    Mickey002Timer.Enabled = False
    Mickey003Timer.Enabled = False
    Mickey004Timer.Enabled = False
    Mickey005Timer.Enabled = False
    Mickey006Timer.Enabled = False
    Mickey007Timer.Enabled = False
    Mickey008Timer.Enabled = False
    Mickey009Timer.Enabled = False
    Mickey010Timer.Enabled = False
    Mickey011Timer.Enabled = False
    Mrsloomis001Timer.Enabled = False
    Mrsloomis002Timer.Enabled = False
    Mrsloomis003Timer.Enabled = False
    Mrsloomis004Timer.Enabled = False
    Mrsloomis005Timer.Enabled = False
    Mrsloomis006Timer.Enabled = False
    Mrsloomis007Timer.Enabled = False
    Mrsloomis008Timer.Enabled = False
    Mrsloomis009Timer.Enabled = False
    Mrsloomis010Timer.Enabled = False
    Mrsloomis011Timer.Enabled = False
    Roman001Timer.Enabled = False
    Roman002Timer.Enabled = False
    Roman003Timer.Enabled = False
    Roman004Timer.Enabled = False
    Roman005Timer.Enabled = False
    Roman006Timer.Enabled = False
    Roman007Timer.Enabled = False
    Roman008Timer.Enabled = False
    Roman009Timer.Enabled = False
    Roman010Timer.Enabled = False
    Roman011Timer.Enabled = False
    Charlie001Timer.Enabled = False
    Charlie002Timer.Enabled = False
    Charlie003Timer.Enabled = False
    Charlie004Timer.Enabled = False
    Charlie005Timer.Enabled = False
    Charlie006Timer.Enabled = False
    Charlie007Timer.Enabled = False
    Charlie008Timer.Enabled = False
    Charlie009Timer.Enabled = False
    Charlie010Timer.Enabled = False
    Charlie011Timer.Enabled = False
    Richie001Timer.Enabled = False
    Richie002Timer.Enabled = False
    Richie003Timer.Enabled = False
    Richie004Timer.Enabled = False
    Richie005Timer.Enabled = False
    Richie006Timer.Enabled = False
    Richie007Timer.Enabled = False
    Richie008Timer.Enabled = False
    Richie009Timer.Enabled = False
    Richie010Timer.Enabled = False
    Richie011Timer.Enabled = False
    Ethan001Timer.Enabled = False
    Ethan002Timer.Enabled = False
    Ethan003Timer.Enabled = False
    Ethan004Timer.Enabled = False
    Ethan005Timer.Enabled = False
    Ethan006Timer.Enabled = False
    Ethan007Timer.Enabled = False
    Ethan008Timer.Enabled = False
    Ethan009Timer.Enabled = False
    Ethan010Timer.Enabled = False
    Ethan011Timer.Enabled = False
    Wayne001Timer.Enabled = False
    Wayne002Timer.Enabled = False
    Wayne003Timer.Enabled = False
    Wayne004Timer.Enabled = False
    Wayne005Timer.Enabled = False
    Wayne006Timer.Enabled = False
    Wayne007Timer.Enabled = False
    Wayne008Timer.Enabled = False
    Wayne009Timer.Enabled = False
    Wayne010Timer.Enabled = False
    Wayne011Timer.Enabled = False
    Dewey001Timer.Enabled = False
    Dewey002Timer.Enabled = False
    Dewey003Timer.Enabled = False
    Dewey004Timer.Enabled = False
    Dewey005Timer.Enabled = False
    Dewey006Timer.Enabled = False
    Dewey007Timer.Enabled = False
    Dewey008Timer.Enabled = False
    Dewey009Timer.Enabled = False
    Dewey010Timer.Enabled = False
    Dewey011Timer.Enabled = False
    Gale001Timer.Enabled = False
    Gale002Timer.Enabled = False
    Gale003Timer.Enabled = False
    Gale004Timer.Enabled = False
    Gale005Timer.Enabled = False
    Gale006Timer.Enabled = False
    Gale007Timer.Enabled = False
    Gale008Timer.Enabled = False
    Gale009Timer.Enabled = False
    Gale010Timer.Enabled = False
    Gale011Timer.Enabled = False
    Tara001Timer.Enabled = False
    Tara002Timer.Enabled = False
    Tara003Timer.Enabled = False
    Tara004Timer.Enabled = False
    Tara005Timer.Enabled = False
    Tara006Timer.Enabled = False
    Tara007Timer.Enabled = False
    Tara008Timer.Enabled = False
    Tara009Timer.Enabled = False
    Tara010Timer.Enabled = False
    Tara011Timer.Enabled = False
    Sam001Timer.Enabled = False
    Sam002Timer.Enabled = False
    Sam003Timer.Enabled = False
    Sam004Timer.Enabled = False
    Sam005Timer.Enabled = False
    Sam006Timer.Enabled = False
    Sam007Timer.Enabled = False
    Sam008Timer.Enabled = False
    Sam009Timer.Enabled = False
    Sam010Timer.Enabled = False
    Sam011Timer.Enabled = False
    Sid001Timer.Enabled = False
    Sid002Timer.Enabled = False
    Sid003Timer.Enabled = False
    Sid004Timer.Enabled = False
    Sid005Timer.Enabled = False
    Sid006Timer.Enabled = False
    Sid007Timer.Enabled = False
    Sid008Timer.Enabled = False
    Sid009Timer.Enabled = False
    Sid010Timer.Enabled = False
    Sid011Timer.Enabled = False
    SM001Timer.Enabled = False
    SM002Timer.Enabled = False
    SM003Timer.Enabled = False
    SM004Timer.Enabled = False
    SM005Timer.Enabled = False
    SM006Timer.Enabled = False
    SM007Timer.Enabled = False
    SM008Timer.Enabled = False
    SM009Timer.Enabled = False
    SM010Timer.Enabled = False
    SM011Timer.Enabled = False
    MovieScreen.visible = False
    MovieScreenBlank.visible = True
    MovieScreen1Animation.Enabled = False
    MovieScreen2Animation.Enabled = False
    MovieScreen3Animation.Enabled = False
    MovieScreen4Animation.Enabled = False
    MovieScreen5Animation.Enabled = False
    MovieScreen6Animation.Enabled = False
    MovieScreen7Animation.Enabled = False
    MovieScreen8Animation.Enabled = False
    MovieScreen9Animation.Enabled = False
    MovieScreen10Animation.Enabled = False
    MovieScreen11Animation.Enabled = False
    MovieScreen12Animation.Enabled = False
    MovieScreen13Animation.Enabled = False
    MovieScreen14Animation.Enabled = False
    MovieScreen15Animation.Enabled = False
    MovieScreen16Animation.Enabled = False
    MovieScreen17Animation.Enabled = False
    MovieScreen18Animation.Enabled = False
    MovieScreen19Animation.Enabled = False
    MovieScreen20Animation.Enabled = False
    MovieScreen21Animation.Enabled = False
    MovieScreen22Animation.Enabled = False
    MovieScreen23Animation.Enabled = False
    MovieScreen24Animation.Enabled = False
    MovieScreen25Animation.Enabled = False
    MovieScreen26Animation.Enabled = False
    MovieScreen27Animation.Enabled = False
    MovieScreen28Animation.Enabled = False
    MovieScreen29Animation.Enabled = False
    MovieScreen30Animation.Enabled = False
    MovieScreen31Animation.Enabled = False
    MovieScreen32Animation.Enabled = False
    MovieScreen33Animation.Enabled = False
    MovieScreen34Animation.Enabled = False
    MovieScreen35Animation.Enabled = False
    MovieScreen36Animation.Enabled = False
    MovieScreenBlank.imageA = ""
End Sub




Sub Poster001
    MovieScreenBlank.imageA = "Scream001"
End Sub



'***************************************
' End of Main Mode Kickers and Catchers 
'***************************************

Sub EndOfModeKicker001_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker001.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker002_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker002.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker003_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker003.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker004_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker004.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker005_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker005.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker006_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker006.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker007_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker007.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker008_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker008.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker009_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker009.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker010_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker010.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker011_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker011.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker012_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker012.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker013_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker013.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker014_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker014.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker015_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker015.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker016_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker016.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker017_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker017.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfModeKicker018_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfModeKicker018.DestroyBall:EndOfModeTarget001.Isdropped = 0:EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub MBDrainKicker_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    MBDrainKicker.DestroyBall:EndOfModeKicker014.enabled = 0:EndOfModeKicker017.enabled = 0
    MBDrainOutKicker.CreateBall:MBDrainOutKicker.Kick 100,9:PlaySound ""
    MBDrainOutKicker.Kick 100, a
End Sub


Sub MBDrainTrigger_hit
    MBDrainKicker.enabled = 1
    EndOfModeKicker014.enabled = 0
    EndOfModeKicker017.enabled = 0
    EnableBallSaver 1
End Sub


'MBDrainTrigger.enabled = True


Sub EndOfModeClosed
    EndOfModeTarget001.Isdropped = 1
    EndOfModeTarget002.Isdropped = 1
    EndOfModeKicker001.enabled = 0
    EndOfModeKicker002.enabled = 0
    EndOfModeKicker003.enabled = 0
    EndOfModeKicker004.enabled = 0
    EndOfModeKicker005.enabled = 0
    EndOfModeKicker006.enabled = 0
    EndOfModeKicker007.enabled = 0
    EndOfModeKicker008.enabled = 0
    EndOfModeKicker009.enabled = 0
    EndOfModeKicker010.enabled = 0
    EndOfModeKicker011.enabled = 0
    EndOfModeKicker012.enabled = 0
    EndOfModeKicker013.enabled = 0
    EndOfModeKicker014.enabled = 0
    EndOfModeKicker015.enabled = 0
    EndOfModeKicker016.enabled = 0
    EndOfModeKicker017.enabled = 0
    EndOfModeKicker018.enabled = 0
    MBDrainTrigger.enabled = False
    MBDrainKicker.enabled = 0
End Sub


Sub MainModeEndTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    MainModeEndTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    EndOfModeClosed
    EndOfModeColorChange
    ModeLitLightItUp
    pupBGmusicrestart
End Sub




Sub EndOfModeColorChange
    If BallsOnPlayfield = 1 Then
    ColorChangeBack
    End If
End Sub



Sub EndOfModeKickersActive
    if BallsOnPlayfield =3 Then
    EndOfModeKicker001.enabled = 0
    EndOfModeKicker002.enabled = 0
    EndOfModeKicker003.enabled = 0
    EndOfModeKicker004.enabled = 0
    EndOfModeKicker005.enabled = 0
    EndOfModeKicker006.enabled = 0
    EndOfModeKicker007.enabled = 0
    EndOfModeKicker008.enabled = 0
    EndOfModeKicker009.enabled = 0
    EndOfModeKicker010.enabled = 0
    EndOfModeKicker011.enabled = 0
    EndOfModeKicker012.enabled = 0
    EndOfModeKicker013.enabled = 0
    EndOfModeKicker014.enabled = 0
    EndOfModeKicker015.enabled = 0
    EndOfModeKicker016.enabled = 0
    EndOfModeKicker017.enabled = 0
    EndOfModeKicker018.enabled = 0
    End If
    if BallsOnPlayfield =2 Then
    EndOfModeKicker001.enabled = 0
    EndOfModeKicker002.enabled = 0
    EndOfModeKicker003.enabled = 0
    EndOfModeKicker004.enabled = 0
    EndOfModeKicker005.enabled = 0
    EndOfModeKicker006.enabled = 0
    EndOfModeKicker007.enabled = 0
    EndOfModeKicker008.enabled = 0
    EndOfModeKicker009.enabled = 0
    EndOfModeKicker010.enabled = 0
    EndOfModeKicker011.enabled = 0
    EndOfModeKicker012.enabled = 0
    EndOfModeKicker013.enabled = 0
    EndOfModeKicker014.enabled = 0
    EndOfModeKicker015.enabled = 0
    EndOfModeKicker016.enabled = 0
    EndOfModeKicker017.enabled = 0
    EndOfModeKicker018.enabled = 0
    End If
    if BallsOnPlayfield =1 Then
    EndOfModeKicker001.enabled = 1
    EndOfModeKicker002.enabled = 1
    EndOfModeKicker003.enabled = 1
    EndOfModeKicker004.enabled = 1
    EndOfModeKicker005.enabled = 1
    EndOfModeKicker006.enabled = 1
    EndOfModeKicker007.enabled = 1
    EndOfModeKicker008.enabled = 1
    EndOfModeKicker009.enabled = 1
    EndOfModeKicker010.enabled = 1
    EndOfModeKicker011.enabled = 1
    EndOfModeKicker012.enabled = 1
    EndOfModeKicker013.enabled = 1
    EndOfModeKicker014.enabled = 1
    EndOfModeKicker015.enabled = 1
    EndOfModeKicker016.enabled = 1
    EndOfModeKicker017.enabled = 1
    EndOfModeKicker018.enabled = 1
    End If
End Sub



'******************************
' Secondary Mode Triggers
'******************************

Sub ModeTwo000_hit
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F5, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    'weapon hit
    If WeaponHits(CurrentPlayer, 1) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 1) = 0
        CheckWeapons
    End If
    'teen killed
    If F001.State Then TeenKilled:F001.State = 0
End Sub

'ModeTwo000.enabled = True

Sub ModeTwo001_hit
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    If LastSwitchHit = "Trigger017" Then
        AwardCombo
    Else
        ComboCount = 1
    End If
    If F004.State Then TeenKilled:F004.State = 0
    If li069.State Then AwardTargetJackpot
    'weapon hit
    If WeaponHits(CurrentPlayer, 2) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 2) = 0
        CheckWeapons
    End If
    If bPoliceStarted Then 'the police hurry up is started
        PoliceRampHits = PoliceRampHits + 1
        If PoliceRampHits = 3 Then
            AwardPoliceJackpot
            StopPolice
     ' remember last trigger hit by the ball
    LastSwitchHit = "ModeTwo001"
        End If
    End If
End Sub

'ModeTwo001.enabled = True

Sub ModeTwo002_hit
    PLaySoundAtBall SoundFXDOF("fx_Target",107,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    'weapon hit
    If WeaponHits(CurrentPlayer, 3) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 3) = 0
        CheckWeapons
    End If
    'extra ball
    If li070.State Then AwardExtraBall:li070.State = 0
    TargetHits(CurrentPlayer, 1) = 0
    CheckTargets
End Sub

'ModeTwo002.enabled = True


Sub ModeTwo003_hit
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    If ActiveBall.VelY > 10 Then
        ActiveBall.VelY = 10
    End If
    mMagnet.MagnetOn = True
    DOF 112, DOFOn
    Me.TimerEnabled = 1 'to turn off the Magnet
    'Jason multiball
    If bJasonMBStarted Then
        If ActiveBall.VelY < 0 Then 'this means the ball going up
            DMDFlush
            DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(3) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
            LightEffect 2
            GiEffect 2
            Addscore2 ArrowMultiPlier(3) * 1000000
            If ArrowMultiPlier(3) < 5 Then
                ArrowMultiPlier(3) = ArrowMultiPlier(3) + 1
                UpdateArrowLights
            End If
        End If
    End If
End Sub


Sub ModeTwo003_Timer
    Me.TimerEnabled = 0
    ReleaseMagnetBalls
End Sub

'ModeTwo003.enabled = True

Sub ModeTwo004_hit
    If Tilted OR ActiveBall.VelY > 0 Then Exit Sub
    Addscore 5000
    If aWeaponSJactive Then     'the lit Super Jackpot light is lit, so lit the Super Jackpot Light at the right loop
        SuperJackpotTimer_Timer 'call the timer to stop the 30s timer and blinking light at the cabin
        li060.State = 2
    End If
End Sub

'ModeTwo004.enabled = True

Sub ModeTwo005_hit
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F3, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    If LastSwitchHit = "ModeTwo009" Then
        AwardLoop
    Else
        li061.State = 2 'super loops light
        LoopCount = 1
    End If
    If F005.State Then TeenKilled:F005.State = 0
    'weapon hit
    If WeaponHits(CurrentPlayer, 4) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 4) = 0
        CheckWeapons
            ' remember last trigger hit by the ball
    LastSwitchHit = "ModeTwo005"
    End If
End Sub

'ModeTwo005.enabled = True

Sub ModeTwo006_hit
    If Tilted Then Exit Sub
    Addscore 5000
    If LastSwitchHit = "Trigger017" Then
        AwardCombo
    Else
        ComboCount = 1
    End If
    If F003.State Then TeenKilled:F003.State = 0
    'weapon hit
    If WeaponHits(CurrentPlayer, 5) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 5) = 0
        CheckWeapons
    End If
    If bTommyStarted Then 'the Tommy Jarvis hurry up is started so award the jackpot
        AwardTommyJackpot
        StopTommyJarvis
    End If
    If bPoliceStarted Then 'the police hurry up is started
        PoliceRampHits = PoliceRampHits + 1
        If PoliceRampHits = 3 Then
            AwardPoliceJackpot
            StopPolice
        End If
    End If
        ' remember last trigger hit by the ball
    LastSwitchHit = "ModeTwo006"
    End Sub
'End Sub

'ModeTwo006.enabled = True

Sub ModeTwo007_hit
    PLaySoundAt "fx_sensor", Trigger009
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    PlayThunder
    Addscore 10000
    Flashforms f2A, 800, 50, 0
    Flashforms F2B, 800, 50, 0
    If LastSwitchHit = "ModeTwo009" Then
        AwardLoop
    Else
        li061.State = 2 'super loops light
        LoopCount = 1
    End If
    If F002.State Then TeenKilled:F002.State = 0
    ' Weapons Super Jackpot
    If aWeaponSJactive AND li060.State Then AwardWeaponsSuperJackpot
    ' remember last trigger hit by the ball
    LastSwitchHit = "ModeTwo007"
    End Sub

'ModeTwo007.enabled = True

Sub ModeTwo008_hit
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F4, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    'weapon Hit
    If WeaponHits(CurrentPlayer, 6) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 6) = 0
        CheckWeapons
    End If
    If li077.State Then 'give the special, which in this table is an add-a-ball
        PlaySound "vo_special"
        AwardExtraBall
        Addscore 150000
        li077.State = 0
    End If
End Sub

'ModeTwo008.enabled = True


Sub ModeTwo009_hit
    'end top loop
    PLaySoundAt "fx_sensor", Trigger008
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    ' remember last trigger hit by the ball
    LastSwitchHit = "ModeTwo009"
    'End If
End Sub
    
'ModeTwo009.enabled = True

'*****************************

Sub ModeThree000_hit
    If li062.State = 2 Then
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F5, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    'weapon hit
    If WeaponHits(CurrentPlayer, 1) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 1) = 0
        CheckWeapons
    End If
    'teen killed
    If F001.State Then TeenKilled:F001.State = 0
    'Jason multiball
    If bJasonMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(1) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(1) * 1000000
        If ArrowMultiPlier(1) < 5 Then
            ArrowMultiPlier(1) = ArrowMultiPlier(1) + 1
            UpdateArrowLights
        End If
    End If
    'Michael multiball
    If bMichaelMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        Addscore2 1000000
        MichaelSJValue = MichaelSJValue + 2000000
        If MichaelSJValue >= 5000000 Then
            li032.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
                Case Else:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
            End Select
            PlaySound "vo_getthesuperjackpot"
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li062.State Then
                TargetModeHits = TargetModeHits + 1
                li062.State = 0
                CheckWinMode
            End If
        Case 5, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
End If
End Sub


'ModeThree000.enabled = True

Sub ModeThree001_hit
    If li064.State = 2 Then
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    If LastSwitchHit = "Trigger017" Then
        AwardCombo
    Else
        ComboCount = 1
    End If
    If F004.State Then TeenKilled:F004.State = 0
    If li069.State Then AwardTargetJackpot
    'weapon hit
    If WeaponHits(CurrentPlayer, 2) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 2) = 0
        CheckWeapons
    End If
    If bPoliceStarted Then 'the police hurry up is started
        PoliceRampHits = PoliceRampHits + 1
        If PoliceRampHits = 3 Then
            AwardPoliceJackpot
            StopPolice
        End If
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(2) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(2) * 1000000
        If ArrowMultiPlier(2) < 5 Then
            ArrowMultiPlier(2) = ArrowMultiPlier(2) + 1
            UpdateArrowLights
        End If
    End If
    'Freddy multiball
    If bFreddyMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        Addscore2 1000000
        FreddySJValue = FreddySJValue + 2000000
        If FreddySJValue >= 5000000 Then
            li060.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, ""
                Case Else:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, ""
            End Select
            PlaySound "vo_getthesuperjackpot"
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li064.State Then
                TargetModeHits = TargetModeHits + 1
                li064.State = 0
                CheckWinMode
            End If
        Case 5, 6, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "ModeThree001"
End If
End Sub

'ModeThree001.enabled = True

Sub ModeThree002_hit
    If li065.State = 2 Then
    PLaySoundAtBall SoundFXDOF("fx_Target",107,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    'weapon hit
    If WeaponHits(CurrentPlayer, 3) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 3) = 0
        CheckWeapons
    End If
    'extra ball
    If li070.State Then AwardExtraBall:li070.State = 0
    Select Case NewMode
        Case 2
            If li065.State Then
                WinMode
            End If
        Case 9
            If li065.State Then
                TargetModeHits = TargetModeHits + 1
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 1) = 0
            CheckTargets
    End Select
End If
End Sub

'ModeThree002.enabled = True


Sub ModeThree003_hit
    If li053.State = 2 Then
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    If ActiveBall.VelY > 10 Then
        ActiveBall.VelY = 10
    End If
    mMagnet.MagnetOn = True
    DOF 112, DOFOn
    Me.TimerEnabled = 1 'to turn off the Magnet
    'Jason multiball
    If bJasonMBStarted Then
        If ActiveBall.VelY < 0 Then 'this means the ball going up
            DMDFlush
            DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(3) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
            LightEffect 2
            GiEffect 2
            Addscore2 ArrowMultiPlier(3) * 1000000
            If ArrowMultiPlier(3) < 5 Then
                ArrowMultiPlier(3) = ArrowMultiPlier(3) + 1
                UpdateArrowLights
            End If
        End If
    End If
    'Modes
    If ActiveBall.VelY < 0 Then 'this means the ball going up
        Select Case NewMode
            Case 5, 13, 14
                TargetModeHits = TargetModeHits + 1
                CheckWinMode
            Case 15
                AwardJackpot
        End Select
    End If
End If
End Sub


Sub ModeThree003_Timer
    Me.TimerEnabled = 0
    ReleaseMagnetBalls
End Sub

'ModeThree003.enabled = True

Sub ModeThree004_hit
    Select Case NewMode
        Case 3
            If li066.State Then
                TargetModeHits = TargetModeHits + 1
                li066.State = 0
                CheckWinMode
            End If
        Case 5, 13, 14
            If li066.State Then
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
            End If
            If ReadyToKill Then 'kill her :)
                WinMode
            End If
        Case 15
            AwardJackpot
    End Select
End Sub

'ModeThree004.enabled = True

Sub ModeThree005_hit
    If li067.State = 2 Then
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F3, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    If LastSwitchHit = "Trigger008" Then
        AwardLoop
    Else
        li061.State = 2 'super loops light
        LoopCount = 1
    End If
    If F005.State Then TeenKilled:F005.State = 0
    'weapon hit
    If WeaponHits(CurrentPlayer, 4) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 4) = 0
        CheckWeapons
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(5) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(5) * 1000000
        If ArrowMultiPlier(5) < 5 Then
            ArrowMultiPlier(5) = ArrowMultiPlier(5) + 1
            UpdateArrowLights
        End If
    End If
    'Michael multiball
    If bMichaelMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        Addscore2 1000000
        MichaelSJValue = MichaelSJValue + 2000000
        If MichaelSJValue >= 5000000 Then
            li032.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
                Case Else:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
            End Select
            PlaySound "vo_getthesuperjackpot"
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li067.State Then
                TargetModeHits = TargetModeHits + 1
                li067.State = 0
                CheckWinMode
            End If
        Case 5, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "ModeThree005"
End If
End Sub

'ModeThree005.enabled = True

Sub ModeThree006_hit
    If li068.State = 2 Then
    If Tilted Then Exit Sub
    Addscore 5000
    If LastSwitchHit = "Trigger017" Then
        AwardCombo
    Else
        ComboCount = 1
    End If
    If F003.State Then TeenKilled:F003.State = 0
    'weapon hit
    If WeaponHits(CurrentPlayer, 5) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 5) = 0
        CheckWeapons
    End If
    If bTommyStarted Then 'the Tommy Jarvis hurry up is started so award the jackpot
        AwardTommyJackpot
        StopTommyJarvis
    End If
    If bPoliceStarted Then 'the police hurry up is started
        PoliceRampHits = PoliceRampHits + 1
        If PoliceRampHits = 3 Then
            AwardPoliceJackpot
            StopPolice
        End If
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(6) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(6) * 1000000
        If ArrowMultiPlier(6) < 5 Then
            ArrowMultiPlier(6) = ArrowMultiPlier(6) + 1
            UpdateArrowLights
        End If
    End If
    'Freddy multiball
    If bFreddyMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        Addscore2 1000000
        FreddySJValue = FreddySJValue + 2000000
        If FreddySJValue >= 5000000 Then
            li060.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, ""
                Case Else:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, ""
            End Select
            PlaySound "vo_getthesuperjackpot"
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li068.State Then
                TargetModeHits = TargetModeHits + 1
                li068.State = 0
                CheckWinMode
            End If
        Case 5, 6, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "ModeThree006"
End If
End Sub

'ModeThree006.enabled = True

Sub ModeThree007_hit
     If li052.State = 2 Then
    PLaySoundAt "fx_sensor", Trigger009
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    PlayThunder
    Addscore 10000
    Flashforms f2A, 800, 50, 0
    Flashforms F2B, 800, 50, 0
    If LastSwitchHit = "Trigger008" Then
        AwardLoop
    Else
        li061.State = 2 'super loops light
        LoopCount = 1
    End If
    If F002.State Then TeenKilled:F002.State = 0
    ' Weapons Super Jackpot
    If aWeaponSJactive AND li060.State Then AwardWeaponsSuperJackpot
    'Jason multiball
    If bJasonMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(7) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(7) * 1000000
        If ArrowMultiPlier(7) < 5 Then
            ArrowMultiPlier(7) = ArrowMultiPlier(7) + 1
            UpdateArrowLights
        End If
    End If
    'Freddy multiball
    If bFreddyMBStarted and li060.State Then
        DOF 126, DOFPulse
        DMDFlush
        DMD CL("SUPER JACKPOT"), CL(FormatScore(FreddySJValue) ), "_", eBlink, eNone, eNone, 1000, True, "vo_superjackpot"
        Addscore2 FreddySJValue
        FreddySJValue = 1000000
        li060.State = 0
        LightEffect 2
        GiEffect 2
    End If
    'Modes
    Select Case NewMode
        Case 5, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "ModeThree007"
End If
End Sub


'ModeThree007.enabled = True

Sub ModeThree008_hit
    If li059.State = 2 Then
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F4, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    'weapon Hit
    If WeaponHits(CurrentPlayer, 6) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 6) = 0
        CheckWeapons
    End If
    If li077.State Then 'give the special, which in this table is an add-a-ball
        PlaySound "vo_special"
        'AddMultiball 1
        AwardExtraBall
        Addscore 150000
        li077.State = 0
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(8) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(8) * 1000000
        If ArrowMultiPlier(8) < 5 Then
            ArrowMultiPlier(8) = ArrowMultiPlier(8) + 1
            UpdateArrowLights
        End If
    End If
    'Michael multiball
    If bMichaelMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        Addscore2 1000000
        MichaelSJValue = MichaelSJValue + 2000000
        If MichaelSJValue >= 5000000 Then
            li032.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
                Case Else:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, ""
            End Select
            PlaySound "vo_getthesuperjackpot"
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select case NewMode
        Case 5, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
End If
End Sub

'ModeThree008.enabled = True


Sub ModeTwoAllOn
    ModeTwo000.enabled = True
    ModeTwo001.enabled = True
    ModeTwo002.enabled = True
    ModeTwo003.enabled = True
    ModeTwo004.enabled = True
    ModeTwo005.enabled = True
    ModeTwo006.enabled = True
    ModeTwo007.enabled = True
    ModeTwo008.enabled = True
    ModeTwo009.enabled = True
End Sub


Sub ModeTwoAllOff
    ModeTwo000.enabled = False
    ModeTwo001.enabled = False
    ModeTwo002.enabled = False
    ModeTwo003.enabled = False
    ModeTwo004.enabled = False
    ModeTwo005.enabled = False
    ModeTwo006.enabled = False
    ModeTwo007.enabled = False
    ModeTwo008.enabled = False
    ModeTwo009.enabled = False
End Sub


Sub ModeThreeAllOn
    ModeThree000.enabled = True
    ModeThree001.enabled = True
    ModeThree002.enabled = True
    ModeThree003.enabled = True
    ModeThree004.enabled = True
    ModeThree005.enabled = True
    ModeThree006.enabled = True
    ModeThree008.enabled = True
    ModeThree007.enabled = True
    Trigger007.enabled =False
    Trigger012.enabled =False
    Trigger013.enabled =False
    Trigger009.enabled =False
    Trigger014.enabled =False
    Trigger015.enabled =False
    Trigger016.enabled =False
    Trigger008.enabled =False
    Trigger011.enabled =False
    SidMBTrigger.enabled =False
    Captrigger.enabled =False
End Sub


Sub ModeThreeAllOff
    ModeThree000.enabled = False
    ModeThree001.enabled = False
    ModeThree002.enabled = False
    ModeThree003.enabled = False
    ModeThree004.enabled = False
    ModeThree005.enabled = False
    ModeThree006.enabled = False
    ModeThree008.enabled = False
    ModeThree007.enabled = False
    Trigger007.enabled =True
    Trigger012.enabled =True
    Trigger013.enabled =True
    Trigger009.enabled =True
    Trigger014.enabled =True
    Trigger015.enabled =True
    Trigger016.enabled =True
    Trigger008.enabled =True
    Trigger011.enabled =True
    Captrigger.enabled =True
End Sub


Sub KillModeThree
    ModeThree000.enabled = False
    ModeThree001.enabled = False
    ModeThree002.enabled = False
    ModeThree003.enabled = False
    ModeThree004.enabled = False
    ModeThree005.enabled = False
    ModeThree006.enabled = False
    ModeThree008.enabled = False
    ModeThree007.enabled = False
End Sub


Sub ModeThreekillTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    ModeThreekillTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    ModeThreeAllOff
End Sub

'******************************
' Ghostface Glowing Eyes
'******************************

Sub BigGFEyesGlowingOn
    BigGlowingLEye.State = 2
    BigGlowingREye.State = 2
    APFadeRedLight.State = 2
End Sub



Sub GFEyesGlowingOFF
    BigGlowingLEye.State = 0
    BigGlowingREye.State = 0
    APFadeRedLight.State = 0
End Sub


Sub BigGFEyesGlowingPinkOn
    BigGlowingLEyePink.State = 2
    BigGlowingREyePink.State = 2
    APFadePinkLight.State = 2
End Sub



Sub GFEyesGlowingPinkOFF
    BigGlowingLEyePink.State = 0
    BigGlowingREyePink.State = 0
    APFadePinkLight.State = 0
End Sub        


Sub LittleGFEyesGlowingOn
    GlowingRedLeftEye.State = 2
    GlowingRedREye.State = 2
    BigGlowingLEyeGreen.State = 2
    BigGlowingREyeGreen.State = 2
End Sub



Sub LittleGFEyesGlowingOFF
    GlowingRedLeftEye.State = 0
    GlowingRedREye.State = 0
    BigGlowingLEyeGreen.State = 0
    BigGlowingREyeGreen.State = 0
End Sub



'******************************
' MultiBall Letter Lights
'****************************** 

Sub MBLettersOn
    MBLight001.State = 2
    MBLight002.State = 2
    MBLight003.State = 2
    MBLight004.State = 2
    MBLight005.State = 2
    MBLight006.State = 2
    MBLight007.State = 2
    MBLight008.State = 2
    MBLight009.State = 2
    MBLight010.State = 2
End Sub


Sub MBLettersOff
    MBLight001.State = 0
    MBLight002.State = 0
    MBLight003.State = 0
    MBLight004.State = 0
    MBLight005.State = 0
    MBLight006.State = 0
    MBLight007.State = 0
    MBLight008.State = 0
    MBLight009.State = 0
    MBLight010.State = 0
End Sub



'**************************************************
' Light for the GF Voice changer on the apron
'**************************************************
Sub VOLight
    Flashforms voRlight, 1000, 80, 0
    vpmtimer.addtimer 1500, "VOLight2'"
End Sub


Sub VOLight2
    Flashforms voRlight, 1000, 80, 0
End Sub
    
       



'******************************
' Specialty Drain Functions
'******************************

Sub GFDrain
    ghostfaceBody.Rotz = 0
    ghostfaceMask.Rotz = 0
    GF001.Roty = -40
    GF002.Roty = -40
    GF003.Roty = 0
    GF004.Roty = 30
    GF005.Roty = 40
    SidMBTrigger.enabled =True
    BigCellRingTrigger.enabled =False
    BigCellRingOffTrigger.enabled =False
    SmallFlimReelTimer.enabled =False
    BigFlimReelTimer.enabled =False
    FilmReelLight.State = 0
    ModeLitTrigger.enabled =True
    Trigger017.enabled =True
    AmberKillTimer.Enabled = True
    GFModeTarget.Isdropped = 1
    GF006.Z =-90
    MickeyTimer.Enabled = False
    MBlocklight.State = 0
    SidMBTrigger.enabled =True
    RestartMBKillSwitch
    KillModeThree
    LittleGFEyesGlowingOFF
    GFEyesGlowingOFF
    GFEyesGlowingPinkOFF
    KillAllMovies
    ModeRingTriggersKill
    AllDropped
    KillSwitchRestart
    pupdrain
    Topperbase
    Fulldmdbase
    bgMusicMute
    pupBGMusic
    shutthedoorsdrain
    CharlieLnTOff
    mickeyNromanOff
    charlietarget5.Enabled = False
    charlietarget6.Enabled = False
    charlielight5.State = 0
    charlielight6.State = 0
    charlielight05.State = 0
    charlielight06.State = 0
 End Sub


Sub GFTiltDrain
    If Tilted Then
    ghostfaceBody.Rotz = 0
    ghostfaceMask.Rotz = 0
    GF001.Roty = -40
    GF002.Roty = -40
    GF003.Roty = 0
    GF004.Roty = 30
    GF005.Roty = 40
    SidMBTrigger.enabled =True
    BigCellRingTrigger.enabled =False
    BigCellRingOffTrigger.enabled =False
    SmallFlimReelTimer.enabled =False
    BigFlimReelTimer.enabled =False
    FilmReelLight.State = 0
    ModeLitTrigger.enabled =True
    Trigger017.enabled =True
    AmberKillTimer.Enabled = True
    GFModeTarget.Isdropped = 1
    GFModeTarget001.Isdropped = 1
    GF006.Z =-90
    MickeyTimer.Enabled = False
    MBlocklight.State = 0
    SidMBTrigger.enabled =True
    RestartMBKillSwitch
    KillModeThree
    LittleGFEyesGlowingOFF
    GFEyesGlowingOFF
    GFEyesGlowingPinkOFF
    KillAllMovies
    ModeRingTriggersKill
    AllDropped
    KillSwitchRestart
    pupdrain
    Topperbase
    Fulldmdbase
    bgMusicMute
    pupBGMusic
    shutthedoorsdrain
    CharlieLnTOff
    mickeyNromanOff
    WIZB001. isdropped = 1
    WIZB002. isdropped = 1
    WIZB003. isdropped = 1
    WIZB004. isdropped = 1
    WIZB005. isdropped = 1
    WIZB006. isdropped = 1
    charlietarget5.Enabled = False
    charlietarget6.Enabled = False
    charlielight5.State = 0
    charlielight6.State = 0
    charlielight05.State = 0
    charlielight06.State = 0
    End If
 End Sub


'******************************
' Specialty Start Up Functions
'******************************

Sub SpecialTableStartUps
    pupintro
    Fulldmdbase
    Topperintro
    AllDropped
    WIZB001. isdropped = 1
    WIZB002. isdropped = 1
    WIZB003. isdropped = 1
    WIZB004. isdropped = 1
    WIZB005. isdropped = 1
    WIZB006. isdropped = 1
    SidMBTrigger.enabled =True
    BigFlimReelTimer.enabled =True
    SmallFlimReelTimer.enabled =True
    IphoneSwitchBackAttract
    FilmReelLight.State = 2
    BigGlowingLEye.State = 2
    BigGlowingREye.State = 2
    GlowingRedLeftEye.State = 2
    GlowingRedREye.State = 2
    WizTarget001.Isdropped = 1
    WizTarget002.Isdropped = 1
    Trigger011.enabled =True
    ModeLitTrigger.enabled =True
    MovieScreenBlank.visible = True
    MovieScreen35Animation.Enabled = True
End Sub


Sub SpecialGameStartUps
    SidMBTrigger.enabled =True
    BigFlimReelTimer.enabled =False
    SmallFlimReelTimer.enabled =False
    IphoneSwitchUpStart
    KillAllMovies
    pupBGMusicStart
    Topperbase
    pupstartup
    FilmReelLight.State = 0
    BigGlowingLEye.State = 0
    BigGlowingREye.State = 0
    GlowingRedLeftEye.State = 0
    GlowingRedREye.State = 0
    ModeLitTrigger.enabled =True
    GFModeTarget.Isdropped = 1
    GFModeTarget001.Isdropped = 1
    GF006.Z =-90
End Sub


Sub SpecialGameRestarts
    SidMBTrigger.enabled =True
    BigFlimReelTimer.enabled =True
    SmallFlimReelTimer.enabled =True
    ModeRingTriggersKill
    IphoneSwitchBackAttract
    Topperintro
    pupintro
    Fulldmdbase
    bgMusicMute
    pupBGMusicgamerestart
    shutthedoorsdrain
    FilmReelLight.State = 2
    BigGlowingLEye.State = 2
    BigGlowingREye.State = 2
    GlowingRedLeftEye.State = 2
    GlowingRedREye.State = 2
    Trigger011.enabled =True
    ModeLitTrigger.enabled =True
    MovieScreenBlank.visible = True
    MovieScreen35Animation.Enabled = True
    MBKillSwitchRestart.enabled =False
    Kicker001.enabled =False
End Sub



'******************************
'' WIZARD MB TIMER
'******************************
Sub WizardModeTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardModeTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    NewMode = 15
    StartMode
    pupmodewizard
    Fulldmdwizard
    Toppermodewizard
    ModeRingTriggersOff
    StopPolice
    StopTommyJarvis
    Killa.enabled =True
    BigCellRingTrigger.enabled =False
    BigIphoneRingTrigger.enabled =False
    WizardOopsKicker.enabled = 1
    WizardOopsKicker001.enabled = 1
    WIZB001. isdropped = 0
    WIZB002. isdropped = 0
    WIZB003. isdropped = 0
    WIZB004. isdropped = 0
    WIZB005. isdropped = 0
    WIZB006. isdropped = 0
End Sub


Sub WizardKillFlipperTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardKillFlipperTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    DisableFlippers True
End Sub



Sub WizardMBpreTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardMBpreTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    WizardMBTimer.Enabled = True
    WizardDrainKicker.Enabled = 1
    Trigger013.enabled =False
    WizTarget001.Isdropped = 1
    WizTarget002.Isdropped = 1
    WIZB001. isdropped = 0
    WIZB002. isdropped = 0
    WIZB003. isdropped = 0
    WIZB004. isdropped = 0
    WIZB005. isdropped = 0
    WIZB006. isdropped = 0
    BigCellRingTrigger.enabled =False
    BigIphoneRingTrigger.enabled =False
    ModeRingTriggersOff
    StopPolice
    DisableFlippers False
    vpmtimer.addtimer 2600, "WizardPreKicker'"
    vpmtimer.addtimer 2650, "wizardprekickeroff'"
    AddMultiball 4
End Sub


Sub WizardMBTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardMBTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    WizardKickersActive
    closedoors
    Gate002.Open = True
    pupmodewizardend
    DisableFlippers True
    WizET.Enabled = True
    WizTarget001.Isdropped = 1
    WizTarget002.Isdropped = 1
    openthedoors.Enabled = False
    WizardEndTimer.Enabled = True
    FlagTimer.Enabled = True
End Sub


Sub WizardEndTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardEndTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    opendoors
    WizardKickersDeactive
    pupBGMusicStart
    AllDropped
    WizardDrainKicker.Enabled = 0
    EndOfModeTarget001.Isdropped = 1
    Gate002.Open = False
    GFDefeatedTimer.Enabled = True
    openthedoors.Enabled = True
    WizardEndTimer001.Enabled = True
    Trigger013.enabled =True
    GFShow2Trigger.enabled =True
    ModeLitTrigger.enabled =True
    Trigger017.enabled =True
End Sub


Sub WizardEndTimer001_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardEndTimer001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    closedoors
    GFEyesGlowingOFF
    GFEyesGlowingPinkOFF
    KillAllMovies
    ModeRingTriggersKill
    EndOfModeColorChange
    WizModeTriggersNLightsOff
    WizKillBakOnTriggersNLights
    Fulldmdbase
    Topperbase
    WizardOopsKicker.enabled = 0
    WizardOopsKicker001.enabled = 0
    WIZB001. isdropped = 1
    WIZB002. isdropped = 1
    WIZB003. isdropped = 1
    WIZB004. isdropped = 1
    WIZB005. isdropped = 1
    WIZB006. isdropped = 1
    DisableFlippers False
End Sub


'******************************************
' End of Wizard Mode Kickers and Catchers 
'******************************************
Sub WizardKickersActive
    EndOfWKicker001.enabled = 1
    EndOfWKicker002.enabled = 1
    EndOfWKicker003.enabled = 1
    EndOfWKicker004.enabled = 1
    EndOfWKicker005.enabled = 1
    EndOfWKicker006.enabled = 1
    EndOfWKicker007.enabled = 1
    EndOfWKicker008.enabled = 1
    EndOfWKicker009.enabled = 1
    EndOfWKicker010.enabled = 1
    EndOfWKicker011.enabled = 1
    EndOfWKicker012.enabled = 1
    EndOfWKicker013.enabled = 1
    EndOfWKicker014.enabled = 1
    EndOfWKicker015.enabled = 1
    EndOfWKicker016.enabled = 1
    EndOfWKicker017.enabled = 1
    EndOfWKicker018.enabled = 1
End Sub


Sub WizardKickersDeactive
    EndOfWKicker001.enabled = 0
    EndOfWKicker002.enabled = 0
    EndOfWKicker003.enabled = 0
    EndOfWKicker004.enabled = 0
    EndOfWKicker005.enabled = 0
    EndOfWKicker006.enabled = 0
    EndOfWKicker007.enabled = 0
    EndOfWKicker008.enabled = 0
    EndOfWKicker009.enabled = 0
    EndOfWKicker010.enabled = 0
    EndOfWKicker011.enabled = 0
    EndOfWKicker012.enabled = 0
    EndOfWKicker013.enabled = 0
    EndOfWKicker014.enabled = 0
    EndOfWKicker015.enabled = 0
    EndOfWKicker016.enabled = 0
    EndOfWKicker017.enabled = 0
    EndOfWKicker018.enabled = 0
End Sub


'Sub wizprekicker_hit
  '  wizprekicker.enabled = 1
'End Sub


Sub wizardprekickeron
    wizprekicker.enabled = 1
End Sub

Sub wizardprekickeroff
    wizprekicker.enabled = 0
End Sub


Sub WizardPreKicker
    wizprekicker.Kick 3,15
End Sub

'vpmtimer.addtimer 500, "WizardPreKicker'":wizprekicker.enabled = 0



'******WizKill Triggers and Lights******
Sub WizKillTriggersNLights
    Trigger012.Enabled = False
    li062.State = 0
    Trigger016.Enabled = False
    li064.State = 0
    Trigger007.Enabled = False
    li053.State = 0
    Trigger011.Enabled = False
    li066.State = 0
    Trigger014.Enabled = False
    li067.State = 0
    Trigger015.Enabled = False
    li068.State = 0
    Trigger009.Enabled = False
    li052.State = 0
    Trigger013.Enabled = False
    li059.State = 0
    blocktarget006. isdropped = 0
    SidMBTrigger.enabled =False
End Sub


'******WizKillBakOn Triggers and Lights******
Sub WizKillBakOnTriggersNLights
    Trigger012.Enabled = True
    li062.State = 0
    Trigger016.Enabled = True
    li064.State = 0
    Trigger007.Enabled = True
    li053.State = 0
    Trigger011.Enabled = True
    li066.State = 0
    Trigger014.Enabled = True
    li067.State = 0
    Trigger015.Enabled = True
    li068.State = 0
    Trigger009.Enabled = True
    li052.State = 0
    Trigger013.Enabled = True
    li059.State = 0
    blocktarget006. isdropped = 1
    SidMBTrigger.Enabled = True
End Sub


'******WizMode Triggers On******
Sub WizModeTriggersNLightsOn
    WizTrigger001.Enabled = True
    WizTrigger002.Enabled = True
    WizTrigger003.Enabled = True
    WizTrigger004.Enabled = True
    WizTrigger005.Enabled = True
    WizTrigger006.Enabled = True
    WizTrigger007.Enabled = True
    WizTrigger008.Enabled = True
End Sub


'******WizMode Triggers and Lights Off******
Sub WizModeTriggersNLightsOff
    WizTrigger001.Enabled = False
    WizLight001.State = 0
    WizTrigger005.Enabled = False
    WizLight002.State = 0
    WizTrigger003.Enabled = False
    WizLight003.State = 0
    WizTrigger004.Enabled = False
    WizLight004.State = 0
    WizTrigger005.Enabled = False
    WizLight005.State = 0
    WizTrigger006.Enabled = False
    WizLight006.State = 0
    WizTrigger007.Enabled = False
    WizLight007.State = 0
    WizTrigger008.Enabled = False
    WizLight008.State = 0
End Sub



'******WizMode Triggers hits******
Sub WizTrigger001_Hit
    AwardJackpot
End Sub


Sub WizTrigger002_Hit
    AwardJackpot
End Sub


Sub WizTrigger003_Hit
    DMDFlush
    DMD CL("JACKPOT"), CL(FormatScore(Jackpot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1500, True, "" 
    AddScore2 Jackpot(CurrentPlayer)
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + 100000
End Sub


Sub WizTrigger004_Hit
    AwardJackpot
End Sub


Sub WizTrigger005_Hit
    AwardJackpot
End Sub


Sub WizTrigger006_Hit
    AwardJackpot
End Sub


Sub WizTrigger007_Hit
    AwardJackpot
End Sub


Sub WizTrigger008_Hit
    AwardJackpot
End Sub
    




Sub EndOfWKicker001_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker001.DestroyBall:EndOfModeTarget001.Isdropped = 0:Gate002.Open = True:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker002_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker002.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker003_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker003.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker004_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker004.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker005_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker005.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker006_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker006.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker007_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker007.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker008_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker008.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker009_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker009.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker010_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker010.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker011_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker011.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker012_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker012.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker013_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker013.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker014_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker014.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker015_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker015.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker016_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker016.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker017_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker017.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub EndOfWKicker018_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    EndOfWKicker018.DestroyBall:EndOfModeTarget001.Isdropped = 0:door001.RotateToStart:door002.RotateToStart
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub MBWDrainKicker_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    MBDrainKicker.DestroyBall:EndOfModeKicker014.enabled = 0:EndOfModeKicker017.enabled = 0
    MBDrainOutKicker.CreateBall:MBDrainOutKicker.Kick 100,9:PlaySound ""
    MBDrainOutKicker.Kick 100, a
End Sub


Sub WizardDrainKicker_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    WizardDrainKicker.DestroyBall':EndOfModeTarget001.Isdropped = 0':EndOfModeTarget002.Isdropped = 0
    EndOfModeKickOutKicker001.CreateBall:EndOfModeKickOutKicker001.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    EndOfModeKickOutKicker001.Kick 100, a
End Sub


Sub WizardOopsKicker_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    WizardOopsKicker.DestroyBall':EndOfModeTarget001.Isdropped = 0':EndOfModeTarget002.Isdropped = 0
    WizardOopsOutKicker.CreateBall:WizardOopsOutKicker.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    WizardOopsOutKicker.Kick 100, a
End Sub


Sub WizardOopsKicker001_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    WizardOopsKicker001.DestroyBall':EndOfModeTarget001.Isdropped = 0':EndOfModeTarget002.Isdropped = 0
    WizardOopsOutKicker.CreateBall:WizardOopsOutKicker.Kick 100,9:PlaySound ""':MBDrainTrigger.enabled = True
    WizardOopsOutKicker.Kick 100, a
End Sub



'******************************
' Basment Doors Open & Close
'******************************

Sub closethedoorstimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    closethedoorstimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    closedoors
End Sub


Sub openthedoors_Hit
    If ballsonplayfield =1 and MBlocklight.State = 0 Then
    vpmtimer.addtimer 500, "opendoors'"
    'opendoors
    closethedoorstimer.Enabled = True
    'vpmtimer.addtimer 5500, "closedoors'"
    End If
    if MBlocklight.State = 1 Then
    vpmtimer.addtimer 500, "opendoors'"
    vpmtimer.addtimer 5700, "opendoors'"
    closethedoorstimer.Enabled = False 
    End If   
End Sub


Sub openthedoors2_Hit
    If ballsonplayfield =1 and MBlocklight.State = 0 Then
    'vpmtimer.addtimer 500, "opendoors'"
    opendoors
    vpmtimer.addtimer 3500, "closedoors'"
    End If
    if MBlocklight.State = 1 Then
    vpmtimer.addtimer 500, "opendoors'"
    End If    
End Sub


Sub Closeemup
    vpmtimer.addtimer 900, "closedoors'"
    if MBlocklight.State = 1 Then
    vpmtimer.addtimer 5700, "opendoors'"
    End If
End Sub


Sub doortrigger2off_Hit
    openthedoors2.enabled =False
End Sub


Sub doortrigger2on_Hit
    openthedoors2.enabled =True
End Sub


Sub door2triggersoff
    openthedoors2.enabled =False
    doortrigger2off.enabled =False
    doortrigger2on.enabled =False
End Sub


Sub door2triggersbackon
    'openthedoors2.enabled =False
    doortrigger2off.enabled =True
    doortrigger2on.enabled =True
End Sub
    
    

Sub closedoors
    door001.RotateToStart
    door002.RotateToStart
    'vpmtimer.addtimer 5800, "closedoors'"
End Sub


Sub opendoors
    door001.RotateToEnd
    door002.RotateToEnd
    'vpmtimer.addtimer 5800, "closedoors'"
End Sub


Sub shutthedoorsdrain
    vpmtimer.addtimer 5900, "closedoors'"
End Sub


Sub openemup
    vpmtimer.addtimer 5800, "closedoors'"
end Sub


Sub GateOpen
    GFModeTarget001.Isdropped = 1
End Sub


Sub GateClosed
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
End Sub



Sub ScoopGateTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    ScoopGateTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GFModeTarget001.Isdropped = 1
    GF006.Z =-90
End Sub


Sub ScoopGateTimer001_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    ScoopGateTimer001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GFModeTarget001.Isdropped = 1
    GF006.Z =-90
End Sub


Sub ScoopGateTimer002_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    ScoopGateTimer002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GFModeTarget001.Isdropped = 1
    GF006.Z =-90
End Sub


Sub ScoopGateTimer2_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    ScoopGateTimer2.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GFModeTarget001.Isdropped = 1
    GF006.Z =-90
End Sub


Sub ScoopGateTimer3_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    ScoopGateTimer3.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GFModeTarget001.Isdropped = 1
    GF006.Z =-90
End Sub

'ScoopGateTimer2.Enabled = True
'GateClosed 



Sub LRampKicker_Hit
    LRampKicker.Kick 28,30
End Sub 


Sub RRampKicker_Hit
    RRampKicker.Kick 275,20
End Sub   



'******************************
' CHARLIE TARGETS & LIGHTS 5&6
'******************************
Sub charlietarget5_Hit
    vpmtimer.addtimer 1100, "CharlieLnT5'"
    If charlielight5.State Then
                TargetModeHits = TargetModeHits + 1
                charlielight5.State = 0
                charlielight05.State = 0
                CheckWinMode
            End If
End Sub


Sub charlietarget6_Hit
    vpmtimer.addtimer 1100, "CharlieLnT6'"
    If charlielight6.State Then
                TargetModeHits = TargetModeHits + 1
                charlielight6.State = 0
                charlielight06.State = 0
                CheckWinMode
            End If
End Sub

    
'charlielight6.State = 0
'charlietarget6.Isdropped = 1




Sub CharlieLnTOff
    charlietarget5.Enabled = False
    charlietarget6.Enabled = False
    charlielight5.State = 0
    charlielight6.State = 0
    charlielight05.State = 0
    charlielight06.State = 0
End Sub



Sub CharlieLnT5
    charlietarget5.Enabled = False
    charlielight5.State = 0
    charlielight05.State = 0
End Sub


Sub CharlieLnT6
    charlietarget6.Enabled = False
    charlielight6.State = 0
    charlielight06.State = 0
End Sub
    


'******************************
' Mickey Mode Kill Timer
'******************************
Sub MickeyTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    MickeyTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GFEyesGlowingOFF
    GFEyesGlowingPinkOFF
    KillAllMovies
    ModeRingTriggersKill
    EndOfModeClosed
    EndOfModeColorChange
    ModeLitLightItUp
    KillAllMovies
    pupBGmusicrestart
    li062.State = 0
    li064.State = 0
    li066.State = 0
    li067.State = 0
    li068.State = 0
End Sub


Sub mickeyNromanOff
    MickeyTimer.Enabled = False
    GFEyesGlowingOFF
    GFEyesGlowingPinkOFF
    KillAllMovies
    ModeRingTriggersKill
    EndOfModeClosed
    EndOfModeColorChange
    ModeLitLightItUp
    KillAllMovies
    li062.State = 0
    li064.State = 0
    li066.State = 0
    li067.State = 0
    li068.State = 0
End Sub



'******************************
' PUP-Pack multiball lock
'******************************
Sub MBlockPUP
    If NewMode = 0 and MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 1 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 2 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 3 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 4 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 5 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 6 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 7 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 8 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 9 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 10 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 11 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 12 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 13 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 14 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
    If NewMode = 15 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","locklit.mkv",mblockvol,1
    End If
End Sub


Sub MBlockPUP1
    If NewMode = 0 and MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 1 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 2 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 3 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 4 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 5 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 6 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 7 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 8 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 9 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 10 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 11 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 12 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 13 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 14 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
    If NewMode = 15 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock1.mkv",mblockvol,1
    End If
End Sub


Sub MBlockPUP2
    If NewMode = 0 and MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 1 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 2 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 3 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 4 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 5 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 6 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 7 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 8 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 9 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 10 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 11 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 12 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 13 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 14 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
    If NewMode = 15 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videomblock","lock2.mkv",mblockvol,1
    End If
End Sub



'******************************
' PUP-Pack Police Hurry Up
'******************************
Sub PoliceHurryPUP
    If NewMode = 0 and MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 1 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 2 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 3 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 4 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 5 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 6 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 7 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 8 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 9 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 10 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 11 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 12 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 13 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 14 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 15 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicehurry","policehurry.mkv",videovol,1
    ScoopGateTimer3.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
End Sub


Sub PoliceKilledPUP
    If NewMode = 0 and MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 1 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 2 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 3 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 4 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 5 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 6 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 7 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 8 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 9 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 10 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 11 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 12 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 13 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 14 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
    If NewMode = 15 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videopolicekilled","",videovol,1
    End If
End Sub



'******************************
' PUP-Pack Core4 Hurry up
'******************************
Sub Core4HurryPUP
    If NewMode = 0 and MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
    If NewMode = 1 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 2 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 3 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 4 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 5 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 6 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 7 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 8 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 9 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 10 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 11 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 12 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 13 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 14 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed
    End If
    If NewMode = 15 And MBlocklight.State = 0 And BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videocore4hurry","core4hurry.mkv",videovol,1
    ScoopGateTimer2.Enabled = True
    GFModeTarget001.Isdropped = 0
    GF006.Z =5
    GateClosed 
    End If
End Sub




'******************************
' PUP-Pack Base & Intro
'******************************
Sub pupintro
    PuPlayer.playlistplayex pBackglass,"videointro","",introvol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupbase
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupstartup
    PuPlayer.playlistplayex pBackglass,"videostart","Start.mkv",videovol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupdrain
    PuPlayer.playlistplayex pBackglass,"videodrain","drain.mkv",drainvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
End Sub




'******************************
' PUP-Pack Modes
'******************************
Sub pupmodebilly
    PuPlayer.playlistplayex pBackglass,"videobilly","billy.mkv",billyvol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmodestu
    PuPlayer.playlistplayex pBackglass,"videostu","stu.mkv",stuvol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmoderichie
    PuPlayer.playlistplayex pBackglass,"videorichie","richie.mkv",richievol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmodeamber
    PuPlayer.playlistplayex pBackglass,"videoamber","amber.mkv",ambervol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmodequinn
    PuPlayer.playlistplayex pBackglass,"videoquinn","quinn.mkv",quinnvol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmodeethan
    PuPlayer.playlistplayex pBackglass,"videoethan","ethan.mkv",ethanvol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmodewayne
    PuPlayer.playlistplayex pBackglass,"videowayne","wayne.mkv",waynevol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmodejill
    PuPlayer.playlistplayex pBackglass,"videojill","jill.mkv",jillvol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmodecharlie
    PuPlayer.playlistplayex pBackglass,"videocharlie","charlie.mkv",charlievol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmodemickey
    PuPlayer.playlistplayex pBackglass,"videomickey","mickey.mkv",mickeyvol,1
End Sub


Sub pupmoderoman
    PuPlayer.playlistplayex pBackglass,"videoroman","roman.mkv",romanvol,1
End Sub


Sub pupmodegale
    PuPlayer.playlistplayex pBackglass,"videogale","gale.mkv",galevol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmodemloomis
    PuPlayer.playlistplayex pBackglass,"videomloomis","mloomis.mkv",mloomisvol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmodedewey
    PuPlayer.playlistplayex pBackglass,"videodewey","dewey.mkv",deweyvol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmodewizard
    PuPlayer.playlistplayex pBackglass,"videowizard","wizardmode.mkv",wizardvol,1
End Sub


Sub pupmodewizardend
    PuPlayer.playlistplayex pBackglass,"EndOfWizardMode","wizardend.mkv",wizardvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


'******************************
' PUP-Pack End of Modes
'******************************
Sub pupendofmodebilly
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedbilly.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmodestu
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedstu.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmoderichie
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedrichie.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmodeamber
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedamber.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmodequinn
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedquinn.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmodeethan
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedethan.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmodewayne
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedwayne.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmodejill
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedjill.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmodecharlie
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedcharlie.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmodemickey
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedmickey.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmoderoman
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedroman.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmodegale
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedgale.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmodemloomis
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killedmloomis.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


Sub pupendofmodedewey
    if BallsOnPlayfield =1 Then
    PuPlayer.playlistplayex pBackglass,"videokilled","killeddewey.mkv",killedvol,1
    PuPlayer.playlistplayex pBackglass,"videobase","Base.mkv",videovol,1
    PuPlayer.SetBackground pBackglass,1
    End If
End Sub


'******************************
' PUP-Pack Multiballs
'******************************

Sub pupmultiballsid
    PuPlayer.playlistplayex pBackglass,"videosid","",sidvol,1
    PuPlayer.SetBackground pBackglass,1
End Sub



Sub pupmultiballtara
    PuPlayer.playlistplayex pBackglass,"videotara","tara.mkv",taravol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


Sub pupmultiballsam
    PuPlayer.playlistplayex pBackglass,"videosam","sam.mkv",samvol,1
    PuPlayer.SetBackground pBackglass,1
End Sub


'******************************
' PUP-Pack FULLDMD
'******************************

Sub Fulldmdbase
    PuPlayer.playlistplayex pFullDMD,"fulldmdbase","fulldmdbase.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdquinn
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdquinn.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdtara
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdtara.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdsam
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdsam.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdsid
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdsid.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdamber
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdamber.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub



Sub Fulldmdbilly
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdbilly.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdcharlie
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdcharlie.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmddewey
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmddewey.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdethan
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdethan.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdgale
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdgale.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdjill
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdjill.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdmickey
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdmickey.mkv",0,1
End Sub


Sub Fulldmdbmloomis
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdmloomis.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdrichie
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdrichie.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdroman
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdroman.mkv",0,1
End Sub


Sub Fulldmdstu
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdstu.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdwayne
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdwayne.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub


Sub Fulldmdwizard
    PuPlayer.playlistplayex pFullDMD,"fulldmd","fulldmdwizard.mkv",0,1
    PuPlayer.playlistplayex pFullDMD,"fulldmdbase","fulldmdbase.mkv",0,1
    PuPlayer.SetBackground pFullDMD,1
End Sub



'***********************************
' PUP-Pack Topper Videos base&intro
'***********************************

Sub Topperintro
    PuPlayer.playlistplayex pTopper2,"topperintro","introtopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Topperbase
    PuPlayer.playlistplayex pTopper2,"topperbase","topperbase.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


'******************************
' PUP-Pack Topper Videos Modes
'******************************

Sub Toppermodeamber
    PuPlayer.playlistplayex pTopper2,"topper","ambertopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub



Sub Toppermodebilly
    PuPlayer.playlistplayex pTopper2,"topper","billytopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodecharlie
    PuPlayer.playlistplayex pTopper2,"topper","charlietopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodedewey
    PuPlayer.playlistplayex pTopper2,"topper","deweytopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodeethan
    PuPlayer.playlistplayex pTopper2,"topper","ethantopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodegale
    PuPlayer.playlistplayex pTopper2,"topper","galetopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodejill
    PuPlayer.playlistplayex pTopper2,"topper","jilltopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodemickey
    PuPlayer.playlistplayex pTopper2,"topper","mickeytopper.mkv",0,1
End Sub


Sub Toppermodemloomis
    PuPlayer.playlistplayex pTopper2,"topper","mloomistopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodequinn
    PuPlayer.playlistplayex pTopper2,"topper","quinntopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermoderichie
    PuPlayer.playlistplayex pTopper2,"topper","richietopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermoderoman
    PuPlayer.playlistplayex pTopper2,"topper","romantopper.mkv",0,1
End Sub


Sub Toppermodesam
    PuPlayer.playlistplayex pTopper2,"topper","samtopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodesid
    PuPlayer.playlistplayex pTopper2,"topper","sidtopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodestu
    PuPlayer.playlistplayex pTopper2,"topper","stutopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodetara
    PuPlayer.playlistplayex pTopper2,"topper","taratopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodewayne
    PuPlayer.playlistplayex pTopper2,"topper","waynetopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub


Sub Toppermodewizard
    PuPlayer.playlistplayex pTopper2,"topper","WizardTopper.mkv",0,1
    PuPlayer.SetBackground pTopper2,1
End Sub

'******************************
' PUP-Pack Background Music
'******************************

Sub bgmusictriggeron_Hit
    PuPlayer.playlistplayex pMusic,"audiobg","red right hand.mp3",soundtrackvol,1
    PuPlayer.SetLoop pMusic,1
End Sub


Sub bgmusictriggeron001_Hit
    PuPlayer.playlistplayex pMusic,"audiohorror","",Audiovol,1
    PuPlayer.playlistplayex pMusic,"audiobg","red right hand.mp3",soundtrackvol,1
    PuPlayer.SetLoop pMusic,1
End Sub

Sub bgmusictriggeroff_Hit
    bgmusictriggeron.Enabled = False
    bgmusictriggeron001.Enabled = False
End Sub

Sub pupBGMusicgamerestart
    bgmusictriggeron.Enabled = True
    bgmusictriggeron001.Enabled = False
End Sub


Sub pupBGMusic
    bgmusictriggeron001.Enabled = True
    bgmusictriggeron.Enabled = False
End Sub


Sub pupBGMusicStart
    bgmusictriggeron.Enabled = True
End Sub

Sub bgMusicMute
    PuPlayer.playlistplayex pMusic,"audiobg","mute.mp3",0,1
    PuPlayer.SetLoop pMusic,1
End Sub


Sub pupBGmusicrestart
    PuPlayer.playlistplayex pMusic,"audiohorror","",Audiovol,1
    PuPlayer.playlistplayex pMusic,"audiobg","red right hand.mp3",soundtrackvol,1
    PuPlayer.SetLoop pMusic,1
End Sub


Sub musicrestart_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    musicrestart.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupBGmusicrestart
End Sub





'*******************************
' End of Wizard Mode ANIMATIONS
'*******************************
  Dim GFDir
GFDir = 5 'this is both the direction, if + goes up, if - goes down, and also the speed

Sub FlagTimer_Timer
    wizknife.z = wizknife.z + GFDir
    wizknife001.z = wizknife001.z + GFDir
    If wizknife.z > 15 Then GFDir = -5 'goes down
    If wizknife.z < -30 Then GFDir = 5
    If wizknife001.z > 15 Then GFDir = -5 'goes down
    If wizknife001.z < -30 Then GFDir = 5
    wizknife.visible = 1
    wizknife001.visible = 1
End Sub


 Dim GF2Dir
GF2Dir = 5 'this is both the direction, if + goes up, if - goes down, and also the speed

Sub FlagTimer001_Timer
    wizknife.z = wizknife.z - GF2Dir
    wizknife001.z = wizknife001.z - GF2Dir
    FlagTimer002.Enabled = True
End Sub


Sub FlagTimer002_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    FlagTimer002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    FlagTimer001.Enabled = False
    FlagTimer.Enabled = False
    wizknife.z =10
    wizknife001.z =10
    wizknife.visible = 0
    wizknife001.visible = 0
  end sub


Sub GFDefeatedTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    GFDefeatedTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    FlagTimer001.Enabled = True
End Sub



'**************************************************
' Lightening flasher FX at the right loop
'**************************************************

Sub Strike1flash
    FlashForms strike1, 1800, 30, 0
End Sub


Sub Strike2flash
    FlashForms strike2, 2000, 45, 0
End Sub


Sub Strike3flash
    FlashForms strike3, 1900, 55, 0
End Sub




Sub ShowStrikes1
    Strike1flash
    Strike2flash
    Strike3flash
End Sub



Sub TimedTrigger_Hit
 Select Case NewMode
        Case 3
            If li066.State Then
                TargetModeHits = TargetModeHits + 1
                li066.State = 0
                CheckWinMode
            End If
        Case 5, 13, 14
            If li066.State Then
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
            End If
            If ReadyToKill Then 'kill her :)
                WinMode
            End If
        Case 15
            AwardJackpot
    End Select
End Sub





Sub SidMBSwitchTimer_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SidMBSwitchTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SidMBTrigger.enabled =True
End Sub



'**************************************************
' Sid MB Trigger inside the basement
'**************************************************

Sub SidMBTrigger_Hit
    If Tilted OR ActiveBall.VelY > 0 Then Exit Sub
    If bJasonMBStarted Then
        DOF 127, DOFPulse
        DMDFlush
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(4) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(7)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(4) * 1000000
        If ArrowMultiPlier(4) < 5 Then
            ArrowMultiPlier(4) = ArrowMultiPlier(4) + 1
            UpdateArrowLights
        End If
    End If
    ' lock balls
    Select Case BallsInLock(CurrentPlayer)
        Case 0: 'enabled the first lock
            DMD "_", CL("LOCK IS LIT"), "_", eNone, eNone, eNone, 1000, True, "vo_lockislit"
            BallsInLock(CurrentPlayer) = 1
            UpdateLockLights
            MBlockPUP
            Closeemup
            ScoopGateTimer.Enabled = True
            GFModeTarget001.Isdropped = 0
            GF006.Z =5
            GateClosed    
        Case 1: 'lock 1
            DMD "_", CL("BALL 1 LOCKED"), "_", eNone, eNone, eNone, 1000, True, ""
            BallsInLock(CurrentPlayer) = 2
            UpdateLockLights
            MBlockPUP1
            Closeemup
            ScoopGateTimer.Enabled = False
            ScoopGateTimer001.Enabled = True
            GFModeTarget001.Isdropped = 0
            GF006.Z =5
            GateClosed
        Case 2: 'lock 2
            DMD "_", CL("BALL 2 LOCKED"), "_", eNone, eNone, eNone, 1000, True, ""
            BallsInLock(CurrentPlayer) = 3
            UpdateLockLights
            MBlockPUP2
            Closeemup
            ScoopGateTimer001.Enabled = False
            ScoopGateTimer002.Enabled = True
            GFModeTarget001.Isdropped = 0
            GF006.Z =5
            GateClosed
            MBKillSwitchRestart.enabled =True
            Kicker001.enabled =True
        Case 3 'lock 3 - start multiball if there is not a multiball already
            If NOT bMultiBallMode Then
                DMD "_", CL("BALL 3 LOCKED"), "_", eNone, eNone, eNone, 1000, True, "vo_ball3locked"
                BallsInLock(CurrentPlayer) = 4
                UpdateLockLights
                lighteffect 2
                Flasheffect 5
                StartJasonMultiball
            End If
    End Select
 End Sub



'**************************************************
' THESE ARE TO MAKE SURE THE PUP VIDESO PLAY
'**************************************************
Sub BillyT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    BillyT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodebilly
    Fulldmdbilly
    Toppermodebilly
    bgMusicMute
End Sub


Sub StuT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    StuT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodestu
    Fulldmdstu
    Toppermodestu
    bgMusicMute
End Sub


Sub MickeyT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    MickeyT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodemickey
    Fulldmdmickey
    Toppermodemickey
    bgMusicMute
End Sub


Sub MloomisT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    MloomisT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodemloomis
    Fulldmdbmloomis
    Toppermodemloomis
    bgMusicMute
End Sub


Sub RomanT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    RomanT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmoderoman
    Fulldmdroman
    Toppermoderoman
    bgMusicMute
End Sub


Sub JillT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    JillT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodejill
    Fulldmdjill
    Toppermodejill
    bgMusicMute
End Sub


Sub CharlieT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    CharlieT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodecharlie
    Fulldmdcharlie
    Toppermodecharlie
    bgMusicMute
End Sub


Sub RichieT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    RichieT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmoderichie
    Fulldmdrichie
    Toppermoderichie
    bgMusicMute
End Sub


Sub AmberT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    AmberT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodeamber
    Fulldmdamber
    Toppermodeamber
    bgMusicMute
End Sub


Sub QuinnT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    QuinnT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodequinn
    Fulldmdquinn
    Toppermodequinn
    bgMusicMute
End Sub



Sub EthanT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    EthanT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodeethan
    Fulldmdethan
    Toppermodeethan
    bgMusicMute
End Sub


Sub WayneT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    WayneT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodewayne
    Fulldmdwayne
    Toppermodewayne
    bgMusicMute
End Sub


Sub DeweyT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    DeweyT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodedewey
    Fulldmddewey
    Toppermodedewey
    bgMusicMute
End Sub



Sub GaleT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    GaleT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodegale
    Fulldmdgale
    Toppermodegale
    bgMusicMute
End Sub


Sub SidT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SidT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmultiballsid
    Fulldmdsid
    Toppermodesid
    bgMusicMute
End Sub


Sub SamT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    SamT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmultiballsam
    Fulldmdsam
    Toppermodesam
    bgMusicMute
End Sub


Sub TaraT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    TaraT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmultiballtara
    Fulldmdtara
    Toppermodetara
    bgMusicMute
End Sub


Sub WizT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupmodewizard
    Fulldmdwizard
    Toppermodewizard
    bgMusicMute
End Sub


Sub BaseT_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    BaseT.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    pupdrain
    Topperbase
    Fulldmdbase
    'bgMusicMute
End Sub



Sub BillyET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    BillyET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodebilly
    Fulldmdbase
    Topperbase
End Sub


Sub StuET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    StuET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodestu
    Fulldmdbase
    Topperbase
End Sub


Sub MickeyET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    MickeyET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodemickey
    Fulldmdbase
    Topperbase
End Sub


Sub MloomisET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    MloomisET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodemloomis
    Fulldmdbase
    Topperbase
End Sub


Sub RomanET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    RomanET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmoderoman
    Fulldmdbase
    Topperbase
End Sub


Sub JillET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    JillET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodejill
    Fulldmdbase
    Topperbase
End Sub


Sub CharlieET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    CharlieET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodecharlie
    Fulldmdbase
    Topperbase
End Sub


Sub RichieET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    RichieET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmoderichie
    Fulldmdbase
    Topperbase
End Sub


Sub AmberET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    AmberET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodeamber
    Fulldmdbase
    Topperbase
End Sub


Sub QuinnET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    QuinnET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodequinn
    Fulldmdbase
    Topperbase
End Sub


Sub EthanET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    EthanET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodeethan
    Fulldmdbase
    Topperbase
End Sub


Sub WayneET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    WayneET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodewayne
    Fulldmdbase
    Topperbase
End Sub


Sub DeweyET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    DeweyET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodedewey
    Fulldmdbase
    Topperbase
End Sub


Sub GaleET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    GaleET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupendofmodegale
    Fulldmdbase
    Topperbase
End Sub


Sub WizET_Timer 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizET.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'pupmodewizardend
    Fulldmdbase
    Topperbase
End Sub





Sub Killa_Hit()
    vpmtimer.addtimer 900, "killatrigger'"
    ModeRingTriggersOff
	StopPolice
    StopTommyJarvis
    BigCellRingTrigger.enabled =False
    BigIphoneRingTrigger.enabled =False
    WizardOopsKicker.enabled = 1
    WizardOopsKicker001.enabled = 1
    WIZB001. isdropped = 0
    WIZB002. isdropped = 0
    WIZB003. isdropped = 0
    WIZB004. isdropped = 0
    WIZB005. isdropped = 0
    WIZB006. isdropped = 0
End Sub

Sub killatrigger
    Killa.enabled =False
End Sub
    
    


