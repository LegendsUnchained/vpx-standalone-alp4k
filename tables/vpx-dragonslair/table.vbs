'*************************************************************************
'                      VISUAL PINBALL X
'               	Dragon's Lair Pinball
'*************************************************************************
'
' * Changelog *
'
'2024-02-27 LTEK mode v1.1.1 --  Optimized for full screen Backglass Videos with PuP's FullDMD and fixed stuck ball behind drawbridge
'
'- NEW -'
'	PuP Pack Option 4 - PuP on BG, PupDMD on FullDMD ... made for 3 screens (Backglass, 16x9 FullDMD, Playfield)
'	new simplified FullDMD overlay so the focus is on the DMD messages and score
'	added 7 theme music MP3 from Dragon's Lair video games (to Front End media, and to PuP Pack's Music folder)
'- CHANGED - 
'	removed Backglass overlay to enable full screen videos
'	removed nudge from Magna buttons as they were not needed
'	enabled Playfield Apron 'Quest' screens (notifications)
'	moved Dirk's "regeneration" to Plunger Launch Ball event (was on pop bumpers)
'- FIXED -
'	in original table the ball frequently gets stuck behind draw bridge, added invisible wall
'- ENHANCED - 
'   lot of script cleanup and inline documentation in case anyone else wants to mod it, will make 100x easier
'
'*************************************************************************
Option Explicit
Randomize

'*************************************************************************
' USER Config -- OK to edit this section
'*************************************************************************
Const SongVolume = 0.3	' Value is from 0 to 1 (100%)
Const BallsPerGame = 3  ' usually 3 or 5
Const Pupscreen1 = 1	' Enable Quest Videos on Playfield Apron (1 = Yes, 0 = No)

'*************************************************************************
' ADVANCED Config -- DO NOT EDIT unless NOT using FullDMD
'*************************************************************************
Const EnablePupDmd = TRUE     
' TRUE by Default to use PuP DMD built-into the table  - PuP Pack must be on/used for this to work
' only set to FALSE if using FlexDMD below
Const EnableFlexDmd = FALSE   '  Use the external FlexDMD. Enabling this will NOT disable the Pup_Pack's DMD, Set FALSE if you only want the Table's Built-in DMD (reccomended for 3 screen setups with FullDMD)
Const FlexDMDHighQuality = TRUE ' TRUE = DMD 256*64 // FALSE = DMD 128*32
Dim PuPDMDDriverType: PuPDMDDriverType = 2   ' 0 = 4:1 Screen // 1 = RealDMD  // 2 = FullDMD (16x9 or simular screen)
Dim useRealDMDScale : useRealDMDScale = 0    ' 0 or 1 for RealDMD scaling. ' For RealDMD ONLY
'*************************************************************************

'*************************************************************************
' - DO NOT EDIT BELOW HERE -
'*************************************************************************
Const EnablePupPack = TRUE ' Don't change - table requires PuP to be TRUE
Dim pGameName  : pGameName = "dragonlair"  'pupvideos foldername
Const BallSize = 40    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1.3   ' standard ball mass in JP's VPX Physics v3
Const FPS = 1		' If your Computer is slow set to 0, otherwise set to 1
dim useDMDVideos    : useDMDVideos = TRUE   ' true or false to use DMD splash videos.
'*****************************************************
' 				Define Constants
'*****************************************************
Const cGameName = "Mphisto" 'for used DOF
Const myVersion = "1.1.1"
Const MaxPlayers = 4         ' from 1 to 4
Const MaxBonusMultiplier = 5 'limit Bonus multiplier
Const MaxMultiballs = 3      ' max number of balls during multiballs
Const BallSaverTime = 10 ' value in seconds

'*************************** PuP Settings ********************************
' // Section Removed, PuP use is required so this code is not //
'*************************************************************************
'Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'IF EnablePuppack_EnableFlexDmd Then usePUP   = True: End If 'enable Pinup Player functions for this table
'cPuPPack = "dragonlair"
'Sub PuPStart(cPuPPack)
 '   If PUPStatus=true then Exit Sub
 '   If usePUP=true then
 '       Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
 '       If PuPlayer is Nothing Then
 '           usePUP=false
          '  PUPStatus=false
			
 '       Else
 '           PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
 '           PUPStatus=true
 '       End If
 '   End If
'End Sub
'Sub pupevent(EventNum)
'    if (usePUP=false or PUPStatus=false) then Exit Sub
'    PuPlayer.B2SData "E"&EventNum,1 
'End Sub
'*************************************************************************


'************************************************************
'		Load the core.vbs for supporting Subs and functions
'************************************************************
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    On Error Goto 0
End Sub

'*****************************************************
'			 Use FlexDMD if in FS mode
'*****************************************************
Dim UseFlexDMD
If Table1.ShowDT = True then
   useFlexDMD = False
	Else
		If EnableFlexDmd Then UseFlexDMD = True: End If
End If

'****************************************************
' 				Define Global Variables
'****************************************************
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim PlayfieldMultiplier(4)
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim JackpotValue
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(2)
Dim bAutoPlunger
Dim bAttractMode
Dim x

'****************************************************
' 			Define Game Control Variables
'****************************************************
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLLock
Dim BallsInRLock
'****************************************************
' 				Define Game Flags
'****************************************************
Dim bFreePlay
Dim bGameInPlay : bGameInPlay=False
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bBallSaverActive
Dim bBallSaverReady
Dim BallsInHole
Dim plungerIM 'used mostly as an autofire plunger
Dim bSongSelect
'********************************
' 		QuestInProgress
'********************************
Dim QuestInProgress1(4)
Dim QuestInProgress2(4)
Dim QuestInProgress3(4)
Dim QuestInProgress4(4)
Dim QuestInProgress5(4)
Dim QuestInProgress6(4)
Dim QuestInProgress7(4)
Dim QuestInProgress8(4)
Dim QuestInProgress9(4)
Dim QuestInProgress10(4)
Dim QuestInProgress20(4)
Dim QuestInProgress21(4)
Dim QuestInProgress11(4)
Dim QuestInProgress12(4)
Dim QuestInProgress13(4)
Dim QuestInProgress14(4)
Dim QuestInProgress141(4)
Dim QuestInProgress142(4)
Dim Questwin(4)

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************
'PuPStart(cPuPPack)
Sub Table1_Init()
    LoadEM
    Dim i
	If HasPup Then PuPInit: End If
	pupevent 399  'game initializing
	'pupevent 800
    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init
	
    ' load saved values, highscore, names, jackpot
    Loadhs
	'Impulse Plunger as autoplunger
	Const IMPowerSetting = 50 ' Plunger Power (45)
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .CreateEvents "plungerIM"
    End With
    ' Initalise the DMD display
    'If UseFlexDmd Then 
	DMD_Init': End If
	
    ' freeplay or coins
    bFreePlay = True ' Free play: no coins required

    if bFreePlay Then DOF 125, DOFOn

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
	'initialise any other flags
	bBallSaverActive = False
	bBallSaverReady = False
	BallsInHole = 0
    bMultiBallMode = False
	bGameInPlay = False
   	QuestInProgress1(CurrentPlayer) = False
	QuestInProgress2(CurrentPlayer) = False
	QuestInProgress3(CurrentPlayer) = False
	QuestInProgress4(CurrentPlayer) = False
	QuestInProgress5(CurrentPlayer) = False
	QuestInProgress6(CurrentPlayer) = False
	QuestInProgress7(CurrentPlayer) = False
	QuestInProgress8(CurrentPlayer) = False
	QuestInProgress9(CurrentPlayer) = False
	QuestInProgress10(CurrentPlayer) = False
	QuestInProgress20(CurrentPlayer) = False
	QuestInProgress21(CurrentPlayer) = False
	QuestInProgress11(CurrentPlayer) = False
	QuestInProgress12(CurrentPlayer) = False
	QuestInProgress13(CurrentPlayer) = False
	QuestInProgress14(CurrentPlayer) = False
	QuestInProgress141(CurrentPlayer) = False
	QuestInProgress142(CurrentPlayer) = False
	'Checklightquest()
	HolePos(CurrentPlayer) = 0
	HolePos1(CurrentPlayer) = 0
	Holepos2(CurrentPlayer) = 0
	PFQUESTflasher.visible = False
	questflasher.visible = False
	Animgate1.enabled = False
	videoborne1.enabled = True
	'storm1.Enabled = True
	'Stormflasher.visible = True
	'Q1.enabled = False
	'Q1w.enabled = False
	'Q1l.enabled = False
	G1.Enabled = False
	hitquest.Enabled = False
	Hitflasher.visible = False
	Hitflasher001.visible = False
	Hitflasher002.visible = False
	Superjkp.enabled= False
	bAutoPlunger = False
    bMusicOn = False
    ResetHoleLights
	BallsOnPlayfield = 0
    BallsInLLock = 0
    BallsInRLock = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    JackpotValue = 150000
    bBonusHeld = False
    bJustStarted = True
	bSongSelect = False
    ' set any lights for the attract mode
    GiOff
    StartAttractMode
	    ' Start the RealTime timer
    RealTime.Enabled = 1
	Bridgepos = 1
	'Down.Enabled = True
	
    ' Load table color
    LoadLut
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    ' Pre-game music owns its keys; confirming must not also add a player.
    If bSongSelect And Not bGameInPlay Then
        SelectSong keycode
        Exit Sub
    End If

    If pQuestChoiceActive() And Not Tilted Then
        If keycode = LeftFlipperKey Or keycode = RightFlipperKey Or keycode = PlungerKey Or keycode = StartGameKey Then
            bSongSelect = False
            If keycode = StartGameKey Then keycode = PlungerKey
            pHandleQuestChoice keycode
            Exit Sub
        End If
    End If

    If keycode = LeftMagnaSave Then If (bGameInPlay = False) Then bLutActive = True : NextLUT : End If
    'If KeyCode = RightMagnaSave Then MusicOff: End If
     '   If bLutActive Then NextLUT:End If
    

    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False)Then
            DMDFlush
            DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            pupDMDDisplay "attract", "CREDITS "&(Credits), "", 1, 0, 10
			If Credits = 1 Then
                PlaySound"fx_coin"
            ElseIf Credits > 3 Then
                PlaySound"fx_coin"
            End If
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
        PlaySoundAt "fx_reload", plunger
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Table specific
If bsongSelect Then
        SelectSong(keycode)
    End If
    ' Normal flipper action

    
	If bGameInPlay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1
        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt
		' *** Disabled Nudge using buttons - not needed for Desktop or Cabinet *** LTEK 2024-02-12
		'If keycode = LeftMagnaSave Then Nudge 90, 2:PlaySound "fx_nudge", 0, 1, -0.1, 0.25':CheckTilt
		'If keycode = RightMagnaSave Then Nudge 270, 2:PlaySound "fx_nudge", 0, 1, -0.1, 0.25':CheckTilt


        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "fx_Next"
                Else
                    If(Credits> 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "fx_Next"
                        If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "fx_Esc"
							pupDMDDisplay "attract", "CREDITS "&(Credits)&"^INSERT COIN", "", 1, 0, 10
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True)Then
					'pupevent 800
					'pupevent 300
                    If(BallsOnPlayfield = 0)Then
                        pBeginMusicSelection
                    End If
                Else
                    If(Credits> 0)Then
                        If(BallsOnPlayfield = 0)Then
                            pBeginMusicSelection
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "fx_Esc"
						pupDMDDisplay "attract", "CREDITS "&(Credits)&"^INSERT COIN", "", 1, 0, 10
                        ShowTableInfo
                    End If
                End If
            End If
    End If 
'*******************************************
' If (QuestInProgress Key)
'*******************************************

End Sub

Function pQuestChoiceActive()
    pQuestChoiceActive = False
    If QuestInProgress1(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress2(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress3(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress4(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress5(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress6(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress7(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress8(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress9(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress10(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress20(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress21(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress11(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress12(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress13(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress14(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress141(CurrentPlayer) Then pQuestChoiceActive = True
    If QuestInProgress142(CurrentPlayer) Then pQuestChoiceActive = True
End Function

Sub pClearQuestChoices()
    QuestInProgress1(CurrentPlayer) = False
    QuestInProgress2(CurrentPlayer) = False
    QuestInProgress3(CurrentPlayer) = False
    QuestInProgress4(CurrentPlayer) = False
    QuestInProgress5(CurrentPlayer) = False
    QuestInProgress6(CurrentPlayer) = False
    QuestInProgress7(CurrentPlayer) = False
    QuestInProgress8(CurrentPlayer) = False
    QuestInProgress9(CurrentPlayer) = False
    QuestInProgress10(CurrentPlayer) = False
    QuestInProgress20(CurrentPlayer) = False
    QuestInProgress21(CurrentPlayer) = False
    QuestInProgress11(CurrentPlayer) = False
    QuestInProgress12(CurrentPlayer) = False
    QuestInProgress13(CurrentPlayer) = False
    QuestInProgress14(CurrentPlayer) = False
    QuestInProgress141(CurrentPlayer) = False
    QuestInProgress142(CurrentPlayer) = False
End Sub

Sub pHandleQuestChoice(ByVal keycode)
	If QuestInProgress1(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = PlungerKey then
		Pupevent 624
		DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "sword", "Sword","",1,0,10
		QuestInProgress1(CurrentPlayer) = False
		pupevent 823
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eBlink, eNone, 2000, True, "begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 5000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		
		vpmtimer.addtimer 8000, "CatchHoleExit '"
		Screen.visible = False
		Screen001.visible = False
		
		Else
			If keycode = RightFlipperKey Then
			PuPEvent 623
			DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "right","right","@right.mp4",1,0,10
			QuestInProgress1(CurrentPlayer) = False
			Pupevent 822
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 5000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 5000, "CatchHoleExit '"
			
			Screen001.visible = False
			Screen002.visible = False
		Else
			If keycode = LeftFlipperkey Then
			PuPEvent 622
			DMD "", "", "Lef512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "left", "left", "@left.mp4",1,0,10
			QuestInProgress1(CurrentPlayer) = False
			PuPEvent 822
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 5000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 5000, "CatchHoleExit '"
			Screen.visible = False
			Screen002.visible = False
			End If
			End If
			
		End If
		
        Exit Sub  ' Handle only the quest active for this press.
	End If
	
	If QuestInProgress2(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = RightFlipperKey then
		PuPEvent 623
		DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "right","right","@right.mp4",1,0,10
		QuestInProgress2(CurrentPlayer) = False
		pupevent 826
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 3000, "CatchHoleExit '"
		Screen001.visible = False
		Screen002.visible = False
		Else
			If keycode = LeftFlipperKey Then
			PuPEvent 622
			DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "left", "left","@left.mp4",1,0,10
			QuestInProgress2(CurrentPlayer) = False
			Pupevent 825
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "sword", "sword","",1,0,10
			QuestInProgress2(CurrentPlayer) = False
			Pupevent 825
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
	
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress3(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = RightFlipperKey then
		PuPEvent 623
		DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "right","right","@right.mp4",1,0,10
		QuestInProgress3(CurrentPlayer) = False
		Pupevent 829
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 3000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 4000, "CatchHoleExit '"
		Screen001.visible = False
		Screen002.visible = False
		Else
			If keycode = LeftFlipperKey Then
			PuPEvent 622
			DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "left", "left","@left.mp4",1,0,10
			QuestInProgress3(CurrentPlayer) = False
			Pupevent 828
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword", "Sword","",1,0,10
			QuestInProgress3(CurrentPlayer) = False
			Pupevent 828
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
	
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress4(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = LeftFlipperKey then
		PuPEvent 622
		DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "left", "left","@left.mp4",1,0,10
		QuestInProgress4(CurrentPlayer) = False
		Pupevent 832
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 3000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 4000, "CatchHoleExit '"
		Screen.visible = False
		Screen002.visible = False
		Else
			If keycode = RightFlipperKey Then
			PuPEvent 623
			DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "right","right","@right.mp4",1,0,10
			QuestInProgress4(CurrentPlayer) = False
			Pupevent 831
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen001.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword","Sword","",1,0,10
			QuestInProgress4(CurrentPlayer) = False
			Pupevent 831
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
	
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress5(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = LeftFlipperKey then
		PuPEvent 622
		DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "left", "left","@left.mp4",1,0,10
		QuestInProgress5(CurrentPlayer) = False
		Pupevent 835
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 3000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 4000, "CatchHoleExit '"
		Screen.visible = False
		Screen002.visible = False
		Else
			If keycode = RightFlipperKey Then
			PuPEvent 623
			DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "right","right","@right.mp4",1,0,10
			QuestInProgress5(CurrentPlayer) = False
			Pupevent 834
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen001.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword","Sword","",1,0,10
			QuestInProgress5(CurrentPlayer) = False
			Pupevent 834
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
	
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress6(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = LeftFlipperKey then
		PuPEvent 622
		DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "left", "left","@left.mp4",1,0,10
		QuestInProgress6(CurrentPlayer) = False
		Pupevent 838
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 3000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 4000, "CatchHoleExit '"
		Screen.visible = False
		Screen002.visible = False
		Else
			If keycode = RightFlipperKey Then
			PuPEvent 623
			DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "right","right","@right.mp4",1,0,10
			QuestInProgress6(CurrentPlayer) = False
			Pupevent 837
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen001.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword","Sword","",1,0,10
			QuestInProgress6(CurrentPlayer) = False
			Pupevent 837
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
	
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress7(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = PlungerKey then
		PuPEvent 624
		DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "sword","Sword","",1,0,10
		QuestInProgress7(CurrentPlayer) = False
		Pupevent 841
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 4000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 5000, "CatchHoleExit '"
		Screen.visible = False
		Screen001.visible = False
		Else
			If keycode = LeftFlipperKey Then
			PuPEvent 622
			DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "left", "left","@left.mp4",1,0,10
			QuestInProgress7(CurrentPlayer) = False
			Pupevent 840
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen002.visible = False
		Else
			If keycode = RightFlipperKey Then
			PuPEvent 623
			DMD "", "", "right512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "right", "right","@right.mp4",1,0,10
			QuestInProgress7(CurrentPlayer) = False
			Pupevent 840
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen001.visible = False
			Screen002.visible = False
			End If
		End If
	End If
	
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress8(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = LeftFlipperKey then
		PuPEvent 622
		DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "left", "left","@left.mp4",1,0,10
		QuestInProgress8(CurrentPlayer) = False
		Pupevent 844
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 3000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 4000, "CatchHoleExit '"
		Screen.visible = False
		Screen002.visible = False
		Else
			If keycode = RightFlipperKey Then
			PuPEvent 623
			DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "right","right","@right.mp4",1,0,10
			QuestInProgress8(CurrentPlayer) = False
			Pupevent 843
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen001.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword","Sword","",1,0,10
			QuestInProgress8(CurrentPlayer) = False
			Pupevent 843
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
	
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress9(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = LeftFlipperKey then
		PuPEvent 622
		DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "left", "left","@left.mp4",1,0,10
		QuestInProgress9(CurrentPlayer) = False
		Pupevent 847
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 3000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 4000, "CatchHoleExit '"
		Screen.visible = False
		Screen002.visible = False
		Else
			If keycode = RightFlipperKey Then
			PuPEvent 623
			DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "right","right","@right.mp4",1,0,10
			QuestInProgress9(CurrentPlayer) = False
			Pupevent 846
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen001.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword","Sword","@right.mp4",1,0,10
			QuestInProgress9(CurrentPlayer) = False
			Pupevent 846
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
	
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress10(CurrentPlayer) AND NOT Tilted Then
		pupevent 900		
		If keycode = RightFlipperKey then
		PuPEvent 623
		DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "right","right","@right.mp4",1,0,10
		QuestInProgress10(CurrentPlayer) = False
		AddScore 50000
		IncrementHoleLights1	
		DMD "", "", "Next", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "next","next","@next.mp4",1,0,10
		Screen001.visible = False
		Screen002.visible = False
		vp100
		Else
			If keycode = LeftFlipperKey Then
			PuPEvent 622
			DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "left", "left","@left.mp4",1,0,10
			QuestInProgress10(CurrentPlayer) = False
			Pupevent 849
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword", "Sword","",1,0,10
			QuestInProgress10(CurrentPlayer) = False
			Pupevent 849
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress20(CurrentPlayer) AND NOT Tilted Then
		pupevent 900			
		If keycode = RightFlipperKey then
		PuPEvent 623
		DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "right","right","@right.mp4",1,0,10
		QuestInProgress20(CurrentPlayer) = False
		AddScore 50000
		IncrementHoleLights1
		DMD "", "", "Next", eNone, eNone, eNone, 1000, True, "Begin-tune"
		pupDMDDisplay "next","next","@next.mp4",1,0,10
		Screen001.visible = False
		Screen002.visible = False
		vp100
		Else
			If keycode = LeftFlipperKey Then
			PuPEvent 622
			DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "left", "left","@left.mp4",1,0,10
			QuestInProgress20(CurrentPlayer) = False
			Pupevent 949
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword", "Sword","",1,0,10
			QuestInProgress20(CurrentPlayer) = False
			Pupevent 949
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress21(CurrentPlayer) AND NOT Tilted Then
		pupevent 900			
		If keycode = LeftFlipperKey Then
		PuPEvent 622
		DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "left", "left","@left.mp4",1,0,10
		QuestInProgress21(CurrentPlayer) = False
		Pupevent 850
		GiEffect 3
		DOF 119, 2 'Strobe
		DOF 118, 2 'Fan
		LightEffect 3
		AwardSpecial
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 5000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 7000, "CatchHoleExit '"
		Screen.visible = False
		Screen002.visible = False
		IncrementHoleLights1
		Else
			If keycode = RightFlipperKey Then
			PuPEvent 623
			DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "right","right","@right.mp4",1,0,10
			QuestInProgress21(CurrentPlayer) = False
			Pupevent 951
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 3000, "CatchHoleExit '"
			Screen001.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword","Sword","@right.mp4",1,0,10
			QuestInProgress21(CurrentPlayer) = False
			Pupevent 951
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 3000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
	
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress11(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = RightFlipperKey then
		PuPEvent 623
		DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "right","right","@right.mp4",1,0,10
		QuestInProgress11(CurrentPlayer) = False
		Pupevent 853
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "Begin-tune" 
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 3000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 4000, "CatchHoleExit '"
		Screen001.visible = False
		Screen002.visible = False
		Else
			If keycode = LeftFlipperKey Then
			PuPEvent 622
			DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "left", "left","@left.mp4",1,0,10
			QuestInProgress11(CurrentPlayer) = False
			Pupevent 852
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword", "Sword","",1,0,10
			QuestInProgress11(CurrentPlayer) = False
			Pupevent 852
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
	
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress12(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = RightFlipperKey then
		PuPEvent 623
		DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "right","right","@right.mp4",1,0,10
		QuestInProgress12(CurrentPlayer) = False
		Pupevent 856
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 3000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 4000, "CatchHoleExit '"
		Screen001.visible = False
		Screen002.visible = False
		Else
			If keycode = LeftFlipperKey Then
			PuPEvent 622
			DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "left", "left","@left.mp4",1,0,10
			QuestInProgress12(CurrentPlayer) = False
			Pupevent 855
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword", "Sword","",1,0,10
			QuestInProgress12(CurrentPlayer) = False
			Pupevent 855
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
		End If
		
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress13(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = RightFlipperKey then
		PuPEvent 623
		DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "right","right","@right.mp4",1,0,10
		QuestInProgress13(CurrentPlayer) = False
		Pupevent 859
		AddScore 10000
		GiEffect 3
		DOF 119, 2 'Strobe
		LightEffect 3
		IncrementHoleLights1
		DMD "", "", "great", eNone, eNone, eNone, 1000, True, "begin-tune"  
		pupDMDDisplay "great","great","@great.mp4", 2,0,10
		DMD "", "", "Ballexit", eNone, eBlink, eNone, 3000, True, ""
		pupDMDDisplay "exit","exit","@exit.mp4",5,0,41
		vpmtimer.addtimer 8000, "CatchHoleExit '"
		Screen001.visible = False
		Screen002.visible = False
		Else
			If keycode = LeftFlipperKey Then
			PuPEvent 622
			DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "left", "left","@left.mp4",1,0,10
			QuestInProgress13(CurrentPlayer) = False
			Pupevent 858
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword", "Sword","",1,0,10
			QuestInProgress13(CurrentPlayer) = False
			Pupevent 858
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
		End If
		
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress14(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = LeftFlipperKey then
		PuPEvent 622
		DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "left", "left","@left.mp4",1,0,10
		QuestInProgress14(CurrentPlayer) = False
		AddScore 75000
		IncrementHoleLights1
		DMD "", "", "Next", eNone, eNone, eNone, 1000, True, "Begin-tune"  
		pupDMDDisplay "next","next","@next.mp4",1,0,10
		Screen.visible = False
		Screen002.visible = False
		vp100
		Else
			If keycode = RightFlipperKey Then
			PuPEvent 623
			DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "right","right","@right.mp4",1,0,10
			QuestInProgress14(CurrentPlayer) = False
			Pupevent 861
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen001.visible = False
			Screen002.visible = False
		Else
			If keycode = PlungerKey Then
			PuPEvent 624
			DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Sword","Sword","",1,0,10
			QuestInProgress14(CurrentPlayer) = False
			Pupevent 861
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen001.visible = False
			End If
		End If
	End If
        Exit Sub  ' Handle only the quest active for this press.
	End If
		If QuestInProgress141(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = PlungerKey then
		PuPEvent 624
		DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "Sword","Sword","",1,0,10
		QuestInProgress141(CurrentPlayer) = False
		AddScore 75000
		IncrementHoleLights1
		DMD "", "", "Next", eNone, eNone, eNone, 1000, True, ""
		pupDMDDisplay "next","next","@next.mp4",1,0,10
		Screen.visible = False
		Screen001.visible = False
		vp100
		Else
			If keycode = LeftFlipperKey Then
			PuPEvent 622
			DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "left", "left","@left.mp4",1,0,10
			QuestInProgress141(CurrentPlayer) = False
			Pupevent 953
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen002.visible = False
		Else
			If keycode = RightFlipperKey Then
			PuPEvent 623
			DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Right", "Right","@Right.mp4",1,0,10
			QuestInProgress141(CurrentPlayer) = False
			Pupevent 953
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen001.visible = False
			Screen002.visible = False
			End If
		End If
	End If
        Exit Sub  ' Handle only the quest active for this press.
	End If
	If QuestInProgress142(CurrentPlayer) AND NOT Tilted Then
		pupevent 900				
		If keycode = PlungerKey then
		DMD "", "", "Sword512", eNone, eBlink, eNone, 1000, True, ""
		pupDMDDisplay "Sword","Sword","",1,0,10
		QuestInProgress142(CurrentPlayer) = False
		Pupevent 862
		AddScore 200000
		GiEffect 3
		DOF 119, 2 'Strobe
		DOF 118, 2 'Fan
		LightEffect 3
		IncrementHoleLights1
		Holepos1(CurrentPlayer)= 0
		vpmtimer.addtimer 22000, "CatchHoleExit '"
		Screen.visible = False
		Screen001.visible = False
		'Final
		Else
			If keycode = LeftFlipperKey Then
			PuPEvent 622
			DMD "", "", "Left512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "left", "left","@left.mp4",1,0,10
			QuestInProgress142(CurrentPlayer) = False
			Pupevent 955
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen.visible = False
			Screen002.visible = False
		Else
			If keycode = RightFlipperKey Then
			PuPEvent 623
			DMD "", "", "Right512", eNone, eBlink, eNone, 1000, True, ""
			pupDMDDisplay "Right", "Right","@Right.mp4",1,0,10
			QuestInProgress142(CurrentPlayer) = False
			Pupevent 955
			DMD "", "", "Tryagain", eNone, eBlink, eNone, 4000, True, "tryagain"
			pupDMDDisplay "try","try","@tryagain.mp4",5,0,10
			vpmtimer.addtimer 4000, "CatchHoleExit '"
			Screen001.visible = False
			Screen002.visible = False
			End If
		End If
	End If
	
        Exit Sub  ' Handle only the quest active for this press.
	End If
	
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = LeftMagnaSave Then bLutActive = False
	
    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
        If bBallInPlungerLane Then PlaySoundAt "fx_fire", plunger
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
        End If
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
	If Pupscreen1 = 1 Then
		Screen002.TimerEnabled=false
		Screen001.TimerEnabled=false
		Screen.TimerEnabled=false
		Screen.VideoCapUpdate=""
		Screen001.VideoCapUpdate=""
		Screen002.VideoCapUpdate=""
	End If
    Savehs
	vpmTimer.AddTimer 2500,""
    Controller.Stop
    If UseFlexDmd Then FlexDMD.Run = False
    ' If B2SOn = true Then Controller.Stop
	vpmTimer.AddTimer 2500,""
	Controller.Stop
 
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.75:LeftFlipper.RotateToEnd
		DOF 101,1
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.2:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.75:RightFlipper.RotateToEnd
		DOF 102,1
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.2:RightFlipper.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                  'Called when table is nudged
    Tilt = Tilt + TiltSensitivity              'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity)AND(Tilt <15)Then 'show a warning
        DMD "_", CL(1, "CAREFUL!"), "_", eNone, eBlinkFast, eNone, 500, True, ""
		pupDMDDisplay "attract", "CAREFUL !", "", 1, 1, 10
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "", "", "TILT", eNone, eNone, eBlink, 200, False, ""
		pupDMDDisplay "attract", "TILT !", "", 1, 0, 10
        DisableTable True
		CatchHoleExit
		TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
		
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt> 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
		LightQuesttilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Force = 0
        Bumper2.Force = 0
        Bumper3.Force = 0
		'LeftSlingShot.Disabled = 1
		'RightSlingShot.Disabled = 1
		'EndOfBallComplete
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
		LightQuesttilt.StopPlay
        Bumper1.Force = 10
        Bumper2.Force = 10
        Bumper3.Force = 10
		'LeftSlingShot.Disabled = 0
		'RightSlingShot.Disabled = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'*****************************************
'         Music as wav sounds
' in VPX 10.7 you may use also mp3 or ogg
'*****************************************
Dim Song, Songnr
Song = ""
Songnr = INT(RND * 10)

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, SongVolume
        End If
    End If
End Sub

Sub PlayRandomSong
    Songnr = INT(RND * 10)
    PLaySelectedSong
End Sub

Sub PLaySelectedSong
    Select Case Songnr
        Case 0:PlaySong "DLMusic0" : pupevent 500: pupevent 900
        Case 1:PlaySong "DLMusic1" : pupevent 501: pupevent 900
        Case 2:PlaySong "DLMusic2" : pupevent 502: pupevent 900
        Case 3:PlaySong "DLMusic3" : pupevent 503: pupevent 900
        Case 4:PlaySong "DLMusic4" : pupevent 504: pupevent 900
        Case 5:PlaySong "DLMusic5" : pupevent 505: pupevent 900
        Case 6:PlaySong "DLMusic6" : pupevent 506: pupevent 900
        Case 7:PlaySong "DLMusic7" : pupevent 507: pupevent 900
        Case 8:PlaySong "DLMusic8" : pupevent 508: pupevent 900
        Case 9:PlaySong "DLMusic9" : pupevent 509: pupevent 900
    End Select
	
End Sub

Sub pBeginMusicSelection
    ' No game reset or ball creation happens until the music is confirmed.
    bSongSelect = True
    bMusicOn = True
    pInAttract = False
    pAttractReset = -1
    PriorityReset = -1
    pDMDCurPriority = -1
    pDMDSplashToken = pDMDSplashToken + 1
    pDMDVideoPlaying = False
    StopAttractMode
    If HasPup And PUPStatus Then
        PuPlayer.playstop pDMD
        PuPlayer.playstop pDMDSplashScreen
        PuPlayer.playstop pMusic
    End If
    UpdateDMDSong
End Sub

Sub SelectSong(keycode)
    If keycode = PlungerKey OR keycode = StartGameKey Then
        If Not bGameInPlay Then
            If Not bFreePlay Then
                If Credits < 1 Then Exit Sub
                Credits = Credits - 1
                If Credits < 1 Then DOF 125, DOFOff
            End If
            bSongSelect = False
            ResetForNewGame
            Exit Sub
        End If
        bsongSelect = False
        PriorityReset = -1
        pDMDCurPriority = -1
        If HasPup And EnablePupDmd And PUPStatus Then
            pDMDSetPage pScores
            pUpdateScores
        End If
        Exit Sub
    End If
    If keycode = LeftFlipperKey Then
        Songnr = (Songnr + 9) MOD 10
        UpdateDMDSong
    End If
    If keycode = RightFlipperKey Then
        Songnr = (Songnr + 1) MOD 10
        UpdateDMDSong
    End If
End Sub

Sub UpdateDMDSong() 'Updates the DMD with the chosen song
    If Not bSongSelect Then Exit Sub
    DMDFlush
    Select Case Songnr
        Case 0:DMD "", "", "sel01", eNone, eNone, eNone, 2000, True, "" 
        Case 1:DMD "", "", "sel02", eNone, eNone, eNone, 2000, True, ""
        Case 2:DMD "", "", "sel03", eNone, eNone, eNone, 2000, True, ""
        Case 3:DMD "", "", "sel04", eNone, eNone, eNone, 2000, True, ""
        Case 4:DMD "", "", "sel05", eNone, eNone, eNone, 2000, True, ""
        Case 5:DMD "", "", "sel06", eNone, eNone, eNone, 2000, True, ""
        Case 6:DMD "", "", "sel07", eNone, eNone, eNone, 2000, True, ""
        Case 7:DMD "", "", "sel08", eNone, eNone, eNone, 2000, True, ""
        Case 8:DMD "", "", "sel09", eNone, eNone, eNone, 2000, True, ""
        Case 9:DMD "", "", "sel10", eNone, eNone, eNone, 2000, True, ""
        'Case 10:DMD "", "", "sel10", eNone, eNone, eNone, 2000, True, ""
        
    End Select
    If HasPup And EnablePupDmd And PUPStatus Then
        PriorityReset = -1
        pDMDCurPriority = -1
        pLine1Ani = ""
        pLine2Ani = ""
        pLine3Ani = ""
        pDMDSetPage 9
        PuPlayer.LabelSet pDMD,"MusTitle","SELECT YOUR MUSIC",1,""
        PuPlayer.LabelSet pDMD,"MusTrack","TRACK " & (Songnr + 1) & " / 10",1,""
        PuPlayer.LabelSet pDMD,"MusKeys","LEFT / RIGHT TO CHOOSE",1,""
        PuPlayer.LabelSet pDMD,"MusGo","START / LAUNCH TO PLAY",1,""
    End If
    PLaySelectedSong
End Sub
'**********************
'     GI effects
'**********************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GiOn
    DOF 119, 1
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aGiLights2
        bulb.State = 2
    Next
    gieye001.State = 0
    gieye002.State = 0
End Sub

Sub GiOff
    DOF 119, 0
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aGiLights2
        bulb.State = 0
    Next
    gieye001.State = 0
    gieye002.State = 0
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 100
            LightSeqGi.Play SeqBlinking, , 25, 20
        Case 2 'random
            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqRandom, 100, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 10, 10
		Case 4 'center - used in the bonus count
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqCircleOutOn, 15, 1
        Case 5 'top down
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 15, 2
        Case 6 'down to top
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 15, 1
    End Select
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v3.0
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
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.4, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 0     'number of locked balls
Const maxvel = 42 'max ball velocity
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

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z -24

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 10
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

        ' jps ball speed control
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
Sub aTargets_Hit(idx):ActiveBall.VelZ = BallVel(Activeball) * (RND / 2):End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i
	bMusicOn = True
	'pupevent 910  ' pup background music
	Bridgepos = 2
	'Ramppos = 2
	'animationflamme 
	questflasher.visible = False
	Superjkp.Enabled = False
	G1.enabled = False
	bGameInPLay = True
	gi023.State = 0
    'resets the score display, and turn off attract mode
    StopAttractMode
    GiOn
	
	'pupevent 900
	pupevent 800  ' game loaded (initialization complete; Pupevent E399 is over)
	Screen.visible= False
	Screen001.visible = False
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
    SkillshotValue(0) = 1000
    SkillshotValue(1) = 15000
    SkillshotValue(2) = 30000
	'Jukebox = 1
    ' initialise any other flags
    Tilt = 0
	
    

    ' initialise Game variables
    Game_Init()

    ' Start audio independently of the selection menu / lane sensor.
    Song = ""
    PLaySelectedSong

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
    AddScore 0
	'HolePos = 0
	'HolePos1 = 0
    ' set the current players bonus multiplier back down to 1X
    SetBonusMultiplier 1
	hitquest.Enabled = True
	Hitflasher.visible = True
	'ballsave
	bBallSaverReady = True
    ' reduce the playfield multiplier
    ' reset any drop targets, lights, game Mode etc..

    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False

    'Reset any table specific
    ResetNewBallVariables
    ResetNewBallLights()
	ResetHoleLights()
    'and the skillshot
    bSkillShotReady = True
	Bridgepos=2
	'Up.Enabled = True
'Change the music ?
	'Music
End Sub

' Create a new ball on the Playfield
Sub Brelease
	 ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1
	
    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
	BallRelease.Kick 90, 4
	'pupevent 819
End Sub
Sub CreateNewBall()
   'pupevent 819
    'BallRelease.Kick 90, 4
	vpmTimer.AddTimer 2500, "Brelease '"
' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
   If BallsOnPlayfield> 1 Then
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
        If BallsOnPlayfield <MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                CreateMultiballTimer.Enabled = False
				Superjkp.enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
			Superjkp.enabled = False
        End If
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()

    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

    If NOT Tilted Then
        StartBonusCount
		'pupevent 801
    ' add a bit of a delay to allow for the bonus points to be shown & added up
     'vpmtimer.addtimer 6000, "EndOfBall2 '" 'done in the bonuscount
    Else 'if tilted then only add a short delay
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
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots
	
    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0)Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, ""
		pupDMDDisplay "Extrab", "EXTRA BALL^SHOOT AGAIN", "", 1, 0, 10
        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1
		'pupevent 801
        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            'debug.print "No More Balls, High Score Entry"

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
	'
    'debug.print "EndOfBall - Complete"
	'Checklightquest
    ' are there multiple players playing this game ?
    If(PlayersPlayingGame> 1)Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer> PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode

        EndOfGame()  ' set the machine into game over mode
		' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            PlaySound "vo_player" &CurrentPlayer
            DMD "_", CL(1, "PLAYER " &CurrentPlayer), "_", eNone, eNone, eNone, 800, True, ""
        End If
    End If
	End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
	questflasher.visible = True
	G1.Enabled = True
	'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
        'ChangeSong
    End If

    bJustStarted = False
    ' ensure that the flippers are down	
	SolLFlipper 0
	SolRFlipper 0
    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball
    EjectLeftkicker
    EjectRightkicker
    ' set any lights for the attract mode
    DisableTable True
    HolePos(CurrentPlayer) = 0
	Holepos1(CurrentPlayer) = 0
	Holepos2(CurrentPlayer) = 0
	screen.visible = False		' turn off Playfield Apron Left
	screen001.visible = False  	' turn off Playfield Apron Right
	'pupevent 901  ' pup background music stop
	pupevent 803  ' pup video: game over
	'GiOff
    StartAttractMode
' you may wish to light any Game Over Light you may have
End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

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
    ' Exit Sub ' only for debugging - this way you can add balls from the debug window
	
    BallsOnPlayfield = BallsOnPlayfield - 1
	
    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain
    'if Tilted the end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    PlaySound"SoundAfterballDrains"
    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True)AND(Tilted = False)Then
		' is the ball saver active,
        If(bBallSaverActive = True) Then
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            DMD "_", CL(1, "BALL SAVED"), "_", eNone, eBlinkfast, eNone, 800, True, ""
			Pupevent 810
	    Else
         ' cancel any multiball if on last ball (ie. lost all other balls)
			If(BallsOnPlayfield - BallsInLlock - BallsInRLock = 1)Then
            ' AND in a multi-ball??
				If(bMultiBallMode = True)then
                ' not in multiball mode any more
					bMultiBallMode = False
					Bridgepos = 2
                ' you may wish to change any music over at this point and
					'ChangeSong
                ' turn off any multiball specific lights
					ChangeGi white
            'stop any multiball modes

            End If
        End If

        ' was that the last ball on the playfield
        If(BallsOnPlayfield - BallsInLlock - BallsInRLock = 0)Then
            ' End Mode and timers
            'ChangeSong
            ChangeGi white
			Pupevent 801  ' pup video: BallLost
			snakelight.BlinkInterval = 125
			snakelight.State = 2
            ' Show the end of ball animation
            ' and continue with the end of ball
            ' DMD something?
            StopEndOfBallMode
			vpmtimer.addtimer 2000, "EndOfBall '" 'the delay is depending of the animation of the end of ball, if there is no animation then move to the end of ball
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
    bBallInPlungerLane = True
    'LaunchLight.State = 2     ' turn on Launch light   *not used in this tabl
	Pupevent 819 ' pup video: regenerate Dirk

    'be sure to update the Scoreboard after the animations, if any

    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
   ' debug.print "autofire the ball"
       PlungerIM.AutoFire
       DOF 121, DOFPulse
	   PlaySoundAt "Plunger", swPlungerRest
      bAutoPlunger = False
    End If
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    End If
' show the message to shoot the ball in case the player has fallen sleep :)
    swPlungerRest.TimerEnabled = 1

    'Start the Selection of the skillshot if ready
   If bSkillShotReady Then
       StartSkillshot
		'If Balls = 1 Then PlaySound "vo_changetrack"play
   End If
    ' remember last trigger hit by the ball.
   LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
'ChangeSong
	'LaunchLight.State = 0   ' turn off LaunchLight  *not used in this table
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds

Sub swPlungerRest_Timer
    DMD "_", CL(1, "SHOOT THE BALL"), "_", eNone, eNone, eNone, 800, True, ""
	pupDMDDisplay "Shoot", "SHOOT THE BALL", "", 1, 0, 10
    swPlungerRest.TimerEnabled = 0
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimer.Interval = 1000 * seconds
    BallSaverTimer.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimer_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimer.Enabled = False
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
' In this table we use SecondRound variable to double the score points in the second round after killing Malthael
Sub AddScore(points)
    If(Tilted = False)Then
        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
    End if
' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'in this table the jackpotValue also increases with the BonusPoints
    If(Tilted = False)Then
        ' increase the jackpot
        JackpotValue = JackpotValue + points * 1000 * BonusMultiplier(CurrentPlayer)
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
        If BonusPoints(CurrentPlayer)> 39 Then
            BonusPoints(CurrentPlayer) = 39
        End If
        UpdateBonusLights
    End if
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxBonusMultiplier)then
        ' then add and set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
        DMD "_", CL(1, "BONUS X " &NewBonusLevel), "_", eNone, eNone, eNone, 2000, True, "fx_bonus"
		pupDMDDisplay "attract", "BONUS X "&(NewBonusLevel), "", 2, 0, 10
		'Videobonus
    Else
        AddScore 50000
        DMD "_", CL(1, "50000"), "_", eNone, eNone, eNone, 800, True, ""
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly




Sub SetBonusMultiplier(Level)
	    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level
    ' Update the lights
    Select Case Level
    	Case 1:li003.State = 0:li004.State = 0:li005.State = 0:li006.State = 0
        Case 2:li003.State = 1:li004.State = 0:li005.State = 0:li006.State = 0 : pupevent 812
		Case 3:li003.State = 0:li004.State = 1:li005.State = 0:li006.State = 0 : pupevent 813
		Case 4:li003.State = 0:li004.State = 0:li005.State = 1:li006.State = 0 : pupevent 814
		Case 5:li003.State = 0:li004.State = 0:li005.State = 0:li006.State = 1 : pupevent 815
    End Select
End Sub

Sub StartBonusCount
    BonusCountTimer.Interval = 50
    BonusCountTimer.Enabled = 1
End Sub

Sub BonusCountTimer_Timer
    'debug.print "BonusCount_Timer"
    If BonusPoints(CurrentPlayer)> 0 Then
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer)-1
        AddScore 1000 * BonusMultiplier(Currentplayer)
        UpdateBonusLights
    Else
        BonusCountTimer.Enabled = 0
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

Sub UpdateBonusLights
    Select Case BonusPoints(CurrentPlayer)MOD 10
        Case 0:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 1:bl1.State = 1:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 2:bl1.State = 1:bl2.State = 1:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 3:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 4:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 5:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 6:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 7:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 1:bl8.State = 0:bl9.State = 0
        Case 8:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 1:bl8.State = 1:bl9.State = 0
        Case 9:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 1:bl8.State = 1:bl9.State = 1
    End Select
    Select Case BonusPoints(CurrentPlayer) \ 10
        Case 0:bl10.State = 0:bl20.State = 0:bl30.State = 0
        Case 1:bl10.State = 1:bl20.State = 0:bl30.State = 0
        Case 2:bl10.State = 1:bl20.State = 1:bl30.State = 0
        Case 3:bl10.State = 1:bl20.State = 1:bl30.State = 1
    End Select
End Sub

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        DMD "_", CL(1, ("EXTRA BALL WON")), "_", eNone, eBlink, eNone, 1500, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        pupDMDDisplay "attract", "EXTRA BALL^WON", "", 2, 0, 10
		DOF 121, DOFPulse
		pupevent 802
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
		flashforms flasher001, 2000, 50, 0
		flashforms flasher002, 2000, 50, 0
        LightShootAgain.State = 1 'light the shoot again lamp
        GiEffect 3
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL(1, ("EXTRA GAME WON")), "_", eNone, eBlink, eNone, 1500, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    pupDMDDisplay "attract", "EXTRA GAME^WON", "", 2, 0, 10
	DOF 121, DOFPulse
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
End Sub

Sub AwardJackpot() 'award a normal jackpot
    DMD CL(0, FormatScore(JackpotValue)), CL(1, "JACKPOT"), "bkborder", eBlinkFast, eBlinkFast, eNone, 1500, True, ""
    pupDMDDisplay "attract",FormatScore(JackpotValue) & " ^JACKPOT", "", 2, 0, 10
	DOF 126, DOFPulse
    PlaySound "Jackpot"
    AddScore JackpotValue
    LightEffect 2
    'reset Jackpot after it has been collected
    JackpotValue = 50000
End Sub

Sub AwardSuperJackpot() 'award a Super jackpot
    DMD CL(0, FormatScore(JackpotValue)), CL(1, "SUPERJACKPOT"), "bkborder", eBlinkFast, eBlinkFast, eNone, 1500, True, ""
    pupDMDDisplay "attract",FormatScore(JackpotValue) & " ^JACKPOT", "", 2, 0, 10
	DOF 126, DOFPulse
    PlaySound "Jackpot"
    AddScore JackpotValue
    LightEffect 2
    'reset Jackpot after it has been collected
    JackpotValue = 150000
End Sub

Sub StopSkillShot
    SkillShotTimer.Enabled = 0
End Sub

Sub AwardSkillshot()
    li025.State = 0
    li026.State = 0
    li027.State = 0
    bSkillShotReady = False
    'show dmd animation
    DMD CL(0, FormatScore(SkillshotValue(SkStep))), CL(1, ("SKILLSHOT")), "bkborder", eBlinkFast, eBlink, eNone, 2000, True, ""
    pupDMDDisplay "attract",FormatScore(SkillshotValue(SkStep)) & " ^SKILLSHOT", "", 2, 0, 10
	DOF 127, DOFPulse
    'PlaySound "fx_fanfare2"
    Addscore SkillShotValue(SkStep)
    GiEffect 2
'    LightEffect 2
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 100000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 100000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(cGameName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
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
    HighScore(0) = 150000
    HighScore(1) = 140000
    HighScore(2) = 130000
    HighScore(3) = 120000
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
    tmp = Score(1)
    If Score(2)> tmp Then tmp = Score(2)
    If Score(3)> tmp Then tmp = Score(3)
    If Score(4)> tmp Then tmp = Score(4)

    If tmp> HighScore(1)Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
        DOF 125, DOFOn
    End If

    If tmp> HighScore(3)Then
        PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
		
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
	pupevent 804	
	hsbModeActive = True
    'ChangeSong
    hsLetterFlash = 0

    hsEnteredDigits(0) = "A "
    hsEnteredDigits(1) = "_"
    hsEnteredDigits(2) = "_"
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' ` is back arrow
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
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit> 0)then
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
'
    TempTopStr = "YOUR NAME:"
    dLine(0) = ExpandLine(TempTopStr, 0)
    DMDUpdate 0
'
	TempBotStr = " >"
    if(hsCurrentDigit> 0)then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)
'
    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempBotStr = TempBotStr & "_"
			pupDMDDisplay "HSEnter"," ^"&TempBotStr&"_","",20,0,10
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
			pupDMDDisplay "HSEnter", " ^" & TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1), "", 20, 0, 10
        end if
    end if
'
    if(hsCurrentDigit <1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <  "
    dLine(1) = ExpandLine(TempBotStr, 1)
    DMDUpdate 1
	pupDMDDisplay "HSEnter", "Enter Initials^"&" "&Mid(TempBotStr,2,5), "", 20, 0, 10  
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2)then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False
    'ChangeSong
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
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
            If HighScore(j) <HighScore(j + 1)Then
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

'*********
'   LUT
'*********

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

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 10:UpdateLUT:SaveLUT:End Sub

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
    End Select
End Sub

' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
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

Dim dCharsPerLine(2)
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
    If EnableFlexDMD Then
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
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 12, 22
                Next
                For i = 20 to 39
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
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
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
    deBlinkSlowRate = 5
    deBlinkFastRate = 2
    dCharsPerLine(0) = 20 'characters lower line
    dCharsPerLine(1) = 20 'characters top line
    dCharsPerLine(2) = 1  'characters back line
    For i = 0 to 2
        dLine(i) = Space(dCharsPerLine(i))
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
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
    Dim tmp, tmp1, tmp2
    if(dqHead = dqTail)Then
        tmp = RL(0, FormatScore(Score(Currentplayer)))
        'tmp = CL(0, FormatScore(Score(Currentplayer) ) )
        tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)
        tmp2 = "bkborder"
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail <dqSize)Then
        if(Text0 = "_")Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0, 0)
        End If

        if(Text1 = "_")Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1, 1)
        End If

        if(Text2 = "_")Then
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
        if(dqTail = 1)Then
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
            Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
            Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
        End Select
    Next
    if(dqSound(dqHead) <> "")Then
        PlaySound(dqSound(dqHead))
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
    if(dqHead = dqTail)Then
        if(dqbFlush(Head) = True)Then
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
        if(deCount(i) <> deCountEnd(i))Then
            deCount(i) = deCount(i) + 1

            select case(dqEffect(i, dqHead))
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), dCharsPerLine(i)- 1)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1)- deCount(i), 1)
                    Temp = Temp & Left(dLine(i), dCharsPerLine(i)- 1)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkSlowRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i))
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkFastRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i))
                    End If
            End Select

            if(dqText(i, dqHead) <> "_")Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next

    if(deCount(0) = deCountEnd(0))and(deCount(1) = deCountEnd(1))and(deCount(2) = deCountEnd(2))Then

        if(dqTimeOn(dqHead) = 0)Then
            DMDFlush()
        Else
            if(BlinkEffect = True)Then
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

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(dCharsPerLine(id))
    Else
        if(Len(TempStr)> Space(dCharsPerLine(id)))Then
            TempStr = Left(TempStr, Space(dCharsPerLine(id)))
        Else
            if(Len(TempStr) <dCharsPerLine(id))Then
                TempStr = TempStr & Space(dCharsPerLine(id)- Len(TempStr))
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i, NumString
    NumString = CStr(abs(Num))
    For i = Len(NumString)-3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1))then
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 48) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function

Function CL(id, NumString) 'center line
    Dim Temp, TempStr
    Temp = (dCharsPerLine(id)- Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(id, NumString) 'right line
    Dim Temp, TempStr
    Temp = dCharsPerLine(id)- Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
End Function

'**************
' Update DMD
'**************

Sub DMDUpdate(id)
    Dim digit, value
    If UseFlexDmd Then FlexDMD.LockRenderThread
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
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "bkempty"
            Digits(40).ImageA = dLine(2)
            If UseFlexDmd Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
    End Select
    If UseFlexDmd Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
    If achar = "" Then achar = " "
    achar = ASC(achar)
    Digits(adigit).ImageA = Chars(achar)
    If UseFlexDmd Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
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
    For i = 0 to 255:Chars(i) = "dempty":Next

    Chars(32) = "dempty"
    Chars(35) = "jp1"
    Chars(36) = "jp2"
    Chars(37) = "jp3"
    Chars(38) = "title1"
    Chars(39) = "title2"
    Chars(40) = "title3"
    Chars(46) = "dot"     '.
    Chars(48) = "d0"      '0
    Chars(49) = "d1"      '1
    Chars(50) = "d2"      '2
    Chars(51) = "d3"      '3
    Chars(52) = "d4"      '4
    Chars(53) = "d5"      '5
    Chars(54) = "d6"      '6
    Chars(55) = "d7"      '7
    Chars(56) = "d8"      '8
    Chars(57) = "d9"      '9
    Chars(60) = "dless"   '<
    Chars(61) = "dequal"  '=
    Chars(62) = "dmore"   '>
    Chars(64) = "bkempty" '@
    Chars(65) = "da"      'A
    Chars(66) = "db"      'B
    Chars(67) = "dc"      'C
    Chars(68) = "dd"      'D
    Chars(69) = "de"      'E
    Chars(70) = "df"      'F
    Chars(71) = "dg"      'G
    Chars(72) = "dh"      'H
    Chars(73) = "di"      'I
    Chars(74) = "dj"      'J
    Chars(75) = "dk"      'K
    Chars(76) = "dl"      'L
    Chars(77) = "dm"      'M
    Chars(78) = "dn"      'N
    Chars(79) = "do"      'O
    Chars(80) = "dp"      'P
    Chars(81) = "dq"      'Q
    Chars(82) = "dr"      'R
    Chars(83) = "ds"      'S
    Chars(84) = "dt"      'T
    Chars(85) = "du"      'U
    Chars(86) = "dv"      'V
    Chars(87) = "dw"      'W
    Chars(88) = "dx"      'X
    Chars(89) = "dy"      'Y
    Chars(90) = "dz"      'Z
    Chars(94) = "dup"     '^
    '    Chars(95) = '_
    Chars(96) = "d0a"  '0.
    Chars(97) = "d1a"  '1. 'a
    Chars(98) = "d2a"  '2. 'b
    Chars(99) = "d3a"  '3. 'c
    Chars(100) = "d4a" '4. 'd
    Chars(101) = "d5a" '5. 'e
    Chars(102) = "d6a" '6. 'f
    Chars(103) = "d7a" '7. 'g
    Chars(104) = "d8a" '8. 'h
    Chars(105) = "d9a" '9  'i
End Sub

'********************
' Real Time updates
'********************
' used for all the real time updates like gates, diverters, flippers

Sub Realtime_Timer
    RollingUpdate
    'LeftFlipperTop.Rotz = LeftFlipper.CurrentAngle
    'RightFlipperTop.Rotz = RightFlipper.CurrentAngle
    Diverter001.Rotz = DiverterF.CurrentAngle
	'Diverter002.Rotz = DiverterF001.CurrentAngle

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
' 10 colors: red, orange, amber, yellow...
'******************************************
' in this table this colors are use to keep track of the progress during the acts and battles

'colors
Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white

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

Sub SetLightColor(n, col, stat)
    Select Case col
        Case 0
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
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
            n.color = RGB(255, 252, 224)
            n.colorfull = RGB(193, 91, 0)
        Case white
            n.color = RGB(255, 252, 224)
            n.colorfull = RGB(193, 91, 0)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

'*************************
' Rainbow Changing Lights
'*************************

Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

Sub StartRainbow(n)
    set RainbowLights = n
    RGBStep = 0
    RGBFactor = 5
    rRed = 255
    rGreen = 0
    rBlue = 0
    RainbowTimer.Enabled = 1
End Sub

Sub StopRainbow()
    Dim obj
    RainbowTimer.Enabled = 0
    RainbowTimer.Enabled = 0
End Sub

Sub RainbowTimer_Timer 'rainbow led light color changing
    Dim obj
    Select Case RGBStep
        Case 0 'Green
            rGreen = rGreen + RGBFactor
            If rGreen> 255 then
                rGreen = 255
                RGBStep = 1
            End If
        Case 1 'Red
            rRed = rRed - RGBFactor
            If rRed <0 then
                rRed = 0
                RGBStep = 2
            End If
        Case 2 'Blue
            rBlue = rBlue + RGBFactor
            If rBlue> 255 then
                rBlue = 255
                RGBStep = 3
            End If
        Case 3 'Green
            rGreen = rGreen - RGBFactor
            If rGreen <0 then
                rGreen = 0
                RGBStep = 4
            End If
        Case 4 'Red
            rRed = rRed + RGBFactor
            If rRed> 255 then
                rRed = 255
                RGBStep = 5
            End If
        Case 5 'Blue
            rBlue = rBlue - RGBFactor
            If rBlue <0 then
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
    If Score(1)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD "", "", "gameover", eNone, eNone, eBlink, 2000, False, ""
    If bFreePlay Then
        DMD "", CL(1, "FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits> 0 Then
            DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    'DMD "", "", "cezpresents", eNone, eNone, eNone, 3000, False, ""
	'DMD "", "", "production", eNone, eNone, eNone, 3000, False, ""
    'DMD "", "", "DLlogo3", eNone, eNone, eNone, 2000, False, ""
	DMD "", "", "DLlogo1", eNone, eNone, eNone, 2000, False, ""
	DMD "", "", "DLlogo2", eNone, eNone, eNone, 2000, False, ""
	DMD "", "", "DLlogo", eNone, eNone, eNone, 2000, False, ""
	DMD "", "", "DLlogo3", eNone, eNone, eNone, 2000, False, ""
	
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0)), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    'ChangeSong
	bMusicOn = False
    StartLightSeq
	LightQuest001Seq
    DMDFlush
    ShowTableInfo
	pAttractStart
	'pupevent 800: pupevent 900
	'pupevent 901		
End Sub

Sub StopAttractMode
    DMDScoreNow
    LightSeqAttract.StopPlay
	LightQuest001.StopPlay
End Sub

Sub LightQuest001Seq()
	LightQuest001.UpdateInterval = 25
    LightQuest001.Play SeqBlinking, , 5, 150
    LightQuest001.Play SeqRandom, 40, , 4000
    LightQuest001.Play SeqAllOff
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqUpOn, 50, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqDownOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqCircleOutOn, 15, 2
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqUpOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqDownOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqUpOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqDownOn, 25, 1
    LightQuest001.UpdateInterval = 10
    LightQuest001.Play SeqCircleOutOn, 15, 3
    LightQuest001.UpdateInterval = 5
    LightQuest001.Play SeqRightOn, 50, 1
    LightQuest001.UpdateInterval = 5
    LightQuest001.Play SeqLeftOn, 50, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqRightOn, 50, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqLeftOn, 50, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqRightOn, 40, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqLeftOn, 40, 1
    LightQuest001.UpdateInterval = 10
    LightQuest001.Play SeqRightOn, 30, 1
    LightQuest001.UpdateInterval = 10
    LightQuest001.Play SeqLeftOn, 30, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqRightOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqLeftOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqRightOn, 15, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqLeftOn, 15, 1
    LightQuest001.UpdateInterval = 10
    LightQuest001.Play SeqCircleOutOn, 15, 3
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqLeftOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqRightOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqLeftOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqUpOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqDownOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqUpOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqDownOn, 25, 1
    LightQuest001.UpdateInterval = 5
    LightQuest001.Play SeqStripe1VertOn, 50, 2
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqCircleOutOn, 15, 2
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqStripe1VertOn, 50, 3
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqLeftOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqRightOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqLeftOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqUpOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqDownOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqCircleOutOn, 15, 2
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqStripe2VertOn, 50, 3
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqLeftOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqRightOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqLeftOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqUpOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqDownOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqUpOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqDownOn, 25, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqStripe1VertOn, 25, 3
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqStripe2VertOn, 25, 3
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqUpOn, 15, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqDownOn, 15, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqUpOn, 15, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqDownOn, 15, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqUpOn, 15, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqDownOn, 15, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqRightOn, 15, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqLeftOn, 15, 1
    LightQuest001.UpdateInterval = 8
    LightQuest001.Play SeqRightOn, 15, 1
    End Sub


Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
	LightQuest001Seq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
	LightQuesttilt.Play SeqAllOff
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
Dim SkStep
Dim SpecialL1
Dim SpecialL2
Dim SpecialL3
Dim SpecialL4
Dim SpecialL5
Dim BallInLkicker
Dim BallInRkicker
Dim bLeftLockEnabled
Dim bRightLockEnabled
Dim Drop1, Drop2, Drop3
Dim DropValue


Sub Game_Init() 'called at the start of a new game
    Dim i, j
    DisableTable False
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
	TurnOffQuestLights()
    'Play some Music
    'ChangeSong
    'Init Variables
    bLeftLockEnabled = False
    bRightLockEnabled = False
    
	BallsInRLock = 0
    BallsInLLock = 0
    rampdiverter.IsDropped = 1
	HolePos(CurrentPlayer)= 0
	HolePos1(CurrentPlayer) = 0
	Holepos2(CurrentPlayer) = 0
	'Bridgepos = 2
	'movebridge.Enabled = True
	'up.enabled = true
	pDMDStartGame
    Target001.isDropped = 0:PlaySoundAt "fx_resetdrop", target001
'Init Delays/Timers
'MainMode Init()
'Init lights
End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained, reset skillshot, modes, timers
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
    SpecialL1 = 0
    SpecialL2 = 0
    SpecialL3 = 0
    SpecialL4 = 0
    SpecialL5 = 0
    ResetDropTargets
    DropValue = 1000
    CloseGate
	'CloseGate1

End Sub

Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
    TurnOffPlayfieldLights
    li045.State = 1
    li047.State = 1
    li031.State = 2
    If(BallsInRLock + BallsInLLock)> 0 Then LitCenterLights
    If bRightLockEnabled Then li046.State = 2
    li036.State = 1:li037.State = 1:li038.State = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub TurnOffQuestLights()
	Dim a
	For each a in aQuest
		a.State = 0
	Next
End Sub


'************
' SKILLSHOT
'************

Sub StartSkillShot() 'Setup and updates the skillshot lights
    SkStep = INT(3 * RND(1))
    SkillShotTimer.Interval = 1000
    SkillShotTimer.Enabled = 1
    DMD CL(0, "HIT LIT LIGHT"), CL(1, "FOR SKILLSHOT"), "", eNone, eNone, eNone, 1500, True, ""
	pupDMDDisplay "attract","HIT LIT LIGHT^FOR SKILLSHOT", "", 2, 0, 10
	FormatScore(SkillshotValue(SkStep))
End Sub

Sub SkillShotTimer_Timer
    SkStep = (SkStep + 1)MOD 3
    If SkillShotTimer.Interval> 200 Then
        SkillShotTimer.Interval = SkillShotTimer.Interval -25
    End If
   Select Case SkStep
        Case 0:li025.State = 0:li026.State = 0:li027.State = 1
        Case 1:li025.State = 0:li026.State = 1:li027.State = 0
        Case 2:li025.State = 1:li026.State = 0:li027.State = 0
    End Select
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

'*******************
' 10 points rubbers
'*******************

'Sub rsband002_Hit:Addscore 10:End Sub
Sub rlband004_Hit:Addscore 10:End Sub
Sub rlband006_Hit:Addscore 10:End Sub
Sub rlband003_Hit:Addscore 10:End Sub
'Sub rlband001_Hit:Addscore 10:PlaySound"fx_rubber_post":End Sub
Sub rlband007_Hit:Addscore 10:PlaySound"fx_rubber_post":End Sub
'*********************
' SlingShot
'*********************

' Slingshots has been hit
' In this table the slingshots change the outlanes lights

Dim LStep, RStep

Sub LeftSlingShot_Slingshot()
    If Tilted Then Exit Sub
    'PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lpouss
    'DOF 103,2 'DOF Solenoid/MX
    LeftSling004.Visible = 1
	'FlashForMs gi002, 2000, 50, 0
    Lpouss.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    ' add some points
    AddScore 200
    ' check modes
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    'LastSwitchHit = "LeftSlingShot"
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSling004.Visible = 0:LeftSling003.Visible = 1:Lpouss.RotX = 14
        Case 2:LeftSling003.Visible = 0:LeftSling002.Visible = 1:Lpouss.RotX = 0
        Case 3:LeftSling002.Visible = 0:Lpouss.RotX = -20:LeftSlingShot.TimerEnabled = 1
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot()
    If Tilted Then Exit Sub
    'PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFcontactors), Rpouss
    'DOF 104,2 'DOF Solenoid/MX
    RightSling004.Visible = 1
	'FlashForMs gi016, 2000, 50, 0
    Rpouss.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    ' add some points
    AddScore 200
    ' check modes
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    'LastSwitchHit = "RightSlingShot"
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSling004.Visible = 0:RightSling003.Visible = 1:Rpouss.RotX = 14
        Case 2:RightSling003.Visible = 0:RightSling002.Visible = 1:Rpouss.RotX = 0
        Case 3:RightSling002.Visible = 0:Rpouss.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*********
' Bumpers
'*********

Sub Bumper1_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 103, DOFPulse, DOFContactors), Bumper1
        DOF 108,1
        ' add some points
		FlashForMs bumper1L, 2000, 50, 0
		
        Addscore 500 * Bumper1L.State
        PlaySound"fx_bumper"
    End If
End Sub

Sub Bumper2_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 104, DOFPulse, DOFContactors), Bumper2
        DOF 109,1
        FlashForMs bumper2L, 2000, 50, 0
        Addscore 500 * Bumper2L.State
        PlaySound"fx_bumper"
    End If
End Sub

Sub Bumper3_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 105, DOFPulse, DOFContactors), Bumper3
        DOF 110,1
		FlashForMs bumper3L, 2000, 50, 0
        ' add some points
		Addscore 500 * Bumper3L.State
        PlaySound"fx_bumper"
    End If
End Sub

'***********
' Rollovers
'***********

Sub Trigger005_Hit 'Skillshot trigger
    'DOF 128, DOFPulse
    PlaySoundAt "chouette", Trigger005
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        StopSkillShot
    End If
End Sub

Sub Trigger001_Hit 'right inlane
    'DOF 128, DOFPulse
    'PlaySoundAt "hoho1", Trigger001
    RandomTrigger13
	Sling
	If Tilted Then Exit Sub
    'PlaySound"Top&InlaneRolloversLaser"
    Addscore 1000 + 29000 * SpecialL5
    AddBonus 1
    Flashforms li021, 500, 50, 1
    SpecialL5 = 1
    CheckSpecial
End Sub

Sub Trigger002_Hit 'right outlane
    'DOF 128, DOFPulse
    If ( bBallSaverReady = False) Then pupevent 801: End If
	RandomTrigger24
    If Tilted Then Exit Sub
    Addscore 40000
    If li002.State = 1 Then
        AwardSpecial
    Else
        PlaySound ""
    End IF
    vpmtimer.Addtimer 2000, "CloseGate '"
End Sub

Sub Trigger003_Hit 'left inlane
    'DOF 128, DOFPulse
    'PlaySoundAt "hoho", Trigger003
	RandomTrigger13
	Sling
    If Tilted Then Exit Sub
    PlaySound""
    Addscore 1000 + 29000 * SpecialL4
    AddBonus 1
    Flashforms li020, 500, 50, 1
    SpecialL4 = 1
    CheckSpecial
End Sub

Sub Trigger004_Hit 'left outlane
    'DOF 128, DOFPulse
    
	If (bBallSaverReady = False) THen pupevent 801: End If
	RandomTrigger24
    If Tilted Then Exit Sub
    Addscore 40000
    If li001.State = 1 Then
        AwardSpecial
    Else
        PlaySound ""
    End IF
End Sub

Sub Trigger006_Hit 'right kicker sensor
    'DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
    PlaySound ""
    AddScore 1000
    If BallsinRlock = 1 Then EjectRightkicker            'although this should never happen
    If bRightLockEnabled Then rampdiverter.IsDropped = 0 'next ball will go to the left kicker though the ramp.
End Sub

Sub Trigger007_Hit                                       'right loop
    'DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger007
    If Tilted Then Exit Sub
    PlaySound"bat"
    AddScore 1000 + 9000 * li047.State
    li047.State = 1
End Sub

Sub Trigger008_Hit 'top right lane
    'DOF 128, DOFPulse
    PlaySoundAt "chouette", Trigger008
	'RandomTrigger8910
    If Tilted Then Exit Sub
    'PlaySound"Top&InlaneRolloversLaser"
    If bSkillShotReady Then
        AwardSkillshot
    Else
        Addscore 1000 + 14000 * SpecialL1
        AddBonus 1
    End If
    Flashforms li035, 500, 50, 1
    SpecialL1 = 1
    CheckSpecial
End Sub

Sub Trigger009_Hit ' top center lane
    'DOF 128, DOFPulse
    PlaySoundAt "chouette", Trigger009
	'RandomTrigger8910 
    If Tilted Then Exit Sub
   ' PlaySound"Top&InlaneRolloversLaser"
    If bSkillShotReady Then
        AwardSkillshot
    Else
        Addscore 1000 + 14000 * SpecialL2
        AddBonus 1
    End If
    Flashforms li034, 500, 50, 1
    SpecialL2 = 1
    CheckSpecial
End Sub

Sub Trigger010_Hit 'top left lane
    'DOF 128, DOFPulse
    PlaySoundAt "chouette", Trigger010
	'RandomTrigger8910 
    If Tilted Then Exit Sub
    'PlaySound"Top&InlaneRolloversLaser"
    If bSkillShotReady Then
        AwardSkillshot
    Else
        Addscore 1000 + 14000 * SpecialL3
        AddBonus 1
    End If
    Flashforms li033, 500, 50, 1
    SpecialL3 = 1
    CheckSpecial
End Sub

Sub Trigger011_Hit 'left loop
    'DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger011
    If Tilted Then Exit Sub
    PlaySound"Bell"
    AddScore 1000 + 9000 * li045.State
    li045.State = 1
End Sub

Sub Trigger012_Hit 'left kicker sensor
    'DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger012
    If Tilted Then Exit Sub
    PlaySound ""
    AddScore 1000
    If BallsinLlock = 1 Then EjectLeftkicker
End Sub

Sub Trigger014_Hit 
	PlaySoundAt "thunder2", Trigger014
	'pupevent 819  'video: Dirk regenerates
End Sub

Sub Trigger013_Hit
	Screen002.visible = True
	'Pupevent 819    'video: Dirk regenerates
	Screen002.visible = False
End Sub

' Droptargets
Sub TargetQuest_Hit
	AddScore 1000
	PlaySoundAt "lazer", TargetQuest
	If Tilted Then Exit Sub
	IncrementHoleLights
	'animationflamme
	
End Sub 

Sub TargetQuest001_Hit
	AddScore 1000
	PlaySoundAt "lazer", TargetQuest001
	If Tilted Then Exit Sub
	IncrementHoleLights2
	'animationflamme
	
End Sub 

Sub Target001_Hit 'left ramp droptarget
    PlaySoundAt "lazer", Target001
    If Tilted Then Exit Sub
    PlaySound"DropTargets"
    bLeftLockEnabled = True
    Addscore 30000
End Sub

Sub Target002_Hit 'top right droptarget
    PlaySoundAtBall "lazer"
    If Tilted Then Exit Sub
    PlaySound"DropTargets"
    bumper1L.State = 1
    Addscore DropValue
    Drop1 = 1
    CheckDroptgs
End Sub

Sub Target003_Hit 'center right droptarget
    PlaySoundAtBall "lazer"
    If Tilted Then Exit Sub
    PlaySound"DropTargets"
    bumper2L.State = 1
    Addscore DropValue
    Drop2 = 1
    CheckDroptgs
End Sub

Sub Target004_Hit 'lower right droptarget
    PlaySoundAtBall "lazer"
    If Tilted Then Exit Sub
    PlaySound"DropTargets"
    bumper3L.State = 1
    Addscore DropValue
    Drop3 = 1
    CheckDroptgs
End Sub

Sub ResetDroptargets
    Drop1 = 0
    Drop2 = 0
    Drop3 = 0
    PlaySoundat "fx_resetdrop", Target003
    Target002.IsDropped = 0
    Target003.IsDropped = 0
    Target004.IsDropped = 0
End Sub

Sub CheckDroptgs
    If Drop1 + Drop2 + Drop3 = 3 Then
        Select case Dropvalue
            Case 1000:DropValue = 5000:li024.State = 1:AddBonusMultiplier 1
            Case 5000:DropValue = 10000:li023.State = 1:AddBonusMultiplier 1
            Case 10000:DropValue = 5000:li022.State = 1:AddBonusMultiplier 1
            Case Else:AddBonusMultiplier 1
        End Select
        vpmtimer.addtimer 5000, "ResetDroptargets '" 'wait 5 seconds to reset droptargets so you may hit the target behind to open the right gate
    End If
End Sub

' Standup targets

Sub Target005_Hit 'behind droptargets
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    'PlayRoar
    Addscore 1000
    DiverterF.RotateToEnd
    PlaySoundAt "fx_SolenoidON", DiverterF
    PlaySound"RightGateTarget"
End Sub

'Sub Opengate
	'DiverterF001.RotateToStart
	'PlaySoundAt "open_door", DiverterF001
'End Sub


'Sub CloseGate1
	'DiverterF001.RotateToEnd
	'PlaySoundAt "fx_SolenoidOff", DiverterF001
'End Sub

Sub CloseGate
    DiverterF.RotateToStart
    PlaySoundAt "fx_SolenoidOff", DiverterF
    'PlaySound"door_close"
End Sub

Sub Target006_Hit 'first green target
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    PlaySound"LeftTargets"
    AddBonus 1
    If li031.State = 1 Then
        Addscore 30000
    ElseIf li031.State = 2 Then 'next green target
        li031.State = 1
        li030.State = 2
        Addscore 1000
    End If
    If li032.State = 2 Then
        li032.State = 0
        AwardExtraBall
    End If
End Sub

Sub Target007_Hit '2nd green target
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    PlaySound"LeftTargets"
    AddBonus 1
    If li030.State = 1 Then
        Addscore 30000
    ElseIf li030.State = 2 Then 'next green target
        li030.State = 1
        li029.State = 2
        Addscore 1000
    End If
End Sub

Sub Target008_Hit '3rd green target
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    PlaySound"LeftTargets"
    AddBonus 1
    If li029.State = 1 Then
        Addscore 30000
    ElseIf li029.State = 2 Then 'next green target
        li029.State = 1
        li028.State = 2
        Addscore 1000
    End If
End Sub

Sub Target009_Hit '4th green target
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    PlaySound"LeftTargets"
    AddBonus 1
    If li028.State = 1 Then
        Addscore 30000
    ElseIf li028.State = 2 Then 'next green target
        li028.State = 1
        li032.State = 2         'lit extraball
        DMD "_", CL(1, ("EXTRA BALL IS LIT")), "_", eNone, eBlink, eNone, 1500, True, ""
        Addscore 1000
    End If
End Sub

' Special

Sub CheckSpecial
    If SpecialL1 + SpecialL2 + SpecialL3 + SpecialL4 + SpecialL5 = 5 Then
        DMD "_", CL(1, "SPECIAL IS LIT"), "", eNone, eBlink, eNone, 1500, True, ""
		pupDMDDisplay "SPECIAL","SPECIAL IS LIT", "", 2, 0, 10
        li001.State = 1
        li002.State = 1
    End If
End Sub

'*******************
' Holes - kickers
'*******************

Sub kicker001_hit 'left kicker
    PlaySoundAt "fx_kicker_enter", kicker001
    If Tilted Then EjectLeftkicker:Exit Sub
    iF BallsInLLock = 1 Then Exit Sub
    If bLeftLockEnabled Then
        GiEffect 2
        AddMultiball 1
        LitCenterLights
        BallsInLLock = 1
        DMD CL(0, "BALL LEFT LOCKED"), CL(1, "SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, ""
		pupDMDDisplay "BALLOCK","BALL LEFT LOCKED^SHOOT AGAIN", "", 2, 0, 10
    Else
        EjectLeftkicker
    End If
	'If CreateMultiballTimer.Enabled = True then
	'	Superjkp.enabled= True
	'	AwardSuperJackpot
	'	PlaySound "thunder2"
	'	EjectLeftkicker
	'Else
		'EjectLeftkicker
	'End If
End Sub

Sub EjectLeftkicker
    PlaySoundAt "fx_kicker", kicker001
    kicker001.kick 90, 10
End Sub

Sub kicker002_hit 'right kicker
    PlaySoundAt "fx_kicker_enter", kicker002
    If Tilted Then EjectRightkicker:Exit Sub
    If BallsInRLock = 1 Then Exit Sub
    If bRightLockEnabled Then
        GiEffect 2
        AddMultiball 1
        LitCenterLights
        BallsInRLock = 1
        DMD CL(0, "BALL RIGHT LOCKED"), CL(1, "SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, ""
		pupDMDDisplay "BALLOCK","BALL RIGHT LOCKED^SHOOT AGAIN", "", 2, 0, 10
    Else
        bRightLockEnabled = True
        li046.State = 2
        EjectRightkicker
    End If
	'If CreateMultiballTimer.Enabled = True  then
		'Superjkp.enabled = True
		'PlaySound "thunder2"
		'AwardSuperJackpot
		'EjectRightkicker
	'Else
		'EjectRightkicker
	'End If
End Sub

Sub EjectRightkicker
    PlaySoundAt "fx_kicker", kicker001
    kicker002.kick -90, 10
End Sub

Sub kicker003_hit 'center kicker
    PlaySoundAt "fx_kicker_enter", kicker003
    If Tilted Then EjectCenterkicker:Exit Sub
    flashforms flasher001, 2000, 50, 0
    flashforms flasher002, 2000, 50, 0
    If BallsInRLock + BallsInLLock> 0 Then '1 or 2 balls are locked then start multiball
        bMultiBallMode = True
		
		'CloseGate1
		Bridgepos = 3
		'wallbridge.collidable = 0
        DMD "", "", "MULTIBALL", eNone, eNone, eBlink, 2000, True, ""
		pupDMDDisplay "MULTIBL","MULTIBALL", "", 2, 0, 10
        vpmtimer.addtimer 2000, "EjectCenterkicker '"
        vpmtimer.addtimer 2500, "EjectRightkicker '"
        vpmtimer.addtimer 3500, "EjectLeftkicker '"
        BallsInRLock = 0
        BallsInLLock = 0
        GiEffect 3
		DOF 119, 2 'Strobe
		pupevent 818
    Else
        vpmtimer.addtimer 1000, "EjectCenterkicker '"
    End If
    If bMultiBallMode Then
        AwardJackpot
        PlaySound "thunder2"
    Else
        PlaySound "Scream2"
    End If
End Sub

Sub EjectCenterkicker
    PlaySoundAt "fx_kicker", kicker003
    kicker003.kick 190, 10
End Sub

Sub LitCenterLights
    'li039.State = 2
    'li040.State = 2
    'li041.State = 2
    'li042.State = 2
    li043.State = 2
    li044.State = 2
End Sub

'*************
' Play Effects
'*************

'Sub PlayRoar
'    Dim tmp
'    tmp = INT(5 * RND(1))
'    Select Case tmp
'        Case 0:PlaySound "music2"
'        Case 1:PlaySound "music3"
'        Case 2:PlaySound "music4"
'        Case 3:PlaySound "begin-tune"
'        Case 4:PlaySound ""
''    End Select
'End Sub



'**************************
' Upper Lanes & loop switch
'**************************

Dim HolePos(4)
Dim HolePos1(4)
Dim Holepos2(4)

Sub animationflamme
	
	Animgate1.Enabled = True
	hitquest.Enabled = False
	Hitflasher.visible = False
	'questflasher.visible = False
	
End Sub

Sub bonusquest
	AddScore 15000
	DMD CL(0, "BONUS LIGHT DAPHNE"), CL(1, "+ 15000"), "", eNone, eBlink, eNone, 1500, True, "begin-tune"
	pupDMDDisplay "BONUS","BONUS LIGHT DAPHNE", "", 2, 1, 10
	GiEffect 1
	
End Sub

Sub IncrementHoleLights()
    If HolePos(CurrentPlayer) <> 7 Then
        HolePos(CurrentPlayer) = HolePos(CurrentPlayer) + 1
        If HolePos(CurrentPlayer) = 7 Then
            HolePos(CurrentPlayer) = 1
			bonusquest
		
        End If
    End If
    UpdateHoleLights()
End Sub

Sub UpdateHoleLights()
    Select Case HolePos(CurrentPlayer)
        Case 0:HoleLight1.State = 0:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 0: HoleLight5.State = 0: HoleLight6.State = 0
        Case 1:HoleLight1.State = 2:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 0: HoleLight5.State = 0: HoleLight6.State = 0
        Case 2:HoleLight1.State = 2:HoleLight2.State = 2:HoleLight3.State = 0:HoleLight4.State = 0: HoleLight5.State = 0: HoleLight6.State = 0
        Case 3:HoleLight1.State = 2:HoleLight2.State = 2:HoleLight3.State = 2:HoleLight4.State = 0: HoleLight5.State = 0: HoleLight6.State = 0
        Case 4:HoleLight1.State = 2:HoleLight2.State = 2:HoleLight3.State = 2:HoleLight4.State = 2: HoleLight5.State = 0: HoleLight6.State = 0
		Case 5:HoleLight1.State = 2:HoleLight2.State = 2:HoleLight3.State = 2:HoleLight4.State = 2: HoleLight5.State = 2: HoleLight6.State = 0
		Case 6:HoleLight1.State = 2:HoleLight2.State = 2:HoleLight3.State = 2:HoleLight4.State = 2: HoleLight5.State = 2: HoleLight6.State = 2
		End Select
End Sub

Sub bonusquest2
	AddScore 7000
	DMD CL(0, "BONUS LIGHT DIRK"), CL(1, "+ 7000"), "", eNone, eBlink, eNone, 1500, True, "begin-tune"
	pupDMDDisplay "BONUS","BONUS LIGHT DIRK", "", 2, 1, 10
	GiEffect 1
	
End Sub

Sub IncrementHoleLights2()
    If HolePos2(CurrentPlayer) <> 5 Then
        HolePos2(CurrentPlayer) = HolePos2(CurrentPlayer) + 1
        If HolePos2(CurrentPlayer) = 5 Then
            HolePos2(CurrentPlayer) = 1
			bonusquest2
		
        End If
    End If
    UpdateHoleLights2()
End Sub

Sub UpdateHoleLights2()
    Select Case HolePos2(CurrentPlayer)
        Case 0:Dirk1.State = 0:Dirk2.State = 0:Dirk3.State = 0:Dirk4.State = 0
        Case 1:Dirk1.State = 2:Dirk2.State = 0:Dirk3.State = 0:Dirk4.State = 0
        Case 2:Dirk1.State = 2:Dirk2.State = 2:Dirk3.State = 0:Dirk4.State = 0
        Case 3:Dirk1.State = 2:Dirk2.State = 2:Dirk3.State = 2:Dirk4.State = 0
        Case 4:Dirk1.State = 2:Dirk2.State = 2:Dirk3.State = 2:Dirk4.State = 2
		End Select
End Sub
Sub IncrementHoleLights1()
    If HolePos1(CurrentPlayer) <> 16 Then
        HolePos1(CurrentPlayer) = HolePos1(CurrentPlayer) + 1
        If HolePos1(CurrentPlayer) = 16 Then HolePos1(CurrentPlayer) = 0
		
        End If
    UpdateHoleLights1()
End Sub

Sub UpdateHoleLights1()
    Select Case HolePos1(CurrentPlayer)
        Case 0:m1.State = 0:m2.State = 0:m3.State = 0:m4.State = 0: m5.State = 0:m6.State = 0:m7.State = 0:m8.State = 0:m9.State = 0: m10.State = 0
        Case 1:m1.State = 2:m2.State = 0:m3.State = 0:m4.State = 0: m5.State = 0:m6.State = 0:m7.State = 0:m8.State = 0:m9.State = 0
        Case 2:m1.State = 2:m2.State = 2:m3.State = 0:m4.State = 0: m5.State = 0:m6.State = 0:m7.State = 0:m8.State = 0:m9.State = 0
        Case 3:m1.State = 2:m2.State = 2:m3.State = 2:m4.State = 0: m5.State = 0:m6.State = 0:m7.State = 0:m8.State = 0:m9.State = 0
        Case 4:m1.State = 2:m2.State = 2:m3.State = 2:m4.State = 2: m5.State = 0:m6.State = 0:m7.State = 0:m8.State = 0:m9.State = 0
		Case 5:m1.State = 2:m2.State = 2:m3.State = 2:m4.State = 2: m5.State = 2:m6.State = 0:m7.State = 0:m8.State = 0:m9.State = 0
		Case 6:m1.State = 2:m2.State = 2:m3.State = 2:m4.State = 2: m5.State = 2:m6.State = 0:m7.State = 0:m8.State = 0:m9.State = 0
		Case 7:m1.State = 2:m2.State = 2:m3.State = 2:m4.State = 2: m5.State = 2:m6.State = 0:m7.State = 0:m8.State = 0:m9.State = 0
		Case 8:m1.State = 2:m2.State = 2:m3.State = 2:m4.State = 2: m5.State = 2:m6.State = 2:m7.State = 0:m8.State = 0:m9.State = 0
		Case 9:m1.State = 2:m2.State = 2:m3.State = 2:m4.State = 2: m5.State = 2:m6.State = 2:m7.State = 2:m8.State = 0:m9.State = 0
		Case 10:m1.State = 2:m2.State = 2:m3.State = 2:m4.State = 2: m5.State = 2:m6.State = 2:m7.State = 2:m8.State = 2:m9.State = 0
		Case 11:m1.State = 2:m2.State = 2:m3.State = 2:m4.State = 2: m5.State = 2:m6.State = 2:m7.State = 2:m8.State = 2:m9.State = 2
		Case 12:m1.State = 2:m2.State = 2:m3.State = 2:m4.State = 2: m5.State = 2:m6.State = 2:m7.State = 2:m8.State = 2:m9.State = 2
		Case 13:m1.State = 2:m2.State = 2:m3.State = 2:m4.State = 2: m5.State = 2:m6.State = 2:m7.State = 2:m8.State = 2:m9.State = 2
		Case 14:m1.State = 3:m2.State = 3:m3.State = 3:m4.State = 3: m5.State = 3:m6.State = 3:m7.State = 3:m8.State = 3:m9.State = 3: m10.State = 3
		End Select
End Sub
Sub ResetHoleLights()
	'Checklightquest()
'HolePos = 0
	'HolePos1 =0
    UpdateHoleLights
End Sub

'***************************
' Catch Hole /Catch'em Mode
'***************************

' Start always first the Cath'em mode if the light is blinking other wise gives a random award if one of the other lights are blinking.
' Catch'em mode ends after 2 minutes

Dim bCatchemMode, CatchID, CatchMaxHits, CatchHits, BallInHole

Dim aBall

Sub CatchHole_Timer
    Do While aBall.Z> 0
        aBall.Z = aBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
End Sub

Sub CatchHole_Hit
    If Tilted Then CatchHoleExit:Exit Sub
	'PlaySoundAt "thunder2", CatchHole
    BallInHole = BallInHole + 1
    Set aBall = ActiveBall:Me.TimerEnabled = 1
    If Bridgepos= 1 Then
		Mission
		storm1.Enabled = True
		Stormflasher.visible = True
	Else
        vpmtimer.addtimer 500, "FlashForMs FlasherExitHole, 1000, 30, 0 '"
        vpmtimer.addtimer 1000, "CatchHoleExit '"
		End If
	'If CreateMultiballTimer.Enabled = True Then 
		'Superjkp.Enabled = True
		'Jackpotrampquest
	'End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "CatchHole"
	
End Sub

Sub CatchHoleExit()
    ' Clear both mini-game video layers when the ball returns to play.
    If HasPup And EnablePupDmd And PUPStatus Then
        PuPlayer.playstop pDMD
        PuPlayer.playstop 16
        pDMDVideoPlaying = False
        PriorityReset = -1
        pDMDCurPriority = -1
        If Not pInAttract Then
            pDMDSetPage pScores
            pUpdateScores
        End If
    End If
    If BallInHole> 0 Then
        BallInHole = BallInHole - 1
        CatchHole1.CreateSizedball BallSize / 2
        'UpdateBallImage
        PlaySoundAt SoundFXDOF("fx_popper", 120, DOFPulse, DOFContactors), CatchHole
        DOF 121, DOFPulse
        CatchHole1.Kick 175, 14, 1
       ' vpmtimer.addtimer 3000, "CatchHoleExit '" 'repeat until all the balls are kicked out
    End If
	quest.visible = False
	Screen.visible = False
	Screen001.visible = False
	Screen002.visible = False
End Sub
Sub vp100()
	If BallInHole> 0 Then
        BallInHole = BallInHole - 1
        vp10.CreateSizedball BallSize / 2
        'UpdateBallImage
        'PlaySoundAt SoundFXDOF("fx_popper", 120, DOFPulse, DOFContactors), CatchHole
        'DOF 121, DOFPulse
        vp10.kick 175, 5,1
       ' vpmtimer.addtimer 3000, "CatchHoleExit '" 'repeat until all the balls are kicked out
    End If
End Sub

Sub Jackpotrampquest
	AwardSuperJackpot
	vpmtimer.addtimer 1000, "CatchHoleExit '"
End Sub

'***********************
'		BridgeQuest	
'***********************
Dim Bridgepos


Sub movebridge_Timer
	Select Case Bridgepos
		Case 1 : 'down Bridge
			Bridge.rotx = Bridge.rotx-1
			If Bridge.rotx <= -90 Then
			bridge.rotx = -90
			
				
				wallbridge.collidable = 0
				
			End If
					
		Case 2 : 'Up Bridge
			Bridge.rotx = Bridge.rotx+1
			If Bridge.rotx >= 0 Then
				Bridge.rotx = 0
				
				wallbridge.collidable = 1
			End If
		Case 3 : 
			Bridge.rotx = Bridge.rotx+1
			If Bridge.rotx >= 0 Then
				Bridge.rotx = 0
				
				wallbridge.collidable = 0
			End If
	End Select
End Sub

'Dim Ramppos
	
'Sub moveramp_timer
'	Select Case Ramppos
'		Case 1 : ' upramp
		'Ramp023.heighttop = Ramp023.heighttop + 1
'		Ramp023.visible = 1
'		Ramp023.collidable = 1
		

'		Case 2 : 'dwramp
		'Ramp023.heighttop = Ramp023.heighttop - 1
'		Ramp023.visible = 0
'		Ramp023.collidable = 0
		
'	End Select
'End Sub


Sub wallbridge_Hit
	animationflamme
	PlaySound "bruitage save me"
	Bridgepos = 1
	pupDMDDisplay "attract", "The quest begins", "", 3, 0, 10
'	moveramp.Enabled = True
	movebridge.Enabled = True
	playsound "pont-levis": playsound "pont-levis2"
End Sub
				
				
			
	
	
'**************
' SlotMachine
'**************

Dim DLQuest, DLQuest1, DLQuest2, DLQuest3, DLQuest4

'StopAttractMode
DLQuest = Array("quest1", "quest2", "quest3", "quest4", "quest5", "quest6", "quest7", "quest8", "quest9", "quest10", "quest11", "quest12", "quest13", "quest14", "Ballsaved10", "ballsaved20")
DLQuest1 = Array("quest1", "quest2", "quest3", "quest4", "quest5", "quest6", "quest7", "quest8", "quest9", "quest10", "quest11", "quest12", "quest13", "quest14", "Ballsaved10", "ballsaved20")
DLQuest2 = Array("quest1", "quest2", "quest3", "quest4", "quest5", "quest6", "quest7", "quest8", "quest9", "quest10", "quest11", "quest12", "quest13", "quest14", "Ballsaved10", "ballsaved20")
DLQuest3 = Array("quest1", "quest2", "quest3", "quest4", "quest5", "quest6", "quest7", "quest8", "quest9", "quest10", "quest11", "quest12", "quest13", "quest14", "Ballsaved10", "ballsaved20")
DLQuest4 = Array("quest1", "quest2", "quest3", "quest4", "quest5", "quest6", "quest7", "quest8", "quest9", "quest10", "quest11", "quest12", "quest13", "quest14", "Ballsaved10", "ballsaved20")

Sub StartSlotmachine() ' uses the HolePos variable
    pupDMDDisplay "SLOT","SLOT", "@slot.mp4", 5, 0, 10
	Dim i
    
    DMDFlush
    Select Case HolePos1(CurrentPlayer)
         Case 1: 'DLQuest
            For i = 0 to 15
                DMD "", "", DLQuest(i), eNone, eNone, eNone, 150, False, "fx_spinner"
            Next
        Case 2: 'DLQuest1
            For i = 0 to 15
                DMD "", "", DLQuest1(i), eNone, eNone, eNone, 150, False, "fx_spinner"
            Next
        Case 3: 'DLQuest2
            For i = 0 to 15
                DMD "", "", DLQuest2(i), eNone, eNone, eNone, 150, False, "fx_spinner"
            Next
        Case 4: 'DLQuest3
            For i = 0 to 15
                DMD "", "", DLQuest3(i), eNone, eNone, eNone, 150, False, "fx_spinner"
            Next
		Case 5: 'DLQuest4
            For i = 0 to 15
                DMD "", "", DLQuest4(i), eNone, eNone, eNone, 150, False, "fx_spinner"
            Next
    End Select
    DOF 142, DOFPulse
    vpmtimer.AddTimer 2500, "GiveSlotAward '"
End Sub

Sub GiveSlotAward()
    Dim tmp
    DMDFlush
    tmp = INT(RND * 13)

    Select Case HolePos1(CurrentPlayer)
        Case 1: 'DLQuest
            DMD "", "", DLQuest(tmp), eNone, eNone, eBlinkFast, 800, True, "po_fanfare6"
        Case 2: 'DLQuest1
            DMD "", "", DLQuest1(tmp), eNone, eNone, eBlinkFast, 800, True, "po_fanfare6"
        Case 3: 'DLQuest2
            DMD "", "", DLQuest2(tmp), eNone, eNone, eBlinkFast, 800, True, "po_fanfare6"
        Case 4: 'DLQuest3
            DMD "", "", DLQuest3(tmp), eNone, eNone, eBlinkFast, 800, True, "po_fanfare6"
		Case 5: 'DLQuest4
            DMD "", "", DLQuest4(tmp), eNone, eNone, eBlinkFast, 800, True, "po_fanfare6"
    End Select

    Select Case tmp 'test Quest1
		Case 0:Quest1' first bridge
		Case 1:Quest2' door
		Case 2:Quest3' hole
		Case 3:Quest4' Monkey
		Case 4:Quest5' Octopus
		Case 5:Quest6' water gate
		Case 6:Quest7' weapon room
		Case 7:Quest8' Bridge ball
		Case 8:Quest9' hole wall
		Case 9:BSD20'	EnableBallSaver 20
		Case 10:Quest11' electrik guardian
		Case 11:Quest12' Sword
		Case 12:Quest13' Boat
		'Case 13:BSD10'	EnableBallSaver 10
		'Case 14:BSD10' EnableBallSaver 10
		'Case 15:BSD20' EnableBallSaver 20
		
    End Select
    GiEffect 1
	
	storm1.Enabled= False
	Stormflasher.visible = False
	End Sub


'******************************************
'			Speed ball trigger
'******************************************
Dim M50, M51
M50 = 1
M51 = 1
Dim MyBall1

Sub TMag(Enabled)
    If Enabled Then
        M50 = 1
        'Timer2.Enabled = 0
        Timer2.Enabled = 1
    End If
End Sub
'Sub Timer2_Timer:M50 = 0:Timer2.Enabled = 0:End Sub

Sub Magnetquest_Hit
	If M50 = 1 Then
        Set MyBall1 = ActiveBall
        If MyBall1.VelY < 0 Then
            MyBall1.VelY = MyBall1.VelY * 1.5
            'If MyBall1.VelY < -60 Then
                'MyBall1.VelY = -60
            'Else
                'If MyBall1.VelY > -35 Then MyBall1.VelY = -35
            'End If
        End If
    End If
End Sub



Sub LMag(Enabled)
    If Enabled Then
        M51 = 1
        'Timer2.Enabled = 0
        Timer3.Enabled = 1
    End If
End Sub
'Sub Timer2_Timer:M50 = 0:Timer2.Enabled = 0:End Sub

Sub Sling
	If M51 = 1 Then
        Set MyBall1 = ActiveBall
        If MyBall1.VelY < 0 Then
            MyBall1.VelY = MyBall1.VelY / 20	
            'If MyBall1.VelY < -60 Then
                'MyBall1.VelY = -60
            'Else
                'If MyBall1.VelY > -35 Then MyBall1.VelY = -35
            'End If
        End If
    End If
End Sub

'*****************************************
'				Quest
'*****************************************

Sub Mission

	If HolePos1(CurrentPlayer) = 5 Then 
	Quest10
	Else
	If HolePos1(CurrentPlayer) = 6  Then
	Quest20	
	Else
	If HolePos1(CurrentPlayer) = 7  Then
	Quest21
	Else
	If HolePos1(CurrentPlayer) = 11 Then 
	Quest14
	Else	
	If HolePos1(CurrentPlayer) = 12 Then 
	Quest141
	Else
	If HolePos1(CurrentPlayer) = 13 Then
	Quest142
	Else
	StartSlotmachine
	End If
	End If
	End If
	End If
	End If
	End If
End Sub

Sub Quest1
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress1(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","quest1", "@Quest1.mp4", 7, 0, 10
	Pupevent 821
	Pupevent 621
	DMD "", "", "Quest1", eNone, eNone, eNone, 1000, True, "Quest1" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 3000, True, ""
End Sub

Sub Quest2
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress2(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","quest2", "@Quest2.mp4", 7, 0, 10
	Pupevent 824
	PuPEvent 621
	DMD "", "", "Quest2", eNone, eNone, eNone, 1000, True, "Quest2" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 3000, True, ""
End Sub

Sub Quest3
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress3(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","quest3", "@Quest3.mp4", 9, 0, 10
	Pupevent 827
	PuPEvent 621
	DMD "", "", "Quest3", eNone, eNone, eNone, 1000, True, "Quest3" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 5000, True, ""
End Sub

Sub Quest4
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress4(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","quest4", "@Quest4.mp4", 12, 0, 10
	Pupevent 830
	PuPEvent 621
	DMD "", "", "Quest4", eNone, eNone, eNone, 1000, True, "Quest4" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 8000, True, ""
End Sub

Sub Quest5
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress5(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","quest5", "@Quest5.mp4", 8, 0, 10
	Pupevent 833
	PuPEvent 621
	DMD "", "", "Quest5", eNone, eNone, eNone, 1000, True, "quest5" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 4000, True, ""
End Sub

Sub Quest6
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress6(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","quest6", "@Quest6.mp4", 7, 0, 10
	Pupevent 836
	PuPEvent 621
	DMD "", "", "Quest6", eNone, eNone, eNone, 1000, True, "Quest6" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 3000, True, ""
End Sub

Sub Quest7
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress7(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","quest7", "@Quest7.mp4", 20, 0, 10
	Pupevent 839
	PuPEvent 621
	DMD "", "", "Quest7", eNone, eNone, eNone, 1000, True, "Quest7" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 16000, True, ""
End Sub

Sub Quest8
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress8(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","quest8", "@Quest8.mp4", 14, 0, 10
	Pupevent 842
	PuPEvent 621
	DMD "", "", "Quest8", eNone, eNone, eNone, 1000, True, "Quest8" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 10000, True, ""
End Sub

Sub Quest9
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress9(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","quest9", "@Quest9.mp4", 6, 0, 10
	Pupevent 845
	PuPEvent 621
	DMD "", "", "Quest9", eNone, eNone, eNone, 1000, True, "Quest9" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 2000, True, ""
End Sub

Sub Quest10
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress10(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","Super^Quest", "@Superquest.mp4", 7, 0, 10
	Pupevent 848
	PuPEvent 621
	DMD "", "", "superquest", eNone, eNone, eNone, 1000, True, "Superquest" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 3000, True, ""
End Sub

Sub Quest20
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress20(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","loor", "@loor20.mp4", 4, 0, 10
	Pupevent 948
	PuPEvent 621
	'DMD "", "", "Next", eNone, eNone, eNone, 1000, True, "" 
	'DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 5000, True, ""
End Sub

Sub Quest21
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress21(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	Pupevent 950
	PuPEvent 621
	pupDMDDisplay "Quest","loor", "@loor20.mp4", 4, 0, 10
	
	'DMD "", "", "next", eNone, eNone, eNone, 1000, True, "" 
	'DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 5000, True, ""
End Sub

Sub Quest11
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress11(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","Quest11", "@Quest11.mp4", 7, 0, 10
	Pupevent 851
	PuPEvent 621
	DMD "", "", "Quest11", eNone, eNone, eNone, 1000, True, "Quest11" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 3000, True, ""
End Sub

Sub Quest12
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress12(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","quest12", "@Quest12.mp4", 7, 0, 10
	Pupevent 854
	PuPEvent 621
	DMD "", "", "Quest12", eNone, eNone, eNone, 1000, True, "Quest12" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 3000, True, ""
End Sub

Sub Quest13
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress13(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","quest13", "@Quest13.mp4", 11, 0, 10
	Pupevent 857
	PuPEvent 621
	DMD "", "", "Quest13", eNone, eNone, eNone, 1000, True, "Quest13" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 7000, True, ""
End Sub

Sub Quest14
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress14(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","finalquest", "@finalquest.mp4", 33, 0, 10
	Pupevent 860
	PuPEvent 621
	DMD "", "", "finalquest", eNone, eNone, eNone, 1000, True, "finalquest" 
	DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 30000, True, ""
End Sub

Sub Quest141
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress141(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	Pupevent 952
	PuPEvent 621
	pupDMDDisplay "Quest","loor", "@loor141.mp4", 20, 0, 10
	'DMD "", "", "next", eNone, eNone, eNone, 1000, True, "" 
	'DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 20000, True, ""
End Sub

Sub Quest142
    pClearQuestChoices
    bSongSelect = False
    QuestInProgress142(CurrentPlayer) = True
	'quest.visible = True
	Screen.visible = True
	Screen001.visible = True
	Screen002.visible = True
	pupDMDDisplay "Quest","loor", "@loor142.mp4", 10, 0, 10
	Pupevent 954
	PuPEvent 621
	'DMD "", "", "Next", eNone, eNone, eNone, 1000, True, "" 
	'DMD "", "", "destiny", eNone, eNone, eNone, 2000, True, "destiny"
	DMD "", "", "lorr", eNone, eNone, eNone, 9000, True, ""
End Sub

Sub BSD10
	DMD "", "", "Ballsaved10", eNone, eNone, eNone, 1000, True,""
	pupDMDDisplay "ballsv","BALL SAVED", "@bllsaved.mp4", 1, 0, 10
	EnableBallSaver 10   'ball save 10 seconds
	StopSkillShot
	CatchHoleExit
End Sub

Sub BSD20
	DMD "", "", "Ballsaved20", eNone, eNone, eNone, 1000, True,""
	pupDMDDisplay "ballsv","BALL SAVED", "@bllsaved.mp4", 1, 0, 10
	EnableBallSaver 20   'ball save 20 seconds
	StopSkillShot
	CatchHoleExit
End Sub

dim Animgate
sub Animgate1_Timer
'Playsound "door_open"
PFQUESTflasher001.visible = False
Animgate = Animgate + 1 : If Animgate > 41 then Stopanimgate: end If 
select case Animgate
				case 1 :PFQUESTflasher.ImageA="qb1":PFQUESTflasher.visible = True
				case 2 :PFQUESTflasher.ImageA="qb2"
				case 3 :PFQUESTflasher.ImageA="qb3"
				case 4 :PFQUESTflasher.ImageA="qb4"
				case 5 :PFQUESTflasher.ImageA="qb5"
				case 6 :PFQUESTflasher.ImageA="qb6"
				case 7 :PFQUESTflasher.ImageA="qb7"
				case 8 :PFQUESTflasher.ImageA="qb8"
				case 9 :PFQUESTflasher.ImageA="qb9"
				case 10 :PFQUESTflasher.ImageA="qb10"
				case 11 :PFQUESTflasher.ImageA="qb11"
				case 12 :PFQUESTflasher.ImageA="qb12"
				case 13 :PFQUESTflasher.ImageA="qb13"
				case 14 :PFQUESTflasher.ImageA="qb14"
				case 15 :PFQUESTflasher.ImageA="qb15"
				case 16 :PFQUESTflasher.ImageA="qb16"
				case 17 :PFQUESTflasher.ImageA="qb17"
				case 18 :PFQUESTflasher.ImageA="qb18"
				case 19 :PFQUESTflasher.ImageA="qb19"
				case 20 :PFQUESTflasher.ImageA="qb20"
				case 21 :PFQUESTflasher.ImageA="qb21"
				case 22 :PFQUESTflasher.ImageA="qb22"
				case 23 :PFQUESTflasher.ImageA="qb23"
				case 24 :PFQUESTflasher.ImageA="qb24"
				case 25 :PFQUESTflasher.ImageA="qb25"
				case 26 :PFQUESTflasher.ImageA="qb26"
				case 27 :PFQUESTflasher.ImageA="qb27"
				case 28 :PFQUESTflasher.ImageA="qb28"
				case 29 :PFQUESTflasher.ImageA="qb29"
				case 30 :PFQUESTflasher.ImageA="qb30"
				case 31 :PFQUESTflasher.ImageA="qb31"
				case 32 :PFQUESTflasher.ImageA="qb32"
				case 33 :PFQUESTflasher.ImageA="qb33"
				case 34 :PFQUESTflasher.ImageA="qb34"
				case 35 :PFQUESTflasher.ImageA="qb35"
				case 36 :PFQUESTflasher.ImageA="qb36"
				case 37 :PFQUESTflasher.ImageA="qb37"
				case 38 :PFQUESTflasher.ImageA="qb38"
				case 39 :PFQUESTflasher.ImageA="qb39"
				case 40 :PFQUESTflasher.ImageA="qb40"
				case 41 :PFQUESTflasher.ImageA="qb41"
				
			end Select
	
	
End Sub

Sub Stopanimgate
	
	'opengate
	
	Animgate = 0
	Animgate1.Enabled = False
	PFQUESTflasher.visible = False
	PFQUESTflasher001.visible = True
End Sub

'*************************************
'			Random audio
'*************************************

Sub RandomTrigger8910()
	PlaySound("dragonlair_" & Int(Rnd*3)+1)
End Sub

Sub RandomTrigger13()
	PlaySound("hoho" & Int(Rnd*3)+1)
End Sub
Sub RandomTrigger24()
	PlaySound("cri" & Int(Rnd*6)+1)
End Sub


' ************************************
' 			Video in arcade
' ************************************
dim VBorne
sub videoborne1_Timer
vBorne = vBorne + 1 : If VBorne > 65 then Vborne = 1: end If 
select case vBorne
				case 1 :videoborneflasher.ImageA="vb01"
				case 2 :videoborneflasher.ImageA="vb02"
				case 3 :videoborneflasher.ImageA="vb03"
				case 4 :videoborneflasher.ImageA="vb04"
				case 5 :videoborneflasher.ImageA="vb05"
				case 6 :videoborneflasher.ImageA="vb06"
				case 7 :videoborneflasher.ImageA="vb07"
				case 8 :videoborneflasher.ImageA="vb08"
				case 9 :videoborneflasher.ImageA="vb09"
				case 10 :videoborneflasher.ImageA="vb10"
				case 11 :videoborneflasher.ImageA="vb11"
				case 12 :videoborneflasher.ImageA="vb12"
				case 13 :videoborneflasher.ImageA="vb13"
				case 14 :videoborneflasher.ImageA="vb14"
				case 15 :videoborneflasher.ImageA="vb15"
				case 16 :videoborneflasher.ImageA="vb16"
				case 17 :videoborneflasher.ImageA="vb17"
				case 18 :videoborneflasher.ImageA="vb18"
				case 19 :videoborneflasher.ImageA="vb19"
				case 20 :videoborneflasher.ImageA="vb20"
				case 21 :videoborneflasher.ImageA="vb21"
				case 22 :videoborneflasher.ImageA="vb22"
				case 23 :videoborneflasher.ImageA="vb23"
				case 24 :videoborneflasher.ImageA="vb24"
				case 25 :videoborneflasher.ImageA="vb25"
				case 26 :videoborneflasher.ImageA="vb26"
				case 27 :videoborneflasher.ImageA="vb27"
				case 28 :videoborneflasher.ImageA="vb28"
				case 29 :videoborneflasher.ImageA="vb29"
				case 30 :videoborneflasher.ImageA="vb30"
				case 31 :videoborneflasher.ImageA="vb31"
				case 32 :videoborneflasher.ImageA="vb32"
				case 33 :videoborneflasher.ImageA="vb33"
				case 34 :videoborneflasher.ImageA="vb34"
				case 35 :videoborneflasher.ImageA="vb35"
				case 36 :videoborneflasher.ImageA="vb36"
				case 37 :videoborneflasher.ImageA="vb37"
				case 38 :videoborneflasher.ImageA="vb38"
				case 39 :videoborneflasher.ImageA="vb39"
				case 40 :videoborneflasher.ImageA="vb40"
				case 41 :videoborneflasher.ImageA="vb41"
				case 42 :videoborneflasher.ImageA="vb42"
				case 43 :videoborneflasher.ImageA="vb43"
				case 44 :videoborneflasher.ImageA="vb44"
				case 45 :videoborneflasher.ImageA="vb45"
				case 46 :videoborneflasher.ImageA="vb46"
				case 47 :videoborneflasher.ImageA="vb47"
				case 48 :videoborneflasher.ImageA="vb48"
				case 49 :videoborneflasher.ImageA="vb49"
				case 50 :videoborneflasher.ImageA="vb50"
				case 51 :videoborneflasher.ImageA="vb51"
				case 52 :videoborneflasher.ImageA="vb52"
				case 53 :videoborneflasher.ImageA="vb53"
				case 54 :videoborneflasher.ImageA="vb54"
				case 55 :videoborneflasher.ImageA="vb55"
				case 56 :videoborneflasher.ImageA="vb56"
				case 57 :videoborneflasher.ImageA="vb57"
				case 58 :videoborneflasher.ImageA="vb58"
				case 59 :videoborneflasher.ImageA="vb59"
				case 60 :videoborneflasher.ImageA="vb60"
				case 61 :videoborneflasher.ImageA="vb61"
				case 62 :videoborneflasher.ImageA="vb62"
				case 63 :videoborneflasher.ImageA="vb63"
				case 64 :videoborneflasher.ImageA="vb64"
				case 65 :videoborneflasher.ImageA="vb65"
		End Select
	
End Sub	
dim Hit1
sub hitquest_timer
Hit1 = Hit1 + 1 : If Hit1 > 8 then Hit1 = 1: end If 
select case Hit1
				case 1 :hitflasher.ImageA="Hit": Hitflasher.visible = True
				case 2 :hitflasher.ImageA="hitvierge"
				case 3 :hitflasher.ImageA="Hit"
				case 4 :hitflasher.ImageA="hitvierge"
				case 5 :hitflasher.ImageA="Hit"
				case 6 :hitflasher.ImageA="hitvierge"
				case 7 :hitflasher.ImageA="Hit"
				case 8 :hitflasher.ImageA="hitvierge"
			End Select
End Sub	

Dim SJPK
Sub Superjkp_timer
SJPK = SJPK + 1 : If SJPK > 8 Then SJPK = 1: End if
Select case SJPK
				Case 1 :Hitflasher.ImageA="Superjkp":Hitflasher001.ImageA="Superjkp":Hitflasher002.ImageA="Superjkp": Hitflasher.visible = True: Hitflasher001.visible = True: Hitflasher002.visible = True
				case 2 :hitflasher.ImageA="hitvierge":hitflasher001.ImageA="hitvierge":hitflasher002.ImageA="hitvierge" 
				case 3 :hitflasher.ImageA="Superjkp":Hitflasher001.ImageA="Superjkp":Hitflasher002.ImageA="Superjkp"
				case 4 :hitflasher.ImageA="hitvierge":hitflasher001.ImageA="hitvierge":hitflasher002.ImageA="hitvierge"
				case 5 :hitflasher.ImageA="Superjkp":Hitflasher001.ImageA="Superjkp":Hitflasher002.ImageA="Superjkp"
				case 6 :hitflasher.ImageA="hitvierge":hitflasher001.ImageA="hitvierge":hitflasher002.ImageA="hitvierge"
				case 7 :hitflasher.ImageA="Superjkp":Hitflasher001.ImageA="Superjkp":Hitflasher002.ImageA="Superjkp"
				case 8 :hitflasher.ImageA="hitvierge":hitflasher001.ImageA="hitvierge":hitflasher002.ImageA="hitvierge"
			End Select
End Sub

DIM GO1
Sub G1_Timer

GO1 = GO1 + 1 : If GO1 > 56 then GO1 = 1: end If 
select case GO1
				case 1 :questflasher.ImageA="Gameover_00001": questflasher.visible = True
				case 2 :questflasher.ImageA="Gameover_00002"
				case 3 :questflasher.ImageA="Gameover_00003"
				case 4 :questflasher.ImageA="Gameover_00004"
				case 5 :questflasher.ImageA="Gameover_00005"
				case 6 :questflasher.ImageA="Gameover_00006"
				case 7 :questflasher.ImageA="Gameover_00007"
				case 8 :questflasher.ImageA="Gameover_00008"
				case 9 :questflasher.ImageA="Gameover_00009"
				case 10 :questflasher.ImageA="Gameover_00010"
				case 11 :questflasher.ImageA="Gameover_00011"
				case 12 :questflasher.ImageA="Gameover_00012"
				case 13 :questflasher.ImageA="Gameover_00013"
				case 14 :questflasher.ImageA="Gameover_00014"
				case 15 :questflasher.ImageA="Gameover_00015"
				case 16 :questflasher.ImageA="Gameover_00016"
				case 17 :questflasher.ImageA="Gameover_00017"
				case 18 :questflasher.ImageA="Gameover_00018"
				case 19 :questflasher.ImageA="Gameover_00019"
				case 20 :questflasher.ImageA="Gameover_00020"
				case 21 :questflasher.ImageA="Gameover_00021"
				case 22 :questflasher.ImageA="Gameover_00022"
				case 23 :questflasher.ImageA="Gameover_00023"
				case 24 :questflasher.ImageA="Gameover_00024"
				case 25 :questflasher.ImageA="Gameover_00025"
				case 26 :questflasher.ImageA="Gameover_00026"
				case 27 :questflasher.ImageA="Gameover_00027"
				case 28 :questflasher.ImageA="Gameover_00028"
				case 29 :questflasher.ImageA="Gameover_00029"
				case 30 :questflasher.ImageA="Gameover_00030"
				case 31 :questflasher.ImageA="Gameover_00031"
				case 32 :questflasher.ImageA="Gameover_00032"
				case 33 :questflasher.ImageA="Gameover_00033"
				case 34 :questflasher.ImageA="Gameover_00034"
				case 35 :questflasher.ImageA="Gameover_00035"
				case 36 :questflasher.ImageA="Gameover_00036"
				case 37 :questflasher.ImageA="Gameover_00037"
				case 38 :questflasher.ImageA="Gameover_00038"
				case 39 :questflasher.ImageA="Gameover_00039"
				case 40 :questflasher.ImageA="Gameover_00040"
				case 41 :questflasher.ImageA="Gameover_00041"
				case 42 :questflasher.ImageA="Gameover_00042"
				case 43 :questflasher.ImageA="Gameover_00043"
				case 44 :questflasher.ImageA="Gameover_00044"
				case 45 :questflasher.ImageA="Gameover_00045"
				case 46 :questflasher.ImageA="Gameover_00046"
				case 47 :questflasher.ImageA="Gameover_00047"
				case 48 :questflasher.ImageA="Gameover_00048"
				case 49 :questflasher.ImageA="Gameover_00049"
				case 50 :questflasher.ImageA="Gameover_00050"
				case 51 :questflasher.ImageA="Gameover_00051"
				case 52 :questflasher.ImageA="Gameover_00052"
				case 53 :questflasher.ImageA="Gameover_00053"
				case 54 :questflasher.ImageA="Gameover_00054"
				case 55 :questflasher.ImageA="Gameover_00055"
				case 56 :questflasher.ImageA="Gameover_00056"
				
				
			End Select
	'G1.enabled = False
	'questflasher.visible = False
End Sub	
'***************************************
' 			Storm
'***************************************
DIM Eclair
Sub storm1_timer
Eclair = Eclair + 1 : If Eclair > 9 then Eclair = 1 = False: end If 
Select Case Eclair
					case 1 : Stormflasher.ImageA="Eclairs01": Stormflasher.visible = True
					case 2 : Stormflasher.ImageA="Eclairs02"
					case 3 : Stormflasher.ImageA="Eclairs03"
					case 4 : Stormflasher.ImageA="Eclairs04"
					case 5 : Stormflasher.ImageA="Eclairs05"
					case 6 : Stormflasher.ImageA="Eclairs06"
					case 7 : Stormflasher.ImageA="Eclairs07"
					case 8 : Stormflasher.ImageA="Eclairs08"
					case 9 : Stormflasher.ImageA="Eclairs09"
		End Select
End Sub

Sub Table1_MusicDone()
	
End Sub









'********************* START OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF BELOW   THIS LINE!!!! ***************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'
'
'  Quick Steps:
'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
'      2>  above set global variable pGameName="yourgame"
'      3>  copy paste the settings section above to top of table script for user changes.
'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
'      5>  go to your table1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				if icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function

Dim HasPup: HasPup = EnablePupPack
'if EnablePupDmd Then HasPuP = True  : End If 'dont set to false as it will break pup

Const pTopper=0
Const pDMD=1
Const pDMDSplashScreen=16  ' Temporary video layer above the score layer.
Const pBackglass=2
Const pPlayfield=3
Const pMusic=4
Const pMusic2=5
Const pCallouts=6
Const pBackglass2=7
Const pTopper2=8
Const pPopUP=9
Const pPopUP2=10
Const pBackglassVideos=12
Const pPuPOverlay=13
Const pTopperVideos=14
Const pApron=18

'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pTargerLetters=5

'dmdType
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pDMDTypeFULL=2






Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode
Dim PUPStatus: PUPStatus=false



'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit
    If PUPStatus Then Exit Sub  ' Initialize the PuP pack and labels once.

	Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
	PuPlayer.B2SInit "", pGameName
	PUPStatus=true
	If EnablePupDmd Then

		if (PuPDMDDriverType=pDMDTypeReal) and (useRealDMDScale=1) Then 
			PuPlayer.setScreenEx pDMD,0,0,128,32,0  'if hardware set the dmd to 128,32
			pDMDSetBackFrame("default.png")  'reset frame after rescale
		End if

		PuPlayer.LabelInit pDMD


		if PuPDMDDriverType=pDMDTypeReal then

			Set PUPDMDObject = CreateObject("PUPDMDControl.DMD") 
			PUPDMDObject.DMDOpen
			PUPDMDObject.DMDPuPMirror
			PUPDMDObject.DMDPuPTextMirror
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":32, ""FQ"":3 }"   'set no antialias on font render if real
		End If


		pSetPageLayouts
        pupDMDUpdate.Interval = 250
        pupDMDUpdate.Enabled = True

	pDMDSetPage(pDMDBlank)   'set blank text overlay page.
	pAttractStart' firsttime running for like an startup video..

	Else

	PuPlayer.SendMsg "{ ""mt"":301, ""SN"": 1, ""FN"":12}"
	End If

End Sub 'end PUPINIT



'PinUP Player DMD Helper Functions

Sub pDMDLabelHide(labName)
PuPlayer.LabelSet pDMD,labName,"",0,""   
end sub




Sub pDMDScrollBig(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub


Sub pDMDSplashScore(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
end Sub

Sub pDMDSplashScoreScroll(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
end Sub

Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':3,'hstart':5,'hend':80,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
end sub

Sub pDMDTargetLettersInfo(msgText,msgInfo, timeSec)  'msgInfo = '0211'  0= layer 1, 1=layer 2, 2=top layer3.
'this function is when you want to hilite spelled words.  Like B O N U S but have O S hilited as already hit markers... see example.
PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
Dim backText
Dim middleText
Dim flashText
Dim curChar
Dim i
Dim offchars:offchars=0
Dim spaces:spaces=" "  'set this to 1 or more depends on font space width.  only works with certain fonts
                          'if using a fixed font width then set spaces to just one space.

For i=1 To Len(msgInfo)
    curChar="" & Mid(msgInfo,i,1)
    if curChar="0" Then
            backText=backText & Mid(msgText,i,1)
            middleText=middleText & spaces
            flashText=flashText & spaces          
            offchars=offchars+1
    End If
    if curChar="1" Then
            backText=backText & spaces
            middleText=middleText & Mid(msgText,i,1)
            flashText=flashText & spaces
    End If
    if curChar="2" Then
            backText=backText & spaces
            middleText=middleText & spaces
            flashText=flashText & Mid(msgText,i,1)
    End If   
Next 

if offchars=0 Then 'all litup!... flash entire string
   backText=""
   middleText=""
   FlashText=msgText
end if  

PuPlayer.LabelSet pDMD,"Back5"  ,backText  ,1,""
PuPlayer.LabelSet pDMD,"Middle5",middleText,1,""
PuPlayer.LabelSet pDMD,"Flash5" ,flashText ,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & "}"   
end Sub


Sub pDMDSetPage(pagenum)    
    PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub

Sub pHideOverlayText(pDisp)
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& pDisp &", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
end Sub



Sub pDMDShowLines3(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,3,timeSec,""
PuPlayer.LabelSet pDMD,"Splash3a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash3b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash3c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowLines2(msgText,msgText2,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,4,timeSec,""
PuPlayer.LabelSet pDMD,"Splash4a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash4b",msgText2,vis,pLine2Ani
end Sub

Sub pDMDShowCounter(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,6,timeSec,""
PuPlayer.LabelSet pDMD,"Splash6a",msgText,vis, pLine1Ani
PuPlayer.LabelSet pDMD,"Splash6b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash6c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowBig(msgText,timeSec, mColor)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,vis,pLine1Ani
end sub


Sub pDMDShowHS(msgText,msgText2,msgText3,timeSec) 'High Score
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,7,timeSec,""
PuPlayer.LabelSet pDMD,"Splash7a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash7b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash7c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDSetBackFrame(fname)
  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
end Sub

Sub pDMDStartBackLoop(fPlayList,fname)
  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
  PuPlayer.SetBackGround pDMD,1
end Sub

Sub pDMDStopBackLoop
  PuPlayer.SetBackGround pDMD,0
  PuPlayer.playstop pDMD
end Sub


Dim pNumLines

'Theme Colors for Text (not used currenlty,  use the |<colornum> in text labels for colouring.
Dim SpecialInfo
Dim pLine1Color : pLine1Color=8454143  
Dim pLine2Color : pLine2Color=8454143
Dim pLine3Color :  pLine3Color=8454143
Dim curLine1Color: curLine1Color=pLine1Color  'can change later
Dim curLine2Color: curLine2Color=pLine2Color  'can change later
Dim curLine3Color: curLine3Color=pLine3Color  'can change later


Dim pDMDCurPriority: pDMDCurPriority =-1
Dim pDMDDefVolume: pDMDDefVolume = 0   'default no audio on pDMD

Dim pLine1
Dim pLine2
Dim pLine3
Dim pLine1Ani
Dim pLine2Ani
Dim pLine3Ani

Dim PriorityReset:PriorityReset=-1
DIM pAttractReset:pAttractReset=-1
DIM pAttractBetween: pAttractBetween=2000 '1 second between calls to next attract page
DIM pDMDVideoPlaying: pDMDVideoPlaying=false
Dim pDMDSplashToken: pDMDSplashToken=0


'****************************************************************************
'*********************     DO NOT MODIFY BELOW      *************************
'****************************************************************************
'********************* START PUPDMD FRAMEWORK v1.0 **************************
'****************************************************************************
Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345

DIM curPos
	if pDMDCurPriority>pPriority then Exit Sub  'if something is being displayed that we don't want interrupted.  same level will interrupt.
	pDMDCurPriority=pPriority
    pDMDSplashToken=pDMDSplashToken+1  ' Invalidate older splash callbacks.
if timeSec=0 then timeSec=1 'don't allow page default page by accident
	pLine1=""
	pLine2=""
	pLine3=""
	pLine1Ani=""
	pLine2Ani=""
	pLine3Ani=""
if pAni=1 Then  'we flashy now aren't we
	pLine1Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	pLine2Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	pLine3Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
end If
curPos=InStr(pText,"^")   'Lets break apart the string if needed
if curPos>0 Then 
   pLine1=Left(pText,curPos-1) 
   pText=Right(pText,Len(pText) - curPos)
   
   curPos=InStr(pText,"^")   'Lets break apart the string
   if curPOS>0 Then
      pLine2=Left(pText,curPos-1) 
      pText=Right(pText,Len(pText) - curPos)

      curPos=InStr("^",pText)   'Lets break apart the string   
      if curPos>0 Then
         pline3=Left(pText,curPos-1) 
      Else 
        if pText<>"" Then pline3=pText 
      End if 
   Else 
      if pText<>"" Then pLine2=pText
   End if    
Else 
  pLine1=pText  'just one line with no break 
End if
'lets see how many lines to Show
pNumLines=0
if pLine1<>"" then pNumLines=pNumlines+1
if pLine2<>"" then pNumLines=pNumlines+1
if pLine3<>"" then pNumLines=pNumlines+1

if pDMDVideoPlaying Then
    PuPlayer.playstop pDMDSplashScreen
    pDMDVideoPlaying=False
End if
if (VideoName<>"") and (useDMDVideos) Then
    ' Keep temporary clips off the persistent score screen.
    PuPlayer.playstop pDMD
    PuPlayer.playstop pDMDSplashScreen
    PuPlayer.playlistplayex pDMDSplashScreen,"DMDSplash",VideoName,pDMDDefVolume,pPriority
    pDMDVideoPlaying=True
    PriorityReset=TimeSec*1000
    ' Use the ball-release timer service as an independent expiry path.
    vpmtimer.addtimer CLng(TimeSec*1000), "pDMDSplashExpired " & pDMDSplashToken & " '"
    If pInAttract Then
        pDMDSetPage pDMDBlank
    Else
        pDMDSetPage pScores
        pUpdateScores
    End If
    Exit Sub
end if

if StrComp(pEventID,"shownum",1)=0 Then              'check eventIDs
    pDMDShowCounter pLine1,pLine2,pLine3,timeSec
Elseif StrComp(pEventID,"target",1)=0 Then              'check eventIDs
    pDMDTargetLettersInfo pLine1,pLine2,timeSec
Elseif StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
    pDMDShowHS pLine1,pLine2,pline3,timeSec
Elseif (pNumLines=3) Then                'depends on # of lines which one to use.  pAni=1 will flash.
    pDMDShowLines3 pLine1,pLine2,pLine3,TimeSec
Elseif (pNumLines=2) Then
    pDMDShowLines2 pLine1,pLine2,TimeSec
Elseif (pNumLines=1) Then
    pDMDShowBig pLine1,timeSec, curLine1Color
Else
    pDMDShowBig pLine1,timeSec, curLine1Color
End if

PriorityReset=TimeSec*1000
End Sub 'pupDMDDisplay message

Sub pDMDSplashExpired(splashToken)
    ' A delayed callback must never stop a newer clip.
    If splashToken <> pDMDSplashToken Then Exit Sub
    If Not pDMDVideoPlaying Then Exit Sub
    PuPlayer.playstop pDMDSplashScreen
    pDMDVideoPlaying=False
    PriorityReset=-1
    pDMDCurPriority=-1
    If pInAttract Then
        pAttractReset=pAttractBetween
    Else
        pDMDSetPage pScores
        pUpdateScores
    End If
End Sub

Sub pupDMDupdate_Timer()
	pUpdateScores    

    if PriorityReset>0 Then  'for splashes we need to reset current prioirty on timer
       PriorityReset=PriorityReset-pupDMDUpdate.interval
       if PriorityReset<=0 Then 
            ' Stop expired splash media so its final frame cannot cover the scores.
            if pDMDVideoPlaying Then
                PuPlayer.playstop pDMDSplashScreen
                pDMDVideoPlaying=false
            End if
            pDMDCurPriority=-1
            if pInAttract Then
                pAttractReset=pAttractBetween
            Else
                pDMDSetPage pScores
                pUpdateScores
            End if
       End if
    End if

    if pAttractReset>0 Then  'for splashes we need to reset current prioirty on timer
       pAttractReset=pAttractReset-pupDMDUpdate.interval
       if pAttractReset<=0 Then 
            pAttractReset=-1            
            if pInAttract then pAttractNext
			End if
    end if 
End Sub

Sub PuPEvent(EventNum)
	if hasPUP=false then Exit Sub
    ' Quest panels also use screen 16; cancel expiry from an older script clip.
    Select Case EventNum
        Case 401, 821, 824, 827, 830, 833, 836, 839, 842, 845, 848, 851, 854, 857, 860, 948, 950, 952, 954
            pDMDSplashToken=pDMDSplashToken+1
            pDMDVideoPlaying=False
            PriorityReset=-1
            pDMDCurPriority=-1
            PuPlayer.playstop pDMDSplashScreen
    End Select
	PuPlayer.B2SData "E"&EventNum,1  'send event to puppack driver  
End Sub
'****************************************************************************
'*********************     DO NOT MODIFY ABOVE      *************************
'****************************************************************************
'********************* END OF PUPDMD FRAMEWORK v1.0 *************************
'****************************************************************************

'****************************************************************************
' PUP DMD SETTINGS
'****************************************************************************
' NOTE: Fonts must be in folder \PuUPVideos\<tablename>\FONTS
' "case sensitive exact naming fonts!"
'****************************************************************************
Sub pSetPageLayouts
	DIM dmddef
	DIM dmdalt
	DIM dmdscr
	DIM dmdfixed
'****************************************************************************
'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
' 
'<screen#>, in standard we’d set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to ‘force’ a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT… this will assign this label to this ‘page’ or group.
'<visible> initial state of label. visible=1 show, 0 = off.
'****************************************************************************
	IF PuPDMDDriverType=pDMDTypeReal Then ' for Real DMD Mirroring // 128x32 Real Color DMD
		dmdalt="Breathe Fire"
		dmdfixed="Breathe Fire"
		dmdscr="Breathe Fire"    'main scorefont
		dmddef="Breathe Fire"
		'Page 1 (default score display)
			 PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20,990203   ,0,2,2,95,0,1,0
			 PuPlayer.LabelNew pDMD,"Play1"   ,dmdalt,21,33023   ,1,0,0,15,0,1,0
			 PuPlayer.LabelNew pDMD,"Ball"    ,dmdalt,21,990203   ,1,2,0,85,0,1,0
			 PuPlayer.LabelNew pDMD,"MsgScore",dmddef,40,33023   ,0,1,0, 0,40,1,0
			 PuPlayer.LabelNew pDMD,"CurScore",dmdscr,40,33023   ,0,1,1, 0,0,1,1
		'Page 2 (default Text Splash 1 Big Line)
			 PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,30,33023,0,1,1,0,0,2,0
		'Page 3 (default Text Splash 2 and 3 Lines)
			 PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,8454143,0,1,0,0,2,3,0
			 PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,33023,0,1,0,0,30,3,0
			 PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,33023,0,1,0,0,55,3,0
		'Page 4 (2 Line Gameplay DMD)
			 PuPlayer.LabelNew pDMD,"Splash4a",dmddef,30,8454143,0,1,0,0,0,4,0
			 PuPlayer.LabelNew pDMD,"Splash4b",dmddef,30,33023,0,1,2,0,75,4,0
		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0
		'Page 7 (Show High Scores Fixed Fonts)
			PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0
	END IF  ' use PuPDMDDriver

	IF PuPDMDDriverType=pDMDTypeLCD THEN  ' for  PuP DMD // 4:1 ratio LCD display
		'dmddef="Impact"
		dmdalt="Breathe Fire"    
		dmdfixed="Breathe Fire"
		dmdscr="Breathe Fire"  'main score font
		dmddef="Breathe Fire"
		'Page 1 (default score display)
			PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20,990203   ,0,2,2,95,0,1,0
			PuPlayer.LabelNew pDMD,"Play1"   ,dmdalt,20,990203  ,1,0,0,15,0,1,0
			PuPlayer.LabelNew pDMD,"Ball"    ,dmdalt,20,990203   ,1,2,0,85,0,1,0
			PuPlayer.LabelNew pDMD,"MsgScore",dmddef,45,33023   ,0,1,0, 0,40,1,0
			PuPlayer.LabelNew pDMD,"CurScore",dmdscr,40,33023   ,0,1,1, 0,0,1,0
		'Page 2 (default Text Splash 1 Big Line)
			PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40,33023,0,1,1,0,0,2,0
		'Page 3 (default Text 3 Lines)
			PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,8454143,0,1,0,0,2,3,0
			PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,33023,0,1,0,0,30,3,0
			PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,33023,0,1,0,0,57,3,0
		'Page 4 (default Text 2 Line)
			PuPlayer.LabelNew pDMD,"Splash4a",dmddef,40,8454143,0,1,0,0,0,4,0
			PuPlayer.LabelNew pDMD,"Splash4b",dmddef,30,33023,0,1,2,0,75,4,0
		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0
		'Page 7 (Show High Scores Fixed Fonts)
			PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0
	END IF  ' use PuPDMDDriver

	IF PuPDMDDriverType=pDMDTypeFULL THEN  ' for PuP DMD // 16x9 FullDMD LCD display
		' *** 2024-02-04 LTEK // modified FullDMD text Scale % to properly fit ***
		'dmddef="Impact"
		dmdalt="Breathe Fire"    
		dmdfixed="Breathe Fire"
		dmdscr="Breathe Fire"  'main score font
		dmddef="Breathe Fire"
		'Page 1 (default score display)
			PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20,990203   ,0,2,2,95,0,1,0
			PuPlayer.LabelNew pDMD,"Play1"   ,dmdalt,20,990203  ,1,0,0,15,0,1,0
			PuPlayer.LabelNew pDMD,"Ball"    ,dmdalt,20,990203   ,1,2,0,85,0,1,0
			PuPlayer.LabelNew pDMD,"MsgScore",dmddef,20,33023   ,0,1,0, 0,40,1,0
			PuPlayer.LabelNew pDMD,"CurScore",dmdscr,30,33023   ,0,1,1, 0,0,1,0		
		'Page 2 (default Text Splash 1 Big Line)
			PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,25,33023,0,1,1,0,0,2,0
		'Page 3 (default Text 3 Lines)
			PuPlayer.LabelNew pDMD,"Splash3a",dmddef,25,8454143,0,1,0,0,2,3,0
			PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,25,33023,0,1,0,0,30,3,0
			PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,20,33023,0,1,0,0,57,3,0
		'Page 4 (default Text 2 Line)
			PuPlayer.LabelNew pDMD,"Splash4a",dmddef,25,8454143,0,1,0,0,0,4,0
			PuPlayer.LabelNew pDMD,"Splash4b",dmddef,20,33023,0,1,2,0,75,4,0
		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,25,8421504,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,25,65535  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,25,65535  ,0,1,1,0,0,5,0
		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,25,65280,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,20,33023,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,20,33023,0,1,0,60,50,6,0
		'Page 7 (Show High Scores Fixed Fonts)
			PuPlayer.LabelNew pDMD,"Splash7a",dmddef,15,8454143,0,1,0,0,2,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,20,33023,0,1,0,0,20,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,20,33023,0,1,0,0,50,7,0
		'Page 8 (default Text 2 Line)
			PuPlayer.LabelNew pDMD,"Splash8a",dmddef,25,8454143,0,1,0,0,0,4,0
			PuPlayer.LabelNew pDMD,"Splash8b",dmddef,20,33023,0,1,2,0,75,4,0
	END IF  ' use PuPDMDDriver
    ' Page 9: compact music-only labels; leave gameplay text sizes intact.
    PuPlayer.LabelNew pDMD,"MusTitle",dmddef,12,8454143,0,1,0,0,8,9,0
    PuPlayer.LabelNew pDMD,"MusTrack",dmdalt,18,33023,0,1,0,0,28,9,0
    PuPlayer.LabelNew pDMD,"MusKeys",dmddef,8,33023,0,1,0,0,56,9,0
    PuPlayer.LabelNew pDMD,"MusGo",dmddef,8,33023,0,1,0,0,69,9,0
END Sub 'page Layouts


'*****************************************************************
' PUPDMD Custom Subs / Events
'*****************************************************************
'
'   call pDMDStartGame,pDMDStartBall,pGameOver,pAttractStart
'
Sub pDMDStartGame
	pInAttract=false
	pDMDSetPage(pScores)   'set blank text overlay page.
end Sub

Sub pDMDStartBall
end Sub

Sub pDMDGameOver
	pAttractStart
end Sub

Sub pAttractStart
	pDMDSetPage(pDMDBlank)   'set blank text overlay page.
	pCurAttractPos=0
	pInAttract=True          'Startup in AttractMode
	pAttractNext
end Sub

'Sub pDMDStartUP
 'pupDMDDisplay "attract","Welcome","@welcome.mp4",2,0,10
 'pInAttract=true
'end Sub

DIM pCurAttractPos: pCurAttractPos=0

' ** called auto each page next and timed already in DMD_Timer
' ** must use pupDMDDisplay or it wont advance auto

Sub pAttractNext    ' ** PuP DMD's Atrract Mode onscreen text rotation
	pCurAttractPos=pCurAttractPos+1
  Select Case pCurAttractPos
		Case 1  
			pupDMDDisplay "attract","Dragon's Lair","",3,1,10
		Case 2
			pupDMDDisplay "attract","Save Daphne^by Finishing^All Quests","",3,0,10
		Case 3
			If bFreePlay Then
				pupDMDDisplay "attract", "FREE PLAY^PRESS START", "", 3, 0, 10
			ElseIf Credits = 0 Then
				pupDMDDisplay "attract", "CREDITS 0^INSERT COIN", "", 3, 0, 10
			Else    
				pupDMDDisplay "attract", "PRESS START"&"^CREDITS: "&(Credits), "", 3, 0, 10
			End If
		Case 4
			pupDMDDisplay "highscore","High Scores^1. " & HighScoreName(0) & "  " & HighScore(0)&"^2. " & HighScoreName(1) & "  " & HighScore(1) , "", 3, 0, 10  
		Case 5 
			pupDMDDisplay "highscore","High Scores^3. " & HighScoreName(2) & "  " & HighScore(2)&"^4. " & HighScoreName(3) & "  " & HighScore(3) , "", 3, 0, 10     
		Case 6
			pupDMDDisplay "attract","Table by^New French Team^Production","",3,0,10      'CEZ and JMX
		Case 7
			pupDMDDisplay "attract"," ^Graphics by JMX^Scripting by CEZ","", 3, 0, 10 'TEST
		Case 8
			pupDMDDisplay "attract"," ^PuP Pack & Script^Mods by LTek","",3,0,10
		Case 9
			'pupDMDDisplay "attract","Thanks to JPSalas^MTC^VPNation","",3,0,10
			If bFreePlay Then
				pupDMDDisplay "attract", "FREE PLAY^PRESS START", "", 3, 0, 10
			Else
				pupDMDDisplay "attract", "PRESS START"&"^CREDITS: "&(Credits), "", 3, 0, 10
			End If
		'Case 10
		'	If score(currentplayer) > 0 Then    
		'		pupDMDDisplay "GAMEOVER", "GAME OVER^Last Score "&(score(currentplayer)), "", 3, 1, 10
		'		pupDMDDisplay "GAMEOVER", "GAME OVER^Last Score "&(score(currentplayer)), "", 3, 1, 10
		'	Else
		'		pupDMDDisplay "GAMEOVER", "GAME OVER", "", 3, 1, 10 'GAME OVER
		'	end if  
		Case Else
			pCurAttractPos=0
			pAttractNext 'reset to beginning
	END Select
END Sub


'**************************************************************
' Gameplay Score Updates
'**************************************************************
'Dim CurTestScore:CurTestScore=100000
Sub pUpdateScores  'call this ONLY on timer 300ms is good enough
	if pDMDCurPage <> pScores then Exit Sub
	'puPlayer.LabelSet pDMD,"Credits","CREDITS " & ""& Credits ,1,""
	'puPlayer.LabelSet pDMD,"Play1","Player 1",1,""
	'puPlayer.LabelSet pDMD,"Ball"," "&pDMDCurPriority ,1,""
	puPlayer.LabelSet pDMD,"CurScore","" & FormatNumber(Score(CurrentPlayer),0) ,1,""
	puPlayer.LabelSet pDMD,"Play1","Player " & CurrentPlayer,1,""
	puPlayer.LabelSet pDMD,"Ball","ball " & ""& balls ,1,""
end Sub

'backward compatiblity with old methods and new pupdmd framework.
Sub pDMDSplashLines(msgText,msgText2,timeSec,Ani)
	if msgText="" Then msgText=" "
	if msgText2="" Then msgText2="  "
	pupDMDDisplay "default", msgText&"^"&msgText2, "", timesec, Ani, 10
end Sub

SUB pDMDSplashBonus(msgText,msgText2,timeSec,Ani)
	if msgText="" Then msgText=" "
	if msgText2="" Then msgText2="  "
	pupDMDDisplay "default", msgText&"^"&msgText2, "", timesec, Ani, 10
end Sub


SUB pDMDSplash3Lines(msgText,msgText2,msgText3,timeSec,Ani)
	if msgText="" Then msgText=" "
	if msgText2="" Then msgText2="  "
	if msgText3="" Then msgText3="  "
	pupDMDDisplay "default", msgText&"^"&msgText2&"^"&msgText3, "", timesec, Ani, 10
end Sub


SUB pDMDSplashBig(msgText,timeSec,Ani)
	pupDMDDisplay "default",msgText, "", timesec, Ani, 10
end Sub

SUB pUpdateMystery(msgText,msgText2,msgText3,timeSec,Ani)
	if msgText="" Then msgText=" "
	if msgText2="" Then msgText2="  "
	if msgText3="" Then msgText3="  "
	If enablePupDMD Then
		PuPlayer.LabelSet pDMD,"Splash3a",msgText,1,""
		PuPlayer.LabelSet pDMD,"Splash3b",msgText2,1,""
		PuPlayer.LabelSet pDMD,"Splash3c",msgText3,1,""
	End If
end Sub

' PuPInit is called by Table1_Init; do not initialize it again at script scope.

'**************************************************************
' PuP DMD Display   'REMOVED' 
'**************************************************************
' Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345
' ** EXAMPLES BELOW **
'pupDMDDisplay "shoot", "SHOOT AGAIN!", ", 3, 1, 10 
'pupDMDDisplay "default", "DATA GADGET LIT", "@DataGadgetLit.mp4", 3, 1, 10
'pupDMDDisplay "shoot", "SHOOT AGAIN!", "@shootagain.mp4", 3, 1, 10   
'pupDMDDisplay "balllock", "Ball^Locked|16744448", "", 5, 1, 10             '  5 seconds,  1=flash, 10=priority, ball is first line, locked on second and locked has custom color |
'pupDMDDisplay "balllock","Ball 2^is^Locked", "balllocked2.mp4",3, 1,10     '  3 seconds,  1=flash, play balllocked2.mp4 from dmdsplash folder, 
'pupDMDDisplay "balllock","Ball^is^Locked", "@balllocked.mp4",3, 1,10       '  3 seconds,  1=flash, play @balllocked.mp4 from dmdsplash folder, because @ text by default is hidden unless useDmDvideos is disabled.
'pupDMDDisplay "shownum", "3^More To|616744448^GOOOO", "", 5, 1, 10         ' "shownum" is special.  layout is line1=BIG NUMBER and line2,line3 are side two lines.  "4^Ramps^Left"
'pupDMDDisplay "target", "POTTER^110120", "blank.mp4", 10, 0, 10            ' 'target'...  first string is line,  second is 0=off,1=already on, 2=flash on for each character in line (count must match)
'pupDMDDisplay "highscore", "High Score^AAA   2451654^BBB   2342342", "", 5, 0, 10            ' highscore is special  line1=text title like highscore, line2, line3 are fixed fonts to show AAA 123,123,123
'pupDMDDisplay "highscore", "High Score^AAA   2451654|616744448^BBB   2342342", "", 5, 0, 10  ' sames as above but notice how we use a custom color for text |
'**************************************************************

'**************************************************************
' PLAYFIELD APRON SCREENS (Left - Center - Right)
'     note...triggers and images required to be from PuP Pack 
'**************************************************************
' PUPSCREEN17 ... Playfield Apron LEFT 
' PUPSCREEN18 ... Playfield Apron RIGHT 
' PUPSCREEN19 ... Playfield Apron CENTER 

'Sub quest_Timer()       ' PLAYFIELD QUEST SCREEN, removed from playfield and moved to FullDMD
'		Quest.VideoCapUpdate="PUPSCREEN16"
'end Sub		

'If PupScreen1 =1 Then
'	Quest.VideoCapWidth=400	
'	Quest.VideoCapHeight=200
'	Quest.visible=False
 '   Quest.TimerEnabled=true
 '   Quest.TimerInterval=60
 '   If FPS = 1 Then Quest.timerinterval=40
'End If

'if PuPScreen1 = 0 Then
'    Quest.visible=false	
'    Quest.TimerEnabled=false
'    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 18, ""FN"":12 }" 'STOPSCREEN
'End If

Sub Screen001_Timer()    ' PLAYFIELD LEFT APRON SCREEN
		Screen001.VideoCapUpdate="PUPSCREEN17"
end Sub		

If Pupscreen1 =1 Then
	Screen001.VideoCapWidth=200	
	Screen001.VideoCapHeight=200
	Screen001.visible=False	
    Screen001.TimerEnabled=true
    Screen001.TimerInterval=60
    If FPS = 1 Then Screen001.timerinterval=40
End If

if Pupscreen1 = 0 Then
    Screen001.visible=false	
    Screen001.TimerEnabled=false
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 18, ""FN"":12 }" 'STOPSCREEN
End If

Sub Screen_Timer()      ' PLAYFIELD RIGHT APRON SCREEN
		Screen.VideoCapUpdate="PUPSCREEN18"
end Sub		

If PupScreen1 =1 Then
	Screen.VideoCapWidth=200	
	Screen.VideoCapHeight=200
	Screen.visible=False
    Screen.TimerEnabled=true
    Screen.TimerInterval=60
    If FPS = 1 Then Screen.timerinterval=40
End If

if PuPScreen1 = 0 Then
    Screen.visible=false	
    Screen.TimerEnabled=false
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 18, ""FN"":12 }" 'STOPSCREEN
End If

Sub Screen002_Timer()      ' PLAYFIELD CENTER APRON SCREEN
		Screen002.VideoCapUpdate="PUPSCREEN19"
end Sub		

If Pupscreen1 =1 Then
	Screen002.VideoCapWidth=200	
	Screen002.VideoCapHeight=200
	Screen002.visible=False
    Screen002.TimerEnabled=true
    Screen002.TimerInterval=60
    If FPS = 1 Then Screen002.timerinterval=40
End If

if Pupscreen1 = 0 Then
    Screen002.visible=false	
    Screen002.TimerEnabled=false
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 18, ""FN"":12 }" 'STOPSCREEN
End If

Sub Screen001_Init()
	
End Sub