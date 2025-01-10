Option Explicit
Randomize



Const cGameName = "SexyGirl"
Const cloneGameName = "playboy"
Const UseSolenoids=1,UseLamps=True,UseGI=0,UseSyn=1,SSolenoidOn="SolOn",SSolenoidOff="Soloff",SFlipperOn="FlipperUpLeft",SFlipperOff="FlipperDown"
Const SCoin="coin3"

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

LoadVPM "01560000","Bally.vbs", 3.36Dim bsTrough, dtDrop, bsSaucer, balls, scount, brain, h, screens(190),plungerIM, xxDim Language
Dim NumOfBalls
Dim BIP

'*****************************************************
'*                     Options                       *
'*****************************************************

Const GI_ON					= 1					' 0 or 1 to disable or enable GI

'Instruction Cards
Language = 1  ' 0 = German   1 = English
NumOfBalls = 1   ' 0 = 3ball    1 = 5ball

'*****************************************************
'*               Fluppers Flashers                   *
'*****************************************************

Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4
FlasherLight1.IntensityScale = 0
Flasherlight2.IntensityScale = 0
Flasherlight3.IntensityScale = 0
'Flasherlight4.IntensityScale = 0


'*** white flasher ***
Sub FlasherFlash1_Timer()
	dim flashx3, matdim
	If not FlasherFlash1.TimerEnabled Then 
		FlasherFlash1.TimerEnabled = True
		FlasherFlash1.visible = 1
		FlasherLit1.visible = 1
	End If
	flashx3 = FlashLevel1 * FlashLevel1 * FlashLevel1
	Flasherflash1.opacity = 1000 * flashx3
	FlasherLit1.BlendDisableLighting = 10 * flashx3
	Flasherbase1.BlendDisableLighting =  flashx3
	FlasherLight1.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel1)
	FlasherLit1.material = "domelit" & matdim
	FlashLevel1 = FlashLevel1 * 0.85 - 0.01
	If FlashLevel1 < 0.15 Then
		FlasherLit1.visible = 0
	Else
		FlasherLit1.visible = 1
	end If
	If FlashLevel1 < 0 Then
		FlasherFlash1.TimerEnabled = False
		FlasherFlash1.visible = 0
	End If
End Sub

'*** Red flasher ***
Sub FlasherFlash2_Timer()
	dim flashx3, matdim
	If not Flasherflash2.TimerEnabled Then 
		Flasherflash2.TimerEnabled = True
		Flasherflash2.visible = 1
		Flasherlit2.visible = 1
	End If
	flashx3 = FlashLevel2 * FlashLevel2 * FlashLevel2
	Flasherflash2.opacity = 1500 * flashx3
	Flasherlit2.BlendDisableLighting = 10 * flashx3
	Flasherbase2.BlendDisableLighting =  flashx3
	Flasherlight2.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel2)
	Flasherlit2.material = "domelit" & matdim
	FlashLevel2 = FlashLevel2 * 0.9 - 0.01
	If FlashLevel2 < 0.15 Then
		Flasherlit2.visible = 0
	Else
		Flasherlit2.visible = 1
	end If
	If FlashLevel2 < 0 Then
		Flasherflash2.TimerEnabled = False
		Flasherflash2.visible = 0
	End If
End Sub

'*** blue flasher ***
Sub FlasherFlash3_Timer()
	dim flashx3, matdim
	If not Flasherflash3.TimerEnabled Then 
		Flasherflash3.TimerEnabled = True
		Flasherflash3.visible = 1
		Flasherlit3.visible = 1
	End If
	flashx3 = FlashLevel3 * FlashLevel3 * FlashLevel3
	Flasherflash3.opacity = 8000 * flashx3
	Flasherlit3.BlendDisableLighting = 10 * flashx3
	Flasherbase3.BlendDisableLighting =  flashx3
	Flasherlight3.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	Flasherlit3.material = "domelit" & matdim
	FlashLevel3 = FlashLevel3 * 0.85 - 0.01
	If FlashLevel3 < 0.15 Then
		Flasherlit3.visible = 0
	Else
		Flasherlit3.visible = 1
	end If
	If FlashLevel3 < 0 Then
		Flasherflash3.TimerEnabled = False
		Flasherflash3.visible = 0
	End If
End Sub

'*****************************************************
'*                      Timers                       *
'*****************************************************
Dim AA
AA=0

Sub Flr_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:FlashLevel1 = 1 : FlasherFlash1_Timer
    Case 1:FlashLevel3 = 1 : FlasherFlash3_Timer
    Case 2:FlashLevel2 = 1 : FlasherFlash2_Timer
    End Select
end sub

Sub Fll_Timer
    FlashLevel3 = 1 : FlasherFlash3_Timer
end sub

Sub Flu_Timer
    If AA=0 Then FlashLevel2 = 1 : FlasherFlash2_Timer:AA=1:Exit Sub
    If AA=1 Then FlashLevel1 = 1 : FlasherFlash1_Timer:AA=0
end sub

Sub Flashoff_Timer
    Fll.enabled=false
    Flu.enabled=false
    Flr.enabled=false
    Flashoff.enabled=false
end sub


Dim x

Sub GiEffect(value) ' value is the duration of the blink
    For each x in aGi
        x.Duration 2, 500 * value, 1
    Next
End Sub


Dim colornow, TotalColor
TotalColor=5

Sub colorgi
	colornow = colornow + 1

	 If colornow=1 Then Dim x:For each x in aGi:x.color=RGB(255,0,0):Next:Exit Sub
	 If colornow=2 Then Dim y:For each y in aGi:y.color=RGB(0,0,255):Next:Exit Sub
	 If colornow=3 Then Dim z:For each z in aGi:z.color=RGB(255,255,0):Next:Exit Sub
	 If colornow=4 Then Dim r:For each r in aGi:r.color=RGB(255,0,255):Next:Exit Sub
	 If colornow=5 Then Dim w:For each w in aGi:w.color=RGB(255,255,155):Next
	If colornow = TotalColor Then colornow = 0
End Sub


'*****************************************************
'*        STAT's Modified Change Team script         *
'*****************************************************
' ********************************************
' one Table, all Teams, STAT

Dim TotalTeams, TeamNow, TotalTeamsb, TeamNowb

TotalTeams = 10

Sub ChangeTeam
	TeamNow = TeamNow + 1
	Table1.BallImage = "ball" &TeamNow
If TeamNow > 1 Then
		Wall9.image = "Plastics-"&TeamNow
		Wall17.image = "Plastics-"&TeamNow
		Wall20.image = "Plastics-"&TeamNow
		Wall21.image = "Plastics-"&TeamNow
		Wall22.image = "Plastics-"&TeamNow
		Wall23.image = "Plastics-"&TeamNow
		Wall26.image = "Plastics-"&TeamNow
		Wall27.image = "Plastics-"&TeamNow
		Wall28.image = "Plastics-"&TeamNow
		Primitive54.image = "Bumper Cap-"&TeamNow
		Primitive56.image = "Bumper Cap-"&TeamNow
		Primitive66.image = "Bumper Cap-"&TeamNow
		Primitive2.image = "Apron Sexy Girl No Cards-"&TeamNow
	Else
		Wall9.image = "Plastics"
		Wall17.image = "Plastics"
		Wall20.image = "Plastics"
		Wall21.image = "Plastics"
		Wall22.image = "Plastics"
		Wall23.image = "Plastics"
		Wall26.image = "Plastics"
		Wall27.image = "Plastics"
		Wall28.image = "Plastics"
		Primitive54.image = "Bumper Cap"
		Primitive56.image = "Bumper Cap"
		Primitive66.image = "Bumper Cap"
        Primitive2.image = "Apron Sexy Girl No Cards"
	End If
	If TeamNow = TotalTeams Then TeamNow = 0
End Sub

TotalTeamsb = 24

Sub ChangeTeamb
	TeamNowb = TeamNowb + 1
	If TeamNowb > 1 Then
		Primitive54.image = "Bumper Cap-"&TeamNowb
		Primitive56.image = "Bumper Cap-"&TeamNowb
		Primitive66.image = "Bumper Cap-"&TeamNowb
	Else
		Primitive54.image = "Bumper Cap"
		Primitive56.image = "Bumper Cap"
		Primitive66.image = "Bumper Cap"
	End If
	If TeamNowb = TotalTeamsb Then TeamNowb = 0
End Sub
'*****************************************************
'*                     Solonoids                     *
'*****************************************************SolCallback(7) = "bsTrough.SolOut"SolCallback(6)     = "vpmSolSound ""Knocker"","'SolCallback(12)      = "vpmSolSound ""Sling"","'SolCallback(14)      = "vpmSolSound ""Sling"","'SolCallback(9)        = "vpmSolSound ""Bumper"","'SolCallback(10)        = "vpmSolSound ""Bumper"","'SolCallback(11)        = "vpmSolSound ""Bumper"","SolCallback(8)      = "bsSaucer.SolOut"SolCallback(13) = "SolTargetReset"SolCallback(19)     	= "solGI"
SolCallback(sLLFlipper) = "solLFlipper"
SolCallback(sLRFlipper) = "solRFlipper"
'*****************************************************
'*                    Table Init                     *
'*****************************************************Sub Table1_Init
 		With Controller		.GameName = cloneGameName		.SplashInfoLine = "Christmas Pinball (Arkon 1980)" & vbNewLine & " VPX table by Mickey-Lizard" & vbNewLine & "Graphics and Sound MOD by Xenonph v2.0"		.HandleKeyboard = False		.ShowTitle = False		.ShowDMDOnly = True	    .ShowFrame = False
    Controller.Games("playboy").Settings.Value("sound")=0 		SetBallDip		On Error Resume Next			.Run			.Hidden=1			If Err Then MsgBox Err.Description		On Error Goto 0	End With' ' Main Timer init	PinMAMETimer.Interval = PinMAMEInterval	PinMAMETimer.Enabled = True
          Dim x
          x = INT(1 * RND(1) )
          Select Case x
          Case 0:PlayMusic"D.Mode\001. Depeche Mode - Enjoy the Silence.mp3":ITrigger.timerinterval=254000:ITrigger.timerenabled=1
          End Select' Nudging	vpmNudge.TiltSwitch = 7	vpmNudge.Sensitivity = 1	vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingShot)	Set bsTrough = New cvpmBallStack 												' Trough handler	bsTrough.InitSw 0,8,0,0,0,0,0,0	bsTrough.InitKick Kickout, 60, 4	bsTrough.Balls = 1	Set dtDrop = New cvpmDropTarget	dtDrop.InitDrop Array(Target1,Target2,Target3,Target4,Target5),Array(1,2,3,4,5)	'dtDrop.InitSnd "Drop","Reset"	Set bsSaucer = New cvpmBallStack	bsSaucer.InitSaucer GrottoKicker,32,0,20	'bsSaucer.InitExitSnd "Saucer","Saucer"	if Controller.Dip(0)+Controller.Dip(1)+Controller.Dip(2)+Controller.Dip(3)  < 1 then Controller.Pause=true : msgBox "Please select your game options!" & vbNewLine & "You will only have to do this once!" : vpmShowDips : Controller.Pause=false
CheckInstructionCards

Dim DesktopMode:DesktopMode = Table1.ShowDT

If DesktopMode = True Then
		For each xx in aSDO
			xx.visible = 1
		Next

		Light1Player.visible = 1
		Light2Player.visible = 1
		Light3Player.visible = 1
		Light4Player.visible = 1
		BallInPlayLight.visible = 1
		MatchLight.visible = 1
		GameOverLight.visible = 1
		Light57.visible = 1
        Light58.visible = 1
        Light59.visible = 1
		Ramp1730.visible = True
		Ramp1729.visible = True
		Ramp1731.visible = True
		Ramp1732.visible = True
		Ramp1.visible = True
End If
If DesktopMode = False Then
		For each xx in aSDO
			xx.visible = 0
		Next

		Light1Player.visible = 0
		Light2Player.visible = 0
		Light3Player.visible = 0
		Light4Player.visible = 0
		BallInPlayLight.visible = 0
		MatchLight.visible = 0
		GameOverLight.visible = 0
		Light57.visible = 0
        Light58.visible = 0
        Light59.visible = 0
		Ramp1730.visible = False
		Ramp1729.visible = False
		Ramp1731.visible = False
		Ramp1732.visible = False
		Ramp1.visible = False
End If

End SubSub SetBallDip	With Controller		.Dip(0) = &HE2		'.Dip(0) = &H44		.Dip(2) = &HD1		'.Dip(2) = &HDD		.Dip(3) = &H01		'.Dip(3) = &H01		if balls < 4 then 	' For 3 balls per game			.Dip(1) = &H6A		else 				' For 5 balls per game			.Dip(1) = &HEA		end if	end withEnd Sub
'*****************************************************
'*                   Drop Targets                    *
'*****************************************************
Sub Target1_Hit   : PlaySound SoundFX("Drop",DOFContactors) : Controller.Switch(1) = True :PlaySound"CPE15": End Sub
Sub Target2_Hit   : PlaySound SoundFX("Drop",DOFContactors) : Controller.Switch(2) = True :PlaySound"CPE15": End Sub
Sub Target3_Hit   : PlaySound SoundFX("Drop",DOFContactors) : Controller.Switch(3) = True :PlaySound"CPE15": End Sub
Sub Target4_Hit   : PlaySound SoundFX("Drop",DOFContactors) : Controller.Switch(4) = True :PlaySound"CPE15": End Sub
Sub Target5_Hit   : PlaySound SoundFX("Drop",DOFContactors) : Controller.Switch(5) = True :PlaySound"CPE15": End Sub
Sub SolTargetReset(enabled)
	if enabled then	PlaySound SoundFX("Reset",DOFContactors)
	dtDrop.SolDropUp enabled
End Sub
'*****************************************************
'*                Keyboard Handlers                  *
'*****************************************************Sub Table1_KeyDown(ByVal keycode)
	'If KeyCode = RightMagnaSave Then ChangeTeamb:GiEffect 1
	If KeyCode = RightMagnaSave Then NextTrack
    If KeyCode = 3 Then colorgi
	If KeyCode = LeftMagnaSave Then ChangeTeam:GiEffect 1
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("nudge", 0), 0, 1, 0, 0.25
	If KeyCode=PlungerKey Then Plunger.Pullback:PlaySound SoundFX("PlungerPull",DOFContactors)
	If vpmKeyDown(KeyCode) Then Exit Sub

End Sub

'******************************
'*     HiRez00: Music Mod     *
'******************************

Dim musicNum
Sub NextTrack

'If musicNum = 0 Then PlayMusic "EM\Stones\RS01_Track.mp3" End If

If musicNum = 0 Then PlayMusic "D.Mode\002. Depeche Mode - Little 15.mp3" End If
If musicNum = 1 Then PlayMusic "D.Mode\003. Depeche Mode - It's No Good.mp3" End If
If musicNum = 2 Then PlayMusic "D.Mode\004. Depeche Mode - Fly on the Windscreen.mp3" End If
If musicNum = 3 Then PlayMusic "D.Mode\005. Depeche Mode - I Feel You.mp3" End If
If musicNum = 4 Then PlayMusic "D.Mode\006. Depeche Mode - A Question of Lust.mp3" End If
If musicNum = 5 Then PlayMusic "D.Mode\007. Depeche Mode - Walking in My Shoes.mp3" End If
If musicNum = 6 Then PlayMusic "D.Mode\008. Depeche Mode - Photographic (Some Bizzare Version).mp3" End If
If musicNum = 7 Then PlayMusic "D.Mode\009. Depeche Mode - A Question of Time.mp3" End If
If musicNum = 8 Then PlayMusic "D.Mode\010. Depeche Mode - Dream On.mp3" End If
If musicNum = 9 Then PlayMusic "D.Mode\011. Depeche Mode - Dreaming of Me.mp3" End If
If musicNum = 10 Then PlayMusic "D.Mode\012. Depeche Mode - Somebody.mp3" End If
If musicNum = 11 Then PlayMusic "D.Mode\013. Depeche Mode - Stripped.mp3" End If
If musicNum = 12 Then PlayMusic "D.Mode\014. Depeche Mode - World in My Eyes.mp3" End If
If musicNum = 13 Then PlayMusic "D.Mode\015. Depeche Mode - Policy of Truth.mp3" End If
If musicNum = 14 Then PlayMusic "D.Mode\016. Depeche Mode - In Your Room.mp3" End If
If musicNum = 15 Then PlayMusic "D.Mode\017. Depeche Mode - Useless.mp3" End If
If musicNum = 16 Then PlayMusic "D.Mode\018. Depeche Mode - Suffer Well.mp3" End If
If musicNum = 17 Then PlayMusic "D.Mode\019. Depeche Mode - Never Let Me Down Again.mp3" End If
If musicNum = 18 Then PlayMusic "D.Mode\020. Depeche Mode - People Are People.mp3" End If
If musicNum = 19 Then PlayMusic "D.Mode\021. Depeche Mode - Everything Counts.mp3" End If
If musicNum = 20 Then PlayMusic "D.Mode\022. Depeche Mode - Home.mp3" End If
If musicNum = 21 Then PlayMusic "D.Mode\023. Depeche Mode - It's Called a Heart.mp3" End If
If musicNum = 22 Then PlayMusic "D.Mode\024. Depeche Mode - Leave in Silence.mp3" End If
If musicNum = 23 Then PlayMusic "D.Mode\025. Depeche Mode - Barrel of a Gun.mp3" End If
If musicNum = 24 Then PlayMusic "D.Mode\026. Depeche Mode - Love in Itself.mp3" End If
If musicNum = 25 Then PlayMusic "D.Mode\027. Depeche Mode - Told You So.mp3" End If
If musicNum = 26 Then PlayMusic "D.Mode\028. Depeche Mode - Shake the Disease.mp3" End If
If musicNum = 27 Then PlayMusic "D.Mode\029. Depeche Mode - See You.mp3" End If
If musicNum = 28 Then PlayMusic "D.Mode\030. Depeche Mode - Precious.mp3" End If
If musicNum = 29 Then PlayMusic "D.Mode\031. Depeche Mode - Personal Jesus.mp3" End If
If musicNum = 30 Then PlayMusic "D.Mode\032. Depeche Mode - Blasphemous Rumours.mp3" End If
If musicNum = 31 Then PlayMusic "D.Mode\033. Depeche Mode - The Meaning of Love.mp3" End If
If musicNum = 32 Then PlayMusic "D.Mode\034. Depeche Mode - Flexible.mp3" End If
If musicNum = 33 Then PlayMusic "D.Mode\035. Depeche Mode - Pipeline.mp3" End If
If musicNum = 34 Then PlayMusic "D.Mode\036. Depeche Mode - Now This is Fun.mp3" End If
If musicNum = 35 Then PlayMusic "D.Mode\037. Depeche Mode - Work Hard.mp3" End If
If musicNum = 36 Then PlayMusic "D.Mode\038. Depeche Mode - New Life.mp3" End If
If musicNum = 37 Then PlayMusic "D.Mode\039. Depeche Mode - Everything Counts Live.mp3" End If
If musicNum = 38 Then PlayMusic "D.Mode\040. Depeche Mode - Get the Balance Right.mp3" End If
If musicNum = 39 Then PlayMusic "D.Mode\041. Depeche Mode - Condemnation.mp3" End If
If musicNum = 40 Then PlayMusic "D.Mode\042. Depeche Mode - Just Can't Get Enough.mp3" End If
If musicNum = 41 Then PlayMusic "D.Mode\043. Depeche Mode - Strangelove.mp3" End If
If musicNum = 42 Then PlayMusic "D.Mode\044. Depeche Mode - Only When I Lose Myself.mp3" End If
If musicNum = 43 Then PlayMusic "D.Mode\045. Depeche Mode - Behind the Wheel.mp3" End If
If musicNum = 44 Then PlayMusic "D.Mode\046. Depeche Mode - Master and Servant.mp3" End If
If musicNum = 45 Then PlayMusic "D.Mode\047. Depeche Mode - Martyr.mp3" End If
If musicNum = 46 Then PlayMusic "D.Mode\001. Depeche Mode - Enjoy the Silence.mp3" End If
musicNum = (musicNum + 1) mod 47

End Sub

Sub Table1_MusicDone
        NextTrack

End Sub
Sub Table1_KeyUp(ByVal keycode)
	If KeyCode = 6 Then
            PlaySound"fx_coin" 
            Flr.enabled=true:Flr.interval=100:Flashoff.enabled=true:Flashoff.interval=5000
			Dim x
			x = INT(8 * RND(1) )
			Select Case x
			Case 0:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC01"
			Case 1:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC02"
			Case 2:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC03"
			Case 3:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC04"
            Case 4:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC36"
            Case 5:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC37"
            Case 6:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC38"
            Case 7:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC39"
			End Select
			end if
	If KeyCode = 4 Then
            PlaySound"fx_coin" 
            Flr.enabled=true:Flr.interval=100:Flashoff.enabled=true:Flashoff.interval=5000
			Dim y
			y = INT(8 * RND(1) )
			Select Case y
			Case 0:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC01"
			Case 1:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC02"
			Case 2:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC03"
			Case 3:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC04"
            Case 4:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC36"
            Case 5:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC37"
            Case 6:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC38"
            Case 7:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":PlaySound"CPC39"
			End Select
			end if
	If KeyCode = 2 Then
		'EndMusic:StopSound"CPC01":StopSound"CPC02":StopSound"CPC03":StopSound"CPC04":StopSound"CPC36":StopSound"CPC37":StopSound"CPC38":StopSound"CPC39":StopSound"CPD01":StopSound"CPD02":StopSound"CPD03":PlaySound"CPS01":GiEffect 7
	End If
	If KeyCode=PlungerKey Then Plunger.Fire:PlaySound SoundFX("Plunger",DOFContactors)
	If KeyCode=PlungerKey and BIP=1 Then StopSound"CPW02":PlaySound"CPE10"

    If KeyCode=PlungerKey and BIP=0 Then
			  playsound"plungerreleasefree"  
              End If 
	If vpmKeyUp(KeyCode) Then Exit Sub
End Sub

'PLUNGER TRIGGER
'Sub Trigger1_Hit()
              'Drain.timerenabled=0:BallReleaseGate.timerenabled=0:GTrigger.timerenabled=0:RTrigger.timerenabled=0:ITrigger.timerenabled=0:GateR.timerenabled=0
              'Flu.enabled=true:Flu.interval=300
              'Flr.enabled=False
              'Dim z
              'z = INT(27 * RND(1) )
              'Select Case z
              'Case 0:PlayMusic"Pink Floyd - Another Brick in the Wall, Pt. 2 - The Wall.mp3":GateL.timerinterval=226000:GateL.timerenabled=1
              'Case 1:PlayMusic"Pink Floyd - Comfortably Numb - The Wall.mp3":GateL.timerinterval=378000:GateL.timerenabled=1
              'Case 2:PlayMusic"Pink Floyd - Echoes - Echoes.mp3":GateL.timerinterval=990000:GateL.timerenabled=1
              'Case 3:PlayMusic"Pink Floyd - Final Cut - Final CutThe.mp3":GateL.timerinterval=288000:GateL.timerenabled=1
              'Case 4:PlayMusic"Pink Floyd - Have a Cigar - Wish You Were Here.mp3":GateL.timerinterval=310000:GateL.timerenabled=1
              'Case 5:PlayMusic"Pink Floyd - Hey You - The Wall.mp3":GateL.timerinterval=283000:GateL.timerenabled=1
              'Case 6:PlayMusic"Pink Floyd - High Hopes - The Division Bell.mp3":GateL.timerinterval=478000:GateL.timerenabled=1
              'Case 7:PlayMusic"Pink Floyd - In The Flesh - The Wall.mp3":GateL.timerinterval=199000:GateL.timerenabled=1
              'Case 8:PlayMusic"Pink Floyd - Learning To Fly - A Momentary Lapse Of Reason.mp3":GateL.timerinterval=295000:GateL.timerenabled=1
              'Case 9:PlayMusic"Pink Floyd - Money  - The Dark Side of the Moon.mp3":GateL.timerinterval=374000:GateL.timerenabled=1
              'Case 10:PlayMusic"Pink Floyd - Mother - The Wall.mp3":GateL.timerinterval=332000:GateL.timerenabled=1
              'Case 11:PlayMusic"Pink Floyd - On The Turning Away - A Momentary Lapse Of Reason.mp3":GateL.timerinterval=332000:GateL.timerenabled=1
              'Case 12:PlayMusic"Pink Floyd - One Of These Days - Meddle.mp3":GateL.timerinterval=321000:GateL.timerenabled=1
              'Case 13:PlayMusic"Pink Floyd - Run Like Hell - The Wall.mp3":GateL.timerinterval=258000:GateL.timerenabled=1
              'Case 14:PlayMusic"Pink Floyd - Sheep - Echoes.mp3":GateL.timerinterval=562000:GateL.timerenabled=1
              'Case 15:PlayMusic"Pink Floyd - Shine on You Crazy Diamond, Pts. 1-5 - Wish You Were Here.mp3":GateL.timerinterval=812000:GateL.timerenabled=1
              'Case 16:PlayMusic"Pink Floyd - Take It Back - The Division Bell.mp3":GateL.timerinterval=371000:GateL.timerenabled=1     
              'Case 17:PlayMusic"Pink Floyd - The Fletcher Memorial Home - Final Cut.mp3":GateL.timerinterval=248000:GateL.timerenabled=1
              'Case 18:PlayMusic"Pink Floyd - The Great Gig in the Sky  - The Dark Side of the Moon.mp3":GateL.timerinterval=279000:GateL.timerenabled=1
              'Case 19:PlayMusic"Pink Floyd - The Gunners Dream - Final Cut.mp3":GateL.timerinterval=305000:GateL.timerenabled=1
              'Case 20:PlayMusic"Pink Floyd - The Happiest Days of Our Lives - The Wall.mp3":GateL.timerinterval=96000:GateL.timerenabled=1
              'Case 21:PlayMusic"Pink Floyd - The Hero's Return - Final Cut.mp3":GateL.timerinterval=179000:GateL.timerenabled=1
              'Case 22:PlayMusic"Pink Floyd - Time  - The Dark Side of the Moon.mp3":GateL.timerinterval=406000:GateL.timerenabled=1
              'Case 23:PlayMusic"Pink Floyd - Us and Them  - The Dark Side of the Moon.mp3":GateL.timerinterval=442000:GateL.timerenabled=1
              'Case 24:PlayMusic"Pink Floyd - What Do You Want from Me - The Division Bell.mp3":GateL.timerinterval=261000:GateL.timerenabled=1
              'Case 25:PlayMusic"Pink Floyd - Wish You Were Here - Wish You Were Here.mp3":GateL.timerinterval=326000:GateL.timerenabled=1
              'Case 26:PlayMusic"Pink Floyd - Young Lust - The Wall.mp3":GateL.timerinterval=193000:GateL.timerenabled=1
              'End Select
'End Sub





'*****************************************************
'*                     Flippers                      *
'*****************************************************

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("FlipperUp",DOFContactors):Flipper1.RotateToEnd
     Else
         PlaySound SoundFX("FlipperDown",DOFContactors):Flipper1.RotateToStart
     End If
End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("FlipperUp",DOFContactors):Flipper2.RotateToEnd
     Else
         PlaySound SoundFX("FlipperDown",DOFContactors):Flipper2.RotateToStart
     End If
End Sub


Sub FlipperTimer_Timer()
   LFlip.RotY = Flipper1.CurrentAngle
   RFlip.RotY = Flipper2.CurrentAngle 
End Sub	

'*****************************************************
'*                    Sling Shots                    *
'*****************************************************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot : vpmTimer.PulseSwitch 36, 0, 0
    PlaySound SoundFX("SlingShot",DOFContactors), 0, 1, 0.05, 0.05
    PlaySound"CPE10"
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled=0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot : vpmTimer.PulseSwitch 37, 0, 0
    PlaySound SoundFX("SlingShot",DOFContactors),0,1,-0.05,0.05
    PlaySound"CPE10"
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled=0
    End Select
    LStep = LStep + 1
End Sub

'*****************************************************
'*                       Gates                       *
'*****************************************************

Sub GateTimer_Timer()
   GateRP.RotZ = ABS(GateR.currentangle)
   GateLP.RotZ = ABS(GateL.currentangle)
End Sub	

 Sub GateR_Hit():PlaySound "gate2":BallReleaseGate.timerenabled=0:GTrigger.timerenabled=0:BIP=0:FlashLevel2 = 1 : FlasherFlash2_Timer
 
 End Sub
 Sub GateR_timer
    Dim x
    x = INT(27 * RND(1) )
    Select Case x
            'Case 0:PlayMusic"Pink Floyd - Another Brick in the Wall, Pt. 2 - The Wall.mp3":ITrigger.timerinterval=226000:ITrigger.timerenabled=1
           'Case 1:PlayMusic"Pink Floyd - Comfortably Numb - The Wall.mp3":ITrigger.timerinterval=378000:ITrigger.timerenabled=1
            'Case 2:PlayMusic"Pink Floyd - Echoes - Echoes.mp3":ITrigger.timerinterval=990000:ITrigger.timerenabled=1
            'Case 3:PlayMusic"Pink Floyd - Final Cut - Final CutThe.mp3":ITrigger.timerinterval=288000:ITrigger.timerenabled=1
            'Case 4:PlayMusic"Pink Floyd - Have a Cigar - Wish You Were Here.mp3":ITrigger.timerinterval=310000:ITrigger.timerenabled=1
            'Case 5:PlayMusic"Pink Floyd - Hey You - The Wall.mp3":ITrigger.timerinterval=283000:ITrigger.timerenabled=1
            'Case 6:PlayMusic"Pink Floyd - High Hopes - The Division Bell.mp3":ITrigger.timerinterval=478000:ITrigger.timerenabled=1
            'Case 7:PlayMusic"Pink Floyd - In The Flesh - The Wall.mp3":ITrigger.timerinterval=199000:ITrigger.timerenabled=1
            'Case 8:PlayMusic"Pink Floyd - Learning To Fly - A Momentary Lapse Of Reason.mp3":ITrigger.timerinterval=295000:ITrigger.timerenabled=1
            'Case 9:PlayMusic"Pink Floyd - Money  - The Dark Side of the Moon.mp3":ITrigger.timerinterval=374000:ITrigger.timerenabled=1
            'Case 10:PlayMusic"Pink Floyd - Mother - The Wall.mp3":ITrigger.timerinterval=332000:ITrigger.timerenabled=1
            'Case 11:PlayMusic"Pink Floyd - On The Turning Away - A Momentary Lapse Of Reason.mp3":ITrigger.timerinterval=332000:ITrigger.timerenabled=1
            'Case 12:PlayMusic"Pink Floyd - One Of These Days - Meddle.mp3":ITrigger.timerinterval=321000:ITrigger.timerenabled=1
            'Case 13:PlayMusic"Pink Floyd - Run Like Hell - The Wall.mp3":ITrigger.timerinterval=258000:ITrigger.timerenabled=1
            'Case 14:PlayMusic"Pink Floyd - Sheep - Echoes.mp3":ITrigger.timerinterval=562000:ITrigger.timerenabled=1
            'Case 15:PlayMusic"Pink Floyd - Shine on You Crazy Diamond, Pts. 1-5 - Wish You Were Here.mp3":ITrigger.timerinterval=812000:ITrigger.timerenabled=1
            'Case 16:PlayMusic"Pink Floyd - Take It Back - The Division Bell.mp3":ITrigger.timerinterval=371000:ITrigger.timerenabled=1
            'Case 17:PlayMusic"Pink Floyd - The Fletcher Memorial Home - Final Cut.mp3":ITrigger.timerinterval=248000:ITrigger.timerenabled=1
            'Case 18:PlayMusic"Pink Floyd - The Great Gig in the Sky  - The Dark Side of the Moon.mp3":ITrigger.timerinterval=279000:ITrigger.timerenabled=1
            'Case 19:PlayMusic"Pink Floyd - The Gunners Dream - Final Cut.mp3":ITrigger.timerinterval=305000:ITrigger.timerenabled=1
            'Case 20:PlayMusic"Pink Floyd - The Happiest Days of Our Lives - The Wall.mp3":ITrigger.timerinterval=96000:ITrigger.timerenabled=1
            'Case 21:PlayMusic"Pink Floyd - The Hero's Return - Final Cut.mp3":ITrigger.timerinterval=179000:ITrigger.timerenabled=1
            'Case 22:PlayMusic"Pink Floyd - Time  - The Dark Side of the Moon.mp3":ITrigger.timerinterval=406000:ITrigger.timerenabled=1
            'Case 23:PlayMusic"Pink Floyd - Us and Them  - The Dark Side of the Moon.mp3":ITrigger.timerinterval=442000:ITrigger.timerenabled=1
            'Case 24:PlayMusic"Pink Floyd - What Do You Want from Me - The Division Bell.mp3":ITrigger.timerinterval=261000:ITrigger.timerenabled=1
            'Case 25:PlayMusic"Pink Floyd - Wish You Were Here - Wish You Were Here.mp3":ITrigger.timerinterval=326000:ITrigger.timerenabled=1
            'Case 26:PlayMusic"Pink Floyd - Young Lust - The Wall.mp3":ITrigger.timerinterval=193000:ITrigger.timerenabled=1
            
    End Select
    GateR.timerenabled=0
 End sub
 Sub GateL_Hit():PlaySound "gate2":FlashLevel1 = 1 : FlasherFlash1_Timer
 End Sub
 Sub GateL_timer
			Dim x
			x = INT(27 * RND(1) )
			Select Case x
            'Case 0:PlayMusic"Pink Floyd - Another Brick in the Wall, Pt. 2 - The Wall.mp3":GrottoGate.timerinterval=226000:GrottoGate.timerenabled=1
            'Case 1:PlayMusic"Pink Floyd - Comfortably Numb - The Wall.mp3":GrottoGate.timerinterval=378000:GrottoGate.timerenabled=1
            'Case 2:PlayMusic"Pink Floyd - Echoes - Echoes.mp3":GrottoGate.timerinterval=990000:GrottoGate.timerenabled=1
            'Case 3:PlayMusic"Pink Floyd - Final Cut - Final CutThe.mp3":GrottoGate.timerinterval=288000:GrottoGate.timerenabled=1
            'Case 4:PlayMusic"Pink Floyd - Have a Cigar - Wish You Were Here.mp3":GrottoGate.timerinterval=310000:GrottoGate.timerenabled=1
            'Case 5:PlayMusic"Pink Floyd - Hey You - The Wall.mp3":GrottoGate.timerinterval=283000:GrottoGate.timerenabled=1
            'Case 6:PlayMusic"Pink Floyd - High Hopes - The Division Bell.mp3":GrottoGate.timerinterval=478000:GrottoGate.timerenabled=1
            'Case 7:PlayMusic"Pink Floyd - In The Flesh - The Wall.mp3":GrottoGate.timerinterval=199000:GrottoGate.timerenabled=1
            'Case 8:PlayMusic"Pink Floyd - Learning To Fly - A Momentary Lapse Of Reason.mp3":GrottoGate.timerinterval=295000:GrottoGate.timerenabled=1
            'Case 9:PlayMusic"Pink Floyd - Money  - The Dark Side of the Moon.mp3":GrottoGate.timerinterval=374000:GrottoGate.timerenabled=1
            'Case 10:PlayMusic"Pink Floyd - Mother - The Wall.mp3":GrottoGate.timerinterval=332000:GrottoGate.timerenabled=1
            'Case 11:PlayMusic"Pink Floyd - On The Turning Away - A Momentary Lapse Of Reason.mp3":GrottoGate.timerinterval=332000:GrottoGate.timerenabled=1
            'Case 12:PlayMusic"Pink Floyd - One Of These Days - Meddle.mp3":GrottoGate.timerinterval=321000:GrottoGate.timerenabled=1
            'Case 13:PlayMusic"Pink Floyd - Run Like Hell - The Wall.mp3":GrottoGate.timerinterval=258000:GrottoGate.timerenabled=1
            'Case 14:PlayMusic"Pink Floyd - Sheep - Echoes.mp3":GrottoGate.timerinterval=562000:GrottoGate.timerenabled=1
            'Case 15:PlayMusic"Pink Floyd - Shine on You Crazy Diamond, Pts. 1-5 - Wish You Were Here.mp3":GrottoGate.timerinterval=812000:GrottoGate.timerenabled=1
            'Case 16:PlayMusic"Pink Floyd - Take It Back - The Division Bell.mp3":GrottoGate.timerinterval=371000:GrottoGate.timerenabled=1
            'Case 17:PlayMusic"Pink Floyd - The Fletcher Memorial Home - Final Cut.mp3":GrottoGate.timerinterval=248000:GrottoGate.timerenabled=1
            'Case 18:PlayMusic"Pink Floyd - The Great Gig in the Sky  - The Dark Side of the Moon.mp3":GrottoGate.timerinterval=279000:GrottoGate.timerenabled=1
            'Case 19:PlayMusic"Pink Floyd - The Gunners Dream - Final Cut.mp3":GrottoGate.timerinterval=305000:GrottoGate.timerenabled=1
            'Case 20:PlayMusic"Pink Floyd - The Happiest Days of Our Lives - The Wall.mp3":GrottoGate.timerinterval=96000:GrottoGate.timerenabled=1
            'Case 21:PlayMusic"Pink Floyd - The Hero's Return - Final Cut.mp3":GrottoGate.timerinterval=179000:GrottoGate.timerenabled=1
            'Case 22:PlayMusic"Pink Floyd - Time  - The Dark Side of the Moon.mp3":GrottoGate.timerinterval=406000:GrottoGate.timerenabled=1
            'Case 23:PlayMusic"Pink Floyd - Us and Them  - The Dark Side of the Moon.mp3":GrottoGate.timerinterval=442000:GrottoGate.timerenabled=1
            'Case 24:PlayMusic"Pink Floyd - What Do You Want from Me - The Division Bell.mp3":GrottoGate.timerinterval=261000:GrottoGate.timerenabled=1
            'Case 25:PlayMusic"Pink Floyd - Wish You Were Here - Wish You Were Here.mp3":GrottoGate.timerinterval=326000:GrottoGate.timerenabled=1
            'Case 26:PlayMusic"Pink Floyd - Young Lust - The Wall.mp3":GrottoGate.timerinterval=193000:GrottoGate.timerenabled=1           
			End Select
 GateL.timerenabled=0
 End sub

 Sub GrottoGate_Hit():PlaySound "gate2":End Sub
 Sub GrottoGate_timer
			Dim x
			x = INT(27 * RND(1) )
			Select Case x
              'Case 0:PlayMusic"Pink Floyd - Another Brick in the Wall, Pt. 2 - The Wall.mp3":GateL.timerinterval=226000:GateL.timerenabled=1
              'Case 1:PlayMusic"Pink Floyd - Comfortably Numb - The Wall.mp3":GateL.timerinterval=378000:GateL.timerenabled=1
              'Case 2:PlayMusic"Pink Floyd - Echoes - Echoes.mp3":GateL.timerinterval=990000:GateL.timerenabled=1
              'Case 3:PlayMusic"Pink Floyd - Final Cut - Final CutThe.mp3":GateL.timerinterval=288000:GateL.timerenabled=1
              'Case 4:PlayMusic"Pink Floyd - Have a Cigar - Wish You Were Here.mp3":GateL.timerinterval=310000:GateL.timerenabled=1
              'Case 5:PlayMusic"Pink Floyd - Hey You - The Wall.mp3":GateL.timerinterval=283000:GateL.timerenabled=1
              'Case 6:PlayMusic"Pink Floyd - High Hopes - The Division Bell.mp3":GateL.timerinterval=478000:GateL.timerenabled=1
              'Case 7:PlayMusic"Pink Floyd - In The Flesh - The Wall.mp3":GateL.timerinterval=199000:GateL.timerenabled=1
              'Case 8:PlayMusic"Pink Floyd - Learning To Fly - A Momentary Lapse Of Reason.mp3":GateL.timerinterval=295000:GateL.timerenabled=1
              'Case 9:PlayMusic"Pink Floyd - Money  - The Dark Side of the Moon.mp3":GateL.timerinterval=374000:GateL.timerenabled=1
              'Case 10:PlayMusic"Pink Floyd - Mother - The Wall.mp3":GateL.timerinterval=332000:GateL.timerenabled=1
              'Case 11:PlayMusic"Pink Floyd - On The Turning Away - A Momentary Lapse Of Reason.mp3":GateL.timerinterval=332000:GateL.timerenabled=1
              'Case 12:PlayMusic"Pink Floyd - One Of These Days - Meddle.mp3":GateL.timerinterval=321000:GateL.timerenabled=1
              'Case 13:PlayMusic"Pink Floyd - Run Like Hell - The Wall.mp3":GateL.timerinterval=258000:GateL.timerenabled=1
              'Case 14:PlayMusic"Pink Floyd - Sheep - Echoes.mp3":GateL.timerinterval=562000:GateL.timerenabled=1
              'Case 15:PlayMusic"Pink Floyd - Shine on You Crazy Diamond, Pts. 1-5 - Wish You Were Here.mp3":GateL.timerinterval=812000:GateL.timerenabled=1
              'Case 16:PlayMusic"Pink Floyd - Take It Back - The Division Bell.mp3":GateL.timerinterval=371000:GateL.timerenabled=1
              'Case 17:PlayMusic"Pink Floyd - The Fletcher Memorial Home - Final Cut.mp3":GateL.timerinterval=248000:GateL.timerenabled=1
              'Case 18:PlayMusic"Pink Floyd - The Great Gig in the Sky  - The Dark Side of the Moon.mp3":GateL.timerinterval=279000:GateL.timerenabled=1
              'Case 19:PlayMusic"Pink Floyd - The Gunners Dream - Final Cut.mp3":GateL.timerinterval=305000:GateL.timerenabled=1
              'Case 20:PlayMusic"Pink Floyd - The Happiest Days of Our Lives - The Wall.mp3":GateL.timerinterval=96000:GateL.timerenabled=1
              'Case 21:PlayMusic"Pink Floyd - The Hero's Return - Final Cut.mp3":GateL.timerinterval=179000:GateL.timerenabled=1
              'Case 22:PlayMusic"Pink Floyd - Time  - The Dark Side of the Moon.mp3":GateL.timerinterval=406000:GateL.timerenabled=1
              'Case 23:PlayMusic"Pink Floyd - Us and Them  - The Dark Side of the Moon.mp3":GateL.timerinterval=442000:GateL.timerenabled=1
              'Case 24:PlayMusic"Pink Floyd - What Do You Want from Me - The Division Bell.mp3":GateL.timerinterval=261000:GateL.timerenabled=1
              'Case 25:PlayMusic"Pink Floyd - Wish You Were Here - Wish You Were Here.mp3":GateL.timerinterval=326000:GateL.timerenabled=1
              'Case 26:PlayMusic"Pink Floyd - Young Lust - The Wall.mp3":GateL.timerinterval=193000:GateL.timerenabled=1
              
			End Select
 GrottoGate.timerenabled=0
 End sub 
 Sub BallReleaseGate_Hit():PlaySound "gate2":BIP=1:GateR.timerenabled=0:StopSound"CPD01":StopSound"CPD02":StopSound"CPD03":PlaySound"CPW02":GTrigger.timerinterval=2500:GTrigger.timerenabled=1:ITrigger.timerenabled=0:RTrigger.timerinterval=20000:RTrigger.timerenabled=1:End Sub

'*****************************************************
'*                  Switch Handling                  *
'*****************************************************
Sub GTrigger_Hit   : PlaySound "Rollover" : Controller.Switch(21) = True :GateR.timerenabled=0
            Flu.enabled=false
			Dim x
			x = INT(5 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE01"
			Case 1:PlaySound"CPE03"
			Case 2:PlaySound"CPE04"
			Case 3:PlaySound"CPE03"
			Case 4:PlaySound"CPE01"
			End Select
End Sub

Sub GTrigger_Timer:PlaySound"CPW02":End Sub

Sub GTrigger_UnHit : Controller.Switch(21) = False : End Sub
Sub ITrigger_Hit   : PlaySound "Rollover" :Controller.Switch(20) = True :GateR.timerenabled=0
            Flu.enabled=false
			Dim x
			x = INT(5 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE01"
			Case 1:PlaySound"CPE03"
			Case 2:PlaySound"CPE04"
			Case 3:PlaySound"CPE03"
			Case 4:PlaySound"CPE01"
			End Select
End Sub

 Sub ITrigger_Timer
            Dim x
            x = INT(27 * RND(1) )
            Select Case x
            'Case 0:PlayMusic"Pink Floyd - Another Brick in the Wall, Pt. 2 - The Wall.mp3":GateR.timerinterval=226000:GateR.timerenabled=1
            'Case 1:PlayMusic"Pink Floyd - Comfortably Numb - The Wall.mp3":GateR.timerinterval=378000:GateR.timerenabled=1
            'Case 2:PlayMusic"Pink Floyd - Echoes - Echoes.mp3":GateR.timerinterval=990000:GateR.timerenabled=1
            'Case 3:PlayMusic"Pink Floyd - Final Cut - Final CutThe.mp3":GateR.timerinterval=288000:GateR.timerenabled=1
            'Case 4:PlayMusic"Pink Floyd - Have a Cigar - Wish You Were Here.mp3":GateR.timerinterval=310000:GateR.timerenabled=1
            'Case 5:PlayMusic"Pink Floyd - Hey You - The Wall.mp3":GateR.timerinterval=283000:GateR.timerenabled=1
            'Case 6:PlayMusic"Pink Floyd - High Hopes - The Division Bell.mp3":GateR.timerinterval=478000:GateR.timerenabled=1
            'Case 7:PlayMusic"Pink Floyd - In The Flesh - The Wall.mp3":GateR.timerinterval=199000:GateR.timerenabled=1
            'Case 8:PlayMusic"Pink Floyd - Learning To Fly - A Momentary Lapse Of Reason.mp3":GateR.timerinterval=295000:GateR.timerenabled=1
            'Case 9:PlayMusic"Pink Floyd - Money  - The Dark Side of the Moon.mp3":GateR.timerinterval=374000:GateR.timerenabled=1
            'Case 10:PlayMusic"Pink Floyd - Mother - The Wall.mp3":GateR.timerinterval=332000:GateR.timerenabled=1
            'Case 11:PlayMusic"Pink Floyd - On The Turning Away - A Momentary Lapse Of Reason.mp3":GateR.timerinterval=332000:GateR.timerenabled=1
            'Case 12:PlayMusic"Pink Floyd - One Of These Days - Meddle.mp3":GateR.timerinterval=321000:GateR.timerenabled=1
            'Case 13:PlayMusic"Pink Floyd - Run Like Hell - The Wall.mp3":GateR.timerinterval=258000:GateR.timerenabled=1
            'Case 14:PlayMusic"Pink Floyd - Sheep - Echoes.mp3":GateR.timerinterval=562000:GateR.timerenabled=1
            'Case 15:PlayMusic"Pink Floyd - Shine on You Crazy Diamond, Pts. 1-5 - Wish You Were Here.mp3":GateR.timerinterval=812000:GateR.timerenabled=1
            'Case 16:PlayMusic"Pink Floyd - Take It Back - The Division Bell.mp3":GateR.timerinterval=371000:GateR.timerenabled=1
            'Case 17:PlayMusic"Pink Floyd - The Fletcher Memorial Home - Final Cut.mp3":GateR.timerinterval=248000:GateR.timerenabled=1
            'Case 18:PlayMusic"Pink Floyd - The Great Gig in the Sky  - The Dark Side of the Moon.mp3":GateR.timerinterval=279000:GateR.timerenabled=1
            'Case 19:PlayMusic"Pink Floyd - The Gunners Dream - Final Cut.mp3":GateR.timerinterval=305000:GateR.timerenabled=1
            'Case 20:PlayMusic"Pink Floyd - The Happiest Days of Our Lives - The Wall.mp3":GateR.timerinterval=96000:GateR.timerenabled=1
            'Case 21:PlayMusic"Pink Floyd - The Hero's Return - Final Cut.mp3":GateR.timerinterval=179000:GateR.timerenabled=1
            'Case 22:PlayMusic"Pink Floyd - Time  - The Dark Side of the Moon.mp3":GateR.timerinterval=406000:GateR.timerenabled=1
            'Case 23:PlayMusic"Pink Floyd - Us and Them  - The Dark Side of the Moon.mp3":GateR.timerinterval=442000:GateR.timerenabled=1
            'Case 24:PlayMusic"Pink Floyd - What Do You Want from Me - The Division Bell.mp3":GateR.timerinterval=261000:GateR.timerenabled=1
            'Case 25:PlayMusic"Pink Floyd - Wish You Were Here - Wish You Were Here.mp3":GateR.timerinterval=326000:GateR.timerenabled=1
            'Case 26:PlayMusic"Pink Floyd - Young Lust - The Wall.mp3":GateR.timerinterval=193000:GateR.timerenabled=1
            
            End Select
            ITrigger.timerenabled=0
 End Sub

Sub ITrigger_UnHit : Controller.Switch(20) = False : End Sub
Sub RTrigger_Hit   : PlaySound "Rollover" :Controller.Switch(19) = True :GateR.timerenabled=0
            Flu.enabled=false
			Dim x
			x = INT(5 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE01"
			Case 1:PlaySound"CPE03"
			Case 2:PlaySound"CPE04"
			Case 3:PlaySound"CPE03"
			Case 4:PlaySound"CPE01"
			End Select
End Sub

 Sub RTrigger_Timer:Dim x:x = INT(1 * RND(1) ):Select Case x
            Case 0:PlaySound"CPC09"
            
            
            End Select
            End Sub
Sub RTrigger_UnHit : Controller.Switch(19) = False : End Sub
Sub LTrigger_Hit   : PlaySound "Rollover" :Controller.Switch(18) = True :GateR.timerenabled=0
            Flu.enabled=false
			Dim x
			x = INT(5 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE01"
			Case 1:PlaySound"CPE03"
			Case 2:PlaySound"CPE04"
			Case 3:PlaySound"CPE03"
			Case 4:PlaySound"CPE01"
			End Select
End Sub
Sub LTrigger_UnHit : Controller.Switch(18) = False : End Sub
Sub RightOutlane_Hit   : PlaySound "Rollover" :Controller.Switch(22) = True :PlaySound"CPD10": End Sub
Sub RightOutlane_UnHit : Controller.Switch(22) = False : End Sub
Sub LeftOutlane_Hit   : PlaySound "Rollover" :Controller.Switch(23) = True :PlaySound"CPD11": End Sub
Sub LeftOutlane_UnHit : Controller.Switch(23) = False : End Sub
Sub RightInlane_Hit   : PlaySound "Rollover" :Controller.Switch(24) = True  
			Dim x
			x = INT(5 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE08"
			Case 1:PlaySound"CPE09"
			Case 2:PlaySound"CPE08"
			Case 3:PlaySound"CPE09"
			Case 4:PlaySound"CPE08"
			End Select
End Sub
Sub RightInlane_UnHit : Controller.Switch(24) = False : End Sub
Sub StarTrigger_Hit   : Controller.Switch(30) = True  
			Dim x
			x = INT(3 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE01"
			Case 1:PlaySound"CPE03"
			Case 2:PlaySound"CPE04"
			End Select
End Sub
Sub StarTrigger_UnHit : Controller.Switch(30) = False : End Sub
Sub FivekTrigger_Hit   : PlaySound "Rollover" :Controller.Switch(31) = True 
			Dim x
			x = INT(3 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE01"
			Case 1:PlaySound"CPE03"
			Case 2:PlaySound"CPE04"
			End Select
End Sub
Sub FivekTrigger_UnHit : Controller.Switch(31) = False : End Sub
Sub GrottoTrigger_Hit   : PlaySound "Rollover" :Controller.Switch(24) = True : End Sub
Sub GrottoTrigger_UnHit : Controller.Switch(24) = False : End Sub
Sub Drain_Hit : ClearBallID : bsTrough.AddBall Me : PlaySound "Drain" 
			Dim x
			x = INT(1 * RND(1) )
			Select Case x
			
            'Case 0:EndMusic:PlaySound"drain":Drain.timerinterval=1000:Drain.timerenabled=1:GiEffect 2
            
            
			End Select

BallReleaseGate.timerenabled=1

GrottoGate.timerenabled=0
GateL.timerenabled=0

End Sub

Sub drain_timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:PlaySound"CPD01":ITrigger.timerinterval=37000:ITrigger.timerenabled=1
    Case 1:PlaySound"CPD02":ITrigger.timerinterval=37000:ITrigger.timerenabled=1
    Case 2:PlaySound"CPD03":ITrigger.timerinterval=37000:ITrigger.timerenabled=1
    End Select
Drain.timerenabled=0
End sub 
Sub GrottoKicker_Hit : bsSaucer.AddBall 0:FlashLevel3 = 1 : FlasherFlash3_Timer:Fll.enabled=true:Fll.interval=400:GiEffect 23
    Dim x
    x = Int(1 * RND(1) )
    Select Case x
    Case 0:PlaySound"CPK01"
	
End Select
End Sub
Sub GrottoKicker_UnHit :PlaySound SoundFX("ballrelease",DOFContactors):PlaySound"CPE02":FlashLevel3 = 1 : FlasherFlash3_Timer
     Fll.enabled=False:Flu.enabled=true:Flu.interval=300
End Sub

Sub KickOut_UnHit :PlaySound SoundFX("ballrelease",DOFContactors):Drain.timerenabled=0: End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'                      Switch Handling                         '
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''Sub DropTargetSlingshot_Slingshot : vpmTimer.PulseSwitch 33, 0, 0 : End SubSub Bumper2_Hit : PlaySound SoundFX("Bumper",DOFContactors):vpmTimer.PulseSwitch 38, 0, 0 :PlaySound"CPE05":FlashLevel2 = 1 : FlasherFlash2_Timer: End SubSub Bumper1_Hit : PlaySound SoundFX("Bumper",DOFContactors):vpmTimer.PulseSwitch 39, 0, 0 :PlaySound"CPE06":FlashLevel1 = 1 : FlasherFlash1_Timer: End SubSub Bumper3_Hit : PlaySound SoundFX("Bumper",DOFContactors):vpmTimer.PulseSwitch 40, 0, 0 :PlaySound"CPE07":FlashLevel1 = 1 : FlasherFlash1_Timer:FlashLevel2 = 1 : FlasherFlash2_Timer: End Sub'*****************************************************
'*                Model And S Targets                *
'*****************************************************
 Sub sw17_Hit:vpmTimer.PulseSw 17:Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFContactors):FlashLevel1 = 1 : FlasherFlash1_Timer:FlashLevel2 = 1 : FlasherFlash2_Timer
 	If h = True Then
		ScreenTimer.Enabled = True:GiEffect 2						
	End If
			Dim x
			x = INT(4 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE11"
			Case 1:PlaySound"CPE12"
			Case 2:PlaySound"CPE13"
			Case 3:PlaySound"CPE14"
			End Select
End Sub
 Sub sw25_Hit:vpmTimer.PulseSw 25:Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFContactors):FlashLevel3 = 1 : FlasherFlash3_Timer
 	If h = True Then
		ScreenTimer.Enabled = True:GiEffect 2						
	End If
			Dim x
			x = INT(4 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE11"
			Case 1:PlaySound"CPE12"
			Case 2:PlaySound"CPE13"
			Case 3:PlaySound"CPE14"
			End Select
End Sub
 Sub sw26_Hit:vpmTimer.PulseSw 26:Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFContactors):FlashLevel3 = 1 : FlasherFlash3_Timer
 	If h = True Then
		ScreenTimer.Enabled = True:GiEffect 2						
	End If
			Dim x
			x = INT(4 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE11"
			Case 1:PlaySound"CPE12"
			Case 2:PlaySound"CPE13"
			Case 3:PlaySound"CPE14"
			End Select
End Sub
 Sub sw27_Hit:vpmTimer.PulseSw 27:Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFContactors):FlashLevel3 = 1 : FlasherFlash3_Timer
 	If h = True Then
		ScreenTimer.Enabled = True:GiEffect 2						
	End If
			Dim x
			x = INT(4 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE11"
			Case 1:PlaySound"CPE12"
			Case 2:PlaySound"CPE13"
			Case 3:PlaySound"CPE14"
			End Select
End Sub
 Sub sw28_Hit:vpmTimer.PulseSw 28:Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFContactors):FlashLevel3 = 1 : FlasherFlash3_Timer
 	If h = True Then
		ScreenTimer.Enabled = True:GiEffect 2						
	End If
			Dim x
			x = INT(4 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE11"
			Case 1:PlaySound"CPE12"
			Case 2:PlaySound"CPE13"
			Case 3:PlaySound"CPE14"
			End Select
End Sub
 Sub sw29_Hit:vpmTimer.PulseSw 29:Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFContactors):FlashLevel3 = 1 : FlasherFlash3_Timer
 	If h = True Then
		ScreenTimer.Enabled = True:GiEffect 2						
	End If
			Dim x
			x = INT(4 * RND(1) )
			Select Case x
			Case 0:PlaySound"CPE11"
			Case 1:PlaySound"CPE12"
			Case 2:PlaySound"CPE13"
			Case 3:PlaySound"CPE14"
			End Select
End Sub'*****************************************************
'*               Projector Handling                  *
'*****************************************************
balls = 3
scount = 71		' How many different screens?
brain = 1		' Contains the present screen									
h = False		' Projector off


Sub ScreenTimer_Timer					'here are the changing of pics
'	screens(brain - 1).IsDropped = True
		If brain = scount Then
			brain = 1
		End If 
'	screens(brain).IsDropped = False
	Projector.Image = "pic"&brain
	ScreenTimer.Enabled = False
	brain = brain + 1					
End Sub

Sub ProjectorOff()						'cut-off projector
'	screens(brain-1).IsDropped = True	
'	screens(0).IsDropped = False
	Projector.Image = "pic0"
	brain = 1							'reset to first pic at cut-off (like original???)
	h = False
End Sub

Sub ProjectorOn()						'switch projector on
	h = True
End Sub

'*****************************************************
'*                 Instruction Cards                 *
'*****************************************************
Sub CheckInstructionCards()
	If Language = 0 Then
		InstructionsL.image = "IGL"	
		If NumOfBalls = 0 Then
			InstructionsR.image = "IGR3-ball"
		Else
			InstructionsR.image = "IGR5-ball"
		End If
	Else
		InstructionsL.image = "IEL"
		If NumOfBalls = 0 Then
			InstructionsR.image = "IER3-ball"
		Else
			InstructionsR.image = "IER5-ball"	
		End If
	End If
End Sub


'*****************************************************
'*                      Lights                       *
'*****************************************************Set Lights(15)  = Light1Player
Set Lights(31)  = Light2Player
Set Lights(47)  = Light3Player
Set Lights(63)  = Light4Player
Set Lights(13)  = BallInPlayLight
Lights(1)  = array(Light1k,Light001)Lights(17) = array(Light2k,Light002)Lights(33) = array(Light3k,Light003)Lights(49) = array(Light14k,Light004)Lights(2)  = array(Light15k,Light005)Lights(18) = array(Light16k,Light006)Lights(34) = array(Light17k,Light007)Lights(50) = array(Light18k,Light008)Lights(3)  = array(Light19k,Light009)Lights(19) = array(Light10k,Light010)Set Lights(35) = Light120kSet Lights(55) = Light2XSet Lights(39) = Light3XSet Lights(23) = Light5XSet Lights(4)  = GLightSet Lights(20) = ILightSet Lights(36) = RLightSet Lights(52) = LLightSet Lights(5)  = SLightSet Lights(6)  = KickerGLight1Set Lights(22) = KickerILight1Set Lights(38) = KickerRLight1Set Lights(54) = KickerLLight1Set Lights(7)  = KickerSLight1Set Lights(21) = Kicker25kLightSet Lights(40) = JeanLightSet Lights(8)  = JoyLightSet Lights(24) = LinnLightSet Lights(56) = SusyLightSet Lights(9)  = EvaLightSet Lights(37) = Arrow5kLightSet Lights(57) = ArrowExtraLightSet Lights(41) = ArrowSpecialLightSet Lights(25) = TriggerLightSet Lights(44) = RightOutlaneLightSet Lights(60) = LeftOutlaneLightSet Lights(53) = TargetSpecialLightSet Lights(51) = GirlsSpecialLightSet Lights(43) = ExtraballLightSet Lights(59) = CreditLightSet Lights(45) = GameOverLightSet Lights(11) = ExtraballLight2Set Lights(29) = HighscoreLightSet Lights(27) = MatchLightSet Lights(61) = TiltLightSet LampCallBack = GetRef("UpdateMultipleLamps")Public Sub UpdateMultipleLamps
If GameOverLight.State = 0 Then   BallInPlayLight.State = 1   Else    BallInPlayLight.State = 0End IfIf 	GameOverLight.State = 1 Then				ProjectorOff()End IfIf 	Light2X.State = 1 Then	ProjectorOn()'Else	'ProjectorOff()       					'mode of operation "B"??End IfIf 	Light3X.State = 1 Then 	ProjectorOn()'Else	'ProjectorOff()							'mode of operation "B"??End IfIf 	Light5X.State = 1 Then	ProjectorOn()'Else	'ProjectorOff()							'mode of operation "B"??End IfEnd Sub
'*****************************************************
'*                    GI Lights                      *
'*****************************************************
Sub solGI (enabled)
	Dim xx
	debug.print enabled
	If GI_ON = 1 Then
		For each xx in aGI
			xx.state = enabled
		Next
	Else
		For each xx in aGI
			xx.state = 1
		Next
	End If
End Sub

'*****************************************************
'*                Dip Switch Settings                *
'*****************************************************
Sub editDips	Dim vpmDips : Set vpmDips = New cvpmDips	With vpmDips		'How to implent projector mode of operation ("A" or "B") to Dips ???		.AddForm  370,500,"Sexy Girl - DIP switch settings"		.AddFrame   2,  5, 115,"Balls per game",32768,Array("3 balls",0,"5 balls",32768)		.AddFrame   2, 53, 115,"Credits Display",&H00080000,Array("On",&H00080000,"Off",0)		.AddFrame   2,102, 115,"Match feature",&H00100000,Array("On",&H00100000,"Off",0)		.AddFrame   2,150, 115,"Drop Target Special",&H00200000,Array("Lit until collected",&H200000,"Lit until ball lost",0)		.AddFrame   2,198, 115,"5 Girls",&H00400000,Array("Held until made",&H400000,"Not held",0)		.AddFrame   2,248, 115,"G && L Triggers",&H20000000,Array("Tied together",&H20000000,"Not tied",0)		.AddFrame   2,298, 115,"I && R Triggers",&H10000000,Array("Tied together",&H10000000,"Not tied",0)		.AddFrame   2,348, 115,"Outlanes",&H00800000,Array("Both lit",&H800000,"Alternating",0)		.AddFrame   2,398, 115,"Sounds-Scoring", &H80, Array("Chime",0,"Noise",&H80)		.AddFrame   2,448, 115,"Sounds-Coin (no credit)",&H80000000,Array("Chime",0,"Noise",&H80000000)		.AddFrame 130,  5, 118,"High Score Award",&H00006000,Array("Replay",&H6000,"Extra Ball",&H4000,"No Award",0)		.AddFrame 130, 75, 118,"Rollover Extra && Special",&H40000000,Array("Hold until made",&H40000000,"Not Held",0)		.AddFrame 130,130, 118,"High game to date",&H00000060,Array("No award",0,"1 credit",&H20,"2 credits",&H40,"3 credits",&H60)		.AddFrame 130,211, 118,"Max. credits",&H00070000,Array("5 credits",0,"10 credits",&H10000,"15 credits",&H20000,"20 credits",&H30000,"25 credits",&H40000,"30 credits",&H50000,"35 credits",&H60000,"40 credits",&H70000)		.AddFrame 130,350, 118,"Coin Slot 2 (Key 3)",&H0F000000,Array("Same as Coin 1",&H0000000,"1/coin",&H1000000,"2/coin",&H2000000,_							   "3/coin",&H3000000,"4/coin",&H4000000,"5/coin",&H5000000,"6/coin",&H6000000,"7/coin",&H7000000,_							   "8/coin",&H8000000)		.AddFrame 260,  5, 90,"Coin Slot 1 (Key 4)",&H000001F,Array("3/2 coins",&H00,"1 coin",&H02,"1/2 coins",&H03,"2/coin",&H04,"2/2 coins",&H05,_							  "3/coin",&H06,"3/2 coins",&H07,"4/coin",&H08,"4/2 coins",&H09,"5/coin",&H0A,"5/2 coins",&H0B,_							  "6/coin",&H0C,"6/2 coins",&H0D,"7/coin",&H0E,"7/2 coins",&H0F)		.AddFrame 260,265, 90,"Coin Slot 3 (Key 5)",&H1F00,Array("3/2 coins",&H0000,"1 coin",&H0200,"1/2 coins",&H0300,"2/coin",&H0400,"2/2 coins",&H05000000,_							  "3/coin",&H0600,"3/2 coins",&H0700,"4/coin",&H0800,"4/2 coins",&H0900,"5/coin",&H0A00,"5/2 coins",&H0B00,_							  "6/coin",&H0C00,"6/2 coins",&H0D00,"7/coin",&H0E00,"7/2 coins",&H0F00)		.ViewDips	End WithEnd SubSet vpmShowDips = GetRef("editDips")
'**************************************************************************************************************
'*                                             For Ball Rolling                                               *
'**************************************************************************************************************
'****************************************
' B2B Collision by Steely & Pinball Ken
'****************************************
	
Dim tnopb, nosf, iball, cnt, coff, errMessage, B2BOn

B2BOn = 2 '0=Off, 1=On, 2=AutoDetect
CheckB2B
XYdata.interval = 10 ' <<<<< ADD timer named XYData to table
tnopb = 3 	' <<<<< SET to the "Total Number Of Possible Balls" in play at any one time
nosf = 10	' <<<<< SET to the "Number Of Sound Files" used / B2B collision volume levels
coff = False

ReDim CurrentBall(tnopb), BallStatus(tnopb)

For cnt = 0 to ubound(BallStatus) : BallStatus(cnt) = 0 : Next

'****************************************
' B2B AutoDisable for XP x64 Added by Koadic
'****************************************

Sub CheckB2B			' Added by Koadic for XP x64 handling
  Dim osver, cpuver, check
  On Error Resume Next
	For x = 0 to 1 : If B2BOn = x Then Exit Sub : End If : Next 'If B2BOn is set manually, then end routine
	Set check = CreateObject("WScript.Shell")
	osver = check.RegRead ("HKLM\Software\Microsoft\Windows NT\CurrentVersion\CurrentVersion")
	cpuver = check.RegRead ("HKLM\SYSTEM\ControlSet001\Control\Session Manager\Environment\Processor_Architecture")
	If osver < 6 and cpuver = "AMD64" Then B2BOn = 0 Else B2BOn = 1 'If OS is XP and 64bit, then disable B2B
   	If Err Then B2BOn = 1 'If there is an error in detecting either OS or x32/x64, then default to On
  On Error Goto 0
End Sub

' ===============================================================================================
'     destruk's new vpmCreateBall
'     use it: vpmCreateBall kicker
' adapted for core.vbs 3,38 & ballsize
' ===============================================================================================

Set vpmCreateBall = GetRef("MyCreateBallRoutine")
Function MyCreateBallRoutine(aKicker)
    For cnt = 1 to ubound(ballStatus)
        If ballStatus(cnt) = 0 Then
            If Not IsEmpty(vpmBallImage) Then
                Set CurrentBall(cnt) = aKicker.CreateSizedBall(BSize).Image
            Else
                Set CurrentBall(cnt) = aKicker.CreateSizedBall(BSize)
            End If
            Set MyCreateBallRoutine = aKicker
            currentball(cnt).uservalue = cnt
            ballStatus(cnt) = 1
            ballStatus(0) = ballStatus(0) + 1
            If Not coff Then
                If ballStatus(0) > 1 And Not XYdata.Enabled Then
                    XYdata.enabled = True
                End If
            End If
            Exit For
        End If
    Next
End Function

' Use CreateBallID(kickername) to manually create a ball with a BallID
' Can also be used on nonVPM tables (EM or Custom)

Sub CreateBallID(aKicker)
	For cnt = 1 to ubound(ballStatus)				' Loop through all possible ball IDs
		If ballStatus(cnt) = 0 Then					' If ball ID is available...
			Set CurrentBall(cnt) = aKicker.createball		' Set ball object with the first available ID
			CurrentBall(cnt).uservalue = cnt				' Assign the ball's uservalue to it's new ID
			ballStatus(cnt) = 1						' Mark this ball status active
			ballStatus(0) = ballStatus(0)+1 		' Increment ballStatus(0), the number of active balls
			If B2BOn > 0 Then						' If B2BOn is 0, it overrides auto-turn on collision detection
													' If more than one ball active, start collision detection process
				If ballStatus(0) > 1 and XYdata.enabled = False Then XYdata.enabled = True
			End If
			Exit For								' New ball ID assigned, exit loop
		End If
   	Next 
End Sub

'Call this sub from every kicker that destroys a ball, before the ball is destroyed.
	
Sub ClearBallID
  	On Error Resume Next							' Error handling for debugging purposes
   	iball = ActiveBall.uservalue					' Get the ball ID to be cleared
   	If Err Then Msgbox Err.description & vbCrLf & iball
    ballStatus(iBall) = 0 							' Clear the ball status
   	ballStatus(0) = ballStatus(0)-1 				' Subtract 1 ball from the # of balls in play
   	On Error Goto 0
End Sub

Sub ClearBallIDByNo(iBall)
  	On Error Resume Next							' Error handling for debugging purposes
   	ballStatus(iBall) = 0 							' Clear the ball status
   	ballStatus(0) = ballStatus(0)-1 				' Subtract 1 ball from the # of balls in play
   	On Error Goto 0
End Sub

'=====================================================
' <<<<<<<<<<<<<<<<< XYdata_Timer >>>>>>>>>>>>>>>>>
'=====================================================

'Ball data collection and B2B Collision detection.
	
ReDim baX(tnopb,4), baY(tnopb,4), baZ(tnopb,4), bVx(tnopb,4), bVy(tnopb,4), TotalVel(tnopb,4)
Dim cForce, bDistance, xyTime, cFactor, id, id2, id3, B1, B2

Sub XYdata_Timer()
    xyTime = Timer+(XYdata.interval*.001)						' xyTime is the system timer plus the current interval time
   	If id2 >= 4 Then id2 = 0									' Loop four times and start over
   	id2 = id2+1													' Increment the ball sampler ID
   	For id = 1 to ubound(ballStatus)							' Loop once for each possible ball
		If ballStatus(id) = 1 Then								' If ball is active...
			baX(id,id2) = round(CurrentBall(id).x,2)			' Sample x-coord
			baY(id,id2) = round(CurrentBall(id).y,2)			' Sample y-coord
			baZ(id,id2) = round(CurrentBall(id).z,2)			' Sample z-coord
			bVx(id,id2) = round(CurrentBall(id).velx,2)			' Sample x-velocity
			bVy(id,id2) = round(CurrentBall(id).vely,2)			' Sample y-velocity
			TotalVel(id,id2) = (bVx(id,id2)^2 + bVy(id,id2)^2) 	' Calculate total velocity
			If TotalVel(id,id2) > TotalVel(0,0) Then TotalVel(0,0) = int(TotalVel(id,id2))
		End If
   	Next
	id3 = id2 : B2 = 2 : B1 = 1									' Set up the counters for looping
	Do
		If ballStatus(B1) = 1 and ballStatus(B2) = 1 Then		' If both balls are active...
			bDistance = int((TotalVel(B1,id3)+TotalVel(B2,id3))^1.04)
			If ((baX(B1,id3) - baX(B2,id3))^2 + (baY(B1,id3) - baY(B2,id3))^2) < 2800 + bDistance Then
				If ABS(baZ(B1,id3) - baZ(B2,id3)) < 50 Then 
					Exit Sub
				End If
			End If
		End If
		B1 = B1+1												' Increment ball1
		If B1 = ubound(ballstatus) Then Exit Do					' Exit loop if all ball combinations checked	
		If B1 >= B2 then B1 = 1:B2 = B2+1						' If ball1 >= reset ball1 and increment ball2
	Loop
 	If ballStatus(0) <= 1 Then XYdata.enabled = False 			' Turn off timer if one ball or less
	If XYdata.interval >= 40 Then B2BOn = 0 : XYdata.enabled = False	' Auto-shut off
	If Timer > xyTime * 3 Then B2BOn = 0 : XYdata.enabled = False	' Auto-shut off
   	If Timer > xyTime Then XYdata.interval = XYdata.interval+1	' Increment interval if needed
End Sub

'=========================================================
' <<<<<<<<<<< Collide(ball id1, ball id2) >>>>>>>>>>>
'=========================================================
'Calculate the collision force and play sound accordingly.
	
Dim cTime, cb1,cb2, avgBallx, cAngle, bAngle1, bAngle2

Sub Collide(cb1,cb2) 	
 	If TotalVel(0,0) / 1.8 > cFactor Then cFactor = int(TotalVel(0,0) / 1.8)
  	avgBallx = (bvX(cb2,1) + bvX(cb2,2) + bvX(cb2,3) + bvX(cb2,4)) / 4
  	If avgBallx < bvX(cb2,id2) + .1 and avgBallx > bvX(cb2,id2) - .1 Then
		If ABS(TotalVel(cb1,id2) - TotalVel(cb2,id2)) < .000005 Then Exit Sub
 	End If
  	If Timer < cTime Then Exit Sub
  	cTime = Timer+.1								' Limits collisions to .1 seconds apart
 	GetAngle baX(cb1,id3) - baX(cb2,id3), baY(cb1,id3) - baY(cb2,id3), cAngle	' Collision angle via x/y-coordinates
	id3 = id3 - 1 : If id3 = 0 Then id3 = 4			' Step back one xyData sampling for a good velocity reading
 	GetAngle bVx(cb1,id3), bVy(cb1,id3), bAngle1	' ball 1 travel direction, via velocity
 	GetAngle bVx(cb2,id3), bVy(cb2,id3), bAngle2	' ball 2 travel direction, via velocity
	cForce = Cint((abs(TotalVel(cb1,id3)*Cos(cAngle-bAngle1))+abs(TotalVel(cb2,id3)*Cos(cAngle-bAngle2))))
    	If cForce < 4 Then Exit Sub					' Another collision limiter
   	cForce = Cint((cForce)/(cFactor/nosf))			' Divides up cForce for the proper sound selection.
  	If cForce > nosf-1 Then cForce = nosf-1			' First sound file 0(zero) minus one from number of sound files
   	PlaySound("collide" & cForce)					' Combines "collide" with the calculated sound level and play sound
End Sub

'=================================================
' <<<<<<<< GetAngle(X, Y, Anglename) >>>>>>>>
'=================================================
Dim Xin,Yin,rAngle,Radit,wAngle,Pi
Pi = Round(4*Atn(1),6)					'3.1415926535897932384626433832795

Sub GetAngle(Xin, Yin, wAngle)						
 	If Sgn(Xin) = 0 Then
 		If Sgn(Yin) = 1 Then rAngle = 3 * Pi/2 Else rAngle = Pi/2
 		If Sgn(Yin) = 0 Then rAngle = 0
 	Else
 		rAngle = atn(-Yin/Xin)
 	End If
 	If sgn(Xin) = -1 Then Radit = Pi Else Radit = 0
 	If sgn(Xin) = 1 and sgn(Yin) = 1 Then Radit = 2 * Pi
 	wAngle = round((Radit + rAngle),4)
End Sub
 
 '******************************************
' Use RollingSoundTimer to call div subs
'******************************************

Sub RollingSoundTimer_Timer()
	RollingSound	
End Sub

'****************************************
' JimmyFingers Enhanced Ball Rolling Script (Extension of Rascal's Original)
'****************************************

'*****Requires B2B script addition placed before this script addition*****

ReDim BRVeloY(tnopb), BRVeloX(tnopb), rolling(tnopb), rollingfast(tnopb)
Dim b
b = 0

RollingSoundTimer.Interval = 60/tnopb

Sub RollingSound()
	B = B + 1
	If B > tnopb Then B = 1
	If BallStatus(b) = 0 Then Exit Sub

	BRVeloY(b) = Cint(CurrentBall(b).VelY)
	BRVeloX(b) = Cint(CurrentBall(b).VelX)
	If((ABS(BRVeloY(b))> 3 AND (ABS(BRVeloY(b))< 10) or (ABS(BRVeloX(b) )> 3 AND (ABS(BRVeloX(b))< 10)))) Then
		If rolling(b) = True then
			Exit Sub
		Else
			If rollingfast(b) = True then
				StopSound "JF_rollingfaster"
				rollingfast(b) = False
			Else
				rolling(b) = True
				Select Case Int(Rnd*3)+1
					Case 1 : PlaySound "JF_roll1"
					Case 2 : PlaySound "JF_roll2"
					Case 3 : PlaySound "JF_roll3"
				End Select
			End If
		End If
	ElseIf (ABS(BRVeloY(b) )> 10 or ABS(BRVeloX(b) )> 10) Then
		If rollingfast(b) = True then
			Exit Sub
		Else
			If rolling(b) = True then
				StopSound "JF_roll1"
				StopSound "JF_roll2"
				StopSound "JF_roll3"
				rolling(b) = False
			Else
				rollingfast(b) = True
				PlaySound "JF_rollingfaster"
			End If
		End If
	Else
		If rolling(b) = True Then
			StopSound "JF_roll1"
			StopSound "JF_roll2"
			StopSound "JF_roll3"
			rolling(b) = False
		ElseIf rollingfast(b) = True Then
			StopSound "JF_rollingfaster"
			rollingfast(b) = False
		End If
	End If
End Sub

Sub StopRollingSound()
	StopSound "JF_roll1"
	StopSound "JF_roll2"
	StopSound "JF_roll3"
	StopSound "JF_rollingfaster"
End Sub
'**************************************************************************************************************
'*                                             End Ball Rolling                                               *
'**************************************************************************************************************




'*****************************************************
'*               DesktopMode Displays                *
'*****************************************************


Dim DisplayPatterns(11)
Dim DigStorage(32)
Dim LEDMod, ARMod

Sub CheckAR
	If Table1.BackdropImage = "backglass-4-3" Then
		LEDMod=22:ARMod=2
	Else
		LEDMod=0:ARMod=0
	End If
	For each obj in aSDO:obj.setvalue(LEDMod):Next
	For each obj in aSDO2:obj.setvalue(LEDMod):Next
	For each obj in aReels:obj.setvalue(ARMod):Next
End Sub

'Binary/Hex Pattern Recognition Array
DisplayPatterns(0) = 0		'0000000 Blank
DisplayPatterns(1) = 63		'0111111 zero
DisplayPatterns(2) = 6		'0000110 one
DisplayPatterns(3) = 91		'1011011 two
DisplayPatterns(4) = 79		'1001111 three
DisplayPatterns(5) = 102	'1100110 four
DisplayPatterns(6) = 109	'1101101 five
DisplayPatterns(7) = 125	'1111101 six
DisplayPatterns(8) = 7		'0000111 seven
DisplayPatterns(9) = 127	'1111111 eight
DisplayPatterns(10)= 111	'1101111 nine

Sub DisplayTimer_Timer ' 7-Digit output
	On Error Resume Next
	Dim ChgLED,ii,chg,stat,obj,TempCount,temptext,adj

	ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF) 'hex of binary (display 111111, or first 6 digits)
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(ChgLED)
			chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			For TempCount = 0 to 10
				If stat = DisplayPatterns(TempCount) then
					aSDO(chgLED(ii, 0)).SetValue(TempCount+LEDMod)
					DigStorage(chgLED(ii, 0)) = TempCount
				End If
				If stat = (DisplayPatterns(TempCount) + 128) then
					aSDO(chgLED(ii, 0)).SetValue(TempCount+11+LEDMod)
					DigStorage(chgLED(ii, 0)) = TempCount
				End If
			Next
		Next
	End IF
End Sub
Sub Table1_Exit():Controller.Stop:Controller.Games("playboy").Settings.Value("sound")=1:End Sub
















 