'*
'*
'*        Night of the Living Dead '68 (Original 2018)
'*        Table by: HiRez00 & Xenonph
'*
'*        Original Table - Out of Sight (Gottlieb 1974)
'*        Table build/scripted by Loserman76
'*        http://www.ipdb.org/machine.cgi?id=823
'*
'*

option explicit
Randomize
ExecuteGlobal GetTextFile("core.vbs")

On Error Resume Next
ExecuteGlobal GetTextFile("Controller.vbs")
If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the scripts folder."
On Error Goto 0

Const cGameName = "NOTLD68"

Const ShadowFlippersOn = true
Const ShadowBallOn = true

Const ShadowConfigFile = false

Wall45.timerinterval=1000:Wall45.timerenabled=1


Dim Controller	' B2S
Dim B2SScore	' B2S Score Displayed
Dim B2SOn		'True/False if want backglass

Const HSFileName="NOTLD68VPX.txt"
Const B2STableName="NOTLD68"
Const LMEMTableConfig="LMEMTables.txt"
Const LMEMShadowConfig="LMEMShadows.txt"
Dim EnableBallShadow
Dim EnableFlipperShadow


'* this value adjusts score motor behavior - 0 allows you to continue scoring while the score motor is running - 1 sets score motor to behave more like a real EM
Const ScoreMotorAdjustment=1

'* this is a debug setting to use an older scoring routine vs a newer score routine - don't change this value
Const ScoreAdditionAdjustment=1

'* this controls whether you hear bells (0) or chimes (1) when scoring
Const ChimesOn=1

dim ScoreChecker
dim CheckAllScores
dim sortscores(4)
dim sortplayers(4)
Dim TextStr,TextStr2
Dim i,xx,LStep,RStep
Dim obj
Dim bgpos
Dim kgpos
Dim dooralreadyopen
Dim kgdooralreadyopen
Dim TargetSpecialLit
Dim Points210counter
Dim Points500counter
Dim Points1000counter
Dim Points2000counter
Dim BallsPerGame
Dim InProgress
Dim BallInPlay
Dim CreditsPerCoin
Dim Score100K(4)
Dim Score(4)
Dim ScoreDisplay(4)
Dim HighScorePaid(4)
Dim HighScore
Dim HighScoreReward
Dim BonusMultiplier
Dim Credits
Dim Match
Dim Replay1
Dim Replay2
Dim Replay3
Dim Replay1Paid(4)
Dim Replay2Paid(4)
Dim Replay3Paid(4)
Dim TableTilted
Dim TiltCount

Dim OperatorMenu

Dim BonusBooster
Dim BonusBoosterCounter
Dim BonusCounter
Dim HoleCounter

Dim Ones
Dim Tens
Dim Hundreds
Dim Thousands

Dim Player
Dim Players

Dim rst
Dim bonuscountdown
Dim TempMultiCounter
dim TempPlayerup
dim RotatorTemp

Dim bump1
Dim bump2
Dim bump3

Dim LastChime10
Dim LastChime100
Dim LastChime1000

Dim Score10
Dim Score100

Dim targettempscore

Dim LeftTargetCounter
Dim RightTargetCounter

Dim MotorRunning
Dim Replay1Table(15)
Dim Replay2Table(15)
Dim Replay3Table(15)
Dim ReplayTableSet
Dim ReplayLevel
Dim ReplayTableMax
Dim BonusSpecialThreshold

'**************************
'Fluppers Flashers

Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5
Dim FlashLevel6, FlashLevel7, FlashLevel8, FlashLevel9, FlashLevel10
Dim FlashLevel11, FlashLevel12, FlashLevel13, FlashLevel14, FlashLevel15
Dim FlashLevel16, FlashLevel17, FlashLevel18, FlashLevel19, FlashLevel20
FlasherLight1.IntensityScale = 0
Flasherlight2.IntensityScale = 0
Flasherlight3.IntensityScale = 0
Flasherlight4.IntensityScale = 0
FlasherLight5.IntensityScale = 0
Flasherlight6.IntensityScale = 0
Flasherlight7.IntensityScale = 0
Flasherlight8.IntensityScale = 0
FlasherLight9.IntensityScale = 0
Flasherlight10.IntensityScale = 0
Flasherlight11.IntensityScale = 0
Flasherlight12.IntensityScale = 0
Flasherlight13.IntensityScale = 0
Flasherlight14.IntensityScale = 0
Flasherlight15.IntensityScale = 0



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

'*** white flasher ***
Sub FlasherFlash2_Timer()
	dim flashx3, matdim
	If not FlasherFlash2.TimerEnabled Then 
		FlasherFlash2.TimerEnabled = True
		FlasherFlash2.visible = 1
		FlasherLit2.visible = 1
	End If
	flashx3 = FlashLevel2 * FlashLevel2 * FlashLevel2
	Flasherflash2.opacity = 1000 * flashx3
	FlasherLit2.BlendDisableLighting = 10 * flashx3
	Flasherbase2.BlendDisableLighting =  flashx3
	FlasherLight2.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel2)
	FlasherLit2.material = "domelit" & matdim
	FlashLevel2 = FlashLevel2 * 0.85 - 0.01
	If FlashLevel2 < 0.15 Then
		FlasherLit2.visible = 0
	Else
		FlasherLit2.visible = 1
	end If
	If FlashLevel2 < 0 Then
		FlasherFlash2.TimerEnabled = False
		FlasherFlash2.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash3_Timer()
	dim flashx3, matdim
	If not FlasherFlash3.TimerEnabled Then 
		FlasherFlash3.TimerEnabled = True
		FlasherFlash3.visible = 1
		FlasherLit3.visible = 1
	End If
	flashx3 = FlashLevel3 * FlashLevel3 * FlashLevel3
	Flasherflash3.opacity = 1000 * flashx3
	FlasherLit3.BlendDisableLighting = 10 * flashx3
	Flasherbase3.BlendDisableLighting =  flashx3
	FlasherLight3.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	FlasherLit3.material = "domelit" & matdim
	FlashLevel3 = FlashLevel3 * 0.85 - 0.01
	If FlashLevel3 < 0.15 Then
		FlasherLit3.visible = 0
	Else
		FlasherLit3.visible = 1
	end If
	If FlashLevel3 < 0 Then
		FlasherFlash3.TimerEnabled = False
		FlasherFlash3.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash4_Timer()
	dim flashx3, matdim
	If not FlasherFlash4.TimerEnabled Then 
		FlasherFlash4.TimerEnabled = True
		FlasherFlash4.visible = 1
		FlasherLit4.visible = 1
	End If
	flashx3 = FlashLevel4 * FlashLevel4 * FlashLevel4
	Flasherflash4.opacity = 1000 * flashx3
	FlasherLit4.BlendDisableLighting = 10 * flashx3
	Flasherbase4.BlendDisableLighting =  flashx3
	FlasherLight4.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel4)
	FlasherLit4.material = "domelit" & matdim
	FlashLevel4 = FlashLevel4 * 0.85 - 0.01
	If FlashLevel4 < 0.15 Then
		FlasherLit4.visible = 0
	Else
		FlasherLit4.visible = 1
	end If
	If FlashLevel4 < 0 Then
		FlasherFlash4.TimerEnabled = False
		FlasherFlash4.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash5_Timer()
	dim flashx3, matdim
	If not FlasherFlash5.TimerEnabled Then 
		FlasherFlash5.TimerEnabled = True
		FlasherFlash5.visible = 1
		FlasherLit5.visible = 1
	End If
	flashx3 = FlashLevel5 * FlashLevel5 * FlashLevel5
	Flasherflash5.opacity = 1000 * flashx3
	FlasherLit5.BlendDisableLighting = 10 * flashx3
	Flasherbase5.BlendDisableLighting =  flashx3
	FlasherLight5.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel5)
	FlasherLit5.material = "domelit" & matdim
	FlashLevel5 = FlashLevel5 * 0.85 - 0.01
	If FlashLevel5 < 0.15 Then
		FlasherLit5.visible = 0
	Else
		FlasherLit5.visible = 1
	end If
	If FlashLevel5 < 0 Then
		FlasherFlash5.TimerEnabled = False
		FlasherFlash5.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash6_Timer()
	dim flashx3, matdim
	If not FlasherFlash6.TimerEnabled Then 
		FlasherFlash6.TimerEnabled = True
		FlasherFlash6.visible = 1
		FlasherLit6.visible = 1
	End If
	flashx3 = FlashLevel6 * FlashLevel6 * FlashLevel6
	Flasherflash6.opacity = 1000 * flashx3
	FlasherLit6.BlendDisableLighting = 10 * flashx3
	Flasherbase6.BlendDisableLighting =  flashx3
	FlasherLight6.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel6)
	FlasherLit6.material = "domelit" & matdim
	FlashLevel6 = FlashLevel6 * 0.85 - 0.01
	If FlashLevel6 < 0.15 Then
		FlasherLit6.visible = 0
	Else
		FlasherLit6.visible = 1
	end If
	If FlashLevel6 < 0 Then
		FlasherFlash6.TimerEnabled = False
		FlasherFlash6.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash7_Timer()
	dim flashx3, matdim
	If not FlasherFlash7.TimerEnabled Then 
		FlasherFlash7.TimerEnabled = True
		FlasherFlash7.visible = 1
		FlasherLit7.visible = 1
	End If
	flashx3 = FlashLevel7 * FlashLevel7 * FlashLevel7
	Flasherflash7.opacity = 1000 * flashx3
	FlasherLit7.BlendDisableLighting = 10 * flashx3
	Flasherbase7.BlendDisableLighting =  flashx3
	FlasherLight7.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel7)
	FlasherLit7.material = "domelit" & matdim
	FlashLevel7 = FlashLevel7 * 0.85 - 0.01
	If FlashLevel7 < 0.15 Then
		FlasherLit7.visible = 0
	Else
		FlasherLit7.visible = 1
	end If
	If FlashLevel7 < 0 Then
		FlasherFlash7.TimerEnabled = False
		FlasherFlash7.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash8_Timer()
	dim flashx3, matdim
	If not FlasherFlash8.TimerEnabled Then 
		FlasherFlash8.TimerEnabled = True
		FlasherFlash8.visible = 1
		FlasherLit8.visible = 1
	End If
	flashx3 = FlashLevel8 * FlashLevel8 * FlashLevel8
	Flasherflash8.opacity = 1000 * flashx3
	FlasherLit8.BlendDisableLighting = 10 * flashx3
	Flasherbase8.BlendDisableLighting =  flashx3
	FlasherLight8.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel8)
	FlasherLit8.material = "domelit" & matdim
	FlashLevel8 = FlashLevel8 * 0.85 - 0.01
	If FlashLevel8 < 0.15 Then
		FlasherLit8.visible = 0
	Else
		FlasherLit8.visible = 1
	end If
	If FlashLevel8 < 0 Then
		FlasherFlash8.TimerEnabled = False
		FlasherFlash8.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash9_Timer()
	dim flashx3, matdim
	If not FlasherFlash9.TimerEnabled Then 
		FlasherFlash9.TimerEnabled = True
		FlasherFlash9.visible = 1
		FlasherLit9.visible = 1
	End If
	flashx3 = FlashLevel9 * FlashLevel9 * FlashLevel9
	Flasherflash9.opacity = 1000 * flashx3
	FlasherLit9.BlendDisableLighting = 10 * flashx3
	Flasherbase9.BlendDisableLighting =  flashx3
	FlasherLight9.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel9)
	FlasherLit9.material = "domelit" & matdim
	FlashLevel9 = FlashLevel9 * 0.85 - 0.01
	If FlashLevel9 < 0.15 Then
		FlasherLit9.visible = 0
	Else
		FlasherLit9.visible = 1
	end If
	If FlashLevel9 < 0 Then
		FlasherFlash9.TimerEnabled = False
		FlasherFlash9.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash10_Timer()
	dim flashx3, matdim
	If not FlasherFlash10.TimerEnabled Then 
		FlasherFlash10.TimerEnabled = True
		FlasherFlash10.visible = 1
		FlasherLit10.visible = 1
	End If
	flashx3 = FlashLevel10 * FlashLevel10 * FlashLevel10
	Flasherflash10.opacity = 1000 * flashx3
	FlasherLit10.BlendDisableLighting = 10 * flashx3
	Flasherbase10.BlendDisableLighting =  flashx3
	FlasherLight10.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel10)
	FlasherLit10.material = "domelit" & matdim
	FlashLevel10 = FlashLevel10 * 0.85 - 0.01
	If FlashLevel10 < 0.15 Then
		FlasherLit10.visible = 0
	Else
		FlasherLit10.visible = 1
	end If
	If FlashLevel10 < 0 Then
		FlasherFlash10.TimerEnabled = False
		FlasherFlash10.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash11_Timer()
	dim flashx3, matdim
	If not FlasherFlash11.TimerEnabled Then 
		FlasherFlash11.TimerEnabled = True
		FlasherFlash11.visible = 1
		FlasherLit11.visible = 1
	End If
	flashx3 = FlashLevel11 * FlashLevel11 * FlashLevel11
	Flasherflash11.opacity = 1000 * flashx3
	FlasherLit11.BlendDisableLighting = 10 * flashx3
	Flasherbase11.BlendDisableLighting =  flashx3
	FlasherLight11.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel11)
	FlasherLit11.material = "domelit" & matdim
	FlashLevel11 = FlashLevel11 * 0.85 - 0.01
	If FlashLevel11 < 0.15 Then
		FlasherLit11.visible = 0
	Else
		FlasherLit11.visible = 1
	end If
	If FlashLevel11 < 0 Then
		FlasherFlash11.TimerEnabled = False
		FlasherFlash11.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash12_Timer()
	dim flashx3, matdim
	If not FlasherFlash12.TimerEnabled Then 
		FlasherFlash12.TimerEnabled = True
		FlasherFlash12.visible = 1
		FlasherLit12.visible = 1
	End If
	flashx3 = FlashLevel12 * FlashLevel12 * FlashLevel12
	Flasherflash12.opacity = 1000 * flashx3
	FlasherLit12.BlendDisableLighting = 10 * flashx3
	Flasherbase12.BlendDisableLighting =  flashx3
	FlasherLight12.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel12)
	FlasherLit12.material = "domelit" & matdim
	FlashLevel12 = FlashLevel12 * 0.85 - 0.01
	If FlashLevel12 < 0.15 Then
		FlasherLit12.visible = 0
	Else
		FlasherLit12.visible = 1
	end If
	If FlashLevel12 < 0 Then
		FlasherFlash12.TimerEnabled = False
		FlasherFlash12.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash13_Timer()
	dim flashx3, matdim
	If not FlasherFlash13.TimerEnabled Then 
		FlasherFlash13.TimerEnabled = True
		FlasherFlash13.visible = 1
		FlasherLit13.visible = 1
	End If
	flashx3 = FlashLevel13 * FlashLevel13 * FlashLevel13
	Flasherflash13.opacity = 1000 * flashx3
	FlasherLit13.BlendDisableLighting = 10 * flashx3
	Flasherbase13.BlendDisableLighting =  flashx3
	FlasherLight13.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel13)
	FlasherLit13.material = "domelit" & matdim
	FlashLevel13 = FlashLevel13 * 0.85 - 0.01
	If FlashLevel13 < 0.15 Then
		FlasherLit13.visible = 0
	Else
		FlasherLit13.visible = 1
	end If
	If FlashLevel13 < 0 Then
		FlasherFlash13.TimerEnabled = False
		FlasherFlash13.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash14_Timer()
	dim flashx3, matdim
	If not FlasherFlash14.TimerEnabled Then 
		FlasherFlash14.TimerEnabled = True
		FlasherFlash14.visible = 1
		FlasherLit14.visible = 1
	End If
	flashx3 = FlashLevel14 * FlashLevel14 * FlashLevel14
	Flasherflash14.opacity = 1000 * flashx3
	FlasherLit14.BlendDisableLighting = 10 * flashx3
	Flasherbase14.BlendDisableLighting =  flashx3
	FlasherLight14.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel14)
	FlasherLit14.material = "domelit" & matdim
	FlashLevel14 = FlashLevel14 * 0.85 - 0.01
	If FlashLevel14 < 0.15 Then
		FlasherLit14.visible = 0
	Else
		FlasherLit14.visible = 1
	end If
	If FlashLevel14 < 0 Then
		FlasherFlash14.TimerEnabled = False
		FlasherFlash14.visible = 0
	End If
End Sub

'*** white flasher ***
Sub FlasherFlash15_Timer()
	dim flashx3, matdim
	If not FlasherFlash15.TimerEnabled Then 
		FlasherFlash15.TimerEnabled = True
		FlasherFlash15.visible = 1
		FlasherLit15.visible = 1
	End If
	flashx3 = FlashLevel15 * FlashLevel15 * FlashLevel15
	Flasherflash15.opacity = 1000 * flashx3
	FlasherLit15.BlendDisableLighting = 10 * flashx3
	Flasherbase15.BlendDisableLighting =  flashx3
	FlasherLight15.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel15)
	FlasherLit15.material = "domelit" & matdim
	FlashLevel15 = FlashLevel15 * 0.85 - 0.01
	If FlashLevel15 < 0.15 Then
		FlasherLit15.visible = 0
	Else
		FlasherLit15.visible = 1
	end If
	If FlashLevel15 < 0 Then
		FlasherFlash15.TimerEnabled = False
		FlasherFlash15.visible = 0
	End If
End Sub




'**********************************************

Dim AA
Dim BB
Dim CC
BB=1
CC=0

Sub StopSounds()

   StopSound"0nc06"
   StopSound"0nc07"
   StopSound"0nc08"
   StopSound"0nr01"
   StopSound"0nr02"
   StopSound"0nr03"
   StopSound"0nr04"
   StopSound"0nr05"
   StopSound"0nr06"
   StopSound"0nr07"
   StopSound"0nr08"
   StopSound"0nr09"
   StopSound"0nr10"
   StopSound"0nr11"
   StopSound"0nr12"
   StopSound"0nr13"
   StopSound"0nr14"
   StopSound"0nr15"
   StopSound"0nr16"
   StopSound"0nr17"
   StopSound"0nr18"
   StopSound"0nr19"
   StopSound"0nr20"
   StopSound"0nr21"
   StopSound"0nr22"
   StopSound"0nr23"
   StopSound"0nr24"
   StopSound"0nr25"

End Sub

Sub StopUpSounds()

   StopSound"0nt01"
   StopSound"0nt02"
   StopSound"0nt03"
   StopSound"0nt04"
   StopSound"0nt05"
   StopSound"0nt06"
   StopSound"0nt07"
   StopSound"0nt08"
   StopSound"0nt09"
   StopSound"0nt10"
   StopSound"0nt11"
   StopSound"0nt12"
   StopSound"0nt13"
   StopSound"0nt14"

End Sub




Sub Table1_Init()
	If Table1.ShowDT = false then
		For each obj in DesktopCrap
			obj.visible=False
		next
	End If

	OperatorMenuBackdrop.image = "PostitBL"
	For XOpt = 1 to MaxOption
		Eval("OperatorOption"&XOpt).image = "PostitBL"
	next
		
	For XOpt = 1 to 256
		Eval("Option"&XOpt).image = "PostItBL"
	next
	
	LoadEM
	LoadLMEMConfig2
	HideOptions
	SetupReplayTables
	PlasticsOff
	BumpersOff
	OperatorMenu=0
	HighScore=0
	MotorRunning=0
	HighScoreReward=3
	Credits=0
	BallsPerGame=5
	ReplayLevel=1
	BonusSpecialThreshold=1
	loadhs
	if HighScore=0 then HighScore=50000


	TableTilted=false
	
	Match=int(Rnd*10)*10
	MatchReel.SetValue((Match/10)+1)

	CanPlayReel.SetValue(0)
	BallInPlayReel.SetValue(0)

	For each obj in PlayerHuds
		obj.SetValue(0)
	next

	For each obj in PlayerHUDScores
		obj.state=0
	next

	For each obj in PlayerScoresOn
		obj.ResetToZero
	next

	For each obj in PlayerScores
		obj.ResetToZero
	next

	for each obj in bottgate
		obj.isdropped=true
    next
	for each obj in kickgate
		obj.isdropped=true
    next
	for each obj in Bonus
		obj.state=0
	next
	for each obj in StarLights
		obj.state=1
	next

	Replay1=Replay1Table(ReplayLevel)
	Replay2=Replay2Table(ReplayLevel)
	Replay3=Replay3Table(ReplayLevel)

	BonusCounter=0
	HoleCounter=0
    bgpos=6
	kgpos=0
    bottgate(bgpos).isdropped=false
	kickgate(kgpos).isdropped=false
	primgate.RotY=90
	primgate1.RotY=30
 	dooralreadyopen=0
	kgdooralreadyopen=0

	InstructCard.image="IC_"+FormatNumber(BallsPerGame,0)
	RefreshReplayCard
	GameOverReel.SetValue(1)
	TiltReel.SetValue(1)
	
	Bumper1Light.state=0
	Bumper2Light.state=0
	Bumper3Light.state=0
	BumpersOff

	TargetSpecialLit = 0
	Points210counter=0
	Points500counter=0
	Points1000counter=0
	Points2000counter=0
	
	BonusBooster=3
	BonusBoosterCounter=0
	Players=0
	RotatorTemp=1
	InProgress=false


	ScoreText.text=HighScore


	If B2SOn Then
		If Match = 0 then
			Controller.B2SSetMatch 100
		else
			Controller.B2SSetMatch Match
		end if

		Controller.B2SSetScoreRolloverPlayer1 0
		Controller.B2SSetScoreRolloverPlayer2 0
		Controller.B2SSetScoreRolloverPlayer3 0
		Controller.B2SSetScoreRolloverPlayer4 0

		'Controller.B2SSetScore 6,HighScore
		Controller.B2SSetTilt 0
		Controller.B2SSetCredits Credits
		Controller.B2SSetGameOver 1
	End If

	for i=1 to 4
    player=i
		If B2SOn Then 
			Controller.B2SSetScorePlayer player, 0
		End If
	next
	bump1=1
	bump2=1
	bump3=1
	InitPauser5.enabled=true
	If Credits > 0 Then DOF 114, 1
    LoadLUT
End Sub

'*************
'   JP'S LUT
'*************

Dim bLutActive, LUTImage
Dim x
Sub LoadLUT
	bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 9: UpdateLUT: SaveLUT: End Sub

Sub UpdateLUT
Select Case LutImage
Case 0: table1.ColorGradeImage = "LUT0"
Case 1: table1.ColorGradeImage = "LUT1"
Case 2: table1.ColorGradeImage = "LUT2"
Case 3: table1.ColorGradeImage = "LUT3"
Case 4: table1.ColorGradeImage = "LUT4"
Case 5: table1.ColorGradeImage = "LUT5"
Case 6: table1.ColorGradeImage = "LUT6"
Case 7: table1.ColorGradeImage = "LUT7"
Case 8: table1.ColorGradeImage = "LUT8"
End Select
End Sub


Sub Table1_exit()
	savehs
	SaveLMEMConfig
	SaveLMEMConfig2
	If B2SOn Then Controller.Stop
end sub


Sub Table1_KeyDown(ByVal keycode)

	' GNMOD
	if EnteringInitials then
		CollectInitials(keycode)
		exit sub
	end if


	if EnteringOptions then
		CollectOptions(keycode)
		exit sub
	end if



	If keycode = PlungerKey Then
		Plunger.PullBack
		PlungerPulled = 1
	End If

	if keycode = LeftFlipperKey and InProgress = false then
		OperatorMenuTimer.Enabled = true
	end if
	' END GNMOD


	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false Then
		LeftFlipper.RotateToEnd
		PlaySound "buzzL",-1
		PlaySound "FlipperUp"
		DOF 101, 1
	End If
    
	If keycode = RightFlipperKey  and InProgress=true and TableTilted=false Then
		RightFlipper.RotateToEnd

		PlaySound "FlipperUp"
		PlaySound "buzz",-1
		DOF 102, 1
	End If

	If keycode = LeftMagnaSave Then bLutActive = True
	If keycode = RightMagnaSave Then 
		If bLutActive Then NextLUT: End If
	End If
    
	If keycode = LeftTiltKey Then
		Nudge 90, 2
		TiltIt
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
		TiltIt
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
		TiltIt
	End If

	If keycode = MechanicalTilt Then
		TiltCount=2
		TiltIt
	End If

	If keycode = AddCreditKey or keycode = 4 then
		If B2SOn Then
			'Controller.B2SSetScorePlayer6 HighScore
			
		End If

		playsound "coinin"
		AddSpecial2
	end if

   if keycode = 5 then 
		playsound "coinin"
		AddSpecial2
		keycode= StartGameKey
	end if


   if keycode = StartGameKey and Credits>0 and InProgress=true and Players>0 and Players<2 and BallInPlay<2 then
		Credits=Credits-1
		If Credits = 0 Then DOF 114, 0
		CreditsReel.SetValue(Credits)
		Players=Players+1
		CanPlayReel.SetValue(Players)
		playsound "click"
		If B2SOn Then
			Controller.B2SSetCanPlay Players
			If Players=2 Then
				Controller.B2SSetScoreRolloverPlayer2 0
			End If
			If Players=3 Then
				Controller.B2SSetScoreRolloverPlayer3 0
			End If
			If Players=4 Then
				Controller.B2SSetScoreRolloverPlayer4 0
			End If
			Controller.B2SSetCredits Credits
		End If
    end if

	if keycode=StartGameKey and Credits>0 and InProgress=false and Players=0 AND EnteringOptions=0 then
'GNMOD
		OperatorMenuTimer.Enabled = false
'END GNMOD
		Credits=Credits-1
		If Credits = 0 Then DOF 114, 0
		CreditsReel.SetValue(Credits)
		Players=1
		CanPlayReel.SetValue(Players)
		MatchReel.SetValue(0)
		Player=1
		playsound "StartUpSequence":TargetRight1.timerenabled=0:TriggerTop1.timerenabled=0:Bumper3.timerenabled=0:Wall49.timerenabled=0:Wall45.timerenabled=0:Wall46.timerenabled=0:BB=1:EndMusic:StopSounds:PlaySound"0nc06":Bumper3.timerinterval=9500:Bumper3.timerenabled=1:Trigger2.timerenabled=0:TargetLeft1.timerenabled=0
		TempPlayerUp=Player
		PlayerUpRotator.enabled=true
		rst=0
		BallInPlay=1
		InProgress=true
		resettimer.enabled=true
		BonusMultiplier=1
		GameOverReel.SetValue(0)
		If B2SOn Then
			Controller.B2SSetTilt 0
			Controller.B2SSetGameOver 0
			Controller.B2SSetMatch 0
			Controller.B2SSetCredits Credits
			'Controller.B2SSetScore 6,HighScore
			Controller.B2SSetCanPlay 1
			Controller.B2SSetData 81,0
			Controller.B2SSetData 82,0
			Controller.B2SSetData 83,0
			Controller.B2SSetData 84,0
			Controller.B2SSetPlayerUp 1
			Controller.B2SSetData 81,1
			Controller.B2SSetBallInPlay BallInPlay
			Controller.B2SSetScoreRolloverPlayer1 0
			Controller.B2SSetScoreRolloverPlayer2 0
		End If
		If Table1.ShowDT = True then
			For each obj in PlayerScores
				obj.ResetToZero
				obj.Visible=true
			next
			For each obj in PlayerScoresOn
				obj.ResetToZero
				obj.Visible=false
			next

			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			PlayerHuds(Player-1).SetValue(1)
			PlayerHUDScores(Player-1).state=1
			PlayerScores(Player-1).Visible=0
			PlayerScoresOn(Player-1).Visible=1
		end If

	end if


    
End Sub

Sub Table1_KeyUp(ByVal keycode)

	' GNMOD
	if EnteringInitials then
		exit sub
	end if

	If keycode = PlungerKey Then
		
		if PlungerPulled = 0 then
			exit sub
		end if
		
		PlaySound"plungerrelease"
		Plunger.Fire
	End If

	if keycode = LeftFlipperKey then
		OperatorMenuTimer.Enabled = false
	end if

	' END GNMOD    
    
	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false Then
		LeftFlipper.RotateToStart
		PlaySound "FlipperDown"
		StopSound "buzzL"
		DOF 101, 0
	End If
    
	If keycode = RightFlipperKey and InProgress=true and TableTilted=false Then
		RightFlipper.RotateToStart
        StopSound "buzz"
		PlaySound "FlipperDown"
		DOF 102, 0
	End If

    If keycode = LeftMagnaSave Then bLutActive = False

End Sub


Sub Drain_Hit()
	Drain.DestroyBall
	PlaySound "fx_drain":EndMusic:StopUpSounds:StopSounds
	DOF 105, 2
	Pause4Bonustimer.enabled=1:Wall47.timerenabled=0:Bumper1.timerenabled=0:BB=0:AA=1
			Dim o
			o = INT(25 * RND(1) )
			Select Case o
			Case 0:PlaySound"0nr01":Bumper2.timerinterval=8800:Bumper2.timerenabled=1
			Case 1:PlaySound"0nr02":Bumper2.timerinterval=8000:Bumper2.timerenabled=1
			Case 2:PlaySound"0nr03":Bumper2.timerinterval=9000:Bumper2.timerenabled=1
			Case 3:PlaySound"0nr04":Bumper2.timerinterval=8900:Bumper2.timerenabled=1
			Case 4:PlaySound"0nr05":Bumper2.timerinterval=10400:Bumper2.timerenabled=1
			Case 5:PlaySound"0nr06":Bumper2.timerinterval=8000:Bumper2.timerenabled=1
			Case 6:PlaySound"0nr07":Bumper2.timerinterval=9900:Bumper2.timerenabled=1
			Case 7:PlaySound"0nr08":Bumper2.timerinterval=6900:Bumper2.timerenabled=1
			Case 8:PlaySound"0nr09":Bumper2.timerinterval=4400:Bumper2.timerenabled=1
			Case 9:PlaySound"0nr10":Bumper2.timerinterval=5700:Bumper2.timerenabled=1
			Case 10:PlaySound"0nr11":Bumper2.timerinterval=8300:Bumper2.timerenabled=1
			Case 11:PlaySound"0nr12":Bumper2.timerinterval=7900:Bumper2.timerenabled=1
			Case 12:PlaySound"0nr13":Bumper2.timerinterval=8200:Bumper2.timerenabled=1
			Case 13:PlaySound"0nr14":Bumper2.timerinterval=7200:Bumper2.timerenabled=1
			Case 14:PlaySound"0nr15":Bumper2.timerinterval=12600:Bumper2.timerenabled=1
			Case 15:PlaySound"0nr16":Bumper2.timerinterval=10900:Bumper2.timerenabled=1
			Case 16:PlaySound"0nr17":Bumper2.timerinterval=5800:Bumper2.timerenabled=1
			Case 17:PlaySound"0nr18":Bumper2.timerinterval=7800:Bumper2.timerenabled=1
			Case 18:PlaySound"0nr19":Bumper2.timerinterval=8700:Bumper2.timerenabled=1
			Case 19:PlaySound"0nr20":Bumper2.timerinterval=12200:Bumper2.timerenabled=1
			Case 20:PlaySound"0nr21":Bumper2.timerinterval=10100:Bumper2.timerenabled=1
			Case 21:PlaySound"0nr22":Bumper2.timerinterval=10800:Bumper2.timerenabled=1
			Case 22:PlaySound"0nr23":Bumper2.timerinterval=7400:Bumper2.timerenabled=1
			Case 23:PlaySound"0nr24":Bumper2.timerinterval=9400:Bumper2.timerenabled=1
			Case 24:PlaySound"0nr25":Bumper2.timerinterval=9900:Bumper2.timerenabled=1
			End Select		
End Sub

Sub Trigger1_Unhit()	
	DOF 128, 2
	DOF 129, 2
End Sub

Sub Pause4Bonustimer_timer
	Pause4Bonustimer.enabled=0
	AddBonus
	
End Sub

'***********************
'     Flipper Logos
'***********************

Sub UpdateFlipperLogos_Timer
	LFlip.RotY = LeftFlipper.CurrentAngle
	RFlip.RotY = RightFlipper.CurrentAngle
	PGate.Rotz = (Gate.CurrentAngle*.75) + 25
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
End Sub


'***********************
' slingshots
'

Sub RightSlingShot_Slingshot
    PlaySound "right_slingshot", 0, 1, 0.05, 0.05
	DOF 104, 2
    RSling0.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	AddScore 10
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSling0.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound "left_slingshot",0,1,-0.05,0.05
	DOF 103, 2
    LSling0.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	AddScore 10
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing0.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


'***********************************


Sub Trigger2_Unhit()	
			Dim t
			t = INT(8 * RND(1) )
			Select Case t
			Case 0:PlaySound("0nthunder01"):Lightning26.enabled=true
			Case 1:PlaySound("0nthunder02"):Lightning26.enabled=true
			Case 2:PlaySound("0nthunder03"):Lightning26.enabled=true
			Case 3:PlaySound("0nthunder04"):Lightning26.enabled=true
			Case 4:PlaySound("0nthunder05"):Lightning26.enabled=true
			Case 5:PlaySound("0nthunder06"):Lightning26.enabled=true
			Case 6:PlaySound("0nthunder07"):Lightning26.enabled=true
			Case 7:PlaySound("0nthunder08"):Lightning26.enabled=true
			End Select
End Sub

Sub Trigger2_Timer
			Dim t
			t = INT(8 * RND(1) )
			Select Case t
			Case 0:PlaySound("0nthunder01"):Lightning26.enabled=true
			Case 1:PlaySound("0nthunder02"):Lightning26.enabled=true
			Case 2:PlaySound("0nthunder03"):Lightning26.enabled=true
			Case 3:PlaySound("0nthunder04"):Lightning26.enabled=true
			Case 4:PlaySound("0nthunder05"):Lightning26.enabled=true
			Case 5:PlaySound("0nthunder06"):Lightning26.enabled=true
			Case 6:PlaySound("0nthunder07"):Lightning26.enabled=true
			Case 7:PlaySound("0nthunder08"):Lightning26.enabled=true
			End Select
Trigger2.timerenabled=0
End Sub

'**********************************
'Lightning Center

Sub Lightning1_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:FlashLevel1 = 1 : FlasherFlash1_Timer:FlashLevel2 = 1 : FlasherFlash2_Timer
    Case 1:FlashLevel3 = 1 : FlasherFlash3_Timer:FlashLevel4 = 1 : FlasherFlash4_Timer:FlashLevel5 = 1 : FlasherFlash5_Timer
    End Select
end sub 


Sub Lightning1off_Timer
    Lightning1.enabled=false
    Lightning1off.enabled=false
    Lightning2.enabled=true:Lightning2.interval=100:Lightning2off.enabled=true:Lightning2off.interval=400
end sub


Sub Lightning2_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:FlashLevel5 = 1 : FlasherFlash5_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 1:FlashLevel7 = 1 : FlasherFlash7_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 2:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 3:FlashLevel7 = 1 : FlasherFlash7_Timer
    End Select
end sub 

Sub Lightning2off_Timer
    Lightning2.enabled=false
    Lightning2off.enabled=false
    Lightning3.enabled=true:Lightning3.interval=100:Lightning3off.enabled=true:Lightning3off.interval=300
end sub

Sub Lightning3_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:FlashLevel8 = 1 : FlasherFlash8_Timer:FlashLevel10 = 1 : FlasherFlash10_Timer
    Case 1:FlashLevel11 = 1 : FlasherFlash11_Timer:FlashLevel9 = 1 : FlasherFlash9_Timer
    Case 2:FlashLevel9 = 1 : FlasherFlash9_Timer
    Case 3:FlashLevel11 = 1 : FlasherFlash11_Timer
    End Select
end sub 

Sub Lightning3off_Timer
    Lightning3.enabled=false
    Lightning3off.enabled=false
    Lightning4.enabled=true:Lightning4.interval=100:Lightning4off.enabled=true:Lightning4off.interval=200
end sub


Sub Lightning4_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:FlashLevel13 = 1 : FlasherFlash13_Timer:FlashLevel12 = 1 : FlasherFlash12_Timer
    Case 1:FlashLevel12 = 1 : FlasherFlash12_Timer
    Case 2:FlashLevel13 = 1 : FlasherFlash13_Timer
    End Select
end sub 

Sub Lightning4off_Timer
    Lightning4.enabled=false
    Lightning4off.enabled=false
    Lightning5.enabled=true:Lightning5.interval=100:Lightning5off.enabled=true:Lightning5off.interval=200
end sub

Sub Lightning5_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:FlashLevel14 = 1 : FlasherFlash14_Timer
    Case 1:FlashLevel15 = 1 : FlasherFlash15_Timer:FlashLevel14 = 1 : FlasherFlash14_Timer
    Case 2:FlashLevel15 = 1 : FlasherFlash15_Timer
    End Select
end sub 

Sub Lightning5off_Timer
    Lightning5.enabled=false
    Lightning5off.enabled=false
end sub

'**********************************
'Lightning Left

Sub Lightning6_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:FlashLevel1 = 1 : FlasherFlash1_Timer:FlashLevel2 = 1 : FlasherFlash2_Timer
    Case 1:FlashLevel3 = 1 : FlasherFlash3_Timer:FlashLevel4 = 1 : FlasherFlash4_Timer:FlashLevel5 = 1 : FlasherFlash5_Timer
    End Select
end sub 


Sub Lightning6off_Timer
    Lightning6.enabled=false
    Lightning6off.enabled=false
    Lightning7.enabled=true:Lightning7.interval=100:Lightning7off.enabled=true:Lightning7off.interval=400
end sub


Sub Lightning7_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:FlashLevel5 = 1 : FlasherFlash5_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 1:FlashLevel7 = 1 : FlasherFlash7_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 2:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 3:FlashLevel7 = 1 : FlasherFlash7_Timer
    End Select
end sub 

Sub Lightning7off_Timer
    Lightning7.enabled=false
    Lightning7off.enabled=false
    Lightning8.enabled=true:Lightning8.interval=100:Lightning8off.enabled=true:Lightning8off.interval=300
end sub

Sub Lightning8_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:FlashLevel8 = 1 : FlasherFlash8_Timer
    Case 1:FlashLevel9 = 1 : FlasherFlash9_Timer
    Case 2:FlashLevel9 = 1 : FlasherFlash9_Timer
    Case 3:FlashLevel8 = 1 : FlasherFlash8_Timer
    End Select
end sub 

Sub Lightning8off_Timer
    Lightning8.enabled=false
    Lightning8off.enabled=false
    Lightning9.enabled=true:Lightning9.interval=100:Lightning9off.enabled=true:Lightning9off.interval=200
end sub


Sub Lightning9_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:FlashLevel12 = 1 : FlasherFlash12_Timer
    Case 1:FlashLevel14 = 1 : FlasherFlash14_Timer
    Case 2:FlashLevel12 = 1 : FlasherFlash12_Timer
    End Select
end sub 

Sub Lightning9off_Timer
    Lightning9.enabled=false
    Lightning9off.enabled=false
    Lightning10.enabled=true:Lightning10.interval=100:Lightning10off.enabled=true:Lightning10off.interval=200
end sub

Sub Lightning10_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:FlashLevel14 = 1 : FlasherFlash14_Timer
    Case 1:FlashLevel15 = 1 : FlasherFlash15_Timer:FlashLevel14 = 1 : FlasherFlash14_Timer
    Case 2:FlashLevel14 = 1 : FlasherFlash14_Timer
    End Select
end sub 

Sub Lightning10off_Timer
    Lightning10.enabled=false
    Lightning10off.enabled=false
end sub


'*************************************
'Lightning Right

Sub Lightning11_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:FlashLevel1 = 1 : FlasherFlash1_Timer:FlashLevel2 = 1 : FlasherFlash2_Timer
    Case 1:FlashLevel3 = 1 : FlasherFlash3_Timer:FlashLevel4 = 1 : FlasherFlash4_Timer:FlashLevel5 = 1 : FlasherFlash5_Timer
    End Select
end sub 


Sub Lightning11off_Timer
    Lightning11.enabled=false
    Lightning11off.enabled=false
    Lightning12.enabled=true:Lightning12.interval=100:Lightning12off.enabled=true:Lightning12off.interval=400
end sub


Sub Lightning12_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:FlashLevel5 = 1 : FlasherFlash5_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 1:FlashLevel7 = 1 : FlasherFlash7_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 2:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 3:FlashLevel7 = 1 : FlasherFlash7_Timer
    End Select
end sub 

Sub Lightning12off_Timer
    Lightning12.enabled=false
    Lightning12off.enabled=false
    Lightning13.enabled=true:Lightning13.interval=100:Lightning13off.enabled=true:Lightning13off.interval=300
end sub

Sub Lightning13_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:FlashLevel10 = 1 : FlasherFlash10_Timer
    Case 1:FlashLevel11 = 1 : FlasherFlash11_Timer
    Case 2:FlashLevel10 = 1 : FlasherFlash10_Timer
    Case 3:FlashLevel11 = 1 : FlasherFlash11_Timer
    End Select
end sub 

Sub Lightning13off_Timer
    Lightning13.enabled=false
    Lightning13off.enabled=false
    Lightning14.enabled=true:Lightning14.interval=100:Lightning14off.enabled=true:Lightning14off.interval=200
end sub


Sub Lightning14_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:FlashLevel13 = 1 : FlasherFlash13_Timer
    Case 1:FlashLevel15 = 1 : FlasherFlash15_Timer
    Case 2:FlashLevel13 = 1 : FlasherFlash13_Timer
    End Select
end sub 

Sub Lightning14off_Timer
    Lightning14.enabled=false
    Lightning14off.enabled=false
    Lightning15.enabled=true:Lightning15.interval=100:Lightning15off.enabled=true:Lightning15off.interval=200
end sub

Sub Lightning15_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:FlashLevel15 = 1 : FlasherFlash15_Timer
    Case 1:FlashLevel15 = 1 : FlasherFlash15_Timer:FlashLevel14 = 1 : FlasherFlash14_Timer
    Case 2:FlashLevel15 = 1 : FlasherFlash15_Timer
    End Select
end sub 

Sub Lightning15off_Timer
    Lightning15.enabled=false
    Lightning15off.enabled=false
end sub


'*********************************
'Lightning Left to Right

Sub Lightning16_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:FlashLevel1 = 1 : FlasherFlash1_Timer:FlashLevel2 = 1 : FlasherFlash2_Timer
    Case 1:FlashLevel3 = 1 : FlasherFlash3_Timer:FlashLevel4 = 1 : FlasherFlash4_Timer:FlashLevel5 = 1 : FlasherFlash5_Timer
    End Select
end sub 


Sub Lightning16off_Timer
    Lightning16.enabled=false
    Lightning16off.enabled=false
    Lightning17.enabled=true:Lightning17.interval=100:Lightning17off.enabled=true:Lightning17off.interval=400
end sub


Sub Lightning17_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:FlashLevel5 = 1 : FlasherFlash5_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 1:FlashLevel7 = 1 : FlasherFlash7_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 2:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 3:FlashLevel7 = 1 : FlasherFlash7_Timer
    End Select
end sub 

Sub Lightning17off_Timer
    Lightning17.enabled=false
    Lightning17off.enabled=false
    Lightning18.enabled=true:Lightning18.interval=100:Lightning18off.enabled=true:Lightning18off.interval=300
end sub

Sub Lightning18_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:FlashLevel8 = 1 : FlasherFlash8_Timer
    Case 1:FlashLevel9 = 1 : FlasherFlash9_Timer
    Case 2:FlashLevel9 = 1 : FlasherFlash9_Timer
    Case 3:FlashLevel8 = 1 : FlasherFlash8_Timer
    End Select
end sub 

Sub Lightning18off_Timer
    Lightning18.enabled=false
    Lightning18off.enabled=false
    Lightning19.enabled=true:Lightning19.interval=100:Lightning19off.enabled=true:Lightning19off.interval=200
end sub


Sub Lightning19_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:FlashLevel13 = 1 : FlasherFlash13_Timer
    Case 1:FlashLevel11 = 1 : FlasherFlash11_Timer
    Case 2:FlashLevel13 = 1 : FlasherFlash13_Timer
    End Select
end sub 

Sub Lightning19off_Timer
    Lightning19.enabled=false
    Lightning19off.enabled=false
    Lightning20.enabled=true:Lightning20.interval=100:Lightning20off.enabled=true:Lightning20off.interval=200
end sub

Sub Lightning20_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:FlashLevel15 = 1 : FlasherFlash15_Timer
    Case 1:FlashLevel15 = 1 : FlasherFlash15_Timer:FlashLevel14 = 1 : FlasherFlash14_Timer
    Case 2:FlashLevel15 = 1 : FlasherFlash15_Timer
    End Select
end sub 

Sub Lightning20off_Timer
    Lightning20.enabled=false
    Lightning20off.enabled=false
end sub

'*************************************
'Lightning Right to Left

Sub Lightning21_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:FlashLevel1 = 1 : FlasherFlash1_Timer:FlashLevel2 = 1 : FlasherFlash2_Timer
    Case 1:FlashLevel3 = 1 : FlasherFlash3_Timer:FlashLevel4 = 1 : FlasherFlash4_Timer:FlashLevel5 = 1 : FlasherFlash5_Timer
    End Select
end sub 


Sub Lightning21off_Timer
    Lightning21.enabled=false
    Lightning21off.enabled=false
    Lightning22.enabled=true:Lightning22.interval=100:Lightning22off.enabled=true:Lightning22off.interval=400
end sub


Sub Lightning22_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:FlashLevel5 = 1 : FlasherFlash5_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 1:FlashLevel7 = 1 : FlasherFlash7_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 2:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 3:FlashLevel7 = 1 : FlasherFlash7_Timer
    End Select
end sub 

Sub Lightning22off_Timer
    Lightning22.enabled=false
    Lightning22off.enabled=false
    Lightning23.enabled=true:Lightning23.interval=100:Lightning23off.enabled=true:Lightning23off.interval=300
end sub

Sub Lightning23_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:FlashLevel10 = 1 : FlasherFlash10_Timer
    Case 1:FlashLevel11 = 1 : FlasherFlash11_Timer
    Case 2:FlashLevel10 = 1 : FlasherFlash10_Timer
    Case 3:FlashLevel11 = 1 : FlasherFlash11_Timer
    End Select
end sub 

Sub Lightning23off_Timer
    Lightning23.enabled=false
    Lightning23off.enabled=false
    Lightning24.enabled=true:Lightning24.interval=100:Lightning24off.enabled=true:Lightning24off.interval=200
end sub


Sub Lightning24_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:FlashLevel12 = 1 : FlasherFlash12_Timer
    Case 1:FlashLevel9 = 1 : FlasherFlash9_Timer
    Case 2:FlashLevel12 = 1 : FlasherFlash12_Timer
    End Select
end sub 

Sub Lightning24off_Timer
    Lightning24.enabled=false
    Lightning24off.enabled=false
    Lightning25.enabled=true:Lightning25.interval=100:Lightning25off.enabled=true:Lightning25off.interval=200
end sub

Sub Lightning25_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:FlashLevel14 = 1 : FlasherFlash14_Timer
    Case 1:FlashLevel15 = 1 : FlasherFlash15_Timer:FlashLevel14 = 1 : FlasherFlash14_Timer
    Case 2:FlashLevel14 = 1 : FlasherFlash14_Timer
    End Select
end sub 

Sub Lightning25off_Timer
    Lightning25.enabled=false
    Lightning25off.enabled=false
end sub

'********************************
'Random Lightning Pattern Selection

Sub Lightning26_Timer
    Dim x
    x = INT(5 * RND(1) )
    Select Case x
    Case 0:Lightning1.enabled=true:Lightning1.interval=100:Lightning1off.enabled=true:Lightning1off.interval=500
    Case 1:Lightning6.enabled=true:Lightning6.interval=100:Lightning6off.enabled=true:Lightning6off.interval=500
    Case 2:Lightning11.enabled=true:Lightning11.interval=100:Lightning11off.enabled=true:Lightning11off.interval=500
    Case 3:Lightning16.enabled=true:Lightning16.interval=100:Lightning16off.enabled=true:Lightning16off.interval=500
    Case 4:Lightning21.enabled=true:Lightning21.interval=100:Lightning21off.enabled=true:Lightning21off.interval=500
    End Select
Lightning26.enabled=false
end sub 

'********************************

Sub BallReleaseGate_Hit()
        Wall49.timerenabled=0:Wall45.timerenabled=0:Wall46.timerenabled=0:CC=CC+1
        If BB=0 and CC=<5 Then
			Dim o
			o = INT(7 * RND(1) )
			Select Case o
			Case 0:PlayMusic"0np02.mp3":Wall47.timerinterval=26000:Wall47.timerenabled=1
			Case 1:PlayMusic"0np03.mp3":Wall47.timerinterval=75000:Wall47.timerenabled=1
			Case 2:PlayMusic"0np04.mp3":Wall47.timerinterval=66500:Wall47.timerenabled=1
			Case 3:PlayMusic"0np05.mp3":Wall47.timerinterval=123000:Wall47.timerenabled=1
			Case 4:PlayMusic"0np06.mp3":Wall47.timerinterval=69000:Wall47.timerenabled=1
			Case 5:PlayMusic"0np07.mp3":Wall47.timerinterval=50500:Wall47.timerenabled=1
			Case 6:PlayMusic"0np08.mp3":Wall47.timerinterval=155500:Wall47.timerenabled=1
			End Select
        End If
        If CC=5 Then
            PlayMusic"0np09.mp3":Wall47.timerinterval=107000:Wall47.timerenabled=1:CC=0
        End If
        If BallsPerGame=3 and CC=3 then PlayMusic"0np09.mp3":Wall47.timerinterval=107000:Wall47.timerenabled=1:CC=0:End If
End Sub



Sub Wall44_Hit()
	
	If TableTilted=false then
		AddScore(10)
	End if
	
End Sub

Sub Wall44_Timer
    Dim x
    x = INT(9 * RND(1) )
    Select Case x
    Case 0:FlashLevel1 = 1 : FlasherFlash1_Timer:FlashLevel2 = 1 : FlasherFlash2_Timer
    Case 1:FlashLevel3 = 1 : FlasherFlash3_Timer:FlashLevel4 = 1 : FlasherFlash4_Timer:FlashLevel5 = 1 : FlasherFlash5_Timer
    Case 2:FlashLevel5 = 1 : FlasherFlash5_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 3:FlashLevel7 = 1 : FlasherFlash7_Timer:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 4:FlashLevel8 = 1 : FlasherFlash8_Timer:FlashLevel10 = 1 : FlasherFlash10_Timer
    Case 5:FlashLevel11 = 1 : FlasherFlash11_Timer:FlashLevel9 = 1 : FlasherFlash9_Timer
    Case 6:FlashLevel13 = 1 : FlasherFlash13_Timer:FlashLevel12 = 1 : FlasherFlash12_Timer
    Case 7:FlashLevel15 = 1 : FlasherFlash15_Timer:FlashLevel14 = 1 : FlasherFlash14_Timer
    Case 8:FlashLevel15 = 1 : FlasherFlash15_Timer:FlashLevel14 = 1 : FlasherFlash14_Timer:FlashLevel1 = 1 : FlasherFlash1_Timer:FlashLevel2 = 1 : FlasherFlash2_Timer:FlashLevel3 = 1 : FlasherFlash3_Timer:FlashLevel4 = 1 : FlasherFlash4_Timer:FlashLevel5 = 1 : FlasherFlash5_Timer
    End Select

End Sub

Sub Wall33_Hit()
	
	If TableTilted=false then
		AddScore(10)
	End if
End Sub

Sub Wall33_Timer
         Wall44.timerenabled=0
         Wall33.timerenabled=0
End Sub

Sub Wall49_Hit()
	
	If TableTilted=false then
		AddScore(10)
	End if
End Sub

Sub Wall49_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:PlayMusic"0na05.mp3":TargetLeft1.timerinterval=121000:TargetLeft1.timerenabled=1
    Case 1:PlayMusic"0na09.mp3":TargetLeft1.timerinterval=20500:TargetLeft1.timerenabled=1
    End Select
Wall49.timerenabled=0
End Sub

Sub Wall45_Hit()
	
	If TableTilted=false then
		AddScore(10)
	End if
End Sub

Sub Wall45_Timer
      PlayMusic"0na08.mp3":Wall46.timerinterval=91500:Wall46.timerenabled=1
      Trigger2.timerinterval=56500:Trigger2.timerenabled=1
Wall45.timerenabled=0
End Sub

Sub Wall46_Hit()
	
	If TableTilted=false then
		AddScore(10)
	End if
End Sub

Sub Wall46_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:PlayMusic"0na01.mp3":Wall49.timerinterval=56500:Wall49.timerenabled=1:Trigger2.timerinterval=56500:Trigger2.timerenabled=1
    Case 1:PlayMusic"0na04.mp3":Wall49.timerinterval=30000:Wall49.timerenabled=1
    End Select
Wall46.timerenabled=0
End Sub

Sub Wall47_Hit()
	
	If TableTilted=false then
		AddScore(10)
	End if
End Sub

Sub Wall47_Timer
    Dim x
    x = INT(7 * RND(1) )
    Select Case x
			Case 0:PlayMusic"0np02.mp3":Bumper1.timerinterval=26000:Bumper1.timerenabled=1
			Case 1:PlayMusic"0np03.mp3":Bumper1.timerinterval=75000:Bumper1.timerenabled=1
			Case 2:PlayMusic"0np04.mp3":Bumper1.timerinterval=66500:Bumper1.timerenabled=1
			Case 3:PlayMusic"0np05.mp3":Bumper1.timerinterval=123000:Bumper1.timerenabled=1
			Case 4:PlayMusic"0np06.mp3":Bumper1.timerinterval=69000:Bumper1.timerenabled=1
			Case 5:PlayMusic"0np07.mp3":Bumper1.timerinterval=50500:Bumper1.timerenabled=1
			Case 6:PlayMusic"0np08.mp3":Bumper1.timerinterval=155500:Bumper1.timerenabled=1
    End Select
Wall47.timerenabled=0
End Sub

Sub Bumper1_Hit
	If TableTilted=false then

		PlaySound "bumper1"
		DOF 107, 2
		bump1 = 1
		If Bumper1Light.state=1 then
			AddScore(100)
		Else
			AddScore(10)
		End If
		ToggleBumper
    end if
    
End Sub

Sub Bumper1_Timer
    Dim x
    x = INT(7 * RND(1) )
    Select Case x
			Case 0:PlayMusic"0np02.mp3":Wall47.timerinterval=26000:Wall47.timerenabled=1
			Case 1:PlayMusic"0np03.mp3":Wall47.timerinterval=75000:Wall47.timerenabled=1
			Case 2:PlayMusic"0np04.mp3":Wall47.timerinterval=66500:Wall47.timerenabled=1
			Case 3:PlayMusic"0np05.mp3":Wall47.timerinterval=123000:Wall47.timerenabled=1
			Case 4:PlayMusic"0np06.mp3":Wall47.timerinterval=69000:Wall47.timerenabled=1
			Case 5:PlayMusic"0np07.mp3":Wall47.timerinterval=50500:Wall47.timerenabled=1
			Case 6:PlayMusic"0np08.mp3":Wall47.timerinterval=155500:Wall47.timerenabled=1
    End Select
Bumper1.timerenabled=0
End Sub

Sub Bumper2_Hit
	If TableTilted=false then

		PlaySound "bumper1"
		DOF 108, 2
		bump2 = 1
		If Bumper2Light.state=1 then
			AddScore(1000)
		Else
			AddScore(100)
		End If
    end if
    
End Sub

Sub Bumper2_Timer
    AA=0
Bumper2.timerenabled=0
End Sub

Sub Bumper3_Hit
	If TableTilted=false then

		PlaySound "bumper1"
		DOF 109, 2
		bump3 = 1
		If Bumper3Light.state=1 then
			AddScore(100)
		Else
			AddScore(10)
		End If
		ToggleBumper
    end if
    
End Sub

Sub Bumper3_Timer
    BB=0:PlayMusic"0np01.mp3":Wall47.timerinterval=135000:Wall47.timerenabled=1
Bumper3.timerenabled=0
End Sub


Sub TargetLeft1_Hit()
	If TableTilted=false then
		DOF 125, 2
        StopSound "0nt11":PlaySound"0nt11"
		If LeftRolloverLight.state=1 then
			SetMotor(500)
			IncreaseBonus
		else
			SetMotor(50)
		end if
	end if
end Sub

Sub TargetLeft1_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:PlayMusic"0na02.mp3":TargetRight1.timerinterval=21500:TargetRight1.timerenabled=1
    Case 1:PlayMusic"0na10.mp3":TargetRight1.timerinterval=20000:TargetRight1.timerenabled=1
    End Select
TargetLeft1.timerenabled=0
End Sub


Sub TargetRight1_Hit()
	If TableTilted=false then
		DOF 126, 2
        StopSound "0nt11":PlaySound"0nt11"
		If RightRolloverLight.state=1 then
			SetMotor(500)
			IncreaseBonus
		else
			SetMotor(50)
		end if
	end if

end Sub


Sub TargetRight1_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:PlayMusic"0na03.mp3":TriggerTop1.timerinterval=27300:TriggerTop1.timerenabled=1
    Case 1:PlayMusic"0na07.mp3":TriggerTop1.timerinterval=125200:TriggerTop1.timerenabled=1
    End Select
TargetRight1.timerenabled=0
End Sub


Sub TriggerTop1_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 116, 2
        If AA=0 then
         AA=1
         Dim x
         x = INT(14 * RND(1) )
         Select Case x
	        Case 0:PlaySound"0nt01":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
		    Case 1:PlaySound"0nt02":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
		    Case 2:PlaySound"0nt03":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 3:PlaySound"0nt04":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 4:PlaySound"0nt01":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 5:PlaySound"0nt06":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 6:PlaySound"0nt07":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 7:PlaySound"0nt08":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 8:PlaySound"0nt09":Bumper2.timerinterval=6000:Bumper2.timerenabled=1
			Case 9:PlaySound"0nt10":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 10:PlaySound"0nt01":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 11:PlaySound"0nt12":Bumper2.timerinterval=3500:Bumper2.timerenabled=1
			Case 12:PlaySound"0nt13":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 13:PlaySound"0nt14":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
         End Select
        end if
		If UpperLight1.state=1 then
			SetMotor(500)
			IncreaseBonus
		else
			SetMotor(50)
		end if
		
	end if
End Sub


Sub TriggerTop1_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:PlayMusic"0na08.mp3":Wall46.timerinterval=91500:Wall46.timerenabled=1:Trigger2.timerinterval=56500:Trigger2.timerenabled=1
    Case 1:PlayMusic"0na11.mp3":Wall46.timerinterval=14600:Wall46.timerenabled=1
    End Select
TriggerTop1.timerenabled=0
End Sub


Sub TriggerTop2_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 117, 2
        If AA=0 then
         AA=1
         Dim x
         x = INT(14 * RND(1) )
         Select Case x
	        Case 0:PlaySound"0nt01":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
		    Case 1:PlaySound"0nt02":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
		    Case 2:PlaySound"0nt03":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 3:PlaySound"0nt04":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 4:PlaySound"0nt01":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 5:PlaySound"0nt06":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 6:PlaySound"0nt07":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 7:PlaySound"0nt08":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 8:PlaySound"0nt09":Bumper2.timerinterval=6000:Bumper2.timerenabled=1
			Case 9:PlaySound"0nt10":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 10:PlaySound"0nt01":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 11:PlaySound"0nt12":Bumper2.timerinterval=3500:Bumper2.timerenabled=1
			Case 12:PlaySound"0nt13":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 13:PlaySound"0nt14":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
         End Select
        end if
		If UpperLight2.state=1 then
			SetMotor(500)
			IncreaseBonus
		else
			SetMotor(50)
		end if
		
	end if
End Sub


Sub TriggerTop3_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 118, 2
        If AA=0 then
         AA=1
         Dim x
         x = INT(14 * RND(1) )
         Select Case x
	        Case 0:PlaySound"0nt01":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
		    Case 1:PlaySound"0nt02":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
		    Case 2:PlaySound"0nt03":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 3:PlaySound"0nt04":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 4:PlaySound"0nt01":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 5:PlaySound"0nt06":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 6:PlaySound"0nt07":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 7:PlaySound"0nt08":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 8:PlaySound"0nt09":Bumper2.timerinterval=6000:Bumper2.timerenabled=1
			Case 9:PlaySound"0nt10":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 10:PlaySound"0nt01":Bumper2.timerinterval=3000:Bumper2.timerenabled=1
			Case 11:PlaySound"0nt12":Bumper2.timerinterval=3500:Bumper2.timerenabled=1
			Case 12:PlaySound"0nt13":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
			Case 13:PlaySound"0nt14":Bumper2.timerinterval=4000:Bumper2.timerenabled=1
         End Select
        end if
		If UpperLight3.state=1 then
			SetMotor(500)
			IncreaseBonus
		else
			SetMotor(50)
		end if
		
	end if
End Sub


Sub TriggerRightRollover_Hit()
	If TableTilted=false then
		PlaySound "sensor":PlaySound"0nc08"
		DOF 124, 2
		If RightRolloverLight.state=1 then
			SetMotor(500)
			IncreaseBonus
		Else
			SetMotor(50)
		End If
		if dooralreadyopen=0 then
			openg.enabled=true:PlaySound"0ng01"
			DOF 132, 2
		end if
	End if
End Sub

Sub TriggerLeftRollover_Hit()
	If TableTilted=false then
		PlaySound "sensor":PlaySound"0nc08"
		DOF 123, 2
		If LeftRolloverLight.state=1 then
			SetMotor(500)
			IncreaseBonus
		Else
			SetMotor(50)
		End If
		if kgdooralreadyopen=0 then
			openkg.enabled=true:PlaySound"0ng02"
			DOF 132, 2
		end if
	End If
End Sub

Sub TriggerLeftInlane_Hit()
	If TableTilted=false then
		PlaySound "sensor":PlaySound"0nc07"
		DOF 120, 2
		If LeftInlaneLight.state=1 then
			SetMotor(500)
			IncreaseBonus
		else
			SetMotor(50)
		end if
	End If
End Sub

Sub TriggerRightInlane_Hit()
	If TableTilted=false then
		PlaySound "sensor":PlaySound"0nc07"
		DOF 121, 2
		If RightInlaneLight.state=1 then
			SetMotor(500)
			IncreaseBonus
		else
			SetMotor(50)
		end if
	End If
End Sub

Sub TriggerLeftOutlane_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 119, 2
        Dim x
         x = INT(5 * RND(1) )
         Select Case x
         Case 0:PlaySound"0nc01"
         Case 1:PlaySound"0nc02"
         Case 2:PlaySound"0nc03"
         Case 3:PlaySound"0nc04"
         Case 4:PlaySound"0nc05"
        End Select
		SetMotor(500)
		
		If LeftOutlaneLight.state=1 then
			AddSpecial
		end if
		IncreaseBonus
	End If
End Sub

Sub TriggerRightOutlane_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 122, 2
        Dim x
         x = INT(5 * RND(1) )
         Select Case x
         Case 0:PlaySound"0nc01"
         Case 1:PlaySound"0nc02"
         Case 2:PlaySound"0nc03"
         Case 3:PlaySound"0nc04"
         Case 4:PlaySound"0nc05"
        End Select
		SetMotor(500)
		
		If RightOutlaneLight.state=1 then
			AddSpecial
		end if
		IncreaseBonus
	End If
End Sub

Sub StarTrigger1_Hit()
	if TableTilted=false then
		AddScore(100)
		ToggleBumper
		DOF 130, 2:PlaySound"0nc07"
		if RightTargetLight1.state=1 then
			RightTargetLight1.state=0
			LeftTargetLight1.state=0
			RightTargetLight2.state=1
			LeftTargetLight2.state=1
			exit sub
		end if
		if RightTargetLight2.state=1 then
			RightTargetLight2.state=0
			LeftTargetLight2.state=0
			RightTargetLight3.state=1
			LeftTargetLight3.state=1
			exit sub
		end if
		if RightTargetLight3.state=1 then
			RightTargetLight3.state=0
			LeftTargetLight3.state=0
			RightTargetLight4.state=1
			LeftTargetLight4.state=1
			exit sub
		end if
		if RightTargetLight4.state=1 then
			RightTargetLight4.state=0
			LeftTargetLight4.state=0
			RightTargetLight5.state=1
			LeftTargetLight5.state=1
			exit sub
		end if
		if RightTargetLight5.state=1 then
			RightTargetLight5.state=0
			LeftTargetLight5.state=0
			RightTargetLight1.state=1
			LeftTargetLight1.state=1
			exit sub
		end if
		
	end if
End Sub

Sub Lefttarget1_Hit()
	if TableTilted=false then
		DOF 110, 2
		if Lefttarget1.IsDropped=0 then
			LeftTargetCounter=LeftTargetCounter+1
			targettempscore=50
			if LeftTargetLight1.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			if LeftTarget10x.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			Lefttarget1.IsDropped=1
			SetMotor(targettempscore)
			CheckAllDrops
		end if
	end if
end sub

Sub Lefttarget2_Hit()
	if TableTilted=false then
		DOF 110, 2
		if Lefttarget2.IsDropped=0 then
			LeftTargetCounter=LeftTargetCounter+1
			targettempscore=50
			if LeftTargetLight2.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			if LeftTarget10x.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			Lefttarget2.IsDropped=1
			SetMotor(targettempscore)
			CheckAllDrops
		end if
	end if
end sub

Sub Lefttarget3_Hit()
	if TableTilted=false then
		DOF 110, 2
		if Lefttarget3.IsDropped=0 then
			LeftTargetCounter=LeftTargetCounter+1
			targettempscore=50
			if LeftTargetLight3.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			if LeftTarget10x.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			Lefttarget3.IsDropped=1
			SetMotor(targettempscore)
			CheckAllDrops
		end if
	end if
end sub

Sub Lefttarget4_Hit()
	if TableTilted=false then
		DOF 110, 2
		if Lefttarget4.IsDropped=0 then
			LeftTargetCounter=LeftTargetCounter+1
			targettempscore=50
			if LeftTargetLight4.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			if LeftTarget10x.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			Lefttarget4.IsDropped=1
			SetMotor(targettempscore)
			CheckAllDrops
		end if
	end if
end sub

Sub Lefttarget5_Hit()
	if TableTilted=false then
		DOF 110, 2
		if Lefttarget5.IsDropped=0 then
			LeftTargetCounter=LeftTargetCounter+1
			targettempscore=50
			if LeftTargetLight5.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			if LeftTarget10x.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			Lefttarget5.IsDropped=1
			SetMotor(targettempscore)
			CheckAllDrops
		end if
	end if
end sub

Sub Righttarget1_Hit()
	if TableTilted=false then
		DOF 111, 2
		if Righttarget1.IsDropped=0 then
			RightTargetCounter=RightTargetCounter+1
			targettempscore=50
			if RightTargetLight1.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			if RightTarget10x.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			Righttarget1.IsDropped=1
			SetMotor(targettempscore)
			CheckAllDrops
		end if
	end if
end sub

Sub Righttarget2_Hit()
	if TableTilted=false then
		DOF 111, 2
		if Righttarget2.IsDropped=0 then
			RightTargetCounter=RightTargetCounter+1
			targettempscore=50
			if RightTargetLight2.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			if RightTarget10x.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			Righttarget2.IsDropped=1
			SetMotor(targettempscore)
			CheckAllDrops
		end if
	end if
end sub

Sub Righttarget3_Hit()
	if TableTilted=false then
		DOF 111, 2
		if Righttarget3.IsDropped=0 then
			RightTargetCounter=RightTargetCounter+1
			targettempscore=50
			if RightTargetLight3.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			if RightTarget10x.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			Righttarget3.IsDropped=1
			SetMotor(targettempscore)
			CheckAllDrops
		end if
	end if
end sub

Sub Righttarget4_Hit()
	if TableTilted=false then
		DOF 111, 2
		if Righttarget4.IsDropped=0 then
			RightTargetCounter=RightTargetCounter+1
			targettempscore=50
			if RightTargetLight4.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			if RightTarget10x.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			Righttarget4.IsDropped=1
			SetMotor(targettempscore)
			CheckAllDrops
		end if
	end if
end sub

Sub Righttarget5_Hit()
	if TableTilted=false then
		DOF 111, 2
		if Righttarget5.IsDropped=0 then
			RightTargetCounter=RightTargetCounter+1
			targettempscore=50
			if RightTargetLight5.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			if RightTarget10x.state=1 then
				targettempscore=targettempscore*10
				IncreaseBonus
			end if
			Righttarget5.IsDropped=1
			SetMotor(targettempscore)
			CheckAllDrops
		end if
	end if
end sub

Sub ResetDrops
	for each obj in LeftDrops
		obj.IsDropped=0
	next
	DOF 112, 2	
	for each obj in RightDrops
		obj.IsDropped=0
	next
	DOF 113, 2	
	LeftTargetCounter=0
	RightTargetCounter=0
end Sub

Sub CheckAllDrops
	

	if BallsPerGame=5 and (LeftTargetCounter=5) and (RightTargetCounter=5) then
			DoubleBonus.state=1
			BonusMultiplier=2
			
	end if
	if (BallsPerGame=3) and ((LeftTargetCounter=5) or (RightTargetCounter=5)) then
			DoubleBonus.state=1
			BonusMultiplier=2
			
	end if
end Sub

sub ResetDropsTimer_timer
		ResetDropsTimer.enabled=0
		ResetDrops
end sub

Sub Kicker3_Hit()
		Kicker3.TIMERINTERVAL = 100
		Kicker3.TIMERENABLED = TRUE
		
End Sub

Sub Kicker3_Timer()
	Kicker3.TIMERENABLED = FALSE
	Kicker3.Kick 0,22
	PlaySound"saucer"
	DOF 127, 2
	delaykgclose.enabled=true

End Sub




Sub CloseGateTrigger_Hit()
	if dooralreadyopen=1 then
		closeg.enabled=true:PlaySound"0ng03"
		DOF 132, 2
	end if
End Sub


Sub AddSpecial()
	PlaySound"knocker"
	Credits=Credits+1
	DOF 114, 1
	DOF 115, 2
	if Credits>15 then Credits=15
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	CreditsReel.SetValue(Credits)
End Sub

Sub AddSpecial2()
	PlaySound"click"
	DOF 114, 1
	Credits=Credits+1
	if Credits>15 then Credits=15
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	CreditsReel.SetValue(Credits)
End Sub

Sub AddBonus()
	bonuscountdown=bonuscounter
	ScoreBonus.enabled=true
End Sub

Sub ToggleBumper
	if UpperLight1.state=1 then
		UpperLight1.state=0
		LeftTarget10x.state=0
		RightRolloverLight.state=0
		LeftInlaneLight.state=0
		
		UpperLight3.state=1
		RightTarget10x.state=1
		LeftRolloverLight.state=1
		RightInlaneLight.state=1
		Bumper1Light.state=1
		Bumper2Light.state=1
		Bumper3Light.state=1
		BumpersOn
	else
		UpperLight1.state=1
		LeftTarget10x.state=1
		RightRolloverLight.state=1
		LeftInlaneLight.state=1
		
		UpperLight3.state=0
		RightTarget10x.state=0
		LeftRolloverLight.state=0
		RightInlaneLight.state=0
		Bumper1Light.state=0
		Bumper2Light.state=0
		Bumper3Light.state=0
		BumpersOff
	end if
end sub



Sub ResetBallDrops()


	ResetDrops
	for each obj in Bonus
		obj.state=0
	next
	for each obj in StarLights
		obj.state=1
	next
	BonusCounter=0
	HoleCounter=0
	LeftOutlaneLight.state=0
	RightOutlaneLight.state=0
End Sub


Sub LightsOut
	for each obj in Bonus
		obj.state=0
	next
	for each obj in StarLights
		obj.state=0
	next
	BonusCounter=0
	HoleCounter=0
	Bumper1Light.state=0
	Bumper2Light.state=0
	Bumper3Light.state=0

	if dooralreadyopen=1 then
		closeg.enabled=true
		DOF 132, 2
	end if
	if kgdooralreadyopen=1 then
		closekg.enabled=true
		DOF 132, 2
	end if
end sub

Sub ResetBalls()

	TempMultiCounter=BallsPerGame-BallInPlay

	ResetBallDrops
	BonusMultiplier=1
	DoubleBonus.state=0
	If (BallsPerGame=BallInPlay) then
		DoubleBonus.state=1
		BonusMultiplier=2
	End If
	TableTilted=false
	TiltReel.SetValue(0)
	If B2Son then
		Controller.B2SSetTilt 0
	end if
	PlasticsOn
	'CreateBallID BallRelease
	Ballrelease.CreateSizedBall 25
    Ballrelease.Kick 40,7
	DOF 106, 2	
	if dooralreadyopen=1 then
		closeg.enabled=true
		DOF 132, 2
	end if
	if kgdooralreadyopen=1 then
		closekg.enabled=true
		DOF 132, 2
	end if
	BallInPlayReel.SetValue(BallInPlay)
End Sub

sub delaykgclose_timer
	delaykgclose.enabled=false
	closekg.enabled=true:PlaySound"0ng03"
	DOF 132, 2
end sub

 sub openg_timer
    bottgate(bgpos).isdropped=true
    bgpos=bgpos-1
    bottgate(bgpos).isdropped=false  
	primgate.RotY=30+(bgpos*10)
     if bgpos=0 then 
		playsound "postup"
		GateOpenLight.state=1
		openg.enabled=false
		DOF 132, 2
 		dooralreadyopen=1
	end if

 end sub 

sub closeg_timer
    closeg.interval=10
    bottgate(bgpos).isdropped=true
    bgpos=bgpos+1
    bottgate(bgpos).isdropped=false   
    primgate.RotY=30+(bgpos*10)
 	if bgpos=6 then
		GateOpenLight.state=0
 		closeg.enabled=false
		DOF 132, 2
		dooralreadyopen=0
 	end if
end sub   

 sub openkg_timer
    kickgate(kgpos).isdropped=true
    kgpos=kgpos+1
    kickgate(kgpos).isdropped=false  
	primgate1.RotY=30+(kgpos*10)
     if kgpos=6 then 
		playsound "postup"
		KickGateOpenLight.state=1
		openkg.enabled=false
		DOF 132, 2
 		kgdooralreadyopen=1
	end if

 end sub 

sub closekg_timer
    closekg.interval=10
    kickgate(kgpos).isdropped=true
    kgpos=kgpos-1
    kickgate(kgpos).isdropped=false   
    primgate1.RotY=30+(kgpos*10)
 	if kgpos=0 then
		KickGateOpenLight.state=0
 		closekg.enabled=false
		DOF 132, 2
		kgdooralreadyopen=0
 	end if
end sub   

sub resettimer_timer
    rst=rst+1
    for i=1 to 4
 	If B2SOn Then 
		Controller.B2SSetScorePlayer i, 0
	End If    
	next   
    if rst=20 then
    playsound "StartBall1"
    end if
    if rst=24 then
    newgame
    resettimer.enabled=false
    end if
end sub

sub ScoreBonus_timer

	if bonuscountdown<=0 then
		ScoreBonus.enabled=false
		ScoreBonus.interval=600
		NextBallDelay.enabled=true
		exit sub
	end if
	if BonusCountdown>10 then
		SetMotor2(1000*BonusMultiplier)
		Bonus(bonuscountdown-10).state=0
	else
		SetMotor2(1000*BonusMultiplier)
		Bonus(bonuscountdown).state=0
	end if
	If BonusMultiplier=2 then
		Bonus(0).state=1
	end if
	If BonusMultiplier=1 then
		ScoreBonus.interval=400
	Else
		ScoreBonus.interval=400
	end if
	bonuscountdown=bonuscountdown-1

	if bonuscountdown>10 then
		Bonus(bonuscountdown-10).state=1
		exit sub
	end if
	if bonuscountdown>0 then
		Bonus(bonuscountdown).state=1
	end if

end sub

sub NextBallDelay_timer()
	NextBallDelay.enabled=false
	nextball

end sub

sub newgame
	InProgress=true
	queuedscore=0
	for i = 1 to 4
		Score(i)=0
		Score100K(1)=0
		HighScorePaid(i)=false
		Replay1Paid(i)=false
		Replay2Paid(i)=false
		Replay3Paid(i)=false
	next
	If B2SOn Then
		Controller.B2SSetTilt 0
		Controller.B2SSetGameOver 0
		Controller.B2SSetMatch 0
'		Controller.B2SSetScorePlayer1 0
'		Controller.B2SSetScorePlayer2 0
'		Controller.B2SSetScorePlayer3 0
'		Controller.B2SSetScorePlayer4 0
		Controller.B2SSetBallInPlay BallInPlay
	End if
	UpperLight2.state=1
	for each obj in LeftTargetLights
		obj.state=0
	next
	for each obj in RightTargetLights
		obj.state=0
	next

	LeftTargetLight1.state=1
	RightTargetLight1.state=1
	ToggleBumper
	ResetBalls
End sub

sub nextball
	If B2SOn Then
		Controller.B2SSetTilt 0
	End If
	Player=Player+1
	If Player>Players Then
		BallInPlay=BallInPlay+1
		If BallInPlay>BallsPerGame then
			PlaySound("MotorLeer")
			InProgress=false
			GameOverReel.SetValue(1)
			If B2SOn Then
				Controller.B2SSetGameOver 1
				Controller.B2SSetPlayerUp 0
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetBallInPlay 0
				Controller.B2SSetCanPlay 0
			End If
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
			end If
			BallInPlayReel.SetValue(0)
			CanPlayReel.SetValue(0)
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			LightsOut
			BumpersOff
			PlasticsOff
			checkmatch
			CheckHighScore
			Players=0
			HighScoreTimer.interval=100
			HighScoreTimer.enabled=True
		Else
			Player=1
			If B2SOn Then
				Controller.B2SSetPlayerUp Player
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetData 80+Player,1
				Controller.B2SSetBallInPlay BallInPlay

			End If
			PlaySound("RotateThruPlayers")
			TempPlayerUp=Player
			PlayerUpRotator.enabled=true
			PlayStartBall.enabled=true
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
				PlayerHuds(Player-1).SetValue(1)
				PlayerHUDScores(Player-1).state=1
				PlayerScores(Player-1).visible=0
				PlayerScoresOn(Player-1).visible=1
			end If

			ResetBalls
		End If
	Else 
		If B2SOn Then
			Controller.B2SSetPlayerUp Player
			Controller.B2SSetData 81,0
			Controller.B2SSetData 82,0
			Controller.B2SSetData 83,0
			Controller.B2SSetData 84,0
			Controller.B2SSetData 80+Player,1
			Controller.B2SSetBallInPlay BallInPlay
		End If
		PlaySound("RotateThruPlayers")
		TempPlayerUp=Player
'		PlayerUpRotator.enabled=true
'		PlayStartBall.enabled=true
		For each obj in PlayerHuds
			obj.SetValue(0)
		next
		For each obj in PlayerHUDScores
				obj.state=0
			next
		If Table1.ShowDT = True then
			For each obj in PlayerScores
					obj.visible=1
			Next
			For each obj in PlayerScoresOn
					obj.visible=0
			Next
			PlayerHuds(Player-1).SetValue(1)
			PlayerHUDScores(Player-1).state=1
			PlayerScores(Player-1).visible=0
			PlayerScoresOn(Player-1).visible=1
		end If
		ResetBalls
	End If

End sub

sub CheckHighScore
	Dim playertops
		dim si
	dim sj
	dim stemp
	dim stempplayers
	for i=1 to 4
		sortscores(i)=0
		sortplayers(i)=0
	next
	playertops=0
	for i = 1 to Players
		sortscores(i)=Score(i)
		sortplayers(i)=i
	next

	for si = 1 to Players
		for sj = 1 to Players-1
			if sortscores(sj)>sortscores(sj+1) then
				stemp=sortscores(sj+1)
				stempplayers=sortplayers(sj+1)
				sortscores(sj+1)=sortscores(sj)
				sortplayers(sj+1)=sortplayers(sj)
				sortscores(sj)=stemp
				sortplayers(sj)=stempplayers
			end if
		next
	next
	ScoreChecker=4
	CheckAllScores=1
	NewHighScore sortscores(ScoreChecker),sortplayers(ScoreChecker)
	savehs
end sub


sub checkmatch
	Dim tempmatch
	tempmatch=Int(Rnd*10)
	Match=tempmatch*10
	MatchReel.SetValue(tempmatch+1)
	
	If B2SOn Then
		If Match = 0 Then
			Controller.B2SSetMatch 100
		Else
			Controller.B2SSetMatch Match
		End If
	End if
	for i = 1 to Players
		if Match=(Score(i) mod 100) then
			AddSpecial
		end if
	next
end sub

Sub TiltTimer_Timer()
	if TiltCount > 0 then TiltCount = TiltCount - 1
	if TiltCount = 0 then
		TiltTimer.Enabled = False
	end if
end sub

Sub TiltIt()
		TiltCount = TiltCount + 1
		if TiltCount = 3 then
			TableTilted=True
			PlasticsOff
			BumpersOff
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			TiltReel.SetValue(1)
			If B2Son then
				Controller.B2SSetTilt 1
			end if
		else
			TiltTimer.Interval = 500
			TiltTimer.Enabled = True
		end if
	
end sub

Sub IncreaseBonus()

	
	If BonusCounter=15 then
		exit sub
	end if
	If BonusCounter<10 then
		Bonus(BonusCounter).state=0
	end if
	if BonusCounter>10 then
		Bonus(BonusCounter-10).state=0
	end if
	BonusCounter=BonusCounter+1

	if BonusCounter>10 then
		Bonus(10).State=1
		Bonus(BonusCounter-10).state=1
	else
		Bonus(BonusCounter).State=1
	end if
	
	LeftOutlaneLight.state=0
	RightOutlaneLight.state=0
	if BonusMultiplier=2 then
		DoubleBonus.state=1
	end if

	if (BonusSpecialThreshold=0) then
		if (BonusCounter=8) or (BonusCounter=12) then
			LeftOutlaneLight.state=1
			RightOutlaneLight.state=1
		end if
	else
		if (BonusCounter=10) or (BonusCounter=15) then
			LeftOutlaneLight.state=1
			RightOutlaneLight.state=1
		end if
	end if
End Sub
	

Sub BonusBoost_Timer()
	IncreaseBonus
	BonusBoosterCounter=BonusBoosterCounter-1
	If BonusBoosterCounter=0 then
		BonusBoost.enabled=false
	end if

end sub

Sub CheckForLightSpecial()

	if (TopLightA.state=0) and (TopLightB.state=0) and (TopLightC.state=0) then
		TopRightTargetLight.State=1
		TopLeftTargetLight.State=1
	end if
end sub

Sub PlayStartBall_timer()

	PlayStartBall.enabled=false
	PlaySound("StartBall2-5")
end sub

Sub PlayerUpRotator_timer()
		If RotatorTemp<5 then
			TempPlayerUp=TempPlayerUp+1
			If TempPlayerUp>4 then
				TempPlayerUp=1
			end if
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
				PlayerHuds(TempPlayerUp-1).SetValue(1)
				PlayerHUDScores(TempPlayerUp-1).state=1
				PlayerScores(TempPlayerUp-1).visible=0
				PlayerScoresOn(TempPlayerUp-1).visible=1
			end If
			If B2SOn Then
				Controller.B2SSetPlayerUp TempPlayerUp
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetData 80+TempPlayerUp,1
			End If

		else
			if B2SOn then
				Controller.B2SSetPlayerUp Player
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetData 80+Player,1
			end if
			PlayerUpRotator.enabled=false
			RotatorTemp=1
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
				PlayerHuds(Player-1).SetValue(1)
				PlayerHUDScores(Player-1).state=1
				PlayerScores(Player-1).visible=0
				PlayerScoresOn(Player-1).visible=1
			end If
		end if
		RotatorTemp=RotatorTemp+1

	
end sub


sub savehs
	' Based on Black's Highscore routines
	Dim FileObj
	Dim ScoreFile
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & HSFileName,True)
		ScoreFile.WriteLine 0
		ScoreFile.WriteLine Credits
		scorefile.writeline BallsPerGame
		scorefile.writeline BonusSpecialThreshold
		scorefile.writeline ReplayLevel

		for xx=1 to 5
			scorefile.writeline HSScore(xx)
		next
		for xx=1 to 5
			scorefile.writeline HSName(xx)
		next

		ScoreFile.Close
	Set ScoreFile=Nothing
	Set FileObj=Nothing
end sub

sub loadhs
    ' Based on Black's Highscore routines
	Dim FileObj
	Dim ScoreFile
    dim temp1
    dim temp2
	dim temp3
	dim temp4
	dim temp5
	dim temp6
	dim temp7
	dim temp8
	dim temp9
	dim temp10
	dim temp11
	dim temp12
	dim temp13
	dim temp14
	dim temp15

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & HSFileName) then
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & HSFileName)
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		temp1=TextStr.ReadLine
		temp2=textstr.readline
		temp3=textstr.readline
		temp4=textstr.readline
		temp5=textstr.readline
		HighScore=cdbl(temp1)
		if HighScore<1 then
			temp6=textstr.readline
			temp7=textstr.readline
			temp8=textstr.readline
			temp9=textstr.readline
			temp10=textstr.readline
			temp11=textstr.readline
			temp12=textstr.readline
			temp13=textstr.readline
			temp14=textstr.readline
			temp15=textstr.readline
		end if

		TextStr.Close

	    Credits=cdbl(temp2)
		BallsPerGame=cdbl(temp3)
		ReplayLevel=cdbl(temp5)
		BonusSpecialThreshold=cdbl(temp4)


		if HighScore<1 then
			HSScore(1) = int(temp6)
			HSScore(2) = int(temp7)
			HSScore(3) = int(temp8)
			HSScore(4) = int(temp9)
			HSScore(5) = int(temp10)
			
			HSName(1) = temp11
			HSName(2) = temp12
			HSName(3) = temp13
			HSName(4) = temp14
			HSName(5) = temp15
		end if


		Set ScoreFile=Nothing
	    Set FileObj=Nothing
end sub

sub SaveLMEMConfig
	Dim FileObj
	Dim LMConfig
	dim temp1
	dim tempb2s
	tempb2s=0
	if B2SOn=true then
		tempb2s=1
	else
		tempb2s=0
	end if
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set LMConfig=FileObj.CreateTextFile(UserDirectory & LMEMTableConfig,True)
	LMConfig.WriteLine tempb2s
	LMConfig.Close
	Set LMConfig=Nothing
	Set FileObj=Nothing

end Sub

sub LoadLMEMConfig
	Dim FileObj
	Dim LMConfig
	dim tempC
	dim tempb2s

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & LMEMTableConfig) then
		Exit Sub
	End if
	Set LMConfig=FileObj.GetFile(UserDirectory & LMEMTableConfig)
	Set TextStr2=LMConfig.OpenAsTextStream(1,0)
	If (TextStr2.AtEndOfStream=True) then
		Exit Sub
	End if
	tempC=TextStr2.ReadLine
	TextStr2.Close
	tempb2s=cdbl(tempC)
	if tempb2s=0 then
		B2SOn=false
	else
		B2SOn=true
	end if
	Set LMConfig=Nothing
	Set FileObj=Nothing
end sub

sub SaveLMEMConfig2
	If ShadowConfigFile=false then exit sub
	Dim FileObj
	Dim LMConfig2
	dim temp1
	dim temp2
	dim tempBS
	dim tempFS

	if EnableBallShadow=true then
		tempBS=1
	else
		tempBS=0
	end if
	if EnableFlipperShadow=true then
		tempFS=1
	else
		tempFS=0
	end if

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set LMConfig2=FileObj.CreateTextFile(UserDirectory & LMEMShadowConfig,True)
	LMConfig2.WriteLine tempBS
	LMConfig2.WriteLine tempFS
	LMConfig2.Close
	Set LMConfig2=Nothing
	Set FileObj=Nothing

end Sub

sub LoadLMEMConfig2
	If ShadowConfigFile=false then
		EnableBallShadow = ShadowBallOn
		BallShadowUpdate.enabled = ShadowBallOn
		EnableFlipperShadow = ShadowFlippersOn
		FlipperLSh.visible = ShadowFlippersOn
		FlipperRSh.visible = ShadowFlippersOn
		exit sub
	end if
	Dim FileObj
	Dim LMConfig2
	dim tempC
	dim tempD
	dim tempFS
	dim tempBS

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & LMEMShadowConfig) then
		Exit Sub
	End if
	Set LMConfig2=FileObj.GetFile(UserDirectory & LMEMShadowConfig)
	Set TextStr2=LMConfig2.OpenAsTextStream(1,0)
	If (TextStr2.AtEndOfStream=True) then
		Exit Sub
	End if
	tempC=TextStr2.ReadLine
	tempD=TextStr2.Readline
	TextStr2.Close
	tempBS=cdbl(tempC)
	tempFS=cdbl(tempD)
	if tempBS=0 then
		EnableBallShadow=false
		BallShadowUpdate.enabled=false
	else
		EnableBallShadow=true
	end if
	if tempFS=0 then
		EnableFlipperShadow=false
		FlipperLSh.visible=false
		FLipperRSh.visible=false
	else
		EnableFlipperShadow=true
	end if
	Set LMConfig2=Nothing
	Set FileObj=Nothing

end sub

Sub DisplayHighScore


end sub


sub InitPauser5_timer
		If B2SOn Then
			'Controller.B2SSetScore 6,HighScore
		End If
		DisplayHighScore
		CreditsReel.SetValue(Credits)
		InitPauser5.enabled=false
end sub



sub BumpersOff
	Bumper1Light.Visible=false
	Bumper2Light.Visible=false
	Bumper3Light.Visible=false 

	
end sub

sub BumpersOn
	Bumper1Light.Visible=True
	Bumper2Light.Visible=True
	Bumper3Light.Visible=True 

end sub

Sub PlasticsOn
	For each obj in Flashers
		obj.Visible=true
	next

end sub

Sub PlasticsOff
	For each obj in Flashers
		obj.Visible=false
    next    
    If BB=0 Then    
     EndMusic: PlayMusic"0na06.mp3":TargetLeft1.timerinterval=125500:TargetLeft1.timerenabled=1
    End If
end sub

Sub SetupReplayTables

	Replay1Table(1)=55000
	Replay1Table(2)=61000
	Replay1Table(3)=65000
	Replay1Table(4)=72000
	Replay1Table(5)=77000
	Replay1Table(6)=43000
	Replay1Table(7)=45000
	Replay1Table(8)=50000
	Replay1Table(9)=53000
	Replay1Table(10)=57000
	Replay1Table(11)=60000
	Replay1Table(12)=63000
	Replay1Table(13)=66000
	Replay1Table(14)=69000
	Replay1Table(15)=999000

	Replay2Table(1)=69000
	Replay2Table(2)=75000
	Replay2Table(3)=79000
	Replay2Table(4)=86000
	Replay2Table(5)=91000
	Replay2Table(6)=57000
	Replay2Table(7)=59000
	Replay2Table(8)=64000
	Replay2Table(9)=67000
	Replay2Table(10)=71000
	Replay2Table(11)=74000
	Replay2Table(12)=77000
	Replay2Table(13)=80000
	Replay2Table(14)=83000
	Replay2Table(15)=999000

	Replay3Table(1)=77000
	Replay3Table(2)=83000
	Replay3Table(3)=87000
	Replay3Table(4)=94000
	Replay3Table(5)=99000
	Replay3Table(6)=65000
	Replay3Table(7)=67000
	Replay3Table(8)=72000
	Replay3Table(9)=75000
	Replay3Table(10)=79000
	Replay3Table(11)=82000
	Replay3Table(12)=85000
	Replay3Table(13)=88000
	Replay3Table(14)=91000
	Replay3Table(15)=999000

	ReplayTableMax=5

end sub

Sub RefreshReplayCard
	Dim tempst1
	Dim tempst2
	
	tempst1=FormatNumber(BallsPerGame,0)
	tempst2=FormatNumber(ReplayLevel,0)

	ReplayCard.image = "SC" + tempst2
	Replay1=Replay1Table(ReplayLevel)
	Replay2=Replay2Table(ReplayLevel)
	Replay3=Replay3Table(ReplayLevel)
end sub


'****************************************
'  SCORE MOTOR
'****************************************

ScoreMotorTimer.Enabled = 1
ScoreMotorTimer.Interval = 135 '135
AddScoreTimer.Enabled = 1
AddScoreTimer.Interval = 135

Dim queuedscore
Dim MotorMode
Dim MotorPosition

Sub SetMotor(y)
	Select Case ScoreMotorAdjustment
		Case 0:
			queuedscore=queuedscore+y
		Case 1:
			If MotorRunning<>1 And InProgress=true then
				queuedscore=queuedscore+y
			end if
		end Select
end sub

Sub SetMotor2(x)
	If MotorRunning<>1 And InProgress=true then
		MotorRunning=1
		
		Select Case x
			Case 10:
				AddScore(10)
				MotorRunning=0
				BumpersOn
				
			Case 20:
				MotorMode=10
				MotorPosition=2
				BumpersOff
			Case 30:
				MotorMode=10
				MotorPosition=3
				BumpersOff
			Case 40:
				MotorMode=10
				MotorPosition=4
				BumpersOff
			Case 50:
				MotorMode=10
				MotorPosition=5
				BumpersOff
			Case 100:
				AddScore(100)
				MotorRunning=0
				BumpersOn
			Case 200:
				MotorMode=100
				MotorPosition=2
				BumpersOff
			Case 300:
				MotorMode=100
				MotorPosition=3
				BumpersOff
			Case 400:
				MotorMode=100
				MotorPosition=4
				BumpersOff
			Case 500:
				MotorMode=100
				MotorPosition=5
				BumpersOff
			Case 1000:
				AddScore(1000)
				MotorRunning=0
				BumpersOn
			Case 2000:
				MotorMode=1000
				MotorPosition=2
				BumpersOff
			Case 3000:
				MotorMode=1000
				MotorPosition=3
				BumpersOff
			Case 4000:
				MotorMode=1000
				MotorPosition=4
				BumpersOff
			Case 5000:
				MotorMode=1000
				MotorPosition=5
				BumpersOff
		End Select
	End If
End Sub

Sub AddScoreTimer_Timer
	Dim tempscore
	
	
	If MotorRunning<>1 And InProgress=true then
		if queuedscore>=5000 then
			tempscore=5000
			queuedscore=queuedscore-5000
			SetMotor2(5000)
			exit sub
		end if
		if queuedscore>=4000 then
			tempscore=4000
			queuedscore=queuedscore-4000
			SetMotor2(4000)
			exit sub
		end if
				
		if queuedscore>=3000 then
			tempscore=3000
			queuedscore=queuedscore-3000
			SetMotor2(3000)
			exit sub
		end if
			
		if queuedscore>=2000 then
			tempscore=2000
			queuedscore=queuedscore-2000
			SetMotor2(2000)
			exit sub
		end if
			
		if queuedscore>=1000 then
			tempscore=1000
			queuedscore=queuedscore-1000
			SetMotor2(1000)
			exit sub
		end if

		if queuedscore>=500 then
			tempscore=500
			queuedscore=queuedscore-500
			SetMotor2(500)
			exit sub
		end if
		if queuedscore>=400 then
			tempscore=400
			queuedscore=queuedscore-400
			SetMotor2(400)
			exit sub
		end if
		if queuedscore>=300 then
			tempscore=300
			queuedscore=queuedscore-300
			SetMotor2(300)
			exit sub
		end if
		if queuedscore>=200 then
			tempscore=200
			queuedscore=queuedscore-200
			SetMotor2(200)
			exit sub
		end if
		if queuedscore>=100 then
			tempscore=100
			queuedscore=queuedscore-100
			SetMotor2(100)
			exit sub
		end if

		if queuedscore>=50 then
			tempscore=50
			queuedscore=queuedscore-50
			SetMotor2(50)
			exit sub
		end if
		if queuedscore>=40 then
			tempscore=40
			queuedscore=queuedscore-40
			SetMotor2(40)
			exit sub
		end if
		if queuedscore>=30 then
			tempscore=30
			queuedscore=queuedscore-30
			SetMotor2(30)
			exit sub
		end if
		if queuedscore>=20 then
			tempscore=20
			queuedscore=queuedscore-20
			SetMotor2(20)
			exit sub
		end if
		if queuedscore>=10 then
			tempscore=10
			queuedscore=queuedscore-10
			SetMotor2(10)
			exit sub
		end if


	End If


end Sub

Sub ScoreMotorTimer_Timer
	If MotorPosition > 0 Then
		Select Case MotorPosition
			Case 5,4,3,2:
				If MotorMode=1000 Then 
					AddScore(1000)
				end if
				if MotorMode=100 then
					AddScore(100)
				End If
				if MotorMode=10 then
					AddScore(10)
				End if
				MotorPosition=MotorPosition-1
			Case 1:
				If MotorMode=1000 Then 
					AddScore(1000)
				end if
				If MotorMode=100 then 
					AddScore(100)
				End If
				if MotorMode=10 then
					AddScore(10)
				End if
				MotorPosition=0:MotorRunning=0:BumpersOn
		End Select
	End If

End Sub


Sub AddScore(x)
	If TableTilted=true then exit sub
	Select Case ScoreAdditionAdjustment
		Case 0:
			AddScore1(x)
		Case 1:
			AddScore2(x)
	end Select

end sub


Sub AddScore1(x)
'	debugtext.text=score
	Select Case x
		Case 1:
			PlayChime(10)
			Score(Player)=Score(Player)+1
			
		Case 10:
			PlayChime(10)
			Score(Player)=Score(Player)+10
'			debugscore=debugscore+10
			ToggleAlternatingRelay
		Case 100:
			PlayChime(100)
			Score(Player)=Score(Player)+100
'			debugscore=debugscore+100
			
		Case 1000:
			PlayChime(1000)
			Score(Player)=Score(Player)+1000
'			debugscore=debugscore+1000
	End Select
	PlayerScores(Player-1).AddValue(x)
	PlayerScoresOn(Player-1).AddValue(x)
	If ScoreDisplay(Player)<100000 then
		ScoreDisplay(Player)=Score(Player)
	Else
		Score100K(Player)=Int(Score(Player)/100000)
		ScoreDisplay(Player)=Score(Player)-100000
	End If
	if Score(Player)=>100000 then
		If B2SOn Then
			If Player=1 Then
				Controller.B2SSetScoreRolloverPlayer1 Score100K(Player)
			End If
			If Player=2 Then
				Controller.B2SSetScoreRolloverPlayer2 Score100K(Player)
			End If

			If Player=3 Then
				Controller.B2SSetScoreRolloverPlayer3 Score100K(Player)
			End If

			If Player=4 Then
				Controller.B2SSetScoreRolloverPlayer4 Score100K(Player)
			End If
		End If
	End If
	If B2SOn Then
		Controller.B2SSetScorePlayer Player, ScoreDisplay(Player)
	End If
	If Score(Player)>Replay1 and Replay1Paid(Player)=false then
		Replay1Paid(Player)=True
		AddSpecial
	End If
	If Score(Player)>Replay2 and Replay2Paid(Player)=false then
		Replay2Paid(Player)=True
		AddSpecial
	End If
	If Score(Player)>Replay3 and Replay3Paid(Player)=false then
		Replay3Paid(Player)=True
		AddSpecial
	End If
'	ScoreText.text=debugscore
End Sub

Sub AddScore2(x)
	Dim OldScore, NewScore, OldTestScore, NewTestScore
    OldScore = Score(Player)

	Select Case x
        Case 1:
            Score(Player)=Score(Player)+1
		Case 10:
			Score(Player)=Score(Player)+10
		Case 100:
			Score(Player)=Score(Player)+100
		Case 1000:
			Score(Player)=Score(Player)+1000
	End Select
	NewScore = Score(Player)
	if Score(Player)=>100000 then
		If B2SOn Then
			If Player=1 Then
				Controller.B2SSetScoreRolloverPlayer1 1
			End If
			If Player=2 Then
				Controller.B2SSetScoreRolloverPlayer2 1
			End If

			If Player=3 Then
				Controller.B2SSetScoreRolloverPlayer3 1
			End If

			If Player=4 Then
				Controller.B2SSetScoreRolloverPlayer4 1
			End If
		End If
	End If

	OldTestScore = OldScore
	NewTestScore = NewScore
	Do
		if OldTestScore < Replay1 and NewTestScore >= Replay1 then
			AddSpecial()
			NewTestScore = 0
		Elseif OldTestScore < Replay2 and NewTestScore >= Replay2 then
			AddSpecial()
			NewTestScore = 0
		Elseif OldTestScore < Replay3 and NewTestScore >= Replay3 then
			AddSpecial()
			NewTestScore = 0
		End if
		NewTestScore = NewTestScore - 100000
		OldTestScore = OldTestScore - 100000
	Loop While NewTestScore > 0

    OldScore = int(OldScore / 10)	' divide by 10 for games with fixed 0 in 1s position, by 1 for games with real 1s digits
    NewScore = int(NewScore / 10)	' divide by 10 for games with fixed 0 in 1s position, by 1 for games with real 1s digits
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore&", OldScore Mod 10="&OldScore Mod 10 & ", NewScore % 10="&NewScore Mod 10)

    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(10)
		
    end if

    OldScore = int(OldScore / 10)
    NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(100)
		
    end if

    OldScore = int(OldScore / 10)
    NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(1000)
		
    end if

    OldScore = int(OldScore / 10)
    NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(1000)
    end if

	If B2SOn Then
		Controller.B2SSetScorePlayer Player, Score(Player)
	End If
'	EMReel1.SetValue Score(Player)
	PlayerScores(Player-1).AddValue(x)
	PlayerScoresOn(Player-1).AddValue(x)
End Sub



Sub PlayChime(x)
	if ChimesOn=0 then
		Select Case x
			Case 10
				If LastChime10=1 Then
					PlaySound SoundFXDOF("SpinACard_1_10_Point_Bell",141,DOFPulse,DOFChimes)
					LastChime10=0
				Else
					PlaySound SoundFXDOF("SpinACard_1_10_Point_Bell",141,DOFPulse,DOFChimes)
					LastChime10=1
				End If
			Case 100
				If LastChime100=1 Then
					PlaySound SoundFXDOF("SpinACard_1_100_Point_Bell",142,DOFPulse,DOFChimes)
					LastChime100=0
				Else
					PlaySound SoundFXDOF("SpinACard_1_100_Point_Bell",142,DOFPulse,DOFChimes)
					LastChime100=1
				End If
		
		End Select
	else
		Select Case x
			Case 10
				If LastChime10=1 Then
					PlaySound SoundFXDOF("SJ_Chime_10a",141,DOFPulse,DOFChimes)
					LastChime10=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_10b",141,DOFPulse,DOFChimes)
					LastChime10=1
				End If
			Case 100
				If LastChime100=1 Then
					PlaySound SoundFXDOF("SJ_Chime_100a",142,DOFPulse,DOFChimes)
					LastChime100=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_100b",142,DOFPulse,DOFChimes)
					LastChime100=1
				End If
			Case 1000
				If LastChime1000=1 Then
					PlaySound SoundFXDOF("SJ_Chime_1000a",143,DOFPulse,DOFChimes)
					LastChime1000=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_1000b",143,DOFPulse,DOFChimes)
					LastChime1000=1
				End If
		End Select
	end if
End Sub


Sub HideOptions()

end sub

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
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingSoundTimer_Timer()
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub



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
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'*****************************************
'	Object sounds
'*****************************************

Sub Plastics_Hit (idx)
	PlaySound "woodhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

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
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
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

' ============================================================================================
' GNMOD - Multiple High Score Display and Collection
' ============================================================================================
Dim EnteringInitials		' Normally zero, set to non-zero to enter initials
EnteringInitials = 0

Dim PlungerPulled
PlungerPulled = 0

Dim SelectedChar			' character under the "cursor" when entering initials

Dim HSTimerCount			' Pass counter for HS timer, scores are cycled by the timer
HSTimerCount = 5			' Timer is initially enabled, it'll wrap from 5 to 1 when it's displayed

Dim InitialString			' the string holding the player's initials as they're entered

Dim AlphaString				' A-Z, 0-9, space (_) and backspace (<)
Dim AlphaStringPos			' pointer to AlphaString, move forward and backward with flipper keys
AlphaString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_<"

Dim HSNewHigh				' The new score to be recorded

Dim HSScore(5)				' High Scores read in from config file
Dim HSName(5)				' High Score Initials read in from config file

' default high scores, remove this when the scores are available from the config file
HSScore(1) = 75000
HSScore(2) = 70000
HSScore(3) = 60000
HSScore(4) = 55000
HSScore(5) = 50000

HSName(1) = "AAA"
HSName(2) = "ZZZ"
HSName(3) = "XXX"
HSName(4) = "ABC"
HSName(5) = "BBB"

Sub HighScoreTimer_Timer
	
	if EnteringInitials then
		if HSTimerCount = 1 then
			SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
			HSTimerCount = 2
		else
			SetHSLine 3, InitialString
			HSTimerCount = 1
		end if
	elseif InProgress then
		SetHSLine 1, "HIGH SCORE1"
		SetHSLine 2, HSScore(1)
		SetHSLine 3, HSName(1)
		HSTimerCount = 5	' set so the highest score will show after the game is over
		HighScoreTimer.enabled=false
	elseif CheckAllScores then
		NewHighScore sortscores(ScoreChecker),sortplayers(ScoreChecker)

	else
		' cycle through high scores
		HighScoreTimer.interval=2000
		HSTimerCount = HSTimerCount + 1
		if HsTimerCount > 5 then
			HSTimerCount = 1
		End If
		SetHSLine 1, "HIGH SCORE"+FormatNumber(HSTimerCount,0)
		SetHSLine 2, HSScore(HSTimerCount)
		SetHSLine 3, HSName(HSTimerCount)
	end if
End Sub

Function GetHSChar(String, Index)
	dim ThisChar
	dim FileName
	ThisChar = Mid(String, Index, 1)
	FileName = "PostIt"
	if ThisChar = " " or ThisChar = "" then
		FileName = FileName & "BL"
	elseif ThisChar = "<" then
		FileName = FileName & "LT"
	elseif ThisChar = "_" then
		FileName = FileName & "SP"
	else
		FileName = FileName & ThisChar
	End If
	GetHSChar = FileName
End Function

Sub SetHsLine(LineNo, String)
	dim Letter
	dim ThisDigit
	dim ThisChar
	dim StrLen
	dim LetterLine
	dim Index
	dim StartHSArray
	dim EndHSArray
	dim LetterName
	dim xfor
	StartHSArray=array(0,1,12,22)
	EndHSArray=array(0,11,21,31)
	StrLen = len(string)
	Index = 1

	for xfor = StartHSArray(LineNo) to EndHSArray(LineNo)
		Eval("HS"&xfor).image = GetHSChar(String, Index)
		Index = Index + 1
	next

End Sub

Sub NewHighScore(NewScore, PlayNum)
	if NewScore > HSScore(5) then
		HighScoreTimer.interval = 500
		HSTimerCount = 1
		AlphaStringPos = 1		' start with first character "A"
		EnteringInitials = 1	' intercept the control keys while entering initials
		InitialString = ""		' initials entered so far, initialize to empty
		SetHSLine 1, "PLAYER "+FormatNumber(PlayNum,0)
		SetHSLine 2, "ENTER NAME"
		SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
		HSNewHigh = NewScore
		For xx=1 to HighScoreReward
			AddSpecial
		next
	End if
	ScoreChecker=ScoreChecker-1
	if ScoreChecker=0 then
		CheckAllScores=0
	end if
End Sub

Sub CollectInitials(keycode)
	If keycode = LeftFlipperKey Then
		' back up to previous character
		AlphaStringPos = AlphaStringPos - 1
		if AlphaStringPos < 1 then
			AlphaStringPos = len(AlphaString)		' handle wrap from beginning to end
			if InitialString = "" then
				' Skip the backspace if there are no characters to backspace over
				AlphaStringPos = AlphaStringPos - 1
			End if
		end if
		SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
		PlaySound "DropTargetDropped"
	elseif keycode = RightFlipperKey Then
		' advance to next character
		AlphaStringPos = AlphaStringPos + 1
		if AlphaStringPos > len(AlphaString) or (AlphaStringPos = len(AlphaString) and InitialString = "") then
			' Skip the backspace if there are no characters to backspace over
			AlphaStringPos = 1
		end if
		SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
		PlaySound "DropTargetDropped"
	elseif keycode = StartGameKey or keycode = PlungerKey Then
		SelectedChar = MID(AlphaString, AlphaStringPos, 1)
		if SelectedChar = "_" then
			InitialString = InitialString & " "
			PlaySound("Ding10")
		elseif SelectedChar = "<" then
			InitialString = MID(InitialString, 1, len(InitialString) - 1)
			if len(InitialString) = 0 then
				' If there are no more characters to back over, don't leave the < displayed
				AlphaStringPos = 1
			end if
			PlaySound("Ding100")
		else
			InitialString = InitialString & SelectedChar
			PlaySound("Ding10")
		end if
		if len(InitialString) < 3 then
			SetHSLine 3, InitialString & SelectedChar
		End If
	End If
	if len(InitialString) = 3 then
		' save the score
		for i = 5 to 1 step -1
			if i = 1 or (HSNewHigh > HSScore(i) and HSNewHigh <= HSScore(i - 1)) then
				' Replace the score at this location
				if i < 5 then
' MsgBox("Moving " & i & " to " & (i + 1))
					HSScore(i + 1) = HSScore(i)
					HSName(i + 1) = HSName(i)
				end if
' MsgBox("Saving initials " & InitialString & " to position " & i)
				EnteringInitials = 0
				HSScore(i) = HSNewHigh
				HSName(i) = InitialString
				HSTimerCount = 5
				HighScoreTimer_Timer
				HighScoreTimer.interval = 2000
				PlaySound("Ding1000")
				exit sub
			elseif i < 5 then
				' move the score in this slot down by 1, it's been exceeded by the new score
' MsgBox("Moving " & i & " to " & (i + 1))
				HSScore(i + 1) = HSScore(i)
				HSName(i + 1) = HSName(i)
			end if
		next
	End If

End Sub
' END GNMOD
' ============================================================================================
' GNMOD - New Options menu
' ============================================================================================
Dim EnteringOptions
Dim CurrentOption
Dim OptionCHS
Dim MaxOption
Dim OptionHighScorePosition
Dim XOpt
Dim StartingArray
Dim EndingArray

StartingArray=Array(0,1,2,30,33,61,89,117,145,173,201,229)
EndingArray=Array(0,1,29,32,60,88,116,144,172,200,228,256)
EnteringOptions = 0
MaxOption = 9
OptionCHS = 0
OptionHighScorePosition = 0
Const OptionLinesToMark="111100011"
Const OptionLine1="" 'do not use this line
Const OptionLine2="" 'do not use this line
Const OptionLine3="" 'do not use this line
Const OptionLine4="Special lights Bonus Values "
Const OptionLine5=""
Const OptionLine6=""
Const OptionLine7=""
Const OptionLine8="" 'do not use this line
Const OptionLine9="" 'do not use this line

Sub OperatorMenuTimer_Timer
	OperatorMenuBackdrop.image = "OperatorMenu"
	EnteringOptions = 1
	OptionCHS = 0
	CurrentOption = 1
	DisplayAllOptions
	OperatorOption1.image = "BluePlus"
	SetHighScoreOption

End Sub

Sub DisplayAllOptions
	dim linecounter
	dim tempstring
	For linecounter = 1 to MaxOption
		tempstring=Eval("OptionLine"&linecounter)
		Select Case linecounter
			Case 1:
				tempstring=tempstring + FormatNumber(BallsPerGame,0)
				SetOptLine 1,tempstring
			Case 2:
				if Replay3Table(ReplayLevel)=999000 then
					tempstring = tempstring + FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0)
				else
					tempstring = tempstring + FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0) + "/" + FormatNumber(Replay3Table(ReplayLevel),0)
				end if
				SetOptLine 2,tempstring
			Case 3:
				If OptionCHS=0 then
					tempstring = "NO"
				else
					tempstring = "YES"
				end if
				SetOptLine 3,tempstring
			Case 4:
				SetOptLine 4, tempstring
				if BonusSpecialThreshold=0 then
					tempstring = "8,000/12,000"
				else
					tempstring = "10,000/15,000"
				end if
				SetOptLine 5, tempstring
			Case 5:
				SetOptLine 6, tempstring
				SetOptLine 7, tempstring

			Case 6:
				SetOptLine 8, tempstring
				SetOptLine 9, tempstring
				
			Case 7:
				SetOptLine 10, tempstring
				SetOptLine 11, tempstring
			
			Case 8:
		
			Case 9:
			
	
		End Select
		
	next
end sub

sub MoveArrow
	do 
		CurrentOption = CurrentOption + 1
		If CurrentOption>Len(OptionLinesToMark) then
			CurrentOption=1
		end if
	loop until Mid(OptionLinesToMark,CurrentOption,1)="1"
end sub

sub CollectOptions(ByVal keycode)
	if Keycode = LeftFlipperKey then
		PlaySound "DropTargetDropped"
		For XOpt = 1 to MaxOption
			Eval("OperatorOption"&XOpt).image = "PostitBL"
		next
		MoveArrow
		if CurrentOption<8 then
			Eval("OperatorOption"&CurrentOption).image = "BluePlus"
		elseif CurrentOption=8 then
			Eval("OperatorOption"&CurrentOption).image = "GreenCheck"
		else
			Eval("OperatorOption"&CurrentOption).image = "RedX"
		end if
			
	elseif Keycode = RightFlipperKey then
		PlaySound "DropTargetDropped"
		if CurrentOption = 1 then
			If BallsPerGame = 3 then
				BallsPerGame = 5
			else
				BallsPerGame = 3
			end if
			DisplayAllOptions
		elseif CurrentOption = 2 then
			ReplayLevel=ReplayLevel+1
			If ReplayLevel>ReplayTableMax then
				ReplayLevel=1
			end if
			DisplayAllOptions
		elseif CurrentOption = 3 then
			if OptionCHS = 0 then
				OptionCHS = 1
				
			else
				OptionCHS = 0
				
			end if
			DisplayAllOptions
		elseif CurrentOption = 4 then
			if BonusSpecialThreshold=1 then
				BonusSpecialThreshold=0
			else
				BonusSpecialThreshold=1
			end if
			DisplayAllOptions
		elseif CurrentOption = 8 or CurrentOption = 9 then
				if OptionCHS=1 then
					HSScore(1) = 75000	
					HSScore(2) = 70000
					HSScore(3) = 60000
					HSScore(4) = 55000
					HSScore(5) = 50000

					HSName(1) = "AAA"
					HSName(2) = "ZZZ"
					HSName(3) = "XXX"
					HSName(4) = "ABC"
					HSName(5) = "BBB"
				end if
	
				if CurrentOption = 8 then
					savehs
				else
					loadhs
				end if
				OperatorMenuBackdrop.image = "PostitBL"
				For XOpt = 1 to MaxOption
					Eval("OperatorOption"&XOpt).image = "PostitBL"
				next
			
				For XOpt = 1 to 256
					Eval("Option"&XOpt).image = "PostItBL"
				next
				RefreshReplayCard
				InstructCard.image="IC_"+FormatNumber(BallsPerGame,0)
				EnteringOptions = 0

		end if
	end if
End Sub

Sub SetHighScoreOption
	
End Sub

Function GetOptChar(String, Index)
	dim ThisChar
	dim FileName
	ThisChar = Mid(String, Index, 1)
	FileName = "PostIt"
	if ThisChar = " " or ThisChar = "" then
		FileName = FileName & "BL"
	elseif ThisChar = "<" then
		FileName = FileName & "LT"
	elseif ThisChar = "_" then
		FileName = FileName & "SP"
	elseif ThisChar = "/" then
		FileName = FileName & "SL"
	elseif ThisChar = "," then
		FileName = FileName & "CM"
	else
		FileName = FileName & ThisChar
	End If
	GetOptChar = FileName
End Function

Sub SetOptLine(LineNo, String)
	dim xfor
	dim Letter
	dim ThisDigit
	dim ThisChar
	dim StrLen
	dim LetterLine
	dim Index
	dim LetterName
	StrLen = len(string)
	Index = 1

	for xfor = StartingArray(LineNo) to EndingArray(LineNo)
		Eval("Option"&xfor).image = GetOptChar(string, Index)
		Index = Index + 1
	next


End Sub
