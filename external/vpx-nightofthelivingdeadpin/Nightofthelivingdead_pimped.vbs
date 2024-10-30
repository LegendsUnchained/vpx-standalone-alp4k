'*        Shannon1 Pinvention's Nightofthelivingdead built on
'*        Gottlieb's Jet Spin (1977)
'*        Table build/scripted by Loserman!
'*        Artwork redrawn by GNance
'*        Sounds and lighting effects from NOTLD 68 by HiRez and Xenoph
'*

option explicit
Randomize
ExecuteGlobal GetTextFile("core.vbs")

On Error Resume Next
ExecuteGlobal GetTextFile("Controller.vbs")
If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the scripts folder."
On Error Goto 0

Const cGameName = "SuperSpin_1977"

Const ShadowFlippersOn = true
Const ShadowBallOn = true

Const ShadowConfigFile = false

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


Dim Controller	' B2S
Dim B2SScore	' B2S Score Displayed
Const HSFileName="NightOfTheLivingDead_77VPX.txt"
Const B2STableName="NightOfTheLivingDead_1977"
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
Dim B2SOn
Dim TextStr,TextStr2
Dim i
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
Dim debugscore

Dim IncreaseBonusValue
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

dim GreenTargetsDownCounter
dim BlueTargetsDownCounter
dim YellowTargetsDownCounter

Dim bump1
Dim bump2
Dim bump3

Dim LastChime10
Dim LastChime100
Dim LastChime1000

Dim Score10
Dim Score100

Dim LStep, LStep2, RStep, xx

dim tempbumper

Dim MotorRunning

Dim Replay1Table(15)
Dim Replay2Table(15)
Dim Replay3Table(15)
Dim ReplayTableSet
Dim ReplayLevel
Dim ReplayTableMax
Dim RotoReplay1
Dim RotoReplay2
Dim RotoReplay3
Dim RotoReplay4
Dim ABCDReplay

Dim KickerCounter


Dim RotoLeftWheel(15)       ' Actual Roto Wheel Number array
Dim RotoCenterWheel(15)       ' Actual Roto Wheel Number array
Dim RotoRightWheel(15)       ' Actual Roto Wheel Number array
Dim RotoPos             ' Index to Left Showing RotoWheel Number
Dim RWAnim				' Wheel Animation Phase
Dim RotoCycles			' Number of Digits To Animate on a Spin

Dim StopRotoSpinning,RotoMotion,RotoPosition,RotoTable
RotoTable = array (500,400,300,200,100,999,400,300,200,100,500,400,300,200,100,500,400,300)


Dim X

Dim mHole

Dim ExtraBallFrequency
Dim ScoreMotorCounter

Dim ZenithCounter, ZenithCounter2

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
	ReplayLevel=1
	BallsPerGame=5
	ExtraBallFrequency=1
	EMReel1.ResetToZero
	HighScore=0
	MotorRunning=0
	HighScoreReward=1
	BallTextBox.text=""
	MatchTextBox.text=""
	Credits=0
	IncreaseBonusValue=0
	loadhs
	if HighScore=0 then HighScore=50000


	RotoPosition=Int(rnd*14)
	Roto2.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition),0)
	Roto4.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+1),0)
	Roto6.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+2),0)
	Roto1.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition),0)
	Roto3.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+1),0)
	Roto5.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+2),0)
	Roto7.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+3),0)
	Roto1.visible=False
	Roto2.visible=True
	Roto3.visible=False
	Roto4.visible=True
	Roto5.visible=False
	Roto6.visible=True
	Roto7.visible=False

   
	
	TableTilted=false
	TiltReel.SetValue(1)
	Match=int(Rnd*10)*10
	MatchReel.SetValue((Match/10)+1)
	GameOverReel.SetValue(1)
	CanPlayReel.SetValue(0)
	BallInPlayReel.SetValue(0)

	For each obj in PlayerHuds
		obj.SetValue(0)
	next
	For each obj in PlayerScores
		obj.ResetToZero
	next
	For each obj in Player100K
		obj.ResetToZero
	next

	Replay1=Replay1Table(ReplayLevel)
	Replay2=Replay2Table(ReplayLevel)
	Replay3=Replay3Table(ReplayLevel)

	BonusCounter=0
	HoleCounter=0
    bgpos=6
	kgpos=0
	RotoReplay1=false
	RotoReplay2=false
	RotoReplay3=false
	RotoReplay4=false
	ABCDReplay=false
	For each obj in StartLightsOff
		obj.state=0
	next
	For each obj in StartLightsOn
		obj.state=0
	next
	for each obj in Bonus
		obj.state=0
	next

 	dooralreadyopen=0
	kgdooralreadyopen=0
	
	Bumper2Light.visible=0
	
	Bumper1Light.state=0
	Bumper2Light.visible=0
	Bumper2Light.state=0
	layer3caps.image="layer3_off"
	layer4caps.image="layer4_off"


	InstructCard.image="IC"+FormatNumber(BallsPerGame,0)
	
	RefreshReplayCard
'	ResetBalls
'	PlaySound "Plunger"

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


		if Match=0 then
			Controller.B2SSetMatch 100
		else
			Controller.B2SSetMatch Match
		end if
		Controller.B2SSetScoreRolloverPlayer1 0

		Controller.B2SSetScore 6,HighScore
		Controller.B2SSetTilt 0
		Controller.B2SSetCredits Credits
		Controller.B2SSetGameOver 1
	End If
	EMReel10.SetValue(Credits)
	for i=1 to 1
    player=i
		If B2SOn Then 
			Controller.B2SSetScorePlayer player, 0
		End If
	next
	bump1=1
	bump2=1
	bump3=1
	InitPauser5.enabled=true
	If Credits > 0 Then DOF 123, DOFOn
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
		PlaySound SoundFXDOF("FlipperUp",101,DOFOn,DOFFlippers)
		PlaySound "buzzL",-1
	End If
    
	If keycode = RightFlipperKey  and InProgress=true and TableTilted=false Then
		RightFlipper.RotateToEnd
		PlaySound "buzz",-1
		PlaySound SoundFXDOF("FlipperUp",102,DOFOn,DOFFlippers)
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
		If Credits < 1 Then DOF 123, DOFOff
		EMReel10.SetValue(Credits)
		
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

	if keycode=StartGameKey and Credits>0 and InProgress=false and Players=0 and EnteringOptions = 0 then
'GNMOD
		OperatorMenuTimer.Enabled = false
'END GNMOD
		Credits=Credits-1
		If Credits < 1 Then DOF 123, DOFOff
		EMReel10.SetValue(Credits)
		Players=1
		CanPlayReel.SetValue(Players)
		MatchReel.SetValue(0)
		GameOverReel.SetValue(0)
		Player=1
		playsound "StartUpSequence":TargetCenter1.timerenabled=0:TriggerTopA.timerenabled=0:Bumper2.timerenabled=0:Wall49.timerenabled=0:Wall45.timerenabled=0:Wall46.timerenabled=0:BB=1:EndMusic:StopSounds:PlaySound"0nc06":Bumper2.timerinterval=9500:Bumper2.timerenabled=1:Trigger2.timerenabled=0:TargetCenter2.timerenabled=0
		TempPlayerUp=Player
'		PlayerUpRotator.enabled=true
		rst=0
		BallInPlay=1
		InProgress=true
		resettimer.enabled=true
		BonusMultiplier=1
		If B2SOn Then
			Controller.B2SSetTilt 0
			Controller.B2SSetGameOver 0
			Controller.B2SSetMatch 0
			Controller.B2SSetCredits Credits
			Controller.B2SSetScore 6,HighScore
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
			Controller.B2SSetScoreRolloverPlayer3 0
			Controller.B2SSetScoreRolloverPlayer4 0
			Controller.B2SSetScorePlayer1 0
			Controller.B2SSetScorePlayer2 0
			Controller.B2SSetScorePlayer3 0
			Controller.B2SSetScorePlayer4 0
		End If
		For each obj in PlayerScores
'			obj.ResetToZero
		next
		For each obj in PlayerHuds
			obj.SetValue(0)
		next
		For each obj in Player100K
			obj.ResetToZero
		next
		If Table1.ShowDT = True then
			For each obj in PlayerScores
'				obj.ResetToZero
				obj.Visible=true
			next
			For each obj in PlayerScoresOn
'				obj.ResetToZero
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
		PlaySound SoundFXDOF("FlipperDown",101,DOFOff,DOFFlippers)
		StopSound "buzzL"
	End If
    
	If keycode = RightFlipperKey and InProgress=true and TableTilted=false Then
		RightFlipper.RotateToStart
 		StopSound "buzz"    
		PlaySound SoundFXDOF("FlipperDown",102,DOFOff,DOFFlippers)
	End If

End Sub


Sub Drain_Hit()
	Drain.DestroyBall
	PlaySound "fx_drain":EndMusic:StopUpSounds:StopSounds
	DOF 115, 2
	ZenithCounter=0
	ZenithCounter2=0
	ZenithTimer.enabled=true
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

Sub CloseGateTrigger_hit
	DOF 116, DOFPulse
End Sub

Sub Pause4Bonustimer_timer
	Pause4Bonustimer.enabled=0
	AddBonus
	
	
End Sub

Sub ZenithTimer_timer
	If ZenithCounter>24 then
		ZenithTimer.enabled=false
	else
		ZenithCounter=ZenithCounter+1
		ZenithCounter2=ZenithCounter2+1
		Select case (ZenithCounter2 mod 8)
			case 0:
				Ramp001.image="mummyL1"
			case 1:
				Ramp001.image="mummyL2"
			case 2:
				Ramp001.image="mummyL3"
			case 3:
				Ramp001.image="mummyL4"
			case 4:
				Ramp001.image="mummyR4"
			case 5:
				Ramp001.image="mummyR3"
			case 6:
				Ramp001.image="mummyR2"
			case 7:
				Ramp001.image="mummyR1"
		end select
	end if
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

'***********************
'     Flipper Logos
'***********************

Sub UpdateFlipperLogos_Timer
	LFlip.RotZ = LeftFlipper.CurrentAngle
	RFlip.RotZ = RightFlipper.CurrentAngle
	PGate.Rotz = (Gate.CurrentAngle*.75) + 25
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
End Sub

'**********************************************************

 '***********************
' slingshots
'

Sub RightSlingShot_Slingshot
	PlaySound SoundFXDOF("right_slingshot",104,DOFPulse,DOFContactors),0,1,-0.05,0.05
    RSling0.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -12
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
	PlaySound SoundFXDOF("left_slingshot",103,DOFPulse,DOFContactors),0,1,-0.05,0.05
    LSling0.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -12
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

Sub RubberWall4_Hit()
	If TableTilted=false then
		AddScore(10)
	end if
end sub

Sub RubberWall18_Hit()
	If TableTilted=false then
		AddScore(10)
	end if
end sub

Sub RubberWall2_Hit()
	If TableTilted=false then
		AddScore(10)
	end if
end sub

'***********************************


Sub Bumper1_Hit
	If TableTilted=false then

		PlaySound SoundFXDOF("bumper1",105,DOFPulse,DOFContactors)
		bump1 = 1
		if BallsPerGame=5 then
			AddScore(100)
		else
			AddScore(1000)
		end if

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

		PlaySound SoundFXDOF("bumper1",106,DOFPulse,DOFContactors)
		bump2 = 1
		if BallsPerGame=5 then
			AddScore(100)
		else
			AddScore(1000)
		end if
		
    end if
    
End Sub

Sub Bumper2_Timer
    BB=0:PlayMusic"0np01.mp3":Wall47.timerinterval=135000:Wall47.timerenabled=1
Bumper2.timerenabled=0
End Sub


'***********************************



Sub TriggerTopA_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 107, DOFPulse
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
		
		If UpperALight.state=1 then
			RotoSpin
			IncreaseBonus
			SetMotor(3000)
		else
			SetMotor(500)
		End if
	end if
End Sub

Sub TriggerTopA_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:PlayMusic"0na08.mp3":Wall46.timerinterval=91500:Wall46.timerenabled=1:Trigger2.timerinterval=56500:Trigger2.timerenabled=1
    Case 1:PlayMusic"0na11.mp3":Wall46.timerinterval=14600:Wall46.timerenabled=1
    End Select
TriggerTopA.timerenabled=0
End Sub


Sub TriggerTopB_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 108, DOFPulse
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
		
		If UpperBLight.state=1 then
			RotoSpin
			IncreaseBonus
			SetMotor(3000)
		else
			SetMotor(500)
		End if
	end if
End Sub

Sub TriggerTopC_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 109, DOFPulse
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
		
		If UpperCLight.state=1 then
			RotoSpin
			IncreaseBonus
			SetMotor(3000)
		else
			SetMotor(500)
		End if

	end if
End Sub

Sub TriggerLeftInlane_Hit()
	If TableTilted=false then
		PlaySound "sensor":PlaySound"Onc07"
		DOF 111, DOFPulse
		If Bonus001.state=1 then
			SetMotor(1000)
		else
			AddScore(100)
		end if
		RotoSpin
		IncreaseBonus

	End If
End Sub


Sub TriggerRightInlane_Hit()
	If TableTilted=false then
		PlaySound "sensor":PlaySound"Onc07"
		DOF 112, DOFPulse
		If Bonus002.state=1 then
			SetMotor(1000)
		else
			AddScore(100)
		end if
		RotoSpin
		IncreaseBonus
	End If

End Sub

Sub TriggerLeftOutlane_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 110, DOFPulse
        Dim x
         x = INT(5 * RND(1) )
         Select Case x
         Case 0:PlaySound"0nc01"
         Case 1:PlaySound"0nc02"
         Case 2:PlaySound"0nc03"
         Case 3:PlaySound"0nc04"
         Case 4:PlaySound"0nc05"
        End Select
		IncreaseBonusValue=3
		If Bonus004.state=1 then
			SetMotor(3000)
		else
			SetMotor(500)
		end if
	end If
End Sub

Sub TriggerRightOutlane_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 113, DOFPulse
        Dim x
         x = INT(5 * RND(1) )
         Select Case x
         Case 0:PlaySound"0nc01"
         Case 1:PlaySound"0nc02"
         Case 2:PlaySound"0nc03"
         Case 3:PlaySound"0nc04"
         Case 4:PlaySound"0nc05"
        End Select
		IncreaseBonusValue=3
		If Bonus003.state=1 then
			SetMotor(3000)
		else
			SetMotor(500)
		end if

	End If
End Sub


'****************************************************

sub Kicker1_Hit()
	Dim speedx,speedy,finalspeed
	speedx=activeball.velx
	speedy=activeball.vely
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	if TableTilted=true then
		Kicker1.Timerenabled=1
		exit sub
	end if
	if finalspeed>20 then
		Kicker1.Kick 0,0
		activeball.velx=speedx
		activeball.vely=speedy
	else

		KickerHolder1.enabled=1
	end if

end sub

sub KickerHolder1_timer
	if MotorRunning=0 then
		KickerHolder1.enabled=0
		Kicker1Process
	end if
end sub

sub Kicker1Process

	KickerCounter=1
	X=RotoTable(RotoPosition+1)
	if X=999 then
		X=500
	end if
	X=X*10
	SetMotor(X)
	Kicker1.timerinterval=(500)
	Kicker1.timerenabled=1
	
end sub


Sub Kicker1_Timer()
	Kicker1.timerenabled=0
'	RotoSpin
	Kicker1.kick 240,10
	DOF 120, DOFPulse
	DOF 121, DOFPulse
	Pkickarm1.rotz=15
	KickerArm1.enabled=true
end sub

sub KickerArm1_timer
	Pkickarm1.rotz=0
	KickerArm1.enabled=false
end sub
	
'****************************************************
Sub RotoSpin()
    X=INT(RND()*10)						' get random Number from 0-9
    If X < 4 Then
       
       RotoCycles=10
     Else
       If X < 8 Then
          
          RotoCycles=11
        Else
         
          RotoCycles=12
        End If
    End if

    If RWAnim <> 0 Then
       Exit Sub							' Animation Already Underway
    End If
	RotoMotion=0
	StopRotoSpinning=0
	NewRotoTimer.enabled=true
    StopSound "BuzzLeft"                ' In Case Quick Hit
    PlaySound "RotoStart"				' Sound "Kick" Solenoid 
	DOF 124, DOFPulse
End Sub
'
'  RotoWheel Spin Animation Timer moves Through the Animation Stages
 '  for "RotoCycle" Digits Spinning by windows, and
'  then Raises the newly Selected Wheel Position Digits
'
Sub NewRotoTimer_timer
	If RotoMotion=1 then
		RotoPosition=RotoPosition+1
		if RotoPosition>14 then RotoPosition=0
		RotoMotion=0
		RotoCycles=RotoCycles-1
		If RotoCycles<0 then StopRotoSpinning=1
	Else
		RotoMotion=1
	end If


	if RotoMotion=1 Then

		Roto1.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition),0)
		Roto3.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+1),0)
		Roto5.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+2),0)
		Roto7.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+3),0)
		Roto1.visible=True
		Roto2.visible=False
		Roto3.visible=True
		Roto4.visible=False
		Roto5.visible=True
		Roto6.visible=False
		Roto7.visible=True
		Roto2.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition),0)
		Roto4.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+1),0)
		Roto6.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+2),0)
	Else
		Roto2.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition),0)
		Roto4.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+1),0)
		Roto6.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+2),0)
		Roto1.visible=False
		Roto2.visible=True
		Roto3.visible=False
		Roto4.visible=True
		Roto5.visible=False
		Roto6.visible=True
		Roto7.visible=False
		Roto1.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition),0)
		Roto3.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+1),0)
		Roto5.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+2),0)
		Roto7.image = "target-roto-t1-"+FormatNumber(RotoTable(RotoPosition+3),0)
		if StopRotoSpinning=1 Then
			StopRotoSpinning=0
			NewRotoTimer.enabled=False
		end If
	end if


end Sub



'****************************************************
Sub RotoNumLeft_Hit()
    PlaySound"Rifle"
    PlaySound SoundFXDOF("Target",117,DOFPulse,DOFTargets)
 '
'  Spot Left Roto Number
 '
	X = RotoTable(RotoPosition)       			
	if X=999 then
		X=500
	end if
	SetMotor(x)
	LeftRotoHitLight.state=1
	RotoHitLights.enabled=true

End Sub
'
'
' Right Roto Number Hit
'
Sub RotoNumRight_Hit()
    PlaySound"Rifle"
    PlaySound SoundFXDOF("Target",117,DOFPulse,DOFTargets)

	X = RotoTable(RotoPosition+2)       			
	if X=999 then
		X=500
	end if
	SetMotor(x)
	RightRotoHitLight.state=1
	RotoHitLights.enabled=true


End Sub

Sub RotoNumCenter_Hit()
    PlaySound"Good"
    PlaySound SoundFXDOF("Target",117,DOFPulse,DOFTargets)

	X = RotoTable(RotoPosition+1)       			
	if X=999 then
		X=500
		If CenterRotoSpecialLight.state=1 then
			AddSpecial
		end if
	end if
	IncreaseBonusValue= x/100
	SetMotor(x*10)
	CenterRotoHitLight.state=1
	RotoHitLights.enabled=true


End Sub
'
' End Right Num hit Animation
'

Sub RotoHitLights_Timer()
	LeftRotoHitLight.state=0
	RightRotoHitLight.state=0
	CenterRotoHitLight.state=0
	RotoHitLights.enabled=false
end sub


'****************************************************

Sub TargetCenter1_Hit()
	DOF 118, DOFPulse
    StopSound "0nt11":PlaySound"0nt11"
	If TableTilted=false then
		If LeftTargetSpecialLight.state=0 then
			IncreaseBonusValue=3
			SetMotor(3000)
			
		else
			SetMotor(5000)
			AddSpecial
			LeftTargetSpecialLight.state=0
		End if
		TargetCenter1.timerenabled=1
	End If
end Sub	

Sub TargetCenter1_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:PlayMusic"0na02.mp3":TargetCenter2.timerinterval=21500:TargetCenter2.timerenabled=1
    Case 1:PlayMusic"0na10.mp3":TargetCenter2.timerinterval=20000:TargetCenter2.timerenabled=1
    End Select
TargetCenter1.timerenabled=0
End Sub

Sub TargetCenter2_Hit()
	DOF 119, DOFPulse
    StopSound "0nt08":PlaySound"0nt08"
	If TableTilted=false then
		X=RotoTable(RotoPosition+2)
		if X=999 then
			X=500
		end if
		SetMotor(X)
		If RightTargetExtraBallLight.state=1 then
			SamePlayerShootsAgain.state=1
			RightTargetExtraBallLight.state=0
		End if
		TargetCenter2.timerenabled=1
	End If
end Sub	

Sub TargetCenter2_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:PlayMusic"0na03.mp3":TargetCenter3.timerinterval=27300:TargetCenter3.timerenabled=1
    Case 1:PlayMusic"0na07.mp3":TargetCenter3.timerinterval=125200:TargetCenter3.timerenabled=1
    End Select
TargetCenter2.timerenabled=0
End Sub

Sub TargetCenter3_Hit()
	DOF 118, DOFPulse
    StopSound "0nc08"
	PlaySound "0nc08"
	If TableTilted=false then
		X=RotoTable(RotoPosition)
		if X=999 then
			X=500
		end if
		SetMotor(X)
		If LeftTargetExtraBallLight.state=1 then
			SamePlayerShootsAgain.state=1
			LeftTargetExtraBallLight.state=0
		End if
		TargetCenter3.timerenabled=1
	End If
end Sub	

Sub TargetCenter3_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:PlayMusic"0na02.mp3":TargetCenter3.timerinterval=27300:TargetCenter3.timerenabled=1
    Case 1:PlayMusic"0na10.mp3":TargetCenter3.timerinterval=125200:TargetCenter3.timerenabled=1
    End Select
TargetCenter3.timerenabled=0
End Sub

'****************************************************
' Varitarget

Dim Lvariawarded, Lnewpos, Loldpos
Dim Rvariawarded, Rnewpos, Roldpos
Dim LeftPosition_Vari
Dim RightPosition_Vari
Dim Varimultiplier

Lvariawarded = 0
Lnewpos=0
Loldpos=0


Sub LeftVT_Hit(idx)
	DOF 126, DOFPulse
	Dim x
	Lnewpos=idx
	If ActiveBall.VelY <0 Then
		ActiveBall.VelY = ActiveBall.VelY * 0.89
		PlaySound "fx_solenoidoff"
		'LeftVari(Loldpos).IsDropped = True
		'LeftVari(Lnewpos).IsDropped = False
		VariTargetR.TransX = -(LeftPosition_Vari*10)
		LeftPosition_Vari = idx +1
		
	End If
	Loldpos=Lnewpos
End Sub

Sub LeftVT_UnHit(idx)
	If ActiveBall.VelY >0 Then
		TimerLeftVariTarget.Enabled = True
	End If
End Sub

Sub vt22_Hit() 
	DOF 125, DOFPulse
	Dim x
	Lvariawarded = 1
	IncreaseBonusValue=5
	If BallsPerGame=5 then
		SetMotor(500)
	else
		SetMotor(5000)
	end if
	LeftTargetSpecialLight.state=1
	Bonus005.state=0
	Bonus001.state=1
	Bonus002.state=1
	Bonus003.state=1
	Bonus004.state=1
End Sub


Sub TimerLeftVariTarget_Timer()
'        LeftVari(LeftPosition_Vari).IsDropped = True
        LeftPosition_Vari = LeftPosition_Vari - 1
		If LeftPosition_Vari <0 Then
			LeftPosition_Vari = 0
		End If
		
'        LeftVari(LeftPosition_Vari).IsDropped = False
		VariTargetR.TransX = -(LeftPosition_Vari*10)
		If LeftPosition_Vari = 17 AND Lvariawarded = 0 Then 
			IncreaseBonusValue=5
			Bonus001.state=1
			Bonus002.state=1
			Bonus003.state=1
			Bonus004.state=1
			if BallsPerGame=5 then
				SetMotor(500)
			else
				SetMotor(5000)
			end if
			Lvariawarded = 1
		End If
		If LeftPosition_Vari = 14 AND Lvariawarded = 0 Then 
			IncreaseBonusValue=4
			Bonus001.state=1
			Bonus002.state=1
			if BallsPerGame=5 then
				SetMotor(400)
			else
				SetMotor(4000)
			end if
			
			Lvariawarded = 1

		End If
		If LeftPosition_Vari = 11 AND Lvariawarded = 0 Then 
			IncreaseBonusValue=3
			Bonus001.state=1
			Bonus002.state=1
			if BallsPerGame=5 then
				SetMotor(300)
			else
				SetMotor(3000)
			end if
			
			Lvariawarded = 1

		End If
		If LeftPosition_Vari = 7 AND Lvariawarded = 0 Then 
			IncreaseBonusValue=2
			if BallsPerGame=5 then
				SetMotor(200)
			else
				SetMotor(2000)
			end if
			
			Lvariawarded = 1

		End If
		If LeftPosition_Vari = 4 AND Lvariawarded = 0 Then 
			if BallsPerGame=5 then
				AddScore(100)
			else
				AddScore(1000)
			end if
			Lvariawarded = 1
			IncreaseBonus
		End If
        If LeftPosition_Vari = 0 Then
		  Lvariawarded = 0
		  RotoSpin
        TimerLeftVariTarget.Enabled = False
    End If
End Sub

'****************************************************

Sub AddSpecial()
	PlaySound SoundFXDOF("knocker",122,DOFPulse,DOFKnocker)
	DOF 121, DOFPulse
	Credits=Credits+1
	DOF 123, DOFOn
	if Credits>15 then Credits=9
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	EMReel10.SetValue(Credits)
End Sub

Sub AddSpecial2()
	PlaySound"click"
	Credits=Credits+1
	DOF 123, DOFOn
	if Credits>15 then Credits=9
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	EMReel10.SetValue(Credits)
End Sub

Sub AddBonus()
	bonuscountdown=bonuscounter
	ScoreMotorCounter=0
	ScoreBonus.enabled=true
End Sub


Sub ToggleOutlane
	if LeftOutlaneLight.state=1 then
		LeftOutlaneLight.state=0
		RightOutlaneLight.state=1
	Else
		LeftOutlaneLight.state=1
		RightOutlaneLight.state=0
	end if
end sub

Sub ToggleBumper
'	if Bumper1.state=1 then
'		Bumper1.state=0
'		Bumper1L.image="1000WhenLit-Unlit"
'		Bumper1R.State = ABS(Bumper1R.State - 1) 'refresh light
'	else
'		Bumper1.state=1
'		Bumper1L.image="1000WhenLit-Lit"
'		Bumper1R.State = ABS(Bumper1R.State - 1) 'refresh light
'	end if
	If TargetSpecialLit=1 then
		if UpperBlueLight.state=1 then
			UpperBlueLight.state=0
			UpperGreenLight.state=1
		else
			UpperBlueLight.state=1
			UpperGreenLight.state=0
		end if
	end if
end sub


Sub ResetBallDrops()



	BonusCounter=0
	HoleCounter=0
End Sub


Sub LightsOut
	BonusCounter=0
	HoleCounter=0
	Bumper1Light.visible=0
	layer3caps.image="layer3_off"
	layer4caps.image="layer4_off"
	Bumper1Light.state=0
	Bumper2Light.visible=0
	Bumper2Light.state=0
	For each obj in StartLightsOff
		obj.state=0
	next
	For each obj in StartLightsOn
		obj.state=0
	next
	StopSound "buzzL"
	StopSound "buzz"
end sub

Sub ResetDrops()

	for each obj in AllTargets
		obj.IsDropped=0
	next
	PlaySound"dropsup"
	BlueTargetsDownCounter=0
	YellowTargetsDownCounter=0
	GreenTargetsDownCounter=0

End Sub

Sub ResetBalls()

	TempMultiCounter=BallsPerGame-BallInPlay

	For each obj in StartLightsOff
		obj.state=0
	next
	For each obj in StartLightsOn
		obj.state=1
	next
	Bonus005.state=1
	Bonus001.state=0
	Bonus002.state=0
	Bonus003.state=0
	Bonus004.state=0
	ResetBallDrops
	BonusMultiplier=1

	TableTilted=false
	TiltReel.SetValue(0)
	If B2Son then
		Controller.B2SSetTilt 0
	end if

	PlasticsOn
	BumpersOn
	RotoSpin
	'CreateBallID BallRelease
	Ballrelease.CreateSizedBall 25
    Ballrelease.Kick 40,7 
	DOF 114, DOFPulse
	BallInPlayReel.SetValue(BallInPlay)

End Sub

sub delaykgclose_timer
	delaykgclose.enabled=false
	closekg.enabled=true

end sub

sub resettimer_timer
    rst=rst+1
	If rst=1 then
		PlayerUpRotator.enabled=true
	end if
	if rst=7 then
		PlayerUpRotator.enabled=true
	end if

	if rst>10 and rst<21 then
		ResetReelsToZero(1)
	end if
	if rst>20 and rst<31 then
		ResetReelsToZero(2)
	end if
  
    if rst=32 then
    playsound "StartBall1"
    end if
    if rst=35 then
    newgame
    resettimer.enabled=false
    end if
end sub

Sub ResetReelsToZero(reelzeroflag)
	dim d1(5)
	dim d2(5)
	dim scorestring1, scorestring2

	If reelzeroflag=1 then
		scorestring1=CStr(Score(1))
		scorestring1=right("00000" & scorestring1,5)
		for i=0 to 4
			d1(i)=CInt(mid(scorestring1,i+1,1))
		next
		for i=0 to 4
			if d1(i)>0 then 
				d1(i)=d1(i)+1
				if d1(i)>9 then d1(i)=0
			end if

		next
		Score(1)=(d1(0)*10000) + (d1(1)*1000) + (d1(2)*100) + (d1(3)*10) + d1(4)
		If B2SOn Then
			Controller.B2SSetScorePlayer 1, Score(1)
		End If
		PlayerScores(0).SetValue(Score(1))
		PlayerScoresOn(0).SetValue(Score(1))

	end if
	If reelzeroflag=2 then
		scorestring1=CStr(Score(2))
		scorestring1=right("00000" & scorestring1,5)
		for i=0 to 4
			d1(i)=CInt(mid(scorestring1,i+1,1))
		next
		for i=0 to 4
			if d1(i)>0 then 
				d1(i)=d1(i)+1
				if d1(i)>9 then d1(i)=0
			end if

		next
		Score(2)=(d1(0)*10000) + (d1(1)*1000) + (d1(2)*100) + (d1(3)*10) + d1(4)
		If B2SOn Then
			Controller.B2SSetScorePlayer 2, Score(2)
		End If
		PlayerScores(1).SetValue(Score(2))
		PlayerScoresOn(1).SetValue(Score(2))

	end if

end sub


sub ScoreBonus_timer

	if bonuscountdown<=0 then
		ScoreBonus.enabled=false
		'ScoreBonus.interval=600
		NextBallDelay.enabled=true
		exit sub
	end if

	Select case ScoreMotorCounter
		case 0,1,2,3,4:
			AddScore(1000)
			If bonuscountdown>10 then
				'Bonus(bonuscountdown-10).state=0
			else
				'Bonus(bonuscountdown).state=0
						
			end if
			bonuscountdown=bonuscountdown-1
			if bonuscountdown>=0 then
				If bonuscountdown<=10 then
					'Bonus(bonuscountdown).state=1
				elseif bonuscountdown>10 then
					'Bonus(10).state=1
					'Bonus(bonuscountdown-10).state=1
				end if
			end if
					
		case 5:

	end select
	ScoreMotorCounter=ScoreMotorCounter+1
	If ScoreMotorCounter>5 then
		ScoreMotorCounter=0
	end if
end sub

sub NextBallDelay_timer()
	NextBallDelay.enabled=false
	nextball

end sub

sub newgame
	InProgress=true
	debugscore=0
	queuedscore=0
	EMReel1.ResetToZero
	MatchTextBox.text=""
	BallTextBox.text=FormatNumber(BallInPlay,0)
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
		Controller.B2SSetScorePlayer1 0
		Controller.B2SSetScorePlayer2 0
		Controller.B2SSetScorePlayer3 0
		Controller.B2SSetScorePlayer4 0
		Controller.B2SSetBallInPlay BallInPlay
	End if

	RotoReplay1=false
	RotoReplay2=false
	RotoReplay3=false
	RotoReplay4=false
	ABCDReplay=false
	For each obj in StartLightsOff
		obj.state=0
	next
	For each obj in StartLightsOn
		obj.state=1
	next

'	Bumper1L.image="Bumper100Lit"
	Bumper1Light.visible=1
	Bumper1Light.state=1
	layer3caps.image="layer3"
	layer4caps.image="layer4"
'	Bumper2L.image="Bumper100Lit"
	Bumper2Light.visible=1
	Bumper2Light.state=1

	Bonus001.state=0
	Bonus002.state=0
	Bonus003.state=0
	Bonus004.state=0
	Bonus006.state=1
	ResetBalls
End sub

sub nextball
	If B2SOn Then
		Controller.B2SSetTilt 0
	End If
	If SamePlayerShootsAgain.state=0 then
		Player=Player+1
	else
		SamePlayerShootsAgain.state=0
	end if
	If Player>Players Then
		BallInPlay=BallInPlay+1
		If BallInPlay>BallsPerGame then
			PlaySound("MotorLeer")
			InProgress=false
			
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
			GameOverReel.SetValue(1)
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			LightsOut
			PlasticsOff
			BallTextBox.text=""
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
	if Match=0 then
		MatchTextBox.text="Match: 00"
	else
		MatchTextBox.text="Match: " + FormatNumber(Match,0)
	end if

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
			TiltReel.SetValue(1)
			PlasticsOff
			BumpersOff
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
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
		'Bonus(BonusCounter).state=0
	end if
	if BonusCounter>10 then
		'Bonus(BonusCounter-10).state=0
	end if
	BonusCounter=BonusCounter+1
	
	if BonusCounter>10 then
		'Bonus(10).State=1
		'Bonus(BonusCounter-10).state=1
	else
		'Bonus(BonusCounter).State=1
	end if
	
	RightTargetExtraBallLight.state=0
	LeftTargetExtraBallLight.state=0
	CenterRotoSpecialLight.state=0
	if BonusCounter=4 and ExtraBallFrequency=1 then
		RightTargetExtraBallLight.state=1
	end if
	if BonusCounter=6 then
		LeftTargetExtraBallLight.state=1
	end if
	if BonusCounter=8 then
		RightTargetExtraBallLight.state=1
	end if
	if BonusCounter=10 and BallsPerGame=3 then
		LeftTargetExtraBallLight.state=1
	end if
	if BonusCounter=15 then
		CenterRotoSpecialLight.state=1
	end if
	if UpperBLight.state=1 then
		UpperBLight.state=0
		UpperALight.state=1
		UpperCLight.state=1
	else
		UpperBLight.state=1
		UpperALight.state=0
		UpperCLight.state=0
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
	PlaySound("ballrel")
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
				
				PlayerScores(Player-1).visible=0
				PlayerScoresOn(Player-1).visible=1
			end If
		end if
		RotatorTemp=RotatorTemp+1

	
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
		scorefile.writeline ExtraBallFrequency
		ScoreFile.WriteLine ReplayLevel
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
		temp5=textstr.readline
		temp4=textstr.readline
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
		if HighScore<1 then
			Credits=cdbl(temp2)
			BallsPerGame=cdbl(temp3)	
			ExtraBallFrequency=cdbl(temp5)
			ReplayLevel=cdbl(temp4)
		end if

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

Sub DisplayHighScore
	

end sub

sub InitPauser5_timer
		If B2SOn Then
			Controller.B2SSetScore 6,HighScore
		End If
'		EMReel5.SetValue(HighScore)
		DisplayHighScore
		InitPauser5.enabled=false
end sub

sub ResetDropsTimer_timer
		ResetDropsTimer.enabled=0
		ResetDrops
end sub

sub BumpersOff
	Bumper1Light.visible=0
	Bumper2Light.visible=0
	layer3caps.image="layer3_off"
	layer4caps.image="layer4_off"
end sub

sub BumpersOn
	if Bumper1Light.state=1 then
'		Bumper1L.image="Bumper100Lit"
		Bumper1Light.visible=1
	layer3caps.image="layer3"
	layer4caps.image="layer4"
	end if
	if Bumper2Light.state=1 then
'		Bumper2L.image="Bumper100Lit"
		Bumper2Light.visible=1
	layer3caps.image="layer3"
	layer4caps.image="layer4"
	end if
end sub

Sub PlasticsOn

	For each obj in Flashers
		obj.visible=1
	next
	playfield_off.visible=0
	layer1.image="layer1"
	layer2.image="layer2"
	lsling0.image="layer2"
	rsling0.image="layer2"
	layer3.image="layer3"
	layer3a.image="layer3"
	layer4.image="layer4"

end sub

Sub PlasticsOff

	For each obj in Flashers
		obj.visible=0
	next
	StopSound "buzzL"
	StopSound "buzz"
	playfield_off.visible=1
	layer1.image="layer1_off"
	layer2.image="layer2_off"
	lsling0.image="layer2_off"
	rsling0.image="layer2_off"
	layer3.image="layer3_off"
	layer3a.image="layer3_off"
	layer4.image="layer4_off"
end sub

Sub SetupReplayTables

	Replay1Table(1)=70000
	Replay1Table(2)=80000
	Replay1Table(3)=80000
	Replay1Table(4)=90000
	Replay1Table(5)=90000
	Replay1Table(6)=100000
	Replay1Table(7)=100000
	Replay1Table(8)=110000
	Replay1Table(9)=110000
	Replay1Table(10)=120000
	Replay1Table(11)=120000
	Replay1Table(12)=999000
	Replay1Table(13)=999000
	Replay1Table(14)=999000
	Replay1Table(15)=999000

	Replay2Table(1)=100000
	Replay2Table(2)=100000
	Replay2Table(3)=110000
	Replay2Table(4)=110000
	Replay2Table(5)=120000
	Replay2Table(6)=120000
	Replay2Table(7)=130000
	Replay2Table(8)=130000
	Replay2Table(9)=140000
	Replay2Table(10)=140000
	Replay2Table(11)=150000
	Replay2Table(12)=999000
	Replay2Table(13)=999000
	Replay2Table(14)=999000
	Replay2Table(15)=999000

	Replay3Table(1)=180000
	Replay3Table(2)=180000
	Replay3Table(3)=180000
	Replay3Table(4)=180000
	Replay3Table(5)=180000
	Replay3Table(6)=180000
	Replay3Table(7)=190000
	Replay3Table(8)=190000
	Replay3Table(9)=190000
	Replay3Table(10)=190000
	Replay3Table(11)=999000
	Replay3Table(12)=999000
	Replay3Table(13)=999000
	Replay3Table(14)=999000
	Replay3Table(15)=999000

	ReplayTableMax=11

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
	If IncreaseBonusValue>0 then
		IncreaseBonus
		IncreaseBonusValue=IncreaseBonusValue-1
	End if
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
			PlayChime(100)
			Score(Player)=Score(Player)+10
'			debugscore=debugscore+10
			ToggleBumper
		Case 100:
			PlayChime(1000)
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
		if OldTestScore < Replay1 and NewTestScore >= Replay1 and Replay1Paid(Player)=false then
			AddSpecial()
			Replay1Paid(Player)=True
			NewTestScore = 0
		Elseif OldTestScore < Replay2 and NewTestScore >= Replay2 and Replay2Paid(Player)=false then
			AddSpecial()
			Replay2Paid(Player)=True
			NewTestScore = 0
		Elseif OldTestScore < Replay3 and NewTestScore >= Replay3 and Replay3Paid(Player)=false then
			AddSpecial()
			Replay3Paid(Player)=True
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
		ToggleBumper
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
					PlaySound SoundFXDOF("SpinACard_100_Point_Bell",142,DOFPulse,DOFChimes)
					LastChime100=0
				Else
					PlaySound SoundFXDOF("SpinACard_100_Point_Bell",142,DOFPulse,DOFChimes)
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
Const OptionLine4="Extra Ball Light Frequency"
Const OptionLine5=""
Const OptionLine6=""
Const OptionLine7=""
Const OptionLine8="" 'do not use this line
Const OptionLine9="" 'do not use this line

Sub OperatorMenuTimer_Timer
	EnteringOptions = 1
	OperatorMenuTimer.enabled=false
	ShowOperatorMenu
end sub

sub ShowOperatorMenu
	OperatorMenuBackdrop.image = "OperatorMenu"

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
				if ExtraBallFrequency=0 then
					tempstring = "Conservative"
				else
					tempstring = "Liberal"
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
			ExtraBallFrequency=ExtraBallFrequency+1
			if ExtraBallFrequency>1 then 
				ExtraBallFrequency=0
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
				InstructCard.image="IC"+FormatNumber(BallsPerGame,0)
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

dim LineLengths(22)	' maximum number of lines
Sub SetOptLine(LineNo, String)
	Dim DispLen
    Dim StrLen
	dim xfor
	dim Letter
	dim ThisDigit
	dim ThisChar
	dim LetterLine
	dim Index
	dim LetterName
	StrLen = len(string)
	Index = 1

	StrLen = len(String)
    DispLen = StrLen
    if (DispLen < LineLengths(LineNo)) Then
        DispLen = LineLengths(LineNo)
    end If

	for xfor = StartingArray(LineNo) to StartingArray(LineNo) + DispLen
		Eval("Option"&xfor).image = GetOptChar(string, Index)
		Index = Index + 1
	next
	LineLengths(LineNo) = StrLen

End Sub


