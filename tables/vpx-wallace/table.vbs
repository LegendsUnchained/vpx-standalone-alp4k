'	--------------------------------------------
'	WALLACE AND GROMIT
'	--------------------------------------------
'	Visual Pinball table ED-70
'	An original VP table by Dave Sanders
'	Design, script, table graphics and art 
'	by Dave Sanders, March 2006
'	--------------------------------------------

Option Explicit  'Force explicit variable declaration.
Randomize
Dim UseFlexDMD, UseFlexDMDBackgroundImage, FlexDMDCalloutStlye
'	--------------------------------------------
'	USER OPTIONS
'	--------------------------------------------
UseFlexDMD = 1						' 0 = Off (use desktop or B2S scoring)
									' 1 = On  (but no desktop or B2S scoring)
									' 2 = On  (flexmd with desktop or b2s scoring still active)
UseFlexDMDBackgroundImage = 1		' 0 = Off (black dmd background)
									' 1 = On  (gromit style wallpaper background)
FlexDMDCalloutStlye = 0				' 0 = Callouts overlayed on score
									' 1 = Callouts appear over credit / ball data (in smaller text), pup friendly

'	--------------------------------------------



On Error Resume Next
ExecuteGlobal GetTextFile("Controller.vbs")
If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the scripts folder."
On Error Goto 0
Dim Controller	' B2S
Dim B2SScore	' B2S Score Displayed

if Table1.showdt = False Then Ramp006.visible = 0:Ramp007.visible = 0

Dim WandGLight(3)
Dim ToolLight(3)
Dim VegLight(5)
Dim HoleLight(8)
Dim SpotLight(9)
Dim MultLight(5)
Dim RoundLight(7)
Dim NickParkLight(4)
Dim ContraptionLight(6)
Dim OtherLight(9)

Set WandGLight(1)=Light01
Set WandGLight(2)=Light02
Set WandGLight(3)=Light03

Set ToolLight(1)=Light04
Set ToolLight(2)=Light05
Set ToolLight(3)=Light06

Set VegLight(1)=Light07
Set VegLight(2)=Light08
Set VegLight(3)=Light09
Set VegLight(4)=Light10
Set VegLight(5)=Light11

Set HoleLight(1)=Light13
Set HoleLight(2)=Light14
Set HoleLight(3)=Light15
Set HoleLight(4)=Light16
Set HoleLight(5)=Light17
Set HoleLight(6)=Light18
Set HoleLight(7)=Light19
Set HoleLight(8)=Light20

Set SpotLight(1)=Light24
Set SpotLight(2)=Light25
Set SpotLight(3)=Light26
Set SpotLight(4)=Light27
Set SpotLight(5)=Light28
Set SpotLight(6)=Light29
Set SpotLight(7)=Light30
Set SpotLight(8)=Light31
Set SpotLight(9)=Light32

Set MultLight(1)=Light33
Set MultLight(2)=Light34
Set MultLight(3)=Light35
Set MultLight(4)=Light36
Set MultLight(5)=Light37

Set RoundLight(1)=Light38
Set RoundLight(2)=Light39
Set RoundLight(3)=Light40
Set RoundLight(4)=Light41
Set RoundLight(5)=Light42
Set RoundLight(6)=Light43
Set RoundLight(7)=Light44

Set NickParkLight(1)=Light49
Set NickParkLight(2)=Light50
Set NickParkLight(3)=Light51
Set NickParkLight(4)=Light52

Set ContraptionLight(1)=Light53
Set ContraptionLight(2)=Light54
Set ContraptionLight(3)=Light55
Set ContraptionLight(4)=Light56
Set ContraptionLight(5)=Light57
Set ContraptionLight(6)=Light58

Set OtherLight(1)=Light12
Set OtherLight(2)=Light21
Set OtherLight(3)=Light22
Set OtherLight(4)=Light23
Set OtherLight(5)=Light45
Set OtherLight(6)=Light46
Set OtherLight(7)=Light48
Set OtherLight(8)=Light49
Set OtherLight(9)=Light59

Dim BumperLight(3)
Dim BumpPause(3)
Dim BumpLit(3)

Dim BedTime
Dim BallsLocked
Dim FlashStatus
Dim PulseStatus
Dim SwitchPause(9)
Dim SpotPause(9)

Dim f,g,h,ff,gg,hh,fff,ggg,hhh

Dim Score
Dim HighScore(4)
Dim LastScore
Dim Credits
Dim GotReplay
Dim ReplayScore
Dim BallsLeft
Dim BallMax
Dim Round(6)
Dim CurrentRound
Dim RoundReady
Dim RoundDone
Dim RoundTime
Dim RoundTotal
Dim ToastTime
Dim ToastVal
Dim MultiBalls
Dim MultiRound
Dim Veg(5)
Dim VegDone
Dim VegHit
Dim Relight(2)
Dim JackpotDone
Dim RocketDone
Dim CheeseReady
Dim CheeseDone
Dim TrainDone
Dim WhichTrain
Dim MoonDone
Dim Sheep(9)
Dim SpotDone(3)
Dim SheepDone
Dim WAndG(4)
Dim WAndGDone
Dim BonusMult
Dim Tool(3)
Dim ToolDone
Dim ContraptionReady
Dim ContraptionDone
Dim WhichContraption
Dim WhichCheese
Dim ContraptionAward
Dim NickPark(5)
Dim NickLaneDone
Dim NickParkDone
Dim SkillTime
Dim SkillVal
Dim MusicBreak
Dim DropPause

Dim TextWipe
Dim TextBlink
Dim TextEvent
Dim TextFeature
Dim AttractCount
Dim DMDCount
Dim InPlunger
Dim BallActive
Dim DemoTime
Dim SPReady
Dim ShootAgain
Dim StartPause
Dim BonusPause
Dim BonusTotal
Dim ScoopPause(2)
Dim MatchNum
Dim TiltSensor
Dim MachineTilt
Dim CreditPause
Dim BallSave
Dim HighActive
Dim LightBlink
Dim LightPulse(7)
Dim ToastHeld
Dim GateTime
Dim SlamTilt
Dim IsCabinet

Randomize

IsCabinet = true
if Table1.ShowDT = true then IsCabinet = false

If IsCabinet = true then
	Set Controller = CreateObject("B2S.Server")
	Controller.Run
	Score1.Visible = false
	Score2.Visible = false
	CreditReel.Visible = false
	BallReel.Visible = false
	InfoBox1.Visible = false
	InfoBox2.Visible = false
End If


Dim b2sChrLookup2(96)
' Predefined characters, A-Z 0-9 and + - * " ' ( ) / < = > [ \ ] _ `
' Predefined characters, A-Z 0-9 and + - * " ' ( ) / < = > [ \ ] _ `
b2sChrLookup2(34)=514:b2sChrLookup2(35)=0:b2sChrLookup2(39)=1024:b2sChrLookup2(40)=5120:b2sChrLookup2(41)=16640:b2sChrLookup2(42)=32576
b2sChrLookup2(43)=10816:b2sChrLookup2(44)=16384:b2sChrLookup2(45)=2112:b2sChrLookup2(47)=17408:b2sChrLookup2(48)=17471
b2sChrLookup2(49)=8704:b2sChrLookup2(50)=2139:b2sChrLookup2(51)=2127:b2sChrLookup2(52)=2150:b2sChrLookup2(53)=2157:b2sChrLookup2(54)=2173
b2sChrLookup2(55)=7:b2sChrLookup2(56)=2175:b2sChrLookup2(57)=2159:b2sChrLookup2(60)=5120:b2sChrLookup2(61)=2120:b2sChrLookup2(62)=16640
b2sChrLookup2(65)=2167:b2sChrLookup2(66)=10767:b2sChrLookup2(67)=57:b2sChrLookup2(68)=8719:b2sChrLookup2(69)=2169:b2sChrLookup2(70)=2161
b2sChrLookup2(71)=2109:b2sChrLookup2(72)=2166:b2sChrLookup2(73)=8713:b2sChrLookup2(74)=30:b2sChrLookup2(75)=5232:b2sChrLookup2(76)=56
b2sChrLookup2(77)=1334:b2sChrLookup2(78)=4406:b2sChrLookup2(79)=63:b2sChrLookup2(80)=2163:b2sChrLookup2(81)=4159:b2sChrLookup2(82)=6259
b2sChrLookup2(83)=2157:b2sChrLookup2(84)=8705:b2sChrLookup2(85)=62:b2sChrLookup2(86)=17456:b2sChrLookup2(87)=20534:b2sChrLookup2(88)=21760
b2sChrLookup2(89)=9472:b2sChrLookup2(90)=17417:b2sChrLookup2(91)=57:b2sChrLookup2(92)=4352:b2sChrLookup2(93)=15:b2sChrLookup2(95)=8:
b2sChrLookup2(96)=256:b2sChrLookup2(46)=4


Sub LED(LEDMessage, num) 'Convert text message to proper LED bars to display on Backglass
		
	Dim i

	If UseFlexDMD <> 0 Then 
		FlexDMD.LockRenderThread
		Dim fmtString 
		fmtString = CentreString(LEDMessage,16)
		With FlexDMDScene
			If FlexDMDCalloutStlye = 0 Then
				Select Case num
				Case 1
					For i = 0 To 15
						.GetImage("Text1_" & i).Bitmap = FlexDMD.NewImage("", "VPX.DMD" & Asc(Mid(fmtString,i + 1, 1))).Bitmap
					Next
				Case 2
					For i = 0 To 15
						.GetImage("Text2_" & i).Bitmap = FlexDMD.NewImage("", "VPX.DMD" & Asc(Mid(fmtString,i + 1, 1))).Bitmap
					Next
				End Select
			Else
				.GetLabel("Text" & num).Text = fmtString
				If fmtString <> Space(16) Then
					Select Case num
					Case 1
						.GetLabel("Credits").Visible = False
					Case 2
						.GetLabel("Ball").Visible = False
					End Select
				Else
					Select Case num
					Case 1
						.GetLabel("Credits").Visible = True
					Case 2
						.GetLabel("Ball").Visible = True
					End Select
				End If
			End If
		End With
		FlexDMD.UnlockRenderThread
		If UseFlexDMD = 1 Then Exit Sub
	End If

	If IsCabinet = True then
		dim LL, stnum
		LL=16' line length
		stnum = 9
		if num = 2 then stnum = 25
		For i = stnum to stnum + LL - 1 ' clear all LEDs
			Controller.B2SSetLED i, 0
		Next
		If Len(LEDMessage) > 0 and len(LEDMessage) <  LL+1 Then ' is there a message to show and is it not too long
			For i = 1 to len(LEDMessage)
				Controller.B2SSetLED stnum + int((ll -  len(LEDMessage))/2) + i - 1, b2sChrLookup2(Asc(UCase(Mid(LEDMessage,i,1)))) ' display each letter and have the the message centered
				Err.Clear
			Next
		End if	
	End If
	if num = 1 then InfoBox1.Text=LEDMessage
	if num = 2 then InfoBox2.Text=LEDMessage
End Sub

Sub CR(loc) 'CreditReel

	If UseFlexDMD <> 0 Then 
		FlexDMD.LockRenderThread
		Select Case loc
		Case 0
			FlexDMDScene.GetLabel("Credits").Text = Space(13)
		Case 1
			FlexDMDScene.GetLabel("Credits").Text = Space(13)
		Case 2
			FlexDMDScene.GetLabel("Credits").Text = " CREDITS 0   "
		Case 3
			FlexDMDScene.GetLabel("Credits").Text = " CREDITS 1   "
		Case 4
			FlexDMDScene.GetLabel("Credits").Text = " CREDITS 2   "
		Case 5
			FlexDMDScene.GetLabel("Credits").Text = " CREDITS 3   "
		Case 6
			FlexDMDScene.GetLabel("Credits").Text = " CREDITS 4   "
		Case 7
			FlexDMDScene.GetLabel("Credits").Text = " CREDITS 5   "
		Case 8
			FlexDMDScene.GetLabel("Credits").Text = " CREDITS 6   "
		Case 9
			FlexDMDScene.GetLabel("Credits").Text = " CREDITS 7   "
		Case 10
			FlexDMDScene.GetLabel("Credits").Text = " CREDITS 8   "
		Case 11
			FlexDMDScene.GetLabel("Credits").Text = " CREDITS 9   "
		Case 12
			FlexDMDScene.GetLabel("Credits").Text = "     HIGHEST "
		Case 13
			FlexDMDScene.GetLabel("Credits").Text = "     DISPLAY "
		Case 14
			FlexDMDScene.GetLabel("Credits").Text = "     DANGER  "
		Case 15
			FlexDMDScene.GetLabel("Credits").Text = "      TILT   "
		End Select
		FlexDMD.UnlockRenderThread
		If UseFlexDMD = 1 Then Exit Sub
	End If


	If IsCabinet = True then
		dim i
		For i = 20 to 34 ' clear all 
			Controller.B2SSetData i,0
		Next
		Controller.B2SSetData loc + 18,1
	End if
	CreditReel.SetValue(loc)
End Sub 

Sub BR(loc) 'BallReel

	If UseFlexDMD <> 0 Then 
		FlexDMD.LockRenderThread
		Select Case loc
		Case 0
			FlexDMDScene.GetLabel("Ball").Text = Space(13)
		Case 1
			FlexDMDScene.GetLabel("Ball").Text = Space(13)
		Case 2
			FlexDMDScene.GetLabel("Ball").Text = "      BALL 1 "
		Case 3
			FlexDMDScene.GetLabel("Ball").Text = "      BALL 2 "
		Case 4
			FlexDMDScene.GetLabel("Ball").Text = "      BALL 3 "
		Case 5
			FlexDMDScene.GetLabel("Ball").Text = "      BALL 4 "
		Case 6
			FlexDMDScene.GetLabel("Ball").Text = "      BALL 5 "
		Case 7
			FlexDMDScene.GetLabel("Ball").Text = " SCORE 1     "
		Case 8
			FlexDMDScene.GetLabel("Ball").Text = " SCORE 2     "
		Case 9
			FlexDMDScene.GetLabel("Ball").Text = " SCORE 3     "
		Case 10
			FlexDMDScene.GetLabel("Ball").Text = " SCORE 4     "
		Case 11
			FlexDMDScene.GetLabel("Ball").Text = " TEST        "
		Case 12
			FlexDMDScene.GetLabel("Ball").Text = " TEST OK     "
		Case 13
			FlexDMDScene.GetLabel("Ball").Text = " GAME OVER   "
		Case 14
			FlexDMDScene.GetLabel("Ball").Text = "SHOOT AGAIN  "
		Case 15
			FlexDMDScene.GetLabel("Ball").Text = "  TILT       "
		Case 16
			FlexDMDScene.GetLabel("Ball").Text = " DANGER      "
		Case 17
			FlexDMDScene.GetLabel("Ball").Text = "   MATCH   "
		End Select
		FlexDMD.UnlockRenderThread
		If UseFlexDMD = 1 Then Exit Sub
	End If

	If IsCabinet = True then
		dim i
		For i = 41 to 56 ' clear all 
			Controller.B2SSetData i,0
		Next
		Controller.B2SSetData loc + 39,1
	End if
	BallReel.SetValue(loc)

End Sub 

Sub SetScore(value)
	If IsCabinet = True then
		Controller.B2SSetScorePlayer1 Score
	End If

End Sub

Sub prt(stuff)  'code to print append debug information values to tables folder as the cgamename .log
   Dim WriteFileObject, WriteFileName
   Set WriteFileObject = CreateObject("Scripting.FileSystemObject")
   Set WriteFileName = WriteFileObject.OpenTextFile("Wallace.log", 8, True )
   WriteFileName.WriteLine(Now & "-" & stuff)
   WriteFileName.close
End Sub



Sub AttractTimer_Timer()
	If AttractCount>1 then
		AttractMode
	End If
	
	If BallActive>0 then
	LightBlink=LightBlink+1
		If LightBlink>4 then LightBlink=1
		If LightBlink=1 or LightBlink=3 then CheckLights
	End If
End Sub

Sub GameTimer_Timer()

	If MachineTilt=0 then
		If SkillTime>0 then
			SkillTime=SkillTime-1
			If SkillTime=0 then CheckLights
		End If
		
		If RoundTime>0 then
			RoundTime=RoundTime-1
			
			If RoundTime=2250 then
				MusicBreak=0
				BallSave=750
				SavePost.IsDropped=False
				PlaySound "Click"
				GateTime=750
				GateOpen
			End If
			
			If RoundTime=500 then PlaySound "V-TimeLow"
			
			If RoundTime=0 then
				RoundReady=0
				TextWipe=50
				RoundDone=RoundDone+1
				Round(CurrentRound)=1
				If CurrentRound=1 then
					TextEvent=18
					PlaySound "V-TWT1End"
				End If
				If CurrentRound=2 then
					TextEvent=21
					PlaySound "V-TWT2End"
				End If
				If CurrentRound=3 then
					If RoundTotal=2000000 then TextWipe=100
					If RoundTotal<2000000 then PlaySound "V-AGDO1Failure"
					TextEvent=24
					For f=1 to 3
						Tool(f)=0
					Next
					ToolDone=0
				End If
				If CurrentRound=4 then
					TextEvent=27
					PlaySound "V-AGDO2End"
				End If
				If CurrentRound=5 then
					TextEvent=30
					PlaySound "V-ACS1End"
				End If
				If CurrentRound=6 then
					TextWipe=49
					If RoundTotal=2200000 then TextWipe=100
					If RoundTotal<2200000 then PlaySound "V-ACS2Failure"
					TextEvent=33
					For f=1 to 9
						Sheep(f)=0
					Next
					SpotDone(1)=0
					SpotDone(2)=0
					SpotDone(3)=0
					SheepDone=0
				End If
				TextFeature=0
				If MusicBreak=1 or (CurrentRound<>1 and CurrentRound<>6 and (CurrentRound<>3 or RoundTotal=2000000)) then
					MusicBreak=0
					If NickParkDone=4 or RoundDone=6 then PlayMusic "W&G-MultiballReady.MP3"
					If (CurrentRound<>3 or RoundTotal=2000000) and (CurrentRound<>6 or RoundTotal=2700000) and NickParkDone<4 and RoundDone<6 then PlayMusic "W&G-MainTheme.MP3"
				End If
				CurrentRound=0
				If RoundDone=6 then CurrentRound=7
				If ToastHeld=1 then
					TextWipe=0
					ToasterDown
					ToastTime=1000
					ToastVal=500000
					PlayMusic "W&G-Toast.MP3"
					ToastHeld=0
				End If
				CheckLights
			End If
		End If
		
		If ToastTime>0 and ToastHeld=0 then
			ToastTime=ToastTime-1
			If ToastTime<1000 and ToastTime/2=Int(ToastTime/2) then ToastVal=ToastVal-1000
			If ToastTime=0 then
				ToasterUp
				PlaySound "V-ToastFailure"
				TextFeature=0
			End If
		End If
		
	End If

	If StartPause>0 then
		StartPause=StartPause-1
		If StartPause=425 then 
			AllLightsOff
			StopAllSound
			BR 0
			PlayMusic "W&G-GameStart.MP3"
		End If
		If StartPause=65 then
			AllLightsOn
			SaveData
		End If
		If StartPause=0 then
			AllLightsOff
			If NickParkDone=4 or RoundDone=6 then PlayMusic "W&G-MultiballReady.MP3"
			If NickParkDone<4 and RoundDone<6 then PlayMusic "W&G-BallWaiting.MP3"
			PlaySound "BallOut"
			InPlunger=1
			If ShootAgain=0 then
				LED "LET'S GET", 1
				LED "CRACKING", 2
			End If
			If ShootAgain>0 then
				ShootAgain=ShootAgain-1
				LED "SHOOT AGAIN", 1
				LED "", 2
				PlaySound "V-ExtraBallStart"
				BR 14
			End If
			BallExit.CreateBall
			BallExit.Kick 90,5
			BallActive=1
			TextWipe=50
			TextEvent=-1
			PrintScore
			CheckLights
			End If	
		End If

		If DropPause>0 then
			DropPause=DropPause-1
			If DropPause=0 then
			Wendolene.IsDropped=False
			PlaySound "Click"
		End If
	End If

	If CreditPause>0 then
		CreditPause=CreditPause-1
		If CreditPause=0 then
			Credits=Credits+1
			If Credits>9 then Credits=9
			PrintCredits
		End If
	End If

	If TiltSensor>0 and BallActive=1 and MachineTilt=0 then 
		TiltSensor=TiltSensor-1
		If TiltSensor=0 then TextWipe=0
		If TiltSensor>50 and TiltSensor<100 then
			If TextEvent<>-3 then
				StopSound "V-Danger"
				PlaySound "V-Danger"
				TextWipe=25
				TextEvent=-3
			End If
		End If
		If BallActive=1 and TextEvent=-3 then
			If TextBlink=1 then
			LED "DANGER", 1
			LED "", 2
			Else
			LED "", 1
			LED "DANGER", 2
			End If
			BR 16
		End If
		If TiltSensor>99 then
			MachineTilt=1
			LED "TILT", 1
			LED "", 2
	'		Bumper1.Disabled=True
	'		Bumper2.Disabled=True
	'		Bumper3.Disabled=True
			LeftSlingshot.Disabled=True
			RightSlingshot.Disabled=True
			TextEvent=0
			TextWipe=9999999
			SkillTime=0
			If RoundTime>0 then
				RoundTime=0
				Round(CurrentRound)=1
				RoundDone=RoundDone+1
					If CurrentRound=6 then
						ToolDone=0
						For f=1 to 3
							Tool(f)=0
						Next
					End If
				If CurrentRound=3 then
					For f=1 to 9
						Sheep(f)=0
					Next
					SpotDone(1)=0
					SpotDone(2)=0
					SpotDone(3)=0
				End If
				CurrentRound=0
			End If
			If MultiRound=1 then
				For f=1 to 5
					NickPark(f)=0
				Next
				NickLaneDone=0
				NickParkDone=0
			End If
			If MultiRound=2 then
				For f=1 to 5
					Veg(f)=0
				Next
				VegDone=0
				VegHit=0
				For f=1 to 9
					Sheep(f)=0
				Next
				SpotDone(1)=0
				SpotDone(2)=0
				SpotDone(3)=0
				RoundReady=0
				CurrentRound=0
				RoundDone=0
				For f=1 to 6
					Round(f)=0
				Next
			End If
			ToastTime=0
			ToastVal=0
			ToasterUp
			InPlunger=0
			MultiRound=0
			BallSave=0
			SavePost.IsDropped=True
			GateTime=0
			GateClose
			If SlamTilt=1 then
				BallsLeft=1
				ShootAgain=0
				LED "SLAM TILT", 1
				LastScore=Score
				Score1.SetValue(0):SetScore(0)
				Score2.SetValue(0)
				CR 0
				BR 0
			End If
			For f=1 to 7
				LightPulse(f)=0
			Next
			AllLightsOff
			StopAllSound
	'		PlayMusic ""
			f=Rnd
			If f>.5 then PlaySound "V-Tilt1"
			If f<=.5 then PlaySound "V-Tilt2"
			MachineTilt=1
		End If
		If MachineTilt=1 and SlamTilt=0 then BR 15
		If MachineTilt=0 and TiltSensor=0 then PrintScore 
	End If

	If ScoopPause(1)>0 then
		ScoopPause(1)=ScoopPause(1)-1
		If ScoopPause(1)=0 then
			LeftScoop.Kick 152+(Rnd*5),15
			PlaySound "Kicker"
		End If
	End If

	If ScoopPause(2)>0 then
		ScoopPause(2)=ScoopPause(2)-1
		If ScoopPause(2)=0 then
			RightScoop.Kick 230+(Rnd*5),15
			PlaySound "Kicker"
		End If
	End If

	If BallSave>0 then
		BallSave=BallSave-1
		If BallSave=0 then
			SavePost.IsDropped=True
			PlaySound "Drop"
			BallSave=-1
		End If
	End If
	
	If GateTime>0 then
		GateTime=GateTime-1
		If GateTime=0 then GateClose
	End If

	If BallActive=0 and DemoTime>0 then
		DemoTime=DemoTime+1
		CheckDemo
	End If

	For f=1 to 9
		If SwitchPause(f)>0 then
			SwitchPause(f)=SwitchPause(f)-1	
		End If
	Next
	
	For f=1 to 9
		If SpotPause(f)>0 then
			SpotPause(f)=SpotPause(f)-1
			If SpotPause(f)=5 then
				PlaySound "Spot"				
			End If
			If SpotPause(f)=0 then				
			End If
		End If
	Next

	If BedTime>0 then
		BedTime=BedTime-1
		If BedTime=450 then
			BallSave=0
			MultiBalls=3
			RoundTotal=0
			If RoundTime>0 then
				RoundTime=0
				Round(CurrentRound)=1
				RoundDone=RoundDone+1
				CurrentRound=0
			End If
			ToastTime=0
			ToastVal=0
			ToasterUp
	'		PlayMusic ""
			If MultiRound=1 then PlaySound "V-MultiballLaunch"
			If MultiRound=2 then
				PlaySound "V-WererabbitLaunch"
				If Wendolene.IsDropped=True then PlaySound "Click"
				Wendolene.IsDropped=False
				For f=1 to 5
					Veg(f)=0
					VegDone=0
				Next
				Relight(1)=0
				Relight(2)=0
			End If
		End If
		If BedTime=400 or BedTime=275 then
			PlaySound "BallOut"
			BallExit.CreateBall
			BallExit.Kick 90,5
		End If
		If BedTime=325 or BedTime=200 then
			PlaySound "Kicker"
			Plunger.Kick 0,250
		End If
		If BedTime=85 and BallsLocked=1 then BedAnim.heighttop = 180
		If BedTime=80 then BedAnim.heighttop = 230
		If BedTime=75 or BedTime=60 or BedTime=45 then
			BedEject.CreateBall
			BedEject.Kick 180,5
			PlaySound "Kicker"
			BallsLocked=BallsLocked-1
			If BallsLocked=0 then BedTime=45
		End If
		
		If BedTime=45 and MachineTilt=0 then 
			If InPlunger=1 then
			SkillTime=250
				If BallsLeft=BallMax and MultiRound=0 then PlaySound "V-AfterLaunch"
			End If
			If BallSave=0 then
				BallSave=750
				GateTime=750
				GateOpen
				SavePost.IsDropped=False
				PlaySound "Click"
			End If
			If MultiRound=1 and BallSave=750 then
				PlaySound "V-MultiballStart"
				PlayMusic "W&G-Multiball.MP3"
				GateOpen
			End If
			If MultiRound=2 and BallSave=750 then
				PlaySound "V-WererabbitStart"
				PlayMusic "W&G-FinalMode.MP3"
				GateOpen
			End If
			If MultiRound=0 and (BallsLeft<BallMax or BallSave<500) then
				h=(Int(Rnd*3))+1
				StopSound "V-Tools1"
				StopSound "V-Tools2"
				If h=1 then PlaySound "V-BedEject1"
				If h=2 then PlaySound "V-BedEject2"
				If h=3 then PlaySound "V-BedEject3"
			End If
			InPlunger=0
			If BallsLocked>0 then BedTime=60
		End If
		If BedTime=5 then BedAnim.heighttop = 180
		If BedTime=0 then BedAnim.heighttop = 131
	End If	
	
	If BallActive=0 then AttractTwo

	If BallActive=2 then EndOfBall
	
End Sub

Sub PlungerLightTrigger_Hit:PlungerLight.state = 2:GIOFF:End Sub
Sub PlungerLightTrigger_UnHit:PlungerLight.state = 0:End Sub

Sub Bumper1_Hit()
	Bumper1L.duration 1, 300, 0
	Bumper1L1.duration 1, 300, 0
'	BumpPause(1)=6
	PlaySound "Jet"	
	If MachineTilt=0 then
		StopSound "V-Bumper1"
		StopSound "V-Bumper2"
		StopSound "V-Bumper3"
		PlaySound "V-Bumper1"
		AddScore(1510)
		If RoundTime>0 and CurrentRound=1 then
			AddScore(98490)
			RoundTotal=RoundTotal+100000
			If RoundTotal/1000000 = Int(RoundTotal/1000000) then PlaySound "V-TWT1Score"
		End If
	End If	
End Sub

Sub Bumper2_Hit()
	Bumper2L.duration 1, 300, 0
	Bumper2L1.duration 1, 300, 0
'	BumpPause(2)=6
	PlaySound "Jet"	
	If MachineTilt=0 then
		StopSound "V-Bumper1"
		StopSound "V-Bumper2"
		StopSound "V-Bumper3"
		PlaySound "V-Bumper2"
		AddScore(1510)
		If RoundTime>0 and CurrentRound=1 then
			AddScore(98490)
			RoundTotal=RoundTotal+100000
			If RoundTotal/1000000 = Int(RoundTotal/1000000) then PlaySound "V-TWT1Score"
		End If
	End If	
End Sub

Sub Bumper3_Hit()
	Bumper3L.duration 1, 300, 0
	Bumper3L1.duration 1, 300, 0
'	BumpPause(3)=6
	PlaySound "Jet"	
	If MachineTilt=0 then
		StopSound "V-Bumper1"
		StopSound "V-Bumper2"
		StopSound "V-Bumper3"
		PlaySound "V-Bumper3"
		AddScore(1510)
		If RoundTime>0 and CurrentRound=1 then
			AddScore(98490)
			RoundTotal=RoundTotal+100000
			If RoundTotal/1000000 = Int(RoundTotal/1000000) then PlaySound "V-TWT1Score"
		End If
	End If	
End Sub

Sub SoundOn_Hit()
	If ActiveBall.VelY<-10 then
		PlaySound "Launch"
		If MachineTilt=0 and InPlunger=1 and MultiRound=0 then PlaySound "V-Launch"
		LED "", 1
		LED "", 2
		If MachineTilt=0 then PrintScore
		If InPlunger=1 and NickParkDone<4 and RoundDone<6 then PlayMusic "W&G-MainTheme.MP3"
	End If
End Sub

Sub Table1_KeyDown(ByVal keycode)

	If keycode = PlungerKey Then
		Plunger.Kick 0,250
	End If

	If keycode = LeftFlipperKey Then
		If BallActive=0 and DemoTime=0 and AttractCount>0 and StartPause=0 then
			AttractCount=0
			DemoTime=1
		End If
		If MachineTilt=0 and BallActive=1 then
			LeftFlipper.RotateToEnd
			Flipper3.RotateToEnd
			PlaySound "FlipperUp"
			NickPark(5)=NickPark(1)
			NickPark(1)=NickPark(2)
			NickPark(2)=NickPark(5)
			NickPark(5)=NickPark(3)
			NickPark(3)=NickPark(4)
			NickPark(4)=NickPark(5)
			WAndG(4)=WAndG(1)
			WAndG(1)=WAndG(2)
			WAndG(2)=WAndG(3)
			WAndG(3)=WAndG(4)
		End If
	End If
    
	If keycode = RightFlipperKey Then
		If BallActive=0 and DemoTime=0 and AttractCount>0 and StartPause=0 then
			AttractCount=0
			DemoTime=1
		End If
		If MachineTilt=0 and BallActive=1 then
			RightFlipper.RotateToEnd
			PlaySound "FlipperUp"
			NickPark(5)=NickPark(1)
			NickPark(1)=NickPark(2)
			NickPark(2)=NickPark(5)
			NickPark(5)=NickPark(3)
			NickPark(3)=NickPark(4)
			NickPark(4)=NickPark(5)
			WAndG(4)=WAndG(3)
			WAndG(3)=WAndG(2)
			WAndG(2)=WAndG(1)
			WAndG(1)=WAndG(4)
			CheckLights
		End If
	End If
    
	If keycode = LeftTiltKey and MachineTilt=0 and BallActive=1 Then
		Nudge 90, 2.5
		TiltSensor=TiltSensor+50
	End If
    
	If keycode = RightTiltKey and MachineTilt=0 and BallActive=1 Then
		Nudge 270, 2.5
		TiltSensor=TiltSensor+50
	End If
    
	If keycode = CenterTiltKey and MachineTilt=0 and BallActive=1 Then
		Nudge 0, 2.5
		TiltSensor=TiltSensor+50
	End If
	
	If KeyCode=10 and BallMax=5 and CreditPause=0 and AttractCount>0 then
		BallMax=3
		LED "THREE-BALL PLAY", 1
		LED "SELECTED", 2
		AttractCount=1351
		DMDCount=751
		ReplayScore=ReplayScore/2
		SaveData
	End If
	
	If KeyCode=11 and BallMax=3 and CreditPause=0 and AttractCount>0 then
		BallMax=5
		LED "FIVE-BALL PLAY", 1
		LED "SELECTED", 2
		AttractCount=1351
		DMDCount=751
		ReplayScore=ReplayScore*2
		SaveData
	End If
	
	If keycode=AddCreditKey and CreditPause=0 and SlamTilt=0 and (BallActive>0 or AttractCount>0 or DemoTime>0) then
		PlaySound "V-CoinIn"
		CreditPause=38
	End If
	
	If keycode=StartGameKey and BallActive=0 and (AttractCount>0 or DemoTime>0) and StartPause=0 then
		If Credits=0 then 
			StopSound "V-NoCredit"
			PlaySound "V-NoCredit"
		End If	
		If Credits>0 then
			Credits=Credits-1
			PrintCredits
			AllLightsOff
			FirstBall
		End If
	End If
	
	If keycode=StartGameKey and BallActive=1 then
		TiltSensor=999
		SlamTilt=1
		LED "SLAM TILT", 1
	End If
    
End Sub

Sub Table1_KeyUp(ByVal keycode)
    
	If keycode = LeftFlipperKey and BallActive>0 Then
		LeftFlipper.RotateToStart
		Flipper3.RotateToStart
		PlaySound "FlipperDown"
	End If
    
	If keycode = RightFlipperKey and BallActive>0 Then
		RightFlipper.RotateToStart
		PlaySound "FlipperDown"
	End If

End Sub

Sub Drain_Hit()
	Drain.DestroyBall
	PlaySound "Drain"
	MultiBalls=MultiBalls-1
	If MultiBalls=1 and BallActive=1 then
		If MultiRound=1 then
			For f=1 to 5
				NickPark(f)=0
			Next
			NickLaneDone=0
			NickParkDone=0
			If MachineTilt=0 then
				PlayMusic "W&G-MultiballEnd.MP3"
				PlaySound "V-MultiballEnd"
				TextWipe=50
				TextEvent=16
				MultiRound=0
				JackpotDone=0
				If ToastHeld=1 then
					TextWipe=0
					ToasterDown
					ToastTime=1000
					ToastVal=500000
					PlayMusic "W&G-Toast.MP3"
					ToastHeld=0
				End If
				CheckLights
			End If
		End If
		If MultiRound=2 then
			For f=1 to 5
				Veg(f)=0
			Next
			VegDone=0
			VegHit=0
			For f=1 to 9
				Sheep(f)=0
			Next
			SpotDone(1)=0
			SpotDone(2)=0
			SpotDone(3)=0
			RoundReady=0
			CurrentRound=0
			RoundDone=0
			For f=1 to 6
				Round(f)=0
			Next
			If MachineTilt=0 then
				PlayMusic "W&G-MultiballEnd.MP3"
				PlaySound "V-WererabbitEnd"
				TextWipe=50
				TextEvent=36
				MultiRound=0
				If ToastHeld=1 then
					TextWipe=0
					ToasterDown
					ToastTime=1000
					ToastVal=500000
					PlayMusic "W&G-Toast.MP3"
					ToastHeld=0
				End If
				CheckLights
			End If
		End If

	End If
	
	If MultiBalls=0 and BallActive=1 then
	'	Bumper1.Disabled=False
	'	Bumper2.Disabled=False
	'	Bumper3.Disabled=False
	'	LeftSlingshot.Disabled=False
	'	RightSlingshot.Disabled=False
		GateTime=0
		GateClose
		If ToastTime>0 then
			ToastTime=0
			ToasterUp
			PlaySound "Click"
		End If
		If RoundTime>0 then
			RoundTime=0
			Round(CurrentRound)=1
			RoundDone=RoundDone+1
			If CurrentRound=3 then
				ToolDone=0
				For f=1 to 3
					Tool(f)=0
				Next
			End If
			If CurrentRound=6 then
				For f=1 to 9
					Sheep(f)=0
				Next
				SpotDone(1)=0
				SpotDone(2)=0
				SpotDone(3)=0
			End If
			CurrentRound=0
		End If
		For f=1 to 7
			LightPulse(f)=0
		Next
		If Wendolene.IsDropped=True then
			PlaySound "Click"
			Wendolene.IsDropped=False
		End If
		If BallSave>0 then
			BallSave=-1
			SavePost.IsDropped=True
			PlaySound "Drop"
		End If
		If MachineTilt=1 then
			LED "", 1
			LED "", 2
			BonusPause=25
		End If
		If MachineTilt=0 then BonusPause=351
		If SlamTilt=1 then BonusPause=-390
		TextWipe=0
		TextEvent=0
		TextFeature=0
		BallActive=2
	End If

End Sub

Sub LeftSlingshot_Slingshot()
	If MachineTilt=0 then
		PlaySound "Sling"
		StopSound "V-Sling1"
		StopSound "V-Sling2"
		PlaySound "V-Sling1"
		AddScore(130)
	End If
End Sub

Sub RightSlingshot_Slingshot()
	If MachineTilt=0 then
		PlaySound "Sling"
		StopSound "V-Sling1"
		StopSound "V-Sling2"
		PlaySound "V-Sling2"
		AddScore(130)
	End If
End Sub

Sub Spinner1_spin:Playsound "fx_spinner":AddScore(60):End Sub
Sub Spinner2_spin:Playsound "fx_spinner":AddScore(60):End Sub
Sub Spinner3_spin:Playsound "fx_spinner":AddScore(60):End Sub
Sub Spinner4_spin:Playsound "fx_spinner":AddScore(60):End Sub

Sub RampFlash_Hit:FlasherLight3.duration 2, 1200, 0:FlasherLight2.duration 2, 1200, 0:GIBLINK: End Sub

Sub GIBLINK()
	dim xx
	for each xx in GI:xx.duration 2, 1000, 1:Next
End Sub

Sub GION()
	dim xx
	for each xx in GI:xx.state = 1:Next
	'Table1.colorgradeimage = "ColorGradeLUT256x16_extraConSat"
End Sub

Sub GIOFF()
	dim xx
	for each xx in GI:xx.state = 0:Next
	'Table1.colorgradeimage = "ColorGrade_4"
End Sub

Sub CheckTool(f)
	fff=0
	StopSound "V-MidLane"
	PlaySound "V-MidLane"
	If Tool(f)=0 and LightPulse(3)=0 then
		Tool(f)=1
		ToolDone=ToolDone+1
		If ToolDone=3 and (CurrentRound<>3 or RoundTime=0) then
			LightPulse(3)=25
			StopAllSound
			h=Rnd
			If h>.5 then PlaySound "V-Tools1"
			If h<=.5 then PlaySound "V-Tools2"
			ToolDone=0
			For g=1 to 3
				Tool(g)=0
			Next
			TextWipe=50
			If ContraptionReady=0 then TextEvent=6
			If ContraptionReady=1 then
				TextEvent=39
				fff=1
				AddScore(200000)
			End If
			ContraptionReady=1
		End If
		If ToolDone=3 and CurrentRound=3 and RoundTime>0 then
			StopSound "V-Midlane"
			PlaySound "V-AGDO1Score"
			TextWipe=50
			TextEvent=23
			fff=1
			AddScore(1000000)
			RoundTotal=1000000
		End If
		CheckLights
		If fff=0 then AddScore(10100)
	End If
End Sub

Sub Lane1_Hit()
	SwitchPause(1)=12
	If MachineTilt=0 then
		CheckTool(1)
		If MultiRound=2 then CheckVeg(1)
	End If
End Sub

Sub Lane2_Hit()
	SwitchPause(5)=12
	If MachineTilt=0 then
		CheckTool(2)
		If MultiRound=2 then CheckVeg(3)
	End If
End Sub

Sub Lane3_Hit()
	If MachineTilt=0 and InPlunger=0 and BedTime<50 then CheckTool(3)
	If MultiRound=2 and BedTime<100 then CheckVeg(5)
    FlasherLight3.duration 2, 1200, 0:FlasherLight2.duration 2, 1200, 0
	GIBLINK
End Sub

Sub CheckVeg(f)
	If Veg(f)=0 then
		Veg(f)=1
		VegHit=f
		VegDone=VegDone+1
		fff=1
		StopAllSound
		If VegHit=1 then PlaySound "V-VegScore1"
		If VegHit=2 then PlaySound "V-VegScore2"
		If VegHit=3 then PlaySound "V-VegScore3"
		If VegHit=4 then PlaySound "V-VegScore4"
		If VegHit=5 then PlaySound "V-VegScore5"
		TextWipe=50
		TextEvent=35
		AddScore(500000)
		RoundTotal=RoundTotal+500000
		CheckLights
	End If
End Sub

Sub CheckWAndG(f)
	fff=0
	StopSound "V-TopLane"
	PlaySound "V-TopLane"
	If WAndG(f)=0 and LightPulse(1)=0 then
		WAndG(f)=1
		WAndGDone=WAndGDone+1
		If WAndGDone=3 then
			LightPulse(1)=25
			WAndGDone=0
			StopSound "V-TopLane"
			PlaySound "V-BonusX"
			For g=1 to 3
				WAndG(g)=0
			Next
			If BonusMult=6 then
				TextWipe=50
				TextEvent=9	
				AddScore(250000)
				fff=1
			End If
			If BonusMult<6 then
				BonusMult=BonusMult+1
				TextWipe=50
				TextEvent=8
			End If
		End If
		If fff=0 then AddScore(5100)
		CheckLights
	End If
End Sub

Sub LaneW_Hit()
	SwitchPause(2)=12
	If MachineTilt=0 then CheckWAndG(1)
End Sub

Sub LaneAnd_Hit()
	SwitchPause(3)=12
	If MachineTilt=0 then CheckWAndG(2)
End Sub

Sub LaneG_Hit()
	SwitchPause(4)=12
	If MachineTilt=0 then CheckWAndG(3)
End Sub

Sub CheckNickPark(f)
	fff=0
	StopSound "V-ReturnLane"
	PlaySound "V-ReturnLane"
	If NickPark(f)=0 and LightPulse(2)=0 then
		NickPark(f)=1
		NickLaneDone=NickLaneDone+1
		If NickLaneDone=4 then
			LightPulse(2)=25
			NickLaneDone=0
			TextWipe=50
			TextEvent=10
			AddScore(250000)
			fff=1
			NickParkDone=NickParkDone+1
			StopAllSound
			If NickParkDone=1 then PlaySound "V-NickPark1"
			If NickParkDone=2 then PlaySound "V-NickPark2"
			If NickParkDone=3 then PlaySound "V-NickPark3"						
			If NickParkDone=4 then
				PlaySound "V-NickPark4"
				If RoundTime=0 and MultiRound=0 then PlayMusic "W&G-MultiballReady.MP3"
			End If
			If NickParkDone=5 then
				PlaySound "V-NickPark1"
				TextEvent=11
				NickParkDone=4
			End If
			If GateTime<750 then GateTime=750
			GateOpen
			For g=1 to 4
				NickPark(g)=0
			Next
		End If
		If fff=0 then AddScore(10100)
		CheckLights
	End If
End Sub

Sub LaneNI_Hit()
	SwitchPause(6)=12
	If MachineTilt=0 then
		CheckNickPark(1)
		StopAllSound
		If SPReady<>1 then
			h=(Int(Rnd*3))+1
			If h=1 then PlaySound "V-Outlane1"
			If h=2 then PlaySound "V-Outlane2"
			If h=3 then PlaySound "V-Outlane3"
		End If
		If SPReady=1 then
			SPReady=2
			PlayMusic "W&G-Special2.MP3"
			PlaySound "V-DoubleJackpot"
			CreditPause=1
			PlaySound "Knock"
			TextWipe=50
			TextEvent=2
		End If
	End If
End Sub

Sub LaneCK_Hit()
	SwitchPause(7)=12
	If MachineTilt=0 then CheckNickPark(2)
End Sub

Sub LanePA_Hit()
	SwitchPause(8)=12
	If MachineTilt=0 then CheckNickPark(3)
End Sub

Sub LaneRK_Hit()
	SwitchPause(9)=12
	If MachineTilt=0 then
		CheckNickPark(4)
		StopAllSound
		If SPReady<>1 then
			h=(Int(Rnd*3))+1
			If h=1 then PlaySound "V-Outlane1"
			If h=2 then PlaySound "V-Outlane2"
			If h=3 then PlaySound "V-Outlane3"
		End If
		If SPReady=1 then
			SPReady=2
			PlayMusic "W&G-Special2.MP3"
			PlaySound "V-DoubleJackpot"
			CreditPause=1
			PlaySound "Knock"
			TextWipe=50
			TextEvent=2
		End If
	End If
End Sub

Sub RampBase_Hit()
	If ActiveBall.VelY<-5 and MachineTilt=0 then PlaySound "V-Train"
End Sub

Sub RampScore_Hit()
	fff=0
	If MachineTilt=0 then

		TrainDone=TrainDone+1
		WhichTrain=WhichTrain+1
		If WhichTrain>30 then WhichTrain=1
		TextWipe=50
		TextEvent=3
		If WhichTrain/5=Int(WhichTrain/5) then PlaySound "V-TrainBonus"
		If WhichTrain=5 or WhichTrain=20 then
			AddScore(250000)
			fff=1
		End If
		If WhichTrain=10 or WhichTrain=25 then
			AddScore(500000)
			fff=1
		End If
		If WhichTrain=15 then
			ShootAgain=ShootAgain+1
			PlayMusic "W&G-ExtraBall.MP3"
		End If
		If WhichTrain=30 then SPReady=1

		If SkillTime>0 then
			SkillVal=SkillVal+250000
			If SkillVal>1000000 then SkillVal=1000000
			fff=1
			SkillTime=0
			TextWipe=50
			TextEvent=37
			AddScore(SkillVal)
			StopSound "V-TrainBonus"
			PlaySound "V-SkillShot"
		End If
		If RoundTime>0 and CurrentRound=2 then
			TextWipe=50
			TextEvent=20
			AddScore(300000)
			fff=1
			RoundTotal=RoundTotal+300000
			StopSound "V-TrainBonus"
			PlaySound "V-TWT2Score"
		End If
		
		If MultiRound=2 then CheckVeg(2)

		If fff=0 then AddScore(15000)
		CheckLights
	End If
End Sub

Sub CheckSpot(f)
	fff=0
	StopSound "V-Spot1"
	StopSound "V-Spot2"
	StopSound "V-Spot3"
	If f=1 or f=4 or f=7 then
		PlaySound "V-Spot1"
	End If
	If f=2 or f=5 or f=8 then
		PlaySound "V-Spot2"
	End If
	If f=3 or f=6 or f=9 then
		PlaySound "V-Spot3"
	End If
	g=2
	If f<4 then g=1
	If f>6 then g=3 
	If Sheep(f)=0 and LightPulse(4+g)=0 and (CurrentRound<>6 or RoundTime=0) then
		Sheep(f)=1
		SpotDone(g)=SpotDone(g)+1
		If SpotDone(g)=3 then
			LightPulse(4+g)=25
			For f=1 to 9
				Sheep(f)=0
			Next
			For f=1 to 3
				SpotDone(f)=0
			Next
			If RoundTime>0 or MultiRound>0 then
				StopAllSound
				PlaySound "V-NickPark1"
				TextEvent=38
				TextWipe=50
				AddScore(300000)
				fff=1
			End If
			If RoundTime=0 and MultiRound=0 then
			StopAllSound
			If g=1 and (Round(1)=0 or Round(2)=0) then PlaySound "V-3Bank1"
			If g=2 and (Round(3)=0 or Round(4)=0) then PlaySound "V-3Bank2"
			If g=3 and (Round(5)=0 or Round(6)=0) then PlaySound "V-3Bank3"
			If RoundDone=6 then PlaySound "V-3Bank4"
			If Round(g*2)=1 and Round((g*2)-1)=1 then
				If RoundDone<6 then PlaySound "V-NickPark1"
				TextWipe=50
				TextEvent=38
				AddScore(300000)
				fff=1
			Else
				If Round(g*2)=0 then CurrentRound=g*2
				If Round((g*2)-1)=0 then CurrentRound=(g*2)-1
				RoundReady=1
				TextWipe=50
				TextEvent=39+g
			End If
		End If
		SavePost.IsDropped=False
		If BallSave=0 then PlaySound "Click"
		If BallSave<750 then BallSave=750
		End If
	End If
			
	If Sheep(f)=0 and CurrentRound=6 and RoundTime>0 then
		Sheep(f)=1
		SheepDone=SheepDone+1 
		TextWipe=50
		TextEvent=33
		If SheepDone<9 then TextEvent=32
		fff=1
		g=50000+(SheepDone*50000)
		RoundTotal=RoundTotal+g
		If SheepDone=9 then
			RoundTime=1
			PlaySound "V-AC1Success"
			PlayMusic "W&G-RoundEnd.MP3"
			AddScore(g)
		End If
	End If
			
	CheckLights
	If fff=0 then AddScore(5100)

End Sub

Sub Spot1a_Hit()
	SpotPause(1)=6
	If MachineTilt=0 then CheckSpot(1)
End Sub

Sub Spot2a_Hit()
	SpotPause(2)=6
	If MachineTilt=0 then CheckSpot(2)
End Sub

Sub Spot3a_Hit()
	SpotPause(3)=6
	If MachineTilt=0 then CheckSpot(3)
End Sub

Sub Spot4a_Hit()
	SpotPause(4)=6
	If MachineTilt=0 then CheckSpot(4)
End Sub

Sub Spot5a_Hit()
	SpotPause(5)=6
	If MachineTilt=0 then CheckSpot(5)
End Sub

Sub Spot6a_Hit()
	SpotPause(6)=6
	If MachineTilt=0 then CheckSpot(6)
End Sub

Sub Spot7a_Hit()
	SpotPause(7)=6
	If MachineTilt=0 then CheckSpot(7)
End Sub

Sub Spot8a_Hit()
	SpotPause(8)=6
	If MachineTilt=0 then CheckSpot(8)
End Sub

Sub Spot9a_Hit()
	SpotPause(9)=6
	If MachineTilt=0 then CheckSpot(9)
End Sub

Sub LeftScoop_Hit()
	StopAllSound
	PlaySound "KickerIn"
	ScoopPause(1)=20
	
	If MachineTilt=0 then
		ScoopPause(1)=100
		fff=0
				
		If CurrentRound=3 and RoundTime>0 and ToolDone=3 then
			fff=1
			RoundTime=1
			RoundTotal=2000000
			AddScore(1000000)
			PlaySound "V-AGDO1Success"
			PlayMusic "W&G-RoundEnd.MP3"
		End If
	
		If MultiRound=0 and RoundReady=1 and CurrentRound/2<>Int(CurrentRound/2) then
			fff=1
			ScoopPause(1)=100
			RoundTime=2350
			RoundReady=0
			RoundTotal=0
			ToastTime=0
			ToastVal=0
			TextWipe=50
			If CurrentRound=1 then
			TextEvent=17
			PlaySound "V-TWT1Start"
			PlayMusic "W&G-Mode1.MP3"
		End If
		If CurrentRound=3 then
			ToolDone=0
			For f=1 to 3
				Tool(f)=0
			Next
			If Wendolene.IsDropped=True then PlaySound "Click"
			Wendolene.IsDropped=False
			TextEvent=22
			PlaySound "V-AGDO1Start"
			PlayMusic "W&G-Mode2.MP3"
		End If
		If CurrentRound=5 then
			If Wendolene.IsDropped=True then PlaySound "Click"
				Wendolene.IsDropped=False
				TextEvent=28
				PlaySound "V-ACS1Start"
				PlayMusic "W&G-Mode3.MP3"
			End If
			AddScore(25000)
		End If
	
		If ContraptionReady=1 then
			fff=1
			ContraptionReady=0
			ContraptionDone=ContraptionDone+1
			WhichContraption=WhichContraption+1
			If RoundTime<2350 then
			If WhichContraption=1 or WhichContraption=4 then PlaySound "V-Contraption1"
			If WhichContraption=2 or WhichContraption=5 then PlaySound "V-Contraption2"
			If WhichContraption=3 or WhichContraption=6 then PlaySound "V-Contraption3"
			If RoundTime<>1 or ToolDone<>3 then TextWipe=50
			If RoundTime<>1 or ToolDone<>3 then TextEvent=5
			If WhichContraption<6 then AddScore(200000*WhichContraption)
		End If
		If WhichContraption=6 then
			WhichContraption=0
			LightPulse(4)=25
			If RoundTime>0 and RoundTime<2350 then MusicBreak=1
			If ContraptionAward=1 then
				ShootAgain=ShootAgain+1
				PlayMusic "W&G-ExtraBall.MP3"
			End If
			If ContraptionAward=2 then
				PlayMusic "W&G-Special.MP3"
				CreditPause=1
				PlaySound "Knock"
			End If
			ContraptionAward=3-ContraptionAward
		End If	
	End If

	If JackpotDone=0 and MultiRound=1 then
		fff=1
		JackpotDone=1
		StopAllSound
		PlaySound "V-Jackpot1"
		RoundTotal=RoundTotal+500000
		TextWipe=50
		TextEvent=14
		AddScore(500000)
	End If
	
	If MultiRound=2 and VegDone>4 then
		fff=1
		If Relight(1)=1 then AddScore(25000)
		If Relight(1)=0 then
			Relight(1)=1
			VegDone=VegDone+1
			If VegDone=6 then
				PlaySound "V-VegScoop1"
				TextWipe=50
				TextEvent=43
			End If
			If VegDone=7 then
				PlaySound "V-WererabbitStart"
				VegDone=0
				VegHit=0
				For f=1 to 5
					Veg(f)=0
				Next
				Relight(1)=0
				Relight(2)=0
				TextWipe=50
				TextEvent=44
			End If
			RoundTotal=RoundTotal+1000000
			AddScore(1000000)
		End If
	End If	

	If fff=0 then
		h=Rnd
		If h>.5 then PlaySound "V-LeftUnlit1"
		If h<=.5 then PlaySound "V-LeftUnlit2"
		AddScore(25000)
	End If
			
	CheckLights
	End If

End Sub

Sub Safety_Hit()
	If ActiveBall.VelX>-3 then
		ActiveBall.VelX=ActiveBall.VelX-3
		ActiveBall.VelY=ActiveBall.VelY+3
	End If
End Sub

Sub RightScoop_Hit()
	StopAllSound
	PlaySound "KickerIn"
	ScoopPause(2)=20
	
	If MachineTilt=0 then
		ScoopPause(2)=100
		fff=0
		
		If CurrentRound=4 and RoundTime>0 then
			fff=1
			MoonDone=MoonDone+1
			If MoonDone>3 then MoonDone=1
			RoundTotal=RoundTotal+500000
			TextWipe=50
			TextEvent=26
			AddScore(500000)
			If MoonDone=1 then PlaySound "V-AGDO2Score1"
			If MoonDone=2 then PlaySound "V-AGDO2Score2"
			If MoonDone=3 then PlaySound "V-AGDO2Score3"
		End If
	
		If MultiRound=0 and RoundReady=1 and CurrentRound/2=Int(CurrentRound/2) then
			fff=1
			ScoopPause(2)=100
			RoundTime=2350
			RoundReady=0
			RoundTotal=0
			ToastVal=0
			ToastTime=0
			TextWipe=50
			If CurrentRound=2 then
				TextEvent=19
				PlaySound "V-TWT2Start"
				PlayMusic "W&G-Mode2.MP3"
			End If
			If CurrentRound=4 then
				TextEvent=25
				PlaySound "V-AGDO2Start"
				PlayMusic "W&G-Mode3.MP3"
			End If
			If CurrentRound=6 then
				SheepDone=0
				For f=1 to 9
					Sheep(f)=0
				Next
				For f=1 to 3
					SpotDone(f)=0
				Next
				TextEvent=31
				PlaySound "V-ACS2Start"
				PlayMusic "W&G-Mode1.MP3"
			End If
			AddScore(25000)	
		End If
	
		If CheeseReady=1 then
			fff=1
			CheeseReady=0
			WhichCheese=WhichCheese+1
			CheeseDone=CheeseDone+1
			If RoundTime<2350 and (CurrentRound<>4 or RoundTime=0) then
				If WhichCheese=1 then PlaySound "V-GetCheese1"
				If WhichCheese=2 then PlaySound "V-GetCheese2"
				TextWipe=50
				TextEvent=4
				AddScore(25000)
			End If
			If WhichCheese=3 then
				If RoundTime=0 and MultiRound=0 then
					PlayMusic "W&G-Toast.MP3"
					PlaySound "V-ToastStart"
					ToasterDown
				End If
				WhichCheese=0
				ToastTime=1100
				ToastVal=500000
				If RoundTime>0 or MultiRound>0 then
					ToastHeld=1
					If RoundTime<2350 then PlaySound "V-AGDO2Score1"
					ToastTime=0
				End If
			End If	
		End If
	
		If JackpotDone=1 and MultiRound=1 then
			fff=1
			JackpotDone=2
			StopAllSound
			PlaySound "V-Jackpot2"
			RoundTotal=RoundTotal+500000
			TextWipe=50
			TextEvent=14
			AddScore(500000)
		End If
		
		If MultiRound=2 and VegDone>4 then
			fff=1
			If Relight(2)=1 then AddScore(25000)
			If Relight(2)=0 then
				Relight(2)=1
				VegDone=VegDone+1
				If VegDone=6 then
					PlaySound "V-VegScoop1"
					TextWipe=50
					TextEvent=43
				End If
				If VegDone=7 then
					PlaySound "V-WererabbitStart"
					VegDone=0
					VegHit=0
					For f=1 to 5
						Veg(f)=0
					Next
					Relight(1)=0
					Relight(2)=0
					TextWipe=50
					TextEvent=44
				End If
				AddScore(1000000)
				RoundTotal=RoundTotal+1000000
			End If
		End If	

		If fff=0 then
			h=Rnd
			If h>.5 then PlaySound "V-RightUnlit1"
			If h<=.5 then PlaySound "V-RightUnlit2"
			AddScore(25000)
		End If
			
		CheckLights
	End If
End Sub

Sub Wendolene_Hit()
	Wendolene.IsDropped=True
	PlaySound "Drop"
	If MachineTilt=0 then
		fff=0
		If CheeseReady=1 then PlaySound "V-Drop"
		If CheeseReady=0 then
			h=(Int(Rnd*3))+1
			If h=1 then PlaySound "V-CheeseLit1"
			If h=2 then PlaySound "V-CheeseLit2"
			If h=3 then PlaySound "V-CheeseLit3"
			CheeseReady=1
		End If
		If RoundTime>0 and CurrentRound=5 then
			RoundTotal=RoundTotal+300000
			StopSound "V-Drop"
			StopSound "V-CheeseLit1"
			StopSound "V-CheeseLit2"
			StopSound "V-CheeseLit3"
			PlaySound "V-ACS1Score"
			TextWipe=50
			TextEvent=29
			AddScore(300000)
			fff=1
			DropPause=50
		End If
		If fff=0 then AddScore(10100)
	End If
End Sub

Sub Toaster_Hit()
	If MachineTilt=0 then
		fff=0
		If ToastTime=0 then PlaySound "V-Toaster"
		If ToastTime>0 then
			ToasterUp
			ToastTime=0
			PlayMusic "W&G-RoundEnd.MP3"
			StopSound "V-ToastStart"
			PlaySound "V-ToastScore"
			TextWipe=50
			TextEvent=7
			TextFeature=0
			AddScore(ToastVal)
			fff=1
		End If
		If MultiRound=2 then CheckVeg(4)
		If fff=0 then AddScore(10100)
	End If
End Sub


Sub BallSpeed_Hit()
	ActiveBall.VelY = ActiveBall.VelY/3
End Sub

Sub ToTop_Hit()
	ToTop.DestroyBall
	FromBottom.CreateBall
	FromBottom.Kick 180,5
End Sub

Sub BedLock_Hit()
	BedLock.DestroyBall
	PlaySound "KickerIn"
	BallsLocked=BallsLocked+1
	If MachineTilt=1 then BedTime=100
	If MachineTilt=0 then
		fff=0
		If BallsLocked=1 then
			If MultiRound>0 then BedTime=100
			If MultiRound=0 and RoundDone<6 and NickParkDone<4 then BedTime=100
			If MultiRound=0 and (RoundDone>5 or NickParkDone>3) then
				BedTime=451
				TextWipe=200
				If NickParkDone>3 then
					MultiRound=1
					TextEvent=13
				End If
				If RoundDone>5 and MultiRound=0 then
					MultiRound=2
					TextEvent=34
				End If
			End If
		End If
		If JackpotDone=2 and MultiRound=1 then
			StopAllSound
			PlaySound "V-DoubleJackpot"
			AddScore(1000000)
			fff=1
			RoundTotal=RoundTotal+1000000
			JackpotDone=0
		End If
		If fff=0 and BallsLocked=1 then AddScore(25000)
		CheckLights
	End If
	
End Sub

Sub Table1_Init()
	FlexDMD_Init
	GateClose	
	BedAnim.heighttop = 131
'	Flasher.SetValue(0)
	BoardAnim.imageA = "Board0"
	ToasterUp
	SavePost.IsDropped=True	

	AllLightsOff
	AttractCount=-250
	DMDCount=-250

	TextBlink=1
	TextWipe=0
	TextEvent=0
	Credits=0
	LastScore=0
	
	Score1.SetValue(0):SetScore 0
	Score2.SetValue(0)
	CR 0
	BR 0

	HighScore(1)=30000000
	HighScore(2)=25000000
	HighScore(3)=20000000
	HighScore(4)=15000000
	ReplayScore=15000000
	BallMax=3
	
	Score=LastScore
	BallActive=0
End Sub

Sub Gate1_Hit()
	PlaySound "Gate3"
End Sub

Sub Gate2_Hit()
	PlaySound "Gate"
End Sub

Sub Gate3_Hit()
	PlaySound "Gate"
End Sub

Sub Gate4_Hit()
	PlaySound "Gate"
End Sub

Sub Gate5_Hit()
	PlaySound "Gate"
	Wendolene.IsDropped=False
	PlaySound "Click"
End Sub

Sub AddScore(f)
	If MachineTilt=0 then
		Score=Score+f
		If Score>99999990 then
			PlaySound "Knock"
			Score=Score-100000000
			Credits=Credits+2
			CreditPause=1
		End If	
		If GotReplay=0 and Score>ReplayScore then
			GotReplay=1
			PlaySound "Knock"
			CreditPause=1
			If BallActive=1 then
				TextWipe=50
				TextEvent=1
			End If
		End If
		PrintScore
	End If
End Sub

Sub PrintCredits
	If Credits>9 then Credits=9
	If BallActive=1 or (DMDCount>0 and DMDCount<301) then CR Credits+2
	CreditLight.State=0
	If Credits>0 then CreditLight.State=1
	SaveData
End Sub

Sub FirstBall

	StopAllSound
	DemoTime=0
	AllLightsOn
	SlamTilt=0
	StartPause=426
	
	Score=0
	GotReplay=0
	PrintScore
	LED "", 1
	LED "", 2
	BallsLeft=BallMax+1
	ShootAgain=0


	For f=1 to 6
		Round(f)=0
	Next
	RoundDone=0
	RoundReady=0
	CurrentRound=0
	WhichCheese=0
	TrainDone=1

	WhichTrain=1
	CheeseDone=0
	ContraptionDone=0
	WhichContraption=0
	ContraptionReady=0
	ContraptionAward=1
	CheeseReady=0
	
	For f=1 to 3
		Tool(f)=0
	Next
	ToolDone=0
	
	For f=1 to 4
		WandG(f)=0
	Next
	WandGDone=0

	For f=1 to 5
		NickPark(f)=0
	Next
	NickLaneDone=0
	NickParkDone=0
	
	SkillVal=0

	BallSave=0
	AttractCount=-999999
	DMDCount=-999999
	
	NextBall
End Sub

Sub NextBall

	AllLightsOff

	SPReady=0	
	BonusMult=1	
	
	For f=1 to 5
		Veg(f)=0
	Next
	
	For f=1 to 7
		LightPulse(f)=0
	Next
		
	For f=1 to 9
		Sheep(f)=0
	Next
	For f=1 to 3
		SpotDone(f)=0
	Next
	
	MultiBalls=1
	MultiRound=0

	ToastVal=0
	ToastTime=0
	ToastHeld=0
	SheepDone=0
	VegDone=0
	RocketDone=0
	MoonDone=0
	JackPotDone=0
	
	RoundTime=0
	RoundTotal=0

	If ShootAgain=0 then BallsLeft=BallsLeft-1
			
	MachineTilt=0
	TiltSensor=0
	LightBlink=0
	MusicBreak=0
	
	For f=1 to 7
		LightPulse(f)=0
	Next

End Sub

Sub CheckLights
	If BallActive>0 and MachineTilt=0 then
		AllLightsOff
		
		If BumpPause(1)>0 then
			Bumper1L.State=1
			Bumper1L1.State=1
		End If
		If BumpPause(2)>0 then
			Bumper2L.State=1
			Bumper2L1.State=1
		End If
		If BumpPause(3)>0 then
			Bumper3L.State=1
			Bumper3L1.State=1
		End If
		
		If ShootAgain>0 then Light59.state = 1
		If SPReady=1 then
			Light45.state = 1
			Light46.state = 1
		End If

		If BonusMult>1 then
			For f=1 to BonusMult-1
				MultLight(f).state = 1
			Next
		End If
		If MultiRound=0 and RoundReady=1 and RoundTime=0 and CurrentRound/2<>Int(CurrentRound/2) then Light15.state = 1
		If MultiRound=0 and RoundReady=1 and RoundTime=0 and CurrentRound/2=Int(CurrentRound/2) then Light19.state = 1
		
		For g=1 to 6
			If Round(g)=1 then RoundLight(g).state = 1
		Next
		
		For g=1 to 3
			If LightPulse(1)=0 and WAndG(g)=1 then WAndGLight(g).state = 1
		Next
		
		For g=1 to 4
			If LightPulse(2)=0 and NickPark(g)=1 then NickParkLight(g).state = 1
		Next
		
		If CurrentRound<>3 or RoundTime=0 then
			For g=1 to 3
				If LightPulse(3)=0 and Tool(g)=1 then ToolLight(g).state = 1
			Next
		End If
	
		If WhichContraption>1 then
			For g=1 to WhichContraption
				If LightPulse(4)=0 then ContraptionLight(g).state = 1
			Next
		End If
		If WhichContraption=1 then Light53.state = 1
			
		If CurrentRound<>6 or RoundTime=0 then
			For g=1 to 3
				If LightPulse(5)=0 and Sheep(g)=1 then SpotLight(g).state = 1
				If LightPulse(6)=0 and Sheep(g+3)=1 then SpotLight(g+3).state = 1
				If LightPulse(7)=0 and Sheep(g+6)=1 then SpotLight(g+6).state = 1
			Next
		End If
	
		If LightBlink>2 then CheckBlink
	End If
End Sub

Sub CheckBlink
	If NickParkDone<4 then BoardAnim.imageA = ("Board" & NickParkDone+1)
	If MultiRound=0 and LightBlink=3 and (NickParkDone=4 or RoundDone=6) then FlashStatus=8
	If ContraptionReady=1 then ContraptionLight(WhichContraption+1).state = 1
	If RoundTime>0 or RoundReady=1 then RoundLight(CurrentRound).state = 1
	If ToastTime>0 then Light12.state = 1
	If SkillTime>0 then Light23.state = 1
	If RoundTime>0 and CurrentRound=2 then Light21.state = 1
	If RoundTime>0 and CurrentRound=5 then Light22.state = 1
	If RoundTime>0 and CurrentRound=4 then Light18.state = 1
	
	If MultiRound=1 then
		If JackpotDone=0 then Light16.state = 1
		If JackpotDone=1 then Light20.state = 1
		If JackpotDone=2 and LightBlink=3 then FlashStatus=8
	End If
	
	If RoundDone=6 then Light44.state = 1
	If MultiRound=2 then
		For g=1 to 5
			If Veg(g)=0 then VegLight(g).state = 1
		Next
		If VegDone>4 and Relight(1)=0 then Light15.state = 1
		If VegDone>4 and Relight(2)=0 then Light19.state = 1
	End If
	
	If CurrentRound=3 and RoundTime>0 then
		For f=1 to 3
			If Tool(f)=0 then ToolLight(f).state = 1
		Next
		If ToolDone=3 then Light14.state = 1
	End If
	
	If CurrentRound=6 and RoundTime>0 then
		For f=1 to 9
			If Sheep(f)=0 then SpotLight(f).state = 1
		Next
	End If
	
	If CurrentRound=1 and RoundTime>0 then
		Bumper1L.State=1
        Bumper1L1.State=1
        Bumper2L.State=1
        Bumper2L1.State=1
        Bumper3L.State=1
        Bumper3L1.State=1
	End If
	
	If ContraptionReady=1 then Light13.state = 1
	If CheeseReady=1 then Light17.state = 1	
	If ContraptionAward=1 then Light47.state = 1
	If ContraptionAward=2 then Light48.state = 1
End Sub

Sub TextTimer_Timer()

	TextBlink=-TextBlink
	
	If BallActive=1 and MachineTilt=0 then

		If TextWipe>0 then TextWipe=TextWipe-1
		If TextWipe=0 then TextEvent=0
		If TextEvent=0 and TextWipe=0 then

			ff=0
			If TextFeature<11 then
				TextFeature=TextFeature+1
				If TextFeature>10 then TextFeature=1
			End If
		
			If ToastTime>0 then TextFeature=101
			If CurrentRound=1 and RoundTime>0 then TextFeature=102
			If CurrentRound=2 and RoundTime>0 then TextFeature=103
			If CurrentRound=3 and RoundTime>0 and ToolDone<3 then TextFeature=104
			If CurrentRound=3 and RoundTime>0 and ToolDone=3 then TextFeature=105
			If CurrentRound=4 and RoundTime>0 then TextFeature=106
			If CurrentRound=5 and RoundTime>0 then TextFeature=107
			If CurrentRound=6 and RoundTime>0 then TextFeature=108
			

			CheckTextFeature
			If ff=0 then CheckTextFeature																																						

			If ff=0 then
					LED "", 1
					LED "", 2
			End If
		End If	

		If TextEvent=1 then
			If TextBlink=1 then
				LED "REPLAY", 1
				LED "", 2
			End If
			If TextBlink=-1 then
				LED "", 1
				LED "REPLAY", 2
			End If
		End If

		If TextEvent=2 then
			If TextBlink=1 then
				LED "SPECIAL", 1
				LED "", 2
			End If
			If TextBlink=-1 then
				LED "", 1
				LED "SPECIAL", 2
			End If
		End If

		If TextEvent=3 then
			If TextBlink=1 then
				If TrainDone=1 then LED "1 TRAIN RIDE", 1
				If TrainDone>1 then LED TrainDone&" TRAIN RIDES", 1	
			End If
			If TextBlink=-1 then
				If TrainDone=1 then LED "  TRAIN RIDE", 1
				If TrainDone>1 and TrainDone<10 then LED "  TRAIN RIDES", 1
				If TrainDone>9 then LED "   TRAIN RIDES", 1
			End If
			If TextBlink=-1 then LED "", 2
			If TextBlink=1 and (WhichTrain=5 or WhichTrain=20) then LED "250,000", 2
			If TextBlink=1 and (WhichTrain=10 or WhichTrain=25) then LED "500,000", 2
			If TextBlink=1 and WhichTrain=15 then LED "EXTRA BALL", 2
			If TextBlink=1 and WhichTrain=30 then LED "SPECIAL IS LIT", 2
			If WhichTrain<5 then LED "250,000 AT "&(Int (TrainDone/30))*30+5, 2
			If WhichTrain>5 and WhichTrain<10 then LED "500,000 AT "&(Int (TrainDone/30))*30+10, 2
			If WhichTrain>10 and WhichTrain<15 then LED "EXTRA BALL AT "&(Int (TrainDone/30))*30+15, 2
			If WhichTrain>15 and WhichTrain<20 then LED "250,000 AT "&(Int (TrainDone/30))*30+20, 2
			If WhichTrain>20 and WhichTrain<25 then LED "500,000 AT "&(Int (TrainDone/30))*30+25, 2
			If WhichTrain>25 and WhichTrain<30 then LED "SPEC. LIT AT "&(Int (TrainDone/30))*30+30, 2
		End If

		If TextEvent=4 then
			If TextBlink=1 then
				If CheeseDone=1 then LED "1 CHEESE", 1
				If CheeseDone>1 then LED CheeseDone&" CHEESES", 1	
			End If
			If TextBlink=-1 then
				If CheeseDone=1 then LED "  CHEESE", 1
				If CheeseDone>1 and CheeseDone<10 then LED "  CHEESES", 1
				If CheeseDone>9 then LED "   CHEESES", 1
			End If
			If CheeseDone/3=Int(CheeseDone/3) then
				If TextBlink=-1 then LED "", 2
				If TextBlink=1 and MultiRound=0 and RoundTime=0 then LED "GO FOR TOASTER", 2
				If TextBlink=1 and MultiRound>0 or RoundTime>0 then LED "TOASTER READY", 2
			End If
			If CheeseDone/3<>Int(CheeseDone/3) then LED "TOASTER AT "&((Int(CheeseDone/3))+1)*3, 2
		End If

		If TextEvent=5 then
			If WhichContraption=1 then LED "TELLYSCOPE", 1
			If WhichContraption=2 then LED "SOCCOMATIC", 1
			If WhichContraption=3 then LED "SNOOZATRON", 1
			If WhichContraption=4 then LED "AUTOCHEF", 1
			If WhichContraption=5 then LED "SNOWMANATRON", 1
			If WhichContraption=0 then LED "BUNNYLURE", 1
			If TextBlink=-1 then LED "", 2
			If TextBlink=-1 then
				If WhichContraption=1 then LED "200,000", 2
				If WhichContraption=2 then LED "400,000", 2
				If WhichContraption=3 then LED "600,000", 2
				If WhichContraption=4 then LED "800,000", 2
				If WhichContraption=5 then LED "1,000,000", 2
				If WhichContraption=0 and ContraptionAward=2 then LED "EXTRA BALL", 2
				If WhichContraption=0 and ContraptionAward=1 then LED "SPECIAL", 2
			End If
		End If

		If TextEvent=6 then
			LED "CONTRAPTION", 1
			LED "IS LIT", 2
		End If

		If TextEvent=7 then
			LED "CRACKING TOAST", 1
			If TextBlink=-1 then LED FormatNumber ((ToastVal), 0, -1, 0, -1), 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=8 then
			LED "BONUS", 1
			If TextBlink=-1 then LED BonusMult&"X", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=9 then
			LED "W-&-G", 1
			If TextBlink=-1 then LED "250,000", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=10 then
			If NickParkDone=1 then LED "SLIPPERS", 1
			If NickParkDone=2 then LED "BREAKFAST", 1
			If NickParkDone=3 then LED "NEWSPAPER", 1
			If NickParkDone=4 then LED "WALKIES", 1
			If TextBlink=-1 then LED "250,000", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=11 then
			LED "NI-CK-PA-RK", 1
			If TextBlink=-1 then LED "250,000", 2
			If TextBlink=1 then LED "", 2
		End If
		
		If TextEvent=12 then
			LED "CHEESE", 1
			LED "IS LIT", 2
		End If

		If TextEvent=13 then
			LED "MULTIBALL", 1
			If TextBlink=1 then LED "", 2
			If TextBlink=-1 then LED "START", 2
		End If

		If TextEvent=14 then
			LED "JACKPOT", 1
			If TextBlink=-1 then LED "500,000", 2
			If TextBlink=1 then LED "", 2
		End If
		
		If TextEvent=15 then
			LED "DOUBLE JACKPOT", 1
			If TextBlink=-1 then LED "1,000,000", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=16 then
			LED "MULTIBALL TOTAL", 1
			If TextBlink=-1 then LED FormatNumber ((RoundTotal), 0, -1, 0, -1), 2
			If TextBlink=-1 and RoundTotal=0 then LED "00", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=17 then
			LED "TROUSER TROUBLE", 1
			LED "HIT POP BUMPERS", 2
		End If

		If TextEvent=18 then
			LED "TROUSER TOTAL", 1
			If TextBlink=-1 then LED FormatNumber ((RoundTotal), 0, -1, 0, -1), 2
			If TextBlink=-1 and RoundTotal=0 then LED "00", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=19 then
			LED "FEATHERS MCGRAW", 1
			LED "IS GETTING AWAY", 2
		End If

		If TextEvent=20 then
			LED "CHASE AWARD", 1
			If TextBlink=-1 then LED "300,000", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=21 then 
			LED "FEATHERS TOTAL", 1
			If TextBlink=-1 then LED FormatNumber ((RoundTotal), 0, -1, 0, -1), 2
			If TextBlink=-1 and RoundTotal=0 then LED "00", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=22 then
			LED "BUILD THE", 1
			LED "MOON ROCKET", 2
		End If

		If TextEvent=23 then
			If TextBlink=-1 then LED "1,000,000", 1
			If TextBlink=1 then LED "", 1
			LED "GO FOR LAUNCH", 2
		End If

		If TextEvent=24 and TextWipe>49 then
			LED "BLAST OFF", 1
			If TextBlink=-1 then LED "1,000,000", 2
			If TextBlink=1 then LED "", 2
		End If
		If TextEvent=24 and TextWipe<50 then
			LED "ROCKET TOTAL", 1
			If TextBlink=-1 then LED FormatNumber ((RoundTotal), 0, -1, 0, -1), 2
			If TextBlink=-1 and RoundTotal=0 then LED "00", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=25 then
			LED "TRY SOME", 1
			LED "MOON CHEESE", 2
		End If

		If TextEvent=26 then
			LED "DELICIOUS", 1
			If TextBlink=-1 then LED "500,000", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=27 then
			LED "MOON TOTAL", 1
			If TextBlink=-1 then LED FormatNumber ((RoundTotal), 0, -1, 0, -1), 2
			If TextBlink=-1 and RoundTotal=0 then LED "00", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=28 then
			LED "WENDOLENE'S", 1
			LED "WINDOWS", 2
		End If

		If TextEvent=29 then
			LED "SQUEAKY CLEAN", 1
			If TextBlink=-1 then LED "300,000", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=30 then
			LED "WINDOWS TOTAL", 1
			If TextBlink=-1 then LED FormatNumber ((RoundTotal), 0, -1, 0, -1), 2
			If TextBlink=-1 and RoundTotal=0 then LED "00", 2
			If TextBlink=1 then LED "", 2
		End If
		
		If TextEvent=31 then
			LED "SAVE THE SHEEP", 1
			LED "FROM PRESTON", 2
		End If

		If TextEvent=32 then
			If SheepDone=1 then LED "FIRST SHEEP", 1
			If SheepDone=2 then LED "SECOND SHEEP", 1
			If SheepDone=3 then LED "THIRD SHEEP", 1
			If SheepDone=4 then LED "FOURTH SHEEP", 1
			If SheepDone=5 then LED "FIFTH SHEEP", 1
			If SheepDone=6 then LED "SIXTH SHEEP", 1
			If SheepDone=7 then LED "SEVENTH SHEEP", 1
			If SheepDone=8 then LED "EIGHTH SHEEP", 1
			If TextBlink=-1 then LED "", 2
			If TextBlink=-1 then
				If SheepDone=1 then LED "100,000", 2
				If SheepDone=2 then LED "150,000", 2
				If SheepDone=3 then LED "200,000", 2
				If SheepDone=4 then LED "250,000", 2
				If SheepDone=5 then LED "300,000", 2
				If SheepDone=6 then LED "350,000", 2
				If SheepDone=7 then LED "400,000", 2
				If SheepDone=8 then LED "450,000", 2
			End If
		End If

		If TextEvent=33 and TextWipe>49 then
			LED "LAST SHEEP", 1
			If TextBlink=-1 then LED "500,000", 2
			If TextBlink=1 then LED "", 2
		End If
		If TextEvent=33 and TextWipe<50 then
			LED "SHEEP TOTAL", 1
			If TextBlink=-1 then LED FormatNumber ((RoundTotal), 0, -1, 0, -1), 2
			If TextBlink=-1 and RoundTotal=0 then LED "00", 2
			If TextBlink=1 then LED "", 2
		End If
	
		If TextEvent=34 then
			LED "ULTIMATE ROUND", 1
			If TextBlink=1 then LED "", 2
			If TextBlink=-1 then LED "START", 2
		End If
		
		If TextEvent=35 then
			If VegHit=1 then LED "PUMPKIN", 1
			If VegHit=2 then LED "TOMATO", 1
			If VegHit=3 then LED "LETTUCE", 1
			If VegHit=4 then LED "CARROT", 1
			If VegHit=5 then LED "TURNIP", 1
			If TextBlink=-1 then LED "", 2
			If TextBlink=-1 then LED "500,000", 2
		End If

		If TextEvent=36 then
			LED "WERERABBIT TOTAL", 1
			If TextBlink=-1 then LED FormatNumber ((RoundTotal), 0, -1, 0, -1), 2
			If TextBlink=-1 and RoundTotal=0 then LED "00", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=37 then
			LED "SKILL SHOT", 1
			If TextBlink=-1 then LED FormatNumber ((SkillVal),0,-1,0,-1), 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=38 then
			LED "3-BANK BONUS", 1
			If TextBlink=-1 then LED "300,000" , 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=39 then
			LED "1-2-3 BONUS", 1
			If TextBlink=-1 then LED "200,000", 2
			If TextBlink=1 then LED "", 2
		End If
			
		If TextEvent=40 then
			LED "WRONG TROUSERS", 1
			LED "IS READY", 2
		End If

		If TextEvent=41 then
			LED "A GRAND DAY OUT", 1
			LED "IS READY", 2
		End If

		If TextEvent=42 then
			LED "A CLOSE SHAVE", 1
			LED "IS READY", 2
		End If

		If TextEvent=43 then
			LED "FIRST VEG SCOOP", 1
			If TextBlink=-1 then LED "1,000,000", 2
			If TextBlink=1 then LED "", 2
		End If

		If TextEvent=44 then
			LED "SECOND VEG SCOOP", 1
			If TextBlink=-1 then LED "1,000,000", 2
			If TextBlink=1 then LED "", 2
		End If

	End If

End Sub

Sub CheckDemo
	If DemoTime=2 then
	'	PlayMusic ""
		StopAllSound
		PlaySound "V-DemoStart"
		AttractCount=-999999
		DMDCount=-999999
		Score1.SetValue(0)
		Score2.SetValue(0)
		If UseFlexDMD <> 0 Then 
			FlexDMD.LockRenderThread
			Dim i
			With FlexDMDScene
				' clear score display
				For i = 8 To 1 Step - 1
					.GetImage("Score" & (i - 1)).Bitmap = FlexDMD.NewImage("", "VPX.DMDLargeSpace").Bitmap
				Next
				.GetImage("Comma2").Visible = False
				.GetImage("Comma1").Visible = False
			End with
			FlexDMD.UnlockRenderThread
		End If
		CR 0
		BR 0
	End If
	If DemoTime=2 or DemoTime=25 or DemoTime=48 or DemoTime=71 then
		AllLightsOn
		LED "INSTRUCTIONS", 1
		LED "", 2
	End If
	If DemoTime=17 or DemoTime=40 or DemoTime=63 or DemoTime=86 then
		AllLightsOff
		LED "", 1
		LED "", 2
	End If
	If DemoTime=115 then PlayMusic "W&G-MultiballReady.mp3"
	If DemoTime=120 then
		PlaySound "V-Demo2"
		LED "TOP W&G LANES", 1
		LED "ANDVANCE X.", 2
	End If
	If DemoTime=220 then
		LED "", 1
		LED "", 2
	End If
	If DemoTime=235 then
		LED "1-2-3 TOOL LANES", 1
		LED "LIGHT LEFT SCOOP", 2
	End If	
	If DemoTime=335 then
		LED "FOR CONTRAPTION.", 1
		LED "", 2
	End If	
	If DemoTime=435 then
		PlaySound "V-Contraption2"
		LED "COMPLETE ALL", 1
		LED "CONTRAPTIONS", 2
	End If
	If DemoTime=535 then
		LED "FOR EXTRA BALL", 1
		LED "OR SPECIAL.", 2
	End If
	If DemoTime=635 then
		LED "", 1
		LED "", 2
	End If
	If DemoTime=650 then
		PlaySound "V-TrainBonus"
		LED "RIDE THE TRAIN", 1
		LED "FOR SKILL SHOT,", 2
	End If
	If DemoTime=750 then
		LED "EXTRA BALL AND", 1
		LED "OUTLANE SPECIAL.", 2
	End If
	If DemoTime=850 then
		LED "", 1
		LED "", 2
	End If
	If DemoTime=865 then
		LED "WENDOLENE", 1
		LED "TARGET LIGHTS", 2
	End If
	If DemoTime=965 then
		PlaySound "V-CheeseLit2"
		LED "RIGHT SCOOP", 1
		LED "FOR CHEESE.", 2
	End If
	If DemoTime=1065 then
		LED "COLLECT", 1
		LED "THREE CHEESES", 2
	End If
	If DemoTime=1165 then
		PlaySound "V-ToastScore"
		LED "TO START", 1
		LED "TOAST COUNTDOWN.", 2
	End If
	If DemoTime=1265 then
		LED "", 1
		LED "", 2
	End If
	If DemoTime=1280 then
		PlaySound "V-Spot3"
		LED "MAKING A 3-BANK", 1
		LED "RAISES POST", 2
	End If
	If DemoTime=1380 then
		LED "AND LIGHTS", 1
		LED "NEXT ROUND.", 2
	End If
	If DemoTime=1480 then
		PlaySound "V-LeftUnlit2"
		LED "SHOOT LIT LEFT", 1
		LED "OR RIGHT SCOOP", 2
	End If
	If DemoTime=1580 then
		LED "TO PLAY ACT 1", 1
		LED "OR ACT 2.", 2
	End If
	If DemoTime=1680 then
		LED "WHEN ALL ROUNDS", 1
		LED "ARE COMPLETE,", 2
	End If
	If DemoTime=1780 then
		LED "UPPER LEVEL", 1
		LED "LIGHTS TO START", 2
	End If
	If DemoTime=1880 then
		PlaySound "V-VegScoop2"
		LED "WERE-RABBIT", 1
		LED "ULTIMATE ROUND.", 2
	End If
	If DemoTime=1980 then
		LED "", 1
		LED "", 2
	End If
	If DemoTime=1995 then
		LED "NI-CK-PA-PK", 1
		LED "OPENS GATES", 2
	End If
	If DemoTime=2095 then
		PlaySound "V-NickPark1"
		LED "AND ADVANCES", 1
		LED "MULTIBALL.", 2
	End If
	If DemoTime=2195 then
		LED "SHOOT FOR", 1
		LED "UPPER LEVEL", 2
	End If
	If DemoTime=2295 then
		PlaySound "V-MultiballEject"
		LED "WHEN MULTIBALL", 1
		LED "IS READY.", 2
	End If
	If DemoTime=2395 then
		LED "DURING", 1
		LED "MULTIBALL", 2
	End If
	If DemoTime=2495 then
		PlaySound "V-SkillShot"
		LED "SHOOT SCOOPS", 1
		LED "FOR JACKPOT,", 2
	End If
	If DemoTime=2595 then
		LED "THEN SHOOT", 1
		LED "UPPER LEVEL", 2
	End If
	If DemoTime=2695 then
		LED "FOR DOUBLE", 1
		LED "JACKPOT.", 2
	End If

	If DemoTime>119 and DemoTime<2095 then
		If (DemoTime/5)=Int(DemoTime/5) then AllLightsOff
	End If	
	
	If DemoTime>119 and DemoTime<220 then
		If (DemoTime/10)=Int(DemoTime/10) then
			For f=1 to 3
				WandGLight(f).state = 1
			Next
			For f=1 to 5
				MultLight(f).state = 1
			Next
		End If
	End If

	If DemoTime>234 and DemoTime<335 then
		If (DemoTime/10)=Int(DemoTime/10) then
			For f=1 to 3
				ToolLight(f).state = 1
			Next
		End If
	End If
	
	If DemoTime>334 and DemoTime<435 then
		If (DemoTime/10)=Int(DemoTime/10) then Light13.state = 1
	End If

	If DemoTime>434 and DemoTime<535 then
		If (DemoTime/10)=Int(DemoTime/10) then
			For f=1 to 6
				ContraptionLight(f).state = 1
			Next
		End If
	End If	
	
	If DemoTime>534 and DemoTime<635 then
		If (DemoTime/10)=Int(DemoTime/10) then
			Light47.state = 1
			Light48.state = 1
		End If
	End If
	
	If DemoTime>649 and DemoTime<750 then
		If (DemoTime/10)=Int(DemoTime/10) then Light23.state = 1
		If (DemoTime/10)=Int(DemoTime/10) then Light21.state = 1
	End If
	
	If DemoTime>749 and DemoTime<850 then
		If (DemoTime/10)=Int(DemoTime/10) then
			Light45.state = 1
			Light46.state = 1
			Light59.state = 1
		End If
	End If
	
	If DemoTime>864 and DemoTime<965 then
		If (DemoTime/10)=Int(DemoTime/10) then Light22.state = 1
	End If

	If DemoTime>964 and DemoTime<1165 then
		If (DemoTime/10)=Int(DemoTime/10) then Light17.state = 1
	End If
	
	If DemoTime=1065 then
		ToasterDown
		PlaySound "Drop"
	End If
	
	If DemoTime=1165 then
		ToasterUp
		PlaySound "Click"
	End If
	
	If DemoTime>1164 and DemoTime<1265 then
		If (DemoTime/5)=Int(DemoTime/5) then AllLightsOff
		If (DemoTime/10)=Int(DemoTime/10) then Light12.state = 1
	End If
	
	If DemoTime=1280 then
		SavePost.IsDropped=False
		PlaySound "Click"
	End If
	
	If DemoTime>1279 and DemoTime<1380 then
		If (DemoTime/10)=Int(DemoTime/10) then
			For f=1 to 9
				SpotLight(f).state = 1
			Next
		End If
	End If
	
	If DemoTime=1480 then
		SavePost.IsDropped=True
		PlaySound "Drop"
	End If
	
	If DemoTime>1379 and DemoTime<1480 then
		If (DemoTime/10)=Int(DemoTime/10) then
			For f=1 to 6
				RoundLight(f).state = 1
			Next
		End If
	End If
	
	If DemoTime>1479 and DemoTime<1680 then
		If (DemoTime/10)=Int(DemoTime/10) then
			Light15.state = 1
			Light19.state = 1
		End If
	End If

	If DemoTime>1679 and DemoTime<1780 then
		If (DemoTime/10)=Int(DemoTime/10) then
			For f=1 to 6
				RoundLight(f).state = 1
			Next
		End If
	End If

	If DemoTime>1779 and DemoTime<1880 then
		If (DemoTime/10)=Int(DemoTime/10) then Light44.state = 1
		If (DemoTime/20)=Int(DemoTime/20) then FlashStatus=8
	End If
	
	If DemoTime>1879 and DemoTime<1980 then
		If (DemoTime/10)=Int(DemoTime/10) then
			For f=1 to 5
				VegLight(f).state = 1
			Next
		End If
	End If
	
	If DemoTime>1994 and DemoTime<2095 then
		If (DemoTime/10)=Int(DemoTime/10) then
			For f=1 to 4
				NickParkLight(f).state = 1
			Next
		End If
	End If
	If DemoTime=1995 then GateOpen
	If DemoTime=2195 then GateClose	
	If DemoTime=2095 then AllLightsOff
	If DemoTime=2095 or DemoTime=2145 then BoardAnim.imageA = "Board1"
	If DemoTime=2108 or DemoTime=2158 then BoardAnim.imageA = "Board2"
	If DemoTime=2120 or DemoTime=2169 then BoardAnim.imageA = "Board3"
	If DemoTime=2133 or DemoTime=2183 then BoardAnim.imageA = "Board4"
	
	If DemoTime>2194 and DemoTime<2395 then
		If (DemoTime/5)=Int(DemoTime/5) then BoardAnim.imageA = "Board0"
		If (DemoTime/10)=Int(DemoTime/10) then BoardAnim.imageA = "Board5"
		If ((DemoTime-15)/20)=Int((DemoTime-15)/20) then FlashStatus=8
	End If
	
	If DemoTime=2295 then BedAnim.heighttop = 180
	If DemoTime=2300 then BedAnim.heighttop = 230
	
	If DemoTime=2395 then BedAnim.heighttop = 180
	If DemoTime=2400 then BedAnim.heighttop = 131
	
	If DemoTime>2394 and DemoTime<2594 then
		If (DemoTime/5)=Int(DemoTime/5) then AllLightsOff
		If (DemoTime/10)=Int(DemoTime/10) then
			Light16.state = 1
			Light20.state = 1
		End If
	End If
	
	If DemoTime>2594 and DemoTime<2795 then
		If (DemoTime/5)=Int(DemoTime/5) then AllLightsOff
		If (DemoTime/10)=Int(DemoTime/10) then AllLightsOn
	End If
						
	If DemoTime=2795 then
		AllLightsOff
		LED "", 1
		LED "", 2
		PlaySound "V-Bonus3"
		DemoTime=0
		AttractCount=-2
		DMDCount=-2
	End If 
End Sub

Sub ToasterDown
	ToastUp.IsDropped=True
	SwitchUp.IsDropped=True
	ToastDown.IsDropped=False
	SwitchDown.IsDropped=False
End Sub

Sub ToasterUp
	ToastUp.IsDropped=False
	SwitchUp.IsDropped=False
	ToastDown.IsDropped=True
	SwitchDown.IsDropped=True
End Sub

Sub AttractMode
	PulseStatus=PulseStatus+1
	If PulseStatus>8 then PulseStatus=1
	AllLightsOff
	If PulseStatus<5 then BoardAnim.imageA = "Board5"
	If PulseStatus>4 then BoardAnim.imageA = "Board0"
	If PulseStatus>4 then
        Bumper1L.State=1
        Bumper1L1.State=1
        Bumper2L.State=1
        Bumper2L1.State=1
        Bumper3L.State=1
        Bumper3L1.State=1
	End If
	If PulseStatus=8 then FlashStatus=8
	If PulseStatus>2 then Light01.state = 1
	If PulseStatus<3 or PulseStatus>4 then Light02.state = 1
	If PulseStatus<5 or PulseStatus>6 then Light03.state = 1
	If PulseStatus<3 then Light04.state = 1
	If PulseStatus=5 or PulseStatus=6 then Light05.state = 1
	If PulseStatus<3 then Light06.state = 1
	If PulseStatus<3 then Light07.state = 1
	If PulseStatus=3 or PulseStatus=4 then Light08.state = 1
	If PulseStatus=5 or PulseStatus=6 then Light09.state = 1
	If PulseStatus>6 then Light10.state = 1
	If PulseStatus<3 then Light11.state = 1
	If PulseStatus>6 then Light12.state = 1
	If PulseStatus<5 then Light13.state = 1
	If PulseStatus>1 and PulseStatus<6 then Light14.state = 1
	If PulseStatus>2 and PulseStatus<7 then Light15.state = 1	
	If PulseStatus>3 and PulseStatus<8 then Light16.state = 1	
	If PulseStatus>4 then Light17.state = 1	
	If PulseStatus=1 or PulseStatus>5 then Light18.state = 1
	If PulseStatus<3 or PulseStatus>6 then Light19.state = 1
	If PulseStatus<4 or PulseStatus=8 then Light20.state = 1
	If PulseStatus=3 or PulseStatus=4 then Light21.state = 1
	If PulseStatus=5 or PulseStatus=6 then Light22.state = 1
	If PulseStatus=3 or PulseStatus=4 then Light23.state = 1
	If PulseStatus<4 then Light24.state = 1
	If PulseStatus>1 and PulseStatus<5 then Light25.state = 1
	If PulseStatus>2 and PulseStatus<6 then Light26.state = 1
	If PulseStatus>4 and PulseStatus<8 then Light27.state = 1
	If PulseStatus>5 then Light28.state = 1
	If PulseStatus>4 and PulseStatus<8 then Light29.state = 1
	If PulseStatus>2 and PulseStatus<6 then Light30.state = 1
	If PulseStatus>1 and PulseStatus<5 then Light31.state = 1
	If PulseStatus<4 then Light32.state = 1
	If PulseStatus<>1 then Light33.state = 1
	If PulseStatus<>2 and PulseStatus<>8 then Light34.state = 1
	If PulseStatus<>3 and PulseStatus<>7 then Light35.state = 1
	If PulseStatus<>4 and PulseStatus<>6 then Light36.state = 1
	If PulseStatus<>5 then Light37.state = 1
	If PulseStatus=1 or PulseStatus=4 or PulseStatus=5 or PulseStatus=8 then Light38.state = 1
	If PulseStatus=3 or PulseStatus=4 or PulseStatus=7 or PulseStatus=8 then Light39.state = 1
	If PulseStatus=1 or PulseStatus=2 or PulseStatus=5 or PulseStatus=6 then Light40.state = 1	
	If PulseStatus=2 or PulseStatus=3 or PulseStatus=6 or PulseStatus=7 then Light41.state = 1	
	If PulseStatus=3 or PulseStatus=4 or PulseStatus=7 or PulseStatus=8 then Light42.state = 1	
	If PulseStatus=1 or PulseStatus=4 or PulseStatus=5 or PulseStatus=8 then Light43.state = 1
	If PulseStatus<>4 and PulseStatus<>8 then Light44.state = 1
	If PulseStatus<5 then Light45.state = 1
	If PulseStatus>4 then Light46.state = 1
	If PulseStatus>1 and PulseStatus<6 then Light47.state = 1
	If PulseStatus>3 and PulseStatus<8 then Light48.state = 1
	If PulseStatus=1 or PulseStatus=2 then Light49.state = 1
	If PulseStatus=3 or PulseStatus=4 then Light50.state = 1
	If PulseStatus=5 or PulseStatus=6 then Light51.state = 1
	If PulseStatus=7 or PulseStatus=8 then Light52.state = 1
	If PulseStatus<4 then Light53.state = 1
	If PulseStatus>1 and PulseStatus<5 then Light54.state = 1
	If PulseStatus>2 and PulseStatus<6 then Light55.state = 1
	If PulseStatus>3 and PulseStatus<7 then Light56.state = 1
	If PulseStatus>4 and PulseStatus<8 then Light57.state = 1
	If PulseStatus>5 then Light58.state = 1
	If PulseStatus>2 and PulseStatus<7 then Light59.state = 1
End Sub

Sub CheckTextFeature
	If TextFeature=1 then
		If RoundReady=1 and MultiRound=0 and RoundTime=0 then
			LED "NEXT ROUND READY", 1
			LED "SHOOT LIT SCOOP", 2
			TextWipe=50
			ff=1
		Else
			TextFeature=2
		End If
	End If
	
	If TextFeature=2 then
		If ContraptionReady=1 and MultiRound=0 and RoundTime=0 then
			LED "SHOOT LEFT SCOOP", 1
			LED "FOR CONTRAPTION", 2
			TextWipe=50
			ff=1
		Else
			TextFeature=3
			End If
	End If
	
	If TextFeature=3 then
		If CheeseReady=1 and MultiRound=0 and RoundTime=0 then
			LED "CHEESE IS LIT", 1
			LED "RIGHT SCOOP SHOT", 2
			TextWipe=50
			ff=1
		Else
			TextFeature=4
		End If
	End If
	
	If TextFeature=4 then
		If NickParkDone=4 and MultiRound=0 and RoundTime=0 then
			LED "MULTIBALL READY", 1
			LED "SHOOT BEDROOM", 2
			TextWipe=50
			ff=1
		Else
			TextFeature=5
		End If
	End If
	
	If TextFeature=5 then
		If MultiBalls>1 and MultiRound=1 and JackpotDone<2 then
			LED "SHOOT LIT SCOOP", 1
			LED "FOR JACKPOT", 2
			TextWipe=50
			ff=1
		Else
			TextFeature=6
		End If
	End If
	
	If TextFeature=6 then
		If MultiBalls>1 and MultiRound=1 and JackpotDone=2 then
			LED "BEDROOM AWARDS", 1
			LED "DOUBLE JACKPOT", 2
			TextWipe=50
			ff=1
		Else
			TextFeature=7
		End If
	End If

	If TextFeature=7 then
		If RoundDone=6 and MultiRound=0 and NickParkDone<4 then
			LED "WERERABBIT READY", 1
			LED "SHOOT BEDROOM", 2
			TextWipe=50
			ff=1
		Else
			TextFeature=8
		End If
	End If

	If TextFeature=8 then
		If MultiBalls>1 and MultiRound=2 and VegDone<5 then
			LED "GET ALL LIT", 1
			LED "VEGETABLES", 2
			TextWipe=50
			ff=1
		Else
			TextFeature=9
		End If
	End If
					
	If TextFeature=9 then
		If MultiBalls>1 and MultiRound=2 and VegDone=5 then
			LED "BOTH SCOOP SHOTS", 1
			LED "RELIGHT VEGGIES", 2
			TextWipe=50
			ff=1
		Else
			TextFeature=10
		End If
	End If
	
	If TextFeature=10 then
		If MultiBalls>1 and MultiRound=2 and VegDone=6 then
			LED "ONE MORE SCOOP", 1
			LED "FOR VEGETABLES", 2
			TextWipe=50
			ff=1
		Else
			TextFeature=1
		End If
	End If
								
	If TextFeature=101 then
		LED "SHOOT TOASTER", 1
		LED FormatNumber ((ToastVal), 0, -1, 0, -1), 2
		ff=1
	End If
	
	If TextFeature=102 then
		LED "POPS SCORE 100K", 1
		LED "TIME " & (INT(RoundTime/50)), 2
		ff=1
	End If
	
	If TextFeature=103 then
		LED "SHOOT TRAIN RAMP", 1
		LED "TIME " & (INT(RoundTime/50)), 2
		ff=1
	End If
	
	If TextFeature=104 then
		LED "MAKE TOOL SHOTS", 1
		LED "TIME " & (INT(RoundTime/50)), 2
		ff=1
	End If
	
	If TextFeature=105 then
		LED "SHOOT LEFT SCOOP", 1
		LED "TIME " & (INT(RoundTime/50)), 2
		ff=1
	End If
	
	If TextFeature=106 then
		LED "RIGHT SCOOP SHOT", 1
		LED "TIME " & (INT(RoundTime/50)), 2
		ff=1
	End If
	
	If TextFeature=107 then
		LED "HIT DROP TARGET", 1
		LED "TIME " & (INT(RoundTime/50)), 2
		ff=1
	End If
	
	If TextFeature=108 then
		LED "HIT SPOT TARGETS", 1
		LED "TIME " & (INT(RoundTime/50)), 2
		ff=1
	End If
			
End Sub

Sub PrintScore

	CR Credits+2
	If BallsLeft=0 then BR 13
	If BallsLeft>0 then
		If BallMax=5 then BR 7-BallsLeft
		If BallMax=3 then BR 5-BallsLeft
	End If

	If UseFlexDMD <> 0 Then 
		FlexDMD.LockRenderThread

		Dim i, lenScore
		Dim fmtScore
		fmtScore = Score
		If IsNumeric(fmtScore) = False Then fmtScore = "00"
		fmtScore = Right(Score,8)
		lenScore = Len(fmtScore)

		With FlexDMDScene
			For i = 8 To 1 Step - 1
				If i > lenScore  Then
					.GetImage("Score" & (i - 1)).Bitmap = FlexDMD.NewImage("", "VPX.DMDLargeSpace").Bitmap
				Else
					.GetImage("Score" & (i - 1)).Bitmap = FlexDMD.NewImage("", "VPX.DMDLarge" & Mid(fmtScore, -i + lenScore + 1, 1)).Bitmap
				End If
			Next
			If LenScore > 3 Then 
				.GetImage("Comma2").Visible = True
			Else
				.GetImage("Comma2").Visible = False
			End If
			If LenScore > 6 Then 
				.GetImage("Comma1").Visible = True
			Else
				.GetImage("Comma1").Visible = False
			End If
		End with

		FlexDMD.UnlockRenderThread
		If UseFlexDMD = 1 Then Exit Sub
	End If

	SetScore Score
	ff=Score
	gg=0
	hh=00

	gg=Int(ff/10000000)
	If gg>0 then hh=hh+((gg+1)*10648)
	ff=ff-(gg*10000000)
	gg=Int(ff/1000000)
	If gg>0 or hh>0 then hh=hh+((gg+12)*484)
	ff=ff-(gg*1000000)
	gg=Int(ff/100000)
	If gg>0 or hh>0 then hh=hh+((gg+1)*22)
	ff=ff-(gg*100000)
	gg=Int(ff/10000)
	If gg>0 or hh>0 then hh=hh+gg+1
	ff=ff-(gg*10000)
	Score1.SetValue(hh)

	gg=Int(ff/1000)
	If gg>0 or hh>0 then hh=(gg+12)*10648
	ff=ff-(gg*1000)
	gg=Int(ff/100)
	If gg>0 or hh>0 then hh=hh+((gg+1)*484)
	ff=ff-(gg*100)
	gg=Int(ff/10)
	hh=hh+((gg+1)*22)+1
	Score2.SetValue(hh)
End Sub
	
Sub AllLightsOn
	For f=1 to 58
		Lights(f).state = 1
	Next
	BoardAnim.imageA = "Board5"
'	Flasher.SetValue(6)
 	FlashStatus=-1
	Bumper1L.State=1
	Bumper1L1.State=1
	Bumper2L.State=1
	Bumper2L1.State=1
	Bumper3L.State=1
	Bumper3L1.State=1
End Sub

Sub AllLightsOff
	For f=1 to 58
		Lights(f).state = 0
	Next
	BoardAnim.imageA = "Board0"
	If FlashStatus=-1 then FlashStatus=4
	Bumper1L.State=0
	Bumper1L1.State=0
	Bumper2L.State=0
	Bumper2L1.State=0
	Bumper3L.State=0
	Bumper3L1.State=0
	light47.state = 0
End Sub

Sub LoadData
    Dim value
    Value = LoadValue("WandG", "HighScore(1)")
    If (Value <> "") then HighScore(1) = CDbl(Value) End If
    Value = LoadValue("WandG", "HighScore(2)")
    If (Value <> "") then HighScore(2) = CDbl(Value) End If
    Value = LoadValue("WandG", "HighScore(3)")
    If (Value <> "") then HighScore(3) = CDbl(Value) End If
    Value = LoadValue("WandG", "HighScore(4)")
    If (Value <> "") then HighScore(4) = CDbl(Value) End If
    Value = LoadValue("WandG", "Credits")
    If (Value <> "") then Credits = CDbl(Value) End If
    Value = LoadValue("WandG", "LastScore")
    If (Value <> "") then LastScore = CDbl(Value) End If
    Value = LoadValue("WandG", "ReplayScore")
    If (Value <> "") then ReplayScore = CDbl(Value) End If
    Value = LoadValue("WandG", "BallMax")
    If (Value <> "") then BallMax = CDbl(Value) End If
End Sub

Sub SaveData
    SaveValue "WandG", "HighScore(1)", HighScore(1)
    SaveValue "WandG", "HighScore(2)", HighScore(2)
    SaveValue "WandG", "HighScore(3)", HighScore(3)
    SaveValue "WandG", "HighScore(4)", HighScore(4)
    SaveValue "WandG", "Credits", Credits  
    SaveValue "WandG", "LastScore", LastScore
    SaveValue "WandG", "ReplayScore", ReplayScore
    SaveValue "WandG", "BallMax", BallMax
End Sub

Sub StopAllSound
	StopSound "V-3Bank1"
	StopSound "V-3Bank2"
	StopSound "V-3Bank3"
	StopSound "V-3Bank4"
	StopSound "V-ACS1End"
	StopSound "V-ACS1Score"
	StopSound "V-ACS1Start"
	StopSound "V-ACS2Failure"
	StopSound "V-ACS2Start"
	StopSound "V-ACS2Success"
	StopSound "V-AfterLaunch"
	StopSound "V-AGDO1Failure"
	StopSound "V-AGDO1Score"
	StopSound "V-AGDO1Start"
	StopSound "V-AGDO1Success"
	StopSound "V-AGDO2End"
	StopSound "V-AGDO2Score1"
	StopSound "V-AGDO2Score2"
	StopSound "V-AGDO2Score3"
	StopSound "V-AGDO2Start"
	StopSound "V-BallRestart"
	StopSound "V-BedEject1"
	StopSound "V-BedEject2"
	StopSound "V-BedEject3"
	StopSound "V-Bonus1"
	StopSound "V-Bonus2"
	StopSound "V-Bonus3"
	StopSound "V-Bonus4"
	StopSound "V-Bonus5"
	StopSound "V-BonusX"
	StopSound "V-Bumper1"
	StopSound "V-Bumper2"
	StopSound "V-Bumper3"
	StopSound "V-CheeseLit1"
	StopSound "V-CheeseLit2"
	StopSound "V-CheeseLit3"
	StopSound "V-CoinIn"
	StopSound "V-Contraption1"
	StopSound "V-Contraption2"
	StopSound "V-Contraption3"
	StopSound "V-Danger"
	StopSound "V-Demo2"
	StopSound "V-DemoStart"
	StopSound "V-DoubleJackpot"
	StopSound "V-ExtraBallStart"
	StopSound "V-GameOver1"
	StopSound "V-GameOver2"
	StopSound "V-GetCheese1"
	StopSound "V-GetCheese2"
	StopSound "V-Jackpot1"
	StopSound "V-Jackpot2"
	StopSound "V-Launch"
	StopSound "V-LeftUnlit1"
	StopSound "V-LeftUnlit2"
	StopSound "V-Midlane"
	StopSound "V-MultiballEject"
	StopSound "V-MultiballEnd"
	StopSound "V-MultiballStart"
	StopSound "V-NickPark1"
	StopSound "V-NickPark2"
	StopSound "V-NickPark3"
	StopSound "V-NickPark4"
	StopSound "V-NoCredit"
	StopSound "V-Outlane1"
	StopSound "V-Outlane2"
	StopSound "V-Outlane3"
	StopSound "V-ReturnLane"
	StopSound "V-RightUnlit1"
	StopSound "V-RightUnlit2"
	StopSound "V-SkillShot"
	StopSound "V-Sling1"
	StopSound "V-Sling2"
	StopSound "V-Spot1"
	StopSound "V-Spot2"
	StopSound "V-Spot3"
	StopSound "V-Tilt1"
	StopSound "V-Tilt2"
	StopSound "V-TimeLow"
	StopSound "V-Toaster"
	StopSound "V-ToastFailure"
	StopSound "V-ToastScore"
	StopSound "V-ToastStart"
	StopSound "V-Tools1"
	StopSound "V-Tools2"
	StopSound "V-TopLane"
	StopSound "V-Train"
	StopSound "V-TrainBonus"
	StopSound "V-TWT1End"
	StopSound "V-TWT1Score"
	StopSound "V-TWT1Start"
	StopSound "V-TWT2End"
	StopSound "V-TWT2Score"
	StopSound "V-TWT2Start"
	StopSound "V-VegScoop1"
	StopSound "V-VegScoop2"
	StopSound "V-VegScore1"
	StopSound "V-VegScore2"
	StopSound "V-VegScore3"
	StopSound "V-VegScore4"
	StopSound "V-VegScore5"
	StopSound "V-WererabbitEnd"
	StopSound "V-WererabbitLaunch"
	StopSound "V-WererabbitStart"
End Sub

Sub AttractTwo
	DMDCount=DMDCount+1
	If DMDCount>900 then DMDCount=1
	AttractCount=AttractCount+1
	If AttractCount>1500 then AttractCount=1
		
	If AttractCount=-230 then PlaySound "BootUp"
	If AttractCount=-205 then
		AllLightsOn
		Score1.SetValue(122705)
		Score2.SetValue(122705)
		CR 1
		BR 1
		LED "GAME NO. ED-70", 1
		LED "WALLACE & GROMIT", 2
	End If
	If AttractCount=-200 then AllLightsOff	
	If AttractCount=-150 then
		LED "INITIALIZING", 1
		LED "", 2
		AllLightsOn
		PlaySound "V-Tools1"
		SavePost.IsDropped=False
		GateOpen
		PlaySound "Click"
		ToasterUp
		BedAnim.heighttop = 180
		CR 13
		BR 11
	End If
	If AttractCount=-145 then BedAnim.heighttop = 230
	If AttractCount=-150 or AttractCount=-100 then
		Score1.SetValue(133860)
		Score2.SetValue(133860)
	End If
	If AttractCount=-145 or AttractCount=-95 then
		Score1.SetValue(145015)
		Score2.SetValue(145015)
	End If
	If AttractCount=-140 or AttractCount=-90 then
		Score1.SetValue(156170)
		Score2.SetValue(156170)
	End If
	If AttractCount=-135 or AttractCount=-85 then
		Score1.SetValue(167325)
		Score2.SetValue(167325)
	End If
	If AttractCount=-130 or AttractCount=-80 then
		Score1.SetValue(178480)
		Score2.SetValue(178480)
	End If
	If AttractCount=-125 or AttractCount=-75 then
		Score1.SetValue(189635)
		Score2.SetValue(189635)
	End If
	If AttractCount=-120 or AttractCount=-70 then
		Score1.SetValue(200790)
		Score2.SetValue(200790)
	End If
	If AttractCount=-115 or AttractCount=-65 then
		Score1.SetValue(211945)
		Score2.SetValue(211945)
	End If
	If AttractCount=-110 or AttractCount=-60 then
		Score1.SetValue(223100)
		Score2.SetValue(223100)
	End If
	If AttractCount=-105 or AttractCount=-55 then
		Score1.SetValue(234255)
		Score2.SetValue(234255)
	End If
	If AttractCount=-55 then BedAnim.heighttop = 180
	If AttractCount=-50 then
		LED "INITIALIZE", 1
		LED "COMPLETE", 2
		AllLightsOff
		SavePost.IsDropped=True
		GateClose
		PlaySound "Drop"
		ToasterDown
		Score1.SetValue(0):SetScore 0
		Score2.SetValue(0)
		CR 13
		BR 12
		BedAnim.heighttop = 131
		LoadData
	End If
	If AttractCount=-5 then PlayMusic "W&G-GameOver.mp3"
	If AttractCount=1 then
		LED "GAME OVER", 1
		If Credits=0 then LED "INSERT COIN", 2
		If Credits>0 then LED "PRESS START", 2
	End If
	If AttractCount=151 then
		LED "5 - CREDIT", 1
		LED "1 - START", 2
	End If
	If AttractCount=301 then
		LED "9 - THREE-BALL", 1
		LED "0 - FIVE-BALL", 2
	End If
	If AttractCount=451 then
		LED "REPLAY AT", 1
		LED FormatNumber(ReplayScore,0,-1,0,-1), 2
	End If
	If AttractCount=601 then
		LED "PRESS FLIPPER", 1
		LED "FOR RULES", 2
	End If
	If AttractCount=751 then
		LED "GAME DESIGN BY", 1
		LED "DAVE SANDERS", 2
	End If
	If AttractCount=901 then
		LED "PETER SALLIS", 1
		LED "AS WALLACE", 2
	End If
	If AttractCount=1051 then
		LED "MUSIC BY", 1
		LED "JULIAN NOTT", 2
	End If
	If AttractCount=1201 then
		LED "DEDICATED TO", 1
		LED "NICK PARK", 2
	End If
	If AttractCount=1351 then
		LED "CRACKING", 1
		LED "PINBALL GROMIT", 2
	End If
	If DMDCount=1 then
		Score=LastScore
		PrintScore
	End If
	If DMDCount=301 then
		Score=HighScore(1)
		PrintScore
		CR 12
		BR 7
	End If
	If DMDCount=451 then
		Score=HighScore(2)
		PrintScore
		CR 12
		BR 8
	End If
	If DMDCount=601 then
		Score=HighScore(3)
		PrintScore
		CR 12
		BR 9
	End If
	If DMDCount=751 then
		Score=HighScore(4)
		PrintScore
		CR 12
		BR 10
	End If
End Sub

Sub Table1_MusicDone()
	If BallActive=1 and MachineTilt=0 then
		If InPlunger=1 then
			If NickParkDone=4 or RoundDone=6 then PlayMusic "W&G-MultiballReady.MP3"
			If NickParkDone<4 and RoundDone<6 then PlayMusic "W&G-BallWaiting.MP3"
		End If
		If InPlunger=0 and MultiRound=0 and RoundTime=0 then
			If NickParkDone=4 or RoundDone=6 then PlayMusic "W&G-MultiballReady.MP3"
			If NickParkDone<4 and RoundDone<6 then PlayMusic "W&G-MainTheme.MP3"
		End If
		If MultiRound=1 then PlayMusic "W&G-Multiball.MP3"
		If MultiRound=2 then PlayMusic "W&G-FinalMode.MP3"
	End If
	If RoundTime>0 then 
		If CurrentRound=1 or CurrentRound=6 then PlayMusic "W&G-Mode1.MP3"
		If CurrentRound=2 or CurrentRound=3 then PlayMusic "W&G-Mode2.MP3"
		If CurrentRound=4 or CurrentRound=5 then PlayMusic "W&G-Mode3.MP3"
	End If	
End Sub

Sub LeftTrig_Hit()
	If ActiveBall.X>240 then ActiveBall.VelX=ActiveBall.VelX-6
	If ActiveBall.X<220 then ActiveBall.VelX=ActiveBall.VelX+6
	If ActiveBall.Y>780 then ActiveBall.VelY=ActiveBall.VelY-6
End Sub

Sub RightTrig_Hit()
	If ActiveBall.X<850 then ActiveBall.VelX=ActiveBall.VelX+6
	If ActiveBall.Y>1165 then ActiveBall.VelY=ActiveBall.VelY-6
End Sub
	
Sub GateOpen
	LeftGate.rotatetoend
	RightGate.rotatetoend
	LeftGatePrim.roty = 50
    RightGatePrim.roty = -50
End Sub
	
Sub GateClose
	LeftGate.rotatetostart
	RightGate.rotatetostart
	LeftGatePrim.roty = 0
    RightGatePrim.roty = 0
End Sub	

Sub FlashTimer_Timer()
	If FlashStatus>0 then
		FlashStatus=FlashStatus-1
	'	If FlashStatus<8 then Flasher.SetValue(FlashStatus)
	End If

	If StartPause>65 and StartPause<315 then
		AllLightsOff
		For f=1 to 58
			g=Int(Rnd*250)
			If g>StartPause-65 then Lights(f).state = 1
		Next
	End If
	
	For f=1 to 7
		If LightPulse(f)>0 then LightPulse(f)=LightPulse(f)-1
	Next
	If LightPulse(1)>0 then
		For f=1 to 3
			WAndGLight(f).state = 0
			If LightPulse(1)/2=Int(LightPulse(1)/2) then WAndGLight(f).state = 1
		Next
	End If
	If LightPulse(2)>0 then
		For f=1 to 4
			NickParkLight(f).state = 0
			If LightPulse(2)/2=Int(LightPulse(2)/2) then NickParkLight(f).state = 1
		Next
	End If
	If LightPulse(3)>0 then
		For f=1 to 3
			ToolLight(f).state = 0
			If LightPulse(3)/2=Int(LightPulse(3)/2) then ToolLight(f).state = 1
		Next
	End If
	If LightPulse(4)>0 then
		For f=1 to 6
			ContraptionLight(f).state = 0
			If LightPulse(4)/2=Int(LightPulse(4)/2) then ContraptionLight(f).state = 1
		Next
	End If
	If LightPulse(5)>0 then
		For f=1 to 3
			SpotLight(f).state = 0
			If LightPulse(5)/2=Int(LightPulse(5)/2) then SpotLight(f).state = 1
		Next
	End If
	If LightPulse(6)>0 then
		For f=1 to 3
			SpotLight(f+3).state = 0
			If LightPulse(6)/2=Int(LightPulse(6)/2) then SpotLight(f+3).state = 1
		Next
	End If
	If LightPulse(7)>0 then
		For f=1 to 3
			SpotLight(f+6).state = 0
			If LightPulse(7)/2=Int(LightPulse(7)/2) then SpotLight(f+6).state = 1
		Next
	End If
End Sub

Sub EndOfBall
	BonusPause=BonusPause-1
	If BonusPause=350 then
		If SPReady<>2 then PlayMusic "W&G-BallEnd.MP3"
		If SPReady=2 then SPReady=3
		BonusTotal=((TrainDone*10000)+(CheeseDone*30000)+(ContraptionDone*50000))*BonusMult
	End If
	If BonusPause=290 then
		If TrainDone=1 then LED TrainDone&" RAMP", 1
		If TrainDone<>1 then LED TrainDone&" RAMPS", 1
		LED FormatNumber ((TrainDone*10000), 0, -1, 0, -1), 2
	End If
	If BonusPause=250 then
		If CheeseDone=1 then LED CheeseDone&" CHEESE", 1
		If CheeseDone<>1 then LED CheeseDone&" CHEESES", 1
		If CheeseDone=0 then LED "00", 2
		If CheeseDone>0 then LED FormatNumber ((CheeseDone*30000), 0, -1, 0, -1), 2
	End If
	If BonusPause=210 then
		If ContraptionDone=1 then LED ContraptionDone&" CONTRAPTION", 1
		If ContraptionDone<>1 then LED ContraptionDone&" CONTRAPTIONS", 1
		If ContraptionDone=0 then LED "00", 2
		If ContraptionDone>0 then LED FormatNumber ((ContraptionDone*50000), 0, -1, 0, -1), 2
	End If
	If BonusPause=170 then
		LED "BONUS", 1
		LED BonusMult&"X", 2
	End If
	If BonusPause=130 then
		LED "TOTAL", 1
		LED FormatNumber (BonusTotal, 0, -1, 0, -1), 2
		AddScore(BonusTotal)
		If BonusTotal<300000 then PlaySound "V-Bonus1"
		If BonusTotal>299999 and BonusTotal<600000 then PlaySound "V-Bonus2"
		If BonusTotal>599999 and BonusTotal<900000 then PlaySound "V-Bonus3"
		If BonusTotal>899999 and BonusTotal<1200000 then PlaySound "V-Bonus4"
		If BonusTotal>1199999 then PlaySound "V-Bonus5"
	End If
	If BonusPause=0 then
		If BallsLeft>1 or ShootAgain>0 then
			BallActive=0
			BallSave=0
			BonusPause=99999999
			StartPause=25
			NextBall
		End If
	End If

	If BonusPause=-1 then
		LED "", 1
		LED "", 2
		HighActive=0
		gg=Score MOD 100
		If Score>HighScore(4) then
			HighActive=1
			PlayMusic "W&G-HighScore.MP3"
			LED "PLAYER 1 HAS", 1
			LED "A HIGH SCORE", 2
			PlaySound "HighScore"
		Else
			BonusPause=-200
		End If
		LastScore=Score
		If Score>ReplayScore then
			If BallMax=3 then ReplayScore=ReplayScore+5000000
			If BallMax=5 then ReplayScore=ReplayScore+10000000
		Else
			If ReplayScore>10000000 and BallMax=3 then ReplayScore=ReplayScore-1000000
			If ReplayScore>20000000 and BallMax=5 then ReplayScore=ReplayScore-2000000
		End If
		If BallMax=5 and ReplayScore>98000000 then ReplayScore=98000000
		If BallMax=3 and ReplayScore>49000000 then ReplayScore=49000000
		If BallMax=5 and ReplayScore<20000000 then ReplayScore=20000000
		If BallMax=3 and ReplayScore<10000000 then ReplayScore=10000000
	End If
	
	If BonusPause=-100 then
		h=0
		If Score>HighScore(1) then
			h=1
			HighScore(4)=HighScore(3)
			HighScore(3)=HighScore(2)
			HighScore(2)=HighScore(1)
			HighScore(1)=Score
			LED "HIGH SCORE 1", 1
			LED "AWARD 2 CREDITS", 2
			Score=0
		End If
		If Score>HighScore(2) then
			HighScore(4)=HighScore(3)
			HighScore(3)=HighScore(2)
			HighScore(2)=Score
			LED "HIGH SCORE 2", 1
			LED "AWARD 1 CREDIT", 2
			Score=0
		End If
		If Score>HighScore(3) then
			HighScore(4)=HighScore(3)
			HighScore(3)=Score
			LED "HIGH SCORE 3", 1
			LED "AWARD 1 CREDIT", 2
			Score=0
		End If
		If Score>HighScore(4) then
			HighScore(4)=Score
			LED "HIGH SCORE 4", 1
			LED "AWARD 1 CREDIT", 2
			Score=0
		End If
	End If
	If BonusPause=-130 then
		Score=LastScore
		Credits=Credits+h
		CreditPause=1
		CR Credits+2
		PlaySound "Knock"
	End If
		
	If BonusPause=-200 then
		h=(INT (RND*10))*10
		LED "", 1
		LED "", 2
	End If
	If (BonusPause<-220 and BonusPause>-297 and BonusPause/3=Int(BonusPause/3)) or BonusPause=-306 or BonusPause=-316 or BonusPause=-326 then
		h=h+10
		If h>99 then h=0
		If UseFlexDMD <> 0 Then 
			FlexDMD.LockRenderThread
			Dim i
			With FlexDMDScene
				' clear score display
				For i = 8 To 1 Step - 1
					.GetImage("Score" & (i - 1)).Bitmap = FlexDMD.NewImage("", "VPX.DMDLargeSpace").Bitmap
				Next
				.GetImage("Comma2").Visible = False
				.GetImage("Comma1").Visible = False
				'show match
				.GetImage("Score" & 4).Bitmap = FlexDMD.NewImage("", "VPX.DMDLarge0").Bitmap
				.GetImage("Score" & 1).Bitmap = FlexDMD.NewImage("", "VPX.DMDLarge0").Bitmap
				.GetImage("Score" & 2).Bitmap = FlexDMD.NewImage("", "VPX.DMDLarge" & (h / 10)).Bitmap
				.GetImage("Score" & 5).Bitmap = FlexDMD.NewImage("", "VPX.DMDLarge" & (gg / 10)).Bitmap
			End with
			FlexDMD.UnlockRenderThread
		End If
		BR 17
		If UseFlexDMD <> 1 Then
			Score2.SetValue(484*(h/10+1)+22)
			Score1.SetValue(484*(gg/10+1)+22)
		End If
	End If
	If BonusPause=-326 then
		If gg=h then
			PlaySound "Knock"
			CreditPause=1
		End If
		If CreditPause=0 then SaveData
	End If
	If BonusPause=-400 then
		If HighActive=0 and GotReplay=0 and gg<>h then
			PlaySound "V-GameOver1"
		Else
			PlaySound "V-GameOver2"
		End If
		If HighActive=0 and GotReplay=0 then PlayMusic "W&G-GameOver.MP3"
		SlamTilt=0
		BallActive=0
		AttractCount=-1
		BallsLeft=0
		DMDCount=-1
	End If
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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/6, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/6, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
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

 '**********************
'Flipper Shadows
'***********************
Sub RealTime_Timer
  lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
BallShadowUpdate
If FlashStatus>0 then FlasherLight.state = 1:FlasherLight1.state = 1 Else FlasherLight.state = 0:FlasherLight1.state = 0:end if
If SavePost.isdropped = False then SavePostLight.state = 1 else SavePostLight.state = 0
End Sub


Sub BallShadowUpdate()
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
		if BOT(b).z < 24 Then BOT(b).velz = BOT(b).velz /20
    Next	
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
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub Table1_Exit()
	If UseFlexDMD > 0 then
		If IsObject(FlexDMD) Then 
			FlexDMD.Show = False
			FlexDMD.Run = False
			FlexDMD = NULL
		End if
	End if
	If IsObject(Controller) Then Controller.Stop
End Sub

'**********************************************************************************************************
' FlexDMD code - scutters
'**********************************************************************************************************

Dim FlexDMD
Dim FlexDMDScene 

Sub FlexDMD_Init() 

	Dim i, j

	Select Case UseFlexDMD  
	Case 1, 2
	Case Else
		UseFlexDMD = 0
		Exit Sub
	End Select


	On Error Resume Next
	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
	On Error GoTo 0

	If IsObject(FlexDMD) Then

		FlexDMD.RenderMode = 2
		FlexDMD.Width = 128
		FlexDMD.Height = 32
		FlexDMD.Clear = True
		FlexDMD.GameName = "Wallace"
		FlexDMD.TableFile = Table1.Filename & ".vpx"
		FlexDMD.Run = True

		Dim FlexDMDFont

		FlexDMD.LockRenderThread

		Set FlexDMDScene = FlexDMD.NewGroup("ScoreScene")

		With FlexDMDScene

			.AddActor FlexDMD.NewImage("Back", "FlexDMD.Resources.dmds.black.png")
			If UseFlexDMDBackgroundImage = 1 Then .AddActor FlexDMD.NewImage("Wallpaper", "VPX.DMDWallpaper")

			' score text images
			For i = 7 To 0 Step -1
				Select Case i
				Case 7,6
					.AddActor FlexDMD.NewImage("Score" & i, "VPX.DMDLargeSpace")
					.GetImage("Score" & i).SetAlignedPosition 126 - ((i + 1) * 14) - 12,0,0
				Case 5,4,3
					.AddActor FlexDMD.NewImage("Score" & i, "VPX.DMDLargeSpace")
					.GetImage("Score" & i).SetAlignedPosition 126 - ((i + 1) * 14) - 6,0,0
				Case 2,1,0
					.AddActor FlexDMD.NewImage("Score" & i, "VPX.DMDLargeSpace")
					.GetImage("Score" & i).SetAlignedPosition 126 - ((i + 1) * 14),0,0
				End Select
			Next

			.AddActor FlexDMD.NewImage("Comma1", "VPX.DMDLargeComma")
			.GetImage("Comma1").SetAlignedPosition 30,0,0
			.GetImage("Comma1").Visible = False
			.AddActor FlexDMD.NewImage("Comma2", "VPX.DMDLargeComma")
			.GetImage("Comma2").SetAlignedPosition 78,0,0
			.GetImage("Comma2").Visible = False

			' credit,ball info etc lines
			FlexDMDFont = FlexDMD.NewFont("FlexDMD.Resources.udmd-f4by5.fnt", RGB(255,106,0),  vbBlack, 0)
			.AddActor(FlexDMD.NewLabel("Ball", FlexDMDFont, Space(13)))
			.GetLabel("Ball").SetAlignedPosition 64, 26, 0
			.AddActor(FlexDMD.NewLabel("Credits", FlexDMDFont, Space(13)))
			.GetLabel("Credits").SetAlignedPosition 0, 26, 0

			If FlexDMDCalloutStlye <> 1 Then
			' info text lines
				For j = 0 To 1
					For i = 0 To 15
						If j = 0 Then
							.AddActor FlexDMD.NewImage("Text1_" & i, "VPX.DMD32")
							.GetImage("Text1_" & i).SetAlignedPosition 8 + (i * 7),3,0
						Else
							.AddActor FlexDMD.NewImage("Text2_" & i, "VPX.DMD32")
							.GetImage("Text2_" & i).SetAlignedPosition 8 + (i * 7),14,0
						End If
					Next
				Next
				FlexDMDCalloutStlye = 0
			Else
				FlexDMDFont = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbYellow,  vbBlack, 0)
				.AddActor(FlexDMD.NewLabel("Text1", FlexDMDFont, Space(16)))
				.GetLabel("Text1").SetAlignedPosition 0, 26, 0
				.AddActor(FlexDMD.NewLabel("Text2", FlexDMDFont, Space(16)))
				.GetLabel("Text2").SetAlignedPosition 65, 26, 0
				If UseFlexDMDBackgroundImage = 1 Then
					.GetImage("Wallpaper").SetBounds 0,0,128,26
				End If
			End If

		End With
		
		FlexDMD.Stage.AddActor FlexDMDScene
		FlexDMD.UnlockRenderThread
		
	Else
		
		UseFlexDMD = 0	
	End If

End Sub

Function CentreString(inputString, totalLength)
    Dim strLength, paddingSize, leftPadding, rightPadding
    
    ' Get the length of the input string
    strLength = Len(inputString)
    
    ' Calculate total padding needed
    If strLength < totalLength Then
        paddingSize = totalLength - strLength
        
        ' Calculate left and right padding sizes
        leftPadding = Int(paddingSize / 2)
        rightPadding = paddingSize - leftPadding
        
        ' Create padded string
        CentreString = Space(leftPadding) & inputString & Space(rightPadding)
    Else
        ' If input string is longer than or equal to total length, return it as is
        CentreString = inputString
    End If
End Function