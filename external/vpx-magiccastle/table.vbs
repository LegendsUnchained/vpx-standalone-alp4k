Option Explicit
 Randomize

Const cGameName = "mcastle",UseSolenoids=1,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_Flipperup",SFlipperOff="fx_Flipperdown"
Const SCoin="coin",cCredits=""

LoadVPM "01200300","zac.vbs",3.10
Dim DesktopMode: DesktopMode = Table1.ShowDT

'******************* Options *********************
' DMD/Backglass Controller Setting
Const cController = 1		'0=Use value defined in cController.txt, 1=VPinMAME, 2=UVP server, 3=B2S server
'*************************************************

'*************************************************************
Dim cNewController
Sub LoadVPM(VPMver, VBSfile, VBSver)
	Dim FileObj, ControllerFile, TextStr

	On Error Resume Next
	If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
	ExecuteGlobal GetTextFile(VBSfile)
	If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description

	cNewController = 1
	If cController = 0 then
		Set FileObj=CreateObject("Scripting.FileSystemObject")
		If Not FileObj.FolderExists(UserDirectory) then 
			Msgbox "Visual Pinball\User directory does not exist. Defaulting to vPinMame"
		ElseIf Not FileObj.FileExists(UserDirectory & "cController.txt") then
			Set ControllerFile=FileObj.CreateTextFile(UserDirectory & "cController.txt",True)
			ControllerFile.WriteLine 1: ControllerFile.Close
		Else
			Set ControllerFile=FileObj.GetFile(UserDirectory & "cController.txt")
			Set TextStr=ControllerFile.OpenAsTextStream(1,0)
			If (TextStr.AtEndOfStream=True) then
				Set ControllerFile=FileObj.CreateTextFile(UserDirectory & "cController.txt",True)
				ControllerFile.WriteLine 1: ControllerFile.Close
			Else
				cNewController=Textstr.ReadLine: TextStr.Close
			End If
		End If
	Else
		cNewController = cController
	End If

	Select Case cNewController
		Case 1
			Set Controller = CreateObject("B2S.Server")
			If Err Then MsgBox "Can't Load VPinMAME." & vbNewLine & Err.Description
			If VPMver>"" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
			If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
		Case 2
			Set Controller = CreateObject("UltraVP.BackglassServ")
		Case 3,4
			Set Controller = CreateObject("B2S.Server")
	End Select
	On Error Goto 0
End Sub
'*************************************************************

'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1)="vpmSolFlipper Flipper1,Nothing,"
SolCallback(4)="vpmSolSound ""knocker"","
SolCallback(5)="dtL.SolDropUp"
SolCallback(6)="dtO.SolDropUp" 
SolCallback(7)="dtO.SolHit 1,"
SolCallback(21)="dtC.SolDropUp"
SolCallback(22)="dtR.SolDropUp"
SolCallback(24)="bsTrough.SolOut"
SolCallback(sLLFlipper)="vpmSolFlipper LeftFlipper,nothing,"
SolCallback(sLRFlipper)="vpmSolFlipper RightFlipper,RightFlipper1,"

'Primitive Flippers
Sub FlipperTimer_Timer
	FlipperT12.roty = LeftFlipper.currentangle  '+ 180
	FlipperT10.roty = RightFlipper.currentangle '+ 45
	FlipperT11.roty = RightFlipper1.currentangle '+ 45
End Sub

'Initiate Table
'**********************************************************************************************************

Dim bsTrough,dtL,dtR,dtC,dtO

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
         If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine="Magic Castle - Zaccaria, 1984" & vbNewLine & "You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
   		Controller.SolMask(0)=0
      vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
		Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1
	vpmNudge.TiltSwitch=10
	vpmNudge.Sensitivity=5
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)
	
	Set bsTrough=New cvpmBallStack
    bsTrough.InitSw 0,16,0,0,0,0,0,0 
    bsTrough.InitKick sw101,180,2 
    bsTrough.InitExitSnd "ballrelease", "Solenoid"
    bsTrough.Balls=1 
	
	Set dtL=New cvpmDropTarget
	dtL.InitDrop Array(sw28,sw29,sw30,sw31),Array(28,29,30,31)
	dtL.InitSnd "DTDrop","DTReset"

	Set dtO=New cvpmDropTarget
	dtO.InitDrop sw17,17
	dtO.InitSnd "DTDrop","DTReset"

	Set dtR=New cvpmDropTarget
    dtR.InitDrop Array(sw37,sw38,sw39),Array(37,38,39)
	dtR.InitSnd "DTDrop","DTReset"

	Set dtC=New cvpmDropTarget
	dtC.InitDrop Array(sw44,sw45,sw46,sw47),Array(44,45,46,47)
	dtC.InitSnd "DTDrop","DTReset"
End Sub

 '**********************************************************************************************************
'Plunger code
'**********************************************************************************************************
 Sub Table1_KeyDown(ByVal keycode)
	If KeyCode=RightFlipperKey Then Controller.Switch(23)=1
	If vpmKeyDown(KeyCode) Then Exit Sub
	If keycode=PlungerKey Then Plunger.Pullback:playsound"plungerpull"
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If KeyCode=RightFlipperKey Then Controller.Switch(23)=0
	If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode=PlungerKey Then Plunger.Fire:playsound"plunger"
End Sub
'**********************************************************************************************************

 ' Switches
'**********************************************************************************************************

 ' Drain hole
Sub Drain_Hit:playsound"drain":bsTrough.addball me:End Sub

'wire triggers										
Sub sw18_Hit:Controller.Switch(18)=1 : playsound"rollover" : End Sub
Sub sw18_unHit:Controller.Switch(18)=0:End Sub
Sub sw19_Hit:Controller.Switch(19)=1 : playsound"rollover" : End Sub
Sub sw19_unHit:Controller.Switch(19)=0:End Sub
Sub sw22_Hit:Controller.Switch(22)=1 : playsound"rollover" : End Sub
Sub sw22_unHit:Controller.Switch(22)=0:End Sub
Sub sw24_Hit:Controller.Switch(24)=1 : playsound"rollover" : End Sub
Sub sw24_unHit:Controller.Switch(24)=0:End Sub
Sub sw48_Hit:Controller.Switch(48)=1 : playsound"rollover" : End Sub
Sub sw48_unHit:Controller.Switch(48)=0:End Sub



 ' Droptargets
Sub sw28_Hit:dtL.Hit 1:End Sub
Sub sw29_Hit:dtL.Hit 2:End Sub
Sub sw30_Hit:dtL.Hit 3:End Sub
Sub sw31_Hit:dtL.Hit 4:End Sub

Sub sw43_Hit:dtC.Hit 1:End Sub	
Sub sw44_Hit:dtC.Hit 2:End Sub
Sub sw45_Hit:dtC.Hit 3:End Sub
Sub sw46_Hit:dtC.Hit 4:End Sub

Sub sw17_Hit:dtO.Hit 1:End Sub

Sub sw37_Hit:dtR.Hit 1:End Sub
Sub sw38_Hit:dtR.Hit 2:End Sub
Sub sw39_Hit:dtR.Hit 3:End Sub

'Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 41 : playsound"fx_bumper1": End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 42 : playsound"fx_bumper1": End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 40 : playsound"fx_bumper1": End Sub

'Stand Up Targets
Sub sw25_Hit:vpmTimer.PulseSw 25:End Sub
Sub sw26_Hit:vpmTimer.PulseSw 26:End Sub
Sub sw27_Hit:vpmTimer.PulseSw 27:End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:End Sub
Sub sw33_Hit:vpmTimer.PulseSw 33:End Sub

Sub sw47_Hit:vpmTimer.PulseSw 47:End Sub


'2nd floor Triggers
Sub sw34_Hit:vpmTimer.PulseSw 34:End Sub
Sub sw35_Hit:vpmTimer.PulseSw 35:End Sub
Sub sw36_Hit:vpmTimer.PulseSw 36:End Sub

'Generic Sounds
Sub Trigger1_Hit:Playsound"Ball Drop":End Sub ' ball drop
Sub Trigger2_Hit:Playsound"Ball Drop":End Sub ' ball drop
Sub Trigger3_Hit:playsound"Wire Ramp": End Sub ' wire ramp 

'**********************************************************************************************************
'Map lights to an array
'**********************************************************************************************************
Set Lights(1)=l1
Set Lights(2)=l2
Set Lights(3)=l3
Set Lights(4)=l4
Set Lights(5)=l5
Set Lights(8)=l8
Set Lights(9)=l9
Set Lights(11)=l11
Set Lights(12)=l12
Set Lights(14)=l14
Set Lights(15)=l15
Set Lights(16)=l16
Set Lights(18)=l18
Set Lights(19)=l19
Set Lights(21)=l21
Set Lights(22)=l22
Set Lights(23)=l23
Set Lights(24)=l24
Set Lights(25)=l25
Set Lights(26)=l26
Set Lights(28)=l28
Set Lights(29)=l29
Set Lights(30)=l30
Set Lights(32)=l32
Set Lights(34)=l34
Set Lights(35)=l35
Set Lights(36)=l36
Set Lights(38)=l38
Set Lights(39)=l39
Set Lights(40)=l40
Set Lights(41)=l41
Set Lights(42)=l42
Set Lights(43)=l43
Set Lights(44)=l44
Set Lights(45)=l45
Set Lights(47)=l47
Set Lights(48)=l48
Set Lights(49)=l49
Set Lights(51)=l51
Set Lights(53)=l53
Set Lights(55)=l55
Set Lights(57)=l57
Set Lights(58)=l58
Set Lights(59)=l59
Set Lights(61)=l61
Set Lights(63)=l63
Set Lights(64)=l64
Set Lights(65)=l65
Set Lights(68)=l68

'Backglass



'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
Dim Digits(40)

Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)
Digits(6) = Array(LED70,LED71,LED72,LED73,LED74,LED75,LED76)
Digits(7) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(8) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(9) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(10) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(11) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(12) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)
Digits(13) = Array(LED140,LED141,LED142,LED143,LED144,LED145,LED146)
Digits(14) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(15) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(16) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(17) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(18) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(19) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)
Digits(20) = Array(LED210,LED211,LED212,LED213,LED214,LED215,LED216)
Digits(21) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(22) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(23) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(24) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(25) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(26) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)
Digits(27) = Array(LED280,LED281,LED282,LED283,LED284,LED285,LED286)
Digits(28) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(29) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)
Digits(30) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(31) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)
Digits(32) = Array(LED88,LED79,LED97,LED98,LED89,LED78,LED87)
Digits(33) = Array(LED109,LED107,LED118,LED119,LED117,LED99,LED108)
Digits(34) = Array(LED137,LED128,LED139,LED147,LED138,LED127,LED129)
Digits(35) = Array(LED158,LED149,LED167,LED168,LED159,LED148,LED157)
Digits(36) = Array(LED179,LED177,LED188,LED189,LED187,LED169,LED178)
Digits(37) = Array(LED207,LED198,LED209,LED217,LED208,LED197,LED199)
Digits(38) = Array(LED228,LED219,LED237,LED238,LED229,LED218,LED227)
Digits(39) = Array(LED249,LED247,LED258,LED259,LED257,LED239,LED248)

Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		If DesktopMode = True Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 40) then
				For Each obj In Digits(num)
					If chg And 1 Then obj.State = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			else

			end if
		next
		end if
end if
End Sub


'**********************************************************************************************************
'**********************************************************************************************************

'*****GI Lights On
dim xx

For each xx in GI:xx.State = 1: Next

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 21
    PlaySound "left_slingshot", 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 20
    PlaySound "right_slingshot",0,1,-0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub



' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
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
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
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

' The sound is played using the VOL, PAN and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the PAN function will change the stereo position according
' to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub
