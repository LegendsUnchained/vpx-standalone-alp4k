Option Explicit
 Randomize

Const cGameName="elektra",UseSolenoids=2,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_Flipperup",SFlipperOff="fx_Flipperdown"
Const SCoin="coin"

LoadVPM "00990400", "Bally.VBS", 1.2
Dim DesktopMode: DesktopMode = Table1.ShowDT


'******************* Options *********************
' DMD/Backglass Controller Setting
Const cController = 3		'0=Use value defined in cController.txt, 1=VPinMAME, 2=UVP server, 3=B2S server
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
			Set Controller = CreateObject("VPinMAME.Controller")
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
SolCallback(6) = "vpmSolSound ""Knocker"","
SolCallback(7) = "bsTrough.SolOut"
SolCallBack(10) = "bsTSaucer.SolOut"
SolCallBack(11) = "bsRSaucer.SolOut"
SolCallback(12) = "dtDrop.SolDropUp"
SolCallBack(13) = "bsLSaucer.SolOut"
SolCallback(17) = "vpmSolDiverter OutGate, ""Gate"", Not"
SolCallback(19) = "vpmNudge.SolGameOn"
SolCallback(sLLFlipper)   = "leftFlipperSol"
SolCallback(sLRFlipper)   = "rightFlipperSol"

'Solenoid Controlled toys
'**********************************************************************************************************
Sub leftFlipperSol(enabled)
vpmSolFlipper LeftFlipperLower, nothing, (Not Controller.Switch(8)) And enabled
vpmSolFlipper LeftFlipper, LeftFlipperUpper, Controller.Switch(8) And enabled
End Sub

Sub rightFlipperSol(enabled)
vpmSolFlipper RightFlipperLower, nothing, (Not Controller.Switch(8)) And enabled
vpmSolFlipper RightFlipper, RightFlipperUpper, Controller.Switch(8) And enabled
End Sub


Sub FlipperTimer_Timer()
Primitiveflipper.RotY = OutGate.Currentangle
End Sub


'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsTSaucer, bsRSaucer, bsLSaucer, dtDrop

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Elecktra (Bally 1981)"&chr(13)&"You Suck"
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

	vpmNudge.TiltSwitch = 15
	vpmNudge.Sensitivity = 1
	vpmNudge.TiltObj = Array(LeftSlingshot,RightSlingshot)

	Set bsTrough = New cvpmBallStack
	bsTrough.InitSw 0,1,2,0,0,0,0,0
	bsTrough.InitKick BallRelease, 55, 4
	bsTrough.InitExitSnd "BallRelease","solenoid"
	bsTrough.Balls = 2


	Set bsTSaucer = New cvpmBallStack
	bsTSaucer.InitSaucer SW04,4,90,8
	bsTSaucer.KickForceVar = 2
	bsTSaucer.InitExitSnd "Popper","solon"


	Set bsRSaucer = New cvpmBallStack
	bsRSaucer.InitSaucer SW03,3,230,10
	bsRSaucer.KickForceVar = 2
	bsRSaucer.InitExitSnd "Popper","solon"


	Set bsLSaucer = New cvpmBallStack
	bsLSaucer.InitSaucer SW08,8,5,20
	bsLSaucer.InitExitSnd "Popper","solon"
	bsLSaucer.AddBall 0

	SW08.CreateBall
	SW06.CreateBall
	SW06.Kick 270,1


	Set dtDrop = New cvpmDropTarget
	dtDrop.InitDrop Array(SW18,SW19),Array(18,19)
	dtDrop.InitSnd "DTDrop","DTReset"
	
	'Noreturn.isdropped = true
End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	if keycode = LeftFlipperKey then Controller.Switch(5) = true
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	if keycode = LeftFlipperKey then Controller.Switch(5) = false
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
End Sub

'**********************************************************************************************************
' SWITCH HANDLING


'Kickers
Sub Drain_Hit  : bsTrough.AddBall Me : bsTrough.entrysol_on : End Sub
Sub SW03_Hit   : bsRSaucer.AddBall 0 : bsRSaucer.entrysol_on : End Sub
Sub SW04_Hit   : bsTSaucer.AddBall 0 : bsTSaucer.entrysol_on : End Sub
Sub SW08_Hit   : bsLSaucer.AddBall 0 : bsLSaucer.entrysol_on : End Sub



'Main Playfield
'Wire triggers
Sub SW12_Hit   : Controller.Switch(12) =1 : playsound"rollover" : End Sub 
Sub SW12_Unhit : Controller.Switch(12) =0:End Sub
Sub SW13_Hit   : Controller.Switch(13) =1 : playsound"rollover" : End Sub 
Sub SW13_Unhit : Controller.Switch(13) =0:End Sub
Sub SW14_Hit   : Controller.Switch(14) =1 : playsound"rollover" : End Sub 
Sub SW14_Unhit : Controller.Switch(14) =0:End Sub

'Spinners
Sub Spinner2_Spin : vpmTimer.PulseSwitch 25, 0, 0 : playsound"fx_spinner" : End Sub
Sub Spinner1_Spin : vpmTimer.PulseSwitch 26, 0, 0 : playsound"fx_spinner" : End Sub


'Stand Up Targets
Sub SW29_Hit : vpmTimer.PulseSwitch 29, 0, 0 : End Sub
Sub SW30_Hit : vpmTimer.PulseSwitch 30, 0, 0 : End Sub
Sub SW31_Hit : vpmTimer.PulseSwitch 31, 0, 0 : End Sub
Sub SW33_Hit : vpmTimer.PulseSwitch 33, 0, 0 : End Sub
Sub SW34_Hit : vpmTimer.PulseSwitch 34, 0, 0 : End Sub
Sub SW35_Hit : vpmTimer.PulseSwitch 35, 0, 0 : End Sub
Sub SW36_Hit : vpmTimer.PulseSwitch 36, 0, 0 : End Sub
Sub SW37_Hit : vpmTimer.PulseSwitch 37, 0, 0 : End Sub




'Upper Playfield
'Drop Targets
Sub SW18_Hit   : dtDrop.Hit 1 : End Sub
Sub SW19_Hit   : dtDrop.Hit 2 : End Sub
'Stand up Targets
Sub SW20_Hit   : vpmTimer.PulseSwitch 20, 0, 0 : End Sub
Sub SW21U_Hit  : vpmTimer.PulseSwitch 21, 0, 0 : End Sub
Sub SW22U_Hit  : vpmTimer.PulseSwitch 22, 0, 0 : End Sub
Sub SW23U_Hit  : vpmTimer.PulseSwitch 23, 0, 0 : End Sub
'Star Triggers
Sub SW24_Hit   : Controller.Switch(24) =1 : playsound"rollover" : End Sub 
Sub SW24_Unhit : Controller.Switch(24) =0:End Sub
Sub SW27A_Hit  : Controller.Switch(27) =1 : playsound"rollover" : End Sub 
Sub SW27A_Unhit: Controller.Switch(27) =0:End Sub
Sub SW27B_Hit  : Controller.Switch(27) =1 : playsound"rollover" : End Sub 
Sub SW27B_Unhit: Controller.Switch(27) =0:End Sub
Sub SW27C_Hit  : Controller.Switch(27) =1 : playsound"rollover" : End Sub 
Sub SW27C_Unhit: Controller.Switch(27) =0:End Sub
Sub SW28A_Hit  : Controller.Switch(28) =1 : playsound"rollover" : End Sub 
Sub SW28A_Unhit: Controller.Switch(28) =0:End Sub
Sub SW28B_Hit  : Controller.Switch(28) =1 : playsound"rollover" : End Sub 
Sub SW28B_Unhit: Controller.Switch(28) =0:End Sub
Sub SW28C_Hit  : Controller.Switch(28) =1 : playsound"rollover" : End Sub 
Sub SW28C_Unhit: Controller.Switch(28) =0:End Sub
Sub SW38A_Hit  : Controller.Switch(38) =1 : playsound"rollover" : End Sub 
Sub SW38A_Unhit: Controller.Switch(38) =0:End Sub
Sub SW38B_Hit  : Controller.Switch(38) =1 : playsound"rollover" : End Sub 
Sub SW38B_Unhit: Controller.Switch(38) =0:End Sub
'Sling Shot
Sub SW32_Slingshot : vpmTimer.PulseSwitch 23, 0, 0 : playsound "slingshot" : End Sub


'Mini Playfield
'Stand Up Targets
Sub SW07_Hit   : vpmTimer.PulseSwitch 7, 0, 0 : End Sub
Sub SW21L_Hit  : vpmTimer.PulseSwitch 21, 0, 0 : End Sub
Sub SW22L_Hit  : vpmTimer.PulseSwitch 22, 0, 0 : End Sub
Sub SW23L_Hit  : vpmTimer.PulseSwitch 23, 0, 0 : End Sub
Sub SW29L_Hit  : vpmTimer.PulseSwitch 29, 0, 0 : End Sub
Sub SW30L_Hit  : vpmTimer.PulseSwitch 30, 0, 0 : End Sub
Sub SW31L_Hit  : vpmTimer.PulseSwitch 31, 0, 0 : End Sub
'Star Triggers
Sub SW25L_Hit  : Controller.Switch(25) =1 : playsound"rollover" : End Sub 
Sub SW25L_Unhit: Controller.Switch(25) =0:End Sub
Sub SW26L_Hit  : Controller.Switch(26) =1 : playsound"rollover" : End Sub 
Sub SW26L_Unhit: Controller.Switch(26) =0:End Sub


'GI Lights
 Dim N1,O1
 N1=0:O1=0:
 Set LampCallback=GetRef("UpdateMultipleLamps")
 Sub UpdateMultipleLamps
 	N1=Controller.Lamp(63) 
 	If N1<>O1 Then
 		If N1 Then 
			dim mm
			For each mm in GI2:mm.State = 0: Next 'GI Mini PF
			dim xx
			For each xx in GI:xx.State = 1: Next 'GI Main/Upper PF
		Else
			For each mm in GI2:mm.State = 1: Next
			For each xx in GI:xx.State = 0: Next
		End If
 	O1=N1
	End If
 End Sub




'Map lights to an array
'**********************************************************************************************************

'Playfield
Set Lights(1) = L1
Set Lights(2) = L2
Set Lights(3) = L3
set lights(6) = L6
lights(7) = array(L7,L7a,L7b)
set lights(8) = L8
set lights(10) = L10
set lights(12) = L12
Set Lights(17) = L17
Set Lights(18) = L18
Set Lights(19) = L19
set lights(22) = L22
lights(23) = array(L23,L23a,L23b)
set lights(24) = L24
set lights(26) = L26
set lights(28) = L28
Set Lights(33) = L33
Set Lights(34) = L34
Set Lights(35) = L35
set lights(38) = L38
lights(39) = array(L39,L39a,L39b)
set lights(40) = L40
set lights(42) = L42
Set Lights(43) = L43  'Shoot Again PF and Backglass
set lights(44) = L44
Set Lights(49) = L49
Set Lights(50) = L50
set lights(54) = L54
set lights(55) = L55
set lights(59) = L59
set lights(60) = L60

'Upper Playfield
set lights(4) = L4
set lights(5) = L5
set lights(9) = L9
set lights(14) = L14
set lights(15) = L15
set lights(20) = l20
set lights(21) = L21
set lights(25) = L25
set lights(30) = L30
set lights(31) = L31
set lights(36) = L36
set lights(37) = L37
set lights(46) = L46
set lights(52) = L52
lights(53) = array(L53,L53a)
set lights(56) = L56
set lights(62) = L62

'Mini playfield
set lights(65) = L65
set lights(66) = L66
set lights(67) = L67
set lights(68) = L68
set lights(69) = L69
set lights(70) = L70
set lights(81) = L81
set lights(82) = L82
set lights(83) = L83
set lights(84) = L84
set lights(85) = L85
set lights(86) = L86
set lights(97) = L97
set lights(98) = L98
set lights(99) = L99
set lights(100) = L100
set lights(101) = L101
set lights(102) = L102
set lights(113) = L113
set lights(114) = L114
set lights(115) = L115
set lights(118) = L118

'Hide All Backglass Lights when desktop Mode
If DesktopMode = True Then
dim xxx
For each xxx in GL:xxx.visible = 1: Next
else
For each xxx in GL:xxx.visible = 0: Next
end if

'Set Lights(13) = 'Ball In Play
'Set Lights(61) = 'Tilt
'Set Lights(27) = 'Match
'Set Lights(29) = 'High Score
'Set Lights(45) = 'Game Over

'**********************************************************************************************************
' Backglass Light Displays (7 digit 7 segment displays)
Dim Digits(34)

' 1st Player
Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)
Digits(6) = Array(LED70,LED71,LED72,LED73,LED74,LED75,LED76)

' 2nd Player
Digits(7) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(8) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(9) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(10) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(11) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(12) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)
Digits(13) = Array(LED140,LED141,LED142,LED143,LED144,LED145,LED146)

' 3rd Player
Digits(14) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(15) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(16) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(17) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(18) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(19) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)
Digits(20) = Array(LED210,LED211,LED212,LED213,LED214,LED215,LED216)

' 4th Player
Digits(21) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(22) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(23) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(24) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(25) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(26) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)
Digits(27) = Array(LED280,LED281,LED282,LED283,LED284,LED285,LED286)

' Credits
Digits(28) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(29) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)
' Balls
Digits(30) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(31) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)

'Elektra Units
Digits(32) = Array(LED88,LED79,LED97,LED98,LED89,LED78,LED87)
Digits(33) = Array(LED109,LED107,LED118,LED119,LED117,LED99,LED108)

Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		If DesktopMode = True Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 34) then
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
'Bally Elektra
'added by Inkochnito
Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700,400,"Elektra - DIP switches"
		.AddChk 7,10,180,Array("Match feature", &H08000000)'dip 28
		.AddChk 205,10,115,Array("Credits display", &H04000000)'dip 27
		.AddFrame 2,30,190,"Maximum credits",&H03000000,Array("10 credits",0,"15 credits", &H01000000,"25 credits", &H02000000,"40 credits", &H03000000)'dip 25&26
		.AddFrame 2,106,190,"4-5-6 arrow",&H00000020,Array("not in memory",0,"in memory",&H00000020)'dip 6
		.AddFrame 2,152,190,"Left blue targets",&H00000040,Array("not in memory",0,"in memory",&H00000040)'dip 7
		.AddFrame 2,198,190,"Blue lights feature",&H00000080,Array("conservative 1-2-3-4-5",0,"liberal 1-3-5",&H00000080)'dip 8
		.AddFrame 2,248,190,"Minimum time needed",&H00002000,Array("10 Elektra-units",0,"6 Elektra-units",&H00002000)'dip 14
		.AddFrame 2,298,190,"Kicker during timeplay",&H00004000,Array("ball stays in kicker",0,"ball kicked out",&H00004000)'dip 15
		.AddFrame 2,348,190,"No more Elektra-units",32768,Array("game ends",0,"game goes on",32768)'dip 16
		.AddFrame 205,30,190,"Balls per game",&HC0000000,Array ("2 balls",&HC0000000,"3 balls",0,"4 balls",&H80000000,"5 balls",&H40000000)'dip 31&32
		.AddFrame 205,106,190,"Frequenty",&H00100000,Array("50Hz",0,"60Hz",&H00100000)'dip 21
		.AddFrame 205,152,190,"Centre red targets",&H00200000,Array("not in memory",0,"in memory",&H00200000)'dip 22
		.AddFrame 205,198,190,"Top special feature",&H00400000,Array("conservative",0,"liberal",&H00400000)'dip 23
		.AddFrame 205,248,190,"Special memory",&H00800000,Array("Off",0,"On",&H00800000)'dip 24
		.AddFrame 205,298,190,"Replay limit",&H10000000,Array("no limit",0,"1 replay per game",&H10000000)'dip 29
		.AddFrame 205,348,190,"Attract sound",&H20000000,Array("off",0,"on",&H20000000)'dip 30
		.AddLabel 50,400,300,20,"Set selftest position 18 and 19 to 03 for the best gameplay."
		.AddLabel 50,420,300,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub
Set vpmShowDips = GetRef("editDips")

'***************************************************************************************************
'VPX Function CODE
'***************************************************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSwitch 39, 0, 0 
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
	vpmTimer.PulseSwitch 40, 0, 0 
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
    Vol = Csng(BallVel(ball) ^2 / 15000)
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 180 Then
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

 '**********************
'Flipper Shadows
'***********************
Sub RealTime_Timer
  lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
BallShadowUpdate
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
        If BOT(b).Z > 20 and BOT(b).Z < 180 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 90
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
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
