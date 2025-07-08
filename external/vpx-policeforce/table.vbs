Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const CGameName="Polic_L4", UseSolenoids=2,UseLamps=1,UseSync=1,SSolenoidOn="SolenoidOn",SSolenoidOff="SolenoidOff",SFlipperOn="FlipperUp",SFlipperOff="FlipperDown",SCoin="Coin"

LoadVPM "00990300","S11.VBS",3

' Sub LoadVPM(VPMver,VBSfile,VBSver)
' 	On Error Resume Next
' 		If ScriptEngineMajorVersion<5 Then MsgBox"VB Script Engine 5.0 or higher required"
' 		ExecuteGlobal GetTextFile(VBSfile)
' 		If Err Then MsgBox"Unable to open "&VBSfile&". Ensure that it is in the same folder as this table."&vbNewLine&Err.Description
' 		Set Controller=CreateObject("VPinMAME.Controller")
' 		If Err Then MsgBox"Unable to load VPinMAME."&vbNewLine&Err.Description
' 		If VPMver>"" Then
' 			If Controller.Version<VPMver Or Err Then MsgBox"This table requires VPinMAME ver "&VPMver&" or higher."
' 		End If
' 		If VPinMAMEDriverVer<VBSver Or Err Then MsgBox"This table requires "&VBSFile&" ver "&VBSver&" or higher."
' 	On Error Goto 0
BlueCar.TransZ=0
if Police.showDT = False then ramp001.visible = False:ramp002.visible = False

SolCallback(1)="bsTrough.SolIn"
SolCallback(2)="SolTLEject"
SolCallback(3)="SolBallDiverter"
SolCallback(4)="SolRightBank"
SolCallback(5)="SolLREject"
SolCallback(6)="SolMidBank"
SolCallback(7)="vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(8)="SolTREject"
SolCallback(9)="vpmFlasher TakeHighest,"
 SolCallback(10)="SolGI" 'Playfield GI
 SolCallback(11)="SolBackbox" 'Backbox GI
SolCallback(13)="vpmFlasher Array(Z95,Z96),"
SolCallBack(14)="vpmFlasher Z97,"
SolCallback(15)="SolCar" 'CAR MOTOR
'SolCallback(16)="SolRelay" 'CAR DIRECTION
SolCallback(17)="vpmSolSound SoundFX(""Jet"",DOFContactors),"
SolCallback(18)="vpmSolSound SoundFX(""Slingshot"",DOFContactors),"
SolCallback(19)="vpmSolSound SoundFX(""Jet"",DOFContactors),"
SolCallback(20)="vpmSolSound SoundFX(""Slingshot"",DOFContactors),"
SolCallback(21)="vpmSolSound SoundFX(""Jet"",DOFContactors),"
SolCallback(22)="bsTrough.SolOut"
SolCallBack(25)="SolSharkFlasher"
SolCallBack(26)="SolCrocFlasher"
SolCallBack(27)="SolRatFlasher"
SolCallBack(28)="vpmFlasher Array(WeaselFlasher,WeaselFlasherb),"
SolCallBack(29)="Sol29"
SolCallBack(30)="vpmFlasher Z31,"
'SolCallBack(31)="vpmFlasher LeftDomeFlasher,"
'SolCallBack(32)="vpmFlasher RightDomeFlasher,"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
	 	LLFlipper.RotateToEnd
		PlaySound SoundFX("FlipperUp",DOFFlippers)
    Else
		LLFlipper.RotateToStart
		PlaySound SoundFX("FlipperDown",DOFFlippers)
    End If
End Sub
 
Sub SolRFlipper(Enabled)
    If Enabled Then
        LRFlipper.RotateToEnd
		PlaySound SoundFX("FlipperUp",DOFFlippers)
    Else
        LRFlipper.RotateToStart
		PlaySound SoundFX("FlipperFown",DOFFlippers)
	End If
End Sub

 Sub SolSharkFlasher(Enabled)
 	If Enabled Then
 		LeftFlasher.blenddisablelighting = 0.5 
		LeftFlasherL.State=1
        LeftFlasherLa.State=1
 	Else
 		LeftFlasher.blenddisablelighting =	0
        LeftFlasherL.State=0
        LeftFlasherLa.State=0
 	End If
 End Sub
 
Sub SolCrocFlasher(Enabled)
 	If Enabled Then
 		TopFlasher.blenddisablelighting = 0.5
		TopFlasherL.State=1
        TopFlasherLa.State=1
 	Else
 		TopFlasher.blenddisablelighting = 0
        TopFlasherL.State=0
        TopFlasherLa.State=0
 	End If
 End Sub
 
 Sub SolRatFlasher(Enabled)
 	If Enabled Then
 		MidFlasher.blenddisablelighting = 0.5
 		MidFlasherL.State=1
        MidFlasherLa.State=1
 	Else
 		MidFlasher.blenddisablelighting = 0	
        MidFlasherL.State=0
        MidFlasherLa.State=0
 	End If
 End Sub

 Dim xx
 Sub SolGI(Enabled)
 	If Enabled Then
for each xx in GILights: xx.state = 0:Next
Police.colorgradeimage = "Colorgrade_4"
 		GI1.State=0
 		GI2.State=0
 		Z112.State=0
 		Z113.State=0
 		Z114.State=0
 		Z115.State=0
	Else
for each xx in GILights: xx.state = 1: Next
Police.colorgradeimage = "Colorgrade_8"
		GI1.State=1
 		GI2.State=1
 		Z112.State=1
 		Z113.State=1
 		Z114.State=1
 		Z115.State=1
	End If
 End Sub
 
Sub SolBackbox(Enabled)
 	EMReel8.SetValue ABS(Enabled)
	EMReel15.SetValue ABS(Enabled)
End Sub
 
Sub Sol29(Enabled)
 	If Enabled Then
 		EMReel6.SetValue 1
 	Else
 		EMReel6.SetValue 0
 	End If
End Sub

Sub SolBallDiverter(Enabled)
	If Enabled Then
		If Diverter.IsDropped Then
 			Controller.Switch(36)=0
 			Diverter.IsDropped=0
 			Wall33.IsDropped=0
 		Else
 			Controller.Switch(36)=1
			Diverter.IsDropped=1
			Wall33.IsDropped=1
		End If
 	End If
End Sub

Dim R1,R2,R3
R1=0:R2=0:R3=0

Sub SolTREject(Enabled):If Enabled Then:PlaySound"FlapOpen":End If:R2=RND*3:TREject.Kick 290,10+R2:Controller.Switch(15)=0:End Sub
Sub SolLREject(Enabled):If Enabled Then:PlaySound"FlapOpen":End If:R3=RND*1:Kicker1.Kick 300,18-R3:Controller.Switch(23)=0:End Sub
Sub SolTLEject(Enabled):If Enabled Then:PlaySound"FlapOpen":End If:R1=RND*7:TLE.Kick 63,7+R1:Controller.Switch(24)=0:End Sub

Sub SolMidBank(Enabled)
	MidTop.IsDropped=0
	MidMid.IsDropped=0
	MidBot.IsDropped=0
	Controller.Switch(25)=0
	Controller.Switch(26)=0
	Controller.Switch(27)=0
End Sub

Sub SolRightBank(Enabled)
	RTop.IsDropped=0
	RMid.IsDropped=0
	RBot.IsDropped=0
	Controller.Switch(20)=0
	Controller.Switch(21)=0
	Controller.Switch(22)=0
End Sub

Dim bsTrough,mCar

Sub Police_init()
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Police Force"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 1
		.DIP(0)=&H00' Set dipswitch to USA
        .Games(cGameName).Settings.Value("sound")=1
		'.PuPHide = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval:PinMAMETimer.Enabled=1
	vpmNudge.TiltSwitch = 1
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array("Left", "Right", "LeftJetBumper", "RightJetBumper", "LowerJetBumper")


 	Set bsTrough=New cvpmBallStack
	bsTrough.InitSw 10,12,11,0,0,0,0,0
	bsTrough.InitKick BallRelease,65,8
	bsTrough.InitExitSnd "BallRel","Solenoid"
 	bsTrough.KickForceVar=2
 	bsTrough.Balls=2
 
   	Set mCar=New cvpmMech
 	mCar.MType=vpmMechOneDirSol+vpmMechStopEnd+vpmMechLinear
 	mCar.Sol1=15 'Motor
 	mCar.Sol2=16 'Direction
 	mCar.Length=240 'Approximately 4 seconds
 	mCar.Steps=150
 	mCar.AddSw 31,0,1 'Car at Back of table (UP)
 	mCar.AddSw 32,98,99 'Car at ball release area to ramp (BOTTOM)
	mCar.Callback=GetRef("MoveCar")
 	mCar.Start
 
	vpmMapLights AllLights
	 
End Sub

Sub Police_KeyDown(ByVal KeyCode)
 	If KeyCode=RightFlipperKey Then Controller.Switch(57)=1
	If KeyCode=LeftFlipperKey Then Controller.Switch(58)=1
	If KeyDownHandler(KeyCode) Then Exit Sub
	If KeyCode=PlungerKey Then Plunger.Pullback
End Sub

Sub Police_KeyUp(ByVal KeyCode)
	If KeyCode=RightFlipperKey Then Controller.Switch(57)=0
	If KeyCode=LeftFlipperKey Then Controller.Switch(58)=0
	If KeyUpHandler(KeyCode) Then Exit Sub
	If KeyCode=PlungerKey Then Plunger.Fire:PlaySound"Plunger"
End Sub

Sub Drain_Hit:bsTrough.AddBall Me:End Sub
Sub ShooterLane_Hit:Controller.Switch(14)=1:End Sub
Sub ShooterLane_UnHit:Controller.Switch(14)=0:End Sub
Sub TREject_Hit:Controller.Switch(15)=1:End Sub
Sub LeftTop_Hit:vpmTimer.PulseSw 17:End Sub
Sub LeftMid_Hit:vpmTimer.PulseSw 18:End Sub
Sub LeftBot_Hit:vpmTimer.PulseSw 19:End Sub
Sub RTop_Hit:RTop.IsDropped=1:PlaySound SoundFX("DropTarget",DOFDropTargets):Controller.Switch(20)=1:End Sub
Sub RMid_Hit:RMid.IsDropped=1:PlaySound SoundFX("DropTarget",DOFDropTargets):Controller.Switch(21)=1:End Sub
Sub RBot_Hit:RBot.IsDropped=1:PlaySound SoundFX("DropTarget",DOFDropTargets):Controller.Switch(22)=1:End Sub
Sub Kicker1_Hit:Controller.Switch(23)=1:End Sub
Sub TLE_Hit:Controller.Switch(24)=1:End Sub
Sub MidTop_Hit:MidTop.IsDropped=1:PlaySound SoundFX("DropTarget",DOFDropTargets):Controller.Switch(25)=1:End Sub
Sub MidMid_Hit:MidMid.IsDropped=1:PlaySound SoundFX("DropTarget",DOFDropTargets):Controller.Switch(26)=1:End Sub
Sub MidBot_Hit:MidBot.IsDropped=1:PlaySound SoundFX("DropTarget",DOFDropTargets):Controller.Switch(27)=1:End Sub
Sub RtRampEnter_Hit:Controller.Switch(34)=1:End Sub
Sub RtRampEnter_unHit:Controller.Switch(34)=0:End Sub
Sub TLTarget_Hit:vpmTimer.PulseSw 35:End Sub
Sub RightRampToCar_Hit:Controller.Switch(37)=1:End Sub
Sub RightRampToCar_unHit:Controller.Switch(37)=0:End Sub
Sub RRToWire_Hit:Controller.Switch(38)=1:End Sub
Sub RRToWire_unHit:Controller.Switch(38)=0:End Sub
Sub Spinner_Spin:vpmTimer.PulseSw 41:End Sub
Sub MidRampEnter_Hit:Controller.Switch(45)=1:End Sub
Sub MidRampEnter_unHit:Controller.Switch(45)=0:End Sub
Sub MidRampMake_Hit:Controller.Switch(46)=1:End Sub
Sub MidRampMake_unHit:Controller.Switch(46)=0:End Sub
Sub RightOutLane_Hit:Controller.Switch(49)=1:PlaySound"Gate2":End Sub
Sub RightOutLane_UnHit:Controller.Switch(49)=0:End Sub
Sub LeftOutLane_Hit:Controller.Switch(50)=1:PlaySound"Gate2":End Sub
Sub LeftOutLane_UnHit:Controller.Switch(50)=0:End Sub
Sub LeftReturnLane_Hit:Controller.Switch(51)=1:PlaySound"Gate2":End Sub
Sub LeftReturnLane_UnHit:Controller.Switch(51)=0:End Sub
Sub RightReturnLane_Hit:Controller.Switch(52)=1:PlaySound"Gate2":End Sub
Sub RightReturnLane_UnHit:Controller.Switch(52)=0:End Sub
Sub G_Hit:Controller.Switch(54)=1:PlaySound"Gate2":End Sub
Sub G_UnHit:Controller.Switch(54)=0:End Sub
Sub U_Hit:Controller.Switch(55)=1:PlaySound"Gate2":End Sub
Sub U_UnHit:Controller.Switch(55)=0:End Sub
Sub N_Hit:Controller.Switch(56)=1:PlaySound"Gate2":End Sub
Sub N_UnHit:Controller.Switch(56)=0:End Sub
Sub LeftJetBumper_Hit:vpmTimer.PulseSw 60:Me.TimerEnabled=1:End Sub
Sub LeftJetBumper_Timer:Me.TimerEnabled=0:End Sub
Sub RightJetBumper_Hit:vpmTimer.PulseSw 61:Me.TimerEnabled=1:End Sub	
Sub RightJetBumper_Timer:Me.TimerEnabled=0:End Sub
Sub LowerJetBumper_Hit:vpmTimer.PulseSw 62:Me.TimerEnabled=1:End Sub	
Sub LowerJetBumper_Timer:Me.TimerEnabled=0:End Sub
Sub Left_Slingshot:vpmTimer.PulseSw 63:End Sub
Sub Right_Slingshot:vpmTimer.PulseSw 64:End Sub

 Sub Trigger1_Hit
 	If Controller.Switch(24) Then
 		If ActiveBall.VelY<-6 Then ActiveBall.VelY=-6
 	End If
 End Sub
 
 Sub DisplayTimer_Timer
	Dim ChgLED,II,Num,Chg,Stat,Obj
	ChgLED=Controller.ChangedLEDs(&Hffffffff,&Hffffffff)
	If Not IsEmpty(ChgLED) Then
		For II=0 To UBound(ChgLED)
			Num=ChgLED(II,0):Chg=ChgLED(II,1):Stat=ChgLED(II,2)
			For Each Obj In Digits(Num)
				If Chg And 1 Then Obj.State=Stat And 1
				Chg=Chg\2:Stat=Stat\2
			Next
		Next
	End If

if L23.state=1 then Jackpot.visible =1
if L23.state=0 then Jackpot.visible =0
if Z42.state=1 then BMan.visible =1
if Z42.state=0 then BMan.visible =0
if Z43.state=1 then GMan.visible =1
if Z43.state=0 then GMan.visible =0
if Z44.state=1 then Oman.visible =1
if Z44.state=0 then Oman.visible =0
if Z45.state=1 then YMan.visible =1
if Z45.state=0 then YMan.visible =0
if Z46.state=1 then Wman.visible =1
if z46.state=0 then Wman.visible =0

End Sub 

Dim Digits(39)
Digits(0)=Array(Light385,Light386,Light387,Light388,Light389,Light390,Light391,Light392)
Digits(1)=Array(Light393,Light394,Light395,Light396,Light397,Light398,Light399,Light400)
Digits(2)=Array(Light401,Light402,Light403,Light404,Light405,Light406,Light407,Light408)
Digits(3)=Array(Light409,Light410,Light411,Light412,Light413,Light414,Light415,Light416)
Digits(4)=Array(Light417,Light418,Light419,Light420,Light421,Light422,Light423,Light424)
Digits(5)=Array(Light425,Light426,Light427,Light428,Light429,Light430,Light431,Light432)
Digits(6)=Array(Light433,Light434,Light435,Light436,Light437,Light438,Light439,Light440)
Digits(7)=Array(Light441,Light442,Light443,Light444,Light445,Light446,Light447,Light448)
Digits(8)=Array(Light1,Light2,Light3,Light4,Light5,Light6,Light7,Light8,Light9,Light10,Light11,Light12,Light13,Light14,Light15,Light16)
Digits(9)=Array(Light17,Light18,Light19,Light20,Light21,Light22,Light23,Light24,Light25,Light26,Light27,Light28,Light29,Light30,Light31,Light32)
Digits(10)=Array(Light33,Light34,Light35,Light36,Light37,Light38,Light39,Light40,Light41,Light42,Light43,Light44,Light45,Light46,Light47,Light48)
Digits(11)=Array(Light49,Light50,Light51,Light52,Light53,Light54,Light55,Light56,Light57,Light58,Light59,Light60,Light61,Light62,Light63,Light64)
Digits(12)=Array(Light65,Light66,Light67,Light68,Light69,Light70,Light71,Light72,Light73,Light74,Light75,Light76,Light77,Light78,Light79,Light80)
Digits(13)=Array(Light81,Light82,Light83,Light84,Light85,Light86,Light87,Light88,Light89,Light90,Light91,Light92,Light93,Light94,Light95,Light96)
Digits(14)=Array(Light97,Light98,Light99,Light100,Light101,Light102,Light103,Light104,Light105,Light106,Light107,Light108,Light109,Light110,Light111,Light112)
Digits(15)=Array(Light113,Light114,Light115,Light116,Light117,Light118,Light119,Light120,Light121,Light122,Light123,Light124,Light125,Light126,Light127,Light128)
Digits(16)=Array(Light129,Light130,Light131,Light132,Light133,Light134,Light135,Light136,Light137,Light138,Light139,Light140,Light141,Light142,Light143,Light144)
Digits(17)=Array(Light145,Light146,Light147,Light148,Light149,Light150,Light151,Light152,Light153,Light154,Light155,Light156,Light157,Light158,Light159,Light160)
Digits(18)=Array(Light161,Light162,Light163,Light164,Light165,Light166,Light167,Light168,Light169,Light170,Light171,Light172,Light173,Light174,Light175,Light176)
Digits(19)=Array(Light177,Light178,Light179,Light180,Light181,Light182,Light183,Light184,Light185,Light186,Light187,Light188,Light189,Light190,Light191,Light192)
Digits(20)=Array(Light193,Light194,Light195,Light196,Light197,Light198,Light199,Light200,Light201,Light202,Light203,Light204,Light205,Light206,Light207,Light208)
Digits(21)=Array(Light209,Light210,Light211,Light212,Light213,Light214,Light215,Light216,Light217,Light218,Light219,Light220,Light221,Light222,Light223,Light224)
Digits(22)=Array(Light225,Light226,Light227,Light228,Light229,Light230,Light231,Light232,Light233,Light234,Light235,Light236,Light237,Light238,Light239,Light240)
Digits(23)=Array(Light241,Light242,Light243,Light244,Light245,Light246,Light247,Light248,Light249,Light250,Light251,Light252,Light253,Light254,Light255,Light256)
Digits(24)=Array(Light257,Light258,Light259,Light260,Light261,Light262,Light263,Light264)
Digits(25)=Array(Light265,Light266,Light267,Light268,Light269,Light270,Light271,Light272)
Digits(26)=Array(Light273,Light274,Light275,Light276,Light277,Light278,Light279,Light280)
Digits(27)=Array(Light281,Light282,Light283,Light284,Light285,Light286,Light287,Light288)
Digits(28)=Array(Light289,Light290,Light291,Light292,Light293,Light294,Light295,Light296)
Digits(29)=Array(Light297,Light298,Light299,Light300,Light301,Light302,Light303,Light304)
Digits(30)=Array(Light305,Light306,Light307,Light308,Light309,Light310,Light311,Light312)
Digits(31)=Array(Light313,Light314,Light315,Light316,Light317,Light318,Light319,Light320)
Digits(32)=Array(Light321,Light322,Light323,Light324,Light325,Light326,Light327,Light328)
Digits(33)=Array(Light329,Light330,Light331,Light332,Light333,Light334,Light335,Light336)
Digits(34)=Array(Light337,Light338,Light339,Light340,Light341,Light342,Light343,Light344)
Digits(35)=Array(Light345,Light346,Light347,Light348,Light349,Light350,Light351,Light352)
Digits(36)=Array(Light353,Light354,Light355,Light356,Light357,Light358,Light359,Light360)
Digits(37)=Array(Light361,Light362,Light363,Light364,Light365,Light366,Light367,Light368)
Digits(38)=Array(Light369,Light370,Light371,Light372,Light373,Light374,Light375,Light376)
Digits(39)=Array(Light384,Light377,Light378,Light379,Light380,Light381,Light382,Light383)

Set LampCallback=GetRef("UpdateMultipleLamps")

Dim O1,N1,O2,N2,O3,N3,O4,N4,O5,N5,O60,N60,O61,N61,O62,N62
O1=0:O2=0:O3=0:O4=0:O5=0:N1=0:N2=0:N3=0:N4=0:N5=0:O60=0:N60=0:O61=0:N61=0:O62=0:N62=0
Dim O37,N37,O38,N38,O39,N39,O40,N40,N47,N48
O37=0:N37=0:O38=0:N38=0:O39=0:N39=0:O40=0:N40=0:N47=0:N48=0
Dim OldTotal,NewTotal,CarPos
 OldTotal=0:NewTotal=0

'63=Left Dome Lamp
'64=Right Dome Lamp

Sub UpdateMultipleLamps
 	N1=Controller.Lamp(1)
	N2=Controller.Lamp(2)
	N3=Controller.Lamp(3)
	N4=Controller.Lamp(4)
	N5=Controller.Lamp(5)
 	N37=Controller.Lamp(37)
	N38=Controller.Lamp(38)
	N39=Controller.Lamp(39)
	N40=Controller.Lamp(40)
	N47=Controller.Lamp(47)
	N48=Controller.Lamp(48)
	N60=Controller.Lamp(60)
 	N61=Controller.Lamp(61)
 	N62=Controller.Lamp(62)
 	If N1<>O1 Then
 		If N1 Then
 			EMReel1.SetValue 1
 		Else
 			EMReel1.SetValue 0
 		End If
 		O1=N1
 	End If
 	If N2<>O2 Then
 		If N2 Then
 			EMReel2.SetValue 1
 		Else
 			EMReel2.SetValue 0
 		End If
 		O2=N2
 	End If
 	If N3<>O3 Then
 		If N3 Then
 			EMReel3.SetValue 1
 		Else
 			EMReel3.SetValue 0
 		End If
 		O3=N3
 	End If
 	If N4<>O4 Then
 		If N4 Then
 			EMReel4.SetValue 1
 		Else
 			EMReel4.SetValue 0
 		End If
 		O4=N4
 	End If
 	If N5<>O5 Then
 		If N5 Then
 			EMReel5.SetValue 1
 		Else
 			EMReel5.SetValue 0
 		End If
 		O5=N5
 	End If
 	If N60<>O60 Then
 		If N60 Then
 			EMReel7.SetValue 1
 		Else
 			EMReel7.SetValue 0
 		End If
 		O60=N60
 	End If
 	If N61<>O61 Then
 		If N61 Then
 			EMReel9.SetValue 1
 			EMReel13.SetValue 1
 		Else
 			EMReel9.SetValue 0
			EMReel13.SetValue 0
 		End If
 		O61=N61
 	End If
 	If N62<>O62 Then
 		If N62 Then
 			EMReel10.SetValue 1
 			EMReel14.SetValue 1
 		Else
			EMReel10.SetValue 0
 			EMReel14.SetValue 0
 		End If
 		O62=N62
 	End If
  	If N37<>O37 Then
 		If N37 Then			
Flasher001.visible = 1
 		Else'			
Flasher001.visible = 0
 		End If
 		O37=N37
 	End If
  	If N38<>O38 Then
 		If N38 Then			
Flasher002.visible = 1
 		Else			
Flasher002.visible = 0
 		End If
 		O38=N38
 	End If
  	If N39<>O39 Then
 		If N39 Then
 			Emreel11.SetValue 1
Flasher003.visible = 1
  		Else
 			EMReel11.SetValue 0
Flasher003.visible = 0
 		End If
 		O39=N39
 	End If
  	If N40<>O40 Then
 		If N40 Then
 			Emreel12.SetValue 1
Flasher004.visible = 1
  		Else
 			EMReel12.SetValue 0
Flasher004.visible = 0
 		End If
 		O40=N40
 	End If 	  

	NewTotal=ABS(N47)+ABS(N48)*2
 	If NewTotal<>OldTotal Then
 		Select Case OldTotal
 			Case 0:CarLOff(SubTot).blenddisablelighting = 2
 			Case 1:CarFOn(SubTot).blenddisablelighting = 10
 			Case 2:CarTOn(SubTot).blenddisablelighting = 10
 			Case 3:CarAOn(SubTot).blenddisablelighting = 2
 		End Select
 		Select Case NewTotal
			Case 0:CarLOff(SubTot).blenddisablelighting = 0
			Case 1:CarFOn(SubTot).blenddisablelighting = 0
			Case 2:CarTOn(SubTot).blenddisablelighting = 0
			Case 3:CarAOn(SubTot).blenddisablelighting = 0
 		End Select
  		OldTotal=NewTotal
 	End If
  End Sub
 
 Dim SubTot
Dim bb
 SubTot=0
 Sub MoveCar(aNewPos,aSpeed,aLastPos) 
 	If aNewPos>96 Then
 		If BallLocked=True Then
 			Kicker3.CreateBall
 			Kicker3.Kick 140,1            
 			Kicker2.Kick 180,1
 			BallLocked=False
 		End If
 	End If
 	PlaySound SoundFX("motor1",DOFGear)
 	If aNewPos>-1 And aNewPos<100 Then  
for each bb in PoliceCar 
bb.transZ=bb.TransZ-((99-aLastPos)-(99-aNewPos))*6
 Next
end if
'	SubTot=99-aNewPos 
End Sub

Sub SolCar(Enabled)
if Enabled then
PLights.enabled=1
RedLight.state=2
BlueLight.state=2
else
PLights.enabled=0
RedLight.state=0
BlueLight.state=0
End If
End Sub

 Dim BallLocked
 BallLocked=False
 
 Sub Kicker2_Hit 'increases framerate with 1 ball locked
if BallLocked=False Then  Kicker2.DestroyBall
 BallLocked=True
 End Sub
 
Sub Leds_Timer()
Dim ChgLED
ChgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
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
    tmp = tableobj.y * 2 / Police.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / Police.width-1
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

Const tnob = 4 ' total number of balls
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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/8, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/10, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


Sub Police_exit()
	If B2SOn Then
  		Controller.Pause = False
  		Controller.Stop
	End If
End Sub