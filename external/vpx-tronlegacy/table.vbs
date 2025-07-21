  Option Explicit
    Randomize

Const UseVpmModSol = True

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01560000", "sam.VBS", 3.10

Dim DesktopMode: DesktopMode = Table.ShowDT

If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
'Primitive2.visible=1
Else
Ramp16.visible=0
Ramp15.visible=0
'Primitive2.visible=0
End if

'********************
'Standard definitions
'********************

	Const cGameName = "trn_174h"

     Const UseSolenoids = 1
     Const UseLamps = 0
     Const UseSync = 1
     Const HandleMech = 0 

     'Standard Sounds
     Const SSolenoidOn = "Solenoid"
     Const SSolenoidOff = ""
     Const SCoin = "CoinIn"

 '************
' Table init.
'************

   'Variables
    Dim xx
    Dim Bump1, Bump2, Bump3, Mech3bank,bsTrough,bsRHole,DTBank4,turntable,ttDisc1
	Dim PlungerIM
    

  Sub Table_Init
InitVpmFFlipsSam
   vpmInit Me
	UpPost.Isdropped=true
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Tron (Stern 2011)"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 1
		.Hidden = 0
        .Games(cGameName).Settings.Value("sound") = 1
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
	End With

    On Error Goto 0


       '**Trough
    Set bsTrough = New cvpmBallStack
    bsTrough.InitSw 0, 21, 20, 19, 18, 0, 0, 0
    bsTrough.InitKick BallRelease, 90, 8
    bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
    bsTrough.Balls = 4

	'***Right Hole bsRHole
     Set bsRHole = New cvpmBallStack
     With bsRHole
         .InitSw 0, 11, 0, 0, 0, 0, 0, 0
         .InitKick sw11, 200, 20
         .KickZ = 0.4
         .InitExitSnd SoundFX("popper_ball",DOFContactors), SoundFX("Solenoid",DOFContactors)
         .KickForceVar = 2
     End With


 	'DropTargets
   	Set DTBank4 = New cvpmDropTarget  
   	  With DTBank4
   		.InitDrop Array(sw04,sw03,sw02,sw01),Array(4,3,2,1)
        .Initsnd SoundFX("DTL",DOFContactors), SoundFX("DTResetL",DOFContactors)
       End With

      '**Main Timer init
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1
 
	'Nudging
    	vpmNudge.TiltSwitch=-7
    	vpmNudge.Sensitivity=3    	
		vpmNudge.TiltObj=Array(Bumper1b,Bumper2b,Bumper3b,LeftSlingshot,RightSlingshot)

     ' Impulse Plunger
    Const IMPowerSetting = 52
    Const IMTime = 0.7
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .InitExitSnd "plunger2", "plunger"
        .CreateEvents "plungerIM"
    End With

TBPos=28:TBTimer.Enabled=0:TBDown=1:Controller.Switch(52) = 1:Controller.Switch(53) = 0

	recognizer.transx = -30
	recognizer.rotz=1

  End Sub

Sub Table_Paused:Controller.Pause = 1:End Sub
Sub Table_unPaused:Controller.Pause = 0:End Sub

 
'*****Keys
 Sub Table_KeyDown(ByVal keycode)

 	If Keycode = LeftFlipperKey then 
		'SolLFlipper true
	End If
 	If Keycode = RightFlipperKey then 
'		SolRFlipper true
	End If
    If keycode = PlungerKey Then Plunger.Pullback

    If vpmKeyDown(keycode) Then Exit Sub 
    
End Sub

Sub Table_KeyUp(ByVal keycode)
	If vpmKeyUp(keycode) Then Exit Sub
 	If Keycode = LeftFlipperKey then 
		'SolLFlipper false
	End If
 	If Keycode = RightFlipperKey then 
		'SolRFlipper False
	End If
	If Keycode = StartGameKey Then Controller.Switch(16) = 0
    If keycode = PlungerKey Then Plunger.Fire

End Sub

   'Solenoids
SolCallback(1) = "solTrough"
SolCallback(2) = "solAutofire"
SolCallback(3) = "DTBank4.SolDropUp"
SolCallback(4) = "bsRHole.SolOut"
SolCallback(5)="SolDiscMotor"' spinning disk
SolCallback(6) = "TBMove"
SolCallback(7) = "orbitpost"
'SolCallback(8) = "shaker"
SolModCallback(9) = "SetLamp139"
SolModCallback(10) = "SetLamp140"
SolModCallback(11) = "SetLamp141"
'SolCallback(12) = "upperleftflipper"
'SolCallback(13) = "leftslingshot"
'SolCallback(14) = "rightslingshot"

SolCallback(15) = "SolLFlipper"
SolCallback(16) = "SolRFlipper"

'Flashers
SolModCallback(17) = "SetLamp117"    'flash zen
SolModCallback(18) = "SetLamp118"'flash videogame
SolModCallback(19) = "setlamp119"    'flash right domes x2
'SolModCallback(20) = "SetLamp120"  'LE apron left
'SolModCallback(21) = "SetLamp121"   'LE apron right
'SolCallback(22) = "discdirrelay"  'LE disc direction relay
SolCallback(23) = "recogrelay"    'LE recognizer

SolModCallback(25) = "setlamp125"'flash left domes
SolModCallback(26) = "SetLamp126"'flash disc left 
SolModCallback(27) = "SetLamp127"'flash disc right
SolModCallback(28) = "SetLamp128"'flash backpanel x2
SolModCallback(29) = "SetLamp129"'flash recognizer
SolCallback(30) = "SetLamp 130,"'disc motor relay
SolModCallback(31) = "SetLamp131"'flash red disc left x2
SolModCallback(32) = "SetLamp132"'LE flash red disc x2

'Flashers

Sub SetLamp119(m):m = m/255:f119a.state = m:f119b.state = m:f119c.state = m:f119d.state = m:f119e.state = m:f119f.state = m:Primitive8.blenddisablelighting = m*2 +0.05:Primitive10.blenddisablelighting = m*2 +0.05:End Sub
Sub SetLamp125(m):m = m/255:f125a.state = m:f125b.state = m:f125c.state = m:f125d.state = m:f125e.state = m:f125f.state = m:Primitive7.blenddisablelighting = m*2 +0.05:Primitive9.blenddisablelighting = m*2 +0.05:End Sub
Sub SetLamp128(m):m = m/255:f128a.state = m:f128b.state = m:f128c.state = m:f128d.state = m:f128e.state = m:f128f.state = m:Primitive4.blenddisablelighting = m*2 +0.05:Primitive5.blenddisablelighting = m*2 +0.05:End Sub
Sub SetLamp131(m):m = m/255:f131a.state = m:f131b.state = m:End Sub
Sub SetLamp132(m):m = m/255:f132a.state = m:f132b.state = m:End Sub
Sub SetLamp139(m):m = m/255:f9.state = m:f9a.state = m:End Sub
Sub SetLamp140(m):m = m/255:f10.state = m:f10a.state = m:End Sub
Sub SetLamp141(m):m = m/255:f11.state = m:f11a.state = m:End Sub
Sub SetLamp117(m):m = m/255:f117.state = m:End Sub
Sub SetLamp118(m):m = m/255:f118.state = m:End Sub
Sub SetLamp126(m):m = m/255:f126.state = m:End Sub
Sub SetLamp127(m):m = m/255:f127.state = m:End Sub
Sub SetLamp129(m):m = m/255:f129.state = m:recognizer.blenddisablelighting = m +0.1:End Sub
'Sub SetLamp120(m):m = m/255:f120.state = m:End Sub
'Sub SetLamp121(m):m = m/255:f121.state = m:End Sub


Dim XLocation,XDir,T(4),ZRot
XDir=1
XLocation=-30
ZRot=1


Sub Timer1_Timer
	If Controller.Switch(16) = 0 Then

		If XLocation>40 Then XDir=0
		If XLocation<-30 Then XDir=1
		'T(XLocation).IsDropped=1
		If XDir=1 Then XLocation=XLocation+2:ZRot=ZRot-1
		If XDir=0 Then XLocation=XLocation-2:ZRot=ZRot+1
'		T(XLocation).IsDropped=0
		recognizer.transx=XLocation
		recognizer.rotz=zrot
		'recognizer.TriggerSingleUpdate()
'PlaySound SoundFX("motor",DOFContactors)
	End If
End Sub


Sub recogrelay(Enabled)
	If Enabled Then 
		Timer1.enabled=1
	Else
		Timer1.enabled=0
	End If
End Sub

Sub solTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		vpmTimer.PulseSw 22
	End If
 End Sub

Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
 End Sub

Sub Sol3bankmotor(Enabled)
	 	If Enabled then
 		RiseBank
		DropBank
	end if
End Sub


'    Spinning Disk

Sub SolSpinningDisk(Enabled)
	If Enabled Then
		TTTimer_Timer
		TTTimer.Enabled=Enabled
	PlaySound "spindisc", -1
	Else
		TTTimer.Enabled=False
	turntable.MotorOn=0
	stopSound "spindisc"
	End If
	turntable.MotorOn=Enabled
End Sub

Sub orbitpost(Enabled)
	If Enabled Then
		UpPost.Isdropped=false
	Else
		UpPost.Isdropped=true
	End If
 End Sub

' spinning discs (New) Taken from Whirlwind written by Herweh
	Set ttDisc1 = New myTurnTable
		ttDisc1.InitTurnTable Disc1Trigger, 8
		ttDisc1.SpinCW = False
		ttDisc1.CreateEvents "ttDisc1"


Sub sw01_Hit:DTBank4.Hit 4:End Sub
Sub sw02_Hit:DTBank4.Hit 3:End Sub
Sub sw03_Hit:DTBank4.Hit 2:End Sub
Sub sw04_Hit:DTBank4.Hit 1:End Sub
Sub sw7_Hit:Me.TimerEnabled = 1:sw7p.TransX = -2:vpmTimer.PulseSw 7:PlaySound SoundFX("fx_target",DOFContactors):End Sub
Sub sw7_Timer:Me.TimerEnabled = 0:sw7p.TransX = 0:End Sub
Sub sw8_Hit:Me.TimerEnabled = 1:sw8p.TransX = -2:vpmTimer.PulseSw 8:PlaySound SoundFX("fx_target",DOFContactors):End Sub
Sub sw8_Timer:Me.TimerEnabled = 0:sw8p.TransX = 0:End Sub
Sub sw12_Hit:Controller.Switch(12) = 1:PlaySound "rollover":End Sub
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub
Sub sw13_Hit:Me.TimerEnabled = 1:sw13p.TransX = -2:vpmTimer.PulseSw 13:PlaySound SoundFX("fx_target",DOFContactors):End Sub
Sub sw13_Timer:Me.TimerEnabled = 0:sw13p.TransX = 0:End Sub
Sub sw14_Hit:Controller.Switch(14) = 1:PlaySound "rollover":End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub
'Sub sw23:End Sub
Sub sw24_Hit:Controller.Switch(24) = 1:PlaySound "rollover":End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
Sub sw25_Hit:Controller.Switch(25) = 1:PlaySound "rollover":End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
Sub sw28_Hit:Controller.Switch(28) = 1:PlaySound "rollover":End Sub
Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub
Sub sw29_Hit:Controller.Switch(29) = 1:PlaySound "rollover":End Sub
Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub
Sub sw34_Hit:Controller.Switch(34) = 1:PlaySound "Gate":LeftCount = LeftCount + 1:End Sub
Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub
Sub sw35_Hit:Controller.Switch(35) = 1:PlaySound "Gate":End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub
Sub sw36_Spin:vpmTimer.PulseSw 36::playsound"spinner":End Sub
Sub sw37_Hit:Controller.Switch(37) = 1:PlaySound "Gate":RightCount = RightCount + 1:End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub
Sub sw38_Hit:Controller.Switch(38) = 1:PlaySound "Gate":End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub
Sub sw39_Hit:Controller.Switch(39) = 1:PlaySound "rollover":End Sub
Sub sw39_UnHit:Controller.Switch(39) = 0:End Sub
Sub sw41_Hit:Controller.Switch(41) = 1:End Sub
Sub sw41_UnHit:Controller.Switch(41) = 0:End Sub
Sub sw43_Hit:Controller.Switch(43) = 1:PlaySound "rollover":End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub
Sub sw44_Spin:vpmTimer.PulseSw 44::playsound"spinner":End Sub
Sub sw46_Hit:Controller.Switch(46) = 1:PlaySound "rollover":End Sub
Sub sw46_UnHit:Controller.Switch(46) = 0:End Sub
Sub sw48_Hit:Me.TimerEnabled = 1:sw48p.TransX = -2:vpmTimer.PulseSw 48:PlaySound SoundFX("fx_target",DOFContactors):End Sub
Sub sw48_Timer:Me.TimerEnabled = 0:sw48p.TransX = 0:End Sub

'Arcade Scoop
 Dim aBall, aZpos
 Dim bBall, bZpos

 Sub sw11_Hit
     Set bBall = ActiveBall
     PlaySound "kicker_enter_center"
     bZpos = 35
	 'ClearBallID
     Me.TimerInterval = 2
     Me.TimerEnabled = 1
 End Sub
 
 Sub sw11_Timer
     bBall.Z = bZpos
     bZpos = bZpos-4
     If bZpos <-30 Then
         Me.TimerEnabled = 0
         Me.DestroyBall
         bsRHole.AddBall Me
     End If
 End Sub
 
' ===============================================================================================
' spinning discs (New) Taken from Whirlwind written by Herweh
' ===============================================================================================

Dim discAngle, stepAngle, stopDiscs, discsAreRunning

InitDiscs()

Sub InitDiscs()
	discAngle 			= 0
	discsAreRunning		= False
End Sub

Sub SolDiscMotor(Enabled)
	ttDisc1.MotorOn = Enabled
	'ttDisc2.MotorOn = Enabled
	'ttDisc3.MotorOn = Enabled
	If Enabled Then
		stepAngle			= 20.0
		discsAreRunning		= True
		stopDiscs			= False
		DiscsTimer.Interval = 10
		DiscsTimer.Enabled 	= True
	Else
		stopDiscs			= True
		discsAreRunning		= True
	End If
End Sub

Sub DiscsTimer_Timer()
	' calc angle
	discAngle = discAngle + stepAngle
	If discAngle >= 360 Then
		discAngle = discAngle - 360
	End If
	' rotate discs
	Disc1.RotAndTra2 = 360 - discAngle
	If stopDiscs Then
		stepAngle = stepAngle - 0.1
		If stepAngle <= 0 Then
			DiscsTimer.Enabled 	= False
		End If
	End If
End Sub


Class myTurnTable
	Private mX, mY, mSize, mMotorOn, mDir, mBalls, mTrigger
	Public MaxSpeed, SpinDown, Speed

	Private Sub Class_Initialize
		mMotorOn = False : Speed = 0 : mDir = 1 : SpinDown = 15
		Set mBalls = New cvpmDictionary
	End Sub

	Public Sub InitTurntable(aTrigger, aMaxSpeed)
		mX = aTrigger.X : mY = aTrigger.Y : mSize = aTrigger.Radius
		MaxSpeed = aMaxSpeed : Set mTrigger = aTrigger
	End Sub

	Public Sub CreateEvents(aName)
		If vpmCheckEvent(aName, Me) Then
			vpmBuildEvent mTrigger, "Hit", aName & ".AddBall ActiveBall"
			vpmBuildEvent mTrigger, "UnHit", aName & ".RemoveBall ActiveBall"
			vpmBuildEvent mTrigger, "Timer", aName & ".Update"
		End If
	End Sub

	Public Sub SolMotorState(aCW, aEnabled)
		mMotorOn = aEnabled
		If aEnabled Then If aCW Then mDir = 1 Else mDir = -1
		NeedUpdate = True
	End Sub

	Public Property Let MotorOn(aEnabled)
		mMotorOn = aEnabled
		NeedUpdate = (mBalls.Count > 0) Or (SpinDown > 0)
	End Property
	Public Property Get MotorOn
		MotorOn = mMotorOn
	End Property

	Public Sub AddBall(aBall)
		On Error Resume Next
		mBalls.Add aBall,0
		NeedUpdate = True
	End Sub
	Public Sub RemoveBall(aBall)
		On Error Resume Next
		mBalls.Remove aBall
		NeedUpdate = (mBalls.Count > 0) Or (SpinDown > 0)
	End Sub

	Public Property Let SpinCW(aCW)
		If aCW Then mDir = 1 Else mDir = -1
		NeedUpdate = True
	End Property
	Public Property Get SpinCW
		SpinCW = (mDir = 1)
	End Property

	Public Sub Update
		If mMotorOn Then
			Speed = MaxSpeed
			NeedUpdate = mBalls.Count
		Else
			Speed = Speed - SpinDown*MaxSpeed/3000 '100
			If Speed < 0 Then 
				Speed = 0
				'msgbox "off"
				NeedUpdate = mBalls.Count
			End If
		End If
		If Speed > 0 Then
			Dim obj
			On Error Resume Next
			For Each obj In mBalls.Keys
				If obj.X < 0 Or Err Then RemoveBall obj Else AffectBall obj
			Next
			On Error Goto 0
		End If
	End Sub

	Public Sub AffectBall(aBall)
		Dim dX, dY, dist
		dX = aBall.X - mX : dY = aBall.Y - mY : dist = Sqr(dX*dX + dY*dY)
		If dist > mSize Or dist < 1 Or Speed = 0 Then Exit Sub
		aBall.VelX = aBall.VelX - (dY * mDir * Speed / 1000)
		aBall.VelY = aBall.VelY + (dX * mDir * Speed / 1000)
	End Sub

	Private Property Let NeedUpdate(aEnabled)
		If mTrigger.TimerEnabled <> aEnabled Then
			mTrigger.TimerInterval = 10
			mTrigger.TimerEnabled = aEnabled
		End If
	End Property
End Class
  
Sub SolLFlipper(Enabled)
     If Enabled Then
		 PlaySound SoundFX("FlipperUpLeft",DOFContactors)
		 LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
     Else
		 PlaySound SoundFX("FlipperDown",DOFContactors)
		 LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
     End If
 End Sub

Sub SolRFlipper(Enabled)
     If Enabled Then
		 PlaySound SoundFX("FlipperUpRight",DOFContactors)
		 RightFlipper.RotateToEnd
     Else
		 PlaySound SoundFX("FlipperDown",DOFContactors)
		 RightFlipper.RotateToStart
     End If
 End Sub   

 'Drains and Kickers
Dim BallCount:BallCount = 0
   Sub Drain_Hit():PlaySound "Drain"
	'ClearBallID
	BallCount = BallCount - 1
	bsTrough.AddBall Me
	   End Sub
   Sub sw11_UnHit()
	'NewBallID
	End Sub
   Sub BallRelease_UnHit()
	'NewBallID
		BallCount = BallCount + 1		
	End Sub

'***Slings and rubbers

 Dim LStep, RStep

Sub LeftSlingShot_Slingshot
PlaySound SoundFX("left_slingshot",DOFContactors), 0, 0.3, -0.1, 0.25
vpmTimer.PulseSw 26
	LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    LeftSlingShot.TimerInterval = 10
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
PlaySound SoundFX("right_slingshot",DOFContactors), 0, 0.3, 0.1, 0.25
vpmTimer.PulseSw 27
	RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    RightSlingShot.TimerInterval = 10
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

   'Bumpers
      Sub Bumper1b_Hit
      vpmTimer.PulseSw 31
      PlaySound SoundFX("fx_bumper1",DOFContactors)
    	End Sub
     
 
      Sub Bumper2b_Hit
      vpmTimer.PulseSw 30
      PlaySound SoundFX("fx_bumper1",DOFContactors)
       End Sub
 
      Sub Bumper3b_Hit
      vpmTimer.PulseSw 32
      PlaySound SoundFX("fx_bumper1",DOFContactors)
       End Sub
  

Dim LampState(200), FadingLevel(200), FadingState(200)
Dim FlashState(200), FlashLevel(200)
Dim FlashSpeedUp, FlashSpeedDown
Dim x

AllLampsOff()
LampTimer.Interval = 40 'lamp fading speed
LampTimer.Enabled = 1
'
FlashInit()
FlasherTimer.Interval = 10 'flash fading speed
FlasherTimer.Enabled = 1

'' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
			FlashState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
        Next
    End If

    UpdateLamps
End Sub

Sub FlashInit
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
        FlashLevel(i) = 0
    Next

    FlashSpeedUp = 50   ' fast speed when turning on the flasher
    FlashSpeedDown = 10 ' slow speed when turning off the flasher, gives a smooth fading
    AllFlashOff()
End Sub

Sub AllFlashOff
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
    Next
End Sub

Sub Reflections_Timer()

End Sub

Sub UpdateLamps()
NFadeL 1, l1
NFadeL 2, l2
NFadeL 3, l3
NFadeL 4, l4
NFadeL 5, l5
NFadeL 6, l6
NFadeL 7, l7
NFadeL 8, l8
NFadeL 9, l9
NFadeL 10, l10
NFadeL 11, l11
NFadeL 12, l12
NFadeL 13, l13
NFadeL 14, l14
NFadeL 15, l15
NFadeL 16, l16
NFadeL 17, l17
NFadeL 18, l18
NFadeL 19, l19
NFadeL 20, l20
NFadeL 21, l21
NFadeL 22, l22
NFadeL 23, l23
NFadeL 24, l24
NFadeL 25, l25
NFadeL 26, l26
NFadeL 27, l27
NFadeL 28, l28
NFadeL 29, l29
NFadeL 30, l30
NFadeL 31, l31
NFadeL 32, l32
NFadeL 33, l33
NFadeL 34, l34
NFadeL 35, l35
NFadeL 36, l36
NFadeL 37, l37
NFadeL 38, l38
NFadeL 39, l39
NFadeL 40, l40
NFadeL 42, l42
NFadeL 43, l43
NFadeL 45, l45
NFadeL 49, l49
NFadeL 50, l50
NFadeL 51, l51
NFadeL 52, l52
NFadeL 53, l53
NFadeL 54, l54
NFadeL 55, l55
NFadeL 56, l56
NFadeL 57, l57
NFadeL 58, l58
NFadeL 59, l59
NFadeL 60, l60
NFadeL 61, l61
NFadeL 62, l62
NFadeL 63, l63
NFadeL 64, l64

evleft.color = RGB(LampState(106), LampState(105), LampState(104))
evright.color = RGB(LampState(103), LampState(102), LampState(101))
Primitive2.color = RGB(LampState(106), LampState(105), LampState(104))
Primitive1.color = RGB(LampState(106), LampState(105), LampState(104))
lr1.color = RGB(LampState(106), LampState(105), LampState(104))
rr1.color = RGB(LampState(103), LampState(102), LampState(101))
End Sub


Sub FadePrim(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:FadingLevel(nr) = 0
        Case 3:pri.image = c:FadingLevel(nr) = 1
        Case 4:pri.image = b:FadingLevel(nr) = 2
        Case 5:pri.image = a:FadingLevel(nr) = 3
    End Select
End Sub

''Lights

Sub NFadeL(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0:FadingLevel(nr) = 0
        Case 5:a.State = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0
        Case 5:a.State = 1
    End Select
End Sub

' Flasher objects
' Uses own faster timer

Sub Flash(nr, object)
    Select Case FlashState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FlashState(nr) = -1 'completely off
            End if
            Object.opacity = FlashLevel(nr)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
            If FlashLevel(nr) > 1000 Then
                FlashLevel(nr) = 1000
                FlashState(nr) = -2 'completely on
            End if
            Object.opacity = FlashLevel(nr)
    End Select
End Sub

 Sub AllLampsOff():For x = 1 to 200:LampState(x) = 4:FadingLevel(x) = 4:Next:UpdateLamps:UpdateLamps:Updatelamps:End Sub
 

Sub SetLamp(nr, value)
    If value = 0 AND LampState(nr) = 0 Then Exit Sub
    If value = 1 AND LampState(nr) = 1 Then Exit Sub
    LampState(nr) = abs(value) + 4
FadingLevel(nr ) = abs(value) + 4: FadingState(nr ) = abs(value) + 4
End Sub

Sub SetFlash(nr, stat)
    FlashState(nr) = ABS(stat)
End Sub

'*********************************************************************
'* TARGETBANK TARGETS Taken from AFM written by Groni ****************
'*********************************************************************

Sub SW49_Hit
vpmTimer.PulseSw 49
SW49P.X=442.9411
SW49P.Y=449.8546
Me.TimerEnabled = 1
PlaySound SoundFX("fx_target",DOFContactors)
End Sub

Sub SW49_Timer:SW49P.X=442.6875:SW49P.Y=453.3662:Me.TimerEnabled = 0:End Sub

Sub SW50_Hit
vpmTimer.PulseSw 50
SW50P.X=448.6911
SW50P.Y=449.8546
Me.TimerEnabled = 1
PlaySound SoundFX("fx_target",DOFContactors)
End Sub

Sub SW50_Timer:SW50P.X=448.4375:SW50P.Y=453.3662:Me.TimerEnabled = 0:End Sub

Sub SW51_Hit
vpmTimer.PulseSw 51
SW51P.X=454.0661
SW51P.Y=449.8546
Me.TimerEnabled = 1
PlaySound SoundFX("fx_target",DOFContactors)
End Sub

Sub SW51_Timer:SW51P.X=453.8125:SW51P.Y=453.3662:Me.TimerEnabled = 0:End Sub


'*********************************************************************
'* TARGETBANK MOVEMENT Taken from AFM written by Groni ***************
'*********************************************************************

Dim TBPos, TBDown

Sub TBMove (enabled)
if enabled then
TBTimer.Enabled=1
PlaySound SoundFX("TargetBank",DOFContactors)
End If
End Sub

Sub TBTimer_Timer()	
Select Case TBPos
Case 0: MotorBank.Z=-20:SW49P.Z=-20:SW50P.Z=-20:SW51P.Z=-20:TBPos=0:TBDown=0:TBTimer.Enabled=0:Controller.Switch(52) = 0:Controller.Switch(53) = 1:
Case 1: MotorBank.Z=-22:SW49P.Z=-22:SW50P.Z=-22:SW51P.Z=-22
Case 2: MotorBank.Z=-24:SW49P.Z=-24:SW50P.Z=-24:SW51P.Z=-24
Case 3: MotorBank.Z=-26:SW49P.Z=-26:SW50P.Z=-26:SW51P.Z=-26
Case 4: MotorBank.Z=-28:SW49P.Z=-28:SW50P.Z=-28:SW51P.Z=-28
Case 5: MotorBank.Z=-30:SW49P.Z=-30:SW50P.Z=-30:SW51P.Z=-30
Case 6: MotorBank.Z=-32:SW49P.Z=-32:SW50P.Z=-32:SW51P.Z=-32
Case 7: MotorBank.Z=-34:SW49P.Z=-34:SW50P.Z=-34:SW51P.Z=-34
Case 8: MotorBank.Z=-36:SW49P.Z=-36:SW50P.Z=-36:SW51P.Z=-36
Case 9: MotorBank.Z=-38:SW49P.Z=-38:SW50P.Z=-38:SW51P.Z=-38
Case 10: MotorBank.Z=-40:SW49P.Z=-40:SW50P.Z=-40:SW51P.Z=-40
Case 11: MotorBank.Z=-42:SW49P.Z=-42:SW50P.Z=-42:SW51P.Z=-42
Case 12: MotorBank.Z=-44:SW49P.Z=-44:SW50P.Z=-44:SW51P.Z=-44:
Case 13: MotorBank.Z=-46:SW49P.Z=-46:SW50P.Z=-46:SW51P.Z=-46:
Case 14: MotorBank.Z=-48:SW49P.Z=-48:SW50P.Z=-48:SW51P.Z=-48
Case 15: MotorBank.Z=-50:SW49P.Z=-50:SW50P.Z=-50:SW51P.Z=-50
Case 16: MotorBank.Z=-52:SW49P.Z=-52:SW50P.Z=-52:SW51P.Z=-52
Case 17: MotorBank.Z=-54:SW49P.Z=-54:SW50P.Z=-54:SW51P.Z=-54
Case 18: MotorBank.Z=-56:SW49P.Z=-56:SW50P.Z=-56:SW51P.Z=-56
Case 19: MotorBank.Z=-58:SW49P.Z=-58:SW50P.Z=-58:SW51P.Z=-58
Case 20: MotorBank.Z=-60:SW49P.Z=-60:SW50P.Z=-60:SW51P.Z=-60
Case 21: MotorBank.Z=-62:SW49P.Z=-62:SW50P.Z=-62:SW51P.Z=-62
Case 22: MotorBank.Z=-64:SW49P.Z=-64:SW50P.Z=-64:SW51P.Z=-64:SW49.isdropped=0:SW50.isdropped=0:SW51.isdropped=0:DPWall.isdropped=0:DPWall1.isdropped=1
Case 23: MotorBank.Z=-66:SW49P.Z=-66:SW50P.Z=-66:SW51P.Z=-66
Case 24: MotorBank.Z=-68:SW49P.Z=-68:SW50P.Z=-68:SW51P.Z=-68
Case 25: MotorBank.Z=-70:SW49P.Z=-70:SW50P.Z=-70:SW51P.Z=-70
Case 26: MotorBank.Z=-72:SW49P.Z=-72:SW50P.Z=-72:SW51P.Z=-72
Case 27: MotorBank.Z=-74:SW49P.Z=-74:SW50P.Z=-74:SW51P.Z=-74
Case 28: MotorBank.Z=-76:SW49P.Z=-76:SW50P.Z=-76:SW51P.Z=-76:SW49.isdropped=1:SW50.isdropped=1:SW51.isdropped=1:DPWALL.isdropped=1
Case 29: TBTimer.Enabled=0:TBDown=1:Controller.Switch(52) = 1:Controller.Switch(53) = 0
End Select

If TBDown=0 then TBPos=TBPos+1 
If TBDown=1 then TBPos=TBPos-1
End Sub


Sub ShooterLane_Hit()
	Controller.Switch(23)=1
End Sub

Sub ShooterLane_Unhit()
	Controller.Switch(23)=0
End Sub

Dim frame, FinalFrame  'ArcadeTimer
FinalFrame = 126 'number of frames - 1
frame = 0

 Sub ArcadeTimer_Timer()       
	Arcade(frame).isdropped = True
	frame = frame + 1
	If frame = FinalFrame Then frame=0
	Arcade(frame).isdropped = False
 End Sub

Sub Trigger1_hit
	PlaySound "DROP_LEFT"
 End Sub

 Sub Trigger2_hit
	PlaySound "DROP_RIGHT"
 End Sub 


Sub Table_exit()
	Controller.Pause = False
	Controller.Stop
End Sub

Sub RLS_Timer()
              RampGate1.RotZ = -(Spinner4.currentangle)
              RampGate2.RotZ = -(Spinner1.currentangle)
              RampGate3.RotZ = -(Spinner3.currentangle)
              RampGate4.RotZ = -(Spinner2.currentangle)
              SpinnerT4.RotZ = -(sw44.currentangle)
              SpinnerT1.RotZ = -(sw36.currentangle)
End Sub
  
'primitive flippers!
dim MotorCallback
Set MotorCallback = GetRef("GameTimer")
Sub GameTimer
    UpdateFlipperLogos
End Sub

Sub UpdateFlipperLogo_Timer
    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
	LFLogoUP.RotY = LeftFlipper1.CurrentAngle
lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
End Sub

'******DROP TARGET PRIMITIVES******
Dim sw1up, sw2up, sw3up, sw4up
Dim PrimT

Sub PrimT_Timer
	if sw01.IsDropped = True then sw1up = False else sw1up = True
	if sw02.IsDropped = True then sw2up = False else sw2up = True
	if sw03.IsDropped = True then sw3up = False else sw3up = True
	if sw04.IsDropped = True then sw4up = False else sw4up = True
End Sub


Sub sw1T_Timer()
	If sw1up = True and sw1p.z < 0 then sw1p.z = sw1p.z + 3
	If sw1up = False and sw1p.z > -45 then sw1p.z = sw1p.z - 3
	If sw1p.z >= -45 then sw1up = False
End Sub

Sub sw2T_Timer()
	If sw2up = True and sw2p.z < 0 then sw2p.z = sw2p.z + 3
	If sw2up = False and sw2p.z > -45 then sw2p.z = sw2p.z - 3
	If sw2p.z >= -45 then sw2up = False
End Sub

Sub sw3T_Timer()
	If sw3up = True and sw3p.z < 0 then sw3p.z = sw3p.z + 3
	If sw3up = False and sw3p.z > -45 then sw3p.z = sw3p.z - 3
	If sw3p.z >= -45 then sw3up = False
End Sub

Sub sw4T_Timer()
	If sw4up = True and sw4p.z < 0 then sw4p.z = sw4p.z + 3
	If sw4up = False and sw4p.z > -45 then sw4p.z = sw4p.z - 3
	If sw4p.z >= -45 then sw4up = False
End Sub

Set GiCallBack = GetRef("UpdateGi")

Sub UpdateGi(nr,enabled)
If enabled Then
	dim bulb
	for each bulb in GI
	bulb.state = 1
	next
Table.colorgradeimage = "ColorGradeLUT256x16_ConSat"
Primitive11.blenddisablelighting = 0.7
Primitive22.blenddisablelighting = 8
Else
	for each bulb in GI
	bulb.state = 0
	next
Table.colorgradeimage = "ColorGrade_4"
Primitive11.blenddisablelighting = 0.2
Primitive22.blenddisablelighting = 0
End If
End Sub 
 
' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

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
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table.width-1
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
      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/10, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/9, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
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

Dim LeftCount:LeftCount = 0
Sub leftdrop_hit
	If LeftCount = 1 then
		playsound "BallDrop"
	End If
	LeftCount = 0
End Sub

Dim RightCount:RightCount = 0
Sub rightdrop_hit
	If RightCount = 1 then
		playsound "BallDrop"
	End If
	RightCount = 0
End Sub