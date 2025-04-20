' NBA Fastbreak / IPD No. 4023 / March, 1997 / 4 Players
' http://www.ipdb.org/machine.cgi?id=4023
' VP915 v1.0 by JPSalas 2013
' VPX v1.0 by MaX 2016
' based on the tables by Aurich and bmiki75
'2018 Mod by Darth Marino. Extra special thanks to DJROBX for helping implement the Shot Clock
'Fast Flips implemented by DJRobX and CarnyPriest

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const BallSize = 50                 'add this here to redefine the ball size, placed before LoadVPM.'

LoadVPM "01120100", "WPC.VBS", 3.49 'minimum core.vbs version

Dim bsTrough, bsEject, bsSaucer1, bsSaucer2, bsSaucer3, bsSaucer4, mBallCatch, mDefender, MagnetCatch, bgball
Dim PlungerIM, x, bump1, bump2, bump3, ArenaMod, RedClock, FastFlips, ff


'*************************************
'*******FAST FLIPS OPTION*************
'***REQUIRES PINMAME SAMupdate r4614 OR NEWER!!!!!!!!!!!!!!!!!!
'*** http://vpuniverse.com/forums/topic/3651-sambuild32-beta-thread/
'*************************************
'       1 = OFF     2 = ON
Const UseSolenoids = 1
'*************************************


'*************************************
'*******RED SHOT CLOCK MOD************
'       0 = OFF     1 = ON
           RedClock=1
'*************************************

'*************************************
'*******ARENA WALL MOD**************
'       0 = OFF     1 = ON
           ArenaMod=1
'*************************************




Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 1

' Standard Sounds
Const SSolenoidOn = "fx_Solenoidon"
Const SSolenoidOff = "fx_SolenoidOff"
Const SFlipperOn = "fx_flipperup1"
Const SFlipperoff = "fx_flipperdown"
Const SCoin = "fx_coin"
Const cGameName = "nbaf_31"
Const cSingleLFlip=0
Const cSingleRFlip=0

Set GiCallback2 = GetRef("UpdateGI")

sub updategi (no, Enabled)

'	Eval("textbox"&no).text=enabled

end sub


Set MotorCallback = GetRef("GameTimer") 'realtime updates - flipper logos, rolling sound

Sub table1_Init
vpmInit Me
    Dim ii
    With Controller
        
        .GameName = cGameName
        .SplashInfoLine = "NBA Fastbreak, Bally 1997"
   '     .Games(cGameName).Settings.Value("rol") = 0 'rotated vpm display
        .HandleMechanics = 0
        .HandleKeyboard = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .ShowTitle = 0
        .Hidden = 0
'      .Games(cGameName).Settings.Value("dmd_pos_x")=0
'      .Games(cGameName).Settings.Value("dmd_pos_y")=0
        If Err Then MsgBox Err.Description
    End With
    On Error Goto 0
    Controller.SolMask(0) = 0
    vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
    Controller.DIP(0) = &H00
  '  Controller.Run GetPlayerHWnd
controller.run
    Controller.Switch(22) = 1 'close coin door
    Controller.Switch(24) = 1 'and keep it close

If ArenaMod=1 then 
	Wall80.isdropped=false 
	Wall123.isdropped=false 

else 
	Wall80.isdropped=True
	Wall123.isdropped=True
End If

    ' Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(LeftSlingshot, RightSlingshot, bumper1, bumper2, bumper3)

    ' Trough & Ball Release
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 32, 33, 34, 35, 31, 0, 0
        .InitKick BallRelease, 180, 10
        .InitExitSnd "fx_Ballrel", "fx_solenoid"
        .InitEntrySnd "fx_solenoid", "fx_solenoid"
        .IsTrough = True
        .Balls = 4
    End With

    ' Ball Catch Magnet
    Set MagnetCatch = New cvpmMagnet
    With MagnetCatch
        .InitMagnet BCMagnet, 7
        .Solenoid = 8
        .CreateEvents "MagnetCatch"
    End With

    ' Eject
    Set bsEject = New cvpmBallStack
    With bsEject
        .InitSaucer sw25, 25, 165, 7
        .InitExitSnd "fx_popper", "fx_Solenoid"
    End With

    ' Saucers (In the Paint)
    Set bsSaucer1 = New cvpmBallStack
    With bsSaucer1
        .InitSaucer sw68, 68, 65, 32
        .Kickz = 1.2
        .InitExitSnd "fx_popper", "fx_Solenoid"
    End With

    Set bsSaucer2 = New cvpmBallStack
    With bsSaucer2
        .InitSaucer sw67, 67, 26, 28
        .Kickz = 1.15
        .InitExitSnd "fx_popper", "fx_Solenoid"
    End With

    Set bsSaucer3 = New cvpmBallStack
    With bsSaucer3
        .InitSaucer sw66, 66, 337, 32
        .Kickz = 1.15
        .InitExitSnd "fx_popper", "fx_Solenoid"
    End With

    Set bsSaucer4 = New cvpmBallStack
    With bsSaucer4
        .InitSaucer sw65, 65, 293, 31.5
        .Kickz = 1.2
        .InitExitSnd "fx_popper", "fx_Solenoid"
    End With

    ' Defender
    Set mDefender = New cvpmMech
    With mDefender
        .MType = vpmMechOneDirSol + vpmMechStopEnd + vpmMechLinear
        .Sol1 = 37 'Enable
        .Sol2 = 38 'Direction
        .Length = 65
        .Steps = 65
        .AddSw 51, 0, 1
        .AddSw 52, 17, 18
        .AddSw 53, 31, 32
        .AddSw 54, 44, 45
        .AddSw 55, 64, 65
        .CallBack = GetRef("UpdateDefender")
        .Start
    End With
    UpdateDefender 32, 32, 32

    'Impulse Plunger
    Const IMPowerSetting = 38 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swPlunger, IMPowerSetting, IMTime
        .Switch 15
        .Random 0
        .InitExitSnd "fx_plunger2", "fx_plunger"
        .CreateEvents "plungerIM"
    End With

    ' Misc. Initialisation
    LeftSLing.IsDropped = 1:LeftSLing2.IsDropped = 1:LeftSLing3.IsDropped = 1
    RightSLing.IsDropped = 1:RightSLing2.IsDropped = 1:RightSLing3.IsDropped = 1
    LeftSLingH.IsDropped = 1:LeftSLingH2.IsDropped = 1:LeftSLingH3.IsDropped = 1
    RightSLingH.IsDropped = 1:RightSLingH2.IsDropped = 1:RightSLingH3.IsDropped = 1
    sw41a.IsDropped = 1:sw42a.IsDropped = 1:sw43a.IsDropped = 1
    sw28a.IsDropped = 1:sw18a.IsDropped = 1

    For each ii in ADef1:ii.Isdropped = 1:Next
    For each ii in ADef2:ii.visible = 0:ii.Collidable = 0:Next

    BackBall.CreateSizedBall(20).Image = "BasketBall"

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
'StartShake

Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then
	EMreel1.visible=true
Else
	EMreel1.visible=false
end If
End Sub


Sub Table_exit()
Controller.Stop
End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = PlungerKey Then PlaySound "fx_PlungerPull":Controller.Switch(11) = 1
    If keycode = LeftTiltKey Then LeftNudge 90, 1.6, 20:PlaySound "fx_nudge_left"
    If keycode = RightTiltKey Then RightNudge 270, 1.6, 20:PlaySound "fx_nudge_right"
    If keycode = CenterTiltKey Then CenterNudge 0, 2.8, 30:PlaySound "fx_nudge_forward"
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If vpmKeyUp(keycode) Then Exit Sub
    If keycode = PlungerKey Then Controller.Switch(11) = 0
End Sub

'*************************************
'          Nudge System
' based on Noah's nudgetest table
'*************************************

Dim LeftNudgeEffect, RightNudgeEffect, NudgeEffect

Sub LeftNudge(angle, strength, delay)
    vpmNudge.DoNudge angle, (strength * (delay-LeftNudgeEffect) / delay) + RightNudgeEffect / delay
    LeftNudgeEffect = delay
    RightNudgeEffect = 0
    RightNudgeTimer.Enabled = 0
    LeftNudgeTimer.Interval = delay
    LeftNudgeTimer.Enabled = 1
End Sub

Sub RightNudge(angle, strength, delay)
    vpmNudge.DoNudge angle, (strength * (delay-RightNudgeEffect) / delay) + LeftNudgeEffect / delay
    RightNudgeEffect = delay
    LeftNudgeEffect = 0
    LeftNudgeTimer.Enabled = 0
    RightNudgeTimer.Interval = delay
    RightNudgeTimer.Enabled = 1
End Sub

Sub CenterNudge(angle, strength, delay)
    vpmNudge.DoNudge angle, strength * (delay-NudgeEffect) / delay
    NudgeEffect = delay
    NudgeTimer.Interval = delay
    NudgeTimer.Enabled = 1
End Sub

Sub LeftNudgeTimer_Timer()
    LeftNudgeEffect = LeftNudgeEffect-1
    If LeftNudgeEffect = 0 then LeftNudgeTimer.Enabled = 0
End Sub

Sub RightNudgeTimer_Timer()
    RightNudgeEffect = RightNudgeEffect-1
    If RightNudgeEffect = 0 then RightNudgeTimer.Enabled = 0
End Sub

Sub NudgeTimer_Timer()
    NudgeEffect = NudgeEffect-1
    If NudgeEffect = 0 then NudgeTimer.Enabled = False
End Sub

'*********
' Switches
'*********

Dim LStep, RStep

Sub LeftSlingShot_Slingshot:LeftSling.IsDropped = 0:PlaySound "fx_slingshot1":vpmTimer.PulseSw 57:LStep = 0:Me.TimerEnabled = 1:End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 0:LeftSLing.IsDropped = 0:LeftSLingH.IsDropped = 0
        Case 1: 'pause
        Case 2:LeftSLing.IsDropped = 1:LeftSLing2.IsDropped = 0:LeftSLingH.IsDropped = 1:LeftSLingH2.IsDropped = 0
        Case 3:LeftSLing2.IsDropped = 1:LeftSLing3.IsDropped = 0:LeftSLingH2.IsDropped = 1:LeftSLingH3.IsDropped = 0
        Case 4:LeftSLing3.IsDropped = 1:LeftSLingH3.IsDropped = 1:Me.TimerEnabled = 0
    End Select

    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot:RightSling.IsDropped = 0:PlaySound "fx_slingshot2":vpmTimer.PulseSw 58:RStep = 0:Me.TimerEnabled = 1:End Sub
Sub RightSlingShot_Timer
    Select Case RStep
        Case 0:RightSLing.IsDropped = 0:RightSLingH.IsDropped = 0
        Case 1: 'pause
        Case 2:RightSLing.IsDropped = 1:RightSLing2.IsDropped = 0:RightSLingH.IsDropped = 1:RightSLingH2.IsDropped = 0
        Case 3:RightSLing2.IsDropped = 1:RightSLing3.IsDropped = 0:RightSLingH2.IsDropped = 1:RightSLingH3.IsDropped = 0
        Case 4:RightSLing3.IsDropped = 1:RightSLingH3.IsDropped = 1:Me.TimerEnabled = 0
    End Select

    RStep = RStep + 1
End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 61:PlaySound "fx_bumper1":bump1 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper1_Timer()
    Select Case bump1
        Case 1:Ring1.HeightTop = 15:Ring1.HeightBottom = 15:bump1 = 2
        Case 2:Ring1.HeightTop = 25:Ring1.HeightBottom = 25:bump1 = 3
        Case 3:Ring1.HeightTop = 35:Ring1.HeightBottom = 35:bump1 = 4
        Case 4:Ring1.HeightTop = 45:Ring1.HeightBottom = 45:Me.TimerEnabled = 0
    End Select

    'Bumper1R.State = ABS(Bumper1R.State - 1) 'refresh light
End Sub

Sub Bumper2_Hit:vpmTimer.PulseSw 23:PlaySound "fx_bumper2":bump2 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper2_Timer()
    Select Case bump2
        Case 1:Ring2.HeightTop = 15:Ring2.HeightBottom = 15:bump2 = 2
        Case 2:Ring2.HeightTop = 25:Ring2.HeightBottom = 25:bump2 = 3
        Case 3:Ring2.HeightTop = 35:Ring2.HeightBottom = 35:bump2 = 4
        Case 4:Ring2.HeightTop = 45:Ring2.HeightBottom = 45:Me.TimerEnabled = 0
    End Select

   ' Bumper2R.State = ABS(Bumper2R.State - 1) 'refresh light
End Sub

Sub Bumper3_Hit:vpmTimer.PulseSw 62:PlaySound "fx_bumper3":bump3 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper3_Timer()
    Select Case bump3
        Case 1:Ring3.HeightTop = 15:Ring3.HeightBottom = 15:bump3 = 2
        Case 2:Ring3.HeightTop = 25:Ring3.HeightBottom = 25:bump3 = 3
        Case 3:Ring3.HeightTop = 35:Ring3.HeightBottom = 35:bump3 = 4
        Case 4:Ring3.HeightTop = 45:Ring3.HeightBottom = 45:Me.TimerEnabled = 0
    End Select

  '  Bumper3R.State = ABS(Bumper3R.State - 1) 'refresh light
End Sub

' Eject holes
Sub Drain_Hit:ClearBallID:Playsound "fx_drain":bsTrough.AddBall Me:End Sub
Sub sw25_Hit:Playsound "fx_kicker_enter":bsEject.AddBall 0:End Sub

Sub sw65_Hit
    PlaySound "fx_kicker_enter"
    bsSaucer4.AddBall 0
End Sub

Sub sw66_Hit
    PlaySound "fx_kicker_enter"
    bsSaucer3.AddBall 0
End Sub

Sub sw67_Hit
    PlaySound "fx_kicker_enter"
    bsSaucer2.AddBall 0
End Sub

Sub sw68_Hit
    PlaySound "fx_kicker_enter"
    bsSaucer1.AddBall 0
End Sub

' Rollovers
Sub sw26_Hit:la1.IsDropped = 1:Controller.Switch(26) = 1:PlaySound "fx_sensor":End Sub
Sub sw26_UnHit:la1.IsDropped = 0:Controller.Switch(26) = 0:End Sub

Sub sw16_Hit:la2.IsDropped = 1:Controller.Switch(16) = 1:PlaySound "fx_sensor":End Sub
Sub sw16_UnHit:la2.IsDropped = 0:Controller.Switch(16) = 0:End Sub

Sub sw17_Hit:la3.IsDropped = 1:Controller.Switch(17) = 1:PlaySound "fx_sensor":End Sub
Sub sw17_UnHit:la3.IsDropped = 0:Controller.Switch(17) = 0:End Sub

Sub sw27_Hit:la4.IsDropped = 1:Controller.Switch(27) = 1:PlaySound "fx_sensor":End Sub
Sub sw27_UnHit:la4.IsDropped = 0:Controller.Switch(27) = 0:End Sub

Sub sw56_Hit:la5.IsDropped = 1:Controller.Switch(56) = 1:PlaySound "fx_sensor":End Sub
Sub sw56_UnHit:la5.IsDropped = 0:Controller.Switch(56) = 0:End Sub

Sub sw38_Hit:la6.IsDropped = 1:Controller.Switch(38) = 1:PlaySound "fx_sensor":End Sub
Sub sw38_UnHit:la6.IsDropped = 0:Controller.Switch(38) = 0:End Sub

Sub sw48_Hit:la7.IsDropped = 1:Controller.Switch(48) = 1:PlaySound "fx_sensor":End Sub
Sub sw48_UnHit:la7.IsDropped = 0:Controller.Switch(48) = 0:End Sub

'Optos
Sub sw36_Hit:Controller.Switch(36) = 1:End Sub
Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub

Sub sw47_Hit:Controller.Switch(47) = 1:End Sub
Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub

Sub sw45_Hit:Controller.Switch(45) = 1:End Sub
Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub

Sub sw46_Hit:Controller.Switch(46) = 1:PlaySound "fx_metalrolling":ActiveBall.VelY = 10:End Sub
Sub sw46_UnHit:Controller.Switch(46) = 0:End Sub

Sub sw44_Hit:Controller.Switch(44) = 1:End Sub
Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub

Sub sw37_Hit:Controller.Switch(37) = 1:End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub

Sub sw64_Hit:Controller.Switch(64) = 1:End Sub
Sub sw64_UnHit:Controller.Switch(64) = 0:End Sub

Sub sw63_Hit:Controller.Switch(63) = 1:End Sub
Sub sw63_UnHit:Controller.Switch(63) = 0:End Sub

Sub sw115_Hit:Controller.Switch(115) = 1:End Sub
Sub sw115_UnHit:Controller.Switch(115) = 0:End Sub

Sub sw117_Hit:Controller.Switch(117) = 1:End Sub
Sub sw117_UnHit:Controller.Switch(117) = 0:End Sub

Sub sw12_Hit:Controller.Switch(12) = 1:End Sub
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub
Sub sw1_Hit:LeftFlipper1.RotateToStart:End Sub
Sub sw2_Hit:LeftFlipper1.RotateToStart:End Sub
Sub sw3_Hit:LeftFlipper1.RotateToStart:End Sub

Sub bask0_hit:EMreel1.setvalue (1):End Sub
Sub bask1_hit:EMreel1.setvalue (2):End Sub
Sub bask2_hit:EMreel1.setvalue (1):End Sub
Sub bask3_hit:EMreel1.setvalue (0):End Sub

'Targets
Sub sw41_Hit:sw41.IsDropped = 1:sw41a.IsDropped = 0:Me.TimerEnabled = 1:vpmTimer.PulseSw 41:PlaySound "fx_target":End Sub
Sub sw41_Timer:sw41.IsDropped = 0:sw41a.IsDropped = 1:Me.TimerEnabled = 0:End Sub

Sub sw42_Hit:sw42.IsDropped = 1:sw42a.IsDropped = 0:Me.TimerEnabled = 1:vpmTimer.PulseSw 42:PlaySound "fx_target":End Sub
Sub sw42_Timer:sw42.IsDropped = 0:sw42a.IsDropped = 1:Me.TimerEnabled = 0:End Sub

Sub sw43_Hit:sw43.IsDropped = 1:sw43a.IsDropped = 0:Me.TimerEnabled = 1:vpmTimer.PulseSw 43:PlaySound "fx_target":End Sub
Sub sw43_Timer:sw43.IsDropped = 0:sw43a.IsDropped = 1:Me.TimerEnabled = 0:End Sub

Sub sw18_Hit:sw18.IsDropped = 1:sw18a.IsDropped = 0:Me.TimerEnabled = 1:vpmTimer.PulseSw 18:PlaySound "fx_target":End Sub
Sub sw18_Timer:sw18.IsDropped = 0:sw18a.IsDropped = 1:Me.TimerEnabled = 0:End Sub

Sub sw28_Hit:sw28.IsDropped = 1:sw28a.IsDropped = 0:Me.TimerEnabled = 1:vpmTimer.PulseSw 28:PlaySound "fx_target":End Sub
Sub sw28_Timer:sw28.IsDropped = 0:sw28a.IsDropped = 1:Me.TimerEnabled = 0:End Sub

'***********
' Solenoids
'***********

SolCallBack(1) = "Auto_Plunger"
'SolCallBack(2)	= Not Used
SolCallBack(3) = "vpmSolWall Diverter2,True,"
SolCallBack(4) = "vpmSolWall Diverter1,True,"
SolCallBack(5) = "bsEject.SolOut"
SolCallBack(6) = "RightGate.Open ="
SolCallBack(7) = "SolBasket"
'SolCallBack(8)	' magnet - handled in the magnet definition
SolCallBack(9) = "bsTrough.SolOut"
'SolCallBack(10)	= "vpmSolSound ""lSling"","
'SolCallBack(11)	= "vpmSolSound ""lSling"","
'SolCallBack(12)	= "vpmSolSound ""Jet1"","
'SolCallBack(13)	= "vpmSolSound ""Jet1"","
'SolCallBack(14)	= "vpmSolSound ""Jet1"","

SolCallBack(15) = "PassRight2"
SolCallBack(16) = "PassLeft2"

SolCallBack(17) = "SetLamp 117,"
SolCallBack(18) = "SetLamp 118,"
SolCallBack(19) = "SetLamp 119,"
SolCallBack(20) = "SetLamp 120,"
SolCallBack(22) = "SetLamp 122,"
SolCallBack(24) = "SetLamp 124,"

SolCallBack(25) = "PassRight1"
SolCallBack(26) = "PassLeft3"
SolCallBack(27) = "PassRight3"
SolCallBack(28) = "PassLeft4"

SolCallBack(33) = "bsSaucer1.SolOut"
SolCallBack(34) = "bsSaucer2.SolOut"
SolCallBack(35) = "bsSaucer3.SolOut"
SolCallBack(36) = "bsSaucer4.SolOut"
'SolCallBack(37)	= Motor Enable (defender) - handled in the mech
'SolCallBack(38)	= Motor Direction (defender) - handled in the mech
'SolCallBack(39) = "ClockEnable"
'SolCallBack(40) = "ClockCount"

Sub Auto_Plunger(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
    End If
End Sub

Sub SolBasket(Enabled)
    If Enabled Then
        BackBall.Kick 330, 75
If bgball=1 then LeftFlipper1.RotateToEnd
bgball=0
    End If
End Sub

Sub BackBall1_Hit
    BackBall1.Destroyball
	BallReturn1.CreateSizedBall(20).Image = "BasketBall"
	BallReturn1.Kick 100, 15
End Sub
Sub BallReturn2_Hit
BallReturn2.Destroyball
BackBall.CreateSizedBall(20).Image = "BasketBall"
bgball=1
End Sub

Sub UpdateDefender(aNewPos, aSpeed, aLastPos)
    ADef1(aLastPos).IsDropped = True
    ADef1(aNewPos).IsDropped = False
    ADef2(aLastPos).visible = 0:ADef2(aLastPos).collidable = 0
    ADef2(aNewPos).visible = 1:ADef2(aNewPos).collidable = 1
   'DefR.State = ABS(DefR.State -1)
End Sub

' In The Paint

Sub PassRight1(Enabled)
    If Enabled then
        bsSaucer1.Kickz = 0
        bsSaucer1.InitAltKick 110, 6
        bsSaucer1.ExitAltSol_On
        bsSaucer1.Kickz = 1.2
    End If
End Sub

Sub PassRight2(Enabled)
    If Enabled then
        bsSaucer2.Kickz = 0
        bsSaucer2.InitAltKick 70, 8
        bsSaucer2.ExitAltSol_On
        bsSaucer2.Kickz = 1.15
    End If
End Sub

Sub PassRight3(Enabled)
    If Enabled then
        bsSaucer3.Kickz = 0
        bsSaucer3.InitAltKick 45, 9
        bsSaucer3.ExitAltSol_On
        bsSaucer3.Kickz = 1.15
    End If
End Sub

Sub PassLeft2(Enabled)
    If Enabled then
        bsSaucer2.Kickz = 0
        bsSaucer2.InitAltKick 310, 8
        bsSaucer2.ExitAltSol_On
        bsSaucer2.Kickz = 1.15
    End If
End Sub

Sub PassLeft3(Enabled)
    If Enabled then
        bsSaucer3.Kickz = 0
        bsSaucer3.InitAltKick 295, 7.8
        bsSaucer3.ExitAltSol_On
        bsSaucer3.Kickz = 1.15
    End If
End Sub

Sub PassLeft4(Enabled)
    If Enabled then
        bsSaucer4.Kickz = 0
        bsSaucer4.InitAltKick 250, 6
        bsSaucer4.ExitAltSol_On
        bsSaucer4.Kickz = 1.2
    End If
End Sub



'***************************************
'     Special JP Flippers, including:
' - tap code by Jimmifingers
' - recoil fix to enable dropcatches
' - ball hit sound
'**************************************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
'Dim StartLeftFlipperStrength, StartRightFlipperStrength
'Dim StartLeftFlipperSpeed, StartRightFlipperSpeed
'Dim StartLeftFlipperReturn, StartRightFlipperReturn
'Dim StartLeftFlipperRecoil, StartRightFlipperRecoil
'
'StartLeftFlipperStrength = LeftFlipper.Strength
'StartRightFlipperStrength = RightFlipper.Strength
'StartLeftFlipperSpeed = LeftFlipper.Speed
'StartRightFlipperSpeed = RightFlipper.Speed
'StartLeftFlipperReturn = LeftFlipper.Return
'StartRightFlipperReturn = RightFlipper.Return
'StartLeftFlipperRecoil = LeftFlipper.Recoil
'StartRightFlipperRecoil = LeftFlipper.Recoil

Sub SolLFlipper(Enabled)
    If Enabled Then
        LeftFlipper.TimerEnabled = 0
        PlaySound "fx_flipperup1"
        LeftFlipper.RotateToEnd
    Else
'        LFTCount = 1
        PlaySound "fx_flipperdown":
'        LeftFlipper.Recoil = 0.5 'decrease the recoil to allow drop catches
'        LeftFlipper.Speed = .05  'Temporarily drop speed for slower back draw to help visuals on quick tap
'        LeftFlipper.Return = 0.3 'Increase Return strength to compensate for speed drop on return to help against weak ball hit strength from underneath flipper (draining position)
'
        LeftFlipper.RotateToStart
'        LeftFlipper.Strength = StartLeftFlipperStrength * (LFTCount / 6)
'        LeftFlipper.TimerEnabled = 1
'        LeftFlipper.Speed = StartLeftFlipperSpeed
'        LeftFlipper.Return = StartLeftFlipperReturn
'        LeftFlipper.Recoil = StartLeftFlipperRecoil
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        RightFlipper.TimerEnabled = 0
        PlaySound "fx_flipperup1"
        RightFlipper.RotateToEnd

    Else
'        RFTCount = 1
        PlaySound "fx_flipperdown":
'        RightFlipper.Recoil = 0.5 'decrease the recoil to allow drop catches
'        RightFlipper.Speed = .05  'Temporarily drop speed for slower back draw to help visuals on quick tap
'        RightFlipper.Return = 0.3 'Increase Return strength to compensate for speed drop on return to help against weak ball hit strength from underneath flipper (draining position)
'
        RightFlipper.RotateToStart

'        RightFlipper.Strength = StartRightFlipperStrength * (LFTCount / 6)
'        RightFlipper.TimerEnabled = 1
'        RightFlipper.Speed = StartRightFlipperSpeed
'        RightFlipper.Return = StartRightFlipperReturn
'        RightFlipper.Recoil = StartRightFlipperRecoil
    End If
End Sub


Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper"
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper"
End Sub

'***********************
'     Flipper Logos
'***********************

Sub UpdateFlipperLogos
    LFLogo.RotAndTra2 = LeftFlipper.CurrentAngle
    RFlogo.RotAndTra2 = RightFlipper.CurrentAngle
End Sub

'******************
' RealTime Updates
'******************
Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
    RollingSound
    UpdateFlipperLogos
End Sub

'*************************
'   Ball Rolling Sound
' Based on rascal's code
'*************************
Dim VeloY(3), VeloX(3), rolling(3), b
b = 0

Sub RollingSound()
    b = b + 1
    If b > 3 Then b = 1
    If BallStatus(b) = 0 Then
        If rolling(b) = True Then
            StopSound "fx_ballrolling" &b
            rolling(b) = False
            Exit Sub
        Else
            Exit Sub
        End If
    End if

    VeloY(b) = Cint(CurrentBall(b).VelY)
    VeloX(b) = Cint(CurrentBall(b).VelX)
    If(ABS(VeloY(b) ) > 3 or ABS(VeloX(b) ) > 3) and CurrentBall(b).Z < 55 Then 'do not sound if the ball is on a ramp
        If rolling(b) = True then
            Exit Sub
        Else
            rolling(b) = True
            PlaySound "fx_ballrolling" &b
        End If
    Else
        If rolling(b) = True Then
            StopSound "fx_ballrolling" &b
            rolling(b) = False
        End If
    End If
End Sub

'*************************************************
' destruk's new vpmCreateBall for ball collision
' use it: vpmCreateBall kicker
'*************************************************

Set vpmCreateBall = GetRef("mycreateball")
Function mycreateball(aKicker)
    For cnt = 1 to ubound(ballStatus)
        If ballStatus(cnt) = 0 Then
            If Not IsEmpty(vpmBallImage) Then
                Set CurrentBall(cnt) = aKicker.CreateSizedBall(BSize).Image
            Else
                Set CurrentBall(cnt) = aKicker.CreateSizedBall(BSize)
            End If
            Set mycreateball = aKicker
            currentball(cnt).uservalue = cnt
            ballStatus(cnt) = 1
            ballStatus(0) = ballStatus(0) + 1
            If coff = False Then
                If ballStatus(0) > 1 and XYdata.enabled = False Then XYdata.enabled = True
            End If
            Exit For
        End If
    Next
End Function

'**************************************************************
' vpm Ball Collision based on the code by Steely & Pinball Ken
' added destruk's changes, ball size and height check by koadic
'**************************************************************

Const tnopb = 10 'max nr. of balls
Const nosf = 10  'nr. of sound files

ReDim CurrentBall(tnopb), BallStatus(tnopb)
Dim iball, cnt, coff, errMessage

XYdata.interval = 1
coff = False

For cnt = 0 to ubound(BallStatus):BallStatus(cnt) = 0:Next

' Create ball in kicker and assign a Ball ID used mostly in non-vpm tables
Sub CreateBallID(aKicker)
    For cnt = 1 to ubound(ballStatus)
        If ballStatus(cnt) = 0 Then
            Set CurrentBall(cnt) = aKicker.CreateSizedBall(Bsize)
            currentball(cnt).uservalue = cnt
            ballStatus(cnt) = 1
            ballStatus(0) = ballStatus(0) + 1
            If coff = False Then
                If ballStatus(0) > 1 and XYdata.enabled = False Then XYdata.enabled = True
            End If
            Exit For
        End If
    Next
End Sub

Sub ClearBallID
    On Error Resume Next
    iball = ActiveBall.uservalue
    currentball(iball).UserValue = 0
    If Err Then Msgbox Err.description & vbCrLf & iball
    ballStatus(iBall) = 0
    ballStatus(0) = ballStatus(0) -1
    On Error Goto 0
End Sub

' Ball data collection and B2B Collision detection. jpsalas: added height check
ReDim baX(tnopb, 4), baY(tnopb, 4), baZ(tnopb, 4), bVx(tnopb, 4), bVy(tnopb, 4), TotalVel(tnopb, 4)
Dim cForce, bDistance, xyTime, cFactor, id, id2, id3, B1, B2

Sub XYdata_Timer()
    xyTime = Timer + (XYdata.interval * .001)
    If id2 >= 4 Then id2 = 0
    id2 = id2 + 1
    For id = 1 to ubound(ballStatus)
        If ballStatus(id) = 1 Then
            baX(id, id2) = round(currentball(id).x, 2)
            baY(id, id2) = round(currentball(id).y, 2)
            baZ(id, id2) = round(currentball(id).z, 2)
            bVx(id, id2) = round(currentball(id).velx, 2)
            bVy(id, id2) = round(currentball(id).vely, 2)
            TotalVel(id, id2) = (bVx(id, id2) ^2 + bVy(id, id2) ^2)
            If TotalVel(id, id2) > TotalVel(0, 0) Then TotalVel(0, 0) = int(TotalVel(id, id2) )
        End If
    Next

    id3 = id2:B2 = 2:B1 = 1
    Do
        If ballStatus(B1) = 1 and ballStatus(B2) = 1 Then
            bDistance = int((TotalVel(B1, id3) + TotalVel(B2, id3) ) ^(1.04 * (CurrentBall(B1).radius + CurrentBall(B2).radius) / 50) )
            If((baX(B1, id3) - baX(B2, id3) ) ^2 + (baY(B1, id3) - baY(B2, id3) ) ^2) < (2800 * ((CurrentBall(B1).radius + CurrentBall(B2).radius) / 50) ^2) + bDistance Then
                If ABS(baZ(B1, id3) - baZ(B2, id3) ) < (CurrentBall(B1).radius + CurrentBall(B2).radius) Then collide B1, B2:Exit Sub
            End If
        End If
        B1 = B1 + 1
        If B1 = ubound(ballstatus) Then Exit Do
        If B1 >= B2 then B1 = 1:B2 = B2 + 1
    Loop

    If ballStatus(0) <= 1 Then XYdata.enabled = False

    If XYdata.interval >= 40 Then coff = True:XYdata.enabled = False
    If Timer > xyTime * 3 Then coff = True:XYdata.enabled = False
    If Timer > xyTime Then XYdata.interval = XYdata.interval + 1
End Sub

'Calculate the collision force and play sound
Dim cTime, cb1, cb2, avgBallx, cAngle, bAngle1, bAngle2

Sub Collide(cb1, cb2)
    If TotalVel(0, 0) / 1.8 > cFactor Then cFactor = int(TotalVel(0, 0) / 1.8)
    avgBallx = (bvX(cb2, 1) + bvX(cb2, 2) + bvX(cb2, 3) + bvX(cb2, 4) ) / 4
    If avgBallx < bvX(cb2, id2) + .1 and avgBallx > bvX(cb2, id2) -.1 Then
        If ABS(TotalVel(cb1, id2) - TotalVel(cb2, id2) ) < .000005 Then Exit Sub
    End If
    If Timer < cTime Then Exit Sub
    cTime = Timer + .1
    GetAngle baX(cb1, id3) - baX(cb2, id3), baY(cb1, id3) - baY(cb2, id3), cAngle
    id3 = id3 - 1:If id3 = 0 Then id3 = 4
    GetAngle bVx(cb1, id3), bVy(cb1, id3), bAngle1
    GetAngle bVx(cb2, id3), bVy(cb2, id3), bAngle2
    cForce = Cint((abs(TotalVel(cb1, id3) * Cos(cAngle-bAngle1) ) + abs(TotalVel(cb2, id3) * Cos(cAngle-bAngle2) ) ) )
    If cForce < 4 Then Exit Sub
    cForce = Cint((cForce) / (cFactor / nosf) )
    If cForce > nosf-1 Then cForce = nosf-1
    PlaySound("fx_collide" & cForce)
End Sub

' Get angle
Dim Xin, Yin, rAngle, Radit, wAngle, Pi
Pi = Round(4 * Atn(1), 6) '3.1415926535897932384626433832795

Sub GetAngle(Xin, Yin, wAngle)
    If Sgn(Xin) = 0 Then
        If Sgn(Yin) = 1 Then rAngle = 3 * Pi / 2 Else rAngle = Pi / 2
        If Sgn(Yin) = 0 Then rAngle = 0
        Else
            rAngle = atn(- Yin / Xin)
    End If
    If sgn(Xin) = -1 Then Radit = Pi Else Radit = 0
    If sgn(Xin) = 1 and sgn(Yin) = 1 Then Radit = 2 * Pi
    wAngle = round((Radit + rAngle), 4)
End Sub

'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 10 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
    UpdateLamps
End Sub

Sub UpdateLamps
    NfadeL 11, l11
    NfadeL 12, l12
    NfadeL 13, l13
    NfadeL 14, l14
    NfadeL 15, l15
    NfadeL 16, l16
    NfadeL 17, l17
    NfadeL 18, l18
    NfadeL 21, l21
    NfadeL 22, l22
    NfadeL 23, l23
    NfadeL 24, l24
    NfadeL 25, l25
    NfadeL 26, l26
    NfadeL 27, l27
    NfadeL 28, l28
    NfadeL 31, l31
    NfadeL 32, l32
    NfadeL 33, l33
    NfadeL 34, l34
    NfadeL 35, l35
    NfadeL 36, l36
    NfadeL 37, l37
    NfadeL 38, l38
    NfadeL 41, l41
    NfadeL 42, l42
    NfadeL 43, l43
    NfadeL 44, l44
    NfadeL 45, l45
    NfadeL 46, l46
    NfadeL 47, l47
    NfadeL 48, l48
    NfadeL 51, l51
    NfadeL 52, l52
    NfadeL 53, l53
    NfadeL 54, l54
    NfadeL 55, l55
    NfadeL 56, l56
    NfadeL 57, l57
    NfadeL 58, l58
    NFadeLm 61, l61b
    NfadeL 61, l61
    NfadeL 62, l62
    NfadeL 63, l63
    NfadeL 64, l64
    NfadeL 65, l65
    NfadeL 66, l66
    NfadeL 71, l71
    NfadeL 72, l72
    NfadeL 73, l73
    NfadeL 74, l74
    NfadeL 75, l75
    NfadeL 76, l76
    NfadeL 81, l81
    NfadeL 82, l82
    NfadeL 83, l83
    NfadeL 84, l84
    NfadeL 85, l85
    NfadeL 86, l86
    NfadeL 87, l87
    NfadeL 88, l88

'    fadeobj 67, l67, "of_on", "of_a", "of_b", "empty"
'    fadeobj 68, l68, "of_on", "of_a", "of_b", "empty"
'    fadeobj 77, l77, "of_on", "of_a", "of_b", "empty"
'    fadeobj 78, l78, "of_on", "of_a", "of_b", "empty"
flash 67,f67
flash 68,f68
flash 77,f77
flash 78,f78



    ' flashers old
'   fadeobj 117, f17, "rf_on", "rf_a", "rf_b", "empty"
'	NFadeLm 118, bumper1
'	fadeobj 118, f18, "rf_on", "rf_a", "rf_b", "empty"
'   fadeobj 119, f19, "wf_on", "wf_a", "wf_b", "empty"
'   fadeobj 120, f20, "bf_on", "bf_a", "bf_b", "empty"
'   fadeobjm 124, f24, "wf_on", "wf_a", "wf_b", "empty"
'   fadeobj 124, f24b, "bf_on", "bf_a", "bf_b", "empty"

'	flashers new
	flash 117,f17
	flash 118,f18
	flash 119,f19
	flash 120,f20
	flashm 124,f24b
	flash 124,f24
End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

' Flasher objects

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
End Sub

' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub FadeR(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.SetValue 0:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1              'wait
        Case 9:object.SetValue 2:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1          'wait
        Case 13:object.SetValue 3:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeRm(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1
        Case 5:object.SetValue 0
        Case 9:object.SetValue 2
        Case 3:object.SetValue 3
    End Select
End Sub

'Texts

Sub NFadeT(nr, object, message)
    Select Case FadingLevel(nr)
        Case 4:object.Text = "":FadingLevel(nr) = 0
        Case 5:object.Text = message:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, object, b)
    Select Case FadingLevel(nr)
        Case 4:object.Text = ""
        Case 5:object.Text = message
    End Select
End Sub


'********************
' Diverse Help/Sounds
'********************

Sub ARubbers_Hit(idx):PlaySound "fx_rubber":End Sub
Sub APostRubbers_Hit(idx):PlaySound "fx_rubber":End Sub
Sub AMetals_Hit(idx):PlaySound "fx_MetalHit":End Sub
Sub AGates_Hit(idx):PlaySound "fx_Gate":End Sub
Sub APlastics_Hit(idx):PlaySound "fx_plastichit":End Sub

Sub RHelp1_Hit:PlaySound "fx_ballhit":End Sub
Sub RHelp2_Hit:PlaySound "fx_ballhit":End Sub
Sub RHelp5_Hit:PlaySound "fx_ballhit":End Sub
Sub RHelp3_Hit
    If ActiveBall.VelY < -10 Then
        ActiveBall.VelY = -10
    End If
End Sub

Sub RHelp4_Hit
    ActiveBall.VelX = -5
End Sub

' ===============================================================================================
' LED's display
' ===============================================================================================
Dim Digits(2), digitState
 
Digits(0)   = Array(led0,led1,led2,led3,led4,led5,led6)
Digits(1)   = Array(led7,led8,led9,led10,led11,led12,led13)
 
Sub UpdateLED_Timer()
    Dim chgLED, ii, iii, num, chg, stat, digit
    chgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(chgLED) Then
        For ii = 0 To UBound(chgLED)
            num  = chgLED(ii, 0)
            chg  = chgLED(ii, 1)
            stat = chgLED(ii, 2)
            iii  = 0
            For Each digit In Digits(num)
                If (chg And 1) Then digit.State = (stat And 1)
                chg  = chg \ 2
                stat = stat \ 2
            Next
        Next
    End If
End Sub

Sub ShotClock_Timer
If RedClock=1 then ShotClock.Enabled=False
If RedClock=1 then RedShotClock.Enabled=True

If LED0.State=1 then SCleft0.visible =true else SCleft0.visible =false
If LED1.State=1 then SCleft1.visible =true else SCleft1.visible =false
If LED2.State=1 then SCleft2.visible =true else SCleft2.visible =false
If LED3.State=1 then SCleft3.visible =true else SCleft3.visible =false
If LED4.State=1 then SCleft4.visible =true else SCleft4.visible =false
If LED5.State=1 then SCleft5.visible =true else SCleft5.visible =false
If LED6.State=1 then SCleft6.visible =true else SCleft6.visible =false

If LED7.State=1 then SCright0.visible =true else SCright0.visible =false
If LED8.State=1 then SCRight1.visible =true else SCRight1.visible =false
If LED9.State=1 then SCRight2.visible =true else SCRight2.visible =false
If LED10.State=1 then SCRight3.visible =true else SCRight3.visible =false
If LED11.State=1 then SCRight4.visible =true else SCRight4.visible =false
If LED12.State=1 then SCRight5.visible =true else SCRight5.visible =false
If LED13.State=1 then SCRight6.visible =true else SCRight6.visible =false

If l87.State=1 then Flasher2.visible =true else Flasher2.visible =false
If l88.State=1 then Flasher4.visible =true else Flasher4.visible =false

End Sub

Sub RedShotClock_Timer

If LED0.State=1 then SCleft0r.visible =true else SCleft0r.visible =false
If LED1.State=1 then SCleft1r.visible =true else SCleft1r.visible =false
If LED2.State=1 then SCleft2r.visible =true else SCleft2r.visible =false
If LED3.State=1 then SCleft3r.visible =true else SCleft3r.visible =false
If LED4.State=1 then SCleft4r.visible =true else SCleft4r.visible =false
If LED5.State=1 then SCleft5r.visible =true else SCleft5r.visible =false
If LED6.State=1 then SCleft6r.visible =true else SCleft6r.visible =false

If LED7.State=1 then SCright0r.visible =true else SCright0r.visible =false
If LED8.State=1 then SCRight1r.visible =true else SCRight1r.visible =false
If LED9.State=1 then SCRight2r.visible =true else SCRight2r.visible =false
If LED10.State=1 then SCRight3r.visible =true else SCRight3r.visible =false
If LED11.State=1 then SCRight4r.visible =true else SCRight4r.visible =false
If LED12.State=1 then SCRight5r.visible =true else SCRight5r.visible =false
If LED13.State=1 then SCRight6r.visible =true else SCRight6r.visible =false

If l87.State=1 then Flasher2.visible =true else Flasher2.visible =false
If l88.State=1 then Flasher4.visible =true else Flasher4.visible =false

End Sub
