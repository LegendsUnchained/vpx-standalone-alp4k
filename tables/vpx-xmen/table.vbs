' **** XMEN LE ****
'
' This table is based on the original FP conversion by Freneticamnesic
' Brought to you by: ICPJuggla, HauntFreaks, DJRobX and Arngrim
'
 
  Option Explicit
    Randomize
 
Const Ballsize = 51
 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
 
'***********
'Preferences
'***********
 
'Real GI is made from 3 lights - Red, White, and Blue.  This makes some combinations not very different when white is toggled.  
'7 Color mod changes white to green.
Const Use7ColorGI = False
Const EnableFlipperShadows = False
Const EnableIceManShadow = False
Const EnableCheats = False
 
 
Dim DesktopMode: DesktopMode = Table.ShowDT
Dim UseVPMDMD: UseVPMDMD = False
 
If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Primitive58.visible=1
'UseVPMDMD = True   
Else
Ramp16.visible=0
Ramp15.visible=0
Primitive58.visible=0
End if
 
Const UseVPMModSol = True
 
  LoadVPM "01560000", "sam.VBS", 3.10
 
 
 
'*************************************************************
'Toggle DOF sounds on/off based on cController value
'*************************************************************
 
Sub DOF(dofevent, dofstate)
    If cNewController>2 Then
        If dofstate = 2 Then
            Controller.B2SSetData dofevent, 1:Controller.B2SSetData dofevent, 0
        Else
            Controller.B2SSetData dofevent, dofstate
        End If
    End If
End Sub
 
 
 
'********************
'Standard definitions
'********************
 
    Const cGameName = "xmn_151h"
 
     Const UseSolenoids = 1
     Const UseLamps = 0
     Const UseSync = 1
     Const HandleMech = 0
 
     'Standard Sounds
     Const SSolenoidOn = "Solenoid"
     Const SSolenoidOff = ""
     Const SCoin = "CoinIn"
 
 
   'Variables
    Dim xx
    Dim bsL,mTLMag,mTRMag,mDMag,tbTrough,Bump1, Bump2, Bump3, bsRHole, MsLHole, bsLHole, bsTrough, bsSaucer, DTBank5, DTBank4, DTBank3, mDiverter
    Dim PlungerIM,ttSpinner
    Dim GI_State, GI_WhiteOn, GI_RedOn, GI_BlueOn
    GI_State = 0: GI_WhiteOn = 1: GI_RedOn = 1: GI_BlueOn = 1
 
 
     'Table Init
  Sub Table_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        .SplashInfoLine = "XMEN LE, Stern 2012"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 1
        if DesktopMode then
            .Hidden = 0
        Else
            .Hidden = 0
        end if
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
 
    '***Left Hole bsLHole
     Set bsLHole = New cvpmBallStack
     With bsLHole
         .InitSw 0, 4, 0, 0, 0, 0, 0, 0
         .InitKick sw4, 168, 12
         .KickZ = 0.4
         .InitExitSnd SoundFX("popper_ball",DOFContactors), SoundFX("Solenoid",DOFContactors)
         .KickForceVar = 2
     End With
 
 
 
    Set mTLMag= New cvpmMagnet
    With mTLMag
        .InitMagnet Magnet1, 16  
        .GrabCenter = False
        .solenoid=51   ' LE
'       .solenoid=32   ' Pro
        .CreateEvents "mTLMag"
    End With
 
    Set mDMag= New cvpmMagnet
    With mDMag
        .InitMagnet Magnet3, 16  
        .GrabCenter = True
        .CreateEvents "mDMag"
    End With
 
 
    Set ttSpinner = New cvpmTurntable
    ttSpinner.InitTurntable TurnTable, 20
    ttSpinner.SpinDown = 10
    ttSpinner.CreateEvents "ttSpinner"
 
    if not EnableFlipperShadows then
        FlipperLSh.visible = 0
        FlipperRSh.visible = 0
        FlipperRSh1.visible = 0
    end if
    if not EnableIceManShadow Then 
        IceManRampShadow.visible =0
    end if
 
    if EnableCheats then
        CheatWall1.Visible = 1
        CheatWall1.IsDropped = 0
        CheatWall2.Visible = 1
        CheatWall2.IsDropped = 0
        CheatWall3.Visible = 1
        CheatWall3.IsDropped = 0
    Else
        CheatWall1.Visible = 0
        CheatWall1.IsDropped = 1
        CheatWall2.Visible = 0
        CheatWall2.IsDropped = 1
        CheatWall3.Visible = 0
        CheatWall3.IsDropped = 1
    end if
       
       
   
    'Wolvie2.isdropped=1
'wr2.alpha = 0
 
'************************************************************************************************************************************
 
    'DropTargets
 
      '**Main Timer init
           PinMAMETimer.Enabled = 1
 
'   Set bsSaucer=New cvpmBallStack
'   bsSaucer.InitSaucer S37,37,283,15
'   bsSaucer.InitExitSnd"Popper","SolOn"
 
'Nudging
    vpmNudge.TiltSwitch=-7
    vpmNudge.Sensitivity=3
    vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot)
 
  'Magneto Lock
    lockPin1.Isdropped=1:lockPin2.Isdropped=1
  'switches
'    sw1b.IsDropped = 1
'    sw2b.IsDropped = 1
'    sw7b.IsDropped = 1
'    sw8b.IsDropped = 1
'    sw41b.IsDropped = 1
'    sw42b.IsDropped = 1
  End Sub
 
   Sub Table_Paused:Controller.Pause = 1:End Sub
   Sub Table_unPaused:Controller.Pause = 0:End Sub
   Sub Table_exit()
 
 Controller.Stop
 
End sub
 
 
'*****Keys
 Sub Table_KeyDown(ByVal keycode)
 
    If Keycode = LeftFlipperKey then
        'SolLFlipper true
    End If
    If Keycode = RightFlipperKey then
'       SolRFlipper true
    End If
    If keycode = PlungerKey Then Plunger.Pullback: PlaysoundAt "plungerpull",Plunger
'   If keycode = LeftTiltKey Then LeftNudge 80, 1, 20
'    If keycode = RightTiltKey Then RightNudge 280, 1, 20
'    If keycode = CenterTiltKey Then CenterNudge 0, 1, 25
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
    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "plunger",Plunger
    End If
End Sub
 
   'Solenoids
SolCallback(1) = "solTrough"
SolCallback(2) = "solAutofire"
SolCallback(3) = "bsLHole.SolOut"
SolModCallback(4) = "SolMagnetoMagnet"
SolCallback(5) = "bsL.SolOut"
SolCallback(6) = "CLockUp"
SolCallback(7) = "CLockLatch"
SolModCallback(9)  = "SetLampMod 141,"
SolModCallback(10) = "SetLampMod 142,"
SolModCallback(11) = "SetLampMod 143,"
'SolCallback(12) = "SolURFlipper"
SolCallback(15) = "SolLFlipper"
SolCallback(16) = "SolRFlipper"
SolCallback(26) = "vpmSolDiverter RampDiverter,SoundFX(""Diverter"",DOFContactors),"
 
' LE Only
SolCallback(23) = "solDiscMotor"
SolModCallback(30) = "SetLampMod 127, "
SolModCallback(31) = "SetLampMod 131,"   'LE = Bottom Arch
SolCallback(27) = "solIceManMotor"
'SolCallback(51) = "solWolverineMagnet" ' cvpmMagnet handles this for us.
SolCallback(52) = "solLeftNightcrawler"
SolCallback(53) = "solRightNightcrawler"
SolCallback(54) = "solGIWhite"  
SolCallback(55) = "solGIRed"
SolCallback(56) = "solGIBlue"
SolCallback(57) = "solLeftNightcrawlerLatch"
SolCallback(58) = "solRightNightcrawlerLatch"
 
' PRO only
'SolModCallback(27) = "SetLampMod 127,"
 
' Modulated Flasher and Lights objects
 
SolModCallback(17) = "SetLampMod 117,"
SolModCallback(18) = "SetLampMod 118,"
SolModCallback(19) = "SetLampMod 119,"
SolModCallback(20) = "SetLampMod 120,"
SolModCallback(21) = "SetLampMod 121,"
SolModCallback(22) = "SetLampMod 122,"
SolModCallback(25) = "SetLampMod 125,"
SolModCallback(28) = "SetLampMod 128,"
SolModCallback(29) = "SetLampMod 129,"
SolModCallback(32) = "SetLampMod 132,"
 
Dim IceManMotor:IceManMotor = 0
Dim IceManDir:IceManDir = -1
Dim IceManAngle:IceManAngle = 90
Dim IceManSpeed:IceManSpeed = .08
Dim IceManHold:IceManHold = 0
Const IceManMaxAngle = 65
Const IceManMinAngle = 15
 
Sub TimerIceManMotor_Timer()
    ' VP doesn't do dynamic collideable objects.   We want to pause the ramp movement if a ball is on the ramp and the ramp is at a destination.  
    If IceManHold > 0 AND (IceManAngle = IceManMaxAngle OR IceManAngle = IceManMinAngle) Then Exit Sub
    ' Reset just in case is less than 0
    IceManHold = 0
    IceManAngle = IceManAngle + IceManDir * IceManSpeed
    if IceManAngle > IceManMaxAngle Then
        IceManAngle = IceManMaxAngle
        IceManDir = -1
        Controller.Switch(35) = 1
        StopSound "RampMotor"
        me.Enabled = 0
    Elseif IceManAngle < IceManMinAngle Then
        IceManAngle = IceManMinAngle
        IceManDir = 1
        Controller.Switch(34) = 1
        StopSound "RampMotor"
        me.Enabled = 0
    Else       
        select case IceManDir
        case -1:
            IceManRampA.Collidable = 0
            IceManRampB.Collidable  = 1
        case 1:
            IceManRampA.Collidable = 1
            IceManRampB.Collidable = 0
        end select
        Controller.Switch(34) = 0
        Controller.Switch(35) = 0
    End If
    IceManRamp.RotY = IceManAngle  
    if EnableIceManShadow then
        IceManRampShadow.RotZ = 220 + IceManAngle - IceManminAngle
    end if
End Sub
 
Sub solIceManMotor(Enabled)
    TimerIceManMotor.Enabled = Enabled
    if Enabled then PlaySoundAtVol SoundFX("RampMotor",DOFGear), RampBumpMetal1, 0.7
End Sub
 
Sub solMagnetoMagnet(value)
    if Value > 0 then
        mDmag.Strength = 10 * value / 255
        mDMag.MagnetOn = 1
    Else       
        mDMag.MagnetOn = 0
    end If 
end sub
 
 
 
'************************************************************************
'                               Night Crawlers
'************************************************************************
 
Const NightCrawlerVertSpeed = .6
Const NightCrawlerShakeSpeed = .2
Const NightCrawlerShakeDecay = .99
Const NightCrawlerHitFactor = .8
Const NightCrawlerMin = 0
Const NightCrawlerMax = 80
 
Dim LeftNightCrawlerState:LeftNightCrawlerState = 0
Const NCStateMoveUp = 1
Const NCStateShake = 2
Const NCStateMoveDownShake = 3
Const NCStateMoveDown = 4
 
Dim RightNightCrawlerState:RightNightCrawlerState = 0
Dim LeftNightCrawlerShake:LeftNightCrawlerShake = 0
Dim RightNightCrawlerShake:RightNightCrawlerShake = 0
Dim LeftNightCrawlerShakeAngle:LeftNightCrawlerShakeAngle = 0
Dim RightNightCrawlerShakeAngle:RightNightCrawlerShakeAngle = 0
 
Sub LeftNightCrawlerWall_Hit
    Controller.Switch(50) = 1
    PlaySoundAt "flip_hit_3", ActiveBall
    LeftNightCrawlerShakeAngle = 0
    LeftNightCrawlerShake = BallVel(ActiveBall) * NightCrawlerHitFactor
    LeftNightCrawlerState = NCStateShake
    LeftNightCrawlerWall.TimerEnabled = 1
End Sub
 
Sub LeftNightCrawlerWall_UnHit:Controller.Switch(50) = 0:End Sub
 
Sub RightNightCrawlerWall_Hit
    Controller.Switch(51) = 1
    PlaySoundAt "flip_hit_3", ActiveBall
    RightNightCrawlerShakeAngle = 0
    RightNightCrawlerShake = BallVel(ActiveBall) * NightCrawlerHitFactor
    RightNightCrawlerState = NCStateShake
    RightNightCrawlerWall.TimerEnabled = 1
End Sub
 
Sub RightNightCrawlerWall_UnHit:Controller.Switch(51) = 0:End Sub
 
Sub RightNightCrawlerWall_Timer
    select case RightNightCrawlerState
    case NCStateMoveDown:
        RightNightCrawler.Z = RightNightCrawler.Z - NightCrawlerVertSpeed
        if RightNightCrawler.Z <= NightCrawlerMin then
            RightNightCrawler.Z = NightCrawlerMin
            RightNightCrawlerWall.IsDropped = 1
            Controller.Switch(56) = 1
            Me.TimerEnabled = 0
        end If
    case NCStateMoveUp:
        RightNightCrawler.Z = RightNightCrawler.Z + NightCrawlerVertSpeed
        if RightNightCrawler.Z >= NightCrawlerMax then
            RightNightCrawler.Z = NightCrawlerMax
            RightNightCrawlerWall.IsDropped = 0
            Controller.Switch(56) = 0
            RightNightCrawlerShakeAngle = 0
            RightNightCrawlerShake = 8
            RightNightCrawlerState = NCStateShake
        end If
    case NCStateShake:
        RightNightCrawler.TransY = RightNightCrawlerShake * Sin(RightNightCrawlerShakeAngle)
        RightNightCrawlerShakeAngle = RightNightCrawlerShakeAngle + ((NightCrawlerShakeSpeed * RightNightCrawlerShake) / 30)
        RightNightCrawlerShake = RightNightCrawlerShake * NightCrawlerShakeDecay
        if RightNightCrawlerShake < 1 Then 
            RightNightCrawler.TransY = 0
            Me.TimerEnabled =0
        end If     
    case NCStateMoveDownShake:
        RightNightCrawler.TransY = RightNightCrawlerShake * Sin(RightNightCrawlerShakeAngle)
        RightNightCrawlerShakeAngle = RightNightCrawlerShakeAngle + ((NightCrawlerShakeSpeed * RightNightCrawlerShake) / 30)
        RightNightCrawlerShake = RightNightCrawlerShake * NightCrawlerShakeDecay
        if RightNightCrawlerShake < 1 Then 
            RightNightCrawler.TransY = 0
            RightNightCrawlerState = NCStateMoveDown
        end If     
    end Select
End Sub
 
Sub LeftNightCrawlerWall_Timer
    select case LeftNightCrawlerState
    case NCStateMoveDown:
        LeftNightCrawler.Z = LeftNightCrawler.Z - NightCrawlerVertSpeed
        if LeftNightCrawler.Z <= NightCrawlerMin then
            LeftNightCrawler.Z = NightCrawlerMin
            LeftNightCrawlerWall.IsDropped = 1
            Controller.Switch(12) = 1
            Me.TimerEnabled = 0
        end If
    case NCStateMoveUp:
        LeftNightCrawler.Z = LeftNightCrawler.Z + NightCrawlerVertSpeed
        if LeftNightCrawler.Z >= NightCrawlerMax then
            LeftNightCrawler.Z = NightCrawlerMax
            LeftNightCrawlerWall.IsDropped = 0
            Controller.Switch(12) = 0
            LeftNightCrawlerShakeAngle = 0
            LeftNightCrawlerShake = 8
            LeftNightCrawlerState = NCStateShake
        end If
    case NCStateShake:
        LeftNightCrawler.TransY = LeftNightCrawlerShake * Sin(LeftNightCrawlerShakeAngle)
        LeftNightCrawlerShakeAngle = LeftNightCrawlerShakeAngle + ((NightCrawlerShakeSpeed * LeftNightCrawlerShake) / 30)
        LeftNightCrawlerShake = LeftNightCrawlerShake * NightCrawlerShakeDecay
        if LeftNightCrawlerShake < 1 Then  
            LeftNightCrawler.TransY = 0
            Me.TimerEnabled =0
        end If     
    case NCStateMoveDownShake:
        LeftNightCrawler.TransY = LeftNightCrawlerShake * Sin(LeftNightCrawlerShakeAngle)
        LeftNightCrawlerShakeAngle = LeftNightCrawlerShakeAngle + ((NightCrawlerShakeSpeed * LeftNightCrawlerShake) / 30)
        LeftNightCrawlerShake = LeftNightCrawlerShake * NightCrawlerShakeDecay
        if LeftNightCrawlerShake < 1 Then  
            LeftNightCrawler.TransY = 0
            LeftNightCrawlerState = NCStateMoveDown
        end If     
    end Select
End Sub
 
Sub SolLeftNightCrawler(Enabled)
    If Enabled AND LeftNightCrawler.Z <> NightCrawlerMin then
        LeftNightCrawlerShakeAngle = 0
        LeftNightCrawlerShake = 8
        LeftNightCrawlerState = NCStateMoveDownShake
        LeftNightCrawlerWall.TimerEnabled = 1
        PlaySoundAtVol SoundFX("PopUp",DOFContactors), LeftNightCrawler, .001
LeftNightCrawler.blenddisablelighting = 1 
Else 
LeftNightCrawler.blenddisablelighting = 0.3
    End If
End Sub
 
Sub SolLeftNightCrawlerLatch(Enabled)
    If NOT Enabled AND LeftNightCrawler.Z = NightCrawlerMin Then
        PlaySoundAtVol SoundFX("solenoid",DOFContactors), LeftNightCrawler, .5
        LeftNightCrawlerState = NCStateMoveUp
        LeftNightCrawlerWall.TimerEnabled = 1
    End If
End Sub
 
Sub SolRightNightCrawler(Enabled)
    If Enabled AND RightNightCrawler.Z <> NightCrawlerMin then
        RightNightCrawlerShakeAngle = 0
        RightNightCrawlerShake = 8
        RightNightCrawlerState = NCStateMoveDownShake
        RightNightCrawlerWall.TimerEnabled = 1
        PlaySoundAtVol SoundFX("PopUp",DOFContactors), RightNightCrawler, .001
RightNightCrawler.blenddisablelighting = 1
Else
RightNightCrawler.blenddisablelighting = 0.3
    End If
End Sub
 
Sub SolRightNightCrawlerLatch(Enabled)
    If NOT Enabled AND RightNightCrawler.Z = NightCrawlerMin Then
        PlaySoundAtVol SoundFX("solenoid",DOFContactors), RightNightCrawler, .5
        RightNightCrawlerState = NCStateMoveUp
        RightNightCrawlerWall.TimerEnabled = 1
    End If
End Sub
 
'************************************************************************
'                               Turntable
'************************************************************************
 
 
Sub solDiscMotor(Enabled)
    if Enabled Then    
        ttSpinner.MotorOn = True : Playsound SoundFX("disc_noise",DOFGear),-1,0.04,0,0,-60000,0,0,AudioFade(f120b1):f120b1.state = 1
    Else   
        ttSpinner.MotorOn = False : Stopsound "disc_noise":f120b1.state = 0
    end If
end sub
 
Sub TurnTable_Hit
    ttSpinner.AddBall ActiveBall
    if ttSpinner.MotorOn=true then ttSpinner.AffectBall ActiveBall
End Sub
 
Sub TurnTable_unHit
    ttSpinner.RemoveBall ActiveBall
End Sub
 
Sub solTrough(Enabled)
    If Enabled Then
        bsTrough.ExitSol_On
        vpmTimer.PulseSw 22
    End If
 End Sub
 
Sub solAutofire(Enabled)
    If Enabled Then
        PlungerIM.AutoFire:PlaySoundAt "plunger",Plunger
    End If
 End Sub
 
Sub CLockUp(Enabled)
    If Enabled Then
lockPin1.Isdropped=0:lockPin2.Isdropped=0
    End If
 End Sub
 
Sub LockPin1_Timer
    'Dozer would be proud...
    Me.TimerEnabled = 0
    lockPin1.Isdropped=1:lockPin2.Isdropped=1
End Sub
 
Sub CLockLatch(Enabled)
    If Enabled Then
         lockPin1.TimerInterval=500
         lockPin1.TimerEnabled=True
    End If
 End Sub
 
'Scoop
 Dim aBall, aZpos
 Dim bBall, bZpos
 
 Sub sw4_Hit
     Set bBall = ActiveBall
     PlaySoundAt "kicker_enter_center", ActiveBall
     bZpos = 35
     ClearBallID
     Me.TimerInterval = 2
     Me.TimerEnabled = 1
 End Sub
 
 Sub sw4_Timer
     bBall.Z = bZpos
     bZpos = bZpos-4
     If bZpos <-30 Then
         Me.TimerEnabled = 0
         Me.DestroyBall
         bsLHole.AddBall Me
     End If
 End Sub
 
Sub SolDiverter(enabled)
    If enabled Then
        RampDiverter.rotatetoend : PlaySoundAt SoundFX("Diverter",DOFContactors), l29a
    Else
        RampDiverter.rotatetostart : PlaySoundAt SoundFX("Diverter",DOFContactors), l29a
    End If
End Sub
 
    Set bsL = New cvpmBallStack
    With bsL
        .InitSw 0, 55, 0, 0, 0, 0, 0, 0
        .InitKick sw55, 180, 15
        .InitExitSnd SoundFX("popper_ball",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .KickForceVar = 3
    End With
 
   Sub Drain_Hit():PlaySoundAt "Drain", Drain
    'ClearBallID
    BallCount = BallCount - 1
    bsTrough.AddBall Me
   End Sub
 
Sub sw1_Hit:Me.TimerEnabled = 1:sw1p.TransX = -2:vpmTimer.PulseSw 1:PlaySoundAt SoundFX("target",DOFTargets), ActiveBall:End Sub
Sub sw1_Timer:Me.TimerEnabled = 0:sw1p.TransX = 0:End Sub
Sub sw2_Hit:Me.TimerEnabled = 1:sw2p.TransX = -2:vpmTimer.PulseSw 2:PlaySoundAt SoundFX("target",DOFTargets), ActiveBall:End Sub
Sub sw2_Timer:Me.TimerEnabled = 0:sw2p.TransX = 0:End Sub
Sub sw7_Hit:Me.TimerEnabled = 1:sw7p.TransX = -2:vpmTimer.PulseSw 7:PlaySoundAt SoundFX("target",DOFTargets), ActiveBall:End Sub
Sub sw7_Timer:Me.TimerEnabled = 0:sw7p.TransX = 0:End Sub
Sub sw8_Hit:Me.TimerEnabled = 1:sw8p.TransX = -2:vpmTimer.PulseSw 8:PlaySoundAt SoundFX("target",DOFTargets), ActiveBall:End Sub
Sub sw8_Timer:Me.TimerEnabled = 0:sw8p.TransX = 0:End Sub
Sub sw11_Hit:Controller.Switch(11) = 1:PlaySoundAt "Gate", ActiveBall:End Sub
Sub sw11_UnHit:Controller.Switch(11) = 0:End Sub
Sub sw13_Hit:Controller.Switch(13) = 1:PlaySoundAt "Gate", ActiveBall:End Sub
Sub sw13_UnHit:Controller.Switch(13) = 0:End Sub
Sub sw14_Hit:Controller.Switch(14) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub
Sub sw23_Hit:Controller.Switch(23) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
Sub sw24_Hit:Controller.Switch(24) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
Sub sw25_Hit:Controller.Switch(25) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
Sub sw28_Hit:Controller.Switch(28) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub
Sub sw29_Hit:Controller.Switch(29) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub
Sub sw33_Hit:Controller.Switch(33) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub
Sub sw36_Hit:VengeanceHit 2:vpmTimer.PulseSw 36:PlaySoundAt SoundFX("target",DOFTargets), ActiveBall:End Sub
'Sub sw36_Timer:Wolvie1.IsDropped = 0:Wolvie2.IsDropped = 1:wr1.alpha = 1:wr1.triggersingleupdate:wr2.alpha = 0:wr2.triggersingleupdate:Me.TimerEnabled = 0:End Sub
Sub sw38_Hit:PlaySoundAt "rollover", ActiveBall:Controller.Switch(38)=1:End Sub     'Lock 2
Sub sw38_unHit:Controller.Switch(38)=0:End Sub
Sub sw39_Hit:PlaySoundAt "rollover", ActiveBall:Controller.Switch(39)=1:End Sub     'Lock 3
Sub sw39_unHit:Controller.Switch(39)=0:End Sub
Sub sw40_Hit:PlaySoundAt "rollover", ActiveBall:Controller.Switch(40)=1:End Sub     'Lock 4
Sub sw40_unHit:Controller.Switch(40)=0:End Sub
Sub sw41_Hit:Me.TimerEnabled = 1:sw41p.TransX = -2:vpmTimer.PulseSw 41:PlaySoundAt SoundFX("target",DOFTargets), ActiveBall:End Sub
Sub sw41_Timer:Me.TimerEnabled = 0:sw41p.TransX = 0:End Sub
Sub sw42_Hit:Me.TimerEnabled = 1:sw42p.TransX = -2:vpmTimer.PulseSw 42:PlaySoundAt SoundFX("target",DOFTargets), ActiveBall:End Sub
Sub sw42_Timer:Me.TimerEnabled = 0:sw42p.TransX = 0:End Sub
Sub sw47_Spin:vpmTimer.PulseSw 47::playsoundat "spinner", sw47:End Sub
Sub sw48_Hit:Controller.Switch(48) = 1:PlaySoundAt "Gate", ActiveBall:End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub
Sub sw49_Hit:Controller.Switch(49) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
Sub sw49_UnHit:Controller.Switch(49) = 0:End Sub
Sub sw52_Hit:Controller.Switch(52) = 1:PlaySoundAt "Gate", ActiveBall:IceManHold = IceManHold + 1:sw52.TimerInterval=200:vpmtimer.addtimer 300, "sw52.TimerEnabled=1'":End Sub
Sub sw52_Timer
    if IceManDir = -1 then ' Ramp is going across table
        PlaySoundAtVol "fx_ramp_metal", l30, .2
    Else   
        PlaySoundAtVol "fx_ramp_metal", l7, .2
    end If
    me.TimerEnabled =0
End Sub
Sub sw52_UnHit:Controller.Switch(52) = 0:End Sub
Sub sw53_Hit:Controller.Switch(53) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
Sub sw53_UnHit:Controller.Switch(53) = 0:End Sub
Sub sw54_Hit:Controller.Switch(54) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
Sub sw54_UnHit:Controller.Switch(54) = 0:End Sub
Sub sw55a_Hit:ClearBallId:PlaySoundAt "kicker_enter", sw55a:bsL.AddBall Me:End Sub
 
Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySoundAt SoundFX("FlipperUp",DOFFlippers), f131a
         LeftFlipper.RotateToEnd
     Else
         PlaySoundAt SoundFX("FlipperDown",DOFFlippers), f131a
        LeftFlipper.RotateToStart
     End If
 End Sub
 
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySoundAt SoundFX("FlipperUp",DOFFlippers), f131b
         RightFlipper.RotateToEnd:PlaySound SoundFX("FlipperUpUR",DOFFlippers),0,2,AudioPan(RightFlipper1),0,6000,0,0,AudioFade(RightFlipper1):RightFlipper1.RotateToEnd
     Else
         PlaySoundAt SoundFX("FlipperDown",DOFFlippers), f131b
        RightFlipper.RotateToStart:PlaySound SoundFX("FlipperDownUR",DOFFlippers),0,1,AudioPan(RightFlipper1),0,6000,0,0,AudioFade(RightFlipper1):RightFlipper1.RotateToStart
     End If
 End Sub  
 
 
Dim BallCount:BallCount = 0
   Sub BallRelease_UnHit()
    'NewBallID
        BallCount = BallCount + 1
    End Sub
 
 Dim RStep, LStep
 
 Sub LeftSlingShot_Slingshot
    vpmTimer.PulseSw 26
    PlaySound SoundFX("left_slingshot", DOFContactors)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    Me.TimerEnabled = 1
  End Sub
 
 Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
 End Sub
 
 Sub RightSlingShot_Slingshot
    vpmTimer.PulseSw 27
    PlaySound SoundFX("right_slingshot", DOFContactors)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    Me.TimerEnabled = 1
  End Sub
 
 Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4 RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub
 
 
 
    Const IMPowerSetting = 45
    Const IMTime = 0.6
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0
        .InitExitSnd "plunger2", "plunger"
        .CreateEvents "plungerIM"
    End With
 
 
   'Bumpers
      Sub Bumper1b_Hit
      vpmTimer.PulseSw 31
      PlaySoundAt SoundFX("fx_bumper1",DOFContactors),Bumper1b
      End Sub
   
 
      Sub Bumper2b_Hit
      vpmTimer.PulseSw 30
      PlaySoundAt SoundFX("fx_bumper1",DOFContactors),Bumper2b
      End Sub
 
 
      Sub Bumper3b_Hit
      vpmTimer.PulseSw 32
      PlaySoundAt SoundFX("fx_bumper1",DOFContactors), Bumper3b
      End Sub
     
 
 
'***********************************************
'                  Lamps
'***********************************************
 
 
 
Dim LampState(200), FadingLevel(200), FadingState(200)
Dim FlashState(200), FlashLevel(200)
Dim FlashSpeedUp, FlashSpeedDown
'Dim x
 
AllLampsOff()
LampTimer.Interval = -1 'lamp fading speed
LampTimer.Enabled = 1
'
 
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
    disc.roty = (disc.roty + ttSpinner.Speed) Mod 360
    f119.rotz = (disc.roty + 60) Mod 360
    f120.rotz = (disc.roty + 15) Mod 360
 
    UpdateLamps
    UpdateFlipperLogo  
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
 
 
Sub UpdateLampsLE()
    NFadeL 17, l71
    NFadeL 18, l70
    NFadeL 19, l69
    NFadeL 20, l57
    NFadeL 21, l50
    NFadeL 22, l51
    NFadeL 23, l52
    NFadeL 24, l53
    NFadeL 25, l54
    NFadeL 26, l55
    NFadeL 27, l56
    NFadeL 28, l65
    NFadeL 29, l66
    NFadeL 30, l67
    NFadeL 31, l68
    NFadeL 32, l3
    NFadeL 33, l4
    NFadeL 34, l5
    NFadeL 35, l9
    NFadeL 36, l10
    NFadeL 37, l31
    NFadeL 38, l30
    NFadeL 43, Bumper2l60 'top left bumper
    NFadeL 42, l61 'right bumper
    NFadeL 41, Bumper3l62 'lower left bumperNFadeL 43, l43
    NFadeL 45, l12
    NFadeL 46, l13
    NFadeL 47, l14
    NFadeL 48, l15
    NFadeL 49, l23
    NFadeL 50, l22
    NFadeL 51, l21
    NFadeL 52, l16
    NFadeL 53, l17
    NFadeL 54, l18
    NFadeL 55, l20
    NFadeL 56, l19
    NFadeL 57, l24
    NFadeL 58, l25
    NFadeL 59, f59
    NFadeL 60, l28
    NFadeL 63, l63
    NFadeL 65, l35 ' Magneto (Green)
    NFadeL 66, l34
    NFadeL 67, l33 ' Magneto (Red)
    NFadeL 68, l6
    NFadeL 69, l7
    NFadeL 70, l8
    NFadeL 71, l49
    NFadeL 72, l48
    NFadeL 73, l44
NFadeL 74, l43
    NFadeL 75, l36
    NFadeL 76, l37
    NFadeL 77, l29
    NFadeL 78, l45
    NFadeL 79, l46
    NFadeL 80, l47
End Sub
 
Sub UpdateLamps
    UpdateLampsLE
    LampMod 117, f117a
    LampMod 117, f117b
    LampMod 117, f117c
    LampMod 117, f117d
    LampMod 117, f117e
    LampMod 117, f117f
    LampMod 118, f118a
    LampMod 118, f118b
    LampMod 118, f118f
'    LampMod 119, f119
    LampMod 119, f119b
'    LampMod 120, f120  
    LampMod 120, f120b
    LampMod 122, f122a
    LampMod 122, f122b
    LampMod 121, f121
    LampMod 125, f25
    LampMod 132, f132
    LampMod 131, f131a
    LampMod 131, f131b
    LampMod 127, f127
    LampMod 128, l28a
    LampMod 128, l28b
    LampMod 128, l28c
    LampMod 129, l29a
    LampMod 129, l29b
    LampMod 129, l29c
    LampMod 141, f9
LampMod 141, f9b
    LampMod 142, f10
LampMod 142, f10b
    LampMod 143, f11
LampMod 143, f11b
    LampMod 121, f121b
    LampMod 127, f127b
 
'   FadeModPrim 121, wolverine, "wolverine2_30", "wolverine2_20", "wolverine2_16","wolverine2"
'FadeModPrim 127, Primitive5, "magnetotxt3_30", "magnetotxt3_20", "magnetotxt3_16","magnetotxt3"
End Sub
 
 
Sub FadePrim(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:FadingLevel(nr) = 0
        Case 3:pri.image = c:FadingLevel(nr) = 1
        Case 4:pri.image = b:FadingLevel(nr) = 2
        Case 5:pri.image = a:FadingLevel(nr) = 3
    End Select
End Sub
 
Sub FadeModPrim(nr, pri, a, b, c, d)
    dim lev
    lev = FadingLevel(nr) \ 32
    if lev > 3 then lev = 3
    Select Case lev
        Case 0:pri.image = d
        Case 1:pri.image = c
        Case 2:pri.image = b
        Case 3:pri.image = a
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
 
Sub SetLampMod(nr, value)
    If value > 0 Then
        LampState(nr) = 1
    Else
        LampState(nr) = 0
    End If
    FadingLevel(nr) = value
End Sub
 
Sub LampMod(nr, object)
    If TypeName(object) = "Light" Then
        Object.IntensityScale = FadingLevel(nr)/128
        Object.State = LampState(nr)
    End If
    If TypeName(object) = "Flasher" Then
        Object.IntensityScale = FadingLevel(nr)/128
        Object.visible = LampState(nr)
    End If
    If TypeName(object) = "Primitive" Then
        Object.DisableLighting = LampState(nr)
    End If
End Sub
 
Dim x
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
 
Sub swPlunger_Hit:BallinPlunger = 1:End Sub
Sub swPlunger_UnHit:BallinPlunger = 0:End Sub
 
 
' Create ball in kicker and assign a Ball ID used mostly in non-vpm tables
Sub CreateBallID(Kickername)
    For cnt = 1 to ubound(ballStatus)
        If ballStatus(cnt) = 0 Then
            Set currentball(cnt) = Kickername.createball
            currentball(cnt).uservalue = cnt
            ballStatus(cnt) = 1
            ballStatus(0) = ballStatus(0) + 1
            If coff = False Then
                If ballStatus(0)> 1 and XYdata.enabled = False Then XYdata.enabled = True
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
 
 
Sub Table_exit()
    Controller.Pause = False
    Controller.Stop
End Sub
 
 
 
'*******************************
'****Flipper Prims / Shadows****
'*******************************
 
Sub UpdateFlipperLogo
    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
    RFlogo1.RotY = RightFlipper1.CurrentAngle
    if EnableFlipperShadows then
        FlipperLSh.RotZ = LeftFlipper.currentangle
        FlipperRSh.RotZ = RightFlipper.currentangle
        FlipperRSh1.RotZ = RightFlipper1.currentangle
    end if
End Sub
 
'************************************************************************
'                               Color GI
'************************************************************************
 
Sub SolGIWhite(Enabled)
    if Enabled Then
        GI_WhiteOn = 0
    Else   
        GI_WhiteOn = 1
    end If
    UpdateGI
end Sub
 
Sub SolGIRed(Enabled)
    if Enabled Then
        GI_RedOn = 0
    Else   
        GI_RedOn = 1
    end If
    UpdateGI
end Sub
 
Sub SolGIBlue(Enabled)
    if Enabled Then
        GI_BlueOn = 0
    Else   
        GI_BlueOn = 1
    end If
    UpdateGI
end Sub
 
set GICallback = GetRef("ChangeGI")
 
Sub ChangeGI(nr,enabled)
    Select Case nr
        Case 0
        If Enabled Then
            Table.ColorGradeImage= "ColorGrade_8"
backpanel.opacity = 300
            GI_State = 1       
        Else   
            Table.ColorGradeImage= "ColorGrade_1"
backpanel.opacity = 100
            GI_State = 0
        End If 
    End Select
    UpdateGI
End Sub
 
 
Sub UpdateGI
    dim bulb, targcolor
 
    for each bulb in GI_Main
        bulb.state = GI_State
    next
 
    If Use7ColorGI Then
        TargColor = RGB(GI_RedOn * 255, GI_WhiteOn * 255, GI_BlueOn * 255)
    Else
        TargColor = RGB(GI_WhiteOn * 127 + GI_RedOn * 127, GI_WhiteOn * 127, GI_WhiteOn * 127 + GI_BlueOn * 127)
        ' Tweaks...
        if GI_RedOn = 1 AND GI_BlueOn = 0 AND GI_WhiteOn = 0 then TargColor = RGB(200,0,0):DOF 200, DOFOn:DOF 201, DOFOff:DOF 202, DOFOff
        if GI_RedOn = 0 AND GI_BlueOn = 1 AND GI_WhiteOn = 0 then TargColor = RGB(0,0,200):DOF 200, DOFOff:DOF 201, DOFOn:DOF 202, DOFOff
        if GI_RedOn = 1 AND GI_BlueOn = 1 AND GI_WhiteOn = 1 then TargColor = RGB(255,255,255):DOF 200, DOFOff:DOF 201, DOFOff:DOF 202, DOFOn
    end If
       
    for each bulb in GI_Color
        if TargColor > 0 AND GI_State then
            bulb.state = 1
        Else
            bulb.state = 0
        end if
        bulb.color = RGB(0,0,0)
        bulb.colorfull = TargColor
    next
End Sub
 
 
' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************
 
Sub Pins_Hit (idx)
    PlaySoundAtBall "pinhit_low"
End Sub
 
Sub Targets_Hit (idx)
    PlaySoundAtBallVol"target",3
End Sub
 
Sub TargetBankWalls_Hit (idx)
    PlaySoundAtBallVol"target",3
End Sub
 
Sub Metals_Thin_Hit (idx)
    PlaySoundAtBall "metalhit_thin"
End Sub
 
Sub Metals_Medium_Hit (idx)
    PlaySoundAtBall "metalhit_medium"
End Sub
 
Sub Metals2_Hit (idx)
    PlaySoundAtBall "metalhit2"
End Sub
 
Sub Gates_Hit (idx)
    PlaySoundAtBall "gate4"
End Sub
 
Sub Spinner_Spin
    PlaySoundAt "fx_spinner", Spinner
End Sub
 
 
'*******************************************************************************************************
' Random Rubber Bumps by RustyCardores - Used instead of the existing velocity based random rubber script
' x is a volume variable. Decimals decrease volume. Whole Numbers increase volume.
' More effective with PMD as bumps applied from minimum velocity & auto-governed by Vol(ActiveBall)
'*******************************************************************************************************
 
Sub Rubbers_Hit(idx)
    RandomRubberSound()
End Sub
 
Sub Posts_Hit(idx)
    RandomRubberSound()
End Sub
 
Sub RandomRubberSound()
    Select Case Int(Rnd*3)+1
        Case 1 : PlaySoundAtBallVol "rubber_hit_1",1
        Case 2 : PlaySoundAtBallVol "rubber_hit_2",1
        Case 3 : PlaySoundAtBallVol "rubber_hit_3",1
    End Select
End Sub
 
'*****************************************
' END Random Rubber Bumps by RustyCardores
 
 
Sub LeftFlipper_Collide(parm)
    RandomSoundFlipper()
End Sub
 
Sub RightFlipper_Collide(parm)
    RandomSoundFlipper()
End Sub
 
Sub RandomSoundFlipper()
    Select Case Int(Rnd*3)+1
        Case 1 : PlaySoundAtBall "flip_hit_1"
        Case 2 : PlaySoundAtBall "flip_hit_2"
        Case 3 : PlaySoundAtBall "flip_hit_3"
    End Select
End Sub
 
 
'**********************************************************************************************************
' Random Ramp Bumps by RustyCardores & DJRobX - Best used to compliment Rusty & Rob's Raised Ramp RollingBall Script
' Switches added to ramps in key bend locations and called from these collections(idx).
'**********************************************************************************************************
 
Dim NextOrbitHit:NextOrbitHit = 0
 
Sub Orbit_Wall_Hit
    if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
        RandomBump .001, -10000
        ' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much.
        ' Lowering these numbers allow more closely-spaced clunks.
        NextOrbitHit = Timer + .03 + (Rnd * .2)
    end if
End Sub
 
Sub PlasticRampbumps_Hit(idx)
    RandomBump 1, Pitch(ActiveBall)
End Sub
 
Sub MetalRampbumps_Hit(idx)
    RandomBump 1, 6000
End Sub
 
Sub RandomBump(voladj, freq)
    dim BumpSnd:BumpSnd= "fx_rampbump" & CStr(Int(Rnd*7)+1)
    If Table.VersionMinor > 3 OR Table.VersionMajor > 10 Then
        PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
    Else   
        PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1
    End If
End Sub
 
'****************************************
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

Sub PlaySoundAtVol(sound, tableobj, Vol)
    If Table.VersionMinor > 3 OR Table.VersionMajor > 10 Then
        PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
    Else
        PlaySound sound, 1, Vol, AudioPan(tableobj)
    End If
End Sub
 
Sub PlaySoundAtBallVol(sound, VolMult)
    If Table.VersionMinor > 3 OR Table.VersionMajor > 10 Then
        PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
    Else
        PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1
    End If
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
    Vol = Csng(BallVel(ball) ^2 / 4000)
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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
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
  dim mball
  for each mball in mDMag.Balls
    if mball.ID = ball1.ID OR mball.ID = ball2.ID then exit sub
  next
  If Table.VersionMinor > 3 OR Table.VersionMajor > 10 Then
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 80, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
  else
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 80, AudioPan(ball1), 0, Pitch(ball1), 0, 0
  end if
End Sub

 
''*****************************************
''          BALL SHADOW
''*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6)
 
Sub BallShadow_Timer()
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
        If BOT(b).Z > 20 and BOT(b).Z < 140 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 110
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub
 
 
' Dummys used for positionals so timers could be added to balldrops
Sub BallDropSoundLeft(dummy):PlaySound "balldrop",0,1,-.4,0,0,0,0,.8:End Sub
Sub BallDropSoundRight(dummy):PlaySound "balldrop",0,1,.4,0,0,0,0,.8:End Sub
 
Sub leftdrop_Hit()
     vpmTimer.AddTimer 150, "BallDropSoundLeft"
 End Sub
 
Sub IceRampEnd1_Hit:IceManHold = IceManHold - 1:StopSound "fx_ramp_metal":End Sub
Sub IceRampEnd2_Hit:IceManHold = IceManHold - 1:vpmTimer.AddTimer 150, "BallDropSoundRight":StopSound "fx_ramp_metal":End Sub
 
 
Sub RLS_Timer()
    RampGate3.RotZ = -(Spinner2.currentangle)
    RampGate1.RotZ = -(Spinner1.currentangle)
    RampGate4.RotZ = -(Spinner4.currentangle)
    RampGate2.RotZ = -(Spinner3.currentangle)
'    SpinnerT1.RotZ = -(sw9.currentangle)
    SpinnerT2.RotZ = -(sw47.currentangle)
'   ampprim.objrotx = ampf.currentangle
End Sub
 
'Sub PrimT_Timer
'   If Wall455.isdropped = true then ampf.rotatetoend:end if
'   if Wall455.isdropped = false then ampf.rotatetostart:end if
'   ampprim.objrotx = ampf.currentangle
'End Sub
 
Sub uppostt_Timer
    If lockpin1.isdropped = true then uppostf.rotatetoend
    if lockpin1.isdropped = false then uppostf.rotatetostart
    uppostp.transy = uppostf.currentangle
End Sub
 
dim vengeanceBall,zz
set vengeanceBall = kicker1.createball
kicker1.kick 0,0
Dim Mag1
    Set mag1= New cvpmMagnet
    With mag1
        .InitMagnet vengeanceTrigger, 25
        .GrabCenter = False
        .magnetOn = True
    End With
 
Sub VengeanceHit (Vmove)
    dim yy
    vengeanceBall.vely = Vmove / 2
    yy = (rnd(1) - 0.5) * Vmove
    vengeanceBall.velx = yy
    VengeanceTimer.Enabled = 1:zz=0
    'Playsound "solenoidon"
End Sub
 
 
Sub VengeanceTimer_Timer
    wolverine.roty = (vengeanceBall.x - vengeanceTrigger.x) * 2
    'wolverine.transY = (vengeanceBall.y - vengeanceTrigger.y) '* 2
    zz = zz + 1
    if zz = 100 then VengeanceTimer.Enabled = 0
End Sub
 
Sub PrimT_Timer
'    if f9.State = 1 then f9a.visible = 1 else f9a.visible = 0
'    if f10.State = 1 then f10a.visible = 1 else f10a.visible = 0
'    if f11.State = 1 then f11a.visible = 1 else f11a.visible = 0
    if l28a.State = 1 then f28a.visible = 1 else f28a.visible = 0
    if l28a.State = 1 then f28b.visible = 1 else f28b.visible = 0
    if l28a.State = 1 then f28c.visible = 1 else f28c.visible = 0
    if l29a.State = 1 then f29a.visible = 1 else f29a.visible = 0
    if l29a.State = 1 then f29b.visible = 1 else f29b.visible = 0
    if l29a.State = 1 then f29c.visible = 1 else f29c.visible = 0
End Sub 
 
Sub BallRelease_Hit()
   
End Sub