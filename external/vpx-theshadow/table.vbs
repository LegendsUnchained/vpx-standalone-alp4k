Option Explicit
 Randomize

'The Shadow (Bally 1994) By Alessio with further visual, sound, and physics enhancements by Skitso, Markrock76, and Bord

' Arngrim, added RGB undercab DOF command

' Thalamus 2019 February : Improved directional sounds
' !! NOTE : Table not verified yet !!

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 2000    ' Lower number, louder ballrolling/collision sound
Const VolCol = 10      ' Ball collision divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
Const VolRH     = 1.25    ' Rubber hits volume.
Const VolPi     = 1    ' Rubber pins volume.
Const VolTarg   = 1    ' Targets volume.
Const VolSpin   = 1.5  ' Spinners volume.
Const VolFlip   = 1    ' Flipper volume.

 Dim Ballsize,BallMass
 BallSize = 50
 BallMass = (Ballsize^3)/125000

if Table1.showDT = False then ramp14.visible = 0:ramp15.visible = 0

 On Error Resume Next
 ExecuteGlobal GetTextFile("controller.vbs")
 If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
 On Error Goto 0

 Dim DesktopMode:DesktopMode = Table1.ShowDT
 Dim UseVPMDMD:UseVPMDMD = DesktopMode
 Const UseVPMModSol = 1


 LoadVPM "01560000", "WPC.VBS", 3.50

 Const cGameName = "ts_lx5"
 'Const cGameName = "ts_lh6p"

 'STANDARD DEFINITIONS *************************************************************************************************************************************

 Const UseSolenoids = 2
 Const UseLamps = 0
 Const UseSync = 0
 Const HandleMech = 1
 Set MotorCallback = GetRef("HandleMiniPF")
Const cSingleLFlip = 1 '(I then commented this line out if there was a upper left flipper)
'Const cSingleRFlip = 0 '(I then commented this line out if there was a upper right flipper)


'DIMS *************************************************************************************************************************************

 Dim bsTrough, bsL, bsR, dtDrop, x, bump1, bump2, bump3, BallFrame, Mech3bank, bsLeftEject, bsRightEject, bsCenterEject, BallInMagnet, ReleaseMagnetBall,_
 KickerPos, NewKickerPos, LastKickerPos, bslock, Fade, plungerim, LeftNudgeEffect, RightNudgeEffect, NudgeEffect

'STANDARD SOUNDS ******************************************************************************************************************************************

 Const SSolenoidOn = "Solenoid"
 Const SSolenoidOff = ""
'  Const SFlipperOn = "FlipperUp"
 Const SFlipperOn = "FlipperSu"
' Const SFlipperOff = "FlipperDown"
 Const SFlipperOff = "FlipperGiu"
 Const SCoin = "Coin"

'KEY DEFINITIONS ******************************************************************************************************************************************

 Const keyBuyInButton = 3
 Const keyLeftPhurbaButton = 30
 Const keyRightPhurbaButton = 40

'Options ******************************************************************************************************************************************

Const InstrCardType               = 0    'Instruction Cards Type (0= Standard, 1= Custom)
Const ModeSceneLightColor         = 0        'Scene Lights Color (0= White, 1= Cyan, 2= Purple)
Const Flippers                    = 0        'Flipper Options (0= Original, 1= Williams Black, 2= Williams Blue 3= Williams Red) Williams Flippers borrowed from Flupper's Whitewater Table
Const MiniPlayfieldDifficulty     = 2    'Battlefield Physics Difficulty (0= Easy, 1= Mediumish, 2= Hard)
Const DiverterShake               = 1    'Ramp Diverters Shake When Hit (0= No Shake, 1= Shake) Noticed this happening in some videos and thought it would be fun to add
Const BackwallDecal               = 1    'Custom Backwall Decal (0= Not Visible, 1= Visible)
Const PlasticsReflect             = 0    'Plastics Reflect on Sidewalls (0= No, 1= Yes)

 Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "The Shadow (Bally 1994) Skitso Detail Mod"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 1
        .Hidden = DesktopMode
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
    End With

    On Error Goto 0

 'MAIN TIMER INIT ******************************************************************************************************************************************

 PinMAMETimer.Interval = PinMAMEInterval
 PinMAMETimer.Enabled = 1

 'StartShake



 'NUDGE INIT *******************************************************************************************************************************

 vpmNudge.TiltSwitch = 14
 vpmNudge.Sensitivity = 0.5
 vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot)



'DROP TARGETS INIT ****************************************************************************************************************************************


 sw55.isDropped = 0:sw85.IsDropped = 0:sw86.IsDropped = 0:_
 sw87.IsDropped = 0:sw88.IsDropped = 0:WallTarget.isDropped = 0

'BATTLEFIELD MINI KICKER INIT *****************************************************************************************************************************

 Init_MiniKicker

'PHURBA DIVERTERS INIT ************************************************************************************************************************************

 'RPLa.isdropped = 0:RPRa.isdropped = 1:_
' LPL.collidable = 1:LPLa.collidable = 1
 'LPR.collidable = 0:LPRa.collidable = 0
 'RightPhurbaRight.isdropped = 1:RightPhurbaLeft.isdropped = 0:_
 'LeftPhurbaLeft.isdropped = 0:LeftPhurbaRight.isdropped = 1

 'BALLS INIT ***********************************************************************************************************************************************


 Set bsTrough = New cvpmBallStack:With bsTrough
 .InitSw 0, 41, 42, 43, 44, 45, 0, 0
 .InitKick BallRelease, 90, 10
 .InitEntrySnd "Solenoid", "Solenoid"
 .InitExitSnd SoundFX("BallRelease",DOFContactors), SoundFX("Solenoid2",DOFContactors)
 .Balls = 5
 End With

  Controller.Switch(22) = 1

  'End With
  End Sub


 Sub table1_KeyDown(ByVal keycode)

 If keycode = PlungerKey  Then Controller.Switch(11) = True
 If keycode = keyBuyInButton Then Controller.Switch(23) = True
 If keycode = LeftMagnaSave Then Controller.Switch(34) = True
 If keycode = RightMagnaSave Then Controller.Switch(12) = True
 If keycode = LeftTiltKey Then vpmNudge.DoNudge 80, 1.6
 If keycode = RightTiltKey Then vpmNudge.DoNudge 280, 1.6
 If keycode = CenterTiltKey Then vpmNudge.DoNudge 0, 1.6
  if keydownhandler(keycode) then exit sub
 End Sub

 Sub table1_KeyUp(ByVal keycode)

 If keycode = PlungerKey  Then Controller.Switch(11) = False
 If keycode = keyBuyInButton Then Controller.Switch(23) = False
 If keycode = LeftMagnaSave Then Controller.Switch(34) = False
 If keycode = RightMagnaSave Then Controller.Switch(12) = False
 If keyuphandler(keycode) then exit sub
 End Sub

  'GI LIGHTING***********************************************************************************************************************************************
Set GICallback2 = GetRef("UpdateGI")



Dim gistep

Sub UpdateGI(no, step)
    Dim ii
    If step = 0 then exit sub 'only values from 1 to 8 are visible and reliable. 0 is not reliable and 7 & 8 are the same so...
    gistep = (step-1) / 7
    Select Case no
        Case 0
      DOF 101, gistep
            For each ii in aGiBottomLights
                ii.IntensityScale = gistep
            Next
        Case 1
            For each ii in aGiLeftLights
                ii.IntensityScale = gistep
            Next
        Case 2
            For each ii in aGiInsertsBottom
                ii.IntensityScale = gistep
            Next
        Case 3
            For each ii in aGiInsertsTop
                ii.IntensityScale = gistep
            Next
        Case 4
            For each ii in aGiRightLights
                ii.IntensityScale = gistep
            Next
    End Select
End Sub


 'FLIPPERS *************************************************************************************************************************************************

 SolCallback(sLRFlipper) = "SolRFlipper"
 SolCallback(sLLFlipper) = "SolLFlipper"

 Sub SolLFlipper(Enabled)
  vpmSolFlipper LeftFlipper, Nothing, Enabled
  If Enabled then
  PlaysoundAtVol SoundFX("FlipperUpLeft",DOFFlippers), LeftFlipper, VolFLip
  End If
  If Not Enabled then
  PlaysoundAtVol SoundFX("FlipperDownLeft",DOFFlippers), LeftFlipper, VolFlip
  End If
 End Sub

 Sub SolRFlipper(Enabled)
 vpmSolFlipper RightFlipper, RightFlipper2, Enabled
  If Enabled then
  PlaysoundAtVol SoundFX("FlipperUpLeft",DOFFlippers), RightFlipper, VolFlip
  End If
  If Not Enabled then
  PlaysoundAtVol SoundFX("FlipperDownLeft",DOFFlippers), RightFlipper, VolFlip
  End If
 End Sub

'***********************
'     Flipper Logos
'***********************

Sub UpdateFlipperLogos_Timer
  batleft.ObjRotZ = LeftFlipper.CurrentAngle
  batright.ObjRotZ = RightFlipper.CurrentAngle
  batright1.ObjRotZ = RightFlipper2.CurrentAngle
End Sub

'BALL SOUNDS ************************************************************************************************************************************

 Sub RightRampSound_Hit
  PlaysoundAtVol "Metalrolling", ActiveBall, 1
 End Sub

 Sub BallRoll_Hit
  PlaysoundAtVol "Ballrolling", ActiveBall, 1
 End Sub

 Sub BallRoll2_Hit
  PlaysoundAtVol "Ballrolling", ActiveBall, 1
 End Sub

 Sub Skill_UnHit
 PlaysoundAtVol "OnlyMetalRolling", ActiveBall, 1
 End Sub

'AUTOMATIC PLUNGER ****************************************************************************************************************************************

 Const IMPowerSetting = 55   'ERA 40
 Const IMTime = 0.6
 Set plungerIM = New cvpmImpulseP
 With plungerIM
 .InitImpulseP swplunger, IMPowerSetting, IMTime
 .Random 0.3
 .InitExitSnd "plunger2", "plunger"
 .CreateEvents "plungerIM"
 End With

 Sub Auto_Plunger(Enabled)
 If Enabled Then
 PlungerIM.AutoFire
 PlaysoundAtVol SoundFX("plunger2",DOFContactors), Skill, 1
 End If
 End Sub

'SOLENOIDS ************************************************************************************************************************************************

 SolCallback(1)  = "Auto_Plunger"
 SolCallback(2)  = "SolLockupKickout"
 SolCallback(3)  = "SolLeftPhurbaLeft"
 SolCallback(4)  = "SolLeftPhurbaRight"
 SolCallback(5)  = "SolRightPhurbaRight"
 SolCallback(6)  = "SolRightPhurbaLeft"
 SolCallback(7)  = "SolKnocker"
 SolCallback(8)  = "SolWallTargetUp"
 SolCallback(9)  = "solLSling"
 SolCallback(10) = "solRSling"
 SolCallback(11) = "bsRightEject.SolOut"
 SolCallback(12) = "bsLeftEject.SolOut"
 SolCallback(13) = "SolBallRelease"
 Solcallback(14) = "SolBallPopper"
 SolCallback(16) = "SolWallTargetDown"
 Solcallback(24) = "SolMiniDropbank"
 SolCallback(25) = "SolSingleDropUp"
 SolCallback(35) = "SolMagnetOn"
 SolCallback(36) = "SolSingleDropDown"

'FLASHER SOLENOIDS FOR FADING LIGHT SYSTEM ****************************************************************************************************************

 SolCallback(17) = "SetLamp 117,"
 SolCallback(18) = "SetLamp 118,"
 SolCallback(21) = "SetLamp 121,"
 SolCallback(22) = "SetLamp 122,"
 SolCallback(23) = "SetLamp 123,"
 SolCallback(26) = "SetLamp 126,"
 SolCallback(27) = "SetLamp 127,"
 SolCallback(28) = "SetLamp 128,"

 Sub SolKnocker(enabled)
  If enabled then
    PlaySound SoundFX("Knocker",DOFKnocker)
    End If
 End Sub

 Sub shooterlane_Hit():Controller.Switch(48) = 1:End Sub:Sub shooterlane_Unhit():Controller.Switch(48) = 0:End Sub

 Sub SolBallRelease(enabled)
 If enabled Then
 If bsTrough.Balls Then PlaySound SoundFX("BallRelease",DOFContactors) Else PlaySound SoundFX("Solenoid",DOFContactors)
 bsTrough.ExitSol_On
 End If
 End Sub

  Sub table1_Paused:Controller.Pause = 1:End Sub
 Sub table1_unPaused:Controller.Pause = 0:End Sub


'DRAIN HOLES **********************************************************************************************************************************************

' Sub Drain_Hit:PlaysoundAtVol "drain", ActiveBall, 1:bsTrough.AddBall Me:End Sub
 Sub Drain_Hit():PlaySoundAtVol "Drain", ActiveBall, 1:bsTrough.AddBall Me:End Sub
 'Sub Drain2_Hit():PlaySound "Drain":bsTrough.AddBall Me:End Sub
 'Sub Drain3_Hit():PlaySound "Drain":bsTrough.AddBall Me:End Sub:Sub Drain4_Hit():PlaySound "Drain":bsTrough.AddBall Me:End Sub

'SLINGSHOTS ***********************************************************************************************************************************************

 Dim RStep, Lstep

 Sub LeftSlingShot_Slingshot:vpmTimer.PulseSw 61:End Sub
 Sub RightSlingShot_Slingshot:vpmTimer.PulseSw 62:End Sub


 Sub solLSling(enabled)
  If enabled then
    PlaySoundAtVol SoundFX("left_slingshot",DOFContactors), sling1, 1
    LSling.Visible = 0
    LSling1.Visible = 1
    sling1.TransZ = -27
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
  End If
 End Sub

 Sub solRSling(enabled)
  If enabled then
    PlaySoundAtVol SoundFX("right_slingshot",DOFContactors), sling2, 1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling2.TransZ = -27
    RStep = 0
    RightSlingShot.TimerEnabled = 1
  End If
End Sub

 Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling1.TransZ = -15
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling1.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
 End Sub

 Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling2.TransZ = -15
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling2.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
 End Sub

 Sub sw35_Hit:vpmTimer.PulseSwitch 35, 0, "":End Sub


'ROLLOVER SWITCHES ****************************************************************************************************************************************

 Sub sw15_Hit:Controller.Switch(15) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub:Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub

 Sub sw16_Hit:Controller.Switch(16) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub:Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub

 Sub sw17_Hit:Controller.Switch(17) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub:Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub

 Sub sw18_Hit:Controller.Switch(18) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub:Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub

 Sub sw54_Hit:Controller.Switch(54) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub:Sub sw54_UnHit:Controller.Switch(54) = 0:End Sub

 Sub sw57_Hit:Controller.Switch(57) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub:Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub

 Sub sw47_Hit:Controller.Switch(47) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub:Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub

'RAMP HELPERS *********************************************************************************************************************************************

 Sub RHelp1_Hit:ActiveBall.VelZ = -2:ActiveBall.VelY = 0:ActiveBall.VelX = 0:StopSound "metalrolling":PlaySoundAtVol "balldropa", ActiveBall, 1:End Sub

 Sub RHelp2_Hit:ActiveBall.VelZ = -2:ActiveBall.VelY = 0:ActiveBall.VelX = 0:StopSound "metalrolling":PlaySoundAtVol "balldropa", ActiveBall, 1:End Sub

 Sub RHelp3_Hit:ActiveBall.VelZ = -2:ActiveBall.VelY = 0:ActiveBall.VelX = 0:StopSound "OnlyMetalRolling":PlaySoundAtVol "CadutaFerri", ActiveBall, 1:End Sub

 Sub RHelp4_Hit: PlaySoundAtVol "balldropa", ActiveBall, 1:End Sub

 Sub Trigger1_Hit:ActiveBall.VelZ=0:If ActiveBall.VelY<-6 Then:ActiveBall.VelY=-50:End If:End Sub

 Sub Trigger2_Hit:ActiveBall.VelZ=0:If ActiveBall.VelY<-25 Then:ActiveBall.VelY=-25:End If:End Sub

 Sub Trigger5_Hit:ActiveBall.VelZ=0:If ActiveBall.VelY<-33 Then:ActiveBall.VelY=-33:End If:End Sub

 Sub Trigger6_Hit:ActiveBall.VelZ=0:If ActiveBall.VelY<-10 Then:ActiveBall.VelY=-10:End If:End Sub

 Sub RRL_Hit:ActiveBall.VelZ=0:If ActiveBall.VelY<-10 Then ActiveBall.VelY=-10:End If:End Sub  'USCITA SX RAMPA DX

 Sub RRR_Hit:ActiveBall.VelZ=0:If ActiveBall.VelY<-10 Then ActiveBall.VelY=-10:End If:End Sub  'USCITA DX RAMPA DX

 Sub RSUD_Hit:ActiveBall.VelZ=0:If ActiveBall.VelY<-10 Then ActiveBall.VelY=-10:End If:End Sub 'USCITA DX RAMPA SX

 Sub RSUS_Hit:ActiveBall.VelZ=0:If ActiveBall.VelY<-10 Then ActiveBall.VelY=-10:End If:End Sub 'USCITA SX RAMPA SX

 Sub CadutaBattle_Hit: BattleLose.Enabled= 1 : End Sub

 Sub BattleWin_Hit: PlaySoundAtVol "rrenter" , ActiveBall, 1: End Sub

 Sub BattleLose_Timer()
 Playsound "balldropa"
 Me.enabled=0
 End Sub

Sub Glass_Hit
Playsound "AXSGlassHit3"
End Sub

'RAMP SWITCHES ********************************************************************************************************************************************

 Sub sw31_Hit:Controller.Switch(31) = 1:End Sub:Sub sw31_Unhit:Controller.Switch(31) = 0:End Sub

 Sub sw32_Hit:Controller.Switch(32) = 1:End Sub:Sub sw32_Unhit:Controller.Switch(32) = 0:End Sub

 Sub sw75_Hit:Controller.Switch(75) = 1:End Sub:Sub sw75_Unhit:Controller.Switch(75) = 0: PlaysoundAtVol "MetalRolling", ActiveBall, 1:End Sub

 Sub sw76_Hit:Controller.Switch(76) = 1:End Sub:Sub sw76_Unhit:Controller.Switch(76) = 0: PlaysoundAtVol "MetalRolling", ActiveBall, 1:End Sub

 Sub sw77_Hit:Controller.Switch(77) = 1:End Sub:Sub sw77_Unhit:Controller.Switch(77) = 0: PlaysoundAtVol "MetalRolling", ActiveBall, 1:End Sub

 Sub sw78_Hit:Controller.Switch(78) = 1:End Sub:Sub sw78_Unhit:Controller.Switch(78) = 0: PlaysoundAtVol "MetalRolling", ActiveBall, 1:End Sub

 Sub sw58_Hit:Controller.Switch(58) = 1:End Sub:Sub sw58_Unhit:Controller.Switch(58) = 0:End Sub

'MONGOL TARGETS *******************************************************************************************************************************************

 Sub sw25_Hit:vpmTimer.PulseSw 25:Me.TimerEnabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw25_Timer:Me.TimerEnabled = 0:End Sub

 Sub sw26_Hit:vpmTimer.PulseSw 26:Me.TimerEnabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw26_Timer:Me.TimerEnabled = 0:End Sub

 Sub sw27_Hit:vpmTimer.PulseSw 27:Me.TimerEnabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw27_Timer:Me.TimerEnabled = 0:End Sub

 Sub sw28_Hit:vpmTimer.PulseSw 28:Me.TimerEnabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw28_Timer:Me.TimerEnabled = 0:End Sub

 Sub sw52_Hit:vpmTimer.PulseSw 52:Me.TimerEnabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw52_Timer:Me.TimerEnabled = 0:End Sub

 Sub sw53_Hit:vpmTimer.PulseSw 53:Me.TimerEnabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw53_Timer:Me.TimerEnabled = 0:End Sub

'MINI BATTLEFIELD TARGETS *********************************************************************************************************************************

 Sub sw74_Hit:vpmTimer.PulseSw 74:sw74.IsDropped = 1:Me.TimerEnabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw74_Timer:sw74.IsDropped = 0:Me.TimerEnabled = 0:End Sub

 Sub sw73_Hit:vpmTimer.PulseSw 73:sw73.IsDropped = 1:Me.TimerEnabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw73_Timer:sw73.IsDropped = 0:Me.TimerEnabled = 0:End Sub

 Sub sw72_Hit:vpmTimer.PulseSw 72:sw72.IsDropped = 1:Me.TimerEnabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw72_Timer:sw72.IsDropped = 0:Me.TimerEnabled = 0:End Sub

 Sub sw71_Hit:vpmTimer.PulseSw 71:sw71.IsDropped = 1:Me.Timerenabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw71_Timer:sw71.IsDropped = 0:Me.TimerEnabled = 0:End Sub

 Sub sw81_Hit:vpmTimer.PulseSw 81:sw81.IsDropped = 1:Me.Timerenabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw81_Timer:sw81.IsDropped = 0:Me.TimerEnabled = 0:End Sub

 Sub sw82_Hit:vpmTimer.PulseSw 82:sw82.IsDropped = 1:Me.Timerenabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw82_Timer:sw82.IsDropped = 0:Me.TimerEnabled = 0:End Sub

 Sub sw84_Hit:vpmTimer.PulseSw 84:sw84.IsDropped = 1:Me.Timerenabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw84_Timer:sw84.IsDropped = 0:Me.TimerEnabled = 0:End Sub

 Sub sw85_Hit:sw85.IsDropped = 1:p85.TransY = -40:Controller.Switch(85) = 1:PlaySoundAtVol SoundFX("DropTargetDown",DOFDropTargets), ActiveBall, 1:vpmTimer.PulseSw 24:End Sub

 Sub sw86_Hit:sw86.IsDropped = 1:p86.TransY = -40:Controller.Switch(86) = 1:PlaySoundAtVol SoundFX("DropTargetDown",DOFDropTargets), ActiveBall, 1:vpmTimer.PulseSw 24:End Sub

 Sub sw87_Hit:sw87.IsDropped = 1:p87.TransY = -40:Controller.Switch(87) = 1:PlaySoundAtVol SoundFX("DropTargetDown",DOFDropTargets), ActiveBall, 1:vpmTimer.PulseSw 24:End Sub

 Sub sw88_Hit:sw88.IsDropped = 1:p88.TransY = -40:Controller.Switch(88) = 1:PlaySoundAtVol SoundFX("DropTargetDown",DOFDropTargets), ActiveBall, 1:vpmTimer.PulseSw 24:End Sub

'MINI BATTLEFIELD DROPBANK HANDLE *************************************************************************************************************************

 Sub SolMiniDropBank(enabled):If enabled Then:PlaySoundAtVol SoundFX("DropTargetUp",DOFDropTargets),Trigger6,1:_
 sw85.IsDropped = 0:sw86.IsDropped = 0:sw87.IsDropped = 0:sw88.IsDropped = 0:Controller.Switch(85) = 0:_
 p85.TransY = 0: p86.TransY = 0: p87.TransY = 0: p88.TransY = 0:_
 Controller.Switch(86) = 0:Controller.Switch(87) = 0:Controller.Switch(88) = 0:End If:End Sub

'CENTER TARGET ********************************************************************************************************************************************

 Sub sw56_Hit:vpmTimer.PulseSw 56:Me.TimerEnabled = 1:PlaySoundAtVol SoundFX("target",DOFTargets), ActiveBall, 1:End Sub
 Sub sw56_Timer:Me.TimerEnabled = 0:End Sub

'WALL TARGET **********************************************************************************************************************************************


Sub LHP1_Hit()
If ActiveBall.velY < 0  Then    'ball is going up
PlaySoundAtVol "rrenter", ActiveBall, 1
Else
StopSound "rrenter"
End If
End Sub

Sub RHP1_Hit()
If ActiveBall.velY < 0  Then    'ball is going up
PlaySoundAtVol "rrenter", ActiveBall, 1
Else
StopSound "rrenter"
End If
End Sub

 Sub WallTarget_Hit
  PlaysoundAtVol "bersaglio", sw77, 1
 End Sub

 Sub SolWallTargetDown(enabled)
 If enabled Then
 WallTarget.IsDropped = True
 MuroLock.TransY = -70
 Controller.Switch(51) = True
 PlaySoundAtVol SoundFX("muro",DOFTargets), MuroLock, 1
 End If
 End Sub

 Sub SolWallTargetUp(enabled)
 If enabled Then
 Controller.Switch(51) = False
 WallTarget.isDropped = False
 MuroLock.TransY = 0
 PlaySoundAtVol SoundFX("muro",DOFTargets), MuroLock, 1
 End If
 End Sub

 'GESTIONE FOTOCELLULE LOCK

 Sub sw33_Hit()
 FotocellulaDx.Visible=0
 FotocellulaSx.Visible=0
 Controller.Switch(33) = 1
 End Sub

 Sub sw33_Unhit()
 Controller.Switch(33) = 0
 FotocellulaDx.Visible=1
 FotocellulaSx.Visible=1
 End Sub

 'GESTIONE FOTOCELLULE BATTLE

 Sub FotocellulaDisattivaBat_Hit()
  SpegneFotocelluleBat
 End Sub

 Sub FotocellulaDisattivaBat_Hit()
  FotocellulaDxBat.Visible=0
  FotocellulaSxBat.Visible=0
 End Sub

 Sub FotocellulaDisattivaBat_UnHit()
  TimerFotocellula2.enabled=1
 End Sub

 Sub TimerFotocellula2_Timer()
  FotocellulaDxBat.Visible=1
  FotocellulaSxBat.Visible=1
  Me.Enabled=0
 End Sub

  'BATTLEFIELD TARGET ***************************************************************************************************************************************

 Sub sw55_Hit()
 MuroMiniPlayfield.TransZ = 5
 MuroColpito.Enabled= True
 Controller.Switch(55) = True
 sw55.IsDropped = True
 MuroMiniPlayfield.TransY = -40
 PlaysoundAtVol SoundFX("DropTargetUp",DOFDropTargets), ActiveBall, 1
 End Sub

 Sub MuroColpito_Timer()
 MuroMiniPlayfield.TransZ = 0
 Me.Enabled=0
 End Sub

 Sub SolSingleDropDown(enabled)
 If enabled Then
 sw55.IsDropped = True
 MuroMiniPlayfield.TransY = -40
 Controller.Switch(55) = True
 PlaySoundAtVol SoundFX("DropTargetDown",DOFDropTargets), MuroMiniPlayfield, 1
 End If
 End Sub

 Sub SolSingleDropUp(enabled)
 If enabled Then
 sw55.IsDropped = False
 MuroMiniPlayfield.TransY = 0
 Controller.Switch(55) = False
 PlaySoundAtVol SoundFX("DropTargetUp",DOFDropTargets), MuroMiniPlayfield, 1
 End If
 End Sub

'LEFT EJECT ***********************************************************************************************************************************************

 Set bsLeftEject = New cvpmBallStack
 bsLeftEject.Ballimage = "ball1"
 bsLeftEject.InitSaucer sw66, 66, 101.5+Int(Rnd*1), 22+Int(Rnd*1)
 bsLeftEject.InitExitSnd SoundFX("Solenoid",DOFContactors), SoundFX("ExitKicker",DOFContactors)
 bsLeftEject.KickForceVar = 2

 Sub sw66_Hit()
 PlaySoundAtVol "EnterHole", Sw66, 1
 bsLeftEject.AddBall Me
 End Sub

 Sub sw66_UnHit()
 PlaySoundAtVol SoundFX("ExitHole",DOFContactors), Sw66, 1
 End Sub

'RIGHT EJECT **********************************************************************************************************************************************

 Set bsRightEject = New cvpmBallStack
 bsRightEject.Ballimage = "ball1"
 bsRightEject.InitSaucer sw67, 67, 100, 10  ' ERA 67,100,7
 bsRightEject.InitExitSnd SoundFX("Solenoid",DOFContactors), SoundFX("ExitKicker",DOFContactors)
 bsRightEject.KickForceVar = 2

 Sub sw67_Hit()
 PlaySoundAtVol "EnterHole", sw67, 1
 bsRightEject.AddBall Me
 End Sub

 Sub sw67_UnHit()
 PlaySoundAtVol SoundFX("ExitHole",DOFContactors), sw67, 1
 End Sub



'PHURBA DIVERTERS *****************************************************************************************************************************************

 Sub SolLeftPhurbaLeft(enabled) 'SCAMBIO SX DEVIA A DESTRA
 If enabled Then
 PlaySoundAtVol SoundFX("Diverter",DOFContactors), ScambioSinistro, 1
 ScambioSinistro.TransX= 395
 ScambioSinistro.TransZ= 320
 ScambioSinistro.ObjRotZ= 40
 Divertershadow_left.TransX= -42
 Divertershadow_left.TransZ= 0
 Divertershadow_left.TransY= 21
 Divertershadow_left.RotY= 9
 Divertershadow_left.ObjRotZ= 48
 PugnaleSinistro.TransX= 395
 PugnaleSinistro.TransZ= 293
 PugnaleSinistro.ObjRotZ= 40
 DiverterSxVersoDx.IsDropped= 0
 DiverterSxVersoSx.IsDropped= 1
 End If
 End Sub

 Sub SolLeftPhurbaRight(enabled) 'SCAMBIO SX DEVIA A SINISTRA
 If enabled Then
 PlaySoundAtVol SoundFX("Diverter",DOFContactors), ScambioSinistro, 1
 ScambioSinistro.TransX= 0
 ScambioSinistro.TransZ= 0
 ScambioSinistro.ObjRotZ= 0
 Divertershadow_left.TransZ= 0
 Divertershadow_left.TransX= 0
 Divertershadow_left.TransY= 0
 Divertershadow_left.ObjRotZ= 0
 PugnaleSinistro.TransX= 0
 PugnaleSinistro.TransZ= 0
 PugnaleSinistro.ObjRotZ= 0
 DiverterSxVersoDx.IsDropped= 1
 DiverterSxVersoSx.IsDropped= 0
 End If
 End Sub


 Sub SolRightPhurbaLeft(enabled)  'SCAMBIO DX DEVIA A DESTRA
 If enabled Then
 PlaySoundAtVol SoundFX("Diverter",DOFContactors), ScambioDestro, 1
 ScambioDestro.TransX= -24
 ScambioDestro.TransZ= 490
 ScambioDestro.ObjRotZ= 35
 Divertershadow_Right.TransX= 0
 Divertershadow_Right.TransZ= 0
 Divertershadow_Right.TransY= -3
 Divertershadow_Right.RotY= -5
 Divertershadow_Right.RotZ= 31
 Divertershadow_Right.ObjRotZ= 0
 PugnaleDestro.TransX= -28
 PugnaleDestro.TransZ= 450
 PugnaleDestro.ObjRotZ= 35
 DiverterDxVersoDx.IsDropped= 0
 DiverterDxVersoSx.IsDropped= 1
' RPLa.IsDropped = False
' RPRa.IsDropped = True
' RightPhurbaRight.isdropped = True
' RightPhurbaLeft.isdropped = False
 End If
 End Sub

 Sub SolRightPhurbaRight(enabled)  'SCAMBIO DX DEVIA A SINISTRA
 If enabled Then
 PlaySoundAtVol SoundFX("Diverter",DOFContactors), ScambioDestro, 1
 ScambioDestro.TransX= 0
 ScambioDestro.TransZ= 0
 ScambioDestro.ObjRotZ= 0
 Divertershadow_Right.TransX= 57
 Divertershadow_Right.TransY= 10
 Divertershadow_Right.TransZ= 0
 Divertershadow_Right.RotY= 0
 Divertershadow_Right.RotZ= -7
 Divertershadow_Right.ObjRotZ= 0
 PugnaleDestro.TransX= 0
 PugnaleDestro.TransZ= 0
 PugnaleDestro.ObjRotZ= 0
 DiverterDxVersoDx.IsDropped= 1
 DiverterDxVersoSx.IsDropped= 0
' RPLa.IsDropped = True
' RPRa.IsDropped = False
' RightPhurbaRight.isdropped = False
' RightPhurbaLeft.isdropped = True
 End If
 End Sub

 'GATE

 Sub GateMissione_Hit()
  PlaysoundAtVol "gate", ActiveBall, 1
 End Sub

 Sub GateLock_Hit()
  PlaysoundAtVol "gate", ActiveBall, 1
 End Sub

'DIVERTER SHAKE******************************************************************************************************************************************

Sub Trigger3_Hit
If ActiveBall.VelY < 0 then
ScambioDestro.transx=4
Divertershadow_Right.transx=61
PugnaleDestro.transx=4
End If
If ActiveBall.VelY > 0 then
ScambioDestro.transx=0
Divertershadow_Right.transx=57
PugnaleDestro.transx=0
End If
End Sub

Sub Trigger4_Hit
If ActiveBall.VelY < 0 then
ScambioDestro.transx=-2.5
Divertershadow_Right.transx=54.5
PugnaleDestro.transx=-2.5
End If
End Sub

Sub Trigger5_Hit
If ActiveBall.VelY < 0 then
ScambioDestro.transx=0
Divertershadow_Right.transx=57
PugnaleDestro.transx=0
End If
End Sub

Sub Trigger7_Hit
If ActiveBall.VelY < 0 then
ScambioDestro.transx=-27
Divertershadow_Right.transx=-3
PugnaleDestro.transx=-31
End If
If ActiveBall.VelY > 0 then
ScambioDestro.transx=-24
Divertershadow_Right.transx=0
PugnaleDestro.transx=-28
End If
End Sub

Sub Trigger8_Hit
If ActiveBall.VelY < 0 then
ScambioDestro.transx=-21.5
Divertershadow_Right.transx=2.5
PugnaleDestro.transx=-25.5
End If
End Sub

Sub Trigger9_Hit
If ActiveBall.VelY < 0 then
ScambioDestro.transx=-24
Divertershadow_Right.transx=0
PugnaleDestro.transx=-28
End If
End Sub

Sub Trigger10_Hit
If ActiveBall.VelY < 0 then
ScambioSinistro.transx=4
Divertershadow_left.transx=4
PugnaleSinistro.transx=4
End If
If ActiveBall.VelY > 0 then
ScambioSinistro.transx=0
Divertershadow_left.transx=0
PugnaleSinistro.transx=0
End If
End Sub

Sub Trigger11_Hit
If ActiveBall.VelY < 0 then
ScambioSinistro.transx=-2.5
Divertershadow_left.transx=-2.5
PugnaleSinistro.transx=-2.5
End If
End Sub

Sub Trigger12_Hit
If ActiveBall.VelY < 0 then
ScambioSinistro.transx=0
Divertershadow_left.transx=0
PugnaleSinistro.transx=0
End If
End Sub

Sub Trigger13_Hit
If ActiveBall.VelY < 0 then
ScambioSinistro.transx=391
Divertershadow_left.transx=-45
PugnaleSinistro.transx=391
End If
If ActiveBall.VelY > 0 then
ScambioSinistro.transx=395
Divertershadow_left.transx=-42
PugnaleSinistro.transx=395
End If
End Sub

Sub Trigger14_Hit
If ActiveBall.VelY < 0 then
ScambioSinistro.transx=396.5
Divertershadow_left.transx=-40
PugnaleSinistro.transx=396.5
End If
End Sub

Sub Trigger15_Hit
If ActiveBall.VelY < 0 then
ScambioSinistro.transx=395
Divertershadow_left.transx=-42
PugnaleSinistro.transx=395
End If
End Sub

'MAGNET HANDLER *****************************************************************************************************************************************

 Sub MagnetOff_Hit()
 BallInMagnet = False
 MagnetKicker.enabled = False
 MagnetCatch.enabled = False
 MagnetHold.enabled = False
 MagnetHelper1.enabled = False
 MagnetHelper2.enabled = False
 End Sub

 Sub MagnetKickerHelper_Hit()
 BallInMagnet = False
 MagnetKicker.enabled = False
 MagnetCatch.enabled = False
 MagnetHold.enabled = False
 MagnetHelper1.enabled = False
 MagnetHelper2.enabled = False
 End Sub

 Sub MagnetCatch_Hit()
 If BallInMagnet = False Then
 MagnetHold.enabled = True
 MagnetCatch.kick 180, 5 '1
 End If
 End Sub

 Sub MagnetKicker_Hit()
 If BallInMagnet = True Then
 MagnetKicker.enabled = False
 MagnetKicker.kick 0, 20
 PlaysoundAtVol "LockWall", ActiveBall, 1
 Else
 MagnetHold.enabled = True
 MagnetKicker.kick 0, 5 '1
 End If
 End Sub

 Sub MagnetHold_Hit()
 BallInMagnet = True
 MagnetHold.destroyball
 MagnetHoldFloat.Createball.image = "Ball1"
 End Sub

 Sub MagnetHelper1_Hit()
 BallInMagnet = True
 MagnetHelper1.destroyball
 MagnetHoldFloat.Createball.image = "Ball1"
 End Sub

 Sub MagnetHelper2_Hit()
 BallInMagnet = True
 MagnetHelper2.destroyball
 MagnetHoldFloat.Createball.image = "Ball1"
 End Sub

 Dim MagneteAttivo

 Sub SolMagnetOn(enabled)
 If enabled Then
 If BallInMagnet = False Then
 MagnetKicker.enabled = True
 MagnetCatch.enabled = True
 PlaysoundAtVol SoundFX("Magnete",DOFShaker), MagnetCatch, 1 : MagneteAttivo =1
 MagnetHold.enabled = True
 MagnetHelper1.enabled = True
 MagnetHelper2.enabled = True
 End If
 ReleaseMagnetBall = False
 MagnetReleaseTimer.enabled = False
 Else
 ReleaseMagnetBall = True
 MagnetReleaseTimer.enabled = True
 End If
 End Sub

 Sub MagnetReleaseTimer_Timer()
 MagnetReleaseTimer.enabled = False
 If ReleaseMagnetBall = True and BallInMagnet = True Then
 MagnetCatch.enabled = False
 MagnetHold.enabled = False
 MagnetHelper1.enabled = False
 MagnetHelper2.enabled = False
 MagnetHoldFloat.destroyball
 MagnetHold.Createball.image = "Ball1"
 MagnetHold.kick 180, 5
 End If
 End Sub

'BATTLEFIELD MINI KICKER HANDLER **************************************************************************************************************************

'MiniKicker Handling added to MiniPlayfieldDifficulty Options

Sub FotocellulaDisattivaBat_Hit
SlingMiniPF.transz=35
SlingMiniPF1.transz=35
kickpadshadow.transz=35
Trigger1.enabled=1
Playsound "LeftEject", 0, Vol(ActiveBall)*150, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Trigger1_Hit
SlingMiniPF.transz=0
SlingMiniPF1.transz=0
kickpadshadow.transz=0
End Sub

Sub Trigger1_UnHit
Trigger1.enabled=False
End Sub

Sub Trigger2_Hit
SlingMiniPF.transz=0
SlingMiniPF1.transz=0
kickpadshadow.transz=0
End Sub


'BATTLEFIELD VUK ******************************************************************************************************************************************

 Sub MiniPFPopperHelper_Hit
 MiniPFPopperHelper.DestroyBall
 Controller.Switch(68) = True
 PopperEject.Createball.Image = "Ball1"
 PopperEject.Kick 290, 2+rnd*3

 Controller.Switch(68) = False
 End Sub

 Sub Popper_Hit()
 Controller.Switch(68) = True
 Popper.enabled = False
 End Sub

 Sub SolBallPopper(enabled)
 If enabled Then
 If Popper.enabled = False Then
 Controller.Switch(68) = False
 Popper.DestroyBall
 'PlaySound "EnterHole"
 PopperEject.Createball.Image = "Ball1"
 PopperEject.Kick 300, 2+rnd*3
 PlaySoundAtVol SoundFX("ExitVuk",DOFContactors), Popper, 1
 End If
 Popper.enabled = True
 End If
 End Sub

 'Sub Vukup_hit:ActiveBall.Velz=45+Int(Rnd*20):End Sub  'ERA Velz=30              'Added to MiniPlayfieldDifficulty Options

 Sub HoleBattle_Hit()
  PlaySoundAtVol "EnterHole", ActiveBall, 1
 End Sub


'BATTLEFIELD SENSORS******************************************************************************************************************************************

 Sub sw37_Hit:Controller.Switch(37) = 1:End Sub:Sub sw37_Unhit: Controller.Switch(37) = 0:End Sub
 Sub sw38_Hit:Controller.Switch(38) = 1:End Sub:Sub sw38_Unhit: Controller.Switch(38) = 0:End Sub

'BALL LOCK ************************************************************************************************************************************************

 Set bsLock = New cvpmBallStack
 bsLock.Ballimage = "Ball1"
 bsLock.InitSw 0,63,64,65,0,0,0,0
 bsLock.InitKick LockupKickerEject, 120, 2  'ERA 100, 2

 Sub Lockup_Hit()
 bsLock.AddBall Me
 End Sub

 Sub SolLockupKickout(enabled)
 If enabled Then
 PlaySoundAtVol SoundFX("ExitHole",DOFContactors), Lockup, 1
 vpmTimer.AddTimer 250, "Lockup_Serve"
 End If
 End Sub

 Sub Lockup_Serve(No)
 bsLock.ExitSol_On
 End Sub

'FADING LIGHT SYSTEM (C) BY PD/JPSALAS ********************************************************************************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 10 'lamp fading speed
LampTimer.Enabled = 1

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

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0         ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4       ' used to track the fading state
        FlashSpeedUp(x) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.35 ' slower speed when turning off the flasher
        FlashMax(x) = 1          ' the maximum value when on, usually 1
        FlashMin(x) = 0          ' the minimum value when off, usually 0
        FlashLevel(x) = 0        ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub



'TABLE LIGHTS *********************************************************************************************************************************************

 Sub UpdateLamps

 NFadeL 11, Light11
 NFadeLwf 12, Light12, Light12_ref
 NFadeL 13, Light13
 NFadeL 14, Light14
 NFadeL 15, Light15
 NFadeL 16, Light16
 NFadeL 17, Light17
 NFadeLwf 18, Light18, L18_ref
 NFadeL 21, Light21
 NFadeL 22, Light22
 NFadeL 23, Light23
 NFadeL 24, Light24
 NFadeL 25, Light25
 NFadeL 26, Light26
 NFadeL 27, Light27
 NFadeL 28, Light28
 NFadeL 31, Light31
 NFadeL 32, Light32
 NFadeL 33, Light33
 NFadeL 34, Light34
 NFadeL 35, Light35
 NFadeL 36, Light36
 NFadeL 37, Light37
 NFadeL 38, Light38
 NFadeL 41, Light41
 NFadeL 42, Light42
 NFadeL 43, Light43
 NFadeL 44, Light44
 NFadeL 45, Light45
 NFadeL 46, Light46
 NFadeL 47, Light47
 NFadeL 48, Light48
 NFadeLwf 51, Light51, L51_ref
 NFadeL 52, Light52
 NFadeLwf 53, Light53, L53_ref
 NFadeL 54, Light54
 NFadeLwf 55, Light55, L55_ref
 NFadeL 56, Light56
 NFadeL 57, Light57
 NFadeL 58, Light58
 NFadeLwf 61, Light61, L61_ref
 NFadeLwf 62, Light62, L62_ref
 NFadeLwf 63, Light63, L63_ref
 Flashm 64, L64_ref2
 NFadeLwf 64, Light64, L64_ref1
 NFadeLwf 65, Light65, L65_ref
 NFadeL 66, Light66
 NFadeL 67, Light67
 NFadeL 68, Light68
 NFadeL 71, Light71
 NFadeL 72, Light72
 NFadeL 73, Light73
 NFadeLwf 74, Light74,L74_ref
 NFadeLwf 75, Light75, L75_ref
 NFadeL 76, Light76
 NFadeL 77, Light77
 NFadeL 78, Light78
 Flashm 85, L85_ref2
 NFadeLwf 85, Light85, L85_ref1
 NFadeL 86, Light86

 ' GESTIONE FLASH
NFadeLm 117, F117a
 Flashm 117, F117_ref1
 Flashm 117, F117_ref2
 Flash 117, F117
 Flashm 118, F118b
 Flashm 118, F118b_ref
 Flashm 118, F118C
 Flashm 118, F118C_ref
 Flash 118, F118
 Flashm 121, F121A
 Flashm 121, F121B
 Flashm 121, F121C
 Flashm 121, F121D
 Flashm 121, F121D_ref
 Flashm 121, F121C_ref
 Flashm 121, F121_ref
 Flash 121, F121
NFadeLm 122, F122b
NFadeLm 122, F122c
NFadeLm 122, F122d
 Flashm 122, F122A
 Flashm 122, F122
 Flashm 122, F122_ref
 Flash 122, F122A_ref
NFadeLm 123, F123a
NFadeLm 123, F123b
 Flashm 123, F123_ref
 Flashm 123, F123_ref2
 Flash 123, F123
NFadeLm 126, F126a
NFadeLm 126, F126b
 Flash 126, F126
NFadeLm 127, F127a
NFadeLm 127, F127b
 Flash 127, F127
NFadeLm 128, F128a
NFadeLm 128, F128b
 Flashm 128, F128_ref
 Flash 128, F128
NFadeObjm 81, AnelloSxRampaDx, "shadowringlit", "shadowring"
 Flashm 81, F181_ref
 Flashm 81, F181_ref2
 Flash 81, F181
NFadeObjm 82, AnelloDxRampaDx, "shadowringlit", "shadowring"
 Flashm 82, F182_ref
 Flash 82, F182
NFadeObjm 83, AnelloDxRampaDx1, "shadowringlit", "shadowring"
 Flashm 83, F183_ref
 Flash 83, F183
NFadeObjm 84, AnelloDxRampaSx, "shadowringlit", "shadowring"
 Flashm 84, F184_ref
 Flash 84, F184
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

Sub NFadeLm(nr, object) ' used for 2 lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

Sub NFadeLn(nr, object) ' used for 3 lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub


Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:object.image = d:FadingLevel(nr) = 0 'Off
        Case 3:object.image = c:FadingLevel(nr) = 2 'fading...
        Case 4:object.image = b:FadingLevel(nr) = 3 'fading...
        Case 5:object.image = a:FadingLevel(nr) = 1 'ON
 End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:object.image = d
        Case 3:object.image = c
        Case 4:object.image = b
        Case 5:object.image = d
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


Sub SetModLamp(nr, value)
    If value > 0 Then
    LampState(nr) = 1
  Else
    LampState(nr) = 0
  End If
  FadingLevel(nr) = value
End Sub

Sub FadeModLamp(nr, object, factor)
  Object.IntensityScale = FadingLevel(nr) * factor/255
  If TypeName(object) = "Light" Then
    Object.State = LampState(nr)
  End If
  If TypeName(object) = "Flasher" Then
    Object.visible = LampState(nr)
  End If
End Sub

'Light with flasher, own fading speeds and min/max (1.1c - simplifed) 'Uses CGT
Sub NFadeLwF(nr, object1, object2)
    Select Case FadingLevel(nr)
    Case 4
      object1.state = 0
      FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr))
      if FlashLevel(nr) < 0 then FlashLevel(nr) = 0 : FadingLevel(nr) = 0
      object2.IntensityScale = FlashLevel(nr)
    Case 5
      object1.state = 1
      FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedDown(nr))
      if FlashLevel(nr) > 1 then FlashLevel(nr) = 1 : FadingLevel(nr) = 1
      object2.IntensityScale = FlashLevel(nr)
  End Select
End Sub


' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX and Rothbauerw
' PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
    PlaySound sound, 0, ABS(BOT.velz)/17, Pan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function RndNum(min, max)
    RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = ball.x * 2 / table1.width-1
  If tmp > 0 Then
    Pan = Csng(tmp ^10)
  Else
    Pan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
    BallVelZ = INT((ball.VelZ) * -1 )
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
    VolZ = Csng(BallVelZ(ball) ^2 / 200)*1.2
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 or BOT(b).z > 200 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.65, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b))
    Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
        End If
		If BallVel(BOT(b) ) > .3 AND BOT(b).z > 60 And BOT(b).X < 805 AND BOT(b).Y > 610 Then
            rolling(b) = True
            PlaySound("fx_metalrolling" & b), -1, Vol(BOT(b) )*10, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b))
		End If
		If BOT(b).z < 55 Then
		Stopsound "fx_metalrolling0"
		Stopsound "fx_metalrolling1"
		Stopsound "fx_metalrolling2"
		Stopsound "fx_metalrolling3"
        End If
End If

    '***Ball Drop Sounds***
    If BOT(b).VelZ < -5 and BOT(b).z > 205 and BOT(b).x < 362 and BOT(b).y < 554 Then 'height adjust for ball drop sounds
      PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
    End If
    If BOT(b).VelZ < -5 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
      PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
    End If
'End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperRSh1.RotZ = RightFlipper2.currentangle

End Sub


'*****************************************
'           BALL SHADOW
'*****************************************
Sub BallShadowUpdate_Timer()
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
ballShadow(b).opacity = 100
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 80
End If
    Next	
End Sub


Sub Pins_Hit (idx)
  PlaySound "pinhit_low", 0, Vol(ActiveBall)*VolPi, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
  PlaySound "target", 0, Vol(ActiveBall)*VolTarg, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
  PlaySound "metalhit_thin", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
  PlaySound "metalhit_medium", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
  PlaySound "metalhit2", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
  PlaySound "gate4", 0, Vol(ActiveBall)*VolGates, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
  PlaySoundAtVol "fx_spinner", Spinner, VolSpin
End Sub

Sub Battlefield_Hit (idx)
  PlaySound "LeftEject", 0, 10*Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 20 then
    PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End if
  If finalspeed >= 1 AND finalspeed <= 19 then
    RandomSoundRubber()
  End If
End Sub

Sub Posts_Hit(idx)
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed >= 17 then
    RandomSoundRubber1()
  End if
  If finalspeed >= 1 AND finalspeed <= 16 then
    RandomSoundRubber()
  End If
End Sub

Sub RandomSoundRubber1()
  Select Case Int(Rnd*3)+1
    Case 1 : PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 2 : PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 3 : PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End Select
End Sub

Sub RandomSoundRubber()
  Select Case Int(Rnd*3)+1
    Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
    Case 1 : PlaySound "flip_hit_1", 0, 5*Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 2 : PlaySound "flip_hit_2", 0, 5*Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 3 : PlaySound "flip_hit_3", 0, 5*Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End Select
End Sub

Select Case InstrCardType
    Case 0
      Apron1.image= "ApronOK1"
    Case 1
      Apron1.image= "ApronOK2"
  End Select

Select Case ModeSceneLightColor
    Case 0
      Table1.image= "TheShadowPlayfieldNIW"
      Light27.image= "TheShadowPlayfieldNIW"
      Light28.image= "TheShadowPlayfieldNIW"
      Light35.image= "TheShadowPlayfieldNIW"
      Light36.image= "TheShadowPlayfieldNIW"
      Light38.image= "TheShadowPlayfieldNIW"
      Light37.image= "TheShadowPlayfieldNIW"
    Case 1
      Table1.image= "TheShadowPlayfield"
      Light27.image= "TheShadowPlayfield"
      Light28.image= "TheShadowPlayfield"
      Light35.image= "TheShadowPlayfield"
      Light36.image= "TheShadowPlayfield"
      Light38.image= "TheShadowPlayfield"
      Light37.image= "TheShadowPlayfield"
    Case 2
      Table1.image= "TheShadowPlayfieldNIP"
      Light27.image= "TheShadowPlayfieldNIP"
      Light28.image= "TheShadowPlayfieldNIP"
      Light35.image= "TheShadowPlayfieldNIP"
      Light36.image= "TheShadowPlayfieldNIP"
      Light38.image= "TheShadowPlayfieldNIP"
      Light37.image= "TheShadowPlayfieldNIP"
  End Select

Select Case Flippers
    Case 0
      flipperL.Visible = 1
      FlipperR.Visible = 1
      FlipperR2.Visible = 1
    Case 1
      flipperL.Visible = 0
      FlipperR.Visible = 0
      FlipperR2.Visible = 0
      batleft.visible = 1
      batleft.image = "flipperbatblack"
      batright.visible = 1
      batright.image = "flipperbatblack"
      batright1.visible = 1
      batright1.image = "flipperbatblack"
    Case 2
      flipperL.Visible = 0
      FlipperR.Visible = 0
      FlipperR2.Visible = 0
      batleft.visible = 1
      batleft.image = "flipperbatblue"
      batright.visible = 1
      batright.image = "flipperbatblue"
      batright1.visible = 1
      batright1.image = "flipperbatblue"
    Case 3
      flipperL.Visible = 0
      FlipperR.Visible = 0
      FlipperR2.Visible = 0
      batleft.visible = 1
      batleft.image = "flipperbatred"
      batright.visible = 1
      batright.image = "flipperbatred"
      batright1.visible = 1
      batright1.image = "flipperbatred"
  End Select

Select Case MiniPlayfieldDifficulty
    Case 0
Yk1.collidable=0
Yk2.collidable=0
Yk3.collidable=0
Yk4.collidable=0
Yk5.collidable=0
Yk6.collidable=0
Yk7.collidable=0
Yk8.collidable=0
Yk9.collidable=0
Yk10.collidable=0
Yk11.collidable=0
Yk12.collidable=0
Yk13.collidable=0
Yk14.collidable=0
Yk15.collidable=0
Yk16.collidable=0
Yk17.collidable=0
Yk18.collidable=0
Yk19.collidable=0
Zk1.collidable=0
Zk2.collidable=0
Zk3.collidable=0
Zk4.collidable=0
Zk5.collidable=0
Zk6.collidable=0
Zk7.collidable=0
Zk8.collidable=0
Zk9.collidable=0
Zk10.collidable=0
Zk11.collidable=0
Zk12.collidable=0
Zk13.collidable=0
Zk14.collidable=0
Zk15.collidable=0
Zk16.collidable=0
Zk17.collidable=0
Zk18.collidable=0
Zk19.collidable=0

KickerPos = Array(Mk1,Mk2,Mk3,Mk4,Mk5,Mk6,Mk7,Mk8,Mk9,Mk10,Mk11,Mk12,Mk13,Mk14,Mk15,Mk16,Mk17,Mk18,Mk19)

 Sub HandleMiniPF
 FlipperL.RotZ = LeftFlipper.CurrentAngle
 FlipperR.RotZ = RightFlipper.CurrentAngle
 FlipperR2.RotZ = RightFlipper2.CurrentAngle
 PGate1.RotX = Gate1.CurrentAngle
 PGate2.RotX = Gate2.CurrentAngle
 NewKickerPos = Controller.GetMech(0)
 If NewKickerPos <> LastKickerPos Then
 SlingMiniPF.TransX= 90 * (NewKickerPos-9)/9
 SlingMiniPF1.TransX= 90 * (NewKickerPos-9)/9
 kickpadshadow.TransX= 90 * (NewKickerPos-9)/9
 KickerPos(LastKickerPos).IsDropped = True
 KickerPos(NewKickerPos).IsDropped = False
 LastKickerPos = NewKickerPos
 End If
 End Sub

 Sub Init_MiniKicker()
 Mk1.IsDropped = True:Mk2.IsDropped = True:Mk3.IsDropped = True:Mk4.IsDropped = True:_
 Mk5.IsDropped = True:Mk6.IsDropped = True:Mk7.IsDropped = True:Mk8.IsDropped = True:_
 Mk9.IsDropped = True:Mk10.IsDropped = True:Mk11.IsDropped = True:Mk12.IsDropped = True:_
 Mk13.IsDropped = True:Mk14.IsDropped = True:Mk15.IsDropped = True:Mk16.IsDropped = True:_
 Mk17.IsDropped = True:Mk18.IsDropped = True:Mk19.IsDropped = True
 End Sub

 Sub Mk1_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk2_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk3_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk4_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk5_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk6_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk7_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk8_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk9_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk10_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk11_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk12_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk13_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk14_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk15_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk16_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk17_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk18_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Mk19_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub

 Sub Vukup_hit:ActiveBall.Velz=30+Int(Rnd*10):End Sub  'ERA Velz=30
    Case 1

Mk1.collidable=0
Mk2.collidable=0
Mk3.collidable=0
Mk4.collidable=0
Mk5.collidable=0
Mk6.collidable=0
Mk7.collidable=0
Mk8.collidable=0
Mk9.collidable=0
Mk10.collidable=0
Mk11.collidable=0
Mk12.collidable=0
Mk13.collidable=0
Mk14.collidable=0
Mk15.collidable=0
Mk16.collidable=0
Mk17.collidable=0
Mk18.collidable=0
Mk19.collidable=0
Zk1.collidable=0
Zk2.collidable=0
Zk3.collidable=0
Zk4.collidable=0
Zk5.collidable=0
Zk6.collidable=0
Zk7.collidable=0
Zk8.collidable=0
Zk9.collidable=0
Zk10.collidable=0
Zk11.collidable=0
Zk12.collidable=0
Zk13.collidable=0
Zk14.collidable=0
Zk15.collidable=0
Zk16.collidable=0
Zk17.collidable=0
Zk18.collidable=0
Zk19.collidable=0

KickerPos = Array(Yk1,Yk2,Yk3,Yk4,Yk5,Yk6,Yk7,Yk8,Yk9,Yk10,Yk11,Yk12,Yk13,Yk14,Yk15,Yk16,Yk17,Yk18,Yk19)

 Sub HandleMiniPF
 FlipperL.RotZ = LeftFlipper.CurrentAngle
 FlipperR.RotZ = RightFlipper.CurrentAngle
 FlipperR2.RotZ = RightFlipper2.CurrentAngle
 PGate1.RotX = Gate1.CurrentAngle
 PGate2.RotX = Gate2.CurrentAngle
 NewKickerPos = Controller.GetMech(0)
 If NewKickerPos <> LastKickerPos Then
 SlingMiniPF.TransX= 90 * (NewKickerPos-9)/9
 SlingMiniPF1.TransX= 90 * (NewKickerPos-9)/9
 kickpadshadow.TransX= 90 * (NewKickerPos-9)/9
 KickerPos(LastKickerPos).IsDropped = True
 KickerPos(NewKickerPos).IsDropped = False
 LastKickerPos = NewKickerPos
 End If
 End Sub

 Sub Init_MiniKicker()
 Yk1.IsDropped = True:Yk2.IsDropped = True:Yk3.IsDropped = True:Yk4.IsDropped = True:_
 Yk5.IsDropped = True:Yk6.IsDropped = True:Yk7.IsDropped = True:Yk8.IsDropped = True:_
 Yk9.IsDropped = True:Yk10.IsDropped = True:Yk11.IsDropped = True:Yk12.IsDropped = True:_
 Yk13.IsDropped = True:Yk14.IsDropped = True:Yk15.IsDropped = True:Yk16.IsDropped = True:_
 Yk17.IsDropped = True:Yk18.IsDropped = True:Yk19.IsDropped = True
 End Sub

 Sub Yk1_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk2_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk3_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk4_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk5_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk6_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk7_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk8_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk9_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk10_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk11_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk12_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk13_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk14_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk15_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk16_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk17_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk18_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Yk19_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub

 Sub Vukup_hit:ActiveBall.Velz=45+Int(Rnd*15):End Sub  'ERA Velz=30
    Case 2

Mk1.collidable=0
Mk2.collidable=0
Mk3.collidable=0
Mk4.collidable=0
Mk5.collidable=0
Mk6.collidable=0
Mk7.collidable=0
Mk8.collidable=0
Mk9.collidable=0
Mk10.collidable=0
Mk11.collidable=0
Mk12.collidable=0
Mk13.collidable=0
Mk14.collidable=0
Mk15.collidable=0
Mk16.collidable=0
Mk17.collidable=0
Mk18.collidable=0
Mk19.collidable=0
Yk1.collidable=0
Yk2.collidable=0
Yk3.collidable=0
Yk4.collidable=0
Yk5.collidable=0
Yk6.collidable=0
Yk7.collidable=0
Yk8.collidable=0
Yk9.collidable=0
Yk10.collidable=0
Yk11.collidable=0
Yk12.collidable=0
Yk13.collidable=0
Yk14.collidable=0
Yk15.collidable=0
Yk16.collidable=0
Yk17.collidable=0
Yk18.collidable=0
Yk19.collidable=0

KickerPos = Array(Zk1,Zk2,Zk3,Zk4,Zk5,Zk6,Zk7,Zk8,Zk9,Zk10,Zk11,Zk12,Zk13,Zk14,Zk15,Zk16,Zk17,Zk18,Zk19)

 Sub HandleMiniPF
 FlipperL.RotZ = LeftFlipper.CurrentAngle
 FlipperR.RotZ = RightFlipper.CurrentAngle
 FlipperR2.RotZ = RightFlipper2.CurrentAngle
 PGate1.RotX = Gate1.CurrentAngle
 PGate2.RotX = Gate2.CurrentAngle
 NewKickerPos = Controller.GetMech(0)
 If NewKickerPos <> LastKickerPos Then
 SlingMiniPF.TransX= 90 * (NewKickerPos-9)/9
 SlingMiniPF1.TransX= 90 * (NewKickerPos-9)/9
 kickpadshadow.TransX= 90 * (NewKickerPos-9)/9
 KickerPos(LastKickerPos).IsDropped = True
 KickerPos(NewKickerPos).IsDropped = False
 LastKickerPos = NewKickerPos
 End If
 End Sub

 Sub Init_MiniKicker()
 Zk1.IsDropped = True:Zk2.IsDropped = True:Zk3.IsDropped = True:Zk4.IsDropped = True:_
 Zk5.IsDropped = True:Zk6.IsDropped = True:Zk7.IsDropped = True:Zk8.IsDropped = True:_
 Zk9.IsDropped = True:Zk10.IsDropped = True:Zk11.IsDropped = True:Zk12.IsDropped = True:_
 Zk13.IsDropped = True:Zk14.IsDropped = True:Zk15.IsDropped = True:Zk16.IsDropped = True:_
 Zk17.IsDropped = True:Zk18.IsDropped = True:Zk19.IsDropped = True
 End Sub

 Sub Zk1_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk2_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk3_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk4_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk5_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk6_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk7_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk8_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk9_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk10_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk11_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk12_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk13_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk14_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk15_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk16_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk17_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk18_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub
 Sub Zk19_Slingshot():vpmTimer.PulseSwitch 36, 0, "":End Sub

 Sub Vukup_hit:ActiveBall.Velz=50+Int(Rnd*20):End Sub  'ERA Velz=30
End Select

Select Case DiverterShake
    Case 0
      Trigger3.enabled=0
      Trigger4.enabled=0
      Trigger5.enabled=0
      Trigger7.enabled=0
      Trigger8.enabled=0
      Trigger9.enabled=0
      Trigger10.enabled=0
      Trigger11.enabled=0
      Trigger12.enabled=0
      Trigger13.enabled=0
      Trigger14.enabled=0
      Trigger15.enabled=0
    Case 1
      Trigger3.enabled=1
      Trigger4.enabled=1
      Trigger5.enabled=1
      Trigger7.enabled=1
      Trigger8.enabled=1
      Trigger9.enabled=1
      Trigger10.enabled=1
      Trigger11.enabled=1
      Trigger12.enabled=1
      Trigger13.enabled=1
      Trigger14.enabled=1
      Trigger15.enabled=1
    End Select

Select Case BackwallDecal
    Case 0
      Ramp8.visible= 0
    Case 1
      Ramp8.visible= 1
  End Select

Select Case PlasticsReflect
    Case 0
      F3.visible= 0
      F2.visible= 0
    Case 1
      F3.visible= 1
      F2.visible= 1
      Primitive142.visible=1
      Primitive52.visible=0
  End Select

' Thalamus : Exit in a clean and proper way
Sub Table1_exit()
  Controller.Pause = False
  Controller.Stop
End Sub
