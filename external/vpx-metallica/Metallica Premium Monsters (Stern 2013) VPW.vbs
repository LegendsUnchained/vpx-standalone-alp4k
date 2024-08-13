'            /\                                                /\
'           / /\                                              /| \
'          / /  \                                            / |  \
'         / /  ,.----..-------.  ,-..--.   .--.   .--.,-------.| \`\
'        / /  /|  ___||__   __/\/ ,||  |  /|  |  /|  ||   ____||  \
'       /  \ /|| |___ \\ | |  \/ /|||  | / |  | / |  ||  |   / |   \
'      /    ` ||  ___|\\ | |  /  - ||  |/  |  |/  |  ||  |  /  |    \ 
'      /    ` ||  ___|\\ | |  /  - ||  |/  |  |/  |  ||  |  /  |    \ 
'     /,~/    || |___ \\\| | /  /| ||  '--.|  '--.|  ||  '----.|  |\ \
'     ` /     ||_____|\\\|_|/__/ |_||_____||_____||__||._____.||  '-' \ 
'      /     /\\     \ \\\  \  \ | ||     ||     ||  ||       //\  ___ \ 
'     / /\/|/  \\_____\/  \__\__\\_||_____||_____||__|.______//  \|   \ \ 
'    / /    \  /                                              \  /\    \ \
'   / / ,/\/ \/                                                \/  \/\. \ \ 
'  / /./   \                                                       /   \.\ \
' / //   ./                                                         \.   \\ \
' \    ./         Metallica Premium Monsters (Stern 2013)             \.    /
'  \ ./                                                                 \. /
'   `           originally by Fren and many many mods since               '
'*****************************************************************************

'       VPIN WORKSHOP mod
'
'       VPW Table Revisions (full history at the end)
'
' 108 - apophis - added slingshot corrections
' 109 - Aubrel - snake and skull coffin mods added. CoffinBottom's "disable lighting" values fixed. Playfield insert CN19 fixed
' 110 - iaakki - all physics code updated, blocker walls added to plastics, gravemarker walls adjusted to match play videos, Target physics fixed, targetbouncer updated, left orb exit fixed 
' 111 - iaakki - nudge reduced, sling corner bouncer removed, left spinner lubed
' 112 - iaakki - LMag reworked
' 1.1 Release - apophis - Cleaned up some script. Automated VRRoom and CabinetMode selections. Default ROM set to mtl_180h

'*****************************************************************************

Option Explicit
    Randomize


'-------------  Volume Option  -------------
Const VolumeDial = 0.8       ' Global volume multiplier for the mechanical sounds. Recommended values should be no greater than 1.


'-------------  VR Room Options  -------------
Const VRRoomChoice = 1       ' This option only applies in VR. 1 - 360 Room, 2 - Minimal Room, 3 - Ultra Minimal

 
'-------------  Other Options  -------------
Const SnakeMod = 1 			 ' 0 = standard table, 1 = custom snake ramp mod (right ramps and plastic)
Const SkullCoffinMod = 1 	 ' 0 = standard table, 1 = custom skull coffin mod



  'Const cGameName = "mtl_170hc"
  Const cGameName = "mtl_170h"
  'Const cGameName = "mtl_180hc"
  'Const cGameName = "mtl_180h"

  Const UseSolenoids = 1
  Const UseLamps = 0
  Const UseSync = 1
  Const HandleMech = 0
  Const SSolenoidOn = "Solenoid"
  Const SSolenoidOff = ""
  Const SCoin = "CoinIn"
  Const UseVPMModSol = true

  Dim xx
  Dim Bump1, Bump2, Bump3, Mech3bank,bsTrough,bsLHole, bsRHole,dtl,cbRight,turntable,cbCaptive,cbCaptive2
  Dim PlungerIM,LMag,RMag, HMag, HMag2
  Dim cBall
  Dim mCaptive
  Dim VarHidden
  Dim CrossMech
  Dim DayNight
  Dim SparkyMod
  Dim LeftCard, CenterCard, RightCard
  Dim LeftImage, CenterImage, RightImage
  LeftImage=array("apron_left_1", "apron_left_2", "apron_left_3")
  CenterImage=array("apron_center_1", "apron_center_2", "apron_center_3")
  RightImage=array("apron_right_1", "apron_right_2", "apron_right_3", "apron_right_4")

  Dim luts, lutpos
  luts = array("ColorGradeLUT256x16_tmt1", "ColorGradeLUT256x16_tmt", "ColorGradeLUT256x16_new", "ColorGradeLUT256x16_ModDeSat" )
  LoadLUT
  Table.ColorGradeImage = luts(lutpos)
  ' lutpos = 0              '  set the nr of the LUT you want to use (0 = first in the list above, 1 = second, etc); 2 is the default
  LeftCard = 0 ' 0 = standard yellow, 1 = standard white, 2 = custom black
  CenterCard = 0 ' 0 = premium, 1 = custom, 2 = pro
  RightCard = 0 ' 0 = standard yellow, 1 = standard white, 2 = custom black, 3 alt custom black

  Dim DesktopMode:DesktopMode = Table.ShowDT
  Dim UseVPMDMD
  Dim VRRoom, CabinetMode

  If RenderingMode=2 Then VRRoom = VRRoomChoice Else VRRoom = 0
  If VRRoom<>0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode
  If VRRoom=0 and Not DesktopMode Then CabinetMode = 1 Else CabinetMode = 0


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

  LoadVPM "01560000", "sam.VBS", 3.10


  Set cBall = ckicker.createball
  ckicker.Kick 0, 0

Dim CapBall1, CapBall2

Sub table_Init
'    VPMInit Me
  InitVpmFFlipsSAM
  UpPost.Isdropped=true
  With Controller
    .GameName = cGameName
    If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
    .SplashInfoLine = "Metallica (Stern 2013)"
    .HandleKeyboard = 0
    .ShowTitle = 0
    .ShowDMDOnly = 1
    .ShowFrame = 0
    .HandleMechanics = 1
    .Hidden = VarHidden
    On Error Resume Next
    .Run GetPlayerHWnd
    If Err Then MsgBox Err.Description
  End With

    On Error Goto 0

'Trough
    Set bsTrough = New cvpmBallStack
    bsTrough.InitSw 0, 21, 20, 19, 18, 0, 0, 0
    bsTrough.InitKick BallRelease, 90, 14
    bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), "Solenoid"
    bsTrough.Balls = 4

'Hole bsRHole - right eject
    Set bsRHole = New cvpmBallStack
      With bsRHole
          .InitSw 0, 51, 0, 0, 0, 0, 0, 0
          .InitKick kicker3, 40, 56
          .InitExitSnd SoundFX("popper_ball",DOFContactors), "Solenoid"
          '.KickForceVar = 2
      End With


  Set CapBall1 = Fkicker.createball:fkicker.kick 0,0,0
  Set CapBall2 = ekicker.createball:ekicker.kick 0,0,0

  CapBall1.FrontDecal = "NoScratches"
  CapBall2.FrontDecal = "NoScratches"

  HammerP.RotX = 35


  sparkyhead.blenddisablelighting = 0.1


  DayNight = Table.NightDay 'read day/night slider value
  Intensity ' Sets GI brightness depending on day/night slider settings

  Set LMag=New cvpmMagnet
  LMag.InitMagnet LMagnet,15
  LMag.GrabCenter=false

  Set RMag=New cvpmMagnet
  RMag.InitMagnet RMagnet,40
  RMag.GrabCenter=True

  Set HMag=New cvpmMagnet
  HMag.InitMagnet HMagnet,20
  HMag.GrabCenter=True

  If SkullCoffinMod=1 Then CoffinBottom.Image="coffinbottom_skullmod"
  If SnakeMod = 1 Then
    Primitive55.visible = 0
    Primitive118.visible = 1
    Primitive28.image = "right_ramp_map_snakemod"
    RWire.Material= "GreenWire"
  Else
    Primitive55.visible = 1
    Primitive55.image = "psnakeuppermodif"
    Primitive118.visible = 0
    Primitive28.image = "right_ramp_map"
  End If
  Primitive30.image = "left_ramp_map"
  Primitive113.image = "plunger_ramp_map"

  vpmNudge.TiltSwitch=-7
  vpmNudge.Sensitivity= 1
  vpmNudge.TiltObj=Array(Bumper1b,Bumper2b,Bumper3b,LeftSlingshot,RightSlingshot)

'Apron Cards
  ApronCardCenter.image = CenterImage(CenterCard)
  ApronCardLeft.image = LeftImage(LeftCard)
  ApronCardRight.image = RightImage(RightCard)

End Sub

 Sub table_Paused:Controller.Pause = 1:End Sub
 Sub table_unPaused:Controller.Pause = 0:End Sub

'********************
'   Keys
'**********************

Sub table_KeyDown(ByVal keycode)
  If keycode = LeftMagnaSave then
    lutpos = lutpos - 1 : If lutpos < 0 Then lutpos = 0 : end if
    Table.ColorGradeImage = luts(lutpos)
  End if

  If keycode = RightMagnaSave then
    lutpos = lutpos + 1 : If lutpos > 3 Then lutpos = 3: end if
    Table.ColorGradeImage = luts(lutpos)
  End if


    If keycode = PlungerKey Then Plunger.Pullback

  If keycode = LeftTiltKey Then Nudge 90, 2:SoundNudgeLeft()
  If keycode = RightTiltKey Then Nudge 270, 2:SoundNudgeRight()
  If keycode = CenterTiltKey Then Nudge 0, 1:SoundNudgeCenter()

  'nFozzy Begin'
  If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
  If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress
  'nFozzy End'


    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub table_KeyUp(ByVal keycode)

  'nFozzy Begin'
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
  'nFozzy End'

  If vpmKeyUp(keycode) Then Exit Sub
  If Keycode = StartGameKey Then Controller.Switch(16) = 0
    If keycode = PlungerKey Then
    Plunger.Fire
        PlaySound "Plunger"
  End If
End Sub

SolCallback(1) = "solTrough"
SolCallback(2) = "solAutofire"
'SolCallback(3) = "LMag.MagnetOn="
SolCallback(3) = "SolLMag"
SolCallback(4) = "RMag.MagnetOn="
SolCallback(5) = "SnakeKick"
SolCallback(6) = "ScoopKick"
'SolCallback(7) = "orbitpost"  'premium not used
'SolCallback(8) = shaker motor ' optional
SolCallback(9)  = "vpmSolSound SoundFX(""jet3"",DOFContactors),"
SolCallback(10) = "vpmSolSound SoundFX(""jet3"",DOFContactors),"
SolCallback(11) = "vpmSolSound SoundFX(""jet3"",DOFContactors),"
SolCallback(12) = "SnakeJawLatch"  ' premium snake jaw latch
'SolCallback(13) = Left Slingshot
'SolCallback(14) = Right Slingshot
SolCallback(15) = "SolLFlipper"
SolCallback(16) = "SolRFlipper"
'SolCallback(17) = not used
SolCallback(18) = "SolSparkyHead"
SolModCallback(19) = "SolCrossPrim"   'Grave Marker
SolCallback(20) = "CrossMotor"      'premium grave marker motor
SolCallback(21) = "SolBackPanelLeft"  'Back Panel Left
SolCallback(22) = "SolBackPanelRight"   'Back Panel Right
SolModCallback(23) = "SolLeftRamp"    '"Flash23" Left Ramp
SolCallback(25)   = "SetLamp 125,"  'Pop Bumpers
SolModCallback(26) = "SolGraveMarker" 'Grave Marker x2
SolModCallback(27) = "SetLamp 227,"   'Electric Chair x2
SolModCallback(28) = "SolSparkyFlash"   'Electric Chair Spot
SolModCallback(29) = "SolRightRamp"   'Right Ramp
SolModCallback(30) = "SolSnakeFlasher"  'premium snake flasher 'captive ball flasher
SolModCallback(31) = "SolCoffinFlasher" 'coffin insert flasher x2
SolmodCallback(32) = "SetLamp 132,"   'electric chair insert flasher
SolCallback(51) = "CLockRelease"
SolCallback(52) = "CoffinMagDown" 'Premium coffinMagnet Down
SolCallback(53) = "Hammer" 'premium hammer
SolCallback(54) = "resetinline" ' premium DTresets
Solcallback(55) = "OrbitPost" ' premium loop post
SolCallback(56) = "SolSnakeJaw" '  premium Snake Jaw
SolCallback(57) = "HMag.MagnetOn=" ' premium CoffinMag On

controller.switch(60) = 1
controller.switch(61) = 1
controller.switch(62) = 1


sub SolLMag(Enabled)
	if (enabled) then
'		debug.print "magnet on"
		LMag.MagnetOn=True
		GraveRelease.enabled = false
		cLMagnetRelease = 0
	Else
'		debug.print "magnet off"
		GraveRelease.interval = 15 'RndInt(14,15)
		GraveRelease.enabled = true
	end if
End sub

dim cLMagnetRelease : cLMagnetRelease = 0

sub GraveRelease_timer
	select Case cLMagnetRelease
		case 12: LMag.MagnetOn=False
		Case 21: LMag.MagnetOn=True
		case 33: LMag.MagnetOn=False
		case 35: LMag.MagnetOn=False : GraveRelease.enabled = false : cLMagnetRelease = 0
	end select
	cLMagnetRelease = cLMagnetRelease + 1
end sub

Sub sw60_hit
	sw60.Isdropped=True
	Controller.switch (60)= 0
	PlaySound SoundFX("DTC",DOFDropTargets)
	TargetBouncer activeball, 1.1
End sub

Sub sw61_hit
	sw61.Isdropped=True
	Controller.switch (61)= 0
	PlaySound SoundFX("DTC",DOFDropTargets)
	TargetBouncer activeball, 1.1
End sub

Sub sw62_hit
	sw62.Isdropped=True
	Controller.switch (62)= 0
	PlaySound SoundFX("DTC",DOFDropTargets)
	TargetBouncer activeball, 1.1
End sub

Sub resetinline(Enabled)
  If Enabled Then
    sw60.Isdropped=false
    Controller.switch (60)=1
    sw61.Isdropped=False
    Controller.switch (61)=1
    sw62.Isdropped=False
    Controller.switch (62)=1
    PlaySound SoundFX("DTReset",DOFContactors)
  End If
 End Sub

Sub SnakeKick(enabled)
    If enabled Then
  sw54.timerenabled = true
    End If
End Sub

Sub SnakeJawLatch(enabled)
  If Enabled Then
    snakejawf.rotatetoend
    JawLatch.isDropped = 0
    Controller.Switch(56) = 0
  End If
End Sub

Sub Solsnakejaw(enabled)
  If Enabled Then
  snakejawf.rotatetoStart
  JawLatch.isDropped = 1
  Controller.Switch(56) = 1
  End If
End Sub

Sub sw54_Timer()
  PlaySound SoundFX("popper_ball", DOFContactors)
    sw54.Kick 195, 32
    controller.switch (54) = 0
  sw54.timerenabled = 0
End Sub

dim hammerTrans, hammerSize, hammerRot
Sub Hammer(enabled)
  If Enabled Then
    hammerRot = 35
    hammerTrans = 0
    hammerSize = 12
    HammerTD.Enabled = 1
    HammerTU.Enabled = 0
    MagnetHole.Enabled = 1
    HMag.MagnetOn = 0
  Else
    hammerRot = 0
    hammerTrans = 15
    hammerSize = 17
    HammerTD.Enabled = 0
    HammerTU.Enabled = 1
    MagnetHole.Enabled = 0
  End If
End Sub

Sub HammerTD_Timer
  If HammerTU.Enabled = 0 Then
    If hammerRot > 0 Then
      hammerRot = hammerRot - 1
      hammerTrans = hammerTrans + 0.429
      hammerSize = hammerSize + 0.143
      HammerP.RotX = hammerRot
      hammershadow.transy = hammerTrans
      hammershadow.size_x = hammerSize
      hammershadow.size_y = hammerSize
    ElseIf hammerRot = 0 Then
      HammerTD.enabled = 0
      hammershadow.transy = 15
      hammershadow.size_x = 17
      hammershadow.size_y = 17
    End If
  End If
End Sub

Sub HammerTU_Timer
  If HammerTD.Enabled = 0 Then
    If hammerRot < 35 Then
      hammerRot = hammerRot + 1
      hammerTrans = hammerTrans - 0.429
      hammerSize = hammerSize - 0.143
      HammerP.RotX = hammerRot
      hammershadow.transy = hammerTrans
      hammershadow.size_x = hammerSize
      hammershadow.size_y = hammerSize
    ElseIf hammerRot = 35 Then
      HammerTU.Enabled = 0
      hammershadow.transy = 0
      hammershadow.size_x = 12
      hammershadow.size_y = 12
    End If
  End If
End Sub

Sub CoffinMagDown(enabled)
  if Enabled Then
    HMagnetP.Z = -100 '-75
    MagnetHole.Enabled = 1
  Else
    HMagnetP.Z = 0
    MagnetHole.Enabled = 0
  End If
End Sub

Sub CrossMotor(enabled)
  If Enabled Then
    If CrossPrim.Z = 60 Then CrossTimerDown.Enabled = 1
    If CrossPrim.Z = 0 Then CrossTimerUp.Enabled = 1
  Else
  End If
End Sub


Sub CrossTimerUp_Timer
    If CrossPrim.Z < 60 Then
      CrossPrim.Z = CrossPrim.Z+1
      F119.bulbhaloheight = f119.bulbhaloheight+1
      F119b.bulbhaloheight = f119b.bulbhaloheight+1
      F119c.bulbhaloheight = f119c.bulbhaloheight+1
      F119d.bulbhaloheight = f119d.bulbhaloheight+1
      Controller.Switch(33)=0
      Controller.Switch(34)=0
     End If
    If CrossPrim.Z = 60 Then Controller.Switch(34) = 1:CrossTimerUp.Enabled = 0
End Sub

Sub CrossTimerDown_Timer
    If CrossPrim.Z > 0 Then
      CrossPrim.Z = CrossPrim.Z-1
      F119.bulbhaloheight = f119.bulbhaloheight-1
      F119b.bulbhaloheight = f119b.bulbhaloheight-1
      F119c.bulbhaloheight = f119c.bulbhaloheight-1
      F119d.bulbhaloheight = f119d.bulbhaloheight-1
      Controller.Switch(33)=0
      Controller.Switch(34)=0
    End If
    If CrossPrim.Z = 0 Then Controller.Switch(33) = 1: CrossTimerDown.Enabled = 0
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

Sub orbitpost(Enabled)
  If Enabled Then
    UpPost.Isdropped=0
  Else
    UpPost.Isdropped=1
  End If
 End Sub


Sub SolSparkyHead(Enabled)
    If Enabled Then
        SparkyShake
    Playsound "SmallSol"
    End If
End Sub

Sub SparkyShake
    cball.vely = 10 + 2 * (RND(1) - RND(1) )
End Sub

Sub Sparky
    Sparkyhead.rotx = (ckicker.y - cball.y)
    Sparkyhead.roty = (cball.x - ckicker.x)
End Sub



 Dim dBall, dZpos

Sub ScoopKick(Enabled)
  If Enabled Then
  Kicker1.Enabled = 0
  vpmtimer.addtimer 600, "Kicker1.enabled= 1'"
  bsRHole.ExitSol_On
  End If
End Sub

 Sub kicker3_Hit
      bsRHole.AddBall Me
 End Sub

Sub Kicker1_Hit
  Playsound "ScoopLeft"
End Sub


'**********
' Flupper flashers

Dim FlashLevel28, FlashLevel19, FlashLevel21, FlashLevel22, FlashLevel26, FlashLevel30, FlashLevel23, FlashLevel29
Dim FlashLevel31
Const FlasherIntensityGIOn = .65
Const FlasherIntensityGIOff = .85

F126a.IntensityScale = 0
F126b.IntensityScale = 0
f130.IntensityScale = 0
f130b.IntensityScale = 0
F123b.IntensityScale = 0
F129b.IntensityScale = 0
L31C.IntensityScale = 0
L31C2.IntensityScale = 0
L31C3.IntensityScale = 0
L31C4.IntensityScale = 0
L31C5.IntensityScale = 0
L31C6.IntensityScale = 0

Sub SolSparkyFlash(enabled)
  if Enabled then
    Dim Intensity : If lampstate(136) = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
    FlashLevel28 = Intensity * 2
    sparkyPFflasher_Timer
  end if
End Sub

Sub SolCrossPrim(enabled)
  if Enabled then
    Dim Intensity : If lampstate(136) = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
    FlashLevel19 = Intensity
    Wall32.timerinterval=30 'stealing wall32 as timer
    Wall32_Timer
  end if
End Sub

Sub SolBackPanelLeft(enabled)
  if Enabled then
    Dim Intensity : If lampstate(136) = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
    FlashLevel21 = Intensity * 2
    f21f_Timer
  end if
End Sub

Sub SolBackPanelRight(enabled)
  if Enabled then
    Dim Intensity : If lampstate(136) = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
    FlashLevel22 = Intensity * 2
    f22f_Timer
  end if
End Sub


sub SolGraveMarker(enabled)
  if Enabled then
    Dim Intensity : If lampstate(136) = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
    FlashLevel26 = Intensity
    f126c_Timer
  end if
End Sub

sub SolSnakeFlasher(enabled)
  if Enabled then
    Dim Intensity : If lampstate(136) = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
    FlashLevel30 = Intensity
    F130c_Timer
  end if
End Sub

sub SolLeftRamp(enabled)
  if Enabled then
    Dim Intensity : If lampstate(136) = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
    FlashLevel23 = Intensity
    Flasher1_Timer
  end if
End Sub

sub SolRightRamp(enabled)
  if Enabled then
    Dim Intensity : If lampstate(136) = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
    FlashLevel29 = Intensity
    Flasher2_Timer
  end if
End Sub

sub SolCoffinFlasher(enabled)
  if Enabled then
    Dim Intensity : If lampstate(136) = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
    FlashLevel31 = Intensity
    fcoffin_Timer
  end if
End Sub


sub fcoffin_Timer()
  dim flashx3
  If not fcoffin.TimerEnabled Then
    fcoffin.TimerEnabled = True
  End If
  flashx3 = FlashLevel31*FlashLevel31*FlashLevel31
  L31C.IntensityScale = flashx3
  L31C2.IntensityScale = flashx3
  L31C3.IntensityScale = flashx3
  L31C4.IntensityScale = flashx3
  L31C5.IntensityScale = flashx3
  L31C6.IntensityScale = flashx3
  fcoffin.opacity = 300 * flashx3
  Wall61.blenddisablelighting = 2 * flashx3
  coffinbottom.blenddisablelighting = 1 + 10 * flashx3
  FlashLevel31 = FlashLevel31 * 0.85 - 0.01
  If FlashLevel31 < 0 Then
    fcoffin.TimerEnabled = False
  End If
end sub

sub Flasher1_Timer()
  dim flashx3
  If not Flasher1.TimerEnabled Then
    Flasher1.TimerEnabled = True
    Flasher1.visible = 1
    F123c.visible = 1
  End If
  flashx3 = FlashLevel23*FlashLevel23*FlashLevel23
  F123b.IntensityScale = flashx3
  Flasher1.opacity = 300 * flashx3
  F123c.opacity = 250 * flashx3
  FlashLevel23 = FlashLevel23 * 0.85 - 0.01
  If FlashLevel23 < 0 Then
    Flasher1.TimerEnabled = False
    Flasher1.visible = 0
    F123c.visible = 0
  End If
end sub


sub Flasher2_Timer()
  dim flashx3
  If not Flasher2.TimerEnabled Then
    Flasher2.TimerEnabled = True
    Flasher2.visible = 1
    F129c.visible = 1
  End If
  flashx3 = FlashLevel29*FlashLevel29*FlashLevel29
  F129b.IntensityScale = flashx3
  Flasher2.opacity = 300 * flashx3
  F129c.opacity = 300 * flashx3
  FlashLevel29 = FlashLevel29 * 0.85 - 0.01
  If FlashLevel29 < 0 Then
    Flasher2.TimerEnabled = False
    Flasher2.visible = 0
    F129c.visible = 0
  End If
end sub

sub F130c_Timer()
  dim flashx3
  If not F130c.TimerEnabled Then
    F130c.TimerEnabled = True
    F130c.visible = 1
    F130d.visible = 1
  End If
  flashx3 = FlashLevel30*FlashLevel30*FlashLevel30
  F130.IntensityScale = flashx3
  F130b.IntensityScale = flashx3
  F130c.opacity = 300 * flashx3
  F130d.opacity = 300 * flashx3
  Primitive19.blenddisablelighting = 0.15 * flashx3
  FlashLevel30 = FlashLevel30 * 0.85 - 0.01
  If FlashLevel30 < 0 Then
    F130c.TimerEnabled = False
    F130c.visible = 0
    F130d.visible = 0
  End If
end sub

sub f126c_Timer()
  dim flashx3
  If not f126c.TimerEnabled Then
    f126c.TimerEnabled = True
    f126c.visible = 1
  End If
  flashx3 = FlashLevel26*FlashLevel26*FlashLevel26
  f126c.opacity = 400 * flashx3
  F126a.IntensityScale = flashx3
  f126b.IntensityScale = flashx3
  FlashLevel26 = FlashLevel26 * 0.85 - 0.01
  If FlashLevel26 < 0 Then
    f126c.TimerEnabled = False
    f126c.visible = 0
  End If
end sub

sub f22f_Timer()
  dim flashx3
  If not f22f.TimerEnabled Then
    f22f.TimerEnabled = True
    f22f.visible = 1
  End If
  flashx3 = FlashLevel22*FlashLevel22*FlashLevel22
  f22f.opacity = 300 * flashx3
  FlashLevel22 = FlashLevel22 * 0.85 - 0.01
  If FlashLevel22 < 0 Then
    f22f.TimerEnabled = False
    f22f.visible = 0
  End If
end sub

sub f21f_Timer()
  dim flashx3
  If not f21f.TimerEnabled Then
    f21f.TimerEnabled = True
    f21f.visible = 1
  End If
  flashx3 = FlashLevel21*FlashLevel21*FlashLevel21
  f21f.opacity = 300 * flashx3
  FlashLevel21 = FlashLevel21 * 0.85 - 0.01
  If FlashLevel21 < 0 Then
    f21f.TimerEnabled = False
    f21f.visible = 0
  End If
end sub

sub Wall32_Timer()
  dim flashx3
  If not Wall32.TimerEnabled Then
    Wall32.TimerEnabled = True
  End If
  flashx3 = FlashLevel19*FlashLevel19*FlashLevel19
  CrossPrim.blenddisablelighting = 10 * flashx3

  FlashLevel19 = FlashLevel19 * 0.65 - 0.01

  If FlashLevel19 < 0 Then
    Wall32.TimerEnabled = False
    CrossPrim.blenddisablelighting = 0
  End If

end sub

sub sparkyPFflasher_Timer()
  dim flashx3
  If not sparkyPFflasher.TimerEnabled Then
    sparkyPFflasher.TimerEnabled = True
    sparkyPFflasher.visible = 1
    sparkybloom.visible=1
    hammershadow.visible=0
  End If
  flashx3 = FlashLevel28*FlashLevel28*FlashLevel28
  sparkybloom.opacity = 250 * flashx3
  sparkyPFflasher.opacity = 20 * flashx3
  sparkyhead.blenddisablelighting = 0.2 * flashx3 + 0.1
  sparkybody.blenddisablelighting = 0.2 * flashx3
  SparkyChair.blenddisablelighting = 0.2 * flashx3
  Primitive28.blenddisablelighting = 0.01 * flashx3 'right ramp
  Primitive30.blenddisablelighting = 0.01 * flashx3 'left ramp
  Primitive45.blenddisablelighting = 0.2 * flashx3
  Primitive103.blenddisablelighting = 0.2 * flashx3
  Primitive102.blenddisablelighting = 0.1 * flashx3

  FlashLevel28 = FlashLevel28 * 0.85 - 0.01
  If FlashLevel28 < 0.4 Then
    hammershadow.visible=1
  Else
    hammershadow.visible=0
  end if
  If FlashLevel28 < 0 Then
    sparkyPFflasher.TimerEnabled = False
    sparkyPFflasher.visible = 0
    sparkybloom.visible=0
    sparkyhead.blenddisablelighting = 0.1
    hammershadow.visible=1
  End If

end sub

Sub CLockRelease(Enabled)
  If Enabled Then
  CoffinStop.isDropped = 1
  Else
  CoffinStop.isDropped = 0
  End If
End Sub

'***********************************************
'***********************************************
          'Switches
'***********************************************
'***********************************************

Sub CapWall_Hit:PlaySound "fx_collide":End Sub

Sub sw1_Hit:Controller.Switch(1) = 1:End Sub
Sub sw1_UnHit:Controller.Switch(1) = 0:End Sub
Sub sw3_Hit:Controller.Switch(3) = 1:End Sub
Sub sw3_UnHit:Controller.Switch(3) = 0:End Sub
Sub sw4_Hit:vpmTimer.PulseSw 4:End Sub
Sub sw7_Hit:TargetBouncer activeball, 1.1:vpmTimer.PulseSw 7:End Sub
Sub sw9_Hit:TargetBouncer activeball, 1.1:vpmTimer.PulseSw 9:End Sub
Sub sw23_Hit:Controller.Switch(23) = 1:End Sub
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
Sub sw24_Hit:Controller.Switch(24) = 1:End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
Sub sw25_Hit:Controller.Switch(25) = 1:End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
Sub sw28_Hit:Controller.Switch(28) = 1:End Sub
Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub
Sub sw29_Hit:Controller.Switch(29) = 1:End Sub
Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub
Sub sw35_Hit:TargetBouncer activeball, 1.1:vpmTimer.PulseSw 35:End Sub
Sub sw36_Hit():TargetBouncer activeball, 1.1:vpmTimer.PulseSw 36:End Sub
Sub sw37_Hit():TargetBouncer activeball, 1.1:vpmTimer.PulseSw 37:End Sub
Sub Lspinner_Spin():vpmTimer.PulseSW 38:End Sub
Sub Rspinner_Spin():vpmTimer.PulseSW 39:End Sub
Sub sw40_Hit():TargetBouncer activeball, 1.1:vpmTimer.PulseSw 40:End Sub
Sub sw41_Hit():TargetBouncer activeball, 1.1:vpmTimer.PulseSw 41:End Sub

Sub sw42_Hit()
  vpmTimer.PulseSw 42
  HammerP.RotX = 34
  sw42.HasHitEvent = 0
  sw42.timerenabled = 1
End Sub

sub sw42_timer
  HammerP.RotX = 35
  sw42.HasHitEvent = 1
  sw42.timerenabled = 0
end Sub


sub Trigger001_hit
  'reducing speed variation from plunger lane
  if activeball.velx > -18 and activeball.velx < -16 then
    activeball.velx = -17.5
  end if
end sub

Sub sw43_Hit
  Controller.Switch(43) = 1
End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub
Sub sw44_Hit:Controller.Switch(44) = 1:End Sub
Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub
Sub sw45_Hit:Controller.Switch(45) = 1:End Sub
Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub
Sub sw46_Hit:Controller.Switch(46) = 1:End Sub
Sub sw46_UnHit:Controller.Switch(46) = 0:End Sub
Sub sw47g_hit:PlaySoundAtLevelActiveBall "Gate", Vol(ActiveBall) * MetalImpactSoundFactor:vpmTimer.PulseSw 47:Activeball.UserValue=1:PlaySoundAtLevelActiveBall "RightRampWire", Vol(ActiveBall) * MetalImpactSoundFactor:End Sub
Sub sw50g_hit:PlaySoundAtLevelActiveBall "Gate", Vol(ActiveBall) * MetalImpactSoundFactor:vpmTimer.PulseSw 50:Activeball.UserValue=1:PlaySoundAtLevelActiveBall "LeftRampWire", Vol(ActiveBall) * MetalImpactSoundFactor:End Sub
Sub sw52_Hit:Controller.Switch(52)=1:End Sub
Sub sw52_unHit:Controller.Switch(52)=0:End Sub
Sub sw53_Hit:vpmTimer.PulseSw 53:End Sub
Sub sw54_Hit:Controller.Switch(54) = 1:Playsound "kicker_enter_center": End Sub
Sub JawLatch_Hit:Controller.Switch(55) = 1:snakejaw.transz=10:me.TimerEnabled = 1:End Sub
Sub JawLatch_Timer:Controller.Switch(55) = 0:snakejaw.transz=0: me.timerenabled = 0:End Sub
Sub sw57_Hit:Controller.Switch(57)=1:End Sub
Sub sw57_unHit:Controller.Switch(57)=0:End Sub
Sub sw58_Hit:Controller.Switch(58)=1:End Sub
Sub sw58_unHit:Controller.Switch(58)=0:End Sub
Sub sw59_Hit:Controller.Switch(59)=1:End Sub
Sub sw59_unHit:Controller.Switch(59)=0:End Sub
Sub SW63_Hit:Controller.Switch(63) = 1: End Sub
Sub sw63_unHit:Controller.Switch(63) = 0: End Sub
Sub LMagnet_Hit:LMag.AddBall ActiveBall:End Sub
Sub LMagnet_UnHit:LMag.RemoveBall ActiveBall:End Sub
Sub HMagnet_Hit:HMag.AddBall ActiveBall:End Sub
Sub HMagnet_UnHit:HMag.RemoveBall ActiveBall:End Sub
Sub SW64_Hit:Controller.Switch(64) = 1: sw64.timerenabled = 1:End Sub


Sub sw64_Timer
  If HMag.MagnetOn = 0 Then
    sw64.kick 100, 2
    Controller.Switch(64) = 0
  End If
  sw64.TimerEnabled = 0
End Sub


Sub RMagnet_Hit
  RMag.AddBall ActiveBall
  If RMag.MagnetOn=true then
  End If
End Sub

Sub RMagnet_UnHit:RMag.RemoveBall ActiveBall:End Sub

Dim BallCount:BallCount = 0
Sub Drain_Hit()
  RandomSoundDrain(Drain)
  BallCount = BallCount - 1
  bsTrough.AddBall Me
End Sub

Sub BallRelease_UnHit()
    BallCount = BallCount + 1
End Sub

Sub RLS()
  sw47p.RotX = -(sw47g.currentangle)
  sw50p.RotX = -(sw50g.currentangle)
  If Leftsling = True and Left1.ObjRotZ < -7 then Left1.ObjRotZ = Left1.ObjRotZ + 2
  If Leftsling = False and Left1.ObjRotZ > -20 then Left1.ObjRotZ = Left1.ObjRotZ - 2
  If Left1.ObjRotZ >= -7 then Leftsling = False
  If Leftsling = True and Left2.ObjRotZ > -212.5 then Left2.ObjRotZ = Left2.ObjRotZ - 2
  If Leftsling = False and Left2.ObjRotZ < -199 then Left2.ObjRotZ = Left2.ObjRotZ + 2
  If Left2.ObjRotZ <= -212.5 then Leftsling = False
  If Leftsling = True and Left3.TransZ > -23 then Left3.TransZ = Left3.TransZ - 4
  If Leftsling = False and Left3.TransZ < -0 then Left3.TransZ = Left3.TransZ + 4
  If Left3.TransZ <= -23 then Leftsling = False

  If Rightsling = True and Right1.ObjRotZ > 7 then Right1.ObjRotZ = Right1.ObjRotZ - 2
  If Rightsling = False and Right1.ObjRotZ < 20 then Right1.ObjRotZ = Right1.ObjRotZ + 2
  If Right1.ObjRotZ <= 7 then Rightsling = False
  If Rightsling = True and Right2.ObjRotZ < 212.5 then Right2.ObjRotZ = Right2.ObjRotZ + 2
  If Rightsling = False and Right2.ObjRotZ > 199 then Right2.ObjRotZ = Right2.ObjRotZ - 2
  If Right2.ObjRotZ >= 212.5 then Rightsling = False
  If Rightsling = True and Right3.TransZ > -23 then Right3.TransZ = Right3.TransZ - 4
  If Rightsling = False and Right3.TransZ < -0 then Right3.TransZ = Right3.TransZ + 4
  If Right3.TransZ <= -23 then Rightsling = False
End Sub

Sub LeftSlingShot_Slingshot
  LS.VelocityCorrect(activeball)
  Leftsling = True
  Controller.Switch(26) = 1
  RandomSoundSlingshotLeft(Left1)
  LeftSlingshot.TimerEnabled = 1
  End Sub

Dim Leftsling:Leftsling = False


 Sub LeftSlingShot_Timer:Me.TimerEnabled = 0:Controller.Switch(26) = 0:End Sub

 Sub RightSlingShot_Slingshot
  RS.VelocityCorrect(activeball)
  Rightsling = True
  Controller.Switch(27) = 1
  RandomSoundSlingshotRight(Right1)
  RightSlingshot.TimerEnabled = 1
  End Sub

 Dim Rightsling:Rightsling = False

Sub RightSlingShot_Timer:Me.TimerEnabled = 0:Controller.Switch(27) = 0:End Sub

    Const IMPowerSetting = 55
    Const IMTime = 0.6
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .InitExitSnd SoundFX("Plunger_Release_Ball" ,DOFContactors), SoundFX("Plunger_Release_No_Ball" ,DOFContactors)
        .CreateEvents "plungerIM"
    End With

      Sub Bumper1b_Hit
      vpmTimer.PulseSw 31
      RandomSoundBumperTop(Bumper1b)
      End Sub


      Sub Bumper2b_Hit
      vpmTimer.PulseSw 30
      RandomSoundBumperMiddle(Bumper2b)
       End Sub

      Sub Bumper3b_Hit
      vpmTimer.PulseSw 32
      RandomSoundBumperBottom(Bumper3b)
       End Sub

Dim LampState(300), FadingLevel(300), FadingState(300)
Dim FlashState(200), FlashLevel(200), FlashMin(200), FlashMax(200)
Dim FlashSpeedUp, FlashSpeedDown
Dim x

AllLampsOff()
LampTimer.Interval = 40 'lamp fading speed
LampTimer.Enabled = 1

For each xx in SparkySpots:xx.intensityscale = 0.2:Next


FlashInit()
'FlasherTimer.Interval = 10 'flash fading speed
'FlasherTimer.Enabled = 1

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

    FlashSpeedUp = 100  ' fast speed when turning on the flasher
    FlashSpeedDown = 70 ' slow speed when turning off the flasher, gives a smooth fading
    AllFlashOff()
End Sub

Sub AllFlashOff
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
    Next
End Sub

Sub SetRGBLamp(Lamp, R, G, B)
   Lamp.Color = RGB(R, G, B)
   Lamp.ColorFull = RGB(R, G, B)
   Lamp.State = 1
End Sub

dim bulb, bulb2, bulb3, bulb4
Sub UpdateLamps
    NFadeLm 17, l17
  FadeDisableLighting 17, p17, 88
    NFadeLm 18, l18
  FadeDisableLighting 18, p18, 20
    NFadeL 19, l19
    NFadeLm 21, l21
  FadeDisableLighting 21, p21, 88
    NFadeLm 22, l22
  FadeDisableLighting 22, Primitive73, 5
    NFadeL 23, l23
    NFadeL 24, l24
    NFadeLm 25, l25
  FadeDisableLighting 25, p25, 15
    NFadeLm 26, l26
  FadeDisableLighting 26, p26, 60
    NFadeLm 27, l27
  FadeDisableLighting 27, p27, 88
    NFadeLm 28, l28
  FadeDisableLighting 28, p28, 88
    NFadeLm 29, l29
  FadeDisableLighting 29, p29, 20
    NFadeLm 30, l30
    NFadeLm 31, l31
  FadeDisableLighting 31, p31, 3
    NFadeLm 32, l32
  FadeDisableLighting 32, p32, 3
    NFadeLm 33, l33
  FadeDisableLighting 33, p33, 88
    NFadeLm 34, l34
  FadeDisableLighting 34, p34, 20
    NFadeL 35, l35
    NFadeL 37, l37
    NFadeL 38, l38
    NFadeL 39, l39
    NFadeL 40, l40
    NFadeL 41, l41
    NFadeLm 42, l42
  FadeDisableLighting 42, p42, 60
    NFadeLm 43, l43
  FadeDisableLighting 43, p43, 40
    NFadeLm 44, l44
  FadeDisableLighting 44, p44, 40
    NFadeLm 45, l45
  FadeDisableLighting 45, p45, 40
    NFadeLm 46, l46
    NFadeLm 47, l47
  FadeDisableLighting 47, p47, 20
    NFadeLm 48, l48
  FadeDisableLighting 48, p48, 88
    NFadeLm 49, l49
  FadeDisableLighting 49, p49, 88
    NFadeLm 50, l50
  FadeDisableLighting 50, p50, 20
    NFadeL 51, l51
    NFadeLm 53, l53
  FadeDisableLighting 53, p53A, 40
  FadeDisableLighting 53, p53B, 40
    NFadeLm 55, l55
  FadeDisableLighting 55, p55, 3
    NFadeLm 56, l56
  FadeDisableLighting 56, p56, 3
    NFadeLm 57, l57
  FadeDisableLighting 57, p57, 3
    NFadeLm 58, l58
  FadeDisableLighting 58, p58, 3
    NFadeLm 59, l59
  FadeDisableLighting 59, p59, 5
    NFadeLm 60, l60
  FadeDisableLighting 60, p60, 5
    NFadeLm 61, l61
  FadeDisableLighting 61, p61, 5
    NFadeLm 62, l62
  FadeDisableLighting 62, p62, 5
    NFadeLm 63, l63
  FadeDisableLighting 63, p63, 88
    NFadeLm 64, l64
  FadeDisableLighting 64, p64, 60
    NFadeLm 65, l65
  FadeDisableLighting 65, p65, 3
    NFadeLm 66, l66
  FadeDisableLighting 66, p66, 60
    NFadeLm 67, l67
  FadeDisableLighting 67, p67, 60
    NFadeLm 68, l68
  FadeDisableLighting 68, p68, 100
    NFadeLm 69, l69
  FadeDisableLighting 69, p69, 100
    NFadeLm 70, l70
  FadeDisableLighting 70, p70, 100
    NFadeLm 71, l71
  FadeDisableLighting 71, p71, 3

  NFadeLm 73, l73f
  FadeDisableLighting 73, l73p, 7

  NFadeLm 74, l74f
  FadeDisableLighting 74, l74p, 5

  NFadeLm 75, l75f
  FadeDisableLighting 75, l75p, 5

    NFadeLm 77, l77
  FadeDisableLighting 77, p77, 88
    NFadeLm 78, l78
  FadeDisableLighting 78, p78, 88
    NFadeLm 79, l79
  FadeDisableLighting 79, p79, 88
  NFadeLm 80, l80
  FadeDisableLighting 80, p80, 88

  NFadeL 125, f125

  'Lights at PF around sparky, reduced transmit
  NFadeLm 227, F127a
  NFadeL 227, f127b


  'Sparky insert flasher
  NFadeL 132, F132

  SetRGBLamp CN4, LampState(87),LampState(88),LampState(89)
  SetRGBLamp CN9, Lampstate(99),Lampstate(100),Lampstate(101)
  SetRGBLamp CN5, Lampstate(90), Lampstate(91),Lampstate(92)
  SetRGBLamp CN13, Lampstate(108),Lampstate(109),Lampstate(110)
  SetRGBLamp CN11, Lampstate(102),Lampstate(103),Lampstate(104)
  SetRGBLamp CN19, Lampstate(126),Lampstate(127),Lampstate(128)


  for each bulb in GIW
    SetRGBLamp bulb, LampState(136),LampState(136),LampState(136)
  next
  for each bulb2 in GIR
    SetRGBLamp bulb2, LampState(130),0,0
  next
  for each bulb3 in GIB
    SetRGBLamp bulb3, 0,0,LampState(132)
  next
  for each bulb4 in GIWUP
    SetRGBLamp bulb4, LampState(134),LampState(134),LampState(134)
  Next
  For each xx in GIR_P: xx.blenddisablelighting = LampState (130)/40:Next
  For each xx in GIB_P: xx.blenddisablelighting = LampState (132)/40:Next
  For each xx in GIR_F: xx.intensityScale = LampState (130)*0.01:Next
  For each xx in GIB_F: xx.intensityScale = LampState (132)*0.01:Next

  'GI control
  For each xx in GIWF: xx.intensityScale = lampstate(136)*0.9+5: Next
  PlayfieldShadow.opacity=150 - LampState(136) '100 when GI on, 150 when GI off
  For each xx in Metals_Thin: xx.blenddisablelighting = LampState(136)/50 + 0.3: Next
  Primitive78.blenddisablelighting = LampState(136)/7000
  Wall365.blenddisablelighting = LampState(136)/6+0.2
  Wall161.blenddisablelighting = LampState(136)/6+0.2

End Sub

Dim GILevel
HammerP.blenddisablelighting = 0.1

Sub Intensity
  If DayNight <= 20 Then
      GILevel = .5
  ElseIf DayNight <= 40 Then
      GILevel = .4125
  ElseIf DayNight <= 60 Then
      GILevel = .325
  ElseIf DayNight <= 80 Then
      GILevel = .2375
  Elseif DayNight <= 100  Then
      GILevel = .15
  End If

  For each xx in GIW: xx.Intensity = xx.Intensity * GILevel: xx.FalloffPower = 2: Next
  For each xx in GIR: xx.Intensity = xx.Intensity * GILevel: xx.FalloffPower = 2: Next
  For each xx in GIB: xx.Intensity = xx.Intensity * GILevel: xx.FalloffPower = 2: Next
  For each xx in GIWUP: xx.Intensity = xx.Intensity * GILevel: xx.FalloffPower = 2: Next

End Sub

Sub FadePrim(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:FadingLevel(nr) = 0
        Case 3:pri.image = c:FadingLevel(nr) = 1
        Case 4:pri.image = b:FadingLevel(nr) = 2
        Case 5:pri.image = a:FadingLevel(nr) = 3
    End Select
End Sub

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

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FlashState(nr) = 0 'completely off
            End if
            Object.opacity = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
            If FlashLevel(nr) > 1000 Then
                FlashLevel(nr) = 1000
                FlashState(nr) = 1 'completely on
            End if
            Object.opacity = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object)
  Object.opacity = FlashLevel(nr)
End Sub



Sub AllLampsOff():For x = 1 to 200:LampState(x) = 4:FadingLevel(x) = 4:Next:UpdateLamps:UpdateLamps:Updatelamps:End Sub


Sub SetLamp(nr, value)
  If value = 0 AND LampState(nr) = 0 Then Exit Sub
  If value = 1 AND LampState(nr) = 1 Then Exit Sub
  LampState(nr) = abs(value) + 4
  FadingLevel(nr ) = abs(value) + 4: FadingState(nr ) = abs(value) + 4
End Sub


Sub FadeDisableLighting(nr, a, alvl)
  Select Case FadingLevel(nr)
    Case 4
      a.UserValue = a.UserValue - 0.335
      If a.UserValue < 0 Then
        a.UserValue = 0
        FadingLevel(nr) = -1
      end If
      a.BlendDisableLighting = alvl * a.UserValue 'brightness
    Case 5
      a.UserValue = a.UserValue + 0.335
      If a.UserValue > 1 Then
        a.UserValue = 1
        FadingLevel(nr) = -1
      end If
      a.BlendDisableLighting = alvl * a.UserValue 'brightness
  End Select
End Sub

sub sparkyDL(nr, a, alvl, offDL)
  Select Case FadingLevel(nr)
    Case 4
      a.UserValue = a.UserValue - 0.5
      If a.UserValue < 0 Then
        a.UserValue = 0
      end If
      a.BlendDisableLighting = alvl * a.UserValue + offDL 'brightness
    Case 5
      a.UserValue = a.UserValue + 0.5
      If a.UserValue > 1 Then
        a.UserValue = 1
      end If
      a.BlendDisableLighting = alvl * a.UserValue + offDL'brightness
  End Select
end sub

Sub FlashDisableLighting(nr, a, alvl)
  Select Case FadingLevel(nr)
    Case 4
      a.UserValue = a.UserValue - 0.5
      If a.UserValue < 0 Then
        a.UserValue = 0
        FadingLevel(nr) = -1
      end If
      a.BlendDisableLighting = alvl * a.UserValue 'brightness
    Case 5
      a.UserValue = a.UserValue + 0.5
      If a.UserValue > 1 Then
        a.UserValue = 1
        FadingLevel(nr) = -1
      end If
      a.BlendDisableLighting = alvl * a.UserValue 'brightness
  End Select
End Sub

Sub FlashDisableLightingM(nr, a, alvl)
  Select Case FadingLevel(nr)
    Case 4
      a.UserValue = a.UserValue - 0.5
      If a.UserValue < 0 Then
        a.UserValue = 0
      end If
      a.BlendDisableLighting = alvl * a.UserValue 'brightness
    Case 5
      a.UserValue = a.UserValue + 0.5
      If a.UserValue > 1 Then
        a.UserValue = 1
      end If
      a.BlendDisableLighting = alvl * a.UserValue 'brightness
  End Select
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

'Sub NFadeL(nr, object)
'    Select Case FadingLevel(nr)
'        Case 4:object.state = 0:FadingLevel(nr) = 0
'        Case 5:object.state = 1:FadingLevel(nr) = 1
'    End Select
'End Sub
'
'Sub NFadeLm(nr, object) ' used for multiple lights
'    Select Case FadingLevel(nr)
'        Case 4:object.state = 0
'        Case 5:object.state = 1
'    End Select
'End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1            'wait
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
        Case 4:object.image = b:FadingLevel(nr) = 0
        Case 5:object.image = a:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

Sub SetFlash(nr, stat)
    FlashState(nr) = ABS(stat)
End Sub
'
'Sub FlasherTimer_Timer()
'
'End Sub


'******************************************************
'         RealTime Updates
'******************************************************
dim CurFlipAngleL, CurFlipAngleR, track
Sub FrameTimer_Timer
  snakejaw.objrotx = snakejawf.currentangle

  if l66.State = 1 then L66f.visible = 1 else l66f.visible = 0
  if l67.State = 1 then l67f.visible = 1 else l67f.visible = 0
  if l23.State = 1 then l23f.visible = 1 else l23f.visible = 0
  if l24.State = 1 then l24f.visible = 1 else l24f.visible = 0

  RollingSound
  Sparky
'  Cor.Update
  RLS
  CurFlipAngleL = LeftFlipper.currentangle
  CurFlipAngleR = RightFlipper.currentangle
  FlipperLSh.RotZ = CurFlipAngleL
  FlipperRSh.RotZ = CurFlipAngleR
  LeftFlipperP.ObjRotZ = CurFlipAngleL
  RightFlipperP.ObjRotZ = CurFlipAngleR
  BallShadowUpdate
  VR_Primary_plunger.Y = 10.881 + (5* Plunger.Position) -20
End Sub

'******************************************************
'         FLIPPERS
'******************************************************

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
  If Enabled Then
    LF.Fire  'leftflipper.rotatetoend

    If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
      RandomSoundReflipUpLeft LeftFlipper
    Else
      SoundFlipperUpAttackLeft LeftFlipper
      RandomSoundFlipperUpLeft LeftFlipper
    End If
  Else
    LeftFlipper.RotateToStart
    If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
      RandomSoundFlipperDownLeft LeftFlipper
    End If
    FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper(Enabled)
  If Enabled Then
    RF.Fire 'rightflipper.rotatetoend

    If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
      RandomSoundReflipUpRight RightFlipper
    Else
      SoundFlipperUpAttackRight RightFlipper
      RandomSoundFlipperUpRight RightFlipper
    End If
  Else
    RightFlipper.RotateToStart
    If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
      RandomSoundFlipperDownRight RightFlipper
    End If
    FlipperRightHitParm = FlipperUpSoundLevel
  End If
End Sub

Sub LeftFlipper_Collide(parm)
  LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
  RightFlipperCollide parm
End Sub

'******************************************************
'   FLIPPER CORRECTION INITIALIZATION
'******************************************************
'Flipper Correction Initialization early 90’s and after

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5.5
        AddPt "Polarity", 2, 0.4, -5.5
        AddPt "Polarity", 3, 0.6, -5.0
        AddPt "Polarity", 4, 0.65, -4.5
        AddPt "Polarity", 5, 0.7, -4.0
        AddPt "Polarity", 6, 0.75, -3.5
        AddPt "Polarity", 7, 0.8, -3.0
        AddPt "Polarity", 8, 0.85, -2.5
        AddPt "Polarity", 9, 0.9,-2.0
        AddPt "Polarity", 10, 0.95, -1.5
        AddPt "Polarity", 11, 1, -1.0
        AddPt "Polarity", 12, 1.05, -0.5
        AddPt "Polarity", 13, 1.1, 0
        AddPt "Polarity", 14, 1.3, 0

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub


'******************************************************
'                        FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
        dim a : a = Array(LF, RF)
        dim x : for each x in a
                x.addpoint aStr, idx, aX, aY
        Next
End Sub

Class FlipperPolarity
        Public DebugOn, Enabled
        Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
        Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
        private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
        Private Balls(20), balldata(20)
        
        dim PolarityIn, PolarityOut
        dim VelocityIn, VelocityOut
        dim YcoefIn, YcoefOut
        Public Sub Class_Initialize 
                redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
                Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
        End Sub
        
        Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
        Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
        Public Property Get StartPoint : StartPoint = FlipperStart : End Property
        Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
        Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
        Public Property Get EndPointY: EndPointY = FlipperEndY : End Property
        
        Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
                Select Case aChooseArray
                        case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
                        Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
                        Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
                End Select
                if gametime > 100 then Report aChooseArray
        End Sub 

        Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
                if not DebugOn then exit sub
                dim a1, a2 : Select Case aChooseArray
                        case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
                        Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
                        Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
                        case else :tbpl.text = "wrong string" : exit sub
                End Select
                dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
                tbpl.text = str
        End Sub
        
        Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

        Private Sub RemoveBall(aBall)
                dim x : for x = 0 to uBound(balls)
                        if TypeName(balls(x) ) = "IBall" then 
                                if aBall.ID = Balls(x).ID Then
                                        balls(x) = Empty
                                        Balldata(x).Reset
                                End If
                        End If
                Next
        End Sub
        
        Public Sub Fire() 
                Flipper.RotateToEnd
                processballs
        End Sub

        Public Property Get Pos 'returns % position a ball. For debug stuff.
                dim x : for x = 0 to uBound(balls)
                        if not IsEmpty(balls(x) ) then
                                pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
                        End If
                Next                
        End Property

        Public Sub ProcessBalls() 'save data of balls in flipper range
                FlipAt = GameTime
                dim x : for x = 0 to uBound(balls)
                        if not IsEmpty(balls(x) ) then
                                balldata(x).Data = balls(x)
                        End If
                Next
                PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
                PartialFlipCoef = abs(PartialFlipCoef-1)
        End Sub
        Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect
        
        Public Sub PolarityCorrect(aBall)
                if FlipperOn() then 
                        dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

                        'y safety Exit
                        if aBall.VelY > -8 then 'ball going down
                                RemoveBall aBall
                                exit Sub
                        end if

                        'Find balldata. BallPos = % on Flipper
                        for x = 0 to uBound(Balls)
                                if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
                                        idx = x
                                        BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
                                        if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
                                end if
                        Next

                        If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
                                BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
                                if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
                        End If

                        'Velocity correction
                        if not IsEmpty(VelocityIn(0) ) then
                                Dim VelCoef
         :                         VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

                                if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

                                if Enabled then aBall.Velx = aBall.Velx*VelCoef
                                if Enabled then aBall.Vely = aBall.Vely*VelCoef
                        End If

                        'Polarity Correction (optional now)
                        if not IsEmpty(PolarityIn(0) ) then
                                If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
                                dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
        
                                if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
                                'playsound "fx_knocker"
                        End If
                End If
                RemoveBall aBall
        End Sub
End Class

'******************************************************
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
        dim x, aCount : aCount = 0
        redim a(uBound(aArray) )
        for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
                if not IsEmpty(aArray(x) ) Then
                        if IsObject(aArray(x)) then 
                                Set a(aCount) = aArray(x)
                        Else
                                a(aCount) = aArray(x)
                        End If
                        aCount = aCount + 1
                End If
        Next
        if offset < 0 then offset = 0
        redim aArray(aCount-1+offset)        'Resize original array
        for x = 0 to aCount-1                'set objects back into original array
                if IsObject(a(x)) then 
                        Set aArray(x) = a(x)
                Else
                        aArray(x) = a(x)
                End If
        Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
        ShuffleArray aArray1, offset
        ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
        dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
        Y = M*x+b
        PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
        Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
        Public Property Let Data(aBall)
                With aBall
                        x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
                        id = .ID : mass = .mass : radius = .radius
                end with
        End Property
        Public Sub Reset()
                x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
                id = Empty : mass = Empty : radius = Empty
        End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
        dim y 'Y output
        dim L 'Line
        dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
                if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
        Next
        if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
        Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

        if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
        if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

        LinearEnvelope = Y
End Function


'******************************************************
'                        FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
        FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
        FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
        FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
        FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
        Dim BOT, b

        If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
                EOSNudge1 = 1
                'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
                If Flipper2.currentangle = EndAngle2 Then 
                        BOT = GetBalls
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
                                        'Debug.Print "ball in flip1. exit"
                                         exit Sub
                                end If
                        Next
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
                                        BOT(b).velx = BOT(b).velx / 1.7
                                        BOT(b).vely = BOT(b).vely - 1
                                end If
                        Next
                End If
        Else 
                If Flipper1.currentangle <> EndAngle1 then 
                        EOSNudge1 = 0
                end if
        End If
End Sub

'*****************
' Maths
'*****************
Const PI = 3.1415927

Function dSin(degrees)
        dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
        dcos = cos(degrees * Pi/180)
End Function

'*************************************************
' Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
        Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
        DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
        Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
        AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
        DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
        Dim DiffAngle
        DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
        If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

        If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
                FlipperTrigger = True
        Else
                FlipperTrigger = False
        End If        
End Function

' Used for drop targets and stand up targets
Function Atn2(dy, dx)
        dim pi
        pi = 4*Atn(1)

        If dx > 0 Then
                Atn2 = Atn(dy / dx)
        ElseIf dx < 0 Then
                If dy = 0 Then 
                        Atn2 = pi
                Else
                        Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
                end if
        ElseIf dx = 0 Then
                if dy = 0 Then
                        Atn2 = 0
                else
                        Atn2 = Sgn(dy) * pi / 2
                end if
        End If
End Function

'*************************************************
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
        FlipperPress = 1
        Flipper.Elasticity = FElasticity

        Flipper.eostorque = EOST         
        Flipper.eostorqueangle = EOSA         
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
        FlipperPress = 0
        Flipper.eostorqueangle = EOSA
        Flipper.eostorque = EOST*EOSReturn/FReturn

        
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
                Dim BOT, b
                BOT = GetBalls
                        
                For b = 0 to UBound(BOT)
                        If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
                                If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
                        End If
                Next
        End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
        Dim Dir
        Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

        If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
                If FState <> 1 Then
                        Flipper.rampup = SOSRampup 
                        Flipper.endangle = FEndAngle - 3*Dir
                        Flipper.Elasticity = FElasticity * SOSEM
                        FCount = 0 
                        FState = 1
                End If
        ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
                if FCount = 0 Then FCount = GameTime

                If FState <> 2 Then
                        Flipper.eostorqueangle = EOSAnew
                        Flipper.eostorque = EOSTnew
                        Flipper.rampup = EOSRampup                        
                        Flipper.endangle = FEndAngle
                        FState = 2
                End If
        Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
                If FState <> 3 Then
                        Flipper.eostorque = EOST        
                        Flipper.eostorqueangle = EOSA
                        Flipper.rampup = Frampup
                        Flipper.Elasticity = FElasticity
                        FState = 3
                End If

        End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
        Dim Dir
        Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
        Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
        Dim CatchTime : CatchTime = GameTime - FCount

        if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
                if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
                        LiveCatchBounce = 0
                else
                        LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
                end If

                If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
                ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
                ball.angmomx= 0
                ball.angmomy= 0
                ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
        End If
End Sub

'*****************************************************************************************************
'*******************************************************************************************************
'END nFOZZY FLIPPERS'


'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1                            'volume level; range [0, 1]
NudgeLeftSoundLevel = 1                         'volume level; range [0, 1]
NudgeRightSoundLevel = 1                        'volume level; range [0, 1]
NudgeCenterSoundLevel = 1                       'volume level; range [0, 1]
StartButtonSoundLevel = 0.1                       'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr                     'volume level; range [0, 1]
PlungerPullSoundLevel = 1                       'volume level; range [0, 1]
RollingSoundFactor = 1.1/5

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010                      'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635                'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                                   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                                  'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel                'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel               'sound helper; not configurable
SlingshotSoundLevel = 0.95                        'volume level; range [0, 1]
BumperSoundFactor = 4.25                        'volume multiplier; must not be zero
KnockerSoundLevel = 1                           'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2                  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5                     'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5                     'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5                    'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025                 'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025                 'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8                  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075                     'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5                          'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10                     'volume multiplier; must not be zero
DTSoundLevel = 0.25                           'volume multiplier; must not be zero
RolloverSoundLevel = 0.5                                        'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8                           'volume level; range [0, 1]
BallReleaseSoundLevel = 1                       'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2                  'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015                   'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5                         'volume multiplier; must not be zero


'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
  PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
  PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
  PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
  PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
    PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
  Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
  Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
  PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


' *********************************************************************
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************

Dim tablewidth, tableheight : tablewidth = Table.width : tableheight = Table.height

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
    tmp = tableobj.y * 2 / tableheight-1

  if tmp > 7000 Then
    tmp = 7000
  elseif tmp < -7000 Then
    tmp = -7000
  end if

    If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1

  if tmp > 7000 Then
    tmp = 7000
  elseif tmp < -7000 Then
    tmp = -7000
  end if

    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
  Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
  VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
    PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
    RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
    RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
  PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
  PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
  PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
  PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub


Sub SoundPlungerPull()
  PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
  PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
  PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger1111



End Sub



'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////

Sub KnockerSolenoid()
  PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
  PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
  StopSound "fx_ballrolling0"
  StopSound "fx_ballrolling1"
  StopSound "fx_ballrolling2"
  StopSound "fx_ballrolling3"
  StopSound "fx_ballrolling4"
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
  PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
  PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
  PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
  PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
  PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
  PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
  FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
  PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
  FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
    PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
  PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
  PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
  PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
  PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
  PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
  PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
  FlipperLeftHitParm = parm/10
  If FlipperLeftHitParm > 1 Then
    FlipperLeftHitParm = 1
  End If
  FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
  CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
  'CheckFlipperSlack Activeball, LeftFlipper, parm
  RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
  FlipperRightHitParm = parm/10
  If FlipperRightHitParm > 1 Then
    FlipperRightHitParm = 1
  End If
  FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
  CheckLiveCatch Activeball, RightFlipper, RFCount, parm
  'CheckFlipperSlack Activeball, RightFlipper, parm
  RandomSoundRubberFlipper(parm)
End Sub

'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 1 	'Level of bounces. Recommmended value of 0.7

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 and aball.vely > 0 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
'        debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
'        debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub


Sub RandomSoundRubberFlipper(parm)
  PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
  PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
  RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 5 then
    RandomSoundRubberStrong 1
  End if
  If finalspeed <= 5 then
    RandomSoundRubberWeak()
  End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
  Select Case Int(Rnd*10)+1
    Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
    Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
    Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
    Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
    Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
    Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
    Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
    Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
    Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
    Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
  End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
  PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 5 then
    RandomSoundRubberStrong 1
  End if
  If finalspeed <= 5 then
    RandomSoundRubberWeak()
  End If
End Sub

Sub RandomSoundWall()
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 16 then
    Select Case Int(Rnd*5)+1
      Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
      Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
      Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
      Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
      Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
    End Select
  End if
  If finalspeed >= 6 AND finalspeed <= 16 then
    Select Case Int(Rnd*4)+1
      Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
      Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
      Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
      Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
    End Select
  End If
  If finalspeed < 6 Then
    Select Case Int(Rnd*3)+1
      Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
      Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
      Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
    End Select
  End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
  PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
  RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
  RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 16 then
    PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
  End if
  If finalspeed >= 6 AND finalspeed <= 16 then
    Select Case Int(Rnd*2)+1
      Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
      Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
    End Select
  End If
  If finalspeed < 6 Then
    Select Case Int(Rnd*2)+1
      Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
      Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
    End Select
  End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
  PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
' If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
'   RandomSoundBottomArchBallGuideHardHit()
' Else
'   RandomSoundBottomArchBallGuide
' End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 16 then
    Select Case Int(Rnd*2)+1
      Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
      Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
    End Select
  End if
  If finalspeed >= 6 AND finalspeed <= 16 then
    PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
  End If
  If finalspeed < 6 Then
    PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
  End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
  PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
  PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 10 then
    RandomSoundTargetHitStrong()
    RandomSoundBallBouncePlayfieldSoft Activeball
  Else
    RandomSoundTargetHitWeak()
  End If
End Sub

Sub Targets_Hit (idx)
  PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
  Select Case Int(Rnd*9)+1
    Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
    Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
    Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
    Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
    Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
    Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
    Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
    Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
    Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
  End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
  PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
  Select Case Int(Rnd*5)+1
    Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
    Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
    Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
    Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
    Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
  End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
  PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
  PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
  SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
  SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
  PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
  PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
  If Activeball.velx > 1 Then SoundPlayfieldGate
  StopSound "Arch_L1"
  StopSound "Arch_L2"
  StopSound "Arch_L3"
  StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
  If activeball.velx < -8 Then
    RandomSoundRightArch
  End If
End Sub

Sub Arch2_hit()
  If Activeball.velx < 1 Then SoundPlayfieldGate
  StopSound "Arch_R1"
  StopSound "Arch_R2"
  StopSound "Arch_R3"
  StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
  If activeball.velx > 10 Then
    RandomSoundLeftArch
  End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
  PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
  Select Case scenario
    Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
    Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
  End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
  Dim snd
  Select Case Int(Rnd*7)+1
    Case 1 : snd = "Ball_Collide_1"
    Case 2 : snd = "Ball_Collide_2"
    Case 3 : snd = "Ball_Collide_3"
    Case 4 : snd = "Ball_Collide_4"
    Case 5 : snd = "Ball_Collide_5"
    Case 6 : snd = "Ball_Collide_6"
    Case 7 : snd = "Ball_Collide_7"
  End Select

  PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'/////////////////////////////////////////////////////////////////
'         End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'*********************************************************************
'                    OLD Supporting Ball & Sound Functions
'*********************************************************************


Sub TargetBankWalls_Hit (idx)
  PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

'Sub Gates_Hit (idx)
'  PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'End Sub

Sub Spinner_Spin
  PlaySound "fx_spinner",0,.25,0,0.25
End Sub


'******************************************************
'   BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 10 ' total number of balls
ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
  Dim i
  For i = 0 to tnob
    rolling(i) = False
  Next
End Sub

Sub RollingSound()
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
    If BallVel(BOT(b)) > 1 Then
      rolling(b) = True
      if BOT(b).z < 10 Then 'Ball on playfield
        PlaySound ("fx_ballrolling" & b), -1, VolPlayfieldRoll(BOT(b)) * 0.05 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
      Else 'Ball on Raised Ramp
        PlaySound ("fx_ballrolling" & b), -1, VolPlayfieldRoll(BOT(b)) * 100 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b))+50000, 1, 0, AudioFade(BOT(b))
      End If

    Else
      If rolling(b) = True Then
        StopSound("fx_ballrolling" & b)
        rolling(b) = False
      End If
    End If

    '***Ball Drop Sounds***
    If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
      If DropCount(b) >= 5 Then
        DropCount(b) = 0
        If BOT(b).velz > -7 Then
          RandomSoundBallBouncePlayfieldSoft BOT(b)
        Else
          RandomSoundBallBouncePlayfieldHard BOT(b)
        End If
      End If
    End If
    If DropCount(b) < 5 Then
      DropCount(b) = DropCount(b) + 1
    End If
  Next
End Sub

'*****************************************
'     BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8,BallShadow9,BallShadow10)

Sub BallShadowUpdate()
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
        If BOT(b).X < Table.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/10) + ((BOT(b).X - (Table.Width/2))/13)) + 5
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/10) + ((BOT(b).X - (Table.Width/2))/13)) - 5
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    If BOT(b).Z > 28 then
      if BOT(b).UserValue = 1 Then
        'debug.print "ball on wireramp"
        BallShadow(b).Z = (b+1) / 100 '0.01 - 0.09 to avoid z-fighting
        BallShadow(b).RotX = 0
        BallShadow(b).size_x = 5 * ((BOT(b).Z-(ballsize/2))/70)
        BallShadow(b).size_y = 5 * ((BOT(b).Z-(ballsize/2))/70)
      Else
        BallShadow(b).Z = BOT(b).Z - Ballsize
        BallShadow(b).RotX = -25
      end if
    else
      BallShadow(b).Z = (b+1) / 100 '0.01 - 0.09 to avoid z-fighting
      BallShadow(b).RotX = 0
      BallShadow(b).size_x = 5
      BallShadow(b).size_y = 5
      BOT(b).UserValue = 0
    end If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************


' Dim leftdrop:leftdrop = 0
' Sub leftdrop1_Hit:leftdrop = 1:End Sub
' Sub leftdrop2_Hit:
'   If leftdrop = 1 then
'     PlaySound "drop_left"
'   End If
'   StopSound "RightRamp"
'   leftdrop = 0
' End Sub

' Dim rightdrop:rightdrop = 0
' Sub rightdrop1_Hit:rightdrop = 1:End Sub
' Sub rightdrop2_Hit
'   If rightdrop = 1 then
'     PlaySound "drop_Right"
'   End If
'   StopSound "RightRamp"
'   rightdrop = 0
' End Sub

Sub RRamp_Hit
  PlaySoundAtLevelActiveBall "RightRampMetal", Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

Sub LRamp_Hit
  PlaySoundAtLevelActiveBall "LeftRampMetal", Vol(ActiveBall) * MetalImpactSoundFactor
End Sub
Sub SaveLUT
    Dim FileObj
    Dim ScoreFile
    Set FileObj=CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory) then
      Exit Sub
    End if
    Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "MetallicaLUT.txt",True)
      ScoreFile.WriteLine lutpos
    Set ScoreFile=Nothing
    Set FileObj=Nothing
End Sub
Sub LoadLUT
  Dim FileObj, ScoreFile, TextStr
   Set FileObj=CreateObject("Scripting.FileSystemObject")
  If Not FileObj.FolderExists(UserDirectory) then
    Exit Sub
  End if
  If Not FileObj.FileExists(UserDirectory & "MetallicaLUT.txt") then
    lutpos=2
    Exit Sub
  End if
  Set ScoreFile=FileObj.GetFile(UserDirectory & "MetallicaLUT.txt")
  Set TextStr=ScoreFile.OpenAsTextStream(1,0)
    If (TextStr.AtEndOfStream=True) then
      Exit Sub
    End if
    lutpos = int (TextStr.ReadLine)
    Set ScoreFile = Nothing
      Set FileObj = Nothing
End Sub
Sub Table_exit()
 SaveLUT
 Controller.Stop
End sub

'******************************************************
'       FLIPPER AND RUBBER CORRECTION
'******************************************************


'****************************************************************************
'PHYSICS DAMPENERS
'****************************************************************************

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer activeball, 0.9
End Sub

Sub bzCol_Rubber_Corner_011_hit
	RubbersD.dampen Activeball
end sub

Sub zCol_Rubber_Corner_009_hit
	RubbersD.dampen Activeball
end sub

Sub dSleeves_Hit(idx) 
    SleevesD.Dampen Activeball
	TargetBouncer activeball, 0.8
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False        
FlippersD.addpoint 0, 0, 1.1        
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
        Public Print, debugOn 'tbpOut.text
        public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
        Public ModIn, ModOut
        Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

        Public Sub AddPoint(aIdx, aX, aY) 
                ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
                if gametime > 100 then Report
        End Sub

        public sub Dampen(aBall)
                if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
                dim RealCOR, DesiredCOR, str, coef
                DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
                RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
                coef = desiredcor / realcor 
                if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
                "actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
                if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

                aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
                if debugOn then TBPout.text = str
        End Sub

        public sub Dampenf(aBall, parm) 'Rubberizer is handle here
                dim RealCOR, DesiredCOR, str, coef
                DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
                RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
                coef = desiredcor / realcor 
                If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
                        aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
                End If
        End Sub

        Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
                dim x : for x = 0 to uBound(aObj.ModIn)
                        addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
                Next
        End Sub


        Public Sub Report()         'debug, reports all coords in tbPL.text
                if not debugOn then exit sub
                dim a1, a2 : a1 = ModIn : a2 = ModOut
                dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
                TBPout.text = str
        End Sub

End Class

'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00,	-4
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function


Class SlingshotCorrection
	Public DebugOn, Enabled
	private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2

	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): Enabled = True : End Sub 

	Public Property let Object(aInput) : Set Slingshot = aInput : End Property
	Public Property Let EndPoint1(aInput) : SlingX1 = aInput.x: SlingY1 = aInput.y: End Property
	Public Property Let EndPoint2(aInput) : SlingX2 = aInput.x: SlingY2 = aInput.y: End Property

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 then Report
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		If not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


	Public Sub VelocityCorrect(aBall)
		dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then 
			XL = SlingX1 : YL = SlingY1 : XR = SlingX2 : YR = SlingY2
		Else
			XL = SlingX2 : YL = SlingY2 : XR = SlingX1 : YR = SlingY1
		End If

		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then 
			If ABS(XR-XL) > ABS(YR-YL) Then 
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If

		'Velocity angle correction
		If not IsEmpty(ModIn(0) ) then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
			'debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'debug.print " " 
		End If
	End Sub

End Class




'******************************************************
'                TRACK ALL BALL VELOCITIES
'                 FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
        public ballvel, ballvelx, ballvely

        Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

        Public Sub Update()        'tracks in-ball-velocity
                dim str, b, AllBalls, highestID : allBalls = getballs

                for each b in allballs
                        if b.id >= HighestID then highestID = b.id
                Next

                if uBound(ballvel) < highestID then redim ballvel(highestID)        'set bounds
                if uBound(ballvelx) < highestID then redim ballvelx(highestID)        'set bounds
                if uBound(ballvely) < highestID then redim ballvely(highestID)        'set bounds

                for each b in allballs
                        ballvel(b.id) = BallSpeed(b)
                        ballvelx(b.id) = b.velx
                        ballvely(b.id) = b.vely
                Next
        End Sub
End Class

Sub RDampen_Timer()
       Cor.Update
End Sub

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
  Dim AB, BC, CD, DA
  AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
  BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
  CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
  DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)

  If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
    InRect = True
  Else
    InRect = False
  End If
End Function

'******************************************************
'   STAND-UP TARGET INITIALIZATION
'******************************************************

'Define a variable for each stand-up target
Dim ST12, ST15 , ST85, ST86, ST23

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
'   primary:      vp target to determine target hit
' prim:       primitive target used for visuals and animation
'             IMPORTANT!!!
'             transy must be used to offset the target animation
' switch:       ROM switch number
' animate:      Arrary slot for handling the animation instrucitons, set to 0



'ST12 = Array(sw12, sw12p,12, 0)
'ST15 = Array(sw15, sw15p,15, 0)
'ST23 = Array(sw23, sw23p,23, 0)
'ST85 = Array(sw85, frankytargets,85, 0)
'ST86 = Array(sw86, frankytargets,86, 0)


'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST12, ST15, ST85, ST86, ST23)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  0.5       'vpunits per animation step (control return to Start)
Const STMaxOffset = 6       'max vp units target moves when hit
Const STHitSound = "fx_target"  'Stand-up Target Hit sound

Const STMass = 0.2        'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'       STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
  Dim i
  i = STArrayID(switch)

  'PlayTargetSound
  STArray(i)(3) =  STCheckHit(Activeball,STArray(i)(0))

  If STArray(i)(3) <> 0 Then
    DTBallPhysics Activeball, STArray(i)(0).orientation, STMass
  End If
  DoSTAnim
End Sub

Sub STHit2(target, switch)
  'PlayTargetSound
  If STCheckHit(Activeball,target) = 1 Then
    vpmTimer.PulseSw switch
  End If
End Sub

Function STArrayID(switch)
  Dim i
  For i = 0 to uBound(STArray)
    If STArray(i)(2) = switch Then STArrayID = i:Exit Function
  Next
End Function

'Check if target is hit on it's face
Function STCheckHit(aBall, target)
  dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter
  rangle = (target.orientation - 90) * 3.1416 / 180
  bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
  bangleafter = Atn2(aBall.vely,aball.velx)

  perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
  perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle)

  If perpvel <= 0 or perpvelafter >= 0 Then
    STCheckHit = 0
  Else
    STCheckHit = 1
  End If
End Function


Sub DoSTAnim()
  Dim i
  For i=0 to Ubound(STArray)
    STArray(i)(3) = STAnimate(STArray(i)(0),STArray(i)(1),STArray(i)(2),STArray(i)(3))
  Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
  Dim animtime

  STAnimate = animate

  if animate = 0  Then
    primary.uservalue = 0
    STAnimate = 0
    Exit Function
  Elseif primary.uservalue = 0 then
    primary.uservalue = gametime
  end if

  animtime = gametime - primary.uservalue

  If animate = 1 Then
    primary.collidable = 0
    prim.transy = -STMaxOffset
    vpmTimer.PulseSw switch
    STAnimate = 2
    Exit Function
  elseif animate = 2 Then
    prim.transy = prim.transy + STAnimStep
    If prim.transy >= 0 Then
      prim.transy = 0
      primary.collidable = 1
      STAnimate = 0
      Exit Function
    Else
      STAnimate = 2
    End If
  End If
End Function

sub DTBallPhysics(aBall, angle, mass)
  dim rangle,bangle,calc1, calc2, calc3
  rangle = (angle - 90) * 3.1416 / 180
  bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))

  calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
  calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
  calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

  aBall.velx = calc1 * cos(rangle) + calc2
  aBall.vely = calc1 * sin(rangle) + calc3
End Sub


'*******************************************************
' End nFozzy Dampening'
'******************************************************


'////////////////////// Options //////////////////////

If CabinetMode = 1 Then
    PinCab_Rails.visible = 0
    PinCab_Blades.Size_y = 2000
  Else
    PinCab_Rails.visible = 1
    PinCab_Blades.Size_y = 1000
End If

DIM VRThings
If VRRoom > 0 Then
  TextBox.visible = 0
  PinCab_Backglass.blenddisablelighting = 5
  If VRRoom = 1 Then
    for each VRThings in VRCab:VRThings.visible = 1:Next
    for each VRThings in VRStuff:VRThings.visible = 0:Next
    for each VRThings in VR360:VRThings.visible = 1:Next
  End If
  If VRRoom = 2 Then
    for each VRThings in VRCab:VRThings.visible = 1:Next
    for each VRThings in VRStuff:VRThings.visible = 1:Next
    for each VRThings in VR360:VRThings.visible = 0:Next
  End If
  If VRRoom = 3 Then
    for each VRThings in VRCab:VRThings.visible = 0:Next
    for each VRThings in VRStuff:VRThings.visible = 0:Next
    for each VRThings in VR360:VRThings.visible = 0:Next
    PinCab_Backglass.visible = 1
'   PinCab_Backbox.visible = 1
    DMD.visible = 1
  End If
Else
    for each VRThings in VRCab:VRThings.visible = 0:Next
    for each VRThings in VRStuff:VRThings.visible = 0:Next
    for each VRThings in VR360:VRThings.visible = 0:Next
    If DesktopMode then TextBox.visible = 1 else TextBox.visible = 0 End If
End if



' 001 - EBisLit - New Playfield
' 002 - EBisLit - insert cutout, added insert image layer
' 003 - iaakki - Added sample flupper 3D inserts and imported requisite materials & images
' 004 - EBisLit - 3D insert creation from Iaakki template, placement and initial intensity tweaks
' 005 - EBisLit - pf artifact cleanup, mech alignments, more 3D insert tweaking (depth, intensity, falloff, frosted Lights)
' 006 = Benji - Added nfozzy physics and flipper sounds from TOM. New (?) script error discovered: "type mismatch Scoopkick"
' 007 - EBisLit - image insert re-alignment, turn on bulb display for center inserts, new center apron decal
' 008 - iaakki - Added 6 RGB inserts, reduced live catch time window to 12, as it was too easy on this table
' 009 - iaakki - Modified inserts L59-L62, Sparky voltage increased via Flash28 sub.
' 010 - EBisLit - New layer for guitar inside spider inserts. Added frosted primitives for guitar pick inserts.
' 011 - Benji - Added playfield shadows on a Flasher named "Playfield Shadow" on Layer 11
' 013 - Benji - Added Fleep sounds
' 014 - iaakki - insert bulbs retuned, GI falloff power reduced in script, RGB inserts reworked, flip tricks enabled, slackyflips disabled, flip trigger areas reworked to 23 units
' 015 - iaakki - PF altered, insert flashers reworked, items on layers 5,9,11 realigned
' 016 - iaakki - combined to sixtoes version
' 017 - iaakki - aligned stuff on layers 1,2,3 to pf. Left altered items unlocked
' 018 - iaakki - aligning continued and reworked and aligned GI on layer 4. Some lights aligned on layer 8.
' 019 - Sixtoe - Aligned more stuff including fittings and cross lights, aligned ramps and rebuilt shooter exit, adjusted captive ball section.
' 021 - iaakki - pegs, posts, rubberbands reworked, sleeves missing.. Updated physics scripts. AO shadow needs rework, snake head depth bias issues,
' 022 - iaakki - swapped nuts with lowpoly versions, PF cleanup and insert adjustments
' 023 - iaakki - pf mesh and all pf flashers realigned, fuel lights adjusted
' 024 - iaakki - Slingshot timers fix, captive ball fixed and magnets tested
' 025 - iaakki - added latest AO shadow from benji, hammer normal map, minor lights adjustments..
' 026 - Sixtoe - Added VR Room and Cabinet Mode, unified timers, removed old floodlights, dropped split slingshot GI height, tweaked a few things.
' 027 - iaakki - fixed captive ball mess, disabled apron hit sounds
' 028 - iaakki - coffin lights improved, hammer shadow added, standup targets realigned so right ramp works properly
' 029 - iaakki - captive balls and fliptriggersreworked one more time, hammer nudge added, fine tunings here and there.
' 030 - iaakki - snake tongue fixed, snake jaw nudge and material adjust, PF shadow fading with GI, New LUT and general lighting adjust, few inserts adjusted
' 031 - iaakki - new flipper bats, captive ball safe guard, snake head polished, hammer shadow z-value fixed
' 032 - iaakki - fixed some fading elements, some new flashers just to test in VR
' 033 - iaakki - moving all solenoid controlled lamps to lamp update code
' 034 - iaakki - sideblade flashers reworked with new flare image, sparky tune
' 035 - sixtoe - various fixes and lighting rework
' 036 - iaakki - lighting tune
' 037 - sixtoe - hooked up missing lights, loads more light tweaks.
' 038 - iaakki - most of the flasher code redone
' 039 - iaakki - solenoid flashers done, coffin reworked again
' 040 - sixtoe - fixed vpx ramps cutting through ramp prims, hooked up back wall coloured GI lights properly
' 041 - iaakki - left ramp entrance fixed to return correctly, right ramp entrance reworked, left orb adjusted, super skillshot fixed, flipper angles altered, elasticity reduced, livecatch tuned
' 043 - iaakki - right ramp entrance adjusted more, laneguide metals tied to GI, hammer dulled
' 044 - iaakki - hammershadow hidden, when sparky flashes
' 045 - Sixtoe - fixed sparkyhead lighting?, completely redid pop bumper lighting!, tweaked shadow layers, tidied up layers.
' 046 - iaakki - laneguide DL adjusted, f125 and f127 fixed and adjusted, hammershadow effect fixed
' 047 - Sixtoe - fixed ballshadow clashes, plunger lane and some lighting work
' 048 - iaakki - LUT changed, super skill shot tweaks, pf shadow control reverted
' 050 - tomate - LUT levels changed a bit, new textures for Dark's Sparky model, new Sparky's chair prim, new wireramps textures
' 051 - Sixtoe - Added DL for chair in line with head and body, replaced wall19 with Prim9, removed old texture switch, bit of tidying
' 052 - iaakki - wall19/prim19 db and z-fighting solved, sparky adjust, hmagnet radius 70>55 and timer change. SW42 hit threshold 3 -> 4, L22 brightness tuned
' RC1 - iaakki - hmagnet size 63, script cleanup
' RC2 - EBisLit - Adding flupper's script which allows LUT switching via Magnasave

' 101 - iaakki - table angle 6.5, flip angles adjusted, plunger adjusted, sling threshold and power changed, grave magnet made smaller, LUT selector tweaked
' 102 - iaakki - flip elasticity restored to 0.83 and left and right sling adjusted
' 103 - tomate - some fixes on the wire ramps prims, new textures, add some clearcoat over flippers prims, back to the previous LUT
' 104 - skitso - -Reworked lower PF GI lighting, Tinkered a bit with the LUT (a tad warmer tones and slight desaturation), Added HDR -ball, decreased ball's PF reflection and added higher resolution ball scratches texture, Lowered global bloom strenght setting.
' 105 - Sixtoe - fixed some playfield odditites, turned down vr cab lighting
' 106 - iaakki/oqqsan - Made lut save working, added 2 more balls to script, improved ball shadows to work on ramps
' 107 - iaakki - Csng overflow bug fixed, Rubberizer and Targetbouncer added, FlipperNudge values adjusted, Wall34 set collidable
' Thal - reduced ball rolling volume from 1.1 to 0.05, misc walls to prevent ball stuck.