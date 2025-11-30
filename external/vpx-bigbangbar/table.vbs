''************ Big Bamg Bar
''************ Capcom, 1996
''************ vp9 version by unclewilly and jimmyfingers, artwork by grizz
''************ original 3dmodels by rom extracted from his Future Pinball release
''************ vpx conversion by ninuzzu

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0 

 if table.ShowDT=true then
 sidewall1.visible=true
 sidewall2.visible=true 
else
 sidewall1.visible=false
 sidewall2.visible=false
end if

LoadVPM "01560000", "Capcom.VBS", 3.10

' Thalamus 2018-07-19
' Added/Updated "Positional Sound Playback Functions" and "Supporting Ball & Sound Functions"
' Had some custom code in this section - hope I didn't break anything.
' Supports fastflips but needs script updates
' Thalamus 2018-10-26 : Improved directional sounds

' !! NOTE : Table not verified yet !!
' Please import sample rollover to the table, it is referenced but
' not in the table.

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 1000    ' Lower number, louder ballrolling/collition sound
Const VolCol = 50      ' Ball collition divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 2    ' Bumpers volume.
Const VolRol    = 1    ' Rollovers volume.
Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
Const VolRB     = 1    ' Rubber bands volume.
Const VolRH     = 12    ' Rubber hits volume.
Const VolPo     = 1    ' Rubber posts volume.
Const VolPi     = 1    ' Rubber pins volume.
Const VolPlast  = 1    ' Plastics volume.
Const VolTarg   = 1    ' Targets volume.
Const VolWood   = 1    ' Woods volume.
Const VolKick   = 3    ' Kicker volume.
Const VolSpin   = 1.5  ' Spinners volume.
Const VolFlip   = 1    ' Flipper volume.

Dim DesktopMode:DesktopMode = Table.ShowDT
Dim UseVPMDMD:UseVPMDMD = DesktopMode

   



	Const cGameName = "bbb109"
	Const UseSolenoids = 2
	Const UseLamps = 0
	Const UseSync = 0
	Const HandleMech = 0

	'Standard Sounds
	Const SSolenoidOn = "Solenoid"
	Const SSolenoidOff = ""
	Const SCoin = "Coin"

'******Variables***************
	Dim xx
	Dim bsTrough, DTBank4, DTBank3, DTBank1, bsRHole
	Dim captb1, captb2, captb3, captb4
	Dim bsTHelp, bsSw46, bsSw47, bsSw48
	Dim bsALL, bsALR

'********Table Init************
	Sub Table_Init
	With Controller
		.GameName = cGameName
		.SplashInfoLine = "Big Bang Bar, Capcom 1996"
		.HandleMechanics = 0
		.HandleKeyboard = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.ShowTitle = 0
		.Hidden = 0  'DesktopMode
	End With
		On Error Resume Next
		Controller.Run
  	If Err Then MsgBox Err.Description
      On Error Goto 0

	'Nudging
    	vpmNudge.TiltSwitch=10
    	vpmNudge.Sensitivity=5
    	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)

'********Trough***************
	Set bsTrough = New cvpmBallStack
	With bsTrough
		.InitSw 35,36,37,38,39,0,0,0
		.InitKick BallRelease, 90, 8
		.InitExitSnd "ballrelease", "Solenoid"
		.Balls = 4
	End With

'********Right Hole***********
	Set bsRHole = New cvpmBallStack
	With bsRHole
		.InitSaucer sw67, 67, 220, 18
		.KickZ = 0.4
		.InitExitSnd "scoop_exit", "Solenoid"
		.KickForceVar = 2
	End With

     'Ball lock stacks
     Set bsSw46 = New cvpmBallStack
     With bsSw46
         .InitSw 0, 0, 0, 0, 0, 0, 0, 0
     End With

     Set bsSw47 = New cvpmBallStack
     With bsSw47
         .InitSw 0, 0, 0, 0, 0, 0, 0, 0
     End With

     Set bsSw48 = New cvpmBallStack
     With bsSw48
         .InitSw 0, 0, 0, 0, 0, 0, 0, 0
     End With

     Set bsTHelp = New cvpmBallStack  'To help if all 4 balls get caught in lower lock
     With bsTHelp
         .InitSw 0, 0, 0, 0, 0, 0, 0, 0
     End With

'********Alien Locks ***********
     Set bsALL = New cvpmBallStack
     With bsALL
         .InitSw 0, 0, 0, 0, 0, 0, 0, 0
         .InitKick sw61Out, 160, 1
     End With

     Set bsALR = New cvpmBallStack
     With bsALR
         .InitSw 0, 0, 0, 0, 0, 0, 0, 0
         .InitKick sw62Out, 190, 1
     End With

'********DropTargets
   	Set DTBank4 = New cvpmDropTarget
   	  With DTBank4
   		.InitDrop Array(sw17,sw18,sw19,sw20), Array(17,18,19,20)
        .Initsnd "", "fx_resetdrop"
       End With

   	Set DTBank3 = New cvpmDropTarget
   	  With DTBank3
   		.InitDrop Array(sw49,sw50,sw51), Array(49,50,51)
        .Initsnd "", "fx_resetdrop2"
       End With

   	Set DTBank1 = New cvpmDropTarget
   	  With DTBank1
   		.InitDrop sw58, 58
        .Initsnd "", "fx_resetdrop3"
       End With

'*******Captive balls
	' Initialize Captive Ball System
	Controller.Switch(77) = True : Controller.Switch(78) = False : Controller.Switch(79) = True : Controller.Switch(80) = False
	BP1 = True : BP2 = True : BP3 = True : BP4 = True : BP1R = False : BP2R = False : BP3L = False : BP4L = False
	capmove1.enabled = true : capmove3.enabled = true : capmove3L.enabled = false: capmove1R.enabled = false
	capmove2.enabled = true : capmove4.enabled = true : capmove4L.enabled = true : capmove2R.enabled = true
	CreateBalls

'********Main Timer init
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

'********KickBack Init
  kickback.PullBack

'********Diverters Init
  DivLR.IsDropped=1

  End Sub

   Sub Table_Paused:Controller.Pause = 1:End Sub
   Sub Table_unPaused:Controller.Pause = 0:End Sub
   Sub Table_Exit:Controller.Stop():End Sub

'***************************************
'        KEYS
'***************************************
 
'dim flipperHoldCodeLeft, flipperHoldCodeRight
 
 Sub Table_KeyDown(ByVal keycode)
    If keycode = PlungerKey Then PlaySoundAtVol "PlungerPull", Plunger, 1:Plunger.Pullback
  If keycode = LeftTiltKey Then Playsound "fx_nudge"
    If keycode = RightTiltKey Then Playsound "fx_nudge"
    If keycode = CenterTiltKey Then Playsound "nudge"
    If vpmKeyDown(keycode) Then Exit Sub
End Sub
 
Sub Table_KeyUp(ByVal keycode)
If vpmKeyUp(keycode) Then Exit Sub
    If keycode = PlungerKey Then StopSound "PlungerPull":PlaySoundAtVol "Plunger", Plunger, 1:Plunger.Fire
End Sub
 
'******************************************
'Solenoids
'******************************************
 
SolCallback(1) = "bsTrough.SolIn" 'OutHole
SolCallback(2) = "bsTrough.SolOut" 'Ball Release
SolCallback(3) = "vpmSolSound ""knocker""," 'Knocker
SolCallback(4) = "vpmSolSound ""SlingshotLeft""," 'LeftSling
SolCallback(5) = "vpmSolSound ""SlingshotRight""," 'RightSling
SolCallback(6) = "SolKickBack" 'KickBack
SolCallback(7) = "sol4Bank" '4-Bank drop target
SolCallback(8) = "SolLowerLockPin" 'Lower Lock Post
 
SolCallback(9) = "SolLFlipper" 'Left Flipper
SolCallback(10) = "SolRFlipper" 'Right Flipper
SolCallback(11) = "SolRFlipper" 'Upper Flipper
 
SolCallback(12) = "bsRHole.SolOut" 'Eject Hole
SolCallback(13) = "SolLRDIvert" 'Island Diverter
SolCallback(14) = "SolRDivert1" 'Ramp Diverter 1
SolCallback(15) = "SolRDivert2" 'Ramp Diverter 2
SolCallback(16) = "SolRDivert3" 'Alien Lock Post
SolCallback(17) = "sol3Bank" '3-Bank drop target
SolCallback(18) = "vpmSolSound ""fx_bumper1""," 'Left Bumper
SolCallback(19) = "vpmSolSound ""fx_bumper2""," 'Middle Bumper
SolCallback(20) = "vpmSolSound ""fx_bumper3""," 'Right Bumper
SolCallback(21)  ="setlamp 161," 'BackBox Left Flasher
SolCallback(22)  ="setlamp 162," 'Tube Dancer Flasher
SolCallback(23)  ="setlamp 163," 'Dance Floor Flasher
SolCallback(24)  ="setlamp 164," 'Eject Hole Flasher
SolCallback(25)  ="setlamp 165," 'Alien Lock Flasher
SolCallback(26)  ="setlamp 166," 'Lower Lock Flasher
SolCallback(27) = "GateLeft" 'Loop Left Gate
SolCallback(28) = "GateRight" 'Loop Right Gate
SolCallback(29) = "sol1Bank" '1-Bank drop target
SolCallback(30) = "solDancer" 'Tube Dancer Motor
SolCallback(31) = "SolAlienForward" 'Alien Motor Forward
SolCallback(32)  ="SolAlienReverse" 'ALien Motor Reverse
 
SolCallback(51)  ="SolGameOn" 'Game on/off control
 
'******************************************
'        Flippers
'******************************************
 
'******Flipper fix stuff till emulation is fixed
 
Dim GameStateOn

Sub SolGameOn(Enabled)
    GameStateOn = Enabled
End Sub
 
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
 
Sub SolLFlipper(Enabled)
    If Enabled And GameStateOn Then
        LeftFlipper.RotateToEnd
        PlaySoundAtVol "fx_flipperup", LeftFlipper, VolFlip
        Controller.Switch(33) = 1
    Else
        LeftFlipper.RotateToStart
        Controller.Switch(33) = 0
        PlaySoundAtVol "fx_flipperdown", LeftFlipper, VolFlip
    End If
End Sub
 
Sub SolRFlipper(Enabled)
    If Enabled And GameStateOn Then
        RightFlipper.RotateToEnd
        RightFlipper1.RotateToEnd
        PlaySoundAtVol "fx_flipperup", RightFlipper, VolFlip
        Controller.Switch(34) = 1
        Controller.Switch(68) = 1
    Else
        RightFlipper.RotateToStart
        RightFlipper1.RotateToStart
        PlaySoundAtVol "fx_flipperdown", RightFlipper, VolFlip
        Controller.Switch(34) = 0
        Controller.Switch(68) = 0
    End If
End Sub

'******************************************
'Drop Targets
'******************************************

Dim tdir

  Sub sw17_Hit:DTBank4.Hit 1:tdir = -1:me.TimerEnabled = 1:End Sub
  Sub sw18_Hit:DTBank4.Hit 2:tdir = -1:me.TimerEnabled = 1:End Sub
  Sub sw19_Hit:DTBank4.Hit 3:tdir = -1:me.TimerEnabled = 1:End Sub
  Sub sw20_Hit:DTBank4.Hit 4:tdir = -1:me.TimerEnabled = 1:End Sub

  Sub sw49_Hit:DTBank3.Hit 1:tdir = -1:me.TimerEnabled = 1:End Sub
  Sub sw50_Hit:DTBank3.Hit 2:tdir = -1:me.TimerEnabled = 1:End Sub
  Sub sw51_Hit:DTBank3.Hit 3:tdir = -1:me.TimerEnabled = 1:End Sub
  Sub sw58_Hit:DTBank1.Hit 1:tdir = -1:me.TimerEnabled = 1:End Sub

   Sub sol4Bank(Enabled)
		If Enabled Then
			tdir=1
			sw17.TimerEnabled = 1:sw18.TimerEnabled = 1:sw19.TimerEnabled = 1:sw20.TimerEnabled = 1
			DTBank4.DropSol_On
		End if
   End Sub

   Sub sol3Bank(Enabled)
		If Enabled Then
			tdir=1
			sw49.TimerEnabled = 1:sw50.TimerEnabled = 1:sw51.TimerEnabled = 1
			DTBank3.DropSol_On
		End if
   End Sub

   Sub sol1Bank(Enabled)
		If Enabled Then
			tDir = 1
			sw58.TimerEnabled = 1
			DTBank1.DropSol_On
		End if
   End Sub

  Sub sw17_Timer()
	sw17p.z=sw17p.z+1*tdir
	If sw17p.z < -50 then sw17p.z =-50:me.timerenabled = 0
	If sw17p.z>0 then sw17p.z=0:me.timerenabled = 0
  End Sub

  Sub sw18_Timer()
	sw18p.z=sw18p.z+1*tdir
	If sw18p.z < -50 then sw18p.z =-50:me.timerenabled = 0
	If sw18p.z>0 then sw18p.z=0:me.timerenabled = 0
  End Sub

  Sub sw19_Timer()
	sw19p.z=sw19p.z+1*tdir
	If sw19p.z < -50 then sw19p.z =-50:me.timerenabled = 0
	If sw19p.z>0 then sw19p.z=0:me.timerenabled = 0
  End Sub

  Sub sw20_Timer()
	sw20p.z=sw20p.z+1*tdir
	If sw20p.z < -50 then sw20p.z =-50:me.timerenabled = 0
	If sw20p.z>0 then sw20p.z=0:me.timerenabled = 0
  End Sub

  Sub sw49_Timer()
	sw49p.z=sw49p.z+1*tdir
	If sw49p.z < -50 then sw49p.z =-50:me.timerenabled = 0
	If sw49p.z>0 then sw49p.z=0:me.timerenabled = 0
  End Sub

  Sub sw50_Timer()
	sw50p.z=sw50p.z+1*tdir
	If sw50p.z < -50 then sw50p.z =-50:me.timerenabled = 0
	If sw50p.z>0 then sw50p.z=0:me.timerenabled = 0
  End Sub

  Sub sw51_Timer()
	sw51p.z=sw51p.z+1*tdir
	If sw51p.z < -50 then sw51p.z =-50:me.timerenabled = 0
	If sw51p.z>0 then sw51p.z=0:me.timerenabled = 0
  End Sub

  Sub sw58_Timer()
	sw58p.z=sw58p.z+1*tdir
	If sw58p.z < -50 then sw58p.z =-50:me.timerenabled = 0
	If sw58p.z>0 then sw58p.z=0:me.timerenabled = 0
  End Sub

'******************************************
'        DANCER ANIMATION
'******************************************
dim dance, yy

sub solDancer(enabled)
if enabled then
dancerT.enabled=1
For each yy in Dancer
yy.blenddisablelighting=0.2:Next
else
dancerT.enabled=0
For each yy in Dancer
yy.blenddisablelighting=0:Next
if dancer1.visible=0 then
dancer1.visible=1
dancer2.visible=0
dancer3.visible=0
dancer4.visible=0
dancer5.visible=0
dancer6.visible=0
dancer7.visible=0
dancer8.visible=0
dancer9.visible=0
dancer10.visible=0
end if
end if
end sub

sub dancerT_timer()
select case dance
case 1:
dancer1.visible=1
dancer2.visible=0
dancer3.visible=0
dancer4.visible=0
dancer5.visible=0
dancer6.visible=0
dancer7.visible=0
dancer8.visible=0
dancer9.visible=0
dancer10.visible=0
mechpost.rotx=0

case 2
dancer1.visible=0
dancer2.visible=1
dancer3.visible=0
dancer4.visible=0
dancer5.visible=0
dancer6.visible=0
dancer7.visible=0
dancer8.visible=0
dancer9.visible=0
dancer10.visible=0
mechpost.rotx=2

case 3
dancer1.visible=0
dancer2.visible=0
dancer3.visible=1
dancer4.visible=0
dancer5.visible=0
dancer6.visible=0
dancer7.visible=0
dancer8.visible=0
dancer9.visible=0
dancer10.visible=0
mechpost.rotx=3

case 4
dancer1.visible=0
dancer2.visible=0
dancer3.visible=0
dancer4.visible=1
dancer5.visible=0
dancer6.visible=0
dancer7.visible=0
dancer8.visible=0
dancer9.visible=0
dancer10.visible=0
mechpost.rotx=6

case 5
dancer1.visible=0
dancer2.visible=0
dancer3.visible=0
dancer4.visible=0
dancer5.visible=1
dancer6.visible=0
dancer7.visible=0
dancer8.visible=0
dancer9.visible=0
dancer10.visible=0
mechpost.rotx=4

case 6
dancer1.visible=0
dancer2.visible=0
dancer3.visible=0
dancer4.visible=0
dancer5.visible=0
dancer6.visible=1
dancer7.visible=0
dancer8.visible=0
dancer9.visible=0
dancer10.visible=0
mechpost.rotx=3

case 7
dancer1.visible=0
dancer2.visible=0
dancer3.visible=0
dancer4.visible=0
dancer5.visible=0
dancer6.visible=0
dancer7.visible=1
dancer8.visible=0
dancer9.visible=0
dancer10.visible=0
mechpost.rotx=6

case 8
dancer1.visible=0
dancer2.visible=0
dancer3.visible=0
dancer4.visible=0
dancer5.visible=0
dancer6.visible=0
dancer7.visible=0
dancer8.visible=1
dancer9.visible=0
mechpost.rotx=4

case 9
dancer1.visible=0
dancer2.visible=0
dancer3.visible=0
dancer4.visible=0
dancer5.visible=0
dancer6.visible=0
dancer7.visible=0
dancer8.visible=0
dancer9.visible=1
mechpost.rotx=2

case 10
dancer1.visible=0
dancer2.visible=0
dancer3.visible=0
dancer4.visible=0
dancer5.visible=0
dancer6.visible=0
dancer7.visible=0
dancer8.visible=0
dancer9.visible=0
dancer10.visible=1
mechpost.rotx=0
dance=1
End select
dance=dance+1

end sub

'******************************************
'         Gates
'******************************************

 dim GateStateL, GateStateR : GateStateL = 0 : GateStateR = 0
 '
 Sub GateLeft(enabled)  : If enabled then : GateL.Open = true : GateStateL = 1 : vpmTimer.AddTimer 1000, "GateCloseL" : End If : End Sub
 Sub GateCloseL(aSw)    : GateL.Open = false  : GateStateL = 0 : End Sub

 Sub GateRight(enabled) : If enabled then : GateR.Open = true : GateStateR = 1 : vpmTimer.AddTimer 1000, "GateCloseR" : End If : End Sub
 Sub GateCloseR(aSw)    : GateR.Open = false : GateStateR = 0 : End Sub


'******************************************
'			Captive ball script
'		based on pacdudes script
'******************************************

	dim mBallDir, mBallCos, mBallSin
	dim mTrig, mWall, mKickers, mVelX, mVelY, mVelX2, mVelY2
	dim ForceTransL, ForceTransR, MinForce
	dim BP1,BP2,BP3,BP4,BP1R,BP2R,BP3L,BP4L

'   Default Settings
	ForceTransL = 0.4 : ForceTransR = 0.6 : MinForce = 5 : mBallDir = 10
	mBallCos = Cos(mBallDir * 3.1415927/180) : mBallSin = Sin(mBallDir * 3.1415927/180)

'	Trigger Leads (Left/Right Side)
	Sub CapLLead_Hit() : mVelX  = activeBall.VelX : mVelY  = activeBall.VelY : End Sub
	Sub CapRLead_Hit() : mVelX2 = activeBall.VelX : mVelY2 = activeBall.VelY : End Sub

'   Left Side Captive Ball Hits (Up Motion)
	Sub CapLHit_Hit()
		dim force, dx, dy
		dX = ActiveBall.X - Captive1.X : dY = ActiveBall.Y - Captive1.Y
 		force = -ForceTransL * (dY * mVelY + dX * mVelX) * (dY * mBallCos + dX * mBallSin) / (dX*dX + dY*dY)
		If force < 1 Then Exit Sub
		If force < MinForce Then force = MinForce
		If BP3L Then : BP3L = False : ForceTransL = 0.3 :PlaySoundAtVol "fx_collide", CapMove1, 1: Captive3L.DestroyBall : CapMove3L.CreateBall : CapMove3L.Kick mBallDir, force : Else :_
		If BP4L Then : BP4L = False : ForceTransL = 0.4 : Controller.Switch(78) = False :PlaySoundAtVol "fx_collide", CapMove1, 1: Captive4L.DestroyBall : CapMove4L.CreateBall : CapMove3L.enabled = False : CapMove4L.Kick mBallDir, force : Else :_
		If BP2  Then : BP2  = False : ForceTransL = 0.6:PlaySoundAtVol "fx_collide", CapMove1, 1: Captive2.DestroyBall : CapMove2.CreateBall : CapMove4L.enabled = False : CapMove2.Kick mBallDir, force : Else :_
		If BP1  Then : BP1  = False : Controller.Switch(77) = False :PlaySoundAtVol "fx_collide", CapMove1, 1: Captive1.DestroyBall : CapMove1.CreateBall : CapMove2.enabled = False : CapMove1.Kick mBallDir, force :_
		End If : End If : End If : End If
	End Sub

'   Right Side Captive Ball Hits (Up Motion)
	Sub CapRHit_Hit()
		dim force2, dx2, dy2
		dX2 = ActiveBall.X - Captive3.X : dY2 = ActiveBall.Y - Captive3.Y
 		force2 = -ForceTransR * (dY2 * mVelY2 + dX2 * mVelX2) * (dY2 * mBallCos + dX2 * mBallSin) / (dX2*dX2 + dY2*dY2)
		If force2 < 1 Then Exit Sub
		If force2 < MinForce Then force2 = MinForce
		If BP1R Then : BP1R = False : ForceTransR = 0.3 :PlaySoundAtVol "fx_collide", CapMove3, 1: Captive1R.DestroyBall : CapMove1R.CreateBall : CapMove1R.Kick mBallDir, force2 : Else :_
		If BP2R Then : BP2R = False : ForceTransR = 0.4 : Controller.Switch(80) = False :PlaySoundAtVol "fx_collide", CapMove3, 1: Captive2R.DestroyBall : CapMove2R.CreateBall : CapMove1R.enabled = False : CapMove2R.Kick mBallDir, force2 : Else :_
		If BP4  Then : BP4  = False : ForceTransR = 0.6 :PlaySoundAtVol "fx_collide", CapMove3, 1: Captive4.DestroyBall : CapMove4.CreateBall : CapMove2R.enabled = False : CapMove4.Kick mBallDir, force2 : Else :_
		If BP3  Then : BP3  = False : Controller.Switch(79) = False :PlaySoundAtVol "fx_collide", CapMove3, 1: Captive3.DestroyBall : CapMove3.CreateBall : CapMove4.enabled = False : CapMove3.Kick mBallDir, force2 :_
		End If : End If : End If : End If
	End Sub

'   Left Side Return Hits (Down Motion)
	Sub CapMove1_Hit()
	If BP1 = True Then
	 CapMove1.DestroyBall : CapMove2_Hit
	Else
		BP1  = True : Controller.Switch(77) = True : CapMove1.DestroyBall : Captive1.CreateBall : CapMove2.enabled = True
    End If
	End Sub
	Sub CapMove2_Hit()
	If BP2 = True Then
	 CapMove2.DestroyBall : CapMove4L_Hit
	Else
		BP2  = True : ForceTransL = 0.4 : CapMove2.DestroyBall : Captive2.CreateBall : CapMove4L.enabled = True
    end if
	End Sub
	Sub CapMove4L_Hit()
	If BP4L = True Then
	 CapMove4L.DestroyBall : CapMove3L_Hit
	Else
		BP4L = True : ForceTransL = 0.3 : Controller.Switch(78) = True : CapMove4L.DestroyBall : Captive4L.CreateBall : CapMove3L.enabled = True
    End If
	End Sub
	Sub CapMove3L_Hit()
		BP3L = True : ForceTransL = 0.2 : CapMove3L.DestroyBall : Captive3L.CreateBall
	End Sub

'	Right Side Return Hits (Down Motion)
	Sub CapMove3_Hit()
	If BP3 = True Then
	 CapMove3.DestroyBall : CapMove4_Hit
	Else
		BP3  = True : Controller.Switch(79) = True : CapMove3.DestroyBall : Captive3.CreateBall : CapMove4.enabled = True
    End If
	End Sub
	Sub CapMove4_Hit()
	If BP4 = True Then
	 CapMove4.DestroyBall : CapMove2R_Hit
	Else
		BP4  = True : ForceTransR = 0.4 : CapMove4.DestroyBall : Captive4.CreateBall : CapMove2R.enabled = True
    End If
	End Sub
	Sub CapMove2R_Hit()
	If BP2R = True Then
	 CapMove2R.DestroyBall : CapMove1R_Hit
	Else
		BP2R = True : ForceTransR = 0.3 : Controller.Switch(80) = True : CapMove2R.DestroyBall : Captive2R.CreateBall : CapMove1R.enabled = True
    End If
	End Sub
	Sub CapMove1R_Hit()
		BP1R = True : ForceTransR = 0.2 : CapMove1R.DestroyBall : Captive1R.CreateBall
	End Sub

'   Destroy any Existing Balls and Create the 4 Captive Balls (either real or reel-based)
	Sub CreateBalls()
	Captive1.DestroyBall  : Captive2.DestroyBall  : Captive3.DestroyBall  : Captive4.DestroyBall
	Captive4L.DestroyBall : Captive3L.DestroyBall : Captive1R.DestroyBall : Captive2R.DestroyBall
		If BP1  Then : Captive1.CreateBall  : End If : If BP2  Then : Captive2.CreateBall  : End If : If BP4L Then : Captive4L.CreateBall : End If
		If BP3L Then : Captive3L.CreateBall : End If : If BP3  Then : Captive3.CreateBall  : End If : If BP4  Then : Captive4.CreateBall  : End If
		If BP2R Then : Captive2R.CreateBall : End If : If BP1R Then : Captive1R.CreateBall : End If
End Sub

'******************************************
'      LowerLock And Post Handling
'******************************************

dim postdwn : postdwn = 0
  Sub SolLowerLockPin(Enabled)
	if enabled then
		If lockPin.IsDropped=0 Then:PlaysoundAtVol "diverter", sw48, 1:End If
		If postdwn = 0 Then vpmTimer.AddTimer 500, "PostUp"
		lockPin.IsDropped=1:postdwn = 1
		If bsSw48.Balls>0 Then
			sw48H.DestroyBall:sw48.CreateBall:bsSw48.SolOut 1:sw48.Kick 210, 1
			If bsSw48.Balls = 0 Then
				controller.switch(48) = 0
			End If
		End If
		If bsSw47.Balls>0 Then
			sw47H.DestroyBall:sw47.CreateBall:bsSw47.SolOut 1:sw47.Kick 180, 1
			If bsSw47.Balls = 0 Then
				controller.switch(47) = 0
			End If
		End If
		If bsSw46.Balls>0 Then
			sw46H.DestroyBall:sw46.CreateBall:bsSw46.SolOut 1:sw46.Kick 180, 1
			If bsSw46.Balls = 0 Then
				controller.switch(46) = 0
			End If
		End If
		If bsTHelp.Balls>0 Then
			swTHelpH.DestroyBall:swTHelp.CreateBall:bsTHelp.SolOut 1:swTHelp.Kick 180, 1
		End If
	End If
  End Sub

  Sub PostUp(aSw)
	postdwn = 0:lockPin.IsDropped=0
  End Sub

  Sub swTHelp_Hit()
	Dim BSpeed
'	BIPFF = BIPFF - 1
	If bsTHelp.Balls>0 Then
		swTHelp.DestroyBall:sw46_Hit
	Else
		BSpeed = ActiveBall.VelY
		If bsSw46.Balls = 0 then
			swTHelp.Kick 180, BSpeed
		Else
			swTHelp.DestroyBall:swTHelpH.CreateBall:bsTHelp.AddBall 1
		End If
	End If
  End Sub

  Sub sw46_Hit()
	Dim BSpeed
	If bsSw46.Balls>0 Then
		sw46.DestroyBall:sw48_Hit
	Else
		BSpeed = ActiveBall.VelY
		If bsSw47.Balls = 0 then
			vpmTimer.PulseSw 46
			sw46.Kick 180, BSpeed
		Else
			controller.switch(46) = 1
			sw46.DestroyBall:sw46H.CreateBall:bsSw46.AddBall 1
		End If
	End If
  End Sub

  Sub sw47_Hit()
	Dim BSpeed
	If bsSw47.Balls>0 Then
		sw47.DestroyBall:sw46_Hit
	Else
		BSpeed = ActiveBall.VelY
		If bsSw48.Balls = 0 then
			vpmTimer.PulseSw 47
			sw47.Kick 180, BSpeed
		Else
			controller.switch(47) = 1
			sw47.DestroyBall:sw47H.CreateBall:bsSw47.AddBall 1
		End If
	End If
  End Sub

  Sub sw48_Hit()
	Dim BSpeed
	BSpeed = ActiveBall.VelY
	If bsSw48.Balls>0 and bsSw47.Balls>0 and bsSw46.Balls>0 Then
		sw48.Kick 210, BSpeed
	Else
		If bsSw48.Balls > 0 then
			sw48.DestroyBall:sw47_Hit
		Else
			controller.switch(48) = 1
			sw48.DestroyBall:sw48H.CreateBall:bsSw48.AddBall 1
		End If
	End If
  End Sub

'  Sub sw48_UnHit():BIPFF = BIPFF + 1:End Sub

'******************************************
'      Left Kickback
'******************************************

  Sub SolKickBack(enabled)
	If enabled then
		PlaySoundAtVol "Solenoid",KickBack,VolKick
		kickback.Fire
	else
		kickback.PullBack
	End If
  End Sub

'******************************************
'      Ramp diverters
'******************************************

  Sub SolLRDIvert(Enabled)
	If Enabled then
		debug.print "Multiball div on"
		DivLR.IsDropped=0
	PlaySoundAtVol "Diverter",Primitive18,1
	Else
		debug.print "Multiball div off"
		DivLR.IsDropped=1
	End If
  End Sub

  Sub SolRDivert1(Enabled)
	If Enabled then
		debug.print "Tube div on"
		DivTubef.RotateToEnd
		DivTube.isDropped=1
		PlaySoundAtVol "Diverter",DivTubeP,1
		vpmTimer.AddTimer 2000, "UnRDivert"
	End If
  End Sub
  Sub UnRDivert(aSw)
		debug.print "Tube div off"
		DivTubef.RotateToStart
		DivTube.isDropped=0
  End Sub

Sub SolRDivert2(Enabled)
	If Enabled then
		debug.print "Aliens div on"
		DivTube2f.RotateToEnd
		DivTube2.isDropped=1
	PlaySoundAtVol "Diverter",DivTube2P,1
	Else
		debug.print "Aliens div off"
		DivTube2f.RotateToStart
		DivTube2.isDropped=0
	End If
  End Sub

  Sub SolRDivert3(Enabled)
	If Enabled then
		debug.print "L Alien div on"
		DivLock.IsDropped=1
	PlaySoundAtVol "Diverter",GateR,1
	Else
		debug.print "L Alien div on"
		DivLock.IsDropped=0
	End If
  End Sub

'******************************************
' Alien Lock Mech Handler
'******************************************

'Note : The Startup timer is a workaround for motor overshoot caused by no power control option in VPM for the solenoids.
' - It sends an extra quarter cycle of switch changes with no animation before animation engages at actual overshoot position)
' - This prevents the alien mech from looping 2-3 times every time it starts due to position loss caused by overshoot in the
' - forward direction.  The alien mech has also been modified to correct a mis-position problem that happens sometimes after
' - Looped In Space mode ends.  The correction ensures the alien mech always faces home when it stops.  Since during the game
' - it only ever stops at the home position, this makes it fully functional, although the operator test mode doesn't function
' - as one would expect it to (making full loops instead of small movements), but that has no bearing on normal gameplay.
dim startstatus, checkstart : startstatus = 0 : checkstart = 0

  Sub SolAlienForward(Enabled)
	If enabled then
		forward = 1 : ALockTimer.enabled = 1
	Else
		forward = 0
	End If
  End Sub

  Sub SolAlienReverse(Enabled)
	If enabled then
		reverse = 1 : ALockTimer.enabled = 1
	Else
		reverse = 0
	End If
  End Sub

Sub ALockStartup_Timer() : If direction = 0 then : If checkstart = 0 and startstatus = 1 then : vpmTimer.AddTimer 1000, "CheckStatus" : End If : checkstart = 1 : Else : checkstart = 0 : End If : End Sub
Sub CheckStatus(aSw) : if checkstart = 1 Then : startstatus = 0 : OldPos = 31 : NewPos = 0 : End If : End Sub
dim NewPos, forward, reverse, OldPos : OldPos = 31 : NewPos = 0 : forward = 0 : reverse = 0
'dim RAlien:RAlien = Array(RM0,RM1,RM2,RM3,RM4,RM5,RM6,RM7,RM8,RM9)
'dim LAlien:LAlien = Array(RM10,RM19,RM18,RM17,RM16,RM15,RM14,RM13,RM12,RM11)

dim direction
Sub ALockTimer_timer()
	if reverse = 1 then : direction = -1 : end if : If forward = 1 then direction = 1 : end if
	NewPos = OldPos + direction
	If (NewPos > OldPos) and NewPos >= 32 Then NewPos = 0
	If (NewPos < OldPos) and NewPos  <  0 Then NewPos = 31
If startstatus = 0 and forward = 1 then
	if NewPos >=  7   then startstatus = 1
	Select Case NewPos
		Case 0 : controller.switch(57) = 0 ' Home Notch 1 (Reset Trick)
		Case 1 : controller.switch(57) = 1
		Case 2 : controller.switch(57) = 0 ' Home Notch 2 (Rest Trick)
		Case 3 : controller.switch(57) = 1
		Case 7 : controller.switch(57) = 1
	End Select
Else
	If forward = 1 or reverse = 1 Then
	Select Case NewPos
		Case 0 : controller.switch(57) = 0 ' Home Notch 1
		Case 1 : controller.switch(57) = 1
		Case 2 : controller.switch(57) = 0 ' Home Notch 2
		Case 3 : controller.switch(57) = 1
		Case 7 : controller.switch(57) = 1
		Case 8 : controller.switch(57) = 0 ' Quarter Turn
		Case 9 : controller.switch(57) = 1
		Case 15: controller.switch(57) = 1
		Case 16: controller.switch(57) = 0 ' Half Turn
		Case 17: controller.switch(57) = 1
		Case 23: controller.switch(57) = 1
		Case 24: controller.switch(57) = 0 ' Three Quarters Turn
		Case 25: controller.switch(57) = 1
		Case 31: controller.switch(57) = 1
	End Select
	End If
	' Animate Aliens
	Select Case NewPos
		Case  2 : Alien1.rotZ=-255:Alien2.rotZ=345  	'AlienReel.setvalue 8
		Case  3 : Alien1.rotZ=-270:Alien2.rotZ=0  		'AlienReel.setvalue 9
		Case  5 : Alien1.rotZ=-290:Alien2.rotZ=20   	'AlienReel.setvalue 9
		Case  6 : Alien1.rotZ=-315:Alien2.rotZ=45   	'AlienReel.setvalue 0
		Case  9 : Alien1.rotZ=-330:Alien2.rotZ=60   	'AlienReel.setvalue 0
		Case 10 : Alien1.rotZ=0:Alien2.rotZ=90   		'AlienReel.setvalue 1
		Case 11 : Alien1.rotZ=-15:Alien2.rotZ=105	   	'AlienReel.setvalue 1
		Case 12 : Alien1.rotZ=-30:Alien2.rotZ=120   	'AlienReel.setvalue 2
		Case 15 : Alien1.rotZ=-45:Alien2.rotZ=135   	'AlienReel.setvalue 2
		Case 16 : Alien1.rotZ=-60:Alien2.rotZ=150   	'AlienReel.setvalue 3
		Case 18 : Alien1.rotZ=-75:Alien2.rotZ=165   	'AlienReel.setvalue 3
		Case 19 : Alien1.rotZ=-90:Alien2.rotZ=180   	'AlienReel.setvalue 4
		Case 21 : Alien1.rotZ=-120:Alien2.rotZ=200   	'AlienReel.setvalue 4
		Case 22 : Alien1.rotZ=-135:Alien2.rotZ=225   	'AlienReel.setvalue 5
		Case 24 : Alien1.rotZ=-160:Alien2.rotZ=250   	'AlienReel.setvalue 5
		Case 25 : Alien1.rotZ=-180:Alien2.rotZ=270   	'AlienReel.setvalue 6
		Case 27 : Alien1.rotZ=-195:Alien2.rotZ=285   	'AlienReel.setvalue 6
		Case 28 : Alien1.rotZ=-210:Alien2.rotZ=300   	'AlienReel.setvalue 7
		Case 30 : Alien1.rotZ=-225:Alien2.rotZ=315   	'AlienReel.setvalue 7
		Case 31 : Alien1.rotZ=-240:Alien2.rotZ=330   	'AlienReel.setvalue 8

'		Case  2 : RAlien(7).IsDropped=1:RAlien(9).IsDropped=1:RAlien(8).IsDropped=0  'AlienReel.setvalue 8
'				  LAlien(7).IsDropped=1:LAlien(9).IsDropped=1:LAlien(8).IsDropped=0
'		Case  3 : RAlien(8).IsDropped=1:RAlien(0).IsDropped=1:RAlien(9).IsDropped=0   'AlienReel.setvalue 9
'				  LAlien(8).IsDropped=1:LAlien(0).IsDropped=1:LAlien(9).IsDropped=0
'		Case  5 : RAlien(0).IsDropped=1:RAlien(8).IsDropped=1:RAlien(9).IsDropped=0   'AlienReel.setvalue 9
'				  LAlien(0).IsDropped=1:LAlien(8).IsDropped=1:LAlien(9).IsDropped=0
'		Case  6 : RAlien(9).IsDropped=1:RAlien(1).IsDropped=1:RAlien(0).IsDropped=0   'AlienReel.setvalue 0
'				  LAlien(9).IsDropped=1:LAlien(1).IsDropped=1:LAlien(0).IsDropped=0
'		Case  9 : RAlien(9).IsDropped=1:RAlien(1).IsDropped=1:RAlien(0).IsDropped=0   'AlienReel.setvalue 0
'				  LAlien(9).IsDropped=1:LAlien(1).IsDropped=1:LAlien(0).IsDropped=0
'		Case 10 : RAlien(2).IsDropped=1:RAlien(0).IsDropped=1:RAlien(1).IsDropped=0   'AlienReel.setvalue 1
'				  LAlien(2).IsDropped=1:LAlien(0).IsDropped=1:LAlien(1).IsDropped=0
'		Case 11 : RAlien(2).IsDropped=1:RAlien(0).IsDropped=1:RAlien(1).IsDropped=0   'AlienReel.setvalue 1
'				  LAlien(2).IsDropped=1:LAlien(0).IsDropped=1:LAlien(1).IsDropped=0
'		Case 12 : RAlien(1).IsDropped=1:RAlien(3).IsDropped=1:RAlien(2).IsDropped=0   'AlienReel.setvalue 2
'				  LAlien(1).IsDropped=1:LAlien(3).IsDropped=1:LAlien(2).IsDropped=0
'		Case 15 : RAlien(1).IsDropped=1:RAlien(3).IsDropped=1:RAlien(2).IsDropped=0   'AlienReel.setvalue 2
'				  LAlien(1).IsDropped=1:LAlien(3).IsDropped=1:LAlien(2).IsDropped=0
'		Case 16 : RAlien(2).IsDropped=1:RAlien(4).IsDropped=1:RAlien(3).IsDropped=0   'AlienReel.setvalue 3
'				  LAlien(2).IsDropped=1:LAlien(4).IsDropped=1:LAlien(3).IsDropped=0
'		Case 18 : RAlien(2).IsDropped=1:RAlien(4).IsDropped=1:RAlien(3).IsDropped=0   'AlienReel.setvalue 3
'				  LAlien(2).IsDropped=1:LAlien(4).IsDropped=1:LAlien(3).IsDropped=0
'		Case 19 : RAlien(3).IsDropped=1:RAlien(5).IsDropped=1:RAlien(4).IsDropped=0   'AlienReel.setvalue 4
'				  LAlien(3).IsDropped=1:LAlien(5).IsDropped=1:LAlien(4).IsDropped=0
'		Case 21 : RAlien(3).IsDropped=1:RAlien(5).IsDropped=1:RAlien(4).IsDropped=0   'AlienReel.setvalue 4
'				  LAlien(3).IsDropped=1:LAlien(5).IsDropped=1:LAlien(4).IsDropped=0
'		Case 22 : RAlien(4).IsDropped=1:RAlien(6).IsDropped=1:RAlien(5).IsDropped=0   'AlienReel.setvalue 5
'				  LAlien(4).IsDropped=1:LAlien(6).IsDropped=1:LAlien(5).IsDropped=0
'		Case 24 : RAlien(4).IsDropped=1:RAlien(6).IsDropped=1:RAlien(5).IsDropped=0   'AlienReel.setvalue 5
'				  LAlien(4).IsDropped=1:LAlien(6).IsDropped=1:LAlien(5).IsDropped=0
'		Case 25 : RAlien(5).IsDropped=1:RAlien(7).IsDropped=1:RAlien(6).IsDropped=0   'AlienReel.setvalue 6
'				  LAlien(5).IsDropped=1:LAlien(7).IsDropped=1:LAlien(6).IsDropped=0
'		Case 27 : RAlien(5).IsDropped=1:RAlien(7).IsDropped=1:RAlien(6).IsDropped=0   'AlienReel.setvalue 6
'				  LAlien(5).IsDropped=1:LAlien(7).IsDropped=1:LAlien(6).IsDropped=0
'		Case 28 : RAlien(6).IsDropped=1:RAlien(8).IsDropped=1:RAlien(7).IsDropped=0   'AlienReel.setvalue 7
'				  LAlien(6).IsDropped=1:LAlien(8).IsDropped=1:LAlien(7).IsDropped=0
'		Case 30 : RAlien(6).IsDropped=1:RAlien(8).IsDropped=1:RAlien(7).IsDropped=0   'AlienReel.setvalue 7
'				  LAlien(6).IsDropped=1:LAlien(8).IsDropped=1:LAlien(7).IsDropped=0
'		Case 31 : RAlien(7).IsDropped=1:RAlien(9).IsDropped=1:RAlien(8).IsDropped=0   'AlienReel.setvalue 8
'				  LAlien(7).IsDropped=1:LAlien(9).IsDropped=1:LAlien(8).IsDropped=0
	End Select

	'Eject Lock
	If (NewPos >= 12 and NewPos <= 15) and (bsALL.balls > 0 or bsALR.balls > 0) Then
		AlienEjectR : vpmTimer.Addtimer 300, "AlienEjectL"
	End If
End If
	OldPos = NewPos
	If direction = 1 Then
		If forward = 0 and reverse = 0 and NewPos >= 6 and NewPos <= 9 Then : Direction = 0 : ALockTimer.enabled = False : StopSound "Motor": End If
	Else
		If forward = 0 and reverse = 0 Then : Direction = 0 : ALockTimer.enabled = False : End If
	End If
End Sub

	' Eject Ball Subs
dim BIPFF:BIPFF = 0

Sub AlienEjectL(aSw) : bsALL.SolOut 1 : End Sub
Sub AlienEjectR()    : bsALR.SolOut 1 : End Sub

 Sub sw61Out_UnHit():BIPFF = BIPFF + 1:End Sub
 Sub sw62Out_UnHit():BIPFF = BIPFF + 1:End Sub

'******************************************
'			Drains and Kickers
'******************************************

	Sub Drain_Hit():PlaySound "Drain":BIPFF = BIPFF - 1:bsTrough.AddBall Me:Drain.DestroyBall:End Sub
	Sub BallRelease_UnHit():BIPFF = BIPFF + 1:End Sub
	Sub sw61_Hit():BIPFF = BIPFF - 1:bsALL.AddBall Me:End Sub
	Sub sw62_Hit():BIPFF = BIPFF - 1:bsALR.AddBall Me:End Sub
	Sub sw67_Hit:Playsound "kicker_enter":bsRHole.AddBall Me:End Sub


'******************************************
'	JF's Positional SlingShot Strength
'******************************************

Dim RStep, Lstep

 Dim LeftSlingYMiddle, RightSlingYMiddle
 Dim LeftSlingYRange, RightSlingYRange
 Dim LSYPosFactor, RSYPosFactor
 Dim LSBallAngle, LSAngleFactorY
 Dim RSBallAngle, RSAngleFactorY
 Dim SlingYPosEffectStrength:SlingYPosEffectStrength=1.67 		'Use value between 1 and 2; 1 is light effect and 2 is strong effect

 Const LeftSlingYTop=1480.926			'Enter values from table control points for top and bottom Y coordinate for left and right sling
 Const LeftSlingYBottom=1673.57
 Const RightSlingYTop=1485.802
 Const RightSlingYBottom=1678.224

 LeftSlingYMiddle=(LeftSlingYBottom+LeftSlingYTop)/2
 RightSlingYMiddle=(RightSlingYBottom+RightSlingYTop)/2
 LeftSlingYRange=(LeftSlingYBottom-LeftSlingYTop)/2
 RightSlingYRange=(RightSlingYBottom-RightSlingYTop)/2


 Sub LeftSlingShot_Slingshot
	'Positional SlingShot Strength Routine Related
	GetAngle Cint(ActiveBall.VelX),Cint(ActiveBall.VelY), LSBallAngle
	LSAngleFactorY=Round(ABS(cos(LSBallAngle)),3)
	LSYPosFactor=(ABS(LeftSlingYMiddle-activeball.y))/(LeftSlingYRange/SlingYPosEffectStrength)
	DampenXY 2, (.3+LSYPosFactor), 5, 2, (.5+LSYPosFactor*LSAngleFactorY), 5
	'End Positional SlingShot Strength Routine Related
 	vpmTimer.PulseSw 41:LStep = 0:Me.TimerEnabled = 1
	LSling.Visible = 0
	LSling1.Visible = 1
	sling1.TransZ = -27
  End Sub

 Sub RightSlingShot_Slingshot
	'Positional SlingShot Strength Routine Related
	GetAngle Cint(ActiveBall.VelX),Cint(ActiveBall.VelY), RSBallAngle
	RSAngleFactorY=Round(ABS(cos(RSBallAngle)),3)
	RSYPosFactor=(ABS(RightSlingYMiddle-activeball.y))/(RightSlingYRange/SlingYPosEffectStrength)
	DampenXY 2, (.3+RSYPosFactor), 5, 2, (.5+RSYPosFactor*RSAngleFactorY), 5
	'End Positional SlingShot Strength Routine Related
 	vpmTimer.PulseSw 42:RStep = 0:Me.TimerEnabled = 1
	RSling.Visible = 0
	RSling1.Visible = 1
	sling2.TransZ = -27
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

'************************************************
'			Bumpers Animation
'************************************************

Dim dirRing1:dirRing1 = -1
Dim dirRing2:dirRing2 = -1
Dim dirRing3:dirRing3 = -1

Sub Bumper1_Hit:vpmTimer.PulseSw 56:me.TimerEnabled = 1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 54:me.TimerEnabled = 1:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 55:me.TimerEnabled = 1:End Sub

Sub Bumper1_timer()
	Bumper1Ring.Z = Bumper1Ring.Z + (5 * dirRing1)
	If Bumper1Ring.Z <= -40 Then dirRing1 = 1
	If Bumper1Ring.Z >= 0 Then
		dirRing1 = -1
		Bumper1Ring.Z = 0
		Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper2_timer()
	Bumper2Ring.Z = Bumper2Ring.Z + (5 * dirRing2)
	If Bumper2Ring.Z <= -40 Then dirRing2 = 1
	If Bumper2Ring.Z >= 0 Then
		dirRing2 = -1
		Bumper2Ring.Z = 0
		Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper3_timer()
	Bumper3Ring.Z = Bumper3Ring.Z + (5 * dirRing3)
	If Bumper3Ring.Z <= -40 Then dirRing3 = 1
	If Bumper3Ring.Z >= 0 Then
		dirRing3 = -1
		Bumper3Ring.Z = 0
		Me.TimerEnabled = 0
	End If
End Sub

'************************************************
'			Switches
'************************************************

'*******Rollover switches
 Sub sw26_Hit:Controller.Switch(26) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub

 Sub sw27_Hit:Controller.Switch(27) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw27_UnHit:Controller.Switch(27) = 0:End Sub

 Sub sw28_Hit:Controller.Switch(28) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub

 Sub sw29_Hit:Controller.Switch(29) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub

 Sub sw30_Hit:Controller.Switch(30) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw30_UnHit:Controller.Switch(30) = 0:End Sub

 Sub sw43_Hit:Controller.Switch(43) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:plungerbulb.state=2:End Sub
 Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub

 Sub sw44_Hit:Controller.Switch(44) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub

 Sub sw45_Hit:vpmtimer.addtimer 8000, "Controller.Switch(45) = 1 '":PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw45_UnHit:vpmtimer.addtimer 8000, "Controller.Switch(45) = 0 '":End Sub

 Sub sw59_Hit:Controller.Switch(59) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw59_UnHit:Controller.Switch(59) = 0:End Sub

 Sub sw60_Hit:Controller.Switch(60) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw60_UnHit:Controller.Switch(60) = 0:End Sub

 Sub sw65_Hit:Controller.Switch(65) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw65_UnHit:Controller.Switch(65) = 0:End Sub

 Sub sw66_Hit:Controller.Switch(66) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw66_UnHit:Controller.Switch(66) = 0:End Sub

'*******Ramp Switches
  Sub sw24_Hit()::vpmTimer.PulseSw 24:PlaySoundAtVol "Gate", sw24, VolGates:End Sub

 Sub sw32_Hit():Controller.Switch(32) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub

 Sub sw69_Hit:Controller.Switch(69) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw69_UnHit:Controller.Switch(69) = 0:End Sub

 Sub sw70_Hit:Controller.Switch(70) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw70_UnHit:Controller.Switch(70) = 0:End Sub

 Sub sw71_Hit:Controller.Switch(71) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw71_UnHit:Controller.Switch(71) = 0:End Sub

 Sub sw31_Hit:Controller.Switch(31) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub
 Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub

  Sub sw61a_Hit()::vpmTimer.PulseSw 61:End Sub
  Sub sw62a_Hit()::vpmTimer.PulseSw 62:End Sub

'*******Spinner
  Sub sw25_Spin():vpmTimer.PulseSw 25:PlaySoundAtVol "fx_spinner", sw25, VolSpin:End Sub

'*******StandUp Targets
  Sub sw21_Hit():vpmTimer.PulseSw 21:sw21p.transX=-5:me.TimerEnabled=1:End Sub
  Sub sw21_Timer():sw21p.transX=0:ME.TimerEnabled=0:End Sub

  Sub sw22_Hit():vpmTimer.PulseSw 22:sw22p.transX=-5:me.TimerEnabled=1:End Sub
  Sub sw22_Timer():sw22p.transX=0:ME.TimerEnabled=0:End Sub

  Sub sw23_Hit():vpmTimer.PulseSw 23:sw23p.transX=-5:me.TimerEnabled=1:End Sub
  Sub sw23_Timer():sw23p.transX=0:ME.TimerEnabled=0:End Sub

  Sub sw52_Hit():vpmTimer.PulseSw 52:sw52p.transX=-5:me.TimerEnabled=1:End Sub
  Sub sw52_Timer():sw52p.transX=0:ME.TimerEnabled=0:End Sub

  Sub sw53_Hit():vpmTimer.PulseSw 53:sw53p.transX=-5:me.TimerEnabled=1:End Sub
  Sub sw53_Timer():sw53p.transX=0:ME.TimerEnabled=0:End Sub


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
'GI
If LampState(9) = 0 and LampState(10) = 0 and lampstate(11)= 0 and lampstate (25)=0 then GILeft.state=0
If LampState(9) = 1 and LampState(10) = 1 and lampstate(11)= 1 and lampstate (25)=1 then GILeft.state=1
If LampState(12) = 0 and LampState(14) = 0  then GIBLeft.state=0
If LampState(12) = 1 and LampState(14) = 1  then GIBLeft.state=1
If LampState(17) = 0 and LampState(18) = 0 then GIRight.state=0
If LampState(17) = 1 and LampState(18) = 1 then GIRight.state=1
If LampState(21) = 0 and LampState(22) = 0 and lampstate(23)= 0 and lampstate(24)= 0 then GIBRight.state=0
If LampState(21) = 1 and LampState(22) = 1 and lampstate(23)= 1 and lampstate(24)= 1 then GIBRight.state=1
If LampState(33) = 0 and LampState(34) = 0 and lampstate(35)= 0 and lampstate (36)=0 then GITop.state=0
If LampState(33) = 1 and LampState(34) = 1 and lampstate(35)= 1 and lampstate (36)=1 then GITop.state=1

	NFadeL  9, l9			' 4-Bank GI 1
	NFadeL 10, l10			' 4-Bank GI 2
	NFadeL 11, l11			' 4-Bank GI 3
	NFadeL 12, l12			' Left Sling GI 1
	NFadeLm 14, l14			' Left Flipper GI 1
NFadeLm 14, l14a
NFadeL 14, l14b
	NFadeL 17, l17			' Upper Right Flipper GI 1
	NFadeL 18, l18			' Eject Hole GI 1
	NFadeL 21, l21			' Right Sling GI 1
	NFadeL 22, l22			' Right Sling GI 2
	NFadeLm 23, l23			' Right Flipper GI 1
NFadeL 23, l23a
	NFadeLm 24, l24			' Right Flipper GI 2
NFadeL 24, l24a
	NFadeL 25, l25			' Tube GI 1
	NFadeL 26, l26			' Tube GI 2
	NFadeL 27, l27			' Tube GI 3
	NFadeL 28, l28			' Tube GI 4
	NFadeL 29, l29			' Tube GI 5
	NFadeLm 33, l33			' Hoot GI 1
	NFadeLm 33, l33a
	NFadeLm 34, l34			' Hoot GI 2
FadeDisableLighting 34, Primitive7, 0.25
	NFadeLm 33, l33b
	NFadeLm 35, l35			' Hoot GI 3
	NFadeLm 33, l33c
	NFadeLm 36, l36			' Hoot GI 4
	NFadeLm 33, l33d
	NFadeL 37, l37			' Alien GI 1
	NFadeL 38, l38			' Alien GI 2
	NFadeL 39, l39			' Alien GI 3
	NFadeLm 40, l40			' Captive GI 1
NFadeLm 40, l40a
FadeDisableLighting 40, Primitive8, 0.2
	NFadeL 128, l128		' Upper Right Flipper GI 2

'Inserts
	NFadeL 19, l19			' Spaceship 1
	NFadeL 20, l20			' Spaceship 2
	NFadeL 30, l30			' Left Orbit Chase 1
	NFadeL 31, l31			' Left Orbit Chase 2
	NFadeL 32, l32			' Left Orbit Chase 3
	NFadeL 41, l41			' Right Orbit Chase 1
	NFadeL 42, l42			' Right Orbit Chase 2
	NFadeL 43, l43			' Right Orbit Chase 3
	NFadeLm 49, l49			' Rollover B
	Flash 49, l49r
	NFadeLm 50, l50			' Rollover A
	Flash 50, l50r
	NFadeLm 51, l51			' Rollover R
	Flash 51, l51r
	NFadeL 65, l65			' Bonus 2X
	NFadeL 66, l66			' Bonus 3X
	NFadeL 67, l67			' Mode: Underground
	NFadeL 68, l68			' Mode: Big Bang
	NFadeL 69, l69			' Mode: Bar Brawl
	NFadeL 70, l70			' Mode: Ball Busters
	NFadeL 71, l71			' Mode: Looped
	NFadeL 72, l72			' Shoot Again
	NFadeL 73, l73			' Mode: Babescanner
	NFadeL 74, l74			' Mode: Waitress
	NFadeL 75, l75			' Shoot: Cosmic Dartz
	NFadeLm 76, l76			' Special (Outlane R)
	Flash 76, l76r
	NFadeL 77, l77			' Inlane Right
	NFadeL 78, l78			' Bonus 5X
	NFadeL 79, l79			' Bonus 4X
	NFadeL 80, l80			' Mode: Tube Dancer
	NFadeL 81, l81			' Shoot: Left Orbit
	NFadeL 82, l82			' Shoot: Babescanner
	NFadeL 83, l83			' 4-Bank Mars
	NFadeL 84, l84			' 4-Bank Pythos
	NFadeL 85, l85			' 4-Bank Venus
	NFadeL 86, l86			' 4-Bank Mercury
	NFadeLm 87, l87			' Free Shot (Outlane L)
	Flash 87, l87r
	NFadeL 88, l88			' Inlane Left
	NFadeL 89, l89			' Mode: Cosmic Dartz
	NFadeL 90, l90			' Mode: Tour de Bar
	NFadeL 91, l91			' Mode: Mosh
	NFadeL 92, l92			' Mode: Happy Hour
	NFadeL 93, l93			' Mode: Extra Ball
	NFadeL 94, l94			' Mode: Get Lucky
	NFadeL 95, l95			' Mode: Loonapalooza
	NFadeL 97, l97			' Ramp Jackpot
	NFadeL 98, l98			' Ramp Standup Left
	NFadeL 99, l99			' Ramp Standup Right
	NFadeL 100, l100		' Ramp Standup Side
	NFadeL 101, l101		' Double Jackpot
	NFadeL 102, l102		' Shoot: Tour de Bar
	NFadeL 103, l103		' Shoot: Underground 1
	NFadeL 105, l105		' Captive Left 4
	NFadeL 106, l106		' Captive Left 3
	NFadeL 107, l107		' Captive Left 2
	NFadeL 108, l108		' Captive Left 1
	NFadeL 109, l109		' Captive Right 4
	NFadeL 110, l110		' Captive Right 3
	NFadeL 111, l111		' Captive Right 2
	NFadeL 112, l112		' Captive Right 1
	NFadeL 113, l113		' 3-Bank Uranus
	NFadeL 114, l114		' 3-Bank Neptune
	NFadeL 115, l115		' 3-Bank Pluto
	NFadeL 116, l116		' Shoot: Right Orbit
	NFadeL 117, l117		' DJ Eyes
	NFadeL 118, l118		' Shoot: Lunapalooza
	NFadeL 121, l121		' Shoot: Underground 2
	NFadeLm 122, l122		' Star Bumper Left
	NFadeL 122, l122a
	NFadeLm 123, l123		' Star Bumper Medium
	NFadeL 123, l123a
	NFadeLm 124, l124		' Star Bumper Right
	NFadeL 124, l124a
	NFadeL 125, l125		' Dance Floor
	NFadeL 126, l126		' Shoot: Extra Ball
	NFadeL 127, l127		' Shoot: Big Bang

' Ramp Arrows
	NFadeLm 57, L57			' Low
	FadeObj 57, l57p, "l_on", "l_b", "l_a", "l_off"
	NFadeLm 58, L58			' Mid
	FadeObj 58, l58p, "l_on", "l_b", "l_a", "l_off"
	NFadeLm 59, L59			' Top
	FadeObj 59, l59p, "l_on", "l_b", "l_a", "l_off"

NFadeL 62, neon            ' Neon Light


'Bulbs
NFadeLm 44, l44a
	Flash 44, l44			' Alien Lock Left Bulb
NFadeLm 45, l45a
	Flash 45, l45			' Alien Lock Right Bulb
NFadeLm 52, l52a
	Flash 52, l52			' Tube Sign X-Ball Bulb
NFadeLm 53, l53a
	Flash 53, l53			' Tube Sign 10 Million Bulb
NFadeLm 54, l54a
	Flash 54, l54			' Tube Sign Jackpot Bulb

	NFadeLm 104, l104		' Qualify Mode Bulb
NFadeL 104, l104a
	NFadeLm 119, l119		' Island: Lock Ready Bulb
NFadeL 119, l119a
	NFadeLm 120, l120		' Island: Mode Ready Bulb
NFadeL 120, l120a

'Flashers
	NFadeLm 161, f21			'BackBox Left Flasher
	Flash 161, f21a
	NFadeL 162, f22			'Tube Dancer Flasher
	NFadeL 163, f23			'Dance Floor Flasher
	NFadeLm 164, f24			'Eject Hole Flasher
	Flash 164, f24a
	NFadeLm 165, f25			'Alien Lock Flasher
	Flash 165, f25a
	NFadeL 166, F26 		'Lower Lock Flasher

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

Sub NFadeLm(nr, object) ' used for multiple lights
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
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
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

Sub FadeDisableLighting(nr, a, alvl)
	Select Case FadingLevel(nr)
		Case 4
			a.UserValue = a.UserValue - 0.1
			If a.UserValue < 0 Then 
				a.UserValue = 0
				FadingLevel(nr) = 0
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
		Case 5
			a.UserValue = a.UserValue + 0.50
			If a.UserValue > 1 Then 
				a.UserValue = 1
				FadingLevel(nr) = 1
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
	End Select
End Sub

'******************
' RealTime Updates
'******************
Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
    UpdateMechs
End Sub

Sub UpdateMechs
GateSWsx.RotZ= -GateL.currentangle
GateSWdx.RotZ= -GateR.currentangle
sw24p.rotx=-Spinner1.currentangle + 90
DivTubeP.rotz=DivTubef.currentangle
DivTube2P.rotz=DivTube2f.currentangle
End Sub

Sub RealTime_timer
LeftFlipperP.roty=LeftFlipper.currentangle
RightFlipperP.roty=RightFlipper.currentangle
RightUFlipperP.roty=RightFlipper1.currentangle
lfs.RotZ = LeftFlipper.CurrentAngle
rfs.RotZ = RightFlipper.CurrentAngle
BallShadowUpdate
End Sub

' *********************************************************************
'                      Ramp Helpers
' *********************************************************************

sub TubeHelper_hit(): If ActiveBall.VelY < -1 Then activeball.vely= -1 : end if:end sub

Sub LRHelpers_Hit(idx)
	If ActiveBall.VelY < 0 AND ActiveBall.VelY  > -18 Then
		ActiveBall.VelY=ActiveBall.VelY/1.08
		ActiveBall.VelX=ActiveBall.VelX/1.08
	End If
	If ActiveBall.VelY > 0 Then
		ActiveBall.VelY=ActiveBall.VelY/1.1
		ActiveBall.VelX=ActiveBall.VelX/1.1
	End If
End Sub




' *********************************************************************
'                      Extra Sounds
' *********************************************************************

'Sub LWireStart_Hit():PlaysoundAtVol "WireRamp", ActiveBall, 1:End Sub

'Sub RWireStart_Hit():PlaysoundAtVol "WireRamp", ActiveBall, 1:End Sub

'Sub RWireStart1_Hit():PlaysoundAtVol "WireRamp", ActiveBall, 1:End Sub

Sub ShooterEnd_Hit():If activeball.z > 30  Then vpmTimer.AddTimer 150, "BallHitSound":plungerbulb.state=0:End If:End Sub

Sub BallHitSound(dummy):PlaySound "ballhit":End Sub

Sub LWireEnd_Hit()
     vpmTimer.AddTimer 150, "BallHitSound":
	 StopSound "WireRamp"
 End Sub

Sub RWireEnd_Hit()
     vpmTimer.AddTimer 150, "BallHitSound":
	 StopSound "WireRamp"
 End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall)*VolTarg, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub TargetBankWalls_Hit (idx)
	PlaySound "droptarget", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall)*VolGates, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper1_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
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

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Dim Xin,Yin,rAngle,Radit,wAngle,Pi
Pi = Round(4*Atn(1),6)					'3.1415926535897932384626433832795

Sub GetAngle(Xin, Yin, wAngle)
  If Sgn(Xin) = 0 Then
    If Sgn(Yin) = 1 Then rAngle = 3 * Pi/2 Else rAngle = Pi/2
      If Sgn(Yin) = 0 Then rAngle = 0
  Else
    rAngle = atn(-Yin/Xin)
  End If
  If sgn(Xin) = -1 Then Radit = Pi Else Radit = 0
  If sgn(Xin) = 1 and sgn(Yin) = 1 Then Radit = 2 * Pi
  wAngle = round((Radit + rAngle),4)
End Sub

Sub DampenXY (dtx,dfx,rx, dty, dfy, ry)	 'dt is threshold speed, df is dampen factor 0 to 1 (higher more dampening), r is randomness
	Dim dfxRandomness
	Dim dfyRandomness
	rx=cint(rx)
	ry=cint(ry)
	dfxRandomness=INT(RND*(2*rx+1))
	dfyRandomness=INT(RND*(2*ry+1))
	dfx=dfx+(rx-dfxRandomness)*.01
	dfy=dfy+(ry-dfyRandomness)*.01
	If ABS(activeball.velx) > dtx Then activeball.velx=activeball.velx*(1-dfx*(ABS(activeball.velx)/100))
	If ABS(activeball.vely) > dty Then activeball.vely=activeball.vely*(1-dfy*(ABS(activeball.vely)/100))
End Sub

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table" is the name of the table
  Dim tmp
  tmp = tableobj.y * 2 / table.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table" is the name of the table
  Dim tmp
  tmp = tableobj.x * 2 / table.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table.width-1
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

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 10 ' total number of balls
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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, Pan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
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
    If ball1.id = 666 OR ball2.id = 666 Then
      PlaySound("rubber_hit_3"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(Ball1)
    else
      PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
    End If
End Sub

Sub BallShadowUpdate()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8,BallShadow9,BallShadow10)
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