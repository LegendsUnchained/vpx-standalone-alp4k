Option Explicit
Randomize




'***********************************************************************************
'****				        	Lamp reference      				  		  	****
'***********************************************************************************
Const lBallRelease					= 2
Const lShootAgain					= 3
Const lLeft50k1						= 5
Const lLeft50k2						= 6
Const lLeft50k3						= 7
Const lLeft50k4						= 8
Const lExtraBall					= 9
Const lLeftOutlane					= 10
Const lRightOutlane					= 11
Const lTopKicker					= 12
Const l100k							= 13
Const lCenterLamps1					= 14
Const lTopRamp						= 15
Const lCenterLamps2					= 16
Const lCar1							= 17
Const lCar2							= 18
Const lCar3							= 19
Const lCar4							= 20
Const lCar5							= 21
Const lCar6							= 22
Const lCar7							= 23
Const lCar8							= 24
Const lSpotTarget1					= 25
Const lSpotTarget2					= 26
Const lSpotTarget3					= 27
Const lSpotTarget4					= 28
Const lSpotTarget5					= 29
Const lSpotTarget6					= 30
Const lMultiplier1X					= 31
Const lMultiplier2X					= 32
Const lMultiplier4X					= 33
Const lMultiplier8X					= 34
Const lLeftSpinnerDouble			= 35
Const lDropTarget1					= 36
Const lDropTarget2					= 37
Const lDropTarget3					= 38
Const lDropTarget4					= 39
Const lDropTarget50k				= 40
Const lDropTarget100k				= 41
Const lDropTarget200k				= 42
Const lDropTargetSpecial			= 43
Const lRightExtraBall				= 44
Const lTopRace						= 45
Const lRightBottomRace				= 46
Const lRightSpinner					= 47
Const lLeftSpinner					= 51
' start of virtual lamps
Const lLightshowL1					= 60
Const lLightshowL2					= 61
Const lLightshowL3					= 62
Const lLightshowL4					= 63
Const lLightshowL5					= 64
Const lLightshowR1					= 65
Const lLightshowR2					= 66
Const lLightshowR3					= 67
Const lLightshowR4					= 68
Const lLightshowR5					= 69
Const lLeft100k						= 70
Const lRight100k					= 71

Const lFlashersTop					= 196 '(virtual lamp for top flashers)
Const lFlashersLeft					= 197 '(virtual lamp for left flashers)
Const lFlashersRight				= 198 '(virtual lamp for right flashers)
Const lGI							= 199 '(virtual lamp for GI)

'***********************************************************************************
'****				        	Switch reference      				  		  	****
'***********************************************************************************
Const swTopRightRampRollunder		= 30
Const swSpotTarget1					= 40
Const swSpotTarget4					= 41
Const swDropTarget1					= 42
Const swTopLeftRampRollunder		= 43
Const swTopRollover					= 44
Const swLeftOutlane					= 45
Const swSling						= 46
Const swSpotTarget2					= 50
Const swSpotTarget5					= 51
Const swDropTarget2					= 52
Const swLeftSpinner					= 53
Const swRightSpinner				= 54
Const swLeftReturnlane				= 55
Const swTrough						= 56
Const swTiltMe						= 57
Const swSpotTarget3					= 60
Const swSpotTarget6					= 61
Const swDropTarget3					= 62
Const swLeftRollUnder				= 63
Const swTopKicker					= 64
Const swRightReturnlane				= 65
Const swOuthole						= 66
Const swLeftSpotTarget				= 70
Const swRightSpotTarget				= 71
Const swDropTarget4					= 72
Const swLeftTrackExitRollunder		= 73
Const swRightKicker					= 74
Const swRightOutlane				= 75
Const swHoleKicker					= 76


'***********************************************************************************
'****				        	Solenoid reference      				    	****
'***********************************************************************************
const sHoleKicker					= 1
const sDropTargetsReset				= 2
const sFlashersTop	 				= 3
const sFlashersRight 				= 4
const sRightKicker	 				= 5
const sFlashersLeft	 				= 7
const sKnocker		 				= 8
const sOuthole		 				= 9
const sFlippersEnable				= 10


'***********************************************************************************
'****				       		DOF reference	 	     				    	****
'***********************************************************************************
const dShooterLane	 				= 201	' Shooterlane
const dLeftSlingshot 				= 211	' Left Slingshot
const dRightSlingshot 				= 212	' Right Slingshot

Dim OptionReset
'OptionReset = 1  'Uncomment to reset to default options in case of error OR keep all changes temporary

' Version 1.1.0: initial release
' Version 1.0.1: fixed DOF light for shooterlane

'***********************************************************************************
'****            		 Constants and global variables						****
'***********************************************************************************
const UseSolenoids	= 2
const UseLamps		= False
const UseGI			= False 								'Only WPC games have special GI circuit.
Const SCoin        = "Coin"

Dim MaxFlasherLevel: MaxFlasherLevel = 99
Dim MaxGILevel: MaxGILevel = 99
Dim ImPlunger: ImPlunger = 99
Dim OutlaneLPos: OutlaneLPos = 0

Dim Controller, cController, cGameName, bsTrough, bsTopKicker, bsRightKicker, bsHoleKicker, plungerIM
Dim DullFlippers, Tilted, FlipperEnabled, GI_On, GameOn, Hidden, i
Dim DesktopMode: DesktopMode = Victory.ShowDT
Dim DebugSwitch: DebugSwitch = False						'Don't even think about it, only Sindbad is allowed to do that ...
cGameName="victory"


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM"01001100","sys80.vbs",3.42
'OptionsLoad


Sub Victory_Init
    vpmInit Me
    With Controller
		.GameName=cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine="Victory by Sindbad"
		.HandleKeyboard=0
		.ShowTitle=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.HandleMechanics=0
		.Hidden=0
		.SolMask(0) = 0
	On Error Resume Next
		.Run GetPlayerHWnd
	End With
	If Err Then MsgBox Err.Description
	On Error Goto 0
	
	vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'"
	PinMAMETimer.Interval=PinMAMEInterval
    PinMAMETimer.Enabled=1

	' Trough handler
	Set bsTrough=New cvpmBallStack
	With bsTrough
		.InitSw swOuthole,swTrough,0,0,0,0,0,0
		.InitKick BallRelease, 90, 7
		.Balls=2
	End With

	' Top Kicker
	Set bsTopKicker=new cvpmBallStack
	With bsTopKicker
		.InitSaucer TopKicker, swTopKicker, 0, 40
	End With
		
	' Right Kicker
	Set bsRightKicker=new cvpmBallStack
	With bsRightKicker
		.InitSaucer RightKicker, swRightKicker, 0, 25
	End With

	' Hole Kicker
	Set bsHoleKicker=new cvpmBallStack
	With bsHoleKicker
		.InitSaucer HoleKicker, swHoleKicker, 160, 8
	End With

	vpmNudge.TiltSwitch = swTiltMe
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(SlingL, SlingR)

	Tilted = False
	GI_On = False
	GameOn = False
	FlipperEnabled = True
	Flippers_Init
	Backdrop_Init
	Slingshots_Init
	Outlanes_Init
	Kickers_Init
	Flashers_Init
	Illumination_Init
End Sub

Sub Victory_Paused:Controller.Pause = True:End Sub
Sub Victory_UnPaused:Controller.Pause = False:End Sub
Sub Victory_Exit:Controller.Pause = False:Controller.Stop:End Sub 



'***********************************************************************************
'****               	  Keyboard (Input) Handling								****
'***********************************************************************************
Sub Victory_KeyDown(ByVal keycode)
'	If ((keycode = StartGameKey) AND (TBLL_Credits > 0)) Then vpmTimer.AddTimer 500, "TBLL_Credits = TBLL_Credits - 1 '"
	If keycode = PlungerKey Then 
		If ImPlunger = True Then 
			PlungerIM.Pullback:PTime.Enabled = 1:Plunger.TimerEnabled = 0:Pcount = 0
		Else
			Plunger.Pullback
		End If
	End If
	If keycode = LeftTiltKey Then LeftNudge 80, 1.2, 20:PlaySound SoundFX("NudgeL",0)
    If keycode = RightTiltKey Then RightNudge 280, 1.2, 20:PlaySound SoundFX("NudgeR",0)
	If keycode = CenterTiltKey Then 
		If DebugSwitch = True Then 
			AllOn
		Else
			CenterNudge 0, 1.6, 25:PlaySound SoundFX("NudgeC",0)
		End If
	End If
   	If vpmKeyDown(keycode) Then Exit Sub 
End Sub

Sub Victory_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then 
		If ImPlunger = True Then			
			PlungerIM.Fire:PTime.Enabled = 0:Pcount = 0:PTime2.Enabled = 1
		Else
			Plunger.Fire
		End If
	End If
 	If vpmKeyUp(keycode) Then Exit Sub
End Sub



'***********************************************************************************
'****				nudging based on Noah's nudge test table					****
'*********************************************************************************** 
Dim LeftNudgeEffect, RightNudgeEffect, CenterNudgeEffect
 
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
    vpmNudge.DoNudge angle, strength * (delay-CenterNudgeEffect) / delay
    CenterNudgeEffect = delay
    CenterNudgeTimer.Interval = delay
    CenterNudgeTimer.Enabled = 1
End Sub
 
 Sub LeftNudgeTimer_Timer()
     LeftNudgeEffect = LeftNudgeEffect-1
     If LeftNudgeEffect = 0 then Me.Enabled = 0
 End Sub
 
 Sub RightNudgeTimer_Timer()
     RightNudgeEffect = RightNudgeEffect-1
     If RightNudgeEffect = 0 then Me.Enabled = 0
 End Sub
 
 Sub CenterNudgeTimer_Timer()
     CenterNudgeEffect = CenterNudgeEffect-1
     If CenterNudgeEffect = 0 then Me.Enabled = 0
 End Sub



'***********************************************************************************
'****								Knocker               						****
'*********************************************************************************** 
SolCallback(sKnocker) = "DoKnocker"

Sub DoKnocker(enabled)
	If enabled Then PlaySound SoundFX("Knocker",DOFKnocker)
End Sub



'***********************************************************************************
'****						  Drains and Kickers           						****
'***********************************************************************************
Dim CurrentBall
SolCallback(sOuthole)= "DoOuthole"
SolCallback(sRightKicker)= "DoRightKicker"
SolCallback(sHoleKicker)= "DoHoleKicker"

Sub DoOuthole(enabled)
	If enabled = True Then
		bsTrough.EntrySol_On
		If Tilted = True then SetGIon: Tilted = False
	End If
End Sub

Sub DoBallRelease
	If Tilted = True then SetGIon: Tilted = False
	PlaySound SoundFX("BallRelease",DOFContactors)
	BallRelease.TimerEnabled = True
End Sub
 
Sub BallRelease_Timer()
 	Me.TimerEnabled = False
	bsTrough.ExitSol_On
End Sub

Sub TopKicker_Hit(): PlaySound "SaucerHit": bsTopKicker.AddBall Me: End Sub
Sub RightKicker_Hit(): PlaySound "SaucerHit": bsRightKicker.AddBall Me: End Sub
Sub HoleKicker_Hit(): PlaySound "SaucerHit": bsHoleKicker.AddBall Me: End Sub

Sub DoTopKicker
	PlaySound SoundFX("SaucerKick",DOFContactors)
	bsTopKicker.ExitSol_On
End Sub

Sub DoRightKicker(enabled)
	If enabled = True Then 
		PlaySound SoundFX("SaucerKick",DOFContactors)
		bsRightKicker.ExitSol_On
	End If
End Sub

Sub DoHoleKicker(enabled)
	If enabled = True Then 
		PlaySound SoundFX("SaucerKick",DOFContactors)
		bsHoleKicker.ExitSol_On
	End If
End Sub

Sub Drain_Hit()
	PlaySound "Drain"
	bsTrough.AddBall Me
 	If Tilted = True then SetGIon: Tilted = False
End Sub

Sub Kickers_Init
End Sub



'***********************************************************************************
'****							Flippers                 						****
'***********************************************************************************

SolCallback(sLRFlipper) = "SolFlip FlipperLR, FlipperUR," ' Right Flipper
SolCallback(sLLFlipper) = "SolFlip FlipperLL, FlipperUL," ' Left Flipper
SolCallback(sFlippersEnable) = "DoEnable"
Sub FlipperLL_Collide(parm):PlaySound "RubberFlipper":End Sub
Sub FlipperLR_Collide(parm):PlaySound "RubberFlipper":End Sub
Sub FlipperUL_Collide(parm):PlaySound "RubberFlipper":End Sub
Sub FlipperUR_Collide(parm):PlaySound "RubberFlipper":End Sub

Sub SolFlip(aFlip1, aFlip2, aEnabled) 
	If aEnabled Then
		If DullFlippers = 1 Then
			PlaySound SoundFX("FlipperUpDull",DOFFlippers)
		Else
			PlaySound SoundFX("FlipperUp",DOFFlippers)
		End If
		aFlip1.RotateToEnd: aFlip2.RotateToEnd
	Else
		If DullFlippers = 1 Then
			PlaySound SoundFX("FlipperDownDull",DOFFlippers)
		Else
			PlaySound SoundFX("FlipperDown",DOFFlippers)
		End If
		aFlip1.RotateToStart: aFlip2.RotateToStart
	End If
End Sub

Sub DoEnable(enabled)
 	If enabled = True Then 
		vpmNudge.SolGameOn enabled	
		If GI_On = False Then SetGIOn
		SLingL.Disabled = 0
		SLingR.Disabled = 0
		FlipperEnabled = True
	Else
		FlipperEnabled = False
		SLingL.Disabled = 1
		SLingR.Disabled = 1
	End If
End Sub

Sub Flippers_Init
	FlipperLL.Visible = 0:FlipperLL.Enabled = 1:FlipperLLP.Visible = 1
	FlipperLR.Visible = 0:FlipperLR.Enabled = 1:FlipperLRP.Visible = 1
	FlipperUL.Visible = 0:FlipperUL.Enabled = 1:FlipperULP.Visible = 1
	FlipperUR.Visible = 0:FlipperUR.Enabled = 1:FlipperURP.Visible = 1
End Sub

'***********************************************************************************
'****							Slingshot and walls        						****
'*********************************************************************************** 
Dim SLPos,SRPos

Sub SlingL_Slingshot()
	If FlipperEnabled = False Then Exit Sub
	LSling1.Visible = 0:LSling2.Visible = 0:LSling3.Visible = 0: LSling4.Visible = 1: LSling.TransZ = -27
	vpmTimer.PulseSw(swSling): PlaySound SoundFX ("SlingL",DOFContactors),1:DOF dLeftSlingshot, 2
	SLPos = 0: Me.TimerEnabled = 1
	LightshowChangeSide
End Sub 

Sub SlingL_Timer
    Select Case SLPos
        Case 2:	LSling1.Visible = 0:LSling2.Visible = 0:LSling3.Visible = 1: LSling4.Visible = 0: LSling.TransZ = -17
		Case 3:	LSling1.Visible = 0:LSling2.Visible = 1:LSling3.Visible = 0: LSling4.Visible = 0: LSling.TransZ = -8
		Case 4:	LSling1.Visible = 1:LSling2.Visible = 0:LSling3.Visible = 0: LSling4.Visible = 0: LSling.TransZ = 0 :Me.TimerEnabled = 0
    End Select
    SLPos = SLPos + 1
End Sub

Sub SlingR_Slingshot()
	If FlipperEnabled = False Then Exit Sub
	RSling1.Visible = 0:RSling2.Visible = 0:RSling3.Visible = 0: RSling4.Visible = 1: RSling.TransZ = -27
	vpmTimer.PulseSw(swSling): PlaySound SoundFX ("SlingR",DOFContactors):DOF dRightSlingshot, 2
	SRPos = 0: Me.TimerEnabled = 1
	LightshowChangeSide
End Sub 

Sub SlingR_Timer
    Select Case SRPos
        Case 2:	RSling1.Visible = 0:RSling2.Visible = 0:RSling3.Visible = 1: RSling4.Visible = 0: RSling.TransZ = -17
		Case 3:	RSling1.Visible = 0:RSling2.Visible = 1:RSling3.Visible = 0: RSling4.Visible = 0: RSling.TransZ = -8
		Case 4:	RSling1.Visible = 1:RSling2.Visible = 0:RSling3.Visible = 0: RSling4.Visible = 0: RSling.TransZ = 0 :Me.TimerEnabled = 0
    End Select
    SRPos = SRPos + 1
End Sub

Sub Slingshots_Init
	LSling1.Visible = 1:LSling2.Visible = 0:LSling3.Visible = 0: LSling4.Visible = 0: LSling.TransZ = 0
	RSling1.Visible = 1:RSling2.Visible = 0:RSling3.Visible = 0: RSling4.Visible = 0: RSling.TransZ = 0
End Sub

Sub Outlanes_Init
	Dim OutlaneLPosArray: OutlaneLPosArray = Array(1144, 1157.5, 1169)
	OutlaneLP1.Y = OutlaneLPosArray(OutlaneLPos): OutlaneLP2.Y = OutlaneLPosArray(OutlaneLPos): OutlaneLP3.Y = OutlaneLPosArray(OutlaneLPos)
	Select Case OutlaneLPos
		Case 0:RubberOutlaneL1.Collidable = 1: RubberOutlaneL2.Collidable = 0: RubberOutlaneL3.Collidable = 0
		Case 1:RubberOutlaneL1.Collidable = 0: RubberOutlaneL2.Collidable = 1: RubberOutlaneL3.Collidable = 0
		Case 2:RubberOutlaneL1.Collidable = 0: RubberOutlaneL2.Collidable = 0: RubberOutlaneL3.Collidable = 1
	End Select
End Sub



'***********************************************************************************
'****								  Drop Targets								****
'***********************************************************************************

Class DropTarget
  Private m_switchnumber, m_dropobject1, m_dropobject2, m_switchobject, m_increment, m_position

  Public Property Get SwitchNumber(): SwitchNumber = m_switchnumber: End Property
  Public Property Let SwitchNumber(input): m_switchnumber = input: End Property

  Public Property Get DropObject1(): Set DropObject1 = m_dropobject1: End Property
  Public Property Let DropObject1(input): Set m_dropobject1 = input: End Property

  Public Property Get DropObject2(): Set DropObject2 = m_dropobject2: End Property
  Public Property Let DropObject2(input): Set m_dropobject2 = input: End Property

  Public Property Get SwitchObject(): Set SwitchObject = m_switchobject: End Property
  Public Property Let SwitchObject(input): Set m_switchobject = input: End Property

  Public Property Get Increment(): Increment = m_increment: End Property
  Public Property Let Increment(input): m_increment = input: End Property

  Public Property Get Position(): Position = m_position: End Property
  Public Property Let Position(input): m_position = input: End Property

  Public default Function init(switchnumber, dropobject1, dropobject2, switchobject, increment, position)
     m_switchnumber = switchnumber
    Set m_dropobject1 = dropobject1
    Set m_dropobject2 = dropobject2
    Set m_switchobject = switchobject
    m_increment = increment
    m_position = position

    Set Init = Me
  End Function
End Class


Dim DropTargetPositions: DropTargetPositions=Array(-50, -40, -30, -20, -10, 0, 10, 10)
Dim DropTargets: DropTargets=Array((new DropTarget)(swDropTarget1, sw42, Nothing, sw42p,30,0),(new DropTarget)(swDropTarget2, sw52, Nothing, sw52p,30,0), (new DropTarget)(swDropTarget3, sw62, Nothing, sw62p,30,0),(new DropTarget)(swDropTarget4, sw72, Nothing, sw72p,30,0))

SolCallBack(sDropTargetsReset) = "DoDropTargetsUp"
Sub sw42_Hit: DropTargetHit 0: End Sub
Sub sw52_Hit: DropTargetHit 1: End Sub
Sub sw62_Hit: DropTargetHit 2: End Sub
Sub sw72_Hit: DropTargetHit 3: End Sub
Sub DoDropTargetsUp(enabled): If enabled then DropTargetBankUp 0, 3:End If: End Sub

Sub DropTargetBankUp(first, last)
	Dim i
	For i = first to last: DropTargets(i).Position = 1: Next
	PlaySound SoundFX("BankReset",DOFContactors)
End Sub

Sub DropTargetHit(nr)
 	If Tilted = True OR DropTargets(nr).Position <> 0 then Exit Sub
 	PlaySound SoundFX("DropTargetHit",DOFDropTargets) 
	DropTargets(nr).Position = -1
End Sub

Sub DropTargetsTimer_Timer
	Dim i
	For i = 0 to uBound(DropTargets)
		If DropTargets(i).Position < 0 Then DropTargetDown i										' Drop Targets that have to go down
		If DropTargets(i).Position > 0 Then DropTargetUp i										' Drop Targets that have to go up
	Next
End Sub

Sub DropTargetDown(i)
	Dim pos
	If DropTargets(i).Position = -6 Then															' Bottom position reached?
		DropTargets(i).Position = 0																' If yes, mark status as parked
		If NOT DropTargets(i).DropObject1 is Nothing Then DropTargets(i).DropObject1.IsDropped = 1			' Mark "down" postion in DropTarget
		If NOT DropTargets(i).DropObject2 is Nothing Then DropTargets(i).DropObject2.IsDropped = 1			' Mark "down" postion in DropTarget
		If DropTargets(i).SwitchNumber > 0 Then Controller.Switch(DropTargets(i).SwitchNumber) = 1				' Turn ROM switch on 
	Else
		pos = DropTargetPositions(DropTargets(i).Position + 5) + DropTargets(i).Increment				' calculate new position
		If DropTargets(i).SwitchObject.z > pos Then 													' is current position higher?
			DropTargets(i).SwitchObject.z = pos														' if yes, step down
		End If																				' Drop Target moved!
		DropTargets(i).Position = DropTargets(i).Position - 1											' Decrement position variable
	End If
End Sub

Sub DropTargetUp(i)
	Dim pos
	If DropTargets(i).Position = 8 Then															' Top position reached?
		DropTargets(i).Position = 0																' If yes, mark status as parked
		If NOT DropTargets(i).DropObject1 is Nothing Then DropTargets(i).DropObject1.IsDropped = 0			' Mark "up" postion in DropTarget
		If NOT DropTargets(i).DropObject2 is Nothing Then DropTargets(i).DropObject2.IsDropped = 0			' Mark "up" postion in DropTarget
		DropTargets(i).SwitchObject.z = DropTargets(i).Increment + DropTargetPositions(5)					' Drive the primitive in "Up" position
		If DropTargets(i).SwitchNumber > 0 Then Controller.Switch(DropTargets(i).SwitchNumber) = 0				' Turn ROM switch off 
	Else
		pos = DropTargetPositions(DropTargets(i).Position) + DropTargets(i).Increment					' calculate new position
		If DropTargets(i).SwitchObject.z < pos Then 													' is current position lower?
			DropTargets(i).SwitchObject.z = pos														' if yes, step up
		End If																				' Drop Target moved!
	DropTargets(i).Position = DropTargets(i).Position + 1												' Decrement position in variable
	End If
End Sub



'***********************************************************************************
'****						      Targets                 						****
'*********************************************************************************** 
Sub sw70_Hit:sw70p.TransX = 4:vpmTimer.PulseSw(swLeftSpotTarget):Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw70_Timer:sw70p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw71_Hit:sw71p.TransX = 4:vpmTimer.PulseSw(swRightSpotTarget):Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw71_Timer:sw71p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw40_Hit:sw40p.TransX = 4:vpmTimer.PulseSw(swSpotTarget1):Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw40_Timer:sw40p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw41_Hit:sw41p.TransX = 4:vpmTimer.PulseSw(swSpotTarget4):Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw41_Timer:sw41p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw50_Hit:sw50p.TransX = 4:vpmTimer.PulseSw(swSpotTarget2):Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw51_Hit:sw51p.TransX = 4:vpmTimer.PulseSw(swSpotTarget5):Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw51_Timer:sw51p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw60_Hit:sw60p.TransX = 4:vpmTimer.PulseSw(swSpotTarget3):Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw60_Timer:sw60p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw61_Hit:sw61p.TransX = 4:vpmTimer.PulseSw(swSpotTarget6):Me.TimerEnabled = 1:PlaySound SoundFX("Target",DOFTargets):End Sub
Sub sw61_Timer:sw61p.TransX = 0:Me.TimerEnabled = 0:End Sub



'***********************************************************************************
'****						   Rollovers and triggers      						****
'*********************************************************************************** 
Sub sw44_Hit():PlaySound "Sensor":Controller.Switch(swTopRollover) = 1:sw44p.Visible = 0:End Sub
Sub sw44_UnHit():Controller.Switch(swTopRollover) = 0:sw44p.Visible = 1:End Sub
Sub sw45_Hit():PlaySound "Sensor":Controller.Switch(swLeftOutlane) = 1:sw45p.Visible = 0:End Sub
Sub sw45_UnHit():Controller.Switch(swLeftOutlane) = 0:sw45p.Visible = 1:End Sub
Sub sw55_Hit():PlaySound "Sensor":Controller.Switch(swLeftReturnlane) = 1:sw55p.Visible = 0:End Sub
Sub sw55_UnHit():Controller.Switch(swLeftReturnlane) = 0:sw55p.Visible = 1:End Sub
Sub sw65_Hit():PlaySound "Sensor":Controller.Switch(swRightReturnlane) = 1:sw65p.Visible = 0:End Sub
Sub sw65_UnHit():Controller.Switch(swRightReturnlane) = 0:sw65p.Visible = 1:End Sub
Sub sw75_Hit():PlaySound "Sensor":Controller.Switch(swRightOutlane) = 1:sw75p.Visible = 0:End Sub
Sub sw75_UnHit():Controller.Switch(swRightOutlane) = 0:sw75p.Visible = 1:End Sub
Sub TopKickerTrigger_Hit():PlaySound "Sensor":TopKickerTriggerP.Visible = 0:End Sub
Sub TopKickerTrigger_UnHit():TopKickerTriggerP.Visible = 1:End Sub
Sub RightKickerTrigger_Hit():PlaySound "Sensor":RightKickerTriggerP.Visible = 0:End Sub
Sub RightKickerTrigger_UnHit():RightKickerTriggerP.Visible = 1:End Sub
Sub swShooterLane_Hit():Set CurrentBall = ActiveBall:DOF dShooterLane, 1:End Sub
Sub swShooterLane_UnHit():DOF dShooterLane, 0:End Sub



'***********************************************************************************
'****						   Spinners and gates	      						****
'*********************************************************************************** 
Sub sw30_Hit():PlaySound "Gate":Controller.Switch(swTopRightRampRollunder) = 1:End Sub
Sub sw30_UnHit():Controller.Switch(swTopRightRampRollunder) = 0:End Sub
Sub sw43_Hit():PlaySound "Gate":Controller.Switch(swTopLeftRampRollunder) = 1:End Sub
Sub sw43_UnHit():Controller.Switch(swTopLeftRampRollunder) = 0:End Sub
Sub sw53_Spin():PlaySound "Spinner":vpmTimer.PulseSw (swLeftSpinner):End Sub
Sub sw54_Spin():PlaySound "Spinner":vpmTimer.PulseSw (swRightSpinner):End Sub
Sub sw63_Hit():PlaySound "Gate":Controller.Switch(swLeftRollUnder) = 1:End Sub
Sub sw63_UnHit():Controller.Switch(swLeftRollUnder) = 0:End Sub
Sub sw73_Hit():PlaySound "Gate":Controller.Switch(swLeftTrackExitRollunder) = 1:End Sub
Sub sw73_UnHit():Controller.Switch(swLeftTrackExitRollunder) = 0:End Sub


'***********************************************************************************
'****				Init siderails and stuff for desktop mode 					****
'***********************************************************************************
Sub Backdrop_Init
	Dim iw
	If DesktopMode = True Then
		RampLB.Visible = 1:RampLT.Visible = 1:RampRB.Visible = 1:RampRT.Visible = 1
		For each iw in cBackdropLights: iw.Visible = True:Next
		LedsTimer.Interval = 33
		LedsTimer.Enabled = 1
	Else
		RampLB.Visible = 0:RampLT.Visible = 0:RampRB.Visible = 0:RampRT.Visible = 0
		For each iw in cBackdropLights: iw.Visible = False:Next
	End If
End Sub



'***********************************************************************************
'**** 							 Light routines						        	****
'***********************************************************************************
Dim LampState(200), FadingState(200)
Dim FlasherState(200), FlasherLevel(200), FlasherMaxLevel(200), FlasherUser(200), FlasherUpSpeed, FlasherDownSpeed
Dim Bulb_Array: Bulb_Array = Array("Texture Bulb_off", "Texture Bulb_B", "Texture Bulb_A", "Texture Bulb_on")
Dim DomeRed_Array: DomeRed_Array = Array("DomeRed_off", "DomeRed_B", "DomeRed_A", "DomeRed_on")
Dim DomeOrange_Array: DomeOrange_Array = Array("DomeOrange_off", "DomeOrange_B", "DomeOrange_A", "DomeOrange_on")
Dim DomeBlue_Array: DomeBlue_Array = Array("DomeBlue_off", "DomeBlue_B", "DomeBlue_A", "DomeBlue_on")

SolCallback(sFlashersTop) = "SolFlash lFlashersTop," ' Top Flashers
SolCallback(sFlashersRight) = "SolFlash lFlashersRight," ' Right Flashers
SolCallback(sFlashersLeft) = "SolFlash lFlashersLeft," ' Left Flashers

Sub SolFlash(nr, enabled)
	If enabled Then
		LampState(nr) = 1:FadingState(nr) = 5:FlasherState(nr) = 5
	Else
		LampState(nr) = 0:FadingState(nr) = 4:FlasherState(nr) = 4
	End If
End Sub

Sub Illumination_Init
	AllLampsOff()
	AllFlashersOff()
    FlasherUpSpeed = 20
    FlasherDownSpeed = 4
	FlasherTimer.Interval = 10
	FlasherTimer.Enabled = 1
	LampTimer.Interval = 30			
	LampTimer.Enabled = 1
End Sub

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
	If Tilted Then Exit Sub
	chgLamp = Controller.ChangedLamps
	If Not IsEmpty(chgLamp) Then
		GameOn = True: If GI_On = False Then SetGIOn
		If DebugSwitch = False Then
			For ii = 0 To UBound(chgLamp)
				LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
				FadingState(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
				FlasherState(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
			Next
		End If
	End If
    UpdateLamps
End Sub

Sub AllLampsOff()
	Dim x
	For x = 0 to 200:LampState(x)=0:FadingState(x)=4:Next
	UpdateLamps:UpdateLamps:Updatelamps
End Sub

Sub AllFlashersOff()
    Dim x
    For x = 0 to 200:FlasherState(x)=4:FlasherLevel(x)=0:FlasherMaxLevel(x)=MaxFlasherLevel:Next
	FlasherMaxLevel(lGI) = MaxGILevel
End Sub

Sub AllOn
	Dim x
	LightshowTimer.Enabled = False
	AllLampsOff
	For x = 0 to 200:LampState(x)=1:FadingState(x)=5:FlasherState(x)=5:Next
	UpdateLamps:UpdateLamps:Updatelamps
End Sub

Sub UpdateLamps
	If GameOn = False Then Exit Sub
	If LampState(lBallRelease) = 1 Then DoBallRelease:LampState(lBallRelease) = 0
	If LampState(lTopKicker) = 1 Then DoTopKicker:LampState(lTopKicker) = 0
	FadeLights LShootAgain, Array(l3, l3g)
	FadeLights lLeft50k1, Array(l5, l5g)
	FadeLights lLeft50k2, Array(l6, l6g)
	FadeLights lLeft50k3, Array(l7, l7g)
	FadeLights lLeft50k4, Array(l8, l8g)
	FadeLights lExtraBall, Array(l9, l9g)
	FadeLights lLeftOutlane, Array(l10, l10bg)
	FadeLights lRightOutlane, Array(l11, l11bg)
	FadeLights lLeft100k, Array(l70, l70g)
	FadeLights lRight100k, Array(l71, l71g)
	FadeLights lCenterLamps1, Array(l14a, l14b, l14ag, l14bg)
	nFadePrim lTopRamp, Bulb15, Bulb_Array
	FadeLight lTopRamp, l15
	FadeLights lCenterLamps2, Array(l16a, l16b, l16ag, l16bg)
	FadeLights lCar1, Array(l17a, l17b, l17ag, l17bg)
	FadeLights lCar2, Array(l18a, l18b, l18ag, l18bg)
	FadeLights lCar3, Array(l19a, l19b, l19ag, l19bg)
	FadeLights lCar4, Array(l20a, l20b, l20ag, l20bg)
	FadeLights lCar5, Array(l21a, l21b, l21ag, l21bg)
	FadeLights lCar6, Array(l22a, l22b, l22ag, l22bg)
	FadeLights lCar7, Array(l23a, l23b, l23ag, l23bg)
	FadeLights lCar8, Array(l24a, l24b, l24ag, l24bg)
	FadeLights lSpotTarget1, Array(l25a, l25b, l25ag, l25bg)
	FadeLights lSpotTarget2, Array(l26a, l26b, l26ag, l26bg)
	FadeLights lSpotTarget3, Array(l27a, l27b, l27ag, l27bg)
	FadeLights lSpotTarget4, Array(l28a, l28b, l28ag, l28bg)
	FadeLights lSpotTarget5, Array(l29a, l29b, l29ag, l29bg)
	FadeLights lSpotTarget6, Array(l30a, l30b, l30ag, l30bg)
	FadeLights lMultiplier1X, Array(l31, l31g)
	FadeLights lMultiplier2X, Array(l32, l32g)
	FadeLights lMultiplier4X, Array(l33, l33g)
	FadeLights lMultiplier8X, Array(l34, l34g)
	FadeLights lLeftSpinnerDouble, Array(l35, l35g)
	FadeLights lDropTarget1, Array(l36	, l36g)
	FadeLights lDropTarget2, Array(l37, l37g)
	FadeLights lDropTarget3, Array(l38, l38g)	
	FadeLights lDropTarget4, Array(l39, l39g)
	FadeLights lDropTarget50k, Array(l40, l40g)
	FadeLights lDropTarget100k, Array(l41, l41g)
	FadeLights lDropTarget200k, Array(l42, l42g)
	FadeLights lDropTargetSpecial, Array(l43, l43g)
	FadeLight lRightExtraBall, l44
	FadeLights lTopRace, Array(l45a, l45b, l45ag, l45bg)
	FadeLights lRightBottomRace, Array(l46, l46g)
	FadeLights lRightSpinner, Array(l47a, l47b, l47ag, l47bg)
	FadeLights lLeftSpinner, Array(l51a, l51b, l51ag, l51bg)
FadeLights lFlashersLeft, Array(lo,lr,lo1,lr1)
FadeLights lFlashersRight, Array(ro,rr,ro1,rr1)
FadeLights lFlashersTop, Array(lb,rb,lb1,lr1)
	nFadePrim lFlashersRight, DomeRightRed, DomeRed_Array
	FadePrim lFlashersRight, DomeRightOrange, DomeOrange_Array
	nFadePrim lFlashersLeft, DomeLeftRed, DomeRed_Array
	FadePrim lFlashersLeft, DomeLeftOrange, DomeOrange_Array
	nFadePrim lFlashersTop, DomeLeftBlue, DomeBlue_Array
	FadePrim lFlashersTop, DomeRightBlue, DomeBlue_Array
End Sub

Sub FadeLight(nr, light)
	Select Case FadingState(nr)
		Case 2:FadingState(nr) = 0
		Case 3:FadingState(nr) = 2
		Case 4:light.State = LightStateOff:FadingState(nr) = 3
		Case 5:light.State = LightStateOn:FadingState(nr) = 1
	End Select
End Sub

Sub FadeLights(nr, lights)
	Dim ip
	Select Case FadingState(nr)
		Case 2:FadingState(nr) = 0
		Case 3:FadingState(nr) = 2
		Case 4:For each ip in lights:ip.State = LightStateOff:Next:FadingState(nr) = 3
		Case 5:For each ip in lights:ip.State = LightStateOn:Next:FadingState(nr) = 1
	End Select
End Sub

Sub FadePrim(nr, prim, group)
	Select Case FadingState(nr)
		Case 2:prim.Image = group(0):prim.DisableLighting = 0:FadingState(nr) = 0
		Case 3:prim.Image = group(1):FadingState(nr) = 2
		Case 4:prim.Image = group(2):FadingState(nr) = 3
		Case 5:prim.Image = group(3):prim.DisableLighting = 1:FadingState(nr) = 1
	End Select
End Sub

Sub nFadePrim(nr, prim, group)
	Select Case FadingState(nr)
		Case 2:prim.Image = group(0):prim.DisableLighting = 0
		Case 3:prim.Image = group(1)
		Case 4:prim.Image = group(2)
		Case 5:prim.Image = group(3):prim.DisableLighting = 1
	End Select
End Sub

Sub FadePrims(nr, prims, group)
	Dim ip
	Select Case FadingState(nr)
		Case 2:For each ip in prims:ip.Image = group(0):Next:FadingState(nr) = 0
		Case 3:For each ip in prims:ip.Image = group(1):Next:FadingState(nr) = 2
		Case 4:For each ip in prims:ip.Image = group(2):Next:FadingState(nr) = 3
		Case 5:For each ip in prims:ip.Image = group(3):Next:FadingState(nr) = 1
	End Select
End Sub

Sub SetGIOn
	For each i in GI_Bulbs: i.State = LightStateOn: Next
	FadingState(lGI) = 5: FlasherState(lGI) = 5
	GI_On = True
End Sub

Sub FlasherTimer_Timer()
	Dim fi
	If GI_On = False Then Exit Sub
	FlashMultiple lGI, GI_Flashers, 8, 8
	FlashMultiple lFlashersLeft, Array(FlasherLeftRed, FlasherLeftOrange), 25, 5
	FlashMultiple lFlashersRight, Array(FlasherRightRed, FlasherRightOrange), 25, 5
	FlashMultiple lFlashersTop, Array(FlasherRightBlue, FlasherLeftBlue), 25, 5
	FlashMultiple lTopRamp, Array(Flasher15Bulb, Flasher15PF, Flasher15P1, Flasher15P2), 25, 5
	FlashMultiple lLightshowL1, Array(FL1L, FL6L), 50, 15
	FadeLights lLightshowL1, Array(FL1LG, FL6LG)
	FlashMultiple lLightshowL2, Array(FL2L, FL7L), 50, 15
	FadeLights lLightshowL2, Array(FL2LG, FL7LG)
	FlashMultiple lLightshowL3, Array(FL3L, FL8L), 50, 15
	FadeLights lLightshowL3, Array(FL3LG, FL8LG)
	FlashMultiple lLightshowL4, Array(FL4L, FL9L), 50, 15
	FadeLights lLightshowL4, Array(FL4LG, FL9LG)
	FlashMultiple lLightshowL5, Array(FL5L, FL10L), 50, 15
	FadeLights lLightshowL5, Array(FL5LG, FL10LG)
	FlashMultiple lLightshowR1, Array(FL1R, FL6R), 50, 15
	FadeLights lLightshowR1, Array(FL1RG, FL6RG)
	FlashMultiple lLightshowR2, Array(FL2R, FL7R), 50, 15
	FadeLights lLightshowR2, Array(FL2RG, FL7RG)
	FlashMultiple lLightshowR3, Array(FL3R, FL8R), 50, 15
	FadeLights lLightshowR3, Array(FL3RG, FL8RG)
	FlashMultiple lLightshowR4, Array(FL4R, FL9R), 50, 15
	FadeLights lLightshowR4, Array(FL4RG, FL9RG)
	FlashMultiple lLightshowR5, Array(FL5R, FL10R), 50, 15
	FadeLights lLightshowR5, Array(FL5RG, FL10RG)
if FL1Lg.state = 1 then Bulb1L.blenddisablelighting = 5:Bulb6L.blenddisablelighting = 5 else Bulb1L.blenddisablelighting = 0:Bulb6L.blenddisablelighting = 0
if FL2Lg.state = 1 then Bulb2L.blenddisablelighting = 5:Bulb7L.blenddisablelighting = 5 else Bulb2L.blenddisablelighting = 0:Bulb7L.blenddisablelighting = 0
if FL3Lg.state = 1 then Bulb3L.blenddisablelighting = 5:Bulb8L.blenddisablelighting = 5 else Bulb3L.blenddisablelighting = 0:Bulb8L.blenddisablelighting = 0
if FL4Lg.state = 1 then Bulb4L.blenddisablelighting = 5:Bulb9L.blenddisablelighting = 5 else Bulb4L.blenddisablelighting = 0:Bulb9L.blenddisablelighting = 0
if FL5Lg.state = 1 then Bulb5L.blenddisablelighting = 5:Bulb10L.blenddisablelighting = 5 else Bulb5L.blenddisablelighting = 0:Bulb10L.blenddisablelighting = 0
if FL1Rg.state = 1 then Bulb1R.blenddisablelighting = 5:Bulb6R.blenddisablelighting = 5 else Bulb1R.blenddisablelighting = 0:Bulb6R.blenddisablelighting = 0
if FL2Rg.state = 1 then Bulb2R.blenddisablelighting = 5:Bulb7R.blenddisablelighting = 5 else Bulb2R.blenddisablelighting = 0:Bulb7R.blenddisablelighting = 0
if FL3Rg.state = 1 then Bulb3R.blenddisablelighting = 5:Bulb8R.blenddisablelighting = 5 else Bulb3R.blenddisablelighting = 0:Bulb8R.blenddisablelighting = 0
if FL4Rg.state = 1 then Bulb4R.blenddisablelighting = 5:Bulb9R.blenddisablelighting = 5 else Bulb4R.blenddisablelighting = 0:Bulb9R.blenddisablelighting = 0
if FL5Rg.state = 1 then Bulb5R.blenddisablelighting = 5:Bulb10R.blenddisablelighting = 5 else Bulb5R.blenddisablelighting = 0:Bulb10R.blenddisablelighting = 0
End Sub

			

Sub Flashers_Init
	Dim ifl, F_Array, B_Array
	F_Array = Array(Array(FL1L, 465, 286, 184, 465, 296, 184,1), Array(FL2L, 430, 348, 184, 430, 358, 184,1), Array(FL3L, 379, 400, 184, 379, 410, 184,1), _
					Array(FL4L, 324, 454, 184, 324, 464, 184,1), Array(FL5L, 269, 508, 184, 269, 518, 184,1), Array(FL6L, 215, 562, 182, 215, 572, 182,1), _
					Array(FL7L, 158, 617, 175, 158, 627, 175,1), Array(FL8L, 103, 673, 170, 103, 683, 170,1), Array(FL9L, 62, 744, 163, 62, 754, 163,1), _ 
					Array(FL10L, 59, 819, 159, 59, 788, 159,1), Array(FL1R, 918, 355, 184, 916, 360, 184,1), Array(FL2R, 900, 418, 183, 900, 418, 183,1), _ 
					Array(FL3R, 882, 481, 182, 882, 491, 182,1), Array(FL4R, 866, 546, 182, 866, 556, 182,1), Array(FL5R, 849, 614, 181, 849, 624, 181,1), _
					Array(FL6R, 835, 689, 181, 833, 699, 181,1), Array(FL7R, 829, 761, 181, 826, 771, 181,1), Array(FL8R, 825, 841, 178, 820, 851, 178,1), _
					Array(FL9R, 821, 922, 169, 816, 932, 169,1), Array(FL10R, 816, 999, 164, 812, 1009, 164,1), Array(FlasherRightRed, 817, 1153, 171, 767, 1335, 241,0), _
					Array(FlasherRightOrange, 873, 1245, 171, 813, 1418, 241,0), Array(FlasherLeftRed, 61, 989, 171, 111, 1171, 241,0), Array(FlasherLeftOrange, 58, 1083, 171, 118, 1256, 241,0), _
					Array(FlasherLeftBlue, 363, 187, 228, 383, 473, 241,0), Array(FlasherRightBlue, 448, 187, 228, 448, 473, 241,0))
	B_Array = Array(Array(FL1RG, 919, 355, 916, 36), Array(FL2RG, 901, 405, 901, 405), Array(FL3RG, 882, 469, 882, 469), Array(FL4RG, 864, 533, 854, 533), Array(FL5RG, 850, 603, 850, 603), _
					Array(FL6RG, 837, 678, 834, 678), Array(FL7RG, 830, 749, 826, 749), Array(FL8RG, 826, 830, 820, 851), Array(FL9RG, 821, 911, 816, 911), Array(FL10RG, 816, 989, 812, 989))

	For each ifl in F_Array
		If (DesktopMode = False) Then 
'			ifl(0).RotX = -Victory.InclinationFS / 2
			ifl(0).X = ifl(1)
			ifl(0).Y = ifl(2)
			ifl(0).Height = ifl(3)
		Else
			ifl(0).X = ifl(4)
			ifl(0).Y = ifl(5)
			ifl(0).Height = ifl(6)
'			If ifl(7) = 1 Then 	ifl(0).RotX = -Victory.Inclination / 2
		End If
	Next
	For each ifl in B_Array
		If (DesktopMode = False) Then 
			ifl(0).X = ifl(1)
			ifl(0).Y = ifl(2)
		Else
			ifl(0).X = ifl(3)
			ifl(0).Y = ifl(4)
		End If
	Next
End Sub

Sub SetMaxIlluLevels
    Dim x
    For x = 0 to 200:FlasherMaxLevel(x)=MaxFlasherLevel:Next
	FlasherMaxLevel(LGI) = MaxGILevel
	FlasherMaxLevel(lFlashersLeft) = MaxFlasherLevel
	FlasherMaxLevel(lFlashersRight) = MaxFlasherLevel
	FlasherMaxLevel(lFlashersTop) = MaxFlasherLevel
	FlasherMaxLevel(lTopRamp) = MaxFlasherLevel
End Sub

Sub FlashSingle(nr, object, stepup, stepdown) 
    Select Case FlasherState(nr)
        Case 4 'off
            FlasherLevel(nr) = FlasherLevel(nr) - stepdown
            If FlasherLevel(nr) < 0 Then
                FlasherLevel(nr) = 0
                FlasherState(nr) = 0 'completely off
            End if
            Object.Opacity = FlasherLevel(nr)
        Case 5 ' on
            FlasherLevel(nr) = FlasherLevel(nr) + stepup
            If FlasherLevel(nr) > FlasherMaxLevel(nr) Then
                FlasherLevel(nr) = FlasherMaxLevel(nr)
                FlasherState(nr) = 1 'completely on
            End if
            Object.Opacity = FlasherLevel(nr)
        Case 6 ' Pulse
            FlasherLevel(nr) = FlasherLevel(nr) + stepup
            If FlasherLevel(nr) > FlasherMaxLevel(nr) Then
                FlasherLevel(nr) = FlasherMaxLevel(nr)
                FlasherState(nr) = 4 'switch to off
            End if
            Object.Opacity = FlasherLevel(nr)
    End Select
End Sub

Sub FlashMultiple(nr, object, stepup, stepdown)
	Dim ih
    Select Case FlasherState(nr)
        Case 4 'off
            FlasherLevel(nr) = FlasherLevel(nr) - stepdown
            If FlasherLevel(nr) < 0 Then
				FlasherLevel(nr) = 0
                FlasherState(nr) = 0 'completely off
			End If
            For each ih in object:ih.Opacity = FlasherLevel(nr):Next
        Case 5 ' on
            FlasherLevel(nr) = FlasherLevel(nr) + stepup
            If FlasherLevel(nr) > FlasherMaxLevel(nr) Then
                FlasherLevel(nr) = FlasherMaxLevel(nr)
                FlasherState(nr) = 1 'completely on
            End if
            For each ih in object:ih.Opacity = FlasherLevel(nr):Next
        Case 6 ' Pulse
            FlasherLevel(nr) = FlasherLevel(nr) + stepup
            If FlasherLevel(nr) > FlasherMaxLevel(nr) Then
                FlasherLevel(nr) = FlasherMaxLevel(nr)
                FlasherState(nr) = 4 'switch to off
            End if
            For each ih in object:ih.Opacity = FlasherLevel(nr):Next
    End Select
End Sub

Dim LightshowCycle, LightshowSide: LightshowSide = 0
Dim Virtual_L13_Lamps: Virtual_L13_Lamps=Array(lLeft100k, lRight100k)
Dim LightshowStartBulbs: LightshowStartBulbs = Array(lLightshowL1, lLightshowR1)
Dim oldL13_state

Sub LightshowTimer_Timer
	FlasherState(LightshowStartBulbs(LightshowSide)+LightshowCycle) = 4
	FadingState(LightshowStartBulbs(LightshowSide)+LightshowCycle) = 4
	LightshowCycle = (LightshowCycle + 1) Mod 5
	FlasherState(LightshowStartBulbs(LightshowSide)+LightshowCycle) = 5
	FadingState(LightshowStartBulbs(LightshowSide)+LightshowCycle) = 5
	If FadingState(l100k) <> oldL13_state Then	
		FadingState(Virtual_L13_Lamps(LightshowSide)) = FadingState(l100k)
		oldL13_state = FadingState(l100k)
	End If
End Sub

Sub LightshowChangeSide
	Dim oldlampstate: oldlampstate = FadingState(Virtual_L13_Lamps(LightshowSide))
	FadingState(Virtual_L13_Lamps(LightshowSide)) = 4
	LightshowTimer.Enabled = False
	LightshowClr(LightshowSide)
	LightshowSide = LightshowSide XOR 1
	If ((oldlampstate = 5) OR (oldlampstate = 1)) Then FadingState(Virtual_L13_Lamps(LightshowSide)) = 5
	LightshowTimer.Enabled = True
End Sub

Sub LightshowClr(side)
	Dim li
	For li = 0 to 4:FlasherState(LightshowStartBulbs(side)+li) = 4:FadingState(LightshowStartBulbs(side)+li) = 4:Next
	LightshowCycle = 9
End Sub

Sub Lightshow_Init
	LightshowClr 0: LightshowClr 1: LightshowCycle = 9
	LightshowTimer.Interval = 150
	LightshowTimer.Enabled = True
End Sub

Dim Digits(40)
Digits(0) = Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
Digits(1) = Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
Digits(2) = Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
Digits(3) = Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
Digits(4) = Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
Digits(5) = Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
Digits(6) = Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
Digits(7) = Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
Digits(8) = Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
Digits(9) = Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
Digits(10) = Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
Digits(11) = Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
Digits(12) = Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
Digits(13) = Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
Digits(14) = Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
Digits(15) = Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
Digits(16) = Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
Digits(17) = Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
Digits(18) = Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
Digits(19) = Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
Digits(20) = Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
Digits(21) = Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
Digits(22) = Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
Digits(23) = Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
Digits(24) = Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
Digits(25) = Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
Digits(26) = Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
Digits(27) = Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
Digits(28) = Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
Digits(29) = Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
Digits(30) = Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
Digits(31) = Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)
Digits(32) = Array(c00, c05, c0c, c0d, c08, c01, c06, c0f, c02, c03, c04, c07, c0b, c0a, c09, c0e)
Digits(33) = Array(c10, c15, c1c, c1d, c18, c11, c16, c1f, c12, c13, c14, c17, c1b, c1a, c19, c1e)
Digits(34) = Array(c20, c25, c2c, c2d, c28, c21, c26, c2f, c22, c23, c24, c27, c2b, c2a, c29, c2e)
Digits(35) = Array(c30, c35, c3c, c3d, c38, c31, c36, c3f, c32, c33, c34, c37, c3b, c3a, c39, c3e)
Digits(36) = Array(c40, c45, c4c, c4d, c48, c41, c46, c4f, c42, c43, c44, c47, c4b, c4a, c49, c4e)
Digits(37) = Array(c50, c55, c5c, c5d, c58, c51, c56, c5f, c52, c53, c54, c57, c5b, c5a, c59, c5e)
Digits(38) = Array(c60, c65, c6c, c6d, c68, c61, c66, c6f, c62, c63, c64, c67, c6b, c6a, c69, c6e)
Digits(39) = Array(c70, c75, c7c, c7d, c78, c71, c76, c7f, c72, c73, c74, c77, c7b, c7a, c79, c7e)

Sub LedsTimer_Timer()
	On Error Resume Next
	Dim ChgLED,ii,num,chg,stat,obj

	ChgLED = Controller.ChangedLEDs(&HFFFFFFFF, &HFFFFFFFF) 
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(ChgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			For Each obj In Digits(num)
				If chg And 1 Then obj.State = stat And 1
				chg = chg\2 : stat = stat\2
			Next
		Next
	End IF
End Sub

Sub DOF(dofevent, dofstate)	
	If cController < 3 Then Exit Sub
	If dofstate = 2 Then
		Controller.B2SSetData dofevent, 1:Controller.B2SSetData dofevent, 0
	Else
		Controller.B2SSetData dofevent, dofstate
	End If
End Sub 



'***********************************************************************************
'****				        		Sound routines      				    	****
'***********************************************************************************
Dim BallFalling: BallFalling = 0
Sub cWalls_Rubber_Hit(idx):PlaySound "RubberHit", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0.25:End Sub
Sub cWalls_Metal_Hit(idx):PlaySound "MetalHit", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0.25:End Sub
Sub cTrackEntries_Hit(idx):PlaySound "BallRollingMetal":End Sub
Sub cTrackExits_Hit(idx):If ActiveBall.z > 50 Then:StopSound "BallRolling": BallFalling = 1:End If:End Sub
Sub cTrackExits_UnHit(idx):If BallFalling = 1 Then:PlaySound "BallHit", 0:BallFalling = 0:End If:End Sub


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
    tmp = tableobj.y * 2 / Victory.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / Victory.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 6000)
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
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub BallShadow_Timer()
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
ballShadow(b).opacity = 90
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub


'***********************************************************************************
'****				DIP switch routines (parts by Luvthatapex) 					****
'***********************************************************************************
Dim TableOptions, TableName

Sub CustomizeTable
	Sys80ShowDips
	OptionsEdit
End Sub

Sub OptionsEdit
	Dim DT_offs: DT_offs = 0
	Dim vpmDips1: Set vpmDips1 = New cvpmDips
	With vpmDips1
		.AddForm 350, 380, "Victory - Table Options"
		.AddLabel 0,0 + DT_offs,110,15,"Sound Options"
		.AddChkExtra 15,15 + DT_offs,155, Array("Enable dull flippers*", &H00000080)
		.AddLabel 0,45 + DT_offs,180,15,"Graphics Options"
		.AddFrameExtra 15,60 + DT_offs,205,"GI Brigthness*",&H00000300, Array("100% (normal)", 0, "200% (bright)", &H00000100, "300% (brighter)", &H00000200)
		.AddFrameExtra 15,140 + DT_offs,205,"Flasher Brigthness*",&H00000C00, Array("100% (normal)", 0, "200% (bright)", &H00000400, "300% (brighter)", &H00000800)
		.AddLabel 0,215 + DT_offs,180,15,"Play Options"
		.AddFrameExtra 15,230 + DT_offs,205,"Left Outlane*",&H00003000, Array("Liberal", 0, "Normal", &H00001000, "Difficult", &H00002000)
		.AddChkExtra 0,305 + DT_offs,155, Array("Disable Menu Next Start", &H00000001)
	End With
	TableOptions = vpmDips1.ViewDipsExtra(TableOptions)
	SaveValue TableName,"Options",TableOptions
	OptionsToVariables
End Sub

Sub OptionsLoad
	TableName="Victory"
	Set vpmShowDips = GetRef("CustomizeTable")
 	TableOptions = LoadValue(TableName,"Options")			
	If TableOptions = "" Or OptionReset Then 
		TableOptions = 1				
		OptionsEdit							
	Else
		If TableOptions And 1 = 0 Then
			TableOptions = TableOptions OR 1		
			OptionsEdit							
		Else
			OptionsToVariables
		End If
	End If
End Sub

Sub OptionsToVariables
	Dim newGI, newFlash, newOL
	If DesktopMode = False Then
		cController = ((TableOptions AND &H00000070) / &H00000010)
	Else
		cController = 1
	End If
	DullFlippers = (TableOptions AND &H00000080) / &H00000080
	newGI = (((TableOptions AND &H00000300) / &H00000100) + 1) * 100
	newFlash = (((TableOptions AND &H00000C00) / &H00000400) + 1) * 100
	If ((newGI <> MaxGILevel) OR (newFlash <> MaxFlasherLevel)) Then
		MaxGILevel = newGI: MaxFlasherLevel = newFlash
		If GI_On = True Then SetMaxIlluLevels: FlasherState(LGI) = 5
	End If
	newOL = ((TableOptions AND &H00003000) / &H00001000)
	If newOL <> OutlaneLPos Then
		OutlaneLPos = newOL
		Outlanes_Init
	End If
End Sub



'***********************************************************************************
'****							RealTime Updates								****
'***********************************************************************************
Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
	UpdateSpinners
    UpdateFlippers
'	DebugShowAll
End Sub

Sub UpdateSpinners
	sw30p.RotZ = -(sw30s.currentangle)
	sw43p.RotZ = -(sw43s.currentangle)
	sw53p.RotZ = -(sw53.currentangle)
	sw54p.RotZ = -(sw54.currentangle)
	sw63p.RotZ = -(sw63.currentangle)
	sw73p.RotZ = -(sw73s.currentangle)
	GateBRP.RotZ = ABS(GateBR.currentangle)
End Sub

Sub UpdateFlippers
	FlipperLLP.RotY = FlipperLL.CurrentAngle
	FlipperLRP.RotY = FlipperLR.CurrentAngle
	FlipperULP.RotY = FlipperUL.CurrentAngle
	FlipperURP.RotY = FlipperUR.CurrentAngle
End Sub



'***********************************************************************************
'****								Misc stuff									****
'***********************************************************************************
Dim Pi: Pi = 4 * Atn(1)
Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
	if ABS(dSin) < 0.000001 Then dSin = 0
	if ABS(dSin) > 0.999999 Then dSin = 1 * sgn(dSin)
End Function

Function dCos(degrees)
	dCos = cos(degrees * Pi/180)
	if ABS(dCos) < 0.000001 Then dCos = 0
	if ABS(dCos) > 0.999999 Then dCos = 1 * sgn(dCos)
End Function



'***********************************************************************************
'****				        		Debug Stuff		      				    	****
'***********************************************************************************
Sub DebugPulseSwitch
	Dim sw
	sw = InputBox("Enter a number")
	VPMTimer.PulseSw sw
End Sub

Sub DebugShowLamps
	Dim ii,tmp: tmp= ""
	For ii = 1 to 30:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 31 to 60:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 61 to 90:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 91 to 120:tmp = tmp & (Controller.Lamp(ii) AND 1):Next
	DebugText.Text = tmp
End Sub

Sub DebugShowSwitches
	Dim ii,tmp: tmp= ""
	For ii = 1 to 30:tmp = tmp & (Controller.Switch(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 31 to 60:tmp = tmp & (Controller.Switch(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 61 to 90:tmp = tmp & (Controller.Switch(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 91 to 120:tmp = tmp & (Controller.Switch(ii) AND 1):Next
	DebugText.Text = tmp
End Sub

Sub DebugShowSolenoids
	Dim ii,tmp: tmp= ""
	For ii = 1 to 30:tmp = tmp & (Controller.Solenoid(ii) AND 1):Next
	DebugText.Text = tmp
End Sub

Sub DebugPosBall
	CurrentBall.X = 245:CurrentBall.Y = 516:CurrentBall.Z = 25:CurrentBall.VelX = -3:CurrentBall.VelY = -40
End Sub

Sub DebugShowAll
	Dim ii,tmp: tmp= "Solenoid Matrix" & Chr(13)
	For ii = 1 to 30:tmp = tmp & (Controller.Solenoid(ii) AND 1):Next
	tmp= tmp & Chr(13) & "Switch Matrix" & Chr(13)
	For ii = 1 to 30:tmp = tmp & (Controller.Switch(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 31 to 60:tmp = tmp & (Controller.Switch(ii) AND 1):Next
	tmp= tmp & Chr(13) & "Lamp Matrix" & Chr(13)
	For ii = 1 to 30:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 31 to 60:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 61 to 90:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 91 to 120:tmp = tmp & (Controller.Lamp(ii) AND 1):Next
	DebugText.Text = tmp
	DebugText.Text = tmp
End Sub
