option Explicit 
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

' ball size
Const BallSize 				= 50				' sets the ball size
Const BallMass 				= 1.95

Const cgamename = "viprsega", UseSolenoids=1,UseLamps=0,UseGI=0, SCoin="coin3"

LoadVPM "01530000","sega.vbs",3.1

'Flipper Rampup mode: 0 = fast, 1 = medium, 2 = slow (tap passes should work)
dim FlipperCoilRampupMode : FlipperCoilRampupMode = 0

'************************
'Glowball
'*************************

' *** Ball Settings ********** Change this values on line 276
'6 = GlowBall
'0 = Normal Ball

Dim ChooseBall : ChooseBall 			= 0		
							

'************************
' Viper Cars Headlight Mod
'************************
Dim ViperCarLights : ViperCarLights = 1	'Set to 0 to disable Headlights			


'************************
' PLAYFIELD SHADOW INTENSITY DURING GI OFF OR ON (adds additional visual depth) 
' usable range is 0 (lighter) - 100 (darker)  >  On and Off are separated file , not on same scale...
'************************
Dim ShadowOpacityGIOff : ShadowOpacityGIOff = 75
Dim ShadowOpacityUVOn : ShadowOpacityUVOn  = 90
Dim ShadowOpacityGIOn : ShadowOpacityGIOn  = 90

					
'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

SolCallback(1)="bsTrough.SolOut"
SolCallback(2)="SolAutofire"
SolCallback(3)="bsLVUK.SolOut"
SolCallback(4)="bsRVUK.SolOut"
SolCallback(5)="bsSaucer.SolOut"
'SolCallback(6)="SolPostLeft" 'UK Only
'8 European Token Dispenser
'SolCallback(9)="vpmSolSound ""Jet3"","
'SolCallback(10)="vpmSolSound ""Jet3"","
'SolCallback(11)="vpmSolSound ""Jet3"","
'SolCallback(12)="vpmSolSound ""lsling"","
'SolCallback(13)="vpmSolSound ""lsling"","
SolCallback(14)="SolPost"
SolCallback(17)="SolRacoonLeft"
SolCallback(18)="SolRacoonRight"
SolCallback(20)="SolRampDiv"
SolCallback(21)="SolOrbitDiv"
'SolCallback(22)="SolPostRight"  'UK only
SolCallback(23)="SetLamp 127,"
'24 Coin Meter
SolCallback(25)="SetLamp 124,"   'F1 X2
SolCallback(26)="SetLamp 123,"   'F2 X2
SolCallback(27)="SetLamp 122,"   'F3 X2
SolCallback(28)="FlashRed10"   'F4 X3
SolCallback(29)="FlashBlue2"    'F5 X3
SolCallback(30)="SetLamp 129,"   'F6 X2
SolCallback(32)="SetLamp 128,"   'F8 X3


SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"


Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("flipperup",DOFFlippers):LeftFlipper.RotateToEnd
        LF.FIRE
     Else
         PlaySound SoundFX("flipperdown",DOFFlippers):LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("flipperup",DOFFlippers):RightFlipper.RotateToEnd
        RF.FIRE
     Else
         PlaySound SoundFX("flipperdown",DOFFlippers):RightFlipper.RotateToStart
     End If
End Sub 

'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
End Sub

  Sub SolPost(Enabled)
	If Enabled Then
		CenterPost.IsDropped=0
		CenterPostPrim.transZ = 23
		L56.BulbHaloHeight = 22
		Playsound SoundFX("Centerpost_Up",DOFContactors)
	Else
		CenterPost.IsDropped=1
		CenterPostPrim.transZ = 0
		L56.BulbHaloHeight = 1
		Playsound SoundFX("Centerpost_Down",DOFContactors)
	End If
End Sub

' Sub SolPostLeft(Enabled)  'UK Only
'	If Enabled Then
'		LeftPost.IsDropped=0
'		Playsound SoundFX("Centerpost_Up",DOFContactors)
'	Else
'		LeftPost.IsDropped=1
'		Playsound SoundFX("Centerpost_Down",DOFContactors)
'	End If
'End Sub

' Sub SolPostRight(Enabled)  'UK Only
'	If Enabled Then
'		RightPost.IsDropped=0
'		Playsound SoundFX("Centerpost_Up",DOFContactors)
'	Else
'		RightPost.IsDropped=1
'		Playsound SoundFX("Centerpost_Down",DOFContactors)
'	End If
'End Sub


Sub SolRacoonLeft(Enabled)
    If Enabled Then
        RacoonLeft.TransZ = 20
    PlaySoundAt "Racoon", RacoonLeft
    Else
        RacoonLeft.TransZ = 0
    PlaySoundAt "Racoon", RacoonLeft
    End If
End Sub



Sub SolRacoonRight(Enabled)
    If Enabled Then
        RacoonRight.TransZ = 20
    PlaySoundAt "Racoon", RacoonRight
    Else
        RacoonRight.TransZ = 0
    PlaySoundAt "Racoon", RacoonRight
    End If
End Sub

 Sub SolRampDiv(Enabled)
	If Enabled Then
 		LeftRampDiverter.IsDropped=0
         PlaySound SoundFX("flipperup",DOFContactors)
	Else
		LeftRampDiverter.IsDropped=1
        PlaySound SoundFX("flipperdown",DOFContactors)
	End If
End Sub

 Sub SolOrbitDiv(Enabled)
	If Enabled Then
 		OrbitDiverter.IsDropped=0
        PlaySound SoundFX("flipperup",DOFContactors) 
	Else
		OrbitDiverter.IsDropped=1
        PlaySound SoundFX("flipperdown",DOFContactors)
	End If
End Sub


'**********************************************************************************************************
'Stern-Sega GI  modified by AMaraklov to allow for UV GI functionality based on GI code used in IronMaidenVirtualTime
'**********************************************************************************************************
Dim isGIOn

'Utiliti to Flip flop things
Function IIF(bool, obj1, obj2)
	If bool Then
		IIF = obj1
	Else
		IIF = obj2
	End If
End Function

' Setup Gi lights to be initially on then let the fade code take over
Sub InitGI(startUp)	
	If startUp Then
		isGIOn = True
		UpdateGI 0,True
	End If
	Dim coll, obj

	'init GIInverse
	For Each obj In GIInverse
		With obj
			.IntensityScale 	= IIF(isGIOn,0,1)
			.State				= LightStateOn			
			'.Color			= WhiteOverhead
			'.ColorFull 		= WhiteOverheadFull
			'.Intensity 		= WhiteOverheadI
		End With
	Next

	
	' init GI overhead
	For Each obj In Array(GIOverhead)
		With obj
			.IntensityScale 	= IIF(isGIOn,1,0)
			.State				= LightStateOn			
			'.Color			= WhiteOverhead
			'.ColorFull 		= WhiteOverheadFull
			'.Intensity 		= WhiteOverheadI
		End With
	Next
	' init GI bulbs
	For Each obj In GIBulbs
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn		
		'obj.Color		= WhiteBulbs
		'obj.ColorFull 	= WhiteBulbsFull
		'obj.Intensity 	= WhiteBulbsI * 1		
	Next		
	' init GI lights
	For Each obj in GI
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn		
		'obj.Color		= White
		'obj.ColorFull	= WhiteFull
		'obj.Intensity 	= WhiteI * 1		
	Next
	For Each obj In GIPlastics
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn		
		'obj.Color		= White
		'obj.ColorFull	= WhiteFull
		'obj.Intensity 	= WhitePlasticI * 1 * IIF(obj.TimerInterval=-2,0.5,1)		
	Next			
	
End Sub





Dim GIDir : GIDir = 0
Dim GIStep : GIStep = 0
'Dim GIMode : GIMode = 0 '( 0 = GiOff, 1=GiOn, 2 = GiUV

set GICallback = GetRef("UpdateGI")
Sub UpdateGI(no, Enabled)
	If Enabled = 0 And Not isGIOn Then Exit Sub 'Terminate
	
	If Enabled Then		
		ChooseBall 			= 10 'Non Glow Ball
		ChangeBall(ChooseBall)
		GIDir = 1 : GITimer_Timer
		PlaySound "fx_relay"
	Else
		ChooseBall 			= 6 'Glow Ball
		ChangeBall(ChooseBall)
		GIDir = -1 : GITimer_Timer
		PlaySound "fx_relay"
	End If
End Sub

' Gi Timer for fading up and down lights / Changes in GI
Sub GITimer_Timer()
If Not GITimer.Enabled Then GITimer.Enabled = True

' Erhöhe / verringere GIStep je nach GIDir
GIStep = GIStep + GIDir

' 1) GIStep begrenzen (nur 0 bis 2)
If GIStep < 0 Then GIStep = 0
If GIStep > 2 Then GIStep = 2

Debug.Print GIStep

' set opacity of the shadow overlays (angepasst auf max. 2 statt 4! Das war ja viel zu viel..)
SetShadowOpacityAndGIOverhead
SetGIIllumination
SetGIIlluminationInv
Set3DModels
SetMaterials
SetRamps

' GI on/off nur in Schritten 0–2 => Timer stoppen wenn am Ende
If (GIDir = 1 And GIStep = 2) Or (GIDir = -1 And GIStep = 0) Then
GITimer.Enabled = False
End If
End Sub

Sub SetShadowOpacityAndGIOverhead()
    fGIOn.Opacity        = (ShadowOpacityGIOn / 2) * GIStep
    fGIOn.IntensityScale = (GIStep / 2)

    fGIUV.Opacity        = (ShadowOpacityUVOn / 2) * (2 - GIStep)
    fGIUV.IntensityScale = (2 - GIStep)

    GIOverhead.IntensityScale = GIStep / 2
End Sub

Sub SetGIIllumination()
	' set GI illumination
	Dim coll, obj
	For Each coll In Array(GI,GIPlastics)',GIBulbs)
		For Each obj In coll
			If obj.TimerInterval <> -1 Then obj.IntensityScale = GIStep/4
		Next
	Next
End Sub
Sub SetGIIlluminationInv() 'Inverse Illumination
	' set GI illumination
	Dim obj	
		For Each obj In GIInverse
			If obj.TimerInterval <> -1 Then obj.IntensityScale = abs(1-(GIStep/4))
		Next	
End Sub



Sub SetMaterials()
	UseGIMaterial GIPlastics, "Plastic with opaque image", GIStep
	'UseGIMaterial GIRubbers, "Rubber White", GIStep
	'UseGIMaterial GIWireTrigger, "Metal0.8", GIStep
	'UseGIMaterial GILocknuts, "Metal Chrome S34", GIStep
	'UseGIMaterial GIScrews, "Metal0.8", GIStep
	'UseGIMaterial GIPlasticScrews, "Metal0.8", GIStep
	'UseGIMaterial Array(pMetalWalls), "Metal Light", GIStep
	'UseGIMaterial GIMetalChromes, "Metal Chrome S34", GIStep
	'UseGIMaterial GIMetalPrims, "Metal S34", GIStep
	'UseGIMaterial GIYellowPosts, "Plastic White", GIStep
	'UseGIMaterial GIBlackPosts, "Plastic White", GIStep
	'UseGIMaterial GIRedTargets, "Plastic with opaque image", GIStep
	'UseGIMaterial GIDropTargets, "Plastic with opaque image", GIStep
	'UseGIMaterial GIYellowTargets, "Plastic with opaque image", GIStep
	'UseGIMaterial GIBlueTargets, "Plastic with opaque image", GIStep
	'UseGIMaterial GITargetsParts, "Metal S34", GIStep
	'UseGIMaterial GIRedPlasticPegs, "TransparentPlasticRed", GIStep
	'UseGIMaterial GIYellowPlasticPegs, "TransparentPlasticYellow", GIStep
	'UseGIMaterial GIBluePlasticPegs, "TransparentPlasticBlue", GIStep
	'UseGIMaterial GIGates, "Metal Wires", GIStep
	'UseGIMaterial4Bumper GIBumpers, "Plastic White", GIStep
	'UseGIMaterial GIBulbGlasses, "Lamps Glass", GIStep
	'UseGIMaterial Array(pSpinner27,pSpinner51), "Plastic with an image V", GIStep
	'UseGIMaterial Array(pToplaneW,pToplaneWA,pToplaneAR,pToplaneR), "TransparentPlasticYellow", GIStep
	'UseGIMaterial Array(pLevelPlate,pLevelPlate2), "Metal0.8", GIStep
	'UseGIMaterial Array(pLeftFlipper,pRightFlipper), "Plastic with an image", GIStep
	'UseGIMaterial Array(pApron,rApronPlunger), "Apron", GIStep
	'UseGIMaterial Array(Plunger), "Plunger", GIStep
	'UseGIMaterial Array(rSaucer38StopOnTop), "Metal Wires", GIStep
	'UseGIMaterial Array(pBackboard), "Plastics Light", GIStep
End Sub
Sub TurnInverseOnOff(OnOff)
	Dim obj
	For Each obj In GIInverse
		With obj
			.IntensityScale 	= IIF(OnOff,1,0)
			.State				= IIF(OnOff,1,0)		
			'.Color			= WhiteOverhead
			'.ColorFull 		= WhiteOverheadFull
			'.Intensity 		= WhiteOverheadI
		End With
	Next
End  Sub
Sub Set3DModels()	' 3D Model Texture Change based on Lighting State
	If GIStep < 2 Then
		RacoonLeft.image = "RacoonPink_UV_D"
		RacoonRight.image = "RacoonGrey_UV_D"
		TurnInverseOnOff(True)
	Else
		RacoonLeft.image = "RacoonPink_Reg_D"
		RacoonRight.image = "RacoonGrey_Reg_D"
		TurnInverseOnOff(False)
	End If
End Sub

Sub SetRamps()
	' Not Yet Implemented	
End Sub

' Functions for dynamic Material manipulation
Sub UseGIMaterial(coll, matName, currentGIStep)
	Dim obj, mat
	mat = CreateMaterialName(matName, currentGIStep)
	For Each obj In coll : obj.Material = mat : Next
End Sub
Sub UseGIMaterial4Bumper(coll, matName, currentGIStep)
	Dim obj, mat
	mat = CreateMaterialName(matName, currentGIStep)
	For Each obj In coll : obj.SkirtMaterial = mat : Next
End Sub
Function CreateMaterialName(matName, currentGIStep)
	CreateMaterialName = matName & IIF(currentGIStep=0, " Dark", IIF(currentGIStep<4, " Dark" & currentGIStep,""))
End Function


'Car Lights is technically not part of GI but because it also deals with GI I'm putting it here

'Dim CarLightsDirL,CarLightsDirR : CarLightsDirL = 0 : CarLightsDirR = 0
'Dim CarLightsStep : CarLightsStep = 0


'Sub TriggerCarLights( objList, directionL, directionR )
'	Dim obj
'	CarLightsDirL = directionL
'	CarLightsDirR = directionR
'	CarLights_Timer
'	For Each obj In objList 
'		If obj.TimerInterval <> -1 Then 
'			If CarLightsStep = 4 Then obj.State = 0
'			else obj.State = 1
'			obj.IntensityScale = (CarLightsStep/4)
'		End If
'	Next
'End Sub


'Sub CarLights_Timer()
'	If Not CarLights.Enabled Then CarLights.Enabled = True
'	CarLightsStep = CarLightsStep + CarLightsDir
'
'	If (CarLightsDir = 1 And CarLightsStep = 4) Or (CarLightsDir = -1 And CarLightsStep = 0) Then
'		CarLightsStep = CarLightsStep + CarLightsDir
'		CarLights.Enabled = False
'	End If
'End Sub


'**********************************************************************************************************
'End GI
'**********************************************************************************************************


'****************************************** Debug test code to see if effect is working ******************
'Sub fGIOff001_Timer()
'	If fGIOff001.visible Then	
'		fGIOff001.visible = False
'		fGIOff002.visible = True
'		fGIOff001.IntensityScale = 0
'		fGIOff002.IntensityScale = 4
'	Else
'		fGIOff001.visible = True
'		fGIOff002.visible = False
'		fGIOff001.IntensityScale = 5
'		fGIOff002.IntensityScale = 0
'	End If
'End Sub
'**********************************************************************************************************



' *********************************************************************
' colors
' *********************************************************************
Dim White, WhiteFull, WhiteI, WhiteP, WhitePlastic, WhitePlasticFull, WhitePlasticI, WhiteBumper, WhiteBumperFull, WhiteBumperI, WhiteBulbs, WhiteBulbsFull, WhiteBulbsI, WhiteOverheadFull, WhiteOverhead, WhiteOverheadI
WhiteFull = rgb(255,255,255) 
White = rgb(255,196,64) 'rgb(255,255,180)
WhiteI = 20
WhitePlasticFull = rgb(255,255,180) 
WhitePlastic = rgb(255,255,180)
WhitePlasticI = 25
WhiteBumperFull = rgb(255,255,180) 
WhiteBumper = rgb(255,255,180)
WhiteBumperI = 25
WhiteBulbsFull = rgb(255,255,180)
WhiteBulbs = rgb(255,255,180)
WhiteBulbsI = 10 * ShadowOpacityGIOff
WhiteOverheadFull = rgb(255,255,180)
WhiteOverhead = rgb(255,255,180)
WhiteOverheadI = .15




'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************


Dim bsTrough,bsSaucer,bsLVUK,bsRVUK

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Sega Viper Night Drivin'"&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 0
        .Games(cGameName).Settings.Value("sound")=1
		'.PuPHide = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled= true

Glowball_Init

	vpmNudge.TiltSwitch=56
    vpmNudge.Sensitivity=5
    vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingShot,RightSlingShot)

	Set bsTrough=New cvpmBallStack
	bsTrough.InitSw 0,15,14,13,12,0,0,0
	bsTrough.InitKick BallRelease,45,5
	bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
	bsTrough.Balls=4

    Set bsSaucer=New cvpmBallStack
	bsSaucer.InitSaucer sw44,44,225,6
	bsSaucer.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

	Set bsLVUK=New cvpmBallStack
	bsLVUK.InitSaucer sw45,45,205,7
	bsLVUK.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

	Set bsRVUK=New cvpmBallStack
	bsRVUK.InitSaucer sw46,46,160,8
	bsRVUK.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

 	LeftPost.IsDropped=1:RightPost.IsDropped=1
    CenterPost.IsDropped=1
    LeftRampDiverter.IsDropped=1
    RacoonLeft.TransZ = 20
    RacoonRight.TransZ = 20

	InitGI(True)

End Sub

'**********************************************************
' Small shake of Racoons when nudging
'**********************************************************

Dim SmallShake:SmallShake = 0

Sub aSaucerShake
    SmallShake = 6
    SaucerShake.Enabled = True
End Sub

Sub SaucerShake_Timer
    RacoonLeft.Transz = SmallShake / 2
    RacoonRight.Transz = SmallShake / 2
    If SmallShake = 0 Then SaucerShake.Enabled = False:Exit Sub
    If SmallShake < 0 Then
        SmallShake = ABS(SmallShake) - 0.1
    Else
        SmallShake = - SmallShake + 0.1
    End If
End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************


Sub Table1_KeyDown(ByVal KeyCode)

	If keycode = LeftFlipperKey Then 
	    lfpress = 1
	end if
	If keycode = RightFlipperKey Then 
	    rfpress = 1
	end if

	If keycode = LeftMagnaSave Then bLutActive = True
	If keycode = RightMagnaSave Then 
		If bLutActive Then NextLUT: End If
    End If
'	If KeyCode=LeftMagnaSave Then Controller.Switch(20)=1   'UK Only
'	If KeyCode=RightMagnaSave Then Controller.Switch(21)=1  'UK Only
	If KeyCode=PlungerKey Then Controller.Switch(53)=1
    If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound SoundFX("fx_nudge",0), 0, 1, -0.1, 0.25:aSaucerShake
    If keycode = RightTiltKey Then Nudge 270, 6:PlaySound SoundFX("fx_nudge",0), 0, 1, 0.1, 0.25:aSaucerShake
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge",0), 0, 1, 0, 0.25:aSaucerShake
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)

If keycode = LeftFlipperKey Then 
		lfpress = 0
		leftflipper.eostorqueangle = EOSA
		leftflipper.eostorque = EOST
	End If
	If keycode = RightFlipperKey Then 
		rfpress = 0
		rightflipper.eostorqueangle = EOSA
		rightflipper.eostorque = EOST
	End If

    If keycode = LeftMagnaSave Then bLutActive = False
'	If KeyCode=LeftMagnaSave Then Controller.Switch(20)=0   'UK Only
'	If KeyCode=RightMagnaSave Then Controller.Switch(21)=0  'UK Only
	If KeyCode=PlungerKey Then Controller.Switch(53)=0
	If KeyUpHandler(keycode) Then Exit Sub
End Sub

Dim plungerIM
    ' Impulse Plunger
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, 55, 0.6
        .Random 0.3
        .switch 16
        .InitExitSnd SoundFX("fx_AutoPlunger",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .CreateEvents "plungerIM"
    End With
    
    plungerIM.Strength = 40
    

'**********************************************************************************************************
' Drain hole and kickers
'**********************************************************************************************************

Sub Drain_Hit:bsTrough.AddBall Me :playsound"drain" : End Sub
Sub sw44_Hit:bsSaucer.AddBall 0: playsound "popper_ball": End Sub	
Sub sw45_Hit:bsLVUK.AddBall 0:playsound "popper_ball": End Sub		
Sub sw46_Hit:bsRVUK.AddBall 0:playsound "popper_ball": End Sub		
		

'*******Ramp gates*******

Sub sw18_hit:PlaySound "Gate5":Controller.Switch(18)=1:End Sub
Sub sw19_hit:PlaySound "Gate5":Controller.Switch(19)=1:End Sub
Sub sw25_hit:PlaySound "Gate5":Controller.Switch(25)=1:End Sub
Sub sw26_hit:PlaySound "Gate5":Controller.Switch(26)=1:End Sub
Sub sw27_hit:PlaySound "Gate5":Controller.Switch(27)=1:End Sub
Sub sw27_unhit:Controller.Switch(27)=0:End Sub
Sub sw28_hit:PlaySound "Gate5":Controller.Switch(28)=1:End Sub



'Optical Trigger

Sub sw17_hit:Controller.Switch(17)=1:End Sub   'Top Jump Metal Ramp
Sub sw17_unhit:Controller.Switch(17)=0:End Sub

'******Rollover Switches*********

Sub SW16_Hit:: playsound"rollover" : End Sub ' Coded to impulse plunger
'Sub SW16_unHit:Controller.Switch(13)=0:End Sub
Sub sw41_hit:Controller.Switch(41)=1:Playsound "rollover":End Sub
Sub sw41_unhit:Controller.Switch(41)=0:End Sub
Sub sw42_hit:Controller.Switch(42)=1:Playsound "rollover":End Sub
Sub sw42_unhit:Controller.Switch(42)=0:End Sub
Sub sw47_hit:Controller.Switch(47)=1:Playsound "rollover":End Sub
Sub sw47_unhit:Controller.Switch(47)=0:End Sub
Sub sw48_hit:Controller.Switch(48)=1:Playsound "rollover":End Sub
Sub sw48_unhit:Controller.Switch(48)=0:End Sub
Sub sw57_hit:Controller.Switch(57)=1:Playsound "rollover":End Sub
Sub sw57_unhit:Controller.Switch(57)=0:End Sub
Sub sw58_hit:Controller.Switch(58)=1:Playsound "rollover":End Sub
Sub sw58_unhit:Controller.Switch(58)=0:End Sub
Sub sw60_hit:Controller.Switch(60)=1:Playsound "rollover":End Sub
Sub sw60_unhit:Controller.Switch(60)=0:End Sub
Sub sw61_hit:Controller.Switch(61)=1:Playsound "rollover":End Sub
Sub sw61_unhit:Controller.Switch(61)=0:End Sub


'*********Stand Up Targets*******

Sub sw29_hit:vpmTimer.pulseSw 29 : Playsound "target":End Sub 
Sub sw30_hit:vpmTimer.pulseSw 30 : Playsound "target":End Sub
Sub sw31_hit:vpmTimer.pulseSw 31 : Playsound "target":End Sub
Sub sw32_hit:vpmTimer.pulseSw 32 : Playsound "target":End Sub

'*********Scoring Rubbers*******

Sub sw22_hit:vpmTimer.pulseSw 22 : End Sub 
Sub sw23_hit:vpmTimer.pulseSw 23 : End Sub 

'*********Bumper switches*******

Sub Bumper1_Hit:vpmTimer.pulseSW 49 : PlayBumperSound : End Sub
Sub Bumper2_Hit:vpmTimer.pulseSW 50 : PlayBumperSound : End Sub
Sub Bumper3_Hit:vpmTimer.pulseSW 51 : PlayBumperSound : End Sub


Sub PlayBumperSound()
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySound SoundFX("fx_bumper1",DOFContactors)
			Case 2 : PlaySound SoundFX("fx_bumper2",DOFContactors)
			Case 3 : PlaySound SoundFX("fx_bumper3",DOFContactors)
		End Select
End Sub


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 62
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0    
'	gi1.State = 0:Gi2.State = 0
	'TriggerCarLights Array(GI_R_Headlight1,GI_R_Headlight2,GI_R_TailLight1,GI_R_TailLight2), 1
	
	Dim InvertState
	InvertState = IIF(GI_R_Headlight1.State,0,1)

	If (GIDir = 1) Then
		GI_R_Headlight1.State = InvertState : GI_R_Headlight1.IntensityScale = InvertState
		GI_R_Headlight2.State = InvertState : GI_R_Headlight2.IntensityScale = InvertState
		GI_R_TailLight1.State = InvertState : GI_R_TailLight1.IntensityScale = InvertState
		GI_R_TailLight2.State = InvertState : GI_R_TailLight2.IntensityScale = InvertState
	End If
	RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
	'TriggerCarLights Array(GI_R_Headlight1,GI_R_Headlight2,GI_R_TailLight1,GI_R_TailLight2), -1
	
	Dim InvertState
	InvertState = IIF(GI_R_Headlight1.State,0,1)
	
	If (GIDir = 1) Then
		GI_R_Headlight1.State = InvertState : GI_R_Headlight1.IntensityScale = InvertState
		GI_R_Headlight2.State = InvertState : GI_R_Headlight2.IntensityScale = InvertState
		GI_R_TailLight1.State = InvertState : GI_R_TailLight1.IntensityScale = InvertState
		GI_R_TailLight2.State = InvertState : GI_R_TailLight2.IntensityScale = InvertState
	End If
	RightSlingShot.TimerEnabled = 0
End Sub


Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 59
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
'	gi3.State = 0:Gi4.State = 0
	'TriggerCarLights Array(GI_L_Headlight1,GI_L_Headlight2,GI_L_TailLight1,GI_L_TailLight2), 1

	Dim InvertState : InvertState = 0
	InvertState = IIF(GI_L_TailLight1.State,0,1)
	'InvertState = Abs(Not GI_L_TailLight1.State)

	If (GIDir = 1) Then
		GI_L_Headlight1.State = InvertState : GI_L_Headlight1.IntensityScale = InvertState
		GI_L_Headlight2.State = InvertState : GI_L_Headlight2.IntensityScale = InvertState
		GI_L_TailLight1.State = InvertState : GI_L_TailLight1.IntensityScale = InvertState
		GI_L_TailLight2.State = InvertState : GI_L_TailLight2.IntensityScale = InvertState
	End If
End Sub

Sub LeftSlingShot_Timer	
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
	'TriggerCarLights Array(GI_L_Headlight1,GI_L_Headlight2,GI_L_TailLight1,GI_L_TailLight2), -1

	Dim InvertState : InvertState = 0
	InvertState = IIF(GI_L_Headlight1.State,0,1)
	'InvertState = Abs(Not GI_L_Headlight1.State)
	
	If (GIDir = 1) Then
		GI_L_Headlight1.State = InvertState : GI_L_Headlight1.IntensityScale = InvertState
		GI_L_Headlight2.State = InvertState : GI_L_Headlight2.IntensityScale = InvertState
		GI_L_TailLight1.State = InvertState : GI_L_TailLight1.IntensityScale = InvertState
		GI_L_TailLight2.State = InvertState : GI_L_TailLight2.IntensityScale = InvertState
	End If
    LeftSlingShot.TimerEnabled = 0
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
LampTimer.Interval = 5 'lamp fading speed
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

Sub UpdateLamps()

 
    NFadeL 1, l1
    NFadeL 2, l2
    NFadeL 3, l3
    NFadeL 4, l4
    NFadeL 5, l5
    NFadeL 6, l6
    NFadeL 7, L7
    NFadeL 8, l8
    NFadeL 9, l9
    NFadeL 10, l10
    NFadeL 11, L11
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
    NFadeL 41, l41
    NFadeL 42, l42
    NFadeL 43, l43
    NFadeL 44, l44
    NFadeL 45, l45
    NFadeL 46, l46
    NFadeL 47, l47
    NFadeL 48, l48
    NFadeL 49, l49
    NFadeL 50, l50
    NFadeL 51, l51
    NFadeL 52, l52
    NFadeL 53, l53
    NFadeL 54, l54
    NFadeL 55, l55
    NFadeL 56, l56  'Drain Post
    NFadeL 57, l57
    NFadeL 58, l58
    NFadeL 59, l59
    NFadeL 60, l60
    NFadeL 61, l61
    NFadeL 62, l62
    NFadeL 63, l63
    NFadeL 64, l64
    NFadeLm 65, l65  'Bumper 1
    NFadeL 65, l65a  'Bumper 1
    NFadeLm 66, l66  'Bumper 2
    NFadeL 66, l66a  'Bumper 2
    NFadeLm 67, l67  'Bumper 3
    NFadeL 67, l67a  'Bumper 3
    NFadeL 68, L68
    NFadeL 69, l69

'Solenid Controlled Lamps

NFadeLm 122, S122
NFadeL 122, S122a

NFadeLm 123, S123
NFadeL 123, S123a

NFadeLm 124, S124
NFadeL 124, S124a

'NFadeLm 125, S125a
'NFadeLm 125, S125b
'NFadeL 125, S125c

'NFadeLm 126, S126a
'NFadeLm 126, S126b
'NFadeL 126, S126c

NFadeLm 127, S127a
NFadeLm 127, S127b
NFadeL 127, S127c

NFadeLm 128, S128
NFadeL 128, S128A

NFadeLm 129, S129
NFadeL 129, S129A

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

' *********************************************************************
'Fluppers Domes  V2
' *********************************************************************

Sub FlashRed10(flstate) 'Red Domes
  If Flstate Then
    Objlevel(10) = 1 : FlasherFlash10_Timer
    Objlevel(11) = 1 : FlasherFlash11_Timer
    Objlevel(13) = 1 : FlasherFlash13_Timer
  End If
End Sub


Sub FlashBlue2(flstate) 'Blue Domes
  If Flstate Then
    Objlevel(2) = 1 : FlasherFlash2_Timer
    Objlevel(4) = 1 : FlasherFlash4_Timer
    Objlevel(6) = 1 : FlasherFlash6_Timer
  End If
End Sub


Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness
 
								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************
 
 
Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 2, "blue"
InitFlasher 4, "blue"
InitFlasher 6, "blue"
'InitFlasher 9, "white"
InitFlasher 10, "red"
InitFlasher 11, "red"
InitFlasher 13, "red"
'InitFlasher 16, "yellow"
'InitFlasher 17, "yellow"
'InitFlasher 18, "yellow"
 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 11,90
 
Sub InitFlasher(nr, col)
  ' store all objects in an array for use in FlashFlasher subroutine
  Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
  Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
  ' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
  If objbase(nr).RotY = 0 Then
    objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
    objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
  End If
  ' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
  objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
  objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
  objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
  objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
  objbase(nr).BlendDisableLighting = FlasherOffBrightness
  ' set the texture and color of all objects
  select case objbase(nr).image
    Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col :
    Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
    Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
  end select
  If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
    select case col
      Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
      Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
      Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
      Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255)
      Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
      Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
    end select
  objlight(nr).colorfull = objlight(nr).color
  If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then
    objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
    ObjFlasher(nr).y = ObjFlasher(nr).y + 10
  End If
End Sub
 
Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub
 
Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub
 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub
'Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub
Sub FlasherFlash13_Timer() : FlashFlasher(13) : End Sub
'Sub FlasherFlash16_Timer() : FlashFlasher(16) : End Sub
'Sub FlasherFlash17_Timer() : FlashFlasher(17) : End Sub
'Sub FlasherFlash18_Timer() : FlashFlasher(18) : End Sub


'*************
'   JP'S LUT
'*************

Dim bLutActive, LUTImage
Sub LoadLUT
Dim x
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 10: UpdateLUT: SaveLUT: End Sub

Sub UpdateLUT
Select Case LutImage
Case 0: table1.ColorGradeImage = "LUT0"
Case 1: table1.ColorGradeImage = "LUT1"
Case 2: table1.ColorGradeImage = "LUT2"
Case 3: table1.ColorGradeImage = "LUT3"
Case 4: table1.ColorGradeImage = "LUT4"
Case 5: table1.ColorGradeImage = "LUT5"
Case 6: table1.ColorGradeImage = "LUT6"
Case 7: table1.ColorGradeImage = "LUT7"
Case 8: table1.ColorGradeImage = "LUT8"
Case 9: table1.ColorGradeImage = "LUT9"

End Select
End Sub

'*********** Glowball Section ***************
Dim GlowBall, CustomBulbIntensity(11)
Dim  GBred(11)
Dim GBgreen(11), GBblue(11)
Dim CustomBallImage(11), CustomBallLogoMode(11), CustomBallDecal(11), CustomBallGlow(11)


' default Ball
CustomBallGlow(0) = 		False
CustomBallImage(0) = 		"TTMMball"
CustomBallLogoMode(0) = 	False
CustomBallDecal(0) = 		"scratches"
CustomBulbIntensity(0) = 	0.01
GBred(0) = 0 : GBgreen(0)	= 0 : GBblue(0) = 0

' white GlowBall
CustomBallGlow(1) = 		True
CustomBallImage(1) = 		"white"
CustomBallLogoMode(1) = 	True
CustomBallDecal(1) = 		""
CustomBulbIntensity(1) = 	0
GBred(1) = 255 : GBgreen(1)	= 255 : GBblue(1) = 255

' Magma GlowBall
CustomBallGlow(2) = 		True
CustomBallImage(2) = 		"ballblack"
CustomBallLogoMode(2) = 	True
CustomBallDecal(2) = 		"magma6"
CustomBulbIntensity(2) = 	0
GBred(2) = 255 : GBgreen(2)	= 20 : GBblue(2) = 20

' Blue ball
CustomBallGlow(3) = 		True
CustomBallImage(3) = 		"blueball2"
CustomBallLogoMode(3) = 	False
CustomBallDecal(3) = 		""
CustomBulbIntensity(3) = 	0
GBred(3) = 30 : GBgreen(3)	= 40 : GBblue(3) = 200

' HDR ball
CustomBallGlow(4) = 		False
CustomBallImage(4) = 		"ball_HDR"
CustomBallLogoMode(4) = 	False
CustomBallDecal(4) = 		"JPBall-Scratches"
CustomBulbIntensity(4) = 	0.01
GBred(4) = 0 : GBgreen(4)	= 0 : GBblue(4) = 0

' Earth
CustomBallGlow(5) = 		True
CustomBallImage(5) = 		"ballblack"
CustomBallLogoMode(5) = 	True
CustomBallDecal(5) = 		"earth"
CustomBulbIntensity(5) = 	0
GBred(5) = 100 : GBgreen(5)	= 100 : GBblue(5) = 100

' green GlowBall
CustomBallGlow(6) = 		True
CustomBallImage(6) = 		"glowball green"
CustomBallLogoMode(6) = 	True
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
GBred(6) = 100 : GBgreen(6)	= 255 : GBblue(6) = 100

' blue GlowBall
CustomBallGlow(7) = 		True
CustomBallImage(7) = 		"glowball blue"
CustomBallLogoMode(7) = 	True
CustomBallDecal(7) = 		""
CustomBulbIntensity(7) = 	0
GBred(7) = 50 : GBgreen(7)	= 50 : GBblue(7) = 255
'GBred(7) = 100 : GBgreen(7)	= 100 : GBblue(7) = 255

' red GlowBall
CustomBallGlow(8) = 		True
CustomBallImage(8) = 		"glowball orange"
CustomBallLogoMode(8) = 	True
CustomBallDecal(8) = 		""
CustomBulbIntensity(8) = 	0
GBred(8) = 255 : GBgreen(8)	= 0 : GBblue(8) = 000
'GBred(8) = 255 : GBgreen(8)	= 255 : GBblue(8) = 100  'orange

' shiny Ball
CustomBallGlow(9) = 		False
CustomBallImage(9) = 		"pinball3"
CustomBallLogoMode(9) = 	False
CustomBallDecal(9) = 		"JPBall-Scratches"
CustomBulbIntensity(9) = 	0.01
GBred(9) = 0 : GBgreen(9)	= 0 : GBblue(9) = 0

'green non glow ball
CustomBallGlow(10) = 		True
CustomBallImage(10) = 		"glowball green"
CustomBallLogoMode(10) = 	True
CustomBallDecal(10) = 		""
CustomBulbIntensity(10) = 	0
GBred(10) = 0 : GBgreen(10)	= 20 : GBblue(10) = 0


' *** prepare the variable with references to three lights for glow ball ***
Dim Glowing(11)
Set Glowing(0) = Glowball1 : Set Glowing(1) = Glowball2 : Set Glowing(2) = Glowball3 : Set Glowing(3) = Glowball4


'*** change ball appearance ***

Sub ChangeBall(ballnr)
	Dim BOT, ii, col
	table1.BallDecalMode = CustomBallLogoMode(ballnr)
	table1.BallFrontDecal = CustomBallDecal(ballnr)
	table1.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
	table1.BallImage = CustomBallImage(ballnr)
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 3
		col = RGB(GBred(ballnr), GBgreen(ballnr), GBblue(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
	Next
End Sub



' *** Ball Shadow code / Glow Ball code / Primitive Flipper Update ***

Dim BallShadowArray
BallShadowArray = Array (BallShadow1, BallShadow2, BallShadow3)
Const anglecompensate = 15

Sub GraphicsTimer_Timer()
	Dim BOT, b
    BOT = GetBalls

	' switch off glowlight for removed Balls
	IF GlowBall Then
		For b = UBound(BOT) + 1 to 3
			If GlowBall and Glowing(b).state = 1 Then Glowing(b).state = 0 End If
		Next
	End If

    For b = 0 to UBound(BOT)
		' *** move ball shadow for max 3 balls ***
'		If BallShadow and b < 3 Then
'			If BOT(b).X < table1.Width/2 Then
'				BallShadowArray(b).X = ((BOT(b).X) - (50/6) + ((BOT(b).X - (table1.Width/2))/7)) + 10
'			Else
'				BallShadowArray(b).X = ((BOT(b).X) + (50/6) + ((BOT(b).X - (table1.Width/2))/7)) - 10
'			End If
'			BallShadowArray(b).Y = BOT(b).Y + 20 : BallShadowArray(b).Z = 1
'			If BOT(b).Z > 20 Then BallShadowArray(b).visible = 1 Else BallShadowArray(b).visible = 0 End If
'		End If
		' *** move glowball light for max 3 balls ***
		If GlowBall and b < 4 Then
			If Glowing(b).state = 0 Then Glowing(b).state = 1 end if
			Glowing(b).BulbHaloHeight = BOT(b).z + 51
			Glowing(b).x = BOT(b).x : Glowing(b).y = BOT(b).y + anglecompensate
		End If
	Next
'	If ChooseBats = 1 Then
'		' *** move primitive bats ***
'		batleft.objrotz = LeftFlipper.CurrentAngle + 1
'		batleftshadow.objrotz = batleft.objrotz
'		batright.objrotz = RightFlipper.CurrentAngle - 1
'		batrightshadow.objrotz  = batright.objrotz 
'	Else
'		If ChooseBats > 1 Then
'			' *** move glowbats ***
'			GlowBatLightLeft.y = 1720 - 121 + LeftFlipper.CurrentAngle
'			glowbatleft.objrotz = LeftFlipper.CurrentAngle
'			GlowBatLightRight.y =1720 - 121 - RightFlipper.CurrentAngle
'			glowbatright.objrotz = RightFlipper.CurrentAngle
'		End If
'	End If
End Sub

Sub Glowball_Init
	ChangeBall(ChooseBall)
	If GlowBall Then GraphicsTimer.enabled = True End If
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
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function


'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************

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
        StopSound "fx_ballrolling" & b
        StopSound "fx_plasticrolling" & b
        StopSound "fx_metalrolling" & b
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
		If BallVel(BOT(b) ) > 1 Then
            rolling(b) = True
			If BOT(b).Z > 30 Then
				' ball on plastic ramp
				StopSound "fx_ballrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_plasticrolling" & b, -1, Vol(BOT(b)) / 2, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			Else
				' ball on playfield
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_ballrolling" & b, -1, Vol(BOT(b)) / 2, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			End If
		Else
			If rolling(b) Then
                StopSound "fx_ballrolling" & b
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
                rolling(b) = False
            End If
		End If
		
		'   ball drop sounds matching the adjusted height params but not the way down the ramps
		If BOT(b).VelZ < -1 And BOT(b).Z < 55 And BOT(b).Z > 27 then 'And Not InRect(BOT(b).X, BOT(b).Y, 610,320, 740,320, 740,550, 610,550) And Not InRect(BOT(b).X, BOT(b).Y, 180,400, 230,400, 230, 550, 180,550) Then
			PlaySound "fx_ball_drop" & Int(Rnd()*3), 0, ABS(BOT(b).VelZ)/17*5, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If

    Next
End Sub
'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	LogoSx.RotZ = LeftFlipper.CurrentAngle - 90
	LogoDx.RotZ = RightFlipper.CurrentAngle + 90

End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_timer()
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
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
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

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


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
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'                  Start nFOZZY FLIPPERS'
'******************************************************

'Flipper Correction Initialization late 80�s to early 90�s
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
        AddPt "Polarity", 1, 0.05, -5
        AddPt "Polarity", 2, 0.4, -5
        AddPt "Polarity", 3, 0.6, -4.5
        AddPt "Polarity", 4, 0.65, -4.0
        AddPt "Polarity", 5, 0.7, -3.5
        AddPt "Polarity", 6, 0.75, -3.0
        AddPt "Polarity", 7, 0.8, -2.5
        AddPt "Polarity", 8, 0.85, -2.0
        AddPt "Polarity", 9, 0.9,-1.5
        AddPt "Polarity", 10, 0.95, -1.0
        AddPt "Polarity", 11, 1, -0.5
        AddPt "Polarity", 12, 1.1, 0
        AddPt "Polarity", 13, 1.3, 0

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
'			FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
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

	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
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
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)						'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
	 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
	
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'playsound "fx_knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'		FLIPPER POLARITY AND RUBBER DAMPENER
'			SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
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
Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
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
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
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

' Used for drop targets and flipper tricks
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

'******************************************************
'			FLIPPER TRICKS
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

	If abs(Flipper1.currentangle) < abs(Endangle1) + 3 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
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
					'debug.print "flippernudge!!"
					BOT(b).velx = BOT(b).velx /1.5
					BOT(b).vely = BOT(b).vely - 1
				end If
			Next
		End If
	Else 
		If abs(Flipper1.currentangle) > abs(Endangle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************
Const Pi = 3.1415927

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
Const EOSTnew = 1 '0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 4.5
	Case 2:
		SOSRampup = 8.5
End Select
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.035 '0.025


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
	Dir = Flipper.startangle/Abs(Flipper.startangle)	'-1 for Right Flipper

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
Const LiveDistanceMax = 117  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce															'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.8 Then						'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)	'Partial catch when catch happens a bit late
		end If

		'debug.print "Live catch! Bounce: " & LiveCatchBounce

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (16 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	End If
End Sub

'*****************************************************************************************************
'END nFOZZY FLIPPERS'

Sub Table1_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub