Option Explicit

Sub Timer1_Timer()
	pLspinner.RotZ = -(LeftSpinner.currentangle)
	pRspinner.RotZ = -(RightSpinner.currentangle)
	G1.RotZ = -(Spinner1.currentangle)
	G2.RotZ = -(Spinner2.currentangle)
	G3.RotZ = -(Spinner3.currentangle)
	G4.RotZ = -(Spinner4.currentangle)
	G5.RotZ = -(gate.currentangle)
	G6.RotZ = -(Spinner5.currentangle)
	Ptest.RotX = arm.RotX
	BallUP.RotX = PTest.RotX
	BallDOWN.RotX = clawdown.RotX
End Sub

if Table1.showDT = False then ramp047.visible = False:ramp050.visible = False

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0


LoadVPM "01550000", "SEGA.vbs", 3.26
Const UseSolenoids = True
Const UseLamps     = False
Const UseSync 	   = True

' Standard Sounds used by Driver help code
Const SSolenoidOn   = "SolOn"
Const SSolenoidOff  = "SolOff"
Const SFlipperOn    = "FlipperUp"
Const SFlipperOff   = "FlipperDown"
Const SCoin			= "coin3"

SolCallback(1)="bsTrough.SolOut"
SolCallback(2)="bsLaunch.SolOut"
'SolCallback(3)="dtDrop.solDropUp"
SolCallback(4)="bsScoop.SolOut"
'SolCallback(5)=magnet
'SolCallback(6)=shaker motor
SolCallBack(7) ="solSmagnet" 'snagger up magnet
SolCallback(9)="vpmSolSound playsound(""jet3""),"
SolCallback(10)="vpmSolSound playsound(""jet3""),"
SolCallback(11)="vpmSolSound playsound(""jet3""),"
SolCallback(12)="vpmSolSound playsound(""Sling""),"
SolCallback(13)="vpmSolSound playsound(""Sling""),"
SolCallback(14)= "SolPFMagnet"
SolCallback(sLLFlipper)="SolFlipper LeftFlipper,Nothing,"
SolCallback(sLRFlipper)="SolFlipper RightFlipper,Nothing,"
'SolCallBack(17)="SetLamp 117,"		'Flash: In Egg
SolCallBack(17)="EggFlash"		'Flash: In Egg
SolCallback(18)="Solsnagger"
SolCallback(19)="Solsnaggerrelay"
SolCallBack(20) ="DTdrop.SolHit 1,"
'SolCallback(21)="SetLamp 121,"
SolCallback(21)="SolEggout"
SolCallBack(22)="SetLamp 122,"		'"L" Ramp
SolCallBack(23)="SetLamp 123," 	'POPs
SolCallBack(25)="SetLamp 101," 	'San Diego
SolCallBack(26)="SetLamp 102," 	'LWR RT
SolCallBack(27)="SetLamp 103," 	'MID RT
SolCallBack(28)="SetLamp 104," 	'TOP RT
SolCallBack(29)="SetLamp 105," 	'TOP LT
SolCallBack(30)="SetLamp 106," 	'MID LT
SolCallBack(31)="SetLamp 107," 	'LWR RT
SolCallBack(32)="SetLamp 108," 	'"

' Flipper Subs
  SolCallback(sLRFlipper) = "SolRFlipper"
  SolCallback(sLLFlipper) = "SolLFlipper"
 
  Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound playsound("FlipperUp"):LeftFlipper.RotateToEnd
     Else
         PlaySound playsound("FlipperDown"):LeftFlipper.RotateToStart
     End If
  End Sub
  
  Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound playsound("FlipperUp"):RightFlipper.RotateToEnd
     Else
         PlaySound playsound("FlipperDown"):RightFlipper.RotateToStart
     End If
  End Sub


'-----------------------------------
' Init the table, Start VPinMAME
'-----------------------------------
Dim bsTrough, bsLaunch, DTDrop, bsScoop, mOrbitMagnet, Snagger, mSnagger, mPFMagnet, mPFMagnet1, x, xx, bump1, bump2, bump3, SnaggerArm

Sub Table1_Init()
	vpmInit me
	On Error Resume Next
		Controller.GameName = "jplstw22"
		If Err Then MsgBox Err.Description, 0, "Game " & gameName & " Not found." : Exit Sub
	On Error Goto 0
	Controller.SplashInfoLine = "Jurrasic Park 2 The Lost World - Sega, 1997" & vbNewLine & "Bigus1/Edizzle/freneticamnesic/Dark/javier1515/arngrim"
	Controller.HandleKeyboard = False
	Controller.ShowTitle = False
	Controller.ShowFrame = False
	Controller.ShowDMDOnly = True
	Controller.HandleMechanics = 0
	Controller.DIP(0) = &H00 ' Set dipswitch to USA
	On Error Resume Next
		Controller.Run
		If Err Then MsgBox Err.Description
	On Error Goto 0

	PinMAMETimer.Interval = PinMAMEInterval
	vpmNudge.TiltSwitch = 56
	vpmNudge.Sensitivity = 5

'-----------------------------------
' Set Up Ballstacks and init info
'-----------------------------------
Set bsTrough = new cvpmBallStack
	bsTrough.InitSw 0,15,14,13,12,0,0,0
	bsTrough.InitKick BallRelease,40,6
	bsTrough.Balls = 4
	bsTrough.InitExitSnd playsound("Ballrel"),playsound("Solenoid")

Set bsLaunch=New cvpmBallStack
	bsLaunch.InitSaucer Launch,16,0,30
	bsLaunch.InitExitSnd playsound("solenoid"),playsound("Solon")

Set bsScoop = new cvpmBallStack
	bsScoop.InitSw 0,46,0,0,0,0,0,0
	bsScoop.InitKick Scoop, 200,15
	bsScoop.InitExitSnd playsound("scoopexit"),playsound("Solenoid")

Set DTdrop=New cvpmDropTarget: With DTdrop
	DTdrop.InitDrop DTsw29, 29
'	DTdrop.CreateEvents "DTdrop"
End With
	Solcallback(3)="DTdrop.SolDropUp"

Set Snagger=New cvpmMech
	Snagger.Sol1=18
	Snagger.Sol2=19
	Snagger.Length=420
	Snagger.MType= vpmMechOneSol+vpmMechReverse+vpmMechLinear
	Snagger.Steps=150
	Snagger.AddSw 35,0,0
	Snagger.AddSw 36,149,149
	Snagger.Callback=GetRef("UpdateSnagger")
	Snagger.Start


Set mPFMagnet=New cvpmMagnet
With mPFMagnet
	mPFMagnet.InitMagnet PFMagnet,20
	'mPFMagnet.Solenoid=14
	mPFMagnet.GrabCenter=true
	mPFMagnet.CreateEvents "mPFMagnet"
End With

Set mOrbitMagnet=New cvpmMagnet
	mOrbitMagnet.InitMagnet OrbitMagnet,70
	mOrbitMagnet.Solenoid=5
	mOrbitMagnet.GrabCenter=True
	mOrbitMagnet.CreateEvents "mOrbitMagnet"
 
End Sub

'-----------------------------------
' keyboard routines
'-----------------------------------

Sub Table1_KeyDown(ByVal KeyCode)
 	If vpmKeyDown(KeyCode) Then Exit Sub
	If KeyCode=PlungerKey Then Controller.Switch(53)=1
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If vpmKeyUp(KeyCode) Then Exit Sub
	If KeyCode=PlungerKey Then Controller.Switch(53)=0
End Sub

Sub SolTrough(Enabled)
	If Enabled Then	
		bsTrough.ExitSol_On
	vpmTimer.PulseSw 15	
	End If
End Sub

Sub Drain_Hit:bsTrough.AddBall Me: playsound "drain":Me.TimerInterval=100:Me.TimerEnabled=True:End Sub
Sub launch_Hit:bsLaunch.AddBall 0:launchlight.state = 2:End Sub
Sub launch_unHit:launchlight.state = 0:End Sub
Sub scoop_Hit: bsScoop.AddBall me End Sub

Sub EggFlash(Enabled)
	If Enabled Then
SetLamp 118, 1
SetLamp 119, 1
		Else
SetLamp 118, 0
SetLamp 119, 0
	End If
End Sub

Sub SolPFMagnet(enabled)
    If enabled Then 
	mPFMagnet.MagnetOn = 1
	Else 
	mPFMagnet.MagnetOn = 0
End If
End Sub

Sub MagnetKicker_Hit: Me.destroyBall: vpmTimer.AddTimer 4000,"bsScoop.AddBall": End Sub
Sub solSmagnet(Enabled)
	If Enabled Then
    MagnetKicker.Enabled = 1 
	PlaySound "collide1"
    Else
    MagnetKicker.Enabled = 0
 End if
End Sub

Sub solsnagger(enabled)
	If enabled then
		snagger1=True
Light024.state = 2
	Else
 snagger1=False
Light024.state = 0
	End if
End Sub

Sub solsnaggerrelay(enabled)
	If enabled then
		snagger2R = True
	Else snagger2R=False
	End if
End Sub

Dim snagger1:snagger1 = False
Dim snagger2:snagger2 = False
Dim snagger2R:snagger2R = False

Sub snagger1t_Timer()
	If snagger1 = True and snagger2R = False and arm.RotX >= 0 Then
		arm.rotX = arm.rotX - .5
	End if

	If arm.rotX <= 0 Then
		snagger2 = True
	End if

	If snagger1 = True and snagger2R = True and clawdown.rotX >= 0 Then
		arm.rotX = arm.rotX + .7
	End if
	
	If snagger1 = False and snagger2 = False and snagger2R = False and arm.rotX =< 0 Then
		arm.rotX = arm.rotX + .7
	End if

End Sub

Sub snagger2t_Timer()
	If snagger2 = True and snagger2R = False and clawdown.rotX >= -90 Then
		clawdown.rotX = clawdown.rotX - .5
	End if
	
	If snagger2 = True and snagger2R = True and clawdown.rotX <= 0 Then
		clawdown.rotX = clawdown.rotX + .5
	End if

	If snagger2 = True and snagger2R = True and clawdown.rotX >= 0 and arm.rotX <= 0 Then
		arm.rotX = arm.rotX + .7
	End if
	
	If clawdown.rotX = 0 Then
		snagger2 = False
	End if

End Sub


Sub UpdateSnagger(aNewPos,aSpeed,aLastPos)
	DOF 101, 2
	if anewpos < 150 then
		'Ptest.rotx = -anewpos -25
end if
End Sub

Sub SolEggout(Enabled)
	If Enabled Then
		EggRot = True
	Else
		EggRot = False
	End if
End Sub

Dim EggRot:EggRot = False
Sub vpmFastTimer_Timer()	
	If EggRot = True and Pegg.RotX >= 40 then 
		Pegg.RotX = Pegg.RotX - 3
		BabyTREX.Y = BabyTREX.Y + 1
	End If
	If EggRot = False and Pegg.RotX <= 90 then 
		Pegg.RotX = Pegg.RotX + 3
	BabyTREX.Y = BabyTREX.Y - 1
	End If
	'If Pegg.RotX= 0 then EggRot = False	
End Sub


Sub FlippersTimer_Timer()
	UpdateFlipperLogo	
End Sub

Sub UpdateFlipperLogo
    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
End Sub 

'***Slings and rubbers
  ' Slings
 Dim LStep, RStep

For each xx in LSling2:xx.visible = 0:Next
For each xx in LSling1:xx.visible = 0:Next

For each xx in RSling2:xx.visible = 0:Next
For each xx in RSling1:xx.visible = 0:Next
 
 Sub LeftSlingShot_Slingshot:vpmTimer.PulseSw 59:PlaySound playsound("slingshot"):
 	For each xx in LSling2:xx.visible = 1:Next
 	PlaySound "LSling":LStep = 0:Me.TimerEnabled = 1
  End Sub
 
 Sub LeftSlingShot_Timer
	 Select Case LStep
         Case 0: 'pause
         Case 1: 'pause
         Case 2:For each xx in LSling2:xx.visible = 0:Next
 				For each xx in LSling1:xx.visible = 1:Next
         Case 3: 'pause
         Case 4:For each xx in LSling1:xx.visible = 0:Next
 				Me.TimerEnabled = 0
     End Select
     LStep = LStep + 1
 End Sub
 
 Sub RightSlingShot_Slingshot:vpmTimer.PulseSw 62:PlaySound playsound("slingshot"):
 	 	For each xx in RSling2:xx.visible = 1:Next
 	PlaySound "RSling":RStep = 0:Me.TimerEnabled = 1
  End Sub
 
 Sub RightSlingShot_Timer
	 Select Case RStep
         Case 0: 'pause
         Case 1: 'pause
         Case 2:For each xx in RSling2:xx.visible = 0:Next
 				For each xx in RSling1:xx.visible = 1:Next
         Case 3:  'Pause
         Case 4:For each xx in RSling1:xx.visible = 0:Next
 				Me.TimerEnabled = 0
     End Select
     RStep = RStep + 1
 End Sub


   'Bumpers
      Sub Bumper1b_Hit:vpmTimer.pulseSW 49:PlaySound playsound("bumper"):bump1 = 1:Me.TimerEnabled = 1:End Sub
     
       Sub Bumper1b_Timer()
           Select Case bump1
               Case 1:BR1.z = 0:bump1 = 2
               Case 2:BR1.z = -35:bump1 = 3
               Case 3:BR1.z = -45:bump1 = 4
               Case 4:BR1.z = -45:bump1 = 5
               Case 5:BR1.z = -35:bump1 = 6
               Case 6:BR1.z = 0:bump1 = 7
               Case 7:BR1.z = 10:Me.TimerEnabled = 0
           End Select
       End Sub
 
      Sub Bumper2b_Hit:vpmTimer.pulseSW 50:PlaySound playsound("bumper"):bump2 = 1:Me.TimerEnabled = 1:End Sub
     
       Sub Bumper2b_Timer()
           Select Case bump2
               Case 1:BR2.z = 0:bump2 = 2
               Case 2:BR2.z = -35:bump2 = 3
               Case 3:BR2.z = -45:bump2 = 4
               Case 4:BR2.z = -45:bump2 = 5
               Case 5:BR2.z = -35:bump2 = 6
               Case 6:BR2.z = 0:bump2 = 7
               Case 7:BR2.z = 10:Me.TimerEnabled = 0
           End Select
       End Sub
 
      Sub Bumper3b_Hit:vpmTimer.pulseSW 51:PlaySound playsound("bumper"):bump3 = 1:Me.TimerEnabled = 1:End Sub
     
       Sub Bumper3b_Timer()
           Select Case bump3
               Case 1:BR3.z = 0:bump3 = 2
               Case 2:BR3.z = -35:bump3 = 3
               Case 3:BR3.z = -45:bump3 = 4
               Case 4:BR3.z = -45:bump3 = 5
               Case 5:BR3.z = -35:bump3 = 6
               Case 6:BR3.z = 0:bump3 = 7
               Case 7:BR3.z = 10:Me.TimerEnabled = 0
           End Select

       End Sub

'******DROP TARGET PRIMITIVES BY FRENETICAMNESIC******
Dim sw29up
Dim PrimT

Sub PrimT_Timer
	if DTsw29.IsDropped = True then sw29up = False else sw29up = True
End Sub


Sub sw29T_Timer()
	If sw29up = True and sw29p.z < 0 then sw29p.z = sw29p.z + 3
	If sw29up = False and sw29p.z > -50 then sw29p.z = sw29p.z - 3
	If sw29p.z >= -50 then sw29up = False
End Sub

'*************************************************************************************************************************
' upright Target using Primitive by oooPLAYER1ooo
'*************************************************************************************************************************
' case 0 + case 6 need to be the start pos of the primitive object
'
'what is happening here is by using a timer on the table named "test2" we are moving the target back slightly
' each step then in case 4 onwards returning it to its original spot

Dim targetpos1, targetpos2, targetpos3, targetpos4, targetpos5, targetpos6, targetpos7, targetpos8, targetpos9, targetpos10, targetpos11, targetpos12

Sub target1_Timer()	
 	   Select Case targetpos1
        Case 0: T1.x=890:playsound playsound("target"):           
        Case 1: T1.x=895 
        Case 2: T1.x=905
        Case 3: T1.x=910
        Case 4: T1.x=905
        Case 5: T1.x=895
        Case 6: T1.x=890 :target1.Enabled = 0

End Select

 targetpos1 = targetpos1 + 1
  End Sub


Sub target2_Timer()	
 	   Select Case targetpos2
        Case 0: T2.x=885:playsound playsound("target"):           
        Case 1: T2.x=890 
        Case 2: T2.x=900 
        Case 3: T2.x=905
        Case 4: T2.x=900
        Case 5: T2.x=890 
        Case 6: T2.x=885 :target2.Enabled = 0

End Select

 targetpos2 = targetpos2 + 1
  End Sub


Sub target3_Timer()	
 	   Select Case targetpos3
        Case 0: T3.x=880:playsound playsound("target"):           
        Case 1: T3.x=885
        Case 2: T3.x=895 
        Case 3: T3.x=900
        Case 4: T3.x=895 
        Case 5: T3.x=885 
        Case 6: T3.x=880 :target3.Enabled = 0

End Select

 targetpos3 = targetpos3 + 1
  End Sub


  Sub target4_Timer()	
 	   Select Case targetpos4
        Case 0: T4.x=55:playsound playsound("target"):           
        Case 1: T4.x=50 
        Case 2: T4.x=40
        Case 3: T4.x=35
        Case 4: T4.x=40 
        Case 5: T4.x=50 
        Case 6: T4.x=55 :target4.Enabled = 0

 
End Select

 targetpos4 = targetpos4 + 1
  End Sub

Sub target5_Timer()	
 	   Select Case targetpos5
        Case 0: T5.x=75:playsound playsound("target"):           
        Case 1: T5.x=70 
        Case 2: T5.x=60 
        Case 3: T5.x=55
        Case 4: T5.x=60
        Case 5: T5.x=70 
        Case 6: T5.x=75 :target5.Enabled = 0

End Select

 targetpos5 = targetpos5 + 1
  End Sub


Sub target6_Timer()	
 	   Select Case targetpos6
        Case 0: T6.x=95:playsound playsound("target"):           
        Case 1: T6.x=90 
        Case 2: T6.x=80 
        Case 3: T6.x=75
        Case 4: T6.x=80 
        Case 5: T6.x=90
        Case 6: T6.x=95 :target6.Enabled = 0

End Select

 targetpos6 = targetpos6 + 1
  End Sub


  Sub target7_Timer()	
 	   Select Case targetpos7
        Case 0: T7.y=405:playsound playsound("target"):           
        Case 1: T7.y=400 
        Case 2: T7.y=390
        Case 3: T7.y=385
        Case 4: T7.y=390 
        Case 5: T7.y=400 
        Case 6: T7.y=405 :target7.Enabled = 0

 
End Select

 targetpos7 = targetpos7 + 1
  End Sub

  Sub target8_Timer()	
 	   Select Case targetpos8
        Case 0: T8.y=750:playsound playsound("target"):           
        Case 1: T8.y=745 
        Case 2: T8.y=735 
        Case 3: T8.y=730
        Case 4: T8.y=735
        Case 5: T8.y=745
        Case 6: T8.y=750 :target8.Enabled = 0

 
End Select

 targetpos8 = targetpos8 + 1
  End Sub

Sub target9_Timer()	
 	   Select Case targetpos9
        Case 0: T9.y=715:playsound playsound("target"):           
        Case 1: T9.y=710 
        Case 2: T9.y=700 
        Case 3: T9.y=695
        Case 4: T9.y=700
        Case 5: T9.y=710 
        Case 6: T9.y=715 :target9.Enabled = 0

End Select

 targetpos9 = targetpos9 + 1
  End Sub


Sub target10_Timer()	
 	   Select Case targetpos10
        Case 0: T10.y=585:playsound playsound("target"):           
        Case 1: T10.y=580 
        Case 2: T10.y=570 
        Case 3: T10.y=565
        Case 4: T10.y=570 
        Case 5: T10.y=580
        Case 6: T10.y=585 :target10.Enabled = 0

End Select

 targetpos10 = targetpos10 + 1
  End Sub


  Sub target11_Timer()	
 	   Select Case targetpos11
        Case 0: T11.y=585:playsound playsound("target"):           
        Case 1: T11.y=580
        Case 2: T11.y=570
        Case 3: T11.y=565
        Case 4: T11.y=570
        Case 5: T11.y=580 
        Case 6: T11.y=585 :target11.Enabled = 0

 
End Select

 targetpos11= targetpos11+ 1
  End Sub

  Sub target12_Timer()	
 	   Select Case targetpos12
        Case 0: T12.y=735:playsound playsound("target"):           
        Case 1: T12.y=730
        Case 2: T12.y=720
        Case 3: T12.y=715
        Case 4: T12.y=720
        Case 5: T12.y=730
        Case 6: T12.y=735 :target12.Enabled = 0

 
End Select

 targetpos12= targetpos12+ 1
  End Sub

'*************************************************************************************************************************

'invisible wall to trigger hit on droptarget, add switch call here if using on a vpinmame table
sub sw09_hit():targetpos7=0:target7.Enabled=1: vpmtimer.PulseSw 9: Me.TimerEnabled = 1: end Sub
Sub sw09_Timer:Me.TimerEnabled = 0:End Sub
sub sw30_hit():targetpos1=0:target1.Enabled=1: vpmtimer.PulseSw 30: Me.TimerEnabled = 1:  end Sub
Sub sw30_Timer:Me.TimerEnabled = 0: End Sub
sub sw31_hit():targetpos2=0:target2.Enabled=1: vpmtimer.PulseSw 31: Me.TimerEnabled = 1:  end Sub
Sub sw31_Timer:Me.TimerEnabled = 0: End Sub
sub sw32_hit():targetpos3=0:target3.Enabled=1: vpmtimer.PulseSw 32: Me.TimerEnabled = 1:  end Sub
Sub sw32_Timer:Me.TimerEnabled = 0: End Sub
sub sw38_hit():targetpos4=0:target4.Enabled=1: vpmtimer.PulseSw 38: Me.TimerEnabled = 1:  end Sub
Sub sw38_Timer:Me.TimerEnabled = 0: End Sub
sub sw39_hit():targetpos5=0:target5.Enabled=1: vpmtimer.PulseSw 39: Me.TimerEnabled = 1:  end Sub
Sub sw39_Timer:Me.TimerEnabled = 0: End Sub
sub sw40_hit():targetpos6=0:target6.Enabled=1: vpmtimer.PulseSw 40: Me.TimerEnabled = 1:  end Sub
Sub sw40_Timer:Me.TimerEnabled = 0: End Sub
sub sw41_hit():targetpos8=0:target8.Enabled=1: vpmtimer.PulseSw 41:Me.TimerEnabled = 1: end Sub
Sub sw41_Timer:Me.TimerEnabled = 0:End Sub
sub sw42_hit():targetpos9=0:target9.Enabled=1: vpmtimer.PulseSw 42:Me.TimerEnabled = 1: end Sub
Sub sw42_Timer:Me.TimerEnabled = 0:End Sub
sub sw43_hit():targetpos10=0:target10.Enabled=1: vpmtimer.PulseSw 43:Me.TimerEnabled = 1: end Sub
Sub sw43_Timer:Me.TimerEnabled = 0:End Sub
sub sw44_hit():targetpos11=0:target11.Enabled=1: vpmtimer.PulseSw 44:Me.TimerEnabled = 1: end Sub
Sub sw44_Timer:Me.TimerEnabled = 0:End Sub
sub sw45_hit()
If mPFMagnet.MagnetOn = 1 Then
targetpos12=0:target12.Enabled=1:Me.TimerEnabled = 1
Else
targetpos12=0:target12.Enabled=1: vpmtimer.PulseSw 45:Me.TimerEnabled = 1
End If
End Sub
Sub sw45_Timer:Me.TimerEnabled = 0:End Sub

'********Switches**********

Sub Trigger3_hit: PlaySound "gate":End Sub

Sub trigger17_hit:Playsound "sensor":Controller.Switch(17)=1:End Sub
Sub trigger17_unhit:Controller.Switch(17)=0:End Sub
Sub trigger18_hit:Controller.Switch(18)=1:End Sub
Sub trigger18_unhit:Controller.Switch(18)=0:End Sub
Sub trigger19_hit:Controller.Switch(19)=1:End Sub
Sub trigger19_unhit:Controller.Switch(19)=0:End Sub
Sub trigger20_hit:Controller.Switch(20)=1:End Sub
Sub trigger20_unhit:Controller.Switch(20)=0:End Sub
Sub trigger21_hit:PlaySound "gate":Controller.Switch(21)=1:End Sub
Sub trigger21_unhit:Controller.Switch(21)=0:End Sub
Sub trigger22_hit:Controller.Switch(22)=1:End Sub
Sub trigger22_unhit:Controller.Switch(22)=0:End Sub
Sub trigger23_hit:PlaySound "gate":Controller.Switch(23)=1:End Sub
Sub trigger23_unhit:Controller.Switch(23)=0:End Sub
Sub trigger24_hit:PlaySound "gate":Controller.Switch(24)=1: End Sub
Sub trigger24_unhit:Controller.Switch(24)=0: End Sub

Sub DTsw29_hit:DTdrop.Hit 1:Me.TimerEnabled = 1:Playsound playsound("Dtarget"):End Sub
Sub DTsw29_Timer:Me.TimerEnabled = 0:Playsound playsound("Dtarget"): End Sub

Sub LeftSpinner_Spin:PlaySound "metalhit_thin":vpmTimer.PulseSwitch(33),0,"" end sub
Sub RightSpinner_Spin:PlaySound "metalhit_thin":vpmTimer.PulseSwitch(34),0,"" end sub

Sub trigger37_hit:Controller.Switch(37)=1:PlaySound "gate":End Sub
Sub trigger37_unhit:Controller.Switch(37)=0:End Sub


Sub scoop_Hit:bsscoop.AddBall Me:playsound "scoopenter":End Sub	


Sub trigger47_hit:Playsound "sensor":Controller.Switch(47)=1:End Sub
Sub trigger47_unhit:Controller.Switch(47)=0:End Sub
Sub trigger48_hit:Playsound "sensor":Controller.Switch(48)=1:End Sub
Sub trigger48_unhit:Controller.Switch(48)=0:End Sub
Sub trigger57_hit:Playsound "sensor":Controller.Switch(57)=1:End Sub
Sub trigger57_unhit:Controller.Switch(57)=0:End Sub
Sub trigger58_hit:Playsound "sensor":Controller.Switch(58)=1:End Sub
Sub trigger58_unhit:Controller.Switch(58)=0:End Sub
Sub trigger60_hit:Playsound "sensor":Controller.Switch(60)=1:End Sub
Sub trigger60_unhit:Controller.Switch(60)=0:End Sub
Sub trigger61_hit:Playsound "sensor":Controller.Switch(61)=1:End Sub
Sub trigger61_unhit:Controller.Switch(61)=0:End Sub

'**************		GI		*****************

Set GiCallback2 = GetRef("UpdateGI")
Dim xxx
Sub UpdateGI(nr,step)
				For each xxx in GILights:xxx.IntensityScale = 0.1 * step:next
		If Step>=7 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_extraConSat":Else Table1.ColorGradeImage = "ColorGrade_" & (step+3):End If
        		For each xxx in GILights:xxx.state=step:Next
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

Sub UpdateLamps
	NFadeL 1, Light1
	NFadeL 2, Light2
	NFadeL 3, Light3
	NFadeL 4, Light4
	NFadeL 5, Light5
	NFadeL 6, Light6
	NFadeL 7, Light7
	NFadeL 8, Light8
	NFadeL 9, Light9
	NFadeL 10, Light10
	NFadeL 11, Light11
	NFadeL 12, Light12
	NFadeL 13, Light13
	NFadeL 14, Light14
	NFadeL 15, Light15
	NFadeL 16, Light16
	NFadeL 17, Light17
	NFadeL 18, Light18
	NFadeL 19, Light19
	NFadeL 20, Light20
	NFadeL 21, Light21
	NFadeL 22, Light22
	NFadeL 23, Light23
	NFadeL 24, Light24
    NFadeL 25, Light25
    NFadeL 26, Light26
    NFadeL 27, Light27
    NFadeL 28, Light28
	NFadeL 29, Light29
    NFadeL 30, Light30
    NFadeL 31, Light31
    NFadeL 32, Light32
    NFadeL 33, Light33
    NFadeL 34, Light34
    NFadeL 35, Light35
    NFadeL 36, Light36
    NFadeL 37, Light37
	NFadeL 38, Light38
    NFadeL 39, Light39
    NFadeL 40, Light40
    'NFadeL 41, Light41
    NFadeL 42, Light42
    NFadeL 43, Light43
    NFadeL 44, Light44
    NFadeL 45, Light45
    NFadeL 46, Light46
    NFadeL 47, Light47
	'NFadeL 48, Light48
    NFadeLm 49, Light49
    NFadeL 49, Light49a
    NFadeLm 50, Light50
    NFadeL 50, Light50a
    NFadeLm 51, Light51
    NFadeL 51, Light51a
    NFadeL 54, Light54
    NFadeL 56, Light56	'Smart Missile/ Launch Button

    NFadeLm 103, L103a
    NFadeLm 103, L103b
    NFadeLm 103, L103c
    NFadeL 103, L103
    NFadeLm 106, L106a
    NFadeLm 106, L106b
    NFadeLm 106, L106c
    NFadeL 106, L106		
	NFadeLm 123, F123a
	NFadeLm 123, F123b
	NFadeL 123, F123
    NFadeL 101, F101
	NFadeL 102, F102
	NFadeL 104, F104
	NFadeL 105, F105
	NFadeL 107, F107
	NFadeL 108, F108
'	Flash 117, F117
	NFadeL 122, F122
'	Flash 121, F123
	FadeDisableLighting 118, BabyTREX, 1
	FadeDisableLighting 119, BackEgg, 1
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


Sub snaggerrot_Timer()
	If Ptest.RotX < 1 then
		Ptest.visible = 0
		clawdown.visible = 1
	Else
		Ptest.visible = 1
		clawdown.visible = 0
	End If
End Sub

Sub ballrot_Timer()
	If snagger2R = True and clawdown.rotX < 0 Then
		'BallUP.visible  = 0
		BallDOWN.visible = 1
	End if
		
	If Snagger2R = 1 and ptest.rotX > 0 Then
	'BallUP.visible = 1
	BallDOWN.visible = 0
	End If

	If ptest.rotX < 90 and ptest.rotX > 0 and snagger2R = True Then
	BallUP.visible = 1
	
	'BallDOWN.visible = 0
	End If

	If snagger1 = False Then
	BallUp.visible = 0
	End If

	If clawdown.RotX >=0 Then
	BallDOWN.visible = 0
	End If
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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/7, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/12, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
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


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall)/2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*5, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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


Sub Table1_exit()
	Controller.Pause = False
	Controller.Stop
End Sub