'Stern NBA 2009 By oooPlayer1ooo 




'*********************************************************************************
'*********       Load mame     ********************************************************
'*********************************************************************************

Option Explicit
Randomize

Const UseVpmModSol = True

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

if table1.ShowDT=true then
 LeftRail.visible=true
 RightRail.visible=true 
else
 LeftRail.visible=false
 RightRail.visible=false
end if

LoadVPM "01120100","SAM.VBS",3.02

Const cGameName="nba_802",cCredits="Stern NBA",UseSolenoids=1,UseLamps=1,UseGI=0,UseSync=1
Const SSolenoidOn="solon",SSolenoidOff="soloff",SFlipperOn="FlipperUp",SFlipperOff="FlipperDown",sCoin="Coin3"


Dim  bsTrough, bsVUK, mbackboard, turntable, mvpDrop, PlungerIM, CaptBall

'************************************************************************************
'***********        Table Init         **********************************************
'************************************************************************************


  Sub Table1_Init
InitVpmFFlipsSAM
  vpmInit Me
       On Error Resume Next
	       With Controller
		     .GameName=cGameName
		If Err Then MsgBox"Can't start Game"&cGameName&vbNewLine&Err.Description:Exit Sub
		     .SplashInfoLine=cCredits
		     .HandleMechanics=0
		     .ShowDMDOnly=1
		     .ShowFrame=0
		     .ShowTitle=0

             .Run
		If Err Then MsgBox Err.Description
	        End With
	   On Error Goto 0

'   Impulse Plunger

    Const IMPowerSetting = 55	
    Const IMTime = 0.6
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .InitExitSnd "plunger2", "plunger"
        .CreateEvents "plungerIM"
    End with

'    Nudging

	vpmNudge.TiltSwitch=-7
  	vpmNudge.Sensitivity=3

'   Main Timer init

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

'   Trough

    Set bsTrough = New cvpmBallStack
    bsTrough.InitSw 0, 21, 20, 19, 18, 0, 0, 0
    bsTrough.InitKick BallRelease, 90, 7
    bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), "Solenoid"
    bsTrough.Balls = 4

'   Backboard Magnet

    Set mbackboard = New cvpmMagnet
     With mbackboard
         .InitMagnet magtrigger, 60
         .Solenoid = 3
         .CreateEvents "mbackboard"
         .GrabCenter = 1

     End With

'    Spinning Disk

     Set turntable = New cvpmTurnTable
     With turntable
         .InitTurnTable spinningdisk,50
         .CreateEvents "turntable"
         .SpinCW = -1
         .spinUp = 100
         .SpinDown = 10

     End With

'    MVP Dropbank

     set mvpDrop = new cvpmDropTarget
        with mvpDrop
	         .InitDrop Array(Sw13,Sw12,Sw11), Array(13,12,11)
	         .Initsnd SoundFX("fx_droptarget", DOFContactors), SoundFX("fx_resetdrop", DOFContactors)
             .CreateEvents "mvpDrop"

       End With

     CaptKicker1a.CreateBall:CaptKicker1a.kick 180, 0

End Sub

Sub RightRampHelper_Hit
	If ActiveBall.VelY < 0 Then 
		ActiveBall.VelY = ActiveBall.VelY * 1.3
	End If
End Sub

Sub Drain_Hit():PlaySound "Drain":GIT1000.Enabled=1:bsTrough.AddBall Me:End Sub

Sub shooter_Hit:UpdateGI 0,0::Controller.Switch(35) = 1:Playsound "fx_kicker_enter", DOFContactors:End Sub
Sub shooter_UnHit:Playsound SoundFX("fx_kicker", DOFContactors):End Sub
Sub righteject_Hit:Controller.Switch(9) = 1:giflash.Enabled=1:Playsound "fx_kicker_enter", DOFContactors:End Sub
Sub righteject_UnHit:Playsound SoundFX("fx_kicker", DOFContactors):End Sub
'captiveball.CreateBall

div2.Isdropped = 1

'*******************************************************************************************
'***********          Keys             *****************************************************
'*******************************************************************************************

'Keys
Sub Table1_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then Plunger.Pullback 	
	If Keycode = StartGameKey Then Controller.Switch(16) = 1
	If vpmKeyDown(Keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If vpmKeyUp(Keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound "Plunger"
	If Keycode = StartGameKey Then Controller.Switch(16) = 0
End Sub

'******************************************************************************
'*******             solenoid callbacks                ************************
'******************************************************************************
dim diverter

   SolCallback(1) = "solTrough" ' trough
   SolCallback(2) = "solAutofire" ' autofire
   SolCallback(3) = "magnetwall" ' basket magnet
   SolCallback(4)="solshooter"' basket shooter
   SolCallback(5)="solnbabox" 'NBA BOX Diverter
   SolCallback(6)="solSpinningDisk"' spinning disk
   SolCallback(7)= "mvpDrop.SolDropUp"'  MVP 3 Drop Bank
   solcallback(12)="rampup"
solModcallback(13)= "SetLamp80" 'Backboard
solModcallback(14)= "SetLamp81" 'left ramp
   SolCallback(15) = "SolLFlipper"' Left Flipper
   SolCallback(16) = "SolRFlipper"' Right Flipper
   'SolCallback(17) = "LSlingShot"' leftsling
   'SolCallback(18) = "RSlingShot"' Rightsling
   SolCallback(20) = "solrighteject" ' right eject hole
   SolCallback(21) = "orbitblock" ' right eject hole

   SolModCallback(19) = "SetLamp151"' basketball flasher
   SolModCallback(23) = "SetLamp141"
solModcallback(25)= "SetLamp82" 'left ramp arrow
solModcallback(26)= "SetLamp83" 'left kicker arrow
solModcallback(27)= "SetLamp84" 'right kicker arrow
solModcallback(28)= "SetLamp85" 'right ramp arrow
   SolModCallback(29) = "SetLamp142"
solModcallback(30)= "SetLamp86" 'captive ball
solModcallback(31)= "SetLamp87" 'bumpers flash
   SolModCallback(32) = "Setlamp140"
   SolModCallback(9) = "Setlamp60"  'pop bumpers
   SolModCallback(10) = "Setlamp61"
   SolModCallback(11) = "Setlamp62"
   

   'SolCallback(sLRFlipper) = "SolRFlipper"  
   'SolCallback(sLLFlipper) = "SolLFlipper"  

set GICallback = GetRef("UpdateGI")

'*******************************************************************************
'*********            solenoid fuctions                   **********************
'*******************************************************************************

:primitive1.RotAndTra5=-48
Sub orbitblock(enabled)
    if Enabled then
       
:stopper.IsDropped=0
orbitTimerUp.Enabled=1
     else
        orbitTimerDWN.Enabled=1
:stopper.IsDropped=1

     end if
    End Sub

Dim STPos
StPos=0

  Sub orbitTimerUp_Timer()
 	 Select Case STPos	
 			Case 1:primitive1.RotAndTra5=-48
 			Case 2:primitive1.RotAndTra5=-38
 			Case 3:primitive1.RotAndTra5=-28
 			Case 4:primitive1.RotAndTra5=-18
 			Case 5:primitive1.RotAndTra5=8
            Case 6:primitive1.RotAndTra5=0:orbitTimerUp.Enabled=0 
End Select

 	If STpos<6 then STPos=STpos+1
  End Sub
 
  Sub orbitTimerDWN_Timer()
 	 Select Case STPos	

 			Case 1:primitive1.RotAndTra5=-48:orbitTimerDWN.Enabled=0
 			Case 2:primitive1.RotAndTra5=-38
 			Case 3:primitive1.RotAndTra5=-28
 			Case 4:primitive1.RotAndTra5=-18
 			Case 5:primitive1.RotAndTra5=8
            Case 6:primitive1.RotAndTra5=0
 	End Select
 	If STpos>0 Then STPos=STpos-1
  End Sub
'**************************************************************************************************************
'**************************************************************************************************************
Sub SetLamp140(m):m = m/255:sflash1.state=m:sflash2.state=m:sflash3.state=m:sflash4.state=m:Slingflashlinks.blenddisablelighting=m*4:Slingflashlinks001.blenddisablelighting=m*4:End Sub
Sub SetLamp141(m):m = m/255:l141.state=m:L141b.state=m:End Sub
Sub SetLamp142(m):m = m/255:l142.state=m:L142b.state=m:End Sub
Sub SetLamp151(m):m = m/255:bbp.blenddisablelighting=m*2 +0.2:End Sub
Sub SetLamp60(m):m = m/255:l60b.state=m:End Sub
Sub SetLamp61(m):m = m/255:l61b.state=m:End Sub
Sub SetLamp62(m):m = m/255:l62b.state=m:End Sub
Sub SetLamp80(m):m = m/255:l80.state=m:End Sub
Sub SetLamp81(m):m = m/255:l81.state=m:End Sub
Sub SetLamp82(m):m = m/255:l82.state=m:End Sub
Sub SetLamp83(m):m = m/255:l83.state=m:End Sub
Sub SetLamp84(m):m = m/255:l84.state=m:End Sub
Sub SetLamp85(m):m = m/255:l85.state=m:End Sub
Sub SetLamp86(m):m = m/255:l86.state=m:End Sub
Sub SetLamp87(m):m = m/255:l87.state=m:l87a.state=m:End Sub

Sub SetPops(enabled)
	If Enabled Then
		SetLamp 60, 1
		SetLamp 61, 1
		SetLamp 62, 1
	Else
		SetLamp 60, 0
		SetLamp 61, 0
		SetLamp 62, 0
	End If
End Sub

'   free throw kicker

        Sub solshooter(Enabled)
	        If Enabled Then
		       shooter.kick -39, 39, 0.9               
               Controller.Switch(35) = 0
               UpdateGI 0,3             
              
	        End If
        End Sub


'   Right Eject hole

        Sub solrighteject(Enabled)
	        If Enabled Then
		       righteject.kick -40, 20, 0               
               Controller.Switch(9) = 0
               giflash.Enabled=0:UpdateGI 0,3     
             
	        End If
        End Sub


'********************  Spinning Discs Animation Timer ****************************
Dim SpinnerMotorOff, SpinnerStep, ss

Sub solSpinningDisk(enabled)
	If enabled Then
		turntable.MotorOn = True
		SpinnerStep = 10
		SpinnerMotorOff = False
		TTTimer.Interval = 7
		TTTimer.enabled = True
	Else
		SpinnerMotorOff = True
		turntable.MotorOn = False
	end If
End Sub

Sub TTTimer_Timer()
	If Not(SpinnerMotorOff) Then
		spindisc.ObjRotZ  = ss
		ss = ss + SpinnerStep
	Else
		if SpinnerStep < 0 Then
			TTTimer.enabled = False
		Else
		'slow the rate of spin by decreasing rotation step
			SpinnerStep = SpinnerStep - 0.05
			
			spindisc.ObjRotZ  = ss
			ss = ss + SpinnerStep
		End If
	End If
	if ss > 360 then ss = ss - 360
End Sub


'    Nba Box Ramp Diverter

         Sub solnbaBox(Enabled)
            If Enabled Then

             div1.IsDropped = 1   
             div2.IsDropped = 0       
        else

              div1.IsDropped = 0            
              div2.IsDropped = 1
             End If

          End Sub

'    Left Slingshot
   
       Sub LSlingShot(Enabled)
           If Enabled Then
               PlaySound SoundFX("slingshot-left",DOFContactors)
           End If
       End Sub

'    Right Slingshot
   
       Sub RSlingShot(Enabled)
           If Enabled Then
               PlaySound SoundFX("slingshot-right",DOFContactors)
           End If
       End Sub

'    Left flipper
   
       Sub SolLFlipper(Enabled)
           If Enabled Then
               PlaySound SoundFX("fx_FlipperUp",DOFFlippers):LeftFlipper.RotateToEnd:
 			   
           Else
               PlaySound SoundFX("fx_flipperdown",DOFFlippers):LeftFlipper.RotateToStart
 			   
           End If
       End Sub
     
'    Right flipper


       Sub SolRFlipper(Enabled)
           If Enabled Then
               PlaySound SoundFX("fx_FlipperUp",DOFFlippers):RightFlipper.RotateToEnd':RightFlipperUp.RotateToEnd
 			  
           Else
               PlaySound SoundFX("fx_flipperdown",DOFFlippers):RightFlipper.RotateToStart':RightFlipperUp.RotateToStart
 			   
           End If
       End Sub

'  Autofire plunger

          Sub solAutofire(Enabled)
	           If Enabled Then

		          PlungerIM.AutoFire
	          
                 End If
          End Sub

' Main trough
        
           Sub solTrough(enabled)
 	             If enabled then

 		            bsTrough.ExitSol_On
 		           vpmTimer.PulseSw 22
 	
                  end if
           End Sub

' wall base for basket magnet "Init by solenoid 3 and used with vpmMagnet class for propper basket effect."

sub magnetwall(enabled)
     If enabled then 
ringblock.collidable = 1
else
ringblock.collidable = 0
end if
 end sub

 Sub leftRampHelper_Hit:ActiveBall.VelY=0:PlaySound "balldrop":End Sub
 Sub leftRampHelper1_Hit:ActiveBall.VelY=0:PlaySound "balldrop":End Sub


 ' autoPlunger init
	AutoPlunger.Pullback

'************************************************************************************
'*****************       SLING Subs                      ****************************
'************************************************************************************


'  init sling walls

LeftSling.IsDropped = 1:LeftSling2.IsDropped = 1:LeftSling3.IsDropped = 1:
RightSling.IsDropped = 1:RightSling2.IsDropped = 1:RightSling3.IsDropped = 1:

'  Sling Routines

Dim LStep, RStep 
 
 Sub LeftSlingShot_Slingshot:LeftSling.IsDropped = 0:PlaySound SoundFX("Lsling",DOFContactors):Controller.Switch(26) = 1:LStep=0:Me.TimerEnabled = 1:End Sub
  Sub LeftSlingShot_Timer
 Select Case LStep
     Case 0: LeftSLing.IsDropped = 0
      Case 1: 'pause
      Case 2: LeftSLing.IsDropped = 1:LeftSLing2.IsDropped = 0
      Case 3: LeftSLing2.IsDropped = 1:LeftSLing3.IsDropped = 0
      Case 4: LeftSLing3.IsDropped = 1:Me.TimerEnabled = 0
  End Select
  LStep=LStep+1
Controller.Switch(26) = 0
  End Sub
 
 Sub RightSlingShot_Slingshot:RightSling.IsDropped = 0:PlaySound SoundFX("Rsling",DOFContactors):Controller.Switch(27) = 1:RStep=0:Me.TimerEnabled = 1:End Sub
 Sub RightSlingShot_Timer
  Select Case RStep
     Case 0: RightSLing.IsDropped = 0
      Case 1: 'pause
      Case 2: RightSLing.IsDropped = 1:RightSLing2.IsDropped = 0
      Case 3: RightSLing2.IsDropped = 1:RightSLing3.IsDropped = 0
      Case 4: RightSLing3.IsDropped = 1:Me.TimerEnabled = 0
  End Select
   RStep=RStep+1
Controller.Switch(27) = 0
 End Sub

'***** Bumpers 

Sub sw30_Hit:vpmTimer.PulseSw 30:PlaySound SoundFX("bumper-thump",DOFContactors):End Sub 

 Sub sw31_Hit:vpmTimer.PulseSw 31:PlaySound SoundFX("bumper-thump",DOFContactors):End Sub 
 
 Sub sw32_Hit:vpmTimer.PulseSw 32:PlaySound SoundFX("bumper-thump",DOFContactors):End Sub

'************************************************************************************
'*****************       M V P 3 Drop Target Bank        ****************************
'************************************************************************************

  'M
     Sub Sw13_Hit:mvpDrop.Hit 1:Sw13.isDropped=1:vpmTimer.PulseSwitch(13),0,"" : End Sub
  'V
     Sub Sw12_Hit:mvpDrop.Hit 2:Sw12.isDropped=1:vpmTimer.PulseSwitch(12),0,"" : End Sub
  'P
     Sub Sw11_Hit:mvpDrop.Hit 3:Sw11.isDropped=1:vpmTimer.PulseSwitch(11),0,"" : End Sub


'************************************************************************************
'***********            Rollover Subs          **************************************
'************************************************************************************
 
 Sub sw14_Hit:Controller.Switch(14) = 1:PlaySound "rollover":End Sub
 Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub
 Sub sw23_Hit:Controller.Switch(23) = 1:PlaySound "rollover":End Sub
 Sub sw23_UnHit:Controller.Switch(23) = 0:UpdateGI 0,3:End Sub 
 Sub sw24_Hit:Controller.Switch(24) = 1:PlaySound "rollover":End Sub
 Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
 Sub sw25_Hit:Controller.Switch(25) = 1:PlaySound "rollover":End Sub
 Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
 Sub sw28_Hit:Controller.Switch(28) = 1:PlaySound "rollover":End Sub
 Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub
 Sub sw29_Hit:Controller.Switch(29) = 1:PlaySound "rollover":End Sub
 Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub
 Sub sw36_Hit:Controller.Switch(36) = 1:PlaySound "rollover":End Sub
 Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub

'************************************************************************************
'***********            Allstar          **************************************
'************************************************************************************

    Sub sw1_Hit:vpmTimer.PulseSw 1:PlaySound SoundFX("target",DOFTargets):End Sub   
    Sub sw2_Hit:vpmTimer.PulseSw 2:PlaySound SoundFX("target",DOFTargets):End Sub    
    Sub sw3_Hit:vpmTimer.PulseSw 3:PlaySound SoundFX("target",DOFTargets):End Sub   
    Sub sw4_Hit:vpmTimer.PulseSw 4:PlaySound SoundFX("target",DOFTargets):End Sub    
    Sub sw5_Hit:vpmTimer.PulseSw 5:PlaySound SoundFX("target",DOFTargets):End Sub   
    Sub sw6_Hit:vpmTimer.PulseSw 6:PlaySound SoundFX("target",DOFTargets):End Sub    
    Sub sw7_Hit:vpmTimer.PulseSw 7:PlaySound SoundFX("target",DOFTargets):End Sub 

'   other standups

    Sub sw47_Hit:vpmTimer.PulseSw 47:sw47.IsDropped = 1:sw47a.IsDropped = 0:Me.TimerEnabled = 1:PlaySound SoundFX("target",DOFTargets):End Sub
    Sub sw47_Timer:sw47.IsDropped = 0:sw47a.IsDropped = 1:Me.TimerEnabled = 0:End Sub

    Sub sw10_Hit:vpmTimer.PulseSw 10:sw10.IsDropped = 1:sw10a.IsDropped = 0:Me.TimerEnabled = 1:PlaySound SoundFX("target",DOFTargets):End Sub
    Sub sw10_Timer:sw10.IsDropped = 0:sw10a.IsDropped = 1:Me.TimerEnabled = 0:End Sub

'ramp switch before nba box

Sub Sw37_Hit:vpmTimer.PulseSwitch(37),0,"" : End Sub

Sub Sw8_Hit:vpmTimer.PulseSwitch(8),0,"" : End Sub

' 3point ramp optic switch

 Sub sw33_Hit:Controller.Switch(33) = 1:End Sub
 Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub

' Hoop Optic switch
 Sub hooptrigger_Hit:Controller.Switch(34) = 1:End Sub
 Sub hooptrigger_UnHit:Controller.Switch(34) = 0:End Sub

  '******************************************************************************************************************************************************************************************
 '  JP's Fading Lamps 3.4 VP9 Fading only
 '      Based on PD's Fading Lights
 ' SetLamp 0 is Off
 ' SetLamp 1 is On
 ' LampState(x) current state
 '*******************************************************************************************
 
set lights(3)=L3
set lights(4)=L4
set lights(5)=L5
set lights(6)=L6
set lights(7)=L7
set lights(9)=L9

set lights(10)=L10
set lights(12)=L12

set lights(13)=L13
set lights(14)=L14
set lights(15)=L15
set lights(16)=L16
set lights(17)=L17
set lights(18)=L18
set lights(19)=L19

set lights(20)=L20
set lights(21)=L21
set lights(22)=L22
set lights(23)=L23
set lights(24)=L24
set lights(25)=L25
set lights(26)=L26
set lights(27)=L27
set lights(28)=L28

set lights(30)=L30
set lights(31)=L31
set lights(32)=L32
set lights(33)=L33
set lights(35)=L35
set lights(36)=L36
set lights(37)=L37
set lights(39)=L39

set lights(40)=L40
set lights(41)=L41
set lights(42)=L42
set lights(43)=L43
set lights(44)=L44
set lights(45)=L45
set lights(46)=L46
set lights(47)=L47
set lights(48)=L48
set lights(49)=L49

set lights(50)=L50
set lights(51)=L51
set lights(52)=L52
set lights(54)=L54
set lights(57)=L57
set lights(58)=L58
set lights(59)=L59

lights(60)=array(L60,l60a)
lights(61)=array(L61,l61a)
lights(62)=array(L62,l62a)
set lights(63)=L63
set lights(64)=L64


Dim LampState(200)
 Dim X
 
Sub SetLamp(nr, value)	
End Sub

SetPops 0

'***************************************************************************************************************************************************************************************
'***********************GI Routine****************************

dim xx 
  Sub UpdateGI(No,enabled)
 	if enabled then 		
	For Each xx in GILights:xx.State=1:Next  
    Table1.colorgradeimage = "ColorGrade_8"
	URPL.image="nba upper ramp":lmramp.image="ramp steel":botplastics.image="bottomflash":bbp1.image="backbox_primitive":wall106.blenddisablelighting=0.1
DOF 103, DOFOn
    Else
For Each xx in GILights:xx.State=0:Next
    Table1.colorgradeimage = "ColorGrade_4"
    URPL.image="nba upper ramp_off":lmramp.image="ramp steel_off":botplastics.image="bottomflash_off":bbp1.image="backbox_primitive_off":wall106.blenddisablelighting=0
DOF 103, DOFOff
   End If
 End Sub

dim flash 
'flash = 0

Sub giflash_Timer
    Select Case flash
        Case 0:UpdateGI 0,0:PlaySound "relay"
        Case 1:UpdateGI 0,3
        Case 2:UpdateGI 0,0:PlaySound "relay"
        Case 3:UpdateGI 0,3
        Case 4:UpdateGI 0,0:PlaySound "relay"
        Case 5:UpdateGI 0,3
        Case 6:UpdateGI 0,0:PlaySound "relay"
        Case 7:UpdateGI 0,3
        Case 8:UpdateGI 0,0:PlaySound "relay"
        Case 9:UpdateGI 0,3:flash = 0
    End Select

    flash = flash + 1
End Sub

Sub UpdateFlipperLogos
    LFLogo.RotZ = LeftFlipper.CurrentAngle
    RFlogo.RotZ = RightFlipper.CurrentAngle
End Sub

'******************
' RealTime Updates
'******************
dim MotorCallback
Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
    UpdateFlipperLogos
End Sub

'*********************************************************************************************
rsolidramp6.visible=1:
rsolidramp5.visible=0:
rsolidramp4.visible=0:
rsolidramp3.visible=0:
rsolidramp2.visible=0:
rsolidramp1.visible=0
'      [ raising ramp animation]
 Dim Hpos, sPos	'Animation position 
  Hpos = 0:	'Set Starting positions
 
 sub rampup(enabled)
 if enabled then
 	rampTimerUp.Enabled=1 'Start animation
    rsolidramp.collidable=1
 else
    rampTimerDwn.Enabled=1
    sPos=1:sramp.Enabled=1
    
 end if
 End Sub
 
 'rsolidramp6.visible=1

Sub sramp_Timer()
 	 Select Case sPos	
 			Case 0:
 			Case 1:rsolidramp.collidable=0:sramp.Enabled=0
 			End Select
End Sub

  Sub rampTimerUp_Timer()
 	 Select Case HPos	
 			Case 1:rsolidramp6.visible=1
 			Case 2:rsolidramp5.visible=1:rsolidramp6.visible=0
 			Case 3:rsolidramp4.visible=1:rsolidramp5.visible=0
 			Case 4:rsolidramp3.visible=1:rsolidramp4.visible=0
 			Case 5:rsolidramp2.visible=1:rsolidramp3.visible=0
            Case 6:rsolidramp1.visible=1:rsolidramp2.visible=0:rampTimerUp.Enabled=0 
End Select

 	If Hpos<6 then HPos=Hpos+1
  End Sub
 
  Sub rampTimerDWN_Timer()
 	 Select Case HPos	

 			Case 1:rsolidramp6.visible=1:rampTimerDWN.Enabled=0
 			Case 2:rsolidramp5.visible=0:rsolidramp6.visible=1:
 			Case 3:rsolidramp4.visible=0:rsolidramp5.visible=1:
 			Case 4:rsolidramp3.visible=0:rsolidramp4.visible=1:
 			Case 5:rsolidramp2.visible=0:rsolidramp3.visible=1:
            Case 6:rsolidramp1.visible=0:rsolidramp2.visible=1:
 	End Select
 	If Hpos>0 Then HPos=Hpos-1
  End Sub
'**************************************************************************************************************

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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/10, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
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

 '**********************
'Flipper Shadows
'***********************
Sub RealTime_Timer
  lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
BallShadowUpdate
End Sub


Sub BallShadowUpdate()
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
        If BOT(b).Z > 20 and BOT(b).Z < 300 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 90
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 80
End If
    Next	
End Sub

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
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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