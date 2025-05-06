'************ Freddy, A Nightmare On Elm Street
'************ Premier 1994
'************ unclewilly, grizz, hassenchop for VPM5 version 
'************ and base for the VPX one by Team PP : JPJ - Chucky - Aetios - NeoFR45 - Arngrim


    Option Explicit
    Randomize


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0


Const BallSize = 52
Const BallMass = 1.8

'*********************************************************************************************************
'*** to show dmd in desktop Mod - Taken from ACDC Ninuzzu (THX) And Thanks To Rob Ross for Helping *******
Dim UseVPMDMD, DesktopMode
DesktopMode = Table1.ShowDT 
If NOT DesktopMode Then UseVPMDMD = False		'hides the internal VPMDMD when using the color ROM or when table is in Full Screen and color ROM is not in use
If DesktopMode Then UseVPMDMD = True							'shows the internal VPMDMD when in desktop mode 
'*********************************************************************************************************


   LoadVPM "01560000", "gts3.VBS", 3.10

 
   'Variables
	Dim xx
	Dim Bump1, Bump2, Bump3
	Dim cGameName
 	Dim bsTrough, DTBank, MechBoiler, bsVUKT, bsVUKB, bsFreddyHead, cb, PlungerIM, bsBallrelease, FastFlips
	Dim clawOn
	Dim BallinPlunger
	Dim GlobalSoundLevel, R, G, B, TI
	Dim obj, rGreen, rRed, rBlue, RGBFactor, RGBStep, ballinplay
	GlobalSoundLevel = 2
 
     Const UseSolenoids = 2
     Const UseLamps = 0
     Const UseSync = 0
     Const HandleMech = 0
	Const Scoin = "Coinin"

'************************** Start Options *****************************

'**********************************************************************
'**     Option sides or not sides in FS : 0 = without or 1 = with    **
'**********************************************************************
Dim sides                                                                  '**
sides = 1															'**
'**********************************************************************

'**********************************************************************
'**    Option Freddy's Animations - Opt = 0 = without or 1 = with    **
'**********************************************************************
Dim Opt                                                                    '**
Opt = 1															    '**
'**********************************************************************

'**********************************************************************
'**    Option Shadows - Opt = 0 = without or 1 = with                **
'**********************************************************************
Dim shad                                                                   '**
shad = 0															'**
'**********************************************************************

'*************************** End Options ******************************



'******** Flupper Dome ini **********
Dim FlashLevel1, FlashLevel2
F17L.IntensityScale = 0
F19L.IntensityScale = 0
'************************************

   cGameName = "freddy"
 
Sub Table1_Exit  '  in some tables this needs to be Table1_Exit
    Controller.Stop
End Sub



 '************
 ' Table init.
 '************

  Sub Table1_Init
shadow.opacity = 80
sideoff

vpmInit Me
dm

	With Controller
        .GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Freddy, Premier 1994"
        .HandleMechanics = 0
        .HandleKeyboard = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .ShowTitle = 0
      End With
   	  Controller.Hidden = 0
      On Error Resume Next
  
  	  Controller.Run
  '	If Err Then MsgBox Err.Description
  '   On Error Goto 0


    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1,Bumper2,LeftSlingshot,RightSlingshot, sw16, sw15)


	clawOn = 0
	FclawR.enabled = 0
	FclawL.enabled = 0

	JawP.z=110'35 'old 20
	Primitive6.z=20
	Primitive4.objrotx = -3
	Primitive4.objroty = 3

'	SaveValue cRegistryName, "Options", 0 : Exit Sub  ' This will clear your Registry settings if uncommented
'  	FreddyOptions = CInt("0" & LoadValue(cRegistryName, "Options")) : FreddySetOptions 
' 	If (FreddyOptions And cOptNoStartM) = 0 Then FreddyShowDips:end if	



' Autofire Plunger
    Const IMPowerSetting = 45
    Const IMTime = 0.4
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .InitExitSnd "plunger", "solenoid"
        .CreateEvents "plungerIM"
    End With




       '**Trough
          Set bsTrough = New cvpmBallStack
            With bsTrough
             .InitSw 23,0,0,22,0,0,0,0
      	     .InitKick BallRelease, 90, 8
                .InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
       	     '.BallImage = "ballDark"
             .Balls = 3
            End With
		bsTrough.AddBall 0

  Set bsBallRelease = New cvpmBallStack
     With bsBallRelease
         .InitSaucer BallRelease, 150, 110, 20
         .InitEntrySnd "solenoid", "solenoid"
           .InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("solenoid",DOFContactors)
     End With

          Set bsFreddyHead = New cvpmBallStack
            With bsFreddyHead
             .InitSw 0,80,0,0,0,0,0,0
      	     .InitKick sw80a, 130, 21	
      		 .KickForceVar = 5
      		 .KickAngleVar = 5
              .InitExitSnd SoundFX("Solenoid",DOFContactors), SoundFX("Solenoid",DOFContactors)
            End With

   '**VUK Top
          Set bsVUKT = New cvpmBallStack
            With bsVUKT
             .InitSaucer sw21,21,190,10
			 .KickZ= 3
             .InitExitSnd SoundFX("Solenoid",DOFContactors), SoundFX("Solenoid",DOFContactors)
            End With

   '**VUK bottom
          Set bsVUKB = New cvpmBallStack
            With bsVUKB
             .InitSaucer sw20,20,150,15
			 .KickZ= 99
             .InitExitSnd SoundFX("Solenoid",DOFContactors), SoundFX("Solenoid",DOFContactors)
            End With


   'DropTargets
   	Set DTBank = New cvpmDropTarget  
   	  With DTBank
   		.InitDrop Array(Array(sw17,sw17a),Array(sw27,sw27a),Array(sw37,sw37a)), Array(17,27,37)
		.InitSnd SoundFX("DTC",DOFDropTargets),SoundFX("DTReset",DOFContactors)
       End With

   'Boiler Door Animation
     Set MechBoiler = new cvpmMech
     With MechBoiler
         .Sol1 = 24
         .Mtype = vpmMechReverse + vpmMechOneSol  'vpmMechLinear + 
         .Length = 110
         .Steps = 10
		 .Acc = 20
		 .Ret = 0
         .AddSw 24, 1, 1
         .AddSw 34, 9, 9
         .Callback = GetRef("UpdateBoiler")
         .Start
     End With

  'Captive ball
     Set cb = New cvpmCaptiveBall
     With cb
         .InitCaptive captTrig, captwall, capt, 360
         .ForceTrans = .9
         .MinForce = 3.5
         .Start
     End With

'**** Fastflips
    Set FastFlips = new cFastFlips
    with FastFlips
       .CallBackL = "SolLflipper"  'Point these to flipper subs
       .CallBackR = "SolRflipper"  '...
'       .CallBackUL = "SolULflipper"'...(upper flippers, if needed)
'       .CallBackUR = "SolURflipper"'...
       .TiltObjects = True 'Optional, if True calls vpmnudge.solgameon automatically. IF YOU GET A LINE 1 ERROR, DISABLE THIS! (or setup vpmNudge.TiltObj!)
 '      .DebugOn = True        'Debug, always-on flippers. Call FastFlips.DebugOn True or False in debugger to enable/disable.
    end with


  'BoilerDoorInit
		InitBoilerDoor


'********** diverters init ************
	Door01.ObjRotz = 0
	Door02.ObjRotz = 245
	RD1.IsDropped=1:RD2.IsDropped=1:RDOpen.IsDropped=1
	TDA1.IsDropped=1:TDA2.IsDropped=1:TDOpen.IsDropped=1
'**************************************

'**** Kickback and autoplunger ****
	kickbackwall.IsDropped=1
	kickback.PullBack
'	Plunger.PullBack
'**********************************

'********** Init Gi - New VPX *************
	For each xx in GI:xx.state=0:next
		FLaneL1.state = 0
		FLaneL.state = 0
		FLaneL12.state = 0
		FLaneL2.state = 0
		shadow.opacity = 80

		RainbowTimer.enabled = 1
'******************************************

'********** Go for GI Light On ************
    GIT.Enabled=1
'******************************************

'********** InitFlashers **************
	l10.state = 0
	l10In.state = 0
	l11.state = 0
	l11In.state = 0
	'f17l.state = 0 'FlasherDome, Flupper's method never state off
	f17lint.state = 0
	'f19l.state = 0 'FlasherDome, Flupper's method never state off
	f19lint.state = 0
	PRefresh.state = 0
	Lightclaw.state = 0
	Flasherflash3.amount = 0
	Flasherflash3.intensityScale = 0
	F19flashsecA.amount = 0
	F19FlashsecA.intensityScale = 0
	F19flashsecB.amount = 0
	F19FlashsecB.intensityScale = 0
	F19flashsecC.amount = 0
	F19FlashsecC.intensityScale = 0
	F19flashsecD.amount = 0
	F19FlashsecD.intensityScale = 0
	F17flashsecA.amount = 0
	F17FlashsecA.intensityScale = 0
	F17flashsecB.amount = 0
	F17FlashsecB.intensityScale = 0
	F17flashsecC.amount = 0
	F17FlashsecC.intensityScale = 0
	F17flashsecD.amount = 0
	F17FlashsecD.intensityScale = 0
	Flash22r.amount = 0
	Flash22r.intensityScale = 0
'***************************************

'********** variables ******************
rt=0
rb=0
plb=0
plt=0
RGBStep = 0
rgreen = 0
rblue = 0
rRed = 0
ballinplay = 0 'made for scripting GI (SolCallback(32) taken for FastFlips)
'***************************************
  End Sub
'*********************** end table ini ********************************

'********** GI Light On ***********
  Sub GIT_Timer()
	For each xx in GI:xx.state=1:next
	GIT.Enabled=0
if shad = 1 then shadow.opacity = 30:end If
	SideOn
  End Sub
'**********************************


 Sub InitBoilerDoor()
     For xx = 1 To 7
         boilerdoor(xx).IsDropped = 1
     Next
 End Sub
 
 Sub UpdateBoiler(currpos, currspeed, lastpos)
     If currpos <> lastpos Then
         boilerdoor(lastpos).IsDropped = 1
         boilerdoor(currpos).IsDropped = 0
     End If
 End Sub



   Sub Table1_Paused:Controller.Pause = 1:End Sub
   Sub Table1_unPaused:Controller.Pause = 0:End Sub
 

'*************** Keys *****************
Sub Table1_KeyDown(ByVal Keycode)

	'********* nudge *******
	If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge",0)
	If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge",0)
	If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge",0)

	If Keycode = StartGameKey then Controller.Switch(4) = 1:end If

	If Keycode = KeyReset then 
		For each xx in GI:xx.state=0:next
		GIT.Enabled=1
	End if

    If keycode = PlungerKey Then 
		Plunger.Pullback
	end if


    If KeyCode = LeftFlipperKey then FastFlips.FlipL True ':  FastFlips.FlipUL True
    If KeyCode = RightFlipperKey then FastFlips.FlipR True:FastFlips.FlipUR True

' 	If Keycode = LeftFlipperKey then 
'		If FGA = 1 then 
'			glove.RotateToEnd
'			Controller.Switch(45) = 1 
'			else
'			Controller.Switch(45) = 1 
'		end if
'	End If
'
' 	If Keycode = RightFlipperKey then Controller.Switch(47) = 1:end if

    If KeyCode = RightMagnaSave then 
			Controller.Switch(7) = 1
			clawOn = 1
			FClawL.rotatetoend
			FClawR.rotatetoend
	end if

    If vpmKeyDown(keycode) Then Exit Sub 
End Sub


Sub Table1_KeyUp(ByVal keycode)
	If Keycode = StartGameKey then Controller.Switch(4) = 0

    If KeyCode = LeftFlipperKey then FastFlips.FlipL False ':  FastFlips.FlipUL True
    If KeyCode = RightFlipperKey then FastFlips.FlipR False:FastFlips.FlipUR True

' 	If Keycode = LeftFlipperKey then 
'		glove.RotateToStart
'		Controller.Switch(45) = 0
'	end if
'		
' 	If Keycode = RightFlipperKey then Controller.Switch(47) = 0

	If KeyCode = RightMagnaSave then 
		Controller.Switch(7) = 0
		clawOn = 0
		Lightclaw.state = 0
    		FClawL.rotatetostart
		FClawR.rotatetostart
		FClaw.rotatetostart
		Flasherflash3.amount = 0
		Flasherflash3.intensityScale = 0
	end If

	If keycode = PlungerKey Then
        If(BallinPlunger = 1) then 'the ball is in the plunger lane
            PlaySound "Plunger2"
		Plunger.Fire
        else
            PlaySound "Plunger"
		Plunger.Fire
        end if
    End If

     If vpmKeyUp(keycode) Then Exit Sub
End Sub

'***************** end keys ********************


'**************** Solenoids ********************

		SolCallback(1) = "" 'Top Bumper
		SolCallback(2) = "" 'Bottom Bumper
		SolCallback(3) = "" 'LeftSling
		SolCallback(4) = "" 'RightSling
		SolCallback(5) = "" 'LeftKicking Target
		SolCallback(6) = "" 'RightKicking Target
		SolCallback(7) = "SolKickBack"
		SolCallback(8) = "SolClawSave"
		SolCallback(9) = "solAutofire" '"SolAutoplung"
		SolCallback(10) = "SolDivTop"
		SolCallback(11) = "SolDivBot"            
		SolCallback(12) = "solTUK"
		SolCallback(13) = "bsFreddyHead.SolOut"  
		SolCallback(14) = "bsVUKB.SolOut"   
		SolCallback(15) = "SolMouth"   
		SolCallback(16) = "ResetDrops" 'OK  		      
		SolCallback(17) = "SolLanes"  'SolLanes      		  
		SolCallback(18) = "Solflash18" 'ok flasher inlay près des flippers              
		SolCallback(19) = "Solflash19" 'ok flash 17 flasher près du glover      
		SolCallback(20) = "SolF24"  '  'ok flash 19 flasher haut gauche   
		SolCallback(21) = "SolFreddy"  'ok Freddy's Head        
		SolCallback(22) = "SolF22"     'ok Boiler - Four        
		SolCallback(23) = "SolHouse"              
		'SolCallback(24) = "SolBoilerMotor"          
		SolCallback(25) = "SolGloveFlipper"
		SolCallback(27) = "Solrelease"   ' old commented '"SolTicketDis"
		SolCallback(28) = "bsTrough.SolOut"
		SolCallback(29) = "bsTrough.SolIn"
		SolCallback(30) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
		SolCallback(31) = "vpmNudge.SolGameOn"
'		SolCallback(32)  ="FastFlips.TiltSol"
		SolCallback(32)  ="solGOR"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
'SolCallback(sLURFlipper) = "SolURflipper"

sub GION
		if shad = 1 then shadow.opacity = 30:end if
		For each xx in GI:xx.state=1:next
		l10.state = 1
		l10In.state = 1
		l11.state = 1
		l11In.state = 1
		SideOn
end Sub

sub GIOFF
		shadow.opacity = 80
		For each xx in GI:xx.state=0:next
		l10.state = 0
		l10In.state = 0
		l11.state = 0
		l11In.state = 0

		SideOff
end Sub


Sub SolLFlipper(Enabled)
	If enabled Then
			If FGA = 1 then
				PlaySound SoundFX("LFlip",DOFFlippers):LeftFlipper.RotateToEnd
				glove.RotateToEnd
'				Controller.Switch(81) = 1 '45
			else
				PlaySound SoundFX("LFlip",DOFFlippers):LeftFlipper.RotateToEnd
'				Controller.Switch(81) = 1 
		end if
	else
		LeftFlipper.RotateToStart 
'		Controller.Switch(81) = 0
		glove.RotateToStart
         PlaySound SoundFX("LFlipd",DOFFlippers)
	End If
  End Sub

Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("RFlip",DOFFlippers)
		RightFlipper.RotateToEnd
		RightFlipper1.RotateToEnd
' 		controller.Switch(82)=1 '47
     Else
         RightFlipper.RotateToStart
		RightFlipper1.RotateToStart
' 		controller.Switch(82)=0
         PlaySound SoundFX("RFlipd",DOFFlippers)
     End If

End Sub

'Sub SolURflipper(Enabled)
'     If Enabled Then
'         RightFlipper1.RotateToEnd
' 		controller.Switch(82)=1 '47
'     Else
'         RightFlipper1.RotateToStart
' 		controller.Switch(82)=0
'     End If
'
'end Sub

Sub SolRelease(Enabled)
	If Enabled Then 
		bsTrough.ExitSol_On
        vpmCreateBall BallRelease
	bsBallRelease.AddBall 0
    End If
End Sub

Sub BallRelease_unhit
	ballinplay = ballinplay + 1
	'if ballinplay = 1 then GION:end if
End Sub

  Sub SolLanes(enabled)
	if enabled then 
		FLaneL1.state = 1
		FLaneL.state = 1
		FLaneL12.state = 1
		FLaneL2.state = 1
	Else
		FLaneL1.state = 0
		FLaneL.state = 0
		FLaneL12.state = 0
		FLaneL2.state = 0
	end if
  End Sub

  Sub Solflash18(Enabled)
	if enabled then 
		f18.state = 1
		f18l.state = 1
	Else
		f18.state = 0		
		f18l.state = 0
	end if
  End Sub

  Sub Solflash19(Enabled)
	if enabled then 

		FlashLevel2 = 1 : FlasherFlash2_Timer 'Flupper's command to flash
'		f17l.state = 1
		f17lint.state = 1
		F17flashsecA.amount = 70
		F17FlashsecA.intensityScale = 1
		F17flashsecB.amount = 70
		F17FlashsecB.intensityScale = 1
		F17flashsecC.amount = 70
		F17FlashsecC.intensityScale = 1
		F17flashsecD.amount = 70
		F17FlashsecD.intensityScale = 1
		F17flashsecE.amount = 70
		F17FlashsecE.intensityScale = 1
	Else
		FlashLevel2 = 0
'		f17l.state = 0
		f17lint.state = 0
		F17flashsecA.amount = 0
		F17FlashsecA.intensityScale = 0
		F17flashsecB.amount = 0
		F17FlashsecB.intensityScale = 0
		F17flashsecC.amount = 0
		F17FlashsecC.intensityScale = 0
		F17flashsecD.amount = 0
		F17FlashsecD.intensityScale = 0
		F17flashsecE.amount = 0
		F17FlashsecE.intensityScale = 0
	end if
  End Sub

  Sub SolF24(Enabled)
	if enabled then 
		FlashLevel1 = 1 : FlasherFlash1_Timer 'Flupper's command to flash
'		f19l.state = 1
		f19lint.state = 1
		F19flashsecA.amount = 70
		F19FlashsecA.intensityScale = 1
		F19flashsecB.amount = 70
		F19FlashsecB.intensityScale = 1
		F19flashsecC.amount = 70
		F19FlashsecC.intensityScale = 1
		F19flashsecD.amount = 70
		F19FlashsecD.intensityScale = 1
	Else
		FlashLevel1 = 0
'		f19l.state = 0
		f19lint.state = 0
		F19flashsecA.amount = 0
		F19FlashsecA.intensityScale = 0
		F19flashsecB.amount = 0
		F19FlashsecB.intensityScale = 0
		F19flashsecC.amount = 0
		F19FlashsecC.intensityScale = 0
		F19flashsecD.amount = 0
		F19FlashsecD.intensityScale = 0
	end if
  End Sub

  Sub SolF22(Enabled)
	if enabled then 
		Flash22.state = 1
		Flash22b.state = 1
		setlamp 122, 1
		
	Else
		Flash22.state = 0
		Flash22b.state = 0
		setlamp 122, 0
	end if
  End Sub

  Sub SolHouse(enabled)
	if enabled then 
		F25L.state = 1
		F25L1.state = 1
	Else
		F25L.state = 0
		F25L1.state = 0
	end if
  End Sub

  Sub SolFreddy(enabled)
	if enabled then 
		Flash21.state = 1
		Flash21a.state = 1
		Flash21b.state = 1
	Else
		Flash21.state = 0
		Flash21a.state = 0
		Flash21b.state = 0
	end if
  End Sub


  Sub solGOR(aon)
	FastFlips.TiltSol aOn
	If aon = true then
		GION
	else
		GIOFF
	end if
  End Sub


'Solenoid top up kicker
Dim aBall,aZpos, KickSpeed

'****** VUK ********
 KickSpeed = 10
 
 Sub solTUK(Enabled)
 	If Enabled then
 		PlaySound SoundFX("scoopexit",DOFContactors)
		UpKTimer.Interval = 2			'Set timer to 1ms
		UpKTimer.Enabled = True
 	end if
 End Sub

  Sub UpKTimer_Timer()
	aBall.Z=aZpos					'Move the ball Z position to the value of variable aZpos
	aZpos=aZpos+KickSpeed			'Add <Kickspeed> from ball to Z position and repeat the line above
	If aZpos>110 Then				'If the ball Z position is at 220 then,
		UpKTimer.Enabled=0			'disable the timer and use the normal kicker code
		bsVUKT.ExitSol_On
	End If
 End Sub
'******* END VUK *********


'************************* Solenoid Glove *********************
 Dim FGA:FGA=0
  Sub SolGloveFlipper(enabled)
	If enabled then 
		FGA=1
	Else
		FGA=0
	End If 
  End Sub
'**************************************************************


'************************* Solenoid Jaw ***********************
  'Solenoid jaw
	Dim JawDir, Jpos, JActive:JawDir=1: Jpos=0: JActive=0
  Sub SolMouth(enabled)
	If enabled then
		jawMid.Enabled=1:JawDir=1:JActive=1  
	else
		if JActive = 0 then 
			jawMid.Enabled=0:Jpos=0
			JawP.z=110:JActive=0:
			If opt = 1 then 
				Primitive6.z=20
			Else
				Primitive6.z=20
			End If
		Else	
		jawMid.Enabled=1
		end if
	end if
  End Sub

  Sub jawMid_Timer()
	Select Case Jpos
		Case 0: JawP.z=100
			if opt = 1 then Primitive6.z=24:end if
		Case 1: JawP.z=91
			if opt = 1 then Primitive6.z=20:end if
		Case 2: JawP.z=86
			if opt = 1 then Primitive6.z=16:end if
		Case 3: JawP.z=77
			if opt = 1 then Primitive6.z=14:end if
		Case 4: JawP.z=72
			if opt = 1 then Primitive6.z=18:end if
		Case 5: JawP.z=67
			if opt = 1 then Primitive6.z=24:end if
		Case 6: JawP.z=62
			if opt = 1 then Primitive6.z=20:end if
		Case 7: JawP.z=59
			if opt = 1 then Primitive6.z=16:end if
		Case 8: JawP.z=57
			if opt = 1 then Primitive6.z=14:end if
		Case 9: JawP.z=56
			if opt = 1 then Primitive6.z=18:end if

	End Select
	Jpos=Jpos+JawDir
	If Jpos=10 Then 
		Jpos=9:JawDir=-1
	End If
	If Jpos=-1 Then 
		Jpos=0:JawDir=1:Jactive = 0
		jawMid.Enabled=0
	End If
  End Sub
'**************************************************************


'******************** Claw Save *************************

Sub SolClawSave(enabled)
	if clawOn = 1 then 
		FclawR.enabled = 1
		FclawL.enabled = 1
		FClaw.rotatetoend
		Lightclaw.state = 1
	Flasherflash3.amount = 70
	Flasherflash3.intensityScale = 1
		PlaySound SoundFX("claw",DOFContactors)
	end if
	if clawOn = 0 then 
		FclawR.enabled = 0
		FclawL.enabled = 0
		FClaw.rotatetostart
		Lightclaw.state = 0
	Flasherflash3.amount = 0
	Flasherflash3.intensityScale = 0
	end if
End Sub

'******************* End claw save **********************


Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
 End Sub



  Sub SolKickBack(enabled)
	If enabled then
		kickback.Fire:PlaySound SoundFX("Solenoid",DOFcontactors)
		kickbackwall.TimerEnabled=1
	End If
  End Sub

  Sub kickbackwall_Timer()
	'kickbackwall.IsDropped=1 'I let it to prevent a no working kickback. Old vp
	kickback.PullBack
	kickbackwall.TimerEnabled=0
  End Sub

  'Diverters\
  Dim TDP, BDP,TDIR,BDIR:TDP=0:BDP=0:TDIR=1:BDIR=1
  'Top Diverter


'******************** Doors Diverters ***********************

  Sub SolDivTop(enabled)
	If Enabled then
		TDClosed.IsDropped=1:TDOpen.IsDropped=0:TDIR=1
		TDClosed.TimerEnabled=1
	Else
		TDClosed.IsDropped=0:TDOpen.IsDropped=1:TDIR=-1
		TDClosed.TimerEnabled=1
	End If
  End Sub

  Sub TDClosed_Timer()
	Select Case TDP
		Case 0: Door02.ObjRotz = 245:TDA0.IsDropped=0:TDA1.IsDropped=1:TDA2.IsDropped=1
		Case 1: Door02.ObjRotz = 262:TDA0.IsDropped=1:TDA1.IsDropped=0:TDA2.IsDropped=1
		Case 2: Door02.ObjRotz = 285:TDA0.IsDropped=1:TDA1.IsDropped=1:TDA2.IsDropped=0
	End Select
	TDP=TDP+TDIR
	If TDP=3 Then 
		TDClosed.TimerEnabled=0:TDP=2
	End If
	If TDP=-1 Then 
		TDClosed.TimerEnabled=0:TDP=0
	End If
  End Sub
		
  'Bottom Divertert
  Sub SolDivBot(enabled)
	If Enabled then
		RDOpen.IsDropped=0:RDClosed.IsDropped=1
		RDClosed.TimerEnabled=1:BDIR=1
	Else
		RDOpen.IsDropped=1:RDClosed.IsDropped=0
		RDClosed.TimerEnabled=1:BDIR=-1
	End If
  End Sub

  Sub RDClosed_Timer()
	Select Case BDP
		Case 0: Door01.ObjRotz = 0:RD0.IsDropped=0:RD1.IsDropped=1:RD2.IsDropped=1
		Case 1: Door01.ObjRotz = -12:RD0.IsDropped=1:RD1.IsDropped=0:RD2.IsDropped=1
		Case 2: Door01.ObjRotz = -24:RD0.IsDropped=1:RD1.IsDropped=1:RD2.IsDropped=0
	End Select
	BDP=BDP+BDIR
	If BDP=3 Then 
		RDClosed.TimerEnabled=0:BDP=2
	End If
	If BDP=-1 Then 
		RDClosed.TimerEnabled=0:BDP=0
	End If
  End Sub

'******************** End Doors Diverters ***********************



'***********************************************************************
'**************************    Flipper Subs    *************************

'       SolCallback(sLRFlipper) = "SolRFlipper"
'       SolCallback(sLLFlipper) = "SolLFlipper"
   
'       Sub SolLFlipper(Enabled)
'           If Enabled Then
'               PlaySound "LFlip":LeftFlipper.RotateToEnd
' 			  
'           Else
'               PlaySound "LFlipD":LeftFlipper.RotateToStart
' 			   
'           End If
'       End Sub
'     
'       Sub SolRFlipper(Enabled)
'           If Enabled Then
'               PlaySound "RFlip":RightFlipper.RotateToEnd:RightFlipper1.RotateToEnd
' 			  
'           Else
'               PlaySound "RFlipD":RightFlipper.RotateToStart:RightFlipper1.RotateToStart
' 			   
'           End If
'       End Sub

 'Drains and Kickers
  Dim BIT:BIT=0
	Sub Drain_Hit():PlaySound "Drain"
		ballinplay = ballinplay - 1
		ClearBallID:bsTrough.AddBall Me:
	'	if ballinplay = 0 then GIOFF:end If
	End Sub

   Sub sw21_Hit():PlaySound "kicker_enter":Set aBall=ActiveBall:aZpos=90:bsVUKT.AddBall 0:End Sub
   Sub sw20_Hit():PlaySound "kicker_enter":bsVUKB.AddBall 0:End Sub
   Sub SubwayEnter_Hit():PlaySound "Drain":ClearBallID:SubwayEnter.Destroyball:BIT=BIT+1:Me.TimerEnabled=1:End Sub
   Sub SubwayEnter_Timer()
		vpmTimer.PulseSw 100
		subwayExit.CreateBall:subwayExit.Kick 135, 2
		BIT=BIT-1
		If BIT=0 then SubwayEnter.TimerEnabled=0
   End Sub
   Sub sw80_Hit():ClearBallID:sw80.destroyball:bsFreddyHead.AddBall me:sw80a.Kick 130, 10:End Sub
   Sub sw80a_Unhit():PlaySound"DROP_RIGHT":End Sub



'*********************************************************************
'*************************    Slingshots    **************************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySound SoundFX("Rsling",DOFContactors), 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    vpmTimer.PulseSw 13:RStep = 0:RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 2:RSLing1.Visible = 0:RSLing2.Visible = 0:RSLing3.Visible = 1:sling1.TransZ = -10
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:RSLing3.Visible = 0:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1

if RSLing1.Visible = 0 then 
RSLing1.Visible = 0 
end if 
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("Lsling",DOFContactors),0,1,-0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    vpmTimer.PulseSw 12:LStep = 0:LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 2:LSLing1.Visible = 0:LSLing2.Visible = 0:LSLing3.Visible = 1:sling2.TransZ = -10
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:LSLing3.Visible = 0:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1

if LSLing1.Visible = 0 then 
RSLing1.Visible = 0
End If
End Sub
'*********************************************************************



 ' Captive Ball 
 Sub captTrig_Hit:cb.TrigHit ActiveBall:End Sub
 Sub captTrig_UnHit:cb.TrigHit 0:End Sub
 Sub captwall_Hit:PlaySound "collide4":cb.BallHit ActiveBall:End Sub
 Sub Capt_Hit:cb.BallReturn Me:End Sub

  


'********************************************************** 
'************************ Bumpers *************************
'**********************************************************
Dim hatdir, ht
	ht = 0

Sub Bumper1_Hit
    vpmTimer.PulseSw 10 
	bumpersounds()
	Me.TimerEnabled = 1
	hatdir = 1
	ht=0:
	if opt = 1 then 
		hat.enabled = 1
	Else
		hat.enabled = 0
	end if
End Sub

Sub Bumper1_Timer
	Me.Timerenabled = 0
End Sub

Sub Bumper2_Hit
    vpmTimer.PulseSw 11
	bumpersounds()
	Me.TimerEnabled = 1
	hatdir=-1
	ht=6
if opt = 1 then
	hat.enabled = 1
	Else
	hat.enabled = 0
end if
End Sub

Sub Bumper2_Timer
	Me.Timerenabled = 0
End Sub	

sub hat_timer()
	select case ht
		Case 0 : Primitive4.objrotx = -2:Primitive4.objroty = 2:Primitive6.z=23
		Case 1 : Primitive4.objrotx = 0:Primitive4.objroty = 0:Primitive6.z=25
		Case 2 : Primitive4.objrotx = 1:Primitive4.objroty = -1:Primitive6.z=26
		Case 3 : Primitive4.objrotx = 2:Primitive4.objroty = -2:Primitive6.z=27
		Case 4 : Primitive4.objrotx = 0:Primitive4.objroty = 0:Primitive6.z=24
		Case 5 : Primitive4.objrotx = -2:Primitive4.objroty = 2:Primitive6.z=22
		Case 6 : Primitive4.objrotx = -1:Primitive4.objroty = 1:Primitive6.z=20
	end Select
	ht=ht + hatdir
	if ht = -1 then hat.enabled = 0:ht=6:hatdir=-1:Primitive4.objrotx = -3:Primitive4.objroty = 3:Primitive6.z=20
	if ht = 7 then hat.enabled = 0:ht=0:hatdir=1:Primitive4.objrotx = -3:Primitive4.objroty = 3:Primitive6.z=20
End Sub

sub BumperSounds()
	Select Case Int(Rnd*3)
		Case 0 : PlaySound SoundFX("bumper1",DOFContactors)
		Case 1 : PlaySound SoundFX("bumper2",DOFContactors)
		Case 2 : PlaySound SoundFX("bumper2",DOFContactors)
	End Select
End Sub

 
   'Rollover & Ramp Switches
 Sub sw92_Hit:Controller.Switch(92) = 1:PlaySound "rollover":End Sub
 Sub sw92_UnHit:Controller.Switch(92) = 0:End Sub
 
 Sub sw93_Hit:Controller.Switch(93) = 1:PlaySound "rollover":End Sub
 Sub sw93_UnHit:Controller.Switch(93) = 0:End Sub
 
 Sub sw102_Hit:Controller.Switch(102) = 1:PlaySound "rollover":End Sub
 Sub sw102_UnHit:Controller.Switch(102) = 0:End Sub
 
 Sub sw103_Hit:Controller.Switch(103) = 1:PlaySound "rollover":End Sub
 Sub sw103_UnHit:Controller.Switch(103) = 0:End Sub
 
 Sub sw90_Hit:Controller.Switch(90) = 1:End Sub
 Sub sw90_UnHit:Controller.Switch(90) = 0:End Sub

 Sub sw25_Hit:Controller.Switch(25) = 1:BallinPlunger = 1:PRefresh.state =1:End Sub
 Sub sw25_UnHit:Controller.Switch(25) = 0:BallinPlunger = 0:PRefresh.state =0:End Sub


' **************************************************************************
' ****************** Kicking Targets ***************************************

Dim KT1Step, KT2step, KT3step

Sub SW16_Slingshot
    PlaySound SoundFX("Rsling",DOFContactors),0,1,-0.2,0
	sw16p.ObjRotY=4
    sw16p.TransX = -4
    vpmTimer.PulseSw 16
	KT1Step = 0
	SW16.TimerEnabled = 1
End Sub

Sub SW16_Timer
    Select Case KT1Step
        Case 0:sw16p.TransX = 8:sw16p.ObjRotY=-4
        Case 1:sw16p.TransX = 4:sw16p.ObjRotY=-2
        Case 2:sw16p.TransX = 0:sw16p.ObjRotY=0:SW16.TimerEnabled = 0
    End Select
    KT1Step = KT1Step + 1
End Sub

Sub SW15_Slingshot
    PlaySound SoundFX("Rsling",DOFContactors),0,1,-0.2,0
	sw15p.ObjRotY=4
    sw15p.TransX = -4
    vpmTimer.PulseSw 15
	KT2Step = 0
	SW15.TimerEnabled = 1
End Sub

Sub SW15_Timer
    Select Case KT2Step
        Case 0:sw15p.TransX = 8:sw15p.ObjRotY=-4
        Case 1:sw15p.TransX = 4:sw15p.ObjRotY=-2
        Case 2:sw15p.TransX = 0:sw15p.ObjRotY=0:SW15.TimerEnabled = 0
    End Select
    KT2Step = KT2Step + 1
End Sub

 Sub SW14_Slingshot
    PlaySound SoundFX("Lsling",DOFContactors),0,1,-0.2,0
	sw14p.ObjRotY=4
    sw14p.TransX = -4
    vpmTimer.PulseSw 14
	KT3Step = 0
	SW14.TimerEnabled = 1
End Sub

Sub SW14_Timer
    Select Case KT3Step
        Case 0:sw14p.TransX = 8:sw14p.ObjRotY=-4
        Case 1:sw14p.TransX = 4:sw14p.ObjRotY=-2
        Case 2:sw14p.TransX = 0:sw14p.ObjRotY=0:SW14.TimerEnabled = 0
    End Select
    KT3Step = KT3Step + 1
End Sub

' **************************************************************************
' ****************** Freddy's Head *****************************************
 Sub sw26_Hit:vpmTimer.PulseSw 26:PlaySound SoundFX("target",DOFTargets):End Sub


' **************************************************************************
' ****************** Freddy's Targets **************************************
Sub sw96_Hit:vpmTimer.PulseSw 96:PlaySound SoundFX("target",DOFTargets)
	if opt = 1 then eyetarget.enabled = 1:end If
End Sub
Sub sw106_Hit:vpmTimer.PulseSw 106:PlaySound SoundFX("target",DOFTargets)
	if opt = 1 then eyetarget.enabled = 1:end If
End Sub
Sub sw97_Hit:vpmTimer.PulseSw 97:PlaySound SoundFX("target",DOFTargets)
if opt = 1 then eyetarget.enabled = 1:end If
End Sub
Sub sw107_Hit:vpmTimer.PulseSw 107:PlaySound SoundFX("target",DOFTargets)
if opt = 1 then eyetarget.enabled = 1:end If
End Sub

Dim eyet
eyet=0
sub eyetarget_timer()
	select case eyet
		case 0 :Primitive6.z=17
		case 1 :Primitive6.z=15
		case 2 :Primitive6.z=18
		case 3 :Primitive6.z=20
	end Select
Primitive6.blenddisablelighting = 1
	eyet=eyet + 1
	if eyet = 4 then eyet=0:eyetarget.enabled = 0:Primitive6.blenddisablelighting = 0
end Sub


 Sub boilerdoor_Hit(idx):vpmTimer.PulseSw 110:PlaySound SoundFX("target",DOFShaker):End Sub


' **************************************************************************
' ****************** Standup Targets ***************************************
Sub SW94_hit():sw94p.transx = -10:Me.TimerEnabled = 1:PlaySound SoundFX("target",DOFTargets):vpmTimer.PulseSw 94:End Sub
Sub SW94_Timer():sw94p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW95_hit():sw95p.transx = -10:Me.TimerEnabled = 1:PlaySound SoundFX("target",DOFTargets):vpmTimer.PulseSw 95:End Sub
Sub SW95_Timer():sw95p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW104_hit():sw104p.transx = -10:Me.TimerEnabled = 1:PlaySound SoundFX("target",DOFTargets):vpmTimer.PulseSw 104:End Sub
Sub SW104_Timer():sw104p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW105_hit():sw105p.transx = -10:Me.TimerEnabled = 1:PlaySound SoundFX("target",DOFTargets):vpmTimer.PulseSw 105:End Sub
Sub SW105_Timer():sw105p.transx = 0:Me.TimerEnabled = 0:End Sub


' **************************************************************************
' ****************** Drop Targets ******************************************

	dim sw17dir, sw27Dir, sw37Dir
	dim sw17Pos, sw27Pos, sw37Pos
 
	sw17Dir = 1:sw27Dir = 1:sw37Dir = 1
	sw17Pos = 0:sw27Pos = 0:sw37Pos = 0

  'Targets Init
	sw17a.TimerEnabled = 1:sw27a.TimerEnabled = 1:sw37a.TimerEnabled = 1


  Sub sw17_Hit:DTBank.Hit 1:sw17Dir = 0:sw17a.TimerEnabled = 1:sw1727.isdropped = 1:End Sub
  Sub sw27_Hit:DTBank.Hit 2:sw27Dir = 0:sw27a.TimerEnabled = 1:sw1727.isdropped = 1:sw2737.isdropped = 1:End Sub
  Sub sw37_Hit:DTBank.Hit 3:sw37Dir = 0:sw37a.TimerEnabled = 1:sw2737.isdropped = 1:End Sub
  Sub sw1727_Hit
		sw1727.isdropped = 1:sw2737.isdropped = 1
		DTBank.Hit 1:sw17Dir = 0:sw17a.TimerEnabled = 1
		DTBank.Hit 2:sw27Dir = 0:sw27a.TimerEnabled = 1
	End Sub

  Sub sw2737_Hit
		:sw1727.isdropped = 1:sw2737.isdropped = 1
		DTBank.Hit 2:sw27Dir = 0:sw27a.TimerEnabled = 1
		DTBank.Hit 3:sw37Dir = 0:sw37a.TimerEnabled = 1
	End Sub


 Sub sw17a_Timer()	

  Select Case sw17Pos
        Case 0: sw17P.z=24
				If sw17Dir = 1 then 
					sw17a.TimerEnabled = 0
				else
					sw17Dir = 0
					sw17a.TimerEnabled = 1
				end if
        Case 1: sw17P.z=26
        Case 2: sw17P.z=29
        Case 3: sw17P.z=26
        Case 4: sw17P.z=22
        Case 5: sw17P.z=18
        Case 6: sw17P.z=14
        Case 7: sw17P.z=10
        Case 8: sw17P.z=6
        Case 9: sw17P.z=2
        Case 10: sw17P.z=-4
        Case 11: sw17P.z=-10
        Case 12: sw17P.z=-16
        Case 13: sw17P.z=-19:sw17P.ReflectionEnabled = true
        Case 14: sw17P.z=-22:sw17P.ReflectionEnabled = false
				 If sw17Dir = 1 then
				 else
					sw17a.TimerEnabled = 0
			     end if
End Select
	If sw17Dir = 1 then
		If sw17pos>0 then sw17pos=sw17pos-1
	else
		If sw17pos<14 then sw17pos=sw17pos+1
	end if
  End Sub





 Sub sw27a_Timer()	
  Select Case sw27Pos
        Case 0: sw27P.z=24
				 If sw27Dir = 1 then
					sw27a.TimerEnabled = 0
				 else
					sw27Dir = 0
					sw27a.TimerEnabled = 1
			     end if        
        Case 1: sw27P.z=26
        Case 2: sw27P.z=29
        Case 3: sw27P.z=26
        Case 4: sw27P.z=22
        Case 5: sw27P.z=18
        Case 6: sw27P.z=14
        Case 7: sw27P.z=10
        Case 8: sw27P.z=6
        Case 9: sw27P.z=2
        Case 10: sw27P.z=-4
        Case 11: sw27P.z=-10
        Case 12: sw27P.z=-16
        Case 13: sw27P.z=-19:sw27P.ReflectionEnabled = true
        Case 14: sw27P.z=-22:sw27P.ReflectionEnabled = false
				 If sw27Dir = 1 then
				 else
					sw27a.TimerEnabled = 0
			     end if
End Select
	If sw27Dir = 1 then
		If sw27pos>0 then sw27pos=sw27pos-1
	else
		If sw27pos<14 then sw27pos=sw27pos+1
	end if
  End Sub


 Sub sw37a_Timer()	
  Select Case sw37Pos
        Case 0: sw37P.z=24
				 If sw37Dir = 1 then
					sw37a.TimerEnabled = 0
				 else
					sw37Dir = 0
					sw37a.TimerEnabled = 1
			     end if        
        Case 1: sw37P.z=26
        Case 2: sw37P.z=29
        Case 3: sw37P.z=26
        Case 4: sw37P.z=22
        Case 5: sw37P.z=18
        Case 6: sw37P.z=14
        Case 7: sw37P.z=10
        Case 8: sw37P.z=6
        Case 9: sw37P.z=2
        Case 10: sw37P.z=-4
        Case 11: sw37P.z=-10
        Case 12: sw37P.z=-16
        Case 13: sw37P.z=-19:sw37P.ReflectionEnabled = true
        Case 14: sw37P.z=-22:sw37P.ReflectionEnabled = false
				 If sw37Dir = 1 then
				 else
					sw37a.TimerEnabled = 0
			     end if
End Select
	If sw37Dir = 1 then
		If sw37pos>0 then sw37pos=sw37pos-1
	else
		If sw37pos<14 then sw37pos=sw37pos+1
	end if
  End Sub



'******* DT Subs jpj - Add timer before the drop targets will respawn *******

   Sub ResetDrops(Enabled)
		If Enabled Then
			resetD.interval = 250
			resetD.enabled = 1
		End if
   End Sub

sub resetD_timer
			sw17Dir = 1:sw27Dir = 1:sw37Dir = 1
			sw17a.TimerEnabled = 1:sw27a.TimerEnabled = 1:sw37a.TimerEnabled = 1
			sw1727.isdropped = 0:sw2737.isdropped = 0
			DTBank.DropSol_On
			resetD.enabled=0
end Sub

' **************************************************************************



 '*************************
 ' Plunger 
 '*************************
dim plb, plt 'plunger bottom plunger top


Sub swPlunger_Hit:BallinPlunger = 1:End Sub  'in this sub you may add a switch, for example Controller.Switch(14) = 1

Sub swPlunger_UnHit:BallinPlunger = 0:playsound "launchball01":plb=1:End Sub    
                      'in this sub you may add a switch, for example Controller.Switch(14) = 0




 '======================
 'Options Menu Variables
 '======================

' Const cRegistryName = "FreddyUW"
' Const cOptNoFade    = &H08
' Const cOptNoStartM	= &H10
'
' 
' Dim FreddyOptions
' 
' Private Sub FreddyShowDips
' 	If Not IsObject(vpmDips) Then ' First time
' 		Set vpmDips = New cvpmDips
' 		With vpmDips
' 			.AddForm 100, 100, "Freddy Game Settings"
' 	  		.AddFrameExtra 0,55,145,"Disable",0,_
' 				Array("Menu At Start", cOptNoStartM,"Alpha Flashers", cOptNoFade)
' 		End With
' 	End If
' 	FreddyOptions = vpmDips.ViewDipsExtra(FreddyOptions)
' 	FreddySetOptions : SaveValue cRegistryName, "Options", FreddyOptions
' End Sub
' Set vpmShowDips = GetRef("FreddyShowDips")
 
 '=========================
 ' Handle Freddy Options
 '=========================				
' Dim AlphaF    : AlphaF	 = True
' 
' 
' Sub FreddySetOptions
' 	If freddyOptions And cOptNoFade Then
' 		AlphaF = False 
' 	Else
' 		AlphaF = True
' 	End If
' End Sub	


 
 '****************************************
  '  JP's Fading Lamps 3.5 VP9 Fading only
  '      Based on PD's Fading Lights
  ' SetLamp 0 is Off
  ' SetLamp 1 is On
  ' LampState(x) current state
  '****************************************
  Dim x

Dim FadingState(130), LampState(130)

  
  AllLampsOff()
  LampTimer.Interval = 30
  LampTimer.Enabled = 1
  
Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
            FadingState(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
        Next
    End If

    UpdateLamps
End Sub
 
 Sub UpdateLamps
     nFadeL 0, l0
     'FadeL 1, l, la  'credit button
     'FadeL 2, 2, 2a  'not used
	FadeLm 3, Amp1
	FadeAR 3, Ampoule01, "AmpRougeOff", "AmpRougeOnb", "AmpRougeOna", "AmpRougeOn"
	FadeLm 4, Amp2
	FadeAR 4, Ampoule02, "AmpVertOff", "AmpVertOnb", "AmpVertOna", "AmpVertOn"
	FadeLm 5, Amp3
	FadeAR 5, Ampoule03, "AmpVertOff", "AmpVertOnb", "AmpVertOna", "AmpVertOn"
	FadeLm 6, Amp4
	FadeAR 6, Ampoule04, "AmpVertOff", "AmpVertOnb", "AmpVertOna", "AmpVertOn"
	FadeLm 7, Amp5
	FadeAR 7, Ampoule05, "AmpVertOff", "AmpVertOnb", "AmpVertOna", "AmpVertOn"
	 FadeL 10, l10, l10In
	 FadeL 11, l11, l11In
	if sw17dir = 0 and l11.state = 1 then 
		l11dt1.State =1
	Else	
		l11dt1.State =0
	end if
	if sw27dir = 0 and l11.state = 1 then 
		l11dt2.State =1
	Else	
		l11dt2.State =0
	end if
	if sw37dir = 0 and l11.state = 1 then 
		l11dt3.State =1
	Else	
		l11dt3.State =0
	end if

     'nFadeL 12
     nFadeL 13, l13
     nFadeL 14, l14
     nFadeL 15, l15
     nFadeL 16, l16
     nFadeL 17, l17

     'nFadeL 21, l21
     'nFadeL 22, l22
     nFadeL 23, l23
     nFadeL 24, l24
     nFadeL 25, l25
     nFadeL 26, l26
     nFadeL 27, l27

     nFadeL 30, l30
     nFadeL 31, l31
     nFadeL 32, l32
     nFadeL 33, l33
     nFadeL 34, l34
     nFadeL 35, l35
     nFadeL 36, l36
     nFadeL 37, l37

     nFadeL 40, l40
     nFadeL 41, l41
     nFadeL 42, l42
     nFadeL 43, l43
     'nFadeL 44, l44  'not used
     nFadeL 45, l45
     nFadeL 46, l46
     nFadeL 47, l47

     nFadeL 50, l50
     nFadeL 51, l51
     nFadeL 52, l52
     nFadeL 53, l53
     nFadeL 54, l54
     nFadeL 55, l55
     nFadeL 56, l56
     nFadeL 57, l57

     nFadeL 60, l60
     nFadeL 61, l61
     nFadeL 62, l62
     nFadeL 63, l63
     'nFadeL 64, l64  'not used
     nFadeL 65, l65
     nFadeL 66, l66
     nFadeL 67, l67

     nFadeL 70, l70
     nFadeL 71, l71
     nFadeL 72, l72
     FadeL 73, l73, l73ref
     FadeL 74, l74, l74ref  
     nFadeL 75, l75
     nFadeL 76, l76
     nFadeL 77, l77

     nFadeL 85, l85
     nFadeL 86, l86
     nFadeL 87, l87

     FadeL 91, l91, l91p
     FadeL 92, l92, l92p
     FadeL 93, l93, l93p
     FadeL 94, l94, l94p  
     FadeL 95, l95, l95p
     FadeL 96, l96, l96p
     FadeLm 97, l97p2
     FadeLm 97, l97p3
     FadeLm 97, l97p4
     FadeLm 97, l97p5
     FadeLm 97, l97p6
     FadeL 97, l97, l97p1

	If NOT DesktopMode then
			 NfadeL 103, l103
			 NfadeL 104, l104
			 NfadeL 105, l105
			 NfadeL 106, l106
			 NfadeL 107, l107
			 NfadeL 113, l113
			 NfadeL 114, l114
			 NfadeL 115, l115
			 NfadeL 116, l116
			 NfadeL 117, l117
		else
			 NfadeL 103, l103d
			 NfadeL 104, l104d
			 NfadeL 105, l105d
			 NfadeL 106, l106d
			 NfadeL 107, l107d
			 NfadeL 113, l113d
			 NfadeL 114, l114d
			 NfadeL 115, l115d
			 NfadeL 116, l116d
			 NfadeL 117, l117d
	End if
     nFadeL 119, f18

     'NfadeL 122, flash22
if flash22.state=1 then
		Flash22r.amount = 180
		Flash22r.intensityScale = 1
		Flash22r1.amount = 180
		Flash22r1.intensityScale = 1
	Else
		Flash22r.amount = 0
		Flash22r.intensityScale = 0
		Flash22r1.amount = 0
		Flash22r1.intensityScale = 0
end if

	FadeDisableLighting 122, Primitive87, 1

     FadeL 123, flash21, flash21a

  '   FadeWCol 126, Cabin, Cabina, Cabinb

 End Sub

Sub SideOff
	If NOT DesktopMode then
		if sides = 1 then
			sideLOn.visible = 0
			sideLOff.visible = 1
			sideROn.visible = 0
			sideROff.visible = 1
		end if
	end if
end Sub

Sub SideOn
	If NOT DesktopMode then
		if sides = 1 then
			sideLOn.visible = 1
			sideLOff.visible = 0
			sideROn.visible = 1
			sideROff.visible = 0
		end if
	end if
end Sub

Sub RainbowTimer_Timer 'rainbow led light color changing

RGBFactor =20
    Select Case RGBStep
        Case 0 'Green
            rGreen = rGreen + RGBFactor
            If rGreen > 255 then
                rGreen = 255
                RGBStep = 1
            End If
        Case 1 'Red
            rRed = rRed - RGBFactor
            If rRed < 0 then
                rRed = 0
                RGBStep = 2
            End If
        Case 2 'Blue
            rBlue = rBlue + RGBFactor
            If rBlue > 255 then
                rBlue = 255
                RGBStep = 3
            End If
        Case 3 'Green
            rGreen = rGreen - RGBFactor
            If rGreen < 0 then
                rGreen = 0
                RGBStep = 4
            End If
        Case 4 'Red
            rRed = rRed + RGBFactor
            If rRed > 255 then
                rRed = 255
                RGBStep = 5
            End If
        Case 5 'Blue
            rBlue = rBlue - RGBFactor
            If rBlue < 0 then
                rBlue = 0
                RGBStep = 0
            End If
    End Select
  '      For each obj in RainbowLights
   '         obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
  '          obj.colorfull = RGB(rRed, rGreen, rBlue)
  '      Next
            test.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
            test.colorfull = RGB(rRed, rGreen, rBlue)
            test1.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
            test1.colorfull = RGB(rRed, rGreen, rBlue)
            test2.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
            test2.colorfull = RGB(rRed, rGreen, rBlue)
	
End Sub
 
Sub AllLampsOff()
	Dim x
	For x = 0 to 130
		LampState(x) = 0
		FadingState(x) = 4
	Next
UpdateLamps:'UpdateLamps:Updatelamps
End Sub

Sub SetLamp(nr, value)
	If value <> LampState(nr) Then
		LampState(nr) = abs(value)
		FadingState(nr) = abs(value) + 4
	End If
End Sub

Sub FadeAR(nr, ramp, a, b, c, d)
	Select Case FadingState(nr)
		Case 2:ramp.image = a:FadingState(nr) = 0 'Off
		Case 3:ramp.image = c:FadingState(nr) = 2 'fading...
		Case 4:ramp.image = b:FadingState(nr) = 3 'fading...
		Case 5:ramp.image = d:FadingState(nr) = 1 'ON
	End Select
End Sub
  
Sub FadeL(nr, a,b)
    Select Case FadingState(nr)
        Case 4:a.state = 0:b.state = 0:FadingState(nr) = 0
        Case 5:a.state = 1:b.state = 1:FadingState(nr) = 1
    End Select
End Sub

Sub FadeLL(nr, a,b,c)
    Select Case FadingState(nr)
        Case 4:a.state = 0:b.state = 0:c.state = 0:FadingState(nr) = 0
        Case 5:a.state = 1:b.state = 1:c.state = 1:FadingState(nr) = 1
    End Select
End Sub
  
  Sub FadeLm(nr, a)
      Select Case FadingState(nr)
          Case 4:a.state = 0
          Case 5:a.state = 1
      End Select
  End Sub

  Sub NFadeL(nr, a)
      Select Case FadingState(nr)
          Case 4:a.state = 0:FadingState(nr) = 0
          Case 5:a.State = 1:FadingState(nr) = 1
      End Select
  End Sub
  
Sub FadeDisableLighting(nr, a, alvl)
	Select Case FadingState(nr)
		Case 4
			a.UserValue = a.UserValue - 0.1
			If a.UserValue < 0 Then 
				a.UserValue = 0
				FadingState(nr) = 0
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
		Case 5
			a.UserValue = a.UserValue + 0.50
			If a.UserValue > 1 Then 
				a.UserValue = 1
				FadingState(nr) = 1
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
	End Select
End Sub

'*** Red flasher - Flupper's Method ***
Sub FlasherFlash2_Timer()
	dim flashx3, matdim
	If not Flasherflash2.TimerEnabled Then 
		Flasherflash2.TimerEnabled = True
		Flasherflash2.visible = 1
		Flasherlit2.visible = 1
	End If
	flashx3 = FlashLevel2 * FlashLevel2 * FlashLevel2
	Flasherflash2.opacity = 1500 * flashx3
	Flasherlit2.BlendDisableLighting = 10 * flashx3
	Flasherbase2.BlendDisableLighting =  flashx3
	F17L.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel2)
	Flasherlit2.material = "domelit" & matdim
	FlashLevel2 = FlashLevel2 * 0.9 - 0.01
	If FlashLevel2 < 0.15 Then
		Flasherlit2.visible = 0
	Else
		Flasherlit2.visible = 1
	end If
	If FlashLevel2 < 0 Then
		Flasherflash2.TimerEnabled = False
		Flasherflash2.visible = 0
	End If
End Sub

Sub FlasherFlash1_Timer()
	dim flashx3, matdim
	If not Flasherflash1.TimerEnabled Then 
		Flasherflash1.TimerEnabled = True
		Flasherflash1.visible = 1
		Flasherlit1.visible = 1
	End If
	flashx3 = FlashLevel1 * FlashLevel1 * FlashLevel1
	Flasherflash1.opacity = 1500 * flashx3
	Flasherlit1.BlendDisableLighting = 10 * flashx3
	Flasherbase1.BlendDisableLighting =  flashx3
	F19L.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel2)
	Flasherlit1.material = "domelit" & matdim
	FlashLevel1 = FlashLevel1 * 0.9 - 0.01
	If FlashLevel1 < 0.15 Then
		Flasherlit1.visible = 0
	Else
		Flasherlit1.visible = 1
	end If
	If FlashLevel1 < 0 Then
		Flasherflash1.TimerEnabled = False
		Flasherflash1.visible = 0
	End If
End Sub
'***************************************


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

'******************************
' destruk's new vpmCreateBall
' use it: vpmCreateBall kicker
' Use it in vpm tables instead
' of CreateBallID kickername
'******************************

Set vpmCreateBall = GetRef("mypersonalcreateballroutine")
Function mypersonalcreateballroutine(aKicker)
    For cnt = 1 to ubound(ballStatus)        ' Loop through all possible ball IDs
        If ballStatus(cnt) = 0 Then            ' If ball ID is available...
        If Not IsEmpty(vpmBallImage) Then
            Set currentball(cnt) = aKicker.CreateBall.Image            ' Set ball object with the first available ID
        Else
            Set currentball(cnt) = aKicker.CreateBall
        End If
        Set mypersonalcreateballroutine = aKicker
        currentball(cnt).uservalue = cnt            ' Assign the ball's uservalue to it's new ID
        ballStatus(cnt) = 1                ' Mark this ball status active
        ballStatus(0) = ballStatus(0)+1         ' Increment ballStatus(0), the number of active balls
    If coff = False Then                ' If collision off, overrides auto-turn on collision detection
                            ' If more than one ball active, start collision detection process
    If ballStatus(0) > 1 and XYdata.enabled = False Then XYdata.enabled = True
    End If
    Exit For                    ' New ball ID assigned, exit loop
        End If
        Next
End Function
 
'****************************************
' B2B Collision by Steely & Pinball Ken
' jpsalas: added destruk's changes
'  & ball height check
'****************************************

Dim tnopb, nosf
'
tnopb = 5
nosf = 7

Dim currentball(10), ballStatus(10)
Dim iball, cnt, coff, errMessage

XYdata.interval = 1
coff = False

For cnt = 0 to ubound(ballStatus)
    ballStatus(cnt) = 0
Next

' Create ball in kicker and assign a Ball ID used mostly in non-vpm tables
Sub CreateBallID(Kickername)
    For cnt = 1 to ubound(ballStatus)
        If ballStatus(cnt) = 0 Then
			If Not IsEmpty(vpmBallImage) Then		' Set ball object with the first available ID
				Set currentball(cnt) = Kickername.Createsizedball(15*brc)
			Else
				Set currentball(cnt) = Kickername.Createsizedball(15*brc)
			end If
			'Set currentball(cnt) = Kickername.createball
            currentball(cnt).uservalue = cnt
            ballStatus(cnt) = 1
            ballStatus(0) = ballStatus(0) + 1
            If coff = False Then
                If ballStatus(0) > 1 and XYdata.enabled = False Then XYdata.enabled = True
            End If
		End If
     Exit For
        
    Next
End Sub

Sub ClearBallID
    On Error Resume Next
    iball = ActiveBall.uservalue
    currentball(iball).UserValue = 0
    'If Err Then Msgbox Err.description & vbCrLf & iball
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
            bDistance = int((TotalVel(B1, id3) + TotalVel(B2, id3) ) ^1.04)
			If ABS(baZ(B1, id3) - baZ(B2, id3)) < 50 Then
				If((baX(B1, id3) - baX(B2, id3) ) ^2 + (baY(B1, id3) - baY(B2, id3) ) ^2) < 2800 + bDistance Then collide B1, B2:Exit Sub
			End If
        End If
        B1 = B1 + 1
        If B1 >= ballStatus(0) Then Exit Do
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
    PlaySound("collide" & cForce)
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
'***************************************************************




'**************************************************************
'************************    Sounds    ************************
'**************************************************************

 'Sounds
dim rt, rb 'ramp top and ramp bottom

 dim speedx
 dim speedy
 dim finalspeed
  Sub RHSND_Hit(IDX)
 	finalspeed=SQR(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
 	if finalspeed > 11 then PlaySound "rubber" else PlaySound "rubberFlipper":end if
   End Sub
  Sub Gates_Hit(IDX):RandomSoundGates():End Sub  
  Sub LeftFlipper_Collide(parm)
	RandomSoundRubber()
   End Sub
   Sub RightFlipper_Collide(parm)
	RandomSoundRubber()
   End Sub

	sub trigentreerampebefore_unHit() 

		if rb=1 then 
			rb=0
			stopsound "sramp2"
			playsound "launchballreturn01"
		end if
	end Sub 

	sub trigentreerampe_unHit()
		if plb=1 and rb = 0 Then
			plb=0
			stopsound "launchball01"
		end If	

		if rb=0 then
			PlaySound "sramp2"
			rb=1
		end if
	end Sub 

	sub sortierampe1_unHit()
			PlaySound "DROP_RIGHT"
			stopsound "sramp2" 

			rb=0
	end Sub

	sub Ramp1_unhit()
		StopSound "sramp2"
		PlaySound"DROP_RIGHT" 
	end Sub

	Sub RampHelp_Hit():PlaySound"rail":End Sub
	Sub RampHelp1_Hit():PlaySound"DROP_RIGHT":End Sub
	Sub RampHelp2_Hit():stopsound "ramp":PlaySound"DROP_LEFT":End Sub 
	Sub RampHelp3_Hit():stopsound "ramp":PlaySound"DROP_RIGHT":End Sub 

	Sub RampHelp3_UnHit():	PlaySound"DROP_LEFT":End Sub 

	sub trigger1_hit()
		playsound "sramp2", 0, Vol(ActiveBall)*0.005*GlobalSoundLevel , AudioPan(ActiveBall), 0, Pitch(ActiveBall)*25, 0, 0
	end sub

	sub Trigger1_unhit()
		stopsound "sramp2"
	end sub

	sub trigger2_hit()
		playsound "sramp2", 0, Vol(ActiveBall)*0.005*GlobalSoundLevel , AudioPan(ActiveBall), 0, Pitch(ActiveBall)*22, 0, 0
	end sub

	sub Trigger2_unhit()
		stopsound "sramp2"
	end sub

'sub Wall36_hit()
'	msgbox activeball.z
'end sub

' Extra Sounds
'Sub Sound1_Hit:PlaySound "metalrolling":End Sub
'Sub Sound2_Hit:PlaySound "metalrolling":End Sub
'Sub Sound3_Hit:PlaySound "metalrolling":End Sub
'Sub BallRol1_Hit:PlaySound "ballrolling":End Sub
'
'
'Sub BalldropSound: PlaySound "fx_ball_drop" : End Sub
'Sub OnBallBallCollision(ball1, ball2, velocity) : PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0 : End Sub
'
'Sub Trigger1_Hit() : Stopsound "plasticrolling" : PlaySound "metalrolling", -1,GlobalSoundLevel * 0.3, 0, 0, 1, 0, 0 : End Sub
'
'Sub LeftFlipper_Collide(parm) : RandomSoundFlipper() : End Sub
'Sub RightFlipper_Collide(parm) : RandomSoundFlipper() : End Sub
'
'
'Sub Pins_Hit (idx)
'	PlaySound "plastic", 0, Vol(ActiveBall)*GlobalSoundLevel, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
'End Sub
'
'Sub Targets_Hit (idx)
'	PlaySound "target", 0, Vol(ActiveBall)*GlobalSoundLevel, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
'End Sub

Sub Metal_Hit (idx)
	RandomSoundMetal()
End Sub

'Sub MetalsThin_Hit (idx)
'	PlaySound "metalhit_thin", 0, Vol(ActiveBall)*GlobalSoundLevel, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'End Sub
'
'Sub MetalsMedium_Hit (idx)
'	PlaySound "metalhit_medium", 0, Vol(ActiveBall)*GlobalSoundLevel, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'End Sub
'
'Sub Metals2_Hit (idx)
'	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'End Sub
'
'Sub Gates_Hit (idx)
'	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'End Sub
'
'Sub Spinner1_Hit ()
'	PlaySound "fx_spinner", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'End Sub
'
'Sub Spinner2_Hit ()
'	PlaySound "fx_spinner", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*20*GlobalSoundLevel , AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
	End if
	If finalspeed >= 1 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

'Sub Posts_Hit(idx)
' 	dim finalspeed
'  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
' 	If finalspeed > 16 then 
'		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'	End if
'	If finalspeed >= 6 AND finalspeed <= 16 then
' 		RandomSoundRubber()
' 	End If
'End Sub

Sub RandomSoundMetal()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "metal_1", 0, Vol(ActiveBall)*10*GlobalSoundLevel , AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
		Case 2 : PlaySound "metal_2", 0, Vol(ActiveBall)*10*GlobalSoundLevel , AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
		Case 3 : PlaySound "metal_3", 0, Vol(ActiveBall)*10*GlobalSoundLevel , AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
	End Select
End Sub
Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*13*GlobalSoundLevel , AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*11*GlobalSoundLevel , AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*12*GlobalSoundLevel , AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
	End Select
End Sub
Sub RandomSoundGates()
	Select Case Int(Rnd*2)+1
		Case 1 : PlaySound "Gate", 0, Vol(ActiveBall)*10*GlobalSoundLevel , AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
		Case 2 : PlaySound "Gate4", 0, Vol(ActiveBall)*10*GlobalSoundLevel , AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
	End Select
End Sub
Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 :PlaySound "flip_hit_1", 0, GlobalSoundLevel * ballvel(ActiveBall) / 50, -0.1, 0.25
		Case 2 :PlaySound "flip_hit_2", 0, GlobalSoundLevel * ballvel(ActiveBall) / 50, -0.1, 0.25
		Case 3 :PlaySound "flip_hit_3", 0, GlobalSoundLevel * ballvel(ActiveBall) / 50, -0.1, 0.25
	End Select
End Sub

'*******************************************************************************************

Sub sw21h_Hit()
	ClearBallID
	me.destroyball
	sw21h1.createball
	sw21h1.kick 80,10
End Sub

Sub sw21h1_Unhit()
	playsound "ramp"
End Sub

'*****************************************
'			BALL SHADOW
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
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 13
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 13
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'***************************************************
'**              Flippers Primitive		          **
'***************************************************
'**     Include Flipper's shadows from Ninuzzu    **
'***************************************************
Sub flippers_Timer()
GloveP.objRotZ = glove.currentAngle-180
LeftFlipperP.objRotZ = LeftFlipper.CurrentAngle-90
RightFlipperP1.objRotZ = RightFlipper1.CurrentAngle-90
RightFlipperP.objRotZ = RightFlipper.CurrentAngle-90
	if shad = 1 Then
		LeftFlipperSh.RotZ = LeftFlipper.currentangle
		RightFlipperSh1.RotZ = RightFlipper1.currentangle
		RightFlipperSh.RotZ = RightFlipper.currentangle
	end if
claw.objRotX = Fclaw.currentangle
	if clawOn = 0 then 
		FclawR.enabled = 0
		FclawL.enabled = 0
		FClaw.rotatetostart
	end if
End Sub


'**************************************************************
'**********************  Glass Reflect  ***********************
'**************************************************************

Dim g3
g3 = 0
Sub GR6_hit
Primitive77.Image = "GlassRefl01"
end Sub
Sub GR1_hit
Primitive77.Image = "GlassRefl01"
end Sub
Sub GR2_exit
if g3=1 then 
Primitive77.Image = "GlassRefl01"
g3=0
end if
end Sub
Sub GR2_hit
Primitive77.Image = "GlassRefl02"
end Sub
Sub GR3_hit
g3=1
Primitive77.Image = "GlassRefl03"
end Sub
Sub GR4_hit
Primitive77.Image = "GlassRefl04"
end Sub
Sub GR5_hit
Primitive77.Image = "GlassRefl05"
end Sub








'cFastFlips by nFozzy
'Bypasses pinmame callback for faster and more responsive flippers
'Version 1.1 beta2 (More proper behaviour, extra safety against script errors)
'*************************************************
Function NullFunction(aEnabled):End Function    '1 argument null function placeholder
Class cFastFlips
    Public TiltObjects, DebugOn, hi
    Private SubL, SubUL, SubR, SubUR, FlippersEnabled, Delay, LagCompensation, Name, FlipState(3)
   
    Private Sub Class_Initialize()
        Delay = 0 : FlippersEnabled = False : DebugOn = False : LagCompensation = False
        Set SubL = GetRef("NullFunction"): Set SubR = GetRef("NullFunction") : Set SubUL = GetRef("NullFunction"): Set SubUR = GetRef("NullFunction")
    End Sub
   
    'set callbacks
    Public Property Let CallBackL(aInput)  : Set SubL  = GetRef(aInput) : Decouple sLLFlipper, aInput: End Property
    Public Property Let CallBackUL(aInput) : Set SubUL = GetRef(aInput) : End Property
    Public Property Let CallBackR(aInput)  : Set SubR  = GetRef(aInput) : Decouple sLRFlipper, aInput:  End Property
    Public Property Let CallBackUR(aInput) : Set SubUR = GetRef(aInput) : End Property
    Public Sub InitDelay(aName, aDelay) : Name = aName : delay = aDelay : End Sub   'Create Delay
    'Automatically decouple flipper solcallback script lines (only if both are pointing to the same sub) thanks gtxjoe
    Private Sub Decouple(aSolType, aInput)  : If StrComp(SolCallback(aSolType),aInput,1) = 0 then SolCallback(aSolType) = Empty End If : End Sub
 
    'call callbacks
    Public Sub FlipL(aEnabled)
        FlipState(0) = aEnabled 'track flipper button states: the game-on sol flips immediately if the button is held down (1.1)
        If not FlippersEnabled and not DebugOn then Exit Sub
        subL aEnabled
    End Sub
 
    Public Sub FlipR(aEnabled)
        FlipState(1) = aEnabled
        If not FlippersEnabled and not DebugOn then Exit Sub
        subR aEnabled
    End Sub
 
    Public Sub FlipUL(aEnabled)
        FlipState(2) = aEnabled
        If not FlippersEnabled and not DebugOn then Exit Sub
        subUL aEnabled
    End Sub
 
    Public Sub FlipUR(aEnabled)
        FlipState(3) = aEnabled
        If not FlippersEnabled and not DebugOn then Exit Sub
        subUR aEnabled
    End Sub
   
    Public Sub TiltSol(aEnabled)    'Handle solenoid / Delay (if delayinit)
        If delay > 0 and not aEnabled then  'handle delay
            vpmtimer.addtimer Delay, Name & ".FireDelay" & "'"
            LagCompensation = True
        else
            If Delay > 0 then LagCompensation = False
            EnableFlippers(aEnabled)
        end If
    End Sub
   
    Sub FireDelay() : If LagCompensation then EnableFlippers False End If : End Sub
   
    Private Sub EnableFlippers(aEnabled)
        If aEnabled then SubL FlipState(0) : SubR FlipState(1) : subUL FlipState(2) : subUR FlipState(3)
        FlippersEnabled = aEnabled
        If TiltObjects then vpmnudge.solgameon aEnabled
        If Not aEnabled then
            subL False
            subR False
            If not IsEmpty(subUL) then subUL False
            If not IsEmpty(subUR) then subUR False
        End If     
    End Sub

    End Class

Sub DM
	ramp167.visible = DesktopMode:ramp170.visible = DesktopMode
	ramp167Black.visible = Not DesktopMode:ramp170Black.visible = Not DesktopMode
	ramp32.visible = DesktopMode:
if sides = 0 then
'SideLOn.visible = DesktopMode:SideROn.visible = DesktopMode
SideLOff.visible = DesktopMode:SideROff.visible = DesktopMode
end if
end Sub