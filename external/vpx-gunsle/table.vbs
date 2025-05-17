' Based on the PM5 Table :
 ' Tabla Guns And Roses creada por Data East en 1994
 ' Graficos para el VP por Lord Hiryu
 ' Script por JP Salas


' Possible issue : ball not be launched in the right launch lane. Wait a short moment and it will be.


' VPX version :
' translated for VPX by Team PP 2017 (NeoFR45, Aetios, Chucky87, Arngrim and JPJ)
' Thanks to JP Salas to permit this translation
' Thanks Knorr for your Sound pack !!!! Very Usefull, and very clean !!!
' Thanks to Brian Ryan for helping in choosing environnement file, and for the Shadow Method ;) ;) ;)
' Very big Thanks Chucky for your lynx's eyes !!! :)
' Graphism : Néo and JPJ
' 3d : Aetios and JPJ At first, and finaly a BIG HELP to improve all from Ninuzzu and Tom Tower !!!!
' Dof : Arngrim
' Script : JP Salas and JPJ for adaptation in VPX and scripting animations and lights and more ;)
'		   And Ninuzzu who helped me resolving the plunger launch method !!!!
' Big Thanks for beta testing : Jens Leiensetter (see his youtube channel : Bambi Platfuss)
'								Peskopat, Mariopourlavie
'								My Super Chucky, always there
'								Shadow, Arngrim, Bertie
' Thanks to Bertie for his GNR death picture in the spinner of the Yellow ramp !!!
' Thanks to the Pinball community for all part of script found everywhere, and for all your tips and tricks ;) ;) ;) and your kindness for answer when support is needed (Ninuzzu, Brian, JPS)
' Another Thanks for Jens for all of his photos of the real pinball !!! Helpful for finalising, and searching some finition's detail (not all but a lot i've seen).
' Thanks for all of you i forget :)
'
' JPJ - Team PP


Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const BallSize = 24
Const BallMass = 1.9

' Thalamus 2019 November : Improved directional sounds
' !! NOTE : Table not verified yet !!

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 2000    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

'Dim DesktopMode:DesktopMode = Table1.ShowDT

'**********************************************************************************************************
'Kickers
'**********************************************************************************************************

sub K1_hit()
k1.destroyball
k2.createball
k2.kick 120,2
end Sub

'*********************************************************************************************************
'*** to show dmd in desktop Mod - Taken from ACDC Ninuzzu (THX) And Thanks To Rob Ross for Helping *******
Dim UseVPMDMD, DesktopMode
DesktopMode = Table1.ShowDT
If NOT DesktopMode Then UseVPMDMD = False		'hides the internal VPMDMD when using the color ROM or when table is in Full Screen and color ROM is not in use
If DesktopMode Then UseVPMDMD = True							'shows the internal VPMDMD when in desktop mode
'*********************************************************************************************************

 LoadVPM "01120100", "de.vbs", 3.02

 'Sub LoadVPM(VPMver, VBSfile, VBSver)
 '    On Error Resume Next
 '    If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
 '    ExecuteGlobal GetTextFile(VBSfile)
 '    If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description
 '    Set Controller = CreateObject("B2S.Server")
 '	 'Set Controller = CreateObject("UltraVP.BackglassServ")
 '    If Err Then MsgBox "Can't Load VPinMAME." & vbNewLine & Err.Description
 '    If VPMver > "" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
 '   If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
 '   On Error Goto 0
 ' End Sub

Dim bsTrough, bsBallRelease, bsCenterScoop, bsEject, dtLBank, dtRBank, bsVuk, mLeftMagnet, mRightMagnet, mCenterMagnet, cbCaptive
Dim x, bump1, bump2, bump3, BallInVuk, VukStep, Hole, seq, seqb, swrot, grvar, gr, balldropA, balldropB, testJP
Dim SndRedRamp, SndYellowRamp, SndRedRampV, SndYellowRampV, SndLaunchRamp, SndLaunchRampV, MH
Dim GlobalSoundLevel 'Diner's Method for amplify mechanical sounds, thanks to them :
Dim Myst, Mystv, Mystnudge, numcap
Dim OptionOpacity
Dim Cap(17)
Dim light
Dim FlashClearEtape
Dim FlashClearEtapeR
Dim FlashGreenEtape
Dim FlashRedEtape
Dim sides
Dim FastFlips
GlobalSoundLevel = 2


'**********************************************
'** TRY Option Myst Mod : 0 = off or 1 = On  **
'**********************************************
Myst = 0                      			         '**
'**********************************************************************
'**     Option sides or not sides in FS : 0 = without or 1 = with    **
'**********************************************************************
sides = 1															'**
'**********************************************************************

 Const cGameName = "gnr_300"

 Const UseSolenoids = 2
 Const UseLamps = 1
 Const UseGI = 1
 Const UseSync = 0
 Const HandleMech = 0


 ' Standard Sounds
 Const SSolenoidOn = "Solenoid"
 Const SSolenoidOff = ""
' Const SFlipperOn = "FlipperUp"
' Const SFlipperOff = "FlipperDown"
 Const SinCoin = "coin"

 ' TT addition to fix ball hangs in kickers
 Dim BallInSw38


 'Dim BallInCapKicker
 '************
 ' Table init.
 '************

 Sub Table1_Init
vpmInit Me
'dm


     With Controller
         .GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
         .SplashInfoLine = "Guns 'N' Roses, Data East 1994" & vbNewLine & "VPM table by Lord Hiryu & JPSalas v.1.0"
		.Games(cGameName).Settings.Value("sound") = 1
         .HandleMechanics = 0
         .HandleKeyboard = 0
         .ShowDMDOnly = 1
         .ShowFrame = 0
         .ShowTitle = 0
         .Hidden = DesktopMode
		  If Err Then MsgBox Err.Description
		On Error Goto 0
     End With

	Controller.SolMask(0) = 0
     vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
     Controller.Run

     ' Nudging
     vpmNudge.TiltSwitch = 1
     vpmNudge.Sensitivity = 5
     vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot, LeftSlingShotH)


     ' Trough & Ball Release
 Set bsTrough = New cvpmBallStack
	With bsTrough
		.InitSw 0, 14, 13, 12, 11, 10, 9, 0
		'.InitSw 0, 14, 13, 12, 11, 10, 9, 0
 		.Balls = 6
	End With


     Set bsBallRelease = New cvpmBallStack
     With bsBallRelease
         .InitSaucer BallRelease, 150, 110, 20
         .InitEntrySnd "solenoid", "solenoid"
         .InitExitSnd SoundFX("ballrel",DOFContactors), SoundFX("solenoid",DOFContactors)
     End With

     ' Center Scoop
     Set bsCenterScoop = New cvpmBallStack
     With bsCenterScoop
         .InitSw 0, 38, 0, 0, 0, 0, 0, 0
         .InitKick sw38a, 192, 20
         .KickZ = 0.4
         .KickForceVar = 2
		.KickBalls = 3
         .InitExitSnd SoundFX("ballrel",DOFContactors), SoundFX("solenoid",DOFContactors)
     End With

     Set bsEject = New cvpmBallStack
     With bsEject
         .InitSaucer sw37, 37, 280, 10
		.KickZ = 0.4
         .KickForceVar = 2
		.InitExitSnd SoundFX("ballrel",DOFContactors), SoundFX("solenoid",DOFContactors)
     End With

     'vuk

     Set bsVuk = New cvpmBallStack
     With bsVuk
         .InitSw 0, 39, 0, 0, 0, 0, 0, 0
         .InitKick sw39a, 0, 90
		.KickZ = 1.56
		.KickForceVar = 2
         .InitExitSnd SoundFX("ballrel",DOFContactors), SoundFX("solenoid",DOFContactors)
     End With

 '   Set bsVuk = New cvpmBallStack
 '   With bsVuk
'
'        .InitSaucer Sw39a, 39, 0, 60
'        .KickZ = 1.56
'        '.KickZ = 1.57
'        .KickForceVar = 2
'        .InitExitSnd SoundFX("ballrel",DOFContactors), SoundFX("solenoid",DOFContactors)
'    End With


     ' Magnets
     Set mLeftMagnet = New cvpmMagnet
     With mLeftMagnet
         .InitMagnet LeftMagnet, 16
         .Solenoid = 51
         .GrabCenter = 0
         .CreateEvents "mLeftMagnet"
     End With

     Set mRightMagnet = New cvpmMagnet
     With mRightMagnet
         .InitMagnet RightMagnet, 16
         .Solenoid = 53
         .GrabCenter = 0
         .CreateEvents "mRightMagnet"
     End With

     Set mCenterMagnet = New cvpmMagnet
     With mCenterMagnet
         .InitMagnet CenterMagnet, 16
         .Solenoid = 52
         .GrabCenter = 0
         .CreateEvents "mCenterMagnet"
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

     'Captive Ball
	CapKicker.CreateSizedBallWithMass Ballsize, BallMass:CapKicker.Kick 0,3:CapKicker.enabled = 0

	' replaced captive ball with directly creating ball to kicker
	 'vpmCreateBall CapKicker


     ' Init GI
     GiOff


     ' Init Plungers & Div
'	Plunger.Pullback
	KickBack.Pullback
     SolTrapDoor 0

     'Mas flashers (27-04-09)
     f8.state = 0
     f8a.state = 0
     f7.state = 0
     f7a.state = 0

     ' Turn of Flashers
     SolFlash25 0:SolFlash26 0:SolFlash27 0:SolFlash28 0:SolFlash29 0:SolFlash30 0

     ' Setup Lamps
     vpmMapLights AllLamps

     ' Main Timer init
     PinMAMETimer.Interval = PinMAMEInterval
     PinMAMETimer.Enabled = 1

     ' GI's
	flash5.state = 0
	flash6.state = 0:
	flash5i.state = 1:flash6i.state = 1
	Flash5i.intensity = 0.3:flash6i.intensity = 0.3
	flash2veille.state = 0 :flash2veille1.state = 0
	flash2veille.intensity = 0.75 :flash2veille1.intensity = 0.75
	BallInSw38 = 0
	 'BallInCapKicker = 1
	slingflashL.state = 0
	slingflashR.state = 0
		GIBWL.color = RGB(212,212,212)
		GIBWL.intensity = 2
		GIBWR.color = RGB(212,212,212)
		GIBWR.intensity = 2
	GIBWL.state = 0
	GIBWR.state = 0


'********* rm = reflection Mod *********
	rm.enabled = 1 '  ****Activation****
'***************************************

'********    FLippers DE Anim     ******
	logo.enabled = 1' ****Activation****
'***************************************

FlashClearAnim.enabled = 1
FlashClearAnim2.enabled = 1
FlashGreenAnim.enabled = 1


	hole=0
	seq=1
	seqb=1
	grvar=1
	gr=2
	balldropA=0
	balldropB=0
	SndRedRamp=0
	SndRedRampV=0
	SndYellowRamp=0
	SndYellowRampV=0
	SndLaunchRamp=0
	SndLaunchRampV=0
	swrot=-4
	MH = 0

	MystHole1.visible = 0
	MystHole2.visible = 0
	MystHole3.visible = 0

	HoleRampLight.intensity = 0
	HoleRampLight1.intensity = 0
	HoleRampLight2.intensity = 0
	HoleRampLight3.intensity = 0
	HoleRampLight4.intensity = 0
	GIcenter4.state = 0
	Mystv=0
	Mystnudge = 1

	testjp = 0
	cache.isdropped = 1
	cache1.isdropped = 1

	mm01.visible = 0
	mm02.visible = 0
	mm03.visible = 0
	mm04.visible = 0
	mml01.state = 0
	mml01.intensity = 0

	FlashClearEtape = 0
	FlashClearEtapeR = 0
	FlashRedEtape = 0
	FlashGreenEtape = 0

End Sub

'****************** End Table Ini ************


 Sub table1_Paused:Controller.Pause = 1:End Sub
 Sub table1_unPaused:Controller.Pause = 0:End Sub

Sub HoleLight_Timer()
if hole=1 then LightSequence:GreenRamp
if hole=0 then Hole01.state=0:Hole02.state=0:Hole03.state=0:Hole04.state=0:Hole1r.state=0:Hole2r.state=0:Hole3r.state=0:Hole4r.state=0:seq=1
End Sub

Sub LightSequence
Select Case seq
Case 1:
Hole01.state= 1:Hole02.State=0:Hole03.state= 0:Hole04.State=0
Hole1r.state= 1:Hole2r.State=0:Hole3r.state= 0:Hole4r.State=0
seq = 2
Case 2:
Hole01.state= 0:Hole02.State=1:Hole03.state= 0:Hole04.State=0
Hole1r.state= 0:Hole2r.State=1:Hole3r.state= 0:Hole4r.State=0
seq = 3
Case 3:
Hole01.state= 0:Hole02.State=0:Hole03.state= 1:Hole04.State=0
Hole1r.state= 0:Hole2r.State=0:Hole3r.state= 1:Hole4r.State=0
seq = 4
Case 4:
Hole01.state= 0:Hole02.State=0:Hole03.state= 0:Hole04.State=1
Hole1r.state= 0:Hole2r.State=0:Hole3r.state= 0:Hole4r.State=1
seq = 5
Case 5:
Hole01.state= 0:Hole02.State=0:Hole03.state= 1:Hole04.State=0
Hole1r.state= 0:Hole2r.State=0:Hole3r.state= 1:Hole4r.State=0
seq = 6
Case 6:
Hole01.state= 0:Hole02.State=1:Hole03.state= 0:Hole04.State=0
Hole1r.state= 0:Hole2r.State=1:Hole3r.state= 0:Hole4r.State=0
seq = 7
Case 7:
Hole01.state= 1:Hole02.State=0:Hole03.state= 0:Hole04.State=0
Hole1r.state= 1:Hole2r.State=0:Hole3r.state= 0:Hole4r.State=0
seq = 8
Case 8:
Hole01.state= 0:Hole02.State=1:Hole03.state= 0:Hole04.State=0
Hole1r.state= 0:Hole2r.State=1:Hole3r.state= 0:Hole4r.State=0
seq = 3
End Select
End Sub

Sub GreenRamp
gr=gr+grvar
HoleRampLight.intensity = gr
HoleRampLight1.intensity = gr
HoleRampLight2.intensity = gr
HoleRampLight3.intensity = gr
HoleRampLight4.intensity = gr/4
if gr=>20 then grvar=-1
if gr=<2 then grvar=1
End Sub


' **************************************************************************
' ****************** Drop Targets ******************************************
' Left Drop Targets


	dim sw33Dir, sw34Dir, sw59Dir
	dim sw33Pos, sw34Pos, sw59Pos

	sw33Dir = 1:sw34Dir = 1:sw59Dir = 1
	sw33Pos = 0:sw34Pos = 0:sw59Pos = 0

  'Targets Init
	sw33a.TimerEnabled = 1:sw34a.TimerEnabled = 1:sw59a.TimerEnabled = 1


   	Set DTLBank = New cvpmDropTarget
   	  With DTLBank
   		.InitDrop Array(Array(sw33,sw33a),Array(sw34,sw34a),Array(sw59,sw59a)), Array(33,34,59)
		.InitSnd SoundFX("target_drop",DOFDropTargets),SoundFX("reset_drop",DOFContactors)
       End With


  Sub sw33_Hit:DTLBank.Hit 1:sw33Dir = 0:sw33a.TimerEnabled = 1:End Sub
  Sub sw34_Hit:DTLBank.Hit 2:sw34Dir = 0:sw34a.TimerEnabled = 1:End Sub
  Sub sw59_Hit:DTLBank.Hit 3:sw59Dir = 0:sw59a.TimerEnabled = 1:End Sub



 Sub sw33a_Timer()

  Select Case sw33Pos
        Case 0: sw33P.z=24
				If sw33Dir = 1 then
					sw33a.TimerEnabled = 0
				else
					sw33Dir = 0
					sw33a.TimerEnabled = 1
				end if
        Case 1: sw33P.z=26
        Case 2: sw33P.z=29
        Case 3: sw33P.z=26
        Case 4: sw33P.z=22
        Case 5: sw33P.z=18
        Case 6: sw33P.z=14
        Case 7: sw33P.z=10
        Case 8: sw33P.z=6
        Case 9: sw33P.z=2
        Case 10: sw33P.z=-4
        Case 11: sw33P.z=-10
        Case 12: sw33P.z=-16
        Case 13: sw33P.z=-19:sw33P.ReflectionEnabled = true
        Case 14: sw33P.z=-22:sw33P.ReflectionEnabled = false
				 If sw33Dir = 1 then
				 else
					sw33a.TimerEnabled = 0
			     end if
End Select
	If sw33Dir = 1 then
		If sw33pos>0 then sw33pos=sw33pos-1
	else
		If sw33pos<14 then sw33pos=sw33pos+1
	end if
  End Sub





 Sub sw34a_Timer()
  Select Case sw34Pos
        Case 0: sw34P.z=24
				 If sw34Dir = 1 then
					sw34a.TimerEnabled = 0
				 else
					sw34Dir = 0
					sw34a.TimerEnabled = 1
			     end if
        Case 1: sw34P.z=26
        Case 2: sw34P.z=29
        Case 3: sw34P.z=26
        Case 4: sw34P.z=22
        Case 5: sw34P.z=18
        Case 6: sw34P.z=14
        Case 7: sw34P.z=10
        Case 8: sw34P.z=6
        Case 9: sw34P.z=2
        Case 10: sw34P.z=-4
        Case 11: sw34P.z=-10
        Case 12: sw34P.z=-16
        Case 13: sw34P.z=-19:sw34P.ReflectionEnabled = true
        Case 14: sw34P.z=-22:sw34P.ReflectionEnabled = false
				 If sw34Dir = 1 then
				 else
					sw34a.TimerEnabled = 0
			     end if
End Select
	If sw34Dir = 1 then
		If sw34pos>0 then sw34pos=sw34pos-1
	else
		If sw34pos<14 then sw34pos=sw34pos+1
	end if
  End Sub


 Sub sw59a_Timer()
  Select Case sw59Pos
        Case 0: sw59P.z=24
				 If sw59Dir = 1 then
					sw59a.TimerEnabled = 0
				 else
					sw59Dir = 0
					sw59a.TimerEnabled = 1
			     end if
        Case 1: sw59P.z=26
        Case 2: sw59P.z=29
        Case 3: sw59P.z=26
        Case 4: sw59P.z=22
        Case 5: sw59P.z=18
        Case 6: sw59P.z=14
        Case 7: sw59P.z=10
        Case 8: sw59P.z=6
        Case 9: sw59P.z=2
        Case 10: sw59P.z=-4
        Case 11: sw59P.z=-10
        Case 12: sw59P.z=-16
        Case 13: sw59P.z=-19:sw59P.ReflectionEnabled = true
        Case 14: sw59P.z=-22:sw59P.ReflectionEnabled = false
				 If sw59Dir = 1 then
				 else
					sw59a.TimerEnabled = 0
			     end if
End Select
	If sw59Dir = 1 then
		If sw59pos>0 then sw59pos=sw59pos-1
	else
		If sw59pos<14 then sw59pos=sw59pos+1
	end if
  End Sub



'DT Subs
   Sub ResetDropsL(Enabled)
		If Enabled Then
			sw33Dir = 1:sw34Dir = 1:sw59Dir = 1
			sw33a.TimerEnabled = 1:sw34a.TimerEnabled = 1:sw59a.TimerEnabled = 1
			DTLBank.DropSol_On
		End if
   End Sub

' **************************************************************************
' Right Drop Targets


	dim sw36Dir, sw35Dir, sw57Dir
	dim sw36Pos, sw35Pos, sw57Pos

	sw36Dir = 1:sw35Dir = 1:sw57Dir = 1
	sw36Pos = 0:sw35Pos = 0:sw57Pos = 0

  'Targets Init
	sw36a.TimerEnabled = 1:sw35a.TimerEnabled = 1:sw57a.TimerEnabled = 1


   	Set DTRBank = New cvpmDropTarget
   	  With DTRBank
   		.InitDrop Array(Array(sw36,sw36a),Array(sw35,sw35a),Array(sw57,sw57a)), Array(36,35,57)
		.InitSnd SoundFX("target_drop",DOFDropTargets),SoundFX("reset_drop",DOFContactors)
       End With


  Sub sw36_Hit:DTRBank.Hit 1:sw36Dir = 0:sw36a.TimerEnabled = 1:End Sub
  Sub sw35_Hit:DTRBank.Hit 2:sw35Dir = 0:sw35a.TimerEnabled = 1:End Sub
  Sub sw57_Hit:DTRBank.Hit 3:sw57Dir = 0:sw57a.TimerEnabled = 1:End Sub



 Sub sw36a_Timer()

  Select Case sw36Pos
        Case 0: sw36P.z=24
				If sw36Dir = 1 then
					sw36a.TimerEnabled = 0
				else
					sw36Dir = 0
					sw36a.TimerEnabled = 1
				end if
        Case 1: sw36P.z=26
        Case 2: sw36P.z=29
        Case 3: sw36P.z=26
        Case 4: sw36P.z=22
        Case 5: sw36P.z=18
        Case 6: sw36P.z=14
        Case 7: sw36P.z=10
        Case 8: sw36P.z=6
        Case 9: sw36P.z=2
        Case 10: sw36P.z=-4
        Case 11: sw36P.z=-10
        Case 12: sw36P.z=-16
        Case 13: sw36P.z=-19:sw36P.ReflectionEnabled = true
        Case 14: sw36P.z=-22:sw36P.ReflectionEnabled = false
				 If sw36Dir = 1 then
				 else
					sw36a.TimerEnabled = 0
			     end if
End Select
	If sw36Dir = 1 then
		If sw36pos>0 then sw36pos=sw36pos-1
	else
		If sw36pos<14 then sw36pos=sw36pos+1
	end if
  End Sub





 Sub sw35a_Timer()
  Select Case sw35Pos
        Case 0: sw35P.z=24
				 If sw35Dir = 1 then
					sw35a.TimerEnabled = 0
				 else
					sw35Dir = 0
					sw35a.TimerEnabled = 1
			     end if
        Case 1: sw35P.z=26
        Case 2: sw35P.z=29
        Case 3: sw35P.z=26
        Case 4: sw35P.z=22
        Case 5: sw35P.z=18
        Case 6: sw35P.z=14
        Case 7: sw35P.z=10
        Case 8: sw35P.z=6
        Case 9: sw35P.z=2
        Case 10: sw35P.z=-4
        Case 11: sw35P.z=-10
        Case 12: sw35P.z=-16
        Case 13: sw35P.z=-19:sw35P.ReflectionEnabled = true
        Case 14: sw35P.z=-22:sw35P.ReflectionEnabled = false
				 If sw35Dir = 1 then
				 else
					sw35a.TimerEnabled = 0
			     end if
End Select
	If sw35Dir = 1 then
		If sw35pos>0 then sw35pos=sw35pos-1
	else
		If sw35pos<14 then sw35pos=sw35pos+1
	end if
  End Sub


 Sub sw57a_Timer()
  Select Case sw57Pos
        Case 0: sw57P.z=24
				 If sw57Dir = 1 then
					sw57a.TimerEnabled = 0
				 else
					sw57Dir = 0
					sw57a.TimerEnabled = 1
			     end if
        Case 1: sw57P.z=26
        Case 2: sw57P.z=29
        Case 3: sw57P.z=26
        Case 4: sw57P.z=22
        Case 5: sw57P.z=18
        Case 6: sw57P.z=14
        Case 7: sw57P.z=10
        Case 8: sw57P.z=6
        Case 9: sw57P.z=2
        Case 10: sw57P.z=-4
        Case 11: sw57P.z=-10
        Case 12: sw57P.z=-16
        Case 13: sw57P.z=-19:sw57P.ReflectionEnabled = true
        Case 14: sw57P.z=-22:sw57P.ReflectionEnabled = false
				 If sw57Dir = 1 then
				 else
					sw57a.TimerEnabled = 0
			     end if
End Select
	If sw57Dir = 1 then
		If sw57pos>0 then sw57pos=sw57pos-1
	else
		If sw57pos<14 then sw57pos=sw57pos+1
	end if
  End Sub



'DT Subs
   Sub ResetDropsR(Enabled)
		If Enabled Then
			sw36Dir = 1:sw35Dir = 1:sw57Dir = 1
			sw36a.TimerEnabled = 1:sw35a.TimerEnabled = 1:sw57a.TimerEnabled = 1
			DTRBank.DropSol_On
		End if
   End Sub

' **************************************************************************


 '**********
 ' Keys
 '**********
'*****************
' TT nudge
' ignore this -> used only for my cab
'*****************

' StopShake

Sub Table1_Exit  '  in some tables this needs to be Table1_Exit
    Controller.Stop
End Sub

Dim NudgeDirection
Dim MyNudge

Sub MyNudgeTimer_Timer()
    me.enabled = false
    if NudgeDirection = "L" Then LeftNudge 90, 2, 20:PlaySound SoundFX("fx_nudge_left",0):end if
    if NudgeDirection = "R" Then RightNudge 270, 2, 20:PlaySound SoundFX("fx_nudge_right",0):end if
End Sub



Sub table1_KeyDown(ByVal Keycode)

     If vpmKeyDown(keycode) Then Exit Sub
     If keycode = keyFront Then vpmTimer.pulsesw 8 'Buy-in Button - 2 key

     If keycode = PlungerKey Then Plunger2.Pullback:vpmTimer.PulseSw 62
     If keycode = LeftTiltKey Then LeftNudge 80, 1.2, 20
     If keycode = RightTiltKey Then RightNudge 280, 1.2, 20
     If keycode = CenterTiltKey Then CenterNudge 0, 1.6, 25


    If KeyCode = LeftFlipperKey then FastFlips.FlipL True ':  FastFlips.FlipUL True
    If KeyCode = RightFlipperKey then FastFlips.FlipR True ':FastFlips.FlipUR True

 End Sub

 Sub Table1_KeyUp(ByVal Keycode)
     If vpmKeyUp(keycode) Then Exit Sub
     If keycode = PlungerKey Then Plunger2.Fire:PlaySoundAtVol SoundFX("plunger",DOFcontactors), Plunger2, 1
    If KeyCode = LeftFlipperKey then FastFlips.FlipL False ':  FastFlips.FlipUL True
    If KeyCode = RightFlipperKey then FastFlips.FlipR False' :FastFlips.FlipUR True

 End Sub



 '*********
 ' Switches
 '*********

 ' Slings

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep, Hstep

Sub RightSlingShot_Slingshot
    vpmTimer.PulseSw 28
    PlaySoundAtVol SoundFX("right_slingshot",DOFContactors), sling1, 1
	slingflashR.state = 0
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
	slingflashR.state = 1
		GIBWR.color = RGB(255,0,0):GIBWR.intensity = 15:GIBWR.state = 1

if RSLing1.Visible = 0 then slingflashR.state = 0:GIBWR.color = RGB(212,212,212):GIBWR.intensity = 2:GIBWR.state = 1:end If
End Sub

Sub LeftSlingShot_Slingshot
    vpmTimer.PulseSw 29
    PlaySoundAtVol SoundFX("left_slingshot",DOFContactors), sling2, 1
	slingflashL.state = 0
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
	slingflashL.state = 1
		GIBWL.state = 1:GIBWL.color = RGB(0,128,0):GIBWL.intensity = 15


if LSLing1.Visible = 0 then slingflashL.state = 0:GIBWL.state = 1:GIBWL.color = RGB(212,212,212):GIBWL.intensity = 2:end If
End Sub

Sub LeftSlingShotH_Slingshot
    vpmTimer.PulseSw 30
    PlaySoundAtVol SoundFX("left_slingshot",DOFContactors), slingh, 1
    LSlingH.Visible = 0
    LSlingH1.Visible = 1
    slingH.TransZ = -20
    HStep = 0
    LeftSlingShotH.TimerEnabled = 1
End Sub

Sub LeftSlingShotH_Timer
    Select Case HStep
        Case 3:LSLingH1.Visible = 0:LSLingH2.Visible = 1:slingH.TransZ = -10
        Case 4:FlashGreenEtape = 1:LSLingH2.Visible = 0:LSLingH.Visible = 1:slingH.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    HStep = HStep + 1
		flash6.state = 1
		GIBWR.color = RGB(255,0,0):GIBWR.intensity = 15:GIBWR.state = 1
		GIBWL.color = RGB(0,128,0):GIBWL.intensity = 15:GIBWL.state = 1
		flashvertref.amount = 70
		flashvertref.intensityScale = 1
		if LSLingH1.Visible = 0 then GIBWL.color = RGB(212,212,212):GIBWL.intensity = 2:GIBWL.state = 1:GIBWR.color = RGB(212,212,212):GIBWR.intensity = 2:GIBWR.state = 1:flash6.state = 0:flashvertref.amount = 0:flashvertref.intensityScale = 0:end If


End Sub

'**********************************************************
'************************ Bumpers *************************
'**********************************************************

Sub Bumper1_Hit
    vpmTimer.PulseSw 25
	bumpersounds()
	Me.TimerEnabled = 1
	Flasher101.state = 1
		GIBWL.color = RGB(0,128,0):GIBWL.intensity = 15:GIBWL.state = 1
	BackWall.image = "backwallon4Bump"
End Sub

Sub Bumper1_Timer
	Me.Timerenabled = 0
	Flasher101.state = 0
		GIBWL.color = RGB(212,212,212):GIBWL.intensity = 2:GIBWL.state = 1
	BackWall.image = "backwallon"
End Sub

Sub Bumper2_Hit
    vpmTimer.PulseSw 26
	bumpersounds()
	Me.TimerEnabled = 1
	Flasher102.state = 1
		GIBWL.color = RGB(0,128,0):GIBWL.intensity = 15:GIBWL.state = 1
		GIBWR.color = RGB(255,0,0):GIBWR.intensity = 15:GIBWR.state = 1
	BackWall.image = "backwallon4Bump"
End Sub

Sub Bumper2_Timer
	Me.Timerenabled = 0
	Flasher102.state = 0
		GIBWL.color = RGB(212,212,212):GIBWL.intensity = 2:GIBWL.state = 1
		GIBWR.color = RGB(212,212,212):GIBWR.intensity = 2:GIBWR.state = 1
	BackWall.image = "backwallon"
End Sub

Sub Bumper3_Hit
    vpmTimer.PulseSw 27
	bumpersounds()
	Me.TimerEnabled = 1
	Flasher103.state = 1
		GIBWR.color = RGB(255,0,0):GIBWR.intensity = 15:GIBWR.state = 1
	BackWall.image = "backwallon4Bump"
End Sub

Sub Bumper3_Timer
	Me.Timerenabled = 0
	Flasher103.state = 0
	BackWall.image = "backwallon"
	GIBWR.color = RGB(212,212,212):GIBWR.intensity = 2:GIBWR.state = 1
End Sub

sub BumperSounds()
	Select Case Int(Rnd*2)
		Case 0 : PlaySoundAtVol SoundFX("bumper1",DOFContactors), ActiveBall, 1
		Case 1 : PlaySoundAtVol SoundFX("bumper2",DOFContactors), ActiveBall, 1
	End Select
End Sub


' Rollovers
 Sub sw16_Hit():Controller.Switch(16) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub
 Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub

 Sub sw53_Hit():Controller.Switch(53) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub
 Sub sw53_UnHit:Controller.Switch(53) = 0:End Sub

 Sub sw54_Hit():Controller.Switch(54) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub
 Sub sw54_UnHit:Controller.Switch(54) = 0:End Sub

 Sub sw55_Hit():Controller.Switch(55) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub
 Sub sw55_UnHit:Controller.Switch(55) = 0:End Sub

 Sub sw56_Hit():Controller.Switch(56) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub
 Sub sw56_UnHit:Controller.Switch(56) = 0:End Sub

 Sub sw58_Hit():Controller.Switch(58) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub
 Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub

 Sub sw60_Hit():Controller.Switch(60) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub
 Sub sw60_UnHit:Controller.Switch(60) = 0:End Sub

 Sub sw24_Hit():Controller.Switch(24) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:hole=1:HoleLight.enabled = 1:End Sub
 Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub

 Sub sw21_Hit():Controller.Switch(21) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub
 Sub sw21_UnHit:Controller.Switch(21) = 0:End Sub

 Sub sw22_Hit():Controller.Switch(22) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub
 Sub sw22_UnHit:Controller.Switch(22) = 0:End Sub

 Sub sw23_Hit():Controller.Switch(23) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub
 Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub

 Sub sw48_Hit():Controller.Switch(48) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:End Sub
 Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub

 ' Ramp Switches
 Sub sw49_Hit():Controller.Switch(49) = 1:PlaySoundAtVol "gate", ActiveBall, 1:sndredrampV=1:End Sub
 Sub sw49_UnHit:Controller.Switch(49) = 0:End Sub


' Switch metalic ramp red JPJ

Sub sw50_Hit():Controller.Switch(50) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:seqb=1:RedMetalAnim.enabled = 1: 'anim
Mystv = Mystv + 1
if Mystv=11 then resetMyst
if Myst=1 then
	if Mystv=1 or Mystv=1 or Mystv=3 or Mystv=5 or Mystv=7 or Mystv=9 then playsound "biere":capchoice
end if
if Myst=1 Then
	if Mystv=2 or Mystv=4 or Mystv=6 or Mystv=8 or Mystv=10 then playsound "pchit":capchoice
end if
if Mystv=11 then resetMyst

End Sub

Sub sw50_UnHit:Controller.Switch(50) = 0:End Sub

Sub RedMetalAnim_timer
Select Case seqb
Case 1:seqb = 2:
swRedMetal.rotz = -87
Case 2:seqb = 3:
swRedMetal.rotz = -95
Case 3:seqb = 4:
swRedMetal.rotz = -101
Case 4:seqb = 5:
swRedMetal.rotz = -97
Case 5:seqb = 6:
swRedMetal.rotz = -91
Case 6:
swRedMetal.rotz = -85:RedMetalAnim.enabled = 0
End Select
'RedMetalAnim.enabled = 0
End Sub

'Sub Anim
'Select Case seqb
'Case 1:seqb = 2:
'swRedMetal.rotz = -87
'Case 2:seqb = 3:
'swRedMetal.rotz = -95
'Case 3:seqb = 4:
'swRedMetal.rotz = -101
'Case 4:seqb = 5:
'swRedMetal.rotz = -97
'Case 5:seqb = 6:
'swRedMetal.rotz = -91
'Case 6:
'swRedMetal.rotz = -85
'End Select
'End Sub



 Sub sw51_Hit():Controller.Switch(51) = 1:PlaySoundAtVol "gate", ActiveBall, 1:SndYellowRampV=1:Controller.Switch(52) = 0::End Sub
 Sub sw51_UnHit:Controller.Switch(51) = 0:End Sub

 Sub sw52_Hit():Controller.Switch(52) = 1 'PlaySound "gate" enlevé car plus mis au même endroit
	End Sub
' Sub sw52_UnHit:debug.print "sorti du switch 52":Controller.Switch(52) = 0:
'	End Sub
Sub gate5_hit:Controller.Switch(52) = 1:	End Sub


 Sub sw40_Hit():Controller.Switch(40) = 1:PlaySoundAtVol "sensor", ActiveBall, 1:balldropB=1:End Sub
 Sub sw40_UnHit:Controller.Switch(40) = 0:End Sub

 ' Targets

Sub SW17_hit():sw17p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 17:End Sub
Sub SW17_Timer():sw17p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW18_hit():sw18p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 18:End Sub
Sub SW18_Timer():sw18p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW19_hit():sw19p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 19:End Sub
Sub SW19_Timer():sw19p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW20_hit():sw20p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 20:End Sub
Sub SW20_Timer():sw20p.transx = 0:Me.TimerEnabled = 0:End Sub







 ' Drain & holes
 Sub Drain_Hit:PlaysoundAtVol "drain", Drain, 1:ClearBallID:bsTrough.AddBall Me:End Sub
 Sub sw37_Hit:playSoundAtVol "kicker_enter", sw37, 1:
	'ClearBallID:
	'sw37.DestroyBall:
	bsEject.AddBall 0
End Sub

Sub sw39_Hit:playSoundAtVol "kicker_enter", sw39, 1:
	cache1.isdropped = 0
	 ClearBallID
bsVuk.AddBall Me
sw39a.enabled=1
sw39.enabled=0
End Sub
sub sw39a_unhit
sw39a.enabled=0
sw39.enabled = 1
cache1.isdropped = 1
end Sub

  ' Center Scoop
 Dim aBall, aZpos

 Sub sw38_Hit
		cache.isdropped = 0
		MH=1
		 MystAnimTimer.Enabled=1
	If BallInSw38 = 0 Then
		 me.enabled = 0
		 BallInSw38 = 1
		 playSoundAtVol("kicker_enter"), sw38, 1
		 Set aBall = ActiveBall
		 aZpos = 35
		 ClearBallID
		 sw38.Destroyball
		 bsCenterScoop.AddBall Me
		Me.TimerInterval = 10000 '2000
		Me.TimerEnabled = 1
		 sw38a.Enabled = 1
	End If
 End Sub


 Sub sw38_Timer
      Me.TimerEnabled = 0
	  BallInSw38 = 0
	  me.enabled = 1
 End Sub


Sub sw38a_Unhit

sw38a.Enabled = 0
MH=0
MystAnimTimer.Enabled=0
MH=0:MystHole1.visible = 0:MystHole2.visible = 0:MystHole3.visible = 0:BallInSw38 = 0

sw38.Enabled = 1
		cache.isdropped = 1
End Sub



'************************************
'**** Mystery Ball Animation JPJ ****
'************************************

Sub MystAnimTimer_Timer
MystAnimation
End Sub


Sub MystAnimation
Select Case MH
Case 1:MH = 2:
MystHole1.visible = 1:MystHole2.visible = 0:MystHole3.visible = 0
Case 2:MH = 3:
MystHole1.visible = 0:MystHole2.visible = 1:MystHole3.visible = 0
Case 3:MH = 4:
MystHole1.visible = 0:MystHole2.visible = 0:MystHole3.visible = 1
Case 4:MH = 5:
MystHole1.visible = 0:MystHole2.visible = 1:MystHole3.visible = 0
Case 5:MH = 6:
MystHole1.visible = 1:MystHole2.visible = 0:MystHole3.visible = 0
Case 6:MH=1:
MystHole1.visible = 0:MystHole2.visible = 1:MystHole3.visible = 0
End Select
End Sub



 ' Ramp Helpers
  Sub RHelp1_Hit:ClearBallID:RHelp1.DestroyBall:vpmCreateBall RHelp1a:RHelp1a.kick 0, 0:PlaySoundAtVol "BallHit", ActiveBall, 1:End Sub
  Sub RHelp2_Hit:ClearBallID:RHelp2.DestroyBall:vpmCreateBall RHelp2a:RHelp2a.kick 0, 0:PlaySoundAtVol "BallHit", ActiveBall, 1:End Sub


Sub Trigger1_Hit:sndLaunchrampV=1:End Sub
Sub Trigger2_Hit:playsoundAtVol "rail", ActiveBall, 1:End Sub
Sub TriggerTest_Hit:testJP=1:End Sub
Sub TriggerTest2_Hit:testJP=0:End Sub
Sub Trigger9_Hit
if balldropA=1 then balldropBig():balldropA=0
End Sub
Sub Trigger10_Hit
if balldropB = 1 then balldropSides()
hole=0:HoleLight.enabled = 0:Hole01.state=0:Hole02.state=0:Hole03.state=0:Hole04.state=0:Hole1r.state=0:Hole2r.state=0:Hole3r.state=0:Hole4r.state=0:HoleRampLight.intensity = 0:HoleRampLight1.intensity = 0:HoleRampLight2.intensity = 0:HoleRampLight3.intensity = 0:balldropB=0
End Sub
Sub Trigger4_Hit:playsoundAtVol "WirerampLeft", ActiveBall, 1
Mystv = Mystv + 1
if Myst=1 then
	if Mystv=1 or Mystv=1 or Mystv=3 or Mystv=5 or Mystv=7 or Mystv=9 then playsound "biere":capchoice
end if
if Myst=1 Then
	if Mystv=2 or Mystv=4 or Mystv=6 or Mystv=8 or Mystv=10 then playsound "pchit":capchoice
end if
if Mystv=11 then resetMyst
End Sub
Sub Trigger5_Hit:playsoundAtVol "Wireramp2jpmod", ActiveBall, 1:End Sub
Sub Trigger6_Hit:balldropSides():stopsound "WirerampLeft":playsoundAtVol "WirerampStop", ActiveBall, 1:End Sub
Sub Trigger7_Hit:balldropSides():stopsound "Wireramp2jpmod":playsoundAtVol "WirerampStop", ActiveBall, 1:End Sub
Sub Trigger8_Hit:stopsound "LaunchWire":balldropA=1:sndLaunchramp=0:sndLaunchrampV=0:End Sub

Sub Trigger11RedRamp_Hit:
	if sndredramp=0 and sndredrampV=0 and myst = 0 then playsound "plasticramp", 2, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):SndRedRamp=1
	if sndredramp=1 and sndredrampV=1 and myst = 0 then stopsound "plasticramp":SndRedRamp=0:SndRedRampV=0
	if sndredramp=0 and sndredrampV=0 and myst = 1 then playsound "mystramp", 2, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):SndRedRamp=1
	if sndredramp=1 and sndredrampV=1 and myst = 1 then stopsound "mystramp":SndRedRamp=0:SndRedRampV=0

End Sub

Sub Trigger12RedRampOut_Hit:stopsound "plasticramp":stopsound "mystramp":SndRedRamp=0:SndRedRampV=0:End Sub

Sub Trigger13YellowRamp_Hit:
	If ActiveBall.VelX<-11 and ActiveBall.VelY<-29 then ActiveBall.VelX=-11:ActiveBall.Vely=-29:End If
	'If ActiveBall.VelY<-29 then ActiveBall.Vely=-29:End If
	if sndyellowramp=0 and sndyellowrampV=0 and myst = 0 then playsound "plasticramp2", 2, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):SndYellowRamp=1
	if sndyellowramp=1 and sndyellowrampV=1 and myst = 0 then stopsound "plasticramp2":SndyellowRamp=0:SndYellowRampV=0
	if sndyellowramp=0 and sndyellowrampV=0 and myst = 1 then playsound "mystramp", 2, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):SndYellowRamp=1
	if sndyellowramp=1 and sndyellowrampV=1 and myst = 1 then stopsound "mystramp":SndyellowRamp=0:SndYellowRampV=0
End Sub

Sub Trigger14YellowRampOut_Hit:stopsound "plasticramp2":stopsound "mystramp":SndYellowRamp=0:SndYellowRampV=0:Controller.Switch(52) = 0:End Sub

Sub Trigger15LaunchRamp_Hit:
	if sndLaunchramp=0 and sndLaunchrampV=0 then playsound "LaunchWire", 3, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):SndLaunchRamp=1
	if sndLaunchramp=1 and sndLaunchrampV=1 then stopsound "LaunchWire":SndLaunchRamp=0:SndLaunchRampV=0
End Sub

sub balldropSides()
	Select Case Int(Rnd*2)
		Case 0 : PlaySoundAtBallVol "balldrop1", 1
		Case 1 : PlaySoundAtBallVol "balldrop2", 1
	End Select
End Sub

 sub balldropBig()
	Select Case Int(Rnd*3)
		Case 0 : PlaySoundAtBallVol "drop1", 1
		Case 1 : PlaySoundAtBallVol "drop2", 1
		Case 2 : PlaySoundAtBallVol "drop3", 1
	End Select
End Sub

 'FLIPPERS *************************************************************************************************************************************************

'SolCallback(sLRFlipper) = "SolRFlipper"
'SolCallback(sLLFlipper) = "SolLFlipper"
'
'Sub SolLFlipper(Enabled)
'		 If Enabled Then
'			 PlaySound SoundFX("fx_FlipperUp",DOFFlippers):LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
'	else
'			 PlaySound SoundFX("fx_FlipperDown",DOFFlippers):LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
'		 End If
'	  End Sub
'
'	Sub SolRFlipper(Enabled)
'		 If Enabled Then
'			 PlaySound SoundFX("fx_FlipperUp",DOFFlippers):RightFlipper.RotateToEnd
'		 Else
'			 PlaySound SoundFX("fx_FlipperDown",DOFFlippers):RightFlipper.RotateToStart
'		 End If
'	End Sub





Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

sub cache_hit
	PlaySoundAtBallVol("fx_collide"),1
end sub
sub cache1_hit
	PlaySoundAtBallVol("fx_collide"), 1
end sub

 '********************
 'Solenoids & Flashers
 '********************

 SolCallback(1) = "SolRelease"
 SolCallback(2) = "bsBallRelease.SolOut"
 SolCallback(3) = "SolAutoLaunch"
 SolCallback(4) = "bsEject.SolOut"
 SolCallback(5) = "bsVuk.SolOut"
 SolCallback(6) = "bsCenterScoop.SolOut"
 SolCallback(7) = "SolTrapDoor"
 SolCallback(8) = "vpmSolSound SoundFX(""knocker"",DOFKnocker),"
 SolCallback(9) = "ResetDropsR"  'old "dtRBank.SolDropUp"
 SolCallback(11) = "SolGi"
 SolCallback(12) = "ResetDropsL" ' old "dtLBank.SolDropUp"
 SolCallback(14) = "SolKickBack" ' "vpmSolAutoPlunger KickBack,10,"
 SolCallback(23) = "FastFlips.TiltSol" ' Fastflips
 SolCallback(25) = "SolFlash25" ' Left Bumper
 SolCallback(26) = "SolFlash26" ' Mid Bumper
 SolCallback(27) = "SolFlash27" ' Right Bumper
 SolCallback(28) = "SolFlash28" ' Left Slingshot
 SolCallback(29) = "SolFlash29" ' Right Slingshot
 SolCallback(30) = "SolFlash30" ' Top Slingshot In Duff zone
 SolCallback(31) = "SolFlash31" ' Not Use ???
 SolCallback(32) = "SolFlash32" ' Not Use ???
 SolCallback(sLRFlipper) = "SolRFlipper"
 SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
		 If Enabled Then
			 PlaySoundAtVol SoundFX("fx_FlipperUp",DOFFlippers), LeftFlipper, 1:LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
       PlaySoundAtVol "fx_FlipperUp", LeftFlipper1, 1
	else
			 PlaySoundAtVol SoundFX("fx_FlipperDown",DOFFlippers), LeftFlipper, 1:LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
       PlaySoundAtVol "fx_FlipperDown", LeftFlipper1, 1
		 End If
	  End Sub

	Sub SolRFlipper(Enabled)
		 If Enabled Then
			 PlaySoundAtVol SoundFX("fx_FlipperUp",DOFFlippers), RightFlipper, 1:RightFlipper.RotateToEnd
		 Else
			 PlaySoundAtVol SoundFX("fx_FlipperDown",DOFFlippers), RightFlipper, 1:RightFlipper.RotateToStart
		 End If
	End Sub


Sub SolAutoLaunch (enabled)
	If enabled Then Plunger.fire:'PlaysoundAtVol SoundFX ("plunger",DOFContactors), Plunger, 1
End Sub

Sub SolKickBack(enabled)
    If enabled Then
       Kickback.Fire
       PlaysoundAtVol SoundFX ("plunger",DOFContactors), Kickback, 1
    Else
       Kickback.PullBack
    End If
End Sub


Sub SolRelease(Enabled)
   If Enabled And TestJP=0 and bsBallrelease <> 1 And bsTrough.Balls > 0 Then 'pb with multiball when ballrelease was allready waiting with a ball
		bsTrough.ExitSol_On
        vpmCreateBall BallRelease
	bsBallRelease.AddBall 0
    End If
End Sub

 Sub SolTrapDoor(Enabled)
     If Enabled Then
         TrapDoor.Enabled = 1
		Hole=1
         SnakeTrapDoor.IsDropped = 1
         SP.Material = "SPOn"
         TrapDoorOpen.IsDropped = 0
     Else
         TrapDoor.Enabled = 0
		Hole=0
         SnakeTrapDoor.IsDropped = 0
         SP.Material = "SPOff"
         TrapDoorOpen.IsDropped = 1
     End If
 End Sub

 Sub TrapDoor_Hit
	ClearBallID
     TrapDoor.DestroyBall
     PlaySoundAtVol "Ballhit", TrapDoorA, 1
	stopsound "plasticramp2":SndYellowRamp=0:SndYellowRampV=0
     vpmCreateBall TrapDoorA
     TrapDoorA.kick 0, 2
 End Sub

 Sub SolFlash25(Enabled)
     If Enabled Then
		flash5.state = 1
        flash5i.state = 1
		flashrougeref.amount = 70
		flashrougeref.intensityScale = 1
		flash6.state = 1
        flash6i.state = 1
		FlashGreenEtape = 1
		flashvertref.amount = 70
		flashvertref.intensityScale = 1
		FlashRedEtape = 1
'		GIBWL.color = RGB(0,128,0):GIBWL.intensity = 10:
     Else
		flash5.state = 0
        flash5i.state = 1
		flashrougeref.amount = 0
		flashrougeref.intensityScale = 0
		flash6.state = 0
        flash6i.state = 0
		flashvertref.amount = 0
		flashvertref.intensityScale = 0
'		GIBWL.color = RGB(212,212,212):GIBWL.intensity = 2
     end If
 End Sub

 Sub SolFlash26(Enabled)
     If Enabled Then
          f26s.state = 1
		flash2.state = 1
		flash4.state = 1
		flash4a.Amount = 50
		flash4a.IntensityScale = 1
		flash4b.Amount = 70
		flash4b.IntensityScale = 1
		FlashClearEtapeR = 1
'		GIBWL.color = RGB(0,128,0):GIBWL.intensity = 10:
'		GIBWR.color = RGB(255,0,0):GIBWR.intensity = 10
     Else
          f26s.state = 0
		flash2.state = 0
		flash4.state = 0
		flash4a.Amount = 0
		flash4a.IntensityScale = 0
		flash4b.Amount = 0
		flash4b.IntensityScale = 0
'		GIBWL.state = 1:GIBWL.color = RGB(212,212,212):GIBWL.intensity = 2
'		GIBWR.state = 1:GIBWR.color = RGB(212,212,212):GIBWR.intensity = 2
     End If
 End Sub

 Sub SolFlash27(Enabled)
     If Enabled Then
		flash1.state = 1
		flash1a.Amount = 50
		flash1a.IntensityScale = 1
		flash1b.Amount = 70
		flash1b.IntensityScale = 1
		flash1c.Amount = 30
		flash1c.IntensityScale = 1
		GIBWR.color = RGB(255,0,0):GIBWR.intensity = 15:GIBWR.state = 1
		FlashClearEtape = 1
     Else
		flash1.state = 0
		flash1a.Amount = 0
		flash1a.IntensityScale = 0
		flash1b.Amount = 0
		flash1b.IntensityScale = 0
		flash1c.Amount = 0
		flash1c.IntensityScale = 0
		GIBWR.color = RGB(212,212,212):GIBWR.intensity = 2:GIBWR.state = 1
     End If

 End Sub




 Sub SolFlash28(Enabled)
     If Enabled Then
         f28a.State = 1
         f28b.State = 1
		f26S.state = 1
		flash1.state = 1
		flash1a.Amount = 50
		flash1a.IntensityScale = 1
		flash1b.Amount = 70
		flash1b.IntensityScale = 1
		flash1c.Amount = 30
		flash1c.IntensityScale = 1
		flash3.state = 1
		FlashClearEtape = 1

     Else
         f28a.State = 0
         f28b.State = 0
		f26S.state = 0
		flash1.state = 0
		flash1a.Amount = 0
		flash1a.IntensityScale = 0
		flash1b.Amount = 0
		flash1b.IntensityScale = 0
		flash1c.Amount = 0
		flash1c.IntensityScale = 0
		flash3.state = 0
     End If
 End Sub



 Sub SolFlash29(Enabled)
     If Enabled Then
         JackpotRojo.State = 1
		'flash2.state = 1
		flash4.state = 1
		flash4a.Amount = 50
		flash4a.IntensityScale = 1
		flash4b.Amount = 70
		flash4b.IntensityScale = 1
		FlashClearEtapeR = 1
     Else
         JackpotRojo.State = 0
		'flash2.state = 0
		flash4.state = 0
		flash4a.Amount = 0
		flash4a.IntensityScale = 0
		flash4b.Amount = 0
		flash4b.IntensityScale = 0
     End If
 End Sub

 Sub SolFlash30(Enabled)
     If Enabled Then
         JackpotAm.State = 1
     Else
         JackpotAm.State = 0
     End If
 End Sub

  Sub SolFlash31(Enabled)
     If Enabled Then
     f7.state = 0
     f7a.state = 0
     Else
     f7.state = 1
     f7a.state = 1
     End If
 End Sub

  Sub SolFlash32(Enabled)
     If Enabled Then
		F8.state = 1
		F8a.state = 1
     Else
		F8.state = 0
		F8a.state = 0
     End If
 End Sub

 '*****
 ' GI
 '*****

 Sub SolGi(Enabled)
     If Enabled Then
         GiOff
     Else
         GiOn
     End If
 End Sub

 Sub GiOn
     For Each x in GILights:
		x.State = 1
		if l62.state = 1 Then
			RefLaunchBall.Amount = 10
			RefLaunchBall.IntensityScale = 1
		else
			RefLaunchBall.Amount = 0
			RefLaunchBall.IntensityScale = 0
		end If
	Next
     For Each x in LightAmbiant:
		x.State = 1
	Next
	BackWall.Image = "BackWallOn"
'	Flash5i.intensity = 0.75:Flash6i.intensity = 1
	flash2veille.state = 1 :flash2veille1.state = 1
	flash2veille.intensity = 0.75 :flash2veille1.intensity = 0.75
	GIBWL.color = RGB(212,212,212):GIBWL.intensity = 2:GIBWL.state = 1:
	GIBWR.color = RGB(212,212,212):GIBWR.intensity = 2:GIBWR.state = 1
 End Sub

 Sub GiOff
     For Each x in GILights
		x.State = 0
		if l62.state = 1 Then
			RefLaunchBall.Amount = 10
			RefLaunchBall.IntensityScale = 1
		else
			l62z.state = 0
			RefLaunchBall.Amount = 0
			RefLaunchBall.IntensityScale = 0
		end If
	Next
     For Each x in LightAmbiant:
		x.State = 0
	Next
	if FlashClearEtape = 0 then
		flash2veille1.state = 0
	end If
	if FlashClearEtapeR = 0 Then
		flash2veille.state = 0
	end If
	if FlashGreenEtape = 0 then
		flash6i.intensity = 0.3
	end If
	if FlashRedEtape = 0 Then
		Flash5i.intensity = 0.3
	end if
	BackWall.Image = "BackWallOn"
	flash2veille.intensity = 0 :flash2veille1.intensity = 0
	GIBWL.color = RGB(212,212,212):GIBWL.intensity = 1:GIBWL.state = 1
	GIBWR.color = RGB(212,212,212):GIBWR.intensity = 1:GIBWR.state = 1
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

Cap(1) = "cap01"
Cap(2) = "cap02"
Cap(3) = "cap03"
Cap(4) = "cap04"
Cap(5) = "cap05"
Cap(6) = "cap06"
Cap(7) = "cap07"
Cap(8) = "cap08"
Cap(9) = "cap09"
Cap(10) = "cap10"
Cap(11) = "cap11"
Cap(12) = "cap12"
Cap(13) = "cap13"
Cap(14) = "cap14"
Cap(15) = "cap15"
Cap(16) = "cap16"
Cap(17) = "cap17"


Sub capchoice
	Select Case Int(Rnd*17)+1
		Case 1 : numcap = 1:Table1.BallFrontDecal = Cap(numcap)
		Case 2 : numcap = 2:Table1.BallFrontDecal = Cap(numcap)
		Case 3 : numcap = 3:Table1.BallFrontDecal = Cap(numcap)
		Case 4 : numcap = 4:Table1.BallFrontDecal = Cap(numcap)
		Case 5 : numcap = 5:Table1.BallFrontDecal = Cap(numcap)
		Case 6 : numcap = 6:Table1.BallFrontDecal = Cap(numcap)
		Case 7 : numcap = 7:Table1.BallFrontDecal = Cap(numcap)
		Case 8 : numcap = 8:Table1.BallFrontDecal = Cap(numcap)
		Case 9 : numcap = 9:Table1.BallFrontDecal = Cap(numcap)
		Case 10 : numcap = 10:Table1.BallFrontDecal = Cap(numcap)
		Case 11 : numcap = 11:Table1.BallFrontDecal = Cap(numcap)
		Case 12 : numcap = 12:Table1.BallFrontDecal = Cap(numcap)
		Case 13 : numcap = 13:Table1.BallFrontDecal = Cap(numcap)
		Case 14 : numcap = 14:Table1.BallFrontDecal = Cap(numcap)
		Case 15 : numcap = 15:Table1.BallFrontDecal = Cap(numcap)
		Case 16 : numcap = 16:Table1.BallFrontDecal = Cap(numcap)
		Case 17 : numcap = 17:Table1.BallFrontDecal = Cap(numcap)
	End Select
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
     CenterNudgeTimer.Interval = delay
     CenterNudgeTimer.Enabled = 1
 End Sub

 Sub LeftNudgeTimer_Timer()
     LeftNudgeEffect = LeftNudgeEffect-1
     If LeftNudgeEffect = 0 then LeftNudgeTimer.Enabled = 0

 End Sub

 Sub RightNudgeTimer_Timer()
     RightNudgeEffect = RightNudgeEffect-1
     If RightNudgeEffect = 0 then RightNudgeTimer.Enabled = 0
 End Sub

 Sub CenterNudgeTimer_Timer()
     NudgeEffect = NudgeEffect-1
     If NudgeEffect = 0 then CenterNudgeTimer.Enabled = 0
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

' play a looping sound at a location with volume
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
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
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
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
tnopb = 10
nosf = 10

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
' ReDim baX(tnopb, 4), baY(tnopb, 4), baZ(tnopb, 4), bVx(tnopb, 4), bVy(tnopb, 4), TotalVel(tnopb, 4)
' Dim cForce, bDistance, xyTime, cFactor, id, id2, id3, B1, B2

' Sub XYdata_Timer()
'     xyTime = Timer + (XYdata.interval * .001)
'     If id2 >= 4 Then id2 = 0
'     id2 = id2 + 1
'     For id = 1 to ubound(ballStatus)
'         If ballStatus(id) = 1 Then
'             baX(id, id2) = round(currentball(id).x, 2)
'             baY(id, id2) = round(currentball(id).y, 2)
'             baZ(id, id2) = round(currentball(id).z, 2)
'             bVx(id, id2) = round(currentball(id).velx, 2)
'             bVy(id, id2) = round(currentball(id).vely, 2)
'             TotalVel(id, id2) = (bVx(id, id2) ^2 + bVy(id, id2) ^2)
'             If TotalVel(id, id2) > TotalVel(0, 0) Then TotalVel(0, 0) = int(TotalVel(id, id2) )
'         End If
'     Next
'
'     id3 = id2:B2 = 2:B1 = 1
'     Do
'         If ballStatus(B1) = 1 and ballStatus(B2) = 1 Then
'             bDistance = int((TotalVel(B1, id3) + TotalVel(B2, id3) ) ^1.04)
' 			If ABS(baZ(B1, id3) - baZ(B2, id3)) < 50 Then
' 				If((baX(B1, id3) - baX(B2, id3) ) ^2 + (baY(B1, id3) - baY(B2, id3) ) ^2) < 2800 + bDistance Then collide B1, B2:Exit Sub
' 			End If
'         End If
'         B1 = B1 + 1
'         If B1 >= ballStatus(0) Then Exit Do
'         If B1 >= B2 then B1 = 1:B2 = B2 + 1
'     Loop
'
'     If ballStatus(0) <= 1 Then XYdata.enabled = False
'
'     If XYdata.interval >= 40 Then coff = True:XYdata.enabled = False
'     If Timer > xyTime * 3 Then coff = True:XYdata.enabled = False
'     If Timer > xyTime Then XYdata.interval = XYdata.interval + 1
' End Sub

' 'Calculate the collision force and play sound
' Dim cTime, cb1, cb2, avgBallx, cAngle, bAngle1, bAngle2
'
' Sub Collide(cb1, cb2)
'     If TotalVel(0, 0) / 1.8 > cFactor Then cFactor = int(TotalVel(0, 0) / 1.8)
'     avgBallx = (bvX(cb2, 1) + bvX(cb2, 2) + bvX(cb2, 3) + bvX(cb2, 4) ) / 4
'     If avgBallx < bvX(cb2, id2) + .1 and avgBallx > bvX(cb2, id2) -.1 Then
'         If ABS(TotalVel(cb1, id2) - TotalVel(cb2, id2) ) < .000005 Then Exit Sub
'     End If
'     If Timer < cTime Then Exit Sub
'     cTime = Timer + .1
'     GetAngle baX(cb1, id3) - baX(cb2, id3), baY(cb1, id3) - baY(cb2, id3), cAngle
'     id3 = id3 - 1:If id3 = 0 Then id3 = 4
'     GetAngle bVx(cb1, id3), bVy(cb1, id3), bAngle1
'     GetAngle bVx(cb2, id3), bVy(cb2, id3), bAngle2
'     cForce = Cint((abs(TotalVel(cb1, id3) * Cos(cAngle-bAngle1) ) + abs(TotalVel(cb2, id3) * Cos(cAngle-bAngle2) ) ) )
'     If cForce < 4 Then Exit Sub
'     cForce = Cint((cForce) / (cFactor / nosf) )
'     If cForce > nosf-1 Then cForce = nosf-1
'     PlaySound("collide" & cForce)
' End Sub

' Get angle
' Dim Xin, Yin, rAngle, Radit, wAngle, Pi
' Pi = Round(4 * Atn(1), 6) '3.1415926535897932384626433832795
'
' Sub GetAngle(Xin, Yin, wAngle)
'     If Sgn(Xin) = 0 Then
'         If Sgn(Yin) = 1 Then rAngle = 3 * Pi / 2 Else rAngle = Pi / 2
'         If Sgn(Yin) = 0 Then rAngle = 0
'         Else
'             rAngle = atn(- Yin / Xin)
'     End If
'     If sgn(Xin) = -1 Then Radit = Pi Else Radit = 0
'     If sgn(Xin) = 1 and sgn(Yin) = 1 Then Radit = 2 * Pi
'     wAngle = round((Radit + rAngle), 4)
' End Sub


'***************************************************************
'*******                 JPJ - more lights               *******
'***************************************************************
Sub GiHelper_Timer()
	if l54.state = 1 then
		l54_reflet.state = 1
		GIcenter4.state=1
	else
		l54_reflet.state = 0
		GIcenter4.state = 0
	end if

	if l62.state = 1 then
		GIShooter.state = 1
		l62z.state = 1
		RefLaunchBall.Amount = 10
		RefLaunchBall.IntensityScale = 1
	else
		GIShooter.state = 0
		l62z.state = 0
		RefLaunchBall.Amount = 0
		RefLaunchBall.IntensityScale = 0
	end If

	if l55.state = 1 then
		l55a.state = 1
		GIShooter1.state = 1
	else
		l55a.state = 0
		GIShooter1.state = 0
	end if

	if l35.state = 1 then
		l35_reflet.state = 1
	else
		l35_reflet.state = 0
	end if

	if l40.state = 1 then
		l40_reflet.state = 1
	else
		l40_reflet.state = 0
	end if

	if l55a.state = 1 then
		l55a_reflet.state = 1
	else
		l55a_reflet.state = 0
	end if

'	if GIright11.state = 1 Then
'		sider.Image = "SideRWoodOn"
'		sidel.Image = "SideLWoodOn"
'	Else
'		sider.Image = "SideRWoodOff"
'		sidel.Image = "SideLWoodOff"
'	End If

End Sub




'**************************************************************
'************************    Sounds    ************************
'**************************************************************

' Extra Sounds
Sub Sound1_Hit:PlaySoundAtBall "metalrolling", 1:End Sub
Sub Sound2_Hit:PlaySoundAtBall "metalrolling", 1:End Sub
Sub Sound3_Hit:PlaySoundAtBall "metalrolling", 1:End Sub
Sub BallRol1_Hit:PlaySoundAtBall "ballrolling", 1:End Sub


Sub BalldropSound: PlaySoundAtBallVol "fx_ball_drop", 1 : End Sub
' Sub OnBallBallCollision(ball1, ball2, velocity) : PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0 : End Sub

Sub Trigger1_Hit() : Stopsound "plasticrolling" : PlaySoundAtBallVol "metalrolling", GlobalSoundLevel * 0.3 : End Sub

Sub LeftFlipper_Collide(parm) : RandomSoundFlipper() : End Sub
Sub RightFlipper_Collide(parm) : RandomSoundFlipper() : End Sub


Sub Pins_Hit (idx)
	PlaySound "plastic", 0, Vol(ActiveBall)*GlobalSoundLevel, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall)*GlobalSoundLevel, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub MetalsThin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall)*GlobalSoundLevel, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub MetalsMedium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall)*GlobalSoundLevel, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner1_Hit ()
	PlaySound "fx_spinner", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner2_Hit ()
	PlaySound "fx_spinner", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then
		PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*10*GlobalSoundLevel , pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 1 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*10*GlobalSoundLevel , pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*10*GlobalSoundLevel , pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*10*GlobalSoundLevel , pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	End Select
End Sub


Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 :PlaySoundAtBallVol "flip_hit_1", GlobalSoundLevel * ballvel(ActiveBall) / 50
		Case 2 :PlaySoundAtBallVol "flip_hit_2", GlobalSoundLevel * ballvel(ActiveBall) / 50
		Case 3 :PlaySoundAtBallVol "flip_hit_3", GlobalSoundLevel * ballvel(ActiveBall) / 50
	End Select
End Sub


'***************************************
'**     Data-east Flippers Ninuzzu    **
'***************************************

Sub Logo_Timer()
LeftFlipperP.RotZ = LeftFlipper.CurrentAngle
LeftFlipper1P.RotZ = LeftFlipper1.CurrentAngle
RightFlipperP.RotZ = RightFlipper.CurrentAngle
End Sub


'****************************
'**     Anim Flashers      **
'****************************

sub FlashClearAnim_timer()
select Case FlashClearEtape
	Case 0:Primitive13.image = "OctaDome_Clear_00":FlashClearEtape=0
	flash2veille1.state = 1
	flash2veille1.intensity = 0.75
	Case 1:Primitive13.image = "OctaDome_Clear_10":FlashClearEtape=2
	flash2veille1.state = 1
	flash2veille1.intensity = 14
	Case 2:Primitive13.image = "OctaDome_Clear_09":FlashClearEtape=3
	flash2veille1.state = 1
	flash2veille1.intensity = 12.6
	Case 3:Primitive13.image = "OctaDome_Clear_08":FlashClearEtape=4
	flash2veille1.state = 1
	flash2veille1.intensity = 11.2
	Case 4:Primitive13.image = "OctaDome_Clear_07":FlashClearEtape=5
	flash2veille1.state = 1
	flash2veille1.intensity = 9.8
	Case 5:Primitive13.image = "OctaDome_Clear_06":FlashClearEtape=6
	flash2veille1.state = 1
	flash2veille1.intensity = 8.4
	Case 6:Primitive13.image = "OctaDome_Clear_05":FlashClearEtape=7
	flash2veille1.state = 1
	flash2veille1.intensity = 7
	Case 7:Primitive13.image = "OctaDome_Clear_04":FlashClearEtape=8
	flash2veille1.state = 1
	flash2veille1.intensity = 5.6
	Case 8:Primitive13.image = "OctaDome_Clear_03":FlashClearEtape=9
	flash2veille1.state = 1
	flash2veille1.intensity = 4.2
	Case 9:Primitive13.image = "OctaDome_Clear_02":FlashClearEtape=10
	flash2veille1.state = 1
	flash2veille1.intensity = 2.8
	Case 10:Primitive13.image = "OctaDome_Clear_01":FlashClearEtape=11
	flash2veille1.state = 1
	flash2veille1.intensity = 1.4
	Case 11:Primitive13.image = "OctaDome_Clear_00":FlashClearEtape=0
	flash2veille1.state = 0.75
	flash2veille1.intensity = 0.75
End select
End Sub

sub FlashClearAnim2_timer()
select Case FlashClearEtapeR
	Case 0:Primitive16.image = "OctaDome_Clear_00":FlashClearEtapeR=0
	flash2veille.state = 1
	flash2veille.intensity = 0.75
	Case 1:Primitive16.image = "OctaDome_Clear_10":FlashClearEtapeR=2
	flash2veille.state = 1
	flash2veille.intensity = 20
	Case 2:Primitive16.image = "OctaDome_Clear_09":FlashClearEtapeR=3
	flash2veille.state = 1
	flash2veille.intensity = 18
	Case 3:Primitive16.image = "OctaDome_Clear_08":FlashClearEtapeR=4
	flash2veille.state = 1
	flash2veille.intensity = 16
	Case 4:Primitive16.image = "OctaDome_Clear_07":FlashClearEtapeR=5
	flash2veille.state = 1
	flash2veille.intensity = 14
	Case 5:Primitive16.image = "OctaDome_Clear_06":FlashClearEtapeR=6
	flash2veille.state = 1
	flash2veille.intensity = 12
	Case 6:Primitive16.image = "OctaDome_Clear_05":FlashClearEtapeR=7
	flash2veille.state = 1
	flash2veille.intensity = 10
	Case 7:Primitive16.image = "OctaDome_Clear_04":FlashClearEtapeR=8
	flash2veille.state = 1
	flash2veille.intensity = 8
	Case 8:Primitive16.image = "OctaDome_Clear_03":FlashClearEtapeR=9
	flash2veille.state = 1
	flash2veille.intensity = 6
	Case 9:Primitive16.image = "OctaDome_Clear_02":FlashClearEtapeR=10
	flash2veille.state = 1
	flash2veille.intensity = 4
	Case 10:Primitive16.image = "OctaDome_Clear_01":FlashClearEtapeR=11
	flash2veille.state = 1
	flash2veille.intensity = 2
	Case 11:Primitive16.image = "OctaDome_Clear_00":FlashClearEtapeR=0
	flash2veille.state = 0.75
	flash2veille.intensity = 0.75
End select
End Sub

'sub FlashGreenAnim_timer()
select Case FlashGreenEtape
	Case 0:Primitive14.image = "OctaDome_Green_00":FlashGreenEtape=0
	flash6i.state = 1
	flash6i.intensity = 0.3
	Case 1:Primitive14.image = "OctaDome_Green_10":FlashGreenEtape=2
	flash6i.state = 1
	flash6i.intensity = 12
	Case 2:Primitive14.image = "OctaDome_Green_09":FlashGreenEtape=3
	flash6i.state = 1
	flash6i.intensity = 10.8
	Case 3:Primitive14.image = "OctaDome_Green_08":FlashGreenEtape=4
	flash6i.state = 1
	flash6i.intensity = 9.6
	Case 4:Primitive14.image = "OctaDome_Green_07":FlashGreenEtape=5
	flash6i.state = 1
	flash6i.intensity = 8.4
	Case 5:Primitive14.image = "OctaDome_Green_06":FlashGreenEtape=6
	flash6i.state = 1
	flash6i.intensity = 7.2
	Case 6:Primitive14.image = "OctaDome_Green_05":FlashGreenEtape=7
	flash6i.state = 1
	flash6i.intensity = 6
	Case 7:Primitive14.image = "OctaDome_Green_04":FlashGreenEtape=8
	flash6i.state = 1
	flash6i.intensity = 4.8
	Case 8:Primitive14.image = "OctaDome_Green_03":FlashGreenEtape=9
	flash6i.state = 1
	flash6i.intensity = 3.6
	Case 9:Primitive14.image = "OctaDome_Green_02":FlashGreenEtape=10
	flash6i.state = 1
	flash6i.intensity = 2.4
	Case 10:Primitive14.image = "OctaDome_Green_01":FlashGreenEtape=11
	flash6i.state = 1
	flash6i.intensity = 1.2
	Case 11:Primitive14.image = "OctaDome_Green_00":FlashGreenEtape=0
	flash6i.state = 1
	flash6i.intensity = 0.75
End select
'End Sub

'sub FlashRedAnim_timer()
select Case FlashRedEtape
	Case 0:Primitive15.image = "OctaDome_Red_00":FlashRedEtape=0
	flash5i.state = 1
	flash5i.intensity = 0.75
	Case 1:Primitive15.image = "OctaDome_Red_10":FlashRedEtape=2
	flash5i.state = 1
	flash5i.intensity = 12
	Case 2:Primitive15.image = "OctaDome_Red_09":FlashRedEtape=3
	flash5i.state = 1
	flash5i.intensity = 10.8
	Case 3:Primitive15.image = "OctaDome_Red_08":FlashRedEtape=4
	flash5i.state = 1
	flash5i.intensity = 9.6
	Case 4:Primitive15.image = "OctaDome_Red_07":FlashRedEtape=5
	flash5i.state = 1
	flash5i.intensity = 8.4
	Case 5:Primitive15.image = "OctaDome_Red_06":FlashRedEtape=6
	flash5i.state = 1
	flash5i.intensity = 7.2
	Case 6:Primitive15.image = "OctaDome_Red_05":FlashRedEtape=7
	flash5i.state = 1
	flash5i.intensity = 6
	Case 7:Primitive15.image = "OctaDome_Red_04":FlashRedEtape=8
	flash5i.state = 1
	flash5i.intensity = 4.8
	Case 8:Primitive15.image = "OctaDome_Red_03":FlashRedEtape=9
	flash5i.state = 1
	flash5i.intensity = 3.6
	Case 9:Primitive15.image = "OctaDome_Red_02":FlashRedEtape=10
	flash5i.state = 1
	flash5i.intensity = 2.4
	Case 10:Primitive15.image = "OctaDome_Red_01":FlashRedEtape=11
	flash5i.state = 1
	flash5i.intensity = 1.2
	Case 11:Primitive15.image = "OctaDome_Red_00":FlashRedEtape=0
	flash5i.state = 1
	flash5i.intensity = 0.75
End select
'End Sub



'****************************
'** Reflection Mod Routine **
'****************************
Sub MystAnimationB
if Myst=1 and Mystv=1 then Myst1.visible = 1:mm01.visible = 0:mm02.visible = 0:mm03.visible = 0:mm04.visible = 0:mml01.state = 0:end If
if Myst=1 and Mystv=>1 then Myst1.rotz = Myst1.rotz +3
if Myst=1 and Mystv=2 then Myst2.visible = 1:mm01.visible = 0:mm02.visible = 0:mm03.visible = 0:mm04.visible = 0:mml01.state = 0:end If
if Myst=1 and Mystv=>2 then Myst2.rotz = Myst2.rotz +5
if Myst=1 and Mystv=3 then Myst3.visible = 1:mm01.visible = 1:mm02.visible = 0:mm03.visible = 0:mm04.visible = 0:mml01.state = 1:mml01.intensity = 5:end If
if Myst=1 and Mystv=>3 then Myst3.rotz = Myst3.rotz +2
if Myst=1 and Mystv=4 then Myst4.visible = 1:mm01.visible = 1:mm02.visible = 0:mm03.visible = 0:mm04.visible = 0:mml01.state = 1:mml01.intensity = 7:end If
if Myst=1 and Mystv=>4 then Myst4.rotz = Myst4.rotz +3
if Myst=1 and Mystv=5 then Myst5.visible = 1:mm01.visible = 0:mm02.visible = 1:mm03.visible = 0:mm04.visible = 0:mml01.state = 1:mml01.intensity = 9:end If
if Myst=1 and Mystv=>5 then Myst5.rotz = Myst5.rotz +4
if Myst=1 and Mystv=6 then Myst6.visible = 1:mm01.visible = 0:mm02.visible = 1:mm03.visible = 0:mm04.visible = 0:mml01.state = 1:mml01.intensity = 11:end If
if Myst=1 and Mystv=>6 then Myst6.rotz = Myst6.rotz +2
if Myst=1 and Mystv=7 then Myst7.visible = 1:mm01.visible = 0:mm02.visible = 0:mm03.visible = 1:mm04.visible = 0:mml01.state = 1:mml01.intensity = 13:end If
if Myst=1 and Mystv=>7 then Myst7.rotz = Myst7.rotz +3
if Myst=1 and Mystv=8 then Myst8.visible = 1:mm01.visible = 0:mm02.visible = 0:mm03.visible = 1:mm04.visible = 0:mml01.state = 1:mml01.intensity = 15:end If
if Myst=1 and Mystv=>8 then Myst8.rotz = Myst8.rotz +4
if Myst=1 and Mystv=9 then Myst9.visible = 1:mm01.visible = 0:mm02.visible = 0:mm03.visible = 0:mm04.visible = 1:mml01.state = 1:mml01.intensity = 17:end If
if Myst=1 and Mystv=>9 then Myst9.rotz = Myst9.rotz +2
if Myst=1 and Mystv=10 then Myst10.visible = 1:mm01.visible = 0:mm02.visible = 0:mm03.visible = 0:mm04.visible = 1:mml01.state = 1:mml01.intensity = 20:end If
if Myst=1 and Mystv=10 then Myst10.rotz = Myst10.rotz +6
end Sub

Sub RM_Timer()

if myst = 1 then MystAnimationB
pgate5.Rotx = gate5.CurrentAngle + 180

If l1.state = 1 then l1z.state = 1 Else l1z.state = 0: end If
If l2.state = 1 then l2z.state = 1 Else l2z.state = 0: end If
If l3.state = 1 then l3z.state = 1 Else l3z.state = 0: end If
If l4.state = 1 then l4z.state = 1 Else l4z.state = 0: end If
If l5.state = 1 then l5z.state = 1 Else l5z.state = 0: end If
If l6.state = 1 then l6z.state = 1 Else l6z.state = 0: end If
If l7.state = 1 then l7z.state = 1 Else l7z.state = 0: end If
If l8.state = 1 then l8z.state = 1 Else l8z.state = 0: end If
'If l9.state = 1 then l9z.state = 1 Else l9z.state = 0: end If
If l10.state = 1 then l10z.state = 1 Else l10z.state = 0: end If
If l11.state = 1 then l11z.state = 1 Else l11z.state = 0: end If
'If l12.state = 1 then l12z.state = 1 Else l12z.state = 0: end If
'If l13.state = 1 then l13z.state = 1 Else l13z.state = 0: end If
'If l14.state = 1 then l14z.state = 1 Else l14z.state = 0: end If
'If l15.state = 1 then l15z.state = 1 Else l15z.state = 0: end If
'If l16.state = 1 then l16z.state = 1 Else l16z.state = 0: end If
If l17.state = 1 then l17z.state = 1 Else l17z.state = 0: end If
If l17.state = 1 then l17r.state = 1 Else l17r.state = 0: end If
If l18.state = 1 then l18z.state = 1 Else l18z.state = 0: end If
If l18.state = 1 then l18r.state = 1 Else l18r.state = 0: end If
If l19.state = 1 then l19z.state = 1 Else l19z.state = 0: end If
'If l20.state = 1 then l20z.state = 1 Else l20z.state = 0: end If
'If l21.state = 1 then l21z.state = 1 Else l21z.state = 0: end If
'If l22.state = 1 then l22z.state = 1 Else l22z.state = 0: end If
'If l23.state = 1 then l23z.state = 1 Else l23z.state = 0: end If
'If l24.state = 1 then l24z.state = 1 Else l24z.state = 0: end If
If l25.state = 1 then l25z.state = 1 Else l25z.state = 0: end If
If l26.state = 1 then l26z.state = 1 Else l26z.state = 0: end If
If l27.state = 1 then l27z.state = 1 Else l27z.state = 0: end If
If l28.state = 1 then l28z.state = 1 Else l28z.state = 0: end If
If f28b.state = 1 then f28bz.state = 1 Else f28bz.state = 0: end If
If f28a.state = 1 then f28az.state = 1 Else f28az.state = 0: end If
If l29.state = 1 then l29z.state = 1 Else l29z.state = 0: end If
If l29.state = 1 then l29a.state = 1 Else l29a.state = 0: end If
If l30.state = 1 then l30z.state = 1 Else l30z.state = 0: end If
If l30.state = 1 then l30a.state = 1 Else l30a.state = 0: end If
If l31.state = 1 then l31z.state = 1 Else l31z.state = 0: end If
If l31.state = 1 then l31a.state = 1 Else l31a.state = 0: end If
If l32.state = 1 then l32z.state = 1 Else l32z.state = 0: end If
If l32.state = 1 then l32a.state = 1 Else l32a.state = 0: end If
If l33.state = 1 then l33z.state = 1 Else l33z.state = 0: end If
If l34.state = 1 then l34z.state = 1 Else l34z.state = 0: end If
'If l35.state = 1 then l35z.state = 1 Else l35z.state = 0: end If
If l36.state = 1 then
		l36r.state = 1
		flash36r.amount = 120
		flash36r.intensityScale = 2
	Else
		l36r.state = 0
		flash36r.amount = 0
		flash36r.intensityScale = 0
end If
If l37.state = 1 then l37z.state = 1 Else l37z.state = 0: end If
If l38.state = 1 then l38z.state = 1 Else l38z.state = 0: end If
If l39.state = 1 then l39z.state = 1 Else l39z.state = 0: end If
'If l40.state = 1 then l40z.state = 1 Else l40z.state = 0: end If
If l41.state = 1 then l41z.state = 1 Else l41z.state = 0: end If
If l42.state = 1 then l42z.state = 1 Else l42z.state = 0: end If
If l43.state = 1 then l43z.state = 1 Else l43z.state = 0: end If
If l44.state = 1 then l44z.state = 1 Else l44z.state = 0: end If
If l45.state = 1 then l45z.state = 1 Else l45z.state = 0: end If
If l46.state = 1 then l46z.state = 1 Else l46z.state = 0: end If
If l47.state = 1 then l47z.state = 1 Else l47z.state = 0: end If
If l48.state = 1 then l48z.state = 1 Else l48z.state = 0: end If
If l49.state = 1 then l49z.state = 1 Else l49z.state = 0: end If
If l50.state = 1 then l50z.state = 1 Else l50z.state = 0: end If
If l51.state = 1 then l51z.state = 1 Else l51z.state = 0: end If
If l52.state = 1 then l52z.state = 1 Else l52z.state = 0: end If
If l53.state = 1 then l53z.state = 1 Else l53z.state = 0: end If
'If l54.state = 1 then l54z.state = 1 Else l54z.state = 0: end If
If l55.state = 1 then GIShooter1.state = 1 Else GIShooter1.state = 0: end If
If l56.state = 1 then l56z.state = 1 Else l56z.state = 0: end If
If l57.state = 1 then l57z.state = 1 Else l57z.state = 0: end If
If l57.state = 1 then l57r.state = 1 Else l57r.state = 0: end If
If l58.state = 1 then l58z.state = 1 Else l58z.state = 0: end If
If l58.state = 1 then l58r.state = 1 Else l58r.state = 0: end If
If l59.state = 1 then l59z.state = 1 Else l59z.state = 0: end If
If l59.state = 1 then l59r.state = 1 Else l59r.state = 0: end If
If l60.state = 1 then l60z.state = 1 Else l60z.state = 0: end If
If l60.state = 1 then l60r.state = 1 Else l60r.state = 0: end If
If l61.state = 1 then l61z.state = 1 Else l61z.state = 0: end If
If f26S.state = 1 then f26Sz.state = 1 Else f26Sz.state = 0: end If
If JackpotRojo.state = 1 then 	JackpotRojoz.state = 1 Else JackpotRojoz.state = 0: end If
If JackpotAm.state = 1 then JackpotAmz.state = 1 Else JackpotAmz.state = 0: end If
End Sub

Sub resetMyst

Myst1.visible = 0
Myst2.visible = 0
Myst3.visible = 0
Myst4.visible = 0
Myst5.visible = 0
Myst6.visible = 0
Myst7.visible = 0
Myst8.visible = 0
Myst9.visible = 0
Myst10.visible = 0
Mystv=0
if myst =1 then mm01.visible = 0:mm02.visible = 0:mm03.visible = 0:mm04.visible = 0:mml01.state = 0:mml01.intensity = 0:WinSounds:end If
end Sub

sub WinSounds()
	Select Case Int(Rnd*3)
		Case 0 : PlaySound SoundFX("bierwin",DOFContactors)
		Case 1 : PlaySound SoundFX("bierwin2",DOFContactors)
		Case 2 : PlaySound SoundFX("bierwin3",DOFContactors)
	End Select
End Sub

'****************************************
'Flasher Routines
'****************************************
Dim FlashState(200), FlashLevel(200)
Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp, FlashSpeedDown

FlashInit()
FlasherTimer.Interval = 10 'flash fading speed
FlasherTimer.Enabled = 1
'
'' Lamp & Flasher Timers
'
Sub FlasherTimer_Timer()
	Flash  101, Flasher101
	Flash  102, Flasher102
	NfadeL 103, Flasher103

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

Sub SetFlash(nr, stat)
    FlashState(nr) = ABS(stat)
End Sub

Sub Flash(nr, object)
    Select Case FlashState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FlashState(nr) = -1 'completely off
            End if
 '           Object.alpha = FlashLevel(nr)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
            If FlashLevel(nr) > 255 Then
                FlashLevel(nr) = 255
                FlashState(nr) = -2 'completely on
            End if
 '           Object.alpha = FlashLevel(nr)
    End Select
End Sub

Sub NFadeL(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0:FadingLevel(nr) = 0
        Case 5:a.State = 1:FadingLevel(nr) = 1
    End Select
End Sub

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

Sub FlashL(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0:FadingLevel(nr) = 0 ':b.state = 0:FadingLevel(nr) = 0 pour une deuxième
        Case 5:a.state = 1:FadingLevel(nr) = 1 ':b.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

'Sub DM
'	ramp167.visible = DesktopMode:ramp170.visible = DesktopMode
''	primitive13DM.visible = DesktopMode:primitive13.visible = Not DesktopMode
'	ramp167Black.visible = Not DesktopMode:ramp170Black.visible = Not DesktopMode
'	ramp32.visible = DesktopMode:
'if sides = 0 then
'SideL.visible = DesktopMode:SideR.visible = DesktopMode
'Else
'SideL.visible = Not DesktopMode:SideR.visible = Not DesktopMode
'end if
'end Sub
