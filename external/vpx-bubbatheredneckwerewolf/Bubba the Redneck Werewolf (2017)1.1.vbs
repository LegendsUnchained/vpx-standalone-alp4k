'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'''''Bubba the Redneck Werewolf'''''''''''''
'''''''release date 10-29-2017''''''''''''''
'built by: HauntFreaks, BordDog, Bord
'********************************************
'NOT EVER FOR COMMERCIAL USE OR RESALE
'NO BUNDLING IN VPINCABS
'NO DVDS
'NO DISTRIBUTION FOR MONEYS OF ANY KIND
'MORE OR LESS DON'T BE A DICK
'********************************************


Option Explicit
Randomize
 
Const cGameName = "hlywoodh"
 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0
 
LoadVPM "01210000", "sys80.VBS", 3.1

 
'**********************************************************
'********       OPTIONS     *******************************
'**********************************************************
 
Dim BallShadows: Ballshadows=1          '******************set to 1 to turn on Ball shadows
Dim FlipperShadows: FlipperShadows=1  '***********set to 1 to turn on Flipper shadows
Dim ROMSounds: ROMSounds=1				'**********set to 0 for no rom sounds, 1 to play rom sounds
 
'*********** Desktop/Cabinet settings ************************
dim HiddenValue
If Table1.ShowDT = true Then
    HiddenValue = 0
    SideRailLeft.visible=1
    SideRailRight.visible=1
Else
    HiddenValue = 1
    SideRailLeft.visible=0
    SideRailRight.visible=0
End If
 
 
'************************************************
'************************************************
'************************************************
'************************************************
'************************************************
Const UseSolenoids = True
Const UseLamps = True
Const UseSync = False
Const UseGI = False
 
' Standard Sounds
Const SSolenoidOn = "fx_solenoid"
Const SSolenoidOff = "fx_solenoidoff"
Const SCoin = "fx_coin"
 
Dim bsTrough, bslLock, bsrLock, dtRight, dtLeft, FastFlips, stat
 

 
Sub Table1_Init
     With Controller
         .GameName = cGameName
         If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
         .SplashInfoLine = "Hollywood Heat (Gottlieb 1986)"&chr(13)&"1.0"
         .HandleKeyboard = 0
         .ShowTitle = 0
         .ShowDMDOnly = 1
         .ShowFrame = 0
         .HandleMechanics = False
		 .Games(cGameName).Settings.Value("sound") = ROMSounds
		 .Hidden = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With

	stat=0 
	ALlights.uservalue=1

	CaptiveKick.createball
	CaptiveKick.kick 0,0

    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
 
'************************Trough
 
	sw76.CreateBall
	sw20.CreateBall
	sw10.CreateBall
	Controller.Switch(76) = 1
	Controller.Switch(20) = 1
	Controller.Switch(10) = 1

'Kickers 

    Set bslLock=New cvpmBallStack
    with bslLock
        .InitSaucer sw46,46,-170,10
        .InitExitSnd Soundfx("fx_ballrel",DOFContactors), Soundfx("HoleKick",DOFContactors)
    end with
 
    Set bsrLock=New cvpmBallStack
    with bsrLock
        .InitSaucer sw56,56,120,11
        .InitExitSnd Soundfx("fx_ballrel",DOFContactors), Soundfx("HoleKick",DOFContactors)
    end with
 
' Nudging
	vpmNudge.TiltSwitch = 57
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(leftslingshot, rightslingshot, TopSlingShot, URightSlingShot, bumper1)
 
    Set dtLeft=New cvpmDropTarget
        dtLeft.InitDrop Array(sw40,sw50,sw60),Array(40,50,60)
        dtLeft.InitSnd SoundFX("drop1",DOFDropTargets),SoundFX("DTReset",DOFDropTargets)
 
    Set dtRight=New cvpmDropTarget
        dtRight.InitDrop Array(sw41,sw51,sw61),Array(41,51,61)
        dtRight.InitSnd SoundFX("drop1",DOFDropTargets),SoundFX("DTReset",DOFDropTargets)
 
	if ballshadows=1 then
        BallShadowUpdate.enabled=1
      else
        BallShadowUpdate.enabled=0
    end if
 
    if flippershadows=1 then
        FlipperLSh.visible=1
        FlipperRSh.visible=1
       else
        FlipperLSh.visible=0
        FlipperRSh.visible=0
 
    end if

 	vpmMapLights CPULights

    Set FastFlips = new cFastFlips
    with FastFlips
        .CallBackL = "SolLflipper"  'Point these to flipper subs
        .CallBackR = "SolRflipper"  '...
    '   .CallBackUL = "SolULflipper"'...(upper flippers, if needed)
    '   .CallBackUR = "SolURflipper"'...
        .TiltObjects = True 'Optional, if True calls vpmnudge.solgameon automatically. IF YOU GET A LINE 1 ERROR, DISABLE THIS! (or setup vpmNudge.TiltObj!)
    '   .DebugOn = False        'Debug, always-on flippers. Call FastFlips.DebugOn True or False in debugger to enable/disable.
    end with
 
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ' //////////////////////
 ' B2S Light Show
 ' cause i mean everyone loves a good light show
 ' 1 = Background
 ' 2 = Moon
 ' 3 = Bubba
 ' 4 = Logo
 ' 5 = Logo2
 ' 6 = Girl
 ' /////////////////////
 ' example B2S call 
 ' startB2S(#)

Dim b2sstep
b2sstep = 0
'b2sflash.enabled = 0
Dim b2satm

Sub startB2S(aB2S)
	b2sflash.enabled = 1
	b2satm = ab2s
End Sub

Sub b2sflash_timer
    If B2SOn Then
	b2sstep = b2sstep + 1
	Select Case b2sstep
		Case 0
		Controller.B2SSetData b2satm, 0
		Case 1
		Controller.B2SSetData b2satm, 1
		Case 2
		Controller.B2SSetData b2satm, 0
		Case 3
		Controller.B2SSetData b2satm, 1
		Case 4
		Controller.B2SSetData b2satm, 0
		Case 5
		Controller.B2SSetData b2satm, 1
		Case 6
		Controller.B2SSetData b2satm, 0
		Case 7
		Controller.B2SSetData b2satm, 1
		Case 8
		Controller.B2SSetData b2satm, 0
		b2sstep = 0
		b2sflash.enabled = 0
	End Select
    End If
End Sub

' /////////////////
' END b2s 
' well that was quick, tacos anyone?
 
'************************************************
' Solenoids
'************************************************
SolCallback(1) =    "SolLLock"
SolCallback(2) =    "SolRLock"
SolCallback(4) =    "dtDrop2"
SolCallback(5) =    "SolLeftTargetReset"
SolCallback(6) =    "SolRightTargetReset"
SolCallback(7) =     "dtDrop3"
SolCallback(8) =    "solknocker"
SolCallback(9) =    "solballrelease"
SolCallback(10) = "FastFlips.TiltSol"
 
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
 
Sub SolLFlipper(Enabled)
     If Enabled Then
        PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd: LeftFlipper1.RotateToEnd
		controller.Switch(6)=1
     Else
        PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart: LeftFlipper1.RotateToStart
		controller.Switch(6)=0
     End If
  End Sub

' paired drop target dropping in "In-Sync" mode

sub dtDrop2(enabled):dtLeft.hit 2:dtRight.hit 2:end sub 
sub dtDrop3(enabled):dtLeft.hit 3:dtRight.hit 3:end sub

Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd:RightFlipper1.RotateToEnd
		controller.Switch(16)=1
		controller.Switch(71)=1
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart:RightFlipper1.RotateToStart
		controller.Switch(16)=0
		controller.Switch(71)=0
     End If
End Sub

Sub ALlights_timer
	EVAL("AL"&ALlights.uservalue).state=1
	EVAL("AL"&ALlights.uservalue&"a").state=1
	if ALlights.uservalue>1 then 
			EVAL("AL"&(ALlights.uservalue-1)).state=0
			EVAL("AL"&(ALlights.uservalue-1)&"a").state=0
		else
			AL10.state=0
			AL10a.state=0
	end if
	ALlights.uservalue=ALlights.uservalue+1
	if ALlights.uservalue>10 then allights.uservalue=1
End sub
 
Sub FlipperTimer_Timer

testbox.text="LeftKick"&LeftKickTimer.enabled
testbox1.text="Bubba"& bubbaTimer.enabled
testbox2.text="RedRamp"& RedRampStop1.timerenabled

	LFlip.RotY = LeftFlipper.CurrentAngle
	RFlip.RotY = RightFlipper.CurrentAngle
	LFlip1.RotY = LeftFlipper1.CurrentAngle-90
	RFlip1.RotY = RightFlipper1.CurrentAngle-90

'	Pgate.rotz = (Gate.currentangle*.75)+25

	if FlipperShadows=1 then
		FlipperLSh.RotZ = LeftFlipper.currentangle
		FlipperRSh.RotZ = RightFlipper.currentangle
	end if
End Sub


' Ball locks / kickers

Sub sw46_Hit
	startB2S(198)
	PlaySound "holein"
	PlaySound "growl"
	bslLock.AddBall 0
	stat=0
	if LeftKickTimer.enabled=false and RedRampStop1.timerenabled=false and bubbatimer.enabled=false then 
		LeftKickTimer.uservalue=1
		Pbubba.RotX = -5
		LeftKickTimer.enabled=1
	end if
		
End Sub

Sub sw56_Hit:
	startB2S(198)
	PlaySound "growl"
	PlaySound "holein":bsrLock.AddBall 0
	if LeftKickTimer.enabled=false and RedRampStop1.timerenabled=false and bubbatimer.enabled=false then 
		BubbaTimer.uservalue=1
		bubbatimer.enabled=1
	end if
	stat=0
End Sub

Sub SolLLock(enabled)
	If enabled Then
		bslLock.ExitSol_On
		PkickarmL.RotZ = 15
		if LeftKickTimer.enabled=false and RedRampStop1.timerenabled=false and bubbatimer.enabled=false then 
			LeftKickTimer.uservalue = 1
			Pbubba.RotX = -5
			LeftKickTimer.Enabled = 1
		end if
	End If
End Sub

Sub LeftKickTimer_timer
		select case me.uservalue
		  case 1:
			Pbubba.ObjRotX = 15
			Pbubba.RotZ = -10
		  case 2:
			Pbubba.ObjRotX = 10
			Pbubba.RotZ = -20
		  case 3:
			Pbubba.ObjRotX = 5
			Pbubba.RotZ = -30
		  case 4:
			Pbubba.ObjRotX = 0
			Pbubba.RotZ = -40
		  case 5:
			Pbubba.ObjRotX = -5
		  case 6:
			Pbubba.ObjRotX = -10
		  case 7:
			Pbubba.ObjRotX = -15
		  case 9:
			Pbubba.ObjRotX = -10
		  case 10:
			Pbubba.ObjRotX = -5
			Pbubba.RotZ = -30
		  case 11:
			Pbubba.ObjRotX = 0
			Pbubba.RotZ = -20
		  case 12:
			Pbubba.ObjRotX = 5
			Pbubba.RotZ = -10
		  case 13:
			Pbubba.ObjRotX = 10
		  case 14:
			PkickarmL.rotz=0
			Pbubba.ObjRotX = 20
			Pbubba.RotZ = 0
			LeftKickTimer.enabled=false
		end Select
		me.uservalue = me.uservalue+1
End Sub	

Sub SolRLock(enabled)
	If enabled Then
		bsrLock.ExitSol_On
		RightKickTimer.uservalue = 0
		PkickarmR.RotZ = 15
		RightKickTimer.Enabled = 1
	End If
End Sub

Sub RightKickTimer_timer
	select case me.uservalue
	  case 5:
		PkickarmR.rotz=0
		me.enabled=0
	end Select
	me.uservalue = me.uservalue+1
End Sub	

Sub BubbaTimer_timer
		select case me.uservalue
		  case 1:
			Pbubba.RotX = -5
			Pbubba.RotZ = 5
		  case 2:
			Pbubba.RotX = -10
			Pbubba.RotZ = 10
		  case 3:
			Pbubba.RotX = -15
			Pbubba.RotZ = 15
		  case 4:
			Pbubba.RotX = -20
		  case 6:
			Pbubba.RotX = -15
		  case 7:
			Pbubba.RotX = -10
			Pbubba.RotZ = 10 
		  case 8:
			Pbubba.RotX = -5
			Pbubba.RotZ = 5
		  case 9:
			Pbubba.RotX = 0
			Pbubba.RotZ = 0
			BubbaTimer.enabled=false
		end Select
		me.uservalue = me.uservalue+1
End Sub	

'******************************************************
'			TROUGH BASED ON NFOZZY'S
'******************************************************

Sub sw10_Hit():Controller.Switch(10) = 1:UpdateTrough:End Sub
Sub sw10_UnHit():Controller.Switch(10) = 0:UpdateTrough:End Sub
Sub sw20_Hit():Controller.Switch(20) = 1:UpdateTrough:End Sub
Sub sw20_UnHit():Controller.Switch(20) = 0:UpdateTrough:End Sub
Sub sw76_Hit():Controller.Switch(76) = 1:UpdateTrough:End Sub
Sub sw76_UnHit():Controller.Switch(76) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 500
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw76.BallCntOver = 0 Then sw20.kick 60, 1
	UpdateTroughTimer1.Interval = 500
	UpdateTroughTimer1.Enabled = 1
	Me.Enabled = 0
End Sub

Sub UpdateTroughTimer1_Timer()
	If sw20.BallCntOver = 0 Then sw10.kick 60, 1
	Me.Enabled = 0
End Sub

'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub sw66_Hit()
	PlaySoundat "drain", sw66
	UpdateTrough
	Controller.Switch(66) = 1
End Sub

Sub sw66_UnHit()
	Controller.Switch(66) = 0
End Sub

Sub solballrelease(enabled)
	If enabled Then 
		sw66.kick 60,40
		PlaySoundat SoundFX("fx_Solenoid",DOFContactors), sw66
	End If
End Sub

set Lights(12) = L12

Set LampCallback = GetRef("UpdateMultipleLamps")
Sub UpdateMultipleLamps
    if controller.lamp(12)=true and sw76.BallCntOver > 0 then
		sw76.kick 60, 7
		PlaySoundAt "ballrelease", sw76
		UpdateTrough
    end if

    if controller.lamp(2)=true then dtleft.hit 1:dtRight.hit 1
End Sub

 
'*****Drop Lights Off
   dim xx
'    For each xx in dtLeftLights: xx.state=0:Next
 


sub musictimer_timer
	me.enabled=false
end sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************
 
Sub Table1_KeyDown(ByVal KeyCode)
    If KeyCode = LeftFlipperKey then FastFlips.FlipL True :  FastFlips.FlipUL True
    If KeyCode = RightFlipperKey then FastFlips.FlipR True :  FastFlips.FlipUR True
	If KeyCode = startgamekey and musictimer.enabled=false then 
		playsound "bubba_start"
		musictimer.enabled=true
	end if


'If KeyCode = LeftMagnaSave then LeftKickTimer.uservalue=1: LeftKickTimer.enabled=1
'If KeyCode = RightMagnaSave then BubbaTimer.uservalue=1: BubbaTimer.enabled=1
'If keycode = CenterTiltKey then RedRampStop1.uservalue=1: RedRampStop1.timerenabled=1

    If keycode = PlungerKey Then Plunger.Pullback:PlaySoundAt "plungerpull", Plunger

'************************   Start Ball Control 1/3
	if keycode = 46 then	 			' C Key
		If contball = 1 Then
			contball = 0
		Else
			contball = 1
		End If
	End If
	if keycode = 48 then 				'B Key
		If bcboost = 1 Then
			bcboost = bcboostmulti
		Else
			bcboost = 1
		End If
	End If
	if keycode = 203 then bcleft = 1		' Left Arrow
	if keycode = 200 then bcup = 1			' Up Arrow
	if keycode = 208 then bcdown = 1		' Down Arrow
	if keycode = 205 then bcright = 1		' Right Arrow
'************************   End Ball Control 1/3

    If KeyDownHandler(keycode) Then Exit Sub

End Sub
 
Sub Table1_KeyUp(ByVal KeyCode)
    If KeyCode = LeftFlipperKey then FastFlips.FlipL False :  FastFlips.FlipUL False
    If KeyCode = RightFlipperKey then FastFlips.FlipR False :  FastFlips.FlipUR False

    If keycode = PlungerKey Then Plunger.Fire:PlaySoundAt "plunger", Plunger

'************************   Start Ball Control 2/3
	if keycode = 203 then bcleft = 0		' Left Arrow
	if keycode = 200 then bcup = 0			' Up Arrow
	if keycode = 208 then bcdown = 0		' Down Arrow
	if keycode = 205 then bcright = 0		' Right Arrow
'************************   End Ball Control 2/3

    If KeyUpHandler(keycode) Then Exit Sub

End Sub

'************************   Start Ball Control 3/3
Sub StartControl_Hit()
	startB2S(199)
	Set ControlBall = ActiveBall
	contballinplay = true
End Sub

Sub StopControl_Hit()
	contballinplay = false
End Sub	

Dim bcup, bcdown, bcleft, bcright, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti

bcboost = 1		'Do Not Change - default setting
bcvel = 4		'Controls the speed of the ball movement
bcyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3	'Boost multiplier to ball veloctiy (toggled with the B key) 

Sub BallControl_Timer()
	If Contball and ContBallInPlay then
		If bcright = 1 Then
			ControlBall.velx = bcvel*bcboost
		ElseIf bcleft = 1 Then
			ControlBall.velx = - bcvel*bcboost
		Else
			ControlBall.velx=0
		End If

		If bcup = 1 Then
			ControlBall.vely = -bcvel*bcboost
		ElseIf bcdown = 1 Then
			ControlBall.vely = bcvel*bcboost
		Else
			ControlBall.vely= bcyveloffset
		End If
	End If
End Sub
'************************   End Ball Control 3/3


 
'Drop Targets
 Sub Sw40_Dropped:dtleft.Hit 1 : End Sub
 Sub Sw50_Dropped:dtleft.Hit 2 : End Sub
 Sub Sw60_Dropped:dtleft.Hit 3 : End Sub
 
 Sub Sw41_Dropped:dtRight.Hit 1 : End Sub  
 Sub Sw51_Dropped:dtRight.Hit 2 : End Sub
 Sub Sw61_Dropped:dtRight.Hit 3 : End Sub

 
Sub SolRightTargetReset(enabled)
    dim xx
    if enabled then
        dtRight.SolDropUp enabled
    end if
End Sub
 
Sub SolLeftTargetReset(enabled)
    dim xx
    if enabled then
        dtLeft.SolDropUp enabled
'        For each xx in DTLeftLights: xx.state=0:Next
    end if
End Sub
 
'Bumpers
 
Sub bumper1_Hit : vpmTimer.PulseSw 30 : PlaySoundAt SoundFX("fx_bumper4",DOFContactors), Bumper1: DOF 206, DOFPulse:
	startB2S(199)
	PlaySoundAtBall "buuba_lightning"
End Sub

Sub bumper2_Hit : vpmTimer.PulseSw 30 : PlaySoundAt SoundFX("fx_bumper4",DOFContactors), Bumper2: DOF 207, DOFPulse:
	startB2S(199)
	PlaySoundAtBall "buuba_lightning"
End Sub



'Wire Triggers
Sub SW42_Hit
	Controller.Switch(42)=1
	if LeftKickTimer.enabled=false and RedRampStop1.timerenabled=false and bubbatimer.enabled=false then 
		BubbaTimer.uservalue=1
		bubbatimer.enabled=1
	end if
End Sub    
Sub SW42_unHit:Controller.Switch(42)=0:End Sub
Sub SW52_Hit:Controller.Switch(52)=1::End Sub    
Sub SW52_unHit:Controller.Switch(52)=0:End Sub
Sub SW62_Hit:Controller.Switch(62)=1::End Sub    
Sub SW62_unHit:Controller.Switch(62)=0:End Sub
Sub SW73_Hit:Controller.Switch(73)=1::End Sub    
Sub SW73_unHit:Controller.Switch(73)=0:End Sub
Sub SW45_Hit:Controller.Switch(45)=1:End Sub    
Sub SW45_unHit:Controller.Switch(45)=0:End Sub
Sub SW55_Hit:Controller.Switch(55)=1:End Sub    
Sub SW55_unHit:Controller.Switch(55)=0:End Sub
Sub SW65_Hit:Controller.Switch(65)=1:End Sub    
Sub SW65_unHit:Controller.Switch(65)=0:End Sub
Sub SW75_Hit:Controller.Switch(75)=1:End Sub   
Sub SW75_unHit:Controller.Switch(75)=0:End Sub
Sub SW72_Hit:Controller.Switch(72)=1:End Sub    
Sub SW72_unHit:Controller.Switch(72)=0:End Sub
Sub SW70_Hit:Controller.Switch(70)=1:End Sub    
Sub SW70_unHit:Controller.Switch(70)=0:End Sub
Sub SW74_Hit:Controller.Switch(74)=1:End Sub   
Sub SW74_unHit:Controller.Switch(74)=0:End Sub
Sub SW31_Hit:Controller.Switch(31)=1:End Sub   
Sub SW31_unHit:Controller.Switch(31)=0:End Sub

 
'Targets
Sub sw43_Hit:vpmTimer.PulseSw (43):End Sub
Sub sw53_Hit:vpmTimer.PulseSw (53):End Sub
Sub sw63_Hit:vpmTimer.PulseSw (63):End Sub
Sub sw44_Hit:vpmTimer.PulseSw (44):End Sub
Sub sw54_Hit:vpmTimer.PulseSw (54):End Sub
Sub sw64_Hit:vpmTimer.PulseSw (64):End Sub
Sub sw73_Hit:vpmTimer.PulseSw (73):End Sub


 
Sub SolKnocker(Enabled)
    If Enabled Then PlaySound SoundFX("Knocker",DOFKnocker)
End Sub
 

'*****************************************
'	Ramp and Drop Sounds
'*****************************************

Sub saveus_Hit()
	PlaySoundAtBall "bubba_save_us"
End Sub

Sub RedRampStart_Hit()
	if Activeball.vely>0 then  
		PlaySoundAtBall "plasticroll"
	  else
		PlaySoundAtBall "plasticroll1"
	end if
End Sub

Sub RedRampStop_Hit()
	StopSound "plasticroll"
	StopSound "plasticroll1"
End Sub

Sub RedRampStop1_Hit()
	StopSound "plasticroll1"
	vpmTimer.AddTimer 300, "BallDropSound(1)'"
	if LeftKickTimer.enabled=false and RedRampStop1.timerenabled=false and bubbatimer.enabled=false then 
		redrampstop1.uservalue=1
		redrampstop1.timerenabled=1
	end if
End Sub

Sub RedRampStop1_timer
		select case me.uservalue
		  case 1:
			PlaySoundAt "howl", Pbubba
			Pbubba.RotX = 5
		  case 2:
			Pbubba.RotX = 10
		  case 3:
			Pbubba.RotX = 15
		  case 4:
			Pbubba.RotX = 20
		  case 5:
			Pbubba.RotX = 25
		  case 6:
			Pbubba.RotX = 30
		  case 7:
			Pbubba.RotX = 35
		  case 8:
			Pbubba.RotX = 40
		  case 9:
			Pbubba.RotX = 45
		  case 10:
			Pbubba.RotX = 50
		  case 12:
			Pbubba.RotX = 45
		  case 13:
			Pbubba.RotX = 40
		  case 14:
			Pbubba.RotX = 35
		  case 15:
			Pbubba.RotX = 30
		  case 16:
			Pbubba.RotX = 25
		  case 17:
			Pbubba.RotX = 20
		  case 18:
			Pbubba.RotX = 15
		  case 19:
			Pbubba.RotX = 10
		  case 20:
			Pbubba.RotX = 5
		  case 21:
			Pbubba.RotX = 0
			RedRampStop1.timerenabled=false
		end Select
		me.uservalue = me.uservalue+1
End Sub	

Sub SmallRampDrop_Hit()
	vpmTimer.AddTimer 300, "BallDropSound1(1)'"
End Sub


Sub BallDropSound(vol)
	PlaySound "BallDrop", 0, vol, AudioPan(RedRampStop1), 0,0,0, 1, AudioFade(RedRampStop1)
End Sub

Sub BallDropSound1(vol)
	PlaySound "BallDrop", 0, vol, AudioPan(SmallRampDrop), 0,0,0, 1, AudioFade(SmallRampDrop)
End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep, Tstep, URstep

 
Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot",DOFContactors), slingR
    DOF 202, DOFPulse
    vpmtimer.PulseSw(32)
    RSling.Visible = 0
    RSling1.Visible = 1
    slingR.objroty = -15
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub
 
Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:slingR.objroty = -7
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:slingR.objroty = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub
 
Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot",DOFContactors), slingL
    DOF 201, DOFPulse
    vpmtimer.pulsesw(32)
    LSling.Visible = 0
    LSling1.Visible = 1
    slingL.objroty = 15
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub
 
Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:slingL.objroty = 7
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:slingL.objroty=0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub
 
Sub TopSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot",DOFContactors), slingT
    DOF 203, DOFPulse
    vpmtimer.pulsesw(32)
    Tsling.Visible = 0
    Tsling1.Visible = 1
    slingT.objroty = 15
    TStep = 0
    TopSlingShot.TimerEnabled = 1
End Sub
 
Sub TopSlingShot_Timer
    Select Case TStep
        Case 3:Tsling1.Visible = 0:Tsling2.Visible = 1:slingT.objroty = 7
        Case 4:Tsling2.Visible = 0:Tsling.Visible = 1:slingT.objroty=0:TopSlingShot.TimerEnabled = 0
    End Select
    TStep = TStep + 1
End Sub

Sub URightSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot",DOFContactors), slingUR
    DOF 204, DOFPulse
    vpmtimer.PulseSw(32)
    URSling.Visible = 0
    URSling1.Visible = 1
    slingUR.objroty = -15
    URStep = 0
    URightSlingShot.TimerEnabled = 1
End Sub
 
Sub URightSlingShot_Timer
    Select Case URStep
        Case 3:URSLing1.Visible = 0:URSLing2.Visible = 1:slingUR.objroty = -7
        Case 4:URSLing2.Visible = 0:URSLing.Visible = 1:slingUR.objroty = 0:URightSlingShot.TimerEnabled = 0
    End Select
    URStep = URStep + 1
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
 
'*****************************************
'      Ball Rolling Sounds by JP
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
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
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'**********************
' Object Hit Sounds
'**********************
Sub a_Woods_Hit(idx)
	PlaySound "fx_Woodhit", 0, Vol(ActiveBall), Audiopan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub
 
Sub a_Triggers_Hit(idx)
	PlaySound "fx_switch", 0, Vol(ActiveBall), Audiopan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub
 
Sub a_Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub a_Targets_Hit (idx)
	PlaySound "fx_target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub a_DropTargets_Hit (idx)
	PlaySound "fx_drop1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub a_Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub a_Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub a_Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub a_Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub a_Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub a_Posts_Hit(idx)
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


'**********************
'GI uselamps workaround
'**********************

dim GIlamps : set GIlamps = New GIcatcherobject
Class GIcatcherObject   'object that disguises itself as a light. (UseLamps workaround for System80 GI circuit)
    Public Property Let State(input)
        dim x
        if input = 1 then 'If GI switch is engaged, turn off GI.
            for each x in gi : x.state = 0 : next
        elseif input = 0 then
            for each x in gi : x.state = 1 : next
        end if
        'tb.text = "gitcatcher.state = " & input    'debug
    End Property
End Class
 
           
set Lights(1) = GIlamps 'GI circuit
 
 
'*****************************************
'           BALL SHADOW by ninnuzu
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
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/16) + ((BOT(b).X - (Table1.Width/2))/17))' + 13
       Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/16) + ((BOT(b).X - (Table1.Width/2))/17))' - 13
       End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub
 
'Gottlieb Hollywood Heat
'added by Inkochnito
Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700,400,"Hollywood Heat - DIP switches"
		.AddFrame 2,4,190,"Maximum credits",49152,Array("8 credits",0,"10 credits",32768,"15 credits",&H00004000,"20 credits",49152)'dip 15&16
		.AddFrame 2,80,190,"Coin chute 1 and 2 control",&H00002000,Array("seperate",0,"same",&H00002000)'dip 14
		.AddFrame 2,126,190,"Playfield special",&H00200000,Array("replay",0,"extra ball",&H00200000)'dip 22
		.AddFrame 2,172,190,"High games to date control",&H00000020,Array("no effect",0,"reset high games 2-5 on power off",&H00000020)'dip6
		.AddFrame 2,218,190,"Added a letter to PINBALL when",&H40000000,Array("top rolovers are completed",0,"top rolovers are lit",&H40000000)'dip 31
		.AddFrame 2,264,190,"Drop target lamps",&H80000000,Array("reset ball to ball",0,"memorize ball to ball",&H80000000)'dip 32
		.AddFrame 205,4,190,"High score to date awards",&H00C00000,Array("not displayed and no award",0,"displayed and no award",&H00800000,"displayed and 2 replays",&H00400000,"displayed and 3 replays",&H00C00000)'dip 23&24
		.AddFrame 205,80,190,"Balls per game",&H01000000,Array("5 balls",0,"3 balls",&H01000000)'dip 25
		.AddFrame 205,126,190,"Replay limit",&H04000000,Array("no limit",0,"one per ball",&H04000000)'dip 27
		.AddFrame 205,172,190,"Novelty",&H08000000,Array("normal",0,"extra ball and replay scores 500000",&H08000000)'dip 28
		.AddFrame 205,218,190,"Game mode",&H10000000,Array("replay",0,"extra ball",&H10000000)'dip 29
		.AddFrame 205,264,190,"3rd coin chute credits control",&H20000000,Array("no effect",0,"add 9",&H20000000)'dip 30
		.AddChk 205,316,180,Array("Match feature",&H02000000)'dip 26
		.AddChk 2,316,190,Array("Attract sound",&H00000040)'dip 7
		.AddLabel 50,340,300,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub
Set vpmShowDips = GetRef("editDips")
 
Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
 
Sub table1_Exit
	Controller.Games(cGameName).Settings.Value("sound")=1
	Controller.Stop
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